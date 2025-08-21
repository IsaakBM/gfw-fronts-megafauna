library(future.apply)
library(dplyr)
library(terra)
library(sf)
library(stringr)
library(tibble)

neardist_gfw_dir_parallel <- function(fsle_path,
                                      gfw_dir,
                                      cutoff     = 0.75,
                                      meters_crs = "ESRI:54030",
                                      ncores     = 5,
                                      output_dir = NULL,
                                      file_prefix = "gfw",
                                      front_name  = NULL) {
  # --- Checks
  if (!file.exists(fsle_path)) stop("Front raster not found: ", fsle_path)
  if (!dir.exists(gfw_dir))    stop("GFW directory not found: ", gfw_dir)
  
  # --- 1) Parse month tag (YYYY-MM) from raster filename
  ym_tag <- stringr::str_extract(basename(fsle_path), "\\d{4}-\\d{2}")
  if (is.na(ym_tag)) stop("Could not parse YYYY-MM tag from fsle_path: ", fsle_path)
  
  # Infer front metric name if not provided
  if (is.null(front_name) || !nzchar(front_name)) {
    nm <- tolower(basename(fsle_path))
    front_name <- if (stringr::str_detect(nm, "boa")) "boa"
    else if (stringr::str_detect(nm, "fsle")) "fsle"
    else "front"
  }
  
  # --- 2) Pick only GFW files that match that month tag
  gfw_files <- list.files(gfw_dir, pattern = "\\.rds$", full.names = TRUE)
  gfw_files <- gfw_files[stringr::str_detect(basename(gfw_files), fixed(ym_tag))]
  if (length(gfw_files) == 0) {
    message("No GFW .rds files matched ", ym_tag, " in ", gfw_dir)
    return(tibble())
  }
  
  # --- 3) Read & bind GFW sf points, add lon/lat ONCE (optimization)
  gfw_sf <- do.call(rbind, lapply(gfw_files, function(f) {
    x <- readRDS(f)
    if (inherits(x, "sf")) return(x)
    if (is.data.frame(x))  return(sf::st_as_sf(x, coords = c("x","y"), crs = 4326))
    if (is.list(x) && inherits(x[[1]], "sf")) return(x[[1]])
    if (is.list(x) && is.data.frame(x[[1]]))  return(sf::st_as_sf(x[[1]], coords = c("x","y"), crs = 4326))
    stop("Don’t know how to read GFW file: ", f)
  }))
  if (!all(c("date","geartype") %in% names(gfw_sf))) stop("GFW data must contain: date, geartype")
  gfw_sf$date <- as.Date(gfw_sf$date)
  
  # Precompute lon/lat and attach as regular columns
  coords <- sf::st_coordinates(gfw_sf)
  gfw_sf$lon <- coords[,1]
  gfw_sf$lat <- coords[,2]
  
  # --- 4) Parallel per-date extraction (re-open raster inside workers)
  plan(multisession, workers = ncores)
  
  res_list <- future_lapply(
    sort(unique(as.character(gfw_sf$date))),
    function(dstr) {
      library(terra); library(dplyr); library(sf); library(tibble)
      
      rstack <- terra::rast(fsle_path)
      if (!(dstr %in% names(rstack))) return(NULL)
      
      # Raster for that day → meters CRS
      r_day   <- rstack[[dstr]]
      r_day_m <- terra::project(r_day, meters_crs)
      
      # Threshold via quantile
      thr_value <- stats::quantile(terra::values(r_day_m), probs = cutoff, na.rm = TRUE)
      r_strong  <- terra::classify(r_day_m, rbind(c(-Inf, thr_value, NA), c(thr_value, Inf, 1)))
      
      # Distance to nearest strong-front cell (meters)
      dmap <- terra::distance(r_strong)
      
      # Points for that date
      pts_d_sf <- dplyr::filter(gfw_sf, as.character(.data$date) == dstr)
      if (!nrow(pts_d_sf)) return(NULL)
      
      # sf -> SpatVector (for projection/extract)
      pts   <- terra::vect(pts_d_sf)
      pts_m <- terra::project(pts, terra::crs(dmap))
      
      # Extract metrics
      dist_m  <- terra::extract(dmap,    pts_m)[,2]
      dist_km <- dist_m / 1000
      val_pt  <- terra::extract(r_day_m, pts_m)[,2]
      in_str  <- !is.na(terra::extract(r_strong, pts_m)[,2])
      
      # Drop geometry; optional cols; bind (lon/lat already present)
      atts <- sf::st_drop_geometry(pts_d_sf)
      for (nm in c("flag","hours","fishing_hours","mmsi_present")) {
        if (!nm %in% names(atts)) atts[[nm]] <- NA
      }
      
      tibble::as_tibble(dplyr::bind_cols(
        atts[, c("date","geartype","flag","hours","fishing_hours","mmsi_present","lon","lat")],
        tibble::tibble(
          FrontMetric        = front_name,
          CutoffProb         = cutoff,
          ThresholdValue     = thr_value,
          DistHFront_km      = dist_km,
          FrontVal_at_point  = val_pt,
          InStrongFront      = in_str
        )
      ))
    },
    future.scheduling = 1
  )
  
  plan(sequential)
  
  res <- dplyr::bind_rows(res_list)
  
  # --- 5) Optional writing: one RDS per MONTH, filename: gfw_<metric>_<YYYY-MM>_cutoff-<cutoff>.rds
  if (!is.null(output_dir) && nrow(res)) {
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    
    res <- res %>% mutate(ym = format(as.Date(date), "%Y-%m"))
    split_res <- split(res, res$ym, drop = TRUE)
    
    invisible(lapply(names(split_res), function(mo) {
      sgl <- split_res[[mo]]
      if (!nrow(sgl)) return(NULL)
      ffname <- paste0(file_prefix, "_", front_name, "_", mo, "_cutoff-", cutoff, ".rds")
      saveRDS(sgl, file.path(output_dir, ffname))
      NULL
    }))
  }
  
  return(res)
}


res <- neardist_gfw_dir_parallel(
  fsle_path   = "data_rout/FSLE_SWIO_2019-01.tif", # or BOA...
  gfw_dir     = "data_rout/gfw_test/",
  cutoff      = 0.75,
  meters_crs  = "ESRI:54030",
  ncores      = 5,
  output_dir  = "data_rout/gfw_neardist_fsle/",
  file_prefix = "gfw"
  # front_name = "fsle" # optional
)
