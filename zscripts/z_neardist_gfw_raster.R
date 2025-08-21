
library(future.apply)
library(dplyr)
library(terra)
library(sf)
library(tibble)

# fsle: SpatRaster with daily layers named "YYYY-MM-DD" (EPSG:4326)
# gfw_sf: sf POINTS with cols: date, geartype, (optionally flag, hours, fishing_hours, mmsi_present)

# Helper for optional columns
`%||%` <- function(x, y) if (is.null(x)) y else x


testing03 <- neardist_gfw_dir_parallel(fsle_path = "data_rout/FSLE_SWIO_2019-01.tif", 
                                          gfw_dir = "data_rout/gfw_test", 
                                          cutoff = 0.75, 
                                          meters_crs = "ESRI:54030", 
                                          ncores = 4)




library(future.apply)
library(dplyr)
library(terra)
library(sf)
library(stringr)
library(tibble)

# distances come out in kilometers
neardist_gfw_dir_parallel <- function(fsle_path,     # e.g. "data_rout/FSLE_SWIO_2019-01.tif"
                                      gfw_dir,       # e.g. "data_rout/gfw/"
                                      cutoff,
                                      meters_crs,
                                      ncores) {
  
  # Checks
  if (!file.exists(fsle_path)) stop("FSLE raster not found: ", fsle_path)
  if (!dir.exists(gfw_dir))    stop("GFW directory not found: ", gfw_dir)
  
  # ---- 1) derive year-month tag from fsle filename (YYYY-MM)
  ym_tag <- stringr::str_extract(basename(fsle_path), "\\d{4}-\\d{2}")
  if (is.na(ym_tag)) stop("Could not parse YYYY-MM tag from fsle_path name: ", fsle_path)
  
  # ---- 2) list & filter GFW files by that tag
  gfw_files <- list.files(gfw_dir, pattern = "\\.rds$", full.names = TRUE)
  gfw_files <- gfw_files[stringr::str_detect(basename(gfw_files), fixed(ym_tag))]
  if (length(gfw_files) == 0) {
    message("No GFW .rds files matched tag ", ym_tag, " in ", gfw_dir)
    return(tibble())
  }
  
  # ---- 3) read and bind GFW sf points
  gfw_sf <- do.call(rbind, lapply(gfw_files, function(f) {
    x <- readRDS(f)
    if (inherits(x, "sf")) return(x)
    if (is.data.frame(x))  return(sf::st_as_sf(x, coords = c("x","y"), crs = 4326))
    if (is.list(x) && inherits(x[[1]], "sf")) return(x[[1]])
    if (is.list(x) && is.data.frame(x[[1]]))  return(sf::st_as_sf(x[[1]], coords = c("x","y"), crs = 4326))
    stop("Don’t know how to read GFW file: ", f)
  }))
  
  # ensure required cols
  need <- c("date","geartype")
  if (!all(need %in% names(gfw_sf))) stop("GFW data must contain: date, geartype")
  gfw_sf$date <- as.Date(gfw_sf$date)
  
  # ---- 4) run the parallel per-date terra extraction (built to be worker-safe)
  plan(multisession, workers = ncores)
  
  res_list <- future_lapply(
    sort(unique(as.character(gfw_sf$date))),
    function(dstr) {
      library(terra); library(dplyr); library(sf); library(tibble)
      
      fsle <- terra::rast(fsle_path)
      if (!(dstr %in% names(fsle))) return(NULL)
      
      # FSLE for this day → meters
      r_front   <- fsle[[dstr]]
      r_front_m <- terra::project(r_front, meters_crs)
      
      # strong-front mask
      q        <- stats::quantile(terra::values(r_front_m), probs = cutoff, na.rm = TRUE)
      r_strong <- terra::classify(r_front_m, rbind(c(-Inf, q, NA), c(q, Inf, 1)))
      
      # distance map to nearest strong front (meters)
      dmap <- terra::distance(r_strong)
      
      # GFW points for this date
      pts_d_sf <- dplyr::filter(gfw_sf, as.character(.data$date) == dstr)
      if (!nrow(pts_d_sf)) return(NULL)
      
      pts   <- terra::vect(pts_d_sf)                # sf -> SpatVector
      pts_m <- terra::project(pts, terra::crs(dmap))
      
      # extract metrics
      dist_to_front_m  <- terra::extract(dmap,      pts_m)[,2]
      dist_to_front_km <- dist_to_front_m / 1000
      front_at_point   <- terra::extract(r_front_m, pts_m)[,2]
      in_strong_front  <- !is.na(terra::extract(r_strong, pts_m)[,2])
      
      # drop geometry, bind attributes + metrics
      atts <- sf::st_drop_geometry(pts_d_sf)
      for (nm in c("flag","hours","fishing_hours","mmsi_present")) {
        if (!nm %in% names(atts)) atts[[nm]] <- NA
      }
      
      tibble::as_tibble(dplyr::bind_cols(
        atts[, c("date","geartype","flag","hours","fishing_hours","mmsi_present")],
        tibble::tibble(
          DistHFront_km     = dist_to_front_km,
          FrontMag_at_point = front_at_point,
          InStrongFront     = in_strong_front
        )
      ))
    },
    future.scheduling = 1
  )
  
  plan(sequential)
  
  dplyr::bind_rows(res_list)
}





