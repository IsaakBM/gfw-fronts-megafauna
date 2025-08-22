suppressPackageStartupMessages({
  library(terra)
  library(sf)
  library(dplyr)
  library(stringr)
  library(tibble)
  library(future.apply)
})

# ----- helpers --------------------------------------------------------------

# handy infix (if you don't already have it defined)
`%||%` <- function(x, y) if (is.null(x) || !nzchar(x)) y else x

.extract_df <- function(x) {
  if (inherits(x, "sf"))         return(sf::st_drop_geometry(x))
  if (inherits(x, "data.frame")) return(x)
  if (is.list(x) && "data" %in% names(x)) {
    d <- x[["data"]]
    if (inherits(d, "sf"))         return(sf::st_drop_geometry(d))
    if (inherits(d, "data.frame")) return(d)
  }
  if (is.list(x)) {
    for (el in x) {
      if (inherits(el, "sf"))         return(sf::st_drop_geometry(el))
      if (inherits(el, "data.frame")) return(el)
    }
  }
  stop("No usable data.frame found inside RDS object.")
}

# map species -> abbreviation used in filenames
species_abbr <- function(sp) {
  sp_l <- tolower(trimws(sp))
  if (grepl("red[- ]?tailed.*tropicbird", sp_l)) return("RTTB")
  if (grepl("wedge[- ]?tailed.*shearwater", sp_l)) return("WTSH")
  if (grepl("whale.*shark", sp_l))                return("WHSH")
  if (grepl("loggerhead.*turtle", sp_l) || sp_l == "turtle") return("LGHT")
  # fallback if mixed/unknown
  "MIXD"
}

infer_front_name <- function(path) {
  nm <- tolower(basename(path))
  if (str_detect(nm, "fsle")) "fsle" else if (str_detect(nm, "boa")) "boa" else "front"
}

# ----- main -----------------------------------------------------------------

neardist_track_dir_parallel <- function(fsle_path,
                                        track_dir,
                                        output_dir,
                                        cutoff     = 0.75,
                                        meters_crs = "ESRI:54030",
                                        front_name = NULL,
                                        n_cores    = 5) {
  
  stopifnot(file.exists(fsle_path), dir.exists(track_dir))
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  front_name <- front_name %||% infer_front_name(fsle_path)
  
  # YYYY-MM tag from fsle_path
  ym_tag <- str_extract(basename(fsle_path), "\\d{4}-\\d{2}")
  if (is.na(ym_tag)) stop("Could not parse YYYY-MM tag from fsle_path: ", fsle_path)
  
  # list and filter files by that tag
  files <- list.files(track_dir, pattern = "\\.rds$", full.names = TRUE)
  files <- files[str_detect(basename(files), fixed(ym_tag))]
  if (length(files) == 0) {
    message("No track files matched ", ym_tag, " in ", track_dir)
    return(tibble())
  }
  
  # read & standardize all matching tracks
  tracks <- bind_rows(lapply(files, function(f) {
    df <- .extract_df(readRDS(f))
    
    # standardize names
    if ("dates" %in% names(df) && !("date" %in% names(df))) df <- rename(df, date = dates)
    if (!all(c("x","y","date") %in% names(df))) {
      stop("Track data must contain x, y, date: ", f)
    }
    df$date <- as.Date(df$date)
    
    # species / track_id from filename + columns
    fb <- basename(f)
    if (str_detect(fb, regex("^NosyVe", ignore_case = TRUE))) {
      sp  <- "Red-tailed tropicbird"
      tid <- df$group
    } else if (str_detect(fb, regex("^WTSH", ignore_case = TRUE))) {
      sp  <- "Wedge-tailed shearwater"
      tid <- df$group
    } else {
      if (!("group" %in% names(df))) stop("Expected 'group' column (species) in: ", f)
      sp  <- df$group
      tid <- if ("ptt" %in% names(df)) df$ptt else df$group
    }
    
    mutate(df,
           species  = sp,
           track_id = tid,
           lon      = x,
           lat      = y
    )
  }))
  
  if (!nrow(tracks)) {
    message("No rows after reading tracks for ", ym_tag)
    return(tibble())
  }
  
  # load monthly raster stack once (each worker reopens internally)
  # determine the set of days to process (intersection with layer names)
  rstack    <- terra::rast(fsle_path)
  layer_days <- names(rstack)
  proc_days <- sort(intersect(unique(as.character(tracks$date)), layer_days))
  if (!length(proc_days)) {
    message("No overlapping daily layers between tracks and raster for ", ym_tag)
    return(tibble())
  }
  
  plan(multisession, workers = n_cores)
  
  res_list <- future_lapply(
    proc_days,
    function(dstr) {
      library(terra); library(dplyr); library(sf); library(tibble)
      
      rstack <- terra::rast(fsle_path)
      r_day   <- rstack[[dstr]]
      r_day_m <- terra::project(r_day, meters_crs)
      
      thr <- stats::quantile(terra::values(r_day_m), probs = cutoff, na.rm = TRUE)
      r_strong <- terra::classify(r_day_m, rbind(c(-Inf, thr, NA), c(thr, Inf, 1)))
      dmap <- terra::distance(r_strong)
      
      sub <- tracks[tracks$date == as.Date(dstr), , drop = FALSE]
      if (!nrow(sub)) return(NULL)
      
      pts_sf <- sf::st_as_sf(sub, coords = c("x","y"), crs = 4326)
      pvec   <- terra::vect(pts_sf)
      p_m    <- terra::project(pvec, terra::crs(dmap))
      
      dist_m <- terra::extract(dmap,    p_m)[,2]
      val_pt <- terra::extract(r_day_m, p_m)[,2]
      in_sf  <- !is.na(terra::extract(r_strong, p_m)[,2])
      
      atts <- sf::st_drop_geometry(pts_sf)
      
      tibble(
        date              = atts$date,
        lon               = atts$lon,
        lat               = atts$lat,
        species           = atts$species,
        track_id          = atts$track_id,
        FrontMetric       = front_name,
        CutoffProb        = cutoff,
        ThresholdValue    = thr,
        DistHFront_km     = dist_m / 1000,
        FrontVal_at_point = val_pt,
        InStrongFront     = in_sf,
        ym                = format(atts$date, "%Y-%m")
      )
    },
    future.scheduling = 1
  )
  
  plan(sequential)
  
  res <- bind_rows(res_list)
  if (!nrow(res)) return(res)
  
  # write one file per species for this month
  res <- mutate(res, sp_abbr = vapply(species, species_abbr, character(1)))
  split_res <- split(res, res$sp_abbr, drop = TRUE)
  
  invisible(lapply(names(split_res), function(abbr) {
    df <- split_res[[abbr]]
    if (!nrow(df)) return(NULL)
    ff <- file.path(
      output_dir,
      sprintf("%s_%s_%s_cutoff-%.2f.rds", abbr, front_name, unique(df$ym), cutoff)
    )
    saveRDS(df, ff)
    NULL
  }))
  
  res
}

res <- neardist_track_dir_parallel(
  fsle_path  = "data-raw/fronts_dynamical/FSLE_SWIO_2019-02.tif",  # or BOAonMUR_2018-05.tif
  track_dir  = "data-raw/NosyVe_02/",         # folder with many .rds across years
  output_dir = "outputs/",
  cutoff     = 0.75,
  meters_crs = "ESRI:54030",
  n_cores    = 5
)


test01 <- readRDS("outputs/RTTB_fsle_2019-02_cutoff-0.75.rds")
nrow(test01) # 3097
nrow(test01[test01$InStrongFront == TRUE, ]) # 625
nrow(test01[test01$InStrongFront == FALSE, ]) # 2472
