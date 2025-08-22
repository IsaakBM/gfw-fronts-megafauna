# Simple single-file distance-to-fronts for tracking data (no parallel)
# Input:
#   fsle_path  : path to monthly FSLE/BOA raster (daily layers named "YYYY-MM-DD")
#   track_file : one .rds file for a single species/track (see examples in your dirs)
#   cutoff     : quantile for "strong front" threshold (e.g., 0.75)
#   meters_crs : CRS used for distance calculations (must be meters), e.g. "ESRI:54030"
#   front_name : optional short name ("fsle","boa"); if NULL inferred from filename
# Output: tibble with columns:
#   date, lon, lat, species, track_id, FrontMetric, CutoffProb, ThresholdValue,
#   DistHFront_km, FrontVal_at_point, InStrongFront, ym


res1 <- neardist_track_single(
  fsle_path  = "data-raw/fronts_dynamical/FSLE_SWIO_2019-02.tif",
  track_file = "data-raw/NosyVe_02/NosyVe_2019-02_EA639135.rds",  # example
  cutoff     = 0.75,
  meters_crs = "ESRI:54030",
  front_name = NULL
)

dplyr::glimpse(res1)


neardist_track_single <- function(fsle_path,
                                  track_file,
                                  cutoff     = 0.75,
                                  meters_crs = "ESRI:54030",
                                  front_name = NULL) {
  
  stopifnot(file.exists(fsle_path), file.exists(track_file))
  suppressPackageStartupMessages({
    library(terra); library(sf); library(dplyr); library(tibble); library(stringr)
  })
  
  # Infer front product name if not provided
  if (is.null(front_name) || !nzchar(front_name)) {
    nm <- tolower(basename(fsle_path))
    front_name <- dplyr::case_when(
      str_detect(nm, "fsle") ~ "fsle",
      str_detect(nm, "boa")  ~ "boa",
      TRUE                   ~ "front"
    )
  }
  
  # ---- read track + harmonize columns ----
  # small helper to extract a data.frame from arbitrary RDS structures
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
  df <- .extract_df(readRDS(track_file))
  
  # standardize names: dates -> date; x,y must exist
  if ("dates" %in% names(df) && !("date" %in% names(df))) {
    df <- dplyr::rename(df, date = dates)
  }
  if (!all(c("x","y","date") %in% names(df))) {
    stop("Track data must contain columns: x, y, date")
  }
  df$date <- as.Date(df$date)
  
  # species / track_id rules by filename
  fbase <- basename(track_file)
  if (str_detect(fbase, regex("NosyVe", ignore_case = TRUE))) {
    df$species  <- "Red-tailed tropicbird"
    df$track_id <- df$group
  } else if (str_detect(fbase, regex("WTSH", ignore_case = TRUE))) {
    df$species  <- "Wedge-tailed shearwater"
    df$track_id <- df$group
  } else {
    # whale shark / turtle style: group = species, ptt = track
    if (!("group" %in% names(df))) stop("Expected 'group' column for species.")
    df$species <- df$group
    df$track_id <- if ("ptt" %in% names(df)) df$ptt else df$group
  }
  
  # keep lon/lat as plain columns
  df$lon <- df$x
  df$lat <- df$y
  
  # build sf and SpatVector when needed
  pts_sf <- sf::st_as_sf(df, coords = c("x","y"), crs = 4326)
  
  # load monthly raster stack
  rstack <- terra::rast(fsle_path)
  
  # iterate days present in both track and raster
  out <- vector("list", length = 0)
  for (dstr in sort(unique(as.character(df$date)))) {
    if (!(dstr %in% names(rstack))) next  # skip if this day isn't in the stack
    
    # raster for this day → meters
    r_day   <- rstack[[dstr]]
    r_day_m <- terra::project(r_day, meters_crs)
    
    # threshold (strong front)
    thr <- stats::quantile(terra::values(r_day_m), probs = cutoff, na.rm = TRUE)
    r_strong <- terra::classify(r_day_m, rbind(c(-Inf, thr, NA), c(thr, Inf, 1)))
    
    # distance map to nearest strong-front cell (meters)
    dmap <- terra::distance(r_strong)
    
    # subset points for this date
    sub_sf <- pts_sf %>% dplyr::filter(as.character(.data$date) == dstr)
    if (!nrow(sub_sf)) next
    
    # extract
    pvec   <- terra::vect(sub_sf)
    p_m    <- terra::project(pvec, terra::crs(dmap))
    dist_m <- terra::extract(dmap,    p_m)[,2]
    val_pt <- terra::extract(r_day_m, p_m)[,2]
    in_sf  <- !is.na(terra::extract(r_strong, p_m)[,2])
    
    atts <- sf::st_drop_geometry(sub_sf)
    tib  <- tibble::tibble(
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
    out[[length(out) + 1]] <- tib
  }
  
  dplyr::bind_rows(out)
}
