###############################################################################
# neardist_aerial_dir_parallel.R
# Purpose : Compute distances from aerial observation points to strong ocean fronts.
# Author  : Adapted from neardist_track_dir_parallel()
# Notes   : Processes one directory at a time (e.g., marine_mammals, seabirds, etc.)
###############################################################################

#' Distance from aerial observation points to strong ocean fronts (parallel, by month)
#'
#' Computes distances from aerial observation points (marine mammals, seabirds,
#' or other megafauna) to strong ocean fronts derived from FSLE or BOA rasters.
#' Works per directory, processing one monthly raster at a time, matching points
#' by exact date (YYYY-MM-DD) to daily raster layers, computing a strong-front
#' mask via a quantile threshold, and extracting per-point distances to the nearest
#' strong front in parallel.
#'
#' @param fsle_path Character. Path to the monthly FSLE/BOA raster file with daily
#'   layers named as dates (e.g., \code{"data-raw/FSLE_SWIO_2010-01.tif"}).
#' @param track_dir Character. Directory containing aerial observation `.rds` files
#'   (e.g., \code{"data-raw/aerial/marine_mammals/"}). Filenames can be anything.
#' @param output_dir Character. Directory where the output `.rds` files will
#'   be written (one file per input species per month). Created if missing.
#' @param cutoff Numeric. Quantile threshold for classifying strong fronts
#'   (e.g., \code{0.75} = top 25\% strongest).
#' @param meters_crs Character or \code{CRS}. Projection used for distance
#'   calculations (must be in meters), e.g., \code{"ESRI:54030"}.
#' @param n_cores Integer. Number of workers for parallel computation.
#'
#' @details
#' For each `.rds` file in `track_dir`, this function:
#' \enumerate{
#'   \item Reads aerial observation points.
#'   \item Extracts species name (\code{nom_latin}) and family (\code{famille_fr}).
#'   \item Matches points by exact date to daily raster layers.
#'   \item Reprojects raster to \code{meters_crs}, builds a strong-front mask
#'         using the chosen \code{cutoff}, and computes distance to nearest strong front.
#'   \item Saves one `.rds` per input file, per month, into \code{output_dir}, named:
#'         \code{<prefix>_<front>_<YYYY-MM>_<species>_cutoff-<cutoff>.rds}, where
#'         \code{prefix} = "mm" / "sb" / "om" depending on directory.
#' }
#'
#' @return A tibble with one row per observation point, including:
#' \itemize{
#'   \item \strong{date} — Observation date.
#'   \item \strong{lon}, \strong{lat} — Geographic coordinates (WGS84).
#'   \item \strong{species} — Species name (from \code{nom_latin}).
#'   \item \strong{sp_abbr} — Family group (from \code{famille_fr}).
#'   \item \strong{track_id} — Always NA for aerial observations.
#'   \item \strong{FrontMetric} — Front product name (e.g., "fsle" or "boa").
#'   \item \strong{CutoffProb} — Quantile used.
#'   \item \strong{ThresholdValue} — Threshold value for strong fronts.
#'   \item \strong{DistHFront\_km} — Distance to nearest strong front (km).
#'   \item \strong{FrontVal_at_point} — Front value at the observation point.
#'   \item \strong{InStrongFront} — TRUE if the point falls inside a strong front.
#'   \item \strong{ym} — Year-month tag (YYYY-MM).
#' }
#'
#' @import future.apply
#' @importFrom future plan multisession sequential
#' @import dplyr
#' @import terra
#' @import sf
#' @import stringr
#' @import tibble
#'
#' @examples
#' \dontrun{
#' res <- neardist_aerial_dir_parallel(
#'   fsle_path  = "data-raw/FSLE_SWIO_2010-01.tif",
#'   track_dir  = "data-raw/aerial/marine_mammals/",
#'   output_dir = "outputs/aerial/marine_mammals/",
#'   cutoff     = 0.75,
#'   meters_crs = "ESRI:54030",
#'   n_cores    = 5
#' )
#' }
#'
#' @export
# -----------------------------------------------------------------------------

# --- Helper to infer front product name from raster path ----------------------
infer_front_name <- function(path) {
  nm <- tolower(basename(path))
  if (stringr::str_detect(nm, "fsle")) {
    "fsle"
  } else if (stringr::str_detect(nm, "boa")) {
    "boa"
  } else {
    "front"
  }
}

# --- Main function -----------------------------------------------------------
neardist_aerial_dir_parallel <- function(fsle_path,
                                         track_dir,
                                         output_dir,
                                         cutoff     = 0.75,
                                         meters_crs = "ESRI:54030",
                                         n_cores    = 5) {
  
  # Preconditions
  stopifnot(file.exists(fsle_path), dir.exists(track_dir))
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  front_name <- infer_front_name(fsle_path)
  
  # Get all .rds files in track_dir
  files <- list.files(track_dir, pattern = "\\.rds$", full.names = TRUE)
  if (length(files) == 0) {
    message("No .rds files found in ", track_dir)
    return(tibble::tibble())
  }
  
  # Read raster stack and extract day names
  rstack      <- terra::rast(fsle_path)
  layer_days  <- names(rstack)
  
  results <- dplyr::bind_rows(lapply(files, function(f) {
    # Read file
    df <- readRDS(f)
    
    # Expect columns: x, y, dates, nom_latin, famille_fr
    if (!all(c("x","y","dates","nom_latin","famille_fr") %in% names(df))) {
      stop("Missing required columns in: ", f)
    }
    df$date <- as.Date(df$dates)
    
    # Parse ym tag for naming outputs
    ym_tag <- unique(format(df$date, "%Y-%m"))
    
    # Species + family names
    df <- df %>%
      dplyr::mutate(
        species  = .data$nom_latin,
        sp_abbr  = .data$famille_fr,
        track_id = NA_character_
      )
    
    # Find valid dates matching raster layers
    proc_days <- sort(intersect(unique(as.character(df$date)), layer_days))
    if (!length(proc_days)) {
      message("No overlapping daily layers for ", basename(f))
      return(tibble::tibble())
    }
    
    # Set up parallel plan
    future::plan(future::multisession, workers = n_cores)
    on.exit(future::plan(future::sequential), add = TRUE)
    
    # Process each day in parallel
    res_list <- future.apply::future_lapply(
      proc_days,
      function(dstr) {
        library(terra); library(dplyr); library(sf); library(tibble)
        
        rstack <- terra::rast(fsle_path)
        r_day   <- rstack[[dstr]]
        r_day_m <- terra::project(r_day, meters_crs)
        
        thr <- stats::quantile(terra::values(r_day_m), probs = cutoff, na.rm = TRUE)
        r_strong <- terra::classify(r_day_m, rbind(c(-Inf, thr, NA), c(thr, Inf, 1)))
        dmap <- terra::distance(r_strong)
        
        sub <- df[df$date == as.Date(dstr), , drop = FALSE]
        if (!nrow(sub)) return(NULL)
        
        pts_sf <- sf::st_as_sf(sub, coords = c("x","y"), crs = 4326)
        pvec   <- terra::vect(pts_sf)
        p_m    <- terra::project(pvec, terra::crs(dmap))
        
        dist_m <- terra::extract(dmap,    p_m)[,2]
        val_pt <- terra::extract(r_day_m, p_m)[,2]
        in_sf  <- !is.na(terra::extract(r_strong, p_m)[,2])
        
        tibble::tibble(
          date              = sub$date,
          lon               = sub$x,
          lat               = sub$y,
          species           = sub$species,
          sp_abbr           = sub$sp_abbr,
          track_id          = NA_character_,
          FrontMetric       = front_name,
          CutoffProb        = cutoff,
          ThresholdValue    = thr,
          DistHFront_km     = dist_m / 1000,
          FrontVal_at_point = val_pt,
          InStrongFront     = in_sf,
          ym                = format(sub$date, "%Y-%m")
        )
      },
      future.scheduling = 1,
      future.seed = TRUE
    )
    
    # Combine all days for this file
    res <- dplyr::bind_rows(res_list)
    if (!nrow(res)) return(res)
    
    # Build clean species name for output file (preserve spaces, strip unsafe)
    sp_clean <- gsub("[/\\\\:*?\"<>|]", "-", unique(df$species)[1])
    sp_clean <- trimws(gsub("\\s+", " ", sp_clean))
    
    # Prefix based on directory name (unchanged)
    prefix <- dplyr::case_when(
      grepl("marine_mammals", track_dir)  ~ "mm",
      grepl("seabirds", track_dir)        ~ "sb",
      grepl("other_megafauna", track_dir) ~ "om",
      TRUE                                ~ "obs"
    )
    
    # ---- Only change: include <front_name> after <prefix> in the filename ----
    out_file <- file.path(
      output_dir,
      sprintf("%s_%s_%s_%s_cutoff-%.2f.rds",
              prefix, front_name, ym_tag, sp_clean, cutoff)
    )
    
    saveRDS(res, out_file)
    message("Saved: ", out_file)
    
    res
  }))
  
  results
}