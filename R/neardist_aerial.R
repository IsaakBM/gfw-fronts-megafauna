###############################################################################
# neardist_aerial_dir_parallel.R
# Purpose : Calculate distances from aerial observation points to strong ocean
#           fronts (FSLE / BOA) for each species, by month, in parallel.
# Notes   : Produces one RDS output per species per input file (per month).
###############################################################################

#' Distance from aerial observation points to strong ocean fronts (parallel, by month)
#'
#' Computes distances from aerial observation points (e.g., marine mammals, seabirds,
#' other megafauna) to strong ocean fronts derived from FSLE or BOA rasters. The function
#' processes one month at a time (\code{fsle_path}), matches observations by exact date
#' (YYYY-MM-DD) to daily raster layers, computes a strong-front mask via a quantile threshold,
#' and extracts per-point distances to the nearest strong front in parallel.
#'
#' @param fsle_path Character. Path to the monthly FSLE/BOA raster file with daily
#'   layers named as dates (e.g., \code{"data-raw/FSLE_SWIO_2010-01.tif"}).
#' @param track_dir Character. Directory containing aerial observation \code{.rds} files.
#'   Files are expected to contain columns \code{x}, \code{y}, \code{dates}, \code{nom_latin},
#'   and \code{famille_fr}. Filenames should include the month tag (e.g., \code{"2010-01"}).
#' @param output_dir Character. Directory where the output \code{.rds} files will be
#'   written. Created if it does not exist.
#' @param cutoff Numeric. Quantile threshold for classifying strong fronts
#'   (e.g., \code{0.75} for the top 25\% strongest values).
#' @param meters_crs Character or \code{CRS}. Projection used for distance
#'   calculations (must be in meters), e.g., \code{"ESRI:54030"}.
#' @param front_name Character or \code{NULL}. Optional short name for the front
#'   product (e.g., \code{"fsle"}, \code{"boa"}). If \code{NULL}, inferred from
#'   \code{fsle_path}.
#' @param n_cores Integer. Number of workers for parallel computation.
#'
#' @return A tibble with one row per observation that matched a raster day, including:
#' \itemize{
#'   \item \strong{date} — Observation date.
#'   \item \strong{lon}, \strong{lat} — Geographic coordinates (WGS84).
#'   \item \strong{species} — Species name (from \code{nom_latin}).
#'   \item \strong{track_id} — Always \code{NA} for aerial observations.
#'   \item \strong{FrontMetric} — Front product name (\code{"fsle"} or \code{"boa"}).
#'   \item \strong{CutoffProb} — The quantile used.
#'   \item \strong{ThresholdValue} — Threshold value for the day's raster.
#'   \item \strong{DistHFront_km} — Distance to nearest strong front (km).
#'   \item \strong{FrontVal_at_point} — Front value at the observation point.
#'   \item \strong{InStrongFront} — TRUE if the point falls within a strong-front cell.
#'   \item \strong{ym} — Year-month tag (\code{"YYYY-MM"}).
#'   \item \strong{sp_abbr} — French family name (\code{famille_fr}).
#' }
#'
#' @section Output filenames:
#' Files are written to \code{output_dir} using:
#' \preformatted{
#' <prefix>_<YYYY-MM>_<Species name>_cutoff-<cutoff>.rds
#' }
#' where \code{<prefix>} is inferred from the directory name:
#' \itemize{
#'   \item \code{marine_mammals} \(\rightarrow\) \code{mm}
#'   \item \code{seabirds} \(\rightarrow\) \code{sb}
#'   \item \code{other_megafauna} \(\rightarrow\) \code{om}
#'   \item otherwise: initials of words (e.g., \code{"some_group"} \(\rightarrow\) \code{"sg"})
#' }
#'
#' @import future.apply
#' @import dplyr
#' @import terra
#' @import sf
#' @import tibble
#' @import stringr
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

# ---- helpers ----------------------------------------------------------------

infer_front_name <- function(path) {
  nm <- tolower(basename(path))
  if (stringr::str_detect(nm, "fsle")) "fsle"
  else if (stringr::str_detect(nm, "boa")) "boa"
  else "front"
}

infer_prefix_from_dir <- function(track_dir) {
  base <- basename(normalizePath(track_dir, mustWork = FALSE))
  base_l <- tolower(gsub("[^a-z0-9_]+", "_", base))
  if (base_l == "marine_mammals") return("mm")
  if (base_l == "seabirds")       return("sb")
  if (base_l == "other_megafauna")return("om")
  # fallback: initials of words
  paste0(substr(unlist(strsplit(base_l, "_+")), 1, 1), collapse = "")
}

sanitize_for_filename <- function(x) {
  x <- as.character(x)
  x <- gsub("[/\\\\:*?\"<>|]", "-", x)   # replace illegal path chars
  x <- gsub("\\s+", " ", x)             # normalize spaces
  x <- trimws(x)
  x
}

# ---- main -------------------------------------------------------------------

neardist_aerial_dir_parallel <- function(fsle_path,
                                         track_dir,
                                         output_dir,
                                         cutoff     = 0.75,
                                         meters_crs = "ESRI:54030",
                                         front_name = NULL,
                                         n_cores    = 5) {
  stopifnot(file.exists(fsle_path), dir.exists(track_dir))
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # front metric + prefix
  if (is.null(front_name) || !nzchar(front_name)) {
    front_name <- infer_front_name(fsle_path)
  }
  file_prefix <- infer_prefix_from_dir(track_dir)
  
  # YYYY-MM tag from raster filename
  ym_tag <- stringr::str_extract(basename(fsle_path), "\\d{4}-\\d{2}")
  if (is.na(ym_tag)) stop("Could not parse YYYY-MM tag from fsle_path: ", fsle_path)
  
  # candidate files (prefer those with month tag)
  files <- list.files(track_dir, pattern = "\\.rds$", full.names = TRUE)
  files_m <- files[stringr::str_detect(basename(files), fixed(ym_tag))]
  if (length(files_m) == 0) files_m <- files
  if (length(files_m) == 0) {
    message("No .rds files found in ", track_dir)
    return(tibble::tibble())
  }
  
  # load raster stack once
  rstack     <- terra::rast(fsle_path)
  layer_days <- names(rstack)
  
  # parallel plan
  future.apply::plan(future.apply::multisession, workers = n_cores)
  
  # process each file independently -> one output per file/species/month
  all_res <- lapply(files_m, function(f) {
    df <- readRDS(f)
    req <- c("x","y","dates","nom_latin","famille_fr")
    miss <- setdiff(req, names(df))
    if (length(miss)) {
      warning("Skipping file (missing columns): ", basename(f), " -> ", paste(miss, collapse = ", "))
      return(NULL)
    }
    
    # standardize schema
    df <- dplyr::transmute(
      df,
      date     = as.Date(dates),
      lon      = x,
      lat      = y,
      species  = nom_latin,
      sp_abbr  = famille_fr,
      track_id = NA_character_
    )
    
    # keep only rows in target month
    df <- dplyr::filter(df, format(date, "%Y-%m") == ym_tag)
    if (!nrow(df)) return(NULL)
    
    # days to process (intersection)
    proc_days <- sort(intersect(unique(as.character(df$date)), layer_days))
    if (!length(proc_days)) return(NULL)
    
    # per-day compute in parallel
    res_list <- future.apply::future_lapply(
      proc_days,
      function(dstr) {
        library(terra); library(dplyr); library(sf); library(tibble)
        
        rstack <- terra::rast(fsle_path)
        r_day   <- rstack[[dstr]]
        r_day_m <- terra::project(r_day, meters_crs)
        
        # threshold via quantile (robust)
        thr <- terra::global(r_day_m, fun = quantile, probs = cutoff, na.rm = TRUE)[1,1]
        
        r_strong <- terra::classify(r_day_m, rbind(c(-Inf, thr, NA), c(thr, Inf, 1)))
        dmap     <- terra::distance(r_strong)
        
        sub <- df[df$date == as.Date(dstr), , drop = FALSE]
        if (!nrow(sub)) return(NULL)
        
        pts_sf <- sf::st_as_sf(sub, coords = c("lon","lat"), crs = 4326)
        pvec   <- terra::vect(pts_sf)
        p_m    <- terra::project(pvec, terra::crs(dmap))
        
        dist_m <- terra::extract(dmap,    p_m)[,2]
        val_pt <- terra::extract(r_day_m, p_m)[,2]
        in_sf  <- !is.na(terra::extract(r_strong, p_m)[,2])
        
        tibble::tibble(
          date              = sub$date,
          lon               = sub$lon,
          lat               = sub$lat,
          species           = sub$species,
          track_id          = sub$track_id,
          FrontMetric       = front_name,
          CutoffProb        = cutoff,
          ThresholdValue    = thr,
          DistHFront_km     = dist_m / 1000,
          FrontVal_at_point = val_pt,
          InStrongFront     = in_sf,
          ym                = format(sub$date, "%Y-%m"),
          sp_abbr           = sub$sp_abbr
        )
      },
      future.scheduling = 1,
      future.seed = TRUE
    )
    
    res <- dplyr::bind_rows(res_list)
    if (!nrow(res)) return(NULL)
    
    # species label for filename (preserve spaces, strip illegal)
    species_name <- sanitize_for_filename(unique(res$species))
    if (length(species_name) != 1L) species_name <- sanitize_for_filename(res$species[1])
    
    # build filename and save
    outfile <- file.path(
      output_dir,
      sprintf("%s_%s_%s_cutoff-%.2f.rds", file_prefix, ym_tag, species_name, cutoff)
    )
    saveRDS(res, outfile)
    message("Saved: ", outfile)
    
    res
  })
  
  future.apply::plan(future.apply::sequential)
  
  dplyr::bind_rows(all_res)
}