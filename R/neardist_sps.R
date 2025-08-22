#' Distance from animal tracking points to strong ocean fronts (parallel, by month)
#'
#' Computes distances from animal tracking points (whale sharks, turtles, red-tailed
#' tropicbirds, wedge-tailed shearwaters) to strong ocean fronts derived from FSLE
#' or BOA rasters. The function processes one month at a time (from \code{fsle_path}),
#' matches points by exact date (YYYY-MM-DD) to daily raster layers, computes a
#' strong-front mask via a quantile threshold, and extracts per-point distances
#' to the nearest strong front in parallel.
#'
#' @param fsle_path Character. Path to the monthly FSLE/BOA raster file with daily
#'   layers named as dates (e.g., \code{"data-raw/FSLE_SWIO_2018-05.tif"} or
#'   \code{"data-raw/BOAonMUR_SWIO_2018-05.tif"}).
#' @param track_dir Character. Directory containing tracking \code{.rds} files for
#'   a species (or mixed), where filenames include the month tag (e.g., \code{"2018-05"}).
#'   Files can be formatted as lists/tibbles/sf and may follow patterns like:
#'   \itemize{
#'     \item \code{mmf_WhaleShark_YYYY-MM_<ptt>.rds}
#'     \item \code{TMNT_YYYY-MM_<ptt>.rds}
#'     \item \code{NosyVe_YYYY-MM_<track>.rds}   (Red-tailed tropicbird)
#'     \item \code{WTSH_YYYY-MM_<track>.rds}     (Wedge-tailed shearwater)
#'   }
#' @param output_dir Character. Directory where the output \code{.rds} files will
#'   be written (one file per species for the month). Created if it does not exist.
#' @param cutoff Numeric. Quantile threshold for classifying strong fronts
#'   (e.g., \code{0.75} for the top 25\% strongest values).
#' @param meters_crs Character or \code{CRS}. Projection used for distance
#'   calculations (must be in meters), e.g., \code{"ESRI:54030"}.
#' @param front_name Character or \code{NULL}. Optional short name for the front
#'   product (e.g., \code{"fsle"}, \code{"boa"}). If \code{NULL}, inferred from
#'   \code{fsle_path}.
#' @param n_cores Integer. Number of workers for parallel computation.
#'
#' @details
#' Only \code{.rds} files whose filenames contain the same \emph{YYYY-MM} as
#' \code{fsle_path} are processed. Within that month, points are matched by exact
#' date (YYYY-MM-DD) to the corresponding raster layer. The function:
#' \enumerate{
#'   \item Reads and standardizes tracking files (\code{x}, \code{y}, \code{date}).
#'   \item Infers \code{species} and \code{track_id}:
#'     \itemize{
#'       \item \code{NosyVe_*}  \eqn{\rightarrow} species = "Red-tailed tropicbird", track\_id = \code{group}
#'       \item \code{WTSH_*}    \eqn{\rightarrow} species = "Wedge-tailed shearwater", track\_id = \code{group}
#'       \item others (e.g., Whale Shark / Turtle): species = \code{group}, track\_id = \code{ptt} (fallback to \code{group} if \code{ptt} missing)
#'     }
#'   \item For each day: reprojects the daily layer to \code{meters\_crs}, computes a
#'         quantile threshold (\code{cutoff}), builds a strong-front mask, then uses
#'         \code{terra::distance()} to get the distance-to-nearest-strong-front map.
#'   \item Extracts per-point metrics: distance (km), front value at point, and a boolean
#'         flag for being inside a strong front.
#'   \item Writes one \code{.rds} per species for that month into \code{output\_dir}, using:
#'         \code{<SPECIES_ABBR>_<front>_<YYYY-MM>_cutoff-<cutoff>.rds}, e.g.,
#'         \code{RTTB_fsle_2018-05_cutoff-0.75.rds}.
#' }
#'
#' Species abbreviations used in filenames:
#' \itemize{
#'   \item Red-tailed tropicbird \eqn{\rightarrow} \code{RTTB}
#'   \item Wedge-tailed shearwater \eqn{\rightarrow} \code{WTSH}
#'   \item Whale Shark \eqn{\rightarrow} \code{WHSH}
#'   \item Loggerhead Turtle \eqn{\rightarrow} \code{LGHT}
#' }
#'
#' @return A tibble with one row per tracking point that matched a raster day, including:
#' \itemize{
#'   \item \strong{date} — Observation date.
#'   \item \strong{lon}, \strong{lat} — Geographic coordinates (WGS84).
#'   \item \strong{species} — Species label.
#'   \item \strong{track\_id} — Track identifier (PTT or code).
#'   \item \strong{FrontMetric} — Front product name (e.g., "fsle" or "boa").
#'   \item \strong{CutoffProb} — The quantile used.
#'   \item \strong{ThresholdValue} — Threshold value for the day's raster.
#'   \item \strong{DistHFront\_km} — Distance to nearest strong front (km).
#'   \item \strong{FrontVal\_at\_point} — Front value at the point.
#'   \item \strong{InStrongFront} — TRUE if the point falls within a strong-front cell.
#'   \item \strong{ym} — Year-month tag (\code{"YYYY-MM"}).
#' }
#'
#' @import future.apply
#' @import dplyr
#' @import terra
#' @import sf
#' @import stringr
#' @import tibble
#'
#' @examples
#' \dontrun{
#' res <- neardist_track_dir_parallel(
#'   fsle_path  = "data-raw/FSLE_SWIO_2018-05.tif",
#'   track_dir  = "data-raw/NosyVe_tracks/",
#'   output_dir = "outputs/",
#'   cutoff     = 0.75,
#'   meters_crs = "ESRI:54030",
#'   n_cores    = 5
#' )
#' }
#'
#' @export

# ----- SOME fx helpers --------------------------------------------------------------

# handy infix
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

# ----- main FUNCTION -----------------------------------------------------------------

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
    future.scheduling = 1,
    future.seed = TRUE   # ensures parallel-safe, reproducible RNG bc warning in previous version
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