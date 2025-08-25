#' Aggregate GFW Fishing Data by Cell and Gear (HPC-optimized)
#'
#' Aggregates fishing hours and fishing kW-hours by grid cell
#' (`lon_bin`, `lat_bin`) and `gear`. Optionally crops using a lon/lat bbox,
#' projects to Robinson, and optionally crops in Robinson.
#'
#' @param parquet_path Path to the GFW `.parquet` dataset.
#' @param bbox_lonlat Optional bounding box in lon/lat: `c(xmin, ymin, xmax, ymax)`.
#' @param robinson Logical; if `TRUE`, returns an `sf` object projected to Robinson.
#' @param rob_bbox Optional bounding box in Robinson meters: `c(xmin, ymin, xmax, ymax)`.
#' @param save_rds Optional file path to save the result as an `.rds`.
#'
#' @return A tibble (if `robinson = FALSE`) or an `sf` object (if `robinson = TRUE`).
#' @export
#'
#' @import arrow
#' @import dplyr
#' @importFrom sf st_as_sf st_transform st_crs st_bbox st_crop
aggregate_gfw_by_cell <- function(
    parquet_path,
    bbox_lonlat = NULL,
    robinson = FALSE,
    rob_bbox = NULL,
    save_rds = NULL
) {
  # ---- threading (pick up SLURM cores if present) -------------------------
  n_threads <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", unset = NA))
  if (is.na(n_threads) || n_threads <= 0) {
    n_threads <- max(1L, getOption("mc.cores", 1L))
  }
  # Respect external setting if the runner already set ARROW_NUM_THREADS
  env_threads <- suppressWarnings(as.integer(Sys.getenv("ARROW_NUM_THREADS", unset = NA)))
  if (!is.na(env_threads) && env_threads > 0L) n_threads <- env_threads
  # Tell Arrow how many threads to use
  Sys.setenv(ARROW_NUM_THREADS = n_threads)
  arrow::set_cpu_count(n_threads)
  
  # ---- open dataset (prune columns early to reduce I/O) -------------------
  needed_cols <- c("lon_bin", "lat_bin", "gear", "fishing_hours", "fishing_kw_hours")
  ds <- arrow::open_dataset(
    parquet_path,
    format  = "parquet",
    columns = needed_cols
  )
  
  # ---- build lazy query: filter -> group_by -> summarise ------------------
  q <- ds %>%
    {
      if (!is.null(bbox_lonlat)) {
        stopifnot(length(bbox_lonlat) == 4)
        xmin <- bbox_lonlat[1]; ymin <- bbox_lonlat[2]
        xmax <- bbox_lonlat[3]; ymax <- bbox_lonlat[4]
        dplyr::filter(., lon_bin >= xmin, lon_bin <= xmax,
                      lat_bin >= ymin, lat_bin <= ymax)
      } else .
    } %>%
    dplyr::group_by(lon_bin, lat_bin, gear) %>%
    dplyr::summarise(
      fishing_hours_sum    = sum(fishing_hours, na.rm = TRUE),
      fishing_kw_hours_sum = sum(fishing_kw_hours, na.rm = TRUE),
      .groups = "drop"
    )
  
  # ---- collect then (optionally) project/crop in Robinson -----------------
  agg_df <- q %>%
    dplyr::collect() %>%
    dplyr::rename(lon = lon_bin, lat = lat_bin)
  
  if (robinson) {
    agg_sf <- sf::st_as_sf(agg_df, coords = c("lon", "lat"), crs = 4326)
    
    rob_crs <- tryCatch(sf::st_crs("ESRI:54030"), error = function(e) NULL)
    if (is.null(rob_crs)) rob_crs <- sf::st_crs("+proj=robin +datum=WGS84 +units=m +no_defs")
    
    agg_sf_rob <- sf::st_transform(agg_sf, crs = rob_crs)
    
    if (!is.null(rob_bbox)) {
      stopifnot(length(rob_bbox) == 4)
      rb <- sf::st_bbox(c(
        xmin = rob_bbox[1], ymin = rob_bbox[2],
        xmax = rob_bbox[3], ymax = rob_bbox[4]
      ), crs = rob_crs)
      agg_sf_rob <- sf::st_crop(agg_sf_rob, rb)
    }
    
    if (!is.null(save_rds)) saveRDS(agg_sf_rob, save_rds)
    return(agg_sf_rob)
  } else {
    if (!is.null(save_rds)) saveRDS(agg_df, save_rds)
    return(agg_df)
  }
}