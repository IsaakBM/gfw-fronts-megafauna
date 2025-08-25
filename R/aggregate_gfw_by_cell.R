#' Aggregate GFW Fishing Data by Cell and Gear
#'
#' This function aggregates fishing hours and fishing kW-hours by grid cell
#' (`lon_bin`, `lat_bin`) and `gear`. It optionally crops the result using
#' a bounding box in lon/lat, projects to Robinson, and/or crops in Robinson.
#' Results can be saved as `.rds`.
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
#'
#' @examples
#' \dontrun{
#' # Aggregate, crop to Mozambique Channel in lon/lat, keep WGS84
#' agg_ll <- aggregate_gfw_by_cell(
#'   parquet_path = "data-raw/gfw_data_by_flag_and_gear_v20250820.parquet",
#'   bbox_lonlat  = c(30, -30, 60, 0),
#'   robinson     = FALSE,
#'   save_rds     = "data/agg_cell_gear_ll.rds"
#' )
#'
#' # Aggregate, project to Robinson, crop in Robinson bbox
#' agg_rob <- aggregate_gfw_by_cell(
#'   parquet_path = "data-raw/gfw_data_by_flag_and_gear_v20250820.parquet",
#'   bbox_lonlat  = c(30, -30, 60, 0),
#'   robinson     = TRUE,
#'   rob_bbox     = c(2671900, -3743317, 5654583, -748663.4),
#'   save_rds     = "data/agg_cell_gear_rob.rds"
#' )
#' }
aggregate_gfw_by_cell <- function(
    parquet_path,
    bbox_lonlat = NULL,
    robinson = FALSE,
    rob_bbox = NULL,
    save_rds = NULL
) {
  # Open dataset lazily
  ds <- arrow::open_dataset(parquet_path, format = "parquet")
  
  # Aggregate by lon/lat cell + gear (lazy, efficient)
  agg_lazy <- ds %>%
    dplyr::group_by(lon_bin, lat_bin, gear) %>%
    dplyr::summarise(
      fishing_hours_sum    = sum(fishing_hours, na.rm = TRUE),
      fishing_kw_hours_sum = sum(fishing_kw_hours, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Optional crop in lon/lat BEFORE collect (much faster)
  if (!is.null(bbox_lonlat)) {
    stopifnot(length(bbox_lonlat) == 4)
    xmin <- bbox_lonlat[1]; ymin <- bbox_lonlat[2]
    xmax <- bbox_lonlat[3]; ymax <- bbox_lonlat[4]
    
    agg_lazy <- agg_lazy %>%
      dplyr::filter(lon_bin >= xmin, lon_bin <= xmax,
                    lat_bin >= ymin, lat_bin <= ymax)
  }
  
  # Collect aggregated dataset into memory
  agg_df <- agg_lazy %>%
    dplyr::collect() %>%
    dplyr::rename(lon = lon_bin, lat = lat_bin)
  
  # Optional projection to Robinson and cropping in Robinson coordinates
  if (robinson) {
    # Convert to sf object
    agg_sf <- sf::st_as_sf(agg_df, coords = c("lon", "lat"), crs = 4326)
    
    # Robinson projection
    rob_crs <- tryCatch(sf::st_crs("ESRI:54030"), error = function(e) NULL)
    if (is.null(rob_crs)) {
      rob_crs <- sf::st_crs("+proj=robin +datum=WGS84 +units=m +no_defs")
    }
    agg_sf_rob <- sf::st_transform(agg_sf, crs = rob_crs)
    
    # Optional crop in Robinson
    if (!is.null(rob_bbox)) {
      stopifnot(length(rob_bbox) == 4)
      rb <- sf::st_bbox(
        c(xmin = rob_bbox[1], ymin = rob_bbox[2],
          xmax = rob_bbox[3], ymax = rob_bbox[4]),
        crs = rob_crs
      )
      agg_sf_rob <- sf::st_crop(agg_sf_rob, rb)
    }
    
    # Save if requested
    if (!is.null(save_rds)) saveRDS(agg_sf_rob, save_rds)
    return(agg_sf_rob)
  } else {
    # Save if requested
    if (!is.null(save_rds)) saveRDS(agg_df, save_rds)
    return(agg_df)
  }
}