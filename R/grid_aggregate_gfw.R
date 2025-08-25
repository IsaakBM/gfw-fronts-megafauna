#' Aggregate Fishing Effort Data to a Regular Lon/Lat Grid
#'
#' This function bins fishing effort points into a regular lon/lat grid
#' of user-specified resolution, avoiding floating-point join issues by
#' using integer bin indices. It optionally filters by gear type(s),
#' restricts to a bounding box, and aggregates both fishing hours and
#' kW-hours if present. Returns an `sf` polygon layer representing grid
#' cells with associated totals.
#'
#' ## Supported Gear Types
#' The input data may include the following values in the `gear` column:
#' \itemize{
#'   \item drifting_longlines
#'   \item tuna_purse_seines
#'   \item fishing
#'   \item trawlers
#'   \item dredge_fishing
#'   \item set_longlines
#'   \item pole_and_line
#'   \item squid_jigger
#'   \item fixed_gear
#'   \item set_gillnets
#'   \item other_purse_seines
#'   \item purse_seines
#' }
#'
#' @param a A data.frame with columns `lon`, `lat`, `gear`,
#'   `fishing_hours_sum`, and optionally `fishing_kw_hours_sum`,
#'   or an `sf` object with POINT geometry and the same attributes.
#' @param grid_size Numeric. Grid cell size in degrees (e.g. `0.25`, `0.10`).
#' @param gears Optional character vector of gear types to keep. If `NULL`
#'   (default), all gear types are used.
#' @param crs Integer or `st_crs` object. Coordinate reference system to
#'   enforce (default: `4326`, WGS84 lon/lat).
#' @param bbox Optional numeric vector `c(xmin, ymin, xmax, ymax)` in degrees
#'   defining a bounding box. If `NULL`, the extent of the data is used.
#'
#' @return An `sf` polygon grid with attributes:
#' \describe{
#'   \item{ix, iy}{integer bin indices for joining}
#'   \item{lon_bin, lat_bin}{lower-left corner of the grid cell in degrees}
#'   \item{total_fishing_hours}{aggregated fishing hours}
#'   \item{total_fishing_kw_hours}{aggregated fishing kW-hours, if present}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' df <- tibble::tibble(
#'   lon = c(54.48, 56.43, 49.19),
#'   lat = c(-7.61, -12.26, -4.30),
#'   gear = c("drifting_longlines","drifting_longlines","drifting_longlines"),
#'   fishing_hours_sum = c(0.668, 3.44, 1.88),
#'   fishing_kw_hours_sum = c(462, 2163, 1358)
#' )
#'
#' g10 <- grid_aggregate_sf(df, grid_size = 0.10)              # all gears
#' g25 <- grid_aggregate_sf(df, grid_size = 0.25, gears="trawlers")  # filter by gear
#' }
#'
#' @import sf
#' @import dplyr
grid_aggregate_sf <- function(
    a,
    grid_size = 0.10,
    gears = NULL,
    crs = 4326,
    bbox = NULL
) {
  stopifnot(is.data.frame(a) || inherits(a, "sf"))
  
  # Ensure sf POINT in WGS84
  if (!inherits(a, "sf")) {
    req <- c("lon","lat","gear","fishing_hours_sum")
    miss <- setdiff(req, names(a))
    if (length(miss)) stop("Missing columns: ", paste(miss, collapse=", "))
    a <- sf::st_as_sf(a, coords = c("lon","lat"), crs = crs)
  } else if (is.na(sf::st_crs(a))) {
    sf::st_crs(a) <- crs
  } else if (sf::st_crs(a)$epsg != crs) {
    a <- sf::st_transform(a, crs)
  }
  
  # Optional gear filter
  if (!is.null(gears) && "gear" %in% names(a)) {
    a <- dplyr::filter(a, .data$gear %in% gears)
  }
  
  scale_fac <- 1 / grid_size
  
  # Aligned bbox
  if (is.null(bbox)) {
    bb <- sf::st_bbox(a)
    xmin <- floor(as.numeric(bb["xmin"]) * scale_fac) / scale_fac
    ymin <- floor(as.numeric(bb["ymin"]) * scale_fac) / scale_fac
    xmax <- ceiling(as.numeric(bb["xmax"]) * scale_fac) / scale_fac
    ymax <- ceiling(as.numeric(bb["ymax"]) * scale_fac) / scale_fac
  } else {
    stopifnot(length(bbox) == 4)
    xmin <- floor(bbox[1] * scale_fac) / scale_fac
    ymin <- floor(bbox[2] * scale_fac) / scale_fac
    xmax <- ceiling(bbox[3] * scale_fac) / scale_fac
    ymax <- ceiling(bbox[4] * scale_fac) / scale_fac
  }
  
  nx <- as.integer(round((xmax - xmin) * scale_fac))
  ny <- as.integer(round((ymax - ymin) * scale_fac))
  if (nx <= 0L || ny <= 0L) stop("Empty grid: check bbox/grid_size.")
  
  # Grid polygons
  extent_bbox <- sf::st_bbox(c(xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax), crs = crs)
  grid <- sf::st_make_grid(extent_bbox, n = c(nx, ny), what = "polygons", square = TRUE)
  
  # Integer indices for grid cells
  cent <- sf::st_coordinates(sf::st_centroid(grid))
  ix_g <- as.integer(round((cent[,1] - xmin)/grid_size - 0.5))
  iy_g <- as.integer(round((cent[,2] - ymin)/grid_size - 0.5))
  grid_sf <- sf::st_sf(ix = ix_g, iy = iy_g, geometry = grid) |>
    dplyr::mutate(
      lon_bin = xmin + .data$ix * grid_size,
      lat_bin = ymin + .data$iy * grid_size
    )
  
  # Points -> integer bin indices
  coords <- sf::st_coordinates(a)
  pts <- a |>
    dplyr::mutate(
      ix = as.integer(floor((coords[,1] - xmin) * scale_fac)),
      iy = as.integer(floor((coords[,2] - ymin) * scale_fac))
    ) |>
    sf::st_drop_geometry()
  
  # Aggregate
  has_kw <- "fishing_kw_hours_sum" %in% names(pts)
  if (has_kw) {
    df_grid <- pts |>
      dplyr::group_by(.data$ix, .data$iy) |>
      dplyr::summarise(
        total_fishing_hours    = sum(.data$fishing_hours_sum, na.rm = TRUE),
        total_fishing_kw_hours = sum(.data$fishing_kw_hours_sum, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    df_grid <- pts |>
      dplyr::group_by(.data$ix, .data$iy) |>
      dplyr::summarise(
        total_fishing_hours = sum(.data$fishing_hours_sum, na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  out <- dplyr::left_join(grid_sf, df_grid, by = c("ix","iy"))
  ord_cols <- c("ix","iy","lon_bin","lat_bin",
                intersect(c("total_fishing_hours","total_fishing_kw_hours"), names(out)))
  out[, c(ord_cols, setdiff(names(out), c(ord_cols,"geometry")), "geometry")]
}