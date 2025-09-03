#' Aggregate "Strong Front" Points to a Regular Lon/Lat Grid
#'
#' Filters DFF-like point data to InStrongFront == TRUE and bins points into a
#' regular lon/lat grid (float-safe via integer indices), returning an `sf`
#' polygon layer with counts per grid cell.
#'
#' @param x Either:
#'   - a data.frame / tibble with columns `lon`, `lat`, `InStrongFront`
#'     (optionally `track_id`), or an `sf` POINT object with those attributes; OR
#'   - a character path to an .rds containing one of the above.
#' @param grid_size Numeric. Grid cell size in degrees (e.g., 0.25, 0.10).
#' @param crs Integer or `st_crs`. Target CRS for lon/lat coordinates (default 4326).
#' @param bbox Optional numeric vector c(xmin, ymin, xmax, ymax) in degrees.
#'             If NULL, uses data extent (expanded to grid edges).
#'
#' @return `sf` polygon grid with:
#'   - ix, iy (integer bin indices)
#'   - lon_bin, lat_bin (lower-left corner of the cell, degrees)
#'   - strong_count (number of points with InStrongFront == TRUE in the cell)
#'   - strong_tracks (unique track_id count, if `track_id` exists)
#'
#' @export
#' @examples
#' \dontrun{
#' g_fronts <- grid_aggregate_strongfront_sf(DFF, grid_size = 0.10, bbox = c(30,-30,60,0))
#' }
grid_aggregate_strongfront_sf <- function(
    x,
    grid_size = 0.10,
    crs = 4326,
    bbox = NULL
) {
  # ---- load/standardize input ----
  if (is.character(x) && length(x) == 1L && file.exists(x)) {
    x <- readRDS(x)
  }
  if (!inherits(x, "sf")) {
    x <- as.data.frame(x)
    req <- c("lon", "lat", "InStrongFront")
    miss <- setdiff(req, names(x))
    if (length(miss)) stop("Missing columns: ", paste(miss, collapse = ", "))
    x <- sf::st_as_sf(x, coords = c("lon","lat"), crs = crs)
  } else {
    if (is.na(sf::st_crs(x))) {
      sf::st_crs(x) <- crs
    } else if (!identical(sf::st_crs(x), sf::st_crs(crs))) {
      x <- sf::st_transform(x, crs)
    }
    # ensure required attribute exists
    if (!("InStrongFront" %in% names(x)))
      stop("`InStrongFront` column not found in `x`.")
  }
  
  # ---- keep only TRUE strong-front points ----
  x <- dplyr::filter(x, !is.na(.data$InStrongFront) & .data$InStrongFront)
  if (nrow(x) == 0L) {
    warning("No rows with InStrongFront == TRUE. Returning empty grid (no counts).")
  }
  
  # ---- grid math (float-safe integer bins) ----
  scale_fac <- 1 / grid_size
  if (is.null(bbox)) {
    bb <- sf::st_bbox(x)
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
  
  extent_bbox <- sf::st_bbox(c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax), crs = crs)
  grid <- sf::st_make_grid(extent_bbox, n = c(nx, ny), what = "polygons", square = TRUE)
  
  cent <- sf::st_coordinates(sf::st_centroid(grid))
  ix_g <- as.integer(round((cent[,1] - xmin)/grid_size - 0.5))
  iy_g <- as.integer(round((cent[,2] - ymin)/grid_size - 0.5))
  grid_sf <- sf::st_sf(ix = ix_g, iy = iy_g, geometry = grid) |>
    dplyr::mutate(
      lon_bin = xmin + .data$ix * grid_size,
      lat_bin = ymin + .data$iy * grid_size
    )
  
  # ---- points -> integer bin indices ----
  coords <- sf::st_coordinates(x)
  pts <- x |>
    dplyr::mutate(
      ix = as.integer(floor((coords[,1] - xmin) * scale_fac)),
      iy = as.integer(floor((coords[,2] - ymin) * scale_fac))
    ) |>
    sf::st_drop_geometry()
  
  # ---- aggregate counts (and unique tracks if available) ----
  has_track <- "track_id" %in% names(pts)
  if (has_track) {
    df_grid <- pts |>
      dplyr::group_by(.data$ix, .data$iy) |>
      dplyr::summarise(
        strong_count  = dplyr::n(),
        strong_tracks = dplyr::n_distinct(.data$track_id),
        .groups = "drop"
      )
  } else {
    df_grid <- pts |>
      dplyr::group_by(.data$ix, .data$iy) |>
      dplyr::summarise(
        strong_count = dplyr::n(),
        .groups = "drop"
      )
  }
  
  out <- dplyr::left_join(grid_sf, df_grid, by = c("ix","iy"))
  ord_cols <- c("ix","iy","lon_bin","lat_bin",
                intersect(c("strong_count","strong_tracks"), names(out)))
  out[, c(ord_cols, setdiff(names(out), c(ord_cols,"geometry")), "geometry")]
}