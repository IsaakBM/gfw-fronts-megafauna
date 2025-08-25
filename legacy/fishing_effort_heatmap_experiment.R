library(sf)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(scales)

# Load data
a1 <- readRDS("data-raw/agg_cell_gear_mzc_rob.rds")
a1 <- sf::st_as_sf(a1, coords = c("lon", "lat"), crs = 4326)

# -------------------------------
# 0) Sanity check CRS
# -------------------------------
stopifnot(!is.na(st_crs(a1)))
if (st_crs(a1)$epsg != 4326) {
  a1 <- st_transform(a1, 4326)
}

# -------------------------------
# 1) Define grid size + bbox aligned to grid
# -------------------------------
grid_size <- 0.25

bb <- st_bbox(a1)
xmin <- floor(as.numeric(bb["xmin"]) / grid_size) * grid_size
ymin <- floor(as.numeric(bb["ymin"]) / grid_size) * grid_size
xmax <- ceiling(as.numeric(bb["xmax"]) / grid_size) * grid_size
ymax <- ceiling(as.numeric(bb["ymax"]) / grid_size) * grid_size

stopifnot(all(is.finite(c(xmin, ymin, xmax, ymax))))  # sanity check

# -------------------------------
# 2) Make the grid (sf polygons)
# -------------------------------
extent_bbox <- st_bbox(
  c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax),
  crs = st_crs(a1)
)

grid <- st_make_grid(
  extent_bbox,
  cellsize = c(grid_size, grid_size),
  what = "polygons",
  square = TRUE
)

# -------------------------------
# 3) Assign lon_bin & lat_bin to grid cells
# -------------------------------
cent <- st_coordinates(st_point_on_surface(grid))
grid_sf <- st_sf(geometry = grid) |>
  mutate(
    lon_bin = cent[, 1] - grid_size / 2,
    lat_bin = cent[, 2] - grid_size / 2
  )

# -------------------------------
# 4) Aggregate fishing hours by bins
# -------------------------------
coords <- st_coordinates(a1)
a1_bins <- a1 |>
  mutate(
    lon = coords[, 1],
    lat = coords[, 2],
    lon_bin = floor(lon / grid_size) * grid_size,
    lat_bin = floor(lat / grid_size) * grid_size
  ) |>
  st_drop_geometry()

df_grid <- a1_bins |>
  group_by(lon_bin, lat_bin) |>
  summarise(
    total_fishing_hours = sum(fishing_hours_sum, na.rm = TRUE),
    .groups = "drop"
  )

# -------------------------------
# 5) Join aggregated values to polygons
# -------------------------------
df_grid_sf <- grid_sf |>
  left_join(df_grid, by = c("lon_bin", "lat_bin"))


mzc_sf_lat <- get_world_latlon()

log_breaks <- c(1, 10, 100, 1e3, 1e4, 1e5)

# 4) Plot with RColorBrewer + log scaling
ggtest <- ggplot() +
  geom_sf(data = df_grid_sf, aes(fill = total_fishing_hours), color = NA) +
  scale_fill_distiller(
    palette = "GnBu",
    direction = 1,
    trans = "log10",                                # LOG SCALE ✅
    breaks = log_breaks,
    labels = label_number(accuracy = 1, big.mark = ","),
    na.value = "white",                             # WHITE CELLS FOR NAs ✅
    guide = guide_colourbar(
      title.position = "top",
      title = "Fishing Hours (log scale)"
    )
  ) +
  geom_sf(data = mzc_sf_lat, linewidth = 0.2, fill = "grey20", color = "grey30") +
  coord_sf(
    xlim = c(30, 60),   # Longitude limits
    ylim = c(-30, 0),   # Latitude limits
    expand = FALSE
  ) +
  labs(
    title = "Fishing Effort Heatmap (Log Scale)",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    legend.position = "right"
  )

ggsave("outputs/ztest_02.pdf", plot = ggtest, width = 10, height = 10, dpi = 300, limitsize = FALSE)


