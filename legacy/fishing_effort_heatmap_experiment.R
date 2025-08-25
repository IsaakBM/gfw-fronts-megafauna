library(sf)
library(dplyr)

a1 <- readRDS("data-raw/agg_cell_gear_mzc_rob.rds")
a1 <- st_as_sf(a1, coords = c("lon", "lat"), crs = 4326)
if (st_crs(a1)$epsg != 4326) a1 <- st_transform(a1, 4326)

grid_size <- 0.10 #0.01 takes time
sf::st_crs(a1) <- 4326

scale_fac <- 1 / grid_size

# aligned bbox
bb  <- sf::st_bbox(a1)
xmin <- floor(as.numeric(bb["xmin"]) * scale_fac) / scale_fac
ymin <- floor(as.numeric(bb["ymin"]) * scale_fac) / scale_fac
xmax <- ceiling(as.numeric(bb["xmax"]) * scale_fac) / scale_fac
ymax <- ceiling(as.numeric(bb["ymax"]) * scale_fac) / scale_fac

nx <- as.integer(round((xmax - xmin) * scale_fac))
ny <- as.integer(round((ymax - ymin) * scale_fac))

# --- grid polygons (no float drift) ---
extent_bbox <- sf::st_bbox(c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax), crs = 4326)
grid <- sf::st_make_grid(extent_bbox, n = c(nx, ny), what = "polygons", square = TRUE)

# centers of each cell
cent <- sf::st_coordinates(sf::st_centroid(grid))

# derive exact integer indices for each cell from its center:
# center_x = xmin + ix*Δ + Δ/2  =>  ix = round((center_x - xmin)/Δ - 0.5)
ix_g <- as.integer(round((cent[,1] - xmin)/grid_size - 0.5))
iy_g <- as.integer(round((cent[,2] - ymin)/grid_size - 0.5))

grid_sf <- sf::st_sf(ix = ix_g, iy = iy_g, geometry = grid) |>
  dplyr::mutate(
    lon_bin = xmin + ix * grid_size,
    lat_bin = ymin + iy * grid_size
  )

# --- points -> integer bin indices (floor) ---
coords <- sf::st_coordinates(a1)
pts <- a1 |>
  dplyr::mutate(
    ix = as.integer(floor((coords[,1] - xmin) * scale_fac)),
    iy = as.integer(floor((coords[,2] - ymin) * scale_fac))
  ) |>
  sf::st_drop_geometry()

df_grid <- pts |>
  dplyr::group_by(ix, iy) |>
  dplyr::summarise(total_fishing_hours = sum(fishing_hours_sum, na.rm = TRUE), .groups = "drop")

# --- join by integer keys (no float joins) ---
df_grid_sf <- grid_sf |>
  dplyr::left_join(df_grid, by = c("ix","iy"))



# -----

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

ggsave("outputs/ztest_03.pdf", plot = ggtest, width = 10, height = 10, dpi = 300, limitsize = FALSE)
