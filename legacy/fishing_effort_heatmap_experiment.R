a1 <- readRDS("data-raw/gfw_mzc_rds/GFWSF_fleet-daily-csvs-100-v2-2019.rds")
# If your data is an sf object, extract lon/lat first:
a1_coords <- a1 %>%
  mutate(lon = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2])
# Aggregate fishing hours by grid cell
df_grid <- a1_coords %>%
  mutate(
    lon_bin = floor(lon / grid_size) * grid_size,
    lat_bin = floor(lat / grid_size) * grid_size
  ) %>%
  group_by(lon_bin, lat_bin) %>%
  summarise(total_fishing_hours = sum(fishing_hours, na.rm = TRUE), .groups = "drop")

# Robinson projection
robin <- "+proj=robin +lon_0=0 +datum=WGS84 +units=m +no_defs"
grid_size <- 0.25  # size of lon/lat bins

# 1) Build sf polygons for grid cells (WGS84 first)
df_grid_sf <- df_grid %>%
  mutate(
    xmin = lon_bin,
    xmax = lon_bin + grid_size,
    ymin = lat_bin,
    ymax = lat_bin + grid_size
  ) %>%
  rowwise() %>%
  mutate(
    geometry = st_as_sfc(
      st_bbox(c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax), crs = 4326)
    )
  ) %>%
  ungroup() %>%
  st_as_sf()

# 2) Reproject polygons to Robinson
df_grid_robin <- st_transform(df_grid_sf, crs = robin)


# 3) Log-scale breaks for better readability
log_breaks <- c(1, 10, 100, 1e3, 1e4, 1e5)

# 4) Plot with RColorBrewer + log scaling
ggtest <- ggplot() +
  geom_sf(data = mzc_sf_Rob, linewidth = 0.2, fill = "grey20", color = "grey30") +
  geom_sf(data = df_grid_robin, aes(fill = total_fishing_hours), color = NA) +
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
  coord_sf(expand = FALSE) +
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

ggsave("outputs/ztest_01.pdf", plot = ggtest, width = 10, height = 10, dpi = 300, limitsize = FALSE)
