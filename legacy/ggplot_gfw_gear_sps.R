

g_dll <- grid_aggregate_sf("data-raw/agg_cell_gear_mzc_rob.rds", grid_size = 0.10, gears = "drifting_longlines")

mzc_sf_lat <- get_world_latlon()
log_breaks <- c(1, 10, 100, 1e3, 1e4, 1e5)

# 4) Plot with RColorBrewer + log scaling
ggtest <- ggplot() +
  geom_sf(data = g_dll, aes(fill = total_fishing_hours), color = NA) +
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
    xlim = c(30, 65),   # Longitude limits
    ylim = c(-35, 0),   # Latitude limits
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

ggsave("outputs/ztest_03_drifting_longlines.pdf", plot = ggtest, width = 10, height = 10, dpi = 300, limitsize = FALSE)
