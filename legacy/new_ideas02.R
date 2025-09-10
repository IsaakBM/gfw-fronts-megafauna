asdf <- ggplot2::ggplot() +
  # ---------------------------------------------------------------------------
# 1) Gridded layers (draw in correct order: medium → high → species → overlap)
# ---------------------------------------------------------------------------
# Medium fishing effort layer (bottom of fishing)
ggplot2::geom_sf(
  data = dplyr::filter(gfw, fishing_hours_cat == "Medium"),
  ggplot2::aes(fill = "Medium fishing effort"),
  color = NA,
  alpha = 0.7
) +
  # High fishing effort layer (above medium)
  ggplot2::geom_sf(
    data = plot_high,
    ggplot2::aes(fill = "High fishing effort"),
    color = NA,
    alpha = 0.7
  ) +
  # # Species layer (now above all fishing effort)
  # ggplot2::geom_sf(
  #   data = plot_species,
  #   ggplot2::aes(fill = "Species in fronts"),
  #   color = NA
  # ) +
  # Overlap layer (absolute top, highlighted in pink)
  ggplot2::geom_sf(
    data = plot_overlap,
    ggplot2::aes(fill = "Overlap"),
    color = NA
  ) +
  # ---------------------------------------------------------------------------
# 2) Basemap (land + borders)
# ---------------------------------------------------------------------------
ggplot2::geom_sf(
  data = mzc_sf_lat,
  linewidth = 0.2,
  fill = "grey20",
  color = "grey30"
) +
  # ---------------------------------------------------------------------------
# 3) Combined fill scale & legend
# ---------------------------------------------------------------------------
ggplot2::scale_fill_manual(
  values = c(
    "Overlap"               = "purple",  # pink for overlap
    "High fishing effort"   = "#f03b20",  # orange for high effort
    "Medium fishing effort" = "#feb24c",  # yellow-orange for medium effort
    "Species in fronts"     = "#4575b4"   # blue for species
  ),
  breaks = c("Overlap", "High fishing effort", "Medium fishing effort", "Species in fronts"),
  name   = "Overlap Between Species in Fronts<br/>and Fishing Effort",
  guide  = ggplot2::guide_legend(
    title.position = "top",
    title.theme    = ggtext::element_markdown(hjust = 0),
    nrow = 4   # vertical stacking: Overlap → High → Medium → Species
  )
) +
  # ---------------------------------------------------------------------------
# 4) Islands (stars + labels)
# ---------------------------------------------------------------------------
ggplot2::geom_point(
  data  = islands_lbl_df,
  ggplot2::aes(x = lon, y = lat),
  shape = 23,
  fill  = "green",
  color = "black",
  size = 6,
  stroke = 0.5
) +
  ggtext::geom_richtext(
    data  = islands_lbl_df,
    ggplot2::aes(x = lon, y = lat, label = name),
    label.color   = "black",
    fill          = "white",
    size          = 3.2,
    fontface      = "bold",
    vjust         = -1.2,
    label.padding = grid::unit(c(1.5, 2, 1.5, 2), "pt"),
    label.r       = grid::unit(2, "pt")
  ) +
  # ---------------------------------------------------------------------------
# 5) Viewport, labels, and theme
# ---------------------------------------------------------------------------
ggplot2::coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  ggplot2::labs(title = "", x = "", y = "") +
  ggplot2::theme_minimal(base_size = 13) +
  ggplot2::theme(
    plot.background  = ggplot2::element_rect(fill = "white", colour = NA),
    panel.background = ggplot2::element_rect(fill = "white", colour = NA),
    panel.grid       = ggplot2::element_blank(),
    axis.text        = ggplot2::element_text(color = "grey70"),
    axis.ticks       = ggplot2::element_line(color = "grey50"),
    legend.position  = "right",
    legend.title     = ggplot2::element_text(
      hjust = 0,
      color = "black",
      size = 9,
      face = "bold"
    ),
    legend.text      = ggplot2::element_text(
      color = "black",
      size = 9
    )
  )
  
  ggsave(
    filename = "outputs/asdf.pdf",
    plot     = asdf,
    width    = params$width_in,
    height   = params$height_in,
    dpi      = params$dpi
  )
