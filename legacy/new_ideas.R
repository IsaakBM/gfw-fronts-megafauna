ggplot2::ggplot() +
  # ---------------------------------------------------------------------------
# 1) Categorical fishing effort layer
# ---------------------------------------------------------------------------
ggplot2::geom_sf(
  data  = gfw,
  ggplot2::aes(fill = fishing_hours_cat),
  color = NA
) +
  # ---------------------------------------------------------------------------
# 2) Overlap layer (pink, semi-transparent, included in legend)
# ---------------------------------------------------------------------------
ggplot2::geom_sf(
  data  = plot_overlap,
  ggplot2::aes(fill = "Overlap"),  # map to aesthetic so it appears in legend
  alpha = 0.6,                     # transparency so fishing effort shows below
  color = NA
) +
  # ---------------------------------------------------------------------------
# 3) Landmask + borders (black look)
# ---------------------------------------------------------------------------
ggplot2::geom_sf(
  data = mzc_sf_lat,
  linewidth = 0.2,
  fill = "black",
  color = "black"
) +
  # ---------------------------------------------------------------------------
# 4) Combined palette: fishing effort + overlap
# ---------------------------------------------------------------------------
ggplot2::scale_fill_manual(
  values = c(
    "Overlap" = "#ff66b2",   # pink for overlap
    "Low"     = "#ffeda0",   # low effort
    "Medium"  = "#feb24c",   # medium effort
    "High"    = "#f03b20"    # high effort
  ),
  breaks = c("Overlap", "High", "Medium", "Low"),  # put overlap on top
  na.value = "white",
  name = "Fishing Effort & Overlap"
) +
  # ---------------------------------------------------------------------------
# 5) Island markers + labels
# ---------------------------------------------------------------------------
# Proper filled star symbols (shape 23 = 5-point filled star)
ggplot2::geom_point(
  data  = islands_lbl_df,
  ggplot2::aes(x = lon, y = lat),
  shape = 23,           # filled star
  fill  = "green3",     # star fill
  color = "black",      # star border
  size  = 4,            # slightly larger
  stroke = 0.5          # thicker outline
) +
  # Labels above stars using ggtext for style consistency
  ggtext::geom_richtext(
    data  = islands_lbl_df,
    ggplot2::aes(x = lon, y = lat, label = name),
    label.color   = "black",                   # black border around text
    fill          = "white",                   # white label background
    size          = 3.2,
    fontface      = "bold",
    vjust         = -1.2,                      # nudge labels above stars
    label.padding = grid::unit(c(1.5, 2, 1.5, 2), "pt"),
    label.r       = grid::unit(2, "pt")
  ) +
  # ---------------------------------------------------------------------------
# 6) Viewport, labels, and theme
# ---------------------------------------------------------------------------
ggplot2::coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  ggplot2::labs(title = "", x = "", y = "") +
  ggplot2::theme_minimal(base_size = 13) +
  ggplot2::theme(
    # --- Panel & background ---
    plot.background  = ggplot2::element_rect(fill = "white", colour = NA),
    panel.background = ggplot2::element_rect(fill = "white", colour = NA),
    panel.grid       = ggplot2::element_blank(),
    # --- Axes ---
    axis.text        = ggplot2::element_text(color = "grey30"),   # clean & soft axis labels
    axis.ticks       = ggplot2::element_line(color = "grey60"),   # subtle tick marks
    # --- Legend ---
    legend.position  = "right",
    legend.title     = ggplot2::element_text(
      hjust = 0,
      color = "black",
      size = 9
    ),
    legend.text      = ggplot2::element_text(
      hjust = 0,
      size = 9,
      color = "black"
    )
  )
