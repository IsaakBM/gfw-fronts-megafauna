#' Create a Species-in-Fronts Fishing Effort Plot
#'
#' Generates a publication-ready ggplot showing fishing-effort heatmaps and
#' species tracks within oceanic fronts (thermal or dynamical). Tracks are read
#' with [read_tracks_outputs()] and fishing effort is aggregated with
#' [grid_aggregate_sf()]. Legend titles use HTML via **ggtext** (e.g., km² and
#' Log₁₀) and the plot is saved as PNG and PDF.
#'
#' @inheritParams read_tracks_outputs
#' @inheritParams grid_aggregate_sf
#' @param tracks_dir Character. Path to the directory containing track outputs.
#'   Default: `"outputs/tracks"`.
#' @param product Character. Type of fronts: `"boa"` (thermal) or `"fsle"`
#'   (dynamical).
#' @param grid_file Character. Path to aggregated fishing-effort RDS file.
#'   Default: `"data-raw/agg_cell_gear_mzc_rob.rds"`.
#' @param gears Character or `NULL`. Gear filter (e.g., `"drifting_longlines"`).
#'   If `NULL`, uses all gears.
#' @param grid_size Numeric. Grid resolution in decimal degrees. Default `0.10`.
#' @param xlim,ylim Numeric vectors. Map extent. Defaults `c(30, 65)` and
#'   `c(-35, 0)`.
#' @param output_dir Character. Directory to save output plots. Default
#'   `"outputs"`.
#' @param width,height,dpi Numeric. Save dimensions (inches) and resolution.
#'
#' @return Invisibly returns the ggplot object.
#'
#' @seealso [read_tracks_outputs()], [grid_aggregate_sf()]
#'
#' @examples
#' \dontrun{
#' # Thermal (boa), all gears
#' make_front_species_plot(product = "boa")
#'
#' # Dynamical (fsle), drifting longlines only
#' make_front_species_plot(product = "fsle", gears = "drifting_longlines")
#'
#' # Custom output directory
#' make_front_species_plot(product = "boa", output_dir = "outputs/figures")
#' }
#'
#' @import ggplot2
#' @importFrom ggtext element_markdown
#' @importFrom dplyr filter mutate
#' @importFrom sf st_as_sf
#' @importFrom scales label_number
#' @export
#' 
make_front_species_plot <- function(
    tracks_dir   = "outputs/tracks",
    product      = c("boa", "fsle"),
    grid_file    = "data-raw/agg_cell_gear_mzc_rob.rds",
    gears        = NULL,
    grid_size    = 0.10,
    xlim         = c(30, 65),
    ylim         = c(-35, 0),
    output_dir   = "outputs",
    width        = 10,
    height       = 10,
    dpi          = 300
) {
  # ---- setup ----
  product <- match.arg(product)
  front_label <- if (product == "boa") "thermal" else "dynamical"
  gear_label  <- if (is.null(gears) || length(gears) == 0) "all" else gears
  
  # ---- read tracks (points in fronts) ----
  DFF <- read_tracks_outputs(base_dir = tracks_dir, product = product)
  
  # ensure sf if lon/lat present
  if (!inherits(DFF, "sf")) {
    if (all(c("lon", "lat") %in% names(DFF))) {
      DFF <- sf::st_as_sf(DFF, coords = c("lon", "lat"), crs = 4326)
    } else {
      stop("`DFF` is not sf and lon/lat columns are missing.")
    }
  }
  
  # keep only points in strong fronts if column exists
  if ("InStrongFront" %in% names(DFF)) {
    DFF <- dplyr::filter(DFF, InStrongFront == TRUE)
  }
  
  # ---- species order + shapes (robust to extras) ----
  canonical <- c("Whale Shark", "Wedge-tailed shearwater",
                 "Red-tailed tropicbird", "Turtle")
  # keep canonical first, then any others present
  other_levels <- setdiff(sort(unique(as.character(DFF$species))), canonical)
  sp_levels <- c(canonical, other_levels)
  DFF$species <- factor(as.character(DFF$species), levels = sp_levels)
  
  base_shapes <- c(16, 17, 15, 3, 8, 18, 7, 4, 0, 1, 2, 5, 6) # pool if extras appear
  shape_map <- setNames(base_shapes[seq_along(sp_levels)], sp_levels)
  # enforce canonical shapes when present
  fixed_map <- c("Whale Shark" = 16, "Wedge-tailed shearwater" = 17,
                 "Red-tailed tropicbird" = 15, "Turtle" = 3)
  for (nm in intersect(names(fixed_map), names(shape_map))) {
    shape_map[nm] <- fixed_map[nm]
  }
  
  # ---- fishing-effort grid ----
  g_dll <- grid_aggregate_sf(grid_file, grid_size = grid_size, gears = gears)
  
  # ---- basemap & scale breaks ----
  mzc_sf_lat <- get_world_latlon()
  log_breaks <- c(1, 10, 100, 1e3, 1e4, 1e5)
  
  # ---- plot ----
  ggtest <- ggplot2::ggplot() +
    # effort heatmap
    ggplot2::geom_sf(data = g_dll, ggplot2::aes(fill = total_fishing_hours), color = NA) +
    ggplot2::scale_fill_distiller(
      palette = "GnBu", direction = 1, trans = "log10",
      breaks = log_breaks,
      labels = scales::label_number(accuracy = 1, big.mark = ","),
      na.value = "white",
      guide = ggplot2::guide_colourbar(
        title.position = "top",
        title = "Fishing Effort<br/>by 10 km<sup>2</sup> (Log<sub>10</sub> Scale)",
        title.theme = ggtext::element_markdown(hjust = 0)
      )
    ) +
    # basemap
    ggplot2::geom_sf(data = mzc_sf_lat, linewidth = 0.2, fill = "grey20", color = "grey30") +
    # species points: single color, shape by species
    ggplot2::geom_sf(
      data = DFF,
      ggplot2::aes(shape = species),
      color = "black",
      size  = 0.7,
      alpha = 0.55,
      inherit.aes = FALSE
    ) +
    ggplot2::scale_shape_manual(
      values = shape_map,
      name = paste0("Species<br/>(in ", front_label, " fronts)")
    ) +
    ggplot2::guides(
      shape = ggplot2::guide_legend(
        override.aes = list(size = 2, alpha = 1),
        title.position = "top",
        title.theme = ggtext::element_markdown(hjust = 0)
      )
    ) +
    ggplot2::coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    ggplot2::labs(title = "", x = "", y = "") +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      panel.grid      = ggplot2::element_blank(),
      legend.position = "right",
      legend.title    = ggplot2::element_text(hjust = 0),
      legend.text     = ggplot2::element_text(hjust = 0)
    )
  
  # ---- save ----
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  base_name <- file.path(output_dir, sprintf("%s_%s_sps", product, gear_label))
  
  ggplot2::ggsave(
    paste0(base_name, ".png"),
    plot = ggtest, width = width, height = height, dpi = dpi, limitsize = FALSE
  )
  ggplot2::ggsave(
    paste0(base_name, ".pdf"),
    plot = ggtest, width = width, height = height, dpi = dpi,
    limitsize = FALSE, device = cairo_pdf
  )
  
  invisible(ggtest)
}
