###############################################################################
# 04_run_make_overlap_map.R
# Purpose: Generate map + metrics of overlap (High GFW effort × species-in-fronts)
# Notes  : Uses project functions + central load_packages.R for libraries.
###############################################################################

# ---- 00) Setup ---------------------------------------------------------------
# Activate renv (if available) and source required scripts
if (requireNamespace("renv", quietly = TRUE)) try(renv::activate(), silent = TRUE)

# Load packages from central manager
source("R/load_packages.R")
source("R/utils_helpers.R")
# Source project functions
source("R/grid_aggregate_gfw.R")
source("R/grid_aggregate_sps.R")
source("R/classify_fishing_effort.R")
source("R/read_tracks.R")

# Parameters (edit as needed)
params <- list(
  rds_path     = "data-raw/agg_cell_gear_mzc_rob.rds",
  grid_size    = 0.10,
  gears        = c("drifting_longlines","tuna_purse_seines","set_longlines",
                   "other_purse_seines","purse_seines"),
  crs          = 4326,
  bbox         = c(30, -35, 65, 0),
  tracks_dir   = "outputs/tracks",
  out_dir_fig  = "outputs/figures",
  out_dir_tab  = "outputs/tables",
  out_pdf      = "outputs/figures/BritoMorales_Fi_5.pdf",
  out_csv      = "outputs/tables/overlap_gfw_species_summary.csv",
  dpi          = 400,
  width_in     = 10,
  height_in    = 10
)

# -----------------------------------------------------------------------------
# ---- 01) Aggregate & classify GFW effort ------------------------------------
#         (grid aggregation + Low/Medium/High categories)
# -----------------------------------------------------------------------------
agg <- grid_aggregate_sf(
  rds_path  = params$rds_path,
  grid_size = params$grid_size,
  gears     = params$gears,
  crs       = params$crs,
  bbox      = params$bbox
)
gfw <- classify_fishing_effort(agg) #### here the plot missing!

# -----------------------------------------------------------------------------
# ---- 02) Aggregate species positions in strong fronts -----------------------
#         (InStrongFront == TRUE → counts per grid cell)
# -----------------------------------------------------------------------------
DFF_tracks <- read_tracks_outputs(base_dir = params$tracks_dir)
DFF_aerial <- read_tracks_outputs(base_dir = "outputs/aerial")
DFF <- rbind(DFF_tracks, DFF_aerial)
sps <- grid_aggregate_strongfront_sf(
  x         = DFF,
  grid_size = params$grid_size,
  bbox      = params$bbox
)

# -----------------------------------------------------------------------------
# ---- 03) Merge layers + build masks + compute overlap metrics ---------------
#         (High-effort cells, species-front cells, their intersection,
#          and percentages relative to the total number of grid cells)
# -----------------------------------------------------------------------------
plot_df <- gfw %>%
  left_join(
    sps %>%
      st_drop_geometry() %>%
      select(ix, iy, strong_count),
    by = c("ix","iy")
  ) %>%
  mutate(
    high_effort   = fishing_hours_cat == "High",
    species_front = !is.na(strong_count) & strong_count > 0,
    overlap       = high_effort & species_front,
    category      = dplyr::case_when(
      overlap       ~ "Overlap",
      high_effort   ~ "High fishing effort",
      species_front ~ "Species in fronts",
      TRUE          ~ "None"
    )
  )

# Cell-count metrics (uniform grid area)
A <- sum(plot_df$high_effort,   na.rm = TRUE)   # High-effort cells
B <- sum(plot_df$species_front, na.rm = TRUE)   # Species-in-front cells
I <- sum(plot_df$overlap,       na.rm = TRUE)   # Overlap cells

P_high    <- if (A > 0) 100 * I / A else NA_real_  # % of High cells with species
P_species <- if (B > 0) 100 * I / B else NA_real_  # % of species cells in High
Jaccard   <- if ((A + B - I) > 0) I / (A + B - I) else NA_real_

overlap_summary <- tibble::tibble(
  metric = c(
    "high_cells",
    "species_cells",
    "overlap_cells",
    "pct_high_with_species",
    "pct_species_in_high",
    "jaccard_iou"
  ),
  value = c(
    A,
    B,
    I,
    P_high,
    P_species,
    Jaccard
  ),
  description = c(
    "Total number of grid cells classified as High fishing effort",
    "Total number of grid cells with species present in strong fronts",
    "Number of grid cells where species presence and High fishing effort overlap",
    "Among all High fishing effort cells, the % that overlap with species presence (fisheries exposure)",
    "Among all species-in-front cells, the % that overlap with High fishing effort (local species risk)",
    "Jaccard index: ratio of overlap cells relative to the union of High-effort and species-in-front cells"
  )
)

# ---- Add total grid cell metrics ----
N_total <- nrow(plot_df)

pct_high_of_total    <- if (N_total > 0) 100 * A / N_total else NA_real_
pct_species_of_total <- if (N_total > 0) 100 * B / N_total else NA_real_
pct_overlap_of_total <- if (N_total > 0) 100 * I / N_total else NA_real_

overlap_summary <- dplyr::bind_rows(
  overlap_summary,
  tibble::tibble(
    metric = c(
      "total_cells",
      "pct_of_total_high_cells",
      "pct_of_total_species_cells",
      "pct_of_total_overlap_cells"
    ),
    value = c(
      N_total,
      pct_high_of_total,
      pct_species_of_total,
      pct_overlap_of_total
    ),
    description = c(
      "Total number of grid cells in the study grid (map extent)",
      "Share of all grid cells that are High fishing effort",
      "Share of all grid cells that contain species in strong fronts",
      "Share of all grid cells where High fishing effort and species fronts overlap"
    )
  )
)

# Save annotated summary
readr::write_csv(overlap_summary, params$out_csv)

# -----------------------------------------------------------------------------
# ---- 04) Plot overlap map ---------------------------------------------------
#         (High effort, species-only, and overlap categories)
# -----------------------------------------------------------------------------
# basemap in same CRS as plot_df
mzc_sf_lat <- get_world_latlon() |> sf::st_transform(sf::st_crs(plot_df))
# island in same CRS as plot_df
islands_lbl <- sf::st_transform(islands_lbl, sf::st_crs(plot_df))

xlim <- c(30, 65)
ylim <- c(-35, 0)

plot_high    <- dplyr::filter(plot_df, category == "High fishing effort")
plot_species <- dplyr::filter(plot_df, category == "Species in fronts")
plot_overlap <- dplyr::filter(plot_df, category == "Overlap")

ggtest <- ggplot2::ggplot() +
  # ---------------------------------------------------------------------------
  # 1) Gridded layers (draw in order: species → high effort → overlap)
  # ---------------------------------------------------------------------------
  ggplot2::geom_sf(
    data = plot_species,
    ggplot2::aes(fill = "Species in fronts"),
    color = NA
  ) +
    ggplot2::geom_sf(
      data = plot_high,
      ggplot2::aes(fill = "High fishing effort"),
      color = NA
    ) +
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
  # 3) Fill scale & legend
  # ---------------------------------------------------------------------------
  ggplot2::scale_fill_manual(
    values = c(
      "Overlap"              = "#d73027",
      "High fishing effort"  = "#fc8d59",
      "Species in fronts"    = "#4575b4"
    ),
    breaks = c("Overlap", "High fishing effort", "Species in fronts"),
    name   = "Overlap Between Species in Fronts<br/>and High Fishing Effort",
    guide  = ggplot2::guide_legend(
      title.position = "top",        # title above the legend
      title.theme    = ggtext::element_markdown(hjust = 0.5),
      nrow = 1                      #  legend horizontal
    )
  ) +
  # ---------------------------------------------------------------------------
  # 4) Islands (stars + labels)
  # ---------------------------------------------------------------------------
  ggplot2::geom_point(
    data  = islands_lbl_df,
    ggplot2::aes(x = lon, y = lat),
    shape = 23,
    fill  = "green3",
    color = "black",
    size  = 4,
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
      # <-- CHANGE LEGEND POSITION TO BOTTOM -->
      legend.position  = "bottom",
      legend.title     = ggplot2::element_text(
        hjust = 0.5,
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
    filename = params$out_pdf,
    plot     = ggtest,
    width    = params$width_in,
    height   = params$height_in,
    dpi      = params$dpi
  )
  message("Saved figure: ", params$out_png)

# -----------------------------------------------------------------------------
# ---- 05) Plot fishing effort categories --------------------------------------
#         (Low / Medium / High effort classification from GFW data)
# -----------------------------------------------------------------------------

p_fishcat <- ggplot2::ggplot() +
  # ---------------------------------------------------------------------------
  # 1) Categorical fishing effort layer
  # ---------------------------------------------------------------------------
  ggplot2::geom_sf(
    data  = gfw,
    ggplot2::aes(fill = fishing_hours_cat),
    color = NA
  ) +
  # ---------------------------------------------------------------------------
  # 2) Landmask + borders (black look)
  # ---------------------------------------------------------------------------
  ggplot2::geom_sf(
    data = mzc_sf_lat,
    linewidth = 0.2,
    fill = "black",
    color = "black"
  ) +
  # ---------------------------------------------------------------------------
  # 3) Fishing effort palette (3-class “RdYlBu”-style)
  # ---------------------------------------------------------------------------
  ggplot2::scale_fill_manual(
    values = c(
      "Low"    = "#ffeda0", # low effort
      "Medium" = "#feb24c", # medium effort
      "High"   = "#f03b20"  # high effort
    ),
    breaks   = c("High", "Medium", "Low"),  # force "High" first
    na.value = "white",
    name     = "Fishing Effort (categorical)"
  ) +
  # ---------------------------------------------------------------------------
  # 4) Island markers + labels
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
  # 5) Viewport, labels, and theme
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

ggsave(
  filename = "outputs/figures/final/BritoMorales_ED_Fi_11.pdf",
  plot     = p_fishcat,
  width    = params$width_in,
  height   = params$height_in,
  dpi      = params$dpi
)
message("Saved figure: ", params$out_png)

# -----------------------------------------------------------------------------
# End
# -----------------------------------------------------------------------------

