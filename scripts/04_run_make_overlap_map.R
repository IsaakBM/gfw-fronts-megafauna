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
  bbox         = c(30, -30, 60, 0),
  tracks_dir   = "outputs/tracks",
  out_dir_fig  = "outputs/figures",
  out_dir_tab  = "outputs/tables",
  out_png      = "outputs/figures/overlap_gfw_species.png",
  out_csv      = "outputs/tables/overlap_gfw_species_summary.csv",
  dpi          = 300,
  width_in     = 8,
  height_in    = 7
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
gfw <- classify_fishing_effort(agg)

# -----------------------------------------------------------------------------
# ---- 02) Aggregate species positions in strong fronts -----------------------
#         (InStrongFront == TRUE → counts per grid cell)
# -----------------------------------------------------------------------------
DFF <- read_tracks_outputs(base_dir = params$tracks_dir)
sps <- grid_aggregate_strongfront_sf(
  x         = DFF,
  grid_size = params$grid_size,
  bbox      = params$bbox
)

# -----------------------------------------------------------------------------
# ---- 03) Merge layers + build masks + compute overlap metrics ---------------
#         (High-effort cells, species-front cells, and their intersection)
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

# Save annotated summary
readr::write_csv(overlap_summary, params$out_csv)

# -----------------------------------------------------------------------------
# ---- 04) Plot overlap map ---------------------------------------------------
#         (High effort, species-only, and overlap categories)
# -----------------------------------------------------------------------------
mzc_sf_lat <- get_world_latlon()
# ensure same CRS
mzc_sf_lat <- sf::st_transform(mzc_sf_lat, sf::st_crs(plot_df))

xlim <- c(30, 65)
ylim <- c(-35, 0)

# split layers so we control order; drop "None"
plot_high    <- dplyr::filter(plot_df, category == "High fishing effort")
plot_species <- dplyr::filter(plot_df, category == "Species in fronts")
plot_overlap <- dplyr::filter(plot_df, category == "Overlap")

ggtest <- ggplot2::ggplot() +
  # draw grid layers first (species, then high, then overlap on top)
  ggplot2::geom_sf(data = plot_species, ggplot2::aes(fill = "Species in fronts"), color = NA) +
  ggplot2::geom_sf(data = plot_high,    ggplot2::aes(fill = "High fishing effort"), color = NA) +
  ggplot2::geom_sf(data = plot_overlap, ggplot2::aes(fill = "Overlap"),             color = NA) +
  # coastline ON TOP so edges are visible
  ggplot2::geom_sf(data = mzc_sf_lat, linewidth = 0.2, fill = "grey90", color = "grey30") +
  ggplot2::scale_fill_manual(
    values = c(
      "Overlap"             = "#d73027",
      "High fishing effort" = "#fc8d59",
      "Species in fronts"   = "#4575b4"
    ),
    breaks = c("Overlap", "High fishing effort", "Species in fronts"),
    name   = "Overlap Between Species in Fronts<br/>and High Fishing Effort",
    guide  = ggplot2::guide_legend(
      title.position = "top",
      title.theme    = ggtext::element_markdown(hjust = 0)
    )
  ) +
  ggplot2::coord_sf(xlim = xlim, ylim = ylim, expand = FALSE, datum = NA) +
  ggplot2::labs(title = "", x = "", y = "") +
  ggplot2::theme_minimal(base_size = 13) +
  ggplot2::theme(
    panel.grid      = ggplot2::element_blank(),
    legend.position = "right",
    legend.title    = ggplot2::element_text(hjust = 0),
    legend.text     = ggplot2::element_text(hjust = 0)
  )



ggsave(
  filename = params$out_png,
  plot     = p,
  width    = params$width_in,
  height   = params$height_in,
  dpi      = params$dpi
)
message("Saved figure: ", params$out_png)

# -----------------------------------------------------------------------------
# End
# -----------------------------------------------------------------------------
