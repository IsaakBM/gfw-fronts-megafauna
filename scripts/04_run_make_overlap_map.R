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
  crs       = params$crs
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
  high_cells             = A,
  species_cells          = B,
  overlap_cells          = I,
  pct_high_with_species  = P_high,
  pct_species_in_high    = P_species,
  jaccard_iou            = Jaccard
)
readr::write_csv(overlap_summary, params$out_csv)

# -----------------------------------------------------------------------------
# ---- 04) Plot overlap map ---------------------------------------------------
#         (High effort, species-only, and overlap categories)
# -----------------------------------------------------------------------------

p <- ggplot() +
  geom_sf(
    data = plot_df,
    aes(fill = category),
    color = NA
  ) +
  scale_fill_manual(
    values = c(
      "Overlap"             = "#d73027", # red
      "High fishing effort" = "#fc8d59", # orange
      "Species in fronts"   = "#4575b4", # blue
      "None"                = "grey90"   # light grey
    ),
    breaks = c("Overlap", "High fishing effort", "Species in fronts"),
    na.value = "white",
    guide = guide_legend(
      title = "Overlap Between Species in Fronts<br/>and High Fishing Effort",
      title.position = "top",
      title.theme = ggtext::element_markdown(hjust = 0)
    )
  ) +
  theme_bw() +
  theme(
    legend.position = "right",
    panel.grid      = element_blank(),
    legend.title    = element_text(size = 11),
    legend.text     = element_text(size = 9)
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
