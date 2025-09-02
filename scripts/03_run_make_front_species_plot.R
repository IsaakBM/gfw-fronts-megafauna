###############################################################################
# 03_run_make_front_species_plot.R
# Purpose: Generate final figures (fronts × species; all fronts; gear types)
# Notes  : Structure-only cleanup—no changes to logic, arguments, or outputs.
###############################################################################

# ---- 00) Setup ---------------------------------------------------------------
# Activate renv (if available) and source required scripts
if (requireNamespace("renv", quietly = TRUE)) try(renv::activate(), silent = TRUE)
source("R/load_packages.R")
source("R/utils_helpers.R")
source("R/read_tracks.R")
source("R/grid_aggregate_gfw.R")
source("R/plot_species_in_fronts_effort.R")

# -----------------------------------------------------------------------------
# ---- 01) Plots: individual front products (with tracks) ---------------------
#         (BOA and FSLE side-by-side, shared tagging)
# -----------------------------------------------------------------------------

# BOA
test01 <- make_front_species_plot(
  tracks_dir = "outputs/tracks",
  product    = "boa",
  grid_file  = "data-raw/agg_cell_gear_mzc_rob.rds",
  # gears      = "drifting_longlines",
  grid_size  = 0.10,
  xlim       = c(30, 65),
  ylim       = c(-35, 0),
  output_dir = "outputs",
  width      = 10, height = 10, dpi = 300
)

# FSLE
test02 <- make_front_species_plot(
  tracks_dir = "outputs/tracks",
  product    = "fsle",
  grid_file  = "data-raw/agg_cell_gear_mzc_rob.rds",
  # gears      = "drifting_longlines",
  grid_size  = 0.10,
  xlim       = c(30, 65),
  ylim       = c(-35, 0),
  output_dir = "outputs",
  width      = 10, height = 10, dpi = 300
)

# Combine BOA + FSLE
patch_test <- patchwork::wrap_plots(test02, test01, ncol = 2, byrow = TRUE) +
  plot_annotation(title = "") +
  plot_layout(guides = "collect") +
  plot_annotation(
    tag_prefix = "(",
    tag_levels = "a",
    tag_suffix = ")"
  )

# Save combined panel
ggsave(
  # filename = "outputs/drifting_longlines_figv01.png",
  plot     = patch_test,
  width    = 20, height = 10, dpi = 400
)

# -----------------------------------------------------------------------------
# ---- 02) Plot: all fronts (no species split) --------------------------------
#         (Figure 5; by_species = FALSE)
# -----------------------------------------------------------------------------

test03 <- make_front_species_plot(
  tracks_dir = "outputs/tracks",
  # product    = "boa",
  grid_file  = "data-raw/agg_cell_gear_mzc_rob.rds",
  # gears      = "drifting_longlines",
  grid_size  = 0.10,
  xlim       = c(30, 65),
  ylim       = c(-35, -5),
  # output_dir = "outputs",
  width      = 10, height = 5, dpi = 300,
  by_species = FALSE
)

# Save (PDF as final)
ggsave(
  filename = "outputs/figures/final/BritoMorales_Fi_5.pdf",
  plot     = test03,
  width    = 10, height = 10, dpi = 400
)

# -----------------------------------------------------------------------------
# ---- 03) Plots: all gear types (no tracks) ----------------------------------
#         (Excludes dredge_fishing; shared scale; grid of panels)
# -----------------------------------------------------------------------------

# 3.1 Define all gear types
gear_list <- c(
  "drifting_longlines","tuna_purse_seines","fishing","trawlers","dredge_fishing",
  "set_longlines","pole_and_line","squid_jigger","fixed_gear","set_gillnets",
  "other_purse_seines","purse_seines"
)

# 3.2 Preallocate a named list and populate with plots
test_plot <- setNames(vector("list", length(gear_list)), gear_list)
for (i in seq_along(gear_list)) {
  g <- gear_list[i]
  message("Processing gear: ", g)
  
  test_plot[[g]] <- make_front_species_plot(
    tracks_dir = NULL,
    grid_file  = "data-raw/agg_cell_gear_mzc_rob.rds",
    gears      = g,
    grid_size  = 0.10,
    xlim       = c(30, 65),
    ylim       = c(-35, -5),
    # output_dir = "outputs/gear_plots",
    width      = 10, height = 5, dpi = 300,
    by_species = FALSE
  )
}

# 3.3 Shared fill scale (identical across all panels)
effort_scale <- scale_fill_distiller(
  palette  = "Reds",   # "GnBu"
  direction = 1,
  trans    = "log10",
  breaks   = c(1, 10, 100, 1e3, 1e4, 1e5),
  limits   = c(1, 1e5),  # force identical range across all children
  labels   = scales::label_number(accuracy = 1, big.mark = ","),
  na.value = "white",
  name     = "Fishing Effort\nby 10 km² (log[10])",
  guide    = guide_colorbar(title.position = "top")
)

# 3.4 Exclude dredge_fishing
test_plot_nodredge <- test_plot[names(test_plot) != "dredge_fishing"]

# 3.5 Human-friendly titles
nice_names <- c(
  drifting_longlines = "Drifting longlines",
  tuna_purse_seines  = "Tuna purse seines",
  fishing            = "Fishing (unspecified)",
  trawlers           = "Trawlers",
  dredge_fishing     = "Dredge fishing",
  set_longlines      = "Set longlines",
  pole_and_line      = "Pole-and-line",
  squid_jigger       = "Squid jiggers",
  fixed_gear         = "Fixed gear",
  set_gillnets       = "Set gillnets",
  other_purse_seines = "Other purse seines",
  purse_seines       = "Purse seines"
)

# 3.6 Apply shared scale + titles + light styling
test_plot_fixed <- lapply(seq_along(test_plot_nodredge), function(i) {
  p  <- test_plot_nodredge[[i]]
  nm <- nice_names[names(test_plot_nodredge)[i]]
  
  p + effort_scale + guides(fill = guide_colorbar()) +
    labs(title = nm) +
    theme(
      legend.position = "right",
      plot.title = element_text(
        hjust = 0.5,
        face  = "bold",
        size  = 10,
        margin = margin(b = 3)
      )
    )
})

# Keep names consistent for later references
names(test_plot_fixed) <- names(test_plot_nodredge)

# 3.7 Assemble gear grid and save
patch_all_gt <- wrap_plots(plotlist = test_plot_fixed, ncol = 4, byrow = TRUE) +
  plot_layout(guides = "collect") +
  plot_annotation(
    tag_prefix = "(",
    tag_levels = "a",
    tag_suffix = ")"
  )
ggsave(
  filename  = "outputs/figures/final/BritoMorales_ED_Fi_9.png",
  plot      = patch_all_gt,
  width     = 20, height = 20, dpi = 400, limitsize = FALSE
)
ggsave(
  filename  = "outputs/figures/final/BritoMorales_ED_Fi_9.pdf",
  plot      = patch_all_gt,
  width     = 20, height = 20, dpi = 400, limitsize = FALSE
)

# -----------------------------------------------------------------------------
# End
# -----------------------------------------------------------------------------