# Activate renv 
if (requireNamespace("renv", quietly = TRUE)) try(renv::activate(), silent = TRUE)
source("R/load_packages.R")
source("R/utils_helpers.R")
source("R/read_tracks.R")
source("R/grid_aggregate_gfw.R")
source("R/plot_species_in_fronts_effort.R")



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

test02 <- make_front_species_plot(
  tracks_dir = "outputs/tracks",
  product    = "fsle",
  grid_file  = "data-raw/agg_cell_gear_mzc_rob.rds",
  # gears      = "drifting_longlines",
  grid_size  = 0.10,
  xlim       = c(30, 65),
  ylim       = c(-35, 0),
  output_dir = "outputs",
  width      = 10, height = 10, dpi = 300,
)

patch_test <- patchwork::wrap_plots(test02, test01, ncol = 2, byrow = TRUE) +
  plot_annotation(title = "") + 
  plot_layout(guides = "collect") +
  plot_annotation(tag_prefix = "(",
                  tag_levels = "a", 
                  tag_suffix = ")")
ggsave("outputs/drifting_longlines_figv01.png", plot = patch_test, width = 20, height = 10, dpi = 400)

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
ggsave("outputs/figures/final/all-gears_all-fronts_figv04.png", plot = test03, width = 10, height = 10, dpi = 400)
