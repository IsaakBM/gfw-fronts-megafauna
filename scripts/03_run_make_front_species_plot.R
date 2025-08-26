# Activate renv 
if (requireNamespace("renv", quietly = TRUE)) try(renv::activate(), silent = TRUE)
source("R/load_packages.R")
source("R/utils_helpers.R")
source("R/read_tracks.R")
source("R/grid_aggregate_gfw.R")
source("R/plot_species_in_fronts_effort.R")



# make_front_species_plot(product = "fsle", gears = "drifting_longlines")

make_front_species_plot(
  tracks_dir = "outputs/tracks",
  product    = "boa",
  grid_file  = "data-raw/agg_cell_gear_mzc_rob.rds",
  gears      = "drifting_longlines",
  grid_size  = 0.10,
  xlim       = c(30, 65),
  ylim       = c(-35, 0),
  output_dir = "outputs",
  width      = 10, height = 10, dpi = 300
)
