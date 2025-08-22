# Activate renv 
if (requireNamespace("renv", quietly = TRUE)) try(renv::activate(), silent = TRUE)

source("R/load_packages.R")
source("R/utils_helpers.R")
source("R/neardist_sps.R")

res <- neardist_track_dir_parallel(
  fsle_path  = "data-raw/fronts_dynamical/FSLE_SWIO_2019-02.tif",  # or BOAonMUR_2018-05.tif
  track_dir  = "data-raw/NosyVe_02/",         # folder with many .rds across years
  output_dir = "outputs/",
  cutoff     = 0.75,
  meters_crs = "ESRI:54030",
  n_cores    = 5
)


