# Activate renv 
if (requireNamespace("renv", quietly = TRUE)) try(renv::activate(), silent = TRUE)

source("R/load_packages.R")
source("R/utils_helpers.R")
source("R/neardist.R")

# cfg <- yaml::read_yaml("config/default.yml")

res <- neardist_gfw_dir_parallel(
  fsle_path   = "data_rout/FSLE_SWIO_2019-01.tif",
  gfw_dir     = "data_rout/gfw_test/",
  cutoff      = 0.75,
  meters_crs  = "ESRI:54030",
  ncores      = 5,
  output_dir  = "outputs/",  # can be NULL
  file_prefix = "gfw",
  front_name  = NULL   # can be NULL
)

