# Activate renv -----------------------------------------------------------
if (requireNamespace("renv", quietly = TRUE)) try(renv::activate(), silent = TRUE)
source("R/load_packages.R")
source("R/utils_helpers.R")
source("R/neardist_aerial.R")

# Aerial Surveys ---- 2010-01; 2010-02; 2010-03 --------------------

  # Define target dates
  target_dates <- c("2010-01", "2010-02", "2010-03")
  # --- FSLE files ---
  fsle_files <- list.files(
    path = "data-raw/fronts_dynamical",
    pattern = "FSLE_SWIO_.*\\.tif$",
    full.names = TRUE
  )
  fsle_files <- fsle_files[grepl(paste(target_dates, collapse = "|"), fsle_files)]
  # --- BOAonMUR files ---
  boa_files <- list.files(
    path = "data-raw/fronts_thermal",
    pattern = "BOAonMUR_SWIO_.*\\.tif$",
    full.names = TRUE
  )
  boa_files <- boa_files[grepl(paste(target_dates, collapse = "|"), boa_files)]
  # --- Combine both into one vector ---
  all_front_files <- c(fsle_files, boa_files)
  # --- Loop through all matching rasters ---
  # Marine Mammals
  for (i in seq_along(all_front_files)) {
    front_file <- all_front_files[i]
    message(sprintf("Processing %d/%d: %s", i, length(all_front_files), basename(front_file)))  # progress message
    # Call your function
    res <- neardist_aerial_dir_parallel(
      fsle_path  = front_file,
      track_dir  = "data-raw/aerial/marine_mammals/",
      output_dir = "outputs/aerial/marine_mammals/",
      cutoff     = 0.75,
      meters_crs = "ESRI:54030",
      n_cores    = 5
    )
    message("Finished: ", basename(front_file))
  }
  # Seabirds
  for (i in seq_along(all_front_files)) {
    front_file <- all_front_files[i]
    message(sprintf("Processing %d/%d: %s", i, length(all_front_files), basename(front_file)))  # progress message
    # Call your function
    res <- neardist_aerial_dir_parallel(
      fsle_path  = front_file,
      track_dir  = "data-raw/aerial/seabirds/",
      output_dir = "outputs/aerial/seabirds/",
      cutoff     = 0.75,
      meters_crs = "ESRI:54030",
      n_cores    = 5
    )
    message("Finished: ", basename(front_file))
  }
  # other megafauna
  for (i in seq_along(all_front_files)) {
    front_file <- all_front_files[i]
    message(sprintf("Processing %d/%d: %s", i, length(all_front_files), basename(front_file)))  # progress message
    # Call your function
    res <- neardist_aerial_dir_parallel(
      fsle_path  = front_file,
      track_dir  = "data-raw/aerial/other_megafauna/",
      output_dir = "outputs/aerial/other_megafauna/",
      cutoff     = 0.75,
      meters_crs = "ESRI:54030",
      n_cores    = 5
    )
    message("Finished: ", basename(front_file))
  }
  