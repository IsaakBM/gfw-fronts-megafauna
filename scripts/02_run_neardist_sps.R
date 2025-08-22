# Activate renv -----------------------------------------------------------
if (requireNamespace("renv", quietly = TRUE)) try(renv::activate(), silent = TRUE)
source("R/load_packages.R")
source("R/utils_helpers.R")
source("R/neardist_sps.R")

# Seabird RTTB ---- 2018-05; 2018-12; 2019-02; 2019-03 --------------------
  
  # Define target dates
  target_dates <- c("2018-05", "2018-12", "2019-02", "2019-03")
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
  for (i in seq_along(all_front_files)) {
    front_file <- all_front_files[i]
    message(sprintf("Processing %d/%d: %s", i, length(all_front_files), basename(front_file)))  # progress message
    # Call your function
    res <- neardist_track_dir_parallel(
      fsle_path  = front_file,
      track_dir  = "data-raw/tracks/RTTB/",   # <<-- change species dir when needed
      output_dir = "outputs/tracks/RTTB/",
      cutoff     = 0.75,
      meters_crs = "ESRI:54030",
      n_cores    = 5
      )
    message("Finished: ", basename(front_file))
  }

# Seabird WTSH ---- 2017-11; 2017-12; 2018-12; 2019-01 --------------------
  
  # Define target dates
  target_dates <- c("2017-11", "2017-12", "2018-12", "2019-01")
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
  for (i in seq_along(all_front_files)) {
    front_file <- all_front_files[i]
    message(sprintf("Processing %d/%d: %s", i, length(all_front_files), basename(front_file)))  # progress message
    # Call your function
    res <- neardist_track_dir_parallel(
      fsle_path  = front_file,
      track_dir  = "data-raw/tracks/WTSH/",   # <<-- change species dir when needed
      output_dir = "outputs/tracks/WTSH/",
      cutoff     = 0.75,
      meters_crs = "ESRI:54030",
      n_cores    = 5
    )
    message("Finished: ", basename(front_file))
  }

# Whale Shark WHSH ---- 2011-07; 2011-08; 2011-09; 2011-10; 2011-11; 2011-12; 2016-10; 2016-11; 2016-12 --------------------
  
  # Define target dates
  target_dates <- c("2011-07", "2011-08", "2011-09", "2011-10", "2011-11", "2011-12", "2016-10", "2016-11", "2016-12")
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
  for (i in seq_along(all_front_files)) {
    front_file <- all_front_files[i]
    message(sprintf("Processing %d/%d: %s", i, length(all_front_files), basename(front_file)))  # progress message
    # Call your function
    res <- neardist_track_dir_parallel(
      fsle_path  = front_file,
      track_dir  = "data-raw/tracks/WHSH/",   # <<-- change species dir when needed
      output_dir = "outputs/tracks/WHSH/",
      cutoff     = 0.75,
      meters_crs = "ESRI:54030",
      n_cores    = 5
    )
    message("Finished: ", basename(front_file))
  }

# Loggerhead turtle LGHT ---- 2019, 2020, 2021 (but 2021 not data for BOA)--------------------
  
  # Define target dates
  dates <- format(seq(as.Date("2019-01-01"), as.Date("2021-12-01"), by = "month"), "%Y-%m")
  target_dates <- dates[!dates %in% c("2019-09", "2020-01", "2020-06", "2020-07")]
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
  for (i in seq_along(all_front_files)) {
    front_file <- all_front_files[i]
    message(sprintf("Processing %d/%d: %s", i, length(all_front_files), basename(front_file)))  # progress message
    # Call your function
    res <- neardist_track_dir_parallel(
      fsle_path  = front_file,
      track_dir  = "data-raw/tracks/LGHT/",   # <<-- change species dir when needed
      output_dir = "outputs/tracks/LGHT/",
      cutoff     = 0.75,
      meters_crs = "ESRI:54030",
      n_cores    = 5
    )
    message("Finished: ", basename(front_file))
  }
  
  