# ============================
# Collect + read output RDS (tracks/*) for a given front product
# Author: Isaac Brito-Morales
# Email: ibrito@conservation.org
# ============================

read_tracks_outputs <- function(base_dir = "outputs/tracks",
                                product  = NULL,                # <-- NULL = read BOTH
                                ignore_case = TRUE) {
  if (!is.null(product)) {
    product <- match.arg(product, c("boa", "fsle"))
  }
  
  # 1) list species subdirs (first level only)
  species_dirs <- list.dirs(path = base_dir, full.names = TRUE, recursive = FALSE)
  
  # 2) list all .rds files within those subdirs
  all_files <- unlist(lapply(species_dirs, function(dir)
    list.files(dir, pattern = "\\.rds$", full.names = TRUE, recursive = FALSE)),
    use.names = FALSE
  )
  
  if (!length(all_files)) {
    message("No .rds files found under: ", base_dir)
    return(tibble())
  }
  
  # 3) filter for product token in filename (e.g., "boa" or "fsle")
  if (is.null(product)) {
    target_files <- all_files                            # <-- read BOTH
  } else {
    rx <- if (ignore_case) regex(product, ignore_case = TRUE) else product
    target_files <- all_files[str_detect(basename(all_files), rx)]
    # base-R alternative:
    # target_files <- all_files[grepl(product, basename(all_files), ignore.case = ignore_case)]
  }
  
  if (!length(target_files)) {
    message("No files matched product '", product, "' under: ", base_dir)
    return(tibble())
  }
  
  # 4) robust reader: works with sf / data.frame / list-wrapped
  read_one <- function(f) {
    x <- tryCatch(readRDS(f), error = function(e) return(NULL))
    if (is.null(x)) return(NULL)
    if (inherits(x, "sf")) x <- sf::st_drop_geometry(x)
    if (is.list(x) && !is.data.frame(x)) {
      # try first element if list-wrapped
      x <- x[[1]]
      if (inherits(x, "sf")) x <- sf::st_drop_geometry(x)
    }
    if (!is.data.frame(x)) return(NULL)
    # attach source file (handy!)
    x <- tibble::as_tibble(x)
    x$track_id <- as.character(x$track_id)
    x$.source_file <- basename(f)
    x
  }
  
  # 5) read and bind
  df_list <- lapply(target_files, read_one)
  df_list <- df_list[!vapply(df_list, is.null, logical(1))]
  
  if (!length(df_list)) {
    message("No readable data.frames among matched files.")
    return(tibble())
  }
  
  out <- dplyr::bind_rows(df_list)
  
  # optional: arrange nicely
  out %>% arrange(date, species, track_id, .by_group = FALSE)
}

# DFF <- read_tracks_outputs(base_dir = "outputs/tracks", product = "boa")
# DFF <- read_tracks_outputs(base_dir = "outputs/tracks")