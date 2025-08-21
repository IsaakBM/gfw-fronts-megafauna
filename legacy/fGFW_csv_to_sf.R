# This code was written by Isaac Brito-Morales (ibrito@conservation.org)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

# needs a general bbox to extract according to long/lat
gfw_sf <- function(input, output) {
  # List of pacakges that we will use
    list.of.packages <- c("terra", "dplyr", # nolint
    "tidyr", "readr",
    "stringr", "sf",
    "ggplot2", "patchwork")
  # If is not installed, install the pacakge
    new.packages <- list.of.packages[!(list.of.packages %in% # nolint
    installed.packages()[, "Package"])]
    if (length(new.packages)) install.packages(new.packages)
  # Load packages
    lapply(list.of.packages, require, character.only = TRUE)

  #
    files <- list.files(
      path = input,
      pattern = ".csv",
      recursive = FALSE,
      full.names = TRUE)
    t1_ls <- lapply(
      files,
      function(x) {d1 <- read_csv(x, col_types = cols())}) # nolint
    nsm <- basename(files) %>%
      str_remove_all(".csv")
    names(t1_ls) <- nsm
  #
    t1_sf <- lapply(t1_ls, function(x) {
      t1 <- x %>%
        dplyr::filter(cell_ll_lat < -7 & cell_ll_lat >- 30) %>% # nolint
        dplyr::filter(cell_ll_lon > 31 & cell_ll_lon < 60) %>%
        st_as_sf(coords = c("cell_ll_lon", "cell_ll_lat"),
                 crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")}) # nolint
    t1_df <- do.call(rbind, t1_sf)
  #
    saveRDS(t1_df, paste0(output, paste0("GFWSF", "_", basename(input), ".rds"))) # nolint
}