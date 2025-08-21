# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

source("zscripts/fGFW_csv_to_sf.R")
source("zscripts/z_helpFX.R")
library(doParallel)
library(foreach)

#
  gfw_dir <- list.dirs(path = "Inputs/GFW/mmsi_daily",
  full.names = TRUE, recursive = FALSE)
#
  UseCores <- 3 # nolint
  cl <- makeCluster(UseCores)
  registerDoParallel(cl)
  gfw_list <- foreach(i = 1:length(gfw_dir), # nolint
                      .packages = c("terra",
                      "dplyr",
                      "tidyr",
                      "readr",
                      "stringr",
                      "sf",
                      "ggplot2",
                      "patchwork")) %dopar% {
    gfw_sf(input = gfw_dir[i], output = "Outputs/GFW_Mzb_rds/mmsi_daily/")
    }
  stopCluster(cl)