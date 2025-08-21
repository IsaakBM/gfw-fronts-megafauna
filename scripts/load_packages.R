# Centralized package loader for scripts
libs <- c(
  "dplyr","ggplot2","sf","terra","stringr","tibble",
  "rnaturalearth","rnaturalearthdata","RColorBrewer","units"
)
new_libs <- libs[!(libs %in% installed.packages()[, "Package"])]
if (length(new_libs)) install.packages(new_libs)
invisible(lapply(libs, require, character.only = TRUE))
