library(arrow)
library(dplyr)

path <- "data-raw/gfw_data_by_flag_and_gear_v20250820.parquet"  # or a directory of parquet files
ds <- open_dataset(path)       # lazy, no full load

# 1) Column names (no read)
names(ds)
# or
ds$schema

# 2) Date range (only scans the date column)
date_col <- "date"  # change if needed
ds %>%
  summarise(
    min_date = min(!!rlang::sym(date_col), na.rm = TRUE),
    max_date = max(!!rlang::sym(date_col), na.rm = TRUE)
  ) %>%
  collect()

# 2016-01-01 2023-12-31
