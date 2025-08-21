a <- readRDS("data-raw/gfw_mzc_rds/GFWSF_fleet-daily-csvs-100-v2-2020.rds")
names(a)
range(a$fishing_hours)


gwf_files <- list.files(
  path = "data-raw/gfw_mzc_rds", 
  pattern = ".rds", 
  all.files = TRUE, 
  full.names = TRUE, 
  recursive = FALSE)
gfw_yr_ls <- vector("list", length = length(gwf_files))
for(i in seq_along(gfw_yr_ls)) {
  gfw_yr_ls[[i]] <- readRDS(gwf_files[i])}
