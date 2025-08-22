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


wsT <- readRDS("data-raw/mm_mmf_02/mmf_WhaleShark_2011-08_71214.rds")
sbT1 <- readRDS("data-raw/NosyVe_02/NosyVe_2018-05_EA583867.rds")
sbT2 <- readRDS("data-raw/WTSH_02/WTSH_2017-11_FX20845.rds")
trtT <- readRDS("data-raw/TMNT_02v02/TMNT_2019-01_657110.rds")

readRDS("outputs/gfw_fsle_2019-01_cutoff-0.75.rds")
unique(wsT$ptt)
