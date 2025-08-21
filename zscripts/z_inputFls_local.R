# This code was written by Isaac Brito-Morales (ibrito@conservation.org)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

library(terra)
library(sf)
library(ggplot2)
library(RColorBrewer)
library(patchwork)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(gganimate)
library(animation)
library(tidyr)
library(transformr)
library(stringr)
library(readr)
library(data.table)
library(doParallel)
library(foreach)


########################
####### GFW
########################
  
  # gfw <- readRDS("Outputs/GFW_Mzb_rds/fleet_daily/GFWSF_fleet-daily-csvs-100-v2-2019.rds") %>% 
  #   dplyr::filter(fishing_hours >= 5) %>% 
  #   dplyr::filter(geartype == "drifting_longlines")
  # 
  # gfw01 <- gfw %>% 
  #   dplyr::filter(date %in% gfw$date[stringr::str_detect(string = gfw$date, pattern = "2019-01.*")])
  # write_rds(gfw01, "data_rout/gfw_test/gfw_2019-01_driftinglonglines.rds")
  # 
  # 
  # gfw <- readRDS("Outputs/GFW_Mzb_rds/fleet_daily/GFWSF_fleet-daily-csvs-100-v2-2019.rds") %>% 
  #   dplyr::filter(fishing_hours >= 5)
  # gt <- unique(gfw$geartype)
  # ms <- format(seq(as.Date("2019-01-01"), as.Date("2019-12-01"), by = "month"), "%Y-%m")
  # outdir <- "data_rout/gfw_test/"
  # 
  #   for(i in seq_along(gt)) {
  #     
  #     sgl <- gfw %>% 
  #       dplyr::filter(geartype == gt[i])
  #     ms2 <- ms[ms %in% unique(str_sub(sgl$date, 1, 7))]
  #     
  #     for(j in seq_along(ms2)) {
  #       
  #       sgl2 <- sgl %>% 
  #         dplyr::filter(date %in% sgl$date[stringr::str_detect(string = sgl$date, pattern = paste0(ms2[j], ".", "*"))])
  #       write_rds(sgl2, paste0(outdir, "gfw_", ms2[j], "_", str_replace_all(unique(sgl2$geartype), "_", ""), ".rds"))
  #       
  #     }
  #   }
  
  
  
  
  