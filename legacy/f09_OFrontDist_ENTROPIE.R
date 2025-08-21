# This code was written by Isaac Brito-Morales (ibrito@conservation.org)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

# source("/home/sandbox-sparc/mmw_belmont/zscripts/z_inputFls.R")
source("zscripts/z_inputFls_local.R")
source("zscripts/z_helpFX.R")

neardist_sim <- function(pus, fsle_sf, fdata, cutoff, output) {
  
  library(sf)
  library(terra)
  library(stringr)
  library(dplyr)
  library(data.table)
  library(future.apply)
  library(parallel)
  library(doParallel)
  library(foreach)
  
  dist_fx <- function(PUs, fauna, ofdates, cutoff) {
    
    UseCores <- 5
    cl <- parallel::makeCluster(UseCores)
    doParallel::registerDoParallel(cl)
    lsout <- vector("list", length = nrow(fauna))
    ls_out <- foreach(x = 1:nrow(fauna),
                      .packages = c("terra",
                                    "dplyr", 
                                    "sf", 
                                    "stringr")) %dopar% {
                                      dist02 <- st_distance(fauna[x, ], ofdates, by_element = FALSE) %>%
                                        t() %>%
                                        as_tibble()
                                      colnames(dist02) <- as.character(fauna$geartype)  
                                      # Get the upper front quantile of front
                                      qfront <- ofdates %>%
                                        as_tibble() %>% 
                                        dplyr::select(2) %>% 
                                        quantile(probs = cutoff, na.rm = TRUE) %>% 
                                        as.vector()
                                      
                                      final <- cbind(PUs[,1], ofdates[,2], dist02) %>% 
                                        as_tibble() %>% 
                                        dplyr::select(-geometry, -geometry.1) %>% 
                                        dplyr::arrange(.[[3]]) %>% 
                                        dplyr::filter(.[[2]] > qfront) %>% 
                                        dplyr::slice(1)
                                      
                                      lsout[[x]] <- final %>% 
                                        dplyr::rename_with(.cols = 1, ~"pxID") %>% 
                                        dplyr::rename_with(.cols = 2, ~"FrontMag") %>% 
                                        dplyr::rename_with(.cols = 3, ~"DistHFront") %>% 
                                        dplyr::mutate(group = unique(fauna$geartype), 
                                                      dates = unique(fauna$date))
                                      
                                      
                                      
                                    }
    stopCluster(cl)
    FFF <- do.call(rbind, ls_out)
    return(FFF)
  }
  
  # Reading inputs
    PUs <- st_read(pus)
    sf1 <- readRDS(fsle_sf)
    nms <- names(sf1) %>% 
      stringr::str_extract(pattern = ".*(?=\\.)")
    colnames(sf1) <- nms
  # 
    vecFls <- list.files(path = fdata, pattern = ".rds", all.files = TRUE, full.names = TRUE, recursive = FALSE)
    OFdates <- stringr::str_remove(unlist(stringr::str_split(basename(fsle_sf), pattern = "_"))[3], pattern = ".rds")
    vecFls <- vecFls[stringr::str_detect(string = vecFls, pattern = OFdates) == TRUE]
    
    FFdf <-  future.apply::future_lapply(vecFls, future.scheduling = 5, FUN = function(x) {
      # 
        LatLon <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" # nolint
        robin <- "+proj=robin +lon_0=0 +datum=WGS84 +units=m +no_defs" # nolint
      # 
        fdata01 <- readRDS(x)
        if(is.data.frame(fdata01)) {
          fdata01
          } else {
            fdata01 <- fdata01[[1]]
          }
      # ocean fronts per day of each biodiversity data
        df01 <- sf1 %>% 
          dplyr::select(as.character(unique(fdata01$date)))
      # we need to create a vector with the unique date information
        Fdates <- unique(fdata01$date)
        FF <- vector("list", length = length(Fdates))
        for(j in seq_along(Fdates)) {
          # Filter the megafauna data for each date
            mmF <- fdata01 %>% 
              sf::st_as_sf(coords = c("x", "y"), crs = LatLon) %>% # from dataframe to sf object
              dplyr::filter(date == Fdates[j]) %>% # add the j here
              sf::st_transform(crs = robin)
          # Filter Front data for each date of the megafauna data
            OFCdates <- df01 %>% 
              dplyr::select(as.character(Fdates[j]))
            OFCdates <- cbind(PUs, OFCdates) %>% 
              st_transform(crs = robin)
          # 
            FF[[j]] <- dist_fx(PUs = PUs, fauna = mmF, ofdates = OFCdates, cutoff = cutoff)
        }
      # Tidy up the final list
        FFdf <- do.call(rbind, FF)
        })

  # File name for the output
    lapply(FFdf, function(x){
      sgl <- x
      ngrd <- unlist(stringr::str_split(basename(output), "_"))[1]
      ndate <- paste0(unlist(stringr::str_split(unlist(unique(sgl$dates)[1]), "-"))[1:2], collapse = "-")
      ffname <- paste(ngrd, ndate, unique(sgl$group), sep = "_")
      saveRDS(sgl, paste0(output, ffname, paste("_cutoff", cutoff, sep = "-"), ".rds"))
    })
    
    return(FFdf)
}

# GFW
  # FSLE
    system.time(tt <- neardist_sim(pus = "InputLayers/boundaries/PUs_MZ_50km2.shp",
                                   fsle_sf = "data_rout/fsle_pus_50km2/FSLE_SWIO_2019-12.rds",
                                   fdata = "data_rout/gfw_test",
                                   cutoff = 0.75,
                                   output = "data_rout/gfw_neardist_fsle/"))
  # BOA
    system.time(tt <- neardist_sim(pus = "InputLayers/boundaries/PUs_MZ_50km2.shp",
                                   fsle_sf = "data_rout/boar_pus_50km2/BOAonMUR_SWIO_2019-01.rds",
                                   fdata = "data_rout/gfw_test",
                                   cutoff = 0.75,
                                   output = "data_rout/gfw_neardist_boa/"))