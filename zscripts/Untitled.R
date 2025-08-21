pus = "InputLayers/boundaries/PUs_MZ_50km2.shp"
fsle_sf = "data_rout/fsle_pus_50km2/FSLE_SWIO_2019-01.rds"
fdata = "data_rout/gfw_test"
cutoff = 0.75
output = "data_rout/gfw_neardist_fsle/"

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

unique(FFdf[[1]]$dates)

hist(FFdf[[1]]$DistHFront) 

lapply(FFdf, function(x){
  sgl <- FFdf[[1]]
  ngrd <- unlist(stringr::str_split(basename(output), "_"))[1]
  ndate <- paste0(unlist(stringr::str_split(unlist(unique(sgl$dates)[1]), "-"))[1:2], collapse = "-")
  ffname <- paste(ngrd, ndate, unique(sgl$group), sep = "_")
  saveRDS(sgl, paste0(output, ffname, paste("_cutoff", cutoff, sep = "-"), ".rds"))
})

readRDS("data_rout/gfw_neardist_fsle/gfw_2019-01_drifting_longlines_cutoff-0.75.rds")


asdf <- modelvsrealH(indirData = "data_rout/gfw_neardist_fsle", 
                       indirModel = "data_rout/gfw_neardist_fsle")

asdf_plot01 <-  entropie_mmf(input = asdf,
                             xlabs = "Distance to high FSLE (km)", 
                             ylabs = "Density")



asdf <- modelvsrealH(indirData = "data_rout/gfw_neardist_fsle", 
                     indirModel = "data_rout/gfw_neardist_fsle")
input = asdf
xlabs = "Distance to high FSLE (km)"
ylabs = "Density"

entropie_mmf <- function(input, xlabs, ylabs) {
  
  df01 <- input[[1]] %>% 
    dplyr::filter(data == "original")
  gt <- unique(df01$group)
  
  gg_list <- vector("list", length = length(gt))
  for(i in seq_along(gt)) {
    
    sgl <- df01 %>% 
      dplyr::filter(group == gt[i])
    sgl$DistHFront <- as.double(sgl$DistHFront)
    
    gg_list[[i]] <- ggplot(sgl, aes(x = x)) +
      geom_density(aes(x = DistHFront, y = after_stat(density)), 
                   lwd = 1, 
                   colour = "#1f77b4",
                   fill = "#1f77b4",
                   alpha = 0.50, 
                   adjust = 0.1) +
      geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(breaks = seq(-0.4, 0.4, 0.1), 
                         limits = c(-0.05, 0.32), 
                         expand = c(0, 0), 
                         labels = function(x) abs(x)) +
      coord_cartesian(xlim = c(0, 30)) +
      # Major labels
      labs(x = xlabs,
           y = ylabs) +
      theme_op01 +
      theme_bw() +
      geom_richtext(inherit.aes = FALSE, 
                    data = tibble(x = 5, y = 0.2, 
                                  label = paste("n =", length(sgl$group), 
                                                "<br>", paste("gear type = ", unique(sgl$group)))),
                    aes(x = x, y = y, label = label), 
                    size = 3.5,
                    fill = "white", 
                    colour = "#1f77b4",
                    label.color = "black",
                    hjust = 0)
  }
  }
  
  test04 <- patchwork::wrap_plots(gg_list, ncol = 3, byrow = TRUE) +
    plot_annotation(title = "") + 
    plot_layout(guides = "collect") +
    plot_annotation(tag_prefix = "(",
                    tag_levels = "a", 
                    tag_suffix = ")")
  ggsave("Figures/gfw_2019_FSLE.png", plot = test04, width = 15, height = 7, dpi = 400)

  
  
  
  
  
  

  df01 <- testing03
  gt <- unique(df01$geartype)
  gg_list <- vector("list", length = length(gt))
  for(i in seq_along(gt)) {
    
    sgl <- df01 %>% 
      dplyr::filter(geartype == gt[i])
    
    gg_list[[i]] <- ggplot(sgl, aes(x = x)) +
      geom_density(aes(x = DistHFront_km, y = after_stat(density)), 
                   lwd = 1, 
                   colour = "#1f77b4",
                   fill = "#1f77b4",
                   alpha = 0.50, 
                   adjust = 1) +
      geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(breaks = seq(-0.4, 0.4, 0.1), 
                         limits = c(-0.05, 0.32), 
                         expand = c(0, 0), 
                         labels = function(x) abs(x)) +
      coord_cartesian(xlim = c(0, 30)) +
      # Major labels
      labs(x = xlabs,
           y = ylabs) +
      theme_op01 +
      theme_bw() +
      geom_richtext(inherit.aes = FALSE, 
                    data = tibble(x = 5, y = 0.2, 
                                  label = paste("n =", length(sgl$geartype), 
                                                "<br>", paste("gear type = ", unique(sgl$geartype)))),
                    aes(x = x, y = y, label = label), 
                    size = 3.5,
                    fill = "white", 
                    colour = "#1f77b4",
                    label.color = "black",
                    hjust = 0)
  }
  
  test05 <- patchwork::wrap_plots(gg_list, ncol = 3, byrow = TRUE) +
    plot_annotation(title = "") + 
    plot_layout(guides = "collect") +
    plot_annotation(tag_prefix = "(",
                    tag_levels = "a", 
                    tag_suffix = ")")
  ggsave("Figures/gfw_2019_FSLE_v02.png", plot = test05, width = 15, height = 7, dpi = 400)
