# This code was written by Isaac Brito-Morales (ibrito@conservation.org)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

# geom_density() computes a smooth kernel density estimate (KDE), which is a smoothed version of the histogram. 
# It gives you a sense of the distribution shape, but it can sometimes obscure or smooth out smaller peaks or variations 
# if they are not prominent enough.

source("zscripts/z_helpFX.R")
source("zscripts/z_inputFls_local.R")
source("zscripts/f12_MergeClean.R")

########################
####### ENTROPIE
########################
# Red-tailed tropic bird 
  # FSLE
    rttb01 <- modelvsrealH(indirData = "data_rout/NosyVe_neardist_fsle", 
                        indirModel = "data_rout/NosyVe_simulated_fsle")
    rttb_plot01 <-  entropie_mmf(input = rttb01,
                               xlabs = "Distance to high FSLE (km)", 
                               ylabs = "Density")
    rttb_plot01 <- rttb_plot01 + ggtitle("Red-tailed tropic bird") + labs(y = "")
  # BOA
    rttb02 <- modelvsrealH(indirData = "data_rout/NosyVe_neardist_boa", 
                           indirModel = "data_rout/NosyVe_simulated_boa")
    rttb_plot02 <-  entropie_mmf(input = rttb02,
                                 xlabs = "Distance to high thermal front (km)", 
                                 ylabs = "Density")
    rttb_plot02 <- rttb_plot02 + labs(y = "")
    
# Wedge-tailed shearwater
  # FSLE
    wtsw01 <- modelvsrealH(indirData = "data_rout/WTSH_neardist_fsle", 
                           indirModel = "data_rout/WTSH_simulated_fsle")
    wtsw_plot01 <-  entropie_mmf(input = wtsw01,
                                 xlabs = "Distance to high FSLE (km)", 
                                 ylabs = "Density")
    wtsw_plot01 <- wtsw_plot01 + ggtitle("Wedge-tailed shearwater") + labs(y = "")
  # BOA
    wtsw02 <- modelvsrealH(indirData = "data_rout/WTSH_neardist_boa", 
                           indirModel = "data_rout/WTSH_simulated_boa")
    wtsw_plot02 <-  entropie_mmf(input = wtsw02,
                                 xlabs = "Distance to high thermal front (km)", 
                                 ylabs = "Density")
    wtsw_plot02 <- wtsw_plot02 + labs(y = "")

########################
####### MMF
########################
# Whale Shark tracking data
  # FSLE
    ws01 <- modelvsrealH(indirData = "data_rout/mm_mmf_neardist_fsle",
                         indirModel = "data_rout/mm_mmf_simulated_fsle")
    ws_plot01 <-  entropie_mmf(input = ws01,
                                 xlabs = "Distance to high FSLE (km)", 
                                 ylabs = "Density")
    ws_plot01 <- ws_plot01 + ggtitle("Whale Shark tracking data")
  # BOA 
    ws02 <- modelvsrealH(indirData = "data_rout/mm_mmf_neardist_boa", 
                         indirModel = "data_rout/mm_mmf_simulated_boa")
    ws_plot02 <-  entropie_mmf(input = ws02,
                                 xlabs = "Distance to high thermal front (km)", 
                                 ylabs = "Density")

########################
####### REMMOA
########################
# FSLE
  # Seabirds
    remmoa_sb01 <- modelvsrealH(indirData = "data_rout_remmoa_testv02/mm_remmoa_neardist_sb_fsle", 
                                indirModel = "data_rout_remmoa_testv02/mm_remmoa_simulated_sb_fsle")
    remmoa_sbplot01 <- remmoa(input = remmoa_sb01,
                              xlabs = "Distance to high FSLE (km)", 
                              ylabs = "Density")
    remmoa_sbplot01 <- remmoa_sbplot01 + ggtitle("Seabirds") + labs(y = "")
    
  # Marine Mammals
    remmoa_mm01 <- modelvsrealH(indirData = "data_rout_remmoa_testv02/mm_remmoa_neardist_mm_fsle",
                                indirModel = "data_rout_remmoa_testv02/mm_remmoa_simulated_mm_fsle")
    remmoa_mmplot01 <- remmoa(input = remmoa_mm01,
                              xlabs = "Distance to high FSLE (km)", 
                              ylabs = "Density")
    remmoa_mmplot01 <- remmoa_mmplot01 + ggtitle("Marine mammals")
  # Other Megafauna (sharks, rays, turtles)
    remmoa_om01 <- modelvsrealH(indirData = "data_rout_remmoa_testv04/mm_remmoa_neardist_om_fsle", 
                                indirModel = "data_rout_remmoa_testv04/mm_remmoa_simulated_om_fsle")
    remmoa_omplot01 <- remmoa(input = remmoa_om01,
                              xlabs = "Distance to high FSLE (km)", 
                              ylabs = "Density")
    remmoa_omplot01 <- remmoa_omplot01 + ggtitle("Other Megafauna (sharks, rays, turtles)") + labs(y = "")

# BOA
  # Seabirds
    remmoa_sb02 <- modelvsrealH(indirData = "data_rout_remmoa_testv02/mm_remmoa_neardist_sb_boa", 
                                indirModel = "data_rout_remmoa_testv02/mm_remmoa_simulated_sb_boa")
    remmoa_sbplot02 <- remmoa(input = remmoa_sb02,
                              xlabs = "Distance to high thermal front (km)", 
                              ylabs = "Density")
    remmoa_sbplot02 <- remmoa_sbplot02 + labs(y = "")
    
  # Marine Mammals
    remmoa_mm02 <- modelvsrealH(indirData = "data_rout_remmoa_testv02/mm_remmoa_neardist_mm_boa",
                                indirModel = "data_rout_remmoa_testv02/mm_remmoa_simulated_mm_boa")
    remmoa_mmplot02 <- remmoa(input = remmoa_mm02,
                              xlabs = "Distance to high thermal front (km)", 
                              ylabs = "Density")
    remmoa_mmplot02 <- remmoa_mmplot02
  # Other Megafauna (sharks, rays, turtles)
    remmoa_om02 <- modelvsrealH(indirData = "data_rout_remmoa_testv03/mm_remmoa_neardist_om_boa", 
                                indirModel = "data_rout_remmoa_testv03/mm_remmoa_simulated_om_boa")
    remmoa_omplot02 <- remmoa(input = remmoa_om02,
                              xlabs = "Distance to high thermal front (km)", 
                              ylabs = "Density")
    remmoa_omplot02 <- remmoa_omplot02 + labs(y = "")
    
    
########################
####### STORM
########################
# Turtles
  # FSLE
    tmnt01 <- modelvsrealH(indirData = "data_rout/TMNT_neardist_fsle",
                           indirModel = "data_rout/TMNT_simulated_fsle")
    tmnt01_plot01 <-  entropie_mmf(input = tmnt01,
                                   xlabs = "Distance to high FSLE (km)", 
                                   ylabs = "Density")
    tmnt01_plot01 <- tmnt01_plot01 + ggtitle("Turtles") + labs(y = "")
  # BOA
    tmnt02 <- modelvsrealH(indirData = "data_rout/TMNT_neardist_boa",
                           indirModel = "data_rout/TMNT_simulated_boa")
    tmnt02_plot02 <-  entropie_mmf(input = tmnt02,
                                   xlabs = "Distance to high thermal front (km)", 
                                   ylabs = "Density") 
    tmnt02_plot02 <- tmnt02_plot02 + labs(y = "")
    
    
########################
####### Creating the patchwork
####### figure
########################
    
# TRACKING DATA (NO TURTLES)
  test <- list(ws_plot01, rttb_plot01, wtsw_plot01, 
               ws_plot02, rttb_plot02, wtsw_plot02)
  test02 <- patchwork::wrap_plots(test, ncol = 3, byrow = TRUE) +
    plot_annotation(title = "") + 
    plot_layout(guides = "collect") +
    plot_annotation(tag_prefix = "(",
                    tag_levels = "a", 
                    tag_suffix = ")")
  ggsave("test02.png", plot = test02, width = 15, height = 7, dpi = 400)
    
# REMMOA DATA (AERIAL VIZ)
  test2 <- list(remmoa_mmplot01, remmoa_sbplot01, remmoa_omplot01, 
                remmoa_mmplot02, remmoa_sbplot02, remmoa_omplot02)
  test03 <- patchwork::wrap_plots(test2, ncol = 3, byrow = TRUE) +
    plot_annotation(title = "") + 
    plot_layout(guides = "collect") +
    plot_annotation(tag_prefix = "(",
                    tag_levels = "a", 
                    tag_suffix = ")")
  ggsave("z_aerial_random-point_test04_delete.png", plot = test03, width = 15, height = 7, dpi = 400)
  
# TRACKING DATA (JUST TURTLES)
  test4 <- list(tmnt01_plot01, 
                tmnt02_plot02)
  test04 <- patchwork::wrap_plots(test4, ncol = 1, byrow = TRUE) +
    plot_annotation(title = "") + 
    plot_layout(guides = "collect") +
    plot_annotation(tag_prefix = "(",
                    tag_levels = "a", 
                    tag_suffix = ")")
  ggsave("test04.png", plot = test04, width = 5, height = 7, dpi = 400)

# TRACKING DATA (JUST TURTLES)
  test <- list(ws_plot01, rttb_plot01, wtsw_plot01, tmnt01_plot01, 
               ws_plot02, rttb_plot02, wtsw_plot02, tmnt02_plot02)
  test05 <- patchwork::wrap_plots(test, ncol = 4, byrow = TRUE) +
    plot_annotation(title = "") + 
    plot_layout(guides = "collect") +
    plot_annotation(tag_prefix = "(",
                    tag_levels = "a", 
                    tag_suffix = ")")
  ggsave("figures/tracking_wshark_sb_tmnt_v05.pdf", plot = test05, width = 18, height = 7, dpi = 400)
  

