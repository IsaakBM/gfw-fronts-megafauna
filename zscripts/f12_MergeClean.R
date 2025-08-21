# This code was written by Isaac Brito-Morales (ibrito@conservation.org)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

# geom_density() computes a smooth kernel density estimate (KDE), which is a smoothed version of the histogram. 
# It gives you a sense of the distribution shape, but it can sometimes obscure or smooth out smaller peaks or variations 
# if they are not prominent enough.

library(dplyr)
library(ggplot2)
library(patchwork)
library(units)
library(ggtext)
library(gdata)

########################
####### Generalities
########################

theme_op01 <- theme(plot.title = element_text(face = "plain", size = 20, hjust = 0.5),
                    plot.tag = element_text(colour = "black", face = "bold", size = 23),
                    axis.title.x = element_text(size = rel(1.5), angle = 0),
                    axis.text.x = element_text(size = rel(2), angle = 0),
                    axis.title.y = element_text(size = rel(1.5), angle = 90),
                    axis.text.y = element_text(size = rel(2), angle = 0),
                    legend.title = element_text(colour = "black", face = "bold", size = 15),
                    legend.text = element_text(colour = "black", face = "bold", size = 13),
                    legend.key.height = unit(1.5, "cm"),
                    legend.key.width = unit(1.5, "cm"))

########################
####### function to get
####### a list element 
########################

modelvsrealH <- function(indirData, indirModel) {
  # 
    library(dplyr)
    library(ggplot2)
    library(units)
    library(ggtext)
    library(gdata)
  
  # 
    lsO <- list.files(path = indirData, 
                      pattern = ".rds", 
                      all.files = TRUE, 
                      full.names = TRUE, 
                      recursive = FALSE)
    lsS <- list.files(path = indirModel, 
                      pattern = ".rds", 
                      all.files = TRUE, 
                      full.names = TRUE, 
                      recursive = FALSE)
    # data manipulation of original data
      dfOr <- lapply(lsO, function(x) {
        sgl <- readRDS(x)
        sgl$DistHFront <- units::set_units(sgl$DistHFront, "km")
        sgl})
      dfOr <- do.call(rbind, dfOr) %>% 
        as_tibble() %>% 
        dplyr::mutate(data = "original")
    # data manipulation of simulation data
      dfS <- lapply(lsS, function(x) {
        sgl <- readRDS(x)
        sgl$DistHFront <- units::set_units(sgl$DistHFront, "km")
        sgl})
      dfS <- do.call(rbind, dfS) %>% 
        as_tibble() %>% 
        dplyr::mutate(data = "simulation")
      # merge both datasets
        DFF <- rbind(dfOr, dfS) %>% 
          as_tibble()
      
    # 
      FO <- dfOr %>% 
        dplyr::rename(DistHFrontO = DistHFront) %>% 
        dplyr::select(DistHFrontO)
      # median 2.347741 km
      FS <- dfS %>% 
        dplyr::mutate(DistHFrontS = DistHFront) %>% 
        dplyr::select(DistHFrontS)
      # median: 26.1951 [km]
    # 
      if(nrow(FO) != nrow(FS)) {
        FF <- gdata::cbindX(FO, FS) %>% 
          as_tibble() %>% 
          dplyr::mutate(DistHFrontO = as.numeric(DistHFrontO), 
                        DistHFrontS = as.numeric(DistHFrontS))
        dfF.ls <- list(DFF, FF)
        } else {
          FF <- cbind(FO, FS) %>% 
            as_tibble() %>% 
            dplyr::mutate(DistHFrontO = as.numeric(DistHFrontO), 
                          DistHFrontS = as.numeric(DistHFrontS))
          dfF.ls <- list(DFF, FF)
        }
      return(dfF.ls)
}


########################
####### plot ENTROPIES
####### and MMF 
########################

entropie_mmf <- function(input, xlabs, ylabs) {
  
  df01 <- input[[1]]
  df02 <- input[[2]]
  
  ggp01 <- ggplot(df02, aes(x = x)) +
    # Top
      # geom_histogram(aes(x = DistHFrontO, y = after_stat(density)),
      #                bins = 30,
      #                colour = "#404040",# Soft Black for border
      #                fill = "#1f77b4",
      #                binwidth = 2) +
      geom_density(aes(x = DistHFrontO, y = after_stat(density)), 
                   lwd = 1, 
                   colour = "#1f77b4",
                   fill = "#1f77b4",
                   alpha = 0.50, 
                   adjust = 0.1) +
    # Bottom
      # geom_histogram(aes(x = DistHFrontS, y = after_stat(-density)),
      #                bins = 30,
      #                colour = "#404040",  # Soft Black for border
      #                fill = "#ff7f0e",    # Orange for fill
      #                binwidth = 2) +
      geom_density(aes(x = DistHFrontS, y = after_stat(-density)), 
                   lwd = 1, 
                   colour = "#ff7f0e",  # Orange #ff7f0e
                   fill = "#ff7f0e", # Orange #ff7f0e
                   alpha = 0.50, 
                   adjust = 0.9) +
      geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(breaks = seq(-0.4, 0.4, 0.1), 
                         limits = c(-0.32, 0.32), 
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
                                  label = paste("n =", length(unique(df01$group)), "<br>data = original")),
                    aes(x = x, y = y, label = label), 
                    size = 3.5,
                    fill = "white", 
                    colour = "#1f77b4",
                    label.color = "black",
                    hjust = 0) +
      geom_richtext(inherit.aes = FALSE, 
                    data = tibble(x = 5, y = -0.2, 
                                  label = paste("n =", length(unique(df01$group)), "<br>data = random walk model")),
                    aes(x = x, y = y, label = label), 
                    size = 3.5,
                    fill = "white", 
                    colour = "#ff7f0e",# Orange #ff7f0e
                    label.color = "black",
                    hjust = 0)
  return(ggp01)
}


########################
####### plot REMMOA
####### SB, MM, OM
########################

remmoa <- function(input, xlabs, ylabs) {
  
  df01 <- input[[1]]
  df02 <- input[[2]]
  
  ggp01 <- ggplot(df02, aes(x = x)) +
    # Top
    # geom_histogram(aes(x = DistHFrontO, y = after_stat(density)),
    #                bins = 30,
    #                colour = "#404040",# Soft Black for border
    #                fill = "#1f77b4",
    #                binwidth = 2) +
    geom_density(aes(x = DistHFrontO, y = after_stat(density)), 
                 lwd = 1, 
                 colour = "#1f77b4",
                 fill = "#1f77b4",
                 alpha = 0.50, 
                 adjust = 0.5) + #0.03
    # Bottom
    # geom_histogram(aes(x = DistHFrontS, y = after_stat(-density)),
    #                bins = 30,
    #                colour = "#404040",  # Soft Black for border
    #                fill = "#ff7f0e",    # Orange for fill
    #                binwidth = 2) +
    geom_density(aes(x = DistHFrontS, y = after_stat(-density)), 
                 lwd = 1, 
                 colour = "#ff7f0e",  # Orange #ff7f0e
                 fill = "#ff7f0e", # Orange #ff7f0e
                 alpha = 0.50, 
                 adjust = 0.5) + # 0.1
    geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(breaks = seq(-0.4, 0.4, 0.1), 
                       limits = c(-0.32, 0.32), 
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
                                label = paste("n =", length(unique(df01$group)), "<br>data = original")),
                  aes(x = x, y = y, label = label), 
                  size = 3.5,
                  fill = "white", 
                  colour = "#1f77b4",
                  label.color = "black",
                  hjust = 0) +
    geom_richtext(inherit.aes = FALSE,
                  data = tibble(x = 5, y = -0.2,
                                label = paste("n =", length(unique(df01$group)), "<br>data = random point model")),
                  aes(x = x, y = y, label = label),
                  size = 3.5,
                  fill = "white",
                  colour = "#ff7f0e",
                  label.color = "black",
                  hjust = 0)
  return(ggp01)
}



# # Chart
# p0 <- ggplot(df5.3[[2]], aes(x=x) ) +
#   # Top
#   geom_density( aes(x = DistHFrontO, y = ..density..), fill="#69b3a2",  adjust = 0.35) +
#   # geom_histogram( aes(x = DistHFrontO, y = ..density..), fill="#69b3a2", bins = 30, binwidth = 2)
#   # Bottom
#   geom_density( aes(x = DistHFrontS, y = -..density..), fill= "#404080", adjust = 0.35) +
#   scale_y_continuous(breaks = seq(-0.5, 0.5, 0.1), 
#                      limits = c(-0.30, 0.30), 
#                      expand = c(0, 0)) +
#   coord_cartesian(xlim = c(0, 50)) +
#   labs(x = "Distance to high thermal front (km)",
#        y = "Density") +
#   theme_bw() +
#   geom_richtext(inherit.aes = FALSE, 
#                 data = tibble(x = 27, y = 0.15, label = paste("n", "=", "nrow(s1)", sep = " ")), 
#                 aes(x = x, y = y, label = label), 
#                 size = 6,
#                 fill = NA)
# 
# p1 <- ggplot(df5.3[[2]], aes(x = x) ) +
#   geom_histogram(aes(x = DistHFrontO, y = (after_stat(count))/sum(after_stat(count))), fill="#69b3a2", binwidth = 2, bins = 30, position = "stack") +
#   geom_histogram(aes(x = DistHFrontS, y = -(after_stat(count))/sum(after_stat(count))), fill= "#404080", binwidth = 2, bins = 30, position = "stack") +
#   scale_y_continuous(breaks = seq(-1, 1, 0.2), 
#                      limits = c(-0.5, 0.5), 
#                      expand = c(0, 0)) +
#   coord_cartesian(xlim = c(0, 50)) +
#   labs(x = "Distance to high thermal front (km)",
#        y = "Density") +
#   theme_bw()
# 
# 
# 
# # REMMOA
# ggplot(df5.3[[2]], aes(x = x)) +
#   # Top
#   geom_histogram(aes(x = DistHFrontO, y = ..density..), 
#                  bins = 30, 
#                  colour = "1", 
#                  binwidth = 2) +
#   geom_density(aes(x = DistHFrontO, y = ..density..), 
#                lwd = 1, 
#                colour = "#2b8cbe", 
#                fill = "#2b8cbe", 
#                alpha = 0.50, 
#                adjust = 0.03) +
#   # Bottom
#   geom_histogram(aes(x = DistHFrontS, y = -..density..), 
#                  bins = 30, 
#                  colour = "1", 
#                  binwidth = 2) +
#   geom_density(aes(x = DistHFrontS, y = -..density..), 
#                lwd = 1, 
#                colour = "#2b8cbe", 
#                fill = "#2b8cbe", 
#                alpha = 0.50, 
#                adjust = 0.1) +
#   scale_y_continuous(breaks = seq(-0.5, 0.5, 0.1), 
#                      limits = c(-0.30, 0.30), 
#                      expand = c(0, 0), 
#                      labels = function(x) abs(x)) +
#   coord_cartesian(xlim = c(0, 30))

