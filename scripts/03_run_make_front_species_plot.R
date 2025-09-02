# Activate renv 
if (requireNamespace("renv", quietly = TRUE)) try(renv::activate(), silent = TRUE)
source("R/load_packages.R")
source("R/utils_helpers.R")
source("R/read_tracks.R")
source("R/grid_aggregate_gfw.R")
source("R/plot_species_in_fronts_effort.R")



test01 <- make_front_species_plot(
  tracks_dir = "outputs/tracks",
  product    = "boa",
  grid_file  = "data-raw/agg_cell_gear_mzc_rob.rds",
  # gears      = "drifting_longlines",
  grid_size  = 0.10,
  xlim       = c(30, 65),
  ylim       = c(-35, 0),
  output_dir = "outputs",
  width      = 10, height = 10, dpi = 300
)

test02 <- make_front_species_plot(
  tracks_dir = "outputs/tracks",
  product    = "fsle",
  grid_file  = "data-raw/agg_cell_gear_mzc_rob.rds",
  # gears      = "drifting_longlines",
  grid_size  = 0.10,
  xlim       = c(30, 65),
  ylim       = c(-35, 0),
  output_dir = "outputs",
  width      = 10, height = 10, dpi = 300,
)

patch_test <- patchwork::wrap_plots(test02, test01, ncol = 2, byrow = TRUE) +
  plot_annotation(title = "") + 
  plot_layout(guides = "collect") +
  plot_annotation(tag_prefix = "(",
                  tag_levels = "a", 
                  tag_suffix = ")")
ggsave("outputs/drifting_longlines_figv01.png", plot = patch_test, width = 20, height = 10, dpi = 400)

test03 <- make_front_species_plot(
  tracks_dir = "outputs/tracks",
  # product    = "boa",
  grid_file  = "data-raw/agg_cell_gear_mzc_rob.rds",
  # gears      = "drifting_longlines",
  grid_size  = 0.10,
  xlim       = c(30, 65),
  ylim       = c(-35, -5),
  # output_dir = "outputs",
  width      = 10, height = 5, dpi = 300, 
  by_species = FALSE
)
ggsave("outputs/figures/final/all-gears_all-fronts_figv04.png", plot = test03, width = 10, height = 10, dpi = 400)





# Define all gear types
gear_list <- c(
  "drifting_longlines","tuna_purse_seines","fishing","trawlers","dredge_fishing",
  "set_longlines","pole_and_line","squid_jigger","fixed_gear","set_gillnets",
  "other_purse_seines","purse_seines"
)

# Preallocate a named list
test_plot <- setNames(vector("list", length(gear_list)), gear_list)
for (i in seq_along(gear_list)) {
  g <- gear_list[i]
  message("Processing gear: ", g)
  
  test_plot[[g]] <- make_front_species_plot(
    tracks_dir = NULL,
    grid_file  = "data-raw/agg_cell_gear_mzc_rob.rds",
    gears      = g,
    grid_size  = 0.10,
    xlim       = c(30, 65),
    ylim       = c(-35, -5),
    # output_dir = "outputs/gear_plots",
    width      = 10, height = 5, dpi = 300,
    by_species = FALSE
  )
}
# 1) One shared scale with fixed limits + identical name
effort_scale <- scale_fill_distiller(
  palette = "GnBu",
  direction = 1,
  trans = "log10",
  breaks = c(1, 10, 100, 1e3, 1e4, 1e5),
  limits = c(1, 1e5),  # <-- FORCE identical range across all children
  labels = scales::label_number(accuracy = 1, big.mark = ","),
  na.value = "white",
  name = "Fishing Effort<br/>by 10 km<sup>2</sup> (Log<sub>10</sub> Scale)",  # same name in all
  guide = guide_colorbar(title.position = "top")
)

# 2) Apply the SAME scale to each plot (and don't hide the fill guide anywhere)
test_plot_fixed <- lapply(test_plot, function(p) {
  p + effort_scale + guides(fill = guide_colorbar()) +
    theme(legend.position = "right")  # temp position; will be collected later
})

# 3) Wrap and collect
patch_all_gt <- wrap_plots(plotlist = test_plot_fixed, ncol = 4, byrow = TRUE) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_prefix = "(",
                  tag_levels = "a", 
                  tag_suffix = ")") &
  theme(legend.position = "bottom", legend.box = "vertical")
ggsave("outputs/asdf_v03.png", plot = patch_all_gt, width = 40, height = 40, dpi = 300)
