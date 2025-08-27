# Activate renv -----------------------------------------------------------
if (requireNamespace("renv", quietly = TRUE)) try(renv::activate(), silent = TRUE)
source("R/load_packages.R")
source("R/utils_helpers.R")


mzc_sf_Rob <- get_world_robin() %>% 
  st_crop(xmin = 2671900, ymin = -3743317, xmax = 5654583, ymax = -748663.4)

test01 <- readRDS("outputs/tracks/WHSH/WHSH_fsle_2016-10_cutoff-0.75.rds")
test02 <- readRDS("outputs/tracks/RTTB/RTTB_fsle_2018-05_cutoff-0.75.rds")
test03 <- readRDS("outputs/tracks/WTSH/WTSH_fsle_2018-12_cutoff-0.75.rds")
# test04 <- readRDS("outputs/tracks/LGHT/LGHT_fsle_2021-10_cutoff-0.75.rds")

test <- rbind(test01, test02, test03)
df01 <- test %>% 
  dplyr::filter(InStrongFront == TRUE)

pts_sf <- st_as_sf(df01, coords = c("lon", "lat"), crs = 4326)
pts_robin <- st_transform(pts_sf, crs = robin)

unique(pts_robin$species)
unique(pts_robin$InStrongFront)

ggplot() +
  geom_sf(data = mzc_sf_Rob, linewidth = 0.2, fill = "grey20", color = "grey30") +
  geom_sf(
    data = pts_robin,           # your sf with geometry in Robinson
    aes(fill = species),        # color by species
    shape = 21,                 # filled circle so 'fill' works
    size = 1,
    alpha = 0.8,
    color = "black",
    stroke = 0.2
  ) +
  guides(fill = guide_legend(title = "Species")) +
  coord_sf(crs = st_crs(pts_robin))
  

library(dplyr)
library(sf)
library(lwgeom)
library(ggplot2)

# inputs:
# - pts_robin: your sf POINT layer in Robinson meters (the 4,575-row object you showed)
# - mzc_sf_Rob: your basemap polygons in the same CRS

# 1) union points per species (keeps only species + geometry)
hulls <- pts_robin %>%
  st_make_valid() %>%
  group_by(species) %>%
  summarise(.groups = "drop")  # dissolves geometries per species

# 2) concave hulls (tune concavity: higher = looser, lower = tighter)
st_geometry(hulls) <- st_concave_hull(
  st_geometry(hulls),
  ratio = 0.,         # tighter (try 0.25–0.50)
  length_threshold = 0, # increase to simplify long skinny spikes (meters)
  allow_holes = FALSE
)

# 3) optional cleanup: drop tiny fragments + light smooth
hulls_clean <- hulls %>%
  st_cast("MULTIPOLYGON", warn = FALSE) %>%
  st_collection_extract("POLYGON") %>%
  mutate(area_km2 = as.numeric(st_area(geometry)) / 1e6) %>%
  group_by(species) %>%
  filter(area_km2 > 10) %>%     # drop pieces < 10 km² (adjust as needed)
  ungroup()

# simple smooth via buffer out/in (adjust meters)
hulls_smooth <- hulls_clean %>% st_buffer(2000) %>% st_buffer(-2000)

# 4) map
ggplot() +
  geom_sf(data = mzc_sf_Rob, fill = "grey20", color = "grey30", linewidth = 0.2) +
  geom_sf(data = hulls_smooth, aes(fill = species), alpha = 0.35, color = NA) +
  geom_sf(data = pts_robin, aes(color = species), size = 0.8, alpha = 0.6) +
  guides(fill = guide_legend(title = "Occupancy footprint"), color = "none") +
  coord_sf(crs = st_crs(pts_robin)) +
  theme_minimal()


