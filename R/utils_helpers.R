# ============================
# Utility Helpers
# Author: Isaac Brito-Morales
# Email: ibrito@conservation.org
# ============================

# --- CRS Definitions ---
LatLon <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
moll   <- "+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs"
robin  <- "+proj=robin +lon_0=0 +datum=WGS84 +units=m +no_defs"

# --- Quick Functions ---
get_world_robin <- function() {
  ne_countries(scale = "medium", returnclass = "sf") |>
    st_transform(crs = robin) |>
    st_make_valid()
}

# --- Color palettes ---
front_palette <- RColorBrewer::brewer.pal(9, "YlGnBu")

# --- Units helpers ---
km_to_m <- function(km) units::set_units(km, "m")
m_to_km <- function(m) units::set_units(m, "km")