# 0. Load libraries and declare functions ----
# ════════════════════════════════════════════

# * 0.1. Load libraries ----
# ──────────────────────────
library(tidyverse)
library(osrm)
library(parallel)
library(sf)
library(leaflet)

# * 0.2. Set OSRM options ----
# ────────────────────────────
options(osrm.server = 'http://router.project-osrm.org/') 
options(osrm.profile = 'car') 

# * 0.3. Define functions ----
# ────────────────────────────
# Create the route function using osrm
fnCreateRoute <- function(x, overview){
  osrmRoute(src = unname(as.numeric(x[c('SRC_LNG', 'SRC_LAT')])),
            dst = unname(as.numeric(x[c('DST_LNG', 'DST_LAT')])),
            overview = overview)
}

# 1. Load data ----
# ═════════════════

# * 1.1. Load East Kent Hospitals catchment area ----
# ───────────────────────────────────────────────────

# NB: Replace this with the location of the rds file on local machine
df_ekhuft_catchment <- readRDS('./data/EK_Filter.rds')

# * 1.2. Load LSOA 2011 Popn Weight Centroids ----
# ────────────────────────────────────────────────

# NB: The LSOA 2011 PWC file can be found here on Open Geography Portal
# https://geoportal.statistics.gov.uk/datasets/54a76a36e3b4420a9ea83fcf7994525d_0/explore
df_lsoa11_pwc <- read.csv('C:/Data/OpenGeography/Lookups/LSOA11_PWC/LSOA_Dec_2011_PWC_in_England_and_Wales_2022_1923591000694358693.csv') %>%
  mutate(EAST = x, NRTH = y) %>%
  st_as_sf(coords = c('x','y'), dim = 'XY', crs = 27700) %>%
  st_transform(crs = 4326) %>%
  mutate(LNG = st_coordinates(.)[,1],
         LAT = st_coordinates(.)[,2]) %>%
  st_drop_geometry() %>%
  select(LSOA11CD, EAST, NRTH, LNG, LAT)

# 2. Process data ----
# ════════════════════

# * 2.1. Geocode LSOA 2011 ----
# ─────────────────────────────
df_ekhuft_catchment <- df_ekhuft_catchment %>% left_join(df_lsoa11_pwc, by = 'LSOA11CD')

# * 2.2. Create Journey Grid ----
# ───────────────────────────────

df_journeys <- expand_grid(
  df_ekhuft_catchment %>% mutate(SRC = LSOA11CD, SRC_LNG = LNG, SRC_LAT = LAT, .keep = 'none'), 
  df_ekhuft_catchment %>% mutate(DST = LSOA11CD, DST_LNG = LNG, DST_LAT = LAT, .keep = 'none'),
  .name_repair = 'universal')

# * 2.3. Calculate Routes ----
# ────────────────────────────

# Create the clusters for parallelisation
n_cores <- detectCores()
# Leave one cluster free
clust <- makeCluster(n_cores - 1)
# Export the route creation function
clusterExport(clust, c('fnCreateRoute','osrmRoute'))
# Get the routes - distance and duration only using overview = FALSE
res <- parApply(clust, X = df_journeys, MARGIN = 1, FUN = fnCreateRoute, overview = FALSE)
# Stop the clusters
stopCluster(clust)

# Add the duration and distance to the journeys data frame
df_journeys <- df_journeys %>%
  mutate(duration_mins = res[1,],
         distance_km = res[2,])

# 3. Output data ----
# ═══════════════════
write.csv(df_journeys, 'travel_matrix_ekhuft.csv')

# 4. Bonus code ----
# ══════════════════
sf_lsoa11 <- st_read(dsn = 'C:\\Data\\OpenGeography\\Shapefiles\\LSOA11',
                     layer = 'lsoa11') %>%
  st_transform(crs = 4326) %>%
  semi_join(df_ekhuft_catchment, by = 'LSOA11CD') %>%
  summarise()

leaflet() %>%
  addTiles() %>%
  addPolygons(data = sf_lsoa11)
  