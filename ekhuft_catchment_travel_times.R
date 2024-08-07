# 0. Load libraries and define functions ----
# ═══════════════════════════════════════════
library(tidyverse)
library(readxl)
library(sf)

# 1. Load data ----
# ═════════════════

# * 1.1. Load trust catchment data ----
# ─────────────────────────────────────

df_ohid_catchment <- read_excel(path = 'D:/Data/OHID/Trust_Catchment_Areas/2022 Trust Catchment Populations_Supplementary MSOA Analysis.xlsx', 
                                sheet = 'All Admissions') %>% 
  filter(TrustCode == 'RVV' & CatchmentYear == '2020' & FPTP=='TRUE')

# * 1.2. Load trust site data ----
# ────────────────────────────────
df_trust_sites <- data.frame(CODE = c('RVVKC', 'RVV01', 'RVV09'),
                             NAME = c('Kent and Canterbury Hospital', 'William Harvey Hospital', 'Queen Elizabeth The Queen Mother Hospital'),
                             PCODE = c('CT1 3NG', 'TN24 0LZ', 'CT9 4AN'),
                             EASTING = c(615456, 604090, 635967),
                             NORTHING = c(156464, 142068, 169786),
                             LONGITUDE = c(1.0870832, 0.91620878, 1.3893845),
                             LATITUDE = c(51.266589, 51.141489, 51.378053))

# * 1.3. Load OA|LSOA|MSOA lookup data ----
# ─────────────────────────────────────────

df_oa_lsoa_msoa_lu <- read.csv('D:/Data/OpenGeography/Lookups/OA11_LSOA11_MSOA11_LAD11/Output_Area_to_Lower_Layer_Super_Output_Area_to_Middle_Layer_Super_Output_Area_to_Local_Authority_District_(December_2011)_Lookup_in_England_and_Wales.csv') %>%
  select(OA11CD, LSOA11CD, MSOA11CD)

# * * 1.3.1. Load OA PWC data ----
# ────────────────────────────────

df_oa_pwc <- read.csv('D:/Data/OpenGeography/Shapefiles/OA11_PWC/Output_Areas__December_2011__Population_Weighted_Centroids.csv') %>% 
  mutate(OA11CD, EASTING = x, NORTHING = y, X = x, Y = y) %>%
  st_as_sf(coords = c('X', 'Y'), dim = 'XY', crs = 27700) %>%
  st_transform(crs = 4326) %>%
  mutate(LONGITUDE = st_coordinates(.)[,1],
         LATITUDE = st_coordinates(.)[,2]) %>%
  st_drop_geometry() %>%
  select(-c(OBJECTID, GlobalID, x, y))

# * * 1.3.2. Load LSOA PWC data ----
# ──────────────────────────────────

df_lsoa_pwc <- read.csv('D:/Data/OpenGeography/Shapefiles/LSOA11_PWC/LSOA_Dec_2011_PWC_in_England_and_Wales_2022_1923591000694358693.csv') %>% 
  mutate(LSOA11CD, EASTING = x, NORTHING = y, X = x, Y = y) %>%
  st_as_sf(coords = c('X', 'Y'), dim = 'XY', crs = 27700) %>%
  st_transform(crs = 4326) %>%
  mutate(LONGITUDE = st_coordinates(.)[,1],
         LATITUDE = st_coordinates(.)[,2]) %>%
  st_drop_geometry() %>%
  select(-c(OBJECTID, LSOA11NM, GlobalID, x, y))

# 2. Process data ----
# ════════════════════


# 3. Display data ----
# ════════════════════