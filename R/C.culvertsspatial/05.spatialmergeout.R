# TITLE: Spatial data merge
# AUTHOR: Braeden Van Deynze
# DATE: Aug., 2020
# INPUTS: "culverts_full_mapping.csv"" data for culvert work site data; additional outputs from spatial data module and Blake's ArcGIS pulls
# OUTPUTS: Rectangular culvert work site data with spatial variables attached
# STATUS: Draft

# User note: Designed so that you can easily comment out specific spatial script outputs for partial data pull

rm(list = ls())
# Load libraries
library(tidyverse)
library(here)
library(janitor)
library(readxl)

# Load base work site data
df_culv <- read_csv(here("output/culverts_full_mapping.csv")) %>% 
  st_as_sf(coords = c('longitude', 'latitude'), crs = 4326) %>%
  select(-county)

# Load other outputs

# 01.countycensustests.R - CBP jobs data
df_cbp <- read_csv(here("output/spatial/culverts_cbp.csv"))
# 02.culvertsdensitytets.R - Density measures for nearby culverts
df_density <- read_csv(here("output/spatial/culverts_density.csv"))
# 03.culvertsroadstets.R - OSM streets data
df_osmroads <- read_csv(here("output/spatial/culverts_osmroads.csv"))
# 04.culvertsstreamtest.R - NHD+ stream data
df_nhdstreams <- read_csv(here("output/spatial/culverts_nhdstreams.csv"))

# Blake's ArcGIS pulls
df_blake <- read_xlsx(here("data/Culverts spatial overlays v 06Aug2020.xlsx"), sheet = 1) %>% as_tibble() %>% clean_names() %>% mutate(pure_culv = as.logical(pure_culv))

# Add fips codes for culverts
# Load county data to ID missing counties
temp <- tempfile()
tempdir <- tempdir()
download.file("https://www2.census.gov/geo/tiger/TIGER2017/COUNTY/tl_2017_us_county.zip", temp)
unzip(temp, exdir = tempdir)
sf_counties <- read_sf(paste0(tempdir, "\\tl_2017_us_county.shp")) %>%
  st_transform(crs = 4326) %>%
  clean_names() %>%
  select(fips = geoid, county = name, state_fips = statefp)
unlink(temp)
rm(temp, tempdir)

# Spatial join
df_culv <- st_join(df_culv, sf_counties)


# Merge up
df_culv_out <-
  df_culv %>%
  mutate(fips = as.double(fips)) %>%
  # CBP jobs data
  left_join(df_cbp, by = c("project_year" = "year", "fips" = "fips")) %>%
  # Density measures for nearby culverts
  # left_join(df_density) %>%
  # OSM streets data
  # left_join(df_osmroads) %>%
  # NHD+ stream data
  # left_join(df_nhdstreams) %>%
  # Blake's ArcGIS data
  left_join(df_blake)

# Write out
df_culv_out %>%
  write_csv(here("output/culverts_full_spatial.csv"))
  