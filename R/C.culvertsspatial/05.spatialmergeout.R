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
# df_density <- read_csv(here("output/spatial/culverts_density.csv"))
# 03.culvertsroadstets.R - OSM streets data
# df_osmroads <- read_csv(here("output/spatial/culverts_osmroads.csv"))
# 04.culvertsstreamtest.R - NHD+ stream data
# df_nhdstreams <- read_csv(here("output/spatial/culverts_nhdstreams.csv"))

# Blake's ArcGIS pulls
df_blake <- read_xlsx(here("data/Culverts spatial overlays v 06Aug2020.xlsx"), sheet = 1) %>% as_tibble() %>% clean_names() %>% mutate(pure_culv = as.logical(pure_culv))

# Add fips codes for culverts
# Load county data to ID missing counties
temp <- tempfile()
tempdir <- tempdir()
download.file("https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_county_500k.zip", temp)
unzip(temp, exdir = tempdir)
sf_counties <- read_sf(paste0(tempdir, "\\cb_2018_us_county_500k.shp")) %>%
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
  left_join(df_blake %>% select(worksite_id, nlcd_2001:here_publi), by = "worksite_id")

# Write out
df_culv_out %>%
  mutate(
    latitude = st_coordinates(geometry)[,2],
    longitude = st_coordinates(geometry)[,1]
  ) %>%
  st_drop_geometry() %>%
  write_csv(here("output/culverts_full_spatial.csv"))

# Some summary stats
qplot(df_culv_out$nhd_dist_m) + ggtitle("Frequency of distance (m) to nearest NHD+ segment", "-999 appears to be NA")
qplot(df_culv_out$snet_distm) + ggtitle("Frequency of distance (m) to nearest StreamNET segment", "-999 appears to be NA; Some REALLY far")
qplot(df_culv_out$here_distm) + ggtitle("Frequency of distance (m) to nearest HERE segment")
qplot(df_culv_out$slope_deg) + ggtitle("Frequency of slope (deg) in grid cell", "-9999 appears to be NA") + xlim(-1, 25)


tabyl(df_culv_out$nlcd_2016) %>% arrange(-percent)
# Class	              Value	Classification                Description
# Barren	            31	  Barren Land (Rock/Sand/Clay)	areas of bedrock, desert pavement, scarps, talus, slides, volcanic material, glacial debris, sand dunes, strip mines, gravel pits and other accumulations of earthen material. Generally, vegetation accounts for less than 15% of total cover.
# Developed	          21	  Developed, Open Space	        areas with a mixture of some constructed materials, but mostly vegetation in the form of lawn grasses. Impervious surfaces account for less than 20% of total cover. These areas most commonly include large-lot single-family housing units, parks, golf courses, and vegetation planted in developed settings for recreation, erosion control, or aesthetic purposes.
# Developed           22	  Developed, Low Intensity	    areas with a mixture of constructed materials and vegetation. Impervious surfaces account for 20% to 49% percent of total cover. These areas most commonly include single-family housing units.
# Developed           23	  Developed, Medium Intensity	  areas with a mixture of constructed materials and vegetation. Impervious surfaces account for 50% to 79% of the total cover. These areas most commonly include single-family housing units.
# Developed           24  	Developed High Intensity	    highly developed areas where people reside or work in high numbers. Examples include apartment complexes, row houses and commercial/industrial. Impervious surfaces account for 80% to 100% of the total cover.
# Forest              41	  Deciduous Forest	            areas dominated by trees generally greater than 5 meters tall, and greater than 20% of total vegetation cover. More than 75% of the tree species shed foliage simultaneously in response to seasonal change.
# Forest	            42	  Evergreen Forest	            areas dominated by trees generally greater than 5 meters tall, and greater than 20% of total vegetation cover. More than 75% of the tree species maintain their leaves all year. Canopy is never without green foliage.
# Forest	            43	  Mixed Forest	                areas dominated by trees generally greater than 5 meters tall, and greater than 20% of total vegetation cover. Neither deciduous nor evergreen species are greater than 75% of total tree cover.
# Herbaceous	        71	  Grassland/Herbaceous	        areas dominated by gramanoid or herbaceous vegetation, generally greater than 80% of total vegetation. These areas are not subject to intensive management such as tilling, but can be utilized for grazing.
# Herbaceous	        72	  Sedge/Herbaceous	Alaska only areas dominated by sedges and forbs, generally greater than 80% of total vegetation. This type can occur with significant other grasses or other grass like plants, and includes sedge tundra, and sedge tussock tundra.
# Herbaceous	        73	  Lichens	Alaska only           areas dominated by fruticose or foliose lichens generally greater than 80% of total vegetation.
# Herbaceous	        74	  Moss	Alaska only             areas dominated by mosses, generally greater than 80% of total vegetation.
# Planted-Cultivated	81	  Pasture/Hay                   areas of grasses, legumes, or grass	legume mixtures planted for livestock grazing or the production of seed or hay crops, typically on a perennial cycle. Pasture/hay vegetation accounts for greater than 20% of total vegetation.
# Planted-Cultivated	82	  Cultivated Crops	            areas used for the production of annual crops, such as corn, soybeans, vegetables, tobacco, and cotton, and also perennial woody crops such as orchards and vineyards. Crop vegetation accounts for greater than 20% of total vegetation. This class also includes all land being actively tilled.
# Shrubland	          51	  Dwarf Scrub	Alaska only       areas dominated by shrubs less than 20 centimeters tall with shrub canopy typically greater than 20% of total vegetation. This type is often co-associated with grasses, sedges, herbs, and non-vascular vegetation.
# Shrubland	          52	  Shrub/Scrub	                  areas dominated by shrubs; less than 5 meters tall with shrub canopy typically greater than 20% of total vegetation. This class includes true shrubs, young trees in an early successional stage or trees stunted from environmental conditions.
# Water	              11	  Open Water	                  areas of open water, generally with less than 25% cover of vegetation or soil.
# Water	              12	  Perennial Ice/Snow	          areas characterized by a perennial cover of ice and/or snow, generally greater than 25% of total cover.
# Wetlands	          90	  Woody Wetlands	              areas where forest or shrubland vegetation accounts for greater than 20% of vegetative cover and the soil or substrate is periodically saturated with or covered with water.
# Wetlands	          95	  Emergent Herbaceous Wetlands	areas where perennial herbaceous vegetation accounts for greater than 80% of vegetative cover and the soil or substrate is periodically saturated with or covered with water.

tabyl(df_culv_out$snet_spp) %>% arrange(-percent)
tabyl(df_culv_out$snet_use) %>% arrange(-percent)
tabyl(df_culv_out$here_publi) %>% arrange(-percent)
tabyl(df_culv_out$here_paved) %>% arrange(-percent)

# Simple model (needs adjustment of cost to cost per culvert)
lm(log(adj_cost) ~ factor(project_year) + factor(basin) + factor(project_source) + pure_culv + factor(nlcd_2016) + factor(snet_use) + slope_deg + factor(here_paved) + I(here_distm < 200):I(nhd_dist_m < 150) + emp_agforest + emp_const, data = df_culv_out %>% filter(slope_deg >= 0, nhd_dist_m >= 0)) %>% summary
