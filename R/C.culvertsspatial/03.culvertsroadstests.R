# TITLE: County econ data from US census for culverts
# AUTHOR: Braeden Van Deynze
# DATE: July, 2020
# INPUTS: "culverts_full_mapping.csv"" data for culvert work site data
# OUTPUTS: Data on nearest road to culvert work site

# Based on example in https://stackoverflow.com/questions/47675571/r-spatial-join-between-spatialpoints-gps-coordinates-and-spatiallinesdatafra/47731067#47731067

# Load libraries
library(raster)
library(sf)
library(sp)
library(geosphere)
library(osmdata)

library(tidyverse)
library(here)
library(janitor)



# Load data
df_culverts <- read_csv("output/culverts_full_mapping.csv", n_max = 10000)

# Where is the mean lat/long as a starting point for a test run
df_culverts %>%
  summarize(
    lat = mean(latitude),
    long = mean(longitude)
  )

# Read roads
sf_roads <-
  opq(
    bbox = c(-123.2, 44.65, -123.1, 44.8),
    timeout = 3000,
    memsize = 4e+9
  ) %>%
  add_osm_feature(
    key = "highway",
    value = c(
      "motorway",
      "trunk",
      "primary",
      "secondary",
      "tertiary",
      "unclassified",
      "residential"
    )
  ) %>%
  osmdata_sf()
sf_roads <- 
  sf_roads$osm_lines %>%
  st_set_crs(4326)
# Lots of missing info for important things like lanes (only ever 2 anyways in this snapshot) and surface (see summary below)
sf_roads %>% st_drop_geometry() %>% as_tibble() %>% select(name, highway, lanes, surface) %>% tabyl(surface)
# TODO: Can we do some sort of intropoloation for missing surface? Like, regional/local paved/unpaved share by road-type (primary, secondary, tertiary, etc.)

# Build test grid near mean of lat/long (outside Salem, OR)
sf_culverts_test <-
  df_culverts %>%
  filter(
    latitude >= 44.65,
    latitude <= 44.8,
    longitude <= -123.1,
    longitude >= -123.2
  ) %>%
  st_as_sf(coords = c("longitude", "latitude")) %>%
  st_set_crs(4326)

# Map them
ggplot() +
  geom_sf(aes(color = highway), data = sf_roads) + 
  geom_sf(data = sf_culverts_test, color = "red")
# None of the culverts are directly on roads, but we can still use the geosphere library to identify the nearest road (line)

system.time(df_distance <- dist2Line(as(sf_culverts_test, "Spatial"), as(sf_roads, "Spatial")))
# On my laptop w/ 16 GB RAM, 2.8GHz, i7-7700HQ CPU
# user  system elapsed 
# 5.83    0.00    5.83 
# Even for just 7 culverts, and a fairly small area, still takes quite a long time
(df_distance <- df_distance %>% as_tibble())
# Row order is the same as row order in original culvert tibble; ID is the row number from the sf_roads simple feature collection; distance is distance in meters to nearest road
# So we can bind columns directly to the culvert tibble, convert the roads sf to a tibble with the row number as the ID, and then join to grab fields for road info

sf_culverts_test <-
  sf_culverts_test %>%
  bind_cols(df_distance %>% as_tibble) %>%
  left_join(sf_roads %>% st_drop_geometry() %>% select(-county) %>% mutate(ID = row_number()), by = "ID")
# So we have a proof of concept that finds nearest road and identifies OSM class and features, nice!
# But even the nearest work site is nearly 300m from the nearest OSM road, so we should probably make some sort of distance threshold beyond which we simply assign "unclassified"
# TODO: Experiment with distance thresholds
# TODO: Expand distance merge to larger share/all culverts
# TODO: Replicate for rivers/streams
