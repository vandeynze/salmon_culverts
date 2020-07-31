# TITLE: Attach OpenStreetMap data for nearest road to culvert work site
# AUTHOR: Braeden Van Deynze
# DATE: July, 2020
# INPUTS: "culverts_full_mapping.csv"" data for culvert work site data
# OUTPUTS: Data on nearest road to culvert work site
# STATUS: Proof-of-concept operable
# PRIORITY: (1) Identify distance threshold for unclassified road (2) Expand for full data (3) Prep for sourcing

# Based on example in https://stackoverflow.com/questions/47675571/r-spatial-join-between-spatialpoints-gps-coordinates-and-spatiallinesdatafra/47731067#47731067

# This script is proof-of-concept. It highlights a small selection of culverts in a small area, identifies the nearest OpenStreetMaps road to each culvert,
# then returns the distance to that road and all available data on that road from OSM.

# This program will likely need optimization (in terms of limiting search areas for nearest roads to reduce data held in memory). Examples in the link above.

# Many culverts are not near matches for roads. In post matching processing, we can flag work sites distant from roads for further classification.

rm(list = ls())
# Load libraries
library(raster)
library(sf)
library(sp)
library(geosphere)
library(osmdata)
library(mapdata)
library(ggmap)

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

# Set box
long_min = -123.3
long_max = -123
lat_min = 44.5
lat_max = 44.8

# Read roads
sf_roads <-
  opq(
    bbox = c(long_min, lat_min, long_max, lat_max), # Test near Salem, OR
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
sf_roads %>% st_drop_geometry() %>% as_tibble() %>% dplyr::select(name, highway, lanes, surface) %>% tabyl(surface)
# TODO: Can we do some sort of intropoloation for missing surface? Like, regional/local paved/unpaved share by road-type (primary, secondary, tertiary, etc.)
sf_roads %>% st_drop_geometry() %>% as_tibble() %>% dplyr::select(name, highway, lanes, surface) %>% tabyl(surface, highway)
# At least in this test sample, not much useful surface info...

# Build test grid near mean of lat/long (outside Salem, OR)
sf_culverts_test <-
  df_culverts %>%
  filter(
    latitude >= lat_min,
    latitude <= lat_max,
    longitude <= long_max,
    longitude >= long_min
  ) %>%
  st_as_sf(coords = c("longitude", "latitude")) %>%
  st_set_crs(4326)

# Map them
ggplot() +
  geom_sf(aes(color = highway), data = sf_roads) + 
  geom_sf(data = sf_culverts_test, color = "red") +
  coord_sf()
# Few culverts are directly on roads, but we can still use the geosphere library to identify the nearest road (line)

# Add sat. background for context
sf_sat <-
  get_map(
    # location = "salem, oregon",
    source = "google",
    location = c(left = long_min, right = long_max, bottom = lat_min, top = lat_max),
    maptype = "hybrid",
    scale = 4,
    force = TRUE
  )

ggmap_bbox <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")), 
                       c("ymin", "xmin", "ymax", "xmax"))
  
  # Coonvert the bbox to an sf polygon, transform it to 3857, 
  # and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
  
  # Overwrite the bbox of the ggmap object with the transformed coordinates 
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  map
}

sf_sat <- ggmap_bbox(sf_sat)

ggmap(sf_sat) +
# ggplot() +
  coord_sf(crs = st_crs(3857)) +
  geom_sf(aes(color = highway), data = sf_roads %>% st_transform(3857), inherit.aes = FALSE) + 
  geom_sf(data = sf_culverts_test %>% st_transform(3857), color = "red", inherit.aes = FALSE)

# Looks like the cluster at the top is a series of work sites on a wetland area near farm fields near the Willamette.
# Roads will be poor matches. The road matches will be wrong; these all look like they are on tiny, unpaved, private roads.


system.time(df_distance <- dist2Line(as(sf_culverts_test, "Spatial"), as(sf_roads, "Spatial")))
# On my laptop w/ 16 GB RAM, 2.8GHz, i7-7700HQ CPU
# user  system elapsed 
# 5.83    0.00    5.83 
# Even for just 11 culverts, and a fairly small area, still takes quite a long time
(df_distance <- df_distance %>% as_tibble())
# Row order is the same as row order in original culvert tibble; ID is the row number from the sf_roads simple feature collection; distance is distance in meters to nearest road
# So we can bind columns directly to the culvert tibble, convert the roads sf to a tibble with the row number as the ID, and then join to grab fields for road info

(sf_culverts_test <-
  sf_culverts_test %>%
  bind_cols(df_distance %>% as_tibble) %>%
  left_join(sf_roads %>% st_drop_geometry() %>% dplyr::select(-county) %>% mutate(ID = row_number()), by = "ID")) 

sf_culverts_test %>%
  dplyr::select(worksite_id:pure_culv, distance, name, highway, lanes, surface, maxspeed)

qplot(x = distance, data = sf_culverts_test) + ggtitle("Distribution of distance from nearest road (m)")

# Add poor match flag
match_thresh = 250

sf_culverts_test <-
  sf_culverts_test %>%
  mutate(poor_match = I(distance > match_thresh))

sf_culverts_test %>% tabyl(poor_match)

# Plot poor match flag
ggmap(sf_sat) +
  # ggplot() +
  coord_sf(crs = st_crs(3857)) +
  geom_sf(aes(color = highway), data = sf_roads %>% st_transform(3857), inherit.aes = FALSE) + 
  geom_sf(aes(shape = factor(poor_match)), data = sf_culverts_test %>% st_transform(3857), color = "red", size = 2, inherit.aes = FALSE) +
  ggtitle("Poor matches (> 250m) all in the ag/wetland interface in north-center portion of map, near the Willamette", "These matches are likely unreliable, but could be assumed to be in a similar small, unpaved road category")



# So we have a proof of concept that finds nearest road and identifies OSM class and features, nice!
# But even the most work sites are over 300m from the nearest OSM road, so we should probably make some sort of distance threshold beyond which we simply assign "unclassified"
# TODO: Experiment with distance thresholds
# TODO: Link into Google Maps or other to try to ID missing road
# TODO: Expand distance merge to larger share/all culverts
# TODO: Replicate for rivers/streams
