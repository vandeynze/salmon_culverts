# Prepare environment ----
rm(list = ls())
# Load libraries
library(sf)
library(maps)
library(ggmap)
library(raster)
library(tidyverse)
library(here)
library(janitor)
library(tmap)
library(nhdplusTools)
library(patchwork)
library(osmdata)
# library(ggsn)
# devtools::install_github("3wen/legendMap")
library(legendMap)


# Set parameters ----
# ____ Set big map parameters ----
# Big focuses on case area
# Set bounding box
xmin_big = -124.9
xmax_big = -120.6
ymin_big = 46.3
ymax_big = 49.1
# Set scaling parameter to control overall size scales for printing
scale_big = 1

# ____ Set sml map parameters ----
# Sml focuses on concrete wa
# Set bounding box
xmin_sml = -121.85
xmax_sml = -121.5
ymin_sml = 48.45
ymax_sml = 48.56
scale_sml = 1


# Load data ----
# ____ Load borders ----
sf_us <- getData("GADM", country = "USA", download = TRUE, path = here("output"), level = 1) %>% st_as_sf() %>% filter(NAME_1 %in% c('California', 'Nevada', 'Utah', 'Wyoming', 'Montana', 'Idaho', 'Oregon', 'Washington'))
sf_canada <- getData("GADM", country = "CAN", download = TRUE, path = here("output"), level = 1) %>% st_as_sf() %>% filter(NAME_1 %in% c("British Columbia", "Alberta"))
sf_base <- 
  rbind(sf_us, sf_canada)

# ____ Load culverts ----
sf_allculv_wdfw <- st_read(here("data/culv_inventories/WdfwFishPassage/WdfwFishPassage.gdb"), layer = "WDFW_FishPassageSite")
sf_allculv_wdfw <- sf_allculv_wdfw %>% clean_names()

# Set culvert data for mapping
df_mapculv_wdfw <-
  sf_allculv_wdfw %>%
  mutate(
    passability = case_when(
      percent_fish_passable_code == 0 ~ NA_character_,
      percent_fish_passable_code == 10 ~ "0 percent",
      percent_fish_passable_code == 20 ~ "33 percent",
      percent_fish_passable_code == 30 ~ "66 percent",
      percent_fish_passable_code == 40 ~ "100 percent",
      percent_fish_passable_code == 99 ~ "Unknown",
      is.na(percent_fish_passable_code) ~ NA_character_
    ),
    passability = ordered(passability, levels = c("0 percent", "33 percent", "66 percent", "100 percent", "Unknown")),
    # Codes needed (1 = "city", 2 = "county", 3 = "federal", 4 = "private", 5 =
    # "state", 6 = "tribal", 7 = "other", 8 = "port", 9 = "drainage district, 11
    # = "irrigation district", 12 = "unknown")
    ownership = case_when(
      owner_type_code == 1 ~ "city",
      owner_type_code == 2 ~ "county",
      owner_type_code == 3 ~ "federal",
      owner_type_code == 4 ~ "private",
      owner_type_code == 5 ~ "state",
      is.na(owner_type_code) ~ NA_character_,
      TRUE ~ "other"
    ),
    # Codes needed (1 = culvert, 2 = non-culvert xing, 3 = dam, 4 = other, 5 = natural barrier)
    type = case_when(
      fish_passage_feature_type_code == 1 ~ "culvert",
      fish_passage_feature_type_code == 2 ~ "non-culvert crossing",
      fish_passage_feature_type_code == 3 ~ "dam",
      fish_passage_feature_type_code == 4 ~ "other",
      fish_passage_feature_type_code == 5 ~ "natural barrier",
      is.na(fish_passage_feature_type_code) ~ NA_character_
    )
  ) %>%
  st_transform(st_crs(sf_base)) %>%
  drop_na(passability)

# ____ Load basin borders ----
if(file.exists(here("output/wdb/WBD_National_GDB.zip"))) {
  sf_basin <-
    nhdplusTools::download_wbd(here("output/wdb")) %>% st_read(layer = "WBDHU6", quiet = TRUE)
} else {
  sf_basin <-
    st_read(here("output/wdb/WBD_National_GDB.gdb"), layer = "WBDHU6", quiet = TRUE) %>%
    st_transform(st_crs(sf_base))
}

# ____ Load case area borders ----
sf_case <-
  st_read(here("data/WSDOT_-_Fish_Passage_US_v._WA_Case_Area_Boundary-shp/WSDOT_-_Fish_Passage_US_v._WA_Case_Area_Boundary.shp"), quiet = TRUE) %>%
  st_transform(st_crs(sf_base))

# ____ Load basemaps ----
# Default resolution for full area
sf_landscape <-
  get_map(
    location = c(xmin_big, ymin_big, xmax_big, ymax_big),
    maptype = "terrain-background"
  )
# High-res basemap for concrete
sf_landscape_hr <-
  get_map(
    location = c(xmin_sml, ymin_sml, xmax_sml, ymax_sml),
    maptype = "terrain-background"
  )

# ____ Load streams ---- 
# High-res streams for concrete
if(!file.exists(here("data/NHDPlusHR/17/NHDPLUS_H_1711_HU4_GDB.jpg"))) {
  download_nhdplushr(here("data/NHDPlusHR"), "1711") 
}
sf_rivers_sml <- get_nhdplushr(here("data/NHDPlusHR"), layers = c("NHDFlowline", "NHDWaterbody"), proj = st_crs(sf_base))

# ____ Load roads ----
# Open street maps roads for full area
sf_roads_big <- 
  opq(
    bbox = c(xmin_big, ymin_big, xmax_big, ymax_big)
  ) %>%
  add_osm_feature(key = "highway", value = c("motorway", "trunk", "primary")) %>%
  osmdata_sf()
sf_roads_big$osm_lines <- sf_roads_big$osm_lines %>% st_transform(st_crs(sf_base))

# Open street maps roads for concrete
sf_roads_sml <- 
  opq(
    bbox = c(xmin_sml, ymin_sml, xmax_sml, ymax_sml)
  ) %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf()
sf_roads_sml$osm_lines <-
  sf_roads_sml$osm_lines %>%
  mutate(
    highway_size = case_when(
      highway == "primary" ~ "highway",
      highway == "primary_link" ~ "highway",
      highway == "secondary" ~ "road",
      highway == "tertiary" ~ "road",
      highway == "unclassified" ~ "road",
      highway == "residential" ~ "road",
      highway %in% c("track", "service") ~ "service road",
      highway %in% c("footway", "steps", "path") ~ "trail"
      
    )
  ) %>%
  st_transform(st_crs(sf_base))
# Set theme ----
theme_map_custom <- function() {
  theme_bw() %+replace%
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "right",
    legend.background = element_rect(color = NA)
  )
}
  
# Build shared scales ----
scales_shared <- 
  list(
    # Add scale for barrier ownership
    scale_fill_brewer(
      "Culvert ownership",
      type = "qual",
      na.value = "white",
      na.translate = FALSE,
      guide = 
        guide_legend(
          override.aes = 
            list(
              size = 4,
              shape = 22
            )
        ),
      labels = str_to_sentence
    ),
    # Add scale for barrier shapes
    scale_shape_manual(
      "Barrier type",
      values = c(
        "culvert" = 21,
        "non-culvert crossing" = 23,
        "dam" = 22
      ),
      na.value = 4,
      guide =
        guide_legend(
          override.aes = 
            list(
              size = 4
            )
        ),
      labels = str_to_sentence
    )
  )

# Build big map ----
map_big <-
  # Add basemap
  ggmap(sf_landscape) +
  # ggplot(df_mapculv_wdfw) +
  # Add roads
  geom_sf(
    aes(
      size = highway, 
      geometry = geometry
    ),
    data = sf_roads_big$osm_lines, 
    color = "darkred", 
    inherit.aes = FALSE
  ) +
  # Add basin borders
  # geom_sf(
  #   data = sf_basin,
  #   fill = NA, 
  #   color = "black", 
  #   linetype = "dashed", 
  #   inherit.aes = FALSE, 
  #   size = 0.5*scale_big
  # ) +
  # Add case area borders
  geom_sf(
    data = sf_case, 
    fill = NA, 
    color = "red",
    linetype = "dashed", 
    inherit.aes = FALSE, 
    size = 0.5*scale_big
  ) +
  # Add non-culvert barriers
  geom_sf(
    aes(
      shape = type
    ),
    data = df_mapculv_wdfw %>% filter(type %in% c("non-culvert crossing", "dam")) %>% filter(passability != "100 percent"),
    inherit.aes = FALSE,
    size = 0.8,
    fill = "black"
  ) +
  # Add barrier culverts
  geom_sf(
    aes(
      fill = ownership,
      shape = type
    ),
    data = df_mapculv_wdfw %>% filter(type %in% c("culvert")) %>% filter(passability != "100 percent"),
    size = 1,
    stroke = 0.6,
    inherit.aes = FALSE
  ) +
  # Add rectangle highlighting sml map
  geom_sf(
    data = st_as_sfc(st_bbox(c(xmin = xmin_sml, xmax = xmax_sml, ymin = ymin_sml, ymax = ymax_sml), crs = st_crs(sf_base))),
    color = "red",
    fill = NA,
    inherit.aes = FALSE
  ) +
  # Add scalebar and north compass
  scale_bar(
    lon = xmin_big + 0.15,
    lat = ymin_big + 0.1,
    distance_lon = 30,
    distance_lat = 5,
    distance_legend = -5,
    # orientation = FALSE,
    arrow_length = 30, arrow_distance = 10, arrow_north_size = 6
  ) +
  # Add shared scales
  scales_shared +
  # Add scale for road size
  scale_size_manual(
    values = c(
      "motorway" = 1*scale_big,
      "trunk" = 0.8*scale_big,
      "primary" = 0.5*scale_big
    ),
    guide = guide_none()
  ) +
  # Add coordinates for full case area (fixed)
  coord_sf(
    xlim = c(xmin_big, xmax_big),
    ylim = c(ymin_big, ymax_big)
  ) +
  # Add theme
  theme_map_custom()

# Save to test
ggsave(here("output/figs/fig_mapconcrete_big.png"), map_big)

# Build small map ----
map_sml <-
  ggmap(sf_landscape_hr) +
  # Add rivers and other waterbodies
  geom_sf(
    data = sf_rivers_sml$NHDFlowline,
    color = "cornflowerblue",
    size = 0.4,
    inherit.aes = FALSE
  ) +
  geom_sf(
    data = 
      sf_rivers_sml$NHDWaterbody %>% 
      # Remove ice
      filter(FTYPE != 378),
    color = "cornflowerblue", 
    fill = "cornflowerblue", 
    inherit.aes = FALSE
  ) +
  # Add roads
  geom_sf(
    aes(
      size = highway_size,
      linetype = highway_size,
      geometry = geometry
      ), 
    data = sf_roads_sml$osm_lines, 
    color = "darkred",
    inherit.aes = FALSE
  ) +
  # Add non-culvert barriers
  geom_sf(
    aes(
      shape = type
    ),
    data = df_mapculv_wdfw %>% filter(type %in% c("non-culvert crossing", "dam")) %>% filter(passability != "100 percent"),
    inherit.aes = FALSE,
    fill = "black"
  ) +
  # Add barrier culverts
  geom_sf(
    aes(
      fill = ownership,
      shape = type
    ),
    data = df_mapculv_wdfw %>% filter(type %in% c("culvert")) %>% filter(passability != "100 percent"),
    inherit.aes = FALSE
  ) +
  # Add scalebar and north compass
  scale_bar(
    lon = xmin_sml + 0.015,
    lat = ymin_sml + 0.01,
    distance_lon = 2,
    distance_lat = 0.4,
    distance_legend = -0.4,
    # orientation = FALSE,
    arrow_length = 2, arrow_distance = 0.8, arrow_north_size = 6
  ) +
  # Add shared scales
  scales_shared +
  # Add scale for road size
  scale_size_manual(
    values = c(
      "highway" = 1,
      "road" = 0.4,
      "service road" = 0.3,
      "trail" = 0.3
    ),
    guide = guide_none()
  ) +
  # Add scale for road linetype
  scale_linetype_manual(
    values = c(
      "highway" ="solid", 
      "road" = "solid",
      "service road" = "solid",
      "trail" = "dashed"
    ),
    guide = guide_none()
  ) +
  # Add coordinates for full case area (fixed)
  coord_sf(
    xlim = c(xmin_sml, xmax_sml),
    ylim = c(ymin_sml, ymax_sml)
  ) +
  # Add theme
  theme_map_custom() +
  theme(legend.position = "none")

# Save to test
ggsave(here("output/figs/fig_mapconcrete_sml.png"), map_sml)

# Build full figure ----
map_full <- 
  ((map_big / map_sml) | guide_area()) + plot_layout(guides = "collect", widths = c(3, 1)) + plot_annotation(tag_levels = "A") & 
  theme(plot.tag.position = "topleft")

ggsave(here("output/figs/fig_mapconcrete.png"), map_full, width = 6.5, height = 7)
