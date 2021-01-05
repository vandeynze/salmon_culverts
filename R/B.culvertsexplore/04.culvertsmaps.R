# TITLE: Maps of culvert worksites
# AUTHOR: Braeden Van Deynze
# DATE: March, 2020
# INPUTS: "culverts_wrk_working.csv" data for pure culvert worksite data
# OUTPUTS: maps in "output/maps/culverts/" folders

# Prepeare environment ====
# Clear environment
rm(list = ls())

# Load libraries
library(ggmap)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(osmdata)
library(raster)
library(scales)
library(searchable)
library(tidyverse)
library(ggthemes)
library(janitor)
library(here)

# Set ggplot2 themes
theme_set(theme_clean())
theme_update(
  plot.background = element_rect(color = NA),
  plot.title.position = "plot"
) # Removes ugly black border of plot

# Load culvert data ====
df_culv_wrk_pure <- read_csv(here("/output/culverts_wrk_working.csv"))

# Prepare map data ====
# setwd("./maps/sf")
sf_us <- getData("GADM", country = "USA", download = TRUE, path = here("output"), level = 1) %>% st_as_sf() %>% filter(NAME_1 %in% c('California', 'Nevada', 'Utah', 'Wyoming', 'Montana', 'Idaho', 'Oregon', 'Washington'))
sf_canada <- getData("GADM", country = "CAN", download = TRUE, path = here("output"), level = 1) %>% st_as_sf() %>% filter(NAME_1 %in% c("British Columbia", "Alberta"))
sf_base <- 
  rbind(sf_us, sf_canada)

sf_roads <-
  opq(
    bbox = c(-126, 40, -110, 49.5),
    timeout = 3000,
    memsize = 4e+9
  ) %>%
  add_osm_feature(
    key = "highway",
    value = c(
      "motorway",
      "trunk",
      "primary"
    )
  ) %>%
  osmdata_sf() %>%
  unname_osmdata_sf()

sf_rivers <-
  opq(
    bbox = c(-126, 40, -110, 49.5),
    # bbox = c(-122.5, 47.5, -122, 48), # Seattle area check
    timeout = 3000,
    memsize = 4e+9
  ) %>%
  add_osm_feature(
    key = "waterway",
    value = c(
      # "streams", # Can also add "streams" and other levels for lower level streams, but dramatically increases file size
      # "canal",
      # "brook",
      "river",
      "riverbank"
    )
  ) %>%
  osmdata_sf()

sf_lakes <-
  opq(
    bbox = c(-126, 40, -110, 49.5),
    # bbox = c(-122.5, 47.5, -122, 48), # Seattle area check
    timeout = 3000,
    memsize = 4e+9
  ) %>%
  add_osm_feature(
    key = "water",
    value = c(
      "lake"
    )
  ) %>%
  osmdata_sf()

# Fix broken osm_multipolygons names for plotting (https://github.com/rstudio/leaflet/issues/631#issuecomment-504729274)
for(i in seq(nrow(sf_lakes$osm_multipolygons))) {
  names(sf_lakes$osm_multipolygons$geometry[i][[1]]) = NULL
  names(sf_lakes$osm_multipolygons$geometry[[i]][[1]]) = NULL
}
# sf_rivers <- unname_osmdata_sf(sf_rivers) # Doesn't seem to work
# names(sf_rivers$osm_multipolygons$geometry) = NULL
for(i in seq(nrow(sf_rivers$osm_multipolygons))) {
  names(sf_rivers$osm_multipolygons$geometry[i][[1]]) = NULL
  names(sf_rivers$osm_multipolygons$geometry[[i]][[1]]) = NULL
}

# Build base map ====
base_map <-
  ggplot() +
  geom_sf(data = sf_base, fill = "antiquewhite1", color = NA) +
  geom_sf(data = sf_rivers$osm_lines, color = "steelblue1", fill = "steelblue1", size = 0.3) +
  geom_sf(data = sf_rivers$osm_multilines, color = "steelblue1", fill = "steelblue1", size = 0.3) +
  geom_sf(data = sf_rivers$osm_polygons, color = "steelblue1", fill = "steelblue1", size = 0.3) +
  geom_sf(data = sf_rivers$osm_multipolygons, color = "steelblue1", fill = "steelblue1", size = 0.3) +
  geom_sf(data = sf_lakes$osm_lines, color = "steelblue1", fill = "steelblue1") +
  geom_sf(data = sf_lakes$osm_polygons %>% filter(water == "lake"), color = "steelblue1", fill = "steelblue1") +
  geom_sf(data = sf_lakes$osm_multipolygons, color = "steelblue1", fill = "steelblue1") +
  geom_sf(data = sf_lakes$osm_polygons %>% filter(is.na(water) & !is.na(place)), color = "steelblue1", fill = "antiquewhite1") +
  geom_sf(data = sf_roads$osm_lines, color = "firebrick1", size = 0.3) +
  geom_sf(data = sf_base, fill = NA, color = "black") +
  coord_sf(
    xlim = c(-126, -111.5), ylim = c(41.5, 49.5),
    # xlim = c(-122.5, -122), ylim = c(47.5, 48), # Seattle area check
    expand = FALSE
  ) +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "aliceblue", size = 1),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = c(0.99, 0.01),
    legend.justification = c("right", "bottom"),
    legend.box.background = element_rect(color = "black", size = 1),
    legend.title = element_text(size = 10)
  ) +
  labs(
    x = NULL,
    y = NULL
  )

base_map_draft <-
  ggplot() +
  geom_sf(data = sf_base, fill = "antiquewhite1", color = "black") +
  coord_sf(
    xlim = c(-126, -111.5),
    ylim = c(41.5, 49.5),
    expand = FALSE
  ) +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "aliceblue", size = 1),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = c(0.99, 0.01),
    legend.justification = c("right", "bottom"),
    legend.box.background = element_rect(color = "black", size = 1),
    legend.title = element_text(size = 10)
  ) +
  labs(
    x = NULL,
    y = NULL
  )

# Build further maps ====
fig_map_cost <-
  base_map_draft +
  # base_map +
  geom_point(
    data = df_culv_wrk_pure %>% filter(cost_per_culvert > 2000),
    aes(
      x = longitude,
      y = latitude,
      fill = cost_per_culvert
    ),
    shape = 21,
    color = "black",
    stroke = 0.1,
    size = 1.9
  ) +
  scale_fill_distiller(
    "Cost per culvert\n(log scale)",
    # palette = "YlGn",
    # palette = "Spectral",
    palette = "RdYlGn",
    labels = dollar,
    direction = -1,
    trans = "log10",
    na.value = "grey70"
  ) +
  ggtitle("Map of culvert worksites, by cost per culvert")
# fig_map_cost
ggsave(
  filename = here("/output/maps/culverts/fig_map_cost.png"),
  plot = fig_map_cost,
  device = "png",
  width = 8,
  height = 6
)

fig_map_year <-
  # base_map_draft +
  base_map +
  geom_point(
    data = df_culv_wrk_pure %>% filter(cost_per_culvert > 2000),
    aes(
      x = longitude,
      y = latitude,
      fill = completed_year
    ),
    shape = 21,
    color = "black",
    stroke = 0.1,
    size = 1.9
  ) +
  scale_fill_distiller(
    "Year",
    palette = "Spectral",
    direction = 1,
    na.value = "grey70"
  ) +
  ggtitle("Map of culvert worksites, by year")
# fig_map_year
ggsave(
  filename = here("/output/maps/culverts/fig_map_year.png"),
  plot = fig_map_year,
  device = "png",
  width = 8,
  height = 6
)

source_levels <-
  df_culv_wrk_pure %>%
  tabyl(project_source) %>%
  arrange(-n) %>%
  pull(project_source)

fig_map_source <-
  # base_map_draft +
  base_map +
  geom_point(
    data = df_culv_wrk_pure %>% filter(cost_per_culvert > 2000, project_source %in% source_levels[1:6]),
    aes(
      x = longitude,
      y = latitude,
      fill = project_source
    ),
    shape = 21,
    color = "black",
    stroke = 0.1,
    size = 1.9
  ) +
  scale_fill_brewer(
    "Source",
    palette = "Dark2"
  ) +
  ggtitle("Map of culvert worksites, by reporting source")
ggsave(
  filename = here("/output/maps/culverts/fig_map_source.png"),
  plot = fig_map_source,
  device = "png",
  width = 8,
  height = 6
)
