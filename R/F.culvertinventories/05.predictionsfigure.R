# Load libraries ----
library(tidyverse)
library(scales)
library(sf)
library(ggmap)
library(janitor)
library(here)
library(osmdata)
# devtools::install_github("3wen/legendMap")
library(legendMap)
library(maps)
library(RColorBrewer)

# Load data ----
sf_inv_preds <-
  here("output/inv_preds.csv") %>% read_csv(guess_max = 30000)  %>%
  mutate(geom = gsub(geometry,pattern="(\\))|(\\()|c",replacement = ""))%>%
  tidyr::separate(geom,into=c("lat","lon"),sep=",")%>%
  st_as_sf(.,coords=c("lat","lon"),crs=4326)

sf_allculv_wdfw <- st_read(here("data/culv_inventories/WdfwFishPassage/WdfwFishPassage.gdb"), layer = "WDFW_FishPassageSite") %>% clean_names() %>% st_transform(st_crs(sf_inv_preds))
# sf_allculv_odfw <- st_read(here("data/culv_inventories/ODFW_44_5_ofpbds_gdb/ofpbds_gdb.gdb"), layer = "ofpbds_pt") %>% clean_names() %>% st_transform(st_crs(sf_inv_preds))


# Plot it ----
xmin_sml = -121.85
xmax_sml = -121.5
ymin_sml = 48.45
ymax_sml = 48.56

xmin_big = -124.9
xmax_big = -120.6
ymin_big = 46.3
ymax_big = 49.1
scale_big = 1

# __ Load borders ----
sf_us <- raster::getData("GADM", country = "USA", download = TRUE, path = here("output"), level = 1) %>% st_as_sf() %>% filter(NAME_1 %in% c('California', 'Nevada', 'Utah', 'Wyoming', 'Montana', 'Idaho', 'Oregon', 'Washington'))
sf_canada <- raster::getData("GADM", country = "CAN", download = TRUE, path = here("output"), level = 1) %>% st_as_sf() %>% filter(NAME_1 %in% c("British Columbia", "Alberta"))
sf_base <- 
  rbind(sf_us, sf_canada)

# __ Set culvert data for mapping ----
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

# __ Load case area borders ----
sf_case <-
  st_read(here("data/WSDOT_-_Fish_Passage_US_v._WA_Case_Area_Boundary-shp/WSDOT_-_Fish_Passage_US_v._WA_Case_Area_Boundary.shp"), quiet = TRUE) %>%
  st_transform(st_crs(sf_base))

# __ Load basemaps ----
# Default resolution for full area
sf_landscape <-
  get_map(
    location = c(xmin_big, ymin_big, xmax_big, ymax_big),
    maptype = "terrain-background"
  )
# High-res basemap for concrete
# sf_landscape_hr <-
#   get_map(
#     location = c(xmin_sml, ymin_sml, xmax_sml, ymax_sml),
#     maptype = "terrain-background"
#   )

# __ Load streams ---- 
# High-res streams for concrete
# if(!file.exists(here("data/NHDPlusHR/17/NHDPLUS_H_1711_HU4_GDB.jpg"))) {
#   download_nhdplushr(here("data/NHDPlusHR"), "1711") 
# }
# sf_rivers_sml <- get_nhdplushr(here("data/NHDPlusHR"), layers = c("NHDFlowline", "NHDWaterbody"), proj = st_crs(sf_base))

# __ Load roads ----
# Open street maps roads for full area
sf_roads_big <- 
  opq(
    bbox = c(xmin_big, ymin_big, xmax_big, ymax_big),
    timeout = 60
  ) %>%
  add_osm_feature(key = "highway", value = c("motorway", "trunk", "primary", "secondary")) %>%
  osmdata_sf()
sf_roads_big$osm_lines <- sf_roads_big$osm_lines %>% st_transform(st_crs(sf_base))

# Open street maps roads for concrete
# sf_roads_sml <- 
#   opq(
#     bbox = c(xmin_sml, ymin_sml, xmax_sml, ymax_sml)
#   ) %>%
#   add_osm_feature(key = "highway") %>%
#   osmdata_sf()
# sf_roads_sml$osm_lines <-
#   sf_roads_sml$osm_lines %>%
#   mutate(
#     highway_size = case_when(
#       highway == "primary" ~ "highway",
#       highway == "primary_link" ~ "highway",
#       highway == "secondary" ~ "road",
#       highway == "tertiary" ~ "road",
#       highway == "unclassified" ~ "road",
#       highway == "residential" ~ "road",
#       highway %in% c("track", "service") ~ "service road",
#       highway %in% c("footway", "steps", "path") ~ "trail"
#       
#     )
#   ) %>%
#   st_transform(st_crs(sf_base))

# __ Set theme ----
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


# __ Plot barrier-level estimates ----
map_costpreds <-
  ggmap(sf_landscape, darken = c(0.3, "white")) +
  # ggplot() +
  # Add roads
  geom_sf(
    aes(
      size = highway,
      geometry = geometry
    ),
    data = sf_roads_big$osm_lines,
    color = "grey50",
    inherit.aes = FALSE
  ) +
  geom_sf(aes(color = costpred_brt), data = sf_inv_preds %>% st_as_sf() %>% st_transform("WGS84"), size = 0.5, inherit.aes = FALSE) +
  # Add case area borders
  geom_sf(
    data = sf_case, 
    fill = NA, 
    color = "red",
    linetype = "dashed", 
    inherit.aes = FALSE, 
    size = 0.8*scale_big
  ) +
  scale_bar(
    lon = xmin_big + 0.15,
    lat = ymin_big + 0.1,
    distance_lon = 30,
    distance_lat = 5,
    distance_legend = -5,
    # orientation = FALSE,
    arrow_length = 30, arrow_distance = 10, arrow_north_size = 6
  ) +
  scale_size_manual(
    values = c(
      "motorway" = 1*scale_big,
      "trunk" = 0.8*scale_big,
      "primary" = 0.5*scale_big,
      "secondary" = 0.5*scale_big
    ),
    guide = guide_none()
  ) +
  scale_color_fermenter(
    "Predicted cost decile",
    palette = "RdYlGn",
    direction = -1,
    # colors = c(brewer.pal(5,"RdYlGn")[5], brewer.pal(5,"RdYlGn")[3], brewer.pal(5,"RdYlGn")[1]),
    limits = c(min(sf_inv_preds$costpred_brt) - 0.00001, max(sf_inv_preds$costpred_brt) + 0.00001),
    breaks = 
      # c(0, seq(9, 12, 1), Inf),
      c(
        min(sf_inv_preds$costpred_brt) - 0.00001,
      quantile(
        sf_inv_preds$costpred_brt,
        probs = seq(0.1, 0.9, 0.1),
        na.rm = TRUE
      ),
      max(sf_inv_preds$costpred_brt) + 0.00001
      ),
    labels = percent(seq(0, 1, 0.1), 1),
    na.value = NA,
    oob = scales::censor,
    guide = guide_colorsteps(
      even.steps = TRUE,
      barheight = unit(10, "lines"),
      # show.limits = TRUE
    )
  ) +
  coord_sf(
    # xlim = c(xmin_sml, xmax_sml),
    # ylim = c(ymin_sml, ymax_sml)
    xlim = c(xmin_big, xmax_big),
    ylim = c(ymin_big, ymax_big)
  ) + 
  theme_map_custom()

ggsave(here("output/figs/fig_map_costpreds.png"), map_costpreds, width = 6.5, height = 4)



# __ Plot HUC8 level estimates ----
if(!file.exists(herehere("data/WBD_17_HU2_Shape/Shape/WBDHU8.shp"))) {
  download.file("https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/WBD/HU2/Shape/WBD_17_HU2_Shape.zip", here("data/WBD_17_HU2_Shape.zip"))
  unzip(here("data/WBD_17_HU2_Shape.zip"), exdir = here("data/WBD_17_HU2_Shape/"))
}
sf_huc12 <- st_read(here("data/WBD_17_HU2_Shape/Shape/WBDHU12.shp"))

sf_inv_preds_huc12 <-
  sf_inv_preds %>%
  mutate(
    huc12 = as.character(huc12)
  ) %>%
  group_by(huc12) %>%
  summarize(
    costpred_brt_mean = mean(costpred_brt, na.rm = TRUE),
    costpred_brt_coefvar = sd(costpred_brt, na.rm = TRUE)/mean(costpred_brt, na.rm = TRUE)
  ) %>%
  right_join(sf_huc12, by = "huc12")

ggplot() + 
  geom_sf(aes(fill = costpred_brt_coefvar), data = sf_inv_preds_huc12 %>% st_as_sf()) +
  coord_sf(
    # xlim = c(xmin_sml, xmax_sml),
    # ylim = c(ymin_sml, ymax_sml)
    xlim = c(xmin_big, xmax_big),
    ylim = c(ymin_big, ymax_big)
  )  + 
  scale_fill_fermenter(
    palette = "RdYlGn", 
    direction = -1, 
    # breaks = 
    #   c(0, seq(9, 12, 1), Inf),
    # quantile(
    #   sf_inv_preds$costpred_ols, 
    #   probs = seq(0, 1, 0.2),
    #   na.rm = TRUE
    # ),
    # labels = function(x) exp(x) %>% dollar(),
    na.value = NA
  )
