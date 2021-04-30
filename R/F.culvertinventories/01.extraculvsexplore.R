# TITLE: Explore fish passage inventories
# AUTHOR: B. Van Deynze
# DATA: Oct. 2020

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

# Download data ----
# ____ WDFW inventory ----
if(!file.exists(here("data/culv_inventories/WdfwFishPassage.zip"))){
  download.file(
    "https://fortress.wa.gov/dfw/public/PublicDownload/habitat/FishPassage/WdfwFishPassage.zip",
    here("data/culv_inventories/WdfwFishPassage.zip")
  )
  unzip(
    here("data/culv_inventories/WdfwFishPassage.zip"),
    exdir = here("data/culv_inventories/WdfwFishPassage")
  )
}

st_layers(here("data/culv_inventories/WdfwFishPassage/WdfwFishPassage.gdb"))
sf_allculv_wdfw <- st_read(here("data/culv_inventories/WdfwFishPassage/WdfwFishPassage.gdb"), layer = "WDFW_FishPassageSite")

# Check projection
st_crs(sf_allculv_wdfw)

# Check variable names
names(sf_allculv_wdfw)
# Weird NAD83 projection, will want to update

# Repair names
sf_allculv_wdfw <- sf_allculv_wdfw %>% clean_names()
names(sf_allculv_wdfw)
# Lots of variables look like they're codes, will need to find codebook
# Currently building custom codebook using https://geodataservices.wdfw.wa.gov/hp/fishpassage/index.html

# Check some key variables
sf_allculv_wdfw %>% tabyl(feature_type)
# Lots of culverts! Over 35k!
summary(sf_allculv_wdfw$lineal_gain_measurement)
# Big distribution of lineal gain, though unclear how this is calculated; appears to be in meters
sf_allculv_wdfw %>% filter(str_detect(feature_type, "Culvert")) %>% tabyl(fish_passage_feature_type_code) # Codes needed (1 = culvert, 2 = non-culvert xing, 3 = dam, 4 = other, 5 = natural barrier)
sf_allculv_wdfw %>% filter(str_detect(feature_type, "Culvert")) %>% tabyl(fish_passage_barrier_status_code) # Codes needed (0 = NA, 10 = Barrier, 20 = Not a barrier, 99 = Unknown)
sf_allculv_wdfw %>% tabyl(percent_fish_passable_code) # Codes needed (0 = NA, 10 = 0, 20 = 33, 30 = 66, 40 = 100, 99 = Unknown)
sf_allculv_wdfw %>% tabyl(owner_type_code) # Codes needed (1 = "city", 2 = "county", 3 = "federal", 4 = "private", 5 = "state", 6 = "tribal", 7 = "other", 8 = "port", 9 = "drainage district, 11 = "irrigation district", 12 = "unknown")
sf_allculv_wdfw %>% filter(feature_type == "Culvert", significant_reach_code == 10) %>% st_drop_geometry() %>% tabyl(owner_type_code, fish_passage_barrier_status_code) %>% pivot_longer(-1, "passage_status") %>% ggplot(aes(x = factor(owner_type_code), y = value, fill = passage_status)) + geom_col(position = "dodge") + ggtitle("Passage status by ownership type")
sf_allculv_wdfw %>% tabyl(potential_species) # Presented as list of potential species, will need to be separated with stringr tools
sf_allculv_wdfw %>% tabyl(fish_use_code) # Codes needed (0 = NA, 10 = Yes, 20 = No, 99 = Unknown)
sf_allculv_wdfw %>% tabyl(significant_reach_code) # Codes needed (0 = NA, 10 = Yes, 20 = No, 99 = Unknown)
sf_allculv_wdfw %>% tabyl(case_area_flag) # No values
# Lots of codes, will probably need to request a code book from WDFW

# ________ Map it ----
# Load borders
sf_us <- getData("GADM", country = "USA", download = TRUE, path = here("output"), level = 1) %>% st_as_sf() %>% filter(NAME_1 %in% c('California', 'Nevada', 'Utah', 'Wyoming', 'Montana', 'Idaho', 'Oregon', 'Washington'))
sf_canada <- getData("GADM", country = "CAN", download = TRUE, path = here("output"), level = 1) %>% st_as_sf() %>% filter(NAME_1 %in% c("British Columbia", "Alberta"))
sf_base <- 
  rbind(sf_us, sf_canada)

# Basin borders
if(file.exists(here("output/wdb/WBD_National_GDB.zip"))) {
  sf_basin <-
    nhdplusTools::download_wbd(here("output/wdb")) %>% st_read(layer = "WBDHU6", quiet = TRUE)
} else {
  sf_basin <-
    st_read(here("output/wdb/WBD_National_GDB.gdb"), layer = "WBDHU6", quiet = TRUE) %>%
    st_transform(st_crs(sf_base))
}

# sf_basin <- sf_basin %>% filter(name %in% c("Puget Sound", "Willamette", "John Day", "Washington Coastal", "Southern Oregon Coastal", "Northern Oregon Coastal", "Lower Columbia", "Middle Columbia", "Upper Columbia"))
# Case area borders
sf_case <-
  st_read(here("data/WSDOT_-_Fish_Passage_US_v._WA_Case_Area_Boundary-shp/WSDOT_-_Fish_Passage_US_v._WA_Case_Area_Boundary.shp"), quiet = TRUE) %>%
  st_transform(st_crs(sf_base))

# Set bounding parameters
# Big focuses on case area
xmin_big = -124.9
xmax_big = -120.7
ymin_big = 46.3
ymax_big = 49
# Sml focuses on concrete wa area
# xmin_sml = -121.9
# xmax_sml = -121.3
# ymin_sml = 48.4
# ymax_sml = 48.8

# Sml focuses on concrete wa even smaller
xmin_sml = -121.85
xmax_sml = -121.5
ymin_sml = 48.45
ymax_sml = 48.56

# Basemap for big map
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

# High-res streams for concrete
if(!file.exists(here("data/NHDPlusHR/17/NHDPLUS_H_1711_HU4_GDB.jpg"))) {
  download_nhdplushr(here("data/NHDPlusHR"), "1711") 
}
sf_rivers_sml <- get_nhdplushr(here("data/NHDPlusHR"), layers = c("NHDFlowline", "NHDWaterbody"), proj = st_crs(sf_base))

base_map_draft_big <-
  # ggplot() +
  ggmap(sf_landscape) +
  # ggmap(sf_landscape_hr) +
  # geom_sf(data = sf_base, fill = "antiquewhite1", color = "black") +
  geom_sf(data = sf_basin, fill = NA, color = "black", linetype = "dashed", inherit.aes = FALSE) +
  geom_sf(data = sf_case, fill = NA, color = "red", inherit.aes = FALSE) +
  # coord_sf(
  #   xlim = c(-124.9, -120.7),
  #   ylim = c(46.3, 49),
  #   expand = FALSE
  # ) +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "aliceblue", size = 1),
    # axis.text = element_blank(),
    axis.ticks = element_blank(),
    # legend.position = c(0.99, 0.01),
    # legend.justification = c("right", "bottom"),
    legend.position = "right",
    legend.box.background = element_rect(color = "black", size = 1),
    legend.title = element_text(size = 10),
    axis.title = element_blank()
  )

base_map_draft_sml <-
  # ggplot() +
  # ggmap(sf_landscape) +
  ggmap(sf_landscape_hr) +
  # geom_sf(data = sf_base, fill = "antiquewhite1", color = "black") +
  geom_sf(data = sf_rivers_sml$NHDFlowline, color = "cornflowerblue", inherit.aes = FALSE) +
  geom_sf(data = sf_rivers_sml$NHDWaterbody %>% filter(FTYPE != 378), color = "cornflowerblue", fill = "cornflowerblue", inherit.aes = FALSE) +
  geom_sf(data = sf_basin, fill = NA, color = "black", linetype = "dashed", inherit.aes = FALSE) +
  geom_sf(data = sf_case, fill = NA, color = "red", inherit.aes = FALSE) +
  # coord_sf(
  #   xlim = c(-124.9, -120.7),
  #   ylim = c(46.3, 49),
  #   expand = FALSE
  # ) +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "aliceblue", size = 1),
    # axis.text = element_blank(),
    axis.ticks = element_blank(),
    # legend.position = c(0.99, 0.01),
    # legend.justification = c("right", "bottom"),
    legend.position = "right",
    legend.box.background = element_rect(color = "black", size = 1),
    legend.title = element_text(size = 10),
    axis.title = element_blank()
  )

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

# Map small map
map_sml <-
  base_map_draft_sml +
  geom_sf(
    aes(
      # fill = passability,
      shape = type,
      size = type
    ),
    data = df_mapculv_wdfw %>% filter(type %in% c("non-culvert crossing", "dam")) %>% filter(passability != "100 percent"),
    inherit.aes = FALSE,
    fill = "black"
  ) +  
  geom_sf(
    aes(
      fill = ownership,
      shape = type,
      size = type
    ),
    data = df_mapculv_wdfw %>% filter(type %in% c("culvert")) %>% filter(passability != "100 percent"),
    inherit.aes = FALSE
  ) +
  # scale_fill_manual(
  #   values = c(
  #     "0 percent" = "red",
  #     "33 percent" = "orange",
  #     "66 percent" = "yellow",
  #     "100 percent" = "darkgreen",
  #     "Unknown" = "purple"
  #   ),
  #   na.value = "grey40",
  #   guide = guide_legend(override.aes = list(shape = 22, size = 2))
  # ) +
  # scale_alpha_manual(
  #   values = c(
  #     "TRUE" = 0.5,
  #     "FALSE" = 1
  #   )
  # ) +
  scale_fill_brewer(
    type = "qual",
    na.value = "white",
    na.translate = FALSE,
    guide = guide_legend(override.aes = list(shape = 22, size = 2))
  ) +
  scale_shape_manual(
    values = c(
      "culvert" = 21,
      "non-culvert crossing" = 23,
      "dam" = 22,
      "natural barrier" = 25,
      "other" = 24
    ),
    na.value = 4
  ) +
  scale_size_manual(
    values = c(
      "culvert" = 2.5,
      "non-culvert crossing" = 1,
      "dam" = 1,
      "natural barrier" = 0.3,
      "other" = 0.3
    ),
    na.value = 0.3
  ) +
  # coord_sf(
  #   xlim = c(st_bbox(sf_case %>% st_transform(st_crs(sf_base)))$xmin, st_bbox(sf_case%>% st_transform(st_crs(sf_base)))$xmax),
  #   ylim = c(st_bbox(sf_case %>% st_transform(st_crs(sf_base)))$ymin, st_bbox(sf_case%>% st_transform(st_crs(sf_base)))$ymax)
  # )
  # Coordinates for full case area
  # coord_sf(
  #   xlim = c(-124.9, -120.7),
  #   ylim = c(46.3, 49)
  # )
  # Coordinates for Concrete, WA area
  coord_sf(
    xlim = c(xmin_sml, xmax_sml),
    ylim = c(ymin_sml, ymax_sml)
  ) +
  theme(
    legend.position = "none",
    axis.text = element_blank()
  )

# Map big map
map_big <-
  base_map_draft_big +
  geom_sf(
    aes(
      # fill = passability,
      shape = type,
      size = type
    ),
    data = df_mapculv_wdfw %>% filter(type %in% c("non-culvert crossing", "dam")) %>% filter(passability != "100 percent"),
    inherit.aes = FALSE,
    fill = "black"
  ) +
  geom_sf(
    aes(
      fill = ownership,
      shape = type,
      size = type
    ),
    data = df_mapculv_wdfw %>% filter(type %in% c("culvert")) %>% filter(passability != "100 percent"),
    inherit.aes = FALSE
  ) +

  geom_sf(
    data = st_as_sfc(st_bbox(c(xmin = xmin_sml, xmax = xmax_sml, ymin = ymin_sml, ymax = ymax_sml), crs = st_crs(sf_base))),
    color = "red",
    size = 1.5,
    fill = NA,
    inherit.aes = FALSE
  ) +
  # scale_fill_manual(
  #   values = c(
  #     "0 percent" = "red",
  #     "33 percent" = "orange",
  #     "66 percent" = "yellow",
  #     "100 percent" = "darkgreen",
  #     "Unknown" = "purple"
  #   ),
  #   na.value = "grey40",
  #   guide = guide_legend(override.aes = list(shape = 22, size = 2))
  # ) +
  scale_fill_brewer(
    "Culvert ownership",
    type = "qual",
    na.value = "white",
    na.translate = FALSE,
    guide = guide_legend(override.aes = list(shape = 22, size = 3)),
    labels = str_to_sentence
  ) +
  scale_shape_manual(
    "Barrier type",
    values = c(
      "culvert" = 21,
      "non-culvert crossing" = 23,
      "dam" = 22,
      "natural barrier" = 25,
      "other" = 24
    ),
    guide = guide_legend(override.aes = list(size = 3)),
    na.value = 4,
    labels = str_to_sentence
  ) +
  scale_size_manual(
    values = c(
      "culvert" = 2,
      "non-culvert crossing" = 1,
      "dam" = 1,
      "natural barrier" = 0.3,
      "other" = 0.3
    ),
    na.value = 0.3,
    guide = guide_none()
  ) +
  # Coordinates for full case area
  coord_sf(
    xlim = c(xmin_big, xmax_big),
    ylim = c(ymin_big, ymax_big)
  ) +
  theme(
    legend.position = "left",
    legend.background = element_rect(color = "white")
  )

map_big + map_sml + 
  plot_layout(widths = c(2, 1))

# ____ ODFW inventory ----
if(!file.exists(here("data/culv_inventories/ODFW_44_5_ofpbds_gdb.zip"))){
  download.file(
    "https://nrimp.dfw.state.or.us/web%20stores/data%20libraries/files/ODFW/ODFW_44_5_ofpbds_gdb.zip",
    here("data/culv_inventories/ODFW_44_5_ofpbds_gdb.zip")
  )
  unzip(
    here("data/culv_inventories/ODFW_44_5_ofpbds_gdb.zip"),
    exdir = here("data/culv_inventories/ODFW_44_5_ofpbds_gdb")
  )
}

st_layers(here("data/culv_inventories/ODFW_44_5_ofpbds_gdb/ofpbds_gdb.gdb"))
sf_allculv_odfw <- st_read(here("data/culv_inventories/ODFW_44_5_ofpbds_gdb/ofpbds_gdb.gdb"), layer = "ofpbds_pt")

# Check projection
st_crs(sf_allculv_odfw)
# Similar NAD83 projection but for Oregon

# Check variable names
names(sf_allculv_odfw)

# Repair names
sf_allculv_odfw <- sf_allculv_odfw %>% clean_names()
names(sf_allculv_odfw)

# Barrier type
sf_allculv_odfw %>% tabyl(fpb_ftr_ty)
# Lots of culverts! Over 26k!

# Owner by characater vector
sf_allculv_odfw %>% filter(fpb_ftr_ty == "Culvert") %>% tabyl(fpb_own)
# Owner by type

sf_allculv_odfw %>% filter(fpb_ftr_ty == "Culvert") %>% tabyl(fpb_own_ty)
# Fish passage status (45% unknown)
sf_allculv_odfw %>% filter(fpb_ftr_ty == "Culvert") %>% tabyl(fpb_f_pas_sta)

# WA DNR RMAP data ----
# sf_rmap <- read_sf(here("data/culv_inventories/Public_Forest_Practices_WADNR_PUBLIC_FP_Road_Maint_Pts.lyr"))