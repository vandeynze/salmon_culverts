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
library(modelr)
library(readxl)
library(gbm)
library(patchwork)


# Load data ----
# Load NLCD key
key_nlcd <-
  read_xlsx(
    here(
      "/data/Culverts spatial overlays v 20Jan2021.xlsx"
    ), 
    sheet = 3
  ) %>% 
  as_tibble() %>%
  clean_names() %>%
  rename(
    nlcd_current_class = class,
    nlcd_current_fullclass = classification
  ) %>%
  mutate(across(where(is_character), str_to_sentence)) %>%
  filter(across(description, ~!str_detect(., "Alaska only"))) %>%
  select(-description)

# Load data
df_culv <-
  read_csv(here("output", "culverts_pure_modelling.csv")) %>%
  mutate(
    # project_year = ordered(project_year),
    project_source = relevel(factor(project_source), ref = "OWRI"),
    basin = relevel(factor(basin), ref = "SOUTHERN OREGON COASTAL"),
    fips = factor(fips),
    state_fips = factor(state_fips),
    here_class = as.character(here_class),
    here_speed = relevel(factor(here_speed), ref = 6),
    tot_dist = I(n_worksites * dist_mean)
  ) %>%
  left_join(
    key_nlcd, 
    by = c("nlcd_current" = "value")
  ) %>%
  filter(
    !(nlcd_current_class %in% c("Barren", "Water")),
    tot_dist < 10000
  ) %>%
  mutate(
    nlcd_current_class = relevel(factor(nlcd_current_class), ref = "Forest")
  )

# Load inventories
sf_inv_preds <-
  here("output/inv_preds.csv") %>% read_csv(guess_max = 30000)  %>%
  mutate(geom = gsub(geometry,pattern="(\\))|(\\()|c",replacement = ""))%>%
  tidyr::separate(geom,into=c("lat","lon"),sep=",")%>%
  st_as_sf(.,coords=c("lat","lon"),crs=4326)

# sf_allculv_odfw <- st_read(here("data/culv_inventories/ODFW_44_5_ofpbds_gdb/ofpbds_gdb.gdb"), layer = "ofpbds_pt") %>% clean_names() %>% st_transform(st_crs(sf_inv_preds))

sf_allculv_wdfw <- st_read(here("data/culv_inventories/WdfwFishPassage/WdfwFishPassage.gdb"), layer = "WDFW_FishPassageSite")
sf_allculv_wdfw %>% clean_names() %>% 
  filter(
    feature_type == "Culvert"
  ) %>% select(site_record_id:feature_type) %>% st_drop_geometry() %>% write_csv(here("output/allculvs/culvinventory_wdfw.csv"))
# Check projection
st_crs(sf_allculv_wdfw)
sf_allculv_wdfw <-
  sf_allculv_wdfw %>% clean_names() %>% st_transform(st_crs(sf_inv_preds)) %>%
  filter(
    feature_type == "Culvert",
    fish_passage_barrier_status_code == 10
  )

sf_allculv_odfw <- st_read(here("data/culv_inventories/ODFW_44_5_ofpbds_gdb/ofpbds_gdb.gdb"), layer = "ofpbds_pt")
sf_allculv_odfw %>% 
  clean_names() %>%
  filter(
    fpb_ftr_ty == "Culvert",
  ) %>% select(fpb_ftr_id:fpb_o_site_id) %>% st_drop_geometry() %>% write_csv(here("output/allculvs/culvinventory_odfw.csv"))
sf_allculv_odfw <-
  sf_allculv_odfw %>% clean_names() %>% st_transform(st_crs(sf_inv_preds)) %>%
  filter(
    fpb_ftr_ty == "Culvert",
    fpb_f_pas_sta %in% c("Blocked", "Partial")
  )

sf_allculv_odfw <-
  sf_allculv_odfw %>%
  mutate(Ownership = case_when(
    fpb_own_ty == "County" ~ "County",
    fpb_own_ty == "State" ~ "State",
    TRUE ~ "Other"
  ))
sf_allculv_wdfw <-
  sf_allculv_wdfw %>%
  mutate(Ownership = case_when(
    owner_type_code == 2 ~ "County",
    owner_type_code == 5 ~ "State",
    TRUE ~ "Other"
  ))

# Load models and predictions
mod_brt <- read_rds(here("output/costfits/boostedregression.rds"))
sf_culv_preds <-
  df_culv %>%
  modelr::add_predictions(mod_brt, var = "costpred_brt_true") %>%
  mutate(
    project_source = "OWRI",
    tot_dist = 0,
    dist_max = 0,
    dist_mean = 0,
    n_culverts = 1,
    n_worksites = 1,
    project_year = 2015
  ) %>%
  modelr::add_predictions(mod_brt, var = "costpred_brt") %>%
  mutate(
    latlong = str_sub(geometry, 3, nchar(geometry)-1),
    long = as.numeric(stringr::word(latlong, 1, sep = ", ")),
    lat = as.numeric(stringr::word(latlong, 2, sep = ", ")),
    latlong = NULL,
    geometry = NULL,
    residual = log(cost_per_culvert) - costpred_brt_true
  ) %>%
  st_as_sf(coords = c("long", "lat")) %>% st_set_crs("WGS84")
  

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

xmin_full = -126
xmax_full = -117
ymin_full = 41.5
ymax_full = 49.5


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

# __ Load basin borders
if(file.exists(here("output/wdb/WBD_National_GDB.zip"))) {
  sf_basin <-
    nhdplusTools::download_wbd(here("output/wdb")) %>% st_read(layer = "WBDHU6", quiet = TRUE)
} else {
  sf_basin <-
    st_read(here("output/wdb/WBD_National_GDB.gdb"), layer = "WBDHU6", quiet = TRUE)
}
# sf_basin <- sf_basin %>% filter(name %in% c("Puget Sound", "Willamette", "John Day", "Washington Coastal", "Southern Oregon Coastal", "Northern Oregon Coastal", "Lower Columbia", "Middle Columbia", "Upper Columbia"))


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
      panel.background = element_rect(fill = "aliceblue", size = 1),
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

# __ Plot full area as update to abstract figure ----
# Merge ownership variables into predictinos
sf_inv_preds <-
  sf_inv_preds %>%
  left_join(
    sf_allculv_wdfw %>%
      st_drop_geometry() %>%
      select(site_record_id, ownership_wdfw = Ownership),
    by = c("site_recor" = "site_record_id")
  ) %>%
  left_join(
    sf_allculv_odfw %>%
      st_drop_geometry() %>%
      select(fpb_ftr_id, ownership_odfw = Ownership),
    by = c("fpb_ftr_id" = "fpb_ftr_id")
  ) %>%
  mutate(
    ownership = case_when(
      is.na(ownership_wdfw) ~ ownership_odfw,
      is.na(ownership_odfw) ~ ownership_wdfw
    )
  )

sf_inv_preds_barrier <-
  sf_inv_preds %>%
  filter(!is.na(ownership)) %>%
  mutate(ownership = ordered(ownership, levels = c("State", "County", "Other")))

# Map it
map_full_base <-
  ggplot() +
  geom_sf(data = sf_base, fill = "antiquewhite1", color = "black") +
  geom_sf(
    aes(
      # color = exp(costpred_brt),
      color = ownership
    ),
    data = sf_inv_preds_barrier,
    size = 1,
    # color = "grey60"
  ) +
  geom_sf(data = sf_basin, fill = NA, color = "red") +
  scale_color_manual(
    "Ownership entity",
    values = c("State" = "grey40", "County" = "grey60", "Other" = "grey80"), 
    breaks = c("County", "State", "Other"), 
    guide = guide_legend(override.aes = list(size = 3))
  ) +
  # scale_color_distiller(
  #   "Predicted cost",
  #   # palette = "YlGn",
  #   # palette = "Spectral",
  #   palette = "RdYlGn",
  #   limits = c(2000, 650000),
  #   labels = dollar_format(accuracy = 1),
  #   # breaks = c(30000, 50000, 100000, 300000),
  #   direction = -1,
  #   trans = "log10",
  #   na.value = "grey70",
  #   guide = guide_none()
  #   # guide = guide_colorbar(barwidth = unit(20, "pt"), barheight = unit(90, "pt"))
  # ) +
  coord_sf(
    xlim = c(xmin_full, xmax_full),
    ylim = c(ymin_full, ymax_full),
    expand = FALSE
  ) +
  theme_map_custom() +
  theme(
    # legend.position = c(0.97, 0.03), legend.justification = c(1, 0),
    legend.background = element_rect(color = "black"), 
    legend.key.size = unit(0.3, "pt"),
    legend.box = "vertical",
    # legend.title = element_text(size = 8),
    # legend.text = element_text(size = 6)
  )

map_full_base_sample <-
  map_full_base +
  geom_sf(
    aes(
      # fill = exp(costpred_brt)
      fill = cost_per_culvert
    ),
    data = sf_culv_preds,
    shape = 21,
    color = "black",
    stroke = 0.1,
    size = 3
  ) +
  scale_fill_distiller(
    # "Predicted cost",
    "Reported cost",
    # palette = "YlGn",
    # palette = "Spectral",
    palette = "RdYlGn",
    limits = c(2000, 650000),
    labels = dollar_format(accuracy = 1),
    # breaks = c(30000, 50000, 100000, 300000),
    direction = -1,
    trans = "log10",
    na.value = "grey70",
    guide = guide_colorbar(barwidth = unit(20, "pt"), barheight = unit(90, "pt"), direction = "vertical")
  ) +
  coord_sf(
    xlim = c(xmin_full, xmax_full),
    ylim = c(ymin_full, ymax_full),
    expand = FALSE
  )

map_full_preds_base <-
  ggplot() +
  geom_sf(data = sf_base, fill = "antiquewhite1", color = "black") +
  geom_sf(
    aes(
      color = exp(costpred_brt),
      # color = ownership
    ),
    data = sf_inv_preds_barrier,
    size = 1,
    # color = "grey60"
  ) +
  geom_sf(data = sf_basin, fill = NA, color = "red") +
  # scale_color_manual(
  #   "Ownership entity",
  #   values = c("State" = "grey40", "County" = "grey60", "Other" = "grey80"), 
  #   breaks = c("County", "State", "Other"), 
  #   guide = guide_legend(override.aes = list(size = 3))
  # ) +
  scale_color_distiller(
    "Predicted cost\npercentile",
    # palette = "YlGn",
    # palette = "Spectral",
    palette = "RdYlGn",
    # limits = c(2000, 650000),
    # labels = dollar_format(accuracy = 1),
    breaks = quantile(exp(sf_inv_preds$costpred_brt), probs = c(0.05, seq(0, 1, 0.2), 0.95)),
    limits = c(min(exp(sf_inv_preds$costpred_brt)), max(exp(sf_inv_preds$costpred_brt))),
    direction = -1,
    trans = "log10",
    na.value = "grey70",
    # guide = guide_none()
    guide = guide_colorbar(barwidth = unit(20, "pt"), barheight = unit(90, "pt"))
  ) +
  coord_sf(
    xlim = c(xmin_full, xmax_full),
    ylim = c(ymin_full, ymax_full),
    expand = FALSE
  ) +
  theme_map_custom() +
  theme(
    # legend.position = c(0.97, 0.03), legend.justification = c(1, 0),
    legend.background = element_rect(color = "black"), 
    legend.key.size = unit(0.3, "pt"), 
    # legend.title = element_text(size = 8),
    # legend.text = element_text(size = 6)
  )

map_full_preds_sample <-
  map_full_preds_base +
  geom_sf(
    aes(
      fill = exp(costpred_brt)
      # fill = cost_per_culvert
    ),
    data = sf_culv_preds,
    shape = 21,
    color = "black",
    stroke = 0.1,
    size = 3
  ) +
  scale_fill_distiller(
    "Predicted cost\npercentile",
    # "Reported cost",
    # palette = "YlGn",
    # palette = "Spectral",
    palette = "RdYlGn",
    # limits = c(2000, 650000),
    # labels = dollar_format(accuracy = 1),
    breaks = quantile(exp(sf_inv_preds$costpred_brt), probs = c(0.05, seq(0, 1, 0.2), 0.95)),
    limits = c(min(exp(sf_inv_preds$costpred_brt)), max(exp(sf_inv_preds$costpred_brt))),
    direction = -1,
    trans = "log10",
    na.value = "grey70",
    guide = guide_none()
    # guide = guide_colorbar(barwidth = unit(20, "pt"), barheight = unit(90, "pt"))
  ) +
  coord_sf(
    xlim = c(xmin_full, xmax_full),
    ylim = c(ymin_full, ymax_full),
    expand = FALSE
  )

map_full_preds_ownership <-
  map_full_preds_base %+%
  sf_inv_preds_barrier +
  facet_wrap(~ ownership) +
  # theme(
  #   legend.position = "none"
  # ) +
  coord_sf(
    xlim = c(xmin_full, xmax_full),
    ylim = c(ymin_full, ymax_full),
    expand = FALSE
  )

ggsave(
  here("output/figs/fig_culvdata.png"),
  map_full_base,
  height = 7.5,
  width = 6.5
)

ggsave(
  here("output/figs/fig_culvdata_sample.png"),
  map_full_base_sample,
  height = 7.5,
  width = 6.5
)

ggsave(
  here("output/figs/fig_culvpreds.png"),
  cowplot::plot_grid(
    map_full_preds_sample, 
    map_full_preds_ownership + theme(legend.position = "none"),
    labels = c("A", "B"),
    ncol = 1, 
    rel_heights = c(2, 1))
  ,
  height = 7.5,
  width = 6.5
)

ggsave(
  here("output/figs/fig_culvpreds_sample.png"),
  map_full_preds_sample,
  height = 7.5,
  width = 6.5
)

ggsave(
  here("output/figs/fig_culvpreds_own.png"),
  map_full_preds_ownership,
  height = 4,
  width = 13
)

ggsave(
  here("output/figs/fig_culvpreds_base.png"),
  map_full_preds_base,
  height = 7.5,
  width = 6.5
)

# __ Plot HUC10 level estimates ----
if(!file.exists(here("data/WBD_17_HU2_Shape/Shape/WBDHU8.shp"))) {
  download.file("https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/WBD/HU2/Shape/WBD_17_HU2_Shape.zip", here("data/WBD_17_HU2_Shape.zip"))
  unzip(here("data/WBD_17_HU2_Shape.zip"), exdir = here("data/WBD_17_HU2_Shape/"))
}
sf_huc10 <- st_read(here("data/WBD_17_HU2_Shape/Shape/WBDHU10.shp"))

sf_inv_preds_huc10 <-
  sf_inv_preds %>%
  mutate(
    # huc12 = as.character(huc12),
    huc10 = trunc(huc12/1e2) %>% as.character()
  ) %>%
  st_drop_geometry() %>%
  group_by(huc10) %>%
  summarize(
    costpred_brt_mean = mean(exp(costpred_brt), na.rm = TRUE),
    costpred_brt_coefvar = sd(exp(costpred_brt), na.rm = TRUE)/mean(exp(costpred_brt), na.rm = TRUE)
  ) %>%
  mutate(
    costpred_brt_mean_q = percent_rank(costpred_brt_mean)
  ) %>%
  right_join(sf_huc10, by = "huc10")

map_full_preds_mean <-
  ggplot() +
  geom_sf(data = sf_base, fill = "antiquewhite1", color = "black") +
  geom_sf(
    aes(
      fill = costpred_brt_mean_q
    ),
    color = NA,
    data = sf_inv_preds_huc10 %>% st_as_sf() %>% st_transform(st_crs(sf_base)),
    alpha = 0.75
  ) +
  geom_sf(data = sf_basin, fill = NA, color = "red") +
  theme_map_custom() +
  theme(
    # legend.position = c(0.97, 0.03), legend.justification = c(1, 0),
    legend.background = element_rect(color = "black"), 
    legend.key.size = unit(0.3, "pt"), 
    # legend.title = element_text(size = 8),
    # legend.text = element_text(size = 6)
  ) + 
  coord_sf(
    xlim = c(xmin_full, xmax_full),
    ylim = c(ymin_full, ymax_full),
    expand = FALSE
  )  + 
  scale_fill_fermenter(
    "Predicted cost\npercentile",
    palette = "RdYlGn", 
    direction = -1, 
    breaks = extended_breaks(7),
    limits = c(0.01, 0.99),
    labels = scales::percent_format(1),
    # breaks = 
    #   c(0, seq(9, 12, 1), Inf),
    # quantile(
    #   sf_inv_preds$costpred_ols, 
    #   probs = seq(0, 1, 0.2),
    #   na.rm = TRUE
    # ),
    # labels = function(x) exp(x) %>% dollar(),
    na.value = NA,
    guide = guide_colorsteps(barwidth = unit(20, "pt"), barheight = unit(90, "pt"))
  )

ggsave(
  here("output/figs/fig_culvpreds_mean.png"),
  map_full_preds_mean,
  height = 7.5,
  width = 6.5
)


map_full_preds_var <-
  ggplot() +
  geom_sf(data = sf_base, fill = "antiquewhite1", color = "black") +
  geom_sf(
    aes(
      fill = costpred_brt_coefvar
    ),
    color = NA,
    data = sf_inv_preds_huc10 %>% st_as_sf() %>% st_transform(st_crs(sf_base)),
    alpha = 0.75
  ) +
  geom_sf(data = sf_basin, fill = NA, color = "red") +
  theme_map_custom() +
  theme(
    # legend.position = c(0.97, 0.03), legend.justification = c(1, 0),
    legend.background = element_rect(color = "black"), 
    legend.key.size = unit(0.3, "pt"), 
    # legend.title = element_text(size = 8),
    # legend.text = element_text(size = 6)
  ) + 
  coord_sf(
    xlim = c(xmin_full, xmax_full),
    ylim = c(ymin_full, ymax_full),
    expand = FALSE
  )  + 
  scale_fill_fermenter(
    "Predicted cost\nvariability",
    palette = "RdYlGn", 
    direction = -1, 
    breaks = extended_breaks(7),
    limits = c(0.01, 0.99),
    # breaks = 
    #   c(0, seq(9, 12, 1), Inf),
    # quantile(
    #   sf_inv_preds$costpred_ols, 
    #   probs = seq(0, 1, 0.2),
    #   na.rm = TRUE
    # ),
    # labels = function(x) exp(x) %>% dollar(),
    na.value = NA,
    guide = guide_colorsteps(barwidth = unit(20, "pt"), barheight = unit(90, "pt"))
  )

ggsave(
  here("output/figs/fig_culvpreds_var.png"),
  map_full_preds_var,
  height = 7.5,
  width = 6.5
)

# Breakdown by ownership

sf_allculv_own <-
  bind_rows(
    sf_allculv_odfw %>%
      st_drop_geometry() %>%
      dplyr::select(fpb_ftr_id, Ownership),
    sf_allculv_wdfw %>%
      st_drop_geometry() %>%
      dplyr::select(site_id, Ownership)
  ) %>%
  rename(ownership = Ownership)

sf_inv_preds_huc10_own <-
  sf_inv_preds %>%
  mutate(
    # huc12 = as.character(huc12),
    huc10 = trunc(huc12/1e2) %>% as.character()
  ) %>%
  left_join(sf_allculv_own) %>%
  st_drop_geometry() %>%
  group_by(huc10, ownership) %>%
  summarize(
    costpred_brt_mean = mean(exp(costpred_brt), na.rm = TRUE),
    costpred_brt_coefvar = sd(exp(costpred_brt), na.rm = TRUE)/mean(exp(costpred_brt), na.rm = TRUE)
  ) %>%
  mutate(
    costpred_brt_mean_q = percent_rank(costpred_brt_mean)
  ) %>%
  right_join(sf_huc10, by = "huc10") %>%
  drop_na(ownership) %>%
  mutate(ownership = ordered(ownership, levels = c("State", "County", "Other")))

map_full_preds_mean_own <-
  ggplot() +
  geom_sf(data = sf_base, fill = "antiquewhite1", color = "black") +
  geom_sf(
    aes(
      fill = costpred_brt_mean_q
    ),
    color = NA,
    data = sf_inv_preds_huc10_own %>% st_as_sf() %>% st_transform(st_crs(sf_base)),
    alpha = 0.75
  ) +
  geom_sf(data = sf_basin, fill = NA, color = "red") +
  theme_map_custom() +
  theme(
    # legend.position = c(0.97, 0.03), legend.justification = c(1, 0),
    legend.background = element_rect(color = "black"), 
    legend.key.size = unit(0.3, "pt"), 
    # legend.title = element_text(size = 8),
    # legend.text = element_text(size = 6)
  ) +
  facet_wrap(~ ownership, nrow = 1) + 
  coord_sf(
    xlim = c(xmin_full, xmax_full),
    ylim = c(ymin_full, ymax_full),
    expand = FALSE
  )  + 
  scale_fill_fermenter(
    "Predicted cost\npercentile",
    palette = "RdYlGn", 
    direction = -1, 
    breaks = extended_breaks(7),
    limits = c(0.01, 0.99),
    labels = scales::percent_format(1),
    # breaks = 
    #   c(0, seq(9, 12, 1), Inf),
    # quantile(
    #   sf_inv_preds$costpred_ols, 
    #   probs = seq(0, 1, 0.2),
    #   na.rm = TRUE
    # ),
    # labels = function(x) exp(x) %>% dollar(),
    na.value = NA,
    guide = guide_colorsteps(barwidth = unit(20, "pt"), barheight = unit(90, "pt"))
  ) 

ggsave(
  here("output/figs/fig_culvpreds_mean_own.png"),
  map_full_preds_mean_own,
  height = 4,
  width = 13
)


map_full_preds_var_own <-
  ggplot() +
  geom_sf(data = sf_base, fill = "antiquewhite1", color = "black") +
  geom_sf(
    aes(
      fill = costpred_brt_coefvar
    ),
    color = NA,
    data = sf_inv_preds_huc10_own %>% st_as_sf() %>% st_transform(st_crs(sf_base)),
    alpha = 0.75
  ) +
  geom_sf(data = sf_basin, fill = NA, color = "red") +
  theme_map_custom() +
  theme(
    # legend.position = c(0.97, 0.03), legend.justification = c(1, 0),
    legend.background = element_rect(color = "black"), 
    legend.key.size = unit(0.3, "pt"), 
    # legend.title = element_text(size = 8),
    # legend.text = element_text(size = 6)
  ) +
  facet_wrap(~ ownership, nrow = 1) + 
  coord_sf(
    xlim = c(xmin_full, xmax_full),
    ylim = c(ymin_full, ymax_full),
    expand = FALSE
  )  + 
  scale_fill_fermenter(
    "Predicted cost\nvariability",
    palette = "RdYlGn", 
    direction = -1, 
    breaks = extended_breaks(7),
    limits = c(0.01, 0.99),
    # breaks = 
    #   c(0, seq(9, 12, 1), Inf),
    # quantile(
    #   sf_inv_preds$costpred_ols, 
    #   probs = seq(0, 1, 0.2),
    #   na.rm = TRUE
    # ),
    # labels = function(x) exp(x) %>% dollar(),
    na.value = NA,
    guide = guide_colorsteps(barwidth = unit(20, "pt"), barheight = unit(90, "pt"))
  ) 

ggsave(
  here("output/figs/fig_culvpreds_var_own.png"),
  map_full_preds_var_own,
  height = 4,
  width = 13
)


sf_culv_preds_huc10 <-
  sf_culv_preds %>%
  mutate(
    # huc12 = as.character(huc12),
    huc10 = trunc(huc12/1e2) %>% as.character()
  ) %>%
  st_drop_geometry() %>%
  group_by(huc10) %>%
  summarize(
    costpred_brt_mean = mean(exp(costpred_brt), na.rm = TRUE),
    costpred_brt_coefvar = sd(exp(costpred_brt), na.rm = TRUE)/mean(exp(costpred_brt), na.rm = TRUE),
    costpred_brt_rmse = sqrt(mean(residual^2))
  ) %>%
  right_join(sf_huc10, by = "huc10")

map_full_preds_rmse <-
  ggplot() +
  geom_sf(data = sf_base, fill = "antiquewhite1", color = "black") +
  geom_sf(
    aes(
      fill = costpred_brt_rmse
    ),
    color = NA,
    data = sf_culv_preds_huc10 %>% st_as_sf() %>% st_transform(st_crs(sf_base)),
    alpha = 0.75
  ) +
  geom_sf(data = sf_basin, fill = NA, color = "red") +
  theme_map_custom() +
  theme(
    # legend.position = c(0.97, 0.03), legend.justification = c(1, 0),
    legend.background = element_rect(color = "black"), 
    legend.key.size = unit(0.3, "pt"), 
    # legend.title = element_text(size = 8),
    # legend.text = element_text(size = 6)
  ) + 
  coord_sf(
    xlim = c(xmin_full, xmax_full),
    ylim = c(ymin_full, ymax_full),
    expand = FALSE
  )  + 
  scale_fill_fermenter(
    "RMSE",
    palette = "RdYlGn", 
    direction = -1, 
    breaks = extended_breaks(7),
    # limits = c(0.01, 0.99),
    # breaks = 
    #   c(0, seq(9, 12, 1), Inf),
    # quantile(
    #   sf_inv_preds$costpred_ols, 
    #   probs = seq(0, 1, 0.2),
    #   na.rm = TRUE
    # ),
    # labels = function(x) exp(x) %>% dollar(),
    na.value = NA,
    guide = guide_colorsteps(barwidth = unit(20, "pt"), barheight = unit(90, "pt"))
  )
map_full_preds_rmse

# __ Plot densities of costs by ownership ----
# Population vs. sample
sf_inv_preds_barrier %>%
  ggplot() +
  geom_density(
    aes(
      x = exp(costpred_brt),
      y = after_stat(count),
      color = "Population",
      fill = "Population"
    ),
    size = 1,
    alpha = 0.4
  ) +
  geom_density(
    aes(
      x = exp(costpred_brt),
      y = after_stat(count),
      color = "Sample",
      fill = "Sample"
    ),
    data = sf_culv_preds,
    size = 1,
    alpha = 0.4
  ) +
  scale_x_log10("Predicted cost, log-scale", label = dollar) +
  scale_y_continuous("Count", label = comma) +
  scale_color_brewer(NULL, type = "qual") +
  scale_fill_brewer(NULL, type = "qual") +
  # facet_wrap(~ basin) +
  theme_bw() 

# Overlay
sf_inv_preds_barrier %>%
  ggplot() +
  geom_density(
    aes(
      x = exp(costpred_brt),
      y = after_stat(count)
    ),
    color = "black",
    # fill = "grey50",
    size = 1,
    alpha = 0.4
  ) + 
  geom_density(
    aes(
      x = exp(costpred_brt),
      y = after_stat(count),
      color = fct_rev(ownership),
      fill = fct_rev(ownership)
    ),
    size = 1,
    alpha = 0.4,
    # position = "stack"
  ) +
  scale_x_log10("Predicted cost, log-scale", label = dollar) +
  scale_y_continuous("Count", label = comma) +
  scale_color_brewer(NULL, type = "qual") +
  scale_fill_brewer(NULL, type = "qual") +
  theme_bw()

# Stacked
sf_inv_preds_barrier %>%
  ggplot() +
  geom_density(
    aes(
      x = exp(costpred_brt),
      y = after_stat(count),
      color = fct_rev(ownership),
      fill = fct_rev(ownership)
    ),
    size = 1,
    alpha = 0.4,
    position = "stack"
  ) +
  scale_x_log10("Predicted cost, log-scale", label = dollar) +
  scale_y_continuous("Count", label = comma) +
  scale_color_brewer(NULL, type = "qual") +
  scale_fill_brewer(NULL, type = "qual") +
  theme_bw()

# Facet
sf_inv_preds_barrier %>%
  ggplot() +
  geom_density(
    aes(
      x = exp(costpred_brt),
      y = after_stat(count)
    ),
    color = "black",
    # fill = "grey50",
    size = 1,
    alpha = 0.4
  ) + 
  geom_density(
    aes(
      x = exp(costpred_brt),
      y = after_stat(count),
      color = fct_rev(ownership),
      fill = fct_rev(ownership)
    ),
    size = 1,
    alpha = 0.4
  ) +
  scale_x_log10("Predicted cost, thou. log-scale", label = function(x) dollar(x/1000, 1)) +
  scale_y_continuous("Count", label = comma) +
  scale_color_brewer(NULL, type = "qual") +
  scale_fill_brewer(NULL, type = "qual") +
  theme_bw() +
  facet_grid(ownership ~ basin)
  
