# TITLE: NHD+ stream data for worksites
# AUTHOR: Sunny Jardine, Braeden Van Deynze
# DATE: Aug., 2020
# INPUTS: "culverts_full_mapping.csv"" data for culvert work site data
# OUTPUTS: Data on nearest NHD+ to culvert work site
# STATUS: Operable for full data, currently for slope and total upstream potential

# https://usgs-r.github.io/nhdplusTools/index.html
rm(list = ls())
library(sf)
library(tidyverse)
library(nhdplusTools)
library(here)


#nhdplusTools:::get_UT
#nhdplusTools:::get_start_comid

# Load culvert data
df_culv <- read_csv(here("output/culverts_full_mapping.csv")) %>% 
  st_as_sf(., coords = c('longitude', 'latitude'), crs = 4326) %>% 
  st_transform(., crs = 4269)

# Load streams data from NHD+
if(!file.exists(here("data/NHDPlusV21_NationalData_Seamless_Geodatabase_Lower48_07.7z"))) download.file("https://s3.amazonaws.com/edap-nhdplus/NHDPlusV21/Data/NationalData/NHDPlusV21_NationalData_Seamless_Geodatabase_Lower48_07.7z", here("data/"))
# Extract in data folder
if(!file.exists(here("data/NHDPlusNationalData/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb"))) stop("Extract NHDPlus into /data/ directory!")

# View available layers
# st_layers(dsn = here("data/NHDPlusNationalData/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb"))

# Load flowline network data (Really big! This might take awhile)
fln <- read_sf(dsn = here("data/NHDPlusNationalData/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb"), 
                layer = "NHDFlowline_Network") 

# Reformat to meet sf standards
fln <- st_zm(fln)

# List stream data variable names
names(fln)

# Build function to calculate total potential upstream habitat from point (does not account for other blockages)
upst_km <- function(x, nhd_source = fln) {
  # require(nhdplusTools)  
  start_point <- x
  start_comid <- discover_nhdplus_id(start_point)
  UT_comids <- get_UT(nhd_source, start_comid) #upstream with tributaries
  
  fln_sub <- nhd_source %>% 
    filter(COMID %in% UT_comids)
  sum(fln_sub$LENGTHKM) 
}

# Build function to grab stream slope
strm_slope <- function(x, nhd_source = fln) {
  require(nhdplusTools)
  comid <- discover_nhdplus_id(x)
  fln %>% filter(COMID == comid) %>% pull(SLOPE)
}


# Test on subset
set.seed(832020)
df_test <- df_culv %>% sample_n(15)
system.time(
  (df_test <- 
     df_test %>%
     rowwise() %>%
     mutate(
       upst_km = tryCatch({upst_km(geometry)}, # ALL NAs
                          error = function(e) {NA}),
       slope = tryCatch({strm_slope(geometry)}, # ALL NAs
                        error = function(e) {NA}),
       comid = tryCatch({discover_nhdplus_id(geometry)},
                        error = function(e) {NA})
     ))
)
# For 15 culverts
# user  system elapsed 
# 25.42   10.86   40.06 
df_test


# Map
start_comid <- df_test %>% mutate(start_comid = discover_nhdplus_id(geometry)) %>% pull(start_comid)
UT_comids <- map(start_comid, ~get_UT(fln, .)) %>% unlist #upstream with tributaries
upst_fln <-
  fln %>%
  filter(COMID %in% UT_comids)

ggplot() +
  # geom_sf(aes(geometry = Shape), data = fln, color = "lightblue1") + # Full rivers (adds a ton of time)
  geom_sf(aes(geometry = Shape), data = upst_fln, color = "lightblue2") + # Upstream rivers
  geom_sf(aes(geometry = geometry, color = "Target culverts"), data = df_test, size = 1) + # Target culverts
  geom_sf(aes(geometry = geometry), data = df_culv, color = "grey60", size = 0.5) + # All culverts
  coord_sf(xlim = c(st_bbox(upst_fln)[c(1,3)]), ylim = c(st_bbox(upst_fln)[c(2,4)])) +
  ggtitle("Upstream reaches for target culverts", "Obviously other blockages upstream will affect immediate impact") +
  scale_color_discrete("") +
  ggthemes::theme_map() +
  theme(legend.position = "right")

sum(is.na(df_culv$upst_km))
wrn <- warnings()

# Run on full
system.time(
  (df_culv <- df_culv %>%
    rowwise() %>%
    mutate(
      upst_dist = tryCatch({upst_km(geometry)},
                         error = function(e) {NA}),
      slope = tryCatch({strm_slope(geometry)},
                       error = function(e) {NA}),
      comid = tryCatch({discover_nhdplus_id(geometry)},
                       error = function(e) {NA})
    ))
)
sum(is.na(df_culv$upst_dist))


# Write out
df_culv %>%
  st_drop_geometry() %>%
  write_csv(here("output/spatial/culverts_nhdstreams.csv"))
