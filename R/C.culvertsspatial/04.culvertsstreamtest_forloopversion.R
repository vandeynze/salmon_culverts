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
if(
  !file.exists(
    here(
      "data/NHDPlusV21_NationalData_Seamless_Geodatabase_Lower48_07.7z"
      )
    )
  ) {
  download.file(
    "https://s3.amazonaws.com/edap-nhdplus/NHDPlusV21/Data/NationalData/NHDPlusV21_NationalData_Seamless_Geodatabase_Lower48_07.7z",
    here("data/")
  )
}
# Extract in data folder
if(
  !file.exists(
    here(
      "data/NHDPlusNationalData/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb"
      )
    )
) stop("Extract NHDPlus into /data/ directory!")

# View available layers
# st_layers(dsn = here("data/NHDPlusNationalData/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb"))

# Set nhdplus path
nhdplus_path(here("data", "NHDPlusNationalData", "NHDPlusV21_National_Seamless_Flattened_Lower48.gdb"))

# Load flowline network data (Really big! This might take awhile)
fln <- read_sf(dsn = here("data/NHDPlusNationalData/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb"), 
                layer = "NHDFlowline_Network")

# Reformat to meet sf standards
fln <- st_zm(fln)

# List stream data variable names
names(fln)
st_crs(fln)

# Check flowlind ids

sample_flines <- sf::read_sf(system.file("extdata",
                                         "petapsco_flowlines.gpkg",
                                         package = "nhdplusTools"))
system.time(get_flowline_index(sample_flines,
                   sf::st_sfc(sf::st_point(c(-76.87479,
                                             39.48233)),
                              crs = 4326)))

# Takes much longer with the full fln, so let's crop it
st_bbox(df_culv)
df_culv <- st_transform(df_culv, st_crs(fln))
st_bbox(df_culv)

fln_sml <-
  st_crop(
    fln,
    df_culv
  )
# Much better
write_rds(fln_sml, here("output","spatial","nhdplus_fln_sml.rds"))

# TODO Write if exists functionality
fln_sml <- read_rds(here("output","spatial","nhdplus_fln_sml.rds"))
fln_sml_ls <- st_cast(fln_sml, "LINESTRING")

get_flowline_index(fln_sml_ls, df_culv %>% slice(1))

# We have lift-off for a COMID and distance ("offset")

# Build function to calculate total potential upstream habitat from point (does not account for other blockages)
upst_km <- function(x, nhd_source = fln_sml_ls) {
  # require(nhdplusTools)  
  start_point <- x
  start_comid <- get_flowline_index(nhd_source, start_point)
  UT_comids <- get_UT(nhd_source, start_comid) #upstream with tributaries
  
  fln_sub <- nhd_source %>% 
    filter(COMID %in% UT_comids)
  sum(fln_sub$LENGTHKM) 
}

# Build function to grab stream slope
# strm_slope <- function(x, nhd_source = fln) {
#   # require(nhdplusTools)
#   comid <- discover_nhdplus_id(x)
#   fln %>% filter(COMID == comid) %>% pull(SLOPE)
# }
# Done better by just joining on the fln_sml

# Test on subset
set.seed(832020)
df_test <- df_culv %>% sample_n(15)
# system.time(
#   (df_test <- 
#      df_test %>%
#      rowwise() %>%
#      mutate(
#        # upst_km = upst_km(geometry),
#        upst_km = tryCatch({upst_km(geometry)}, # ALL NAs
#                           error = function(e) {NA}),
#        slope = tryCatch({strm_slope(geometry)}, # ALL NAs
#                         error = function(e) {NA}),
#        comid = tryCatch({discover_nhdplus_id(geometry)},
#                         error = function(e) {NA})
#      ))
# )
# 
# df_test %>% select(worksite_id, project_year, basin, upst_km, slope, comid)

# Test non-tidyverse version

slope <- numeric(nrow(df_test))
for(i in 1:nrow(df_test)){
  slope[i] <- tryCatch({strm_slope(df_test$geometry[i])}, error = function(e) {NA})
}

comid <- numeric(nrow(df_test))
for(i in 1:nrow(df_test)){
  comid[i] <- tryCatch({discover_nhdplus_id(df_test$geometry[i])}, error = function(e) {NA})
}

df_stream <- tibble(comid, slope)

df_test <- bind_cols(df_test, df_stream)

# For 15 culverts
# user  system elapsed 
# 25.42   10.86   40.06 
df_test %>% select(worksite_id, project_year, basin, upst_dist, slope, comid)


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

# wrn <- warnings()

# Run on full
# (df_culv <- df_culv %>%
#   rowwise() %>%
#   mutate(
#     upst_km = tryCatch({upst_km(geometry)},
#                        error = function(e) {NA}),
#     slope = tryCatch({strm_slope(geometry)},
#                      error = function(e) {NA}),
#     comid = tryCatch({discover_nhdplus_id(geometry)},
#                      error = function(e) {NA})
#   ))

slope <- numeric(nrow(df_culv))
for(i in 1:nrow(df_culv)){
  slope[i] <- tryCatch({strm_slope(df_culv$geometry[i])}, error = function(e) {NA})
}

comid <- numeric(nrow(df_culv))
for(i in 1:nrow(df_culv)){
  comid[i] <- discover_nhdplus_id(df_culv$geometry[i])
}



df_stream <- tibble(comid, slope)

df_culv <- bind_cols(df_culv %>% select(-(comid:upst_dist)), df_stream)

sum(is.na(df_culv$slope))


# Write out
df_culv %>%
  st_drop_geometry() %>%
  write_csv(here("output/spatial/culverts_nhdstreams.csv"))
