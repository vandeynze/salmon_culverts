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
library(janitor)

#nhdplusTools:::get_UT
#nhdplusTools:::get_start_comid

# Load culvert data
df_culv <- read_csv(here("output/culverts_full_mapping.csv")) %>% 
  st_as_sf(., coords = c('longitude', 'latitude'), crs = 4326) %>% 
  st_transform(., crs = 4269)

# Rejoining HUC codes
df_pnshp <-
  read_csv(here("data/PNSHP_raw.csv"), guess_max = 1e6) %>% clean_names() %>%
  distinct(worksite_pk, .keep_all = TRUE) %>%
  mutate(worksite_pk = str_to_upper(worksite_pk)) %>%
  select(worksite_pk, starts_with("huc"))

df_culv <-
  df_culv %>%
  left_join(df_pnshp, by = c("worksite_id" = "worksite_pk"))

# And add distinct HUC lists
list_huc4 <-
  unique(df_culv$huc_4th_field) %>% as.character()

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

# Read full NHDPlus if needed, then build crop version as rds
if(!file.exists(here("output","spatial","nhdplus_fln_sml.rds"))){
  fln <- read_sf(dsn = here("data/NHDPlusNationalData/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb"), 
                 layer = "NHDFlowline_Network")
  
  # Reformat to meet sf standards
  fln <- st_zm(fln)
  
  # List stream data variable names
  # names(fln)
  # st_crs(fln)
  
  # Takes much longer with the full fln, so let's crop it
  # st_bbox(df_culv)
  df_culv <- st_transform(df_culv, st_crs(fln))
  
  fln_sml <-
    st_crop(
      fln,
      df_culv
    )
  # Much better
  write_rds(fln_sml, here("output","spatial","nhdplus_fln_sml.rds"))
}

fln_sml <- read_rds(here("output","spatial","nhdplus_fln_sml.rds"))
# Reduce to linestring
fln_sml_ls <- st_cast(fln_sml, "LINESTRING")

# Download from...
# https://prd-tnm.s3.amazonaws.com/index.html?prefix=StagedProducts/Hydrography/NHDPlusHR/Beta/GDB/
st_layers(here("data/NHDPLUS_H_0101_HU4_GDB/NHDPLUS_H_0101_HU4_GDB.gdb"))
# NHDPlusFlowlineVAA includes...
# - Slope
# - Eleveation
# ... in more detail than we would get from v2.1
# There are also monthly precip and temp estimates, but not sure those are neccessary

fln_hr_test <- st_read(here("data/NHDPLUS_H_0101_HU4_GDB/NHDPLUS_H_0101_HU4_GDB.gdb"), layer = "NHDPlusIncrROMA")

# Get NHD HR
download_nhdplushr(here("data/NHDPlusHR"), hu_list = list_huc4[1])


# Get comids, reachcodes, reach size, and distance from linestring
df_comids <- get_flowline_index(fln_sml_ls, df_culv)

# We have lift-off for a COMID and distance ("offset")
# Surprisingly quick too!

df_culv <-
  df_culv %>%
  bind_cols(
    df_comids %>%
      clean_names()
  ) %>%
  left_join(
    fln_sml %>%
      st_drop_geometry() %>%
      clean_names() %>%
      select(comid, slope),
    by = "comid"
  )

# Really fast actually! Can easily be extended for use with other point collections

# Build function to calculate total potential upstream habitat from point (does not account for other blockages)
upst_km <- function(x, nhd_source = fln_sml_ls) {
  # require(nhdplusTools)  
  start_point <- x # Should be comid
  # start_comid <- get_flowline_index(nhd_source, start_point)
  UT_comids <- get_UT(nhd_source, start_point) #upstream with tributaries
  
  fln_sub <- nhd_source %>% 
    filter(COMID %in% UT_comids)
  sum(fln_sub$LENGTHKM) 
}

# Test
df_culv %>% slice(1:15) %>% rowwise() %>% mutate(test_upst = tryCatch(upst_km(comid), error = NA)) %>% select(test_upst)


# Execute 
df_culv <-
  df_culv %>% rowwise() %>% mutate(upst_dist = tryCatch(upst_km(comid), error = NA))
sum(is.na(df_culv$upst_dist))
summary(df_culv$upst_dist)
map(unique(df_culv$basin), ~summary(df_culv$upst_dist[df_culv$basin == .]))
qplot(x = log(upst_dist), data = df_culv, facets = ~ basin)

# Can use st_intersects with a poly cover (say, for species habitat use designations) on
# the nhd_source to get upstream hab by species, or habitat use (or both)

# Note that in the supp attributes joining step in the next file, we get a
# upstream distance measure, but not by species

# Build function to grab stream slope
# strm_slope <- function(x, nhd_source = fln) {
#   # require(nhdplusTools)
#   comid <- discover_nhdplus_id(x)
#   fln %>% filter(COMID == comid) %>% pull(SLOPE)
# }
# Done better by just joining on the fln_sml


# Write out
df_culv %>%
  # st_drop_geometry() %>%
  write_csv(here("output/spatial/culverts_nhdstreams.csv"))
