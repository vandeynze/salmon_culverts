# TITLE: Explore fish passage inventories
# AUTHOR: B. Van Deynze
# DATA: Oct. 2020

# Prepare environment ----
rm(list = ls())
# Load libraries
library(tidyverse)
library(here)
library(janitor)
library(sf)

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

# Check some key variables
sf_allculv_wdfw %>% tabyl(feature_type)
# Lots of culverts! Over 35k!
summary(sf_allculv_wdfw$lineal_gain_measurement)
# Big distribution of lineal gain, though unclear how this is calculated; appears to be in meters
sf_allculv_wdfw %>% tabyl(fish_passage_feature_type_code)
sf_allculv_wdfw %>% tabyl(fish_passage_barrier_status_code)
sf_allculv_wdfw %>% tabyl(percent_fish_passable_code)
sf_allculv_wdfw %>% tabyl(owner_type_code)
sf_allculv_wdfw %>% tabyl(potential_species)
sf_allculv_wdfw %>% tabyl(fish_use_code)
sf_allculv_wdfw %>% tabyl(case_area_flag)
sf_allculv_wdfw %>% tabyl(huc12name)
# Lotds of codes, will probably need to request a code book from WDFW



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
sf_allculv_odfw %>% tabyl(fpb_own)
# Owner by type
sf_allculv_odfw %>% tabyl(fpb_own_ty)
# Fish passage status (52% unknown)
sf_allculv_odfw %>% tabyl(fpb_f_pas_sta)
