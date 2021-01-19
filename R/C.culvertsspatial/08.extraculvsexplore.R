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
