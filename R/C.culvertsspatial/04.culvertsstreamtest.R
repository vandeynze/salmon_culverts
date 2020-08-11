#https://usgs-r.github.io/nhdplusTools/index.html
rm(list = ls())
library(sf)
library(tidyverse)
library(nhdplusTools)
library(here)

#nhdplusTools:::get_UT
#nhdplusTools:::get_start_comid

df_culv <- read_csv(here("output/culverts_full_mapping.csv")) %>% 
  st_as_sf(., coords = c('longitude', 'latitude'), crs = 4326) %>% 
  st_transform(., crs = 4269)

st_layers(dsn = here("data/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb"))
fln <- read_sf(dsn = here("data/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb"), 
                layer = "NHDFlowline_Network") 

fln <- st_zm(fln)

upst_km <- function(x) {
  require(nhdplusTools)  
  start_point <- x
  start_comid <- discover_nhdplus_id(start_point)
  UT_comids <- get_UT(fln, start_comid) #upstream with tributaries
  
  fln_sub <- fln %>% 
    filter(COMID %in% UT_comids)
  sum(fln_sub$LENGTHKM) 
}
 
df_culv <- df_culv %>%
   rowwise() %>%
   mutate(upst_km = tryCatch({upst_km(geometry)},
                              error = function(e) {NA}))

sum(is.na(df_culv$upst_km))
wrn <- warnings()