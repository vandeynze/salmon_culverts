# TITLE: Spatial data merge
# AUTHOR: Braeden Van Deynze
# DATE: Aug., 2020
# INPUTS: "culverts_full_mapping.csv"" data for culvert work site data; additional outputs from spatial data module and Blake's ArcGIS pulls
# OUTPUTS: Rectangular culvert work site data with spatial variables attached
# STATUS: Draft

# User note: Designed so that you can easily comment out specific spatial script outputs for partial data pull

rm(list = ls())
# Load libraries
library(sf)
library(tidyverse)
library(here)
library(janitor)
library(readxl)
library(spatialEco)
library(elevatr)
library(data.table)
library(crayon)

# Base Data ----
# Load base work site data
df_culv <- read_csv(here("output/culverts_full_mapping.csv")) %>% 
  st_as_sf(coords = c('longitude', 'latitude'), crs = 4326) %>%
  select(-county)

# Load other outputs

# 01.countycensus.R - CBP jobs data
df_cbp <- read_csv(here("output/spatial/culverts_cbp.csv"))
# 02.culvertsdensitytets.R - Density measures for nearby culverts
# df_density <- read_csv(here("output/spatial/culverts_density.csv"))
# 03.culvertsroadstets.R - OSM streets data
# df_osmroads <- read_csv(here("output/spatial/culverts_osmroads.csv"))
# 04.culvertsstreams.R - NHDPlus V2.1 stream data
df_nhdstreams <- read_csv(here("output/spatial/culverts_nhdstreams.csv")) %>% select(worksite_id, project_year, comid, slope, upst_dist)

# Blake's ArcGIS pulls
df_blake <- read_xlsx(here("data/Culverts spatial overlays v 20Aug2020.xlsx"), sheet = 1) %>% as_tibble() %>% clean_names() %>% mutate(pure_culv = as.logical(pure_culv))

# NHDPlus attributes ----
# From https://www.sciencebase.gov/catalog/item/5669a79ee4b08895842a1d47

# Need to recognize difference between "total" (or *_tot) vs. "diversion" (or
# *_acc) routing

# From Brakebill, Schwarz, and Wieczorek (2019; https://doi.org/10.3133/sir20195127):
# "There are two concepts of upstream drainage area: diversion-apportioned and
# total upstream. Diversion-apportioned upstream area is determined by
# apportioning accumulated drainage area at a node to a downstream flowline,
# according to the flowline’s diversion fraction. This approach is akin to the
# way streamflow would accumulate. In contrast, total upstream area represents
# the total area of all catchments upstream from the given flowline, inclusive
# of the flowline’s catchment area. Owing to diversions, this area concept is
# much more difficult to compute; care is required to avoid the double counting
# of an area where diverted pathways reconnect."

# That is, the diversion routing *weights* by the fraction of flow that
# accumulates from upstream catchement, accouting for diversions. Total routing
# ignores diversions and attributes the upstream area without equal area
# weights.

# Check file structure and unzip files if necessary
# To install data, download the following ZIP archives from the corresponding links and place in the
# /data/NHDPlusSuppData folder
# The following will check for each and direct to URL, and open the install folder, then unzip if needed
open.dir <- function(dir = getwd()){
  if (.Platform['OS.type'] == "windows"){
    shell.exec(dir)
  } else {
    system(paste(Sys.getenv("R_BROWSER"), dir))
  }
}

nhdplus_prompt <-
  function(
    file, # A .zip to check for
    redirect, # URL to open when file is missing
    name = NA_character_,
    path = here("data/NHDPlusSuppData"), # Path where .zip must be placed,
    note = paste0(yellow(file), " does not exist! Please use 'download all' under 'Attached Files' from", redirect, " and place in ", path)
  ){
    require(here)
    require(crayon)
    if(file.exists(file)) {
      message(paste(file, "exists!"))
    } else {
      browseURL(redirect)
      open.dir(path)
      message(red(note))
      if(!is.na(name)) message(red(paste0("Be sure to add the data label! 'AttributesforNH.zip' --> 'AttributesforNH - ", yellow(name), ".zip'")))
    }
  }

# Establish lists to check
supp_files <-
  c(
    here("data", "NHDPlusSuppData", "AttributesforNH - BASIN.zip"),
    here("data", "NHDPlusSuppData", "AttributesforNH - HDEN.zip"),
    here("data", "NHDPlusSuppData", "AttributesforNH - NLCD.zip"),
    here("data", "NHDPlusSuppData", "AttributesforNH - POPDEN00.zip"),
    here("data", "NHDPlusSuppData", "AttributesforNH - POPDEN10.zip"),
    here("data", "NHDPlusSuppData", "AttributesforNH - PPT.zip"),
    here("data", "NHDPlusSuppData", "AttributesforNH - RUN.zip"),
    here("data", "NHDPlusSuppData", "AttributesforNH - TAV.zip"),
    here("data", "NHDPlusSuppData", "BANKFULL_CONUS.zip"),
    here("data", "NHDPlusSuppData", "BFI_CONUS.zip")
  )
  

supp_names <-
  c(
    "BASIN",
    "HDEN",
    "NLCD",
    "POPDEN00",
    "POPDEN10",
    "PPT",
    "RUN",
    "TAV",
    NA_character_,
    NA_character_
  )

supp_urls <-
  c(
    "https://www.sciencebase.gov/catalog/item/57976a0ce4b021cadec97890",
    "https://www.sciencebase.gov/catalog/item/5910de31e4b0e541a03ac983",
    "https://www.sciencebase.gov/catalog/item/5d66b3b6e4b0c4f70cefb11d",
    "https://www.sciencebase.gov/catalog/item/5728f532e4b0b13d3918aa0a",
    "https://www.sciencebase.gov/catalog/item/5728f746e4b0b13d3918aa1e",
    "https://www.sciencebase.gov/catalog/item/57bf5c07e4b0f2f0ceb75b1b",
    "https://www.sciencebase.gov/catalog/item/57bf5e25e4b0f2f0ceb75b77",
    "https://www.sciencebase.gov/catalog/item/5787ea72e4b0d27deb377b6d",
    "https://www.sciencebase.gov/catalog/item/5cf02bdae4b0b51330e22b85",
    "https://www.sciencebase.gov/catalog/item/5669a8e3e4b08895842a1d4f"
  )

# Check if all zips are present
if(sum(supp_files %in% list.files(here("data/NHDPlusSuppData"), full.names = TRUE)) != length(supp_files)) {
  # Loop over files to check each if needed
  mapply(nhdplus_prompt, file = supp_files, redirect = supp_urls, name = supp_names, USE.NAMES = FALSE)
  stop(red("Resolve all conflicts in red to proceed! Re-run step when all files are downloaded."))
} else {
  message(green("All ZIPs are good to go!"))
}

# Unzip all the files
supp_names_unzip <-
  c(
    "BASIN",
    "HDEN",
    "NLCD",
    "POPDEN",
    "POPDEN",
    "PPT",
    "RUN",
    "TAV",
    NA_character_,
    NA_character_
  )

mapply(unzip, zipfile = supp_files, exdir = here("data", "NHDPlusSuppData", replace_na(supp_names_unzip, ".")))

# Unzip relevant years for recursively zipped structures
supp_files_unzip <-
  list(
    "BASIN" = list.files(here("data","NHDPlusSuppData","BASIN"), "*.zip", full.names = TRUE),
    "HDEN" = list.files(here("data","NHDPlusSuppData","HDEN"), "*.zip", full.names = TRUE) %>% str_subset("HDENS(9|0|1)0"),
    "NLCD" = list.files(here("data","NHDPlusSuppData","NLCD"), "*.zip", full.names = TRUE),
    "POPDEN" = list.files(here("data","NHDPlusSuppData","POPDEN"), "*.zip", full.names = TRUE),
    "PPT" = list.files(here("data","NHDPlusSuppData","PPT"), "*.zip", full.names = TRUE)[51:71],
    "RUN" = list.files(here("data","NHDPlusSuppData","RUN"), "*.zip", full.names = TRUE)[51:71],
    "TAV" = list.files(here("data","NHDPlusSuppData","TAV"), "*.zip", full.names = TRUE)[51:71]
  )
mapply(unzip, zipfile = supp_files_unzip %>% unlist, exdir = gsub("(.*\\/).*", "\\1", supp_files_unzip %>% unlist(use.names = FALSE)) %>% str_sub(1, -2))


# ____ Water Balance Estimates ----
# (methods from Wolock & McCabe 2017 https://doi.org/10.1029/2011WR010630)
# Annual Average Precipitation in mm 1945-2015: https://www.sciencebase.gov/catalog/item/57bf5c07e4b0f2f0ceb75b1b
# New vars: ppt_* - annual average precipitation in mm
ppt_filenames <- list.files(here("data","NHDPlusSuppData","PPT"), pattern="*.txt", full.names=TRUE, recursive = TRUE)
df_ppt <- map(ppt_filenames, ~read_csv(., na = "-9999") %>% rename(comid = 1, ppt_cat = 2, ppt_acc = 3, ppt_tot = 4)) %>% bind_rows(.id = "project_year") %>% mutate(project_year = as.numeric(project_year) + 1995)
df_nhdstreams <- df_nhdstreams %>% left_join(df_ppt)
rm(df_ppt, ppt_filenames)

# Annual Average Runoff in mm 1945-2015: https://www.sciencebase.gov/catalog/item/57bf5e25e4b0f2f0ceb75b77
# New vars: run_*
# run_filenames <- list.files(here("data","NHDPlusSuppData","RUN"), pattern="*.txt", full.names=TRUE, recursive = TRUE)
# df_run <- map(run_filenames, ~read_csv(., na = "-9999") %>% rename(comid = 1, run_cat = 2, run_acc = 3, run_tot = 4)) %>% bind_rows(.id = "project_year") %>% mutate(project_year = as.numeric(project_year) + 1995)
# df_nhdstreams <- df_nhdstreams %>% left_join(df_run)
# rm(df_run, run_filenames)

# Annual Average Temperature in C 1945-2015: https://www.sciencebase.gov/catalog/item/5787ea72e4b0d27deb377b6d
# Could proxy for hab quality?
# New vars: temp_*
# temp_filenames <- list.files(here("data","NHDPlusSuppData","TAV"), pattern="*.TXT", full.names=TRUE, recursive = TRUE)
# df_temp <- map(temp_filenames, ~read_csv(., na = "-9999") %>% rename(comid = 1, temp_cat = 2, temp_acc = 3, temp_tot = 4)) %>% bind_rows(.id = "project_year") %>% mutate(project_year = as.numeric(project_year) + 1995)
# df_nhdstreams <- df_nhdstreams %>% left_join(df_temp)
# rm(df_temp, temp_filenames)

# ____ Hydrologic Attributes ----
# Base Flow Index 2003: https://www.sciencebase.gov/catalog/item/5669a8e3e4b08895842a1d4f
# Units are percent. Base flow is the component of streamflow that can be attributed to ground-water discharge into streams.
# Only single 2003 measure is available (interact with time trend to capture changing importance over time?)
# New vars: bfi_*
df_bfi <- read_csv(here("data","NHDPlusSuppData","BFI_CONUS.txt"), na = "-9999") %>% clean_names() %>% select(comid, bfi_cat = cat_bfi, bfi_acc = acc_bfi, bfi_tot = tot_bfi)
df_nhdstreams <- df_nhdstreams %>% left_join(df_bfi)
rm(df_bfi)

# Bankfull depth, width, and cross-sectional area: https://www.sciencebase.gov/catalog/item/5669a8e3e4b08895842a1d4f
# Data created using regional regression estimates from Bieger et al. (2015): http://doi.org/10.1111/jawr.12282
# New vars: bankfull_width; bankfull_depth; bank_full_xsec_area
df_bfull <- read_csv(here("data","NHDPlusSuppData","BANKFULL_CONUS.txt"), na = "-9999") %>% clean_names()
df_nhdstreams <- df_nhdstreams %>% left_join(df_bfull)
rm(df_bfull)

# ____ NLCD Land Cover Attributes ----
# https://www.sciencebase.gov/catalog/item/5d66b3b6e4b0c4f70cefb11d
# Percent land cover of catchment area by cover class, matching to nearest year available
# New vars: nlcd_[class]_*
nlcd_filenames <- list.files(here("data","NHDPlusSuppData","NLCD"), pattern="*.TXT$", full.names=TRUE, recursive = TRUE)
nlcd_years <- rep(c(2001, 2004, 2006, 2008, 2011, 2013, 2016), each = 3)
nlcd_method <- rep(c("acc", "cat", "tot"), length.out = length(nlcd_years))
nlcd_vars <- c(
  "comid",
  "nlcd_openwater",
  "nlcd_icesnow",
  "nlcd_devopen",
  "nlcd_devlow",
  "nlcd_devmed",
  "nlcd_devhigh",
  "nlcd_barren",
  "nlcd_fordec",
  "nlcd_forcon",
  "nlcd_formix",
  "nlcd_shrub",
  "nlcd_grass",
  "nlcd_pasture",
  "nlcd_crop",
  "nlcd_wetwood",
  "nlcd_wetherb",
  "nlcd_nodat",
  "project_year"
)
df_nlcd <- map(nlcd_filenames, ~read_csv(.) %>% clean_names(.))
names(df_nlcd) <- paste(nlcd_years, nlcd_method, sep = "_")
df_nlcd <-
  map2(
    .x = df_nlcd,
    .y = nlcd_years,
    ~ mutate(
      .x, 
      project_year = .y
    )
  )
df_nlcd <-
  map(
    .x = df_nlcd,
    ~ setNames(.x, nlcd_vars)
  )
df_nlcd <-
  map2(
    .x = df_nlcd,
    .y = nlcd_method,
    ~ setNames(.x, c("comid", paste(names(.x[2:18]), .y, sep = "_"), "project_year"))
  )
nhd_comids <- distinct(df_nhdstreams, comid) %>% pull(comid)
df_nlcd <- map(df_nlcd, ~filter(.x, comid %in% nhd_comids))
df_nlcd <- map(unique(nlcd_years), ~df_nlcd[names(df_nlcd) %>% str_detect(as.character(.x))] %>% reduce(full_join)) %>% bind_rows()

# Class	              Value	Classification                Description
# Water	              11	  Open Water	                  areas of open water, generally with less than 25% cover of vegetation or soil.
# Water	              12	  Perennial Ice/Snow	          areas characterized by a perennial cover of ice and/or snow, generally greater than 25% of total cover.
# Developed	          21	  Developed, Open Space	        areas with a mixture of some constructed materials, but mostly vegetation in the form of lawn grasses. Impervious surfaces account for less than 20% of total cover. These areas most commonly include large-lot single-family housing units, parks, golf courses, and vegetation planted in developed settings for recreation, erosion control, or aesthetic purposes.
# Developed           22	  Developed, Low Intensity	    areas with a mixture of constructed materials and vegetation. Impervious surfaces account for 20% to 49% percent of total cover. These areas most commonly include single-family housing units.
# Developed           23	  Developed, Medium Intensity	  areas with a mixture of constructed materials and vegetation. Impervious surfaces account for 50% to 79% of the total cover. These areas most commonly include single-family housing units.
# Developed           24  	Developed High Intensity	    highly developed areas where people reside or work in high numbers. Examples include apartment complexes, row houses and commercial/industrial. Impervious surfaces account for 80% to 100% of the total cover.
# Barren	            31	  Barren Land (Rock/Sand/Clay)	areas of bedrock, desert pavement, scarps, talus, slides, volcanic material, glacial debris, sand dunes, strip mines, gravel pits and other accumulations of earthen material. Generally, vegetation accounts for less than 15% of total cover.
# Forest              41	  Deciduous Forest	            areas dominated by trees generally greater than 5 meters tall, and greater than 20% of total vegetation cover. More than 75% of the tree species shed foliage simultaneously in response to seasonal change.
# Forest	            42	  Evergreen Forest	            areas dominated by trees generally greater than 5 meters tall, and greater than 20% of total vegetation cover. More than 75% of the tree species maintain their leaves all year. Canopy is never without green foliage.
# Forest	            43	  Mixed Forest	                areas dominated by trees generally greater than 5 meters tall, and greater than 20% of total vegetation cover. Neither deciduous nor evergreen species are greater than 75% of total tree cover.
# Shrubland	          52	  Shrub/Scrub	                  areas dominated by shrubs; less than 5 meters tall with shrub canopy typically greater than 20% of total vegetation. This class includes true shrubs, young trees in an early successional stage or trees stunted from environmental conditions.
# Herbaceous	        71	  Grassland/Herbaceous	        areas dominated by gramanoid or herbaceous vegetation, generally greater than 80% of total vegetation. These areas are not subject to intensive management such as tilling, but can be utilized for grazing.
# Planted-Cultivated	81	  Pasture/Hay                   areas of grasses, legumes, or grass	legume mixtures planted for livestock grazing or the production of seed or hay crops, typically on a perennial cycle. Pasture/hay vegetation accounts for greater than 20% of total vegetation.
# Planted-Cultivated	82	  Cultivated Crops	            areas used for the production of annual crops, such as corn, soybeans, vegetables, tobacco, and cotton, and also perennial woody crops such as orchards and vineyards. Crop vegetation accounts for greater than 20% of total vegetation. This class also includes all land being actively tilled.
# Wetlands	          90	  Woody Wetlands	              areas where forest or shrubland vegetation accounts for greater than 20% of vegetative cover and the soil or substrate is periodically saturated with or covered with water.
# Wetlands	          95	  Emergent Herbaceous Wetlands	areas where perennial herbaceous vegetation accounts for greater than 80% of vegetative cover and the soil or substrate is periodically saturated with or covered with water.

# Use data.table rolling joins to link to nearest NLCD year
df_nhdstreams <- as.data.table(df_nhdstreams, key = c("worksite_id", "comid", "project_year"))
df_nlcd <- as.data.table(df_nlcd, key = c("comid", "project_year"))
df_nhdstreams <- df_nlcd[df_nhdstreams, roll = "nearest", on = .(comid, project_year)]

rm(df_nlcd, nlcd_vars, nlcd_filenames, nlcd_method, nlcd_years)

# ____ Population/Infrastructure Attributes ----
# Housing density in units per square km 1940-2010 in ten year Census intervals: https://www.sciencebase.gov/catalog/item/5910de31e4b0e541a03ac983
# New vars: hdens_*
hden_filenames <- list.files(here("data","NHDPlusSuppData","HDEN"), pattern = "*.txt$", full.names = TRUE, recursive = TRUE)
df_hden <- map(hden_filenames, ~read_csv(.) %>% rename(comid = 1, hdens_cat = 2, hdens_cat_nodata = 3, hdens_acc = 4, hdens_acc_notdata = 5, hdens_tot = 6, hdens_tot_nodata = 7)) %>% bind_rows(.id = "project_year") %>% mutate(project_year = (as.numeric(project_year) - 1) * 10 + 2000) %>% as.data.table()
df_nhdstreams <- df_hden[df_nhdstreams, roll = "nearest", on = .(comid, project_year)]
rm(hden_filenames, df_hden)

# Population density in persons per square km based on 2000/2010 Census block data: https://www.sciencebase.gov/catalog/item/5728f532e4b0b13d3918aa0a / https://www.sciencebase.gov/catalog/item/5728f746e4b0b13d3918aa1e
# New vars: popden_*
popden_filenames <- list.files(here("data","NHDPlusSuppData","POPDEN"), pattern = "*.txt$", full.names = TRUE, recursive = TRUE)
df_popden <- map(popden_filenames, ~read_csv(.) %>% rename(comid = 1, popdens_cat = 2, popdens_cat_nodata = 3, popdens_acc = 4, popdens_acc_notdata = 5, popdens_tot = 6, popdens_tot_nodata = 7)) %>% bind_rows(.id = "project_year") %>% mutate(project_year = (as.numeric(project_year) - 1) * 10 + 2000) %>% as.data.table()
df_nhdstreams <- df_popden[df_nhdstreams, roll = "nearest", on = .(comid, project_year)]
rm(popden_filenames, df_popden)

# ____ Basin Characteristics ----
# Includes basin area, stream slope, road density, stream density 
# New vars: *_basin_area, *_stream_slope, *_basin_slope, *_elev_mean, *_elev_min, *_elev_max, *_stream_length, *_rdx (road density), *_strm_dens
basin_filenames <- c(list.files(here("data","NHDPlusSuppData","BASIN"), pattern = c("*.txt$"), full.names = TRUE, recursive = TRUE), list.files(here("data","NHDPlusSuppData","BASIN"), pattern = c("*.TXT$"), full.names = TRUE, recursive = TRUE))
df_basin <- map(basin_filenames, ~read_csv(.) %>% clean_names()) %>% setNames(basin_filenames) %>% reduce(left_join, by = "comid")
df_nhdstreams <- df_nhdstreams %>% left_join(df_basin, by = "comid")
rm(basin_filenames, df_basin)

# Ruggedness ----
# Data from USGS National Elevation Dataset accessed via elevatr
# ~ 30min to download the full set for the region

sf_elev <-
  df_culv %>%
  st_crop(
   y = c(xmin = -120, ymin = 42, xmax = -119, ymax = 48)
  ) %>%
  get_elev_raster(z = 11) %>% # Guide to zoom levels: https://github.com/tilezen/joerd/blob/master/docs/data-sources.md#what-is-the-ground-resolution
  vrm(9) # Download successful but DRM reprojection step still did not complete after 48hrs

raster::plot(sf_elev)


# FIPS Codes ----
# Add fips codes for culverts
# Load county data to ID missing counties
temp <- tempfile()
tempdir <- tempdir()
download.file("https://www2.census.gov/geo/tiger/TIGER2018/COUNTY/tl_2018_us_county.zip", temp)
unzip(temp, exdir = tempdir)
sf_counties <- read_sf(paste0(tempdir, "\\tl_2018_us_county.shp")) %>%
  st_transform(crs = 4326) %>%
  clean_names() %>%
  select(fips = geoid, county = name, state_fips = statefp)
unlink(temp)
rm(temp, tempdir)

# Spatial join
df_culv <- st_join(df_culv, sf_counties)

# Final Merge ----
# Merge up
df_culv_out <-
  df_culv %>%
  mutate(fips = as.double(fips)) %>%
  # CBP jobs data
  left_join(df_cbp, by = c("project_year" = "year", "fips" = "fips")) %>%
  # Density measures for nearby culverts
  # left_join(df_density) %>%
  # OSM streets data
  # left_join(df_osmroads) %>%
  # NHD+ stream data
  left_join(df_nhdstreams, by = "worksite_id") %>%
  # Blake's ArcGIS data
  left_join(df_blake %>% select(worksite_id = worksite_i, nlcd_2001:here_publi), by = "worksite_id")

# Identify nearest NLCD to project date
df_culv_out <-
  df_culv_out %>%
  rowwise() %>%
  mutate(
    project_year = project_year.x,
    project_year.y = NULL,
    project_year.x = NULL,
    nlcd_current = case_when(
      project_year <= 2001 ~ nlcd_2001,
      2001 < project_year & project_year <= 2004 ~ nlcd_2004,
      2004 < project_year & project_year <= 2006 ~ nlcd_2006,
      2006 < project_year & project_year <= 2008 ~ nlcd_2008,
      2008 < project_year & project_year <= 2011 ~ nlcd_2011,
      2011 < project_year & project_year <= 2013 ~ nlcd_2013,
      2013 < project_year & project_year <= 2016 ~ nlcd_2016
    ),
    nhd_dist_m = case_when(nhd_dist_m < 0 ~ NA_real_, TRUE ~ nhd_dist_m),
    snet_distm = case_when(snet_distm < 0 ~ NA_real_, TRUE ~ snet_distm),
    slope_deg = case_when(slope_deg < 0 ~ NA_real_, TRUE ~ slope_deg)
  )

# Write out
df_culv_out %>%
  mutate(
    latitude = st_coordinates(geometry)[,2],
    longitude = st_coordinates(geometry)[,1],
    geometry = NULL
  ) %>%
  # st_drop_geometry() %>%
  write_csv(here("output/culverts_full_spatial.csv"))

# Select only "pure" culverts and attach data with project culvert counts from B.culvertsexplore modulee
df_culv_pure <- read_csv(here("output","culverts_wrk_working.csv")) %>% select(worksite_id, n_worksites, n_culverts, starts_with("action_fishpass"), cost_per_culvert, dist_max, dist_mean)
df_culv_out_pure <-
  df_culv_out %>%
  right_join(df_culv_pure) %>%
  mutate(
    latitude = st_coordinates(geometry)[,2],
    longitude = st_coordinates(geometry)[,1],
    geometry = NULL
  )

df_culv_out_pure %>%
  write_csv(here("output/culverts_pure_spatial.csv"))

