# Purpose: Calculate cost predictions for inventory culverts using models from D.culvertcosts module

# Load packages ----
library(sf)
library(tidyverse)
library(janitor)
library(here)
library(scales)
library(broom)
library(readxl)
library(sandwich)
library(lmtest)
library(searchable)
library(vip)
library(pdp)
library(rpart)
library(rpart.plot)
library(randomForest)
library(gbm)
library(modelr)
library(nhdplusTools)
library(rgdal)

# Load data ----
# Load NLCD key
key_nlcd <-
  read_xlsx(
    here(
      "data/Culverts spatial overlays v 06Aug2020.xlsx"
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

# Load HUC6 key
key_huc6 <-
  read_csv(here("data/huc6_key.csv"))

# PNSHP data models are fit on
df_culv <-
  read_csv(here("output", "culverts_pure_modelling.csv")) %>%
  # Relevel factors following D. module
  mutate(
    project_year = ordered(project_year),
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

# Inventory coordinates
df_wdfw <-
  read_csv(here("output/allculvs/culvinventory_wdfw.csv"))
df_odfw <-
  read_csv(here("output/allculvs/culvinventory_odfw.csv"))

# Merged inventory with explanatory variables, prepped for mapping
sf_inv <- 
  read_csv(here("output/inv_draft22042021.csv"), guess_max = 30000) %>%
  select(-geometry) %>%
  left_join(df_wdfw %>% select(site_record_id, site_longitude, site_latitude), by = c("site_recor" = "site_record_id")) %>%
  left_join(df_odfw %>% select(fpb_ftr_id, fpb_long, fpb_lat), by = "fpb_ftr_id") %>%
  mutate(
    long =
      case_when(
        is.na(site_longitude) ~ fpb_long,
        is.na(fpb_long) ~ site_longitude
      ),
    lat =
      case_when(
        is.na(site_latitude) ~ fpb_lat,
        is.na(fpb_lat) ~ site_latitude
      )
  ) %>%
  select(
    -c(fpb_long, fpb_lat, site_longitude, site_latitude)
  ) %>%
  # Create modified road variables classifying bad matches following C. module
  rowwise() %>%
  mutate(
    # Class bad matches
    here_class_0 = here_class,
    here_class = case_when(
      here_distm > 150 ~ "5",
      TRUE ~ as.character(here_class)
    ),
    here_class_badmatch = case_when(
      here_distm > 150 ~ "6",
      TRUE ~ as.character(here_class)
    ),
    here_class_namatch = case_when(
      here_distm > 150 ~ NA_character_,
      TRUE ~ as.character(here_class)
    ),
    # Speed bad matches
    here_speed_0 = here_speed,
    here_speed = case_when(
      here_distm > 150 ~ "8",
      TRUE ~ as.character(here_speed)
    ),
    here_speed_badmatch = case_when(
      here_distm > 150 ~ "9",
      TRUE ~ as.character(here_speed)
    ),
    # Paved bad matches
    here_paved_0 = here_paved,
    here_paved = case_when(
      here_distm > 150 ~ "N",
      TRUE ~ as.character(here_paved)
    )
  ) %>%
  # Create aggregate propery ownership variables following C. module
  mutate(
    # pvall_5km_buff = sum(c_across(starts_with("pv") & ends_with("5km_buff"))),
    # stall_5km_buff = sum(c_across(starts_with("st") & ends_with("5km_buff"))),
    # fedother_5km_buff = sum(c_across(starts_with(c("bpa", "coe", "fws", "bia", "gsa", "nps")) & ends_with("5km_buff"))),
    # pvall_2km_buff = sum(c_across(starts_with("pv") & ends_with("2km_buff"))),
    # stall_2km_buff = sum(c_across(starts_with("st") & ends_with("2km_buff"))),
    # fedother_2km_buff = sum(c_across(starts_with(c("bpa", "coe", "fws", "bia", "gsa", "nps")) & ends_with("2km_buff"))),
    pvall_1km_buff = sum(c_across(starts_with("pv") & ends_with("1km_buff"))),
    stall_1km_buff = sum(c_across(starts_with("st") & ends_with("1km_buff"))),
    fedother_1km_buff = sum(c_across(starts_with(c("bpa", "coe", "fws", "gsa", "bia", "nps")) & ends_with("1km_buff")))
  ) %>%
  ungroup() %>%
  # Add and normalize variables needed for prediction
  mutate(
    # Drop NA stream slope readings
    slope = case_when(slope < 0 ~ NA_real_, TRUE ~ slope),
    # Normalize to single worksite, culvert improvement projects
    n_worksites = 1,
    dist_mean = 0,
    dist_max = 0,
    action_fishpass_culvrem_prj = 0,
    action_fishpass_culvinst_prj = 0,
    # Set project year as ordered factor (only one value, 2015, though)
    project_year = ordered(project_year),
    # Set project source to base level used in prediction (OWRI)
    project_source = "OWRI",
    project_source = relevel(factor(project_source), ref = "OWRI"),
    # Set NLCD current value to most recent reading
    nlcd_current = nlcd_2016,
    # Identify basin
    huc6 = trunc(huc12/1e6),
    # Set FIPS values as factors
    fips = factor(fips),
    state_fips = factor(state_fips),
    # Set HERE road data as factors
    here_class = as.character(here_class),
    here_speed = relevel(factor(here_speed), ref = "6"),
    # Construct total distance variable (should be zero for all observations)
    tot_dist = I(n_worksites * dist_mean)
  ) %>%
  # Read NLCD values to labels
  left_join(
    key_nlcd, 
    by = c("nlcd_current" = "value")
  ) %>%
  # Read HUC6 values to labels
  left_join(
    key_huc6
  ) %>%
  # Set factors
  mutate(
    nlcd_current_class = relevel(factor(nlcd_current_class), ref = "Forest"),
    basin = relevel(factor(basin), ref = "SOUTHERN OREGON COASTAL")
  ) %>%
  mutate(
    across(where(is.character), factor)
  )  %>%
  mutate(
    across(c(here_speed:here_publi, here_class_0:here_paved_0), factor),
  ) %>%
  # Prepare geometry for mapping
  st_as_sf(., coords = c('long', 'lat'), crs = 4326) %>% 
  st_transform(., crs = 4269)


# Load fits ----
# OLS
mod_ols <- read_rds(here("output/costfits/ols_full.rds"))

# BRT
mod_brt <- read_rds(here("output/costfits/boostedregression.rds"))

# RF
mod_rf <- read_rds(here("output/costfits/randomforest.rds"))

# Prepare normalized data for predictions ----




  
# Calculate predicted costs ----
add_columns <- function(df, columns){
  new <- rep(NA_character_, length(columns))
  names(new) <- columns
  mutate(df, !!!new)
}


sf_inv_preds <-
  sf_inv %>%
  # Normalize basins, roads, and land cover for missing levels in training
  ungroup() %>%
  mutate(
    # TODO Identify observations/basins where fixed effects are standardized to Southern Oregon Coastal
    basin = case_when(!(basin %in% unique(df_culv$basin)) ~ "SOUTHERN OREGON COASTAL", TRUE ~ as.character(basin)),
    basin = relevel(factor(basin), ref = "SOUTHERN OREGON COASTAL"),
    # TODO Identify observations where roads are reclassified
    here_speed = 
      case_when(
        here_speed == "2" ~ "3",
        here_speed != "0" ~ as.character(here_speed)
      ),
    here_speed = relevel(factor(here_speed), ref = "6"),
    # TODO Identify observations where land cover is reclassified
    nlcd_current_class = 
      case_when(
        nlcd_current_class %in% c("Barren", "Water") ~ "Forest",
        TRUE ~ as.character(nlcd_current_class)
      ),
    nlcd_current_class = relevel(factor(nlcd_current_class), ref = "Forest")
  ) %>%
  # Add missing columns for BRT predictions
  add_columns(
    names(df_culv)[!(names(df_culv) %in% names(sf_inv))]
  ) %>%
  rowwise() %>%
  add_predictions(mod_ols, var = "costpred_ols")  %>%
  add_predictions(mod_brt, var = "costpred_brt")
summary(sf_inv_preds$costpred_ols)
summary(sf_inv_preds$costpred_brt)

# Save it out ----
write_csv(sf_inv_preds, here("output/inv_preds.csv"))
