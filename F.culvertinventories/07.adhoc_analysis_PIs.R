# Ad hoc cost analysis for PI paper
# B. Van Deynze
# May '22

library(tidyverse)
library(here)
library(janitor)
library(readxl)
library(sf)

# Load case area boundaries ----
sf_case <-
  st_read(here("data/WSDOT_-_Fish_Passage_US_v._WA_Case_Area_Boundary-shp/WSDOT_-_Fish_Passage_US_v._WA_Case_Area_Boundary.shp"), quiet = TRUE) %>%
  st_transform(crs = 4269)


# Pull summary stats for PNSHP sample ----
## Load & prep PNSHP sample ----
# Load NLCD key
key_nlcd <-
  read_xlsx(
    here(
      "data/Culverts spatial overlays v 20Jan2021.xlsx"
    ), 
    sheet = 4
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
sf_culv <-
  read_csv(here("output", "culverts_pure_modelling.csv")) %>%
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
  ) %>% mutate(
    latlong = str_sub(geometry, 3, nchar(geometry)-1),
    long = as.numeric(stringr::word(latlong, 1, sep = ", ")),
    lat = as.numeric(stringr::word(latlong, 2, sep = ", ")),
    latlong = NULL,
    geometry = NULL
  ) %>%
  st_as_sf(coords = c("long", "lat")) %>% st_set_crs(4269) %>%
  st_filter(sf_case)

## Summary stats for PNSHP sample ----
# Number of barriers in PNSHP sample in case area
nrow(sf_culv)

# N, mean, med., and s.d. by class
sf_culv %>%
  rowwise() %>%
  mutate(
    here_speed = as.character(here_speed),
    road_class = case_when(
      here_speed %in% c("8", "7") ~ 1,
      here_speed %in% c("4", "5", "6") ~ 2,
      here_speed %in% c("2", "3") ~ 3
    )
  ) %>%
  st_drop_geometry() %>%
  group_by(road_class) %>%
  summarize(
    n = n(),
    mean_cost = mean(cost_per_culvert),
    med_cost = median(cost_per_culvert),
    sd_cost = sd(cost_per_culvert)
  )
# # A tibble: 2 × 5
# road_class     n mean_cost med_cost sd_cost
# <dbl> <int>     <dbl>    <dbl>   <dbl>
#   1          1    46   129305.   86816. 117440.
# 2          2    83   188974.  126333. 177295.

# Overall
sf_culv %>%
  # rowwise() %>%
  mutate(
    here_speed = as.character(here_speed),
    road_class = case_when(
      here_speed %in% c("8", "7") ~ 1,
      here_speed %in% c("4", "5", "6") ~ 2,
      here_speed %in% c("2", "3") ~ 3
    )
  ) %>%
  st_drop_geometry() %>%
  # group_by(road_class) %>%
  summarize(
    n = n(),
    mean_cost = mean(cost_per_culvert),
    med_cost = median(cost_per_culvert),
    sd_cost = sd(cost_per_culvert)
  )

# Pull summary stats for WDFW inventory estimates ----
## Load & prep inventory estimates ----
sf_inv_preds <-
  here("output/inv_preds.csv") %>% read_csv(guess_max = 30000)  %>%
  mutate(geom = gsub(geometry,pattern="(\\))|(\\()|c",replacement = ""))%>%
  tidyr::separate(geom,into=c("lat","lon"),sep=",")%>%
  st_as_sf(.,coords=c("lat","lon"),crs=4326) %>%
  st_transform(crs= 4269) %>%
  st_filter(sf_case)

## Summary stats for WDFW inventory ----
# Number of barriers in case area
nrow(sf_inv_preds)
# [1] 25442

# N, mean, med., and s.d. by class
sf_inv_preds %>%
  rowwise() %>%
  mutate(
    here_speed = as.character(here_speed),
    road_class = case_when(
      here_speed_0 %in% c("8", "7", "0") ~ 1,
      here_speed_0 %in% c("4", "5", "6") ~ 2,
      here_speed_0 %in% c("2", "3") ~ 3
    )
  ) %>%
  st_drop_geometry() %>%
  group_by(road_class) %>%
  summarize(
    n = n(),
    mean_cost = mean(exp(costpred_brt)),
    med_cost = median(exp(costpred_brt)),
    sd_cost = sd(exp(costpred_brt))
  )
# # A tibble: 3 × 5
# road_class     n mean_cost med_cost sd_cost
# <dbl> <int>     <dbl>    <dbl>   <dbl>
#   1          1  2576    45629.   39789.  23572.
# 2          2 20501    64097.   59316.  33038.
# 3          3  2365    61003.   54851.  32135.

# OLS version
sf_inv_preds %>%
  rowwise() %>%
  mutate(
    here_speed = as.character(here_speed),
    road_class = case_when(
      here_speed_0 %in% c("8", "7", "0") ~ 1,
      here_speed_0 %in% c("4", "5", "6") ~ 2,
      here_speed_0 %in% c("2", "3") ~ 3
    )
  ) %>%
  st_drop_geometry() %>%
  drop_na(costpred_ols) %>%
  group_by(road_class) %>%
  summarize(
    n = n(),
    mean_cost = mean(exp(costpred_ols)),
    med_cost = median(exp(costpred_ols)),
    sd_cost = sd(exp(costpred_ols))
  )
