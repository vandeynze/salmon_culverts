# TITLE: Isolate all pure culvert work sites and those with other roadwork alongside culverts in PNSHP
# AUTHOR: Braeden Van Deynze
# DATE: June, 2020
# INPUTS: "PNSHP_full_working.csv" for full PNSHP data
# OUTPUTS: "culverts_full_mapping.csv" for culverts for mapping and future modelling

rm(list = ls())

library(tidyverse)
library(janitor)
library(here)


# Load data ====
df_pnshp <- read_csv(here("/output/PNSHP_full_working.csv"))
df_prj <- read_csv(here("/output/PNSHP_prj_working.csv"))

# Investigate basics of raw data ====
# Select only culvert-related projects
df_culv <-
  df_pnshp %>%
  filter(
    action_fishpass_culvimp == 1 | action_fishpass_culvinst == 1 | action_fishpass_culvrem == 1
  )
df_culv_prj <-
  df_prj %>%
  filter(
    action_fishpass_culvimp == 1 | action_fishpass_culvinst == 1 | action_fishpass_culvrem == 1
  )

# This version includes mixed projects
df_culv_all <-
  df_pnshp %>%
  semi_join(df_culv_prj, by = "project_id")

# This is a work site version
df_culv_wrk_all <-
  df_culv_all %>%
  group_by(project_id, worksite_id) %>%
  mutate_at(
    vars(starts_with("action_") & !ends_with(c("_prj", "_count"))),
    list(
      ~as.numeric(I(sum(.) > 0)),
      ~sum(.))
  ) %>%
  rename_at(
    vars(ends_with("_count")),
    ~paste0(.,"_prj")
  ) %>%
  select(
    -(starts_with("action_") & !ends_with(c("_sum", "_as.numeric", "_prj")))
  ) %>%
  rename_at(
    vars(starts_with("action_") & ends_with("_as.numeric")),
    ~str_replace(., "_as.numeric", "_wrk")
  ) %>%
  rename_at(
    vars(starts_with("action_") & ends_with("_sum")),
    ~str_replace(., "_sum", "_count_wrk")
  ) %>%
  # select(-action) %>%
  distinct(project_id, worksite_id, .keep_all = TRUE)

df_culv_prj_pure <-
  df_culv_prj %>%
  filter_at(
    vars(starts_with("action_") & !contains("culv")),
    all_vars(. == 0)
  )
df_culv_pure <-
  df_culv_all %>%
  semi_join(
    df_culv_prj_pure,
    by = "project_id"
  )

ls_culv_pure_wrk <-
  df_culv_pure %>%
  distinct(worksite_id) %>%
  pull(worksite_id)

ls_culv_pure_prj <-
  df_culv_pure %>%
  distinct(project_id) %>%
  pull(project_id)

df_culv_wrk_all <-
  df_culv_wrk_all %>%
  mutate(
    pure_culv = I(worksite_id %in% ls_culv_pure_wrk)
  ) 
  
  
  
# For export
df_culv_export <-
  df_culv_wrk_all %>%
  select(
    project_id,
    worksite_id,
    project_year,
    county,
    state,
    latitude,
    longitude,
    subwatershed,
    watershed,
    subbasin,
    basin,
    project_source,
    adj_cost,
    pure_culv
  ) %>%
  drop_na(adj_cost) %>%
  filter(project_year %in% c(1996:2015))
  
write_csv(df_culv_export, here("/output/culverts_full_mapping.csv"))

# This file contains both pure and mixed culvert work sites
# In the future, this should feed into 02.culvertsprep.R, which currently builds its own list from the PNSHP core that excludes mixed projects