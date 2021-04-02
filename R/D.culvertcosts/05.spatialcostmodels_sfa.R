# Stochastic frontier cost models

library(here)
library(tidyverse)
library(janitor)
library(broom)
library(frontier)
library(readxl)
library(lmtest)
library(sandwich)
library(forcats)

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

# Load data
df_culv <-
  read_csv(here("output", "culverts_full_modelling.csv")) %>%
  mutate(
    # project_year = ordered(project_year),
    project_source = relevel(factor(project_source), ref = "OWRI"),
    basin = relevel(factor(basin), ref = "SOUTHERN OREGON COASTAL"),
    fips = factor(fips),
    state_fips = factor(state_fips),
    here_class = as.character(here_class),
    here_speed = relevel(factor(here_speed), ref = 6),
    # tot_dist = I(n_worksites * dist_mean)
  ) %>%
  left_join(
    key_nlcd, 
    by = c("nlcd_current" = "value")
  ) %>%
  filter(
    !(nlcd_current_class %in% c("Barren", "Water")),
    # tot_dist < 10000
  ) %>%
  mutate(
    nlcd_current_class = relevel(factor(nlcd_current_class, ordered = FALSE), ref = "Forest")
  ) %>%
  rowwise() %>%
  mutate(
    across(
      starts_with("emp_"),
      ~ . / sum(c_across(starts_with("emp")), na.rm = TRUE),
      .names = "{col}_prop"
    )
  )

# Load full pnshp for other action counts, n_worksites, etc.
df_pnshp <-
  read_csv(
    here("output/PNSHP_prj_working.csv")
  )
df_wrk <-
  read_csv(
    here("output/PNSHP_full_working.csv")
  ) %>%
  distinct(worksite_id, latitude, longitude)

df_culv <-
  df_culv %>%
  left_join(
    df_pnshp %>% select(
      project_id,
      project_year,
      n_worksites,
      starts_with("action_")
    )
  ) %>%
  left_join(df_wrk) %>%
  # Summarize major non-culvert actions
  rowwise() %>%
  mutate(
    action_roadwork = sum(action_sedireduce_roaddrain_count, action_sedireduce_roadobli_count),
    action_habitat = sum(action_riparian_plant_count, action_instream_wooddebris_count, action_instream_logjam_count),
    action_culvrem = action_fishpass_culvrem_count,
    action_culvinst = action_fishpass_culvimp_count,
    across(
      c(action_roadwork:action_culvinst),
      ~ factor(I(. > 0), ordered = FALSE) %>% fct_recode(., "Y" = "TRUE", "N" = "FALSE") %>% fct_relevel(., "N")
    ),
    here_speed = as.character(here_speed) %>% fct_collapse(., "< 30 mph" = c("8", "7", "6"), "30 - 55 mph" = c("5", "4"), "> 55 mph" = c("3", "2", "1", "0")) %>% fct_relevel(., "< 30 mph") 
  )

# Add total distance between worksites
df_culv <-
  df_culv %>%
  # slice(1:10) %>%
  mutate(
    tot_dist = map_dbl(
      project_id,
      function(x){
        distmat <- geosphere::distm(
          df_culv %>% filter(project_id == x) %>% select(longitude, latitude)
        )
        sum(distmat[lower.tri(distmat)])
      }
    )
  )

# Final cleaning and filtering
df_culv <-
  df_culv %>%
  filter(
    # n_worksites < 15,
    # tot_dist < 350000,
    here_speed != "0"
  ) %>% 
  mutate(
    here_speed = fct_drop(here_speed),
    across(
      c(action_habitat:action_culvinst),
      ~ fct_relevel(., "N")
    )
  )

df_culv_back <- df_culv

df_culv <- df_culv %>% filter(log(adj_cost) > 9)
# Base model with OLS
(mod_full <- 
  lm(
    log(adj_cost/n_worksites) ~
      # log(I(adj_cost / n_worksites)) ~
      # Scale/scope of project controls: number of culverts, distance between worksites, type of culvert work
      n_worksites * tot_dist + # I(n_worksites^2) + # factor(I(n_worksites == 1)) +
      # action_fishpass_culvrem_count + action_fishpass_culvimp_count + action_fishpass_culvinst_count +
      # action_habitat + action_roadwork + # action_culvinst + action_culvrem +
      # Stream features at worksite: slope, bankfull width
      slope * bankfull_width + 
      # Road features at worksite: paved, road class
      here_speed +
      # Physical features of worksite: terrain slope, land cover
      # slope_deg + factor(nlcd_current_class) +
      cat_basin_slope + cat_elev_mean + nlcd_current_class +
      # Population features: housing density, jobs in construction, jobs in ag/forestry
      hdens_cat + emp_const_prop + emp_agforest_prop + ua_dist + # factor(publand) +
      # Fixed effects
      basin + factor(project_year) + project_source,
    df_culv %>% filter(n_worksites < 15, tot_dist < 150000)
  # )) %>% coeftest(vcov. = vcovHC(., "HC3"))
  )) %>% coeftest(., vcov. = vcovCL(., ~ project_id))
  # )) %>% summary()

df_culv <- df_culv 

# SFA model
# Kinda works, sometimes. Results roughly consistent with what we'd expect. I'd like to explore this further.
(
  mod_sfa <-
    # lm(
    sfa(
      log(adj_cost/n_worksites) ~
        # log(I(adj_cost / n_worksites)) ~
        # Scale/scope of project controls: number of culverts, distance between worksites, type of culvert work
        n_worksites * tot_dist + # I(n_worksites^2) + # factor(I(n_worksites == 1)) +
        # action_fishpass_culvrem_count + action_fishpass_culvimp_count + action_fishpass_culvinst_count +
        #action_habitat + action_roadwork +# action_culvinst + action_culvrem +
        # Stream features at worksite: slope, bankfull width
        slope * bankfull_width + 
        # Road features at worksite: paved, road class
        here_speed +
        # Physical features of worksite: terrain slope, land cover
        # slope_deg + factor(nlcd_current_class) +
        cat_basin_slope + cat_elev_mean + nlcd_current_class +
        # Population features: housing density, jobs in construction, jobs in ag/forestry
        hdens_cat + emp_const_prop + emp_agforest_prop + ua_dist + # factor(publand) +
        # Fixed effects
        basin + factor(project_year) + project_source, # |
        # inefficency terms
        # n_worksites * tot_dist + # I(n_worksites^2) + # factor(I(n_worksites == 1)) +
        # action_fishpass_culvrem_count + action_fishpass_culvimp_count + action_fishpass_culvinst_count +
        #action_habitat + action_roadwork +# action_culvinst + action_culvrem +
        # Stream features at worksite: slope, bankfull width
        # slope * bankfull_width +
        # Road features at worksite: paved, road class
        # here_speed +
        # Physical features of worksite: terrain slope, land cover
        # slope_deg + factor(nlcd_current_class) +
        # cat_basin_slope + cat_elev_mean + nlcd_current_class +
        # Population features: housing density, jobs in construction, jobs in ag/forestry
        # hdens_cat + emp_const_prop + emp_agforest_prop + ua_dist +  # factor(publand) +
        # Fixed effects
        # basin + factor(project_year) + project_source,
      data = df_culv %>% filter(pure_culv == TRUE),
      ineffDecrease = FALSE
    )
) %>% summary()
# ) %>% coeftest(vcov. = vcovHC(.))
