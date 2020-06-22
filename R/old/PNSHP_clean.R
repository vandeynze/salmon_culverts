# Purpose: Clean PNSHP data for general purpose use
# Output: Clean files at project, worksite, and action levels

library(tidyverse)
library(knitr)
library(janitor)
library(quantmod)
library(fastDummies)

rm(list = ls())

# Import data ====
# Load worksite data
setwd("C:/Users/braeden.vandeynze/Documents/Salmon Cost Project/Analysis/data") # For Braeden; comment out for other users
df_pnshp <-
  read_csv(
    "PNSHP_worksite-subtype_20200210.csv",
    # "PNSHP_worksite-subtype_20200113.csv", # Outdated; missing values for most subtypes; remove once 20200210 version works
    guess_max = 200000 # Neccessary to avoid type identification issues resulting from missing values
  ) %>%
  select(
    project_id = ProjectPK,
    worksite_id = WorksitePK,
    Project_Year,
    Completed_Year,
    Latitude,
    Longitude,
    County,
    State,
    Subwatershed,
    Watershed,
    Subbasin,
    Basin,
    Project_Source,
    # action_cat = Mapcat,
    action = SubTypeName,
    Project_Cost,
    Metric,
    NumericValue,
    Units
  ) %>%
  clean_names()
  
# N = 164,869 (worksite x subtype x metric level)

# Clean data ====
df_pnshp <-
  df_pnshp %>%
  # Convert all character columns to uppercase
  mutate_if(
    is.character,
    str_to_upper 
  ) %>%
  # Counts of distinct worksites and action types at project level, then action types at worksite level
  group_by(project_id, worksite_id) %>%
  mutate(
    n_action_wrk = n_distinct(action)
  ) %>%
  group_by(project_id) %>%
  mutate(
    n_worksites = n_distinct(worksite_id),
    n_action_prj = n_distinct(action)
  ) %>%
  ungroup() %>%
  # Clean years
  mutate(
    completed_year = case_when(
      is.na(completed_year) ~ project_year
      TRUE ~ completed_year
    )
  ) %>%
  # Clean state names
  mutate(
    state_full = case_when(
      state == "IDAHO" ~ "ID",
      state == "MONTANA" ~ "MT",
      state == "OREGON" ~ "OR",
      state == "WASHINGTON" ~ "WA",
      state == "<NULL>" ~ NA_character_,
      TRUE ~ state
    ),
    state = case_when(
      state_full == "ID,WY" ~ "ID",
      state_full == "ID,OR" ~ "ID",
      state_full == "CN,WA" ~ "WA",
      state_full == "CA,OR" ~ "OR",
      state_full == "ID,OR,WA" ~ "ID",
      state_full == "NV,OR" ~ "OR",
      state_full == "OR,WA" ~ "OR",
      state_full == "ID,WA" ~ "ID",
      TRUE ~ state_full
    )
    # Will need to check for primary state, multistate projects later
    # But just picking first state from main states as check (states presented alphedbetically so this is not a good fix)
    # Likely doesn't matter because coordinates and funding org will be more important in the long-run and can be used to recover geographic units
  ) %>%
  # Clean metric names
  mutate(
    metric = case_when(
      str_detect(metric, "AREA") ~ "area", # AREA TREATED always in acres
      str_detect(metric, c("LENGTH|MILES")) ~ "length", # LENGTH TREATED always in miles
      str_detect(metric, c("NUMBER|COUNT|SPECIES")) ~ "count", # Impercise, but unimportant in first stages as other metrics on non-priority actions
      is.na(metric) ~ NA_character_,
      TRUE ~ "DROP" # Allows distinguishing between true NA and irrelevant/redundent metrics
    )
  ) %>%
  filter(metric != "DROP" | is.na(metric)) %>% # Drops redundent metrics but retains true NAs; n = 155,238
  select(-units) %>% # Units redundent, see above
  distinct() %>%
  # Clean action names for priority actions (roughly, those w/ more than 1,000 observations), drop remaining
  mutate(
    action_full = action,
    action = recode(
      str_to_lower(action),
      .default = NA_character_, # Comment out to retain original values with no replacement
      "instream - large woody debris" = "instr_wooddeb", 
      "instream - wood structure/ log jam" = "instr_woodstr",
      "instream - boulders" = "instr_bould",
      "instream - deflectors/ barbs" = "instr_barbs",
      "instream - rootwads" = "instr_root",
      "instream - rock weirs" = "instr_weirrock",
      "instream - log weirs" = "instr_weirlog",
      "instream - off channel habitat" = "instr_offchnl",
      "instream - streambank stabilization" = "instr_bank",
      "instream - channel reconfiguration" = "instr_chan", 
      "instream - channel connectivity" = "instr_chan",
      "instream - spawning gravel placement" = "instr_spwn",
      "riparian - weed control" = "ripar_weed",
      "riparian - planting" = "ripar_plant",
      "riparian - fencing" = "ripar_fence",
      "riparian - livestock exclusion" = "ripar_lvstck",
      "riparian - forestry practices/ stand management" = "ripar_vegmgmt",
      "upland vegetation - vegetation/ stand management" = "upveg_vegmgmt",
      "upland vegetation - planting" = "upveg_plant",
      "upland vegetation - slope stabilization" = "upveg_slope",
      "upland vegetation - invasive plant control" = "upveg_invctrl",
      "wetland - wetland improvement/ enhancement" = "wetlnd_imp",
      "wetland - planting" = "wetlnd_imp",
      "wetland - wetland creation" = "wetlnd_imp",
      "wetland - wetland restoration" = "wetlnd_imp",
      "sediment reduction - road drainage system improvements" = "sedred_drain",
      "sediment reduction - road obliteration" = "sedred_roadob",
      "sediment reduction - sediment control" = "sedred_ctrl",
      "sediment reduction - erosion control structures" = "sedred_ctrl",
      "sediment reduction - road drainage system improvement " = "sedred_drain",
      "sediment reduction - road stream crossing improvement" = "sedred_drain",
      "fish passage - culvert improvements or upgrades" = "fishpass_culvimp",
      "fish passage - culvert removal" = "fishpass_culvrem",
      "fish passage - culvert installation" = "fishpass_culvinst",
      "fish passage - road crossings in stream (other than culverts)" = "fishpass_roadcrx",
      "land protected, acquired, or leased - streambank protection" = "landprot_stream",
      "land protected, acquired, or leased - wetland or estuarine area protction" = "landprot_wet",
      "instream flow - irrigation practice improvement" = "instflow_irri",
      "instream flow - water leased or purchased" = "instflow_rights"
    )
  ) %>%
  filter(!is.na(action)) %>% # n = 137,728
  # Pivot metrics into columns
  pivot_wider( # Need a way to manage multiple metrics
    names_from = metric,
    values_from = numeric_value,
    names_prefix = "metric_",
    values_fn = list(numeric_value = sum)
  ) %>% # n = 133,456,
  select(-metric_NA) %>% # 24 varaibles; 19,133 actions with at least one metric (vs. 29,030 in unprocessed data)
  # Dummify action variables for use in worksite/project summaries (can be used in summing, etc.)
  dummy_cols("action") # 55 variables

# Spare checks on actions, available metrics
df_pnshp %>% tabyl(action) %>% arrange(n)
df_pnshp %>% mutate(na_metric = I(!is.na(metric_length) | !is.na(metric_area) | !is.na(metric_count))) %>% tabyl(na_metric)

# Convert costs to 2019 dollars ====
# This section can be executed anywhere before collapsing data to higher levels of analysis
# Will eventually want to update to 2020 dollars; most recent data is Oct '19 as of Feb '20

# Grab CPI from St. Louis FED
gdpdef <- getSymbols("GDPDEF", src='FRED', auto.assign = FALSE)

# Get annually
avg_infl <- apply.yearly(gdpdef, mean)

# Conversion factor to 2019 dollars
cf <- as.numeric(avg_infl['2019'])/avg_infl

# Convert xts object to dataframe
df_cf <- data.frame(year = as.numeric(as.character(substr(index(cf), 1, 4))), coredata(cf))

# Merge with PNSHP and convert costs to 2018 dollars
df_pnshp <-
  df_pnshp %>%
  left_join(
    df_cf,
    by = c("completed_year" = "year")
  ) %>%
  mutate(
    project_cost = case_when( # Marks zero-cost projects as NA
      project_cost == 0 ~ NA_real_,
      TRUE ~ project_cost
    ),
    adj_cost = project_cost * GDPDEF
  ) %>%
  select(-GDPDEF)

rm(avg_infl, cf, df_cf, gdpdef)

# Quick check for cost availability; 98,455 observations (74%) have cost avaiable; 13,099 have both cost and metrics available
df_pnshp %>% mutate(na_cost = !is.na(adj_cost)) %>% tabyl(na_cost)
df_pnshp %>% mutate(na_cost = !is.na(adj_cost)) %>% mutate(na_metric = I(!is.na(metric_length) | !is.na(metric_area) | !is.na(metric_count))) %>% tabyl(na_cost, na_metric)

# Filter down to relevant timeframe and scale ====
df_pnshp <-
  df_pnshp %>%
  filter(
    completed_year %in% c(1991:2015) & # 25 year timeframe
      n_wrk <= 50
  )
# n = 70,613

# Create project and worksite level data ====
# Prepare column order for convient grouping
df_pnshp <-
  df_pnshp %>%
  select(
    project_id,
    project_year, completed_year,
    county, state, state_full,
    subwatershed:project_source,
    ends_with("_cost"),
    n_act, n_act_type_prj, n_wrk,
    worksite_id, latitude, longitude,
    n_act_type_wrk,
    starts_with("metric_"),
    starts_with("action"),
    everything()
  )

# Function for selecting most common (modal) value in a vector (for example, within a dplyr group when summarizing or mutating)
# Note: for ties, returns first alphebetically for character vectors, returns minimum for numeric vectors
most_common <- function(x){
  y <- names(which.max(table(x)))
  ifelse(
    is.null(y),
    ifelse(is.character(x),
           return(NA_character_),
           return(NA_real_)
    ),
    ifelse(is.character(x),
           return(y),
           return(as.numeric(y))
    )
  )
}

# Summarize to worksite level
df_pnshp_wrk <-
  df_pnshp %>%
  group_by(
    project_id, worksite_id
  ) %>%
  mutate_at(
    vars(ends_with("_year")), max, na.rm = TRUE
  ) %>%
  mutate_at(
   vars(county:project_source, ends_with("_cost"), starts_with("n_"), ends_with("tude")),
   most_common
  )

# NEXT STEP: Find a way to retain metrics when reducing to worksite level observations


# Save out data for further use ====
write_csv(PNSHP_pjt, "PNSHP_pjt.csv")
write_csv(PNSHP_wrk, "PNSHP_wrk.csv")
write_csv(PNSHP_act, "PNSHP_act.csv")
write_csv(PNSHP, "PNSHP_clean.csv")
