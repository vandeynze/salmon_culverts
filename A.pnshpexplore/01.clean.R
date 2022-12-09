# TITLE: Clean PNSHP data for cost analysis
# AUTHOR: Braeden Van Deynze
# DATE: February, 2020
# INPUTS: "PNSHP_raw.csv" for raw PNSHP data
# OUTPUTS: "PNSHP_full_working.csv" for full reduced PNSHP data; "PNSHP_prj_working.csv" for data aggregated to project level

# Prepare environment ====
# Clear environment
rm(list = ls())

# Load pakages
library(tidyverse)
library(janitor) # For name cleaning functions
library(fastDummies) # For efficient dummy variable generation
library(quantmod) # For FRED data pull and conversion to 2019 dollars
library(geosphere) # For distance calculations
library(searchable) # For action key label inversion
library(here)

# Set top-level working directory


# Build custom functions
# Should move these to a utilities.R script and source later
# most_common: function for selecting most common (modal) value in a vector (for example, within a dplyr group when summarizing or mutating)
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

# Import data ====
# Load data

df_pnshp <-
  read_csv(
    here(
      "/data/PNSHP_raw.csv"
    ),
    # "PNSHP_worksite-subtype_20200113.csv", # Outdated; missing values for most subtypes; remove once 20200210 version works
    guess_max = 200000 # Necessary to avoid type identification issues resulting from missing values
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
# All character columns to uppercase
df_pnshp <-
  df_pnshp %>%
  # Convert all character columns to uppercase
  mutate_if(
    is.character,
    str_to_upper 
  )

# Impute years from project_year when completed_year is missing
df_pnshp <-
  df_pnshp %>%
  # Clean years
  mutate(
    completed_year = case_when(
      is.na(completed_year) ~ project_year,
      TRUE ~ completed_year
    )
  )

# Clean worksite coordinates by replacing zeros with NA
df_pnshp <-
  df_pnshp %>%
  mutate_at(
    vars(latitude, longitude),
    na_if, 0
  )

# Clean state names
df_pnshp <-
  df_pnshp %>%
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
  )
# Will need to check for primary state, multistate projects later
# But just picking first state from main states as check (states presented alphedbetically so this is not a good fix)
# Likely doesn't matter because coordinates and funding org will be more important in the long-run and can be used to recover geographic units

# Clean source and basin names
df_pnshp <-
  df_pnshp %>%
  mutate(
    project_source = recode(
      project_source,
      "WARCO" = "WA RCO"
    ),
    basin = recode(
      basin,
      "MIDDLE SNAKE/BOISE" = "MIDDLE SNAKE-BOISE"
    )
  )

# Clean project_costs by replacing zeros with NA
df_pnshp <-
  df_pnshp %>%
  mutate(
    project_cost = na_if(project_cost, 0)
  )

# Charactarize projects (and worksites/actions) by data structure ====
# Generate counts of actions, worksites, and metrics by project and worksite
df_pnshp <-
  df_pnshp %>%
  # Counts of distinct metrics by action
  group_by(project_id, worksite_id, action) %>%
  mutate(
    n_metric = n_distinct(metric, na.rm = TRUE)
  ) %>%
  # Counts of distinct worksites and action types at project level, then action types at worksite level
  group_by(project_id, worksite_id) %>%
  mutate(
    n_action_wrk = n_distinct(action),
    n_metric_wrk = sum(!is.na(metric)),
    n_metric_wrk_dist = n_distinct(metric, na.rm = TRUE)
  ) %>%
  group_by(project_id) %>%
  mutate(
    n_worksites = n_distinct(worksite_id),
    n_action_prj = n_distinct(worksite_id, action),
    n_action_prj_dist = n_distinct(action),
    n_metric_prj = sum(!is.na(metric)),
    n_metric_prj_dist = n_distinct(metric, na.rm = TRUE)
  ) %>%
  ungroup()

# Create indicator for when all worksites in a project have the same actions
df_pnshp <-
  df_pnshp %>%
  group_by(project_id, worksite_id) %>%
  mutate(
    same_action = I(n_action_wrk == n_action_prj_dist)
  ) %>%
  group_by(project_id) %>%
  mutate(
    same_action = prod(same_action)
  ) %>%
  ungroup()

# Create indicator for when at least some actions are missing metrics
df_pnshp <-
  df_pnshp %>%
  group_by(project_id, worksite_id, action) %>%
  mutate(
    missing_metric = sum(is.na(metric))
  ) %>%
  group_by(project_id) %>%
  mutate(
    missing_metric = sum(missing_metric)
  ) %>%
  ungroup()

# Generate indicators at project and worksite level for action types
# Create key for converting actions from longform to short code
action_key <-
  c(
    "ESTUARY/ NEARSHORE - CHANNEL MODIFICATION" = "est_chanmod",
    "ESTUARY/ NEARSHORE - CREATION OF NEW ESTUARINE HABITAT" = "est_create",
    "ESTUARY/ NEARSHORE - DIKE BREACHING/ REMOVAL" = "est_dikebreach",
    "ESTUARY/ NEARSHORE - DIKE RECONFIGURATION" = "est_dikereconf",
    "ESTUARY/ NEARSHORE - INVASIVE SPECIES TREATED" = "est_invasive",
    "ESTUARY/ NEARSHORE - REMOVAL OF EXISTING FILL MATERIAL" = "est_fillrem",
    "ESTUARY/ NEARSHORE - RESTORATION/REHABILITATION OF ESTUARINE HABITAT" = "est_resthab",
    "ESTUARY/ NEARSHORE - SHORELINE ARMOR REMOVAL OR MODIFICATION" = "est_armormod",
    "ESTUARY/ NEARSHORE - TIDEGATE ALTERATION/ REMOVAL" = "est_tidegate",
    "FISH PASSAGE - BARRIERS (DAMS OR LOG JAMS)" = "fishpass_barriers",
    "FISH PASSAGE - CULVERT IMPROVEMENTS OR UPGRADES" = "fishpass_culvimp",
    "FISH PASSAGE - CULVERT INSTALLATION" = "fishpass_culvinst",
    "FISH PASSAGE - CULVERT REMOVAL" = "fishpass_culvrem",
    "FISH PASSAGE - DIVERSION DAM/ PUSH UP DAM REMOVAL" = "fishpass_diverdam",
    "FISH PASSAGE - FISH LADDER IMPROVED" = "fishpass_ladderimp",
    "FISH PASSAGE - FISH LADDER INSTALLED" = "fishpass_ladderinst",
    "FISH PASSAGE - FISHWAYS (CHUTES OR POOLS) INSTALLED" = "fishpass_waysinst",
    "FISH PASSAGE - ROAD CROSSINGS IN STREAM (OTHER THAN CULVERTS)" = "fishpass_roadcross",
    "FISH PASSAGE - WEIRS (INCOMPLETE DAMS)" = "fishpass_weirs",
    "FISH PASSAGE IMPROVEMENT - ROCKED FORD- ROAD STREAM CROSSING" = "fishpass_rockford",
    "FISH SCREENING - FISH SCREEN INSTALLED" = "screen_inst",
    "FISH SCREENING - FISH SCREEN REPLACED" = "screen_replace",
    "INSTREAM - BEAVER INTRODUCTION" = "instream_beaver",
    "INSTREAM - BOULDERS" = "instream_boulder",
    "INSTREAM - CHANNEL CONNECTIVITY" = "instream_connect",
    "INSTREAM - CHANNEL RECONFIGURATION" = "instream_reconf",
    "INSTREAM - DEFLECTORS/ BARBS" = "instream_barbs",
    "INSTREAM - LARGE WOODY DEBRIS" = "instream_wooddebris",
    "INSTREAM - LOG WEIRS" = "instream_logweir",
    "INSTREAM - OFF CHANNEL HABITAT" = "instream_offchannel",
    "INSTREAM - PLANT REMOVAL/ CONTROL" = "instream_plantrem",
    "INSTREAM - PREDATOR REMOVAL PROJECT" = "instream_predrem",
    "INSTREAM - ROCK WEIRS" = "instream_rockweir",
    "INSTREAM - ROOTWADS" = "instream_rootwad",
    "INSTREAM - SPAWNING GRAVEL PLACEMENT" = "instream_gravel",
    "INSTREAM - STREAMBANK STABILIZATION" = "instream_bankstab",
    "INSTREAM - WOOD STRUCTURE/ LOG JAM" = "instream_logjam",
    "INSTREAM FLOW - IRRIGATION PRACTICE IMPROVEMENT" = "flow_irriimp",
    "INSTREAM FLOW - WATER LEASED OR PURCHASED" = "flow_waterlease",
    "LAND PROTECTED, ACQUIRED, OR LEASED - STREAMBANK PROTECTION" = "land_streambank",
    "LAND PROTECTED, ACQUIRED, OR LEASED - WETLAND OR ESTUARINE AREA PROTECTION" = "land_wetland",
    "NUTRIENT ENRICHMENT - CARCASS ANALOG" = "nutrient_carcanalog",
    "NUTRIENT ENRICHMENT - CARCASS PLACEMENT" = "nutrient_carcplace",
    "NUTRIENT ENRICHMENT - FERTILIZER" = "nutrient_fert",
    "PROJECT MAINTENANCE - SITE MAINTENANCE" = "projmaint_sitemaint",
    "RIPARIAN - CONSERVATION GRAZING MANAGEMENT" = "riparian_graze",
    "RIPARIAN - FENCING" = "riparian_fence",
    "RIPARIAN - FORESTRY PRACTICES/ STAND MANAGEMENT" = "riparian_forest",
    "RIPARIAN - LIVESTOCK EXCLUSION" = "riparian_liveexcl",
    "RIPARIAN - LIVESTOCK WATER DEVELOPMENT" = "riparian_livewater",
    "RIPARIAN - PLANTING" = "riparian_plant",
    "RIPARIAN - WATER GAP DEVELOPMENT" = "riparian_watergap",
    "RIPARIAN - WEED CONTROL" = "riparian_weed",
    "SEDIMENT REDUCTION - EROSION CONTROL STRUCTURES" = "sedireduce_contstruc",
    "SEDIMENT REDUCTION - ROAD DRAINAGE SYSTEM IMPROVEMENTS" = "sedireduce_roaddrain",
    "SEDIMENT REDUCTION - ROAD OBLITERATION" = "sedireduce_roadobli",
    "SEDIMENT REDUCTION - ROAD RECONSTRUCTION" = "sedireduce_roadrecon",
    "SEDIMENT REDUCTION - ROAD RELOCATION" = "sedireduce_roadrelo",
    "SEDIMENT REDUCTION - ROAD STREAM CROSSING IMPROVEMENTS" = "sedireduce_roadcross",
    "SEDIMENT REDUCTION - SEDIMENT CONTROL" = "sedireduce_sedicont",
    "UPLAND AGRICULTURE - BEST MANAGEMENT PRACTICES/ AGRICULTURE MANAGEMENT" = "agri_mgmt",
    "UPLAND AGRICULTURE - BEST MANAGEMENT PRACTICES/ STRUCTURAL PRACTICES" = "agri_structure",
    "UPLAND AGRICULTURE - BEST MANAGEMENT PRACTICES/ VEGETATIVE AND TILLING PRACTICES" = "agri_tillage",
    "UPLAND LIVESTOCK - UPLAND EXCLUSION OR FENCING" = "livestock_fence",
    "UPLAND LIVESTOCK - UPLAND GRAZING MANAGEMENT" = "livestock_graze",
    "UPLAND LIVESTOCK - WATER DEVELOPMENT" = "livestock_water",
    "UPLAND VEGETATION - INVASIVE PLANT CONTROL" = "uplandveg_invasive",
    "UPLAND VEGETATION - PLANTING" = "uplandveg_plant",
    "UPLAND VEGETATION - SLOPE STABILIZATION" = "uplandveg_slope",
    "UPLAND VEGETATION - VEGETATION/ STAND MANAGEMENT" = "uplandveg_standmgmt",
    "WATER QUALITY IMPROVEMENT - LIVESTOCK MANURE MANAGEMENT" = "wqimp_manure",
    "WATER QUALITY IMPROVEMENT - REFUSE REMOVAL" = "wqimp_refuse",
    "WATER QUALITY IMPROVEMENT - RETURN FLOW COOLING" = "wqimp_flowcool",
    "WATER QUALITY IMPROVEMENT - SEWAGE CLEAN-UP" = "wqimp_sewage",
    "WATER QUALITY IMPROVEMENT - STORMWATER/WASTEWATER" = "wqimp_wastewater",
    "WATER QUALITY IMPROVEMENT - TOXIC CLEAN-UP" = "wqimp_toxic",
    "WETLAND - PLANT REMOVAL/ CONTROL" = "wetland_plantrem",
    "WETLAND - PLANTING" = "wetland_plant",
    "WETLAND - WETLAND CREATION" = "wetland_create",
    "WETLAND - WETLAND IMPROVEMENT/ ENHANCEMENT" = "wetland_imp",
    "WETLAND - WETLAND INVASIVE SPECIES REMOVAL" = "wetland_invasive",
    "WETLAND - WETLAND RESTORATION" = "wetland_restore"
  )
  
df_pnshp <-
  df_pnshp %>%
  # Merge obviously redundent action labels
  mutate(
    action = recode(
      action,
      "SEDIMENT REDUCTION - ROAD DRAINAGE SYSTEM IMPROVEMENT" = "SEDIMENT REDUCTION - ROAD DRAINAGE SYSTEM IMPROVEMENTS",
      "SEDIMENT REDUCTION - ROAD STREAM CROSSING IMPROVEMENT" = "SEDIMENT REDUCTION - ROAD STREAM CROSSING IMPROVEMENTS",
      "UPLAND WETLAND - WETLAND IMPROVEMENT/ ENHANCEMENT" = "WETLAND - WETLAND IMPROVEMENT/ ENHANCEMENT",
      "WATER QUALITY IMPROVEMENT - STORMWATER/WASTEWATER MODIFICATION/ TREATMENT" = "WATER QUALITY IMPROVEMENT - STORMWATER/WASTEWATER",
      "WETLAND - WETLAND INVASIVE/NOXIOUS WEED SPECIES REMOVAL" = "WETLAND - WETLAND INVASIVE SPECIES REMOVAL",
      "WETLAND - WETLAND VEGETATION PLANTING" = "WETLAND - PLANTING"
    )
  ) %>%
  # Recode all actions to short codes
  mutate(
    action = recode(action, !!!action_key)
  ) %>%
  dummy_cols("action")

# Convert costs to 2019 dollars ====
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

# Build indicators for cost/metric availability, number of action class
df_pnshp <-
  df_pnshp %>%
  mutate(
    cost_avail = !is.na(adj_cost),
    metric_avail = I(n_metric > 0),
    both_avail = as.logical(cost_avail * metric_avail),
    avail_class = case_when(
      cost_avail == TRUE & metric_avail == TRUE ~ "Costs and Metric Available",
      cost_avail == TRUE & metric_avail == FALSE ~ "Costs Available, No Metric",
      # cost_avail == TRUE & metric_avail == TRUE ~ "Metric Only",
      cost_avail == FALSE ~ "No Costs"
    ),
    n_action_class = case_when(
      n_action_prj > 1 & n_action_prj_dist > 1 ~ "Multiple Actions, Multiple Types",
      n_action_prj == 1 & n_action_prj_dist == 1 ~ "Single Action, Single Type",
      n_action_prj > 1 & n_action_prj_dist == 1 ~ "Multiple Actions, Single Type"
    ),
    avail_class = ordered(avail_class, levels = c("No Costs", "Costs Available, No Metric", "Costs and Metric Available"))
  )

# Summarize actions by project ====
# This takes a while so go check your emails or something
df_prj <-
  df_pnshp %>%
  group_by(project_id) %>%
  mutate_at(
    vars(starts_with("action_")),
    list(
      ~as.numeric(I(sum(.) > 0)),
      ~sum(.))
  ) %>%
  select(
    -(starts_with("action_") & !ends_with(c("_sum", "_as.numeric")))
  ) %>%
  rename_at(
    vars(starts_with("action_") & ends_with("_as.numeric")),
    ~str_remove(., "_as.numeric")
  ) %>%
  rename_at(
    vars(starts_with("action_") & ends_with("_sum")),
    ~str_replace(., "_sum", "_count")
  )

# Summarize cost and metric availability indicators
df_prj <-
  df_prj %>%
  mutate(
    cost_avail = !is.na(adj_cost),
    metric_avail = I(n_metric_prj > 0),
    both_avail = as.logical(cost_avail * metric_avail),
    avail_class = case_when(
      cost_avail == TRUE & metric_avail == TRUE ~ "Costs and Metric Available",
      cost_avail == TRUE & metric_avail == FALSE ~ "Costs Available, No Metric",
      # cost_avail == TRUE & metric_avail == TRUE ~ "Metric Only",
      cost_avail == FALSE ~ "No Costs"
    ),
    n_action_class = case_when(
      n_action_prj > 1 & n_action_prj_dist > 1 ~ "Multiple Actions, Multiple Types",
      n_action_prj == 1 & n_action_prj_dist == 1 ~ "Single Action, Single Type",
      n_action_prj > 1 & n_action_prj_dist == 1 ~ "Multiple Actions, Single Type"
    ),
    avail_class = ordered(avail_class, levels = c("No Costs", "Costs Available, No Metric", "Costs and Metric Available"))
  )

# Identify dominate geographic info
# Center of all worksites but taking the mean of latitude and longitude
# Keep most frequent basin
df_prj <-
  df_prj %>%
  group_by(project_id) %>%
  mutate_at(
    c("latitude", "longitude"),
    ~mean(., na.rm = TRUE)
  ) %>%
  mutate(
    basin = most_common(basin)
  )

# Drop extra info and reduce to distinct projects
df_prj <-
  df_prj %>%
  select(
    -worksite_id,
    -county,
    -state,
    -subwatershed,
    -watershed,
    -subbasin,
    -action,
    -metric,
    -numeric_value,
    -units,
    -state_full,
    -n_metric,
    -n_action_wrk,
    -n_metric_wrk,
    -n_metric_wrk_dist,
  ) %>%
  distinct(project_id, .keep_all = TRUE)

# Save relevant data ====
# Filter out NA coordinates, outside year range, extreme outliers for number of worksites and cost levels
df_pnshp <-
  df_pnshp %>%
  filter(
    !is.na(latitude) & completed_year %in% c(1991:2015) & n_worksites < 50
  ) %>%
  filter(
    adj_cost < 50000000 | is.na(adj_cost)
  )

df_prj <-
  df_prj %>%
  filter(
    !is.na(latitude) & completed_year %in% c(1991:2015) & n_worksites < 50
  ) %>%
  filter(
    adj_cost < 50000000 | is.na(adj_cost)
  )

# Save it
write_csv(df_pnshp, here("/output/PNSHP_full_working.csv"))
write_csv(df_prj, here("/output/PNSHP_prj_working.csv"))