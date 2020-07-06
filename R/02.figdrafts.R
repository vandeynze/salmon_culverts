# TITLE: Draft descriptive figures for PNSHP data
# AUTHOR: Braeden Van Deynze
# DATE: March, 2020
# INPUTS: "PNSHP_full_working.csv" for full reduced PNSHP data; "PNSHP_prj_working.csv" for data aggregated to project level
# OUTPUTS: None


# Prepeare environment ====
# Clear environment
rm(list = ls())

# Load libraries
library(tidyverse)
library(ggthemes)
library(scales)
library(searchable)
library(janitor)

# Set top-level working directory
# wd <- "C:/Users/Braeden/Desktop/NOAA/Analysis"
# # wd <- "C:/Users/braeden.vandeynze/Documents/Salmon Cost Project/Analysis/"  # For Braeden; comment out for other users
# setwd(wd)

# Set ggplot2 themes
theme_set(theme_clean())
theme_update(plot.background = element_rect(color = NA)) # Removes ugly black border of plot

# Load data ====
# setwd(wd)
setwd("./output") # For Braeden; comment out for other users
df_pnshp <- read_csv("PNSHP_full_working.csv")
df_prj <- read_csv("PNSHP_prj_working.csv")

# Prep level lists and keys ====
# Reduce full data to action-level (removes excess rows for multiple metric actions)
df_pnshp <-
  df_pnshp %>%
  distinct(project_id, worksite_id, action, .keep_all = TRUE) %>%
  mutate(
    avail_class = ordered(avail_class, levels = c("No Costs", "Costs Available, No Metric", "Costs and Metric Available"))
  )

# Doesn't do anything but potentially useful in case of screw-ups in cleaning stage
df_prj <-
  df_prj %>%
  distinct(project_id, .keep_all = TRUE) %>%
  mutate(
    avail_class = ordered(avail_class, levels = c("No Costs", "Costs Available, No Metric", "Costs and Metric Available"))
  )

# Key for moving between action codes and full names
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

# Pull and sort labels for ordered factors used in facets
action_levels <-
  df_pnshp %>%
  mutate(action_full = recode(action, !!!invert(action_key))) %>%
  tabyl(action_full) %>%
  arrange(-n) %>%
  pull(action_full)

basin_levels <-
  df_pnshp %>%
  tabyl(basin) %>%
  arrange(-n) %>%
  pull(basin)

source_levels <-
  df_pnshp %>%
  tabyl(project_source) %>%
  arrange(-n) %>%
  pull(project_source)

basin_levels_prj <-
  df_prj %>%
  tabyl(basin) %>%
  arrange(-n) %>%
  pull(basin)

source_levels_prj <-
  df_prj %>%
  tabyl(project_source) %>%
  arrange(-n) %>%
  pull(project_source)

action_levels_spend <-
  df_prj %>%
  filter(adj_cost < 50000000) %>%
  ungroup() %>%
  mutate_at(
    vars(starts_with("action_")),
    as.numeric
  ) %>%
  mutate_at(
    vars(starts_with("action_")),
    list(~ .*adj_cost)
  ) %>%
  summarize_at(
    vars(starts_with("action_")),
    list(~sum(., na.rm = TRUE))
  ) %>%
  pivot_longer(
    everything(),
    names_to = "action",
    names_prefix = "action_",
    values_to = "adj_cost"
  ) %>%
  mutate(action_full = recode(action, !!!invert(action_key))) %>%
  arrange(-adj_cost) %>%
  pull(action_full)

basin_levels_spend <-
  df_prj %>%
  group_by(basin) %>%
  summarize(adj_cost = sum(adj_cost, na.rm = TRUE)) %>%
  arrange(-adj_cost) %>%
  pull(basin)

source_levels_spend <-
  df_prj %>%
  group_by(project_source) %>%
  summarize(adj_cost = sum(adj_cost, na.rm = TRUE)) %>%
  arrange(-adj_cost) %>%
  pull(project_source)

# Generate figures ====
# Generates horizontal column charts (or line/area charts when time is involved) over action, year, basin, and source
# Each section that follows builds a summary tibble from df_pnshp or df_prj then plots it over over grouping variables
# To improve readibility, the *_levels_* character vectors are used to restrict charts to the top n levels for each type

# Build base figures ====
# Base bar plot for counts
base_count_bar <-
  ggplot(df_pnshp, aes(y = n)) +
  geom_col(
    aes(
      fill = -log(n)
    )
  ) +
  scale_x_discrete(
    name = NULL
  ) +
  scale_y_continuous(
    name = "Count",
    labels = label_comma(),
    position = "right"
  ) +
  scale_fill_distiller(
    palette = "YlGn"
  ) +
  theme(
    legend.position = "none"
  ) +
  coord_flip()

# Base line plot for counts
# (Technically an area plot)
base_count_line <-
  ggplot(df_pnshp, aes(y = n, x = completed_year)) +
  geom_vline(
    xintercept = seq(1990, 2015, 5),
    alpha = 0.5,
    linetype = "dashed"
  ) +
  geom_area(
    fill = "#BAE4B3"
  ) +
  scale_x_continuous(
    name = NULL
  ) +
  scale_y_continuous(
    name = "Count",
    labels = label_comma()
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5)
  )

# Base bar plot for counts with costs
base_countcost_bar <-
  ggplot(df_pnshp, aes(y = n, fill = avail_class, group = avail_class)) +
  geom_col() +
  scale_x_discrete(
    name = NULL
  ) +
  scale_y_continuous(
    name = "Count",
    labels = label_comma(),
    position = "right"
  ) +
  scale_fill_brewer("Costs/Metric Reported", palette = "Greens") +
  coord_flip()

# Base line plot for counts with costs
base_countcost_line <-
  ggplot(
    df_pnshp,
    aes(
      x = completed_year,
      y = n,
      fill = avail_class,
      group = avail_class
    )
  ) +
  geom_vline(
    xintercept = seq(1990, 2015, 5),
    alpha = 0.5,
    linetype = "dashed"
  ) +
  geom_area() +
  scale_x_continuous(
    name = NULL
  ) +
  scale_y_continuous(
    name = "Count",
    labels = label_comma()
  ) +
  scale_fill_brewer("Costs/Metric Reported", palette = "Greens") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5)
  )

# Base bar plot for costs
base_cost_bar <-
  ggplot(
    df_prj,
    aes(
      y = adj_cost/1000
    )
  ) +
  geom_col(
    aes(
      fill = -log(adj_cost)
    )
  ) +
  scale_x_discrete(
    name = NULL
  ) +
  scale_y_continuous(
    name = NULL,
    labels = label_comma(),
    position = "right"
  ) +
  scale_fill_distiller(palette = "YlGn") +
  theme(
    legend.position = "none"
  ) +
  coord_flip()

# Base line plot for costs
base_cost_line <-
  ggplot(
    df_prj,
    aes(
      x = completed_year,
      y = adj_cost/1000
    )
  ) +
  geom_vline(
    xintercept = seq(1990, 2015, 5),
    alpha = 0.5,
    linetype = "dashed"
  ) +
  geom_area(
    fill = "#A1D99B"
  ) +
  scale_x_continuous(
    name = NULL
  ) +
  scale_y_continuous(
    name = NULL,
    labels = label_comma()
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5)
  )

# Action counts w/ costs ====
# By type
fig_countcost_type <-
  base_countcost_bar %+%
  (df_pnshp %>%
     group_by(action, avail_class) %>%
     summarize(n = n()) %>%
     mutate(action_full = recode(action, !!!invert(action_key))) %>%
     filter(action_full %in% action_levels[1:50])
  ) +
  aes(
    x = reorder(action_full, n)
  ) +
  ggtitle("Action count, by action type", "50 (of 82) most common action types")
fig_countcost_type

# By year
fig_countcost_year <-
  base_countcost_line %+%
  (df_pnshp %>%
     group_by(completed_year, avail_class) %>%
     summarize(n = n())
  ) +
  ggtitle("Action count, by year")
fig_countcost_year

# By basin
fig_countcost_basin <-
  base_countcost_bar %+%
  (df_pnshp %>%
     group_by(basin, avail_class) %>%
     summarize(n = n()) %>%
     mutate(basin_fac = factor(basin, basin_levels, ordered = TRUE)) %>%
     filter(!is.na(basin))
  ) +
  aes(
    x = reorder(basin, n)
  ) +
  ggtitle("Action count, by basin")
fig_countcost_basin

# By source
fig_countcost_source <-
  base_countcost_bar %+%
  (df_pnshp %>%
     group_by(project_source, avail_class) %>%
     summarize(n = n()) %>%
     mutate(source_fac = factor(project_source, source_levels, ordered = TRUE)) %>%
     filter(!is.na(project_source))
  ) +
  aes(
    x = reorder(project_source, n)
  ) +
  ggtitle("Action count, by source")
fig_countcost_source

# By year and type
fig_countcost_year_type <-
  base_countcost_line %+%
  (df_pnshp %>%
     group_by(action, completed_year, avail_class) %>%
     summarize(n = n()) %>%
     mutate(action_full = recode(action, !!!invert(action_key))) %>%
     mutate(action_fac = factor(action_full, action_levels, ordered = TRUE)) %>%
     filter(action_full %in% action_levels[1:16])
  ) +
  ggtitle("Action count, by year and action type", "16 (of 82) most common action types") +
  facet_wrap(~ action_fac, ncol = 4)
fig_countcost_year_type

# By basin and type
fig_countcost_basin_type <-
  base_countcost_bar %+%
  (df_pnshp %>%
     group_by(action, basin, avail_class) %>%
     summarize(n = n()) %>%
     mutate(action_full = recode(action, !!!invert(action_key))) %>%
     mutate(action_fac = factor(action_full, action_levels, ordered = TRUE)) %>%
     mutate(basin_fac = factor(basin, basin_levels, ordered = TRUE)) %>%
     filter(
       action_full %in% action_levels[1:16]
     ) %>%
     filter(
       basin %in% basin_levels[1:10]
     )
  ) +
  aes(
    x = reorder(action_full, n)
  ) +
  ggtitle("Action count, by basin and type", "16 (of 82) most common action types in 10 (of 47) basins with most actions") +
  facet_wrap(~ basin_fac, ncol = 2)
fig_countcost_basin_type

# By source and type
fig_countcost_source_type <-
  base_countcost_bar %+%
  (df_pnshp %>%
     group_by(action, project_source, avail_class) %>%
     summarize(n = n()) %>%
     mutate(action_full = recode(action, !!!invert(action_key))) %>%
     mutate(action_fac = factor(action_full, action_levels, ordered = TRUE)) %>%
     mutate(source_fac = factor(project_source, source_levels, ordered = TRUE)) %>%
     filter(
       action_full %in% action_levels[1:12]
     ) %>%
     filter(
       project_source %in% source_levels[1:16]
     )
  ) +
  aes(
    x = reorder(action_full, n)
  ) +
  ggtitle("Action count, by source and type", "12 (of 82) most common action types by 16 sources (of 38) with most actions; note X-axis varies by source") +
  facet_wrap(~ source_fac, scales = "free_x", ncol = 4)
fig_countcost_source_type

# By source and year
fig_countcost_source_year <-
  base_countcost_line %+%
  (df_pnshp %>%
     group_by(completed_year, project_source, avail_class) %>%
     summarize(n = n()) %>%
     mutate(source_fac = factor(project_source, source_levels, ordered = TRUE)) %>%
     filter(
       project_source %in% source_levels[1:10]
     )
  ) +
  ggtitle("Action count, by source and year", "10 sources (of 38) with most actions") +
  facet_wrap(~ source_fac, ncol = 2)
fig_countcost_source_year

# By basin and year
fig_countcost_basin_year <-
  base_countcost_line %+%
  (df_pnshp %>%
     group_by(completed_year, basin, avail_class) %>%
     summarize(n = n()) %>%
     mutate(basin_fac = factor(basin, basin_levels, ordered = TRUE)) %>%
     filter(
       basin %in% basin_levels[1:12]
     )
  ) +
  ggtitle("Action count, by basin and year", "12 basins (of 47) with most actions") +
  facet_wrap(~ basin_fac, ncol = 4)
fig_countcost_basin_year

# By basin and funding
fig_countcost_basin_source <-
  base_countcost_bar %+%
  (df_pnshp %>%
     group_by(project_source, basin, avail_class) %>%
     summarize(n = n()) %>%
     mutate(source_fac = factor(project_source, source_levels, ordered = TRUE)) %>%
     mutate(basin_fac = factor(basin, basin_levels, ordered = TRUE)) %>%
     filter(
       project_source %in% source_levels[1:12]
     ) %>%
     filter(
       basin %in% basin_levels[1:10]
     )
  ) +
  aes(
    x = reorder(basin, n)
  ) +
  ggtitle("Action count, by basin and funding source", "10 basins (of 47) and 12 sources (of 38) with most actions") +
  facet_wrap(~ source_fac, ncol = 4)
fig_countcost_basin_source

# Project count w/ costs ====
# By year
fig_prjcost_year <-
  base_countcost_line %+%
  (df_prj %>%
     group_by(completed_year, avail_class) %>%
     summarize(n = n())
  ) +
  ggtitle("Project count, by year")
fig_prjcost_year

# By basin
fig_prjcost_basin <-
  base_countcost_bar %+%
  (df_prj %>%
     group_by(basin, avail_class) %>%
     summarize(n = n()) %>%
     mutate(basin_fac = factor(basin, basin_levels_prj, ordered = TRUE)) %>%
     filter(!is.na(basin))
  ) +
  aes(
    x = reorder(basin, n)
  ) +
  ggtitle("Project count, by basin")
fig_prjcost_basin

# By source
fig_prjcost_source <-
  base_countcost_bar %+%
  (df_prj %>%
     group_by(project_source, avail_class) %>%
     summarize(n = n()) %>%
     mutate(source_fac = factor(project_source, source_levels_prj, ordered = TRUE)) %>%
     filter(!is.na(project_source))
  ) +
  aes(
    x = reorder(project_source, n)
  ) +
  ggtitle("Project count, by source")
fig_prjcost_source

# By source and year
fig_prjcost_source_year <-
  base_countcost_line %+%
  (df_prj %>%
     group_by(completed_year, project_source, avail_class) %>%
     summarize(n = n()) %>%
     mutate(source_fac = factor(project_source, source_levels_prj, ordered = TRUE)) %>%
     filter(
       project_source %in% source_levels_prj[1:12]
     )
  ) +
  ggtitle("Project count, by source and year", "12 sources (of 38) with most actions") +
  facet_wrap(~ source_fac, ncol = 4)
fig_prjcost_source_year

# By basin and year
fig_prjcost_basin_year <-
  base_countcost_line %+%
  (df_prj %>%
     group_by(completed_year, basin, avail_class) %>%
     summarize(n = n()) %>%
     mutate(basin_fac = factor(basin, basin_levels_prj, ordered = TRUE)) %>%
     filter(
       basin %in% basin_levels_prj[1:12]
     )
  ) +
  ggtitle("Project count, by basin and year", "12 basins (of 47) with most actions") +
  facet_wrap(~ basin_fac, ncol = 4)
fig_prjcost_basin_year

# By basin and funding
fig_prjcost_basin_source <-
  base_countcost_bar %+%
  (df_prj %>%
     group_by(project_source, basin, avail_class) %>%
     summarize(n = n()) %>%
     mutate(source_fac = factor(project_source, source_levels_prj, ordered = TRUE)) %>%
     mutate(basin_fac = factor(basin, basin_levels_prj, ordered = TRUE)) %>%
     filter(
       project_source %in% source_levels_prj[1:12]
     ) %>%
     filter(
       basin %in% basin_levels_prj[1:10]
     )
  ) +
  aes(
    x = reorder(basin, n)
  ) +
  ggtitle("Project count, by basin and source", "10 basins (of 47) and 12 funders (of 38) with most actions") +
  facet_wrap(~ source_fac, ncol = 4)
fig_prjcost_basin_source

# Spending totals ====
# By action
fig_spend_action <-
  base_cost_bar %+%
  (df_prj %>%
     ungroup() %>%
     mutate_at(
       vars(starts_with("action_")),
       as.numeric
     ) %>%
     mutate_at(
       vars(starts_with("action_")),
       list(~ .*adj_cost)
     ) %>%
     summarize_at(
       vars(starts_with("action_")),
       list(~sum(., na.rm = TRUE))
     ) %>%
     pivot_longer(
       everything(),
       names_to = "action",
       names_prefix = "action_",
       values_to = "adj_cost"
     ) %>%
     mutate(action_full = recode(action, !!!invert(action_key))) %>%
     filter(action_full %in% action_levels[1:50])
  ) +
  aes(
    x = reorder(action_full, adj_cost)
  ) +
  ggtitle("Expenditures ($thou), by action type", "50 (of 82) most common action types")
fig_spend_action

# By year
fig_spend_year <-
  base_cost_line %+%
  (df_prj %>%
     group_by(completed_year) %>%
     summarize(adj_cost = sum(adj_cost, na.rm = TRUE))
  ) +
  ggtitle("Expenditures ($thou), by year")
fig_spend_year

# By basin
fig_spend_basin <-
  base_cost_bar %+%
  (df_prj %>%
     group_by(basin) %>%
     summarize(adj_cost = sum(adj_cost, na.rm = TRUE)) %>%
     drop_na()
  ) +
  aes(
    x = reorder(basin, adj_cost)
  ) +
  ggtitle("Expenditures ($thou), by basin")
fig_spend_basin

# By source
fig_spend_source <-
  base_cost_bar %+%
  (df_prj %>%
     group_by(project_source) %>%
     summarize(adj_cost = sum(adj_cost, na.rm = TRUE)) %>%
     drop_na()
  ) +
  aes(
    x = reorder(project_source, adj_cost)
  ) +
  ggtitle("Expenditures ($thou), by source")
fig_spend_source

# By year and action
fig_spend_year_action <-
  base_cost_line %+%
  (df_prj %>%
     ungroup() %>%
     mutate_at(
       vars(starts_with("action_")),
       as.numeric
     ) %>%
     mutate_at(
       vars(starts_with("action_")),
       list(~ .*adj_cost)
     ) %>%
     group_by(completed_year) %>%
     summarize_at(
       vars(starts_with("action_")),
       list(~sum(., na.rm = TRUE))
     ) %>%
     pivot_longer(
       starts_with("action_"),
       names_to = "action",
       names_prefix = "action_",
       values_to = "adj_cost"
     ) %>%
     mutate(action_full = recode(action, !!!invert(action_key))) %>%
     mutate(action_fac = factor(action_full, action_levels_spend, ordered = TRUE)) %>%
     filter(action_full %in% action_levels_spend[1:18])
  ) +
  ggtitle("Expenditures ($thou), by year and action type", "18 (of 82) action types with highest expenditures") +
  theme(
    strip.text = element_text(size = 8)
  ) +
  facet_wrap(~ action_fac, ncol = 3)
fig_spend_year_action

# By year and basin
fig_spend_year_basin <-
  base_cost_line %+%
  (df_prj %>%
     group_by(completed_year, basin) %>%
     summarize(adj_cost = sum(adj_cost, na.rm = TRUE)) %>%
     drop_na() %>%
     mutate(basin_fac = factor(basin, basin_levels_spend, ordered = TRUE)) %>%
     filter(basin %in% basin_levels_spend[1:12])
  ) + 
  ggtitle("Expenditures ($thou), by year and basin", "12 (of 47) basins with highest expenditures") +
  facet_wrap(~ basin_fac, ncol = 3)
fig_spend_year_basin

# By year and source
fig_spend_year_source <-
  base_cost_line %+%
  (df_prj %>%
     group_by(completed_year, project_source) %>%
     summarize(adj_cost = sum(adj_cost, na.rm = TRUE)) %>%
     drop_na() %>%
     mutate(source_fac = factor(project_source, source_levels_spend, ordered = TRUE)) %>%
     filter(project_source %in% source_levels_spend[1:8])
  ) +
  ggtitle("Expenditures ($thou), by year and source", "8 (of 38) sources with highest reported expenditures") +
  facet_wrap(~ source_fac, ncol = 2)
fig_spend_year_source

# By basin and action
fig_spend_basin_action <-
  base_cost_bar %+%
  (df_prj %>%
     ungroup() %>%
     mutate_at(
       vars(starts_with("action_")),
       as.numeric
     ) %>%
     mutate_at(
       vars(starts_with("action_")),
       list(~ .*adj_cost)
     ) %>%
     group_by(basin) %>%
     summarize_at(
       vars(starts_with("action_")),
       list(~sum(., na.rm = TRUE))
     ) %>%
     pivot_longer(
       starts_with("action_"),
       names_to = "action",
       names_prefix = "action_",
       values_to = "adj_cost"
     ) %>%
     mutate(action_full = recode(action, !!!invert(action_key))) %>%
     mutate(action_fac = factor(action_full, action_levels_spend, ordered = TRUE)) %>%
     mutate(basin_fac = factor(basin, basin_levels_spend, ordered = TRUE))  %>%
     filter(action_full %in% action_levels_spend[1:16]) %>%
     filter(basin %in% basin_levels_spend[1:8])
  ) +
  aes(
    x = reorder(action_full, adj_cost)
  ) +
  ggtitle("Expenditures ($thous), by basin and action type", "16 (of 82) action types with highest expenditures in \n8 (of 47) basins with highest expenditures") +
  facet_wrap(~ basin_fac, ncol = 2)
fig_spend_basin_action

# By source and action
fig_spend_source_action <-
  base_cost_bar %+%
  (df_prj %>%
     ungroup() %>%
     mutate_at(
       vars(starts_with("action_")),
       as.numeric
     ) %>%
     mutate_at(
       vars(starts_with("action_")),
       list(~ .*adj_cost)
     ) %>%
     group_by(project_source) %>%
     summarize_at(
       vars(starts_with("action_")),
       list(~sum(., na.rm = TRUE))
     ) %>%
     pivot_longer(
       starts_with("action_"),
       names_to = "action",
       names_prefix = "action_",
       values_to = "adj_cost"
     ) %>%
     mutate(action_full = recode(action, !!!invert(action_key))) %>%
     mutate(action_fac = factor(action_full, action_levels_spend, ordered = TRUE)) %>%
     mutate(source_fac = factor(project_source, source_levels_spend, ordered = TRUE)) %>%
     filter(action_full %in% action_levels_spend[1:16]) %>%
     filter(project_source %in% source_levels_spend[1:8])
  ) +
  aes(
    x = reorder(action_full, adj_cost)
  ) +
  ggtitle("Expenditures ($thous), by source and action type", "16 (of 82) action types with highest expenditures by \n8 (of 38) sources with highest expenditures") +
  facet_wrap(~ source_fac, ncol = 2)
fig_spend_source_action

# By source and basin
fig_spend_source_basin <-
  base_cost_bar %+%
  (df_prj %>%
     group_by(project_source, basin) %>%
     summarize(adj_cost = sum(adj_cost, na.rm = TRUE)) %>%
     drop_na() %>%
     mutate(basin_fac = factor(basin, basin_levels_spend, ordered = TRUE)) %>%
     mutate(source_fac = factor(project_source, source_levels_spend, ordered = TRUE)) %>%
     filter(basin %in% basin_levels_spend[1:8]) %>%
     filter(project_source %in% source_levels_spend[1:8])
  ) +
  aes(
    x = reorder(project_source, adj_cost)
  ) +
  facet_wrap(~ basin_fac, ncol = 2) +
  ggtitle("Expenditures ($thou), by source and basin", "8 (of 47) basins with highest expenditures and \n8 (of 38) sources with highest expenditures")
fig_spend_source_basin

# Spending per project ====
# By action
# Note these figures represent expenditures per project with reported costs
fig_spendproj_action <-
  base_cost_bar %+%
  (df_prj %>%
     drop_na(adj_cost) %>%
     ungroup() %>%
     mutate_at(
       vars(starts_with("action_")),
       as.numeric
     ) %>%
     mutate_at(
       vars(starts_with("action_")),
       list(~ .*adj_cost)
     ) %>%
     summarize_at(
       vars(starts_with("action_")),
       list(~sum(., na.rm = TRUE)/sum(I(. > 0), na.rm = TRUE))
     ) %>%
     pivot_longer(
       everything(),
       names_to = "action",
       names_prefix = "action_",
       values_to = "adj_cost"
     ) %>%
     mutate(action_full = recode(action, !!!invert(action_key))) %>%
     filter(action_full %in% action_levels[1:50])
  ) +
  aes(
    x = reorder(action_full, adj_cost)
  ) +
  ggtitle("Expenditures per project ($thou), by action type", "50 (of 82) most common action types")
fig_spendproj_action

# By year
fig_spendproj_year <-
  base_cost_line %+%
  (df_prj %>%
     drop_na(adj_cost) %>%
     group_by(completed_year) %>%
     summarize(adj_cost = sum(adj_cost, na.rm = TRUE)/ sum(!is.na(adj_cost)))
  ) +
  ggtitle("Expenditures per project ($thou), by year")
fig_spendproj_year

# By basin
fig_spendproj_basin <-
  base_cost_bar %+%
  (df_prj %>%
     drop_na(adj_cost) %>%
     group_by(basin) %>%
     summarize(adj_cost = sum(adj_cost, na.rm = TRUE)/ sum(!is.na(adj_cost))) %>%
     drop_na()
  ) +
  aes(
    x = reorder(basin, adj_cost)
  ) +
  ggtitle("Expenditures per project ($thou), by basin")
fig_spendproj_basin

# By source
fig_spendproj_source <-
  base_cost_bar %+%
  (df_prj %>%
     drop_na(adj_cost) %>%
     group_by(project_source) %>%
     summarize(adj_cost = sum(adj_cost, na.rm = TRUE) / sum(!is.na(adj_cost))) %>%
     drop_na()
  ) +
  aes(
    x = reorder(project_source, adj_cost)
  ) +
  ggtitle("Expenditures per project ($thou), by source")
fig_spendproj_source

# By year and action
fig_spendproj_year_action <-
  base_cost_line %+%
  (df_prj %>%
     drop_na(adj_cost) %>%
     ungroup() %>%
     mutate_at(
       vars(starts_with("action_")),
       as.numeric
     ) %>%
     mutate_at(
       vars(starts_with("action_")),
       list(~ .*adj_cost)
     ) %>%
     group_by(completed_year) %>%
     summarize_at(
       vars(starts_with("action_")),
       list(~sum(., na.rm = TRUE)/ sum(!is.na(adj_cost)))
     ) %>%
     pivot_longer(
       starts_with("action_"),
       names_to = "action",
       names_prefix = "action_",
       values_to = "adj_cost"
     ) %>%
     mutate(action_full = recode(action, !!!invert(action_key))) %>%
     mutate(action_fac = factor(action_full, action_levels_spend, ordered = TRUE)) %>%
     filter(action_full %in% action_levels_spend[1:18])
  ) +
  ggtitle("Expenditures per project ($thou), by year and action type", "18 (of 82) action types with highest expenditures") +
  facet_wrap(~ action_fac, ncol = 3)
fig_spendproj_year_action

# By year and basin
fig_spendproj_year_basin <-
  base_cost_line %+%
  (df_prj %>%
     drop_na(adj_cost) %>%
     group_by(completed_year, basin) %>%
     summarize(adj_cost = sum(adj_cost, na.rm = TRUE) / sum(!is.na(adj_cost))) %>%
     drop_na() %>%
     mutate(basin_fac = factor(basin, basin_levels_spend, ordered = TRUE)) %>%
     filter(basin %in% basin_levels_spend[1:12])
  ) +
  ggtitle("Expenditures per project ($thou), by year and basin", "12 (of 47) basins with highest expenditures") +
  facet_wrap(~ basin_fac, ncol = 3)
fig_spendproj_year_basin

# By year and source
fig_spendproj_year_source <-
  base_cost_line %+%
  (df_prj %>%
     drop_na(adj_cost) %>%
     group_by(completed_year, project_source) %>%
     summarize(adj_cost = sum(adj_cost, na.rm = TRUE) / sum(!is.na(adj_cost))) %>%
     drop_na() %>%
     mutate(source_fac = factor(project_source, source_levels_spend, ordered = TRUE)) %>%
     filter(project_source %in% source_levels_spend[1:8])
  ) +
  ggtitle("Expenditures per project ($thou), by year and source", "8 (of 38) sources with highest reported expenditures") +
  facet_wrap(~ source_fac, ncol = 2)
fig_spendproj_year_source

# By basin and action
fig_spendproj_basin_action <-
  base_cost_bar %+%
  (df_prj %>%
     drop_na(adj_cost) %>%
     ungroup() %>%
     mutate_at(
       vars(starts_with("action_")),
       as.numeric
     ) %>%
     mutate_at(
       vars(starts_with("action_")),
       list(~ .*adj_cost)
     ) %>%
     group_by(basin) %>%
     summarize_at(
       vars(starts_with("action_")),
       list(~sum(., na.rm = TRUE) / sum(!is.na(adj_cost)))
     ) %>%
     pivot_longer(
       starts_with("action_"),
       names_to = "action",
       names_prefix = "action_",
       values_to = "adj_cost"
     ) %>%
     mutate(action_full = recode(action, !!!invert(action_key))) %>%
     mutate(action_fac = factor(action_full, action_levels_spend, ordered = TRUE)) %>%
     mutate(basin_fac = factor(basin, basin_levels_spend, ordered = TRUE)) %>%
     filter(action_full %in% action_levels_spend[1:16]) %>%
     filter(basin %in% basin_levels_spend[1:8])
  ) +
  aes(
    x = reorder(action_full, adj_cost)
  ) +
  ggtitle("Expenditures per project ($thous), by basin and action type", "16 (of 82) action types with highest expenditures in \n8 (of 47) basins with highest expenditures") +
  facet_wrap(~ basin_fac, ncol = 2)
fig_spendproj_basin_action

# By source and action
fig_spendproj_source_action <-
  base_cost_bar %+%
  (df_prj %>%
     drop_na(adj_cost) %>%
     ungroup() %>%
     mutate_at(
       vars(starts_with("action_")),
       as.numeric
     ) %>%
     mutate_at(
       vars(starts_with("action_")),
       list(~ .*adj_cost)
     ) %>%
     group_by(project_source) %>%
     summarize_at(
       vars(starts_with("action_")),
       list(~sum(., na.rm = TRUE) / sum(!is.na(adj_cost)))
     ) %>%
     pivot_longer(
       starts_with("action_"),
       names_to = "action",
       names_prefix = "action_",
       values_to = "adj_cost"
     ) %>%
     mutate(action_full = recode(action, !!!invert(action_key))) %>%
     mutate(action_fac = factor(action_full, action_levels_spend, ordered = TRUE)) %>%
     mutate(source_fac = factor(project_source, source_levels_spend, ordered = TRUE)) %>%
     filter(action_full %in% action_levels_spend[1:16]) %>%
     filter(project_source %in% source_levels_spend[1:8])
  ) + 
  aes(
    x = reorder(action_full, adj_cost)
  ) +
  ggtitle("Expenditures per project ($thous), by source and action type", "16 (of 82) action types with highest expenditures by \n8 (of 38) sources with highest expenditures") +
  facet_wrap(~ source_fac, ncol = 2)
fig_spendproj_source_action

# By source and basin
fig_spendproj_source_basin <-
  base_cost_bar %+%
  (df_prj %>%
     drop_na(adj_cost) %>%
     group_by(project_source, basin) %>%
     summarize(adj_cost = sum(adj_cost, na.rm = TRUE) / sum(!is.na(adj_cost))) %>%
     drop_na() %>%
     mutate(basin_fac = factor(basin, basin_levels_spend, ordered = TRUE)) %>%
     mutate(source_fac = factor(project_source, source_levels_spend, ordered = TRUE)) %>%
     filter(basin %in% basin_levels_spend[1:8]) %>%
     filter(project_source %in% source_levels_spend[1:8])
  ) +
  aes(
    x = reorder(project_source, adj_cost)
  ) +
  facet_wrap(~ basin_fac, ncol = 2) +
  ggtitle("Expenditures per project ($thou), by source and basin", "8 (of 47) basins with highest expenditures and \n8 (of 38) sources with highest expenditures")
fig_spendproj_source_basin

# Spending per action ====
# Note these figures represent expenditures per action for projects with reported costs

# Prepare action count for joining denominator
df_actcount <-
  df_prj %>%
  drop_na(adj_cost) %>%
  ungroup() %>%
  mutate_at(
    vars(starts_with("action_")),
    as.numeric
  ) %>%
  mutate_at(
    vars(starts_with("action_")),
    list(n_action_act = ~ .*n_action_prj)
  ) %>%
  pivot_longer(
    ends_with("_n_action_act"),
    names_to = "action",
    names_prefix = "action_",
    values_to = "n_action_act"
  ) %>%
  mutate(
    action = str_remove(action, "_n_action_act")
  )

# Prep joining character vector for join by variables
join_actcount <-
  c(
    "project_id",
    "project_year",
    "completed_year",
    "latitude",
    "longitude",
    "basin",
    "project_source",
    "project_cost",
    "n_worksites",
    "n_action_prj",
    "n_action_prj_dist",
    "n_metric_prj",
    "n_metric_prj_dist",
    "same_action",
    "missing_metric",
    "action_sedireduce_sedicont",
    "action_agri_mgmt",
    "action_livestock_water",
    "action_uplandveg_slope",
    "action_livestock_fence",
    "action_uplandveg_plant",
    "action_riparian_plant",
    "action_riparian_fence",
    "action_sedireduce_roaddrain",
    "action_sedireduce_roadobli",
    "action_instream_barbs",
    "action_instream_bankstab",
    "action_instream_logjam",
    "action_fishpass_culvimp",
    "action_sedireduce_contstruc",
    "action_instream_rootwad",
    "action_riparian_watergap",
    "action_agri_structure",
    "action_instream_wooddebris",
    "action_riparian_weed",
    "action_livestock_graze",
    "action_wetland_imp",
    "action_riparian_forest",
    "action_uplandveg_invasive",
    "action_uplandveg_standmgmt",
    "action_instream_connect",
    "action_fishpass_culvrem",
    "action_sedireduce_roadrecon",
    "action_fishpass_barriers",
    "action_instream_boulder",
    "action_est_resthab",
    "action_wetland_restore",
    "action_fishpass_roadcross",
    "action_sedireduce_roadrelo",
    "action_sedireduce_roadcross",
    "action_land_wetland",
    "action_riparian_graze",
    "action_instream_gravel",
    "action_instream_offchannel",
    "action_instream_reconf",
    "action_instream_logweir",
    "action_flow_irriimp",
    "action_est_fillrem",
    "action_fishpass_ladderinst",
    "action_land_streambank",
    "action_instream_beaver",
    "action_instream_rockweir",
    "action_fishpass_ladderimp",
    "action_fishpass_diverdam",
    "action_fishpass_waysinst",
    "action_screen_inst",
    "action_screen_replace",
    "action_flow_waterlease",
    "action_fishpass_weirs",
    "action_est_dikebreach",
    "action_wqimp_toxic",
    "action_wetland_create",
    "action_riparian_liveexcl",
    "action_riparian_livewater",
    "action_fishpass_culvinst",
    "action_wetland_plant",
    "action_instream_plantrem",
    "action_est_dikereconf",
    "action_nutrient_carcanalog",
    "action_nutrient_carcplace",
    "action_nutrient_fert",
    "action_instream_predrem",
    "action_fishpass_rockford",
    "action_wqimp_refuse",
    "action_wetland_plantrem",
    "action_wetland_invasive",
    "action_est_invasive",
    "action_est_tidegate",
    "action_est_chanmod",
    "action_agri_tillage",
    "action_wqimp_flowcool",
    "action_est_create",
    "action_est_armormod",
    "action_projmaint_sitemaint",
    "action_wqimp_wastewater",
    "action_wqimp_manure",
    "action_wqimp_sewage",
    "adj_cost",
    "action"
  )

# By action
fig_spendact_action <-
  base_cost_bar %+%
  (df_prj %>%
     ungroup() %>%
     mutate_at(
       vars(starts_with("action_")),
       as.numeric
     ) %>%
     mutate_at(
       vars(starts_with("action_")),
       list(adj_cost_act = ~ .*adj_cost)
     ) %>%
     pivot_longer(
       ends_with("_adj_cost_act"),
       names_to = "action",
       names_prefix = "action_",
       values_to = "adj_cost_act"
     ) %>%
     mutate(
       action = str_remove(action, "_adj_cost_act")
     ) %>%
     left_join(
       df_actcount,
       by = join_actcount
     ) %>%
     group_by(action) %>%
     summarize(
       adj_cost = sum(adj_cost_act, na.rm = TRUE) / sum(n_action_act, na.rm = TRUE)
     ) %>%
     mutate(action_full = recode(action, !!!invert(action_key))) %>%
     filter(action_full %in% action_levels[1:50])
  ) +
  aes(
    x = reorder(action_full, adj_cost)
  ) +
  ggtitle("Expenditures per action ($thou), by action type", "50 (of 82) most common action types")
fig_spendact_action

# By year
fig_spendact_year <-
  base_cost_line %+%
  (df_prj %>%
     drop_na(adj_cost) %>%
     group_by(completed_year) %>%
     summarize(adj_cost = sum(adj_cost, na.rm = TRUE)/ sum(n_action_prj))
  ) +
  ggtitle("Expenditures per action ($thou), by year")
fig_spendact_year

# By basin
fig_spendact_basin <-
  base_cost_bar %+%
  (df_prj %>%
     drop_na(adj_cost) %>%
     group_by(basin) %>%
     summarize(adj_cost = sum(adj_cost, na.rm = TRUE)/ sum(n_action_prj)) %>%
     drop_na()
  ) +
  aes(
    x = reorder(basin, adj_cost)
  ) +
  ggtitle("Expenditures per action ($thou), by basin")
fig_spendact_basin

# By source
fig_spendact_source <-
  base_cost_bar %+%
  (df_prj %>%
     drop_na(adj_cost) %>%
     group_by(project_source) %>%
     summarize(adj_cost = sum(adj_cost, na.rm = TRUE) / sum(n_action_prj)) %>%
     drop_na()
  ) +
  aes(
    x = reorder(project_source, adj_cost)
  ) +
  ggtitle("Expenditures per action ($thou), by source")
fig_spendact_source

# By year and action
fig_spendact_year_action <-
  base_cost_line %+%
  (df_prj %>%
     drop_na(adj_cost) %>%
     ungroup() %>%
     mutate_at(
       vars(starts_with("action_")),
       as.numeric
     ) %>%
     mutate_at(
       vars(starts_with("action_")),
       list(adj_cost_act = ~ .*adj_cost)
     ) %>%
     pivot_longer(
       ends_with("_adj_cost_act"),
       names_to = "action",
       names_prefix = "action_",
       values_to = "adj_cost_act"
     ) %>%
     mutate(
       action = str_remove(action, "_adj_cost_act")
     ) %>%
     left_join(df_actcount, by = join_actcount) %>%
     group_by(action, completed_year) %>%
     summarize(
       adj_cost = sum(adj_cost_act, na.rm = TRUE) / sum(n_action_act, na.rm = TRUE)
     ) %>%
     mutate(action_full = recode(action, !!!invert(action_key))) %>%
     mutate(action_fac = factor(action_full, action_levels_spend, ordered = TRUE)) %>%
     filter(action_full %in% action_levels_spend[1:18])
  ) +
  ggtitle("Expenditures per action ($thou), by year and action type", "18 (of 82) action types with highest expenditures") +
  theme(
    strip.text = element_text(size = 8)
  ) +
  facet_wrap(~ action_fac, ncol = 3)
fig_spendact_year_action

# By year and basin
fig_spendact_year_basin <-
  base_cost_line %+%
  (df_prj %>%
     drop_na(adj_cost) %>%
     group_by(completed_year, basin) %>%
     summarize(adj_cost = sum(adj_cost, na.rm = TRUE) / sum(n_action_prj)) %>%
     drop_na() %>%
     mutate(basin_fac = factor(basin, basin_levels_spend, ordered = TRUE)) %>%
     filter(basin %in% basin_levels_spend[1:12])
  ) +
  ggtitle("Expenditures per action ($thou), by year and basin", "12 (of 47) basins with highest expenditures") +
  facet_wrap(~ basin_fac, ncol = 3)
fig_spendact_year_basin

# By year and source
fig_spendact_year_source <-
  base_cost_line %+%
  (df_prj %>%
     drop_na(adj_cost) %>%
     group_by(completed_year, project_source) %>%
     summarize(adj_cost = sum(adj_cost, na.rm = TRUE) / sum(n_action_prj)) %>%
     drop_na() %>%
     mutate(source_fac = factor(project_source, source_levels_spend, ordered = TRUE)) %>%
     filter(project_source %in% source_levels_spend[1:8])
  ) +
  ggtitle("Expenditures per action ($thou), by year and source", "8 (of 38) sources with highest reported expenditures") +
  facet_wrap(~ source_fac, ncol = 2)
fig_spendact_year_source

# By basin and action
fig_spendact_basin_action <-
  base_cost_bar %+%
  (df_prj %>%
     drop_na(adj_cost) %>%
     ungroup() %>%
     mutate_at(
       vars(starts_with("action_")),
       as.numeric
     ) %>%
     mutate_at(
       vars(starts_with("action_")),
       list(adj_cost_act = ~ .*adj_cost)
     ) %>%
     pivot_longer(
       ends_with("_adj_cost_act"),
       names_to = "action",
       names_prefix = "action_",
       values_to = "adj_cost_act"
     ) %>%
     mutate(
       action = str_remove(action, "_adj_cost_act")
     ) %>%
     left_join(df_actcount, by = join_actcount) %>%
     group_by(action, basin) %>%
     summarize(
       adj_cost = sum(adj_cost_act, na.rm = TRUE) / sum(n_action_act, na.rm = TRUE)
     ) %>%
     mutate(action_full = recode(action, !!!invert(action_key))) %>%
     mutate(action_fac = factor(action_full, action_levels_spend, ordered = TRUE)) %>%
     mutate(basin_fac = factor(basin, basin_levels_spend, ordered = TRUE)) %>%
     filter(action_full %in% action_levels_spend[1:16]) %>%
     filter(basin %in% basin_levels_spend[1:8])
  ) +
  aes(
    x = reorder(action_full, adj_cost)
  ) +
  ggtitle("Expenditures per action ($thous), by basin and action type", "16 (of 82) action types with highest expenditures in \n8 (of 47) basins with highest expenditures") +
  facet_wrap(~ basin_fac, ncol = 2)
fig_spendact_basin_action

# By source and action
fig_spendact_source_action <-
  base_cost_bar %+%
  (df_prj %>%
     drop_na(adj_cost) %>%
     ungroup() %>%
     mutate_at(
       vars(starts_with("action_")),
       as.numeric
     ) %>%
     mutate_at(
       vars(starts_with("action_")),
       list(adj_cost_act = ~ .*adj_cost)
     ) %>%
     pivot_longer(
       ends_with("_adj_cost_act"),
       names_to = "action",
       names_prefix = "action_",
       values_to = "adj_cost_act"
     ) %>%
     mutate(
       action = str_remove(action, "_adj_cost_act")
     ) %>%
     left_join(df_actcount, by = join_actcount) %>%
     group_by(action, project_source) %>%
     summarize(
       adj_cost = sum(adj_cost_act, na.rm = TRUE) / sum(n_action_act, na.rm = TRUE)
     ) %>%
     mutate(action_full = recode(action, !!!invert(action_key))) %>%
     mutate(action_fac = factor(action_full, action_levels_spend, ordered = TRUE)) %>%
     mutate(source_fac = factor(project_source, source_levels_spend, ordered = TRUE)) %>%
     filter(action_full %in% action_levels_spend[1:16]) %>%
     filter(project_source %in% source_levels_spend[1:8])
  ) +
  aes(
    x = reorder(action_full, adj_cost)
  ) +
  ggtitle("Expenditures per action ($thous), by source and action type", "16 (of 82) action types with highest expenditures by \n8 (of 38) sources with highest expenditures") +
  facet_wrap(~ source_fac, ncol = 2)
fig_spendact_source_action

# By source and basin
fig_spendact_source_basin <-
  base_cost_bar %+%
  (df_prj %>%
     drop_na(adj_cost) %>%
     group_by(project_source, basin) %>%
     summarize(adj_cost = sum(adj_cost, na.rm = TRUE) / sum(n_action_prj)) %>%
     drop_na() %>%
     mutate(basin_fac = factor(basin, basin_levels_spend, ordered = TRUE)) %>%
     mutate(source_fac = factor(project_source, source_levels_spend, ordered = TRUE)) %>%
     filter(basin %in% basin_levels_spend[1:8]) %>%
     filter(project_source %in% source_levels_spend[1:8])
  ) +
  aes(
    x = reorder(project_source, adj_cost)
  ) +
  facet_wrap(~ basin_fac, ncol = 2) +
  ggtitle("Expenditures per action ($thou), by source and basin", "8 (of 47) basins with highest expenditures and \n8 (of 38) sources with highest expenditures")
fig_spendact_source_basin