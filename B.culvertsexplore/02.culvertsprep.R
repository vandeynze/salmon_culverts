# TITLE: Data preperation for culvert exploration
# AUTHOR: Braeden Van Deynze
# DATE: March, 2020
# INPUTS: "PNSHP_full_working.csv" for full reduced PNSHP data; "PNSHP_prj_working.csv" for data aggregated to project level
# OUTPUTS: "culverts_*_working.csv" data for pure culvert project data at different levels of aggregation

# Prepeare environment ====
# Clear environment
rm(list = ls())

# Load libraries
library(janitor)
library(tidyverse)
library(geosphere)
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

# This is a worksite version
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

# Drop full data
rm(df_prj, df_pnshp)

# Basic counts
df_culv %>% count() %>% pull() %>% format(big.mark = ",")
# (n = 7,006)
df_culv_prj %>% count() %>% pull() %>% format(big.mark = ",")
# (n = 3,107)

# Counts of cost and metric availability
df_culv_prj %>% tabyl(avail_class)
df_culv_prj %>% tabyl(cost_avail)
# 2,609 projects with costs, 820 explicitly with metrics

# Check full data for non-culvert actions
df_culv_all %>% tabyl(action) %>% arrange(desc(n))
# Damn there is a lot of "cross-contamination" going on here...

# Let's isolate "pure" projects, i.e. the ones that are just culvert projects
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

# Pure counts
df_culv_pure %>% count() %>% pull() %>% format(big.mark = ",")
# (n = 3,273)
df_culv_prj_pure %>% count() %>% pull() %>% format(big.mark = ",")
# (n = 2,072)

# Counts of cost and metric availability
df_culv_prj_pure %>% tabyl(avail_class)
df_culv_prj_pure %>% tabyl(cost_avail)
# 1,796 projects with costs, 609 explicitly with metrics

# And then we'll seperate out the rest of the "dirty" projects
df_culv_prj_dirty <-
  df_culv_prj %>%
  anti_join(
    df_culv_prj_pure,
    by = "project_id"
  )
df_culv_dirty <-
  df_culv_all %>%
  anti_join(
    df_culv_prj_pure,
    by = "project_id"
  )

# Counts of cost and metric availability
df_culv_prj_dirty %>% tabyl(avail_class)
df_culv_prj_dirty %>% tabyl(cost_avail)
# So it looks like we can recover as many as 813 projects with costs, which would bring our total to 2,609 projects, a 45% increase

# What kind of other actions are grouped with culverts?
df_culv_dirty %>% tabyl(action) %>% arrange(-n)
# Lots, but especially ROAD DRAINAGE SYSTEM IMPROVEMENTS (sedireduce_roaddrain) and ROAD OBLITERATION (sedireduce_roadobli)
# But before we figure out what to doe with these, let's develop a way to break down the multiple site and COUNT OF BLOCKAGES metric projects

# Counting culverts for pure projects ====
# To start, let's take a look at how many projects have multiple worksites and how many have multiple actions
df_culv_prj_pure %>% tabyl(n_worksites)
df_culv_prj_pure %>% tabyl(n_action_prj)
# So it's a few hundred in each category, but likely a lot of overlap
# The goal here is to get a consistent $/culvert and # of culverts variable for each project
# We can then project the $/culvert variable onto each worksite, and use the geolocated worksites as the unit of analysis

# Lets start by building the # of culverts variable
# For projects that don't have the COUNT OF BLOCKAGES metric, that's straight forward: it's just the number of actions
# But we don't have any indicator in the project level data that identifies where COUNT OF BLOCKAGES is yet, so let's make one
df_culv_pure <-
  df_culv_pure %>%
  group_by(project_id) %>%
  mutate(
    count_avail = I(sum(I(metric == "COUNT OF BLOCKAGES"), na.rm = TRUE) > 0)
  )
df_culv_prj_pure <-
  df_culv_prj_pure %>%
  left_join(
    df_culv_pure %>% distinct(project_id, count_avail),
    by = "project_id"
  )
df_culv_prj_pure %>% tabyl(count_avail)
# Now the pure culvert project data has a variable "count_avail" that is TRUE when one or more of the actions has the COUNT OF BLOCKAGES metric

# Let's add a culvert count for the ones that don't
df_culv_prj_pure <-
  df_culv_prj_pure %>%
  mutate(
    n_culverts = case_when(
      count_avail == FALSE ~ n_action_prj,
      TRUE ~ NA_real_
    )
  )
df_culv_prj_pure %>% tabyl(n_culverts)
# That leaves us with 285 projects with the COUNT OF BLOCKAGES metric that we'll need to address
# Let's break them out in project and action form to take a closer look
df_culv_prj_count <-
  df_culv_prj_pure %>%
  filter(
    count_avail == TRUE
  )
df_culv_count <-
  df_culv_pure %>%
  ungroup() %>%
  semi_join(
    df_culv_prj_count,
    by = "project_id"
  )

# Lets check for what other types of metrics we're dealing with here
df_culv_count %>% tabyl(metric)
# Okay so only 15 of the 764 action x metric rows have NA metrics while 389 have COUNT OF BLOCKAGES

# The ones that have only one action should be pretty easy to address, so let's update them
df_culv_count %>%
  filter(
    n_action_prj == 1,
    metric == "COUNT OF BLOCKAGES"
  ) %>%
  select(
    project_id,
    worksite_id,
    action,
    metric,
    numeric_value,
    units
  ) %>%
  sample_n(5, weight = numeric_value) %>%
  view

df_culv_count <-
  df_culv_count %>%
  mutate(
    n_culverts_temp = case_when(
      metric != "COUNT OF BLOCKAGES" ~ 0, # This so we can later sum over this variable later to eliminate non-count metrics
      metric == "COUNT OF BLOCKAGES" & n_action_prj == 1 ~ numeric_value,
      TRUE ~ NA_real_
    )
  )
df_culv_count %>% tabyl(n_culverts_temp)
# So there's 156 more action x metric rows we have to deal with, and we should probably take a look at those ones with huge numbers of blockages too

# Taking a look at the ones we still have to take care of...
# df_culv_count %>%
#   filter(is.na(n_culverts_temp)) %>%
#   select(
#     project_id,
#     worksite_id,
#     action,
#     metric,
#     numeric_value,
#     n_metric,
#     n_metric_wrk,
#     n_metric_wrk_dist,
#     n_metric_prj,
#     n_metric_prj_dist,
#     n_action_wrk,
#     n_action_prj,
#     n_metric_prj_dist,
#     n_worksites
#   ) %>%
#   view

# For many, COUNT OF BLOCKAGES is exactly equal to the number of actions, so we can work with that 
df_culv_count <-
  df_culv_count %>%
  mutate(
    n_culverts_temp = case_when(
      !is.na(n_culverts_temp) ~ n_culverts_temp,
      numeric_value == n_action_prj ~ 1,
      TRUE ~ NA_real_
    )
  )
df_culv_count %>% tabyl(n_culverts_temp)

# Okay now we're down to 99 cases
# df_culv_count %>%
#   filter(is.na(n_culverts_temp)) %>%
#   select(
#     project_id,
#     worksite_id,
#     action,
#     metric,
#     numeric_value,
#     n_metric,
#     n_metric_wrk,
#     n_metric_wrk_dist,
#     n_metric_prj,
#     n_metric_prj_dist,
#     n_action_wrk,
#     n_action_prj,
#     n_metric_prj_dist,
#     n_worksites
#   ) %>%
#   view

# In a lot of other cases, COUNT OF BLOCKAGES is 1, and the number of worksites exactly equals the number of actions, so we can use that
df_culv_count <-
  df_culv_count %>%
  mutate(
    n_culverts_temp = case_when(
      !is.na(n_culverts_temp) ~ n_culverts_temp,
      numeric_value == 1 & n_action_prj == n_worksites ~ 1,
      TRUE ~ NA_real_
    )
  )
df_culv_count %>% tabyl(n_culverts_temp)
# Down to 67

# Now let's check out the weird cases where metric is NA for some worksites (there should be 15 rows with metric NA)
# df_culv_count %>%
#   group_by(project_id) %>%
#   mutate(
#     na_metric_prj = I(sum(is.na(metric), na.rm = TRUE) > 0) # Identifies when a project has a NA metric
#   ) %>%
#   filter(na_metric_prj == TRUE) %>%
#   select(
#     project_id,
#     worksite_id,
#     action,
#     metric,
#     numeric_value,
#     n_culverts_temp,
#     n_metric,
#     n_metric_wrk,
#     n_metric_wrk_dist,
#     n_metric_prj,
#     n_metric_prj_dist,
#     n_action_wrk,
#     n_action_prj,
#     n_metric_prj_dist,
#     n_worksites
#   ) %>%
#   view
# So it looks like in all of these cases we can assign a 1 in the culvrt count column to the NAs
df_culv_count <-
  df_culv_count %>%
  group_by(project_id) %>%
  mutate(
    na_metric_prj = I(sum(is.na(metric), na.rm = TRUE) > 0) # Identifies when a project has a NA metric
  ) %>%
  ungroup() %>%
  mutate(
    n_culverts_temp = case_when(
      !is.na(n_culverts_temp) ~ n_culverts_temp,
      na_metric_prj == TRUE ~ 1,
      TRUE ~ NA_real_
    )
  ) %>%
  select(-na_metric_prj)
df_culv_count %>% tabyl(n_culverts_temp)  
# We've got it down to 49

# Let's look at what's left
# df_culv_count %>%
#   group_by(project_id) %>%
#   mutate(
#     na_culv_prj = I(sum(is.na(n_culverts_temp), na.rm = TRUE) > 0) # Identifies when a project has a NA culvert count
#   ) %>%
#   filter(na_culv_prj == TRUE) %>%
#   select(
#     project_id,
#     worksite_id,
#     action,
#     metric,
#     numeric_value,
#     n_culverts_temp,
#     adj_cost,
#     n_metric,
#     n_metric_wrk,
#     n_metric_wrk_dist,
#     n_metric_prj,
#     n_metric_prj_dist,
#     n_action_wrk,
#     n_action_prj,
#     n_metric_prj_dist,
#     n_worksites
#   ) %>%
#   view
# These all look ambiguous, so let's drop them and move along
df_culv_count <-
  df_culv_count %>%
  group_by(project_id) %>%
  mutate(
    na_culv_prj = I(sum(is.na(n_culverts_temp), na.rm = TRUE) > 0) # Identifies when a project has a NA culvert count
  ) %>%
  ungroup() %>%
  filter(na_culv_prj == FALSE) %>%
  select(-na_culv_prj)

# We'll now merge the culvert counts into the project data
df_culv_prj_count <-
  df_culv_prj_count %>%
  left_join(
    df_culv_count %>%
      group_by(project_id) %>%
      summarize(
        n_culverts_temp = sum(n_culverts_temp)
      ),
    by = "project_id"
  ) %>%
  mutate(
    n_culverts = case_when(
      !is.na(n_culverts) ~ n_culverts,
      is.na(n_culverts) ~ n_culverts_temp
    ),
    n_culverts_temp = NULL,
    n_culverts_fromcount = 1
  ) %>%
  filter(
    !is.na(n_culverts)
  )

# And we'll merge that back into the pure culvert projects
df_culv_prj_pure <-
  df_culv_prj_pure %>%
  left_join(
    df_culv_prj_count %>%
      distinct(
        project_id,
        n_culverts,
        n_culverts_fromcount
      ),
    by = "project_id",
    suffix = c("", "_temp")
  ) %>%
  mutate(
    n_culverts = case_when(
      !is.na(n_culverts) ~ n_culverts,
      is.na(n_culverts) ~ n_culverts_temp
    ),
    n_culverts_temp = NULL,
    n_culverts_fromcount = case_when(
      is.na(n_culverts_fromcount) ~ 0,
      TRUE ~ 1
    )
  ) %>%
  drop_na(n_culverts) %>%
  # And create our $/culvert variable
  mutate(
    cost_per_culvert = adj_cost / n_culverts
  ) %>%
  # And at this point, we can drop the extra columns as well to prepare for export
  select(
    -(starts_with("action_") & !contains("culv")),
    -project_year,
    -same_action,
    -missing_metric,
    -n_action_class,
    -count_avail,
    -metric_avail,
    -both_avail,
    -avail_class
  )
# And here's a list of those projects
ls_culv_prj_pure <-
  df_culv_prj_pure %>%
  distinct(project_id) %>%
  pull(project_id)

# We should build these for action and worksite level data as well
df_culv_pure <-
  df_culv_pure %>%
  ungroup() %>%
  filter(
    project_id %in% ls_culv_prj_pure
  ) %>%
  left_join(
    df_culv_prj_pure %>% select(project_id, n_culverts, starts_with("action"), n_culverts, n_culverts_fromcount, cost_per_culvert),
    by = "project_id",
    suffix = c("", "_prj")
  ) %>%
  select(
    -(starts_with("action_") & !contains("culv")),
    -project_year,
    -same_action,
    -missing_metric,
    -n_action_class,
    -count_avail,
    -metric_avail,
    -both_avail,
    -avail_class,
    -metric,
    -numeric_value,
    -units
  ) %>%
  distinct(
    project_id,
    worksite_id,
    action,
    .keep_all = TRUE
  )

df_culv_wrk_pure <-
  df_culv_pure %>%
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

# Adding distance variables ====
# Here we calculate the mean and maximum distance between worksites at the project level
df_culv_wrk_pure <-
  df_culv_wrk_pure %>%
  mutate(
    dist_max = map_dbl(
      project_id,
      ~ max(
        distm(
          df_culv %>% filter(project_id == .x) %>% select(longitude, latitude)
        )
      )
    ),
    dist_mean = map_dbl(
      project_id,
      ~ mean(
        distm(
          df_culv %>% filter(project_id == .x) %>% select(longitude, latitude)
        )
      )
    )
  )

# And merge these variables back into the project-level data
df_culv_prj_pure <-
  df_culv_prj_pure %>%
  left_join(
    df_culv_wrk_pure %>%
      ungroup() %>%
      distinct(
        project_id,
        dist_max,
        dist_mean
      ),
    by = "project_id"
  )


# Export for future use ====
# Key data is df_culv_pure, df_culv_wrk_pure, df_culv_prj_pure

write_csv(df_culv_pure, here("/output/culverts_full_working.csv"))
write_csv(df_culv_wrk_pure, here("/output/culverts_wrk_working.csv"))
write_csv(df_culv_prj_pure, here("/output/culverts_prj_working.csv"))
