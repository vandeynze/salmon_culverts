# TITLE: Simple pilot models of culvert work costs
# AUTHOR: Braeden Van Deynze
# DATE: March, 2020
# INPUTS: "culverts_wrk_working.csv" data for pure culvert worksite data
# OUTPUTS: model estimates (unsaved)

# Prepeare environment ====
# Clear environment
rm(list = ls())

# Load libraries
library(janitor)
library(tidyverse)

# Set top-level working directory
wd <- "C:/Users/Braeden/Desktop/NOAA/Analysis"
# wd <- "C:/Users/braeden.vandeynze/Documents/Salmon Cost Project/Analysis/"  # For Braeden; comment out for other users
setwd(wd)

# Load and prepare culvert data ====
setwd(wd)
setwd("./output") # For Braeden; comment out for other users
df_culv <- read_csv("culverts_wrk_working.csv")


# Filter out cost per culvert outliers
quantile(df_culv$cost_per_culvert, na.rm = TRUE, probs = c(0, 0.01, 0.1, 0.25, 0.5, 0.75, 0.9, 0.99, 1), type = 8) %>% format(big.mark = ",", scientific = FALSE, digits = 0, trim = TRUE)
df_culv <-
  df_culv %>%
  drop_na(cost_per_culvert) %>%
  filter(cost_per_culvert > 2000, cost_per_culvert < 750000, completed_year > 1995)
# Drops 534 worksites

# Build project level data
df_culv_prj <-
  df_culv %>%
  distinct(
    project_id,
    cost_per_culvert,
    n_culverts,
    dist_mean,
    n_worksites, 
    action_fishpass_culvrem_prj,
    action_fishpass_culvinst_prj,
    project_source,
    basin,
    completed_year
  )


# Explanatory variable summaries ====
# Full data
df_culv_prj %>%
  summarise_at(
    vars(n_culverts, dist_mean),
    list(min, mean, median, max, sd)
  ) %>%
  rename_at(
    vars(ends_with("fn1")), list(~str_replace(., "_fn1", "-min"))
  ) %>%
  rename_at(
    vars(ends_with("fn2")), list(~str_replace(., "_fn2", "-mean"))
  ) %>%
  rename_at(
    vars(ends_with("fn3")), list(~str_replace(., "_fn3", "-median"))
  ) %>%
  rename_at(
    vars(ends_with("fn4")), list(~str_replace(., "_fn4", "-max"))
  ) %>%
  rename_at(
    vars(ends_with("fn5")), list(~str_replace(., "_fn5", "-sd"))
  ) %>%
  pivot_longer(
    everything(),
    names_to = c("variable", "stat"),
    names_sep = "-"
  ) %>%
  arrange(variable) %>%
  pivot_wider(
    names_from = "stat",
    values_from = "value"
  )

# Multiple worksites only
df_culv_prj %>%
  filter(n_worksites > 1) %>%
  summarise_at(
    vars(n_culverts, dist_mean),
    list(min, mean, median, max, sd)
  ) %>%
  rename_at(
    vars(ends_with("fn1")), list(~str_replace(., "_fn1", "-min"))
  ) %>%
  rename_at(
    vars(ends_with("fn2")), list(~str_replace(., "_fn2", "-mean"))
  ) %>%
  rename_at(
    vars(ends_with("fn3")), list(~str_replace(., "_fn3", "-median"))
  ) %>%
  rename_at(
    vars(ends_with("fn4")), list(~str_replace(., "_fn4", "-max"))
  ) %>%
  rename_at(
    vars(ends_with("fn5")), list(~str_replace(., "_fn5", "-sd"))
  ) %>%
  pivot_longer(
    everything(),
    names_to = c("variable", "stat"),
    names_sep = "-"
  ) %>%
  arrange(variable) %>%
  pivot_wider(
    names_from = "stat",
    values_from = "value"
  )

# Histogram of n_culverts
ggplot(data = df_culv_prj) +
  geom_bar(aes(n_culverts), fill = "forestgreen") +
  labs(x = "Number of culverts", y = NULL, title = "Frequency of projects by number of culverts") +
  scale_y_continuous(labels = scales::label_comma())

# Histogram of dist_mean
ggplot(data = df_culv_prj %>% filter(dist_mean > 0)) +
  geom_histogram(aes(dist_mean), bins = 30, fill = "forestgreen",  color = "white") +
  labs(x = "Mean distance between work sites, meters", y = NULL,
       title = "Frequency of projects by mean distance between work sites",
       subtitle = "Single worksite projects omitted") +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_x_continuous(labels = scales::label_comma())

ggplot(data = df_culv_prj %>% filter(dist_mean > 0)) +
  geom_histogram(aes(dist_mean), bins = 30, fill = "forestgreen",  color = "white") +
  labs(x = "Mean distance between work sites, meters (log scale)", y = NULL,
       title = "Frequency of projects by mean distance between work sites",
       subtitle = "Single work site projects omitted") +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_x_log10(labels = scales::label_comma())

# Frequency table of single vs. multi-worksite projects
df_culv_prj %>%
  mutate(multiwrk = I(n_worksites > 1)) %>%
  tabyl(multiwrk)
# 320 projects with multiple worksites in project

# Cor. of n_culverts and dist_mean
cor(df_culv_prj$n_culverts, df_culv_prj$dist_mean)

# Note dist_max also considered but highly correlated with dist_mean

# Pilot regressions ====
# No fixed effects, n_culverts only
lm(
  log(cost_per_culvert) ~
    n_culverts,
  df_culv_prj
) %>%
  summary
# More culverts in project, lower cost per culverts

# No fixed effects, distance only
lm(
  log(cost_per_culvert) ~
    dist_mean,
  df_culv_prj
) %>%
  summary
# Result likely due to high correlation with n_culverts

# No fixed effects, log distance only
lm(
  log(cost_per_culvert) ~
    log(dist_mean+1),
  df_culv_prj
) %>%
  summary
# Result likely due to high correlation with n_culverts

# No fixed effects, n_culverts and distance
lm(
  log(cost_per_culvert) ~
    n_culverts + dist_mean,
  df_culv_prj
) %>%
  summary
# Only n_culverts result survives

# No fixed effects, n_culverts and log distance
lm(
  log(cost_per_culvert) ~
    n_culverts + log(dist_mean+1),
  df_culv_prj
) %>%
  summary
# Log dist_mean significant and negative

# Full fixed effects, n_culverts only
lm(
  log(cost_per_culvert) ~
    n_culverts +
    action_fishpass_culvrem_prj + action_fishpass_culvinst_prj +
    project_source + basin + factor(completed_year),
  df_culv_prj
) %>%
  summary
# n_culverts result survives fixed effects

# Full fixed effects, distance only
lm(
  log(cost_per_culvert) ~
    dist_mean +
    action_fishpass_culvrem_prj + action_fishpass_culvinst_prj +
    project_source + basin + factor(completed_year),
  df_culv_prj
) %>%
  summary
# Still likely due to omitted n_culverts and high correlation

# Full fixed effects, log distance only
lm(
  log(cost_per_culvert) ~
    log(dist_mean+1) +
    action_fishpass_culvrem_prj + action_fishpass_culvinst_prj +
    project_source + basin + factor(completed_year),
  df_culv_prj
) %>%
  summary
# Still likely due to omitted n_culverts and high correlation

# Full fixed effects, n_culverts and distance
lm(
  log(cost_per_culvert) ~
    n_culverts + dist_mean +
    action_fishpass_culvrem_prj + action_fishpass_culvinst_prj +
    project_source + basin + factor(completed_year),
  df_culv_prj
) %>%
  summary
# n_culvert result remains the same, dist_mean insignificant

# Full fixed effects, n_culverts and log distance
lm(
  log(cost_per_culvert) ~
    n_culverts + log(dist_mean+1) +
    action_fishpass_culvrem_prj + action_fishpass_culvinst_prj +
    project_source + basin + factor(completed_year),
  df_culv_prj
) %>%
  summary
# Still wouldn't read into dist_mean result too much

# Okay how about when we include a dummy for when there is only one worksite...
# Full fixed effects, n_culverts and distance and worksites dummy
lm(
  log(cost_per_culvert) ~
    n_culverts + dist_mean + I(n_worksites == 1) +
    action_fishpass_culvrem_prj + action_fishpass_culvinst_prj +
    project_source + basin + factor(completed_year),
  df_culv_prj
) %>%
  summary
# n_culvert result remains the same, dist_mean insignificant, single worksite projects have higher costs

# Full fixed effects, n_culverts and log distance and worksites dummy (Preferred model)
lm(
  log(cost_per_culvert) ~
    n_culverts + log(dist_mean+1) + I(n_worksites == 1) +
    action_fishpass_culvrem_prj + action_fishpass_culvinst_prj +
    project_source + basin + factor(completed_year),
  df_culv_prj
) %>%
  summary
# n_culvert and single worksite dummy results remain, and now worksites further away from others in the project appear to be more expensive too, but statistically insignificant
# But magnitude of the effect is extremely small (doubling of distance leads to only 2.7% increase in cost per culvert on average; ten-times the distance only 10% increase in cost)

# Full fixed effects, n_culverts as dummies
lm(
  log(cost_per_culvert) ~
    I(n_culverts == 2) + I(n_culverts == 3) + I(n_culverts == 4) + I(n_culverts >= 5) +
    action_fishpass_culvrem_prj + action_fishpass_culvinst_prj +
    project_source + basin + factor(completed_year),
  df_culv_prj
) %>%
  summary
# Same n_culvert pattern

# Full fixed effects, n_culverts as dummies and distance
lm(
  log(cost_per_culvert) ~
    I(n_culverts == 2) + I(n_culverts == 3) + I(n_culverts == 4) + I(n_culverts >= 5) + dist_mean +
    action_fishpass_culvrem_wrk + action_fishpass_culvinst_wrk +
    project_source + basin + factor(completed_year),
  df_culv
) %>%
  summary
# Same n_culvert pattern, dist_mean insignificant

# Full fixed effects, n_culverts as dummies and log distance
lm(
  log(cost_per_culvert) ~
    I(n_culverts == 2) + I(n_culverts == 3) + I(n_culverts == 4) + I(n_culverts >= 5) + log(dist_mean+1) + 
    action_fishpass_culvrem_prj + action_fishpass_culvinst_prj +
    project_source + basin + factor(completed_year),
  df_culv_prj
) %>%
  summary
# Same n_culvert pattern, dist_mean insignificant

# Full fixed effects, n_culverts as dummies and distance, with single worksite dummy
lm(
  log(cost_per_culvert) ~
    I(n_culverts == 2) + I(n_culverts == 3) + I(n_culverts == 4) + I(n_culverts >= 5) + dist_mean + I(n_worksites == 1) +
    action_fishpass_culvrem_prj + action_fishpass_culvinst_prj +
    project_source + basin + factor(completed_year),
  df_culv_prj
) %>%
  summary
# Same n_culvert pattern, dist_mean insignificant

# Full fixed effects, n_culverts as dummies and log distance, with single worksite dummy
lm(
  log(cost_per_culvert) ~
    I(n_culverts == 2) + I(n_culverts == 3) + I(n_culverts == 4) + I(n_culverts >= 5) + log(dist_mean+1) + I(n_worksites == 1) +
    action_fishpass_culvrem_prj + action_fishpass_culvinst_prj +
    project_source + basin + factor(completed_year),
  df_culv_prj
) %>%
  summary
# Same n_culvert pattern, log dist_mean significant but magnitude is very small
# Also, some concerns about collinearity between n_worksites == 1 and n_culverts == 1
df_culv %>% tabyl(n_culverts, n_worksites)


# These models are estimated for completeness, but generally less informative
# No fixed effects, n_culverts as dummies
lm(
  log(cost_per_culvert) ~
    I(n_culverts == 2) + I(n_culverts == 3) + I(n_culverts == 4) + I(n_culverts >= 5),
  df_culv
) %>%
  summary
# Same n_culvert pattern

# No fixed effects, n_culverts as dummies and distance
lm(
  log(cost_per_culvert) ~
    I(n_culverts == 2) + I(n_culverts == 3) + I(n_culverts == 4) + I(n_culverts >= 5) + dist_mean,
  df_culv
) %>%
  summary
# Same n_culvert pattern, dist_mean has negative effect on costs (-2% cost per culvert for every additional 1km between worksites)

# No fixed effects, n_culverts as dummies and log distance
lm(
  log(cost_per_culvert) ~
    I(n_culverts == 2) + I(n_culverts == 3) + I(n_culverts == 4) + I(n_culverts >= 5) + log(dist_mean+1),
  df_culv
) %>%
  summary
# Same n_culvert pattern, same distance pattern as above

# No fixed effects, n_culverts as dummies and distance, with single worksite dummy
lm(
  log(cost_per_culvert) ~
    I(n_culverts == 2) + I(n_culverts == 3) + I(n_culverts == 4) + I(n_culverts >= 5) + dist_mean + I(n_worksites == 1),
  df_culv
) %>%
  summary
# Same n_culvert pattern but insignificant parameters, distance insignificant, single worksite dummy positive and signficant, doing a lot of the legwork here

# No fixed effects, n_culverts as dummies and log distance, with single worksite dummy
lm(
  log(cost_per_culvert) ~
    I(n_culverts == 2) + I(n_culverts == 3) + I(n_culverts == 4) + I(n_culverts >= 5) + log(dist_mean+1) + I(n_worksites == 1),
  df_culv
) %>%
  summary
# Frankly, these models are mostly uninformative due to differences in reporting between sources that are otherwise accounted for with fixed effects

# Let's check when we only look at the 807 worksites that are part of multiple worksite projects
# (Retained for completeness, but this method is less useful than the multi-worksite dummy method we just demonstrated)
# No fixed effects, n_culverts only, multi-worksites only
lm(
  log(cost_per_culvert) ~
    n_culverts,
  df_culv %>% filter(n_worksites > 1)
) %>%
  summary
# n_culverts result remains, but weaker

# No fixed effects, distance only, multi-worksites only
lm(
  log(cost_per_culvert) ~
    dist_mean,
  df_culv %>% filter(n_worksites > 1)
) %>%
  summary
# Same omitted variable issue, but weaker than in full data

# No fixed effects, log distance only, multi-worksites only
lm(
  log(cost_per_culvert) ~
    log(dist_mean+1),
  df_culv %>% filter(n_worksites > 1)
) %>%
  summary
# Insignificant

# No fixed effects, n_culverts and distance, multi-worksites only
lm(
  log(cost_per_culvert) ~
    n_culverts + dist_mean,
  df_culv %>% filter(n_worksites > 1)
) %>%
  summary
# n_culverts effect dominates

# No fixed effects, n_culverts and log distance, multi-worksites only
lm(
  log(cost_per_culvert) ~
    n_culverts + log(dist_mean+1),
  df_culv %>% filter(n_worksites > 1)
) %>%
  summary
# n_culverts result still dominates

# Full fixed effects, n_culverts only, multi-worksites only
lm(
  log(cost_per_culvert) ~
    n_culverts +
    action_fishpass_culvrem_wrk + action_fishpass_culvinst_wrk +
    project_source + basin + factor(completed_year),
  df_culv %>% filter(n_worksites > 1)
) %>%
  summary
# n_culverts result survives fixed effects

# Full fixed effects, distance only, multi-worksites only
lm(
  log(cost_per_culvert) ~
    dist_mean +
    action_fishpass_culvrem_wrk + action_fishpass_culvinst_wrk +
    project_source + basin + factor(completed_year),
  df_culv %>% filter(n_worksites > 1)
) %>%
  summary
# dist_mean insignificant

# Full fixed effects, log distance only, multi-worksites only
lm(
  log(cost_per_culvert) ~
    log(dist_mean+1) +
    action_fishpass_culvrem_wrk + action_fishpass_culvinst_wrk +
    project_source + basin + factor(completed_year),
  df_culv %>% filter(n_worksites > 1)
) %>%
  summary
# dist_mean insignificant when log transformed too

# Full fixed effects, n_culverts and distance, multi-worksites only
lm(
  log(cost_per_culvert) ~
    n_culverts + dist_mean +
    action_fishpass_culvrem_wrk + action_fishpass_culvinst_wrk +
    project_source + basin + factor(completed_year),
  df_culv %>% filter(n_worksites > 1)
) %>%
  summary
# For multi-worksite projects, n_culverts reduces per culvert costs, and weak evidence that when they are further apart costs rise

# Full fixed effects, n_culverts and log distance, multi-worksites only
lm(
  log(cost_per_culvert) ~
    n_culverts + log(dist_mean+1) +
    action_fishpass_culvrem_wrk + action_fishpass_culvinst_wrk +
    project_source + basin + factor(completed_year),
  df_culv %>% filter(n_worksites > 1)
) %>%
  summary
# Evidence is stronger when distance is log transformed


# Figures of results ====
# Fixed effects plots by reporting source
# Highlight preferred model

# Fixed effects plots by year
# Lines connecting fixed points
# Highlight preferred model

