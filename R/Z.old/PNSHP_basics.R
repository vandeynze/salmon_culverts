# Purpose: Explore basic properties of raw PNSHP cost data
# Outputs: Figures and tables describign PNSHP cost data by action type, basin, year, etc.

setwd("C:/Users/braeden.vandeynze/Documents/Salmon Cost Project/Analysis/data") # For Braeden; comment out for other users

library(quantmod)
library(tidyverse)
library(knitr)
rm(list = ls())


# Load data at project, worksite, action, metric level ====
PNSHP_pjt <- read_csv("PNSHP_pjt.csv", guess_max = 10000)
PNSHP_wrk <- read_csv("PNSHP_wrk.csv", guess_max = 10000)
PNSHP_act <- read_csv("PNSHP_act.csv", guess_max = 10000)
PNSHP <- read_csv("PNSHP_clean.csv", guess_max = 10000)

# Tabulate projects by various descriptors ====
# Set table function to always show NA when present
table <- function(..., useNA = "ifany") base::table(..., useNA = useNA)

# Count project subtypes and metrics
table(PNSHP_act$SubTypeName)
# Number of subtypes: 75; Top five subtypes: Sediment Reduction - Road Drainage System Improvements, Riparian - Planting, Riparian - Fencing, Instream - Large Woody Debris, Sediment Reduction - Road Obliteration
# Look for subtypes that might be directly comparable

# Count frequency of cost data
(!is.na(PNSHP_pjt$Project_Cost)) %>% sum
(!is.na(PNSHP_wrk$Project_Cost)) %>% sum
(!is.na(PNSHP_act$Project_Cost)) %>% sum

(!is.na(PNSHP_wrk$Latitude)) %>% sum

# 13,001 projects with no cost data, out of 37,210 projects (~33% of projects with missing values)

# Repeat above with screened data ====
attach(PNSHP_pjt)

qplot(NWorksites, binwidth = 1) + ggtitle("Number of Worksites per project")
qplot(NSubtypes, binwidth = 1) + ggtitle("Number of action subtypes per project")
qplot(Completed_Year, binwidth = 1) + ggtitle("Projects by year")
# Dramatic slowdown in projects after boom following listing; not sure how much is due to lag in data entry in later years (i.e. '18/'19)

table(Completed_Year)
table(State) %>% sort
table(Basin) %>% sort
table(Project_Source) %>% sort

# Some descriptives of project costs
summary(Project_Cost)
summary(Project_Cost/NWorksites)
# Some extremely expensive projects here; max project cost is 36mil, with only one worksite, but several with over 10mil in costs
# Should also think about the "zero" cost projects; are they funded by in-kind donations or are they just nulls

qplot(Project_Cost, xlim = c(1, 1000000)) # Limits set to exclude outliers and ambiguous zeros
qplot(Project_Cost/NWorksites, xlim = c(1, 1000000))
qplot(y = Project_Cost/NWorksites, geom = "boxplot")
qplot(y = log(Project_Cost/NWorksites), geom = "boxplot")
qplot(x = Project_Cost/NWorksites, geom = "density")
qplot(x = log(Project_Cost/NWorksites), geom = "density") # Costs essentially have a lognormal distribution, and justification could be made for dropping 0 cost projects

detach(PNSHP_pjt)

table(PNSHP_act$SubTypeName) %>% sort
qplot(
  x = Completed_Year,
  y = n,
  color = SubTypeName,
  geom = "line",
  data = PNSHP_act %>%
    filter(!is.na(SubTypeName)) %>%
    group_by(SubTypeName) %>%
    mutate(n = n()) %>%
    ungroup() %>%
    filter(dense_rank(desc(n)) <= 10) %>%
    group_by(Completed_Year, SubTypeName) %>%
    summarize(n = n())
) +
  ggtitle("Projects by year")


# Generate summary reports ====
options(tibble.print_max = Inf)

# Action subtype counts
PNSHP_summ_action <-
  PNSHP_act %>%
  group_by(SubTypeName) %>%
  summarize(
    NProjects = n_distinct(ProjectPK),
    NWorksites = n_distinct(WorksitePK, ProjectPK)
  ) %>%
  arrange(desc(NProjects)) %>%
  left_join(
    PNSHP_act %>%
      filter(!is.na(Project_Cost)) %>%
      group_by(SubTypeName) %>%
      summarize(
        NProjectsCosts = n_distinct(ProjectPK),
        NWorksitesCosts = n_distinct(WorksitePK, ProjectPK)
      )
  )
PNSHP_summ_action 

# Year counts
PNSHP_summ_year <-
  PNSHP_act %>%
  group_by(Completed_Year) %>%
  summarize(
    NProjects = n_distinct(ProjectPK),
    NWorksites = n_distinct(WorksitePK, ProjectPK)
  ) %>%
  arrange(Completed_Year) %>%
  left_join(
    PNSHP_act %>%
      filter(!is.na(Project_Cost)) %>%
      group_by(Completed_Year) %>%
      summarize(
        NProjectsCosts = n_distinct(ProjectPK),
        NWorksitesCosts = n_distinct(WorksitePK, ProjectPK)
      )
  )

# State counts
PNSHP_summ_state <-
  PNSHP_act %>%
  group_by(State) %>%
  summarize(
    NProjects = n_distinct(ProjectPK),
    NWorksites = n_distinct(WorksitePK, ProjectPK)
  ) %>%
  arrange(desc(NProjects)) %>%
  left_join(
    PNSHP_act %>%
      filter(!is.na(Project_Cost)) %>%
      group_by(State) %>%
      summarize(
        NProjectsCosts = n_distinct(ProjectPK),
        NWorksitesCosts = n_distinct(WorksitePK, ProjectPK)
      )
  )

# Basin counts
PNSHP_summ_basin <-
  PNSHP_act %>%
  group_by(Basin) %>%
  summarize(
    NProjects = n_distinct(ProjectPK),
    NWorksites = n_distinct(WorksitePK, ProjectPK)
  ) %>%
  arrange(desc(NProjects)) %>%
  left_join(
    PNSHP_act %>%
      filter(!is.na(Project_Cost)) %>%
      group_by(Basin) %>%
      summarize(
        NProjectsCosts = n_distinct(ProjectPK),
        NWorksitesCosts = n_distinct(WorksitePK, ProjectPK)
      )
  )


# Cost summaries by action
PNSHP_summ_cost_act <-
  PNSHP_act %>%
  group_by(SubTypeName) %>%
  summarize(
    Cost_Mean = mean(Project_Cost/NWorksites, na.rm = TRUE),
    Cost_Med = median(Project_Cost/NWorksites, na.rm = TRUE),
    Cost_StdDev = sd(Project_Cost/NWorksites, na.rm = TRUE),
    N = n()
  ) %>%
  arrange(desc(N))
PNSHP_summ_cost_act


# Cost summaries by basin
PNSHP_summ_cost_basin <-
  PNSHP_pjt %>%
  group_by(Basin) %>%
  summarize(
    Cost_Mean = mean(Adj_Cost/NWorksites, na.rm = TRUE),
    Cost_Med = median(Adj_Cost/NWorksites, na.rm = TRUE),
    Cost_StdDev = sd(Adj_Cost/NWorksites, na.rm = TRUE),
    N = n()
  ) %>%
  arrange(desc(N))
PNSHP_summ_cost_basin

# List 10 most common action subtypes and 10 most common basins
subtypes <-
  count(PNSHP_act %>% ungroup %>% filter(!is.na(SubTypeName)), SubTypeName, sort = TRUE) %>% slice(1:10) %>% select(SubTypeName) %>% pull()
basins <-
  count(PNSHP_act %>% ungroup %>% filter(!is.na(Basin)), Basin, sort = TRUE) %>% slice(1:10) %>% select(Basin) %>% pull()

# Density plots for costs
library(ggthemes)

PNSHP_wrk %>%
  ungroup %>%
  filter(Adj_Cost > 0 & NWorksites == 1 & SubTypeName %in% subtypes[1:10] & Basin %in% basins[1:5]) %>%
  count(SubTypeName, Metric) %>%
  print(n = 30)

plot_costs <-
  ggplot(
    PNSHP_wrk %>% filter(Adj_Cost > 0 & NWorksites == 1 & SubTypeName %in% subtypes[1:10] & Basin %in% basins[1:10] & Metric != "Cost at this Worksite"),
    aes(
      Adj_Cost/NumericValue,
      fill = SubTypeName,
      linetype = Metric
    )
  ) +
  geom_density(
    alpha = 0.2,
    bw = 0.15
  ) +
  facet_wrap(
    vars(SubTypeName),
    scales = "free_y",
    ncol = 1
  ) +
  theme_clean() +
  theme(
    legend.position = "bottom"
  ) +
  scale_fill_hue(guide = "none") +
  scale_x_log10(labels = scales::label_dollar(), breaks = c(0, 10, 1000, 10000, 100000, 1000000, 10000000))
plot_costs
