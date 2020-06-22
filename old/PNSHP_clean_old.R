# Purpose: Clean and explore PNSHP cost data for availability and basic properties
# Output: (1) Figures and tables summarizing data availability at different levels; (2) Clean files at project, worksite, and action levels


setwd("C:/Users/braeden.vandeynze/Documents/Salmon Cost Project/Analysis/data") # For Braeden; comment out for other users

library(quantmod)
library(tidyverse)
library(knitr)
rm(list = ls())

# Import and clean worksite-level data ====
# Load worksite data

PNSHP <-
  read_csv(
    "PNSHP_worksite-subtype_20200113.csv",
    col_types = cols_only(
      ProjectPK = "c",
      WorksitePK = "c",
      SubTypeName = "c",
      Mapcat = "c",
      Project_Source = "c",
      Project_Year = "d",
      Completed_Year = "d",
      Project_Cost = "d",
      Latitude = "d",
      Longitude = "d",
      State = "c",
      County = "c",
      Basin = "c",
      Metric = "c",
      NumericValue = "d",
      Units = "c"
    )
  ) %>%
  select(
    ProjectPK,
    WorksitePK,
    Project_Year,
    Completed_Year,
    Latitude,
    Longitude,
    County,
    State,
    Basin,
    Project_Source,
    TypeName = Mapcat,
    SubTypeName,
    Project_Cost,
    Metric,
    NumericValue,
    Units
  )
# N = 118,189 (worksite x subtype x metric, or action level)

# Make upper case and clean state names
char <- c("Basin", "State", "County", "Project_Source", "TypeName", "SubTypeName")
PNSHP <-
  PNSHP %>%
  mutate_at(
    char,
    str_to_upper 
  ) %>%
  mutate(
    StateFull = case_when(
      State == "IDAHO" ~ "ID",
      State == "MONTANA" ~ "MT",
      State == "OREGON" ~ "OR",
      State == "WASHINGTON" ~ "WA",
      State == "<NULL>" ~ NA_character_,
      TRUE ~ State
    ),
    State = case_when(
      StateFull == "ID,WY" ~ "ID",
      StateFull == "ID,OR" ~ "ID",
      StateFull == "CN,WA" ~ "WA",
      StateFull == "CA,OR" ~ "OR",
      StateFull == "ID,OR,WA" ~ "ID",
      StateFull == "NV,OR" ~ "OR",
      StateFull == "OR,WA" ~ "OR",
      StateFull == "ID,WA" ~ "ID",
      TRUE ~ StateFull
    )
  )
# Will need to check for primary state, multistate projects later, but just picking first state from main states as check (states presented alphedbetically so this is not a good fix)


rm(char)

# Convert costs to 2019 dollars ====
# Grab CPI from St. Louis FED
getSymbols("GDPDEF", src='FRED', auto.assign = TRUE)

# Get annually
avg.infl <- apply.yearly(GDPDEF, mean)

# Conversion factor to 2019 dollars
cf <- as.numeric(avg.infl['2019'])/avg.infl

# Convert xts object to dataframe
cf.df <- data.frame(year = as.numeric(as.character(substr(index(cf), 1, 4))), coredata(cf))

# Merge with PNSHP and convert costs to 2018 dollars
PNSHP <-
  PNSHP %>%
  left_join(
    cf.df,
    by = c("Completed_Year" = "year")
  ) %>%
  mutate(
    Adj_Cost = Project_Cost * GDPDEF
  )

rm(avg.infl, cf, cf.df, GDPDEF)


# Create project and worksite level data ====
# Remove duplicate rows for metric level data (eliminates cases where only distinguishing factor is "VerboseName")

# Add fields for number of worksites and subtypes per project
PNSHP <-
  PNSHP %>%
  group_by(ProjectPK) %>%
  mutate(
    NWorksites = n_distinct(WorksitePK),
    NSubtypes = n_distinct(SubTypeName)
  )

PNSHP_act <-
  PNSHP %>%
  distinct(ProjectPK, WorksitePK, SubTypeName,
           .keep_all = TRUE)
# N = 111,089 (worksite x subtype)



# Remove duplicate rows for worksite level data
PNSHP_wrk <-
  PNSHP_act %>%
  distinct(ProjectPK, WorksitePK,
           .keep_all = TRUE)
# N = 103,858 (worksites)

# Remove duplicate rows for project level data
PNSHP_pjt <-
  PNSHP_wrk %>%
  distinct(ProjectPK,
           .keep_all = TRUE)
# N = 45,436 (projects)



# Probe data and further cleaning ====
# Change table defaults to useNA = "ifany"
table <- function(..., useNA = "ifany") base::table(..., useNA = useNA)

attach(PNSHP_pjt)
# Histogram and table of worksites per project
qplot(NWorksites)
table(NWorksites)
# A handful of projects have more than 50 projects; set 50 projects as arbitrary scope cut off (for now)
# Before screening, ~85% of project have one site, <10% have two, and <5% have three or more

# Histogram and table of subtypes per project
qplot(NSubtypes, binwidth = 1)
table(NSubtypes)
# Most projects have only one action; 95% have one

# Histogram and table of projects by year
qplot(Completed_Year, binwidth = 1)
table(Completed_Year)
# Lots of non-sensical data here with years far into the future (asperiational?); seems lik 1905 is some sort of default date; 6,724 projects with no year
# Will clear out 1905 years and restrict data to 1990-2019 (30 years including run-up to ESA listing, 2019 retained for compatibility with future data)
# Will retain projects with NULL years for now but use for panal estimators will be limited

# Count states and basins
table(State)
# Number of states: 4; vast majority of data from OR; 9,411 with no state assigned (can be recovered from worksite location?)
# Decide how to handle the (few) multi-state projects

table(Basin)
# Number of basins: 46; Top five basins: Southern Oregon Coastal, Puget Sound, Willamette, Northern Oregon Coastal, Lower Snake; 9,206 with no basin assigned
# Basin and state data can be recovered from Lat/Long worksite coordinates for possible panel estimators

detach(PNSHP_pjt)

# Screen based on above criteria
PNSHP_pjt <-
  PNSHP_pjt %>%
  filter(
    NWorksites <= 50 &
      Completed_Year >= 1990 &
      Completed_Year <= 2019
  )
# 37,208 projects remain after screening

# Apply screen to worksite data using joining
PNSHP_wrk <-
  PNSHP_wrk %>%
  semi_join(PNSHP_pjt, by = "ProjectPK")
# 64,735 worksites after screen

# Apply screen to action level data using joining
PNSHP_act <-
  PNSHP_act %>%
  semi_join(PNSHP_pjt, by = "ProjectPK")
# 70,281 actions after screen

# Apply screen to metric level using joining
PNSHP <-
  PNSHP %>%
  semi_join(PNSHP_pjt, by = "ProjectPK")
# 75,869 action x metric observations after screening (some actions with multiple metrics)


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

# Save out data for further use ====
write_csv(PNSHP_pjt, "PNSHP_pjt.csv")
write_csv(PNSHP_wrk, "PNSHP_wrk.csv")
write_csv(PNSHP_act, "PNSHP_act.csv")
write_csv(PNSHP, "PNSHP_clean.csv")
