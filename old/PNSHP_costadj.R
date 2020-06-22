setwd("C:/Users/braeden.vandeynze/Documents/Salmon Cost Data/Analysis/data") # For Braeden; comment out for other users

library(quantmod)
library(tidyverse)
rm(list = ls())

# Import and clean worksite-level data ====
# Load worksite data
PNSHP <- read_csv("PNSHP_worksite-subtype.csv")
# N = 493,031 (worksite x metric x "verbose name")

#Make upper case and clean state names
char <- c("Basin", "Subbasin", "Watershed", "Subwatershed", "State", "County", "Project_Name", "Project_Source", "Units", "SubTypeName", "Metric")
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

# Remove duplicate rows for metric level data (eliminates cases where only distinguishing factor is "VerboseName")
same <- c("WorkSitePK", "Metric", "Units", "SubTypeName")
PNSHP_mtc <-
  PNSHP %>%
  distinct(WorkSitePK, Metric, Units, SubTypeName,
           .keep_all = TRUE)
# N = 153,066 (worksite x subtype x metric)


# Convert costs to 2018 dollars ====
# Grab CPI from St. Louis FED
getSymbols("GDPDEF", src='FRED', auto.assign = TRUE)

# Get annually
avg.infl <- apply.yearly(GDPDEF, mean)

# Conversion factor to 2018 dollars
cf <- as.numeric(avg.infl['2018'])/avg.infl

# Convert xts object to dataframe
cf.df <- data.frame(year = as.numeric(as.character(substr(index(cf), 1, 4))), coredata(cf))

# Merge with PNSHP and convert costs to 2018 dollars
PNSHP_mtc <-
  PNSHP_mtc %>%
  left_join(
    cf.df,
    by = c("Completed_Year" = "year")
  ) %>%
  mutate(
    Adj_Cost = Project_Cost * GDPDEF
  )



# Create project and worksite level data ====


# Add fields for number of worksites and subtypes per project
PNSHP_mtc <-
  PNSHP_mtc %>%
  group_by(ProjectPK) %>%
  mutate( # Needs checking for weird results
    NWorksites = n_distinct(WorkSitePK),
    NSubtypes = n_distinct(SubTypeName),
    NMetrics = n_distinct(Metric),
    NUnits = n_distinct(Units)
  )

# Remove duplicate rows for worksite level data
PNSHP_wrk <-
  PNSHP_mtc %>%
  distinct(WorkSitePK,
           .keep_all = TRUE)
# N = 87,845 (worksite)

# Remove duplicate rows for project level data
PNSHP_pjt <-
  PNSHP_wrk %>%
  distinct(ProjectPK,
           .keep_all = TRUE)
# N = 36,643 (project)



# Probe data and further cleaning ====
# Change table defaults to useNA = "ifany"
table <- function(..., useNA = "ifany") base::table(..., useNA = useNA)

attach(PNSHP_pjt)
# Histogram and table of worksites per project
qplot(NWorksites)
table(NWorksites)
# A handful of projects have more than 50 projects; set 50 projects as arbitrary scope cut off (for now)
# Before screening, 84% of project have one site, 7% have two, and <10% have three or more

# Histogram and table of subtypes per project
qplot(NSubtypes, binwidth = 1)
table(NSubtypes)
# Most projects have only one action; 71% have one, >90% have five or fewer

# Histogram and table of projects by year
qplot(Completed_Year, binwidth = 1)
table(Completed_Year)
# Lots of non-sensical data here with years far into the future (asperiational?); seems lik 1905 is some sort of default date; 6,574 projects with no year
# Will clear out 1905 years and restrict data to 1990-2019 (30 years including run-up to ESA listing, 2019 retained for compatibility with future data)
# Will retain projects with NULL years for now but use for panal estimators will be limited

# Count states and basins
table(State)
# Number of states: 4; vast majority of data from OR; 5,102 with no state assigned (can be recovered from worksite location?)
# Decide how to handle the (few) multi-state projects

table(Basin)
# Number of basins: 47; Top five basins: Southern Oregon Coastal, Puget Sound, Willamette, Northern Oregon Coastal, Lower Snake; 3,793 with no basin assigned

detach(PNSHP_pjt)

# Screen based on above criteria
PNSHP_pjt <-
  PNSHP_pjt %>%
  filter(
    NWorksites <= 50 &
      Completed_Year >= 1990 &
      Completed_Year <= 2019
  )
# 28,577 projects remain after screening

# Apply screen to worksite data using joining
PNSHP_wrk <-
  PNSHP_wrk %>%
  semi_join(PNSHP_pjt, by = "ProjectPK")
# 48,902 worksites after screen

# Apply screen to metric data using joining
PNSHP_mtc <-
  PNSHP_mtc %>%
  semi_join(PNSHP_pjt, by = "ProjectPK")
# 84,385 actions after screen

# Count project subtypes and metrics
table(PNSHP_mtc$SubTypeName)
# Number of subtypes: 79; Top five subtypes: Sediment Reduction - Road Drainage System Improvements, Riparian - Planting, Riparian - Fencing, Sediment Reduction - Road Obliteration, Instream - Large Woody Debris
# Add main type column
# Look for subtypes that might be directly comparable

table(PNSHP_mtc$Metric)
# Number of metrics: 20; Top five metrics: Length Treated, Area Treated, Count of Blockages, Cost at this Worksite (not really a metric and only for 971 sites), Proposed Discharge
# Metrics are frequently absent (66,525 missing values)

# Count frequency of cost data
is.na(PNSHP_pjt$Project_Cost) %>% sum
# 9,361 projects with no cost data, out of 28,577 projects (33% of projects with missing values)


# Repeat above with screened data ====
attach(PNSHP_pjt)

qplot(NWorksites, binwidth = 1)
qplot(NSubtypes, binwidth = 1)
qplot(Completed_Year, binwidth = 1) # Dramatic slowdown in projects after boom following listing

table(Completed_Year)
table(State) %>% sort
table(Basin) %>% sort

detach(PNSHP_pjt)

table(PNSHP_mtc$SubTypeName) %>% sort

