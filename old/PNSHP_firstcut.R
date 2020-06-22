setwd("C:/Users/braeden.vandeynze/Documents/Salmon Cost Data/Analysis/data") # For Braeden; comment out for other users

library(quantmod)
library(tidyverse)
library(knitr)
rm(list = ls())

# Import and clean worksite-level data ====
# Load worksite data

PNSHP <-
  read_csv(
    "PNSHP_worksite-subtype_20200109.csv",
    col_types = cols_only(
      ProjectPK = "c",
      WorkSitePK = "c",
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
      Basin = "c"
    )
  ) %>%
  select(
    ProjectPK,
    WorkSitePK,
    Project_Source,
    Project_Year,
    Completed_Year,
    Latitude,
    Longitude,
    County,
    State,
    Basin,
    TypeName = Mapcat,
    SubTypeName,
    Project_Cost
  )
# N = 171,409 (worksite x subtype, or action level)

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

# Remove duplicate rows for metric level data (eliminates cases where only distinguishing factor is "VerboseName")

PNSHP_act <-
  PNSHP %>%
  distinct(ProjectPK, WorkSitePK, SubTypeName,
           .keep_all = TRUE)
# N = 167,846 (worksite x subtype)

rm(char)

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
PNSHP_act <-
  PNSHP_act %>%
  left_join(
    cf.df,
    by = c("Completed_Year" = "year")
  ) %>%
  mutate(
    Adj_Cost = Project_Cost * GDPDEF
  )

rm(avg.infl, cf, cf.df, GDPDEF)

# Create project and worksite level data ====

# Add fields for number of worksites and subtypes per project
PNSHP_act <-
  PNSHP_act %>%
  group_by(ProjectPK) %>%
  mutate(
    NWorksites = n_distinct(WorkSitePK),
    NSubtypes = n_distinct(SubTypeName)
  )

# Remove duplicate rows for worksite level data
PNSHP_wrk <-
  PNSHP_act %>%
  distinct(ProjectPK, WorkSitePK,
           .keep_all = TRUE)
# N = 101,816 (worksites)

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
# Number of states: 4; vast majority of data from OR; 9,423 with no state assigned (can be recovered from worksite location?)
# Decide how to handle the (few) multi-state projects

table(Basin)
# Number of basins: 46; Top five basins: Southern Oregon Coastal, Puget Sound, Willamette, Northern Oregon Coastal, Lower Snake; 9,208 with no basin assigned

detach(PNSHP_pjt)

# Screen based on above criteria
PNSHP_pjt <-
  PNSHP_pjt %>%
  filter(
    NWorksites <= 50 &
      Completed_Year >= 1990 &
      Completed_Year <= 2019
  )
# 37,210 projects remain after screening

# Apply screen to worksite data using joining
PNSHP_wrk <-
  PNSHP_wrk %>%
  semi_join(PNSHP_pjt, by = "ProjectPK")
# 62,884 worksites after screen

# Apply screen to action level data using joining
PNSHP_act <-
  PNSHP_act %>%
  semi_join(PNSHP_pjt, by = "ProjectPK")
# 101,274 actions after screen

# Count project subtypes and metrics
table(PNSHP_act$SubTypeName)
# Number of subtypes: 88; Top five subtypes: Sediment Reduction - Road Drainage System Improvements, Riparian - Planting, Riparian - Fencing, Instream - Large Woody Debris, Sediment Reduction - Road Obliteration
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

detach(PNSHP_pjt)

table(PNSHP_act$SubTypeName) %>% sort

# Generate summary reports ====

# Action subtype counts
PNSHP_summ_action <-
  PNSHP_act %>%
  group_by(SubTypeName) %>%
  summarize(
    NProjects = n_distinct(ProjectPK),
    NWorksites = n_distinct(WorkSitePK, ProjectPK)
  ) %>%
  arrange(desc(NProjects)) %>%
  left_join(
    PNSHP_act %>%
      filter(!is.na(Project_Cost)) %>%
      group_by(SubTypeName) %>%
      summarize(
        NProjectsCosts = n_distinct(ProjectPK),
        NWorksitesCosts = n_distinct(WorkSitePK, ProjectPK)
      )
  )

# Year counts
PNSHP_summ_year <-
  PNSHP_act %>%
  group_by(Completed_Year) %>%
  summarize(
    NProjects = n_distinct(ProjectPK),
    NWorksites = n_distinct(WorkSitePK, ProjectPK)
  ) %>%
  arrange(Completed_Year) %>%
  left_join(
    PNSHP_act %>%
      filter(!is.na(Project_Cost)) %>%
      group_by(Completed_Year) %>%
      summarize(
        NProjectsCosts = n_distinct(ProjectPK),
        NWorksitesCosts = n_distinct(WorkSitePK, ProjectPK)
      )
  )
  
# State counts
PNSHP_summ_state <-
  PNSHP_act %>%
  group_by(State) %>%
  summarize(
    NProjects = n_distinct(ProjectPK),
    NWorksites = n_distinct(WorkSitePK, ProjectPK)
  ) %>%
  arrange(desc(NProjects)) %>%
  left_join(
    PNSHP_act %>%
      filter(!is.na(Project_Cost)) %>%
      group_by(State) %>%
      summarize(
        NProjectsCosts = n_distinct(ProjectPK),
        NWorksitesCosts = n_distinct(WorkSitePK, ProjectPK)
      )
  )

# Basin counts
PNSHP_summ_basin <-
  PNSHP_act %>%
  group_by(Basin) %>%
  summarize(
    NProjects = n_distinct(ProjectPK),
    NWorksites = n_distinct(WorkSitePK, ProjectPK)
  ) %>%
  arrange(desc(NProjects)) %>%
  left_join(
    PNSHP_act %>%
      filter(!is.na(Project_Cost)) %>%
      group_by(Basin) %>%
      summarize(
        NProjectsCosts = n_distinct(ProjectPK),
        NWorksitesCosts = n_distinct(WorkSitePK, ProjectPK)
      )
  )
