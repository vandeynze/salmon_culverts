
# Prepeare environment ====
# Clear environment
rm(list = ls())

# Load libraries
library(sp)
library(maps)
library(maptools)
library(censusapi)
# library(tigris)
library(janitor)
library(tidyverse)

# Load data
# wd <- "C:/Users/Braeden/Desktop/NOAA/Analysis" # Change to where your version of "Analysis" folder for culverts is located
# wd <- "D:/Braeden/OneDrive/Documents/My Work/NOAA/Analysis"
# setwd(wd)
setwd("./output")
df_culv <- read_csv("culverts_wrk_working.csv") # This should be in there if you copied from google drive

# Build functions for finding county/fips from lat/long coordinates
find_county <- function(x, y) {
  require(sp)
  require(maps)
  require(maptools)
  # The arguments to this function, x and y, are vectors in which:
  #   - x contains the longitude in degrees (negative in the US)
  #   - y contains the latitude in degrees
  pointsDF <- data.frame(x, y)
  
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per county
  counties <- maps::map('county', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(counties$names, ":"), function(x) x[1])
  counties_sp <- map2SpatialPolygons(counties, IDs=IDs,
                                     proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, counties_sp)
  
  # Return the county names of the Polygons object containing each point
  countyNames <- sapply(counties_sp@polygons, function(x) x@ID)
  countyNames[indices]
}

find_fips <- function(x) {
  require(maps)
  # The argument to this function, x, is a vector which contains the name of a county in "state,county" format
  # Compatible with the maps::county.fips dataframe only
  data.frame("x.name" = x) %>% mutate(x.name = as.character(x)) %>% left_join(county.fips, by = c("x.name" = "polyname")) %>% pull(fips)
}

# Begin exploring...

# Add county and fips variables
df_culv <-
  df_culv %>%
  mutate(
    county = find_county(x = longitude, y = latitude),
    fips = find_fips(county),
    state_fips = round(fips, -3)/1000
  )

df_culv %>% tabyl(fips) %>% arrange(-n)
df_culv %>% tabyl(state_fips) %>% arrange(-n)

# Set up census api access
census_key <- "a6269eff97f65894fc88029146636aafcc45f463" # Feel free to use my access key or request yoiur own
list_fips <- df_culv %>% tabyl(fips, show_na = FALSE) %>% arrange(-n) %>% pull(fips)
list_fips_state <- df_culv %>% tabyl(state_fips, show_na = FALSE) %>% arrange(-n) %>% pull(state_fips)
list_years <- df_culv %>% tabyl(completed_year, show_na = FALSE) %>% arrange(completed_year) %>% pull(completed_year)

# Test census pull with getCensus
# Target NAICS codes:
# 11 - Ag, forestry, fishing and hunting
# 23 - Construction
# 0 - Total for all sectors

# Build census functions

# Loops getCensus over multiple years for select STATE FIPS and NAICS codes for CBP
# Only use top level NAICS codes (two digits); lower level classification is unsupported due to changes in NAICS over time
# (Base function getCensus pulls only one year at a time and requires year specific NAICS codes as input, so get_census identifies proper code set to assign)
# Needs to build out SIC code compatability for vintage prior to 1998
get_census <- function(years, naics, fips, key = census_key, program = "cbp") {
  require(censusapi)
  fips_chr <- paste(fips, collapse = ",")
  naics_chr <- as.character(naics)
  census <-
    suppressWarnings(
    map_df(
      years,
      function(x) {
        map_df(
          naics_chr,
          function(y) {
            ifelse(
              x >= 1998 & x < 2003,
              df <- getCensus(
                program,
                vintage = as.character(x),
                naics1997 = y,
                vars =
                  c(
                    "EMP",
                    "ESTAB",
                    "NAICS1997",
                    "NAICS1997_TTL",
                    "PAYANN",
                    "GEO_TTL"
                  ),
                region = "county:*", 
                regionin = paste0("state:", fips_chr), 
                key = key 
              ),
              NA
            )
            ifelse(
              x >= 2003 & x < 2008,
              df <- getCensus(
                program,
                vintage = as.character(x),
                naics2002 = y,
                vars =
                  c(
                    "EMP",
                    "ESTAB",
                    "NAICS2002",
                    "NAICS2002_TTL",
                    "PAYANN",
                    "GEO_TTL"
                  ),
                region = "county:*", 
                regionin = paste0("state:", fips_chr), 
                key = key 
              ),
              NA
            )
            ifelse(  
              x >= 2008 & x < 2012,
              df <- getCensus(
                program,
                vintage = as.character(x),
                naics2007 = y,
                vars =
                  c(
                    "EMP",
                    "ESTAB",
                    "NAICS2007",
                    "NAICS2007_TTL",
                    "PAYANN",
                    "GEO_TTL"
                  ),
                region = "county:*", 
                regionin = paste0("state:", fips_chr), 
                key = key 
              ),
              NA
            )
            ifelse(  
              x >= 2012 & x < 2017, 
              df <- getCensus(
                program,
                vintage = as.character(x),
                naics2012 = y,
                vars =
                  c(
                    "EMP",
                    "ESTAB",
                    "NAICS2012",
                    "NAICS2012_TTL",
                    "PAYANN",
                    "GEO_TTL"
                  ),
                region = "county:*", 
                regionin = paste0("state:", fips_chr), 
                key = key 
              ),
              NA
            )
            ifelse(
              x >= 2017,
              df <- getCensus(
                program,
                vintage = as.character(x),
                naics2017 = y,
                vars =
                  c(
                    "EMP",
                    "ESTAB",
                    "NAICS2017",
                    "NAICS2017_TTL",
                    "PAYANN",
                    "GEO_TTL"
                  ),
                region = "county:*", 
                regionin = paste0("state:", fips_chr), 
              ),
              NA
            )
            return(df)
          }
    )
      },
      .id = "year"
    ) %>%
    mutate(
      fips = as.numeric(state) * 1000 + as.numeric(county),
      year = as.numeric(year) + min(years) - 1
    ) %>%
    mutate_at(
      vars(ESTAB, EMP, PAYANN),
      as.numeric
    ) %>%
    unite(
      naics,
      starts_with("NAICS") & !ends_with("_TTL") & !ends_with("_1"),
      na.rm = TRUE
    ) %>%
    unite(
      naics_ttl,
      starts_with("NAICS") & ends_with("_TTL"),
      na.rm = TRUE
    ) %>%
    select(year, fips, state, county, GEO_TTL, ESTAB, EMP, PAYANN, naics, naics_ttl) %>%
    clean_names() %>%
    mutate(naics = as.numeric(naics)) %>%
    tibble()
  )
  census
}


df_census_test <-
  get_census(
    years = c(2008:2015),
    naics = c(23, 11),
    fips = 53
  )
df_census_test 
  
ggplot(
  df_census_test %>%
    filter(state == "53") %>%
    pivot_wider(
      id_cols = c(year, fips, state, county, geo_ttl),
      names_from = naics,
      names_prefix = "naics",
      values_from = emp
    ) %>%
    rowwise() %>%
    mutate(
      ymin = min(naics23, naics11),
      ymax = max(naics23, naics11)
    )
) + 
  geom_linerange(aes(x = as.character(year), y = ymin, ymax = ymax, group = geo_ttl), position = position_dodge(width = 0.9))
 
ggplot(
  df_census_test %>% filter(fips %in% list_fips) %>% mutate(payann = payann / emp) %>% filter(payann != Inf) %>% group_by(geo_ttl, year) %>% mutate(payann_max = max(payann))
) + 
  geom_line(aes(x = reorder(geo_ttl, payann_max), y = payann, group = geo_ttl), position = position_dodge(width = 0.9), color = "grey90") +
  geom_point(aes(x = reorder(geo_ttl, payann_max), y = payann, color = naics_ttl), position = position_dodge(width = 0.9)) +
  geom_text(aes(x = reorder(geo_ttl, payann_max), y = payann_max, group = geo_ttl, label = geo_ttl, alpha = naics), angle = 90, size = 2.5, nudge_y = 1, hjust = 0) +
  facet_wrap(~year) +
  ggthemes::theme_clean() +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
  ) +
  ggtitle("Annual Payroll by Industry") +
  scale_alpha(range = c(0, 1)) +
  scale_y_continuous(label = scales::label_comma())



area_county <- 
  counties(
    year = c(2015),
    state = c("WA", "OR", "ID", "MT"),
    cb = TRUE, 
    class = "sf", 
    progress_bar = FALSE
  ) %>%
  clean_names() %>%
  filter(
    geoid %in% list_fips
  ) %>%
  mutate(
    area = aland / 2589988,
    geoid = as.numeric(geoid)
  ) %>%
  select(geoid, area) %>%
  sf::st_drop_geometry()
area_county