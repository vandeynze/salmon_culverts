# TITLE: County econ data from US census for culverts
# AUTHOR: Braeden Van Deynze
# DATE: June, 2020
# INPUTS: "culverts_full_mapping.csv"" data for culvert work site data
# OUTPUTS: relevant econ variables for culverts

# Prepeare environment ====
# Clear environment
rm(list = ls())

# Load libraries
library(sp)
library(sf)
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
df_culv <- read_csv("culverts_full_mapping.csv") # This should be in there if you copied from google drive

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

df_culv %>% tabyl(fips) %>% arrange(-n) # 181 NA
df_culv %>% tabyl(state_fips) %>% arrange(-n) # 181 NA (same as above)
df_culv %>% tabyl(county) %>% arrange(-n) # 146 NA

# Assign geometry
sf_culv <-
  df_culv %>%
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326
  )

# Map against missing IDs for county
sf_states <-
  st_as_sf(maps::map("state", plot = FALSE, fill = TRUE)) %>%
  filter(ID %in% c("california", "washington", "oregon", "idaho"))

# Missing FIPS
sf_culv %>% ggplot() + geom_sf(data = sf_states) + geom_sf(aes(color = is.na(fips))) + coord_sf(xlim = c(-125,-110), ylim = c(42, 49))

# Missing counties
sf_culv %>% ggplot() + geom_sf(data = sf_states) + geom_sf(aes(color = is.na(county))) + coord_sf(xlim = c(-125,-110), ylim = c(42, 49))

# Almost all missing are on the coast...
# Will deal with this later

# Set up census api access
census_key <- "a6269eff97f65894fc88029146636aafcc45f463" # Feel free to use my access key or request your own
list_fips <- df_culv %>% tabyl(fips, show_na = FALSE) %>% arrange(-n) %>% pull(fips)
list_fips_state <- df_culv %>% tabyl(state_fips, show_na = FALSE) %>% arrange(-n) %>% pull(state_fips)
list_years <- df_culv %>% tabyl(project_year, show_na = FALSE) %>% arrange(project_year) %>% pull(project_year)

# Test census pull with getCensus
# Target NAICS codes:
# 11 - Ag, forestry, fishing and hunting
# 23 - Construction
# 0 - Total for all sectors

# Build census functions

# Loops getCensus over multiple years for select STATE FIPS and NAICS codes for CBP
# Only use top level NAICS codes (two digits); lower level classification is unsupported due to changes in NAICS over time
# (Base function getCensus pulls only one year at a time and requires year specific NAICS codes as input, so get_census identifies proper code set to assign)
# Needs to build out SIC code compatibility for vintage prior to 1998
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
    # mutate(naics = as.numeric(naics)) %>%
    tibble()
  )
  census
}


df_census_test <-
  get_census(
    years = c(2008:2010),
    naics = c(
      23, 11
              ),
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
    rowwise()
) + 
  geom_point(aes(x = as.character(year), y = naics23, color = factor(fips))) +
  geom_line(aes(x = as.character(year), y = naics23, color = factor(fips), group = factor(fips))) +
  theme(legend.position = "none")
 
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

df_census_test2 <-
  get_census(
    years = c(2010:2012),
    naics = c(11,21,22,23,"31-33",42,"44-45","48-49",51,52,53,54,55,56,61,62,71,72,81),
    fips = 53
  )
df_census_test2
df_census_test2 %>% tabyl(naics)
df_census_test2 %>% tabyl(naics, naics_ttl)


df_census_test2_summs <-
  df_census_test2 %>%
  group_by(fips, year) %>%
  mutate(
    totpayann = sum(payann),
    totemp = sum(emp),
    totestab = sum(estab),
    naics = case_when(
      naics_ttl == "Manufacturing" ~ "31-33",
      naics_ttl == "Transportation and warehousing" ~ "48-49",
      naics_ttl == "Retail trade" ~ "44-45",
      TRUE ~ naics
    )
  ) %>%
  ungroup() %>%
  mutate(
    proppayann = payann / totpayann,
    propemp = emp / totemp,
    propestab = estab / totestab
  )
df_census_test2_summs


df_census_test2_summs %>%
  ggplot() +
  geom_col(
    aes(
      x = factor(fips),
      y = propemp,
      fill = naics
    ),
    position = "stack"
  ) +
  facet_wrap(~ year)

# Try linking it to culverts
# Wide summs df
(df_census_test2_summs2 <-
  df_census_test2_summs %>%
  select(
    -c(totpayann, totemp, totestab, emp, estab, payann, proppayann, propestab, state, county, geo_ttl, naics_ttl)
  ) %>%
  pivot_wider(
    id_cols = c(year, fips),
    names_from = naics,
    values_from = propemp,
    names_prefix = "naics"
  ))
  
df_culv_test <-
  df_culv %>%
  filter(
    project_year %in% c(2010:2012),
    state == "WA"
  ) %>%
  left_join(
    df_census_test2_summs2,
    by = c(
      "project_year" = "year",
      "fips" = "fips"
    )
  )
df_culv_test %>% select(starts_with("naics")) %>% summary()
# It pretty much works!


# Full join
df_census <-
  get_census(
    years = c(1998:2015),
    naics = c(11,21,22,23,"31-33",42,"44-45","48-49",51,52,53,54,55,56,61,62,71,72,81),
    fips = list_fips_state
  )


df_census_summs <-
  df_census %>%
  group_by(fips, year) %>%
  mutate(
    totpayann = sum(payann),
    totemp = sum(emp),
    totestab = sum(estab),
    naics = case_when(
      naics_ttl == "Manufacturing" ~ "31-33",
      naics_ttl %in% c("Transportation and warehousing", "Transportation & warehousing", "Transportation and Warehousing") ~ "48-49",
      naics_ttl %in% c("Retail trade", "Retail Trade") ~ "44-45",
      TRUE ~ naics
    )
  ) %>%
  ungroup() %>%
  mutate(
    proppayann = payann / totpayann,
    propemp = emp / totemp,
    propestab = estab / totestab
  )
df_census_summs
df_census_summs %>% tabyl(naics_ttl, fips, year)


# Wide summs df
(df_census_summs <-
    df_census_summs %>%
    select(
      -c(totpayann, totemp, totestab, emp, estab, payann, proppayann, propestab, state, county, geo_ttl, naics_ttl)
    ) %>%
    pivot_wider(
      id_cols = c(year, fips),
      names_from = naics,
      values_from = propemp,
      names_prefix = "naics"
    ))

df_culv_test <-
  df_culv %>%
  filter(
    # project_year %in% c(2010:2012),
    # state == "WA"
  ) %>%
  left_join(
    df_census_summs,
    by = c(
      "project_year" = "year",
      "fips" = "fips"
    )
  )
df_culv_test %>% select(starts_with("naics")) %>% summary()



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