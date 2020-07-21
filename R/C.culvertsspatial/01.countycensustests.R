# TITLE: County econ data from US census for culverts
# AUTHOR: Braeden Van Deynze
# DATE: July, 2020
# INPUTS: "culverts_full_mapping.csv"" data for culvert work site data
# OUTPUTS: Culvert dataset with employment data

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
library(here)

# Load culvert worksite data
df_culv <- read_csv(here("output/culverts_wrk_working.csv"))

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
    # years = c(2001:2015),
    years = c(2001:2005,2007:2015),
    naics = c(
      23,11
              ),
    fips = "41"
  )
df_census_test %>% filter(year == 2005)
# 2006 is broken for some reason and included in a different year; weird but I'm rolling with it

# So it works, but need to run over all naics base codes in order to draw totals
# There's also a mismatch issue with the naics_ttl and geo_ttl's that will need cross-walks between versions
# Or could index?
# Also lots of zeros which appear to be missing values, likely due to privacy limitations on the data
# Generally, these levels are very stable, though the ratio between construction and forestry/ag varies between counties
# This means that a county-fixed effects model would absorb most of this variation
# Because of little temporal variation, I think we can get away with some sort of fixed average across years, which would avoid missing values problem

ggplot(
  df_census_test %>%
    filter(state == "41"),
  aes(x = as.character(year), y = log(emp), color = factor(fips))
) + 
  geom_point(aes(shape = factor(naics))) +
  geom_line(aes(linetype = factor(naics), group = factor(naics))) +
  scale_color_discrete(guide = NULL) +
  ggthemes::theme_clean() +
  theme(legend.position = "bottom", axis.text = element_text(angle = 90)) +
  ggtitle("Log Employment by County over Time", "On log scale, not much change over time; Lots of missing value issues too") +
  facet_wrap(~ fips)


ggplot(
  df_census_test %>%
    filter(state == "41") %>%
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
  ggthemes::theme_clean() +
  theme(legend.position = "none", axis.text = element_text(angle = 90)) +
  ggtitle("Construction Employment by County over Time", "Not much difference in between county variation over time,\nbut absolute levels show large variation across time and between counties")
  
 
ggplot(
  df_census_test %>% filter(fips %in% list_fips) %>% mutate(payann = payann / emp) %>% filter(payann != Inf) %>% group_by(geo_ttl, year) %>% mutate(payann_max = max(payann))
) + 
  geom_line(aes(x = reorder(fips, payann_max), y = payann, group = fips), position = position_dodge(width = 0.9), color = "grey90") +
  geom_point(aes(x = reorder(fips, payann_max), y = payann, color = naics), position = position_dodge(width = 0.9)) +
  geom_text(aes(x = reorder(fips, payann_max), y = payann_max, group = fips, label = geo_ttl), angle = 90, size = 2.5, nudge_y = 1, hjust = 0) +
  facet_wrap(~year) +
  ggthemes::theme_clean() +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "bottom"
  ) +
  ggtitle("Annual Payroll by Industry", "Not much difference in ratio between industry within county or relative pay between counties over time,\nbut ratios are different across counties") +
  # scale_alpha_discrete(levels = c(0, 1)) +
  scale_y_continuous(label = scales::label_comma())

# This test contains all fips categories
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
  ggthemes::theme_clean() +
  # theme(legend.position = "none", axis.text = element_text(angle = 90)) +
  ggtitle("Proportion of Total Employment by NAICS Code by State", "Not much change in overall patterns between years") +
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
    completed_year %in% c(2010:2012),
    state == "WA"
  ) %>%
  left_join(
    df_census_test2_summs2,
    by = c(
      "completed_year" = "year",
      "fips" = "fips"
    )
  )
df_culv_test %>% select(starts_with("naics")) %>% summary()
# It pretty much works!


# Full join
df_census <-
  get_census(
    years = c(2001:2005, 2007:2015),
    naics = c(11,21,22,23,"31-33",42,"44-45","48-49",51,52,53,54,55,56,61,62,71,72,81),
    fips = list_fips_state %>% as.character()
  )
# Weird problem with 2006 rn but still recovers 2006 with another call for some reason; this is a bizarre undocumented api problem I think

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


# Wide summs df
(df_census_summs <-
    df_census_summs %>%
    select(
      -c(
        totpayann, totemp, totestab, emp, estab, payann, proppayann, propestab, # Tweak here to grab other measures
        state, county, geo_ttl, naics_ttl)
    ) %>%
    pivot_wider(
      id_cols = c(year, fips),
      names_from = naics,
      values_from = propemp, # Tweak here to grab other measures
      names_prefix = "naics"
    ))

# NAICS KEY
# 11 Ag, forestry
# 21 Mining, oil and gas
# 22 Utilities
# 23 Construction
# 31-33 Manufacturing
# 42 Wholesale trade
# 44-45 Retail trade
# 48-49 Transport and warehousing
# 51 Information
# 52 Finance and insurance
# 53 Real estate
# 54 Professional/scientific services
# 55 Management
# 56 Admin/support/waste management
# 61 Education
# 62 Health/social care
# 71 Arts/entertainment/recreation
# 72 Hospitality/food
# 81 Other

# Update to better names
df_census_summs <-
    df_census_summs %>%
    rename(
      emp_agforest = naics11,
      emp_mining = naics21,
      emp_util = naics22,
      emp_const = naics23,
      emp_manuf = `naics31-33`,
      emp_wholesale = naics42,
      emp_retail = `naics44-45`,
      emp_transport = `naics48-49`,
      emp_info = naics51,
      emp_finance = naics52,
      emp_realestate = naics53,
      emp_profsci = naics54,
      emp_mgmn = naics55,
      emp_admin = naics56,
      emp_educ = naics61,
      emp_health = naics62,
      emp_arts = naics71,
      emp_food = naics72,
      emp_other = naics81
    )


df_culv <-
  df_culv %>%
  filter(
    # project_year %in% c(2010:2012),
    # state == "WA"
  ) %>%
  select(
    -starts_with("emp_")
  ) %>%
  left_join(
    df_census_summs,
    by = c(
      "completed_year" = "year",
      "fips" = "fips"
    )
  )
df_culv %>% select(fips, completed_year, starts_with("emp_")) %>% mutate(across(-completed_year, ~is.na(.))) %>% tabyl(emp_agforest, completed_year)
# Lots missing values, but mostly in the years before CBP is pulled < 2001


# Save out
write_csv(df_culv, here("output/culverts_full_working.csv"))