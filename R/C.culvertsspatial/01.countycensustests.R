# TITLE: County econ data from US census for culverts
# AUTHOR: Braeden Van Deynze
# DATE: July, 2020
# INPUTS: "culverts_full_mapping.csv"" data for culvert work site data
# OUTPUTS: Data on nearest road to culvert work site
# STATUS: Operable for full data with levels and proportion outputs
# PRIORITY: (1) Prep for sourcing
  
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

# Load culvert work site data
# Load data
sf_culv <- read_csv(here("output/culverts_full_mapping.csv")) %>% 
  st_as_sf(coords = c('longitude', 'latitude'), crs = 4326) %>%
  select(-county)

# Load county data to ID missing counties
temp <- tempfile()
tempdir <- tempdir()
download.file("https://www2.census.gov/geo/tiger/TIGER2017/COUNTY/tl_2017_us_county.zip", temp)
unzip(temp, exdir = tempdir)
sf_counties <- read_sf(paste0(tempdir, "\\tl_2017_us_county.shp")) %>%
  st_transform(crs = 4326) %>%
  clean_names() %>%
  select(fips = geoid, county = name, state_fips = statefp)
unlink(temp)
rm(temp, tempdir)

# Spatial join
sf_culv <- st_join(sf_culv, sf_counties) %>% 
  mutate(match = !is.na(fips))

# Check for missing counties
sum(sf_culv$match) # No missing

sf_culv %>% tabyl(fips) %>% arrange(-n)
sf_culv %>% tabyl(state_fips) %>% arrange(-n)
sf_culv %>% tabyl(county) %>% arrange(-n)

# Set up census api access
census_key <- "a6269eff97f65894fc88029146636aafcc45f463" # Feel free to use my access key or request your own

# Move to df for culverts and load lists for mapping
df_culv <- sf_culv %>% st_drop_geometry()

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


# df_census_test <-
#   get_census(
#     # years = c(2001:2015),
#     years = c(2001:2005,2007:2015),
#     naics = c(
#       23,11
#               ),
#     fips = "41"
#   )
# df_census_test %>% filter(year == 2006)
# 2006 is broken for some reason and included in a different year; weird but I'm rolling with it

# So it works, but need to run over all naics base codes in order to draw totals
# There's also a mismatch issue with the naics_ttl and geo_ttl's that will need cross-walks between versions
# Or could index?
# Also lots of zeros which appear to be missing values, likely due to privacy limitations on the data (I see this w/ NASS and counties w/ few farms too)

# ggplot(
#   df_census_test %>%
#     filter(state == "41"),
#   aes(x = as.character(year), y = log(emp), color = factor(fips))
# ) + 
#   geom_point(aes(shape = factor(naics))) +
#   geom_line(aes(linetype = factor(naics), group = factor(naics))) +
#   scale_color_discrete(guide = NULL) +
#   ggthemes::theme_clean() +
#   theme(legend.position = "bottom", axis.text = element_text(angle = 90)) +
#   ggtitle("Log Employment by County over Time", "On log scale, not much change over time; Lots of missing value issues too") +
#   facet_wrap(~ fips)
# 
# 
# ggplot(
#   df_census_test %>%
#     filter(state == "41") %>%
#     pivot_wider(
#       id_cols = c(year, fips, state, county, geo_ttl),
#       names_from = naics,
#       names_prefix = "naics",
#       values_from = emp
#     ) %>%
#     rowwise()
# ) + 
#   geom_point(aes(x = as.character(year), y = naics23, color = factor(fips))) +
#   geom_line(aes(x = as.character(year), y = naics23, color = factor(fips), group = factor(fips))) +
#   ggthemes::theme_clean() +
#   theme(legend.position = "none", axis.text = element_text(angle = 90)) +
#   ggtitle("Construction Employment by County over Time", "Not much difference in between county variation over time,\nbut absolute levels show large variation across time and between counties")
#   
#  
# ggplot(
#   df_census_test %>% filter(fips %in% list_fips) %>% mutate(payann = payann / emp) %>% filter(payann != Inf) %>% group_by(geo_ttl, year) %>% mutate(payann_max = max(payann))
# ) + 
#   geom_line(aes(x = reorder(fips, payann_max), y = payann, group = fips), position = position_dodge(width = 0.9), color = "grey90") +
#   geom_point(aes(x = reorder(fips, payann_max), y = payann, color = naics), position = position_dodge(width = 0.9)) +
#   geom_text(aes(x = reorder(fips, payann_max), y = payann_max, group = fips, label = geo_ttl), angle = 90, size = 2.5, nudge_y = 1, hjust = 0) +
#   facet_wrap(~year) +
#   ggthemes::theme_clean() +
#   theme(
#     axis.text.x = element_blank(),
#     axis.title.x = element_blank(),
#     axis.ticks.x = element_blank(),
#     axis.title.y = element_blank(),
#     legend.position = "bottom"
#   ) +
#   ggtitle("Annual Payroll by Industry", "Not much difference in ratio between industry within county or relative pay between counties over time,\nbut ratios are different across counties") +
#   # scale_alpha_discrete(levels = c(0, 1)) +
#   scale_y_continuous(label = scales::label_comma())
# 
# # This test contains all fips categories
# df_census_test2 <-
#   get_census(
#     years = c(2010:2012),
#     naics = c(11,21,22,23,"31-33",42,"44-45","48-49",51,52,53,54,55,56,61,62,71,72,81),
#     fips = 53
#   )
# df_census_test2
# df_census_test2 %>% tabyl(naics)
# df_census_test2 %>% tabyl(naics, naics_ttl)
# 
# 
# df_census_test2_summs <-
#   df_census_test2 %>%
#   group_by(fips, year) %>%
#   mutate(
#     totpayann = sum(payann),
#     totemp = sum(emp),
#     totestab = sum(estab),
#     naics = case_when(
#       naics_ttl == "Manufacturing" ~ "31-33",
#       naics_ttl == "Transportation and warehousing" ~ "48-49",
#       naics_ttl == "Retail trade" ~ "44-45",
#       TRUE ~ naics
#     )
#   ) %>%
#   ungroup() %>%
#   mutate(
#     proppayann = payann / totpayann,
#     propemp = emp / totemp,
#     propestab = estab / totestab
#   )
# df_census_test2_summs
# 
# 
# df_census_test2_summs %>%
#   ggplot() +
#   geom_col(
#     aes(
#       x = factor(fips),
#       y = propemp,
#       fill = naics
#     ),
#     position = "stack"
#   ) +
#   ggthemes::theme_clean() +
#   # theme(legend.position = "none", axis.text = element_text(angle = 90)) +
#   ggtitle("Proportion of Total Employment by NAICS Code by State", "Not much change in overall patterns between years") +
#   facet_wrap(~ year)
# 
# # Try linking it to culverts
# # Wide summs df
# (df_census_test2_summs2 <-
#   df_census_test2_summs %>%
#   select(
#     -c(totpayann, totemp, totestab, emp, estab, payann, proppayann, propestab, state, county, geo_ttl, naics_ttl)
#   ) %>%
#   pivot_wider(
#     id_cols = c(year, fips),
#     names_from = naics,
#     values_from = propemp,
#     names_prefix = "naics"
#   ))
#   
# df_culv_test <-
#   df_culv %>%
#   filter(
#     completed_year %in% c(2010:2012),
#     state == "WA"
#   ) %>%
#   left_join(
#     df_census_test2_summs2,
#     by = c(
#       "project_year" = "year",
#       "fips" = "fips"
#     )
#   )
# df_culv_test %>% select(starts_with("naics")) %>% summary()
# It pretty much works!


# Full join
df_census <-
  get_census(
    years = c(2001:2015),
    naics = c(11,21,22,23,"31-33",42,"44-45","48-49",51,52,53,54,55,56,61,62,71,72,81),
    fips = list_fips_state %>% as.character()
  )
# Weird problem with 2009
df_census2 <-
  get_census(
    years = c(2010:2015),
    naics = c(11,21,22,23,"31-33",42,"44-45","48-49",51,52,53,54,55,56,61,62,71,72,81),
    fips = list_fips_state %>% as.character()
  )

# Can be finicky, try it until it succeeds basically
df_census10 <- get_census(
    years = 2009,
    naics = c(61,62,71,72,81),
    fips = list_fips_state %>% as.character()
  )

df_census <-
  df_census %>%
  bind_rows(df_census2, df_census3, df_census4, df_census5, df_census6, df_census7, df_census8, df_census9, df_census10)
df_census %>% tabyl(year)
# Looks good

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
  mutate(fips = as.double(fips)) %>%
  left_join(
    df_census_summs,
    by = c(
      "project_year" = "year",
      "fips" = "fips"
    )
  )
df_culv %>% select(fips, project_year, starts_with("emp_")) %>% mutate(across(-project_year, ~is.na(.))) %>% tabyl(emp_agforest, project_year)
# Lots missing values, but mostly in the years before CBP is pulled < 2001


# Save out census data for future merging
write_csv(df_census_summs, here("output/spatial/culverts_cbp.csv"))
