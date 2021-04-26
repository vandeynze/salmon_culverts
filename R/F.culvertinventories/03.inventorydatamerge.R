# Purpose: Links variables used in spatial cost modeling (D.culvertcosts) to state inventory data from WDFW and ODFW

# Load packages ----
library(tidyverse)
library(here)
library(janitor)
library(readxl)
library(sf)
library(scales)
library(ggthemes)
library(nhdplusTools)
library(data.table)
library(nngeo)


# Build base data ----
# __ Load Blake's data ----
df_blake <-
  read_xlsx(here("data/Culverts inventory overlays v 14Apr2021.xlsx"), sheet = 1) %>%
  clean_names()

# __ Load inventory data ----
df_wdfw <-
  read_csv(here("output/allculvs/culvinventory_wdfw.csv"))
  
df_odfw <-
  read_csv(here("output/allculvs/culvinventory_odfw.csv"))

# __ Merge together ----
# sf_base will be what we build on
# We will use invent_id as our key throughout
sf_base <-
  df_blake %>%
  left_join(df_wdfw, by = c("site_recor" = "site_record_id")) %>%
  left_join(df_odfw, by = "fpb_ftr_id") %>%
  mutate(
    long =
      case_when(
        is.na(site_longitude) ~ fpb_long,
        is.na(fpb_long) ~ site_longitude
      ),
    lat =
      case_when(
        is.na(site_latitude) ~ fpb_lat,
        is.na(fpb_lat) ~ site_latitude
      )
  ) %>%
  select(
    -c(fpb_long, fpb_lat, site_longitude, site_latitude)
  ) %>% 
  st_as_sf(., coords = c('long', 'lat'), crs = 4326) %>% 
  st_transform(., crs = 4269)

# Clean out memory
rm(df_blake, df_odfw, df_wdfw)

# Layer on flowline COMIDs ----
# __ Load NHDPlus V2.1 flowlines ----
nhdplus_path(here("data", "NHDPlusNationalData", "NHDPlusV21_National_Seamless_Flattened_Lower48.gdb"))

if(!file.exists(here("output","spatial","nhdplus_fln_inv.rds"))){
  fln <- read_sf(dsn = here("data/NHDPlusNationalData/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb"), 
                 layer = "NHDFlowline_Network")
  
  # Reformat to meet sf standards
  fln <- st_zm(fln)
  
  # List stream data variable names
  # names(fln)
  # st_crs(fln)
  
  # Takes much longer with the full fln, so let's crop it
  # st_bbox(df_culv)
  sf_culv <- st_transform(sf_base, st_crs(fln))
  
  fln_inv <-
    st_crop(
      fln,
      sf_culv
    )
  # Much better
  write_rds(fln_inv, here("output","spatial","nhdplus_fln_inv.rds"))
}

fln_inv <- read_rds(here("output","spatial","nhdplus_fln_inv.rds"))
# Reduce to linestring
fln_inv_ls <- st_cast(fln_inv, "LINESTRING") %>% st_transform(3857)
sf_base <- st_transform(sf_base, 3857)

# Find matching COMIDs on 30m segments and 1500m search radius
df_comids <- get_flowline_index(fln_inv_ls, sf_base, search_radius = 1500, precision = 30)

sf_base <-
  sf_base %>%
  rowid_to_column("id") %>%
  left_join(
    df_comids %>%
      janitor::clean_names(),
    by = "id"
  ) %>%
  left_join(
    fln_inv %>%
      st_drop_geometry() %>%
      janitor::clean_names() %>%
      select(comid, slope),
    by = "comid"
  )

rm(df_comids, fln_inv, fln, fln_inv_ls)

# __ Load NHDPlus V2.1 Selected Attributes ----
# Precipitation
ppt_filenames <- list.files(here("data","NHDPlusSuppData","PPT"), pattern="*.txt", full.names=TRUE, recursive = TRUE)
df_ppt <- map(ppt_filenames, ~read_csv(., na = "-9999") %>% rename(comid = 1, ppt_cat = 2, ppt_acc = 3, ppt_tot = 4)) %>% bind_rows(.id = "project_year") %>% mutate(project_year = as.numeric(project_year) + 1995)
sf_base <- left_join(sf_base, df_ppt %>% filter(project_year == 2015) %>% select(-project_year))
rm(ppt_filenames, df_ppt)

# BFI
df_bfi <- read_csv(here("data","NHDPlusSuppData","BFI_CONUS.txt"), na = "-9999") %>% clean_names() %>% select(comid, bfi_cat = cat_bfi, bfi_acc = acc_bfi, bfi_tot = tot_bfi)
sf_base <- left_join(sf_base, df_bfi)
rm(df_bfi)

# Bankfull width, height, area
df_bfull <- read_csv(here("data","NHDPlusSuppData","BANKFULL_CONUS.txt"), na = "-9999") %>% clean_names()
sf_base <- left_join(sf_base, df_bfull)
rm(df_bfull)

# NLCD in catchment
nlcd_filenames <- list.files(here("data","NHDPlusSuppData","NLCD"), pattern="*.TXT$", full.names=TRUE, recursive = TRUE)
nlcd_years <- rep(c(2001, 2004, 2006, 2008, 2011, 2013, 2016), each = 3)
nlcd_method <- rep(c("acc", "cat", "tot"), length.out = length(nlcd_years))
nlcd_vars <- c(
  "comid",
  "nlcd_openwater",
  "nlcd_icesnow",
  "nlcd_devopen",
  "nlcd_devlow",
  "nlcd_devmed",
  "nlcd_devhigh",
  "nlcd_barren",
  "nlcd_fordec",
  "nlcd_forcon",
  "nlcd_formix",
  "nlcd_shrub",
  "nlcd_grass",
  "nlcd_pasture",
  "nlcd_crop",
  "nlcd_wetwood",
  "nlcd_wetherb",
  "nlcd_nodat",
  "project_year"
)
df_nlcd <- map(nlcd_filenames, ~read_csv(.) %>% clean_names(.))
names(df_nlcd) <- paste(nlcd_years, nlcd_method, sep = "_")
df_nlcd <-
  map2(
    .x = df_nlcd,
    .y = nlcd_years,
    ~ mutate(
      .x, 
      project_year = .y
    )
  )
df_nlcd <-
  map(
    .x = df_nlcd,
    ~ setNames(.x, nlcd_vars)
  )
df_nlcd <-
  map2(
    .x = df_nlcd,
    .y = nlcd_method,
    ~ setNames(.x, c("comid", paste(names(.x[2:18]), .y, sep = "_"), "project_year"))
  )

nhd_comids <- distinct(sf_base, comid) %>% pull(comid)
df_nlcd <- map(df_nlcd, ~filter(.x, comid %in% nhd_comids))
df_nlcd <- map(unique(nlcd_years), ~df_nlcd[names(df_nlcd) %>% str_detect(as.character(.x))] %>% reduce(full_join)) %>% bind_rows()

sf_base <- sf_base %>% mutate(project_year = 2015)
sf_base <- as.data.table(sf_base, key = c("invent_id", "comid", "project_year"))
df_nlcd <- as.data.table(df_nlcd, key = c("comid", "project_year"))
sf_base <- df_nlcd[sf_base, roll = "nearest", on = .(comid, project_year)]

rm(df_nlcd, nlcd_vars, nlcd_filenames, nlcd_method, nlcd_years, nhd_comids)

# Housing density
hden_filenames <- list.files(here("data","NHDPlusSuppData","HDEN"), pattern = "*.txt$", full.names = TRUE, recursive = TRUE)
df_hden <- map(hden_filenames, ~read_csv(.) %>% rename(comid = 1, hdens_cat = 2, hdens_cat_nodata = 3, hdens_acc = 4, hdens_acc_notdata = 5, hdens_tot = 6, hdens_tot_nodata = 7)) %>% bind_rows(.id = "project_year") %>% mutate(project_year = (as.numeric(project_year) - 1) * 10 + 2000) %>% as.data.table()
sf_base <- left_join(sf_base, df_hden %>% filter(project_year == 2010) %>% select(-project_year))
rm(hden_filenames, df_hden)

# Population density
popden_filenames <- list.files(here("data","NHDPlusSuppData","POPDEN"), pattern = "*.txt$", full.names = TRUE, recursive = TRUE)
df_popden <- map(popden_filenames, ~read_csv(.) %>% rename(comid = 1, popdens_cat = 2, popdens_cat_nodata = 3, popdens_acc = 4, popdens_acc_notdata = 5, popdens_tot = 6, popdens_tot_nodata = 7)) %>% bind_rows(.id = "project_year") %>% mutate(project_year = (as.numeric(project_year) - 1) * 10 + 2000) %>% as.data.table()
sf_base <- left_join(sf_base, df_popden %>% filter(project_year == 2010) %>% select(-project_year))
rm(popden_filenames, df_popden)

# Basin charactaristics
basin_filenames <- c(list.files(here("data","NHDPlusSuppData","BASIN"), pattern = c("*.txt$"), full.names = TRUE, recursive = TRUE), list.files(here("data","NHDPlusSuppData","BASIN"), pattern = c("*.TXT$"), full.names = TRUE, recursive = TRUE))
df_basin <- map(basin_filenames, ~read_csv(.) %>% clean_names()) %>% setNames(basin_filenames) %>% reduce(left_join, by = "comid")
sf_base <- left_join(sf_base, df_basin %>% select(-x1.x, -x1.y, -id), by = "comid")
rm(basin_filenames, df_basin)

sf_base <- st_as_sf(sf_base)

# Load other covers and distance variables from administrative shapes ----
# __ Load census urban area/cluster shapes ----
# Conditional download and unzip of urban area shapes from census
if(!file.exists(here("data/tl_2019_us_uac10.zip"))){
  download.file(
    "https://www2.census.gov/geo/tiger/TIGER2019/UAC/tl_2019_us_uac10.zip",
    here("data/tl_2019_us_uac10.zip")
  )
}
if(!file.exists(here("data/tl_2019_us_uac10/tl_2019_us_uac10.shp"))) {
  unzip(
    here("data/tl_2019_us_uac10.zip"),
    exdir = here("data/tl_2019_us_uac10/")
  )
}

# Load urban area shapes
sf_uac <-
  read_sf(here("data/tl_2019_us_uac10/tl_2019_us_uac10.shp")) %>%
  filter(str_ends(NAME10, "WA") | str_ends(NAME10, "OR")) %>%
  st_transform(st_crs(sf_base))

# Match nearest neighbor for each site using nngeo::st_nn
# Note that for all nearly 62k sites this takes a while!
# Maybe run overnight?
if(!file.exists(here("output/spatial/inv_uacdist.csv"))) {
  df_dist_uc <- 
    sf_base %>%
    st_nn(
      sf_uac,
      returnDist = TRUE
    ) %>%
    as_tibble() %>%
    mutate(across(everything(), unlist)) %>%
    rename(uc_nn_index = nn, uc_dist = dist) %>%
    left_join(
      sf_uac %>% st_drop_geometry() %>% select(uc_nn_name = NAMELSAD10) %>% rowid_to_column(),
      by = c("uc_nn_index" = "rowid")
    ) %>%
    select(-uc_nn_index)
  df_dist_ua <- 
    sf_base %>%
    st_nn(
      sf_uac %>% filter(UATYP10 == "U"),
      returnDist = TRUE
    ) %>%
    as_tibble() %>%
    mutate(across(everything(), unlist)) %>%
    rename(ua_nn_index = nn, ua_dist = dist) %>%
    left_join(
      sf_uac %>% filter(UATYP10 == "U") %>% st_drop_geometry() %>% select(ua_nn_name = NAMELSAD10) %>% rowid_to_column(),
      by = c("ua_nn_index" = "rowid")
    ) %>%
    select(-ua_nn_index)
  
  
  sf_base <-
    sf_base %>% bind_cols(df_dist_uc)
  sf_base <-
    sf_base %>% bind_cols(df_dist_ua)
  
  
  # Save a copy of the matches to save time later
  df_dist_ua %>%
    bind_cols(df_dist_uc) %>%
    write_csv(here("output/spatial/inv_uacdist.csv"))
  
} else {
  df_dist_uac <- read_csv(here("output/spatial/inv_uacdist.csv"))
  sf_culv <- sf_culv %>% bind_cols(df_dist_uac)
}
rm(df_dist_uac, df_dist_uc, df_dist_ua)

# __ Load public land data layer ----
# Conditional download and unzip of Protected Area Database from USGS
if(!file.exists(here("data/PADUS2_0_DOIRegion9_Shapefile.zip"))){
  download.file(
    "https://www.sciencebase.gov/catalog/file/get/5cc0e8c7e4b09b8c0b72951f?f=__disk__ad%2F4f%2F98%2Fad4f98886638258ed25d8a1d6553cb5c23c688c1",
    here("data/PADUS2_0_DOIRegion9_Shapefile.zip"),
    mode = "wb"
  )
}
if(!file.exists(here("data/PADUS2_0_DOIRegion9_Shapefile/PADUS2_0Designation_DOIRegion9.shp"))){
  unzip(
    here("data/PADUS2_0_DOIRegion9_Shapefile.zip"),
    exdir = here("data/PADUS2_0_DOIRegion9_Shapefile/")
  )
}

# Read in geodatabase as simple feature polygons
sf_pad <- st_read(here("data/PADUS2_0_DOIRegion9_Shapefile/PADUS2_0Designation_DOIRegion9.shp")) %>% st_zm() %>% st_transform(st_crs(sf_base))

# Cover check on worksites with publicly managed land
sf_base <-
  sf_base %>%
  mutate(
    publand = st_intersects(sf_base, sf_pad %>% filter(Mang_Type != "PVT" & Mang_Type != "NGO") %>% st_transform(st_crs(sf_base)) %>% st_zm()),
    publand = I(as.character(publand) != "integer(0)")
  )
rm(sf_pad)

# __ Load county data to ID missing counties ----
download.file("https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_county_500k.zip", here("data/cb_2018_us_county_500k.zip"))
unzip(here("data/cb_2018_us_county_500k.zip"), exdir = here("data/cb_2018_us_county_500k"))
sf_counties <-
  read_sf(here("data/cb_2018_us_county_500k/cb_2018_us_county_500k.shp")) %>%
  st_transform(crs = st_crs(sf_base)) %>%
  clean_names() %>%
  select(fips = geoid, state_fips = statefp)

sf_base <- st_join(sf_base, sf_counties)
rm(sf_counties)

# __ Load county-level employment patterns ----
# Set up census api access
census_key <- "a6269eff97f65894fc88029146636aafcc45f463" # Feel free to use my access key or request your own

# Get fips and year lists
list_fips <- sf_base %>% st_drop_geometry() %>% tabyl(fips, show_na = FALSE) %>% arrange(-n) %>% pull(fips)
list_fips_state <- sf_base %>% st_drop_geometry() %>% tabyl(state_fips, show_na = FALSE) %>% arrange(-n) %>% pull(state_fips)
list_years <- 2015

# Build census check function
get_census <-
  function(years, naics, fips, key = census_key, program = "cbp") {
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

# Get census data
# Full join
df_census <-
  get_census(
    years = c(2015),
    naics = c(11,21,22,23,"31-33",42,"44-45","48-49",51,52,53,54,55,56,61,62,71,72,81),
    fips = list_fips_state %>% as.character()
  )
# Can be finicky, try it until it succeeds basically
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

# Wide summs df
df_census_summs <-
  df_census_summs %>%
  select(
    -c(
      totpayann, totemp, totestab, estab, payann, proppayann, propestab, # Tweak here to grab other measures
      state, county, geo_ttl, naics_ttl)
  ) %>%
  pivot_wider(
    id_cols = c(year, fips),
    names_from = naics,
    values_from = emp, # Tweak here to grab other measures
    names_prefix = "naics"
  )

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
    emp_other = naics81,
    project_year = year
  ) %>%
  mutate(
    across(starts_with("emp_"), ~replace_na(., 0)),
    fips = as.character(fips),
    fips = case_when(nchar(fips) == 4 ~ paste0("0", fips), TRUE ~ fips)
  )

# Join it
sf_base <- left_join(sf_base, df_census_summs)
sf_base <- sf_base %>% distinct(invent_id, .keep_all = TRUE)
rm(df_census, df_census_summs, list_years, list_fips_state, list_fips, census_key)

# Save out ----
write_csv(sf_base, here("output/inv_draft22042021.csv"))
# This draft is missing data on distances to urban areas

sf_base <- 
  read_csv(here("output/inv_draft21042021.csv"), guess_max = 30000) %>%
  select(-geometry) %>%
  left_join(df_wdfw %>% select(site_record_id, site_longitude, site_latitude), by = c("site_recor" = "site_record_id")) %>%
  left_join(df_odfw %>% select(fpb_ftr_id, fpb_long, fpb_lat), by = "fpb_ftr_id") %>%
  mutate(
    long =
      case_when(
        is.na(site_longitude) ~ fpb_long,
        is.na(fpb_long) ~ site_longitude
      ),
    lat =
      case_when(
        is.na(site_latitude) ~ fpb_lat,
        is.na(fpb_lat) ~ site_latitude
      )
  ) %>%
  select(
    -c(fpb_long, fpb_lat, site_longitude, site_latitude)
  ) %>% 
  st_as_sf(., coords = c('long', 'lat'), crs = 4326) %>% 
  st_transform(., crs = 4269)
