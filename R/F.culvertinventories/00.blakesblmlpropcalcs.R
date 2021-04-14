# Calculate buffer proportions from area totals using Blake's data

# Load packages
library(tidyverse)
library(readxl)
library(here)
library(janitor)

# Read data
df <- read_excel(here("data/First 50k 500m buff BLM own.xlsx"), sheet = 1)

names(df)
n_distinct(df$Fpb_ftr_id) # Site ID # Per BF: legacy site ID from ODFW inventory, not unique with WDFW invenotry so invent_id built across the two
df %>% tabyl(Site_recor) # Not sure, all 0 # Per BF: legacy site ID from WDFW inventory not included in test run
n_distinct(df$Invent_id) # A new internal Site ID # Confirmed by BF
df %>% tabyl(Property_s) # Property ownership labels # Confirmed by BF not unique to site ID (multiple ownership "polygons" per site due to internal buffer - cover mechanics)
summary(df$Area_Meters) # Area in of ownership type in sq meters

# Pivot wider
# So data is in long format: each row represents a site x property ownership combo, with the area as teh value
# We want it wider so that each row is a site and each column is the area for each property class

# First lets drop the extra columns
df <- df %>% clean_names() %>% select(-fpb_ftr_id, -site_recor)

# Let's check how many unique property classes there are by site
df %>%
  group_by(invent_id) %>%
  summarize(
    n = n(),
    n_prop = n_distinct(property_s),
    uniqueness = n/n_prop
  ) %>%
  summary()

# Okay so one problem is that the rows are not uniquely identified, so we need to do some summing first
df <-
  df %>%
  group_by(invent_id, property_s) %>%
  summarize(area_meters_sum = sum(area_meters))

# Then we will "pivot_wide"
df_wide <-
  df %>%
  ungroup() %>%
  pivot_wider(
    id_cols = invent_id,
    names_from = property_s,
    values_from = area_meters_sum
  ) %>%
  # And clean the names
  clean_names() %>%
  # And add a total column
  rowwise() %>%
  mutate(tot = sum(c_across(-invent_id), na.rm = TRUE)) %>%
  # New line to set NAs as true zeros per BF suggestion
  mutate(across(-invent_id, ~replace_na(., 0)))

# Sweet now we need proportions
df_wide <-
  df_wide %>%
  mutate(
    across(
      -invent_id,
      ~ . / tot,
      .names = "{col}_prop"
    )
  )
# NAs as zeros carrys through by properties of math

# Great! Eye check for first 10 rows or so looks just like Blake's pivot tables. 

# And finally write it out
write_csv(df_wide, here("output/blmprop_processed.csv"))
