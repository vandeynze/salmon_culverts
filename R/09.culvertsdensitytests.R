# TITLE: Density analysis for culverts
# AUTHOR: Braeden Van Deynze
# DATE: June, 2020
# INPUTS: "culverts_full_mapping.csv"" data for culvert work site data
# OUTPUTS: density metrics for nearby culvert projects

# Prepare environment ====
# Clear environment
rm(list = ls())

# Load libraries
library(MASS)
library(sf)
library(stars)
library(raster)

library(janitor)
library(tidyverse)
library(RColorBrewer)

# Set top-level working directory
# wd <- "C:/Users/Braeden/Desktop/NOAA/Analysis"
# wd <- "C:/Users/braeden.vandeynze/Documents/Salmon Cost Project/Analysis/"  # For Braeden; comment out for other users
wd <- "D:/Braeden/OneDrive/Documents/My Work/NOAA/Analysis"
setwd(wd)

# Load and prepare culvert data ====
setwd("./output") # For Braeden; comment out for other users
df_culv <- read_csv("culverts_wrk_working.csv")


# Filter out cost per culvert outliers
quantile(df_culv$cost_per_culvert, na.rm = TRUE, probs = c(0, 0.01, 0.1, 0.25, 0.5, 0.75, 0.9, 0.99, 1), type = 8) %>% format(big.mark = ",", scientific = FALSE, digits = 0, trim = TRUE)
df_culv <-
  df_culv %>%
  drop_na(cost_per_culvert) %>%
  filter(cost_per_culvert > 2000, cost_per_culvert < 750000, completed_year > 1995)
# Drops 534 worksites

# Build project level data
df_culv_prj <-
  df_culv %>%
  distinct(
    project_id,
    cost_per_culvert,
    n_culverts,
    dist_mean,
    n_worksites, 
    action_fishpass_culvrem_prj,
    action_fishpass_culvinst_prj,
    project_source,
    basin,
    completed_year
  )

# Build kde function
st_kde <- function(points, cellarea = 1e6){
  # points is a SimpleFeatures object with point geometry and a CRS assigned
  # cellarea is the area of each (square) cell in square meters (default is 100,000 square meters, or 1 square kilometer)
  
  # Check for required libraries
  require(MASS)
  require(raster)
  require(sf)
  
  # Create reference objects
  crs_points <- st_crs(points) # Native CRS so that returned density geometry can be returned to appropriate projection
  cellsize <- sqrt(cellarea) # Length of a side of the square cell based on desired area
  
  # Transform points from spherical projection to cartesian approximation
  points_lambert <- st_transform(points, crs = 31370)
  
  # Create reference vector for window to estimate surface over
  extent_vec <- st_bbox(points_lambert)[c(1, 3, 2, 4)]
  
  # Calculate grid size
  n_y <- ceiling((extent_vec[4] - extent_vec[3]) / cellsize)
  n_x <- ceiling((extent_vec[2] - extent_vec[1]) / cellsize)
  
  # Expand window extremes to allow for full cells in grid (no partial cells)
  extent_vec[2] <- extent_vec[1] + (n_x * cellsize) - cellsize
  extent_vec[4] <- extent_vec[3] + (n_y * cellsize) - cellsize
  
  # Create pure coordinates object
  coords <- points_lambert %>% st_coordinates
  
  # Generate matrix of kernal density using MASS::kde2d
  matrix <- kde2d(coords[, 1],coords[, 2], n = c(n_x, n_y), lims = extent_vec)
  
  # Rasterize matrix
  density <- matrix %>% raster
  
  # Transform raster to simple features 
  density_sf <- 
    density %>% 
    rasterToPolygons() %>% 
    st_as_sf() %>% 
    # Transform density estimates to appropriate units
    rename(density = layer) %>%
    mutate(
      density_m2 = density * nrow(points),
      density_km2 = density_m2 * 1e6, # per sq km
      density_10km2 = density_km2 * 100, # per 10 sq km
      density_100km2 = density_10km2 * 100 # per 100 sq km
    )
  
  # Correct projections for simple features geometry
  st_crs(density_sf) <- st_crs(points_lambert)
  density_sf <- density_sf %>% st_transform(crs_points) %>% as_tibble %>% st_as_sf()
  
  return(density_sf)
}

# Prep base map
sf_us <- getData("GADM", country = "USA", level = 1) %>% st_as_sf() %>% filter(NAME_1 %in% c('California', 'Nevada', 'Utah', 'Wyoming', 'Montana', 'Idaho', 'Oregon', 'Washington'))
sf_canada <- getData("GADM", country = "CAN", level = 1) %>% st_as_sf() %>% filter(NAME_1 %in% c("British Columbia", "Alberta"))
sf_base <- 
  # read_rds("sf_base.rds")
  rbind(sf_us, sf_canada)
rm(sf_us, sf_canada)

# Prep data for spatial analysis using sf
sf_culv <- st_as_sf(df_culv, coords = c("longitude", "latitude"))
st_crs(sf_culv) <- st_crs(sf_base)

# Estimate kde
sf_kde <- st_kde(sf_culv)

# Plot
# Set colors
reds_ramp <- colorRampPalette(brewer.pal(9, name = "Reds"))

# Histogram
ggplot() +
  geom_histogram(data = sf_kde, aes(x = density_10km2), bins = 30, fill = reds_ramp(30))

# Map
map_density_allyears <- 
  ggplot() +
  geom_sf(data = sf_base, fill = "antiquewhite1") +
  geom_sf(data = sf_culv, size = 0.1) +
  geom_sf(data = sf_kde %>% filter(density_10km2 >= 0.5), aes(fill = density_10km2), color = NA, alpha = 0.7) +
  scale_fill_fermenter(expression(Culverts~per~10~km^2), palette = "Reds", direction = 1) +
  # scale_alpha(range = c(0, 1)) +
  coord_sf(
    xlim = c(-126, -111.5),
    ylim = c(41.5, 49.5),
    expand = FALSE
  ) +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "aliceblue", size = 1),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = c(0.99, 0.01),
    legend.justification = c("right", "bottom"),
    legend.box.background = element_rect(color = "black", size = 1),
    legend.title = element_text(size = 10)
  ) +
  labs(
    title = "Culvert project density, all years",
    x = NULL,
    y = NULL
  )

setwd(wd)
setwd("./output/maps/culverts")
ggsave(
  filename = "fig_map_density.png",
  plot = map_density_allyears,
  device = "png",
  width = 8,
  height = 6
)

rm(sf_kde, map_density_allyears)

# Repeat by year in rolling fashion
# For contemporaneous projects and all previous projects
# We need to run st_kde for each year
# Then we need to use st_join with sf_culv for the appropriate year's kde

# Run st_kde for each year and map to a list of tibbles with sf geometries
list_years <- df_culv %>% distinct(completed_year) %>% pull(completed_year) %>% sort()

# Builds contemperanteous densities (year by year); note is 6.1 Gb in local memory so be sure to clear out before moving on
list_kde_contemp <-
  map(
    list_years,
    function(year) {
      sf_culv_year <- sf_culv %>% filter(completed_year == year)
      sf_kde_year <- st_kde(sf_culv_year)
      return(sf_kde_year)
    }
  )
names(list_kde_contemp) <- list_years

# Assign densities based on appropriate year
list_culv_byyear <-
  sf_culv %>%
  group_by(completed_year) %>%
  group_split()
names(list_culv_byyear) <- list_years

list_culv_byyear_density <-
  map2(
    list_culv_byyear,
    list_kde_contemp,
    st_join
  )
sf_culv_density <- do.call(rbind, list_culv_byyear_density)

# Clear out density estimate
rm(list_kde_contemp)

# Plot
# Set colors
blues_ramp <- colorRampPalette(brewer.pal(9, name = "Blues"))
distinct_vector <- unlist(mapply(brewer.pal, brewer.pal.info[brewer.pal.info$category == 'qual',]$maxcolors, rownames(brewer.pal.info[brewer.pal.info$category == 'qual',])))

ggplot(data = sf_culv_density) +
  geom_density(aes(x = density_10km2, fill = ordered(completed_year))) +
  scale_fill_discrete(expression(Culverts~per~10~km^2), palette = blues_ramp(length(list_years))) +
  facet_wrap(~ completed_year)

ggplot(data = sf_culv_density, aes(x = density_100km2, y = cost_per_culvert)) +
  geom_point(aes(color = project_source)) +
  geom_smooth(method = "lm", color = "black") +
  scale_color_discrete("Reporting source", palette = sample(distinct_vector, n_distinct(sf_culv_density, )))
  # scale_color_brewer() +
  # scale_y_log10()

# Builds cumulative version (points persistent)
list_kde_cumul <-
  map(
    list_years,
    function(year) {
      sf_culv_year <- sf_culv %>% filter(completed_year <= year)
      sf_kde_year <- st_kde(sf_culv_year)
      return(sf_kde_year)
    }
  )
names(list_kde_cumul) <- list_years
