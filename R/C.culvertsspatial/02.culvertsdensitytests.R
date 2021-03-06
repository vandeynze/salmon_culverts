# TITLE: Density analysis for culverts
# AUTHOR: Braeden Van Deynze
# DATE: July, 2020
# INPUTS: "culverts_full_mapping.csv"" data for culvert work site data
# OUTPUTS: density metrics for nearby culvert projects
# STATUS: Proof-of-concept is operable, may need optimization for run over full data
# PRIORITY: (1) Scale up for full data (2) Select thresholds (3) Move production code to the front and option out exploratory code into end matter, prep for sourcing

# Prepare environment ====
# Clear environment
rm(list = ls())

# Load libraries
library(MASS)
library(sf)
library(stars)
library(raster)
library(here)
library(janitor)
library(tidyverse)
library(RColorBrewer)


# Load and prepare culvert data ====
df_culv <- read_csv(here("output/culverts_full_mapping.csv"))


# Build kde function
# Estimates a kernel density estimate for a collection of projected points
# Can set the "cellarea" to adjust the size of the (square) cell over which the density is estimated
# Larger cellarea is faster to estimate
# Too small of cellarea would not really be useful
# How to choose cellarea? Hard to say...
# Value of function is a sf polygon collection of cells with culvert kernel density estimate and converted to different density units (culverts per XXkm2)
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
sf_culv <-
  df_culv %>%
  st_as_sf(coords = c("longitude", "latitude")) %>%
  st_set_crs(4326)

# Build smaller test culvert sf collection
# sf_culv_test <-
#   sf_culv %>%
#   st_crop(., y = c(ymin = 44, xmin = -124.5, ymax = 45, xmax = -123))

# Estimate kde
system.time(sf_kde <- st_kde(sf_culv, cellarea = 1e6))
# On my laptop w/ 16 GB RAM, 2.8GHz, i7-7700HQ CPU
# user  system elapsed 
# 203.08    4.11   208.12 
# Pretty quick, a few minutes

# system.time(sf_kde <- st_kde(sf_culv_test, cellarea = 1e5))
# Okay a LOT slower when you ramp down the cell size

# Plot
# Set colors
reds_ramp <- colorRampPalette(brewer.pal(9, name = "Reds"))

# Histogram
ggplot() +
  geom_histogram(data = sf_kde, aes(x = density_10km2), bins = 30, fill = reds_ramp(30)) +
  ggtitle("Distribution of cell culvert densities", "Large areas with few culverts, but large concentration in the tails")

# Map
(map_density_allyears <- 
  ggplot() +
    geom_sf(data = sf_base, fill = "antiquewhite1", size = 0.25) +
    geom_sf(data = sf_culv, size = 0.1) +
    geom_sf(
      data = 
        sf_kde %>% 
        filter(density_10km2 >= 0.5) %>%
        mutate(density_cut = 
                 base::cut(
                   density_10km2,
                   breaks = 
                     c(0, 5, 10, 15, 20, Inf),
                   ordered_result = TRUE,
                   right = FALSE
                 )
        ) %>% 
        group_by(density_cut) %>% summarize(),
      aes(color = density_cut),
      size = 1,
      fill = NA
    ) +
    scale_color_brewer(expression(Culverts~per~10~km^2), palette = "Reds", direction = 1) +
    # scale_alpha(range = c(0, 1)) +
    coord_sf(
      xlim = c(-124.5, -113.5),
      ylim = c(41, 49),
      expand = FALSE
    ) +
    theme_bw() +
    theme(
      panel.background = element_rect(fill = "aliceblue"),
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
    ))

ggsave(
  filename = here("output/maps/culverts/fig_map_density.png"),
  plot = map_density_allyears,
  device = "png",
  width = 8,
  height = 6
)


# Repeat by year in rolling fashion
# For contemporaneous projects and all previous projects
# We need to run st_kde for each year
# Then we need to use st_join with sf_culv for the appropriate year's kde


# Run st_kde for each year and map to a list of tibbles with sf geometries
list_years <- sf_culv %>% st_drop_geometry() %>% ungroup %>% distinct(project_year) %>% pull(project_year) %>% sort()
# list_years <- list_years[c(10:15)] # Short year list for testing

list_kde_contemp <-
  map(
    list_years,
    function(year) {
      sf_culv_year <- sf_culv %>% filter(project_year == year)
      sf_kde_year <- st_kde(sf_culv_year)
      return(sf_kde_year)
    }
  )
names(list_kde_contemp) <- list_years

# Assign densities based on appropriate year
list_culv_byyear <-
  sf_culv %>%
  group_by(project_year) %>%
  filter(project_year %in% list_years) %>%
  group_split()
names(list_culv_byyear) <- list_years

list_culv_byyear_density <-
  map2(
    list_culv_byyear,
    list_kde_contemp,
    st_join
  )
sf_culv <- do.call(rbind, list_culv_byyear_density)

# Clear out density estimate
rm(list_kde_contemp, list_culv_byyear, list_culv_byyear_density)

# Plot
ggplot() +
  geom_sf(data = sf_base, fill = "antiquewhite1", size = 0.15) +
  geom_sf(aes(color = density_10km2), data = sf_culv %>% filter(project_year %in% list_years)) +
  scale_color_distiller(expression(Culverts~per~10~km^2), palette = "Reds", direction = 1) +
  coord_sf(
    xlim = c(-124.5, -113.5),
    ylim = c(41, 49),
    expand = FALSE
  ) +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "aliceblue", size = 1),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "right",
    legend.justification = c("right", "bottom"),
    legend.box.background = element_rect(color = "black", size = 1),
    legend.title = element_text(size = 10)
  ) +
  facet_wrap(~ project_year) +
  ggtitle("Culvert work site density, contemperaneous projects, by year")
ggsave(
  here("output","maps","culverts","fig_map_density_contemp_byyear.png"),   
  device = "png",
  width = 8,
  height = 6
)

ggplot(sf_culv %>% filter(state %in% c("ID", "OR", "WA"))) +
  geom_histogram(aes(density_10km2, fill = stat(count)), bins = 30) +
  scale_fill_distiller("", direction = 1, palette = "Reds") +
  # theme_void() +
  theme(legend.position = "none", axis.title.y = element_blank(), axis.ticks.y = element_blank(), panel.grid = element_blank(), axis.text.y = element_blank()) +
  ggtitle("Distribution of cell culvert densities, contemperaneous projects", "Much denser concentration in OR, especially in '00s") +
  xlab("Density [Work sites per 10km2]") +
  facet_grid(project_year ~ state)
ggsave(
  here("output","figs","culverts","fig_density_contemp_byyear.png"),   
  device = "png",
  width = 8,
  height = 6
)

# Builds cumulative version (points persistent)
list_kde_cumul <-
  map(
    list_years,
    function(year) {
      sf_culv_year <- sf_culv %>% filter(project_year <= year)
      sf_kde_year <- st_kde(sf_culv_year)
      return(sf_kde_year)
    }
  )
names(list_kde_cumul) <- list_years

# Assign densities based on appropriate year
list_culv_byyear <-
  sf_culv %>%
  group_by(project_year) %>%
  group_split()
names(list_culv_byyear) <- list_years

list_culv_byyear_density <-
  map2(
    list_culv_byyear,
    list_kde_cumul,
    st_join,
    suffix = c("_contemp", "_cummul")
  )
sf_culv <- do.call(rbind, list_culv_byyear_density)

# Plot
ggplot() +
  geom_sf(data = sf_base, fill = "antiquewhite1") +
  geom_sf(aes(color = density_10km2_cummul), data = sf_culv %>% filter(project_year %in% list_years)) +
  scale_color_distiller(expression(Culvert~work~sites~per~10~km^2~(up~to~year)), palette = "Reds", direction = 1) +
  coord_sf(
    # xlim = c(-124.5, -123),
    # ylim = c(44, 45),
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
  facet_wrap(~ project_year)

# Buffer method for raw count ====
# Based on discussion found here: https://lbusettspatialr.blogspot.com/2018/02/speeding-up-spatial-analyses-by.html
# Just the basic st_intersects version for now, but method using data.table might be more effective for full runs
# sf_culv <- sf_culv_test
radius_list = c(1/1200, 1/600, 1/120, 1/60, 1/12)
# 0.5km, 1km, 5km, 10km, 50km

# Prepare years in order for easy (sloppy) merging
sf_culv <- sf_culv %>% arrange(project_year)
sf_culv_buff <- sf_culv_buff %>% arrange(project_year)
(years = sf_culv %>% pull(project_year) %>% unique())

for(i in length(radius_list)) {
radius = radius_list[i]
sf_culv_buff <- st_buffer(sf_culv, radius)
(radius_units = round(mean(sf_culv_buff %>% st_length %>% units::set_units(km)), 2))
(sf_culv_int <- st_intersects(sf_culv_buff, sf_culv))
(sf_culv_int <- 
  tibble(
    id = sf_culv_buff$worksite_id,
    int_ids = lapply(sf_culv_int, FUN = function(x) sf_culv_buff$worksite_id[x])
  )%>%
  rowwise() %>%
  mutate(
    int_count = length(int_ids)
  ))

# Looks good, let's merge the data and map it
sf_culv <-
  sf_culv %>%
  left_join(sf_culv_int %>% select(id, paste0("int_count_", radius_units) = int_count), by = c("worksite_id" = "id"))

# Histogram
ggplot(sf_culv) + geom_histogram(aes(x = paste0("int_count_", radius_units)), color = NA, fill = reds_ramp(30)) + ggtitle(paste0("Distribution of culvert work sites within ", radius_units,"km, across all years"))

# Map
ggplot() +
  geom_sf(data = sf_base, fill = "antiquewhite1") +
  geom_sf(aes(color = paste0("int_count_", radius_units)), data = sf_culv) +
  geom_sf(aes(linetype = "Example buffer"), data = sf_culv_buff %>% slice(100), fill = NA, size = 1, color = "red") +
  scale_color_distiller(paste0("Culvert work sites within ", radius_units, "km (all years)"), palette = "Reds", direction = 1) +
  scale_linetype(NULL) +
  coord_sf(
    xlim = c(-124.5, -123),
    ylim = c(44, 45),
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
  )

# Extend to contemp. and cummul. versions
# Find points within 50 km wrt each point contemporaneously
(sf_culv_int_contemp <- map(years, ~st_intersects(sf_culv_buff %>% filter(project_year == .x), sf_culv %>% filter(project_year == .x))) %>% unlist(recursive = FALSE))
(sf_culv_int_contemp <- 
    tibble(
      id = sf_culv_buff$worksite_id,
      int_ids_contemp = map(sf_culv_int_contemp, ~ sf_culv_buff$worksite_id[.x])
    )%>%
    rowwise() %>%
    mutate(
      int_count_contemp = length(int_ids_contemp)
    ))

# Looks good, let's merge the data
sf_culv <-
  sf_culv %>%
  left_join(sf_culv_int_contemp %>% select(id, paste0("int_count_contemp_", radius_units) = int_count_contemp), by = c("worksite_id" = "id"))

# Find points within 50 km wrt each point cummulatively
(sf_culv_int_cummul <- map(years, ~st_intersects(sf_culv_buff %>% filter(project_year <= .x), sf_culv %>% filter(project_year <= .x))) %>% unlist(recursive = FALSE))
(sf_culv_int_cummul <- 
    tibble(
      id = sf_culv_buff$worksite_id,
      int_ids_cummul = map(sf_culv_int_cummul, ~ sf_culv_buff$worksite_id[.x])
    )%>%
    rowwise() %>%
    mutate(
      int_count_cummul = length(int_ids_cummul)
    ))

# Looks good, let's merge the data
sf_culv <-
  sf_culv %>%
  left_join(sf_culv_int_cummul %>% select(id, paste0("int_count_cummul_", radius_units) = int_count_cummul), by = c("worksite_id" = "id"))
}

# Can adjust the dist argument of the buffer to pick different radiuses
# TODO: Extend to contemp. and cummul. versions and prepare full output frame for saving
# TODO: Run on full sample of culvert work sites



# Save out full data ====
sf_culv %>%
  st_drop_geometry() %>%
  write_csv(here("output/spatial/culverts_density.csv"))
