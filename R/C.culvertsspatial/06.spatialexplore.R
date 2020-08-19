#' ---
#' title: "Explore spatially-derived variables for culvert work sites"
#' author: "B. Van Deynze"
#' date: "Aug. 18th, 2020"
#' output:
#'    html_document:
#'       toc: true
#'       toc_float:
#'          collapsed: true
#' ---

#' This document summarizes spatially-derived variables for PNSHP culvert work
#' sites. These variables are collected by linking the work site points to other
#' spatial data sets via polygon covers or identifying the nearest line. As of
#' right now, we have data from Blake on roads, streams, habitat use, slope, and
#' land cover. I also have collected some county-level data on employment levels
#' in relevant sectors.

#+ warning=F, message=F, echo=F
# TITLE: Explore spatially derived variables for culvert work sites
# AUTHORS: B. Van Deynze
# DATE: Aug. 2020
# INPUTS: culverts_full_spatial.csv in /output/ directory
# OUTPUTS: HTML report

# Prepare environment ----
rm(list = ls())

library(scales)
library(tidyverse)
library(here)
library(sf)
library(readxl)
library(janitor)
library(rmarkdown)
library(knitr)
library(kableExtra)

# Wraps long subtitles in ggplot
wrapper <- function(label, dev_width = dev.size("in")[1], dev_scaler = 12)  {   
  paste(strwrap(label, dev_width * dev_scaler), collapse = "\n") 
}

# Load and prep data ----
sf_culv <- 
  read_csv(here("output/culverts_full_spatial.csv")) %>%
  st_as_sf(
    coords = c('longitude', 'latitude'),
    crs = 4326
  )

# Identify nearest NLCD to project date
sf_culv <-
  sf_culv %>%
  rowwise() %>%
  mutate(
    across(
      c(starts_with("nlcd"), nhd_ftype, here_speed, here_class),
      factor
    )
  ) %>%
  mutate(
    nlcd_current = case_when(
      project_year <= 2001 ~ nlcd_2001,
      2001 < project_year & project_year <= 2004 ~ nlcd_2004,
      2004 < project_year & project_year <= 2006 ~ nlcd_2006,
      2006 < project_year & project_year <= 2008 ~ nlcd_2008,
      2008 < project_year & project_year <= 2011 ~ nlcd_2011,
      2011 < project_year & project_year <= 2013 ~ nlcd_2013,
      2013 < project_year & project_year <= 2016 ~ nlcd_2016
    ),
    nhd_dist_m = case_when(nhd_dist_m < 0 ~ NA_real_, TRUE ~ nhd_dist_m),
    snet_distm = case_when(snet_distm < 0 ~ NA_real_, TRUE ~ snet_distm),
    slope_deg = case_when(slope_deg < 0 ~ NA_real_, TRUE ~ slope_deg)
  )

# We're ready to plot!

# Plot for visualization ----

#' ## Evaluating accuracy of stream and road matches  
#'  
#' First thing we'll do is check how accurate the culvert work site points match
#' up with the road and stream data recovered from three sources: (1)
#' [HERE](https://gii.dhs.gov/hifld/sites/default/files/2019-08/HIFLD%20Licensed%20HERE%20One%20Pager.pdf)
#' for network-able road features, (2) [NHDPlus HR](https://hydro.nationalmap.gov/arcgis/rest/services/NHDPlus_HR/MapServer)
#' for stream/river classification and features, (3)
#' [StreamNet](https://www.streamnet.org/data/interactive-maps-and-gis-data/) for
#' salmonid habitat use data by species. We will evaluate the precision of
#' matches using distance to the "matching" line, measured in meters.  
#'  
#' We suspect two possible sources of imprecision. First, work site points may be
#' reported with imprecision, in which case the match would represent the most
#' likely feature, but with less certainty than if distance was lower. Second,
#' the work site may be on a road or stream that is not represented in our source
#' data. In this case, the matched feature would misrepresent the true feature we
#' are trying to identify for the work site.  
#'  
#' In either case, we will need to identify and decide how to deal with "poor"
#' matches. For now, we will define poor matches as work sites over 150m from the
#' target feature (i.e. road or stream), represented in the figures below with a
#' red horizontal line.  

#+ fig.width=10, fig.height=6, echo=F, message=F
# Identify good matches for streams and roads ----
# HERE roads
sf_culv %>%
  group_by(
    project_year,
    basin
  ) %>%
  # select(
  # summarise(
  #   n = n(),
  #   n_pure = sum(pure_culv == TRUE),
  #   across(
  #     c(
  #       where(is.numeric),
  #       -c(
  #         # project_year,
  #         fips,
  #         state_fips,
  #         nhd_r_code,
  #         n,
  #         n_pure
  #       )
  #     ),
  #     list(
  #       mean = ~mean(., na.rm = TRUE), sd = ~sd(., na.rm = TRUE), min = ~min(., na.rm = TRUE), max = ~max(., na.rm = TRUE)
  #     )
  #   ),
  #   .groups = "keep"
  # ) %>%
  add_count() %>% ungroup() %>%
  add_count(basin, name = "n_basin") %>%
  filter(
    # pure_culv == TRUE,
    n_basin > 90
  ) %>%
  drop_na(here_distm) %>%
  ggplot(
    aes(
      x = project_year,
      y = here_distm,
      color = basin,
      fill = basin
    )
  ) +
  geom_violin(aes(group = project_year), fill = "white", na.rm = TRUE, scale = "width") +
  geom_jitter(aes(size = 10/n), width = 0.2) +
  geom_hline(yintercept = 150, color = "red") +
  geom_label(aes(label = paste("n =", n)), x = 1997, y = 4000, fill = "white", color = "black", size = 3, data = sf_culv %>% count(basin) %>% filter(n > 90)) +
  theme(legend.position = "none") +
  scale_size_continuous(range = c(0.1,1)) +
  theme(legend.position = "none") +
  facet_wrap("basin") +
  ggtitle(
    "Distance to nearest HERE road (m), grouped by basin",
    "Most (> 70%) culvert points are within 150m of a road object; Rest could be supplemented with OSM roads"
  )
# Number of "good" matches
# sum(I(sf_culv$here_distm <= 150))/nrow(sf_culv)

# NHDPlus streams
sf_culv %>%
  group_by(
    project_year,
    basin
  ) %>%
  # select(
  # summarise(
  #   n = n(),
  #   n_pure = sum(pure_culv == TRUE),
  #   across(
  #     c(
  #       where(is.numeric),
  #       -c(
  #         # project_year,
  #         fips,
  #         state_fips,
#         nhd_r_code,
#         n,
#         n_pure
#       )
#     ),
#     list(
#       mean = ~mean(., na.rm = TRUE), sd = ~sd(., na.rm = TRUE), min = ~min(., na.rm = TRUE), max = ~max(., na.rm = TRUE)
#     )
#   ),
#   .groups = "keep"
# ) %>%
  add_count() %>% ungroup() %>%
  add_count(basin, name = "n_basin") %>%
  filter(
    # pure_culv == TRUE,
    n_basin > 90
  ) %>%
  drop_na(nhd_dist_m) %>%
  ggplot(
    aes(
      x = project_year,
      y = nhd_dist_m,
      color = basin,
      fill = basin
    )
  ) +
  geom_violin(aes(group = project_year), fill = "white", na.rm = TRUE, scale = "width") +
  geom_jitter(aes(size = 1/n), width = 0.2) +
  geom_hline(yintercept = 150, color = "red") +
  geom_label(aes(label = paste("n =", n)), x = 1997, y = 850, fill = "white", color = "black", size = 3, data = sf_culv %>% count(basin) %>% filter(n > 90)) +
  theme(legend.position = "none") +
  scale_size_continuous(range = c(0.1,1)) +
  theme(legend.position = "none") +
  facet_wrap("basin") +
  ggtitle(
    "Distance to nearest NHD+ stream (m), grouped by basin",
    wrapper("Nearly all (> 90%) culvert points are within 150m of a stream object; Many of those > 150m are culvert projects grouped in with other road maintenance classified work like drainage improvement so match rate strictly for culvert work sites is even better")
  )
# Number of "good" matches
# sum(I(sf_culv$nhd_dist_m <= 150), na.rm = TRUE)/nrow(sf_culv)

# StreamNet streams
sf_culv %>%
  group_by(
    project_year,
    basin
  ) %>%
  # select(
  # mutate(
  # summarize(
    # n = n(),
  #   n_pure = sum(pure_culv == TRUE),
  # across(
  #   c(
  #     where(is.numeric),
  #     -c(
  #       # project_year,
  #       fips,
  #       state_fips,
  #       nhd_r_code,
  #       n,
  #       n_pure
  #     )
  #   ),
  #   list(
  #     mean = ~mean(., na.rm = TRUE), sd = ~sd(., na.rm = TRUE), min = ~min(., na.rm = TRUE), max = ~max(., na.rm = TRUE)
  #   )
  # ),
  # .groups = "keep"
  # ) %>%
  add_count() %>% ungroup() %>%
  add_count(basin, name = "n_basin") %>%
  filter(
    # pure_culv == TRUE,
    n_basin > 90
  ) %>%
  filter(
    # pure_culv == TRUE,
    n > 10
  ) %>%
  drop_na(snet_distm) %>%
  ggplot(
    aes(
      x = project_year,
      y = snet_distm,
      color = basin,
      fill = basin
    )
  ) +
  geom_violin(aes(group = project_year), fill = "white", na.rm = TRUE, scale = "width") +
  geom_jitter(aes(size = 1/n), width = 0.2) +
  geom_hline(yintercept = 150, color = "red") +
  geom_label(aes(label = paste("n =", n)), x = 1997, y = 4500, fill = "white", color = "black", size = 3, data = sf_culv %>% count(basin) %>% filter(n > 90)) +
  theme(legend.position = "none") +
  scale_size_continuous(range = c(0.1,1)) +
  facet_wrap("basin") +
  ggtitle(
    "Distance to nearest StreamNet stream (m), grouped by basin",
    wrapper("Not as good of matches for StreamNet, only ~50% culvert points are within 150 meters of a stream object; Probably less important, as we can apply ESU area overlays based on HUC code")
  )
# Number of "good" matches
# sum(I(sf_culv$snet_distm <= 150), na.rm = TRUE)/nrow(sf_culv)

#' So StreamNet's less dense streams have the worst matches, while NHD+ has very
#' close matches. StreamNet is most useful for salmon habitat identification,
#' though HUC code based habitat maps can be used instead (e.g. maps seen in
#' [Crozier et al. 2018](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0217711)).  
#'   
#' The HERE roads have lots of close matches, but also plenty that are far off
#' the mark. We can supplement with [OpenStreetMaps (OSM)](https://wiki.openstreetmap.org/wiki/Main_Page) road data, which appears
#' to be more dense than the HERE network.  

#+
#' ## Examine road features  
#'   
#' The HERE data provides variables for *speed class*, *class*, *paved*, and
#' *public*. Paved status, speed class, and road type are potential cost
#' drivers. Plots for all three are provided below.

#+ fig.width=10, fig.height=6, echo=F, message=F
# Examine HERE road data ----
# Speed class
# Basins with more than 300 work sites
sf_culv %>%
  ungroup() %>%
  add_count(basin, name = "n_basin") %>% filter(n_basin > 300) %>%
  group_by(basin, n_basin, project_year, here_speed) %>% count() %>%
  ggplot(
    aes(
      x = project_year,
      y = n,
      fill = ordered(as.character(here_speed))
    )
  ) +
  geom_col() +
  geom_label(aes(label = paste("n =", comma(n))), x = 1997, y = 300, fill = "white", color = "black", size = 3, data = sf_culv %>% count(basin) %>% filter(n > 400)) +
  scale_fill_discrete("HERE Speed Class") +
  facet_wrap("basin") +
  ggtitle(
    "HERE road speed class, grouped by basin (basins with more than 300 work sites)",
    wrapper("Most road in the well represented basins are class 7, with a number of lower classes and some higher lasses in the Willamette")
  )

# Basins w/ less than 300 work sites
sf_culv %>%
  ungroup() %>%
  add_count(basin, name = "n_basin") %>% filter(n_basin < 300 & n_basin > 30) %>%
  group_by(basin, n_basin, project_year, here_speed) %>% count() %>%
  ggplot(
    aes(
      x = project_year,
      y = n,
      fill = ordered(as.character(here_speed))
    )
  ) +
  geom_col() +
  geom_label(aes(label = paste("n =", n)), x = 1997, y = 45, fill = "white", color = "black", size = 3, data = sf_culv %>% count(basin) %>% filter(n < 400 & n > 30)) +
  scale_fill_discrete("HERE Speed Class") +
  facet_wrap("basin") +
  ggtitle(
    "HERE road speed class, grouped by basin (basins with fewer than 300 work sites)",
    wrapper("Lots of diversity in speed classes in the less represented basins; It appears that 0 is the NULL value and there are missing values for the Clearwater and Salmon basins")
  )

# Road class
# Basins with more than 300 work sites
sf_culv %>%
  ungroup() %>%
  add_count(basin, name = "n_basin") %>% filter(n_basin > 300) %>%
  group_by(basin, n_basin, project_year, here_class) %>% count() %>%
  ggplot(
    aes(
      x = project_year,
      y = n,
      fill = ordered(as.character(here_class))
    )
  ) +
  geom_col() +
  geom_label(aes(label = paste("n =", comma(n))), x = 1997, y = 300, fill = "white", color = "black", size = 3, data = sf_culv %>% count(basin) %>% filter(n > 400)) +
  scale_fill_discrete("HERE Road Class") +
  facet_wrap("basin") +
  ggtitle(
    "HERE road class, grouped by basin (basins with more than 300 work sites)",
    wrapper("Almost all roads are class 5")
  )

# Basins w/ less than 300 work sites
sf_culv %>%
  ungroup() %>%
  add_count(basin, name = "n_basin") %>% filter(n_basin < 300 & n_basin > 30) %>%
  group_by(basin, n_basin, project_year, here_class) %>% count() %>%
  ggplot(
    aes(
      x = project_year,
      y = n,
      fill = ordered(as.character(here_class))
    )
  ) +
  geom_col() +
  geom_label(aes(label = paste("n =", n)), x = 1997, y = 45, fill = "white", color = "black", size = 3, data = sf_culv %>% count(basin) %>% filter(n < 400 & n > 30)) +
  scale_fill_discrete("HERE Road Class") +
  facet_wrap("basin") +
  ggtitle(
    "HERE road class, grouped by basin (basins with fewer than 300 work sites)",
    wrapper("Slightly more diversity in the less represented basins, with a smattering of class 4 roads; Salmon and Clearwater still missing")
  )

# Paved class
# Basins with more than 300 work sites
sf_culv %>%
  ungroup() %>%
  add_count(basin, name = "n_basin") %>% filter(n_basin > 300) %>%
  group_by(basin, n_basin, project_year, here_paved) %>% count() %>%
  ggplot(
    aes(
      x = project_year,
      y = n,
      fill = here_paved
    )
  ) +
  geom_col() +
  geom_label(aes(label = paste("n =", comma(n))), x = 1997, y = 300, fill = "white", color = "black", size = 3, data = sf_culv %>% count(basin) %>% filter(n > 400)) +
  scale_fill_discrete("HERE Paved") +
  facet_wrap("basin") +
  ggtitle(
    "HERE paved classification, grouped by basin (basins with more than 300 work sites)",
    wrapper("Most roads with culvert work sites are not paved, but a good number of paved roads as well")
  )

# Basins w/ less than 300 work sites
sf_culv %>%
  ungroup() %>%
  add_count(basin, name = "n_basin") %>% filter(n_basin < 300 & n_basin > 30) %>%
  group_by(basin, n_basin, project_year, here_paved) %>% count() %>%
  ggplot(
    aes(
      x = project_year,
      y = n,
      fill = here_paved
    )
  ) +
  geom_col() +
  geom_label(aes(label = paste("n =", n)), x = 1997, y = 45, fill = "white", color = "black", size = 3, data = sf_culv %>% count(basin) %>% filter(n < 400 & n > 30)) +
  scale_fill_discrete("HERE Paved") +
  facet_wrap("basin") +
  ggtitle(
    "HERE paved classification, grouped by basin (basins with fewer than 300 work sites)",
    wrapper("More paved representation, especially in Washington; This might explain a lot of why Washington reported costs are so much higher; Salmon and Clearwater still missing")
  )

#+
#' ## Salmon use of streams with work sites  
#'   
#' The StreamNet data provides variables for *species* and *habitat use*. These
#' variables will be valuable for modeling benefits of culvert improvement, but
#' may also lead to stricter design requirements if in the habitat of ESA listed
#' species.  

#+ fig.width=10, fig.height=6, echo=F, message=F
# Examine StreamNet data ----
# Species
# Basins with more than 300 work sites
sf_culv %>%
  ungroup() %>%
  add_count(basin, name = "n_basin") %>% filter(n_basin > 300) %>%
  group_by(basin, n_basin, project_year, snet_spp) %>% count() %>%
  ggplot(
    aes(
      x = project_year,
      y = n,
      fill = snet_spp
    )
  ) +
  geom_col() +
  geom_label(aes(label = paste("n =", comma(n))), x = 1997, y = 300, fill = "white", color = "black", size = 3, data = sf_culv %>% count(basin) %>% filter(n > 400)) +
  scale_fill_discrete("StreamNet species") +
  facet_wrap("basin") +
  ggtitle(
    "StreamNet species, grouped by basin (basins with more than 300 work sites)",
    wrapper("Steelhead and Coho are the most well represented, with plenty of culverts in Chinook territory as well")
  )

# Basins w/ less than 300 work sites
sf_culv %>%
  ungroup() %>%
  add_count(basin, name = "n_basin") %>% filter(n_basin < 300 & n_basin > 30) %>%
  group_by(basin, n_basin, project_year, snet_spp) %>% count() %>%
  ggplot(
    aes(
      x = project_year,
      y = n,
      fill = snet_spp
    )
  ) +
  geom_col() +
  geom_label(aes(label = paste("n =", comma(n))), x = 1997, y = 45, fill = "white", color = "black", size = 3, data = sf_culv %>% count(basin) %>% filter(n < 300 & n > 30)) +
  scale_fill_discrete("StreamNet species") +
  facet_wrap("basin") +
  ggtitle(
    "StreamNet species, grouped by basin (basins with more than 300 work sites)",
    wrapper("Steelhead and Coho are sitll the most commonly represented")
  )

# Species
# Basins with more than 300 work sites
sf_culv %>%
  ungroup() %>%
  add_count(basin, name = "n_basin") %>% filter(n_basin > 300) %>%
  group_by(basin, n_basin, project_year, snet_use) %>% count() %>%
  ggplot(
    aes(
      x = project_year,
      y = n,
      fill = snet_use
    )
  ) +
  geom_col() +
  geom_label(aes(label = paste("n =", comma(n))), x = 1997, y = 300, fill = "white", color = "black", size = 3, data = sf_culv %>% count(basin) %>% filter(n > 400)) +
  scale_fill_discrete("StreamNet use") +
  facet_wrap("basin") +
  ggtitle(
    "StreamNet habitat use, grouped by basin (basins with more than 300 work sites)",
    wrapper("Work sites are almost exclusively near spawning and rearing streams")
  )

# Basins w/ less than 300 work sites
sf_culv %>%
  ungroup() %>%
  add_count(basin, name = "n_basin") %>% filter(n_basin < 300 & n_basin > 30) %>%
  group_by(basin, n_basin, project_year, snet_use) %>% count() %>%
  ggplot(
    aes(
      x = project_year,
      y = n,
      fill = snet_use
    )
  ) +
  geom_col() +
  geom_label(aes(label = paste("n =", comma(n))), x = 1997, y = 45, fill = "white", color = "black", size = 3, data = sf_culv %>% count(basin) %>% filter(n < 300 & n > 30)) +
  scale_fill_discrete("StreamNet use") +
  facet_wrap("basin") +
  ggtitle(
    "StreamNet habitat use, grouped by basin (basins with more than 300 work sites)",
    wrapper("Work sites are most commonly near spawning and rearing streams, though in Washington they are frequently on migration streams")
  )

#+
#' Key takeaway is that the culverts projects in Washington and Oregon are on
#' distinctly different streams. In Washington, reported projects are more
#' frequently on migration streams. These streams are likely to open up more
#' upstream territory, but may also be wider and require more complex culvert
#' designs. In Oregon, the reported projects are overwhelmingly on spawning and
#' rearing streams, which (I believe) tend to be smaller and therefore woud
#' require less complex designs.  

#+
#' ## Slope at work site  
#'   
#' The slope at the work site may increase project cost by restricting access to
#' the culvert or by requiring a more complex culvert design. We measure slope
#' using the slope in degrees of the
#' [GTOPO30](https://www.usgs.gov/centers/eros/science/usgs-eros-archive-digital-elevation-global-30-arc-second-elevation-gtopo30?qt-science_center_objects=0#qt-science_center_objects)
#' grid cell the work site falls in.  

#+ fig.width=8, fig.height=6, echo=F, message=F
# Examine slope ----
# Slope
sf_culv %>%
  group_by(basin, project_year) %>%
  add_count() %>%
  ungroup() %>%
  add_count(basin, name = "n_basin") %>%
  filter(
    # pure_culv == TRUE,
    n_basin > 90
  ) %>%
  drop_na(slope_deg) %>%
  ggplot(
    aes(
      x = project_year,
      y = slope_deg,
      color = basin,
      fill = basin
    )
  ) +
  geom_violin(aes(group = project_year), fill = "white", na.rm = TRUE, scale = "width") +
  geom_jitter(aes(size = 1/n), width = 0.2) +
  # geom_label(aes(label = paste("n =", n)), x = 2014, y = 2250, fill = "white", color = "black", size = 3, data = sf_culv %>% count(basin) %>% filter(n > 30)) +
  theme(legend.position = "none") +
  scale_size_continuous(range = c(0.1,1)) +
  facet_wrap("basin") +
  ggtitle(
    "Slope at work site, grouped by basin",
    wrapper("Plenty of variation over time and space, even within basins")
  )

#+
#' ## Land cover at work sites  
#'   
#' Land cover can proxy for land use, which can effect how difficult it is to
#' access and perform construction activities at the work site, which may
#' increase costs. It may also be correlated with road features (surface
#' material, typical traffic, etc.) or stream features, and could be useful for
#' providing more information for work sites where matches to roads and streams
#' are poor.  
#'  
#' The table below presents the NLCD land cover classifications. We use the "class" level of detail in the figures that follow.
 
#+ fig.width=10, fig.height=6, echo=F, message=F
# Examine land cover data ----
(key_nlcd <-
    read_xlsx(here("data/Culverts spatial overlays v 06Aug2020.xlsx"), sheet = 3) %>% as_tibble() %>% clean_names()
) %>%
  # print(n = 20)
  knitr::kable() %>%
  kableExtra::kable_styling()

# Basins with more than 300 work sites
sf_culv %>%
  mutate(nlcd_current = as.numeric(as.character(nlcd_current))) %>%
  left_join(
    key_nlcd,
    by = c("nlcd_current" = "value")
  ) %>%
  rename(
    nlcd_class = class,
    nlcd_classfull = classification,
    nlcd_description = description
  ) %>%
  ungroup() %>%
  add_count(basin, name = "n_basin") %>% filter(n_basin > 300) %>%
  group_by(basin, n_basin, project_year, nlcd_class) %>% count() %>%
  ggplot(
    aes(
      x = project_year,
      y = n,
      fill = nlcd_class
    )
  ) +
  geom_col() +
  geom_label(aes(label = paste("n =", comma(n))), x = 1997, y = 300, fill = "white", color = "black", size = 3, data = sf_culv %>% count(basin) %>% filter(n > 400)) +
  scale_fill_discrete("NLCD Class") +
  facet_wrap("basin") +
  ggtitle(
    "NLCD land cover class, grouped by basin (basins with more than 300 work sites)",
    wrapper("Forest dominates, with Developed also well represented; Planted-Cultivated, Shrubland, and Wetlands also have some representation")
  )

# Basins w/ less than 300 work sites
sf_culv %>%
  mutate(nlcd_current = as.numeric(as.character(nlcd_current))) %>%
  left_join(
    key_nlcd,
    by = c("nlcd_current" = "value")
  ) %>%
  rename(
    nlcd_class = class,
    nlcd_classfull = classification,
    nlcd_description = description
  ) %>%
  ungroup() %>%
  add_count(basin, name = "n_basin") %>% filter(n_basin < 300 & n_basin > 30) %>%
  group_by(basin, project_year, nlcd_class) %>% count() %>%
  ggplot(
    aes(
      x = project_year,
      y = n,
      fill = nlcd_class
    )
  ) +
  geom_col() +
  geom_label(aes(label = paste("n =", n)), x = 1997, y = 45, fill = "white", color = "black", size = 3, data = sf_culv %>% count(basin) %>% filter(n < 400 & n > 30)) +
  scale_fill_discrete("NLCD Class") +
  facet_wrap("basin") +
  ggtitle(
    "NLCD land cover class, grouped by basin (basins with fewer than 300 work sites)",
    wrapper("Land cover is more diverse in less-represented basins; Forest still dominates, though Developed is well represented in the Puget Sound")
  )

#+
#' ## Job shares in relevant sectors  
#'   
#' To capture labor market patterns that might affect culvert costs, we calculate
#' the share of total employment for construction and ag/forestry NAICS codes for
#' the county the work site is located in via [County Business Patterns](https://www.census.gov/programs-surveys/cbp.html).  

#+ fig.width=8, fig.height=6, echo=F, message=F
# Examine employment levels ----
# Employment in ag/forestry firms
sf_culv %>%
  group_by(basin, project_year) %>%
  add_count() %>%
  ungroup() %>%
  add_count(basin, name = "n_basin") %>%
  filter(
    # pure_culv == TRUE,
    n_basin > 90
  ) %>%
  drop_na(emp_agforest) %>%
  ggplot(
    aes(
      x = project_year,
      y = emp_agforest,
      color = basin,
      fill = basin
    )
  ) +
  geom_violin(aes(group = project_year), fill = "white", na.rm = TRUE, scale = "width") +
  geom_jitter(aes(size = 1/n), width = 0.2) +
  # geom_label(aes(label = paste("n =", n)), x = 2014, y = 2250, fill = "white", color = "black", size = 3, data = sf_culv %>% count(basin) %>% filter(n > 30)) +
  theme(legend.position = "none") +
  scale_size_continuous(range = c(0.1,1)) +
  facet_wrap("basin", scales = "free_y") +
  ggtitle(
    "Employment at ag or forestry firms, grouped by basin",
    wrapper("Plenty of variation over time and space, even within basins")
  )

# Employment in construction firms
sf_culv %>%
  group_by(basin, project_year) %>%
  add_count() %>%
  ungroup() %>%
  add_count(basin, name = "n_basin") %>%
  filter(
    # pure_culv == TRUE,
    n_basin > 90
  ) %>%
  drop_na(emp_const) %>%
  ggplot(
    aes(
      x = project_year,
      y = emp_const,
      color = basin,
      fill = basin
    )
  ) +
  geom_violin(aes(group = project_year), fill = "white", na.rm = TRUE, scale = "width") +
  geom_jitter(aes(size = 1/n), width = 0.2) +
  # geom_label(aes(label = paste("n =", n)), x = 2014, y = 60000, fill = "white", color = "black", size = 3, data = sf_culv %>% count(basin) %>% filter(n > 30)) +
  theme(legend.position = "none") +
  scale_size_continuous(range = c(0.1,1)) +
  facet_wrap("basin", scales = "free_y") +
  ggtitle(
    "Employment at construction firms, grouped by basin",
    wrapper("Plenty of variation over time and space, even within basins, but levels much higher in Puget Sound (obviously); Levels track more closely to larger economic conditions than ag/forestry jobs")
  )

# Other notes ----
#+
#' ## Key takeaways  
#'   
#' There are noticeable differences between Washington and Oregon culvert
#' projects represented in the data. Washington projects tend to more frequently
#' be on migration routes and on paved roads. Oregon projects are more likely to
#' be on spawning and rearing streatms and unpaved roads. This likely goes a
#' long way in explaining the large differences in costs across the two states.  
#'  
#' ## Questions and next steps  
#'  
#' ### Questions  
#'  
#' 1. StreamNet data provided only identifies one species. Are there overlapping
#' species ranges? Could this column be provided as a list of species?  
#' 2. No descriptions of HERE road feature variables are provided. Where can
#' these descriptions be found?  
#'  
#' ### Next steps  
#'  
#' 1. NHDPlus stream matches for all culverts in the Clearwater basin are
#' missing, as well as all from the Salmon basin and almost all from the Spokane
#' basin. NHDPlus data should be recovered for these regions.  
#' 2. Execute code to generate measures for...
#'   a. Upstream habitat potential and other stream data from NHDPlus,  
#'   b. Nearest OSM road,  
#'   c. Density of nearby culvert work sites, and  
#'   d. ESU habitat ranges from HUC boundaries.
#' 3. Clean up identification of "pure" culvert projects (i.e. projects that include non-culvert work in addition to culvert improvements).  
#' 4. Integrate new spatial data into cost models.  

#+
#' ## End Matter  
# Created on...
Sys.time()

# Using...
sessionInfo()

# Render output
# render(here("R/C.culvertsspatial/06.spatialexplore.R"), output_file = here("output/culvertsspatial_report_2020aug18.html"))
