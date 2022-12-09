#' ---
#' title: "Summary of Spatially Derived Variables for Culvert Projects"
#' author: "B. Van Deynze"
#' date: '`r format(Sys.Date(), "%B %d, %Y")`'
#' output:
#'    html_document:
#'       number_sections: true
#'       toc: true
#'       toc_float:
#'          collapsed: true
#' ---

#' This document summarizes spatially-derived variables for PNSHP culvert work
#' sites. These variables are collected by linking the worksite points to other
#' spatial data sets via polygon covers or identifying the nearest line.
#' Variables were selected for their value as either potential drivers of
#' project cost, or as potential proxies for ecological benefits.
#'
#' As of right now, we have data from Blake on roads, streams, habitat use,
#' slope, and land cover. I also have collected some county-level data on
#' employment levels in relevant sectors. With some help from Sunny, I also
#' pulled the nearest NHD+ stream COMID, which was used to link a number of
#' additional variables related to stream, land cover, and population features
#' of the stream and upstream basins.
#'
#' In the first section, I summarize data sources and define important
#' variables. In the next section, I evaluate the quality of matches for road
#' and stream data. In the third sections, I present descriptive figures for the
#' new variables. And, finally, I close with some early regression results
#' incorporating the new variables.

#+ warning=F, message=F, echo=F, include=F

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
library(ggcorrplot)
library(mctest)

# Wraps long subtitles in ggplot
wrapper <- function(label, dev_width = dev.size("in")[1], dev_scaler = 12)  {   
  paste(strwrap(label, dev_width * dev_scaler), collapse = "\n") 
}

# Load and prep data ----
sf_culv <- 
  read_csv(here("output/culverts_pure_spatial.csv")) %>% 
  # select(latitude = latitude.x, longitude = longitude.x, everything(), -latitude.y, -longitude.y) %>%
  drop_na(longitude, latitude) %>%
  st_as_sf(
    coords = c('longitude', 'latitude'),
    crs = 4326
  ) %>%
  mutate(
    across(
      c(starts_with("nlcd") & contains("_20"), nlcd_current, nhd_ftype, here_speed, here_class),
      factor
    )
  ) %>%
  mutate(
    across(
      where(is.numeric),
      na_if,
      y = -9999
    )
  )
tibble(names(sf_culv), lapply(sf_culv, typeof) %>% unlist()) %>% print(n=Inf)
summary(sf_culv)

# Load in nhdplus v2.1 stream distances
sf_culv <-
  sf_culv %>%
  left_join(
    read_csv(here("output/spatial/culverts_nhdstreams.csv")) %>%
      select(worksite_id, offset),
      by = "worksite_id"
  ) %>%
  left_join(
    read_csv(here("output/culverts_nhdstreams_metersmatch.csv")) %>%
      select(worksite_id, offset_m = offset),
    by = "worksite_id"
  )

#+
#' # Data sources and variable definitions
#'
#' ## Culvert worksite data  

#' Data on culvert worksite locations and associated project information
#' (reporting source, project cost, year), are from the
#' ***Pacific Northwest Salmon Habitat Projects*** database, which documents
#' projects relevant to salmonid habitat restoration, including culvert
#' improvements. We restrict the sample to projects that:  
#' 
#' 1) Only include culvert actions across any worksites;  
#' 2) Were completed between 2001 and 2015;  
#' 3) Were located only in Washington or Oregon;  
#' 4) Were reported by a source that reports more than 20 total projects;  
#' 5) Were located in basins with over 20 worksites reported;  
#' 6) Reported project costs per culvert between $2,000 and $750,000 (roughly 5th and 95th percentiles of all reported project costs).  
#' 

#+ include=F
sf_culv <-
  sf_culv %>%
  filter(
    state_fips %in% c(41, 53),
    # cost_per_culvert > 2000,
    # cost_per_culvert < 750000,
    project_year > 2000
  ) %>%
  add_count(basin) %>%
  filter(n > 20) %>%
  select(-n) %>%
  add_count(project_source) %>%
  filter(n > 20) %>%
  select(-n) %>%
  # Second check on basin to make sure they still have more than 20
  add_count(basin) %>%
  filter(n > 20) %>%
  select(-n)

# We're ready to plot!

# Plot for visualization ----
#+
#' # Evaluating accuracy of stream and road matches
#'
#' First thing we'll do is check how accurate the culvert worksite points match
#' up with the road and stream data recovered from three sources: (1) [HERE](https://gii.dhs.gov/hifld/sites/default/files/2019-08/HIFLD%20Licensed%20HERE%20One%20Pager.pdf)
#' for network-able road features, (2) [NHDPlus HR](https://hydro.nationalmap.gov/arcgis/rest/services/NHDPlus_HR/MapServer)
#' for stream/river classification and features, (3) [StreamNet](https://www.streamnet.org/data/interactive-maps-and-gis-data/)
#' for salmonid habitat use data by species. We will evaluate the precision of
#' matches using distance to the "matching" line, measured in meters.
#'
#' We suspect two possible sources of imprecision. First, worksite points may
#' be reported with imprecision, in which case the match would represent the
#' most likely feature, but with less certainty than if distance was lower.
#' Second, the worksite may be on a road or stream that is not represented in
#' our source data. In this case, the matched feature would misrepresent the
#' true feature we are trying to identify for the worksite.
#'
#' In either case, we will need to identify and decide how to deal with "poor"
#' matches. For now, we will ***define poor matches as worksites over 150m from
#' the target feature*** (i.e. road or stream), represented in the figures below
#' with a red horizontal line. This threshold is easily adjustable, and
#' sensitivity to this threshold will also be examined.

#+ fig.width=10, fig.height=10, echo=F, message=F
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
    # n_basin > 90
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
  geom_jitter(width = 0.25) +
  geom_violin(aes(group = project_year), alpha = 0.7, scale = "width", width = 0.5) +
  geom_hline(yintercept = 150, color = "red") +
  # geom_hline(yintercept = 50, color = "red", linetype = "dashed") +
  geom_label(aes(label = paste("n =", n)), x = 1997, y = 4000, fill = "white", color = "black", size = 3, data = sf_culv %>% count(basin)) +
  theme(legend.position = "none") +
  # scale_size_continuous(range = c(0.1,1)) +
  scale_y_continuous(label = label_comma(1)) +
  facet_wrap("basin", ncol = 3) +
  labs(
    title = "Distance to nearest HERE road (m), grouped by basin",
    subtitle = "Most (> 70%) culvert points are within 150m of a road object; > 50% within 50m",
    caption = "Points represent individual worksite observations, red line indicates 150m cut-off"
  )
# Number of "good" matches
# sum(I(sf_culv$here_distm <= 50))/nrow(sf_culv); sum(I(sf_culv$here_distm <= 150))/nrow(sf_culv)

# NHDPlus HR streams
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
    # n_basin > 90
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
  geom_jitter(width = 0.25) +
  geom_violin(aes(group = project_year), alpha = 0.7, scale = "width", width = 0.5) +
  geom_hline(yintercept = 150, color = "red") +
  geom_label(aes(label = paste("n =", n)), x = 1997, y = 850, fill = "white", color = "black", size = 3, data = sf_culv %>% count(basin)) +
  # scale_size_continuous(range = c(0.1,1)) +
  scale_y_continuous(label = label_comma(1)) +
  facet_wrap("basin", ncol = 3) +
  theme(legend.position = "none") +
  labs(
    title = "Distance to nearest NHDPlus HR stream (m), grouped by basin",
    subtitle = wrapper("Nearly all (> 95%) culvert points are within 150m of a stream object, with > 85% within 50m; Many of those > 150m are culvert projects grouped in with other road maintenance classified work like drainage improvement so match rate strictly for culvert worksites is even better"),
    caption = "Points represent individual worksite observations, red line indicates 150m cut-off"
  )
# Number of "good" matches
sum(!I(sf_culv$nhd_dist_m <= 150), na.rm = TRUE)

# NHDPlus V2.1 streams
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
    # n_basin > 90
  ) %>%
  filter(
    # pure_culv == TRUE,
    # n > 10
  ) %>%
  drop_na(offset_m) %>%
  ggplot(
    aes(
      x = project_year,
      y = offset_m,
      color = basin,
      fill = basin
    )
  ) +
  geom_jitter(width = 0.25) +
  geom_violin(aes(group = project_year), alpha = 0.7, scale = "width", width = 0.5) +
  geom_hline(yintercept = 150, color = "red") +
  # geom_label(aes(label = paste("n =", n)), x = 1998, y = 4500, fill = "white", color = "black", size = 3, data = sf_culv %>% count(basin)) +
  # scale_size_continuous(range = c(0.1,1)) +
  scale_y_continuous(label = label_comma(1)) +
  facet_wrap("basin", ncol = 3) +
  theme(legend.position = "none") +
  labs(
    title = "Distance to nearest NHDPlus V2.1 stream (m), grouped by basin",
    subtitle = wrapper("Not as good of matches for NHDPlus HR, only ~75% culvert points are within 150 meters of a stream object (omitted as matching has 150m search buffer)"),
    caption = "Points represent individual worksite observations, red line indicates 150m cut-off"
  )
# Number of "good" matches
sum(is.na(sf_culv$offset_m), na.rm = TRUE)/nrow(sf_culv)
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
    # n_basin > 90
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
  geom_jitter(width = 0.25) +
  geom_violin(aes(group = project_year), alpha = 0.7, scale = "width", width = 0.5) +
  geom_hline(yintercept = 150, color = "red") +
  geom_label(aes(label = paste("n =", n)), x = 1998, y = 4500, fill = "white", color = "black", size = 3, data = sf_culv %>% count(basin)) +
  # scale_size_continuous(range = c(0.1,1)) +
  scale_y_continuous(label = label_comma(1)) +
  facet_wrap("basin", ncol = 3) +
  theme(legend.position = "none") +
  labs(
    title = "Distance to nearest StreamNet stream (m), grouped by basin",
    subtitle = wrapper("Not as good of matches for StreamNet, only ~60% culvert points are within 150 meters of a stream object; Probably less important, as we can apply ESU area overlays based on HUC code"),
    caption = "Points represent individual worksite observations, red line indicates 150m cut-off"
  )
# Number of "good" matches
# sum(I(sf_culv$snet_distm <= 150), na.rm = TRUE)/nrow(sf_culv)

#' ## Dealing with poor matches

#' So ***StreamNet***'s less dense streams have the worst matches, while NHDPlus has
#' very close matches. StreamNet is most useful for salmon habitat
#' identification, though HUC code based habitat maps can be used instead (e.g.
#' maps seen in [Crozier et al. (2018)](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0217711)).
#' I think using this approach might be preferred for consistency, given the
#' large number of poor matches in the StreamNet data. For the cost modeling
#' stage, this variables is less important. For StreamNet,  ***wait for next
#' steps.***  
#' 

#' The HERE roads have lots of close matches, but also plenty that are far off
#' the mark. For poor matches, we can consider two approaches. First, we can
#' assume all poor matches are on "small roads" (e.g.. back roads, private
#' access roads, logging roads, etc.) and ***assign to lowest HERE road class, and unpaved***.
#' Alternatively, we can drop these observations, or assign them to their own
#' class.  
#'
sf_culv <-
  sf_culv %>%
  rowwise() %>%
  mutate(
    # Class bad matches
    here_class_0 = here_class,
    here_class = case_when(
      here_distm > 150 ~ "5",
      TRUE ~ as.character(here_class)
    ),
    here_class_badmatch = case_when(
      here_distm > 150 ~ "6",
      TRUE ~ as.character(here_class)
    ),
    here_class_namatch = case_when(
      here_distm > 150 ~ NA_character_,
      TRUE ~ as.character(here_class)
    ),
    # Speed bad matches
    here_speed_0 = here_speed,
    here_speed = case_when(
      here_distm > 150 ~ "8",
      TRUE ~ as.character(here_speed)
    ),
    here_speed_badmatch = case_when(
      here_distm > 150 ~ "9",
      TRUE ~ as.character(here_speed)
    ),
    # Paved bad matches
    here_paved_0 = here_paved,
    here_paved = case_when(
      here_distm > 150 ~ "N",
      TRUE ~ as.character(here_paved)
    )
  )

#' For ***NHDPlus***, because the vast majority of the worksites are close
#' matches, we can ***drop the poor match worksites***, defined as sites more than
#' 150m from the nearest stream line.
#' 
sf_culv <-
  sf_culv %>%
  filter(
    nhd_dist_m < 150
  )

#+
#' # Descriptive figures
#' ## HERE roads: Speed, functional class, and paved  
#'   
#' The HERE data provides variables for *speed class*, *functional class*, *paved*, and
#' *public*. Paved status, speed class, and road type are potential cost
#' drivers, indicating busier, larger roads that require longer fish crossings,
#' more expensive construction on the affected road, and expected flagging costs
#' associated with traffic diversion. Plots for all three are provided below.
#' 
#' 

#+ fig.width=10, fig.height=10, echo=F, message=F
# Examine HERE road data ----
# Table of classification definitions
key_here <- read_xlsx(here("data/Culverts spatial overlays v 20Aug2020.xlsx"), sheet = 3) %>%
  as_tibble() %>%
  mutate(Classification = str_to_sentence(Classification), Description = str_to_sentence(Description)) %>%
  bind_rows(
    tibble(
      "Classification" = "Functional class",
      "Value" = 6,
      "Description" = '"Roads" associated with worksites > 150m from the nearest HERE road for here_class_badmatch.'
    )
  )

key_here %>%
  arrange(Classification) %>%
  kable(caption = "Value key for HERE road variables") %>%
  kable_styling() %>%
  collapse_rows(
    1,
    valign = "top",
    row_group_label_position = "stacked",
    headers_to_remove = 1
  )

# Speed class

sf_culv %>%
  ungroup() %>%
  add_count(basin, name = "n_basin") %>%
  group_by(basin, n_basin, project_year, here_speed) %>% count() %>%
  ggplot(
    aes(
      x = project_year,
      y = n,
      fill = ordered(as.character(here_speed))
    )
  ) +
  geom_col() +
  geom_label(aes(label = paste("n =", comma(n, 1)), y = n/7), x = 2014, fill = "white", color = "black", size = 3, data = sf_culv %>% count(basin)) +
  scale_fill_discrete("HERE Speed Class") +
  facet_wrap("basin", ncol = 2, scales = "free_y") +
  ggtitle(
    "HERE road speed class, grouped by basin",
    wrapper("Many fewer class 5 roads in the more poorly represented basins than in better represened basins; Similar persistance in class 8 (bad matches)")
  ) +
  theme(legend.position = "bottom")

# Road class
sf_culv %>%
  ungroup() %>%
  add_count(basin, name = "n_basin") %>% 
  group_by(basin, n_basin, project_year, here_class_badmatch) %>% count() %>%
  ggplot(
    aes(
      x = project_year,
      y = n,
      fill = ordered(as.character(here_class_badmatch))
    )
  ) +
  geom_col() +
  geom_label(aes(label = paste("n =", comma(n, 1)), y = n/7), x = 2014, fill = "white", color = "black", size = 3, data = sf_culv %>% count(basin)) +
  scale_fill_discrete("HERE Road Class") +
  facet_wrap("basin", ncol = 2, scales = "free_y") +
  ggtitle(
    "HERE road class, grouped by basin",
    wrapper("Almost all roads are class 5 (smallest road), and a good portion are > 150m from a road match (class 6)")
  ) +
  theme(legend.position = "bottom")

# Paved class
# Basins with more than 250 worksites
sf_culv %>%
  ungroup() %>%
  add_count(basin, name = "n_basin") %>%
  group_by(basin, n_basin, project_year, here_paved) %>% count() %>%
  ggplot(
    aes(
      x = project_year,
      y = n,
      fill = here_paved
    )
  ) +
  geom_col() +
  geom_label(aes(label = paste("n =", comma(n, 1)), y = n/7), x = 2014, fill = "white", color = "black", size = 3, data = sf_culv %>% count(basin)) +
  scale_fill_discrete("HERE Paved") +
  facet_wrap("basin", ncol = 2, scales = "free_y") +
  ggtitle(
    "HERE paved classification, grouped by basin",
    wrapper("Most roads with culvert worksites are not paved, but a good number of paved roads as well")
  ) +
  theme(legend.position = "bottom")

#+
#' ## SalmonNet: Salmon habitat use
#'   
#' The StreamNet data provides variables for *species* and *habitat use*. These
#' variables will be valuable for modeling benefits of culvert improvement, but
#' may also lead to stricter design requirements if in the habitat of ESA listed
#' species.  

#+ fig.width=10, fig.height=10, echo=F, message=F
# Examine SalmonNet data ----
# Species
# Basins with more than 250 worksites
sf_culv %>%
  ungroup() %>%
  add_count(basin, name = "n_basin") %>% 
  group_by(basin, n_basin, project_year, snet_spp) %>% count() %>%
  ggplot(
    aes(
      x = project_year,
      y = n,
      fill = snet_spp
    )
  ) +
  geom_col() +
  geom_label(aes(label = paste("n =", comma(n, 1)), y = n/7), x = 2014, fill = "white", color = "black", size = 3, data = sf_culv %>% count(basin)) +
  scale_fill_discrete("StreamNet species") +
  facet_wrap("basin", ncol = 2, scales = "free_y") +
  ggtitle(
    "StreamNet species, grouped by basin",
    wrapper("Steelhead and Coho are the most well represented, with plenty of culverts in Chinook territory as well")
  ) +
  theme(legend.position = "bottom")


# Habitat use
# Basins with more than 250 worksites
sf_culv %>%
  ungroup() %>%
  add_count(basin, name = "n_basin") %>%
  group_by(basin, n_basin, project_year, snet_use) %>% count() %>%
  ggplot(
    aes(
      x = project_year,
      y = n,
      fill = snet_use
    )
  ) +
  geom_col() +
  geom_label(aes(label = paste("n =", comma(n, 1)), y = n/7), x = 2014, fill = "white", color = "black", size = 3, data = sf_culv %>% count(basin)) +
  scale_fill_discrete("StreamNet habitat use") +
  facet_wrap("basin", ncol = 2, scales = "free_y") +
  ggtitle(
    "StreamNet habitat use, grouped by basin",
    wrapper("Overwhelmingly spawning and rearing streams, except in WA where there is more work on migration only")
  ) +
  theme(legend.position = "bottom")

#+
#' Key takeaway is that the culverts projects in Washington and Oregon are on
#' distinctly different streams. In Washington, reported projects are more
#' frequently on migration streams. These streams are likely to open up more
#' upstream territory, but may also be wider and require more complex culvert
#' designs. In Oregon, the reported projects are overwhelmingly on spawning and
#' rearing streams, which (I believe) tend to be smaller and therefore would
#' require less complex designs.  
#' 
#' We can dig into attributes of associated NHDPlus streams and their associated
#' catchments to further dig into this finding on the stream side.  

#+
#' ## NHDPlus attributes: Slope, bankfull width, and more  
#'
#' Stream characteristics are likely critical cost drivers. All state culvert
#' design guidelines point to specific design needs for stream crossings when
#' the crossing is either particularly wide (as measured by ***bankfull
#' width***) or at a steep slope (as measured by ***slope***).  
#'
#' We gather these variables from [NHDPlus Selected Attributes Version 2.1
#' (Wiezorek, Jackson, & Schwarz,
#' 2018)](https://www.sciencebase.gov/catalog/item/5669a79ee4b08895842a1d47). by
#' identifying the nearest NHDPlus stream object's COMID and matching that COMID
#' with the appropriate attributes. From the same database, we also gather
#' ***population density***, ***housing density***, ***annual precipitation***,
#' ***road density***, ***stream density***, ***stream length***, ***basin
#' area***, and ***NLCD land cover class share*** in the stream's catchment.  
#'
#' These variables area also available aggregated over upstream networked
#' NHDPlus streams and catchments, allowing for measures such as ***total
#' upstream length*** and habitat features for associated catchments (e.g. road
#' density, land cover, precipitation). These aggregated variables are
#' calculated via two methods. Using the *total* method, all area in upstream
#' catchments is weighted equally. Using the *accumulated* methods, area in
#' upstream catchments is weighted by the proportion of flow that reaches the
#' target catchment, accounting for natural and anthropogenic diversions.  
#'
#' For cost modeling, we focus on the ***bankfull width*** and ***slope*** variables. In the
#' next steps, when benefit proxies are constructed for each culvert worksite,
#' we can use the remaining variables to characterize upstream habitat with
#' restored access.  

#+ fig.width=10, fig.height=10, echo=F, message=F, warning=F
# Examine NHDPlus attribute data ----
# Slope
sf_culv %>%
  ungroup() %>%
  add_count(basin, name = "n_basin") %>%
  ggplot(
    aes(
      x = project_year,
      y = slope,
      fill = basin,
      color = basin
    )
  ) +
  geom_jitter(width = 0.25) +
  geom_violin(aes(group = project_year), alpha = 0.7, scale = "width", width = 0.5) +
  geom_label(aes(label = paste("n =", comma(n, 1)), y = 0.25), x = 2014, fill = "white", color = "black", size = 3, data = sf_culv %>% count(basin)) +
  stat_summary(
    fun = mean,
    shape = "cross",
    stroke = 1.5,
    geom = "point",
    color = "black"
  ) +
  scale_fill_discrete(guide = NULL) +
  scale_color_discrete(guide = NULL) +
  facet_wrap("basin", ncol = 2) +
  labs(
    title = "NHDPlus stream slope (pct), grouped by basin",
    subtitle = wrapper("Similar distribtuions of slope across basins and years"),
    caption = "Bold X indicates basin-year mean, points represent individual worksite observations"
  )
  

# Bankfull width

sf_culv %>%
  ungroup() %>%
  add_count(basin, name = "n_basin") %>%
  ggplot(
    aes(
      x = project_year,
      y = bankfull_width,
      fill = basin,
      color = basin
    )
  ) +
  geom_jitter(width = 0.25) +
  geom_violin(aes(group = project_year), alpha = 0.7, scale = "width", width = 0.5) +
  geom_hline(yintercept = 50, color = "red") +
  geom_label(aes(label = paste("n =", comma(n, 1)), y = 50), x = 2014, fill = "white", color = "black", size = 3, data = sf_culv %>% count(basin)) +
  stat_summary(
    fun = mean,
    shape = "cross",
    stroke = 1.5,
    geom = "point",
    color = "black"
  ) +
  scale_fill_discrete(guide = NULL) +
  scale_color_discrete(guide = NULL) +
  scale_y_log10(label = label_comma(1)) +
  facet_wrap("basin", ncol = 2) +
  labs(
    title = "NHDPlus stream bankfull width (m, log-scale), grouped by basin",
    subtitle = wrapper("Similar distribtuions of slope across basins and years; Some extreme outliers (>50m) that likely constitute poor matches or poorly categorized project"),
    caption = "Bold X indicates basin-year mean, points represent individual worksite observations, red line indicates 50m cut-off"
  )


#+
#' So it looks like the slope variable is pretty well behaved. We will ***drop
#' worksites with bankfull width over 50m*** to remove extreme outliers.  
#' 

sf_culv <-
  sf_culv %>%
  filter(
    bankfull_width < 50
  )

#' Otherwise, these variables show good behavior across these two physical
#' attributes. Culverts in Washington are associated with larger streams, which
#' will likely mean more expensive projects.  

#+ fig.width=10, fig.height=10, echo=F, message=F, warning=F
# Examine population and housing density ----
# Pdens
sf_culv %>%
  ungroup() %>%
  add_count(basin, name = "n_basin") %>%
  # filter(popdens_cat < 100) %>%
  ggplot(
    aes(
      x = project_year,
      y = popdens_cat,
      fill = basin,
      color = basin
    )
  ) +
  geom_jitter(width = 0.25) +
  geom_violin(aes(group = project_year), alpha = 0.7, scale = "width", width = 0.5) +
  geom_label(aes(label = paste("n =", comma(n, accuracy = 1))), x = 1996, y = 2.5, fill = "white", color = "black", size = 3, data = sf_culv %>% count(basin)) +
  stat_summary(
    fun = mean,
    shape = "cross",
    stroke = 1.5,
    geom = "point",
    color = "black"
  ) +
  scale_fill_discrete(guide = NULL) +
  scale_color_discrete(guide = NULL) +
  # scale_y_log10(label = label_comma(1)) +
  facet_wrap("basin", ncol = 2) +
  labs(
    title = "NHDPlus population density (persons per km sq.), grouped by basin",
    subtitle = wrapper("Similar distribtuions of slope across basins and years; Lots of streams with catchments w/ zero persons"),
    caption = "Bold X indicates basin-year mean, points represent individual worksite observations"
  )


# Hdens
# Basins with more than 250 worksites
sf_culv %>%
  ungroup() %>%
  add_count(basin, name = "n_basin") %>%
  # filter(popdens_cat < 100) %>%
  ggplot(
    aes(
      x = project_year,
      y = hdens_cat,
      fill = basin,
      color = basin
    )
  ) +
  geom_jitter(width = 0.25) +
  geom_violin(aes(group = project_year), alpha = 0.7, scale = "width", width = 0.5) +
  geom_label(aes(label = paste("n =", comma(n, accuracy = 1))), x = 1996, y = 2.5, fill = "white", color = "black", size = 3, data = sf_culv %>% count(basin)) +
  stat_summary(
    fun = mean,
    shape = "cross",
    stroke = 1.5,
    geom = "point",
    color = "black"
  ) +
  scale_fill_discrete(guide = NULL) +
  scale_color_discrete(guide = NULL) +
  # scale_y_log10(label = label_comma(1)) +
  facet_wrap("basin", ncol = 2) +
  labs(
    title = "NHDPlus housing density (units per km sq.), grouped by basin",
    subtitle = wrapper("Similar distribtuions of slope across basins and years; Lots of streams with catchments w/ zero units"),
    caption = "Bold X indicates basin-year mean, points represent individual worksite observations"
  )


#'  
#'  Population and housing density are highly correlated (Pearson's coef. = 
#'  `r cor(sf_culv$popdens_cat, sf_culv$hdens_cat, use = "complete.obs") %>% format(digits = 3)`), so we probably 
#' have to pick one or the other. Design documents point to site access as a
#' crucial driver in project cost, and note that negotiating with private
#' landowners can complicate projects. Housing density is a good proxy for this
#' driver. Population density can be a good proxy for road traffic, which can
#' increase flagging costs. Both effects are likely to increase costs. While we
#' won't be able to distinguish a mechanism, including either can control for
#' density effects.  
#'
#' We should also consider alternative measures here that might better capture
#' the hypothetical mechanisms at play. For example, variables such as the
#' number of distinct parcels in the catchment, whether the worksite point is
#' on public land, or the proportion of the catchment that is in public land
#' might better capture increased costs due to access issues.  
#' 


#+
#' ## Slope at worksite  
#'   
#' The slope at the worksite may increase project cost by restricting access to
#' the culvert. We measure slope using the slope in degrees of the
#' [GTOPO30](https://www.usgs.gov/centers/eros/science/usgs-eros-archive-digital-elevation-global-30-arc-second-elevation-gtopo30?qt-science_center_objects=0#qt-science_center_objects)
#' grid cell the worksite falls in.  
#' 
#' We can also measure slope using NHD+ attributes, which reports the slope of the catchment, distinct from the channel slope, for each stream segment.  
#' 
#' Note that slope at worksites and the stream slope are distinct, both in
#' their measurement and the mechanism through which they influence costs.
#' Stream slope is our best available proxy for the slope of the stream at the
#' culvert, which can require more complex, expensive culvert design to ensure
#' fish passage. Slope at worksite is the *land* slope in the immediate
#' vicinity of the culvert, which can restrict site access and available staging
#' area, increasing project costs. While closely related, these are distinct
#' (Pearson's coef. = `r cor(sf_culv$slope, sf_culv$slope_deg, use = "complete.obs") %>% format(digits = 3)`).
#' 

#+ fig.width=10, fig.height=10, echo=F, message=F, warning=F
# Examine slope ----
# Slope
sf_culv %>%
  group_by(basin, project_year) %>%
  add_count() %>%
  ungroup() %>%
  add_count(basin, name = "n_basin") %>%
  filter(
    # pure_culv == TRUE,
    # n_basin > 90
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
  geom_jitter(width = 0.25) +
  geom_violin(aes(group = project_year), alpha = 0.7, width = 0.5, na.rm = TRUE, scale = "width") +
  geom_label(aes(label = paste("n =", n)), x = 1997, y = 25, fill = "white", color = "black", size = 3, data = sf_culv %>% count(basin)) +
  stat_summary(
    fun = mean,
    shape = "cross",
    stroke = 1.5,
    geom = "point",
    color = "black"
  ) +
  theme(legend.position = "none") +
  scale_size_continuous(range = c(0.1,1)) +
  facet_wrap("basin", ncol = 3) +
  labs(
    title = "Slope at worksite (GTOPO30), grouped by basin",
    subtitle = wrapper("Plenty of variation over time and space, even within basins"),
    caption = "Bold X indicates basin-year mean, points represent individual worksite observations"
  )

sf_culv %>%
  group_by(basin, project_year) %>%
  add_count() %>%
  ungroup() %>%
  add_count(basin, name = "n_basin") %>%
  filter(
    # pure_culv == TRUE,
    # n_basin > 90
  ) %>%
  drop_na(cat_basin_slope) %>%
  ggplot(
    aes(
      x = project_year,
      y = cat_basin_slope,
      color = basin,
      fill = basin
    )
  ) +
  geom_jitter(width = 0.25) +
  geom_violin(aes(group = project_year), alpha = 0.7, width = 0.5, na.rm = TRUE, scale = "width") +
  geom_label(aes(label = paste("n =", n)), x = 1997, y = 25, fill = "white", color = "black", size = 3, data = sf_culv %>% count(basin)) +
  stat_summary(
    fun = mean,
    shape = "cross",
    stroke = 1.5,
    geom = "point",
    color = "black"
  ) +
  theme(legend.position = "none") +
  scale_size_continuous(range = c(0.1,1)) +
  facet_wrap("basin", ncol = 3) +
  labs(
    title = "Slope at worksite (NHD+), grouped by basin",
    subtitle = wrapper("Plenty of variation over time and space, even within basins"),
    caption = "Bold X indicates basin-year mean, points represent individual worksite observations"
  )

#+
#' ## Land cover at worksites  
#'   
#' Land cover can proxy for land use, which can effect how difficult it is to
#' access and perform construction activities at the worksite, which may
#' increase costs. It may also be correlated with road features (surface
#' material, typical traffic, etc.) or stream features, and could be useful for
#' providing more information for worksites where matches to roads and streams
#' are poor.  
#'  
#' The table below presents the NLCD land cover classifications. We use the "class" level of detail in the figures that follow.
 
#+ fig.width=10, fig.height=10, echo=F, message=F, warning=F
# Examine land cover data ----
(
  key_nlcd <-
    read_xlsx(
      here(
        "/data/Culverts spatial overlays v 20Jan2021.xlsx"
      ), 
      sheet = 3
    ) %>% 
    as_tibble() %>%
    clean_names("sentence") %>% 
    mutate(across(where(is_character), str_to_sentence)) %>%
    filter(across(Description, ~!str_detect(., "Alaska only")))
) %>%
  # print(n = 20)
  knitr::kable() %>%
  kableExtra::kable_styling()

# Basins with more than 250 worksites
sf_culv %>%
  mutate(nlcd_current = as.numeric(as.character(nlcd_current))) %>%
  left_join(
    key_nlcd,
    by = c("nlcd_current" = "Value")
  ) %>%
  rename(
    nlcd_class = Class,
    nlcd_classfull = Classification,
    nlcd_description = Description
  ) %>%
  ungroup() %>%
  add_count(basin, name = "n_basin") %>% 
  group_by(basin, n_basin, project_year, nlcd_class) %>% count() %>%
  ggplot(
    aes(
      x = project_year,
      y = n,
      fill = nlcd_class
    )
  ) +
  geom_col() +
  geom_label(aes(label = paste("n =", comma(n, 1)), y = n/7), x = 2014, fill = "white", color = "black", size = 3, data = sf_culv %>% count(basin)) +
  scale_fill_discrete("NLCD Class") +
  facet_wrap("basin", ncol = 2, scales = "free_y") +
  ggtitle(
    "NLCD land cover class, grouped by basin",
    wrapper("Forest dominates, with Developed also well represented; Planted-Cultivated, Shrubland, and Wetlands also have some representation")
  ) +
  theme(legend.position = "bottom")


#+
#' ## Job shares in relevant sectors  
#'   
#' To capture labor market patterns that might affect culvert costs, we
#' collect the number of jobs for construction and ag/forestry
#' NAICS codes for the county the worksite is located in via [County Business
#' Patterns](https://www.census.gov/programs-surveys/cbp.html). Access to
#' experienced labor can keep costs down. These variables are intended to
#' capture that effect.  
#'
#' Because counties are so large in Oregon and Washington, this is not the ideal
#' measure of job patterns. For example, culverts in rural King County will have
#' jobs numbers driven largely by patterns in Seattle, despite being as much as
#' 40 miles away. Other ideas on how to capture these effects would be
#' appreciated!  

#+ fig.width=10, fig.height=10, echo=F, message=F, warning=F
# Examine employment levels ----
# Employment in ag/forestry firms
sf_culv %>%
  group_by(basin, project_year, fips) %>%
  add_count() %>%
  ungroup() %>%
  add_count(basin, name = "n_basin") %>%
  filter(
    # pure_culv == TRUE,
    # n_basin > 90
  ) %>%
  drop_na(emp_agforest) %>%
  ggplot(
    aes(
      x = project_year,
      y = emp_agforest,
      group = fips,
      color = basin,
      # fill = basin
    )
  ) +
  geom_line(
    # aes(size = n)
  ) +
  geom_point(aes(size = n)) +
  # geom_label(aes(label = paste("n =", n)), x = 2014, y = 2250, fill = "white", color = "black", size = 3, data = sf_culv %>% count(basin) %>% filter(n > 30)) +
  theme(legend.position = "none") +
  scale_size_continuous(range = c(1,5)) +
  scale_y_continuous(label = label_comma(1)) +
  scale_color_discrete(guide = NULL) +
  facet_wrap("basin", scales = "free_y", ncol = 2) +
  labs(
    title = "County-level employment at ag or forestry firms, grouped by basin",
    subtitle = wrapper("Plenty of variation over time and space, even within basins"),
    caption = "Points represent county-year observations represented in sample, size is relative to number of worksites in county-year"
  )

# Employment in construction firms
sf_culv %>%
  group_by(basin, project_year, fips) %>%
  add_count() %>%
  ungroup() %>%
  add_count(basin, name = "n_basin") %>%
  filter(
    # pure_culv == TRUE,
    # n_basin > 90
  ) %>%
  drop_na(emp_const) %>%
  ggplot(
    aes(
      x = project_year,
      y = emp_const,
      color = basin,
      # fill = basin
    )
  ) +
  geom_line(
    aes(
      group = fips
    )
  ) +
  geom_point(aes(size = n, group = fips)) +
  # geom_label(aes(label = paste("n =", n)), x = 2014, y = 2250, fill = "white", color = "black", size = 3, data = sf_culv %>% count(basin) %>% filter(n > 30)) +
  # stat_summary(
  #   fun = mean,
  #   shape = "cross",
  #   stroke = 1.5,
  #   geom = "point",
  #   color = "black"
  # ) +
  theme(legend.position = "none") +
  scale_size_continuous(range = c(1,5)) +
  scale_y_continuous(label = label_comma(1)) +
  scale_color_discrete(guide = NULL) +
  facet_wrap("basin", scales = "free_y", ncol = 2) +
  labs(
    title = "County-level employment at construction firms, grouped by basin",
    subtitle = wrapper("Plenty of variation over time and space, even within basins, but levels much higher in Puget Sound (obviously); Levels track more closely to larger economic conditions than ag/forestry jobs"),
    caption = "Points represent county-year observations represented in sample, size is relative to number of worksites in county-year"
  )


#' ## US Census: Distance to population center
#' 

#' We proxy for availability of supplies and labor by measuring the distance
#' from each worksite to the boundary of census designated "Urban Areas" and
#' "Urban Clusters" as of the 2010 census
#' ([source](https://www.census.gov/programs-surveys/geography/guidance/geo-areas/urban-rural/2010-urban-rural.html)).
#' Urban areas are defined as contiguous areas of 50,000 or more people, while
#' urban clusters are defined as contiguous areas of 2,500 to 50,000 people.
#' (Worksites within urban areas/clusters are assigned 0.)  
#' 

#+ fig.width=10, fig.height=16, echo=F, message=F, warning=F
# Examine distance to urban area ----
sf_culv %>%
  ungroup() %>%
  add_count(basin, name = "n_basin") %>%
  pivot_longer(
    c(ends_with("_dist") & !starts_with("upst_")),
    names_to = "urban_class",
    values_to = "urban_dist"
  ) %>%
  ggplot(
    aes(
      x = project_year,
      y = urban_dist/1000,
      group = urban_class,
      fill = basin,
      color = basin
    )
  ) +
  geom_jitter(width = 0.25) +
  geom_violin(aes(group = project_year), alpha = 0.7, scale = "width", width = 0.5) +
  # geom_label(aes(label = paste("n =", comma(n, accuracy = 1))), x = 1996, y = 0.25, fill = "white", color = "black", size = 3, data = sf_culv %>% count(basin) %>% filter(n > 400)) +
  stat_summary(
    fun = mean,
    shape = "cross",
    stroke = 1.5,
    geom = "point",
    color = "black"
  ) +
  # scale_fill_discrete(guide = NULL) +
  # scale_color_discrete(guide = NULL) +
  scale_y_continuous(labels = comma) +
  facet_grid(basin ~ urban_class, scales = "free_y", labeller = label_wrap_gen(10)) +
  labs(
    title = "Distance (km) to urban area (ua)/cluster (uc), grouped by basin",
    subtitle = wrapper("Lots of within basin variability"),
    caption = "Bold X indicates basin-year mean, points represent individual worksite observations"
  ) +
  theme(legend.position = "none", strip.text.y = element_text(size = 8))


#+
#' ## HIFLD: Material supply operation density  
#' 

#' We proxy for availability of raw materials and machinery by generating a
#' kernel density field for operations identified in the following NAICS
#' categories:  
#' - Merchant Wholesalers Durable Goods: Brick, Stone and Related (brick_coun/brick_totp)  
#' - Merchant Wholesalers Durable Goods: Construction and Mining  (const_coun/counst_top)  
#' - Merchant Wholesalers Durable Goods: Metals Service Centers (metal_coun/metal_top)  
#' - Merchant Wholesalers Durable Goods: All above sub-categores (merch_coun/merch_top)  
#' - Sand & Gravel Operations: Sales Yard (sales_coun)  
#' - Sand & Gravel Operations: All sub-categories, incl. Sales Yard (sand_count)  

#' For the "Merchant Wholesalers Durable Goods" categories we include densities
#' based on both simple site count (*_coun) and weighted by persons/site to scale for
#' operation size (*_totp). For Sand & Graveel Operations, only simple site count
#' densities are estimated as persons/site data is unavailable.
#' 

#+ fig.width=10, fig.height=16, echo=F, message=F, warning=F
# Examine supplier densities ----
sf_culv %>%
  ungroup() %>%
  add_count(basin, name = "n_basin") %>%
  pivot_longer(
    c(ends_with("_coun"), ends_with("_totp"), ends_with("_count")),
    names_to = "supplier_class",
    values_to = "supplier_density"
  ) %>%
  ggplot(
    aes(
      x = project_year,
      y = supplier_density,
      # shape = supplier_class,
      group = supplier_class,
      fill = supplier_class,
      color = supplier_class
    )
  ) +
  geom_jitter(width = 0.25) +
  geom_violin(aes(group = project_year), alpha = 0.7, scale = "width", width = 0.5) +
  # geom_label(aes(label = paste("n =", comma(n, accuracy = 1))), x = 1996, y = 0.25, fill = "white", color = "black", size = 3, data = sf_culv %>% count(basin) %>% filter(n > 400)) +
  stat_summary(
    fun = mean,
    shape = "cross",
    stroke = 1.5,
    geom = "point",
    color = "black"
  ) +
  # scale_fill_discrete(guide = NULL) +
  # scale_color_discrete(guide = NULL) +
  facet_grid(supplier_class ~ basin, scales = "free_y", labeller = label_wrap_gen(10)) +
  labs(
    title = "Supplier densities, grouped by basin",
    subtitle = wrapper("Differences across years and basins but different variables track each other closely"),
    caption = "Bold X indicates basin-year mean, points represent individual worksite observations"
  ) +
  theme(legend.position = "none")

#+ fig.width=10, fig.height=10, echo=F, message=F, warning=F
# Correlations across variables
df_corr <-
  sf_culv %>%
  # st_drop_geometry() %>%
  select(
    brick_coun:sand_count
  )

mat_corr <-
  df_corr %>%
  cor() %>%
  as.matrix()

mat_corr[upper.tri(mat_corr)] <- NA

ggcorrplot(
  mat_corr,
  lab = TRUE, lab_size = 3,
  colors = c("#6D9EC1", "white", "#E46726")
) + 
  theme(
    plot.title.position = "plot",
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "#ADADAD", linetype = "dashed"),
    axis.text.x = element_text(angle = 60, vjust = 1),
    # plot.margin = unit(c(0.5, 0, 1.5, 0), "cm")
  ) +
  scale_fill_gradient2(
    name = "Corr. coef. (r)",
    limits = c(-1, 1),
    low = "#F8766D",
    mid = "#FFFFFF",
    high = "#619CFF",
    na.value = "#EAEAEA"
  ) +
  ggtitle(
    "Correlations among potential supplier density covariates",
    str_wrap("Strong correlations across most groups, but some meaningful differences, especially with the sand/gravel sales sites and the brick/stone/concrete suppliers")
  ) +
  coord_fixed(
    # ylim = c(0, 20),
    # xlim = c(0, 20),
    expand = FALSE,
    clip = "off"
  )

#+ echo=FALSE, message=FALSE, warning=FALSE
#' ## BLM: Surface jurisdiction of land  
#' 

#' We are also interested in public land and private land nearby worksites as a
#' proxy for land access. We identify the proportion of land ownership by group
#' within 500m, 1km, and 2.5km radius circular buffer of each worksite using BLM
#' surface jurisdiction records from 2019. (*Blake could you provide more detailed
#' sourcing on these data?*). We distinguish between the following ownership entities:  
#'

key_blm <- read_xlsx(here("data/Culverts spatial overlays v 20Jan2021.xlsx"), sheet = 5) %>%
  as_tibble() %>%
  filter(Present == 1) %>%
  mutate(Present = NULL)

key_blm %>%
  arrange(Code) %>%
  kable(caption = "Value key for BLM land ownership variables") %>%
  kable_styling() %>%
  collapse_rows(
    1,
    valign = "top",
    row_group_label_position = "stacked",
    headers_to_remove = 1
  )

#' Lands managed by private industry in this context refer mainly to forestry
#' activities. High values indicate that the culvert in question is likely owned
#' by a timber company replacing the barrier under state forestland policies.  

#+ fig.width=10, fig.height=16, echo=F, message=F
# Examine land ownership distributions ----
sf_culv %>%
  ungroup() %>%
  add_count(basin, name = "n_basin") %>% 
  pivot_longer(
    c(ends_with("_5km_buff")),
    names_to = "ownership_code",
    # names_pattern = ".*_5km_buff$",
    values_to = "ownership_prop"
  ) %>%
  mutate(ownership_code = ownership_code %>% str_remove("_5km_buff") %>% str_to_upper()) %>%
  left_join(key_blm, by = c("ownership_code" = "Code")) %>%
  ggplot(
    aes(
      x = project_year,
      y = ownership_prop,
      # shape = supplier_class,
      group = Description,
      fill = Description,
      color = Description
    )
  ) +
  geom_jitter(width = 0.25) +
  geom_violin(aes(group = project_year), alpha = 0.7, scale = "width", width = 0.5) +
  # geom_label(aes(label = paste("n =", comma(n, accuracy = 1))), x = 1996, y = 0.25, fill = "white", color = "black", size = 3, data = sf_culv %>% count(basin) %>% filter(n > 400)) +
  stat_summary(
    fun = mean,
    shape = "cross",
    stroke = 1.5,
    geom = "point",
    color = "black"
  ) +
  # scale_fill_discrete(guide = NULL) +
  # scale_color_discrete(guide = NULL) +
  facet_grid(ownership_code ~ basin, labeller = label_wrap_gen(10)) +
  labs(
    title = "Ownership group proportion at 5km buffer, grouped by basin",
    subtitle = wrapper("Due to many categories having little variation and low representation, we will group some"),
    caption = "Bold X indicates basin-year mean, points represent individual worksite observations"
  ) +
  theme(legend.position = "none")


#' We also provide summaries for composite variables that group private land,
#' state-managed land, and federally-managed land (other than BLM and USFS,
#' which manage significant amounts on their onw) by summing across relevant
#' variables.

#+ fig.width=10, fig.height=16, echo=F, message=F
sf_culv <-
  sf_culv %>%
  rowwise() %>%
  mutate(
    pvall_5km_buff = sum(c_across(starts_with("pv") & ends_with("5km_buff"))),
    stall_5km_buff = sum(c_across(starts_with("st") & ends_with("5km_buff"))),
    fedother_5km_buff = sum(c_across(starts_with(c("bpa", "coe", "fws", "bia", "gsa", "nps")) & ends_with("5km_buff"))),
    pvall_2km_buff = sum(c_across(starts_with("pv") & ends_with("2km_buff"))),
    stall_2km_buff = sum(c_across(starts_with("st") & ends_with("2km_buff"))),
    fedother_2km_buff = sum(c_across(starts_with(c("bpa", "coe", "fws", "bia", "gsa", "nps")) & ends_with("2km_buff"))),
    pvall_1km_buff = sum(c_across(starts_with("pv") & ends_with("1km_buff"))),
    stall_1km_buff = sum(c_across(starts_with("st") & ends_with("1km_buff"))),
    fedother_1km_buff = sum(c_across(starts_with(c("bpa", "coe", "fws", "gsa", "bia", "nps")) & ends_with("1km_buff")))
  )

key_blm <-
  key_blm %>%
  bind_rows(
    tibble(
      "Code" = c("PVALL", "STALL", "FEDOTHER"),
      "Description" = c("All Private Land", "All State Agencies", "All Other Federal Agencies")
    )
  )

sf_culv %>%
  ungroup() %>%
  add_count(basin, name = "n_basin") %>% 
  # filter(n_basin > 150) %>%
  pivot_longer(
    paste0(c("pvall", "stall", "fedother", "lg", "blm", "usfs"), "_5km_buff"),
    names_to = "ownership_code",
    # names_pattern = ".*_5km_buff$",
    values_to = "ownership_prop"
  ) %>%
  mutate(ownership_code = ownership_code %>% str_remove("_5km_buff") %>% str_to_upper()) %>%
  left_join(key_blm, by = c("ownership_code" = "Code")) %>%
  ggplot(
    aes(
      x = project_year,
      y = ownership_prop,
      # shape = supplier_class,
      group = Description,
      fill = Description,
      color = Description
    )
  ) +
  geom_jitter(width = 0.25) +
  geom_violin(aes(group = project_year), alpha = 0.7, scale = "width", width = 0.5) +
  # geom_label(aes(label = paste("n =", comma(n, accuracy = 1))), x = 1996, y = 0.25, fill = "white", color = "black", size = 3, data = sf_culv %>% count(basin) %>% filter(n > 400)) +
  stat_summary(
    fun = mean,
    shape = "cross",
    stroke = 1.5,
    geom = "point",
    color = "black"
  ) +
  # scale_fill_discrete(guide = NULL) +
  # scale_color_discrete(guide = NULL) +
  facet_grid(ownership_code ~ basin, labeller = label_wrap_gen(10)) +
  labs(
    title = "Ownership group proportion at 5km buffer, grouped by basin",
    subtitle = wrapper("Good spread for most of these groups across basins and years; Can dig deeper with 2km and 1km but 5km will do for now"),
    caption = "Bold X indicates basin-year mean, points represent individual worksite observations"
  ) +
  theme(legend.position = "none")



#+
#' ## PNSHP: Culvert project costs  
#'   
#' Culvert project costs are the primary dependent variable in this study. We
#' calculate cost per culvert at the project level by first tallying the number
#' of culverts installed, upgraded, or removed at the project level. We then
#' divide project-level costs by the number of culverts. This cost per culvert
#' measure is assigned to each worksite associated with that project.

#+ fig.width=10, fig.height=10, echo=F, message=F
# Examine cost distributions ----
sf_culv %>%
  group_by(basin, project_year) %>%
  add_count() %>%
  ungroup() %>%
  add_count(basin, name = "n_basin") %>%
  filter(
    # pure_culv == TRUE,
    # n_basin > 90
  ) %>%
  drop_na(cost_per_culvert) %>%
  ggplot(
    aes(
      x = project_year,
      y = cost_per_culvert,
      color = basin,
      fill = basin
    )
  ) +
  geom_jitter(width = 0.25) +
  geom_violin(aes(group = project_year), alpha = 0.7, scale = "width", width = 0.5) +
  geom_label(aes(label = paste("n =", n)), x = 1997, y = 5.5, fill = "white", color = "black", size = 3, data = sf_culv %>% count(basin)) +
  stat_summary(
    fun = mean,
    shape = "cross",
    stroke = 1.5,
    geom = "point",
    color = "black"
  ) +
  theme(legend.position = "none") +
  scale_size_continuous(range = c(0.1,1)) +
  scale_y_log10(labels = label_dollar()) +
  facet_wrap("basin", ncol = 2) +
  labs(
    title = "Cost per culvert ($USD2019, log-scale), grouped by basin",
    subtitle = wrapper("Plenty of variation over time and space, even within basins; Washington averages consistently above Oregon averages"),
    caption = "Bold X indicates basin-year mean, points represent individual worksite observations"
  )


# Other notes ----
#+
#' # Key takeaways  
#'   
#' There are noticeable differences between Washington and Oregon culvert
#' projects represented in the data. Washington projects tend to more frequently
#' be on migration routes and on paved roads, and on wider streams. Oregon projects are more likely to
#' be on spawning and rearing streams, unpaved roads, and on narrower streams. This likely goes a
#' long way in explaining the large differences in costs across the two states, but can be accounted for 
#' with thoughtful inclusion of covariates in the conditioning set.  
#' 
#' The PNSHP culverts data also over-represent smaller, unpaved roads across the board. This limits the usefulness of the data 
#' for projecting costs for passage improvements on larger roads (i.e., with speed limits >40 mph). These limitations are likely to 
#' be reflected by larger standard errors on any estimates of marginal effects, but should be noted with any presentation of results.  
#' 
#' Finally, a version of the proceeding analysis of the potential explanatory variables will be conducted for the fish passage barrier inventories 
#' maintained by Oregon and Washington Departments of Fish and Wildlife. This exercise will reveal how representative the PNSHP projects are relative to 
#' potential future projects.
#'  





# Render output
# rmarkdown::render(here::here("R/C.culvertsspatial/06.spatialexplore.R"), output_file = here::here("output/culvertsspatial_report_2020sep24.html"))

#+ include=F
# Save out final data ----
write_csv(sf_culv, here("output", "culverts_pure_modelling.csv"))

