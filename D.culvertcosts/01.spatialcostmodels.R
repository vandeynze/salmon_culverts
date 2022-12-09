#' ---
#' title: "Culvert Cost Models with Spatially Explicit Data"
#' author: "B. Van Deynze"
#' date: '`r format(Sys.Date(), "%B %d, %Y")`'
#' output:
#'    html_document:
#'       number_sections: true
#'       toc: true
#'       toc_float:
#'          collapsed: true
#' ---
#' 
#+ include=F
# Prepare environment and data ----
rm(list = ls())

library(ggmap)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(osmdata)
library(raster)
library(searchable)
library(ggthemes)
library(equatiomatic)
library(MASS)
library(tidyverse)
library(janitor)
library(here)
library(scales)
library(knitr)
library(kableExtra)
library(broom)
library(margins)
library(ggeffects)
library(forcats)
library(readxl)
library(mctest)
library(ggcorrplot)
library(gganimate)
library(ggtext)
library(gifski)
library(sandwich)
library(lmtest)
library(nhdplusTools)

opts_chunk$set(echo=FALSE)

# Wraps long subtitles in ggplot
wrapper <- function(label, dev_width = dev.size("in")[1], dev_scaler = 12)  {   
  paste(strwrap(label, dev_width * dev_scaler), collapse = "\n") 
}

# Introduction and theory ----
#+

#' # Incorporating costs in conservation planning  

#' **Niche:** use of cost data in conservation plans  
#' 
#' - Just as benefits, variability in costs can be large  
#'   - Including spatial variability and variability across scope and scale of project  
#'   - Understanding this variability can improve planning outcomes  
#' 
#' ***Key cites***  
#' **Babcock et al. 1997:** https://doi.org/10.2307/3147171  
#'   
#' - Describes relative efficiency of management rules under different joint distributions of costs and benefits  
#' - Alternative targeting instruments considered incl. cost-targeting, benefit-targeting, and ratio-targeting (cost per benefit targeting)  
#' - Relative variability of benefits and costs, and correlation between the two, determine effects of sub-optimal targeting  
#'   

#+ fig.width=10, fig.height=10, echo=F, message=F, warning=F, animation.hook="gifski"
set.seed(123456)
sigma = matrix( 
  c(2, 0, 0, 1), # the data elements 
  nrow=2,              # number of rows 
  ncol=2,              # number of columns 
  byrow = TRUE
)        # fill matrix by rows
xy <- mvrnorm(100, c(0, 0), sigma)
# cov(xy)
# cor(xy)
xy <- data.frame(xy)
xy <- xy %>% mutate(c_target = I(X1<0), b_target = I(X2>0), r_target = I(X1<0 & X2>0 | ((X1/X2)>1 & X2<0) | ((X1/X2)<1 & X2>0)))
  
ggplot() +
  geom_text(
    aes(
      x = c(-3, 1, -3, 1),
      y = c(3, 3, -2.95, -2.95)
    ),
    label =
      c(
        "I. High-benefit, low-cost",
        "II. High-benefit, high-cost",
        "III. Low-benefit, low-cost",
        "IV. Low-benefit, high-cost"
      ),
    hjust = 0,
    size = 5
  ) +
  # geom_rect(
  #   aes(
  #     xmax = c(0.5, 1, 0.5, 1),
  #     xmin = c(0, 0.5, 0, 0.5),
  #     ymax = c(1, 1, 0.5, 0.5),
  #     ymin = c(0.5, 0.5, 0, 0)
  #   ),
  #   fill = c("green", "yellow", "yellow", "red")
  # ) +
  geom_point(
    aes(
      x = X1,
      y = X2,
      # alpha = c_target
      alpha = b_target
      # alpha = r_target
    ),
    data = xy
  ) +
  # stat_ellipse(
  #   aes(
  #     x = X1,
  #     y = X2
  #   )
  # ) +
  geom_vline(
    aes(
      # color = "Cost targeting",
      xintercept = 0
    ),
    color = "#E41A1C",
    # size = 1.5
  ) +
  geom_hline(
    aes(
      # color = "Benefit targeting",
      yintercept = 0
    ),
    color = "#377EB8",
    size = 1.5
  ) +
  geom_abline(
    aes(
      # color = "Ratio (C/B) targeting",
      slope = 1, intercept = 0
    ),
    # size = 1.5,
    color = "#4DAF4A"
  ) +
  labs(
    # title = "Potential projects in cost-benefit space: <span style='color:#4DAF4A;'>Ratio (C/B)</span> targeting",
    title = "Potential projects in cost-benefit space: <span style='color:#377EB8;'>Benefit (B)</span> targeting",
    # title = "Potential projects in cost-benefit space: <span style='color:#E41A1C;'>Cost (C)</span> targeting",
    # subtitle = wrapper("Lines illustrate thresholds for which projects would be selected under different targeting schemes; Projects above and to the left of each line would be selected under each scheme"),
    # subtitle = wrapper(" \n "),
    x = "Cost",
    y = "Benefit",
    caption = "Recreation of figures 1-3 of Babcock et al. (1997)"
  ) +
  ggthemes::theme_clean() +
  scale_alpha_discrete(c(1,0.8), guide = NULL) +
  scale_color_discrete("Targeting scheme", guide = NULL) +
  theme(
    plot.title.position = "plot",
    plot.title = element_markdown(),
    text = element_text(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.position = "bottom"
  ) +
  coord_fixed(xlim = c(-3, 3), ylim = c(-3, 3))

ggplot() +
  geom_text(
    aes(
      x = c(-3, 1, -3, 1),
      y = c(3, 3, -2.95, -2.95)
    ),
    label =
      c(
        "I. High-benefit, low-cost",
        "II. High-benefit, high-cost",
        "III. Low-benefit, low-cost",
        "IV. Low-benefit, high-cost"
      ),
    hjust = 0,
    size = 5
  ) +
  # geom_rect(
  #   aes(
  #     xmax = c(0.5, 1, 0.5, 1),
  #     xmin = c(0, 0.5, 0, 0.5),
  #     ymax = c(1, 1, 0.5, 0.5),
  #     ymin = c(0.5, 0.5, 0, 0)
  #   ),
  #   fill = c("green", "yellow", "yellow", "red")
  # ) +
  geom_point(
    aes(
      x = X1,
      y = X2,
      alpha = c_target
      # alpha = b_target
      # alpha = r_target
    ),
    data = xy
  ) +
  # stat_ellipse(
  #   aes(
  #     x = X1,
  #     y = X2
  #   )
  # ) +
  geom_vline(
    aes(
      # color = "Cost targeting",
      xintercept = 0
    ),
    color = "#E41A1C",
    size = 1.5
  ) +
  geom_hline(
    aes(
      # color = "Benefit targeting",
      yintercept = 0
    ),
    color = "#377EB8",
    # size = 1.5
  ) +
  geom_abline(
    aes(
      # color = "Ratio (C/B) targeting",
      slope = 1, intercept = 0
    ),
    # size = 1.5,
    color = "#4DAF4A"
  ) +
  labs(
    # title = "Potential projects in cost-benefit space: <span style='color:#4DAF4A;'>Ratio (C/B)</span> targeting",
    # title = "Potential projects in cost-benefit space: <span style='color:#377EB8;'>Benefit (B)</span> targeting",
    title = "Potential projects in cost-benefit space: <span style='color:#E41A1C;'>Cost (C)</span> targeting",
    # subtitle = wrapper("Lines illustrate thresholds for which projects would be selected under different targeting schemes; Projects above and to the left of each line would be selected under each scheme"),
    # subtitle = wrapper(" \n "),
    x = "Cost",
    y = "Benefit",
    caption = "Recreation of figures 1-3 of Babcock et al. (1997)"
  ) +
  ggthemes::theme_clean() +
  scale_alpha_discrete(c(1,0.8), guide = NULL) +
  scale_color_discrete("Targeting scheme", guide = NULL) +
  theme(
    plot.title.position = "plot",
    plot.title = element_markdown(),
    text = element_text(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.position = "bottom"
  ) +
  coord_fixed(xlim = c(-3, 3), ylim = c(-3, 3))

ggplot() +
  geom_text(
    aes(
      x = c(-3, 1, -3, 1),
      y = c(3, 3, -2.95, -2.95)
    ),
    label =
      c(
        "I. High-benefit, low-cost",
        "II. High-benefit, high-cost",
        "III. Low-benefit, low-cost",
        "IV. Low-benefit, high-cost"
      ),
    hjust = 0,
    size = 5
  ) +
  # geom_rect(
  #   aes(
  #     xmax = c(0.5, 1, 0.5, 1),
  #     xmin = c(0, 0.5, 0, 0.5),
  #     ymax = c(1, 1, 0.5, 0.5),
  #     ymin = c(0.5, 0.5, 0, 0)
  #   ),
  #   fill = c("green", "yellow", "yellow", "red")
  # ) +
  geom_point(
    aes(
      x = X1,
      y = X2,
      # alpha = c_target
      # alpha = b_target
      alpha = r_target
    ),
    data = xy
  ) +
  # stat_ellipse(
  #   aes(
  #     x = X1,
  #     y = X2
  #   )
  # ) +
  geom_vline(
    aes(
      # color = "Cost targeting",
      xintercept = 0
    ),
    color = "#E41A1C",
    # size = 1.5
  ) +
  geom_hline(
    aes(
      # color = "Benefit targeting",
      yintercept = 0
    ),
    color = "#377EB8",
    # size = 1.5
  ) +
  geom_abline(
    aes(
      # color = "Ratio (C/B) targeting",
      slope = 1, intercept = 0
    ),
    size = 1.5,
    color = "#4DAF4A"
  ) +
  labs(
    title = "Potential projects in cost-benefit space: <span style='color:#4DAF4A;'>Ratio (C/B)</span> targeting",
    # title = "Potential projects in cost-benefit space: <span style='color:#377EB8;'>Benefit (B)</span> targeting",
    # title = "Potential projects in cost-benefit space: <span style='color:#E41A1C;'>Cost (C)</span> targeting",
    # subtitle = wrapper("Lines illustrate thresholds for which projects would be selected under different targeting schemes; Projects above and to the left of each line would be selected under each scheme"),
    # subtitle = wrapper(" \n "),
    x = "Cost",
    y = "Benefit",
    caption = "Recreation of figures 1-3 of Babcock et al. (1997)"
  ) +
  ggthemes::theme_clean() +
  scale_alpha_discrete(c(1,0.8), guide = NULL) +
  scale_color_discrete("Targeting scheme", guide = NULL) +
  theme(
    plot.title.position = "plot",
    plot.title = element_markdown(),
    text = element_text(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.position = "bottom"
  ) +
  coord_fixed(xlim = c(-3, 3), ylim = c(-3, 3))



#' **Naidoo et al. 2006:** https://doi.org/10.1016/j.tree.2006.10.003  
#'   
#' - Types of costs: acquisition, management, transaction (and opportunity, damage costs)  
#'   - (Can be continuous or one-off)  
#' - Often based on non-monetary proxies  
#'   - Most often area  
#'   - Sometimes weighted but in often arbitrary ways  
#'   - Efficiency gains from incorporating costs  
#'   
#' **Gap**
#'   
#' - Past looks at culverts have focused on benefits and used simplified cost models  
#' - Past looks at conservation costs have focused on land acquisition costs rather than restoration efforts  
#'   - Unique features of culvert improvement in PNW: upstream land access model, lots of streams/roads, large variation in slope and stream size  
#' - Timely b/c Washington culvert case  
#'   
#' **Research approach**
#'   
#' - Examine variability in cost levels and drivers of costs across culvert projects in PNW using statistical model  
#' - Compare levels and variability of costs to (possibly several) benefit measures  
#' - Apply model to extant culverts to compare costs/benefit distributions over...  
#'   - Space: where are high benefit, low cost culverts?  
#'   - Observed projects vs. all culverts: what kind of decision rule is distribution of projects consistent with?  
#' 
#' 
#' *RQ1:* How much variability is there in costs for culvert improvements?  
#'   
#' - Over space?  
#' - For observed projects vs. potential projects?  
#' - Relative to variability in benefits? (And implications for planning rules/future research)  
#'   
#' *RQ2:* What are drivers of culvert improvement costs?  
#'   
#' - Economic drivers: economies of scale, transaction costs  
#' - Geophysical drivers: stream features, terrain features  
#' - And are these drivers also drivers of benefits (i.e. upstream habitat quality for target species)?  
#' 

#+ include=F

# Load NLCD key
key_nlcd <-
  read_xlsx(
    here(
      "/data/Culverts spatial overlays v 20Jan2021.xlsx"
    ), 
    sheet = 4
  ) %>% 
  as_tibble() %>%
  clean_names() %>%
  rename(
    nlcd_current_class = class,
    nlcd_current_fullclass = classification
  ) %>%
  mutate(across(where(is_character), str_to_sentence)) %>%
  filter(across(description, ~!str_detect(., "Alaska only"))) %>%
  select(-description)

# Load data
df_culv <-
  read_csv(here("output", "culverts_pure_modelling.csv")) %>%
  mutate(
    # project_year = ordered(project_year),
    project_source = relevel(factor(project_source), ref = "OWRI"),
    basin = relevel(factor(basin), ref = "SOUTHERN OREGON COASTAL"),
    fips = factor(fips),
    state_fips = factor(state_fips),
    here_class = as.character(here_class),
    here_speed = relevel(factor(here_speed), ref = 6),
    tot_dist = I(n_worksites * dist_mean)
  ) %>%
  left_join(
    key_nlcd, 
    by = c("nlcd_current" = "value")
  ) %>%
  filter(
    !(nlcd_current_class %in% c("Barren", "Water")),
    tot_dist < 10000
  ) %>%
  mutate(
    nlcd_current_class = relevel(factor(nlcd_current_class), ref = "Forest")
  )
  
  

names(df_culv)

# Data description ----
#+ echo=F
#' # Data description
#' 

#' The unit of observation in our data is a **culvert worksite**. These data
#' include all unique worksites associated with a culvert action in the PNSHP
#' data between 2001 and 2015. Each worksite is associated with a *project*, a
#' set of geographic coordinates, and the number of culverts at the site.
#' Projects are associated with a year, a reporting source, and a unique cost.
#' We also calculate the number of culverts associated with the project (methods
#' found
#' [here](https://vandeynze.github.io/salmon_culverts/culverts_summary.html#Appendix:_Creating_a_Consistent_Project-level_Culvert_Count)).
#' Note that a project may be associated with multiple related worksites,
#' though `r tabyl(df_culv$n_worksites) %>% slice(1) %>% pull(percent) %>%
#' percent` of worksites are uniquely identified to a project.

#'
#' Dependent and explanatory variables included in the empirical model are described below,
#' including a brief justification for inclusion. A more in-depth exploration of
#' these variables can be found in [this
#' report](https://https://vandeynze.github.io/salmon_culverts/spatial_variables_summary.html).  

options(knitr.kable.NA = '')
df_culv %>%
  drop_na(emp_agforest, slope, slope_deg) %>%
  mutate(`Distance between worksites (m)` = I(dist_mean*n_worksites)) %>%
  select(
    `Cost per culvert ($USD2019)` = cost_per_culvert,
    `Number of worksites (count)` = n_worksites,
    `Distance between worksites (m)`,
    `Stream slope (%)` = slope,
    `Bankfull width (m)` = bankfull_width,
    `Paved road` = here_paved,
    `Road speed class` = here_speed,
    `Terrain slope (deg)` = cat_basin_slope,
    `Land cover class` = nlcd_current_class,
    `Housing density (units per sq. km)` = hdens_cat,
    `Construction employment (jobs)` = emp_const,
    `Ag/forestry employment (jobs)` = emp_agforest,
    `Distance to urban area (m)` = ua_dist,
    
      `Density of construction equipment wholesalers (workers per sq. km)` = const_totp,
      `Density of brick, stone, and related wholesalers (workers per sq. km)` = brick_totp,
      `Density of durable metals wholesalers (workers per sq. km)` = metal_totp,
      `Density of sand and gravel sales yards (firms per sq. km)` = sales_coun,
      `Private land, individual or company owner (proportion of land within 500m radius)` = pv_1km_buff,
      `Private land, managed by industry (proportion of land within 500m radius)` = pvi_1km_buff,
      `Private land, managed by non-industrial owner (proportion of land within 500m radius)` = pvn_1km_buff,
      
    `Basin` = basin,
    `Year` = project_year,
    `Reporting source` = project_source
  ) %>%
  mutate(
    `Road speed class` = factor(`Road speed class`),
    `Paved road` = factor(`Paved road`),
    `Land cover class` = factor(`Land cover class`),
    Year = ordered(Year)
  ) %>%
  summarize(
    across(where(is.numeric), list(Mean = mean, `Std. dev.` = sd), .names = "{col}_{fn}", na.rm = TRUE),
    across(where(is.factor), list(`Number of levels` = nlevels), .names = "{col}_{fn}")
  ) %>%
  pivot_longer(everything(), names_to = c("Variable", "stat"), names_sep = "_") %>%
  rowwise() %>%
  mutate(across(where(is.numeric), ~(format(signif(., digits = 3), scientific = FALSE, big.mark = ",")))) %>%
  pivot_wider(Variable, names_from = stat, values_from = value) %>%
  # print(n = Inf)
  kable(
    caption = paste0("Descriptive statistics (n = ", comma(nrow(df_culv %>% drop_na(emp_agforest, slope, slope_deg))), ")"),
    # escape = FALSE,
  ) %>%
  kable_styling("hover", fixed_thead = TRUE)

#' ## Cost estimates  
#'   

#' **Cost per culvert ($USD, 2019)** is our primary dependent variable. This
#' variable can also be interpreted as the *project average costs* at the work
#' site. This variable is constructed by dividing the provided project costs by
#' the number of culverts associated with the project.  
#'   

#' ## Stream hydrological features  
#' 
#' - **Stream slope (% grade)**: slope of stream at road crossing can require more
#' expensive crossing design; identified via COMID matching with NHDPlus
#' attributes.  
#' - **Bankfull width (m)**: bankfull width is the preferred measure of stream width
#' at road crossing, accounting for potential width during high-water events;
#' identified via COMID matching with NHDPlus attributes.  
#' Note that these two variables are interacted to capture design guidelines that indicate
#' increased complexity in culvert designs associated with streams that are both particularly 
#' wide and steep.  

#+ echo=F
#' ## Road features  
#' 
#' - **Road paved (indicator)**: modification of a paved road is more expensive;
#' may also proxy for higher traffic volumes; measured via HERE road data for
#' nearest object.  
#' - **Road speed class (categorical)**: wider roads with more traffic are expected to
#' be more expensive; measured via HERE road data for nearest object; classes range from  2 (fastest) to 7 (slowest).
#' 
#+ echo=F
key_here <- read_xlsx(here("/data/Culverts spatial overlays v 20Jan2021.xlsx"), sheet = 3) %>%
  as_tibble() %>%
  mutate(Classification = str_to_sentence(Classification), Description = str_to_sentence(Description))
  # bind_rows(
  #   tibble(
  #     "Classification" = "Functional class",
  #     "Value" = 6,
  #     "Description" = '"Roads" associated with worksites > 150m from the nearest HERE road for here_class_badmatch.'
  #   )
  # )

key_here %>%
  filter(Classification == "Speed category") %>%
  arrange(Classification) %>%
  kable(caption = "Value key for HERE road variables") %>%
  kable_styling() %>%
  collapse_rows(
    1,
    valign = "top",
    row_group_label_position = "stacked",
    headers_to_remove = 1
  )
#' ## Terrain features  
#' 
#' - **Terrain slope (degrees)**: steeper terrain is expected to require more
#' expensive projects; measured by the
#' ~~[GTOPO30](https://www.usgs.gov/centers/eros/science/usgs-eros-archive-digital-elevation-global-30-arc-second-elevation-gtopo30?qt-science_center_objects=0#qt-science_center_objects)
#' grid cell the worksite falls in~~ slope recorded for the *catchment* the stream is associated with via NHDPlus Selected Attributes data release, as opposed to the stream slope.
#'
#' - **Land cover (categorical)**: different land covers may be associated more
#' expensive projects (e.g. less accessible sites in forest, difficult soils in
#' welands, etc.); identified via cover with worksite coordinates and NLCD land
#' cover layer for nearest available year; here we use the broader NLCD Group
#' definition rather than the more detailed classification (see below).  
#' 
#' - **Elevation (m)**: mean elevation in meters in NHDPlus catchment.
#' 
#+ echo=F
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
    rename(Group = Class) %>%
    filter(across(Description, ~!str_detect(., "Alaska only")))
) %>%
  # print(n = 20)
  knitr::kable() %>%
  kableExtra::kable_styling()
#+ echo=F
#' ## Economic, social, and built-environment characteristics features  
#'
#' - **Housing density (units per sq. km)**: more parcels near worksite
#' introduces complexities related to site access and available areas for
#' staging, etc.; measured for the immediate catchment of stream identified via
#' matching with NHDPlus attribute data.  
#' - **Employment in ag/forestry (jobs in county)**: availability of skilled
#' labor may reduce project costs; employment data from
#' county the worksite is located in via [County Business
#' Patterns](https://www.census.gov/programs-surveys/cbp.html) data.  
#' - **Employment in construction (jobs in county)**: see above.  
#' - **Distance to urban area (m)**: as a measure of access to labor and equipment,
#'  euclidean distance to the nearest census designated urban area, defined as 
#'  contiguous area with at least 50,000 residents.  
#' - **Density of wholesalers (workers/firms per sq. km)**: Using firm point data from 
#' [Homeland Infrastructure Foundation-Level Data (HIFLD)](https://gii.dhs.gov/hifld/), 
#' we calculate a kernel density field (1km resolution grid/100km search radius) to measure 
#' access to equipment and materials required for common culvert replacement designs, including...  
#'   - *Construction equipment (workers)*, representing heavy machinery required for digging, moving materials, etc.,  
#'   - *Brick, stone, and related (workers)*, representing concrete, pavement, and structural materials required to build replacement culverts,  
#'   - *Durable metals (workers)*, representing sheet metal used for piping, rebar, and other structural metals materials, and  
#'   - *Sand and gravel sales yards (firms)*, representing suppliers for fill commonly used to build roads back up after construction.  
#' - **Private land (proportion within 500m radius)**: we use BLM's 2019 Surface Jurisdiction Map to calculate the proportion of land
#' within a 500m radius of each worksite that is privately managed, distinguishing between land managed by individuals or companies 
#' (residential or commercial land), industry (typically forestry in the region), and non-industrial (including private conservation) entities;
#' Note that the baseline group (i.e., 0 in all three proportions) represents a pooled group of all other land ownership types, which includes all 
#' publicly-owned and managed land (i.e., national and state forests and parks, U.S. Fish and Wildlife land, BLM land, etc.).
#' 
#+ echo=F
#' ## Scale and scope features  
#'
#' - **Number of worksites associated with project (count):** addressing multiple
#' culverts under the same project may provide scale benefits, but might also
#' increase complexity; measured via PNSHP database.  
#' - **Distance between project worksites (m)**: more dispersed worksites
#' under a single project may increase project costs due to increased
#' transportation costs (and time); measured as the total euclidean distance
#' between worksites for multiple worksite projects. This variable is interacted 
#' with the number of worksites to allow flexible coordination/scale effects.  
#' - **Action type (categorical):** PNSHP distinguishes between culvert removals
#' and culvert installations, in addition to culvert improvements (the dominate
#' category); we expect removals to be cheapest, followed by improvements and
#' installations; dummies are included when a project includes one or more
#' culverts flagged as either removals or installations.  
#'   
#+ echo=F
#' ## Fixed effects  
#' 
#' - **Year**: the year the project was completed  
#' - **Basin**: the basin (HUC6) where the worksite is located  
#' - **Reporting source**: the reporting source for the project  

# Simple correlations across variables ----
# +
#' # Simple correlations across variables  
#' 
#' Here we present a couple measures of correlation between potential continuous
#' explanatory variables. The figure below show provides Pearson's correlation
#' coefficients for each pair of continuous explanatory variables included in
#' the initial models, along side the dependent variable. Also included is the
#' Variance Inflation Factor for each variable as calculated when all presented
#' variables are included in a simple log-linear model, with cost per culvert as
#' the dependent variable. Because of previously mentioned high correlation
#' between housing density and population density, we include only housing
#' density.

#+ fig.width=10, fig.height=10, echo=F, message=F, warning=F
# Grab only variables needed for corr. plot
df_corr <-
  df_culv %>%
  mutate(`Distance between sites` = I(dist_mean*n_worksites)) %>%
  select(
    # employment vars
    `Employment, construction` = emp_const, `Employment, ag/forestry` = emp_agforest,
    # pop vars
    `Housing density` = hdens_cat, `Distance to urban area` = ua_dist,
    # popdens_cat,
    # stream vars
    `Stream slope` = slope, `Bankfull width` = bankfull_width,
    `Terrain slope` = cat_basin_slope, `Elevation` = cat_elev_mean,
    `Distance between sites`, `Number of worksites` = n_worksites,
    `Cost per culvert` = cost_per_culvert,
    lg_1km_buff,
    stall_1km_buff,
    # usfs_1km_buff,
    # fedother_1km_buff,
    # blm_1km_buff,
    # pvall_1km_buff,
    pvi_1km_buff,
    pvn_1km_buff,
    pv_1km_buff,
    const_totp,
    brick_totp,
    metal_totp,
    sales_coun
  ) %>%
  drop_na()

mat_corr <-
  df_corr %>%
  cor() %>%
  as.matrix()


mat_vifs <- imcdiag(lm(log(`Cost per culvert`) ~ ., df_corr), method = "VIF")[[1]] %>% as.matrix
mat_vifs <- rbind(mat_vifs, c(NA, NA))
diag(mat_corr) <- mat_vifs[,1] %>% round(1)
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
    "Correlations among potential covariates",
    "VIF along diagonal"
  ) +
  coord_fixed(
    # ylim = c(0, 20),
    # xlim = c(0, 20),
    expand = FALSE,
    clip = "off"
  )

#+
#' It looks like  distance between worksites and the number of culverts is
#' positively correlated, and both are negatively correlated with average
#' project costs. We would expect distance to increase costs but the
#' number of culverts to decrease costs (due to economies of scale), all else
#' equal. Disentangling these effects should be possible with
#' multiple regression.  
#' 
#' Stream slope is negatively correlated with bankfull width, which means wider
#' streams tend to be less steep. Stream slope is also positively correlated
#' with terrain slope, as mentioned earlier. None of the three are strongly
#' correlated with costs.  
#' 
#' Finally, the two employment variables are positively correlated, and
#' construction employment is positively correlated with housing density.
#' Ag/forestry employment is weakly positively correlated with measures that
#' indicate more rugged terrain such as stream and terrain slope. Housing density
#' and to a lesser degree construction employment are positively correlated with
#' costs.  
#' 
#' ~~No variables have particularly large VIFs, suggesting little potential for
#' error-inflating multicollinearity.~~ (A large VIF indicates that the variable is
#' strongly correlated with the other variables in the model, leading to
#' inflated standard errors and limiting the model's usefulness for prediction
#' or inference.)  
#' 

#+ estimate, include=F
# Estimate models ----
# Full model
mod_full <- 
  lm(
    log(cost_per_culvert) ~
    # log(I(adj_cost / n_worksites)) ~
      # Scale/scope of project controls: number of culverts, distance between worksites, type of culvert work
      n_worksites * tot_dist + # factor(I(n_worksites == 1)) +
      action_fishpass_culvrem_prj + action_fishpass_culvinst_prj +
      # Stream features at worksite: slope, bankfull width
      slope * bankfull_width + 
      # Road features at worksite: paved, road class
      factor(here_paved) + factor(here_speed) +
      # Physical features of worksite: terrain slope, land cover
      # slope_deg + factor(nlcd_current_class) +
      cat_basin_slope + cat_elev_mean + factor(nlcd_current_class) +
      # Population features: housing density, jobs in construction, jobs in ag/forestry
      hdens_cat + emp_const + emp_agforest + ua_dist + # factor(publand) +
      # Supplier density
      # merch_totp + 
      const_totp +
      brick_totp +
      metal_totp +
      sales_coun +
      # sand_count +
      # Land ownership
      # pvall_1km_buff +
      pv_1km_buff +
      pvi_1km_buff +
      pvn_1km_buff +
      # stall_1km_buff +
      # I(fedother_1km_buff +
      # blm_1km_buff +
      # usfs_1km_buff +
      # lg_1km_buff +
      # Fixed effects
      basin + factor(project_year) + project_source,
    df_culv
  )

write_rds(mod_full, here("output/costfits/ols_full.rds"))

coeftest(
# summary(
  mod_full, 
         vcov. = vcovCL(mod_full, cluster = ~ project_id, type = "HC3")
         # vcov. = vcovHC(mod_full)
)

# Remove some variables
# Remove scale/scope controls
# mod_drop_scale <-
#   mod_full %>% update(. ~ . - (n_culverts + log(dist_mean+1) + factor(I(n_worksites == 1))))
# mod_drop_scope <-
#   mod_full %>% update(. ~ . - (n_culverts + log(dist_mean+1) + factor(I(n_worksites == 1)) + action_fishpass_culvrem_prj + action_fishpass_culvinst_prj))
# 
# # Remove physical worksite features
# mod_drop_phys <-
#   mod_full %>% update(. ~ . - (slope_deg + factor(nlcd_current)))
# 
# # Remove pop features
# mod_drop_pop <-
#   mod_full %>% update(. ~ . - (hdens_cat + emp_const + emp_agforest))
# mod_drop_emp <-
#   mod_full %>% update(. ~ . - (emp_const + emp_agforest))

# Remove everything but streams and roads
# mod_drop_streamsonly <-
#   mod_full %>% update(
#     . ~ . - (
#       n_culverts + log(dist_mean + 1) + factor(I(n_worksites == 1)) +
#         action_fishpass_culvrem_prj + action_fishpass_culvinst_prj +
#         here_paved + factor(here_class) +
#         slope_deg + factor(nlcd_current) +
#         hdens_cat + emp_const + emp_agforest
#     )
#   )
# 
# mod_drop_roadsonly <-
#   mod_full %>% update(
#     . ~ . - (
#       n_culverts + log(dist_mean + 1) + factor(I(n_worksites == 1)) +
#         action_fishpass_culvrem_prj + action_fishpass_culvinst_prj +
#         slope * bankfull_width +
#         slope_deg + factor(nlcd_current) +
#         hdens_cat + emp_const + emp_agforest
#     )
#   )
# 
# mod_drop_roadsandstreams <-
#   mod_full %>% update(
#     . ~ . - (
#       n_culverts + log(dist_mean + 1) + factor(I(n_worksites == 1)) +
#         action_fishpass_culvrem_prj + action_fishpass_culvinst_prj +
#         slope_deg + factor(nlcd_current) +
#         hdens_cat + emp_const + emp_agforest
#     )
#   )

# Remove fixed effects
mod_nofe <-
  mod_full %>% update(. ~ . - (basin + factor(project_year) + project_source))
mod_nofe_onlysource <-
  mod_full %>% update(. ~ . - (basin + factor(project_year)))
mod_nofe_onlyyear <-
  mod_full %>% update(. ~ . - (basin + project_source))
mod_nofe_onlybasin <-
  mod_full %>% update(. ~ . - (factor(project_year) + project_source))
mod_nofe_nobasin <-
  mod_full %>% update(. ~ . - basin)
mod_nofe_noyear <-
  mod_full %>% update(. ~ . - factor(project_year))
mod_nofe_nosource <-
  mod_full %>% update(. ~ . - project_source)

# Focus on certain basins
# mod_basins_wore <-
#   mod_full %>% update(. ~ . - project_source - basin, data = df_culv %>% filter(basin %in% c("SOUTHERN OREGON COASTAL", "NORTHERN OREGON COASTAL", "WILLAMETTE")))
# mod_basins_wwash <-
#   mod_full %>% update(. ~ . - project_source - basin, data = df_culv %>% filter(basin %in% c("WASHINGTON COASTAL", "PUGET SOUND")))
mod_basins_core <-
  mod_full %>% update(. ~ . - project_source - basin, data = df_culv %>% filter(basin %in% c("WASHINGTON COASTAL", "PUGET SOUND", "SOUTHERN OREGON COASTAL", "NORTHERN OREGON COASTAL", "WILLAMETTE")))

# Focus on certain sources
mod_sources_core <-
  mod_full %>% update(. ~ . - project_source - basin, data = df_culv %>% filter(project_source == "OWRI" | project_source == "WA RCO"))
# mod_sources_warco <-
#   mod_full %>% update(. ~ . - project_source - basin, data = df_culv %>% filter(project_source == "WA RCO"))

#+ methods, echo=F, message=F, warning=F
#' # Estimation  
#' 
#' We estimate log-linear models estimated via OLS, with the average project
#' cost as the dependent variable and the worksite as the unit of observation.
#' Stream slope and bankfull width are interacted in this specification.
#' Recommendations in culvert engineering reports indicate that more expensive
#' culvert designs are particularly necessary when both of these variables are
#' extreme, and an interaction term can capture this effect.
#' 

# Giving up on rendering the equation for now
# extract_eq(mod_full)

#'
#' In addition to the fully specified model (mod_full), we present nine
#' alternative models that include different fixed effects configurations or
#' only sub-samples of the data focused on basin or reporting source criteria.
#' For basins, we provide results estimated on a "core" group representing the
#' five most frequently represented basins (Washington Coastal, Puget Sound,
#' Southern Oregon Coastal, Northern Oregon Coastal, Willamette). Finally, we
#' also estimate the model on only projects reported by OWRI and WA
#' RCO, to examine how the two sources who report the most projects influence
#' the overall results.
#' 

#' The resulting coefficients for the fixed effects and categorical variables,
#' when exponentiated, can be interpreted as the ratio of average costs for that
#' group relative those of the base group. Results for continuous variables are
#' presented as exponentiated average marginal effect of a single standard
#' deviation change, which can be interpreted as the ratio of costs relative to
#' a worksite with a standard deviation lower for the variable.  
#'
#' ## Coefficient estimates  

#+ echo=F
# Present model estimates ----
mods <- mget(ls(patter = "mod_"))
mods <- mods[c(2, 1, 3:length(mods))]
# mods <- mods[["mod_full"]]
mods_pars <-
  map_df(mods, ~coeftest(., vcov. = vcovCL(., cluster = ~ project_id, "HC3")) %>% tidy, .id = "model") %>%
  complete(model, term) %>%
  mutate(
    stars = case_when(
      p.value < 0.01 ~ "***",
      p.value < 0.05 ~ "**",
      p.value < 0.1 ~ "*",
      TRUE ~ ""
    ),
    full.est = if_else(
      is.na(estimate),
      "&#8210;",
      paste0(
        format(signif(estimate, 3), scientific = FALSE, drop0trailing = TRUE, trim = TRUE),
        stars,
        "<br>(", format(signif(std.error, 3), scientific = FALSE, drop0trailing = TRUE, trim = TRUE), ")"
      )
    ),
    term = case_when(
      term == "(Intercept)" ~ "Intercept",
      term == "action_fishpass_culvinst_prj" ~ "Culvert installation (dummy)",
      term == "action_fishpass_culvrem_prj" ~ "Culvert removal (dummy)",
      term == "n_worksites" ~ "Number of worksites",
      term == "tot_dist" ~ "Distance between worksites",
      # term == "log(dist_mean + 1)" ~ "Mean distance between worksites, log",
      # term == "factor(I(n_worksites == 1))TRUE" ~ "Single worksite (dummy)",
      term == "slope" ~ "Stream slope",
      term == "bankfull_width" ~ "Bankfull width",
      str_detect(term, "here_paved") ~ "Road paved (dummy)",
      term == "cat_basin_slope" ~ "Terrain slope",
      term == "cat_elev_mean" ~ "Elevation",
      term == "hdens_cat" ~ "Housing density",
      term == "emp_const" ~ "Construction employment",
      term == "emp_agforest" ~ "Ag/forestry employment",
      term == "ua_dist" ~ "Distance to urban area",
      
      term == "const_totp" ~ "Density (employee-weighted) of construction equipment suppliers",
      term == "brick_totp" ~ "Density (employee-weighted) of brick, concrete, and related materials suppliers",
      term == "metal_totp" ~ "Density (employee-weighted) of metal materials suppliers",
      term == "sales_coun" ~ "Density of sand and gravel sales yards",
      term == "pv_1km_buff" ~ "Private land, individual or company (% 500m buffer)",
      term == "pvi_1km_buff" ~ "Private land, managed by industry  (% 500m buffer)",
      term == "pvn_1km_buff" ~ "Private land, managed by non-industrial owner  (% 500m buffer)",
      
      term == "slope:bankfull_width" ~ "Stream slope X bankfull width",
      term == "n_worksites:tot_dist" ~ "Number of worksites X distance",
      TRUE ~ term
    ),
    term = str_replace(term, "project_source", "Project source: "),
    term = str_replace(term, "basin", "Basin: "),
    term = str_replace(term, "factor[(]project_year[)]", "Year: "),
    term = str_replace(term, "factor[(]nlcd_current_class[)]", "Land cover: "),
    term = str_replace(term, "factor[(]here_speed[)]", "Road speed class: ")
  ) %>%
  select(
    model, term, full.est
  ) %>%
  pivot_wider(id_cols = model, names_from = term, values_from = full.est) %>%
  select(
    model,
    Intercept,
    # Stream features
    "Stream slope",
    "Bankfull width",
    "Stream slope X bankfull width",
    # Road features
    starts_with("Road"),
    # Land features
    "Terrain slope", "Elevation",
    starts_with("Land cover"),
    # Pop features
    "Housing density",
    ends_with("employment"),
    "Distance to urban area",
    "Density (employee-weighted) of construction equipment suppliers",
    "Density (employee-weighted) of brick, concrete, and related materials suppliers",
    "Density (employee-weighted) of metal materials suppliers",
    "Density of sand and gravel sales yards",
    "Private land, individual or company (% 500m buffer)",
    "Private land, managed by industry  (% 500m buffer)",
    "Private land, managed by non-industrial owner  (% 500m buffer)",
    # Scale/scope features
    "Number of worksites",
    starts_with("Distance between "),
    "Number of worksites X distance",
    starts_with("Culvert "),
    starts_with("Project source: "),
    starts_with("Year: "),
    starts_with("Basin: ")
  )
mods_stats <-
  map_df(mods, glance, .id = "model") %>%
  mutate(
    `Adj. R2` = signif(adj.r.squared, 3),
    AIC = round(AIC, 1),
    BIC = round(BIC, 1),
    N = df + df.residual
  ) %>%
  select(model, `Adj. R2`, AIC, BIC, N) %>%
  mutate_all(~as.character(.))
mods_pars %>%
  left_join(mods_stats, by = "model") %>%
  pivot_longer(-model, names_to = "Term", values_to = "Value") %>%
  pivot_wider(id_cols = Term, names_from = model, values_from = Value) %>%
  select(Term, mod_full, contains("nofe"), contains("basins"), contains("sources")) %>%
  # kable() %>%
  kable(
    caption = "Cost models",
    escape = FALSE,
    table.attr = "class=\"fixedcol\"",
    align = 
      paste0(
        "l", 
        paste(
          rep(
            "c",
            length(mods)
            ), 
          collapse = ""
          )
        )
    ) %>%
  kable_styling("hover", fixed_thead = TRUE) %>%
  column_spec(1 , bold = TRUE) %>%
  add_header_above(
    c(
      " " = 1,
      " " = 1,
      "Alternative fixed effects" = 7,
      "Sub-samples" = 2
    )
  ) %>%
  row_spec(c(ncol(mods_pars) - 1, ncol(mods_pars) + ncol(mods_stats) - 2), extra_css = "border-bottom: 1px solid") %>%
  add_footnote("* p < 0.1, ** p < 0.05, *** p < 0.01; Heteroskedasticity-consistent clustered standard errors in parentheses (HC3, clustered at project-level)", notation = "none") %>%
  scroll_box(height = "800px")

#' ## Model fit discussion  
#'

#' The full model has an adjusted R-squared of `r mod_full %>% glance() %>% pull(adj.r.squared) %>% round(3)`,
#' indicating a decent model fit. The version of the model with no fixed effects has an adjusted R-squared of
#' `r mod_nofe %>% glance() %>% pull(adj.r.squared) %>% round(3)`, indicating that a
#' significant amount of variability is explained by the additional explanatory
#' variables. When fixed effect categories are removed, we can check which fixed
#' effects explain the most variation relative to each other. It looks like
#' reporting source accounts for the most variation, followed by basin then
#' year, based on the relative R-squares of models where each is removed and
#' where each is included on its own.
#' 

#' We can also compare AIC and BIC; the model with only fixed effects for
#' reporting source is preferred on the basis of BIC, which includes a stronger
#' penalty for the number of estimated parameters, while the full model is
#' superior on the basis of AIC. We use the full model in what proceeds as the
#' preferred model.  
#' 

#'
#' When the model is fit only on culverts in the "core" basins or sources,
#' adjusted R-squared falls signifciantly This suggests that information from
#' the additional sources and outside the core basins improves the fit of the
#' model.  
#' 

#+ echo=F, message=F, warning=F
# Plot figures ----
#' # Model visualizations  
#'
#' ## Slope and bankfull width interaction effect  
#' 

#' To demonstrate the nuance in how bankfull width and slope jointly influence
#' costs, we present predicted cost contours across both variables.  
#' 

#+ echo=F, message=F, warning=F, fig.dim=c(8,8)
# ____ Slope and width effect space ----
mods <- mods["mod_full"]

predict_cost_interaction <-
  function(
    model,
    var1 = "slope",
    var2 = "bankfull_width",
    lims1 = c(0, 0.3),
    lims2 = c(0, 50),
    by1 = 0.01,
    by2 = 5
  ) {
    model %>%
      ggpredict(
        c(
          paste0(var1, " [", paste(lims1, collapse = ":"), " by=", by1, "]"),
          paste0(var2, " [", paste(lims2, collapse = ":"), " by=", by2, "]")
        )
      ) %>%
      tibble %>%
      mutate(
        !!var2 := as.numeric(as.character(group)),
        !!var1 := x,
        x = NULL,
        group = NULL
      )
  }

fig_bankfullslope <- (
  map_df(mods, predict_cost_interaction, .id = "model") %>% 
    
  # filter(model == "mod_full") %>%
  ggplot() +
    aes(
      x = slope,
      y = bankfull_width,
      z = predicted
      # z = log(predicted)
    ) +
    geom_contour_filled(
      breaks = c(0, c(0.25, 0.5, 0.75, 1, 1.5, 2, 2.5)*ggpredict(mod_full, "slope [mean]")["predicted"]%>%pull, Inf)
      # breaks = log(c(0, seq(10000, 25000, 2500), Inf))
    ) +
    geom_point(
      aes(
        x = slope,
        y = bankfull_width,
        z = NULL
      ),
      data = df_culv,
      color = "grey30",
      alpha = 0.3,
      size = 1
    ) +
    geom_contour(
      breaks = 
        ggpredict(
          mod_full,
          "slope [mean]"
        )["predicted"],
      color = "black",
      linetype = "solid",
      size = 1
    ) +
    geom_contour(
      breaks = 
        ggpredict(
          mod_full,
          "slope [mean]",
          vcov.fun = "vcovHC",
          vcov.type = "HC3",
        )[c("conf.low", "conf.high")],
      color = "black",
      linetype = "dashed",
      # size = 1
    ) +
    scale_x_continuous(labels = function(x) {
      x*100
    }) +
    scale_fill_brewer(
      name = str_wrap("Predicted cost per culvert, relative to mean", 30),
      direction = -1,
      palette = "RdBu",
      labels = c(
        "0.25 to 0.5 x mean",
        "0.5 to 0.75 x mean",
        "0.75 to 1 x mean",
        "1 to 1.5 x mean",
        "1.5 to 2 x mean",
        "2 to 2.5 x mean",
        "Over 2.5 x mean"
      )
    ) +
    # facet_wrap("model", nrow = round(sqrt(length(mods)))) +

    theme(
      legend.position = c(0.96, 0.96), legend.justification = c(1, 1),
      aspect.ratio = 1,
      panel.background = element_rect(fill = "white"),
      # text = element_text(size = 20)
    ) +
    labs(
      # title = wrapper("Predicted average costs by bankfull width (m) and slope (% grade)"),
      # subtitle = 
      #   wrapper(
      #     "Points represent underlying observations; Lines indicates cost contour at mean slope and banfull width with 95% c.i."),
      x = "Slope (% grade)",
      y = "Bankfull width (m)"
    )
)


df_culv %>%
  ggplot() +
  aes(
    x = slope,
    y = bankfull_width,
    color = cost_per_culvert
  ) +
  geom_point() +
  scale_color_fermenter(
    name = "Cost per culvert ($USD)",
    breaks = c(
      0,
      5000,
      7500,
      10000,
      12500,
      15000,
      20000,
      40000,
      80000,
      160000,
      Inf
    ),
    direction = -1,
    labels = label_dollar(),
    palette = "RdBu",
    # guide = "legend"
  ) +
  theme(
    legend.position = c(0.9, 0.8),
    legend.key.size = unit(1, "cm"),
    plot.title.position = "plot"
  ) +
  labs(
    title = "Observed average cost by bankfull width (m) and slope (% grade)",
    subtitle = wrapper("Difficult to see pattern in raw data, likely due large underlying variation resulting from fixed effects (different basins, years, etc.)"),
    x = "Slope",
    y = "Bankfull width"
  )
#+
#' Worksites are distributed along a slope - bankfull width convex curve, with
#' few projects both high slope and high width. This pattern mirrors the cost
#' contours over slope - bankfull width space. Comparing the observed projects
#' to other culverts in the Washington or Oregon inventories will show whether
#' this relationship exists for all culverts or whether projects were selected
#' along the cost curve. That is, would projects that did not occur exist in the
#' upper-right space?  
#' 

#' ## Number of worksites and total distance interaction effect  
#' 
#+ echo=F, message=F, warning=F, fig.dim=c(8,8)
# ____ Worksites and distance effect space ----
fig_sitesdist <- (
  
  map_df(mods, predict_cost_interaction, var1 = "n_worksites", var2 = "tot_dist", lims1 = c(0, 6), lims2 = c(0, 10000), by1 = 0.1, by2 = 100, .id = "model") %>%
    # filter(model == "mod_full") %>%
    ggplot() +
    aes(
      x = n_worksites,
      y = tot_dist/1000,
      z = predicted
    ) +
    geom_contour_filled(
      breaks = c(0, c(0.25, 0.5, 0.75, 1, 1.5, 2, 2.5)*ggpredict(mod_full, "n_worksites [1]")["predicted"]%>%pull, Inf)
    ) +
    scale_fill_brewer(
      name = str_wrap("Predicted cost per culvert, relative to mean", 30),
      direction = -1,
      palette = "RdBu",
      limits = c(
        "(0, 4004]",
        "(4004, 8007]",
        "(8007, 12011]",
        "(12011, 16014]",
        "(16014, 24021]",
        "(24021, 32028]",
        "Blah"
      ),
      labels = c(
        "0.25 to 0.5 x mean",
        "0.5 to 0.75 x mean",
        "0.75 to 1 x mean",
        "1 to 1.5 x mean",
        "1.5 to 2 x mean",
        "2 to 2.5 x mean",
        "Over 2.5 x mean"
      )
    ) +
    # facet_wrap("model", nrow = round(sqrt(length(mods)))) +
    geom_violin(
      aes(
        group = n_worksites,
        y = tot_dist/1000,
        z = NULL
      ),
      data = df_culv %>% filter(n_worksites <= 6, tot_dist <= 1e5),
      color = "grey30",
      alpha = 0.3
    ) +
    # geom_point(
    #   aes(x = x, y = y, shape = shape), size = 2, stroke = 2,
    #   inherit.aes = FALSE,
    #   data = tibble(x = c(1, 2), y = c(0, 5), shape = as.character(c(1, 4)))
    # ) +
    geom_contour(
      breaks = 
        ggpredict(
          mod_full,
          "n_worksites [1]",
          condition = c("tot_dist" = 0)
        )["predicted"],
      size = 1, color = "black",
      linetype = "solid"
    ) +
    geom_contour(
      breaks = 
        ggpredict(
          mod_full,
          "n_worksites [1]",
          condition = c("tot_dist" = 0),
          vcov.fun = "vcovHC",
          vcov.type = "HC3",
        )[c("conf.low", "conf.high")],
      color = "black",
      linetype = "dashed"
    ) +
    theme(
      legend.position = c(0.04, 0.96), legend.justification = c(0, 1),
      aspect.ratio = 1,
      panel.background = element_rect(fill = "white"),
      # text = element_text(size = 20)
    ) +
    scale_x_continuous(n.breaks = 6, limits = c(0,6)) +
    scale_shape_manual(values = c(4, 8), guide = NULL) +
    # scale_y_log10() +
    labs(
      # title = str_wrap("Predicted average costs by number of worksites and total distance between worksites (km)", 65),
      # subtitle = str_wrap("Violin plots represent underlying observations"),
      x = "Number of worksites",
      y = "Total distance between worksites (km)"
    )
)
    
  #' We can repeat the exercise for the number of worksites and total distance
  #' between worksites to examine the trade-off between economies of scale from
  #' grouping multiple worksites under one project and increased costs in
#' coordination as proxied by total distance between sites. The contours of this
#' cost surface can be interpreted as the distance limit for which adding an
#' additional worksite to a project is associated with economies or
#' dis-economies of scale.
#' 

#' For example, consider a potential project with one worksite (indicated by
#' **X**) considering expanding to include a second worksite located 5km away
#' (indicated by an **\***). Because the second worksite would increase the total
#' distance between worksites below the distance where the cost contour
#' (indicated by the solid line, with dashed lines indicating a 95% c.i.)
#' crosses two worksites, expanding the project would be associated with
#' economies of scale.
#' 

#' That these contours tend to be quite steep initially indicates strong
#' potential for economies of scale when opportunities to group nearby worksites
#' under a single project arise. As the number of worksites increases, these
#' contours flatten, indicating that the maximum distance for an efficient
#' additional worksite drops quickly.  
#' 

library(patchwork)
cowplot::save_plot(

    plot = fig_bankfullslope / fig_sitesdist + plot_layout(guides = "collect") + plot_annotation(tag_levels = 'a'),
    filename = here("output/figs/culverts/fig_contours.png"), base_width = 6.5, base_height = 7.5
  
)

#' ### Continuous variables  
#' 

#' To display how costs vary across the continuous explanatory variables, we
#' plot average marginal effects. These are scaled to a standard deviation
#' change for each variable. Average marginal effects are calculated with other
#' variables held at their means, which is most relevant for slope and bankfull
#' width for which the model allows an interaction effect. The resulting
#' estimate is exponentiated so that it can be interpreted as the expected ratio
#' of costs resulting from a standard deviation change in the continuous
#' variable.
#' 

#+ echo=F, message=F, warning=F, fig.dim=c(8,12)
# ____ Marginal effects plots ----
# Custom wrapper of margins::margins for use with map_df
margins_custom <-
  function(mod, terms) { # Takes a model and a character vector of terms
    margins(
      mod, 
      variables = terms, 
      change = "sd", 
      vcov = vcovCL(mod, ~ project_id, "HC3")
    ) %>% summary %>% clean_names %>% # Returns marginal effect of a 1 s.d. change in variable
      rowwise() %>%
      mutate(
        sd = sd(mod$model[factor] %>% pull)
      )
  }

map_df(
  mods, 
  margins_custom,
  terms =
    c(
      "bankfull_width",
      "slope",
      "cat_basin_slope",
      "cat_elev_mean",
      "n_worksites",
      "tot_dist",
      "hdens_cat",
      "emp_const",
      "emp_agforest",
      "ua_dist",
      
      
      "const_totp",
      "brick_totp",
      "metal_totp",
      "sales_coun",
      "pv_1km_buff",
      "pvi_1km_buff",
      "pvn_1km_buff"
      
    ),
  .id = "model"
  ) %>%
  mutate(
    factor = case_when(
      factor == "bankfull_width" ~ "Bankfull width",
      factor == "slope" ~ "Stream slope",
      factor == "cat_basin_slope" ~ "Terrain slope",
      factor == "cat_elev_mean" ~ "Elevation",
      factor == "n_worksites" ~ "Number of worksites",
      factor == "tot_dist" ~ "Distance between worksites",
      factor == "hdens_cat" ~ "Housing density",
      factor == "emp_const" ~ "Construction employment",
      factor == "emp_agforest" ~ "Ag/forestry employment",
      factor == "ua_dist" ~ "Distance to urban area",
      
      factor == "const_totp" ~ "Construction equipment suppliers",
      factor == "brick_totp" ~ "Brick, concrete, and related materials suppliers",
      factor == "metal_totp" ~ "Metal materials suppliers",
      factor == "sales_coun" ~ "Sand and gravel sales yards",
      factor == "pv_1km_buff" ~ "Managed by individual or company",
      factor == "pvi_1km_buff" ~ "Managed by industry",
      factor == "pvn_1km_buff" ~ "Managed by non-industrial owner"
      
    ),
    group = case_when(
      factor %in% c("Bankfull width", "Stream slope") ~ "Stream features",
      factor %in% c("Terrain slope", "Elevation") ~ "Terrain features",
      factor %in% c("Distance between worksites", "Number of worksites") ~ "Project scale",
      factor %in% c("Construction employment", "Ag/forestry employment", "Housing density", "Distance to urban area") ~ "Population features",
      factor %in% c(
        "Construction equipment suppliers",
        "Brick, concrete, and related materials suppliers",
        "Metal materials suppliers",
        "Sand and gravel sales yards"
      ) ~ "Supplier Density",
      factor %in% c(
        "Managed by individual or company",
        "Managed by industry",
        "Managed by non-industrial owner"
      ) ~ "Private Land Ownership (% in 500m buffer)"
    ),
    estimate = exp(ame),
    conf.low = exp(lower),
    conf.high = exp(upper),
    p.value = I(p < 0.05),
    mod.color = case_when(
      p.value == TRUE & model == "mod_full" ~ "sig-pref",
      p.value == TRUE & model != "mod_full" ~ "sig-nopref",
      p.value == FALSE & model == "mod_full" ~ "nosig-pref",
      p.value == FALSE & model != "mod_full" ~ "nosig-nopref"
    ),
    mod.color = ordered(mod.color, levels = c("sig-pref", "sig-nopref", "nosig-pref", "nosig-nopref"))
  ) %>%
  select(model, factor, estimate, conf.low, conf.high, p.value, group, sd) %>%
  ggplot() +
  geom_pointrange(
    aes(
      y = factor,
      x = estimate,
      xmin = conf.low,
      xmax = conf.high,
      group = model,
      color = p.value
    ),
    size = 1,
    # shape = 21,
    position = position_dodge(width = 0.8)
  ) +
  geom_label(
    aes(
      label = round(estimate, 2),
      y = factor
    ),
    x = -0.1
    # size = 18
  ) +
  scale_color_manual(values = c("darkolivegreen3", "darkgreen", "grey60", "grey80")) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  xlim(-0.25, 2.5) +
  labs(
    y = NULL,
    x = "Project average costs", 
    title = "Marginal effects for continuous variables",
    subtitle = "Project average costs relative to a single standard deviation shift",
    caption = str_wrap("Lines indicate 95% confidence interval based on clustered robust standard errors; Significant coefficients highlighted in dark; Point-range for metal supplier density excluded due to scale mismatch")
  ) +
  theme(
    legend.position = "none",
    plot.title.position = "plot",
    text = element_text(size = 18)
  ) +
  facet_wrap("group", ncol = 1, scales = "free_y")

#' A standard deviation increase in number of worksites is associated with 46%
#' lower average costs in the preferred model when other variables, most
#' importantly distance between worksites, are held at their means. On the other
#' hand, a standard deviation increase in distance between worksites is
#' associated with average costs 13% higher, though this marginal effect is not
#' statistically different from zero. This conflict is at the core of the
#' managerial tradeoffs to consider when grouping worksites under a single
#' project, as described in the preceeding section. That the negative
#' association between costs and the number of worksites exceeds the positive
#' association with total distance for the mean project suggests that there are
#' unexploited opportunities for economies of scale.
#'
#' The average marginal effects for all population features are insignificant in
#' the preferred model. ~~However, in some of the alternative models, housing
#' density and distance to urban area has a slight positive association with
#' costs. The housing density effect might be evidence of increased access costs
#' due to negotiating with multiple landowners, while the distance association
#' may be evidence of increased costs due to lack of access to materials or
#' labor. We are in the process of gathering improved proxies for each of these
#' potential mechanisms.~~  
#' 

#' One reason the population features may be insignificant is because we measure
#' access to labor and materials in alternative ways in the model. Worksites
#' closer to construction equipment and brick/concrete firms, and to a lesser
#' extent gravel/sand sales yards, are associated with significantly lower costs,
#' while those closer to metal suppliers have much higher costs.
#' 

#' Private land ownership influences costs in an interesting way. Note that for
#' these variables, the implicit baseline are worksites with no
#' privately-managed land within a 500m buffer. Such culverts are most often
#' owned by government agencies. Compared to this baseline, culverts surrounded
#' by land managed by a non-industrial private owner (i.e., non-profit
#' conservation groups) are associated with higher costs, while culverts
#' surrounded by industrially-managed land (i.e., land managed for forestry) are
#' associated with lower costs. This key result indicates efficiencies
#' associated with barrier improvements conducted by large forest landowners.

#' Bankfull width and stream slope both have positive associations in the
#' preferred models. A standard deviation increase in either is associated with
#' about fifty percent higher average costs when the other is held at its mean.
#' As seen in the earlier figure and raw coefficients, this effect is driven
#' largely by the interaction term in the model.
#'
#' Finally, a standard deviation increase in the terrain slope of the catchment
#' the worksite is located in is associated with 19% higher costs. That is,
#' culverts in hillier or more mountainous areas are more expensive to improve.
#' On the otherhand, higher eleveations are associated with lower costs, though
#' this relationship is not statistically significant.  
#' 

#' ## Fixed effects and categorical variables  
#'   

#' We can examine the fixed effect estimates to compare relative expected costs
#' across groups. We do this by exponentiating the point estimates. The
#' resulting value can be interpreted as the ratio of costs in a given group
#' relative to a base group.  
#' 

# ____ Fixed effects plots ----
#' ### Year effects  
#+ echo=F, message=F, warning=F, fig.dim=c(8,8)

# Year FE
map_df(mods, ~ coeftest(., vcovHC(., "HC3")) %>% tidy(conf.int = TRUE), conf.int = TRUE, .id = "model") %>%
  filter(str_detect(term, "year")) %>%
  mutate(
    estimate = exp(estimate),
    conf.low = exp(conf.low),
    conf.high = exp(conf.high),
    year = str_remove(term, "factor[(]project_year[)]"),
    p.value = I(p.value < 0.05),
    mod.color = case_when(
      p.value == TRUE & model == "mod_full" ~ "sig-pref",
      p.value == TRUE & model != "mod_full" ~ "sig-nopref",
      p.value == FALSE & model == "mod_full" ~ "nosig-pref",
      p.value == FALSE & model != "mod_full" ~ "nosig-nopref"
    ),
    mod.color = ordered(mod.color, levels = c("sig-pref", "sig-nopref", "nosig-pref", "nosig-nopref"))
  ) %>%
  select(model, year, estimate, conf.low, conf.high, mod.color) %>%
  # left_join(mods_years_counts, by = c("year" = "completed_year")) %>%
  # mutate(
  #   year_n = paste0(
  #     year,
  #     "\n(n = ",
  #     comma(n),
  #     ")"
  #   )
  # ) %>%
  ggplot() +
  geom_pointrange(
    aes(
      x = year,
      y = estimate,
      ymin = conf.low,
      ymax = conf.high,
      group = model,
      color = mod.color
    ),
    size = 1,
    position = position_dodge(width = 0.8)
  ) +
  scale_color_manual(values = c("darkgreen", "darkolivegreen3", "grey60", "grey80")) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(
    x = "Year", 
    y = "Project average costs", 
    title = "Year fixed effects",
    subtitle = "Project average costs relative to 2001 levels",
    caption = "Lines indicate 95% confidence interval; Significant coefficients highlighted in dark"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.75
    ),
    text = element_text(size = 18),
    plot.title.position = "plot"
  )

#' For the preferred model, project average costs are as much as twice as high
#' than 2001 levels between 2002 and 2007, when other factors are controlled
#' for. In years that follow, costs return to around 2001 levels. In 2015, the
#' last year of the sample, costs are nearly two-and-a-half times 2001 average
#' costs, though this effect is weakly estimated and based on only a limited
#' number of observations from this year.
#' 

#' ### Reporting source effects  
#+ echo=F, message=F, warning=F, fig.dim=c(8,8)

# Source FE
map_df(mods, ~ coeftest(., vcovHC(., "HC3")) %>% tidy(conf.int = TRUE), conf.int = TRUE, .id = "model") %>%
  filter(str_detect(term, "source")) %>%
  mutate(
    source = str_remove(term, "project_source"),
    estimate = exp(estimate),
    conf.low = exp(conf.low),
    conf.high = exp(conf.high),
    p.value = I(p.value < 0.05),
    mod.color = case_when(
      p.value == TRUE & model == "mod_full" ~ "sig-pref",
      p.value == TRUE & model != "mod_full" ~ "sig-nopref",
      p.value == FALSE & model == "mod_full" ~ "nosig-pref",
      p.value == FALSE & model != "mod_full" ~ "nosig-nopref"
    ),
    mod.color = ordered(mod.color, levels = c("sig-pref", "sig-nopref", "nosig-pref", "nosig-nopref"))
  ) %>%
  group_by(source) %>%
  select(model, source, estimate, conf.low, conf.high, mod.color) %>%
ggplot() +
  geom_pointrange(
    aes(
      y = reorder(source, estimate),
      x = estimate,
      xmin = conf.low,
      xmax = conf.high,
      group = model,
      color = mod.color
    ),
    position = position_dodge(width = 0.6),
    size = 1
  ) +
  # geom_text(
  #   aes(
  #     x = c(0.5, 1.5),
  #     y = c(0.6, 0.6),
  #     hjust = c(1, 0),
  #     label = c("← Lower costs", "Higher costs →")
  #   ),
  #   size = 3.5
  # ) +
  scale_color_manual(values = c("darkgreen", "darkolivegreen3", "grey60", "grey80")) +
  # scale_x_continuous(breaks = waiver()) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  labs(
    y = NULL, 
    x = "Project average costs", 
    title = "Reporting source fixed effects",
    subtitle = "Project average costs relative to OWRI",
    caption = "Lines indicate 95% confidence interval; Significant coefficients highlighted in color"
  ) +
  # theme_clean() +
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    plot.background = element_rect(color = NA),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    text = element_text(size = 18)
  )

#' BLM and REO have tightly estimated positive associations with costs, around
#' two-and-a-half times costs observed from OWRI. OWRI projects have the lowest
#' average costs, followed by WA RCO costs which are associated with 66% higher
#' costs. Projects reported by Habitat Work Schedule and SRFBD coefficients
#' estimated with less precision, but larger point levels than BLM and REO.
#' 

#' ### Basin effects  
#+ echo=F, message=F, warning=F, fig.dim=c(8,8)

# Basin FE
map_df(mods, ~ coeftest(., vcovHC(., "HC1")) %>% tidy(conf.int = TRUE), conf.int = TRUE, .id = "model") %>%
  filter(str_detect(term, "basin"), term != "cat_basin_slope") %>%
  mutate(
    basin = str_remove(term, "basin"),
    estimate = exp(estimate),
    conf.low = exp(conf.low),
    conf.high = exp(conf.high),
    p.value = I(p.value < 0.05),
    mod.color = case_when(
      p.value == TRUE & model == "mod_full" ~ "sig-pref",
      p.value == TRUE & model != "mod_full" ~ "sig-nopref",
      p.value == FALSE & model == "mod_full" ~ "nosig-pref",
      p.value == FALSE & model != "mod_full" ~ "nosig-nopref"
    ),
    mod.color = ordered(mod.color, levels = c("sig-pref", "sig-nopref", "nosig-pref", "nosig-nopref"))
  ) %>%
  group_by(basin) %>%
  select(model, basin, estimate, conf.low, conf.high, mod.color) %>%
  ggplot() +
  geom_pointrange(
    aes(
      y = reorder(basin, estimate),
      x = estimate,
      xmin = conf.low,
      xmax = conf.high,
      group = model,
      color = mod.color
    ),
    size = 1,
    position = position_dodge(width = 0.6)
  ) +
  scale_color_manual(values = c("darkgreen", "darkolivegreen3", "grey60", "grey80")) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  labs(
    y = NULL, 
    x = "Project average costs", 
    title = "Basin fixed effects",
    subtitle = "Project average costs relative to Southern Oregon Coastal",
    caption = "Lines indicate 95% confidence interval; Significant coefficients highlighted in dark"
  ) +
  # theme_clean() +
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    plot.background = element_rect(color = NA),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    text = element_text(size = 18)
  )

#' After controlling for other factors with the preferred model, projects in the
#' Puget Sound and Lower Columbia basins have the highest average costs,
#' followed by the Washington Coastal and Middle Columbia basins. In general, the
#' pattern seems to be that projects in Oregon basins have lower average costs,
#' with the Willamette and both Oregon Coastal basins coming in with the lowest costs.  
#' 

#' ### Land cover effects  
# Land class
#+ echo=F, message=F, warning=F, fig.dim=c(8,8)

map_df(mods, ~ coeftest(., vcovHC(., "HC1")) %>% tidy(conf.int = TRUE), .id = "model") %>%
  filter(str_detect(term, "nlcd")) %>%
  mutate(
    nlcd_current = str_sub(term, 27),
    estimate = exp(estimate),
    conf.low = exp(conf.low),
    conf.high = exp(conf.high),
    p.value = I(p.value < 0.05),
    mod.color = case_when(
      p.value == TRUE & model == "mod_full" ~ "sig-pref",
      p.value == TRUE & model != "mod_full" ~ "sig-nopref",
      p.value == FALSE & model == "mod_full" ~ "nosig-pref",
      p.value == FALSE & model != "mod_full" ~ "nosig-nopref"
    ),
    mod.color = ordered(mod.color, levels = c("sig-pref", "sig-nopref", "nosig-pref", "nosig-nopref"))
  ) %>%
  group_by(nlcd_current) %>%
  select(model, nlcd_current, estimate, conf.low, conf.high, mod.color) %>%
  ggplot() +
  geom_pointrange(
    aes(
      y = reorder(nlcd_current, estimate),
      x = estimate,
      xmin = conf.low,
      xmax = conf.high,
      group = model,
      color = mod.color
    ),
    size = 1,
    position = position_dodge(width = 0.6)
  ) +
  geom_label(
    aes(
      y = reorder(nlcd_current, estimate),
      label = round(estimate, 2)
    ),
    x = 0.6
  ) +
  scale_color_manual(values = c("darkgreen", "darkolivegreen3", "grey60", "grey80")) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  labs(
    y = NULL, 
    x = "Project average costs", 
    title = "NLCD land cover effects",
    subtitle = "Project average costs relative to Forest",
    caption = str_wrap("Lines indicate 95% confidence interval; Significant coefficients highlighted in dark")
  ) +
  # theme_clean() +
  xlim(0.5, 2.25) +
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    plot.background = element_rect(color = NA),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    text = element_text(size = 18)
  )

#' Cultivated cropland and developed land covers have positive associations with
#' costs, relative to worksites found in areas with forest land cover. Shrubland
#' also has a positive association, though the relationship is not statistically
#' significant. The remaining land covers, wetlands and herbaceous, do not
#' appear to have different costs than the forest land cover baseline.  
#' 

#' ### Road feature effects  
#+ echo=F, message=F, warning=F, fig.dim=c(8,8)

# Road features
map_df(mods, ~ coeftest(., vcovHC(., "HC1")) %>% tidy(conf.int = TRUE), .id = "model") %>%
  filter(str_detect(term, "here")) %>%
  mutate(
    here_var = case_when(
      str_detect(term, "paved") ~ "Road paved (dummy)",
      str_detect(term, "factor") ~ str_replace(term, "factor[(]here_speed[])]", "Road speed class: ")
      ),
    estimate = exp(estimate),
    conf.low = exp(conf.low),
    conf.high = exp(conf.high),
    p.value = I(p.value < 0.05),
    mod.color = case_when(
      p.value == TRUE & model == "mod_full" ~ "sig-pref",
      p.value == TRUE & model != "mod_full" ~ "sig-nopref",
      p.value == FALSE & model == "mod_full" ~ "nosig-pref",
      p.value == FALSE & model != "mod_full" ~ "nosig-nopref"
    ),
    mod.color = ordered(mod.color, levels = c("sig-pref", "sig-nopref", "nosig-pref", "nosig-nopref"))
  ) %>%
  group_by(here_var) %>%
  select(model, here_var, estimate, conf.low, conf.high, mod.color) %>%
  ggplot() +
  geom_pointrange(
    aes(
      y = here_var,
      x = estimate,
      xmin = conf.low,
      xmax = conf.high,
      group = model,
      color = mod.color
    ),
    size = 1,
    position = position_dodge(width = 0.6)
  ) +
  geom_label(
    aes(
      y = here_var,
      label = round(estimate, 2)
    ),
    x = 0.6
  ) +
  scale_color_manual(values = c("darkgreen", "darkolivegreen3", "grey60", "grey80")) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  labs(
    y = NULL, 
    x = "Project average costs", 
    title = "Road feature effects",
    subtitle = str_wrap("Project average costs relative to speed class 8 (smallest roads)"),
    caption = str_wrap("Lines indicate 95% confidence interval; Significant coefficients highlighted in dark")
  ) +
  # theme_clean() +
  xlim(0.5, 3.5) +
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    plot.background = element_rect(color = "white"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    text = element_text(size = 18)
  )

#' There appears to be a positive association between road speed class and
#' project costs, where the largest, most heavily trafficked roads are
#' associated with higher worksite costs. The point estimates for the largest
#' speed class have large standard errors, likely due to lower representation of
#' these roads in the sample.  
#' 

#' ### Scope and scale effects  
#+ echo=F, message=F, warning=F, fig.dim=c(8,4)

# Project scope effects
map_df(mods, ~ coeftest(., vcovHC(., "HC1")) %>% tidy(conf.int = TRUE), conf.int = TRUE, .id = "model") %>%
  filter(str_detect(term, "action")) %>%
  mutate(
    action_var = case_when(
      str_detect(term, "culvrem") ~ "Culvert removal (dummy)",
      str_detect(term, "culvinst") ~ "Culvert installation (dummy)"
    ),
    estimate = exp(estimate),
    conf.low = exp(conf.low),
    conf.high = exp(conf.high),
    p.value = I(p.value < 0.05),
    mod.color = case_when(
      p.value == TRUE & model == "mod_full" ~ "sig-pref",
      p.value == TRUE & model != "mod_full" ~ "sig-nopref",
      p.value == FALSE & model == "mod_full" ~ "nosig-pref",
      p.value == FALSE & model != "mod_full" ~ "nosig-nopref"
    ),
    mod.color = ordered(mod.color, levels = c("sig-pref", "sig-nopref", "nosig-pref", "nosig-nopref"))
  ) %>%
  group_by(action_var) %>%
  select(model, action_var, estimate, conf.low, conf.high, mod.color) %>%
  ggplot() +
  geom_pointrange(
    aes(
      y = action_var,
      x = estimate,
      xmin = conf.low,
      xmax = conf.high,
      group = model,
      color = mod.color
    ),
    size = 1,
    position = position_dodge(width = 0.6)
  ) +
  scale_color_manual(values = c("darkolivegreen3", "grey60", "grey80")) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  labs(
    y = NULL, 
    x = "Project average costs", 
    title = "Project scope effects",
    subtitle = "Project average costs relative to a project with only culvert improvements",
    caption = "Lines indicate 95% confidence interval; Significant coefficients highlighted in dark"
  ) +
  # theme_clean() +
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    plot.background = element_rect(color = NA),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    text = element_text(size = 18)
  )

#' There is some evidence from the models that installations are more expensive
#' than improvements (the baseline) and removals, though this association
#' largely washes out when the full suite of fixed effects is included. This may
#' indicate that distinctions between these categories in the data are loosely
#' defined.
#' 

# ____ Maps ----
#' ## Residual and predicted value maps  
#' 

#' In this section, we map both the residuals and predicted values for each
#' in-sample worksite. Patterns may reveal areas where costs are more expensive,
#' where missing explanatory variables may be effecting costs over a specific
#' region. They also demonstrate how these models could be used to project costs
#' for the full inventories of fish passage barriers documented by state agencies.  
#' 

#+ message=F, warnings=F, echo=F, fig.dim=c(8,8)
sf_us <- getData("GADM", country = "USA", download = TRUE, path = here("output"), level = 1) %>% st_as_sf() %>% filter(NAME_1 %in% c('California', 'Nevada', 'Utah', 'Wyoming', 'Montana', 'Idaho', 'Oregon', 'Washington'))
sf_canada <- getData("GADM", country = "CAN", download = TRUE, path = here("output"), level = 1) %>% st_as_sf() %>% filter(NAME_1 %in% c("British Columbia", "Alberta"))
sf_base <- 
  rbind(sf_us, sf_canada)

if(file.exists(here("output/wdb/WBD_National_GDB.zip"))) {
  sf_basin <-
    nhdplusTools::download_wbd(here("output/wdb")) %>% st_read(layer = "WBDHU6", quiet = TRUE)
} else {
  sf_basin <-
    st_read(here("output/wdb/WBD_National_GDB.gdb"), layer = "WBDHU6", quiet = TRUE)
}

sf_basin <- sf_basin %>% filter(name %in% c("Puget Sound", "Willamette", "John Day", "Washington Coastal", "Southern Oregon Coastal", "Northern Oregon Coastal", "Lower Columbia", "Middle Columbia", "Upper Columbia"))

# sf_roads <-
#   opq(
#     bbox = c(-126, 40, -110, 49.5),
#     timeout = 3000,
#     memsize = 4e+9
#   ) %>%
#   add_osm_feature(
#     key = "highway",
#     value = c(
#       "motorway",
#       "trunk",
#       "primary"
#     )
#   ) %>%
#   osmdata_sf() %>%
#   unname_osmdata_sf()
# 
# sf_rivers <-
#   opq(
#     bbox = c(-126, 40, -110, 49.5),
#     # bbox = c(-122.5, 47.5, -122, 48), # Seattle area check
#     timeout = 3000,
#     memsize = 4e+9
#   ) %>%
#   add_osm_feature(
#     key = "waterway",
#     value = c(
#       # "streams", # Can also add "streams" and other levels for lower level streams, but dramatically increases file size
#       # "canal",
#       # "brook",
#       "river",
#       "riverbank"
#     )
#   ) %>%
#   osmdata_sf()
# 
# sf_lakes <-
#   opq(
#     bbox = c(-126, 40, -110, 49.5),
#     # bbox = c(-122.5, 47.5, -122, 48), # Seattle area check
#     timeout = 3000,
#     memsize = 4e+9
#   ) %>%
#   add_osm_feature(
#     key = "water",
#     value = c(
#       "lake"
#     )
#   ) %>%
#   osmdata_sf()

# Fix broken osm_multipolygons names for plotting (https://github.com/rstudio/leaflet/issues/631#issuecomment-504729274)
# for(i in seq(nrow(sf_lakes$osm_multipolygons))) {
#   names(sf_lakes$osm_multipolygons$geometry[i][[1]]) = NULL
#   names(sf_lakes$osm_multipolygons$geometry[[i]][[1]]) = NULL
# }
# sf_rivers <- unname_osmdata_sf(sf_rivers) # Doesn't seem to work
# names(sf_rivers$osm_multipolygons$geometry) = NULL
# for(i in seq(nrow(sf_rivers$osm_multipolygons))) {
#   names(sf_rivers$osm_multipolygons$geometry[i][[1]]) = NULL
#   names(sf_rivers$osm_multipolygons$geometry[[i]][[1]]) = NULL
# }

# Build base map 
# base_map <-
#   ggplot() +
#   geom_sf(data = sf_base, fill = "antiquewhite1", color = NA) +
#   geom_sf(data = sf_rivers$osm_lines, color = "steelblue1", fill = "steelblue1", size = 0.3) +
#   geom_sf(data = sf_rivers$osm_multilines, color = "steelblue1", fill = "steelblue1", size = 0.3) +
#   geom_sf(data = sf_rivers$osm_polygons, color = "steelblue1", fill = "steelblue1", size = 0.3) +
#   geom_sf(data = sf_rivers$osm_multipolygons, color = "steelblue1", fill = "steelblue1", size = 0.3) +
#   geom_sf(data = sf_lakes$osm_lines, color = "steelblue1", fill = "steelblue1") +
#   geom_sf(data = sf_lakes$osm_polygons %>% filter(water == "lake"), color = "steelblue1", fill = "steelblue1") +
#   geom_sf(data = sf_lakes$osm_multipolygons, color = "steelblue1", fill = "steelblue1") +
#   geom_sf(data = sf_lakes$osm_polygons %>% filter(is.na(water) & !is.na(place)), color = "steelblue1", fill = "antiquewhite1") +
#   geom_sf(data = sf_roads$osm_lines, color = "firebrick1", size = 0.3) +
#   geom_sf(data = sf_base, fill = NA, color = "black") +
#   coord_sf(
#     xlim = c(-126, -111.5), ylim = c(41.5, 49.5),
#     # xlim = c(-122.5, -122), ylim = c(47.5, 48), # Seattle area check
#     expand = FALSE
#   ) +
#   theme_bw() +
#   theme(
#     panel.background = element_rect(fill = "aliceblue", size = 1),
#     axis.text = element_blank(),
#     axis.ticks = element_blank(),
#     legend.position = c(0.99, 0.01),
#     legend.justification = c("right", "bottom"),
#     legend.box.background = element_rect(color = "black", size = 1),
#     legend.title = element_text(size = 10)
#   ) +
#   labs(
#     x = NULL,
#     y = NULL
#   )

base_map_draft <-
  ggplot() +
  geom_sf(data = sf_base, fill = "antiquewhite1", color = "black") +
  geom_sf(data = sf_basin, fill = NA, color = "red") +
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
    x = NULL,
    y = NULL
  )
#+ message=F, warnings=F, fig.dim=c(8,8)

# Residuals map
sf_culv <-
  df_culv %>%
  drop_na(
    cost_per_culvert, n_culverts, dist_mean, n_worksites, action_fishpass_culvinst_prj, action_fishpass_culvrem_prj,
    slope, bankfull_width, here_class, cat_basin_slope, cat_elev_mean, ua_dist, here_paved, nlcd_current_class, hdens_cat, emp_const, emp_agforest, basin, project_year, project_source
  ) %>%
  mutate(
    latlong = str_sub(geometry, 3, nchar(geometry)-1),
    long = as.numeric(stringr::word(latlong, 1, sep = ", ")),
    lat = as.numeric(stringr::word(latlong, 2, sep = ", ")),
    latlong = NULL,
    geometry = NULL
  ) %>%
  st_as_sf(coords = c("long", "lat")) %>% st_set_crs("WGS84") %>%
  bind_cols(residuals = mod_full$residuals)

base_map_draft +
  # base_map +
  geom_sf(
    data = sf_culv,
    aes(
      # x = longitude,
      # y = latitude,
      # geometry = geometry,
      fill = residuals
    ),
    shape = 21,
    color = "black",
    stroke = 0.1,
    size = 2.5
  ) +
  scale_fill_distiller(
    "Model residual",
    # palette = "YlGn",
    # palette = "Spectral",
    palette = "RdBu",
    # labels = dollar,
    direction = 1,
    # trans = "log10",
    na.value = "grey70"
  ) +
  ggtitle(
    "Map of residuals",
    str_wrap("Residuals measured on log scale; \nRed boarders indicate basin (HUC6) boundaries for included basins")
  ) +
  coord_sf(
    xlim = c(-126, -117),
    ylim = c(41.5, 49.5),
    expand = FALSE
  )

#' By looking at the map of residuals, we can examine where the model performs
#' better or worse. Few obvious patterns emerge. There seems to be some
#' clustering of positive residuals in the Portland area, suggesting that costs
#' are underestimated for that region. Otherwise, the residuals appear to be
#' fairly well distributed. We examine the spatial concentration of the
#' residuals and consider spatial lag specifications in a future report.  
#' 

#+ message=F, warnings=F, fig.dim=c(8,8)
# Predicted values
base_map_draft +
  # base_map +
  geom_sf(
    data = 
      sf_culv %>% 
      bind_cols(
        yhat_2015_owri = 
          exp(
            predict(
              mod_full,
              sf_culv %>% 
                mutate(
                  project_year = 2015, 
                  n_worksites = 1,
                  tot_dist = 0,
                  project_source = "OWRI",
                  # basin = "SOUTHERN OREGON COASTAL"
                )
              )
            ),
        
        ),
      # pull(yhat_2015_owri) %>% summary
      # filter(yhat_2015_owri < 200000),
    aes(
      # x = longitude,
      # y = latitude,
      fill = yhat_2015_owri
    ),
    shape = 21,
    color = "black",
    stroke = 0.1,
    size = 2.5
  ) +
  scale_fill_distiller(
    "Predicted cost",
    # palette = "YlGn",
    # palette = "Spectral",
    palette = "RdYlGn",
    labels = dollar_format(accuracy = 1),
    direction = -1,
    trans = "log10",
    na.value = "grey70"
  ) +
  ggtitle(
    "Map of predicted values",
    str_wrap("Year set to 2015, source set to OWRI, basin set to Southern Oregon Coastal, and number of culverts set to one for all worksites; Red boarders indicate basin (HUC6) boundaries for included basins")
  ) +
  coord_sf(
    xlim = c(-126, -117),
    ylim = c(41.5, 49.5),
    expand = FALSE
  )

#' The map of predicted values highlights areas where physical conditions (i.e.
#' road, stream, and terrain features) have the largest impact on costs. By
#' holding year, basin, reporting source, and project scale effects constant, we
#' can identify where the remaining variables in the model predict higher or
#' lower costs. Here, we see higher costs particularly in the Southern Oregon
#' Coastal basin. At the mouth of the Columbia and around the Portland area,
#' there appear to be clusters of lower cost projects. The John Day basin
#' projects in Eastern Oregon also appear to have lower costs than other areas.  

#+ message=F, warnings=F, fig.dim=c(8,8)
# Prediction std.err.
base_map_draft +
  # base_map +
  geom_sf(
    data = 
      sf_culv %>% 
      bind_cols(
        yhat_2015_owri = 
          exp(
            predict(
              mod_full,
              sf_culv %>% 
                mutate(
                  project_year = 2015, 
                  n_worksites = 1,
                  tot_dist = 0,
                  project_source = "OWRI",
                  # basin = "SOUTHERN OREGON COASTAL"
                ),
              
            )
          ),
        se_2015_owri = 
          predict(
            mod_full,
            sf_culv %>% 
              mutate(
                project_year = 2015, 
                n_worksites = 1,
                tot_dist = 0,
                project_source = "OWRI",
                # basin = "SOUTHERN OREGON COASTAL"
              ),
            se.fit = TRUE
          )[["se.fit"]]        
      ) %>%
      filter(se_2015_owri < 0.8),
    aes(
      # x = longitude,
      # y = latitude,
      # fill = yhat_2015_owri
      fill = exp(se_2015_owri)
    ),
    shape = 21,
    color = "black",
    stroke = 0.1,
    size = 2.5
  ) +
  scale_fill_distiller(
    str_wrap("Prediction standard error"),
    # palette = "YlGn",
    # palette = "Spectral",
    # labels = dollar_format(accuracy = 1),
    direction = 1,
    # trans = "log10",
    na.value = "grey70"
  ) +
  ggtitle(
    "Map of prediction standard errors",
    str_wrap("Year set to 2015, source set to OWRI, basin set to Southern Oregon Coastal, and number of culverts set to one for all worksites; Red boarders indicate basin (HUC6) boundaries for included basins")
  ) +
  coord_sf(
    xlim = c(-126, -117),
    ylim = c(41.5, 49.5),
    expand = FALSE
  )

#' This map shows the standard errors on the cost predictions for each worksite
#' in the sample. Darker points indicate increased uncertainty. Uncertainty
#' appears to be the highest in the John Day and Puget Sound regions. This
#' suggests that costs estimates are the least reliable for these areas, and
#' could be improved with more observations from these regions.  
#'   

#' Finally, we present a number of summary statistics by basin for the predictions and metrics presented above.  

#+ echo=F, message=F, warning=F
sf_culv %>% 
  bind_cols(
    yhat_2015_owri = 
      exp(
        predict(
          mod_full,
          sf_culv %>% 
            mutate(
              project_year = 2015,
              # bankfull_width = 30,
              # slope = 0.02,
              # here_speed = "3",
              n_worksites = 1,
              tot_dist = 0,
              project_source = "OWRI",
              # basin = "SOUTHERN OREGON COASTAL"
            )
        )
      ),
    se_2015_owri = 
      predict(
        mod_full,
        sf_culv %>% 
          mutate(
            project_year = 2015, 
            n_worksites = 1,
            tot_dist = 0,
            project_source = "OWRI",
            # basin = "SOUTHERN OREGON COASTAL"
          ),
        se.fit = TRUE
      )[["se.fit"]]        
  ) %>%
  st_drop_geometry() %>%
  rename(Basin = basin) %>%
  group_by(Basin) %>%
  summarize(
    yhat_sort = mean(yhat_2015_owri),
    `Mean Predicted Value` = comma(round(mean(yhat_2015_owri))),
    `Predicted Value Coefficient of Variation` = round(sd(yhat_2015_owri)/mean(yhat_2015_owri), 3),
    `Mean Prediction Standard Error (log scale)` = round(mean(se_2015_owri), 3),
    `Mean Absolute Residual (log scale)` = round(mean(abs(residuals)), 3),
    `N` = n()
    ) %>%
  arrange(yhat_sort) %>%
  select(-yhat_sort) %>%
  kable(
    caption = "Prediction charactaristics across basins",
    escape = FALSE,
    align = "lccccc"
  ) %>%
  kable_styling("hover", fixed_thead = TRUE)

#' The mean predicted value by basin indicates basins where culverts are more or
#' less expensive based on landscape conditions (fixed and project effects
#' fixed). Puget Sound culvert worksites are the most expensive, while Northern
#' Oregon Coastal worksites are the least expensive. The predicted value
#' coefficient of variation shows where costs vary the most *within* a basin on
#' a consistent scale. Costs vary the most within the Upper Columbia basin, and
#' the least in the John Day basin. Finally, the mean prediction standard error
#' shows where model uncertainty is highest. As observed on the map, Middle and
#' Upper Columbia, and Puget Sound basins have the largest prediction standard
#' errors, while the Western Oregon basins (Northern and Southern Oregon
#' Coastal, and the Lower Columbaia) have the smallest, likely because these
#' basins are releatively well-represented in the sample.  
#' 


#+ echo=F, message=F, warning=F, fig.dim=c(8,8)
#' # Benefit - cost visualizations  
#'   

# ____ Benefits and costs plots ----
#' As a simple examination of what kind of decision rule might be in play for
#' determining which culverts were selected, we plot as a benefit proxy total
#' upstream length (in km) versus observed cost per culvert. Under a
#' cost-targeting rule, all worksites would be to the left of a cost threshold,
#' while for a benefit-targeting rule all would be above a benefit threshold. A
#' benefit-cost ratio standard would be above an upward sloping line.  
#' 

#' It should be acknowledged there are severe limitations to this application.
#' Most obviously, without including data on un-attempted projects we are left
#' assuming the total possible project space is "dense" (i.e. in any point in
#' cost-benefit space we don't observe a project, we assume a project there
#' would be possible). This assumption can be addressed by incorporating data
#' from state agency culvert inventories. Second, our benefits measure is rough
#' at best, and alternative measures that account for upstream/downstream
#' barriers as well as habitat conditions and species ranges may more
#' realistically describe the decision space. Finally, in looking across
#' projects from the full study area, we may miss heterogeneity in targeting
#' rules across jurisdictions or regions. We partially address this final point
#' by also examining the distributions of subsets of worksites by region and year.

#+ echo=F, message=F, warning=F, fig.dim=c(8,8)
sf_culv %>% 
  bind_cols(
    yhat_2015_owri = 
      exp(
        predict(
          mod_full,
          sf_culv %>% 
            mutate(
              project_year = 2015, 
              n_worksites = 1, 
              tot_dist = 0,
              project_source = "OWRI",
              basin = "SOUTHERN OREGON COASTAL"
            )
          )
        )
  ) %>%
  mutate(
    # basin = fct_lump_lowfreq(basin, "OTHER"),
    basin = fct_lump_min(basin, 35, other_level = "OTHER")
  ) %>%
  ggplot() + 
  aes(
    # x = cost_per_culvert,
    x = yhat_2015_owri,
    y = upst_dist,
    # y = tot_stream_length,
    color = basin
  ) + 
  geom_point() +
  scale_y_log10("Total upstream length (km)", label = label_comma(1)) +
  scale_x_log10("Cost per culvert (K $USD)", label = label_dollar(1, scale = 0.001)) +
  # scale_color_fermenter("Project year", palette = "Spectral", show.limits = TRUE, guide = guide_colorsteps(barwidth = 10, title.position = "top")) +
  scale_color_brewer(type = "qual", palette = 2) +
  theme(
    aspect.ratio = 1,
    legend.position = "none",
    plot.title.position = "plot",
    text = element_text(size = 18),
    axis.text = element_text(size = 10),
    strip.text = element_text(size = 10)
  ) +
  # facet_wrap("basin") +
  ggtitle("Worksites in cost - benefit space", "Both on a log scale for clarity; Colors indicate basin")
  # coord_fixed(1000/4500)

sf_culv %>% 
  bind_cols(
    yhat_2015_owri = 
      exp(
        predict(
          mod_full,
          sf_culv %>% 
            mutate(
              project_year = 2015, 
              n_worksites = 1, 
              tot_dist = 0,
              project_source = "OWRI",
              basin = "SOUTHERN OREGON COASTAL"
            )
        )
      )
  ) %>%
  mutate(
    # basin = fct_lump_lowfreq(basin, "OTHER"),
    basin = fct_lump_min(basin, 35, other_level = "OTHER")
  ) %>%
  ggplot() + 
  aes(
    # x = cost_per_culvert,
    x = yhat_2015_owri,
    y = upst_dist,
    # y = tot_stream_length,
    color = basin
  ) + 
  geom_point() +
  scale_y_log10("Total upstream length (km)", label = label_comma(1)) +
  scale_x_log10("Cost per culvert (K $USD)", label = label_dollar(1, scale = 0.001)) +
  # scale_color_fermenter("Project year", palette = "Spectral", show.limits = TRUE, guide = guide_colorsteps(barwidth = 10, title.position = "top")) +
  scale_color_brewer(type = "qual", palette = 2) +
  theme(
    aspect.ratio = 1,
    legend.position = "none",
    plot.title.position = "plot",
    text = element_text(size = 18),
    axis.text = element_text(size = 10),
    strip.text = element_text(size = 10)
  ) +
  facet_wrap("basin") +
  ggtitle("Worksites in cost - benefit space, by basin", "Both on a log scale for clarity; Colors indicate basin") 

sf_culv %>% 
  bind_cols(
    yhat_2015_owri = 
      exp(
        predict(
          mod_full,
          sf_culv %>% 
            mutate(
              project_year = 2015, 
              n_worksites = 1, 
              tot_dist = 0,
              project_source = "OWRI",
              basin = "SOUTHERN OREGON COASTAL"
            )
        )
      )
  ) %>%
  mutate(
    # basin = fct_lump_lowfreq(basin, "OTHER"),
    basin = fct_lump_min(basin, 35, other_level = "OTHER")
  ) %>%
  ggplot() + 
  aes(
    # x = cost_per_culvert,
    x = yhat_2015_owri,
    y = upst_dist,
    # y = tot_stream_length,
    color = basin
  ) + 
  geom_point() +
  scale_y_log10("Total upstream length (km)", label = label_comma(1)) +
  scale_x_log10("Cost per culvert (K $USD)", label = label_dollar(1, scale = 0.001)) +
  # scale_color_fermenter("Project year", palette = "Spectral", show.limits = TRUE, guide = guide_colorsteps(barwidth = 10, title.position = "top")) +
  scale_color_brewer(type = "qual", palette = 2) +
  theme(
    aspect.ratio = 1,
    legend.position = "none",
    plot.title.position = "plot",
    text = element_text(size = 18),
    axis.text = element_text(size = 10),
    strip.text = element_text(size = 10)
  ) +
  facet_wrap("project_year", nrow = 3) +
  ggtitle("Worksites in cost - benefit space, by year", "Both on a log scale for clarity; Colors indicate basin") 
# coord_fixed(0.75)

#' No strong evidence that projects are selected on cost or benefit-cost ratio basis.
#' It does somewhat appear that the observed projects follow a benefit targeting
#' pattern with a fairly low benefit cut-off. For Lower Columbia, Upper
#' Columbia, and Washington Coastal, and Puget Sound basins, there appears to be a slight upward
#' tilt in the higher-cost region, indicating that benefit-cost targeting may be
#' more frequent for higher-cost projects.  
#' 

#' Adding culverts where no project is observed with costs predicted via the
#' above models could reveal where observed projects exist in the space
#' relative to the universe of potential projects, which should provide more
#' clarity to the above analysis.  

#+
#' # Conclusions  
#' ## Key findings  
#'   

# Key findings ----

#' 1. Stream features slope and bankfull width increase average costs, especially when they are both high.  

#' 2. Paved roads are more expensive to improve, while other road variables have little discernible effect.  

#' 3. Some evidence of economies of scale, in that worksites associated with
#' projects associated with more worksites tend to have lower average costs.
#' This effect is countered by a positive association between costs and total
#' distance between worksites.  

#' 4. John Day basin worksites are the lowest variance in costs between
#' worksites, while Northern Oregon Coastal worksites have the lowest costs
#' overall. Costs are highest in the Puget Sound and Washington Coastal basins.
#' Cost variance is also particularly high in the Upper Columbia and Willamette
#' basins.

#+
#' ## Next steps  
#'   
# Next steps ----
#' 1. Forecast costs/benefits for culvert inventories from Oregon and Washington  
#' 2. Compare results of OLS estimates to estimates of models with spatial lags, and machine learning methods (boosted regression trees)
#' 3. Analyze how outcomes (cost/benefit targeting patterns, cost levels/variation, model uncertainty) across culvert ownership, jurisdictions  
#' 


# Render output
# rmarkdown::render(here::here("R/C.culvertsspatial/07.spatialcostmodels.R"), output_file = here::here("output/culvertsmodels_report_2020oct06.html"))


