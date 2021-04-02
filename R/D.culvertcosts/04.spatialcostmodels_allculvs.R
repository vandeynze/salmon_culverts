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
      "data/Culverts spatial overlays v 06Aug2020.xlsx"
    ), 
    sheet = 3
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
  read_csv(here("output", "culverts_full_modelling.csv")) %>%
  mutate(
    # project_year = ordered(project_year),
    project_source = relevel(factor(project_source), ref = "OWRI"),
    basin = relevel(factor(basin), ref = "SOUTHERN OREGON COASTAL"),
    fips = factor(fips),
    state_fips = factor(state_fips),
    here_class = as.character(here_class),
    here_speed = forcats::fct_inseq(factor(here_speed), ordered = TRUE),
    # tot_dist = I(n_worksites * dist_mean)
  ) %>%
  left_join(
    key_nlcd, 
    by = c("nlcd_current" = "value")
  ) %>%
  filter(
    !(nlcd_current_class %in% c("Barren", "Water")),
    # tot_dist < 10000
  ) %>%
  mutate(
    nlcd_current_class = forcats::fct_infreq(factor(nlcd_current_class), ordered = TRUE)
  ) %>%
  rowwise() %>%
  mutate(
    across(
      starts_with("emp_"),
      ~ . / sum(c_across(starts_with("emp")), na.rm = TRUE),
      .names = "{col}_prop"
    )
  )

# Load full pnshp for other action counts, n_worksites, etc.
df_pnshp <-
  read_csv(
    here("output/PNSHP_prj_working.csv")
  )
df_wrk <-
  read_csv(
    here("output/PNSHP_full_working.csv")
  ) %>%
  distinct(worksite_id, latitude, longitude)

df_culv <-
  df_culv %>%
  left_join(
    df_pnshp %>% select(
      project_id,
      project_year,
      n_worksites,
      starts_with("action_")
    )
  ) %>%
  left_join(df_wrk) %>%
  # Summarize major non-culvert actions
  rowwise() %>%
  mutate(
    action_roadwork = sum(action_sedireduce_roaddrain_count, action_sedireduce_roadobli_count),
    action_habitat = sum(action_riparian_plant_count, action_instream_wooddebris_count, action_instream_logjam_count),
    action_culvrem = action_fishpass_culvrem_count,
    action_culvinst = action_fishpass_culvimp_count
  )

# Add total distance between worksites
df_culv <-
  df_culv %>%
  # slice(1:10) %>%
  mutate(
    tot_dist = map_dbl(
      project_id,
      function(x){
        distmat <- geosphere::distm(
          df_culv %>% filter(project_id == x) %>% select(longitude, latitude)
        )
        sum(distmat[lower.tri(distmat)])
      }
    )
  )
  

names(df_culv)

# Filter extreme outliers
df_culv_back <- df_culv
df_culv <- df_culv %>% filter(n_worksites <= 20, tot_dist <= 350000) %>%
  ungroup() %>%
  mutate(
    here_speed2 = forcats::fct_collapse(factor(here_speed), "< 5 mph" = "8", "5-30 mph" = c("7","6"), other_level = "30+ mph"),
    publand = case_when(publand ~ "Y", TRUE ~ "N"),
    pure_culv = case_when(pure_culv ~ "N", TRUE ~ "Y")
  )

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
  mutate(`Distance between worksites (m)` = tot_dist) %>%
  select(
    `Total project costs ($USD2019)` = adj_cost,
    `Number of worksites (count)` = n_worksites,
    `Distance between worksites (m)`,
    `Stream slope (%)` = slope,
    `Bankfull width (m)` = bankfull_width,
    `Paved road` = here_paved,
    `Road speed class` = here_speed,
    `Terrain slope (deg)` = cat_basin_slope,
    `Land cover class` = nlcd_current_class,
    `Housing density (units per sq. km)` = hdens_cat,
    `Construction employment (proportion total jobs)` = emp_const,
    `Ag/forestry employment (proportion total jobs)` = emp_agforest,
    `Distance to urban area (m)` = ua_dist,
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

#' **Total costs ($USD, 2019)** is our primary dependent variable. This variable
#' is adjusted for inflation. It may include different aspects of the project
#' depending on the reporting. We consider this statistical error and account
#' somewhat for this effect by including reporting source fixed effects.
#'

#' ## Stream hydrological features  
#' 
#' - **Stream slope (% grade)**: slope of stream at road crossing can require more
#' expensive crossing design; identified via COMID matching with NHDPlus
#' attributes.  
#' - **Bankfull width (m)**: bankfull width is the preferred measure of stream width
#' at road crossing, accounting for potential width during high-water events;
#' identified via COMID matching with NHDPlus attributes.  

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
key_here <- read_xlsx(here("data/Culverts spatial overlays v 20Aug2020.xlsx"), sheet = 3) %>%
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
#' grid cell the worksite falls in~~ slope recorded for the *catchment* the stream is associated with, as opposed to the stream slope.
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
        "data/Culverts spatial overlays v 06Aug2020.xlsx"
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
#' 
#+ echo=F
#' ## Scale and scope controls  
#'
#' - **Number of worksites associated with project (count):** addressing multiple
#' culverts under the same project may provide scale benefits, but might also
#' increase complexity; measured via PNSHP database.  
#' - **Distance between project worksites (m)**: more dispersed worksites
#' under a single project may increase project costs due to increased
#' transportation costs (and time); measured as the total euclidean distance
#' between worksites for multiple worksite projects. This variable is interacted 
#' with the number of worksites to allow flexible corrdination/scale effects.  
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
  mutate(`Distance between sites` = tot_dist) %>%
  select(
    # employment vars
    `Employment, construction` = emp_const_prop, `Employment, ag/forestry` = emp_agforest_prop,
    # pop vars
    `Housing density` = hdens_cat, `Distance to urban area` = ua_dist,
    # popdens_cat,
    # stream vars
    `Stream slope` = slope, `Bankfull width` = bankfull_width,
    `Terrain slope` = cat_basin_slope, `Elevation` = cat_elev_mean,
    `Distance between sites`, `Number of worksites` = n_worksites,
    `Total costs` = adj_cost
  ) %>%
  drop_na()

mat_corr <-
  df_corr %>%
  cor() %>%
  as.matrix()


mat_vifs <- imcdiag(lm(log(`Total costs`) ~ ., df_corr), method = "VIF")[[1]] %>% as.matrix
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
#' No variables have particularly large VIFs, suggesting little potential for
#' error-inflating multicollinearity. (A large VIF indicates that the variable is
#' strongly correlated with the other variables in the model, leading to
#' inflated standard errors and limiting the model's usefulness for prediction
#' or inference.)  
#' 

#+ estimate, include=F
# Estimate models ----
# Full model
(mod_full <- 
   lm(
     log(adj_cost) ~
       # log(I(adj_cost / n_worksites)) ~
       # Scale/scope of project controls: number of culverts, distance between worksites, type of culvert work
       n_worksites * tot_dist + I(n_worksites ^ 2) +  # factor(I(n_worksites == 1)) +
       # action_fishpass_culvrem_count + action_fishpass_culvimp_count + action_fishpass_culvinst_count +
       factor(I(action_habitat > 0)) + factor(I(action_roadwork > 0)) + pure_culv +
       # Stream features at worksite: slope, bankfull width
       slope * bankfull_width + 
       # Road features at worksite: paved, road class
       factor(here_paved) + factor(here_speed2, ordered = FALSE) +
       # Physical features of worksite: terrain slope, land cover
       # slope_deg + factor(nlcd_current_class) +
       cat_basin_slope + cat_elev_mean + factor(nlcd_current_class, ordered = FALSE) +
       # Population features: housing density, jobs in construction, jobs in ag/forestry
       hdens_cat + emp_const_prop + emp_agforest_prop + ua_dist + publand +
       # Fixed effects
       basin + factor(project_year) + project_source,
     df_culv %>% filter(n_worksites < 10, tot_dist < 100000)
     # )) %>% coeftest(vcov. = vcovHC(., "HC3"))
   )) %>% coeftest(vcov. = vcovCL(., ~ project_id))

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
  map_df(mods, ~coeftest(., vcov. = vcovCL(., ~ project_source)) %>% tidy, .id = "model") %>%
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
      term == "factor(I(action_habitat > 0))TRUE" ~ "Habitat actions (dummy)",
      term == "factor(I(action_roadwork > 0))TRUE" ~ "Road drainage actions (dummy)",
      term == "n_worksites" ~ "Number of worksites",
      term == "I(n_worksites^2)" ~ "Number of worksites (squared)",
      term == "tot_dist" ~ "Distance between worksites",
      # term == "log(dist_mean + 1)" ~ "Mean distance between worksites, log",
      # term == "factor(I(n_worksites == 1))TRUE" ~ "Single worksite (dummy)",
      term == "slope" ~ "Stream slope",
      term == "bankfull_width" ~ "Bankfull width",
      str_detect(term, "here_paved") ~ "Road paved (dummy)",
      term == "cat_basin_slope" ~ "Terrain slope",
      term == "cat_elev_mean" ~ "Elevation",
      term == "hdens_cat" ~ "Housing density",
      term == "emp_const_prop" ~ "Construction employment",
      term == "emp_agforest_prop" ~ "Ag/forestry employment",
      term == "ua_dist" ~ "Distance to urban area",
      term == "slope:bankfull_width" ~ "Stream slope X bankfull width",
      term == "n_worksites:tot_dist" ~ "Number of worksites X distance",
      term == "pure_culvY" ~ "Other actions (dummy)",
      term == "publandY" ~ "Public land (dummy)",
      TRUE ~ term
    ),
    term = str_replace(term, "project_source", "Project source: "),
    term = str_replace(term, "basin", "Basin: "),
    term = str_replace(term, "factor[(]project_year[)]", "Year: "),
    term = str_replace(term, "factor[(]nlcd_current_class, ordered = FLASE[)]", "Land cover: "),
    term = str_replace(term, "factor[(]here_speed2, ordered = FALSE[)]", "Road speed class: ")
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
    starts_with("Road speed"),
    "Road paved (dummy)",
    # Land features
    "Terrain slope", "Elevation",
    starts_with("Land cover"),
    # Pop features
    "Housing density",
    ends_with("employment"),
    "Distance to urban area",
    "Public land (dummy)",
    # Scale/scope features
    "Number of worksites",
    "Number of worksites (squared)",
    starts_with("Distance between "),
    "Number of worksites X distance",
    "Habitat actions (dummy)",
    "Road drainage actions (dummy)",
    "Other actions (dummy)",
    # starts_with("Culvert "),
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
  add_footnote("* p < 0.1, ** p < 0.05, *** p < 0.01; Heteroskedasticity-consistent standard errors in parentheses (HC3)", notation = "none") %>%
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
#' year, based on the relative R-squareds of models where each is removed and
#' where each is included on its own.
#' 

#' We can also compare AIC and BIC; the model with only fixed effects for
#' reporting source is preferred on the basis of BIC, which includes a stronger
#' penalty for the number of estimated parameters, while the full model is
#' preferred on the basis of AIC. We use the full model in what proceeds as the
#' preferred model.  
#' 

#'
#' When the model is fit only on culverts in the "core" basins or sources,
#' adjusted R-squared falls dramatically. This may suggest that information from
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
    breaks = c(0, seq(10000, 20000, 2500), Inf)
    # breaks = log(c(0, seq(10000, 25000, 2500), Inf))
  ) +
  geom_contour(
    breaks = 
      ggpredict(
        mod_full,
        "slope [mean]"
      )["predicted"],
    color = "black",
    linetype = "solid"
  ) +
  geom_contour(
    breaks = 
      ggpredict(
        mod_full,
        "slope [mean]",
        # vcov.fun = "vcovHC",
        # vcov.type = "HC1",
      )[c("conf.low", "conf.high")],
    color = "black",
    linetype = "dashed"
  ) +
  scale_fill_brewer(
    name = wrapper("Predicted cost per culvert"),
    direction = -1,
    palette = "Spectral",
    labels = c(
      "$0 to $10,000",
      "$10,001 to $12,500",
      "$12,501 to $15,000",
      "$15,001 to $17,500",
      "$17,501 to $20,000",
      "Over $20,000"
    )
  ) +
  # facet_wrap("model", nrow = round(sqrt(length(mods)))) +
  geom_point(
    aes(
      x = slope,
      y = bankfull_width,
      z = NULL
    ),
    data = df_culv,
    color = "grey30",
    alpha = 0.3,
    size = 0.2
  ) +
  theme(
    # legend.position = "bottom" 
  ) +
  coord_fixed(0.3/50) +
  labs(
    title = wrapper("Predicted average costs by bankfull width (m) and slope (% grade)"),
    subtitle = 
      wrapper(
        "Predictions based on full model with other continuous variables at means and categorical variables at their modes;
        points represent underlying observations; line indicates cost contour at mean slope and banfull width with 95% c.i."
      ),
    x = "Slope",
    y = "Bankfull width"
  )

df_culv %>%
  ggplot() +
  aes(
    x = slope,
    y = bankfull_width,
    color = adj_cost / n_worksites
  ) +
  geom_point() +
  scale_color_fermenter(
    name = "Cost per worksite ($USD)",
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
    palette = "Spectral",
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

map_df(mods, predict_cost_interaction, var1 = "n_worksites", var2 = "tot_dist", lims1 = c(0, 10), lims2 = c(0, 100000), by1 = 0.1, by2 = 100, .id = "model") %>%
  # filter(model == "mod_full") %>%
  ggplot() +
  aes(
    x = n_worksites,
    y = tot_dist/1000,
    z = predicted/n_worksites
  ) +
  geom_contour_filled(
    breaks =
      c(
        seq(0, 20000, 2500),
        Inf
      )
  ) +
  scale_fill_brewer(
    name = wrapper("Predicted cost per worksite"),
    direction = -1,
    palette = "Spectral",
    labels =
      c(
        "$0 to $2,500",
        "$2,501 to $5,000",
        "$5,001 to $7,500",
        "$7,501 to $10,000",
        "$10,001 to $12,500",
        "$12,501 to $15,000",
        "$15,001 to $17,500",
        "$17,501 to $20,000",
        "Over $20,000"
      )
  ) +
  # facet_wrap("model", nrow = round(sqrt(length(mods)))) +
  geom_violin(
    aes(
      group = n_worksites,
      y = tot_dist/1000,
      z = NULL
    ),
    data = df_culv %>% filter(n_worksites <= 10, tot_dist <= 100000),
    color = "grey30",
    alpha = 0.3
  ) +
  geom_point(
    aes(x = x, y = y, shape = shape), size = 2, stroke = 2,
    inherit.aes = FALSE,
    data = tibble(x = c(1, 2), y = c(0, 5), shape = as.character(c(1, 4)))
  ) +
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
        # vcov.fun = "vcovHC",
        # vcov.type = "HC3",
      )[c("conf.low", "conf.high")],
    color = "black",
    linetype = "dashed"
  ) +
  theme(
    # legend.position = "bottom" 
  ) +
  # coord_fixed(6/10) +
  scale_x_continuous(n.breaks = 6, limits = c(0,10)) +
  scale_shape_manual(values = c(4, 8), guide = NULL) +
  # scale_y_log10() +
  labs(
    title = wrapper("Predicted average costs by number of worksites and total distance between worksites (km)"),
    subtitle = wrapper("Predictions based on full model with other continuous variables at means and categorical variables at their modes; violin plots represent underlying observations"),
    x = "Number of worksites",
    y = "Total distance between worksites (km)"
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

#+ echo=F, message=F, warning=F, fig.dim=c(8,8)
# ____ Marginal effects plots ----
# Custom wrapper of margins::margins for use with map_df
margins_custom <-
  function(mod, terms) { # Takes a model and a character vector of terms
    margins(
      mod, 
      variables = terms, 
      change = "sd", 
      # vcov = vcovHC(mod, "HC1")
      # vcov = vcovHC(mod, "HC3")
      vcov = vcovCL(mod, ~ project_id)
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
      "emp_const_prop",
      "emp_agforest_prop",
      "ua_dist"
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
      factor == "emp_const_prop" ~ "Construction employment",
      factor == "emp_agforest_prop" ~ "Ag/forestry employment",
      factor == "ua_dist" ~ "Distance to urban area"
    ),
    group = case_when(
      factor %in% c("Bankfull width", "Stream slope") ~ "Stream features",
      factor %in% c("Terrain slope", "Elevation") ~ "Terrain features",
      factor %in% c("Distance between worksites", "Number of worksites") ~ "Project scale",
      factor %in% c("Construction employment", "Ag/forestry employment", "Housing density", "Distance to urban area") ~ "Population features",
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
  # rename(estimate = ame) %>%
  dplyr::select(model, factor, estimate, conf.low, conf.high, p.value, group, sd) %>%
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
  xlim(-0.25, 4) +
  labs(
    y = NULL,
    x = "Project costs", 
    title = "Marginal effects for continuous variables",
    subtitle = "Project costs relative to a single standard deviation shift",
    caption = "Lines indicate 95% confidence interval; Significant coefficients highlighted in dark"
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
#' the preferred model. However, in some of the alternative models, housing
#' density and distance to urban area has a slight positive association with
#' costs. The housing density effect might be evidence of increased access costs
#' due to negotiating with multiple landowners, while the distance association
#' may be evidence of increased costs due to lack of access to materials or
#' labor. We are in the process of gathering improved proxies for each of these
#' potential mechanisms.  
#' 

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
map_df(mods, ~ coeftest(., vcovCL(., ~ project_id)) %>% tidy(conf.int = TRUE), conf.int = TRUE, .id = "model") %>%
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
  dplyr::select(model, year, estimate, conf.low, conf.high, mod.color) %>%
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
    y = "Project costs", 
    title = "Year fixed effects",
    subtitle = "Project costs relative to 2001 levels",
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
map_df(mods, ~ coeftest(., vcovCL(., ~ project_id)) %>% tidy(conf.int = TRUE), conf.int = TRUE, .id = "model") %>%
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
  dplyr::select(model, source, estimate, conf.low, conf.high, mod.color) %>%
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
    x = "Project costs", 
    title = "Reporting source fixed effects",
    subtitle = "Project costs relative to OWRI",
    caption = "Lines indicate 95% confidence interval; 
    Significant coefficients highlighted in color"
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
map_df(mods, ~ coeftest(., vcovCL(., ~ project_id)) %>% tidy(conf.int = TRUE), conf.int = TRUE, .id = "model") %>%
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
  dplyr::select(model, basin, estimate, conf.low, conf.high, mod.color) %>%
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
    x = "Project costs", 
    title = "Basin fixed effects",
    subtitle = "Project costs relative to Southern Oregon Coastal",
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

map_df(mods, ~ coeftest(., vcovCL(., ~ project_id)) %>% tidy(conf.int = TRUE), .id = "model") %>%
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
  dplyr::select(model, nlcd_current, estimate, conf.low, conf.high, mod.color) %>%
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
    x = 0.5
  ) +
  scale_color_manual(values = c("darkgreen", "darkolivegreen3", "grey60", "grey80")) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  labs(
    y = NULL, 
    x = "Project costs", 
    title = "NLCD land cover effects",
    subtitle = "Project costs relative to Forest",
    caption = str_wrap("Lines indicate 95% confidence interval; Significant coefficients highlighted in dark")
  ) +
  # theme_clean() +
  xlim(0.5, 2.5) +
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
map_df(mods, ~ coeftest(., vcovCL(., ~ project_id)) %>% tidy(conf.int = TRUE), .id = "model") %>%
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
  dplyr::select(model, here_var, estimate, conf.low, conf.high, mod.color) %>%
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
    x = "Project costs", 
    title = "Road feature effects",
    subtitle = str_wrap("Project costs relative to speed class 8 (smallest roads)"),
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
      str_detect(term, "roadwork") ~ "Other roadwork(dummy)",
      str_detect(term, "habitat") ~ "Habitat work (dummy)"
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
  dplyr::select(model, action_var, estimate, conf.low, conf.high, mod.color) %>%
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

sf_basin <- sf_basin %>% filter(name %in% c("Puget Sound", "Willamette", "John Day", "Washington Coastal", "Southern Oregon Coastal", "Northern Oregon Coastal", "Lower Columbia", "Middle Columbia", "Upper Columbia", "Lower Snake", "Deschutes"))

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
    adj_cost, n_worksites, tot_dist, action_habitat, action_roadwork,
    slope, bankfull_width, here_class, cat_basin_slope, cat_elev_mean, ua_dist, here_paved, nlcd_current_class, hdens_cat, emp_const_prop, emp_agforest_prop, basin, project_year, project_source
  ) %>%
  mutate(
    latlong = str_sub(geometry, 3, nchar(geometry)-1),
    long = as.numeric(stringr::word(latlong, 1, sep = ", ")),
    lat = as.numeric(stringr::word(latlong, 2, sep = ", ")),
    latlong = NULL,
    geometry = NULL
  ) %>%
  filter(n_worksites < 10, tot_dist < 100000) %>%
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
#' better or worse. Few obvsious patterns emerge. There seems to be some
#' clustering of positive residuals in the Portland area, suggesting that costs
#' are underestimated for that region. Otherwise, the residuals appear to be
#' fairly well distributed.  
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
                  basin = "SOUTHERN OREGON COASTAL")
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
                  basin = "SOUTHERN OREGON COASTAL"),
              
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
                basin = "SOUTHERN OREGON COASTAL"),
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
              n_worksites = 1,
              tot_dist = 0,
              project_source = "OWRI",
              basin = "SOUTHERN OREGON COASTAL")
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
            basin = "SOUTHERN OREGON COASTAL"),
        se.fit = TRUE
      )[["se.fit"]]        
  ) %>%
  st_drop_geometry() %>%
  rename(Basin = basin) %>%
  group_by(Basin) %>%
  summarize(
    `Mean Predicted Value` = comma(round(mean(yhat_2015_owri))),
    `Predicted Value Coefficient of Variation` = round(sd(yhat_2015_owri)/mean(yhat_2015_owri), 3),
    `Mean Prediction Standard Error` = round(mean(se_2015_owri), 3)
    ) %>%
  arrange(`Mean Predicted Value`) %>%
  kable(
    caption = "Prediction charactaristics across basins",
    escape = FALSE,
    align = "lccc"
  ) %>%
  kable_styling("hover", fixed_thead = TRUE)

#' The mean predicted value by basin indicates basins where culverts are more or
#' less expensive. Southern Oregon Coastal culvert worksites are the most
#' expensive, while John Day basin worksites are the least expensive. The
#' predicted value coefficient of variation shows where costs vary the most
#' *within* a basin on a consistent scale. Costs vary the most within the Upper
#' Columbia basin, and the least in the John Day basin. Finally, the mean
#' prediction standard error shows where model uncertainty is highest. As
#' observed on the map, John Day and Puget Sound basins have the largest
#' prediction standard errors, while the Western Oregon basins (Northern and
#' Southern Oregon Coastal, and the Lower Columbaia) have the smallest.  
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
    legend.position = "none",
    plot.title.position = "plot",
    text = element_text(size = 18),
    axis.text = element_text(size = 10),
    strip.text = element_text(size = 10)
  ) +
  # facet_wrap("basin") +
  ggtitle("Worksites in cost - benefit space", "Both on a log scale for clarity; Colors indicate basin") +
  coord_fixed(1000/4500)

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
    legend.position = "none",
    plot.title.position = "plot",
    text = element_text(size = 18),
    axis.text = element_text(size = 10),
    strip.text = element_text(size = 10)
  ) +
  facet_wrap("basin") +
  ggtitle("Worksites in cost - benefit space, by basin", "Both on a log scale for clarity; Colors indicate basin") +
  coord_fixed(1000/4500)

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
    legend.position = "none",
    plot.title.position = "plot",
    text = element_text(size = 18),
    axis.text = element_text(size = 10),
    strip.text = element_text(size = 10)
  ) +
  facet_wrap("project_year", nrow = 3) +
  ggtitle("Worksites in cost - benefit space, by year", "Both on a log scale for clarity; Colors indicate basin") +
  coord_fixed(1000/4500)
# coord_fixed(0.75)

#' No evidence that projects are selected on cost or benefit-cost ratio basis.
#' It does somewhat appear that the observed projects follow a benefit targeting
#' pattern with a fairly low benefit cut-off. For Lower Columbia, Upper
#' Columbia, and Washington Coastal basins, there appears to be a slight upward
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
#' This effect is countered by a postive association between costs and total
#' distance between worksites.  

#' 4. John Day basin worksites are the lowest cost and lowest variance in costs
#' between worksites. However, the model error is also highest in this basin.
#' Costs are highest in the Southern Oregon Coastal and Washington Coastal
#' basins. Cost variance is also particularly high in the Upper Columbia.  

#+
#' ## Next steps  
#'   
# Next steps ----
#' 1. More variables: additional population proximity measures, including distance to materials suppliers (gravel, pavement, metal, machinary, etc.) and parcel density data  
#' 2. Improved benefit estimates: total upstream distance by species and habitat use, accounting for upstream/downstream blockages, habitat potential measures, etc.  
#' 3. Forecast costs/benefits for culvert inventories from Oregon and Washington  
#' 4. Integrate Lorenz curve and Gini coefficient analysis from Babcock et al. (1997) for more consistent comparison between cost and benefit concentration among worksites  
#' 5. Highlight distribution of high-benefit/low-cost projects relative to traditional tribal lands, different recreational fishing areas, etc.  
#' 6. Analyze how outcomes (cost/benefit targeting efficiency relative to ratio targeting, cost levels/variation, model uncertainty) across culvert ownership, jursdictions  
#' 


# Render output
# rmarkdown::render(here::here("R/C.culvertsspatial/07.spatialcostmodels.R"), output_file = here::here("output/culvertsmodels_report_2020oct06.html"))


