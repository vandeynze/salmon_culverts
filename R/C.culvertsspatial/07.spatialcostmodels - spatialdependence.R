#' ---
#' title: "Spatial Lag Models with Culvert Cost Data"
#' author: "R. Fonner, B. Van Deynze"
#' date: '`r format(Sys.Date(), "%B %d, %Y")`'
#' output:
#'    html_document:
#'       number_sections: true
#'       toc: true
#'       toc_float:
#'          collapsed: true
#' ---
#' 

#+ echo=F
# Introduction ----

#' Because of the spatial nature of the culvert worksite data, we suspect there
#' may be different forms of spatial associations within the data. These
#' associations may undermine the assumptions of our base OLS models. We use
#' three tools to measure spatial heterogeneity and dependence in the cost data.  
#'   

#' 1. Variograms: these plots display how the "semivariance" (half the variance
#' of the differences between all points within a given distance, a measure of
#' variance within a buffer) changes as a function of distance between
#' observations.  

#' 2. Moran's I: this is a test of spatial autocorrelation, where a rejection of
#' the null hypothesis indicates a violation of the OLS assumption of
#' independent observations.

#' 3. Anselin's Lagrange Multiplier Tests: these tests serve as an assessment of
#' model misspecification due to spatial associations.  
#' 

#' We use these tools on the base model found in [this
#' report](../spatial_cost_models.html), with one key change. After initial
#' testing, we found including spatial fixed effects like `basin` along side
#' band-based spatial weighting schemes (i.e. those that enforce a strict
#' maximum distance beyond which weights are zero) can result in a singular (or
#' at least computationally singular) covaraite matrix. We also found that
#' including `distance to nearest urban area` or other density-based variables
#' based on distance to fixed points (i.e., the supplier density variables)
#' result in similar singularities, likely because they closely track the same
#' processes which spatial weighting structures are intended to account for.
#' **Therefore, the `basin` fixed effect, `distance to nearest urban
#' area`, and all four of the supplier densitiy variables are removed in the models
#' presented and examined below.**
#' 

#+ include=F
#
# Investigate spatial associations
# in the culvert average cost data
# Robby Fonner
#

# Prepare environment and data ----
rm(list = ls())

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
library(geoR)
library(fields)
library(sphet)
library(spdep)
library(spatialreg)
library(sandwich)
library(lmtest)


# Load NLCD key
key_nlcd <-
  read_xlsx(
    here(
      "Data/Culverts spatial overlays v 06Aug2020.xlsx"
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
  read_csv(here("output", "culverts_pure_modelling.csv")) %>%
  mutate(
    # project_year = ordered(project_year),
    project_source = relevel(factor(project_source), ref = "OWRI"),
    basin = relevel(factor(basin), ref = "SOUTHERN OREGON COASTAL"),
    fips = factor(fips),
    state_fips = factor(state_fips),
    here_class = as.character(here_class),
    here_speed = relevel(factor(here_speed), ref = 6),
    tot_dist = I(n_worksites * dist_mean),
    latlong = str_sub(geometry, 3, nchar(geometry)-1),
    long = as.numeric( word(latlong, 1, sep = ", ")),
    lat = as.numeric( word(latlong, 2, sep = ", "))
  ) %>%
  left_join(
    key_nlcd, 
    by = c("nlcd_current" = "value")
  ) %>%
  filter(
    !(nlcd_current_class %in% c("Barren", "Water")),
    tot_dist < 10000
  ) %>%
  select(-latlong) %>%
  mutate(
    nlcd_current_class = relevel(factor(nlcd_current_class), ref = "Forest")
  ) %>%
  drop_na(emp_const, emp_agforest, slope)


# Estimate original model ----
  
dep <- "log(cost_per_culvert)"
indep <-
  c(
    "n_worksites * tot_dist",
    "action_fishpass_culvrem_prj", "action_fishpass_culvinst_prj",
    # Stream features at work site: slope, bankfull width
    "slope * bankfull_width",
    # Road features at work site: paved, road class
    "factor(here_paved)", "factor(here_speed)",
    # Physical features of work site: ground slope, land cover
    "cat_basin_slope",
    "cat_elev_mean",
    "factor(nlcd_current_class)",
    # Population features: housing density, jobs in construction, jobs in ag/forestry
    "hdens_cat", "emp_const", "emp_agforest",  "ua_dist",
    # Supplier density
    "const_totp",
    "brick_totp",
    "metal_totp",
    "sales_coun",
    # Land ownership
    "pv_1km_buff",
    "pvi_1km_buff",
    "pvn_1km_buff",
    # Fixed effects
    "basin",
    "factor(project_year)",
    "project_source"
  )
f <- as.formula(
  paste(dep,
        paste(indep, collapse = "+"),
        sep = "~")
  )

mod_full <- 
  lm(f, df_culv)

coeftest(mod_full, .vcov = vcovCL(mod_full, ~ project_id, type = "HC3"))
summary(mod_full)


# Variograms ----
#' # Variograms  
#'   

#' A variogram is used to identify the distance at which the variance within a
#' buffer is roughly equivalent to the variance over the entire space. This
#' distance will then be used to define which points are considered "neighbors"
#' and which are not.
#'
#' We construct variograms for both the dependent variable (`log(cost per
#' culvert)`) and the model residuals to investigate spatial dependence in both
#' costs and the model's errors.

#+ include=F
##INVESTIGATE SPATIAL DEPENDENCE IN DEPENENT VARIABLE AND ERROR

##Semivariogram plots of spatial dependence against distance from observation (note: .1 degree ~ 11.1 KM)
distmat <- dist(df_culv[c("long","lat")])
#summarize distances and use information to define semivariance range (alternative buffers)
# summary(distmat)
buffs <- seq(0, .5, l = 10)
#Plot semivariance in log(avg_cost)
semivar_dep <- variog( coords = df_culv[c("long","lat")], data = log(df_culv$cost_per_culvert), breaks = buffs )
#Plot residual semivariance
semivar_res <- variog( coords = df_culv[c("long","lat")], data = mod_full$residuals, breaks = buffs )
#+ fig.width=10, fig.height=10, echo=F, message=F, warning=F
plot(semivar_dep, type = "b", main = "Variogram for log(cost_per_culvert)", xlab = "distance (degree)")
plot(semivar_res, type = "b", main = "Variogram for residuals", xlab = "distance (degree)")

      # ---> Plots imply that most of the semivariance in depvar and error is captured within 
      #      20km buffer of observation

#' These plots imply that most of the semivariance in both the dependent
#' variable and the error term is captured within a 0.2 degree (or ~20km). We
#' will use this as our threshold for defining which points are neighbors. We
#' also remove 29 worksites with no neighbors (i.e. with no other worksites within
#' 20km) to ensure suitable spatial weighting matrix structure.

#+ include=F
# Prepare data for weights ----
# Remove double points
# df_culv <- df_culv %>% add_count(lat, long) %>% filter(n == 1) %>% select(-n)
# There are 51 worksites with the same lat/long

# Set neighbor threshold (km)
thresh <- 20

# Identify observations with no neighbors --- problematic with 0/1 neighbor matrices, only dropping to investigate spat dep
id_mat <- nb2mat(dnearneigh(as.matrix(df_culv[c("long","lat")]), 
                          d1 = 0, d2 = thresh, longlat = T), zero.policy = T)

id_islands <- df_culv$worksite_id[colSums(id_mat) == 0]
df_culv_sp <- df_culv %>%
 filter(!worksite_id %in% id_islands)

id_mat_time <- matrix(nrow = nrow(df_culv_sp), ncol = nrow(df_culv_sp))
for(i in c(1:nrow(df_culv_sp))) {
  for(j in c(1:nrow(df_culv_sp))) {
  id_mat_time[i, j] = as.numeric(df_culv_sp[i,"project_year"] == df_culv_sp[j,"project_year"])
  }
}
diag(id_mat_time) <- 0

##Estimate same model with "islands" and doubles removed
mod_full_sp <- 
  lm(f, data = df_culv_sp)
summary(mod_full_sp)
coeftest(mod_full_sp, .vcov = vcovCL(mod_full_sp, ~ project_id, "HC3"))

# Generate weights ----
##CREATE NEIGHBOR AND DISTANCE MATRICIES --- after re-running model with "trimmed" sample
#Neighbor matrices -- neighbors specified based on a distance buffer assumed to have equal dependence (0/1)
w_band <- nb2listw(dnearneigh(as.matrix(df_culv_sp[c("long","lat")]),
                             d1=0, d2 = thresh, longlat = T) , style="W")
#Distance matrix in KM
coords_xy <- df_culv_sp[c("long", "lat")]
coordinates(coords_xy) <- ~long+lat
dist_km <- rdist.earth(coordinates(coords_xy), miles = F)

#Inverse distance weight matrix -- assumes dependence decays with increasing distance
w_decay <- ifelse(dist_km != 0, 1/dist_km, dist_km) #Inverse distance matrix
diag(w_decay) <- 0
w_decay_t <- mat2listw(id_mat_time * as.matrix(w_decay), style="W")
w_hybrid <- ifelse(dist_km > thresh, 0, w_decay)   #Create neighbors buffer (optional)
# w_hybrid_t <- mat2listw(id_mat_time * as.matrix(w_hybrid), style="W")

w_decay <- mat2listw(as.matrix(w_decay), style="W")    #Normalized list
w_hybrid <- mat2listw(as.matrix(w_hybrid), style="W")

# w_band_t <- mat2listw(id_mat_time * as.matrix(ifelse(dist_km == 0, 0, ifelse(dist_km > thresh, 0, w_decay))), style="W")

# Build time-dependent weights (i.e. no cross-period distances)
  

# Spatial dependence tests ----
#+
#' # Statistical tests of spatial dependence
#' 

#' We use the 20km range established using the above variograms to construct
#' four spatial weighting matrices:  
#'   

#' 1. *Distance band specification (Band)*: weighting matrix is zeros and ones, where *w~ij~* =
#' *w~ji~* = 1 when *worksite_i* is within 20km of *worksite_j* (assumes all neighbors
#' within buffer have equal dependence).  

#' 2. *Inverse distance decay specification (Decay)*: weighting matrix is *w~ij~* = *w~ji~* = 1/*d~ij~*
#' where *d~ij~* is the distance between *worksite~i~* and *worksite~j~* (assumes
#' dependence decays with distance).  

#' 3. *Hybrid specification (Hybrid)*: weights are inverse distances when worksite pair is
#' within buffer, and zero otherwise.  

#' 4. *Inverse distance decay with time specification (Decay_t)*: weights are as the
#' inverse distance decay specification but distances are only computed between
#' worksties that share the same project year. This essentially imposes a restriction 
#' of no between-year spatial effects.  
#'   

#' For each of these sptail weights specifications, we perform a suite of tests of spatial dependence.  
#'   

#' 1. *Moran's I tests*: these tests, performed on both the dependent variable (MoranDep)
#' and the residuals from the model (MoranResid), identify spatial dependence, where the
#' null is no spatial dependence (i.e., distribution over space as good as random).  

#' 2. *Anselin's Lagrange multiplier tests*: these tests of linear restrictions
#' identify mispecification due to spatial dependence, distinguishing whether
#' the source is due to missing spatial lags of the dependent variable or
#' missing spatial error structure, or both. Per Anselin's suggestion, we
#' consider the robust tests only if both simple tests reject the null.  

#'    - *LM test against error (LMerr)*: tests against the null of no spatial error.  
#'    - *LM test against lag (LMlag)*: tests against the null of no spatial lag structure.  
#'    - *Robust LM test against error (RLMerr)*: a version of LMerr robust to the presence of spatial lag.  
#'    - *Robust LM test against lag (RLMlag)*: a version of LMlag robust to the presence of spatial error.  
#'    - *LMtest against Spatial Autoregressive Moving Average (SARMA)*: tests against the null of neither spatial error nor spatial lag.  
#' 

#+ include=F
##TESTS OF SPATIAL DEPENDENCE

# Goal is to build a table with rows as tests and columns as spatial matrix specification

# Let's put together a function that gathers the test stats based on a model and a weighting matrix
# Built for use with map and reduce

spdep_suite <-
  function(
    model,
    listw
  ){
    if(exists("y", model)) {
      y <- model$y
    } else {
      model_y <- update(model, y = TRUE)
      y <- model_y$y
    }
    #Moran's I test on on dependent variable - H0: No spatial dependence
    moran.dep.test <- moran.test(y, listw)
      moran.dep.stat <- moran.dep.test$statistic
      moran.dep.p <- moran.dep.test$p.value
    #Moran's I test on residuals
    moran.lm.test <- lm.morantest(model, listw, alternative = "two.sided")
      moran.lm.stat <- moran.lm.test$statistic
      moran.lm.p <- moran.lm.test$p.value
    #Lagrange Multiplier test for spatially model specification -- SAR? AR? SARAR?, SARA if all rejected.
    LM_test.test <- lm.LMtests(model, listw, test = "all")
    LM_test.stat <- c()
    LM_test.p <- c()
      for(i in 1:length(LM_test.test)) {
        LM_test.stat[i] <- LM_test.test[[i]]$statistic
        LM_test.p[i] <- LM_test.test[[i]]$p.value
      }
    # names(LM_test.stat) <- names(LM_test)
    # names(LM_test.p) <- names(LM_test)
    tibble(
      test_name = c("MoranDep", "MoranResid", names(LM_test.test)),
      test_stat = c(moran.dep.stat, moran.lm.stat, LM_test.stat),
      test_p = c(moran.dep.p, moran.lm.p, LM_test.p),
    ) %>% 
      mutate(
        test_stat = format(test_stat, digits = 3),
        test_p = format.pval(test_p, digits = 3, eps = 0.001)
      ) %>%
      transmute(test_name, test_out = paste0(test_stat, " (", test_p, ")"))
  }

# A - Neighbor specification (distance band)
spdep_suite(mod_full_sp, w_band)
# spdep_suite(mod_full_sp, w_band_t)

# B - Inverse distance specification (distance decay)
spdep_suite(mod_full_sp, w_decay)
spdep_suite(mod_full_sp, w_decay_t)

# C - Inverse distance with buffer specification (distance decay with band cutoff)
spdep_suite(mod_full_sp, w_hybrid)
# spdep_suite(mod_full_sp, w_hybrid_t)

#+ echo=F
# All together!
# List of weighting matrix
list_weights <- lst(w_band, w_decay, w_hybrid, w_decay_t)
# Map our function over it and present as fancy kable
map(
  list_weights,
  ~spdep_suite(mod_full_sp, .)
) %>%
  reduce(left_join, by = "test_name") %>%
  rename_with(~names(list_weights) %>% str_remove("w_") %>% str_to_sentence(), .cols = -test_name) %>%
  rename(Test = 1) %>%
  kable(
    caption = "Test statistics against spatial dependence"
  ) %>%
  kable_styling("hover") %>%
  column_spec(1 , bold = TRUE) %>%
  add_header_above(
    c(
      " " = 1,
      "Spatial weights" = 4
    )
  ) %>%
  add_footnote("P-values in parentheses", notation = "none")

#' The Moran test results indicate a strong degree of spatial clustering (or
#' "hot spots") in both the raw dependent variable and the residuals, though the
#' test statistic shrinks considerably in the residual, suggesting the observed
#' explanatory variables control for a portion of this effect.  
#' 

#' For all four weighting schemes, the first-stage LM tests are inconclusive,
#' rejecting the null in all cases. For all three specifications, the robust LM
#' tests indicate a spatial lag specification is more appropriate than the
#' spatial error specification. (The SARMA test suggests that spatial dependence
#' of either type is present.)
#'   
#+ echo=F
f_sp <- update.formula(f, . ~ . - basin - ua_dist - const_totp - brick_totp - metal_totp - sales_coun)
f_sp_t <- update.formula(f_sp, . ~ . - factor(project_year))


#+ include=F
# Estimate new models ----
# ESTIMATE MODELS via GMM (Kelejian & Prucha 2010)
# Band specification (neighborhood)
mod_band <- spreg(f_sp, data = df_culv_sp, listw = w_band, het = TRUE, na.action = na.omit, model = "lag")
summary(mod_band)

# Decay (inverse distance)
mod_decay <- spreg(f_sp, data = df_culv_sp, listw = w_decay, het = TRUE, na.action = na.omit, model = "lag")
summary(mod_decay)

# Decay w/ time (inverse distance)
mod_decay_t <- spreg(
  # log(cost_per_culvert) ~ n_worksites*tot_dist + slope*bankfull_width, 
  f_sp_t,
  data = df_culv_sp, listw = w_decay_t, het = TRUE, na.action = na.omit, model = "lag")
summary(mod_decay_t)

# Hybrid (inverse distance w/ neighborhood cut-off)
mod_hybrid <- spreg(f_sp, data = df_culv_sp, listw = w_hybrid, het = TRUE, na.action = na.omit, model = "lag")
summary(mod_hybrid)

#' # Spatial econometric models  
#' 

#' Here we present three Cliff-Ord type spatial models, one for each spatial
#' weighting scheme. Based on the preceding tests, we estimate these models with
#' both spatial lag (lambda) terms. We estimate these models via a GMM estimator
#' that accounts for heteroskedastic errors of unknown form ([Kelejian and
#' Prucha 2010](https://doi.org/10.1016/j.jeconom.2009.10.025)).  
#' 

#' Unfortunately, these estimators do not behave well when other distance-based
#' variables, including the distance to the nearest urban area, density of
#' suppliers, and the basin fixed effect.
#' Therefore, these variables are omitted from these specifications.

#+ echo=F
tidy.sphet <- function(x) {
  
  result <- summary(x)$CoefTable %>%
    tibble::as_tibble(rownames = "term") %>%
    dplyr::rename(estimate = Estimate,
                  std.error = `Std. Error`,
                  statistic = `t-value`,
                  p.value = `Pr(>|t|)`)
  
  result
}

mods <- lst(mod_full_sp, mod_band, mod_decay, mod_hybrid, mod_decay_t)
mods_pars <-
  map_df(mods, tidy, .id = "model") %>%
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
      # term == "factor(I(n_worksites == 1))TRUE" ~ "Single worksite (dummy)",
      term == "slope" ~ "Stream slope",
      term == "bankfull_width" ~ "Bankfull width",
      str_detect(term, "here_paved") ~ "Road paved (dummy)",
      term == "cat_basin_slope" ~ "Terrain slope",
      term == "cat_elev_mean" ~ "Elevation",
      term == "hdens_cat" ~ "Housing density",
      term == "emp_const" ~ "Construction employment",
      term == "emp_agforest" ~ "Ag/forestry employment",
      # term == "ua_dist" ~ "Distance to urban area",
      term == "pv_1km_buff" ~ "Private land, individual or company (% 500m buffer)",
      term == "pvi_1km_buff" ~ "Private land, managed by industry  (% 500m buffer)",
      term == "pvn_1km_buff" ~ "Private land, managed by non-industrial owner  (% 500m buffer)",
      term == "slope:bankfull_width" ~ "Stream slope X bankfull width",
      term == "n_worksites:tot_dist" ~ "Number of worksites X distance",
      term == "lambda" ~ "Lambda (lag parameter)",
      TRUE ~ term
    ),
    term = str_replace(term, "project_source", "Project source: "),
    # term = str_replace(term, "basin", "Basin: "),
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
    "Private land, individual or company (% 500m buffer)",
    "Private land, managed by industry  (% 500m buffer)",
    "Private land, managed by non-industrial owner  (% 500m buffer)",
    # "Distance to urban area",
    # Scale/scope features
    "Number of worksites",
    # starts_with("Distance between "),
    "Number of worksites X distance",
    starts_with("Culvert "),
    starts_with("Project source: "),
    starts_with("Year: "),
    "Lambda (lag parameter)"
  )

mods_pars %>%
  # left_join(mods_stats, by = "model") %>%
  pivot_longer(-model, names_to = "Term", values_to = "Value") %>%
  pivot_wider(id_cols = Term, names_from = model, values_from = Value) %>%
  select(Term, OLS = mod_full_sp, Band = mod_band, Decay = mod_decay, Hybrid = mod_hybrid, `Decay w/ Time` = `mod_decay_t`) %>%
  # kable() %>%
  kable(
    caption = "Spatial dependence models",
    escape = FALSE,
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
      "w/ Spatial Weights" = 4
    )
  ) %>%
  add_footnote("* p < 0.1, ** p < 0.05, *** p < 0.01", notation = "none") %>%
  scroll_box(height = "1200px")

#+
#' Overall, the conclusions we draw about the associations between costs and the
#' explanatory variables are quite consistent when compared to the basic OLS estimates.  
#'   

#' The spatial dependence parameters on all models are positive and
#' statistically significant, indicating positive spatial spillovers from nearby
#' projects. The positive lambdas indicate that more expensive worksites
#' in one location drive up costs nearby. These spillovers are present both 
#' between years and within years, as indicated by the positive lambda estimate 
#' when the time-dependent weights are used.    
#' 

#' Because the spatial lag parameter is non-zero, we cannot interpret the
#' coefficents as direct marginal effects as we would the OLS coefficients. A
#' change in any of the explanatory variables at one worksite impacts not only
#' the worksite's costs, but also costs at neighboring worksites, which in turn
#' impact costs at the original site. To account for these feedbacks, we can
#' calculate direct, total, and indirect effects. A variable's direct effect is
#' the effect (in terms of a cost multiplier) of a single unit increase at one
#' worksite on the costs at that same worksite, averaged over all worksites. A
#' variable's total effect is the effect of a single unit increase at one
#' worksite on costs across all worksites, averaged over all worksites.
#' 

#' Note that a "unit" change in different variables can be quite dramatic. For
#' example, slope is measured on a unit interval though only really ranges fro 0
#' to about 0.3, so a unit change would be an unrealistic range to expect within
#' the data. In the future, these effects calculations will be scaled to a
#' standard deviation change rather than unit change where appropriate to aid in
#' interpretation.  
#' 

#' These calculations are dependent on the spatial structure of the data, and
#' therefore should be interpreted with the following caveat. The worksite data
#' in PNSHP is not comprehensive for all culvert projects in the region during
#' the time period. Because we cannot identify where projects absent in the database were,
#' we cannot fully model the feedback effects that may occur with missing projects in the
#' data. Therefore, I believe these effect calculations will be of limited use.  
#' 

#+ echo=FALSE
tidy.impacts <-
  function(mod, listw) {
    impacts <- spatialreg::impacts(mod, listw = listw)
    tibble(
      Term = attr(impacts, "bnames"),
      Direct = exp(impacts$direct),
      Indirect = exp(impacts$indirect),
      Total = exp(impacts$total)
    ) %>%
      mutate(
        across(
          2:4,
          ~ format(signif(., 3), scientific = FALSE, drop0trailing = TRUE, trim = TRUE)
        )
      ) %>%
      rowwise() %>%
      mutate(
        `Effects (Direct / Indirect / Total)` = paste0(c_across(Direct:Total), collapse = " / ")
      ) %>%
      select(-c(2:4))
  }


mods_pars_impacts <-
  map2_df(mods[-1], list_weights, tidy.impacts, .id = "model") %>%
  complete(model, Term) %>%
  mutate(
    Term = case_when(
      Term == "(Intercept)" ~ "Intercept",
      Term == "action_fishpass_culvinst_prj" ~ "Culvert installation (dummy)",
      Term == "action_fishpass_culvrem_prj" ~ "Culvert removal (dummy)",
      Term == "n_worksites" ~ "Number of worksites",
      Term == "tot_dist" ~ "Distance between worksites",
      # term == "factor(I(n_worksites == 1))TRUE" ~ "Single worksite (dummy)",
      Term == "slope" ~ "Stream slope",
      Term == "bankfull_width" ~ "Bankfull width",
      str_detect(Term, "here_paved") ~ "Road paved (dummy)",
      Term == "cat_basin_slope" ~ "Terrain slope",
      Term == "cat_elev_mean" ~ "Elevation",
      Term == "hdens_cat" ~ "Housing density",
      Term == "emp_const" ~ "Construction employment",
      Term == "emp_agforest" ~ "Ag/forestry employment",
      # term == "ua_dist" ~ "Distance to urban area",
      Term == "pv_1km_buff" ~ "Private land, individual or company (% 500m buffer)",
      Term == "pvi_1km_buff" ~ "Private land, managed by industry  (% 500m buffer)",
      Term == "pvn_1km_buff" ~ "Private land, managed by non-industrial owner  (% 500m buffer)",
      Term == "slope:bankfull_width" ~ "Stream slope X bankfull width",
      Term == "n_worksites:tot_dist" ~ "Number of worksites X distance",
      # Term == "lambda" ~ "Lambda (lag parameter)",
      TRUE ~ Term
    ),
    Term = str_replace(Term, "project_source", "Project source: "),
    # term = str_replace(term, "basin", "Basin: "),
    Term = str_replace(Term, "factor[(]project_year[)]", "Year: "),
    Term = str_replace(Term, "factor[(]nlcd_current_class[)]", "Land cover: "),
    Term = str_replace(Term, "factor[(]here_speed[)]", "Road speed class: ")
  ) %>%
  select(
    model, Term, `Effects (Direct / Indirect / Total)`
  ) %>%
  pivot_wider(id_cols = model, names_from = Term, values_from = `Effects (Direct / Indirect / Total)`) %>%
  select(
    model,
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
    "Private land, individual or company (% 500m buffer)",
    "Private land, managed by industry  (% 500m buffer)",
    "Private land, managed by non-industrial owner  (% 500m buffer)",
    # "Distance to urban area",
    # Scale/scope features
    "Number of worksites",
    # starts_with("Distance between "),
    "Number of worksites X distance",
    starts_with("Culvert ")
  )

mods_pars_impacts %>%
  pivot_longer(-model, names_to = "Term", values_to = "Value") %>%
  pivot_wider(id_cols = Term, names_from = model, values_from = Value) %>%
  select(Term, Band = mod_band, Decay = mod_decay, Hybrid = mod_hybrid, `Decay w/ Time` = `mod_decay_t`) %>%
  # kable() %>%
  kable(
    caption = "Effects decomposition for spatial dependence models",
    escape = FALSE,
    align = 
      paste0(
        "l", 
        paste(
          rep(
            "c",
            length(mods[-1])
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
      "Effect (Direct / Indirect / Total)" = 4
    )
  ) %>%
  scroll_box(height = "1200px")


#+ echo=F
# Conclusion ----


#' # Closing Thoughts  
#' 

#' Overall, the spatial econometric methods have little effect on inference
#' regarding the explanatory variables. However, their inclusion does account
#' for an additional source of unobserved variance in the OLS models, while the
#' specific GMM methodology provides robustness against heteroskedastic errors.
#' The spatial parameters themselves provide additional richness to the
#' interpretation of the model. However, given the sparseness of our data, I
#' believe these models are of limited use. Interpreting feedback effects seems
#' particularly challenging, and prediction methods for out-of-sample points are
#' still not particularly well developed. (Definitely no off-the-shelf tools.)  
