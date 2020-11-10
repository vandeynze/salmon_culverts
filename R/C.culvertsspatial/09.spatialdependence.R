#' ---
#' title: "Spatial Associations in Culvert Cost Data"
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


#+ include=F
#################################
#
# Investigate spatial associations
# in the culvert average cost data
# Robby Fonner
#
#################################

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
library(spdep)
library(fields)
library(sphet)


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
  read_csv(here("output/culverts_pure_modelling.csv")) %>%
  mutate(
    # project_year = ordered(project_year),
    project_source = relevel(factor(project_source), ref = "OWRI"),
    basin = relevel(factor(basin), ref = "SOUTHERN OREGON COASTAL"),
    fips = factor(fips),
    state_fips = factor(state_fips),
    latlong = str_sub(geometry, 3, nchar(geometry)-1),
    long = as.numeric( word(latlong, 1, sep = ", ")),
    lat = as.numeric( word(latlong, 2, sep = ", "))
    # here_class = ordered(here_class)
  ) %>%
  left_join(
    key_nlcd, 
    by = c("nlcd_current" = "value")
  ) %>%
  select(-latlong) %>%
  #complete cases
  filter(complete.cases(cost_per_culvert,n_culverts,dist_mean,n_worksites,action_fishpass_culvrem_prj,action_fishpass_culvinst_prj,slope,
                        bankfull_width,here_paved,here_class,slope_deg,nlcd_current_class,hdens_cat,emp_const, emp_agforest, basin, project_year,
                        project_source))

# ESTIMATE FULL MODEL 
  
Dep <- "log(cost_per_culvert)"
Indep <- c("n_culverts" , "log(dist_mean+1)" , "factor(I(n_worksites==1))" , "action_fishpass_culvrem_prj" , "action_fishpass_culvinst_prj" ,
           # Stream features at work site: slope, bankfull width
           "slope * bankfull_width" , 
           # Road features at work site: paved, road class
           "here_paved" , "factor(here_class)" ,
           # Physical features of work site: ground slope, land cover
           "slope_deg" , "factor(nlcd_current_class)" ,
           # Population features: housing density, jobs in construction, jobs in ag/forestry
           "hdens_cat" , "emp_const" , "emp_agforest" ,
           # Fixed effects
           "basin" , "factor(project_year)" , "project_source")
f <- as.formula(
  paste(Dep,
        paste(Indep, collapse = "+"),
        sep = "~"))

mod_full <- 
  lm(f, df_culv )

summary(mod_full)
#plot(mod_full)


#+ fig.width=10, fig.height=10, echo=F, message=F, warning=F
#' # Variograms  
#'   

#' A variogram is used to identify the distance at which the variance within a
#' buffer is roughly equivalent to the variance over the entire space. This
#' distance will then be used to define which points are considered "neighbors"
#' and which are not.
#'
#' We construct variograms for both the dependent variable (`log(cost per
#' culvert)``) and the model residuals to investigate spatial dependence in both
#' costs and the model's errors.


##INVESTIGATE SPATIAL DEPENDENCE IN DEPENENT VARIABLE AND ERROR

##Semivariogram plots of spatial dependence against distance from observation (note: .1 degree ~ 11.1 KM)
distmat <- dist(df_culv[c("long","lat")])
#summarize distances and use information to define semivariance range (alternative buffers)
summary(distmat)
buffs <- seq(0, .5, l = 10)
#Plot semivariance in log(avg_cost)
semiVar_dep <- variog( coords = df_culv[c("long","lat")], data = log(df_culv$cost_per_culvert), breaks = buffs )
plot(semiVar_dep, type = "b", main = "Variogram for log(cost_per_culvert)", xlab = "distance (degree)")
#Plot residual semivariance
semiVar_res <- variog( coords = df_culv[c("long","lat")], data = mod_full$residuals, breaks = buffs )
plot(semiVar_res, type = "b", main = "Variogram for residuals", xlab = "distance (degree)")

      # ---> Plots imply that most of the semivariance in depvar and error is captured within 
      #      20km buffer of observation

#' These plots imply that most of the semivariance in both the dependent
#' variable and the error term is captured within a 0.2 degree (or ~20km). We
#' will use this as our threshold for defining which points are neighbors. We
#' also remove 30 worksites with no neighbors (i.e. with no other worksites within
#' 20km) to ensure suitable spatial weighting matrix structure.


#Set neighbor threshold (km)
thresh <- 20

#Identify observations with no neighbors --- problematic with 0/1 neighbor matrices, only dropping to investigate spat dep
IDmat <- nb2mat(dnearneigh(as.matrix(df_culv[c("long","lat")]), 
                          d1=0, d2 = thresh, longlat = T), zero.policy=T)

Islands <- df_culv$worksite_id[colSums(IDmat)==0]
df_culv_sp <- df_culv %>%
 filter(!worksite_id %in% Islands)

##Estimate same model with "islands" removed
mod_full_sp <- 
  lm(f, data = df_culv_sp)
# summary(mod_full_sp)

##CREATE NEIGHBOR AND DISTANCE MATRICIES --- after re-running model with "trimmed" sample
#Neighbor matrices -- neighbors specified based on a distance buffer assumed to have equal dependence (0/1)
neigh <- nb2listw(dnearneigh(as.matrix(df_culv_sp[c("long","lat")]),
                             d1=0, d2 = thresh, longlat = T) , style="W")
#Distance matrix in KM
XY <- df_culv_sp[c("long","lat")]
coordinates(XY) <- ~long+lat
distKM <- rdist.earth(coordinates(XY), miles = F)

#Inverse distance weight matrix -- assumes dependence decays with increasing distance
InvDist <- ifelse(distKM!=0, 1/distKM, distKM) #Inverse distance matrix
diag(InvDist) <- 0
InvDist_buff <- ifelse(distKM > thresh, 0, InvDist)   #Create neighbors buffer (optional)
InvDist <- mat2listw(as.matrix(InvDist), style="W")    #Normalized list
InvDist_buff <- mat2listw(as.matrix(InvDist_buff), style="W")

#+
#' # Statistical tests of spatial dependence
#' 

#' We use the 20km range established using the above variograms to construct
#' three spatial weighting matrices:  
#'   

#' 1. *Neighbor specification*: weighting matrix is zeros and ones, where w_ij =
#' w_ji = 1 when worksite_i is within 20km of worksite_j (assumes all neighbors
#' within buffer have equal dependence);  

#' 2. *Inverse distance specification:* weighting matrix is w_ij = w_ji = 1/d_ij
#' where d_ij is the distance between worksite_i and worksite_j (assumes
#' dependence decays with distance), and;  

#' 3. *Hybrid specification*: weights are inverse distances when worksite pair is
#' within buffer, and zero otherwise.  

#' For each of these specifications, we estimate the core model and perform a suite of tests of spatial dependence:  
#'   

#' 1. *Moran's I tests*: these tests, performed on both the dependent variable
#' and the residuals from the model, identify spatial dependence, where the
#' null is no spatial dependence (i.e., distribution over space as good as random), and;  

#' 2. *Anselin's Lagrange multiplier tests*: these tests of linear restrictions
#' identify mispecification due to spatial dependence, distinguishing whether
#' the source is due to missing spatial lags of the dependent variable or
#' missing spatial error structure, or both.
#' 

##TESTS OF SPATIAL DEPENDENCE

# Goal is to build a table with rows as tests and columns as spatial matrix specification

# Let's put together a function that gathers the test stats based on a model and a weighting matrix
# Built for use with map_dfc

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
      test_name = c("MoranDep", "MoranResid", names(LM_test)),
      test_stat = c(moran.dep.stat, moran.lm.stat, LM_test.stat),
      test_p = c(moran.dep.p, moran.lm.p, LM_test.p),
    ) %>% 
      mutate(
        test_stat = format(test_stat, digits = 3),
        test_p = format.pval(test_p, digits = 3, eps = 0.001)
      ) %>%
      transmute(test_name, test_out = paste0(test_stat, " (", test_p, ")"))
  }

#A - Neighbor specification
spdep_suite(mod_full_sp, neigh)

#B - InvDist specification
spdep_suite(mod_full_sp, InvDist)

#C - InvDist with buffer specification
spdep_suite(mod_full_sp, InvDist_buff)

# ESTIMATE MODELS via GMM (Kelejian & Prucha 2010)
#Neighbors
SARAR_neigh <- gstslshet(f, data = df_culv_sp, listw = neigh, zero.policy =F, na.omit, inverse=T, sarar = F, initial.value = "SAR") 
summary(SARAR_neigh)

#Inverse distance
SARAR_Inv <- gstslshet(f, data = df_culv_sp, listw = InvDist, zero.policy =F, na.omit, inverse=T, sarar = T) 
summary(SARAR_Inv)

#Inverse distance w buffer
SARAR_InvBuff <- gstslshet(f, data = df_culv_sp, listw = InvDist_buff, zero.policy =F, na.action = na.omit, inverse=T, sarar = T, initial.value = "SAR") 
summary(SARAR_InvBuff)


