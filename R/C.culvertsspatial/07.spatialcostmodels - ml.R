#' ---
#' title: "Culvert Cost Models with Spatially Explicit Data, Machine Learning Tests"
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

# Based loosely on Ch. 9-12 of Boehmke & Greenwell (https://bradleyboehmke.github.io/HOML/)

# Prepare environment and data ----
rm(list = ls())


library(searchable)
library(tidyverse)
library(janitor)
library(here)
library(readxl)
library(sandwich)
library(lmtest)


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

coeftest(
# summary(
  mod_full, 
         vcov. = vcovCL(mod_full, cluster = ~ project_id, type = "HC3")
         # vcov. = vcovHC(mod_full)
)

# ____ Try ML models ----
library(MASS)
set.seed(1)
library(tree)
set.seed(1)

# Select just dependent variable and possible features, plus fix character/categorical variables as factors
df_tree <-
  df_culv %>%
  select(
    -c(project_id:subbasin),
    -adj_cost, -pure_culv, -county, -state_fips, -ua_nn_name, -uc_nn_name, -fips, -ends_with("_nodata"), -c(id:tot_s1830), -c(nlcd_2001:nlcd_2016), -c(snet_distm:snet_name), -hu_mod,
    -geometry, -huc12, -huc12_name, -comid, -to_huc12, -c(nhd_dist_m:nhd_ftype),
    cost_per_culvert
  ) %>%
  mutate(
    across(c(here_speed:here_publi, here_class_0:here_paved_0), factor),
    project_year = ordered(project_year)
  ) %>%
  mutate(
    across(where(is.character), factor)
  ) %>%
  drop_na()
# Only 802 of 1,249 not missing any variables we keep (could dig deeper into what's dropped here to build up to larger sample)

# Select training group (half the total observations, used to build the model, remaining obs are used as out-of-sample test group)
train = sample(1:nrow(df_tree), nrow(df_tree)/2)

# Build a single regression tree
# tree_culv = tree(cost_per_culvert ~ ., df_tree, subset = train) # Non-log version
tree_culv = tree(log(cost_per_culvert) ~ ., df_tree, subset = train)
summary(tree_culv)
plot(tree_culv)
text(tree_culv, pretty=0)
# In each "leaf" the number is the median log(cost) of all worksites in the set that passes all the qualifiers above
# Each "branch" assigns worksites to the left if it passes the qualifier, right otherwise (YES --> left; No --> right)
# Fixed effects (project source, year) still show up as features, as does land use and private-industrial land (forestry)

# Cross validate to check different tree size
# We're looking for the number of nodes that minimizes the deviance
cv_tree = cv.tree(tree_culv); cv_tree
plot(cv_tree$size, cv_tree$dev, type='b')
# Tree size just 2 or 3 has the lowest deviance, but cross validation here is fairly variable
set.seed(2)
cv_tree = cv.tree(tree_culv); cv_tree
plot(cv_tree$size, cv_tree$dev, type='b')
# See 5 is lowest on this one
cv_tree = cv.tree(tree_culv); cv_tree
plot(cv_tree$size, cv_tree$dev, type='b')
# And 5 is lowest on this one too
cv_tree = cv.tree(tree_culv); cv_tree
plot(cv_tree$size, cv_tree$dev, type='b')
# And 18/19 and 23/24 are much lower on this one

# So I'm not sure how useful this approach is

# Prune tree to size with lowest deviance(s)
set.seed(1)
prune_culv_5 = prune.tree(tree_culv, best = 5)
summary(prune_culv_5)
plot(prune_culv_5)
text(prune_culv_5, pretty=0)
# Just like in the OLS model, OWRI projects, especially with multiple worksites
# (actions) are cheapest, some years are particularly expensive (2004-2007). The
# tree also finds sites further from construction equipment suppliers are more
# expansive (const_totp = employee-weighted construction machinery supplier density)

set.seed(1)
prune_culv_18 = prune.tree(tree_culv, best = 18)
summary(prune_culv_18)
plot(prune_culv_18)
text(prune_culv_18, pretty=0)
# This one is a lot like the original tree, which had 25 leaves so that's what we'd expect

# Predictions
# Let's see how the 5 leaf tree, 18 leaf tree, and full 25 leaf tree perform on the test group, by computing the root mean square error
yhat_culv_5 = predict(prune_culv_5, newdata = df_tree[-train,])
y_test = df_tree[-train, "cost_per_culvert"]
sqrt(sum((yhat_culv_5 - log(y_test)) ^ 2)/length(yhat_culv_5)) %>% c("Root Mean Square Error" = .)
plot(x = yhat_culv_5, y = log(y_test) %>% pull(cost_per_culvert))
abline(0,1)
# Not pretty, but it's something! Let's try again with the other trees
yhat_culv_18 = predict(prune_culv_18, newdata = df_tree[-train,])
sqrt(sum((yhat_culv_18 - log(y_test)) ^ 2)/length(yhat_culv_18)) %>% c("Root Mean Square Error" = .)
plot(x = yhat_culv_18, y = log(y_test) %>% pull(cost_per_culvert))
abline(0,1)
# Okay in terms of RMSE it's even worse, and the plot isn't pretty either
yhat_culv = predict(tree_culv, newdata = df_tree[-train,])
sqrt(sum((yhat_culv - log(y_test)) ^ 2)/length(yhat_culv)) %>% c("Root Mean Square Error" = .)
plot(x = yhat_culv, y = log(y_test) %>% pull(cost_per_culvert))
abline(0,1)
# And the RMSE is just as bad

# Finally, we'll do the same with the OLS model
mod_train <- update(mod_full, data = df_tree[train,])
yhat_culv_ols = predict(mod_train, newdata = df_tree[-train,])
sqrt(sum((yhat_culv_ols - log(y_test)) ^ 2)/length(yhat_culv_ols)) %>% c("Root Mean Square Error" = .)
plot(x = yhat_culv_ols, y = log(y_test) %>% pull(cost_per_culvert))
abline(0,1)

# The RMSE is about the same as the smallest tree

# ASIDE: One thing to think about is that project year, project source, and the
# number of worksites area all variables impossible to really observe for true
# out-of-sample culverts. These variables drive a lot of the cost variation, but
# the OLS model with fixed effects allows us to make predictions out-of-sample
# by "averaging" over these effects. We can do this for the trees too, but not
# sure it will be as useful if these features/variables drive most of the
# prediction (as is the case with the 4 leaf tree)

# Random forests
library(randomForest)

# This method uses "bagging", or bootstrap aggregating, to boostrap sample the
# training set N (1,000 here) times and generate a tree using M randomly
# selected candidate variables. The resulting model averages predictions over
# all the generated trees in the "forest"

set.seed(1)
bag_culv = randomForest(log(cost_per_culvert) ~ ., data = df_tree %>% select(-project_source, -basin), subset = train, mtry = 25, ntree = 1000, importance = TRUE)
bag_culv
# Looks like a good chance this is better than both the OLS and individual trees

# Predictions
yhat_culv_bag = predict(bag_culv, newdata = df_tree[-train,])
sqrt(sum((yhat_culv_bag - log(y_test)) ^ 2)/length(yhat_culv_bag)) %>% c("Root Mean Squared Error" = .)
# Even lower root mean square error than OLS or the individual trees
plot(x = yhat_culv_bag, y = log(y_test) %>% pull(cost_per_culvert))
abline(0,1)
# Looking better... but consistently underestimates costs at high level and over estimates at low end

importance(bag_culv)
varImpPlot(bag_culv)
# Most important vars, in terms of decreasing MSE and increasing node purity (consistency w/i nodes)...
# Private industry landownership nearby (i.e. forestry, various measures [pvi_Xkm_buff])
# Project year
# Forest (in upstream catchments [acc/tot])
# Accumulated upstream length (various measures [upst_dist, tot_stream_length, acc_stream_length])
# BFI (flow index, accumulated upstream catchments [acc])
# Elevation (in upstream catchments and locally [cat_elev_mean])
# Pop density (including employment "emp" measures)
# Material supplier density (const/brick/metal_totp)
# Basin slope (upstream catchments)
# Stream + road density
# Total distance between worksites (tot_dist)
# Distance to nearest road (here_distm)

# These are a lot of the variables we found important in the OLS model

# Boosting This method is similar to the bagging method above in that it
# generates a "random forest". Unlike the random forest above, each underlying
# "tree" in the "forest" is build sequentially on the residuals of the previous
# tree, so that each successive tree added results in improved prediction
# accuracy of the ensemble model (the forest). It proceeds using gradient
# descent method until it thinks it has found the global minimum for some loss
# function (here RMSE/SSE/MSE equivalently).

library(gbm)
set.seed(1)

# Takes ~1min to fun on my machine
boost_culv = gbm(log(cost_per_culvert) ~ ., data = df_tree[train,], distribution = "gaussian", n.trees = 5000, interaction.depth = 4, cv.folds = 5)
summary(boost_culv)

# Most important factors are...
# The fixed effects, especially source and basin
# Upstream distance
# Distance to road (here_distm)
# Basin slope upstream
# Employment (in finance but other employment measures are high too, probably a proxy for pop density)
# Catchment stream density
# Land use
# Stream slope (for reach)
# Private industrial land use and BLM land in 5km buffer
# Upstream stream slope
# Upstream road density
# Road paved status
# Upstream stream density
# Distance to urban cluster

# Again, mostly what we have in the OLS model, even closer match than the random forest results.
# Some new variables stand out, especially upstream distance and stream density measures

# We can look at the "partial" effects of each variable in the model

plot(boost_culv, i = "project_source") # Just like the fixed effects in the OLS model
plot(boost_culv, i = "basin") # Also pretty much the same
plot(boost_culv, i = "project_year") # Here too
# Same patterns we see in the fixed effects models

plot(boost_culv, i = "upst_dist"); summary(df_tree$upst_dist) # The culverts with the lowest upstream distance are cheaper, but not by much and no difference after a bit
plot(boost_culv, i = "upst_dist", xlim = c(0, 10)) # See here for predictions over the 0-75 percentile, steady increase in log(costs)
plot(boost_culv, i = "tot_total_road_dens"); summary(df_tree$tot_total_road_dens) # Small variation here but lower upstream road density associated with lower costs (Good!)
plot(boost_culv, i = "here_distm"); summary(df_tree$here_distm) # Further from an identified road, cheaper costs (smaller road!)
plot(boost_culv, i = "cat_strm_dens"); summary(df_tree$cat_strm_dens) # Higher stream density, higher costs
plot(boost_culv, i = "nlcd_current_fullclass"); summary(df_tree$nlcd_current_fullclass) # Looks like the coefficients from the OLS model
plot(boost_culv, i = "slope"); summary(df_tree$slope) # Steeper stream, more expensive
plot(boost_culv, i = "acc_basin_slope"); summary(df_tree$acc_basin_slope) # Steeper basins upstream, higher costs
plot(boost_culv, i = "pvi_1km_buff"); summary(df_tree$pvi_5km_buff) # Cheaper projects near private industrial land, especially above the 75th perc.
plot(boost_culv, i = "blm_5km_buff"); summary(df_tree$blm_5km_buff) # Similar pattern for BLM land
plot(boost_culv, i = "n_worksites"); summary(df_tree$n_worksites) # Single worksite projects most expensive, just like in OLS model
plot(boost_culv, i = "uc_dist"); summary(df_tree$uc_dist) # More expensive further from urban cluster, especially at furthest distances
plot(boost_culv, i = "here_speed"); summary(df_tree$here_speed) # Lowest speed class roads are the cheapest

# We can look at interactions, but don't find much
plot(boost_culv, i = c("n_worksites", "tot_dist")); summary(df_tree$tot_dist) # Total distance itself doesn't have much impact
plot(boost_culv, i = c("n_worksites", "dist_max")); summary(df_tree$dist_max) # Max distance has higher importance and we can see here evidence of a "distance-worksites" sweet spot

plot(boost_culv, i = c("slope", "bankfull_width")); summary(df_tree$slope); points(df_tree$slope, df_tree$bankfull_width)
# Looks quite a bit like our bankfull width - slope chart from the interaction effects in OLS
# Basically in-line with what we already knew, but what about predictive power...

yhat_boost = predict(boost_culv, newdata = df_tree[-train,], n.trees = 5000)
plot(x = yhat_boost, y = log(y_test) %>% pull(cost_per_culvert))
abline(0,1)

sqrt(sum((yhat_boost - log(y_test)) ^ 2)/length(yhat_boost)) %>% c("Root Mean Squared Error" = .)
# Not much different than the random forest
# But the fit line looks a LOT better

gbm.perf(boost_culv)
best <- which.min(boost_culv$cv.error)
sqrt(boost_culv$cv.error[best])
# So the 141st tree achieves the lowest training set RMSE (meaning we could save a lot of time during estimation)

# From here, we can adjust a bunch of the hyperparameters (the gradient descent
# rate, the number of possible leaves on each tree, the minimum observations in
# each leaf, size of the training set) and might find even better model
# performance. There are also a class of "stochastic" gradient boosting methods
# that can achieve better out of bag performance and avoid local mins by
# instituting the bootstrap techniques in bagged random forest.
#
# It might also be possible to tune the model for better performance in specific
# regions by adjusting the train/test balance or a custom loss function, but I
# have to dig deeper into the methods here.
