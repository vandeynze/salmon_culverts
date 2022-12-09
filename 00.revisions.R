# Ad hoc analysis for revisions
# Van Deynze, May '22
# Target date for resubmit: June 10

# Prepare environment ----
## Load packages ----

# Core packages
library(tidyverse)
library(here)
library(janitor)

# Specific packages
library(readxl) # Reads Excel sheets
library(fastDummies) # Builds dummy variables from factors
library(forcats) # Manipulates categorical variables
library(mctest) # Tests for multicollinarity
library(ggcorrplot) # Plots correlation plots in easily manipulated ggplot format
library(knitr) # Makes nice tables
library(kableExtra) # Makes nicer tables
library(vip) # Calculates variable importance
library(sf)
# devtools::install_github("3wen/legendMap")
library(legendMap)
library(patchwork)
library(scales)



## Load data ----
# Load NLCD key
key_nlcd <-
  read_xlsx(
    here(
      "data/Culverts spatial overlays v 20Jan2021.xlsx"
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

### Load PNSHP data ----
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
    here_paved = here_paved_0,
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

## Load inventory data ----
df_inv <-
  read_csv(here("output/inv_preds.csv"), guess_max = 1e6)

# Prepare correlation analysis ----
## Prepare data for correlation analysis ----
df_corr <-
  df_culv %>%
  mutate(
    here_paved = as.character(here_paved), 
    here_speed = ordered(here_speed) %>% fct_inorder(ordered = T) %>% fct_rev() %>% as.character(),
    nlcd_current_class = factor(nlcd_current_class) %>% fct_infreq() %>% as.character(),
    project_year = ordered(project_year) %>% as.character()
  ) %>%
  select(
    cost_per_culvert,
    bankfull_width,
    slope,
    here_speed,
    here_paved,
    cat_basin_slope,
    cat_elev_mean,
    nlcd_current_class,
    pv_1km_buff,
    pvi_1km_buff,
    pvn_1km_buff,
    hdens_cat,
    ua_dist,
    emp_const,
    emp_agforest,
    sales_coun,
    const_totp,
    brick_totp,
    n_worksites,
    tot_dist,
    action_fishpass_culvrem_prj,
    action_fishpass_culvinst_prj,
    basin,
    project_source,
    project_year
  ) %>%
  drop_na() %>%
  dummy_columns(
    ignore_na = TRUE,
    remove_first_dummy = TRUE,
    remove_selected_columns = TRUE
  )

mat_corr <-
  df_corr %>%
  cor() %>%
  as.matrix()


mat_vifs <- imcdiag(lm(log(cost_per_culvert) ~ ., df_corr), method = "VIF")[[1]] %>% as.matrix
mat_vifs <- rbind(mat_vifs, c(NA, NA))
diag(mat_corr) <- mat_vifs[,1] %>% round(1)
mat_corr[upper.tri(mat_corr)] <- NA

## Plot corrleation plot ----
ggcorrplot(
  mat_corr,
  lab = FALSE, lab_size = 2,
  colors = c("#6D9EC1", "white", "#E46726")
) +
  # geom_label(
  #     label = mat_vifs[,1],
  #     x = rep(1, nrow(mat_vifs)),
  #     y = c(1:nrow(mat_vifs)),
  #     data = tibble(x = c(1:nrow(mat_vifs)))
  # ) +
  theme(
    plot.title.position = "plot",
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "#ADADAD", linetype = "dashed"),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(angle = 45, vjust = 1, size = 9),
    axis.ticks.x.bottom = element_line(color = "#ADADAD"),
    legend.background = element_rect(color = "#ADADAD", fill = "white"),
    legend.position = c(0.15, 0.9),
    legend.margin = margin(5, 10, 10, 10),
    legend.direction = "horizontal"
    # plot.margin = unit(c(0.5, 0, 1.5, 0), "cm")
  ) +
  guides(fill = guide_colorbar(title.position = "top", label.position = "bottom")) +
  scale_fill_gradient2(
    name = "Pearson's corr. coef. (r)",
    limits = c(-1, 1),
    low = "#F8766D",
    mid = "#FFFFFF",
    high = "#619CFF",
    na.value = "#EAEAEA"
  ) +
  ggtitle(
    "Correlations among covariates"
  ) +
  coord_fixed(
    # ylim = c(0, 20),
    # xlim = c(0, 20),
    expand = FALSE,
    clip = "off"
  )

## Prepare VIFs table ----
tibble(var = rownames(mat_vifs), vif = mat_vifs[,1]) %>%
  rowwise() %>%
  mutate(across(where(is.numeric), ~(format(signif(., digits = 3), scientific = FALSE, big.mark = ",")))) %>%
  head(-1) %>%
  kable() %>%
  kable_styling()

# No correlations stand out as particularly worrisome, and none of the VIFs are particularly high


# Prepare plots of distributions of costs ----
fig_costdens_culv <-
  df_corr %>%
  ggplot() +
  geom_density(
    aes(x = cost_per_culvert),
    stat = "density",
    fill = "#fb6a4a",
    alpha = 0.7
  ) +
  scale_x_continuous(
    "Cost per culvert ($)",
    labels = label_dollar(),
    # trans = "log10"
  ) +
  scale_y_continuous("Density") +
  ggthemes::theme_clean() +
  theme(plot.background = element_blank())

fig_costdens_inv <-
  df_inv %>%
  ggplot() +
  geom_density(
    aes(
      x = exp(costpred_brt)
    ),
    stat = "density",
    fill = "#fb6a4a",
    alpha = 0.7
  ) +
  scale_x_continuous(
    "Cost per culvert ($)",
    labels = label_dollar(),
    # trans = "log10"
  ) +
  scale_y_continuous("Density") +
  # facet_wrap("basin_true", nrow = 7) +
  ggthemes::theme_clean() +
  theme(plot.background = element_blank())

fig_costdens_inv + facet_wrap("basin_true", nrow = 7)

# Prepare sample vs. population summary statistics and test of means ----
## Prepare population summary statistics ----
df_inv_summ <-
  df_inv %>%
  select(
    cost_per_culvert,
    bankfull_width,
    slope,
    here_paved,
    here_speed,
    cat_basin_slope,
    cat_elev_mean,
    nlcd_current_class,
    pv_1km_buff,
    pvi_1km_buff,
    pvn_1km_buff,
    hdens_cat,
    ua_dist,
    emp_const,
    emp_agforest,
    sales_coun,
    const_totp,
    brick_totp
  ) %>%
  mutate(
    here_paved = as.character(here_paved), 
    here_speed = ordered(here_speed) %>% fct_inorder(ordered = T) %>% fct_rev() %>% as.character(),
    nlcd_current_class = factor(nlcd_current_class) %>% fct_infreq() %>% as.character()
  ) %>%
  # drop_na() %>%
  dummy_columns(
    ignore_na = TRUE,
    remove_first_dummy = FALSE,
    remove_selected_columns = TRUE
  ) %>%
  select(where(is.numeric)) %>%
  relocate(starts_with("here"), .after = slope) %>%
  relocate(starts_with("nlcd"), .after = cat_elev_mean) %>%
  drop_na() %>%
  map_df(.f = ~ broom::tidy(summary(.x)), .id = "variable") %>%
  bind_cols(
    df_inv %>%
      select(
        cost_per_culvert,
        bankfull_width,
        slope,
        here_paved,
        here_speed,
        cat_basin_slope,
        cat_elev_mean,
        nlcd_current_class,
        pv_1km_buff,
        pvi_1km_buff,
        pvn_1km_buff,
        hdens_cat,
        ua_dist,
        emp_const,
        emp_agforest,
        sales_coun,
        const_totp,
        brick_totp
      ) %>%
      mutate(
        here_paved = as.character(here_paved), 
        here_speed = ordered(here_speed) %>% fct_inorder(ordered = T) %>% fct_rev() %>% as.character(),
        nlcd_current_class = factor(nlcd_current_class) %>% fct_infreq() %>% as.character()
      ) %>%
      dummy_columns(
        ignore_na = TRUE,
        remove_first_dummy = FALSE,
        remove_selected_columns = TRUE
      ) %>%
      select(where(is.numeric)) %>%
      relocate(starts_with("here"), .after = slope) %>%
      relocate(starts_with("nlcd"), .after = cat_elev_mean) %>%
      drop_na() %>%
      map_df(.f = ~ broom::tidy(sd(.x)), .id = "variable") %>%
      select(sd = x)
  )

## Prepare sample summary statistics ----
df_culv_summ <-
  df_culv %>%
  select(
    cost_per_culvert,
    bankfull_width,
    slope,
    here_speed,
    here_paved,
    cat_basin_slope,
    cat_elev_mean,
    nlcd_current_class,
    pv_1km_buff,
    pvi_1km_buff,
    pvn_1km_buff,
    hdens_cat,
    ua_dist,
    emp_const,
    emp_agforest,
    sales_coun,
    const_totp,
    brick_totp,
    n_worksites,
    tot_dist,
    action_fishpass_culvrem_prj,
    action_fishpass_culvinst_prj,
  ) %>%
  mutate(
    here_paved = as.character(here_paved), 
    here_speed = ordered(here_speed) %>% fct_inorder(ordered = T) %>% fct_rev() %>% as.character(),
    nlcd_current_class = factor(nlcd_current_class) %>% fct_infreq() %>% as.character()
  ) %>%
  dummy_columns(
    ignore_na = TRUE,
    remove_first_dummy = FALSE,
    remove_selected_columns = TRUE
  ) %>%
  select(where(is.numeric)) %>%
  relocate(starts_with("here"), .after = slope) %>%
  relocate(starts_with("nlcd"), .after = cat_elev_mean) %>%
  drop_na() %>%
  map_df(.f = ~ broom::tidy(summary(.x)), .id = "variable") %>%
  bind_cols(
    df_culv %>%
      select(
        cost_per_culvert,
        bankfull_width,
        slope,
        here_speed,
        here_paved,
        cat_basin_slope,
        cat_elev_mean,
        nlcd_current_class,
        pv_1km_buff,
        pvi_1km_buff,
        pvn_1km_buff,
        hdens_cat,
        ua_dist,
        emp_const,
        emp_agforest,
        sales_coun,
        const_totp,
        brick_totp,
        n_worksites,
        tot_dist,
        action_fishpass_culvrem_prj,
        action_fishpass_culvinst_prj,
      ) %>%
      mutate(
        here_paved = as.character(here_paved), 
        here_speed = ordered(here_speed) %>% fct_inorder(ordered = T) %>% fct_rev() %>% as.character(),
        nlcd_current_class = factor(nlcd_current_class) %>% fct_infreq() %>% as.character()
      ) %>%
      dummy_columns(
        ignore_na = TRUE,
        remove_first_dummy = FALSE,
        remove_selected_columns = TRUE
      ) %>%
      select(where(is.numeric)) %>%
      relocate(starts_with("here"), .after = slope) %>%
      relocate(starts_with("nlcd"), .after = cat_elev_mean) %>%
      drop_na() %>%
      map_df(.f = ~ broom::tidy(sd(.x)), .id = "variable") %>%
      select(sd = x)
  )

## Combine into one tibble ----
options(knitr.kable.NA = "--")
df_summ <-
  left_join(
    df_culv_summ,
    df_inv_summ,
    by = "variable",
    suffix = c("_sample", "_pop")
  )
df_summ %>%
  rowwise() %>%
  mutate(
    t = (mean_sample - mean_pop) / (sqrt((sd_sample^2 / nrow(df_culv)) + (sd_pop^2 / nrow(df_inv))))
  ) %>%
  mutate(across(where(is.numeric), ~(format(signif(., digits = 3), scientific = FALSE, big.mark = ",")))) %>%
  select(
    "Variable" = variable,
    "Mean, PNSHP" = mean_sample,
    "Std. Dev., PNSHP" = sd_sample,
    "Mean, Inventories" = mean_pop,
    "Std. Dev., Inventories" = sd_pop,
    "t-value" = t
  ) %>%
  kable() %>%
  kable_styling()


# Prepare area of applicability (AOA) analysis ----
# Meyer & Pedesma 2021: https://doi.org/10.1111/2041-210X.13650

# Based on "dissimilarity index" (DI) based on minimum distance to training data
# in multidimensional predictor space, with predictors weighted on importance in model

# Resulting predictions within the AOA are comparable to as if training data
# were randomly drawn from that space

## Load fitted models ----
# OLS
mod_ols <- read_rds(here("output/costfits/ols_full.rds"))

# BRT
mod_brt <- read_rds(here("output/costfits/boostedregression.rds"))

# RF
mod_rf <- read_rds(here("output/costfits/randomforest.rds"))

## Load inventory predictions ----
# Load inventories
sf_inv_preds <-
  here("output/inv_preds.csv") %>% read_csv(guess_max = 30000)  %>%
  mutate(geom = gsub(geometry,pattern="(\\))|(\\()|c",replacement = ""))%>%
  tidyr::separate(geom,into=c("lat","lon"),sep=",")

## Calculate AOA ----

# TODO
# Trying the packages has been quite the headache due to inconsistent methods 
# relative to the methods used last year...
# However the methods are fairly straightforward and should be easy enough
# to implement with custom code.
# So everything below here can be safely ignored... we will do
# this by hand.

### Standardize variables ----

# Merge sample and pop dataframes for consistent standardization
df_all_std <-
  bind_rows(
    df_culv %>% mutate(samp_dummy = 1),
    df_inv %>% mutate(samp_dummy = 0, here_speed = as.factor(here_speed), here_class = as.factor(here_class))
  )

# Generate names vector for dummy variables
dummy_vars_sample <-
  df_all_std %>%
  mutate(
    here_paved = as.factor(here_paved),
    here_paved_0 = as.factor(here_paved_0),
    here_speed = ordered(here_speed) %>% fct_inorder(ordered = T) %>% fct_rev(),
    here_speed_0 = ordered(here_speed_0) %>% fct_inorder(ordered = T) %>% fct_rev(),
    here_speed_badmatch = ordered(here_speed_badmatch) %>% fct_inorder(ordered = T) %>% fct_rev(),
    here_class = ordered(here_class) %>% fct_inorder(ordered = T) %>% fct_rev(),
    here_class_0 = ordered(here_class_0) %>% fct_inorder(ordered = T) %>% fct_rev(),
    here_class_badmatch = ordered(here_class_badmatch) %>% fct_inorder(ordered = T) %>% fct_rev(),
    here_publi = as.factor(here_publi),
    project_year = as.character(project_year) %>% ordered(),
    basin = as.factor(basin),
    project_source = as.factor(project_source),
    across(
      starts_with("nlcd") & where(is.character),
      ~factor(.) %>% fct_infreq()
    )
  ) %>%
  select(where(~is.factor(.))) %>%
  names()

# Vector of variable names used in ML methods
ml_vars <- 
  mod_brt$var.names

df_all_std <-
  df_all_std %>%
  ungroup() %>%
  mutate(
    here_paved = as.factor(here_paved), 
    here_speed = ordered(here_speed) %>% fct_inorder(ordered = T) %>% fct_rev(),
    here_speed_0 = ordered(here_speed_0) %>% fct_inorder(ordered = T) %>% fct_rev(),
    here_speed_badmatch = ordered(here_speed_badmatch) %>% fct_inorder(ordered = T) %>% fct_rev(),
    here_class = ordered(here_class) %>% fct_inorder(ordered = T) %>% fct_rev(),
    here_class_0 = ordered(here_class_0) %>% fct_inorder(ordered = T) %>% fct_rev(),
    here_class_badmatch = ordered(here_class_badmatch) %>% fct_inorder(ordered = T) %>% fct_rev(),
    project_year = as.character(project_year) %>% ordered(),
    basin = as.factor(basin),
    project_source = as.factor(project_source),
    across(
      starts_with(c("nlcd", "action")) & where(is.character),
      ~factor(.) %>% fct_infreq()
    )
  ) %>%
  # Make dummy variables
  dummy_columns(
    select_columns = dummy_vars_sample,
    ignore_na = TRUE,
    remove_first_dummy = FALSE,
    remove_selected_columns = TRUE
  ) %>%
  # Scale variables
  mutate(
    across(
      where(is.numeric) & starts_with(ml_vars),
      ~ (.x - mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE)
    )
  ) %>%
  select(starts_with(ml_vars), samp_dummy)

### Weight variables by variable importance ----
# Can do this with no weights and for each method, too, for comparison

# Drop columns with NA
na_cols <-
  df_all_std %>% 
  summarise(across(everything(), ~ sum(is.na(.)))) %>% 
  pivot_longer(everything()) %>% 
  filter(value %in% c(nrow(df_culv), nrow(df_inv), nrow(df_culv) + nrow(df_inv))) %>% 
  pull(name)

df_all_std <-
  df_all_std %>%
  select(-all_of(na_cols))

# Drop rows with NA
df_all_std <-
  df_all_std %>%
  drop_na()

# Build weights
## Seperate df_all into matrices
mat_culv_std <-
  (df_all_std %>%
     filter(samp_dummy == 1) %>%
     select(-samp_dummy, -ends_with("nodata"), -here_class_namatch) %>%
     select(order(colnames(.))) %>%
     # FOR TESTING ONLY, FILTER TO PUGET SOUND
     # filter(`basin_PUGET SOUND` > 0) %>%
     # END TEST SECTION
     as.matrix())
mat_inv_std <-
  (df_all_std %>%
     filter(samp_dummy == 0) %>%
     select(-samp_dummy, -ends_with("nodata"), -here_class_namatch) %>%
     select(order(colnames(.))) %>%
     # FOR TESTING ONLY, FILTER TO PUGET SOUND & SAMPLE 200 POINTS
     # filter(`basin_PUGET SOUND` > 0) %>% sample_n(200) %>%
     # END TEST SECTION
     as.matrix()) 

## Null (uniform) weights
w_null <-
  rep(1, ncol(mat_inv_std))

w_ols <-
  vi(mod_ols) %>%
  select(-Sign) %>%
  mutate(
    Variable =
      case_when(
        str_starts(Variable, "basin") ~ str_replace(Variable, "basin", "basin_"),
        str_starts(Variable, "project_source") ~ str_replace(Variable, "project_source", "project_source_"),
        str_starts(Variable, "factor\\(project_year\\)") ~ str_replace(Variable, "factor\\(project_year\\)", "project_year_"),
        str_starts(Variable, "factor\\(nlcd_current_class\\)") ~ str_replace(Variable, "factor\\(nlcd_current_class\\)", "nlcd_current_class_"),
        str_starts(Variable, "factor\\(here_speed\\)") ~ str_replace(Variable, "factor\\(here_speed\\)", "here_speed_"),
        str_starts(Variable, "factor\\(here_paved\\)") ~ str_replace(Variable, "factor\\(here_paved\\)", "here_paved_"),
        TRUE ~ Variable
      )
  ) %>%
  arrange(Variable) %>%
  filter(
    Variable %in% names(df_all_std)
  )

w_ols_names <-
  w_ols %>% pull(Variable)
w_ols <-
  w_ols %>% pull(Importance)

w_brt <-
  vi(mod_brt) %>%
  rowwise() %>%
  mutate(
    freq = 
      sum(str_starts(df_all_std %>% select(starts_with(dummy_vars_sample)) %>% names, Variable)),
    freq = if_else(freq == 0, 1, as.double(freq)),
    freq =
      case_when(
        Variable == "here_speed" ~ 6,
        Variable == "here_class" ~ 5,
        Variable == "here_paved" ~ 2,
        TRUE ~ freq
      )
  ) %>%
  slice(
    rep(
      seq_len(
        n()
      ),
      freq
    )
  ) %>%
  select(-freq) %>%
  arrange(Variable) %>%
  filter(
    Variable %in% c(names(df_all_std), dummy_vars_sample)
  )

w_brt_names <-
  w_brt %>% pull(Variable)
w_brt <-
  w_brt %>% pull(Importance)

w_rf <-
  vi(mod_rf) %>%
  rowwise() %>%
  mutate(
    freq = 
      sum(str_starts(df_all_std %>% select(starts_with(dummy_vars_sample)) %>% names, Variable)),
    freq = if_else(freq == 0, 1, as.double(freq)),
    freq =
      case_when(
        Variable == "here_speed" ~ 6,
        Variable == "here_class" ~ 5,
        Variable == "here_paved" ~ 2,
        TRUE ~ freq
      )
  ) %>%
  slice(
    rep(
      seq_len(
        n()
      ),
      freq
    )
  ) %>%
  select(-freq) %>%
  arrange(Variable) %>%
  filter(
    Variable %in% c(names(df_all_std), dummy_vars_sample)
  )

w_rf_names <-
  w_rf %>% pull(Variable)
w_rf <-
  w_rf %>% pull(Importance)

# Apply weights and separate into sample and pop matrices

### Calculate minimum multivariate distance in predictor space to nearest point in training data ----
# Distance function
euclidean <- function(a, b) sqrt(sum((a - b)^2))

# Generate new matrix where rows are rows of inventory and columns are distances to sample points
calc_dists <-
  function(
    inv_mat, # (standardized) data matrix of inventory (population) data
    culv_mat, # (standardized) data matrix of sample data, with same columns as above
    w = rep(1, ncol(inv_mat)), # weights, defaults to ones vector (i.e., no effect)
    w_names = NULL,
    w_thresh = 0 # threshold for weights, higher can decrease run-time by removing excess variables
  ) {
    # Reduce matrices and weights to variables with weights that exceed threshold (or are selected by name)
    if(is.null(w_names)) {
      inv_mat = inv_mat[, I(w > w_thresh)]
      culv_mat = culv_mat[, I(w > w_thresh)]
      w = w[I(w > w_thresh)]
    } else {
      inv_mat = inv_mat[, w_names]
      culv_mat = culv_mat[, w_names]
    }
    
    # Weight matrices
    inv_mat <- inv_mat * w
    culv_mat <- culv_mat * w
    
    # Calculate weights
    dists_mat <-
      matrix(nrow = nrow(inv_mat), ncol = nrow(culv_mat))
    for(i in c(1:nrow(inv_mat))) {
      for(j in c(1:nrow(culv_mat))) {
        dists_mat[i, j] <-
          euclidean(inv_mat[i, ], culv_mat[j, ])
      }
    }
    
    return(dists_mat)
  }

mat_dists_null <-
  calc_dists(
    mat_inv_std,
    mat_culv_std,
    w = w_null
  )

mat_dists_ols <-
  calc_dists(
    mat_inv_std,
    mat_culv_std,
    w = w_ols,
    w_names = w_ols_names
  )

mat_dists_brt <-
  calc_dists(
    mat_inv_std,
    mat_culv_std,
    w = w_brt
  )

mat_dists_rf <-
  calc_dists(
    mat_inv_std,
    mat_culv_std,
    w = w_rf
  )

# Generate minimum distance vector
calc_min_dist <-
  function(
    mat_dists
  ) {
    min_dists <-
      matrix(nrow = nrow(mat_dists), ncol = 1)
    for(i in c(1:nrow(min_dists))) {
      if(sum(diag(mat_dists)) == 0 | sum(is.na(diag(mat_dists))) == nrow(mat_dists)) {
        min_dists[i, 1] <-
          min(mat_dists[i,-i], na.rm = T)
      } else {
        min_dists[i, 1] <-
          min(mat_dists[i,], na.rm = T)
      }
    }
    
    return(min_dists)
  }

min_dists_null <- calc_min_dist(mat_dists_null)
min_dists_ols <- calc_min_dist(mat_dists_ols)
min_dists_brt <- calc_min_dist(mat_dists_brt)
min_dists_rf <- calc_min_dist(mat_dists_rf)

### Compute dissimilarity index for all points in projection space ----
# Compute average of all pairwise distances in training data
mat_dists_null_culv <-
  calc_dists(
    mat_culv_std,
    mat_culv_std,
    w = w_null
  )
diag(mat_dists_null_culv) <- NA

mat_dists_ols_culv <-
  calc_dists(
    mat_culv_std,
    mat_culv_std,
    w = w_ols,
    w_names = w_ols_names
  )
diag(mat_dists_ols_culv) <- NA

mat_dists_brt_culv <-
  calc_dists(
    mat_culv_std,
    mat_culv_std,
    w = w_brt
  )
diag(mat_dists_brt_culv) <- NA

mat_dists_rf_culv <-
  calc_dists(
    mat_culv_std,
    mat_culv_std,
    w = w_rf
  )
diag(mat_dists_rf_culv) <- NA

dist_bar_null <- mean(mat_dists_null_culv, na.rm = T)
dist_bar_ols <- mean(mat_dists_ols_culv, na.rm = T)
dist_bar_brt <- mean(mat_dists_brt_culv, na.rm = T)
dist_bar_rf <- mean(mat_dists_rf_culv, na.rm = T)


# Generate minimum distance vector
min_dists_null_culv <- calc_min_dist(mat_dists_null_culv)
min_dists_ols_culv <- calc_min_dist(mat_dists_ols_culv)
min_dists_brt_culv <- calc_min_dist(mat_dists_brt_culv)
min_dists_rf_culv <- calc_min_dist(mat_dists_rf_culv)

# Divide distance to nearest point in inventory data by average distance between training data
min_dists_di_null <- min_dists_null/dist_bar_null
min_dists_di_ols <- min_dists_ols/dist_bar_ols
min_dists_di_brt <- min_dists_brt/dist_bar_brt
min_dists_di_rf <- min_dists_rf/dist_bar_rf

min_dists_culv_di_null <- min_dists_null_culv/dist_bar_null
min_dists_culv_di_ols <- min_dists_ols_culv/dist_bar_ols
min_dists_culv_di_brt <- min_dists_brt_culv/dist_bar_brt
min_dists_culv_di_rf <- min_dists_rf_culv/dist_bar_rf

summary(min_dists_culv_di_null)
summary(min_dists_culv_di_ols)
summary(min_dists_culv_di_brt)
summary(min_dists_culv_di_rf)

# Define AOA threshold (larger than the 75-percentile plus 1.5 times the IQR)
out_max_null <- quantile(min_dists_culv_di_null)["75%"] + 1.5*IQR(min_dists_culv_di_null)
out_max_ols <- quantile(min_dists_culv_di_ols)["75%"] + 1.5*IQR(min_dists_culv_di_ols)
out_max_brt <- quantile(min_dists_culv_di_brt)["75%"] + 1.5*IQR(min_dists_culv_di_brt)
out_max_rf <- quantile(min_dists_culv_di_rf)["75%"] + 1.5*IQR(min_dists_culv_di_rf)


aoa_thresh_null <- max(min_dists_culv_di_null[min_dists_culv_di_null < out_max_null])
aoa_thresh_ols <- max(min_dists_culv_di_ols[min_dists_culv_di_ols < out_max_ols])
aoa_thresh_brt <- max(min_dists_culv_di_brt[min_dists_culv_di_brt < out_max_brt])
aoa_thresh_rf <- max(min_dists_culv_di_rf[min_dists_culv_di_rf < out_max_rf])

length(min_dists_di_null[min_dists_di_null < aoa_thresh_null]) / length(min_dists_di_null)
length(min_dists_di_ols[min_dists_di_ols < aoa_thresh_ols]) / length(min_dists_di_ols)
length(min_dists_di_brt[min_dists_di_brt < aoa_thresh_brt]) / length(min_dists_di_brt)
length(min_dists_di_rf[min_dists_di_rf < aoa_thresh_rf]) / length(min_dists_di_rf)

length(min_dists_culv_di_null[min_dists_culv_di_null < out_max_null]) / length(min_dists_culv_di_null)
length(min_dists_culv_di_ols[min_dists_culv_di_ols < out_max_ols]) / length(min_dists_culv_di_ols)
length(min_dists_culv_di_brt[min_dists_culv_di_brt < out_max_brt]) / length(min_dists_culv_di_brt)
length(min_dists_culv_di_rf[min_dists_culv_di_rf < out_max_rf]) / length(min_dists_culv_di_rf)


# Clear out NAs from dfs to rejoin
df_inv_di <-
  df_inv %>%
  ungroup() %>%
  mutate(
    here_paved = as.factor(here_paved), 
    here_speed = ordered(here_speed) %>% fct_inorder(ordered = T) %>% fct_rev(),
    here_speed_0 = ordered(here_speed_0) %>% fct_inorder(ordered = T) %>% fct_rev(),
    here_speed_badmatch = ordered(here_speed_badmatch) %>% fct_inorder(ordered = T) %>% fct_rev(),
    here_class = ordered(here_class) %>% fct_inorder(ordered = T) %>% fct_rev(),
    here_class_0 = ordered(here_class_0) %>% fct_inorder(ordered = T) %>% fct_rev(),
    here_class_badmatch = ordered(here_class_badmatch) %>% fct_inorder(ordered = T) %>% fct_rev(),
    project_year = as.character(project_year) %>% ordered(),
    basin = as.factor(basin),
    project_source = as.factor(project_source),
    across(
      starts_with(c("nlcd", "action")) & where(is.character),
      ~factor(.) %>% fct_infreq()
    )
  ) %>%
  select(id, geometry, huc12, costpred_brt, starts_with(ml_vars), -all_of(na_cols), basin_true) %>%
  drop_na() %>%
  bind_cols(
    di_null = as.vector(min_dists_di_null),
    di_ols = as.vector(min_dists_di_ols),
    di_brt = as.vector(min_dists_di_brt),
    di_rf = as.vector(min_dists_di_rf)
  ) %>%
  rowwise() %>%
  mutate(
    aoa_null = di_null < aoa_thresh_null,
    aoa_ols = di_ols < aoa_thresh_ols,
    aoa_brt = di_brt < aoa_thresh_brt,
    aoa_rf = di_rf < aoa_thresh_rf
  )
  
df_culv_di <-
  df_culv %>%
  drop_na(
    all_of(
      intersect(
        names(
          df_all_std
        ),
        names(
          df_culv
        )
      )
    )
  ) %>%
  bind_cols(
    min_dists_di_null = as.vector(min_dists_culv_di_null),
    min_dists_di_ols = as.vector(min_dists_culv_di_ols),
    min_dists_di_brt = as.vector(min_dists_culv_di_brt),
    min_dists_di_rf = as.vector(min_dists_culv_di_rf)
  )

# Compare barriers inside and outside AoA ----
## Prepare inside AoA summary statistics ----
df_inaoa_summ <-
  df_inv_di %>%
  filter(aoa_brt == TRUE) %>%
  select(
    bankfull_width,
    slope,
    here_paved,
    here_speed,
    cat_basin_slope,
    cat_elev_mean,
    nlcd_current_class,
    pv_1km_buff,
    pvi_1km_buff,
    pvn_1km_buff,
    hdens_cat,
    ua_dist,
    emp_const,
    emp_agforest,
    sales_coun,
    const_totp,
    brick_totp
  ) %>%
  mutate(
    here_paved = as.character(here_paved), 
    here_speed = ordered(here_speed) %>% fct_inorder(ordered = T) %>% fct_rev() %>% as.character(),
    nlcd_current_class = factor(nlcd_current_class) %>% fct_infreq() %>% as.character()
  ) %>%
  # drop_na() %>%
  dummy_columns(
    ignore_na = TRUE,
    remove_first_dummy = FALSE,
    remove_selected_columns = TRUE
  ) %>%
  select(where(is.numeric)) %>%
  relocate(starts_with("here"), .after = slope) %>%
  relocate(starts_with("nlcd"), .after = cat_elev_mean) %>%
  drop_na() %>%
  map_df(.f = ~ broom::tidy(summary(.x)), .id = "variable") %>%
  bind_cols(
    df_inv_di %>%
      filter(aoa_brt == TRUE) %>%
      select(
        bankfull_width,
        slope,
        here_paved,
        here_speed,
        cat_basin_slope,
        cat_elev_mean,
        nlcd_current_class,
        pv_1km_buff,
        pvi_1km_buff,
        pvn_1km_buff,
        hdens_cat,
        ua_dist,
        emp_const,
        emp_agforest,
        sales_coun,
        const_totp,
        brick_totp
      ) %>%
      mutate(
        here_paved = as.character(here_paved), 
        here_speed = ordered(here_speed) %>% fct_inorder(ordered = T) %>% fct_rev() %>% as.character(),
        nlcd_current_class = factor(nlcd_current_class) %>% fct_infreq() %>% as.character()
      ) %>%
      dummy_columns(
        ignore_na = TRUE,
        remove_first_dummy = FALSE,
        remove_selected_columns = TRUE
      ) %>%
      select(where(is.numeric)) %>%
      relocate(starts_with("here"), .after = slope) %>%
      relocate(starts_with("nlcd"), .after = cat_elev_mean) %>%
      drop_na() %>%
      map_df(.f = ~ broom::tidy(sd(.x)), .id = "variable") %>%
      select(sd = x)
  )

## Prepare outside AoA summary statistics ----
df_outaoa_summ <-
  df_inv_di %>%
  filter(aoa_brt == FALSE) %>%
  select(
    bankfull_width,
    slope,
    here_speed,
    here_paved,
    cat_basin_slope,
    cat_elev_mean,
    nlcd_current_class,
    pv_1km_buff,
    pvi_1km_buff,
    pvn_1km_buff,
    hdens_cat,
    ua_dist,
    emp_const,
    emp_agforest,
    sales_coun,
    const_totp,
    brick_totp
  ) %>%
  mutate(
    here_paved = as.character(here_paved), 
    here_speed = ordered(here_speed) %>% fct_inorder(ordered = T) %>% fct_rev() %>% as.character(),
    nlcd_current_class = factor(nlcd_current_class) %>% fct_infreq() %>% as.character()
  ) %>%
  dummy_columns(
    ignore_na = TRUE,
    remove_first_dummy = FALSE,
    remove_selected_columns = TRUE
  ) %>%
  select(where(is.numeric)) %>%
  relocate(starts_with("here"), .after = slope) %>%
  relocate(starts_with("nlcd"), .after = cat_elev_mean) %>%
  drop_na() %>%
  map_df(.f = ~ broom::tidy(summary(.x)), .id = "variable") %>%
  bind_cols(
    df_inv_di %>%
      filter(aoa_brt == FALSE) %>%
      select(
        bankfull_width,
        slope,
        here_speed,
        here_paved,
        cat_basin_slope,
        cat_elev_mean,
        nlcd_current_class,
        pv_1km_buff,
        pvi_1km_buff,
        pvn_1km_buff,
        hdens_cat,
        ua_dist,
        emp_const,
        emp_agforest,
        sales_coun,
        const_totp,
        brick_totp
      ) %>%
      mutate(
        here_paved = as.character(here_paved), 
        here_speed = ordered(here_speed) %>% fct_inorder(ordered = T) %>% fct_rev() %>% as.character(),
        nlcd_current_class = factor(nlcd_current_class) %>% fct_infreq() %>% as.character()
      ) %>%
      dummy_columns(
        ignore_na = TRUE,
        remove_first_dummy = FALSE,
        remove_selected_columns = TRUE
      ) %>%
      select(where(is.numeric)) %>%
      relocate(starts_with("here"), .after = slope) %>%
      relocate(starts_with("nlcd"), .after = cat_elev_mean) %>%
      drop_na() %>%
      map_df(.f = ~ broom::tidy(sd(.x)), .id = "variable") %>%
      select(sd = x)
  )

## Combine into one tibble ----
options(knitr.kable.NA = "--")
df_summ_aoa <-
  left_join(
    df_inaoa_summ,
    df_outaoa_summ,
    by = "variable",
    suffix = c("_inaoa", "_outaoa")
  )
df_summ_aoa %>%
  rowwise() %>%
  mutate(
    t = (mean_inaoa - mean_outaoa) / (sqrt((sd_inaoa^2 / nrow(df_inv_di[df_inv_di$aoa_brt==TRUE,])) + (sd_outaoa^2 / nrow(df_inv_di[df_inv_di$aoa_brt==FALSE,]))))
  ) %>%
  rowwise() %>%
  mutate(across(where(is.numeric), ~(format(signif(., digits = 3), scientific = FALSE, big.mark = ",")))) %>%
  select(
    "Variable" = variable,
    "Mean, In AoA" = mean_inaoa,
    "Std. Dev., In AoA" = sd_inaoa,
    "Mean, Out AoA" = mean_outaoa,
    "Std. Dev., Out AoA" = sd_outaoa,
    # "t-value" = t
  ) %>%
  kable() %>%
  kable_styling()

## Calculate % in AoA by stats ----
df_inv_di %>%
  mutate(here_speed = as.character(here_speed)) %>%
  group_by(
    here_speed
  ) %>%
  summarize(mean_aoa = mean(aoa_brt)) %>%
  arrange(here_speed)

df_inv_di %>%
  mutate(here_paved = as.character(here_paved)) %>%
  group_by(
    here_paved
  ) %>%
  summarize(mean_aoa = mean(aoa_brt)) %>%
  arrange(here_paved)

df_inv_di %>%
  mutate(nlcd_current_class = as.character(nlcd_current_class)) %>%
  group_by(
    nlcd_current_class
  ) %>%
  summarize(mean_aoa = mean(aoa_brt)) %>%
  arrange(nlcd_current_class)

df_inv_di %>%
  group_by(
    basin_true
  ) %>%
  summarize(mean_aoa = mean(aoa_brt)) %>%
  arrange(basin_true) %>%
  kable() %>%
  kable_styling()


# Prepare benefits vs. costs plot ----
fig_costbeni <- 
  df_inv_di %>%
  filter(
    bankfull_width < 15,
    aoa_brt == TRUE
  ) %>%
  ggplot(
    aes(
      x = exp(costpred_brt),
      y = tot_stream_length
    )
  ) +
  geom_point(
    aes(
      color = log(tot_stream_length / exp(costpred_brt))
    ),
    alpha = 0.75
  ) +
  geom_density_2d(color = "black", alpha = 0.3) +
  scale_color_distiller(
    "Benefit - cost ratio",
    palette = "RdBu",
    breaks = c(-3, -13),
    labels = c("High", "Low"),
    guide = guide_colorbar(title.position = "top", ticks = FALSE)
  ) +
  # scale_x_continuous(
  scale_x_log10(
    breaks = c(
      # 0,
      0.25 * mean(exp(df_inv$costpred_brt)),
      0.5 * mean(exp(df_inv$costpred_brt)),
      # 0.75 * mean(exp(df_inv$costpred_brt)),
      mean(exp(df_inv$costpred_brt)),
      # 1.5 * mean(exp(df_inv$costpred_brt)),
      2 * mean(exp(df_inv$costpred_brt)),
      4 * mean(exp(df_inv$costpred_brt)),
      8 * mean(exp(df_inv$costpred_brt))
      # Inf
    ),
    labels = c(
      "0.25 x mean",
      "0.5 x mean",
      # "0.75 x mean",
      "1 x mean",
      # "1.5 x mean",
      "2 x mean",
      "4 x mean",
      "8 x mean"
    )
  ) +
  # scale_y_continuous(
  scale_y_log10(
    breaks = c(
      1,
      10,
      100,
      1000
    ),
    labels = c(
      "1",
      "10",
      "100",
      "1,000"
    )
  ) +
  theme(
    legend.position = c(0.85, 0.1),
    legend.direction = "horizontal",
    aspect.ratio = 1,
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
    # text = element_text(size = 20)
  ) +
  labs(
    # title = str_wrap("Predicted average costs by number of worksites and total distance between worksites (km)", 65),
    # subtitle = str_wrap("Violin plots represent underlying observations"),
    x = "Predicted cost",
    y = "Total upstream distance (m)"
  )

ggsave(
  here("output/figs/fig_costbeni.png"),
  fig_costbeni,
  height = 5,
  width = 6.5
)

# Update prediction results with AoA ----
## Load borders ----
sf_us <- raster::getData("GADM", country = "USA", download = TRUE, path = here("output"), level = 1) %>% st_as_sf() %>% filter(NAME_1 %in% c('California', 'Nevada', 'Utah', 'Wyoming', 'Montana', 'Idaho', 'Oregon', 'Washington'))
sf_canada <- raster::getData("GADM", country = "CAN", download = TRUE, path = here("output"), level = 1) %>% st_as_sf() %>% filter(NAME_1 %in% c("British Columbia", "Alberta"))
sf_base <- 
  rbind(sf_us, sf_canada)

## Make inventory data mappable ----
sf_inv <-
  df_inv_di %>%
  mutate(
    latlong = str_sub(geometry, 3, nchar(geometry)-1),
    long = as.numeric(stringr::word(latlong, 1, sep = ", ")),
    lat = as.numeric(stringr::word(latlong, 2, sep = ", ")),
    latlong = NULL,
    geometry = NULL
  ) %>%
  st_as_sf(coords = c("long", "lat")) %>% st_set_crs("WGS84") %>%
  st_transform(st_crs(sf_base))

## Load basin borders ----
if(file.exists(here("output/wdb/WBD_National_GDB.zip"))) {
  sf_basin <-
    nhdplusTools::download_wbd(here("output/wdb")) %>% st_read(layer = "WBDHU6", quiet = TRUE)
} else {
  sf_basin <-
    st_read(here("output/wdb/WBD_National_GDB.gdb"), layer = "WBDHU6", quiet = TRUE)
}

## Set theme ----
theme_map_custom <- function() {
  theme_bw() %+replace%
    theme(
      panel.background = element_rect(fill = "aliceblue", size = 1),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "right",
      legend.background = element_rect(color = NA)
    )
}

## Plot barrier-level estimates ----
xmin_full = -126
xmax_full = -117
ymin_full = 41.5
ymax_full = 49.5

map_full_preds_base <-
  ggplot() +
  geom_sf(data = sf_base, fill = "antiquewhite1", color = "black") +
  geom_sf(
    aes(
      color = exp(costpred_brt) %>% percent_rank(),
      # color = ownership
    ),
    data = sf_inv %>% filter(aoa_brt == TRUE),
    size = 1,
    # color = "grey60"
  ) +
  geom_sf(data = sf_basin, fill = NA, color = "red") +
  # scale_color_manual(
  #   "Ownership entity",
  #   values = c("State" = "grey40", "County" = "grey60", "Other" = "grey80"), 
  #   breaks = c("County", "State", "Other"), 
  #   guide = guide_legend(override.aes = list(size = 3))
  # ) +
  # legendMap::scale_bar(
  #   lon = xmin_full + 0.4,
  #   lat = ymin_full + 0.25,
  #   distance_lon = 50,
  #   distance_lat = 15,
  #   distance_legend = -10,
  #   legend_size = 2,
  #   # orientation = FALSE,
  #   arrow_length = 100, arrow_distance = 40, arrow_north_size = 5
  # ) +
  scale_color_fermenter(
    "Predicted cost\npercentile",
    palette = "RdBu", 
    direction = -1, 
    breaks = scales::extended_breaks(7),
    limits = c(0.01, 0.99),
    labels = scales::percent_format(1),
    # breaks = 
    #   c(0, seq(9, 12, 1), Inf),
    # quantile(
    #   sf_inv_preds$costpred_ols, 
    #   probs = seq(0, 1, 0.2),
    #   na.rm = TRUE
    # ),
    # labels = function(x) exp(x) %>% dollar(),
    na.value = NA,
    # guide = guide_colorsteps(barwidth = unit(20, "pt"), barheight = unit(90, "pt"))
    guide = guide_colorsteps(barheight = unit(20, "pt"), barwidth = unit(120, "pt"))
  ) +
  # scale_color_distiller(
  #   "Predicted cost\npercentile",
  #   # palette = "YlGn",
  #   # palette = "Spectral",
  #   palette = "RdYlGn",
  #   # limits = c(2000, 650000),
  #   # labels = dollar_format(accuracy = 1),
  #   breaks = quantile(exp(sf_inv_preds$costpred_brt), probs = c(0.05, seq(0, 1, 0.2), 0.95)),
  #   limits = c(min(exp(sf_inv_preds$costpred_brt)), max(exp(sf_inv_preds$costpred_brt))),
  #   direction = -1,
  #   trans = "log10",
  #   na.value = "grey70",
  #   # guide = guide_none()
  #   guide = guide_colorbar(barwidth = unit(20, "pt"), barheight = unit(90, "pt"))
  # ) +
  coord_sf(
    xlim = c(xmin_full, xmax_full),
    ylim = c(ymin_full, ymax_full),
    expand = FALSE
  ) +
  theme_map_custom() +
  theme(
    # legend.position = c(0.97, 0.03), legend.justification = c(1, 0),
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.background = element_rect(color = "white"), 
    legend.key.size = unit(0.3, "pt"), 
    # legend.title = element_text(size = 8),
    # legend.text = element_text(size = 6)
  )

## Plot HUC10 level summaries ----
if(!file.exists(here("data/WBD_17_HU2_Shape/Shape/WBDHU8.shp"))) {
  download.file("https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/WBD/HU2/Shape/WBD_17_HU2_Shape.zip", here("data/WBD_17_HU2_Shape.zip"))
  unzip(here("data/WBD_17_HU2_Shape.zip"), exdir = here("data/WBD_17_HU2_Shape/"))
}
sf_huc10 <- st_read(here("data/WBD_17_HU2_Shape/Shape/WBDHU10.shp"))

sf_inv_preds_huc10 <-
  sf_inv %>%
  mutate(
    # huc12 = as.character(huc12),
    huc10 = trunc(huc12/1e2) %>% as.character()
  ) %>%
  st_drop_geometry() %>%
  group_by(huc10) %>%
  summarize(
    costpred_brt_mean = mean(exp(costpred_brt), na.rm = TRUE),
    costpred_brt_coefvar = sd(exp(costpred_brt), na.rm = TRUE)/mean(exp(costpred_brt), na.rm = TRUE),
    costpred_brt_aoa = mean(aoa_brt)
  ) %>%
  mutate(
    costpred_brt_mean_q = percent_rank(costpred_brt_mean),
    costpred_brt_mean_std = costpred_brt_mean / max(costpred_brt_mean)
  ) %>%
  right_join(sf_huc10, by = "huc10")

map_full_preds_mean <-
  ggplot() +
  geom_sf(data = sf_base, fill = "antiquewhite1", color = "black") +
  geom_sf(
    aes(
      fill = costpred_brt_mean_q
    ),
    color = NA,
    data = sf_inv_preds_huc10 %>% st_as_sf() %>% st_transform(st_crs(sf_base)),
    alpha = 0.75
  ) +
  geom_sf(data = sf_basin, fill = NA, color = "red") +
  # scale_bar(
  #   lon = xmin_full + 0.4,
  #   lat = ymin_full + 0.25,
  #   distance_lon = 50,
  #   distance_lat = 15,
  #   distance_legend = -10,
  #   legend_size = 3,
  #   # orientation = FALSE,
  #   arrow_length = 100, arrow_distance = 40, arrow_north_size = 5
  # ) +
  theme_map_custom() +
  theme(
    # legend.position = c(0.97, 0.03), legend.justification = c(1, 0),
    legend.direction = "horizontal",
    legend.background = element_rect(color = "white"), 
    legend.key.size = unit(0.3, "pt"),
    legend.box = "horizontal"
    # legend.title = element_text(size = 8),
    # legend.text = element_text(size = 6)
  ) + 
  coord_sf(
    xlim = c(xmin_full, xmax_full),
    ylim = c(ymin_full, ymax_full),
    expand = FALSE
  ) + 
  scale_fill_fermenter(
    "Predicted cost\npercentile",
    palette = "RdBu", 
    direction = -1, 
    breaks = scales::extended_breaks(7),
    limits = c(0.01, 0.99),
    labels = scales::percent_format(1),
    # breaks = 
    #   c(0, seq(9, 12, 1), Inf),
    # quantile(
    #   sf_inv_preds$costpred_ols, 
    #   probs = seq(0, 1, 0.2),
    #   na.rm = TRUE
    # ),
    # labels = function(x) exp(x) %>% dollar(),
    na.value = NA,
    # guide = guide_colorsteps(barwidth = unit(20, "pt"), barheight = unit(90, "pt"))
    guide = guide_colorsteps(barheight = unit(20, "pt"), barwidth = unit(120, "pt"))
  )

map_full_preds_var <-
  ggplot() +
  geom_sf(data = sf_base, fill = "antiquewhite1", color = "black") +
  geom_sf(
    aes(
      fill = costpred_brt_coefvar
    ),
    color = NA,
    data = sf_inv_preds_huc10 %>% st_as_sf() %>% st_transform(st_crs(sf_base)),
    alpha = 0.75
  ) +
  geom_sf(data = sf_basin, fill = NA, color = "red") +
  # scale_bar(
  #   lon = xmin_full + 0.4,
  #   lat = ymin_full + 0.25,
  #   distance_lon = 50,
  #   distance_lat = 15,
  #   distance_legend = -10,
  #   legend_size = 3,
  #   # orientation = FALSE,
  #   arrow_length = 100, arrow_distance = 40, arrow_north_size = 5
  # ) +
  theme_map_custom() +
  theme(
    # legend.position = c(0.97, 0.03), legend.justification = c(1, 0),
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.background = element_rect(color = "white"), 
    legend.key.size = unit(0.3, "pt"), 
    # legend.title = element_text(size = 8),
    # legend.text = element_text(size = 6)
  ) + 
  coord_sf(
    xlim = c(xmin_full, xmax_full),
    ylim = c(ymin_full, ymax_full),
    expand = FALSE
  )  + 
  scale_fill_fermenter(
    "Predicted cost\nvariability",
    palette = "PuOr", 
    direction = -1, 
    breaks = scales::extended_breaks(7),
    limits = c(0.01, 0.99),
    # breaks = 
    #   c(0, seq(9, 12, 1), Inf),
    # quantile(
    #   sf_inv_preds$costpred_ols, 
    #   probs = seq(0, 1, 0.2),
    #   na.rm = TRUE
    # ),
    # labels = function(x) exp(x) %>% dollar(),
    na.value = NA,
    # guide = guide_colorsteps(barwidth = unit(20, "pt"), barheight = unit(90, "pt"))
    guide = guide_colorsteps(barheight = unit(20, "pt"), barwidth = unit(120, "pt"))
  )

ggsave(
  here("output/figs/fig_culvpreds_composite.png"),
  ((map_full_preds_base | (map_full_preds_mean / map_full_preds_var)) + plot_layout(widths = c(2, 1))) / guide_area() + plot_annotation(tag_levels = 'a') + plot_layout(guides = "collect", heights = c(6, 1)) & theme(legend.box = "horizontal"),
  height = 7,
  width = 6.5
)

## Plot maps of barrier-level AoA ----
map_full_preds_aoa <-
  ggplot() +
  geom_sf(data = sf_base, fill = "antiquewhite1", color = "black") +
  geom_sf(
    aes(
      # color = di_brt,
      color = "Yes"
    ),
    data = sf_inv %>% filter(aoa_brt == TRUE),
    size = 0.3
  ) +
  geom_sf(
    aes(
      # color = aoa_brt,
      color = "No"
    ),
    data = sf_inv %>% filter(aoa_brt == FALSE),
    size = 0.5
  ) +
  geom_sf(data = sf_basin, fill = NA, color = "red") +
  # scale_color_fermenter(
  #   "Dissimilarity index",
  #   palette = "Reds", 
  #   direction = 1, 
  #   # breaks = scales::extended_breaks(7),
  #   # limits = c(0.01, 0.99),
  #   # breaks = 
  #   #   c(0, seq(9, 12, 1), Inf),
  #   # quantile(
  #   #   sf_inv_preds$costpred_ols, 
  #   #   probs = seq(0, 1, 0.2),
  #   #   na.rm = TRUE
  #   # ),
  #   # labels = function(x) exp(x) %>% dollar(),
  #   na.value = NA,
  #   # guide = guide_colorsteps(barwidth = unit(20, "pt"), barheight = unit(90, "pt"))
  #   guide = guide_colorsteps(barheight = unit(20, "pt"), barwidth = unit(120, "pt"))
  # ) +
  scale_color_manual(
    "Within Area of Applicability", 
    values = c("Yes" = "#d9d9d9", "No" = "#fb6a4a"),
    guide_legend(keywidth = unit(50, "pt"), keyheight = unit(10, "pt"))
  ) +
  coord_sf(
    xlim = c(xmin_full, xmax_full),
    ylim = c(ymin_full, ymax_full),
    expand = FALSE
  ) +
  theme_map_custom() +
  theme(
    # legend.position = c(0.97, 0.03), legend.justification = c(1, 0),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.background = element_rect(color = "white"), 
    legend.key.size = unit(0.3, "pt"), 
    # legend.title = element_text(size = 8),
    # legend.text = element_text(size = 6)
  )

ggsave(
  here("output/figs/fig_culvpreds_aoa.png"),
  map_full_preds_aoa,
  height = 7.5,
  width = 6.5
)

map_full_preds_aoahuc10 <-
  ggplot() +
  geom_sf(data = sf_base, fill = "antiquewhite1", color = "black") +
  geom_sf(
    aes(
      fill = costpred_brt_aoa
    ),
    color = NA,
    data = sf_inv_preds_huc10 %>% st_as_sf() %>% st_transform(st_crs(sf_base)),
    alpha = 0.75
  ) +
  geom_sf(data = sf_basin, fill = NA, color = "red") +
  # scale_bar(
  #   lon = xmin_full + 0.4,
  #   lat = ymin_full + 0.25,
  #   distance_lon = 50,
  #   distance_lat = 15,
  #   distance_legend = -10,
  #   legend_size = 3,
  #   # orientation = FALSE,
  #   arrow_length = 100, arrow_distance = 40, arrow_north_size = 5
  # ) +
  theme_map_custom() +
  theme(
    # legend.position = c(0.97, 0.03), legend.justification = c(1, 0),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.background = element_rect(color = "white"), 
    legend.key.size = unit(0.3, "pt"),
    legend.box = "horizontal"
    # legend.title = element_text(size = 8),
    # legend.text = element_text(size = 6)
  ) + 
  coord_sf(
    xlim = c(xmin_full, xmax_full),
    ylim = c(ymin_full, ymax_full),
    expand = FALSE
  ) + 
  scale_fill_fermenter(
    "Percent barriers in AoA",
    palette = "Reds", 
    direction = -1, 
    breaks = scales::extended_breaks(7),
    limits = c(0.01, 0.99),
    labels = scales::percent_format(1),
    # breaks = 
    #   c(0, seq(9, 12, 1), Inf),
    # quantile(
    #   sf_inv_preds$costpred_ols, 
    #   probs = seq(0, 1, 0.2),
    #   na.rm = TRUE
    # ),
    # labels = function(x) exp(x) %>% dollar(),
    na.value = NA,
    # guide = guide_colorsteps(barwidth = unit(20, "pt"), barheight = unit(90, "pt"))
    guide = guide_colorsteps(
      barheight = unit(10, "pt"),
      barwidth = unit(120, "pt")
    )
  )

ggsave(
  here("output/figs/fig_culvpreds_aoahuc10.png"),
  map_full_preds_aoahuc10,
  height = 7.5,
  width = 6.5
)

ggsave(
  here("output/figs/fig_culvpreds_aoacomposite.png"),
  (map_full_preds_aoa | map_full_preds_aoahuc10) / guide_area() + plot_annotation(tag_levels = 'a') + 
    plot_layout(guides = "collect", heights = c(9, 1), widths = c(1, 1)) & 
    theme(legend.box = "horizontal", legend.justification = "left", legend.box.just = "left"),
  height = 4,
  width = 6.5
)


