# TITLE: Descriptive figures for pure culvert projects
# AUTHOR: Braeden Van Deynze
# DATE: March, 2020
# INPUTS: "culverts_*_working.csv" data for pure culvert project data at different levels of aggregation
# OUTPUTS: figures in "output/figs/culverts/" folders

# Prepeare environment ====
# Clear environment
rm(list = ls())

# Load libraries
library(scales)
library(searchable)
library(tidyverse)
library(ggthemes)
library(janitor)
library(here)

# Build custom functions
# This one un-factors years (or other numerically labelled factors) while retaining their numeric values
unfactor <-
  function(x) {
    as.numeric(as.character(x))
  }


# Set ggplot2 themes
theme_set(theme_clean())
theme_update(
  plot.background = element_rect(color = NA),
  plot.title.position = "plot"
) # Removes ugly black border of plot

# Load data ====
df_culv_pure <- read_csv(here("/output/culverts_full_working.csv"))
df_culv_wrk_pure <- read_csv(here("/output/culverts_wrk_working.csv"))
df_culv_prj_pure <- read_csv(here("/output/culverts_prj_working.csv"))

# Build base figures and keys ====
base_countcost_bar <-
  ggplot(
    df_culv_prj_pure
  ) +
  geom_col(    
    aes(
      y = n, 
      fill = cost_avail, 
      group = cost_avail
    )
  ) +
  # stat_summary(
  #   aes(
  #     y = n,
  #     label = comma(after_stat(y)),
  #     hjust = "left"
  #   ),
  #   geom = "text",
  #   fun = "sum"
  # ) +
  scale_x_discrete(
    name = NULL
  ) +
  scale_y_continuous(
    name = "Count",
    labels = label_comma(),
    position = "right"
  ) +
  scale_fill_brewer(
    "Costs Reported", 
    palette = "Greens",
    labels = c("No", "Yes")
  ) +
  theme(
    legend.position = c(0.85, 0.1)
  ) +
  coord_flip()

base_countcost_line <-
  ggplot(
    df_culv_prj_pure,
    aes(
      x = completed_year,
      y = n,
      fill = cost_avail,
      group = cost_avail
    )
  ) +
  geom_vline(
    xintercept = seq(1990, 2015, 5),
    alpha = 0.5,
    linetype = "dashed"
  ) +
  geom_area() +
  scale_x_continuous(
    name = NULL
  ) +
  scale_y_continuous(
    name = "Count",
    labels = label_comma()
  ) +
  scale_fill_brewer(
    "Costs Reported", 
    palette = "Greens",
    labels = c("No", "Yes")
  ) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 0,
      vjust = 0.5
    ),
    legend.position = c(0.85, 0.90)
  )

base_cost_bar <-
  ggplot(
    df_culv_prj_pure,
    aes(
      y = adj_cost/1000
    )
  ) +
  geom_col(
    aes(
      fill = -log(adj_cost)
    )
  ) +
  scale_x_discrete(
    name = NULL
  ) +
  scale_y_continuous(
    name = NULL,
    labels = label_comma(),
    position = "right"
  ) +
  scale_fill_distiller(palette = "YlGn") +
  theme(
    legend.position = "none"
  ) +
  coord_flip()

base_cost_line <-
  ggplot(
    df_culv_prj_pure,
    aes(
      x = completed_year,
      y = adj_cost/1000
    )
  ) +
  geom_vline(
    xintercept = seq(1990, 2015, 5),
    alpha = 0.5,
    linetype = "dashed"
  ) +
  geom_area(
    fill = "#A1D99B"
  ) +
  scale_x_continuous(
    name = NULL
  ) +
  scale_y_continuous(
    name = NULL,
    labels = label_comma()
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5)
  )

base_costdist_violin <-
  ggplot(
    df_culv_prj_pure,
    aes(
      y = cost_per_culvert
    )
  ) +
  geom_violin(
    alpha = 0.6
  ) +
  geom_boxplot(
    width = 0.1,
    fill = "white"
  ) +
  # geom_jitter(
  #   aes(
  #     size = n_culverts
  #   ),
  #   stroke = 1,
  #   fill = "white",
  #   shape = 21,
  #   position = position_jitter(0.2)
  # ) +
  # stat_summary(
  #   fun = mean,
  #   shape = "cross",
  #   stroke = 1.5,
  #   geom = "point"
  # ) +
  # coord_flip() +
  scale_fill_brewer(
    palette = "Greens",
    guide = NULL
  ) +
  scale_y_log10(
    "Cost per Culvert",
    labels = label_dollar()
  ) +
  scale_x_discrete(
    name = NULL
  ) +
  theme(
    legend.position = "none"
  )

# Set up ordered levels and action key
action_key <-
  c(
    "FISH PASSAGE - CULVERT IMPROVEMENTS OR UPGRADES" = "fishpass_culvimp",
    "FISH PASSAGE - CULVERT INSTALLATION" = "fishpass_culvinst",
    "FISH PASSAGE - CULVERT REMOVAL" = "fishpass_culvrem"
  )

basin_levels <-
  df_culv_pure %>%
  tabyl(basin) %>%
  arrange(-n) %>%
  pull(basin)

source_levels <-
  df_culv_pure %>%
  tabyl(project_source) %>%
  arrange(-n) %>%
  pull(project_source)

action_levels <-
  df_culv_pure %>%
  mutate(action = recode(action, !!!invert(action_key))) %>%
  tabyl(action) %>%
  arrange(n) %>%
  pull(action)

basin_costdist_levels <-
  df_culv_prj_pure %>%
  filter(basin %in% basin_levels[1:8]) %>%
  group_by(basin) %>%
  summarize(cost_per_culvert = median(cost_per_culvert, na.rm = TRUE)) %>%
  arrange(-cost_per_culvert) %>%
  pull(basin)

source_costdist_levels <-
  df_culv_prj_pure %>%
  filter(project_source %in% source_levels[1:6]) %>%
  group_by(project_source) %>%
  summarize(cost_per_culvert = median(cost_per_culvert, na.rm = TRUE)) %>%
  arrange(-cost_per_culvert) %>%
  pull(project_source)

action_costdist_levels <-
  df_culv_pure %>%
  group_by(action = recode(action, !!!invert(action_key))) %>%
  summarize(cost_per_culvert = median(cost_per_culvert, na.rm = TRUE)) %>%
  arrange(-cost_per_culvert) %>%
  pull(action)

# Action counts w/ costs ====
# By year
(
  fig_countcost_year <-
    base_countcost_line %+%
    (df_culv_pure %>%
       group_by(
         completed_year = ordered(completed_year),
         cost_avail = factor(cost_avail),
         .drop = FALSE
       ) %>%
       count() %>%
       ungroup() %>%
       mutate(
         completed_year = unfactor(completed_year)
       )
    ) +
    ggtitle("Culvert action count, by year")
)

# By action type
(
  fig_countcost_action <-
    base_countcost_bar %+%
    (df_culv_pure %>%
       group_by(
         action = recode(action, !!!invert(action_key)) %>% ordered(action_levels),
         cost_avail = factor(cost_avail)
       ) %>%
       count() %>%
       drop_na()
    ) +
    aes(
      x = action
    ) +
    ggtitle("Culvert action count, by action type")
)

# By basin
(
  fig_countcost_basin <-
    base_countcost_bar %+%
    (df_culv_pure %>%
       group_by(
         basin = ordered(basin, basin_levels) %>% fct_explicit_na(),
         cost_avail = factor(cost_avail)
       ) %>%
       count() %>%
       drop_na()
    ) +
    aes(
      x = fct_rev(basin)
    ) +
    ggtitle("Culvert action count, by basin")
)

# By source
(
  fig_countcost_source <-
    base_countcost_bar %+%
    (df_culv_pure %>%
       group_by(
         project_source = ordered(project_source, source_levels),
         cost_avail = factor(cost_avail)
       ) %>%
       count() %>%
       drop_na()
    ) +
    aes(
      x = fct_rev(project_source)
    ) +
    ggtitle("Culvert action count, by reporting source")
)

# By action and year
(
  fig_countcost_action_year <-
    base_countcost_line %+%
    (df_culv_pure %>%
       group_by(
         completed_year = ordered(completed_year),
         action = recode(action, !!!invert(action_key)) %>% ordered(action_levels),
         cost_avail = factor(cost_avail),
         .drop = FALSE
       ) %>%
       count() %>%
       ungroup() %>%
       mutate(
         completed_year = unfactor(completed_year)
       )
    ) +
    ggtitle("Culvert action count, by action type and year") +
    facet_wrap(~ action, ncol = 1)
)

# By basin and year
(
  fig_countcost_basin_year <-
    base_countcost_line %+%
    (df_culv_pure %>%
       group_by(
         completed_year = ordered(completed_year),
         basin = ordered(basin, basin_levels) %>% fct_explicit_na(),
         cost_avail = factor(cost_avail),
         .drop = FALSE
       ) %>%
       count() %>%
       ungroup() %>%
       mutate(
         completed_year = unfactor(completed_year)
       ) %>%
       filter(
         basin %in% basin_levels[1:8]
       )
    ) +
    ggtitle("Culvert action count, by basin and year", "8 basins (of 27) with most actions") +
    facet_wrap(~ basin, ncol = 2) +
    theme(
      legend.position = "bottom" 
    )
)
  
# By source and year
(
  fig_countcost_source_year <-
    base_countcost_line %+%
    (df_culv_pure %>%
       group_by(
         completed_year = ordered(completed_year),
         project_source = ordered(project_source, source_levels),
         cost_avail = factor(cost_avail),
         .drop = FALSE
       ) %>%
       count() %>%
       ungroup() %>%
       mutate(
         completed_year = unfactor(completed_year)
       ) %>%
       filter(
         project_source %in% source_levels[1:6]
       )
    ) +
    ggtitle("Culvert action count, by reporting source and year", "6 sources (of 20) with most actions") +
    facet_wrap(~ project_source, ncol = 2) +
    theme(legend.position = "bottom")
)

# By basin and action
fig_countcost_basin_action <-
  base_countcost_bar %+%
  (df_culv_pure %>%
     group_by(
       action = recode(action, !!!invert(action_key)) %>% ordered(action_levels),
       basin = ordered(basin, basin_levels) %>% fct_explicit_na(),
       cost_avail = factor(cost_avail)
     ) %>%
     count() %>%
     filter(
       basin %in% basin_levels[1:8]
     )
  ) +
  aes(
    x = fct_rev(basin)
  ) +
  ggtitle("Culvert action count, by basin and action type", "8 basins (of 47) with most actions") +
  facet_wrap(~ action, ncol = 1)
fig_countcost_basin_action

# By basin and source
fig_countcost_basin_source <-
  base_countcost_bar %+%
  (df_culv_pure %>%
     group_by(
       project_source = ordered(project_source, source_levels),
       basin = ordered(basin, basin_levels) %>% fct_explicit_na(),
       cost_avail = factor(cost_avail)
     ) %>%
     count() %>%
     filter(
       project_source %in% source_levels[1:6]
     ) %>%
     filter(
       basin %in% basin_levels[1:8]
     )
  ) +
  aes(
    x = fct_rev(basin)
  ) +
  ggtitle("Culvert action count, by basin and reporting source", "8 basins (of 47) and 6 sources (of 38) with most actions") +
  facet_wrap(~ project_source, ncol = 2)
fig_countcost_basin_source

# By source and action
fig_countcost_source_action <-
  base_countcost_bar %+%
  (df_culv_pure %>%
     group_by(
       action = recode(action, !!!invert(action_key)) %>% ordered(action_levels),
       project_source = ordered(project_source, source_levels),
       cost_avail = factor(cost_avail)
     ) %>%
     count() %>%
     filter(
       project_source %in% source_levels[1:6]
     )
  ) +
  aes(
    x = fct_rev(project_source)
  ) +
  ggtitle("Culvert action count, by reporting source and action type", "6 sources (of 38) with most actions") +
  facet_wrap(~ action, ncol = 1)
fig_countcost_source_action

# Project count w/ costs ====
# By year
fig_prjcost_year <-
  base_countcost_line %+%
  (df_culv_prj_pure %>%
     group_by(
       completed_year = ordered(completed_year),
       cost_avail = factor(cost_avail),
       .drop = FALSE
     ) %>%
     count() %>%
     ungroup() %>%
     mutate(
       completed_year = unfactor(completed_year)
     )
  ) +
  ggtitle("Culvert project count, by year")
fig_prjcost_year

# By basin
fig_prjcost_basin <-
  base_countcost_bar %+%
  (df_culv_prj_pure %>%
     group_by(
       basin = ordered(basin, basin_levels) %>% fct_explicit_na(),
       cost_avail = factor(cost_avail)
     ) %>%
     count() %>%
     drop_na()
  ) +
  aes(
    x = fct_rev(basin)
  ) +
  ggtitle("Culvert project count, by basin")
fig_prjcost_basin

# By source
fig_prjcost_source <-
  base_countcost_bar %+%
  (df_culv_prj_pure %>%
     group_by(
       project_source = ordered(project_source, source_levels),
       cost_avail = factor(cost_avail)
     ) %>%
     count() %>%
     drop_na()
  ) +
  aes(
    x = fct_rev(project_source)
  ) +
  ggtitle("Culvert project count, by source")
fig_prjcost_source

# By source and year
fig_prjcost_source_year <-
  base_countcost_line %+%
  (df_culv_prj_pure %>%
     group_by(
       completed_year = ordered(completed_year),
       project_source = ordered(project_source, source_levels),
       cost_avail = factor(cost_avail),
       .drop = FALSE
     ) %>%
     count() %>%
     ungroup() %>%
     mutate(
       completed_year = unfactor(completed_year)
     ) %>%
     filter(
       project_source %in% source_levels[1:6]
     )
  ) +
  ggtitle("Culvert project count, by source and year", "6 sources (of 20) with most actions") +
  facet_wrap(~ project_source, ncol = 2)
fig_prjcost_source_year

# By basin and year
fig_prjcost_basin_year <-
  base_countcost_line %+%
  (df_culv_prj_pure %>%
     group_by(
       completed_year = ordered(completed_year),
       basin = ordered(basin, basin_levels) %>% fct_explicit_na(),
       cost_avail = factor(cost_avail),
       .drop = FALSE
     ) %>%
     count() %>%
     ungroup() %>%
     mutate(
       completed_year = unfactor(completed_year)
     ) %>%
     filter(
       basin %in% basin_levels[1:8]
     )
  ) +
  ggtitle("Culvert project count, by basin and year", "8 basins (of 27) with most actions") +
  facet_wrap(~ basin, ncol = 2)
fig_prjcost_basin_year

# By basin and funding
fig_prjcost_basin_source <-
  base_countcost_bar %+%
  (df_culv_prj_pure %>%
     group_by(
       project_source = ordered(project_source, source_levels),
       basin = ordered(basin, basin_levels) %>% fct_explicit_na(),
       cost_avail = factor(cost_avail)
     ) %>%
     count() %>%
     filter(
       project_source %in% source_levels[1:6]
     ) %>%
     filter(
       basin %in% basin_levels[1:8]
     )
  ) +
  aes(
    x = fct_rev(basin)
  ) +
  ggtitle("Culvert project count, by basin and source", "8 basins (of 47) and 6 sources (of 38) with most actions") +
  facet_wrap(~ project_source, ncol = 2)
fig_prjcost_basin_source


# Total costs ====
# By year
fig_cost_year <-
  base_cost_line %+%
  (df_culv_prj_pure %>%
     group_by(completed_year) %>%
     summarize(adj_cost = sum(adj_cost, na.rm = TRUE))
  ) +
  ggtitle("Expenditures on culverts ($thou), by year")
fig_cost_year

# By basin
fig_cost_basin <-
  base_cost_bar %+%
  (df_culv_prj_pure %>%
     group_by(basin) %>%
     summarize(adj_cost = sum(adj_cost, na.rm = TRUE)) %>%
     drop_na()
  ) +
  aes(
    x = reorder(basin, adj_cost)
  ) +
  ggtitle("Expenditures on culverts ($thou), by basin")
fig_cost_basin

# By source
fig_cost_source <-
  base_cost_bar %+%
  (df_culv_prj_pure %>%
     group_by(project_source) %>%
     summarize(adj_cost = sum(adj_cost, na.rm = TRUE)) %>%
     drop_na()
  ) +
  aes(
    x = reorder(project_source, adj_cost)
  ) +
  ggtitle("Expenditures on culverts ($thou), by reporting source")
fig_cost_source

# By year and basin
fig_cost_year_basin <-
  base_cost_line %+%
  (df_culv_prj_pure %>%
     group_by(completed_year, basin) %>%
     summarize(adj_cost = sum(adj_cost, na.rm = TRUE)) %>%
     drop_na() %>%
     mutate(basin_fac = factor(basin, basin_levels, ordered = TRUE)) %>%
     filter(basin %in% basin_levels[1:8])
  ) + 
  ggtitle("Expenditures on culverts ($thou), by year and basin", "8 (of 47) basins with most projects") +
  facet_wrap(~ basin_fac, ncol = 2)
fig_cost_year_basin

# By year and source
fig_cost_year_source <-
  base_cost_line %+%
  (df_culv_prj_pure %>%
     group_by(completed_year, project_source) %>%
     summarize(adj_cost = sum(adj_cost, na.rm = TRUE)) %>%
     drop_na() %>%
     mutate(source_fac = factor(project_source, source_levels, ordered = TRUE)) %>%
     filter(project_source %in% source_levels[1:6])
  ) + 
  ggtitle("Expenditures on culverts ($thou), by year and reporting source", "6 (of 38) sources with most projects") +
  facet_wrap(~ source_fac, ncol = 2)
fig_cost_year_source

# By basin and source
fig_cost_source_basin <-
  base_cost_bar %+%
  (df_culv_prj_pure %>%
     group_by(project_source, basin) %>%
     summarize(adj_cost = sum(adj_cost, na.rm = TRUE)) %>%
     drop_na() %>%
     mutate(basin_fac = factor(basin, basin_levels, ordered = TRUE)) %>%
     mutate(source_fac = factor(project_source, source_levels, ordered = TRUE)) %>%
     filter(basin %in% basin_levels[1:8]) %>%
     filter(project_source %in% source_levels[1:6])
  ) +
  aes(
    x = reorder(project_source, adj_cost)
  ) +
  facet_wrap(~ basin_fac, ncol = 2) +
  ggtitle("Expenditures on culverts ($thou), by reporting source and basin", "8 (of 47) basins with most projects and \n6 (of 38) sources with most projects")
fig_cost_source_basin

# Cost per culvert distributions ====
# By basin
fig_costdist_action <-
  base_costdist_violin %+%
  (df_culv_pure %>%
     filter(
       cost_avail == TRUE 
     ) %>%
     group_by(action = recode(action, !!!invert(action_key))) %>%
     mutate(
       action_n = paste0(
         action,
         "\n(n = ",
         comma(n()),
         ")"
       )
     )
  ) +
  aes(
    x = reorder(action_n, adj_cost),
    fill = action,
    group = action
  ) +
  ggtitle(
    "Cost per culvert at action level, by action type"
  ) +
  coord_flip()
fig_costdist_action

# By basin
fig_costdist_basin <-
  base_costdist_violin %+%
  (df_culv_prj_pure %>%
     filter(
       cost_avail == TRUE 
     ) %>%
     filter(
       basin %in% basin_levels[1:8]
     ) %>%
     group_by(basin) %>%
     mutate(
       basin_n = paste0(
         basin,
         "\n(n = ",
         comma(n()),
         ")"
       )
     )
  ) +
  aes(
    x = reorder(basin_n, adj_cost),
    fill = basin,
    group = basin
  ) +
  ggtitle(
    "Cost per culvert at project level, by basin"
  ) +
  coord_flip()
fig_costdist_basin

# By source
fig_costdist_source <-
  base_costdist_violin %+%
  (df_culv_prj_pure %>%
     filter(
       cost_avail == TRUE 
     ) %>%
     filter(
       project_source %in% source_levels[1:6]
     ) %>%
     group_by(project_source) %>%
     mutate(
       project_source_n = paste0(
         project_source,
         "\n(n = ",
         comma(n()),
         ")"
       )
     )
  ) +
  aes(
    x = reorder(project_source_n, adj_cost),
    fill = project_source,
    group = project_source
  ) +
  ggtitle(
    "Cost per culvert at project level, by source"
  ) +
  coord_flip()
fig_costdist_source

# By year
fig_costdist_year <-
  base_costdist_violin %+%
  (df_culv_prj_pure %>%
     filter(
       cost_avail == TRUE 
     ) %>%
     group_by(completed_year = ordered(completed_year))
  ) +
  aes(
    x = completed_year,
    fill = completed_year,
    group = completed_year
  ) +
  scale_fill_manual(
    values = colorRampPalette(RColorBrewer::brewer.pal(9, "Greens"))(25),
    guide = NULL
  ) +
  ggtitle(
    "Cost per culvert at project level, by year"
  )
fig_costdist_year

# By both (skipping this cause it's not that informative)
# fig_costdist_basin_source <-
#   base_costdist_violin %+%
#   (df_culv_prj_pure %>%
#      filter(
#        cost_avail == TRUE 
#      ) %>%
#      filter(
#        project_source %in% source_levels[1:6],
#        basin %in% basin_levels[1:8]
#      ) %>%
#      group_by(basin) %>%
#      mutate(
#        basin_n = paste0(
#          basin,
#          "\n(n = ",
#          comma(n()),
#          ")"
#        )
#      )
#   ) +
#   aes(
#     x = project_source,
#     fill = project_source,
#     group = project_source
#   ) +
#   ggtitle(
#     "Cost per culvert at project level, by basin and source"
#   ) +
#   facet_wrap(~ basin_n, ncol = 2)
# fig_costdist_basin_source


# Save figures ====

# List of figures
ls_figs <-
  names(.GlobalEnv) %>% str_subset("^fig_") %>% sort

# Set figure sizes
sz_base <- list(width = 8, height = 6)
sz_small <- list(width = 8, height = 2)

# List of sizes
ls_sz <-
  list(
    sz_base, # fig_cost_*
    sz_base,
    sz_base,
    sz_base,
    sz_base,
    sz_base,
    sz_base, # fig_costdist_*
    sz_base,
    sz_base,
    sz_base,
    sz_small, # fig_countcost_action_*
    sz_base,
    sz_base, # fig_countcost_basin_*
    sz_base,
    sz_base,
    sz_base,
    sz_base, # fig_countcost_source_*
    sz_base,
    sz_base,
    sz_base, # fig_countcost_year
    sz_base, # fig_prjcost_basin_*
    sz_base,
    sz_base,
    sz_base, # fig_prjcost_source_*
    sz_base,
    sz_base  # fig_prjcost_year
  )

purrr::map2(
  ls_figs,
  ls_sz,
  ~ggsave(
    filename = here(paste0("/output/figs/culverts/", .x,".png")),
    plot = get(.x),
    device = "png",
    width = .y["width"] %>% as.numeric,
    height = .y["height"] %>% as.numeric
  )
) %>% invisible
