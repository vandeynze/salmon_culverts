#' ---
#' title: "Interactive Maps for Culvert Replacement Cost Predictions [WORK IN PROGRESS; PLEASE DO NOT SHARE WITHOUT PERMISSION]"
#' author: "B. Van Deynze (w/ R. Fonner, B. Feist, S. Jardine, & D. Holland)"
#' date: '`r format(Sys.Date(), "%B %d, %Y")`'
#' output:
#'    html_document:
#'       number_sections: true
#'       code_folding: hide
#'       toc: false
#' ---
#' 

#+ include=FALSE
# Prepare environment and data ----
library(tidyverse)
library(sf)
library(ggmap)
library(janitor)
library(here)
library(RColorBrewer)
library(readxl)
library(leaflet)
library(gbm)


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

# Load inventories

# sf_allculv_odfw <- st_read(here("data/culv_inventories/ODFW_44_5_ofpbds_gdb/ofpbds_gdb.gdb"), layer = "ofpbds_pt") %>% clean_names() %>% st_transform(st_crs(sf_inv_preds))

sf_allculv_wdfw <- st_read(here("data/culv_inventories/WdfwFishPassage/WdfwFishPassage.gdb"), layer = "WDFW_FishPassageSite")
sf_allculv_wdfw %>% clean_names() %>% 
  filter(
    feature_type == "Culvert"
  ) %>% select(site_record_id:feature_type) %>% st_drop_geometry() %>% write_csv(here("output/allculvs/culvinventory_wdfw.csv"))
# Check projection
st_crs(sf_allculv_wdfw)
sf_allculv_wdfw <-
  sf_allculv_wdfw %>% clean_names() %>% st_transform(4326) %>%
  filter(
    feature_type == "Culvert",
    fish_passage_barrier_status_code == 10
  )

sf_allculv_odfw <- st_read(here("data/culv_inventories/ODFW_44_5_ofpbds_gdb/ofpbds_gdb.gdb"), layer = "ofpbds_pt")
sf_allculv_odfw %>% 
  clean_names() %>%
  filter(
    fpb_ftr_ty == "Culvert",
  ) %>% select(fpb_ftr_id:fpb_o_site_id) %>% st_drop_geometry() %>% write_csv(here("output/allculvs/culvinventory_odfw.csv"))
sf_allculv_odfw <-
  sf_allculv_odfw %>% clean_names() %>% st_transform(4326) %>%
  filter(
    fpb_ftr_ty == "Culvert",
    fpb_f_pas_sta %in% c("Blocked", "Partial")
  )

sf_allculv_odfw <-
  sf_allculv_odfw %>%
  mutate(Ownership = case_when(
    fpb_own_ty == "County" ~ "County",
    fpb_own_ty == "State" ~ "State",
    TRUE ~ "Other"
  ))
sf_allculv_wdfw <-
  sf_allculv_wdfw %>%
  mutate(Ownership = case_when(
    owner_type_code == 2 ~ "County",
    owner_type_code == 5 ~ "State",
    TRUE ~ "Other"
  ))

sf_inv_preds <-
  here("output/inv_preds.csv") %>% read_csv(guess_max = 30000) %>%
  right_join(
    sf_allculv_wdfw,
    by = "site_id",
    suffix = c("", "_wdfw")
  ) %>%
  # right_join(
  #   sf_allculv_odfw,
  #   by = "fpb_ftr_id",
  #   suffix = c("", "_odfw")
  # ) %>%
  # filter(!(is.na(site_id) & fpb_ftr_id == 0)) %>%
  mutate(geom = gsub(geometry,pattern = "(\\))|(\\()|c", replacement = "")) %>%
  tidyr::separate(geom, into = c("lat", "lon"), sep = ",") %>%
  st_as_sf(., coords = c("lat", "lon"), crs = 4326)



# Try leaflet ---- 

#' Below is a map of culverts identified in the Washington
#' Department of Fish and Wildlife's Fish Passage Barrier Inventory as 
#' of January 2021. Each barrier has been assigned to a "Cost Percentile" 
#' representing the predicted cost of correcting the barrier relative to others 
#' in the database. Predictions are based boosted regression trees fit on 1,235 barrier
#' correction projects documented in the Pacific Northwest Salmonid Habitat Projects database.
#' Detailed methods are available in a manuscript currently under peer review and available 
#' upon request.


pal <- 
  colorQuantile(
    palette = "RdBu",
    domain = sf_inv_preds$costpred_brt,
    n = 10,
    reverse = TRUE
  )

leaflet(sf_inv_preds, width = "100%") %>%
  addTiles %>% # Add default OpenStreetMap map tiles
  addCircleMarkers(
    radius = 5,
    weight = 1.5, 
    color = "black", 
    opacity = 1, 
    fillOpacity = 1, 
    fillColor = ~ pal(costpred_brt), 
    clusterOptions = markerClusterOptions(spiderfyOnMaxZoom = FALSE, disableClusteringAtZoom = 10),
    popup = 
      ~ paste0(
        "<b>WDFW Site ID:</b> ", 
        "<a href='http://apps.wdfw.wa.gov/fishpassagephotos/Reports/", site_id, "_Report.pdf' target = '_blank'>",
        site_id,
        "</a><br>", 
        "<b>Cost Percentile:</b> ", match(pal(costpred_brt), rev(brewer.pal(10, "RdBu")))*10, "%",
        "<br>",
        "<b>Passability:</b> ", case_when(percent_fish_passable_code == 99 ~ "Unknown", 
                                              percent_fish_passable_code == 10 ~ "0%", 
                                              percent_fish_passable_code == 20 ~ "33%", 
                                              percent_fish_passable_code == 30 ~ "66%", 
                                          ),
        "<br>",
        "<b>Potential Species:</b>", case_when(is.na(potential_species) ~ " ",
                                               TRUE ~ ""), 
        potential_species,
        "<br>",
        "<b>Lineal Gain:</b> ", scales::comma(lineal_gain_measurement, 1), case_when(is.na(lineal_gain_measurement) ~ "",
                                                                                     TRUE ~ "m"),
        "<br>",
        "<b>Ownership:</b> ", case_when(owner_type_code == 1 ~ "City", 
                                        owner_type_code == 2 ~ "County", 
                                        owner_type_code == 3 ~ "Federal", 
                                        owner_type_code == 4 ~ "Private",
                                        owner_type_code == 5 ~ "State", 
                                        owner_type_code == 6 ~ "Tribal", 
                                        owner_type_code == 7 ~ "Other", 
                                        owner_type_code == 8 ~ "Port",
                                        owner_type_code == 9 ~ "Drainage District", 
                                        owner_type_code == 11 ~ "Irrigation District", 
                                        owner_type_code == 12 ~ "Unknown", 
                                        # 1 = "city", 2 = "county", 3 = "federal", 4 = "private", 5 = "state", 6 = "tribal", 7 = "other", 8 = "port", 9 = "drainage district, 11 = "irrigation district", 12 = "unknown"
        ),
        "<br>",
        "<b>Survey Date:</b> ", survey_date,
        ""
      )
  ) %>%
  addLegend(colors = rev(brewer.pal(10, "RdBu")), labels = paste0(c(1:10)*10, "%"), opacity = 1, title = "Cost Percentile") %>%
  # addPopups %>%
  setView(lng = -122, lat = 47.5, zoom = 7)

#+ fig.height=20, out.width='100%'
# Present regional and ownership summaries ----
# __Plot densities ----

sf_inv_preds %>% 
  st_drop_geometry() %>% 
  # filter(Ownership == "County") %>% 
  mutate(
    value = "county"
  ) %>%
  select(
    county_name,
    value,
    costpred_brt
  ) %>%
  bind_rows(
    sf_inv_preds %>% 
      st_drop_geometry() %>% 
      select(
        county_name,
        # value,
        costpred_brt
      ) %>%
      expand_grid(county_name2 = unique(sf_inv_preds$county_name)) %>%
      mutate(
        value = "all",
        county_name = county_name2,
        county_name2 = NULL
      )
  ) %>%
  filter(county_name != "") %>%
  group_by(county_name, value) %>%
  mutate(
    fill_col = case_when(
      value == "all" ~ NA_real_,
      value == "county" ~ mean(exp(costpred_brt))
    ),
    fill_val = mean(exp(costpred_brt))
  ) %>%
  ungroup() %>%
  arrange(-fill_col) %>%
  mutate(
    county_name = fct_inorder(factor(county_name))
  ) %>%
  ggplot() + 
  geom_density(
    aes(
      x = exp(costpred_brt),
      # y = after_stat(scaled),
      fill = fill_col,
      group = value,
      # alpha = value
    ),
    # stat = "scaled",
    alpha = 0.5
    # bins = 100
    # position = position_stack()
  ) + 
  geom_vline(
    aes(
      xintercept = fill_val,
      group = value,
      # color = fill_col
      linetype = value
    ),
    # size = 1,
    # linetype = "dashed",
    # data = ~ .x %>%
    #   group_by(county_name, value) %>%
    #   summarise(mean_cost = mean(exp(costpred_brt))) %>%
    #   filter(county_name != "")
  ) +
  geom_label(
    aes(
      x = 7e3,
      y = 2.5, 
      label = n
    ),
    hjust = 0.05,
    size = 2.75,
    data = ~ .x %>% filter(value == "county") %>% group_by(county_name) %>% summarize(n = paste("N =", scales::comma(n())))
  ) +
  scale_x_log10(
    expression("" %<-% "Lower Cost                 (Log - Scale)                 Higher Cost" %->% ""),    
    breaks = NULL
  ) +
  scale_y_continuous(
    "Density",
    labels = NULL,
  ) +
  # scale_alpha_manual(
  #   values =
  #     c(
  #       "county" = 0.5,
  #       "all" = 0
  #     )
  # ) +
  # scale_fill_manual(
  #   values = 
  #     c(
  #       "county" = "grey",
  #       "all" = "white"
  #     )
  # ) +
  # scale_color_distiller(palette = "RdBu", na.value = "black") +
  scale_linetype_manual(
    values =
      c(
        "county" = "solid",
        "all" = "dashed"
      )
  ) +
  scale_fill_distiller(palette = "RdBu", na.value = "grey90") +
  ggthemes::theme_clean() +
  theme(
    legend.position = "none",
    plot.background = element_rect(color = "white"),
    # panel.backgroun = element_rect(fill = "grey85")
  ) +
  facet_wrap(
    "county_name", 
    # scales = "free_y",
    ncol = 3
  ) +
  labs(
    title = "Predicted cost distribtuions by county",
    subtitle = str_wrap("Solid line represents county mean, dashed line represents overall mean. Background curve represents overall distribution across all counties. Area under all curves scaled to one (i.e., sizes of curves do not represent relative number of barriers).")
  )

#+ fig.height=6.15, out.width='100%'
# Codes needed (1 = "city", 2 = "county", 3 = "federal", 4 = "private", 5 = "state", 6 = "tribal", 7 = "other", 8 = "port", 9 = "drainage district, 11 = "irrigation district", 12 = "unknown")
sf_inv_preds %>% 
  st_drop_geometry() %>% 
  # filter(Ownership == "County") %>% 
  mutate(
    value = "owner_type_code",
    owner_type_code =
      case_when(
        owner_type_code == 1 ~ "City",
        owner_type_code == 2 ~ "County", 
        owner_type_code == 3 ~ "Federal", 
        owner_type_code == 4 ~ "Private",
        owner_type_code == 5 ~ "State",
        owner_type_code == 6 ~ "Tribal",
        owner_type_code == 7 ~ "Other",
        owner_type_code == 8 ~ "Port",
        owner_type_code == 9 ~ "Drainage district",
        owner_type_code == 11 ~ "Irrigation district",
        owner_type_code == 12 ~ "Unknown"
      )
  ) %>%
  select(
    owner_type_code,
    value,
    costpred_brt
  ) %>%
  bind_rows(
    sf_inv_preds %>% 
      st_drop_geometry() %>% 
      select(
        owner_type_code,
        # value,
        costpred_brt
      ) %>%
      expand_grid(owner_code2 = unique(sf_inv_preds$owner_type_code)) %>%
      mutate(
        value = "all",
        owner_type_code = as.character(owner_code2),
        owner_code2 = NULL
      )
  ) %>%
  filter(!(owner_type_code %in% c(1:12))) %>% drop_na() %>%
  group_by(owner_type_code, value) %>%
  mutate(
    fill_col = case_when(
      value == "all" ~ NA_real_,
      value == "owner_type_code" ~ mean(exp(costpred_brt))
    ),
    fill_val = mean(exp(costpred_brt))
  ) %>%
  ungroup() %>%
  arrange(-fill_col) %>%
  mutate(
    owner_type_code = fct_inorder(factor(owner_type_code))
  ) %>%
  ggplot() + 
  geom_density(
    aes(
      x = exp(costpred_brt),
      # y = after_stat(scaled),
      fill = fill_col,
      group = value,
      # alpha = value
    ),
    # stat = "scaled",
    alpha = 0.5
    # bins = 100
    # position = position_stack()
  ) + 
  geom_vline(
    aes(
      xintercept = fill_val,
      group = value,
      # color = fill_col
      linetype = value
    ),
    # size = 1,
    # linetype = "dashed",
    # data = ~ .x %>%
    #   group_by(county_name, value) %>%
    #   summarise(mean_cost = mean(exp(costpred_brt))) %>%
    #   filter(county_name != "")
  ) +
  geom_label(
    aes(
      x = 7e3,
      y = 2.5, 
      label = n
    ),
    hjust = 0.05,
    size = 2.75,
    data = ~ .x %>% filter(value == "owner_type_code") %>% group_by(owner_type_code) %>% summarize(n = paste("N =", scales::comma(n())))
  ) +
  scale_x_log10(
    expression("" %<-% "Lower Cost                 (Log - Scale)                 Higher Cost" %->% ""),    
    breaks = NULL
  ) +
  scale_y_continuous(
    "Density",
    labels = NULL,
  ) +
  # scale_alpha_manual(
  #   values =
  #     c(
  #       "county" = 0.5,
  #       "all" = 0
  #     )
  # ) +
  # scale_fill_manual(
  #   values = 
  #     c(
  #       "county" = "grey",
#       "all" = "white"
#     )
# ) +
# scale_color_distiller(palette = "RdBu", na.value = "black") +
scale_linetype_manual(
  values =
    c(
      "owner_type_code" = "solid",
      "all" = "dashed"
    )
) +
  scale_fill_distiller(palette = "RdBu", na.value = "grey90") +
  ggthemes::theme_clean() +
  theme(
    legend.position = "none",
    plot.background = element_rect(color = "white"),
    # panel.backgroun = element_rect(fill = "grey85")
  ) +
  facet_wrap(
    "owner_type_code", 
    # scales = "free_y",
    ncol = 3
  ) +
  labs(
    title = "Predicted cost distribtuions by ownership",
    subtitle = str_wrap("Solid line represents ownership class mean. Area under all curves scaled to one (i.e., sizes of curves do not represent relative number of barriers).")
  )
