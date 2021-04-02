library(tidyverse)
library(pdftools)
wsdot_report <- "C:/Users/Braeden/Downloads/Env-StrRest-FishPassageAnnualReport.pdf"
wsdot_txt <- pdf_text(wsdot_report)

wsdot_txt[25]
wsdot_tbl_data <- 
  tibble(
    region =
      pdf_data(wsdot_report)[[25]] %>%
      filter(
        y > 130, y < 759,
        x == 38
      ) %>%
      arrange(y) %>%
      pull(text),
    site_id =
      pdf_data(wsdot_report)[[25]] %>%
      filter(
        y > 130, y < 759,
        x >= 76, x < 141
      ) %>%
      arrange(y) %>%
      group_by(y) %>%
      summarize(text = str_c(text, collapse = " ")) %>%
      pull(text),
    road =
      pdf_data(wsdot_report)[[25]] %>%
      filter(
        y > 130, y < 759,
        x >= 141, x < 188
      ) %>%
      arrange(y) %>%
      group_by(y) %>%
      summarize(text = str_c(text, collapse = " ")) %>%
      pull(text),
    year_fixed =
      pdf_data(wsdot_report)[[25]] %>%
      filter(
        y > 130, y < 759,
        x >= 443, x < 471
      ) %>%
      arrange(y) %>%
      group_by(y) %>%
      summarize(text = str_c(text, collapse = " ")) %>%
      pull(text),
    # lineal_gain =
    #   pdf_data(wsdot_report)[[25]] %>%
    #   filter(
    #     y > 130, y < 759,
    #     x >= 471, x < 536
    #   ) %>%
    #   arrange(y) %>%
    #   group_by(y) %>%
    #   summarize(text = str_c(text, collapse = " ")) %>%
    #   pull(text),
    project_cost =
      pdf_data(wsdot_report)[[25]] %>%
      filter(
        y > 130, y < 759,
        x >= 536
      ) %>%
      arrange(y) %>%
      group_by(y) %>%
      summarize(text = str_c(text, collapse = " ")) %>%
      pull(text)
  )

