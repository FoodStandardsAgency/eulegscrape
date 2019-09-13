# SCRIPT TO PRODUCE LIST OF SUBSTANCES AUTHORISED IN THE MANUFACTURE OF REGENERATED CELLULOSE FILM

library(dplyr)
library(magrittr)
library(readr)
library(eulegscrape)
library(rvest)
library(htmltab)
library(stringr)


url <- read_csv("./reference/legislation-urls.csv") %>%
  filter(product == "cellulose") %>%
  .[1,2] %>% as.character() %>%
  geturl()

# Annex II first part - uncoated regenerated cellulose film

uncoated <- read_html(url) %>%
  html_nodes("table") %>%
  .[27] %>%
  as.character() %>%
  htmltab() %>%
  as_tibble() %>%
  mutate_all(., ~iconv(., from = "UTF-8", to = "UTF-8")) %>%
  firstclean() %>%
  mutate(Denominations = str_replace(Denominations, "^-", "    ")) %>%
  replace(is.na(.), "")

write_excel_csv(uncoated, "./csv/foodcontact/cellulose/uncoated.csv")

# Annex II second part - coated regenerated cellulose film

coated <- read_html(url) %>%
  html_nodes("table") %>%
  .[83] %>%
  as.character() %>%
  htmltab() %>%
  as_tibble() %>%
  mutate_all(., ~iconv(., from = "UTF-8", to = "UTF-8")) %>%
  firstclean() %>%
  mutate(Denominations = str_replace(Denominations, "^-", "    ")) %>%
  replace(is.na(.), "")

write_excel_csv(coated, "./csv/foodcontact/cellulose/coated.csv")

