# SCRIPT TO PRODUCE LIST OF SUBSTANCES AUTHORISED IN THE MANUFACTURE OF REGENERATED CELLULOSE FILM

library(dplyr)
library(magrittr)
library(readr)
library(eulegscrape)
library(rvest)
library(htmltab)
library(stringr)
library(assertthat)


url <- read_csv("./reference/legislation-urls.csv") %>%
  filter(product == "cellulose") %>%
  .[1,2] %>% as.character() %>%
  geturl()

htmldoc <- read_html(url)

fetchtable <- function(htmldoc, n) {
  htmldoc %>%
    html_nodes("table") %>%
    .[n] %>%
    as.character() %>%
    htmltab() %>%
    as_tibble()
}

# Annex II first part - uncoated regenerated cellulose film

table <- fetchtable(htmldoc, 27)
assert_that(ncol(table) == 2 & nrow(table) >= 50, msg = "cellulose - annex II first part probably wrong table")

uncoated <- table %>%
  mutate_all(., ~iconv(., from = "UTF-8", to = "UTF-8")) %>%
  firstclean() %>%
  mutate(Denominations = str_replace(Denominations, "^-", "    ")) %>%
  replace(is.na(.), "")

write_excel_csv(uncoated, "./csv/foodcontact/cellulose/uncoated.csv")

# Annex II second part - coated regenerated cellulose film

table <- fetchtable(htmldoc, 83)
assert_that(ncol(table) == 2 & nrow(table) >= 50, msg = "cellulose - annex III first part probably wrong table")

coated <- table %>%
  mutate_all(., ~iconv(., from = "UTF-8", to = "UTF-8")) %>%
  firstclean() %>%
  mutate(Denominations = str_replace(Denominations, "^-", "    ")) %>%
  replace(is.na(.), "")

write_excel_csv(coated, "./csv/foodcontact/cellulose/coated.csv")

