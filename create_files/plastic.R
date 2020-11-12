# SCRIPT TO PRODUCE UNION LIST OF AUTHORISED SUBSTANCES

library(dplyr)
library(magrittr)
library(readr)
library(rvest)
library(eulegscrape)
library(stringr)
library(tidyr)
library(assertthat)
library(purrr)


url <- read_csv("./reference/legislation-urls.csv") %>%
  filter(product == "plastic") %>%
  .[1,2] %>% as.character() %>%
  geturl()

rcheck = bow("https://ec.europa.eu/food/safety/animal-feed/feed-additives/eu-register_en",
             user_agent = "Food Standards Agency https://www.food.gov.uk/about-us/web-scraping-policy-0")

assert_that(is.polite(rcheck) == TRUE, msg = "do not scrape")

htmldoc <- read_html(url,
                     user_agent = "Food Standards Agency https://www.food.gov.uk/about-us/web-scraping-policy-0")

# Annex I, Table 1

table <- getbadtable(htmldoc, 4)
assert_that(ncol(table) == 11 & nrow(table) >= 100, msg = "plastic - annex I table 1 probably not right table")

substances <- table %>%
  firstclean() %>%
  slice(-1) %>%
  mutate_at(vars(X9, X11), ~bracketonly(.)) %>%
  getnames() %>%
  replace(is.na(.), "") %>%
  slice(-nrow(.)) %>%
  mutate_all(., ~str_replace_all(., "(\\s){2,}", " "))

write_excel_csv(substances, "./csv/foodcontact/plastic/substances.csv")

# Annex I, Table 2

table <- getbadtable(htmldoc, 5)
assert_that(ncol(table) == 4 & nrow(table) >= 40, msg = "plastic - annex I table 2 probably not right table")

grouprestrict <- table %>%
  firstclean() %>%
  slice(-1) %>%
  mutate(X2 = str_replace_all(X2, "(\\s){2,}", ", ")) %>%
  getnames() %>%
  replace(is.na(.), "")

write_excel_csv(grouprestrict, "./csv/foodcontact/plastic/group-restrictions.csv")

# Annex I, Table 3

table <- getbadtable(htmldoc, 6)
assert_that(ncol(table) == 2 & nrow(table) >= 30, msg = "plastic - annex I table 3 probably not right table")

compliance <- table %>%
  firstclean() %>%
  slice(-1) %>%
  mutate(X1 = bracketonly(X1)) %>%
  getnames() %>%
  replace(is.na(.), "")

write_excel_csv(compliance, "./csv/foodcontact/plastic/compliance.csv")


# Annex II - restrictions on materials and articles

table <- gettable(url, 8)
assert_that(ncol(table) == 2 & nrow(table) == 1, msg = "plastic - annex II probably not right table")

materialrestrict <- table %>% .[1,2] %>% as.character() %>%
  str_split(., "\n") %>%
  .[[1]] %>%
  tibble::enframe(name = NULL) %>%
  firstclean() %>%
  mutate_all(., ~str_replace_all(., "(\\s){2,}", "")) %>%
  separate(value, c("Substance", "Specific migration limit"), sep = "=") %>%
  mutate_all(., ~trimws(.)) %>%
  slice(-1)

write_excel_csv(materialrestrict, "./csv/foodcontact/plastic/material-restrictions.csv")


# Annex III, Table 2 (and simulant names from Table 1)

table <- gettable(url, 10)
assert_that(ncol(table) == 2 & nrow(table) >= 5, msg = "plastic - annex III table 1 probably not right table")

simname <- table %>%
  firstclean() %>%
  getnames() %>%
  mutate(Abbreviation = str_remove(Abbreviation, fixed("Food simulant ")))
namereplace <- simname$`Food simulant` %>% as.character()
names(namereplace) <- simname$Abbreviation

table <- getbadtable(htmldoc, 11)
assert_that(ncol(table) == 8 & nrow(table) >= 120, msg = "plastic - annex III table 2 probably not right table")

simulants <- table %>%
  mutate_at(vars(X3:X8), ~recode(., !!!namereplace)) %>%
  firstclean() %>%
  slice(-1:-2) %>%
  getnames() %>%
  replace(is.na(.), "")

write_excel_csv(simulants, "./csv/foodcontact/plastic/simulants.csv")

