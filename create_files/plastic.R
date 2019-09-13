# SCRIPT TO PRODUCE UNION LIST OF AUTHORISED SUBSTANCES

library(dplyr)
library(magrittr)
library(readr)
library(rvest)
library(eulegscrape)
library(stringr)
library(tidyr)


url <- read_csv("./reference/legislation-urls.csv") %>%
  filter(product == "plastic") %>%
  .[1,2] %>% as.character() %>%
  geturl()

htmldoc <- read_html(url)

# Annex I, Table 1

substances <- getbadtable(htmldoc, 4) %>%
  firstclean() %>%
  slice(-1) %>%
  mutate_at(vars(X9, X11), ~bracketonly(.)) %>%
  getnames() %>%
  replace(is.na(.), "") %>%
  slice(-nrow(.)) %>%
  mutate_all(., ~str_replace_all(., "(\\s){2,}", " "))

write_excel_csv(substances, "./csv/foodcontact/plastic/substances.csv")

# Annex I, Table 2

grouprestrict <- getbadtable(htmldoc, 5) %>%
  firstclean() %>%
  slice(-1) %>%
  mutate(X2 = str_replace_all(X2, "(\\s){2,}", ", ")) %>%
  getnames() %>%
  replace(is.na(.), "")

write_excel_csv(grouprestrict, "./csv/foodcontact/plastic/group-restrictions.csv")

# Annex I, Table 3

compliance <- getbadtable(htmldoc, 6) %>%
  firstclean() %>%
  slice(-1) %>%
  mutate(X1 = bracketonly(X1)) %>%
  getnames() %>%
  replace(is.na(.), "")

write_excel_csv(compliance, "./csv/foodcontact/plastic/compliance.csv")


# Annex II - restrictions on materials and articles

materialrestrict <- gettable(url, 8) %>% .[1,2] %>% as.character() %>%
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

simname <- gettable(url, 10) %>%
  firstclean() %>%
  getnames() %>%
  mutate(Abbreviation = str_remove(Abbreviation, fixed("Food simulant ")))
namereplace <- simname$`Food simulant` %>% as.character()
names(namereplace) <- simname$Abbreviation


simulants <- getbadtable(htmldoc, 11) %>%
  mutate_at(vars(X3:X8), ~recode(., !!!namereplace)) %>%
  firstclean() %>%
  slice(-1:-2) %>%
  getnames() %>%
  replace(is.na(.), "")

write_excel_csv(simulants, "./csv/foodcontact/plastic/simulants.csv")

