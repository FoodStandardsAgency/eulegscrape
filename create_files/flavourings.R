# SCRIPT TO PRODUCE THE UNION LIST OF APPROVED FLAVOURINGS

library(dplyr)
library(magrittr)
library(readr)
library(stringr)
library(eulegscrape)
library(assertthat)


url <- read_csv("./reference/legislation-urls.csv") %>%
  filter(product == "flavourings") %>%
  .[1,2] %>% as.character() %>%
  geturl()

# ANNEX I part A

table <- gettable(url, 4)
assert_that(nrow(table) > 2000 & ncol(table) ==9, msg = "flavourings annex I part A - this is probably not grabbing the right table")

parta <- firstclean(table) %>%
  mutate(X7 = extraspace(X7)) %>%
  getnames(2) %>%
  slice(-1)

# ANNEX I part E

table <- gettable(url, 5)
assert_that(nrow(table) > 0 & ncol(table) ==9, msg = "flavourings annex I part E - this is probably not grabbing the right table")

parte <- firstclean(table) %>%
  mutate_at(vars(X6,X7), ~extraspace(.)) %>%
  slice(-1)
names(parte) <- names(parta)

# Bind together and save out

flavourings <- bind_rows(parta, parte) %>%
    replace(., is.na(.), "")

write_excel_csv(flavourings, "./csv/flavourings/flavourings.csv")

