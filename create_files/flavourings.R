# SCRIPT TO PRODUCE THE UNION LIST OF APPROVED FLAVOURINGS

library(dplyr)
library(magrittr)
library(readr)
library(stringr)
library(eulegscrape)


url <- read_csv("./reference/legislation-urls.csv") %>%
  filter(product == "flavourings") %>%
  .[1,2] %>% as.character() %>%
  geturl()

# ANNEX I part A

parta <- firstclean(gettable(url, 4)) %>%
  mutate(X7 = extraspace(X7)) %>%
  getnames(2) %>%
  slice(-1)

# ANNEX I part E

parte <- firstclean(gettable(url, 5)) %>%
  mutate_at(vars(X6,X7), ~extraspace(.)) %>%
  slice(-1)
names(parte) <- names(parta)

# Bind together and save out

flavourings <- bind_rows(parta, parte) %>%
    replace(., is.na(.), "")

write_excel_csv(flavourings, "./csv/flavourings/flavourings.csv")

