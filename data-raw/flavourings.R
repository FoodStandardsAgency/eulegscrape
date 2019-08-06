# SCRIPT TO PRODUCE THE UNION LIST OF APPROVED FLAVOURINGS

library(dplyr)
library(magrittr)
library(eulegscrape)


url <- legurls %>%
  filter(product == "flavourings") %>%
  .[1,3] %>% as.character()

# ANNEX I part A

parta <- firstclean(gettable(url, 4))

colnames <- parta[2,] %>% as.character()
names(parta) <- colnames

parta <- parta %>%
  slice(-1:-2)

# ANNEX I part E

parte <- firstclean(gettable(url, 5)) %>%
    slice(2)

names(parte) <- colnames

# Bind together and save out

flavourings <- bind_rows(parta, parte) %>%
    replace(., is.na(.), "")

usethis::use_data(flavourings, overwrite = T)

