library(readr)
library(dplyr)
library(magrittr)
library(purrr)
library(rvest)
library(eulegscrape)

legurls <- read_csv("./data-raw/legislation-urls.csv")

#' Update the list of urls with the latest consolidated versions
#'

legurls <- legurls %>%
  mutate(eulexurl = map_chr(uklegurl, geturl))

usethis::use_data(legurls, overwrite = T)
