library(tidyverse)
library(rvest)

legurls <- read_csv("./data-raw/legislation-urls.csv")

#' Update the list of urls with the latest consolidated versions
#'

legurls <- legurls %>%
  rowwise() %>%
  mutate(eulexurl = geturl(uklegurl))

usethis::use_data(legurls, overwrite = T)
