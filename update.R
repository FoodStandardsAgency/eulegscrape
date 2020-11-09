
library(readr)
library(purrr)
library(dplyr)
library(eulegscrape)
library(polite)
devtools::load_all()


# read in table of URLs

urltable <- read_csv("reference/legislation-urls.csv")

# get latest urls

url2 <- urltable %>%
  mutate(latest = pmap_chr(., ~geturl(..2)))

# which products need updating (and is it OK to do so - polite check)?

products <- url2 %>%
  filter(eurlex != latest) %>%
  rowwise() %>%
  mutate(pol = is.polite(bow(eurlex))) %>%
  filter(pol == TRUE) %>%
  pull(product)


if(length(products) > 0) {
  # source makefiles for these products

  map(products, function(x) paste0("create_files/",x,".R")) %>% unlist() %>% map(., source)

  # update urls in reference table

  url2 %>%
    select(product, uklegurl, eurlex = latest) %>%
    write_csv(., "reference/legislation-urls.csv")
} else {
  print("no updates needed!")
}



