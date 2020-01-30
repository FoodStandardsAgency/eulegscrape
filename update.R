
library(readr)
library(purrr)
library(dplyr)
library(eulegscrape)


# read in table of URLs

urltable <- read_csv("reference/legislation-urls.csv")

# get latest urls

url2 <- urltable %>%
  mutate(latest = pmap_chr(., ~geturl(..2)))

# which products need updating?

products <- url2 %>%
  filter(eurlex != latest) %>%
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



