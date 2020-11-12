# SCRIPT TO PRODUCE THE UNION LIST OF APPROVED SMOKE FLAVOURINGS

library(dplyr)
library(magrittr)
library(tidyr)
library(purrr)
library(readr)
library(stringr)
library(eulegscrape)
library(assertthat)


url <- read_csv("./reference/legislation-urls.csv") %>%
  filter(product == "smoke") %>%
  .[1,2] %>% as.character() %>%
  geturl()

rcheck = bow("https://ec.europa.eu/food/safety/animal-feed/feed-additives/eu-register_en",
             user_agent = "Food Standards Agency https://www.food.gov.uk/about-us/web-scraping-policy-0")

assert_that(is.polite(rcheck) == TRUE, msg = "do not scrape")


getinfo <- function(table) {
  table %>%
  filter(grepl("Unique|Name|Address|Description|Date", X1)) %>%
  select(X1, X2) %>%
  spread(X1, X2) %>%
  mutate_at(vars(1), ~gsub("(\n\\s+)", "; ", .)) %>%
  mutate_at(vars(4), ~str_replace_all(extraspace(.), "\n-", ""))
}

getconditions <- function(table) {
  table %>%
  mutate(`Unique code` = as.character(.[1,2])) %>%
  filter(!grepl("Unique|Name|Address|Description|Date", X1)) %>%
  mutate(shift = if_else(grepl("^[0-9]", X1), 1, 0)) %>%
  mutate(X5 = if_else(shift == 1, X4, X5)) %>%
  mutate(X4 = if_else(shift == 1, X3, X4)) %>%
  mutate(X3 = if_else(shift == 1, X2, X3)) %>%
  mutate(X2 = if_else(shift == 1, X1, X2)) %>%
  mutate(X1 = ifelse(shift == 1, NA, X1)) %>%
  fill(X1) %>%
  filter(grepl("^(Conditions)", X1) & !is.na(X5)) %>%
  select(`Unique code`, `Food category` = X2, `Maximum level g/kg` = X5) %>%
  mutate(`Food category` = gsub("\n\\s+", " ", `Food category`)) %>%
  mutate(`Maximum level g/kg` = decimals(`Maximum level g/kg`))
}


tables <- map(c(32, 61, 87, 115, 147, 173, 195, 224, 255, 301), gettable, url = url)
map(tables, function(x) assert_that(ncol(x) >= 25 & nrow(x) >= 10, msg = "smoke - probably not the right tables"))

getsmoke <- function(table) {
  cleantable <- firstclean(table)
  info <- getinfo(cleantable)
  conditions <- getconditions(cleantable)
  left_join(info, conditions)
}

smoke <- map_df(tables, getsmoke)

write_excel_csv(smoke, "./csv/smoke/smoke.csv")



