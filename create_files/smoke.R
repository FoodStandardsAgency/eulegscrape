# SCRIPT TO PRODUCE THE UNION LIST OF APPROVED SMOKE FLAVOURINGS

library(dplyr)
library(magrittr)
library(tidyr)
library(purrr)
library(readr)
library(stringr)
library(eulegscrape)


url <- read_csv("./reference/legislation-urls.csv") %>%
  filter(product == "smoke") %>%
  .[1,2] %>% as.character() %>%
  geturl()


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


getsmoke <- function(n) {
  table <- firstclean(gettable(url, n))
  info <- getinfo(table)
  conditions <- getconditions(table)
  left_join(info, conditions)
}

smoke <- map(c(32, 61, 87, 115, 147, 173, 195, 224, 255, 301), getsmoke) %>%
  bind_rows()


write_excel_csv(smoke, "./csv/smoke/smoke.csv")



