# SCRIPT TO PRODUCE UNION LIST OF NOVEL FOODS

library(dplyr)
library(magrittr)
library(readr)
library(stringr)
library(tidyr)
library(eulegscrape)
library(rvest)
library(htmltab)
library(assertthat)

url <- read_csv("./reference/legislation-urls.csv") %>%
  filter(product == "novel") %>%
  .[1,2] %>% as.character() %>%
  geturl()

rcheck = bow("https://ec.europa.eu/food/safety/animal-feed/feed-additives/eu-register_en",
             user_agent = "Food Standards Agency https://www.food.gov.uk/about-us/web-scraping-policy-0")

assert_that(is.polite(rcheck) == TRUE, msg = "do not scrape")

htmldoc <- read_html(url,
                     user_agent = "Food Standards Agency https://www.food.gov.uk/about-us/web-scraping-policy-0")

table <- htmldoc %>%
  html_nodes("table") %>%
  .[9] %>%
  as.character() %>%
  str_replace_all(., fixed("\n"), "[NEWLINE]") %>%
  htmltab(., header = 0) %>%
  as_tibble()

assert_that(ncol(table) == 6 & nrow(table) >= 700, msg = "novel - probably fetching wrong table")

novel <- table %>%
  mutate_all(., ~iconv(., from = "UTF-8", to = "UTF-8")) %>%
  firstclean() %>%
  mutate(fn = str_extract_all(V2, "(\\([0-9]\\))")) %>%
  mutate(fn = str_replace(as.character(fn), "^ch.*", "")) %>%
  mutate_all(., ~str_replace_all(., fixed("[NEWLINE]"), "\n")) %>%
  mutate_all(., ~str_remove(., "^(\\s){2,}")) %>%
  mutate_all(., ~str_remove(., "(\\s){1,}$")) %>%
  mutate(V1 = str_replace_all(V1, "(\\s){2,}", " ")) %>%
  mutate(V3 = str_replace_all(str_replace_all(V3, "\n", "; "), "(\\s){2,}", " ")) %>%
  mutate_at(vars(V2, V4, V6), ~str_replace_all(., "(\\s){2,}", "\n")) %>%
  mutate(delrow = ifelse(V1 == "" & V2 == "" & V3 == "" & V4 == "" & V5 == "" & V6 == "", 1, 0)) %>%
  filter(delrow == 0) %>%
  select(-delrow)

footnotes <- novel %>%
  slice(nrow(.)) %>%
  select(V1) %>%
  mutate(V1 = str_remove(str_replace_all(V1, "(\\([0-9]\\))", "//\\1"), "^//")) %>%
  separate(V1, into = letters[1:6], sep = "//") %>%
  gather() %>%
  select(footnote = value) %>%
  mutate(fn = str_extract_all(footnote, "(\\([0-9]\\))")) %>% unnest()

novel <- novel %>%
  left_join(footnotes, by = "fn") %>%
  select(-fn) %>%
  slice(-nrow(.)) %>%
  mutate(footnote = if_else(row_number() == 1, "footnote", footnote)) %>%
  getnames() %>%
  replace(is.na(.), "")

write_excel_csv(novel, "csv/novelfoods/authorised-novel-foods.csv")


