# SCRIPT TO EXTRACT THE EU REGISTER OF FEED ADDITIVES

library(pdftools)
library(magrittr)
library(purrr)
library(dplyr)
library(tidyr)
library(eulegscrape)
library(readr)
library(stringr)
library(httr)

url <- "https://ec.europa.eu/food/sites/food/files/safety/docs/animal-feed-eu-reg-comm_register_feed_additives_1831-03.pdf"
download.file(url, "./reference/feedreg.pdf", mode = "wb")

pdfdata <- pdf_data("./reference/feedreg.pdf")

# identify the tables with the annex I data

startend <- pdfdata %>%
  map(~ .x %>% mutate(top = ifelse(.[1,6] == "Annex", 1, 0)) %>% summarise(end = mean(top))) %>%
  bind_rows() %>%
  mutate(tablenumber = row_number()) %>%
  filter(end == 1)
start <- startend[1,2] %>% as.numeric()
end <- startend[2,2] %>% as.numeric()

annex1 <- pdfdata[start:(end-1)]

getpdftable <- function(table) {

  # if it's the first table get rid of annex I title

  if(table[1,6] == "Annex") {
    table <- table %>% slice(-1:-5)
  }


  # what y co-ordinate does each row start on
  # anchor by "OJ" because this is the only thing that is consistent in every row
  # take away a few because some things are slightly higher

  rowstart <- table %>%
    filter(text == "OJ" & x == 594) %>%
    select(y) %>%
    mutate(y = y-3) %>%
    mutate(rownumber = as.character(row_number() + 1))

  # identify which words belong to which rows/columns

  getrowcol <- table %>%
    # gets rid of the footer on every page
    filter(y < 547) %>%
    # delineate columns - hard coded, not clear how to do otherwise
    mutate(col = cut(x,
                     breaks = c(56, 97, 140, 246, 296, 459, 591, 655, 704, 782, Inf),
                     labels = c(letters[1:10]),
                     include.lowest = TRUE)) %>%
    # delineate rows
    mutate(row = cut(y,
                     breaks = c(1, rowstart$y,Inf),
                     labels = c("1", rowstart$rownumber),
                     include.lowest = TRUE, right = FALSE)) %>%
    # get rid of "(Annex I of Reg. 1831/03)
    mutate(subtitle = if_else(col == "a" & text %in% c("(Annex", "I", "of", "Reg.") |
                              col == "b" & text == "1831/03)", 1, 0)) %>%
    filter(subtitle == 0) %>%
    select(-subtitle) %>%
    arrange(row, col, y)

  # bit of a hacky thing to get rid of a problematic footnote row on one page

  delete <- getrowcol %>%
    filter(is.na(col)) %>%
    .$y %>% as.numeric()

  # paste together the words in the same cells

  getrowcol %>%
    filter(!y %in% delete) %>%
    group_by(row, col) %>%
    mutate(text = paste(text, collapse = " ")) %>%
    select(row, col, text) %>% unique() %>% spread(col, text) %>%
    ungroup() %>%
    mutate(topline = ifelse(!is.na(j), row_number(), NA)) %>%
    fill(topline) %>%
    replace(., is.na(.), "") %>%
    group_by(topline) %>%
    mutate_at(vars(-topline), ~paste(., collapse = " ")) %>%
    unique() %>%
    ungroup() %>%
    mutate_all(., trimws) %>%
    select(-topline, -row) %>%
    getnames()
}

annextable <- map_df(annex1, getpdftable)

# links

countries <- tibble(code = c("BG", "HR", "CS", "DA", "NL", "EN", "ET", "FI",
                             "FR", "DE", "EL", "HU", "GA", "IT", "LV", "LT",
                             "MT", "PL", "PT", "RO", "SK", "SL", "ES", "SV"),
                    lang = c("BUL", "HRV", "CES", "DAN", "NLD", "ENG", "EST",
                             "FIN", "FRA", "DEU", "ELL", "HUN", "GLE", "ITA",
                             "LAV", "LIT", "MLT", "POL", "POR", "RON", "SLK",
                             "SLV", "SPA", "SWE"))

ccodes <- countries$code %>% paste0(collapse = "|")


# create the links

links <- annextable %>%
  mutate(id = row_number()) %>%
  select(id, OJRef = `Reference in OJ`) %>%
  mutate(sepref = str_split(OJRef, "OJ")) %>%
  unnest() %>%
  filter(sepref != "") %>%
  mutate(sepref = str_remove_all(sepref, " / .*$")) %>%
  separate(sepref, c("sepref", "page"), "p.") %>%
  mutate(page = str_remove_all(page, "[^0-9]")) %>%
  mutate(page = ifelse(nchar(page) == 1, paste0("000",page), page)) %>%
  mutate(page = ifelse(nchar(page) == 2, paste0("00",page), page)) %>%
  mutate(page = ifelse(nchar(page) == 3, paste0("0",page), page)) %>%
  mutate(date = str_extract(sepref, "([0-9]){1,2}[\\./]([0-9]){1,2}[\\./]([0-9]){2,4}")) %>%
  separate(date, into = c("day", "month", "year"), sep = "\\.|/") %>%
  mutate(year = ifelse(nchar(year) < 4, paste0("20",year), year)) %>%
  mutate(vol = str_extract(sepref, "[LC]( )?([0-9]){1,3}")) %>%
  mutate(type = str_extract(vol, "[LC]")) %>%
  mutate(vol = str_remove_all(vol, "[^0-9]")) %>%
  mutate(vol = ifelse(nchar(vol) == 2, paste0("0", vol), vol)) %>%
  mutate(vol = ifelse(nchar(vol) == 1, paste0("00",vol), vol)) %>%
  mutate(ccode = ifelse(grepl(ccodes, sepref), str_extract(sepref, ccodes), "EN")) %>%
  left_join(countries, by = c("ccode" = "code")) %>%
  mutate(link = paste0("https://eur-lex.europa.eu/legal-content/EN/TXT/PDF/?uri=uriserv:OJ.",
                       type,"_.",year,".",vol,".01.",page,".01.",lang)) %>%
  rowwise() %>%
  mutate(linkerror = http_error(link)) %>%
  ungroup() %>%
  mutate(link = if_else(linkerror == TRUE, "", link)) %>%
  select(id, link) %>%
  group_by(id) %>%
  mutate(`Link(s)` = paste0(link, collapse = " / ")) %>%
  ungroup() %>%
  select(id, `Link(s)`) %>%
  unique() %>%
  select(-id)

annextable <- bind_cols(annextable, links)


# merge back into main data table


write_excel_csv(annextable, "./csv/feedadd/feed-additives.csv")
