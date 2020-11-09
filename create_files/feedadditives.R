# SCRIPT TO EXTRACT THE EU REGISTER OF FEED ADDITIVES

library(pdftools)
library(magrittr)
library(purrr)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(httr)
library(polite)
library(assertthat)

devtools::load_all()

# web scraping check: not really scraping as such (it's a download) but will still check robots.txt for the feed additives page

rcheck = bow("https://ec.europa.eu/food/safety/animal-feed/feed-additives/eu-register_en")

assert_that(is.polite(rcheck) == TRUE, msg = "do not scrape")


# download the pdf

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

# this is now a list of tables (one for each page) containing the data

annex1 <- pdfdata[start:(end-1)]

# define function to get data out of a table

getpdftable <- function(i, tablist) {

  pagenumber <- paste0("page",i)

  # if it's the first table, get rid of main title
  # otherwise get rid of everything until data (incl col headings)

  if(i == 1) {

    table <- tablist[[i]] %>%
      slice(-1:-5)

    #table <- tablist[[i]] %>%
    #  filter(y > 150)

  } else {
    table <- tablist[[i]] %>%
      filter(y > 80)
  }

  # what y co-ordinate does each row start on
  # anchor by "OJ" because this is the only thing that is consistent in every row
  # take away a few because some things are slightly higher

  rowstart <- table %>%
    filter(text == "OJ" & x == 595) %>%
    select(y) %>%
    mutate(y = y-3) %>%
    mutate(rownumber = as.character(row_number() + 1))

  # identify which words belong to which rows/columns

  getrowcol <- table %>%
    # gets rid of the footer on every page
    filter(y < 547) %>%
    # delineate columns - hard coded, not clear how to do otherwise
    mutate(col = cut(x,
                     breaks = c(56, 97, 140, 246, 295, 459, 591, 655, 704, 782, Inf),
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

  # bit of a hacky thing to get rid of a problematic footnote on a couple of pages

  delete <- getrowcol %>%
    filter(is.na(col)) %>%
    .$y %>% as.numeric()

  # paste together the words in the same cells
  # where entries run on to multiple rows, identify the first

  getrowcol %>%
    filter(!y %in% delete) %>%
    group_by(row, col) %>%
    mutate(text = paste(text, collapse = " ")) %>%
    select(row, col, text) %>% unique() %>% spread(col, text) %>%
    ungroup() %>%
    mutate(topline = ifelse(!is.na(j), paste0(pagenumber,"-",row_number()), NA)) %>%
    select(-row)

}

# extract data from all tables and merge into one DF
# paste together content that has gone onto multiple lines
# add column names

annextable <- map_df(c(1:length(annex1)), getpdftable, annex1) %>%
  fill(topline) %>%
  replace(., is.na(.), "") %>%
  group_by(topline) %>%
  mutate_at(vars(-topline), ~paste(., collapse = " ")) %>%
  unique() %>%
  ungroup() %>%
  mutate_all(., trimws) %>%
  select(-topline) %>%
  getnames()


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
  unnest(cols = c(sepref)) %>%
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
  mutate(link = if_else(linkerror == TRUE, paste0(link,"[BROKEN LINK]"), link)) %>%
  select(id, link) %>%
  group_by(id) %>%
  mutate(`Link(s)` = paste0(link, collapse = " / ")) %>%
  ungroup() %>%
  select(id, `Link(s)`) %>%
  unique() %>%
  mutate(link = str_split(`Link(s)`, " / ")) %>%
  unnest(cols = c(link)) %>%
  select(-`Link(s)`)


annextable <- annextable %>%
  mutate(id = row_number()) %>%
  mutate(linktext = str_split(`Reference in OJ`, "(?=OJ)")) %>%
  unnest(cols = c(linktext)) %>%
  filter(linktext != "") %>%
  mutate(linktext = str_remove_all(linktext, " / .*$")) %>%
  bind_cols(links)

# check that's worked!

assert_that(nrow(annextable %>% filter(id != id1)) == 0)

# get rid of second id variable

annextable <- annextable %>%
  select(-id1)

# final bit to manually fix some of the weird spacing caused by the poor typesetting

wrong <- c("f eedingstuffs",
           "f av ourably",
           "f ish",
           "Enzy mes",
           "Artif icial",
           "ef fect",
           "f avourably",
           "f eedingstuffs",
           "f eedingstuf",
           "f ed",
           "f ood",
           "Emulsif iers",
           "Flav ouring",
           "f lora",
           "hy giene",
           "Hy giene",
           "sy nthetic",
           "f lav ourings",
           "f actors",
           "improv ement",
           "f eed",
           "f aeces",
           "Preserv atives",
           "additiv es",
           "deriv atives",
           "ef f ect",
           "flav ourings",
           "def ined")

right = c("feedingstuffs",
          "favourably",
          "fish",
          "Enzymes",
          "Artificial",
          "effect",
          "favourably",
          "feedingstuffs",
          "feedingstuf",
          "fed",
          "food",
          "Emulsifiers",
          "Flavouring",
          "flora",
          "hygiene",
          "Hygiene",
          "synthetic",
          "flav ourings",
          "factors",
          "improvement",
          "feed",
          "faeces",
          "Preservatives",
          "additives",
          "derivatives",
          "effect",
          "flavourings",
          "defined")

names(right) <- wrong

annextable <- annextable %>%
  mutate(Subclassification = str_replace_all(Subclassification, right))


# write to csv


write_excel_csv(annextable, "./csv/feedadd/feed-additives.csv")
