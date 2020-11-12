# SCRIPT TO PRODUCE THE UNION LIST OF APPROVED CONTAMINANTS

library(dplyr)
library(magrittr)
library(tidyr)
library(stringr)
library(purrr)
library(readr)
library(eulegscrape)
library(rvest)
library(assertthat)

url <- read_csv("./reference/legislation-urls.csv") %>%
  filter(product == "contaminants") %>%
  .[1,2] %>% as.character() %>%
  geturl()

rcheck = bow(url,
             user_agent = "Food Standards Agency https://www.food.gov.uk/about-us/web-scraping-policy-0")

assert_that(is.polite(rcheck) == TRUE, msg = "do not scrape")

contam <- read_html(url,
                    user_agent = "Food Standards Agency https://www.food.gov.uk/about-us/web-scraping-policy-0")

# get footnotes

fn <- contam %>% html_nodes(".footnote")
assert_that(length(fn) >= 80, msg = "contaminants - not getting all the footnotes?")

fnnumbers <- fn %>%
  map(., html_nodes, "a") %>%
  map(., html_nodes, ".superscript") %>%
  map(., html_text) %>%
  as.character()

fnhrefs <- fn %>%
  map(., html_nodes, "a") %>%
  map(., html_attr, "href") %>%
  as.character()

fntext <- fn %>%
  html_text() %>%
  str_replace_all(., "(\u25BA)([A-z0-9]+)", "") %>%
  str_replace_all(., "\u25C4", "") %>%
  str_replace_all(.,"(\\s){2}", "") %>%
  str_replace("\\( ", "\\(")

fnlinks <- bind_cols(fnumber = fnnumbers, link = fnhrefs, fntext = fntext) %>%
  filter(grepl("#src.E", link)) %>%
  mutate(link = gsub("src.", "", link)) %>%
  mutate(link = str_extract(link, "#E[0-9]+")) %>%
  mutate(fnumber = paste0("(",fnumber,")"))

assert_that(ncol(fnlinks) == 3, msg = "contaminants - not picking up footnote info")

# table of footnotes to write out

footnotes <- fnlinks[,3] %>% rename(Footnote = fntext)

write_excel_csv(footnotes, "./csv/contaminants/footnotes.csv")

# lookup table to replace footnote links with correct numbers in the tables

linkreplace <- fnlinks$fnumber %>% as.character()
names(linkreplace) <- fnlinks$link



# function to get the table
# swap footnote numbers for links
# swap links for correct footnote numbers

# function to get table n on the page (different from generic)

getctable <- function(n) {
  contam %>%
    html_nodes("table") %>%
    .[n] %>%
    html_nodes("tr") %>%
    .[!grepl("ancestor", .)] %>%
    as.character() %>%
    paste0(collapse = "") %>%
    paste0("<table>",.,"</table>") %>%
    read_html() %>%
    html_table(fill = TRUE) %>%
    .[[1]] %>%
    as_tibble()
}

# get the tables and check they are probably the right ones

tables <- map(c(4:12), getctable)
assert_that(ncol(tables[[1]]) == 4 & nrow(tables[[1]]) > 10, msg = "contaminants - probably starting in the wrong place")
assert_that(length(tables) == 9, msg = "contaminants - not picking up all the tables")

# function to get footnote numbers that will be swapped in for link names

getfnswap <- function(n) {
  fnnumbers <- contam %>%
    html_nodes("table") %>%
    .[n] %>%
    html_nodes("a") %>%
    map(., html_nodes, ".superscript") %>%
    map(., html_text) %>%
    as.character()

  fnhrefs <- contam %>%
    html_nodes("table") %>%
    .[n] %>%
    html_nodes("a") %>%
    map(., html_attr, "href") %>%
    as.character()

  tablefn <- bind_cols(tnumber = fnnumbers, link = fnhrefs) %>%
    filter(grepl("#E", link)) %>%
    unique() %>%
    mutate(tnumber = paste0("(",tnumber,")"))

  tableswap <- tablefn$link %>% as.character()
  names(tableswap) <- tablefn$tnumber

  return(tableswap)
}

# get the swaps for each table

swaps <- map(c(4:12), getfnswap)

# function to bring it all together

makectable <- function(table, swap) {
  table %>%
    firstclean() %>%
    mutate_all(., ~str_replace_all(., fixed(swap))) %>%
    mutate_all(., ~str_replace_all(., fixed(linkreplace))) %>%
    replace(is.na(.), "")
}

cleantables <- map2(tables, swaps, function(x,y) makectable(x,y))


# section 1: nitrates

nitrate <- cleantables[[1]] %>%
  getnames()

write_excel_csv(nitrate, "./csv/contaminants/nitrate.csv")

# section 2: mycotoxins

mycotoxins <- cleantables[[2]] %>%
  mutate(duplines = ifelse(X3 == X4 & X4 == X5 & !grepl("Maximum level", X4), "1", "")) %>%
  mutate(X4 = ifelse(duplines == 1, "", X4)) %>%
  mutate(X5 = ifelse(duplines == 1, "", X5)) %>%
  select(-duplines) %>%
  mutate(X2 = extraspace(X2)) %>%
  getnames()

write_excel_csv(mycotoxins, "./csv/contaminants/mycotoxins.csv")

# section 3: metals

metals <- cleantables[[3]] %>%
  mutate(X2 = str_replace(X2, "^-", "    ")) %>%
  mutate(X2 = extraspace(X2)) %>%
  getnames()

write_excel_csv(metals, "./csv/contaminants/metals.csv")

# section 4: 3-monochloropropanediol (3-MCPD) and glycidyl fatty acid esters

mcpd <- cleantables[[4]] %>%
  mutate(X3 = extraspace(X3)) %>%
  getnames()

write_excel_csv(mcpd, "./csv/contaminants/mcpd.csv")

# section 5: dioxins and PCBS

# (needs a little manipulation to get the specific maximum limits due to rowspans)

limits <- contam %>%
  html_nodes("table") %>%
  .[8] %>%
  html_nodes("tr") %>%
  .[2] %>%
  map(., html_nodes, "td") %>%
  map(., html_text) %>%
  .[[1]] %>%
  trimws(.)

assert_that(length(limits) == 3, msg = "contaminants, not picking up section 5 specific limits")

dioxins <- cleantables[[5]] %>%
  slice(-2) %>%
  add_row(X1 = "",
          X2 = "",
          X3 = limits[1],
          X4 = limits[2],
          X5 = limits[3],
          .after = 1) %>%
  getnames()

write_excel_csv(dioxins, "./csv/contaminants/dioxins.csv")

# section 6: Polycyclic aromatic hydrocarbons

poly <- cleantables[[6]] %>%
  mutate_at(vars(X3, X4), ~str_replace_all(str_replace_all(., "\n", "; "), "(\\s){2,}", " ")) %>%
  mutate_at(vars(X2), ~str_replace_all(str_replace_all(., "(\\s){2,}", "; "), "; \\(", " (")) %>%
  getnames()

write_excel_csv(poly, "./csv/contaminants/poly.csv")

# section 7: Melamine and its structural analogues

melamine <- cleantables[[7]] %>%
  getnames()

write_excel_csv(melamine, "./csv/contaminants/melamine.csv")

# section 8: Inherent plant toxins


toxins1 <- cleantables[[8]] %>% slice(-1)
toxins2 <- cleantables[[9]] %>% mutate_at(vars(X1, X2), ~gsub("^$", NA, .)) %>%
  fill(X1, X2) %>%
  slice(-2)
toxins <- bind_rows(toxins2, toxins1) %>% arrange(X1) %>% getnames(nrow(.)) %>%
  replace(is.na(.), "")

write_excel_csv(toxins, "./csv/contaminants/toxins.csv")
