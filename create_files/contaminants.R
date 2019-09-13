# SCRIPT TO PRODUCE THE UNION LIST OF APPROVED CONTAMINANTS

library(dplyr)
library(magrittr)
library(tidyr)
library(stringr)
library(purrr)
library(readr)
library(eulegscrape)
library(rvest)

url <- read_csv("./reference/legislation-urls.csv") %>%
  filter(product == "contaminants") %>%
  .[1,2] %>% as.character() %>%
  geturl()

contam <- read_html(url)

# get footnotes

fn <- contam %>% html_nodes(".footnote")

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

# table of footnotes to write out

footnotes <- fnlinks[,3] %>% rename(Footnote = fntext)

write_excel_csv(footnotes, "./csv/contaminants/footnotes.csv")

# lookup table to replace footnote links with correct numbers in the tables

linkreplace <- fnlinks$fnumber %>% as.character()
names(linkreplace) <- fnlinks$link



# function to get the table
# swap footnote numbers for links
# swap links for correct footnote numbers


ctable <- function(n) {
  tablehtml <- contam %>%
    html_nodes("table") %>%
    .[n]

  cleantable <- tablehtml %>%
    html_nodes("tr") %>%
    .[!grepl("ancestor", .)] %>%
    as.character() %>%
    paste0(collapse = "") %>%
    paste0("<table>",.,"</table>") %>%
    read_html() %>%
    html_table(fill = TRUE) %>%
    .[[1]] %>%
    as_tibble() %>%
    firstclean()

  fnnumbers <- tablehtml %>%
    html_nodes("a") %>%
    map(., html_nodes, ".superscript") %>%
    map(., html_text) %>%
    as.character()

  fnhrefs <- tablehtml %>%
    html_nodes("a") %>%
    map(., html_attr, "href") %>%
    as.character()

  tablefn <- bind_cols(tnumber = fnnumbers, link = fnhrefs) %>%
    filter(grepl("#E", link)) %>%
    unique() %>%
    mutate(tnumber = paste0("(",tnumber,")"))

  tableswap <- tablefn$link %>% as.character()
  names(tableswap) <- tablefn$tnumber

  cleantable %>%
    mutate_all(., ~str_replace_all(., fixed(tableswap))) %>%
    mutate_all(., ~str_replace_all(., fixed(linkreplace))) %>%
    replace(is.na(.), "")
}

# section 1: nitrates

nitrate <- ctable(3) %>%
  getnames()

write_excel_csv(nitrate, "./csv/contaminants/nitrate.csv")

# section 2: mycotoxins

mycotoxins <- ctable(4) %>%
  mutate(duplines = ifelse(X3 == X4 & X4 == X5 & !grepl("Maximum level", X4), "1", "")) %>%
  mutate(X4 = ifelse(duplines == 1, "", X4)) %>%
  mutate(X5 = ifelse(duplines == 1, "", X5)) %>%
  select(-duplines) %>%
  mutate(X2 = extraspace(X2)) %>%
  getnames()

write_excel_csv(mycotoxins, "./csv/contaminants/mycotoxins.csv")

# section 3: metals

metals <- ctable(5) %>%
  mutate(X2 = str_replace(X2, "^-", "    ")) %>%
  mutate(X2 = extraspace(X2)) %>%
  getnames()

write_excel_csv(metals, "./csv/contaminants/metals.csv")

# section 4: 3-monochloropropanediol (3-MCPD) and glycidyl fatty acid esters

mcpd <- ctable(6) %>%
  mutate(X3 = extraspace(X3)) %>%
  getnames()

write_excel_csv(mcpd, "./csv/contaminants/mcpd.csv")

# section 5: dioxins and PCBS

# (needs a little manipulation to get the specific maximum limits due to rowspans)

limits <- contam %>%
  html_nodes("table") %>%
  .[7] %>%
  html_nodes("tr") %>%
  .[2] %>%
  map(., html_nodes, "td") %>%
  map(., html_text) %>%
  .[[1]] %>%
  trimws(.)

dioxins <- ctable(7) %>%
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

poly <- ctable(8) %>%
  mutate_at(vars(X3, X4), ~str_replace_all(str_replace_all(., "\n", "; "), "(\\s){2,}", " ")) %>%
  mutate_at(vars(X2), ~str_replace_all(str_replace_all(., "(\\s){2,}", "; "), "; \\(", " (")) %>%
  getnames()

write_excel_csv(poly, "./csv/contaminants/poly.csv")

# section 7: Melamine and its structural analogues

melamine <- ctable(9) %>%
  getnames()

write_excel_csv(melamine, "./csv/contaminants/melamine.csv")

# section 8: Inherent plant toxins


toxins1 <- ctable(10) %>% slice(-1)
toxins2 <- ctable(11) %>% mutate_at(vars(X1, X2), ~gsub("^$", NA, .)) %>%
  fill(X1, X2) %>%
  slice(-2)
toxins <- bind_rows(toxins2, toxins1) %>% arrange(X1) %>% getnames(nrow(.)) %>%
  replace(is.na(.), "")

write_excel_csv(toxins, "./csv/contaminants/toxins.csv")
