# SCRIPT TO PRODUCE THE UNION LIST OF APPROVED ADDITIVES

library(dplyr)
library(magrittr)
library(tidyr)
library(stringr)
library(readr)
library(purrr)
library(eulegscrape)

url <- read_csv("./reference/legislation-urls.csv") %>%
  filter(product == "additives") %>%
  .[1,2] %>% as.character() %>%
  geturl()

# category names (for later merging into table)

categories <- firstclean(gettable(url, 36)) %>%
  filter(X1 != X2) %>%
  mutate(X1 = gsub("\\.$", "", X1)) %>%
  slice(-1) %>%
  select(`Category number` = X1, `Category name` = X2)

# ANNEX II part E

addtable <- firstclean(gettable(url, 37)) %>%
  # shift along rows
  mutate(shift = if_else(grepl("^E", X1), 1, 0),
         X6 = if_else(shift == 1, X5, X6),
         X5 = if_else(shift == 1, X4, X5),
         X4 = if_else(shift == 1, X3, X4),
         X3 = if_else(shift == 1, X2, X3),
         X2 = if_else(shift == 1, X1, X2),
         X1 = ifelse(shift == 1, NA, X1)) %>%
  fill(X1) %>%
  select(-shift)

add <- addtable %>%
  # remove footnote rows
  filter(X1 != X2) %>%
  filter_at(vars(X2, X3), all_vars(!grepl("^\\([0-9]+\\)", .))) %>%
  # remove empty rows
  filter_at(vars(X3, X4, X5, X6), any_vars(!is.na(.))) %>%
  # get rid of category name lines
  filter(X2 != X3) %>%
  mutate(X6 = extraspace(X6)) %>%
  getnames() %>%
  # merge in category names
  left_join(categories) %>%
  replace(., is.na(.), "")

# BRINGING IN FOOTNOTE TEXT

fntable <- addtable %>%
  # footnotes are the ones that are repeated across columns...
  filter(X3 == X4) %>%
  select(X3) %>%
  #.. and start with a bracket
  filter(grepl("^\\(", X3)) %>%
  # to get footnote text
  # make sure bracketed number is followed by a colon
  mutate(fntext = str_replace(X3, "\\)\\s", "\\): ")) %>%
  # extract everything after a semicolon and remove the semicolon and leading space
  mutate(fntext = str_remove(str_extract(fntext, ":.*"), "^:\\s+")) %>%
  # to get footnote number, extract anything that is a number (or asterisk) in brackets
  mutate(fnn = str_extract(X3, "\\([0-9\\*]+\\)")) %>%
  select(fnn, fntext) %>% unique() %>%
  group_by(fnn) %>%
  filter(row_number()==1) %>%
  ungroup()

fnrep <- fntable$fntext
names(fnrep) <- fntable$fnn

additives <- add %>%
  # remove a/b from (11)a and (11)b
  mutate(Footnotes = str_replace(Footnotes, "\\)[A-z]", "\\)")) %>%
  # put a semicolon between footnotes
  mutate(Footnotes = str_replace_all(Footnotes, fixed(")("), ") ; (")) %>%
  mutate(Footnotes = str_replace_all(Footnotes, fixed(") ("), ") ; (")) %>%
  #replace numbers with text equivalents
  mutate(Footnotes = str_replace_all(Footnotes, fixed(fnrep)))

write_excel_csv(additives, "./csv/additives/additives.csv")


# ANNEX III

additives2 <- lapply(c(38:43), function(n) firstclean(gettable(url, n)) %>%
                     filter(X1!=X2) %>%
                     mutate(X3 = str_replace(X3, "^-", "    ")) %>%
                     mutate_all(., ~extraspace(.)) %>%
                     mutate_all(., ~bracketonly(.)) %>%
                     getnames(.) %>%
                     replace(., is.na(.), ""))

map(c(1:6), function(x) write_excel_csv(additives2[[x]], paste0("./csv/additives/additives2_",x,".csv")))
