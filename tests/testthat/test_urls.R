context("URL tests")
suppressWarnings(library(magrittr))
suppressWarnings(library(dplyr))
suppressWarnings(library(purrr))
suppressWarnings(library(readr))

# take the first legislation.gov.uk url and test whether geturl() is generating
# what looks like a sensible EUR-Lex link

eulexurl <- read_csv("legislation-urls.csv") %>%
  filter(product == "flavourings") %>%
  .[1,2] %>% as.character() %>% geturl()

test_that("url generated is correct form", {

  expect_match(eulexurl, "eur-lex")
  expect_match(eulexurl, "CELEX:02008R1334")

})
