context("URL tests")
suppressWarnings(library(magrittr))
suppressWarnings(library(dplyr))
suppressWarnings(library(purrr))

# take the first legislation.gov.uk url and test whether geturl() is generating
# what looks like a sensible EUR-Lex link

uklegurl <- legurls %>%
  .[1,2] %>% as.character()

eulexurl <- geturl(uklegurl)

test_that("url generated is correct form", {

  expect_match(eulexurl, "eur-lex")
  expect_match(eulexurl, "CELEX:02008R1333")

})


# test whether the eur-lex urls in the data file are the most up to date

notmatching <- legurls %>%
  mutate(eulatest = map_chr(uklegurl, geturl)) %>%
  filter(eulexurl != eulatest) %>%
  nrow(.)

test_that("eur-lex link is most up to date", {
  expect_equal(notmatching, 0)
})
