context("Flavourings tests")
suppressWarnings(library(magrittr))
suppressWarnings(library(dplyr))
suppressWarnings(library(rvest))
suppressWarnings(library(readr))

url <- read_csv("legislation-urls.csv") %>%
  filter(product == "flavourings") %>%
  .[1,2] %>% as.character() %>% geturl()
geta <- gettable(url, 4)
gete <- gettable(url, 5)
cleana <- firstclean(geta) %>%
  slice(-1:-2)
anytriangle <- cleana %>%
  filter_at(vars(contains("X")), ~grepl("\u25BC", .)) %>%
  nrow(.)
anydash <- cleana %>%
  filter_at(vars(contains("X")),
            ~grepl("\u002D|\u2010|\u2011|\u2012|\u2013|\u2014|\u2015|\u2E3A|\u2E3B|\uFE58|\uFE63|\uFF0D", .)) %>%
  nrow(.)

test_that("the webpage has the expected number of tables", {

  ntables <- counttables(url)

  expect_equal(ntables, 10)
})

test_that("the scraped tables are of the expected dimensions", {

  expect_equal(ncol(geta), 9)
  expect_gte(nrow(geta), 100)
  expect_match(as.character(geta[2,1]), "FL")

  expect_equal(ncol(gete), 9)
  expect_match(as.character(gete[1,1]), "FL")

})

test_that("the cleaning functions behave as expected", {

  # no modification triangles in any cell

  expect_equal(anytriangle, 0)

  # no bad hyphens in any cell

  expect_equal(anydash, 0)

})
