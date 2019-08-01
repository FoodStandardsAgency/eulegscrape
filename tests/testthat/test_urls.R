context("URL tests")

eurl <- geturl("https://www.legislation.gov.uk/eur/2008/1334/contents")

test_that("url generated is correct form", {

  expect_match(eurl, "eur-lex")
  expect_match(eurl, "CELEX:02008R1334")

})
