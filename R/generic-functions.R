#' Get the most up to date consolidated version from the legislation.gov.uk page
#'
#' @param legurl URL of the legislation on legislation.gov.uk
geturl <- function(legurl) {
  xml2::read_html(legurl) %>%
    rvest::html_nodes(xpath = "//*[@id=\"ukregfromeu\"]") %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href") %>%
    .[2]
}

#' Get number of tables
#'
#' @param url URL of the page you want to count the tables on
#'
counttables <- function(url) {
  xml2::read_html(url) %>%
    rvest::html_nodes("table") %>%
    length()
}

#' Get table number n from webpage url
#'
#' @param url URL of the page you want to get the table from
#' @param n number of the table
#'
gettable <- function(url, n) {
  xml2::read_html(url) %>%
    rvest::html_nodes("table") %>%
    .[n] %>%
    rvest::html_table(fill = TRUE) %>%
    .[[1]] %>%
    tibble::as_tibble()
}

#' Generic cleaning functions
#' Some initial cleaning functions that are applied to all tables;
#' standardising hyphens, removing the markers for legislation updates
#'
#' @param table The table you have scraped from the page and now want to lean
#'
firstclean <- function(table) {

  badhyphens <- "\u002D|\u2010|\u2011|\u2012|\u2013|\u2014|\u2015|\u2E3A|\u2E3B|\uFE58|\uFE63|\uFF0D"

  table %>%
    dplyr::mutate_all(., ~stringr::str_replace_all(., "(\u25BC)([A-z0-9]+)", "")) %>%
    dplyr::mutate_all(., ~stringr::str_replace_all(., badhyphens, "-")) %>%
    dplyr::mutate_all(., ~stringr::str_replace_all(., "[\\-]+$", "")) %>%
    dplyr::mutate_all(., ~stringr::str_replace_all(., "^(\\s)+$", "")) %>%
    dplyr::mutate_all(., ~dplyr::na_if(., "")) %>%
    janitor::remove_empty("rows")
}
