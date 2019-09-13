#' Get the most up to date consolidated version from the legislation.gov.uk page
#'
#' @param url URL of the legislation on legislation.gov.uk
geturl <- function(url) {
  xml2::read_html(url) %>%
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

#' Get table n from a html document (badly formatted)
#'
#' @param htmldoc the html document from the relevant page
#' @param n number of the table
#' @param ag table rows will be excluded that contain this string
#'
getbadtable <- function(htmldoc, n, ag = "ancestor") {
  htmldoc %>%
    rvest::html_nodes("table") %>%
    .[n] %>%
    rvest::html_nodes("tr") %>%
    .[!grepl(ag, .)] %>%
    as.character() %>%
    paste0(collapse = "") %>%
    paste0("<table>",.,"</table>") %>%
    xml2::read_html() %>%
    rvest::html_table(fill = TRUE) %>%
    .[[1]] %>%
    tibble::as_tibble()
}

#' Generic cleaning functions
#' Some initial cleaning functions that are applied to all tables;
#' standardising hyphens, removing the markers for legislation updates
#'
#' @param table The table you have scraped from the page and now want to clean
#'
firstclean <- function(table) {

  badhyphens <- "\u002D|\u2010|\u2011|\u2012|\u2013|\u2014|\u2015|\u2E3A|\u2E3B|\uFE58|\uFE63|\uFF0D"

  table %>%
    dplyr::mutate_all(., ~stringr::str_replace_all(., "(\u25BC)([A-z0-9]+)", "")) %>%
    dplyr::mutate_all(., ~stringr::str_replace_all(., "(\u25BA)([A-z0-9]+)", "")) %>%
    dplyr::mutate_all(., ~stringr::str_replace_all(., "\u25C4", "")) %>%
    dplyr::mutate_all(., ~stringr::str_replace_all(., badhyphens, "-")) %>%
    dplyr::mutate_all(., ~stringr::str_replace_all(., "[\\-]+$", "")) %>%
    dplyr::mutate_all(., ~stringr::str_replace_all(., "^(\\s)+$", "")) %>%
    dplyr::mutate_all(., ~dplyr::na_if(., "")) %>%
    janitor::remove_empty("rows")
}

#' Get names
#' Take a table, make the column names the top row, slice off the top row
#'
#' @param table The table you want to perform this function on
#' @param n The row number you want the names to be (default = 1)
#'
getnames <- function(table, n = 1) {
  names(table) <- table[n,] %>% as.character()
  table %>% dplyr::slice(-n)
}

#' Euro to British decimals
#' Converts comma decimal numbers to dot decimal numbers
#' To be used judiciously on carefully selected columns - it could turn things
#' like flavouring names into nonsense!
#'
#' @param x The string to be converted
#'
decimals <- function(x) {
  gsub("([0-9]+)(,)([0-9]+)", "\\1.\\3", x)
}

#' Clean up extra spaces
#' Cleans up cells with lots of newlines and whitespace, preserving any newlines
#'
#' @param x The string to be converted
#'
extraspace <- function(x) {
  str_replace_all(x, "\n", "NEWLINE") %>%
    str_replace_all(., "(\\s){2,}", "") %>%
    str_replace_all(., "NEWLINE", "\n ") %>%
    str_replace_all(., "(\n ){2,}", "\n")
}

#' Cells that are just bracketed numbers - precede with ' to avoid excel turning
#' it into a negative number
#'
#' @param x The string to be converted
#'
bracketonly <- function(x) {
  str_replace_all(x, "(^\\([0-9]+\\)$)", "'\\1")
}

