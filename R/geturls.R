#' Get latest EUR-Lex url for a piece of legislation
#'
#' @param legurl The link to the legislation on legislation.gov.uk
#'
#' @export
geturl <- function(legurl) {
  xml2::read_html(legurl) %>%
    rvest::html_nodes(xpath = "//*[@id=\"ukregfromeu\"]") %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href") %>%
    .[2]
}

#' Update the list of urls with the latest consolidated versions
#'
geturls <- function() {
  readr::read_csv("./data/legislation-urls.csv") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(eulexurl = geturl(uklegurl)) %>%
    readr::write_csv("./data/legislation-urls.csv")
}
