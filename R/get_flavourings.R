#' Get clean list of flavourings
#'
#' @export
get_flavourings <- function() {
  url <- "https://eur-lex.europa.eu/legal-content/EN/TXT/?uri=CELEX:02008R1334-20181126"

  parta <- firstclean(gettable(url, 4))

  colnames <- parta[2,] %>% as.character()

  names(parta) <- colnames

  parta <- parta %>%
    dplyr::slice(-1:-2)

  parte <- firstclean(gettable(url, 5)) %>%
    dplyr::slice(2)

  names(parte) <- colnames

  dplyr::bind_rows(parta, parte) %>%
    replace(., is.na(.), "")
}
