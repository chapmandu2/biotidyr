#' @title Crush cell line id format
#'
#' @description
#' Helper function to remove punctuation and whitespace from cell line id's.  This helps convert id's between datasets.
#'
#' @param x character vector of ids
#' @return character vector of crushed ids
#' @export crush
#'
#' @examples
#' data("CCLEsmall", package='PharmacoGx')
#' cellNames(CCLEsmall)[1:20]
#' crush(cellNames(CCLEsmall)[1:20])

crush <- function(x) {
    gsub('[[:punct:]]|[[:space:]]','', x)
}
