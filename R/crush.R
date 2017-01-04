#' @title Crush cell line id format
#'
#' @description
#' Helper function to remove punctuation and whitespace from cell line id's.  This helps convert id's between datasets.
#'
#' @param x character vector of ids
#' @param upper_case boolean - return in upper case or not.  Default FALSE
#' @return character vector of crushed ids
#' @export crush
#'
#' @examples
#'
#' data("CCLEsmall", package='PharmacoGx')
#' PharmacoGx::cellNames(CCLEsmall)[1:20]
#' crush(PharmacoGx::cellNames(CCLEsmall)[1:20])

crush <- function(x, upper_case=FALSE) {
    out <- gsub('[[:punct:]]|[[:space:]]','', x)

    if(upper_case) {
        return(toupper(out))
    } else {
        return(out)
    }

}
