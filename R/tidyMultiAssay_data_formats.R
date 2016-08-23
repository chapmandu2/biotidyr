#' @title Data frame formats used in the tidyMultiAssay package
#'
#' @description
#' The tidyMultiAssay package works with the following data frame formats:
#' \describe{
#'  \item{tall_df}{Common currency minimal data frame which can contain any data type}
#'  \item{resp_df}{Used to store user specified response data from a cell panel screen or siRNA screen}
#'  \item{gvg_df}{Genetic vs Genetic data frame used to model or plot one set of genetic features against another set}
#'  \item{rvg_df}{Response vs Genetic data frame used to model or plot a set of response data against a set of genetic features}
#'  }
#' The \code{\link{get_df_format}} and \code{\link{check_df_format}} functions can be used to determine and check the data frame format.
#'
#' @examples
#'
#' #tall_df example
#' data(example_tall_df)
#' dplyr::tbl_df(example_tall_df)
#'
#' #resp_df example
#' data(dietlein_data)
#' dplyr::tbl_df(dietlein_data)
#'
#' #gvg_df example
#' data(example_gvg_df)
#' dplyr::tbl_df(example_gvg_df)
#'
#' #rvg_df example
#' data(example_rvg_df)
#' dplyr::tbl_df(example_rvg_df)
#'
#' @name tidyMultiAssay-data-formats
#' @aliases data-formats
#' @author Phil Chapman <phil.chapman@cruk.manchester.ac.uk>
NULL
