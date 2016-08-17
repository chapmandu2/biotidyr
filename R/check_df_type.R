#' Checks the format of a data frame
#'
#' Helper function to determine whether a supplied data frame is a specific fomat
#' defined in the package.  Either returns a boolean or throws a stop.
#'
#' @param df query data frame
#' @param format format to test - see ?data_formats
#' @param dev_mode stop if test is false.  Default is FALSE
#' @return string of data frame type
#' @export
#'
#' @examples
#' data(dietlein_data)
#' biotidyr:::check_df_type(dietlein_data, 'resp_df')
#'
#' data(example_tall_df)
#' biotidyr:::check_df_type(example_tall_df, 'tall_df')
#'
#' data(mtcars)
#' biotidyr:::check_df_type(mtcars, 'tall_df')
check_df_type <- function(df, format, dev_mode=FALSE) {

    res <- identical(get_df_type(df), format)

    if(dev_mode & !res) {
        stop(sprintf('data.frame should be in %s format, see ?data_formats', format))
    }

    return(res)

}
