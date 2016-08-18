#' Determine type of data frame
#'
#' Helper function to determine whether a supplied data frame is of any of the
#' formats defined in the package.
#'
#' @param df query data frame
#' @return string of data frame type
#' @export
#'
#' @examples
#' data(dietlein_data)
#' biotidyr:::get_df_format(dietlein_data)
#'
#' data(example_tall_df)
#' biotidyr:::get_df_format(example_tall_df)
#'
#' data(example_gvg_df)
#' biotidyr:::get_df_format(example_gvg_df)
#'
#' data(example_rvg_df)
#' biotidyr:::get_df_format(example_rvg_df)
#'
#' data(mtcars)
#' biotidyr:::get_df_format(mtcars)
get_df_format <- function(df) {

    compare_df <- function(query_df, template_df, format_id) {
        data(list=template_df)
        template_df_format <- purrr::map_chr(get(template_df), class)
        query_df_format <- purrr::map_chr(query_df, class)

        if(identical(template_df_format, query_df_format)) {
            return(format_id)
        } else if (identical(names(template_df_format), names(query_df_format)) &
                   !identical(unname(template_df_format), unname(query_df_format))) {
            stop(sprintf('%s format but column types incorrect, try data("%s") for an example', format_id, template_df))
            #return(format_id)
        } else {
            return(NULL)
        }

    }

    res <- c(compare_df(df, 'dietlein_data', 'resp_df'),
            compare_df(df, 'example_tall_df', 'tall_df'),
            compare_df(df, 'example_gvg_df', 'gvg_df'),
            compare_df(df, 'example_rvg_df', 'rvg_df'))

    if(is.null(res)) {
        return('unknown')
    } else {
        return(res)
    }

}
