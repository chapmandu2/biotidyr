#' @title
#' Convert response data from a PharmacoSet into tidy format
#'
#' @description
#' Convert response data from a \linkS4class{PharmacoSet} into tidy format
#'
#' @param x PharmacoSet object
#' @param sample_ids A vector of sample ids.  Default is NULL (don't filter on sample id)
#' @param resp_ids A vector of response ids.  Default is NULL (don't filter on response id)
#' @param resp_col Response variable to retrieve
#' @param extra_cols Optional additional columns to retrieve from the PharmacoSet drug info. Default is NULL
#'
#' @return a data frame in tall_df format
#' @export gather_response.PharmacoSet
#'
#'
#' @examples
#' data('CCLEsmall', package='PharmacoGx')
#' gather_response.PharmacoSet(CCLEsmall, sample_ids=c('CHL-1', 'SW1573'),
#'                          resp_ids=c('AEW541', 'Nilotinib'),
#'                          resp_col='ic50_published')
#'
#' gather_response.PharmacoSet(CCLEsmall, sample_ids=c('CHL-1', 'SW1573'),
#'                          resp_ids=c('AEW541', 'Nilotinib'),
#'                          resp_col='ic50_published', extra_cols='nbr.conc.tested')
gather_response.PharmacoSet <- function(x, sample_ids=NULL, resp_ids=NULL,
                                        resp_col='ic50_published', extra_cols=NULL) {

    #include all samples and response values if either is not specified
    if(is.null(sample_ids)) {
        sample_ids <- PharmacoGx::cellNames(x)
    }

    if(is.null(resp_ids)) {
        resp_ids <- PharmacoGx::drugNames(x)
    }

    drugInfo <- x@sensitivity$info %>%
        tibble::rownames_to_column('drugid_cellid') %>%
        dplyr::select(dplyr::one_of(c('drugid_cellid', 'cellid', 'drugid', extra_cols))) %>%
        dplyr::filter(cellid %in% sample_ids & drugid %in% resp_ids) %>%
        dplyr::tbl_df()

    drugData <- x@sensitivity$profiles %>%
        as.data.frame() %>%
        tibble::rownames_to_column('drugid_cellid') %>%
        dplyr::transmute_('drugid_cellid', 'value'=resp_col) %>%
        dplyr::inner_join(drugInfo, by='drugid_cellid') %>%
        dplyr::tbl_df()

    drugData %>%
        dplyr::mutate(sample_id=cellid, assayed_id=drugid, data_type='resp',
                      original=as.character(value)) %>%
        dplyr::select(dplyr::one_of('sample_id', 'assayed_id', 'data_type',
                         'original', 'value', extra_cols))

}
