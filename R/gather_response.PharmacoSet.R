#' gather_response.PharmacoSet
#'
#' @param x PharmacoSet object
#' @param sample_ids A vector of sample ids.  Default is NULL (don't filter on sample id)
#' @param resp_ids A vector of response ids.  Default is NULL (don't filter on response id)
#' @param resp_col Response variable to retrieve
#'
#' @return tibble
#' @export
#'
#' @examples
#' data('CCLEsmall', package='PharmacoGx')
#' gather_response.PharmacoSet(CCLEsmall, sample_ids=c('CHL-1', 'SW1573'),
#'                          resp_ids=c('AEW541', 'Nilotinib'), resp_col='ic50_published')
gather_response.PharmacoSet <- function(x, sample_ids=NULL, resp_ids=NULL, resp_col='ic50_published') {

    drugInfo <- x@sensitivity$info %>%
        tibble::rownames_to_column('drugid_cellid') %>%
        dplyr::select(drugid_cellid, cellid, drugid) %>%
        dplyr::filter(cellid %in% sample_ids & drugid %in% resp_ids) %>%
        dplyr::tbl_df()

    drugData <- x@sensitivity$profiles %>%
        as.data.frame() %>%
        tibble::rownames_to_column('drugid_cellid') %>%
        dplyr::transmute_('drugid_cellid', 'value'=resp_col) %>%
        dplyr::inner_join(drugInfo, by='drugid_cellid') %>%
        dplyr::tbl_df()

    drugData %>%
        dplyr::transmute(unified_id=cellid, assayed_id=drugid, data_type='resp', original=as.character(value), value)

}
