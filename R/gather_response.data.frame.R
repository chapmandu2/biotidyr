#' gather_response.data.frame
#'
#' @param x a data frame object
#' @param sample_ids A vector of sample ids.  Default is NULL (don't filter on sample id)
#' @param compound_ids A vector of compound ids.  Default is NULL (don't filter on compound id)
#' @param endpoints A vector of endpoints.  Default is NULL (don't filter on endpoint)
#' @param resp_ids A vector of response ids.  Default is NULL (don't filter on response id)
#'
#' @return tibble
#' @export
#'
#' @examples
#' data('dietlein_data')
#' gather_response.data.frame(dietlein_data, sample_ids=c('NCIH1703_LUNG', 'NCIH1792_LUNG'))
#' gather_response.data.frame(dietlein_data, compound_ids='KU60648')
#' gather_response.data.frame(dietlein_data, endpoints='pGI50')
#' gather_response.data.frame(dietlein_data, resp_ids='KU60648_pGI50')
#'
gather_response.data.frame <- function(x, sample_ids=NULL, compound_ids=NULL, endpoints=NULL, resp_ids=NULL) {

    drugInfo <- x %>%
        dplyr::mutate(resp_id = paste(compound_id, endpoint, sep='_'),
                      original = as.character(original))

    if (!is.null(sample_ids)) {
        drugInfo <- drugInfo %>% dplyr::filter(sample_id %in% sample_ids)
    }

    if (!is.null(compound_ids)) {
        drugInfo <- drugInfo %>% dplyr::filter(compound_id %in% compound_ids)
    }

    if (!is.null(endpoints)) {
        drugInfo <- drugInfo %>% dplyr::filter(endpoint %in% endpoints)
    }

    if (!is.null(resp_ids)) {
        drugInfo <- drugInfo %>% dplyr::filter(resp_id %in% resp_ids)
    }

    drugInfo %>% dplyr::transmute(sample_id, assayed_id=resp_id, data_type='resp', original, value)


}
