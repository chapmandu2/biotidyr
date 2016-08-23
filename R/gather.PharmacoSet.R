#' @title
#' Convert a PharmacoSet object into tidy format
#'
#' @description
#' Convert a \linkS4class{PharmacoSet} object into tidy format.  This is essentially a wrapper function
#' for \code{\link{gather_response.PharmacoSet}} and \code{\link{gather_assay.PharmacoSet}}
#'
#' @param x PharmacoSet object
#' @param sample_ids A vector of sample ids.  Default is NULL (don't filter on sample id)
#' @param gene_ids A vector of gene ids.  Default is NULL (don't filter on gene id)
#' @param resp_ids A vector of response ids.  Default is NULL (don't filter on response id)
#' @param sample_col Name of the column in the pData data frame to use for filtering on sample id
#' @param gene_col Name of the column in the rowData data frame to use for filtering on gene id
#' @param resp_col Response variable to retrieve
#' @param data_types Names of the components of the PharmacoSet object to gather
#' @param resp_df Data frame to process response data from (rather than PharmcaGx object)
#'
#' @return a tibble
#' @export
#'
#' @examples
#' data('CCLEsmall', package='PharmacoGx')
#'
#' gather.PharmacoSet(CCLEsmall, sample_ids=c('143B', '23132-87'), gene_ids=c('BRAF', 'EGFR'),
#'  data_types=c('rna', 'mutation'), gene_col=c('Symbol', 'Symbol'))
#'
#' gather.PharmacoSet(CCLEsmall, sample_ids=c('CHL-1', 'SW1573'), gene_ids=c('BRAF', 'EGFR'),
#'  resp_ids=c("AEW541","Nilotinib","PHA-665752","lapatinib"),
#'  data_types=c('rna', 'mutation'), gene_col=c('Symbol', 'Symbol'), resp_col='ic50_published')
#'
#'
#'
gather.PharmacoSet <- function(x, sample_ids=NULL, gene_ids=NULL, resp_ids=NULL,
                              sample_col='cellid', gene_col=c('Symbol', 'gene_name'),
                              resp_col='ic50_published', data_types=c('rna', 'mutation'), resp_df=NULL) {


    stopifnot(length(gene_col) == length(data_types))

    genetic_data <- purrr::map2(data_types, gene_col, function(z1, z2) {
        gather_assay.PharmacoSet(x, sample_ids=sample_ids, gene_ids=gene_ids,
                                data_type=z1, sample_col=sample_col, gene_col=z2)
        }) %>%
        dplyr::bind_rows()

    if(is.null(resp_ids)) {
        return(genetic_data)
    }  else {
        drug_data <- gather_response.PharmacoSet(x, sample_ids=sample_ids, resp_ids=resp_ids, resp_col=resp_col)
        dplyr::bind_rows(genetic_data, drug_data)
    }


}
