#' @title
#' Convert assay data from a PharmacoSet into tidy format
#'
#' @description
#' Convert assay data from a \linkS4class{PharmacoSet} into tidy format
#'
#' @param x A PharmacoSet object
#' @param sample_ids A vector of sample ids.  Default is NULL (don't filter on sample id)
#' @param gene_ids A vector of gene ids.  Default is NULL (don't filter on gene id)
#' @param data_type Name of the component of the PharmacoSet object to gather
#' @param sample_col Name of the column in the pData data frame to use for filtering on sample id
#' @param gene_col Name of the column in the rowData data frame to use for filtering on gene id
#'
#' @return a data frame in tall_df format
#' @export
#'
#' @examples
#' data('CCLEsmall', package='PharmacoGx')
#' gather_assay.PharmacoSet(CCLEsmall, sample_ids=c('143B', '23132-87'),
#'                          gene_ids=c('BRAF', 'EGFR'), data_type='rna')
#' set.seed(20)
#' gather_assay.PharmacoSet(CCLEsmall, sample_ids=sample(PharmacoGx::cellNames(CCLEsmall), 30),
#'                          gene_ids=c('BRAF', 'EGFR'), data_type='mutation')
#'
#' gather_assay.PharmacoSet(CCLEsmall, sample_ids=sample(PharmacoGx::cellNames(CCLEsmall), 30),
#'  gene_ids=c('AK2', 'TFP1'), data_type='rnaseq', gene_col='gene_name')
#'
gather_assay.PharmacoSet <- function(x, sample_ids=NULL, gene_ids=NULL, data_type='rna', sample_col='cellid', gene_col='Symbol') {

    stopifnot(data_type %in% names(x@molecularProfiles))
    y <- x@molecularProfiles[[data_type]]

    dat <- gather.ExpressionSet(y, sample_ids=sample_ids, gene_ids=gene_ids,
                                sample_col=sample_col, gene_col=gene_col) %>%
        dplyr::transmute(sample_id=pData_sample_id, assayed_id=fData_gene_id,
                         data_type=data_type, original=value)

    if(typeof(Biobase::exprs(y)) == 'character') {
        dat <- dplyr::mutate(dat, value=ifelse(original=='wt',0,1))
    } else {
        dat <- dplyr::mutate(dat, value=original, original=as.character(original))
    }

    return(dat)

}
