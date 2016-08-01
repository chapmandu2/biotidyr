#' gather.ExpressionSet
#'
#' Convert an ExpressionSet object into tidy format
#'
#' @param x An ExpressionSet
#' @param sample_ids A vector of sample ids.  Default is NULL (don't filter on sample id)
#' @param gene_ids A vector of gene ids.  Default is NULL (don't filter on gene id)
#' @param sample_col Name of the column in the pData data frame to use for filtering on sample id
#' @param gene_col Name of the column in the rowData data frame to use for filtering on gene id
#'
#' @return tibble
#' @export
#' @importFrom magrittr %>%
#'
#' @examples
#' data('CCLEsmall', package='PharmacoGx')
#' eset <- CCLEsmall@molecularProfiles$rna
#' gather.ExpressionSet(eset, sample_ids=c('143B', '23132-87'), gene_ids=c('BRAF', 'EGFR') )
gather.ExpressionSet <- function(x, sample_ids=NULL, gene_ids=NULL, sample_col='cellid', gene_col='Symbol') {


    Biobase::fData(x) <- Biobase::fData(x) %>%
        dplyr::rename_("fData_gene_id"=gene_col)
    geneIdx <- Biobase::fData(x)$fData_gene_id %in% gene_ids
    geneInfo <- Biobase::fData(x) %>%
        tibble::rownames_to_column('fData_row_id') %>%
        dplyr::transmute(fData_row_id, fData_gene_id) %>%
        dplyr::filter(geneIdx)


    Biobase::pData(x) <- Biobase::pData(x) %>% dplyr::rename_('pData_sample_id'=sample_col)
    clIdx <- Biobase::pData(x)$pData_sample_id %in% sample_ids
    clInfo <- Biobase::pData(x) %>%
        tibble::rownames_to_column('pData_row_id') %>%
        dplyr::select(pData_row_id, pData_sample_id) %>%
        dplyr::filter(clIdx)

    x_flt <- x[geneIdx, clIdx]

    dat <- x_flt %>%
        Biobase::exprs() %>%
        as.data.frame() %>%
        tibble::rownames_to_column('fData_row_id') %>%
        tidyr::gather('pData_row_id', 'value', -fData_row_id) %>%
        dplyr::inner_join(geneInfo, by='fData_row_id') %>%
        dplyr::inner_join(clInfo, by='pData_row_id') %>%
        dplyr::tbl_df()

    return(dat)
}
