#' makeGeneticVsGeneticTibble.PharmacoSet
#'
#' @param x PharmacoSet object
#' @param sample_ids A vector of sample ids.  Default is NULL (don't filter on sample id)
#' @param gene1 First gene(s)
#' @param gene2 Second gene(s)
#' @param data_type1 Data type for gene 1
#' @param data_type2 Data type for gene 2
#' @param sample_col Name of the column in the pData data frame to use for filtering on sample id
#' @param gene_col Name of the column in the rowData data frame to use for filtering on gene id
#'
#' @return tibble
#' @export
#'
#' @examples
#' data('CCLEsmall', package='PharmacoGx')
#'
#' gvg_df <- makeGeneticVsGeneticTibble.PharmacoSet(CCLEsmall, sample_ids=cellNames(CCLEsmall), gene1='RBM5', gene2='RBM5',
#'  data_type1='rna', data_type2='rnaseq', gene_col1 = "Symbol", gene_col2 = "gene_name")
#' gvg_df
#' CancerCellLines::plotGeneticVsGeneticPoint(gvg_df)
makeGeneticVsGeneticTibble.PharmacoSet <- function(x, sample_ids=NULL, gene1, gene2, data_type1, data_type2,
                                                  sample_col = "cellid", gene_col1 = "Symbol", gene_col2 = "Symbol") {

    gene1_data <- gather_assay.PharmacoSet(x, sample_ids, gene_ids=gene1,
                                          data_type=data_type1, sample_col=sample_col, gene_col=gene_col1) %>%
        dplyr::transmute(unified_id, gene1 = assayed_id,
                         feature_type1 = data_type,
                         feature_name1 = paste(assayed_id, data_type, sep = "_"),
                         feature_value1 = value,
                         feature_original1 = original)

    gene2_data <- gather_assay.PharmacoSet(x, sample_ids, gene_ids=gene2,
                                          data_type=data_type2, sample_col=sample_col, gene_col=gene_col2) %>%
        dplyr::transmute(unified_id, gene2 = assayed_id,
                         feature_type2 = data_type,
                         feature_name2 = paste(assayed_id, data_type, sep = "_"),
                         feature_value2 = value,
                         feature_original2 = original)

    gene1_data %>%
        dplyr::inner_join(gene2_data, by = "unified_id")

}
