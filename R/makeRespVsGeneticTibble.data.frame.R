#' makeRespVsGeneticTibble.data.frame
#'
#' @param df data.frame object
#' @param df2 Second data.frame object.  Default is NULL (just use the first data.frame)
#' @param sample_ids A vector of sample ids.  Default is NULL (don't filter on sample id)
#' @param gene_ids Gene ids in df.  Default is NULL (don't filter on gene id)
#' @param data_types Data type for genetic data.  Default is NULL (don't filter on data type)
#' @param compound_ids A vector of compound ids.  Default is NULL (don't filter on compound id)
#' @param endpoints A vector of endpoints.  Default is NULL (don't filter on endpoint)
#' @param resp_ids A vector of response ids.  Default is NULL (don't filter on response id)
#'
#' @return tibble
#' @export
#'
#' @examples
#' data('CCLEsmall', package='PharmacoGx')
#'
#' genetic_data <- gather_assay.PharmacoSet(CCLEsmall, sample_ids=PharmacoGx::cellNames(CCLEsmall),
#'   gene_ids = c('RBM5', 'LAP3', 'CFTR'), data_type = 'rna')
#' resp_data <- gather_response.PharmacoSet(CCLEsmall, sample_ids=PharmacoGx::cellNames(CCLEsmall),
#'                          resp_ids=c('AEW541', 'Nilotinib'), resp_col='ic50_published')
#'
#' rvg_df <- makeRespVsGeneticTibble.data.frame(df=dat1, df2=dat2, sample_ids=PharmacoGx::cellNames(CCLEsmall), gene_ids='RBM5',
#'  data_types='rna', resp_ids='Nilotinib')
#' rvg_df
#' CancerCellLines::plotGeneticVsGeneticPoint(gvg_df)
makeRespVsGeneticTibble.data.frame <- function(df, df2=NULL, sample_ids=NULL, gene_ids=NULL, data_types=NULL, compound_ids=NULL, endpoints=NULL, resp_ids=NULL) {

    if(is.null(df2)) {df2 <- df}

    #filtering
    if(!is.null(sample_ids)) {
        df <- df %>% dplyr::filter(sample_id %in% sample_ids)
        df2 <- df2 %>% dplyr::filter(sample_id %in% sample_ids)
    }

    if(!is.null(gene_ids)) {
        df <- df %>% dplyr::filter(assayed_id %in% gene1)
    }

    if(!is.null(data_types)) {
        df <- df %>% dplyr::filter(data_type %in% data_type1)
    }

    if(!is.null(compound_ids) & 'compound_id' %in% colnames(df2)) {
        df2 <- df2 %>% dplyr::filter(compound_id %in% compound_ids)
    }

    if(!is.null(endpoints) & 'endpoint' %in% colnames(df2)) {
        df2 <- df2 %>% dplyr::filter(endpoint %in% endpoints)
    }

    if(!is.null(resp_ids) & 'resp_id' %in% colnames(df2)) {
        df2 <- df2 %>% dplyr::filter(resp_id %in% resp_ids)
    }

    #modifying   ###  THIS IS THE BIT TO DO ON THE TRAIN!!!
    df <- df %>%
        dplyr::transmute(sample_id, gene1 = assayed_id,
                         feature_type1 = data_type,
                         feature_name1 = paste(assayed_id, data_type, sep = "_"),
                         feature_value1 = value,
                         feature_original1 = original)

    df2 <- df2 %>%
        dplyr::transmute(sample_id, gene2 = assayed_id,
                         feature_type2 = data_type,
                         feature_name2 = paste(assayed_id, data_type, sep = "_"),
                         feature_value2 = value,
                         feature_original2 = original)


    df %>%
        dplyr::inner_join(df2, by = "sample_id")

}
