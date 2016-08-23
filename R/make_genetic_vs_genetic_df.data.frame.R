#' @title
#' Create a genetic_vs_genetic data frame from two tall data frames
#'
#' @description
#' Create a genetic_vs_genetic data frame from two tall data frames
#'
#' @param df data.frame object
#' @param df2 Second data.frame object.  Default is NULL (just use the first data.frame)
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
#' dat1 <- gather_assay.PharmacoSet(CCLEsmall, sample_ids=PharmacoGx::cellNames(CCLEsmall),
#'   gene_ids = c('RBM5', 'LAP3', 'CFTR'), data_type = 'rna')
#' dat2 <- gather_assay.PharmacoSet(CCLEsmall, sample_ids=PharmacoGx::cellNames(CCLEsmall),
#'   gene_ids = c('RBM5', 'LAP3', 'CFTR'), data_type = 'rnaseq', gene_col='gene_name')
#'
#' gvg_df <- make_genetic_vs_genetic_df.data.frame(df=dat1, df2=dat2, sample_ids=PharmacoGx::cellNames(CCLEsmall), gene1='RBM5', gene2='RBM5',
#'  data_type1='rna', data_type2='rnaseq')
#' gvg_df
make_genetic_vs_genetic_df.data.frame <- function(df, df2=NULL, sample_ids=NULL, gene1=NULL, gene2=NULL, data_type1=NULL, data_type2=NULL) {

    if(is.null(df2)) {df2 <- df}

    check_df_format(df, 'tall_df', dev_mode=TRUE)
    check_df_format(df2, 'tall_df', dev_mode=TRUE)

    #filtering
    if(!is.null(sample_ids)) {
        df <- df %>% dplyr::filter(sample_id %in% sample_ids)
        df2 <- df2 %>% dplyr::filter(sample_id %in% sample_ids)
    }

    if(!is.null(gene1)) {
        df <- df %>% dplyr::filter(assayed_id %in% gene1)
    }

    if(!is.null(gene2)) {
        df2 <- df2 %>% dplyr::filter(assayed_id %in% gene2)
    }

    if(!is.null(data_type1)) {
        df <- df %>% dplyr::filter(data_type %in% data_type1)
    }

    if(!is.null(data_type2)) {
        df2 <- df2 %>% dplyr::filter(data_type %in% data_type2)
    }

    #modifying
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
