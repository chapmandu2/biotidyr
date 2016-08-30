#' @title
#' Create a response_vs_genetic data frame from two tall data frames
#'
#' @description
#' Create a response_vs_genetic data frame from two tall data frames
#'
#' @param df a data frame in tall_df format containing the genetic data (and response data if df2 not set)
#' @param df2 a data frame in either tall_df or resp_df format containing the response data.
#'  Default is NULL (just use df)
#' @param sample_ids A vector of sample ids.  Default is NULL (don't filter on sample id)
#' @param gene_ids Gene ids in df.  Default is NULL (don't filter on gene id)
#' @param data_types Data type for genetic data.  Default is NULL (don't filter on data type)
#' @param compound_ids A vector of compound ids.  Default is NULL (don't filter on compound id)
#' @param endpoints A vector of endpoints.  Default is NULL (don't filter on endpoint)
#' @param resp_ids A vector of response ids.  Default is NULL (don't filter on response id)
#'
#' @return a data frame in rvg_df format
#' @export make_response_vs_genetic_df.data.frame
#'
#' @examples
#' data('CCLEsmall', package='PharmacoGx')
#'
#' genetic_data <- gather_assay.PharmacoSet(CCLEsmall, sample_ids=PharmacoGx::cellNames(CCLEsmall),
#'   gene_ids = c('RBM5', 'LAP3', 'CFTR'), data_type = 'rna')
#' resp_data <- gather_response.PharmacoSet(CCLEsmall, sample_ids=PharmacoGx::cellNames(CCLEsmall),
#'                          resp_ids=c('AEW541', 'Nilotinib'), resp_col='ic50_published')
#'
#' rvg_df <- make_response_vs_genetic_df.data.frame(df=genetic_data, df2=resp_data, sample_ids=PharmacoGx::cellNames(CCLEsmall), gene_ids='RBM5',
#'  data_types='rna', resp_ids='Nilotinib')
#' rvg_df
make_response_vs_genetic_df.data.frame <- function(df, df2=NULL, sample_ids=NULL, gene_ids=NULL, data_types=NULL, compound_ids=NULL, endpoints=NULL, resp_ids=NULL) {

    if(is.null(df2)) {df2 <- df}

    check_df_format(df, 'tall_df', dev_mode = TRUE)


    #filtering
    if(!is.null(sample_ids)) {
        df <- df %>% dplyr::filter(sample_id %in% sample_ids)
        df2 <- df2 %>% dplyr::filter(sample_id %in% sample_ids)
    }

    if(!is.null(gene_ids)) {
        df <- df %>% dplyr::filter(assayed_id %in% gene_ids)
    }

    if(!is.null(data_types)) {
        df <- df %>% dplyr::filter(data_type %in% data_types)
    }

    if(check_df_format(df2, 'resp_df')) {
        df2 <- gather_response.data.frame(df2, compound_ids = compound_ids, endpoints = endpoints)
    }


    if(!is.null(resp_ids) & check_df_format(df2, 'tall_df')) {
        df2 <- df2 %>% dplyr::filter(assayed_id %in% resp_ids)
    }

    #modify the data frame
    df <- df %>%
        dplyr::transmute(sample_id,assayed_id,
                         feature_type = data_type,
                         feature_name = paste(assayed_id, data_type, sep = "_"),
                         feature_value = value,
                         feature_original = original)

    df2 <- df2 %>%
        dplyr::transmute(sample_id, resp_id = assayed_id,
                         resp_value = value,
                         resp_original = original)


    #join the data frames by sample_id
    df %>%
        dplyr::inner_join(df2, by = "sample_id")

}
