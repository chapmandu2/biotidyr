#' @title
#' Converts sample id's from one format to another using a data frame with alternate information.
#'
#' @description
#' Converts sample id's from one format to another using a data frame with alternate information.
#'
#' @param df A data frame with a column named sample_id
#' @param id_data A data frame containing information for id conversion
#' @param from_col Define the name of the column matching the old
#' @param to_col Define the name of the column matching the new ids
#' @param other_cols Other columns to include in the output
#' @return a data frame
#' @export convert_ids
#'
#' @examples
#' data('CCLEsmall', package='PharmacoGx')
#'
#' dat1 <- gather.PharmacoSet(CCLEsmall, sample_ids=c('143B', '23132-87'), gene_ids=c('BRAF', 'EGFR'),
#'  data_types=c('rna', 'mutation'), gene_col=c('Symbol', 'Symbol'))
#'
#' data('CancerCellLineIDs')
#' convert_ids(df=dat1, id_data=dplyr::filter(CancerCellLineIDs, id_type=='CCLE'),
#' from_col='alt_id', to_col='unified_id')
#' convert_ids(df=dat1, id_data=dplyr::filter(CancerCellLineIDs, id_type=='CCLE'),
#' from_col='alt_id', to_col='unified_id', other_cols='tissue')
#'
convert_ids <- function(df, id_data=NULL, from_col='native_id', to_col='unified_id', other_cols=NULL) {

    if(is.null(id_data)) {
        data("CancerCellLineIDs")
        id_data <- dplyr::filter(CancerCellLineIDs, id_type=='CCLE')
    }

    id_data <- id_data %>% dplyr::select(dplyr::one_of(from_col, to_col, other_cols))

    df %>%
        dplyr::inner_join(id_data, by=c('sample_id'=from_col)) %>%
        dplyr::mutate_('sample_id'=to_col) %>%
        dplyr::select(-dplyr::one_of(to_col))

}
