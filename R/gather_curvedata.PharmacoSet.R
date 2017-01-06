#' @title
#' Convert raw dose response curve data from a \linkS4class{PharmacoSet} into tidy format
#'
#' @description
#' Convert raw dose response curve data from a \linkS4class{PharmacoSet} into tidy format
#'
#' @param x PharmacoSet object
#' @param sample_ids A vector of sample ids.  Default is NULL (don't filter on sample id)
#' @param resp_ids A vector of response ids.  Default is NULL (don't filter on response id)
#' @param extra_cols Optional additional columns to retrieve from the PharmacoSet drug info. Default is NULL
#'
#' @return a data frame
#' @export gather_curvedata.PharmacoSet
#'
#'
#' @examples
#' data('CCLEsmall', package='PharmacoGx')
#' gather_curvedata.PharmacoSet(CCLEsmall, sample_ids=c('CHL-1', 'SW1573'),
#'                          resp_ids=c('AEW541', 'Nilotinib'))
#' gather_curvedata.PharmacoSet(CCLEsmall, sample_ids=c('CHL-1', 'SW1573'),
#'                          resp_ids=c('AEW541', 'Nilotinib'), extra_cols='nbr.conc.tested')
#' \dontrun{
#'
#' #example to extract PARP inhibitor data from GDSC1000 dataset and what happens
#' #when more than one curve exists per compound/cell line combination
#'
#' library(dplyr)
#' GDSC1000 <- PharmacoGx::downloadPSet('GDSC1000')
#'
#' #get compounds that are PARP inhibitors
#' parp_inhibitors <- drugInfo(GDSC1000) %>%
#'         dplyr::filter(grepl('PARP', TARGET))
#'
#' #get data for parp inhibitors
#' parpi_data <- gather_curvedata.PharmacoSet(GDSC1000, sample_ids=NULL,
#'                          resp_ids=parp_inhibitors$drugid)
#'
#' #note warning about duplicate curves - this is because Olaparib was screened
#' #twice and has two drug ids - 1017 and 1495.  See parp_inhibtors dataframe
#'
#' }
#'
gather_curvedata.PharmacoSet <- function(x, sample_ids=NULL, resp_ids=NULL, extra_cols=NULL) {

    #include all samples and response values if either is not specified
    if(is.null(sample_ids)) {
        sample_ids <- PharmacoGx::cellNames(x)
    }

    if(is.null(resp_ids)) {
        resp_ids <- PharmacoGx::drugNames(x)
    }

    #get information on the drugs selected
    drugInfo <- x@sensitivity$info %>%
        tibble::rownames_to_column('curve_id') %>%
        dplyr::select(dplyr::one_of('curve_id', 'cellid', 'drugid', extra_cols)) %>%
        dplyr::filter(cellid %in% sample_ids & drugid %in% resp_ids) %>%
        dplyr::tbl_df()

    #get the curve names
    curveNames <- drugInfo$curve_id

    #subset the curve data for only those curves that contain the drugs and cellids that we want
    curveData <- x@sensitivity$raw
    curveData <- curveData[rownames(curveData) %in% curveNames,,]

    #convert the dose information data to a tibble
    doseData_df <- curveData[,,'Dose'] %>%
        as.data.frame() %>%
        tibble::rownames_to_column('curve_id') %>%
        tidyr::gather(key='doseid', value='conc', -curve_id) %>%
        dplyr::mutate(conc=as.numeric(conc)) %>%
        dplyr::tbl_df()

    #convert the viability information data to a tibble
    viabilityData_df <- curveData[,,'Viability'] %>%
        as.data.frame() %>%
        tibble::rownames_to_column('curve_id') %>%
        tidyr::gather(key='doseid', value='signal', -curve_id) %>%
        dplyr::mutate(signal=as.numeric(signal)) %>%
        dplyr::tbl_df()

    #combine the dose and viability information
    curveData_df <- viabilityData_df %>%
        dplyr::inner_join(doseData_df, by=c('curve_id', 'doseid')) %>%
        dplyr::select(-doseid) %>%
        dplyr::inner_join(drugInfo, by='curve_id')

    #combine the drug information and return
    out <- curveData_df %>%
        dplyr::mutate(sample_id=cellid, resp_id=drugid) %>%
        dplyr::select(dplyr::one_of('curve_id', 'sample_id', 'resp_id', 'conc', 'signal', extra_cols))

    #check whether any duplicates exist and warn if so
    dup_check <- out %>%
        dplyr::distinct(resp_id, sample_id, curve_id) %>%
        dplyr::group_by(resp_id, sample_id) %>%
        dplyr::summarise(N=n()) %>%
        dplyr::filter(N>1)

    if(nrow(dup_check) > 0) {
        warning("Duplicate curves exist for some cell lines or compounds, check before continuing!")
    }

    #return the data
    return(out)


}
