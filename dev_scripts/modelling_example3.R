library(dplyr)
library(tidyr)
library(tidyMultiAssay)
library(PharmacoGx)
library(purrr)
#library(plotly)

load('~/BigData/PSets/CCLE.RData')
load('~/BigData/PSets/GDSC.RData')

load('~/Documents/2015 Projects/20150521 PARG Eurofins Data Analysis/for_parg_explorer/PARG_screening_data.RData')

#get PARGi resp data
resp_data_parg <- parg_data %>%
    dplyr::rename(sample_id=unified_id) %>%
    gather_response.data.frame(sample_ids = NULL, compound_ids = NULL, endpoints = c('pGI50', 'GR50'), resp_ids=NULL)


#convert between sample ids
#get the PSet ids and crush
pharmacogx_ids <- data_frame(pgx_id=unique(c(cellNames(CCLE), cellNames(GDSC)))) %>%
    dplyr::mutate(crush_id=crush(pgx_id, upper_case=TRUE))

#get the screening ids and crush, also check we don't have any duplicate ids post-crushig
screening_ids <- resp_data_parg %>%
    dplyr::select(sample_id, assayed_id) %>%
    dplyr::distinct() %>%
    dplyr::mutate(crush_id=gsub("_(.*)","",sample_id)) %>%
    dplyr::group_by(crush_id, assayed_id) %>%
    dplyr::mutate(N=n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(N==1)

#make the conversion data frame
convert_ids_df <- screening_ids %>%
    dplyr::distinct(sample_id, crush_id) %>%
    dplyr::inner_join(pharmacogx_ids, by='crush_id')

#convert using convert_ids functino
dat1 <- convert_ids(resp_data_parg, convert_ids_df, from_col='sample_id', to_col='pgx_id')


#function to generate genes data frame
split_genes <- function(pset, data_type, n_groups, gene_col='Symbol') {
    featureInfo(pset, data_type)  %>%
        tbl_df() %>%
        dplyr::filter_(!is.na(gene_col)) %>%
        dplyr::mutate(i=sample(1:n_groups,n(), replace=TRUE)) %>%
        dplyr::group_by(i) %>%
        tidyr::nest(.key = 'genes_df')
}

#function to generate control data frame
make_control_df <- function(pset, data_type, n_groups, gene_col, resp_df) {
    split_genes(get(pset), data_type, n_groups, gene_col) %>%
        dplyr::mutate(resp_df=purrr::map(i, ~resp_df),
                      pset_name=pset,
                      data_type=data_type,
                      gene_col=gene_col)
}

control_df <- dplyr::bind_rows(
    make_control_df('CCLE', 'rna', 16, 'Symbol', dat1),
    make_control_df('CCLE', 'mutation', 16, 'Symbol', dat1),
    #make_control_df('CCLE', 'rnaseq', 16, 'Symbol', dat1),
    make_control_df('GDSC', 'rna', 16, 'Symbol', dat1),
    make_control_df('GDSC', 'mutation', 1, 'gene_name', dat1)
    #make_control_df('GDSC', 'rna2', 16, 'Symbol', dat1)
    )


#define the analysis function
do_analysis <- function(genes_df, resp_df, pset_name, data_type, gene_col) {
    #get the assay data from the pharmacoset
    assay_data <- gather.PharmacoSet(get(pset_name), sample_ids=NULL,
                                     gene_ids = genes_df %>% dplyr::select_(gene_col) %>% unlist() %>% unname(),
                                     data_types=data_type, gene_col=gene_col)

    #make the response vs genetic data frame
    rvg_data <- make_response_vs_genetic_df.data.frame(assay_data, resp_df)

    #nest the data
    rvg_mod <- rvg_data %>%
        dplyr::filter(!is.na(feature_value) & !is.na(resp_value)) %>%
        group_by(assayed_id, feature_type, resp_id) %>%
        tidyr::nest()

    #make a safe glance function that will fail gracefully
    safe_glance <- purrr::possibly(broom::glance, otherwise=NULL)

    #internal do lm function to differentiate between mutation and continuous data
    do_lm <- function(dat, data_type) {
        safe_lm <- purrr::possibly(lm, otherwise=NULL)
        if(data_type == 'mutation') {
            safe_lm(resp_value ~ as.factor(feature_value), data=dat)
        } else {
            safe_lm(resp_value ~ feature_value, data=dat)
        }
    }

    #map the lm functions to the data
    rvg_mod <- rvg_mod %>%
        dplyr::mutate(lm_mod=purrr::map2(data, feature_type, do_lm),
                      lm_glance=purrr::map(lm_mod, safe_glance))

    #get rid of the models and return the summary values - this keeps the data size reasonable
    rvg_mod %>%
        dplyr::filter(!purrr::map_lgl(lm_glance, is.null)) %>%
        dplyr::select(-data, -lm_mod) %>%
        tidyr::unnest() %>%
        dplyr::arrange(p.value)


}


#define a function to process the control data frame
fpar <- function(k, df) {
    df %>%
        dplyr::select(-i) %>%
        dplyr::slice(k) %>%
        purrr::invoke_rows(.f=do_analysis, .d=., .labels=TRUE) %>%
        dplyr::select(-genes_df, -resp_df) %>%
        tidyr::unnest()
}
fpar(6, control_df)
fpar(1:2, control_df)

#process in parallel using foreach
library(foreach)
library(doParallel)
cl = makeCluster(6)
registerDoParallel(cl)

ct <- proc.time()
z <- foreach(j=1:nrow(control_df),
             .packages=c('tidyr', 'dplyr', 'purrr', 'broom', 'tidyMultiAssay', 'PharmacoGx'),
             .export=c('CCLE', 'GDSC')) %dopar%
    fpar(j, control_df)
z <- bind_rows(z)
proc.time() - ct

stopCluster(cl)


