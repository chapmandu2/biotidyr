library(dplyr)
library(tidyr)
library(tidyMultiAssay)
library(PharmacoGx)
library(purrr)
#library(plotly)

load('~/BigData/PSets/CCLE.RData')
#load('~/BigData/PSets/GDSC.RData')

#get ccle resp data
resp_data_ccle <- gather_response.PharmacoSet(CCLE, sample_ids = NULL,
                                              resp_ids = c('AZD6244', 'Lapatinib', 'PLX4720', 'Erlotinib'),
                                              resp_col = 'ic50_recomputed' ) %>%
    dplyr::mutate(value=ifelse(is.infinite(value) | value > 10000, 4, 6-log10(value)))  %>%
    tbl_df()

#function to generate genes data frame
split_genes <- function(pset, data_type, n_groups) {
    featureInfo(pset, data_type)  %>%
        tbl_df() %>%
        dplyr::filter(!is.na(Symbol)) %>%
        dplyr::mutate(i=sample(1:n_groups,n(), replace=TRUE)) %>%
        dplyr::group_by(i) %>%
        tidyr::nest(.key = 'genes_df')
}

#get ccle rna genes
genes_ccle_rna <- split_genes(CCLE, 'rna', 16)
genes_ccle_rna

#construct control data frame for ccle rna
control_ccle_rna_df <- genes_ccle_rna %>%
    dplyr::mutate(resp_df=purrr::map(i, ~resp_data_ccle),
                  pset_name='CCLE',
                  data_type='rna',
                  gene_col='Symbol')
control_ccle_rna_df

#get ccle mutation genes
genes_ccle_mut <- split_genes(CCLE, 'mutation', 16)
genes_ccle_mut

#construct control data frame for ccle mut
control_ccle_mut_df <- genes_ccle_mut %>%
    dplyr::mutate(resp_df=purrr::map(i, ~resp_data_ccle),
                  pset_name='CCLE',
                  data_type='mutation',
                  gene_col='Symbol')
control_ccle_mut_df

#get ccle rnaseq genes
genes_ccle_rnaseq <- split_genes(CCLE, 'rnaseq', 16)
genes_ccle_rnaseq

#construct control data frame for ccle rnaseq
control_ccle_rnaseq_df <- genes_ccle_rnaseq %>%
    dplyr::mutate(resp_df=purrr::map(i, ~resp_data_ccle),
                  pset_name='CCLE',
                  data_type='rnaseq',
                  gene_col='Symbol')
control_ccle_mut_df

#combine to a single control df
control_df <-  dplyr::bind_rows(control_ccle_mut_df, control_ccle_rna_df, control_ccle_mut_df) #%>% dplyr::filter(i<4)


#define the analysis function
do_analysis <- function(genes_df, resp_df, pset_name, data_type, gene_col) {
    #get the assay data from the pharmacoset
    assay_data <- gather.PharmacoSet(get(pset_name), sample_ids=NULL,
                                     gene_ids = genes_df %>% dplyr::select_(gene_col) %>% unlist() %>% unname(),
                                     data_types=data_type, gene_col=gene_col)

    rvg_data <- make_response_vs_genetic_df.data.frame(assay_data, resp_df)

    rvg_mod <- rvg_data %>%
        dplyr::filter(!is.na(feature_value) & !is.na(resp_value)) %>%
        group_by(assayed_id, feature_type, resp_id) %>%
        tidyr::nest()

    safe_glance <- purrr::possibly(broom::glance, otherwise=NULL)

    do_lm <- function(dat, data_type) {
        safe_lm <- purrr::possibly(lm, otherwise=NULL)
        if(data_type == 'mutation') {
            safe_lm(resp_value ~ as.factor(feature_value), data=dat)
        } else {
            safe_lm(resp_value ~ feature_value, data=dat)
        }
    }

    rvg_mod <- rvg_mod %>%
        dplyr::mutate(lm_mod=purrr::map2(data, feature_type, do_lm),
                      lm_glance=purrr::map(lm_mod, safe_glance))


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
fpar(1:5, control_df)

#process in parallel using foreach
library(foreach)
library(doParallel)
cl = makeCluster(8)
registerDoParallel(cl)

ct <- proc.time()
z <- foreach(j=1:nrow(control_df),
             .packages=c('tidyr', 'dplyr', 'purrr', 'broom', 'tidyMultiAssay', 'PharmacoGx'),
             .export=c('CCLE')) %dopar%
     fpar(j, control_df)
z <- bind_rows(z)
proc.time() - ct

stopCluster(cl)


