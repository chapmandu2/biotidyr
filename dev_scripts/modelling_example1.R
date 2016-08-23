library(dplyr)
library(tidyMultiAssay)
library(PharmacoGx)

load('~/BigData/PSets/CCLE.RData')

set.seed(1234)
test_sample_ids <- sample(cellNames(CCLE), 500)
test_gene_ids <-     sample(featureNames(CCLE, 'rna'), 500)
test_drug_ids <- sample(drugNames(CCLE), 5)

resp_data <- gather_response.PharmacoSet(CCLE, test_sample_ids, test_drug_ids)
assay_data <- gather_assay.PharmacoSet(CCLE, test_sample_ids, test_gene_ids, 'rna', gene_col='Probe')

gvg1 <- make_genetic_vs_genetic_df.data.frame(assay_data,
                                              gene1=sample(test_gene_ids, 5),
                                              gene2=sample(test_gene_ids, 1))

rvg1 <- make_response_vs_genetic_df.data.frame(assay_data, resp_data,
                                           gene_ids = sample(test_gene_ids, 1),
                                           sample_ids = '1321N1')

#try to do something useful - ie rnaseq and rna data for egfr, erbb2 plus erlotinib, lapatanib
resp_data2 <- gather_response.PharmacoSet(CCLE, sample_ids = NULL, resp_ids = c('lapatinib', 'Erlotinib'))
assay_data2 <- gather_assay.PharmacoSet(CCLE, sample_ids=NULL, gene_ids = c('BRAF', 'ERBB2', 'EGFR'), 'rna', gene_col='Symbol')
assay_data3 <- gather.PharmacoSet(CCLE, sample_ids=NULL, gene_ids = c('BRAF', 'ERBB2', 'EGFR'), data_types=c('rna', 'rnaseq'), gene_col=c('Symbol', 'Symbol'))

#combo data frames
gvg2 <- make_genetic_vs_genetic_df.data.frame(assay_data2)
gvg3 <- make_genetic_vs_genetic_df.data.frame(assay_data3)

rvg2 <- make_response_vs_genetic_df.data.frame(assay_data2, resp_data2)
rvg3 <- make_response_vs_genetic_df.data.frame(assay_data3, resp_data2)


#some plots
library(ggplot2)
ggplot(gvg2, aes(feature_value1, feature_value2)) +
    geom_point() +
    facet_grid(feature_name1~feature_name2)
ggplot(gvg3, aes(feature_value1, feature_value2)) +
    geom_point() +
    facet_grid(feature_name1~feature_name2)

ggplot(rvg2, aes(feature_value, resp_value)) +
    geom_point() + geom_smooth(method='lm') +
    facet_grid(feature_name~resp_id)
ggplot(rvg3, aes(feature_value, resp_value)) +
    geom_point() + geom_smooth(method='lm') +
    facet_grid(feature_name~resp_id)

#some simple modelling
#get rid of repition in data frame and make rna always feature 1
gvg3_mods <- gvg3 %>%
    dplyr::filter(feature_name1 != feature_name2, feature_type1=='rna', feature_type2=='rnaseq')
ggplot(gvg3_mods, aes(feature_value1, feature_value2)) + geom_point() + facet_grid(feature_name1~feature_name2)
#nest the data
gvg3_mods <- gvg3_mods %>%
    dplyr::group_by(feature_name1, feature_name2) %>%
    tidyr::nest()
gvg3_mods
#apply and extract a linear model - see hadley wickham managing many models r4ds
gvg3_mods <- gvg3_mods %>%
    dplyr::mutate(mod_lm=purrr::map(data, ~ lm(feature_value1 ~ feature_value2, data=.)),
                  mod_glance=purrr::map(mod_lm, broom::glance))
#extract the summary values
gvg3_mods %>% dplyr::select(-data, -mod_lm) %>% tidyr::unnest() %>% dplyr::arrange(desc(r.squared))

#something more useful - look for expression features correlated with response
rvg3_mod <- rvg3 %>%
    dplyr::group_by(feature_name, resp_id) %>%
    tidyr::nest() %>%
    dplyr::mutate(lm_mod=purrr::map(data, ~ lm(resp_value ~ feature_value, data=.)),
                  lm_glance=purrr::map(lm_mod, broom::glance))
rvg3_mod
rvg3_mod %>% dplyr::select(-data, -lm_mod) %>% tidyr::unnest() %>% dplyr::arrange(p.value)

#note that the code would be the same regardless of whether we were looking at 10 genes and 10 compounds or 1000 genes and 1000 copounds
#can also try different modelling paradigms easily
#try cox regression to handle censoring:
library(survival)
rvg3_mod <- rvg3_mod %>%
    dplyr::mutate(cox_mod = purrr::map(data, function(x) coxph(Surv(resp_value, resp_value < 7.9)~feature_value, data=x)),
                  cox_glance = purrr::map(cox_mod, broom::glance))
rvg3_mod
rvg3_mod %>% dplyr::select(-data, -lm_mod, -lm_glance, -cox_mod) %>%
    tidyr::unnest() %>% dplyr::arrange(p.value.log) %>% dplyr::select(feature_name, resp_id, n, nevent, p.value.log, concordance)



