#using a range of different inputs

#how to download a PSet:
library(PharmacoGx)
library(tidyMultiAssay)
library(dplyr)

data('CancerCellLineIDs')
load('~/BigData/PSets/CCLE.RData')
load('~/BigData/PSets/GDSC.RData')

#get rna and mutation data  from CCLE
dat1 <- gather.PharmacoSet(CCLE, sample_ids=NULL, gene_ids=c('BRAF', 'EGFR'),
                           data_types=c('rna', 'mutation'), gene_col=c('Symbol', 'Symbol'))

#convert to unified ids
dat1 <- convert_ids(df=dat1, id_data=dplyr::filter(CancerCellLineIDs, id_type=='CCLE'),
            from_col='alt_id', to_col='unified_id')

#get rna and mutation data from GDSC
dat2 <- gather.PharmacoSet(GDSC, sample_ids=NULL, gene_ids=c('BRAF', 'EGFR'),
                           sample_col = 'cellid',
                           data_types=c('rna', 'mutation'), gene_col=c('Symbol', 'gene_name'))

#convert to unified id's
dat2 <- convert_ids(df=dat2, id_data=dplyr::filter(CancerCellLineIDs, id_type=='gdsc'),
                    from_col='native_id', to_col='unified_id')

#modify data type so we can seperate on origin
dat1 <- dat1 %>% mutate(data_type = paste('ccle', data_type, sep='_'))
dat2 <- dat2 %>% mutate(data_type = paste('gdsc', data_type, sep='_'))

#merge using make_genetic_vs_genetic
combo <- make_genetic_vs_genetic_df.data.frame(dat1,dat2)
#plot
library(ggplot2)
ggplot(combo, aes(feature_value1, feature_value2)) + geom_point() + facet_grid(feature_name1~feature_name2)

#try dietelin data vs ccle
#get response data
data("dietlein_data")

#need to get genetic data as a seperate step as ids will be different
gdat1 <- gather.PharmacoSet(CCLE, gene_ids = c('BRAF', 'EGFR', 'TP53'), gene_col = c('Symbol', 'Symbol'))
gdat1 <- gdat1 %>% convert_ids(id_data=dplyr::filter(CancerCellLineIDs, id_type=='CCLE'), from_col='alt_id', to_col='unified_id')

#now create response_vs_genetic
dietlein_rvg <- make_response_vs_genetic_df.data.frame(gdat1, dietlein_data)

