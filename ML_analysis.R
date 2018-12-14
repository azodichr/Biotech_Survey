#### Biotechnology Survey Analysis 
#
# File contains analysis of machine learning results
#
# Overview:
# 1. Single Feature Logistic Regression Models
# 2. Dig into Biotech Responses
# 3. Cluster by Biotech Opinions
# 4. Get Demographics
#
#####


library(ggplot2)
library(reshape2)

####################################
##### 1. Single Feature Models #####
####################################

sf <- read.csv('RESULTS_multiclass_SF.csv', header=T)
sf$ID <- gsub('SF_feat_', '', sf$ID)
sfm <- melt(sf[,c('ID','F1m')], id='ID', value.name='F1macro' )
sfmsd <- melt(sf[,c('ID', 'F1m_sd')], id='ID', value.name='F1macro.sd' )
sfm <- merge(sfm, sfmsd, by='ID')
sf_order <- reorder(sfm$ID, sfm$F1macro)
ggplot(sfm, aes(x=reorder(ID, F1macro), y=F1macro)) + theme_bw() +
  geom_bar(stat="identity") + geom_hline(yintercept=0.2, color='red') +
  geom_errorbar(aes(ymin=F1macro-F1macro.sd, ymax=F1macro+F1macro.sd), width=.2,
                                            position=position_dodge(.9)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave('181214_ML_SingleFeat_Barplots.pdf', width=5, height = 4)


# How well did each single feature do with each class
reorder_feat <- function(x) { factor(x, levels = sf_order[1:27])}

sfcl <- sf[,grepl('_f1', names(sf))]
sfcl <- sfcl[,!(grepl('sd', names(sfcl)))]
sfcl$ID <- sf$ID
sfclm <- melt(sfcl, id = 'ID')
ggplot(sfclm, aes(x=variable, y=reorder(ID, value, mean), fill=value)) + geom_tile() +
  scale_fill_gradient2(low='blue4', mid='white', high = "firebrick3", midpoint=0.2) + theme_bw()
ggsave('181214_ML_SingleFeat_Group_Heatmap.pdf', width=5, height = 8)

#################################
##### 2. All Feature Models #####
#################################

mf <- read.csv('RESULTS_multiclass.csv', header=T)
mf <- subset(mf, mf$CVf == '5')
mfm <- melt(mf[,c('Alg','F1m')], id='Alg', value.name='F1macro' )
mfmsd <- melt(mf[,c('Alg', 'F1m_sd')], id='Alg', value.name='F1macro.sd' )
mfm <- merge(mfm, mfmsd, by='Alg')

ggplot(mfm, aes(x=reorder(Alg, F1macro), y=F1macro)) + theme_bw() +
  geom_bar(stat="identity") + geom_hline(yintercept=0.2, color='red') +
  geom_errorbar(aes(ymin=F1macro-F1macro.sd, ymax=F1macro+F1macro.sd), width=.2,
                position=position_dodge(.9)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave('181214_ML_MultiFeat_Barplots.pdf', width=3, height = 4)


# Confusion matrix for best algorithm (i.e. RF)
cm <- read.csv('all_feat_RF_cv5_ConfMat.txt', sep='\t')
cmm <- melt(cm, id = 'X')
ggplot(cmm, aes(x=variable, y=X, fill=value)) + geom_tile() +
  geom_text(aes(label=round(value,2))) + labs(x='Predicted', y='True') +
  scale_fill_gradient(low='white', high = "firebrick3", limits=c(0,0.53)) + theme_bw()
ggsave('181214_ML_RF_CM.pdf', width=4, height = 4)


###############################
##### 2. Feature Patterns #####
###############################

d <- read.csv('170503_data_BT_clusters.txt', sep='\t')
feats_used <- scan('feat_ALL', what='character')
for(f in feats_used){
  ggplot(d, aes(x=Biotech_cluster_NF_k5, y= get(f), fill=Biotech_cluster_NF_k5)) +
    geom_violin() + theme_bw() +labs(x='', y = f)
  ggsave(paste('figures_feature_violin/181214_feat_', f, '_dist.pdf', sep=''), width=4, height = 3)
}

feats_binary <- c('white', 'trump', 'veg_diet', 'male', 'hispn', 'farm', 'black', 'asian')

f <- 'white'
d[,f] <- as.numeric(d[,f])
num <- aggregate(get(f) ~ Biotech_cluster_NF_k5, d, FUN=NROW)
sum <- aggregate(get(f) ~ Biotech_cluster_NF_k5, d, FUN=sum)
num$per_1 <- sum[,'get(f)']/num[,'get(f)']
num$per_0 <- 1-num$per_1
m <- melt(num[,c('Biotech_cluster_NF_k5', 'per_1', 'per_2')], id = 'Biotech_cluster_NF_k5')
ggplot(num, aes(x=Biotech_cluster_NF_k5, y= get(f), fill=Biotech_cluster_NF_k5)) +
  geom_violin() + theme_bw() +labs(x='', y = f)
ggsave(paste('figures_feature_dist/181214_feat_', f, '_dist.pdf', sep=''), width=4, height = 3)


