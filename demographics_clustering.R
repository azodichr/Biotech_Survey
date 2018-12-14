#### Biotechnology Survey Demographics

library(ggplot2)
library(reshape2)
library(XLConnect)

#########################
##### Clean up data #####
#########################

raw <- readWorksheetFromFile("ESP 804 Spring 804 Data with scales NEP_value_wts.xlsx", sheet=1)
d <- raw[]


# Replace scores 11 w/ 4 and 12 w/ 5 for likert-like scale in biotech questions
d[,50:74][d[,50:74] == 12] <- 5
d[,50:74][d[,50:74] == 11] <- 4


# Calculate Sci thinking score
key = c(3,1,3,1) # Correct answers
d$SciThinking <- rowMeans(score.multiple.choice(key=key, data=d[c('sr_accuracy','sr_puzzle','sr_correlation','sr_expdesign')], score=FALSE))
hist(d$SciThinking, breaks = 4, labels = TRUE, ylim=c(0,220))


# Calculate scales for each biotech question type (i.e. combine answers to all 5 products)
names(bt) <- gsub('_1', '_Familiar', names(bt))
names(bt) <- gsub('_2', '_Support', names(bt))
names(bt) <- gsub('_4', '_Beneficial', names(bt))
names(bt) <- gsub('_5', '_Risky', names(bt))
names(bt) <- gsub('_6', '_StrictReg', names(bt))

familiarity <- c('biopharm_Familiar','herbicide_Familiar', 'biofort_Familiar', 'hornless_Familiar', 'genedrive_Familiar')
benefit <- c('biopharm_Beneficial','herbicide_Beneficial', 'biofort_Beneficial', 'hornless_Beneficial', 'genedrive_Beneficial')
support <- c('biopharm_Support','herbicide_Support', 'biofort_Support', 'hornless_Support', 'genedrive_Support')
risk <- c('biopharm_Risky','herbicide_Risky', 'biofort_Risky', 'hornless_Risky', 'genedrive_Risky')
regulate <- c('biopharm_StrictReg','herbicide_StrictReg', 'biofort_StrictReg', 'hornless_StrictReg', 'genedrive_StrictReg')

scaleKey <- c(1,1,1,1,1)

familiarity.results <- scoreItems(keys = scaleKey, items = bt[familiarity], totals = FALSE, missing = FALSE, min=1, max = 5)
bt2$BT.fam <- familiarity.results$score
benefit.results <- scoreItems(keys = scaleKey, items = bt[benefit], totals = FALSE, missing = FALSE, min=1, max = 5)
bt2$BT.bene <- benefit.results$score
support.results <- scoreItems(keys = scaleKey, items = bt[support], totals = FALSE, missing = FALSE, min=1, max = 5)
bt2$BT.sup <- support.results$score
risk.results <- scoreItems(keys = scaleKey, items = bt[risk], totals = FALSE, missing = FALSE, min=1, max = 5)
bt2$BT.risk <- risk.results$score
regulate.results <- scoreItems(keys = scaleKey, items = bt[regulate], totals = FALSE, missing = FALSE, min=1, max = 5)
bt2$BT.reg <- regulate.results$score
d$BT.fam <- familiarity.results$score
d$BT.bene <- benefit.results$score
d$BT.sup <- support.results$score
d$BT.risk <- risk.results$score
d$BT.reg <- regulate.results$score



# Calculate scales for religiosity and science support
science <- c("sci_rel_1","sci_rel_2")
religion <- c("sci_rel_3","sci_rel_4","sci_rel_5")
politics <- c('dem_rep','lib_conserv','vote_HvD')
d$vote_HvD <- d$vote_2016
d$vote_HvD <- as.numeric(gsub(3, 'na', d$vote_HvD))
d$vote_HvD <- as.numeric(gsub(4, 'na', d$vote_HvD))

science.results <- scoreItems(keys = c(1,1), items = d[science], totals = FALSE, missing = FALSE, min=1, max = 5)
d$science <- science.results$score
religion.results <- scoreItems(keys = c(1,1,1), items = d[religion], totals = FALSE, missing = FALSE, min=1, max = 5)
d$religiosity <- religion.results$score

politics.results <- scoreItems(keys = c(1,1,1), items = d[politics], totals = FALSE, impute = "mean", missing = TRUE, min=1, max = 5)
d$politics <- politics.results$score


# Education Levle (edu_cat)
#1 - High school or less
#2 - Some college or technical
#3 - College degree +
d$edu_cat <- d$edu_level
hist(d$edu_cat, labels = TRUE)
d$edu_cat <- as.numeric(gsub(3, 1, d$edu_cat))
d$edu_cat <- as.numeric(gsub(4, 2, d$edu_cat))
d$edu_cat <- as.numeric(gsub(10, 2, d$edu_cat))
d$edu_cat <- as.numeric(gsub(6, 3, d$edu_cat))
d$edu_cat <- as.numeric(gsub(12, 3, d$edu_cat))

# Education Level + (edu_cat_STEM)
# 3 - College degree + in humanities
# 4 - College degree + in STEM (health sciences, physical/biological sciences, math, engineering)
d$edu_cat_STEM <- d$edu_cat
d$edu_cat_STEM[d$edu_cat == 3 & d$edu_major_2 == 1] <- as.numeric(gsub(3, 4, d$edu_cat_STEM[d$edu_cat == 3 & d$edu_major_2 == 1]))
d$edu_cat_STEM[d$edu_cat == 3 & d$edu_major_6 == 1] <- as.numeric(gsub(3, 4, d$edu_cat_STEM[d$edu_cat == 3 & d$edu_major_6 == 1]))

# Age (age_cat)
#1 - 18-40 "iGen and Millenials"
#2 - 41-52 "Gen X"
#3 - 53-71 "Baby Boomers"
#4 - 72 + "Silent Generation"
d$age_cat <- d$agey
hist(d$age_cat, labels = TRUE)
d$age_cat[d$age_cat <= 40] <- 1
d$age_cat[d$age_cat >= 41 & d$age_cat <= 52] <- 2
d$age_cat[d$age_cat >=53 & d$age_cat <= 71] <- 3
d$age_cat[d$age_cat >= 72] <- 4


# Renumber ag_familiar
d$ag_famil <- d$ag_familiar
d$ag_famil[d$ag_famil == 10] <- 0
d$ag_famil[d$ag_famil == 11] <- 1
d$ag_famil[d$ag_famil == 12] <- 2
d$ag_famil[d$ag_famil == 13] <- 3
d$ag_famil[d$ag_famil == 14] <- 4


# Did you live in a rural area on or not on a farm or ranch as a child, now, or ever (farm_c, farm_a, farm)
d$farm_c <- d$community_child
d$farm_c[d$farm_c <= 2] <- 1
d$farm_c[d$farm_c > 2] <- 0

d$farm_now <- d$community_current
d$farm_now[d$farm_now <= 2] <- 1
d$farm_now[d$farm_now > 2] <- 0

d$farm <- d$farm_c + d$farm_now
d$farm[d$farm ==2] <- 1

write.csv('')



######################################
##### Dig into Biotech Responses #####
######################################
# Place respondent IDs and biotech answers in seprate file:
bt <- read.csv("ESP_804_Spring_2017_Survey_biotech.csv", header=TRUE, row.names = 1)

names(bt) <- gsub('_1', '_Familiar', names(bt))
names(bt) <- gsub('_2', '_Support', names(bt))
names(bt) <- gsub('_4', '_Beneficial', names(bt))
names(bt) <- gsub('_5', '_Risky', names(bt))
names(bt) <- gsub('_6', '_Reg', names(bt))


# Correlation between responses about biotechnology
sup <- rowMeans(bt[,grepl('Support', names(bt))])
bene <- rowMeans(bt[,grepl('Benef', names(bt))])
risk <- rowMeans(bt[,grepl('Risky', names(bt))])
fam <- rowMeans(bt[,grepl('Fam', names(bt))])
reg <- rowMeans(bt[,grepl('Reg', names(bt))])
cor.test(sup, bene)
cor.test(sup, risk)
cor.test(sup, fam)
cor.test(sup, bene)
cor.test(sup, reg)

bt_tmp <- bt$ID <- row.names(bt)
d_opin <- melt(bt, id = 'ID')
d_opin$prod <- as.character(lapply(strsplit(as.character(d_opin$variable), split="_"),head, n=1))
d_opin$quest <- as.character(lapply(strsplit(as.character(d_opin$variable), split="_"),tail, n=1))
d_opin <- subset(d_opin, quest != 'Familiar')

reorder_tech <- function(x) { factor(x, levels = c(  "hornless","herbicide","biofort", "genedrive", "biopharm"))}

ggplot(d_opin, aes(x = reorder_tech(prod), y = value, group=quest, color=quest, fill=quest)) + 
  theme_bw(10) + stat_summary(geom="ribbon", fun.ymin = function(x) mean(x) - sd(x),
                              fun.ymax = function(x) mean(x) + sd(x), alpha=0.2, color=NA) +
  stat_summary(geom="line", fun.y=function(x) mean(x) , size=2)
#ggsave("181213_AveResp_ByProd.pdf", width=5, height = 5)

summary(lm(value~prod, subset(d_opin, quest=='Support')))
# Note: Significantly less support for hornless and more for biopharaceuticals


# Calculate coefficient of variation for each question for all 5 products (i.e. product nuance)
d_temp <- bt[, grepl('Sup', names(bt))]
d$BT.sup.n <- apply(d_temp, 1, sd) / apply(d_temp, 1, mean) * 100
d_temp <- bt[, grepl('Bene', names(bt))]
d$BT.bene.n <- apply(d_temp, 1, sd) / apply(d_temp, 1, mean) * 100
d_temp <- bt[, grepl('Risk', names(bt))]
d$BT.risk.n <- apply(d_temp, 1, sd) / apply(d_temp, 1, mean) * 100
d_temp <- bt[, grepl('Reg', names(bt))]
d$BT.reg.n <- apply(d_temp, 1, sd) / apply(d_temp, 1, mean) * 100

qplot(d$BT.sup.n, geom='histogram', binwidth=2)
#ggsave("181214_NuanceSupport_Hist.pdf", width=4, height = 4)


# Re-plot responses of just those in top 3rd quartile for nuance in support:
top25p <- row.names(d[d$BT.sup.n > quantile(d$BT.sup.n, 0.75), ] )
d_opin_top25p <- d_opin[(d_opin$ID %in% top25p), ]
ggplot(d_opin_top25p, aes(x = reorder_tech(prod), y = value, group=quest, color=quest, fill=quest)) + 
  theme_bw(10) + stat_summary(geom="ribbon", fun.ymin = function(x) mean(x) - sd(x),
                              fun.ymax = function(x) mean(x) + sd(x), alpha=0.2, color=NA) +
  stat_summary(geom="line", fun.y=function(x) mean(x) , size=2)
#ggsave("181214_AveResp_ByProd_Nuanced25.pdf", width=5, height = 5)




#######################################
##### Cluster by Biotech Opinions #####
#######################################


### Note: Dropping familiarity answers for clustering - this is an independent not dependent variable!
bt <- bt[,!(grepl('Fam', names(bt)))]

# Test number of clusters
library(clValid)

cl_test <- clValid(bt, 3:10, clMethods=c('kmeans'), validation='internal')
optimalScores(cl_test)

### Note: Optimal number of clusters selected based on silhouette width: 5

k <- 5
#Biot_clusters <- kmeans(bt, k) # Note - ran once, saved results. Re-running will require re-doing everything past this step!
d$Biotech_cluster_NF_k5 <- Biot_clusters$cluster
bt$cluster <- Biot_clusters$cluster
write.csv(d, '170503_data_BT_clusters.txt', sep='\t', quote=F)

plotcluster(bt, Biot_clusters$cluster,main="5 Clusters")
Biot_clusters$size


# Plot cluster means
m <- melt(bt, id.var='cluster')
m$question <- as.character(lapply(strsplit(as.character(m$variable), split="_"),tail, n=1))
m$biotech <- as.character(lapply(strsplit(as.character(m$variable), split="_"),head, n=1))

m_mean <- aggregate(value ~ biotech * question * cluster, m, FUN=mean)

#Plot means for each cluster
reorder_quest <- function(x) { factor(x, levels = c("Support", "Beneficial", "Risky", "Reg"))}
reorder_tech <- function(x) { factor(x, levels = c("biopharm", "genedrive", "biofort", "herbicide","hornless"))}

ggplot(m_mean, aes(reorder_quest(question), reorder_tech(biotech), fill=value)) +
  geom_tile() +
  theme_minimal(base_size = 20)+
  scale_fill_gradient2(low='#008837', mid='white', high='#7b3294', midpoint=3, limits=c(1,5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_grid(.~ cluster)
#ggsave("170406_BT_clusters.pdf", width=9, height = 5)

nua <- d[, c("Biotech_cluster_NF_k5", "BT.sup.n", "BT.bene.n", "BT.risk.n", "BT.reg.n")]
nuam <- melt(nua, ID='Biotech_cluster_NF_k5')
nuam_ag <- aggregate(value ~ Biotech_cluster_NF_k5 * variable, nuam, FUN = mean)
nuam_ag$y = 'nuance'
reo_quest_n <- function(x) { factor(x, levels = c("BT.sup.n", "BT.bene.n", "BT.risk.n", "BT.reg.n"))}

ggplot(nuam_ag, aes(reo_quest_n(variable), y, fill=value)) +
  geom_tile() +
  theme_minimal(base_size = 20)+
  scale_fill_gradient(low='white', high='darkred') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_grid(.~ Biotech_cluster_NF_k5)
ggsave("181214_BT_clusters_nuance.pdf", width=5, height = 3)




############################
##### Get Demographics #####
############################


n <- dim(d)[1]
med_age <- summary(d$agey)['Median']
per_male <- summary(as.factor(d$male))[2]/n
per_white <- summary(as.factor(d$white))[2]/n
per_asian <- summary(as.factor(d$asian))[2]/n
per_black <- summary(as.factor(d$black))[2]/n
per_hisp <- summary(as.factor(d$hispn))[2]/n
per_veg <- summary(as.factor(d$veg_diet))[2]/n
mean_income <- mean(d$income, na.rm=T)
mean_polit <- summary(d$politics)['Mean']

row <- c('all', n, med_age, per_male, per_white, per_asian, per_black,
  per_hisp, per_veg, mean_income, mean_polit)
stats <- NULL
stats <- rbind(stats, row)


for(clust in c('a', 'b', 'c', 'd', 'e')){
  dc <- subset(d, Biotech_cluster_NF_k5 == clust)
  n <- dim(dc)[1]
  med_age <- summary(dc$agey)['Median']
  per_male <- summary(as.factor(dc$male))[2]/n
  per_white <- summary(as.factor(dc$white))[2]/n
  per_asian <- summary(as.factor(dc$asian))[2]/n
  per_black <- summary(as.factor(dc$black))[2]/n
  per_hisp <- summary(as.factor(dc$hispn))[2]/n
  per_veg <- summary(as.factor(dc$veg_diet))[2]/n
  mean_income <- mean(dc$income, na.rm=T)
  mean_polit <- summary(dc$politics)['Mean']
  row <- c(clust, n, med_age, per_male, per_white, per_asian, per_black,
           per_hisp, per_veg, mean_income, mean_polit)
  stats <- rbind(stats, row)
}

colnames(stats) <- c('cluster', 'n', 'age.med', 'male.p', 'white.p', 'asian.p', 'black.p',
                  'hisp.p', 'vege.p', 'income.mean', 'politics.mean')
statst <- t(stats)




