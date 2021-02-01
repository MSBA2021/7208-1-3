library(corrplot)
library(psych)
library(ellipse)
library(caret)
library(readxl)
library(DescTools)
library(effects)
library(cluster)
library(ggfortify)

# Alcoa wants you to examine two clustering solutions: 3 and 4 clusters of customers, with a caveat of having at least 20 customers per cluster AND where all groups appear different from each other with respect to share of wallet (usage), the validation variable.
# 
# Run and interpret the following just for the cluster solution you have chosen: 
#   * ANOVA F values for IVs used to create the cluster solution chosen
# * Which two IVs have the greatest impact on maximizing similarity within cluster and dissimilarity between clusters
# * What are the mean differences and effect size for the validation variable via One-way ANOVA.
# * Do this in SPSS and R..and via R give a color visualization of the cluster solution you are advocating...and why.
setwd("~/MSBA/MOD3/7208 Customer Analytics/code/7208_Homework")

# Read data

alcoa <- read_excel('data/alcoa.xls', sheet=1)
alcoa <- as.data.frame(alcoa)

#Summary
# summary(alcoa)

clusterMe <- alcoa[,c('Zspeed','Zpricefle','Zmimage','Zquality', 'Zsatisfaction')]
summary(clusterMe)

findClusters <- clusGap(clusterMe, FUN = kmeans, iter.max = 50, K.max = 4, B = 4)
findClusters
plot(findClusters)
set.seed(500)
alcoaCluster <- kmeans(clusterMe, centers = 3, nstart = 10)
alcoaCluster
names(alcoaCluster)
alcoaCluster$centers
autoplot(alcoaCluster, data = alcoa, frame = TRUE)

# R-script for One-way ANOVA check for usage as the validation variable:
alcoa$ClusterMembership <- factor(alcoaCluster$cluster)
clusterMod <- lm(usage ~ ClusterMembership, alcoa)
allEffects(clusterMod)
plot(allEffects(clusterMod))
anova(clusterMod)
