# https://vbaliga.github.io/verify-that-r-packages-are-installed-and-loaded/
## If a package is installed, it will be loaded. If any 
## are not, the missing package(s) will be installed 
## from CRAN and then loaded.

## First specify the packages of interest
packages = c("psych","readxl","DescTools","cluster","ggfortify","factoextra","gridExtra","DescTools", "stats", "effects")

## Now load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

# MSBA 7208 CA Graded HW Assignment
# CA Assignment: Alcoa wants to segment its 100 customers into smaller more meaningful groups in terms of share of wallet as a validation variable based on the following 0 to 10 interval scales converted to standardized z-scores:
#   * Zspeed
# * Zpricefle
# * Zmimage
# * Zsimage
# * Zquality
# * Zsatisfaction
# 
# Alcoa wants you to examine two clustering solutions: 3 and 4 clusters of customers, with a caveat of having at least 20 customers per cluster AND where all groups appear different from each other with respect to share of wallet (usage), the validation variable.
# 
# Run and interpret the following just for the cluster solution you have chosen: 
#   * ANOVA F values for IVs used to create the cluster solution chosen
# * Which two IVs have the greatest impact on maximizing similarity within cluster and dissimilarity between clusters
# * What are the mean differences and effect size for the validation variable via One-way ANOVA.
# * Do this in SPSS and R..and via R give a color visualization of the cluster solution you are advocating...and why.


# Make sure you set your working directory so that R knows where to look for your data!
print(getwd())
setwd("C:/Users/name/Documents/7208")

# Read data
dat.AlcoaData <- read_excel('data/alcoa.xls', sheet=1)
dat.AlcoaData <- as.data.frame(dat.AlcoaData)



dat.AlcoaData.selected <- dat.AlcoaData[,c('Zspeed','Zpricefle','Zmimage','Zquality', 'Zsatisfaction')]
summary(clusterMe)

# R Script:  Cluster Analysis Alcoa for 3 clusters
dat.AlcoaData.selected <- dat.AlcoaData[, c("Zspeed", "Zpricefle", "Zmimage" ,"Zsimage", "Zquality" ,"Zsatisfaction")]
findClusters <- clusGap(dat.AlcoaData.selected, FUN = kmeans, iter.max = 50, K.max = 4, B = 4)
findClusters
plot(findClusters)
set.seed(500)
dat.AlcoaData.cluster <- kmeans(dat.AlcoaData.selected, centers = 3, nstart = 10)
dat.AlcoaData.cluster
names(dat.AlcoaData.cluster)
dat.AlcoaData.cluster$centers
autoplot(dat.AlcoaData.cluster, data = dat.AlcoaData, frame = TRUE)

# R-script for One-way ANOVA check for usage as the validation variable:
dat.AlcoaData$ClusterMembership <- factor(dat.AlcoaData.cluster$cluster)
clusterMod <- lm(usage ~ ClusterMembership, dat.AlcoaData)
allEffects(clusterMod)
plot(allEffects(clusterMod))
anova(clusterMod)

# This problem is part of your "Mod Homework Assignment" grade, which constitutes 40% of the mod grade.  As such, this problem is worth 10% of your total grade. Will go over this problem during the Feb 4 SO-4 Zoom session. I will post a solution after the Zoom session and you will hand in your answer by Feb 8.  So, if you made mistakes you will have time to correct them for your handed-in grade.
