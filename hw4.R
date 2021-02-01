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

# HW Instructions
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

# Returns a matrix with K.max rows and 4 columns, named "logW", "E.logW", "gap", and "SE.sim", 
# gap = E.logW - logW
# SE.sim corresponds to the standard error of gap
# SE.sim[k]=s[k], where s[k] := sqrt(1 + 1/B) sd^*(gap[])
# sd^*() is the standard deviation of the simulated ("bootstrapped") gap values.
findClusters <- clusGap(clusterMe, FUN = kmeans, iter.max = 50, K.max = 4, B = 4)
findClusters
plot(findClusters)



# K3
#-----------------------------------
k3 <- kmeans(clusterMe, centers = 3, nstart = 10)
p1 <- fviz_cluster(k3, geom = "point", data = clusterMe) + ggtitle("k = 2")
p1
k3$size
k3$centers
#anova
alcoa$ClusterMembership <- factor(k3$cluster)
clusterMod <- lm(usage ~ ClusterMembership, alcoa)
allEffects(clusterMod)
plot(allEffects(clusterMod))
anova(clusterMod)

# K4
#-----------------------------------
k4 <- kmeans(clusterMe, centers = 4, nstart = 10)
p3 <- fviz_cluster(k4, geom = "point",  data = clusterMe) + ggtitle("k = 3")
p3
k4$size
k4$centers
#anova
alcoa$ClusterMembership <- factor(k4$cluster)
clusterMod <- lm(usage ~ ClusterMembership, alcoa)
allEffects(clusterMod)
plot(allEffects(clusterMod))
anova(clusterMod)

# Plot both
grid.arrange(p1, p3, ncol = 1,top="K3 vs K4 Clusters")


# cluster: A vector of integers (from 1:k) indicating the cluster to which each point is allocated.
# centers: A matrix of cluster centers.
# totss: The total sum of squares.
# withinss: Vector of within-cluster sum of squares, one component per cluster.
# tot.withinss: Total within-cluster sum of squares, i.e. sum(withinss).
# betweenss: The between-cluster sum of squares, i.e. $totss-tot.withinss$.
# size: The number of points in each cluster.


# Plot change in WSS by cluster
WSSHolder <- data.frame()
#run kmeans for all clusters up to 100
for(i in 1:6){
  #Run kmeans for each level of i, allowing up to 100 iterations for convergence
  kmeans<- kmeans(x=clusterMe, centers=i, iter.max=100)
  
  #Combine cluster number and cost together, write to df
  WSSHolder<- rbind(WSSHolder, cbind(i, kmeans$tot.withinss))
}
names(WSSHolder) <- c("Clusters", "WithinClusterSS")
ggplot(data=WSSHolder, aes(x=Clusters, y=WithinClusterSS)) + geom_line(colour = "lightblue", size=1, linetype=2) + 
  geom_point(shape=16, color="blue", size=3) +
  theme_minimal() +  ggtitle("Reduction In WSS For Values of K") +  xlab("Num Clusters") + 
  ylab("Within Sum of Squares") +  scale_x_continuous(breaks=seq(from=1, to=10, by= 1))



