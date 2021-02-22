packages = c('lavaan', 'semPlot', 'OpenMx', 'tidyverse', 'knitr', 'kableExtra', 'GGally')

package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

setwd("~/MSBA/MOD3/7208 Customer Analytics/code/7208_Homework")
# ======================= ALCOA===============
dat <- read.csv("data/alcoa.csv")
head(dat)
nrow(dat)

# It looks like you define each equation on a separate line
model <- '
usage  ~ satisfaction + speed 
satisfaction ~ simage + pricefle + speed
'


fit <- cfa(model, data = dat)

summary(fit, fit.measures = TRUE, standardized=T,rsquare=T,modindices = TRUE)


#plot the paths a couple different ways
semPaths(fit, what='std', whatLabels = "std", edge.label.cex = 1.2, label.cex = 2, layout = "tree2", rotation=2, edge.color='purple')

semPaths(fit, what='std', whatLabels = "par", edge.label.cex = 1.2, label.cex = 2, layout = "tree2", rotation=2, edge.color='purple')

# semPaths(fit, what='par', whatLabels = "par", edge.label.cex = 1.2, label.cex = 2, layout = "tree2", rotation=2)

# semPaths(fit, 'std', layout = 'tree2', intercepts = TRUE)
# 
# semPaths(fit, "std", "hide", label.cex=1.5)
# 
# 
# semPaths(fit, "std", edge.label.cex = 0.5, curvePivot = TRUE, layout="tree2")




#========================  WHBM Below =============

dat <- read.csv("data/WHBM7040.csv")
head(dat)
nrow(dat)

# It looks like you define each equation on a separate line
model <- '
TotalYearlyACTV ~ ValueForPrice + cussat
cussat ~ sophis + ValueForPrice + csr + empsim
'


fit <- cfa(model, data = dat)

summary(fit, fit.measures = TRUE, standardized=T,rsquare=T,modindices = TRUE)


#plot the paths a couple different ways
semPaths(fit, whatLabels = "est", edge.label.cex = 1.1, label.cex = 2, layout = "tree2")

semPaths(fit, 'std', layout = 'tree2', intercepts = TRUE)

semPaths(fit, "std", "hide", label.cex=1.5)


semPaths(fit, "std", edge.label.cex = 0.5, curvePivot = TRUE, layout="tree2")

