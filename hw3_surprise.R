library(corrplot)
library(psych)
library(ellipse)
library(caret)
library(readxl)
library(DescTools)
library(effects)


setwd("~/MSBA/MOD3/7208 Customer Analytics/code/7208_Homework")

# Read data

alcoa <- read_excel('data/alcoa.xls', sheet=1)
alcoa <- as.data.frame(alcoa)

#Summary
summary(alcoa[,c('CustomerValueGroup','pricefle','quality','speed')])

# shows no null values
# sapply(alcoa,function(x) sum(is.na(x)))

#Correlation plot
correlations <- cor(alcoa[,c('CustomerValueGroup','pricefle','quality','speed')])
corrplot.mixed(correlations, upper='ellipse')
corrplot.mixed(correlations, upper='ellipse')


# set DV to factor
alcoa$CustomerValueGroup = factor(alcoa$CustomerValueGroup)

#Feature plot / EDA
xAxis <- alcoa[,c('pricefle','quality','speed')]
yAxis <- alcoa[,'CustomerValueGroup']
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=xAxis, y=yAxis, plot="density", auto.key = list(columns = 2), scales=scales)
featurePlot(x=xAxis, y=yAxis, plot="box", auto.key = list(columns = 2), scales=scales)
# xtabs(~CustomerValueGroup + pricefle, data = alcoa)


# Calculate model
logit <- glm(CustomerValueGroup ~ pricefle + quality + speed, data = alcoa, family = binomial(link="logit"))
summary(logit)

#Summary
AIC(logit)
# exp(confint(logit))
# exp(coef(logit))

exp(cbind(OddsRatio = coef(logit), confint(logit)))
PseudoR2(logit)
# plot(allEffects(logit))
plot(allEffects(logit), type = "response")

confusionMatrix(factor(ifelse(predict(logit, type = "response") >= .5, 1, 0)), reference = factor(alcoa$CustomerValueGroup))

anova(logit, test="Chisq")

predicted.data <- data.frame(
  probability.of.CVG=logit$fitted.values,
  valGroup=alcoa$CustomerValueGroup)

predicted.data <- predicted.data[
  order(predicted.data$probability.of.CVG, decreasing=FALSE),]

predicted.data$rank <- 1:nrow(predicted.data) 

ggplot(data=predicted.data, aes(x=rank, y=probability.of.CVG)) +
  geom_point(aes(color=valGroup), alpha=.7, shape=1, stroke=2) +
  xlab("Customer Number (1-100)") +
  ylab("Predicted probability of Value Group") + theme_minimal()


#Oneway ANOVA Check

# dat.AlcoaData$CustomerValueGroup <- factor(dat.AlcoaData$CustomerValueGroup)


alcoa$CustomerValueGroup = as.numeric(alcoa$CustomerValueGroup)


# alcoa <- read_excel('data/alcoa.xls', sheet=1)
alcoa <- as.data.frame(alcoa)
anova_one_way <- aov(CustomerValueGroup ~ pricefle+quality+speed,  data = alcoa)

summary(anova_one_way)
                          
