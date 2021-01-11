library(psych)
library(foreign)
library(DBI)
library(readxl)
library(haven)
library(car)
library(effects)
library(corrplot)
library(BaylorEdPsych)
library(ggfortify)
library(cluster)
library(klaR)
library(rrcov)
library(gmodels)
library(QuantPsyc)
library(caret)
library(sjstats)
library(lm.beta)
library(DescTools)

setwd("~/MSBA/MOD3/Code/R/7208")


dat.Eretail <-  read_xls("data/Eretail7040.xls", sheet=1)
dat.WHBM <- read_xlsx("data/WHBM7040.xlsx", sheet = 1) 
dat.PreDrugData <- read_xlsx("data/PreDrugData7040.xlsx", sheet = 1)
dat.AlcoaData <- read_xlsx("data/Alcoa.xlsx", sheet = 1)

str(dat.Eretail)

# 1. Summary stats in R: multiple variables
describe(dat.Eretail[, c("discount" , "satfirm")])

# 2. Histogram in R
hist(dat.Eretail$discount)

# 3. Scatterplot in R
plot(dat.Eretail$discount, dat.Eretail$satfirm)

# 4. Barplot in R
barplot(table(dat.Eretail$eschool))

#5. One-Way ANOVA in R 
dat.Eretail$BuyAgain <- factor(dat.Eretail$intent)
model <- lm(satfirm ~ BuyAgain, data = dat.Eretail)
summary(model)
allEffects (model)
Anova(model)
plot(allEffects(model))


#6 Multiple Regression in R w. Summary Stats & Correlations
describe(dat.Eretail[, c("intent" ,"discount" ,"cusij" ,"severity")])
cor(dat.Eretail[, c("intent" ,"discount" ,"cusij" ,"severity")])
corrplot(cor(dat.Eretail[, c("intent" ,"discount" ,"cusij" ,"severity")]))
dat.Eretail.lm.reg <- lm(intent ~ discount + cusij + severity, data = dat.Eretail)
summary(dat.Eretail.lm.reg)
coef(dat.Eretail.lm.reg)
lm.beta(dat.Eretail.lm.reg)
Anova(dat.Eretail.lm.reg)
plot(allEffects(dat.Eretail.lm.reg))

#7a. Stepwise Regression in R: Produces Final Model
dat.Eretail.lm.reg <- lm(intent ~ satfirm + ecustocb + jobstress + cuspj + cusij + severity + cincome + cyears + cage + cgender + cschool, data = dat.Eretail)
dat.Eretail.step <- stepAIC(dat.Eretail.lm.reg, direction = "both")
dat.Eretail.step$anova

#7b. Stepwise Regression in R: need to fully run the Final model to obtain estimates.
cor(dat.Eretail[, c("intent", "satfirm","ecustocb", "cuspj" ,"cusij" ,"jobstress")])
corrplot(cor(dat.Eretail[, c("intent" , "satfirm" , "ecustocb", "cuspj" ,"cusij" ,"jobstress")]))
dat.Eretail.lm.reg <- lm(intent ~ satfirm + ecustocb + cuspj + cusij + jobstress, data = dat.Eretail)
summary(dat.Eretail.lm.reg)
coef(dat.Eretail.lm.reg)
lm.beta(dat.Eretail.lm.reg)
Anova(dat.Eretail.lm.reg)
plot(allEffects(dat.Eretail.lm.reg))

#8 Logistic Regression in R
describe(dat.Eretail[, c("intent" ,"satfirm" ,"cusij" ,"severity")])
cor(dat.Eretail[, c("intent" ,"satfirm" ,"cusij" ,"severity")])
corrplot(cor(dat.Eretail[, c("intent" ,"satfirm" ,"cusij" ,"severity")]))
myGLM <- glm(BuyAgain ~ satfirm + cusij + severity, data = dat.Eretail, family = "binomial")
summary(myGLM)
AIC(myGLM)
anova(myGLM, test = "Chisq")
exp(confint(myGLM))
exp(coef(myGLM))
PseudoR2(myGLM)
plot(allEffects(myGLM))
plot(allEffects(myGLM), type = "response")
confusionMatrix(factor(ifelse(predict(myGLM, type = "response") >= .5, 1, 0)), reference = factor(dat.Eretail$BuyAgain))



