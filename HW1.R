# 7208 HW 1
library(psych)
library(tidyr)
library(ggplot2)
library(dplyr)
library(readxl)
library(ggthemes)
library(rsconnect)
library(ggpointdensity)
library(ggpmisc)
library(tidyr)
library(plyr)
library(dplyr)
library(ggmap)
library(ggpubr)
library(ggExtra)
library(DescTools)
library(corrplot)

setwd("~/MSBA/MOD3/Code/R/7208_1-3")

ggplotRegression <- function (fit, lineCol) {
  require(ggplot2)
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point(shape=21, color="#273443", fill="#1EBEA5", position = 'jitter') +
    stat_smooth(method = "lm", col = lineCol,) +
    labs(caption =  paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                          "Intercept =",signif(fit$coef[[1]],5 ),
                          " Slope =",signif(fit$coef[[2]], 5),
                          " P =",signif(summary(fit)$coef[2,4], 5))) + theme_classic2()
}


alcoaDF <- read_excel("data/Alcoa1.xlsx")
print(alcoaDF)
str(alcoaDF)
head(alcoaDF)

alcoaCor <- alcoaDF[c("specbuy","speed",
                      "pricefle","mimage","service","quality")] %>% 
  cor()



# alcoaCor <-cor(alcoaDF)
corrplot(alcoaCor, method="circle")

all <- lm(satisfaction ~ speed+specbuy+pricefle+mimage+service+quality, data= alcoaDF)
summary(all)
coef(all)
# std coeff
lm.beta(all)


Anova(all, test = "Chisq")
exp(confint(all))
exp(coef(all))
# PseudoR2(all)
plot(allEffects(all))
plot(allEffects(all), type = "response")
confusionMatrix(factor(ifelse(predict(all, type = "response") >= .5, 1, 0)), reference = factor(alcoaDF$satisfaction))





speed <- lm(satisfaction ~ speed, data= alcoaDF)
x=ggplotRegression(speed, "#ADB9D3") 
ggpar(p=x, title=" Speed v satisfaction Regression")

specbuy <- lm(satisfaction ~ specbuy, data= alcoaDF)
x=ggplotRegression(specbuy, s"#ADB9D3") 
ggpar(p=x, title="specbuy v satisfaction Regression")

pricefle <- lm(satisfaction ~ pricefle, data= alcoaDF)
x=ggplotRegression(pricefle, "#ADB9D3") 
ggpar(p=x, title="Pricefle v satisfaction Regression")

mimage <- lm(satisfaction ~ mimage, data= alcoaDF)
x=ggplotRegression(mimage, "#ADB9D3") 
ggpar(p=x, title=" mimage v satisfaction Regression")

service <- lm(satisfaction ~ service, data= alcoaDF)
x=ggplotRegression(service, "#ADB9D3") 
ggpar(p=x, title=" service v satisfaction Regression")

quality <- lm(satisfaction ~ quality, data= alcoaDF)
x=ggplotRegression(quality, "#ADB9D3") 
ggpar(p=x, title=" quality v satisfaction Regression")


