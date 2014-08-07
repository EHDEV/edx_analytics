# Week 2 - Lecture and Exercises

# ***** Wine *****
rm(list=ls())
url.wine <- 'https://courses.edx.org/c4x/MITx/15.071x/asset/wine.csv'
download.file(url.wine, destfile='./data/wine.csv', method='curl')
download.file('https://courses.edx.org/c4x/MITx/15.071x/asset/wine_test.csv', destfile='./data/wine_test.csv', method='curl')
wine = read.csv('./data/wine.csv')
setwd('./Documents/workspace/edx_analytics/')
summary(wine)
model1 = lm(Price ~ AGST, data=wine)
model1
summary(model1)
SSE = sum(model1$residuals ^ 2)
SSE
model2 = lm(Price ~ AGST + HarvestRain, data=wine)
summary(model2)
sum(model2$residuals ^ 2)
model3 = lm(Price ~ ., data=wine)
summary(model3)
SSE = sum(model3$residuals ^ 2)
SSE
lm4 <- lm(Price ~ HarvestRain + WinterRain, data=wine)
summary(lm4)
lm4

# std error - how much the coefficient is likely to vary from the estimate value
# tvalue - estimate/standard error - the larger the abs(t-statistic) the more likely the coefficient is to be significant
# Pr(>|t|) - prob that a coefficient is actually 0. Large if abs(|t|) is small and small if abs(|t|) large.
# Signif. codes: ***, **, *, .

model4 = lm(Price ~ AGST + HarvestRain + WinterRain + Age, data=wine)
summary(model4)
# Age is now a significan variable due to multicolinearity. i.e. Age and population are strongly correlated causing them to be insignificant in the model

cor(wine$WinterRain, wine$Price)
# 
lm6 <- lm(Price ~ HarvestRain + WinterRain, data=wine)
summary(lm6)
wine.test = read.csv('./data/wine_test.csv')
str(wine.test)
predictTest = predict(model4, newdata=wine.test)
predictTest
# R^2 = 1 - SSE/SST
SSE = sum((wine.test$Price - predictTest) ^ 2)
SST = sum((wine.test$Price - mean(wine$Price)) ^ 2)
R_Squared = 1 - SSE/SST
R_Squared

# Better model R^2 doesn't mean a better test R^2
wine.test$Price = predictTest

#****** Moneyball ******

rm(list=(ls()))
mb.url = 'https://courses.edx.org/c4x/MITx/15.071x/asset/baseball.csv'
download.file(mb.url, destfile = './data/baseball.csv', method='curl')
baseball = read.csv('./data/baseball.csv')
str(baseball)
moneyball = subset(baseball, Year < 2002)
str(moneyball)
moneyball$RD = moneyball$RS - moneyball$RA
WinsReg = lm(W ~ RD, data=moneyball)
summary(WinsReg)
RunsReg <- lm(RS ~ OBP + SLG + BA, data=moneyball) # to predict Runs Scored from ind variables, On base percentage, Slugging percentage and batting average
summary(RunsReg2)
str(moneyball)
RunsReg2 <- lm(RS ~ OBP + SLG, data=moneyball) 
RunsAllReg <- lm(RA ~ OOBP + OSLG, data=moneyball) # predicting runs allowed from opponent's on base percentage and slugging average
summary(RunsAllReg)
t = as.data.frame(cbind(0.297, 0.370),c('OOBP', 'OSLG'))
predict(RunsReg2, t)
names(t) = c('OBP', 'SLG')
t2 <- t
names(t2) = c('OOBP', 'OSLG')
predict(RunsAllReg, t2)
teamRank = c(1,2,3,3,4,4,4,4,5,5) # Team rank vector
str(baseball)
# Creating vectors using the %in% keyword - which is equivalent to the 'in' operator in SQL
wins2012 <- baseball[which(baseball$RankPlayoffs %in% teamRank & baseball$Year == '2012'),6]
wins2013 <- baseball[which(baseball$RankPlayoffs %in% teamRank & baseball$Year == '2013'),6]

cor(teamRank, wins2012)

