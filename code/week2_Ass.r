# **** Problem 1 ****

rm(list=ls())
data(state)
ls()
class(state.x77)
state.x77[1:3,]
data.frame(state.x77)
statedata = cbind(data.frame(state.x77), state.abb, state.area, state.center,  state.division, state.name, state.region)
str(statedata)
plot(statedata$x, statedata$y, type='p', pch=20, col='red')
str(statedata)
tapply(statedata$HS.Grad, statedata$state.region, mean, na.rm=T)
boxplot(statedata$Murder ~ statedata$state.region)
neast = subset(statedata, statedata$state.region == 'Northeast')
table(neast$state.name, neast$Murder)
sort(tapply(neast$Murder, neast$state.name, mean, na.rm=T))
# All variable linear model
all.lm = lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost + Area, data=statedata)
summary(all.lm)
plot(statedata$Income, statedata$Life.Exp)
simp.lm = lm(Life.Exp ~ Population + Murder + HS.Grad + Frost, data=statedata)
summary(simp.lm)
simp.lm$model
pred.simp = predict(simp.lm)
sort(pred.simp, decreasing=T) 
statedata[which.min(statedata$Life.Exp),] # State with the lowest life exp
statedata[which.max(statedata$Life.Exp),] # State with the highest life exp
sort((pred.simp - statedata$Life.Exp) ^ 2)

# **** Problem 2 ****

rm(list=ls())
# Climate Change Analytics
url.ch = 'https://courses.edx.org/c4x/MITx/15.071x/asset/climate_change.csv'
download.file(url.ch, destfile='./data/climate_change.csv', method='curl')
clmChng = read.csv('./data/climate_change.csv')
str(clmChng)
clmChng.train = subset(clmChng, clmChng$Year <= 2006)
summary(clmChng.train)
clmChng.test = subset(clmChng, clmChng$Year > 2006)
summary(clmChng.test)
clm.lm = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data=clmChng.train)
# clm.lm2 with variables, MEI, TSI, Aerosols and N2O

clm.lm2 = lm(Temp ~ MEI + TSI + Aerosols + N2O, data=clmChng.train)
summary(clm.lm2)

# AUTOMATICALLY BUILDING THE MODEL using the step() function with AIC evaluation.
#The step function will not necessarily produce a very interpretable model - 
#just a model that has balanced quality and simplicity for a particular weighting 
#of quality and simplicity (AIC).

clm.lm.step = step(clm.lm)
summary(clm.lm.step)
ls()

pred.lmstep = predict(clm.lm.step, newdata=clmChng.test)

rsquared <- function(predicted,actual){
  SSE <- sum((predicted - actual) ^ 2)
  TSE <- sum((mean(actual) - actual) ^ 2)
  RSq <- 1-(SSE/TSE)
  return (RSq)
}

rsquared(pred.lmstep, clmChng.test$Temp)
SS = sum((pred.lmstep - clmChng.test$Temp) ^ 2)
ST = sum((mean(clmChng.test$Temp) - clmChng.test$Temp) ^ 2)
1 - (SS/ST) 
length(pred.lmstep)

# **** Problem 3 - Reading Test Scores ****

pisaTrain = read.csv('./data/pisa2009train.csv')
pisaTest = read.csv('./data/pisa2009test.csv')
str(pisaTrain)
str(pisaTest)
tapply(pisaTrain$readingScore, pisaTrain$male, mean, na.rm=T)
summary(pisaTrain)
# Removing NA's 
pisaTrain = na.omit(pisaTrain)

pisaTest = na.omit(pisaTest)
# Releveling factor variable to start with 'White'
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
str(pisaTrain)
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")
lmScore = lm(readingScore ~., data=pisaTrain)
summary(lmScore)
RMSE = sqrt(sum(lmScore$residuals ^ 2)/nrow(pisaTrain))
RMSE
t2 = pisaTrain[which(pisaTrain$grade == 11 | pisaTrain$grade==9),][1,]
t2 = rbind(t2, t2)
t2
t2[1,1] = 9
pred.t2 = predict(lmScore, newdata=t2)
pred.t2[2]
predTest = predict(lmScore, newdata=pisaTest)
max(predTest) - min(predTest)
SSE = sum((predTest - pisaTest$readingScore) ^ 2)
SSE
SST = sum((mean(pisaTest$readingScore) - pisaTest$readingScore) ^ 2)
1- SSE/SST
RMSE = sqrt(SSE/nrow(pisaTest))
RMSE
mean(predict(lmScore))
SSTr = sum((pisaTrain$readingScore - mean(pisaTrain$readingScore)) ^ 2)
SSTr

rm(list=ls())

# **** Problem 4 ****

FluTrain <- read.csv('./data/FluTrain.csv')
str(FluTrain)
attach(FluTrain)
table(Week, ILI)
sort(tapply(ILI, Week, mean, na.rm=T), decreasing=T)
hist(ILI, breaks=200)
#When handling a skewed dependent variable, it is often useful to predict 
# the logarithm of dependent variable instead of the dependent variable itself 
#-- this prevents the small number of unusually large or small observations from 
# having an undue influence on the sum of squared errors of predictive models. 
plot(log(ILI), Queries)
FluTrend1 = lm(log(ILI) ~ Queries, data=FluTrain)
summary(FluTrend1)
FluTest = read.csv('./data/FluTest.csv')
PredTest1 = exp(predict(FluTrend1, newdata=FluTest))
tmp = PredTest1[which(substr(FluTest$Week, 1, 10) == '2012-03-11')]

# relative error is calculated as
# (Observed ILI - Estimated ILI)/Observed ILI
(FluTest[11, 2] - tmp)/FluTest[11, 2]
SSE = sum((PredTest1 - FluTest$ILI) ^ 2)
SST = sum((mean(FluTest$ILI) - FluTest$ILI) ^ 2)
RMSE = sqrt(SSE/nrow(FluTest))

# Creating a new variable that contains ILI values from 2 weeks ago
ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)
plot(log(FluTrain$ILILag2), log(FluTrain$ILI))
FluTrend2 = lm(log(ILI) ~ Queries + log(ILILag2), data=FluTrain)
summary(FluTrend2)
# Adding new variable into test data
ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2)
summary(FluTest)
order(FluTest, by=Week)
sort(FluTrain$Week, decreasing=T)
which(is.na(FluTest$ILILag2))
FluTest[1,] = 1.852736
FluTest[2,] = 2.12413
FluTrain[nrow(FluTrain),]
predTest2 = exp(predict(FluTrend2, newdata=FluTest))
SSE2 = sum((predTest2 - FluTest$ILI) ^ 2)
SST2 = sum((mean(FluTrain$ILI) - FluTest$ILI) ^ 2)
R2 = 1-(SSE2/SST2)
SSE = sum((PredTest1 - FluTest$ILI) ^ 2)
RMSE2 = sqrt(SSE2/nrow(FluTest))
