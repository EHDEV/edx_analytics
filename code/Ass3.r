songs = read.csv('./data/songs.csv')
rm(list=ls())
str(songs)
length(which(songs$year=='2010'))
length(which(songs$artistname=='Michael Jackson'))
a = data.frame(table(songs[, c(3, 39)]))
a[c(1:5),]
a[which(a$artistname=="Michael Jackson"),]
songs[which(songs$Top10==1 & songs$artistname=="Michael Jackson"),c(2,39)]
unique(songs$timesignature)
table(songs$songtitle, songs$tempo)
songs[which.max(songs$tempo),c(1:5)]
SongsTest = subset(songs, songs$year=='2010')
str(SongsTrain)
SongsTrain = subset(songs, songs$year!='2010')
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]
model1 = glm(Top10 ~ .,data=SongsTrain,family=binomial)
summary(model1)
cor(songs$loudness, songs$energy)
SongsLog2 = glm(Top10 ~ . -loudness, data=SongsTrain, family=binomial)
summary(SongsLog2)
SongsLog3 = glm(Top10 ~ . -energy, data=SongsTrain, family=binomial)
summary(SongsLog3)
pred.3 = predict(SongsLog3, type='response', newdata=SongsTest) 
table(SongsTest$Top10)
head(pred.3)
table(SongsTest$Top10, pred.3 > 0.45)
19/(59)
# **** Problem 2 ****

baseball = read.csv("./data/baseball.csv")

sum(table(baseball$Year,baseball$Team))
length(table(baseball$Year))
baseball = subset(baseball, subset=baseball$Playoffs == 1)
nrow(baseball)
str(baseball)
tapply(baseball$Playoffs, baseball$Year, sum)
PlayoffTable = table(baseball$Year)
names(PlayoffTable)
PlayoffTable(c("1990", "2001"))
PlayoffTable["1990", "2001"]
PlayoffTable[c("1990", "2001")]
PlayoffTable[as.character(baseball$Year)]
baseball$NumCompetitors = PlayoffTable[as.character(baseball$Year)] 
length(which(baseball$NumCompetitors == 8))
baseball$WorldSeries = as.numeric(baseball$RankPlayoffs == 1)
length(which(baseball$WorldSeries != 1))
table(baseball$WorldSeries)
length(baseball)
bi1=glm(WorldSeries ~ Year, data=baseball, family=binomial)
bi2=glm(WorldSeries ~ RS, data=baseball, family=binomial)
bi3=glm(WorldSeries ~ RA, data=baseball, family=binomial)
bi4=glm(WorldSeries ~ W, data=baseball, family=binomial)
bi5=glm(WorldSeries ~ OBP, data=baseball, family=binomial)
bi6=glm(WorldSeries ~ SLG, data=baseball, family=binomial)
bi7=glm(WorldSeries ~ BA, data=baseball, family=binomial)
bi8=glm(WorldSeries ~ RankSeason, data=baseball, family=binomial)
bi9=glm(WorldSeries ~ OOBP, data=baseball, family=binomial)
bi10=glm(WorldSeries ~ OSLG, data=baseball, family=binomial)
bi11=glm(WorldSeries ~ League, data=baseball, family=binomial)
bi12=glm(WorldSeries ~ NumCompetitors, data=baseball, family=binomial)
summary(bi13)
bi13= glm(WorldSeries ~ Year + RA + RankSeason + NumCompetitors, data=baseball, family=binomial)
str(baseball)
cor.test(baseball[,c(2,4,10,15)])
cor(baseball$RankSeason,baseball$NumCompetitors)

# *** Prob 3 ***

parole = read.csv('./data/parole.csv')
str(parole)
table(parole$violator)
parole$state = as.factor(parole$state)
# set.seed(144)
# library(caTools)
# split = sample.split(parole$violator, SplitRatio = 0.7)
# train = subset(parole, split == TRUE)
# test = subset(parole, split == FALSE)
# nrow(train)
# train[1:3,]
# train2[1:3,]
# tmp.par = test[1,]
# tmp.par$state = 1
# str(tmp.par)
# tmp.par$state = as.factor(tmp.par$state)
# p.tmp = predict(log1, newdata=tmp.par, type='response')
# p.tmp
# pred.par = predict(log1, newdata=test, type='response')
# log1 = glm(violator ~.,data=train, family=binomial)
# summary(log1)
# log(1.827964)
# str(train)
# exp((0.886637*1) + (0.886637 * 50) + (1 * -4.130588) + (0.094405 * 3) + (0.033379 * 12) + 1.741437 + (-0.017208 * 2))
set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)

train = subset(parole, split == TRUE)

test = subset(parole, split == FALSE)
str(test)
glm.par = glm(violator ~., data=train, family='binomial')
pred.test.par = predict(glm.par, newdata=test, type='response')
sort(pred.test.par, decreasing=T)[1]
tmp.par$state = as.factor(tmp.par$state)
table(test$violator, pred.test.par > 0.5)
tempred = predict(glm.par, newdata=tmp.par)
(167+12)/nrow(test)
table(test$violator)
179/nrow(test)
# *** Prob 4 *** 
tempred
loans = read.csv('./data/loans.csv')
table(loans$not.fully.paid)
1533/(8045 + 1533)
summary(loans)
install.packages('mice')
library(mice)

set.seed(144)

vars.for.imputation = setdiff(names(loans), "not.fully.paid")

imputed = complete(mice(loans[vars.for.imputation]))
iloans = read.csv('./data/loans_imputed.csv')
str(iloans)
loans[vars.for.imputation] = imputed

set.seed(144)
split = sample.split(iloans$not.fully.paid, SplitRatio = 0.7)
train = subset(iloans, split == TRUE)
test = subset(iloans, split == FALSE)
il.mod = glm(not.fully.paid ~., data=train, family=binomial)
summary(il.mod)
tsnew = test[1,]
tsnew = rbind(tsnew,tsnew)
str(tsnew)
tsnew$fico = c(710, 700)
pr = predict(il.mod, newdata=tsnew)
pr[1]-pr[2]
predicted.risk = predict(il.mod, newdata=test, type='response')
test$predicted.risk = predicted.risk
table(test$not.fully.paid, test$predicted.risk > 0.5)
2413/nrow(test)
table(test$not.fully.paid)
intrate.glm = glm(not.fully.paid ~ int.rate, data=train, family=binomial)
summary(intrate.glm)
pred.int.risk = predict(intrate.glm, newdata=test, type='response')
pred.int.risk = sort(pred.int.risk, decreasing=T)
head(pred.int.risk)
table(pred.int.risk > 0.5)
10 * exp(3*.06)
# Calculating profit
test$profit = exp(test$int.rate*3) - 1

test$profit[test$not.fully.paid == 1] = -1
sort(test$profit, decreasing=T)[1] * 10
highInterest = subset(test, int.rate >= .15)
cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]
selectedLoans = subset(highInterest, predicted.risk <= cutoff)
nrow(selectedLoans)
selectedLoans$profit
table(selectedLoans$not.fully.paid)
sum(selectedLoans$profit)
