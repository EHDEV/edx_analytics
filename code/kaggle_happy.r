#************ KAGGLE COMPETITION : HAPPINESS PREDICTION ************

setwd('./Documents/workspace/edx_analytics/')

happy.data <- read.table('./data/happy.csv',header=T, sep=',',na.strings=c("", "NA"), strip.white=T)
happy.data <- read.csv('./data/happy.csv')
str(h2)
summary(happy.data)
table(happy.data$YOB)
head(happy.data[which(is.na(happy.data$YOB)==T),1:10])
t <- as.data.frame(table(happy.data$YOB, happy.data$HouseholdStatus))
happy.data$YOB = as.integer(happy.data$YOB)
happy.data$Age = as.integer(happy.data$Age)
happy.data$Age <- (2014-happy.data$YOB)
happy.data$Happy <- as.factor(happy.data$Happy)
table(happy.data$Income, happy.data$Party)

# ***** h2 ****
str(happy.data)
happy.data = happy.data[,-111]
h2$Happy = as.factor(h2$Happy)
testHappy$votes <- as.integer(testHappy$votes)
testHappy$YOB <- as.integer(testHappy$YOB)

testHappy <- read.csv('./data/test_hpy.csv')

which(testHappy$UserID == 1995)
set.seed(333)
library(caTools)

spl = sample.split(happy.data$Happy, SplitRatio=0.7)
str(spl)

s

#^^^^^^^^^^^^^^^^^^^^^^^^
happy.data <- read.csv('./data/happy.csv')

library(zoo)
h4z <- na.locf(happy.data$YOB, , na.rm=F)
happy.data$YOB <- h4z

library(caTools)
happy.data$Happy <- as.factor(happy.data$Happy)
happy.data$YOB <- as.integer(happy.data$YOB)

testFinComplete$YOB <- as.integer(testFinComplete$YOB)

testFin2 <- read.csv('./data/happy_test.csv')
testFin2$YOB <- na.locf(testFin2$YOB, , na.rm=F)


set.seed(100)
spl = sample.split(happy.data$Happy, SplitRatio=0.7)

train.happy <- subset(happy.data, spl==T)
test.happy <- subset(happy.data, spl ==F)

log1<- glm(Happy ~ . -UserID -votes -Income -Q111580 -Q117193 -Q116881 -Q99982 -Q111220 -Q122770 -Q116953 -Q113583 -Q114152 -Q101596 -Q99581 -Q99480 -Q98578 -Q118117 -Q98078 -Q105840 -Q114961 -Q118892 -Q121699 -Q118232 -Q114517 -Q113181 -Q110740 -Q108950 -Q114748 -Q118233 -Q116197 -Q115602 -Q101163 -Q109367 -Q106272 -Q96024 -Q120472 -Q119851 -Q112270 -Q111848 -Q120650 -Q120978 -Q122120 -Q112478 -Q108342 -Q113584 -Q108754 -Q103293 -Q98059 -Q100562 -YOB -EducationLevel -Q121700 -Q124742 -Q124122 -Q123464 -Q123621, data=train.happy, family=binomial)

predtrf <- predict(tune.happy, newdata=test.happy, type='prob')

table(test.happy$Happy, predtrf[,2] >= 0.5)
tune.happy <- tuneRF(train.happy[,-c(8, 1)], train.happy[,8], stepFactor=1.5,doBest=T)


#^^^^^^^^^^^^^^^^^^^^^^^^^^


library(randomForest)
rft <- tuneRF(Happy ~ . -UserID -votes -Income -Q111580 -Q117193 -Q116881 -Q99982 -Q111220 -Q122770 -Q116953 -Q113583 -Q114152 -Q101596 -Q99581 -Q99480 -Q98578 -Q118117 -Q98078 -Q105840 -Q114961 -Q118892 -Q121699 -Q118232 -Q114517 -Q113181 -Q110740 -Q108950 -Q114748 -Q118233 -Q116197 -Q115602 -Q101163 -Q109367 -Q106272 -Q96024 -Q120472 -Q119851 -Q112270 -Q111848 -Q120650 -Q120978 -Q122120 -Q112478 -Q108342 -Q113584 -Q108754 -Q103293 -Q98059 -Q100562 -YOB -EducationLevel -Q121700 -Q124742 -Q124122 -Q123464 -Q123621, data=train.happy, method='class')

rf2 <- randomForest(Happy ~ . -UserID, data=train.happy, method='class')
pred.rf.happy <- predict(rf2, newdata=test.happy,type='prob')
rfPred <- predict(rf, newdata=test.happy, type='prob')
str(pred.log.h4)
pred.log.h4[1:5,]
testHappy <- read.csv('./data/happy_test.csv')
str(FinalPredrf)
summary(happy.data)
summary(testHappy)
table(test.happy$Happy, rfPred[,2]>=0.5)
table(test.happy$Happy, pred.rf.happy[,2]>=0.5)
FinalPredrf <- predict(rf2, newdata=testHappy,type='prob')
length(which(is.na(FinalPredrf)))
FinalPredrf[1:1,]
dim(h4.tn)
dim(h4.ts)
h4.ts$UserID = as.factor(h4.ts$UserID)
summary(log.h4)
3815
submitRF <- data.frame(UserID=testHappy$UserID, Probability1=FinalPredrf[,2])
str(submitRF)
names(submitRF) = c("UserID", "Probability1")
submitRF$UserID = as.character(submitRF$UserID)
summary(submitRF)
write.table(submitRF, file="./data/submission.csv", row.names=F, sep=',')
log2.h4 <- glm(Happy ~ . -UserID, data=h4.tn, family=binomial)
summary(log.h4)
submission = read.csv('./data/submission.csv')
str(submission)
submission[which(submission$UserID==1983),]
nrow(submission)
nrow(testHappy)
usid = unique(testHappy$UserID)
usid2 = unique(submission$UserID)
h4z$votes = as.factor(h4z$votes)
str(h4z$votes)
pred.log.h <- predict(log.h, newdata=h4.ts)
str(pred.log.h)
pred.tree.h4 <- predict(h4.tree, newdata=h4.ts, type='class')
table(h4.ts$Happy, pred.tree.h4)
summary(log.h4)
log.h$coefficients
h.tree$splits
j <- as.data.frame(as.matrix(tapply(happy.data$Age, happy.data$HouseholdStatus, mean, na.rm = T)))
j

table(happy.data$HouseholdStatus)
sort(testHappy$UserID)

split = sample.split(happy.data, SplitRatio=0.7)

happy.train <- subset(happy.data, split==T)
happy.test <- subset(happy.data, split ==F)

library(rpart)
library(rpart.plot)
tree.happy = rpart(Happy ~ .-UserID, data=happy.train, method='class')
prp(tree.happy)
pred.tree = predict(tree.happy, newdata=happy.test)
str(pred.tree)
pred.tree[1:5,]
plot(tree.happy)
text(tree.happy)
log.happy <- glm(Happy ~ .-UserID, data=happy.train, family=binomial)
summary(log.happy)
table(happy.test$Happy, pred.tree[,2] >= 0.5)
sum(287, 608)/nrow(happy.test)

nrow(unique(happy.data))
nrow(happy.data)

# Seeing if I can create a new variable called Age from YOB (Year of Birth)
bYear <- happy.data$YOB
str(bYear)
str(happy.data)
distances <- dist(happy.data, method='euclidean')
str(distances)
cluster.happy <- hclust(distances, method='ward')
str(cluster.happy)
plot(cluster.happy)

clustGroups.happy <- cutree(cluster.happy, k=6)
str(clustGroups.happy)
happy.data$Happy <- as.factor(happy.data$Happy)
# Getting the percentage of respondents who indicated that they are happy
tapply(happy.data$Happy, clustGroups.happy, mean)
table(happy.data$Happy)
# Subsetting the data into each cluster
c1 = subset(happy.data, clustGroups.happy==1)
c2 = subset(happy.data, clustGroups.happy==2)
c3 = subset(happy.data, clustGroups.happy==3)
c4 = subset(happy.data, clustGroups.happy==4)
c5 = subset(happy.data, clustGroups.happy==5)
c6 = subset(happy.data, clustGroups.happy==6)
c7 = subset(happy.data, clustGroups.happy==7)
c8 = subset(happy.data, clustGroups.happy==8)

c1.train <- subset(c1, split == T)
c1.test <- subset(c1, split == F)
str(c1.train$Happy)
installed.packages()
library(caret)
library(e1071)

fitControl = trainControl(method='cv', number=10) # method='cv' use cross validation, number = 10 means use 10-fold cross validation
cartGrid = expand.grid(.cp=(1:50) * 0.01) # Picking Possible values for the parameter cp - use cp values .01 thru .5
cartGrid
tr2 <- train(Happy ~ ., data = h4.tn, method='rpart', trControl=fitControl, tuneGrid=cartGrid)
str(tr2)
best.tree2 <- tr2$finalModel
prp(tr2$finalModel)
summary(best.tree2)
pred.best.tree2 <- predict(best.tree2, newdata=c1.test)
best.tree2 <- tr2$finalModel
prp(tr$finalModel)
pred.best.tree2 <- predict(best.tree2, newdata=c1.test, type='class')

tr <- train(Happy ~ ., method='rpart', data = c1.train, trControl=fitControl, tuneGrid=cartGrid)
best.tree <- tr$finalModel
prp(best.tree)
summary(best.tree)
pred.best.tree <- predict(best.tree, newdata=c1.test)
length(which(is.na(happy.data)))
c1.tree <- rpart(Happy ~ ., data=c1.train, method='class', control=rpart.control(minbucket=7))
prp(c1.tree)
c1.pred.tree <- predict(c1.tree, newdata=c1.train)
table(c1$Happy, c1.pred.tree)
nrow(c1)
nrow(c1.pred.tree)
summary(happy.train)
library(randomForest)
?na.pass
tree2.happy <- rpart(Happy ~ EducationLevel + Q118237 + Q106997 + Q122771 + YOB , data=happy.train)
set.seed(333)

library(ROCR)
pred.ROC = predict(rf2, newdata=test.happy, type='prob')
head(pred.ROC)
summary(pred.ROC)
# Looking at the predict data we see two columns
# We use second column for thresholding
pred = prediction(pred.ROC[,2], test.happy$Happy)
perf = performance(pred, "tpr", "fpr") # true positive rate, false positive rate
plot(perf)
as.numeric(performance(pred, 'auc')@y.values)
?tuneRF
tune.happy <- tuneRF(train.happy[,-c(8, 1)], train.happy[,8], stepFactor=1.5,doBest=T)
str(train.happy)
levels(train.happy)
str(tune.happy)
str(tune.happy$forest)
class(tune.happy$forest)
tune.happy$mtry
tune.happy$
rf0 <- randomForest(Happy ~ . -UserID, data=train.happy, mtry=15)
pred.rf0 <- predict(rf0, newdata=testHappy, type='prob')
table(test.happy$Happy, pred.rf0[,2]>=0.5)
submitRF <- data.frame(UserID = testHappy$UserID, Probability1 = pred.rf0[,2])

write.table(submitRF, file="./data/submission.csv", row.names=F, sep=',')

submission = read.csv('./data/submission.csv')
str(submission)

str(testHappy$votes)
str

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Part II ----- Reading Happy data as table with blank values treated as missing values
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

rm(list=ls())

happy.data <- read.table('./data/happy.csv',header=T, sep=',',na.strings=c("", "NA"), strip.white=T)
happy.data <- read.csv('./data/happy.csv', na.strings = c("NA", ""))

testFin <- read.csv('./data/happy_test.csv', na.strings=c("", "NA"))

summary(happy.data)
str(happy.data)
library(mice)
names(happy.data)[8]

set.seed(1000)
happyComplete <- complete(mice(happy.data[,-c(1,8,110)], MaxNWts = 6000))
testFinComplete <- complete(mice(testFin[,-c(1,109)], MaxNWts = 6000))

happyComplete$UserID <- happy.data$UserID
happyComplete$Happy <- happy.data$Happy
happyComplete$votes <- happy.data$votes

testFinComplete$UserID <- happy.data$UserID
happyComplete$Happy <- happy.data$Happy
happyComplete$votes <- happy.data$votes

summary(happyComplete)
happy.data$votes = as.integer(happy.data$votes)
happy.data$UserID <- as.integer(happy.data$UserID)
happy.data$YOB = as.integer(happy.data$YOB)

check <- complete(mice(happy.data[,c("HouseholdStatus", "Party")], MaxNWts = 6000))

str(happy.data$EducationLevel)
levels(happy.data$EducationLevel)
happy.data$EducationLevel = trim(happy.data$EducationLevel)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
head(happy.data$EducationLevel)
View(happy.data)

for (i in 1:110){
  happy.data[,i] <- happy.data[happy.data[,i] == "",i] <- NA  
}
summary(natest)

testFinComplete <- complete(mice(testFin, MaxNWts = 6000))

names(happy.data)[8]
happyComplete$UserID <- happy.data$UserID
happyComplete$Happy <- happy.data$Happy
happyComplete$votes <- happy.data$votes

testFinComplete$UserID <- testFin$UserID
happyComplete$Happy <- testFin$Happy
testFinComplete$votes <- testFin$votes

library(caTools)
happyComplete$Happy <- as.factor(happyComplete$Happy)
happyComplete$YOB <- as.integer(happyComplete$YOB)

testFinComplete$YOB <- as.integer(testFinComplete$YOB)

set.seed(100)
spl = sample.split(happyComplete$Happy, SplitRatio=0.7)

train.happy <- subset(happyComplete, spl==T)
test.happy <- subset(happyComplete, spl ==F)


#------------------------------------------------------------------

library(randomForest)
names(train.happy)
tune.happy <- tuneRF(train.happy[,-109], train.happy$Happy, stepFactor=0.5, trace=T, plot=T, improve=0.05, doBest=T)

rf.happy <- randomForest(Happy ~ . -UserID, data=train.happy, method='class')

pred.rf <- predict(rf.happy, newdata=test.happy, type='prob')

pred.tune.rf <- predict(tune.happy, newdata=test.happy, type='prob')

table(test.happy$Happy, pred.tune.rf[,2] > 0.5)
table(test.happy$Happy, pred.rf[,2] > 0.5)

library(ROCR)
pred.ROC = prediction(pred.rf[,2], test.happy$Happy)
perf.ROC = performance(pred.ROC, "tpr", "fpr") # true positive rate, false positive rate
plot(perf.ROC)
as.numeric(performance(pred.ROC, 'auc')@y.values)

pred.tune.ROC = prediction(pred.tune.rf[,2], test.happy$Happy)
perf.tune.ROC = performance(pred.tune.ROC, "tpr", "fpr") # true positive rate, false positive rate
plot(perf.ROC)
as.numeric(performance(pred.tune.ROC, 'auc')@y.values)

pred.Fin <- predict(tune.happy, newdata=testFinComplete, type='prob')
names(testFinComplete)
names(testFin)
submitRF <- data.frame(UserID=testFin$UserID, Probability1=pred.Fin[,2])

write.table(submitRF, file="./data/submission.csv", row.names=F, sep=',')

#--------------------------------------------------------------------

log.hc <- glm(Happy ~ . -UserID -Income -votes -Party -Q119650 -Q122771 -Q102674 -Q116448 -Q115777 -Q115195 -Q102089 -Q122769 -Q120379 -Q106993 -Q105655 -Q116601 -Q106042 -Q100689 -Q112512 -Q108856 -Q111580 -Q117193 -Q116881 -Q99982 -Q111220 -Q122770 -Q116953 -Q113583 -Q114152 -Q101596 -Q99581 -Q99480 -Q98578 -Q118117 -Q98078 -Q105840 -Q114961 -Q118892 -Q121699 -Q118232 -Q114517 -Q113181 -Q110740 -Q108950 -Q114748 -Q118233 -Q116197 -Q115602 -Q101163 -Q109367 -Q106272 -Q96024 -Q120472 -Q119851 -Q112270 -Q111848 -Q120650 -Q120978 -Q122120 -Q112478 -Q108342 -Q113584 -Q108754 -Q103293 -Q98059 -Q100562 -YOB -EducationLevel -Q121700 -Q124742 -Q124122 -Q123464 -Q123621, data=train.happy, family=binomial)
log.hc0 <- glm(Happy ~ . -UserID, data=train.happy, family=binomial)
pred.log <- predict(log.hc, newdata=test.happy)
pred.log2 <- predict(log.hc2, newdata=test.happy)

pred.log.ROC = prediction(pred.log, test.happy$Happy)
perf.log.ROC = performance(pred.log.ROC, "tpr", "fpr") # true positive rate, false positive rate
plot(perf.ROC)
as.numeric(performance(pred.log.ROC, 'auc')@y.values)

pred.log2.ROC = prediction(pred.log2, test.happy$Happy)
perf.log2.ROC = performance(pred.log2.ROC, "tpr", "fpr") # true positive rate, false positive rate
plot(perf.ROC)
as.numeric(performance(pred.log.ROC, 'auc')@y.values)

pred.Fin.log <- predict(log.hc, newdata=testFinComplete, type='response')

submitLog <- data.frame(UserID=testFin$UserID, Probability1=pred.Fin.log)

write.table(submitLog, file="./data/submission_log.csv", row.names=F, sep=',')

summary(perf.log2.ROC)


#+++++++++++++++++++++++ CLUSTERING +++++++++++++++++++++++++++++++++


distances <- dist(happyComplete[,-c(1,107,108,109)], method='euclidean')

cluster.happy <- hclust(distances, method='ward')
plot(cluster.happy)

clustGroups.happy <- cutree(cluster.happy, k=8)

# Getting the percentage of respondents who indicated that they are happy

# Subsetting the data into each cluster
c1 = subset(happyComplete, clustGroups.happy==1)
c2 = subset(happy.data, clustGroups.happy==2)
c3 = subset(happy.data, clustGroups.happy==3)
c4 = subset(happy.data, clustGroups.happy==4)
c5 = subset(happy.data, clustGroups.happy==5)
c6 = subset(happy.data, clustGroups.happy==6)
c7 = subset(happy.data, clustGroups.happy==7)
c8 = subset(happy.data, clustGroups.happy==8)

c1.train <- subset(c1, spl == T)
c1.test <- subset(c1, spl == F)

rf.c1 <- randomForest(Happy ~ . -UserID, data=c1.train, method='class')

pred.rf.c1 <- predict(rf.c1, newdata=test.happy, type='prob')

