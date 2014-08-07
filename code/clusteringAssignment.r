rm(list=ls())
setwd('../')
dk.data = read.csv('./data/dailykos.csv')
str(dk.data)
getwd()
# hclust
dk.data$Document = NULL
dist.h = dist(dk.data, method='euclidean')
clust.dk = hclust(dist.h,method="ward")
plot(clust.dk)
dkClusters = cutree(clust.dk, k=7)
table(dkClusters)
length(which(dkClusters==3))
clust1 = subset(dk.data, dkClusters==1) 
clust2 = subset(dk.data, dkClusters==2) 
clust3 = subset(dk.data, dkClusters==3) 
str(clust3)
clust4 = subset(dk.data, dkClusters==4) 
clust5 = subset(dk.data, dkClusters==5) 
clust6 = subset(dk.data, dkClusters==6) 
clust7 = subset(dk.data, dkClusters==7) 
str(clust3)
str(clust1)
HierCluster = split(dk.data, dkClusters)
str(HierCluster[[3]])

tail(sort(colMeans(clust1[-1])))
tail(sort(colMeans(clust2[-1])))
tail(sort(colMeans(clust3[-1])))
tail(sort(colMeans(clust4[-1])))
tail(sort(colMeans(clust5[-1])))
tail(sort(colMeans(clust6[-1])))
tail(sort(colMeans(clust7[-1])))

set.seed(1000)
KMC = kmeans(dk.data, 7)
summary(KMC)
str(KMC)
str(KMC$size)

kclust1 = subset(dk.data, KMC$cluster==1) 
kclust2 = subset(dk.data, KMC$cluster==2) 
kclust3 = subset(dk.data, KMC$cluster==3) 
str(clust3)
kclust4 = subset(dk.data, KMC$cluster==4) 
kclust5 = subset(dk.data, KMC$cluster==5) 
kclust6 = subset(dk.data, KMC$cluster==6) 
kclust7 = subset(dk.data, KMC$cluster==7) 

tail(sort(colMeans(kclust7[-1])))

table(KMC$cluster, dkClusters)

# ******* PROBLEM 2: MARKET SEGMENTATION FOR AIRLINES ********

rm(list=ls())

airlines <- read.csv('./data/AirlinesCluster.csv')
str(airlines)
summary(airlines)

# Let's go ahead and normalize our data. You can normalize the variables in a data frame by using the preProcess function in the "caret" package.
# You should already have this package installed from Week 4, but if not, go ahead and install it with install.packages("caret"). 
# Then load the package with library(caret).
library(caret)
# caret is short for... classification and regression training, 
#   contains functions to streamline the model training process for complex regression and classification problems.
# Preprocess airlines data normalize features with large or small values. Normalization, (X - mean(X))/stdev(X)
preproc = preProcess(airlines)
str(preproc)
airlinesNorm = predict(preproc, airlines)
summary(airlinesNorm)

dist.euc.air = dist(airlinesNorm, method="euclidean")
air.hclust = hclust(dist.euc.air, method='ward')
plot(air.hclust)

air.cuTree = cutree(air.hclust, k=5)
str(air.cuTree)
table(air.cuTree)

tapply(airlines$Balance, air.cuTree, mean)
tapply(airlines$QualMiles, air.cuTree, mean)
tapply(airlines$BonusMiles, air.cuTree, mean)
tapply(airlines$BonusTrans, air.cuTree, mean)
tapply(airlines$FlightMiles, air.cuTree, mean)
tapply(airlines$DaysSinceEnroll, air.cuTree, mean)

set.seed(88)

kClust <- kmeans(airlinesNorm, 5,iter.max=1000)
str(kClust)
table(kClust$cluster)

tapply(airlines$Balance, kClust$cluster, mean)
tapply(airlines$QualMiles, kClust$cluster, mean)
tapply(airlines$BonusMiles, kClust$cluster, mean)
tapply(airlines$BonusTrans, kClust$cluster, mean)
tapply(airlines$FlightMiles, kClust$cluster, mean)
tapply(airlines$FlightTrans, kClust$cluster, mean)
tapply(airlines$DaysSinceEnroll, kClust$cluster, mean)

# ***** Problem 3 - PREDICTING MEDICAL COSTS WITH CLUSTER-THEN-PREDICT *****

rm(list=ls())
claims <- read.csv('./data/reimbursement.csv')
str(claims)

numC = rowSums(claims[,2:12])
mean(numC>0)
cor(claims)

hist(claims$reimbursement2009)
claims$reimbursement2008 = log(claims$reimbursement2008 + 1)
claims$reimbursement2009 = log(claims$reimbursement2009 + 1)
hist(claims$reimbursement2009)

# Linear regression

set.seed(144)
spl = sample(1:nrow(claims), size=0.7*nrow(claims))
train.re = claims[spl,]
test.re = claims[-spl,]
str(train.re)
str(test.re)
lm.re <- lm(reimbursement2009 ~ ., data = train.re)
summary(lm.re)

pred.tr.lm = predict(lm.re, newdata=test.re)
sse = sum((pred.tr.lm - test.re$reimbursement2009) ^ 2)
sqrt(sse/nrow(test.re))
sum((mean(train.re$reimbursement2009) - test.re$reimbursement2009) ^ 2)/nrow(test.re)




train.limited = train.re

train.limited$reimbursement2009 = NULL

test.limited = test.re

test.limited$reimbursement2009 = NULL

library(caret)

preproc = preProcess(train.limited)
str(preproc)
train.norm = predict(preproc, train.limited)
summary(train.norm)
test.norm = predict(preproc, test.limited)
mean(test.norm$arthritis)

set.seed(144)

km = kmeans(train.norm, 3)
km$centers
str(km )
tapply(train.re$age, km$cluster, mean)
tapply(train.re$stroke, km$cluster, mean)
tapply(train.re$reimbursement2008, km$cluster, mean)
tapply(train.norm$age, km$cluster, mean)
str(train.norm)

library(flexclust)

km.kcca = as.kcca(km, train.norm)

cluster.train = predict(km.kcca)

cluster.test = predict(km.kcca, newdata=test.norm)

str(cluster.test)
table(cluster.test)

# Prediction
train1 = subset(train.re, cluster.train == 1)
train2 = subset(train.re, cluster.train == 2)
train3 = subset(train.re, cluster.train == 3)

test1 = subset(test.re, cluster.test == 1)
test2 = subset(test.re, cluster.test == 2)
test3 = subset(test.re, cluster.test == 3)

summary(train3$reimbursement2009)
lm1 = lm(reimbursement2009 ~ ., data=train1)
lm2 = lm(reimbursement2009 ~ ., data=train2)
lm3 = lm(reimbursement2009 ~ ., data=train3)
summary(lm1)
summary(lm2)

pred.test1 = predict(lm1, newdata=test1)
pred.test2 = predict(lm2, newdata=test2)
pred.test3 = predict(lm3, newdata=test3)
mean(pred.test1)
mean(pred.test2)
mean(pred.test3)

sse1 = sum((pred.test1 - test1$reimbursement2009) ^ 2)
sse2 = sum((pred.test2 - test2$reimbursement2009) ^ 2)
sse3 = sum((pred.test3 - test3$reimbursement2009) ^ 2)

rmse1 = sqrt(sse1)
rmse2 = sqrt(sse2)
rmse3 = sqrt(sse3)

all.predictions = c(pred.test1, pred.test2, pred.test3)

all.outcomes = c(test1$reimbursement2009, test2$reimbursement2009, test3$reimbursement2009)
sse.all = sum((all.predictions - all.outcomes) ^ 2)
rmse.all = sqrt(sse.all/nrow(all.predictions))
rmse.all
sqrt(mean((all.predictions - all.outcomes)^2))
