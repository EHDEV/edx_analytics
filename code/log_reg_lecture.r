quality = read.csv('./data/quality.csv')
str(quality)
table(quality$PoorCare)
# Simple baseline method - in LR, avg outcome for all data points
# In classification, predict the most frequent outcome for all data points
setwd('./Documents/workspace/edx_analytics/')
98/131

#baseline is 75% accuracy

install.packages('caTools')
library(caTools)
# this library helps us to randomly split our data into training and test sets
set.seed(88)
split = sample.split(quality$PoorCare, SplitRatio=0.75)
# TRUE - observations in training set and FALSE - observations in test
# the split makes sure that the outcome is equally represented in both test and training sets

str(split)
qTrain = subset(quality, split == T)
qTest = subset(quality, split == F)
nrow(qTest)
qLog = glm(PoorCare ~ OfficeVisits + Narcotics, data=qTrain, family='binomial')
# 
summary(qLog)
# AIC is a measure of the quality of the model
# provides a means for model selection
# prefer model with the minimum AIC
pred.Train = predict(qLog, type='response') # return probabilities
summary(pred.Train)
tapply(pred.Train, qTrain$PoorCare, mean) # Average prediction for each of the true outcomes
# Log reg predicts the probability of the outcome variable being true (1).
# Here, we are predicting the probability of poor care being provided instead of good care. Thus p(poor Care) == 0 - 1.
# p(y == 1) for poor care
# p(y==0) :=> 1 - p(y==1) for good care
# P(y==1) = 1/1 + e ^ (B0 + B1 * X1 + Bk * Xk)

# Odds = p(y==1)/p(y==0)
# *** Finger Ex ***

qlog2 = glm(PoorCare ~ StartedOnCombination + ProviderCount, data=qTrain, family=binomial)
summary(qlog2)

# Threshold: 
# Confusion (Classification) Matrix

# TN: 
# TP:
# FP: predict outcome is true but actually false
# FN: predict outcome is false but actually true

# Sensitivity = TP/(TP + FN) - percentage of actual positive cases. TP rate
# Specificity = TN/(TN + FP) - percentage of actual negative cases.

# High threshold = low sensitivity and high specificity
# Low threshold = low specificity and high sensitivity
# ROC curve
install.packages("ROCR")
library(ROCR)
ls()
# plotting the ROCR curve 
# Using the predictions we made to create an ROC Curve
rocrPred = prediction(pred.Train, qTrain$PoorCare)
rocrPerf = performance(rocrPred, "tpr", "fpr")
summary(rocrPerf)
str(rocrPerf)
plot(rocrPerf)

predictTest = predict(qLog, type="response", newdata=qTest)
ROCRpredTest = prediction(predictTest, qTest$PoorCare)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc

# *** Heart Study *** 
framingham = read.csv('./data/framingham.csv')
str(framingham)
# We have the following risk factors in the data
#   - Demographic
#   - Behavioral
#   - Medical History
#   - Physical Exam 

ls()
library(caTools)
set.seed(1000)
split = sample.split(framingham$TenYearCHD, SplitRatio=0.65)
train.fr = subset(framingham, split=T)
test.fr = subset(framingham, split=F)
fr.log = glm(TenYearCHD ~ ., data=train.fr, family=binomial)
summary(fr.log)
cor(train.fr)

pred.fr.test = predict(fr.log, newdata=test.fr, type='response')
table(test.fr$TenYearCHD, pred.fr.test > 0.5)
# Accuracy ...
(3082 + 51)/nrow(test.fr)
# Baseline Accuracy ..
(3082 + 19)/nrow(test.fr)
# The model barely beats the baseline model.. Let's compare with ROC Curve
# Computing the out of sample AUC

library(ROCR)
pred.rocr = prediction(pred.fr.test, test.fr$TenYearCHD)
# AUC value for our test set
as.numeric(performance(pred.rocr, "auc")@y.values)
# The model can differenciate between low risk and high risk patients
# with a 74 % of out of sample AUC
# ++++ External Validation ++++ 
# Testing model on different populations
