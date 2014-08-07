#**** Problem 1 - UNDERSTANDING WHY PEOPLE VOTE ****

rm(list=ls())

gerber.data = read.csv('./data/gerber.csv')
attach(gerber.data)
str(gerber.data)
table(gerber.data$voting)
# Which of the four "treatment groups" had the largest fraction of voters?
ht=table(hawthorne, voting)
ct=table(civicduty, voting)
st=table(self, voting)
nt=table(neighbors, voting)
ht[2,2]/sum(ht[2,])
ct[2,2]/sum(ct[2,])
st[2,2]/sum(st[2,])
nt[2,2]/sum(nt[2,])

#There are several ways to get this answer. One is to use the tapply function, and compute the mean value of "voting", sorted by whether or not the people were in each group:
#tapply(gerber$voting, gerber$civicduty, mean)
#tapply(gerber$voting, gerber$hawthorne, mean)
#tapply(gerber$voting, gerber$self, mean)
#tapply(gerber$voting, gerber$neighbors, mean)
#The variable with the largest value in the "1" column has the largest fraction of people voting in their group - this is the Neighbors group
str(gerber.data)
gerb.log = glm(voting ~ hawthorne + civicduty + neighbors + self, data=gerber.data, family=binomial)
summary(gerb.log)
pred.log = predict(gerb.log, type='response')
table(gerber.data$voting, pred.log > 0.36)

# ROC Curve
library(ROCR)
ROCRPred.log = prediction(pred.log, gerber.data$voting)
# Performance - define what to plot in the x and y axis of the ROC curve
ROCRPerf.log = performance(ROCRPred.log, "tpr", "fpr")
plot(ROCRPerf.log, colorize=T, print.cutoffs.at=seq(0,1,0.02),text.adj=c(-0.2, 1.7))

# building a tree to explore the fraction of people who vote, or the probability of voting.
# We’d like CART to split our groups if they have different probabilities of voting. If we used method=‘class’, CART would only split if one of the groups had a probability of voting above 50% and the other had a probability of voting less than 50% (since the predicted outcomes would be different). However, with regression trees, CART will split even if both groups have probability less than 50%.
gerb.tree = rpart(voting ~ hawthorne + civicduty + neighbors + self, data=gerber.data)
summary(gerb.tree)
plot(gerb.tree)
text(gerb.tree)
prp(gerb.tree)
# create another tree with cp value modified to 0.0
gerb.tree2 = rpart(voting ~ hawthorne + civicduty + neighbors + self, data=gerber.data, cp=0.0)
plot(gerb.tree2)
text(gerb.tree2)
prp(gerb.tree2)
# Make a new tree that includes the "sex" variable, again with cp = 0.0. Notice that sex appears as a split that is of secondary importance to the treatment group.
gerb.tree3 = rpart(voting ~ hawthorne + civicduty + neighbors + self + sex, data=gerber.data, cp=0.0)
plot(gerb.tree3)
text(gerb.tree3)
prp(gerb.tree3)

#Let's just focus on the "Control" treatment group. Create a regression tree using just the "control" variable, then create another tree with the "control" and "sex" variables, both with cp=0.0.

#In the "control" only tree, what is the absolute value of the difference in the predicted probability of voting between being in the control group versus being in a different group? 
#You can use the absolute value function to get answer, i.e. abs(Control Prediction - Non-Control Prediction). Add the argument "digits = 6" to the prp command to get a more accurate estimate.
gerb.tree4 = rpart(voting ~ control, data=gerber.data, cp=0.0)
gerb.tree5 = rpart(voting ~ control + sex, data=gerber.data, cp=0.0)
controlPrediction = predict(gerb.tree4)
prp(gerb.tree5, digits=5)
abs(.33418 - .345818)
gerb.log2 = glm(voting ~ control + sex, family=binomial)
summary(gerb.log2)

# The regression tree calculated the percentage voting exactly for every one of the four possibilities (Man, Not Control), (Man, Control), (Woman, Not Control), (Woman, Control). 
#Logistic regression has attempted to do the same, although it wasn't able to do as well because it can't consider exactly the joint possibility of being a women and in the control group.
# We can quantify this precisely. 
#Create the following dataframe (this contains all of the possible values of sex and control), and evaluate your logistic regression using the predict function (where "LogModelSex" is the name of your logistic regression model that uses both control and sex):

Possibilities = data.frame(sex=c(0,0,1,1), control=c(0,1,0,1))
predict(gerb.log2, newdata=Possibilities, type='response')
# The four values in the results correspond to the four possibilities in the order they are stated above ( (Man, Not Control), (Man, Control), (Woman, Not Control), (Woman, Control) ). 
# What is the absolute difference between the tree and the logistic regression for the (Woman, Control) case? 
abs(.30279 - 0.29061)
gerb.log3 = glm(voting ~ sex + control + sex:control, data=gerber.data, family=binomial)
summary(gerb.log3)
predict(gerb.log3, newdata=Possibilities, type='response')
#

# ***** Problem 2 - LETTER RECOGNITION *****

rm(list=ls())
letters.data = read.csv('./data/letters_ABPR.csv')
letters.data$isB = as.factor(letters.data$letter == "B")
library('caTools')
set.seed(1000)
split = sample.split(letters.data$isB, SplitRatio=0.5)
train.letters = subset(letters.data, split==T)
test.letters = subset(letters.data, split==F)
table(test.letters$isB)
1175/nrow(test.letters)
tree.letters = rpart(isB ~ .-letter, data=train.letters, method='class')
prp(tree.letters)
pred.letters = predict(tree.letters, newdata=test.letters, type='class')
head(pred.letters)
table(pred.letters, test.letters$isB)
(1094+361)/nrow(test.letters)
library(randomForest )
rf.letters = randomForest(isB ~ .-letter, data=train.letters)
pred.rf.letters = predict(rf.letters, newdata=test.letters, type='class')
table(pred.rf.letters, test.letters$isB)
(1167 + 367)/nrow(test.letters)

letters.data$letter = as.factor(letters.data$letter)
set.seed(2000)
split = sample.split(letters.data$letter, SplitRatio=0.5)
train.letters = subset(letters.data, split==T)
test.letters = subset(letters.data, split == F)
table(test.letters$letter)
tree2.letters = rpart(letter ~ . -isB, data = train.letters, method='class')
pred.tr1.letters = predict(tree2.letters, newdata=test.letters, type='class')
head(pred.tr1.letters)
ac = table(pred.tr1.letters, test.letters$letter)
(348 + 318 + 363 + 340)/nrow(test.letters)
set.seed(1000)
rf2.letters = randomForest(letter ~ . -isB, data=train.letters)
pred.rf2.letters = predict(rf2.letters, newdata=test.letters, type='class')
table(pred.rf2.letters, test.letters$letter)

# ***** Problem Set 3 - STATE DATA REVISITED ***** 

data(state)
statedata = data.frame(state.x77)
str(statedata)
?state
lm.state = lm(Life.Exp ~ ., data = statedata)
summary(lm.state)
pred.lm.state = predict(lm.state)
lm.sse = sum((pred.lm.state - statedata$Life.Exp) ^ 2)
lm.sse
lm2.state = lm(Life.Exp ~ Population + Murder + Frost + HS.Grad, data=statedata)
summary(lm2.state)
pred2.lm.state = predict(lm2.state)
lm2.sse = sum((pred2.lm.state - statedata$Life.Exp) ^ 2)
lm2.sse
cor(statedata)
tree.state = rpart(Life.Exp ~ ., data=statedata, minbucket = 5)
prp(tree.state)
pred.tree.state = predict(tree.state)
head(pred.tree.state)

sse.tree = sum((pred.tree.state - statedata$Life.Exp) ^ 2)
sse.tree
# Create a tree that predicts Life.Exp using only Area, with the minbucket parameter to 1. 
# What is the SSE of this newest tree?
tree2.state = rpart(Life.Exp ~ Area, data=statedata, minbucket=1)
prp(tree2.state)
pred2.tree.state = predict(tree2.state)
sse.tree2 = sum((pred2.tree.state - statedata$Life.Exp) ^ 2)
sse.tree2
str(statedata)
lm3.state = lm(Life.Exp ~ Area, data=statedata)
summary(lm3.state)

#***** Cross Validation *****

library(caret)
library(e1071)

set.seed(111)
fitControl = trainControl(method='cv', number=10) 
# method='cv' use cross validation, number = 10 means use 10-fold cross validation
cartGrid = expand.grid(.cp=(1:50) * 0.01) 
# Picking Possible values for the parameter cp - use cp values .01 thru .5
train(Life.Exp ~ ., data=statedata, method='rpart', trControl=fitControl, tuneGrid=cartGrid) 
tree3.state = rpart(Life.Exp ~ ., data=statedata, control=rpart.control(cp=0.12))
prp(tree2.state)
pred3.tree.state = predict(tree3.state)
sse.tree3 = sum((pred3.tree.state - statedata$Life.Exp) ^ 2)
sse.tree3
set.seed(111)
fitControl = trainControl(method='cv', number=10)
cartGrid = expand.grid(.cp=(1:50) * 0.01)
train(Life.Exp ~ Area, data=statedata, method='rpart', trControl=fitControl, tuneGrid=cartGrid)
summary(statedata)
# At the end of Part 2 we made a very complex tree using just Area. 
#Use train with the same parameters as before but just using Area as an independent variable to find the best cp value (set the seed to 111 first). 
#Then build a new tree using just Area and this value of cp.
tree4.state = rpart(Life.Exp ~ Area, data=statedata, control=rpart.control(cp=0.02))
prp(tree4.state, digits=10)
pred4.tree.state = predict(tree3.state)
sse.tree4 = sum((pred4.tree.state - statedata$Life.Exp) ^ 2)
sse.tree4

# ****** PREDICTING EARNINGS FROM CENSUS DATA ******

rm(list=ls())
census.data = read.csv('./data/census.csv')
str(census.data)

library('caret')
library(caTools)
library(e1071)
set.seed(2000)
split = sample.split(census.data$over50k, SplitRatio=0.6)
head(split)
tr.census = subset(census.data, split==T)
te.census = subset(census.data, split==F)
str(tr.census)
str(te.census)
log.census = glm(over50k ~ ., data=tr.census, family='binomial')
summary(log.census)
table(census.data$over50k)
pred.log = predict(log.census, newdata=te.census, type='response')
head(pred.log)
table(te.census$over50k, pred.log > 0.5)
table(te.census$over50k)

# Area under the curve (AUC) of the test set
library(ROCR)

rocr.pred.log = prediction(pred.log, te.census$over50k)
rocrPerf = performance(rocr.pred.log, "tpr", "fpr")
plot(perf)
par(mfrow=c(1,1))
auc = as.numeric(performance(rocr.pred.log, "auc")@y.values)
auc

# Let us now build a classification tree for this model. 
library(rpart)
tree.census = rpart(over50k ~ ., data=tr.census, method='class')
prp(tree.census)
pred.tree = predict(tree.census, newdata=te.census)
table(te.census$over50k, pred.tree)
# Accuracy
(9243 + 1596)/nrow(te.census)
# Plot the ROC curve for the CART model you have estimated. 
# Observe that compared to the logistic regression ROC curve, the CART ROC curve is less smooth than the logistic regression ROC curve.
roc.pred.tree = prediction(pred.tree[,2], te.census$over50k)
perf2 = performance(roc.pred.tree, "tpr", "fpr") # true positive rate, false positive rate
plot(perf2)
as.numeric(performance(roc.pred.tree, 'auc')@y.values)

auc = as.numeric(performance(roc.pred.tree, "auc")@y.values)
auc
set.seed(1)

tr.small.census = tr.census[sample(nrow(tr.census), 2000), ]
library(randomForest)
rf.census = randomForest(over50k ~ . -nativecountry, data=tr.small.census)
pred.rf = predict(rf.census, newdata=te.census)
table(te.census$over50k, pred.rf)
str(tr.small.census)
(8867 + 2048)/nrow(te.census)

vu = varUsed(rf.census, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)

dotchart(vusorted$x, names(rf.census$forest$xlevels[vusorted$ix]))
varImpPlot(rf.census)
set.seed(2)
# Cross Validation
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))
fitControl2 = trainControl(method='cv', number=10)
train(over50k ~ ., data=tr.census, method = 'rpart', trControl=fitControl2, tuneGrid=cartGrid)
tree2.census = rpart(over50k ~ ., data=tr.census, method='class', control=rpart.control(cp=0.002))
pred.tree2 = predict(tree2.census, newdata=te.census, type='class')
table(te.census$over50k, pred.tree2)
(9178+1838)/nrow(te.census)
prp(tree2.census)
prp(tree.census)
