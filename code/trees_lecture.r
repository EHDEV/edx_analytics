stevens = read.csv('./data/stevens.csv')
str(stevens)
setwd('./Documents/workspace/edx_analytics/')
# Reverse is the dependant variable
# Split data into train and test
# load library caTools

library(caTools)
set.seed(3000)
split = sample.split(stevens$Reverse, SplitRatio=0.7)
s.train = subset(stevens, split == T)
s.test = subset(stevens, split == F)

#install.packages('rpart')
library(rpart)
#install.packages('rpart.plot')
library(rpart.plot)
# Tree function equivalent to lm or glm is rpart()
# Method='class' is to tell rpart that this is classification and not regression
# control=rpart.control(minbucket=25) - is to tell rpart that minimum number of observations in a bucket is 25
stevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=s.train, method='class', control=rpart.control(minbucket=25))
# plotting tree
prp(stevensTree)
table(stevens$Respondent)
# Predicting
pred.CART = predict(stevensTree, newdata=s.test, type='class')
# Checking accuracy of predictions with a threshold of 0.5
table(s.test$Reverse, pred.CART)
(41 +71)/nrow(s.test)
# Baseline Accuracy
# model beats baseline
91/nrow(s.test)
# Generating ROC Curve for the Cart model
library(ROCR)
pred.ROC = predict(stevensTree, newdata=s.test)
head(pred.ROC)
# Looking at the predict data we see two columns
# We use second column for thresholding
pred = prediction(pred.ROC[,2], s.test$Reverse)
perf = performance(pred, "tpr", "fpr") # true positive rate, false positive rate
plot(perf)
as.numeric(performance(pred, 'auc')@y.values)

#Another Tree with a large minbucket value that results in smaller number of splits
astevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=s.train, method='class', control=rpart.control(minbucket=100))
prp(astevensTre e)

#### Random Forest ####
#install.packages('randomForest')
library(randomForest)
rfNews()
set.seed(100)
?randomForest
stevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=s.train, nodesize=25, ntree=200)
# Here we get a warning: In randomForest.default(m, y, ...) :
#The response has five or fewer unique values.  Are you sure you want to do regression?
# We need to make sure that the outcome variable is of a factor type
s.test$Reverse = as.factor(s.test$Reverse) 
s.train$Reverse = as.factor(s.train$Reverse)# Converting dependant variable to a factor
# create the model again with a factor response variable
stevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=s.train, nodesize=25, ntree=200)
# No more warning
pred.Forest = predict(stevensForest, newdata=s.test)
res = table(s.test$Reverse, pred.Forest)
# Accuracy of the model =>
s.acc = (res[1,1] + res[2,2])/sum(sum(res))

# Let's compare this with Logistic Regression

stevens.glm = glm(Reverse ~ Circuit + Respondent + LowerCourt, data=s.train, family=binomial)
pred1.glm = predict(stevens.glm, newdata=s.test, type='response')
res.2 = table(s.test$Reverse, pred1.glm > 0.5)
res.2
acc.2 = (res.2[1,1] + res.2[2,2])/sum(sum(res.2))
acc.2
summary(stevens.glm)
#**** Cross Validation *****

# How do we select the best value for the minbucket parameter?
# We use k-fold cross validation to determine the optimum minbucket value
# Recall that cross validation works by splitting validation data into k different sets
# Then we train using the k-1 splits and test on the remaining one split. We use each
# of the folds as a cross-validation set and measure the accuracy of our model each time
# We select the model with the highest accuracy rate

# In cross-validation, we use the complexity parameter cp. This is like AIC or R^2 in Logistic Regression and Linear Regression
# Small cp leads to bigger tree (overfitting) and high cp leads to a simple/small tree - high variance

#install.packages('caret')
library(caret) # cross validation package
#install.packages('e1071') # cross validation
library(e1071) # Cross validation package

# define cross validation experiment
fitControl = trainControl(method='cv', number=10) # method='cv' use cross validation, number = 10 means use 10-fold cross validation
cartGrid = expand.grid(.cp=(1:50) * 0.01) # Picking Possible values for the parameter cp - use cp values .01 thru .5

# Performing Cross validation
# method = 'rpart' - validate parameters for a cart tree, trControl= output of trainControl, tuneGrid= the output of cartGrid 
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=s.train, method='rpart', trControl=fitControl, tuneGrid=cartGrid) 
# The final value used was for the model was cp = 0.19 as resulted from the function above

str(cartGrid)
?rpart
stevensTreeCV = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=s.train, method='class', control=rpart.control(cp=0.19))
predictCV = predict(stevensTreeCV, newdata=s.test, type='class')
table(s.test$Reverse, predictCV)
# Accuracy of the new tree model
(59 + 64)/nrow(s.test)
plot(stevensTreeCV)
prp(stevensTreeCV)

#******** Housing Data in Boston Recitation *******
boston = read.csv('./data/boston.csv')
str(boston)
plot(boston$LON, boston$LAT) 
# The plot of latitude and longitude result in a rough map of Boston
# # Show all points that lie on charles river in diff color
points(boston$LON[boston$CHAS == 1], boston$LAT[boston$CHAS ==1], col="blue", pch=19)
# Show MIT in the plot
points(boston$LON[boston$TRACT == 3531], boston$LAT[boston$TRACT ==3531], col="red", pch=19)
# Exploring air polution in the data using the NOX variable
summary(boston$NOX)

# Let's map areas with above average (>=mean(NOX) air polution 
points(boston$LON[boston$NOX >= 0.55], boston$LAT[boston$NOX >=0.55], col="green", pch=19)

# Let's see how prices vary 
plot(boston$LON, boston$LAT) 

summary(boston$MEDV)

# Plotting housing prices above average
# Some patterns exist but are not very obvious in the plot.
points(boston$LON[boston$MEDV >= 21.2], boston$LAT[boston$MEDV >=21.2], col="red", pch=19)

# Can we tell if there is a linear relationship between lat/long and housing prices?
# Let's do exploratory analysis
plot(boston$LAT, boston$MEDV)
plot(boston$LON, boston$MEDV)

# the plots show that the linearity between housing price and lat/long is low
# Let's plot a linear regression

latlong.lm = lm(MEDV ~ LAT + LON, data=boston)
summary(latlong.lm)

# Plotting housing prices above average
# This is to show the linear reg line and see what lm think is above average
points(boston$LON[boston$MEDV >= 21.2], boston$LAT[boston$MEDV >=21.2], col="red", pch=19)
points(boston$LON[latlong.lm$fitted.values >=21.2], boston$LAT[latlong.lm$fitted.values >=21.2], col="green", pch='$')

# the linear reg looks wrong as suspected. We can see on the map that the right hand values have been ignored by the model

# Let's try regression trees
library(rpart)
library(rpart.plot)
latlonTree = rpart(MEDV ~ LAT + LON, data=boston)
prp(latlonTree)

plot(boston$LON, boston$LAT) 
# Plotting housing prices above average
points(boston$LON[boston$MEDV >= 21.2], boston$LAT[boston$MEDV >=21.2], col="red", pch=19)
# predict what the tree thinks is median
fittedValues = predict(latlonTree)
points(boston$LON[fittedValues >=21.2], boston$LAT[fittedValues >=21.2], col="blue", pch='$')
latlonTree = rpart(MEDV ~ LAT + LON, data=boston, minbucket=50)
prp(latlonTree)
abline(v=-71.07)
abline(h=42.17)
str(boston)
# Prediction time 
# Load CaTools to split data to train and test
split = sample.split(boston$MEDV, SplitRatio=0.7)
b.train = subset(boston, split==T)
b.test = subset(boston, split==F)

b.lm = lm(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO , data = b.train)
#b.lm = lm(MEDV ~ .-TRACT, data = b.train)
summary(b.lm)
# Let's calc sum of squared errors
pred.lm = predict(b.lm, newdata=b.test)
sse.lm = sum((pred.lm - b.test$MEDV) ^ 2)
# tree
b.tree = rpart(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO , data = b.train)
prp(b.tree)
pred.tree = predict(b.tree, newdata=b.test)
sse.tree = sum((pred.tree - b.test$MEDV) ^ 2)
sse.tree
sse.lm
# This tells us that regression trees are not better than linear regression for prediction in this case
# RSS for the tree is higher than for lm. lm is better
# Can we have better models if we use cross validation?
# Let's see 
# cp (complexity parameter) - is lambda/RSS

library(caret)
library(e1071)

tr.control = trainControl(method='cv', number=10)
# cp value choices to try
cp.grid = expand.grid(.cp=(0:10) * 0.001)
cp.grid
tr = train(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO , data = b.train, method='rpart', trControl=tr.control, tuneGrid=cp.grid)
best.tree = tr$finalModel
prp(best.tree)
pred.best.tree = predict(best.tree, newdata=b.test)
sse.best.tree = sum((pred.best.tree - b.test$MEDV) ^ 2)
sse.best.tree
sse.tree
sse.lm
# We can see here that the tree model has improved when selected using cross validation. 
# However, we can see that linear regression has a lower sse than the best tree model.
# So, linear reg is better at predicting quantitative values in this case