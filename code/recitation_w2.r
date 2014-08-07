nba.url = 'https://courses.edx.org/c4x/MITx/15.071x/asset/NBA_test.csv'
download.file(nba.url, destfile='./data/NBA_test.csv', method='curl')
ls()
rm(list=ls())
nba.test = read.csv('./data/NBA_test.csv')

# SeasonEnd: Year season ended
# Playoffs - if team made it to playoffs or not
# PTS - points scored during regular season
# oppPTS - points scored by opponent in regular season
# FG - Number of field goal that succeeeded
# FGA - Number of field goal that were attempted
# X3P/X3PA - three pointers scored/attempted
# X2P/X2PA - two pointers scored/attempted
# ORB/DRB - Offensive/Defensive Rebounds
# STL - Steals
# AST - Assists
# BLK - Blocks
# TOV - Turnovers

table(nba.train$W, nba.train$Playoffs)

nba.train$PTSdiff = nba.train$PTS - nba.train$oppPTS
str(nba.train)
str(nba.test)
plot(nba.train$PTSdiff, nba.train$W)

winsReg <- lm(W ~ PTSdiff, data=nba.train)
summary(winsReg)
# R Squared Manually:
1 - sum(winsReg$residuals ^ 2) / sum((nba.train$W - mean(nba.train$W)) ^ 2)
# to guarantee getting into playoffs we saw that 42 wins are necessary
# To get 42 wins, how many points diff is required. Use the linear
# equasion:=> 41 + .03259 * PTSdiff = 42 :=> PTSdiff = (42 - 41)/0.03259 = 30.68426
# A team needs to have a PTSdiff of 31 to win 42 games.

pointsReg <- lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + TOV + STL + BLK, data=nba.train)
summary(pointsReg)

# There is indeed a linear r/nship between points and these statistics
#
SSE = sum(pointsReg$residuals ^ 2)
# RMSE to better interpret the residuals
RMSE = sqrt(SSE/nrow(nba.train))
RMSE/mean(nba.train$PTS) # 2% error...
mean(nba.train$PTS)
# Lets remove one by one, insignificant variables from the model
pointsReg2 <- lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + STL + BLK, data=nba.train)
summary(pointsReg4)
pointsReg3 <- lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL + BLK, data=nba.train)
pointsReg4 <- lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL, data=nba.train)
# By removing these variables, we were able to simplify the model
# without reducing R squared
summary(pointsReg4)
SSE4 = sum(pointsReg4$residuals ^ 2)
RMSE4 = sqrt(SSE4/nrow(nba.train))
RMSE4
pointsPrediction = predict(pointsReg4, newdata=nba.test)
# Calculate the out of sample r squared - the r squared on test data
SSE_T = sum((pointsPrediction - nba.test$PTS) ^ 2)
nrow(pointsPrediction)
SST_T = sum((mean(nba.test$PTS) - nba.test$PTS) ^ 2)
R2 = 1 - (SSE_T/SST_T)
R2
RMSE = sqrt(SSE_T/nrow(nba.test))
RMSE
