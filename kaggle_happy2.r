############ DATA MUNGING FOR THE KAGGLE COMPETITION ############
j
k <- cbind(j,rownames(j))
summary(k)
table(round(k$Age), k$HouseholdStatus)

colnames(k) = c("Avg", "HStatus")
k
idx.na = which(is.na(happy.data$YOB))
length(idx.na)
idx.na[1:4]

tmp.hp <- happy.data[idx.na, c(1, 5,111)]
str(tmp.hp)
#merge(d,m, by.x="v2", by.y="v3")
tmp.merge <- merge(tmp.hp,k,by="HouseholdStatus", all.y=T)
names(k) = c("Age", "HouseholdStatus")
n
str(tmp.merge)
rm(tmp.merge)
summary(tmp.merge)
summary(tmp.merge)

colnames(tmp.merge)[which(names(tmp.merge) == "Age.y")] <- "Age"
tmp.merge <- tmp.merge[,-which(names(tmp.merge) == "Age.x")]
names(happy.data)[111]
str(happy.data)
merge.happy <- merge(happy.data, tmp.merge, by = "UserID", all.x=T, na.rm=T)
summary(k)
k
table(tmp.merge$Age)
tmp.merge$Age = as.integer(round(tmp.merge$Age))
summary(merge.happy)

Married (w/kids)
Single (no kids)

trim <- function (x)  sub("\\s+", "", x)


nrow(happy.data[which(is.na(happy.data$Age)),c("Age", "HouseholdStatus")])

a = happy.data[which(is.na(happy.data$Age) & trim(happy.data$HouseholdStatus)==""),"Age"] = 26
nrow(a)



h3 <- na.omit(happy.data)
str(testHappy$YOB)
s <- sample.split(happy.data$Happy, SplitRatio=0.7)
h3.tn <- subset(happy.data, s == T)
h3.ts <- subset(happy.data, s == F)

log.h3 <- glm(Happy ~ .-UserID, data = h3, family = 'binomial')
tree.h3 <- rpart(Happy ~ ., data = h3.tn, method = 'class')
text(tree.h3)
pred.h3.tree <- predict(tree.h3, newdata = h3.ts)
table(h3.ts$Happy, pred.h3.tree[,2] >= 0.62)
(244+141)/nrow(h3.ts)


fitControl = trainControl(method='cv', number=10) # method='cv' use cross validation, number = 10 means use 10-fold cross validation
cartGrid = expand.grid(.cp=(1:50) * 0.01) # Picking Possible values for the parameter cp - use cp values .01 thru .5

tr3 <- train(Happy ~ ., data = h3.tn, method='rpart', trControl=fitControl, tuneGrid=cartGrid)
tr3
?cumsum

library(zoo)
h4z <- na.locf(h2, , na.rm=F)
str(testHappy)
testHappy <- na.locf(na.locf(testHappy, , na.rm=F), fromLast=T, na.rm=F)
happy.data <- na.locf(na.locf(happy.data, , na.rm=F), fromLast=T, na.rm=F)

str(happy.data)
h4z$UserID[1111:1116]
n <- names(testHappy)
for (i in 1:109){
  print(i)
  if(n[i] != 'votes' | n[i] != 'YOB')
    testHappy[,i] = as.factor(testHappy[, i])
}
n
summary(testHappy)
happy.data$votes = as.integer(happy.data$votes)
happy.data$YOB = as.integer(happy.data$YOB)

testHappy$votes = as.integer(testHappy$votes)
testHappy$YOB = as.integer(testHappy$YOB)

which(is.na(h4z))
if (names(h4z)[111] == 'UserID')
  print("hello")
h4z[nrow(h4z),1]

rm(list=ls())
ls()
str(h4z)
which(is.na(h4z$YOB))
