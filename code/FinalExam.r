# ************************* FINAL EXAM ******************************
rm(list=ls())
elantra = read.csv('./data/elantra.csv')
str(elantra)

library(caTools)

split = sample.split(elantra$ElantraSales, SplitRatio=0.7)

e.train = subset(elantra, elantra$Year <= 2012)
e.test = subset(elantra, elantra$Year >2012)

str(e.train)
str(e.test)

lm1 = lm(ElantraSales ~ Unemployment + CPI_all + CPI_energy + Queries, data=e.train)
summary(lm2)

# Adding Month to the model
lm2 = lm(ElantraSales ~ Month + Unemployment + CPI_all + CPI_energy + Queries, data=e.train)
summary(lm1)

e.train$Month = as.factor(e.train$Month)
e.test$Month = as.factor(e.test$Month)

# With Month as factor let's create another model

lm3 = lm(ElantraSales ~ Month + Unemployment + CPI_all + CPI_energy + Queries, data=e.train)
summary(lm3)

unique(elantra$Month)

# As we have seen before, changes in coefficient signs and signs that are counter to our intuition may be due to a multicolinearity problem. 
# To check, compute the correlations of the variables in the training set.
# Convert month back to numeric to be able to do a correlation on it

e.train$Month = as.numeric(as.character(e.train$Month))

cor(e.train)

lm0 = lm(ElantraSales ~ Month + Unemployment + CPI_all + CPI_energy, data=e.train)
summary(lm0)

pred0 = predict(lm0, newdata=e.test)

sse = sum((pred0 - e.test$ElantraSales)^2)
temp = abs(pred0 - e.test$ElantraSales)

sst = sum((mean(e.train$ElantraSales) - e.test$ElantraSales)^2)
rmse = 1-(sse/sst)
rmse
pred0[1:5]
e.test$ElantraSales[1:5]
mean(e.train$ElantraSales)

sqrt(sse)

#***************** PROBLEM 2 ***************

nytimes = read.csv('./data/nytimes.csv')

str(nytimes )
table(nytimes$popular)
105/nrow(nytimes)
cor(nchar(as.character(nytimes$headline)), nytimes$popular)
# nchar - number of chars in a string

set.seed(144)

split = sample.split(nytimes$popular, SplitRatio=0.7)

nyt.train = subset(nytimes, split==T)
nyt.test = subset(nytimes, split==F)

lgr1 = glm(popular ~ print + type + word.count, data = nyt.train, family=binomial)
summary(lgr1)

pred1 = predict(lgr1, newdata=nyt.test, type="response")
table(nyt.test$popular)

library(help=ROCR)

pred.rocr = prediction(pred1, nyt.test$popular)
perf = performance(pred.rocr, "tpr", "fpr", colrize = T) # true positive rate, false positive rate
plot(perf, colorize = T)
as.numeric(performance(pred.rocr, 'auc')@y.values)

set.seed(144)

#Cross validation

library(caret)
library(rpart)
library(rpart.plot)

fitControl = trainControl(method='cv', number=10)
cartGrid = expand.grid(.cp=(1:50) * 0.01)

train(popular ~ print + type + word.count, data=nyt.train, method='rpart', trControl=fitControl, tuneGrid=cartGrid) 

tree1 = rpart(popular ~ print + type + word.count, data=nyt.train, control=rpart.control(cp=0.01))
tree1$parms
plot(tree1)

library(tm)
library(SnowballC)

corpus = Corpus(VectorSource(as.character(nytimes$snippet)))

corpus[[1]]

corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords,stopwords("en"))
corpus = tm_map(corpus, stemDocument)

dtm = DocumentTermMatrix(corpus)
str(dtm)
dtm$ncol

spdtm = removeSparseTerms(dtm, 0.95)
str(spdtm)

articleText = as.data.frame(as.matrix(spdtm))
str(articleText)
a = colSums(articleText)
which.max(a)
articleText$print = nytimes$print
articleText$type = nytimes$type
articleText$word.count = nytimes$word.count
articleText$popular = nytimes$popular

trainText = subset(articleText, split==T)
testText = subset(articleText, split==F)
str(testText)

glmText = glm(popular ~ ., data = trainText, family=binomial)
summary(glmText)

predText = predict(glmText, newdata=testText, type='response')

predrocr = prediction(predText, testText$popular)
perf = performance(predrocr, "tpr", "fpr")
plot(perf, colorize=T)
as.numeric(performance(predrocr, "auc")@y.values)

#********************* PROBLEM 3 - Clustering ************************

rm(list=ls())

stocks = read.csv('./data/nasdaq_returns.csv')
str(stocks)

table(stocks$industry)
table(stocks$ret2000.12 <= -.1)
tapply(stocks$ret2008.10, stocks$industry, mean)
tapply(stocks$ret2000.02, stocks$industry, mean)

limited = stocks
limited$stock_symbol = NULL
limited[1,c(1,2)]
sort(colMeans(limited), decreasing=T)

distStock = dist(limited, method='euclidean')
clustStock = hclust(distStock, method='ward')
plot(clustStock)

clustGrp = cutree(clustStock, k=5)
str(clustGrp)
table(clustGrp)

c1 = subset(stocks, clustGrp == 1)
c2 = subset(stocks, clustGrp == 2)
c3 = subset(stocks, clustGrp == 3)
c4 = subset(stocks, clustGrp == 4)
c5 = subset(stocks, clustGrp == 5)

table(c1$industry=='Healthcare' | c1$industry=='Technology')
table(c2$industry=='Healthcare' | c2$industry=='Technology')
table(c3$industry=='Healthcare' | c3$industry=='Technology')
table(c4$industry=='Healthcare' | c4$industry=='Technology')
table(c5$industry=='Healthcare' | c5$industry=='Technology')

table(c1$industry)
table(c2$industry)
table(c3$industry)
table(c4$industry)
table(c5$industry)

table(stocks$subindustry == "Electronics Wholesale")

table(c1$subindustry=="Electronics Wholesale")
table(c2$subindustry=="Electronics Wholesale")
table(c3$subindustry=="Electronics Wholesale")
table(c4$subindustry=="Electronics Wholesale")
table(c5$supindustry)

data.frame(table(stocks$industry))

stocks$cluster = clustGrp
mean(c1$ret2000.03)
mean(c2$ret2000.03)
mean(c3$ret2000.03)
mean(c4$ret2000.03)
mean(c5$ret2000.03)

library(ggplot2)

bxp1 = ggplot(stocks,aes(ret2000.02))
bxp1 + geom_boxplot()

set.seed(144)

kms = kmeans(limited, 5)
table(kms$cluster)
str(kms$cluster)
temp = subset(stocks, kms$cluster == 4)
table(stocks$stock_symbol == 'AMZN',stocks$cluster)
stocks$kms = kms$cluster
