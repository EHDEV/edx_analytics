# ******* Text Analytics - Assignment ********

wiki = read.csv('./data/wiki.csv', stringsAsFactors=F)
str(wiki)
wiki$Vandal = as.factor(wiki$Vandal)
table(wiki$Vandal)
library(tm)
library(SnowballC)
corpusAdded = Corpus(VectorSource(wiki$Added))
corpusAdded[[1]]
corpusAdded = tm_map(corpusAdded, removePunctuation)
corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded = tm_map(corpusAdded, stemDocument)

dtmAdded = DocumentTermMatrix(corpusAdded)
str(dtmAdded)
dtmAdded
sparseAdded = removeSparseTerms(dtmAdded, sparse=.997)
sparseAdded
wordsAdded = as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))

corpusRemoved <- Corpus(VectorSource(wiki$Removed))
corpusRemoved <- tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved <- tm_map(corpusRemoved, stemDocument)
dtmRemoved <- DocumentTermMatrix(corpusRemoved)
sparseRemoved <- removeSparseTerms(dtmRemoved, sparse=.997)
wordsRemoved <- as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))
str(wordsRemoved)

set.seed(123)
wikiWords <- cbind(wordsAdded, wordsRemoved)
wikiWords$Vandal <- wiki$Vandal
str(wikiWords)
library(caTools)
spl = sample.split(wikiWords, SplitRatio=0.7)
train.wiki = subset(wikiWords, spl == T)
test.wiki = subset(wikiWords, spl == F)


str(train.wiki)
str(test.wiki)
table(test.wiki$Vandal)
library(rpart)
tree.wiki = rpart(Vandal ~ ., data=train.wiki)
pred.tree.wiki = predict(tree.wiki, newdata=test.wiki, type='class')
pred.tree.wiki[1:5]
table(test.wiki$Vandal, pred.tree.wiki)
prp(tree.wiki)
tmp = predict(tree.wiki, type='class')
table(train.wiki$Vandal, tmp)
wikiWords2 = wikiWords

# Adding a new column to the data frame
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
table(wikiWords2$HTTP)
wikiTrain2 = subset(wikiWords2, spl==TRUE)

wikiTest2 = subset(wikiWords2, spl==FALSE)
tree2.wiki = rpart(Vandal ~ ., data=wikiTrain2)
prp(tree2.wiki)
pred2.tree.wiki <- predict(tree2.wiki, newdata=wikiTest2, type='class')
table(wikiTest2$Vandal, pred2.tree.wiki)

# ****** Problem 2 - AUTOMATING REVIEWS IN MEDICINE *******
rm(list=ls())
trials <- read.csv('./data/clinical_trial.csv', stringsAsFactors=F)
length(which(nchar(trials$abstract) == 0))
trials$title[1258]
library(tm)
library(SnowballC)
corpusTitle <- Corpus(VectorSource(trials$title))
corpusAbstract <- Corpus(VectorSource(trials$abstract))
corpusTitle[[1]]
corpusAbstract[[2]]

corpusTitle <- tm_map(corpusTitle, tolower)
corpusAbstract <- tm_map(corpusAbstract, tolower)
corpusTitle <- tm_map(corpusTitle, removePunctuation)
corpusAbstract <- tm_map(corpusAbstract, removePunctuation)
corpusTitle <- tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract <- tm_map(corpusAbstract, removeWords, stopwords("english"))
corpusTitle <- tm_map(corpusTitle, stemDocument)
corpusAbstract <- tm_map(corpusAbstract, stemDocument)

dtmTitle <- DocumentTermMatrix(corpusTitle)
dtmAbstract <- DocumentTermMatrix(corpusAbstract)
dtmTitle[1,]
sparseTitle <- removeSparseTerms(dtmTitle, .95)
sparseTitle
sparseAbstract <- removeSparseTerms(dtmAbstract, .95)
summary(dfTitle)
dfTitle <- as.data.frame(as.matrix(sparseTitle))
dfAbstract <- as.data.frame(as.matrix(sparseAbstract))
length(dfTitle)
length(dfAbstract)
sort(colSums(dfAbstract), decreasing=T)
colnames(dfTitle) = paste0("T", colnames(dfTitle))
colnames(dfAbstract) = paste0("A", colnames(dfAbstract))
str(trials)
dtm <- cbind(dfTitle, dfAbstract) 
dtm$trial <- trials$trial
str(dtm)

library(caTools)
set.seed(144)
split = sample.split(dtm$trial, SplitRatio=0.7)
train.dtm <- subset(dtm, split==T)
test.dtm <- subset(dtm, split==F)
table(train.dtm$trial)
730/nrow(train.dtm)

library('rpart')
trialCART <- rpart(trial ~ ., data = train.dtm)
library(rpart.plot)
prp(trialCART)
str(dtm$trial)
predCART <- predict(trialCART)
max(predCART)
dim(predCART)
train.dtm$trial = as.factor(train.dtm$trial)
test.dtm$trial = as.factor(test.dtm$trial)
table(train.dtm$trial, predCART >= 0.5)

predCART2 <- predict(trialCART, newdata=test.dtm)
head(predCART2)
table(test.dtm$trial, predCART2 > 0.5)

library(ROCR)

ROCR.pred <- prediction(predCART2, test.dtm$trial)
ROCR.perf <- performance(ROCR.pred, "tpr", "fpr")
as.numeric(performance(ROCR.pred, "auc")@y.values
)

# ****** Problem 3 - SEPARATING SPAM FROM HAM (PART 1) ******

emails <- read.csv('./data/emails.csv', stringsAsFactors = F)
str(emails)
which.min(nchar(emails$text))

corpus = Corpus(VectorSource(emails$text))
corpus[[1]]

corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("en"))
corpus <- tm_map(corpus, stemDocument)

dtm <- DocumentTermMatrix(corpus)
dtm
spdtm <- removeSparseTerms(dtm, .95)
emailsSparse <- as.data.frame(as.matrix(spdtm))
str(emailsSparse)
sort(colSums(emailsSparse), decreasing=T)
emailsSparse$spam <- emails$spam
s = colSums(emailsSparse)
str(s)
summary(s)
table(emailsSparse)
length(which(colSums(subset(emailsSparse, spam == 0))>=1000))

emailsSparse$spam = as.factor(emailsSparse$spam)
set.seed(123)
spl = sample.split(emailsSparse$spam, 0.7)
train.emails <- subset(emailsSparse, spl==T)
test.emails <- subset(emailsSparse, spl==F)
colnames(emailsSparse)[colnames(emailsSparse)=="853"] <- "a853"
spamLog <- glm(spam ~ ., data=train.emails, family=binomial)
spamCART <- rpart(spam ~ ., data=train.emails, method='class')
library(randomForest)
spamRF <- randomForest(spam ~ ., data=train.emails)
str(emailsSparse$spam)
