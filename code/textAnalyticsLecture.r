rm(list=ls())
tweets = read.csv('./data/tweets.csv', stringsAsFactors=F) # F since we are working with text data. 
str(tweets)
# 
tweets$Negative = as.factor(tweets$Avg <= -1)
table(tweets$Negative)
# TM text mining packages
# This package helps us with pre-processing the text data 
install.packages('tm')
install.packages('SnowballC')
library(tm)
library(SnowballC)
# Corlpus: a collection of documents. 
# Need to convert tweets to corpus for pre processing
getSources() # This method from the TM package returns all the accepted sources of documents
corpus = Corpus(VectorSource(tweets$Tweet)) 
# Vector source indicates to the Corpus method that the data is in vector format.
corpus[[2]]
str(corpus)
# Begin preprocessing the corpus
# Convert tweets into lower case
corpus = tm_map(corpus, tolower) # passing corpus and the function to apply to the corpus
# Remove punctuation from tweets
# Removal of punctuation should be tailored to the problem at hand. 
# Change in meaning can occur when removing punctuation and when removing stop words
corpus = tm_map(corpus, removePunctuation)
# Removing sotpwords (i, the, this, who, a, and, or, an, my, etc)
stopwords("english")[1:10]
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
# Stemming a document
corpus = tm_map(corpus, stemDocument)
# Extracting word frequencies in tweets
# words in column, tweets in row

frequencies = DocumentTermMatrix(corpus)
inspect(frequencies[1000:1005, 505:515])
# Sparse matrix - too many zeros
# What are the most popular words?
findFreqTerms(frequencies, lowfreq=100) # lowfreq minimum number of times a word must appear to be displayed in this result
# only 56 terms that appear in the frequencies 20 times or more
# We have a lot of terms that are useless for the prediction model
# more terms -> more independent variables - resulting in complicated model
# We need to remove terms that dont appear very often
sparse = removeSparseTerms(frequencies, 0.995)# sparcity threshold (0.98 - keep terms that appear in 2% or more of the tweets. 0.995 only keep terms that appear in .5% of the tweets or more)
sparse
tweetsSparse = as.data.frame(as.matrix(sparse))
str(tweetsSparse)
# make.names
# Variable names that start with a number are bad in R so we need to convert

colnames(tweetsSparse) = make.names(colnames(tweetsSparse))
# Always do this when doing text analytics
# Add dependant variable
tweetsSparse$Negative = tweets$Negative
# Split into train and test
library(caTools)
set.seed(123)
split = sample.split(tweetsSparse$Negative, 0.7)
trainSparse = subset(tweetsSparse, split == T)
testSparse = subset(tweetsSparse, split == F)

# Using CART and Logistic Regression to predict negative sentiment
library(rpart)
library(rpart.plot)
tweetCART = rpart(Negative ~ ., data = trainSparse, method='class')
prp(tweetCART)
# Evaluating performance of model
predCART = predict(tweetCART, newdata = testSparse, type='class')
table(testSparse$Negative, predCART)
acc.pred = (294 + 18)/nrow(testSparse)
acc.pred
table(test$Negative)
acc.baseline = 300/nrow(testSparse)
acc.baseline
# CART model has better accuracy

# Now let's try with random forest

library(randomForest)
set.seed(123)
tweetRF = randomForest(Negative ~ ., data=trainSparse) # Takes quite long to create model
pred.RF = predict(tweetRF, newdata=testSparse)
table(testSparse$Negative, pred.RF)
sum(293,21)/nrow(testSparse)
# RF is better but takes longer and is less interpretable than the CART model
# Cross validation to pick the best cp for the cart model would increase the model's accuracy

# Now let's use Logistic Regression to make our prediction
tweet.log = glm(Negative ~ ., data=trainSparse, family=binomial)
pred.log = predict(tweet.log, newdata=testSparse, type='response')
table(testSparse$Negative, pred.log > 0.5)
acc.pred.log = sum(253, 32)/nrow(testSparse)
table(testSparse$Negative)
acc.baseline = 300/nrow(testSparse)
acc.baseline
acc.pred.log
  