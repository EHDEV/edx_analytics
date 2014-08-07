#******************* Data Visualization - Home Work ******************************
rm(list=ls())
#+++++++ Problem 1 ++++++++

install.packages('ggplot2')
install.packages('maps')
install.packages('ggmap')

library(ggplot2)
library(maps)
library(ggmap)

statesMap = map_data("state")

ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black") + coord_map("mercator")

polling <- read.csv('./data/PollingImputed.csv')

Train <- subset(polling, polling$Year!=2012)
Test <- subset(polling, polling$Year==2012)

mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")
TestPrediction = predict(mod2, newdata=Test, type="response")
TestPredictionBinary = as.numeric(TestPrediction > 0.5)
predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)

mean(predictionDataFrame$TestPrediction)

predictionDataFrame$region = tolower(predictionDataFrame$Test.State)
predictionMap = merge(statesMap, predictionDataFrame, by = "region")
predictionMap = predictionMap[order(predictionMap$order),]

?merge
summary(predictionMap)
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color = "black")

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")
predictionDataFrame[which(predictionDataFrame$region=='florida' | predictionDataFrame$region=='pennsylvania'),]
TestPrediction
round(9.256861e-05 , 3)

# *********** PROBLEM 2 *****************

parole <- read.csv('./data/parole.csv')

parole$male = as.factor(parole$male)

parole$state = as.factor(parole$state)

parole$crime = as.factor(parole$crime)

str(parole)
130/675
table(parole$state==2, parole$crime)

ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth=5, color="blue")
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5) + facet_grid(male ~ .)

ggplot(data = parole, aes(x = age, fill = male)) + geom_histogram(binwidth = 5)

ggplot(data = parole, aes(x = time.served)) + geom_histogram(binwidth=1, color="blue")

ggplot(data = parole, aes(x = time.served)) + geom_histogram(binwidth = 1) + facet_grid(crime ~ .)




#************ PROBLEM 3 ***********

edges <- read.csv('./data/edges.csv')
users <- read.csv('./data/users.csv')

str(users)
str(edges)
length(edges$V2)
146/42

table(users$locale)

install.packages('igraph')
library(igraph)
?graph.data.frame

g = graph.data.frame(edges, FALSE, users) 

2 * nrow(edges)/82
plot(g, vertex.size=5, vertex.label=NA)

deg.f <- degree(g)
summary(deg.f)
table(deg.f >= 10)
V(g)$size = degree(g)/2+2

plot(g, vertex.label=NA)
sort(deg.f)[1:20]
min(V(g)$size)
table(users$locale)
V(g)$color = "black"

V(g)$color[V(g)$gender == "A"] = "red"

V(g)$color[V(g)$gender == "B"] = "gray"

V(g)$color[V(g)$school == "AB"] = "red"
V(g)$color[V(g)$school == "A"] = "yellow"

V(g)$color[V(g)$locale == "A"] = "red"
V(g)$color[V(g)$locale == "B"] = "yellow"

plot(g, vertex.label=NA)

?igraph.plotting


#******************** PROBLEM 4 *****************
setwd('./Documents/workspace/edx_analytics/')
tweets <- read.csv('./data/tweets.csv',stringsAsFactors=F)

library(tm)
library(SnowballC)

corpus = Corpus(VectorSource(tweets$Tweet)) 
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, c("apple", stopwords("english")))


frequencies = DocumentTermMatrix(corpus)
inspect(frequencies[1000:1005, 505:515])

str(corpus)
allTweets <- as.data.frame(as.matrix(frequencies))
str(allTweets)
length(unique(names(allTweets)))

#install.packages('wordcloud')
#library(wordcloud)
?wordcloud
wordcloud(colnames(allTweets), colSums(allTweets), random.order=F, min.freq=2, max.words=200, rot.per=F)
?
install.packages("RColorBrewer")
library("RColorBrewer")

display.brewer.all()
wordcloud(colnames(allTweets), colSums(allTweets), random.order=F, min.freq=2, max.words=200, colors=brewer.pal(9, "Blues")[c(-1, -2, -3, -4)], rot.per=F)
