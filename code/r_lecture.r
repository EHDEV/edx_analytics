setwd("../")]

download.file(url="https://courses.edx.org/c4x/MITx/15.071x/asset/WHO.csv", destfile="./edx_analytics/WHO.csv", method="curl")
WHO.data <- read.csv("./edx_analytics/WHO.csv")
names(WHO.data)
str(WHO.data)
summary(WHO.data)
WHO.Europe <- subset(WHO.data, Region == 'Europe')
summary(WHO.Europe)
nrow(WHO.Europe)
write.csv(WHO.Europe, "./edx_analytics/WHO.Europe.csv")
ls()
rm(list="WHO.Europe")
mean(WHO.data$Under15)
summary(WHO.data$Under15)
which.min(WHO.data$Under15)
WHO.data$Country[86]
which.max(WHO.data$Under15)
WHO.data$Country[which.max(WHO.data$Under15)]
plot(WHO.data$GNI,WHO.data$FertilityRate)
outliers = subset(WHO.data, GNI > 10000 & FertilityRate > 2.5)
nrow(outliers)
outliers
outliers[c("Country", "GNI", "FertilityRate")]
attach(WHO.data)
boxplot(LifeExpectancy ~ Region)
tapply(LiteracyRate, Region, min, na.rm=T)
mean(Over60)
tapply(Over60, Country, min, na.rm=T)
WHO.data[which.min(Over60),]
WHO.data[which.max(LiteracyRate), ]
