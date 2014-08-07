rm(list=ls())
ls()
url.pew = 'https://courses.edx.org/c4x/MITx/15.071x/asset/AnonymityPoll.csv'
download.file(url=url.pew, destfile='./data/AnonymityPoll.csv', method='curl')
priv.poll <- read.csv('./data/AnonymityPoll.csv')
summary(priv.poll)
str(priv.poll)
summary(priv.poll$Smartphone)
table(priv.poll$Smartphone)

table(priv.poll$State, priv.poll$Region)
table(priv.poll$Internet.Use, priv.poll$Smartphone)
summary(priv.poll$Smartphone)
limited.poll <- subset(priv.poll, Internet.Use == T | Smartphone == T)
str(limited.poll)
summary(limited.poll)
table(limited.poll$Info.On.Internet)
table(limited.poll$Anonymity.Possible)
table(limited.poll$Worry.About.Info)
# Proportion of those who responded to the question and indeed have tried masking their identity
table(limited.poll$Tried.Masking.Identity)[2]/sum(table(limited.poll$Tried.Masking.Identity))
table(limited.poll$Privacy.Laws.Effective)[2]/sum(table(limited.poll$Privacy.Laws.Effective))
hist(limited.poll$Age, breaks=20)
tmp2 <- as.data.frame(table(limited.poll$Age, limited.poll$Info.On.Internet))
#jitter adds or subtracts a small amount of random noise to the values passed to it
#each jitter call generates different results
plot(jitter(limited.poll$Age), jitter(limited.poll$Info.On.Internet))
tapply(limited.poll$Info.On.Internet, limited.poll$Smartphone, mean, na.rm=T)
tapply(limited.poll$Tried.Masking.Identity, limited.poll$Smartphone, mean, na.rm=T)
