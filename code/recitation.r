# Recitation 1 - USDA nutrition data analysis
usda.url = 'https://courses.edx.org/c4x/MITx/15.071x/asset/USDA.csv'
download.file(url=usda.url, destfile='./edx_analytics/data/USDA.csv', method='curl')
USDA = read.csv('./edx_analytics/data/USDA.csv')
str(USDA)
summary(USDA)
plot(USDA$Sodium)
USDA$Description[which.max(USDA$Sodium)] # The food with the maximum sodium content is Salt
attach(USDA)
HighSodium <- subset(USDA, Sodium > 10000) # Subset the food items with the highest sodium content
nrow(HighSodium)
HighSodium$Description
match('CAVIAR', Description) # Getting index of the row with caviar description
USDA$Sodium[match('CAVIAR', Description)] # Finding the sodium level of the food CAVIAR
summary(Sodium)
sd(Sodium, na.rm=T) # Standard deviation
sum(mean(Sodium, na.rm=T) + sd(Sodium, na.rm=T)) # This is to see how many sd's away from the min the sodium content of caviar is
plot(Protein, TotalFat, xlab='Protein', ylab='Fat', col='blue', pch=19)
plot(Protein, TotalFat, xlab='Protein', ylab='Fat', col='blue', pch=15)
plot(Protein, TotalFat, xlab='Protein', ylab='Fat', col='blue', pch=19)
hist(VitaminC, xlab='Vitamin C (mg)', main='Histogram of Vitamin C Levels', xlim=c(0, 100), breaks=100)
boxplot(Sugar,main='Boxplot of Sugar Levels', ylab='Sugar (g)')
HighSodium = Sodium > mean(Sodium, na.rm=T)
str(HighSodium)
HighSodium = as.numeric(HighSodium)
USDA$HighProtein = as.numeric(Protein > mean(Protein, na.rm=T))
USDA$HighFat = as.numeric(TotalFat > mean(TotalFat, na.rm=T))
USDA$HighCarbs = as.numeric(Carbohydrate > mean(Carbohydrate, na.rm=T))
str(USDA)
table(USDA$HighSodium)
table(USDA$HighSodium, USDA$HighFat)
tapply(USDA$Iron, USDA$HighProtein, mean, na.rm=T)#Average amount of iron sorted by high and low protein
tapply(USDA$VitaminC, USDA$HighCarbs, max, na.rm=T)
#High in carbs == high vitamin c?
tapply(USDA$VitaminC, USDA$HighCarbs, summary, na.rm=T)
# Foods with high content a