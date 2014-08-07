url = 'https://courses.edx.org/c4x/MITx/15.071x/asset/mvtWeek1.csv'
download.file(url=url, destfile='./edx_analytics/data/mvtWeek1.csv', method='curl')
mvt = read.csv('./edx_analytics/data/mvtWeek1.csv')
summary(mvt)
str(mvt)
max(mvt$ID)
min(mvt$Beat)
table(mvt$Arrest)
str(table(mvt$LocationDescription))
which(table(mvt$LocationDescription)=='ALLEY')
mvt$Date[1:3]
DateConvert <- as.Date(strptime(mvt$Date,'%m/%d/%y %H:%M'))
summary(DateConvert)
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
mvt$Month <- months(DateConvert)
mvt$Weekday <- weekdays(DateConvert)
mvt$Date <- DateConvert
str(mvt)
which.min(as.data.frame(table(mvt$Month))[,2]) # to find the Month with the lowest grand theft auto 
which.max(as.data.frame(table(mvt$Weekday))[,2])
table(mvt$Arrest)
which.max(as.data.frame(table(mvt[which(mvt$Arrest == T),12]))[,2]) # largest number of vehicle thefts for which an arrest was made
table(mvt$Arrest,mvt$Month) # Alternate to line 22 above 

# **** Problem 3.1: Visualizing Crime Trends ****
hist(mvt$Date, breaks=100)
boxplot(mvt$Date ~ mvt$Arrest, xlab='Arrests made?', ylab='Year')
table(mvt$Arrest)
arrst <- as.data.frame(table(mvt[which(format(mvt$Date, '%y') == '01' && mvt$Arrest == T),4]))
summary(arrst)
tempmvt = tempmvt[which(tempmvt$Arrest == True), ]
tempmvt = tempmvt[which(tempmvt$Arrest == TRUE), ]
temp = as.data.frame(table(mvt$Arrest, mvt$Year), row.names=NULL)
tp7 <- temp[which(temp$Var2 == '2012'),]
as.data.frame(sort(table(mvt$LocationDescription),decreasing=T))[10,] # To determine the locations with the most and least vehicle thefts
Top5 = c('STREET', 'PARKING LOT/GARAGE(NON.RESID.)', 'ALLEY', 'GAS STATION', 'DRIVEWAY - RESIDENTIAL')
Top5_ <- subset(mvt, LocationDescription == 'STREET' | LocationDescription == 'ALLEY' | LocationDescription == 'PARKING LOT/GARAGE(NON.RESID.)' | LocationDescription == 'GAS STATION' | LocationDescription == 'DRIVEWAY - RESIDENTIAL')
nrow(Top5_)
Top5$LocationDescription = factor(Top5$LocationDescription) # Refresh the factor variable LocationDescription by getting rid of values not in variable
str(Top5)
table(Top5$Arrest, Top5$LocationDescription)
as.data.frame(sort(table(Top5$Weekday, Top5$LocationDescription))
