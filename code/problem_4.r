rm(list=ls())
emp.url <- 'https://courses.edx.org/c4x/MITx/15.071x/asset/CPSData.csv'
download.file(emp.url, destfile='./data/CPSData.csv', method='curl')
emp.data <- read.csv('./data/CPSData.csv')
str(emp.data)
which.max(table(emp.data$Industry))
sort(table(emp.data$State))
table(emp.data$Citizenship)
unique(emp.data$Citizenship)
length(emp.data[emp.data$Citizenship != 3, 1])
summary(emp.data)

# Ethnicity and Race - How many races have hispanic ethnicity in the data?

table(emp.data$Race, emp.data$Hispanic)
table(emp.data$Region, is.na(emp.data$Married))
table(emp.data$Sex, is.na(emp.data$Married))
table(emp.data$Age, is.na(emp.data$Married))
table(emp.data$Citizenship, is.na(emp.data$Married))
table(emp.data$State, is.na(emp.data$MetroAreaCode))
tapply(is.na(emp.data$MetroAreaCode), emp.data$State, mean, na.rm=T) 
url.dict <- 'https://courses.edx.org/c4x/MITx/15.071x/asset/MetroAreaCodes.csv'
url.dict2 <- 'https://courses.edx.org/c4x/MITx/15.071x/asset/CountryCodes.csv'
download.file(url.dict, destfile='./data/MetroAreaCodes.csv', method='curl')
download.file(url.dict2, destfile='./data/CountryCodes.csv', method='curl')
metro.dict <- read.csv('./data/MetroAreaCodes.csv')
country.dict <- read.csv('./data/CountryCodes.csv')
str(country.dict)
str(emp.data)
str(metro.dict)
MetroAreaMap = metro.dict
CountryMap = country.dict
length(which(MetroAreaMap$Code == emp.data$MetroAreaCode))
emp.data = merge(emp.data, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
summary(emp.data)
str(emp.data$MetroArea)
sort(table(emp.data$MetroArea), decreasing=T)[1:10]
sort(tapply(emp.data$Hispanic, emp.data$MetroArea, mean, na.rm=T))
sort(tapply(emp.data$Race=='Asian', emp.data$MetroArea, mean, na.rm=T))
length(which(table(emp.data$MetroArea, emp.data$Race == 'Asian') >= .2))
sort(tapply(emp.data$Education=='No high school diploma', emp.data$MetroArea, mean, na.rm = T), decreasing=T)
emp.data <- merge(emp.data, CountryMap, by.x="CountryOfBirthCode", by.y='Code', all.x=T)
str(emp.data)
unique(emp.data$Citizenship)
summary(emp.data)
sort(table(emp.data$Country))
tapply(emp.data$Country != 'United States', emp.data$MetroArea=='New York-Northern New Jersey-Long Island, NY-NJ-PA', mean, na.rm=T)
sort(table(emp.data$MetroArea, emp.data$Country == 'Ethiopia')[,2])
