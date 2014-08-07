who = read.csv('./data/WHO.csv')
str(who)
library(ggplot2)

scatterplot = ggplot(who, aes(x=GNI, y=FertilityRate))

scatterplot + geom_point(color = "blue", size = 3, shape=17) 
scatterplot + geom_point(color = "darkred", size = 3, shape=15) 

fertilityGNIplot = scatterplot + geom_point(color = "blue", size = 3, shape=19)+ ggtitle("Fertility Rate + Gross National Income")

#saving the plot to a pdf
pdf("./data/myWHOplot.pdf")
print(fertilityGNIplot)

dev.off()

scatterplot2 = ggplot(who, aes(x=GNI, y=FertilityRate, color=Region))
scatterplot2 + geom_point()

who[which.max(who$GNI), ]

# Life expectancy
scatterplot2 = ggplot(who, aes(x=GNI, y=FertilityRate, color=LifeExpectancy, shape=Region))
scatterplot2 + geom_point(size = 4)

# Fertility rate of a country a good predictor of percentage of population under 15?

ggplot(who, aes(x=FertilityRate, y=Under15)) + geom_point()

# Let's try using the log(Fertility Rate) instead of FertilityRate

ggplot(who, aes(x=log(FertilityRate), y=Under15)) + geom_point() # relationship is now linear

# Let's create a linear model 

lm1 = lm(Under15 ~ log(FertilityRate), data=who)
summary(lm1)

# add regression line to the plot
#  stat_smooth(method="lm")
# levels = 0.99 - give 99% confidence interval
# se = F - Take away confidence interval. Just show the regression line without the confidence interval
ggplot(who, aes(x=log(FertilityRate), y=Under15)) + geom_point() + stat_smooth(method="lm", se=F, color = 'orange')
# Image shows that the relationship is not linear, rate of change decreases as fertility rate increases

ggplot(who, aes(x=log(FertilityRate), y=Under15, color=Region)) + geom_point() # relationship is now linear

# ****** VISUALIZATION - LECTURE 2 *******

# basic line plots to visualize crime trends
mvt = read.csv('./data/mvt.csv', stringsAsFactors=F)
str(mvt)

# Convert the date to a format that R recognizes

mvt$Date = strptime(mvt$Date, format='%m/%d/%y %H:%M') # conversion to Date
mvt$Weekday = weekdays(mvt$Date)
mvt$Hour = mvt$Date$hour
str(mvt)

# One line plot with weekday and the number of crimes for that day

weekdayCounts = as.data.frame(table(mvt$Weekday))
str(weekdayCounts)

library(ggplot2)

ggplot(weekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1)) # group = 1 groups all the data into one line instead of multiple lines

# We need to convert the Var1 variable to an ordered factor

weekdayCounts$Var1 = factor(weekdayCounts$Var1, ordered=T, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

ggplot(weekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1)) # group = 1 groups all the data into one line instead of multiple lines

# now we need to change the x and y labels to make more sense

ggplot(weekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1)) + xlab("Day of the week") + ylab("Total Motor Vehicle Thefts") # group = 1 groups all the data into one line instead of multiple lines

ggplot(weekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1), linetype=2)  # linetype allows us to select the line type (dashed, thicker, thin, etc lines)

ggplot(weekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1), alpha=0.3)

# Creating line for each day of week and make x axis hour of day

DayHourCounts = as.data.frame(table(mvt$Weekday, mvt$Hour))

str(DayHourCounts)

# convert var2 to a number

DayHourCounts$Hour = as.numeric(as.character(DayHourCounts$Var2))

# Group = day of week

ggplot(DayHourCounts, aes(x=Hour, y=Freq)) + geom_line(aes(group=Var1, color=Var1, size=2))

# Heatmap

DayHourCounts$Var1 = factor(DayHourCounts$Var1, ordered=T, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

ggplot(DayHourCounts, aes(x=Hour, y=Var1)) + geom_tile(aes(fill=Freq))

# Changing the legend title and removing the y axis label. Change the color scheme as well
ggplot(DayHourCounts, aes(x=Hour, y=Var1)) + geom_tile(aes(fill=Freq)) + scale_fill_gradient(name="Total MV Thefts", low="white", high="red") + theme(axis.title.y=element_blank())

# ******* PLOTTING CRIME IN THE CITY OF CHICAGO ************

library(maps)
library(ggmap)

chicago = get_map(location="Chicago", zoom = 11)

?get_map

# Displaying only the first 100 MV thefts so that we are able to see the points on the map
ggmap(chicago) + geom_point(data=mvt[1:100, ], aes(x=Longitude, y=Latitude))

# Round Lat and Long to two digit create crime counts data frame for each area in chicago

LatLongCounts = as.data.frame(table(round(mvt$Longitude, 2), round(mvt$Latitude, 2)))

#total crimes at every point on a grid

str(LatLongCounts)
# convert lat and long to numbers

LatLongCounts$Long = as.numeric(as.character(LatLongCounts$Var1))
LatLongCounts$Lat = as.numeric(as.character(LatLongCounts$Var2))

ggmap(chicago) + geom_point(data=LatLongCounts, aes(x=Long, y=Lat, color=Freq, size=Freq))

# change the color scheme
ggmap(chicago) + geom_point(data=LatLongCounts, aes(x=Long, y=Lat, color=Freq, size=Freq)) + scale_color_gradient(low="yellow", high="red")

# Plot heatmap instead of points

ggmap(chicago) + geom_tile(data=LatLongCounts, aes(x=Long, y=Lat, alpha=Freq, fill="red"))

# Removing the plots outside of the chicago map (on the water)

LatLongCount2 = subset(LatLongCounts, LatLongCounts$Freq > 0)
str(LatLongCounts)
ggmap(chicago) + geom_tile(data = LatLongCount2, aes(x=Long, y=Lat, alpha=Freq, fill="red"))


#************** ANALYZING MURDERS IN THE USA ***************

murders = read.csv('./data/murders.csv')
str(murders)

# Load US map

statesMap = map_data("state")
str(statesMap)

ggplot(statesMap, aes(x=long, y=lat, group=group)) + geom_polygon(fill = 'white', color='brown') + coord_map("mercator")
str(murders)
str(statesMap)

# State Names are in different capitalization between the statesMap and murders data frames

murders$region = tolower(murders$State)
# Now we can join the statesmap df with the murders df using the common column

murderMap = merge(statesMap, murders, by="region")

str(murderMap)

# Data is merged

# plotting the number of murders in the us

ggplot(murderMap, aes(x=long, y=lat, group=group, fill=Murders)) + geom_polygon(color='darkblue') + scale_fill_gradient(low='black', high='red', guide='legend') 

# population
ggplot(murderMap, aes(x=long, y=lat, group=group, fill=Population)) + geom_polygon(color='darkblue') + scale_fill_gradient(low='black', high='red', guide='legend') 

# the maps look the same so we need to plot using murder rate rather than number of murders

murderMap$MurderRate = murderMap$Murders/murderMap$Population * 100000

# Plotting with Murder Rate
ggplot(murderMap, aes(x=long, y=lat, group=group, fill=MurderRate)) + geom_polygon(color='darkblue') + scale_fill_gradient(low='black', high='red', guide='legend') 

# Maroon looking plot with no actual red states. This is due to an outlier region (washington dc with high murder rate)
#
ggplot(murderMap, aes(x=long, y=lat, group=group, fill=MurderRate)) + geom_polygon(color='darkblue') + scale_fill_gradient(low='black', high='red', guide='legend', limits=c(0.9,10)) 

ggplot(murderMap, aes(x=long, y=lat, group=group, fill=GunOwnership)) + geom_polygon(color='darkblue') + scale_fill_gradient(low='black', high='red', guide='legend') 

