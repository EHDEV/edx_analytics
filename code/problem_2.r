rm(list=ls())
ls()
url_ibm = 'https://courses.edx.org/c4x/MITx/15.071x/asset/IBMStock.csv'
url_ge = 'https://courses.edx.org/c4x/MITx/15.071x/asset/GEStock.csv'
url_cc = 'https://courses.edx.org/c4x/MITx/15.071x/asset/CocaColaStock.csv'
url_pg = 'https://courses.edx.org/c4x/MITx/15.071x/asset/ProcterGambleStock.csv'
url_boe = 'https://courses.edx.org/c4x/MITx/15.071x/asset/BoeingStock.csv'

download.file(url=url_ibm, destfile='./data/IBMStock.csv', method='curl')
download.file(url=url_ge, destfile='./data/GEStock.csv', method='curl')
download.file(url=url_cc, destfile='./data/CocaColaStock.csv', method='curl')
download.file(url=url_pg, destfile='./data/PGStock.csv', method='curl')
download.file(url=url_boe, destfile='./data/BoeingStock.csv', method='curl')
setwd('./edx_analytics/')
ibm.data = read.csv('./data/IBMStock.csv')
ge.data = read.csv('./data/GEStock.csv')
cc.data = read.csv('./data/CocaColaStock.csv')
pg.data = read.csv('./data/PGStock.csv')
boeing.data = read.csv('./data/BoeingStock.csv')

# *** Summary Statistics ***

summary(ibm.data)
str(ibm.data)
boeing.data$Date = as.Date(ibm.data$Date, '%m/%d/%y')
sort(ibm.data$Date, decreasing=T)[1]
summary(boeing.data)
min(ge.data$StockPrice)
sd(pg.data$StockPrice)

# *** VISUALIZING STOCK DYNAMICS ***
plot(cc.data$Date, cc.data$StockPrice, type='l', col='blue')
lines(pg.data$Date, pg.data$StockPrice, col='brown')
abline(v=as.Date(c("2000-03-01")), lwd=0.5) # Draw vertical line on the plot
# This will plot the CocaCola stock prices from 1995 through 2005, which are the observations numbered from 301 to 432.
# The additional argument, ylim=c(0,210), makes the y-axis range from 0 to 210. 
plot(cc.data$Date[301:432], cc.data$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(pg.data$Date[301:432], pg.data$StockPrice[301:432], col='blue')
lines(ibm.data$Date[301:432], ibm.data$StockPrice[301:432], col='green')
lines(ge.data$Date[301:432], ge.data$StockPrice[301:432], col='purple')
lines(boeing.data$Date[301:432], boeing.data$StockPrice[301:432], col='orange')
abline(v=as.Date(c("2005-01-01")), lwd=0.5) 
abline(v=as.Date(c("1997-11-01")), lwd=0.5) 

tapply(ibm.data$StockPrice, months(ibm.data$Date), mean, na.rm=T) # Get mean stock price grouped by month
tmp = as.data.frame(as.matrix(tapply(ge.data$StockPrice, months(ge.data$Date), mean, na.rm=T)))

tmp1 = as.data.frame(tapply(cc.data$StockPrice, months(cc.data$Date), mean, na.rm=T), colnames=c('MeanStockPriceByMonth', 'Month')
mean(ibm.data$StockPrice)
tapply(ibm.data$StockPrice, months(ibm.data$Date), mean, na.rm=T)

