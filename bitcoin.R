library(fGarch)
library(tseries)
library(forecast)
library(fpp)
library(xts)

bitcoin <- read.csv("bitcoin.txt", 
                 header=T, sep=",", as.is=TRUE, col.names = c("Date", "Value"))

# set gold as a time series object
bit <- ts(bitcoin$Value)

plot(bit)
acf(bit)
pacf(bit)

dbit <- 