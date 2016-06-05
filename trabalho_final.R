#Trabalho final

#############################
# Load Library
library(fGarch)
library(tseries)
library(forecast)
library(fpp)
#############################
# Load data
gold <- read.csv("project_gold_gld_daily.txt",header=F, sep=",", as.is=TRUE, skip=54, col.names = c("Date", "Value"))

# Set as Time series
gold.ts0 <- ts(gold$Value)

# Remove last 6 observations
gold.ts <- head(gold.ts0,-6)
length(gold.ts0)
length(gold.ts)

##############################
Exploratory Plots for Stationarity Etc

plot.ts(gold.ts)
# The time series is not weakly stationary as the level and variance change over time
# So we cannot apply directly the Box-Jenkins methodology,
#  we will evaluate if first differences  are sufficient
# to turn the time series stationary

# First Differences
gold.dif <- diff(gold.ts)
plot.ts(gold.dif)
# Although the result can be considered adequate concerning the level stationarity,
# issues regarding variance remain. Therefore, we will begin again with a 
# log transformation and a subsequent first difference. 

# Log
gold.log <- log(gold.ts)
# First Difference of log
gold.dif.log <- diff(gold.log)
plot.ts(gold.dif.log)
# Unfortunately the log transformation did not overcome the variance issues.
# From now on we will follow a pragmatic approach: we will try to fit an ARIMA 
# model nonetheless and evaluate at the end the temporal structure of 
# the residuals

###########################
# Seasonality
# Theoretically seasonality should not be presented in financial time
# series as it would mean an arbritrage edge. 

####################
#ARIMA

tsdisplay(gold.dif.log)
Acf(gold.dif.log, plot = FALSE)
Pacf(gold.dif.log, plot = FALSE)

auto.arima(gold.log)
# ARIMA(0,1,0) with drift  
# Random walk 
Box.test(gold.dif.log, lag = 20, type = "Ljung-Box")
# p-value = 0.4783
# There is no well defined temporal structure in the transformed data(gold.dif.log)
# The auto.arima points to a random walk. The Ljung-Box test
# applied to the transformed data does not reject the 
# null, with a p-value of 0.4783. 
# Given these assertions we will fit a linear model to the transformed data
# with only the intercept in order to impose a zero mean series. Then we will
# analyse the ACF and PACF of the linear model residuals and decide for
# a GARCH case. 

fitlin <- Arima(gold.dif.log, order = c(0,0,0))
summary(fitlin)
#
#Coefficients:
#      intercept
#          4e-04 .
#s.e.      2e-04
#
# Signif. codes:  0 ?***? 0.001 ?**? 0.01 ?*? 0.05 ?.? 0.1 ? ? 1

# The intercept is statisticaly significant for a 5% type I error.
# We will try to fit a Arch/Garch as it follows
arga<-fitlin$residuals

#######################################
# Arch/ Garch

arga<-fitlin$residuals
tsdisplay(arga, lag.max = 160)
tsdisplay((arga)**2, lag.max = 160)
# From the analysis of the ACF and PACF of the squares of the linear model
# residuals (arga) we conclude that there is a case for a changing variance 
# over time. As the pattern is conducive to a ARMA model we will do a simulation
# to assist us on the fit decision. 


# As usual in financial time series the GARCH(1,1) was chosen. 
gfit<-garchFit(~garch(1,1), arga)
summary(gfit)
plot(gfit)
#-6.096633 = AIC

#         Estimate  Std. Error  t value Pr(>|t|)    
#mu     -1.808e-15   1.976e-04    0.000        1    
#omega   1.928e-06   4.891e-07    3.942 8.08e-05 ***
#alpha1  5.739e-02   7.288e-03    7.875 3.33e-15 ***
#beta1   9.310e-01   8.606e-03  108.183  < 2e-16 ***
#
# Signif. codes:  0 ?***? 0.001 ?**? 0.01 ?*? 0.05 ?.? 0.1 ? ? 1
#
#Standardised Residuals Tests:
#                                Statistic p-Value  
# Jarque-Bera Test   R    Chi^2  748.1844  0        
# Shapiro-Wilk Test  R    W      0.9771714 0        
# Ljung-Box Test     R    Q(10)  5.21801   0.8761469
# Ljung-Box Test     R    Q(15)  11.53687  0.7136916
# Ljung-Box Test     R    Q(20)  12.36139  0.9030757
# Ljung-Box Test     R^2  Q(10)  11.668    0.3078906
# Ljung-Box Test     R^2  Q(15)  16.75008  0.3340229
# Ljung-Box Test     R^2  Q(20)  20.89322  0.4034416
# LM Arch Test       R    TR^2   13.70953  0.3196408

# The tests results were good excepted for normality. 
# The fit wasn't able to recover and capture the full extreme values dynamics. 
# Anyway we pragmatically will go forcast. 

u <- gfit@sigma.t #variancia
plot(arga)
lines(arga-2*u, lty= 2,col = 3)
lines(arga+2*u, lty= 2,col = 3)


# results from the parameter estimation
head(gfit@fit)

# input data 
head(arga)
tail(arga)

# garch residuals
head(gfit@residuals)
tail(gfit@residuals)

# a numeric vector with the fitted values
head(gfit@fitted)

# a numeric vector with the conditional standard deviation.
head(gfit@sigma.t)



################################################################################
# reproducing model since the beginning
################################################################################

y2 <- diff(log(gold.ts))
fit2 <- Arima(diff(log(gold.ts)), order=c(0,0,0))
res2 <- residuals(fit2)  
tail(res2)
fitg <- garchFit(~garch(1,1), res2)
tail(fitg@residuals)

# 2 step ahead forecast
predict(fitg, n.ahead = 2)
# value of the mean forecast is the same for all periods
# we have just modeled the variance equation and the mean equation has only a constant
# garch will forecast the variance of the returns, but not the returns

# now let's get some return forecasts
# by modeling the mean equation
fitn <- fGarch::garchFit(~ arma(0,0) + garch(1,1), data =diff(log(gold.ts)) , trace =  FALSE)
predict(fitn, n.ahead = 2)
# the mean is still the same because we are using an arma(0,0)
# i.e. it's the original series

# let's try for example an arma(1,0)
fitn <- fGarch::garchFit(~ arma(1,0) + garch(1,1), data=diff(log(gold.ts)) , trace =  FALSE)
predict(fitn, n.ahead = 2)
# now we are forecasting the mean

########

