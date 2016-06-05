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
#Exploratory Plots for Stationarity Etc

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
fitlin$
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
summary(garchFit(~garch(1,1), arga))
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
# Anyway we pragmatically will go to forecasting. 

u <- gfit@sigma.t
argaplusu <- arga+1.96*u
argaminusu <- arga-1.96*u
plot(arga)
lines(argaplus2u, lty= 2,col = 3)
lines(argaminus2u, lty= 2,col = 3)

x <- predict(gfit, n.ahead=6)
str(x)
x
argaforecast <- x$meanForecast
argaforecastplusu <- argaforecast + 1.96*(x$meanError)
argaforecastminusu <- argaforecast - 1.96*(x$meanError)

# voltar aos valores iniciais
# intercet no modelo fitlin fitlin$coef

argaI<- arga + fitlin$coef
argaplusuI<-argaplusu + fitlin$coef
argaminusuI<-argaminusu + fitlin$coef

argaforecastI <- argaforecast + fitlin$coef
argaforecastplusuI<- argaforecastplusu  + fitlin$coef
argaforecastminusuI<- argaforecastminusu  + fitlin$coef

gold.ts.1 <- head(gold.ts,-1)
gold.ts.arga<- (exp(argaI))*gold.ts.1
gold.ts.argaplusu<- (exp(argaplusuI))*gold.ts.1
gold.ts.argaminusu<- (exp(argaminusuI))*gold.ts.1


gold.ts.argaforecast1<- (exp(argaforecastI[1]))*gold.ts[length(gold.ts)]
gold.ts.argaforecast2<- (exp(argaforecastI[2]))*gold.ts.argaforecast1
gold.ts.argaforecast3<- (exp(argaforecastI[3]))*gold.ts.argaforecast2
gold.ts.argaforecast4<- (exp(argaforecastI[4]))*gold.ts.argaforecast3
gold.ts.argaforecast5<- (exp(argaforecastI[5]))*gold.ts.argaforecast4
gold.ts.argaforecast6<- (exp(argaforecastI[6]))*gold.ts.argaforecast5
gold.ts.argaforecast <-c(gold.ts.argaforecast1,gold.ts.argaforecast2,gold.ts.argaforecast3,gold.ts.argaforecast4,gold.ts.argaforecast5,gold.ts.argaforecast6)



gold.ts.argaforecastplusu1<- (exp(argaforecastplusuI[1]))*gold.ts[length(gold.ts)]
gold.ts.argaforecastplusu2<- (exp(argaforecastplusuI[2]))*gold.ts.argaforecastplusu1
gold.ts.argaforecastplusu3<- (exp(argaforecastplusuI[3]))*gold.ts.argaforecastplusu2
gold.ts.argaforecastplusu4<- (exp(argaforecastplusuI[4]))*gold.ts.argaforecastplusu3
gold.ts.argaforecastplusu5<- (exp(argaforecastplusuI[5]))*gold.ts.argaforecastplusu4
gold.ts.argaforecastplusu6<- (exp(argaforecastplusuI[6]))*gold.ts.argaforecastplusu5
gold.ts.argaforecastplusu <- c(gold.ts.argaforecastplusu1, gold.ts.argaforecastplusu2, gold.ts.argaforecastplusu3, gold.ts.argaforecastplusu4, gold.ts.argaforecastplusu5, gold.ts.argaforecastplusu6)



gold.ts.argaforecastminusu1<- (exp(argaforecastminusuI[1]))*gold.ts[length(gold.ts)]
gold.ts.argaforecastminusu2<- (exp(argaforecastminusuI[2]))*gold.ts.argaforecastminusu1
gold.ts.argaforecastminusu3<- (exp(argaforecastminusuI[3]))*gold.ts.argaforecastminusu2
gold.ts.argaforecastminusu4<- (exp(argaforecastminusuI[4]))*gold.ts.argaforecastminusu3
gold.ts.argaforecastminusu5<- (exp(argaforecastminusuI[5]))*gold.ts.argaforecastminusu4
gold.ts.argaforecastminusu6<- (exp(argaforecastminusuI[6]))*gold.ts.argaforecastminusu5
gold.ts.argaforecastminusu <- c(gold.ts.argaforecastminusu1, gold.ts.argaforecastminusu2, gold.ts.argaforecastminusu3, gold.ts.argaforecastminusu4, gold.ts.argaforecastminusu5, gold.ts.argaforecastminusu6)


plot(gold.ts)
lines(gold.ts.arga, lty= 2,col = 3)
lines(gold.ts.argaplusu, lty= 2,col = 4)
lines(gold.ts.argaminusu, lty= 2,col = 4)
lines(c(gold.ts, gold.ts.argaforecast), lty= 2, col = 5)




