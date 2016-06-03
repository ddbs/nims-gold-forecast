source("load.R")

library(fGarch)
library(tseries)


# If the volatility clustering is properly explained by the model, then there 
# will be no autocorrelation in the squared standardized residuals.  
# It is common to do a Ljung-Box test to test for this autocorrelation.

## sizing-up potential heteroscedasticity
##ACF/PACF/Box-Test - valid for variances?
squares <- diff(gold.ts)^2
tsdisplay(squares)
Box.test(squares, 20, 'Ljung-Box')

# model with no ARMA process, and ARCH(1)
garch.fit.1 <- garchFit(formula=~garch(2,0), data=gold.trans, trace=FALSE)  
summary(garch.fit.1)
plot(garch.fit.1)

# model with no ARMA process, and ARCH(2)
garch.fit.2 <- garchFit(formula=~garch(2,0), data=gold.trans, trace=FALSE)  
summary(garch.fit.2)
plot(garch.fit.2)

#model with no ARMA process, and GARCH(1,1)
garch.fit.3 <- garchFit(formula=~garch(1,1), data=gold.trans, trace=FALSE)
summary(garch.fit.3)
plot(garch.fit.3)

# model with an ARMA(1,1) process, and GARCH(1,1)
garch.fit.4 <- garchFit(formula=~arma(1,1)+garch(1,0), data=gold.trans, trace=FALSE)  
summary(garch.fit.4)
plot(garch.fit.4)

## to print specific garch plots:
## plot(garch.fit, which=3)

## how to predict with garch?
## TODO

## alpha1 + alpha2 >= 1 means we have exponential decay?
# http://www.r-bloggers.com/a-practical-introduction-to-garch-modeling/


