library(fGarch)
library(tseries)
library(forecast)
library(fpp)

gold <- read.csv("project_gold_gld_daily.txt",header=F, sep=",", as.is=TRUE, skip=54, col.names = c("Date", "Value"))
gold.ts0 <- ts(gold$Value)
gold.ts <- head(gold.ts0,-6)


#EXPONENTIAL SMOOTHING FORECAST

#Em baixo tentámos modelar a nossa séries e fazer previsões recorrendo ao alisamento exponencial.
#Logo à partida, decidimos não utilizar o método de alisamento exponencial simples, uma vez que a nossa série é caracterizada por ter tendência e não sazonalidade.
#Os métodos de alisamento mais indicados serão portanto o Holt's Linear Trend, Holt's Exponential, Damped Linear Trend e,  Damped Exponential Trend.

fit1<-holt(gold.ts, h=6)
fit2<-holt(gold.ts, initial='optimal', exponential=TRUE, h=6)
fit3<-holt(gold.ts, initial='optimal', damped=TRUE, h=6)
fit4<-holt(gold.ts, initial='optimal', exponential=TRUE, damped=TRUE, h=6)

#AIC Results
AIC(fit1$model)
AIC(fit2$model)
AIC(fit3$model)
AIC(fit4$model)

par(mfrow=c(2,2))
plot(tail(gold.ts,100), main= paste("Holt's Linear"," AIC=", round(AIC(fit1$model),1)), 
     xlab="", ylab = "Prices", col="black", type="l")
lines(tail(fitted(fit1),100), col="cyan")
plot(tail(gold.ts,100), main= paste("Holt's Exponential"," AIC=", round(AIC(fit2$model),1)), 
     xlab="", ylab = "Prices", col="black", type="l")
lines(tail(fitted(fit2),100), col="blue")
plot(tail(gold.ts,100), main= paste("Damped Trend"," AIC=", round(AIC(fit3$model),1)), 
     xlab="", ylab = "Prices", col="black", type="l")
lines(tail(fitted(fit3),100), col="green")
plot(tail(gold.ts,100), main= paste("Exponential Damped Trend"," AIC=", round(AIC(fit2$model),1)), 
     xlab="", ylab = "Prices", col="black", type="l")
lines(tail(fitted(fit4),100), col="red")


# plot forecasts
par(mfrow=c(1,1))
plot(fit1$mean, col="cyan", xlab="", ylab="")
par(new=T)
plot(fit2$mean, col="blue", xlab="", ylab="", axes=F)
par(new=T)
plot(fit3$mean, col="green", xlab="", ylab="", axes=F)
par(new=T)
plot(fit4$mean, col="red", xlab="", ylab="", axes=F)
legend("topleft", lty=1, col=c("cyan","blue","green","red"),
       c("Holt's Linear", "Holt's Exponential","Damped Trend","Exponential Damped Trend"), cex = .7)


#GRAPHICS (seria melhor se focasse o periodo de 2015,5 a 2016,5 - n consegui)
plot(tail(gold.ts,100), ylab="Gold Prices (corrigir titulo)", xlab="Month", col="black", type="l")
lines(tail(fitted(fit1),100), col="cyan")
lines(tail(fitted(fit2),2), col="red")
lines(tail(fitted(fit3),2), col="green")
lines(tail(fitted(fit4),2), col="pink")
legend("topleft", lty=1, col=c("black","blue","red","green","pink"),
       c("Series", "Holt's Linear", "Holt's Exponential","Damped Trend","Exponential Damp. Trend"))


lines(fit1$mean, col="blue")
lines(fit2$mean, col="red")
lines(fit3$mean, col="green")
lines(fit4$mean, col="pink")

#Forecasts (para calcular Erro de previsão dos modelos)

fit1$mean
fit2$mean
fit3$mean
fit4$mean


#Segundo o AIC o melhor modelo para a nossa série é o Holt's Exponential.
#Este modelo é mais adequado para séries com tendência e sem sazonalidade como é o caso da nossa.
#Resta comprovar se o erro de previsão do modelo Holt's Exponential é menor do que o do modelo Damped Exponential Trend cujo AIC foi o segundo mais baixo.


#####





