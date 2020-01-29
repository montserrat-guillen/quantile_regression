#===============================================================
# Chapter 5: Time series quantile regression
##===============================================================
## clear all existing objects
rm(list=ls(all=TRUE))

## set working directory
# setwd("introduce here your working directory") 

setwd("~/Desktop/Libro/Estimaciones/") 
##=========================
## data 
##=========================

load("PJM.RData")

data=data[,2:3]
t=dim(data)[1]
n=dim(data)[2]
price=matrix(,t,n)
for (i in 1:n)
{price_=as.numeric(data[,i])
price[,i]=price_}
power=price[,1]
gas=price[,2]
colnames(price) <-c("power", "gas")

## Define data as to explain power prices using gas lags 
DATA = cbind(power, gas)

#Variable Definition
Y=power[1:(t-5)]
X=gas[1:(t-5)]
X1=gas[2:(t-4)]
X2=gas[3:(t-3)]
X3=gas[4:(t-2)]
X4=gas[5:(t-1)]
X5=gas[6:t]
Y1=power[2:(t-4)]
Y2=power[3:(t-3)]
Y3=power[4:(t-2)]
Y4=power[5:(t-1)]
Y5=power[6:t]

# Figure 7 ##################################################################
# Quantiles for the Price of Electricity (US$) EIA ##########################

x <- Y
n <- length(x)
plot((1:n - 1)/(n - 1), sort(x), type="l",
     xlab = "Fraction of data",
     ylab = "Quantiles of the price of electricity (US$)")

#Figure 8 ##################################################################
# OLS regression of electricity prices natural gas prices ###################

#OLS Regression
olsreg <- lm(Y~X)
summary(olsreg)

plot(Y ~ X, cex=0.1, xlab = "Natural gas prices ($)", ylab = 
       "Electricity prices ($)")
abline(coef(olsreg)[1:2])
cf <- round(coef(olsreg), 3) 
eq <- paste0("electricity= ", cf[1],
             ifelse(sign(cf[2])==1, " + ", " - "), abs(cf[2]), "gas")
mtext(eq, 3, line=-2)

mean(Y)
sqrt(var(Y))
mean(X)
sqrt(var(X))

summary(X)
summary(Y)

#Figure 9 and Table 6 ########################################################
# OLS and quantile regression coefficients at different quantiles ############
#### The following command calls the package ???quantreg??? available in R to perform the estimations and plot the figure.

library(quantreg)


# Quantile Regression
Electricity=Y
Gas=X
quantreg <- rq(Electricity~Gas, tau=seq(0.1,0.9,0.1))
summary(quantreg)
quantreg.plot=summary(quantreg)
plot(quantreg.plot, main=c("Intercept","Natural gas prices"))

### Figure 10 ################################################################
### Quantile regressions for the Electricity prices as a function of the natural gas prices ##########################################################

u = seq(.05,.95,by=.01)
length(u)
coefstd = function(u) summary(rq(Electricity~Gas ,tau=u))$coefficients[,2]
coefest = function(u) summary(rq(Electricity~Gas ,tau=u))$coefficients[,1]
CS = Vectorize(coefstd)(u)
CE = Vectorize(coefest)(u)
plot(Electricity~ Gas,cex=0.1, xlab = "Natural gas ($)", ylab = 
       "Electricity ($)")
abline(a=CE[1,1], b=CE[2,1],col="blue")
abline(a=CE[1,45], b=CE[2,45], col="gray")
abline(a=CE[1,91], b=CE[2,91], col="red")
#text(locator(), labels = c("5%", "50%", "95%"))	


tau=seq(0.1,0.9,0.1)

### Table 7 ##################################################################
### AIC criteria for quantile models with autorregressions ###################
n_=length(Y)
l=length(tau)
#AR 1
k=1
quantreg1 <- rq(Y~Y1, tau=seq(0.1,0.9,0.1))
RSS_1=quantreg1$residuals^2
AIC1= 2*k+(2*n_*log(colSums(RSS_1)))

#AR 2
k=2
quantreg2 <- rq(Y~Y1+Y2, tau=seq(0.1,0.9,0.1))
RSS_2=quantreg2$residuals^2
AIC2= 2*k+(2*n_*log(colSums(RSS_2)))

#AR 3
k=3
quantreg3 <- rq(Y~Y1+Y2+Y3, tau=seq(0.1,0.9,0.1))
RSS_3=quantreg3$residuals^2
AIC3= 2*k+(2*n_*log(colSums(RSS_3)))

#AR 4
k=4
quantreg4 <- rq(Y~Y1+Y2+Y3+Y4, tau=seq(0.1,0.9,0.1))
RSS_4=quantreg4$residuals^2
AIC4= 2*k+(2*n_*log(colSums(RSS_4)))

#AR 5
k=5
quantreg5<- rq(Y~Y1+Y2+Y3+Y4+Y5, tau=seq(0.1,0.9,0.1))
RSS_5=quantreg5$residuals^2
AIC5= 2*k+(2*n_*log(colSums(RSS_5)))

AIC=matrix(c(AIC1,AIC2,AIC3,AIC4,AIC5),,l)

### Figure 11 ################################################################
### Plot of the intercept and the coefficients associated to different lags of the price of electricity at different quantiles ##############################

summary(quantreg5)
quantreg5.plot=summary(quantreg5)
plot(quantreg5.plot, main=c("Intercept","Lag 1","Lag 2","Lag 3","Lag 4","Lag 5"))

### Figure 12 ################################################################
### Plot of electricity prices and percentiles 10th (left) and 90th (right), including five lags of prices as explanatory variables #######################

y2=quantreg5$fitted.values[,9]
y3=quantreg5$fitted.values[,1]
x = seq(1,941,by=1)
plot(x,Electricity, xlab='Observation', ylab='Return')
lines(x,y3,col="red")

plot(x,Electricity, xlab='Observation', ylab='Return')
lines(x,y2,col="blue")

