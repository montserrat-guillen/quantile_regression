#===============================================================
# Chapter 4: Cross-sectional quantile regression
##===============================================================

### Chapter 4 ### Cross-sectional quantile regression
### Importing the data set ###

## clear all existing objects
rm(list=ls(all=TRUE))

## set working directory
setwd("introduce your directory here") 

## obtain data
load("RECS.Rdata")
attach(RECS)

# Figure 2 ##################################################################
# Quantiles for Cost of Electricity (US$) from RECS in 2015 #################

x <- DOLLAREL
n <- length(DOLLAREL)
plot((1:n - 1)/(n - 1), sort(x), type="l",
     xlab = "Fraction of data",
     ylab = "Quantiles of the total cost of electricity (US$)")

# Figure 3 ##################################################################
# OLS regression of Energy Consumption on Square Footage Unweighted and using the sample weights ###########################################################

# defining weights 

RECSnodes <- svydesign(id=~1, weights=~1, data=RECS)
svymean(~DOLLAREL+TOTSQFT_EN, RECSnodes)

# without weights 

RECSdes <- svydesign(id=~1, weights=~NWEIGHT, data=RECS)
svymean(~DOLLAREL+TOTSQFT_EN, RECSdes)

# Linear regression using lm and plotting of the results  

OLS_RECSunw<- lm(DOLLAREL~TOTSQFT_EN, data=RECS, model=T)
summary(OLS_RECSunw)

OLS_RECSw<- lm(DOLLAREL~TOTSQFT_EN, data=RECS, model=T, weights=NWEIGHT)
summary(OLS_RECSw)

# Top panel

plot(DOLLAREL ~ TOTSQFT_EN, data = RECS,  cex=0.1, xlab = "Surface (squared feet)", ylab = 
      "Electricity Energy Consumption in 2015 ($)")
abline(coef(OLS_RECSunw)[1:2])
cf <- round(coef(OLS_RECSunw), 3) 
eq <- paste0("Cost= ", cf[1],
             ifelse(sign(cf[2])==1, " + ", " - "), abs(cf[2]), " Surface")
mtext(eq, 3, line=-2)

# Bottom panel


plot(RECS$DOLLAREL ~ RECS$TOTSQFT_EN, data = RECS, cex=NWEIGHT*2/max(NWEIGHT), 
      xlab = "Surface (squared feet)", ylab = 
       "Electricity Energy Consumption in 2015 ($)")
abline(coef(OLS_RECSw)[1:2])
cf <- round(coef(OLS_RECSw), 3) 
eq <- paste0("(Weighted OLS) Cost= ", cf[1],
             ifelse(sign(cf[2])==1, " + ", " - "), abs(cf[2]), " Surface")
mtext(eq, 3, line=-2)

# Figure 4 and Table 2 #######################################################
# Weighted OLS and weighted quantile regressions’ coefficients ###############
#### The following command calls the package ‘quantreg’ available in R to perform the estimations and plot the figure.

 library(quantreg)

# Quantile regression fitting 

QR_RECS2 <- rq(DOLLAREL~TOTSQFT_EN, data =RECS, model=TRUE,
               weights=NWEIGHT, tau= seq(0.05,0.95,by=0.10))
QR.1<-summary(QR_RECS2)

#  Potting of the results

plot(QR.1, main=c("Intercept","Total square footage"))

# Figure 5 ###################################################################
## Plot of the simple quantile regressions for the Energy Consumption as a function of the household Surface 

plot(RECS$TOTSQFT_EN, RECS$DOLLAREL, xlab = "Surface (squared feet)", ylab = 
       "Electricity Energy Consumption in 2015 ($)")

# define the quantile levels and employ them in the regression

u = seq(.05,.95,by=.01)
length(u)
coefstd = function(u) summary(rq(DOLLAREL~TOTSQFT_EN, data =RECS , weights=NWEIGHT,tau=u))$coefficients[,2]
coefest = function(u) summary(rq(DOLLAREL~TOTSQFT_EN, data =RECS , weights=NWEIGHT,tau=u))$coefficients[,1]
CS = Vectorize(coefstd)(u)
CE = Vectorize(coefest)(u)
abline(a=CE[1,1], b=CE[2,1],col="blue")
abline(a=CE[1,45], b=CE[2,45], col="gray")
abline(a=CE[1,91], b=CE[2,91], col="red")
text(locator(), labels = c("5%", "50%", "95%"))	

# Table 3 ##########################################
# Weighted quantile regressions’ coefficients at different quantiles

QR_RECS3.1 <-rq(DOLLAREL~factor(SWIMPOOL)+TOTSQFT_EN+TOTCSQFT+TOTHSQFT+BEDROOMS+factor(SOLAR)+TVCOLOR+NUMSMPHONE+NUMFLOORFAN+NHSLDMEM,
                 data =subset(RECS,SWIMPOOL>=0) , weights=NWEIGHT,  model=TRUE, tau= 0.25)
summary.rq(QR_RECS3.1, se="rank",covariance = T)

QR_RECS3.2 <- rq(DOLLAREL~factor(SWIMPOOL)+TOTSQFT_EN+TOTCSQFT+TOTHSQFT+BEDROOMS+factor(SOLAR)+TVCOLOR+NUMSMPHONE+NUMFLOORFAN+NHSLDMEM,
                 data =subset(RECS,SWIMPOOL>=0) , weights=NWEIGHT,  model=TRUE, tau=0.50)
summary.rq(QR_RECS3.2, se="rank",covariance = T)

QR_RECS3.3 <- rq(DOLLAREL~factor(SWIMPOOL)+TOTSQFT_EN+TOTCSQFT+TOTHSQFT+BEDROOMS+factor(SOLAR)+TVCOLOR+NUMSMPHONE+NUMFLOORFAN+NHSLDMEM,
                 data =subset(RECS,SWIMPOOL>=0) , weights=NWEIGHT,  model=TRUE, tau= 0.75)
summary.rq(QR_RECS3.3, se="rank",covariance = T)

QR_RECS3.4 <- rq(DOLLAREL~factor(SWIMPOOL)+TOTSQFT_EN+TOTCSQFT+TOTHSQFT+BEDROOMS+factor(SOLAR)+TVCOLOR+NUMSMPHONE+NUMFLOORFAN+NHSLDMEM,
                 data =subset(RECS,SWIMPOOL>=0) , weights=NWEIGHT,  model=TRUE, tau= 0.95)
summary.rq(QR_RECS3.4, se="rank",covariance = T)


# Figure 6  ##########################################
#  Plot of the coefficients of the weighted quantile regression model and the OLS model for Electricity Consumption 

QR_RECS4 <- rq(DOLLAREL~factor(SWIMPOOL)+TOTSQFT_EN+TOTCSQFT+TOTHSQFT+BEDROOMS+factor(SOLAR)+TVCOLOR+NUMSMPHONE+NUMFLOORFAN+NHSLDMEM,
               data =subset(RECS,SWIMPOOL>=0) , weights=NWEIGHT,  model=TRUE, tau= seq(0.05,0.95,by=0.10))
QR.2<-summary(QR_RECS4)

plot(QR.2, main=c("Intercept","Swimming Pool","Square Footage",
                  "Cooled square footage","Heated square footage",
                  "Num.Bedrooms","Electricity generation from solar",
                  "TVCOLOR", "Smart Phones", "Floor fans",
                  "Num. of household members"))

# OLS regression
OLS_RECS4 <- lm(DOLLAREL~factor(SWIMPOOL)+TOTSQFT_EN+TOTCSQFT+TOTHSQFT+BEDROOMS+factor(SOLAR)+TVCOLOR+NUMSMPHONE+NUMFLOORFAN+NHSLDMEM,
                data =subset(RECS,SWIMPOOL>=0) , weights=NWEIGHT,  model=TRUE)
summary(OLS_RECS4)
confint(OLS_RECS4,level = 0.95)

# Table 4 ##########################################
## Weighted OLS and unweighted unconditional quantile regressions’ coefficients at different quantiles
## the following command calls the package ‘uqr’ available in R to conduct unconditional quantile regressions

library(uqr)

formula = DOLLAREL~TOTSQFT_EN
UQR.1=urq(formula,data = RECS, kernel="gaussian", tau=0.25)
UQR.1$coefficients
summary=urqCI(urq =UQR.1,R = 100,graph = TRUE,seed = 1234)
summary$results
UQR.2=urq(formula,data = RECS, kernel="gaussian", tau=0.5)
UQR.2$coefficients
summary=urqCI(urq =UQR.2,R = 100,graph = TRUE,seed = 1234)
summary$results
UQR.3=urq(formula,data = RECS, kernel="gaussian", tau=0.75)
UQR.3$coefficients
summary=urqCI(urq =UQR.3,R = 100,graph = TRUE,seed = 1234)
summary$results
UQR.4=urq(formula,data = RECS, kernel="gaussian", tau=0.95)
UQR.4$coefficients
summary=urqCI(urq =UQR.4,R = 100,graph = TRUE,seed = 1234)
summary$results

