setwd("~/Desktop/dizzy/Submission file")



install.packages("readxl")
install.packages("urca")
install.packages("forecast")
install.packages("tidyverse")
install.packages("vars")
install.packages("tseries")
install.packages("quantmod")
install.packages("tsDyn")

library(readxl)
library(urca)
library(forecast)
library(tidyverse)
library(vars)
library(tseries)
library(quantmod)
library(tsDyn)

#load dataset
hdb<- read_xlsx("HDB yearly.xlsx")
hdb2<- read_xlsx("hdb yearly standardised.xlsx")

#declare time series objects

Price <- ts(hdb2$Price, start=c(1990), frequency=1)
Topten <- ts(hdb2$Topten, start=c(1990), frequency=1)
GDP <- ts(hdb2$GDP, start=c(1990), frequency=1)

####TABLE 1: SUMMARY STATS####
summary(hdb$Price)
summary(hdb$GDP)
summary(hdb$Topten)
sd(hdb$Price)
sd(hdb$GDP)
sd(hdb$Topten)

#Bind into a system#
dset5 <- cbind(Price,Topten,GDP)


###FIGURE 3: TIME SERIES VISUALISATION###
autoplot(dset5)


####APPENDIX A: UNIT ROOT TEST####

##TABLE A1: Phillips Perron Test##
#all vars non-stationary
pp.test(Price) 
pp.test(Topten)
pp.test(GDP)
#all vars stationary at first diff
pp.test(diff(Price)) #p-val<0.05, reject null (diff is stationary)
pp.test(diff(Topten)) #diff is stationary at 1%
pp.test(diff(GDP)) #diff is stationary at 10%

##TABLE A2: KPSS Test (stationary test, null hypothesis is stationary)##
kpss.test(Price) #p-val small, reject null
kpss.test(diff(Price)) #don't reject, I(1)
kpss.test(GDP)
kpss.test(diff(GDP)) #don't reject, I(1)
kpss.test(Topten) #reject
kpss.test(diff(Topten)) #don't reject, I(1)

##Table A1 (Note): Lag selection criteria##
lagselect2 <- VARselect(dset5,lag.max = 6, type = "const")
lagselect2$selection 
#AIC shows 6, hence 6-1=5 lags chosen


#####COINTEGRATION TEST#####

##TABLE 2: Johansen Testing (Trace)##
ctest2t <- ca.jo(dset5, type = "trace", ecdet = "const", K=5)
summary(ctest2t) #we reject hypothesis that r<=1 and don't reject r<=2 therefore 2 relationships

######VECTOR ERROR CORRECTION MODEL######


###TABLE 3:vecm model###

model2 <- VECM(dset5,5,r=2,estim="ML")
summary(model2) 
#cointegrating vector is my Beta matrix; Beta is simply "what relative weights of the series are 
#needed to produce a stationary combination from the original (integrated) variables", **we flip the coefficient signs for beta!!!**
#alpha (loading matrix) which is the ect tells us more about relationship#


#####STRUCTURAL ANALYSIS OF VAR model (derived from VECM)#####

#need to transform VECM to VAR *form* (not a VAR model in itself!)#

model2VAR <- vec2var(ctest2t,r=2)
model2VAR

##FIGURE 4: impulse response functions using vec2var form##

Toptenirf <- irf(model2VAR, impulse="Price", response="Topten", n.ahead=15,boot=TRUE)
plot(Toptenirf, ylab="Topten",main="Response of Top 10% Income Share to 1 s.d. shift in HDB Resale Index") #for every unit increase of price, gini falls

GDPirf <- irf(model2VAR, impulse="Price", response="GDP", n.ahead=15,boot=TRUE)
plot(GDPirf, ylab="GDP",main="Response of GDP per capita to 1 s.d. shift in HDB Resale Index")

##FIGURE 5/APPENDIX C1: variance decomposition (FEVD)##
#explains variation in one variable's rate in terms of shocks to that variable by itself and other vars (in %)

FEVD2 <- fevd(model2VAR, n.ahead=10)
plot(FEVD2) #price increasingly exp by GNI,topten slightly exp by price but mostly GNI, GNI mid-term more explained by price


#####APPENDIX B: DIAGNOSTIC TESTS######

#serial correlation
serial2 <- serial.test(model2VAR, lags.pt=10, type="PT.asymptotic")
serial2 #no serial correlation

#ARCH effects
Arch2 <- arch.test(model2VAR, lags.multi=10, multivariate.only=TRUE)
Arch2 #no arch effects

#normality of residuals
Norm2 <- normality.test(model2VAR)
Norm2





