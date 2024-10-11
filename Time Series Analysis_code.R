rm(list=ls()) # clear workspace
setwd("C:/Users/.../")

library(tidyverse)    # import routines for easy data management
library(tsibble)      # Allow for time series data
library(haven)        # Read files
library(lmtest)       # Commands for hypothesis tests
library(lubridate)    # Used for dates
library(urca)         # Unit Root/Co-integration
library(stargazer)
library(vars)

FREDdata <- read_dta("../...dta") # Load the data into R (or stata)

ls.str(FREDdata) # View the structure of the imported data
view(FREDdata)
summary(FREDdata) #PCEC96 variable is missing 88 observations
FREDdata <- FREDdata[!is.na(FREDdata$PCEC96), ] ##drop missing observations
attach(FREDdata)

#####################################################################
#                                                                   #
#                          Using variable:                          #
#                               PCEC96                              #
#                  Real Personal Consumption Expenditures US        #         
#                                                                   #
#####################################################################


# Simplify PCEC96 to just p
# Convert string-variable: datestr to date-variable: date
FREDdata=FREDdata%>%rename(p=PCEC96)%>%mutate(date=ymd(datestr))%>%as_tsibble(index=date)

#take log of p and the change in log p which is refered to as delta
# we stabilize the variable log(p) by converting in into returns via first difference
FREDdata=FREDdata%>%mutate(lnp=log(p),delta=400*difference(lnp))%>%drop_na()
#400 because 100 to turn to % and 4 because data is quarterly

#####################################################################
#                                1(a)                               #
#      Graph Real Personal Consumption Expenditures of US           #
# Does it appear to be stationary, trend stationary or stochastic?  #
#####################################################################

plot(FREDdata$date,FREDdata$lnp,type="l",  main = "Real Personal Consumption Expenditures of US", xlab = "Year", ylab = "Log")
plot(FREDdata$date,FREDdata$delta,type="l",  main = "Difference in Real Personal Consumption Expenditures of US", xlab = "Year", ylab = "Log")

#####################################################################
#                                1(b)                               #
#        Formally test for the presence of a unit root.             # 
#    Explain what version of the test you used and why you used it. #
#####################################################################
pacf(FREDdata$lnp, main="Partial Autocorrelation Function of Log(PCEC96)") 
#results indicate this is likely an ar(1)
#so put in an extra lag in Dickey-Fuller Test

print(summary(ur.df(FREDdata$lnp, type="trend", lags=2))) #Unit Root Dickey-Fuller Test for stochastic (hence type=trend)


#Unit Root Dickey-Fuller Test for deltalation 
print(summary(ur.df(FREDdata$delta, type="none", lags=1))) 

# delta.lag.1 returned a test statistic (t-value) of -5.876.
# Given that the tau1 critical values are -2.6 -1.95 -1.61
# We reject the null hypothesis of non-stationarity.
# delta is stationary.

######################################################################
#                                1(c)                                #
#                Examine the autocorrelation function.               #
# Propose several possible models that may have generated the series #
######################################################################

acf(FREDdata$lnp, main = "Log(PCEC96) Autocorrelation Function")
pacf(FREDdata$lnp, main = "Log(PCEC96) Partial Autocorrelation Function")


# acf() produces acf together graphs and error bands of the variable p
acf(FREDdata$delta, main = "Difference Autocorrelation Function")

pacf(FREDdata$delta, main="Difference Partial Autocorrelation Function") 
#results indicate 1 lag(1)


######################################################################
#                                1(d)                                #
#            Estimate the model(s) proposed in part 1c.              #
#                                                                    #
######################################################################

# fit an ARIMA(1,0,0) model 
# along with variations including: 
# ARIMA(1,0,1), ARIMA(0,0,1), ARIMA(2,0,2), ARIMA(0,0,2), ARIMA(2,0,0)

arma11=arima(FREDdata$delta,order=c(1,0,1))   # ar(1) and ma(1): delta = delta_t-1 + e_t-1 
arma01=arima(FREDdata$delta,order=c(0,0,1))   # ma(1): delta= e_t-1
arma10=arima(FREDdata$delta,order=c(1,0,0))   # ar(1): delta = delta_t-1
arma22=arima(FREDdata$delta,order=c(2,0,2))   # ar(2) and ma(2): delta = delta_t-1 + delta_t-2 + e_t+ e_t-1 + e_t-2
arma02=arima(FREDdata$delta,order=c(0,0,2))   # ma(2): delta =  e_t-1 + e_t-2
arma20=arima(FREDdata$delta,order=c(2,0,0))   # ar(2): delta = delta_t-1 + delta_t-2)

stargazer(arma11,arma01,arma10,arma02,arma20,
          type="latex",
          column.labels=c("ARMA(1,1)","ARMA(0,1)","ARMA(1,0)","ARMA(2,2)",
                           "ARMA(0,2)","ARMA(2,0)"))


arma11_lnp=arima(FREDdata$lnp,order=c(1,0,1))   # ar(1) and ma(1): delta = delta_t-1 + e_t-1 
arma01_lnp=arima(FREDdata$lnp,order=c(0,0,1))   # ma(1): delta= e_t-1
arma10_lnp=arima(FREDdata$lnp,order=c(1,0,0))   # ar(1): delta = delta_t-1
arma22_lnp=arima(FREDdata$lnp,order=c(2,0,2))   # ar(2) and ma(2): delta = delta_t-1 + delta_t-2 + e_t+ e_t-1 + e_t-2
arma02_lnp=arima(FREDdata$lnp,order=c(0,0,2))   # ma(2): delta =  e_t-1 + e_t-2
arma20_lnp=arima(FREDdata$lnp,order=c(2,0,0))   # ar(2): delta = delta_t-1 + delta_t-2)

stargazer(arma11_lnp,arma01_lnp,arma10_lnp,arma02_lnp,arma20_lnp,
          type="latex",
          column.labels=c("ARMA(1,1)","ARMA(0,1)","ARMA(1,0)","ARMA(2,2)",
                          "ARMA(0,2)","ARMA(2,0)"))


######################################################################
#                                1(f)                                #
# Use the model you deem most appropriate to generate forecasts for  #
#      the next year. Plot forecasted and actual observations.       #
#              Also include 95% forecast error bands.                #
#                                                                    #
######################################################################

#we wanna forecast the model based on a training set data [and initial part of the data (a subsample)]
#Then we wanna compare that forecast to what actually happened in the later part of the data
train=filter(FREDdata,date<as.Date('2021-01-01')) # a new dataset created with data up to 2010 is training data
test=filter(FREDdata,date>=as.Date('2021-01-01')) # a new dataset created with data after 2010 is testing data

#AR1 model for forecasting
ar1=arima(train$lnp,order=c(1,0,0))
#MA1 model for forecasting
ma1=arima(train$delta,order=c(0,0,1))

#AR forecast
fc_ar=predict(ar1,n.ahead=nrow(test))

#MA forecast
fc_ma=predict(ma1,n.ahead=nrow(test))
#number of periods ahead = number of obs in test data

#fancy graph ar
test$pred_ar=fc_ar$pred 
#fancy graph ma

test$pred_ma=fc_ma$pred 
# in the test dataframe create a new column called pred and make that equal to forcast element's prediction 

#AR standard error bands
test$low_ar=fc_ar$pred-1.96*fc_ar$se #1.96 is the 95th percentile of the normal distribution
test$up_ar=fc_ar$pred+1.96*fc_ar$se #1.96*standard error


#MA standard error bands
test$low_ma=fc_ma$pred-1.96*fc_ma$se #1.96 is the 95th percentile of the normal distribution
test$up_ma=fc_ma$pred+1.96*fc_ma$se #1.96*standard error

#MA(1) Forecast for Difference
ggplot(data=test, aes(x=date)) +
  geom_line(aes(y=delta), color="black")+
  geom_line(aes(y=pred_ma), color="blue")+
  geom_line(aes(y=low_ma), color="red")+
  geom_line(aes(y=up_ma), color="red") +
  labs(x="Quarterly",
       y="Difference Log(PCEC96)", title="")

#AR(1) Forecast for Log(PCEC96)
ggplot(data=test, aes(x=date)) +
  geom_line(aes(y=lnp), color="black")+
  geom_line(aes(y=pred_ar), color="blue")+
  geom_line(aes(y=low_ar), color="red")+
  geom_line(aes(y=up_ar), color="red") +
  labs(x="Quarterly",
       y="Log(PCEC96)", title="")



######################################################################
#                                  2                                 #
#      Use a VAR to make a forecast of your assigned variable.       #
#    To do so you will need to choose at least one (but maybe more)  #
#     other variable(s) from the dataset – or even from elsewhere.   #
######################################################################

# RSXFS Advanced Retail Sales: Retail Trade 

######################################################################
#                                 2(a)                               #
#                       Choose another variable                      #
#                Explain why it is reasonable to expect              # 
#     that it will improve the forecast of your assigned variable.   #
######################################################################

FREDdata=FREDdata%>%mutate(lrs=log(RSXFS))%>%drop_na()

######################################################################
#                                 2(b)                               #
#                   Are the new variable(s) stationary?              #
######################################################################

#GDP
plot(FREDdata$date,FREDdata$lrs,type="l",  main = "Retail Sales for US", xlab = "Year", ylab = "Log")

pacf(FREDdata$lrs, main="Partial Autocorrelation Function for Log(RSXFS)") 
#results indicate this is likely an ar(1)
#so put in an extra lag in Dickey-Fuller Test

print(summary(ur.df(FREDdata$lrs, type="trend", lags=2))) #Unit Root Dickey-Fuller Test for stochastic (hence type=trend)


######################################################################
#                                 2(c)                               #
#               Decide how many lags the VAR should have.            #
#                      Explain your reasoning                        #
######################################################################


y=ts(cbind(FREDdata$lnp,FREDdata$lrs),names=c("lnp","lrs"))

print(VARselect(y,type="const"))
#all information criteria tell me to use 2 lags

######################################################################
#                                 2(d)                               #
#               Estimate the VAR and report the coefficients.        #
#                                                                    #
######################################################################
v=VAR(y,p=2,type="const")
print(summary(v))

# with the above code I essentially ran 2 ols regressions:
#        1. lnp = lnp.l1 + lrs.l1 + lnp.l2 + lrs.l2 + const 
#        2. lrs = lnp.l1 + lrs.l1 + lnp.l2 + lrs.l2 + const 

# The coefficients on the lagged terms both sum to close to 1 indicative of
# them being unit roots.
# The coefficients are [L1] 0.2826 and [L2] 0.4969 .... sum to 0.7795
#                      [L1] 1.6039 and [L2] -0.5023 ... sum to 1.1016
#                             respectively


######################################################################
#                                 2(e)                               #
#                      Graph the impulse functions.                  #
#         Comment on their realism and/or economic interpretation.   #                     
######################################################################
plot(irf(v),plot.type="multiple")


######################################################################
#                                 2(f)                               #
#                 Test the system for Granger causation.             #
#                Explain the implications of your results            #
#                 for forecasts of your assigned variable.           #
######################################################################
print(causality(v,cause="lrs")$Granger)

# Results indicate that adding retail sales to the equation improves
# my forecast for Personal Consumption Expenditure
######################################################################
#                                 2(g)                               #
#         Create and graph a forecast of your assigned variable.     #                 #
#                                                                    #
######################################################################

fc=predict(v,n.ahead=4)

plot(fc, main = "plot")




