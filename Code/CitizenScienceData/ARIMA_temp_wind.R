#### Get data and divide into training and test
rm(list = ls())
load("Data/ICData.rda")

dat=station_int[station_int[,4]==12,]
library(forecast)
library(Hmisc)
library(dplyr)
dat=data.frame(dat)

dat$wind.res=Lag(dat$windspeedmph,-1)
dat$tmpf.res=Lag(dat$tempf,-1)
library(fastDummies)
dat2 <- fastDummies::dummy_cols(dat,select_columns = "month",remove_first_dummy = TRUE)
dat2 <-dat2 %>% mutate(id=row_number())
train=dat2 %>% filter((month==9 & day<22 & year==2021)| (month<9 & year==2021) | (year<2021))
test= anti_join(dat2, train,by='id')


### Create the regression variables
xreg.wind.train=data.matrix(train[,c(5:7,12:22)])
xreg.temp.train=data.matrix(train[,c(6,12:22)])

xreg.wind.test=test[,c(5:7,12:22)]
xreg.temp.test=test[,c(6,12:22)]

###ARIMAX models

wind.model=auto.arima(train$wind.res,xreg = xreg.wind.train, seasonal = FALSE)
temp.model=auto.arima(train$tmpf.res,xreg = xreg.temp.train, seasonal = FALSE)

pred.wind=matrix(nrow=(nrow(test)-1),ncol=3)
pred.temp=matrix(nrow=(nrow(test)-1),ncol=3)

wind.model2=Arima(train$wind.res,xreg=as.matrix(xreg.wind.train),model=wind.model)
wind.1=data.frame(forecast(wind.model2,xreg=as.matrix(xreg.wind.test[1,]),h=1))
pred.wind[1,]=c(wind.1$Point.Forecast,wind.1$Lo.95,wind.1$Hi.95)

temp.model2=Arima(train$tmpf.res,xreg=as.matrix(xreg.temp.train),model=temp.model)
temp.1=data.frame(forecast(temp.model2,xreg=as.matrix(xreg.temp.test[1,]),h=1))
pred.temp[1,]=c(temp.1$Point.Forecast,temp.1$Lo.95,temp.1$Hi.95)

# Simplified model for discussion
temp.model33 <- auto.arima(train$tmpf.res, seasonal = FALSE)
temp.model33
# Series: train$tmpf.res
# ARIMA(1,1,1)
#
# Coefficients:
#   ar1      ma1
# 0.5794  -0.8883
# s.e.  0.0560   0.0291
#
# sigma^2 = 52.15:  log likelihood = -1452.75
# AIC=2911.51   AICc=2911.56   BIC=2923.69

### Forecast
for (i in 2:nrow(pred.wind)){
  wind.model2=Arima(c(train$wind.res,test$wind.res[1:(i-1)]),xreg=as.matrix(rbind(xreg.wind.train,xreg.wind.test[1:(i-1),])),model=wind.model)
  wind.1=data.frame(forecast(wind.model2,xreg=as.matrix(xreg.wind.test[i,]),h=1))
  pred.wind[i,]=c(wind.1$Point.Forecast,wind.1$Lo.95,wind.1$Hi.95)

  temp.model2=Arima(c(train$tmpf.res,test$tmpf.res[1:(i-1)]),xreg=as.matrix(rbind(xreg.temp.train,xreg.temp.test[1:(i-1),])),model=temp.model)
  temp.1=data.frame(forecast(temp.model2,xreg=as.matrix(xreg.temp.test[i,]),h=1))
  pred.temp[i,]=c(temp.1$Point.Forecast,temp.1$Lo.95,temp.1$Hi.95)


  temp.model3 = Arima(c(train$tmpf.res,test$tmpf.res[1:(i-1)]), model = temp.model33)
  temp3 = data.frame(forecast(temp.model3, h = 1))
  pred.temp[i,] = c(temp3$Point.Forecast, temp3$Lo.95, temp3$Hi.95)
}

### Actual values
  actual.temp=test$tmpf.res[1:(nrow(test)-1)]
  actual.wind=test$wind.res[1:(nrow(test)-1)]

### Replace predicted negative values with 0
  pred.wind[pred.wind<0]<-0




  results_obs <- cbind(actual.temp)
  results_mean <- pred.temp[,1, drop = FALSE]
  results_lower <- pred.temp[,2, drop = FALSE]
  results_upper <- pred.temp[,3, drop = FALSE]

  alpha = 0.05
  # Calculate performance metrics by station and forecast horizon
  PERF <- lapply(1, function(m) {
    # errors
    e <- results_obs - results_mean
    # mean squared error
    mse <- apply(e^2, 2, mean)
    # coverage
    cvr <- apply((results_lower <= results_obs) & (results_obs <= results_upper), 2, mean)
    # interval score
    iscore <- (results_upper - results_lower) + 2 * (results_lower - results_obs) * as.numeric(results_obs < results_lower) / alpha + 2 * (results_obs - results_upper) * as.numeric(results_obs > results_upper) / alpha
    iscore <- apply(iscore, 2, mean)
    # combine results
    tibble(MSE = mse,
           Coverage = cvr,
           iscore = iscore)
  })
PERF %>% as.data.frame()
# MSE Coverage iscore
# 1 44.84     0.95 32.053

# Fahrenheit to Celsius MSE
sqrt(51.28)*5/9
# [1] 3.978336

sqrt(44.84)*5/9
# [1] 3.7201
