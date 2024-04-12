#### Get data and divide into training and test

load("ICData.rda")

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

wind.model=auto.arima(train$wind.res,xreg = xreg.wind.train)
temp.model=auto.arima(train$tmpf.res,xreg = xreg.temp.train)

pred.wind=matrix(nrow=(nrow(test)-1),ncol=3)
pred.temp=matrix(nrow=(nrow(test)-1),ncol=3)

wind.model2=Arima(train$wind.res,xreg=as.matrix(xreg.wind.train),model=wind.model)
wind.1=data.frame(forecast(wind.model2,xreg=as.matrix(xreg.wind.test[1,]),h=1))
pred.wind[1,]=c(wind.1$Point.Forecast,wind.1$Lo.95,wind.1$Hi.95)

temp.model2=Arima(train$tmpf.res,xreg=as.matrix(xreg.temp.train),model=temp.model)
temp.1=data.frame(forecast(temp.model2,xreg=as.matrix(xreg.temp.test[1,]),h=1))
pred.temp[1,]=c(temp.1$Point.Forecast,temp.1$Lo.95,temp.1$Hi.95)

### Forecast
for (i in 2:nrow(pred.wind)){
  wind.model2=Arima(c(train$wind.res,test$wind.res[1:(i-1)]),xreg=as.matrix(rbind(xreg.wind.train,xreg.wind.test[1:(i-1),])),model=wind.model)
  wind.1=data.frame(forecast(wind.model2,xreg=as.matrix(xreg.wind.test[i,]),h=1))
  pred.wind[i,]=c(wind.1$Point.Forecast,wind.1$Lo.95,wind.1$Hi.95)
  
  temp.model2=Arima(c(train$tmpf.res,test$tmpf.res[1:(i-1)]),xreg=as.matrix(rbind(xreg.temp.train,xreg.temp.test[1:(i-1),])),model=temp.model)
  temp.1=data.frame(forecast(temp.model2,xreg=as.matrix(xreg.temp.test[i,]),h=1))
  pred.temp[i,]=c(temp.1$Point.Forecast,temp.1$Lo.95,temp.1$Hi.95)
  
}

### Actual values
  actual.temp=test$tmpf.res[1:(nrow(test)-1)]
  actual.wind=test$wind.res[1:(nrow(test)-1)]
  
### Replace predicted negative values with 0
  pred.wind[pred.wind<0]<-0
  

  
  