###########################
###########################
### Forecasting Example ###
###########################
###########################


#clear enviroment
rm(list = ls())

#load libraries 
library(tidyverse)
library(BART)
library(tsDyn)
library(lubridate)
library(forecast)

#parallel libraries
library(foreach)
library(doParallel)

#necessary functions
source('functions.R')

#specify cores
options(cores = 4)

#load IC data
load('ICData.rda')
rawData = station_int[,c('tempf', 'windspeedmph')]


###################
### ARIMA Model ###
###################

trainLen = 10308
tau = 24
testLen = 24
timegap = 0
sets = cttv(rawData = rawData,
            tau = tau,
            trainLen = trainLen+timegap,
            testLen = testLen)

#ARIMA for temperature
train.y = sets$yTrain[,1]
fit = auto.arima(train.y)
yhat = forecast(fit, h = testLen, level = c(0.95))[['mean']]

#ARIMA for wind speed
train.y = sets$yTrain[,2]
fit = auto.arima(train.y)
yhat = forecast(fit, h = testLen, level = c(0.95))[['mean']]


######################
### Deep ESN Model ###
######################


#Set hyper-parameters
layers = 2
alpha = 0.64 
lambda.r = 0.1
last.n.h = 50
reduced.units = 25
first.n.h = last.n.h
m = 2
nu = c(0.2,0.8)
n.h = c(rep(first.n.h, layers-1), last.n.h)


#Assumed/fixed parameters
tau = 24
pi.w = rep(0.1, layers)
pi.win = rep(0.1, layers)
eta.w = rep(0.1, layers)
eta.win = rep(0.1, layers)
iterations = 100
locations = 2 
trainLen = 10308-tau
testLen = 24 
timegap = 0


#Generate training/testing/valid sets
sets = cttv(rawData = rawData,
            tau = tau,
            trainLen = trainLen+timegap,
            testLen = testLen)


#Generating input data
input.dat = gen.input.data(trainLen = trainLen+timegap,
                           m = m,
                           tau = tau,
                           yTrain = (sets$yTrain),
                           rawData = (rawData),
                           locations = locations,
                           xTestIndex = sets$xTestIndex,
                           testLen = testLen)
y.scale = input.dat$y.scale
y.train = input.dat$in.sample.y
designMatrix = input.dat$designMatrix
designMatrixOutSample = input.dat$designMatrixOutSample
addScaleMat = input.dat$addScaleMat


#Begin ESN forecasting
testing = deep.esn.with.gap(y.train = y.train,
                            x.insamp = designMatrix,
                            x.outsamp = designMatrixOutSample,
                            y.test = sets$yTest,
                            n.h = n.h,
                            nu = nu,
                            pi.w = pi.w, 
                            pi.win = pi.win,
                            eta.w = eta.w,
                            eta.win = eta.win,
                            lambda.r = lambda.r,
                            alpha = alpha,
                            m = m,
                            iter = iterations,
                            trainLen = trainLen,
                            future = testLen,
                            timegap = timegap,
                            layers = layers,
                            reduced.units = reduced.units,
                            startvalues = NULL,
                            activation = 'tanh',
                            distribution = 'Unif',
                            scale.factor = y.scale,
                            scale.matrix = addScaleMat,
                            logNorm = FALSE,
                            fork = FALSE,
                            parallel = FALSE,
                            verbose = TRUE)



testing$forecastmean




##################
### BART Model ###
##################

# Read in the data
s_i1 <- as.data.frame(station_int)
df = s_i1 %>% 
  mutate(time=as.POSIXct(strptime(paste0(year,"-",month,"-",day," ",hour,":00:00"),
                                  format = "%Y-%m-%d %H:%M:%S"),tz="America/Chicago"))

### creates 1 day lagged indices
df_lag = df %>% 
  select(-c(year,month,day,hour)) %>%
  mutate(time = time + 86400)

colnames(df_lag) = paste0(colnames(df_lag),"_lag")  

df_lag = df_lag %>% rename(time=time_lag)

df_all = df %>% 
  left_join(df_lag) %>%
  dplyr::filter(across(everything(), ~ !is.na(.x)))


df_in = df_all %>%
  dplyr::filter(time <= as.POSIXct(strptime("2021-09-22 12:00:00",
                                            format = "%Y-%m-%d %H:%M:%S"),tz="America/Chicago"))

df_out = df_all %>%
  dplyr::filter(time > as.POSIXct(strptime("2021-09-22 12:00:00",
                                           format = "%Y-%m-%d %H:%M:%S"),tz="America/Chicago"),hour==12)


### BART for temp ###
nmcmc=2000
nburn=1000

dfsub=df_in

y_train_sub=dfsub$tempf

x_train_sub=dfsub %>% 
  select(-c(year,day,tempf,time))

dfsub_out=df_out

x_test_sub=dfsub_out %>% 
  select(-c(year,day,tempf,time))

set.seed(99)
post <- wbart(x_train_sub, y_train_sub, x_test_sub, ndpost=nmcmc, nskip=nburn)

post$ypred.test=post$yhat.test + post$sigma[-(1:nburn)]*matrix(rnorm(nmcmc*length(dfsub_out$tempf)),nrow=nmcmc)

dfsub_out$fit <- post$yhat.test.mean

dfsub_out$lwr =  apply(post$ypred.test,2,quantile,0.025)
dfsub_out$upr =  apply(post$ypred.test,2,quantile,0.975)

post_tempf=post
temperature_bart = dfsub_out

### BART for Wind speed ###
nmcmc=2000
nburn=1000

dfsub=df_in

y_train_sub=dfsub$windspeedmph

x_train_sub=dfsub %>% 
  select(-c(year,day,windspeedmph,time))

dfsub_out=df_out

x_test_sub=dfsub_out %>% 
  select(-c(year,day,windspeedmph,time))

set.seed(99)
post <- wbart(x_train_sub, y_train_sub, x_test_sub, ndpost=nmcmc, nskip=nburn)

post$ypred.test=post$yhat.test + post$sigma[-(1:nburn)]*matrix(rnorm(nmcmc*length(dfsub_out$windspeedmph)),nrow=nmcmc)

dfsub_out$fit <- post$yhat.test.mean

dfsub_out$lwr =  apply(post$ypred.test,2,quantile,0.025)
dfsub_out$upr =  apply(post$ypred.test,2,quantile,0.975)

post_windspeedmph=post
wind_bart = dfsub_out


save(temperature_bart, file = 'BART_Temperature.RData')
save(wind_bart, file = 'BART_Wind.RData')

