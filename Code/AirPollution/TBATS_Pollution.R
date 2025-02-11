library(forecast)
library(lubridate)
library(dplyr)
library(ggplot2)
library(scales)
library(scoringRules)
library(scoringutils)

dat<-read.csv('pm25_2009_2019_golden_horseshoe_sites.csv')
dat<-dat[-2616,]

for.pm<-array(dim=c(12,10,355))
for.lower<-array(dim=c(12,10,355))
for.upper<-array(dim=c(12,10,355))

### On October 9, 2023...changed train to go to 3651 
## instead of 3652 since deleted leap day above.

for (i in 1:12) {
  dat1<-ts(dat[,i],frequency = 365)
  train<-dat1[1:3651]
  test<-dat1[3652:length(dat1)]
  model1=tbats(train)
  temp=data.frame(forecast(model1,h=10))
  for.pm[i,,1]=temp[,1]
  for.lower[i,,1]=temp[,4]
  for.upper[i,,1]=temp[,5]
   for (k in 2:355){
    model2=tbats(c(train,test[1:(k-1)]),model=model1)
    temp=data.frame(forecast(model2,h=10))
    for.pm[i,,k]=temp[,1]
    for.lower[i,,k]=temp[,4]
    for.upper[i,,k]=temp[,5]
   }}

true.pm<-array(dim=c(12,10,355))

for (i in 1:12){
  for (k in 1:355){
    true.pm[i,,k]=dat[(3652+k):(3652+k+9),i]}}
