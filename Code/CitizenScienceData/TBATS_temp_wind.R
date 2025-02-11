load("ICData.rda")
station_int<-data.frame(station_int)
library(forecast)
library(lubridate)
library(dplyr)
library(ggplot2)
library(scales)
library(scoringRules)
library(scoringutils)
wind.ts<-msts(station_int$windspeedmph,seasonal.periods = c(24,24*365))
temp.ts<-msts(station_int$tempf,seasonal.periods = c(24,24*365))

#same code was used for both wind and temperature...
#  just changed train and test


train=temp.ts[1:10308]
test=temp.ts[10309:length(temp.ts)]
#train=wind.ts[1:10308]
#test=wind.ts[10309:length(wind.ts)]

for.mean=vector(length=100)
for.lower=vector(length=100)
for.upper=vector(length=100)
start.time=Sys.time()
model1=tbats(train,use.arma.errors = T,seasonal.periods = c(24,24*365))
end.time=Sys.time()
temp=data.frame(forecast(model1,h=24))
for.mean[1]=temp[24,1]
for.lower[1]=temp[24,4]
for.upper[1]=temp[24,5]


for (i in 2:100){
  model2=tbats(c(train,test[1:(24*(i-1))]),model=model1)
  temp=data.frame(forecast(model2,h=24))
  for.mean[i]=max(0,temp[24,1])
  for.lower[i]=max(0,temp[24,4])
  for.upper[i]=max(0,temp[24,5])
}

all.dat<-cbind(for.mean,for.lower,for.upper)
colnames(all.dat)<-c("prediction","lower","upper")


