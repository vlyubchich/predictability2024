##########################################################
#
# Fourier Wind Direction
#
##########################################################

# Set the working directory
#setwd("/Volumes/GoogleDrive/.shortcut-targets-by-id/1ZaD4u22lYjzvmqq5twsPvWU3ecPu4pkq/Working Group /Uncertainty Quantification/Stefano weather station")

# Clear memory
rm( list = ls() )

library(BART)
library(tidyverse)
library(scoringutils)
library( tsDyn )
library( lubridate )
#library( TSA )
library( forecast )
library(tidyverse)

# Read in the data
load( "WeatherStationCleaned_Interpolated.rda" )

head( station_int )
s_i1 <- as.data.frame( station_int )

df = s_i1 %>% 
  mutate(time=as.POSIXct(strptime(paste0(year,"-",month,"-",day," ",hour,":00:00"),
    format = "%Y-%m-%d %H:%M:%S"),tz="America/Chicago"))

### creates 1-16 day day lagged indices
for(l in 1:16){
  df_lag = df %>% 
  #select(-c(year,month,day,hour)) %>%
  select(c(time,tempf,windspeedmph)) %>%
  mutate(time = time + l*86400)
  
  ind=which(colnames(df_lag)=="time")
  
  colnames(df_lag)[-ind] = paste0(colnames(df_lag)[-ind],"_lag",l)  

  df = df %>% 
  left_join(df_lag)
  }

df_all = df %>% 
    dplyr::filter(across(everything(), ~ !is.na(.x)))
  

df_in = df_all %>%
  dplyr::filter(time <= as.POSIXct(strptime("2021-09-22 12:00:00",
    format = "%Y-%m-%d %H:%M:%S"),tz="America/Chicago"))

df_out = df_all %>%
  dplyr::filter(time > as.POSIXct(strptime("2021-09-22 12:00:00",
    format = "%Y-%m-%d %H:%M:%S"),tz="America/Chicago"),hour==12)


### BART for temp ###

metrics_tempf=function(tab,post){
 out=tab %>% summarise(mspe=mean((tempf-fit)^2),
  crps=mean(crps_sample(tempf,t(post$ypred.test))),
  cp=mean((tempf>lwr)*(tempf<upr)),
  is=0.05/2*mean((upr-lwr)+2*(lwr-tempf)*(tempf<lwr)/.05+ 
 2*(tempf-upr)*(tempf>upr)/.05),
 is2=mean(interval_score(true_values =tempf, lower=lwr, upper=upr, interval_range = 95)))
 out
}

nmcmc=2000
nburn=1000

dfsub=df_in

y_train_sub=dfsub$tempf

x_train_sub=dfsub %>%
    select(contains("tempf_lag"))
  
dfsub_out=df_out
  
x_test_sub=dfsub_out %>% 
    select(contains("tempf_lag"))

set.seed(99)
t0=Sys.time()
post <- wbart(x_train_sub, y_train_sub, x_test_sub, ndpost=nmcmc, nskip=nburn)
t1=Sys.time()
t1-t0

post$ypred.test=post$yhat.test + post$sigma[-(1:nburn)]*matrix(rnorm(nmcmc*length(dfsub_out$tempf)),nrow=nmcmc)

#save(file="bart_post.Rdata",post)

dfsub_out$fit <- post$yhat.test.mean
#dfsub_out$lwr =  apply(post$yhat.test,2,quantile,0.025)
#dfsub_out$upr =  apply(post$yhat.test,2,quantile,0.975)

dfsub_out$lwr =  apply(post$ypred.test,2,quantile,0.025)
dfsub_out$upr =  apply(post$ypred.test,2,quantile,0.975)

#plot(dfsub_out$pm25,dfsub_out$fit)
#abline(a=0,b=1,col="red")

metrics_tempf(dfsub_out,post)
#post_tempf=post

save(dfsub_out, file = 'BART_temp_Forecasts.RData')

######################################################################
######################################################################

### BART for Wind speed ###

metrics_windspeedmph=function(tab,post){
 out=tab %>% summarise(mspe=mean((windspeedmph-fit)^2),
  crps=mean(crps_sample(windspeedmph,t(post$ypred.test))),
  cp=mean((windspeedmph>lwr)*(windspeedmph<upr)),
  is=0.05/2*mean((upr-lwr)+2*(lwr-windspeedmph)*(windspeedmph<lwr)/.05+ 
 2*(windspeedmph-upr)*(windspeedmph>upr)/.05),
 is2=mean(interval_score(true_values =windspeedmph, lower=lwr, upper=upr, interval_range = 95)))
 out
}

nmcmc=2000
nburn=1000

dfsub=df_in

y_train_sub=dfsub$windspeedmph

x_train_sub=dfsub %>% 
    select(contains("windspeedmph_lag"))
  
dfsub_out=df_out
  
x_test_sub=dfsub_out %>% 
    select(contains("windspeedmph_lag"))

set.seed(99)
t2=Sys.time()
post <- wbart(x_train_sub, y_train_sub, x_test_sub, ndpost=nmcmc, nskip=nburn)
t3=Sys.time()
t3-t2

post$ypred.test=post$yhat.test + post$sigma[-(1:nburn)]*matrix(rnorm(nmcmc*length(dfsub_out$windspeedmph)),nrow=nmcmc)

#save(file="bart_post.Rdata",post)

dfsub_out$fit <- post$yhat.test.mean
#dfsub_out$lwr =  apply(post$yhat.test,2,quantile,0.025)
#dfsub_out$upr =  apply(post$yhat.test,2,quantile,0.975)

dfsub_out$lwr =  apply(post$ypred.test,2,quantile,0.025)
dfsub_out$upr =  apply(post$ypred.test,2,quantile,0.975)

#plot(dfsub_out$pm25,dfsub_out$fit)
#abline(a=0,b=1,col="red")

metrics_windspeedmph(dfsub_out,post)
#post_windspeedmph=post

#save.image(file="BART_revision.Rdata")



