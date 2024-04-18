library(tidyverse)
library(lubridate)
library(BART)
library(GGally)
library(scoringutils)

metrics=function(tab){
 out=tab %>% summarise(mspe=mean((pm25-fit)^2),
  crps=mean(crps),
  cp=mean((pm25>lwr)*(pm25<upr)),
  is=0.05/2*mean((upr-lwr)+2*(lwr-pm25)*(pm25<lwr)/.05+ 
 2*(pm25-upr)*(pm25>upr)/.05),
 is2=mean(interval_score(true_values =pm25, lower=lwr, upper=upr, interval_range = 95)))
 out
}

load("pm25_2009_2019_golden_horseshoe_sites.rda")

n_data=length(pm25_ts)
dates <- pm25_ts[[n_data]]
dates_formatted <- ymd(paste(dates$year, dates$month, dates$day, sep = "-"))
n_ts=n_data-1

pm25_with_lag=Reduce("rbind",lapply(1:n_ts, function(i,min_lag=10,max_lag=30){
  df=data.frame(date=dates_formatted,id=rep(i,length(dates_formatted)),pm25=pm25_ts[[i]])
  for (lag in c(min_lag:max_lag,365)){
    df.lag=df %>% 
      select(date,id,pm25) %>% 
      mutate(date=date+lag)
    colnames(df.lag)[3]=paste0("lag",lag)
    df=df %>% 
      left_join(df.lag)
    }
    df
}))


### eda
# pm25_with_lag %>% 
#   dplyr::filter(date >= ymd("2019-01-10")) %>%
#   ggplot(aes(x=date,y=pm25,col=as.factor(id))) + 
#   geom_path()
# 
# pm25_with_lag %>% 
#   ggplot(aes(x=date,y=pm25,col=as.factor(id))) + 
#   facet_wrap(. ~ id, nrow=6) +
#   geom_path()

pm25_wide = pm25_with_lag %>%
  select(date,id,pm25) %>%
  pivot_wider(id_cols=date,names_from=id,values_from = pm25)

# ggpairs(pm25_wide,2:13)

pm25_with_lag=pm25_with_lag %>%
  dplyr::filter(across(everything(), ~ !is.na(.x))) %>%
  mutate(month=month(date),dow=wday(date),
         doy=yday(date),id=as.factor(id))

## training till Dec 31st
pm25_with_lag_in=pm25_with_lag %>%
  dplyr::filter(date < ymd("2019-01-01"))

pm25_with_lag_out=pm25_with_lag %>%
  dplyr::filter(date >= ymd("2019-01-10"))


### naive bart

#y_train=pm25_with_lag_in$pm25
#x_train=pm25_with_lag_in %>%
#  select(-c(date,pm25))
#x_test=pm25_with_lag_out %>%
#  select(-c(date,pm25))
# x_train_lag=as.matrix(x_train %>% 
# select(-c(id,month,dow,doy)))
# l=lm(y_train ~ x_train_lag)
# 
#x_train_factor=x_train %>% 
#  mutate(dow=as.factor(dow),month=as.factor(month))

#x_test_factor=x_test %>% 
#  mutate(dow=as.factor(dow),month=as.factor(month))

#set.seed(99)
#post <- wbart(x_train, y_train, x_test, ndpost=2000, nskip=1000)

#save(file="bart_post.Rdata",post)

#pred <- predict(post, x_test, ndpost=2000)

#y_test=pm25_with_lag_out$pm25
#y_pred=post$yhat.test.mean

#plot(y_pred,y_test,
#     ylim=range(c(y_pred,y_test)),xlim=range(c(y_pred,y_test)))
#abline(a=0,b=1,col="red")

#mse=mean((y_test - y_pred)^2)
#mse
#(cor(y_test,y_pred))^2

#save(file="naive_bart_post.Rdata",post)


## slim bart (by site, only using lag 10, 365, dow, month)

nmcmc=2000
nburn=1000
slimbart=Reduce('rbind',lapply(1:12,function(i) {
  
  dfsub=pm25_with_lag_in %>%
    dplyr::filter(as.numeric(id)==i) %>%
    select(date,id,pm25,lag10,lag11,lag12,lag13,lag14,lag15,lag16,lag17,lag18,lag19)
    
  y_train_sub=dfsub$pm25

  x_train_sub=dfsub %>% 
    select(-c(date,id,pm25))
  
  dfsub_out=pm25_with_lag_out %>%
    dplyr::filter(as.numeric(id)==i) %>%
    select(date,id,pm25,lag10,lag11,lag12,lag13,lag14,lag15,lag16,lag17,lag18,lag19)

  x_test_sub=dfsub_out %>% 
    select(-c(date,id,pm25))

set.seed(99)
t0=Sys.time()
post <- wbart(x_train_sub, y_train_sub, x_test_sub, ndpost=nmcmc, nskip=nburn)
t1=Sys.time()


post$ypred.test=post$yhat.test + post$sigma[-(1:nburn)]*matrix(rnorm(nmcmc*length(dfsub_out$pm25)),nrow=nmcmc)

dfsub_out$crps=mean(crps_sample((pm25_with_lag_out %>% dplyr::filter(id==i))$pm25,t(post$ypred.test)))

#save(file="bart_post.Rdata",post)

dfsub_out$fit <- post$yhat.test.mean
#dfsub_out$lwr =  apply(post$yhat.test,2,quantile,0.025)
#dfsub_out$upr =  apply(post$yhat.test,2,quantile,0.975)

dfsub_out$lwr =  apply(post$ypred.test,2,quantile,0.025)
dfsub_out$upr =  apply(post$ypred.test,2,quantile,0.975)

#plot(dfsub_out$pm25,dfsub_out$fit)
#abline(a=0,b=1,col="red")
dfsub_out$time=t1-t0

dfsub_out
}))

metrics(slimbart)

metricsmat=Reduce('rbind',lapply(1:12, function(i){
  metrics(slimbart %>% dplyr::filter(id==i))
  }))

colMeans(metricsmat)
apply(metricsmat,2,sd)

# slimbart %>% 
#   ggplot(aes(x=date,y=pm25,col=as.factor(id))) + 
#   geom_path() +
#   geom_path(data=slimbart,aes(x=date,y=fit),col="black") +
#   geom_path(data=slimbart,aes(x=date,y=lwr),col="grey") +
#   geom_path(data=slimbart,aes(x=date,y=upr),col="grey") +
#   facet_wrap(. ~ id, nrow=6)
# 
# save(file="slimbart_post.Rdata",slimbart)




