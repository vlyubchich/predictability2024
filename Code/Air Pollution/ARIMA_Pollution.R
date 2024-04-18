library(forecast)
library(lubridate)
library(dplyr)
library(ggplot2)
library(scales)
library(scoringRules)
library(scoringutils)

load('pm25_2009_2019_golden_horseshoe_sites.rda')
#dat<-read.csv('pm25_2009_2019_golden_horseshoe_sites.csv')
#dat<-dat[-2616,]

dates <- pm25_ts[[13]]
pm25_ts <- pm25_ts[-13]

dates_formatted <- ymd(paste(dates$year, dates$month, dates$day, sep = "-"))

n_ts <- length(pm25_ts)
results_lower <- vector("list", n_ts)
results_lower <- lapply(results_lower, FUN = function(x) {
  y <- as.data.frame(matrix(data = NA, nrow = 355, ncol = 10))
  names(y) <- paste0("p", 1:10)
  y
} )
names(results_lower) <- names(pm25_ts)
results_mean <- results_upper <- results_lower
# 95% parametric normal quantile for setting up lower/upper
q <- qnorm(0.975)

for(m in 1:n_ts) {
  x <- pm25_ts[[m]]
  train <- x[dates_formatted < ymd("2019-01-01")]  # update train
  # fit only on the training data
  fit <- auto.arima(train, 
                    max.p = 10,
                    max.q = 10,
                    max.P = 10,
                    max.Q = 10,
                    max.d = 5,
                    seasonal = FALSE,
                    ic = 'aicc')
  
  for(j in 1:355) {
    new_dat <- x[dates_formatted >= (ymd("2009-01-01") + j - 1) &
                   dates_formatted < (ymd("2019-01-01") + j - 1)]
    fit2 <- Arima(y = new_dat, model = fit)
    fore <- predict(fit2, n.ahead = 10, se.fit = TRUE,
                    level = 0.95)
    results_lower[[m]][j, ] <- unlist(fore$pred) - q * unlist(fore$se)
    results_upper[[m]][j, ] <- unlist(fore$pred) + q * unlist(fore$se)
    results_mean[[m]][j, ] <- unlist(fore$pred)
  }
}

results_ARIMA <- vector("list", 3)
results_ARIMA[[1]] <- results_mean
results_ARIMA[[2]] <- results_lower
results_ARIMA[[3]] <- results_upper
