rm(list = ls())

library(forecast)
library(lubridate)
library(dplyr)
library(ggplot2)
theme_set(theme_light())
library(scales)
library(scoringRules)
library(scoringutils)
library(tidyr)

load('Data/pm25_2009_2019_golden_horseshoe_sites.rda')
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
results_obs <- results_mean <- results_upper <- results_lower
# 95% parametric normal quantile for setting up lower/upper
q <- qnorm(0.975)

for(m in 1:n_ts) {
  print(paste(m, Sys.time()))
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
    results_obs[[m]][j, ] <- x[dates_formatted >= (ymd("2019-01-01") + j - 1) &
                                 dates_formatted < (ymd("2019-01-01") + j + 9)]
  }
}

# results_ARIMA <- vector("list", 3)
# results_ARIMA[[1]] <- results_mean
# results_ARIMA[[2]] <- results_lower
# results_ARIMA[[3]] <- results_upper

alpha = 0.05
# Calculate performance metrics by station and forecast horizon
PERF <- lapply(1:n_ts, function(m) {
  # errors
  e <- results_obs[[m]] - results_mean[[m]]
  # mean squared error
  mse <- apply(e^2, 2, mean)
  # coverage
  cvr <- apply((results_lower[[m]] <= results_obs[[m]]) & (results_obs[[m]] <= results_upper[[m]]), 2, mean)
  # interval score
  iscore <- (results_upper[[m]] - results_lower[[m]]) + 2 * (results_lower[[m]] - results_obs[[m]]) * as.numeric(results_obs[[m]] < results_lower[[m]]) / alpha + 2 * (results_obs[[m]] - results_upper[[m]]) * as.numeric(results_obs[[m]] > results_upper[[m]]) / alpha
  iscore <- apply(iscore, 2, mean)
  # combine results
  tibble(MSE = mse,
         Coverage = cvr,
         iscore = iscore,
         Location = m)
})

# Average across stations and horizons (Table 1 of the paper)
bind_rows(PERF) %>%
  apply(2, mean)

bind_rows(PERF) %>%
  apply(2, sd)


PERF_long <- bind_rows(PERF) %>%
  mutate(h = rep(1:10, times = length(PERF))) %>%
  pivot_longer(c(-h, -Location), names_to = "Metric", values_to = "Value") %>%
  mutate(Location = as.factor(Location),
         Metric = factor(Metric, levels = c("MSE", "Coverage", "iscore")))


PERF_long %>%
  filter(Metric %in% c("MSE", "Coverage")) %>%
  ggplot(aes(x = as.factor(h), y = Value)) +
  geom_boxplot() + #(lwd = 1.1, color = "gray70") +
  # geom_line(aes(x = h, y = Value, color = Location), lwd = 0.9) +
  facet_wrap(~Metric, scales = "free") +
  xlab("Forecasting horizon (days ahead)") +
  theme(legend.position = "none")

ggsave("images/HorizonError_ARIMA.png", width = 8, height = 4)


