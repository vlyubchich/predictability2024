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

library(ranger)
Lags <- 1L:10L

load('Data/pm25_2009_2019_golden_horseshoe_sites.rda')
#dat<-read.csv('pm25_2009_2019_golden_horseshoe_sites.csv')
#dat<-dat[-2616,]

dates <- pm25_ts[[13]]
pm25_ts <- pm25_ts[-13]

dates_formatted <- ymd(paste(dates$year, dates$month, dates$day, sep = "-"))
doy <- yday(dates_formatted)

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
# q <- qnorm(0.975)

for(m in 1:n_ts) { # m = 1
  print(paste(m, Sys.time()))
  x <- pm25_ts[[m]]

  # Create lagged values
  lagged_values <- lapply(Lags, function(lag) dplyr::lag(x, lag))
  names(lagged_values) <- paste0("lag", Lags)
  X = data.frame(x, lagged_values, doy) #, t = 1:length(dates_formatted)

  # train <- X[dates_formatted < ymd("2019-01-01"),]  # update train
  # # fit only on the training data
  # fit <- auto.arima(train,
  #                   max.p = 10,
  #                   max.q = 10,
  #                   max.P = 10,
  #                   max.Q = 10,
  #                   max.d = 5,
  #                   seasonal = FALSE,
  #                   ic = 'aicc')

  for (j in 1:355) { # j = 1
    new_dat <- X[dates_formatted >= (ymd("2009-01-01") + j - 1) &
                   dates_formatted < (ymd("2019-01-01") + j - 1),] %>%
        na.omit()



    # fit2 <- Arima(y = new_dat, model = fit)
    fit2 <- ranger(x ~ ., data = new_dat)
    fit2_error <- new_dat$x - fit2$predictions
    # hist(fit2_error, br = 20)
    # use quantiles for prediction intervals:
    fit2_uq <- quantile(fit2_error, 0.975)
    fit2_lq <- quantile(fit2_error, 0.025)

    # Recursive predictions with RF
    data4pred <- new_dat[nrow(new_dat), -1]
    for (l in 1:max(Lags)) { # l = 1
      results_mean[[m]][j, l] <- predict(fit2, data = data4pred)$predictions
      results_lower[[m]][j, l] <- results_mean[[m]][j, l] + fit2_lq
      results_upper[[m]][j, l] <- results_mean[[m]][j, l] + fit2_uq

      # Update data used for predictions
      # data4pred$t <- data4pred$t + 1
      data4pred$doy <- data4pred$doy + 1
      if (data4pred$doy > 365) {
        data4pred$doy <- data4pred$doy - 365
      }
      data4pred[1, 2:10] <- data4pred[1, 1:9]
      data4pred[1, 1] <- results_mean[[m]][j, l]
    }


    # fore <- predict(fit2, n.ahead = 10, se.fit = TRUE,
                    # level = 0.95)
    # results_lower[[m]][j, ] <- unlist(fore$pred) - q * unlist(fore$se)
    # results_upper[[m]][j, ] <- unlist(fore$pred) + q * unlist(fore$se)
    # results_mean[[m]][j, ] <- unlist(fore$pred)
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
# MSE   Coverage     iscore   Location
# 21.1321288  0.9412911 22.3859268  6.5000000

bind_rows(PERF) %>%
  apply(2, sd)
# MSE   Coverage     iscore   Location
# 4.23952629 0.02023072 2.26728642 3.46652661

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

ggsave("images/HorizonError_RF.png", width = 8, height = 4)


# Calculate on the testing set:

testing <- bind_rows(results_obs)

psych::describe(testing[,1])
# vars       n mean   sd median trimmed  mad  min   max range skew kurtosis   se
# X1    1 4260 6.83 4.41   5.79    6.23 3.59 0.16 36.95 36.79 1.45     2.79 0.07
