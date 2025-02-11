#Fourier + ARIMA for wind and temperature

# Clear memory
rm( list = ls() )

library( tsDyn )
library( lubridate )
library( TSA )
library( forecast )
library(scales)
library(scoringRules)
library(scoringutils)

# Read in the data
load( "WeatherStationCleaned_Interpolated.rda" )
head( station_int )
s_i1 <- as.data.frame( station_int )
t1 <- 1:nrow(s_i1)

d1time <- ymd_h(paste( s_i1$year, "-", 
                       s_i1$month, "-",
                       s_i1$day, "-",
                       s_i1$hour ) )
s_i1$Time <- d1time
s_i1$Idx <- t1

d1 <- s_i1[ s_i1$hour == 12,]

d1t_1 <- d1[ d1$Time <= ymd_h("2021-09-22-12"),  ] 
d1v_1 <- d1[ d1$Time > ymd_h("2021-09-22-12"), ]

# Peel of the variables of interest
d1t <- data.frame( Temp = d1t_1$tempf[ d1t_1$hour == 12 ], 
                  Wind = d1t_1$windspeedmph[ d1t_1$hour == 12 ],
                  Time = d1t_1$Time[ d1t_1$hour == 12 ],
                  Idx = d1t_1$Idx[ d1t_1$hour == 12 ])

d1v <- data.frame( Temp = d1v_1$tempf[ d1v_1$hour == 12 ], 
                   Wind = d1v_1$windspeedmph[ d1v_1$hour == 12 ],
                   Time = d1v_1$Time[ d1v_1$hour == 12 ],
                   Idx = d1v_1$Idx[ d1v_1$hour == 12 ])

y_T <- d1t$Temp
y_W <- d1t$Wind

p1_T <- periodogram( y_T )  # Used the periodogram to find the freq
p1_W <- periodogram( y_W )

p1_T_test1 <- data.frame( freq = 1/p1_T$freq, spec = p1_T$spec)
p1_T_test2 <- p1_T_test1[order(-p1_T_test1$spec),]

p1_W_test1 <- data.frame( freq = 1/p1_W$freq, spec = p1_W$spec)
p1_W_test2 <- p1_W_test1[order(-p1_W_test1$spec),]

p1_T_terms <- 20   # The top 20 frequencies were used.
p1_W_terms <- 20

#######################################################################
#
#  Temperature
#
#######################################################################

z1_T <- fourier(ts(c(d1$tempf,1), frequency=p1_T_test2$freq[ 1 ] ), K=1 )
for( i in 2:p1_T_terms ){
  z1_T <- cbind( z1_T, 
               fourier(ts(c(d1$tempf,1), frequency=p1_T_test2$freq[ i ] ), K=1 )
  )
}

z1_T_t <- z1_T[d1$Time < ymd_h("2021-09-22-12"), ]
fit1_T <- auto.arima(y_T, xreg = z1_T_t , seasonal=FALSE )
plot( y_T, type = "l" )
lines( fit1_T$fitted, col = "blue")


prediction1 <- matrix( 0, nrow = 530, ncol = 4 )

for( kk in 3:529 ){
  fit2 <- Arima( d1$tempf[1:kk] , xreg = z1_T[1:kk,], model = fit1_T)
  fore <- predict(fit2, n.ahead = 2, 
                  newxreg = z1_T[ (kk+1):(kk+2),],
                  se.fit = TRUE,
                  level = 0.95)
  fore_m <- unlist( fore$pred[1] )
  fore_u <- fore_m + 2*unlist( fore$se[1] )
  fore_l <- fore_m - 2*unlist( fore$se[1] )
  
  prediction1[ kk+1, 1 ] <- kk
  prediction1[ kk+1, 2 ] <- fore_m
  prediction1[ kk+1, 3 ] <- fore_l
  prediction1[ kk+1, 4 ] <- fore_u
  
}

colnames( prediction1 ) <- c("Time","Mean","Lower","Upper")
save(prediction1, file = "ForecastFourierTemperature.rda")



#######################################################################
#
#  Wind
#
#######################################################################

z1_W <- fourier(ts(d1$wind, frequency=p1_W_test2$freq[ 1 ] ), K=1 )
for( i in 2:p1_W_terms ){
  z1_W <- cbind( z1_W, 
                 fourier(ts(d1$wind, frequency=p1_W_test2$freq[ i ] ), K=1 )
  )
}

z1_W_t <- z1_W[d1$Time <= ymd_h("2021-09-22-12") ,]
fit1_W <- auto.arima(y_W, xreg = z1_W_t , seasonal=FALSE )

plot( y_W, type = "l" )
lines( fit1_W$fitted, col = "blue")

z1_W_p <- z1_W[ d1$Time > ymd_h("2021-09-22-12") , ]
y_W_p <- predict( fit1_W, newxreg = z1_W_p)

plot( 431:530, d1v$Wind, type = "l")
lines( y_W_p$pred, col = "blue")


prediction2 <- matrix( 0, nrow = 530, ncol = 4 )

for( kk in 3:528 ){
  fit2 <- Arima( d1$windspeedmph[1:kk] , xreg = z1_W[1:kk,], model = fit1_W)
  fore <- predict(fit2, n.ahead = 2, 
                  newxreg = z1_W[ (kk+1):(kk+2),],
                  se.fit = TRUE,
                  level = 0.95)
  fore_m <- unlist( fore$pred[1] )
  fore_u <- fore_m + 2*unlist( fore$se[1] )
  fore_l <- fore_m - 2*unlist( fore$se[1] )
  
  prediction2[ kk+1, 1 ] <- kk
  prediction2[ kk+1, 2 ] <- fore_m
  prediction2[ kk+1, 3 ] <- fore_l
  prediction2[ kk+1, 4 ] <- fore_u
  
}

colnames( prediction2 ) <- c("Time","Mean","Lower","Upper")
save(prediction2, file = "ForecastFourierWind.rda")


