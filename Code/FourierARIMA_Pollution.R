#Fourier + ARIMA for Pollution Data

# Clear Everything
rm(list = ls())

library( lubridate )
library( TSA )
library( forecast )


# Read in the data
load("pm25_2009_2019_golden_horseshoe_sites.rda")

# Forecast horizon 
h1 <- 10
q1 <- qnorm(0.975)

# Get some data to work with
d2 <- pm25_ts[[13]]
n_data=length(pm25_ts)
dates <- pm25_ts[[n_data]]
dates_formatted <- ymd(paste(dates$year, dates$month, dates$day, sep = "-"))
n_ts=n_data-1
long.term.pred <- array( 0, dim= c( 12, 10, 355 ) )
long.term.PrC <- array( 0, dim = c( 12, 10, 3, 355 ) )

# Get the data prior to 2019
d3 <- d2[ d2$year < 2019, ]

for( jj in 1:n_ts ){
  d1 <- pm25_ts[[jj]]
  
  d1valid <- d1[ d2$year == 2019 ]
  d1train <- d1[ d2$year < 2019 ]
  d3$dsb <- 1:nrow( d3 )         # Create a time index
  
  d1train <- tsclean( d1train )
  

  
  # Get the periodogram
  p1 <- periodogram( d1train )
  f_test1 <- data.frame( freq = 1/p1$freq, spec = p1$spec)
  f_test2 <- f_test1[order(-f_test1$spec),]
  y <- d1train
  

  
  f_terms <- 20 
  z1 <- fourier(ts(y, frequency=f_test2$freq[ 1 ] ), K=1 )
  for( i in 2:f_terms ){
    z1 <- cbind( z1, 
                 fourier(ts(y, frequency=f_test2$freq[ i ] ), K=1 )
                 )
  }
  
  fit1 <- auto.arima(y, xreg = z1 , seasonal=FALSE )
  
  Ntrain1 <- length( y )
  ynew1 <- d1
  z1p <- fourier(ts(ynew1, frequency=f_test2$freq[ 1 ] ), K=1 )
  for( i in 2:f_terms ){
    z1p <- cbind( z1p, 
                 fourier(ts(ynew1, frequency=f_test2$freq[ i ] ), K=1 )
    )
  }
  

  
  for( kk in 1:355 ){
    fit2 <- Arima( ynew1[1:( Ntrain1 + kk - 1)] , xreg = z1p[1:(Ntrain1 + kk-1),], model = fit1)
    fore <- predict(fit2, n.ahead = 10, 
                    newxreg = z1p[ ( Ntrain1 + kk ):( Ntrain1 + kk + 9 ),],
                    se.fit = TRUE,
                    level = 0.95)
    fore_m <- unlist( fore$pred )
    fore_u <- fore_m + q1*unlist( fore$se )
    fore_l <- fore_m -  q1*unlist( fore$se )
    
    long.term.PrC[ jj, , 1, kk ] <- fore_m
    long.term.PrC[ jj, , 2, kk ] <- fore_l
    long.term.PrC[ jj, , 3, kk ] <- fore_u
    
  }
  
}

results_Fourier <- list( long.term.PrC[ , , 1, ],
                         long.term.PrC[ , , 2, ],
                         long.term.PrC[ , , 3, ] )



save(file = "results_Fourier.RData", results_Fourier)







