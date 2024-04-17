
library(lubridate)
library(keras)
library(mvtnorm)
library(matrixStats)


load("pm25_2009_2019_golden_horseshoe_sites.rda")



tensorfyData.rnn = function(ts, forecastMonthsAhead, timestepsPerSample, trainInds, tuneInds, testInds)
{
  d_in = ncol(ts)

  n.train <- length(trainInds)
  n.tune <- length(tuneInds)
  n.test <- length(testInds)

  X.test.rnn <- array(NA, c(n.test + forecastMonthsAhead - 1, timestepsPerSample, d_in))
  Y.test.rnn <- array(NA, c(n.test + forecastMonthsAhead - 1, forecastMonthsAhead*ncol(ts)))
  
  X.test.rnn.scaled <- array(NA, c(n.test + forecastMonthsAhead - 1, timestepsPerSample, d_in))
  Y.test.rnn.scaled <- array(NA, c(n.test + forecastMonthsAhead - 1, forecastMonthsAhead*ncol(ts)))
  
  
  train <- ts[trainInds, ]
  if(ncol(ts) > 1)
  {
  	colMins.ts <- 0
  	colMaxs.ts <- apply(train, 2, max)
  }
  else
  {
  	colMins.ts <- 0
  	colMaxs.ts <- max(train)
  }
  colDiff.ts <- colMaxs.ts - colMins.ts
  
  
  
  rowCounter <- 1
  for(i in (min(testInds) - forecastMonthsAhead):(max(testInds) - 1))
  {
  	for(j in 1:timestepsPerSample)
  	{
  		X.test.rnn[rowCounter, j, 1:d_in] <- ts[i - timestepsPerSample + j, ]
  		
  		X.test.rnn.scaled[rowCounter, j, 1:d_in] <- (ts[i - timestepsPerSample + j, ] - colMins.ts)/(colDiff.ts)
  	}
  	
  	for(j in 1:forecastMonthsAhead)
  	{
  		if(i + j <= nrow(ts))
  		{
  			Y.test.rnn[rowCounter, ((j-1)*d_in + 1):(j*d_in)] <- ts[i + j, ]
  			Y.test.rnn.scaled[rowCounter, ((j-1)*d_in + 1):(j*d_in)] <- (ts[i + j, ] - colMins.ts)/colDiff.ts
  		}
  		
  	}
  	rowCounter <- rowCounter + 1
  }
  
  
  
  
  X.tune.rnn <- array(NA, c(n.tune + forecastMonthsAhead - 1, timestepsPerSample, d_in))
  Y.tune.rnn <- array(NA, c(n.tune + forecastMonthsAhead - 1, forecastMonthsAhead*ncol(ts)))
  
  X.tune.rnn.scaled <- array(NA, c(n.tune + forecastMonthsAhead - 1, timestepsPerSample, d_in))
  Y.tune.rnn.scaled <- array(NA, c(n.tune + forecastMonthsAhead - 1, forecastMonthsAhead*ncol(ts)))
  
  rowCounter <- 1
  for(i in (min(tuneInds) - forecastMonthsAhead):(max(tuneInds) - 1))
  {
  	for(j in 1:timestepsPerSample)
  	{
  		X.tune.rnn[rowCounter, j, 1:d_in] <- ts[i - timestepsPerSample + j, ]
  		
  		X.tune.rnn.scaled[rowCounter, j, 1:d_in] <- (ts[i - timestepsPerSample + j, ] - colMins.ts)/(colDiff.ts)
  	}
  	
  	for(j in 1:forecastMonthsAhead)
  	{
  		if(i + j <= max(tuneInds))
  		{
  			Y.tune.rnn[rowCounter, ((j-1)*d_in + 1):(j*d_in)] <- ts[i + j, ]
  			Y.tune.rnn.scaled[rowCounter, ((j-1)*d_in + 1):(j*d_in)] <- (ts[i + j, ] - colMins.ts)/colDiff.ts
  		}
  		
  	}
  	rowCounter <- rowCounter + 1
  }


  
  X.train.rnn <- array(NA, c(n.train + forecastMonthsAhead - 1, timestepsPerSample, d_in))
  Y.train.rnn <- array(NA, c(n.train + forecastMonthsAhead - 1, forecastMonthsAhead*ncol(ts)))
  
  X.train.rnn.scaled <- array(NA, c(n.train + forecastMonthsAhead - 1, timestepsPerSample, d_in))
  Y.train.rnn.scaled <- array(NA, c(n.train + forecastMonthsAhead - 1, forecastMonthsAhead*ncol(ts)))

  rowCounter <- 1
  for(i in (timestepsPerSample):(max(trainInds) - 1))
  {
  		for(j in 1:timestepsPerSample)
  		{
  			
  			X.train.rnn[rowCounter, j, 1:d_in] <- ts[i - timestepsPerSample + j, ]
  		
  			X.train.rnn.scaled[rowCounter, j, 1:d_in] <- (ts[i - timestepsPerSample + j, ] - colMins.ts)/(colDiff.ts)
  		}
  	
  		for(j in 1:forecastMonthsAhead)
  		{
  			if(i + j <= max(trainInds))
  			{
  				Y.train.rnn[rowCounter, ((j-1)*d_in + 1):(j*d_in)] <- ts[i + j, ]
  				Y.train.rnn.scaled[rowCounter, ((j-1)*d_in + 1):(j*d_in)] <- (ts[i + j, ] - colMins.ts)/colDiff.ts
  			}
  		
  		}
  		rowCounter <- rowCounter + 1
  }  

  return(list(X.train.rnn = X.train.rnn, Y.train.rnn = Y.train.rnn, X.tune.rnn = X.tune.rnn, Y.tune.rnn = Y.tune.rnn, X.test.rnn = X.test.rnn, Y.test.rnn = Y.test.rnn,
  X.train.rnn.scaled = X.train.rnn.scaled, Y.train.rnn.scaled = Y.train.rnn.scaled, X.tune.rnn.scaled = X.tune.rnn.scaled, Y.tune.rnn.scaled = Y.tune.rnn.scaled, X.test.rnn.scaled = X.test.rnn.scaled, Y.test.rnn.scaled = Y.test.rnn.scaled, colMins.ts = 0, colMaxs.ts  = colMaxs.ts))
}






dates <- pm25_ts[[13]]
pm25_ts <- pm25_ts[-13]


dates_formatted <- ymd(paste(dates$year, dates$month, dates$day, sep = "-"))

#pm25_ts <- list(pm25_ts[[1]])

nts <- length(pm25_ts)
lenTs <- length(pm25_ts[[1]])

df <- matrix(NA, lenTs, nts)
for(i in 1:nts)
{
  df[, i] <- pm25_ts[[i]]
}

T_ind <- which(dates_formatted == as.Date("1/1/2019", format  ="%d/%m/%Y"))
trainTuneX_Inds <- which(dates_formatted < as.Date("1/1/2019", format  ="%d/%m/%Y"))


lags <- 9 #lags = 0 corresponds to just using the current TS val to forecast
lookAhead <- 10
testInds <- T_ind:(length(dates_formatted))
trainInds <- 1:floor(0.8*(T_ind - 1))
tuneInds <- (max(trainInds) + 1):(T_ind - 1)

rnnData <- tensorfyData.rnn(df, lookAhead, lags, trainInds, tuneInds, testInds)


X.train <- rnnData$X.train.rnn
Y.train <- rnnData$Y.train.rnn

X.tune <- rnnData$X.tune.rnn
Y.tune <- rnnData$Y.tune.rnn

X.test <- rnnData$X.test.rnn
Y.test <- rnnData$Y.test.rnn


X.train.scaled <- rnnData$X.train.rnn.scaled
Y.train.scaled <- rnnData$Y.train.rnn.scaled

X.tune.scaled <- rnnData$X.tune.rnn.scaled
Y.tune.scaled <- rnnData$Y.tune.rnn.scaled

X.test.scaled <- rnnData$X.test.rnn.scaled
Y.test.scaled <- rnnData$Y.test.rnn.scaled


badInds <- c()
for(i in 1:(dim(X.train)[1]))
{
	if(any(is.na(X.train[i, , ])) | any(is.na(Y.train[i, ])))
	{
		badInds <- c(badInds, i)
	}
}

if(length(badInds) > 0)
{
	X.train <- X.train[-badInds, , , drop = FALSE]
	X.train.scaled <- X.train.scaled[-badInds, , , drop = FALSE]

	Y.train <- Y.train[-badInds, , drop = FALSE]
	Y.train.scaled <- Y.train.scaled[-badInds, , drop = FALSE]
}



badInds <- c()
for(i in 1:(dim(X.tune)[1]))
{
	if(any(is.na(X.tune[i, , ])) | any(is.na(Y.tune[i, ])))
	{
		badInds <- c(badInds, i)
	}
}

if(length(badInds) > 0)
{
	X.tune <- X.tune[-badInds, , , drop = FALSE]
	X.tune.scaled <- X.tune.scaled[-badInds, , , drop = FALSE]

	Y.tune <- Y.tune[-badInds, , drop = FALSE]
	Y.tune.scaled <- Y.tune.scaled[-badInds, , drop = FALSE]
}

d2 <- dim(X.train)[2]
d3 <- dim(X.train)[3]
X.train.temp <- matrix(NA, dim(X.train)[1], d2*d3)
X.train.scaled.temp <- matrix(NA, dim(X.train)[1], d2*d3)
X.tune.temp <- matrix(NA, dim(X.tune)[1], d2*d3)
X.tune.scaled.temp <- matrix(NA, dim(X.tune)[1], d2*d3)
X.test.temp <- matrix(NA, dim(X.test)[1], d2*d3)
X.test.scaled.temp <- matrix(NA, dim(X.test)[1], d2*d3)

for(i in 1:nrow(X.train.temp))
{
	for(j in 1:d2)
	{
		theseColInds <- ((j - 1)*d3 + 1):(j*d3)
		X.train.temp[i, theseColInds] <- X.train[i, j, ]
		X.train.scaled.temp[i, theseColInds] <- X.train.scaled[i, j, ]
	}
}
X.train <- X.train.temp
X.train.scaled <- X.train.scaled.temp

for(i in 1:nrow(X.tune.temp))
{
	for(j in 1:d2)
	{
		theseColInds <- ((j - 1)*d3 + 1):(j*d3)
		X.tune.temp[i, theseColInds] <- X.tune[i, j, ]
		X.tune.scaled.temp[i, theseColInds] <- X.tune.scaled[i, j, ]
	}
}
X.tune <- X.tune.temp
X.tune.scaled <- X.tune.scaled.temp

for(i in 1:nrow(X.test.temp))
{
	for(j in 1:d2)
	{
		theseColInds <- ((j - 1)*d3 + 1):(j*d3)
		X.test.temp[i, theseColInds] <- X.test[i, j, ]
		X.test.scaled.temp[i, theseColInds] <- X.test.scaled[i, j, ]
	}
}
X.test <- X.test.temp
X.test.scaled <- X.test.scaled.temp







numParticles <- 4
numOutputVar <- ncol(Y.train)
MDN_loss_ln <- function(y_true, y_pred) 
{
  n_kde <- numParticles * numOutputVar
  for (i in 1:numOutputVar) {
    assign(paste0("R_varianceMask_", i), matrix(0, 2 * n_kde + numParticles, numParticles))
    assign(paste0("R_varianceMask_", i), `[<-`(eval(as.name(paste0("R_varianceMask_", 
                                                                   i))), i = (n_kde + (i - 1) * numParticles + 1):(n_kde + 
                                                                                                                     i * numParticles), j = , value = diag(1, numParticles)))
    assign(paste0("varianceMask_", i), k_constant(get(paste0("R_varianceMask_", 
                                                             i)), shape = c(2 * n_kde + numParticles, numParticles)))
    assign(paste0("variableVariance_", i), k_square(k_exp(k_dot(y_pred, 
                                                                get(paste0("varianceMask_", i))))))
  }
  
  R_particle_weight_mask <- matrix(0, 2 * n_kde + numParticles, 
                                   numParticles)
  R_particle_weight_mask[(2 * n_kde + 1):(2 * n_kde + numParticles), ] <- diag(1, numParticles)
  particle_weight_mask <- k_constant(R_particle_weight_mask, 
                                     shape = c(2 * n_kde + numParticles, numParticles))
  particle_weights <- k_dot(y_pred, particle_weight_mask)
  max_particle_weights <- k_max(particle_weights, axis = 2L, keepdims = TRUE)
  weight_denominator <- max_particle_weights + k_log(k_sum(k_exp(particle_weights - max_particle_weights), axis = 2L, keepdims = TRUE))
  R_weight_denominator_rep <- matrix(1, 1, numParticles)
  weight_denominator_rep <- k_constant(R_weight_denominator_rep, 
                                       shape = c(1, numParticles))
  weight_denominator <- k_dot(weight_denominator, weight_denominator_rep)
  weight_numerator <- particle_weights
  R_knot_mask <- matrix(0, numParticles, numParticles)
  
  for (i in 1:numOutputVar) {
    assign(paste0("R_rep_", i), matrix(0, numOutputVar, numParticles))
    assign(paste0("R_rep_", i), `[<-`(eval(as.name(paste0("R_rep_", 
                                                          i))), i = i, j = , value = rep(1, numParticles)))
    assign(paste0("rep_", i), k_constant(get(paste0("R_rep_", 
                                                    i)), shape = c(numOutputVar, numParticles)))
    assign(paste0("true_", i), k_dot(y_true, get(paste0("rep_", 
                                                        i))))
    assign(paste0("R_mask_var_", i), matrix(0, 2 * n_kde + 
                                              numParticles, numParticles))
    for (ii in 1:numParticles) {
      assign(paste0("R_mask_var_", i), `[<-`(eval(as.name(paste0("R_mask_var_", 
                                                                 i))), i = ((i - 1) * numParticles + ii), j = ii, 
                                             value = 1))
    }
    assign(paste0("mask_var_", i), k_constant(get(paste0("R_mask_var_", 
                                                         i)), shape = c(2 * n_kde + numParticles, numParticles)))
    assign(paste0("pred_", i), k_dot(y_pred, get(paste0("mask_var_", 
                                                        i))))
    assign(paste0("logDensity_var_", i), -0.5 * k_log(2 * 
                                                        pi * get(paste0("variableVariance_", i))) - k_log(get(paste0("true_",i))) - 0.5 * 
             k_square(get(paste0("pred_", i)) - k_log(get(paste0("true_", 
                                                           i))))/get(paste0("variableVariance_", i)))
    if (i == 1) {
      logDensity <- weight_numerator - weight_denominator + 
        logDensity_var_1
      if (numOutputVar > 1) {
        logDensity <- k_expand_dims(logDensity, axis = 3L)
      }
    }
    else {
      assign(paste0("logDensity_var_", i), k_expand_dims(get(paste0("logDensity_var_", 
                                                                    i)), axis = 3L))
      logDensity <- k_concatenate(list(logDensity, get(paste0("logDensity_var_", 
                                                              i))), axis = 3L)
    }
  }
  
  if (numOutputVar > 1) {
    log_joint_density <- k_sum(logDensity, axis = 3L, keepdims = FALSE)
    max_log_joint_density <- k_max(log_joint_density, axis = 2L, keepdims = TRUE)
    neg_log_like_loss <- -1 * k_sum(max_log_joint_density + k_log(k_sum(exp(log_joint_density - max_log_joint_density), axis = 2L, keepdims = TRUE)))
  }
  else {
    max_logDensity <- k_max(logDensity, axis = 2L, keepdims = TRUE)
    neg_log_like_loss <- -1 * k_sum(max_logDensity + k_log(k_sum(k_exp(logDensity - max_logDensity), axis = 2L, keepdims = TRUE)))
  }
  
}






model_mdn <- keras_model_sequential()
model_mdn %>% layer_dense(units = 4096, activation = "relu",
                          input_shape = ncol(X.train.scaled))
model_mdn %>% layer_dense(units = 4096, activation = "relu")
model_mdn %>% layer_dense(units = 4096, activation = "relu")
model_mdn %>% layer_dense(units = 4096, activation = "relu")
model_mdn %>% layer_dense(units = numParticles*2*numOutputVar + numParticles)



patienceEarlyStopping = 50
patiencePlateau = 5
lrReduction = 0.5

callback <- list(keras::callback_reduce_lr_on_plateau(monitor = "val_loss", factor = lrReduction, patience = patiencePlateau), keras::callback_early_stopping(monitor = "val_loss", patience = patienceEarlyStopping),
                 keras::callback_model_checkpoint(filepath = paste0("mdn_weights.hdf5"), save_best_only = TRUE, save_weights_only = TRUE))

learningRate = 0.0001
model_mdn %>% compile(
  loss = MDN_loss_ln,
  optimizer = optimizer_adam(lr = learningRate)
)

time1 <- Sys.time()
history_mdn <- fit(model_mdn, x = X.train.scaled, y = Y.train.scaled, epochs = 1000, batch_size = 32, validation_data = list(X.tune.scaled, Y.tune.scaled), callbacks = callback)
time2 <- Sys.time()
print(time2 - time1)

load_model_weights_hdf5(model_mdn, paste0("mdn_weights.hdf5"))

outputs_test_mdn <- predict(model_mdn, X.test.scaled)


rMix <- function(particles, bandwidths, weights, numSamples)
{
  componentIndices <- sample(1:length(particles), numSamples, replace = TRUE, prob = weights)
  mu <- particles[componentIndices]
  if(class(mu) != "matrix")
  {
    mu <- matrix(mu, nrow = 1)
  }
  sigma <- bandwidths[componentIndices]
  if(class(sigma) != "matrix")
  {
    sigma <- matrix(sigma, nrow = 1)
  }
  x <- rlnorm(numSamples, mu, sigma)
  return(x)
}

pMix <- function(x, mu, sigma, weights)
{
	cdf <- 0
	for(i in 1:length(mu))
	{
		cdf <- cdf + weights[i]*plnorm(x, mu[i], sigma[i])
	}
	return(cdf)
}




#We need to have all the predictions in a 3D array of dim 12 x 10 x 355
numSamples <- 1000
long.term.pred <- array(NA, dim = c(nts, lookAhead, 355, numSamples))
long.term.pred.mu <- array(NA, dim = c(nts, lookAhead, 355, numParticles))
long.term.pred.sigma <- array(NA, dim = c(nts, lookAhead, 355, numParticles))
long.term.pred.weights <- array(NA, dim = c(nts, lookAhead, 355, numParticles))



for(i in 1:355)
{
  for(j in 1:lookAhead)
  {
      for(n in 1:nts)
      {
      	  print(c(i, j, n))
          muInds <- ( (j-1)*nts*numParticles + (n-1)*numParticles + 1):((j-1)*nts*numParticles + n*numParticles)
          sdInds <- lookAhead*nts*numParticles + (j-1)*nts*numParticles + ((n-1)*numParticles + 1):(n*numParticles)
          probInds <- 2*lookAhead*nts*numParticles + 1:numParticles
          thisMu <- outputs_test_mdn[i, muInds]
          thisSd <- exp(outputs_test_mdn[i, sdInds])
          thisProbs <- exp(outputs_test_mdn[i, probInds])
          thisProbs <- thisProbs/sum(thisProbs)
          mixSamp <- rMix(thisMu, thisSd, thisProbs, numSamples)
          long.term.pred[n, j, i, ] <- mixSamp*(rnnData$colMaxs.ts[n] - rnnData$colMins.ts)
          
          long.term.pred.mu[n, j, i, ] <- thisMu + log(rnnData$colMaxs.ts[n] - rnnData$colMins.ts)
          long.term.pred.sigma[n, j, i, ] <- thisSd
          long.term.pred.weights[n, j, i, ] <- thisProbs
      }
	}
  }



results <- list(samples = long.term.pred, mu = long.term.pred.mu, sigma = long.term.pred.sigma, p = long.term.pred.weights, test = Y.test)
saveRDS(results, "long.term.pred.RDS", version = 2)

