library(lubridate)

load("ICData.rda")

dates_formatted <- as.Date(paste0(station_int[, "day"], "-", station_int[, "month"], "-", station_int[, "year"]), format = "%d-%m-%Y")

responseColNames <- c("tempf", "windspeedmph")
negInds <- which(station_int[, "tempf"] <= 0)
station_int[negInds, "tempf"] <- 0.001
negInds <- which(station_int[, "windspeedmph"] <= 0)
station_int[negInds, "windspeedmph"] <- 0.001
station_int_edit <- cbind(station_int[, responseColNames])

d_in <- 2*16
d_out <- length(responseColNames)


trainFromIndex <- which(station_int[, "year"] == 2020 & station_int[, "month"] == 10 & station_int[, "day"] == 28 & station_int[, "hour"] == 12)
trainToIndex <- which(station_int[, "year"] == 2021 & station_int[, "month"] == 9 & station_int[, "day"] == 22 & station_int[, "hour"] == 12)

predictFromIndex <- which(station_int[, "year"] == 2021 & station_int[, "month"] == 9 & station_int[, "day"] == 23 & station_int[, "hour"] == 12)
predictToIndex <- which(station_int[, "year"] == 2021 & station_int[, "month"] == 12 & station_int[, "day"] == 31 & station_int[, "hour"] == 12)




#FFNN wrangling
X_train <- matrix(NA, 330, d_in)
Y_train <- matrix(NA, 330, 2)

X_test <- matrix(NA, 100, d_in)
Y_test <- matrix(NA, 100, 2)

count = 0
for(i in trainFromIndex:trainToIndex)
{
  if(station_int[i, "hour"] == 12)
  {
    count = count + 1
    startDate <- as.Date(paste0(station_int[i, "day"], "-", station_int[i, "month"], "-", station_int[i, "year"]), format = "%d-%m-%Y") - 16
    endDate <- as.Date(paste0(station_int[i, "day"], "-", station_int[i, "month"], "-", station_int[i, "year"]), format = "%d-%m-%Y") - 1
    startDate_bits <- as.numeric(strsplit(as.character(startDate), "-")[[1]])
    endDate_bits <- as.numeric(strsplit(as.character(endDate), "-")[[1]])
    
    startInd <- which(station_int[, "year"] == startDate_bits[1] & station_int[, "month"] == startDate_bits[2] & station_int[, "day"] == startDate_bits[3] & station_int[, "hour"] == 12)
    endInd <- which(station_int[, "year"] == endDate_bits[1] & station_int[, "month"] == endDate_bits[2] & station_int[, "day"] == endDate_bits[3] & station_int[, "hour"] == 12)
    
    goodTimeInds <- which(station_int[startInd:endInd, "hour"] == 12)
    theseX <- station_int_edit[(startInd:endInd)[goodTimeInds], ]
    n <- nrow(theseX)
    
    for(j in 1:ncol(station_int_edit))
    {
      X_train[count, ((j - 1)*n + 1):(n*j)] <- theseX[, j]
    }
  
    Y_train[count, ] <- station_int_edit[endInd + 24, responseColNames]
  }
}

X_tune <- X_train[281:330, ]
Y_tune <- Y_train[281:330, ]
X_train <- X_train[1:280, ]
Y_train <- Y_train[1:280, ]

count = 0
for(i in predictFromIndex:predictToIndex)
{
  if(station_int[i, "hour"] == 12)
  {
    count = count + 1
    startDate <- as.Date(paste0(station_int[i, "day"], "-", station_int[i, "month"], "-", station_int[i, "year"]), format = "%d-%m-%Y") - 16
    endDate <- as.Date(paste0(station_int[i, "day"], "-", station_int[i, "month"], "-", station_int[i, "year"]), format = "%d-%m-%Y") - 1
    startDate_bits <- as.numeric(strsplit(as.character(startDate), "-")[[1]])
    endDate_bits <- as.numeric(strsplit(as.character(endDate), "-")[[1]])
    
    startInd <- which(station_int[, "year"] == startDate_bits[1] & station_int[, "month"] == startDate_bits[2] & station_int[, "day"] == startDate_bits[3] & station_int[, "hour"] == 12)
    endInd <- which(station_int[, "year"] == endDate_bits[1] & station_int[, "month"] == endDate_bits[2] & station_int[, "day"] == endDate_bits[3] & station_int[, "hour"] == 12)
    goodTimeInds <- which(station_int[startInd:endInd, "hour"] == 12)
    theseX <- station_int_edit[(startInd:endInd)[goodTimeInds], ]
    n <- nrow(theseX)
    
     for(j in 1:ncol(station_int_edit))
     {
      	X_test[count, ((j - 1)*n + 1):(n*j)] <- theseX[, j]
     }
    
    Y_test[count, ] <- station_int_edit[endInd + 24, responseColNames]
    
  }
}




#scale the training data
colMins_X <- apply(X_train, 2, min) - 0.001
colMins_Y <- c(0, 0)
colMaxs_X <- apply(X_train, 2, max) + 0.001
colMaxs_Y <- apply(Y_train, 2, max)


X_train_scaled <- X_train
Y_train_scaled <- Y_train
X_tune_scaled <- X_tune
Y_tune_scaled <- Y_tune
X_test_scaled <- X_test
Y_test_scaled <- Y_test


for(i in 1:ncol(X_train))
{
  X_train_scaled[, i] <- (X_train[, i] - colMins_X[i])/(colMaxs_X[i] - colMins_X[i])
  X_tune_scaled[, i] <- (X_tune[, i] - colMins_X[i])/(colMaxs_X[i] - colMins_X[i])
  X_test_scaled[, i] <- (X_test[, i] - colMins_X[i])/(colMaxs_X[i] - colMins_X[i])
}

for(i in 1:ncol(Y_train))
{
  Y_train_scaled[, i] <- (Y_train[, i] - colMins_Y[i])/(colMaxs_Y[i] - colMins_Y[i])
  Y_tune_scaled[, i] <- (Y_tune[, i] - colMins_Y[i])/(colMaxs_Y[i] - colMins_Y[i])
  Y_test_scaled[, i] <- (Y_test[, i] - colMins_Y[i])/(colMaxs_Y[i] - colMins_Y[i])
}





library(keras)

numParticles <- 5
numOutputVar <- ncol(Y_train)
MDN_loss <- function(y_true, y_pred) 
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
                                                        pi * get(paste0("variableVariance_", i))) - 0.5 * 
             k_square(get(paste0("pred_", i)) - get(paste0("true_", 
                                                           i)))/get(paste0("variableVariance_", i)))
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
                          input_shape = ncol(X_train_scaled))
model_mdn %>% layer_dense(units = 4096, activation = "relu")
model_mdn %>% layer_dense(units = 4096, activation = "relu")
model_mdn %>% layer_dense(units = 4096, activation = "relu")
model_mdn %>% layer_dense(units = numParticles*2*numOutputVar + numParticles)



patienceEarlyStopping = 50
patiencePlateau = 5
lrReduction = 0.5

callback <- list(keras::callback_reduce_lr_on_plateau(monitor = "val_loss", factor = lrReduction, patience = patiencePlateau), keras::callback_early_stopping(monitor = "val_loss", patience = patienceEarlyStopping),
                 keras::callback_model_checkpoint(filepath = paste0(saveDir, "/", "FFNN_mdn_weights.hdf5"), save_best_only = TRUE, save_weights_only = TRUE))

learningRate = 0.000001
model_mdn %>% compile(
  loss = MDN_loss,
  optimizer = optimizer_adam(lr = learningRate)
)

time1 <- Sys.time()
history_mdn <- fit(model_mdn, x = X_train_scaled, y = Y_train_scaled, epochs = 1000, batch_size = 32, validation_data = list(X_tune_scaled, Y_tune_scaled), callbacks = callback)
time2 <- Sys.time()
print(time2 - time1)

load_model_weights_hdf5(model_mdn, paste0(saveDir, "/", "FFNN_mdn_weights.hdf5"))

outputs_test_mdn <- predict(model_mdn, X_test_scaled)




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
  x <- rnorm(numSamples, mu, sigma)
  return(x)
}

pMix <- function(x, mu, sigma, weights)
{
	cdf <- 0
	for(i in 1:length(mu))
	{
		cdf <- cdf + weights[i]*pnorm(x, mu[i], sigma[i])
	}
	return(cdf)
}




#We need to have all the predictions in a 3D array of dim 12 x 10 x 355
numSamples <- 1000
nts = 2
long.term.pred <- array(NA, dim = c(nts, 100, numSamples))
long.term.pred.mu <- array(NA, dim = c(nts, 100, numParticles))
long.term.pred.sigma <- array(NA, dim = c(nts, 100, numParticles))
long.term.pred.weights <- array(NA, dim = c(nts, 100, numParticles))

for(i in 1:100)
{
      for(n in 1:length(responseColNames))
      {
          muInds <- ((n-1)*numParticles + 1):(n*numParticles)
          sdInds <- nts*numParticles + ((n-1)*numParticles + 1):(n*numParticles)
          probInds <- 2*nts*numParticles + 1:numParticles
          thisMu <- outputs_test_mdn[i, muInds]
          thisSd <- exp(outputs_test_mdn[i, sdInds])
          thisProbs <- exp(outputs_test_mdn[i, probInds])
          thisProbs <- thisProbs/sum(thisProbs)
          mixSamp <- rMix(thisMu, thisSd, thisProbs, numSamples)
		  long.term.pred[n, i, ] <- mixSamp*(colMaxs_Y[n] - colMins_Y[n])
          
          long.term.pred.mu[n, i, ] <- thisMu*(colMaxs_Y[n] - colMins_Y[n])
          long.term.pred.sigma[n, i, ] <- thisSd*(colMaxs_Y[n] - colMins_Y[n])
          long.term.pred.weights[n, i, ] <- thisProbs 
      }
}



results <- list(samples = long.term.pred, mu = long.term.pred.mu, sigma = long.term.pred.sigma, p = long.term.pred.weights, test = Y_test)
saveRDS(results, "Weather_station_FFNN_N.RDS")
















