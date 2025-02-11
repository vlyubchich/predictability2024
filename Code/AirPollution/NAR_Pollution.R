################################################
# This code implements Neural AR (NAR) 
################################################

rm(list=ls())

library(tidyverse)
library(keras)

load('pm25_2009_2019_golden_horseshoe_sites.rda')

#########################################
#creating arrays to store the results
#########################################
long.term.pred <- array(NA,dim=c(12,10,365))
long.term.lower <- array(NA,dim=c(12,10,365))
long.term.upper <- array(NA,dim=c(12,10,365))
y.test.mat <- array(NA,dim=c(12,10,365))

init.time <- proc.time()
for(loc in 1:(length(pm25_ts)-1)) #looping through all locations
{
  Y <- pm25_ts[[loc]] 
  
  year <- pm25_ts[[13]]$year
  
  
  lag_multiple <- function(x, n_vec){
    map(n_vec, lag, x = x) %>% 
      set_names(paste0("lag", n_vec)) %>% 
      as_tibble()
  }
  
  X <- lag_multiple(Y, 1:19)
  
  for(d_lag in 1:10)
  {
    ################################################
    # setting up train and test data sets
    ################################################
    y_train <- log(Y[year<=2018])
    x_train <- as.matrix(X[year<=2018,d_lag+0:9])
    
    no_na_index <- apply(!is.na(x_train),1,sum)==10&!is.na(y_train)
    
    x_train <- as.matrix(x_train[no_na_index,])
    y_train <- y_train[no_na_index]
    
    min_x_train <- min(x_train,na.rm=TRUE)
    max_x_train <- max(x_train,na.rm=TRUE)
    
    x_train <- (x_train-min_x_train)/(max_x_train-min_x_train) #standardizing train data
    
    y_test <- log(Y[year==2019])
    x_test <- as.matrix(X[year==2019,d_lag+0:9])
    
    x_test <- (x_test-min_x_train)/(max_x_train-min_x_train) #standardizing test data
    
    
    # Setting up tuning parameters 
    DropoutRate <- 0.3
    tau <- 20
    
    n_train <- nrow(x_train)
    
    keep_prob <- 1-DropoutRate
    n_train <- nrow(x_train)
    penalty_weight <- keep_prob/(tau* n_train) 
    penalty_intercept <- 1/(tau* n_train) 
    
    #Setting up drouput from the beginning
    dropout_1 <- layer_dropout(rate = DropoutRate)  
    dropout_2 <- layer_dropout(rate = DropoutRate) 
    dropout_3 <- layer_dropout(rate = DropoutRate) 
    
    ###############################################
    # Setting up neural network structure
    ###############################################
    inputs = layer_input(shape = list(ncol(x_train)))
    
    output <- inputs %>%
      layer_dense(units = 64, activation = 'relu',
                  kernel_regularizer=regularizer_l2(penalty_weight), 
                  bias_regularizer=regularizer_l2(penalty_intercept)) %>% 
      dropout_1(training=TRUE) %>%
      layer_dense(units = 32, activation = 'relu',
                  kernel_regularizer=regularizer_l2(penalty_weight), 
                  bias_regularizer=regularizer_l2(penalty_intercept)) %>%
      dropout_2(training=TRUE) %>%
      layer_dense(units = 16, activation = 'relu',
                  kernel_regularizer=regularizer_l2(penalty_weight), 
                  bias_regularizer=regularizer_l2(penalty_intercept)) %>%
      dropout_3(training=TRUE) %>%
      layer_dense(units = 1, activation = 'linear',
                  kernel_regularizer=regularizer_l2(penalty_weight), 
                  bias_regularizer=regularizer_l2(penalty_intercept))
    
    model <- keras_model(inputs, output)
    
    #summary(model)
    
    
    #specify optimizer, loss function, metrics
    model %>% compile(
      loss = 'mean_squared_error',
      optimizer = 'adam',
      metrics = c('mse')
    )
    
    #set up early stopping
    callback_specs=list(callback_early_stopping(monitor = "val_loss", min_delta = 0, patience = 30,
                                                verbose = 0, mode = "auto"),
                        callback_model_checkpoint(filepath='best_model.hdf5',save_freq='epoch' ,save_best_only = TRUE)
    )
    
    
    #running optimization
    history <- model %>% fit(
      x_train, y_train, 
      epochs = 300, batch_size = 500, 
      validation_split = 0.2,
      callbacks = callback_specs
    )
    
    
    #load the saved best model
    model_best = load_model_hdf5('best_model.hdf5',compile=FALSE)
    
    #quickly check the prediction results on training data
    #plot(model_best %>% predict(x_train),y_train)
    
    #prediction via mcdropout sampling
    mc.sample=1000
    testPredict=array(NA,dim=c(nrow(x_test),mc.sample))
    
    for(i in 1:mc.sample)
    {
      testPredict[,i]=model_best %>% predict(x_test)
    }
    
    
    #find 95% prediction interval
    mean_pred <- apply(testPredict,1,mean)
    sd_pred <- sqrt(apply(testPredict,1,var)+1/tau)
    
    PI <- rbind(mean_pred - 1.96*sd_pred,mean_pred + 1.96*sd_pred)
    
    #storing the results in the arrays
    long.term.pred[loc,d_lag,] <- exp(mean_pred)
    long.term.lower[loc,d_lag,] <- exp(PI[1,])
    long.term.upper[loc,d_lag,] <- exp(PI[2,])
    y.test.mat[loc,d_lag,] <- exp(y_test)
  }
  #save the results as a .Rdata file
  save(file='ANN_dropout.Rdata',long.term.pred,long.term.lower,long.term.upper,y.test.mat)
}
proc.time()-init.time

load('ANN_dropout.Rdata')

