library(tidyverse)
library(keras)

rm(list=ls())

# Read in the data
load( "ICData.rda" )

long.term.pred <- array(NA,dim=c(24,1,100))
long.term.lower <- array(NA,dim=c(24,1,100))
long.term.upper <- array(NA,dim=c(24,1,100))
y.test.mat <- array(NA,dim=c(24,1,100))

head( station_int )
s_i1 <- as.data.frame( station_int )

init.time <- proc.time()
for(target_hour in 1:24)
{
  # Peel of the variables of interest
  d1 <- data.frame( Temp = s_i1$tempf[ s_i1$hour == target_hour ], 
                    Wind = s_i1$windspeedmph[ s_i1$hour == target_hour ])
  t1 <- 1:nrow(d1)
  
  #Temp_diff <- d1$Temp[2:nrow(d1)-1]-d1$Temp[2:nrow(d1)]
  #Wind_diff <- d1$Wind[2:nrow(d1)-1]-d1$Wind[2:nrow(d1)]
  
  #T <- length(Temp_diff)
  #t <- 1:T
  
  
  #Y <- Temp_diff 
  Y <- d1$Temp
  
  
  lag_multiple <- function(x, n_vec){
    map(n_vec, lag, x = x) %>% 
      set_names(paste0("lag", n_vec)) %>% 
      as_tibble()
  }
  
  X <- lag_multiple(Y, 1:24)
  
  d_lag <- 1
  #for(d_lag in 1)
  {
    y_train <- Y[1:430]
    x_train <- as.matrix(X[1:430,d_lag+0:15])
    
    no_na_index <- apply(!is.na(x_train),1,sum)==16&!is.na(y_train)
    
    x_train <- as.matrix(x_train[no_na_index,])
    y_train <- y_train[no_na_index]
    
    min_x_train <- min(x_train,na.rm=TRUE)
    max_x_train <- max(x_train,na.rm=TRUE)
    
    x_train <- (x_train-min_x_train)/(max_x_train-min_x_train)
    
    y_test <- Y[431:530]
    x_test <- as.matrix(X[431:530,d_lag+0:15])
    
    x_test <- (x_test-min_x_train)/(max_x_train-min_x_train)
    
    # Normalize test data
    #col_means_train <- attr(train_df, "scaled:center") 
    #col_stddevs_train <- attr(train_df, "scaled:scale")
    #test_df <- scale(test_data, center = col_means_train, scale = col_stddevs_train)
    
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
    
    
    summary(model)
    
    
    #specify optimizer, loss function, metrics
    model %>% compile(
      loss = 'mean_squared_error',
      optimizer = 'adam',
      metrics = c('mse')
    )
    
    #set up early stopping
    callback_specs=list(callback_early_stopping(monitor = "val_loss", min_delta = 0, patience = 10,
                                                verbose = 0, mode = "auto"),
                        callback_model_checkpoint(filepath='best_model.hdf5',save_freq='epoch' ,save_best_only = TRUE)
    )
    
    
    #running optimization
    history <- model %>% fit(
      x_train, y_train, 
      epochs = 300, batch_size = 430, 
      validation_split = 0.2,
      callbacks = callback_specs
    )
    
    
    #load the saved best model
    model_best = load_model_hdf5('best_model.hdf5',compile=FALSE)
    
    plot(model_best %>% predict(x_train),y_train)
    #prediction via mcdrop sampling
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
    
    long.term.pred[target_hour,d_lag,] <- mean_pred
    long.term.lower[target_hour,d_lag,] <- PI[1,]
    long.term.upper[target_hour,d_lag,] <- PI[2,]
    y.test.mat[target_hour,d_lag,] <- y_test
  }
}  
proc.time()-init.time

save(file='ANN_Temp_dropout.Rdata',long.term.pred,long.term.lower,long.term.upper,y.test.mat)


