#########################################
#########################################
### Deep Echo-State Network Functions ###
#########################################
#########################################


#load libraries
library(tidyverse)
library(Matrix)
library(abind)



###############################
### Deep Echo-State Network ###
###############################


deep.esn.with.gap = function(y.train,
                    x.insamp,
                    x.outsamp,
                    y.test = NULL,
                    n.h,
                    nu,
                    pi.w, 
                    pi.win,
                    eta.w,
                    eta.win,
                    lambda.r,
                    alpha,
                    m,
                    iter,
                    trainLen,
                    future,
                    timegap = 0,
                    layers = 3,
                    reduced.units,
                    startvalues = NULL,
                    activation = 'tanh',
                    distribution = 'Normal',
                    logNorm = FALSE,
                    scale.factor,
                    scale.matrix,
                    parallel = F,
                    fork = F,
                    verbose = T)
{

  if(!parallel)
  {
    if(verbose)
    {
      prog.bar = txtProgressBar(min = 0, max = iter, style = 3)
    }
  }
  
  
  ###########################################
  ### Initial Conditions and Known Values ###
  ###########################################
  
  #Set training length and locations
  #cap.t = dim(y.train)[1]
  cap.t = trainLen - (m*future)
  locations = dim(y.train)[2]
  if(is.null(locations) | is.null(cap.t))
  {
    cap.t = length(y.train)
    locations = 1
  }
  
  #Get number of samples for weight matrices
  samp.w = list()
  samp.win = list()
  for(ell in 1:layers)
  {
    samp.w[[ell]] = n.h[ell] * n.h[ell]
    if(ell == 1)
    {
      n.x = (locations * (m+1)) + 1
      samp.win[[1]] = n.h[ell] * n.x
    } else {
      samp.win[[ell]] = n.h[ell] * (reduced.units+1)
    }
  }
  
  #Starting values of hidden units
  if(is.null(startvalues))
  {
    startvalues = list()
    for(ell in 1:layers)
    {
      startvalues[[ell]] = rep(0, n.h[ell])
    }
  }
  
  #Set the activation function
  if(activation == 'identity')
  {
    g.h = function(x)
    {
      return(x)
    } 
  } else if(activation == 'tanh') {
    g.h = function(x)
    {
      placeholder = tanh(x)
      return(placeholder)
    } 
  }
  
  #Set output array
  if(!parallel)
  {
    ensemb.mat = array(0, dim = c(locations, testLen, iter))
  }
  
  #########################
  ### Forecast Ensemble ###
  #########################
  set.seed(NULL)
  
  if(parallel)
  {
  
    #Specify Parallel clusters
    if(fork)
    {
      cl = parallel::makeForkCluster(getOption('cores')) 
    } else if(!fork)
    {
      cl = parallel::makeCluster(getOption('cores'))
    }
    
    #Activate clusters
    doParallel::registerDoParallel(cl)
    
    #Begin parallel iterations
    ensemb.mat = foreach::foreach(k = 1:iter,
                                  .combine = abind,
                                  .inorder = FALSE) %dopar%
      {
        set.seed(NULL)
        
        ##########################################
        ### Generate W and WIN weight matrices ###
        ##########################################
        W = list()
        WIN = list()
        lambda.w = c()
        for(ell in 1:layers)
        {
          #Set sparsity
          gam.w = purrr::rbernoulli(samp.w[[ell]], p = pi.w[ell])
          gam.win = purrr::rbernoulli(samp.win[[ell]], p = pi.win[ell])
          
          #Generate W
          if(distribution == 'Unif')
          {
            unif.w = runif(samp.w[[ell]], min = -eta.w[ell], max = eta.w[ell])
            W[[ell]] = Matrix::Matrix((gam.w == 1)*unif.w + (gam.w == 0)*0,
                                      nrow = n.h[ell], ncol = n.h[ell], sparse = T)
          } else if(distribution == 'Normal')
          {
            norm.w = rnorm(samp.w[[ell]], 0, 1)
            W[[ell]] = Matrix::Matrix((gam.w == 1)*norm.w + (gam.w == 0)*0,
                                      nrow = n.h[ell], ncol = n.h[ell], sparse = T)
          }
        
          #Generate W^in
          n.input = c(n.x, rep(reduced.units+1, (layers-1)))
          if(distribution == 'Unif')
          {
            unif.win = runif(samp.win[[ell]], min = -eta.win[ell], max = eta.win[ell])
            WIN[[ell]] = Matrix::Matrix((gam.win == 1)*unif.win + (gam.win == 0)*0,
                                 nrow = n.h[ell], ncol = n.input[ell], sparse = T)
          } else if(distribution == 'Normal')
          {
            norm.win = rnorm(samp.win[[ell]], 0, 1)
            WIN[[ell]] = Matrix::Matrix((gam.win == 1)*norm.win + (gam.win == 0)*0,
                                 nrow = n.h[ell], ncol = n.input[ell], sparse = T)
          }
          
          #Specify spectral radius
          lambda.w[ell] = max(abs(eigen(W[[ell]])$values))
    
        }
      
        
        ###############################
        ### Initialize Hidden Units ###
        ###############################
        h.prior = list()
        reservoir = list()
        h.forc.prior = list()
        forc.reservoir = list()
        Ident.Mat = diag(((layers-1)*reduced.units) + n.h[layers])
        
        for(ell in 1:layers)
        {
          h.prior[[ell]] = startvalues[[ell]]
          reservoir[[ell]] = matrix(NaN, nrow = n.h[ell], ncol = cap.t+timegap)
          h.forc.prior[[ell]] = rep(0, n.h[ell])
          forc.reservoir[[ell]] = matrix(NaN, nrow = n.h[ell], ncol = future)
        }
        
        
        ####################################
        ### Update Training Hidden Units ###
        ####################################
        input.data = list()
        input.data[[1]] = x.insamp
        output.data = list()
        output.data[[1]] = x.outsamp
        for(ell in 1:layers)
        {
          WIN.x.in.product = Matrix::tcrossprod(WIN[[ell]], input.data[[ell]])
          for(t in 1:cap.t)
          {
            omega = g.h(as.matrix((nu[ell]/lambda.w[ell]) * W[[ell]] %*% h.prior[[ell]] + WIN.x.in.product[,t]))
            h.prior[[ell]] = (1-alpha)*h.prior[[ell]] + alpha*omega
            reservoir[[ell]][,t] = as.numeric(h.prior[[ell]])
          } 
          
          
          if(timegap > 0)
          {
            for(gap in (cap.t+1):(cap.t + timegap))
            {
              omega = g.h(as.matrix((nu[ell]/lambda.w[ell]) * W[[ell]] %*% h.prior[[ell]] + WIN.x.in.product[,gap]))
              h.prior[[ell]] = (1-alpha)*h.prior[[ell]] + alpha*omega
              reservoir[[ell]][,gap] = as.numeric(h.prior[[ell]])
            }
          }
          
          h.forc.prior[[ell]] = reservoir[[ell]][,cap.t+timegap]
          WIN.x.out.product = Matrix::tcrossprod(WIN[[ell]], output.data[[ell]])
          for(fut in 1:future)
          {
            omega.hat = g.h(as.matrix((nu[ell]/lambda.w[ell]) * W[[ell]] %*% h.forc.prior[[ell]] + WIN.x.out.product[,fut]))
            h.forc.prior[[ell]] = (1-alpha)*h.forc.prior[[ell]] + alpha*omega.hat
            forc.reservoir[[ell]][,fut] = as.numeric(h.forc.prior[[ell]])
          } 
          
          
          #Dimension reduction to combine layers
          if(layers > 1)
          {
            placeholder = wql::eof(cbind(reservoir[[ell]], forc.reservoir[[ell]]), n = reduced.units, scale. = FALSE) 
            mean.pca = apply(placeholder$REOF[1:cap.t,], 2, mean)
            sd.pca = apply(placeholder$REOF[1:cap.t,], 2, sd)
            placeholder$REOF = (placeholder$REOF - matrix(mean.pca, nrow = cap.t+timegap+future, ncol = ncol(placeholder$REOF), byrow = TRUE)) / 
              matrix(sd.pca, nrow = cap.t+timegap+future, ncol = ncol(placeholder$REOF), byrow = TRUE)
            input.data[[ell+1]] = cbind(rep(1, (cap.t+timegap)), placeholder$REOF[1:(cap.t+timegap),1:reduced.units])
            output.data[[ell+1]] = cbind(rep(1,future), placeholder$REOF[(cap.t+timegap+1):(cap.t+timegap+future),1:reduced.units])
          } else {
            input.data[[ell+1]] = NULL
            output.data[[ell+1]] = NULL
          }
          
        } 
        

        
        ###################################
        ### Estimate Coefficient Matrix ###
        ###################################
        
        #Get dimension reduced data on same scale
        h.tild = matrix(NaN, nrow = cap.t, ncol = 1)
        for(ell in 2:layers)
        {
          h.tild = cbind(h.tild, g.h(input.data[[ell]][1:cap.t,-1]))
        }
        h.tild = h.tild[,-1]
        
        #Estimate coefficients
        final.design = rbind(reservoir[[layers]][,1:cap.t], t(h.tild))
        ridgeMat = lambda.r * Ident.Mat
        V = t(y.train[1:cap.t,]) %*% t(final.design) %*% solve(Matrix::tcrossprod(final.design, final.design) + ridgeMat)
        
        
        ###########################
        ### Calculate Forecasts ###
        ###########################
        
        #Get dimension reduced data on same scale
        h.tild.out = matrix(NaN, nrow = future, ncol = 1)
        for(ell in 2:layers)
        {
          h.tild.out = cbind(h.tild.out, g.h(output.data[[ell]][,-1]))
        }
        h.tild.out = h.tild.out[,-1]
        
        #Create output design matrix
        final.design.out = rbind(forc.reservoir[[layers]], t(h.tild.out))
        
        #Generate forecasts
        if(logNorm)
        {
          exp((scale.factor * (V %*% final.design.out)) + scale.matrix)
        } else {
          (scale.factor * (V %*% final.design.out)) + scale.matrix
        }
      } 
   
     
  } else {
    
    
    
    #Begin non-parallel iterations
    for(k in 1:iter)
      {
        set.seed(NULL)
        
        ##########################################
        ### Generate W and WIN weight matrices ###
        ##########################################
        W = list()
        WIN = list()
        lambda.w = c()
        for(ell in 1:layers)
        {
          #Set sparsity
          gam.w = purrr::rbernoulli(samp.w[[ell]], p = pi.w[ell])
          gam.win = purrr::rbernoulli(samp.win[[ell]], p = pi.win[ell])
          
          #Generate W
          if(distribution == 'Unif')
          {
            unif.w = runif(samp.w[[ell]], min = -eta.w[ell], max = eta.w[ell])
            W[[ell]] = Matrix::Matrix((gam.w == 1)*unif.w + (gam.w == 0)*0,
                                      nrow = n.h[ell], ncol = n.h[ell], sparse = T)
          } else if(distribution == 'Normal')
          {
            norm.w = rnorm(samp.w[[ell]], 0, 1)
            W[[ell]] = Matrix::Matrix((gam.w == 1)*norm.w + (gam.w == 0)*0,
                                      nrow = n.h[ell], ncol = n.h[ell], sparse = T)
          }
          
          #Generate W^in
          n.input = c(n.x, rep(reduced.units+1, (layers-1)))
          if(distribution == 'Unif')
          {
            unif.win = runif(samp.win[[ell]], min = -eta.win[ell], max = eta.win[ell])
            WIN[[ell]] = Matrix::Matrix((gam.win == 1)*unif.win + (gam.win == 0)*0,
                                        nrow = n.h[ell], ncol = n.input[ell], sparse = T)
          } else if(distribution == 'Normal')
          {
            norm.win = rnorm(samp.win[[ell]], 0, 1)
            WIN[[ell]] = Matrix::Matrix((gam.win == 1)*norm.win + (gam.win == 0)*0,
                                        nrow = n.h[ell], ncol = n.input[ell], sparse = T)
          }
          
          #Specify spectral radius
          lambda.w[ell] = max(abs(eigen(W[[ell]])$values))
          
        }
        
        
        ###############################
        ### Initialize Hidden Units ###
        ###############################
        h.prior = list()
        reservoir = list()
        h.forc.prior = list()
        forc.reservoir = list()
        Ident.Mat = diag(((layers-1)*reduced.units) + n.h[layers])
        
        for(ell in 1:layers)
        {
          h.prior[[ell]] = startvalues[[ell]]
          reservoir[[ell]] = matrix(NaN, nrow = n.h[ell], ncol = cap.t+timegap)
          h.forc.prior[[ell]] = rep(0, n.h[ell])
          forc.reservoir[[ell]] = matrix(NaN, nrow = n.h[ell], ncol = future)
        }
        
        
        ####################################
        ### Update Training Hidden Units ###
        ####################################
        input.data = list()
        input.data[[1]] = x.insamp 
        output.data = list()
        output.data[[1]] = x.outsamp

        #Run through layers
        for(ell in 1:layers)
        {
          WIN.x.in.product = Matrix::tcrossprod(WIN[[ell]], input.data[[ell]])
          for(t in 1:cap.t)
          {
            omega = g.h(as.matrix((nu[ell]/lambda.w[ell]) * W[[ell]] %*% h.prior[[ell]] + WIN.x.in.product[,t]))
            h.prior[[ell]] = (1-alpha)*h.prior[[ell]] + alpha*omega
            reservoir[[ell]][,t] = as.numeric(h.prior[[ell]])
          } 
          
          if(timegap > 0)
          {
            for(gap in (cap.t+1):(cap.t + timegap))
            {
              omega = g.h(as.matrix((nu[ell]/lambda.w[ell]) * W[[ell]] %*% h.prior[[ell]] + WIN.x.in.product[,gap]))
              h.prior[[ell]] = (1-alpha)*h.prior[[ell]] + alpha*omega
              reservoir[[ell]][,gap] = as.numeric(h.prior[[ell]])
            }
          }
          
          h.forc.prior[[ell]] = reservoir[[ell]][,cap.t+timegap]
          WIN.x.out.product = Matrix::tcrossprod(WIN[[ell]], output.data[[ell]])
          for(fut in 1:future)
          {
            omega.hat = g.h(as.matrix((nu[ell]/lambda.w[ell]) * W[[ell]] %*% h.forc.prior[[ell]] + WIN.x.out.product[,fut]))
            h.forc.prior[[ell]] = (1-alpha)*h.forc.prior[[ell]] + alpha*omega.hat
            forc.reservoir[[ell]][,fut] = as.numeric(h.forc.prior[[ell]])
          } 
          
          
          #Dimension reduction to combine layers
          if(layers > 1)
          {
            placeholder = wql::eof(cbind(reservoir[[ell]], forc.reservoir[[ell]]), n = reduced.units, scale. = FALSE) 
            mean.pca = apply(placeholder$REOF[1:cap.t,], 2, mean)
            sd.pca = apply(placeholder$REOF[1:cap.t,], 2, sd)
            placeholder$REOF = (placeholder$REOF - matrix(mean.pca, nrow = cap.t+timegap+future, ncol = ncol(placeholder$REOF), byrow = TRUE)) / 
              matrix(sd.pca, nrow = cap.t+timegap+future, ncol = ncol(placeholder$REOF), byrow = TRUE)
            input.data[[ell+1]] = cbind(rep(1, (cap.t+timegap)), placeholder$REOF[1:(cap.t+timegap),1:reduced.units])
            output.data[[ell+1]] = cbind(rep(1,future), placeholder$REOF[(cap.t+timegap+1):(cap.t+timegap+future),1:reduced.units])
          } else {
            input.data[[ell+1]] = NULL
            output.data[[ell+1]] = NULL
          }
          
        } 
        
        
        ###################################
        ### Estimate Coefficient Matrix ###
        ###################################
        
        #Get dimension reduced data on same scale
        h.tild = matrix(NaN, nrow = cap.t, ncol = 1)
        for(ell in 2:layers)
        {
          h.tild = cbind(h.tild, g.h(input.data[[ell]][1:cap.t,-1]))
        }
        h.tild = h.tild[,-1]
        
        #Estimate coefficients
        final.design = rbind(reservoir[[layers]][,1:cap.t], t(h.tild))
        ridgeMat = lambda.r * Ident.Mat
        V = t(y.train[1:cap.t,]) %*% t(final.design) %*% solve(Matrix::tcrossprod(final.design, final.design) + ridgeMat)
        
        
        
        ###########################
        ### Calculate Forecasts ###
        ###########################
        
        #Get dimension reduced data on same scale
        h.tild.out = matrix(NaN, nrow = future, ncol = 1)
        for(ell in 2:layers)
        {
          h.tild.out = cbind(h.tild.out, g.h(output.data[[ell]][,-1]))
        }
        h.tild.out = h.tild.out[,-1]
        
        #Create output design matrix
        final.design.out = rbind(forc.reservoir[[layers]], t(h.tild.out))
        
        #Generate forecasts
        if(logNorm)
        {
          ensemb.mat[,,k] = exp((scale.factor * (V %*% final.design.out)) + scale.matrix)
        } else {
          ensemb.mat[,,k] = (scale.factor * (V %*% final.design.out)) + scale.matrix
        }
        
        #update progress bare
        if(verbose)
        {
          setTxtProgressBar(prog.bar, k)
        }
        
      } 
    
  }
  
  
  ########################
  ### Finalize Results ###
  ########################
  
  #Close parallel clusters
  if(!parallel)
  {
    if(verbose)
    {
      close(prog.bar)
    }
  } else if(parallel) {
    parallel::stopCluster(cl)
  }
  
  #Calculate forecast mean
  if(!parallel)
  {
    if(testLen > 1)
    {
      forc.mean = sapply(1:locations, function(n) rowMeans(ensemb.mat[n,,]))
    } else if(locations > 1) {
      forc.mean = apply(ensemb.mat[,1,], 1, mean)
    } else {
      forc.mean = mean(ensemb.mat[1,1,])
    }
  } else if(parallel) {
    if(locations > 1 & future == 1)
    {
      forc.mean = apply(ensemb.mat, 1, mean)
    } else if(locations == 1 & future > 1){
      forc.mean = (sapply(1:future, function(x) mean(ensemb.mat[,seq(x, ncol(ensemb.mat), future)])))
    } else if(locations > 1 & future > 1) {
      forc.mean = t(sapply(1:future, function(x) rowMeans(ensemb.mat[,seq(x, ncol(ensemb.mat), future)])))
    } else if(locations == 1 & future == 1) {
      forc.mean = mean(as.numeric(ensemb.mat))
    } else {
      forc.mean = NULL
    }
  }
  
  #Calculate MSE
  if(!is.null(y.test))
  {
    MSE=sum((y.test-forc.mean)^2)/(locations*future)
  } else {
    MSE = NULL
  }
  
  #Compile results
  esn.output = list('predictions' = ensemb.mat,
                    'forecastmean' = forc.mean,
                    'MSE' = MSE)
  return(esn.output)
}

###########################
### Generate Input Data ###
###########################


gen.input.data = function(trainLen,
                          m,
                          tau,
                          yTrain,
                          rawData,
                          locations,
                          xTestIndex,
                          testLen)
{
  in.sample.len = trainLen - (m * tau)
  
  in.sample.x.raw = array(NA, dim = c(in.sample.len, m+1, locations))
  
  for(i in 1:in.sample.len)
  {
    in.sample.x.raw[i,,] = rawData[seq(i, (m*tau + i), by=tau), ]
  }
  
  #Scale in-sample x and y
  in.sample.y.raw = yTrain[(m*tau + 1):trainLen,]
  y.mean = mean(in.sample.y.raw)
  y.scale = sd(in.sample.y.raw)
  
  in.sample.y = (in.sample.y.raw - y.mean)/y.scale
  
  
  mean.train.x = mean(rawData[1:trainLen,])
  sd.train.x = sd(rawData[1:trainLen,])
  
  
  in.sample.x=(in.sample.x.raw - mean.train.x)/sd.train.x
  
  
  designMatrix = matrix(1,in.sample.len, (m + 1)*locations + 1)
  for(i in 1:in.sample.len){
    designMatrix[i,2:((m + 1)*locations + 1)] = as.vector(in.sample.x[i,,])
  }
  
  
  #Out-Sample
  out.sample.x.raw = array(NA, dim = c(testLen, m + 1, locations))
  for(i in 1:testLen)
  {
    out.sample.x.raw[i,,] = rawData[seq(xTestIndex[i]-(m*tau), xTestIndex[i], by=tau),]
  }
  
  
  #Scale out-sample x and y
  out.sample.x = (out.sample.x.raw - mean.train.x)/sd.train.x
  
  designMatrixOutSample = matrix(1, testLen, (m + 1)*locations + 1)
  for(i in 1:testLen)
  {
    designMatrixOutSample[i,2:((m + 1)*locations + 1)] = as.vector(out.sample.x[i,,])
  }
  
  
  
  #Additive scale matric
  addScaleMat = matrix(y.mean, locations, testLen)
  
  input.data.output = list('y.mean' = y.mean,
                           'y.scale' = y.scale,
                           'in.sample.y' = in.sample.y,
                           'in.sample.x' = in.sample.x,
                           'out.sample.x' = out.sample.x,
                           'in.sample.len' = in.sample.len,
                           'designMatrix' = designMatrix,
                           'designMatrixOutSample' = designMatrixOutSample,
                           'testLen' = testLen,
                           'addScaleMat' = addScaleMat)
  return(input.data.output)
}


#######################################################
### Generate Training, Testing, and Validation Sets ###
#######################################################


cttv = function(rawData, tau, trainLen, testLen, validLen = NULL, valid.flag = FALSE)
{
  #Create training and testing sets
  totlength = trainLen + testLen + tau
  yTrain = rawData[(tau+1):(trainLen+tau),]
  yTest = rawData[(trainLen+tau+1):totlength,]
  xTestIndex = seq((trainLen+1), (totlength-tau), 1)
  
  #Create valid sets
  if(valid.flag)
  {
    xValTestIndex=(trainLen+1-validLen):(trainLen)
    yValTestIndex=(trainLen+tau+1-validLen):(trainLen+tau)
    yValid = rawData[yValTestIndex,]
  } else {
    yValid = NULL
    xValTestIndex = NULL
  }
  
  #Return list
  output = list('yTrain' = yTrain,
                'yTest' = yTest,
                'yValid' = yValid,
                'xTestIndex' = xTestIndex,
                'xValTestIndex' = xValTestIndex)
  return(output)
}




