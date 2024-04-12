#################################
#################################
### Forecasting Example - ESN ###
#################################
#################################


#clear enviroment
rm(list = ls())

#load libraries 
library(tidyverse)
library(BART)
library(tsDyn)
library(lubridate)
library(forecast)

#parallel libraries
library(foreach)
library(doParallel)

#necessary functions
source('functions.R')

#specify cores
options(cores = 4)

#load IC data
load('ICData.rda')
rawData = station_int[,c('tempf', 'windspeedmph')]

#change above lines to load in the Pollution data

######################
### Deep ESN Model ###
######################


#Set hyper-parameters
layers = 2
alpha = 0.64 
lambda.r = 0.1
last.n.h = 50
reduced.units = 25
first.n.h = last.n.h
m = 2
nu = c(0.2,0.8)
n.h = c(rep(first.n.h, layers-1), last.n.h)


#Assumed/fixed parameters - these are for the Indiana data and will change for the Pollution data
tau = 24
pi.w = rep(0.1, layers)
pi.win = rep(0.1, layers)
eta.w = rep(0.1, layers)
eta.win = rep(0.1, layers)
iterations = 100
locations = 2 
trainLen = 10308-tau
testLen = 24 
timegap = 0


#Generate training/testing/valid sets
sets = cttv(rawData = rawData,
            tau = tau,
            trainLen = trainLen+timegap,
            testLen = testLen)


#Generating input data
input.dat = gen.input.data(trainLen = trainLen+timegap,
                           m = m,
                           tau = tau,
                           yTrain = (sets$yTrain),
                           rawData = (rawData),
                           locations = locations,
                           xTestIndex = sets$xTestIndex,
                           testLen = testLen)
y.scale = input.dat$y.scale
y.train = input.dat$in.sample.y
designMatrix = input.dat$designMatrix
designMatrixOutSample = input.dat$designMatrixOutSample
addScaleMat = input.dat$addScaleMat


#Begin ESN forecasting
testing = deep.esn.with.gap(y.train = y.train,
                            x.insamp = designMatrix,
                            x.outsamp = designMatrixOutSample,
                            y.test = sets$yTest,
                            n.h = n.h,
                            nu = nu,
                            pi.w = pi.w, 
                            pi.win = pi.win,
                            eta.w = eta.w,
                            eta.win = eta.win,
                            lambda.r = lambda.r,
                            alpha = alpha,
                            m = m,
                            iter = iterations,
                            trainLen = trainLen,
                            future = testLen,
                            timegap = timegap,
                            layers = layers,
                            reduced.units = reduced.units,
                            startvalues = NULL,
                            activation = 'tanh',
                            distribution = 'Unif',
                            scale.factor = y.scale,
                            scale.matrix = addScaleMat,
                            logNorm = FALSE,
                            fork = FALSE,
                            parallel = FALSE,
                            verbose = TRUE)



testing$forecastmean




