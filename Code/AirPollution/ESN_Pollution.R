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

#load pollution data
load('CanadaPM.RData')
rawData = t(canadaPM)

#need to run across 355 windows
#windows ranges from 1 to 355
wins = 355


######################
### Deep ESN Model ###
######################


#Set hyper-parameters
layers = 2
alpha = 0.03 
lambda.r = 0.001
last.n.h = 45
reduced.units = 15
first.n.h = 75
m = 1
nu = c(0.2,0.8)
n.h = c(rep(first.n.h, layers-1), last.n.h)


#Assumed/fixed parameters
tau = 10
pi.w = rep(0.1, layers)
pi.win = rep(0.1, layers)
eta.w = rep(0.1, layers)
eta.win = rep(0.1, layers)
iterations = 100
locations = 12 
trainLen = 3287+365-tau #adjust
timegap = wins-1
testLen = 10 #adjust


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




