rm(list=ls())
setwd('/home/id/learning/dm201/1class_knn_reg/hw1/')

source('src/load.R')
source('src/clean.R')
wine_scaled <- clean_data(load_data())

source('src/functions.R')

# Use forward stepwise regression on the Red Wine data set
f <- forward_stepwise_lm(wine_scaled, 'quality')
# Which variables are included in the optimum solution?
# fixed.acidity, alcohol, volatile.acidity

# use backward stepwise regression 
b <- backward_stepwise_lm(wine_scaled, 'quality')
