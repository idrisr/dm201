rm(list=ls())
setwd('/home/id/learning/dm201/1class_knn_reg/hw1/')

source('src/load.R')
source('src/clean.R')
clean.data <- clean_data(load_data())

source('src/functions.R')
# Use forward stepwise regression on the Red Wine data set


# add 10 fold # cross validation. 

# What is the least squared error? 

# Which variables are included in the optimum solution?
