# Author: Idris Raja

require(leaps)
require(ggplot2)
setwd('/home/id/learning/dm201/1class_knn_reg/src/')
source('get_pros_data.R')

# xvalErr <- matrix(0, ncol(X), nbest)
best_subsets <- function(n, leap_out, data, Y_col) {
    # get X for particular subset
    Y_col_index <- 
    col_subset <- data[, c(leap_out$which[n, ])]
}

cross_val <- function(i_cross_val, n_cross_val, X_subset, Y_subset) {
    index <- 1:nrow(X)
    train_index <- which(index %% n_cross_val == (i_cross_val - 1))
    X_train <- X_subset[train_index, ]
    Y_train <- Y_subset[train_index, drop=FALSE]
    X_test  <- X_subset[-train_index, ]
    Y_test  <- Y_subset[-train_index, ]
    linMod <- lm(Y_train ~ X_train)
}

get_column_subset <- function(leap_out, subset_size, ith_subset) {
    return(column_index)
}

get_rows_test_cv <- function(row_index, n_cross_val, i_cross_val) {
    return(row_index_test)
}

data_scale <- get_data()
nbest <- 40
leap_out <- get_leaps(nbest, data_train, Y_col, X_col)
# (table(row.names(leap_out$which)))

# get which indices of leap_out$which use 2 covariates
Y_col <- 9

# get correct columns for data
X_subset <- best_subsets(9, leap_out, X)

# 1. run leaps to get correct columns
leap_out <- get_leaps(nbest, data_train, Y_col, X_col)

# 2. get which columns to use for subset
subset_cols <- get_column_subset(leap_out, subset_size, ith_subset)

# 3. get which rows to use for test (use all the others for train)
subset_test_rows <- get_rows_test_cv(row_index, n_cross_val, i_cross_val)

# 4. run regression
cv_lm <- lm(Y[subset_test_rows, ] ~ X[subset_test_rows, subset_test_cols])

# 5. use regression results to predict on test
cv_predictions <- predict(cv_lm, X[subset_train_rows, subset_test_cols])

# 6. calc error from prediction


# 7. measure error

n_cross_val <- 10
i_cross_val <- 2 
cross_val(i_cross_val, n_cross_val, X_subset, Y)
