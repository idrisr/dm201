get_data <- function() {
    setwd('/home/id/learning/dm201/1class_knn_reg/data/')
    orig_data <- read.table(file='prostate', header=TRUE, row.names = 1)

    # restrict to training set
    data_train <- orig_data[which(orig_data$train), ]

    X <- data.frame(scale(data_train[, c(-9, -10)]))
    Y <- data_train[, 9, drop=FALSE]
    X_col <- names(X)
    Y_col <- names(Y)
    # rm(X)
    # rm(Y)

    # head(data_scale)
    cor(data_scale[, !(names(data_scale) %in% Y_col)])

    lin_mod <- lm(lpsa ~., data=data_scale)
    summary(lin_mod)
    data_scale <- data.frame(X, Y)
}

# get best input subsets *****************************************************
get_leaps <- function(nbest=40, data, y_col, x_col) {
    x <- data[, (names(data) %in% x_col)]
    y <- data[, (names(data) %in% y_col)]
    leap_out <- leaps(x, y, nbest = nbest)
}

which_subsets <- function(subset_size, leap_out) {
    # get indices of leaps result for particular feature size
    subset <- which(row.names(leap_out$which) == subset_size)
}
