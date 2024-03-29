# TODO: ask online about R equivalent Python PEP 8 plugin
# TODO: ask how do people unit test / any test data mining applications? 

max_name <- function(x) names(which.max(x))
min_name <- function(x) names(which.min(x))

split_x_y_cand <- function(data_, Yi_name, Xi_name) {
    # takes data_frame object, list of X names already included in model, list
    # of Y names already included in model, and returns all other column names
    # that are candidates to be next included column in model
    Xi_cand <- names(data_)[which(!(names(data_) %in% c(Xi_name, Yi_name)))]
}

standard_error_lm <- function(data_, lm_model, test_i) {
    y_name <- names(lm_model$model)[1]
    pred <- predict.lm(lm_model, data_, se = TRUE)
    residual <- pred$fit[test_i] - data_[test_i, which(names(data_)==y_name)]
    # see http://en.wikipedia.org/wiki/Ordinary_least_squares#Estimation
    se <- mean(residual^2)
}

next_regressor <- function(data_, Yi_name, Xi_name) {
    x_cand_name <- split_x_y_cand(data_, Yi_name, Xi_name)
    se <- list()
    n_cv <- 10
    for(x_name in x_cand_name){
        x_form <- paste(c(Xi_name, x_name), collapse='+')
        form <- as.formula(sprintf('%s~%s', Yi_name, x_form))
        # print(form)
        se[x_name] <- cross_val_lm(data_, form, n_cv)
    }
    se <- unlist(se)
}

forward_stepwise_lm <- function(data_, Yi_name) {
    # think of a unit test for this
    Xi_name <- NULL
    Xi_cand_name <- split_x_y_cand(data_, Yi_name, Xi_name)

    best <- list()
    for(Xi_name in Xi_cand_name){
        se <- next_regressor(data_, Yi_name, Xi_name)
        next_regressor <- se[which.min(se)]
        Xi_name <- c(Xi_name, names(next_regressor))
        best[Xi_name] <- next_regressor
    }
    return(best)
}

next_regressor_back <- function(data_, Yi_name, Xi_name) {
    x_cand_name <- split_x_y_cand(data_, Yi_name, Xi_name)
    se <- list()
    n_cv <- 10
    for(x_name in Xi_name){
        Xi_name_wo <- Xi_name[which(!(Xi_name %in% x_name))]
        x_form <- paste(c(Xi_name_wo), collapse='+')
        form <- as.formula(sprintf('%s~%s', Yi_name, x_form))
        print(form)
        se[x_name] <- cross_val_lm(data_, form, n_cv)
    }
    se <- unlist(se)
}

backward_stepwise_lm <- function(data_, Yi_name) {
    #think of a unit test for this
    Xi_name <- names(data_)[!(names(data_) %in% Yi_name)]
    X_count_start <- length(Xi_name) - 1 
    worst <- list()

    for(Xi_name_wo in 1:X_count_start){
        se <- next_regressor_back(data_, Yi_name, Xi_name)
        jext_worst <- se[which.max(se)]
        Xi_name <- Xi_name[which(!Xi_name %in% names(next_worst))]
        worst[paste(Xi_name, collapse=' + ')] <- next_worst
    }
    return(worst)
}

cross_val_lm <- function(data_, formula_, n_cv) {
    # again, just running regressions. but this time on subsets of the data_
    se <- rep(0, n_cv)
    i <- 1:dim(data_)[1]
    for(i_cv in 1:n_cv){
        test_i <- which(i %% n_cv == (i_cv - 1))
        lm_cv <- lm(formula_, data_[-test_i, ])
        se[i_cv] <- standard_error_lm(data_, lm_cv, test_i)
    }
    return(sum(se))
}
