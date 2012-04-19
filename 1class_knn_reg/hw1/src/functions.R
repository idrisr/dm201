# TODO: ask online about R equivalent Python PEP 8 plugin

split_x_y_cand <- function(data, Yi_name, Xi_name) {
    # takes dataframe object, list of X names already included in model, list of
    # Y names already included in model, and returns all other column names that
    # are candidates to be next included column in model
    Xi_cand <- names(data)[which(!(names(data) %in% c(Xi_name, Yi_name)))]
}

standard_error_lm <- function(data, lm_model) {
    pred <- predict.lm(lm_model, data, se = TRUE)
    residual <- pred$fit - lm_model$model[, 1]
    # see http://en.wikipedia.org/wiki/Ordinary_least_squares#Estimation
    se <- mean(residual^2)
}

next_best_regressor <- function(data, Yi_name, Xi_name) {
    x_cand_name <- split_x_y_cand(data, Yi_name, Xi_name)
    se <- list()
    for(x_name in x_cand_name){
        x_form <- paste(c(Xi_name, x_name), collapse='+')
        form <- as.formula(sprintf('%s~%s', Yi_name, x_form))
        next_lm <- lm(formula = form, data = data)
        se[x_name] <- standard_error_lm(data, next_lm)
    }
    return(unlist(se))
}
