
stepwise.lm <- function(data, Xi_name, Yi_name) {
    # TODO: expand comment
    Xi_cand <- names(data)[which(!(names(data) %in% c(Xi_name, Yi_name)))]
    for (x in Xi_cand){
        x_form <- paste(c(Xi_name, x), collapse='+')
        form <- as.formula(sprintf('%s~%s', Yi_name, x_form))
        next.lm <- lm(formula = form, data = data)
    }
    return(TRUE)
}

split_x_y_cand <- function(data, Xi_name, Yi_name) {
    # takes dataframe object, list of X names already included in model, list of
    # Y names already included in model, and returns all other column names that
    # are candidates to be next included column in model
    Xi_cand <- names(data)[which(!(names(data) %in% c(Xi_name, Yi_name)))]
    return(Xi_cand)
}
