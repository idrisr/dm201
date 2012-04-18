
stepwise.lm <- function(data_, Xi_name, Yi_name) {
    # TODO: expand comment
    #regress Y ~ X,
    # Yi_name <- names(data_)[Yi_name]
    # Xi_name <- names(data_)[Xi_name]

    Xi_cand <- names(data_)[which(!(names(data_) %in% c(Xi_name, Yi_name)))]

    for (x in Xi_cand){
        next.lm <- lm(data_[, Yi_name] ~ data_[, Xi_name] + data_[, x])
        print(next.lm)
    }
    return(TRUE)
}
