require('testthat')

context('Cross Validation')

test_that("forward step wise regression", {
    set.seed(1)
    data <- matrix(runif(25), 5, 5)
    x <- data[, 1:4]
    y <- data[, 5]
    lin_mod <- lm(y~x)




})

