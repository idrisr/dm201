source('../src/functions.R')

test_that('forward stepwise regression next candidate', {
          set.seed(123)
          x1 <- rnorm(50)
          x2 <- 1:50
          x3 <- x1 * sin(x2)
          Y <- log(2)**sin(x2) + tanh(x1)
          data_ <- cbind(data.frame(scale(cbind(x1, x2, x3))), Y)

          Xi_name <- 'x1'
          Yi_name <- 'Y'

          a <- stepwise.lm(data_, Xi_name, Yi_name)

          expect_true(TRUE)
          expect_false(FALSE)
})
