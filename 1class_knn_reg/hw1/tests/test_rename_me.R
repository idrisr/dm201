source('../src/functions.R')

# test_that('forward stepwise regression next candidate', {
          # set.seed(123)
          # x1 <- rnorm(50)
          # x2 <- 1:50
          # x3 <- x1 * sin(x2)
          # Y <- log(2)**sin(x2) + tanh(x1)
          # data <- cbind(data.frame(scale(cbind(x1, x2, x3))), Y)

          # Xi_name <- 'x1'
          # Yi_name <- 'Y'

          # a <- stepwise.lm(data, Xi_name, Yi_name)

          # expect_false(a)
          # expect_false(FALSE)
# })

test_that('candidate columns are being properly split out', {
          make_col_name <- function(x) sapply(x, function(x) 
                                              paste('X', x, sep=''))
          df <- data.frame(matrix(rnorm(50), 5, 10), Y=1:5)

          x_cand_name <- split_x_y_cand(df, make_col_name(c(1, 2, 3, 4)), 'Y')
          expect_equivalent(x_cand_name, make_col_name(5:10))

          x_cand_name <- split_x_y_cand(df, make_col_name(1:8), 'Y')
          expect_equivalent(x_cand_name, make_col_name(9:10))

          x_cand_name <- split_x_y_cand(df, make_col_name(c()), 'Y')
          expect_equivalent(x_cand_name, make_col_name(1:10))

          x_cand_name <- split_x_y_cand(df, make_col_name(1:10), 'Y')
          expect_equivalent(x_cand_name, make_col_name(c()))
})
