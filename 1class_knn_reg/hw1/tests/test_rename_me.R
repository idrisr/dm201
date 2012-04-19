source('../src/functions.R')

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

test_that('standard error is right', {
          set.seed(234)
          df <- data.frame(x=1:5, y=runif(5))
          lm_model <- lm(y~x, df)

          # reg slope
          b <- with(df, sum((x-mean(x)) * (y-mean(y))) / sum((x-mean(x))^2))

          # reg intercept
          a <- with(df, mean(y) - b*mean(x))

          y_hat <- b*df$x + a
          residual <- y_hat - df$y
          expect_equal(mean(residual^2), standard_error_lm(df, lm_model))
})
