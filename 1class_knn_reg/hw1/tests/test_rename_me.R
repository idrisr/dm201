source('../src/functions.R')

test_that('candidate columns are being properly split out', {
          make_col_name <- function(x) sapply(x, function(x) 
                                              paste('X', x, sep=''))
          df <- data.frame(matrix(rnorm(50), 5, 10), Y=1:5)

          x_cand_name <- split_x_y_cand(df, 'Y', make_col_name(c(1, 2, 3, 4)))
          expect_equivalent(x_cand_name, make_col_name(5:10))

          x_cand_name <- split_x_y_cand(df, 'Y', make_col_name(1:8))
          expect_equivalent(x_cand_name, make_col_name(9:10))

          x_cand_name <- split_x_y_cand(df, 'Y', make_col_name(c()))
          expect_equivalent(x_cand_name, make_col_name(1:10))

          x_cand_name <- split_x_y_cand(df, 'Y', make_col_name(NULL))
          expect_equivalent(x_cand_name, make_col_name(1:10))

          x_cand_name <- split_x_y_cand(df, 'Y', make_col_name(1:10))
          expect_equivalent(x_cand_name, make_col_name(c()))
})

build_df <- function() {
    set.seed(234)
    df <- data.frame(x=1:5, y=runif(5))
}


test_that('standard error is right', {
          df <- build_df()
          lm_model <- lm(y~x, df)
          # reg slope
          # TODO: make this code snippet a blog post. From reg to matrix form
          b <- with(df, sum((x-mean(x)) * (y-mean(y))) / sum((x-mean(x))^2))
          # reg intercept
          a <- with(df, mean(y) - b*mean(x))
          y_hat <- b*df$x + a
          residual <- y_hat - df$y
          expect_equal(mean(residual^2), standard_error_lm(df, lm_model))
})

build_df2 <- function() {
    set.seed(123234)
    n <- 1000
    y  <- 1:n
    x1 <- y + runif(n, 0, 1)
    x2 <- y + runif(n, 0, 10)
    x3 <- y + runif(n, 0, 100)
    x4 <- y + runif(n, 0, 1000)
    x5 <- y + runif(n, 0, 10000)
    df <- data.frame(x1, x2, x3, x4, x5, y)
}

test_that('best next regressor is returned', {
          # run lm with 1 regressor. See which one comes back with the best se.
          # that's our first regressor in forward wise
          # then try the next best, etc
          max_name <- function(x) names(which.max(x))
          min_name <- function(x) names(which.min(x))

          df <- build_df2()

          se <- next_best_regressor(df, 'y', NULL)
          expect_match(max_name(se), 'x5')
          expect_match(min_name(se), 'x1')

          # TODO: Need to come up with better test data to have expected subset
          # results
          # se <- next_best_regressor(df, 'y', c('x1', 'x2', 'x3'))
          # expect_match(max_name(se), 'x5') 
          # expect_match(min_name(se), 'x4')

          # se <- next_best_regressor(df, 'y', c('x1', 'x3'))
          # expect_match(max_name(se), 'x5')
          # expect_match(min_name(se), 'x2')

          se <- next_best_regressor(df, 'y', c('x1', 'x2', 'x3', 'x4'))
          expect_match('x5', max_name(se))
          expect_match('x5', min_name(se))
})
