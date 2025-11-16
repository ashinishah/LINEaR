test_that("check_linearity updates assumptions correctly", {
  fit <- clean_lm(mpg ~ wt, data = mtcars)
  result <- check_linearity(fit)

  expect_true(is.list(result))
  expect_named(result, c("plot", "test_result", "linearity_ok", "object"))

  # Assumptions list updated
  expect_true("linearity" %in% names(result$object$assumptions))
  expect_true(is.logical(result$object$assumptions$linearity) || is.na(result$object$assumptions$linearity))
})

test_that("check_linearity handles non-linear data", {
  set.seed(123)
  df <- data.frame(x = 1:50, y = (1:50)^2 + rnorm(50, sd = 10))
  fit <- clean_lm(y ~ x, data = df)
  result <- check_linearity(fit)

  expect_true("linearity" %in% names(result$object$assumptions))
})

test_that("check_linearity errors on wrong input", {
  expect_error(check_linearity("not_a_model"))
})
