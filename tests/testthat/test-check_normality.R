test_that("check_normality runs and returns expected structure", {
  fit <- lm(mpg ~ wt + hp, data = mtcars)
  clean_fit <- clean_lm(mpg ~ wt + hp, data = mtcars)

  # Run with defaults
  result <- check_normality(clean_fit)

  expect_type(result, "list")
  expect_true(all(c("plot", "test_result", "skew_kurt", "normality_ok", "object") %in% names(result)))
  expect_s3_class(result$plot, "ggplot")
  expect_s3_class(result$object, "clean_lm")

  # Run with skew/kurt enabled
  result2 <- check_normality(clean_fit, use_test = FALSE, use_skew_kurt = TRUE)
  expect_type(result2$skew_kurt, "list")
  expect_true(all(c("skewness", "kurtosis") %in% names(result2$skew_kurt)))
  expect_true(is.character(result2$normality_ok)) # skipped message
})

test_that("check_normality handles errors gracefully", {
  df <- data.frame(x = 1:2, y = c(1, 2))
  clean_fit_small <- clean_lm(y ~ x, data = df)

  result <- suppressWarnings(check_normality(clean_fit_small, use_test = TRUE))
  expect_true(is.character(result$normality_ok))
})

