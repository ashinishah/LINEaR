test_that("check_LINE errors on wrong input type", {
  fit <- lm(mpg ~ wt + hp, data = mtcars)
  expect_error(check_LINE(fit),
               "Input must be a clean_lm object.")
})

test_that("check_LINE runs and returns expected structure", {
  clean_fit <- clean_lm(mpg ~ wt + hp, data = mtcars)

  result <- check_LINE(clean_fit)

  # Result should be a list with 5 components
  expect_true(is.list(result))
  expect_named(result, c("linearity", "independence", "normality", "equalvariance", "object"))

  # Each sub-result should itself be a list
  expect_true(is.list(result$linearity))
  expect_true(is.list(result$independence))
  expect_true(is.list(result$normality))
  expect_true(is.list(result$equalvariance))

  # Each sub-result should contain a plot and logical/message flag
  expect_s3_class(result$linearity$plot, "ggplot")
  expect_true(is.logical(result$linearity$linearity_ok) || is.character(result$linearity$linearity_ok))

  expect_s3_class(result$independence$plot, "ggplot")
  expect_true(is.logical(result$independence$independence_ok) || is.character(result$independence$independence_ok))

  expect_s3_class(result$normality$plot, "ggplot")
  expect_true(is.logical(result$normality$normality_ok) || is.character(result$normality$normality_ok))

  expect_s3_class(result$equalvariance$plot, "ggplot")
  expect_true(is.logical(result$equalvariance$equalvariance_ok) || is.character(result$equalvariance$equalvariance_ok))

  # Object should be a clean_lm with updated assumptions
  expect_s3_class(result$object, "clean_lm")
  expect_true(all(c("linearity", "independence", "normality", "equalvariance") %in% names(result$object$assumptions)))
})

test_that("check_LINE skips tests when use_tests = FALSE", {
  clean_fit <- clean_lm(mpg ~ wt + hp, data = mtcars)

  result <- check_LINE(clean_fit, use_tests = FALSE)

  # Each test_result should be NULL
  expect_null(result$linearity$test_result)
  expect_null(result$independence$test_result)
  expect_null(result$normality$test_result)
  expect_null(result$equalvariance$test_result)

  # Each *_ok should be a character message
  expect_type(result$linearity$linearity_ok, "character")
  expect_type(result$independence$independence_ok, "character")
  expect_type(result$normality$normality_ok, "character")
  expect_type(result$equalvariance$equalvariance_ok, "character")
})
