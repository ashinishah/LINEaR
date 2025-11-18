test_that("check_independence errors on wrong input type", {
  # Passing a non-clean_lm object should error
  fit <- lm(mpg ~ wt + hp, data = mtcars)
  expect_error(check_independence(fit),
               "Input must be a clean_lm object.")
})

test_that("check_independence runs and returns expected structure", {
  # Use formula + data for clean_lm
  clean_fit <- clean_lm(mpg ~ wt + hp, data = mtcars)

  result <- check_independence(clean_fit)

  # Check returned list components
  expect_true(is.list(result))
  expect_named(result, c("plot", "test_result", "independence_ok", "object"))

  # Plot should be a ggplot object
  expect_s3_class(result$plot, "ggplot")

  # Test result should be an htest object if run
  expect_s3_class(result$test_result, "htest")

  # independence_ok should be logical or character
  expect_true(is.logical(result$independence_ok) || is.character(result$independence_ok))

  # Object should be a clean_lm with updated assumptions
  expect_s3_class(result$object, "clean_lm")
  expect_true("independence" %in% names(result$object$assumptions))
})

test_that("check_independence skips test when use_test = FALSE", {
  clean_fit <- clean_lm(mpg ~ wt + hp, data = mtcars)

  result <- check_independence(clean_fit, use_test = FALSE)

  # Test result should be NULL
  expect_null(result$test_result)

  # independence_ok should be a character message
  expect_type(result$independence_ok, "character")
  expect_match(result$independence_ok, "Test Skipped")
})
