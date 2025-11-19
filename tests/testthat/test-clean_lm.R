test_that("clean_lm returns expected structure", {
  fit <- clean_lm(mpg ~ wt, data = mtcars)

  # Class and structure
  expect_s3_class(fit, "clean_lm")
  expect_true(inherits(fit$model, "lm"))

  # Formula and data
  expect_equal(fit$formula, mpg ~ wt)
  expect_equal(nrow(fit$data), nrow(mtcars))

  # Residuals and fitted values
  expect_equal(length(fit$residuals), nrow(mtcars))
  expect_equal(length(fit$fitted), nrow(mtcars))

  # Assumptions list
  expect_named(fit$assumptions,
               c("linearity","independence","normality","equalvariance"))

  # Call object
  expect_true(is.call(fit$call))
})

test_that("clean_lm works with simple synthetic data", {
  df <- data.frame(x = 1:10, y = 2*(1:10) + rnorm(10))
  result <- clean_lm(y ~ x, df)

  expect_s3_class(result, "clean_lm")
  expect_equal(names(result),
               c("model","formula","data","residuals","fitted","assumptions","call"))
})
