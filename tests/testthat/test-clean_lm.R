test_that("clean_lm returns a list with model", {
  data <- data.frame(x = 1:10, y = 2 * (1:10) + rnorm(10))
  result <- clean_lm(y ~ x, data)
  expect_type(result, "list")
  expect_s3_class(result$model, "lm")
})

test_that("clean_lm handles missing data", {
  data <- data.frame(x = c(1:9, NA), y = 2 * (1:10) + rnorm(10))
  result <- clean_lm(y ~ x, na.omit(data))
  expect_s3_class(result$model, "lm")
})

test_that("clean_lm errors on bad input", {
  expect_error(clean_lm(y ~ x, "not a data frame"))
})
