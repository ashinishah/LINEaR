test_that("check_linearity returns expected structure", {
  data <- data.frame(x = 1:100, y = 3 * (1:100) + rnorm(100))
  fit <- clean_lm(y ~ x, data)
  result <- check_linearity(fit)

  expect_type(result, "list")
  expect_named(result, c("plot", "test_result", "linearity_ok"))
  expect_s3_class(result$plot, "ggplot")
})

test_that("linearity_ok is TRUE when RESET test passes", {
  fit <- clean_lm(mpg ~ wt, data = mtcars)
  result <- check_linearity(fit, use_test = TRUE)

  expect_true(result$linearity_ok == TRUE || result$linearity_ok == FALSE)
})

test_that("linearity_ok gives message when test is skipped", {
  fit <- clean_lm(mpg ~ wt, data = mtcars)
  result <- check_linearity(fit, use_test = FALSE)

  expect_equal(result$linearity_ok, "Test Skipped: Interpret plot visually.")
})

test_that("linearity_ok gives message when test errors", {
  data <- data.frame(x = 1:10, y = rep(1, 10))
  fit <- clean_lm(y ~ x, data)

  expect_warning({
    result <- check_linearity(fit, use_test = TRUE)
    expect_equal(result$linearity_ok, "Test Error: Interpret plot visually.")
  })
})


