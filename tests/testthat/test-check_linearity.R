test_that("check_linearity returns expected structure", {
  sim_data <- simulate_lm_data(n = 30, beta0 = 2, beta1 = 0.5, sigma = 1, seed = 123)
  fit <- clean_lm(y ~ x, data = sim_data)
  res <- check_linearity(fit)

  # Top-level structure
  expect_s3_class(res, "check_linearity")
  expect_s3_class(res, "check_assumption")
  expect_true(all(c("plot","test_result","assumption_ok","message","object") %in% names(res)))

  # Component types
  expect_s3_class(res$plot, "ggplot")
  expect_true(is.logical(res$assumption_ok) || is.na(res$assumption_ok))
  expect_true(is.character(res$message))
  expect_s3_class(res$object, "clean_lm")
})

test_that("check_linearity updates clean_lm object assumptions", {
  sim_data <- simulate_lm_data(n = 20, beta0 = 1, beta1 = 1, sigma = 1, seed = 42)
  fit <- clean_lm(y ~ x, data = sim_data)
  res <- check_linearity(fit)

  expect_true("linearity" %in% names(res$object$assumptions))
  expect_true("linearity" %in% names(res$object$assumptions_msgs))
})

test_that("check_linearity can skip RESET test deterministically", {
  sim_data <- simulate_lm_data(n = 25, beta0 = 0, beta1 = 1, sigma = 1, seed = 99)
  fit <- clean_lm(y ~ x, data = sim_data)
  res <- check_linearity(fit, use_test = FALSE)

  expect_true(is.na(res$assumption_ok))
  expect_match(res$message, "Test skipped")
  expect_null(res$test_result)
})

test_that("check_linearity errors on non-clean_lm input", {
  base_fit <- lm(y ~ x, data = simulate_lm_data(n = 20, beta0 = 1, beta1 = 2, sigma = 1, seed = 11))
  expect_error(check_linearity(base_fit), "must be a clean_lm object")
})

test_that("check_linearity handles RESET test output consistently", {
  sim_data <- simulate_lm_data(n = 30, beta0 = 1, beta1 = 1, sigma = 1, seed = 55)
  fit <- clean_lm(y ~ x, data = sim_data)
  res <- check_linearity(fit)

  # Regardless of p-value, assumption_ok should be logical scalar or NA
  expect_length(res$assumption_ok, 1)
  expect_true(is.logical(res$assumption_ok) || is.na(res$assumption_ok))
  expect_true(nchar(res$message) > 0)
})
