test_that("check_independence returns expected structure", {
  sim_data <- simulate_lm_data(n = 30, beta0 = 1, beta1 = 0.5, sigma = 1, seed = 123)
  fit <- clean_lm(y ~ x, data = sim_data)
  res <- check_independence(fit)

  # Basic structure
  expect_type(res, "list")
  expect_true("assumption_ok" %in% names(res))
  expect_true("plot" %in% names(res))

  # Types
  expect_type(res$assumption_ok, "logical")
  expect_s3_class(res$plot, "ggplot")
})

test_that("check_independence works with different sample sizes", {
  sim_data_small <- simulate_lm_data(n = 10, beta0 = 0, beta1 = 1, sigma = 1, seed = 42)
  fit_small <- clean_lm(y ~ x, data = sim_data_small)
  res_small <- check_independence(fit_small)

  expect_type(res_small$assumption_ok, "logical")
  expect_s3_class(res_small$plot, "ggplot")

  sim_data_large <- simulate_lm_data(n = 200, beta0 = 0, beta1 = 1, sigma = 1, seed = 99)
  fit_large <- clean_lm(y ~ x, data = sim_data_large)
  res_large <- check_independence(fit_large)

  expect_type(res_large$assumption_ok, "logical")
  expect_s3_class(res_large$plot, "ggplot")
})

test_that("check_independence errors on non-clean_lm input", {
  base_fit <- lm(y ~ x, data = simulate_lm_data(n = 20, beta0 = 1, beta1 = 2, sigma = 1, seed = 11))
  expect_error(check_independence(base_fit), "must be a clean_lm object")
})

test_that("check_independence assumption_ok flag is logical scalar", {
  sim_data <- simulate_lm_data(n = 50, beta0 = 2, beta1 = 0.5, sigma = 1, seed = 321)
  fit <- clean_lm(y ~ x, data = sim_data)
  res <- check_independence(fit)

  expect_length(res$assumption_ok, 1)
  expect_true(is.logical(res$assumption_ok))
})
