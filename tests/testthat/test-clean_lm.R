test_that("clean_lm reproduces lm coefficients", {
  sim_data <- simulate_lm_data(n = 50, beta0 = 2, beta1 = 0.5, sigma = 1, seed = 123)
  fit_clean <- clean_lm(y ~ x, data = sim_data)
  fit_base  <- lm(y ~ x, data = sim_data)

  expect_equal(coef(fit_clean$model), coef(fit_base))
  expect_equal(residuals(fit_clean$model), residuals(fit_base))
})

test_that("clean_lm returns expected structure", {
  sim_data <- simulate_lm_data(n = 10, beta0 = 1, beta1 = 2, sigma = 1, seed = 42)
  fit <- clean_lm(y ~ x, data = sim_data)

  expect_s3_class(fit, "clean_lm")
  expect_true("model" %in% names(fit))
  expect_true("assumptions" %in% names(fit))
})
