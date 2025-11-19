test_that("check_LINE runs all assumption checks", {
  sim_data <- simulate_lm_data(n = 30, beta0 = 1, beta1 = 1, sigma = 1, seed = 99)
  fit <- clean_lm(y ~ x, data = sim_data)
  results <- check_LINE(fit)

  expect_type(results, "list")
  expect_true(all(c("linearity", "independence", "normality", "equalvariance") %in% names(results)))
})
