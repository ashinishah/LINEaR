test_that("check_equalvariance returns logical flag and plot", {
  sim_data <- simulate_lm_data(n = 20, beta0 = 0, beta1 = 1, sigma = 1, seed = 4)
  fit <- clean_lm(y ~ x, data = sim_data)
  res <- check_equalvariance(fit)

  expect_type(res$assumption_ok, "logical")
  expect_s3_class(res$plot, "ggplot")
})
