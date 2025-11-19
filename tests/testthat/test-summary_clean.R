test_that("summary.clean_lm prints regression summary with assumption info", {
  sim_data <- simulate_lm_data(n = 30, beta0 = 2, beta1 = 0.5, sigma = 1, seed = 123)
  fit <- clean_lm(y ~ x, data = sim_data)

  # Run summary before assumption checks
  out1 <- capture.output(summary(fit))
  expect_true(any(grepl("Summary of clean_lm model", out1)))
  expect_true(any(grepl("Assumption Checks", out1)))
  expect_true(any(grepl("Not run", out1)))  # should indicate skipped checks

  # Run assumption checks and then summary again
  fit_checked <- check_LINE(fit)
  out2 <- capture.output(summary(fit_checked))
  expect_true(any(grepl("linearity", out2)))
  expect_true(any(grepl("independence", out2)))
  expect_true(any(grepl("normality", out2)))
  expect_true(any(grepl("equalvariance", out2)))
})
