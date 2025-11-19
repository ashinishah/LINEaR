test_that("check_LINE returns expected structure when all checks run", {
  sim_data <- simulate_lm_data(n = 30, beta0 = 2, beta1 = 0.5, sigma = 1, seed = 123)
  fit <- clean_lm(y ~ x, data = sim_data)
  res <- check_LINE(fit)

  # Top-level structure
  expect_s3_class(res, "check_LINE")
  expect_true(all(c("linearity", "independence", "normality", "equalvariance", "object") %in% names(res)))

  # Each assumption result should be a list with assumption_ok, plot, object
  for (assumption in c("linearity","independence","normality","equalvariance")) {
    comp <- res[[assumption]]
    expect_type(comp, "list")
    expect_true(all(c("assumption_ok","plot","object") %in% names(comp)))
    expect_type(comp$assumption_ok, "logical")
    expect_s3_class(comp$plot, "ggplot")
    expect_s3_class(comp$object, "clean_lm")
  }

  # The updated object should be a clean_lm
  expect_s3_class(res$object, "clean_lm")
})

test_that("check_LINE can skip selected checks", {
  sim_data <- simulate_lm_data(n = 20, beta0 = 1, beta1 = 1, sigma = 1, seed = 42)
  fit <- clean_lm(y ~ x, data = sim_data)
  res <- check_LINE(fit, linearity = FALSE, independence = FALSE)

  # Skipped checks should be NULL
  expect_null(res$linearity)
  expect_null(res$independence)

  # Others should still be lists
  expect_type(res$normality, "list")
  expect_type(res$equalvariance, "list")
})

test_that("check_LINE errors on non-clean_lm input", {
  base_fit <- lm(y ~ x, data = simulate_lm_data(n = 20, beta0 = 1, beta1 = 2, sigma = 1, seed = 11))
  expect_error(check_LINE(base_fit), "must be a clean_lm object")
})

test_that("check_LINE integrates with summary.clean_lm", {
  sim_data <- simulate_lm_data(n = 25, beta0 = 1, beta1 = 1, sigma = 1, seed = 55)
  fit <- clean_lm(y ~ x, data = sim_data)
  res <- check_LINE(fit)

  out <- capture.output(summary(res$object))
  expect_true(any(grepl("Assumption Checks", out)))
  expect_true(any(grepl("linearity", out)))
  expect_true(any(grepl("independence", out)))
  expect_true(any(grepl("normality", out)))
  expect_true(any(grepl("equalvariance", out)))
})
