test_that("simulate_lm_data produces reproducible results", {
  d1 <- simulate_lm_data(n = 5, beta0 = 1, beta1 = 2, sigma = 1, seed = 123)
  d2 <- simulate_lm_data(n = 5, beta0 = 1, beta1 = 2, sigma = 1, seed = 123)

  expect_equal(d1, d2)
  expect_true(all(c("x", "y") %in% names(d1)))
})
