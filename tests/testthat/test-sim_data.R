test_that("simulate_lm_data returns a data frame with correct structure", {
  sim <- simulate_lm_data(n = 50)

  # Should be a data frame with two columns
  expect_s3_class(sim, "data.frame")
  expect_named(sim, c("x", "y"))

  # Length should match n
  expect_equal(nrow(sim), 50)
})

test_that("simulate_lm_data respects parameters", {
  sim <- simulate_lm_data(n = 10, beta0 = 2, beta1 = 0.5, sigma = 0, seed = 123)

  # With sigma = 0, y should equal beta0 + beta1 * x exactly
  expect_equal(sim$y, 2 + 0.5 * sim$x)
})

test_that("simulate_lm_data is reproducible with seed", {
  sim1 <- simulate_lm_data(n = 20, seed = 42)
  sim2 <- simulate_lm_data(n = 20, seed = 42)

  # Same seed should give identical results
  expect_equal(sim1, sim2)
})

test_that("simulate_lm_data produces different results with different seeds", {
  sim1 <- simulate_lm_data(n = 20, seed = 1)
  sim2 <- simulate_lm_data(n = 20, seed = 2)

  # Different seeds should give different results
  expect_false(isTRUE(all.equal(sim1, sim2)))
})
