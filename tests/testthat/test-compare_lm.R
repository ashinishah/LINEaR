test_that("compare_lm returns expected structure and correctness", {
  result <- compare_lm(mpg ~ wt + hp, data = mtcars, iterations = 5)

  # Result should be a list with two components
  expect_true(is.list(result))
  expect_named(result, c("correctness", "benchmark"))

  # Correctness should itself be a list with coef, residual, fitted
  expect_true(is.list(result$correctness))
  expect_named(result$correctness, c("coef", "residual", "fitted"))

  # Each correctness check should return TRUE (all.equal returns TRUE if equal)
  expect_true(result$correctness$coef)
  expect_true(result$correctness$residual)
  expect_true(result$correctness$fitted)

  # Benchmark should be a bench_mark object
  expect_s3_class(result$benchmark, "bench_mark")
})
