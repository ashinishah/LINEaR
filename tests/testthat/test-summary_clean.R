test_that("summary_clean_lm errors on wrong input type", {
  fit <- lm(mpg ~ wt + hp, data = mtcars)
  expect_error(summary_clean_lm(fit),
               "Input must be a clean_lm object.")
})

test_that("summary_clean_lm prints summary and assumptions", {
  clean_fit <- clean_lm(mpg ~ wt + hp, data = mtcars)
  clean_fit <- check_LINE(clean_fit)$object

  expect_invisible(summary_clean_lm(clean_fit))

  # Assumptions should be present
  expect_true(all(c("linearity","independence","normality","equalvariance") %in% names(clean_fit$assumptions)))
})
