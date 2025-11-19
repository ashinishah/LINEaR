#' Simulate Linear Regression Data
#'
#' Generates a dataset with a linear relationship between predictor(s)
#' and response, useful for examples, vignettes, and testing.
#'
#' @param n Number of observations. Default is 100.
#' @param beta0 Intercept term. Default is 0.
#' @param beta1 Slope term. Default is 1.
#' @param sigma Standard deviation of the error term. Default is 1.
#' @param seed Optional random seed for reproducibility. Default is NULL.
#'
#' @return A data frame with two columns:
#' \item{x}{Predictor values drawn from Uniform(0, 10).}
#' \item{y}{Response values generated as \eqn{y = beta0 + beta1 * x + error}.}
#'
#' @details
#' This function is mostly for demonstration and testing purposes.
#' It generates a simple linear dataset with one predictor and one response.
#' The error term is drawn from a normal distribution with mean 0 and
#' standard deviation \code{sigma}.
#'
#' @examples
#' sim_data <- simulate_lm_data(n = 50, beta0 = 2, beta1 = 0.5, sigma = 2, seed = 123)
#' head(sim_data)
#'
#' @export
simulate_lm_data <- function(n = 100, beta0 = 0, beta1 = 1, sigma = 1, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  x <- runif(n, 0, 10)
  error <- rnorm(n, mean = 0, sd = sigma)
  y <- beta0 + beta1 * x + error

  data.frame(x = x, y = y)
}
