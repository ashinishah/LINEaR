#' Check Normality Assumption
#'
#' Produces a Q-Q plot, optionally performs a Shapiro-Wilk test,
#' optionally computes skewness and kurtosis in order
#' to assess normality of residuals assumption in a fitted
#' linear regression model.
#'
#' @param object A \code{clean_lm} object. Must contain a \code{model} component
#'   that is a fitted \code{lm} object.
#' @param use_test Logical flag indicating whether to run Shapiro-Wilk test
#'   (\code{stats::shapiro.test}). Defaults to \code{TRUE}.
#' @param use_skew_kurt Logical flag indicating whether to compute skewness and kurtosis.
#'   Defaults to \code{FALSE}.
#'
#' @return An object of class \code{check_normality} and \code{check_assumption},
#'   with components:
#'   \item{plot}{A ggplot2 object showing Q-Q plot.}
#'   \item{test_result}{The result of the Shapiro-Wilk test, or error object if test failed.}
#'   \item{skew_kurt}{A list with skewness and kurtosis values, or \code{NULL} if not computed.}
#'   \item{assumption_ok}{Logical TRUE/FALSE if test succeeded, NA if skipped/errored.}
#'   \item{message}{Character string prompting interpretation.}
#'   \item{object}{The input \code{clean_lm} object with updated assumptions.}
#'
#' @export
check_normality <- function(object, use_test = TRUE, use_skew_kurt = FALSE) {
  if (!inherits(object, "clean_lm")) {
    stop("Input must be a clean_lm object.")
  }

  res <- stats::residuals(object$model)

  plot <- ggplot2::ggplot(data.frame(res = res), ggplot2::aes(sample = res)) +
    ggplot2::stat_qq(color = "green") +
    ggplot2::stat_qq_line(color = "darkgreen") +
    ggplot2::labs(title = "Q-Q Plot of Residuals") +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"))

  test_result <- NULL
  assumption_ok <- NA
  msg <- NULL

  if (isTRUE(use_test)) {
    test_result <- tryCatch(
      stats::shapiro.test(res),
      error = function(e) e
    )

    if (inherits(test_result, "error") || is.na(test_result$p.value)) {
      assumption_ok <- NA
      msg <- "Shapiro-Wilk test could not be computed. Please interpret the Q-Q plot visually."
    } else {
      assumption_ok <- test_result$p.value >= 0.05
      msg <- if (assumption_ok) {
        "Normality assumption appears reasonable."
      } else {
        "Normality assumption may not hold. Inspect the Q-Q plot."
      }
    }
  } else {
    assumption_ok <- NA
    msg <- "Test skipped. Please interpret the Q-Q plot visually."
  }

  skew_kurt <- NULL
  if (isTRUE(use_skew_kurt)) {
    skew <- mean((res - mean(res))^3) / sd(res)^3
    kurt <- mean((res - mean(res))^4) / sd(res)^4
    skew_kurt <- list(skewness = skew, kurtosis = kurt)
  }

  # update clean_lm object
  object$assumptions$normality <- assumption_ok
  object$assumptions_msgs$normality <- msg

  result <- list(
    plot = plot,
    test_result = test_result,
    skew_kurt = skew_kurt,
    assumption_ok = assumption_ok,
    message = msg,
    object = object
  )
  class(result) <- c("check_normality", "check_assumption")
  return(result)
}
