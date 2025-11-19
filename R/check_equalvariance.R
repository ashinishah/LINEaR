#' Check Equal Variance Assumption
#'
#' Produces a residuals vs. fitted values plot, optionally performs
#' the Breusch-Pagan test, to assess homoscedasticity (equal variance of residuals).
#'
#' @param object A \code{clean_lm} object. Must contain a \code{model} component
#'   that is a fitted \code{lm} object.
#' @param use_test Logical flag indicating whether to run Breusch-Pagan test
#'   (\code{lmtest::bptest}). Defaults to \code{TRUE}.
#'
#' @return An object of class \code{check_equalvariance} and \code{check_assumption},
#'   with components:
#'   \item{plot}{Residuals vs. fitted values plot (ggplot2).}
#'   \item{test_result}{Breusch-Pagan test result, or error object if test failed.}
#'   \item{assumption_ok}{Logical TRUE/FALSE if test succeeded, NA if skipped/errored.}
#'   \item{message}{Character string prompting interpretation.}
#'   \item{object}{The input \code{clean_lm} object with updated assumptions.}
#'
#' @export
check_equalvariance <- function(object, use_test = TRUE) {
  if (!inherits(object, "clean_lm")) {
    stop("Input must be a clean_lm object.")
  }

  res <- stats::residuals(object$model)
  fit <- stats::fitted(object$model)

  plot <- ggplot2::ggplot(data.frame(fit = fit, res = res),
                          ggplot2::aes(x = fit, y = res)) +
    ggplot2::geom_point(color = "blue") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "darkblue") +
    ggplot2::labs(title = "Residuals vs. Fitted Values",
                  x = "Fitted Values", y = "Residuals") +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"))

  test_result <- NULL
  assumption_ok <- NA
  msg <- NULL

  if (isTRUE(use_test)) {
    test_result <- tryCatch(
      invisible(lmtest::bptest(object$model)),
      error = function(e) e
    )

    if (inherits(test_result, "error") || is.na(test_result$p.value)) {
      assumption_ok <- NA
      msg <- "Breusch-Pagan test could not be computed. Please interpret the plot visually."
    } else {
      assumption_ok <- unname(test_result$p.value >= 0.05)
      msg <- if (assumption_ok) {
        "Equal variance assumption appears reasonable."
      } else {
        "Equal variance assumption may not hold. Inspect the residual plot."
      }
    }
  } else {
    assumption_ok <- NA
    msg <- "Test skipped. Please interpret the plot visually."
  }

  # update clean_lm object
  object$assumptions$equalvariance <- assumption_ok
  object$assumptions_msgs$equalvariance <- msg

  result <- list(
    plot = plot,
    test_result = test_result,
    assumption_ok = assumption_ok,
    message = msg,
    object = object
  )
  class(result) <- c("check_equalvariance", "check_assumption")
  return(result)
}
