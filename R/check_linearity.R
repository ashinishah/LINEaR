#' Check Linearity Assumption
#'
#' Produces a residuals vs. fitted plot and optionally performs the RESET test
#' to assess linearity assumption in a fitted linear regression model.
#'
#' @param object A \code{clean_lm} object. Must contain a \code{model} component
#'   that is a fitted \code{lm} object.
#' @param use_test Logical flag indicating whether to run the RESET test
#'   (\code{lmtest::resettest}). Defaults to \code{TRUE}.
#'
#' @return An object of class \code{check_linearity} and \code{check_assumption},
#'   with components:
#'   \item{plot}{Residuals vs. fitted values plot (ggplot2).}
#'   \item{test_result}{RESET test result, or error object if test failed.}
#'   \item{assumption_ok}{Logical TRUE/FALSE if test succeeded, NA if skipped/errored.}
#'   \item{message}{Character string prompting interpretation.}
#'   \item{object}{The input \code{clean_lm} object with updated assumptions.}
#'
#' @export
check_linearity <- function(object, use_test = TRUE) {
  if (!inherits(object, "clean_lm")) {
    stop("Input must be a clean_lm object.")
  }

  plot_data <- data.frame(
    fitted = stats::fitted(object$model),
    residuals = stats::residuals(object$model)
  )

  plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$fitted, y = .data$residuals)) +
    ggplot2::geom_point(color = "red") +
    ggplot2::geom_smooth(method = "loess", se = FALSE, color = "darkred") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::labs(title = "Residuals vs. Fitted Values",
                  x = "Fitted Values",
                  y = "Residuals") +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"))

  test_result <- NULL
  assumption_ok <- NA
  msg <- NULL

  if (isTRUE(use_test)) {
    test_result <- tryCatch(
      lmtest::resettest(object$model),
      error = function(e) e
    )

    if (inherits(test_result, "error") || is.na(test_result$p.value)) {
      assumption_ok <- NA
      msg <- "RESET test could not be computed. Please interpret the residual plot visually."
    } else {
      assumption_ok <- test_result$p.value >= 0.05
      msg <- if (assumption_ok) {
        "Linearity assumption appears reasonable."
      } else {
        "Linearity assumption may not hold. Inspect the residual plot."
      }
    }
  } else {
    assumption_ok <- NA
    msg <- "Test skipped. Please interpret the residual plot visually."
  }

  # update assumptions in the clean_lm object
  object$assumptions$linearity <- assumption_ok
  object$assumptions_msgs$linearity <- msg

  result <- list(
    plot = plot,
    test_result = test_result,
    assumption_ok = assumption_ok,
    message = msg,
    object = object
  )
  class(result) <- c("check_linearity", "check_assumption")
  return(result)
}
