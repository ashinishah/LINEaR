#' Check Independence Assumption
#'
#' Produces a residuals vs. observation index plot, optionally performs
#' the Durbin-Watson test, in order to assess independence of residuals
#' assumption in a fitted linear regression model.
#'
#' @param object A \code{clean_lm} object. Must contain a \code{model}
#'   component that is a fitted \code{lm} object.
#' @param use_test Logical flag indicating whether to run Durbin-Watson
#'   test (\code{lmtest::dwtest}). Defaults to \code{TRUE}.
#'
#' @return An object of class \code{check_independence} and \code{check_assumption},
#'   with components:
#'   \item{plot}{Residuals vs. observation index plot (ggplot2).}
#'   \item{test_result}{Durbin-Watson test result, or error object if test failed.}
#'   \item{assumption_ok}{Logical TRUE/FALSE if test succeeded, NA if skipped/errored.}
#'   \item{message}{Character string prompting interpretation.}
#'   \item{object}{The input \code{clean_lm} object with updated assumptions.}
#'
#' @export
check_independence <- function(object, use_test = TRUE) {
  if (!inherits(object, "clean_lm")) {
    stop("Input must be a clean_lm object.")
  }

  res <- stats::residuals(object$model)

  plot <- ggplot2::ggplot(data.frame(index = seq_along(res), res = res),
                          ggplot2::aes(x = index, y = res)) +
    ggplot2::geom_point(color = "orange") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "darkorange") +
    ggplot2::labs(title = "Residuals vs. Observation Index",
                  x = "Observation Index", y = "Residuals") +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"))

  test_result <- NULL
  assumption_ok <- NA
  msg <- NULL

  if (isTRUE(use_test)) {
    test_result <- tryCatch(
      lmtest::dwtest(object$model),
      error = function(e) e
    )

    if (inherits(test_result, "error") || is.na(test_result$p.value)) {
      assumption_ok <- NA
      msg <- "Durbin-Watson test could not be computed. Please interpret the plot visually."
    } else {
      assumption_ok <- test_result$p.value >= 0.05
      msg <- if (assumption_ok) {
        "Independence assumption appears reasonable."
      } else {
        "Independence assumption may not hold. Inspect the residual plot."
      }
    }
  } else {
    assumption_ok <- NA
    msg <- "Test skipped. Please interpret the plot visually."
  }

  # update clean_lm object
  object$assumptions$independence <- assumption_ok
  object$assumptions_msgs$independence <- msg

  result <- list(
    plot = plot,
    test_result = test_result,
    assumption_ok = assumption_ok,
    message = msg,
    object = object
  )
  class(result) <- c("check_independence", "check_assumption")
  return(result)
}
