#' Summary for clean_lm Objects
#'
#' Provides a summary of a \code{clean_lm} object, including regression
#' coefficients, residual diagnostics, and results of LINE assumption checks.
#'
#' @param object A \code{clean_lm} object.
#' @param ... Additional arguments (ignored).
#'
#' @return Prints a formatted summary to the console. Invisibly returns the
#'   \code{clean_lm} object.
#'
#' @details
#' This function extends the base \code{summary.lm()} output by including
#' assumption check results stored in the \code{assumptions} and
#' \code{assumptions_msgs} fields of the \code{clean_lm} object. Each
#' assumption is displayed as either \code{TRUE}, \code{FALSE}, or \code{NA}
#' if the test was skipped or errored, followed by a message prompting
#' interpretation.
#'
#' @examples
#' fit <- clean_lm(mpg ~ wt + hp, data = mtcars)
#' fit <- check_LINE(fit)$object
#' summary_clean_lm(fit)
#'
#' @export
#' @export
summary.clean_lm <- function(object, ...) {
  cat("Summary of clean_lm model:\n\n")
  print(summary(object$model))

  cat("\nAssumption Checks (LINE):\n")

  if (!is.null(object$assumptions)) {
    for (nm in c("linearity", "independence", "normality", "equalvariance")) {
      val <- object$assumptions[[nm]]
      msg <- if (!is.null(object$assumptions_msgs) &&
                 nm %in% names(object$assumptions_msgs)) {
        object$assumptions_msgs[[nm]]
      } else {
        NULL
      }

      if (is.null(val)) {
        cat(" -", nm, ": Not run (skipped in check_LINE)\n")
      } else {
        cat(" -", nm, ": ", ifelse(is.na(val), "NA", val), "\n", sep = "")
        if (!is.null(msg)) cat("   ", msg, "\n")
      }
    }
  } else {
    cat("No assumption checks stored. Run check_LINE() or individual checks.\n")
  }

  invisible(object)
}
