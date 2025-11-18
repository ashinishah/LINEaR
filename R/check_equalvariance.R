#' Check Equal Variance Assumption
#'
#' Produces a residuals vs. fitted values plot, optionally performs
#' the Breusch-Pagan test, in order to assess homoscedasticity
#' (equal variance of residuals) assumption in a fitted linear regression model.
#'
#' @param object A \code{clean_lm} object. Must contain a \code{model} component
#'   that is a fitted \code{lm} object.
#'
#' @param use_test Logical flag indicating whether to run Breusch-Pagan test
#'   (\code{lmtest::bptest}). Defaults to \code{TRUE}.
#'
#' @return A list with the following components:
#' \item{plot}{A ggplot2 object showing residuals vs. fitted values.}
#' \item{test_result}{The result of the Breusch-Pagan test, or \code{NULL} if not run
#'   or if the test errored.}
#' \item{equalvariance_ok}{Logical indicating whether the equal variance assumption
#'   appears valid (\code{TRUE}/\code{FALSE}), or a character message if the test
#'   was skipped or errored.}
#' \item{object}{The input \code{clean_lm} object with its
#'   \code{assumptions$equalvariance} field updated.}
#'
#' @details
#' The function checks equal variance both visually (via residuals vs. fitted plot)
#' and statistically (via the Breusch-Pagan test). A "fan" or "cone" shape in the plot suggests
#' heteroscedasticity.If the points are roughly evenly scattered around zero with similar spread,
#' the equal variance assumption is more plausible. If the test succeeds,
#' \code{equalvariance_ok} is set to \code{TRUE} if the p-value is greater than 0.05,
#' otherwise \code{FALSE}. If the test errors or is skipped, a character message is
#' returned prompting the user to interpret the plot visually.
#'
#' @examples
#' \dontrun{
#' fit <- lm(mpg ~ wt + hp, data = mtcars)
#' clean_fit <- clean_lm(mpg ~ wt + hp, data = mtcars)
#' result <- check_equalvariance(clean_fit)
#' result$plot
#' result$equalvariance_ok
#' }
#'
#' @importFrom stats residuals fitted
#' @importFrom lmtest bptest
#' @import ggplot2
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
  equalvariance_ok <- NULL

  if (isTRUE(use_test)) {
    test_result <- tryCatch({
      lmtest::bptest(object$model)
    }, error = function(e) {
      warning("Breusch-Pagan test errored: ", e$message)
      NULL
    })

    if (is.null(test_result)) {
      equalvariance_ok <- "Test Error: Interpret plot visually."
    } else if (test_result$p.value < 0.05) {
      equalvariance_ok <- FALSE
    } else {
      equalvariance_ok <- TRUE
    }
  } else {
    equalvariance_ok <- "Test Skipped: Interpret plot visually."
  }

  # update clean_lm object
  object$assumptions$equalvariance <- equalvariance_ok

  return(list(
    plot = plot,
    test_result = test_result,
    equalvariance_ok = equalvariance_ok,
    object = object
  ))
}
