#'Check Linearity Assumption
#'
#'Produces a residuals vs. fitted plot and optionally performs the RESET test
#'to assess linearity assumption in a fitted linear regression model.
#'
#'@param object A \code{clean_lm} object. Must contain a \code{model} component
#'that is a fitted \code{lm} object.
#'
#'@param use_test Logical flag indicating whether to run the RESET test
#'  (\code{lmtest::resettest}). Defaults to \code{TRUE}.
#'
#'@return A list with the following components:
#'\item{plot}{A ggplot2 object showing residuals vs. fitted values.}
#'\item{test_result}{The result of the RESET test, or \code{NULL} if not run
#'  or if the test failed.}
#'\item{linearity_ok}{Logical indicating whether the linearity assumption
#'  appears valid (\code{TRUE}/\code{FALSE}), or \code{NA} if not tested.}
#'\item{object}{The input \code{clean_lm} object with its
#'  \code{assumptions$linearity} field updated.}
#'
#'@details
#'The function checks linearity both visually (via residuals vs. fitted plot)
#'and statistically (via the RESET test). If the RESET test is run and
#'succeeds, \code{linearity_ok} is set to \code{TRUE} if the p-value is
#'greater than 0.05, otherwise \code{FALSE}.
#'
#'@examples
#'\dontrun{
#'fit <- lm(mpg ~ wt + hp, data = mtcars)
#'clean_fit <- clean_lm(fit)
#'result <- check_linearity(clean_fit)
#'result$plot
#'result$linearity_ok
#'}
#'
#'@importFrom stats fitted residuals
#'@importFrom lmtest resettest
#'@importFrom rlang .data
#'@import ggplot2
#'
#'@export

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
  if (isTRUE(use_test)) {
    test_result <- tryCatch({
      lmtest::resettest(object$model)
    }, error = function(e) {
      warning("RESET test error:", e$message)
      NULL
    })
  }

  linearity_ok <- if (isTRUE(use_test)) {
    if (is.null(test_result)) {
      NA
    } else test_result$p.value > 0.05
  } else {
    NA
  }

  # update assumptions in the clean_lm object
  object$assumptions$linearity <- linearity_ok

  return(list(
    plot = plot,
    test_result = test_result,
    linearity_ok = linearity_ok,
    object = object
  ))
}
