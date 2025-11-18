#'Check Independence Assumption
#'
#'Produces a residuals vs. observation index plot, optionally performs
#'the Durbin-Watson test, in order to assess independence of residuals
#'assumption in a fitted linear regression model.
#'
#'@param object A \code{clean_lm} object. Must contain a \code{model}
#'  component that is a fitted \code{lm} object.
#'
#'@param use_test Logical flag indicating whether to run Durbin-Watson
#'  test (\code{lmtest::dwtest}). Defaults to \code{TRUE}.
#'
#'@return A list with the following components:
#'\item{plot}{A ggplot2 object showing residuals vs. observation index.}
#'\item{test_result}{The result of the Durbin-Watson test, or \code{NULL} if not run
#'  or if the test errored.}
#'\item{independence_ok}{Logical indicating whether the independence assumption
#'  appears valid (\code{TRUE}/\code{FALSE}), or a character message if the test
#'  was skipped or errored.}
#'\item{object}{The input \code{clean_lm} object with its
#'  \code{assumptions$independence} field updated.}
#'
#'@details
#'The function checks independence both visually (via residual vs. index plot)
#'and statistically (via Durbin-Watson test). The residual plot is informative
#'when the observation index has inherent meaning (e.g. time order). If the
#'Durbin-Watson test is run and succeeds without error, \code{independence_ok}
#'is set to \code{TRUE} if the p-value is greater that 0.05, otherwise \code{FALSE}.
#'If the test errors of is skippped, a characcter message is returned, prompting
#'user to interpret plot visually.
#'
#'@examples
#'\dontrun{
#'fit <- lm(mpg ~ wt + hp, data = mtcars)
#'clean_fit <- clean_lm(fit)
#'result <- check_independence(clean_fit)
#'result$plot
#'result$independence_ok
#'}
#'
#'@importFrom stats residuals
#'@importFrom lmtest dwtest
#'@import ggplot2
#'
#'@export
check_independence <- function(object, use_test = TRUE) {
  if (!inherits(object, "clean_lm")) {
    stop("Input must be a clean_lm object.")
  }

  res <- stats::residuals(object$model)

  plot <- ggplot2::ggplot(data.frame(index= seq_along(res), res = res),
                          ggplot2::aes(x = index, y = res)) +
    ggplot2::geom_point(color = "orange") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "darkorange") +
    ggplot2::labs(title = "Residuals vs. Observation Index",
                  x = "Observation Index", y = "Residuals") +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"))

  test_result <- NULL
  independence_ok <- NULL

  if (isTRUE(use_test)) {
    test_result <- tryCatch({
      lmtest::dwtest(object$model)
    }, error = function(e) {
      warning("Durbin-Watson test errored: ", e$message)
      NULL
    })

    if (is.null(test_result)) {
      independence_ok <- "Test Error: Interpret plot visually."
    } else if (test_result$p.value < 0.05) {
      independence_ok <- FALSE
    } else {
      independence_ok <- TRUE
    }
  } else {
    independence_ok <- "Test Skipped: Interpret plot visually."
  }

  # update clean_lm object
  object$assumptions$independence <- independence_ok

  return(list(
    plot = plot,
    test_result = test_result,
    independence_ok = independence_ok,
    object = object
  ))
}
