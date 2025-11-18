#'Check Normality Assumption
#'
#'Produces a Q-Q plot, optionally performs a Shapiro-Wilk test,
#'optionally computes skewness and kurtosis in order
#'to assess normality of residuals assumption in a fitted
#'linear regression model.
#'
#'@param object A \code{clean_lm} object. Must contain a \code{model} component
#'that is a fitted \code{lm} object.
#'
#'@param use_test Logical flag indicating whether to run Shapiro-Wilk test
#'  (\code{stats::shapiro.test}). Defaults to \code{TRUE}.
#'
#'@param use_skew_kurt Logical flag indicating whether to compute skewness and kurtosis.
#'Defaults to \code{FALSE}.
#'
#'@return A list with the following components:
#'\item{plot}{A ggplot2 object showing Q-Q plot.}
#'\item{test_result}{The result of the Shapiro-Wilk test, or \code{NULL} if not run
#'  or if the test errored.}
#'\item{skew_kurt}{A list with skewness and kurtosis values, or \code{NULL}
#'  if not computed.}
#'\item{normality_ok}{Logical indicating whether the normality assumption
#'  appears valid (\code{TRUE}/\code{FALSE}), or a character message if the test
#'  was skipped or errored.}
#'\item{object}{The input \code{clean_lm} object with its
#'  \code{assumptions$normality} field updated.}
#'
#'@details
#'The function checks normality both visually (via Q-Q plot)
#'and statistically (via the Shapiro-Wilk test). If points closely follow the diagonal
#'line, the normality assumption is more plausible. If the Shapiro-Wilk test is run
#'and succeeds without error, \code{normality_ok} is set to \code{TRUE} if the
#'p-value is greater than 0.05, otherwise \code{FALSE}. If the test errors or is skipped,
#'a character message is returned prompting the user to interpret the plot visually.
#'
#'@examples
#'\dontrun{
#'fit <- lm(mpg ~ wt + hp, data = mtcars)
#'clean_fit <- clean_lm(fit)
#'result <- check_normality(clean_fit)
#'result$plot
#'result$normality_ok
#'}
#'
#'@importFrom stats residuals shapiro.test
#'@import ggplot2
#'
#'@export

check_normality <- function(object, use_test = TRUE, use_skew_kurt = FALSE) {
  if (!inherits(object, "clean_lm")) {
    stop("Input must be a clean_lm object.")
  }

  res <- stats::residuals(object$model)

  plot <- ggplot2::ggplot(data.frame(res = res), ggplot2::aes(sample=res))+
    ggplot2::stat_qq(color="green") +
    ggplot2::stat_qq_line(color="darkgreen") +
    ggplot2::labs(title = "Q-Q Plot of Residuals") +
    ggplot2::theme_minimal()+
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"))

  test_result <- NULL
  normality_ok <- NULL

  if (isTRUE(use_test)){
    test_result <- tryCatch({
      stats::shapiro.test(res)
    }, error = function(e) {
      warning("Shapiro-Wilk test errored: ", e$message)
      NULL
    })

    if(is.null(test_result)) {
      normality_ok <- "Test Error: Interpret plot visually."
    } else if (test_result$p.value < 0.05) {
      normality_ok <- FALSE
    } else {
      normality_ok <- TRUE
    }
  } else {
    normality_ok <- "Test Skipped: Interpret plot visually."
  }

  skew_kurt <- NULL
  if(isTRUE(use_skew_kurt)) {
    skew <- mean((res - mean(res))^3) / sd(res)^3
    kurt <- mean((res-mean(res))^4) / sd(res)^4
    skew_kurt <- list(skewness = skew, kurtosis = kurt)
  }

  #update clean_lm object
  object$assumptions$normality <- normality_ok

  return(list(
    plot = plot,
    test_result = test_result,
    skew_kurt = skew_kurt,
    normality_ok = normality_ok,
    object = object
  ))
}
