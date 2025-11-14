#'Check Linearity Assumption
#'
#'Assesses linearity assumption using a residuals vs. fitted
#'values plot and Ramsey RESET test.
#'
#'@param object A \code{clean_lm} object returned by \code{clean_lm()}.
#'@param use_test Logical; if TRUE, performs Ramsey RESET test using \code{lmtest::resettest()}.
#'
#'@return A list containing:
#'\itemize{
#'  \item \code{plot}: ggplot object showing residuals vs. fitted values
#'  \item \code{test_result}: result of RESET test (if \code{use_test = TRUE})
#'  \item \code{linearity_ok}: logical flag indicating whether linearity assumption holds
#'}
#'
#'@examples
#'data <- data.frame(x = 1:100, y = 3 * (1:100) + rnorm(100))
#'fit <- clean_lm(y ~ x, data)
#'check_linearity(fit)
#'
#'
#'@export

check_linearity <- function(object, use_test = TRUE) {
  if (!inherits(object, "clean_lm")) {
    stop("Input must be a clean_lm object.")
  }

  #Diagnostic Plot: Residuas vs. Fitted Values
  plot <- ggplot2::ggplot(object$data, ggplot2::aes(x= object$fitted, y= object$residuals)) +
    ggplot2::geom_point(color = "red") +
    ggplot2::geom_smooth(method = "loess", se = FALSE, color = "darkred") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::labs(title = "Residuals vs. Fitted Values", x = "Fitted Values", y = "Residuals") +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = element_text(hjust = 0.5, face = "bold"))

  #Statistical Test: Ramsey RESET
  test_result <- NULL
  if (isTRUE(use_test)) {
    test_result <- tryCatch({
      lmtest::resettest(object$model)
    }, error = function(e) {
      warning("RESET test error:", e$message)
      NULL
    })
  }

  #Logical Flag and Message
  linearity_ok <- if (isTRUE(use_test)) {
    if (is.null(test_result)) {
      "Test Error: Interpret plot visually."
    } else if (test_result$p.value > 0.05) {
      TRUE
    } else {
      FALSE
    }
  } else {
    "Test Skipped: Interpret plot visually."
  }

  #output
  return(list(
    plot = plot,
    test_result = test_result,
    linearity_ok = linearity_ok
  ))
}
