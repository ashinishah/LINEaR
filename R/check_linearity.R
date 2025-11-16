check_linearity <- function(object, use_test = TRUE) {
  if (!inherits(object, "clean_lm")) {
    stop("Input must be a clean_lm object.")
  }

  plot_data <- data.frame(
    fitted = stats::fitted(object$model),
    residuals = stats::residuals(object$model)
  )

  plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = fitted, y = residuals)) +
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
