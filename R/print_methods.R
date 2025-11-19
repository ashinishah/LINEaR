#' @export
print.check_assumption <- function(x, ...) {
  # Identify which assumption check it is
  header <- if ("check_linearity" %in% class(x)) {
    "Linearity Check"
  } else if ("check_independence" %in% class(x)) {
    "Independence Check"
  } else if ("check_normality" %in% class(x)) {
    "Normality Check"
  } else if ("check_equalvariance" %in% class(x)) {
    "Equal Variance Check"
  } else {
    "Assumption Check"
  }

  # First line: header + result
  cat(header, ": ", ifelse(is.na(x$assumption_ok), "NA", x$assumption_ok), "\n", sep = "")

  # Second line: message
  if (!is.null(x$message)) cat(x$message, "\n")

  invisible(x)
}

#' @export
print.check_LINE <- function(x, ...) {
  cat("LINE Assumption Checks:\n")
  for (nm in setdiff(names(x), "object")) {
    header <- switch(nm,
                     linearity     = "Linearity Check",
                     independence  = "Independence Check",
                     normality     = "Normality Check",
                     equalvariance = "Equal Variance Check",
                     nm
    )

    val <- x[[nm]]$assumption_ok
    msg <- x[[nm]]$message

    if (is.null(val)) {
      cat(header, ": Not run (skipped in check_LINE)\n")
    } else {
      cat(header, ": ", ifelse(is.na(val), "NA", val), "\n", sep = "")
      if (!is.null(msg)) cat(" ", msg, "\n")
    }
  }
  invisible(x)
}


