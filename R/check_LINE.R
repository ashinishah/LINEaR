#' Check LINE Assumptions
#'
#' Runs all four assumption checks (Linearity, Independence, Normality, Equal Variance)
#' on a fitted linear regression model wrapped in a \code{clean_lm} object.
#'
#' @param object A \code{clean_lm} object. Must contain a \code{model} component
#'   that is a fitted \code{lm} object.
#'
#' @param use_tests Logical flag indicating whether to run statistical tests
#'   (RESET, Durbin-Watson, Shapiro-Wilk, Breusch-Pagan). Defaults to \code{TRUE}.
#'
#' @return A list with the following components:
#' \item{linearity}{Result from \code{check_linearity()}.}
#' \item{independence}{Result from \code{check_independence()}.}
#' \item{normality}{Result from \code{check_normality()}.}
#' \item{equalvariance}{Result from \code{check_equalvariance()}.}
#' \item{object}{The input \code{clean_lm} object with its
#'   \code{assumptions} fields updated.}
#'
#' @details
#' This function provides a one-stop diagnostic for the four classical
#' linear regression assumptions: Linearity, Independence, Normality and
#' Equal Variance. Each component includes a plot,
#' optional statistical test, and a logical flag or message indicating
#' whether the assumption appears valid.
#'
#' @examples
#' \dontrun{
#' fit <- clean_lm(mpg ~ wt + hp, data = mtcars)
#' result <- check_LINE(fit)
#' result$linearity$plot
#' result$normality$normality_ok
#' result$object$assumptions
#' }
#'
#' @export
check_LINE <- function(object, use_tests = TRUE) {
  if (!inherits(object, "clean_lm")) {
    stop("Input must be a clean_lm object.")
  }

  lin_res  <- check_linearity(object, use_test = use_tests)
  ind_res  <- check_independence(object, use_test = use_tests)
  norm_res <- check_normality(object, use_test = use_tests)
  eqv_res  <- check_equalvariance(object, use_test = use_tests)

  # Update assumptions in the clean_lm object
  object$assumptions$linearity     <- lin_res$linearity_ok
  object$assumptions$independence  <- ind_res$independence_ok
  object$assumptions$normality     <- norm_res$normality_ok
  object$assumptions$equalvariance <- eqv_res$equalvariance_ok

  return(list(
    linearity     = lin_res,
    independence  = ind_res,
    normality     = norm_res,
    equalvariance = eqv_res,
    object        = object
  ))
}
