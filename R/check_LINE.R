#' Check LINE Assumptions
#'
#' Runs selected assumption checks (Linearity, Independence, Normality,
#' Equal Variance) on a fitted \code{clean_lm} object.
#'
#' @param object A \code{clean_lm} object.
#' @param linearity Logical, whether to run the linearity check. Default TRUE.
#' @param independence Logical, whether to run the independence check. Default TRUE.
#' @param normality Logical, whether to run the normality check. Default TRUE.
#' @param equalvariance Logical, whether to run the equal variance check. Default TRUE.
#'
#' @return An object of class \code{check_LINE}, with components for each
#'   assumption run, plus the updated \code{clean_lm} object.
#'
#' @export
check_LINE <- function(object,
                       linearity = TRUE,
                       independence = TRUE,
                       normality = TRUE,
                       equalvariance = TRUE) {
  if (!inherits(object, "clean_lm")) {
    stop("Input must be a clean_lm object.")
  }

  lin <- ind <- norm <- eqv <- NULL

  if (linearity) {
    lin <- check_linearity(object)
    object <- lin$object
  }

  if (independence) {
    ind <- check_independence(object)
    object <- ind$object
  }

  if (normality) {
    norm <- check_normality(object)
    object <- norm$object
  }

  if (equalvariance) {
    eqv <- check_equalvariance(object)
    object <- eqv$object
  }

  result <- list(
    linearity      = lin,
    independence   = ind,
    normality      = norm,
    equalvariance = eqv,
    object         = object
  )
  class(result) <- "check_LINE"
  return(result)
}
