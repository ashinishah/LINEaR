#'Clean Linear Model
#'Fits a linear model and stores data for diagnostics.
#'
#'@param formula A formula to fit the data, like 'y~x'
#'@param data A data frame containing the variables fitted in the formula
#'@return A list of class \code{"clean_lm"} containing:
#'\itemize{
#'  \item \code{model}: the fitted \code{lm} object
#'  \item \code{formula}: the model formula
#'  \item \code{data}: input data frame
#'  \item \code{residuals}: residuals from the model
#'  \item \code{fitted}: fitted values from the model
#'  \item \code{assumptions}: list with placeholders for LINE assumption checks
#'  \item \code{call}: original function call
#'  }
#'
#'@examples
#'data<- data.frame(x= 1:10, y = 2*(1:10) + rnorm(10))
#'result <- clean_lm(y~x, data)
#'summary(result$model)
#'@export

clean_lm <- function(formula, data) {
  model <- lm(formula, data)

  output <- list(
    model = model,
    formula = formula,
    data = data,
    residuals = residuals(model),
    fitted = fitted(model),
    assumptions = list(
      linearity = NULL,
      independence = NULL,
      normality = NULL,
      equal_var = NULL
    ),
    call = match.call()
  )

  class(output) <- "clean_lm"
  return(output)
}
