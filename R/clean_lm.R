#'Clean Linear Model
#'Fits a linear model and stores data for diagnostics.
#'
#'@param formula A formula to fit the data, like 'y~x'
#'@param data A data frame containing the variables fitted in the formula
#'@return A list containing the fitted model and residuals
#'@examples
#'data<- data.frame(x= 1:10, y = 2*(1:10) + rnorm(10))
#'result <- clean_lm(y~x, data)
#'summary(result$model)
#'@export

clean_lm <- function(formula, data) {
  model <- lm(formula, data)
  return(list(model=model))
}
