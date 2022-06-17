#' Fit regression model.
#'
#' This function fits a regression model to the df dat. The type of regression
#' fit is modified by the variable type.
#'
#' @param dat is a dataframe
#' @param type is a string indicating linear or polynomial
#' regression to be fitted.
#' @return it returns a regression model
#' @export
#'
fit_regression_model <- function(dat, type = 'linear') {

  if (type == 'linear') {

    fit <- lm(salary ~ year, dat)

  } else if(type == 'poly') {

    fit <- lm(salary ~ year + I(year^2), earnings)

  } else{

    stop('type needs to be eith poly or linear. Please try again.')
  }

  return(fit)

}
