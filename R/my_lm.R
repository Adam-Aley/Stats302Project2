#' Linear Regression Model
#'
#' This function creates a linear regression model based on data input
#'
#' @param formula Formula to run data correlations
#' @param data Dataset which contained inputs run through formula
#' @keywords inference
#'
#' @return Data frame containing values for standard errors, betas,
#' t-statistics, and p-values
#'
#' @examples
#' data("my_penguins")
#' new_penguins <- na.omit(my_penguins)
#' my_lm(lifeExp ~ gdpPercap + continent, my_gapminder)
#' my_lm(body_mass_g ~ flipper_length_mm + island, new_penguins)
#'
#' @export
my_lm <- function(formula, data) {
  # Creates frame model for data
  frame <- model.frame(formula, data)
  # Stores matrix model for x
  X <- model.matrix(formula, data)
  # Store matrix response for y
  Y <- model.response(frame)
  # Computes and stores estimate for linear regression model and response
  beta <- solve(t(X) %*% X) %*% t(X) %*% Y
  # Store degrees of freedom
  df <- length(Y) - length(beta)
  # Creates standard deviation of data
  sigma <- sum((Y - X %*% beta)^2 / df)
  # Creates standard error of data
  se <- diag(sqrt(sigma * solve(t(X) %*% X)))
  # Creates test statistic of data
  tStats <- (beta - 0) / se
  # Creates two sided p value of data
  p_vals <- 2 * pt(abs(tStats), df, lower.tail = FALSE)
  # Returns list of Estimate, Standard Error, test statistic, and p value
  return(data.frame("Estimate" = beta,
                    "Std. Error" = se,
                    "t_value" = tStats,
                    "p_value" = p_vals))
}

