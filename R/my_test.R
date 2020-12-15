#' T.Test Function
#'
#' This function conducts a t.test with a given dataset
#'
#' @param x Dataset on which t.test will operate
#' @param alternative Strings of either "two.sided", "lesser", or "greater" to
#' designate alternative hypothesis
#' @param mu Numeric input designating value of the null hypothesis
#' @keywords inference
#'
#' @return Output list with dataset's standard error, test statistic, degrees
#' freedom, alternative hypothesis, and p-value after run through t.test
#'
#' @examples
#' my_test(my_gapminder$lifeExp, "two-sided", 60)
#' my_test(1:100, "two-sided", 60)
#'
#' @export
my_test <- function(x, alternative, mu) {
  # Creating standard error
  se <- sd(x) / (sqrt(length(x)))
  # Creating test statistic
  test_stat <- (mean(x) - mu) / (sd(x) / (sqrt(length(x))))
  # Creating degrees of freedom
  df <- length(x) - 1
  # Parameters to determine null hypothesis test
  # P value of upper tale test
  if (alternative == "greater" ) {
    p_val <- pt(abs(test_stat), df, lower.tail = FALSE)
    # P value of two sided test
  } else if (alternative == "two-sided") {
    p_val <- 2 * pt(abs(test_stat), df, lower.tail = FALSE)
    # P value of lower tale test
  } else if (alternative == "lesser") {
    p_val <- pt(test_stat, df, lower.tail = TRUE)
  } else {
    # Error message for alternative hypothesis
    stop("Try Again")
  }
  # Store results for se, test statistic, df, alternative hypothesis, and p value
  results <- list("se" = se,
                  "test_stat" = test_stat,
                  "df" = df,
                  "alternative_hypothesis" = alternative,
                  "p_value" = p_val)
  return(results)
}
