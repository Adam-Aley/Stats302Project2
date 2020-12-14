#' K-nearest neighbors cross-validation
#'
#' This function executes a knn model based on input data including
#' cross-validation and training error results
#'
#' @param train Data set to run model on
#' @param cl Values for the true class
#' @param k_nn Number of closest neighbors
#' @param k_cv Total number of folds per model
#' @keywords prediction
#'
#' @return Output list with predicted class, cross-validation error,
#' and training error
#'
#' @examples
#' my_knn_cv(cbind(my_gapminder$lifeExp, my_gapminder$gdpPercap),
#' my_gapminder$continent, 20, 5)
#' my_knn_cv(my_penguins[, -c(1, 2, 7, 8)],
#' my_penguins$species, 1, 5)
#'
#' @import class
#'
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv) {
  # Creates a fold vector to seperate data
  folds <- sample(rep(1:k_cv, length = nrow(train)))
  # Initializes an empty vector to store cv errors
  cv <- numeric(length = k_cv)

  # Loops to generate training and test sets for each fold
  for (i in 1:k_cv) {
    # Specifies training data based on unused folds
    data_train <- train[folds != i,]
    # Specifies test data based on used folds
    data_test <- train[folds == i,]
    # Specifices training set of unused classes
    cl_train <- cl[folds != i]
    # Specifices test set of used classes
    cl_test <- cl[folds == i]
    # Generates predictions
    predictions <- knn(data_train, data_test, cl_train, k_nn)
    # Calculates cv error of unused fold
    cv[i] <- sum(predictions != cl_test) / length(cl_test)
  }

  # Calculates and stores mean cv error
  cv_err <- mean(cv)
  # Stores total predictions from model
  class <- knn(train, train, cl, k_nn)
  # Calculates and stores training error
  train_err <- sum((class != cl) / length(cl))
  # Stores outputs
  output <- list("class" = class, "cv_err" = cv_err, "train_err" = train_err)
  # Returns output
  return(output)
}
