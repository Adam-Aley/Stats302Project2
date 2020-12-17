library(tidyr)
data("my_penguins")
new_penguins <- na.omit(my_penguins)

# Within my test-my_knn_cv.R

test_that("my_knn_cv works properly", {
  expect_is(my_knn_cv(new_penguins[, -c(1, 2, 7, 8)], new_penguins$species,
      1, 5), "list")
})

test_that("wrong syntax throw errors", {
  expect_error(my_knn_cv(new_penguins, "wrong",  1, 5))
  expect_error(my_knn_cv(new_penguins, "species", "number",  5))
  expect_error(my_knn_cv(new_penguins, "species",  1))
  expect_error(my_knn_cv(gapminder, "species", 1,  5))
})
