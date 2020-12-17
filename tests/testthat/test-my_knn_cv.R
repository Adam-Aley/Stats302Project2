library(tidyr)
data("my_penguins")
new_penguins <- na.omit(my_penguins)

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

# Within my test-my_knn_cv.R
test_that("my_knn_cv works properly", {
  expect_is(my_knn_cv(new_penguins[, -c(1, 2, 7, 8)], new_penguins$species,
      1, 5), "list")
})
test_that("incorrect input throws error", {
  expect_error(my_knn_cv(c(1,2,3), c(100,200), "a string", "number"))
  expect_error(my_knn_cv("1","2","3"))
})
