library(tidyr)
data("my_penguins")
new_penguins <- na.omit(my_penguins)

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("my_knn_cv works properly", {
  expect_is(my_knn_cv(new_penguins[, -c(1, 2, 7, 8)], new_penguins$species,
      1, 5), "list")
})
