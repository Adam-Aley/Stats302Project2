library(tidyr)
data("my_penguins")
new_penguins <- na.omit(my_penguins)

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

# Within my test-my_rf_cv.R
test_that("output is numeric", {
  expect_is(my_rf_cv(5), "numeric")
})
