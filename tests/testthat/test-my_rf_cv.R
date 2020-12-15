library(tidyr)
data("my_penguins")
new_penguins <- na.omit(my_penguins)
test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
