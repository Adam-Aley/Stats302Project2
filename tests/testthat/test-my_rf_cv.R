library(tidyr)
data("my_penguins")
new_penguins <- na.omit(my_penguins)

# Within my test-my_rf_cv.R

test_that("invalid input", {
  expect_error(my_rf_cv("string"))
})
