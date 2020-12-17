data("my_penguins")
new_penguins <- na.omit(my_penguins)

# Within my test-my_rf_cv.R

test_that("invalid input", {
  expect_error(my_rf_cv("string"))
})

test_that("wrong syntax will throw errors", {
  expect_error(my_rf_cv(list(c("new_penguins"))))
  expect_error(my_rf_cv(matrix(NA, nrow = 5)))
})
