data("my_penguins")
new_penguins <- na.omit(my_penguins)

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

# Within my test-my_rf_cv.R

test_that("invalid input", {
  expect_error(my_rf_cv("string"))
})

test_that("wrong syntax will throw errors", {
  expect_error(my_rf_cv(list(c("new_penguins"))))
  expect_error(my_rf_cv(matrix(NA, nrow = 5)))
})

test_that("negative inputs not accepts", {
  expect_error(my_rf_cv(my_gapminder, -2))
})
