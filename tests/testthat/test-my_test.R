test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

# within test-my_test.R

test_that("non-numeric input throws error", {
  expect_error(my_test((1:10), 1, 1))
})

test_that("my_test alternative is lesser properly", {
  expect_is(my_test(my_gapminder[[4]], "lesser", 60), "list")
})

test_that("my_test alternative is greater properly", {
  expect_is(my_test(my_gapminder[[4]], "greater", 60), "list")
})

test_that("my_test alternative is two-sided properly", {
  expect_is(my_test(my_gapminder[[4]], "two-sided", 60), "list")
})

test_that("String input throws error", {
  expect_error(my_test(my_gapminder[[4]], 1, 60))
})

test_that("negative inputs not accepts", {
  expect_error(my_rf_cv(my_gapminder, -2))
})
