test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

# within test-my_test.R
test_that("non-numeric input throws error", {
  expect_error(my_test((1:10), 1, 1))
})
