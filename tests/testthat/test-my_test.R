test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

# within test-my_test.R
test_that("non-numeric input throws error", {
  expect_error(my_test((1:10), 1, 1))
})
test_that("my_t.test works properly", {
  expect_is(my_t.test(rnorm(100, 0, 1),"lesser", 5), "list")
  expect_is(my_t.test(rnorm(100, 0, 1),"greater", 5), "list")
  expect_is(my_t.test(rnorm(100, 0, 1),"two-sided", 5), "list")
})
