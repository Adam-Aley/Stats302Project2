data("my_penguins")
new_penguins <- na.omit(my_penguins)

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

# within my test-my_lm.R

test_that("non formula throws error" , {
    expect_error(my_lm(new_penguins$species, new_penguins))
    })

test_that("wrong syntax throws errors", {
  expect_error(my_lm("a", bill_length_mm ~ bill_depth_mm + flipper_length_mm))
  expect_error(my_lm(new_penguins, 2))
})
