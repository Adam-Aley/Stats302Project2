data("my_penguins")
new_penguins <- na.omit(my_penguins)

# within my test-my_lm.R

test_that("non formula throws error" , {
    expect_error(my_lm(new_penguins$species, new_penguins))
    })

test_that("wrong syntax throws errors", {
  expect_error(my_lm("a", bill_length_mm ~ bill_depth_mm + flipper_length_mm))
  expect_error(my_lm(new_penguins, 2))
})
