library(tidyr)
data("my_penguins")
new_penguins <- na.omit(my_penguins)

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

# within my test-my_lm.R
test_that("my_lm is a data frame", {
  expect_is(my_lm(),"data.frame")
})
test_that("non formula throws error" , {
    expect_error(my_lm(new_penguins$species, new_penguins))
    })

