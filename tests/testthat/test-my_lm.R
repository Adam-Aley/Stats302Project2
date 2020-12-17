library(tidyr)
data("my_penguins")
new_penguins <- na.omit(my_penguins)

# within my test-my_lm.R

test_that("non formula throws error" , {
    expect_error(my_lm(new_penguins$species, new_penguins))
    })

