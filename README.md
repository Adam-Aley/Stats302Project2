<!-- badges: start -->
  [![Travis build status](https://travis-ci.com/Adam-Aley/Stats302Project2.svg?branch=master)](https://travis-ci.com/Adam-Aley/Stats302Project2)
  [![Codecov test coverage](https://codecov.io/gh/Adam-Aley/Stats302Project2/branch/master/graph/badge.svg)](https://codecov.io/gh/Adam-Aley/Stats302Project2?branch=master)
  <!-- badges: end -->
  
# Installation

To download Stats302Project2 package, use the code below
```
# install.packages("devtools")
devtools::install_github("Adam-Aley/Stats302Project2")
library(Stats302Project2)  
``` 

# Use

The vignette demonstrates example usage of all main functions. You can see the vignette by using the following code (note that this requires a TeX installation to view properly):

```
devtools::install_github("Adam-Aley/Stats302Project2", build_vignette = TRUE, build_opts = c())
library(Stats302Project2)
# Use this to view the vignette in the Demo HTML help
help(package = "Stats302Project2", help_type = "html")
# Use this to view the vignette as an isolated HTML file
utils::browseVignettes(package = "Stats302Project2")
```
