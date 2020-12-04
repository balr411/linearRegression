
# linearRegression

<!-- badges: start -->
[![Travis build status](https://travis-ci.com/balr411/linearRegression.svg?branch=master)](https://travis-ci.com/balr411/linearRegression)
[![Codecov test coverage](https://codecov.io/gh/balr411/linearRegression/branch/master/graph/badge.svg)](https://codecov.io/gh/balr411/linearRegression?branch=master)
<!-- badges: end -->


The goal of linearRegression is to replicate some of the functionality of the lm function in the stats R package, as well as the anova and summary functions in the stats and base R packages respectively. The linearRegression package is able to fit a linear regression model with a single outcome variable and multiple independent variables including interaction terms. It is able to return regression coefficients, fitted values, and residuals among other things which are described in detail in the corresponding help files. The anova function can compute F-statistics and p-values for each of the predictors included in the model. The summary function computes t-statistics and p-values for each of the predictors, as well as model F-statistic and p-value, R squared, adjusted R squared, and more, which is all described further in the help pages. The three functions implemented in this package have been extensively tested, using unit testing which includes 176 tests covering 100% of the written code. Continuous integration using travis was used to check each updated version of the package. The vignette entitled "Tutorial" contains a tutorial on the usage of the package. This tutorial works through a simple linear regression model in detail, and shows how to fit multiple linear regression models as well as multiple linear regression models containing interaction terms. In the tutorial, the functions from linearRegression are benchmarked against their counterparts in stats and base R using the bench package, and their correctness is shown with a for loop containing multiple calls to all.equal(). Although the linearRegression and summary functions are slower than lm and summary in stats and base, linearRegression's implementation of anova can sometimes be faster than the anova function from the stats package. Please see the tutorial for further discussion on the efficiency of linearRegression as well as the caveats with benchmarking against the already implemented functions. All things considered, linearRegession is a correct and efficient package capable of performing a basic linear regression analysis. 

## Installation

You can install the latest version of linearRegression from GitHub with:

``` r
devtools::install_github("balr411/linearRegression")
```

## Example

This is a basic example which shows you how to fit a multiple linear regression model and extract a data frame containing the estimated regression coefficients, their standard errors, t-statistics, and p-values. 

``` r
library(linearRegression)
fit<-linearRegression(mpg~cyl+disp+hp, dat=mtcars)
linearRegression::summary(fit)$coefficients
```

