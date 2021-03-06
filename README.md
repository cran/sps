Sequential Poisson Sampling
================

<!-- README.md is generated from README.Rmd. Please edit that file. -->

[![CRAN
status](https://www.r-pkg.org/badges/version/sps)](https://cran.r-project.org/package=sps)
[![R-CMD-check](https://github.com/marberts/sps/workflows/R-CMD-check/badge.svg)](https://github.com/marberts/sps/actions)
[![codecov](https://codecov.io/gh/marberts/sps/branch/master/graph/badge.svg?token=5CPGWUF267)](https://app.codecov.io/gh/marberts/sps)

Sequential Poisson sampling is a method for drawing
probability-proportional-to-size samples with a given number of units,
and is commonly used for price-index surveys. This package gives
functions to draw stratified sequential Poisson samples according to the
method by Ohlsson (1998), and generate bootstrap replicate weights
according to the method by Beaumont and Patak (2012).

## Installation

``` r
install.packages("sps")
```

The development version can be found on GitHub.

``` r
devtools::install_github("marberts/sps")
```

## Usage

Given a vector of sizes for units in a population (e.g., revenue for
sampling businesses) and a desired sample size, a stratified sequential
Poisson sample can be drawn with the `sps()` function.

``` r
library(sps)

# Generate some data on sizes for 12 businesses in a single 
# stratum as a simple example
revenue <- c(1:10, 100, 150)

# Draw a sample of 6 businesses
(samp <- sps(revenue, 6))
#> [1] 11 12  5  8  6  9

# Design weights and sampling strata are stored with the sample
weights(samp)
#> [1] 1.000000 1.000000 2.750000 1.718750 2.291667 1.527778
levels(samp)
#> [1] "TA" "TA" "TS" "TS" "TS" "TS"
```

The design weights for a sample can then be used to generate bootstrap
replicate weights with the `sps_repwights()` function.

## References

Beaumont, J.-F. and Patak, Z. (2012). On the Generalized Bootstrap for
Sample Surveys with Special Attention to Poisson Sampling.
*International Statistical Review*, 80(1): 127-148.

Ohlsson, E. (1998). Sequential Poisson Sampling. *Journal of Official
Statistics*, 14(2): 149-162.
