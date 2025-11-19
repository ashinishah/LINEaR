
# LINEaR

<!-- badges: start -->
[![R-CMD-check](https://github.com/ashinishah/LINEaR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ashinishah/LINEaR/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of LINEaR is to provide clean summary output
and diagnostic tools for LINE assumptions (Linearity,
Independence, Normality and Equal Variance) in linear
regression. Users can view visual diagnostics and
perform statistical tests for each assumption, and also
view a summary of validity for all four assumptions.

## Installation

You can install the development version of LINEaR from GitHub using:

``` r
# install.packages("devtools")
devtools::install_github("ashinishah/LINEaR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(LINEaR)
library(LINEaR)

# Simulate data
sim_data <- simulate_lm_data(n = 100, beta0 = 2, beta1 = 0.5, sigma = 1, seed = 123)

# Fit model
fit <- clean_lm(y ~ x, data = sim_data)

# Run assumption checks
results <- check_LINE(fit)

# View summary
summary(fit)

```
## Vignette

For a full demonstration of LINEaR, including diagnostic plots 
and comparisons with base R, see the vignette:

```r
browseVignettes("LINEaR")
```
## Features
- Clean regression summaries with assumption diagnostics
- Functions for each assumption: linearity, independence, normality, equal variance
- Visual diagnostic plots using ggplot2

