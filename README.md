
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

# gnet

The goal of gnet is to …

## Installation

You can install the released version of gnet from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("gnet")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(ergmito)
library(gnet)

# A random sample of networks
nets <- list(ergmito::rbernoulli(3), ergmito::rbernoulli(4))

# Are the edgecounts random??
fun <- function(g, y) mean(sapply(g, function(z) summary(z~edges)) - y)

mytest <- struct_test(nets ~ edges + balance, y = runif(100), R=1000, stat=fun)
```

``` r
# Looking at the distribution
hist(mytest$stat, breaks=10, col="gray", border="transparent")
abline(v = mytest$stat0, col="steelblue", lwd=2, lty="dashed")
```

<img src="man/figures/README-hist-1.png" width="100%" />
