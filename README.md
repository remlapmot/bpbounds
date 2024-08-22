# bpbounds: Nonparametric bounds for the average causal effect for one-sample (bivariate) and two-sample (trivariate) data

<!-- badges: start -->
[![R-CMD-check](https://github.com/remlapmot/bpbounds/actions/workflows/check-full.yaml/badge.svg)](https://github.com/remlapmot/bpbounds/actions/workflows/check-full.yaml)
[![Coverage status](https://codecov.io/gh/remlapmot/bpbounds/branch/master/graph/badge.svg)](https://app.codecov.io/github/remlapmot/bpbounds?branch=master)
[![CRAN status](https://www.r-pkg.org/badges/version/bpbounds)](https://cran.r-project.org/package=bpbounds)
[![RStudio_CRAN_mirror_downloads_badge](https://cranlogs.r-pkg.org/badges/grand-total/bpbounds?color=blue)](https://CRAN.R-project.org/package=bpbounds)
[![r-universe](https://remlapmot.r-universe.dev/badges/bpbounds)](https://remlapmot.r-universe.dev/bpbounds)
<!-- badges: end -->

R package implementing the nonparametric bounds for the average causal effect of [Balke and Pearl (1997)](https://doi.org/10.1080/01621459.1997.10474074).

## Installation

Install the released version of **bpbounds** from CRAN:

``` r
install.packages("bpbounds")
```

or from the MRCIEU R-universe

```r
install.packages("bpbounds", repos = c("https://mrcieu.r-universe.dev", "https://cloud.r-project.org"))
```

Or install the development version from my r-universe

```r
install.packages("bpbounds", repos = c("https://remlapmot.r-universe.dev", "https://cloud.r-project.org"))
```

Or install the development version from GitHub with:

``` r
# install.packages("remotes") 
remotes::install_github("remlapmot/bpbounds")
```

## Shiny App

There is a Shiny app demonstrating the package at: <https://remlapmot.shinyapps.io/bpbounds/>.

## Package website

The helpfiles and vignette are shown on the package website at: <https://remlapmot.github.io/bpbounds/>.

## Authors
Tom Palmer (maintainer, tom.palmer@bristol.ac.uk), Roland Ramsahai, Vanessa Didelez, Nuala Sheehan.

## References

Balke A, Pearl J. Bounds on Treatment Effects from studies with imperfect compliance. Journal of the American Statistical Association, 1997, 92, 439, 1171-1176, doi: [10.1080/01621459.1997.10474074](https://doi.org/10.1080/01621459.1997.10474074).
