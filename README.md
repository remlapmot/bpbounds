# bpbounds

[![Build Status](https://github.com/remlapmot/bpbounds/workflows/R-CMD-check/badge.svg)](https://github.com/remlapmot/bpbounds/actions?workflow=R-CMD-check)
[![Build Status](https://travis-ci.org/remlapmot/bpbounds.svg?branch=master)](https://travis-ci.org/remlapmot/bpbounds)
[![Coverage status](https://codecov.io/gh/remlapmot/bpbounds/branch/master/graph/badge.svg)](https://codecov.io/github/remlapmot/bpbounds?branch=master)
[![CRAN status](https://www.r-pkg.org/badges/version/bpbounds)](https://cran.r-project.org/package=bpbounds)
[![RStudio_CRAN_mirror_downloads_badge](http://cranlogs.r-pkg.org/badges/grand-total/bpbounds?color=blue)](https://CRAN.R-project.org/package=bpbounds)

R package implementing the nonparametric bounds for the average causal effect of Balke and Pearl, JASA, 1997.

## Installation

Install the released version of `bpbounds` from CRAN:

``` r
install.packages("bpbounds")
```

Or install the development version from GitHub with:

``` r
# Uncomment the next command if you don't have the remotes packages installed.
# The tidyverse have moved some functions out of the devtools package.
# install.packages("remotes") 

# I call install_github() with these options to build the vignette.
remotes::install_github("remlapmot/bpbounds", 
                        build_opts = c("--no-resave-data", "--no-manual"), 
                        build_vignettes = TRUE)
```
To update the development version of the package, simply run this command again.

## Shiny App

There is a Shiny app demonstrating the package at: <https://remlapmot.shinyapps.io/bpbounds>.

## Package website

The helpfiles and vignette are shown on the package website at: <https://remlapmot.github.io/bpbounds/>.

## Authors
Tom Palmer (maintainer, tom.palmer@bristol.ac.uk), Roland Ramsahai, Vanessa Didelez, Nuala Sheehan
