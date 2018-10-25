---
title: "Nonparametric bounds for the average causal effect: `bpbounds` examples"
author:
- name: "Tom Palmer"
  affiliation: "Lancaster University"
- name: "Roland Ramsahai"
- name: "Vanessa Didelez"
  affiliation: "Leibniz BIPS and University of Bremen"
- name: "Nuala Sheehan"
  affiliation: "University of Leicester"
date: "2018-10-25"
output: 
  rmarkdown::html_vignette:
    keep_md: true
    fig_caption: yes
bibliography: bibliography.bib
vignette: >
  %\VignetteIndexEntry{Nonparametric bounds for the average causal effect: `bpbounds` examples}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



## Introduction
This short vignette recreates the analysis presented in @palmer-sj-2011 which presented an implementation in Stata of the nonparametric bounds for the average causal effect of @balke-jasa-1997 and some extensions.

We start by loading our package and another needed for data manipulation.

```r
library(tidyr)
library(bpbounds)
```

## Vitamin A supplementation example 
This example is taken from Table 1 of @balke-jasa-1997.


```r
tab1dat <- data.frame(
  z = c(0,0,1,1,1,1,0,0),
  x = c(0,0,0,0,1,1,1,1),
  y = c(0,1,0,1,0,1,0,1),
  freq = c(74,11514,34,2385,12,9663,0,0)
)

tab1inddat <- uncount(tab1dat, freq)
xt <- xtabs( ~ x + y + z, data = tab1inddat)
p <- prop.table(xt, margin = 3)
bpres <- bpbounds(p)
sbp = summary(bpres)
print(sbp)
#> 
#> Data:                    trivariate
#> Instrument categories:   2
#> 
#> Instrumental inequality: TRUE 
#>  Causal parameter Lower bound Upper bound
#>               ACE  -0.1946228 0.005393689
#>      P(Y|do(X=0))   0.9936141 0.993614084
#>      P(Y|do(X=1))   0.7989912 0.999007772
#>               CRR   0.8041263 1.005428354
#> 
#> Monotonicity inequality: TRUE 
#>  Causal parameter Lower bound Upper bound
#>               ACE  -0.1946228 0.005393689
#>      P(Y|do(X=0))   0.9936141 0.993614084
#>      P(Y|do(X=1))   0.7989912 0.999007772
#>               CRR   0.8041263 1.005428354
```

To demonstrate the features of the command we can treat this data as bivariate.

```r
gtab  = xtabs(~ y + z, data = tab1inddat)
gp = prop.table(gtab, margin = 2)
gp
#>    z
#> y             0           1
#>   0 0.006385916 0.003803539
#>   1 0.993614084 0.996196461

ttab  = xtabs(~ x + z, data = tab1inddat)
tp = prop.table(ttab, margin = 2)
tp
#>    z
#> x           0         1
#>   0 1.0000000 0.2000165
#>   1 0.0000000 0.7999835

bpres2 = bpbounds(p=gp, t=tp, fmt="bivariate")
sbp2 = summary(bpres2)
print(sbp2)
#> 
#> Data:                    bivariate
#> Instrument categories:   2
#> 
#> Instrumental inequality: TRUE 
#>  Causal parameter Lower bound Upper bound
#>               ACE  -0.1974342 0.006385916
#>      P(Y|do(X=0))   0.9936141 0.993614084
#>      P(Y|do(X=1))   0.7961799 1.196212998
#>               CRR   0.8012969 1.203901009
#> 
#> Monotonicity inequality: TRUE 
#>  Causal parameter Lower bound Upper bound
#>               ACE  -0.1974342 0.006385916
#>      P(Y|do(X=0))   0.9936141 0.993614084
#>      P(Y|do(X=1))   0.7961799 1.002582378
#>               CRR   0.8012969 1.009025933
```


## Mendelian randomization example
This example uses data from @meleady-ajcn-2003. It is trivariate data with 3 category instrument and binary phenotype and outcomes.


```r
mt3 <- c(.83, .05, .11, .01, .88, .06, .05, .01, .72, .05, .20, .03)
p3 = array(mt3, dim = c(2,2,3), dimnames = list(x = c(0,1), y = c(0,1), z = c(0,1,2)))
p3 = as.table(p3)
bpres3 = bpbounds(p3)
sbp3 = summary(bpres3)
print(sbp3)
#> 
#> Data:                    trivariate
#> Instrument categories:   3
#> 
#> Instrumental inequality: TRUE 
#>  Causal parameter Lower bound Upper bound
#>               ACE       -0.09     0.74000
#>      P(Y|do(X=0))        0.06     0.12000
#>      P(Y|do(X=1))        0.03     0.80000
#>               CRR        0.25    13.33333
#> 
#> Monotonicity inequality: TRUE 
#> NULL
```
We see that in this example the monotonicity inequality is not satisfied.


## Simulated example that does not satisy the IV conditions 
This example recreates that found in section 8.3 of our Stata Journal paper.


```r
set.seed(2232011)
n = 10000
z = rbinom(n, 1, .5)
u = rbinom(n, 1, .5)
px = .05 + .1*z + .1*u
x = rbinom(n, 1, px)
p1 = .1 + .2*z + .05*x + .1*u
y1 = rbinom(n, 1, p1)
p2 = .1 + .05*z + .05*x + .1*u
y2 = rbinom(n, 1, p2)

tab1 = xtabs( ~ x + y1 + z)
p1 <- prop.table(tab1, margin = 3)

bpres1 = bpbounds(p1)
sbp1 = summary(bpres1)
print(sbp1)
#> 
#> Data:                    trivariate
#> Instrument categories:   2
#> 
#> Instrumental inequality: FALSE 
#> NULL
#> 
#> Monotonicity inequality: FALSE 
#> NULL
```
Due to the strong direct effect of the instrument on the outcome, both the instrumental variable and monotonicity inequalities are not satisifed.


```r
tab2 = xtabs( ~ x + y2 + z)
p2 <- prop.table(tab2, margin = 3)

bpres2 = bpbounds(p2)
sbp2 = summary(bpres2)
print(sbp2)
#> 
#> Data:                    trivariate
#> Instrument categories:   2
#> 
#> Instrumental inequality: TRUE 
#>  Causal parameter Lower bound Upper bound
#>               ACE -0.18711965   0.6700688
#>      P(Y|do(X=0))  0.17159525   0.2384172
#>      P(Y|do(X=1))  0.05129753   0.8416640
#>               CRR  0.21515868   4.9049378
#> 
#> Monotonicity inequality: TRUE 
#> NULL
```
However, with the weaker direct effect we see that both are satisfied when the true underlying data generating model does not satisfy the instrumental variable assumptions.
