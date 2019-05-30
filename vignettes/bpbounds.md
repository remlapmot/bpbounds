---
title: "Nonparametric bounds for the average causal effect: `bpbounds` examples"
author:
- name: "Tom Palmer"
  affiliation: "Lancaster University"
  email: tom.palmer@lancaster.ac.uk
- name: "Roland Ramsahai"
- name: "Vanessa Didelez"
  affiliation: "Leibniz BIPS and University of Bremen"
- name: "Nuala Sheehan"
  affiliation: "University of Leicester"
date: "2019-05-30"
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
This short vignette demonstrates the use of the `bpbounds` package. This is a R implementation of the of the nonparametric bounds for the average causal effect of @balke-jasa-1997 and some extensions. Currently this R package is a port of our `bpbounds` Stata package (@palmer-sj-2011). The code implements the approach of calculating the bounds outlined by @ramsahai-uai-2007, @ramsahai-thesis, and @ramsahai-jmlr-2012.

We start by loading our package and the others needed for the code in this vignette.

```r
library(bpbounds)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(tidyr)
```


## Features of the `bpbounds` package
Currently the package has one function, `bpbounds()`, which can accommodate the four scenarios implemented in our Stata command. These are to calculate the bounds for a binary outcome, a binary treatment/phenotype, and:

* a binary instrumental variable;
* a 3 category instrumental variable, e.g. a genotype in Mendelian randomization;

Bounds for these scenarios can be calculated with either

* trivariate data (in which all three variables are measured in one dataset);
* or bivariate data (in which the treatment/phenotype and instrument are measured in one dataset and the outcome and instrument are measured in another sample).


## Vitamin A supplementation example 
This example is taken from Table 1 of @balke-jasa-1997. It is from a study of Vitamin A supplementation consisting of children in 450 villages. Of these, 221 villages were assigned to control and 229 to the treatment. The outcome was mortality.

First we setup a `data.frame` of cell counts and convert this into a `table`. Here we follow the notation that `x` is the received treatment, `y` is the outcome, and `z` is the randomized treatment (the instrumental variable).


```r
tab1dat <- data.frame(
  z = c(0, 0, 1, 1, 1, 1, 0, 0),
  x = c(0, 0, 0, 0, 1, 1, 1, 1),
  y = c(0, 1, 0, 1, 0, 1, 0, 1),
  freq = c(74, 11514, 34, 2385, 12, 9663, 0, 0)
)

tab1inddat <- uncount(tab1dat, freq)
xt <- xtabs(~ x + y + z, data = tab1inddat)
xt
#> , , z = 0
#> 
#>    y
#> x       0     1
#>   0    74 11514
#>   1     0     0
#> 
#> , , z = 1
#> 
#>    y
#> x       0     1
#>   0    34  2385
#>   1    12  9663
```
Next we use `prop.table()` to calculate the conditional probabilities, $P(Y,X|Z)$, and then run `bpbounds()` assuming the data is trivariate. Although we could call `bpbounds()` using the table of cell counts `xt` directly, i.e. `bpbounds(xt)`.

```r
p <- prop.table(xt, margin = 3)
p
#> , , z = 0
#> 
#>    y
#> x              0            1
#>   0 0.0063859165 0.9936140835
#>   1 0.0000000000 0.0000000000
#> 
#> , , z = 1
#> 
#>    y
#> x              0            1
#>   0 0.0028113114 0.1972052257
#>   1 0.0009922276 0.7989912353
```


```r
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
Therefore, the average causal effect is bounded between -0.1946 and 0.0054. The estimate of the ACE is found as 
$$\text{ACE} = \frac{\text{cov}(Y,Z)}{\text{cov}(X,Z)}$$
as follows.

```r
covyz = cov(tab1inddat$y, tab1inddat$z)

covxz = cov(tab1inddat$x, tab1inddat$z)

ace = covyz / covxz
ace
#> [1] 0.003228039
```

### Entering the data as conditional probabilities
If you already know the conditional probabilities you could pass them to `bpbounds()` as follows.

```r
condprob = c(.0064, 0, .9936, 0, .0028, .001, .1972, .799)
tabp = array(condprob,
             dim = c(2, 2, 2),
             dimnames = list(
               x = c(0, 1),
               y = c(0, 1),
               z = c(0, 1)
             )) %>%
  as.table()
bpbounds(tabp)
#> 
#> Data:                    trivariate
#> Instrument categories:   2
#> 
#> Instrumental inequality: TRUE 
#>  Causal parameter Lower bound Upper bound
#>               ACE  -0.1946000    0.005400
#>      P(Y|do(X=0))   0.9936000    0.993600
#>      P(Y|do(X=1))   0.7990000    0.999000
#>               CRR   0.8041465    1.005435
#> 
#> Monotonicity inequality: TRUE 
#>  Causal parameter Lower bound Upper bound
#>               ACE  -0.1946000    0.005400
#>      P(Y|do(X=0))   0.9936000    0.993600
#>      P(Y|do(X=1))   0.7990000    0.999000
#>               CRR   0.8041465    1.005435
```


### Treating the data as bivariate
To demonstrate the features of the command we can treat this data as bivariate.

```r
gtab  = xtabs( ~ y + z, data = tab1inddat)
gp = prop.table(gtab, margin = 2)
gp
#>    z
#> y             0           1
#>   0 0.006385916 0.003803539
#>   1 0.993614084 0.996196461

ttab  = xtabs( ~ x + z, data = tab1inddat)
tp = prop.table(ttab, margin = 2)
tp
#>    z
#> x           0         1
#>   0 1.0000000 0.2000165
#>   1 0.0000000 0.7999835

bpres2 = bpbounds(p = gp, t = tp, fmt = "bivariate")
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
Mendelian randomization is an approach in epidemiology due to @daveysmith-ije-2003 in which genotypes established to be robustly associated with phenotypes are used as instrumental variables in order to better estimate the causal effect of the phenotype on a disease outcome.

This example uses data from @meleady-ajcn-2003. It is trivariate data with 3 category instrument and binary phenotype and outcomes. The instrument is the 677CT polymorphism (rs1801133) in the Methylenetetrahydrofolate Reductase gene, involved in folate metabolism, as an instrumental variable to investigate the effect of homocysteine on cardiovascular
disease. 

The data are presented to us as conditional probabilities, so we take care to enter them in the correct position in the vectors.

```r
mt3 <- c(.83, .05, .11, .01, .88, .06, .05, .01, .72, .05, .20, .03)
p3 = array(mt3,
           dim = c(2, 2, 3),
           dimnames = list(
             x = c(0, 1),
             y = c(0, 1),
             z = c(0, 1, 2)
           ))
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
#> Monotonicity inequality: FALSE
```
We see that in this example the monotonicity inequality is not satisfied.


## Simulated example that does not satisy the IV conditions 
This example recreates that found in section 8.3 of our Stata Journal paper which uses simulated data to show that the IV inequality does not always detect violations to the underlying model assumptions. We simulate two outcomes, `y1` and `y2`, for which the assumptions are violated because there is a direct effect of the instrument, `z`, on the outcome. However, the direct effect on `y2` is much smaller in magnitude than the direct effect on `y1`.


```r
set.seed(2232011)
n = 10000
z = rbinom(n, 1, .5)
u = rbinom(n, 1, .5)
px = .05 + .1 * z + .1 * u
x = rbinom(n, 1, px)
p1 = .1 + .2 * z + .05 * x + .1 * u
y1 = rbinom(n, 1, p1)
p2 = .1 + .05 * z + .05 * x + .1 * u
y2 = rbinom(n, 1, p2)

tab1 = xtabs(~ x + y1 + z)
p1 <- prop.table(tab1, margin = 3)

bpres1 = bpbounds(p1)
sbp1 = summary(bpres1)
print(sbp1)
#> 
#> Data:                    trivariate
#> Instrument categories:   2
#> 
#> Instrumental inequality: FALSE 
#> 
#> Monotonicity inequality: FALSE
```
Due to the strong direct effect of the instrument on the outcome, both the instrumental variable and monotonicity inequalities are not satisifed.


```r
tab2 = xtabs(~ x + y2 + z)
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
#> Monotonicity inequality: FALSE
```
However, with the weaker direct effect we see that both are satisfied when the true underlying data generating model does not satisfy the instrumental variable assumptions.

## Conclusion
In conclusion, we have demonstrated the use of our `bpbounds` package implementing nonparametric bounds for the average causal effect for a range of data scenarios.

## References

