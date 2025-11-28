# Nonparametric Bounds for the Average Causal Effect due to Balke and Pearl.

Nonparametric Bounds for the Average Causal Effect due to Balke and
Pearl.

## Usage

``` r
bpbounds(p, t = NULL, fmt = "trivariate")
```

## Arguments

- p:

  Object of class "table" containing either cell counts or conditional
  probabilities. For trivariate data these are for the
  phenotype/treatment-outcome association given Z, i.e. P(X, Y \| Z).

  Cell counts could be generated from `xtabs(~ x + y + z, data = data)`.
  And then conditional probabilities obtained by calling
  `prop.table(..., margins = 3)` on your object from
  [`xtabs()`](https://rdrr.io/r/stats/xtabs.html).

  If you only know the conditional probabilities you can enter these,
  e.g. for the Balke and Pearl Vitamin A example:

      cp   <- c(.0064, 0, .9936, 0, .0028, .001, .1972, .799)
      tabp <- as.table(array(
        cp,
        dim = c(2, 2, 2),
        dimnames = list(
          x = c(0, 1),
          y = c(0, 1),
          z = c(0, 1)
        )
      ))

  And then call `bpbounds()` using this object.

  For bivariate data this object contains cell conditional probabilities
  for the outcome-instrument (Y\|Z) association.

- t:

  Specified for bivariate data. Object with
  treatment/phenotype-instrument cell counts or conditional
  probabilities, i.e. (X\|Z).

- fmt:

  A character string which should be either "bivariate" (i.e. X, Z in
  one dataset and Y, Z in another dataset) or "trivariate" (X, Y, Z in
  the same dataset).

## Value

List with the following elements:

- fmt:

  whether the data is bivariate or trivariate

- nzcats:

  2 or 3, the no. instrument categories

- inequality:

  Logical, indicating whether the IV inequality is satisfied

- bplb:

  Lower bound of ACE

- bpub:

  Upper bound of ACE

- bplower:

  Vector of lower bound probabilities

- bpupper:

  Vector of upper bound probabilities

- p11low:

  Lower bound of P(Y=1\|do(X=1))

- p11upp:

  Upper bound of P(Y=1\|do(X=1))

- p10low:

  Lower bound of P(Y=1\|do(X=0))

- p10upp:

  Upper bound of P(Y=1\|do(X=0))

- p11lower:

  Vector of probabilities for lower bound of P(Y=1\|do(X=1))

- p11upper:

  Vector of probabilities for upper bound of P(Y=1\|do(X=1))

- p10lower:

  Vector of probabilities for lower bound of P(Y=1\|do(X=0))

- p10upper:

  Vector of probabilities for upper bound of P(Y=1\|do(X=0))

- crrlb:

  Lower bound of CRR

- crrub:

  Upper bound of CRR

- monoinequality:

  Logical, indicating whether the monotonicity inequality is satisfied

- monobplb:

  Lower bound of ACE assuming monotonicity

- monobpub:

  Upper bound of ACE assuming monotonicity

- monobplower:

  Vector of probabilities for lower bound of ACE assuming monotonicity

- monobpupper:

  Vector of probabilities for upper bound of ACE assuming monotonicity

- monop11low:

  Lower bound of P(Y=1\|do(X=1)) assuming monotonicity

- monop11upp:

  Upper bound of P(Y=1\|do(X=1)) assuming monotonicity

- monop10low:

  Lower bound of P(Y=1\|do(X=0)) assuming monotonicity

- monop10upp:

  Upper bound of P(Y=1\|do(X=0)) assuming monotonicity

- monop11lower:

  Vector for corresponding bound above

- monop11upper:

  Vector for corresponding bound above

- monop10lower:

  Vector for corresponding bound above

- monop10upper:

  Vector for corresponding bound above

- monocrrlb:

  Lower bound of CRR assuming monotonicity

- monocrrub:

  Upper bound of CRR assuming monotonicity

## Examples

``` r
# \donttest{
# Vitamin A example, using cell counts

require(tidyr)
#> Loading required package: tidyr
require(bpbounds)

tab1dat <- data.frame(
  z = c(0, 0, 1, 1, 1, 1, 0, 0),
  x = c(0, 0, 0, 0, 1, 1, 1, 1),
  y = c(0, 1, 0, 1, 0, 1, 0, 1),
  freq = c(74, 11514, 34, 2385, 12, 9663, 0, 0)
)
tab1inddat <- uncount(tab1dat, freq)
xt         <- xtabs(~ x + y + z, data = tab1inddat)
p          <- prop.table(xt, margin = 3)
bpres      <- bpbounds(p)
sbpres     <- summary(bpres)
print(sbpres)
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
#> 
# }

# \donttest{
# Vitamin A example, using conditional probabilities

require(bpbounds)
cp = c(.0064, 0, .9936, 0, .0028, .001, .1972, .799)
tabp = as.table(array(
  cp,
  dim = c(2, 2, 2),
  dimnames = list(
    x = c(0, 1),
    y = c(0, 1),
    z = c(0, 1)
  )
))
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
#> 
# }
```
