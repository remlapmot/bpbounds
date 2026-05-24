# Changelog

## bpbounds 0.1.7

CRAN release: 2026-05-24

- bpbounds now requires R 4.1 or later. This is because its soft
  dependency, **tidyr**, has a hard dependency, **purrr**, with this
  requirement (and tidyr is required for the main example in the
  vignette).

- **dplyr** removed as a soft dependency and instances of the
  magrittr/dplyr pipe, `%>%`, replaced with the native pipe, `|>`.

- Fixed incorrect index mapping in `bpbounds_calc_tri_z3()`: the x=0,y=1
  and x=1,y=0 conditional probability cells were swapped for each
  category of Z, giving wrong bounds for the trivariate 3-category
  instrument case (thanks [@sachsmc](https://github.com/sachsmc)).

## bpbounds 0.1.6

CRAN release: 2024-06-13

- Tweak formatting of code in helpfile examples and vignette

- Bumped minimum required version of R to be at least 4.0.0 because this
  is now required by **evaluate**, which is a dependency of **knitr**.

- Bumped version of **roxygen2** used to create package documentation

- Minor edits to README.md

## bpbounds 0.1.5

CRAN release: 2023-05-03

- Additional email address and affiliation edits.

- Remove `LazyData` from `DESCRIPTION`.

- Update **roxygen2** version number in `DESCRIPTION`.

- Simplify NAMESPACE by not importing functions from other packages.

- Improve accompanying pkgdown site

## bpbounds 0.1.4

CRAN release: 2020-01-21

- Fixed typo in vignette.

- Changed email address.

## bpbounds 0.1.3

CRAN release: 2019-02-10

- Added Nuala’s ORCID to DESCRIPTION.

## bpbounds 0.1.2

- Added Roland’s ORCID to DESCRIPTION.

## bpbounds 0.1.1

CRAN release: 2018-12-19

- Added
  [`runExample()`](https://remlapmot.github.io/bpbounds/reference/runExample.md)
  to launch the Shiny App included in ./inst/shiny-examples/myapp .

## bpbounds 0.1.0

CRAN release: 2018-11-18

- R version of our `bpbounds` Stata command distributed with Palmer et
  al., Stata Journal, 2011, 11, 3, 345-367
  <https://www.stata-journal.com/article.html?article=st0232>.

- First submission to CRAN.
