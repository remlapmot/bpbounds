# bpbounds 0.1.8

* For bivariate data the intervention probability bounds, `p10low`, `p10upp`, `p11low`, `p11upp`, and their monotonicity counterparts, are now clamped to [0, 1], so these and the causal risk ratio bounds derived from them can no longer fall outside their feasible ranges (matching the same change in the Stata package). The vectors of individual bound terms are returned unclamped.

* Fixed the ordering of the conditional probabilities passed to the constraint matrix for trivariate data with a 3-category instrument: the x=0,y=1 and x=1,y=0 cells within each category of Z were swapped. This could cause the IV inequality to be wrongly reported as violated on valid data (and, more rarely, vice versa) and could give incorrect ACE bounds. Consequently, the first example table from GitHub issue #3 is in fact compatible with the IV model, so `bpbounds()` now correctly reports its IV inequality as satisfied (thanks to @rmtrane for reporting this in issue #3).

* Fixed two typos in the monotonicity ACE bounds for bivariate data with a binary instrument (`monolow4` and `monoupp4` used `g00` where `g01` was required), which could give monotonicity bounds that excluded the true ACE.

* Fixed the monotonicity ACE upper bound for trivariate data with a 3-category instrument, which is `1 - p100 - p012` rather than `1 - p100 - p110`.

* Following these fixes, the bounds for all cases (trivariate and bivariate data, 2- and 3-category instruments, with and without monotonicity) have been verified to equal the sharp bounds computed by linear programming across simulated instrumental variable models. A new test covers the previously untested monotonicity bounds for trivariate data with a 3-category instrument.

* Fixes to the Shiny app (`runExample()`): the default values for the trivariate data with 3-category instrument example were transposed relative to their labels, and as labelled violated the IV inequality; the bivariate 3-category example is now correctly described as the Mendelian randomization example from the vignette in bivariate form rather than as hypothetical data; updated the contact email address; and refactored the server code to use a single reactive `renderPrint()`.

# bpbounds 0.1.7

* bpbounds now requires R 4.1 or later. This is because its soft dependency, **tidyr**, has a hard dependency, **purrr**, with this requirement (and tidyr is required for the main example in the vignette).

* **dplyr** removed as a soft dependency and instances of the magrittr/dplyr pipe, `%>%`, replaced with the native pipe, `|>`.

* Fixed incorrect index mapping in `bpbounds_calc_tri_z3()`: the x=0,y=1 and x=1,y=0 conditional probability cells were swapped for each category of Z, giving wrong bounds for the trivariate 3-category instrument case (thanks @sachsmc).

# bpbounds 0.1.6

* Tweak formatting of code in helpfile examples and vignette

* Bumped minimum required version of R to be at least 4.0.0 because this is now required by **evaluate**, which is a dependency of **knitr**.

* Bumped version of **roxygen2** used to create package documentation

* Minor edits to README.md

# bpbounds 0.1.5

* Additional email address and affiliation edits.

* Remove `LazyData` from `DESCRIPTION`.

* Update **roxygen2** version number in `DESCRIPTION`.

* Simplify NAMESPACE by not importing functions from other packages.

* Improve accompanying pkgdown site

# bpbounds 0.1.4

* Fixed typo in vignette.

* Changed email address.

# bpbounds 0.1.3

* Added Nuala's ORCID to DESCRIPTION.

# bpbounds 0.1.2

* Added Roland's ORCID to DESCRIPTION.

# bpbounds 0.1.1

* Added `runExample()` to launch the Shiny App included in ./inst/shiny-examples/myapp .

# bpbounds 0.1.0

* R version of our `bpbounds` Stata command distributed with Palmer et al., Stata Journal, 2011, 11, 3, 345-367 <https://www.stata-journal.com/article.html?article=st0232>.

* First submission to CRAN.
