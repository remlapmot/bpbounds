# Run Shiny App demonstrating the package

Run Shiny App demonstrating the package

## Usage

``` r
runExample(...)
```

## Arguments

- ...:

  passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html), e.g.
  `port`, `launch.browser`

## Examples

``` r
if (interactive() && requireNamespace("shiny", quietly = TRUE)) {
  bpbounds::runExample()
}
```
