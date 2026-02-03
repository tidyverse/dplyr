# Compare two numeric vectors

This is a safe way of comparing if two vectors of floating point numbers
are (pairwise) equal. This is safer than using `==`, because it has a
built in tolerance

## Usage

``` r
near(x, y, tol = .Machine$double.eps^0.5)
```

## Arguments

- x, y:

  Numeric vectors to compare

- tol:

  Tolerance of comparison.

## Examples

``` r
sqrt(2) ^ 2 == 2
#> [1] FALSE
near(sqrt(2) ^ 2, 2)
#> [1] TRUE
```
