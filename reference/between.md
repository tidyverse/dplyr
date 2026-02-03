# Detect where values fall in a specified range

This is a shortcut for `x >= left & x <= right`, implemented for local
vectors and translated to the appropriate SQL for remote tables.

## Usage

``` r
between(x, left, right, ..., ptype = NULL)
```

## Arguments

- x:

  A vector

- left, right:

  Boundary values. Both `left` and `right` are recycled to the size of
  `x`.

- ...:

  These dots are for future extensions and must be empty.

- ptype:

  An optional prototype giving the desired output type. The default is
  to compute the common type of `x`, `left`, and `right` using
  [`vctrs::vec_cast_common()`](https://vctrs.r-lib.org/reference/vec_cast.html).

## Value

A logical vector the same size as `x` with a type determined by `ptype`.

## Details

`x`, `left`, and `right` are all cast to their common type before the
comparison is made. Use the `ptype` argument to specify the type
manually.

## See also

[`join_by()`](https://dplyr.tidyverse.org/reference/join_by.md) if you
are looking for documentation for the `between()` overlap join helper.

## Examples

``` r
between(1:12, 7, 9)
#>  [1] FALSE FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE FALSE FALSE
#> [12] FALSE

x <- rnorm(1e2)
x[between(x, -1, 1)]
#>  [1] -0.13399701 -0.27923724 -0.31344598  0.07003485 -0.63912332
#>  [6] -0.04996490 -0.25148344  0.44479712  0.04653138  0.57770907
#> [11]  0.11819487  0.86208648 -0.24323674 -0.20608719  0.01917759
#> [16]  0.02956075  0.54982754 -0.36122126  0.21335575 -0.66508825
#> [21] -0.24589641 -0.97585062  0.13167063  0.48862881  0.28415034
#> [26]  0.23669628  0.52390979  0.60674805 -0.10993567  0.17218172
#> [31] -0.09032729  0.74879127  0.55622433 -0.54825726 -0.15569378
#> [36]  0.43388979 -0.38195111  0.42418757 -0.03810289  0.48614892
#> [41] -0.35436116  0.94634789 -0.29664002 -0.38721358 -0.78543266
#> [46] -0.79554143 -0.69053790 -0.55854199 -0.53666333  0.22712713
#> [51]  0.97845492 -0.20888265  0.25853729 -0.44179945  0.56859986
#> [56]  0.42485844  0.24940178  0.44945378  0.42656655  0.10758399
#> [61]  0.02229473  0.60361101 -0.26265057 -0.52826408  0.19214942
#> [66]  0.84618466  0.08171963 -0.94491206

# On a tibble using `filter()`
filter(starwars, between(height, 100, 150))
#> # A tibble: 5 × 14
#>   name    height  mass hair_color skin_color eye_color birth_year sex  
#>   <chr>    <int> <dbl> <chr>      <chr>      <chr>          <dbl> <chr>
#> 1 Leia O…    150    49 brown      light      brown             19 fema…
#> 2 Mon Mo…    150    NA auburn     fair       blue              48 fema…
#> 3 Watto      137    NA black      blue, grey yellow            NA male 
#> 4 Sebulba    112    40 none       grey, red  orange            NA male 
#> 5 Gasgano    122    NA none       white, bl… black             NA male 
#> # ℹ 6 more variables: gender <chr>, homeworld <chr>, species <chr>,
#> #   films <list>, vehicles <list>, starships <list>

# Using the `ptype` argument with ordered factors, where otherwise everything
# is cast to the common type of character before the comparison
x <- ordered(
  c("low", "medium", "high", "medium"),
  levels = c("low", "medium", "high")
)
between(x, "medium", "high")
#> [1] FALSE FALSE FALSE FALSE
between(x, "medium", "high", ptype = x)
#> [1] FALSE  TRUE  TRUE  TRUE
```
