# Vectorised if-else

`if_else()` is a vectorized
[if-else](https://rdrr.io/r/base/Control.html). Compared to the base R
equivalent, [`ifelse()`](https://rdrr.io/r/base/ifelse.html), this
function allows you to handle missing values in the `condition` with
`missing` and always takes `true`, `false`, and `missing` into account
when determining what the output type should be.

## Usage

``` r
if_else(
  condition,
  true,
  false,
  missing = NULL,
  ...,
  ptype = NULL,
  size = deprecated()
)
```

## Arguments

- condition:

  A logical vector

- true, false:

  Vectors to use for `TRUE` and `FALSE` values of `condition`.

  Both `true` and `false` will be
  [recycled](https://vctrs.r-lib.org/reference/theory-faq-recycling.html)
  to the size of `condition`.

  `true`, `false`, and `missing` (if used) will be cast to their common
  type.

- missing:

  If not `NULL`, will be used as the value for `NA` values of
  `condition`. Follows the same size and type rules as `true` and
  `false`.

- ...:

  These dots are for future extensions and must be empty.

- ptype:

  An optional prototype declaring the desired output type. If supplied,
  this overrides the common type of `true`, `false`, and `missing`.

- size:

  **\[deprecated\]**

  Output size is always taken from `condition`.

## Value

A vector with the same size as `condition` and the same type as the
common type of `true`, `false`, and `missing`.

Where `condition` is `TRUE`, the matching values from `true`, where it
is `FALSE`, the matching values from `false`, and where it is `NA`, the
matching values from `missing`, if provided, otherwise a missing value
will be used.

## See also

[`vctrs::vec_if_else()`](https://vctrs.r-lib.org/reference/vec_if_else.html)

## Examples

``` r
x <- c(-5:5, NA)
if_else(x < 0, NA, x)
#>  [1] NA NA NA NA NA  0  1  2  3  4  5 NA

# Explicitly handle `NA` values in the `condition` with `missing`
if_else(x < 0, "negative", "positive", missing = "missing")
#>  [1] "negative" "negative" "negative" "negative" "negative" "positive"
#>  [7] "positive" "positive" "positive" "positive" "positive" "missing" 

# Unlike `ifelse()`, `if_else()` preserves types
x <- factor(sample(letters[1:5], 10, replace = TRUE))
ifelse(x %in% c("a", "b", "c"), x, NA)
#>  [1]  2  1  1  3 NA NA  1 NA  2  3
if_else(x %in% c("a", "b", "c"), x, NA)
#>  [1] b    a    a    c    <NA> <NA> a    <NA> b    c   
#> Levels: a b c d e

# `if_else()` is often useful for creating new columns inside of `mutate()`
starwars |>
  mutate(category = if_else(height < 100, "short", "tall"), .keep = "used")
#> # A tibble: 87 × 2
#>    height category
#>     <int> <chr>   
#>  1    172 tall    
#>  2    167 tall    
#>  3     96 short   
#>  4    202 tall    
#>  5    150 tall    
#>  6    178 tall    
#>  7    165 tall    
#>  8     97 short   
#>  9    183 tall    
#> 10    182 tall    
#> # ℹ 77 more rows
```
