# Sample n rows from a table

**\[superseded\]** `sample_n()` and `sample_frac()` have been superseded
in favour of
[`slice_sample()`](https://dplyr.tidyverse.org/reference/slice.md).
While they will not be deprecated in the near future, retirement means
that we will only perform critical bug fixes, so we recommend moving to
the newer alternative.

These functions were superseded because we realised it was more
convenient to have two mutually exclusive arguments to one function,
rather than two separate functions. This also made it to clean up a few
other smaller design issues with `sample_n()`/`sample_frac`:

- The connection to
  [`slice()`](https://dplyr.tidyverse.org/reference/slice.md) was not
  obvious.

- The name of the first argument, `tbl`, is inconsistent with other
  single table verbs which use `.data`.

- The `size` argument uses tidy evaluation, which is surprising and
  undocumented.

- It was easier to remove the deprecated `.env` argument.

- `...` was in a suboptimal position.

## Usage

``` r
sample_n(tbl, size, replace = FALSE, weight = NULL, .env = NULL, ...)

sample_frac(tbl, size = 1, replace = FALSE, weight = NULL, .env = NULL, ...)
```

## Arguments

- tbl:

  A data.frame.

- size:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.md)\>
  For `sample_n()`, the number of rows to select. For `sample_frac()`,
  the fraction of rows to select. If `tbl` is grouped, `size` applies to
  each group.

- replace:

  Sample with or without replacement?

- weight:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.md)\>
  Sampling weights. This must evaluate to a vector of non-negative
  numbers the same length as the input. Weights are automatically
  standardised to sum to 1.

- .env:

  DEPRECATED.

- ...:

  ignored

## Examples

``` r
df <- tibble(x = 1:5, w = c(0.1, 0.1, 0.1, 2, 2))

# sample_n() -> slice_sample() ----------------------------------------------
# Was:
sample_n(df, 3)
#> # A tibble: 3 × 2
#>       x     w
#>   <int> <dbl>
#> 1     1   0.1
#> 2     3   0.1
#> 3     4   2  
sample_n(df, 10, replace = TRUE)
#> # A tibble: 10 × 2
#>        x     w
#>    <int> <dbl>
#>  1     2   0.1
#>  2     3   0.1
#>  3     1   0.1
#>  4     3   0.1
#>  5     1   0.1
#>  6     2   0.1
#>  7     5   2  
#>  8     1   0.1
#>  9     3   0.1
#> 10     3   0.1
sample_n(df, 3, weight = w)
#> # A tibble: 3 × 2
#>       x     w
#>   <int> <dbl>
#> 1     5   2  
#> 2     4   2  
#> 3     2   0.1

# Now:
slice_sample(df, n = 3)
#> # A tibble: 3 × 2
#>       x     w
#>   <int> <dbl>
#> 1     2   0.1
#> 2     3   0.1
#> 3     5   2  
slice_sample(df, n = 10, replace = TRUE)
#> # A tibble: 10 × 2
#>        x     w
#>    <int> <dbl>
#>  1     4   2  
#>  2     4   2  
#>  3     1   0.1
#>  4     5   2  
#>  5     3   0.1
#>  6     5   2  
#>  7     3   0.1
#>  8     4   2  
#>  9     4   2  
#> 10     1   0.1
slice_sample(df, n = 3, weight_by = w)
#> # A tibble: 3 × 2
#>       x     w
#>   <int> <dbl>
#> 1     3   0.1
#> 2     5   2  
#> 3     4   2  

# Note that sample_n() would error if n was bigger than the group size
# slice_sample() will just use the available rows for consistency with
# the other slice helpers like slice_head()
try(sample_n(df, 10))
#> Error in sample_n(df, 10) : Can't compute indices.
#> Caused by error:
#> ! `size` must be less than or equal to 5 (size of data).
#> ℹ set `replace = TRUE` to use sampling with replacement.
slice_sample(df, n = 10)
#> # A tibble: 5 × 2
#>       x     w
#>   <int> <dbl>
#> 1     3   0.1
#> 2     1   0.1
#> 3     2   0.1
#> 4     5   2  
#> 5     4   2  

# sample_frac() -> slice_sample() -------------------------------------------
# Was:
sample_frac(df, 0.25)
#> # A tibble: 1 × 2
#>       x     w
#>   <int> <dbl>
#> 1     4     2
sample_frac(df, 2, replace = TRUE)
#> # A tibble: 10 × 2
#>        x     w
#>    <int> <dbl>
#>  1     1   0.1
#>  2     2   0.1
#>  3     5   2  
#>  4     5   2  
#>  5     5   2  
#>  6     1   0.1
#>  7     2   0.1
#>  8     2   0.1
#>  9     2   0.1
#> 10     5   2  

# Now:
slice_sample(df, prop = 0.25)
#> # A tibble: 1 × 2
#>       x     w
#>   <int> <dbl>
#> 1     3   0.1
slice_sample(df, prop = 2, replace = TRUE)
#> # A tibble: 10 × 2
#>        x     w
#>    <int> <dbl>
#>  1     4   2  
#>  2     4   2  
#>  3     3   0.1
#>  4     2   0.1
#>  5     4   2  
#>  6     5   2  
#>  7     2   0.1
#>  8     1   0.1
#>  9     3   0.1
#> 10     3   0.1
```
