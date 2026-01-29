# Create a list of function calls

**\[deprecated\]**

`funs()` is deprecated; please use
[`list()`](https://rdrr.io/r/base/list.html) instead. We deprecated this
function because it provided a unique way of specifying anonymous
functions, rather than adopting the conventions used by purrr and other
packages in the tidyverse.

## Usage

``` r
funs(..., .args = list())
```

## Arguments

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  A list of functions specified by:

  - Their name, `"mean"`

  - The function itself, `mean`

  - A call to the function with `.` as a dummy argument,
    `mean(., na.rm = TRUE)`

  The following notations are **not** supported, see examples:

  - An anonymous function, `function(x) mean(x, na.rm = TRUE)`

  - An anonymous function in purrr notation, `~mean(., na.rm = TRUE)`

- .args, args:

  A named list of additional arguments to be added to all function
  calls. As `funs()` is being deprecated, use other methods to supply
  arguments: `...` argument in [scoped
  verbs](https://dplyr.tidyverse.org/dev/reference/summarise_all.md) or
  make own functions with
  [`purrr::partial()`](https://purrr.tidyverse.org/reference/partial.html).

## Examples

``` r
funs("mean", mean(., na.rm = TRUE))
#> Warning: `funs()` was deprecated in dplyr 0.8.0.
#> ℹ Please use a list of either functions or lambdas:
#> 
#> # Simple named list: list(mean = mean, median = median)
#> 
#> # Auto named with `tibble::lst()`: tibble::lst(mean, median)
#> 
#> # Using lambdas list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
#> <fun_calls>
#> $ mean: mean(.)
#> $ mean: mean(., na.rm = TRUE)
# ->
list(mean = mean, mean = ~ mean(.x, na.rm = TRUE))
#> $mean
#> function (x, ...) 
#> UseMethod("mean")
#> <bytecode: 0x55ff57a40640>
#> <environment: namespace:base>
#> 
#> $mean
#> ~mean(.x, na.rm = TRUE)
#> <environment: 0x55ff648b3400>
#> 

funs(m1 = mean, m2 = "mean", m3 = mean(., na.rm = TRUE))
#> Warning: `funs()` was deprecated in dplyr 0.8.0.
#> ℹ Please use a list of either functions or lambdas:
#> 
#> # Simple named list: list(mean = mean, median = median)
#> 
#> # Auto named with `tibble::lst()`: tibble::lst(mean, median)
#> 
#> # Using lambdas list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
#> <fun_calls>
#> $ m1: mean(.)
#> $ m2: mean(.)
#> $ m3: mean(., na.rm = TRUE)
# ->
list(m1 = mean, m2 = "mean", m3 = ~ mean(.x, na.rm = TRUE))
#> $m1
#> function (x, ...) 
#> UseMethod("mean")
#> <bytecode: 0x55ff57a40640>
#> <environment: namespace:base>
#> 
#> $m2
#> [1] "mean"
#> 
#> $m3
#> ~mean(.x, na.rm = TRUE)
#> <environment: 0x55ff648b3400>
#> 
```
