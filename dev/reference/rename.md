# Rename columns

`rename()` changes the names of individual variables using
`new_name = old_name` syntax; `rename_with()` renames columns using a
function.

## Usage

``` r
rename(.data, ...)

rename_with(.data, .fn, .cols = everything(), ...)
```

## Arguments

- .data:

  A data frame, data frame extension (e.g. a tibble), or a lazy data
  frame (e.g. from dbplyr or dtplyr). See *Methods*, below, for more
  details.

- ...:

  For `rename()`:
  \<[`tidy-select`](https://dplyr.tidyverse.org/dev/reference/dplyr_tidy_select.md)\>
  Use `new_name = old_name` to rename selected variables.

  For `rename_with()`: additional arguments passed onto `.fn`.

- .fn:

  A function used to transform the selected `.cols`. Should return a
  character vector the same length as the input.

- .cols:

  \<[`tidy-select`](https://dplyr.tidyverse.org/dev/reference/dplyr_tidy_select.md)\>
  Columns to rename; defaults to all columns.

## Value

An object of the same type as `.data`. The output has the following
properties:

- Rows are not affected.

- Column names are changed; column order is preserved.

- Data frame attributes are preserved.

- Groups are updated to reflect new names.

## Methods

This function is a **generic**, which means that packages can provide
implementations (methods) for other classes. See the documentation of
individual methods for extra arguments and differences in behaviour.

The following methods are currently available in loaded packages: dbplyr
([`tbl_lazy`](https://dbplyr.tidyverse.org/reference/select.tbl_lazy.html)),
dplyr (`data.frame`) .

## See also

Other single table verbs:
[`arrange()`](https://dplyr.tidyverse.org/dev/reference/arrange.md),
[`filter()`](https://dplyr.tidyverse.org/dev/reference/filter.md),
[`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md),
[`reframe()`](https://dplyr.tidyverse.org/dev/reference/reframe.md),
[`select()`](https://dplyr.tidyverse.org/dev/reference/select.md),
[`slice()`](https://dplyr.tidyverse.org/dev/reference/slice.md),
[`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)

## Examples

``` r
iris <- as_tibble(iris) # so it prints a little nicer
rename(iris, petal_length = Petal.Length)
#> # A tibble: 150 × 5
#>    Sepal.Length Sepal.Width petal_length Petal.Width Species
#>           <dbl>       <dbl>        <dbl>       <dbl> <fct>  
#>  1          5.1         3.5          1.4         0.2 setosa 
#>  2          4.9         3            1.4         0.2 setosa 
#>  3          4.7         3.2          1.3         0.2 setosa 
#>  4          4.6         3.1          1.5         0.2 setosa 
#>  5          5           3.6          1.4         0.2 setosa 
#>  6          5.4         3.9          1.7         0.4 setosa 
#>  7          4.6         3.4          1.4         0.3 setosa 
#>  8          5           3.4          1.5         0.2 setosa 
#>  9          4.4         2.9          1.4         0.2 setosa 
#> 10          4.9         3.1          1.5         0.1 setosa 
#> # ℹ 140 more rows

# Rename using a named vector and `all_of()`
lookup <- c(pl = "Petal.Length", sl = "Sepal.Length")
rename(iris, all_of(lookup))
#> # A tibble: 150 × 5
#>       sl Sepal.Width    pl Petal.Width Species
#>    <dbl>       <dbl> <dbl>       <dbl> <fct>  
#>  1   5.1         3.5   1.4         0.2 setosa 
#>  2   4.9         3     1.4         0.2 setosa 
#>  3   4.7         3.2   1.3         0.2 setosa 
#>  4   4.6         3.1   1.5         0.2 setosa 
#>  5   5           3.6   1.4         0.2 setosa 
#>  6   5.4         3.9   1.7         0.4 setosa 
#>  7   4.6         3.4   1.4         0.3 setosa 
#>  8   5           3.4   1.5         0.2 setosa 
#>  9   4.4         2.9   1.4         0.2 setosa 
#> 10   4.9         3.1   1.5         0.1 setosa 
#> # ℹ 140 more rows

# If your named vector might contain names that don't exist in the data,
# use `any_of()` instead
lookup <- c(lookup, new = "unknown")
try(rename(iris, all_of(lookup)))
#> Error in rename(iris, all_of(lookup)) : 
#>   ℹ In argument: `all_of(lookup)`.
#> Caused by error in `all_of()` at rlang/R/eval-tidy.R:121:3:
#> ! Can't subset elements that don't exist.
#> ✖ Element `unknown` doesn't exist.
rename(iris, any_of(lookup))
#> # A tibble: 150 × 5
#>       sl Sepal.Width    pl Petal.Width Species
#>    <dbl>       <dbl> <dbl>       <dbl> <fct>  
#>  1   5.1         3.5   1.4         0.2 setosa 
#>  2   4.9         3     1.4         0.2 setosa 
#>  3   4.7         3.2   1.3         0.2 setosa 
#>  4   4.6         3.1   1.5         0.2 setosa 
#>  5   5           3.6   1.4         0.2 setosa 
#>  6   5.4         3.9   1.7         0.4 setosa 
#>  7   4.6         3.4   1.4         0.3 setosa 
#>  8   5           3.4   1.5         0.2 setosa 
#>  9   4.4         2.9   1.4         0.2 setosa 
#> 10   4.9         3.1   1.5         0.1 setosa 
#> # ℹ 140 more rows

rename_with(iris, toupper)
#> # A tibble: 150 × 5
#>    SEPAL.LENGTH SEPAL.WIDTH PETAL.LENGTH PETAL.WIDTH SPECIES
#>           <dbl>       <dbl>        <dbl>       <dbl> <fct>  
#>  1          5.1         3.5          1.4         0.2 setosa 
#>  2          4.9         3            1.4         0.2 setosa 
#>  3          4.7         3.2          1.3         0.2 setosa 
#>  4          4.6         3.1          1.5         0.2 setosa 
#>  5          5           3.6          1.4         0.2 setosa 
#>  6          5.4         3.9          1.7         0.4 setosa 
#>  7          4.6         3.4          1.4         0.3 setosa 
#>  8          5           3.4          1.5         0.2 setosa 
#>  9          4.4         2.9          1.4         0.2 setosa 
#> 10          4.9         3.1          1.5         0.1 setosa 
#> # ℹ 140 more rows
rename_with(iris, toupper, starts_with("Petal"))
#> # A tibble: 150 × 5
#>    Sepal.Length Sepal.Width PETAL.LENGTH PETAL.WIDTH Species
#>           <dbl>       <dbl>        <dbl>       <dbl> <fct>  
#>  1          5.1         3.5          1.4         0.2 setosa 
#>  2          4.9         3            1.4         0.2 setosa 
#>  3          4.7         3.2          1.3         0.2 setosa 
#>  4          4.6         3.1          1.5         0.2 setosa 
#>  5          5           3.6          1.4         0.2 setosa 
#>  6          5.4         3.9          1.7         0.4 setosa 
#>  7          4.6         3.4          1.4         0.3 setosa 
#>  8          5           3.4          1.5         0.2 setosa 
#>  9          4.4         2.9          1.4         0.2 setosa 
#> 10          4.9         3.1          1.5         0.1 setosa 
#> # ℹ 140 more rows
rename_with(iris, ~ tolower(gsub(".", "_", .x, fixed = TRUE)))
#> # A tibble: 150 × 5
#>    sepal_length sepal_width petal_length petal_width species
#>           <dbl>       <dbl>        <dbl>       <dbl> <fct>  
#>  1          5.1         3.5          1.4         0.2 setosa 
#>  2          4.9         3            1.4         0.2 setosa 
#>  3          4.7         3.2          1.3         0.2 setosa 
#>  4          4.6         3.1          1.5         0.2 setosa 
#>  5          5           3.6          1.4         0.2 setosa 
#>  6          5.4         3.9          1.7         0.4 setosa 
#>  7          4.6         3.4          1.4         0.3 setosa 
#>  8          5           3.4          1.5         0.2 setosa 
#>  9          4.4         2.9          1.4         0.2 setosa 
#> 10          4.9         3.1          1.5         0.1 setosa 
#> # ℹ 140 more rows

# If your renaming function uses `paste0()`, make sure to set
# `recycle0 = TRUE` to ensure that empty selections are recycled correctly
try(rename_with(
  iris,
  ~ paste0("prefix_", .x),
  starts_with("nonexistent")
))
#> Error in rename_with(iris, ~paste0("prefix_", .x), starts_with("nonexistent")) : 
#>   `.fn` must return a vector of length 0, not 1.

rename_with(
  iris,
  ~ paste0("prefix_", .x, recycle0 = TRUE),
  starts_with("nonexistent")
)
#> # A tibble: 150 × 5
#>    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#>           <dbl>       <dbl>        <dbl>       <dbl> <fct>  
#>  1          5.1         3.5          1.4         0.2 setosa 
#>  2          4.9         3            1.4         0.2 setosa 
#>  3          4.7         3.2          1.3         0.2 setosa 
#>  4          4.6         3.1          1.5         0.2 setosa 
#>  5          5           3.6          1.4         0.2 setosa 
#>  6          5.4         3.9          1.7         0.4 setosa 
#>  7          4.6         3.4          1.4         0.3 setosa 
#>  8          5           3.4          1.5         0.2 setosa 
#>  9          4.4         2.9          1.4         0.2 setosa 
#> 10          4.9         3.1          1.5         0.1 setosa 
#> # ℹ 140 more rows
```
