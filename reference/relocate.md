# Change column order

Use `relocate()` to change column positions, using the same syntax as
[`select()`](https://dplyr.tidyverse.org/reference/select.md) to make it
easy to move blocks of columns at once.

## Usage

``` r
relocate(.data, ..., .before = NULL, .after = NULL)
```

## Arguments

- .data:

  A data frame, data frame extension (e.g. a tibble), or a lazy data
  frame (e.g. from dbplyr or dtplyr). See *Methods*, below, for more
  details.

- ...:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.md)\>
  Columns to move.

- .before, .after:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.md)\>
  Destination of columns selected by `...`. Supplying neither will move
  columns to the left-hand side; specifying both is an error.

## Value

An object of the same type as `.data`. The output has the following
properties:

- Rows are not affected.

- The same columns appear in the output, but (usually) in a different
  place and possibly renamed.

- Data frame attributes are preserved.

- Groups are not affected.

## Methods

This function is a **generic**, which means that packages can provide
implementations (methods) for other classes. See the documentation of
individual methods for extra arguments and differences in behaviour.

The following methods are currently available in loaded packages: dbplyr
([`tbl_lazy`](https://dbplyr.tidyverse.org/reference/select.tbl_lazy.html)),
dplyr (`data.frame`) .

## Examples

``` r
df <- tibble(a = 1, b = 1, c = 1, d = "a", e = "a", f = "a")
df |> relocate(f)
#> # A tibble: 1 × 6
#>   f         a     b     c d     e    
#>   <chr> <dbl> <dbl> <dbl> <chr> <chr>
#> 1 a         1     1     1 a     a    
df |> relocate(a, .after = c)
#> # A tibble: 1 × 6
#>       b     c     a d     e     f    
#>   <dbl> <dbl> <dbl> <chr> <chr> <chr>
#> 1     1     1     1 a     a     a    
df |> relocate(f, .before = b)
#> # A tibble: 1 × 6
#>       a f         b     c d     e    
#>   <dbl> <chr> <dbl> <dbl> <chr> <chr>
#> 1     1 a         1     1 a     a    
df |> relocate(a, .after = last_col())
#> # A tibble: 1 × 6
#>       b     c d     e     f         a
#>   <dbl> <dbl> <chr> <chr> <chr> <dbl>
#> 1     1     1 a     a     a         1

# relocated columns can change name
df |> relocate(ff = f)
#> # A tibble: 1 × 6
#>   ff        a     b     c d     e    
#>   <chr> <dbl> <dbl> <dbl> <chr> <chr>
#> 1 a         1     1     1 a     a    

# Can also select variables based on their type
df |> relocate(where(is.character))
#> # A tibble: 1 × 6
#>   d     e     f         a     b     c
#>   <chr> <chr> <chr> <dbl> <dbl> <dbl>
#> 1 a     a     a         1     1     1
df |> relocate(where(is.numeric), .after = last_col())
#> # A tibble: 1 × 6
#>   d     e     f         a     b     c
#>   <chr> <chr> <chr> <dbl> <dbl> <dbl>
#> 1 a     a     a         1     1     1
# Or with any other select helper
df |> relocate(any_of(c("a", "e", "i", "o", "u")))
#> # A tibble: 1 × 6
#>       a e         b     c d     f    
#>   <dbl> <chr> <dbl> <dbl> <chr> <chr>
#> 1     1 a         1     1 a     a    

# When .before or .after refers to multiple variables they will be
# moved to be immediately before/after the selected variables.
df2 <- tibble(a = 1, b = "a", c = 1, d = "a")
df2 |> relocate(where(is.numeric), .after = where(is.character))
#> # A tibble: 1 × 4
#>   b     d         a     c
#>   <chr> <chr> <dbl> <dbl>
#> 1 a     a         1     1
df2 |> relocate(where(is.numeric), .before = where(is.character))
#> # A tibble: 1 × 4
#>       a     c b     d    
#>   <dbl> <dbl> <chr> <chr>
#> 1     1     1 a     a    
```
