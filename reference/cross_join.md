# Cross join

Cross joins match each row in `x` to every row in `y`, resulting in a
data frame with `nrow(x) * nrow(y)` rows.

Since cross joins result in all possible matches between `x` and `y`,
they technically serve as the basis for all [mutating
joins](https://dplyr.tidyverse.org/reference/mutate-joins.md), which can
generally be thought of as cross joins followed by a filter. In
practice, a more specialized procedure is used for better performance.

## Usage

``` r
cross_join(x, y, ..., copy = FALSE, suffix = c(".x", ".y"))
```

## Arguments

- x, y:

  A pair of data frames, data frame extensions (e.g. a tibble), or lazy
  data frames (e.g. from dbplyr or dtplyr). See *Methods*, below, for
  more details.

- ...:

  Other parameters passed onto methods.

- copy:

  If `x` and `y` are not from the same data source, and `copy` is
  `TRUE`, then `y` will be copied into the same src as `x`. This allows
  you to join tables across srcs, but it is a potentially expensive
  operation so you must opt into it.

- suffix:

  If there are non-joined duplicate variables in `x` and `y`, these
  suffixes will be added to the output to disambiguate them. Should be a
  character vector of length 2.

## Value

An object of the same type as `x` (including the same groups). The
output has the following properties:

- There are `nrow(x) * nrow(y)` rows returned.

- Output columns include all columns from both `x` and `y`. Column name
  collisions are resolved using `suffix`.

- The order of the rows and columns of `x` is preserved as much as
  possible.

## Methods

This function is a **generic**, which means that packages can provide
implementations (methods) for other classes. See the documentation of
individual methods for extra arguments and differences in behaviour.

The following methods are currently available in loaded packages: dbplyr
([`tbl_lazy`](https://dbplyr.tidyverse.org/reference/join.tbl_sql.html)),
dplyr (`data.frame`) .

## See also

Other joins:
[`filter-joins`](https://dplyr.tidyverse.org/reference/filter-joins.md),
[`mutate-joins`](https://dplyr.tidyverse.org/reference/mutate-joins.md),
[`nest_join()`](https://dplyr.tidyverse.org/reference/nest_join.md)

## Examples

``` r
# Cross joins match each row in `x` to every row in `y`.
# Data within the columns is not used in the matching process.
cross_join(band_instruments, band_members)
#> # A tibble: 9 × 4
#>   name.x plays  name.y band   
#>   <chr>  <chr>  <chr>  <chr>  
#> 1 John   guitar Mick   Stones 
#> 2 John   guitar John   Beatles
#> 3 John   guitar Paul   Beatles
#> 4 Paul   bass   Mick   Stones 
#> 5 Paul   bass   John   Beatles
#> 6 Paul   bass   Paul   Beatles
#> 7 Keith  guitar Mick   Stones 
#> 8 Keith  guitar John   Beatles
#> 9 Keith  guitar Paul   Beatles

# Control the suffix added to variables duplicated in
# `x` and `y` with `suffix`.
cross_join(band_instruments, band_members, suffix = c("", "_y"))
#> # A tibble: 9 × 4
#>   name  plays  name_y band   
#>   <chr> <chr>  <chr>  <chr>  
#> 1 John  guitar Mick   Stones 
#> 2 John  guitar John   Beatles
#> 3 John  guitar Paul   Beatles
#> 4 Paul  bass   Mick   Stones 
#> 5 Paul  bass   John   Beatles
#> 6 Paul  bass   Paul   Beatles
#> 7 Keith guitar Mick   Stones 
#> 8 Keith guitar John   Beatles
#> 9 Keith guitar Paul   Beatles
```
