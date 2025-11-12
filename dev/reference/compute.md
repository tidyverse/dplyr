# Force computation of a database query

`compute()` stores results in a remote temporary table. `collect()`
retrieves data into a local tibble. `collapse()` is slightly different:
it doesn't force computation, but instead forces generation of the SQL
query. This is sometimes needed to work around bugs in dplyr's SQL
generation.

All functions preserve grouping and ordering.

## Usage

``` r
compute(x, ...)

collect(x, ...)

collapse(x, ...)
```

## Arguments

- x:

  A data frame, data frame extension (e.g. a tibble), or a lazy data
  frame (e.g. from dbplyr or dtplyr). See *Methods*, below, for more
  details.

- ...:

  Arguments passed on to methods

## Methods

These functions are **generics**, which means that packages can provide
implementations (methods) for other classes. See the documentation of
individual methods for extra arguments and differences in behaviour.

Methods available in currently loaded packages:

- `compute()`: dbplyr
  ([`tbl_sql`](https://dbplyr.tidyverse.org/reference/collapse.tbl_sql.html)),
  dplyr (`data.frame`)

- `collect()`: dbplyr
  ([`tbl_sql`](https://dbplyr.tidyverse.org/reference/collapse.tbl_sql.html)),
  dplyr (`data.frame`)

- `collapse()`: dbplyr
  ([`tbl_sql`](https://dbplyr.tidyverse.org/reference/collapse.tbl_sql.html)),
  dplyr (`data.frame`)

## See also

[`copy_to()`](https://dplyr.tidyverse.org/dev/reference/copy_to.md), the
opposite of `collect()`: it takes a local data frame and uploads it to
the remote source.

## Examples

``` r
mtcars2 <- dbplyr::src_memdb() |>
  copy_to(mtcars, name = "mtcars2-cc", overwrite = TRUE)

remote <- mtcars2 |>
  filter(cyl == 8) |>
  select(mpg:drat)

# Compute query and save in remote table
compute(remote)
#> # Source:   table<`dbplyr_yFTywINVDl`> [?? x 5]
#> # Database: sqlite 3.51.0 [:memory:]
#>      mpg   cyl  disp    hp  drat
#>    <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1  18.7     8  360    175  3.15
#>  2  14.3     8  360    245  3.21
#>  3  16.4     8  276.   180  3.07
#>  4  17.3     8  276.   180  3.07
#>  5  15.2     8  276.   180  3.07
#>  6  10.4     8  472    205  2.93
#>  7  10.4     8  460    215  3   
#>  8  14.7     8  440    230  3.23
#>  9  15.5     8  318    150  2.76
#> 10  15.2     8  304    150  3.15
#> 11  13.3     8  350    245  3.73
#> 12  19.2     8  400    175  3.08
#> 13  15.8     8  351    264  4.22
#> 14  15       8  301    335  3.54

# Compute query bring back to this session
collect(remote)
#> # A tibble: 14 × 5
#>      mpg   cyl  disp    hp  drat
#>    <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1  18.7     8  360    175  3.15
#>  2  14.3     8  360    245  3.21
#>  3  16.4     8  276.   180  3.07
#>  4  17.3     8  276.   180  3.07
#>  5  15.2     8  276.   180  3.07
#>  6  10.4     8  472    205  2.93
#>  7  10.4     8  460    215  3   
#>  8  14.7     8  440    230  3.23
#>  9  15.5     8  318    150  2.76
#> 10  15.2     8  304    150  3.15
#> 11  13.3     8  350    245  3.73
#> 12  19.2     8  400    175  3.08
#> 13  15.8     8  351    264  4.22
#> 14  15       8  301    335  3.54

# Creates a fresh query based on the generated SQL
collapse(remote)
#> # Source:   SQL [?? x 5]
#> # Database: sqlite 3.51.0 [:memory:]
#>      mpg   cyl  disp    hp  drat
#>    <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1  18.7     8  360    175  3.15
#>  2  14.3     8  360    245  3.21
#>  3  16.4     8  276.   180  3.07
#>  4  17.3     8  276.   180  3.07
#>  5  15.2     8  276.   180  3.07
#>  6  10.4     8  472    205  2.93
#>  7  10.4     8  460    215  3   
#>  8  14.7     8  440    230  3.23
#>  9  15.5     8  318    150  2.76
#> 10  15.2     8  304    150  3.15
#> 11  13.3     8  350    245  3.73
#> 12  19.2     8  400    175  3.08
#> 13  15.8     8  351    264  4.22
#> 14  15       8  301    335  3.54
```
