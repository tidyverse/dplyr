# Grouping metadata

This collection of functions accesses data about grouped data frames in
various ways:

- `group_data()` returns a data frame that defines the grouping
  structure. The columns give the values of the grouping variables. The
  last column, always called `.rows`, is a list of integer vectors that
  gives the location of the rows in each group.

- `group_keys()` returns a data frame describing the groups.

- `group_rows()` returns a list of integer vectors giving the rows that
  each group contains.

- `group_indices()` returns an integer vector the same length as `.data`
  that gives the group that each row belongs to.

- `group_vars()` gives names of grouping variables as character vector.

- `groups()` gives the names of the grouping variables as a list of
  symbols.

- `group_size()` gives the size of each group.

- `n_groups()` gives the total number of groups.

See [context](https://dplyr.tidyverse.org/dev/reference/context.md) for
equivalent functions that return values for the *current* group.

## Usage

``` r
group_data(.data)

group_keys(.tbl, ...)

group_rows(.data)

group_indices(.data, ...)

group_vars(x)

groups(x)

group_size(x)

n_groups(x)
```

## Arguments

- .data, .tbl, x:

  A data frame or extension (like a tibble or grouped tibble).

- ...:

  Unused.

## Examples

``` r
df <- tibble(x = c(1,1,2,2))
group_vars(df)
#> character(0)
group_rows(df)
#> <list_of<integer>[1]>
#> [[1]]
#> [1] 1 2 3 4
#> 
group_data(df)
#> # A tibble: 1 × 1
#>         .rows
#>   <list<int>>
#> 1         [4]
group_indices(df)
#> [1] 1 1 1 1

gf <- group_by(df, x)
group_vars(gf)
#> [1] "x"
group_rows(gf)
#> <list_of<integer>[2]>
#> [[1]]
#> [1] 1 2
#> 
#> [[2]]
#> [1] 3 4
#> 
group_data(gf)
#> # A tibble: 2 × 2
#>       x       .rows
#>   <dbl> <list<int>>
#> 1     1         [2]
#> 2     2         [2]
group_indices(gf)
#> [1] 1 1 2 2
```
