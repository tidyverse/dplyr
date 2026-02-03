# List variables provided by a tbl.

`tbl_vars()` returns all variables while `tbl_nongroup_vars()` returns
only non-grouping variables. The `groups` attribute of the object
returned by `tbl_vars()` is a character vector of the grouping columns.

## Usage

``` r
tbl_vars(x)

tbl_nongroup_vars(x)
```

## Arguments

- x:

  A tbl object

## See also

[`group_vars()`](https://dplyr.tidyverse.org/reference/group_data.md)
for a function that returns grouping variables.
