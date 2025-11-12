# A grouped data frame.

The easiest way to create a grouped data frame is to call the
[`group_by()`](https://dplyr.tidyverse.org/dev/reference/group_by.md)
method on a data frame or tbl: this will take care of capturing the
unevaluated expressions for you.

These functions are designed for programmatic use. For data analysis
purposes see
[`group_data()`](https://dplyr.tidyverse.org/dev/reference/group_data.md)
for the accessor functions that retrieve various metadata from a grouped
data frames.

## Usage

``` r
grouped_df(data, vars, drop = group_by_drop_default(data))

is.grouped_df(x)

is_grouped_df(x)
```

## Arguments

- data:

  a tbl or data frame.

- vars:

  A character vector.

- drop:

  When `.drop = TRUE`, empty groups are dropped.
