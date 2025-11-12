# Information about the "current" group or variable

**\[deprecated\]**

These functions were deprecated in dplyr 1.1.0.

- `cur_data()` is deprecated in favor of
  [`pick()`](https://dplyr.tidyverse.org/dev/reference/pick.md).

- `cur_data_all()` is deprecated but does not have a direct replacement
  as selecting the grouping variables is not well-defined and is
  unlikely to ever be useful.

## Usage

``` r
cur_data()

cur_data_all()
```
