# Return a prototype of a tbl

Used in `_if` functions to enable type-based selection even when the
data is lazily generated. Should either return the complete tibble, or
if that can not be computed quickly, a 0-row tibble where the columns
are of the correct type.

## Usage

``` r
tbl_ptype(.data)
```
