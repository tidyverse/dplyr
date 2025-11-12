# Create a "tbl" object

[`tbl()`](https://dplyr.tidyverse.org/dev/reference/tbl.md) is the
standard constructor for tbls.
[`is.tbl()`](https://dplyr.tidyverse.org/dev/reference/tbl.md) tests.

## Usage

``` r
make_tbl(subclass, ...)
```

## Arguments

- subclass:

  name of subclass. "tbl" is an abstract base class, so you must supply
  this value. `tbl_` is automatically prepended to the class name

- ...:

  For [`tbl()`](https://dplyr.tidyverse.org/dev/reference/tbl.md), other
  fields used by class.
