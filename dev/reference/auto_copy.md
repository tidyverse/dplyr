# Copy tables to same source, if necessary

Copy tables to same source, if necessary

## Usage

``` r
auto_copy(x, y, copy = FALSE, ...)
```

## Arguments

- x, y:

  `y` will be copied to `x`, if necessary.

- copy:

  If `x` and `y` are not from the same data source, and `copy` is
  `TRUE`, then `y` will be copied into the same src as `x`. This allows
  you to join tables across srcs, but it is a potentially expensive
  operation so you must opt into it.

- ...:

  Other arguments passed on to methods.
