# Run a function with one order, translating result back to original order

This is used to power the ordering parameters of dplyr's window
functions

## Usage

``` r
with_order(order_by, fun, x, ...)
```

## Arguments

- order_by:

  vector to order by

- fun:

  window function

- x, ...:

  arguments to `f`
