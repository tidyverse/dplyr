# SQL escaping.

These functions are critical when writing functions that translate R
functions to sql functions. Typically a conversion function should
escape all its inputs and return an sql object.

## Usage

``` r
sql(...)
```

## Arguments

- ...:

  Character vectors that will be combined into a single SQL expression.
