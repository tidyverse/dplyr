# Prepare for grouping and other operations

`*_prepare()` performs standard manipulation that is needed prior to
actual data processing. They are only be needed by packages that
implement dplyr backends.

## Usage

``` r
distinct_prepare(
  .data,
  vars,
  group_vars = character(),
  .keep_all = FALSE,
  caller_env = caller_env(2),
  error_call = caller_env()
)

group_by_prepare(
  .data,
  ...,
  .add = FALSE,
  .dots = deprecated(),
  add = deprecated(),
  error_call = caller_env()
)
```

## Value

A list

- data:

  Modified tbl

- groups:

  Modified groups
