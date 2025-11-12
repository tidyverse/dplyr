# Create, modify, and delete columns

**\[superseded\]**

`transmute()` creates a new data frame containing only the specified
computations. It's superseded because you can perform the same job with
`mutate(.keep = "none")`.

## Usage

``` r
transmute(.data, ...)
```

## Arguments

- .data:

  A data frame, data frame extension (e.g. a tibble), or a lazy data
  frame (e.g. from dbplyr or dtplyr). See *Methods*, below, for more
  details.

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Name-value pairs. The name gives the name of the column in the output.

  The value can be:

  - A vector of length 1, which will be recycled to the correct length.

  - A vector the same length as the current group (or the whole data
    frame if ungrouped).

  - `NULL`, to remove the column.

  - A data frame or tibble, to create multiple columns in the output.

## Value

An object of the same type as `.data`. The output has the following
properties:

- Columns created or modified through `...` will be returned in the
  order specified by `...`.

- Unmodified grouping columns will be placed at the front.

- The number of rows is not affected.

- Columns given the value `NULL` will be removed.

- Groups will be recomputed if a grouping variable is mutated.

- Data frame attributes are preserved.

## Methods

This function is a **generic**, which means that packages can provide
implementations (methods) for other classes. See the documentation of
individual methods for extra arguments and differences in behaviour.

Methods available in currently loaded packages: dbplyr (`tbl_lazy`),
dplyr (`data.frame`) .
