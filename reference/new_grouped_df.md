# Low-level construction and validation for the grouped_df and rowwise_df classes

`new_grouped_df()` and `new_rowwise_df()` are constructors designed to
be high-performance so only check types, not values. This means it is
the caller's responsibility to create valid values, and hence this is
for expert use only.

`validate_grouped_df()` and `validate_rowwise_df()` validate the
attributes of a `grouped_df` or a `rowwise_df`.

## Usage

``` r
new_grouped_df(x, groups, ..., class = character())

validate_grouped_df(x, check_bounds = FALSE)

new_rowwise_df(data, group_data = NULL, ..., class = character())

validate_rowwise_df(x)
```

## Arguments

- x:

  A data frame

- groups:

  The grouped structure, `groups` should be a data frame. Its last
  column should be called `.rows` and be a list of 1 based integer
  vectors that all are between 1 and the number of rows of `.data`.

- ...:

  additional attributes

- class:

  additional class, will be prepended to canonical classes.

- check_bounds:

  whether to check all indices for out of bounds problems in
  `grouped_df` objects

## Examples

``` r
# 5 bootstrap samples
tbl <- new_grouped_df(
  tibble(x = rnorm(10)),
  groups = tibble(".rows" := replicate(5, sample(1:10, replace = TRUE), simplify = FALSE))
)
# mean of each bootstrap sample
summarise(tbl, x = mean(x))
#> # A tibble: 5 × 1
#>        x
#>    <dbl>
#> 1 -0.523
#> 2 -0.147
#> 3 -0.152
#> 4 -0.103
#> 5  0.507
```
