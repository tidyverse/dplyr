# Default value for .drop argument of group_by

Default value for .drop argument of group_by

## Usage

``` r
group_by_drop_default(.tbl)
```

## Arguments

- .tbl:

  A data frame

## Value

`TRUE` unless `.tbl` is a grouped data frame that was previously
obtained by `group_by(.drop = FALSE)`

## Examples

``` r
group_by_drop_default(iris)
#> [1] TRUE

iris |>
  group_by(Species) |>
  group_by_drop_default()
#> [1] TRUE

iris |>
  group_by(Species, .drop = FALSE) |>
  group_by_drop_default()
#> [1] FALSE
```
