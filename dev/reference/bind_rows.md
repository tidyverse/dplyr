# Bind multiple data frames by row

Bind any number of data frames by row, making a longer result. This is
similar to `do.call(rbind, dfs)`, but the output will contain all
columns that appear in any of the inputs.

## Usage

``` r
bind_rows(..., .id = NULL)
```

## Arguments

- ...:

  Data frames to combine. Each argument can either be a data frame, a
  list that could be a data frame, or a list of data frames. Columns are
  matched by name, and any missing columns will be filled with `NA`.

- .id:

  The name of an optional identifier column. Provide a string to create
  an output column that identifies each input. The column will use names
  if available, otherwise it will use positions.

## Value

A data frame the same type as the first element of `...`.

## Examples

``` r
df1 <- tibble(x = 1:2, y = letters[1:2])
df2 <- tibble(x = 4:5, z = 1:2)

# You can supply individual data frames as arguments:
bind_rows(df1, df2)
#> # A tibble: 4 × 3
#>       x y         z
#>   <int> <chr> <int>
#> 1     1 a        NA
#> 2     2 b        NA
#> 3     4 NA        1
#> 4     5 NA        2

# Or a list of data frames:
bind_rows(list(df1, df2))
#> # A tibble: 4 × 3
#>       x y         z
#>   <int> <chr> <int>
#> 1     1 a        NA
#> 2     2 b        NA
#> 3     4 NA        1
#> 4     5 NA        2

# When you supply a column name with the `.id` argument, a new
# column is created to link each row to its original data frame
bind_rows(list(df1, df2), .id = "id")
#> # A tibble: 4 × 4
#>   id        x y         z
#>   <chr> <int> <chr> <int>
#> 1 1         1 a        NA
#> 2 1         2 b        NA
#> 3 2         4 NA        1
#> 4 2         5 NA        2
bind_rows(list(a = df1, b = df2), .id = "id")
#> # A tibble: 4 × 4
#>   id        x y         z
#>   <chr> <int> <chr> <int>
#> 1 a         1 a        NA
#> 2 a         2 b        NA
#> 3 b         4 NA        1
#> 4 b         5 NA        2
```
