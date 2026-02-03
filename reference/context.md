# Information about the "current" group or variable

These functions return information about the "current" group or
"current" variable, so only work inside specific contexts like
[`summarise()`](https://dplyr.tidyverse.org/reference/summarise.md) and
[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.md).

- `n()` gives the current group size.

- `cur_group()` gives the group keys, a tibble with one row and one
  column for each grouping variable.

- `cur_group_id()` gives a unique numeric identifier for the current
  group.

- `cur_group_rows()` gives the row indices for the current group.

- `cur_column()` gives the name of the current column (in
  [`across()`](https://dplyr.tidyverse.org/reference/across.md) only).

See
[`group_data()`](https://dplyr.tidyverse.org/reference/group_data.md)
for equivalent functions that return values for all groups.

See [`pick()`](https://dplyr.tidyverse.org/reference/pick.md) for a way
to select a subset of columns using tidyselect syntax while inside
[`summarise()`](https://dplyr.tidyverse.org/reference/summarise.md) or
[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.md).

## Usage

``` r
n()

cur_group()

cur_group_id()

cur_group_rows()

cur_column()
```

## data.table

If you're familiar with data.table:

- `cur_group_id()` \<-\> `.GRP`

- `cur_group()` \<-\> `.BY`

- `cur_group_rows()` \<-\> `.I`

See [`pick()`](https://dplyr.tidyverse.org/reference/pick.md) for an
equivalent to `.SD`.

## Examples

``` r
df <- tibble(
  g = sample(rep(letters[1:3], 1:3)),
  x = runif(6),
  y = runif(6)
)
gf <- df |> group_by(g)

gf |> summarise(n = n())
#> # A tibble: 3 × 2
#>   g         n
#>   <chr> <int>
#> 1 a         1
#> 2 b         2
#> 3 c         3

gf |> mutate(id = cur_group_id())
#> # A tibble: 6 × 4
#> # Groups:   g [3]
#>   g          x     y    id
#>   <chr>  <dbl> <dbl> <int>
#> 1 b     0.984  0.891     2
#> 2 c     0.154  0.673     3
#> 3 c     0.0910 0.737     3
#> 4 c     0.142  0.521     3
#> 5 a     0.690  0.660     1
#> 6 b     0.619  0.822     2
gf |> reframe(row = cur_group_rows())
#> # A tibble: 6 × 2
#>   g       row
#>   <chr> <int>
#> 1 a         5
#> 2 b         1
#> 3 b         6
#> 4 c         2
#> 5 c         3
#> 6 c         4
gf |> summarise(data = list(cur_group()))
#> # A tibble: 3 × 2
#>   g     data            
#>   <chr> <list>          
#> 1 a     <tibble [1 × 1]>
#> 2 b     <tibble [1 × 1]>
#> 3 c     <tibble [1 × 1]>

gf |> mutate(across(everything(), ~ paste(cur_column(), round(.x, 2))))
#> # A tibble: 6 × 3
#> # Groups:   g [3]
#>   g     x      y     
#>   <chr> <chr>  <chr> 
#> 1 b     x 0.98 y 0.89
#> 2 c     x 0.15 y 0.67
#> 3 c     x 0.09 y 0.74
#> 4 c     x 0.14 y 0.52
#> 5 a     x 0.69 y 0.66
#> 6 b     x 0.62 y 0.82
```
