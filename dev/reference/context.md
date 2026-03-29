# Information about the "current" group or variable

These functions return information about the "current" group or
"current" variable, so only work inside specific contexts like
[`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)
and [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md).

- `n()` gives the current group size.

- `cur_group()` gives the group keys, a tibble with one row and one
  column for each grouping variable.

- `cur_group_id()` gives a unique numeric identifier for the current
  group.

- `cur_group_rows()` gives the row indices for the current group.

- `cur_column()` gives the name of the current column (in
  [`across()`](https://dplyr.tidyverse.org/dev/reference/across.md)
  only).

See
[`group_data()`](https://dplyr.tidyverse.org/dev/reference/group_data.md)
for equivalent functions that return values for all groups.

See [`pick()`](https://dplyr.tidyverse.org/dev/reference/pick.md) for a
way to select a subset of columns using tidyselect syntax while inside
[`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)
or [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md).

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

See [`pick()`](https://dplyr.tidyverse.org/dev/reference/pick.md) for an
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
#>   g         x     y    id
#>   <chr> <dbl> <dbl> <int>
#> 1 a     0.721 0.648     1
#> 2 b     0.142 0.320     2
#> 3 c     0.549 0.308     3
#> 4 b     0.954 0.220     2
#> 5 c     0.585 0.369     3
#> 6 c     0.405 0.984     3
gf |> reframe(row = cur_group_rows())
#> # A tibble: 6 × 2
#>   g       row
#>   <chr> <int>
#> 1 a         1
#> 2 b         2
#> 3 b         4
#> 4 c         3
#> 5 c         5
#> 6 c         6
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
#> 1 a     x 0.72 y 0.65
#> 2 b     x 0.14 y 0.32
#> 3 c     x 0.55 y 0.31
#> 4 b     x 0.95 y 0.22
#> 5 c     x 0.59 y 0.37
#> 6 c     x 0.4  y 0.98
```
