# Select grouping variables

This selection helpers matches grouping variables. It can be used in
[`select()`](https://dplyr.tidyverse.org/dev/reference/select.md) or
[`vars()`](https://dplyr.tidyverse.org/dev/reference/vars.md)
selections.

## Usage

``` r
group_cols(vars = NULL, data = NULL)
```

## Arguments

- vars:

  **\[defunct\]**

- data:

  For advanced use only. The default `NULL` automatically finds the
  "current" data frames.

## See also

[`groups()`](https://dplyr.tidyverse.org/dev/reference/group_data.md)
and
[`group_vars()`](https://dplyr.tidyverse.org/dev/reference/group_data.md)
for retrieving the grouping variables outside selection contexts.

## Examples

``` r
gdf <- iris |> group_by(Species)
gdf |> select(group_cols())
#> # A tibble: 150 × 1
#> # Groups:   Species [3]
#>    Species
#>    <fct>  
#>  1 setosa 
#>  2 setosa 
#>  3 setosa 
#>  4 setosa 
#>  5 setosa 
#>  6 setosa 
#>  7 setosa 
#>  8 setosa 
#>  9 setosa 
#> 10 setosa 
#> # ℹ 140 more rows

# Remove the grouping variables from mutate selections:
gdf |> mutate_at(vars(-group_cols()), `/`, 100)
#> # A tibble: 150 × 5
#> # Groups:   Species [3]
#>    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#>           <dbl>       <dbl>        <dbl>       <dbl> <fct>  
#>  1        0.051       0.035        0.014       0.002 setosa 
#>  2        0.049       0.03         0.014       0.002 setosa 
#>  3        0.047       0.032        0.013       0.002 setosa 
#>  4        0.046       0.031        0.015       0.002 setosa 
#>  5        0.05        0.036        0.014       0.002 setosa 
#>  6        0.054       0.039        0.017       0.004 setosa 
#>  7        0.046       0.034        0.014       0.003 setosa 
#>  8        0.05        0.034        0.015       0.002 setosa 
#>  9        0.044       0.029        0.014       0.002 setosa 
#> 10        0.049       0.031        0.015       0.001 setosa 
#> # ℹ 140 more rows
# -> No longer necessary with across()
gdf |> mutate(across(everything(), ~ . / 100))
#> # A tibble: 150 × 5
#> # Groups:   Species [3]
#>    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#>           <dbl>       <dbl>        <dbl>       <dbl> <fct>  
#>  1        0.051       0.035        0.014       0.002 setosa 
#>  2        0.049       0.03         0.014       0.002 setosa 
#>  3        0.047       0.032        0.013       0.002 setosa 
#>  4        0.046       0.031        0.015       0.002 setosa 
#>  5        0.05        0.036        0.014       0.002 setosa 
#>  6        0.054       0.039        0.017       0.004 setosa 
#>  7        0.046       0.034        0.014       0.003 setosa 
#>  8        0.05        0.034        0.015       0.002 setosa 
#>  9        0.044       0.029        0.014       0.002 setosa 
#> 10        0.049       0.031        0.015       0.001 setosa 
#> # ℹ 140 more rows
```
