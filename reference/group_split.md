# Split data frame by groups

**\[experimental\]**

`group_split()` works like
[`base::split()`](https://rdrr.io/r/base/split.html) but:

- It uses the grouping structure from
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.md) and
  therefore is subject to the data mask

- It does not name the elements of the list based on the grouping as
  this only works well for a single character grouping variable.
  Instead, use
  [`group_keys()`](https://dplyr.tidyverse.org/reference/group_data.md)
  to access a data frame that defines the groups.

`group_split()` is primarily designed to work with grouped data frames.
You can pass `...` to group and split an ungrouped data frame, but this
is generally not very useful as you want have easy access to the group
metadata.

## Usage

``` r
group_split(.tbl, ..., .keep = TRUE)
```

## Arguments

- .tbl:

  A tbl.

- ...:

  If `.tbl` is an ungrouped data frame, a grouping specification,
  forwarded to
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.md).

- .keep:

  Should the grouping columns be kept?

## Value

A list of tibbles. Each tibble contains the rows of `.tbl` for the
associated group and all the columns, including the grouping variables.
Note that this returns a
[list_of](https://vctrs.r-lib.org/reference/list_of.html) which is
slightly stricter than a simple list but is useful for representing
lists where every element has the same type.

## Lifecycle

`group_split()` is not stable because you can achieve very similar
results by manipulating the nested column returned from
[`tidyr::nest(.by =)`](https://tidyr.tidyverse.org/reference/nest.html).
That also retains the group keys all within a single data structure.
`group_split()` may be deprecated in the future.

## See also

Other grouping functions:
[`group_by()`](https://dplyr.tidyverse.org/reference/group_by.md),
[`group_map()`](https://dplyr.tidyverse.org/reference/group_map.md),
[`group_nest()`](https://dplyr.tidyverse.org/reference/group_nest.md),
[`group_trim()`](https://dplyr.tidyverse.org/reference/group_trim.md)

## Examples

``` r
ir <- iris |> group_by(Species)

group_split(ir)
#> <list_of<
#>   tbl_df<
#>     Sepal.Length: double
#>     Sepal.Width : double
#>     Petal.Length: double
#>     Petal.Width : double
#>     Species     : factor<fb977>
#>   >
#> >[3]>
#> [[1]]
#> # A tibble: 50 × 5
#>    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#>           <dbl>       <dbl>        <dbl>       <dbl> <fct>  
#>  1          5.1         3.5          1.4         0.2 setosa 
#>  2          4.9         3            1.4         0.2 setosa 
#>  3          4.7         3.2          1.3         0.2 setosa 
#>  4          4.6         3.1          1.5         0.2 setosa 
#>  5          5           3.6          1.4         0.2 setosa 
#>  6          5.4         3.9          1.7         0.4 setosa 
#>  7          4.6         3.4          1.4         0.3 setosa 
#>  8          5           3.4          1.5         0.2 setosa 
#>  9          4.4         2.9          1.4         0.2 setosa 
#> 10          4.9         3.1          1.5         0.1 setosa 
#> # ℹ 40 more rows
#> 
#> [[2]]
#> # A tibble: 50 × 5
#>    Sepal.Length Sepal.Width Petal.Length Petal.Width Species   
#>           <dbl>       <dbl>        <dbl>       <dbl> <fct>     
#>  1          7           3.2          4.7         1.4 versicolor
#>  2          6.4         3.2          4.5         1.5 versicolor
#>  3          6.9         3.1          4.9         1.5 versicolor
#>  4          5.5         2.3          4           1.3 versicolor
#>  5          6.5         2.8          4.6         1.5 versicolor
#>  6          5.7         2.8          4.5         1.3 versicolor
#>  7          6.3         3.3          4.7         1.6 versicolor
#>  8          4.9         2.4          3.3         1   versicolor
#>  9          6.6         2.9          4.6         1.3 versicolor
#> 10          5.2         2.7          3.9         1.4 versicolor
#> # ℹ 40 more rows
#> 
#> [[3]]
#> # A tibble: 50 × 5
#>    Sepal.Length Sepal.Width Petal.Length Petal.Width Species  
#>           <dbl>       <dbl>        <dbl>       <dbl> <fct>    
#>  1          6.3         3.3          6           2.5 virginica
#>  2          5.8         2.7          5.1         1.9 virginica
#>  3          7.1         3            5.9         2.1 virginica
#>  4          6.3         2.9          5.6         1.8 virginica
#>  5          6.5         3            5.8         2.2 virginica
#>  6          7.6         3            6.6         2.1 virginica
#>  7          4.9         2.5          4.5         1.7 virginica
#>  8          7.3         2.9          6.3         1.8 virginica
#>  9          6.7         2.5          5.8         1.8 virginica
#> 10          7.2         3.6          6.1         2.5 virginica
#> # ℹ 40 more rows
#> 
group_keys(ir)
#> # A tibble: 3 × 1
#>   Species   
#>   <fct>     
#> 1 setosa    
#> 2 versicolor
#> 3 virginica 
```
