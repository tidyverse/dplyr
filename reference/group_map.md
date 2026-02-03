# Apply a function to each group

**\[experimental\]**

`group_map()`, `group_modify()` and `group_walk()` are purrr-style
functions that can be used to iterate on grouped tibbles.

## Usage

``` r
group_map(.data, .f, ..., .keep = FALSE)

group_modify(.data, .f, ..., .keep = FALSE)

group_walk(.data, .f, ..., .keep = FALSE)
```

## Arguments

- .data:

  A grouped tibble

- .f:

  A function or formula to apply to each group.

  If a **function**, it is used as is. It should have at least 2 formal
  arguments.

  If a **formula**, e.g. `~ head(.x)`, it is converted to a function.

  In the formula, you can use

  - `.` or `.x` to refer to the subset of rows of `.tbl` for the given
    group

  - `.y` to refer to the key, a one row tibble with one column per
    grouping variable that identifies the group

- ...:

  Additional arguments passed on to `.f`

- .keep:

  are the grouping variables kept in `.x`

## Value

- `group_modify()` returns a grouped tibble. In that case `.f` must
  return a data frame.

- `group_map()` returns a list of results from calling `.f` on each
  group.

- `group_walk()` calls `.f` for side effects and returns the input
  `.tbl`, invisibly.

## Details

Use `group_modify()` when
[`summarize()`](https://dplyr.tidyverse.org/reference/summarise.md) is
too limited, in terms of what you need to do and return for each group.
`group_modify()` is good for "data frame in, data frame out". If that is
too limited, you need to use a
[nested](https://dplyr.tidyverse.org/reference/group_nest.md) or
[split](https://dplyr.tidyverse.org/reference/group_split.md) workflow.
`group_modify()` is an evolution of
[`do()`](https://dplyr.tidyverse.org/reference/do.md), if you have used
that before.

Each conceptual group of the data frame is exposed to the function `.f`
with two pieces of information:

- The subset of the data for the group, exposed as `.x`.

- The key, a tibble with exactly one row and columns for each grouping
  variable, exposed as `.y`.

For completeness, `group_modify()`, `group_map` and `group_walk()` also
work on ungrouped data frames, in that case the function is applied to
the entire data frame (exposed as `.x`), and `.y` is a one row tibble
with no column, consistently with
[`group_keys()`](https://dplyr.tidyverse.org/reference/group_data.md).

## See also

Other grouping functions:
[`group_by()`](https://dplyr.tidyverse.org/reference/group_by.md),
[`group_nest()`](https://dplyr.tidyverse.org/reference/group_nest.md),
[`group_split()`](https://dplyr.tidyverse.org/reference/group_split.md),
[`group_trim()`](https://dplyr.tidyverse.org/reference/group_trim.md)

## Examples

``` r
# return a list
mtcars |>
  group_by(cyl) |>
  group_map(~ head(.x, 2L))
#> [[1]]
#> # A tibble: 2 × 10
#>     mpg  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  22.8  108     93  3.85  2.32  18.6     1     1     4     1
#> 2  24.4  147.    62  3.69  3.19  20       1     0     4     2
#> 
#> [[2]]
#> # A tibble: 2 × 10
#>     mpg  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1    21   160   110   3.9  2.62  16.5     0     1     4     4
#> 2    21   160   110   3.9  2.88  17.0     0     1     4     4
#> 
#> [[3]]
#> # A tibble: 2 × 10
#>     mpg  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  18.7   360   175  3.15  3.44  17.0     0     0     3     2
#> 2  14.3   360   245  3.21  3.57  15.8     0     0     3     4
#> 

# return a tibble grouped by `cyl` with 2 rows per group
# the grouping data is recalculated
mtcars |>
  group_by(cyl) |>
  group_modify(~ head(.x, 2L))
#> # A tibble: 6 × 11
#> # Groups:   cyl [3]
#>     cyl   mpg  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1     4  22.8  108     93  3.85  2.32  18.6     1     1     4     1
#> 2     4  24.4  147.    62  3.69  3.19  20       1     0     4     2
#> 3     6  21    160    110  3.9   2.62  16.5     0     1     4     4
#> 4     6  21    160    110  3.9   2.88  17.0     0     1     4     4
#> 5     8  18.7  360    175  3.15  3.44  17.0     0     0     3     2
#> 6     8  14.3  360    245  3.21  3.57  15.8     0     0     3     4

# a list of tibbles
iris |>
  group_by(Species) |>
  group_map(~ broom::tidy(lm(Petal.Length ~ Sepal.Length, data = .x)))
#> [[1]]
#> # A tibble: 2 × 5
#>   term         estimate std.error statistic p.value
#>   <chr>           <dbl>     <dbl>     <dbl>   <dbl>
#> 1 (Intercept)     0.803    0.344       2.34  0.0238
#> 2 Sepal.Length    0.132    0.0685      1.92  0.0607
#> 
#> [[2]]
#> # A tibble: 2 × 5
#>   term         estimate std.error statistic  p.value
#>   <chr>           <dbl>     <dbl>     <dbl>    <dbl>
#> 1 (Intercept)     0.185    0.514      0.360 7.20e- 1
#> 2 Sepal.Length    0.686    0.0863     7.95  2.59e-10
#> 
#> [[3]]
#> # A tibble: 2 × 5
#>   term         estimate std.error statistic  p.value
#>   <chr>           <dbl>     <dbl>     <dbl>    <dbl>
#> 1 (Intercept)     0.610    0.417       1.46 1.50e- 1
#> 2 Sepal.Length    0.750    0.0630     11.9  6.30e-16
#> 

# a restructured grouped tibble
iris |>
  group_by(Species) |>
  group_modify(~ broom::tidy(lm(Petal.Length ~ Sepal.Length, data = .x)))
#> # A tibble: 6 × 6
#> # Groups:   Species [3]
#>   Species    term         estimate std.error statistic  p.value
#>   <fct>      <chr>           <dbl>     <dbl>     <dbl>    <dbl>
#> 1 setosa     (Intercept)     0.803    0.344      2.34  2.38e- 2
#> 2 setosa     Sepal.Length    0.132    0.0685     1.92  6.07e- 2
#> 3 versicolor (Intercept)     0.185    0.514      0.360 7.20e- 1
#> 4 versicolor Sepal.Length    0.686    0.0863     7.95  2.59e-10
#> 5 virginica  (Intercept)     0.610    0.417      1.46  1.50e- 1
#> 6 virginica  Sepal.Length    0.750    0.0630    11.9   6.30e-16

# a list of vectors
iris |>
  group_by(Species) |>
  group_map(~ quantile(.x$Petal.Length, probs = c(0.25, 0.5, 0.75)))
#> [[1]]
#>   25%   50%   75% 
#> 1.400 1.500 1.575 
#> 
#> [[2]]
#>  25%  50%  75% 
#> 4.00 4.35 4.60 
#> 
#> [[3]]
#>   25%   50%   75% 
#> 5.100 5.550 5.875 
#> 

# to use group_modify() the lambda must return a data frame
iris |>
  group_by(Species) |>
  group_modify(~ {
     quantile(.x$Petal.Length, probs = c(0.25, 0.5, 0.75)) |>
     tibble::enframe(name = "prob", value = "quantile")
  })
#> # A tibble: 9 × 3
#> # Groups:   Species [3]
#>   Species    prob  quantile
#>   <fct>      <chr>    <dbl>
#> 1 setosa     25%       1.4 
#> 2 setosa     50%       1.5 
#> 3 setosa     75%       1.58
#> 4 versicolor 25%       4   
#> 5 versicolor 50%       4.35
#> 6 versicolor 75%       4.6 
#> 7 virginica  25%       5.1 
#> 8 virginica  50%       5.55
#> 9 virginica  75%       5.88

iris |>
  group_by(Species) |>
  group_modify(~ {
    .x |>
      purrr::map_dfc(fivenum) |>
      mutate(nms = c("min", "Q1", "median", "Q3", "max"))
  })
#> # A tibble: 15 × 6
#> # Groups:   Species [3]
#>    Species    Sepal.Length Sepal.Width Petal.Length Petal.Width nms   
#>    <fct>             <dbl>       <dbl>        <dbl>       <dbl> <chr> 
#>  1 setosa              4.3         2.3         1            0.1 min   
#>  2 setosa              4.8         3.2         1.4          0.2 Q1    
#>  3 setosa              5           3.4         1.5          0.2 median
#>  4 setosa              5.2         3.7         1.6          0.3 Q3    
#>  5 setosa              5.8         4.4         1.9          0.6 max   
#>  6 versicolor          4.9         2           3            1   min   
#>  7 versicolor          5.6         2.5         4            1.2 Q1    
#>  8 versicolor          5.9         2.8         4.35         1.3 median
#>  9 versicolor          6.3         3           4.6          1.5 Q3    
#> 10 versicolor          7           3.4         5.1          1.8 max   
#> 11 virginica           4.9         2.2         4.5          1.4 min   
#> 12 virginica           6.2         2.8         5.1          1.8 Q1    
#> 13 virginica           6.5         3           5.55         2   median
#> 14 virginica           6.9         3.2         5.9          2.3 Q3    
#> 15 virginica           7.9         3.8         6.9          2.5 max   

# group_walk() is for side effects
dir.create(temp <- tempfile())
iris |>
  group_by(Species) |>
  group_walk(~ write.csv(.x, file = file.path(temp, paste0(.y$Species, ".csv"))))
list.files(temp, pattern = "csv$")
#> [1] "setosa.csv"     "versicolor.csv" "virginica.csv" 
unlink(temp, recursive = TRUE)

# group_modify() and ungrouped data frames
mtcars |>
  group_modify(~ head(.x, 2L))
#>               mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4      21   6  160 110  3.9 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag  21   6  160 110  3.9 2.875 17.02  0  1    4    4
```
