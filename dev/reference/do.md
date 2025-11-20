# Do anything

**\[superseded\]**

`do()` is superseded as of dplyr 1.0.0, because its syntax never really
felt like it belonged with the rest of dplyr. It's replaced by a
combination of
[`reframe()`](https://dplyr.tidyverse.org/dev/reference/reframe.md)
(which can produce multiple rows and multiple columns),
[`nest_by()`](https://dplyr.tidyverse.org/dev/reference/nest_by.md)
(which creates a
[rowwise](https://dplyr.tidyverse.org/dev/reference/rowwise.md) tibble
of nested data), and
[`pick()`](https://dplyr.tidyverse.org/dev/reference/pick.md) (which
allows you to access the data for the "current" group).

## Usage

``` r
do(.data, ...)
```

## Arguments

- .data:

  a tbl

- ...:

  Expressions to apply to each group. If named, results will be stored
  in a new column. If unnamed, must return a data frame. You can use `.`
  to refer to the current group. You can not mix named and unnamed
  arguments.

## Examples

``` r
# do() with unnamed arguments becomes reframe() or summarise()
# . becomes pick()
by_cyl <- mtcars |> group_by(cyl)
by_cyl |> do(head(., 2))
#> # A tibble: 6 × 11
#> # Groups:   cyl [3]
#>     mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  22.8     4  108     93  3.85  2.32  18.6     1     1     4     1
#> 2  24.4     4  147.    62  3.69  3.19  20       1     0     4     2
#> 3  21       6  160    110  3.9   2.62  16.5     0     1     4     4
#> 4  21       6  160    110  3.9   2.88  17.0     0     1     4     4
#> 5  18.7     8  360    175  3.15  3.44  17.0     0     0     3     2
#> 6  14.3     8  360    245  3.21  3.57  15.8     0     0     3     4
# ->
by_cyl |> reframe(head(pick(everything()), 2))
#> # A tibble: 6 × 11
#>     cyl   mpg  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1     4  22.8  108     93  3.85  2.32  18.6     1     1     4     1
#> 2     4  24.4  147.    62  3.69  3.19  20       1     0     4     2
#> 3     6  21    160    110  3.9   2.62  16.5     0     1     4     4
#> 4     6  21    160    110  3.9   2.88  17.0     0     1     4     4
#> 5     8  18.7  360    175  3.15  3.44  17.0     0     0     3     2
#> 6     8  14.3  360    245  3.21  3.57  15.8     0     0     3     4
by_cyl |> slice_head(n = 2)
#> # A tibble: 6 × 11
#> # Groups:   cyl [3]
#>     mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  22.8     4  108     93  3.85  2.32  18.6     1     1     4     1
#> 2  24.4     4  147.    62  3.69  3.19  20       1     0     4     2
#> 3  21       6  160    110  3.9   2.62  16.5     0     1     4     4
#> 4  21       6  160    110  3.9   2.88  17.0     0     1     4     4
#> 5  18.7     8  360    175  3.15  3.44  17.0     0     0     3     2
#> 6  14.3     8  360    245  3.21  3.57  15.8     0     0     3     4

# Can refer to variables directly
by_cyl |> do(mean = mean(.$vs))
#> # A tibble: 3 × 2
#> # Rowwise: 
#>     cyl mean     
#>   <dbl> <list>   
#> 1     4 <dbl [1]>
#> 2     6 <dbl [1]>
#> 3     8 <dbl [1]>
# ->
by_cyl |> summarise(mean = mean(vs))
#> # A tibble: 3 × 2
#>     cyl  mean
#>   <dbl> <dbl>
#> 1     4 0.909
#> 2     6 0.571
#> 3     8 0    

# do() with named arguments becomes nest_by() + mutate() & list()
models <- by_cyl |> do(mod = lm(mpg ~ disp, data = .))
# ->
models <- mtcars |>
  nest_by(cyl) |>
  mutate(mod = list(lm(mpg ~ disp, data = data)))
models |> summarise(rsq = summary(mod)$r.squared)
#> `summarise()` has converted the output from a rowwise data frame to a
#> grouped data frame.
#> ℹ Summaries were computed rowwise.
#> ℹ Output is grouped by cyl.
#> ℹ Use `summarise(.groups = "keep")` to silence this message.
#> # A tibble: 3 × 2
#> # Groups:   cyl [3]
#>     cyl    rsq
#>   <dbl>  <dbl>
#> 1     4 0.648 
#> 2     6 0.0106
#> 3     8 0.270 

# use broom to turn models into data
models |> do(data.frame(
  var = names(coef(.$mod)),
  coef(summary(.$mod)))
)
#> # A tibble: 6 × 5
#> # Rowwise: 
#>   var         Estimate Std..Error t.value   Pr...t..
#>   <chr>          <dbl>      <dbl>   <dbl>      <dbl>
#> 1 (Intercept) 40.9        3.59     11.4   0.00000120
#> 2 disp        -0.135      0.0332   -4.07  0.00278   
#> 3 (Intercept) 19.1        2.91      6.55  0.00124   
#> 4 disp         0.00361    0.0156    0.232 0.826     
#> 5 (Intercept) 22.0        3.35      6.59  0.0000259 
#> 6 disp        -0.0196     0.00932  -2.11  0.0568    
# ->
models |> reframe(broom::tidy(mod))
#> # A tibble: 6 × 6
#>     cyl term        estimate std.error statistic    p.value
#>   <dbl> <chr>          <dbl>     <dbl>     <dbl>      <dbl>
#> 1     4 (Intercept) 40.9       3.59       11.4   0.00000120
#> 2     4 disp        -0.135     0.0332     -4.07  0.00278   
#> 3     6 (Intercept) 19.1       2.91        6.55  0.00124   
#> 4     6 disp         0.00361   0.0156      0.232 0.826     
#> 5     8 (Intercept) 22.0       3.35        6.59  0.0000259 
#> 6     8 disp        -0.0196    0.00932    -2.11  0.0568    
```
