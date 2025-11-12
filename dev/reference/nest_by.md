# Nest by one or more variables

**\[experimental\]**

`nest_by()` is closely related to
[`group_by()`](https://dplyr.tidyverse.org/dev/reference/group_by.md).
However, instead of storing the group structure in the metadata, it is
made explicit in the data, giving each group key a single row along with
a list-column of data frames that contain all the other data.

`nest_by()` returns a
[rowwise](https://dplyr.tidyverse.org/dev/reference/rowwise.md) data
frame, which makes operations on the grouped data particularly elegant.
See
[`vignette("rowwise")`](https://dplyr.tidyverse.org/dev/articles/rowwise.md)
for more details.

## Usage

``` r
nest_by(.data, ..., .key = "data", .keep = FALSE)
```

## Arguments

- .data:

  A data frame, data frame extension (e.g. a tibble), or a lazy data
  frame (e.g. from dbplyr or dtplyr). See *Methods*, below, for more
  details.

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  In
  [`group_by()`](https://dplyr.tidyverse.org/dev/reference/group_by.md),
  variables or computations to group by. Computations are always done on
  the ungrouped data frame. To perform computations on the grouped data,
  you need to use a separate
  [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md) step
  before the
  [`group_by()`](https://dplyr.tidyverse.org/dev/reference/group_by.md).
  Computations are not allowed in `nest_by()`. In
  [`ungroup()`](https://dplyr.tidyverse.org/dev/reference/group_by.md),
  variables to remove from the grouping.

- .key:

  Name of the list column

- .keep:

  Should the grouping columns be kept in the list column.

## Value

A [rowwise](https://dplyr.tidyverse.org/dev/reference/rowwise.md) data
frame. The output has the following properties:

- The rows come from the underlying
  [`group_keys()`](https://dplyr.tidyverse.org/dev/reference/group_data.md).

- The columns are the grouping keys plus one list-column of data frames.

- Data frame attributes are **not** preserved, because `nest_by()`
  fundamentally creates a new data frame.

A tbl with one row per unique combination of the grouping variables. The
first columns are the grouping variables, followed by a list column of
tibbles with matching rows of the remaining columns.

## Details

Note that `df |> nest_by(x, y)` is roughly equivalent to

    df |>
      group_by(x, y) |>
      summarise(data = list(pick(everything()))) |>
      rowwise()

If you want to unnest a nested data frame, you can either use
[`tidyr::unnest()`](https://tidyr.tidyverse.org/reference/unnest.html)
or take advantage of
[`reframe()`](https://dplyr.tidyverse.org/dev/reference/reframe.md)s
multi-row behaviour:

    nested |>
      reframe(data)

## Lifecycle

`nest_by()` is not stable because
[`tidyr::nest(.by =)`](https://tidyr.tidyverse.org/reference/nest.html)
provides very similar behavior. It may be deprecated in the future.

## Methods

This function is a **generic**, which means that packages can provide
implementations (methods) for other classes. See the documentation of
individual methods for extra arguments and differences in behaviour.

The following methods are currently available in loaded packages: dplyr
(`data.frame`, `grouped_df`) .

## Examples

``` r
# After nesting, you get one row per group
iris |> nest_by(Species)
#> # A tibble: 3 × 2
#> # Rowwise:  Species
#>   Species                  data
#>   <fct>      <list<tibble[,4]>>
#> 1 setosa               [50 × 4]
#> 2 versicolor           [50 × 4]
#> 3 virginica            [50 × 4]
starwars |> nest_by(species)
#> # A tibble: 38 × 2
#> # Rowwise:  species
#>    species                  data
#>    <chr>     <list<tibble[,13]>>
#>  1 Aleena               [1 × 13]
#>  2 Besalisk             [1 × 13]
#>  3 Cerean               [1 × 13]
#>  4 Chagrian             [1 × 13]
#>  5 Clawdite             [1 × 13]
#>  6 Droid                [6 × 13]
#>  7 Dug                  [1 × 13]
#>  8 Ewok                 [1 × 13]
#>  9 Geonosian            [1 × 13]
#> 10 Gungan               [3 × 13]
#> # ℹ 28 more rows

# The output is grouped by row, which makes modelling particularly easy
models <- mtcars |>
  nest_by(cyl) |>
  mutate(model = list(lm(mpg ~ wt, data = data)))
models
#> # A tibble: 3 × 3
#> # Rowwise:  cyl
#>     cyl                data model 
#>   <dbl> <list<tibble[,10]>> <list>
#> 1     4           [11 × 10] <lm>  
#> 2     6            [7 × 10] <lm>  
#> 3     8           [14 × 10] <lm>  

models |> summarise(rsq = summary(model)$r.squared)
#> `summarise()` has grouped output by 'cyl'. You can override using the
#> `.groups` argument.
#> # A tibble: 3 × 2
#> # Groups:   cyl [3]
#>     cyl   rsq
#>   <dbl> <dbl>
#> 1     4 0.509
#> 2     6 0.465
#> 3     8 0.423

# This is particularly elegant with the broom functions
models |> summarise(broom::glance(model))
#> `summarise()` has grouped output by 'cyl'. You can override using the
#> `.groups` argument.
#> # A tibble: 3 × 13
#> # Groups:   cyl [3]
#>     cyl r.squared adj.r.squared sigma statistic p.value    df logLik
#>   <dbl>     <dbl>         <dbl> <dbl>     <dbl>   <dbl> <dbl>  <dbl>
#> 1     4     0.509         0.454  3.33      9.32  0.0137     1 -27.7 
#> 2     6     0.465         0.357  1.17      4.34  0.0918     1  -9.83
#> 3     8     0.423         0.375  2.02      8.80  0.0118     1 -28.7 
#> # ℹ 5 more variables: AIC <dbl>, BIC <dbl>, deviance <dbl>,
#> #   df.residual <int>, nobs <int>
models |> reframe(broom::tidy(model))
#> # A tibble: 6 × 6
#>     cyl term        estimate std.error statistic    p.value
#>   <dbl> <chr>          <dbl>     <dbl>     <dbl>      <dbl>
#> 1     4 (Intercept)    39.6      4.35       9.10 0.00000777
#> 2     4 wt             -5.65     1.85      -3.05 0.0137    
#> 3     6 (Intercept)    28.4      4.18       6.79 0.00105   
#> 4     6 wt             -2.78     1.33      -2.08 0.0918    
#> 5     8 (Intercept)    23.9      3.01       7.94 0.00000405
#> 6     8 wt             -2.19     0.739     -2.97 0.0118    

# Note that you can also `reframe()` to unnest the data
models |> reframe(data)
#> # A tibble: 32 × 11
#>      cyl   mpg  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1     4  22.8 108      93  3.85  2.32  18.6     1     1     4     1
#>  2     4  24.4 147.     62  3.69  3.19  20       1     0     4     2
#>  3     4  22.8 141.     95  3.92  3.15  22.9     1     0     4     2
#>  4     4  32.4  78.7    66  4.08  2.2   19.5     1     1     4     1
#>  5     4  30.4  75.7    52  4.93  1.62  18.5     1     1     4     2
#>  6     4  33.9  71.1    65  4.22  1.84  19.9     1     1     4     1
#>  7     4  21.5 120.     97  3.7   2.46  20.0     1     0     3     1
#>  8     4  27.3  79      66  4.08  1.94  18.9     1     1     4     1
#>  9     4  26   120.     91  4.43  2.14  16.7     0     1     5     2
#> 10     4  30.4  95.1   113  3.77  1.51  16.9     1     1     5     2
#> # ℹ 22 more rows
```
