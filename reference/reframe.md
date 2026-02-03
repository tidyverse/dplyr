# Transform each group to an arbitrary number of rows

While
[`summarise()`](https://dplyr.tidyverse.org/reference/summarise.md)
requires that each argument returns a single value, and
[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.md) requires
that each argument returns the same number of rows as the input,
`reframe()` is a more general workhorse with no requirements on the
number of rows returned per group.

`reframe()` creates a new data frame by applying functions to columns of
an existing data frame. It is most similar to
[`summarise()`](https://dplyr.tidyverse.org/reference/summarise.md),
with two big differences:

- `reframe()` can return an arbitrary number of rows per group, while
  [`summarise()`](https://dplyr.tidyverse.org/reference/summarise.md)
  reduces each group down to a single row.

- `reframe()` always returns an ungrouped data frame, while
  [`summarise()`](https://dplyr.tidyverse.org/reference/summarise.md)
  might return a grouped or rowwise data frame, depending on the
  scenario.

We expect that you'll use
[`summarise()`](https://dplyr.tidyverse.org/reference/summarise.md) much
more often than `reframe()`, but `reframe()` can be particularly helpful
when you need to apply a complex function that doesn't return a single
summary value.

## Usage

``` r
reframe(.data, ..., .by = NULL)
```

## Arguments

- .data:

  A data frame, data frame extension (e.g. a tibble), or a lazy data
  frame (e.g. from dbplyr or dtplyr). See *Methods*, below, for more
  details.

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>

  Name-value pairs of functions. The name will be the name of the
  variable in the result. The value can be a vector of any length.

  Unnamed data frame values add multiple columns from a single
  expression.

- .by:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.md)\>
  Optionally, a selection of columns to group by for just this
  operation, functioning as an alternative to
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.md). For
  details and examples, see
  [?dplyr_by](https://dplyr.tidyverse.org/reference/dplyr_by.md).

## Value

If `.data` is a tibble, a tibble. Otherwise, a data.frame.

- The rows originate from the underlying grouping keys.

- The columns are a combination of the grouping keys and the expressions
  that you provide.

- The output is always ungrouped.

- Data frame attributes are **not** preserved, because `reframe()`
  fundamentally creates a new data frame.

## Connection to tibble

`reframe()` is theoretically connected to two functions in tibble,
[`tibble::enframe()`](https://tibble.tidyverse.org/reference/enframe.html)
and
[`tibble::deframe()`](https://tibble.tidyverse.org/reference/enframe.html):

- `enframe()`: vector -\> data frame

- `deframe()`: data frame -\> vector

- `reframe()`: data frame -\> data frame

## Methods

This function is a **generic**, which means that packages can provide
implementations (methods) for other classes. See the documentation of
individual methods for extra arguments and differences in behaviour.

The following methods are currently available in loaded packages: dbplyr
(`tbl_lazy`), dplyr (`data.frame`) .

## See also

Other single table verbs:
[`arrange()`](https://dplyr.tidyverse.org/reference/arrange.md),
[`filter()`](https://dplyr.tidyverse.org/reference/filter.md),
[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.md),
[`rename()`](https://dplyr.tidyverse.org/reference/rename.md),
[`select()`](https://dplyr.tidyverse.org/reference/select.md),
[`slice()`](https://dplyr.tidyverse.org/reference/slice.md),
[`summarise()`](https://dplyr.tidyverse.org/reference/summarise.md)

## Examples

``` r
table <- c("a", "b", "d", "f")

df <- tibble(
  g = c(1, 1, 1, 2, 2, 2, 2),
  x = c("e", "a", "b", "c", "f", "d", "a")
)

# `reframe()` allows you to apply functions that return
# an arbitrary number of rows
df |>
  reframe(x = intersect(x, table))
#> # A tibble: 4 × 1
#>   x    
#>   <chr>
#> 1 a    
#> 2 b    
#> 3 f    
#> 4 d    

# Functions are applied per group, and each group can return a
# different number of rows.
df |>
  reframe(x = intersect(x, table), .by = g)
#> # A tibble: 5 × 2
#>       g x    
#>   <dbl> <chr>
#> 1     1 a    
#> 2     1 b    
#> 3     2 f    
#> 4     2 d    
#> 5     2 a    

# The output is always ungrouped, even when using `group_by()`
df |>
  group_by(g) |>
  reframe(x = intersect(x, table))
#> # A tibble: 5 × 2
#>       g x    
#>   <dbl> <chr>
#> 1     1 a    
#> 2     1 b    
#> 3     2 f    
#> 4     2 d    
#> 5     2 a    

# You can add multiple columns at once using a single expression by returning
# a data frame.
quantile_df <- function(x, probs = c(0.25, 0.5, 0.75)) {
  tibble(
    val = quantile(x, probs, na.rm = TRUE),
    quant = probs
  )
}

x <- c(10, 15, 18, 12)
quantile_df(x)
#> # A tibble: 3 × 2
#>     val quant
#>   <dbl> <dbl>
#> 1  11.5  0.25
#> 2  13.5  0.5 
#> 3  15.8  0.75

starwars |>
  reframe(quantile_df(height))
#> # A tibble: 3 × 2
#>     val quant
#>   <dbl> <dbl>
#> 1   167  0.25
#> 2   180  0.5 
#> 3   191  0.75

starwars |>
  reframe(quantile_df(height), .by = homeworld)
#> # A tibble: 147 × 3
#>    homeworld   val quant
#>    <chr>     <dbl> <dbl>
#>  1 Tatooine   166.  0.25
#>  2 Tatooine   175   0.5 
#>  3 Tatooine   183   0.75
#>  4 Naboo      168.  0.25
#>  5 Naboo      183   0.5 
#>  6 Naboo      190.  0.75
#>  7 Alderaan   169   0.25
#>  8 Alderaan   188   0.5 
#>  9 Alderaan   190.  0.75
#> 10 Stewjon    182   0.25
#> # ℹ 137 more rows

starwars |>
  reframe(
    across(c(height, mass), quantile_df, .unpack = TRUE),
    .by = homeworld
  )
#> # A tibble: 147 × 5
#>    homeworld height_val height_quant mass_val mass_quant
#>    <chr>          <dbl>        <dbl>    <dbl>      <dbl>
#>  1 Tatooine        166.         0.25     75         0.25
#>  2 Tatooine        175          0.5      80.5       0.5 
#>  3 Tatooine        183          0.75     93         0.75
#>  4 Naboo           168.         0.25     50.2       0.25
#>  5 Naboo           183          0.5      70.5       0.5 
#>  6 Naboo           190.         0.75     80.2       0.75
#>  7 Alderaan        169          0.25     56.5       0.25
#>  8 Alderaan        188          0.5      64         0.5 
#>  9 Alderaan        190.         0.75     71.5       0.75
#> 10 Stewjon         182          0.25     77         0.25
#> # ℹ 137 more rows
```
