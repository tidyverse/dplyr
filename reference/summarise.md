# Summarise each group down to one row

`summarise()` creates a new data frame. It returns one row for each
combination of grouping variables; if there are no grouping variables,
the output will have a single row summarising all observations in the
input. It will contain one column for each grouping variable and one
column for each of the summary statistics that you have specified.

`summarise()` and `summarize()` are synonyms.

## Usage

``` r
summarise(.data, ..., .by = NULL, .groups = NULL)

summarize(.data, ..., .by = NULL, .groups = NULL)
```

## Arguments

- .data:

  A data frame, data frame extension (e.g. a tibble), or a lazy data
  frame (e.g. from dbplyr or dtplyr). See *Methods*, below, for more
  details.

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Name-value pairs of summary functions. The name will be the name of
  the variable in the result.

  The value can be:

  - A vector of length 1, e.g. `min(x)`,
    [`n()`](https://dplyr.tidyverse.org/reference/context.md), or
    `sum(is.na(y))`.

  - A data frame with 1 row, to add multiple columns from a single
    expression.

- .by:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.md)\>
  Optionally, a selection of columns to group by for just this
  operation, functioning as an alternative to
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.md). For
  details and examples, see
  [?dplyr_by](https://dplyr.tidyverse.org/reference/dplyr_by.md).

- .groups:

  **\[experimental\]** Grouping structure of the result.

  - `"drop_last"`: drops the last level of grouping. This was the only
    supported option before version 1.0.0.

  - `"drop"`: All levels of grouping are dropped.

  - `"keep"`: Same grouping structure as `.data`.

  - `"rowwise"`: Each row is its own group.

  When `.groups` is not specified, it is set to `"drop_last"` for a
  grouped data frame, and `"keep"` for a rowwise data frame. In
  addition, a message informs you of how the result will be grouped
  unless the result is ungrouped, the option `"dplyr.summarise.inform"`
  is set to `FALSE`, or when `summarise()` is called from a function in
  a package.

## Value

An object *usually* of the same type as `.data`.

- The rows come from the underlying
  [`group_keys()`](https://dplyr.tidyverse.org/reference/group_data.md).

- The columns are a combination of the grouping keys and the summary
  expressions that you provide.

- The grouping structure is controlled by the `.groups=` argument, the
  output may be another
  [grouped_df](https://dplyr.tidyverse.org/reference/grouped_df.md), a
  [tibble](https://tibble.tidyverse.org/reference/tibble.html) or a
  [rowwise](https://dplyr.tidyverse.org/reference/rowwise.md) data
  frame.

- Data frame attributes are **not** preserved, because `summarise()`
  fundamentally creates a new data frame.

## Useful functions

- Center: [`mean()`](https://rdrr.io/r/base/mean.html),
  [`median()`](https://rdrr.io/r/stats/median.html)

- Spread: [`sd()`](https://rdrr.io/r/stats/sd.html),
  [`IQR()`](https://rdrr.io/r/stats/IQR.html),
  [`mad()`](https://rdrr.io/r/stats/mad.html)

- Range: [`min()`](https://rdrr.io/r/base/Extremes.html),
  [`max()`](https://rdrr.io/r/base/Extremes.html),

- Position: [`first()`](https://dplyr.tidyverse.org/reference/nth.md),
  [`last()`](https://dplyr.tidyverse.org/reference/nth.md),
  [`nth()`](https://dplyr.tidyverse.org/reference/nth.md),

- Count: [`n()`](https://dplyr.tidyverse.org/reference/context.md),
  [`n_distinct()`](https://dplyr.tidyverse.org/reference/n_distinct.md)

- Logical: [`any()`](https://rdrr.io/r/base/any.html),
  [`all()`](https://rdrr.io/r/base/all.html)

## Backend variations

The data frame backend supports creating a variable and using it in the
same summary. This means that previously created summary variables can
be further transformed or combined within the summary, as in
[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.md). However,
it also means that summary variables with the same names as previous
variables overwrite them, making those variables unavailable to later
summary variables.

This behaviour may not be supported in other backends. To avoid
unexpected results, consider using new names for your summary variables,
especially when creating multiple summaries.

## Methods

This function is a **generic**, which means that packages can provide
implementations (methods) for other classes. See the documentation of
individual methods for extra arguments and differences in behaviour.

The following methods are currently available in loaded packages: dbplyr
([`tbl_lazy`](https://dbplyr.tidyverse.org/reference/summarise.tbl_lazy.html)),
dplyr (`data.frame`, `grouped_df`, `rowwise_df`) .

## See also

Other single table verbs:
[`arrange()`](https://dplyr.tidyverse.org/reference/arrange.md),
[`filter()`](https://dplyr.tidyverse.org/reference/filter.md),
[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.md),
[`reframe()`](https://dplyr.tidyverse.org/reference/reframe.md),
[`rename()`](https://dplyr.tidyverse.org/reference/rename.md),
[`select()`](https://dplyr.tidyverse.org/reference/select.md),
[`slice()`](https://dplyr.tidyverse.org/reference/slice.md)

## Examples

``` r
# A summary applied to ungrouped tbl returns a single row
mtcars |>
  summarise(mean = mean(disp), n = n())
#>       mean  n
#> 1 230.7219 32

# Usually, you'll want to group first
mtcars |>
  group_by(cyl) |>
  summarise(mean = mean(disp), n = n())
#> # A tibble: 3 × 3
#>     cyl  mean     n
#>   <dbl> <dbl> <int>
#> 1     4  105.    11
#> 2     6  183.     7
#> 3     8  353.    14

# Each summary call removes one grouping level (since that group
# is now just a single row)
mtcars |>
  group_by(cyl, vs) |>
  summarise(cyl_n = n()) |>
  group_vars()
#> `summarise()` has regrouped the output.
#> ℹ Summaries were computed grouped by cyl and vs.
#> ℹ Output is grouped by cyl.
#> ℹ Use `summarise(.groups = "drop_last")` to silence this message.
#> ℹ Use `summarise(.by = c(cyl, vs))` for per-operation grouping
#>   (`?dplyr::dplyr_by`) instead.
#> [1] "cyl"

# BEWARE: reusing variables may lead to unexpected results
mtcars |>
  group_by(cyl) |>
  summarise(disp = mean(disp), sd = sd(disp))
#> # A tibble: 3 × 3
#>     cyl  disp    sd
#>   <dbl> <dbl> <dbl>
#> 1     4  105.    NA
#> 2     6  183.    NA
#> 3     8  353.    NA

# Refer to column names stored as strings with the `.data` pronoun:
var <- "mass"
summarise(starwars, avg = mean(.data[[var]], na.rm = TRUE))
#> # A tibble: 1 × 1
#>     avg
#>   <dbl>
#> 1  97.3
# Learn more in ?rlang::args_data_masking
```
