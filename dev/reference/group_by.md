# Group by one or more variables

Most data operations are done on groups defined by variables.
`group_by()` takes an existing tbl and converts it into a grouped tbl
where operations are performed "by group". `ungroup()` removes grouping.

## Usage

``` r
group_by(.data, ..., .add = FALSE, .drop = group_by_drop_default(.data))

ungroup(x, ...)
```

## Arguments

- .data:

  A data frame, data frame extension (e.g. a tibble), or a lazy data
  frame (e.g. from dbplyr or dtplyr). See *Methods*, below, for more
  details.

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  In `group_by()`, variables or computations to group by. Computations
  are always done on the ungrouped data frame. To perform computations
  on the grouped data, you need to use a separate
  [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md) step
  before the `group_by()`. Computations are not allowed in
  [`nest_by()`](https://dplyr.tidyverse.org/dev/reference/nest_by.md).
  In `ungroup()`, variables to remove from the grouping.

- .add:

  When `FALSE`, the default, `group_by()` will override existing groups.
  To add to the existing groups, use `.add = TRUE`.

- .drop:

  Drop groups formed by factor levels that don't appear in the data? The
  default is `TRUE` except when `.data` has been previously grouped with
  `.drop = FALSE`. See
  [`group_by_drop_default()`](https://dplyr.tidyverse.org/dev/reference/group_by_drop_default.md)
  for details.

- x:

  A [`tbl()`](https://dplyr.tidyverse.org/dev/reference/tbl.md)

## Value

A grouped data frame with class
[`grouped_df`](https://dplyr.tidyverse.org/dev/reference/grouped_df.md),
unless the combination of `...` and `add` yields a empty set of grouping
columns, in which case a tibble will be returned.

## Methods

These function are **generic**s, which means that packages can provide
implementations (methods) for other classes. See the documentation of
individual methods for extra arguments and differences in behaviour.

Methods available in currently loaded packages:

- `group_by()`: dbplyr
  ([`tbl_lazy`](https://dbplyr.tidyverse.org/reference/group_by.tbl_lazy.html)),
  dplyr (`data.frame`) .

- `ungroup()`: dbplyr (`tbl_lazy`), dplyr (`data.frame`, `grouped_df`,
  `rowwise_df`) .

## Ordering

Currently, `group_by()` internally orders the groups in ascending order.
This results in ordered output from functions that aggregate groups,
such as
[`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md).

When used as grouping columns, character vectors are ordered in the C
locale for performance and reproducibility across R sessions. If the
resulting ordering of your grouped operation matters and is dependent on
the locale, you should follow up the grouped operation with an explicit
call to
[`arrange()`](https://dplyr.tidyverse.org/dev/reference/arrange.md) and
set the `.locale` argument. For example:

    data |>
      group_by(chr) |>
      summarise(avg = mean(x)) |>
      arrange(chr, .locale = "en")

This is often useful as a preliminary step before generating content
intended for humans, such as an HTML table.

### Legacy behavior

**\[deprecated\]**

Prior to dplyr 1.1.0, character vector grouping columns were ordered in
the system locale. Setting the global option `dplyr.legacy_locale` to
`TRUE` retains this legacy behavior, but this has been deprecated.
Update existing code to explicitly call `arrange(.locale = )` instead.
Run `Sys.getlocale("LC_COLLATE")` to determine your system locale, and
compare that against the list in
[`stringi::stri_locale_list()`](https://rdrr.io/pkg/stringi/man/stri_locale_list.html)
to find an appropriate value for `.locale`, i.e. for American English,
`"en_US"`.

## See also

Other grouping functions:
[`group_map()`](https://dplyr.tidyverse.org/dev/reference/group_map.md),
[`group_nest()`](https://dplyr.tidyverse.org/dev/reference/group_nest.md),
[`group_split()`](https://dplyr.tidyverse.org/dev/reference/group_split.md),
[`group_trim()`](https://dplyr.tidyverse.org/dev/reference/group_trim.md)

## Examples

``` r
by_cyl <- mtcars |> group_by(cyl)

# grouping doesn't change how the data looks (apart from listing
# how it's grouped):
by_cyl
#> # A tibble: 32 × 11
#> # Groups:   cyl [3]
#>      mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1  21       6  160    110  3.9   2.62  16.5     0     1     4     4
#>  2  21       6  160    110  3.9   2.88  17.0     0     1     4     4
#>  3  22.8     4  108     93  3.85  2.32  18.6     1     1     4     1
#>  4  21.4     6  258    110  3.08  3.22  19.4     1     0     3     1
#>  5  18.7     8  360    175  3.15  3.44  17.0     0     0     3     2
#>  6  18.1     6  225    105  2.76  3.46  20.2     1     0     3     1
#>  7  14.3     8  360    245  3.21  3.57  15.8     0     0     3     4
#>  8  24.4     4  147.    62  3.69  3.19  20       1     0     4     2
#>  9  22.8     4  141.    95  3.92  3.15  22.9     1     0     4     2
#> 10  19.2     6  168.   123  3.92  3.44  18.3     1     0     4     4
#> # ℹ 22 more rows

# It changes how it acts with the other dplyr verbs:
by_cyl |> summarise(
  disp = mean(disp),
  hp = mean(hp)
)
#> # A tibble: 3 × 3
#>     cyl  disp    hp
#>   <dbl> <dbl> <dbl>
#> 1     4  105.  82.6
#> 2     6  183. 122. 
#> 3     8  353. 209. 
by_cyl |> filter(disp == max(disp))
#> # A tibble: 3 × 11
#> # Groups:   cyl [3]
#>     mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  21.4     6  258    110  3.08  3.22  19.4     1     0     3     1
#> 2  24.4     4  147.    62  3.69  3.19  20       1     0     4     2
#> 3  10.4     8  472    205  2.93  5.25  18.0     0     0     3     4

# Each call to summarise() removes a layer of grouping
by_vs_am <- mtcars |> group_by(vs, am)
by_vs <- by_vs_am |> summarise(n = n())
#> `summarise()` has grouped output by 'vs'. You can override using the
#> `.groups` argument.
by_vs
#> # A tibble: 4 × 3
#> # Groups:   vs [2]
#>      vs    am     n
#>   <dbl> <dbl> <int>
#> 1     0     0    12
#> 2     0     1     6
#> 3     1     0     7
#> 4     1     1     7
by_vs |> summarise(n = sum(n))
#> # A tibble: 2 × 2
#>      vs     n
#>   <dbl> <int>
#> 1     0    18
#> 2     1    14

# To removing grouping, use ungroup
by_vs |>
  ungroup() |>
  summarise(n = sum(n))
#> # A tibble: 1 × 1
#>       n
#>   <int>
#> 1    32

# By default, group_by() overrides existing grouping
by_cyl |>
  group_by(vs, am) |>
  group_vars()
#> [1] "vs" "am"

# Use add = TRUE to instead append
by_cyl |>
  group_by(vs, am, .add = TRUE) |>
  group_vars()
#> [1] "cyl" "vs"  "am" 

# You can group by expressions: this is a short-hand
# for a mutate() followed by a group_by()
mtcars |>
  group_by(vsam = vs + am)
#> # A tibble: 32 × 12
#> # Groups:   vsam [3]
#>      mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1  21       6  160    110  3.9   2.62  16.5     0     1     4     4
#>  2  21       6  160    110  3.9   2.88  17.0     0     1     4     4
#>  3  22.8     4  108     93  3.85  2.32  18.6     1     1     4     1
#>  4  21.4     6  258    110  3.08  3.22  19.4     1     0     3     1
#>  5  18.7     8  360    175  3.15  3.44  17.0     0     0     3     2
#>  6  18.1     6  225    105  2.76  3.46  20.2     1     0     3     1
#>  7  14.3     8  360    245  3.21  3.57  15.8     0     0     3     4
#>  8  24.4     4  147.    62  3.69  3.19  20       1     0     4     2
#>  9  22.8     4  141.    95  3.92  3.15  22.9     1     0     4     2
#> 10  19.2     6  168.   123  3.92  3.44  18.3     1     0     4     4
#> # ℹ 22 more rows
#> # ℹ 1 more variable: vsam <dbl>

# The implicit mutate() step is always performed on the
# ungrouped data. Here we get 3 groups:
mtcars |>
  group_by(vs) |>
  group_by(hp_cut = cut(hp, 3))
#> # A tibble: 32 × 12
#> # Groups:   hp_cut [3]
#>      mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1  21       6  160    110  3.9   2.62  16.5     0     1     4     4
#>  2  21       6  160    110  3.9   2.88  17.0     0     1     4     4
#>  3  22.8     4  108     93  3.85  2.32  18.6     1     1     4     1
#>  4  21.4     6  258    110  3.08  3.22  19.4     1     0     3     1
#>  5  18.7     8  360    175  3.15  3.44  17.0     0     0     3     2
#>  6  18.1     6  225    105  2.76  3.46  20.2     1     0     3     1
#>  7  14.3     8  360    245  3.21  3.57  15.8     0     0     3     4
#>  8  24.4     4  147.    62  3.69  3.19  20       1     0     4     2
#>  9  22.8     4  141.    95  3.92  3.15  22.9     1     0     4     2
#> 10  19.2     6  168.   123  3.92  3.44  18.3     1     0     4     4
#> # ℹ 22 more rows
#> # ℹ 1 more variable: hp_cut <fct>

# If you want it to be performed by groups,
# you have to use an explicit mutate() call.
# Here we get 3 groups per value of vs
mtcars |>
  group_by(vs) |>
  mutate(hp_cut = cut(hp, 3)) |>
  group_by(hp_cut)
#> # A tibble: 32 × 12
#> # Groups:   hp_cut [6]
#>      mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1  21       6  160    110  3.9   2.62  16.5     0     1     4     4
#>  2  21       6  160    110  3.9   2.88  17.0     0     1     4     4
#>  3  22.8     4  108     93  3.85  2.32  18.6     1     1     4     1
#>  4  21.4     6  258    110  3.08  3.22  19.4     1     0     3     1
#>  5  18.7     8  360    175  3.15  3.44  17.0     0     0     3     2
#>  6  18.1     6  225    105  2.76  3.46  20.2     1     0     3     1
#>  7  14.3     8  360    245  3.21  3.57  15.8     0     0     3     4
#>  8  24.4     4  147.    62  3.69  3.19  20       1     0     4     2
#>  9  22.8     4  141.    95  3.92  3.15  22.9     1     0     4     2
#> 10  19.2     6  168.   123  3.92  3.44  18.3     1     0     4     4
#> # ℹ 22 more rows
#> # ℹ 1 more variable: hp_cut <fct>

# when factors are involved and .drop = FALSE, groups can be empty
tbl <- tibble(
  x = 1:10,
  y = factor(rep(c("a", "c"), each  = 5), levels = c("a", "b", "c"))
)
tbl |>
  group_by(y, .drop = FALSE) |>
  group_rows()
#> <list_of<integer>[3]>
#> [[1]]
#> [1] 1 2 3 4 5
#> 
#> [[2]]
#> integer(0)
#> 
#> [[3]]
#> [1]  6  7  8  9 10
#> 
```
