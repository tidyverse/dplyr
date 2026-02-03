# Join specifications

`join_by()` constructs a specification that describes how to join two
tables using a small domain specific language. The result can be
supplied as the `by` argument to any of the join functions (such as
[`left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.md)).

## Usage

``` r
join_by(...)
```

## Arguments

- ...:

  Expressions specifying the join.

  Each expression should consist of one of the following:

  - Equality condition: `==`

  - Inequality conditions: `>=`, `>`, `<=`, or `<`

  - Rolling helper: `closest()`

  - Overlap helpers:
    [`between()`](https://dplyr.tidyverse.org/reference/between.md),
    `within()`, or `overlaps()`

  Other expressions are not supported. If you need to perform a join on
  a computed variable, e.g. `join_by(sales_date - 40 >= promo_date)`,
  you'll need to precompute and store it in a separate column.

  Column names should be specified as quoted or unquoted names. By
  default, the name on the left-hand side of a join condition refers to
  the left-hand table, unless overridden by explicitly prefixing the
  column name with either `x$` or `y$`.

  If a single column name is provided without any join conditions, it is
  interpreted as if that column name was duplicated on each side of
  `==`, i.e. `x` is interpreted as `x == x`.

## Join types

The following types of joins are supported by dplyr:

- Equality joins

- Inequality joins

- Rolling joins

- Overlap joins

- Cross joins

Equality, inequality, rolling, and overlap joins are discussed in more
detail below. Cross joins are implemented through
[`cross_join()`](https://dplyr.tidyverse.org/reference/cross_join.md).

### Equality joins

Equality joins require keys to be equal between one or more pairs of
columns, and are the most common type of join. To construct an equality
join using `join_by()`, supply two column names to join with separated
by `==`. Alternatively, supplying a single name will be interpreted as
an equality join between two columns of the same name. For example,
`join_by(x)` is equivalent to `join_by(x == x)`.

### Inequality joins

Inequality joins match on an inequality, such as `>`, `>=`, `<`, or
`<=`, and are common in time series analysis and genomics. To construct
an inequality join using `join_by()`, supply two column names separated
by one of the above mentioned inequalities.

Note that inequality joins will match a single row in `x` to a
potentially large number of rows in `y`. Be extra careful when
constructing inequality join specifications!

### Rolling joins

Rolling joins are a variant of inequality joins that limit the results
returned from an inequality join condition. They are useful for
"rolling" the closest match forward/backwards when there isn't an exact
match. To construct a rolling join, wrap an inequality with `closest()`.

- `closest(expr)`

  `expr` must be an inequality involving one of: `>`, `>=`, `<`, or
  `<=`.

  For example, `closest(x >= y)` is interpreted as: For each value in
  `x`, find the closest value in `y` that is less than or equal to that
  `x` value.

`closest()` will always use the left-hand table (`x`) as the primary
table, and the right-hand table (`y`) as the one to find the closest
match in, regardless of how the inequality is specified. For example,
`closest(y$a >= x$b)` will always be interpreted as
`closest(x$b <= y$a)`.

### Overlap joins

Overlap joins are a special case of inequality joins involving one or
two columns from the left-hand table *overlapping* a range defined by
two columns from the right-hand table. There are three helpers that
`join_by()` recognizes to assist with constructing overlap joins, all of
which can be constructed from simpler inequalities.

- `between(x, y_lower, y_upper, ..., bounds = "[]")`

  For each value in `x`, this finds everywhere that value falls between
  `[y_lower, y_upper]`. Equivalent to `x >= y_lower, x <= y_upper` by
  default.

  `bounds` can be one of `"[]"`, `"[)"`, `"(]"`, or `"()"` to alter the
  inclusiveness of the lower and upper bounds. This changes whether `>=`
  or `>` and `<=` or `<` are used to build the inequalities shown above.

  Dots are for future extensions and must be empty.

- `within(x_lower, x_upper, y_lower, y_upper)`

  For each range in `[x_lower, x_upper]`, this finds everywhere that
  range falls completely within `[y_lower, y_upper]`. Equivalent to
  `x_lower >= y_lower, x_upper <= y_upper`.

  The inequalities used to build `within()` are the same regardless of
  the inclusiveness of the supplied ranges.

- `overlaps(x_lower, x_upper, y_lower, y_upper, ..., bounds = "[]")`

  For each range in `[x_lower, x_upper]`, this finds everywhere that
  range overlaps `[y_lower, y_upper]` in any capacity. Equivalent to
  `x_lower <= y_upper, x_upper >= y_lower` by default.

  `bounds` can be one of `"[]"`, `"[)"`, `"(]"`, or `"()"` to alter the
  inclusiveness of the lower and upper bounds. `"[]"` uses `<=` and
  `>=`, but the 3 other options use `<` and `>` and generate the exact
  same inequalities.

  Dots are for future extensions and must be empty.

These conditions assume that the ranges are well-formed and non-empty,
i.e. `x_lower <= x_upper` when bounds are treated as `"[]"`, and
`x_lower < x_upper` otherwise.

## Column referencing

When specifying join conditions, `join_by()` assumes that column names
on the left-hand side of the condition refer to the left-hand table
(`x`), and names on the right-hand side of the condition refer to the
right-hand table (`y`). Occasionally, it is clearer to be able to
specify a right-hand table name on the left-hand side of the condition,
and vice versa. To support this, column names can be prefixed by `x$` or
`y$` to explicitly specify which table they come from.

## Examples

``` r
sales <- tibble(
  id = c(1L, 1L, 1L, 2L, 2L),
  sale_date = as.Date(c("2018-12-31", "2019-01-02", "2019-01-05", "2019-01-04", "2019-01-01"))
)
sales
#> # A tibble: 5 × 2
#>      id sale_date 
#>   <int> <date>    
#> 1     1 2018-12-31
#> 2     1 2019-01-02
#> 3     1 2019-01-05
#> 4     2 2019-01-04
#> 5     2 2019-01-01

promos <- tibble(
  id = c(1L, 1L, 2L),
  promo_date = as.Date(c("2019-01-01", "2019-01-05", "2019-01-02"))
)
promos
#> # A tibble: 3 × 2
#>      id promo_date
#>   <int> <date>    
#> 1     1 2019-01-01
#> 2     1 2019-01-05
#> 3     2 2019-01-02

# Match `id` to `id`, and `sale_date` to `promo_date`
by <- join_by(id, sale_date == promo_date)
left_join(sales, promos, by)
#> # A tibble: 5 × 2
#>      id sale_date 
#>   <int> <date>    
#> 1     1 2018-12-31
#> 2     1 2019-01-02
#> 3     1 2019-01-05
#> 4     2 2019-01-04
#> 5     2 2019-01-01

# For each `sale_date` within a particular `id`,
# find all `promo_date`s that occurred before that particular sale
by <- join_by(id, sale_date >= promo_date)
left_join(sales, promos, by)
#> # A tibble: 6 × 3
#>      id sale_date  promo_date
#>   <int> <date>     <date>    
#> 1     1 2018-12-31 NA        
#> 2     1 2019-01-02 2019-01-01
#> 3     1 2019-01-05 2019-01-01
#> 4     1 2019-01-05 2019-01-05
#> 5     2 2019-01-04 2019-01-02
#> 6     2 2019-01-01 NA        

# For each `sale_date` within a particular `id`,
# find only the closest `promo_date` that occurred before that sale
by <- join_by(id, closest(sale_date >= promo_date))
left_join(sales, promos, by)
#> # A tibble: 5 × 3
#>      id sale_date  promo_date
#>   <int> <date>     <date>    
#> 1     1 2018-12-31 NA        
#> 2     1 2019-01-02 2019-01-01
#> 3     1 2019-01-05 2019-01-05
#> 4     2 2019-01-04 2019-01-02
#> 5     2 2019-01-01 NA        

# If you want to disallow exact matching in rolling joins, use `>` rather
# than `>=`. Note that the promo on `2019-01-05` is no longer considered the
# closest match for the sale on the same date.
by <- join_by(id, closest(sale_date > promo_date))
left_join(sales, promos, by)
#> # A tibble: 5 × 3
#>      id sale_date  promo_date
#>   <int> <date>     <date>    
#> 1     1 2018-12-31 NA        
#> 2     1 2019-01-02 2019-01-01
#> 3     1 2019-01-05 2019-01-01
#> 4     2 2019-01-04 2019-01-02
#> 5     2 2019-01-01 NA        

# Same as before, but also require that the promo had to occur at most 1
# day before the sale was made. We'll use a full join to see that id 2's
# promo on `2019-01-02` is no longer matched to the sale on `2019-01-04`.
sales <- mutate(sales, sale_date_lower = sale_date - 1)
by <- join_by(id, closest(sale_date >= promo_date), sale_date_lower <= promo_date)
full_join(sales, promos, by)
#> # A tibble: 6 × 4
#>      id sale_date  sale_date_lower promo_date
#>   <int> <date>     <date>          <date>    
#> 1     1 2018-12-31 2018-12-30      NA        
#> 2     1 2019-01-02 2019-01-01      2019-01-01
#> 3     1 2019-01-05 2019-01-04      2019-01-05
#> 4     2 2019-01-04 2019-01-03      NA        
#> 5     2 2019-01-01 2018-12-31      NA        
#> 6     2 NA         NA              2019-01-02

# ---------------------------------------------------------------------------

segments <- tibble(
  segment_id = 1:4,
  chromosome = c("chr1", "chr2", "chr2", "chr1"),
  start = c(140, 210, 380, 230),
  end = c(150, 240, 415, 280)
)
segments
#> # A tibble: 4 × 4
#>   segment_id chromosome start   end
#>        <int> <chr>      <dbl> <dbl>
#> 1          1 chr1         140   150
#> 2          2 chr2         210   240
#> 3          3 chr2         380   415
#> 4          4 chr1         230   280

reference <- tibble(
  reference_id = 1:4,
  chromosome = c("chr1", "chr1", "chr2", "chr2"),
  start = c(100, 200, 300, 415),
  end = c(150, 250, 399, 450)
)
reference
#> # A tibble: 4 × 4
#>   reference_id chromosome start   end
#>          <int> <chr>      <dbl> <dbl>
#> 1            1 chr1         100   150
#> 2            2 chr1         200   250
#> 3            3 chr2         300   399
#> 4            4 chr2         415   450

# Find every time a segment `start` falls between the reference
# `[start, end]` range.
by <- join_by(chromosome, between(start, start, end))
full_join(segments, reference, by)
#> # A tibble: 5 × 7
#>   segment_id chromosome start.x end.x reference_id start.y end.y
#>        <int> <chr>        <dbl> <dbl>        <int>   <dbl> <dbl>
#> 1          1 chr1           140   150            1     100   150
#> 2          2 chr2           210   240           NA      NA    NA
#> 3          3 chr2           380   415            3     300   399
#> 4          4 chr1           230   280            2     200   250
#> 5         NA chr2            NA    NA            4     415   450

# If you wanted the reference columns first, supply `reference` as `x`
# and `segments` as `y`, then explicitly refer to their columns using `x$`
# and `y$`.
by <- join_by(chromosome, between(y$start, x$start, x$end))
full_join(reference, segments, by)
#> # A tibble: 5 × 7
#>   reference_id chromosome start.x end.x segment_id start.y end.y
#>          <int> <chr>        <dbl> <dbl>      <int>   <dbl> <dbl>
#> 1            1 chr1           100   150          1     140   150
#> 2            2 chr1           200   250          4     230   280
#> 3            3 chr2           300   399          3     380   415
#> 4            4 chr2           415   450         NA      NA    NA
#> 5           NA chr2            NA    NA          2     210   240

# Find every time a segment falls completely within a reference.
# Sometimes using `x$` and `y$` makes your intentions clearer, even if they
# match the default behavior.
by <- join_by(chromosome, within(x$start, x$end, y$start, y$end))
inner_join(segments, reference, by)
#> # A tibble: 1 × 7
#>   segment_id chromosome start.x end.x reference_id start.y end.y
#>        <int> <chr>        <dbl> <dbl>        <int>   <dbl> <dbl>
#> 1          1 chr1           140   150            1     100   150

# Find every time a segment overlaps a reference in any way.
by <- join_by(chromosome, overlaps(x$start, x$end, y$start, y$end))
full_join(segments, reference, by)
#> # A tibble: 5 × 7
#>   segment_id chromosome start.x end.x reference_id start.y end.y
#>        <int> <chr>        <dbl> <dbl>        <int>   <dbl> <dbl>
#> 1          1 chr1           140   150            1     100   150
#> 2          2 chr2           210   240           NA      NA    NA
#> 3          3 chr2           380   415            3     300   399
#> 4          3 chr2           380   415            4     415   450
#> 5          4 chr1           230   280            2     200   250

# It is common to have right-open ranges with bounds like `[)`, which would
# mean an end value of `415` would no longer overlap a start value of `415`.
# Setting `bounds` allows you to compute overlaps with those kinds of ranges.
by <- join_by(chromosome, overlaps(x$start, x$end, y$start, y$end, bounds = "[)"))
full_join(segments, reference, by)
#> # A tibble: 5 × 7
#>   segment_id chromosome start.x end.x reference_id start.y end.y
#>        <int> <chr>        <dbl> <dbl>        <int>   <dbl> <dbl>
#> 1          1 chr1           140   150            1     100   150
#> 2          2 chr2           210   240           NA      NA    NA
#> 3          3 chr2           380   415            3     300   399
#> 4          4 chr1           230   280            2     200   250
#> 5         NA chr2            NA    NA            4     415   450
```
