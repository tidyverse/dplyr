# Per-operation grouping with `.by`/`by`

There are two ways to group in dplyr:

- Persistent grouping with
  [`group_by()`](https://dplyr.tidyverse.org/dev/reference/group_by.md)

- Per-operation grouping with `.by`/`by`

This help page is dedicated to explaining where and why you might want
to use the latter.

Depending on the dplyr verb, the per-operation grouping argument may be
named `.by` or `by`. The *Supported verbs* section below outlines this
on a case-by-case basis. The remainder of this page will refer to `.by`
for simplicity.

Grouping radically affects the computation of the dplyr verb you use it
with, and one of the goals of `.by` is to allow you to place that
grouping specification alongside the code that actually uses it. As an
added benefit, with `.by` you no longer need to remember to
[`ungroup()`](https://dplyr.tidyverse.org/dev/reference/group_by.md)
after
[`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md),
and
[`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)
won't ever message you about how it's handling the groups!

This idea comes from
[data.table](https://CRAN.R-project.org/package=data.table), which
allows you to specify `by` alongside modifications in `j`, like:
`dt[, .(x = mean(x)), by = g]`.

### Supported verbs

- [`mutate(.by = )`](https://dplyr.tidyverse.org/dev/reference/mutate.md)

- [`summarise(.by = )`](https://dplyr.tidyverse.org/dev/reference/summarise.md)

- [`reframe(.by = )`](https://dplyr.tidyverse.org/dev/reference/reframe.md)

- [`filter(.by = )`](https://dplyr.tidyverse.org/dev/reference/filter.md)

- [`filter_out(.by = )`](https://dplyr.tidyverse.org/dev/reference/filter.md)

- [`slice(.by = )`](https://dplyr.tidyverse.org/dev/reference/slice.md)

- [`slice_head(by = )`](https://dplyr.tidyverse.org/dev/reference/slice.md)
  and
  [`slice_tail(by = )`](https://dplyr.tidyverse.org/dev/reference/slice.md)

- [`slice_min(by = )`](https://dplyr.tidyverse.org/dev/reference/slice.md)
  and
  [`slice_max(by = )`](https://dplyr.tidyverse.org/dev/reference/slice.md)

- [`slice_sample(by = )`](https://dplyr.tidyverse.org/dev/reference/slice.md)

Note that some dplyr verbs use `by` while others use `.by`. This is a
purely technical difference.

### Differences between `.by` and [`group_by()`](https://dplyr.tidyverse.org/dev/reference/group_by.md)

|                                                                                                      |                                                                                                    |
|------------------------------------------------------------------------------------------------------|----------------------------------------------------------------------------------------------------|
| `.by`                                                                                                | [`group_by()`](https://dplyr.tidyverse.org/dev/reference/group_by.md)                              |
| Grouping only affects a single verb                                                                  | Grouping is persistent across multiple verbs                                                       |
| Selects variables with [tidy-select](https://dplyr.tidyverse.org/dev/reference/dplyr_tidy_select.md) | Computes expressions with [data-masking](https://rlang.r-lib.org/reference/args_data_masking.html) |
| Summaries use existing order of group keys                                                           | Summaries sort group keys in ascending order                                                       |

### Using `.by`

Let's take a look at the two grouping approaches using this `expenses`
data set, which tracks costs accumulated across various `id`s and
`region`s:

    expenses <- tibble(
      id = c(1, 2, 1, 3, 1, 2, 3),
      region = c("A", "A", "A", "B", "B", "A", "A"),
      cost = c(25, 20, 19, 12, 9, 6, 6)
    )
    expenses
    #> # A tibble: 7 x 3
    #>      id region  cost
    #>   <dbl> <chr>  <dbl>
    #> 1     1 A         25
    #> 2     2 A         20
    #> 3     1 A         19
    #> 4     3 B         12
    #> 5     1 B          9
    #> 6     2 A          6
    #> 7     3 A          6

Imagine that you wanted to compute the average cost per region. You'd
probably write something like this:

    expenses |>
      group_by(region) |>
      summarise(cost = mean(cost))
    #> # A tibble: 2 x 2
    #>   region  cost
    #>   <chr>  <dbl>
    #> 1 A       15.2
    #> 2 B       10.5

Instead, you can now specify the grouping *inline* within the verb:

    expenses |>
      summarise(cost = mean(cost), .by = region)
    #> # A tibble: 2 x 2
    #>   region  cost
    #>   <chr>  <dbl>
    #> 1 A       15.2
    #> 2 B       10.5

`.by` applies to a single operation, meaning that since `expenses` was
an ungrouped data frame, the result after applying `.by` will also
always be an ungrouped data frame, regardless of the number of grouping
columns.

    expenses |>
      summarise(cost = mean(cost), .by = c(id, region))
    #> # A tibble: 5 x 3
    #>      id region  cost
    #>   <dbl> <chr>  <dbl>
    #> 1     1 A         22
    #> 2     2 A         13
    #> 3     3 B         12
    #> 4     1 B          9
    #> 5     3 A          6

Compare that with `group_by() |> summarise()`, where
[`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)
generally peels off 1 layer of grouping by default, typically with a
message that it is doing so:

    expenses |>
      group_by(id, region) |>
      summarise(cost = mean(cost))
    #> `summarise()` has regrouped the output.
    #> i Summaries were computed grouped by id and region.
    #> i Output is grouped by id.
    #> i Use `summarise(.groups = "drop_last")` to silence this message.
    #> i Use `summarise(.by = c(id, region))` for per-operation grouping
    #>   (`?dplyr::dplyr_by`) instead.
    #> # A tibble: 5 x 3
    #> # Groups:   id [3]
    #>      id region  cost
    #>   <dbl> <chr>  <dbl>
    #> 1     1 A         22
    #> 2     1 B          9
    #> 3     2 A         13
    #> 4     3 A          6
    #> 5     3 B         12

Because `.by` grouping applies to a single operation, you don't need to
worry about ungrouping, and it never needs to emit a message to remind
you what it is doing with the groups.

Note that with `.by` we specified multiple columns to group by using the
[tidy-select](https://dplyr.tidyverse.org/dev/reference/dplyr_tidy_select.md)
syntax `c(id, region)`. If you have a character vector of column names
you'd like to group by, you can do so with `.by = all_of(my_cols)`. It
will group by the columns in the order they were provided.

To prevent surprising results, you can't use `.by` on an existing
grouped data frame:

    expenses |>
      group_by(id) |>
      summarise(cost = mean(cost), .by = c(id, region))
    #> Error in `summarise()`:
    #> ! Can't supply `.by` when `.data` is a grouped data frame.

So far we've focused on the usage of `.by` with
[`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md),
but `.by` works with a number of other dplyr verbs. For example, you
could append the mean cost per region onto the original data frame as a
new column rather than computing a summary:

    expenses |>
      mutate(cost_by_region = mean(cost), .by = region)
    #> # A tibble: 7 x 4
    #>      id region  cost cost_by_region
    #>   <dbl> <chr>  <dbl>          <dbl>
    #> 1     1 A         25           15.2
    #> 2     2 A         20           15.2
    #> 3     1 A         19           15.2
    #> 4     3 B         12           10.5
    #> 5     1 B          9           10.5
    #> 6     2 A          6           15.2
    #> 7     3 A          6           15.2

Or you could slice out the maximum cost per combination of id and
region:

    # Note that the argument is named `by` in `slice_max()`
    expenses |>
      slice_max(cost, n = 1, by = c(id, region))
    #> # A tibble: 5 x 3
    #>      id region  cost
    #>   <dbl> <chr>  <dbl>
    #> 1     1 A         25
    #> 2     2 A         20
    #> 3     3 B         12
    #> 4     1 B          9
    #> 5     3 A          6

### Result ordering

When used with `.by`,
[`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md),
[`reframe()`](https://dplyr.tidyverse.org/dev/reference/reframe.md), and
[`slice()`](https://dplyr.tidyverse.org/dev/reference/slice.md) all
maintain the ordering of the existing data. This is different from
[`group_by()`](https://dplyr.tidyverse.org/dev/reference/group_by.md),
which has always sorted the group keys in ascending order.

    df <- tibble(
      month = c("jan", "jan", "feb", "feb", "mar"),
      temp = c(20, 25, 18, 20, 40)
    )

    # Uses ordering by "first appearance" in the original data
    df |>
      summarise(average_temp = mean(temp), .by = month)
    #> # A tibble: 3 x 2
    #>   month average_temp
    #>   <chr>        <dbl>
    #> 1 jan           22.5
    #> 2 feb           19
    #> 3 mar           40

    # Sorts in ascending order
    df |>
      group_by(month) |>
      summarise(average_temp = mean(temp))
    #> # A tibble: 3 x 2
    #>   month average_temp
    #>   <chr>        <dbl>
    #> 1 feb           19
    #> 2 jan           22.5
    #> 3 mar           40

If you need sorted group keys, we recommend that you explicitly use
[`arrange()`](https://dplyr.tidyverse.org/dev/reference/arrange.md)
either before or after the call to
[`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md),
[`reframe()`](https://dplyr.tidyverse.org/dev/reference/reframe.md), or
[`slice()`](https://dplyr.tidyverse.org/dev/reference/slice.md). This
also gives you full access to all of
[`arrange()`](https://dplyr.tidyverse.org/dev/reference/arrange.md)'s
features, such as
[`desc()`](https://dplyr.tidyverse.org/dev/reference/desc.md) and the
`.locale` argument.

### Verbs without `.by` support

If a dplyr verb doesn't support `.by`, then that typically means that
the verb isn't inherently affected by grouping. For example,
[`pull()`](https://dplyr.tidyverse.org/dev/reference/pull.md) and
[`rename()`](https://dplyr.tidyverse.org/dev/reference/rename.md) don't
support `.by`, because specifying columns to group by would not affect
their implementations.

That said, there are a few exceptions to this where sometimes a dplyr
verb doesn't support `.by`, but *does* have special support for grouped
data frames created by
[`group_by()`](https://dplyr.tidyverse.org/dev/reference/group_by.md).
This is typically because the verbs are required to retain the grouping
columns, for example:

- [`select()`](https://dplyr.tidyverse.org/dev/reference/select.md)
  always retains grouping columns, with a message if any aren't
  specified in the
  [`select()`](https://dplyr.tidyverse.org/dev/reference/select.md)
  call.

- [`distinct()`](https://dplyr.tidyverse.org/dev/reference/distinct.md)
  and [`count()`](https://dplyr.tidyverse.org/dev/reference/count.md)
  place unspecified grouping columns at the front of the data frame
  before computing their results.

- [`arrange()`](https://dplyr.tidyverse.org/dev/reference/arrange.md)
  has a `.by_group` argument to optionally order by grouping columns
  first.

If [`group_by()`](https://dplyr.tidyverse.org/dev/reference/group_by.md)
didn't exist, then these verbs would not have special support for
grouped data frames.
