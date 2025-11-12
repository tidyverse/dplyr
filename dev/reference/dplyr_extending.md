# Extending dplyr with new data frame subclasses

**\[experimental\]**

These three functions, along with `names<-` and 1d numeric `[` (i.e.
`x[loc]`) methods, provide a minimal interface for extending dplyr to
work with new data frame subclasses. This means that for simple cases
you should only need to provide a couple of methods, rather than a
method for every dplyr verb.

These functions are a stop-gap measure until we figure out how to solve
the problem more generally, but it's likely that any code you write to
implement them will find a home in what comes next.

## Usage

``` r
dplyr_row_slice(data, i, ...)

dplyr_col_modify(data, cols)

dplyr_reconstruct(data, template)
```

## Arguments

- data:

  A tibble. We use tibbles because they avoid some inconsistent
  subset-assignment use cases.

- i:

  A numeric or logical vector that indexes the rows of `data`.

- cols:

  A named list used to modify columns. A `NULL` value should remove an
  existing column.

- template:

  Template data frame to use for restoring attributes.

## Basic advice

This section gives you basic advice if you want to extend dplyr to work
with your custom data frame subclass, and you want the dplyr methods to
behave in basically the same way.

- If you have data frame attributes that don't depend on the rows or
  columns (and should unconditionally be preserved), you don't need to
  do anything. The one exception to this is if your subclass extends a
  data.frame directly rather than extending a tibble. The `[.data.frame`
  method does not preserve attributes, so you'll need to write a `[`
  method for your subclass that preserves attributes important for your
  class.

- If you have **scalar** attributes that depend on **rows**, implement a
  `dplyr_reconstruct()` method. Your method should recompute the
  attribute depending on rows now present.

- If you have **scalar** attributes that depend on **columns**,
  implement a `dplyr_reconstruct()` method and a 1d `[` method. For
  example, if your class requires that certain columns be present, your
  method should return a data.frame or tibble when those columns are
  removed.

- If your attributes are **vectorised** over **rows**, implement a
  `dplyr_row_slice()` method. This gives you access to `i` so you can
  modify the row attribute accordingly. You'll also need to think
  carefully about how to recompute the attribute in
  `dplyr_reconstruct()`, and you will need to carefully verify the
  behaviour of each verb, and provide additional methods as needed.

- If your attributes that are **vectorised** over **columns**, implement
  `dplyr_col_modify()`, 1d `[`, and `names<-` methods. All of these
  methods know which columns are being modified, so you can update the
  column attribute according. You'll also need to think carefully about
  how to recompute the attribute in `dplyr_reconstruct()`, and you will
  need to carefully verify the behaviour of each verb, and provide
  additional methods as needed.

## Current usage

- [`arrange()`](https://dplyr.tidyverse.org/dev/reference/arrange.md),
  [`filter()`](https://dplyr.tidyverse.org/dev/reference/filter.md),
  [`slice()`](https://dplyr.tidyverse.org/dev/reference/slice.md) (and
  the rest of the `slice_*()` family),
  [`semi_join()`](https://dplyr.tidyverse.org/dev/reference/filter-joins.md),
  and
  [`anti_join()`](https://dplyr.tidyverse.org/dev/reference/filter-joins.md)
  work by generating a vector of row indices, and then subsetting with
  `dplyr_row_slice()`.

- [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md)
  generates a list of new column value (using `NULL` to indicate when
  columns should be deleted), then passes that to `dplyr_col_modify()`.
  It also uses 1d `[` to implement `.keep`, and will call
  [`relocate()`](https://dplyr.tidyverse.org/dev/reference/relocate.md)
  if either `.before` or `.after` are supplied.

- [`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)
  and
  [`reframe()`](https://dplyr.tidyverse.org/dev/reference/reframe.md)
  work similarly to
  [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md) but
  the data modified by `dplyr_col_modify()` comes from
  [`group_data()`](https://dplyr.tidyverse.org/dev/reference/group_data.md)
  or is built from `.by`. Note that this means that the data frames
  returned by
  [`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)
  and
  [`reframe()`](https://dplyr.tidyverse.org/dev/reference/reframe.md)
  are fundamentally new data frames, and will not retain any custom
  subclasses or attributes.

- [`select()`](https://dplyr.tidyverse.org/dev/reference/select.md) uses
  1d `[` to select columns, then `names<-` to rename them.
  [`rename()`](https://dplyr.tidyverse.org/dev/reference/rename.md) just
  uses `names<-`.
  [`relocate()`](https://dplyr.tidyverse.org/dev/reference/relocate.md)
  just uses 1d `[`.

- [`inner_join()`](https://dplyr.tidyverse.org/dev/reference/mutate-joins.md),
  [`left_join()`](https://dplyr.tidyverse.org/dev/reference/mutate-joins.md),
  [`right_join()`](https://dplyr.tidyverse.org/dev/reference/mutate-joins.md),
  and
  [`full_join()`](https://dplyr.tidyverse.org/dev/reference/mutate-joins.md)
  coerce `x` to a tibble, modify the rows, then use
  `dplyr_reconstruct()` to convert back to the same type as `x`.

- [`nest_join()`](https://dplyr.tidyverse.org/dev/reference/nest_join.md)
  converts both `x` and `y` to tibbles, modifies the rows, and uses
  `dplyr_col_modify()` to handle modified key variables and the
  list-column that `y` becomes. It also uses `dplyr_reconstruct()` to
  convert the outer result back to the type of `x`, and to convert the
  nested tibbles back to the type of `y`.

- [`distinct()`](https://dplyr.tidyverse.org/dev/reference/distinct.md)
  does a
  [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md) if
  any expressions are present, then uses 1d `[` to select variables to
  keep, then `dplyr_row_slice()` to select distinct rows.

Note that
[`group_by()`](https://dplyr.tidyverse.org/dev/reference/group_by.md)
and [`ungroup()`](https://dplyr.tidyverse.org/dev/reference/group_by.md)
don't use any of these generics and you'll need to provide methods for
them directly, or rely on `.by` for per-operation grouping.
