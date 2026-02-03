# Argument type: tidy-select

This page describes the `<tidy-select>` argument modifier which
indicates the argument supports **tidy selections**. Tidy selection
provides a concise dialect of R for selecting variables based on their
names or properties.

Tidy selection is a variant of tidy evaluation. This means that inside
functions, tidy-select arguments require special attention, as described
in the *Indirection* section below. If you've never heard of tidy
evaluation before, start with
[`vignette("programming")`](https://dplyr.tidyverse.org/articles/programming.md).

## Overview of selection features

Tidyverse selections implement a dialect of R where operators make it
easy to select variables:

- `:` for selecting a range of consecutive variables.

- `!` for taking the complement of a set of variables.

- `&` and `|` for selecting the intersection or the union of two sets of
  variables.

- [`c()`](https://rdrr.io/r/base/c.html) for combining selections.

In addition, you can use **selection helpers**. Some helpers select
specific columns:

- [`everything()`](https://tidyselect.r-lib.org/reference/everything.html):
  Matches all variables.

- [`last_col()`](https://tidyselect.r-lib.org/reference/everything.html):
  Select last variable, possibly with an offset.

- [`group_cols()`](https://dplyr.tidyverse.org/reference/group_cols.md):
  Select all grouping columns.

Other helpers select variables by matching patterns in their names:

- [`starts_with()`](https://tidyselect.r-lib.org/reference/starts_with.html):
  Starts with a prefix.

- [`ends_with()`](https://tidyselect.r-lib.org/reference/starts_with.html):
  Ends with a suffix.

- [`contains()`](https://tidyselect.r-lib.org/reference/starts_with.html):
  Contains a literal string.

- [`matches()`](https://tidyselect.r-lib.org/reference/starts_with.html):
  Matches a regular expression.

- [`num_range()`](https://tidyselect.r-lib.org/reference/starts_with.html):
  Matches a numerical range like x01, x02, x03.

Or from variables stored in a character vector:

- [`all_of()`](https://tidyselect.r-lib.org/reference/all_of.html):
  Matches variable names in a character vector. All names must be
  present, otherwise an out-of-bounds error is thrown.

- [`any_of()`](https://tidyselect.r-lib.org/reference/all_of.html): Same
  as [`all_of()`](https://tidyselect.r-lib.org/reference/all_of.html),
  except that no error is thrown for names that don't exist.

Or using a predicate function:

- [`where()`](https://tidyselect.r-lib.org/reference/where.html):
  Applies a function to all variables and selects those for which the
  function returns `TRUE`.

## Indirection

There are two main cases:

- If you have a character vector of column names, use
  [`all_of()`](https://tidyselect.r-lib.org/reference/all_of.html) or
  [`any_of()`](https://tidyselect.r-lib.org/reference/all_of.html),
  depending on whether or not you want unknown variable names to cause
  an error, e.g. `select(df, all_of(vars))`,
  `select(df, !any_of(vars))`.

- If you want the user to be able to supply a tidyselect specification
  in a function argument, embrace the function argument, e.g.
  `select(df, {{ vars }})`.
