# Changelog

## dplyr (development version)

- New
  [`when_any()`](https://dplyr.tidyverse.org/dev/reference/when-any-all.md)
  and
  [`when_all()`](https://dplyr.tidyverse.org/dev/reference/when-any-all.md),
  which are elementwise versions of
  [`any()`](https://rdrr.io/r/base/any.html) and
  [`all()`](https://rdrr.io/r/base/all.html). Alternatively, you can
  think of them as performing repeated `|` and `&` on any number of
  inputs, for example:

  - `when_any(x, y, z)` is equivalent to `x | y | z`.

  - `when_all(x, y, z)` is equivalent to `x & y & z`.

  [`when_any()`](https://dplyr.tidyverse.org/dev/reference/when-any-all.md)
  is particularly useful within
  [`filter()`](https://dplyr.tidyverse.org/dev/reference/filter.md) and
  [`filter_out()`](https://dplyr.tidyverse.org/dev/reference/filter.md)
  to specify comma separated conditions combined with `|` rather than
  `&`.

  This work is a result of [Tidyup 8: Expanding the `filter()`
  family](https://github.com/tidyverse/tidyups/pull/30).

- New
  [`filter_out()`](https://dplyr.tidyverse.org/dev/reference/filter.md)
  companion to
  [`filter()`](https://dplyr.tidyverse.org/dev/reference/filter.md).

  - Use
    [`filter()`](https://dplyr.tidyverse.org/dev/reference/filter.md)
    when specifying rows to *keep*.

  - Use
    [`filter_out()`](https://dplyr.tidyverse.org/dev/reference/filter.md)
    when specifying rows to *drop*.

  [`filter_out()`](https://dplyr.tidyverse.org/dev/reference/filter.md)
  simplifies cases where you would have previously used a
  [`filter()`](https://dplyr.tidyverse.org/dev/reference/filter.md) to
  drop rows. It is particularly useful when missing values are involved.
  For example, to drop rows where the `count` is zero:

  ``` r
  df |> filter(count != 0 | is.na(count))

  df |> filter_out(count == 0)
  ```

  With
  [`filter()`](https://dplyr.tidyverse.org/dev/reference/filter.md), you
  must provide a “negative” condition of `!= 0` and must explicitly
  guard against accidentally dropping rows with `NA`. With
  [`filter_out()`](https://dplyr.tidyverse.org/dev/reference/filter.md),
  you directly specify rows to drop and you don’t have to guard against
  dropping rows with `NA`, which tends to result in much clearer code.

  This work is a result of [Tidyup 8: Expanding the `filter()`
  family](https://github.com/tidyverse/tidyups/pull/30), with a lot of
  great feedback from the community
  ([\#6560](https://github.com/tidyverse/dplyr/issues/6560),
  [\#6891](https://github.com/tidyverse/dplyr/issues/6891)).

- The `.groups` message emitted by
  [`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)
  is hopefully more clear now
  ([\#6986](https://github.com/tidyverse/dplyr/issues/6986)).

- [`if_any()`](https://dplyr.tidyverse.org/dev/reference/across.md) and
  [`if_all()`](https://dplyr.tidyverse.org/dev/reference/across.md) are
  now more consistent in all use cases
  ([\#7059](https://github.com/tidyverse/dplyr/issues/7059),
  [\#7077](https://github.com/tidyverse/dplyr/issues/7077),
  [\#7746](https://github.com/tidyverse/dplyr/issues/7746),
  [@jrwinget](https://github.com/jrwinget)). In particular:

  - When called with zero inputs,
    [`if_any()`](https://dplyr.tidyverse.org/dev/reference/across.md)
    returns `FALSE` and
    [`if_all()`](https://dplyr.tidyverse.org/dev/reference/across.md)
    returns `TRUE`.

  - When called with one input, both now return logical vectors rather
    than the original column.

  - The result of applying `.fns` now must be a logical vector.

- `tally_n()` creates fully qualified funciton calls for duckplyr
  compatibility
  ([\#7046](https://github.com/tidyverse/dplyr/issues/7046))

- `storms` has been updated to include 2023 and 2024 data
  ([\#7111](https://github.com/tidyverse/dplyr/issues/7111),
  [@tomalrussell](https://github.com/tomalrussell)).

- Empty
  [`rowwise()`](https://dplyr.tidyverse.org/dev/reference/rowwise.md)
  list-column elements now resolve to
  [`logical()`](https://rdrr.io/r/base/logical.html) rather than a
  random logical of length 1
  ([\#7710](https://github.com/tidyverse/dplyr/issues/7710)).

- [`last_dplyr_warnings()`](https://dplyr.tidyverse.org/dev/reference/last_dplyr_warnings.md)
  no longer prevents objects from being garbage collected
  ([\#7649](https://github.com/tidyverse/dplyr/issues/7649)).

- Progress towards making dplyr conformant with the public C API of R
  ([\#7741](https://github.com/tidyverse/dplyr/issues/7741),
  [\#7797](https://github.com/tidyverse/dplyr/issues/7797)).

- [`case_when()`](https://dplyr.tidyverse.org/dev/reference/case-and-replace-when.md)
  now throws correctly indexed errors when `NULL`s are supplied in `...`
  ([\#7739](https://github.com/tidyverse/dplyr/issues/7739)).

- [`case_when()`](https://dplyr.tidyverse.org/dev/reference/case-and-replace-when.md)
  has gained a new `.unmatched` argument. For extra safety, set
  `.unmatched = "error"` rather than providing a `.default` when you
  believe that you’ve handled every possible case, and it will error if
  a case is left unhandled. The new
  [`recode_values()`](https://dplyr.tidyverse.org/dev/reference/recode-and-replace-values.md)
  also has this argument
  ([\#7653](https://github.com/tidyverse/dplyr/issues/7653)).

- New [`rbind()`](https://rdrr.io/r/base/cbind.html) method for
  `rowwise_df` to avoid creating corrupt rowwise data frames
  (r-lib/vctrs#1935).

- [`case_match()`](https://dplyr.tidyverse.org/dev/reference/case_match.md)
  is soft-deprecated, and is fully replaced by
  [`recode_values()`](https://dplyr.tidyverse.org/dev/reference/recode-and-replace-values.md)
  and
  [`replace_values()`](https://dplyr.tidyverse.org/dev/reference/recode-and-replace-values.md),
  which are more flexible, more powerful, and have much better names.

- The superseded
  [`recode()`](https://dplyr.tidyverse.org/dev/reference/recode.md) now
  has updated documentation showing how to migrate to
  [`recode_values()`](https://dplyr.tidyverse.org/dev/reference/recode-and-replace-values.md)
  and
  [`replace_values()`](https://dplyr.tidyverse.org/dev/reference/recode-and-replace-values.md).

- [`case_when()`](https://dplyr.tidyverse.org/dev/reference/case-and-replace-when.md)
  is now part of a family of 4 related functions, 3 of which are new:

  - Use
    [`case_when()`](https://dplyr.tidyverse.org/dev/reference/case-and-replace-when.md)
    to create a new vector based on logical conditions.
  - Use
    [`replace_when()`](https://dplyr.tidyverse.org/dev/reference/case-and-replace-when.md)
    to update an existing vector based on logical conditions.
  - Use
    [`recode_values()`](https://dplyr.tidyverse.org/dev/reference/recode-and-replace-values.md)
    to create a new vector by mapping all old values to new values.
  - Use
    [`replace_values()`](https://dplyr.tidyverse.org/dev/reference/recode-and-replace-values.md)
    to update an existing vector by mapping some old values to new
    values.

  Learn all about these in a new vignette,
  [`vignette("recoding-replacing")`](https://dplyr.tidyverse.org/dev/articles/recoding-replacing.md).

  [`replace_when()`](https://dplyr.tidyverse.org/dev/reference/case-and-replace-when.md)
  is particularly useful for conditionally mutating rows within one or
  more columns, and can be thought of as an enhanced version of
  [`base::replace()`](https://rdrr.io/r/base/replace.html).

  [`recode_values()`](https://dplyr.tidyverse.org/dev/reference/recode-and-replace-values.md)
  and
  [`replace_values()`](https://dplyr.tidyverse.org/dev/reference/recode-and-replace-values.md)
  have the familiar
  [`case_when()`](https://dplyr.tidyverse.org/dev/reference/case-and-replace-when.md)-style
  formula interface for easy interactive use, but also have `from` and
  `to` arguments as a way for you to incorporate a pre-built lookup
  table, making them more holistic replacements for both
  [`case_match()`](https://dplyr.tidyverse.org/dev/reference/case_match.md)
  and [`recode()`](https://dplyr.tidyverse.org/dev/reference/recode.md).

  This work is a result of [Tidyup 7: Recoding and replacing values in
  the
  tidyverse](https://github.com/tidyverse/tidyups/blob/main/007-tidyverse-recoding-and-replacing.md),
  with a lot of great
  [feedback](https://github.com/tidyverse/tidyups/pull/29) from the
  community ([\#7728](https://github.com/tidyverse/dplyr/issues/7728),
  [\#7729](https://github.com/tidyverse/dplyr/issues/7729)).

- In
  [`case_when()`](https://dplyr.tidyverse.org/dev/reference/case-and-replace-when.md),
  supplying all size 1 LHS inputs along with a size \>1 RHS input is now
  soft-deprecated. This is an improper usage of
  [`case_when()`](https://dplyr.tidyverse.org/dev/reference/case-and-replace-when.md)
  that should instead be a series of if statements, like:

      # Scalars!
      code <- 1L
      flavor <- "vanilla"

      # Previously
      case_when(
        code == 1L && flavor == "chocolate" ~ x,
        code == 1L && flavor == "vanilla" ~ y,
        code == 2L && flavor == "vanilla" ~ z,
        .default = default
      )

      # Now
      if (code == 1L && flavor == "chocolate") {
        x
      } else if (code == 1L && flavor == "vanilla") {
        y
      } else if (code == 2L && flavor == "vanilla") {
        z
      } else {
        default
      }

  The recycling behavior that allows this style of
  [`case_when()`](https://dplyr.tidyverse.org/dev/reference/case-and-replace-when.md)
  to work is unsafe, and can result in silent bugs that we’d like to
  guard against with an error in the future
  ([\#7082](https://github.com/tidyverse/dplyr/issues/7082)).

- The following vector functions have gotten significantly faster and
  use much less memory due to a rewrite in C via vctrs
  ([\#7723](https://github.com/tidyverse/dplyr/issues/7723),
  [\#7725](https://github.com/tidyverse/dplyr/issues/7725),
  [\#7727](https://github.com/tidyverse/dplyr/issues/7727)):

  - [`if_else()`](https://dplyr.tidyverse.org/dev/reference/if_else.md)
  - [`case_when()`](https://dplyr.tidyverse.org/dev/reference/case-and-replace-when.md)
  - [`coalesce()`](https://dplyr.tidyverse.org/dev/reference/coalesce.md)

- [`if_else()`](https://dplyr.tidyverse.org/dev/reference/if_else.md) no
  longer allows `condition` to be a logical array. It must be a logical
  vector with no `dim` attribute
  ([\#7723](https://github.com/tidyverse/dplyr/issues/7723)).

- Passing `size` to
  [`if_else()`](https://dplyr.tidyverse.org/dev/reference/if_else.md) is
  now deprecated. The output size is always taken from the `condition`
  ([\#7722](https://github.com/tidyverse/dplyr/issues/7722)).

- [`bind_rows()`](https://dplyr.tidyverse.org/dev/reference/bind_rows.md)
  now replaces empty (or `NA`) element names in a list with its numeric
  index while preserving existing names
  ([\#7719](https://github.com/tidyverse/dplyr/issues/7719),
  [@Meghansaha](https://github.com/Meghansaha)).

- New
  [`slice_sample()`](https://dplyr.tidyverse.org/dev/reference/slice.md)
  example showing how to use it to shuffle rows
  ([\#7707](https://github.com/tidyverse/dplyr/issues/7707),
  [@Hzanib](https://github.com/Hzanib)).

- Updated
  [`across()`](https://dplyr.tidyverse.org/dev/reference/across.md)
  examples to include an example using
  [`everything()`](https://tidyselect.r-lib.org/reference/everything.html)
  ([\#7621](https://github.com/tidyverse/dplyr/issues/7621),
  [@JBrandenburg02](https://github.com/JBrandenburg02)).

- Clarified how
  [`slice_min()`](https://dplyr.tidyverse.org/dev/reference/slice.md)
  and
  [`slice_max()`](https://dplyr.tidyverse.org/dev/reference/slice.md)
  work in the introduction vignette
  ([\#7717](https://github.com/tidyverse/dplyr/issues/7717),
  [@ccani007](https://github.com/ccani007)).

- [`reframe()`](https://dplyr.tidyverse.org/dev/reference/reframe.md)
  has moved from experimental to stable
  ([\#7713](https://github.com/tidyverse/dplyr/issues/7713),
  [@VisruthSK](https://github.com/VisruthSK)).

- The base pipe is now used throughout the documentation
  ([\#7711](https://github.com/tidyverse/dplyr/issues/7711)).

- R \>=4.1.0 is now required, in line with the [tidyverse
  standard](https://tidyverse.org/blog/2019/04/r-version-support/) of
  supporting the previous 5 minor releases of R
  ([\#7711](https://github.com/tidyverse/dplyr/issues/7711)).

- [`case_when()`](https://dplyr.tidyverse.org/dev/reference/case-and-replace-when.md)
  now throws a better error if one of the conditions is an array
  ([\#6862](https://github.com/tidyverse/dplyr/issues/6862),
  [@ilovemane](https://github.com/ilovemane)).

- [`between()`](https://dplyr.tidyverse.org/dev/reference/between.md)
  gains a new `ptype` argument, allowing users to specify the desired
  output type. This is particularly useful for ordered factors and other
  complex types where the default common type behavior might not be
  ideal ([\#6906](https://github.com/tidyverse/dplyr/issues/6906),
  [@JamesHWade](https://github.com/JamesHWade)).

- Fixed an edge case when coercing data frames to matrices
  ([\#7004](https://github.com/tidyverse/dplyr/issues/7004)).

- Fixed an issue where duckplyr’s ALTREP data frames were being
  materialized early due to internal usage of
  [`ncol()`](https://rdrr.io/r/base/nrow.html)
  ([\#7049](https://github.com/tidyverse/dplyr/issues/7049)).

### Lifecycle changes

#### Breaking changes

- The following were already deprecated, and are now defunct:

  - All underscored standard evaluation versions of major dplyr verbs.
    Deprecated in 0.7.0 (Jun 2017), use the non-underscored version of
    the verb with unquoting instead, see
    [`vignette("programming")`](https://dplyr.tidyverse.org/dev/articles/programming.md).
    This includes:

    - [`add_count_()`](https://dplyr.tidyverse.org/dev/reference/defunct-lazyeval.md)
    - [`add_tally_()`](https://dplyr.tidyverse.org/dev/reference/defunct-lazyeval.md)
    - [`arrange_()`](https://dplyr.tidyverse.org/dev/reference/defunct-lazyeval.md)
    - [`count_()`](https://dplyr.tidyverse.org/dev/reference/defunct-lazyeval.md)
    - [`distinct_()`](https://dplyr.tidyverse.org/dev/reference/defunct-lazyeval.md)
    - [`do_()`](https://dplyr.tidyverse.org/dev/reference/defunct-lazyeval.md)
    - [`filter_()`](https://dplyr.tidyverse.org/dev/reference/defunct-lazyeval.md)
    - [`funs_()`](https://dplyr.tidyverse.org/dev/reference/defunct-lazyeval.md)
    - [`group_by_()`](https://dplyr.tidyverse.org/dev/reference/defunct-lazyeval.md)
    - [`group_indices_()`](https://dplyr.tidyverse.org/dev/reference/defunct-lazyeval.md)
    - [`mutate_()`](https://dplyr.tidyverse.org/dev/reference/defunct-lazyeval.md)
    - [`tally_()`](https://dplyr.tidyverse.org/dev/reference/defunct-lazyeval.md)
    - [`transmute_()`](https://dplyr.tidyverse.org/dev/reference/defunct-lazyeval.md)
    - [`rename_()`](https://dplyr.tidyverse.org/dev/reference/defunct-lazyeval.md)
    - [`select_()`](https://dplyr.tidyverse.org/dev/reference/defunct-lazyeval.md)
    - [`slice_()`](https://dplyr.tidyverse.org/dev/reference/defunct-lazyeval.md)
    - [`summarise_()`](https://dplyr.tidyverse.org/dev/reference/defunct-lazyeval.md)
    - [`summarize_()`](https://dplyr.tidyverse.org/dev/reference/defunct-lazyeval.md)

  - [`mutate_each()`](https://dplyr.tidyverse.org/dev/reference/defunct-each.md),
    [`mutate_each_()`](https://dplyr.tidyverse.org/dev/reference/defunct-each.md),
    [`summarise_each()`](https://dplyr.tidyverse.org/dev/reference/defunct-each.md),
    and
    [`summarise_each_()`](https://dplyr.tidyverse.org/dev/reference/defunct-each.md).
    Deprecated in 0.7.0 (Jun 2017), use
    [`across()`](https://dplyr.tidyverse.org/dev/reference/across.md)
    instead.

  - [`combine()`](https://dplyr.tidyverse.org/dev/reference/defunct.md).
    Deprecated in 1.0.0 (May 2020), use
    [`c()`](https://rdrr.io/r/base/c.html) or
    [`vctrs::vec_c()`](https://vctrs.r-lib.org/reference/vec_c.html)
    instead.

  - [`src_mysql()`](https://dplyr.tidyverse.org/dev/reference/defunct.md),
    [`src_postgres()`](https://dplyr.tidyverse.org/dev/reference/defunct.md),
    [`src_sqlite()`](https://dplyr.tidyverse.org/dev/reference/defunct.md),
    [`src_local()`](https://dplyr.tidyverse.org/dev/reference/defunct.md),
    and
    [`src_df()`](https://dplyr.tidyverse.org/dev/reference/defunct.md).
    Deprecated in 1.0.0 (May 2020), use
    [`tbl()`](https://dplyr.tidyverse.org/dev/reference/tbl.md) instead.

  - [`tbl_df()`](https://dplyr.tidyverse.org/dev/reference/defunct.md)
    and
    [`as.tbl()`](https://dplyr.tidyverse.org/dev/reference/defunct.md).
    Deprecated in 1.0.0 (May 2020), use
    [`tibble::as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)
    instead.

  - [`add_rownames()`](https://dplyr.tidyverse.org/dev/reference/defunct.md).
    Deprecated in 1.0.0 (May 2020), use
    [`tibble::rownames_to_column()`](https://tibble.tidyverse.org/reference/rownames.html)
    instead.

  - The `.drop` argument of
    [`add_count()`](https://dplyr.tidyverse.org/dev/reference/count.md).
    Deprecated in 1.0.0 (May 2020), had no effect.

  - The `add` argument of
    [`group_by()`](https://dplyr.tidyverse.org/dev/reference/group_by.md)
    and
    [`group_by_prepare()`](https://dplyr.tidyverse.org/dev/reference/group_by_prepare.md).
    Deprecated in 1.0.0 (May 2020), use `.add` instead.

  - The `.dots` argument of
    [`group_by()`](https://dplyr.tidyverse.org/dev/reference/group_by.md)
    and
    [`group_by_prepare()`](https://dplyr.tidyverse.org/dev/reference/group_by_prepare.md).
    Deprecated in 1.0.0 (May 2020).

  - The `...` argument of
    [`group_keys()`](https://dplyr.tidyverse.org/dev/reference/group_data.md)
    and
    [`group_indices()`](https://dplyr.tidyverse.org/dev/reference/group_data.md).
    Deprecated in 1.0.0 (May 2020), use
    [`group_by()`](https://dplyr.tidyverse.org/dev/reference/group_by.md)
    first.

  - The `keep` argument of
    [`group_map()`](https://dplyr.tidyverse.org/dev/reference/group_map.md),
    [`group_modify()`](https://dplyr.tidyverse.org/dev/reference/group_map.md),
    and
    [`group_split()`](https://dplyr.tidyverse.org/dev/reference/group_split.md).
    Deprecated in 1.0.0 (May 2020), use `.keep` instead.

  - Using
    [`across()`](https://dplyr.tidyverse.org/dev/reference/across.md)
    and data frames in
    [`filter()`](https://dplyr.tidyverse.org/dev/reference/filter.md).
    Deprecated in 1.0.8 (Feb 2022), use
    [`if_any()`](https://dplyr.tidyverse.org/dev/reference/across.md) or
    [`if_all()`](https://dplyr.tidyverse.org/dev/reference/across.md)
    instead.

  - Returning more or less than 1 row per group in
    [`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md).
    Deprecated in 1.1.0 (Jan 2023), use
    [`reframe()`](https://dplyr.tidyverse.org/dev/reference/reframe.md)
    instead.

  - `multiple = NULL` in joins. Deprecated in 1.1.1 (Mar 2023), use
    `multiple = "all"` instead.

  - `multiple = "error" / "warning"` in joins. Deprecated in 1.1.1 (Mar
    2023), use `relationship = "many-to-one"` instead.

  - The `vars` argument of
    [`group_cols()`](https://dplyr.tidyverse.org/dev/reference/group_cols.md).
    Deprecated in 1.0.0 (Jan 2023).

- The following were already defunct, and have been removed:

  - `id()`. Deprecated in 0.5.0 (Jun 2016), use
    [`vctrs::vec_group_id()`](https://vctrs.r-lib.org/reference/vec_group.html)
    instead. If your package uses NSE and implicitly relied on the
    variable `id` being available, you now need to put
    `utils::globalVariables("id")` inside one of your package files to
    tell R that `id` is a column name.

  - `failwith()`. Deprecated in 0.7.0 (Jun 2017), use
    [`purrr::possibly()`](https://purrr.tidyverse.org/reference/possibly.html)
    instead.

  - `select_vars()` and `select_vars_()`. Deprecated in 0.8.4 (Jan
    2020), use
    [`tidyselect::vars_select()`](https://tidyselect.r-lib.org/reference/vars_select.html)
    instead.

  - `rename_vars()` and `rename_vars_()`. Deprecated in 0.8.4 (Jan
    2020), use
    [`tidyselect::vars_rename()`](https://tidyselect.r-lib.org/reference/vars_select.html)
    instead.

  - `select_var()`. Deprecated in 0.8.4 (Jan 2020), use
    [`tidyselect::vars_pull()`](https://tidyselect.r-lib.org/reference/vars_pull.html)
    instead.

  - `current_vars()`. Deprecated in 0.8.4 (Jan 2020), use
    [`tidyselect::peek_vars()`](https://tidyselect.r-lib.org/reference/peek_vars.html)
    instead.

  - `bench_tbls()`, `compare_tbls()`, `compare_tbls2()`, `eval_tbls()`,
    and `eval_tbls2()`. Deprecated in 1.0.0 (May 2020).

  - `location()` and `changes()`. Deprecated in 1.0.0 (May 2020), use
    [`lobstr::ref()`](https://lobstr.r-lib.org/reference/ref.html)
    instead.

#### Newly deprecated

- The following were already deprecated, and now warn unconditionally if
  used:

  - [`all_equal()`](https://dplyr.tidyverse.org/dev/reference/all_equal.md).
    Deprecated in 1.1.0 (Jan 2023), use
    [`all.equal()`](https://rdrr.io/r/base/all.equal.html) instead.

  - [`progress_estimated()`](https://dplyr.tidyverse.org/dev/reference/progress_estimated.md).
    Deprecated in 1.0.0 (May 2020).

  - [`filter()`](https://dplyr.tidyverse.org/dev/reference/filter.md)
    with a 1 column matrix. Deprecated in 1.1.0 (Jan 2023), use a vector
    instead.

  - [`slice()`](https://dplyr.tidyverse.org/dev/reference/slice.md) with
    a 1 column matrix. Deprecated in 1.1.0 (Jan 2023), use a vector
    instead.

  - Not supplying the `.cols` argument of
    [`across()`](https://dplyr.tidyverse.org/dev/reference/across.md).
    Deprecated in 1.1.0 (Jan 2023).

  - [`group_indices()`](https://dplyr.tidyverse.org/dev/reference/group_data.md)
    with no arguments. Deprecated in 1.0.0 (May 2020), use
    [`cur_group_id()`](https://dplyr.tidyverse.org/dev/reference/context.md)
    instead.

- The following were already soft-deprecated, and now warn
  unconditionally once per session if used:

  - [`cur_data()`](https://dplyr.tidyverse.org/dev/reference/deprec-context.md)
    and
    [`cur_data_all()`](https://dplyr.tidyverse.org/dev/reference/deprec-context.md).
    Deprecated in 1.1.0 (Jan 2023), use
    [`pick()`](https://dplyr.tidyverse.org/dev/reference/pick.md)
    instead.

  - The `...` argument of
    [`across()`](https://dplyr.tidyverse.org/dev/reference/across.md).
    Deprecated in 1.1.0 (Jan 2023), use an anonymous function instead.

  - Using `by = character()` to perform a cross join. Deprecated in
    1.1.0 (Jan 2023), use
    [`cross_join()`](https://dplyr.tidyverse.org/dev/reference/cross_join.md)
    instead.

- The following are newly deprecated:

  - The `dplyr.legacy_locale` global option. If you used this to affect
    the ordering of
    [`arrange()`](https://dplyr.tidyverse.org/dev/reference/arrange.md),
    use `arrange(.locale =)` instead. If you used this to affect the
    ordering of `group_by() |> summarise()`, follow up with an
    additional call to `arrange(.locale =)` instead
    ([\#7760](https://github.com/tidyverse/dplyr/issues/7760)).

#### Newly stable

- `.by` has moved from experimental to stable
  ([\#7762](https://github.com/tidyverse/dplyr/issues/7762)).

## dplyr 1.1.4

CRAN release: 2023-11-17

- [`join_by()`](https://dplyr.tidyverse.org/dev/reference/join_by.md)
  now allows its helper functions to be namespaced with `dplyr::`, like
  `join_by(dplyr::between(x, lower, upper))`
  ([\#6838](https://github.com/tidyverse/dplyr/issues/6838)).

- [`left_join()`](https://dplyr.tidyverse.org/dev/reference/mutate-joins.md)
  and friends now return a specialized error message if they detect that
  your join would return more rows than dplyr can handle
  ([\#6912](https://github.com/tidyverse/dplyr/issues/6912)).

- `slice_*()` now throw the correct error if you forget to name `n`
  while also prefixing the call with `dplyr::`
  ([\#6946](https://github.com/tidyverse/dplyr/issues/6946)).

- [`dplyr_reconstruct()`](https://dplyr.tidyverse.org/dev/reference/dplyr_extending.md)’s
  default method has been rewritten to avoid materializing duckplyr
  queries too early
  ([\#6947](https://github.com/tidyverse/dplyr/issues/6947)).

- Updated the `storms` data to include 2022 data
  ([\#6937](https://github.com/tidyverse/dplyr/issues/6937),
  [@steveharoz](https://github.com/steveharoz)).

- Updated the `starwars` data to use a new API, because the old one is
  defunct. There are very minor changes to the data itself
  ([\#6938](https://github.com/tidyverse/dplyr/issues/6938),
  [@steveharoz](https://github.com/steveharoz)).

## dplyr 1.1.3

CRAN release: 2023-09-03

- [`mutate_each()`](https://dplyr.tidyverse.org/dev/reference/defunct-each.md)
  and
  [`summarise_each()`](https://dplyr.tidyverse.org/dev/reference/defunct-each.md)
  now throw correct deprecation messages
  ([\#6869](https://github.com/tidyverse/dplyr/issues/6869)).

- [`setequal()`](https://dplyr.tidyverse.org/dev/reference/setops.md)
  now requires the input data frames to be compatible, similar to the
  other set methods like
  [`setdiff()`](https://dplyr.tidyverse.org/dev/reference/setops.md) or
  [`intersect()`](https://dplyr.tidyverse.org/dev/reference/setops.md)
  ([\#6786](https://github.com/tidyverse/dplyr/issues/6786)).

## dplyr 1.1.2

CRAN release: 2023-04-20

- [`count()`](https://dplyr.tidyverse.org/dev/reference/count.md) better
  documents that it has a `.drop` argument
  ([\#6820](https://github.com/tidyverse/dplyr/issues/6820)).

- Fixed tests to maintain compatibility with the next version of waldo
  ([\#6823](https://github.com/tidyverse/dplyr/issues/6823)).

- Joins better handle key columns will all `NA`s
  ([\#6804](https://github.com/tidyverse/dplyr/issues/6804)).

## dplyr 1.1.1

CRAN release: 2023-03-22

- Mutating joins now warn about multiple matches much less often. At a
  high level, a warning was previously being thrown when a one-to-many
  or many-to-many relationship was detected between the keys of `x` and
  `y`, but is now only thrown for a many-to-many relationship, which is
  much rarer and much more dangerous than one-to-many because it can
  result in a Cartesian explosion in the number of rows returned from
  the join ([\#6731](https://github.com/tidyverse/dplyr/issues/6731),
  [\#6717](https://github.com/tidyverse/dplyr/issues/6717)).

  We’ve accomplished this in two steps:

  - `multiple` now defaults to `"all"`, and the options of `"error"` and
    `"warning"` are now deprecated in favor of using `relationship` (see
    below). We are using an accelerated deprecation process for these
    two options because they’ve only been available for a few weeks, and
    `relationship` is a clearly superior alternative.

  - The mutating joins gain a new `relationship` argument, allowing you
    to optionally enforce one of the following relationship constraints
    between the keys of `x` and `y`: `"one-to-one"`, `"one-to-many"`,
    `"many-to-one"`, or `"many-to-many"`.

    For example, `"many-to-one"` enforces that each row in `x` can match
    at most 1 row in `y`. If a row in `x` matches \>1 rows in `y`, an
    error is thrown. This option serves as the replacement for
    `multiple = "error"`.

    The default behavior of `relationship` doesn’t assume that there is
    any relationship between `x` and `y`. However, for equality joins it
    will check for the presence of a many-to-many relationship, and will
    warn if it detects one.

  This change unfortunately does mean that if you have set
  `multiple = "all"` to avoid a warning and you happened to be doing a
  many-to-many style join, then you will need to replace
  `multiple = "all"` with `relationship = "many-to-many"` to silence the
  new warning, but we believe this should be rare since many-to-many
  relationships are fairly uncommon.

- Fixed a major performance regression in
  [`case_when()`](https://dplyr.tidyverse.org/dev/reference/case-and-replace-when.md).
  It is still a little slower than in dplyr 1.0.10, but we plan to
  improve this further in the future
  ([\#6674](https://github.com/tidyverse/dplyr/issues/6674)).

- Fixed a performance regression related to
  [`nth()`](https://dplyr.tidyverse.org/dev/reference/nth.md),
  [`first()`](https://dplyr.tidyverse.org/dev/reference/nth.md), and
  [`last()`](https://dplyr.tidyverse.org/dev/reference/nth.md)
  ([\#6682](https://github.com/tidyverse/dplyr/issues/6682)).

- Fixed an issue where expressions involving infix operators had an
  abnormally large amount of overhead
  ([\#6681](https://github.com/tidyverse/dplyr/issues/6681)).

- [`group_data()`](https://dplyr.tidyverse.org/dev/reference/group_data.md)
  on ungrouped data frames is faster
  ([\#6736](https://github.com/tidyverse/dplyr/issues/6736)).

- [`n()`](https://dplyr.tidyverse.org/dev/reference/context.md) is a
  little faster when there are many groups
  ([\#6727](https://github.com/tidyverse/dplyr/issues/6727)).

- [`pick()`](https://dplyr.tidyverse.org/dev/reference/pick.md) now
  returns a 1 row, 0 column tibble when `...` evaluates to an empty
  selection. This makes it more compatible with [tidyverse recycling
  rules](https://vctrs.r-lib.org/reference/theory-faq-recycling.html) in
  some edge cases
  ([\#6685](https://github.com/tidyverse/dplyr/issues/6685)).

- [`if_else()`](https://dplyr.tidyverse.org/dev/reference/if_else.md)
  and
  [`case_when()`](https://dplyr.tidyverse.org/dev/reference/case-and-replace-when.md)
  again accept logical conditions that have attributes
  ([\#6678](https://github.com/tidyverse/dplyr/issues/6678)).

- [`arrange()`](https://dplyr.tidyverse.org/dev/reference/arrange.md)
  can once again sort the `numeric_version` type from base R
  ([\#6680](https://github.com/tidyverse/dplyr/issues/6680)).

- [`slice_sample()`](https://dplyr.tidyverse.org/dev/reference/slice.md)
  now works when the input has a column named `replace`.
  [`slice_min()`](https://dplyr.tidyverse.org/dev/reference/slice.md)
  and
  [`slice_max()`](https://dplyr.tidyverse.org/dev/reference/slice.md)
  now work when the input has columns named `na_rm` or `with_ties`
  ([\#6725](https://github.com/tidyverse/dplyr/issues/6725)).

- [`nth()`](https://dplyr.tidyverse.org/dev/reference/nth.md) now errors
  informatively if `n` is `NA`
  ([\#6682](https://github.com/tidyverse/dplyr/issues/6682)).

- Joins now throw a more informative error when `y` doesn’t have the
  same source as `x`
  ([\#6798](https://github.com/tidyverse/dplyr/issues/6798)).

- All major dplyr verbs now throw an informative error message if the
  input data frame contains a column named `NA` or `""`
  ([\#6758](https://github.com/tidyverse/dplyr/issues/6758)).

- Deprecation warnings thrown by
  [`filter()`](https://dplyr.tidyverse.org/dev/reference/filter.md) now
  mention the correct package where the problem originated from
  ([\#6679](https://github.com/tidyverse/dplyr/issues/6679)).

- Fixed an issue where using `<-` within a grouped
  [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md) or
  [`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)
  could cross contaminate other groups
  ([\#6666](https://github.com/tidyverse/dplyr/issues/6666)).

- The compatibility vignette has been replaced with a more general
  vignette on using dplyr in packages,
  [`vignette("in-packages")`](https://dplyr.tidyverse.org/dev/articles/in-packages.md)
  ([\#6702](https://github.com/tidyverse/dplyr/issues/6702)).

- The developer documentation in
  [`?dplyr_extending`](https://dplyr.tidyverse.org/dev/reference/dplyr_extending.md)
  has been refreshed and brought up to date with all changes made in
  1.1.0 ([\#6695](https://github.com/tidyverse/dplyr/issues/6695)).

- [`rename_with()`](https://dplyr.tidyverse.org/dev/reference/rename.md)
  now includes an example of using `paste0(recycle0 = TRUE)` to
  correctly handle empty selections
  ([\#6688](https://github.com/tidyverse/dplyr/issues/6688)).

- R \>=3.5.0 is now explicitly required. This is in line with the
  tidyverse policy of supporting the [5 most recent versions of
  R](https://tidyverse.org/blog/2019/04/r-version-support/).

## dplyr 1.1.0

CRAN release: 2023-01-29

### New features

- [`.by`/`by`](https://dplyr.tidyverse.org/reference/dplyr_by.html) is
  an experimental alternative to
  [`group_by()`](https://dplyr.tidyverse.org/dev/reference/group_by.md)
  that supports per-operation grouping for
  [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md),
  [`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md),
  [`filter()`](https://dplyr.tidyverse.org/dev/reference/filter.md), and
  the [`slice()`](https://dplyr.tidyverse.org/dev/reference/slice.md)
  family ([\#6528](https://github.com/tidyverse/dplyr/issues/6528)).

  Rather than:

      starwars %>%
        group_by(species, homeworld) %>%
        summarise(mean_height = mean(height))

  You can now write:

      starwars %>%
        summarise(
          mean_height = mean(height),
          .by = c(species, homeworld)
        )

  The most useful reason to do this is because `.by` only affects a
  single operation. In the example above, an ungrouped data frame went
  into the
  [`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)
  call, so an ungrouped data frame will come out; with `.by`, you never
  need to remember to
  [`ungroup()`](https://dplyr.tidyverse.org/dev/reference/group_by.md)
  afterwards and you never need to use the `.groups` argument.

  Additionally, using
  [`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)
  with `.by` will never sort the results by the group key, unlike with
  [`group_by()`](https://dplyr.tidyverse.org/dev/reference/group_by.md).
  Instead, the results are returned using the existing ordering of the
  groups from the original data. We feel this is more predictable,
  better maintains any ordering you might have already applied with a
  previous call to
  [`arrange()`](https://dplyr.tidyverse.org/dev/reference/arrange.md),
  and provides a way to maintain the current ordering without having to
  resort to factors.

  This feature was inspired by
  [data.table](https://CRAN.R-project.org/package=data.table), where the
  equivalent syntax looks like:

      starwars[, .(mean_height = mean(height)), by = .(species, homeworld)]

  [`with_groups()`](https://dplyr.tidyverse.org/dev/reference/with_groups.md)
  is superseded in favor of `.by`
  ([\#6582](https://github.com/tidyverse/dplyr/issues/6582)).

- [`reframe()`](https://dplyr.tidyverse.org/dev/reference/reframe.md) is
  a new experimental verb that creates a new data frame by applying
  functions to columns of an existing data frame. It is very similar to
  [`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md),
  with two big differences:

  - [`reframe()`](https://dplyr.tidyverse.org/dev/reference/reframe.md)
    can return an arbitrary number of rows per group, while
    [`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)
    reduces each group down to a single row.

  - [`reframe()`](https://dplyr.tidyverse.org/dev/reference/reframe.md)
    always returns an ungrouped data frame, while
    [`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)
    might return a grouped or rowwise data frame, depending on the
    scenario.

  [`reframe()`](https://dplyr.tidyverse.org/dev/reference/reframe.md)
  has been added in response to valid concern from the community that
  allowing
  [`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)
  to return any number of rows per group increases the chance for
  accidental bugs. We still feel that this is a powerful technique, and
  is a principled replacement for
  [`do()`](https://dplyr.tidyverse.org/dev/reference/do.md), so we have
  moved these features to
  [`reframe()`](https://dplyr.tidyverse.org/dev/reference/reframe.md)
  ([\#6382](https://github.com/tidyverse/dplyr/issues/6382)).

- [`group_by()`](https://dplyr.tidyverse.org/dev/reference/group_by.md)
  now uses a new algorithm for computing groups. It is often faster than
  the previous approach (especially when there are many groups), and in
  most cases there should be no changes. The one exception is with
  character vectors, see the C locale news bullet below for more details
  ([\#4406](https://github.com/tidyverse/dplyr/issues/4406),
  [\#6297](https://github.com/tidyverse/dplyr/issues/6297)).

- [`arrange()`](https://dplyr.tidyverse.org/dev/reference/arrange.md)
  now uses a faster algorithm for sorting character vectors, which is
  heavily inspired by data.table’s `forder()`. See the C locale news
  bullet below for more details
  ([\#4962](https://github.com/tidyverse/dplyr/issues/4962)).

- Joins have been completely overhauled to enable more flexible join
  operations and provide more tools for quality control. Many of these
  changes are inspired by data.table’s join syntax
  ([\#5914](https://github.com/tidyverse/dplyr/issues/5914),
  [\#5661](https://github.com/tidyverse/dplyr/issues/5661),
  [\#5413](https://github.com/tidyverse/dplyr/issues/5413),
  [\#2240](https://github.com/tidyverse/dplyr/issues/2240)).

  - A *join specification* can now be created through
    [`join_by()`](https://dplyr.tidyverse.org/dev/reference/join_by.md).
    This allows you to specify both the left and right hand side of a
    join using unquoted column names, such as
    `join_by(sale_date == commercial_date)`. Join specifications can be
    supplied to any `*_join()` function as the `by` argument.

  - Join specifications allow for new types of joins:

    - Equality joins: The most common join, specified by `==`. For
      example, `join_by(sale_date == commercial_date)`.

    - Inequality joins: For joining on inequalities, i.e.`>=`, `>`, `<`,
      and `<=`. For example, use `join_by(sale_date >= commercial_date)`
      to find every commercial that aired before a particular sale.

    - Rolling joins: For “rolling” the closest match forward or
      backwards when there isn’t an exact match, specified by using the
      rolling helper,
      [`closest()`](https://dplyr.tidyverse.org/dev/reference/join_by.md).
      For example, `join_by(closest(sale_date >= commercial_date))` to
      find only the most recent commercial that aired before a
      particular sale.

    - Overlap joins: For detecting overlaps between sets of columns,
      specified by using one of the overlap helpers:
      [`between()`](https://dplyr.tidyverse.org/dev/reference/between.md),
      [`within()`](https://dplyr.tidyverse.org/dev/reference/join_by.md),
      or
      [`overlaps()`](https://dplyr.tidyverse.org/dev/reference/join_by.md).
      For example, use
      `join_by(between(commercial_date, sale_date_lower, sale_date))` to
      find commercials that aired before a particular sale, as long as
      they occurred after some lower bound, such as 40 days before the
      sale was made.

    Note that you cannot use arbitrary expressions in the join
    conditions, like `join_by(sale_date - 40 >= commercial_date)`.
    Instead, use
    [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md) to
    create a new column containing the result of `sale_date - 40` and
    refer to that by name in
    [`join_by()`](https://dplyr.tidyverse.org/dev/reference/join_by.md).

  - `multiple` is a new argument for controlling what happens when a row
    in `x` matches multiple rows in `y`. For equality joins and rolling
    joins, where this is usually surprising, this defaults to signalling
    a `"warning"`, but still returns all of the matches. For inequality
    joins, where multiple matches are usually expected, this defaults to
    returning `"all"` of the matches. You can also return only the
    `"first"` or `"last"` match, `"any"` of the matches, or you can
    `"error"`.

  - `keep` now defaults to `NULL` rather than `FALSE`. `NULL` implies
    `keep = FALSE` for equality conditions, but `keep = TRUE` for
    inequality conditions, since you generally want to preserve both
    sides of an inequality join.

  - `unmatched` is a new argument for controlling what happens when a
    row would be dropped because it doesn’t have a match. For backwards
    compatibility, the default is `"drop"`, but you can also choose to
    `"error"` if dropped rows would be surprising.

- [`across()`](https://dplyr.tidyverse.org/dev/reference/across.md)
  gains an experimental `.unpack` argument to optionally unpack (as in,
  [`tidyr::unpack()`](https://tidyr.tidyverse.org/reference/pack.html))
  data frames returned by functions in `.fns`
  ([\#6360](https://github.com/tidyverse/dplyr/issues/6360)).

- [`consecutive_id()`](https://dplyr.tidyverse.org/dev/reference/consecutive_id.md)
  for creating groups based on contiguous runs of the same values, like
  [`data.table::rleid()`](https://rdrr.io/pkg/data.table/man/rleid.html)
  ([\#1534](https://github.com/tidyverse/dplyr/issues/1534)).

- [`case_match()`](https://dplyr.tidyverse.org/dev/reference/case_match.md)
  is a “vectorised switch” variant of
  [`case_when()`](https://dplyr.tidyverse.org/dev/reference/case-and-replace-when.md)
  that matches on values rather than logical expressions. It is like a
  SQL “simple” `CASE WHEN` statement, whereas
  [`case_when()`](https://dplyr.tidyverse.org/dev/reference/case-and-replace-when.md)
  is like a SQL “searched” `CASE WHEN` statement
  ([\#6328](https://github.com/tidyverse/dplyr/issues/6328)).

- [`cross_join()`](https://dplyr.tidyverse.org/dev/reference/cross_join.md)
  is a more explicit and slightly more correct replacement for using
  `by = character()` during a join
  ([\#6604](https://github.com/tidyverse/dplyr/issues/6604)).

- [`pick()`](https://dplyr.tidyverse.org/dev/reference/pick.md) makes it
  easy to access a subset of columns from the current group.
  [`pick()`](https://dplyr.tidyverse.org/dev/reference/pick.md) is
  intended as a replacement for `across(.fns = NULL)`,
  [`cur_data()`](https://dplyr.tidyverse.org/dev/reference/deprec-context.md),
  and
  [`cur_data_all()`](https://dplyr.tidyverse.org/dev/reference/deprec-context.md).
  We feel that
  [`pick()`](https://dplyr.tidyverse.org/dev/reference/pick.md) is a
  much more evocative name when you are just trying to select a subset
  of columns from your data
  ([\#6204](https://github.com/tidyverse/dplyr/issues/6204)).

- [`symdiff()`](https://dplyr.tidyverse.org/dev/reference/setops.md)
  computes the symmetric difference
  ([\#4811](https://github.com/tidyverse/dplyr/issues/4811)).

### Lifecycle changes

#### Breaking changes

- [`arrange()`](https://dplyr.tidyverse.org/dev/reference/arrange.md)
  and
  [`group_by()`](https://dplyr.tidyverse.org/dev/reference/group_by.md)
  now use the C locale, not the system locale, when ordering or grouping
  character vectors. This brings *substantial* performance improvements,
  increases reproducibility across R sessions, makes dplyr more
  consistent with data.table, and we believe it should affect little
  existing code. If it does affect your code, you can use
  `options(dplyr.legacy_locale = TRUE)` to quickly revert to the
  previous behavior. However, in general, we instead recommend that you
  use the new `.locale` argument to precisely specify the desired
  locale. For a full explanation please read the associated
  [grouping](https://github.com/tidyverse/tidyups/blob/main/006-dplyr-group-by-ordering.md)
  and
  [ordering](https://github.com/tidyverse/tidyups/blob/main/003-dplyr-radix-ordering.md)
  tidyups.

- `bench_tbls()`, `compare_tbls()`, `compare_tbls2()`, `eval_tbls()`,
  `eval_tbls2()`, `location()` and `changes()`, Deprecated in 1.0.0 (May
  2020), are now defunct
  ([\#6387](https://github.com/tidyverse/dplyr/issues/6387)).

- [`frame_data()`](https://tibble.tidyverse.org/reference/deprecated.html),
  [`data_frame_()`](https://tibble.tidyverse.org/reference/deprecated.html),
  [`lst_()`](https://tibble.tidyverse.org/reference/deprecated.html) and
  [`tbl_sum()`](https://pillar.r-lib.org/reference/tbl_sum.html) are no
  longer re-exported from tibble
  ([\#6276](https://github.com/tidyverse/dplyr/issues/6276),
  [\#6277](https://github.com/tidyverse/dplyr/issues/6277),
  [\#6278](https://github.com/tidyverse/dplyr/issues/6278),
  [\#6284](https://github.com/tidyverse/dplyr/issues/6284)).

- `select_vars()`, `rename_vars()`, `select_var()` and `current_vars()`,
  deprecated in 0.8.4, are now defunct
  ([\#6387](https://github.com/tidyverse/dplyr/issues/6387)).

#### Newly deprecated

- [`across()`](https://dplyr.tidyverse.org/dev/reference/across.md),
  [`c_across()`](https://dplyr.tidyverse.org/dev/reference/c_across.md),
  [`if_any()`](https://dplyr.tidyverse.org/dev/reference/across.md), and
  [`if_all()`](https://dplyr.tidyverse.org/dev/reference/across.md) now
  require the `.cols` and `.fns` arguments. In general, we now recommend
  that you use
  [`pick()`](https://dplyr.tidyverse.org/dev/reference/pick.md) instead
  of an empty
  [`across()`](https://dplyr.tidyverse.org/dev/reference/across.md) call
  or [`across()`](https://dplyr.tidyverse.org/dev/reference/across.md)
  with no `.fns` (e.g. `across(c(x, y))`.
  ([\#6523](https://github.com/tidyverse/dplyr/issues/6523)).

  - Relying on the previous default of `.cols = everything()` is
    deprecated. We have skipped the soft-deprecation stage in this case,
    because indirect usage of
    [`across()`](https://dplyr.tidyverse.org/dev/reference/across.md)
    and friends in this way is rare.

  - Relying on the previous default of `.fns = NULL` is not yet formally
    soft-deprecated, because there was no good alternative until now,
    but it is discouraged and will be soft-deprecated in the next minor
    release.

- Passing `...` to
  [`across()`](https://dplyr.tidyverse.org/dev/reference/across.md) is
  soft-deprecated because it’s ambiguous when those arguments are
  evaluated. Now, instead of (e.g.) `across(a:b, mean, na.rm = TRUE)`
  you should write `across(a:b, ~ mean(.x, na.rm = TRUE))`
  ([\#6073](https://github.com/tidyverse/dplyr/issues/6073)).

- [`all_equal()`](https://dplyr.tidyverse.org/dev/reference/all_equal.md)
  is deprecated. We’ve advised against it for some time, and we
  explicitly recommend you use
  [`all.equal()`](https://rdrr.io/r/base/all.equal.html), manually
  reordering the rows and columns as needed
  ([\#6324](https://github.com/tidyverse/dplyr/issues/6324)).

- [`cur_data()`](https://dplyr.tidyverse.org/dev/reference/deprec-context.md)
  and
  [`cur_data_all()`](https://dplyr.tidyverse.org/dev/reference/deprec-context.md)
  are soft-deprecated in favour of
  [`pick()`](https://dplyr.tidyverse.org/dev/reference/pick.md)
  ([\#6204](https://github.com/tidyverse/dplyr/issues/6204)).

- Using `by = character()` to perform a cross join is now
  soft-deprecated in favor of
  [`cross_join()`](https://dplyr.tidyverse.org/dev/reference/cross_join.md)
  ([\#6604](https://github.com/tidyverse/dplyr/issues/6604)).

- [`filter()`](https://dplyr.tidyverse.org/dev/reference/filter.md)ing
  with a 1-column matrix is deprecated
  ([\#6091](https://github.com/tidyverse/dplyr/issues/6091)).

- `progress_estimate()` is deprecated for all uses
  ([\#6387](https://github.com/tidyverse/dplyr/issues/6387)).

- Using
  [`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)
  to produce a 0 or \>1 row “summary” is deprecated in favor of the new
  [`reframe()`](https://dplyr.tidyverse.org/dev/reference/reframe.md).
  See the NEWS bullet about
  [`reframe()`](https://dplyr.tidyverse.org/dev/reference/reframe.md)
  for more details
  ([\#6382](https://github.com/tidyverse/dplyr/issues/6382)).

- All functions deprecated in 1.0.0 (released April 2020) and earlier
  now warn every time you use them
  ([\#6387](https://github.com/tidyverse/dplyr/issues/6387)). This
  includes
  [`combine()`](https://dplyr.tidyverse.org/dev/reference/defunct.md),
  [`src_local()`](https://dplyr.tidyverse.org/dev/reference/defunct.md),
  [`src_mysql()`](https://dplyr.tidyverse.org/dev/reference/defunct.md),
  [`src_postgres()`](https://dplyr.tidyverse.org/dev/reference/defunct.md),
  [`src_sqlite()`](https://dplyr.tidyverse.org/dev/reference/defunct.md),
  `rename_vars_()`, `select_vars_()`,
  [`summarise_each_()`](https://dplyr.tidyverse.org/dev/reference/defunct-each.md),
  [`mutate_each_()`](https://dplyr.tidyverse.org/dev/reference/defunct-each.md),
  [`as.tbl()`](https://dplyr.tidyverse.org/dev/reference/defunct.md),
  [`tbl_df()`](https://dplyr.tidyverse.org/dev/reference/defunct.md),
  and a handful of older arguments. They are likely to be made defunct
  in the next major version (but not before mid 2024).

- [`slice()`](https://dplyr.tidyverse.org/dev/reference/slice.md)ing
  with a 1-column matrix is deprecated.

#### Newly superseded

- [`recode()`](https://dplyr.tidyverse.org/dev/reference/recode.md) is
  superseded in favour of
  [`case_match()`](https://dplyr.tidyverse.org/dev/reference/case_match.md)
  ([\#6433](https://github.com/tidyverse/dplyr/issues/6433)).

- [`recode_factor()`](https://dplyr.tidyverse.org/dev/reference/recode.md)
  is superseded. We don’t have a direct replacement for it yet, but we
  plan to add one to forcats. In the meantime you can often use
  `case_match(.ptype = factor(levels = ))` instead
  ([\#6433](https://github.com/tidyverse/dplyr/issues/6433)).

- [`transmute()`](https://dplyr.tidyverse.org/dev/reference/transmute.md)
  is superseded in favour of `mutate(.keep = "none")`
  ([\#6414](https://github.com/tidyverse/dplyr/issues/6414)).

#### Newly stable

- The `.keep`, `.before`, and `.after` arguments to
  [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md) have
  moved from experimental to stable.

- The `rows_*()` family of functions have moved from experimental to
  stable.

### vctrs

Many of dplyr’s vector functions have been rewritten to make use of the
vctrs package, bringing greater consistency and improved performance.

- [`between()`](https://dplyr.tidyverse.org/dev/reference/between.md)
  can now work with all vector types, not just numeric and date-time.
  Additionally, `left` and `right` can now also be vectors (with the
  same length as `x`), and `x`, `left`, and `right` are cast to the
  common type before the comparison is made
  ([\#6183](https://github.com/tidyverse/dplyr/issues/6183),
  [\#6260](https://github.com/tidyverse/dplyr/issues/6260),
  [\#6478](https://github.com/tidyverse/dplyr/issues/6478)).

- [`case_when()`](https://dplyr.tidyverse.org/dev/reference/case-and-replace-when.md)
  ([\#5106](https://github.com/tidyverse/dplyr/issues/5106)):

  - Has a new `.default` argument that is intended to replace usage of
    `TRUE ~ default_value` as a more explicit and readable way to
    specify a default value. In the future, we will deprecate the unsafe
    recycling of the LHS inputs that allows `TRUE ~` to work, so we
    encourage you to switch to using `.default`.

  - No longer requires exact matching of the types of RHS values. For
    example, the following no longer requires you to use
    `NA_character_`.

        x <- c("little", "unknown", "small", "missing", "large")

        case_when(
          x %in% c("little", "small") ~ "one",
          x %in% c("big", "large") ~ "two",
          x %in% c("missing", "unknown") ~ NA
        )

  - Supports a larger variety of RHS value types. For example, you can
    use a data frame to create multiple columns at once.

  - Has new `.ptype` and `.size` arguments which allow you to enforce a
    particular output type and size.

  - Has a better error when types or lengths were incompatible
    ([\#6261](https://github.com/tidyverse/dplyr/issues/6261),
    [\#6206](https://github.com/tidyverse/dplyr/issues/6206)).

- [`coalesce()`](https://dplyr.tidyverse.org/dev/reference/coalesce.md)
  ([\#6265](https://github.com/tidyverse/dplyr/issues/6265)):

  - Discards `NULL` inputs up front.

  - No longer iterates over the columns of data frame input. Instead, a
    row is now only coalesced if it is entirely missing, which is
    consistent with
    [`vctrs::vec_detect_missing()`](https://vctrs.r-lib.org/reference/missing.html)
    and greatly simplifies the implementation.

  - Has new `.ptype` and `.size` arguments which allow you to enforce a
    particular output type and size.

- [`first()`](https://dplyr.tidyverse.org/dev/reference/nth.md),
  [`last()`](https://dplyr.tidyverse.org/dev/reference/nth.md), and
  [`nth()`](https://dplyr.tidyverse.org/dev/reference/nth.md)
  ([\#6331](https://github.com/tidyverse/dplyr/issues/6331)):

  - When used on a data frame, these functions now return a single row
    rather than a single column. This is more consistent with the vctrs
    principle that a data frame is generally treated as a vector of
    rows.

  - The `default` is no longer “guessed”, and will always automatically
    be set to a missing value appropriate for the type of `x`.

  - Error if `n` is not an integer. `nth(x, n = 2)` is fine, but
    `nth(x, n = 2.5)` is now an error.

  - No longer support indexing into scalar objects, like `<lm>` or
    scalar S4 objects
    ([\#6670](https://github.com/tidyverse/dplyr/issues/6670)).

  Additionally, they have all gained an `na_rm` argument since they are
  summary functions
  ([\#6242](https://github.com/tidyverse/dplyr/issues/6242), with
  contributions from [@tnederlof](https://github.com/tnederlof)).

- [`if_else()`](https://dplyr.tidyverse.org/dev/reference/if_else.md)
  gains most of the same benefits as
  [`case_when()`](https://dplyr.tidyverse.org/dev/reference/case-and-replace-when.md).
  In particular,
  [`if_else()`](https://dplyr.tidyverse.org/dev/reference/if_else.md)
  now takes the common type of `true`, `false`, and `missing` to
  determine the output type, meaning that you can now reliably use `NA`,
  rather than `NA_character_` and friends
  ([\#6243](https://github.com/tidyverse/dplyr/issues/6243)).

  [`if_else()`](https://dplyr.tidyverse.org/dev/reference/if_else.md)
  also no longer allows you to supply `NULL` for either `true` or
  `false`, which was an undocumented usage that we consider to be
  off-label, because `true` and `false` are intended to be (and
  documented to be) vector inputs
  ([\#6730](https://github.com/tidyverse/dplyr/issues/6730)).

- [`na_if()`](https://dplyr.tidyverse.org/dev/reference/na_if.md)
  ([\#6329](https://github.com/tidyverse/dplyr/issues/6329)) now casts
  `y` to the type of `x` before comparison, which makes it clearer that
  this function is type and size stable on `x`. In particular, this
  means that you can no longer do `na_if(<tibble>, 0)`, which previously
  accidentally allowed you to replace any instance of `0` across every
  column of the tibble with `NA`.
  [`na_if()`](https://dplyr.tidyverse.org/dev/reference/na_if.md) was
  never intended to work this way, and this is considered off-label
  usage.

  You can also now replace `NaN` values in `x` with `na_if(x, NaN)`.

- [`lag()`](https://dplyr.tidyverse.org/dev/reference/lead-lag.md) and
  [`lead()`](https://dplyr.tidyverse.org/dev/reference/lead-lag.md) now
  cast `default` to the type of `x`, rather than taking the common type.
  This ensures that these functions are type stable on `x`
  ([\#6330](https://github.com/tidyverse/dplyr/issues/6330)).

- [`row_number()`](https://dplyr.tidyverse.org/dev/reference/row_number.md),
  [`min_rank()`](https://dplyr.tidyverse.org/dev/reference/row_number.md),
  [`dense_rank()`](https://dplyr.tidyverse.org/dev/reference/row_number.md),
  [`ntile()`](https://dplyr.tidyverse.org/dev/reference/ntile.md),
  [`cume_dist()`](https://dplyr.tidyverse.org/dev/reference/percent_rank.md),
  and
  [`percent_rank()`](https://dplyr.tidyverse.org/dev/reference/percent_rank.md)
  are faster and work for more types. You can now rank by multiple
  columns by supplying a data frame
  ([\#6428](https://github.com/tidyverse/dplyr/issues/6428)).

- [`with_order()`](https://dplyr.tidyverse.org/dev/reference/with_order.md)
  now checks that the size of `order_by` is the same size as `x`, and
  now works correctly when `order_by` is a data frame
  ([\#6334](https://github.com/tidyverse/dplyr/issues/6334)).

### Minor improvements and bug fixes

- Fixed an issue with latest rlang that caused internal tools (such as
  `mask$eval_all_summarise()`) to be mentioned in error messages
  ([\#6308](https://github.com/tidyverse/dplyr/issues/6308)).

- Warnings are enriched with contextualised information in
  [`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)
  and [`filter()`](https://dplyr.tidyverse.org/dev/reference/filter.md)
  just like they have been in
  [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md) and
  [`arrange()`](https://dplyr.tidyverse.org/dev/reference/arrange.md).

- Joins now reference the correct column in `y` when a type error is
  thrown while joining on two columns with different names
  ([\#6465](https://github.com/tidyverse/dplyr/issues/6465)).

- Joins on very wide tables are no longer bottlenecked by the
  application of `suffix`
  ([\#6642](https://github.com/tidyverse/dplyr/issues/6642)).

- `*_join()` now error if you supply them with additional arguments that
  aren’t used
  ([\#6228](https://github.com/tidyverse/dplyr/issues/6228)).

- [`across()`](https://dplyr.tidyverse.org/dev/reference/across.md) used
  without functions inside a rowwise-data frame no longer generates an
  invalid data frame
  ([\#6264](https://github.com/tidyverse/dplyr/issues/6264)).

- Anonymous functions supplied with `function()` and `\()` are now
  inlined by
  [`across()`](https://dplyr.tidyverse.org/dev/reference/across.md) if
  possible, which slightly improves performance and makes possible
  further optimisations in the future.

- Functions supplied to
  [`across()`](https://dplyr.tidyverse.org/dev/reference/across.md) are
  no longer masked by columns
  ([\#6545](https://github.com/tidyverse/dplyr/issues/6545)). For
  instance, `across(1:2, mean)` will now work as expected even if there
  is a column called `mean`.

- [`across()`](https://dplyr.tidyverse.org/dev/reference/across.md) will
  now error when supplied `...` without a `.fns` argument
  ([\#6638](https://github.com/tidyverse/dplyr/issues/6638)).

- [`arrange()`](https://dplyr.tidyverse.org/dev/reference/arrange.md)
  now correctly ignores `NULL` inputs
  ([\#6193](https://github.com/tidyverse/dplyr/issues/6193)).

- [`arrange()`](https://dplyr.tidyverse.org/dev/reference/arrange.md)
  now works correctly when
  [`across()`](https://dplyr.tidyverse.org/dev/reference/across.md)
  calls are used as the 2nd (or more) ordering expression
  ([\#6495](https://github.com/tidyverse/dplyr/issues/6495)).

- `arrange(df, mydesc::desc(x))` works correctly when mydesc re-exports
  [`dplyr::desc()`](https://dplyr.tidyverse.org/dev/reference/desc.md)
  ([\#6231](https://github.com/tidyverse/dplyr/issues/6231)).

- [`c_across()`](https://dplyr.tidyverse.org/dev/reference/c_across.md)
  now evaluates
  [`all_of()`](https://tidyselect.r-lib.org/reference/all_of.html)
  correctly and no longer allows you to accidentally select grouping
  variables ([\#6522](https://github.com/tidyverse/dplyr/issues/6522)).

- [`c_across()`](https://dplyr.tidyverse.org/dev/reference/c_across.md)
  now throws a more informative error if you try to rename during column
  selection ([\#6522](https://github.com/tidyverse/dplyr/issues/6522)).

- dplyr no longer provides
  [`count()`](https://dplyr.tidyverse.org/dev/reference/count.md) and
  [`tally()`](https://dplyr.tidyverse.org/dev/reference/count.md)
  methods for `tbl_sql`. These methods have been accidentally overriding
  the `tbl_lazy` methods that dbplyr provides, which has resulted in
  issues with the grouping structure of the output
  ([\#6338](https://github.com/tidyverse/dplyr/issues/6338),
  tidyverse/dbplyr#940).

- [`cur_group()`](https://dplyr.tidyverse.org/dev/reference/context.md)
  now works correctly with zero row grouped data frames
  ([\#6304](https://github.com/tidyverse/dplyr/issues/6304)).

- [`desc()`](https://dplyr.tidyverse.org/dev/reference/desc.md) gives a
  useful error message if you give it a non-vector
  ([\#6028](https://github.com/tidyverse/dplyr/issues/6028)).

- [`distinct()`](https://dplyr.tidyverse.org/dev/reference/distinct.md)
  now retains attributes of bare data frames
  ([\#6318](https://github.com/tidyverse/dplyr/issues/6318)).

- [`distinct()`](https://dplyr.tidyverse.org/dev/reference/distinct.md)
  returns columns ordered the way you request, not the same as the input
  data ([\#6156](https://github.com/tidyverse/dplyr/issues/6156)).

- Error messages in
  [`group_by()`](https://dplyr.tidyverse.org/dev/reference/group_by.md),
  [`distinct()`](https://dplyr.tidyverse.org/dev/reference/distinct.md),
  [`tally()`](https://dplyr.tidyverse.org/dev/reference/count.md), and
  [`count()`](https://dplyr.tidyverse.org/dev/reference/count.md) are
  now more relevant
  ([\#6139](https://github.com/tidyverse/dplyr/issues/6139)).

- [`group_by_prepare()`](https://dplyr.tidyverse.org/dev/reference/group_by_prepare.md)
  loses the `caller_env` argument. It was rarely used and it is no
  longer needed
  ([\#6444](https://github.com/tidyverse/dplyr/issues/6444)).

- [`group_walk()`](https://dplyr.tidyverse.org/dev/reference/group_map.md)
  gains an explicit `.keep` argument
  ([\#6530](https://github.com/tidyverse/dplyr/issues/6530)).

- Warnings emitted inside
  [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md) and
  variants are now collected and stashed away. Run the new
  [`last_dplyr_warnings()`](https://dplyr.tidyverse.org/dev/reference/last_dplyr_warnings.md)
  function to see the warnings emitted within dplyr verbs during the
  last top-level command.

  This fixes performance issues when thousands of warnings are emitted
  with rowwise and grouped data frames
  ([\#6005](https://github.com/tidyverse/dplyr/issues/6005),
  [\#6236](https://github.com/tidyverse/dplyr/issues/6236)).

- [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md)
  behaves a little better with 0-row rowwise inputs
  ([\#6303](https://github.com/tidyverse/dplyr/issues/6303)).

- A rowwise
  [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md) now
  automatically unlists list-columns containing length 1 vectors
  ([\#6302](https://github.com/tidyverse/dplyr/issues/6302)).

- [`nest_join()`](https://dplyr.tidyverse.org/dev/reference/nest_join.md)
  has gained the `na_matches` argument that all other joins have.

- [`nest_join()`](https://dplyr.tidyverse.org/dev/reference/nest_join.md)
  now preserves the type of `y`
  ([\#6295](https://github.com/tidyverse/dplyr/issues/6295)).

- [`n_distinct()`](https://dplyr.tidyverse.org/dev/reference/n_distinct.md)
  now errors if you don’t give it any input
  ([\#6535](https://github.com/tidyverse/dplyr/issues/6535)).

- [`nth()`](https://dplyr.tidyverse.org/dev/reference/nth.md),
  [`first()`](https://dplyr.tidyverse.org/dev/reference/nth.md),
  [`last()`](https://dplyr.tidyverse.org/dev/reference/nth.md), and
  [`with_order()`](https://dplyr.tidyverse.org/dev/reference/with_order.md)
  now sort character `order_by` vectors in the C locale. Using character
  vectors for `order_by` is rare, so we expect this to have little
  practical impact
  ([\#6451](https://github.com/tidyverse/dplyr/issues/6451)).

- [`ntile()`](https://dplyr.tidyverse.org/dev/reference/ntile.md) now
  requires `n` to be a single positive integer.

- [`relocate()`](https://dplyr.tidyverse.org/dev/reference/relocate.md)
  now works correctly with empty data frames and when `.before` or
  `.after` result in empty selections
  ([\#6167](https://github.com/tidyverse/dplyr/issues/6167)).

- [`relocate()`](https://dplyr.tidyverse.org/dev/reference/relocate.md)
  no longer drops attributes of bare data frames
  ([\#6341](https://github.com/tidyverse/dplyr/issues/6341)).

- [`relocate()`](https://dplyr.tidyverse.org/dev/reference/relocate.md)
  now retains the last name change when a single column is renamed
  multiple times while it is being moved. This better matches the
  behavior of
  [`rename()`](https://dplyr.tidyverse.org/dev/reference/rename.md)
  ([\#6209](https://github.com/tidyverse/dplyr/issues/6209), with help
  from [@eutwt](https://github.com/eutwt)).

- [`rename()`](https://dplyr.tidyverse.org/dev/reference/rename.md) now
  contains examples of using
  [`all_of()`](https://tidyselect.r-lib.org/reference/all_of.html) and
  [`any_of()`](https://tidyselect.r-lib.org/reference/all_of.html) to
  rename using a named character vector
  ([\#6644](https://github.com/tidyverse/dplyr/issues/6644)).

- [`rename_with()`](https://dplyr.tidyverse.org/dev/reference/rename.md)
  now disallows renaming in the `.cols` tidy-selection
  ([\#6561](https://github.com/tidyverse/dplyr/issues/6561)).

- [`rename_with()`](https://dplyr.tidyverse.org/dev/reference/rename.md)
  now checks that the result of `.fn` is the right type and size
  ([\#6561](https://github.com/tidyverse/dplyr/issues/6561)).

- [`rows_insert()`](https://dplyr.tidyverse.org/dev/reference/rows.md)
  now checks that `y` contains the `by` columns
  ([\#6652](https://github.com/tidyverse/dplyr/issues/6652)).

- [`setequal()`](https://dplyr.tidyverse.org/dev/reference/setops.md)
  ignores differences between freely coercible types (e.g. integer and
  double) ([\#6114](https://github.com/tidyverse/dplyr/issues/6114)) and
  ignores duplicated rows
  ([\#6057](https://github.com/tidyverse/dplyr/issues/6057)).

- [`slice()`](https://dplyr.tidyverse.org/dev/reference/slice.md)
  helpers again produce output equivalent to `slice(.data, 0)` when the
  `n` or `prop` argument is 0, fixing a bug introduced in the previous
  version ([@eutwt](https://github.com/eutwt),
  [\#6184](https://github.com/tidyverse/dplyr/issues/6184)).

- [`slice()`](https://dplyr.tidyverse.org/dev/reference/slice.md) with
  no inputs now returns 0 rows. This is mostly for theoretical
  consistency
  ([\#6573](https://github.com/tidyverse/dplyr/issues/6573)).

- [`slice()`](https://dplyr.tidyverse.org/dev/reference/slice.md) now
  errors if any expressions in `...` are named. This helps avoid
  accidentally misspelling an optional argument, such as `.by`
  ([\#6554](https://github.com/tidyverse/dplyr/issues/6554)).

- `slice_*()` now requires `n` to be an integer.

- `slice_*()` generics now perform argument validation. This should make
  methods more consistent and simpler to implement
  ([\#6361](https://github.com/tidyverse/dplyr/issues/6361)).

- [`slice_min()`](https://dplyr.tidyverse.org/dev/reference/slice.md)
  and
  [`slice_max()`](https://dplyr.tidyverse.org/dev/reference/slice.md)
  can `order_by` multiple variables if you supply them as a data.frame
  or tibble ([\#6176](https://github.com/tidyverse/dplyr/issues/6176)).

- [`slice_min()`](https://dplyr.tidyverse.org/dev/reference/slice.md)
  and
  [`slice_max()`](https://dplyr.tidyverse.org/dev/reference/slice.md)
  now consistently include missing values in the result if necessary
  (i.e. there aren’t enough non-missing values to reach the `n` or
  `prop` you have selected). If you don’t want missing values to be
  included at all, set `na_rm = TRUE`
  ([\#6177](https://github.com/tidyverse/dplyr/issues/6177)).

- [`slice_sample()`](https://dplyr.tidyverse.org/dev/reference/slice.md)
  now accepts negative `n` and `prop` values
  ([\#6402](https://github.com/tidyverse/dplyr/issues/6402)).

- [`slice_sample()`](https://dplyr.tidyverse.org/dev/reference/slice.md)
  returns a data frame or group with the same number of rows as the
  input when `replace = FALSE` and `n` is larger than the number of rows
  or `prop` is larger than 1. This reverts a change made in 1.0.8,
  returning to the behavior of 1.0.7
  ([\#6185](https://github.com/tidyverse/dplyr/issues/6185))

- [`slice_sample()`](https://dplyr.tidyverse.org/dev/reference/slice.md)
  now gives a more informative error when `replace = FALSE` and the
  number of rows requested in the sample exceeds the number of rows in
  the data ([\#6271](https://github.com/tidyverse/dplyr/issues/6271)).

- `storms` has been updated to include 2021 data and some missing storms
  that were omitted due to an error
  ([@steveharoz](https://github.com/steveharoz),
  [\#6320](https://github.com/tidyverse/dplyr/issues/6320)).

- [`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)
  now correctly recycles named 0-column data frames
  ([\#6509](https://github.com/tidyverse/dplyr/issues/6509)).

- [`union_all()`](https://dplyr.tidyverse.org/dev/reference/setops.md),
  like [`union()`](https://dplyr.tidyverse.org/dev/reference/setops.md),
  now requires that data frames be compatible: i.e. they have the same
  columns, and the columns have compatible types.

- [`where()`](https://tidyselect.r-lib.org/reference/where.html) is
  re-exported from tidyselect
  ([\#6597](https://github.com/tidyverse/dplyr/issues/6597)).

## dplyr 1.0.10

CRAN release: 2022-09-01

Hot patch release to resolve R CMD check failures.

## dplyr 1.0.9

CRAN release: 2022-04-28

- New
  [`rows_append()`](https://dplyr.tidyverse.org/dev/reference/rows.md)
  which works like
  [`rows_insert()`](https://dplyr.tidyverse.org/dev/reference/rows.md)
  but ignores keys and allows you to insert arbitrary rows with a
  guarantee that the type of `x` won’t change
  ([\#6249](https://github.com/tidyverse/dplyr/issues/6249), thanks to
  [@krlmlr](https://github.com/krlmlr) for the implementation and
  [@mgirlich](https://github.com/mgirlich) for the idea).

- The `rows_*()` functions no longer require that the key values in `x`
  uniquely identify each row. Additionally,
  [`rows_insert()`](https://dplyr.tidyverse.org/dev/reference/rows.md)
  and
  [`rows_delete()`](https://dplyr.tidyverse.org/dev/reference/rows.md)
  no longer require that the key values in `y` uniquely identify each
  row. Relaxing this restriction should make these functions more
  practically useful for data frames, and alternative backends can
  enforce this in other ways as needed (i.e. through primary keys)
  ([\#5553](https://github.com/tidyverse/dplyr/issues/5553)).

- [`rows_insert()`](https://dplyr.tidyverse.org/dev/reference/rows.md)
  gained a new `conflict` argument allowing you greater control over
  rows in `y` with keys that conflict with keys in `x`. A conflict
  arises if a key in `y` already exists in `x`. By default, a conflict
  results in an error, but you can now also `"ignore"` these `y` rows.
  This is very similar to the `ON CONFLICT DO NOTHING` command from SQL
  ([\#5588](https://github.com/tidyverse/dplyr/issues/5588), with
  helpful additions from [@mgirlich](https://github.com/mgirlich) and
  [@krlmlr](https://github.com/krlmlr)).

- [`rows_update()`](https://dplyr.tidyverse.org/dev/reference/rows.md),
  [`rows_patch()`](https://dplyr.tidyverse.org/dev/reference/rows.md),
  and
  [`rows_delete()`](https://dplyr.tidyverse.org/dev/reference/rows.md)
  gained a new `unmatched` argument allowing you greater control over
  rows in `y` with keys that are unmatched by the keys in `x`. By
  default, an unmatched key results in an error, but you can now also
  `"ignore"` these `y` rows
  ([\#5984](https://github.com/tidyverse/dplyr/issues/5984),
  [\#5699](https://github.com/tidyverse/dplyr/issues/5699)).

- [`rows_delete()`](https://dplyr.tidyverse.org/dev/reference/rows.md)
  no longer requires that the columns of `y` be a strict subset of `x`.
  Only the columns specified through `by` will be utilized from `y`, all
  others will be dropped with a message.

- The `rows_*()` functions now always retain the column types of `x`.
  This behavior was documented, but previously wasn’t being applied
  correctly ([\#6240](https://github.com/tidyverse/dplyr/issues/6240)).

- The `rows_*()` functions now fail elegantly if `y` is a zero column
  data frame and `by` isn’t specified
  ([\#6179](https://github.com/tidyverse/dplyr/issues/6179)).

## dplyr 1.0.8

CRAN release: 2022-02-08

- Better display of error messages thanks to rlang 1.0.0.

- `mutate(.keep = "none")` is no longer identical to
  [`transmute()`](https://dplyr.tidyverse.org/dev/reference/transmute.md).
  [`transmute()`](https://dplyr.tidyverse.org/dev/reference/transmute.md)
  has not been changed, and completely ignores the column ordering of
  the existing data, instead relying on the ordering of expressions
  supplied through `...`. `mutate(.keep = "none")` has been changed to
  ensure that pre-existing columns are never moved, which aligns more
  closely with the other `.keep` options
  ([\#6086](https://github.com/tidyverse/dplyr/issues/6086)).

- [`filter()`](https://dplyr.tidyverse.org/dev/reference/filter.md)
  forbids matrix results
  ([\#5973](https://github.com/tidyverse/dplyr/issues/5973)) and warns
  about data frame results, especially data frames created from
  [`across()`](https://dplyr.tidyverse.org/dev/reference/across.md) with
  a hint to use
  [`if_any()`](https://dplyr.tidyverse.org/dev/reference/across.md) or
  [`if_all()`](https://dplyr.tidyverse.org/dev/reference/across.md).

- [`slice()`](https://dplyr.tidyverse.org/dev/reference/slice.md)
  helpers
  ([`slice_head()`](https://dplyr.tidyverse.org/dev/reference/slice.md),
  [`slice_tail()`](https://dplyr.tidyverse.org/dev/reference/slice.md),
  [`slice_min()`](https://dplyr.tidyverse.org/dev/reference/slice.md),
  [`slice_max()`](https://dplyr.tidyverse.org/dev/reference/slice.md))
  now accept negative values for `n` and `prop`
  ([\#5961](https://github.com/tidyverse/dplyr/issues/5961)).

- [`slice()`](https://dplyr.tidyverse.org/dev/reference/slice.md) now
  indicates which group produces an error
  ([\#5931](https://github.com/tidyverse/dplyr/issues/5931)).

- [`cur_data()`](https://dplyr.tidyverse.org/dev/reference/deprec-context.md)
  and
  [`cur_data_all()`](https://dplyr.tidyverse.org/dev/reference/deprec-context.md)
  don’t simplify list columns in rowwise data frames
  ([\#5901](https://github.com/tidyverse/dplyr/issues/5901)).

- dplyr now uses
  [`rlang::check_installed()`](https://rlang.r-lib.org/reference/is_installed.html)
  to prompt you whether to install required packages that are missing.

- `storms` data updated to 2020
  ([@steveharoz](https://github.com/steveharoz),
  [\#5899](https://github.com/tidyverse/dplyr/issues/5899)).

- [`coalesce()`](https://dplyr.tidyverse.org/dev/reference/coalesce.md)
  accepts 1-D arrays
  ([\#5557](https://github.com/tidyverse/dplyr/issues/5557)).

- The deprecated
  [`trunc_mat()`](https://tibble.tidyverse.org/reference/trunc_mat.html)
  is no longer reexported from dplyr
  ([\#6141](https://github.com/tidyverse/dplyr/issues/6141)).

## dplyr 1.0.7

CRAN release: 2021-06-18

- [`across()`](https://dplyr.tidyverse.org/dev/reference/across.md) uses
  the formula environment when inlining them
  ([\#5886](https://github.com/tidyverse/dplyr/issues/5886)).

- `summarise.rowwise_df()` is quiet when the result is ungrouped
  ([\#5875](https://github.com/tidyverse/dplyr/issues/5875)).

- [`c_across()`](https://dplyr.tidyverse.org/dev/reference/c_across.md)
  and [`across()`](https://dplyr.tidyverse.org/dev/reference/across.md)
  key deparsing not confused by long calls
  ([\#5883](https://github.com/tidyverse/dplyr/issues/5883)).

- [`across()`](https://dplyr.tidyverse.org/dev/reference/across.md)
  handles named selections
  ([\#5207](https://github.com/tidyverse/dplyr/issues/5207)).

## dplyr 1.0.6

CRAN release: 2021-05-05

- [`add_count()`](https://dplyr.tidyverse.org/dev/reference/count.md) is
  now generic
  ([\#5837](https://github.com/tidyverse/dplyr/issues/5837)).

- [`if_any()`](https://dplyr.tidyverse.org/dev/reference/across.md) and
  [`if_all()`](https://dplyr.tidyverse.org/dev/reference/across.md)
  abort when a predicate is mistakingly used as `.cols=`
  ([\#5732](https://github.com/tidyverse/dplyr/issues/5732)).

- Multiple calls to
  [`if_any()`](https://dplyr.tidyverse.org/dev/reference/across.md)
  and/or
  [`if_all()`](https://dplyr.tidyverse.org/dev/reference/across.md) in
  the same expression are now properly disambiguated
  ([\#5782](https://github.com/tidyverse/dplyr/issues/5782)).

- [`filter()`](https://dplyr.tidyverse.org/dev/reference/filter.md) now
  inlines
  [`if_any()`](https://dplyr.tidyverse.org/dev/reference/across.md) and
  [`if_all()`](https://dplyr.tidyverse.org/dev/reference/across.md)
  expressions. This greatly improves performance with grouped data
  frames.

- Fixed behaviour of `...` in top-level
  [`across()`](https://dplyr.tidyverse.org/dev/reference/across.md)
  calls ([\#5813](https://github.com/tidyverse/dplyr/issues/5813),
  [\#5832](https://github.com/tidyverse/dplyr/issues/5832)).

- [`across()`](https://dplyr.tidyverse.org/dev/reference/across.md) now
  inlines lambda-formulas. This is slightly more performant and will
  allow more optimisations in the future.

- Fixed issue in
  [`bind_rows()`](https://dplyr.tidyverse.org/dev/reference/bind_rows.md)
  causing lists to be incorrectly transformed as data frames
  ([\#5417](https://github.com/tidyverse/dplyr/issues/5417),
  [\#5749](https://github.com/tidyverse/dplyr/issues/5749)).

- [`select()`](https://dplyr.tidyverse.org/dev/reference/select.md) no
  longer creates duplicate variables when renaming a variable to the
  same name as a grouping variable
  ([\#5841](https://github.com/tidyverse/dplyr/issues/5841)).

- `dplyr_col_select()` keeps attributes for bare data frames
  ([\#5294](https://github.com/tidyverse/dplyr/issues/5294),
  [\#5831](https://github.com/tidyverse/dplyr/issues/5831)).

- Fixed quosure handling in
  [`dplyr::group_by()`](https://dplyr.tidyverse.org/dev/reference/group_by.md)
  that caused issues with extra arguments (tidyverse/lubridate#959).

- Removed the `name` argument from the
  [`compute()`](https://dplyr.tidyverse.org/dev/reference/compute.md)
  generic ([@ianmcook](https://github.com/ianmcook),
  [\#5783](https://github.com/tidyverse/dplyr/issues/5783)).

- row-wise data frames of 0 rows and list columns are supported again
  ([\#5804](https://github.com/tidyverse/dplyr/issues/5804)).

## dplyr 1.0.5

CRAN release: 2021-03-05

- Fixed edge case of
  [`slice_sample()`](https://dplyr.tidyverse.org/dev/reference/slice.md)
  when `weight_by=` is used and there 0 rows
  ([\#5729](https://github.com/tidyverse/dplyr/issues/5729)).

- [`across()`](https://dplyr.tidyverse.org/dev/reference/across.md) can
  again use columns in functions defined inline
  ([\#5734](https://github.com/tidyverse/dplyr/issues/5734)).

- Using testthat 3rd edition.

- Fixed bugs introduced in
  [`across()`](https://dplyr.tidyverse.org/dev/reference/across.md) in
  previous version
  ([\#5765](https://github.com/tidyverse/dplyr/issues/5765)).

- [`group_by()`](https://dplyr.tidyverse.org/dev/reference/group_by.md)
  keeps attributes unrelated to the grouping
  ([\#5760](https://github.com/tidyverse/dplyr/issues/5760)).

- The `.cols=` argument of
  [`if_any()`](https://dplyr.tidyverse.org/dev/reference/across.md) and
  [`if_all()`](https://dplyr.tidyverse.org/dev/reference/across.md)
  defaults to
  [`everything()`](https://tidyselect.r-lib.org/reference/everything.html).

## dplyr 1.0.4

CRAN release: 2021-02-02

- Improved performance for
  [`across()`](https://dplyr.tidyverse.org/dev/reference/across.md).
  This makes `summarise(across())` and `mutate(across())` perform as
  well as the superseded colwise equivalents
  ([\#5697](https://github.com/tidyverse/dplyr/issues/5697)).

- New functions
  [`if_any()`](https://dplyr.tidyverse.org/dev/reference/across.md) and
  [`if_all()`](https://dplyr.tidyverse.org/dev/reference/across.md)
  ([\#4770](https://github.com/tidyverse/dplyr/issues/4770),
  [\#5713](https://github.com/tidyverse/dplyr/issues/5713)).

- [`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)
  silently ignores NULL results
  ([\#5708](https://github.com/tidyverse/dplyr/issues/5708)).

- Fixed a performance regression in
  [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md) when
  warnings occur once per group
  ([\#5675](https://github.com/tidyverse/dplyr/issues/5675)). We no
  longer instrument warnings with debugging information when
  [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md) is
  called within
  [`suppressWarnings()`](https://rdrr.io/r/base/warning.html).

## dplyr 1.0.3

CRAN release: 2021-01-15

- [`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)
  no longer informs when the result is ungrouped
  ([\#5633](https://github.com/tidyverse/dplyr/issues/5633)).

- `group_by(.drop = FALSE)` preserves ordered factors
  ([@brianrice2](https://github.com/brianrice2),
  [\#5545](https://github.com/tidyverse/dplyr/issues/5545)).

- [`count()`](https://dplyr.tidyverse.org/dev/reference/count.md) and
  [`tally()`](https://dplyr.tidyverse.org/dev/reference/count.md) are
  now generic.

- Removed default fallbacks to lazyeval methods; this will yield better
  error messages when you call a dplyr function with the wrong input,
  and is part of our long term plan to remove the deprecated lazyeval
  interface.

- [`inner_join()`](https://dplyr.tidyverse.org/dev/reference/mutate-joins.md)
  gains a `keep` parameter for consistency with the other mutating joins
  ([@patrickbarks](https://github.com/patrickbarks),
  [\#5581](https://github.com/tidyverse/dplyr/issues/5581)).

- Improved performance with many columns, with a dynamic data mask using
  active bindings and lazy chops
  ([\#5017](https://github.com/tidyverse/dplyr/issues/5017)).

- [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md) and
  friends preserves row names in data frames once more
  ([\#5418](https://github.com/tidyverse/dplyr/issues/5418)).

- [`group_by()`](https://dplyr.tidyverse.org/dev/reference/group_by.md)
  uses the ungrouped data for the implicit mutate step
  ([\#5598](https://github.com/tidyverse/dplyr/issues/5598)). You might
  have to define an
  [`ungroup()`](https://dplyr.tidyverse.org/dev/reference/group_by.md)
  method for custom classes. For example, see
  <https://github.com/hadley/cubelyr/pull/3>.

- [`relocate()`](https://dplyr.tidyverse.org/dev/reference/relocate.md)
  can rename columns it relocates
  ([\#5569](https://github.com/tidyverse/dplyr/issues/5569)).

- [`distinct()`](https://dplyr.tidyverse.org/dev/reference/distinct.md)
  and
  [`group_by()`](https://dplyr.tidyverse.org/dev/reference/group_by.md)
  have better error messages when the mutate step fails
  ([\#5060](https://github.com/tidyverse/dplyr/issues/5060)).

- Clarify that
  [`between()`](https://dplyr.tidyverse.org/dev/reference/between.md) is
  not vectorised
  ([\#5493](https://github.com/tidyverse/dplyr/issues/5493)).

- Fixed
  [`across()`](https://dplyr.tidyverse.org/dev/reference/across.md)
  issue where data frame columns would could not be referred to with
  [`all_of()`](https://tidyselect.r-lib.org/reference/all_of.html) in
  the nested case
  ([`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md)
  within
  [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md))
  ([\#5498](https://github.com/tidyverse/dplyr/issues/5498)).

- [`across()`](https://dplyr.tidyverse.org/dev/reference/across.md)
  handles data frames with 0 columns
  ([\#5523](https://github.com/tidyverse/dplyr/issues/5523)).

- [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md)
  always keeps grouping variables, unconditional to `.keep=`
  ([\#5582](https://github.com/tidyverse/dplyr/issues/5582)).

- dplyr now depends on R 3.3.0

## dplyr 1.0.2

CRAN release: 2020-08-18

- Fixed
  [`across()`](https://dplyr.tidyverse.org/dev/reference/across.md)
  issue where data frame columns would mask objects referred to from
  [`all_of()`](https://tidyselect.r-lib.org/reference/all_of.html)
  ([\#5460](https://github.com/tidyverse/dplyr/issues/5460)).

- [`bind_cols()`](https://dplyr.tidyverse.org/dev/reference/bind_cols.md)
  gains a `.name_repair` argument, passed to
  [`vctrs::vec_cbind()`](https://vctrs.r-lib.org/reference/vec_bind.html)
  ([\#5451](https://github.com/tidyverse/dplyr/issues/5451))

- `summarise(.groups = "rowwise")` makes a rowwise data frame even if
  the input data is not grouped
  ([\#5422](https://github.com/tidyverse/dplyr/issues/5422)).

## dplyr 1.0.1

CRAN release: 2020-07-31

- New function
  [`cur_data_all()`](https://dplyr.tidyverse.org/dev/reference/deprec-context.md)
  similar to
  [`cur_data()`](https://dplyr.tidyverse.org/dev/reference/deprec-context.md)
  but includes the grouping variables
  ([\#5342](https://github.com/tidyverse/dplyr/issues/5342)).

- [`count()`](https://dplyr.tidyverse.org/dev/reference/count.md) and
  [`tally()`](https://dplyr.tidyverse.org/dev/reference/count.md) no
  longer automatically weights by column `n` if present
  ([\#5298](https://github.com/tidyverse/dplyr/issues/5298)). dplyr
  1.0.0 introduced this behaviour because of Hadley’s faulty memory.
  Historically
  [`tally()`](https://dplyr.tidyverse.org/dev/reference/count.md)
  automatically weighted and
  [`count()`](https://dplyr.tidyverse.org/dev/reference/count.md) did
  not, but this behaviour was accidentally changed in 0.8.2
  ([\#4408](https://github.com/tidyverse/dplyr/issues/4408)) so that
  neither automatically weighted by `n`. Since 0.8.2 is almost a year
  old, and the automatically weighting behaviour was a little confusing
  anyway, we’ve removed it from both
  [`count()`](https://dplyr.tidyverse.org/dev/reference/count.md) and
  [`tally()`](https://dplyr.tidyverse.org/dev/reference/count.md).

  Use of `wt = n()` is now deprecated; now just omit the `wt` argument.

- [`coalesce()`](https://dplyr.tidyverse.org/dev/reference/coalesce.md)
  now supports data frames correctly
  ([\#5326](https://github.com/tidyverse/dplyr/issues/5326)).

- [`cummean()`](https://dplyr.tidyverse.org/dev/reference/cumall.md) no
  longer has off-by-one indexing problem
  ([@cropgen](https://github.com/cropgen),
  [\#5287](https://github.com/tidyverse/dplyr/issues/5287)).

- The call stack is preserved on error. This makes it possible to
  [`recover()`](https://rdrr.io/r/utils/recover.html) into problematic
  code called from dplyr verbs
  ([\#5308](https://github.com/tidyverse/dplyr/issues/5308)).

## dplyr 1.0.0

CRAN release: 2020-05-29

### Breaking changes

- [`bind_cols()`](https://dplyr.tidyverse.org/dev/reference/bind_cols.md)
  no longer converts to a tibble, returns a data frame if the input is a
  data frame.

- [`bind_rows()`](https://dplyr.tidyverse.org/dev/reference/bind_rows.md),
  `*_join()`,
  [`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)
  and [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md)
  use vctrs coercion rules. There are two main user facing changes:

  - Combining factor and character vectors silently creates a character
    vector; previously it created a character vector with a warning.

  - Combining multiple factors creates a factor with combined levels;
    previously it created a character vector with a warning.

- [`bind_rows()`](https://dplyr.tidyverse.org/dev/reference/bind_rows.md)
  and other functions use vctrs name repair, see
  [`?vctrs::vec_as_names`](https://vctrs.r-lib.org/reference/vec_as_names.html).

- `all.equal.tbl_df()` removed.

  - Data frames, tibbles and grouped data frames are no longer
    considered equal, even if the data is the same.

  - Equality checks for data frames no longer ignore row order or
    groupings.

  - `expect_equal()` uses
    [`all.equal()`](https://rdrr.io/r/base/all.equal.html) internally.
    When comparing data frames, tests that used to pass may now fail.

- [`distinct()`](https://dplyr.tidyverse.org/dev/reference/distinct.md)
  keeps the original column order.

- [`distinct()`](https://dplyr.tidyverse.org/dev/reference/distinct.md)
  on missing columns now raises an error, it has been a compatibility
  warning for a long time.

- [`group_modify()`](https://dplyr.tidyverse.org/dev/reference/group_map.md)
  puts the grouping variable to the front.

- [`n()`](https://dplyr.tidyverse.org/dev/reference/context.md) and
  [`row_number()`](https://dplyr.tidyverse.org/dev/reference/row_number.md)
  can no longer be called directly when dplyr is not loaded, and this
  now generates an error: `dplyr::mutate(mtcars, x = n())`.

  Fix by prefixing with `dplyr::` as in
  `dplyr::mutate(mtcars, x = dplyr::n())`

- The old data format for `grouped_df` is no longer supported. This may
  affect you if you have serialized grouped data frames to disk,
  e.g. with [`saveRDS()`](https://rdrr.io/r/base/readRDS.html) or when
  using knitr caching.

- [`lead()`](https://dplyr.tidyverse.org/dev/reference/lead-lag.md) and
  [`lag()`](https://dplyr.tidyverse.org/dev/reference/lead-lag.md) are
  stricter about their inputs.

- Extending data frames requires that the extra class or classes are
  added first, not last. Having the extra class at the end causes some
  vctrs operations to fail with a message like:

      Input must be a vector, not a `<data.frame/...>` object

- [`right_join()`](https://dplyr.tidyverse.org/dev/reference/mutate-joins.md)
  no longer sorts the rows of the resulting tibble according to the
  order of the RHS `by` argument in tibble `y`.

### New features

- The `cur_` functions
  ([`cur_data()`](https://dplyr.tidyverse.org/dev/reference/deprec-context.md),
  [`cur_group()`](https://dplyr.tidyverse.org/dev/reference/context.md),
  [`cur_group_id()`](https://dplyr.tidyverse.org/dev/reference/context.md),
  [`cur_group_rows()`](https://dplyr.tidyverse.org/dev/reference/context.md))
  provide a full set of options to you access information about the
  “current” group in dplyr verbs. They are inspired by data.table’s
  `.SD`, `.GRP`, `.BY`, and `.I`.

- The `rows_` functions
  ([`rows_insert()`](https://dplyr.tidyverse.org/dev/reference/rows.md),
  [`rows_update()`](https://dplyr.tidyverse.org/dev/reference/rows.md),
  [`rows_upsert()`](https://dplyr.tidyverse.org/dev/reference/rows.md),
  [`rows_patch()`](https://dplyr.tidyverse.org/dev/reference/rows.md),
  [`rows_delete()`](https://dplyr.tidyverse.org/dev/reference/rows.md))
  provide a new API to insert and delete rows from a second data frame
  or table. Support for updating mutable backends is planned
  ([\#4654](https://github.com/tidyverse/dplyr/issues/4654)).

- [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md) and
  [`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)
  create multiple columns from a single expression if you return a data
  frame ([\#2326](https://github.com/tidyverse/dplyr/issues/2326)).

- [`select()`](https://dplyr.tidyverse.org/dev/reference/select.md) and
  [`rename()`](https://dplyr.tidyverse.org/dev/reference/rename.md) use
  the latest version of the tidyselect interface. Practically, this
  means that you can now combine selections using Boolean logic
  (i.e. `!`, `&` and `|`), and use predicate functions with
  [`where()`](https://tidyselect.r-lib.org/reference/where.html)
  (e.g. `where(is.character)`) to select variables by type
  ([\#4680](https://github.com/tidyverse/dplyr/issues/4680)). It also
  makes it possible to use
  [`select()`](https://dplyr.tidyverse.org/dev/reference/select.md) and
  [`rename()`](https://dplyr.tidyverse.org/dev/reference/rename.md) to
  repair data frames with duplicated names
  ([\#4615](https://github.com/tidyverse/dplyr/issues/4615)) and
  prevents you from accidentally introducing duplicate names
  ([\#4643](https://github.com/tidyverse/dplyr/issues/4643)). This also
  means that dplyr now re-exports
  [`any_of()`](https://tidyselect.r-lib.org/reference/all_of.html) and
  [`all_of()`](https://tidyselect.r-lib.org/reference/all_of.html)
  ([\#5036](https://github.com/tidyverse/dplyr/issues/5036)).

- [`slice()`](https://dplyr.tidyverse.org/dev/reference/slice.md) gains
  a new set of helpers:

  - [`slice_head()`](https://dplyr.tidyverse.org/dev/reference/slice.md)
    and
    [`slice_tail()`](https://dplyr.tidyverse.org/dev/reference/slice.md)
    select the first and last rows, like
    [`head()`](https://rdrr.io/r/utils/head.html) and
    [`tail()`](https://rdrr.io/r/utils/head.html), but return `n` rows
    *per group*.

  - [`slice_sample()`](https://dplyr.tidyverse.org/dev/reference/slice.md)
    randomly selects rows, taking over from
    [`sample_frac()`](https://dplyr.tidyverse.org/dev/reference/sample_n.md)
    and
    [`sample_n()`](https://dplyr.tidyverse.org/dev/reference/sample_n.md).

  - [`slice_min()`](https://dplyr.tidyverse.org/dev/reference/slice.md)
    and
    [`slice_max()`](https://dplyr.tidyverse.org/dev/reference/slice.md)
    select the rows with the minimum or maximum values of a variable,
    taking over from the confusing
    [`top_n()`](https://dplyr.tidyverse.org/dev/reference/top_n.md).

- [`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)
  can create summaries of greater than length 1 if you use a summary
  function that returns multiple values.

- [`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)
  gains a `.groups=` argument to control the grouping structure.

- New
  [`relocate()`](https://dplyr.tidyverse.org/dev/reference/relocate.md)
  verb makes it easy to move columns around within a data frame
  ([\#4598](https://github.com/tidyverse/dplyr/issues/4598)).

- New
  [`rename_with()`](https://dplyr.tidyverse.org/dev/reference/rename.md)
  is designed specifically for the purpose of renaming selected columns
  with a function
  ([\#4771](https://github.com/tidyverse/dplyr/issues/4771)).

- [`ungroup()`](https://dplyr.tidyverse.org/dev/reference/group_by.md)
  can now selectively remove grouping variables
  ([\#3760](https://github.com/tidyverse/dplyr/issues/3760)).

- [`pull()`](https://dplyr.tidyverse.org/dev/reference/pull.md) can now
  return named vectors by specifying an additional column name
  ([@ilarischeinin](https://github.com/ilarischeinin),
  [\#4102](https://github.com/tidyverse/dplyr/issues/4102)).

### Experimental features

- [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md) (for
  data frames only), gains experimental new arguments `.before` and
  `.after` that allow you to control where the new columns are placed
  ([\#2047](https://github.com/tidyverse/dplyr/issues/2047)).

- [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md) (for
  data frames only), gains an experimental new argument called `.keep`
  that allows you to control which variables are kept from the input
  `.data`. `.keep = "all"` is the default; it keeps all variables.
  `.keep = "none"` retains no input variables (except for grouping
  keys), so behaves like
  [`transmute()`](https://dplyr.tidyverse.org/dev/reference/transmute.md).
  `.keep = "unused"` keeps only variables not used to make new columns.
  `.keep = "used"` keeps only the input variables used to create new
  columns; it’s useful for double checking your work
  ([\#3721](https://github.com/tidyverse/dplyr/issues/3721)).

- New, experimental,
  [`with_groups()`](https://dplyr.tidyverse.org/dev/reference/with_groups.md)
  makes it easy to temporarily group or ungroup
  ([\#4711](https://github.com/tidyverse/dplyr/issues/4711)).

### across()

- New function
  [`across()`](https://dplyr.tidyverse.org/dev/reference/across.md) that
  can be used inside
  [`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md),
  [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md), and
  other verbs to apply a function (or a set of functions) to a selection
  of columns. See
  [`vignette("colwise")`](https://dplyr.tidyverse.org/dev/articles/colwise.md)
  for more details.

- New function
  [`c_across()`](https://dplyr.tidyverse.org/dev/reference/c_across.md)
  that can be used inside
  [`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)
  and [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md)
  in row-wise data frames to easily (e.g.) compute a row-wise mean of
  all numeric variables. See
  [`vignette("rowwise")`](https://dplyr.tidyverse.org/dev/articles/rowwise.md)
  for more details.

### rowwise()

- [`rowwise()`](https://dplyr.tidyverse.org/dev/reference/rowwise.md) is
  no longer questioning; we now understand that it’s an important tool
  when you don’t have vectorised code. It now also allows you to specify
  additional variables that should be preserved in the output when
  summarising
  ([\#4723](https://github.com/tidyverse/dplyr/issues/4723)). The
  rowwise-ness is preserved by all operations; you need to explicit drop
  it with
  [`as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)
  or
  [`group_by()`](https://dplyr.tidyverse.org/dev/reference/group_by.md).

- New, experimental,
  [`nest_by()`](https://dplyr.tidyverse.org/dev/reference/nest_by.md).
  It has the same interface as
  [`group_by()`](https://dplyr.tidyverse.org/dev/reference/group_by.md),
  but returns a rowwise data frame of grouping keys, supplemental with a
  list-column of data frames containing the rest of the data.

### vctrs

- The implementation of all dplyr verbs have been changed to use
  primitives provided by the vctrs package. This makes it easier to add
  support for new types of vector, radically simplifies the
  implementation, and makes all dplyr verbs more consistent.

- The place where you are mostly likely to be impacted by the coercion
  changes is when working with factors in joins or grouped mutates: now
  when combining factors with different levels, dplyr creates a new
  factor with the union of the levels. This matches base R more closely,
  and while perhaps strictly less correct, is much more convenient.

- dplyr dropped its two heaviest dependencies: Rcpp and BH. This should
  make it considerably easier and faster to build from source.

- The implementation of all verbs has been carefully thought through.
  This mostly makes implementation simpler but should hopefully increase
  consistency, and also makes it easier to adapt to dplyr to new data
  structures in the new future. Pragmatically, the biggest difference
  for most people will be that each verb documents its return value in
  terms of rows, columns, groups, and data frame attributes.

- Row names are now preserved when working with data frames.

### Grouping

- [`group_by()`](https://dplyr.tidyverse.org/dev/reference/group_by.md)
  uses hashing from the `vctrs` package.

- Grouped data frames now have `names<-`, `[[<-`, `[<-` and `$<-`
  methods that re-generate the underlying grouping. Note that modifying
  grouping variables in multiple steps
  (i.e. `df$grp1 <- 1; df$grp2 <- 1`) will be inefficient since the data
  frame will be regrouped after each modification.

- `[.grouped_df` now regroups to respect any grouping columns that have
  been removed
  ([\#4708](https://github.com/tidyverse/dplyr/issues/4708)).

- [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md) and
  [`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)
  can now modify grouping variables
  ([\#4709](https://github.com/tidyverse/dplyr/issues/4709)).

- [`group_modify()`](https://dplyr.tidyverse.org/dev/reference/group_map.md)
  works with additional arguments
  ([@billdenney](https://github.com/billdenney) and
  [@cderv](https://github.com/cderv),
  [\#4509](https://github.com/tidyverse/dplyr/issues/4509))

- [`group_by()`](https://dplyr.tidyverse.org/dev/reference/group_by.md)
  does not create an arbitrary NA group when grouping by factors with
  `drop = TRUE`
  ([\#4460](https://github.com/tidyverse/dplyr/issues/4460)).

### Lifecycle changes

- All deprecations now use the [lifecycle](https://lifecycle.r-lib.org),
  that means by default you’ll only see a deprecation warning once per
  session, and you can control with `options(lifecycle_verbosity = x)`
  where `x` is one of NULL, “quiet”, “warning”, and “error”.

#### Removed

- `id()`, deprecated in dplyr 0.5.0, is now defunct.

- `failwith()`, deprecated in dplyr 0.7.0, is now defunct.

- `tbl_cube()` and `nasa` have been pulled out into a separate cubelyr
  package ([\#4429](https://github.com/tidyverse/dplyr/issues/4429)).

- `rbind_all()` and `rbind_list()` have been removed
  ([@bjungbogati](https://github.com/bjungbogati),
  [\#4430](https://github.com/tidyverse/dplyr/issues/4430)).

- `dr_dplyr()` has been removed as it is no longer needed
  ([\#4433](https://github.com/tidyverse/dplyr/issues/4433),
  [@smwindecker](https://github.com/smwindecker)).

#### Deprecated

- Use of pkgconfig for setting `na_matches` argument to join functions
  is now deprecated
  ([\#4914](https://github.com/tidyverse/dplyr/issues/4914)). This was
  rarely used, and I’m now confident that the default is correct for R.

- In
  [`add_count()`](https://dplyr.tidyverse.org/dev/reference/count.md),
  the `drop` argument has been deprecated because it didn’t actually
  affect the output.

- [`add_rownames()`](https://dplyr.tidyverse.org/dev/reference/defunct.md):
  please use
  [`tibble::rownames_to_column()`](https://tibble.tidyverse.org/reference/rownames.html)
  instead.

- [`as.tbl()`](https://dplyr.tidyverse.org/dev/reference/defunct.md) and
  [`tbl_df()`](https://dplyr.tidyverse.org/dev/reference/defunct.md):
  please use
  [`as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)
  instead.

- `bench_tbls()`, `compare_tbls()`, `compare_tbls2()`, `eval_tbls()` and
  `eval_tbls2()` are now deprecated. That were only used in a handful of
  packages, and we now believe that you’re better off performing
  comparisons more directly
  ([\#4675](https://github.com/tidyverse/dplyr/issues/4675)).

- [`combine()`](https://dplyr.tidyverse.org/dev/reference/defunct.md):
  please use
  [`vctrs::vec_c()`](https://vctrs.r-lib.org/reference/vec_c.html)
  instead.

- [`funs()`](https://dplyr.tidyverse.org/dev/reference/funs.md): please
  use [`list()`](https://rdrr.io/r/base/list.html) instead.

- `group_by(add = )`: please use `.add` instead.

- `group_by(.dots = )`/`group_by_prepare(.dots = )`: please use `!!!`
  instead ([\#4734](https://github.com/tidyverse/dplyr/issues/4734)).

- The use of zero-arg
  [`group_indices()`](https://dplyr.tidyverse.org/dev/reference/group_data.md)
  to retrieve the group id for the “current” group is deprecated;
  instead use
  [`cur_group_id()`](https://dplyr.tidyverse.org/dev/reference/context.md).

- Passing arguments to
  [`group_keys()`](https://dplyr.tidyverse.org/dev/reference/group_data.md)
  or
  [`group_indices()`](https://dplyr.tidyverse.org/dev/reference/group_data.md)
  to change the grouping has been deprecated, instead do grouping first
  yourself.

- `location()` and `changes()`: please use
  [`lobstr::ref()`](https://lobstr.r-lib.org/reference/ref.html)
  instead.

- [`progress_estimated()`](https://dplyr.tidyverse.org/dev/reference/progress_estimated.md)
  is soft deprecated; it’s not the responsibility of dplyr to provide
  progress bars
  ([\#4935](https://github.com/tidyverse/dplyr/issues/4935)).

- [`src_local()`](https://dplyr.tidyverse.org/dev/reference/defunct.md)
  has been deprecated; it was part of an approach to testing dplyr
  backends that didn’t pan out.

- [`src_mysql()`](https://dplyr.tidyverse.org/dev/reference/defunct.md),
  [`src_postgres()`](https://dplyr.tidyverse.org/dev/reference/defunct.md),
  and
  [`src_sqlite()`](https://dplyr.tidyverse.org/dev/reference/defunct.md)
  has been deprecated. We’ve recommended against them for some time.
  Instead please use the approach described at
  <https://dbplyr.tidyverse.org/>.

- `select_vars()`, `rename_vars()`, `select_var()`, `current_vars()` are
  now deprecated ([@perezp44](https://github.com/perezp44),
  [\#4432](https://github.com/tidyverse/dplyr/issues/4432))

#### Superseded

- The scoped helpers (all functions ending in `_if`, `_at`, or `_all`)
  have been superseded by
  [`across()`](https://dplyr.tidyverse.org/dev/reference/across.md).
  This dramatically reduces the API surface for dplyr, while at the same
  providing providing a more flexible and less error-prone interface
  ([\#4769](https://github.com/tidyverse/dplyr/issues/4769)).

  `rename_*()` and `select_*()` have been superseded by
  [`rename_with()`](https://dplyr.tidyverse.org/dev/reference/rename.md).

- [`do()`](https://dplyr.tidyverse.org/dev/reference/do.md) is
  superseded in favour of
  [`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md).

- [`sample_n()`](https://dplyr.tidyverse.org/dev/reference/sample_n.md)
  and
  [`sample_frac()`](https://dplyr.tidyverse.org/dev/reference/sample_n.md)
  have been superseded by
  [`slice_sample()`](https://dplyr.tidyverse.org/dev/reference/slice.md).
  See
  [`?sample_n`](https://dplyr.tidyverse.org/dev/reference/sample_n.md)
  for details about why, and for examples converting from old to new
  usage.

- [`top_n()`](https://dplyr.tidyverse.org/dev/reference/top_n.md) has
  been superseded
  by[`slice_min()`](https://dplyr.tidyverse.org/dev/reference/slice.md)/[`slice_max()`](https://dplyr.tidyverse.org/dev/reference/slice.md).
  See [`?top_n`](https://dplyr.tidyverse.org/dev/reference/top_n.md) for
  details about why, and how to convert old to new usage
  ([\#4494](https://github.com/tidyverse/dplyr/issues/4494)).

#### Questioning

- [`all_equal()`](https://dplyr.tidyverse.org/dev/reference/all_equal.md)
  is questioning; it solves a problem that no longer seems important.

#### Stable

- [`rowwise()`](https://dplyr.tidyverse.org/dev/reference/rowwise.md) is
  no longer questioning.

### Documentation improvements

- New
  [`vignette("base")`](https://dplyr.tidyverse.org/dev/articles/base.md)
  which describes how dplyr verbs relate to the base R equivalents
  ([@sastoudt](https://github.com/sastoudt),
  [\#4755](https://github.com/tidyverse/dplyr/issues/4755))

- New
  [`vignette("grouping")`](https://dplyr.tidyverse.org/dev/articles/grouping.md)
  gives more details about how dplyr verbs change when applied to
  grouped data frames
  ([\#4779](https://github.com/tidyverse/dplyr/issues/4779),
  [@MikeKSmith](https://github.com/MikeKSmith)).

- [`vignette("programming")`](https://dplyr.tidyverse.org/dev/articles/programming.md)
  has been completely rewritten to reflect our latest vocabulary, the
  most recent rlang features, and our current recommendations. It should
  now be substantially easier to program with dplyr.

### Minor improvements and bug fixes

- dplyr now has a rudimentary, experimental, and stop-gap, extension
  mechanism documented in
  [`?dplyr_extending`](https://dplyr.tidyverse.org/dev/reference/dplyr_extending.md)

- dplyr no longer provides a `all.equal.tbl_df()` method. It never
  should have done so in the first place because it owns neither the
  generic nor the class. It also provided a problematic implementation
  because, by default, it ignored the order of the rows and the columns
  which is usually important. This is likely to cause new test failures
  in downstream packages; but on the whole we believe those failures to
  either reflect unexpected behaviour or tests that need to be
  strengthened
  ([\#2751](https://github.com/tidyverse/dplyr/issues/2751)).

- [`coalesce()`](https://dplyr.tidyverse.org/dev/reference/coalesce.md)
  now uses vctrs recycling and common type coercion rules
  ([\#5186](https://github.com/tidyverse/dplyr/issues/5186)).

- [`count()`](https://dplyr.tidyverse.org/dev/reference/count.md) and
  [`add_count()`](https://dplyr.tidyverse.org/dev/reference/count.md) do
  a better job of preserving input class and attributes
  ([\#4086](https://github.com/tidyverse/dplyr/issues/4086)).

- [`distinct()`](https://dplyr.tidyverse.org/dev/reference/distinct.md)
  errors if you request it use variables that don’t exist (this was
  previously a warning)
  ([\#4656](https://github.com/tidyverse/dplyr/issues/4656)).

- [`filter()`](https://dplyr.tidyverse.org/dev/reference/filter.md),
  [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md) and
  [`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)
  get better error messages.

- [`filter()`](https://dplyr.tidyverse.org/dev/reference/filter.md)
  handles data frame results when all columns are logical vectors by
  reducing them with `&`
  ([\#4678](https://github.com/tidyverse/dplyr/issues/4678)). In
  particular this means
  [`across()`](https://dplyr.tidyverse.org/dev/reference/across.md) can
  be used in
  [`filter()`](https://dplyr.tidyverse.org/dev/reference/filter.md).

- [`left_join()`](https://dplyr.tidyverse.org/dev/reference/mutate-joins.md),
  [`right_join()`](https://dplyr.tidyverse.org/dev/reference/mutate-joins.md),
  and
  [`full_join()`](https://dplyr.tidyverse.org/dev/reference/mutate-joins.md)
  gain a `keep` argument so that you can optionally choose to keep both
  sets of join keys
  ([\#4589](https://github.com/tidyverse/dplyr/issues/4589)). This is
  useful when you want to figure out which rows were missing from either
  side.

- Join functions can now perform a cross-join by specifying
  `by = character()`
  ([\#4206](https://github.com/tidyverse/dplyr/issues/4206).)

- [`groups()`](https://dplyr.tidyverse.org/dev/reference/group_data.md)
  now returns [`list()`](https://rdrr.io/r/base/list.html) for ungrouped
  data; previously it returned `NULL` which was type-unstable (when
  there are groups it returns a list of symbols).

- The first argument of
  [`group_map()`](https://dplyr.tidyverse.org/dev/reference/group_map.md),
  [`group_modify()`](https://dplyr.tidyverse.org/dev/reference/group_map.md)
  and
  [`group_walk()`](https://dplyr.tidyverse.org/dev/reference/group_map.md)
  has been changed to `.data` for consistency with other generics.

- `group_keys.rowwise_df()` gives a 0 column data frame with
  [`n()`](https://dplyr.tidyverse.org/dev/reference/context.md) rows.

- [`group_map()`](https://dplyr.tidyverse.org/dev/reference/group_map.md)
  is now a generic
  ([\#4576](https://github.com/tidyverse/dplyr/issues/4576)).

- `group_by(..., .add = TRUE)` replaces `group_by(..., add = TRUE)`,
  with a deprecation message. The old argument name was a mistake
  because it prevents you from creating a new grouping var called `add`
  and it violates our naming conventions
  ([\#4137](https://github.com/tidyverse/dplyr/issues/4137)).

- [`intersect()`](https://dplyr.tidyverse.org/dev/reference/setops.md),
  [`union()`](https://dplyr.tidyverse.org/dev/reference/setops.md),
  [`setdiff()`](https://dplyr.tidyverse.org/dev/reference/setops.md) and
  [`setequal()`](https://dplyr.tidyverse.org/dev/reference/setops.md)
  generics are now imported from the generics package. This reduces a
  conflict with lubridate.

- [`order_by()`](https://dplyr.tidyverse.org/dev/reference/order_by.md)
  gives an informative hint if you accidentally call it instead of
  [`arrange()`](https://dplyr.tidyverse.org/dev/reference/arrange.md)
  [\#3357](https://github.com/tidyverse/dplyr/issues/3357).

- [`tally()`](https://dplyr.tidyverse.org/dev/reference/count.md) and
  [`count()`](https://dplyr.tidyverse.org/dev/reference/count.md) now
  message if the default output `name` (n), already exists in the data
  frame. To quiet the message, you’ll need to supply an explicit `name`
  ([\#4284](https://github.com/tidyverse/dplyr/issues/4284)). You can
  override the default weighting to using a constant by setting
  `wt = 1`.

- `starwars` dataset now does a better job of separating biological sex
  from gender identity. The previous `gender` column has been renamed to
  `sex`, since it actually describes the individual’s biological sex. A
  new `gender` column encodes the actual gender identity using other
  information about the Star Wars universe
  ([@MeganBeckett](https://github.com/MeganBeckett),
  [\#4456](https://github.com/tidyverse/dplyr/issues/4456)).

- [`src_tbls()`](https://dplyr.tidyverse.org/dev/reference/src_tbls.md)
  accepts `...` arguments
  ([\#4485](https://github.com/tidyverse/dplyr/issues/4485),
  [@ianmcook](https://github.com/ianmcook)). This could be a breaking
  change for some dplyr backend packages that implement
  [`src_tbls()`](https://dplyr.tidyverse.org/dev/reference/src_tbls.md).

- Better performance for extracting slices of factors and ordered
  factors ([\#4501](https://github.com/tidyverse/dplyr/issues/4501)).

- [`rename_at()`](https://dplyr.tidyverse.org/dev/reference/select_all.md)
  and
  [`rename_all()`](https://dplyr.tidyverse.org/dev/reference/select_all.md)
  call the function with a simple character vector, not a
  `dplyr_sel_vars`
  ([\#4459](https://github.com/tidyverse/dplyr/issues/4459)).

- [`ntile()`](https://dplyr.tidyverse.org/dev/reference/ntile.md) is now
  more consistent with database implementations if the buckets have
  irregular size
  ([\#4495](https://github.com/tidyverse/dplyr/issues/4495)).

## dplyr 0.8.5 (2020-03-07)

CRAN release: 2020-03-07

- Maintenance release for compatibility with R-devel.

## dplyr 0.8.4 (2020-01-30)

CRAN release: 2020-01-31

- Adapt tests to changes in dependent packages.

## dplyr 0.8.3 (2019-07-04)

CRAN release: 2019-07-04

- Fixed performance regression introduced in version 0.8.2
  ([\#4458](https://github.com/tidyverse/dplyr/issues/4458)).

## dplyr 0.8.2 (2019-06-28)

CRAN release: 2019-06-29

### New functions

- `top_frac(data, proportion)` is a shorthand for
  `top_n(data, proportion * n())`
  ([\#4017](https://github.com/tidyverse/dplyr/issues/4017)).

### colwise changes

- Using quosures in colwise verbs is deprecated
  ([\#4330](https://github.com/tidyverse/dplyr/issues/4330)).

- Updated
  [`distinct_if()`](https://dplyr.tidyverse.org/dev/reference/distinct_all.md),
  [`distinct_at()`](https://dplyr.tidyverse.org/dev/reference/distinct_all.md)
  and
  [`distinct_all()`](https://dplyr.tidyverse.org/dev/reference/distinct_all.md)
  to include `.keep_all` argument
  ([@beansrowning](https://github.com/beansrowning),
  [\#4343](https://github.com/tidyverse/dplyr/issues/4343)).

- [`rename_at()`](https://dplyr.tidyverse.org/dev/reference/select_all.md)
  handles empty selection
  ([\#4324](https://github.com/tidyverse/dplyr/issues/4324)).

- `*_if()` functions correctly handle columns with special names
  ([\#4380](https://github.com/tidyverse/dplyr/issues/4380)).

- colwise functions support constants in formulas
  ([\#4374](https://github.com/tidyverse/dplyr/issues/4374)).

### Hybrid evaluation changes

- hybrid rank functions correctly handle NA
  ([\#4427](https://github.com/tidyverse/dplyr/issues/4427)).

- [`first()`](https://dplyr.tidyverse.org/dev/reference/nth.md),
  [`last()`](https://dplyr.tidyverse.org/dev/reference/nth.md) and
  [`nth()`](https://dplyr.tidyverse.org/dev/reference/nth.md) hybrid
  version handles factors
  ([\#4295](https://github.com/tidyverse/dplyr/issues/4295)).

### Minor changes

- [`top_n()`](https://dplyr.tidyverse.org/dev/reference/top_n.md) quotes
  its `n` argument, `n` no longer needs to be constant for all groups
  ([\#4017](https://github.com/tidyverse/dplyr/issues/4017)).

- [`tbl_vars()`](https://dplyr.tidyverse.org/dev/reference/tbl_vars.md)
  keeps information on grouping columns by returning a `dplyr_sel_vars`
  object ([\#4106](https://github.com/tidyverse/dplyr/issues/4106)).

- [`group_split()`](https://dplyr.tidyverse.org/dev/reference/group_split.md)
  always sets the `ptype` attribute, which make it more robust in the
  case where there are 0 groups.

- [`group_map()`](https://dplyr.tidyverse.org/dev/reference/group_map.md)
  and
  [`group_modify()`](https://dplyr.tidyverse.org/dev/reference/group_map.md)
  work in the 0 group edge case
  ([\#4421](https://github.com/tidyverse/dplyr/issues/4421))

- [`select.list()`](https://rdrr.io/r/utils/select.list.html) method
  added so that
  [`select()`](https://dplyr.tidyverse.org/dev/reference/select.md) does
  not dispatch on lists
  ([\#4279](https://github.com/tidyverse/dplyr/issues/4279)).

- [`view()`](https://tibble.tidyverse.org/reference/view.html) is
  reexported from tibble
  ([\#4423](https://github.com/tidyverse/dplyr/issues/4423)).

- [`group_by()`](https://dplyr.tidyverse.org/dev/reference/group_by.md)
  puts NA groups last in character vectors
  ([\#4227](https://github.com/tidyverse/dplyr/issues/4227)).

- [`arrange()`](https://dplyr.tidyverse.org/dev/reference/arrange.md)
  handles integer64 objects
  ([\#4366](https://github.com/tidyverse/dplyr/issues/4366)).

- [`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)
  correctly resolves summarised list columns
  ([\#4349](https://github.com/tidyverse/dplyr/issues/4349)).

## dplyr 0.8.1 (2019-05-14)

CRAN release: 2019-05-14

### Breaking changes

- [`group_modify()`](https://dplyr.tidyverse.org/dev/reference/group_map.md)
  is the new name of the function previously known as
  [`group_map()`](https://dplyr.tidyverse.org/dev/reference/group_map.md)

### New functions

- [`group_map()`](https://dplyr.tidyverse.org/dev/reference/group_map.md)
  now only calls the function on each group and return a list.

- [`group_by_drop_default()`](https://dplyr.tidyverse.org/dev/reference/group_by_drop_default.md),
  previously known as `dplyr:::group_drops()` is exported
  ([\#4245](https://github.com/tidyverse/dplyr/issues/4245)).

### Minor changes

- Lists of formulas passed to colwise verbs are now automatically named.

- [`group_by()`](https://dplyr.tidyverse.org/dev/reference/group_by.md)
  does a shallow copy even in the no groups case
  ([\#4221](https://github.com/tidyverse/dplyr/issues/4221)).

- Fixed
  [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md) on
  rowwise data frames with 0 rows
  ([\#4224](https://github.com/tidyverse/dplyr/issues/4224)).

- Fixed handling of bare formulas in colwise verbs
  ([\#4183](https://github.com/tidyverse/dplyr/issues/4183)).

- Fixed performance of
  [`n_distinct()`](https://dplyr.tidyverse.org/dev/reference/n_distinct.md)
  ([\#4202](https://github.com/tidyverse/dplyr/issues/4202)).

- [`group_indices()`](https://dplyr.tidyverse.org/dev/reference/group_data.md)
  now ignores empty groups by default for `data.frame`, which is
  consistent with the default of
  [`group_by()`](https://dplyr.tidyverse.org/dev/reference/group_by.md)
  ([@yutannihilation](https://github.com/yutannihilation),
  [\#4208](https://github.com/tidyverse/dplyr/issues/4208)).

- Fixed integer overflow in hybrid
  [`ntile()`](https://dplyr.tidyverse.org/dev/reference/ntile.md)
  ([\#4186](https://github.com/tidyverse/dplyr/issues/4186)).

- colwise functions
  [`summarise_at()`](https://dplyr.tidyverse.org/dev/reference/summarise_all.md)
  … can rename vars in the case of multiple functions
  ([\#4180](https://github.com/tidyverse/dplyr/issues/4180)).

- [`select_if()`](https://dplyr.tidyverse.org/dev/reference/select_all.md)
  and
  [`rename_if()`](https://dplyr.tidyverse.org/dev/reference/select_all.md)
  handle logical vector predicate
  ([\#4213](https://github.com/tidyverse/dplyr/issues/4213)).

- hybrid [`min()`](https://rdrr.io/r/base/Extremes.html) and
  [`max()`](https://rdrr.io/r/base/Extremes.html) cast to integer when
  possible ([\#4258](https://github.com/tidyverse/dplyr/issues/4258)).

- [`bind_rows()`](https://dplyr.tidyverse.org/dev/reference/bind_rows.md)
  correctly handles the cases where there are multiple consecutive
  `NULL` ([\#4296](https://github.com/tidyverse/dplyr/issues/4296)).

- Support for R 3.1.\* has been dropped. The minimal R version supported
  is now 3.2.0.
  <https://www.tidyverse.org/articles/2019/04/r-version-support/>

- [`rename_at()`](https://dplyr.tidyverse.org/dev/reference/select_all.md)
  handles empty selection
  ([\#4324](https://github.com/tidyverse/dplyr/issues/4324)).

## dplyr 0.8.0.1 (2019-02-15)

CRAN release: 2019-02-15

- Fixed integer C/C++ division, forced released by CRAN
  ([\#4185](https://github.com/tidyverse/dplyr/issues/4185)).

## dplyr 0.8.0 (2019-02-14)

CRAN release: 2019-02-14

### Breaking changes

- The error `could not find function "n"` or the warning
  `` Calling `n()` without importing or prefixing it is deprecated, use `dplyr::n()` ``

  indicates when functions like
  [`n()`](https://dplyr.tidyverse.org/dev/reference/context.md),
  [`row_number()`](https://dplyr.tidyverse.org/dev/reference/row_number.md),
  … are not imported or prefixed.

  The easiest fix is to import dplyr with `import(dplyr)` in your
  `NAMESPACE` or `#' `[`@import`](https://github.com/import)` dplyr` in
  a roxygen comment, alternatively such functions can be imported
  selectively as any other function with `importFrom(dplyr, n)` in the
  `NAMESPACE` or
  `#' `[`@importFrom`](https://github.com/importFrom)` dplyr n` in a
  roxygen comment. The third option is to prefix them, i.e. use
  [`dplyr::n()`](https://dplyr.tidyverse.org/dev/reference/context.md)

- If you see `checking S3 generic/method consistency` in R CMD check for
  your package, note that :

  - [`sample_n()`](https://dplyr.tidyverse.org/dev/reference/sample_n.md)
    and
    [`sample_frac()`](https://dplyr.tidyverse.org/dev/reference/sample_n.md)
    have gained `...`
  - [`filter()`](https://dplyr.tidyverse.org/dev/reference/filter.md)
    and [`slice()`](https://dplyr.tidyverse.org/dev/reference/slice.md)
    have gained `.preserve`
  - [`group_by()`](https://dplyr.tidyverse.org/dev/reference/group_by.md)
    has gained `.drop`

- `` Error: `.data` is a corrupt grouped_df, ... `` signals code that
  makes wrong assumptions about the internals of a grouped data frame.

### New functions

- New selection helpers
  [`group_cols()`](https://dplyr.tidyverse.org/dev/reference/group_cols.md).
  It can be called in selection contexts such as
  [`select()`](https://dplyr.tidyverse.org/dev/reference/select.md) and
  matches the grouping variables of grouped tibbles.

- [`last_col()`](https://tidyselect.r-lib.org/reference/everything.html)
  is re-exported from tidyselect
  ([\#3584](https://github.com/tidyverse/dplyr/issues/3584)).

- [`group_trim()`](https://dplyr.tidyverse.org/dev/reference/group_trim.md)
  drops unused levels of factors that are used as grouping variables.

- [`nest_join()`](https://dplyr.tidyverse.org/dev/reference/nest_join.md)
  creates a list column of the matching rows.
  [`nest_join()`](https://dplyr.tidyverse.org/dev/reference/nest_join.md) +
  [`tidyr::unnest()`](https://tidyr.tidyverse.org/reference/unnest.html)
  is equivalent to `inner_join`
  ([\#3570](https://github.com/tidyverse/dplyr/issues/3570)).

  ``` r
  band_members %>%
    nest_join(band_instruments)
  ```

- [`group_nest()`](https://dplyr.tidyverse.org/dev/reference/group_nest.md)
  is similar to
  [`tidyr::nest()`](https://tidyr.tidyverse.org/reference/nest.html) but
  focusing on the variables to nest by instead of the nested columns.

  ``` r
  starwars %>%
    group_by(species, homeworld) %>%
    group_nest()

  starwars %>%
    group_nest(species, homeworld)
  ```

- [`group_split()`](https://dplyr.tidyverse.org/dev/reference/group_split.md)
  is similar to [`base::split()`](https://rdrr.io/r/base/split.html) but
  operating on existing groups when applied to a grouped data frame, or
  subject to the data mask on ungrouped data frames

  ``` r
  starwars %>%
    group_by(species, homeworld) %>%
    group_split()

  starwars %>%
    group_split(species, homeworld)
  ```

- [`group_map()`](https://dplyr.tidyverse.org/dev/reference/group_map.md)
  and
  [`group_walk()`](https://dplyr.tidyverse.org/dev/reference/group_map.md)
  are purrr-like functions to iterate on groups of a grouped data frame,
  jointly identified by the data subset (exposed as `.x`) and the data
  key (a one row tibble, exposed as `.y`).
  [`group_map()`](https://dplyr.tidyverse.org/dev/reference/group_map.md)
  returns a grouped data frame that combines the results of the
  function,
  [`group_walk()`](https://dplyr.tidyverse.org/dev/reference/group_map.md)
  is only used for side effects and returns its input invisibly.

  ``` r
  mtcars %>%
    group_by(cyl) %>%
    group_map(~ head(.x, 2L))
  ```

- [`distinct_prepare()`](https://dplyr.tidyverse.org/dev/reference/group_by_prepare.md),
  previously known as `distinct_vars()` is exported. This is mostly
  useful for alternative backends (e.g. `dbplyr`).

### Major changes

- [`group_by()`](https://dplyr.tidyverse.org/dev/reference/group_by.md)
  gains the `.drop` argument. When set to `FALSE` the groups are
  generated based on factor levels, hence some groups may be empty
  ([\#341](https://github.com/tidyverse/dplyr/issues/341)).

  ``` r
  # 3 groups
  tibble(
    x = 1:2,
    f = factor(c("a", "b"), levels = c("a", "b", "c"))
  ) %>%
    group_by(f, .drop = FALSE)

  # the order of the grouping variables matter
  df <- tibble(
    x = c(1,2,1,2),
    f = factor(c("a", "b", "a", "b"), levels = c("a", "b", "c"))
  )
  df %>% group_by(f, x, .drop = FALSE)
  df %>% group_by(x, f, .drop = FALSE)
  ```

  The default behaviour drops the empty groups as in the previous
  versions.

  ``` r
  tibble(
      x = 1:2,
      f = factor(c("a", "b"), levels = c("a", "b", "c"))
    ) %>%
      group_by(f)
  ```

- [`filter()`](https://dplyr.tidyverse.org/dev/reference/filter.md) and
  [`slice()`](https://dplyr.tidyverse.org/dev/reference/slice.md) gain a
  `.preserve` argument to control which groups it should keep. The
  default `filter(.preserve = FALSE)` recalculates the grouping
  structure based on the resulting data, otherwise it is kept as is.

  ``` r
  df <- tibble(
    x = c(1,2,1,2),
    f = factor(c("a", "b", "a", "b"), levels = c("a", "b", "c"))
  ) %>%
    group_by(x, f, .drop = FALSE)

  df %>% filter(x == 1)
  df %>% filter(x == 1, .preserve = TRUE)
  ```

- The notion of lazily grouped data frames have disappeared. All dplyr
  verbs now recalculate immediately the grouping structure, and respect
  the levels of factors.

- Subsets of columns now properly dispatch to the `[` or `[[` method
  when the column is an object (a vector with a class) instead of making
  assumptions on how the column should be handled. The `[` method must
  handle integer indices, including `NA_integer_`, i.e. `x[NA_integer_]`
  should produce a vector of the same class as `x` with whatever
  represents a missing value.

### Minor changes

- [`tally()`](https://dplyr.tidyverse.org/dev/reference/count.md) works
  correctly on non-data frame table sources such as `tbl_sql`
  ([\#3075](https://github.com/tidyverse/dplyr/issues/3075)).

- [`sample_n()`](https://dplyr.tidyverse.org/dev/reference/sample_n.md)
  and
  [`sample_frac()`](https://dplyr.tidyverse.org/dev/reference/sample_n.md)
  can use [`n()`](https://dplyr.tidyverse.org/dev/reference/context.md)
  ([\#3527](https://github.com/tidyverse/dplyr/issues/3527))

- [`distinct()`](https://dplyr.tidyverse.org/dev/reference/distinct.md)
  respects the order of the variables provided
  ([\#3195](https://github.com/tidyverse/dplyr/issues/3195),
  [@foo-bar-baz-qux](https://github.com/foo-bar-baz-qux)) and handles
  the 0 rows and 0 columns special case
  ([\#2954](https://github.com/tidyverse/dplyr/issues/2954)).

- [`combine()`](https://dplyr.tidyverse.org/dev/reference/defunct.md)
  uses tidy dots
  ([\#3407](https://github.com/tidyverse/dplyr/issues/3407)).

- [`group_indices()`](https://dplyr.tidyverse.org/dev/reference/group_data.md)
  can be used without argument in expressions in verbs
  ([\#1185](https://github.com/tidyverse/dplyr/issues/1185)).

- Using
  [`mutate_all()`](https://dplyr.tidyverse.org/dev/reference/mutate_all.md),
  [`transmute_all()`](https://dplyr.tidyverse.org/dev/reference/mutate_all.md),
  [`mutate_if()`](https://dplyr.tidyverse.org/dev/reference/mutate_all.md)
  and
  [`transmute_if()`](https://dplyr.tidyverse.org/dev/reference/mutate_all.md)
  with grouped tibbles now informs you that the grouping variables are
  ignored. In the case of the `_all()` verbs, the message invites you to
  use `mutate_at(df, vars(-group_cols()))` (or the equivalent
  [`transmute_at()`](https://dplyr.tidyverse.org/dev/reference/mutate_all.md)
  call) instead if you’d like to make it explicit in your code that the
  operation is not applied on the grouping variables.

- Scoped variants of
  [`arrange()`](https://dplyr.tidyverse.org/dev/reference/arrange.md)
  respect the `.by_group` argument
  ([\#3504](https://github.com/tidyverse/dplyr/issues/3504)).

- [`first()`](https://dplyr.tidyverse.org/dev/reference/nth.md) and
  [`last()`](https://dplyr.tidyverse.org/dev/reference/nth.md) hybrid
  functions fall back to R evaluation when given no arguments
  ([\#3589](https://github.com/tidyverse/dplyr/issues/3589)).

- [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md)
  removes a column when the expression evaluates to `NULL` for all
  groups ([\#2945](https://github.com/tidyverse/dplyr/issues/2945)).

- grouped data frames support `[, drop = TRUE]`
  ([\#3714](https://github.com/tidyverse/dplyr/issues/3714)).

- New low-level constructor
  [`new_grouped_df()`](https://dplyr.tidyverse.org/dev/reference/new_grouped_df.md)
  and validator `validate_grouped_df`
  ([\#3837](https://github.com/tidyverse/dplyr/issues/3837)).

- [`glimpse()`](https://dplyr.tidyverse.org/dev/reference/glimpse.md)
  prints group information on grouped tibbles
  ([\#3384](https://github.com/tidyverse/dplyr/issues/3384)).

- [`sample_n()`](https://dplyr.tidyverse.org/dev/reference/sample_n.md)
  and
  [`sample_frac()`](https://dplyr.tidyverse.org/dev/reference/sample_n.md)
  gain `...` ([\#2888](https://github.com/tidyverse/dplyr/issues/2888)).

- Scoped filter variants now support functions and purrr-like lambdas:

  ``` r
  mtcars %>% filter_at(vars(hp, vs), ~ . %% 2 == 0)
  ```

### Lifecycle

- [`do()`](https://dplyr.tidyverse.org/dev/reference/do.md),
  [`rowwise()`](https://dplyr.tidyverse.org/dev/reference/rowwise.md)
  and
  [`combine()`](https://dplyr.tidyverse.org/dev/reference/defunct.md)
  are questioning
  ([\#3494](https://github.com/tidyverse/dplyr/issues/3494)).

- [`funs()`](https://dplyr.tidyverse.org/dev/reference/funs.md) is
  soft-deprecated and will start issuing warnings in a future version.

### Changes to column wise functions

- Scoped variants for
  [`distinct()`](https://dplyr.tidyverse.org/dev/reference/distinct.md):
  [`distinct_at()`](https://dplyr.tidyverse.org/dev/reference/distinct_all.md),
  [`distinct_if()`](https://dplyr.tidyverse.org/dev/reference/distinct_all.md),
  [`distinct_all()`](https://dplyr.tidyverse.org/dev/reference/distinct_all.md)
  ([\#2948](https://github.com/tidyverse/dplyr/issues/2948)).

- [`summarise_at()`](https://dplyr.tidyverse.org/dev/reference/summarise_all.md)
  excludes the grouping variables
  ([\#3613](https://github.com/tidyverse/dplyr/issues/3613)).

- [`mutate_all()`](https://dplyr.tidyverse.org/dev/reference/mutate_all.md),
  [`mutate_at()`](https://dplyr.tidyverse.org/dev/reference/mutate_all.md),
  [`summarise_all()`](https://dplyr.tidyverse.org/dev/reference/summarise_all.md)
  and
  [`summarise_at()`](https://dplyr.tidyverse.org/dev/reference/summarise_all.md)
  handle utf-8 names
  ([\#2967](https://github.com/tidyverse/dplyr/issues/2967)).

### Performance

- R expressions that cannot be handled with native code are now
  evaluated with unwind-protection when available (on R 3.5 and later).
  This improves the performance of dplyr on data frames with many groups
  (and hence many expressions to evaluate). We benchmarked that
  computing a grouped average is consistently twice as fast with
  unwind-protection enabled.

  Unwind-protection also makes dplyr more robust in corner cases because
  it ensures the C++ destructors are correctly called in all
  circumstances (debugger exit, captured condition, restart invocation).

- [`sample_n()`](https://dplyr.tidyverse.org/dev/reference/sample_n.md)
  and
  [`sample_frac()`](https://dplyr.tidyverse.org/dev/reference/sample_n.md)
  gain `...` ([\#2888](https://github.com/tidyverse/dplyr/issues/2888)).

- Improved performance for wide tibbles
  ([\#3335](https://github.com/tidyverse/dplyr/issues/3335)).

- Faster hybrid [`sum()`](https://rdrr.io/r/base/sum.html),
  [`mean()`](https://rdrr.io/r/base/mean.html),
  [`var()`](https://rdrr.io/r/stats/cor.html) and
  [`sd()`](https://rdrr.io/r/stats/sd.html) for logical vectors
  ([\#3189](https://github.com/tidyverse/dplyr/issues/3189)).

- Hybrid version of `sum(na.rm = FALSE)` exits early when there are
  missing values. This considerably improves performance when there are
  missing values early in the vector
  ([\#3288](https://github.com/tidyverse/dplyr/issues/3288)).

- [`group_by()`](https://dplyr.tidyverse.org/dev/reference/group_by.md)
  does not trigger the additional
  [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md) on
  simple uses of the `.data` pronoun
  ([\#3533](https://github.com/tidyverse/dplyr/issues/3533)).

### Internal

- The grouping metadata of grouped data frame has been reorganized in a
  single tidy tibble, that can be accessed with the new
  [`group_data()`](https://dplyr.tidyverse.org/dev/reference/group_data.md)
  function. The grouping tibble consists of one column per grouping
  variable, followed by a list column of the (1-based) indices of the
  groups. The new
  [`group_rows()`](https://dplyr.tidyverse.org/dev/reference/group_data.md)
  function retrieves that list of indices
  ([\#3489](https://github.com/tidyverse/dplyr/issues/3489)).

  ``` r
  # the grouping metadata, as a tibble
  group_by(starwars, homeworld) %>%
    group_data()

  # the indices
  group_by(starwars, homeworld) %>%
    group_data() %>%
    pull(.rows)

  group_by(starwars, homeworld) %>%
    group_rows()
  ```

- Hybrid evaluation has been completely redesigned for better
  performance and stability.

### Documentation

- Add documentation example for moving variable to back in
  [`?select`](https://dplyr.tidyverse.org/dev/reference/select.md)
  ([\#3051](https://github.com/tidyverse/dplyr/issues/3051)).

- column wise functions are better documented, in particular explaining
  when grouping variables are included as part of the selection.

#### Deprecated and defunct functions

- [`mutate_each()`](https://dplyr.tidyverse.org/dev/reference/defunct-each.md)
  and
  [`summarise_each()`](https://dplyr.tidyverse.org/dev/reference/defunct-each.md)
  are deprecated.

## dplyr 0.7.6

CRAN release: 2018-06-29

- `exprs()` is no longer exported to avoid conflicts with
  `Biobase::exprs()`
  ([\#3638](https://github.com/tidyverse/dplyr/issues/3638)).

- The MASS package is explicitly suggested to fix CRAN warnings on
  R-devel ([\#3657](https://github.com/tidyverse/dplyr/issues/3657)).

- Set operations like
  [`intersect()`](https://dplyr.tidyverse.org/dev/reference/setops.md)
  and [`setdiff()`](https://dplyr.tidyverse.org/dev/reference/setops.md)
  reconstruct groups metadata
  ([\#3587](https://github.com/tidyverse/dplyr/issues/3587)) and keep
  the order of the rows
  ([\#3839](https://github.com/tidyverse/dplyr/issues/3839)).

- Using namespaced calls to
  [`base::sort()`](https://rdrr.io/r/base/sort.html) and
  [`base::unique()`](https://rdrr.io/r/base/unique.html) from C++ code
  to avoid ambiguities when these functions are overridden
  ([\#3644](https://github.com/tidyverse/dplyr/issues/3644)).

- Fix rchk errors
  ([\#3693](https://github.com/tidyverse/dplyr/issues/3693)).

## dplyr 0.7.5 (2018-04-14)

CRAN release: 2018-05-19

### Breaking changes for package developers

- The major change in this version is that dplyr now depends on the
  selecting backend of the tidyselect package. If you have been linking
  to `dplyr::select_helpers` documentation topic, you should update the
  link to point to
  [`tidyselect::select_helpers`](https://tidyselect.r-lib.org/reference/language.html).

- Another change that causes warnings in packages is that dplyr now
  exports the `exprs()` function. This causes a collision with
  `Biobase::exprs()`. Either import functions from dplyr selectively
  rather than in bulk, or do not import `Biobase::exprs()` and refer to
  it with a namespace qualifier.

### Bug fixes

- `distinct(data, "string")` now returns a one-row data frame again.
  (The previous behavior was to return the data unchanged.)

- [`do()`](https://dplyr.tidyverse.org/dev/reference/do.md) operations
  with more than one named argument can access `.`
  ([\#2998](https://github.com/tidyverse/dplyr/issues/2998)).

- Reindexing grouped data frames (e.g. after
  [`filter()`](https://dplyr.tidyverse.org/dev/reference/filter.md) or
  `..._join()`) never updates the `"class"` attribute. This also avoids
  unintended updates to the original object
  ([\#3438](https://github.com/tidyverse/dplyr/issues/3438)).

- Fixed rare column name clash in `..._join()` with non-join columns of
  the same name in both tables
  ([\#3266](https://github.com/tidyverse/dplyr/issues/3266)).

- Fix [`ntile()`](https://dplyr.tidyverse.org/dev/reference/ntile.md)
  and
  [`row_number()`](https://dplyr.tidyverse.org/dev/reference/row_number.md)
  ordering to use the locale-dependent ordering functions in R when
  dealing with character vectors, rather than always using the C-locale
  ordering function in C
  ([\#2792](https://github.com/tidyverse/dplyr/issues/2792),
  [@foo-bar-baz-qux](https://github.com/foo-bar-baz-qux)).

- Summaries of summaries (such as `summarise(b = sum(a), c = sum(b))`)
  are now computed using standard evaluation for simplicity and
  correctness, but slightly slower
  ([\#3233](https://github.com/tidyverse/dplyr/issues/3233)).

- Fixed
  [`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)
  for empty data frames with zero columns
  ([\#3071](https://github.com/tidyverse/dplyr/issues/3071)).

### Major changes

- [`enexpr()`](https://dplyr.tidyverse.org/dev/reference/tidyeval-compat.md),
  [`expr()`](https://dplyr.tidyverse.org/dev/reference/tidyeval-compat.md),
  `exprs()`,
  [`sym()`](https://dplyr.tidyverse.org/dev/reference/tidyeval-compat.md)
  and
  [`syms()`](https://dplyr.tidyverse.org/dev/reference/tidyeval-compat.md)
  are now exported.
  [`sym()`](https://dplyr.tidyverse.org/dev/reference/tidyeval-compat.md)
  and
  [`syms()`](https://dplyr.tidyverse.org/dev/reference/tidyeval-compat.md)
  construct symbols from strings or character vectors. The
  [`expr()`](https://dplyr.tidyverse.org/dev/reference/tidyeval-compat.md)
  variants are equivalent to
  [`quo()`](https://dplyr.tidyverse.org/dev/reference/tidyeval-compat.md),
  [`quos()`](https://dplyr.tidyverse.org/dev/reference/tidyeval-compat.md)
  and
  [`enquo()`](https://dplyr.tidyverse.org/dev/reference/tidyeval-compat.md)
  but return simple expressions rather than quosures. They support
  quasiquotation.

- dplyr now depends on the new tidyselect package to power
  [`select()`](https://dplyr.tidyverse.org/dev/reference/select.md),
  [`rename()`](https://dplyr.tidyverse.org/dev/reference/rename.md),
  [`pull()`](https://dplyr.tidyverse.org/dev/reference/pull.md) and
  their variants
  ([\#2896](https://github.com/tidyverse/dplyr/issues/2896)).
  Consequently `select_vars()`, `select_var()` and `rename_vars()` are
  soft-deprecated and will start issuing warnings in a future version.

  Following the switch to tidyselect,
  [`select()`](https://dplyr.tidyverse.org/dev/reference/select.md) and
  [`rename()`](https://dplyr.tidyverse.org/dev/reference/rename.md)
  fully support character vectors. You can now unquote variables like
  this:

      vars <- c("disp", "cyl")
      select(mtcars, !! vars)
      select(mtcars, -(!! vars))

  Note that this only works in selecting functions because in other
  contexts strings and character vectors are ambiguous. For instance
  strings are a valid input in mutating operations and
  `mutate(df, "foo")` creates a new column by recycling “foo” to the
  number of rows.

### Minor changes

- Support for raw vector columns in
  [`arrange()`](https://dplyr.tidyverse.org/dev/reference/arrange.md),
  [`group_by()`](https://dplyr.tidyverse.org/dev/reference/group_by.md),
  [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md),
  [`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)
  and `..._join()` (minimal `raw` x `raw` support initially)
  ([\#1803](https://github.com/tidyverse/dplyr/issues/1803)).

- [`bind_cols()`](https://dplyr.tidyverse.org/dev/reference/bind_cols.md)
  handles unnamed list
  ([\#3402](https://github.com/tidyverse/dplyr/issues/3402)).

- [`bind_rows()`](https://dplyr.tidyverse.org/dev/reference/bind_rows.md)
  works around corrupt columns that have the object bit set while having
  no class attribute
  ([\#3349](https://github.com/tidyverse/dplyr/issues/3349)).

- [`combine()`](https://dplyr.tidyverse.org/dev/reference/defunct.md)
  returns [`logical()`](https://rdrr.io/r/base/logical.html) when all
  inputs are `NULL` (or when there are no inputs)
  ([\#3365](https://github.com/tidyverse/dplyr/issues/3365),
  [@zeehio](https://github.com/zeehio)).

- [`distinct()`](https://dplyr.tidyverse.org/dev/reference/distinct.md)
  now supports renaming columns
  ([\#3234](https://github.com/tidyverse/dplyr/issues/3234)).

- Hybrid evaluation simplifies `dplyr::foo()` to `foo()`
  ([\#3309](https://github.com/tidyverse/dplyr/issues/3309)). Hybrid
  functions can now be masked by regular R functions to turn off hybrid
  evaluation ([\#3255](https://github.com/tidyverse/dplyr/issues/3255)).
  The hybrid evaluator finds functions from dplyr even if dplyr is not
  attached ([\#3456](https://github.com/tidyverse/dplyr/issues/3456)).

- In [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md)
  it is now illegal to use `data.frame` in the rhs
  ([\#3298](https://github.com/tidyverse/dplyr/issues/3298)).

- Support `!!!` in
  [`recode_factor()`](https://dplyr.tidyverse.org/dev/reference/recode.md)
  ([\#3390](https://github.com/tidyverse/dplyr/issues/3390)).

- [`row_number()`](https://dplyr.tidyverse.org/dev/reference/row_number.md)
  works on empty subsets
  ([\#3454](https://github.com/tidyverse/dplyr/issues/3454)).

- [`select()`](https://dplyr.tidyverse.org/dev/reference/select.md) and
  [`vars()`](https://dplyr.tidyverse.org/dev/reference/vars.md) now
  treat `NULL` as empty inputs
  ([\#3023](https://github.com/tidyverse/dplyr/issues/3023)).

- Scoped select and rename functions
  ([`select_all()`](https://dplyr.tidyverse.org/dev/reference/select_all.md),
  [`rename_if()`](https://dplyr.tidyverse.org/dev/reference/select_all.md)
  etc.) now work with grouped data frames, adapting the grouping as
  necessary ([\#2947](https://github.com/tidyverse/dplyr/issues/2947),
  [\#3410](https://github.com/tidyverse/dplyr/issues/3410)).
  [`group_by_at()`](https://dplyr.tidyverse.org/dev/reference/group_by_all.md)
  can group by an existing grouping variable
  ([\#3351](https://github.com/tidyverse/dplyr/issues/3351)).
  [`arrange_at()`](https://dplyr.tidyverse.org/dev/reference/arrange_all.md)
  can use grouping variables
  ([\#3332](https://github.com/tidyverse/dplyr/issues/3332)).

- [`slice()`](https://dplyr.tidyverse.org/dev/reference/slice.md) no
  longer enforce tibble classes when input is a simple `data.frame`, and
  ignores 0 ([\#3297](https://github.com/tidyverse/dplyr/issues/3297),
  [\#3313](https://github.com/tidyverse/dplyr/issues/3313)).

- [`transmute()`](https://dplyr.tidyverse.org/dev/reference/transmute.md)
  no longer prints a message when including a group variable.

### Documentation

- Improved documentation for
  [`funs()`](https://dplyr.tidyverse.org/dev/reference/funs.md)
  ([\#3094](https://github.com/tidyverse/dplyr/issues/3094)) and set
  operations
  (e.g. [`union()`](https://dplyr.tidyverse.org/dev/reference/setops.md))
  ([\#3238](https://github.com/tidyverse/dplyr/issues/3238),
  [@edublancas](https://github.com/edublancas)).

### Error messages

- Better error message if dbplyr is not installed when accessing
  database backends
  ([\#3225](https://github.com/tidyverse/dplyr/issues/3225)).

- [`arrange()`](https://dplyr.tidyverse.org/dev/reference/arrange.md)
  fails gracefully on `data.frame` columns
  ([\#3153](https://github.com/tidyverse/dplyr/issues/3153)).

- Corrected error message when calling
  [`cbind()`](https://rdrr.io/r/base/cbind.html) with an object of wrong
  length ([\#3085](https://github.com/tidyverse/dplyr/issues/3085)).

- Add warning with explanation to
  [`distinct()`](https://dplyr.tidyverse.org/dev/reference/distinct.md)
  if any of the selected columns are of type `list`
  ([\#3088](https://github.com/tidyverse/dplyr/issues/3088),
  [@foo-bar-baz-qux](https://github.com/foo-bar-baz-qux)), or when used
  on unknown columns
  ([\#2867](https://github.com/tidyverse/dplyr/issues/2867),
  [@foo-bar-baz-qux](https://github.com/foo-bar-baz-qux)).

- Show clear error message for bad arguments to
  [`funs()`](https://dplyr.tidyverse.org/dev/reference/funs.md)
  ([\#3368](https://github.com/tidyverse/dplyr/issues/3368)).

- Better error message in `..._join()` when joining data frames with
  duplicate or `NA` column names. Joining such data frames with a semi-
  or anti-join now gives a warning, which may be converted to an error
  in future versions
  ([\#3243](https://github.com/tidyverse/dplyr/issues/3243),
  [\#3417](https://github.com/tidyverse/dplyr/issues/3417)).

- Dedicated error message when trying to use columns of the `Interval`
  or `Period` classes
  ([\#2568](https://github.com/tidyverse/dplyr/issues/2568)).

- Added an `.onDetach()` hook that allows for plyr to be loaded and
  attached without the warning message that says functions in dplyr will
  be masked, since dplyr is no longer attached
  ([\#3359](https://github.com/tidyverse/dplyr/issues/3359),
  [@jwnorman](https://github.com/jwnorman)).

### Performance

- [`sample_n()`](https://dplyr.tidyverse.org/dev/reference/sample_n.md)
  and
  [`sample_frac()`](https://dplyr.tidyverse.org/dev/reference/sample_n.md)
  on grouped data frame are now faster especially for those with large
  number of groups
  ([\#3193](https://github.com/tidyverse/dplyr/issues/3193),
  [@saurfang](https://github.com/saurfang)).

### Internal

- Compute variable names for joins in R
  ([\#3430](https://github.com/tidyverse/dplyr/issues/3430)).

- Bumped Rcpp dependency to 0.12.15 to avoid imperfect detection of `NA`
  values in hybrid evaluation fixed in RcppCore/Rcpp#790
  ([\#2919](https://github.com/tidyverse/dplyr/issues/2919)).

- Avoid cleaning the data mask, a temporary environment used to evaluate
  expressions. If the environment, in which e.g. a
  [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md)
  expression is evaluated, is preserved until after the operation,
  accessing variables from that environment now gives a warning but
  still returns `NULL`
  ([\#3318](https://github.com/tidyverse/dplyr/issues/3318)).

## dplyr 0.7.4

CRAN release: 2017-09-28

- Fix recent Fedora and ASAN check errors
  ([\#3098](https://github.com/tidyverse/dplyr/issues/3098)).

- Avoid dependency on Rcpp 0.12.10
  ([\#3106](https://github.com/tidyverse/dplyr/issues/3106)).

## dplyr 0.7.3

CRAN release: 2017-09-09

- Fixed protection error that occurred when creating a character column
  using grouped
  [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md)
  ([\#2971](https://github.com/tidyverse/dplyr/issues/2971)).

- Fixed a rare problem with accessing variable values in
  [`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)
  when all groups have size one
  ([\#3050](https://github.com/tidyverse/dplyr/issues/3050)).

- [`distinct()`](https://dplyr.tidyverse.org/dev/reference/distinct.md)
  now throws an error when used on unknown columns
  ([\#2867](https://github.com/tidyverse/dplyr/issues/2867),
  [@foo-bar-baz-qux](https://github.com/foo-bar-baz-qux)).

- Fixed rare out-of-bounds memory write in
  [`slice()`](https://dplyr.tidyverse.org/dev/reference/slice.md) when
  negative indices beyond the number of rows were involved
  ([\#3073](https://github.com/tidyverse/dplyr/issues/3073)).

- [`select()`](https://dplyr.tidyverse.org/dev/reference/select.md),
  [`rename()`](https://dplyr.tidyverse.org/dev/reference/rename.md) and
  [`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)
  no longer change the grouped vars of the original data
  ([\#3038](https://github.com/tidyverse/dplyr/issues/3038)).

- `nth(default = var)`, `first(default = var)` and `last(default = var)`
  fall back to standard evaluation in a grouped operation instead of
  triggering an error
  ([\#3045](https://github.com/tidyverse/dplyr/issues/3045)).

- [`case_when()`](https://dplyr.tidyverse.org/dev/reference/case-and-replace-when.md)
  now works if all LHS are atomic
  ([\#2909](https://github.com/tidyverse/dplyr/issues/2909)), or when
  LHS or RHS values are zero-length vectors
  ([\#3048](https://github.com/tidyverse/dplyr/issues/3048)).

- [`case_when()`](https://dplyr.tidyverse.org/dev/reference/case-and-replace-when.md)
  accepts `NA` on the LHS
  ([\#2927](https://github.com/tidyverse/dplyr/issues/2927)).

- Semi- and anti-joins now preserve the order of left-hand-side data
  frame ([\#3089](https://github.com/tidyverse/dplyr/issues/3089)).

- Improved error message for invalid list arguments to
  [`bind_rows()`](https://dplyr.tidyverse.org/dev/reference/bind_rows.md)
  ([\#3068](https://github.com/tidyverse/dplyr/issues/3068)).

- Grouping by character vectors is now faster
  ([\#2204](https://github.com/tidyverse/dplyr/issues/2204)).

- Fixed a crash that occurred when an unexpected input was supplied to
  the `call` argument of
  [`order_by()`](https://dplyr.tidyverse.org/dev/reference/order_by.md)
  ([\#3065](https://github.com/tidyverse/dplyr/issues/3065)).

## dplyr 0.7.2

CRAN release: 2017-07-20

- Move build-time vs. run-time checks out of `.onLoad()` and into
  `dr_dplyr()`.

## dplyr 0.7.1

CRAN release: 2017-06-22

- Use new versions of bindrcpp and glue to avoid protection problems.
  Avoid wrapping arguments to internal error functions
  ([\#2877](https://github.com/tidyverse/dplyr/issues/2877)). Fix two
  protection mistakes found by rchk
  ([\#2868](https://github.com/tidyverse/dplyr/issues/2868)).

- Fix C++ error that caused compilation to fail on mac cran
  ([\#2862](https://github.com/tidyverse/dplyr/issues/2862))

- Fix undefined behaviour in
  [`between()`](https://dplyr.tidyverse.org/dev/reference/between.md),
  where `NA_REAL` were assigned instead of `NA_LOGICAL`.
  ([\#2855](https://github.com/tidyverse/dplyr/issues/2855),
  [@zeehio](https://github.com/zeehio))

- [`top_n()`](https://dplyr.tidyverse.org/dev/reference/top_n.md) now
  executes operations lazily for compatibility with database backends
  ([\#2848](https://github.com/tidyverse/dplyr/issues/2848)).

- Reuse of new variables created in ungrouped
  [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md)
  possible again, regression introduced in dplyr 0.7.0
  ([\#2869](https://github.com/tidyverse/dplyr/issues/2869)).

- Quosured symbols do not prevent hybrid handling anymore. This should
  fix many performance issues introduced with tidyeval
  ([\#2822](https://github.com/tidyverse/dplyr/issues/2822)).

## dplyr 0.7.0

CRAN release: 2017-06-09

### New data, functions, and features

- Five new datasets provide some interesting built-in datasets to
  demonstrate dplyr verbs
  ([\#2094](https://github.com/tidyverse/dplyr/issues/2094)):

  - `starwars` dataset about starwars characters; has list columns
  - `storms` has the trajectories of ~200 tropical storms
  - `band_members`, `band_instruments` and `band_instruments2` has some
    simple data to demonstrate joins.

- New
  [`add_count()`](https://dplyr.tidyverse.org/dev/reference/count.md)
  and
  [`add_tally()`](https://dplyr.tidyverse.org/dev/reference/count.md)
  for adding an `n` column within groups
  ([\#2078](https://github.com/tidyverse/dplyr/issues/2078),
  [@dgrtwo](https://github.com/dgrtwo)).

- [`arrange()`](https://dplyr.tidyverse.org/dev/reference/arrange.md)
  for grouped data frames gains a `.by_group` argument so you can choose
  to sort by groups if you want to (defaults to `FALSE`)
  ([\#2318](https://github.com/tidyverse/dplyr/issues/2318))

- New [`pull()`](https://dplyr.tidyverse.org/dev/reference/pull.md)
  generic for extracting a single column either by name or position
  (either from the left or the right). Thanks to
  [@paulponcet](https://github.com/paulponcet) for the idea
  ([\#2054](https://github.com/tidyverse/dplyr/issues/2054)).

  This verb is powered with the new `select_var()` internal helper,
  which is exported as well. It is like `select_vars()` but returns a
  single variable.

- [`as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)
  is re-exported from tibble. This is the recommend way to create
  tibbles from existing data frames.
  [`tbl_df()`](https://dplyr.tidyverse.org/dev/reference/defunct.md) has
  been softly deprecated.
  [`tribble()`](https://tibble.tidyverse.org/reference/tribble.html) is
  now imported from tibble
  ([\#2336](https://github.com/tidyverse/dplyr/issues/2336),
  [@chrMongeau](https://github.com/chrMongeau)); this is now preferred
  to
  [`frame_data()`](https://tibble.tidyverse.org/reference/deprecated.html).

### Deprecated and defunct

- dplyr no longer messages that you need dtplyr to work with data.table
  ([\#2489](https://github.com/tidyverse/dplyr/issues/2489)).

- Long deprecated `regroup()`, `mutate_each_q()` and
  `summarise_each_q()` functions have been removed.

- Deprecated `failwith()`. I’m not even sure why it was here.

- Soft-deprecated
  [`mutate_each()`](https://dplyr.tidyverse.org/dev/reference/defunct-each.md)
  and
  [`summarise_each()`](https://dplyr.tidyverse.org/dev/reference/defunct-each.md),
  these functions print a message which will be changed to a warning in
  the next release.

- The `.env` argument to
  [`sample_n()`](https://dplyr.tidyverse.org/dev/reference/sample_n.md)
  and
  [`sample_frac()`](https://dplyr.tidyverse.org/dev/reference/sample_n.md)
  is defunct, passing a value to this argument print a message which
  will be changed to a warning in the next release.

### Databases

This version of dplyr includes some major changes to how database
connections work. By and large, you should be able to continue using
your existing dplyr database code without modification, but there are
two big changes that you should be aware of:

- Almost all database related code has been moved out of dplyr and into
  a new package, [dbplyr](https://github.com/tidyverse/dbplyr/). This
  makes dplyr simpler, and will make it easier to release fixes for bugs
  that only affect databases.
  [`src_mysql()`](https://dplyr.tidyverse.org/dev/reference/defunct.md),
  [`src_postgres()`](https://dplyr.tidyverse.org/dev/reference/defunct.md),
  and
  [`src_sqlite()`](https://dplyr.tidyverse.org/dev/reference/defunct.md)
  will still live dplyr so your existing code continues to work.

- It is no longer necessary to create a remote “src”. Instead you can
  work directly with the database connection returned by DBI. This
  reflects the maturity of the DBI ecosystem. Thanks largely to the work
  of Kirill Muller (funded by the R Consortium) DBI backends are now
  much more consistent, comprehensive, and easier to use. That means
  that there’s no longer a need for a layer in between you and DBI.

You can continue to use
[`src_mysql()`](https://dplyr.tidyverse.org/dev/reference/defunct.md),
[`src_postgres()`](https://dplyr.tidyverse.org/dev/reference/defunct.md),
and
[`src_sqlite()`](https://dplyr.tidyverse.org/dev/reference/defunct.md),
but I recommend a new style that makes the connection to DBI more clear:

``` r
library(dplyr)

con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
DBI::dbWriteTable(con, "mtcars", mtcars)

mtcars2 <- tbl(con, "mtcars")
mtcars2
```

This is particularly useful if you want to perform non-SELECT queries as
you can do whatever you want with
[`DBI::dbGetQuery()`](https://dbi.r-dbi.org/reference/dbGetQuery.html)
and
[`DBI::dbExecute()`](https://dbi.r-dbi.org/reference/dbExecute.html).

If you’ve implemented a database backend for dplyr, please read the
[backend
news](https://github.com/tidyverse/dbplyr/blob/main/NEWS.md#backends) to
see what’s changed from your perspective (not much). If you want to
ensure your package works with both the current and previous version of
dplyr, see
[`wrap_dbplyr_obj()`](https://dplyr.tidyverse.org/dev/reference/check_dbplyr.md)
for helpers.

### UTF-8

- Internally, column names are always represented as character vectors,
  and not as language symbols, to avoid encoding problems on Windows
  ([\#1950](https://github.com/tidyverse/dplyr/issues/1950),
  [\#2387](https://github.com/tidyverse/dplyr/issues/2387),
  [\#2388](https://github.com/tidyverse/dplyr/issues/2388)).

- Error messages and explanations of data frame inequality are now
  encoded in UTF-8, also on Windows
  ([\#2441](https://github.com/tidyverse/dplyr/issues/2441)).

- Joins now always reencode character columns to UTF-8 if necessary.
  This gives a nice speedup, because now pointer comparison can be used
  instead of string comparison, but relies on a proper encoding tag for
  all strings
  ([\#2514](https://github.com/tidyverse/dplyr/issues/2514)).

- Fixed problems when joining factor or character encodings with a mix
  of native and UTF-8 encoded values
  ([\#1885](https://github.com/tidyverse/dplyr/issues/1885),
  [\#2118](https://github.com/tidyverse/dplyr/issues/2118),
  [\#2271](https://github.com/tidyverse/dplyr/issues/2271),
  [\#2451](https://github.com/tidyverse/dplyr/issues/2451)).

- Fix
  [`group_by()`](https://dplyr.tidyverse.org/dev/reference/group_by.md)
  for data frames that have UTF-8 encoded names
  ([\#2284](https://github.com/tidyverse/dplyr/issues/2284),
  [\#2382](https://github.com/tidyverse/dplyr/issues/2382)).

- New
  [`group_vars()`](https://dplyr.tidyverse.org/dev/reference/group_data.md)
  generic that returns the grouping as character vector, to avoid the
  potentially lossy conversion to language symbols. The list returned by
  [`group_by_prepare()`](https://dplyr.tidyverse.org/dev/reference/group_by_prepare.md)
  now has a new `group_names` component
  ([\#1950](https://github.com/tidyverse/dplyr/issues/1950),
  [\#2384](https://github.com/tidyverse/dplyr/issues/2384)).

### Colwise functions

- [`rename()`](https://dplyr.tidyverse.org/dev/reference/rename.md),
  [`select()`](https://dplyr.tidyverse.org/dev/reference/select.md),
  [`group_by()`](https://dplyr.tidyverse.org/dev/reference/group_by.md),
  [`filter()`](https://dplyr.tidyverse.org/dev/reference/filter.md),
  [`arrange()`](https://dplyr.tidyverse.org/dev/reference/arrange.md)
  and
  [`transmute()`](https://dplyr.tidyverse.org/dev/reference/transmute.md)
  now have scoped variants (verbs suffixed with `_if()`, `_at()` and
  `_all()`). Like
  [`mutate_all()`](https://dplyr.tidyverse.org/dev/reference/mutate_all.md),
  [`summarise_if()`](https://dplyr.tidyverse.org/dev/reference/summarise_all.md),
  etc, these variants apply an operation to a selection of variables.

- The scoped verbs taking predicates
  ([`mutate_if()`](https://dplyr.tidyverse.org/dev/reference/mutate_all.md),
  [`summarise_if()`](https://dplyr.tidyverse.org/dev/reference/summarise_all.md),
  etc) now support S3 objects and lazy tables. S3 objects should
  implement methods for
  [`length()`](https://rdrr.io/r/base/length.html), `[[` and
  [`tbl_vars()`](https://dplyr.tidyverse.org/dev/reference/tbl_vars.md).
  For lazy tables, the first 100 rows are collected and the predicate is
  applied on this subset of the data. This is robust for the common case
  of checking the type of a column
  ([\#2129](https://github.com/tidyverse/dplyr/issues/2129)).

- Summarise and mutate colwise functions pass `...` on to the
  manipulation functions.

- The performance of colwise verbs like
  [`mutate_all()`](https://dplyr.tidyverse.org/dev/reference/mutate_all.md)
  is now back to where it was in
  [`mutate_each()`](https://dplyr.tidyverse.org/dev/reference/defunct-each.md).

- [`funs()`](https://dplyr.tidyverse.org/dev/reference/funs.md) has
  better handling of namespaced functions
  ([\#2089](https://github.com/tidyverse/dplyr/issues/2089)).

- Fix issue with
  [`mutate_if()`](https://dplyr.tidyverse.org/dev/reference/mutate_all.md)
  and
  [`summarise_if()`](https://dplyr.tidyverse.org/dev/reference/summarise_all.md)
  when a predicate function returns a vector of `FALSE`
  ([\#1989](https://github.com/tidyverse/dplyr/issues/1989),
  [\#2009](https://github.com/tidyverse/dplyr/issues/2009),
  [\#2011](https://github.com/tidyverse/dplyr/issues/2011)).

### Tidyeval

dplyr has a new approach to non-standard evaluation (NSE) called
tidyeval. It is described in detail in
[`vignette("programming")`](https://dplyr.tidyverse.org/dev/articles/programming.md)
but, in brief, gives you the ability to interpolate values in contexts
where dplyr usually works with expressions:

\`\`\`{r} my_var \<- quo(homeworld)

starwars %\>% group_by(!!my_var) %\>% summarise_at(vars(height:mass),
mean, na.rm = TRUE) \`\`\`

This means that the underscored version of each main verb is no longer
needed, and so these functions have been deprecated (but remain around
for backward compatibility).

- [`order_by()`](https://dplyr.tidyverse.org/dev/reference/order_by.md),
  [`top_n()`](https://dplyr.tidyverse.org/dev/reference/top_n.md),
  [`sample_n()`](https://dplyr.tidyverse.org/dev/reference/sample_n.md)
  and
  [`sample_frac()`](https://dplyr.tidyverse.org/dev/reference/sample_n.md)
  now use tidyeval to capture their arguments by expression. This makes
  it possible to use unquoting idioms (see
  [`vignette("programming")`](https://dplyr.tidyverse.org/dev/articles/programming.md))
  and fixes scoping issues
  ([\#2297](https://github.com/tidyverse/dplyr/issues/2297)).

- Most verbs taking dots now ignore the last argument if empty. This
  makes it easier to copy lines of code without having to worry about
  deleting trailing commas
  ([\#1039](https://github.com/tidyverse/dplyr/issues/1039)).

- \[API\] The new `.data` and `.env` environments can be used inside all
  verbs that operate on data: `.data$column_name` accesses the column
  `column_name`, whereas `.env$var` accesses the external variable
  `var`. Columns or external variables named `.data` or `.env` are
  shadowed, use `.data$...` and/or `.env$...` to access them. (`.data`
  implements strict matching also for the `$` operator
  ([\#2591](https://github.com/tidyverse/dplyr/issues/2591)).)

  The [`column()`](https://rdrr.io/pkg/shiny/man/column.html) and
  `global()` functions have been removed. They were never documented
  officially. Use the new `.data` and `.env` environments instead.

- Expressions in verbs are now interpreted correctly in many cases that
  failed before (e.g., use of `$`,
  [`case_when()`](https://dplyr.tidyverse.org/dev/reference/case-and-replace-when.md),
  nonstandard evaluation, …). These expressions are now evaluated in a
  specially constructed temporary environment that retrieves column data
  on demand with the help of the `bindrcpp` package
  ([\#2190](https://github.com/tidyverse/dplyr/issues/2190)). This
  temporary environment poses restrictions on assignments using `<-`
  inside verbs. To prevent leaking of broken bindings, the temporary
  environment is cleared after the evaluation
  ([\#2435](https://github.com/tidyverse/dplyr/issues/2435)).

### Verbs

#### Joins

- \[API\] `xxx_join.tbl_df(na_matches = "never")` treats all `NA` values
  as different from each other (and from any other value), so that they
  never match. This corresponds to the behavior of joins for database
  sources, and of database joins in general. To match `NA` values, pass
  `na_matches = "na"` to the join verbs; this is only supported for data
  frames. The default is `na_matches = "na"`, kept for the sake of
  compatibility to v0.5.0. It can be tweaked by calling
  `pkgconfig::set_config("dplyr::na_matches", "na")`
  ([\#2033](https://github.com/tidyverse/dplyr/issues/2033)).

- [`common_by()`](https://dplyr.tidyverse.org/dev/reference/common_by.md)
  gets a better error message for unexpected inputs
  ([\#2091](https://github.com/tidyverse/dplyr/issues/2091))

- Fix groups when joining grouped data frames with duplicate columns
  ([\#2330](https://github.com/tidyverse/dplyr/issues/2330),
  [\#2334](https://github.com/tidyverse/dplyr/issues/2334),
  [@davidkretch](https://github.com/davidkretch)).

- One of the two join suffixes can now be an empty string, dplyr no
  longer hangs
  ([\#2228](https://github.com/tidyverse/dplyr/issues/2228),
  [\#2445](https://github.com/tidyverse/dplyr/issues/2445)).

- Anti- and semi-joins warn if factor levels are inconsistent
  ([\#2741](https://github.com/tidyverse/dplyr/issues/2741)).

- Warnings about join column inconsistencies now contain the column
  names ([\#2728](https://github.com/tidyverse/dplyr/issues/2728)).

#### Select

- For selecting variables, the first selector decides if it’s an
  inclusive selection (i.e., the initial column list is empty), or an
  exclusive selection (i.e., the initial column list contains all
  columns). This means that
  `select(mtcars, contains("am"), contains("FOO"), contains("vs"))` now
  returns again both `am` and `vs` columns like in dplyr 0.4.3
  ([\#2275](https://github.com/tidyverse/dplyr/issues/2275),
  [\#2289](https://github.com/tidyverse/dplyr/issues/2289),
  [@r2evans](https://github.com/r2evans)).

- Select helpers now throw an error if called when no variables have
  been set ([\#2452](https://github.com/tidyverse/dplyr/issues/2452))

- Helper functions in
  [`select()`](https://dplyr.tidyverse.org/dev/reference/select.md) (and
  related verbs) are now evaluated in a context where column names do
  not exist ([\#2184](https://github.com/tidyverse/dplyr/issues/2184)).

- [`select()`](https://dplyr.tidyverse.org/dev/reference/select.md) (and
  the internal function `select_vars()`) now support column names in
  addition to column positions. As a result, expressions like
  `select(mtcars, "cyl")` are now allowed.

#### Other

- [`recode()`](https://dplyr.tidyverse.org/dev/reference/recode.md),
  [`case_when()`](https://dplyr.tidyverse.org/dev/reference/case-and-replace-when.md)
  and
  [`coalesce()`](https://dplyr.tidyverse.org/dev/reference/coalesce.md)
  now support splicing of arguments with rlang’s `!!!` operator.

- [`count()`](https://dplyr.tidyverse.org/dev/reference/count.md) now
  preserves the grouping of its input
  ([\#2021](https://github.com/tidyverse/dplyr/issues/2021)).

- [`distinct()`](https://dplyr.tidyverse.org/dev/reference/distinct.md)
  no longer duplicates variables
  ([\#2001](https://github.com/tidyverse/dplyr/issues/2001)).

- Empty
  [`distinct()`](https://dplyr.tidyverse.org/dev/reference/distinct.md)
  with a grouped data frame works the same way as an empty
  [`distinct()`](https://dplyr.tidyverse.org/dev/reference/distinct.md)
  on an ungrouped data frame, namely it uses all variables
  ([\#2476](https://github.com/tidyverse/dplyr/issues/2476)).

- [`copy_to()`](https://dplyr.tidyverse.org/dev/reference/copy_to.md)
  now returns its output invisibly (since you’re often just calling for
  the side-effect).

- [`filter()`](https://dplyr.tidyverse.org/dev/reference/filter.md) and
  [`lag()`](https://dplyr.tidyverse.org/dev/reference/lead-lag.md) throw
  informative error if used with ts objects
  ([\#2219](https://github.com/tidyverse/dplyr/issues/2219))

- [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md)
  recycles list columns of length 1
  ([\#2171](https://github.com/tidyverse/dplyr/issues/2171)).

- [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md)
  gives better error message when attempting to add a non-vector column
  ([\#2319](https://github.com/tidyverse/dplyr/issues/2319)), or
  attempting to remove a column with `NULL`
  ([\#2187](https://github.com/tidyverse/dplyr/issues/2187),
  [\#2439](https://github.com/tidyverse/dplyr/issues/2439)).

- [`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)
  now correctly evaluates newly created factors
  ([\#2217](https://github.com/tidyverse/dplyr/issues/2217)), and can
  create ordered factors
  ([\#2200](https://github.com/tidyverse/dplyr/issues/2200)).

- Ungrouped
  [`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)
  uses summary variables correctly
  ([\#2404](https://github.com/tidyverse/dplyr/issues/2404),
  [\#2453](https://github.com/tidyverse/dplyr/issues/2453)).

- Grouped
  [`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)
  no longer converts character `NA` to empty strings
  ([\#1839](https://github.com/tidyverse/dplyr/issues/1839)).

### Combining and comparing

- [`all_equal()`](https://dplyr.tidyverse.org/dev/reference/all_equal.md)
  now reports multiple problems as a character vector
  ([\#1819](https://github.com/tidyverse/dplyr/issues/1819),
  [\#2442](https://github.com/tidyverse/dplyr/issues/2442)).

- [`all_equal()`](https://dplyr.tidyverse.org/dev/reference/all_equal.md)
  checks that factor levels are equal
  ([\#2440](https://github.com/tidyverse/dplyr/issues/2440),
  [\#2442](https://github.com/tidyverse/dplyr/issues/2442)).

- [`bind_rows()`](https://dplyr.tidyverse.org/dev/reference/bind_rows.md)
  and
  [`bind_cols()`](https://dplyr.tidyverse.org/dev/reference/bind_cols.md)
  give an error for database tables
  ([\#2373](https://github.com/tidyverse/dplyr/issues/2373)).

- [`bind_rows()`](https://dplyr.tidyverse.org/dev/reference/bind_rows.md)
  works correctly with `NULL` arguments and an `.id` argument
  ([\#2056](https://github.com/tidyverse/dplyr/issues/2056)), and also
  for zero-column data frames
  ([\#2175](https://github.com/tidyverse/dplyr/issues/2175)).

- Breaking change:
  [`bind_rows()`](https://dplyr.tidyverse.org/dev/reference/bind_rows.md)
  and
  [`combine()`](https://dplyr.tidyverse.org/dev/reference/defunct.md)
  are more strict when coercing. Logical values are no longer coerced to
  integer and numeric. Date, POSIXct and other integer or double-based
  classes are no longer coerced to integer or double as there is chance
  of attributes or information being lost
  ([\#2209](https://github.com/tidyverse/dplyr/issues/2209),
  [@zeehio](https://github.com/zeehio)).

- [`bind_cols()`](https://dplyr.tidyverse.org/dev/reference/bind_cols.md)
  now calls
  [`tibble::repair_names()`](https://tibble.tidyverse.org/reference/name-repair-superseded.html)
  to ensure that all names are unique
  ([\#2248](https://github.com/tidyverse/dplyr/issues/2248)).

- [`bind_cols()`](https://dplyr.tidyverse.org/dev/reference/bind_cols.md)
  handles empty argument list
  ([\#2048](https://github.com/tidyverse/dplyr/issues/2048)).

- [`bind_cols()`](https://dplyr.tidyverse.org/dev/reference/bind_cols.md)
  better handles `NULL` inputs
  ([\#2303](https://github.com/tidyverse/dplyr/issues/2303),
  [\#2443](https://github.com/tidyverse/dplyr/issues/2443)).

- [`bind_rows()`](https://dplyr.tidyverse.org/dev/reference/bind_rows.md)
  explicitly rejects columns containing data frames
  ([\#2015](https://github.com/tidyverse/dplyr/issues/2015),
  [\#2446](https://github.com/tidyverse/dplyr/issues/2446)).

- [`bind_rows()`](https://dplyr.tidyverse.org/dev/reference/bind_rows.md)
  and
  [`bind_cols()`](https://dplyr.tidyverse.org/dev/reference/bind_cols.md)
  now accept vectors. They are treated as rows by the former and columns
  by the latter. Rows require inner names like `c(col1 = 1, col2 = 2)`,
  while columns require outer names: `col1 = c(1, 2)`. Lists are still
  treated as data frames but can be spliced explicitly with `!!!`,
  e.g. `bind_rows(!!! x)`
  ([\#1676](https://github.com/tidyverse/dplyr/issues/1676)).

- `rbind_list()` and `rbind_all()` now call
  [`.Deprecated()`](https://rdrr.io/r/base/Deprecated.html), they will
  be removed in the next CRAN release. Please use
  [`bind_rows()`](https://dplyr.tidyverse.org/dev/reference/bind_rows.md)
  instead.

- [`combine()`](https://dplyr.tidyverse.org/dev/reference/defunct.md)
  accepts `NA` values
  ([\#2203](https://github.com/tidyverse/dplyr/issues/2203),
  [@zeehio](https://github.com/zeehio))

- [`combine()`](https://dplyr.tidyverse.org/dev/reference/defunct.md)
  and
  [`bind_rows()`](https://dplyr.tidyverse.org/dev/reference/bind_rows.md)
  with character and factor types now always warn about the coercion to
  character ([\#2317](https://github.com/tidyverse/dplyr/issues/2317),
  [@zeehio](https://github.com/zeehio))

- [`combine()`](https://dplyr.tidyverse.org/dev/reference/defunct.md)
  and
  [`bind_rows()`](https://dplyr.tidyverse.org/dev/reference/bind_rows.md)
  accept `difftime` objects.

- `mutate` coerces results from grouped dataframes accepting combinable
  data types (such as `integer` and `numeric`).
  ([\#1892](https://github.com/tidyverse/dplyr/issues/1892),
  [@zeehio](https://github.com/zeehio))

### Vector functions

- `%in%` gets new hybrid handler
  ([\#126](https://github.com/tidyverse/dplyr/issues/126)).

- [`between()`](https://dplyr.tidyverse.org/dev/reference/between.md)
  returns NA if `left` or `right` is `NA` (fixes
  [\#2562](https://github.com/tidyverse/dplyr/issues/2562)).

- [`case_when()`](https://dplyr.tidyverse.org/dev/reference/case-and-replace-when.md)
  supports `NA` values
  ([\#2000](https://github.com/tidyverse/dplyr/issues/2000),
  [@tjmahr](https://github.com/tjmahr)).

- [`first()`](https://dplyr.tidyverse.org/dev/reference/nth.md),
  [`last()`](https://dplyr.tidyverse.org/dev/reference/nth.md), and
  [`nth()`](https://dplyr.tidyverse.org/dev/reference/nth.md) have
  better default values for factor, Dates, POSIXct, and data frame
  inputs ([\#2029](https://github.com/tidyverse/dplyr/issues/2029)).

- Fixed segmentation faults in hybrid evaluation of
  [`first()`](https://dplyr.tidyverse.org/dev/reference/nth.md),
  [`last()`](https://dplyr.tidyverse.org/dev/reference/nth.md),
  [`nth()`](https://dplyr.tidyverse.org/dev/reference/nth.md),
  [`lead()`](https://dplyr.tidyverse.org/dev/reference/lead-lag.md), and
  [`lag()`](https://dplyr.tidyverse.org/dev/reference/lead-lag.md).
  These functions now always fall back to the R implementation if called
  with arguments that the hybrid evaluator cannot handle
  ([\#948](https://github.com/tidyverse/dplyr/issues/948),
  [\#1980](https://github.com/tidyverse/dplyr/issues/1980)).

- [`n_distinct()`](https://dplyr.tidyverse.org/dev/reference/n_distinct.md)
  gets larger hash tables given slightly better performance
  ([\#977](https://github.com/tidyverse/dplyr/issues/977)).

- [`nth()`](https://dplyr.tidyverse.org/dev/reference/nth.md) and
  [`ntile()`](https://dplyr.tidyverse.org/dev/reference/ntile.md) are
  more careful about proper data types of their return values
  ([\#2306](https://github.com/tidyverse/dplyr/issues/2306)).

- [`ntile()`](https://dplyr.tidyverse.org/dev/reference/ntile.md)
  ignores `NA` when computing group membership
  ([\#2564](https://github.com/tidyverse/dplyr/issues/2564)).

- [`lag()`](https://dplyr.tidyverse.org/dev/reference/lead-lag.md)
  enforces integer `n`
  ([\#2162](https://github.com/tidyverse/dplyr/issues/2162),
  [@kevinushey](https://github.com/kevinushey)).

- hybrid [`min()`](https://rdrr.io/r/base/Extremes.html) and
  [`max()`](https://rdrr.io/r/base/Extremes.html) now always return a
  `numeric` and work correctly in edge cases (empty input, all `NA`, …)
  ([\#2305](https://github.com/tidyverse/dplyr/issues/2305),
  [\#2436](https://github.com/tidyverse/dplyr/issues/2436)).

- `min_rank("string")` no longer segfaults in hybrid evaluation
  ([\#2279](https://github.com/tidyverse/dplyr/issues/2279),
  [\#2444](https://github.com/tidyverse/dplyr/issues/2444)).

- [`recode()`](https://dplyr.tidyverse.org/dev/reference/recode.md) can
  now recode a factor to other types
  ([\#2268](https://github.com/tidyverse/dplyr/issues/2268))

- [`recode()`](https://dplyr.tidyverse.org/dev/reference/recode.md)
  gains `.dots` argument to support passing replacements as list
  ([\#2110](https://github.com/tidyverse/dplyr/issues/2110),
  [@jlegewie](https://github.com/jlegewie)).

### Other minor changes and bug fixes

- Many error messages are more helpful by referring to a column name or
  a position in the argument list
  ([\#2448](https://github.com/tidyverse/dplyr/issues/2448)).

- New
  [`is_grouped_df()`](https://dplyr.tidyverse.org/dev/reference/grouped_df.md)
  alias to
  [`is.grouped_df()`](https://dplyr.tidyverse.org/dev/reference/grouped_df.md).

- [`tbl_vars()`](https://dplyr.tidyverse.org/dev/reference/tbl_vars.md)
  now has a `group_vars` argument set to `TRUE` by default. If `FALSE`,
  group variables are not returned.

- Fixed segmentation fault after calling
  [`rename()`](https://dplyr.tidyverse.org/dev/reference/rename.md) on
  an invalid grouped data frame
  ([\#2031](https://github.com/tidyverse/dplyr/issues/2031)).

- `rename_vars()` gains a `strict` argument to control if an error is
  thrown when you try and rename a variable that doesn’t exist.

- Fixed undefined behavior for
  [`slice()`](https://dplyr.tidyverse.org/dev/reference/slice.md) on a
  zero-column data frame
  ([\#2490](https://github.com/tidyverse/dplyr/issues/2490)).

- Fixed very rare case of false match during join
  ([\#2515](https://github.com/tidyverse/dplyr/issues/2515)).

- Restricted workaround for
  [`match()`](https://rdrr.io/r/base/match.html) to R 3.3.0.
  ([\#1858](https://github.com/tidyverse/dplyr/issues/1858)).

- dplyr now warns on load when the version of R or Rcpp during
  installation is different to the currently installed version
  ([\#2514](https://github.com/tidyverse/dplyr/issues/2514)).

- Fixed improper reuse of attributes when creating a list column in
  [`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)
  and perhaps
  [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md)
  ([\#2231](https://github.com/tidyverse/dplyr/issues/2231)).

- [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md) and
  [`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)
  always strip the `names` attribute from new or updated columns, even
  for ungrouped operations
  ([\#1689](https://github.com/tidyverse/dplyr/issues/1689)).

- Fixed rare error that could lead to a segmentation fault in
  `all_equal(ignore_col_order = FALSE)`
  ([\#2502](https://github.com/tidyverse/dplyr/issues/2502)).

- The “dim” and “dimnames” attributes are always stripped when copying a
  vector ([\#1918](https://github.com/tidyverse/dplyr/issues/1918),
  [\#2049](https://github.com/tidyverse/dplyr/issues/2049)).

- `grouped_df` and `rowwise` are registered officially as S3 classes.
  This makes them easier to use with S4
  ([\#2276](https://github.com/tidyverse/dplyr/issues/2276),
  [@joranE](https://github.com/joranE),
  [\#2789](https://github.com/tidyverse/dplyr/issues/2789)).

- All operations that return tibbles now include the `"tbl"` class. This
  is important for correct printing with tibble 1.3.1
  ([\#2789](https://github.com/tidyverse/dplyr/issues/2789)).

- Makeflags uses PKG_CPPFLAGS for defining preprocessor macros.

- astyle formatting for C++ code, tested but not changed as part of the
  tests ([\#2086](https://github.com/tidyverse/dplyr/issues/2086),
  [\#2103](https://github.com/tidyverse/dplyr/issues/2103)).

- Update RStudio project settings to install tests
  ([\#1952](https://github.com/tidyverse/dplyr/issues/1952)).

- Using `Rcpp::interfaces()` to register C callable interfaces, and
  registering all native exported functions via `R_registerRoutines()`
  and `useDynLib(.registration = TRUE)`
  ([\#2146](https://github.com/tidyverse/dplyr/issues/2146)).

- Formatting of grouped data frames now works by overriding the
  [`tbl_sum()`](https://pillar.r-lib.org/reference/tbl_sum.html) generic
  instead of [`print()`](https://rdrr.io/r/base/print.html). This means
  that the output is more consistent with tibble, and that
  [`format()`](https://rdrr.io/r/base/format.html) is now supported also
  for SQL sources
  ([\#2781](https://github.com/tidyverse/dplyr/issues/2781)).

## dplyr 0.5.0

CRAN release: 2016-06-24

### Breaking changes

#### Existing functions

- [`arrange()`](https://dplyr.tidyverse.org/dev/reference/arrange.md)
  once again ignores grouping
  ([\#1206](https://github.com/tidyverse/dplyr/issues/1206)).

- [`distinct()`](https://dplyr.tidyverse.org/dev/reference/distinct.md)
  now only keeps the distinct variables. If you want to return all
  variables (using the first row for non-distinct values) use
  `.keep_all = TRUE`
  ([\#1110](https://github.com/tidyverse/dplyr/issues/1110)). For SQL
  sources, `.keep_all = FALSE` is implemented using `GROUP BY`, and
  `.keep_all = TRUE` raises an error
  ([\#1937](https://github.com/tidyverse/dplyr/issues/1937),
  [\#1942](https://github.com/tidyverse/dplyr/issues/1942),
  [@krlmlr](https://github.com/krlmlr)). (The default behaviour of using
  all variables when none are specified remains - this note only applies
  if you select some variables).

- The select helper functions
  [`starts_with()`](https://tidyselect.r-lib.org/reference/starts_with.html),
  [`ends_with()`](https://tidyselect.r-lib.org/reference/starts_with.html)
  etc are now real exported functions. This means that you’ll need to
  import those functions if you’re using from a package where dplyr is
  not attached. i.e. `dplyr::select(mtcars, starts_with("m"))` used to
  work, but now you’ll need
  `dplyr::select(mtcars, dplyr::starts_with("m"))`.

#### Deprecated and defunct functions

- The long deprecated `chain()`, `chain_q()` and `%.%` have been
  removed. Please use `%>%` instead.

- `id()` has been deprecated. Please use
  [`group_indices()`](https://dplyr.tidyverse.org/dev/reference/group_data.md)
  instead ([\#808](https://github.com/tidyverse/dplyr/issues/808)).

- `rbind_all()` and `rbind_list()` are formally deprecated. Please use
  [`bind_rows()`](https://dplyr.tidyverse.org/dev/reference/bind_rows.md)
  instead ([\#803](https://github.com/tidyverse/dplyr/issues/803)).

- Outdated benchmarking demos have been removed
  ([\#1487](https://github.com/tidyverse/dplyr/issues/1487)).

- Code related to starting and signalling clusters has been moved out to
  [multidplyr](https://github.com/tidyverse/multidplyr).

### New functions

- [`coalesce()`](https://dplyr.tidyverse.org/dev/reference/coalesce.md)
  finds the first non-missing value from a set of vectors.
  ([\#1666](https://github.com/tidyverse/dplyr/issues/1666), thanks to
  [@krlmlr](https://github.com/krlmlr) for initial implementation).

- [`case_when()`](https://dplyr.tidyverse.org/dev/reference/case-and-replace-when.md)
  is a general vectorised if + else if
  ([\#631](https://github.com/tidyverse/dplyr/issues/631)).

- [`if_else()`](https://dplyr.tidyverse.org/dev/reference/if_else.md) is
  a vectorised if statement: it’s a stricter (type-safe), faster, and
  more predictable version of
  [`ifelse()`](https://rdrr.io/r/base/ifelse.html). In SQL it is
  translated to a `CASE` statement.

- [`na_if()`](https://dplyr.tidyverse.org/dev/reference/na_if.md) makes
  it easy to replace a certain value with an `NA`
  ([\#1707](https://github.com/tidyverse/dplyr/issues/1707)). In SQL it
  is translated to `NULL_IF`.

- `near(x, y)` is a helper for `abs(x - y) < tol`
  ([\#1607](https://github.com/tidyverse/dplyr/issues/1607)).

- [`recode()`](https://dplyr.tidyverse.org/dev/reference/recode.md) is
  vectorised equivalent to
  [`switch()`](https://rdrr.io/r/base/switch.html)
  ([\#1710](https://github.com/tidyverse/dplyr/issues/1710)).

- [`union_all()`](https://dplyr.tidyverse.org/dev/reference/setops.md)
  method. Maps to `UNION ALL` for SQL sources,
  [`bind_rows()`](https://dplyr.tidyverse.org/dev/reference/bind_rows.md)
  for data frames/tbl_dfs, and
  [`combine()`](https://dplyr.tidyverse.org/dev/reference/defunct.md)
  for vectors
  ([\#1045](https://github.com/tidyverse/dplyr/issues/1045)).

- A new family of functions replace
  [`summarise_each()`](https://dplyr.tidyverse.org/dev/reference/defunct-each.md)
  and
  [`mutate_each()`](https://dplyr.tidyverse.org/dev/reference/defunct-each.md)
  (which will thus be deprecated in a future release).
  [`summarise_all()`](https://dplyr.tidyverse.org/dev/reference/summarise_all.md)
  and
  [`mutate_all()`](https://dplyr.tidyverse.org/dev/reference/mutate_all.md)
  apply a function to all columns while
  [`summarise_at()`](https://dplyr.tidyverse.org/dev/reference/summarise_all.md)
  and
  [`mutate_at()`](https://dplyr.tidyverse.org/dev/reference/mutate_all.md)
  operate on a subset of columns. These columns are selected with either
  a character vector of columns names, a numeric vector of column
  positions, or a column specification with
  [`select()`](https://dplyr.tidyverse.org/dev/reference/select.md)
  semantics generated by the new `columns()` helper. In addition,
  [`summarise_if()`](https://dplyr.tidyverse.org/dev/reference/summarise_all.md)
  and
  [`mutate_if()`](https://dplyr.tidyverse.org/dev/reference/mutate_all.md)
  take a predicate function or a logical vector (these verbs currently
  require local sources). All these functions can now take ordinary
  functions instead of a list of functions generated by
  [`funs()`](https://dplyr.tidyverse.org/dev/reference/funs.md) (though
  this is only useful for local sources).
  ([\#1845](https://github.com/tidyverse/dplyr/issues/1845),
  [@lionel-](https://github.com/lionel-))

- [`select_if()`](https://dplyr.tidyverse.org/dev/reference/select_all.md)
  lets you select columns with a predicate function. Only compatible
  with local sources.
  ([\#497](https://github.com/tidyverse/dplyr/issues/497),
  [\#1569](https://github.com/tidyverse/dplyr/issues/1569),
  [@lionel-](https://github.com/lionel-))

### Local backends

#### dtplyr

All data table related code has been separated out in to a new dtplyr
package. This decouples the development of the data.table interface from
the development of the dplyr package. If both data.table and dplyr are
loaded, you’ll get a message reminding you to load dtplyr.

#### Tibble

Functions related to the creation and coercion of `tbl_df`s, now live in
their own package: [tibble](https://posit.co/blog/tibble-1-0-0/). See
[`vignette("tibble")`](https://tibble.tidyverse.org/articles/tibble.html)
for more details.

- `$` and `[[` methods that never do partial matching
  ([\#1504](https://github.com/tidyverse/dplyr/issues/1504)), and throw
  an error if the variable does not exist.

- [`all_equal()`](https://dplyr.tidyverse.org/dev/reference/all_equal.md)
  allows to compare data frames ignoring row and column order, and
  optionally ignoring minor differences in type (e.g. int vs. double)
  ([\#821](https://github.com/tidyverse/dplyr/issues/821)). The test
  handles the case where the df has 0 columns
  ([\#1506](https://github.com/tidyverse/dplyr/issues/1506)). The test
  fails fails when convert is `FALSE` and types don’t match
  ([\#1484](https://github.com/tidyverse/dplyr/issues/1484)).

- [`all_equal()`](https://dplyr.tidyverse.org/dev/reference/all_equal.md)
  shows better error message when comparing raw values or when types are
  incompatible and `convert = TRUE`
  ([\#1820](https://github.com/tidyverse/dplyr/issues/1820),
  [@krlmlr](https://github.com/krlmlr)).

- [`add_row()`](https://tibble.tidyverse.org/reference/add_row.html)
  makes it easy to add a new row to data frame
  ([\#1021](https://github.com/tidyverse/dplyr/issues/1021))

- [`as_data_frame()`](https://tibble.tidyverse.org/reference/deprecated.html)
  is now an S3 generic with methods for lists (the old
  [`as_data_frame()`](https://tibble.tidyverse.org/reference/deprecated.html)),
  data frames (trivial), and matrices (with efficient C++
  implementation)
  ([\#876](https://github.com/tidyverse/dplyr/issues/876)). It no longer
  strips subclasses.

- The internals of
  [`data_frame()`](https://tibble.tidyverse.org/reference/deprecated.html)
  and
  [`as_data_frame()`](https://tibble.tidyverse.org/reference/deprecated.html)
  have been aligned, so
  [`as_data_frame()`](https://tibble.tidyverse.org/reference/deprecated.html)
  will now automatically recycle length-1 vectors. Both functions give
  more informative error messages if you attempting to create an invalid
  data frame. You can no longer create a data frame with duplicated
  names ([\#820](https://github.com/tidyverse/dplyr/issues/820)). Both
  check for `POSIXlt` columns, and tell you to use `POSIXct` instead
  ([\#813](https://github.com/tidyverse/dplyr/issues/813)).

- [`frame_data()`](https://tibble.tidyverse.org/reference/deprecated.html)
  properly constructs rectangular tables
  ([\#1377](https://github.com/tidyverse/dplyr/issues/1377),
  [@kevinushey](https://github.com/kevinushey)), and supports list-cols.

- [`glimpse()`](https://dplyr.tidyverse.org/dev/reference/glimpse.md) is
  now a generic. The default method dispatches to
  [`str()`](https://rdrr.io/r/utils/str.html)
  ([\#1325](https://github.com/tidyverse/dplyr/issues/1325)). It now
  (invisibly) returns its first argument
  ([\#1570](https://github.com/tidyverse/dplyr/issues/1570)).

- [`lst()`](https://tibble.tidyverse.org/reference/lst.html) and
  [`lst_()`](https://tibble.tidyverse.org/reference/deprecated.html)
  which create lists in the same way that
  [`data_frame()`](https://tibble.tidyverse.org/reference/deprecated.html)
  and
  [`data_frame_()`](https://tibble.tidyverse.org/reference/deprecated.html)
  create data frames
  ([\#1290](https://github.com/tidyverse/dplyr/issues/1290)).

- `print.tbl_df()` is considerably faster if you have very wide data
  frames. It will now also only list the first 100 additional variables
  not already on screen - control this with the new `n_extra` parameter
  to [`print()`](https://rdrr.io/r/base/print.html)
  ([\#1161](https://github.com/tidyverse/dplyr/issues/1161)). When
  printing a grouped data frame the number of groups is now printed with
  thousands separators
  ([\#1398](https://github.com/tidyverse/dplyr/issues/1398)). The type
  of list columns is correctly printed
  ([\#1379](https://github.com/tidyverse/dplyr/issues/1379))

- Package includes `setOldClass(c("tbl_df", "tbl", "data.frame"))` to
  help with S4 dispatch
  ([\#969](https://github.com/tidyverse/dplyr/issues/969)).

- `tbl_df` automatically generates column names
  ([\#1606](https://github.com/tidyverse/dplyr/issues/1606)).

#### tbl_cube

- new `as_data_frame.tbl_cube()`
  ([\#1563](https://github.com/tidyverse/dplyr/issues/1563),
  [@krlmlr](https://github.com/krlmlr)).

- `tbl_cube`s are now constructed correctly from data frames, duplicate
  dimension values are detected, missing dimension values are filled
  with `NA`. The construction from data frames now guesses the measure
  variables by default, and allows specification of dimension and/or
  measure variables
  ([\#1568](https://github.com/tidyverse/dplyr/issues/1568),
  [@krlmlr](https://github.com/krlmlr)).

- Swap order of `dim_names` and `met_name` arguments in `as.tbl_cube`
  (for `array`, `table` and `matrix`) for consistency with `tbl_cube`
  and `as.tbl_cube.data.frame`. Also, the `met_name` argument to
  `as.tbl_cube.table` now defaults to `"Freq"` for consistency with
  `as.data.frame.table` ([@krlmlr](https://github.com/krlmlr),
  [\#1374](https://github.com/tidyverse/dplyr/issues/1374)).

### Remote backends

- [`as_data_frame()`](https://tibble.tidyverse.org/reference/deprecated.html)
  on SQL sources now returns all rows
  ([\#1752](https://github.com/tidyverse/dplyr/issues/1752),
  [\#1821](https://github.com/tidyverse/dplyr/issues/1821),
  [@krlmlr](https://github.com/krlmlr)).

- [`compute()`](https://dplyr.tidyverse.org/dev/reference/compute.md)
  gets new parameters `indexes` and `unique_indexes` that make it easier
  to add indexes
  ([\#1499](https://github.com/tidyverse/dplyr/issues/1499),
  [@krlmlr](https://github.com/krlmlr)).

- [`db_explain()`](https://dplyr.tidyverse.org/dev/reference/backend_dbplyr.md)
  gains a default method for DBIConnections
  ([\#1177](https://github.com/tidyverse/dplyr/issues/1177)).

- The backend testing system has been improved. This lead to the removal
  of `temp_srcs()`. In the unlikely event that you were using this
  function, you can instead use `test_register_src()`, `test_load()`,
  and `test_frame()`.

- You can now use
  [`right_join()`](https://dplyr.tidyverse.org/dev/reference/mutate-joins.md)
  and
  [`full_join()`](https://dplyr.tidyverse.org/dev/reference/mutate-joins.md)
  with remote tables
  ([\#1172](https://github.com/tidyverse/dplyr/issues/1172)).

#### SQLite

- `src_memdb()` is a session-local in-memory SQLite database.
  `memdb_frame()` works like
  [`data_frame()`](https://tibble.tidyverse.org/reference/deprecated.html),
  but creates a new table in that database.

- [`src_sqlite()`](https://dplyr.tidyverse.org/dev/reference/defunct.md)
  now uses a stricter quoting character, `` ` ``, instead of `"`. SQLite
  “helpfully” will convert `"x"` into a string if there is no identifier
  called x in the current scope
  ([\#1426](https://github.com/tidyverse/dplyr/issues/1426)).

- [`src_sqlite()`](https://dplyr.tidyverse.org/dev/reference/defunct.md)
  throws errors if you try and use it with window functions
  ([\#907](https://github.com/tidyverse/dplyr/issues/907)).

#### SQL translation

- `filter.tbl_sql()` now puts parens around each argument
  ([\#934](https://github.com/tidyverse/dplyr/issues/934)).

- Unary `-` is better translated
  ([\#1002](https://github.com/tidyverse/dplyr/issues/1002)).

- `escape.POSIXt()` method makes it easier to use date times. The date
  is rendered in ISO 8601 format in UTC, which should work in most
  databases ([\#857](https://github.com/tidyverse/dplyr/issues/857)).

- [`is.na()`](https://rdrr.io/r/base/NA.html) gets a missing space
  ([\#1695](https://github.com/tidyverse/dplyr/issues/1695)).

- `if`, [`is.na()`](https://rdrr.io/r/base/NA.html), and
  [`is.null()`](https://rdrr.io/r/base/NULL.html) get extra parens to
  make precedence more clear
  ([\#1695](https://github.com/tidyverse/dplyr/issues/1695)).

- [`pmin()`](https://rdrr.io/r/base/Extremes.html) and
  [`pmax()`](https://rdrr.io/r/base/Extremes.html) are translated to
  `MIN()` and `MAX()`
  ([\#1711](https://github.com/tidyverse/dplyr/issues/1711)).

- Window functions:

  - Work on ungrouped data
    ([\#1061](https://github.com/tidyverse/dplyr/issues/1061)).

  - Warning if order is not set on cumulative window functions.

  - Multiple partitions or ordering variables in windowed functions no
    longer generate extra parentheses, so should work for more databases
    ([\#1060](https://github.com/tidyverse/dplyr/issues/1060))

#### Internals

This version includes an almost total rewrite of how dplyr verbs are
translated into SQL. Previously, I used a rather ad-hoc approach, which
tried to guess when a new subquery was needed. Unfortunately this
approach was fraught with bugs, so in this version I’ve implemented a
much richer internal data model. Now there is a three step process:

1.  When applied to a `tbl_lazy`, each dplyr verb captures its inputs
    and stores in a `op` (short for operation) object.

2.  `sql_build()` iterates through the operations building to build up
    an object that represents a SQL query. These objects are convenient
    for testing as they are lists, and are backend agnostics.

3.  `sql_render()` iterates through the queries and generates the SQL,
    using generics (like
    [`sql_select()`](https://dplyr.tidyverse.org/dev/reference/backend_dbplyr.md))
    that can vary based on the backend.

In the short-term, this increased abstraction is likely to lead to some
minor performance decreases, but the chance of dplyr generating correct
SQL is much much higher. In the long-term, these abstractions will make
it possible to write a query optimiser/compiler in dplyr, which would
make it possible to generate much more succinct queries.

If you have written a dplyr backend, you’ll need to make some minor
changes to your package:

- [`sql_join()`](https://dplyr.tidyverse.org/dev/reference/backend_dbplyr.md)
  has been considerably simplified - it is now only responsible for
  generating the join query, not for generating the intermediate selects
  that rename the variable. Similarly for
  [`sql_semi_join()`](https://dplyr.tidyverse.org/dev/reference/backend_dbplyr.md).
  If you’ve provided new methods in your backend, you’ll need to
  rewrite.

- `select_query()` gains a distinct argument which is used for
  generating queries for
  [`distinct()`](https://dplyr.tidyverse.org/dev/reference/distinct.md).
  It loses the `offset` argument which was never used (and hence never
  tested).

- `src_translate_env()` has been replaced by
  [`sql_translate_env()`](https://dplyr.tidyverse.org/dev/reference/backend_dbplyr.md)
  which should have methods for the connection object.

There were two other tweaks to the exported API, but these are less
likely to affect anyone.

- `translate_sql()` and `partial_eval()` got a new API: now use
  connection + variable names, rather than a `tbl`. This makes testing
  considerably easier. `translate_sql_q()` has been renamed to
  `translate_sql_()`.

- Also note that the sql generation generics now have a default method,
  instead methods for DBIConnection and NULL.

### Minor improvements and bug fixes

#### Single table verbs

- Avoiding segfaults in presence of `raw` columns
  ([\#1803](https://github.com/tidyverse/dplyr/issues/1803),
  [\#1817](https://github.com/tidyverse/dplyr/issues/1817),
  [@krlmlr](https://github.com/krlmlr)).

- [`arrange()`](https://dplyr.tidyverse.org/dev/reference/arrange.md)
  fails gracefully on list columns
  ([\#1489](https://github.com/tidyverse/dplyr/issues/1489)) and
  matrices ([\#1870](https://github.com/tidyverse/dplyr/issues/1870),
  [\#1945](https://github.com/tidyverse/dplyr/issues/1945),
  [@krlmlr](https://github.com/krlmlr)).

- [`count()`](https://dplyr.tidyverse.org/dev/reference/count.md) now
  adds additional grouping variables, rather than overriding existing
  ([\#1703](https://github.com/tidyverse/dplyr/issues/1703)).
  [`tally()`](https://dplyr.tidyverse.org/dev/reference/count.md) and
  [`count()`](https://dplyr.tidyverse.org/dev/reference/count.md) can
  now count a variable called `n`
  ([\#1633](https://github.com/tidyverse/dplyr/issues/1633)). Weighted
  [`count()`](https://dplyr.tidyverse.org/dev/reference/count.md)/[`tally()`](https://dplyr.tidyverse.org/dev/reference/count.md)
  ignore `NA`s
  ([\#1145](https://github.com/tidyverse/dplyr/issues/1145)).

- The progress bar in
  [`do()`](https://dplyr.tidyverse.org/dev/reference/do.md) is now
  updated at most 20 times per second, avoiding unnecessary redraws
  ([\#1734](https://github.com/tidyverse/dplyr/issues/1734),
  [@mkuhn](https://github.com/mkuhn))

- [`distinct()`](https://dplyr.tidyverse.org/dev/reference/distinct.md)
  doesn’t crash when given a 0-column data frame
  ([\#1437](https://github.com/tidyverse/dplyr/issues/1437)).

- [`filter()`](https://dplyr.tidyverse.org/dev/reference/filter.md)
  throws an error if you supply an named arguments. This is usually a
  type: `filter(df, x = 1)` instead of `filter(df, x == 1)`
  ([\#1529](https://github.com/tidyverse/dplyr/issues/1529)).

- [`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)
  correctly coerces factors with different levels
  ([\#1678](https://github.com/tidyverse/dplyr/issues/1678)), handles
  min/max of already summarised variable
  ([\#1622](https://github.com/tidyverse/dplyr/issues/1622)), and
  supports data frames as columns
  ([\#1425](https://github.com/tidyverse/dplyr/issues/1425)).

- [`select()`](https://dplyr.tidyverse.org/dev/reference/select.md) now
  informs you that it adds missing grouping variables
  ([\#1511](https://github.com/tidyverse/dplyr/issues/1511)). It works
  even if the grouping variable has a non-syntactic name
  ([\#1138](https://github.com/tidyverse/dplyr/issues/1138)). Negating a
  failed match (e.g. `select(mtcars, -contains("x"))`) returns all
  columns, instead of no columns
  ([\#1176](https://github.com/tidyverse/dplyr/issues/1176))

  The [`select()`](https://dplyr.tidyverse.org/dev/reference/select.md)
  helpers are now exported and have their own documentation
  ([\#1410](https://github.com/tidyverse/dplyr/issues/1410)).
  [`one_of()`](https://tidyselect.r-lib.org/reference/one_of.html) gives
  a useful error message if variables names are not found in data frame
  ([\#1407](https://github.com/tidyverse/dplyr/issues/1407)).

- The naming behaviour of
  [`summarise_each()`](https://dplyr.tidyverse.org/dev/reference/defunct-each.md)
  and
  [`mutate_each()`](https://dplyr.tidyverse.org/dev/reference/defunct-each.md)
  has been tweaked so that you can force inclusion of both the function
  and the variable name:
  `summarise_each(mtcars, funs(mean = mean), everything())`
  ([\#442](https://github.com/tidyverse/dplyr/issues/442)).

- [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md)
  handles factors that are all `NA`
  ([\#1645](https://github.com/tidyverse/dplyr/issues/1645)), or have
  different levels in different groups
  ([\#1414](https://github.com/tidyverse/dplyr/issues/1414)). It
  disambiguates `NA` and `NaN`
  ([\#1448](https://github.com/tidyverse/dplyr/issues/1448)), and
  silently promotes groups that only contain `NA`
  ([\#1463](https://github.com/tidyverse/dplyr/issues/1463)). It deep
  copies data in list columns
  ([\#1643](https://github.com/tidyverse/dplyr/issues/1643)), and
  correctly fails on incompatible columns
  ([\#1641](https://github.com/tidyverse/dplyr/issues/1641)).
  [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md) on a
  grouped data no longer groups grouping attributes
  ([\#1120](https://github.com/tidyverse/dplyr/issues/1120)).
  [`rowwise()`](https://dplyr.tidyverse.org/dev/reference/rowwise.md)
  mutate gives expected results
  ([\#1381](https://github.com/tidyverse/dplyr/issues/1381)).

- [`one_of()`](https://tidyselect.r-lib.org/reference/one_of.html)
  tolerates unknown variables in `vars`, but warns
  ([\#1848](https://github.com/tidyverse/dplyr/issues/1848),
  [@jennybc](https://github.com/jennybc)).

- `print.grouped_df()` passes on `...` to
  [`print()`](https://rdrr.io/r/base/print.html)
  ([\#1893](https://github.com/tidyverse/dplyr/issues/1893)).

- [`slice()`](https://dplyr.tidyverse.org/dev/reference/slice.md)
  correctly handles grouped attributes
  ([\#1405](https://github.com/tidyverse/dplyr/issues/1405)).

- [`ungroup()`](https://dplyr.tidyverse.org/dev/reference/group_by.md)
  generic gains `...`
  ([\#922](https://github.com/tidyverse/dplyr/issues/922)).

#### Dual table verbs

- [`bind_cols()`](https://dplyr.tidyverse.org/dev/reference/bind_cols.md)
  matches the behaviour of
  [`bind_rows()`](https://dplyr.tidyverse.org/dev/reference/bind_rows.md)
  and ignores `NULL` inputs
  ([\#1148](https://github.com/tidyverse/dplyr/issues/1148)). It also
  handles `POSIXct`s with integer base type
  ([\#1402](https://github.com/tidyverse/dplyr/issues/1402)).

- [`bind_rows()`](https://dplyr.tidyverse.org/dev/reference/bind_rows.md)
  handles 0-length named lists
  ([\#1515](https://github.com/tidyverse/dplyr/issues/1515)), promotes
  factors to characters
  ([\#1538](https://github.com/tidyverse/dplyr/issues/1538)), and warns
  when binding factor and character
  ([\#1485](https://github.com/tidyverse/dplyr/issues/1485)).
  bind_rows()\` is more flexible in the way it can accept data frames,
  lists, list of data frames, and list of lists
  ([\#1389](https://github.com/tidyverse/dplyr/issues/1389)).

- [`bind_rows()`](https://dplyr.tidyverse.org/dev/reference/bind_rows.md)
  rejects `POSIXlt` columns
  ([\#1875](https://github.com/tidyverse/dplyr/issues/1875),
  [@krlmlr](https://github.com/krlmlr)).

- Both
  [`bind_cols()`](https://dplyr.tidyverse.org/dev/reference/bind_cols.md)
  and
  [`bind_rows()`](https://dplyr.tidyverse.org/dev/reference/bind_rows.md)
  infer classes and grouping information from the first data frame
  ([\#1692](https://github.com/tidyverse/dplyr/issues/1692)).

- [`rbind()`](https://rdrr.io/r/base/cbind.html) and
  [`cbind()`](https://rdrr.io/r/base/cbind.html) get
  [`grouped_df()`](https://dplyr.tidyverse.org/dev/reference/grouped_df.md)
  methods that make it harder to create corrupt data frames
  ([\#1385](https://github.com/tidyverse/dplyr/issues/1385)). You should
  still prefer
  [`bind_rows()`](https://dplyr.tidyverse.org/dev/reference/bind_rows.md)
  and
  [`bind_cols()`](https://dplyr.tidyverse.org/dev/reference/bind_cols.md).

- Joins now use correct class when joining on `POSIXct` columns
  ([\#1582](https://github.com/tidyverse/dplyr/issues/1582),
  [@joel23888](https://github.com/joel23888)), and consider time zones
  ([\#819](https://github.com/tidyverse/dplyr/issues/819)). Joins handle
  a `by` that is empty
  ([\#1496](https://github.com/tidyverse/dplyr/issues/1496)), or has
  duplicates ([\#1192](https://github.com/tidyverse/dplyr/issues/1192)).
  Suffixes grow progressively to avoid creating repeated column names
  ([\#1460](https://github.com/tidyverse/dplyr/issues/1460)). Joins on
  string columns should be substantially faster
  ([\#1386](https://github.com/tidyverse/dplyr/issues/1386)). Extra
  attributes are ok if they are identical
  ([\#1636](https://github.com/tidyverse/dplyr/issues/1636)). Joins work
  correct when factor levels not equal
  ([\#1712](https://github.com/tidyverse/dplyr/issues/1712),
  [\#1559](https://github.com/tidyverse/dplyr/issues/1559)). Anti- and
  semi-joins give correct result when by variable is a factor
  ([\#1571](https://github.com/tidyverse/dplyr/issues/1571)), but warn
  if factor levels are inconsistent
  ([\#2741](https://github.com/tidyverse/dplyr/issues/2741)). A clear
  error message is given for joins where an explicit `by` contains
  unavailable columns
  ([\#1928](https://github.com/tidyverse/dplyr/issues/1928),
  [\#1932](https://github.com/tidyverse/dplyr/issues/1932)). Warnings
  about join column inconsistencies now contain the column names
  ([\#2728](https://github.com/tidyverse/dplyr/issues/2728)).

- [`inner_join()`](https://dplyr.tidyverse.org/dev/reference/mutate-joins.md),
  [`left_join()`](https://dplyr.tidyverse.org/dev/reference/mutate-joins.md),
  [`right_join()`](https://dplyr.tidyverse.org/dev/reference/mutate-joins.md),
  and
  [`full_join()`](https://dplyr.tidyverse.org/dev/reference/mutate-joins.md)
  gain a `suffix` argument which allows you to control what suffix
  duplicated variable names receive
  ([\#1296](https://github.com/tidyverse/dplyr/issues/1296)).

- Set operations
  ([`intersect()`](https://dplyr.tidyverse.org/dev/reference/setops.md),
  [`union()`](https://dplyr.tidyverse.org/dev/reference/setops.md) etc)
  respect coercion rules
  ([\#799](https://github.com/tidyverse/dplyr/issues/799)).
  [`setdiff()`](https://dplyr.tidyverse.org/dev/reference/setops.md)
  handles factors with `NA` levels
  ([\#1526](https://github.com/tidyverse/dplyr/issues/1526)).

- There were a number of fixes to enable joining of data frames that
  don’t have the same encoding of column names
  ([\#1513](https://github.com/tidyverse/dplyr/issues/1513)), including
  working around bug 16885 regarding
  [`match()`](https://rdrr.io/r/base/match.html) in R 3.3.0
  ([\#1806](https://github.com/tidyverse/dplyr/issues/1806),
  [\#1810](https://github.com/tidyverse/dplyr/issues/1810),
  [@krlmlr](https://github.com/krlmlr)).

#### Vector functions

- [`combine()`](https://dplyr.tidyverse.org/dev/reference/defunct.md)
  silently drops `NULL` inputs
  ([\#1596](https://github.com/tidyverse/dplyr/issues/1596)).

- Hybrid
  [`cummean()`](https://dplyr.tidyverse.org/dev/reference/cumall.md) is
  more stable against floating point errors
  ([\#1387](https://github.com/tidyverse/dplyr/issues/1387)).

- Hybrid
  [`lead()`](https://dplyr.tidyverse.org/dev/reference/lead-lag.md) and
  [`lag()`](https://dplyr.tidyverse.org/dev/reference/lead-lag.md)
  received a considerable overhaul. They are more careful about more
  complicated expressions
  ([\#1588](https://github.com/tidyverse/dplyr/issues/1588)), and falls
  back more readily to pure R evaluation
  ([\#1411](https://github.com/tidyverse/dplyr/issues/1411)). They
  behave correctly in
  [`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)
  ([\#1434](https://github.com/tidyverse/dplyr/issues/1434)). and handle
  default values for string columns.

- Hybrid [`min()`](https://rdrr.io/r/base/Extremes.html) and
  [`max()`](https://rdrr.io/r/base/Extremes.html) handle empty sets
  ([\#1481](https://github.com/tidyverse/dplyr/issues/1481)).

- [`n_distinct()`](https://dplyr.tidyverse.org/dev/reference/n_distinct.md)
  uses multiple arguments for data frames
  ([\#1084](https://github.com/tidyverse/dplyr/issues/1084)), falls back
  to R evaluation when needed
  ([\#1657](https://github.com/tidyverse/dplyr/issues/1657)), reverting
  decision made in
  ([\#567](https://github.com/tidyverse/dplyr/issues/567)). Passing no
  arguments gives an error
  ([\#1957](https://github.com/tidyverse/dplyr/issues/1957),
  [\#1959](https://github.com/tidyverse/dplyr/issues/1959),
  [@krlmlr](https://github.com/krlmlr)).

- [`nth()`](https://dplyr.tidyverse.org/dev/reference/nth.md) now
  supports negative indices to select from end, e.g. `nth(x, -2)`
  selects the 2nd value from the end of `x`
  ([\#1584](https://github.com/tidyverse/dplyr/issues/1584)).

- [`top_n()`](https://dplyr.tidyverse.org/dev/reference/top_n.md) can
  now also select bottom `n` values by passing a negative value to `n`
  ([\#1008](https://github.com/tidyverse/dplyr/issues/1008),
  [\#1352](https://github.com/tidyverse/dplyr/issues/1352)).

- Hybrid evaluation leaves formulas untouched
  ([\#1447](https://github.com/tidyverse/dplyr/issues/1447)).

## dplyr 0.4.3

CRAN release: 2015-09-01

### Improved encoding support

Until now, dplyr’s support for non-UTF8 encodings has been rather shaky.
This release brings a number of improvement to fix these problems: it’s
probably not perfect, but should be a lot better than the previously
version. This includes fixes to
[`arrange()`](https://dplyr.tidyverse.org/dev/reference/arrange.md)
([\#1280](https://github.com/tidyverse/dplyr/issues/1280)),
[`bind_rows()`](https://dplyr.tidyverse.org/dev/reference/bind_rows.md)
([\#1265](https://github.com/tidyverse/dplyr/issues/1265)),
[`distinct()`](https://dplyr.tidyverse.org/dev/reference/distinct.md)
([\#1179](https://github.com/tidyverse/dplyr/issues/1179)), and joins
([\#1315](https://github.com/tidyverse/dplyr/issues/1315)).
`print.tbl_df()` also received a fix for strings with invalid encodings
([\#851](https://github.com/tidyverse/dplyr/issues/851)).

### Other minor improvements and bug fixes

- [`frame_data()`](https://tibble.tidyverse.org/reference/deprecated.html)
  provides a means for constructing `data_frame`s using a simple
  row-wise language.
  ([\#1358](https://github.com/tidyverse/dplyr/issues/1358),
  [@kevinushey](https://github.com/kevinushey))

- [`all.equal()`](https://rdrr.io/r/base/all.equal.html) no longer runs
  all outputs together
  ([\#1130](https://github.com/tidyverse/dplyr/issues/1130)).

- [`as_data_frame()`](https://tibble.tidyverse.org/reference/deprecated.html)
  gives better error message with NA column names
  ([\#1101](https://github.com/tidyverse/dplyr/issues/1101)).

- `[.tbl_df` is more careful about subsetting column names
  ([\#1245](https://github.com/tidyverse/dplyr/issues/1245)).

- [`arrange()`](https://dplyr.tidyverse.org/dev/reference/arrange.md)
  and [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md)
  work on empty data frames
  ([\#1142](https://github.com/tidyverse/dplyr/issues/1142)).

- [`arrange()`](https://dplyr.tidyverse.org/dev/reference/arrange.md),
  [`filter()`](https://dplyr.tidyverse.org/dev/reference/filter.md),
  [`slice()`](https://dplyr.tidyverse.org/dev/reference/slice.md), and
  [`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)
  preserve data frame meta attributes
  ([\#1064](https://github.com/tidyverse/dplyr/issues/1064)).

- [`bind_rows()`](https://dplyr.tidyverse.org/dev/reference/bind_rows.md)
  and
  [`bind_cols()`](https://dplyr.tidyverse.org/dev/reference/bind_cols.md)
  accept lists
  ([\#1104](https://github.com/tidyverse/dplyr/issues/1104)): during
  initial data cleaning you no longer need to convert lists to data
  frames, but can instead feed them to
  [`bind_rows()`](https://dplyr.tidyverse.org/dev/reference/bind_rows.md)
  directly.

- [`bind_rows()`](https://dplyr.tidyverse.org/dev/reference/bind_rows.md)
  gains a `.id` argument. When supplied, it creates a new column that
  gives the name of each data frame
  ([\#1337](https://github.com/tidyverse/dplyr/issues/1337),
  [@lionel-](https://github.com/lionel-)).

- [`bind_rows()`](https://dplyr.tidyverse.org/dev/reference/bind_rows.md)
  respects the `ordered` attribute of factors
  ([\#1112](https://github.com/tidyverse/dplyr/issues/1112)), and does
  better at comparing `POSIXct`s
  ([\#1125](https://github.com/tidyverse/dplyr/issues/1125)). The `tz`
  attribute is ignored when determining if two `POSIXct` vectors are
  comparable. If the `tz` of all inputs is the same, it’s used,
  otherwise its set to `UTC`.

- [`data_frame()`](https://tibble.tidyverse.org/reference/deprecated.html)
  always produces a `tbl_df`
  ([\#1151](https://github.com/tidyverse/dplyr/issues/1151),
  [@kevinushey](https://github.com/kevinushey))

- `filter(x, TRUE, TRUE)` now just returns `x`
  ([\#1210](https://github.com/tidyverse/dplyr/issues/1210)), it doesn’t
  internally modify the first argument
  ([\#971](https://github.com/tidyverse/dplyr/issues/971)), and it now
  works with rowwise data
  ([\#1099](https://github.com/tidyverse/dplyr/issues/1099)). It once
  again works with data tables
  ([\#906](https://github.com/tidyverse/dplyr/issues/906)).

- [`glimpse()`](https://dplyr.tidyverse.org/dev/reference/glimpse.md)
  also prints out the number of variables in addition to the number of
  observations ([@ilarischeinin](https://github.com/ilarischeinin),
  [\#988](https://github.com/tidyverse/dplyr/issues/988)).

- Joins handles matrix columns better
  ([\#1230](https://github.com/tidyverse/dplyr/issues/1230)), and can
  join `Date` objects with heterogeneous representations (some `Date`s
  are integers, while other are numeric). This also improves
  [`all.equal()`](https://rdrr.io/r/base/all.equal.html)
  ([\#1204](https://github.com/tidyverse/dplyr/issues/1204)).

- Fixed
  [`percent_rank()`](https://dplyr.tidyverse.org/dev/reference/percent_rank.md)
  and
  [`cume_dist()`](https://dplyr.tidyverse.org/dev/reference/percent_rank.md)
  so that missing values no longer affect denominator
  ([\#1132](https://github.com/tidyverse/dplyr/issues/1132)).

- `print.tbl_df()` now displays the class for all variables, not just
  those that don’t fit on the screen
  ([\#1276](https://github.com/tidyverse/dplyr/issues/1276)). It also
  displays duplicated column names correctly
  ([\#1159](https://github.com/tidyverse/dplyr/issues/1159)).

- `print.grouped_df()` now tells you how many groups there are.

- [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md) can
  set to `NULL` the first column (used to segfault,
  [\#1329](https://github.com/tidyverse/dplyr/issues/1329)) and it
  better protects intermediary results (avoiding random segfaults,
  [\#1231](https://github.com/tidyverse/dplyr/issues/1231)).

- [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md) on
  grouped data handles the special case where for the first few groups,
  the result consists of a `logical` vector with only `NA`. This can
  happen when the condition of an `ifelse` is an all `NA` logical vector
  ([\#958](https://github.com/tidyverse/dplyr/issues/958)).

- `mutate.rowwise_df()` handles factors
  ([\#886](https://github.com/tidyverse/dplyr/issues/886)) and correctly
  handles 0-row inputs
  ([\#1300](https://github.com/tidyverse/dplyr/issues/1300)).

- [`n_distinct()`](https://dplyr.tidyverse.org/dev/reference/n_distinct.md)
  gains an `na_rm` argument
  ([\#1052](https://github.com/tidyverse/dplyr/issues/1052)).

- The `Progress` bar used by
  [`do()`](https://dplyr.tidyverse.org/dev/reference/do.md) now respects
  global option `dplyr.show_progress` (default is TRUE) so you can turn
  it off globally ([@jimhester](https://github.com/jimhester)
  [\#1264](https://github.com/tidyverse/dplyr/issues/1264),
  [\#1226](https://github.com/tidyverse/dplyr/issues/1226)).

- [`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)
  handles expressions that returning heterogenous outputs,
  e.g. [`median()`](https://rdrr.io/r/stats/median.html), which that
  sometimes returns an integer, and other times a numeric
  ([\#893](https://github.com/tidyverse/dplyr/issues/893)).

- [`slice()`](https://dplyr.tidyverse.org/dev/reference/slice.md)
  silently drops columns corresponding to an NA
  ([\#1235](https://github.com/tidyverse/dplyr/issues/1235)).

- `ungroup.rowwise_df()` gives a `tbl_df`
  ([\#936](https://github.com/tidyverse/dplyr/issues/936)).

- More explicit duplicated column name error message
  ([\#996](https://github.com/tidyverse/dplyr/issues/996)).

- When “,” is already being used as the decimal point
  (`getOption("OutDec")`), use “.” as the thousands separator when
  printing out formatted numbers
  ([@ilarischeinin](https://github.com/ilarischeinin),
  [\#988](https://github.com/tidyverse/dplyr/issues/988)).

### Databases

- `db_query_fields.SQLiteConnection` uses `build_sql` rather than
  `paste0` ([\#926](https://github.com/tidyverse/dplyr/issues/926),
  [@NikNakk](https://github.com/NikNakk))

- Improved handling of [`log()`](https://rdrr.io/r/base/Log.html)
  ([\#1330](https://github.com/tidyverse/dplyr/issues/1330)).

- `n_distinct(x)` is translated to `COUNT(DISTINCT(x))`
  ([@skparkes](https://github.com/skparkes),
  [\#873](https://github.com/tidyverse/dplyr/issues/873)).

- `print(n = Inf)` now works for remote sources
  ([\#1310](https://github.com/tidyverse/dplyr/issues/1310)).

### Hybrid evaluation

- Hybrid evaluation does not take place for objects with a class
  ([\#1237](https://github.com/tidyverse/dplyr/issues/1237)).

- Improved `$` handling
  ([\#1134](https://github.com/tidyverse/dplyr/issues/1134)).

- Simplified code for
  [`lead()`](https://dplyr.tidyverse.org/dev/reference/lead-lag.md) and
  [`lag()`](https://dplyr.tidyverse.org/dev/reference/lead-lag.md) and
  make sure they work properly on factors
  ([\#955](https://github.com/tidyverse/dplyr/issues/955)). Both respect
  the `default` argument
  ([\#915](https://github.com/tidyverse/dplyr/issues/915)).

- `mutate` can set to `NULL` the first column (used to segfault,
  [\#1329](https://github.com/tidyverse/dplyr/issues/1329)).

- `filter` on grouped data handles indices correctly
  ([\#880](https://github.com/tidyverse/dplyr/issues/880)).

- [`sum()`](https://rdrr.io/r/base/sum.html) issues a warning about
  integer overflow
  ([\#1108](https://github.com/tidyverse/dplyr/issues/1108)).

## dplyr 0.4.2

CRAN release: 2015-06-16

This is a minor release containing fixes for a number of crashes and
issues identified by R CMD CHECK. There is one new “feature”: dplyr no
longer complains about unrecognised attributes, and instead just copies
them over to the output.

- [`lag()`](https://dplyr.tidyverse.org/dev/reference/lead-lag.md) and
  [`lead()`](https://dplyr.tidyverse.org/dev/reference/lead-lag.md) for
  grouped data were confused about indices and therefore produced wrong
  results ([\#925](https://github.com/tidyverse/dplyr/issues/925),
  [\#937](https://github.com/tidyverse/dplyr/issues/937)).
  [`lag()`](https://dplyr.tidyverse.org/dev/reference/lead-lag.md) once
  again overrides
  [`lag()`](https://dplyr.tidyverse.org/dev/reference/lead-lag.md)
  instead of just the default method `lag.default()`. This is necessary
  due to changes in R CMD check. To use the lag function provided by
  another package, use `pkg::lag`.

- Fixed a number of memory issues identified by valgrind.

- Improved performance when working with large number of columns
  ([\#879](https://github.com/tidyverse/dplyr/issues/879)).

- Lists-cols that contain data frames now print a slightly nicer summary
  ([\#1147](https://github.com/tidyverse/dplyr/issues/1147))

- Set operations give more useful error message on incompatible data
  frames ([\#903](https://github.com/tidyverse/dplyr/issues/903)).

- [`all.equal()`](https://rdrr.io/r/base/all.equal.html) gives the
  correct result when `ignore_row_order` is `TRUE`
  ([\#1065](https://github.com/tidyverse/dplyr/issues/1065)) and
  [`all.equal()`](https://rdrr.io/r/base/all.equal.html) correctly
  handles character missing values
  ([\#1095](https://github.com/tidyverse/dplyr/issues/1095)).

- [`bind_cols()`](https://dplyr.tidyverse.org/dev/reference/bind_cols.md)
  always produces a `tbl_df`
  ([\#779](https://github.com/tidyverse/dplyr/issues/779)).

- [`bind_rows()`](https://dplyr.tidyverse.org/dev/reference/bind_rows.md)
  gains a test for a form of data frame corruption
  ([\#1074](https://github.com/tidyverse/dplyr/issues/1074)).

- [`bind_rows()`](https://dplyr.tidyverse.org/dev/reference/bind_rows.md)
  and
  [`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)
  now handles complex columns
  ([\#933](https://github.com/tidyverse/dplyr/issues/933)).

- Workaround for using the constructor of `DataFrame` on an unprotected
  object ([\#998](https://github.com/tidyverse/dplyr/issues/998))

- Improved performance when working with large number of columns
  ([\#879](https://github.com/tidyverse/dplyr/issues/879)).

## dplyr 0.4.1

CRAN release: 2015-01-14

- Don’t assume that RPostgreSQL is available.

## dplyr 0.4.0

CRAN release: 2015-01-08

### New features

- [`add_rownames()`](https://dplyr.tidyverse.org/dev/reference/defunct.md)
  turns row names into an explicit variable
  ([\#639](https://github.com/tidyverse/dplyr/issues/639)).

- [`as_data_frame()`](https://tibble.tidyverse.org/reference/deprecated.html)
  efficiently coerces a list into a data frame
  ([\#749](https://github.com/tidyverse/dplyr/issues/749)).

- [`bind_rows()`](https://dplyr.tidyverse.org/dev/reference/bind_rows.md)
  and
  [`bind_cols()`](https://dplyr.tidyverse.org/dev/reference/bind_cols.md)
  efficiently bind a list of data frames by row or column.
  [`combine()`](https://dplyr.tidyverse.org/dev/reference/defunct.md)
  applies the same coercion rules to vectors (it works like
  [`c()`](https://rdrr.io/r/base/c.html) or
  [`unlist()`](https://rdrr.io/r/base/unlist.html) but is consistent
  with the
  [`bind_rows()`](https://dplyr.tidyverse.org/dev/reference/bind_rows.md)
  rules).

- [`right_join()`](https://dplyr.tidyverse.org/dev/reference/mutate-joins.md)
  (include all rows in `y`, and matching rows in `x`) and
  [`full_join()`](https://dplyr.tidyverse.org/dev/reference/mutate-joins.md)
  (include all rows in `x` and `y`) complete the family of mutating
  joins ([\#96](https://github.com/tidyverse/dplyr/issues/96)).

- [`group_indices()`](https://dplyr.tidyverse.org/dev/reference/group_data.md)
  computes a unique integer id for each group
  ([\#771](https://github.com/tidyverse/dplyr/issues/771)). It can be
  called on a grouped_df without any arguments or on a data frame with
  same arguments as
  [`group_by()`](https://dplyr.tidyverse.org/dev/reference/group_by.md).

### New vignettes

- `vignette("data_frames")` describes dplyr functions that make it
  easier and faster to create and coerce data frames. It subsumes the
  old `memory` vignette.

- [`vignette("two-table")`](https://dplyr.tidyverse.org/dev/articles/two-table.md)
  describes how two-table verbs work in dplyr.

### Minor improvements

- [`data_frame()`](https://tibble.tidyverse.org/reference/deprecated.html)
  (and
  [`as_data_frame()`](https://tibble.tidyverse.org/reference/deprecated.html)
  & [`tbl_df()`](https://dplyr.tidyverse.org/dev/reference/defunct.md))
  now explicitly forbid columns that are data frames or matrices
  ([\#775](https://github.com/tidyverse/dplyr/issues/775)). All columns
  must be either a 1d atomic vector or a 1d list.

- [`do()`](https://dplyr.tidyverse.org/dev/reference/do.md) uses
  lazyeval to correctly evaluate its arguments in the correct
  environment ([\#744](https://github.com/tidyverse/dplyr/issues/744)),
  and new
  [`do_()`](https://dplyr.tidyverse.org/dev/reference/defunct-lazyeval.md)
  is the SE equivalent of
  [`do()`](https://dplyr.tidyverse.org/dev/reference/do.md)
  ([\#718](https://github.com/tidyverse/dplyr/issues/718)). You can
  modify grouped data in place: this is probably a bad idea but it’s
  sometimes convenient
  ([\#737](https://github.com/tidyverse/dplyr/issues/737)).
  [`do()`](https://dplyr.tidyverse.org/dev/reference/do.md) on grouped
  data tables now passes in all columns (not all columns except grouping
  vars) ([\#735](https://github.com/tidyverse/dplyr/issues/735), thanks
  to [@kismsu](https://github.com/kismsu)).
  [`do()`](https://dplyr.tidyverse.org/dev/reference/do.md) with
  database tables no longer potentially includes grouping variables
  twice ([\#673](https://github.com/tidyverse/dplyr/issues/673)).
  Finally, [`do()`](https://dplyr.tidyverse.org/dev/reference/do.md)
  gives more consistent outputs when there are no rows or no groups
  ([\#625](https://github.com/tidyverse/dplyr/issues/625)).

- [`first()`](https://dplyr.tidyverse.org/dev/reference/nth.md) and
  [`last()`](https://dplyr.tidyverse.org/dev/reference/nth.md) preserve
  factors, dates and times
  ([\#509](https://github.com/tidyverse/dplyr/issues/509)).

- Overhaul of single table verbs for data.table backend. They now all
  use a consistent (and simpler) code base. This ensures that (e.g.)
  [`n()`](https://dplyr.tidyverse.org/dev/reference/context.md) now
  works in all verbs
  ([\#579](https://github.com/tidyverse/dplyr/issues/579)).

- In `*_join()`, you can now name only those variables that are
  different between the two tables,
  e.g. `inner_join(x, y, c("a", "b", "c" = "d"))`
  ([\#682](https://github.com/tidyverse/dplyr/issues/682)). If non-join
  columns are the same, dplyr will add `.x` and `.y` suffixes to
  distinguish the source
  ([\#655](https://github.com/tidyverse/dplyr/issues/655)).

- [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md)
  handles complex vectors
  ([\#436](https://github.com/tidyverse/dplyr/issues/436)) and forbids
  `POSIXlt` results (instead of crashing)
  ([\#670](https://github.com/tidyverse/dplyr/issues/670)).

- [`select()`](https://dplyr.tidyverse.org/dev/reference/select.md) now
  implements a more sophisticated algorithm so if you’re doing multiples
  includes and excludes with and without names, you’re more likely to
  get what you expect
  ([\#644](https://github.com/tidyverse/dplyr/issues/644)). You’ll also
  get a better error message if you supply an input that doesn’t resolve
  to an integer column position
  ([\#643](https://github.com/tidyverse/dplyr/issues/643)).

- Printing has received a number of small tweaks. All
  [`print()`](https://rdrr.io/r/base/print.html) methods invisibly
  return their input so you can interleave
  [`print()`](https://rdrr.io/r/base/print.html) statements into a
  pipeline to see interim results.
  [`print()`](https://rdrr.io/r/base/print.html) will column names of 0
  row data frames
  ([\#652](https://github.com/tidyverse/dplyr/issues/652)), and will
  never print more 20 rows (i.e. `options(dplyr.print_max)` is now 20),
  not 100 ([\#710](https://github.com/tidyverse/dplyr/issues/710)). Row
  names are no never printed since no dplyr method is guaranteed to
  preserve them
  ([\#669](https://github.com/tidyverse/dplyr/issues/669)).

  [`glimpse()`](https://dplyr.tidyverse.org/dev/reference/glimpse.md)
  prints the number of observations
  ([\#692](https://github.com/tidyverse/dplyr/issues/692))

  [`type_sum()`](https://pillar.r-lib.org/reference/type_sum.html) gains
  a data frame method.

- [`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)
  handles list output columns
  ([\#832](https://github.com/tidyverse/dplyr/issues/832))

- [`slice()`](https://dplyr.tidyverse.org/dev/reference/slice.md) works
  for data tables
  ([\#717](https://github.com/tidyverse/dplyr/issues/717)).
  Documentation clarifies that slice can’t work with relational
  databases, and the examples show how to achieve the same results using
  [`filter()`](https://dplyr.tidyverse.org/dev/reference/filter.md)
  ([\#720](https://github.com/tidyverse/dplyr/issues/720)).

- dplyr now requires RSQLite \>= 1.0. This shouldn’t affect your code in
  any way (except that RSQLite now doesn’t need to be attached) but does
  simplify the internals
  ([\#622](https://github.com/tidyverse/dplyr/issues/622)).

- Functions that need to combine multiple results into a single column
  (e.g. [`join()`](https://dplyr.tidyverse.org/dev/reference/mutate-joins.md),
  [`bind_rows()`](https://dplyr.tidyverse.org/dev/reference/bind_rows.md)
  and
  [`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md))
  are more careful about coercion.

  Joining factors with the same levels in the same order preserves the
  original levels
  ([\#675](https://github.com/tidyverse/dplyr/issues/675)). Joining
  factors with non-identical levels generates a warning and coerces to
  character ([\#684](https://github.com/tidyverse/dplyr/issues/684)).
  Joining a character to a factor (or vice versa) generates a warning
  and coerces to character. Avoid these warnings by ensuring your data
  is compatible before joining.

  `rbind_list()` will throw an error if you attempt to combine an
  integer and factor
  ([\#751](https://github.com/tidyverse/dplyr/issues/751)).
  [`rbind()`](https://rdrr.io/r/base/cbind.html)ing a column full of
  `NA`s is allowed and just collects the appropriate missing value for
  the column type being collected
  ([\#493](https://github.com/tidyverse/dplyr/issues/493)).

  [`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)
  is more careful about `NA`, e.g. the decision on the result type will
  be delayed until the first non NA value is returned
  ([\#599](https://github.com/tidyverse/dplyr/issues/599)). It will
  complain about loss of precision coercions, which can happen for
  expressions that return integers for some groups and a doubles for
  others ([\#599](https://github.com/tidyverse/dplyr/issues/599)).

- A number of functions gained new or improved hybrid handlers:
  [`first()`](https://dplyr.tidyverse.org/dev/reference/nth.md),
  [`last()`](https://dplyr.tidyverse.org/dev/reference/nth.md),
  [`nth()`](https://dplyr.tidyverse.org/dev/reference/nth.md)
  ([\#626](https://github.com/tidyverse/dplyr/issues/626)),
  [`lead()`](https://dplyr.tidyverse.org/dev/reference/lead-lag.md) &
  [`lag()`](https://dplyr.tidyverse.org/dev/reference/lead-lag.md)
  ([\#683](https://github.com/tidyverse/dplyr/issues/683)), `%in%`
  ([\#126](https://github.com/tidyverse/dplyr/issues/126)). That means
  when you use these functions in a dplyr verb, we handle them in C++,
  rather than calling back to R, and hence improving performance.

  Hybrid
  [`min_rank()`](https://dplyr.tidyverse.org/dev/reference/row_number.md)
  correctly handles `NaN` values
  ([\#726](https://github.com/tidyverse/dplyr/issues/726)). Hybrid
  implementation of
  [`nth()`](https://dplyr.tidyverse.org/dev/reference/nth.md) falls back
  to R evaluation when `n` is not a length one integer or numeric,
  e.g. when it’s an expression
  ([\#734](https://github.com/tidyverse/dplyr/issues/734)).

  Hybrid
  [`dense_rank()`](https://dplyr.tidyverse.org/dev/reference/row_number.md),
  [`min_rank()`](https://dplyr.tidyverse.org/dev/reference/row_number.md),
  [`cume_dist()`](https://dplyr.tidyverse.org/dev/reference/percent_rank.md),
  [`ntile()`](https://dplyr.tidyverse.org/dev/reference/ntile.md),
  [`row_number()`](https://dplyr.tidyverse.org/dev/reference/row_number.md)
  and
  [`percent_rank()`](https://dplyr.tidyverse.org/dev/reference/percent_rank.md)
  now preserve NAs
  ([\#774](https://github.com/tidyverse/dplyr/issues/774))

- `filter` returns its input when it has no rows or no columns
  ([\#782](https://github.com/tidyverse/dplyr/issues/782)).

- Join functions keep attributes (e.g. time zone information) from the
  left argument for `POSIXct` and `Date` objects
  ([\#819](https://github.com/tidyverse/dplyr/issues/819)), and only
  only warn once about each incompatibility
  ([\#798](https://github.com/tidyverse/dplyr/issues/798)).

### Bug fixes

- `[.tbl_df` correctly computes row names for 0-column data frames,
  avoiding problems with xtable
  ([\#656](https://github.com/tidyverse/dplyr/issues/656)).
  `[.grouped_df` will silently drop grouping if you don’t include the
  grouping columns
  ([\#733](https://github.com/tidyverse/dplyr/issues/733)).

- [`data_frame()`](https://tibble.tidyverse.org/reference/deprecated.html)
  now acts correctly if the first argument is a vector to be recycled.
  ([\#680](https://github.com/tidyverse/dplyr/issues/680) thanks
  [@jimhester](https://github.com/jimhester))

- `filter.data.table()` works if the table has a variable called “V1”
  ([\#615](https://github.com/tidyverse/dplyr/issues/615)).

- `*_join()` keeps columns in original order
  ([\#684](https://github.com/tidyverse/dplyr/issues/684)). Joining a
  factor to a character vector doesn’t segfault
  ([\#688](https://github.com/tidyverse/dplyr/issues/688)). `*_join`
  functions can now deal with multiple encodings
  ([\#769](https://github.com/tidyverse/dplyr/issues/769)), and
  correctly name results
  ([\#855](https://github.com/tidyverse/dplyr/issues/855)).

- `*_join.data.table()` works when data.table isn’t attached
  ([\#786](https://github.com/tidyverse/dplyr/issues/786)).

- [`group_by()`](https://dplyr.tidyverse.org/dev/reference/group_by.md)
  on a data table preserves original order of the rows
  ([\#623](https://github.com/tidyverse/dplyr/issues/623)).
  [`group_by()`](https://dplyr.tidyverse.org/dev/reference/group_by.md)
  supports variables with more than 39 characters thanks to a fix in
  lazyeval ([\#705](https://github.com/tidyverse/dplyr/issues/705)). It
  gives meaningful error message when a variable is not found in the
  data frame ([\#716](https://github.com/tidyverse/dplyr/issues/716)).

- [`grouped_df()`](https://dplyr.tidyverse.org/dev/reference/grouped_df.md)
  requires `vars` to be a list of symbols
  ([\#665](https://github.com/tidyverse/dplyr/issues/665)).

- `min(.,na.rm = TRUE)` works with `Date`s built on numeric vectors
  ([\#755](https://github.com/tidyverse/dplyr/issues/755)).

- [`rename_()`](https://dplyr.tidyverse.org/dev/reference/defunct-lazyeval.md)
  generic gets missing `.dots` argument
  ([\#708](https://github.com/tidyverse/dplyr/issues/708)).

- [`row_number()`](https://dplyr.tidyverse.org/dev/reference/row_number.md),
  [`min_rank()`](https://dplyr.tidyverse.org/dev/reference/row_number.md),
  [`percent_rank()`](https://dplyr.tidyverse.org/dev/reference/percent_rank.md),
  [`dense_rank()`](https://dplyr.tidyverse.org/dev/reference/row_number.md),
  [`ntile()`](https://dplyr.tidyverse.org/dev/reference/ntile.md) and
  [`cume_dist()`](https://dplyr.tidyverse.org/dev/reference/percent_rank.md)
  handle data frames with 0 rows
  ([\#762](https://github.com/tidyverse/dplyr/issues/762)). They all
  preserve missing values
  ([\#774](https://github.com/tidyverse/dplyr/issues/774)).
  [`row_number()`](https://dplyr.tidyverse.org/dev/reference/row_number.md)
  doesn’t segfault when giving an external variable with the wrong
  number of variables
  ([\#781](https://github.com/tidyverse/dplyr/issues/781)).

- `group_indices` handles the edge case when there are no variables
  ([\#867](https://github.com/tidyverse/dplyr/issues/867)).

- Removed bogus `NAs introduced by coercion to integer range` on 32-bit
  Windows ([\#2708](https://github.com/tidyverse/dplyr/issues/2708)).

## dplyr 0.3.0.1

CRAN release: 2014-10-08

- Fixed problem with test script on Windows.

## dplyr 0.3

CRAN release: 2014-10-04

### New functions

- [`between()`](https://dplyr.tidyverse.org/dev/reference/between.md)
  vector function efficiently determines if numeric values fall in a
  range, and is translated to special form for SQL
  ([\#503](https://github.com/tidyverse/dplyr/issues/503)).

- [`count()`](https://dplyr.tidyverse.org/dev/reference/count.md) makes
  it even easier to do (weighted) counts
  ([\#358](https://github.com/tidyverse/dplyr/issues/358)).

- [`data_frame()`](https://tibble.tidyverse.org/reference/deprecated.html)
  by [@kevinushey](https://github.com/kevinushey) is a nicer way of
  creating data frames. It never coerces column types (no more
  `stringsAsFactors = FALSE`!), never munges column names, and never
  adds row names. You can use previously defined columns to compute new
  columns ([\#376](https://github.com/tidyverse/dplyr/issues/376)).

- [`distinct()`](https://dplyr.tidyverse.org/dev/reference/distinct.md)
  returns distinct (unique) rows of a tbl
  ([\#97](https://github.com/tidyverse/dplyr/issues/97)). Supply
  additional variables to return the first row for each unique
  combination of variables.

- Set operations,
  [`intersect()`](https://dplyr.tidyverse.org/dev/reference/setops.md),
  [`union()`](https://dplyr.tidyverse.org/dev/reference/setops.md) and
  [`setdiff()`](https://dplyr.tidyverse.org/dev/reference/setops.md) now
  have methods for data frames, data tables and SQL database tables
  ([\#93](https://github.com/tidyverse/dplyr/issues/93)). They pass
  their arguments down to the base functions, which will ensure they
  raise errors if you pass in two many arguments.

- Joins
  (e.g. [`left_join()`](https://dplyr.tidyverse.org/dev/reference/mutate-joins.md),
  [`inner_join()`](https://dplyr.tidyverse.org/dev/reference/mutate-joins.md),
  [`semi_join()`](https://dplyr.tidyverse.org/dev/reference/filter-joins.md),
  [`anti_join()`](https://dplyr.tidyverse.org/dev/reference/filter-joins.md))
  now allow you to join on different variables in `x` and `y` tables by
  supplying a named vector to `by`. For example, `by = c("a" = "b")`
  joins `x.a` to `y.b`.

- [`n_groups()`](https://dplyr.tidyverse.org/dev/reference/group_data.md)
  function tells you how many groups in a tbl. It returns 1 for
  ungrouped data.
  ([\#477](https://github.com/tidyverse/dplyr/issues/477))

- [`transmute()`](https://dplyr.tidyverse.org/dev/reference/transmute.md)
  works like
  [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md) but
  drops all variables that you didn’t explicitly refer to
  ([\#302](https://github.com/tidyverse/dplyr/issues/302)).

- [`rename()`](https://dplyr.tidyverse.org/dev/reference/rename.md)
  makes it easy to rename variables - it works similarly to
  [`select()`](https://dplyr.tidyverse.org/dev/reference/select.md) but
  it preserves columns that you didn’t otherwise touch.

- [`slice()`](https://dplyr.tidyverse.org/dev/reference/slice.md) allows
  you to selecting rows by position
  ([\#226](https://github.com/tidyverse/dplyr/issues/226)). It includes
  positive integers, drops negative integers and you can use expression
  like [`n()`](https://dplyr.tidyverse.org/dev/reference/context.md).

### Programming with dplyr (non-standard evaluation)

- You can now program with dplyr - every function that does non-standard
  evaluation (NSE) has a standard evaluation (SE) version ending in `_`.
  This is powered by the new lazyeval package which provides all the
  tools needed to implement NSE consistently and correctly.

- See `vignette("nse")` for full details.

- `regroup()` is deprecated. Please use the more flexible
  [`group_by_()`](https://dplyr.tidyverse.org/dev/reference/defunct-lazyeval.md)
  instead.

- `summarise_each_q()` and `mutate_each_q()` are deprecated. Please use
  [`summarise_each_()`](https://dplyr.tidyverse.org/dev/reference/defunct-each.md)
  and
  [`mutate_each_()`](https://dplyr.tidyverse.org/dev/reference/defunct-each.md)
  instead.

- `funs_q` has been replaced with `funs_`.

### Removed and deprecated features

- `%.%` has been deprecated: please use `%>%` instead. `chain()` is
  defunct. ([\#518](https://github.com/tidyverse/dplyr/issues/518))

- `filter.numeric()` removed. Need to figure out how to reimplement with
  new lazy eval system.

- The `Progress` refclass is no longer exported to avoid conflicts with
  shiny. Instead use
  [`progress_estimated()`](https://dplyr.tidyverse.org/dev/reference/progress_estimated.md)
  ([\#535](https://github.com/tidyverse/dplyr/issues/535)).

- `src_monetdb()` is now implemented in MonetDB.R, not dplyr.

- `show_sql()` and `explain_sql()` and matching global options
  `dplyr.show_sql` and `dplyr.explain_sql` have been removed. Instead
  use
  [`show_query()`](https://dplyr.tidyverse.org/dev/reference/explain.md)
  and
  [`explain()`](https://dplyr.tidyverse.org/dev/reference/explain.md).

### Minor improvements and bug fixes

- Main verbs now have individual documentation pages
  ([\#519](https://github.com/tidyverse/dplyr/issues/519)).

- `%>%` is simply re-exported from magrittr, instead of creating a local
  copy ([\#496](https://github.com/tidyverse/dplyr/issues/496), thanks
  to [@jimhester](https://github.com/jimhester))

- Examples now use `nycflights13` instead of `hflights` because it the
  variables have better names and there are a few interlinked tables
  ([\#562](https://github.com/tidyverse/dplyr/issues/562)). `Lahman` and
  `nycflights13` are (once again) suggested packages. This means many
  examples will not work unless you explicitly install them with
  `install.packages(c("Lahman", "nycflights13"))`
  ([\#508](https://github.com/tidyverse/dplyr/issues/508)). dplyr now
  depends on Lahman 3.0.1. A number of examples have been updated to
  reflect modified field names
  ([\#586](https://github.com/tidyverse/dplyr/issues/586)).

- [`do()`](https://dplyr.tidyverse.org/dev/reference/do.md) now displays
  the progress bar only when used in interactive prompts and not when
  knitting ([\#428](https://github.com/tidyverse/dplyr/issues/428),
  [@jimhester](https://github.com/jimhester)).

- [`glimpse()`](https://dplyr.tidyverse.org/dev/reference/glimpse.md)
  now prints a trailing new line
  ([\#590](https://github.com/tidyverse/dplyr/issues/590)).

- [`group_by()`](https://dplyr.tidyverse.org/dev/reference/group_by.md)
  has more consistent behaviour when grouping by constants: it creates a
  new column with that value
  ([\#410](https://github.com/tidyverse/dplyr/issues/410)). It renames
  grouping variables
  ([\#410](https://github.com/tidyverse/dplyr/issues/410)). The first
  argument is now `.data` so you can create new groups with name x
  ([\#534](https://github.com/tidyverse/dplyr/issues/534)).

- Now instead of overriding
  [`lag()`](https://dplyr.tidyverse.org/dev/reference/lead-lag.md),
  dplyr overrides `lag.default()`, which should avoid clobbering lag
  methods added by other packages.
  ([\#277](https://github.com/tidyverse/dplyr/issues/277)).

- `mutate(data, a = NULL)` removes the variable `a` from the returned
  dataset ([\#462](https://github.com/tidyverse/dplyr/issues/462)).

- [`trunc_mat()`](https://tibble.tidyverse.org/reference/trunc_mat.html)
  and hence `print.tbl_df()` and friends gets a `width` argument to
  control the default output width. Set `options(dplyr.width = Inf)` to
  always show all columns
  ([\#589](https://github.com/tidyverse/dplyr/issues/589)).

- [`select()`](https://dplyr.tidyverse.org/dev/reference/select.md)
  gains [`one_of()`](https://tidyselect.r-lib.org/reference/one_of.html)
  selector: this allows you to select variables provided by a character
  vector ([\#396](https://github.com/tidyverse/dplyr/issues/396)). It
  fails immediately if you give an empty pattern to
  [`starts_with()`](https://tidyselect.r-lib.org/reference/starts_with.html),
  [`ends_with()`](https://tidyselect.r-lib.org/reference/starts_with.html),
  [`contains()`](https://tidyselect.r-lib.org/reference/starts_with.html)
  or
  [`matches()`](https://tidyselect.r-lib.org/reference/starts_with.html)
  ([\#481](https://github.com/tidyverse/dplyr/issues/481),
  [@leondutoit](https://github.com/leondutoit)). Fixed buglet in
  [`select()`](https://dplyr.tidyverse.org/dev/reference/select.md) so
  that you can now create variables called `val`
  ([\#564](https://github.com/tidyverse/dplyr/issues/564)).

- Switched from RC to R6.

- [`tally()`](https://dplyr.tidyverse.org/dev/reference/count.md) and
  [`top_n()`](https://dplyr.tidyverse.org/dev/reference/top_n.md) work
  consistently: neither accidentally evaluates the `wt` param.
  ([\#426](https://github.com/tidyverse/dplyr/issues/426),
  [@mnel](https://github.com/mnel))

- `rename` handles grouped data
  ([\#640](https://github.com/tidyverse/dplyr/issues/640)).

### Minor improvements and bug fixes by backend

#### Databases

- Correct SQL generation for
  [`paste()`](https://rdrr.io/r/base/paste.html) when used with the
  collapse parameter targeting a Postgres database.
  ([@rbdixon](https://github.com/rbdixon),
  [\#1357](https://github.com/tidyverse/dplyr/issues/1357))

- The db backend system has been completely overhauled in order to make
  it possible to add backends in other packages, and to support a much
  wider range of databases. See `vignette("new-sql-backend")` for
  instruction on how to create your own
  ([\#568](https://github.com/tidyverse/dplyr/issues/568)).

- [`src_mysql()`](https://dplyr.tidyverse.org/dev/reference/defunct.md)
  gains a method for
  [`explain()`](https://dplyr.tidyverse.org/dev/reference/explain.md).

- When [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md)
  creates a new variable that uses a window function, automatically wrap
  the result in a subquery
  ([\#484](https://github.com/tidyverse/dplyr/issues/484)).

- Correct SQL generation for
  [`first()`](https://dplyr.tidyverse.org/dev/reference/nth.md) and
  [`last()`](https://dplyr.tidyverse.org/dev/reference/nth.md)
  ([\#531](https://github.com/tidyverse/dplyr/issues/531)).

- [`order_by()`](https://dplyr.tidyverse.org/dev/reference/order_by.md)
  now works in conjunction with window functions in databases that
  support them.

#### Data frames/`tbl_df`

- All verbs now understand how to work with
  [`difftime()`](https://rdrr.io/r/base/difftime.html)
  ([\#390](https://github.com/tidyverse/dplyr/issues/390)) and `AsIs`
  ([\#453](https://github.com/tidyverse/dplyr/issues/453)) objects. They
  all check that colnames are unique
  ([\#483](https://github.com/tidyverse/dplyr/issues/483)), and are more
  robust when columns are not present
  ([\#348](https://github.com/tidyverse/dplyr/issues/348),
  [\#569](https://github.com/tidyverse/dplyr/issues/569),
  [\#600](https://github.com/tidyverse/dplyr/issues/600)).

- Hybrid evaluation bugs fixed:

  - Call substitution stopped too early when a sub expression contained
    a `$` ([\#502](https://github.com/tidyverse/dplyr/issues/502)).

  - Handle `::` and `:::`
    ([\#412](https://github.com/tidyverse/dplyr/issues/412)).

  - [`cumany()`](https://dplyr.tidyverse.org/dev/reference/cumall.md)
    and
    [`cumall()`](https://dplyr.tidyverse.org/dev/reference/cumall.md)
    properly handle `NA`
    ([\#408](https://github.com/tidyverse/dplyr/issues/408)).

  - [`nth()`](https://dplyr.tidyverse.org/dev/reference/nth.md) now
    correctly preserve the class when using dates, times and factors
    ([\#509](https://github.com/tidyverse/dplyr/issues/509)).

  - no longer substitutes within
    [`order_by()`](https://dplyr.tidyverse.org/dev/reference/order_by.md)
    because
    [`order_by()`](https://dplyr.tidyverse.org/dev/reference/order_by.md)
    needs to do its own NSE
    ([\#169](https://github.com/tidyverse/dplyr/issues/169)).

- `[.tbl_df` always returns a tbl_df (i.e. `drop = FALSE` is the
  default) ([\#587](https://github.com/tidyverse/dplyr/issues/587),
  [\#610](https://github.com/tidyverse/dplyr/issues/610)).
  `[.grouped_df` preserves important output attributes
  ([\#398](https://github.com/tidyverse/dplyr/issues/398)).

- [`arrange()`](https://dplyr.tidyverse.org/dev/reference/arrange.md)
  keeps the grouping structure of grouped data
  ([\#491](https://github.com/tidyverse/dplyr/issues/491),
  [\#605](https://github.com/tidyverse/dplyr/issues/605)), and preserves
  input classes
  ([\#563](https://github.com/tidyverse/dplyr/issues/563)).

- [`contains()`](https://tidyselect.r-lib.org/reference/starts_with.html)
  accidentally matched regular expressions, now it passes `fixed = TRUE`
  to [`grep()`](https://rdrr.io/r/base/grep.html)
  ([\#608](https://github.com/tidyverse/dplyr/issues/608)).

- [`filter()`](https://dplyr.tidyverse.org/dev/reference/filter.md)
  asserts all variables are white listed
  ([\#566](https://github.com/tidyverse/dplyr/issues/566)).

- [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md)
  makes a `rowwise_df` when given a `rowwise_df`
  ([\#463](https://github.com/tidyverse/dplyr/issues/463)).

- `rbind_all()` creates `tbl_df` objects instead of raw `data.frame`s.

- If [`select()`](https://dplyr.tidyverse.org/dev/reference/select.md)
  doesn’t match any variables, it returns a 0-column data frame, instead
  of the original
  ([\#498](https://github.com/tidyverse/dplyr/issues/498)). It no longer
  fails when if some columns are not named
  ([\#492](https://github.com/tidyverse/dplyr/issues/492))

- [`sample_n()`](https://dplyr.tidyverse.org/dev/reference/sample_n.md)
  and
  [`sample_frac()`](https://dplyr.tidyverse.org/dev/reference/sample_n.md)
  methods for data.frames exported.
  ([\#405](https://github.com/tidyverse/dplyr/issues/405),
  [@alyst](https://github.com/alyst))

- A grouped data frame may have 0 groups
  ([\#486](https://github.com/tidyverse/dplyr/issues/486)). Grouped df
  objects gain some basic validity checking, which should prevent some
  crashes related to corrupt `grouped_df` objects made by
  [`rbind()`](https://rdrr.io/r/base/cbind.html)
  ([\#606](https://github.com/tidyverse/dplyr/issues/606)).

- More coherence when joining columns of compatible but different types,
  e.g. when joining a character vector and a factor
  ([\#455](https://github.com/tidyverse/dplyr/issues/455)), or a numeric
  and integer ([\#450](https://github.com/tidyverse/dplyr/issues/450))

- [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md)
  works for on zero-row grouped data frame, and with list columns
  ([\#555](https://github.com/tidyverse/dplyr/issues/555)).

- `LazySubset` was confused about input data size
  ([\#452](https://github.com/tidyverse/dplyr/issues/452)).

- Internal
  [`n_distinct()`](https://dplyr.tidyverse.org/dev/reference/n_distinct.md)
  is stricter about its inputs: it requires one symbol which must be
  from the data frame
  ([\#567](https://github.com/tidyverse/dplyr/issues/567)).

- `rbind_*()` handle data frames with 0 rows
  ([\#597](https://github.com/tidyverse/dplyr/issues/597)). They fill
  character vector columns with `NA` instead of blanks
  ([\#595](https://github.com/tidyverse/dplyr/issues/595)). They work
  with list columns
  ([\#463](https://github.com/tidyverse/dplyr/issues/463)).

- Improved handling of encoding for column names
  ([\#636](https://github.com/tidyverse/dplyr/issues/636)).

- Improved handling of hybrid evaluation re \$ and @
  ([\#645](https://github.com/tidyverse/dplyr/issues/645)).

#### Data tables

- Fix major omission in `tbl_dt()` and `grouped_dt()` methods - I was
  accidentally doing a deep copy on every result :(

- [`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)
  and
  [`group_by()`](https://dplyr.tidyverse.org/dev/reference/group_by.md)
  now retain over-allocation when working with data.tables
  ([\#475](https://github.com/tidyverse/dplyr/issues/475),
  [@arunsrinivasan](https://github.com/arunsrinivasan)).

- joining two data.tables now correctly dispatches to data table
  methods, and result is a data table
  ([\#470](https://github.com/tidyverse/dplyr/issues/470))

#### Cubes

- `summarise.tbl_cube()` works with single grouping variable
  ([\#480](https://github.com/tidyverse/dplyr/issues/480)).

## dplyr 0.2

CRAN release: 2014-05-21

### Piping

dplyr now imports `%>%` from magrittr
([\#330](https://github.com/tidyverse/dplyr/issues/330)). I recommend
that you use this instead of `%.%` because it is easier to type (since
you can hold down the shift key) and is more flexible. With you `%>%`,
you can control which argument on the RHS receives the LHS by using the
pronoun `.`. This makes `%>%` more useful with base R functions because
they don’t always take the data frame as the first argument. For example
you could pipe `mtcars` to
[`xtabs()`](https://rdrr.io/r/stats/xtabs.html) with:

``` R
mtcars %>% xtabs( ~ cyl + vs, data = .)
```

Thanks to [@smbache](https://github.com/smbache) for the excellent
magrittr package. dplyr only provides `%>%` from magrittr, but it
contains many other useful functions. To use them, load `magrittr`
explicitly: [`library(magrittr)`](https://magrittr.tidyverse.org). For
more details, see `vignette("magrittr")`.

`%.%` will be deprecated in a future version of dplyr, but it won’t
happen for a while. I’ve also deprecated `chain()` to encourage a single
style of dplyr usage: please use `%>%` instead.

### Do

[`do()`](https://dplyr.tidyverse.org/dev/reference/do.md) has been
completely overhauled. There are now two ways to use it, either with
multiple named arguments or a single unnamed arguments.
[`group_by()`](https://dplyr.tidyverse.org/dev/reference/group_by.md) +
[`do()`](https://dplyr.tidyverse.org/dev/reference/do.md) is equivalent
to `plyr::dlply`, except it always returns a data frame.

If you use named arguments, each argument becomes a list-variable in the
output. A list-variable can contain any arbitrary R object so it’s
particularly well suited for storing models.

``` R
library(dplyr)
models <- mtcars %>% group_by(cyl) %>% do(lm = lm(mpg ~ wt, data = .))
models %>% summarise(rsq = summary(lm)$r.squared)
```

If you use an unnamed argument, the result should be a data frame. This
allows you to apply arbitrary functions to each group.

``` R
mtcars %>% group_by(cyl) %>% do(head(., 1))
```

Note the use of the `.` pronoun to refer to the data in the current
group.

[`do()`](https://dplyr.tidyverse.org/dev/reference/do.md) also has an
automatic progress bar. It appears if the computation takes longer than
5 seconds and lets you know (approximately) how much longer the job will
take to complete.

### New verbs

dplyr 0.2 adds three new verbs:

- [`glimpse()`](https://dplyr.tidyverse.org/dev/reference/glimpse.md)
  makes it possible to see all the columns in a tbl, displaying as much
  data for each variable as can be fit on a single line.

- [`sample_n()`](https://dplyr.tidyverse.org/dev/reference/sample_n.md)
  randomly samples a fixed number of rows from a tbl;
  [`sample_frac()`](https://dplyr.tidyverse.org/dev/reference/sample_n.md)
  randomly samples a fixed fraction of rows. Only works for local data
  frames and data tables
  ([\#202](https://github.com/tidyverse/dplyr/issues/202)).

- [`summarise_each()`](https://dplyr.tidyverse.org/dev/reference/defunct-each.md)
  and
  [`mutate_each()`](https://dplyr.tidyverse.org/dev/reference/defunct-each.md)
  make it easy to apply one or more functions to multiple columns in a
  tbl ([\#178](https://github.com/tidyverse/dplyr/issues/178)).

### Minor improvements

- If you load plyr after dplyr, you’ll get a message suggesting that you
  load plyr first
  ([\#347](https://github.com/tidyverse/dplyr/issues/347)).

- `as.tbl_cube()` gains a method for matrices
  ([\#359](https://github.com/tidyverse/dplyr/issues/359),
  [@paulstaab](https://github.com/paulstaab))

- [`compute()`](https://dplyr.tidyverse.org/dev/reference/compute.md)
  gains `temporary` argument so you can control whether the results are
  temporary or permanent
  ([\#382](https://github.com/tidyverse/dplyr/issues/382),
  [@cpsievert](https://github.com/cpsievert))

- [`group_by()`](https://dplyr.tidyverse.org/dev/reference/group_by.md)
  now defaults to `add = FALSE` so that it sets the grouping variables
  rather than adding to the existing list. I think this is how most
  people expected `group_by` to work anyway, so it’s unlikely to cause
  problems ([\#385](https://github.com/tidyverse/dplyr/issues/385)).

- Support for [MonetDB](http://www.monetdb.org) tables with
  `src_monetdb()` ([\#8](https://github.com/tidyverse/dplyr/issues/8),
  thanks to [@hannesmuehleisen](https://github.com/hannesmuehleisen)).

- New vignettes:

  - `memory` vignette which discusses how dplyr minimises memory usage
    for local data frames
    ([\#198](https://github.com/tidyverse/dplyr/issues/198)).

  - `new-sql-backend` vignette which discusses how to add a new SQL
    backend/source to dplyr.

- `changes()` output more clearly distinguishes which columns were added
  or deleted.

- [`explain()`](https://dplyr.tidyverse.org/dev/reference/explain.md) is
  now generic.

- dplyr is more careful when setting the keys of data tables, so it
  never accidentally modifies an object that it doesn’t own. It also
  avoids unnecessary key setting which negatively affected performance.
  ([\#193](https://github.com/tidyverse/dplyr/issues/193),
  [\#255](https://github.com/tidyverse/dplyr/issues/255)).

- [`print()`](https://rdrr.io/r/base/print.html) methods for `tbl_df`,
  `tbl_dt` and `tbl_sql` gain `n` argument to control the number of rows
  printed ([\#362](https://github.com/tidyverse/dplyr/issues/362)). They
  also works better when you have columns containing lists of complex
  objects.

- [`row_number()`](https://dplyr.tidyverse.org/dev/reference/row_number.md)
  can be called without arguments, in which case it returns the same as
  `1:n()` ([\#303](https://github.com/tidyverse/dplyr/issues/303)).

- `"comment"` attribute is allowed (white listed) as well as names
  ([\#346](https://github.com/tidyverse/dplyr/issues/346)).

- hybrid versions of `min`, `max`, `mean`, `var`, `sd` and `sum` handle
  the `na.rm` argument
  ([\#168](https://github.com/tidyverse/dplyr/issues/168)). This should
  yield substantial performance improvements for those functions.

- Special case for call to
  [`arrange()`](https://dplyr.tidyverse.org/dev/reference/arrange.md) on
  a grouped data frame with no arguments.
  ([\#369](https://github.com/tidyverse/dplyr/issues/369))

### Bug fixes

- Code adapted to Rcpp \> 0.11.1

- internal `DataDots` class protects against missing variables in verbs
  ([\#314](https://github.com/tidyverse/dplyr/issues/314)), including
  the case where `...` is missing.
  ([\#338](https://github.com/tidyverse/dplyr/issues/338))

- `all.equal.data.frame` from base is no longer bypassed. we now have
  `all.equal.tbl_df` and `all.equal.tbl_dt` methods
  ([\#332](https://github.com/tidyverse/dplyr/issues/332)).

- [`arrange()`](https://dplyr.tidyverse.org/dev/reference/arrange.md)
  correctly handles NA in numeric vectors
  ([\#331](https://github.com/tidyverse/dplyr/issues/331)) and 0 row
  data frames ([\#289](https://github.com/tidyverse/dplyr/issues/289)).

- `copy_to.src_mysql()` now works on windows
  ([\#323](https://github.com/tidyverse/dplyr/issues/323))

- `*_join()` doesn’t reorder column names
  ([\#324](https://github.com/tidyverse/dplyr/issues/324)).

- `rbind_all()` is stricter and only accepts list of data frames
  ([\#288](https://github.com/tidyverse/dplyr/issues/288))

- `rbind_*` propagates time zone information for `POSIXct` columns
  ([\#298](https://github.com/tidyverse/dplyr/issues/298)).

- `rbind_*` is less strict about type promotion. The numeric `Collecter`
  allows collection of integer and logical vectors. The integer
  `Collecter` also collects logical values
  ([\#321](https://github.com/tidyverse/dplyr/issues/321)).

- internal `sum` correctly handles integer (under/over)flow
  ([\#308](https://github.com/tidyverse/dplyr/issues/308)).

- [`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)
  checks consistency of outputs
  ([\#300](https://github.com/tidyverse/dplyr/issues/300)) and drops
  `names` attribute of output columns
  ([\#357](https://github.com/tidyverse/dplyr/issues/357)).

- join functions throw error instead of crashing when there are no
  common variables between the data frames, and also give a better error
  message when only one data frame has a by variable
  ([\#371](https://github.com/tidyverse/dplyr/issues/371)).

- [`top_n()`](https://dplyr.tidyverse.org/dev/reference/top_n.md)
  returns `n` rows instead of `n - 1`
  ([@leondutoit](https://github.com/leondutoit),
  [\#367](https://github.com/tidyverse/dplyr/issues/367)).

- SQL translation always evaluates subsetting operators (`$`, `[`, `[[`)
  locally. ([\#318](https://github.com/tidyverse/dplyr/issues/318)).

- [`select()`](https://dplyr.tidyverse.org/dev/reference/select.md) now
  renames variables in remote sql tbls
  ([\#317](https://github.com/tidyverse/dplyr/issues/317)) and
  implicitly adds grouping variables
  ([\#170](https://github.com/tidyverse/dplyr/issues/170)).

- internal `grouped_df_impl` function errors if there are no variables
  to group by ([\#398](https://github.com/tidyverse/dplyr/issues/398)).

- `n_distinct` did not treat NA correctly in the numeric case
  [\#384](https://github.com/tidyverse/dplyr/issues/384).

- Some compiler warnings triggered by -Wall or -pedantic have been
  eliminated.

- `group_by` only creates one group for NA
  ([\#401](https://github.com/tidyverse/dplyr/issues/401)).

- Hybrid evaluator did not evaluate expression in correct environment
  ([\#403](https://github.com/tidyverse/dplyr/issues/403)).

## dplyr 0.1.3

CRAN release: 2014-03-15

### Bug fixes

- [`select()`](https://dplyr.tidyverse.org/dev/reference/select.md)
  actually renames columns in a data table
  ([\#284](https://github.com/tidyverse/dplyr/issues/284)).

- `rbind_all()` and `rbind_list()` now handle missing values in factors
  ([\#279](https://github.com/tidyverse/dplyr/issues/279)).

- SQL joins now work better if names duplicated in both x and y tables
  ([\#310](https://github.com/tidyverse/dplyr/issues/310)).

- Builds against Rcpp 0.11.1

- [`select()`](https://dplyr.tidyverse.org/dev/reference/select.md)
  correctly works with the vars attribute
  ([\#309](https://github.com/tidyverse/dplyr/issues/309)).

- Internal code is stricter when deciding if a data frame is grouped
  ([\#308](https://github.com/tidyverse/dplyr/issues/308)): this avoids
  a number of situations which previously caused problems.

- More data frame joins work with missing values in keys
  ([\#306](https://github.com/tidyverse/dplyr/issues/306)).

## dplyr 0.1.2

CRAN release: 2014-02-24

### New features

- [`select()`](https://dplyr.tidyverse.org/dev/reference/select.md) is
  substantially more powerful. You can use named arguments to rename
  existing variables, and new functions
  [`starts_with()`](https://tidyselect.r-lib.org/reference/starts_with.html),
  [`ends_with()`](https://tidyselect.r-lib.org/reference/starts_with.html),
  [`contains()`](https://tidyselect.r-lib.org/reference/starts_with.html),
  [`matches()`](https://tidyselect.r-lib.org/reference/starts_with.html)
  and
  [`num_range()`](https://tidyselect.r-lib.org/reference/starts_with.html)
  to select variables based on their names. It now also makes a shallow
  copy, substantially reducing its memory impact
  ([\#158](https://github.com/tidyverse/dplyr/issues/158),
  [\#172](https://github.com/tidyverse/dplyr/issues/172),
  [\#192](https://github.com/tidyverse/dplyr/issues/192),
  [\#232](https://github.com/tidyverse/dplyr/issues/232)).

- [`summarize()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)
  added as alias for
  [`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)
  for people from countries that don’t don’t spell things correctly ;)
  ([\#245](https://github.com/tidyverse/dplyr/issues/245))

### Bug fixes

- [`filter()`](https://dplyr.tidyverse.org/dev/reference/filter.md) now
  fails when given anything other than a logical vector, and correctly
  handles missing values
  ([\#249](https://github.com/tidyverse/dplyr/issues/249)).
  `filter.numeric()` proxies
  [`stats::filter()`](https://rdrr.io/r/stats/filter.html) so you can
  continue to use
  [`filter()`](https://dplyr.tidyverse.org/dev/reference/filter.md)
  function with numeric inputs
  ([\#264](https://github.com/tidyverse/dplyr/issues/264)).

- [`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)
  correctly uses newly created variables
  ([\#259](https://github.com/tidyverse/dplyr/issues/259)).

- [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md)
  correctly propagates attributes
  ([\#265](https://github.com/tidyverse/dplyr/issues/265)) and
  [`mutate.data.frame()`](https://dplyr.tidyverse.org/dev/reference/mutate.md)
  correctly mutates the same variable repeatedly
  ([\#243](https://github.com/tidyverse/dplyr/issues/243)).

- [`lead()`](https://dplyr.tidyverse.org/dev/reference/lead-lag.md) and
  [`lag()`](https://dplyr.tidyverse.org/dev/reference/lead-lag.md)
  preserve attributes, so they now work with dates, times and factors
  ([\#166](https://github.com/tidyverse/dplyr/issues/166)).

- [`n()`](https://dplyr.tidyverse.org/dev/reference/context.md) never
  accepts arguments
  ([\#223](https://github.com/tidyverse/dplyr/issues/223)).

- [`row_number()`](https://dplyr.tidyverse.org/dev/reference/row_number.md)
  gives correct results
  ([\#227](https://github.com/tidyverse/dplyr/issues/227)).

- `rbind_all()` silently ignores data frames with 0 rows or 0 columns
  ([\#274](https://github.com/tidyverse/dplyr/issues/274)).

- [`group_by()`](https://dplyr.tidyverse.org/dev/reference/group_by.md)
  orders the result
  ([\#242](https://github.com/tidyverse/dplyr/issues/242)). It also
  checks that columns are of supported types
  ([\#233](https://github.com/tidyverse/dplyr/issues/233),
  [\#276](https://github.com/tidyverse/dplyr/issues/276)).

- The hybrid evaluator did not handle some expressions correctly, for
  example in `if(n() > 5) 1 else 2` the subexpression
  [`n()`](https://dplyr.tidyverse.org/dev/reference/context.md) was not
  substituted correctly. It also correctly processes `$`
  ([\#278](https://github.com/tidyverse/dplyr/issues/278)).

- [`arrange()`](https://dplyr.tidyverse.org/dev/reference/arrange.md)
  checks that all columns are of supported types
  ([\#266](https://github.com/tidyverse/dplyr/issues/266)). It also
  handles list columns
  ([\#282](https://github.com/tidyverse/dplyr/issues/282)).

- Working towards Solaris compatibility.

- Benchmarking vignette temporarily disabled due to microbenchmark
  problems reported by BDR.

## dplyr 0.1.1

CRAN release: 2014-01-29

### Improvements

- new `location()` and `changes()` functions which provide more
  information about how data frames are stored in memory so that you can
  see what gets copied.

- renamed `explain_tbl()` to
  [`explain()`](https://dplyr.tidyverse.org/dev/reference/explain.md)
  ([\#182](https://github.com/tidyverse/dplyr/issues/182)).

- [`tally()`](https://dplyr.tidyverse.org/dev/reference/count.md) gains
  `sort` argument to sort output so highest counts come first
  ([\#173](https://github.com/tidyverse/dplyr/issues/173)).

- `ungroup.grouped_df()`,
  [`tbl_df()`](https://dplyr.tidyverse.org/dev/reference/defunct.md),
  `as.data.frame.tbl_df()` now only make shallow copies of their inputs
  ([\#191](https://github.com/tidyverse/dplyr/issues/191)).

- The `benchmark-baseball` vignette now contains fairer (including
  grouping times) comparisons with `data.table`.
  ([\#222](https://github.com/tidyverse/dplyr/issues/222))

### Bug fixes

- [`filter()`](https://dplyr.tidyverse.org/dev/reference/filter.md)
  ([\#221](https://github.com/tidyverse/dplyr/issues/221)) and
  [`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)
  ([\#194](https://github.com/tidyverse/dplyr/issues/194)) correctly
  propagate attributes.

- [`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)
  throws an error when asked to summarise an unknown variable instead of
  crashing ([\#208](https://github.com/tidyverse/dplyr/issues/208)).

- [`group_by()`](https://dplyr.tidyverse.org/dev/reference/group_by.md)
  handles factors with missing values
  ([\#183](https://github.com/tidyverse/dplyr/issues/183)).

- [`filter()`](https://dplyr.tidyverse.org/dev/reference/filter.md)
  handles scalar results
  ([\#217](https://github.com/tidyverse/dplyr/issues/217)) and better
  handles scoping, e.g. `filter(., variable)` where `variable` is
  defined in the function that calls `filter`. It also handles `T` and
  `F` as aliases to `TRUE` and `FALSE` if there are no `T` or `F`
  variables in the data or in the scope.

- `select.grouped_df` fails when the grouping variables are not included
  in the selected variables
  ([\#170](https://github.com/tidyverse/dplyr/issues/170))

- `all.equal.data.frame()` handles a corner case where the data frame
  has `NULL` names
  ([\#217](https://github.com/tidyverse/dplyr/issues/217))

- [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md)
  gives informative error message on unsupported types
  ([\#179](https://github.com/tidyverse/dplyr/issues/179))

- dplyr source package no longer includes pandas benchmark, reducing
  download size from 2.8 MB to 0.5 MB.
