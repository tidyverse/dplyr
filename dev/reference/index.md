# Package index

## Data frame verbs

### Rows

Verbs that principally operate on rows.

- [`arrange()`](https://dplyr.tidyverse.org/dev/reference/arrange.md) :
  Order rows using column values
- [`distinct()`](https://dplyr.tidyverse.org/dev/reference/distinct.md)
  : Keep distinct/unique rows
- [`filter()`](https://dplyr.tidyverse.org/dev/reference/filter.md) :
  Keep rows that match a condition
- [`slice()`](https://dplyr.tidyverse.org/dev/reference/slice.md)
  [`slice_head()`](https://dplyr.tidyverse.org/dev/reference/slice.md)
  [`slice_tail()`](https://dplyr.tidyverse.org/dev/reference/slice.md)
  [`slice_min()`](https://dplyr.tidyverse.org/dev/reference/slice.md)
  [`slice_max()`](https://dplyr.tidyverse.org/dev/reference/slice.md)
  [`slice_sample()`](https://dplyr.tidyverse.org/dev/reference/slice.md)
  : Subset rows using their positions

### Columns

Verbs that principally operate on columns.

- [`glimpse`](https://dplyr.tidyverse.org/dev/reference/glimpse.md) :
  Get a glimpse of your data
- [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md) :
  Create, modify, and delete columns
- [`pull()`](https://dplyr.tidyverse.org/dev/reference/pull.md) :
  Extract a single column
- [`relocate()`](https://dplyr.tidyverse.org/dev/reference/relocate.md)
  : Change column order
- [`rename()`](https://dplyr.tidyverse.org/dev/reference/rename.md)
  [`rename_with()`](https://dplyr.tidyverse.org/dev/reference/rename.md)
  : Rename columns
- [`select()`](https://dplyr.tidyverse.org/dev/reference/select.md) :
  Keep or drop columns using their names and types

### Groups

Verbs that principally operate on groups of rows.

- [`count()`](https://dplyr.tidyverse.org/dev/reference/count.md)
  [`tally()`](https://dplyr.tidyverse.org/dev/reference/count.md)
  [`add_count()`](https://dplyr.tidyverse.org/dev/reference/count.md)
  [`add_tally()`](https://dplyr.tidyverse.org/dev/reference/count.md) :
  Count the observations in each group

- [`group_by()`](https://dplyr.tidyverse.org/dev/reference/group_by.md)
  [`ungroup()`](https://dplyr.tidyverse.org/dev/reference/group_by.md) :
  Group by one or more variables

- [`dplyr_by`](https://dplyr.tidyverse.org/dev/reference/dplyr_by.md) :

  Per-operation grouping with `.by`/`by`

- [`rowwise()`](https://dplyr.tidyverse.org/dev/reference/rowwise.md) :
  Group input by rows

- [`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)
  [`summarize()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)
  : Summarise each group down to one row

- [`reframe()`](https://dplyr.tidyverse.org/dev/reference/reframe.md) :
  Transform each group to an arbitrary number of rows

- [`n()`](https://dplyr.tidyverse.org/dev/reference/context.md)
  [`cur_group()`](https://dplyr.tidyverse.org/dev/reference/context.md)
  [`cur_group_id()`](https://dplyr.tidyverse.org/dev/reference/context.md)
  [`cur_group_rows()`](https://dplyr.tidyverse.org/dev/reference/context.md)
  [`cur_column()`](https://dplyr.tidyverse.org/dev/reference/context.md)
  : Information about the "current" group or variable

### Data frames

Verbs that principally operate on pairs of data frames.

- [`bind_cols()`](https://dplyr.tidyverse.org/dev/reference/bind_cols.md)
  : Bind multiple data frames by column
- [`bind_rows()`](https://dplyr.tidyverse.org/dev/reference/bind_rows.md)
  : Bind multiple data frames by row
- [`intersect()`](https://dplyr.tidyverse.org/dev/reference/setops.md)
  [`union()`](https://dplyr.tidyverse.org/dev/reference/setops.md)
  [`union_all()`](https://dplyr.tidyverse.org/dev/reference/setops.md)
  [`setdiff()`](https://dplyr.tidyverse.org/dev/reference/setops.md)
  [`setequal()`](https://dplyr.tidyverse.org/dev/reference/setops.md)
  [`symdiff()`](https://dplyr.tidyverse.org/dev/reference/setops.md) :
  Set operations
- [`inner_join()`](https://dplyr.tidyverse.org/dev/reference/mutate-joins.md)
  [`left_join()`](https://dplyr.tidyverse.org/dev/reference/mutate-joins.md)
  [`right_join()`](https://dplyr.tidyverse.org/dev/reference/mutate-joins.md)
  [`full_join()`](https://dplyr.tidyverse.org/dev/reference/mutate-joins.md)
  : Mutating joins
- [`nest_join()`](https://dplyr.tidyverse.org/dev/reference/nest_join.md)
  : Nest join
- [`semi_join()`](https://dplyr.tidyverse.org/dev/reference/filter-joins.md)
  [`anti_join()`](https://dplyr.tidyverse.org/dev/reference/filter-joins.md)
  : Filtering joins
- [`cross_join()`](https://dplyr.tidyverse.org/dev/reference/cross_join.md)
  : Cross join
- [`join_by()`](https://dplyr.tidyverse.org/dev/reference/join_by.md) :
  Join specifications
- [`rows_insert()`](https://dplyr.tidyverse.org/dev/reference/rows.md)
  [`rows_append()`](https://dplyr.tidyverse.org/dev/reference/rows.md)
  [`rows_update()`](https://dplyr.tidyverse.org/dev/reference/rows.md)
  [`rows_patch()`](https://dplyr.tidyverse.org/dev/reference/rows.md)
  [`rows_upsert()`](https://dplyr.tidyverse.org/dev/reference/rows.md)
  [`rows_delete()`](https://dplyr.tidyverse.org/dev/reference/rows.md) :
  Manipulate individual rows

### Multiple columns

Pair these functions with
[`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md),
[`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md),
[`filter()`](https://dplyr.tidyverse.org/dev/reference/filter.md), and
[`group_by()`](https://dplyr.tidyverse.org/dev/reference/group_by.md) to
operate on multiple columns simultaneously.

- [`across()`](https://dplyr.tidyverse.org/dev/reference/across.md)
  [`if_any()`](https://dplyr.tidyverse.org/dev/reference/across.md)
  [`if_all()`](https://dplyr.tidyverse.org/dev/reference/across.md) :
  Apply a function (or functions) across multiple columns
- [`c_across()`](https://dplyr.tidyverse.org/dev/reference/c_across.md)
  : Combine values from multiple columns
- [`pick()`](https://dplyr.tidyverse.org/dev/reference/pick.md) : Select
  a subset of columns

## Vector functions

Unlike other dplyr functions, these functions work on individual
vectors, not data frames.

- [`between()`](https://dplyr.tidyverse.org/dev/reference/between.md) :
  Detect where values fall in a specified range

- [`case_when()`](https://dplyr.tidyverse.org/dev/reference/case-and-replace-when.md)
  [`replace_when()`](https://dplyr.tidyverse.org/dev/reference/case-and-replace-when.md)
  : A general vectorised if-else

- [`recode_values()`](https://dplyr.tidyverse.org/dev/reference/recode-and-replace-values.md)
  [`replace_values()`](https://dplyr.tidyverse.org/dev/reference/recode-and-replace-values.md)
  : Recode and replace values

- [`coalesce()`](https://dplyr.tidyverse.org/dev/reference/coalesce.md)
  : Find the first non-missing element

- [`consecutive_id()`](https://dplyr.tidyverse.org/dev/reference/consecutive_id.md)
  : Generate a unique identifier for consecutive combinations

- [`cumall()`](https://dplyr.tidyverse.org/dev/reference/cumall.md)
  [`cumany()`](https://dplyr.tidyverse.org/dev/reference/cumall.md)
  [`cummean()`](https://dplyr.tidyverse.org/dev/reference/cumall.md) :
  Cumulative versions of any, all, and mean

- [`desc()`](https://dplyr.tidyverse.org/dev/reference/desc.md) :
  Descending order

- [`if_else()`](https://dplyr.tidyverse.org/dev/reference/if_else.md) :
  Vectorised if-else

- [`lag()`](https://dplyr.tidyverse.org/dev/reference/lead-lag.md)
  [`lead()`](https://dplyr.tidyverse.org/dev/reference/lead-lag.md) :
  Compute lagged or leading values

- [`n_distinct()`](https://dplyr.tidyverse.org/dev/reference/n_distinct.md)
  : Count unique combinations

- [`na_if()`](https://dplyr.tidyverse.org/dev/reference/na_if.md) :

  Convert values to `NA`

- [`near()`](https://dplyr.tidyverse.org/dev/reference/near.md) :
  Compare two numeric vectors

- [`nth()`](https://dplyr.tidyverse.org/dev/reference/nth.md)
  [`first()`](https://dplyr.tidyverse.org/dev/reference/nth.md)
  [`last()`](https://dplyr.tidyverse.org/dev/reference/nth.md) : Extract
  the first, last, or nth value from a vector

- [`ntile()`](https://dplyr.tidyverse.org/dev/reference/ntile.md) :

  Bucket a numeric vector into `n` groups

- [`order_by()`](https://dplyr.tidyverse.org/dev/reference/order_by.md)
  : A helper function for ordering window function output

- [`percent_rank()`](https://dplyr.tidyverse.org/dev/reference/percent_rank.md)
  [`cume_dist()`](https://dplyr.tidyverse.org/dev/reference/percent_rank.md)
  : Proportional ranking functions

- [`row_number()`](https://dplyr.tidyverse.org/dev/reference/row_number.md)
  [`min_rank()`](https://dplyr.tidyverse.org/dev/reference/row_number.md)
  [`dense_rank()`](https://dplyr.tidyverse.org/dev/reference/row_number.md)
  : Integer ranking functions

## Built in datasets

- [`band_members`](https://dplyr.tidyverse.org/dev/reference/band_members.md)
  [`band_instruments`](https://dplyr.tidyverse.org/dev/reference/band_members.md)
  [`band_instruments2`](https://dplyr.tidyverse.org/dev/reference/band_members.md)
  : Band membership
- [`starwars`](https://dplyr.tidyverse.org/dev/reference/starwars.md) :
  Starwars characters
- [`storms`](https://dplyr.tidyverse.org/dev/reference/storms.md) :
  Storm tracks data

## Grouping helpers

This (mostly) experimental family of functions are used to manipulate
groups in various ways.

- [`group_cols()`](https://dplyr.tidyverse.org/dev/reference/group_cols.md)
  : Select grouping variables
- [`group_map()`](https://dplyr.tidyverse.org/dev/reference/group_map.md)
  [`group_modify()`](https://dplyr.tidyverse.org/dev/reference/group_map.md)
  [`group_walk()`](https://dplyr.tidyverse.org/dev/reference/group_map.md)
  **\[experimental\]** : Apply a function to each group
- [`group_trim()`](https://dplyr.tidyverse.org/dev/reference/group_trim.md)
  **\[experimental\]** : Trim grouping structure

## Superseded

Superseded functions have been replaced by new approaches that we
believe to be superior, but we don’t want to force you to change until
you’re ready, so the existing functions will stay around for several
years.

- [`all_vars()`](https://dplyr.tidyverse.org/dev/reference/all_vars.md)
  [`any_vars()`](https://dplyr.tidyverse.org/dev/reference/all_vars.md)
  **\[superseded\]** : Apply predicate to all variables

- [`case_match()`](https://dplyr.tidyverse.org/dev/reference/case_match.md)
  **\[superseded\]** :

  A general vectorised [`switch()`](https://rdrr.io/r/base/switch.html)

- [`recode()`](https://dplyr.tidyverse.org/dev/reference/recode.md)
  [`recode_factor()`](https://dplyr.tidyverse.org/dev/reference/recode.md)
  **\[superseded\]** : Recode values

- [`sample_n()`](https://dplyr.tidyverse.org/dev/reference/sample_n.md)
  [`sample_frac()`](https://dplyr.tidyverse.org/dev/reference/sample_n.md)
  **\[superseded\]** : Sample n rows from a table

- [`scoped`](https://dplyr.tidyverse.org/dev/reference/scoped.md)
  **\[superseded\]** : Operate on a selection of variables

- [`top_n()`](https://dplyr.tidyverse.org/dev/reference/top_n.md)
  [`top_frac()`](https://dplyr.tidyverse.org/dev/reference/top_n.md)
  **\[superseded\]** : Select top (or bottom) n rows (by value)

- [`vars()`](https://dplyr.tidyverse.org/dev/reference/vars.md)
  **\[superseded\]** : Select variables

- [`with_groups()`](https://dplyr.tidyverse.org/dev/reference/with_groups.md)
  **\[superseded\]** : Perform an operation with temporary groups

## Remote tables

- [`auto_copy()`](https://dplyr.tidyverse.org/dev/reference/auto_copy.md)
  : Copy tables to same source, if necessary
- [`compute()`](https://dplyr.tidyverse.org/dev/reference/compute.md)
  [`collect()`](https://dplyr.tidyverse.org/dev/reference/compute.md)
  [`collapse()`](https://dplyr.tidyverse.org/dev/reference/compute.md) :
  Force computation of a database query
- [`copy_to()`](https://dplyr.tidyverse.org/dev/reference/copy_to.md) :
  Copy a local data frame to a remote src
- [`ident()`](https://dplyr.tidyverse.org/dev/reference/ident.md) : Flag
  a character vector as SQL identifiers
- [`explain()`](https://dplyr.tidyverse.org/dev/reference/explain.md)
  [`show_query()`](https://dplyr.tidyverse.org/dev/reference/explain.md)
  : Explain details of a tbl
- [`tbl()`](https://dplyr.tidyverse.org/dev/reference/tbl.md)
  [`is.tbl()`](https://dplyr.tidyverse.org/dev/reference/tbl.md) :
  Create a table from a data source
- [`sql()`](https://dplyr.tidyverse.org/dev/reference/sql.md) : SQL
  escaping.
