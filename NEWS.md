Version 0.1-2 (2015-12-30)
===

- Add former `matrixToDataFrame()` tests, and fix unwanted conversion to factor.
- `base::getElement()` now works with tibbles (#9).


Version 0.1-1 (2015-12-30)
===

- Remove spurious usage of "dplyr" in documentation (#3).
- Remove unused `make_tbl()`.
- Almost full test coverage.


tibble 0.1
==========

First release. Contains functions related to table sources, the `tbl` class and the `tbl_df` subclass, as well as output and helper functions:

## Table sources

- `src()`
- `src_df()`
- `src_local()`
- `src_tbls()`
- `is.src()`
- `same_src()`


## The `tbl` class

- `tbl()`
- `tbl_df()`
- `make_tbl()`
- `tbl_vars()`
- `as.tbl()`, `is.tbl()`


## The `tbl_df` subclass

- `as_data_frame()`
- `data_frame()`, `data_frame_()`
- `frame_data()`, `tibble()`


## Output functions

- `glimpse()`
- `trunc_mat()`, `knit_print.trunc_mat()`
- `dim_desc()`
- `type_sum()`


## Helper functions

- `lst()`, `lst_()`
- `add_row()`
- `add_rownames()`
