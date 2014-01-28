dplyr 0.1.0.99
--------------

* new `location()` and `changes()` functions which provide more information
  about how data frames are stored in memory so that you can see what
  gets copied

* renamed `explain_tbl()` to `explain()` (#182)

* `tally()` gains `sort` argument to sort output so highest counts
  come first. (#173)

* `ungroup.grouped_df()`, `tbl_df()`, `as.data.frame()` no longer make deep
  copies of their input. (#191)

* `summarise` correctly propagate attributes. (#194)

* `summarise` fails on unknown variables (#208)

* `group_by` correctly handles grouping by a factor that has NA. (#183)

* `filter` handles scalar results (#217) and better handles scoping, e.g. 
  `filter(.,variable)` where `variable` is defined in the function that calls
  `filter`. It also handles `T` and `F` correctly as aliases to `TRUE` and 
  `FALSE` only if there are no `T` or `F` variables in the data or 
  in the scope. 
  
* `filter` correctly propagates attributes. (#221)  

* `select.grouped_df` fails when the grouping variables are not included
  in the selected variables (#170)
  
* `all.equal.data.frame` handles the corner case with NULL names (#217)

* `mutate` gives informative error message on unsupported types (#179)

