dplyr 0.1.0.99
--------------

* new `location()` and `changes()` functions which provide more information
  about how data frames are stored in memory so that you can see what
  gets copied

* renamed `explain_tbl()` to `explain()` (#182)

* `tally()` gains `sort` argument to sort output so highest counts
  come first. (#173)
  
* `ungroup.grouped_df` and `tbl_df` no longer make data copies. (#191)

* `summarise` now correctly propagate attributes. (#194)
