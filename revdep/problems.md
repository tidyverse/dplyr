# arrow

<details>

* Version: 8.0.0
* GitHub: https://github.com/apache/arrow
* Source code: https://github.com/cran/arrow
* Date/Publication: 2022-05-09 22:50:02 UTC
* Number of recursive dependencies: 72

Run `cloud_details(, "arrow")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       3.     └─testthat::expect_equal(...) at tests/testthat/helper-expectation.R:42:4
      ── Failure (test-dplyr-distinct.R:85:3): distinct() can contain expressions ────
      `object` (`actual`) not equal to `expected` (`expected`).
      
      `names(actual)`:   "int" "lgl" "x"
      `names(expected)`: "lgl" "int" "x"
      Backtrace:
          ▆
       1. └─arrow:::compare_dplyr_binding(...) at test-dplyr-distinct.R:85:2
       2.   └─arrow:::expect_equal(via_table, expected, ...) at tests/testthat/helper-expectation.R:129:4
       3.     └─testthat::expect_equal(...) at tests/testthat/helper-expectation.R:42:4
      
      [ FAIL 9 | WARN 2 | SKIP 69 | PASS 7110 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 93.3Mb
      sub-directories of 1Mb or more:
        R      4.0Mb
        libs  88.1Mb
    ```

# bigrquery

<details>

* Version: 1.4.0
* GitHub: https://github.com/r-dbi/bigrquery
* Source code: https://github.com/cran/bigrquery
* Date/Publication: 2021-08-05 04:20:35 UTC
* Number of recursive dependencies: 69

Run `cloud_details(, "bigrquery")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... WARNING
    ```
    Missing or unexported object: ‘dplyr::sql’
    ```

# brolgar

<details>

* Version: 0.1.2
* GitHub: https://github.com/njtierney/brolgar
* Source code: https://github.com/cran/brolgar
* Date/Publication: 2021-08-25 12:50:18 UTC
* Number of recursive dependencies: 114

Run `cloud_details(, "brolgar")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       20.           └─vctrs:::stop_lossy_cast(...)
       21.             └─vctrs::stop_incompatible_cast(...)
       22.               └─vctrs::stop_incompatible_type(...)
       23.                 └─vctrs:::stop_incompatible(...)
       24.                   └─vctrs:::stop_vctrs(...)
       25.                     └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
      
      [ FAIL 1 | WARN 1 | SKIP 5 | PASS 270 ]
      Deleting unused snapshots:
      • facet-sample/gg-facet-sample-alt.svg
      • facet-sample/gg-facet-sample.svg
      • facet-strata/gg-facet-strata-along.svg
      • facet-strata/gg-facet-strata.svg
      Error: Test failures
      Execution halted
    ```

# childesr

<details>

* Version: 0.2.3
* GitHub: https://github.com/langcog/childesr
* Source code: https://github.com/cran/childesr
* Date/Publication: 2022-01-27 00:00:02 UTC
* Number of recursive dependencies: 45

Run `cloud_details(, "childesr")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... WARNING
    ```
    Namespace in Imports field not imported from: ‘dbplyr’
      All declared Imports should be used.
    Missing or unexported object: ‘dplyr::sql’
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dbplyr’
      All declared Imports should be used.
    ```

# circumplex

<details>

* Version: 0.3.8
* GitHub: https://github.com/jmgirard/circumplex
* Source code: https://github.com/cran/circumplex
* Date/Publication: 2021-05-28 15:00:06 UTC
* Number of recursive dependencies: 103

Run `cloud_details(, "circumplex")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘circumplex-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: score
    > ### Title: Score circumplex scales from item responses
    > ### Aliases: score
    > 
    > ### ** Examples
    > 
    > data("raw_iipsc")
    ...
     13. └─dplyr::na_if(., "NaN")
     14.   └─vctrs::vec_cast(x = y, to = x, x_arg = "y", to_arg = "x")
     15.     └─vctrs (local) `<fn>`()
     16.       └─vctrs::vec_default_cast(...)
     17.         └─vctrs::stop_incompatible_cast(...)
     18.           └─vctrs::stop_incompatible_type(...)
     19.             └─vctrs:::stop_incompatible(...)
     20.               └─vctrs:::stop_vctrs(...)
     21.                 └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      ══ Skipped tests ═══════════════════════════════════════════════════════════════
      • On CRAN (13)
      • getRversion() > 4 is TRUE (4)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error (test-tidying_functions.R:29:3): score works ──────────────────────────
      <dplyr:::mutate_error/rlang_error/error/condition>
      Error in `mutate(.tbl, !!!funs)`: Problem while computing `PA = (structure(function (..., .x = ..1, .y = ..2, . = ..1) ...`.
      Caused by error in `dplyr::na_if()`:
      ! Can't convert `y` <character> to match type of `x` <double>.
      
      [ FAIL 1 | WARN 0 | SKIP 17 | PASS 100 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘intermediate-ssm-analysis.Rmd’ using rmarkdown
    --- finished re-building ‘intermediate-ssm-analysis.Rmd’
    
    --- re-building ‘introduction-to-ssm-analysis.Rmd’ using rmarkdown
    --- finished re-building ‘introduction-to-ssm-analysis.Rmd’
    
    --- re-building ‘using-instruments.Rmd’ using rmarkdown
    Quitting from lines 135-137 (using-instruments.Rmd) 
    ...
    ..2, . = ..1) ...`.
    Caused by error in `dplyr::na_if()`:
    ! Can't convert `y` <character> to match type of `x` <double>.
    --- failed re-building ‘using-instruments.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘using-instruments.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        doc    1.8Mb
        libs   3.4Mb
    ```

# ckanr

<details>

* Version: 0.6.0
* GitHub: https://github.com/ropensci/ckanr
* Source code: https://github.com/cran/ckanr
* Date/Publication: 2021-02-03 19:30:02 UTC
* Number of recursive dependencies: 65

Run `cloud_details(, "ckanr")` for more info

</details>

## Newly broken

*   checking whether package ‘ckanr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/ckanr/new/ckanr.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

## Installation

### Devel

```
* installing *source* package ‘ckanr’ ...
** package ‘ckanr’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘sql’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘ckanr’
* removing ‘/tmp/workdir/ckanr/new/ckanr.Rcheck/ckanr’


```
### CRAN

```
* installing *source* package ‘ckanr’ ...
** package ‘ckanr’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (ckanr)


```
# comperes

<details>

* Version: 0.2.5
* GitHub: https://github.com/echasnovski/comperes
* Source code: https://github.com/cran/comperes
* Date/Publication: 2020-11-23 21:20:02 UTC
* Number of recursive dependencies: 63

Run `cloud_details(, "comperes")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(comperes)
      > 
      > test_check("comperes")
      [ FAIL 1 | WARN 16 | SKIP 5 | PASS 256 ]
      
      ══ Skipped tests ═══════════════════════════════════════════════════════════════
      • On CRAN (5)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test-head-to-head.R:168:3): h2h_mat allows multiple Head-to-Head functions ──
      `h2h_mat(cr_data)` produced warnings.
      
      [ FAIL 1 | WARN 16 | SKIP 5 | PASS 256 ]
      Error: Test failures
      Execution halted
    ```

# confoundr

<details>

* Version: 1.2
* GitHub: https://github.com/jwjackson/confoundr
* Source code: https://github.com/cran/confoundr
* Date/Publication: 2019-09-20 04:40:02 UTC
* Number of recursive dependencies: 83

Run `cloud_details(, "confoundr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘confoundr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: makehistory.one
    > ### Title: Function to create exposure history for a single time varying
    > ###   exposure.
    > ### Aliases: makehistory.one
    > 
    > ### ** Examples
    > 
    ...
    > mydata.history <- makehistory.one(input=mydata.wide,
    +                                  id="id",
    +                                   times=c(0,1,2),
    +                                   exposure="a",
    +                                   name.history="h"
    +                                   )
    Error in `$<-.data.frame`(`*tmp*`, "call_text", value = c("confoundr::makehistory.one(...)",  : 
      replacement has 27 rows, data has 25
    Calls: makehistory.one ... trace_format -> trace_as_tree -> $<- -> $<-.data.frame
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── Error (testMakeHistoryTwo.r:71:3): Formats and Names under Dropout ──────────
      <dplyr:::mutate_error/rlang_error/error/condition>
      Error in `mutate(., his.name.a = name.history.a, his.name.b = name.history.b, 
          his.time.a = .data$exp.time, his.time.b = .data$exp.time, 
          his.lag = if_else(.data$exp.time == first(.data$exp.time, 
              default = "NA"), "H", lag(paste(.data$exp.value.a, .data$exp.value.b, 
              sep = ""))), his.value.a = CumPaste(.data$his.lag), his.value.b = paste(CumPaste(.data$his.lag), 
              .data$exp.value.a, sep = ""))`: Problem while computing `his.lag = if_else(...)`.
      ℹ The error occurred in group 1: ID = 1.
      Caused by error in `nth()`:
      ! Can't convert `default` <character> to match type of `x` <double>.
      
      [ FAIL 4 | WARN 28 | SKIP 0 | PASS 342 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘quickdemo.Rmd’ using rmarkdown
    Quitting from lines 38-46 (quickdemo.Rmd) 
    Error: processing vignette 'quickdemo.Rmd' failed with diagnostics:
    Problem while computing `his.lag = if_else(...)`.
    ℹ The error occurred in group 1: ID = 1.
    Caused by error in `nth()`:
    ! Can't convert `default` <character> to match type of `x` <double>.
    --- failed re-building ‘quickdemo.Rmd’
    ...
    ℹ The error occurred in group 1: ID = 1001.
    Caused by error in `nth()`:
    ! Can't convert `default` <character> to match type of `x` <double>.
    --- failed re-building ‘selectionbias.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘quickdemo.Rmd’ ‘selectionbias.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# crosstable

<details>

* Version: 0.4.1
* GitHub: https://github.com/DanChaltiel/crosstable
* Source code: https://github.com/cran/crosstable
* Date/Publication: 2022-02-25 12:20:03 UTC
* Number of recursive dependencies: 120

Run `cloud_details(, "crosstable")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       13.       │     └─base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
       14.       │       └─base (local) doTryCatch(return(expr), name, parentenv, handler)
       15.       └─tidyselect:::vars_select_eval(...)
       16.         └─tidyselect:::walk_data_tree(expr, data_mask, context_mask)
       17.           └─tidyselect:::eval_c(expr, data_mask, context_mask)
       18.             └─tidyselect:::reduce_sels(node, data_mask, context_mask, init = init)
       19.               └─tidyselect:::walk_data_tree(new, data_mask, context_mask)
       20.                 └─tidyselect:::as_indices_sel_impl(...)
       21.                   └─tidyselect:::check_predicate_output(xs[[i]], i, call = call)
       22.                     └─cli::cli_abort(...)
       23.                       └─rlang::abort(message, ..., call = call, use_cli_format = TRUE)
      
      [ FAIL 1 | WARN 1 | SKIP 26 | PASS 331 ]
      Error: Test failures
      Execution halted
    ```

# cubble

<details>

* Version: 0.1.1
* GitHub: https://github.com/huizezhang-sherry/cubble
* Source code: https://github.com/cran/cubble
* Date/Publication: 2022-06-02 12:30:06 UTC
* Number of recursive dependencies: 136

Run `cloud_details(, "cubble")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘aggregation.Rmd’ using rmarkdown
    `summarise()` has grouped output by 'id'. You can override using the `.groups`
    argument.
    `summarise()` has grouped output by 'cluster', 'id'. You can override using the
    `.groups` argument.
    `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'
    Adding missing grouping variables: `id`
    Warning: Each row in `x` should match at most 1 row in `y`.
    ℹ Row 1 of `x` matches multiple rows.
    ...
    ℹ The error occurred in row 1.
    Caused by error in `between()`:
    ! Can't convert `left` <double> to <character>.
    --- failed re-building ‘matching.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘cubble.Rmd’ ‘matching.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# disk.frame

<details>

* Version: 0.7.2
* GitHub: https://github.com/DiskFrame/disk.frame
* Source code: https://github.com/cran/disk.frame
* Date/Publication: 2022-03-07 11:40:02 UTC
* Number of recursive dependencies: 103

Run `cloud_details(, "disk.frame")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘disk.frame-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: anti_join.disk.frame
    > ### Title: Performs join/merge for disk.frames
    > ### Aliases: anti_join.disk.frame full_join.disk.frame
    > ###   inner_join.disk.frame left_join.disk.frame semi_join.disk.frame
    > 
    > ### ** Examples
    > 
    ...
    Warning in anti_join.disk.frame(df.df, df2.df) :
      merge_by_chunk_id = FALSE. This will take significantly longer and the preparations needed are performed eagerly which may lead to poor performance. Consider making y a data.frame or set merge_by_chunk_id = TRUE for better performance.
    Appending disk.frames: 
    Appending disk.frames: 
    Error: Arguments in `...` must be used.
    ✖ Problematic arguments:
    • ..1 = xch
    • ..2 = ych
    • overwrite = overwrite
    Execution halted
    ```

# divseg

<details>

* Version: 0.0.4
* GitHub: https://github.com/christopherkenny/divseg
* Source code: https://github.com/cran/divseg
* Date/Publication: 2021-08-09 07:00:05 UTC
* Number of recursive dependencies: 58

Run `cloud_details(, "divseg")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘divseg-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ds_abs_cent
    > ### Title: Compute Absolute Centralization
    > ### Aliases: ds_abs_cent abs_cent
    > 
    > ### ** Examples
    > 
    > data("de_county")
    > ds_abs_cent(de_county, c(pop_white, starts_with('pop_')))
    Error in `$<-.data.frame`(`*tmp*`, "call_text", value = c("divseg::ds_abs_cent(de_county, c(pop_white, starts_with(\"pop_\")))",  : 
      replacement has 19 rows, data has 18
    Calls: ds_abs_cent ... trace_format -> trace_as_tree -> $<- -> $<-.data.frame
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Error in `.gmat * .c`: non-numeric argument to binary operator
      Backtrace:
          ▆
       1. └─divseg::ds_spat_prox(de_county, c(pop_black, starts_with("pop_"))) at test-spat_prox.R:4:2
       2.   └─divseg:::calc_pgg(.data, sub %>% dplyr::pull(.data$.x))
      ── Error (test-spat_prox.R:10:3): spat_prox .name works ────────────────────────
      Error in `.gmat * .c`: non-numeric argument to binary operator
      Backtrace:
          ▆
       1. └─divseg::ds_spat_prox(...) at test-spat_prox.R:10:2
       2.   └─divseg:::calc_pgg(.data, sub %>% dplyr::pull(.data$.x))
      
      [ FAIL 32 | WARN 12 | SKIP 0 | PASS 26 ]
      Error: Test failures
      Execution halted
    ```

# dm

<details>

* Version: 1.0.0
* GitHub: https://github.com/cynkra/dm
* Source code: https://github.com/cran/dm
* Date/Publication: 2022-07-21 18:00:02 UTC
* Number of recursive dependencies: 152

Run `cloud_details(, "dm")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      <dplyr:::mutate_error/rlang_error/error/condition>
      Error in `mutate(., zoom = if_else(table == "tf_1", list(1), NULL))`: Problem while computing `zoom = if_else(table == "tf_1", list(1), NULL)`.
      Caused by error in `if_else()`:
      ! `false` must be a vector, not NULL.
      ── Error (test-zoom.R:99:3): dm_update_tbl() works ─────────────────────────────
      <dplyr:::mutate_error/rlang_error/error/condition>
      Error in `mutate(., zoom = if_else(table == "tf_6", list(tf_7()), NULL), 
          col_tracker_zoom = if_else(table == "tf_6", list(character()), 
              NULL), )`: Problem while computing `zoom = if_else(table == "tf_6", list(tf_7()), NULL)`.
      Caused by error in `if_else()`:
      ! `false` must be a vector, not NULL.
      
      [ FAIL 3 | WARN 24 | SKIP 186 | PASS 1315 ]
      Error: Test failures
      Execution halted
    ```

*   checking R code for possible problems ... NOTE
    ```
    dm_meta_raw: no visible global function definition for ‘sql’
    get_names_table_mariadb: no visible global function definition for
      ‘sql’
    get_names_table_mssql: no visible global function definition for ‘sql’
    get_names_table_postgres: no visible global function definition for
      ‘sql’
    mssql_sys_db: no visible global function definition for ‘sql’
    schema_mariadb: no visible global function definition for ‘sql’
    tbl_lc: no visible global function definition for ‘sql’
    Undefined global functions or variables:
      sql
    ```

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘dm-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: dm_flatten_to_tbl
    > ### Title: Flatten a part of a 'dm' into a wide table
    > ### Aliases: dm_flatten_to_tbl
    > 
    > ### ** Examples
    > 
    > 
    ...
      8. │   └─dm:::check_dm(dm)
      9. │     └─dm::is_dm(dm)
     10. ├─dm::dm_financial()
     11. │ ├─base::withVisible(eval(mc, parent.frame()))
     12. │ └─base::eval(mc, parent.frame())
     13. │   └─base::eval(mc, parent.frame())
     14. └─dm (local) `<fn>`()
     15.   └─dm:::financial_db_con()
     16.     └─rlang::abort(...)
    Execution halted
    ```

# dodgr

<details>

* Version: 0.2.14
* GitHub: https://github.com/ATFutures/dodgr
* Source code: https://github.com/cran/dodgr
* Date/Publication: 2022-06-08 14:40:06 UTC
* Number of recursive dependencies: 109

Run `cloud_details(, "dodgr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘sc-conversion-fns.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > 
      > test_check("dodgr")
      [ FAIL 1 | WARN 0 | SKIP 10 | PASS 199 ]
      
      ══ Skipped tests ═══════════════════════════════════════════════════════════════
      • !test_all is TRUE (1)
      • On CRAN (9)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test-iso.R:16:15): isodists ────────────────────────────────────────
      `net <- weight_streetnet(hsc, wt_profile = "bicycle")` produced warnings.
      
      [ FAIL 1 | WARN 0 | SKIP 10 | PASS 199 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 31.1Mb
      sub-directories of 1Mb or more:
        doc    5.2Mb
        libs  25.3Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# epocakir

<details>

* Version: 0.9.8
* GitHub: https://github.com/alwinw/epocakir
* Source code: https://github.com/cran/epocakir
* Date/Publication: 2022-05-04 23:00:16 UTC
* Number of recursive dependencies: 76

Run `cloud_details(, "epocakir")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘epocakir-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: GFR_staging
    > ### Title: GFR Staging
    > ### Aliases: GFR_staging GFR_staging.data.frame GFR_staging.units
    > ###   GFR_staging.numeric
    > 
    > ### ** Examples
    > 
    > df <- tibble::tibble(
    +   eGFR = units::set_units(c(-1, NA, 100, 70, 50, 35, 20, 10), "mL/min/1.73m2")
    + )
    > 
    > GFR_staging(df, "eGFR")
    Error in `$<-.data.frame`(`*tmp*`, "call_text", value = c("epocakir::GFR_staging(df, \"eGFR\")",  : 
      replacement has 17 rows, data has 15
    Calls: GFR_staging ... trace_format -> trace_as_tree -> $<- -> $<-.data.frame
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(epocakir)
      > 
      > test_check("epocakir")
      [ FAIL 3 | WARN 0 | SKIP 0 | PASS 390 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      Error in `$<-.data.frame`(`*tmp*`, "call_text", value = c("epocakir::GFR_staging(df, \"eGFR\")",  : 
        replacement has 17 rows, data has 15
      Calls: test_check ... trace_format -> trace_as_tree -> $<- -> $<-.data.frame
      Execution halted
    ```

# etl

<details>

* Version: 0.4.0
* GitHub: https://github.com/beanumber/etl
* Source code: https://github.com/cran/etl
* Date/Publication: 2021-05-17 21:50:14 UTC
* Number of recursive dependencies: 109

Run `cloud_details(, "etl")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘etl-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: etl
    > ### Title: Initialize an 'etl' object
    > ### Aliases: etl etl.default summary.etl is.etl print.etl
    > 
    > ### ** Examples
    > 
    > 
    > # Instantiate the etl object
    > cars <- etl("mtcars")
    No database was specified so I created one for you at:
    /tmp/Rtmp6UMnkY/file3b1637b77f44.sqlite3
    Error: `src_sqlite()` was deprecated in dplyr 1.0.0 and is now defunct.
    Please load dbplyr and use `tbl()` directly with a database connection
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        1. ├─testthat::expect_message(...) at test-etl.R:91:2
        2. │ └─testthat:::quasi_capture(enquo(object), label, capture_messages)
        3. │   ├─testthat (local) .capture(...)
        4. │   │ └─base::withCallingHandlers(...)
        5. │   └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
        6. ├─etl::etl("mtcars", dir = file.path(tempdir(), "etltest"))
        7. └─etl:::etl.default("mtcars", dir = file.path(tempdir(), "etltest"))
        8.   └─etl:::verify_con(db, dir)
        9.     └─dplyr::src_sqlite(path = sqlite_file, create = TRUE)
       10.       └─lifecycle::deprecate_stop("1.0.0", "dplyr::src_sqlite()", details = "Please load dbplyr and use `tbl()` directly with a database connection")
       11.         └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 4 | WARN 0 | SKIP 3 | PASS 10 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘extending_etl.Rmd’ using rmarkdown
    Loading required package: dplyr
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    ...
    Error: processing vignette 'using_etl.Rmd' failed with diagnostics:
    `src_sqlite()` was deprecated in dplyr 1.0.0 and is now defunct.
    Please load dbplyr and use `tbl()` directly with a database connection
    --- failed re-building ‘using_etl.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘extending_etl.Rmd’ ‘using_etl.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# exuber

<details>

* Version: 0.4.2
* GitHub: https://github.com/kvasilopoulos/exuber
* Source code: https://github.com/cran/exuber
* Date/Publication: 2020-12-18 07:30:19 UTC
* Number of recursive dependencies: 99

Run `cloud_details(, "exuber")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       32. │           └─base (local) withOneRestart(expr, restarts[[1L]])
       33. │             └─base (local) doWithOneRestart(return(expr), restart)
       34. └─dplyr (local) `<fn>`(`<vctrs___>`)
       35.   └─dplyr:::rethrow_warning_join_matches_multiple(cnd)
       36.     └─dplyr:::warn_join(...)
       37.       └─dplyr:::warn_dplyr(...)
       38.         └─rlang::warn(...)
       39.           └─base::warning(cnd)
       40.             └─base::withRestarts(...)
       41.               └─base (local) withOneRestart(expr, restarts[[1L]])
       42.                 └─base (local) doWithOneRestart(return(expr), restart)
      
      [ FAIL 42 | WARN 50 | SKIP 4 | PASS 155 ]
      Error: Test failures
      Execution halted
    ```

# farr

<details>

* Version: 0.2.27
* GitHub: NA
* Source code: https://github.com/cran/farr
* Date/Publication: 2022-06-30 06:10:02 UTC
* Number of recursive dependencies: 75

Run `cloud_details(, "farr")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... WARNING
    ```
    Missing or unexported object: ‘dplyr::sql’
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1430 marked UTF-8 strings
    ```

# ferrn

<details>

* Version: 0.0.1
* GitHub: https://github.com/huizezhang-sherry/ferrn
* Source code: https://github.com/cran/ferrn
* Date/Publication: 2021-03-17 13:20:16 UTC
* Number of recursive dependencies: 104

Run `cloud_details(, "ferrn")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ferrn-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: explore_space_tour
    > ### Title: Plot the grand tour animation of the bases space in high
    > ###   dimension
    > ### Aliases: explore_space_tour prep_space_tour
    > 
    > ### ** Examples
    > 
    ...
      9. │   ├─base::withCallingHandlers(...)
     10. │   └─mask$eval_all_mutate(quo)
     11. │     └─dplyr (local) eval()
     12. └─dplyr::lead(.data$id, defualt = NA)
     13.   └─rlang::check_dots_empty0(...)
     14.     └─rlang::check_dots_empty(call = call)
     15.       └─rlang:::action_dots(...)
     16.         ├─base (local) try_dots(...)
     17.         └─rlang (local) action(...)
    Execution halted
    ```

# finnts

<details>

* Version: 0.2.0
* GitHub: https://github.com/microsoft/finnts
* Source code: https://github.com/cran/finnts
* Date/Publication: 2022-07-14 15:40:05 UTC
* Number of recursive dependencies: 204

Run `cloud_details(, "finnts")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test-forecast_time_series.R:195:3): final forecast data rows are meaningful ──
      nrow(future_frame) (`actual`) not equal to length(unique(inp_data_combos$Combo)) * forecast_horizon (`expected`).
      
        `actual`:  8
      `expected`: 15
      ── Failure (test-forecast_time_series.R:294:3): final forecast data rows are meaningful ──
      nrow(future_frame) (`actual`) not equal to length(unique(inp_data_combos$Combo)) * forecast_horizon (`expected`).
      
        `actual`: 27
      `expected`: 48
      
      [ FAIL 2 | WARN 21 | SKIP 0 | PASS 111 ]
      Error: Test failures
      Execution halted
    ```

# forestmangr

<details>

* Version: 0.9.4
* GitHub: https://github.com/sollano/forestmangr
* Source code: https://github.com/cran/forestmangr
* Date/Publication: 2021-08-16 13:00:02 UTC
* Number of recursive dependencies: 122

Run `cloud_details(, "forestmangr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘forestmangr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_summarise
    > ### Title: Summarize forest inventory data
    > ### Aliases: plot_summarise
    > 
    > ### ** Examples
    > 
    > library(forestmangr)
    ...
     12. └─dplyr::na_if(., 0)
     13.   └─vctrs::vec_cast(x = y, to = x, x_arg = "y", to_arg = "x")
     14.     └─vctrs (local) `<fn>`()
     15.       └─vctrs::vec_default_cast(...)
     16.         └─vctrs::stop_incompatible_cast(...)
     17.           └─vctrs::stop_incompatible_type(...)
     18.             └─vctrs:::stop_incompatible(...)
     19.               └─vctrs:::stop_vctrs(...)
     20.                 └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘eq_group_fit_en.Rmd’ using rmarkdown
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    ...
        intersect, setdiff, setequal, union
    
    --- finished re-building ‘yield_growth_ptbr.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘invent_vol_plot_en.Rmd’ ‘invent_vol_plot_ptbr.Rmd’ ‘sampling_en.Rmd’
      ‘sampling_ptbr.Rmd’ ‘volume_est_en.Rmd’ ‘volume_est_ptbr.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# FRK

<details>

* Version: 2.0.5
* GitHub: https://github.com/andrewzm/FRK
* Source code: https://github.com/cran/FRK
* Date/Publication: 2022-03-25 08:10:08 UTC
* Number of recursive dependencies: 153

Run `cloud_details(, "FRK")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       3.   └─FRK:::map_data_to_BAUs(...)
       4.     └─FRK (local) .local(data_sp, sp_pols, average_in_BAU, sum_variables, silently)
       5.       └─base::lapply(...)
       6.         └─FRK (local) FUN(X[[i]], ...)
       7.           └─xts:::Ops.xts(i, last(sp_pols@time))
       8.             └─xts::.xts(e, .index(e2), tclass(e2), tzone(e2), tformat = tformat(e2))
      
      [ FAIL 2 | WARN 12 | SKIP 0 | PASS 203 ]
      Error: Test failures
      In addition: Warning message:
      In checkMatrixPackageVersion() : Package version inconsistency detected.
      TMB was built with Matrix version 1.4.1
      Current Matrix version is 1.3.4
      Please re-install 'TMB' from source using install.packages('TMB', type = 'source') or ask CRAN for a binary version of 'TMB' matching CRAN's 'Matrix' package
      Execution halted
    ```

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘FRK_intro.Rnw’ using knitr
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    ...
    l.70 ^^M
            
    !  ==> Fatal error occurred, no output PDF file produced!
    --- failed re-building ‘FRK_non-Gaussian.Rnw’
    
    SUMMARY: processing the following files failed:
      ‘FRK_intro.Rnw’ ‘FRK_non-Gaussian.Rnw’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 85.1Mb
      sub-directories of 1Mb or more:
        data   5.0Mb
        doc    1.7Mb
        libs  77.4Mb
    ```

# funneljoin

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/funneljoin
* Date/Publication: 2019-12-20 14:30:02 UTC
* Number of recursive dependencies: 71

Run `cloud_details(, "funneljoin")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... WARNING
    ```
    Namespace in Imports field not imported from: ‘tibble’
      All declared Imports should be used.
    Missing or unexported object: ‘dplyr::sql’
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tibble’
      All declared Imports should be used.
    ```

# geospark

<details>

* Version: 0.3.1
* GitHub: https://github.com/harryprince/geospark
* Source code: https://github.com/cran/geospark
* Date/Publication: 2020-03-02 05:40:02 UTC
* Number of recursive dependencies: 67

Run `cloud_details(, "geospark")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘geospark-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: st_example
    > ### Title: Spark geometry example.
    > ### Aliases: st_example
    > 
    > ### ** Examples
    > 
    > library(geospark)
    ...
      3. ├─dplyr::mutate(., geom = dplyr::sql("st_geomfromwkt(geom)"))
      4. ├─dplyr:::mutate.data.frame(., geom = dplyr::sql("st_geomfromwkt(geom)"))
      5. │ └─dplyr:::mutate_cols(.data, dplyr_quosures(...), caller_env = caller_env())
      6. │   ├─base::withCallingHandlers(...)
      7. │   └─mask$eval_all_mutate(quo)
      8. │     └─dplyr (local) eval()
      9. └─base::.handleSimpleError(...)
     10.   └─dplyr (local) h(simpleError(msg, call))
     11.     └─rlang::abort(...)
    Execution halted
    ```

*   checking dependencies in R code ... WARNING
    ```
    Namespace in Imports field not imported from: ‘dbplyr’
      All declared Imports should be used.
    Missing or unexported object: ‘dplyr::sql’
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dbplyr’
      All declared Imports should be used.
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# gmgm

<details>

* Version: 1.1.1
* GitHub: NA
* Source code: https://github.com/cran/gmgm
* Date/Publication: 2022-05-27 18:40:02 UTC
* Number of recursive dependencies: 60

Run `cloud_details(, "gmgm")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       21.         └─vctrs:::vec_cast.integer.double(...)
       22.           └─vctrs::maybe_lossy_cast(...)
       23.             ├─base::withRestarts(...)
       24.             │ └─base (local) withOneRestart(expr, restarts[[1L]])
       25.             │   └─base (local) doWithOneRestart(return(expr), restart)
       26.             └─vctrs:::stop_lossy_cast(...)
       27.               └─vctrs::stop_incompatible_cast(...)
       28.                 └─vctrs::stop_incompatible_type(...)
       29.                   └─vctrs:::stop_incompatible(...)
       30.                     └─vctrs:::stop_vctrs(...)
       31.                       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
      
      [ FAIL 57 | WARN 6 | SKIP 0 | PASS 353 ]
      Error: Test failures
      Execution halted
    ```

# groupr

<details>

* Version: 0.1.0
* GitHub: https://github.com/ngriffiths21/groupr
* Source code: https://github.com/cran/groupr
* Date/Publication: 2020-10-14 12:30:06 UTC
* Number of recursive dependencies: 63

Run `cloud_details(, "groupr")` for more info

</details>

## Newly broken

*   checking whether package ‘groupr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/groupr/new/groupr.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

## Installation

### Devel

```
* installing *source* package ‘groupr’ ...
** package ‘groupr’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘tbl_sum’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘groupr’
* removing ‘/tmp/workdir/groupr/new/groupr.Rcheck/groupr’


```
### CRAN

```
* installing *source* package ‘groupr’ ...
** package ‘groupr’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (groupr)


```
# hablar

<details>

* Version: 0.3.0
* GitHub: https://github.com/davidsjoberg/hablar
* Source code: https://github.com/cran/hablar
* Date/Publication: 2020-03-19 22:40:02 UTC
* Number of recursive dependencies: 90

Run `cloud_details(, "hablar")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(hablar)
      > test_check("hablar")
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 416 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test.ifs.R:53:3): if_else_ ─────────────────────────────────────────
      `if_else_(c(T, F, NA), 1, 1L)` did not throw an error.
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 416 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# headliner

<details>

* Version: 0.0.2
* GitHub: https://github.com/rjake/headliner
* Source code: https://github.com/cran/headliner
* Date/Publication: 2022-06-26 23:40:02 UTC
* Number of recursive dependencies: 71

Run `cloud_details(, "headliner")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        5. │   └─dplyr:::filter_rows(.data, ..., caller_env = caller_env())
        6. │     └─dplyr:::filter_eval(dots, mask = mask, error_call = error_call)
        7. │       ├─base::withCallingHandlers(...)
        8. │       └─mask$eval_all_filter(dots, env_filter)
        9. │         └─dplyr (local) eval()
       10. └─base::.handleSimpleError(`<fn>`, "object '' not found", base::quote(eval()))
       11.   └─dplyr (local) h(simpleError(msg, call))
       12.     └─dplyr:::local_error_context(...)
       13.       └─dplyr:::quo_as_label(dots[[.index]])
       14.         └─dplyr:::is_data_pronoun(expr)
       15.           └─rlang::is_call(x, c("[[", "$"))
      
      [ FAIL 3 | WARN 0 | SKIP 0 | PASS 101 ]
      Error: Test failures
      Execution halted
    ```

# heemod

<details>

* Version: 0.14.4
* GitHub: https://github.com/pierucci/heemod
* Source code: https://github.com/cran/heemod
* Date/Publication: 2021-10-06 11:30:12 UTC
* Number of recursive dependencies: 110

Run `cloud_details(, "heemod")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in documentation object 'define_parameters.Rd':
      ‘[dplyr:ranking]{dplyr::row_number()}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘rgho’
    ```

# helda

<details>

* Version: 1.1.5
* GitHub: https://github.com/Redcart/helda
* Source code: https://github.com/cran/helda
* Date/Publication: 2021-01-06 11:00:16 UTC
* Number of recursive dependencies: 126

Run `cloud_details(, "helda")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘helda-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: kmeans_procedure
    > ### Title: K-means procedure
    > ### Aliases: kmeans_procedure
    > ### Keywords: cluster kmeans sizes
    > 
    > ### ** Examples
    > 
    ...
        intersect, setdiff, setequal, union
    
    > data <- iris %>% select(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
    > features <- colnames(data)
    > result <- kmeans_procedure(data = data, columns = features, threshold_min = 2, threshold = 10,
    + verbose=FALSE, seed=10)
    Error in `$<-.data.frame`(`*tmp*`, "call_text", value = c("helda::kmeans_procedure(...)",  : 
      replacement has 11 rows, data has 10
    Calls: kmeans_procedure ... trace_format -> trace_as_tree -> $<- -> $<-.data.frame
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked UTF-8 string
    ```

# hlaR

<details>

* Version: 0.1.3
* GitHub: https://github.com/LarsenLab/hlaR
* Source code: https://github.com/cran/hlaR
* Date/Publication: 2022-05-13 21:00:02 UTC
* Number of recursive dependencies: 152

Run `cloud_details(, "hlaR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘hlaR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: CalEpletMHCI
    > ### Title: Calculate class I HLA eplet mismatch
    > ### Aliases: CalEpletMHCI
    > 
    > ### ** Examples
    > 
    > dat<-read.csv(system.file("extdata/example","MHC_I_test.csv",package="hlaR"),sep=",",header=TRUE)
    ...
     10. └─dplyr (local) `<fn>`(index, "")
     11.   └─vctrs::vec_cast(x = y, to = x, x_arg = "y", to_arg = "x")
     12.     └─vctrs (local) `<fn>`()
     13.       └─vctrs::vec_default_cast(...)
     14.         └─vctrs::stop_incompatible_cast(...)
     15.           └─vctrs::stop_incompatible_type(...)
     16.             └─vctrs:::stop_incompatible(...)
     17.               └─vctrs:::stop_vctrs(...)
     18.                 └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘allele-haplotype.Rmd’ using rmarkdown
    ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ✔ ggplot2 3.3.6           ✔ dplyr   1.0.99.9000
    ✔ tibble  3.1.8           ✔ stringr 1.4.0      
    ✔ tidyr   1.2.0           ✔ forcats 0.5.1      
    ✔ purrr   0.3.4           
    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    ...
    Problem while computing `index = (function (x, y) ...`.
    Caused by error:
    ! Can't convert `y` <character> to match type of `x` <integer>.
    --- failed re-building ‘eplet-mm.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘eplet-mm.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘here’ ‘readxlsb’ ‘usethis’
      All declared Imports should be used.
    ```

# huito

<details>

* Version: 0.2.0
* GitHub: https://github.com/flavjack/huito
* Source code: https://github.com/cran/huito
* Date/Publication: 2022-06-24 10:40:02 UTC
* Number of recursive dependencies: 159

Run `cloud_details(, "huito")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘germinar.Rmd’ using rmarkdown
    Loading required package: magick
    Linking to ImageMagick 6.9.10.23
    Enabled features: fontconfig, freetype, fftw, lcms, pango, webp, x11
    Disabled features: cairo, ghostscript, heic, raw, rsvg
    Using 8 threads
    Loading required package: cowplot
    --- finished re-building ‘germinar.Rmd’
    
    ...
    Disabled features: cairo, ghostscript, heic, raw, rsvg
    Using 8 threads
    Loading required package: cowplot
    --- finished re-building ‘stickers.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘huito.Rmd’ ‘labels.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# huxtable

<details>

* Version: 5.5.0
* GitHub: https://github.com/hughjonesd/huxtable
* Source code: https://github.com/cran/huxtable
* Date/Publication: 2022-06-15 11:30:02 UTC
* Number of recursive dependencies: 165

Run `cloud_details(, "huxtable")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘huxtable-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: by_cases
    > ### Title: Map cell contents to properties using 'case_when'
    > ### Aliases: by_cases
    > 
    > ### ** Examples
    > 
    > if (! requireNamespace("dplyr")) {
    ...
     1. ├─huxtable::map_background_color(...)
     2. │ └─huxtable (local) fn(ht, rc$row, rc$col, current)
     3. │   └─dplyr::case_when(!!!cases)
     4. │     └─dplyr:::case_when_formula_evaluate(...)
     5. │       └─vctrs::vec_size_common(...)
     6. └─vctrs::stop_incompatible_size(...)
     7.   └─vctrs:::stop_incompatible(...)
     8.     └─vctrs:::stop_vctrs(...)
     9.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        1. ├─testthat::expect_equivalent(...) at test-mapping-functions.R:123:2
        2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
        3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
        4. ├─huxtable (local) f(m, 1:3, 1:2, ct)
        5. │ └─dplyr::case_when(!!!cases)
        6. │   └─dplyr:::case_when_formula_evaluate(...)
        7. │     └─vctrs::vec_size_common(...)
        8. └─vctrs::stop_incompatible_size(...)
        9.   └─vctrs:::stop_incompatible(...)
       10.     └─vctrs:::stop_vctrs(...)
       11.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
      
      [ FAIL 2 | WARN 2 | SKIP 25 | PASS 1230 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘design-principles.Rmd’ using rmarkdown
    --- finished re-building ‘design-principles.Rmd’
    
    --- re-building ‘huxreg.Rmd’ using rmarkdown
    --- finished re-building ‘huxreg.Rmd’
    
    --- re-building ‘huxtable.Rmd’ using rmarkdown
    
    Attaching package: 'dplyr'
    ...
    --- failed re-building ‘huxtable.Rmd’
    
    --- re-building ‘themes.Rhtml’ using knitr
    --- finished re-building ‘themes.Rhtml’
    
    SUMMARY: processing the following file failed:
      ‘huxtable.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘R6’ ‘xml2’
      All declared Imports should be used.
    ```

# iNZightTools

<details>

* Version: 1.12.2
* GitHub: https://github.com/iNZightVIT/iNZightTools
* Source code: https://github.com/cran/iNZightTools
* Date/Publication: 2022-01-18 23:32:42 UTC
* Number of recursive dependencies: 86

Run `cloud_details(, "iNZightTools")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      x Problematic argument:
      * suffix = c(".x", ".y")
      Backtrace:
          ▆
       1. ├─d1 %>% ...
       2. └─dplyr::anti_join(...)
       3.   └─rlang (local) `<fn>`()
       4.     └─rlang:::check_dots(env, error, action, call)
       5.       └─rlang:::action_dots(...)
       6.         ├─base (local) try_dots(...)
       7.         └─rlang (local) action(...)
      
      [ FAIL 2 | WARN 0 | SKIP 4 | PASS 333 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘tidyverse’
    ```

# itraxR

<details>

* Version: 1.4
* GitHub: https://github.com/tombishop1/itraxR
* Source code: https://github.com/cran/itraxR
* Date/Publication: 2021-08-17 09:10:02 UTC
* Number of recursive dependencies: 66

Run `cloud_details(, "itraxR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘itraxR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: itrax_import
    > ### Title: Import Itrax core-scanner result file
    > ### Aliases: itrax_import
    > 
    > ### ** Examples
    > 
    > itrax_import(
    ...
     15. └─dplyr::na_if(., 0)
     16.   └─vctrs::vec_cast(x = y, to = x, x_arg = "y", to_arg = "x")
     17.     └─vctrs (local) `<fn>`()
     18.       └─vctrs::vec_default_cast(...)
     19.         └─vctrs::stop_incompatible_cast(...)
     20.           └─vctrs::stop_incompatible_type(...)
     21.             └─vctrs:::stop_incompatible(...)
     22.               └─vctrs:::stop_vctrs(...)
     23.                 └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
    Execution halted
    ```

# janitor

<details>

* Version: 2.1.0
* GitHub: https://github.com/sfirke/janitor
* Source code: https://github.com/cran/janitor
* Date/Publication: 2021-01-05 01:10:04 UTC
* Number of recursive dependencies: 72

Run `cloud_details(, "janitor")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error (test-adorn-percentages.R:69:3): works with totals row/col when denominator = col or all, #357 ──
      Error in `FUN(X[[i]], ...)`: only defined on a data frame with all numeric-alike variables
      Backtrace:
          ▆
       1. ├─source1 %>% adorn_totals(where = c("col", "row")) %>% ... at test-adorn-percentages.R:69:2
       2. └─janitor::adorn_percentages(., denominator = "col")
       3.   └─base::Summary.data.frame(`<tabyl[,3]>`, na.rm = FALSE)
       4.     └─base::lapply(...)
       5.       └─base (local) FUN(X[[i]], ...)
      
      [ FAIL 1 | WARN 0 | SKIP 1 | PASS 639 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘janitor.Rmd’ using rmarkdown
    
    Attaching package: 'janitor'
    
    The following objects are masked from 'package:stats':
    
        chisq.test, fisher.test
    
    --- finished re-building ‘janitor.Rmd’
    ...
    Quitting from lines 239-251 (tabyls.Rmd) 
    Error: processing vignette 'tabyls.Rmd' failed with diagnostics:
    only defined on a data frame with all numeric-alike variables
    --- failed re-building ‘tabyls.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘tabyls.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# JumpeR

<details>

* Version: 0.3.0
* GitHub: NA
* Source code: https://github.com/cran/JumpeR
* Date/Publication: 2021-11-16 19:40:02 UTC
* Number of recursive dependencies: 60

Run `cloud_details(, "JumpeR")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       10. ├─dplyr::mutate(., Row_Numb = as.numeric(Row_Numb))
       11. ├─dplyr::mutate(...)
       12. └─dplyr::na_if(., 10000)
       13.   └─vctrs::vec_cast(x = y, to = x, x_arg = "y", to_arg = "x")
       14.     └─vctrs (local) `<fn>`()
       15.       └─vctrs::vec_default_cast(...)
       16.         └─vctrs::stop_incompatible_cast(...)
       17.           └─vctrs::stop_incompatible_type(...)
       18.             └─vctrs:::stop_incompatible(...)
       19.               └─vctrs:::stop_vctrs(...)
       20.                 └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
      
      [ FAIL 3 | WARN 0 | SKIP 42 | PASS 1 ]
      Error: Test failures
      Execution halted
    ```

# lans2r

<details>

* Version: 1.1.0
* GitHub: https://github.com/KopfLab/lans2r
* Source code: https://github.com/cran/lans2r
* Date/Publication: 2020-06-24 05:20:03 UTC
* Number of recursive dependencies: 85

Run `cloud_details(, "lans2r")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      [ FAIL 4 | WARN 5 | SKIP 0 | PASS 140 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test-load-data.R:24:3): test that it is possible to load multiple LANS summaries ──
      `... <- NULL` produced warnings.
      ── Failure (test-load-data.R:43:3): test that it is possible to load multiple LANS summaries ──
      `... <- NULL` produced warnings.
      ── Failure (test-load-data.R:58:3): test that it is possible to load multiple LANS summaries ──
      `... <- NULL` produced warnings.
      ── Failure (test-load-data.R:81:3): test that it is possible to load LANS maps ──
      `... <- NULL` produced warnings.
      
      [ FAIL 4 | WARN 5 | SKIP 0 | PASS 140 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# lillies

<details>

* Version: 0.2.9
* GitHub: NA
* Source code: https://github.com/cran/lillies
* Date/Publication: 2021-02-16 17:10:05 UTC
* Number of recursive dependencies: 86

Run `cloud_details(, "lillies")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘lillies-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: lyl_aggregated_range
    > ### Title: Life Years Lost at a range of different ages using aggregated
    > ###   data.
    > ### Aliases: lyl_aggregated_range
    > 
    > ### ** Examples
    > 
    ...
        ▆
     1. └─lillies::lyl_aggregated_range(...)
     2.   ├─dplyr::mutate(...)
     3.   └─dplyr::left_join(LYL, ages_onset, by = "age", all.x = T)
     4.     └─rlang (local) `<fn>`()
     5.       └─rlang:::check_dots(env, error, action, call)
     6.         └─rlang:::action_dots(...)
     7.           ├─base (local) try_dots(...)
     8.           └─rlang (local) action(...)
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘ddpcr’
      All declared Imports should be used.
    ```

# macleish

<details>

* Version: 0.3.9
* GitHub: https://github.com/beanumber/macleish
* Source code: https://github.com/cran/macleish
* Date/Publication: 2022-07-06 20:30:07 UTC
* Number of recursive dependencies: 139

Run `cloud_details(, "macleish")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘macleish-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: etl_extract.etl_macleish
    > ### Title: Extract weather data
    > ### Aliases: etl_extract.etl_macleish etl_transform.etl_macleish
    > ###   etl_transform_help
    > 
    > ### ** Examples
    > 
    > 
    > macleish <- etl("macleish")
    No database was specified so I created one for you at:
    /tmp/RtmplrtWq0/file1933645f260a.sqlite3
    Error: `src_sqlite()` was deprecated in dplyr 1.0.0 and is now defunct.
    Please load dbplyr and use `tbl()` directly with a database connection
    Execution halted
    ```

# MBNMAtime

<details>

* Version: 0.2.1
* GitHub: NA
* Source code: https://github.com/cran/MBNMAtime
* Date/Publication: 2021-09-13 15:10:02 UTC
* Number of recursive dependencies: 112

Run `cloud_details(, "MBNMAtime")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        1 
      [ FAIL 2 | WARN 0 | SKIP 13 | PASS 186 ]
      
      ══ Skipped tests ═══════════════════════════════════════════════════════════════
      • On CRAN (13)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test_plot.functions.R:350:3): timeplot functions correctly ─────────
      `timeplot(painnet, plotby = "rel")` produced warnings.
      ── Failure (test_plot.functions.R:357:3): timeplot functions correctly ─────────
      `timeplot(classnetwork, plotby = "rel", level = "class")` produced warnings.
      
      [ FAIL 2 | WARN 0 | SKIP 13 | PASS 186 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘gemtc’
    ```

# metaconfoundr

<details>

* Version: 0.1.0
* GitHub: https://github.com/malcolmbarrett/metaconfoundr
* Source code: https://github.com/cran/metaconfoundr
* Date/Publication: 2021-10-12 08:10:02 UTC
* Number of recursive dependencies: 117

Run `cloud_details(, "metaconfoundr")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘ellipsis’
      All declared Imports should be used.
    ```

# modeldb

<details>

* Version: 0.2.2
* GitHub: https://github.com/tidymodels/modeldb
* Source code: https://github.com/cran/modeldb
* Date/Publication: 2020-02-10 20:50:07 UTC
* Number of recursive dependencies: 92

Run `cloud_details(, "modeldb")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      > library(modeldb)
      > 
      > test_check("modeldb")
      [ FAIL 2 | WARN 1 | SKIP 0 | PASS 17 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test_kmeans.R:11:3): Not specifying variables works ────────────────
      `select(mtcars, wt, mpg) %>% simple_kmeans_db()` produced warnings.
      ── Failure (test_kmeans.R:23:3): Centroid argument is accepted ─────────────────
      `simple_kmeans_db(mtcars, mpg, wt, initial_kmeans = ik)` produced warnings.
      
      [ FAIL 2 | WARN 1 | SKIP 0 | PASS 17 ]
      Error: Test failures
      Execution halted
    ```

# moodleR

<details>

* Version: 1.0.0
* GitHub: https://github.com/chi2labs/moodleR
* Source code: https://github.com/cran/moodleR
* Date/Publication: 2022-03-23 17:00:02 UTC
* Number of recursive dependencies: 106

Run `cloud_details(, "moodleR")` for more info

</details>

## Newly broken

*   checking R code for possible problems ... NOTE
    ```
    mdl_forum_posts: no visible global function definition for ‘sql’
    mdl_grades: no visible global function definition for ‘sql’
    Undefined global functions or variables:
      sql
    ```

# mpathsenser

<details>

* Version: 1.0.3
* GitHub: NA
* Source code: https://github.com/cran/mpathsenser
* Date/Publication: 2022-06-01 09:50:02 UTC
* Number of recursive dependencies: 98

Run `cloud_details(, "mpathsenser")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
          is_null
      
      Could not add data to the data base. Falling back to UPSERT method
        adding: tmp/workdir/mpathsenser/new/mpathsenser.Rcheck/mpathsenser/testdata/test.json (deflated 81%)
        adding: tmp/workdir/mpathsenser/new/mpathsenser.Rcheck/mpathsenser/testdata/test.json (deflated 81%)
      [ FAIL 1 | WARN 12 | SKIP 0 | PASS 249 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test-functions.R:9:3): import ──────────────────────────────────────
      `import(path = path, db = db, recursive = FALSE)` did not throw the expected message.
      
      [ FAIL 1 | WARN 12 | SKIP 0 | PASS 249 ]
      Error: Test failures
      Execution halted
    ```

# mudata2

<details>

* Version: 1.1.2
* GitHub: https://github.com/paleolimbot/mudata2
* Source code: https://github.com/cran/mudata2
* Date/Publication: 2020-03-20 20:20:03 UTC
* Number of recursive dependencies: 99

Run `cloud_details(, "mudata2")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘mudata2-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: write_mudata
    > ### Title: Read/Write mudata objects
    > ### Aliases: write_mudata read_mudata write_mudata_zip read_mudata_zip
    > ###   write_mudata_dir read_mudata_dir write_mudata_json to_mudata_json
    > ###   read_mudata_json from_mudata_json
    > 
    > ### ** Examples
    ...
      3. │   └─mudata2:::write_mudata_common(...)
      4. │     └─mudata2::update_columns_table(md, quiet = FALSE)
      5. │       └─md$columns %>% ...
      6. └─dplyr::left_join(...)
      7.   └─rlang (local) `<fn>`()
      8.     └─rlang:::check_dots(env, error, action, call)
      9.       └─rlang:::action_dots(...)
     10.         ├─base (local) try_dots(...)
     11.         └─rlang (local) action(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘test-all.R’
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
        1. ├─mudata2::write_mudata_json(md_zero, tf_json) at test-mudata-io.R:625:2
        2. │ └─mudata2:::write_mudata_json_common(...)
        3. │   └─mudata2:::write_mudata_common(...)
        4. │     └─mudata2::update_columns_table(md, quiet = FALSE)
        5. │       └─md$columns %>% ...
        6. └─dplyr::left_join(...)
        7.   └─rlang (local) `<fn>`()
        8.     └─rlang:::check_dots(env, error, action, call)
        9.       └─rlang:::action_dots(...)
       10.         ├─base (local) try_dots(...)
       11.         └─rlang (local) action(...)
      
      [ FAIL 19 | WARN 1 | SKIP 1 | PASS 548 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘mudata2.Rmd’ using rmarkdown
    --- finished re-building ‘mudata2.Rmd’
    
    --- re-building ‘mudata_create.Rmd’ using rmarkdown
    Quitting from lines 204-206 (mudata_create.Rmd) 
    Error: processing vignette 'mudata_create.Rmd' failed with diagnostics:
    Arguments in `...` must be used.
    ✖ Problematic argument:
    • prefix = c(".x", ".y")
    --- failed re-building ‘mudata_create.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘mudata_create.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘ggplot2’
      All declared Imports should be used.
    ```

# multicolor

<details>

* Version: 0.1.5
* GitHub: https://github.com/aedobbyn/multicolor
* Source code: https://github.com/cran/multicolor
* Date/Publication: 2021-11-04 16:50:02 UTC
* Number of recursive dependencies: 68

Run `cloud_details(, "multicolor")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      ══ Skipped tests ═══════════════════════════════════════════════════════════════
      • use_color() is not TRUE (1)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test-multicolor.R:89:3): colors(), including grays, rainbow, and rbg work ──
      `suppressMessages(...)` produced warnings.
      ── Failure (test-multicolor.R:103:3): colors(), including grays, rainbow, and rbg work ──
      `suppressMessages(...)` produced warnings.
      ── Failure (test-multicolor.R:113:3): colors(), including grays, rainbow, and rbg work ──
      `suppressMessages(multi_color("asdfjkl;asdfjk;", colors = "rainbow"))` produced warnings.
      
      [ FAIL 3 | WARN 6 | SKIP 1 | PASS 27 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘cowsay’
      All declared Imports should be used.
    ```

# naniar

<details>

* Version: 0.6.1
* GitHub: https://github.com/njtierney/naniar
* Source code: https://github.com/cran/naniar
* Date/Publication: 2021-05-14 10:20:02 UTC
* Number of recursive dependencies: 180

Run `cloud_details(, "naniar")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘exploring-imputed-values.Rmd’ using rmarkdown
    --- finished re-building ‘exploring-imputed-values.Rmd’
    
    --- re-building ‘getting-started-w-naniar.Rmd’ using rmarkdown
    --- finished re-building ‘getting-started-w-naniar.Rmd’
    
    --- re-building ‘naniar-visualisation.Rmd’ using rmarkdown
    --- finished re-building ‘naniar-visualisation.Rmd’
    ...
    --- failed re-building ‘replace-with-na.Rmd’
    
    --- re-building ‘special-missing-values.Rmd’ using rmarkdown
    --- finished re-building ‘special-missing-values.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘replace-with-na.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# nonmemica

<details>

* Version: 0.9.7
* GitHub: NA
* Source code: https://github.com/cran/nonmemica
* Date/Publication: 2020-11-24 07:30:11 UTC
* Number of recursive dependencies: 67

Run `cloud_details(, "nonmemica")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘nonmemica-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: superset.character
    > ### Title: Coerce to Superset from Character
    > ### Aliases: superset.character
    > 
    > ### ** Examples
    > 
    > library(magrittr)
    ...
     14.                 ├─base::withRestarts(...)
     15.                 │ └─base (local) withOneRestart(expr, restarts[[1L]])
     16.                 │   └─base (local) doWithOneRestart(return(expr), restart)
     17.                 └─vctrs:::stop_lossy_cast(...)
     18.                   └─vctrs::stop_incompatible_cast(...)
     19.                     └─vctrs::stop_incompatible_type(...)
     20.                       └─vctrs:::stop_incompatible(...)
     21.                         └─vctrs:::stop_vctrs(...)
     22.                           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
    Execution halted
    ```

# overviewR

<details>

* Version: 0.0.10
* GitHub: https://github.com/cosimameyer/overviewR
* Source code: https://github.com/cran/overviewR
* Date/Publication: 2022-04-15 15:30:02 UTC
* Number of recursive dependencies: 123

Run `cloud_details(, "overviewR")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      • by.x = !!dat1_id
      • by.y = !!dat2_id
      Backtrace:
          ▆
       1. └─overviewR::overview_overlap(...) at test-check_output.R:307:2
       2.   └─dplyr::full_join(...)
       3.     └─rlang (local) `<fn>`()
       4.       └─rlang:::check_dots(env, error, action, call)
       5.         └─rlang:::action_dots(...)
       6.           ├─base (local) try_dots(...)
       7.           └─rlang (local) action(...)
      
      [ FAIL 1 | WARN 0 | SKIP 4 | PASS 52 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘getting-started.Rmd’ using rmarkdown
    --- finished re-building ‘getting-started.Rmd’
    
    --- re-building ‘overviewR_vignette.Rmd’ using rmarkdown
    Loading required package: knitr
    Loading required package: devtools
    Loading required package: usethis
    Loading required package: dplyr
    
    ...
    ✖ Problematic arguments:
    • by.x = !!dat1_id
    • by.y = !!dat2_id
    --- failed re-building ‘overviewR_vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘overviewR_vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# panelr

<details>

* Version: 0.7.6
* GitHub: https://github.com/jacob-long/panelr
* Source code: https://github.com/cran/panelr
* Date/Publication: 2021-12-17 07:40:02 UTC
* Number of recursive dependencies: 172

Run `cloud_details(, "panelr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        2. │ └─testthat::quasi_label(enquo(object), arg = "object")
        3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
        4. ├─dplyr::distinct(w, lwage)
        5. ├─panelr:::distinct.panel_data(w, lwage)
        6. │ ├─panelr:::reconstruct(NextMethod(), .data)
        7. │ └─panelr:::reconstruct.panel_data(NextMethod(), .data)
        8. │   └─base::is.data.frame(new)
        9. ├─base::NextMethod()
       10. └─dplyr:::distinct.data.frame(w, lwage)
       11.   └─dplyr:::dplyr_col_select(out, prep$vars)
       12.     └─rlang::abort(bullets, call = error_call)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 276 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘AER’
    ```

# parsnip

<details>

* Version: 1.0.0
* GitHub: https://github.com/tidymodels/parsnip
* Source code: https://github.com/cran/parsnip
* Date/Publication: 2022-06-16 10:20:02 UTC
* Number of recursive dependencies: 134

Run `cloud_details(, "parsnip")` for more info

</details>

## Newly broken

*   checking Rd files ... WARNING
    ```
    prepare_Rd: Each row in `x` should match at most 1 row in `y`.
    ℹ Row 1 of `x` matches multiple rows.
    ℹ If multiple matches are expected, specify `multiple = "all"` in the join call
      to silence this warning.
    prepare_Rd: Each row in `x` should match at most 1 row in `y`.
    ℹ Row 1 of `x` matches multiple rows.
    ℹ If multiple matches are expected, specify `multiple = "all"` in the join call
      to silence this warning.
    prepare_Rd: Each row in `x` should match at most 1 row in `y`.
    ℹ Row 1 of `x` matches multiple rows.
    ...
    ℹ If multiple matches are expected, specify `multiple = "all"` in the join call
      to silence this warning.
    prepare_Rd: Each row in `x` should match at most 1 row in `y`.
    ℹ Row 1 of `x` matches multiple rows.
    ℹ If multiple matches are expected, specify `multiple = "all"` in the join call
      to silence this warning.
    prepare_Rd: Each row in `x` should match at most 1 row in `y`.
    ℹ Row 1 of `x` matches multiple rows.
    ℹ If multiple matches are expected, specify `multiple = "all"` in the join call
      to silence this warning.
    ```

*   checking for unstated dependencies in examples ... WARNING
    ```
    Warning: Each row in `x` should match at most 1 row in `y`.
    ℹ Row 1 of `x` matches multiple rows.
    ℹ If multiple matches are expected, specify `multiple = "all"` in the join call
      to silence this warning.
    Warning: Each row in `x` should match at most 1 row in `y`.
    ℹ Row 1 of `x` matches multiple rows.
    ℹ If multiple matches are expected, specify `multiple = "all"` in the join call
      to silence this warning.
    Warning: Each row in `x` should match at most 1 row in `y`.
    ℹ Row 1 of `x` matches multiple rows.
    ...
    ℹ If multiple matches are expected, specify `multiple = "all"` in the join call
      to silence this warning.
    Warning: Each row in `x` should match at most 1 row in `y`.
    ℹ Row 1 of `x` matches multiple rows.
    ℹ If multiple matches are expected, specify `multiple = "all"` in the join call
      to silence this warning.
    Warning: Each row in `x` should match at most 1 row in `y`.
    ℹ Row 1 of `x` matches multiple rows.
    ℹ If multiple matches are expected, specify `multiple = "all"` in the join call
      to silence this warning.
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘rules’, ‘baguette’, ‘ipred’, ‘dbarts’, ‘h2o’, ‘agua’, ‘lightgbm’, ‘bonsai’, ‘mboost’, ‘mda’, ‘sda’, ‘sparsediscrim’, ‘klaR’, ‘brulee’, ‘glmnet’, ‘rstan’, ‘rstanarm’, ‘naivebayes’, ‘plsmod’, ‘mixOmics’, ‘pscl’, ‘workflows’, ‘randomForest’, ‘xrf’, ‘flexsurv’, ‘broom’
    ```

# phenofit

<details>

* Version: 0.3.2
* GitHub: https://github.com/eco-hydro/phenofit
* Source code: https://github.com/cran/phenofit
* Date/Publication: 2021-10-15 10:50:02 UTC
* Number of recursive dependencies: 83

Run `cloud_details(, "phenofit")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘phenofit-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: curvefits
    > ### Title: Fine Curve fitting
    > ### Aliases: curvefits
    > 
    > ### ** Examples
    > 
    > data("CA_NS6")
    ...
    +         rFUN = smooth_wWHIT, wFUN = wFUN,
    +         r_min = 0.05, ypeak_min = 0.05,
    +         lambda = 10,
    +         verbose = FALSE
    +     ))
      [season_mov] running 1 ... 
    Error in FUN(X[[i]], ...) : 
      only defined on a data frame with all numeric-alike variables
    Calls: season_mov ... get_A -> %>% -> diff -> Summary.data.frame -> lapply -> FUN
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        9. ├─base::do.call(season_mov, param)
       10. ├─phenofit (local) `<fn>`(INPUT = `<named list>`, options = `<named list>`, rFUN = `<fn>`)
       11. │ ├─base::do.call(opt_season, params_i)
       12. │ └─phenofit (local) `<fn>`(INPUT = `<named list>`, lambda = 14.1253754462275, rFUN = `<fn>`)
       13. │   └─phenofit::find_season.peaks(d_fit, info_peak)
       14. │     └─phenofit:::get_A(ypred, na.rm = FALSE)
       15. │       ├─range(x, na.rm = na.rm) %>% diff()
       16. │       └─base::Summary.data.frame(`<dt[,8]>`, na.rm = FALSE)
       17. │         └─base::lapply(...)
       18. │           └─base (local) FUN(X[[i]], ...)
       19. └─base::diff(.)
      
      [ FAIL 7 | WARN 0 | SKIP 0 | PASS 34 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘phenofit-procedures.Rmd’ using rmarkdown
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:data.table':
    
        between, first, last
    
    The following objects are masked from 'package:stats':
    ...
    Quitting from lines 126-140 (phenofit_CA-NS6.Rmd) 
    Error: processing vignette 'phenofit_CA-NS6.Rmd' failed with diagnostics:
    only defined on a data frame with all numeric-alike variables
    --- failed re-building ‘phenofit_CA-NS6.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘phenofit-procedures.Rmd’ ‘phenofit_CA-NS6.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.4Mb
      sub-directories of 1Mb or more:
        libs   6.3Mb
    ```

# presenter

<details>

* Version: 0.1.1
* GitHub: https://github.com/Harrison4192/presenter
* Source code: https://github.com/cran/presenter
* Date/Publication: 2021-11-18 06:20:05 UTC
* Number of recursive dependencies: 120

Run `cloud_details(, "presenter")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘exportToExcel.Rmd’ using rmarkdown
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    ...
    --- finished re-building ‘formattedFlextable.Rmd’
    
    --- re-building ‘pivotSummary.Rmd’ using rmarkdown
    --- finished re-building ‘pivotSummary.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘flextableAndPowerpoint.Rmd’ ‘flextableThemes.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘badger’
      All declared Imports should be used.
    ```

# PVplr

<details>

* Version: 0.1.1
* GitHub: NA
* Source code: https://github.com/cran/PVplr
* Date/Publication: 2022-05-13 21:10:02 UTC
* Number of recursive dependencies: 91

Run `cloud_details(, "PVplr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘PVplr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plr_variable_check
    > ### Title: Define Standard Variable Names
    > ### Aliases: plr_variable_check
    > 
    > ### ** Examples
    > 
    > var_list <- plr_variable_check(test_df)
    ...
    Backtrace:
        ▆
     1. ├─PVplr::plr_variable_check(test_df)
     2. │ └─dplyr::if_else("wspa" %in% names, "wspa", NULL)
     3. │   └─dplyr:::vec_case_when(...)
     4. │     └─vctrs::list_check_all_vectors(values, arg = values_arg, call = call)
     5. └─vctrs:::stop_scalar_type(`<fn>`(NULL), "false", `<env>`)
     6.   └─vctrs:::stop_vctrs(...)
     7.     └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
    Execution halted
    ```

# questionr

<details>

* Version: 0.7.7
* GitHub: https://github.com/juba/questionr
* Source code: https://github.com/cran/questionr
* Date/Publication: 2022-01-31 16:30:08 UTC
* Number of recursive dependencies: 134

Run `cloud_details(, "questionr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Backtrace:
          ▆
       1. ├─... %>% gather(Var2, Freq, -1) at test_tables.R:107:2
       2. ├─tidyr::gather(., Var2, Freq, -1)
       3. ├─base::as.data.frame(.)
       4. ├─questionr::cprop(.)
       5. └─questionr:::cprop.tabyl(.)
       6.   └─janitor::adorn_percentages(tab, "col")
       7.     └─base::Summary.data.frame(`<tabyl[,4]>`, na.rm = FALSE)
       8.       └─base::lapply(...)
       9.         └─base (local) FUN(X[[i]], ...)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 65 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 8504 marked UTF-8 strings
    ```

# rabhit

<details>

* Version: 0.2.1
* GitHub: NA
* Source code: https://github.com/cran/rabhit
* Date/Publication: 2022-02-16 14:10:02 UTC
* Number of recursive dependencies: 133

Run `cloud_details(, "rabhit")` for more info

</details>

## Newly broken

*   checking whether package ‘rabhit’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/rabhit/new/rabhit.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘rabhit’ ...
** package ‘rabhit’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘data_frame_’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘rabhit’
* removing ‘/tmp/workdir/rabhit/new/rabhit.Rcheck/rabhit’


```
### CRAN

```
* installing *source* package ‘rabhit’ ...
** package ‘rabhit’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (rabhit)


```
# respR

<details>

* Version: 2.0.2
* GitHub: https://github.com/januarharianto/respr
* Source code: https://github.com/cran/respR
* Date/Publication: 2022-03-23 15:50:02 UTC
* Number of recursive dependencies: 124

Run `cloud_details(, "respR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘respR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: calc_rate.ft
    > ### Title: Calculate rate of change in oxygen from flowthrough respirometry
    > ###   data
    > ### Aliases: calc_rate.ft
    > 
    > ### ** Examples
    > 
    ...
     15.                           ├─base::withRestarts(...)
     16.                           │ └─base (local) withOneRestart(expr, restarts[[1L]])
     17.                           │   └─base (local) doWithOneRestart(return(expr), restart)
     18.                           └─vctrs:::stop_lossy_cast(...)
     19.                             └─vctrs::stop_incompatible_cast(...)
     20.                               └─vctrs::stop_incompatible_type(...)
     21.                                 └─vctrs:::stop_incompatible(...)
     22.                                   └─vctrs:::stop_vctrs(...)
     23.                                     └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       12.                 └─vctrs:::vec_cast.integer.double(...)
       13.                   └─vctrs::maybe_lossy_cast(...)
       14.                     ├─base::withRestarts(...)
       15.                     │ └─base (local) withOneRestart(expr, restarts[[1L]])
       16.                     │   └─base (local) doWithOneRestart(return(expr), restart)
       17.                     └─vctrs:::stop_lossy_cast(...)
       18.                       └─vctrs::stop_incompatible_cast(...)
       19.                         └─vctrs::stop_incompatible_type(...)
       20.                           └─vctrs:::stop_incompatible(...)
       21.                             └─vctrs:::stop_vctrs(...)
       22.                               └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
      
      [ FAIL 10 | WARN 0 | SKIP 5 | PASS 4508 ]
      Error: Test failures
      Execution halted
    ```

# RNeXML

<details>

* Version: 2.4.7
* GitHub: https://github.com/ropensci/RNeXML
* Source code: https://github.com/cran/RNeXML
* Date/Publication: 2022-05-13 09:00:10 UTC
* Number of recursive dependencies: 135

Run `cloud_details(, "RNeXML")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘test-all.R’
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      Loading required package: ape
      Done simulation(s).
      [ FAIL 1 | WARN 2 | SKIP 42 | PASS 302 ]
      
      ══ Skipped tests ═══════════════════════════════════════════════════════════════
      • On CRAN (42)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test_meta_extract.R:204:3): ID assignments are correct and complete when meta are nested ──
      sort(meta.cont[, "Meta"]) not equal to sort(unique(meta.nested[, "meta"])).
      names for target but not for current
      
      [ FAIL 1 | WARN 2 | SKIP 42 | PASS 302 ]
      Error: Test failures
      Execution halted
    ```

# romic

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/romic
* Date/Publication: 2021-07-20 09:00:02 UTC
* Number of recursive dependencies: 111

Run `cloud_details(, "romic")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      ══ Skipped tests ═══════════════════════════════════════════════════════════════
      • empty test (1)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test-conversion.R:22:3): triple <-> tidy ───────────────────────────
      `triple` not equal to `brauer_2008_triple`.
      Component "features": Names: 3 string mismatches
      Component "features": Component 2: 500 string mismatches
      Component "features": Component 3: 476 string mismatches
      Component "features": Component 4: 500 string mismatches
      
      [ FAIL 1 | WARN 4 | SKIP 1 | PASS 15 ]
      Error: Test failures
      Execution halted
    ```

# RPresto

<details>

* Version: 1.3.7
* GitHub: https://github.com/prestodb/RPresto
* Source code: https://github.com/cran/RPresto
* Date/Publication: 2021-09-04 12:40:02 UTC
* Number of recursive dependencies: 47

Run `cloud_details(, "RPresto")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        6. │ │     └─rlang::eval_bare(x, .env)
        7. │ ├─dplyr::sql_subquery(con, sql)
        8. │ └─dbplyr:::sql_subquery.DBIConnection(con, sql)
        9. │   ├─dbplyr::sql_query_wrap(con, from = from, name = name, ..., lvl = lvl)
       10. │   └─dbplyr:::sql_query_wrap.DBIConnection(...)
       11. │     └─dbplyr::is.ident(from)
       12. ├─dplyr::sql_subquery(...)
       13. └─dbplyr:::sql_subquery.DBIConnection(...)
       14.   ├─dbplyr::sql_query_wrap(con, from = from, name = name, ..., lvl = lvl)
       15.   └─dbplyr:::sql_query_wrap.DBIConnection(...)
       16.     └─dbplyr::is.ident(from)
      
      [ FAIL 2 | WARN 0 | SKIP 45 | PASS 228 ]
      Error: Test failures
      Execution halted
    ```

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in documentation object 'src_presto.Rd':
      ‘[dplyr]{sql}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

# rsample

<details>

* Version: 1.0.0
* GitHub: https://github.com/tidymodels/rsample
* Source code: https://github.com/cran/rsample
* Date/Publication: 2022-06-24 18:10:02 UTC
* Number of recursive dependencies: 108

Run `cloud_details(, "rsample")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘rsample-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: tidy.rsplit
    > ### Title: Tidy Resampling Object
    > ### Aliases: tidy.rsplit tidy.rset tidy.vfold_cv tidy.nested_cv
    > 
    > ### ** Examples
    > 
    > library(ggplot2)
    ...
        ▆
     1. ├─generics::tidy(mc_cv(mtcars, times = 5))
     2. ├─rsample:::tidy.rset(mc_cv(mtcars, times = 5))
     3. │ └─dplyr::if_else(is.null(dots$unique_ind), TRUE, dots$unique_ind)
     4. │   └─dplyr:::vec_case_when(...)
     5. │     └─vctrs::list_check_all_vectors(values, arg = values_arg, call = call)
     6. └─vctrs:::stop_scalar_type(`<fn>`(NULL), "false", `<env>`)
     7.   └─vctrs:::stop_vctrs(...)
     8.     └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Working_with_rsets.Rmd’ using rmarkdown
    Quitting from lines 209-211 (Working_with_rsets.Rmd) 
    Error: processing vignette 'Working_with_rsets.Rmd' failed with diagnostics:
    `false` must be a vector, not NULL.
    --- failed re-building ‘Working_with_rsets.Rmd’
    
    --- re-building ‘rsample.Rmd’ using rmarkdown
    --- finished re-building ‘rsample.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Working_with_rsets.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# seecolor

<details>

* Version: 0.1.0
* GitHub: https://github.com/lovestat/seecolor
* Source code: https://github.com/cran/seecolor
* Date/Publication: 2020-12-07 17:40:03 UTC
* Number of recursive dependencies: 65

Run `cloud_details(, "seecolor")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘seecolor-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: print_color
    > ### Title: Print the colors
    > ### Aliases: print_color
    > 
    > ### ** Examples
    > 
    > 
    ...
      7. │     ├─crayon::make_style(contrast_color(bg.color))
      8. │     └─seecolor:::contrast_color(bg.color)
      9. │       ├─... %>% ...
     10. │       └─dplyr::if_else(grDevices::col2rgb(x) < 128, 255, 0)
     11. │         └─vctrs::vec_assert(x = condition, ptype = logical(), arg = "condition")
     12. │           └─rlang::abort(...)
     13. ├─purrr::set_names(., c("red", "green", "blue", "maxColorValue"))
     14. ├─base::append(., 255)
     15. └─base::as.list(.)
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Intro-to-seecolor-package.Rmd’ using rmarkdown
    Quitting from lines 49-60 (Intro-to-seecolor-package.Rmd) 
    Error: processing vignette 'Intro-to-seecolor-package.Rmd' failed with diagnostics:
    `condition` must be a vector with type <logical>.
    Instead, it has type <logical[,1]>.
    --- failed re-building ‘Intro-to-seecolor-package.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Intro-to-seecolor-package.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘fansi’
      All declared Imports should be used.
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# sejmRP

<details>

* Version: 1.3.4
* GitHub: https://github.com/mi2-warsaw/sejmRP
* Source code: https://github.com/cran/sejmRP
* Date/Publication: 2017-03-28 17:29:47 UTC
* Number of recursive dependencies: 121

Run `cloud_details(, "sejmRP")` for more info

</details>

## Newly broken

*   checking whether package ‘sejmRP’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/sejmRP/new/sejmRP.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘cluster’ ‘factoextra’ ‘tidyr’
      All declared Imports should be used.
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

## Installation

### Devel

```
* installing *source* package ‘sejmRP’ ...
** package ‘sejmRP’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error: object ‘sql’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘sejmRP’
* removing ‘/tmp/workdir/sejmRP/new/sejmRP.Rcheck/sejmRP’


```
### CRAN

```
* installing *source* package ‘sejmRP’ ...
** package ‘sejmRP’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (sejmRP)


```
# sergeant

<details>

* Version: 0.9.1
* GitHub: NA
* Source code: https://github.com/cran/sergeant
* Date/Publication: 2021-11-29 18:40:02 UTC
* Number of recursive dependencies: 82

Run `cloud_details(, "sergeant")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... WARNING
    ```
    Missing or unexported object: ‘dplyr::sql’
    ```

# shinyNotes

<details>

* Version: 0.0.1
* GitHub: https://github.com/danielkovtun/shinyNotes
* Source code: https://github.com/cran/shinyNotes
* Date/Publication: 2020-02-05 16:30:05 UTC
* Number of recursive dependencies: 72

Run `cloud_details(, "shinyNotes")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘shinyNotes-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: db.write_table
    > ### Title: Write data frames to remote database tables with additional
    > ###   validation
    > ### Aliases: db.write_table
    > 
    > ### ** Examples
    > 
    > connection <- connect_sqlite(auto_disconnect = FALSE)
    > 
    > db.write_table(con = connection, table = 'iris', data = iris)
    Error: 'sql' is not an exported object from 'namespace:dplyr'
    Execution halted
    ```

*   checking dependencies in R code ... WARNING
    ```
    Missing or unexported object: ‘dplyr::sql’
    ```

# skater

<details>

* Version: 0.1.1
* GitHub: https://github.com/signaturescience/skater
* Source code: https://github.com/cran/skater
* Date/Publication: 2022-02-01 16:00:02 UTC
* Number of recursive dependencies: 94

Run `cloud_details(, "skater")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘skater-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: fam2ped
    > ### Title: Fam to pedigree
    > ### Aliases: fam2ped
    > 
    > ### ** Examples
    > 
    > famfile <- system.file("extdata", "3gens.fam", package="skater", mustWork=TRUE)
    ...
     11.   └─dplyr (local) fn(col, ...)
     12.     └─vctrs::vec_cast(x = y, to = x, x_arg = "y", to_arg = "x")
     13.       └─vctrs (local) `<fn>`()
     14.         └─vctrs::vec_default_cast(...)
     15.           └─vctrs::stop_incompatible_cast(...)
     16.             └─vctrs::stop_incompatible_type(...)
     17.               └─vctrs:::stop_incompatible(...)
     18.                 └─vctrs:::stop_vctrs(...)
     19.                   └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       17. │             └─vctrs:::stop_incompatible(...)
       18. │               └─vctrs:::stop_vctrs(...)
       19. │                 └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
       20. │                   └─rlang:::signal_abort(cnd, .file)
       21. │                     └─base::signalCondition(cnd)
       22. ├─dplyr (local) `<fn>`(`<vctrs___>`)
       23. │ └─rlang::abort(bullets, call = call(setup$across_if_fn), parent = cnd)
       24. │   └─rlang:::signal_abort(cnd, .file)
       25. │     └─base::signalCondition(cnd)
       26. └─dplyr (local) `<fn>`(`<rlng_rrr>`)
       27.   └─rlang::abort(...)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 10 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘basic_usage.Rmd’ using rmarkdown
    Quitting from lines 48-50 (basic_usage.Rmd) 
    Error: processing vignette 'basic_usage.Rmd' failed with diagnostics:
    Problem while computing `..1 = dplyr::across(c(dadid, momid),
    dplyr::na_if, 0)`.
    Caused by error in `across()`:
    ! Problem while computing column `dadid`.
    Caused by error in `fn()`:
    ! Can't convert `y` <double> to match type of `x` <character>.
    --- failed re-building ‘basic_usage.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘basic_usage.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# sparklyr

<details>

* Version: 1.7.7
* GitHub: https://github.com/sparklyr/sparklyr
* Source code: https://github.com/cran/sparklyr
* Date/Publication: 2022-06-07 20:50:02 UTC
* Number of recursive dependencies: 111

Run `cloud_details(, "sparklyr")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... WARNING
    ```
    Missing or unexported object: ‘dplyr::sql’
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.3Mb
      sub-directories of 1Mb or more:
        R      1.6Mb
        java   3.4Mb
    ```

# sparklyr.flint

<details>

* Version: 0.2.2
* GitHub: https://github.com/r-spark/sparklyr.flint
* Source code: https://github.com/cran/sparklyr.flint
* Date/Publication: 2022-01-11 08:50:13 UTC
* Number of recursive dependencies: 62

Run `cloud_details(, "sparklyr.flint")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... WARNING
    ```
    Missing or unexported object: ‘dplyr::sql’
    ```

# stars

<details>

* Version: 0.5-6
* GitHub: https://github.com/r-spatial/stars
* Source code: https://github.com/cran/stars
* Date/Publication: 2022-07-21 12:10:02 UTC
* Number of recursive dependencies: 149

Run `cloud_details(, "stars")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘aggregate.R’
      Comparing ‘aggregate.Rout’ to ‘aggregate.Rout.save’ ...4c4
    < Linking to GEOS 3.8.0, GDAL 3.0.4, PROJ 6.3.1; sf_use_s2() is TRUE
    ---
    > Linking to GEOS 3.10.2, GDAL 3.4.3, PROJ 8.2.0; sf_use_s2() is TRUE
      Running ‘align.R’
      Comparing ‘align.Rout’ to ‘align.Rout.save’ ... OK
      Running ‘area.R’
      Comparing ‘area.Rout’ to ‘area.Rout.save’ ...52,64d51
    < /PRODUCT/longitude, 
    ...
      warn\[1\] does not match "Non-canonical axis order found, attempting to correct.".
      Actual value: "Each row in `x` should match at most 1 row in `y`\.\\nℹ Row 2 of `x` matches multiple rows\.\\nℹ If multiple matches are expected, specify `multiple = "all"` in the join call\\n  to silence this warning\."
      Backtrace:
          ▆
       1. └─testthat::expect_match(warn[1], "Non-canonical axis order found, attempting to correct.") at test_ncdf.R:155:2
       2.   └─testthat:::expect_match_(...)
      
      [ FAIL 1 | WARN 102 | SKIP 0 | PASS 161 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.5Mb
      sub-directories of 1Mb or more:
        doc   1.7Mb
        nc    1.7Mb
    ```

# suddengains

<details>

* Version: 0.4.4
* GitHub: https://github.com/milanwiedemann/suddengains
* Source code: https://github.com/cran/suddengains
* Date/Publication: 2020-05-22 22:40:03 UTC
* Number of recursive dependencies: 91

Run `cloud_details(, "suddengains")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘suddengains-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: create_byperson
    > ### Title: Create a data set with one gain per person
    > ### Aliases: create_byperson
    > 
    > ### ** Examples
    > 
    > # Create byperson data set, selecting the largest gain in case of muliple gains
    ...
    +                                 "bdi_s7", "bdi_s8", "bdi_s9",
    +                                 "bdi_s10", "bdi_s11", "bdi_s12"),
    +                 sg_measure_name = "bdi",
    +                 multiple_sg_select = "largest")
    First, second, and third sudden gains criteria were applied.
    The critical value for the third criterion was adjusted for missingness.
    Error in `$<-.data.frame`(`*tmp*`, "call_text", value = c("suddengains::create_byperson(...)",  : 
      replacement has 22 rows, data has 18
    Calls: create_byperson ... trace_format -> trace_as_tree -> $<- -> $<-.data.frame
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘shinygains.Rmd’ using rmarkdown
    --- finished re-building ‘shinygains.Rmd’
    
    --- re-building ‘suddengains-tutorial.Rmd’ using rmarkdown
    Quitting from lines 402-412 (suddengains-tutorial.Rmd) 
    Error: processing vignette 'suddengains-tutorial.Rmd' failed with diagnostics:
    Problem while computing `sg_change_proportion =
    dplyr::na_if(sg_change_proportion, "-Inf")`.
    Caused by error in `dplyr::na_if()`:
    ! Can't convert `y` <character> to match type of `x` <double>.
    --- failed re-building ‘suddengains-tutorial.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘suddengains-tutorial.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# SwimmeR

<details>

* Version: 0.13.0
* GitHub: NA
* Source code: https://github.com/cran/SwimmeR
* Date/Publication: 2021-11-05 17:50:02 UTC
* Number of recursive dependencies: 74

Run `cloud_details(, "SwimmeR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘SwimmeR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: name_reorder
    > ### Title: Orders all names as "Firstname Lastname"
    > ### Aliases: name_reorder
    > 
    > ### ** Examples
    > 
    > name_reorder(
    ...
      7. └─dplyr::na_if(., "")
      8.   └─vctrs::vec_cast(x = y, to = x, x_arg = "y", to_arg = "x")
      9.     └─vctrs (local) `<fn>`()
     10.       └─vctrs::vec_default_cast(...)
     11.         └─vctrs::stop_incompatible_cast(...)
     12.           └─vctrs::stop_incompatible_type(...)
     13.             └─vctrs:::stop_incompatible(...)
     14.               └─vctrs:::stop_vctrs(...)
     15.                 └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       12. ├─dplyr::mutate(...)
       13. ├─dplyr::mutate(...)
       14. └─dplyr::na_if(., "")
       15.   └─vctrs::vec_cast(x = y, to = x, x_arg = "y", to_arg = "x")
       16.     └─vctrs (local) `<fn>`()
       17.       └─vctrs::vec_default_cast(...)
       18.         └─vctrs::stop_incompatible_cast(...)
       19.           └─vctrs::stop_incompatible_type(...)
       20.             └─vctrs:::stop_incompatible(...)
       21.               └─vctrs:::stop_vctrs(...)
       22.                 └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
      
      [ FAIL 16 | WARN 1 | SKIP 49 | PASS 18 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘SwimmeR.Rmd’ using rmarkdown
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    ...
    Quitting from lines 60-66 (SwimmeR.Rmd) 
    Error: processing vignette 'SwimmeR.Rmd' failed with diagnostics:
    Can't convert `y` <character> to match type of `x` <data.frame>.
    --- failed re-building ‘SwimmeR.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘SwimmeR.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# table.express

<details>

* Version: 0.4.0
* GitHub: https://github.com/asardaes/table.express
* Source code: https://github.com/cran/table.express
* Date/Publication: 2022-04-02 19:40:02 UTC
* Number of recursive dependencies: 55

Run `cloud_details(, "table.express")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        5. ├─dplyr::mutate(., num_purchases = length(unique(payment_id)))
        6. ├─dplyr::group_by(., name)
        7. ├─table.express::chain(.)
        8. ├─dplyr::mutate(., purchase_time = NULL)
        9. ├─table.express::chain(.)
       10. └─dplyr::left_join(...)
       11.   └─rlang (local) `<fn>`()
       12.     └─rlang:::check_dots(env, error, action, call)
       13.       └─rlang:::action_dots(...)
       14.         ├─base (local) try_dots(...)
       15.         └─rlang (local) action(...)
      
      [ FAIL 10 | WARN 0 | SKIP 1 | PASS 586 ]
      Error: Test failures
      Execution halted
    ```

# taxizedb

<details>

* Version: 0.3.0
* GitHub: https://github.com/ropensci/taxizedb
* Source code: https://github.com/cran/taxizedb
* Date/Publication: 2021-01-15 06:00:06 UTC
* Number of recursive dependencies: 144

Run `cloud_details(, "taxizedb")` for more info

</details>

## Newly broken

*   checking whether package ‘taxizedb’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/taxizedb/new/taxizedb.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

## Installation

### Devel

```
* installing *source* package ‘taxizedb’ ...
** package ‘taxizedb’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘sql’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘taxizedb’
* removing ‘/tmp/workdir/taxizedb/new/taxizedb.Rcheck/taxizedb’


```
### CRAN

```
* installing *source* package ‘taxizedb’ ...
** package ‘taxizedb’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (taxizedb)


```
# tidygraph

<details>

* Version: 1.2.1
* GitHub: https://github.com/thomasp85/tidygraph
* Source code: https://github.com/cran/tidygraph
* Date/Publication: 2022-04-05 16:00:02 UTC
* Number of recursive dependencies: 75

Run `cloud_details(, "tidygraph")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Attributes: < Lengths: 2, 0 >
      Attributes: < names for target but not for current >
      Attributes: < current is not list-like >
      ── Failure (test-group.R:77:3): grouping with fixed number of groups ───────────
      get_number_of_groups(gr, group_walktrap(n_groups = 7)) not equal to 7.
      Modes: list, numeric
      names for target but not for current
      Attributes: < Modes: list, NULL >
      Attributes: < Lengths: 2, 0 >
      Attributes: < names for target but not for current >
      Attributes: < current is not list-like >
      
      [ FAIL 4 | WARN 23 | SKIP 0 | PASS 276 ]
      Error: Test failures
      Execution halted
    ```

# tidyquery

<details>

* Version: 0.2.3
* GitHub: https://github.com/ianmcook/tidyquery
* Source code: https://github.com/cran/tidyquery
* Date/Publication: 2021-12-02 20:10:02 UTC
* Number of recursive dependencies: 65

Run `cloud_details(, "tidyquery")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
        4. ├─tidyquery::query("SELECT name, list_price FROM games g LEFT ANTI JOIN inventory i ON g.name = i.game")
        5. │ └─tidyquery:::query_(data, sql, TRUE)
        6. │   └─tidyquery:::join(tree)
        7. │     └─out$data %>% ...
        8. └─dplyr (local) join_function(...)
        9.   └─rlang (local) `<fn>`()
       10.     └─rlang:::check_dots(env, error, action, call)
       11.       └─rlang:::action_dots(...)
       12.         ├─base (local) try_dots(...)
       13.         └─rlang (local) action(...)
      
      [ FAIL 2 | WARN 9 | SKIP 2 | PASS 217 ]
      Error: Test failures
      Execution halted
    ```

# tidysmd

<details>

* Version: 0.1.0
* GitHub: https://github.com/malcolmbarrett/tidysmd
* Source code: https://github.com/cran/tidysmd
* Date/Publication: 2021-10-25 07:00:02 UTC
* Number of recursive dependencies: 59

Run `cloud_details(, "tidysmd")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘ellipsis’
      All declared Imports should be used.
    ```

# tidytree

<details>

* Version: 0.3.9
* GitHub: https://github.com/YuLab-SMU/tidytree
* Source code: https://github.com/cran/tidytree
* Date/Publication: 2022-03-04 09:10:02 UTC
* Number of recursive dependencies: 61

Run `cloud_details(, "tidytree")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
           ▆
        1. ├─tree %<>% left_join(dat, by = "node") at test-dplyr-methods.R:125:4
        2. ├─dplyr::left_join(., dat, by = "node")
        3. ├─tidytree:::left_join.treedata(., dat, by = "node")
        4. │ └─dat %>% ...
        5. └─dplyr::left_join(...)
        6.   └─rlang (local) `<fn>`()
        7.     └─rlang:::check_dots(env, error, action, call)
        8.       └─rlang:::action_dots(...)
        9.         ├─base (local) try_dots(...)
       10.         └─rlang (local) action(...)
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 62 ]
      Error: Test failures
      Execution halted
    ```

# tidywikidatar

<details>

* Version: 0.5.3
* GitHub: NA
* Source code: https://github.com/cran/tidywikidatar
* Date/Publication: 2022-06-07 08:10:08 UTC
* Number of recursive dependencies: 135

Run `cloud_details(, "tidywikidatar")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘tidywikidatar-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: tw_filter_people
    > ### Title: Filter search result and keep only people
    > ### Aliases: tw_filter_people
    > 
    > ### ** Examples
    > 
    > tw_search("Ruth Benedict")
    Error in `$<-.data.frame`(`*tmp*`, "call_text", value = c("tidywikidatar::tw_search(\"Ruth Benedict\")",  : 
      replacement has 21 rows, data has 20
    Calls: tw_search ... trace_format -> trace_as_tree -> $<- -> $<-.data.frame
    Execution halted
    ```

# Tplyr

<details>

* Version: 0.4.4
* GitHub: https://github.com/atorus-research/Tplyr
* Source code: https://github.com/cran/Tplyr
* Date/Publication: 2022-01-27 16:00:02 UTC
* Number of recursive dependencies: 117

Run `cloud_details(, "Tplyr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      i Use `spec()` to retrieve the full column specification for this data.
      i Specify the column types or set `show_col_types = FALSE` to quiet this message.
      [ FAIL 1 | WARN 0 | SKIP 52 | PASS 554 ]
      
      ══ Skipped tests ═══════════════════════════════════════════════════════════════
      • On CRAN (52)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test-count.R:197:3): Count layers are summarized without errors and warnings ──
      `build(t19)` produced warnings.
      
      [ FAIL 1 | WARN 0 | SKIP 52 | PASS 554 ]
      Error: Test failures
      Execution halted
    ```

# unpivotr

<details>

* Version: 0.6.2
* GitHub: https://github.com/nacnudus/unpivotr
* Source code: https://github.com/cran/unpivotr
* Date/Publication: 2021-08-22 04:10:02 UTC
* Number of recursive dependencies: 88

Run `cloud_details(, "unpivotr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘unpivotr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: behead
    > ### Title: Strip a level of headers from a pivot table
    > ### Aliases: behead behead_if
    > 
    > ### ** Examples
    > 
    > # A simple table with a row of headers
    ...
    4     1     2 chr       b        NA
    5     2     2 int       <NA>      3
    6     3     2 int       <NA>      4
    > 
    > # Strip the cells in row 1 (the original headers) and use them as data
    > behead(cells, "N", foo)
    Error in `$<-.data.frame`(`*tmp*`, "call_text", value = c("unpivotr::behead(cells, \"N\", foo)",  : 
      replacement has 13 rows, data has 10
    Calls: behead ... trace_format -> trace_as_tree -> $<- -> $<-.data.frame
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(unpivotr)
      > 
      > test_check("unpivotr")
      [ FAIL 13 | WARN 0 | SKIP 0 | PASS 153 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      Error in `$<-.data.frame`(`*tmp*`, "call_text", value = c("... %>% dplyr::select(-row, -col, -data_type, -chr)",  : 
        replacement has 18 rows, data has 15
      Calls: test_check ... trace_format -> trace_as_tree -> $<- -> $<-.data.frame
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘html.Rmd’ using rmarkdown
    --- finished re-building ‘html.Rmd’
    
    --- re-building ‘introduction.Rmd’ using rmarkdown
    Quitting from lines 147-172 (introduction.Rmd) 
    Error: processing vignette 'introduction.Rmd' failed with diagnostics:
    `rlang::expr(.data$row == min(.data$row))` must be a vector, not a call.
    --- failed re-building ‘introduction.Rmd’
    ...
    Quitting from lines 103-108 (small-multiples.Rmd) 
    Error: processing vignette 'small-multiples.Rmd' failed with diagnostics:
    `rlang::expr(.data$row == min(.data$row))` must be a vector, not a call.
    --- failed re-building ‘small-multiples.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘introduction.Rmd’ ‘small-multiples.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# vctrs

<details>

* Version: 0.4.1
* GitHub: https://github.com/r-lib/vctrs
* Source code: https://github.com/cran/vctrs
* Date/Publication: 2022-04-13 10:30:02 UTC
* Number of recursive dependencies: 71

Run `cloud_details(, "vctrs")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        5. │   └─testthat:::expect_condition_matching(...)
        6. │     └─testthat:::quasi_capture(...)
        7. │       ├─testthat (local) .capture(...)
        8. │       │ └─base::withCallingHandlers(...)
        9. │       └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
       10. ├─vctrs:::vec_ptype_common_df_fallback(...)
       11. │ └─vctrs:::vec_ptype_common_params(..., .ptype = .ptype, .df_fallback = DF_FALLBACK_warn) at tests/testthat/helper-vctrs.R:10:2
       12. │   └─vctrs:::vec_ptype_common_opts(...)
       13. └─base::loadNamespace(x)
       14.   ├─base::namespaceImport(...)
       15.   └─base::loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]])
      
      [ FAIL 12 | WARN 0 | SKIP 196 | PASS 4767 ]
      Error: Test failures
      Execution halted
    ```

# vegdata

<details>

* Version: 0.9.11.3
* GitHub: NA
* Source code: https://github.com/cran/vegdata
* Date/Publication: 2022-06-17 23:50:02 UTC
* Number of recursive dependencies: 67

Run `cloud_details(, "vegdata")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... WARNING
    ```
    Missing or unexported object: ‘dplyr::sql’
    ```

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘vegdata.Rnw’ using knitr
    Error: processing vignette 'vegdata.Rnw' failed with diagnostics:
    Running 'texi2dvi' on 'vegdata.tex' failed.
    LaTeX errors:
    ! Package babel Error: Unknown option 'english'. Either you misspelled it
    (babel)                or the language definition file english.ldf was not found.
    
    See the babel package documentation for explanation.
    Type  H <return>  for immediate help.
    ...
    l.76 \usetikzlibrary
                        {shapes}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    --- failed re-building ‘vegdata.Rnw’
    
    SUMMARY: processing the following file failed:
      ‘vegdata.Rnw’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# wordbankr

<details>

* Version: 0.3.1
* GitHub: https://github.com/langcog/wordbankr
* Source code: https://github.com/cran/wordbankr
* Date/Publication: 2020-11-13 23:10:02 UTC
* Number of recursive dependencies: 68

Run `cloud_details(, "wordbankr")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... WARNING
    ```
    Namespace in Imports field not imported from: ‘dbplyr’
      All declared Imports should be used.
    Missing or unexported object: ‘dplyr::sql’
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dbplyr’
      All declared Imports should be used.
    ```

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘wordbankr.Rmd’ using rmarkdown
    Quitting from lines 31-33 (wordbankr.Rmd) 
    Error: processing vignette 'wordbankr.Rmd' failed with diagnostics:
    Failed to connect to database: Error: Can't connect to MySQL server on 'server.wordbank.stanford.edu:3306' (110)
    
    --- failed re-building ‘wordbankr.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘wordbankr.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# wrangle

<details>

* Version: 0.5.7
* GitHub: NA
* Source code: https://github.com/cran/wrangle
* Date/Publication: 2022-01-03 18:20:02 UTC
* Number of recursive dependencies: 19

Run `cloud_details(, "wrangle")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘wrangle-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: status
    > ### Title: Report status.
    > ### Aliases: status
    > 
    > ### ** Examples
    > 
    > library(dplyr)
    ...
     13.                 ├─base::withRestarts(...)
     14.                 │ └─base (local) withOneRestart(expr, restarts[[1L]])
     15.                 │   └─base (local) doWithOneRestart(return(expr), restart)
     16.                 └─vctrs:::stop_lossy_cast(...)
     17.                   └─vctrs::stop_incompatible_cast(...)
     18.                     └─vctrs::stop_incompatible_type(...)
     19.                       └─vctrs:::stop_incompatible(...)
     20.                         └─vctrs:::stop_vctrs(...)
     21.                           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
    Execution halted
    ```

