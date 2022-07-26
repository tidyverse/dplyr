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

# d3r

<details>

* Version: 1.0.0
* GitHub: https://github.com/timelyportfolio/d3r
* Source code: https://github.com/cran/d3r
* Date/Publication: 2021-08-15 18:00:06 UTC
* Number of recursive dependencies: 64

Run `cloud_details(, "d3r")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      • On CRAN (3)
      • V8 cannot be loaded (1)
      • github cannot be loaded (1)
      • igraph cannot be loaded (1)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test_deps.R:39:3): d3_dep-* src href is a valid url ────────────────
      is_valid_url(file.path(jetpack_offline$src$href, jetpack$script)) is not TRUE
      
      `actual`:   FALSE
      `expected`: TRUE 
      
      [ FAIL 1 | WARN 0 | SKIP 6 | PASS 18 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      'igraph', 'partykit', 'treemap', 'V8'
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
      
      [ FAIL 2 | WARN 27 | SKIP 187 | PASS 1318 ]
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
    /tmp/RtmpVRGbiX/file3b09ef81b4e.sqlite3
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

