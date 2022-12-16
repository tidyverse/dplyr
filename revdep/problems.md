# admiral

<details>

* Version: 0.9.0
* GitHub: https://github.com/pharmaverse/admiral
* Source code: https://github.com/cran/admiral
* Date/Publication: 2022-12-06 08:22:31 UTC
* Number of recursive dependencies: 126

Run `cloud_details(, "admiral")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      A summary is given below.
      
      There are columns in BASE and COMPARE with differing attributes !!
      All rows are shown in table below
      
        =====================================================
         VARIABLE  ATTR_NAME  VALUES.BASE     VALUES.COMP    
        -----------------------------------------------------
           ATMF      names       NULL      c("hour", "hour") 
        -----------------------------------------------------
      
      
      [ FAIL 11 | WARN 15 | SKIP 0 | PASS 557 ]
      Error: Test failures
      Execution halted
    ```

# arrow

<details>

* Version: 10.0.1
* GitHub: https://github.com/apache/arrow
* Source code: https://github.com/cran/arrow
* Date/Publication: 2022-12-06 13:40:02 UTC
* Number of recursive dependencies: 74

Run `cloud_details(, "arrow")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       3. │   └─testthat:::quasi_capture(...)
       4. │     ├─testthat (local) .capture(...)
       5. │     │ └─base::withCallingHandlers(...)
       6. │     └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
       7. └─dplyr::slice_min(grouped, n = 5)
       8.   └─rlang::check_required(order_by)
       9.     └─rlang::abort(msg, call = call)
      ── Error ('test-dplyr-summarize.R:301'): Functions that take ... but we only accept a single arg ──
      Error in `summarize(., distinct = n_distinct())`: i In argument: `distinct = n_distinct()`.
      Caused by error in `n_distinct()`:
      ! `...` is absent, but must be supplied.
      
      [ FAIL 13 | WARN 14 | SKIP 71 | PASS 8541 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 122.8Mb
      sub-directories of 1Mb or more:
        R       4.2Mb
        libs  117.4Mb
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘readr’
    ```

# chronicler

<details>

* Version: 0.2.0
* GitHub: https://github.com/b-rodrigues/chronicler
* Source code: https://github.com/cran/chronicler
* Date/Publication: 2022-05-17 09:40:04 UTC
* Number of recursive dependencies: 58

Run `cloud_details(, "chronicler")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘advanced-topics.Rmd’ using rmarkdown
    --- finished re-building ‘advanced-topics.Rmd’
    
    --- re-building ‘maybe-monad.Rmd’ using rmarkdown
    --- finished re-building ‘maybe-monad.Rmd’
    
    --- re-building ‘real-world-example.Rmd’ using rmarkdown
    Quitting from lines 99-101 (real-world-example.Rmd) 
    ...
    Error: processing vignette 'real-world-example.Rmd' failed with diagnostics:
    Must only be used inside data-masking verbs like `mutate()`, `filter()`,
    and `group_by()`.
    --- failed re-building ‘real-world-example.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘real-world-example.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
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
     17.   └─vctrs::vec_default_cast(...)
     18.     ├─base::withRestarts(...)
     19.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     20.     │   └─base (local) doWithOneRestart(return(expr), restart)
     21.     └─vctrs::stop_incompatible_cast(...)
     22.       └─vctrs::stop_incompatible_type(...)
     23.         └─vctrs:::stop_incompatible(...)
     24.           └─vctrs:::stop_vctrs(...)
     25.             └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
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
      ── Error ('test-tidying_functions.R:29'): score works ──────────────────────────
      <dplyr:::mutate_error/rlang_error/error/condition>
      Error in `mutate(.tbl, !!!funs)`: i In argument: `PA = (structure(function (..., .x = ..1, .y = ..2, . = ..1) ...`.
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
      ..1) ...`.
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
      [ FAIL 1 | WARN 22 | SKIP 5 | PASS 256 ]
      
      ══ Skipped tests ═══════════════════════════════════════════════════════════════
      • On CRAN (5)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-head-to-head.R:168'): h2h_mat allows multiple Head-to-Head functions ──
      `h2h_mat(cr_data)` produced warnings.
      
      [ FAIL 1 | WARN 22 | SKIP 5 | PASS 256 ]
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
     21.   └─vctrs::vec_default_cast(...)
     22.     ├─base::withRestarts(...)
     23.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     24.     │   └─base (local) doWithOneRestart(return(expr), restart)
     25.     └─vctrs::stop_incompatible_cast(...)
     26.       └─vctrs::stop_incompatible_type(...)
     27.         └─vctrs:::stop_incompatible(...)
     28.           └─vctrs:::stop_vctrs(...)
     29.             └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── Error ('testMakeHistoryTwo.r:71'): Formats and Names under Dropout ──────────
      <dplyr:::mutate_error/rlang_error/error/condition>
      Error in `mutate(., his.name.a = name.history.a, his.name.b = name.history.b, 
          his.time.a = .data$exp.time, his.time.b = .data$exp.time, 
          his.lag = if_else(.data$exp.time == first(.data$exp.time, 
              default = "NA"), "H", lag(paste(.data$exp.value.a, .data$exp.value.b, 
              sep = ""))), his.value.a = CumPaste(.data$his.lag), his.value.b = paste(CumPaste(.data$his.lag), 
              .data$exp.value.a, sep = ""))`: ℹ In argument: `his.lag = if_else(...)`.
      ℹ In group 1: `ID = 1`.
      Caused by error in `nth()`:
      ! Can't convert `default` <character> to match type of `x` <double>.
      
      [ FAIL 4 | WARN 58 | SKIP 0 | PASS 342 ]
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
    ℹ In argument: `his.lag = if_else(...)`.
    ℹ In group 1: `ID = 1`.
    Caused by error in `nth()`:
    ! Can't convert `default` <character> to match type of `x` <double>.
    --- failed re-building ‘quickdemo.Rmd’
    ...
    ℹ In group 1: `ID = 1001`.
    Caused by error in `nth()`:
    ! Can't convert `default` <character> to match type of `x` <double>.
    --- failed re-building ‘selectionbias.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘quickdemo.Rmd’ ‘selectionbias.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# dbGaPCheckup

<details>

* Version: 1.0.0
* GitHub: https://github.com/lwheinsberg/dbGaPCheckup
* Source code: https://github.com/cran/dbGaPCheckup
* Date/Publication: 2022-11-14 11:20:05 UTC
* Number of recursive dependencies: 109

Run `cloud_details(, "dbGaPCheckup")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘dbGaPCheckup-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: check_report
    > ### Title: Check Report
    > ### Aliases: check_report
    > 
    > ### ** Examples
    > 
    > # Example 1: Incorrectly showing as pass check on first attempt
    ...
     10.   └─vctrs::vec_default_cast(...)
     11.     ├─base::withRestarts(...)
     12.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     13.     │   └─base (local) doWithOneRestart(return(expr), restart)
     14.     └─vctrs::stop_incompatible_cast(...)
     15.       └─vctrs::stop_incompatible_type(...)
     16.         └─vctrs:::stop_incompatible(...)
     17.           └─vctrs:::stop_vctrs(...)
     18.             └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘dbGaPCheckup.Rmd’ using rmarkdown
    Quitting from lines 116-117 (dbGaPCheckup.Rmd) 
    Error: processing vignette 'dbGaPCheckup.Rmd' failed with diagnostics:
    Can't convert `y` <double> to match type of `x` <data.frame>.
    --- failed re-building ‘dbGaPCheckup.Rmd’
    
    --- re-building ‘dbGaPCheckup_vignette.Rmd’ using rmarkdown
    Quitting from lines 181-182 (dbGaPCheckup_vignette.Rmd) 
    Error: processing vignette 'dbGaPCheckup_vignette.Rmd' failed with diagnostics:
    Can't convert `y` <double> to match type of `x` <data.frame>.
    --- failed re-building ‘dbGaPCheckup_vignette.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘dbGaPCheckup.Rmd’ ‘dbGaPCheckup_vignette.Rmd’
    
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
    Appending disk.frames: 
    Error in anti_join(.x, .y, by = by, copy = copy, ..., overwrite = overwrite) : 
      `...` must be empty.
    ✖ Problematic arguments:
    • ..1 = xch
    • ..2 = ych
    • overwrite = overwrite
    ℹ Did you forget to name an argument?
    Calls: anti_join ... resolve.list -> signalConditionsASAP -> signalConditions
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
    ...
     14.   └─vctrs::vec_default_cast(...)
     15.     ├─base::withRestarts(...)
     16.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     17.     │   └─base (local) doWithOneRestart(return(expr), restart)
     18.     └─vctrs::stop_incompatible_cast(...)
     19.       └─vctrs::stop_incompatible_type(...)
     20.         └─vctrs:::stop_incompatible(...)
     21.           └─vctrs:::stop_vctrs(...)
     22.             └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
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
      ── Error ('test-spat_prox.R:10'): spat_prox .name works ────────────────────────
      Error in `.gmat * .c`: non-numeric argument to binary operator
      Backtrace:
          ▆
       1. └─divseg::ds_spat_prox(...) at test-spat_prox.R:10:2
       2.   └─divseg:::calc_pgg(.data, sub %>% dplyr::pull(.data$.x))
      
      [ FAIL 32 | WARN 73 | SKIP 0 | PASS 26 ]
      Error: Test failures
      Execution halted
    ```

# dm

<details>

* Version: 1.0.3
* GitHub: https://github.com/cynkra/dm
* Source code: https://github.com/cran/dm
* Date/Publication: 2022-10-12 15:42:33 UTC
* Number of recursive dependencies: 153

Run `cloud_details(, "dm")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      <dplyr:::mutate_error/rlang_error/error/condition>
      Error in `mutate(., zoom = if_else(table == "tf_1", list(1), NULL))`: i In argument: `zoom = if_else(table == "tf_1", list(1), NULL)`.
      Caused by error in `if_else()`:
      ! `false` must be a vector, not `NULL`.
      ── Error ('test-zoom.R:99'): dm_update_tbl() works ─────────────────────────────
      <dplyr:::mutate_error/rlang_error/error/condition>
      Error in `mutate(., zoom = if_else(table == "tf_6", list(tf_7()), NULL), 
          col_tracker_zoom = if_else(table == "tf_6", list(character()), 
              NULL), )`: i In argument: `zoom = if_else(table == "tf_6", list(tf_7()), NULL)`.
      Caused by error in `if_else()`:
      ! `false` must be a vector, not `NULL`.
      
      [ FAIL 2 | WARN 18 | SKIP 191 | PASS 1317 ]
      Error: Test failures
      Execution halted
    ```

# dodgr

<details>

* Version: 0.2.18
* GitHub: https://github.com/ATFutures/dodgr
* Source code: https://github.com/cran/dodgr
* Date/Publication: 2022-12-07 16:00:07 UTC
* Number of recursive dependencies: 110

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
      [ FAIL 1 | WARN 0 | SKIP 11 | PASS 217 ]
      
      ══ Skipped tests ═══════════════════════════════════════════════════════════════
      • !test_all is TRUE (7)
      • On CRAN (4)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-iso.R:17'): isodists ─────────────────────────────────────────
      `net <- weight_streetnet(hsc, wt_profile = "bicycle")` produced warnings.
      
      [ FAIL 1 | WARN 0 | SKIP 11 | PASS 217 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 34.9Mb
      sub-directories of 1Mb or more:
        doc    6.5Mb
        libs  27.8Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# dtplyr

<details>

* Version: 1.2.2
* GitHub: https://github.com/tidyverse/dtplyr
* Source code: https://github.com/cran/dtplyr
* Date/Publication: 2022-08-20 13:20:02 UTC
* Number of recursive dependencies: 66

Run `cloud_details(, "dtplyr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Backtrace:
          ▆
       1. ├─testthat::expect_error(slice_head(dt, 5), class = "rlib_error_dots_nonempty") at test-step-subset-slice.R:122:2
       2. │ └─testthat:::expect_condition_matching(...)
       3. │   └─testthat:::quasi_capture(...)
       4. │     ├─testthat (local) .capture(...)
       5. │     │ └─base::withCallingHandlers(...)
       6. │     └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
       7. └─dplyr::slice_head(dt, 5)
       8.   └─dplyr:::check_slice_unnamed_n_prop(..., n = n, prop = prop)
       9.     └─rlang::abort(bullets, call = error_call)
      
      [ FAIL 1 | WARN 0 | SKIP 25 | PASS 603 ]
      Error: Test failures
      Execution halted
    ```

# epocakir

<details>

* Version: 0.9.8
* GitHub: https://github.com/alwinw/epocakir
* Source code: https://github.com/cran/epocakir
* Date/Publication: 2022-05-04 23:00:16 UTC
* Number of recursive dependencies: 77

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
    ...
     10. └─vctrs (local) `<fn>`()
     11.   └─vctrs::vec_default_ptype2(...)
     12.     ├─base::withRestarts(...)
     13.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     14.     │   └─base (local) doWithOneRestart(return(expr), restart)
     15.     └─vctrs::stop_incompatible_type(...)
     16.       └─vctrs:::stop_incompatible(...)
     17.         └─vctrs:::stop_vctrs(...)
     18.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        8. │       └─dplyr:::vec_case_when(...)
        9. │         └─vctrs::vec_ptype_common(!!!everything, .ptype = ptype, .call = call)
       10. └─vctrs (local) `<fn>`()
       11.   └─vctrs::vec_default_ptype2(...)
       12.     ├─base::withRestarts(...)
       13.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
       14.     │   └─base (local) doWithOneRestart(return(expr), restart)
       15.     └─vctrs::stop_incompatible_type(...)
       16.       └─vctrs:::stop_incompatible(...)
       17.         └─vctrs:::stop_vctrs(...)
       18.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
      
      [ FAIL 3 | WARN 9 | SKIP 0 | PASS 390 ]
      Error: Test failures
      Execution halted
    ```

# ethnobotanyR

<details>

* Version: 0.1.8
* GitHub: https://github.com/CWWhitney/ethnobotanyR
* Source code: https://github.com/cran/ethnobotanyR
* Date/Publication: 2021-01-06 04:10:02 UTC
* Number of recursive dependencies: 69

Run `cloud_details(, "ethnobotanyR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ethnobotanyR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: FLs
    > ### Title: Fidelity Level (FL)
    > ### Aliases: FLs
    > ### Keywords: arith logic math methods misc survey
    > 
    > ### ** Examples
    > 
    ...
        ▆
     1. └─ethnobotanyR::FLs(ethnobotanydata)
     2.   ├─dplyr::left_join(Iu, Ip, by = "sp_name", na.rm = TRUE)
     3.   └─dplyr:::left_join.data.frame(Iu, Ip, by = "sp_name", na.rm = TRUE)
     4.     └─rlang::check_dots_empty0(...)
     5.       └─rlang::check_dots_empty(call = call)
     6.         └─rlang:::action_dots(...)
     7.           ├─base (local) try_dots(...)
     8.           └─rlang (local) action(...)
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘ethnobotanyr_vignette.Rmd’ using rmarkdown
    Quitting from lines 192-193 (ethnobotanyr_vignette.Rmd) 
    Error: processing vignette 'ethnobotanyr_vignette.Rmd' failed with diagnostics:
    `...` must be empty.
    ✖ Problematic argument:
    • na.rm = TRUE
    --- failed re-building ‘ethnobotanyr_vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘ethnobotanyr_vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# extdplyr

<details>

* Version: 0.1.5
* GitHub: NA
* Source code: https://github.com/cran/extdplyr
* Date/Publication: 2020-04-20 05:20:02 UTC
* Number of recursive dependencies: 39

Run `cloud_details(, "extdplyr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘extdplyr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ind_to_char
    > ### Title: Convert indicator data.frame to character/factor.
    > ### Aliases: ind_to_char ind_to_char_
    > 
    > ### ** Examples
    > 
    > # Supports converting the following atomic types to indicator
    ...
    ! `select_vars()` was deprecated in dplyr 0.8.4 and is now defunct.
    ℹ Please use `tidyselect::vars_select()` instead.
    Backtrace:
        ▆
     1. └─extdplyr::ind_to_char(ind_df, new_y, ya:ye)
     2.   └─dplyr::select_vars(colnames(data), ...)
     3.     └─lifecycle::deprecate_stop("0.8.4", "select_vars()", "tidyselect::vars_select()")
     4.       └─lifecycle:::deprecate_stop0(msg)
     5.         └─rlang::cnd_signal(...)
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# exuber

<details>

* Version: 1.0.0
* GitHub: https://github.com/kvasilopoulos/exuber
* Source code: https://github.com/cran/exuber
* Date/Publication: 2022-08-19 13:50:05 UTC
* Number of recursive dependencies: 100

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
       35.   └─dplyr:::rethrow_warning_join_matches_multiple(cnd, error_call)
       36.     └─dplyr:::warn_join(...)
       37.       └─dplyr:::warn_dplyr(...)
       38.         └─rlang::warn(...)
       39.           └─base::warning(cnd)
       40.             └─base::withRestarts(...)
       41.               └─base (local) withOneRestart(expr, restarts[[1L]])
       42.                 └─base (local) doWithOneRestart(return(expr), restart)
      
      [ FAIL 42 | WARN 56 | SKIP 4 | PASS 194 ]
      Error: Test failures
      Execution halted
    ```

# fastqcr

<details>

* Version: 0.1.2
* GitHub: https://github.com/kassambara/fastqcr
* Source code: https://github.com/cran/fastqcr
* Date/Publication: 2019-01-03 00:20:16 UTC
* Number of recursive dependencies: 76

Run `cloud_details(, "fastqcr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘fastqcr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: qc_aggregate
    > ### Title: Aggregate FastQC Reports for Multiple Samples
    > ### Aliases: qc_aggregate summary.qc_aggregate qc_stats
    > 
    > ### ** Examples
    > 
    > # Demo QC dir
    ...
     1. ├─base::summary(qc)
     2. └─fastqcr:::summary.qc_aggregate(qc)
     3.   ├─dplyr::left_join(res, failed, by = "module", fill = "---")
     4.   └─dplyr:::left_join.data.frame(res, failed, by = "module", fill = "---")
     5.     └─rlang::check_dots_empty0(...)
     6.       └─rlang::check_dots_empty(call = call)
     7.         └─rlang:::action_dots(...)
     8.           ├─base (local) try_dots(...)
     9.           └─rlang (local) action(...)
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# finnts

<details>

* Version: 0.2.1
* GitHub: https://github.com/microsoft/finnts
* Source code: https://github.com/cran/finnts
* Date/Publication: 2022-11-15 23:30:02 UTC
* Number of recursive dependencies: 210

Run `cloud_details(, "finnts")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-forecast_time_series.R:195'): final forecast data rows are meaningful ──
      nrow(future_frame) (`actual`) not equal to length(unique(inp_data_combos$Combo)) * forecast_horizon (`expected`).
      
        `actual`:  8
      `expected`: 15
      ── Failure ('test-forecast_time_series.R:294'): final forecast data rows are meaningful ──
      nrow(future_frame) (`actual`) not equal to length(unique(inp_data_combos$Combo)) * forecast_horizon (`expected`).
      
        `actual`: 27
      `expected`: 48
      
      [ FAIL 2 | WARN 11 | SKIP 0 | PASS 111 ]
      Error: Test failures
      Execution halted
    ```

# forceR

<details>

* Version: 1.0.15
* GitHub: https://github.com/Peter-T-Ruehr/forceR
* Source code: https://github.com/cran/forceR
* Date/Publication: 2022-06-07 14:50:02 UTC
* Number of recursive dependencies: 85

Run `cloud_details(, "forceR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘forceR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: find_strongest_peaks
    > ### Title: Find Peaks
    > ### Aliases: find_strongest_peaks
    > 
    > ### ** Examples
    > 
    > require(dplyr)
    ...
     1. ├─forceR::find_strongest_peaks(df = df.all.200.tax_filtered, no.of.peaks = 5)
     2. │ └─curr.plot.window %>% slice(n = 1) %>% pull(specimen)
     3. ├─dplyr::pull(., specimen)
     4. ├─dplyr::slice(., n = 1)
     5. └─dplyr:::slice.data.frame(., n = 1)
     6.   └─rlang::check_dots_unnamed()
     7.     └─rlang:::action_dots(...)
     8.       ├─base (local) try_dots(...)
     9.       └─rlang (local) action(...)
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘forceR.Rmd’ using rmarkdown
    Quitting from lines 518-523 (forceR.Rmd) 
    Error: processing vignette 'forceR.Rmd' failed with diagnostics:
    Arguments in `...` must be passed by position, not name.
    ✖ Problematic argument:
    • n = 1
    --- failed re-building ‘forceR.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘forceR.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# forestmangr

<details>

* Version: 0.9.4
* GitHub: https://github.com/sollano/forestmangr
* Source code: https://github.com/cran/forestmangr
* Date/Publication: 2021-08-16 13:00:02 UTC
* Number of recursive dependencies: 121

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
     15.   └─vctrs::vec_default_cast(...)
     16.     ├─base::withRestarts(...)
     17.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     18.     │   └─base (local) doWithOneRestart(return(expr), restart)
     19.     └─vctrs::stop_incompatible_cast(...)
     20.       └─vctrs::stop_incompatible_type(...)
     21.         └─vctrs:::stop_incompatible(...)
     22.           └─vctrs:::stop_vctrs(...)
     23.             └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘eq_group_fit_en.Rmd’ using rmarkdown
    --- finished re-building ‘eq_group_fit_en.Rmd’
    
    --- re-building ‘eq_group_fit_ptbr.Rmd’ using rmarkdown
    --- finished re-building ‘eq_group_fit_ptbr.Rmd’
    
    --- re-building ‘invent_vol_plot_en.Rmd’ using rmarkdown
    Quitting from lines 90-92 (invent_vol_plot_en.Rmd) 
    Error: processing vignette 'invent_vol_plot_en.Rmd' failed with diagnostics:
    ...
    
    --- re-building ‘yield_growth_ptbr.Rmd’ using rmarkdown
    --- finished re-building ‘yield_growth_ptbr.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘invent_vol_plot_en.Rmd’ ‘invent_vol_plot_ptbr.Rmd’ ‘sampling_en.Rmd’
      ‘sampling_ptbr.Rmd’ ‘volume_est_en.Rmd’ ‘volume_est_ptbr.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# fqar

<details>

* Version: 0.2.1
* GitHub: https://github.com/equitable-equations/fqar
* Source code: https://github.com/cran/fqar
* Date/Publication: 2022-08-23 12:40:05 UTC
* Number of recursive dependencies: 75

Run `cloud_details(, "fqar")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘fqar-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: assessment_glance
    > ### Title: Obtain tidy summary information for a floristic quality
    > ###   assessment
    > ### Aliases: assessment_glance
    > 
    > ### ** Examples
    > 
    ...
      5.   └─vctrs::vec_default_cast(...)
      6.     ├─base::withRestarts(...)
      7.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
      8.     │   └─base (local) doWithOneRestart(return(expr), restart)
      9.     └─vctrs::stop_incompatible_cast(...)
     10.       └─vctrs::stop_incompatible_type(...)
     11.         └─vctrs:::stop_incompatible(...)
     12.           └─vctrs:::stop_vctrs(...)
     13.             └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        3. │   └─vctrs::vec_cast(x = y, to = x, x_arg = "y", to_arg = "x")
        4. └─vctrs (local) `<fn>`()
        5.   └─vctrs::vec_default_cast(...)
        6.     ├─base::withRestarts(...)
        7.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
        8.     │   └─base (local) doWithOneRestart(return(expr), restart)
        9.     └─vctrs::stop_incompatible_cast(...)
       10.       └─vctrs::stop_incompatible_type(...)
       11.         └─vctrs:::stop_incompatible(...)
       12.           └─vctrs:::stop_vctrs(...)
       13.             └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
      
      [ FAIL 7 | WARN 0 | SKIP 7 | PASS 21 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘fqar.Rmd’ using rmarkdown
    Quitting from lines 93-95 (fqar.Rmd) 
    Error: processing vignette 'fqar.Rmd' failed with diagnostics:
    Can't convert `y` <character> to match type of `x` <data.frame>.
    --- failed re-building ‘fqar.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘fqar.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# FRK

<details>

* Version: 2.1.0
* GitHub: https://github.com/andrewzm/FRK
* Source code: https://github.com/cran/FRK
* Date/Publication: 2022-09-15 09:40:08 UTC
* Number of recursive dependencies: 153

Run `cloud_details(, "FRK")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Error in `.xts(e, .index(e2), tclass(e2), tzone(e2), tformat = tformat(e2))`: index length must match number of observations
      Backtrace:
          ▆
       1. └─FRK::SRE(f, list(STobj), G, grid_BAUs, est_error = FALSE) at test_SRE.R:206:4
       2.   ├─FRK:::map_data_to_BAUs(...)
       3.   └─FRK:::map_data_to_BAUs(...)
       4.     └─FRK (local) .local(data_sp, sp_pols, average_in_BAU, sum_variables, silently)
       5.       └─base::lapply(...)
       6.         └─FRK (local) FUN(X[[i]], ...)
       7.           └─xts:::Ops.xts(i, last(sp_pols@time))
       8.             └─xts::.xts(e, .index(e2), tclass(e2), tzone(e2), tformat = tformat(e2))
      
      [ FAIL 2 | WARN 83 | SKIP 0 | PASS 203 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘FRK_intro.Rnw’ using knitr
    Quitting from lines 772-785 (FRK_intro.Rnw) 
    Error: processing vignette 'FRK_intro.Rnw' failed with diagnostics:
    index length must match number of observations
    --- failed re-building ‘FRK_intro.Rnw’
    
    --- re-building ‘FRK_non-Gaussian.Rnw’ using knitr
    Error: processing vignette 'FRK_non-Gaussian.Rnw' failed with diagnostics:
    Running 'texi2dvi' on 'FRK_non-Gaussian.tex' failed.
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
      installed size is 77.9Mb
      sub-directories of 1Mb or more:
        data   5.0Mb
        doc    1.6Mb
        libs  70.3Mb
    ```

# GauPro

<details>

* Version: 0.2.6
* GitHub: https://github.com/CollinErickson/GauPro
* Source code: https://github.com/cran/GauPro
* Date/Publication: 2022-11-24 08:40:02 UTC
* Number of recursive dependencies: 78

Run `cloud_details(, "GauPro")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘GauPro-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: FactorKernel
    > ### Title: Factor Kernel R6 class
    > ### Aliases: FactorKernel
    > 
    > ### ** Examples
    > 
    > kk <- FactorKernel$new(D=1, nlevels=5, xindex=1)
    ...
    
    > n <- 20
    > X <- cbind(matrix(runif(n,2,6), ncol=1),
    +            matrix(sample(1:2, size=n, replace=TRUE), ncol=1))
    > X <- rbind(X, c(3.3,3))
    > n <- nrow(X)
    > Z <- X[,1] - (X[,2]-1.8)^2 + rnorm(n,0,.1)
    > tibble(X=X, Z) %>% arrange(X,Z)
    Error: Internal error: `vec_order_expand_args()` should expand `decreasing` to have length 1 or length equal to the number of columns of `x` after calling `vec_proxy_order()`.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 15.0Mb
      sub-directories of 1Mb or more:
        R      1.1Mb
        libs  13.3Mb
    ```

# ggblanket

<details>

* Version: 1.6.1
* GitHub: https://github.com/davidhodge931/ggblanket
* Source code: https://github.com/cran/ggblanket
* Date/Publication: 2022-11-18 22:20:02 UTC
* Number of recursive dependencies: 95

Run `cloud_details(, "ggblanket")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggblanket-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: add_tooltip
    > ### Title: Add a tooltip column
    > ### Aliases: add_tooltip
    > 
    > ### ** Examples
    > 
    > library(ggplot2)
    ...
     16.   └─vctrs::vec_default_cast(...)
     17.     ├─base::withRestarts(...)
     18.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     19.     │   └─base (local) doWithOneRestart(return(expr), restart)
     20.     └─vctrs::stop_incompatible_cast(...)
     21.       └─vctrs::stop_incompatible_type(...)
     22.         └─vctrs:::stop_incompatible(...)
     23.           └─vctrs:::stop_vctrs(...)
     24.             └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘ggblanket.Rmd’ using rmarkdown
    Quitting from lines 84-90 (ggblanket.Rmd) 
    Error: processing vignette 'ggblanket.Rmd' failed with diagnostics:
    ℹ In argument: `dplyr::across(...)`.
    Caused by error in `across()`:
    ! Can't compute column `Species`.
    Caused by error in `dplyr::na_if()`:
    ! Can't convert `y` <double> to match type of `x` <character>.
    --- failed re-building ‘ggblanket.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘ggblanket.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# gratia

<details>

* Version: 0.7.3
* GitHub: https://github.com/gavinsimpson/gratia
* Source code: https://github.com/cran/gratia
* Date/Publication: 2022-05-09 11:20:03 UTC
* Number of recursive dependencies: 83

Run `cloud_details(, "gratia")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘test-all.R’
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      • hgam-paper/hgam-paper-bird-move-model-1.svg
      • hgam-paper/hgam-paper-bird-move-model-2.svg
      • hgam-paper/hgam-paper-bird-move-model-3.svg
      • hgam-paper/hgam-paper-bird-move-model-5.svg
      • hgam-paper/hgam-paper-co2-model-1.svg
      • hgam-paper/hgam-paper-co2-model-2.svg
      • hgam-paper/hgam-paper-co2-model-3.svg
      • hgam-paper/hgam-paper-co2-model-4.svg
      • hgam-paper/hgam-paper-co2-model-5.svg
      • hgam-paper/hgam-paper-zoop-model-4.svg
      • hgam-paper/hgam-paper-zoop-model-5.svg
      • rootograms/draw-gaussian-rootogram.svg
      • rootograms/draw-neg-bin-rootogram.svg
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

* Version: 0.3.1
* GitHub: https://github.com/davidsjoberg/hablar
* Source code: https://github.com/cran/hablar
* Date/Publication: 2022-11-11 19:10:02 UTC
* Number of recursive dependencies: 91

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
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 418 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test.ifs.R:53'): if_else_ ─────────────────────────────────────────
      `if_else_(c(T, F, NA), 1, 1L)` did not throw an error.
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 418 ]
      Error: Test failures
      Execution halted
    ```

# headliner

<details>

* Version: 0.0.2
* GitHub: https://github.com/rjake/headliner
* Source code: https://github.com/cran/headliner
* Date/Publication: 2022-06-26 23:40:02 UTC
* Number of recursive dependencies: 72

Run `cloud_details(, "headliner")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        6. │     └─dplyr:::filter_eval(dots, mask = mask, error_call = error_call)
        7. │       ├─base::withCallingHandlers(...)
        8. │       └─mask$eval_all_filter(dots, env_filter)
        9. │         └─dplyr (local) eval()
       10. └─base::.handleSimpleError(`<fn>`, "object '' not found", base::quote(eval()))
       11.   └─dplyr (local) h(simpleError(msg, call))
       12.     └─dplyr:::local_error_context(dots, i = frame[[i_sym]], mask = mask)
       13.       └─dplyr:::new_error_context(dots, i, mask = mask)
       14.         └─dplyr:::quo_as_label(dots[[i]])
       15.           └─dplyr:::is_data_pronoun(expr)
       16.             └─rlang::is_call(x, c("[[", "$"))
      
      [ FAIL 3 | WARN 4 | SKIP 0 | PASS 101 ]
      Error: Test failures
      Execution halted
    ```

# heemod

<details>

* Version: 0.14.4
* GitHub: https://github.com/pierucci/heemod
* Source code: https://github.com/cran/heemod
* Date/Publication: 2021-10-06 11:30:12 UTC
* Number of recursive dependencies: 118

Run `cloud_details(, "heemod")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in documentation object 'define_parameters.Rd':
      ‘[dplyr:ranking]{dplyr::row_number()}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
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
      4. └─vctrs (local) `<fn>`()
      5.   └─vctrs::vec_default_ptype2(...)
      6.     ├─base::withRestarts(...)
      7.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
      8.     │   └─base (local) doWithOneRestart(return(expr), restart)
      9.     └─vctrs::stop_incompatible_type(...)
     10.       └─vctrs:::stop_incompatible(...)
     11.         └─vctrs:::stop_vctrs(...)
     12.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked UTF-8 string
    ```

# hlaR

<details>

* Version: 0.1.5
* GitHub: https://github.com/LarsenLab/hlaR
* Source code: https://github.com/cran/hlaR
* Date/Publication: 2022-10-24 21:35:04 UTC
* Number of recursive dependencies: 151

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
     14.   └─vctrs::vec_default_cast(...)
     15.     ├─base::withRestarts(...)
     16.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     17.     │   └─base (local) doWithOneRestart(return(expr), restart)
     18.     └─vctrs::stop_incompatible_cast(...)
     19.       └─vctrs::stop_incompatible_type(...)
     20.         └─vctrs:::stop_incompatible(...)
     21.           └─vctrs:::stop_vctrs(...)
     22.             └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘allele-haplotype.Rmd’ using rmarkdown
    --- finished re-building ‘allele-haplotype.Rmd’
    
    --- re-building ‘eplet-mm.Rmd’ using rmarkdown
    Quitting from lines 48-53 (eplet-mm.Rmd) 
    Error: processing vignette 'eplet-mm.Rmd' failed with diagnostics:
    ℹ In argument: `index = (function (x, y) ...`.
    Caused by error:
    ! Can't convert `y` <character> to match type of `x` <integer>.
    --- failed re-building ‘eplet-mm.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘eplet-mm.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# huito

<details>

* Version: 0.2.1
* GitHub: https://github.com/flavjack/huito
* Source code: https://github.com/cran/huito
* Date/Publication: 2022-08-11 09:40:16 UTC
* Number of recursive dependencies: 157

Run `cloud_details(, "huito")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘germinar.Rmd’ using rmarkdown
    --- finished re-building ‘germinar.Rmd’
    
    --- re-building ‘huito.Rmd’ using rmarkdown
    --- finished re-building ‘huito.Rmd’
    
    --- re-building ‘labels.Rmd’ using rmarkdown
    Quitting from lines 22-36 (labels.Rmd) 
    ...
    --- failed re-building ‘labels.Rmd’
    
    --- re-building ‘stickers.Rmd’ using rmarkdown
    --- finished re-building ‘stickers.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘labels.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# huxtable

<details>

* Version: 5.5.1
* GitHub: https://github.com/hughjonesd/huxtable
* Source code: https://github.com/cran/huxtable
* Date/Publication: 2022-11-12 08:30:07 UTC
* Number of recursive dependencies: 162

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
        ▆
     1. ├─huxtable::map_background_color(...)
     2. │ └─huxtable (local) fn(ht, rc$row, rc$col, current)
     3. │   └─dplyr::case_when(!!!cases)
     4. │     └─vctrs::vec_size_common(...)
     5. └─vctrs::stop_incompatible_size(...)
     6.   └─vctrs:::stop_incompatible(...)
     7.     └─vctrs:::stop_vctrs(...)
     8.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
           ▆
        1. ├─testthat::expect_equivalent(...) at test-mapping-functions.R:123:2
        2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
        3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
        4. ├─huxtable (local) f(m, 1:3, 1:2, ct)
        5. │ └─dplyr::case_when(!!!cases)
        6. │   └─vctrs::vec_size_common(...)
        7. └─vctrs::stop_incompatible_size(...)
        8.   └─vctrs:::stop_incompatible(...)
        9.     └─vctrs:::stop_vctrs(...)
       10.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
      
      [ FAIL 2 | WARN 2 | SKIP 25 | PASS 1230 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘R6’ ‘xml2’
      All declared Imports should be used.
    ```

# iNZightTools

<details>

* Version: 1.12.3
* GitHub: https://github.com/iNZightVIT/iNZightTools
* Source code: https://github.com/cran/iNZightTools
* Date/Publication: 2022-08-22 20:20:02 UTC
* Number of recursive dependencies: 87

Run `cloud_details(, "iNZightTools")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      * suffix = c(".x", ".y")
      Backtrace:
          ▆
       1. ├─d1 %>% ...
       2. ├─dplyr::anti_join(...)
       3. └─dplyr:::anti_join.data.frame(., d3, by = c(x1 = "x1", x2 = "x6"), suffix = c(".x", ".y"))
       4.   └─rlang::check_dots_empty0(...)
       5.     └─rlang::check_dots_empty(call = call)
       6.       └─rlang:::action_dots(...)
       7.         ├─base (local) try_dots(...)
       8.         └─rlang (local) action(...)
      
      [ FAIL 2 | WARN 0 | SKIP 5 | PASS 331 ]
      Error: Test failures
      Execution halted
    ```

# IPEDSuploadables

<details>

* Version: 2.6.5
* GitHub: https://github.com/AlisonLanski/IPEDSuploadables
* Source code: https://github.com/cran/IPEDSuploadables
* Date/Publication: 2022-12-07 21:02:33 UTC
* Number of recursive dependencies: 79

Run `cloud_details(, "IPEDSuploadables")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘IPEDSuploadables-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: produce_gr_report
    > ### Title: Shortcut function with all steps to provide a Graduation Rates
    > ###   report
    > ### Aliases: produce_gr_report
    > 
    > ### ** Examples
    > 
    ...
      2. │ ├─IPEDSuploadables::write_report(...)
      3. │ └─IPEDSuploadables::make_gr_part_E(df, ugender)
      4. │   └─... %>% ...
      5. ├─dplyr::transmute(...)
      6. ├─dplyr::mutate(...)
      7. ├─dplyr::union_all(., data.frame(GRGU011 = integer(), GRGU012 = integer()))
      8. └─dplyr:::union_all.data.frame(., data.frame(GRGU011 = integer(), GRGU012 = integer()))
      9.   └─dplyr:::check_compatible(x, y)
     10.     └─rlang::abort(c("`x` and `y` are not compatible.", compat), call = error_call)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        1. ├─testthat::expect_equal(make_gr_part_E(gr_students, TRUE), part_outputs$gr_partE) at test-part-outputs.R:67:2
        2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
        3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
        4. ├─IPEDSuploadables::make_gr_part_E(gr_students, TRUE)
        5. │ └─... %>% ...
        6. ├─dplyr::transmute(...)
        7. ├─dplyr::mutate(...)
        8. ├─dplyr::union_all(., data.frame(GRGU011 = integer(), GRGU012 = integer()))
        9. └─dplyr:::union_all.data.frame(., data.frame(GRGU011 = integer(), GRGU012 = integer()))
       10.   └─dplyr:::check_compatible(x, y)
       11.     └─rlang::abort(c("`x` and `y` are not compatible.", compat), call = error_call)
      
      [ FAIL 1 | WARN 71 | SKIP 0 | PASS 62 ]
      Error: Test failures
      Execution halted
    ```

# janitor

<details>

* Version: 2.1.0
* GitHub: https://github.com/sfirke/janitor
* Source code: https://github.com/cran/janitor
* Date/Publication: 2021-01-05 01:10:04 UTC
* Number of recursive dependencies: 73

Run `cloud_details(, "janitor")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error ('test-adorn-percentages.R:69'): works with totals row/col when denominator = col or all, #357 ──
      Error in `FUN(X[[i]], ...)`: only defined on a data frame with all numeric-alike variables
      Backtrace:
          ▆
       1. ├─source1 %>% adorn_totals(where = c("col", "row")) %>% ... at test-adorn-percentages.R:69:2
       2. └─janitor::adorn_percentages(., denominator = "col")
       3.   └─base::Summary.data.frame(`<tabyl[,3]>`, na.rm = FALSE)
       4.     └─base::lapply(...)
       5.       └─base (local) FUN(X[[i]], ...)
      
      [ FAIL 1 | WARN 1 | SKIP 1 | PASS 639 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘janitor.Rmd’ using rmarkdown
    --- finished re-building ‘janitor.Rmd’
    
    --- re-building ‘tabyls.Rmd’ using rmarkdown
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
       13. │ └─vctrs::vec_cast(x = y, to = x, x_arg = "y", to_arg = "x")
       14. └─vctrs (local) `<fn>`()
       15.   └─vctrs::vec_default_cast(...)
       16.     ├─base::withRestarts(...)
       17.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
       18.     │   └─base (local) doWithOneRestart(return(expr), restart)
       19.     └─vctrs::stop_incompatible_cast(...)
       20.       └─vctrs::stop_incompatible_type(...)
       21.         └─vctrs:::stop_incompatible(...)
       22.           └─vctrs:::stop_vctrs(...)
       23.             └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
      
      [ FAIL 3 | WARN 0 | SKIP 42 | PASS 1 ]
      Error: Test failures
      Execution halted
    ```

# kmscv

<details>

* Version: 0.1.0
* GitHub: https://github.com/sametsoekel/kmscv
* Source code: https://github.com/cran/kmscv
* Date/Publication: 2022-06-28 16:20:08 UTC
* Number of recursive dependencies: 48

Run `cloud_details(, "kmscv")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘kmscv-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: kms_cv
    > ### Title: K-Means Based Cross-Validation
    > ### Aliases: kms_cv
    > 
    > ### ** Examples
    > 
    > library(kmscv)
    ...
     1. └─kmscv::kms_cv(...)
     2.   └─purrr::map2_dfr(...)
     3.     └─purrr::map2(.x, .y, .f, ...)
     4.       └─kmscv (local) .f(.x[[i]], .y[[i]], ...)
     5.         ├─dplyr::slice_sample(.x, n = .y)
     6.         └─dplyr:::slice_sample.data.frame(.x, n = .y)
     7.           └─dplyr:::get_slice_size(n = n, prop = prop, allow_outsize = replace)
     8.             └─dplyr:::check_slice_n_prop(n, prop, error_call = error_call)
     9.               └─rlang::abort(...)
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
      [ FAIL 4 | WARN 18 | SKIP 0 | PASS 140 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-load-data.R:24'): test that it is possible to load multiple LANS summaries ──
      `... <- NULL` produced warnings.
      ── Failure ('test-load-data.R:43'): test that it is possible to load multiple LANS summaries ──
      `... <- NULL` produced warnings.
      ── Failure ('test-load-data.R:58'): test that it is possible to load multiple LANS summaries ──
      `... <- NULL` produced warnings.
      ── Failure ('test-load-data.R:81'): test that it is possible to load LANS maps ──
      `... <- NULL` produced warnings.
      
      [ FAIL 4 | WARN 18 | SKIP 0 | PASS 140 ]
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
* Number of recursive dependencies: 93

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
     1. └─lillies::lyl_aggregated_range(...)
     2.   ├─dplyr::mutate(...)
     3.   ├─dplyr::left_join(LYL, ages_onset, by = "age", all.x = T)
     4.   └─dplyr:::left_join.data.frame(LYL, ages_onset, by = "age", all.x = T)
     5.     └─rlang::check_dots_empty0(...)
     6.       └─rlang::check_dots_empty(call = call)
     7.         └─rlang:::action_dots(...)
     8.           ├─base (local) try_dots(...)
     9.           └─rlang (local) action(...)
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘ddpcr’
      All declared Imports should be used.
    ```

# logitr

<details>

* Version: 0.8.0
* GitHub: https://github.com/jhelvy/logitr
* Source code: https://github.com/cran/logitr
* Date/Publication: 2022-10-03 20:30:02 UTC
* Number of recursive dependencies: 147

Run `cloud_details(, "logitr")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘basic_usage.Rmd’ using rmarkdown
    --- finished re-building ‘basic_usage.Rmd’
    
    --- re-building ‘benchmark.Rmd’ using rmarkdown
    --- finished re-building ‘benchmark.Rmd’
    
    --- re-building ‘convergence.Rmd’ using rmarkdown
    --- finished re-building ‘convergence.Rmd’
    
    ...
    --- failed re-building ‘summarizing_results.Rmd’
    
    --- re-building ‘utility_models.Rmd’ using rmarkdown
    --- finished re-building ‘utility_models.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘summarizing_results.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# MBNMAtime

<details>

* Version: 0.2.1
* GitHub: NA
* Source code: https://github.com/cran/MBNMAtime
* Date/Publication: 2021-09-13 15:10:02 UTC
* Number of recursive dependencies: 111

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
      ── Failure ('test_plot.functions.R:350'): timeplot functions correctly ─────────
      `timeplot(painnet, plotby = "rel")` produced warnings.
      ── Failure ('test_plot.functions.R:357'): timeplot functions correctly ─────────
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

# MetAlyzer

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/MetAlyzer
* Date/Publication: 2022-02-01 09:10:08 UTC
* Number of recursive dependencies: 100

Run `cloud_details(, "MetAlyzer")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘MetAlyzer_User_Guide.Rmd’ using rmarkdown
    Quitting from lines 166-169 (MetAlyzer_User_Guide.Rmd) 
    Error: processing vignette 'MetAlyzer_User_Guide.Rmd' failed with diagnostics:
    ℹ In argument: `ANOVA_group = calc_anova(get(categorical),
      .data$transf_Conc, .data$valid_replicates)`.
    ℹ In group 1: `Method = 1`, `Tissue = Drosophila`, `Metabolite = C0`.
    Caused by error in `contrasts<-`:
    ! contrasts can be applied only to factors with 2 or more levels
    --- failed re-building ‘MetAlyzer_User_Guide.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘MetAlyzer_User_Guide.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# mudata2

<details>

* Version: 1.1.2
* GitHub: https://github.com/paleolimbot/mudata2
* Source code: https://github.com/cran/mudata2
* Date/Publication: 2020-03-20 20:20:03 UTC
* Number of recursive dependencies: 100

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
      4. │     └─mudata2::update_columns_table(md, quiet = FALSE)
      5. │       └─md$columns %>% ...
      6. ├─dplyr::left_join(...)
      7. └─dplyr:::left_join.data.frame(...)
      8.   └─rlang::check_dots_empty0(...)
      9.     └─rlang::check_dots_empty(call = call)
     10.       └─rlang:::action_dots(...)
     11.         ├─base (local) try_dots(...)
     12.         └─rlang (local) action(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘test-all.R’
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
        2. │ └─mudata2:::write_mudata_json_common(...)
        3. │   └─mudata2:::write_mudata_common(...)
        4. │     └─mudata2::update_columns_table(md, quiet = FALSE)
        5. │       └─md$columns %>% ...
        6. ├─dplyr::left_join(...)
        7. └─dplyr:::left_join.data.frame(...)
        8.   └─rlang::check_dots_empty0(...)
        9.     └─rlang::check_dots_empty(call = call)
       10.       └─rlang:::action_dots(...)
       11.         ├─base (local) try_dots(...)
       12.         └─rlang (local) action(...)
      
      [ FAIL 19 | WARN 2 | SKIP 1 | PASS 548 ]
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
    `...` must be empty.
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
      ── Failure ('test-multicolor.R:89'): colors(), including grays, rainbow, and rbg work ──
      `suppressMessages(...)` produced warnings.
      ── Failure ('test-multicolor.R:103'): colors(), including grays, rainbow, and rbg work ──
      `suppressMessages(...)` produced warnings.
      ── Failure ('test-multicolor.R:113'): colors(), including grays, rainbow, and rbg work ──
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
* Number of recursive dependencies: 177

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

# ndi

<details>

* Version: 0.1.3
* GitHub: https://github.com/idblr/ndi
* Source code: https://github.com/cran/ndi
* Date/Publication: 2022-12-01 15:20:02 UTC
* Number of recursive dependencies: 110

Run `cloud_details(, "ndi")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ndi-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: powell_wiley
    > ### Title: Neighborhood Deprivation Index based on Andrews et al. (2020)
    > ###   and Slotman et al. (2022)
    > ### Aliases: powell_wiley
    > 
    > ### ** Examples
    > 
    ...
        ▆
     1. └─ndi::powell_wiley(df = DCtracts2020[, -c(3:10)])
     2.   ├─dplyr::left_join(...)
     3.   └─dplyr:::left_join.data.frame(...)
     4.     └─rlang::check_dots_empty0(...)
     5.       └─rlang::check_dots_empty(call = call)
     6.         └─rlang:::action_dots(...)
     7.           ├─base (local) try_dots(...)
     8.           └─rlang (local) action(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        3. │   ├─testthat (local) .capture(...)
        4. │   │ └─base::withCallingHandlers(...)
        5. │   └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
        6. └─ndi::powell_wiley(df = DCtracts2020[, -c(3:10)])
        7.   ├─dplyr::left_join(...)
        8.   └─dplyr:::left_join.data.frame(...)
        9.     └─rlang::check_dots_empty0(...)
       10.       └─rlang::check_dots_empty(call = call)
       11.         └─rlang:::action_dots(...)
       12.           ├─base (local) try_dots(...)
       13.           └─rlang (local) action(...)
      
      [ FAIL 1 | WARN 0 | SKIP 13 | PASS 22 ]
      Error: Test failures
      Execution halted
    ```

# NobBS

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/NobBS
* Date/Publication: 2020-03-03 10:40:02 UTC
* Number of recursive dependencies: 19

Run `cloud_details(, "NobBS")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘NobBS-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: NobBS
    > ### Title: Produce smooth Bayesian nowcasts of incomplete, time-stamped
    > ###   reporting data.
    > ### Aliases: NobBS
    > 
    > ### ** Examples
    > 
    ...
     20. │                 │   │ └─base (local) tryCatchList(expr, classes, parentenv, handlers)
     21. │                 │   │   └─base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
     22. │                 │   │     └─base (local) doTryCatch(return(expr), name, parentenv, handler)
     23. │                 │   └─base::withCallingHandlers(...)
     24. │                 └─rlang::eval_tidy(as_quosure(expr, env), context_mask)
     25. └─dplyr::select_vars(...)
     26.   └─lifecycle::deprecate_stop("0.8.4", "select_vars()", "tidyselect::vars_select()")
     27.     └─lifecycle:::deprecate_stop0(msg)
     28.       └─rlang::cnd_signal(...)
    Execution halted
    ```

# nonmemica

<details>

* Version: 0.9.9
* GitHub: NA
* Source code: https://github.com/cran/nonmemica
* Date/Publication: 2022-10-01 07:00:02 UTC
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

# ontologics

<details>

* Version: 0.5.2
* GitHub: https://github.com/luckinet/ontologics
* Source code: https://github.com/cran/ontologics
* Date/Publication: 2022-10-19 18:32:36 UTC
* Number of recursive dependencies: 71

Run `cloud_details(, "ontologics")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ontologics-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: new_mapping
    > ### Title: Add a new mapping to an ontology
    > ### Aliases: new_mapping
    > 
    > ### ** Examples
    > 
    > ontoDir <- system.file("extdata", "crops.rds", package = "ontologics")
    ...
      9.   └─vctrs::vec_default_cast(...)
     10.     ├─base::withRestarts(...)
     11.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     12.     │   └─base (local) doWithOneRestart(return(expr), restart)
     13.     └─vctrs::stop_incompatible_cast(...)
     14.       └─vctrs::stop_incompatible_type(...)
     15.         └─vctrs:::stop_incompatible(...)
     16.           └─vctrs:::stop_vctrs(...)
     17.             └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘conversion_to_rdf.Rmd’ using rmarkdown
    Quitting from lines 30-34 (conversion_to_rdf.Rmd) 
    Error: processing vignette 'conversion_to_rdf.Rmd' failed with diagnostics:
    Can't convert `y` <character> to match type of `x` <tbl_df>.
    --- failed re-building ‘conversion_to_rdf.Rmd’
    
    --- re-building ‘create_an_ontology.Rmd’ using rmarkdown
    --- finished re-building ‘create_an_ontology.Rmd’
    ...
    --- failed re-building ‘map_new_concepts.Rmd’
    
    --- re-building ‘ontology_database_description.Rmd’ using rmarkdown
    --- finished re-building ‘ontology_database_description.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘conversion_to_rdf.Rmd’ ‘map_new_concepts.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# overviewR

<details>

* Version: 0.0.11
* GitHub: https://github.com/cosimameyer/overviewR
* Source code: https://github.com/cran/overviewR
* Date/Publication: 2022-08-06 08:50:02 UTC
* Number of recursive dependencies: 123

Run `cloud_details(, "overviewR")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      • all = TRUE
      Backtrace:
          ▆
       1. └─overviewR::overview_overlap(...) at test-check_output.R:320:2
       2.   ├─dplyr::full_join(...)
       3.   └─dplyr:::full_join.data.frame(...)
       4.     └─rlang::check_dots_empty0(...)
       5.       └─rlang::check_dots_empty(call = call)
       6.         └─rlang:::action_dots(...)
       7.           ├─base (local) try_dots(...)
       8.           └─rlang (local) action(...)
      
      [ FAIL 1 | WARN 1 | SKIP 4 | PASS 52 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘getting-started.Rmd’ using rmarkdown
    --- finished re-building ‘getting-started.Rmd’
    
    --- re-building ‘overviewR_vignette.Rmd’ using rmarkdown
    Quitting from lines 446-458 (overviewR_vignette.Rmd) 
    Error: processing vignette 'overviewR_vignette.Rmd' failed with diagnostics:
    `...` must be empty.
    ✖ Problematic arguments:
    ...
    • by.x = !!dat1_id
    • by.y = !!dat2_id
    • all = TRUE
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
* Number of recursive dependencies: 169

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

# PHEindicatormethods

<details>

* Version: 1.4.2
* GitHub: https://github.com/PublicHealthEngland/PHEindicatormethods
* Source code: https://github.com/cran/PHEindicatormethods
* Date/Publication: 2022-12-01 00:10:06 UTC
* Number of recursive dependencies: 77

Run `cloud_details(, "PHEindicatormethods")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > test_check("PHEindicatormethods")
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 452 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('testLifeExpectancy.R:315'): LE - warnings are generated when invalid arguments are used ──
      wideci_warning\[2\] does not match "some life expectancy values have a 95% confidence interval > 20 years; these values have been suppressed to NAs".
      Actual value: "Each row in `x` is expected to match at most 1 row in `y`\.\\nℹ Row 1 of `x` matches multiple rows\.\\nℹ If multiple matches are expected, set `multiple = "all"` to silence this\\n  warning\."
      Backtrace:
          ▆
       1. └─testthat::expect_match(wideci_warning[2], "some life expectancy values have a 95% confidence interval > 20 years; these values have been suppressed to NAs") at testLifeExpectancy.R:315:2
       2.   └─testthat:::expect_match_(...)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 452 ]
      Error: Test failures
      Execution halted
    ```

# phenofit

<details>

* Version: 0.3.7
* GitHub: https://github.com/eco-hydro/phenofit
* Source code: https://github.com/cran/phenofit
* Date/Publication: 2022-11-07 11:40:02 UTC
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
    +     options = list(
    +         rFUN = "smooth_wWHIT", wFUN = wFUN,
    +         r_min = 0.05, ypeak_min = 0.05,
    +         lambda = 10,
    +         verbose = FALSE
    +     ))
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
       10. ├─phenofit (local) `<fn>`(INPUT = `<input>`, options = `<named list>`, rFUN = "smooth_wSG")
       11. │ ├─base::do.call(season_input, params_i)
       12. │ └─phenofit (local) `<fn>`(INPUT = `<named list>`, lambda = 10, rFUN = "smooth_wSG")
       13. │   └─phenofit::find_season.peaks(rfit, info_peak)
       14. │     └─phenofit:::get_A(ypred, na.rm = FALSE)
       15. │       ├─range(x, na.rm = na.rm) %>% diff()
       16. │       └─base::Summary.data.frame(`<dt[,8]>`, na.rm = FALSE)
       17. │         └─base::lapply(...)
       18. │           └─base (local) FUN(X[[i]], ...)
       19. └─base::diff(.)
      
      [ FAIL 9 | WARN 0 | SKIP 0 | PASS 45 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘phenofit-procedures.Rmd’ using rmarkdown
    Quitting from lines 71-110 (phenofit-procedures.Rmd) 
    Error: processing vignette 'phenofit-procedures.Rmd' failed with diagnostics:
    only defined on a data frame with all numeric-alike variables
    --- failed re-building ‘phenofit-procedures.Rmd’
    
    --- re-building ‘phenofit_CA-NS6.Rmd’ using rmarkdown
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
      installed size is  7.5Mb
      sub-directories of 1Mb or more:
        libs   6.3Mb
    ```

# PKNCA

<details>

* Version: 0.10.0
* GitHub: https://github.com/billdenney/pknca
* Source code: https://github.com/cran/PKNCA
* Date/Publication: 2022-10-16 03:35:14 UTC
* Number of recursive dependencies: 139

Run `cloud_details(, "PKNCA")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      [ FAIL 4 | WARN 0 | SKIP 0 | PASS 1657 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-dplyr.R:31'): dplyr left_join ────────────────────────────────
      `joined <- left_join(myresult, joindf)` did not throw the expected message.
      ── Failure ('test-dplyr.R:36'): dplyr left_join ────────────────────────────────
      `... <- NULL` did not throw the expected message.
      ── Failure ('test-dplyr.R:42'): dplyr left_join ────────────────────────────────
      `joined <- left_join(myconc, joindf)` did not throw the expected message.
      ── Failure ('test-dplyr.R:47'): dplyr left_join ────────────────────────────────
      `joined_manual$data <- left_join(joined_manual$data, joindf)` did not throw the expected message.
      
      [ FAIL 4 | WARN 0 | SKIP 0 | PASS 1657 ]
      Error: Test failures
      Execution halted
    ```

# presenter

<details>

* Version: 0.1.1
* GitHub: https://github.com/Harrison4192/presenter
* Source code: https://github.com/cran/presenter
* Date/Publication: 2021-11-18 06:20:05 UTC
* Number of recursive dependencies: 121

Run `cloud_details(, "presenter")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘exportToExcel.Rmd’ using rmarkdown
    --- finished re-building ‘exportToExcel.Rmd’
    
    --- re-building ‘flextableAndPowerpoint.Rmd’ using rmarkdown
    Quitting from lines 36-43 (flextableAndPowerpoint.Rmd) 
    Error: processing vignette 'flextableAndPowerpoint.Rmd' failed with diagnostics:
    Can't combine `x` <factor<23ef9>> and `left` <double>.
    --- failed re-building ‘flextableAndPowerpoint.Rmd’
    
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
* Number of recursive dependencies: 135

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
      
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 65 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 8504 marked UTF-8 strings
    ```

# r2dii.analysis

<details>

* Version: 0.2.1
* GitHub: https://github.com/RMI-PACTA/r2dii.analysis
* Source code: https://github.com/cran/r2dii.analysis
* Date/Publication: 2022-11-03 16:50:02 UTC
* Number of recursive dependencies: 76

Run `cloud_details(, "r2dii.analysis")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      ══ Skipped tests ═══════════════════════════════════════════════════════════════
      • On CRAN (4)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-target_market_share.R:923'): projects technology share as 'production / total production' when
                computing by company, unweighted by relative loan size (#288) ──
      out$technology_share (`actual`) not equal to out$production/sum(out$production) (`expected`).
      
        `actual`: 0.5 0.5
      `expected`: 0.1 0.9
      
      [ FAIL 1 | WARN 80 | SKIP 4 | PASS 268 ]
      Error: Test failures
      Execution halted
    ```

# rabhit

<details>

* Version: 0.2.4
* GitHub: NA
* Source code: https://github.com/cran/rabhit
* Date/Publication: 2022-09-22 15:10:02 UTC
* Number of recursive dependencies: 130

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
# recipes

<details>

* Version: 1.0.3
* GitHub: https://github.com/tidymodels/recipes
* Source code: https://github.com/cran/recipes
* Date/Publication: 2022-11-09 16:50:02 UTC
* Number of recursive dependencies: 134

Run `cloud_details(, "recipes")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
                         - "setosa    "     [1]             
                         - "setosa    "     [2]             
                         - "setosa    "     [3]             
                         - "setosa    "     [4]             
                         - "setosa    "     [5]             
                         - "setosa    "     [6]             
                         - "setosa    "     [7]             
                         - "setosa    "     [8]             
                         - "setosa    "     [9]             
                         - "setosa    "     [10]            
      ... ...              ...              and 140 more ...
      
      [ FAIL 3 | WARN 31 | SKIP 401 | PASS 1889 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘fastICA’, ‘dimRed’
    ```

# RNeXML

<details>

* Version: 2.4.8
* GitHub: https://github.com/ropensci/RNeXML
* Source code: https://github.com/cran/RNeXML
* Date/Publication: 2022-10-19 22:27:55 UTC
* Number of recursive dependencies: 139

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
      [ FAIL 1 | WARN 1 | SKIP 42 | PASS 302 ]
      
      ══ Skipped tests ═══════════════════════════════════════════════════════════════
      • On CRAN (42)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test_meta_extract.R:204'): ID assignments are correct and complete when meta are nested ──
      sort(meta.cont[, "Meta"]) not equal to sort(unique(meta.nested[, "meta"])).
      names for target but not for current
      
      [ FAIL 1 | WARN 1 | SKIP 42 | PASS 302 ]
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
      ── Failure ('test-conversion.R:22'): triple <-> tidy ───────────────────────────
      `triple` not equal to `brauer_2008_triple`.
      Component "features": Names: 3 string mismatches
      Component "features": Component 2: 500 string mismatches
      Component "features": Component 3: 476 string mismatches
      Component "features": Component 4: 500 string mismatches
      
      [ FAIL 1 | WARN 5 | SKIP 1 | PASS 15 ]
      Error: Test failures
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

# simplevis

<details>

* Version: 6.4.0
* GitHub: https://github.com/StatisticsNZ/simplevis
* Source code: https://github.com/cran/simplevis
* Date/Publication: 2022-08-05 14:00:02 UTC
* Number of recursive dependencies: 135

Run `cloud_details(, "simplevis")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘simplevis.Rmd’ using rmarkdown
    Quitting from lines 412-437 (simplevis.Rmd) 
    Error: processing vignette 'simplevis.Rmd' failed with diagnostics:
    'names' attribute [9] must be the same length as the vector [3]
    --- failed re-building ‘simplevis.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘simplevis.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# skater

<details>

* Version: 0.1.1
* GitHub: https://github.com/signaturescience/skater
* Source code: https://github.com/cran/skater
* Date/Publication: 2022-02-01 16:00:02 UTC
* Number of recursive dependencies: 96

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
     15.   └─vctrs::vec_default_cast(...)
     16.     ├─base::withRestarts(...)
     17.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     18.     │   └─base (local) doWithOneRestart(return(expr), restart)
     19.     └─vctrs::stop_incompatible_cast(...)
     20.       └─vctrs::stop_incompatible_type(...)
     21.         └─vctrs:::stop_incompatible(...)
     22.           └─vctrs:::stop_vctrs(...)
     23.             └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       21. │       └─vctrs:::stop_incompatible(...)
       22. │         └─vctrs:::stop_vctrs(...)
       23. │           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
       24. │             └─rlang:::signal_abort(cnd, .file)
       25. │               └─base::signalCondition(cnd)
       26. ├─dplyr (local) `<fn>`(`<vctrs_r_>`)
       27. │ └─rlang::abort(bullets, call = call(setup$across_if_fn), parent = cnd)
       28. │   └─rlang:::signal_abort(cnd, .file)
       29. │     └─base::signalCondition(cnd)
       30. └─dplyr (local) `<fn>`(`<rlng_rrr>`)
       31.   └─rlang::abort(message, class = error_class, parent = parent, call = error_call)
      
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
    ℹ In argument: `dplyr::across(c(dadid, momid), dplyr::na_if, 0)`.
    Caused by error in `across()`:
    ! Can't compute column `dadid`.
    Caused by error in `fn()`:
    ! Can't convert `y` <double> to match type of `x` <character>.
    --- failed re-building ‘basic_usage.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘basic_usage.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# skimr

<details>

* Version: 2.1.4
* GitHub: https://github.com/ropensci/skimr
* Source code: https://github.com/cran/skimr
* Date/Publication: 2022-04-15 02:20:02 UTC
* Number of recursive dependencies: 82

Run `cloud_details(, "skimr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ℹ In argument: `dplyr::across(variable_names, mangled_skimmers$funs)`.
      Caused by warning:
      ! Using an external vector in selections was deprecated in tidyselect 1.1.0.
      ℹ Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(variable_names)
      
        # Now:
        data %>% select(all_of(variable_names))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      
      [ FAIL 1 | WARN 5 | SKIP 25 | PASS 630 ]
      Error: Test failures
      Execution halted
    ```

# starschemar

<details>

* Version: 1.2.0
* GitHub: NA
* Source code: https://github.com/cran/starschemar
* Date/Publication: 2020-09-25 21:30:02 UTC
* Number of recursive dependencies: 66

Run `cloud_details(, "starschemar")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘starschemar-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: character_dimensions
    > ### Title: Transform dimension numeric attributes to character
    > ### Aliases: character_dimensions character_dimensions.star_schema
    > 
    > ### ** Examples
    > 
    > library(tidyr)
    ...
      9.       └─dplyr:::summarise.data.frame(.tbl, !!!funs)
     10.         └─dplyr:::compute_by(...)
     11.           └─dplyr:::eval_select_by(by, data, error_call = error_call)
     12.             └─tidyselect::eval_select(...)
     13.               └─tidyselect:::eval_select_impl(...)
     14.                 └─vctrs::vec_assert(x)
     15.                   └─vctrs:::stop_scalar_type(x, arg, call = call)
     16.                     └─vctrs:::stop_vctrs(...)
     17.                       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        4.     └─dplyr::summarise_all(mod_dim, class)
        5.       ├─dplyr::summarise(.tbl, !!!funs)
        6.       └─dplyr:::summarise.data.frame(.tbl, !!!funs)
        7.         └─dplyr:::compute_by(...)
        8.           └─dplyr:::eval_select_by(by, data, error_call = error_call)
        9.             └─tidyselect::eval_select(...)
       10.               └─tidyselect:::eval_select_impl(...)
       11.                 └─vctrs::vec_assert(x)
       12.                   └─vctrs:::stop_scalar_type(x, arg, call = call)
       13.                     └─vctrs:::stop_vctrs(...)
       14.                       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
      
      [ FAIL 11 | WARN 1 | SKIP 0 | PASS 149 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘starschemar.Rmd’ using rmarkdown
    Quitting from lines 354-363 (starschemar.Rmd) 
    Error: processing vignette 'starschemar.Rmd' failed with diagnostics:
    `x` must be a vector, not a <tbl_df/tbl/data.frame/dimension_table> object.
    --- failed re-building ‘starschemar.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘starschemar.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘pander’ ‘readr’ ‘tidyselect’
      All declared Imports should be used.
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
     14.   └─vctrs::vec_default_cast(...)
     15.     ├─base::withRestarts(...)
     16.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     17.     │   └─base (local) doWithOneRestart(return(expr), restart)
     18.     └─vctrs::stop_incompatible_cast(...)
     19.       └─vctrs::stop_incompatible_type(...)
     20.         └─vctrs:::stop_incompatible(...)
     21.           └─vctrs:::stop_vctrs(...)
     22.             └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
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
    ℹ In argument: `sg_change_proportion =
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
     10.   └─vctrs::vec_default_cast(...)
     11.     ├─base::withRestarts(...)
     12.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     13.     │   └─base (local) doWithOneRestart(return(expr), restart)
     14.     └─vctrs::stop_incompatible_cast(...)
     15.       └─vctrs::stop_incompatible_type(...)
     16.         └─vctrs:::stop_incompatible(...)
     17.           └─vctrs:::stop_vctrs(...)
     18.             └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       15. │ └─vctrs::vec_cast(x = y, to = x, x_arg = "y", to_arg = "x")
       16. └─vctrs (local) `<fn>`()
       17.   └─vctrs::vec_default_cast(...)
       18.     ├─base::withRestarts(...)
       19.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
       20.     │   └─base (local) doWithOneRestart(return(expr), restart)
       21.     └─vctrs::stop_incompatible_cast(...)
       22.       └─vctrs::stop_incompatible_type(...)
       23.         └─vctrs:::stop_incompatible(...)
       24.           └─vctrs:::stop_vctrs(...)
       25.             └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
      
      [ FAIL 16 | WARN 3 | SKIP 49 | PASS 18 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘SwimmeR.Rmd’ using rmarkdown
    Quitting from lines 60-66 (SwimmeR.Rmd) 
    Error: processing vignette 'SwimmeR.Rmd' failed with diagnostics:
    Can't convert `y` <character> to match type of `x` <data.frame>.
    --- failed re-building ‘SwimmeR.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘SwimmeR.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# tabshiftr

<details>

* Version: 0.4.0
* GitHub: https://github.com/luckinet/tabshiftr
* Source code: https://github.com/cran/tabshiftr
* Date/Publication: 2022-09-28 23:20:02 UTC
* Number of recursive dependencies: 75

Run `cloud_details(, "tabshiftr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       25. │       └─vctrs:::stop_incompatible(...)
       26. │         └─vctrs:::stop_vctrs(...)
       27. │           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
       28. │             └─rlang:::signal_abort(cnd, .file)
       29. │               └─base::signalCondition(cnd)
       30. ├─dplyr (local) `<fn>`(`<vctrs_r_>`)
       31. │ └─rlang::abort(msg, call = call("across"), parent = cnd)
       32. │   └─rlang:::signal_abort(cnd, .file)
       33. │     └─base::signalCondition(cnd)
       34. └─dplyr (local) `<fn>`(`<rlng_rrr>`)
       35.   └─rlang::abort(message, class = error_class, parent = parent, call = error_call)
      
      [ FAIL 2 | WARN 0 | SKIP 3 | PASS 340 ]
      Error: Test failures
      Execution halted
    ```

# textrecipes

<details>

* Version: 1.0.1
* GitHub: https://github.com/tidymodels/textrecipes
* Source code: https://github.com/cran/textrecipes
* Date/Publication: 2022-10-06 03:20:02 UTC
* Number of recursive dependencies: 124

Run `cloud_details(, "textrecipes")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ Skipped tests ═══════════════════════════════════════════════════════════════
      • On CRAN (88)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error ('test-hashing_dummy.R:42'): hashing collapsed multiple factors ───────
      <dplyr:::mutate_error/rlang_error/error/condition>
      Error in `dplyr::mutate(., `:=`(!!new_name, paste0(dplyr::c_across(dplyr::all_of(hash_cols)), 
          collapse = "")))`: i In argument: `contract_value_band_sponsor_code = paste0(dplyr::c_across(dplyr::all_of(hash_cols)), collapse = "")`.
      i In row 1.
      Caused by error in `dplyr::c_across()`:
      ! Can't rename variables in this context.
      
      [ FAIL 1 | WARN 0 | SKIP 88 | PASS 344 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 4 marked UTF-8 strings
    ```

# tfrmt

<details>

* Version: 0.0.1
* GitHub: https://github.com/GSK-Biostatistics/tfrmt
* Source code: https://github.com/cran/tfrmt
* Date/Publication: 2022-12-06 17:00:02 UTC
* Number of recursive dependencies: 90

Run `cloud_details(, "tfrmt")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       39. │                     └─vctrs:::stop_incompatible(...)
       40. │                       └─vctrs:::stop_vctrs(...)
       41. │                         └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
       42. │                           └─rlang:::signal_abort(cnd, .file)
       43. │                             └─base::signalCondition(cnd)
       44. ├─dplyr (local) `<fn>`(`<vctrs___>`)
       45. │ └─rlang::abort(bullets, call = call(setup$across_if_fn), parent = cnd)
       46. │   └─rlang:::signal_abort(cnd, .file)
       47. │     └─base::signalCondition(cnd)
       48. └─dplyr (local) `<fn>`(`<rlng_rrr>`)
       49.   └─rlang::abort(message, class = error_class, parent = parent, call = error_call)
      
      [ FAIL 32 | WARN 227 | SKIP 0 | PASS 405 ]
      Error: Test failures
      Execution halted
    ```

# tidyCDISC

<details>

* Version: 0.1.0
* GitHub: https://github.com/Biogen-Inc/tidyCDISC
* Source code: https://github.com/cran/tidyCDISC
* Date/Publication: 2022-08-30 13:30:02 UTC
* Number of recursive dependencies: 148

Run `cloud_details(, "tidyCDISC")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      x[2]: "START"
      y[2]: "DECODE"
      
      x[3]: "END"
      y[3]: "EVENTTYP"
      
      x[4]: "tab_st"
      y[4]: "DOMAIN"
      
      x[5]: "tab_en"
      y[5]: "END"
      
      [ FAIL 1 | WARN 8 | SKIP 15 | PASS 97 ]
      Error: Test failures
      Execution halted
    ```

# tidygraph

<details>

* Version: 1.2.2
* GitHub: https://github.com/thomasp85/tidygraph
* Source code: https://github.com/cran/tidygraph
* Date/Publication: 2022-08-22 07:20:02 UTC
* Number of recursive dependencies: 76

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
      ── Failure ('test-group.R:77'): grouping with fixed number of groups ───────────
      get_number_of_groups(gr, group_walktrap(n_groups = 7)) not equal to 7.
      Modes: list, numeric
      names for target but not for current
      Attributes: < Modes: list, NULL >
      Attributes: < Lengths: 2, 0 >
      Attributes: < names for target but not for current >
      Attributes: < current is not list-like >
      
      [ FAIL 4 | WARN 2 | SKIP 0 | PASS 276 ]
      Error: Test failures
      Execution halted
    ```

# tidyplus

<details>

* Version: 0.0.1
* GitHub: NA
* Source code: https://github.com/cran/tidyplus
* Date/Publication: 2022-08-29 09:20:02 UTC
* Number of recursive dependencies: 71

Run `cloud_details(, "tidyplus")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       23. │             └─rlang:::signal_abort(cnd, .file)
       24. │               └─base::signalCondition(cnd)
       25. ├─dplyr (local) `<fn>`(`<vctrs_r_>`)
       26. │ └─rlang::abort(msg, call = call("across"), parent = cnd)
       27. │   └─rlang:::signal_abort(cnd, .file)
       28. │     └─base::signalCondition(cnd)
       29. └─dplyr (local) `<fn>`(`<rlng_rrr>`)
       30.   └─rlang::abort(message, class = error_class, parent = parent, call = error_call)
      
      [ FAIL 1 | WARN 7 | SKIP 3 | PASS 125 ]
      Deleting unused snapshots:
      • replace-na-if/works.csv
      • unite-str/notremove.csv
      Error: Test failures
      Execution halted
    ```

# tidyquery

<details>

* Version: 0.2.3
* GitHub: https://github.com/ianmcook/tidyquery
* Source code: https://github.com/cran/tidyquery
* Date/Publication: 2021-12-02 20:10:02 UTC
* Number of recursive dependencies: 66

Run `cloud_details(, "tidyquery")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        4. ├─tidyquery::query("SELECT name, list_price FROM games g LEFT ANTI JOIN inventory i ON g.name = i.game")
        5. │ └─tidyquery:::query_(data, sql, TRUE)
        6. │   └─tidyquery:::join(tree)
        7. │     └─out$data %>% ...
        8. ├─dplyr (local) join_function(...)
        9. └─dplyr:::anti_join.data.frame(...)
       10.   └─rlang::check_dots_empty0(...)
       11.     └─rlang::check_dots_empty(call = call)
       12.       └─rlang:::action_dots(...)
       13.         ├─base (local) try_dots(...)
       14.         └─rlang (local) action(...)
      
      [ FAIL 2 | WARN 9 | SKIP 2 | PASS 217 ]
      Error: Test failures
      Execution halted
    ```

# tidytransit

<details>

* Version: 1.4
* GitHub: https://github.com/r-transit/tidytransit
* Source code: https://github.com/cran/tidytransit
* Date/Publication: 2022-08-26 08:00:02 UTC
* Number of recursive dependencies: 106

Run `cloud_details(, "tidytransit")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      ══ Skipped tests ═══════════════════════════════════════════════════════════════
      • On CRAN (7)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-read-gtfs.R:113'): files parameter ───────────────────────────
      `read_gtfs(path, files = f)` generated warnings:
      * Each row in `x` is expected to match at most 1 row in `y`.
      ℹ Row 6 of `x` matches multiple rows.
      ℹ If multiple matches are expected, set `multiple = "all"` to silence this
        warning.
      
      [ FAIL 1 | WARN 23 | SKIP 7 | PASS 219 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.0Mb
      sub-directories of 1Mb or more:
        doc       2.1Mb
        extdata   4.5Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘httr’
      All declared Imports should be used.
    ```

# tidytree

<details>

* Version: 0.4.1
* GitHub: https://github.com/YuLab-SMU/tidytree
* Source code: https://github.com/cran/tidytree
* Date/Publication: 2022-09-26 10:10:02 UTC
* Number of recursive dependencies: 61

Run `cloud_details(, "tidytree")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        1. ├─tree %<>% left_join(dat, by = "node") at test-dplyr-methods.R:125:4
        2. ├─dplyr::left_join(., dat, by = "node")
        3. ├─tidytree:::left_join.treedata(., dat, by = "node")
        4. │ └─dat %>% ...
        5. ├─dplyr::left_join(...)
        6. └─dplyr:::left_join.data.frame(., y, by = by, copy = copy, suffix = suffix, !!!dots)
        7.   └─rlang::check_dots_empty0(...)
        8.     └─rlang::check_dots_empty(call = call)
        9.       └─rlang:::action_dots(...)
       10.         ├─base (local) try_dots(...)
       11.         └─rlang (local) action(...)
      
      [ FAIL 2 | WARN 2 | SKIP 0 | PASS 62 ]
      Error: Test failures
      Execution halted
    ```

# topr

<details>

* Version: 1.1.1
* GitHub: https://github.com/GenuityScience/topr
* Source code: https://github.com/cran/topr
* Date/Publication: 2022-11-04 13:10:02 UTC
* Number of recursive dependencies: 77

Run `cloud_details(, "topr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘topr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: annotate_with_nearest_gene
    > ### Title: Get the nearest gene for one or more snps
    > ### Aliases: annotate_with_nearest_gene
    > 
    > ### ** Examples
    > 
    > variants <-get_lead_snps(CD_UKBB)
    ...
      8. ├─dplyr::rename_with(., ~"REF", matches(c("^ref$"), ignore.case = TRUE))
      9. ├─dplyr::rename_with(., ~"BETA", matches(c("^beta$"), ignore.case = TRUE))
     10. ├─dplyr::rename_with(...)
     11. ├─dplyr::rename_with(...)
     12. ├─dplyr::rename_with(...)
     13. ├─dplyr::rename_with(...)
     14. └─dplyr:::rename_with.data.frame(., ~"ID", matches(c("^rsid$", "^rsname$", "^snp$"), ignore.case = TRUE))
     15.   └─cli::cli_abort("{.arg .fn} must return a vector of length {length(sel)}, not {length(new)}.")
     16.     └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        6. ├─topr:::dat_chr_check(.)
        7. ├─dplyr::rename_with(., ~"ALT", matches(c("^alt$"), ignore.case = TRUE))
        8. ├─dplyr::rename_with(., ~"REF", matches(c("^ref$"), ignore.case = TRUE))
        9. ├─dplyr::rename_with(., ~"BETA", matches(c("^beta$"), ignore.case = TRUE))
       10. ├─dplyr::rename_with(...)
       11. ├─dplyr::rename_with(...)
       12. ├─dplyr::rename_with(...)
       13. ├─dplyr::rename_with(...)
       14. └─dplyr:::rename_with.data.frame(., ~"ID", matches(c("^rsid$", "^rsname$", "^snp$"), ignore.case = TRUE))
       15.   └─cli::cli_abort("{.arg .fn} must return a vector of length {length(sel)}, not {length(new)}.")
       16.     └─rlang::abort(...)
      
      [ FAIL 6 | WARN 0 | SKIP 4 | PASS 5 ]
      Error: Test failures
      Execution halted
    ```

# track2KBA

<details>

* Version: 1.0.4
* GitHub: https://github.com/BirdLifeInternational/track2kba
* Source code: https://github.com/cran/track2KBA
* Date/Publication: 2022-11-18 23:50:02 UTC
* Number of recursive dependencies: 100

Run `cloud_details(, "track2KBA")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘track2KBA-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: mapTrips
    > ### Title: Make simple maps of foraging trips
    > ### Aliases: mapTrips
    > 
    > ### ** Examples
    > 
    > ## make some play data
    ...
      7. │   └─dplyr:::mutate_col(dots[[i]], data, mask, new_columns)
      8. │     └─mask$eval_all_mutate(quo)
      9. │       └─dplyr (local) eval()
     10. ├─base::factor(x = .data$tripID, labels = seq_len(length.out = n_distinct(x = .data$tripID)))
     11. └─dplyr::n_distinct(x = .data$tripID)
     12.   └─rlang::check_dots_unnamed()
     13.     └─rlang:::action_dots(...)
     14.       ├─base (local) try_dots(...)
     15.       └─rlang (local) action(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘tinytest.R’
    Running the tests in ‘tests/tinytest.R’ failed.
    Last 13 lines of output:
       diff| 'In argument: `colID = as.character(x = factor(x = .data$tripID, labels = seq_len(length.out = n_distinct(x = .data$tripID))))`.'
      ----- FAILED[xcpt]: test_mapTrips.R<23--23>
       call| expect_silent(mapTrips(trips, colony, IDs = 1))
       diff| Execution was not silent. An error was thrown with message
       diff| 'In argument: `colID = as.character(x = factor(x = .data$tripID, labels = seq_len(length.out = n_distinct(x = .data$tripID))))`.'
      ----- FAILED[xcpt]: test_mapTrips.R<39--39>
       call| expect_silent(mapTrips(trips_idl, colony_idl))
       diff| Execution was not silent. An error was thrown with message
       diff| 'In argument: `colID = as.character(x = factor(x = .data$tripID, labels = seq_len(length.out = n_distinct(x = .data$tripID))))`.'
      Error: 3 out of 128 tests failed
      In addition: Warning message:
      In Matching::ks.boot(WI, BW, alternative = "two.sided", nboots = iterations) :
        For publication quality p-values it is recommended that 'nboots'
       be set equal to at least 500 (preferably 1000)
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
     1. ├─unpivotr::behead(cells, "N", foo)
     2. ├─unpivotr:::behead.data.frame(cells, "N", foo)
     3. │ └─unpivotr:::behead_if.data.frame(...)
     4. │   └─unpivotr:::direction_filter(direction)
     5. │     └─dplyr::case_when(...)
     6. │       └─vctrs::vec_size_common(...)
     7. └─vctrs:::stop_scalar_type(...)
     8.   └─vctrs:::stop_vctrs(...)
     9.     └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        1. ├─as_cells(BOD, FALSE, TRUE) %>% behead("N", "header") %>% ... at test-spatter.R:7:0
        2. ├─dplyr::select(., -col, -chr)
        3. ├─unpivotr::behead(., "N", "header")
        4. ├─unpivotr:::behead.data.frame(., "N", "header")
        5. │ └─unpivotr:::behead_if.data.frame(...)
        6. │   └─unpivotr:::direction_filter(direction)
        7. │     └─dplyr::case_when(...)
        8. │       └─vctrs::vec_size_common(...)
        9. └─vctrs:::stop_scalar_type(...)
       10.   └─vctrs:::stop_vctrs(...)
       11.     └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
      
      [ FAIL 13 | WARN 2 | SKIP 0 | PASS 153 ]
      Error: Test failures
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

# wcep

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/wcep
* Date/Publication: 2020-11-13 11:40:02 UTC
* Number of recursive dependencies: 54

Run `cloud_details(, "wcep")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘wcep-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: wcep
    > ### Title: Analysis of weighted composite endpoints
    > ### Aliases: wcep
    > 
    > ### ** Examples
    > 
    > data(toyexample)
    ...
     1. └─wcep::wcep(toyexample, EW)
     2.   └─wcep:::wcep_core(x[, 1:3], EW, alpha)
     3.     ├─dplyr::left_join(ew_h0, ew1, by = "event", stringsAsFactors = FALSE)
     4.     └─dplyr:::left_join.data.frame(ew_h0, ew1, by = "event", stringsAsFactors = FALSE)
     5.       └─rlang::check_dots_empty0(...)
     6.         └─rlang::check_dots_empty(call = call)
     7.           └─rlang:::action_dots(...)
     8.             ├─base (local) try_dots(...)
     9.             └─rlang (local) action(...)
    Execution halted
    ```

# wrangle

<details>

* Version: 0.5.7
* GitHub: NA
* Source code: https://github.com/cran/wrangle
* Date/Publication: 2022-01-03 18:20:02 UTC
* Number of recursive dependencies: 20

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

