# arrow

<details>

* Version: 10.0.1
* GitHub: https://github.com/apache/arrow
* Source code: https://github.com/cran/arrow
* Date/Publication: 2022-12-06 13:40:02 UTC
* Number of recursive dependencies: 75

Run `revdepcheck::cloud_details(, "arrow")` for more info

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
      
      [ FAIL 13 | WARN 16 | SKIP 71 | PASS 8541 ]
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

# autoGO

<details>

* Version: 0.9
* GitHub: NA
* Source code: https://github.com/cran/autoGO
* Date/Publication: 2023-01-16 10:10:05 UTC
* Number of recursive dependencies: 150

Run `revdepcheck::cloud_details(, "autoGO")` for more info

</details>

## Newly broken

*   checking whether package ‘autoGO’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ape::where’ by ‘dplyr::where’ when loading ‘autoGO’
    See ‘/tmp/workdir/autoGO/new/autoGO.Rcheck/00install.out’ for details.
    ```

# chronicler

<details>

* Version: 0.2.0
* GitHub: https://github.com/b-rodrigues/chronicler
* Source code: https://github.com/cran/chronicler
* Date/Publication: 2022-05-17 09:40:04 UTC
* Number of recursive dependencies: 59

Run `revdepcheck::cloud_details(, "chronicler")` for more info

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

Run `revdepcheck::cloud_details(, "circumplex")` for more info

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
      installed size is  5.5Mb
      sub-directories of 1Mb or more:
        doc    1.8Mb
        libs   3.4Mb
    ```

# confoundr

<details>

* Version: 1.2
* GitHub: https://github.com/jwjackson/confoundr
* Source code: https://github.com/cran/confoundr
* Date/Publication: 2019-09-20 04:40:02 UTC
* Number of recursive dependencies: 83

Run `revdepcheck::cloud_details(, "confoundr")` for more info

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

# disk.frame

<details>

* Version: 0.7.2
* GitHub: https://github.com/DiskFrame/disk.frame
* Source code: https://github.com/cran/disk.frame
* Date/Publication: 2022-03-07 11:40:02 UTC
* Number of recursive dependencies: 103

Run `revdepcheck::cloud_details(, "disk.frame")` for more info

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

# dm

<details>

* Version: 1.0.3
* GitHub: https://github.com/cynkra/dm
* Source code: https://github.com/cran/dm
* Date/Publication: 2022-10-12 15:42:33 UTC
* Number of recursive dependencies: 153

Run `revdepcheck::cloud_details(, "dm")` for more info

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

# dtplyr

<details>

* Version: 1.2.2
* GitHub: https://github.com/tidyverse/dtplyr
* Source code: https://github.com/cran/dtplyr
* Date/Publication: 2022-08-20 13:20:02 UTC
* Number of recursive dependencies: 66

Run `revdepcheck::cloud_details(, "dtplyr")` for more info

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
      
      [ FAIL 1 | WARN 1 | SKIP 25 | PASS 603 ]
      Error: Test failures
      Execution halted
    ```

# extdplyr

<details>

* Version: 0.1.5
* GitHub: NA
* Source code: https://github.com/cran/extdplyr
* Date/Publication: 2020-04-20 05:20:02 UTC
* Number of recursive dependencies: 41

Run `revdepcheck::cloud_details(, "extdplyr")` for more info

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

Run `revdepcheck::cloud_details(, "exuber")` for more info

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
      
      [ FAIL 42 | WARN 57 | SKIP 4 | PASS 194 ]
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

Run `revdepcheck::cloud_details(, "fastqcr")` for more info

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

# forceR

<details>

* Version: 1.0.15
* GitHub: https://github.com/Peter-T-Ruehr/forceR
* Source code: https://github.com/cran/forceR
* Date/Publication: 2022-06-07 14:50:02 UTC
* Number of recursive dependencies: 86

Run `revdepcheck::cloud_details(, "forceR")` for more info

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

Run `revdepcheck::cloud_details(, "forestmangr")` for more info

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

# ggmuller

<details>

* Version: 0.5.4
* GitHub: NA
* Source code: https://github.com/cran/ggmuller
* Date/Publication: 2019-09-05 02:10:17 UTC
* Number of recursive dependencies: 56

Run `revdepcheck::cloud_details(, "ggmuller")` for more info

</details>

## Newly broken

*   checking whether package ‘ggmuller’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ape::where’ by ‘dplyr::where’ when loading ‘ggmuller’
    See ‘/tmp/workdir/ggmuller/new/ggmuller.Rcheck/00install.out’ for details.
    ```

# gratia

<details>

* Version: 0.7.3
* GitHub: https://github.com/gavinsimpson/gratia
* Source code: https://github.com/cran/gratia
* Date/Publication: 2022-05-09 11:20:03 UTC
* Number of recursive dependencies: 84

Run `revdepcheck::cloud_details(, "gratia")` for more info

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

Run `revdepcheck::cloud_details(, "groupr")` for more info

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
* Number of recursive dependencies: 92

Run `revdepcheck::cloud_details(, "hablar")` for more info

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
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 418 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test.ifs.R:53'): if_else_ ─────────────────────────────────────────
      `if_else_(c(T, F, NA), 1, 1L)` did not throw an error.
      
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 418 ]
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

Run `revdepcheck::cloud_details(, "heemod")` for more info

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

Run `revdepcheck::cloud_details(, "helda")` for more info

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

* Version: 0.1.6
* GitHub: https://github.com/LarsenLab/hlaR
* Source code: https://github.com/cran/hlaR
* Date/Publication: 2022-12-20 23:30:02 UTC
* Number of recursive dependencies: 151

Run `revdepcheck::cloud_details(, "hlaR")` for more info

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

# huxtable

<details>

* Version: 5.5.2
* GitHub: https://github.com/hughjonesd/huxtable
* Source code: https://github.com/cran/huxtable
* Date/Publication: 2022-12-16 13:30:02 UTC
* Number of recursive dependencies: 167

Run `revdepcheck::cloud_details(, "huxtable")` for more info

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
        1. ├─testthat::expect_equivalent(...) at test-mapping-functions.R:127:2
        2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
        3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
        4. ├─huxtable (local) f(m, 1:3, 1:2, ct)
        5. │ └─dplyr::case_when(!!!cases)
        6. │   └─vctrs::vec_size_common(...)
        7. └─vctrs::stop_incompatible_size(...)
        8.   └─vctrs:::stop_incompatible(...)
        9.     └─vctrs:::stop_vctrs(...)
       10.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
      
      [ FAIL 2 | WARN 4 | SKIP 25 | PASS 1230 ]
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

# injurytools

<details>

* Version: 1.0.0
* GitHub: https://github.com/lzumeta/injurytools
* Source code: https://github.com/cran/injurytools
* Date/Publication: 2023-01-26 09:40:06 UTC
* Number of recursive dependencies: 103

Run `revdepcheck::cloud_details(, "injurytools")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── Failure ('test-prepare_data.R:87'): prepare_all works fine when dates in injury and exposure data do not match ──
      `w` has length 2, not length 1.
      ── Failure ('test-prepare_data.R:88'): prepare_all works fine when dates in injury and exposure data do not match ──
      `w` does not match "Injury data has been cut".
      Actual values:
      * Injury data has been cut to the given follow-up period \(in exposure data\)
      * Each row in `x` is expected to match at most 1 row in `y`\.\\ni Row 1 of `x` matches multiple rows\.\\ni If multiple matches are expected, set `multiple = "all"` to silence this warning\.
      Backtrace:
          ▆
       1. └─testthat::expect_match(w, regexp = "Injury data has been cut") at test-prepare_data.R:88:2
       2.   └─testthat:::expect_match_(...)
      
      [ FAIL 2 | WARN 1 | SKIP 0 | PASS 96 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 84 marked UTF-8 strings
    ```

# iNZightTools

<details>

* Version: 1.12.3
* GitHub: https://github.com/iNZightVIT/iNZightTools
* Source code: https://github.com/cran/iNZightTools
* Date/Publication: 2022-08-22 20:20:02 UTC
* Number of recursive dependencies: 87

Run `revdepcheck::cloud_details(, "iNZightTools")` for more info

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

Run `revdepcheck::cloud_details(, "IPEDSuploadables")` for more info

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
* Number of recursive dependencies: 74

Run `revdepcheck::cloud_details(, "janitor")` for more info

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

Run `revdepcheck::cloud_details(, "JumpeR")` for more info

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
* Number of recursive dependencies: 49

Run `revdepcheck::cloud_details(, "kmscv")` for more info

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
      5.         ├─purrr:::with_indexed_errors(...)
      6.         │ └─base::withCallingHandlers(...)
      7.         ├─purrr:::call_with_cleanup(...)
      8.         └─kmscv (local) .f(.x[[i]], .y[[i]], ...)
      9.           ├─dplyr::slice_sample(.x, n = .y)
     10.           └─dplyr:::slice_sample.data.frame(.x, n = .y)
     11.             └─dplyr:::get_slice_size(n = n, prop = prop, allow_outsize = replace)
     12.               └─dplyr:::check_slice_n_prop(n, prop, error_call = error_call)
     13.                 └─rlang::abort(...)
    Execution halted
    ```

# lans2r

<details>

* Version: 1.1.0
* GitHub: https://github.com/KopfLab/lans2r
* Source code: https://github.com/cran/lans2r
* Date/Publication: 2020-06-24 05:20:03 UTC
* Number of recursive dependencies: 85

Run `revdepcheck::cloud_details(, "lans2r")` for more info

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

# LARGB

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/LARGB
* Date/Publication: 2021-09-28 09:00:05 UTC
* Number of recursive dependencies: 31

Run `revdepcheck::cloud_details(, "LARGB")` for more info

</details>

## Newly broken

*   checking whether package ‘LARGB’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘dplyr::where’ by ‘imager::where’ when loading ‘LARGB’
    See ‘/tmp/workdir/LARGB/new/LARGB.Rcheck/00install.out’ for details.
    ```

# lillies

<details>

* Version: 0.2.9
* GitHub: NA
* Source code: https://github.com/cran/lillies
* Date/Publication: 2021-02-16 17:10:05 UTC
* Number of recursive dependencies: 95

Run `revdepcheck::cloud_details(, "lillies")` for more info

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

# MBNMAtime

<details>

* Version: 0.2.1
* GitHub: NA
* Source code: https://github.com/cran/MBNMAtime
* Date/Publication: 2021-09-13 15:10:02 UTC
* Number of recursive dependencies: 113

Run `revdepcheck::cloud_details(, "MBNMAtime")` for more info

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

# metacore

<details>

* Version: 0.1.1
* GitHub: https://github.com/atorus-research/metacore
* Source code: https://github.com/cran/metacore
* Date/Publication: 2022-12-07 14:10:06 UTC
* Number of recursive dependencies: 70

Run `revdepcheck::cloud_details(, "metacore")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘metacore-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: check_inconsistent_labels
    > ### Title: Optional checks to consistency of metadata
    > ### Aliases: check_inconsistent_labels check_inconsistent_types
    > ###   check_inconsistent_formats
    > 
    > ### ** Examples
    > 
    ...
      8. ├─dplyr:::mutate.data.frame(...)
      9. │ └─dplyr:::mutate_cols(.data, dplyr_quosures(...), by)
     10. │   ├─base::withCallingHandlers(...)
     11. │   └─dplyr:::mutate_col(dots[[i]], data, mask, new_columns)
     12. │     ├─base::withCallingHandlers(...)
     13. │     └─mask$eval_all_mutate(quo)
     14. │       └─dplyr (local) eval()
     15. └─dplyr:::dplyr_internal_error("dplyr:::mutate_incompatible_size", `<named list>`)
     16.   └─rlang::abort(class = c(class, "dplyr:::internal_error"), dplyr_error_data = data)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       14. │       └─dplyr (local) eval()
       15. ├─dplyr:::dplyr_internal_error(...)
       16. │ └─rlang::abort(class = c(class, "dplyr:::internal_error"), dplyr_error_data = data)
       17. │   └─rlang:::signal_abort(cnd, .file)
       18. │     └─base::signalCondition(cnd)
       19. ├─dplyr (local) `<fn>`(`<dpl:::__>`)
       20. │ └─rlang::abort(msg, call = call("across"), parent = cnd)
       21. │   └─rlang:::signal_abort(cnd, .file)
       22. │     └─base::signalCondition(cnd)
       23. └─dplyr (local) `<fn>`(`<rlng_rrr>`)
       24.   └─rlang::abort(message, class = error_class, parent = parent, call = error_call)
      
      [ FAIL 2 | WARN 1 | SKIP 0 | PASS 67 ]
      Error: Test failures
      Execution halted
    ```

# multicolor

<details>

* Version: 0.1.6
* GitHub: https://github.com/aedobbyn/multicolor
* Source code: https://github.com/cran/multicolor
* Date/Publication: 2023-01-05 19:30:02 UTC
* Number of recursive dependencies: 68

Run `revdepcheck::cloud_details(, "multicolor")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Colors cannot be applied in this environment. Please use another application, such as RStudio or a color-enabled terminal.
      > 
      > test_check("multicolor")
      [ FAIL 1 | WARN 1 | SKIP 1 | PASS 29 ]
      
      ══ Skipped tests ═══════════════════════════════════════════════════════════════
      • use_color() is not TRUE (1)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-multicolor.R:103'): colors(), including grays, rainbow, and rbg work ──
      `suppressMessages(...)` produced warnings.
      
      [ FAIL 1 | WARN 1 | SKIP 1 | PASS 29 ]
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
* Number of recursive dependencies: 189

Run `revdepcheck::cloud_details(, "naniar")` for more info

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

Run `revdepcheck::cloud_details(, "ndi")` for more info

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

Run `revdepcheck::cloud_details(, "NobBS")` for more info

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

# ontologics

<details>

* Version: 0.6.4
* GitHub: https://github.com/luckinet/ontologics
* Source code: https://github.com/cran/ontologics
* Date/Publication: 2023-01-24 14:20:02 UTC
* Number of recursive dependencies: 78

Run `revdepcheck::cloud_details(, "ontologics")` for more info

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

# openalexR

<details>

* Version: 1.0.0
* GitHub: https://github.com/massimoaria/openalexR
* Source code: https://github.com/cran/openalexR
* Date/Publication: 2022-10-06 10:40:02 UTC
* Number of recursive dependencies: 78

Run `revdepcheck::cloud_details(, "openalexR")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘A_Brief_Introduction_to_openalexR.Rmd’ using rmarkdown
    Quitting from lines 342-354 (A_Brief_Introduction_to_openalexR.Rmd) 
    Error: processing vignette 'A_Brief_Introduction_to_openalexR.Rmd' failed with diagnostics:
    missing value where TRUE/FALSE needed
    --- failed re-building ‘A_Brief_Introduction_to_openalexR.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘A_Brief_Introduction_to_openalexR.Rmd’
    
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

Run `revdepcheck::cloud_details(, "overviewR")` for more info

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

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘getting-started.Rmd’ using rmarkdown
    --- finished re-building ‘getting-started.Rmd’
    
    --- re-building ‘overviewR_vignette.Rmd’ using rmarkdown
    Quitting from lines 19-33 (overviewR_vignette.Rmd) 
    Error: processing vignette 'overviewR_vignette.Rmd' failed with diagnostics:
    trying to use CRAN without setting a mirror
    --- failed re-building ‘overviewR_vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘overviewR_vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘xtable’
    ```

# PHEindicatormethods

<details>

* Version: 1.4.2
* GitHub: https://github.com/PublicHealthEngland/PHEindicatormethods
* Source code: https://github.com/cran/PHEindicatormethods
* Date/Publication: 2022-12-01 00:10:06 UTC
* Number of recursive dependencies: 63

Run `revdepcheck::cloud_details(, "PHEindicatormethods")` for more info

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
* Number of recursive dependencies: 84

Run `revdepcheck::cloud_details(, "phenofit")` for more info

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
      installed size is  7.4Mb
      sub-directories of 1Mb or more:
        libs   6.2Mb
    ```

# presenter

<details>

* Version: 0.1.1
* GitHub: https://github.com/Harrison4192/presenter
* Source code: https://github.com/cran/presenter
* Date/Publication: 2021-11-18 06:20:05 UTC
* Number of recursive dependencies: 134

Run `revdepcheck::cloud_details(, "presenter")` for more info

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
* Number of recursive dependencies: 92

Run `revdepcheck::cloud_details(, "PVplr")` for more info

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

Run `revdepcheck::cloud_details(, "questionr")` for more info

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

# rabhit

<details>

* Version: 0.2.4
* GitHub: NA
* Source code: https://github.com/cran/rabhit
* Date/Publication: 2022-09-22 15:10:02 UTC
* Number of recursive dependencies: 130

Run `revdepcheck::cloud_details(, "rabhit")` for more info

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
# romic

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/romic
* Date/Publication: 2021-07-20 09:00:02 UTC
* Number of recursive dependencies: 111

Run `revdepcheck::cloud_details(, "romic")` for more info

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

Run `revdepcheck::cloud_details(, "seecolor")` for more info

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
     11. │       ├─crayon::make_style(contrast_color(bg.color))
     12. │       └─seecolor:::contrast_color(bg.color)
     13. │         ├─... %>% ...
     14. │         └─dplyr::if_else(grDevices::col2rgb(x) < 128, 255, 0)
     15. │           └─vctrs::vec_assert(x = condition, ptype = logical(), arg = "condition")
     16. │             └─rlang::abort(...)
     17. ├─purrr::set_names(., c("red", "green", "blue", "maxColorValue"))
     18. ├─base::append(., 255)
     19. └─base::as.list(.)
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Intro-to-seecolor-package.Rmd’ using rmarkdown
    Quitting from lines 49-60 (Intro-to-seecolor-package.Rmd) 
    Error: processing vignette 'Intro-to-seecolor-package.Rmd' failed with diagnostics:
    [1m[22m[36mℹ[39m In index: 1.
    [1mCaused by error in `dplyr::if_else()`:[22m
    [33m![39m `condition` must be a vector with type <logical>.
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

# sift

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/sift
* Date/Publication: 2021-07-05 09:10:02 UTC
* Number of recursive dependencies: 82

Run `revdepcheck::cloud_details(, "sift")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
        3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
        4. ├─sift::break_join(g, h, brk = "c", by = character())
        5. └─sift:::break_join.data.frame(g, h, brk = "c", by = character())
        6.   ├─dplyr::left_join(...)
        7.   └─dplyr:::left_join.data.frame(...)
        8.     └─dplyr:::join_mutate(...)
        9.       ├─dplyr:::as_join_by(by, error_call = error_call)
       10.       └─dplyr:::as_join_by.character(by, error_call = error_call)
       11.         └─dplyr:::finalise_equi_join_by(x_names, y_names)
       12.           └─rlang::abort(...)
      
      [ FAIL 2 | WARN 9 | SKIP 0 | PASS 39 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1231 marked UTF-8 strings
    ```

# simplevis

<details>

* Version: 6.4.0
* GitHub: https://github.com/StatisticsNZ/simplevis
* Source code: https://github.com/cran/simplevis
* Date/Publication: 2022-08-05 14:00:02 UTC
* Number of recursive dependencies: 134

Run `revdepcheck::cloud_details(, "simplevis")` for more info

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

Run `revdepcheck::cloud_details(, "skater")` for more info

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
       27. │ └─rlang::abort(bullets, call = error_call, parent = cnd)
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
    Caused by error in `dplyr::across()`:
    ! Can't compute column `dadid`.
    Caused by error in `fn()`:
    ! Can't convert `y` <double> to match type of `x` <character>.
    --- failed re-building ‘basic_usage.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘basic_usage.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# starschemar

<details>

* Version: 1.2.0
* GitHub: NA
* Source code: https://github.com/cran/starschemar
* Date/Publication: 2020-09-25 21:30:02 UTC
* Number of recursive dependencies: 67

Run `revdepcheck::cloud_details(, "starschemar")` for more info

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
* Number of recursive dependencies: 92

Run `revdepcheck::cloud_details(, "suddengains")` for more info

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

Run `revdepcheck::cloud_details(, "SwimmeR")` for more info

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

Run `revdepcheck::cloud_details(, "tabshiftr")` for more info

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

# tfrmt

<details>

* Version: 0.0.1
* GitHub: https://github.com/GSK-Biostatistics/tfrmt
* Source code: https://github.com/cran/tfrmt
* Date/Publication: 2022-12-06 17:00:02 UTC
* Number of recursive dependencies: 90

Run `revdepcheck::cloud_details(, "tfrmt")` for more info

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
       45. │ └─rlang::abort(bullets, call = error_call, parent = cnd)
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

Run `revdepcheck::cloud_details(, "tidyCDISC")` for more info

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
      
      [ FAIL 1 | WARN 13 | SKIP 15 | PASS 97 ]
      Error: Test failures
      Execution halted
    ```

# tidygraph

<details>

* Version: 1.2.2
* GitHub: https://github.com/thomasp85/tidygraph
* Source code: https://github.com/cran/tidygraph
* Date/Publication: 2022-08-22 07:20:02 UTC
* Number of recursive dependencies: 77

Run `revdepcheck::cloud_details(, "tidygraph")` for more info

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

# tidytransit

<details>

* Version: 1.4
* GitHub: https://github.com/r-transit/tidytransit
* Source code: https://github.com/cran/tidytransit
* Date/Publication: 2022-08-26 08:00:02 UTC
* Number of recursive dependencies: 106

Run `revdepcheck::cloud_details(, "tidytransit")` for more info

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

# topr

<details>

* Version: 1.1.2
* GitHub: https://github.com/GenuityScience/topr
* Source code: https://github.com/cran/topr
* Date/Publication: 2023-01-13 16:20:02 UTC
* Number of recursive dependencies: 77

Run `revdepcheck::cloud_details(, "topr")` for more info

</details>

## Newly broken

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

Run `revdepcheck::cloud_details(, "track2KBA")` for more info

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

# treeplyr

<details>

* Version: 0.1.10
* GitHub: https://github.com/uyedaj/treeplyr
* Source code: https://github.com/cran/treeplyr
* Date/Publication: 2020-09-17 10:10:02 UTC
* Number of recursive dependencies: 45

Run `revdepcheck::cloud_details(, "treeplyr")` for more info

</details>

## Newly broken

*   checking whether package ‘treeplyr’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ape::where’ by ‘dplyr::where’ when loading ‘treeplyr’
    See ‘/tmp/workdir/treeplyr/new/treeplyr.Rcheck/00install.out’ for details.
    ```

# wcep

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/wcep
* Date/Publication: 2020-11-13 11:40:02 UTC
* Number of recursive dependencies: 56

Run `revdepcheck::cloud_details(, "wcep")` for more info

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

