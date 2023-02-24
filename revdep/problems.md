# APCI

<details>

* Version: 1.0.6
* GitHub: NA
* Source code: https://github.com/cran/APCI
* Date/Publication: 2022-11-11 08:00:02 UTC
* Number of recursive dependencies: 86

Run `revdepcheck::cloud_details(, "APCI")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘1_tests.R’
    Running the tests in ‘tests/1_tests.R’ failed.
    Last 13 lines of output:
          acc2:pcc2     acc3:pcc2     acc4:pcc2     acc5:pcc2     acc6:pcc2 
       -0.216428004  -0.043290370  -0.180281348   0.023065756   0.420335894 
          acc7:pcc2     acc8:pcc2     acc9:pcc2     acc1:pcc3     acc2:pcc3 
        0.156857688  -0.288094238   0.189149410   0.065439829  -0.012447066 
          acc3:pcc3     acc4:pcc3     acc5:pcc3     acc6:pcc3     acc7:pcc3 
       -0.278196394   0.069447828   0.174775636  -0.382346644   0.180839504 
          acc8:pcc3     acc9:pcc3     acc1:pcc4     acc2:pcc4     acc3:pcc4 
       -0.048009596   0.097298409  -0.399042730   0.002354411   0.285678475 
          acc4:pcc4     acc5:pcc4     acc6:pcc4     acc7:pcc4     acc8:pcc4 
        0.287191164   0.247249252  -0.129748091  -0.008281466  -0.388869169 
          acc9:pcc4     acc1:pcc5     acc2:pcc5     acc3:pcc5     acc4:pcc5 
        0.106270072   0.037976315   0.309803463  -0.121759339  -0.117393217 
          acc5:pcc5     acc6:pcc5     acc7:pcc5     acc8:pcc5     acc9:pcc5 
       -0.537288254  -0.346360367   0.277715500   0.650307799  -0.257940113 
      Killed
    ```

# arrow

<details>

* Version: 11.0.0.2
* GitHub: https://github.com/apache/arrow
* Source code: https://github.com/cran/arrow
* Date/Publication: 2023-02-12 14:12:07 UTC
* Number of recursive dependencies: 76

Run `revdepcheck::cloud_details(, "arrow")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        3. │ │ └─base::withVisible(...elt(i))
        4. │ └─query %>% show_query()
        5. ├─dplyr::show_query(.)
        6. ├─tab %>% count(lgl)
        7. ├─dplyr::count(., lgl)
        8. └─arrow:::count.ArrowTabular(., lgl)
        9.   ├─dplyr::tally(...)
       10.   └─arrow:::tally.arrow_dplyr_query(name = name)
       11.     └─dplyr:::check_name(name, dplyr::group_vars(x))
       12.       └─dplyr:::stop_input_type(...)
       13.         └─rlang::abort(message, ..., call = call, arg = arg)
      
      [ FAIL 9 | WARN 10 | SKIP 76 | PASS 7546 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 123.3Mb
      sub-directories of 1Mb or more:
        R       4.2Mb
        libs  118.6Mb
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘readr’
    ```

# autoReg

<details>

* Version: 0.2.6
* GitHub: https://github.com/cardiomoon/autoReg
* Source code: https://github.com/cran/autoReg
* Date/Publication: 2022-04-05 06:20:02 UTC
* Number of recursive dependencies: 225

Run `revdepcheck::cloud_details(, "autoReg")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘autoReg-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: addFitSummary
    > ### Title: Add model summary to an object of class gaze
    > ### Aliases: addFitSummary
    > 
    > ### ** Examples
    > 
    > require(survival)
    ...
     12. │             ├─dplyr::group_vars(x)
     13. │             └─dplyr:::group_vars.data.frame(x)
     14. │               ├─generics::setdiff(names(group_data(x)), ".rows")
     15. │               ├─dplyr::group_data(x)
     16. │               └─dplyr:::group_data.data.frame(x)
     17. │                 └─vctrs::vec_size(.data)
     18. └─vctrs:::stop_scalar_type(`<fn>`(`<gaze[,4]>`), "x", `<env>`)
     19.   └─vctrs:::stop_vctrs(...)
     20.     └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Automatic_Regression_Modeling.Rmd’ using rmarkdown
    Quitting from lines 100-103 (Automatic_Regression_Modeling.Rmd) 
    Error: processing vignette 'Automatic_Regression_Modeling.Rmd' failed with diagnostics:
    `x` must be a vector, not a <gaze/data.frame/tibble> object.
    --- failed re-building ‘Automatic_Regression_Modeling.Rmd’
    
    --- re-building ‘Bootstrap_Prediction.Rmd’ using rmarkdown
    Quitting from lines 37-38 (Bootstrap_Prediction.Rmd) 
    Error: processing vignette 'Bootstrap_Prediction.Rmd' failed with diagnostics:
    ...
    Error: processing vignette 'Survival.Rmd' failed with diagnostics:
    `x` must be a vector, not a <gaze/data.frame/tibble> object.
    --- failed re-building ‘Survival.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘Automatic_Regression_Modeling.Rmd’ ‘Bootstrap_Prediction.Rmd’
      ‘Getting_started.Rmd’ ‘Survival.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# comperes

<details>

* Version: 0.2.6
* GitHub: https://github.com/echasnovski/comperes
* Source code: https://github.com/cran/comperes
* Date/Publication: 2023-01-08 19:00:02 UTC
* Number of recursive dependencies: 63

Run `revdepcheck::cloud_details(, "comperes")` for more info

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
      ── Failure ('test-head-to-head.R:168'): h2h_mat allows multiple Head-to-Head functions ──
      `h2h_mat(cr_data)` produced warnings.
      
      [ FAIL 1 | WARN 16 | SKIP 5 | PASS 256 ]
      Error: Test failures
      Execution halted
    ```

# dbplyr

<details>

* Version: 2.3.1
* GitHub: https://github.com/tidyverse/dbplyr
* Source code: https://github.com/cran/dbplyr
* Date/Publication: 2023-02-24 18:20:02 UTC
* Number of recursive dependencies: 79

Run `revdepcheck::cloud_details(, "dbplyr")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in documentation object 'arrange.tbl_lazy.Rd':
      ‘[dplyr:dplyr_data_masking]{data-masking}’
    
    Missing link or links in documentation object 'count.tbl_lazy.Rd':
      ‘[dplyr:dplyr_data_masking]{data-masking}’
    
    Missing link or links in documentation object 'distinct.tbl_lazy.Rd':
      ‘[dplyr:dplyr_data_masking]{data-masking}’
    
    Missing link or links in documentation object 'filter.tbl_lazy.Rd':
    ...
    Missing link or links in documentation object 'pull.tbl_sql.Rd':
      ‘[dplyr:dplyr_data_masking]{data-masking}’
    
    Missing link or links in documentation object 'select.tbl_lazy.Rd':
      ‘[dplyr:dplyr_data_masking]{data-masking}’
    
    Missing link or links in documentation object 'summarise.tbl_lazy.Rd':
      ‘[dplyr:dplyr_data_masking]{data-masking}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

# dexter

<details>

* Version: 1.2.2
* GitHub: https://github.com/dexter-psychometrics/dexter
* Source code: https://github.com/cran/dexter
* Date/Publication: 2022-11-08 14:10:08 UTC
* Number of recursive dependencies: 85

Run `revdepcheck::cloud_details(, "dexter")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       13. ├─dexter:::pv_recycle(...)
       14. │ └─dexter:::score_tab_single(score, sum(a[last]))
       15. ├─booklet_id
       16. ├─rlang:::`$.rlang_data_pronoun`(.data, booklet_id)
       17. │ └─rlang:::data_pronoun_get(...)
       18. ├─`<fn>`()
       19. └─base::.handleSimpleError(...)
       20.   └─dplyr (local) h(simpleError(msg, call))
       21.     └─rlang::abort(message, class = error_class, parent = parent, call = error_call)
      
      [ FAIL 1 | WARN 23 | SKIP 2 | PASS 220 ]
      Error: Test failures
      In addition: Warning message:
      call dbDisconnect() when finished working with a connection 
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘DIF_vignette.Rmd’ using rmarkdown
    --- finished re-building ‘DIF_vignette.Rmd’
    
    --- re-building ‘Equating.Rmd’ using rmarkdown
    Quitting from lines 241-248 (Equating.Rmd) 
    Error: processing vignette 'Equating.Rmd' failed with diagnostics:
    ℹ In argument: `PVX = pv_recycle(...)`.
    Caused by error:
    ! attempt to select less than one element in integerOneIndex
    ...
    --- finished re-building ‘dexter.Rmd’
    
    --- re-building ‘profile-plots.Rmd’ using rmarkdown
    --- finished re-building ‘profile-plots.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Equating.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 12.9Mb
      sub-directories of 1Mb or more:
        doc    1.1Mb
        libs  11.0Mb
    ```

# dm

<details>

* Version: 1.0.4
* GitHub: https://github.com/cynkra/dm
* Source code: https://github.com/cran/dm
* Date/Publication: 2023-02-11 19:30:02 UTC
* Number of recursive dependencies: 157

Run `revdepcheck::cloud_details(, "dm")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      • only works on `postgres`, `mssql`, `sqlite` (1)
      • only works on `postgres`, `sqlite`, `mssql`, `maria` (1)
      • only works on `sqlite` (1)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-flatten.R:315'): tests with 'bad_dm' work ────────────────────
      `expect_equivalent_tbl(...)` did not throw the expected warning.
      ── Failure ('test-flatten.R:372'): tests with 'bad_dm' work (2) ────────────────
      `expect_equivalent_tbl(...)` did not throw the expected warning.
      ── Failure ('test-flatten.R:419'): tests with 'bad_dm' work (3) ────────────────
      `expect_equivalent_tbl(...)` did not throw the expected warning.
      
      [ FAIL 3 | WARN 222 | SKIP 210 | PASS 1336 ]
      Error: Test failures
      Execution halted
    ```

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in documentation object 'dplyr_table_manipulation.Rd':
      ‘[dplyr:dplyr_data_masking]{data-masking}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

# dtplyr

<details>

* Version: 1.3.0
* GitHub: https://github.com/tidyverse/dtplyr
* Source code: https://github.com/cran/dtplyr
* Date/Publication: 2023-02-24 08:42:43 UTC
* Number of recursive dependencies: 66

Run `revdepcheck::cloud_details(, "dtplyr")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in documentation object 'arrange.dtplyr_step.Rd':
      ‘[dplyr:dplyr_data_masking]{data-masking}’
    
    Missing link or links in documentation object 'count.dtplyr_step.Rd':
      ‘[dplyr:dplyr_data_masking]{data-masking}’
    
    Missing link or links in documentation object 'distinct.dtplyr_step.Rd':
      ‘[dplyr:dplyr_data_masking]{data-masking}’
    
    Missing link or links in documentation object 'filter.dtplyr_step.Rd':
    ...
    Missing link or links in documentation object 'slice.dtplyr_step.Rd':
      ‘[dplyr:dplyr_data_masking]{data-masking}’
    
    Missing link or links in documentation object 'summarise.dtplyr_step.Rd':
      ‘[dplyr:dplyr_data_masking]{data-masking}’
    
    Missing link or links in documentation object 'transmute.dtplyr_step.Rd':
      ‘[dplyr:dplyr_data_masking]{data-masking}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

# dtrackr

<details>

* Version: 0.2.4
* GitHub: NA
* Source code: https://github.com/cran/dtrackr
* Date/Publication: 2022-07-05 21:00:09 UTC
* Number of recursive dependencies: 155

Run `revdepcheck::cloud_details(, "dtrackr")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in documentation object 'add_count.Rd':
      ‘[dplyr:dplyr_data_masking]{data-masking}’
    
    Missing link or links in documentation object 'add_tally.Rd':
      ‘[dplyr:dplyr_data_masking]{data-masking}’
    
    Missing link or links in documentation object 'p_add_count.Rd':
      ‘[dplyr:dplyr_data_masking]{data-masking}’
    
    Missing link or links in documentation object 'p_add_tally.Rd':
      ‘[dplyr:dplyr_data_masking]{data-masking}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

# edeaR

<details>

* Version: 0.9.3
* GitHub: https://github.com/bupaverse/edeaR
* Source code: https://github.com/cran/edeaR
* Date/Publication: 2023-02-16 17:30:02 UTC
* Number of recursive dependencies: 98

Run `revdepcheck::cloud_details(, "edeaR")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in documentation object 'filter_case_condition.Rd':
      ‘dplyr_data_masking’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

# epifitter

<details>

* Version: 0.3.0
* GitHub: https://github.com/AlvesKS/epifitter
* Source code: https://github.com/cran/epifitter
* Date/Publication: 2021-06-14 14:50:02 UTC
* Number of recursive dependencies: 88

Run `revdepcheck::cloud_details(, "epifitter")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘epifitter-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: fit_lin
    > ### Title: Fits epidemic models using data linearization
    > ### Aliases: fit_lin
    > 
    > ### ** Examples
    > 
    > set.seed(1)
    ...
     15. │ └─dplyr:::mutate_cols(.data, dplyr_quosures(...), by)
     16. │   ├─base::withCallingHandlers(...)
     17. │   └─dplyr:::mutate_col(dots[[i]], data, mask, new_columns)
     18. │     └─mask$eval_all_mutate(quo)
     19. │       └─dplyr (local) eval()
     20. ├─`<fn>`()
     21. └─base::.handleSimpleError(...)
     22.   └─dplyr (local) h(simpleError(msg, call))
     23.     └─rlang::abort(message, class = error_class, parent = parent, call = error_call)
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘fitting.Rmd’ using rmarkdown
    Quitting from lines 86-91 (fitting.Rmd) 
    Error: processing vignette 'fitting.Rmd' failed with diagnostics:
    ℹ In argument: `Exponential = *...`.
    Caused by error:
    ! attempt to select less than one element in integerOneIndex
    --- failed re-building ‘fitting.Rmd’
    
    --- re-building ‘simulation.Rmd’ using rmarkdown
    --- finished re-building ‘simulation.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘fitting.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘knitr’
      All declared Imports should be used.
    ```

# exuber

<details>

* Version: 1.0.1
* GitHub: https://github.com/kvasilopoulos/exuber
* Source code: https://github.com/cran/exuber
* Date/Publication: 2023-02-12 21:42:06 UTC
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
       33. │             └─base (local) withOneRestart(expr, restarts[[1L]])
       34. │               └─base (local) doWithOneRestart(return(expr), restart)
       35. └─dplyr (local) `<fn>`(`<vc______>`)
       36.   └─dplyr:::rethrow_warning_join_relationship_many_to_many(cnd, error_call)
       37.     └─dplyr:::warn_join(...)
       38.       └─dplyr:::warn_dplyr(...)
       39.         └─rlang::warn(...)
       40.           └─base::warning(cnd)
       41.             └─base::withRestarts(...)
       42.               └─base (local) withOneRestart(expr, restarts[[1L]])
       43.                 └─base (local) doWithOneRestart(return(expr), restart)
      
      [ FAIL 22 | WARN 29 | SKIP 4 | PASS 214 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        libs   4.3Mb
    ```

# ezplot

<details>

* Version: 0.7.5
* GitHub: NA
* Source code: https://github.com/cran/ezplot
* Date/Publication: 2022-11-26 22:10:02 UTC
* Number of recursive dependencies: 107

Run `revdepcheck::cloud_details(, "ezplot")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ezplot-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: distribution_plot
    > ### Title: distribution_plot
    > ### Aliases: distribution_plot
    > 
    > ### ** Examples
    > 
    > n = 100
    ...
     16. │     └─rlang::eval_tidy(xs[[j]], mask)
     17. ├─dplyr::mutate(...)
     18. ├─dplyr::left_join(., unit, "order")
     19. ├─dplyr::mutate(...)
     20. ├─base::mean(x)
     21. ├─`<fn>`()
     22. └─base::.handleSimpleError(...)
     23.   └─dplyr (local) h(simpleError(msg, call))
     24.     └─rlang::abort(message, class = error_class, parent = parent, call = error_call)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       14. │ └─tibble::tibble(x = x)
       15. │   └─tibble:::tibble_quos(xs, .rows, .name_repair)
       16. │     └─rlang::eval_tidy(xs[[j]], mask)
       17. ├─dplyr::mutate(...)
       18. ├─dplyr::left_join(., unit, "order")
       19. ├─dplyr::mutate(...)
       20. ├─base::mean(x)
       21. ├─`<fn>`()
       22. └─base::.handleSimpleError(...)
       23.   └─dplyr (local) h(simpleError(msg, call))
       24.     └─rlang::abort(message, class = error_class, parent = parent, call = error_call)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 93 ]
      Error: Test failures
      Execution halted
    ```

# fabletools

<details>

* Version: 0.3.2
* GitHub: https://github.com/tidyverts/fabletools
* Source code: https://github.com/cran/fabletools
* Date/Publication: 2021-11-29 05:50:02 UTC
* Number of recursive dependencies: 102

Run `revdepcheck::cloud_details(, "fabletools")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in documentation object 'aggregate_index.Rd':
      ‘[dplyr:dplyr_data_masking]{data-masking}’
    
    Missing link or links in documentation object 'aggregate_key.Rd':
      ‘[dplyr:dplyr_data_masking]{data-masking}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

# ForIT

<details>

* Version: 2.0.1
* GitHub: NA
* Source code: https://github.com/cran/ForIT
* Date/Publication: 2022-06-11 21:30:02 UTC
* Number of recursive dependencies: 69

Run `revdepcheck::cloud_details(, "ForIT")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ForIT-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: INFCvpe
    > ### Title: Estimate bole volume or tree phytomass for individual stems,
    > ###   with associated accuracy info
    > ### Aliases: INFCvpe
    > 
    > ### ** Examples
    > 
    ...
     14. │ └─dplyr:::mutate_cols(.data, dplyr_quosures(...), by)
     15. │   ├─base::withCallingHandlers(...)
     16. │   └─dplyr:::mutate_col(dots[[i]], data, mask, new_columns)
     17. │     └─mask$eval_all_mutate(quo)
     18. │       └─dplyr (local) eval()
     19. ├─`<fn>`()
     20. └─base::.handleSimpleError(...)
     21.   └─dplyr (local) h(simpleError(msg, call))
     22.     └─rlang::abort(message, class = error_class, parent = parent, call = error_call)
    Execution halted
    ```

# GDPuc

<details>

* Version: 0.10.0
* GitHub: https://github.com/pik-piam/GDPuc
* Source code: https://github.com/cran/GDPuc
* Date/Publication: 2023-01-05 20:00:02 UTC
* Number of recursive dependencies: 90

Run `revdepcheck::cloud_details(, "GDPuc")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Caused by error in `dplyr::filter()`:
      i In argument: `.data$year %in% unique(gdp$year)`.
      Caused by error:
      ! attempt to select less than one element in integerOneIndex
      ── Error ('test-composite_conversions.R:579'): constant_USMER_base_x_2_current_USMER ──
      Error in `dplyr::filter(MER, .data$iso3c %in% unique(gdp$iso3c), .data$year %in% 
          unique(gdp$year))`: i In argument: `.data$iso3c %in% unique(gdp$iso3c)`.
      Caused by error in `dplyr::filter()`:
      i In argument: `.data$year %in% unique(gdp$year)`.
      Caused by error:
      ! attempt to select less than one element in integerOneIndex
      
      [ FAIL 15 | WARN 61 | SKIP 0 | PASS 2598 ]
      Error: Test failures
      Execution halted
    ```

# gtreg

<details>

* Version: 0.2.0
* GitHub: https://github.com/shannonpileggi/gtreg
* Source code: https://github.com/cran/gtreg
* Date/Publication: 2022-10-18 15:25:09 UTC
* Number of recursive dependencies: 104

Run `revdepcheck::cloud_details(, "gtreg")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘gtreg-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: tbl_reg_summary
    > ### Title: Data Summary Table
    > ### Aliases: tbl_reg_summary
    > 
    > ### ** Examples
    > 
    > tbl_reg_summary_ex1 <-
    ...
     29. │   └─vctrs::vec_size_common(...)
     30. ├─base::unlist(stat_display)
     31. ├─stat_display
     32. ├─rlang:::`$.rlang_data_pronoun`(.data, stat_display)
     33. │ └─rlang:::data_pronoun_get(...)
     34. ├─`<fn>`()
     35. └─base::.handleSimpleError(...)
     36.   └─dplyr (local) h(simpleError(msg, call))
     37.     └─rlang::abort(message, class = error_class, parent = parent, call = error_call)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       33. ├─stringr::str_extract_all(unlist(stat_display), "\\{.*?\\}")
       34. │ └─stringr:::check_lengths(string, pattern)
       35. │   └─vctrs::vec_size_common(...)
       36. ├─base::unlist(stat_display)
       37. ├─stat_display
       38. ├─rlang:::`$.rlang_data_pronoun`(.data, stat_display)
       39. │ └─rlang:::data_pronoun_get(...)
       40. ├─`<fn>`()
       41. └─base::.handleSimpleError(...)
       42.   └─dplyr (local) h(simpleError(msg, call))
       43.     └─rlang::abort(message, class = error_class, parent = parent, call = error_call)
      
      [ FAIL 14 | WARN 19 | SKIP 4 | PASS 50 ]
      Error: Test failures
      Execution halted
    ```

# gtsummary

<details>

* Version: 1.7.0
* GitHub: https://github.com/ddsjoberg/gtsummary
* Source code: https://github.com/cran/gtsummary
* Date/Publication: 2023-01-13 08:50:06 UTC
* Number of recursive dependencies: 199

Run `revdepcheck::cloud_details(, "gtsummary")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘gtsummary-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: add_n.tbl_summary
    > ### Title: Add column with N
    > ### Aliases: add_n.tbl_summary add_n.tbl_svysummary
    > 
    > ### ** Examples
    > 
    > # Example 1 ----------------------------------
    ...
     25. │   └─vctrs::vec_size_common(...)
     26. ├─base::unlist(stat_display)
     27. ├─stat_display
     28. ├─rlang:::`$.rlang_data_pronoun`(.data, stat_display)
     29. │ └─rlang:::data_pronoun_get(...)
     30. ├─`<fn>`()
     31. └─base::.handleSimpleError(...)
     32.   └─dplyr (local) h(simpleError(msg, call))
     33.     └─rlang::abort(message, class = error_class, parent = parent, call = error_call)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       22. ├─stringr::str_extract_all(unlist(stat_display), "\\{.*?\\}")
       23. │ └─stringr:::check_lengths(string, pattern)
       24. │   └─vctrs::vec_size_common(...)
       25. ├─base::unlist(stat_display)
       26. ├─stat_display
       27. ├─rlang:::`$.rlang_data_pronoun`(.data, stat_display)
       28. │ └─rlang:::data_pronoun_get(...)
       29. ├─`<fn>`()
       30. └─base::.handleSimpleError(...)
       31.   └─dplyr (local) h(simpleError(msg, call))
       32.     └─rlang::abort(message, class = error_class, parent = parent, call = error_call)
      
      [ FAIL 5 | WARN 3 | SKIP 75 | PASS 5 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘gallery.Rmd’ using rmarkdown
    --- finished re-building ‘gallery.Rmd’
    
    --- re-building ‘gtsummary_definition.Rmd’ using rmarkdown
    Quitting from lines 27-38 (gtsummary_definition.Rmd) 
    Error: processing vignette 'gtsummary_definition.Rmd' failed with diagnostics:
    ℹ In argument: `stat_label = stat_label_match(.data$stat_display)`.
    Caused by error:
    ! attempt to select less than one element in integerOneIndex
    ...
    ℹ In argument: `stat_label = stat_label_match(.data$stat_display)`.
    Caused by error:
    ! attempt to select less than one element in integerOneIndex
    --- failed re-building ‘themes.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘gtsummary_definition.Rmd’ ‘themes.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# iNZightTools

<details>

* Version: 1.13.0
* GitHub: https://github.com/iNZightVIT/iNZightTools
* Source code: https://github.com/cran/iNZightTools
* Date/Publication: 2023-01-26 22:10:02 UTC
* Number of recursive dependencies: 115

Run `revdepcheck::cloud_details(, "iNZightTools")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in documentation object 'filter.Rd':
      ‘[dplyr:dplyr_data_masking]{data-masking}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

# IPDFileCheck

<details>

* Version: 0.7.5
* GitHub: NA
* Source code: https://github.com/cran/IPDFileCheck
* Date/Publication: 2022-02-01 08:00:10 UTC
* Number of recursive dependencies: 150

Run `revdepcheck::cloud_details(, "IPDFileCheck")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘IPDFileCheck-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: get_summary_gtsummary
    > ### Title: Function to return the summary table using gtsummary package
    > ### Aliases: get_summary_gtsummary
    > 
    > ### ** Examples
    > 
    > trial <- gtsummary::trial
    ...
     33. │   └─vctrs::vec_size_common(...)
     34. ├─base::unlist(stat_display)
     35. ├─stat_display
     36. ├─rlang:::`$.rlang_data_pronoun`(.data, stat_display)
     37. │ └─rlang:::data_pronoun_get(...)
     38. ├─`<fn>`()
     39. └─base::.handleSimpleError(...)
     40.   └─dplyr (local) h(simpleError(msg, call))
     41.     └─rlang::abort(message, class = error_class, parent = parent, call = error_call)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       25. ├─stringr::str_extract_all(unlist(stat_display), "\\{.*?\\}")
       26. │ └─stringr:::check_lengths(string, pattern)
       27. │   └─vctrs::vec_size_common(...)
       28. ├─base::unlist(stat_display)
       29. ├─stat_display
       30. ├─rlang:::`$.rlang_data_pronoun`(.data, stat_display)
       31. │ └─rlang:::data_pronoun_get(...)
       32. ├─`<fn>`()
       33. └─base::.handleSimpleError(...)
       34.   └─dplyr (local) h(simpleError(msg, call))
       35.     └─rlang::abort(message, class = error_class, parent = parent, call = error_call)
      
      [ FAIL 1 | WARN 4 | SKIP 0 | PASS 162 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘gmodels’ ‘lmtest’ ‘testthat’ ‘tidyverse’ ‘zoo’
      All declared Imports should be used.
    ```

# ir

<details>

* Version: 0.2.1
* GitHub: NA
* Source code: https://github.com/cran/ir
* Date/Publication: 2022-05-02 11:50:08 UTC
* Number of recursive dependencies: 138

Run `revdepcheck::cloud_details(, "ir")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in documentation object 'arrange.ir.Rd':
      ‘[dplyr:dplyr_data_masking]{data-masking}’
    
    Missing link or links in documentation object 'distinct.ir.Rd':
      ‘[dplyr:dplyr_data_masking]{data-masking}’
    
    Missing link or links in documentation object 'filter.ir.Rd':
      ‘[dplyr:dplyr_data_masking]{data-masking}’
    
    Missing link or links in documentation object 'mutate.Rd':
      ‘[dplyr:dplyr_data_masking]{data-masking}’
    
    Missing link or links in documentation object 'slice.Rd':
      ‘[dplyr:dplyr_data_masking]{data-masking}’
    
    Missing link or links in documentation object 'summarize.Rd':
      ‘[dplyr:dplyr_data_masking]{data-masking}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

# itscalledsoccer

<details>

* Version: 0.2.1
* GitHub: https://github.com/American-Soccer-Analysis/itscalledsoccer-r
* Source code: https://github.com/cran/itscalledsoccer
* Date/Publication: 2022-11-07 14:50:05 UTC
* Number of recursive dependencies: 52

Run `revdepcheck::cloud_details(, "itscalledsoccer")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        7. │     ├─base::withCallingHandlers(...)
        8. │     └─mask$eval_all_filter(dots, env_filter)
        9. │       └─dplyr (local) eval()
       10. ├─.data$away_team_id %in% ...
       11. ├─away_team_id
       12. ├─rlang:::`$.rlang_data_pronoun`(.data, away_team_id)
       13. │ └─rlang:::data_pronoun_get(...)
       14. ├─`<fn>`()
       15. └─base::.handleSimpleError(...)
       16.   └─dplyr (local) h(simpleError(msg, call))
       17.     └─rlang::abort(message, class = error_class, parent = parent, call = error_call)
      
      [ FAIL 1 | WARN 199 | SKIP 0 | PASS 271 ]
      Error: Test failures
      Execution halted
    ```

# mapping

<details>

* Version: 1.3
* GitHub: https://github.com/serafinialessio/mapping
* Source code: https://github.com/cran/mapping
* Date/Publication: 2021-07-22 17:40:02 UTC
* Number of recursive dependencies: 150

Run `revdepcheck::cloud_details(, "mapping")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘mapping-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: DE
    > ### Title: Object of class 'UK'
    > ### Aliases: DE
    > 
    > ### ** Examples
    > 
    > 
    ...
     14. │       ├─dplyr::group_vars(x)
     15. │       └─dplyr:::group_vars.data.frame(x)
     16. │         ├─generics::setdiff(names(group_data(x)), ".rows")
     17. │         ├─dplyr::group_data(x)
     18. │         └─dplyr:::group_data.data.frame(x)
     19. │           └─vctrs::vec_size(.data)
     20. └─vctrs:::stop_scalar_type(`<fn>`(`<df[,5]>`), "x", `<env>`)
     21.   └─vctrs:::stop_vctrs(...)
     22.     └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘a-journey-into-mapping.Rmd’ using rmarkdown
    trying URL 'https://raw.githubusercontent.com/dataallaround/geospatial/master/EU/nuts/GeoJSON/eu_2021_nuts0_20m.geojson'
    Content type 'text/plain; charset=utf-8' length 135449 bytes (132 KB)
    ==================================================
    downloaded 132 KB
    
    trying URL 'https://raw.githubusercontent.com/dataallaround/geospatial/master/IT/GeoJSON/it_2018_provincia.geojson'
    Content type 'text/plain; charset=utf-8' length 3541074 bytes (3.4 MB)
    ...
    Quitting from lines 139-140 (a-journey-into-mapping.Rmd) 
    Error: processing vignette 'a-journey-into-mapping.Rmd' failed with diagnostics:
    `x` must be a vector, not a <data.frame/IT> object.
    --- failed re-building ‘a-journey-into-mapping.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘a-journey-into-mapping.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 29618 marked UTF-8 strings
    ```

# NetworkExtinction

<details>

* Version: 1.0.2
* GitHub: NA
* Source code: https://github.com/cran/NetworkExtinction
* Date/Publication: 2022-12-07 20:32:30 UTC
* Number of recursive dependencies: 96

Run `revdepcheck::cloud_details(, "NetworkExtinction")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘NetworkExtinction.Rmd’ using rmarkdown
    Quitting from lines 195-199 (NetworkExtinction.Rmd) 
    Error: processing vignette 'NetworkExtinction.Rmd' failed with diagnostics:
    `x` must be a vector, not a <data.frame/SimulateExt> object.
    --- failed re-building ‘NetworkExtinction.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘NetworkExtinction.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# onemap

<details>

* Version: 3.0.0
* GitHub: https://github.com/augusto-garcia/onemap
* Source code: https://github.com/cran/onemap
* Date/Publication: 2022-11-26 05:00:02 UTC
* Number of recursive dependencies: 156

Run `revdepcheck::cloud_details(, "onemap")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Inbred_Based_Populations.Rmd’ using rmarkdown
    Read 1 item
    Quitting from lines 1696-1699 (Inbred_Based_Populations.Rmd) 
    Error: processing vignette 'Inbred_Based_Populations.Rmd' failed with diagnostics:
    `x` must be a vector, not a <onemap_progeny_haplotypes/f2/data.frame/most.likely> object.
    --- failed re-building ‘Inbred_Based_Populations.Rmd’
    
    --- re-building ‘Introduction_R.Rmd’ using rmarkdown
    ...
    --- finished re-building ‘Outcrossing_Populations.Rmd’
    
    --- re-building ‘Overview.Rmd’ using rmarkdown
    --- finished re-building ‘Overview.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Inbred_Based_Populations.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.1Mb
      sub-directories of 1Mb or more:
        doc    1.6Mb
        libs   5.4Mb
    ```

# PKNCA

<details>

* Version: 0.10.1
* GitHub: https://github.com/billdenney/pknca
* Source code: https://github.com/cran/PKNCA
* Date/Publication: 2023-01-11 10:23:28 UTC
* Number of recursive dependencies: 135

Run `revdepcheck::cloud_details(, "PKNCA")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in documentation object 'filter.PKNCAresults.Rd':
      ‘[dplyr:dplyr_data_masking]{data-masking}’
    
    Missing link or links in documentation object 'mutate.PKNCAresults.Rd':
      ‘[dplyr:dplyr_data_masking]{data-masking}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

# pubh

<details>

* Version: 1.2.7
* GitHub: https://github.com/josie-athens/pubh
* Source code: https://github.com/cran/pubh
* Date/Publication: 2022-04-04 13:50:02 UTC
* Number of recursive dependencies: 235

Run `revdepcheck::cloud_details(, "pubh")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘pubh-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: Bernard
    > ### Title: Survival of patients with sepsis.
    > ### Aliases: Bernard
    > ### Keywords: datasets
    > 
    > ### ** Examples
    > 
    ...
     27. │   └─vctrs::vec_size_common(...)
     28. ├─base::unlist(stat_display)
     29. ├─stat_display
     30. ├─rlang:::`$.rlang_data_pronoun`(.data, stat_display)
     31. │ └─rlang:::data_pronoun_get(...)
     32. ├─`<fn>`()
     33. └─base::.handleSimpleError(...)
     34.   └─dplyr (local) h(simpleError(msg, call))
     35.     └─rlang::abort(message, class = error_class, parent = parent, call = error_call)
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘introduction.Rmd’ using rmarkdown
    Quitting from lines 136-144 (introduction.Rmd) 
    Error: processing vignette 'introduction.Rmd' failed with diagnostics:
    ℹ In argument: `stat_label = stat_label_match(.data$stat_display)`.
    Caused by error:
    ! attempt to select less than one element in integerOneIndex
    --- failed re-building ‘introduction.Rmd’
    
    --- re-building ‘regression.Rmd’ using rmarkdown
    ...
    ℹ In argument: `stat_label = stat_label_match(.data$stat_display)`.
    Caused by error:
    ! attempt to select less than one element in integerOneIndex
    --- failed re-building ‘regression.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘introduction.Rmd’ ‘regression.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘Hmisc’ ‘sjPlot’
      All declared Imports should be used.
    ```

# PupillometryR

<details>

* Version: 0.0.4
* GitHub: https://github.com/samhforbes/PupillometryR
* Source code: https://github.com/cran/PupillometryR
* Date/Publication: 2021-09-19 13:20:15 UTC
* Number of recursive dependencies: 85

Run `revdepcheck::cloud_details(, "PupillometryR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘PupillometryR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.Pupil_test_data
    > ### Title: Pre-prepared plots of PupillometryR data
    > ### Aliases: plot.Pupil_test_data
    > 
    > ### ** Examples
    > 
    > Sdata <- make_pupillometryr_data(data = pupil_data,
    ...
     14. │     ├─dplyr::group_vars(x)
     15. │     └─dplyr:::group_vars.data.frame(x)
     16. │       ├─generics::setdiff(names(group_data(x)), ".rows")
     17. │       ├─dplyr::group_data(x)
     18. │       └─dplyr:::group_data.data.frame(x)
     19. │         └─vctrs::vec_size(.data)
     20. └─vctrs:::stop_scalar_type(`<fn>`(`<Ppl_dff_[,3]>`), "x", `<env>`)
     21.   └─vctrs:::stop_vctrs(...)
     22.     └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘PupillometryR.Rmd’ using rmarkdown
    Quitting from lines 319-325 (PupillometryR.Rmd) 
    Error: processing vignette 'PupillometryR.Rmd' failed with diagnostics:
    `x` must be a vector, not a <Pupil_difference_data/PupillometryR/data.frame/Pupil_function_data> object.
    --- failed re-building ‘PupillometryR.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘PupillometryR.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘data.table’ ‘itsadug’ ‘lazyeval’ ‘mgcv’
      All declared Imports should be used.
    ```

# redist

<details>

* Version: 4.0.1
* GitHub: https://github.com/alarm-redist/redist
* Source code: https://github.com/cran/redist
* Date/Publication: 2022-06-16 06:20:07 UTC
* Number of recursive dependencies: 150

Run `revdepcheck::cloud_details(, "redist")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in documentation object 'compare_plans.Rd':
      ‘[dplyr:dplyr_data_masking]{<data-masking>}’
    
    Missing link or links in documentation object 'number_by.Rd':
      ‘[dplyr:dplyr_data_masking]{<data-masking>}’
    
    Missing link or links in documentation object 'plot.redist_map.Rd':
      ‘[dplyr:dplyr_data_masking]{<data-masking>}’
    
    Missing link or links in documentation object 'prec_cooccurrence.Rd':
    ...
    Missing link or links in documentation object 'redist.plot.scatter.Rd':
      ‘[dplyr:dplyr_data_masking]{<data-masking>}’
    
    Missing link or links in documentation object 'redist.plot.trace.Rd':
      ‘[dplyr:dplyr_data_masking]{<data-masking>}’
    
    Missing link or links in documentation object 'redist_map.Rd':
      ‘[dplyr:dplyr_data_masking]{<data-masking>}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 35.8Mb
      sub-directories of 1Mb or more:
        data   1.2Mb
        libs  31.8Mb
    ```

# rWCVP

<details>

* Version: 1.2.4
* GitHub: https://github.com/matildabrown/rWCVP
* Source code: https://github.com/cran/rWCVP
* Date/Publication: 2023-02-16 15:20:02 UTC
* Number of recursive dependencies: 122

Run `revdepcheck::cloud_details(, "rWCVP")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      [ FAIL 1 | WARN 2 | SKIP 0 | PASS 65 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error ('test-fuzzy_match.R:18'): edit match returns expected output ─────────
      <dplyr:::mutate_error/rlang_error/error/condition>
      Error in `mutate(., match_info = map(cli_progress_along(.data$sanitised_, 
          "Matching"), ~edit_match_name_(.data$sanitised_[.x], wcvp_names)))`: i In argument: `match_info = map(...)`.
      Caused by error in `map()`:
      i In index: 2.
      Caused by error:
      ! attempt to select less than one element in integerOneIndex
      
      [ FAIL 1 | WARN 2 | SKIP 0 | PASS 65 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 16 marked UTF-8 strings
    ```

# saeSim

<details>

* Version: 0.11.0
* GitHub: https://github.com/wahani/saeSim
* Source code: https://github.com/cran/saeSim
* Date/Publication: 2022-02-07 16:40:02 UTC
* Number of recursive dependencies: 97

Run `revdepcheck::cloud_details(, "saeSim")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in documentation object 'sim_resp.Rd':
      ‘[dplyr:dplyr_data_masking]{data-masking}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

# sgsR

<details>

* Version: 1.4.0
* GitHub: https://github.com/tgoodbody/sgsR
* Source code: https://github.com/cran/sgsR
* Date/Publication: 2023-02-09 09:50:02 UTC
* Number of recursive dependencies: 119

Run `revdepcheck::cloud_details(, "sgsR")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
        # Now:
        data %>% select(all_of(X))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>. 
      2: Using an external vector in selections was deprecated in tidyselect 1.1.0.
      ℹ Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(Y)
      
        # Now:
        data %>% select(all_of(Y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>. 
      Execution halted
    ```

## In both

*   checking for executable files ... WARNING
    ```
    Found the following executable files:
      inst/extdata/access.dbf
      inst/extdata/existing.dbf
      inst/extdata/existingna.dbf
    Source packages should not contain undeclared executable files.
    See section ‘Package structure’ in the ‘Writing R Extensions’ manual.
    ```

# srvyr

<details>

* Version: 1.2.0
* GitHub: https://github.com/gergness/srvyr
* Source code: https://github.com/cran/srvyr
* Date/Publication: 2023-02-21 04:10:02 UTC
* Number of recursive dependencies: 99

Run `revdepcheck::cloud_details(, "srvyr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘srvyr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: cascade
    > ### Title: Summarise multiple values into cascading groups
    > ### Aliases: cascade
    > 
    > ### ** Examples
    > 
    > library(survey)
    ...
     15. │                 └─mask$eval_all_summarise(quo)
     16. │                   └─dplyr (local) eval()
     17. ├─srvyr::survey_mean()
     18. │ └─srvyr::survey_prop(...)
     19. │   └─srvyr:::peeled_cur_group_id(.full_svy, cur_group())
     20. └─base::.handleSimpleError(`<fn>`, "subscript out of bounds", base::quote(cur_peel_group$grp_rows[[1]]))
     21.   └─dplyr (local) h(simpleError(msg, call))
     22.     └─dplyr (local) handler(cnd)
     23.       └─rlang::abort(message, class = error_class, parent = parent, call = error_call)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       12. │             └─dplyr (local) eval()
       13. ├─srvyr::survey_mean()
       14. │ └─srvyr::survey_prop(...)
       15. │   └─srvyr:::peeled_cur_group_id(.full_svy, cur_group())
       16. └─base::.handleSimpleError(`<fn>`, "subscript out of bounds", base::quote(cur_peel_group$grp_rows[[1]]))
       17.   └─dplyr (local) h(simpleError(msg, call))
       18.     └─dplyr (local) handler(cnd)
       19.       └─rlang::abort(message, class = error_class, parent = parent, call = error_call)
      ── Failure ('test_survey_statistics.r:683'): unweighted allows passing functions from environment ──
      `test1` not equal to `test_reference`.
      Component "n": Mean relative difference: 5.956522
      
      [ FAIL 10 | WARN 2 | SKIP 0 | PASS 345 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘extending-srvyr.Rmd’ using rmarkdown
    --- finished re-building ‘extending-srvyr.Rmd’
    
    --- re-building ‘srvyr-database.Rmd’ using rmarkdown
    --- finished re-building ‘srvyr-database.Rmd’
    
    --- re-building ‘srvyr-vs-survey.Rmd’ using rmarkdown
    Quitting from lines 160-169 (srvyr-vs-survey.Rmd) 
    ...
    ℹ In argument: `proportion = survey_mean()`.
    Caused by error in `cur_peel_group$grp_rows[[1]]`:
    ! subscript out of bounds
    --- failed re-building ‘srvyr-vs-survey.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘srvyr-vs-survey.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘convey’
    ```

# stRoke

<details>

* Version: 23.1.7
* GitHub: https://github.com/agdamsbo/stRoke
* Source code: https://github.com/cran/stRoke
* Date/Publication: 2023-01-24 10:20:09 UTC
* Number of recursive dependencies: 98

Run `revdepcheck::cloud_details(, "stRoke")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘stRoke-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: generic_stroke
    > ### Title: Generic stroke study outcome
    > ### Aliases: generic_stroke
    > 
    > ### ** Examples
    > 
    > generic_stroke(df = stRoke::talos, group = "rtreat", score = "mrs_6", 
    ...
     25. │   └─vctrs::vec_size_common(...)
     26. ├─base::unlist(stat_display)
     27. ├─stat_display
     28. ├─rlang:::`$.rlang_data_pronoun`(.data, stat_display)
     29. │ └─rlang:::data_pronoun_get(...)
     30. ├─`<fn>`()
     31. └─base::.handleSimpleError(...)
     32.   └─dplyr (local) h(simpleError(msg, call))
     33.     └─rlang::abort(message, class = error_class, parent = parent, call = error_call)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       23. ├─stringr::str_extract_all(unlist(stat_display), "\\{.*?\\}")
       24. │ └─stringr:::check_lengths(string, pattern)
       25. │   └─vctrs::vec_size_common(...)
       26. ├─base::unlist(stat_display)
       27. ├─stat_display
       28. ├─rlang:::`$.rlang_data_pronoun`(.data, stat_display)
       29. │ └─rlang:::data_pronoun_get(...)
       30. ├─`<fn>`()
       31. └─base::.handleSimpleError(...)
       32.   └─dplyr (local) h(simpleError(msg, call))
       33.     └─rlang::abort(message, class = error_class, parent = parent, call = error_call)
      
      [ FAIL 1 | WARN 8 | SKIP 0 | PASS 62 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘toolbox.Rmd’ using rmarkdown
    Quitting from lines 113-117 (toolbox.Rmd) 
    Error: processing vignette 'toolbox.Rmd' failed with diagnostics:
    ℹ In argument: `stat_label = stat_label_match(.data$stat_display)`.
    Caused by error:
    ! attempt to select less than one element in integerOneIndex
    --- failed re-building ‘toolbox.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘toolbox.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# tbrf

<details>

* Version: 0.1.5
* GitHub: https://github.com/mps9506/tbrf
* Source code: https://github.com/cran/tbrf
* Date/Publication: 2020-04-09 04:40:02 UTC
* Number of recursive dependencies: 100

Run `revdepcheck::cloud_details(, "tbrf")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘tbrf-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: tbr_binom
    > ### Title: Time-Based Rolling Binomial Probability
    > ### Aliases: tbr_binom
    > 
    > ### ** Examples
    > 
    > ## Generate Sample Data
    ...
     17. │     ├─tbrf:::tbr_binom_window(...)
     18. │     │ └─tbrf::open_window(x, tcolumn, unit = unit, n, i)
     19. │     │   └─lubridate::interval(tcolumn[i], tcolumn)
     20. │     └─date
     21. ├─`<fn>`()
     22. └─base::.handleSimpleError(...)
     23.   └─purrr (local) h(simpleError(msg, call))
     24.     └─cli::cli_abort(...)
     25.       └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── Error ('test-expectedClass.R:25'): tbr_gmean returns tbl_df in tidy chain ───
      <dplyr:::mutate_error/rlang_error/error/condition>
      Error in `mutate(., `:=`("temp", purrr::map(row_number(), ~tbr_gmean_window(x = !!rlang::enquo(x), 
          tcolumn = !!rlang::enquo(tcolumn), unit = unit, n = n, i = .x, 
          conf = default_dots$conf, na.rm = default_dots$na.rm, type = default_dots$type, 
          R = default_dots$R, parallel = default_dots$parallel, ncpus = default_dots$ncpus, 
          cl = default_dots$cl))))`: ℹ In argument: `temp = purrr::map(...)`.
      Caused by error in `purrr::map()`:
      ℹ In index: 2.
      Caused by error:
      ! attempt to select less than one element in integerOneIndex
      
      [ FAIL 2 | WARN 3 | SKIP 0 | PASS 28 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘intro_to_tbrf.Rmd’ using rmarkdown
    Quitting from lines 104-115 (intro_to_tbrf.Rmd) 
    Error: processing vignette 'intro_to_tbrf.Rmd' failed with diagnostics:
    ℹ In argument: `temp = purrr::map(...)`.
    Caused by error in `purrr::map()`:
    ℹ In index: 2.
    Caused by error:
    ! attempt to select less than one element in integerOneIndex
    --- failed re-building ‘intro_to_tbrf.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘intro_to_tbrf.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# testdat

<details>

* Version: 0.4.1
* GitHub: https://github.com/socialresearchcentre/testdat
* Source code: https://github.com/cran/testdat
* Date/Publication: 2022-08-25 02:50:02 UTC
* Number of recursive dependencies: 80

Run `revdepcheck::cloud_details(, "testdat")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in documentation object 'chk-helper.Rd':
      ‘[dplyr:dplyr_data_masking]{data-masking}’
    
    Missing link or links in documentation object 'conditional-expectations.Rd':
      ‘[dplyr:dplyr_data_masking]{data-masking}’
    
    Missing link or links in documentation object 'data-params.Rd':
      ‘[dplyr:dplyr_data_masking]{data-masking}’
    
    Missing link or links in documentation object 'datacomp-expectations.Rd':
    ...
    Missing link or links in documentation object 'text-expectations.Rd':
      ‘[dplyr:dplyr_data_masking]{data-masking}’
    
    Missing link or links in documentation object 'uniqueness-expectations.Rd':
      ‘[dplyr:dplyr_data_masking]{data-masking}’
    
    Missing link or links in documentation object 'value-expectations.Rd':
      ‘[dplyr:dplyr_data_masking]{data-masking}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

# tidycat

<details>

* Version: 0.1.2
* GitHub: https://github.com/guyabel/tidycat
* Source code: https://github.com/cran/tidycat
* Date/Publication: 2021-08-02 04:20:01 UTC
* Number of recursive dependencies: 70

Run `revdepcheck::cloud_details(, "tidycat")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘intro.Rmd’ using rmarkdown
    Quitting from lines 77-81 (intro.Rmd) 
    Error: processing vignette 'intro.Rmd' failed with diagnostics:
    ℹ In argument: `term = d$term[n]`.
    Caused by error:
    ! attempt to select less than one element in integerOneIndex
    --- failed re-building ‘intro.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘intro.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tidyr’
      All declared Imports should be used.
    ```

# tidycmprsk

<details>

* Version: 0.2.0
* GitHub: https://github.com/MSKCC-Epi-Bio/tidycmprsk
* Source code: https://github.com/cran/tidycmprsk
* Date/Publication: 2022-10-03 07:20:02 UTC
* Number of recursive dependencies: 103

Run `revdepcheck::cloud_details(, "tidycmprsk")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       12. │   └─dplyr:::mutate_col(dots[[i]], data, mask, new_columns)
       13. │     └─mask$eval_all_mutate(quo)
       14. │       └─dplyr (local) eval()
       15. ├─... %>% list()
       16. ├─dplyr::filter(data, ttdeath <= time[2]) %>% nrow()
       17. ├─base::nrow(.)
       18. ├─dplyr::filter(data, ttdeath <= time[2])
       19. ├─`<fn>`()
       20. └─base::.handleSimpleError(...)
       21.   └─dplyr (local) h(simpleError(msg, call))
       22.     └─rlang::abort(message, class = error_class, parent = parent, call = error_call)
      
      [ FAIL 1 | WARN 4 | SKIP 0 | PASS 83 ]
      Error: Test failures
      Execution halted
    ```

# tidyquant

<details>

* Version: 1.0.6
* GitHub: https://github.com/business-science/tidyquant
* Source code: https://github.com/cran/tidyquant
* Date/Publication: 2022-11-16 12:10:06 UTC
* Number of recursive dependencies: 175

Run `revdepcheck::cloud_details(, "tidyquant")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        9. │ └─dplyr:::dplyr_quosures(...)
       10. │   └─rlang::quos(..., .ignore_empty = "all")
       11. └─rlang::parse_quo(rlang::quo_name(function_exprs[[i]]), env = rlang::caller_env())
       12.   ├─rlang::new_quosure(parse_expr(x), as_environment(env))
       13.   └─rlang::parse_expr(x)
       14.     └─base::parse(text = paste_line(x))
      
      [ FAIL 1 | WARN 0 | SKIP 2 | PASS 89 ]
      Error: Test failures
      In addition: Warning messages:
      1: In for (i in seq_along(dots)) { :
        closing unused connection 5 (https://fred.stlouisfed.org/series/XYZ/downloaddata/XYZ.csv)
      2: In for (i in seq_along(dots)) { :
        closing unused connection 4 (https://fred.stlouisfed.org/series/XYZ/downloaddata/XYZ.csv)
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        doc   4.4Mb
    ```

# tidyterra

<details>

* Version: 0.3.2
* GitHub: https://github.com/dieghernan/tidyterra
* Source code: https://github.com/cran/tidyterra
* Date/Publication: 2023-02-24 11:00:03 UTC
* Number of recursive dependencies: 91

Run `revdepcheck::cloud_details(, "tidyterra")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in documentation object 'slice.Spat.Rd':
      ‘[dplyr:dplyr_data_masking]{data-masking}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

# tidytransit

<details>

* Version: 1.4.1
* GitHub: https://github.com/r-transit/tidytransit
* Source code: https://github.com/cran/tidytransit
* Date/Publication: 2023-02-01 12:40:02 UTC
* Number of recursive dependencies: 101

Run `revdepcheck::cloud_details(, "tidytransit")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-read-gtfs.R:85'): files parameter ────────────────────────────
      Expected `read_gtfs(path, files = f)` to run without any warnings.
      ℹ Actually got a <dplyr_warning_join_relationship_many_to_many>:
        Warning in `dplyr::full_join()`:
        Detected an unexpected many-to-many relationship between `x` and `y`.
        ℹ Row 6 of `x` matches multiple rows in `y`.
        ℹ Row 1 of `y` matches multiple rows in `x`.
        ℹ If a many-to-many relationship is expected, set `relationship =
          "many-to-many"` to silence this warning.
      
      [ FAIL 1 | WARN 19 | SKIP 6 | PASS 222 ]
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

# timetk

<details>

* Version: 2.8.2
* GitHub: https://github.com/business-science/timetk
* Source code: https://github.com/cran/timetk
* Date/Publication: 2022-11-17 19:30:02 UTC
* Number of recursive dependencies: 226

Run `revdepcheck::cloud_details(, "timetk")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in documentation object 'slice_period.Rd':
      ‘[dplyr:dplyr_data_masking]{data-masking}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

## In both

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      Attaching package: 'PerformanceAnalytics'
      
      The following object is masked from 'package:graphics':
      
          legend
      
      Loading required package: quantmod
      Loading required package: TTR
      > 
      > # Forecast objects
      > library(forecast)
      > library(robets)
      Error in library(robets) : there is no package called 'robets'
      Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘robets’
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2750 marked UTF-8 strings
    ```

# vctrs

<details>

* Version: 0.5.2
* GitHub: https://github.com/r-lib/vctrs
* Source code: https://github.com/cran/vctrs
* Date/Publication: 2023-01-23 11:20:02 UTC
* Number of recursive dependencies: 71

Run `revdepcheck::cloud_details(, "vctrs")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       1. └─base::loadNamespace(x) at test-type-dplyr.R:155:2
       2.   ├─base::namespaceImport(...)
       3.   └─base::loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]])
      ── Error ('test-type-dplyr.R:166'): common type between rowwise and grouped data frames is a bare df ──
      Error in `loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]])`: namespace 'vctrs' 0.5.2 is already loaded, but >= 0.5.2.9000 is required
      Backtrace:
          ▆
       1. ├─vctrs::vec_ptype_common(...) at test-type-dplyr.R:166:2
       2. └─base::loadNamespace(x)
       3.   ├─base::namespaceImport(...)
       4.   └─base::loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]])
      
      [ FAIL 13 | WARN 33 | SKIP 271 | PASS 5108 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        libs   3.6Mb
    ```

# whomds

<details>

* Version: 1.1.0
* GitHub: https://github.com/lindsayevanslee/whomds
* Source code: https://github.com/cran/whomds
* Date/Publication: 2022-05-27 09:30:02 UTC
* Number of recursive dependencies: 122

Run `revdepcheck::cloud_details(, "whomds")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘whomds-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: table_weightedpct
    > ### Title: Calculate table of percentages or N of response distribution for
    > ###   survey items, survey weighted, disaggregated
    > ### Aliases: table_weightedpct
    > 
    > ### ** Examples
    > 
    ...
     13. │           └─mask$eval_all_summarise(quo)
     14. │             └─dplyr (local) eval()
     15. ├─srvyr::survey_mean(na.rm = TRUE)
     16. │ └─srvyr::survey_prop(...)
     17. │   └─srvyr:::peeled_cur_group_id(.full_svy, cur_group())
     18. └─base::.handleSimpleError(`<fn>`, "subscript out of bounds", base::quote(cur_peel_group$grp_rows[[1]]))
     19.   └─dplyr (local) h(simpleError(msg, call))
     20.     └─dplyr (local) handler(cnd)
     21.       └─rlang::abort(message, class = error_class, parent = parent, call = error_call)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       11. │       └─base::lapply(.x, .f, ...)
       12. │         └─dplyr (local) FUN(X[[i]], ...)
       13. │           └─mask$eval_all_summarise(quo)
       14. │             └─dplyr (local) eval()
       15. ├─srvyr::survey_mean(na.rm = TRUE)
       16. │ └─srvyr::survey_prop(...)
       17. │   └─srvyr:::peeled_cur_group_id(.full_svy, cur_group())
       18. └─base::.handleSimpleError(`<fn>`, "subscript out of bounds", base::quote(cur_peel_group$grp_rows[[1]]))
       19.   └─dplyr (local) h(simpleError(msg, call))
       20.     └─dplyr (local) handler(cnd)
       21.       └─rlang::abort(message, class = error_class, parent = parent, call = error_call)
      
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 2 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘c1_background_EN.Rmd’ using rmarkdown
    --- finished re-building ‘c1_background_EN.Rmd’
    
    --- re-building ‘c1_background_ES.Rmd’ using rmarkdown
    --- finished re-building ‘c1_background_ES.Rmd’
    
    --- re-building ‘c2_getting_started_EN.Rmd’ using rmarkdown
    --- finished re-building ‘c2_getting_started_EN.Rmd’
    
    ...
    ℹ In argument: `prop = survey_mean(na.rm = TRUE)`.
    Caused by error in `cur_peel_group$grp_rows[[1]]`:
    ! subscript out of bounds
    --- failed re-building ‘c6_after_rasch_ES.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘c6_after_rasch_EN.Rmd’ ‘c6_after_rasch_ES.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# yamlet

<details>

* Version: 0.10.10
* GitHub: https://github.com/bergsmat/yamlet
* Source code: https://github.com/cran/yamlet
* Date/Publication: 2023-02-17 15:40:02 UTC
* Number of recursive dependencies: 93

Run `revdepcheck::cloud_details(, "yamlet")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in documentation object 'arrange.decorated.Rd':
      ‘[dplyr:dplyr_data_masking]{data-masking}’
    
    Missing link or links in documentation object 'mutate.decorated.Rd':
      ‘[dplyr:dplyr_data_masking]{data-masking}’
    
    Missing link or links in documentation object 'slice.decorated.Rd':
      ‘[dplyr:dplyr_data_masking]{data-masking}’
    
    Missing link or links in documentation object 'summarise.decorated.Rd':
      ‘[dplyr:dplyr_data_masking]{data-masking}’
    
    Missing link or links in documentation object 'summarize.decorated.Rd':
      ‘[dplyr:dplyr_data_masking]{data-masking}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

