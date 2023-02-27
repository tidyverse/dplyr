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

# isotracer

<details>

* Version: 1.1.3
* GitHub: NA
* Source code: https://github.com/cran/isotracer
* Date/Publication: 2022-03-27 14:30:02 UTC
* Number of recursive dependencies: 155

Run `revdepcheck::cloud_details(, "isotracer")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      `plot(p)` threw an error.
      Message: object 'p' not found
      Class:   simpleError/error/condition
      Backtrace:
          ▆
       1. ├─testthat::expect_error(plot(p), NA) at test-integration.R:207:12
       2. │ └─testthat:::quasi_capture(...)
       3. │   ├─testthat (local) .capture(...)
       4. │   │ └─base::withCallingHandlers(...)
       5. │   └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
       6. └─base::plot(p)
      
      [ FAIL 2 | WARN 6 | SKIP 0 | PASS 285 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 79.7Mb
      sub-directories of 1Mb or more:
        doc    2.9Mb
        libs  74.8Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
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

# rgho

<details>

* Version: 3.0.0
* GitHub: https://github.com/aphp/rgho
* Source code: https://github.com/cran/rgho
* Date/Publication: 2022-08-30 13:30:05 UTC
* Number of recursive dependencies: 61

Run `revdepcheck::cloud_details(, "rgho")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      ══ Skipped tests ═══════════════════════════════════════════════════════════════
      • On CRAN (2)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-get_gho.R:36'): Connection errors ────────────────────────────
      `get_gho_dimensions()` produced unexpected messages.
      Expected match: 404
      Actual values:
      * Server error: (504) Gateway Timeout
      
      
      [ FAIL 1 | WARN 0 | SKIP 2 | PASS 2 ]
      Error: Test failures
      Execution halted
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

