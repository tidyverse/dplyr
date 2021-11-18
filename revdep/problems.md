# arrow

<details>

* Version: 6.0.0.2
* GitHub: https://github.com/apache/arrow
* Source code: https://github.com/cran/arrow
* Date/Publication: 2021-10-27 21:20:06 UTC
* Number of recursive dependencies: 66

Run `cloud_details(, "arrow")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        7. │ │     └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
        8. │ └─rlang::eval_tidy(expr, rlang::new_data_mask(rlang::env(input = record_batch(tbl))))
        9. ├─input %>% filter(chr == b_var)
       10. ├─dplyr::filter(., chr == b_var)
       11. └─arrow:::filter.ArrowTabular(., chr == b_var)
       12.   └─base::lapply(filts, arrow_eval, arrow_mask(.data))
       13.     └─arrow FUN(X[[i]], ...)
       14.       └─base::tryCatch(...)
       15.         └─base tryCatchList(expr, classes, parentenv, handlers)
       16.           └─base tryCatchOne(expr, names, parentenv, handlers[[1L]])
       17.             └─value[[3L]](cond)
      
      [ FAIL 1 | WARN 3 | SKIP 58 | PASS 5783 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 78.5Mb
      sub-directories of 1Mb or more:
        R      3.7Mb
        libs  73.9Mb
    ```

# cvms

<details>

* Version: 1.3.3
* GitHub: https://github.com/ludvigolsen/cvms
* Source code: https://github.com/cran/cvms
* Date/Publication: 2021-11-14 17:20:02 UTC
* Number of recursive dependencies: 128

Run `cloud_details(, "cvms")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Expected match: "missing values and NaNs not allowed if narm is FALSE"
      Actual message: "Problem while computing anum1 structurefunction x 1 y 2 1"
      Backtrace:
          ▆
       1. ├─testthat::expect_error(...) at test_summarize_metrics.R:108:2
       2. │ └─testthat:::quasi_capture(...)
       3. │   ├─testthat .capture(...)
       4. │   │ └─base::withCallingHandlers(...)
       5. │   └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
       6. └─xpectr::strip_msg(...)
       7.   └─xpectr::stop_if(...)
      
      [ FAIL 1 | WARN 0 | SKIP 70 | PASS 3622 ]
      Error: Test failures
      Execution halted
    ```

# datacleanr

<details>

* Version: 1.0.2
* GitHub: https://github.com/the-Hull/datacleanr
* Source code: https://github.com/cran/datacleanr
* Date/Publication: 2021-11-03 05:20:02 UTC
* Number of recursive dependencies: 102

Run `cloud_details(, "datacleanr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        5. │   └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
        6. ├─datacleanr:::filter_scoped(iris, "Spec == 'setosa'", scope_at = NULL)
        7. │ ├─dplyr::group_by(...)
        8. │ ├─dplyr::filter(dplyr::ungroup(dframe), eval(statement), na.rm = TRUE)
        9. │ └─dplyr:::filter.data.frame(...)
       10. │   └─dplyr:::filter_rows(.data, ..., caller_env = caller_env())
       11. │     └─dplyr:::filter_eval(dots, mask = mask, error_call = error_call)
       12. │       ├─base::withCallingHandlers(...)
       13. │       └─mask$eval_all_filter(dots, env_filter)
       14. └─base::eval(statement)
       15.   └─base::eval(statement)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 46 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘htmltools’
      All declared Imports should be used.
    ```

# dm

<details>

* Version: 0.2.5
* GitHub: https://github.com/cynkra/dm
* Source code: https://github.com/cran/dm
* Date/Publication: 2021-10-15 20:30:02 UTC
* Number of recursive dependencies: 135

Run `cloud_details(, "dm")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      • only works on `mssql`, `postgres` (6)
      • only works on `postgres` (2)
      • only works on `sqlite` (1)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test-rows-db.R:64:3): insert + delete with returning argument (#607) ──
      `... <- NULL` did not throw the expected warning.
      Backtrace:
          ▆
       1. └─testthat::expect_warning(...) at test-rows-db.R:64:2
       2.   └─testthat:::expect_condition_matching(...)
      
      [ FAIL 1 | WARN 0 | SKIP 124 | PASS 699 ]
      Error: Test failures
      Execution halted
    ```

# dtplyr

<details>

* Version: 1.1.0
* GitHub: https://github.com/tidyverse/dtplyr
* Source code: https://github.com/cran/dtplyr
* Date/Publication: 2021-02-20 01:50:05 UTC
* Number of recursive dependencies: 61

Run `cloud_details(, "dtplyr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘dtplyr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: filter.dtplyr_step
    > ### Title: Subset rows using column values
    > ### Aliases: filter.dtplyr_step
    > 
    > ### ** Examples
    > 
    > library(dplyr, warn.conflicts = FALSE)
    ...
    
    # Use as.data.table()/as.data.frame()/as_tibble() to access results
    > 
    > dt %>%
    +   group_by(cyl) %>%
    +   filter(mpg > mean(mpg))
    Error in step_subset(parent, i = i) : 
      is.null(i) || is_expression(i) || is_step(i) is not TRUE
    Calls: %>% ... filter.dtplyr_step -> step_subset_i -> step_subset -> stopifnot
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        1. ├─testthat::expect_equal(...) at test-tidyeval.R:202:2
        2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
        3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
        4. ├─dt %>% summarise_at(vars(x), add) %>% pull()
        5. ├─dplyr::pull(.)
        6. └─dplyr::summarise_at(., vars(x), add)
        7.   ├─dplyr::summarise(.tbl, !!!funs)
        8.   └─dtplyr:::summarise.dtplyr_step(.tbl, !!!funs)
        9.     └─dtplyr:::step_subset_j(...)
       10.       └─dtplyr:::step_subset(...)
       11.         └─base::stopifnot(is.null(j) || is_expression(j))
      
      [ FAIL 6 | WARN 0 | SKIP 12 | PASS 335 ]
      Error: Test failures
      Execution halted
    ```

# dynplot

<details>

* Version: 1.1.1
* GitHub: https://github.com/dynverse/dynplot
* Source code: https://github.com/cran/dynplot
* Date/Publication: 2021-06-28 06:30:02 UTC
* Number of recursive dependencies: 129

Run `cloud_details(, "dynplot")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘dynplot-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_heatmap
    > ### Title: Plot expression data along a trajectory
    > ### Aliases: plot_heatmap
    > ### Keywords: plot_trajectory
    > 
    > ### ** Examples
    > 
    ...
     16. │                   └─vctrs `<fn>`()
     17. │                     └─vctrs:::validate_unique(names = names, arg = arg)
     18. │                       └─vctrs:::stop_names_must_be_unique(names, arg)
     19. │                         └─vctrs:::stop_names(...)
     20. │                           └─vctrs:::stop_vctrs(class = c(class, "vctrs_error_names"), ...)
     21. ├─dplyr::mutate(., level = NA, direct = near(.data$x_diff, linearised$margin))
     22. ├─dplyr::arrange(., .data$x_diff)
     23. ├─dplyr::mutate(., x_diff = abs(.data$x_to - .data$x_from))
     24. └─dplyr::filter(., .data$from == .data$to, .data$x_from != .data$x_to)
    Execution halted
    ```

# ergm.ego

<details>

* Version: 1.0.0
* GitHub: https://github.com/statnet/ergm.ego
* Source code: https://github.com/cran/ergm.ego
* Date/Publication: 2021-06-23 07:00:04 UTC
* Number of recursive dependencies: 74

Run `cloud_details(, "ergm.ego")` for more info

</details>

## Newly broken

*   checking contents of ‘data’ directory ... WARNING
    ```
    Output for data("fmhfit", package = "ergm.ego"):
      Search path was changed
    ```

# fable

<details>

* Version: 0.3.1
* GitHub: https://github.com/tidyverts/fable
* Source code: https://github.com/cran/fable
* Date/Publication: 2021-05-16 14:20:07 UTC
* Number of recursive dependencies: 103

Run `cloud_details(, "fable")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘fable-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: refit.AR
    > ### Title: Refit an AR model
    > ### Aliases: refit.AR
    > 
    > ### ** Examples
    > 
    > lung_deaths_male <- as_tsibble(mdeaths)
    ...
    
    sigma^2 estimated as 37672
    AIC = -93.6	AICc = -89.2	BIC = -68.55> 
    > fit %>%
    +   refit(lung_deaths_female) %>%
    +   report()
    Error in UseMethod("report") : 
      no applicable method for 'report' applied to an object of class "c('tbl_df', 'tbl', 'data.frame')"
    Calls: %>% -> report
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
       3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
       4. └─generics::tidy(refit(fable_fit, USAccDeaths_tbl))
      ── Error (test-lm.R:75:3): LM ──────────────────────────────────────────────────
      Error in `UseMethod("tidy")`: no applicable method for 'tidy' applied to an object of class "c('tbl_df', 'tbl', 'data.frame')"
      Backtrace:
          ▆
       1. ├─testthat::expect_identical(...) at test-lm.R:75:2
       2. │ └─testthat::quasi_label(enquo(expected), expected.label, arg = "expected")
       3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
       4. └─generics::tidy(refit(fable_fit, USAccDeaths_tbl))
      
      [ FAIL 3 | WARN 2 | SKIP 1 | PASS 90 ]
      Error: Test failures
      Execution halted
    ```

# fabletools

<details>

* Version: 0.3.1
* GitHub: https://github.com/tidyverts/fabletools
* Source code: https://github.com/cran/fabletools
* Date/Publication: 2021-03-16 22:10:03 UTC
* Number of recursive dependencies: 96

Run `cloud_details(, "fabletools")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘fabletools-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: reconcile
    > ### Title: Forecast reconciliation
    > ### Aliases: reconcile reconcile.mdl_df
    > 
    > ### ** Examples
    > 
    > if (requireNamespace("fable", quietly = TRUE)) {
    ...
    + 
    + lung_deaths_agg %>%
    +   model(lm = TSLM(value ~ trend() + season())) %>%
    +   reconcile(lm = min_trace(lm)) %>% 
    +   forecast()
    + }
    Error in UseMethod("forecast") : 
      no applicable method for 'forecast' applied to an object of class "c('tbl_df', 'tbl', 'data.frame')"
    Calls: %>% -> forecast
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       1. ├─testthat::expect_equal(...) at test-combination.R:9:2
       2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
       3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
       4. ├─dplyr::select(augment(mbl_cmbn), -.model, -.innov)
       5. └─generics::augment(mbl_cmbn)
      ── Error (test-reconciliation.R:26:3): reconciliation ──────────────────────────
      Error in `UseMethod("forecast")`: no applicable method for 'forecast' applied to an object of class "c('tbl_df', 'tbl', 'data.frame')"
      Backtrace:
          ▆
       1. ├─fit_agg %>% reconcile(snaive = min_trace(snaive)) %>% ... at test-reconciliation.R:26:2
       2. └─fabletools::forecast(.)
      
      [ FAIL 2 | WARN 0 | SKIP 1 | PASS 280 ]
      Error: Test failures
      Execution halted
    ```

# gestalt

<details>

* Version: 0.1.8
* GitHub: https://github.com/egnha/gestalt
* Source code: https://github.com/cran/gestalt
* Date/Publication: 2019-06-27 08:20:03 UTC
* Number of recursive dependencies: 47

Run `cloud_details(, "gestalt")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error (test-functions.R:65:3): body can be a closure ────────────────────────
      Error: Body must be an expression or closure
      Backtrace:
          ▆
       1. ├─testthat::expect_equal(foo, fn(x ~ function(y) x + y)) at test-functions.R:65:2
       2. │ └─testthat::quasi_label(enquo(expected), expected.label, arg = "expected")
       3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
       4. └─gestalt::fn(x ~ function(y) x + y)
       5.   └─gestalt make_fn(fun$args, fun$body, ..env)
       6.     └─is_expression(body) %because% ...
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 1040 ]
      Error: Test failures
      Execution halted
    ```

# immunarch

<details>

* Version: 0.6.7
* GitHub: https://github.com/immunomind/immunarch
* Source code: https://github.com/cran/immunarch
* Date/Publication: 2021-10-29 12:00:07 UTC
* Number of recursive dependencies: 204

Run `cloud_details(, "immunarch")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘immunarch-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: pubRepStatistics
    > ### Title: Statistics of number of public clonotypes for each possible
    > ###   combinations of repertoires
    > ### Aliases: pubRepStatistics
    > 
    > ### ** Examples
    > 
    > data(immdata)
    > immdata$data <- lapply(immdata$data, head, 2000)
    > pr <- pubRep(immdata$data, .verbose = FALSE)
    > pubRepStatistics(pr) %>% vis()
    Error in step_subset(parent, i = i) : 
      is.null(i) || is_expression(i) || is_step(i) is not TRUE
    Calls: %>% ... filter.dtplyr_step -> step_subset_i -> step_subset -> stopifnot
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.5Mb
      sub-directories of 1Mb or more:
        data   4.3Mb
        doc    1.6Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dbplyr’
      All declared Imports should be used.
    ```

# isoreader

<details>

* Version: 1.3.1
* GitHub: https://github.com/isoverse/isoreader
* Source code: https://github.com/cran/isoreader
* Date/Publication: 2021-10-15 20:20:02 UTC
* Number of recursive dependencies: 110

Run `cloud_details(, "isoreader")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        6. ├─dplyr::filter(iso_files, dne == 5)
        7. ├─isoreader:::filter.iso_file_list(iso_files, dne == 5)
        8. │ ├─isoreader::iso_filter_files(.data, ..., quiet = TRUE)
        9. │ └─isoreader:::iso_filter_files.iso_file_list(.data, ..., quiet = TRUE)
       10. │   └─iso_get_file_info(iso_files, quiet = TRUE) %>% ...
       11. ├─dplyr::filter(., ...)
       12. └─dplyr:::filter.data.frame(., ...)
       13.   └─dplyr:::filter_rows(.data, ..., caller_env = caller_env())
       14.     └─dplyr:::filter_eval(dots, mask = mask, error_call = error_call)
       15.       ├─base::withCallingHandlers(...)
       16.       └─mask$eval_all_filter(dots, env_filter)
      
      [ FAIL 1 | WARN 0 | SKIP 9 | PASS 860 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Missing or unexported object: ‘glue::collapse’
    ```

# keyholder

<details>

* Version: 0.1.5
* GitHub: https://github.com/echasnovski/keyholder
* Source code: https://github.com/cran/keyholder
* Date/Publication: 2020-05-09 11:20:02 UTC
* Number of recursive dependencies: 56

Run `cloud_details(, "keyholder")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        4. └─magrittr .f1(.)
        5.   └─magrittr::freduce(value, `_function_list`)
        6.     ├─base::withVisible(function_list[[k]](value))
        7.     └─function_list[[k]](value)
        8.       └─dplyr::transmute_at(., vars(mpg, hp), as.integer)
        9.         ├─dplyr::transmute(.tbl, !!!funs)
       10.         └─dplyr:::transmute.data.frame(.tbl, !!!funs)
       11.           └─dplyr:::dplyr_col_select(out, cols_retain)
       12.             ├─.data[loc]
       13.             └─keyholder:::`[.keyed_df`(.data, loc)
       14.               └─keyholder::`keys<-`(`*tmp*`, value = keys(x)[i, , drop = FALSE])
      
      [ FAIL 4 | WARN 0 | SKIP 0 | PASS 301 ]
      Error: Test failures
      Execution halted
    ```

# manymodelr

<details>

* Version: 0.3.7
* GitHub: https://github.com/Nelson-Gon/manymodelr
* Source code: https://github.com/cran/manymodelr
* Date/Publication: 2021-11-15 09:20:09 UTC
* Number of recursive dependencies: 112

Run `cloud_details(, "manymodelr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       16. ├─dplyr::mutate(...)
       17. ├─dplyr:::mutate.data.frame(...)
       18. │ └─dplyr:::mutate_cols(.data, dplyr_quosures(...), caller_env = caller_env())
       19. │   ├─base::withCallingHandlers(...)
       20. │   ├─base::withCallingHandlers(...)
       21. │   └─mask$eval_all_mutate(quo)
       22. ├─manymodelr `<rlng_lm_>`(Val)
       23. │ └─manymodelr:::na_replace.character(., how = how, value = value)
       24. │   └─base::stop("how should be one of ffill, samples, value or get_mode.")
       25. └─base::.handleSimpleError(...)
       26.   └─dplyr h(simpleError(msg, call))
      
      [ FAIL 2 | WARN 4 | SKIP 0 | PASS 94 ]
      Error: Test failures
      Execution halted
    ```

# mcp

<details>

* Version: 0.3.1
* GitHub: https://github.com/lindeloev/mcp
* Source code: https://github.com/cran/mcp
* Date/Publication: 2021-11-17 16:50:02 UTC
* Number of recursive dependencies: 102

Run `cloud_details(, "mcp")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── Failure (helper-runs.R:325:7): good_poisson:
          y ~ 1 + ar(1), ~1 + x + ar(2, 1 + x + I(x^3)) ──
      stringr::str_starts(error_message, expected_error_poisson) is not TRUE
      
      `actual`:   FALSE
      `expected`: TRUE 
      Backtrace:
          ▆
       1. └─mcp:::test_runs(model, ...) at tests/testthat/helper-runs.R:325:6
       2.   └─mcp:::test_pp_eval(fit) at tests/testthat/helper-runs.R:113:6
       3.     └─testthat::expect_true(stringr::str_starts(error_message, expected_error_poisson)) at tests/testthat/helper-runs.R:293:6
      
      [ FAIL 5 | WARN 0 | SKIP 6 | PASS 3625 ]
      Error: Test failures
      Execution halted
    ```

# microeco

<details>

* Version: 0.6.0
* GitHub: NA
* Source code: https://github.com/cran/microeco
* Date/Publication: 2021-11-16 09:10:02 UTC
* Number of recursive dependencies: 190

Run `cloud_details(, "microeco")` for more info

</details>

## Newly broken

*   checking contents of ‘data’ directory ... WARNING
    ```
    Output for data("dataset", package = "microeco"):
      Search path was changed
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘ggtree’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘reshape2’
      All declared Imports should be used.
    ```

# MoMPCA

<details>

* Version: 1.0.1
* GitHub: NA
* Source code: https://github.com/cran/MoMPCA
* Date/Publication: 2021-01-21 13:10:03 UTC
* Number of recursive dependencies: 84

Run `cloud_details(, "MoMPCA")` for more info

</details>

## Newly broken

*   checking tests ...ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(MoMPCA)
      > 
      > test_check("MoMPCA")
      Killed
      sh: 1: Cannot fork
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 16 marked UTF-8 strings
    ```

# motif

<details>

* Version: 0.5.0
* GitHub: https://github.com/Nowosad/motif
* Source code: https://github.com/cran/motif
* Date/Publication: 2021-08-23 12:50:02 UTC
* Number of recursive dependencies: 91

Run `cloud_details(, "motif")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > 
      > # prepare my fun ----------------------------------------------------------
      > my_fun = function(x) sum(!is.na(c(x[[1]])))
      > 
      > test_check("motif")
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error (test-lsp_add_clusters.R:47:1): (code run outside of `test_that()`) ───
      Error in `UseMethod("lsp_add_examples")`: no applicable method for 'lsp_add_examples' applied to an object of class "c('grouped_df', 'tbl_df', 'tbl', 'data.frame')"
      Backtrace:
          ▆
       1. └─motif::lsp_add_examples(x = landform_grid_sf_sel, y = landform) at test-lsp_add_clusters.R:47:0
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 52 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.6Mb
      sub-directories of 1Mb or more:
        libs     6.3Mb
        raster   2.2Mb
    ```

# multidplyr

<details>

* Version: 0.1.0
* GitHub: https://github.com/tidyverse/multidplyr
* Source code: https://github.com/cran/multidplyr
* Date/Publication: 2021-02-08 19:10:03 UTC
* Number of recursive dependencies: 75

Run `cloud_details(, "multidplyr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
           ▆
        1. ├─testthat::expect_equal(pf1 %>% intersect(pf2) %>% pull(), 1) at test-dplyr-dual.R:28:2
        2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
        3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
        4. ├─pf1 %>% intersect(pf2) %>% pull()
        5. ├─dplyr::pull(.)
        6. ├─generics::intersect(., pf2)
        7. └─multidplyr:::intersect.multidplyr_party_df(., pf2)
        8.   └─multidplyr:::shard_call_dual("intersect", x, y, ..., by = by)
        9.     └─multidplyr::cluster_send(...)
       10.       └─multidplyr::cluster_call(cluster, !!code)
      
      [ FAIL 1 | WARN 0 | SKIP 4 | PASS 45 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘R6’
      All declared Imports should be used.
    ```

# multinma

<details>

* Version: 0.3.0
* GitHub: https://github.com/dmphillippo/multinma
* Source code: https://github.com/cran/multinma
* Date/Publication: 2021-03-18 14:00:03 UTC
* Number of recursive dependencies: 134

Run `cloud_details(, "multinma")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        4. │   │ └─base::withCallingHandlers(...)
        5. │   └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
        6. ├─multinma::set_ipd(...)
        7. │ └─multinma:::pull_non_null(data, enquo(r))
        8. │   ├─dplyr::pull(...)
        9. │   ├─dplyr::transmute(...)
       10. │   └─dplyr:::transmute.data.frame(...)
       11. │     └─dplyr:::mutate_cols(.data, dots, caller_env = caller_env())
       12. │       ├─base::withCallingHandlers(...)
       13. │       └─mask$eval_all_mutate(quo)
       14. └─multinma::multi(r_c, r_b, r_a, inclusive = TRUE)
      
      [ FAIL 17 | WARN 0 | SKIP 16 | PASS 692 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 228.2Mb
      sub-directories of 1Mb or more:
        doc     5.9Mb
        libs  221.0Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘RcppParallel’ ‘rstantools’
      All declared Imports should be used.
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# nofrills

<details>

* Version: 0.3.1
* GitHub: https://github.com/egnha/nofrills
* Source code: https://github.com/cran/nofrills
* Date/Publication: 2021-01-08 19:50:05 UTC
* Number of recursive dependencies: 35

Run `cloud_details(, "nofrills")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘nofrills-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: as_fn
    > ### Title: Abbreviated functional arguments
    > ### Aliases: as_fn
    > 
    > ### ** Examples
    > 
    > call_fn <- function(.f, x) {
    ...
    Backtrace:
        ▆
     1. ├─global call_fn(.(. ~ (!!f)(.)^2), 1)
     2. │ └─nofrills::as_fn(.f)
     3. │   └─nofrills:::interpret_fn(x, match.fun(.f), parent.frame(2))
     4. │     └─base::eval(x, env)
     5. │       └─base::eval(x, env)
     6. └─nofrills `<fn>`(. ~ (!!f)(.)^2)
     7.   └─nofrills function_(d$args, d$body, ..env)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > test_check("nofrills")
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error (test-functions.R:48:3): body can be a closure ────────────────────────
      Error in `function_(d$args, d$body, ..env)`: Body must be an expression or closure.
      Backtrace:
          ▆
       1. ├─testthat::expect_equal(fn(x ~ function(y) x + y), foo) at test-functions.R:48:2
       2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
       3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
       4. └─nofrills::fn(x ~ function(y) x + y)
       5.   └─nofrills function_(d$args, d$body, ..env)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 107 ]
      Error: Test failures
      Execution halted
    ```

# pammtools

<details>

* Version: 0.5.7
* GitHub: https://github.com/adibender/pammtools
* Source code: https://github.com/cran/pammtools
* Date/Publication: 2021-06-21 13:00:02 UTC
* Number of recursive dependencies: 130

Run `cloud_details(, "pammtools")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Backtrace:
          ▆
       1. ├─... %>% add_cif(pam) at test-add-functions.R:268:2
       2. ├─pammtools::add_cif(., pam)
       3. └─pammtools:::add_cif.default(., pam)
       4.   └─purrr::map_dfr(...)
       5.     └─purrr::map(.x, .f, ...)
       6.       └─pammtools .f(.x[[i]], ...)
       7.         ├─pammtools:::get_cif(...)
       8.         └─pammtools:::get_cif.default(...)
       9.           └─base::apply(...)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 359 ]
      Error: Test failures
      Execution halted
    ```

# PPforest

<details>

* Version: 0.1.2
* GitHub: https://github.com/natydasilva/PPforest
* Source code: https://github.com/cran/PPforest
* Date/Publication: 2021-10-14 14:40:05 UTC
* Number of recursive dependencies: 81

Run `cloud_details(, "PPforest")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘PPforest-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: PPforest
    > ### Title: Projection Pursuit Random Forest
    > ### Aliases: PPforest
    > 
    > ### ** Examples
    > 
    > #crab example with all the observations used as training
    ...
      7.       └─dplyr:::slice_combine(chunks, mask = mask, error_call = error_call)
      8.         └─vctrs::vec_cast(res, integer())
      9.           └─vctrs `<fn>`()
     10.             └─vctrs:::vec_cast.integer.double(...)
     11.               └─vctrs:::shape_broadcast(out, to, x_arg = x_arg, to_arg = to_arg)
     12.                 └─vctrs::stop_incompatible_cast(...)
     13.                   └─vctrs::stop_incompatible_type(...)
     14.                     └─vctrs:::stop_incompatible(...)
     15.                       └─vctrs:::stop_vctrs(...)
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.3Mb
      sub-directories of 1Mb or more:
        libs   7.2Mb
    ```

# prider

<details>

* Version: 1.0.2
* GitHub: NA
* Source code: https://github.com/cran/prider
* Date/Publication: 2021-09-13 07:30:02 UTC
* Number of recursive dependencies: 29

Run `cloud_details(, "prider")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘prider-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: prider
    > ### Title: Prider
    > ### Aliases: prider print.prider plot.prider
    > 
    > ### ** Examples
    > 
    > test_fasta <- system.file('extdata', 'test.fasta', package = 'prider')
    ...
     14. │   └─base::eval(expr, p)
     15. │     └─base::eval(expr, p)
     16. │       └─base::eval(...)
     17. │         └─base::eval(...)
     18. │           └─vctrs::vec_assert(order_by, size = n, arg = "order_by")
     19. │             └─rlang::abort(...)
     20. │               └─rlang:::signal_abort(cnd, .file)
     21. │                 └─base::signalCondition(cnd)
     22. └─dplyr `<fn>`(`<vctrs___>`)
    Execution halted
    ```

# PVplr

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/PVplr
* Date/Publication: 2020-10-07 12:00:20 UTC
* Number of recursive dependencies: 72

Run `cloud_details(, "PVplr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘PVplr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plr_weighted_regression
    > ### Title: Weighted Regression
    > ### Aliases: plr_weighted_regression
    > 
    > ### ** Examples
    > 
    > # build var_list
    ...
      3.   │ └─base::eval(mf, parent.frame())
      4.   │   └─base::eval(mf, parent.frame())
      5.   ├─stats::model.frame(...)
      6.   └─stats::model.frame.default(...)
      7.     └─base::eval(extras, data, env)
      8.       └─base::eval(extras, data, env)
      9.         ├─wvar
     10.         └─rlang:::`$.rlang_fake_data_pronoun`(.data, wvar)
     11.           └─rlang:::stop_fake_data_subset(call)
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘ggplot2’
      All declared Imports should be used.
    ```

# ruler

<details>

* Version: 0.2.4
* GitHub: https://github.com/echasnovski/ruler
* Source code: https://github.com/cran/ruler
* Date/Publication: 2020-11-25 08:00:03 UTC
* Number of recursive dependencies: 59

Run `cloud_details(, "ruler")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ruler-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: cell-pack
    > ### Title: Cell rule pack
    > ### Aliases: cell-pack
    > 
    > ### ** Examples
    > 
    > cell_outlier_rules <- . %>% dplyr::transmute_at(
    ...
    +   rules(proper_is_neg = . < 0)
    + )
    > 
    > mtcars[1:2, ] %>%
    +   expose(cell_packs(improper_pack, proper_pack)) %>%
    +   get_report()
    Error in `keys<-`(`*tmp*`, value = keys(x)[i, , drop = FALSE]) : 
      Keys object should have the same number of rows as data.
    Calls: %>% ... transmute.data.frame -> dplyr_col_select -> [ -> [.keyed_df -> keys<-
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       14. └─magrittr .pack(.)
       15.   └─magrittr::freduce(value, `_function_list`)
       16.     ├─base::withVisible(function_list[[k]](value))
       17.     └─function_list[[k]](value)
       18.       └─dplyr::transmute_if(...)
       19.         ├─dplyr::transmute(.tbl, !!!funs)
       20.         └─dplyr:::transmute.data.frame(.tbl, !!!funs)
       21.           └─dplyr:::dplyr_col_select(out, cols_retain)
       22.             ├─.data[loc]
       23.             └─keyholder:::`[.keyed_df`(.data, loc)
       24.               └─keyholder::`keys<-`(`*tmp*`, value = keys(x)[i, , drop = FALSE])
      
      [ FAIL 7 | WARN 0 | SKIP 1 | PASS 282 ]
      Error: Test failures
      Execution halted
    ```

# sapfluxnetr

<details>

* Version: 0.1.2
* GitHub: https://github.com/sapfluxnet/sapfluxnetr
* Source code: https://github.com/cran/sapfluxnetr
* Date/Publication: 2021-10-04 07:40:02 UTC
* Number of recursive dependencies: 74

Run `cloud_details(, "sapfluxnetr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        5. │   └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
        6. ├─sapfluxnetr::filter_sites_by_md(...)
        7. │ └─... %>% unique()
        8. ├─base::unique(.)
        9. ├─dplyr::pull(., .data$si_code)
       10. ├─dplyr::filter(., !!!md_dots)
       11. └─dplyr:::filter.data.frame(., !!!md_dots)
       12.   └─dplyr:::filter_rows(.data, ..., caller_env = caller_env())
       13.     └─dplyr:::filter_eval(dots, mask = mask, error_call = error_call)
       14.       ├─base::withCallingHandlers(...)
       15.       └─mask$eval_all_filter(dots, env_filter)
      
      [ FAIL 1 | WARN 0 | SKIP 19 | PASS 350 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 4 marked UTF-8 strings
    ```

# shinymodels

<details>

* Version: 0.1.0
* GitHub: https://github.com/tidymodels/shinymodels
* Source code: https://github.com/cran/shinymodels
* Date/Publication: 2021-11-17 21:00:02 UTC
* Number of recursive dependencies: 146

Run `cloud_details(, "shinymodels")` for more info

</details>

## Newly broken

*   checking contents of ‘data’ directory ... WARNING
    ```
    Output for data("ames_mlp_itr", package = "shinymodels"):
      Search path was changed
    Output for data("cars_bag_vfld", package = "shinymodels"):
      Search path was changed
    Output for data("cell_race", package = "shinymodels"):
      Search path was changed
    Output for data("scat_fda_bt", package = "shinymodels"):
      Search path was changed
    Output for data("two_class_final", package = "shinymodels"):
      Search path was changed
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘finetune’
    ```

# srvyr

<details>

* Version: 1.1.0
* GitHub: https://github.com/gergness/srvyr
* Source code: https://github.com/cran/srvyr
* Date/Publication: 2021-09-29 04:40:02 UTC
* Number of recursive dependencies: 84

Run `cloud_details(, "srvyr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        9. │ └─dplyr:::summarise.grouped_df(.data$variables, !!!.dots, .groups = .groups)
       10. │   └─dplyr:::summarise_cols(.data, dplyr_quosures(...), caller_env = caller_env())
       11. │     ├─base::withCallingHandlers(...)
       12. │     └─dplyr:::map(quosures, summarise_eval_one, mask = mask)
       13. │       └─base::lapply(.x, .f, ...)
       14. │         └─dplyr FUN(X[[i]], ...)
       15. │           └─mask$eval_all_summarise(quo)
       16. └─srvyr::survey_sd(name)
       17.   └─srvyr::survey_var(x, na.rm = na.rm, vartype = NULL)
       18.     └─srvyr:::stop_for_factor(x)
       19.       └─base::stop(...)
      
      [ FAIL 45 | WARN 0 | SKIP 0 | PASS 313 ]
      Error: Test failures
      Execution halted
    ```

# tbrf

<details>

* Version: 0.1.5
* GitHub: https://github.com/mps9506/tbrf
* Source code: https://github.com/cran/tbrf
* Date/Publication: 2020-04-09 04:40:02 UTC
* Number of recursive dependencies: 94

Run `cloud_details(, "tbrf")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        8. │ └─... %>% tidyr::unnest(!!col_name)
        9. ├─tidyr::unnest(., !!col_name)
       10. ├─dplyr::mutate(...)
       11. ├─dplyr:::mutate.data.frame(...)
       12. │ └─dplyr:::mutate_cols(.data, dplyr_quosures(...), caller_env = caller_env())
       13. │   ├─base::withCallingHandlers(...)
       14. │   └─mask$eval_all_mutate(quo)
       15. └─purrr::map(...)
       16.   └─tbrf .f(.x[[i]], ...)
       17.     └─tbrf:::func_window(...)
       18.       └─base::stop("unit must be one of ", paste(u, collapse = ", "))
      
      [ FAIL 7 | WARN 0 | SKIP 0 | PASS 23 ]
      Error: Test failures
      Execution halted
    ```

# testdat

<details>

* Version: 0.3.0
* GitHub: https://github.com/socialresearchcentre/testdat
* Source code: https://github.com/cran/testdat
* Date/Publication: 2021-11-12 10:10:02 UTC
* Number of recursive dependencies: 74

Run `cloud_details(, "testdat")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      x_summ[x_summ$test == "passes", ] has 1 records failing value check on variable `status`.
      Filter: None
      Arguments: `"success", miss = <chr: NA, "">`
      ── Failure (test-reporter_excel.R:44:3): excel_results ─────────────────────────
      `x_xl_summary` not equal to `xl_summary`.
      Component "tests": Mean relative difference: 0.25
      Component "warning": Mean relative difference: 1
      ── Failure (test-reporter_excel.R:47:3): excel_results ─────────────────────────
      nrow(x_xl_passing) not equal to 0.
      1/1 mismatches
      [1] 1 - 0 == 1
      
      [ FAIL 3 | WARN 27 | SKIP 0 | PASS 159 ]
      Error: Test failures
      Execution halted
    ```

# tidyMicro

<details>

* Version: 1.47
* GitHub: https://github.com/CharlieCarpenter/tidyMicro
* Source code: https://github.com/cran/tidyMicro
* Date/Publication: 2020-09-13 17:10:03 UTC
* Number of recursive dependencies: 200

Run `cloud_details(, "tidyMicro")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘tidyMicro-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: alpha_div
    > ### Title: Alpha Diversity Calculations for tidy_micro
    > ### Aliases: alpha_div
    > 
    > ### ** Examples
    > 
    > data(bpd_phy); data(bpd_cla); data(bpd_ord); data(bpd_fam); data(bpd_clin)
    ...
     27. │       └─tidyselect:::walk_data_tree(expr, data_mask, context_mask)
     28. │         └─tidyselect:::eval_c(expr, data_mask, context_mask)
     29. │           └─tidyselect:::reduce_sels(node, data_mask, context_mask, init = init)
     30. │             └─tidyselect:::walk_data_tree(new, data_mask, context_mask)
     31. │               └─tidyselect:::eval_context(expr, context_mask)
     32. │                 └─rlang::eval_tidy(expr, context_mask)
     33. ├─rlang::.data$Lib
     34. └─rlang:::`$.rlang_fake_data_pronoun`(rlang::.data, Lib)
     35.   └─rlang:::stop_fake_data_subset(call)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       23. │     │   └─base::withCallingHandlers(...)
       24. │     └─tidyselect:::vars_select_eval(...)
       25. │       └─tidyselect:::walk_data_tree(expr, data_mask, context_mask)
       26. │         └─tidyselect:::eval_c(expr, data_mask, context_mask)
       27. │           └─tidyselect:::reduce_sels(node, data_mask, context_mask, init = init)
       28. │             └─tidyselect:::walk_data_tree(new, data_mask, context_mask)
       29. │               └─tidyselect:::eval_context(expr, context_mask)
       30. │                 └─rlang::eval_tidy(expr, context_mask)
       31. ├─rlang::.data$Lib
       32. └─rlang:::`$.rlang_fake_data_pronoun`(rlang::.data, Lib)
       33.   └─rlang:::stop_fake_data_subset(call)
      
      [ FAIL 18 | WARN 0 | SKIP 2 | PASS 2 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘Evomorph’ ‘cowplot’ ‘factoextra’ ‘gridExtra’ ‘lme4’ ‘lsr’ ‘plotly’
      ‘png’ ‘shapes’
      All declared Imports should be used.
    ```

# tidyquery

<details>

* Version: 0.2.2
* GitHub: https://github.com/ianmcook/tidyquery
* Source code: https://github.com/cran/tidyquery
* Date/Publication: 2021-02-06 07:30:04 UTC
* Number of recursive dependencies: 65

Run `cloud_details(, "tidyquery")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       11. ├─dplyr fun(., ...)
       12. └─dtplyr:::filter.dtplyr_step(., ...)
       13.   └─dtplyr:::step_subset_i(.data, i)
       14.     └─dtplyr:::step_subset(parent, i = i)
       15.       └─base::stopifnot(is.null(i) || is_expression(i) || is_step(i))
      ── Failure (test-errors.R:96:3): query() fails on two very long expressions with no aliases ──
      `query("SELECT 1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1, 1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+2 FROM games")` did not throw the expected error.
      Backtrace:
          ▆
       1. └─testthat::expect_error(...) at test-errors.R:96:2
       2.   └─testthat:::expect_condition_matching(...)
      
      [ FAIL 2 | WARN 0 | SKIP 1 | PASS 216 ]
      Error: Test failures
      Execution halted
    ```

# timetk

<details>

* Version: 2.6.2
* GitHub: https://github.com/business-science/timetk
* Source code: https://github.com/cran/timetk
* Date/Publication: 2021-11-16 07:00:05 UTC
* Number of recursive dependencies: 209

Run `cloud_details(, "timetk")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘timetk-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: tk_augment_differences
    > ### Title: Add many differenced columns to the data
    > ### Aliases: tk_augment_differences
    > 
    > ### ** Examples
    > 
    > library(tidyverse)
    ...
    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    > library(timetk)
    > 
    > m4_monthly %>%
    +     group_by(id) %>%
    +     tk_augment_differences(value, .lags = 1:20)
    Error: tk_augment_differences(.differences) is missing.
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2750 marked UTF-8 strings
    ```

# Tplyr

<details>

* Version: 0.4.2
* GitHub: https://github.com/atorus-research/Tplyr
* Source code: https://github.com/cran/Tplyr
* Date/Publication: 2021-10-15 13:20:02 UTC
* Number of recursive dependencies: 116

Run `cloud_details(, "Tplyr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       11. │   └─base::evalq(...)
       12. │     └─base::evalq(...)
       13. │       └─built_pop_data %>% ...
       14. ├─dplyr::mutate(...)
       15. ├─dplyr:::mutate.data.frame(...)
       16. │ └─dplyr:::mutate_cols(.data, dplyr_quosures(...), caller_env = caller_env())
       17. │   ├─base::withCallingHandlers(...)
       18. │   └─mask$eval_all_mutate(quo)
       19. ├─forcats::fct_expand(...)
       20. │ └─forcats:::check_factor(f)
       21. └─col_i
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 685 ]
      Error: Test failures
      Execution halted
    ```

# tsibble

<details>

* Version: 1.1.0
* GitHub: https://github.com/tidyverts/tsibble
* Source code: https://github.com/cran/tsibble
* Date/Publication: 2021-10-22 11:10:01 UTC
* Number of recursive dependencies: 90

Run `cloud_details(, "tsibble")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        9. │ └─dplyr:::summarise.grouped_df(...)
       10. │   └─dplyr:::summarise_cols(.data, dplyr_quosures(...), caller_env = caller_env())
       11. │     ├─base::withCallingHandlers(...)
       12. │     └─dplyr:::map(quosures, summarise_eval_one, mask = mask)
       13. │       └─base::lapply(.x, .f, ...)
       14. │         └─dplyr FUN(X[[i]], ...)
       15. │           └─mask$eval_all_summarise(quo)
       16. ├─tsibble:::tbl_gaps(date, idx_full, .name = .name)
       17. │ └─base::stopifnot(has_length(.name, 3))
       18. │   └─base::stop(simpleError(msg, call = if (p <- sys.parent(1L)) sys.call(p)))
       19. └─dplyr `<fn>`(`<smplErrr>`)
      
      [ FAIL 1 | WARN 1 | SKIP 5 | PASS 743 ]
      Error: Test failures
      Execution halted
    ```

# yardstick

<details>

* Version: 0.0.8
* GitHub: https://github.com/tidymodels/yardstick
* Source code: https://github.com/cran/yardstick
* Date/Publication: 2021-03-28 14:50:03 UTC
* Number of recursive dependencies: 77

Run `cloud_details(, "yardstick")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       14. │         └─base::lapply(.x, .f, ...)
       15. │           └─dplyr FUN(X[[i]], ...)
       16. │             └─mask$eval_all_summarise(quo)
       17. └─yardstick metric_fn(truth = truth, estimate = Class1, na_rm = na_rm, options = `<list>`)
       18.   └─yardstick::metric_vec_template(...)
       19.     └─yardstick:::validate_truth_estimate_checks(...)
       20.       ├─yardstick:::validate_truth_estimate_types(truth, estimate, estimator)
       21.       └─yardstick:::validate_truth_estimate_types.factor(...)
       22.         ├─yardstick:::multiclass_checks(truth, estimate)
       23.         └─yardstick:::multiclass_checks.numeric(truth, estimate)
       24.           └─yardstick:::multiclass_checks.matrix(truth, as.matrix(estimate))
      
      [ FAIL 29 | WARN 0 | SKIP 1 | PASS 603 ]
      Error: Test failures
      Execution halted
    ```

