# AATtools

<details>

* Version: 0.0.1
* GitHub: https://github.com/Spiritspeak/AATtools
* Source code: https://github.com/cran/AATtools
* Date/Publication: 2020-06-14 15:10:06 UTC
* Number of recursive dependencies: 22

Run `cloud_details(, "AATtools")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘AATtools-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: aat_bootstrap
    > ### Title: Compute bootstrapped approach-bias scores
    > ### Aliases: aat_bootstrap print.aat_bootstrap plot.aat_bootstrap
    > 
    > ### ** Examples
    > 
    > # Compute 10 bootstrapped AAT scores.
    ...
    The following objects are masked from ‘package:base’:
    
        intersect, setdiff, setequal, union
    
    Error in { : 
      task 1 failed - "Problem while computing `..1 = abs(scale(RT)) < 3`.
    ✖ Input `..1` must be a logical vector, not a logical[,1].
    ℹ The error occurred in group 1: subject = 1."
    Calls: aat_bootstrap -> %dofunc% -> <Anonymous>
    Execution halted
    ```

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

# bigsnpr

<details>

* Version: 1.8.1
* GitHub: https://github.com/privefl/bigsnpr
* Source code: https://github.com/cran/bigsnpr
* Date/Publication: 2021-06-03 11:00:12 UTC
* Number of recursive dependencies: 136

Run `cloud_details(, "bigsnpr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      • is_cran is TRUE (1)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test-1-readBGEN.R:66:3): same variant infos as with QCTOOL ─────────
      dplyr::mutate(test$map[-19, 1:6], chromosome = as.integer(chromosome)) not identical to dplyr::as_tibble(...).
      Names: 3 string mismatches
      Component 1: Modes: numeric, character
      Component 1: target is numeric, current is character
      Component 2: Modes: character, numeric
      Component 2: target is character, current is numeric
      Component 3: 198 string mismatches
      
      [ FAIL 1 | WARN 0 | SKIP 1 | PASS 226 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 18.3Mb
      sub-directories of 1Mb or more:
        libs  16.5Mb
    ```

# bruceR

<details>

* Version: 0.7.3
* GitHub: https://github.com/psychbruce/bruceR
* Source code: https://github.com/cran/bruceR
* Date/Publication: 2021-11-05 17:40:01 UTC
* Number of recursive dependencies: 202

Run `cloud_details(, "bruceR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘bruceR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: Alpha
    > ### Title: Reliability analysis (Cronbach's alpha and McDonald's omega).
    > ### Aliases: Alpha
    > 
    > ### ** Examples
    > 
    > # ?psych::bfi
    > Alpha(bfi, "E", 1:5)  # "E1" & "E2" should be reverse scored
    Warning: replacing previous import ‘ggplot2::enquo’ by ‘jmvcore::enquo’ when loading ‘jmv’
    Error in jmvcore::enquo(vars) : object 'rlang_enquo' not found
    Calls: Alpha -> <Anonymous> -> <Anonymous> -> <Anonymous>
    Execution halted
    ```

# cohorts

<details>

* Version: 1.0.0
* GitHub: https://github.com/PeerChristensen/cohorts
* Source code: https://github.com/cran/cohorts
* Date/Publication: 2021-07-08 12:00:02 UTC
* Number of recursive dependencies: 41

Run `cloud_details(, "cohorts")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘cohorts-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: cohort_table_day
    > ### Title: Create a Cohort Table Using Day Level Event Data
    > ### Aliases: cohort_table_day
    > 
    > ### ** Examples
    > 
    > cohort_table_day(gamelaunch, userid, eventDate)
    Error in get(name, envir = asNamespace(pkg), inherits = FALSE) : 
      object 'find_var' not found
    Calls: cohort_table_day ... pivot_wider.dtplyr_step -> unique -> pull -> pull.dtplyr_step -> ::: -> get
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
      
      [ FAIL 34 | WARN 0 | SKIP 9 | PASS 282 ]
      Error: Test failures
      Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Missing object imported by a ':::' call: ‘dplyr:::find_var’
    ```

# dynfeature

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/dynfeature
* Date/Publication: 2021-06-14 07:30:12 UTC
* Number of recursive dependencies: 120

Run `cloud_details(, "dynfeature")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
          is_null
      
      > 
      > test_check("dynfeature")
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test-calculate_feature_importance.R:170:3): Testing calculate_branching_point_feature_importance ──
      gimp %>% map_chr(class) not equal to c(milestone_id = "factor", feature_id = "factor", importance = "numeric").
      Names: 3 string mismatches
      ── Failure (test-calculate_feature_importance.R:178:3): Testing calculate_branch_feature_importance ──
      gimp %>% map_chr(class) not equal to c(feature_id = "factor", from = "factor", to = "factor", importance = "numeric").
      Names: 3 string mismatches
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 17 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘magrittr’
      All declared Imports should be used.
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

# egor

<details>

* Version: 1.21.10
* GitHub: https://github.com/tilltnet/egor
* Source code: https://github.com/cran/egor
* Date/Publication: 2021-10-07 17:00:05 UTC
* Number of recursive dependencies: 89

Run `cloud_details(, "egor")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘egor-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_egograms
    > ### Title: Plotting _egor_ objects
    > ### Aliases: plot_egograms plot_ego_graphs plot_egor plot.egor
    > 
    > ### ** Examples
    > 
    > e <- make_egor(net.count = 5, max.alters = 12)
    ...
     10. │   └─dplyr:::filter_rows(.data, ..., caller_env = caller_env())
     11. │     └─dplyr:::filter_eval(dots, mask = mask, error_call = error_call)
     12. │       ├─base::withCallingHandlers(...)
     13. │       └─mask$eval_all_filter(dots, env_filter)
     14. ├─dplyr:::dplyr_internal_error(...)
     15. │ └─rlang::abort(class = c(class, "dplyr:::internal_error"), dplyr_error_data = data)
     16. │   └─rlang:::signal_abort(cnd, .file)
     17. │     └─base::signalCondition(cnd)
     18. └─dplyr `<fn>`(`<dpl:::__>`)
    Execution halted
    ```

## In both

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      The following objects are masked from 'package:base':
      
          intersect, setdiff, setequal, union
      
      Loading required package: tibble
      > library(dplyr)
      > 
      > test_check("egor")
      Error in parse(con, n = -1, srcfile = srcfile, encoding = "UTF-8") : 
        test-activate.R:2:11: unexpected '>'
      1: test_that("activate works with singular and plurals of level names", {
      2:   egor32 |>
                   ^
      Calls: test_check ... doTryCatch -> lapply -> FUN -> source_file -> parse
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

# ezcox

<details>

* Version: 1.0.2
* GitHub: https://github.com/ShixiangWang/ezcox
* Source code: https://github.com/cran/ezcox
* Date/Publication: 2021-10-28 15:20:08 UTC
* Number of recursive dependencies: 88

Run `cloud_details(, "ezcox")` for more info

</details>

## Newly broken

*   checking whether package ‘ezcox’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/ezcox/new/ezcox.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      During startup - Warning message:
      package ‘stats’ in options("defaultPackages") was not found 
      Error: package or namespace load failed for ‘utils’:
       .onLoad failed in loadNamespace() for 'utils', details:
        call: system("uname -a", intern = TRUE)
        error: cannot popen 'uname -a', probable reason 'Cannot allocate memory'
      Error: package or namespace load failed for ‘grDevices’ in get(Info[i, 1], envir = env):
       read failed on /opt/R/4.0.3/lib/R/library/grDevices/R/grDevices.rdb
      Error: package or namespace load failed for ‘graphics’:
       .onLoad failed in loadNamespace() for 'grDevices', details:
        call: .select_device()
        error: lazy-load database '/opt/R/4.0.3/lib/R/library/grDevices/R/grDevices.rdb' is corrupt
      Killed
      Error: package or namespace load failed for ‘graphics’ in get(Info[i, 1], envir = env):
       lazy-load database '/opt/R/4.0.3/lib/R/library/grDevices/R/grDevices.rdb' is corrupt
    ```

## Installation

### Devel

```
* installing *source* package ‘ezcox’ ...
** package ‘ezcox’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Killed
ERROR: lazy loading failed for package ‘ezcox’
* removing ‘/tmp/workdir/ezcox/new/ezcox.Rcheck/ezcox’


```
### CRAN

```
* installing *source* package ‘ezcox’ ...
** package ‘ezcox’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (ezcox)


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

# ggcharts

<details>

* Version: 0.2.1
* GitHub: https://github.com/thomas-neitmann/ggcharts
* Source code: https://github.com/cran/ggcharts
* Date/Publication: 2020-05-20 00:40:02 UTC
* Number of recursive dependencies: 85

Run `cloud_details(, "ggcharts")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggcharts-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: diverging_bar_chart
    > ### Title: Diverging Bar Chart
    > ### Aliases: diverging_bar_chart
    > 
    > ### ** Examples
    > 
    > if (requireNamespace("tidyr")) {
    ...
      8. │     └─dplyr:::filter_rows(.data, ..., caller_env = caller_env())
      9. │       └─dplyr:::filter_eval(dots, mask = mask, error_call = error_call)
     10. │         ├─base::withCallingHandlers(...)
     11. │         └─mask$eval_all_filter(dots, env_filter)
     12. ├─dplyr:::dplyr_internal_error(...)
     13. │ └─rlang::abort(class = c(class, "dplyr:::internal_error"), dplyr_error_data = data)
     14. │   └─rlang:::signal_abort(cnd, .file)
     15. │     └─base::signalCondition(cnd)
     16. └─dplyr `<fn>`(`<dpl:::__>`)
    Execution halted
    ```

# huxtable

<details>

* Version: 5.4.0
* GitHub: https://github.com/hughjonesd/huxtable
* Source code: https://github.com/cran/huxtable
* Date/Publication: 2021-05-14 21:30:06 UTC
* Number of recursive dependencies: 169

Run `cloud_details(, "huxtable")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ Skipped tests ═══════════════════════════════════════════════════════════════
      • 0-length tables don't work in flextable yet (1)
      • Awaiting ftExtra improvements (1)
      • Couldn't unload dplyr namespace (2)
      • Not testing, code doesn't play well with R CMD check (3)
      • On CRAN (16)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test-dplyr.R:56:3): mutate, mutate_ and transmute work ─────────────
      bold(ht4)[, 1] not equivalent to c(FALSE, FALSE, FALSE).
      1 element mismatch
      
      [ FAIL 1 | WARN 2 | SKIP 23 | PASS 1207 ]
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

# janitor

<details>

* Version: 2.1.0
* GitHub: https://github.com/sfirke/janitor
* Source code: https://github.com/cran/janitor
* Date/Publication: 2021-01-05 01:10:04 UTC
* Number of recursive dependencies: 67

Run `cloud_details(, "janitor")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘janitor-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: tabyl
    > ### Title: Generate a frequency table (1-, 2-, or 3-way).
    > ### Aliases: tabyl tabyl.default tabyl.data.frame
    > 
    > ### ** Examples
    > 
    > 
    ...
     10. │ └─dplyr:::filter_rows(.data, ..., caller_env = caller_env())
     11. │   └─dplyr:::filter_eval(dots, mask = mask, error_call = error_call)
     12. │     ├─base::withCallingHandlers(...)
     13. │     └─mask$eval_all_filter(dots, env_filter)
     14. ├─dplyr:::dplyr_internal_error(...)
     15. │ └─rlang::abort(class = c(class, "dplyr:::internal_error"), dplyr_error_data = data)
     16. │   └─rlang:::signal_abort(cnd, .file)
     17. │     └─base::signalCondition(cnd)
     18. └─dplyr `<fn>`(`<dpl:::__>`)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        9. ├─dplyr::filter(., !is.na(.[1]))
       10. ├─dplyr:::filter.data.frame(., !is.na(.[1]))
       11. │ └─dplyr:::filter_rows(.data, ..., caller_env = caller_env())
       12. │   └─dplyr:::filter_eval(dots, mask = mask, error_call = error_call)
       13. │     ├─base::withCallingHandlers(...)
       14. │     └─mask$eval_all_filter(dots, env_filter)
       15. ├─dplyr:::dplyr_internal_error(...)
       16. │ └─rlang::abort(class = c(class, "dplyr:::internal_error"), dplyr_error_data = data)
       17. │   └─rlang:::signal_abort(cnd, .file)
       18. │     └─base::signalCondition(cnd)
       19. └─dplyr `<fn>`(`<dpl:::__>`)
      
      [ FAIL 11 | WARN 0 | SKIP 1 | PASS 504 ]
      Error: Test failures
      Execution halted
    ```

# lares

<details>

* Version: 5.0.2
* GitHub: https://github.com/laresbernardo/lares
* Source code: https://github.com/cran/lares
* Date/Publication: 2021-09-10 13:40:02 UTC
* Number of recursive dependencies: 142

Run `cloud_details(, "lares")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘lares-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: distr
    > ### Title: Compare Variables with their Distributions
    > ### Aliases: distr
    > 
    > ### ** Examples
    > 
    > Sys.unsetenv("LARES_FONT") # Temporal
    ...
    > dft %>% distr(Survived, Sex)
    Error in `distr()`: Can't subset `.data` outside of a data mask context.
    Backtrace:
        ▆
     1. ├─dft %>% distr(Survived, Sex)
     2. └─lares::distr(., Survived, Sex)
     3.   ├─<unknown>
     4.   └─rlang:::`$.rlang_fake_data_pronoun`(.data, "value")
     5.     └─rlang:::stop_fake_data_subset(call)
    Execution halted
    ```

# manymodelr

<details>

* Version: 0.3.6
* GitHub: https://github.com/Nelson-Gon/manymodelr
* Source code: https://github.com/cran/manymodelr
* Date/Publication: 2021-08-17 09:20:02 UTC
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

# microeco

<details>

* Version: 0.5.1
* GitHub: NA
* Source code: https://github.com/cran/microeco
* Date/Publication: 2021-09-01 21:50:01 UTC
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

# Momocs

<details>

* Version: 1.3.2
* GitHub: https://github.com/MomX/Momocs
* Source code: https://github.com/cran/Momocs
* Date/Publication: 2020-10-06 15:20:11 UTC
* Number of recursive dependencies: 121

Run `cloud_details(, "Momocs")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘Momocs-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: rm_missing
    > ### Title: Remove shapes with missing data in fac
    > ### Aliases: rm_missing
    > 
    > ### ** Examples
    > 
    > bot$fac$type[3] <- NA
    ...
      8. │     └─dplyr:::filter_rows(.data, ..., caller_env = caller_env())
      9. │       └─dplyr:::filter_eval(dots, mask = mask, error_call = error_call)
     10. │         ├─base::withCallingHandlers(...)
     11. │         └─mask$eval_all_filter(dots, env_filter)
     12. ├─dplyr:::dplyr_internal_error(...)
     13. │ └─rlang::abort(class = c(class, "dplyr:::internal_error"), dplyr_error_data = data)
     14. │   └─rlang:::signal_abort(cnd, .file)
     15. │     └─base::signalCondition(cnd)
     16. └─dplyr `<fn>`(`<dpl:::__>`)
    Execution halted
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

# MRFcov

<details>

* Version: 1.0.38
* GitHub: https://github.com/nicholasjclark/MRFcov
* Source code: https://github.com/cran/MRFcov
* Date/Publication: 2021-03-18 06:40:03 UTC
* Number of recursive dependencies: 103

Run `cloud_details(, "MRFcov")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘MRFcov-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plotMRF_hm
    > ### Title: Plot MRF interaction parameters as a heatmap
    > ### Aliases: plotMRF_hm
    > 
    > ### ** Examples
    > 
    > 
    ...
      6. │ └─dplyr:::filter_rows(.data, ..., caller_env = caller_env())
      7. │   └─dplyr:::filter_eval(dots, mask = mask, error_call = error_call)
      8. │     ├─base::withCallingHandlers(...)
      9. │     └─mask$eval_all_filter(dots, env_filter)
     10. ├─dplyr:::dplyr_internal_error(...)
     11. │ └─rlang::abort(class = c(class, "dplyr:::internal_error"), dplyr_error_data = data)
     12. │   └─rlang:::signal_abort(cnd, .file)
     13. │     └─base::signalCondition(cnd)
     14. └─dplyr `<fn>`(`<dpl:::__>`)
    Execution halted
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
      ── Error (test-partydf.R:49:3): can partition and re-collect ───────────────────
      Error in `get(name, envir = asNamespace(pkg), inherits = FALSE)`: object 'find_var' not found
      Backtrace:
          ▆
       1. ├─testthat::expect_equal(pull(df2, x), df1$x) at test-partydf.R:49:2
       2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
       3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
       4. ├─dplyr::pull(df2, x)
       5. └─multidplyr:::pull.multidplyr_party_df(df2, x)
       6.   └─dplyr:::find_var
       7.     └─base::get(name, envir = asNamespace(pkg), inherits = FALSE)
      
      [ FAIL 3 | WARN 0 | SKIP 4 | PASS 38 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘R6’
      All declared Imports should be used.
    Missing object imported by a ':::' call: ‘dplyr:::find_var’
    ```

# multifear

<details>

* Version: 0.1.2
* GitHub: https://github.com/AngelosPsy/multifear
* Source code: https://github.com/cran/multifear
* Date/Publication: 2021-06-01 20:50:02 UTC
* Number of recursive dependencies: 152

Run `cloud_details(, "multifear")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       11. ├─dplyr::filter(., excl == FALSE)
       12. ├─dplyr:::filter.data.frame(., excl == FALSE)
       13. │ └─dplyr:::filter_rows(.data, ..., caller_env = caller_env())
       14. │   └─dplyr:::filter_eval(dots, mask = mask, error_call = error_call)
       15. │     ├─base::withCallingHandlers(...)
       16. │     └─mask$eval_all_filter(dots, env_filter)
       17. ├─dplyr:::dplyr_internal_error(...)
       18. │ └─rlang::abort(class = c(class, "dplyr:::internal_error"), dplyr_error_data = data)
       19. │   └─rlang:::signal_abort(cnd, .file)
       20. │     └─base::signalCondition(cnd)
       21. └─dplyr `<fn>`(`<dpl:::__>`)
      
      [ FAIL 2 | WARN 1 | SKIP 0 | PASS 15 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘ez’
      All declared Imports should be used.
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
      8. │   └─dplyr:::slice_rows(.data, ..., caller_env = caller_env(2), error_call = error_call)
      9. │     └─dplyr:::slice_eval(mask, dots, error_call = error_call)
     10. │       ├─base::withCallingHandlers(...)
     11. │       └─mask$eval_all(quo)
     12. ├─vctrs::vec_assert(1, size = n, arg = "order_by")
     13. │ └─rlang::abort(...)
     14. │   └─rlang:::signal_abort(cnd, .file)
     15. │     └─base::signalCondition(cnd)
     16. └─dplyr `<fn>`(`<vctrs___>`)
    Execution halted
    ```

# psfmi

<details>

* Version: 1.0.0
* GitHub: https://github.com/mwheymans/psfmi
* Source code: https://github.com/cran/psfmi
* Date/Publication: 2021-09-23 10:10:05 UTC
* Number of recursive dependencies: 168

Run `cloud_details(, "psfmi")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘psfmi-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: pool_D4
    > ### Title: Pools the Likelihood Ratio tests across Multiply Imputed
    > ###   datasets ( method D4)
    > ### Aliases: pool_D4
    > 
    > ### ** Examples
    > 
    ...
      4. │   └─dplyr:::filter_rows(.data, ..., caller_env = caller_env())
      5. │     └─dplyr:::filter_eval(dots, mask = mask, error_call = error_call)
      6. │       ├─base::withCallingHandlers(...)
      7. │       └─mask$eval_all_filter(dots, env_filter)
      8. ├─dplyr:::dplyr_internal_error(...)
      9. │ └─rlang::abort(class = c(class, "dplyr:::internal_error"), dplyr_error_data = data)
     10. │   └─rlang:::signal_abort(cnd, .file)
     11. │     └─base::signalCondition(cnd)
     12. └─dplyr `<fn>`(`<dpl:::__>`)
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘miceadds’
      All declared Imports should be used.
    ```

# psycModel

<details>

* Version: 0.3.2
* GitHub: NA
* Source code: https://github.com/cran/psycModel
* Date/Publication: 2021-09-05 05:00:02 UTC
* Number of recursive dependencies: 171

Run `cloud_details(, "psycModel")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘psycModel-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: cfa_groupwise
    > ### Title: Confirmatory Factor Analysis (groupwise)
    > ### Aliases: cfa_groupwise
    > 
    > ### ** Examples
    > 
    > # The example is used as the illustration of the function output only.
    ...
      5. │ └─dplyr:::filter_rows(.data, ..., caller_env = caller_env())
      6. │   └─dplyr:::filter_eval(dots, mask = mask, error_call = error_call)
      7. │     ├─base::withCallingHandlers(...)
      8. │     └─mask$eval_all_filter(dots, env_filter)
      9. ├─dplyr:::dplyr_internal_error(...)
     10. │ └─rlang::abort(class = c(class, "dplyr:::internal_error"), dplyr_error_data = data)
     11. │   └─rlang:::signal_abort(cnd, .file)
     12. │     └─base::signalCondition(cnd)
     13. └─dplyr `<fn>`(`<dpl:::__>`)
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘lifecycle’ ‘patchwork’
      All declared Imports should be used.
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

# r2dii.match

<details>

* Version: 0.0.11
* GitHub: https://github.com/2DegreesInvesting/r2dii.match
* Source code: https://github.com/cran/r2dii.match
* Date/Publication: 2021-09-23 04:40:08 UTC
* Number of recursive dependencies: 66

Run `cloud_details(, "r2dii.match")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(testthat)
      > library(r2dii.match)
      > 
      > test_check("r2dii.match")
      ══ Skipped tests ═══════════════════════════════════════════════════════════════
      • On CRAN (6)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test-restructure_ald.R:50:3): restructure_ald outputs the expected tibble ──
      Names of `out` ('sector', 'name', 'alias') don't match 'name', 'sector', 'alias'
      
      [ FAIL 1 | WARN 0 | SKIP 6 | PASS 169 ]
      Error: Test failures
      Execution halted
    ```

# saeSim

<details>

* Version: 0.10.0
* GitHub: https://github.com/wahani/saeSim
* Source code: https://github.com/cran/saeSim
* Date/Publication: 2019-03-28 12:50:03 UTC
* Number of recursive dependencies: 101

Run `cloud_details(, "saeSim")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘saeSim-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: sim_sample
    > ### Title: Sampling component
    > ### Aliases: sim_sample
    > 
    > ### ** Examples
    > 
    > # Simple random sample - 5% sample:
    ...
     12. │   └─dplyr:::slice_rows(.data, ..., caller_env = caller_env(2), error_call = error_call)
     13. │     └─dplyr:::slice_eval(mask, dots, error_call = error_call)
     14. │       ├─base::withCallingHandlers(...)
     15. │       └─mask$eval_all(quo)
     16. ├─dplyr:::check_frac(size, replace = replace)
     17. │ └─rlang::abort(bullets)
     18. │   └─rlang:::signal_abort(cnd, .file)
     19. │     └─base::signalCondition(cnd)
     20. └─dplyr `<fn>`(`<rlng_rrr>`)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘test-all.R’
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
       17. ├─dplyr:::sample_frac.data.frame(...)
       18. │ └─dplyr:::slice_impl(...)
       19. │   └─dplyr:::slice_rows(.data, ..., caller_env = caller_env(2), error_call = error_call)
       20. │     └─dplyr:::slice_eval(mask, dots, error_call = error_call)
       21. │       ├─base::withCallingHandlers(...)
       22. │       └─mask$eval_all(quo)
       23. ├─dplyr:::check_frac(size, replace = replace)
       24. │ └─rlang::abort(bullets)
       25. │   └─rlang:::signal_abort(cnd, .file)
       26. │     └─base::signalCondition(cnd)
       27. └─dplyr `<fn>`(`<rlng_rrr>`)
      
      [ FAIL 2 | WARN 6 | SKIP 0 | PASS 134 ]
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

# SimplyAgree

<details>

* Version: 0.0.2
* GitHub: NA
* Source code: https://github.com/cran/SimplyAgree
* Date/Publication: 2021-05-18 14:10:16 UTC
* Number of recursive dependencies: 182

Run `cloud_details(, "SimplyAgree")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
          ▆
       1. └─SimplyAgree::jmvreli(...) at test-reli_stats.R:25:2
       2.   ├─jmvcore::resolveQuo(jmvcore::enquo(vars))
       3.   └─jmvcore::enquo(vars)
      ── Error (test_agree_test.R:9:3): Simple Use Run Through ───────────────────────
      Error in `jmvcore::enquo(method1)`: object 'rlang_enquo' not found
      Backtrace:
          ▆
       1. └─SimplyAgree::jmvagree(...) at test_agree_test.R:9:2
       2.   ├─jmvcore::resolveQuo(jmvcore::enquo(method1))
       3.   └─jmvcore::enquo(method1)
      
      [ FAIL 4 | WARN 16 | SKIP 0 | PASS 28 ]
      Error: Test failures
      Execution halted
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

# tangram.pipe

<details>

* Version: 1.1.0
* GitHub: https://github.com/thomasgstewart/tangram.pipe
* Source code: https://github.com/cran/tangram.pipe
* Date/Publication: 2021-10-11 22:00:02 UTC
* Number of recursive dependencies: 58

Run `cloud_details(, "tangram.pipe")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘tangram.pipe-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: binary_row
    > ### Title: Binary Row
    > ### Aliases: binary_row
    > ### Keywords: tangram.pipe
    > 
    > ### ** Examples
    > 
    ...
      9. │ └─dplyr:::filter_rows(.data, ..., caller_env = caller_env())
     10. │   └─dplyr:::filter_eval(dots, mask = mask, error_call = error_call)
     11. │     ├─base::withCallingHandlers(...)
     12. │     └─mask$eval_all_filter(dots, env_filter)
     13. ├─dplyr:::dplyr_internal_error(...)
     14. │ └─rlang::abort(class = c(class, "dplyr:::internal_error"), dplyr_error_data = data)
     15. │   └─rlang:::signal_abort(cnd, .file)
     16. │     └─base::signalCondition(cnd)
     17. └─dplyr `<fn>`(`<dpl:::__>`)
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

# TeachHist

<details>

* Version: 0.1.2
* GitHub: NA
* Source code: https://github.com/cran/TeachHist
* Date/Publication: 2021-04-08 07:50:05 UTC
* Number of recursive dependencies: 35

Run `cloud_details(, "TeachHist")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘TeachHist-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: TeachHistConfInterv
    > ### Title: Histogram to Visualize Confidence Intervalls
    > ### Aliases: TeachHistConfInterv
    > 
    > ### ** Examples
    > 
    > TeachHistConfInterv()
    ...
      5. │     └─dplyr:::filter_rows(.data, ..., caller_env = caller_env())
      6. │       └─dplyr:::filter_eval(dots, mask = mask, error_call = error_call)
      7. │         ├─base::withCallingHandlers(...)
      8. │         └─mask$eval_all_filter(dots, env_filter)
      9. ├─dplyr:::dplyr_internal_error(...)
     10. │ └─rlang::abort(class = c(class, "dplyr:::internal_error"), dplyr_error_data = data)
     11. │   └─rlang:::signal_abort(cnd, .file)
     12. │     └─base::signalCondition(cnd)
     13. └─dplyr `<fn>`(`<dpl:::__>`)
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
      
      [ FAIL 3 | WARN 27 | SKIP 0 | PASS 165 ]
      Error: Test failures
      Execution halted
    ```

# texter

<details>

* Version: 0.1.9
* GitHub: https://github.com/simmieyungie/texter
* Source code: https://github.com/cran/texter
* Date/Publication: 2021-09-20 14:20:02 UTC
* Number of recursive dependencies: 75

Run `cloud_details(, "texter")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘texter-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: top_Sentiments
    > ### Title: Get the top 10 negative and positive words
    > ### Aliases: top_Sentiments
    > 
    > ### ** Examples
    > 
    > top_Sentiments(doge$text, plot = TRUE)
    ...
      8. │   └─dplyr:::slice_rows(.data, ..., caller_env = caller_env(2), error_call = error_call)
      9. │     └─dplyr:::slice_eval(mask, dots, error_call = error_call)
     10. │       ├─base::withCallingHandlers(...)
     11. │       └─mask$eval_all(quo)
     12. ├─vctrs::vec_assert(n, size = n, arg = "order_by")
     13. │ └─rlang::abort(...)
     14. │   └─rlang:::signal_abort(cnd, .file)
     15. │     └─base::signalCondition(cnd)
     16. └─dplyr `<fn>`(`<vctrs___>`)
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

* Version: 2.6.1
* GitHub: https://github.com/business-science/timetk
* Source code: https://github.com/cran/timetk
* Date/Publication: 2021-01-18 17:40:02 UTC
* Number of recursive dependencies: 207

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

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘generics’
      All declared Imports should be used.
    ```

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

# validata

<details>

* Version: 0.1.0
* GitHub: https://github.com/Harrison4192/validata
* Source code: https://github.com/cran/validata
* Date/Publication: 2021-10-05 08:20:02 UTC
* Number of recursive dependencies: 123

Run `cloud_details(, "validata")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘validata-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: confirm_strlen
    > ### Title: confirm string length
    > ### Aliases: confirm_strlen choose_strlen
    > 
    > ### ** Examples
    > 
    > 
    ...
     14. │ └─dplyr:::filter_rows(.data, ..., caller_env = caller_env())
     15. │   └─dplyr:::filter_eval(dots, mask = mask, error_call = error_call)
     16. │     ├─base::withCallingHandlers(...)
     17. │     └─mask$eval_all_filter(dots, env_filter)
     18. ├─dplyr:::dplyr_internal_error(...)
     19. │ └─rlang::abort(class = c(class, "dplyr:::internal_error"), dplyr_error_data = data)
     20. │   └─rlang:::signal_abort(cnd, .file)
     21. │     └─base::signalCondition(cnd)
     22. └─dplyr `<fn>`(`<dpl:::__>`)
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘BBmisc’ ‘badger’
      All declared Imports should be used.
    ```

# widyr

<details>

* Version: 0.1.4
* GitHub: https://github.com/dgrtwo/widyr
* Source code: https://github.com/cran/widyr
* Date/Publication: 2021-08-12 17:10:02 UTC
* Number of recursive dependencies: 104

Run `cloud_details(, "widyr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘widyr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: widely_svd
    > ### Title: Turn into a wide matrix, perform SVD, return to tidy form
    > ### Aliases: widely_svd widely_svd_
    > 
    > ### ** Examples
    > 
    > 
    ...
     1. ├─... %>% ggplot(aes(`1`, `2`, label = country))
     2. ├─ggplot2::ggplot(., aes(`1`, `2`, label = country))
     3. ├─dplyr::inner_join(...)
     4. ├─tidyr::spread(., dimension, value)
     5. └─tidyr:::spread.data.frame(., dimension, value)
     6.   └─tidyselect::vars_pull(names(data), !!enquo(value))
     7.     ├─tidyselect:::instrument_base_errors(...)
     8.     │ └─base::withCallingHandlers(...)
     9.     └─rlang::eval_tidy(enquo(var), set_names(seq_along(vars), vars))
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘gapminder’
      All declared Imports should be used.
    ```

# xpose

<details>

* Version: 0.4.13
* GitHub: https://github.com/UUPharmacometrics/xpose
* Source code: https://github.com/cran/xpose
* Date/Publication: 2021-06-30 08:00:02 UTC
* Number of recursive dependencies: 102

Run `cloud_details(, "xpose")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘xpose-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: minimization_plots
    > ### Title: Parameter value or gradient vs. iterations
    > ### Aliases: minimization_plots prm_vs_iteration grd_vs_iteration
    > 
    > ### ** Examples
    > 
    > prm_vs_iteration(xpdb_ex_pk)
    ...
     14. │ └─dplyr:::filter_rows(.data, ..., caller_env = caller_env())
     15. │   └─dplyr:::filter_eval(dots, mask = mask, error_call = error_call)
     16. │     ├─base::withCallingHandlers(...)
     17. │     └─mask$eval_all_filter(dots, env_filter)
     18. ├─dplyr:::dplyr_internal_error(...)
     19. │ └─rlang::abort(class = c(class, "dplyr:::internal_error"), dplyr_error_data = data)
     20. │   └─rlang:::signal_abort(cnd, .file)
     21. │     └─base::signalCondition(cnd)
     22. └─dplyr `<fn>`(`<dpl:::__>`)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       55. ├─dplyr::filter(., .[, x_var] >= 0)
       56. ├─dplyr:::filter.data.frame(., .[, x_var] >= 0)
       57. │ └─dplyr:::filter_rows(.data, ..., caller_env = caller_env())
       58. │   └─dplyr:::filter_eval(dots, mask = mask, error_call = error_call)
       59. │     ├─base::withCallingHandlers(...)
       60. │     └─mask$eval_all_filter(dots, env_filter)
       61. ├─dplyr:::dplyr_internal_error(...)
       62. │ └─rlang::abort(class = c(class, "dplyr:::internal_error"), dplyr_error_data = data)
       63. │   └─rlang:::signal_abort(cnd, .file)
       64. │     └─base::signalCondition(cnd)
       65. └─dplyr `<fn>`(`<dpl:::__>`)
      
      [ FAIL 10 | WARN 0 | SKIP 7 | PASS 512 ]
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

