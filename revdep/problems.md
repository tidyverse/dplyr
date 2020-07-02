# amt

<details>

* Version: 0.1.2
* Source code: https://github.com/cran/amt
* URL: https://github.com/jmsigner/amt
* Date/Publication: 2020-06-14 15:20:06 UTC
* Number of recursive dependencies: 166

Run `cloud_details(, "amt")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
      n       : integer
      geometry: sfc_POLYGON
    >`.
    ℹ It must be numeric or character.
    Backtrace:
         █
      1. └─`%>%`(...)
      2.   ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      3.   └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      4.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5.       └─`_fseq`(`_lhs`)
      6.         └─magrittr::freduce(value, `_function_list`)
      7.           ├─base::withVisible(function_list[[k]](value))
      8.           └─function_list[[k]](value)
      9.             ├─amt::hr_to_sf(., hr, id, n)
     10.             └─amt:::hr_to_sf.tbl_df(., hr, id, n)
     11.               ├─dplyr::select(x, !!col)
     12.               └─dplyr:::select.data.frame(x, !!col)
     13.                 └─tidyselect::eval_select(expr(c(...)), .data)
     14.                   └─t
    Execution halted
    ```

# anglr

<details>

* Version: 0.6.0
* Source code: https://github.com/cran/anglr
* URL: https://github.com/hypertidy/anglr
* BugReports: https://github.com/hypertidy/anglr/issues
* Date/Publication: 2020-05-13 23:40:12 UTC
* Number of recursive dependencies: 121

Run `cloud_details(, "anglr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(anglr)
      Warning messages:
      1: In rgl.init(initValue, onlyNULL) : RGL: unable to open X11 display
      2: 'rgl.init' failed, running with 'rgl.useNULL = TRUE'. 
      > 
      > test_check("anglr")
      ── 1. Failure: SC0 round trip suite works (@test-silicate-sanity.R#7)  ─────────
      `{ ... }` produced warnings.
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 80 | SKIPPED: 1 | WARNINGS: 1 | FAILED: 1 ]
      1. Failure: SC0 round trip suite works (@test-silicate-sanity.R#7) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking whether package ‘anglr’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: 'rgl.init' failed, running with 'rgl.useNULL = TRUE'.
    See ‘/tmp/workdir/anglr/new/anglr.Rcheck/00install.out’ for details.
    ```

# baguette

<details>

* Version: 0.0.1
* Source code: https://github.com/cran/baguette
* URL: https://github.com/tidymodels/baguette
* BugReports: https://github.com/tidymodels/baguette/issues
* Date/Publication: 2020-04-14 14:20:04 UTC
* Number of recursive dependencies: 110

Run `cloud_details(, "baguette")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        1. testthat::expect_warning(...)
       13. baguette:::bagger.formula(Class ~ ., data = two_class_dat, base_model = "MARS")
       14. baguette:::bagger_bridge(...)
       15. baguette:::mars_bagger(rs, control, ...)
       16. baguette:::check_for_disaster(rs)
       18. dplyr:::mutate.data.frame(...)
       19. dplyr:::mutate_cols(.data, ...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 148 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 2 ]
      1. Error: check mars opt (@test-mars.R#54) 
      2. Failure: bad inputs (@test-validation.R#138) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# c14bazAAR

<details>

* Version: 1.2.0
* Source code: https://github.com/cran/c14bazAAR
* URL: https://docs.ropensci.org/c14bazAAR, https://github.com/ropensci/c14bazAAR
* BugReports: https://github.com/ropensci/c14bazAAR/issues
* Date/Publication: 2020-01-12 16:50:02 UTC
* Number of recursive dependencies: 142

Run `cloud_details(, "c14bazAAR")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
        |                                                        
        |                                                  |   0%
        |                                                        
        |++                                                |   5%
        |                                                        
        |++++++++++++++++++++++++++++++++++++++++++++++++  |  95%
        |                                                        
        |++++++++++++++++++++++++++++++++++++++++++++++++++| 100%
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 83 | SKIPPED: 2 | WARNINGS: 0 | FAILED: 1 ]
      1. Failure: enforce_types does adjust variable types if necessary (@test_c14_date_list_enforce_types.R#31) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# coalitions

<details>

* Version: 0.6.12
* Source code: https://github.com/cran/coalitions
* URL: http://adibender.github.io/coalitions/
* BugReports: https://github.com/adibender/coalitions/issues
* Date/Publication: 2020-02-06 10:10:06 UTC
* Number of recursive dependencies: 86

Run `cloud_details(, "coalitions")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Check on survey2 isn't true.
      Must have exactly 11 cols, but has 9 cols
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 74 | SKIPPED: 0 | WARNINGS: 16 | FAILED: 7 ]
      1. Failure: State wide german scrapers work (@test-scrapers.R#16) 
      2. Failure: State wide german scrapers work (@test-scrapers.R#17) 
      3. Failure: State wide german scrapers work (@test-scrapers.R#18) 
      4. Failure: State wide german scrapers work (@test-scrapers.R#21) 
      5. Failure: State wide german scrapers work (@test-scrapers.R#26) 
      6. Failure: State wide german scrapers work (@test-scrapers.R#27) 
      7. Failure: State wide german scrapers work (@test-scrapers.R#35) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# easyalluvial

<details>

* Version: 0.2.3
* Source code: https://github.com/cran/easyalluvial
* URL: https://github.com/erblast/easyalluvial
* Date/Publication: 2020-05-07 08:40:20 UTC
* Number of recursive dependencies: 142

Run `cloud_details(, "easyalluvial")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      [1] "Number of flows: 17245"
      [1] "Original Dataframe reduced to 32 %"
      [1] "Maximum weight of a single flow 0.4 %"
      ── 1. Error: add_importance_plot (@test_plot_imp.R#37)  ────────────────────────
      'x' and 'units' must have length > 0
      Backtrace:
       1. easyalluvial::add_marginal_histograms(p, data_input = df, plot = F)
       4. grid::unit(rep(1, ncol), "null")
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 68 | SKIPPED: 45 | WARNINGS: 1 | FAILED: 1 ]
      1. Error: add_importance_plot (@test_plot_imp.R#37) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# eph

<details>

* Version: 0.4.0
* Source code: https://github.com/cran/eph
* URL: https://github.com/holatam/eph
* BugReports: https://github.com/holatam/eph/issues
* Date/Publication: 2020-06-25 08:00:02 UTC
* Number of recursive dependencies: 137

Run `cloud_details(, "eph")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Content type 'application/vnd.ms-excel' length 62976 bytes (61 KB)
      ==================================================
      downloaded 61 KB
      
      trying URL 'https://github.com/holatam/data/raw/master/eph/canasta/canastas.rds'
      Content type 'application/octet-stream' length 1769 bytes
      ==================================================
      downloaded 1769 bytes
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 17 | SKIPPED: 3 | WARNINGS: 2 | FAILED: 1 ]
      1. Error: Sin data (@test-get_microdata.R#23) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘readr’ ‘tidyverse’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked Latin-1 string
      Note: found 721 marked UTF-8 strings
    ```

# fabletools

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/fabletools
* URL: http://fabletools.tidyverts.org/, https://github.com/tidyverts/fabletools
* BugReports: https://github.com/tidyverts/fabletools/issues
* Date/Publication: 2020-06-15 23:40:08 UTC
* Number of recursive dependencies: 94

Run `cloud_details(, "fabletools")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Loading required package: fabletools
      ── 1. Error: In-sample accuracy  ───────────────────────────────────────────────
      Can't recycle `..1` (size 5) to match `fit` (size 1258).
      Backtrace:
        1. testthat::expect_warning(...)
       18. vctrs::stop_incompatible_size(...)
       19. vctrs:::stop_incompatible(...)
       20. vctrs:::stop_vctrs(...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 274 | SKIPPED: 1 | WARNINGS: 0 | FAILED: 1 ]
      1. Error: In-sample accuracy 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘lifecycle’
      All declared Imports should be used.
    ```

# fauxnaif

<details>

* Version: 0.5.6
* Source code: https://github.com/cran/fauxnaif
* URL: https://github.com/rossellhayes/fauxnaif
* BugReports: https://github.com/rossellhayes/fauxnaif/issues
* Date/Publication: 2020-03-01 12:20:10 UTC
* Number of recursive dependencies: 62

Run `cloud_details(, "fauxnaif")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 69 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 18 ]
      1. Failure: two-sided formula produces warning (@test-scoped_na_if.R#68) 
      2. Failure: two-sided formula produces warning (@test-scoped_na_if.R#69) 
      3. Failure: two-sided formula produces warning (@test-scoped_na_if.R#70) 
      4. Failure: two-sided formula produces warning (@test-scoped_na_if.R#71) 
      5. Failure: two-sided formula produces warning (@test-scoped_na_if.R#72) 
      6. Failure: two-sided formula produces warning (@test-scoped_na_if.R#73) 
      7. Failure: non-coercible argument produces warning (@test-scoped_na_if.R#78) 
      8. Failure: non-coercible argument produces warning (@test-scoped_na_if.R#79) 
      9. Failure: non-coercible argument produces warning (@test-scoped_na_if.R#80) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

# forestmangr

<details>

* Version: 0.9.2
* Source code: https://github.com/cran/forestmangr
* URL: https://github.com/sollano/forestmangr#readme
* BugReports: https://github.com/sollano/forestmangr/issues
* Date/Publication: 2020-04-07 14:00:02 UTC
* Number of recursive dependencies: 124

Run `cloud_details(, "forestmangr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    ℹ Input `est_n` must be size 1, not 0.
    ℹ The error occured in group 1: strata = 1.
    Backtrace:
         █
      1. ├─`%>%`(...)
      2. │ └─base::eval(lhs, parent, parent)
      3. │   └─base::eval(lhs, parent, parent)
      4. └─forestmangr::nls_table(...)
      5.   └─`%>%`(...)
      6.     ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      7.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      8.       └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      9.         └─forestmangr:::`_fseq`(`_lhs`)
     10.           └─magrittr::freduce(value, `_function_list`)
     11.             └─function_list[[i]](value)
     12.               ├─dplyr::mutate(...)
     13.               └─dplyr:::mutate.data.frame(...)
     14.                 └─dplyr:::mutate_cols(.data, ...)
     15.                   └─base::tryCatch(...)
     16.   
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘forcats’
      All declared Imports should be used.
    ```

# ggpubr

<details>

* Version: 0.4.0
* Source code: https://github.com/cran/ggpubr
* URL: https://rpkgs.datanovia.com/ggpubr/
* BugReports: https://github.com/kassambara/ggpubr/issues
* Date/Publication: 2020-06-27 06:20:02 UTC
* Number of recursive dependencies: 102

Run `cloud_details(, "ggpubr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    ℹ Input `ci` is `abs(stats::qt(alpha/2, .data$n - 1) * .data$se)`.
    ℹ Input `data` is `map(.data$data, .f, ...)`.
    Error: Can't subset columns that don't exist.
    ✖ Column `n` doesn't exist.
    Backtrace:
         █
      1. ├─ggpubr::ggsummarystats(...)
      2. │ ├─base::do.call(ggsummarystats_core, env)
      3. │ └─(function (data, x, y, summaries = c("n", "median", "iqr"), ggfunc = ggboxplot, ...
      4. │   └─ggpubr::ggsummarytable(...)
      5. │     └─`%>%`(...)
      6. │       ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      7. │       └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      8. │         └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      9. │           └─ggpubr:::`_fseq`(`_lhs`)
     10. │             └─magrittr::freduce(value, `_function_list`)
     11. │               └─function_list[[i]](value)
     12. │                 ├─tidyr::unite(., col = "label", !!!syms(y), sep = "\n")
     13. │                 └─tidyr:::unite.data.frame(., col = "label", !!!syms(y), sep = "\n")
     14. │                   └─tidyselect::eval_select(expr(c
    Execution halted
    ```

# gravity

<details>

* Version: 0.9.8
* Source code: https://github.com/cran/gravity
* URL: http://pachamaltese.github.io/gravity
* Date/Publication: 2019-05-08 21:40:04 UTC
* Number of recursive dependencies: 73

Run `cloud_details(, "gravity")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Error: Problem with `mutate()` input `flows_ek2`.
    ✖ object 'exportmin' not found
    ℹ Input `flows_ek2` is `ifelse(flow > 0, flows_ek1, exportmin)`.
    ℹ The error occured in group 1: iso_d = "AFG".
    Backtrace:
         █
      1. └─gravity::ek_tobit(...)
      2.   └─`%>%`(...)
      3.     ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      4.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5.       └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      6.         └─gravity:::`_fseq`(`_lhs`)
      7.           └─magrittr::freduce(value, `_function_list`)
      8.             └─function_list[[i]](value)
      9.               ├─dplyr::mutate(...)
     10.               └─dplyr:::mutate.data.frame(...)
     11.                 └─dplyr:::mutate_cols(.data, ...)
    <parent: error/simpleError>
    Backtrace:
    █
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ✖ object 'exportmin' not found
      ℹ Input `flows_ek2` is `ifelse(flow > 0, flows_ek1, exportmin)`.
      ℹ The error occured in group 1: iso_d = "AFG".
      Backtrace:
        1. gravity::ek_tobit(...)
        2. dplyr::mutate(...)
        9. dplyr::mutate(...)
       11. dplyr:::mutate_cols(.data, ...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 14 | SKIPPED: 0 | WARNINGS: 2 | FAILED: 1 ]
      1. Error: EK Tobit returns a valid output (@test-ek_tobit.R#20) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# pmdplyr

<details>

* Version: 0.3.3
* Source code: https://github.com/cran/pmdplyr
* URL: https://nickch-k.github.io/pmdplyr, https://github.com/NickCH-K/pmdplyr
* BugReports: https://github.com/NickCH-K/pmdplyr/issues
* Date/Publication: 2020-05-30 07:30:02 UTC
* Number of recursive dependencies: 105

Run `cloud_details(, "pmdplyr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        2. dplyr::mutate(., t = time_variable(date, .method = "week", .breaks = 1))
       10. dplyr::pull(., t)
       14. tidyselect::vars_pull(names(.data), !!enquo(var))
       15. tidyselect:::pull_as_location2(loc, n, vars)
       23. vctrs::vec_as_subscript2(i, arg = "var", logical = "error")
       24. vctrs:::result_get(...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 287 | SKIPPED: 0 | WARNINGS: 3 | FAILED: 3 ]
      1. Failure: time_variable input failstates (@test-bad_input.R#298) 
      2. Failure: time_variable input failstates (@test-bad_input.R#308) 
      3. Error: time_variable works (@test-time_variable.R#35) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# psychmeta

<details>

* Version: 2.3.10
* Source code: https://github.com/cran/psychmeta
* BugReports: https://github.com/psychmeta/psychmeta/issues
* Date/Publication: 2020-06-07 22:50:03 UTC
* Number of recursive dependencies: 84

Run `cloud_details(, "psychmeta")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    ℹ The error occured in group 1: pair_id = 1, construct_x = "X", construct_y = "Y".
    Error: Column `anova` not found in `.data`
    Backtrace:
         █
      1. ├─stats::anova(ma_obj)
      2. ├─psychmeta:::anova.ma_psychmeta(ma_obj)
      3. │ └─`%>%`(...)
      4. │   ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      5. │   └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      6. │     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      7. │       └─psychmeta:::`_fseq`(`_lhs`)
      8. │         └─magrittr::freduce(value, `_function_list`)
      9. │           └─function_list[[i]](value)
     10. │             ├─dplyr::filter(., !is.na(.data$anova))
     11. │             └─dplyr:::filter.data.frame(., !is.na(.data$anova))
     12. │               └─dplyr:::filter_rows(.data, ...)
     13. │                 ├─base::tryCatch(...)
     14. │                 │ └─base:::tryCatchList(expr, classes, parentenv, handlers)
     15. │                 │   └─base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
     16. │            
    Execution halted
    ```

# SEERaBomb

<details>

* Version: 2019.2
* Source code: https://github.com/cran/SEERaBomb
* URL: http://epbi-radivot.cwru.edu/SEERaBomb/SEERaBomb.html
* Date/Publication: 2019-12-12 18:50:03 UTC
* Number of recursive dependencies: 151

Run `cloud_details(, "SEERaBomb")` for more info

</details>

## Newly broken

*   checking whether package ‘SEERaBomb’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: 'rgl.init' failed, running with 'rgl.useNULL = TRUE'.
    See ‘/tmp/workdir/SEERaBomb/new/SEERaBomb.Rcheck/00install.out’ for details.
    ```

# sigminer

<details>

* Version: 1.0.7
* Source code: https://github.com/cran/sigminer
* URL: https://github.com/ShixiangWang/sigminer
* BugReports: https://github.com/ShixiangWang/sigminer/issues
* Date/Publication: 2020-06-17 05:20:02 UTC
* Number of recursive dependencies: 186

Run `cloud_details(, "sigminer")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### Name: show_sig_feature_corrplot
    > ### Title: Draw Corrplot for Signature Exposures and Other Features
    > ### Aliases: show_sig_feature_corrplot
    > 
    > ### ** Examples
    > 
    > 
    > # The data is generated from Wang, Shixiang et al.
    > load(system.file("extdata", "asso_data.RData",
    +   package = "sigminer", mustWork = TRUE
    + ))
    > 
    > p <- show_sig_feature_corrplot(tidy_data.seqz.feature, p_val = 0.05)
    Warning: Problem with `mutate()` input `gg`.
    ✖ Using size for a discrete variable is not advised.
    ℹ Input `gg` is `purrr::map2(.data$data, .data$type, .plot_cor)`.
    ℹ The error occured in group 1: type = "ca".
    Warning: Unknown or uninitialised column: `gg`.
    Error in names(gglist) <- data$type : attempt to set an attribute on NULL
    Calls: show_sig_feature_corrplot
    Execution halted
    ```

# survminer

<details>

* Version: 0.4.7
* Source code: https://github.com/cran/survminer
* URL: http://www.sthda.com/english/rpkgs/survminer/
* BugReports: https://github.com/kassambara/survminer/issues
* Date/Publication: 2020-05-28 11:40:02 UTC
* Number of recursive dependencies: 118

Run `cloud_details(, "survminer")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_warnings()` to see where this warning was generated.
    ℹ Input `survtable` is `purrr::map2(...)`.
    Warning: Unknown or uninitialised column: `survtable`.
    Warning: Unknown or uninitialised column: `survtable`.
    Error: Assigned data `map(...)` must be compatible with existing data.
    ✖ Existing data has 2 rows.
    ✖ Assigned data has 0 rows.
    ℹ Only vectors of size 1 are recycled.
    Backtrace:
        █
     1. └─survminer::ggsurvplot_combine(fit, demo.data)
     2.   ├─base::`$<-`(`*tmp*`, "survtable", value = list())
     3.   └─tibble:::`$<-.tbl_df`(`*tmp*`, "survtable", value = list())
     4.     └─tibble:::tbl_subassign(...)
     5.       └─tibble:::vectbl_recycle_rhs(...)
     6.         └─base::tryCatch(...)
     7.           └─base:::tryCatchList(expr, classes, parentenv, handlers)
     8.             └─base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
     9.               └─value[[3L]](cond)
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        doc   4.8Mb
    ```

# SwimmeR

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/SwimmeR
* Date/Publication: 2020-04-10 04:40:03 UTC
* Number of recursive dependencies: 73

Run `cloud_details(, "SwimmeR")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Backtrace:
       1. dplyr::bind_rows(df_test, .id = "column_label")
       3. vctrs::vec_default_ptype2(...)
       4. vctrs::stop_incompatible_type(...)
       5. vctrs:::stop_incompatible(...)
       6. vctrs:::stop_vctrs(...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 7 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 3 ]
      1. Error: Swim_Parse works 3 (@test-Swim_Parse_works.R#29) 
      2. Error: Swim_Parse works USA (@test-Swim_Parse_works.R#55) 
      3. Error: Swim_Parse works list 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘ggplot2’ ‘scales’ ‘utils’
      All declared Imports should be used.
    ```

# tibbletime

<details>

* Version: 0.1.5
* Source code: https://github.com/cran/tibbletime
* URL: https://github.com/business-science/tibbletime
* BugReports: https://github.com/business-science/tibbletime/issues
* Date/Publication: 2020-06-18 19:00:02 UTC
* Number of recursive dependencies: 62

Run `cloud_details(, "tibbletime")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Error: Column `.time_group` not found in `.data`
    Backtrace:
         █
      1. ├─tibbletime::as_period(FB, "2 weeks")
      2. ├─tibbletime:::as_period.tbl_time(FB, "2 weeks")
      3. │ ├─dplyr::filter(...)
      4. │ ├─tibbletime:::filter.tbl_time(...)
      5. │ │ ├─tibbletime::reconstruct(NextMethod(), copy_.data)
      6. │ │ └─tibbletime:::reconstruct.tbl_time(NextMethod(), copy_.data)
      7. │ ├─base::NextMethod()
      8. │ └─dplyr:::filter.data.frame(...)
      9. │   └─dplyr:::filter_rows(.data, ...)
     10. │     ├─base::tryCatch(...)
     11. │     │ └─base:::tryCatchList(expr, classes, parentenv, handlers)
     12. │     │   └─base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
     13. │     │     └─base:::doTryCatch(return(expr), name, parentenv, handler)
     14. │     └─mask$eval_all_filter(dots, env_filter)
     15. ├─.time_group
     16. ├─rlang:::`$.rlang_data_pronoun`(.data, .time_group)
     17. │ └─rlang:::data_pronou
    Execution halted
    ```

# tidyboot

<details>

* Version: 0.1.1
* Source code: https://github.com/cran/tidyboot
* URL: https://github.com/langcog/tidyboot
* BugReports: http://github.com/langcog/tidyboot/issues
* Date/Publication: 2018-03-14 04:13:49 UTC
* Number of recursive dependencies: 31

Run `cloud_details(, "tidyboot")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Please use `tibble()` instead.
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_warnings()` to see where this warning was generated.
    > gauss2 <- data_frame(value = rnorm(500, mean = 2, sd = 3), condition = 2)
    > df <- bind_rows(gauss1, gauss2)
    > df %>% group_by(condition) %>%
    +   tidyboot(summary_function = function(x) x %>% summarise(mean = mean(value)),
    +            statistics_functions = function(x) x %>%
    +            summarise_at(vars(mean), funs(ci_upper, mean, ci_lower)))
    `summarise()` ungrouping output (override with `.groups` argument)
    Warning: Problem with `mutate()` input `strap`.
    ✖ `as_data_frame()` is deprecated as of tibble 2.0.0.
    Please use `as_tibble()` instead.
    The signature and semantics have changed, see `?as_tibble`.
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_warnings()` to see where this warning was generated.
    ℹ Input `strap` is `purrr::map(strap, dplyr::as_data_frame)`.
    Warning: `cols` is now required when using unnest().
    Please use `cols = c(strap)`
    Error: Input must be list of vectors
    Execution halted
    ```

# unheadr

<details>

* Version: 0.2.1
* Source code: https://github.com/cran/unheadr
* URL: https://github.com/luisDVA/unheadr, https://unheadr.liomys.mx/
* BugReports: https://github.com/luisDVA/unheadr/issues
* Date/Publication: 2020-03-04 01:00:02 UTC
* Number of recursive dependencies: 65

Run `cloud_details(, "unheadr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > test_check("unheadr")
      ── 1. Error: target variable is called 'regex' (@test-untangle2.R#8)  ──────────
      Can't subset columns that don't exist.
      ✖ Column `new_col` doesn't exist.
      Backtrace:
        1. testthat::expect_warning(...)
       27. vctrs:::stop_subscript_oob(...)
       28. vctrs:::stop_subscript(...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 15 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Error: target variable is called 'regex' (@test-untangle2.R#8) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# unpivotr

<details>

* Version: 0.6.0
* Source code: https://github.com/cran/unpivotr
* URL: https://github.com/nacnudus/unpivotr
* BugReports: https://github.com/nacnudus/unpivotr/issues
* Date/Publication: 2020-05-08 17:50:02 UTC
* Number of recursive dependencies: 77

Run `cloud_details(, "unpivotr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(unpivotr)
      > 
      > test_check("unpivotr")
      ── 1. Failure: unpack() works on common data types (@test-pack.R#53)  ──────────
      Not all data returned
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 256 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Failure: unpack() works on common data types (@test-pack.R#53) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# varsExplore

<details>

* Version: 0.1.0
* Source code: https://github.com/cran/varsExplore
* Date/Publication: 2019-10-03 07:40:02 UTC
* Number of recursive dependencies: 60

Run `cloud_details(, "varsExplore")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘varsExplore-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: vars_explore
    > ### Title: Searchable variable explorer with labelled variables
    > ### Aliases: vars_explore
    > 
    > ### ** Examples
    > 
    > 
    > qog <- rio::import("http://www.qogdata.pol.gu.se/dataarchive/qog_bas_cs_jan18.dta")
    > qog_summary <- vars_explore(qog, silent = FALSE, viewer = FALSE)
    Warning: Problem with `mutate()` input `Mean`.
    ✖ argument is not numeric or logical: returning NA
    ℹ Input `Mean` is `purrr::map_dbl(df, ~round(mean(.x, na.rm = TRUE), digits))`.
    Error in `$<-.data.frame`(`*tmp*`, "Min", value = numeric(0)) : 
      replacement has 0 rows, data has 471
    Calls: vars_explore -> $<- -> $<-.data.frame
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘rio’
      All declared Imports should be used.
    ```

# viafr

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/viafr
* URL: https://github.com/stefanieschneider/viafr
* BugReports: https://github.com/stefanieschneider/viafr/issues
* Date/Publication: 2020-04-22 13:10:02 UTC
* Number of recursive dependencies: 54

Run `cloud_details(, "viafr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       44. rlang:::abort_data_pronoun(x)
      
      ── 2. Error: valid query (@test_suggest.R#33)  ─────────────────────────────────
      Column `source_ids` not found in `.data`
      Backtrace:
        1. viafr::viaf_suggest("rembrandt")
       40. rlang:::abort_data_pronoun(x)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 40 | SKIPPED: 0 | WARNINGS: 3 | FAILED: 2 ]
      1. Error: query list (@test_suggest.R#4) 
      2. Error: valid query (@test_suggest.R#33) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2 marked UTF-8 strings
    ```

# xpose

<details>

* Version: 0.4.10
* Source code: https://github.com/cran/xpose
* URL: https://github.com/UUPharmacometrics/xpose
* BugReports: https://github.com/UUPharmacometrics/xpose/issues
* Date/Publication: 2020-06-08 09:50:03 UTC
* Number of recursive dependencies: 95

Run `cloud_details(, "xpose")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 512 | SKIPPED: 7 | WARNINGS: 13 | FAILED: 9 ]
      1. Error: read_nm_files handles one file with inappropriate format (@test-read_nm_files.R#36) 
      2. Failure: error is returned when missing table header (@test-read_nm_tables.R#52) 
      3. Failure: set_vars_type works properly (@test-set_vars.R#17) 
      4. Failure: set_vars_type works properly (@test-set_vars.R#18) 
      5. Failure: set_vars_type works properly (@test-set_vars.R#19) 
      6. Failure: set_vars_type works properly (@test-set_vars.R#20) 
      7. Failure: set_vars_units and set_vars_label works properly (@test-set_vars.R#29) 
      8. Failure: set_vars_units and set_vars_label works properly (@test-set_vars.R#30) 
      9. Failure: properly handles errors in tables (@test-xpose_data.R#64) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

