# Andromeda

<details>

* Version: 0.1.3
* Source code: https://github.com/cran/Andromeda
* URL: https://github.com/OHDSI/Andromeda
* BugReports: https://github.com/OHDSI/Andromeda/issues
* Date/Publication: 2020-05-11 11:10:27 UTC
* Number of recursive dependencies: 57

Run `cloud_details(, "Andromeda")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘Andromeda-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: appendToTable
    > ### Title: Append to an Andromeda table
    > ### Aliases: appendToTable
    > 
    > ### ** Examples
    > 
    > andr <- andromeda(cars = cars)
    > nrow(andr$cars)
    [1] 50
    > # [1] 50
    > 
    > appendToTable(andr$cars, cars)
    Error in (function (classes, fdef, mtable)  : 
      unable to find an inherited method for function ‘dbWriteTable’ for signature ‘"Andromeda", "ident", "data.frame"’
    Calls: appendToTable -> <Anonymous> -> <Anonymous>
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Backtrace:
       1. Andromeda::batchApply(...)
       6. Andromeda::appendToTable(andromeda$cars2, batch)
       7. RSQLite::dbWriteTable(...)
      
      Disconnected Andromeda. This data object can no longer be used
      Disconnected Andromeda. This data object can no longer be used
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 55 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 3 ]
      1. Error: Append from other andromeda (@test-appending.R#21) 
      2. Error: Append from data frame (@test-appending.R#35) 
      3. Error: batchApply safe mode (@test-batching.R#30) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘Rcpp’
      All declared Imports should be used.
    ```

# anomalize

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/anomalize
* URL: https://github.com/business-science/anomalize
* BugReports: https://github.com/business-science/anomalize/issues
* Date/Publication: 2019-09-21 04:10:03 UTC
* Number of recursive dependencies: 161

Run `cloud_details(, "anomalize")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    
    Attaching package: ‘dplyr’
    
    The following objects are masked from ‘package:stats’:
    
        filter, lag
    
    The following objects are masked from ‘package:base’:
    
        intersect, setdiff, setequal, union
    
    > 
    > tidyverse_cran_downloads %>%
    +     ungroup() %>%
    +     filter(package == "tidyquant") %>%
    +     decompose_stl(count)
    Error in time_frequency(data, period = frequency, message = message) : 
      Error time_frequency(): Cannot use on a grouped data frame.
    Frequency should be performed on a single time series.
    Calls: %>% ... withVisible -> <Anonymous> -> decompose_stl -> time_frequency
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 53 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 14 ]
      1. Error: returns a ggplot (@test-plot_anomalies.R#8) 
      2. Error: returns a ggplot (@test-plot_anomaly_decomposition.R#10) 
      3. Error: grouped_tbl_time works (@test-time_apply.R#11) 
      4. Error: tbl_time works (@test-time_apply.R#17) 
      5. Failure: single tbl_df (@test-time_decompose.R#20) 
      6. Error: time_frequency works: period = 'auto' (@test-time_frequency.R#26) 
      7. Error: time_frequency works: period = '1 month' (@test-time_frequency.R#35) 
      8. Error: time_frequency works: period = 5 (@test-time_frequency.R#44) 
      9. Error: time_trend works: period = 'auto' (@test-time_frequency.R#55) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

# arrow

<details>

* Version: 0.17.0
* Source code: https://github.com/cran/arrow
* URL: https://github.com/apache/arrow/, https://arrow.apache.org/docs/r
* BugReports: https://issues.apache.org/jira/projects/ARROW/issues
* Date/Publication: 2020-04-21 18:10:02 UTC
* Number of recursive dependencies: 58

Run `cloud_details(, "arrow")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        2. dplyr::select(., string = chr, int)
       10. dplyr::collect(.)
       13. arrow:::restore_dplyr_features(df, x)
       14. dplyr::grouped_df(df, dplyr::groups(query))
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 1848 | SKIPPED: 19 | WARNINGS: 1 | FAILED: 5 ]
      1. Error: transmute (@test-dplyr.R#231) 
      2. Error: group_by groupings are recorded (@test-dplyr.R#242) 
      3. Error: Empty select still includes the group_by columns (@test-dplyr.R#276) 
      4. Error: arrange (@test-dplyr.R#283) 
      5. Error: group_by then rename (@test-dplyr.R#333) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 54.8Mb
      sub-directories of 1Mb or more:
        R      3.2Mb
        libs  51.1Mb
    ```

# bayesplot

<details>

* Version: 1.7.1
* Source code: https://github.com/cran/bayesplot
* URL: https://mc-stan.org/bayesplot
* BugReports: https://github.com/stan-dev/bayesplot/issues/
* Date/Publication: 2019-12-01 23:00:26 UTC
* Number of recursive dependencies: 143

Run `cloud_details(, "bayesplot")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 1010 | SKIPPED: 35 | WARNINGS: 15 | FAILED: 12 ]
      1. Failure: mcmc_intervals_data computes quantiles (@test-mcmc-intervals.R#24) 
      2. Failure: mcmc_intervals_data computes quantiles (@test-mcmc-intervals.R#25) 
      3. Failure: ppc_intervals_data does math correctly (@test-ppc-intervals.R#60) 
      4. Failure: ppc_intervals_data does math correctly (@test-ppc-intervals.R#61) 
      5. Failure: ppc_intervals_data does math correctly (@test-ppc-intervals.R#62) 
      6. Failure: ppc_intervals_data does math correctly (@test-ppc-intervals.R#63) 
      7. Failure: ppc_intervals_data does math correctly (@test-ppc-intervals.R#64) 
      8. Failure: ppc_intervals_data does math correctly (@test-ppc-intervals.R#76) 
      9. Failure: ppc_intervals_data does math correctly (@test-ppc-intervals.R#77) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.6Mb
      sub-directories of 1Mb or more:
        R     1.8Mb
        doc   4.1Mb
    ```

# bigrquery

<details>

* Version: 1.3.0
* Source code: https://github.com/cran/bigrquery
* URL: https://github.com/rstats-db/bigrquery
* BugReports: https://github.com/rstats-db/bigrquery/issues
* Date/Publication: 2020-05-08 08:50:11 UTC
* Number of recursive dependencies: 63

Run `cloud_details(, "bigrquery")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      ── 3. Failure: %||% translates to IFNULL (@test-dplyr.R#147)  ──────────────────
      sql$select[[2]] not equal to "IFNULL(`x`, 2)".
      1/1 mismatches
      x[1]: "IFNULL(\"x\", 2)"
      y[1]: "IFNULL(`x`, 2)"
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 99 | SKIPPED: 59 | WARNINGS: 0 | FAILED: 3 ]
      1. Failure: casting uses bigquery types (@test-dplyr.R#131) 
      2. Failure: casting uses bigquery types (@test-dplyr.R#132) 
      3. Failure: %||% translates to IFNULL (@test-dplyr.R#147) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# blorr

<details>

* Version: 0.2.2
* Source code: https://github.com/cran/blorr
* URL: URL: https://blorr.rsquaredacademy.com/, https://github.com/rsquaredacademy/blorr
* BugReports: https://github.com/rsquaredacademy/blorr/issues
* Date/Publication: 2020-02-03 11:40:02 UTC
* Number of recursive dependencies: 167

Run `cloud_details(, "blorr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘blorr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: blr_decile_capture_rate
    > ### Title: Event rate by decile
    > ### Aliases: blr_decile_capture_rate
    > 
    > ### ** Examples
    > 
    > model <- glm(honcomp ~ female + read + science, data = hsb2,
    +              family = binomial(link = 'logit'))
    > gt <- blr_gains_table(model)
    New names:
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
                           Added/                                                  
      Step    Variable    Removed        AIC           BIC           C(p)       
      ----------------------------------------------------------------------
         1       x6       addition     18869.627     18885.434    18865.6270    
         2       x1       addition     18571.376     18595.087    18565.3760    
         3       x3       addition     18016.724     18048.338    18008.7240    
         4       x2       addition     16642.374     16681.891    16632.3740    
         5       x5       addition     16640.883     16688.304    16628.8830    
         6       x6       removal      16639.219     16678.736    16629.2190    
      ----------------------------------------------------------------------══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 75 | SKIPPED: 28 | WARNINGS: 1 | FAILED: 1 ]
      1. Error: output from blr_gains_table is as expected (@test-blr-gains-table.R#9) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘lmtest’
    ```

# broom.mixed

<details>

* Version: 0.2.5
* Source code: https://github.com/cran/broom.mixed
* URL: http://github.com/bbolker/broom.mixed
* BugReports: http://github.com/bbolker/broom.mixed/issues
* Date/Publication: 2020-04-19 04:50:08 UTC
* Number of recursive dependencies: 146

Run `cloud_details(, "broom.mixed")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘test-all.R’
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      x[1]: "(Intercept)"
      y[1]: "1"
      
      x[2]: "sin(2 * pi * Time)"
      y[2]: "2"
      
      x[3]: "cos(2 * pi * Time)"
      y[3]: "3"
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 237 | SKIPPED: 0 | WARNINGS: 12 | FAILED: 1 ]
      1. Failure: basic gls tidying (@test-nlme.R#146) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Missing or unexported object: ‘dplyr::as.tbl_cube’
    ```

# cattonum

<details>

* Version: 0.0.4
* Source code: https://github.com/cran/cattonum
* URL: https://github.com/bfgray3/cattonum
* BugReports: https://github.com/bfgray3/cattonum/issues
* Date/Publication: 2020-02-09 12:30:06 UTC
* Number of recursive dependencies: 71

Run `cloud_details(, "cattonum")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 242 | SKIPPED: 4 | WARNINGS: 0 | FAILED: 13 ]
      1. Failure: catto_mean: one tibble training column. (@test-mean.R#63) 
      2. Failure: catto_mean: one tibble training column. (@test-mean.R#63) 
      3. Failure: catto_mean: one tibble training column. (@test-mean.R#63) 
      4. Failure: catto_mean: one tibble training column. (@test-mean.R#67) 
      5. Failure: catto_mean: one tibble training column. (@test-mean.R#67) 
      6. Failure: catto_mean: one tibble training column. (@test-mean.R#67) 
      7. Failure: catto_mean() correctly encodes tibble with logicals. (@test-mean.R#98) 
      8. Failure: catto_median(): one tibble training column. (@test-median.R#60) 
      9. Failure: catto_median(): one tibble training column. (@test-median.R#60) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

# codebook

<details>

* Version: 0.8.2
* Source code: https://github.com/cran/codebook
* URL: https://github.com/rubenarslan/codebook
* BugReports: https://github.com/rubenarslan/codebook/issues
* Date/Publication: 2020-01-09 16:20:07 UTC
* Number of recursive dependencies: 178

Run `cloud_details(, "codebook")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > 
    > ### ** Examples
    > 
    > # will generate figures in a temporary directory
    > old_base_dir <- knitr::opts_knit$get("base.dir")
    > knitr::opts_knit$set(base.dir = tempdir())
    > on.exit(knitr::opts_knit$set(base.dir = old_base_dir))
    > data("bfi")
    > bfi <- bfi[, c("BFIK_open_1", "BFIK_open_1")]
    > md <- codebook(bfi, survey_repetition = "single", metadata_table = FALSE)
    No missing values.
    Error: Argument 1 must be a data frame or a named atomic vector.
    Backtrace:
        █
     1. └─codebook::codebook(bfi, survey_repetition = "single", metadata_table = FALSE)
     2.   └─codebook::metadata_jsonld(results)
     3.     └─codebook::metadata_list(results)
     4.       └─codebook::codebook_table(results)
     5.         └─codebook:::skim_to_wide_labelled(results)
     6.           └─dplyr::bind_rows(...)
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘graphics’ ‘jsonlite’ ‘pander’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 65 marked UTF-8 strings
    ```

# CollapseLevels

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/CollapseLevels
* Date/Publication: 2017-12-04 10:30:12 UTC
* Number of recursive dependencies: 65

Run `cloud_details(, "CollapseLevels")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    ✖ could not find function "n"
    ℹ Input `tot` is `n()`.
    ℹ The error occured in group 1: Account_Balance = "A11".
    Backtrace:
         █
      1. └─CollapseLevels::IVCalc(German_Credit, resp = "Good_Bad", bins = 10)
      2.   └─d %>% dplyr::group_by_(naml) %>% dplyr::summarise(tot = n())
      3.     ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      4.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5.       └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      6.         └─CollapseLevels:::`_fseq`(`_lhs`)
      7.           └─magrittr::freduce(value, `_function_list`)
      8.             ├─base::withVisible(function_list[[k]](value))
      9.             └─function_list[[k]](value)
     10.               ├─dplyr::summarise(., tot = n())
     11.               └─dplyr:::summarise.grouped_df(., tot = n())
     12.                 └─dplyr:::summarise_cols(.data, ...)
    <parent: error/simpleError>
    Backtrace:
    █
    Execution halted
    ```

# correlationfunnel

<details>

* Version: 0.1.0
* Source code: https://github.com/cran/correlationfunnel
* URL: https://github.com/business-science/correlationfunnel
* BugReports: https://github.com/business-science/correlationfunnel/issues
* Date/Publication: 2019-08-06 09:30:09 UTC
* Number of recursive dependencies: 97

Run `cloud_details(, "correlationfunnel")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      ── 4. Failure: Check correlation (@test-correlate.R#61)  ───────────────────────
      nrow(marketing_correlated_tbl) not equal to 74.
      1/1 mismatches
      [1] 65 - 74 == -9
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 12 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 4 ]
      1. Error: Check binarize - numeric (@test-binarize.R#47) 
      2. Error: Check binarize - numeric (@test-binarize.R#45) 
      3. Error: (unknown) (@test-binarize.R#45) 
      4. Failure: Check correlation (@test-correlate.R#61) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.4Mb
      sub-directories of 1Mb or more:
        doc    3.0Mb
        help   1.6Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘utils’
      All declared Imports should be used.
    ```

# cutpointr

<details>

* Version: 1.0.2
* Source code: https://github.com/cran/cutpointr
* URL: https://github.com/thie1e/cutpointr
* BugReports: https://github.com/thie1e/cutpointr/issues
* Date/Publication: 2020-04-14 08:50:10 UTC
* Number of recursive dependencies: 78

Run `cloud_details(, "cutpointr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      names for target but not for current
      
      ── 4. Failure: boot_test works correctly (@test-cutpointr.R#1428)  ─────────────
      round(bt$p_adj, 3) not equal to c(1, 0.647, 0.731).
      names for target but not for current
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 499 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 4 ]
      1. Failure: boot_ci works correctly (@test-cutpointr.R#1394) 
      2. Failure: boot_test works correctly (@test-cutpointr.R#1425) 
      3. Failure: boot_test works correctly (@test-cutpointr.R#1426) 
      4. Failure: boot_test works correctly (@test-cutpointr.R#1428) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# cvms

<details>

* Version: 1.0.1
* Source code: https://github.com/cran/cvms
* URL: https://github.com/ludvigolsen/cvms
* BugReports: https://github.com/ludvigolsen/cvms/issues
* Date/Publication: 2020-04-19 09:30:02 UTC
* Number of recursive dependencies: 120

Run `cloud_details(, "cvms")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 3757 | SKIPPED: 61 | WARNINGS: 6 | FAILED: 12 ]
      1. Failure: the grid order is as expected with create_computation_grid() (@test_compational_grid.R#260) 
      2. Failure: multinomial evaluations with one predicted class column is correctly unpacked in evaluate() (@test_evaluate.R#1361) 
      3. Failure: multinomial evaluations with one predicted class column is correctly unpacked in evaluate() (@test_evaluate.R#1369) 
      4. Failure: multinomial evaluations with one predicted class column is correctly unpacked in evaluate() (@test_evaluate.R#1377) 
      5. Failure: multinomial evaluations with one predicted class column is correctly unpacked in evaluate() (@test_evaluate.R#1385) 
      6. Failure: multinomial evaluations with one predicted class column is correctly unpacked in evaluate() (@test_evaluate.R#1393) 
      7. Failure: evaluate() and confusion_matrix() has same metric values (@test_evaluate.R#4015) 
      8. Failure: evaluate() and confusion_matrix() has same metric values (@test_evaluate.R#4054) 
      9. Failure: evaluate() and evaluate_residuals() has same metric values (@test_evaluate.R#4104) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

# dat

<details>

* Version: 0.4.0
* Source code: https://github.com/cran/dat
* BugReports: https://github.com/wahani/dat/issues
* Date/Publication: 2018-01-20 15:36:18 UTC
* Number of recursive dependencies: 72

Run `cloud_details(, "dat")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > 
    > data("airquality")
    > dat <- as.DataFrame(airquality)
    > dat[~ Month > 4, ][meanWind ~ mean(Wind), sby = "Month"]["meanWind"]
    Warning: `group_by_()` is deprecated as of dplyr 0.7.0.
    Please use `group_by()` instead.
    See vignette('programming') for more help
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_warnings()` to see where this warning was generated.
    Warning: `summarise_()` is deprecated as of dplyr 0.7.0.
    Please use `summarise()` instead.
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_warnings()` to see where this warning was generated.
    Warning: `select_()` is deprecated as of dplyr 0.7.0.
    Please use `select()` instead.
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_warnings()` to see where this warning was generated.
    Error in (function (classes, fdef, mtable)  : 
      unable to find an inherited method for function ‘handleCols’ for signature ‘"DataFrame", "NULL", "integer", "NULL", "NULL"’
    Calls: [ ... freduce -> <Anonymous> -> handleCols -> <Anonymous>
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       19. dplyr:::select.data.frame(x, dplyr::matches(j))
       22. dat:::`[.DataFrame`(.data, loc)
        9. memClassHandler$memClass(.)
        9. dat:::handleRows(., dispatcher(i))
       16. dat:::handleCols(...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 81 | SKIPPED: 1 | WARNINGS: 5 | FAILED: 4 ]
      1. Error: Basic syntax of a DataFrame (@test-DataFrame.R#41) 
      2. Error: split-apply-combine 
      3. Error: mutars by and sby (@test-mutar.R#21) 
      4. Error: S4 stuff (@test-mutar.R#60) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# datastepr

<details>

* Version: 0.0.2
* Source code: https://github.com/cran/datastepr
* URL: https://github.com/bramtayl/datastepr
* BugReports: https://github.com/bramtayl/datastepr/issues
* Date/Publication: 2016-08-20 10:31:35
* Number of recursive dependencies: 57

Run `cloud_details(, "datastepr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Attributes: < Component "class": Lengths (3, 1) differ (string compare on first 1) >
      Attributes: < Component "class": 1 string mismatch >
      
      ── 2. Failure: datastep (@test.R#19)  ──────────────────────────────────────────
      step$results not equal to data.frame(y = 2:11, x = 1:10).
      Attributes: < Component "class": Lengths (3, 1) differ (string compare on first 1) >
      Attributes: < Component "class": 1 string mismatch >
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 3 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 2 ]
      1. Failure: datastep (@test.R#2) 
      2. Failure: datastep (@test.R#19) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# ddpcr

<details>

* Version: 1.14
* Source code: https://github.com/cran/ddpcr
* URL: https://github.com/daattali/ddpcr
* BugReports: https://github.com/daattali/ddpcr/issues
* Date/Publication: 2020-03-23 18:00:06 UTC
* Number of recursive dependencies: 88

Run `cloud_details(, "ddpcr")` for more info

</details>

## Newly broken

*   checking whether package ‘ddpcr’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: `mutate_()` is deprecated as of dplyr 0.7.0.
      Warning: `select_()` is deprecated as of dplyr 0.7.0.
    See ‘/tmp/workdir/ddpcr/new/ddpcr.Rcheck/00install.out’ for details.
    ```

# dials

<details>

* Version: 0.0.6
* Source code: https://github.com/cran/dials
* URL: https://tidymodels.github.io/dials, https://github.com/tidymodels/dials
* BugReports: https://github.com/tidymodels/dials/issues
* Date/Publication: 2020-04-03 15:40:05 UTC
* Number of recursive dependencies: 64

Run `cloud_details(, "dials")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        1. testthat::expect_true(set_test(set_1 %>% arrange(id)))
       10. dplyr::arrange(., id)
       18. dplyr:::arrange_rows(.data, dots)
       19. base::tryCatch(...)
       20. base:::tryCatchList(expr, classes, parentenv, handlers)
       21. base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
       22. value[[3L]](cond)
       23. dplyr:::stop_arrange_transmute(cnd)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 323 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Error: dplyr ops (@test_dplyr_set_compat.R#58) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# disk.frame

<details>

* Version: 0.3.5
* Source code: https://github.com/cran/disk.frame
* URL: https://diskframe.com
* BugReports: https://github.com/xiaodaigh/disk.frame/issues
* Date/Publication: 2020-05-08 13:10:10 UTC
* Number of recursive dependencies: 105

Run `cloud_details(, "disk.frame")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘disk.frame-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: hard_arrange
    > ### Title: Perform a hard arrange
    > ### Aliases: hard_arrange hard_arrange.data.frame hard_arrange.disk.frame
    > 
    > ### ** Examples
    > 
    > iris.df = as.disk.frame(iris, nchunks = 2)
    > 
    > # arrange iris.df by specifies and ensure rows with the same specifies are in the same chunk
    > iris_hard.df = hard_arrange(iris.df, Species)
    Error in `[.data.table`(split_values, , name) : 
      j (the 2nd argument inside [...]) is a single symbol but column name 'name' is not found. Perhaps you intended DT[, ..name]. This difference to data.frame is deliberate and explained in FAQ 1.1.
    Calls: hard_arrange ... resolve.list -> signalConditionsASAP -> signalConditions
    Execution halted
    ```

*   checking S3 generic/method consistency ... WARNING
    ```
    pull:
      function(.data, var, name, ...)
    pull.disk.frame:
      function(.data, var)
    
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

# dm

<details>

* Version: 0.1.2
* Source code: https://github.com/cran/dm
* Date/Publication: 2020-05-04 11:20:02 UTC
* Number of recursive dependencies: 119

Run `cloud_details(, "dm")` for more info

</details>

## Newly broken

*   checking S3 generic/method consistency ... WARNING
    ```
    src_tbls:
      function(x, ...)
    src_tbls.dm:
      function(x)
    
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

# docxtools

<details>

* Version: 0.2.1
* Source code: https://github.com/cran/docxtools
* URL: https://graphdr.github.io/docxtools
* BugReports: https://github.com/graphdr/docxtools/issues
* Date/Publication: 2019-02-09 18:43:13 UTC
* Number of recursive dependencies: 75

Run `cloud_details(, "docxtools")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > data("CO2")
    > x <- head(CO2, n = 5L)
    > format_engr(x)
    Error: Problem with `mutate()` input `observ_index`.
    ✖ Input `observ_index` can't be recycled to size 0.
    ℹ Input `observ_index` is `1:dplyr::n()`.
    ℹ Input `observ_index` must be size 0 or 1, not 2.
    Backtrace:
         █
      1. └─docxtools::format_engr(x)
      2.   └─docxtools:::obs_add(numeric_as_is)
      3.     ├─dplyr::mutate(x, observ_index = 1:dplyr::n())
      4.     └─dplyr:::mutate.data.frame(x, observ_index = 1:dplyr::n())
      5.       └─dplyr:::mutate_cols(.data, ...)
      6.         └─base::tryCatch(...)
      7.           └─base:::tryCatchList(expr, classes, parentenv, handlers)
      8.             └─base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
      9.               └─value[[3L]](cond)
     10.                 └─dplyr:::stop_mutate_recycle_incompatible_size(e, index = i, dots = dots)
     11.                   └─dplyr:::stop_dplyr(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        6. dplyr:::mutate_cols(.data, ...)
        7. base::tryCatch(...)
        8. base:::tryCatchList(expr, classes, parentenv, handlers)
        9. base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
       10. value[[3L]](cond)
       11. dplyr:::stop_mutate_recycle_incompatible_size(e, index = i, dots = dots)
       12. dplyr:::stop_dplyr(...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 19 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 2 ]
      1. Error: Factors are returned unaffected (@test_format_engr.R#14) 
      2. Error: (unknown) (@test_format_engr.R#55) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# driftR

<details>

* Version: 1.1.0
* Source code: https://github.com/cran/driftR
* URL: https://github.com/shaughnessyar/driftR
* BugReports: https://github.com/shaughnessyar/driftR/issues
* Date/Publication: 2018-06-13 22:03:03 UTC
* Number of recursive dependencies: 69

Run `cloud_details(, "driftR")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Attributes: < Component "class": Lengths (3, 1) differ (string compare on first 1) >
      Attributes: < Component "class": 1 string mismatch >
      
      ── 2. Failure: importing the data (@test_read.R#40)  ───────────────────────────
      `sondeResult1` not equal to `sondeClean`.
      Attributes: < Component "class": Lengths (3, 1) differ (string compare on first 1) >
      Attributes: < Component "class": 1 string mismatch >
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 202 | SKIPPED: 0 | WARNINGS: 15 | FAILED: 2 ]
      1. Failure: importing the data (@test_import.R#29) 
      2. Failure: importing the data (@test_read.R#40) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# DSSAT

<details>

* Version: 0.0.1
* Source code: https://github.com/cran/DSSAT
* BugReports: https://github.com/palderman/DSSAT/issues
* Date/Publication: 2020-03-19 12:40:08 UTC
* Number of recursive dependencies: 42

Run `cloud_details(, "DSSAT")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    + "   210   -99 0.259 0.361 0.457 0.020   -99  1.40  0.01  50.0  45.0   0.0 0.000   6.5   -99")
    > 
    > read_soil_profile(sample_sol)
    Error: Problem with `summarise()` input `n`.
    ✖ could not find function "n"
    ℹ Input `n` is `n()`.
    ℹ The error occured in group 1: PEDON = "IB00000001", SOURCE = "IBSNAT", TEXTURE = "SIC", DEPTH = 210, DESCRIPTION = "DEFAULT - DEEP SILTY CLAY", SITE = "Generic", COUNTRY = "Generic", LAT = NA, LONG = NA, SCS FAMILY = "Generic", SCOM = NA, SALB = 0.11, SLU1 = 6, SLDR = 0.3, SLRO = 85, SLNF = 1, SLPF = 1, SMHB = "IB001", SMPX = "IB001", SMKE = "IB001".
    Backtrace:
         █
      1. └─DSSAT::read_soil_profile(sample_sol)
      2.   └─`%>%`(...)
      3.     ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      4.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5.       └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      6.         └─DSSAT:::`_fseq`(`_lhs`)
      7.           └─magrittr::freduce(value, `_function_list`)
      8.             └─function_list[[i]](value)
      9.               └─DSSAT:::collapse_rows(.)
     10.                 └─`%>%`(...)
     11.      
    Execution halted
    ```

# easyr

<details>

* Version: 0.3-1
* Source code: https://github.com/cran/easyr
* URL: https://github.com/oliver-wyman-actuarial/easyr
* BugReports: https://github.com/oliver-wyman-actuarial/easyr/issues
* Date/Publication: 2020-03-20 18:10:05 UTC
* Number of recursive dependencies: 92

Run `cloud_details(, "easyr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ** installing vignettes
      ** testing if installed package can be loaded from temporary location
      ** testing if installed package can be loaded from final location
      ** testing if installed package keeps a record of temporary installation path
      * DONE (doParallel)
      
      The downloaded source packages are in
      	'/tmp/RtmpTD6BtP/downloaded_packages'
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 286 | SKIPPED: 1 | WARNINGS: 0 | FAILED: 2 ]
      1. Failure: works as expected (@test_bindf-joinf.R#57) 
      2. Failure: works as expected (@test_bindf-joinf.R#58) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# eda4treeR

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/eda4treeR
* URL: https://github.com/MYaseen208/eda4treeR
* Date/Publication: 2018-02-04 19:06:12 UTC
* Number of recursive dependencies: 107

Run `cloud_details(, "eda4treeR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    +  dplyr::summarize(Mean=mean(Mean))
    Error: Corrupt `grouped_df` using old (< 0.8.0) format.
    ℹ Strip off old grouping with `ungroup()`.
    Backtrace:
         █
      1. └─DataExam3.1.1 %>% dplyr::group_by(SeedLot) %>% dplyr::summarize(Mean = mean(Mean))
      2.   ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      3.   └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      4.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5.       └─`_fseq`(`_lhs`)
      6.         └─magrittr::freduce(value, `_function_list`)
      7.           └─function_list[[i]](value)
      8.             ├─dplyr::group_by(., SeedLot)
      9.             └─dplyr:::group_by.data.frame(., SeedLot)
     10.               └─dplyr::group_by_prepare(.data, ..., .add = .add)
     11.                 ├─generics::setdiff(group_names, tbl_vars(out))
     12.                 ├─generics:::setdiff.default(group_names, tbl_vars(out))
     13.                 │ └─base::setdiff(x, y, ...)
     14.                 │   └─base::as.vector(y)
     1
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dae’ ‘dplyr’
      All declared Imports should be used.
    ```

# egor

<details>

* Version: 0.20.03
* Source code: https://github.com/cran/egor
* URL: https://github.com/tilltnet/egor, https://tilltnet.github.io/egor/
* BugReports: https://github.com/tilltnet/egor/issues
* Date/Publication: 2020-03-03 00:20:02 UTC
* Number of recursive dependencies: 73

Run `cloud_details(, "egor")` for more info

</details>

## Newly broken

*   checking S3 generic/method consistency ... WARNING
    ```
    group_modify:
      function(.data, .f, ..., .keep)
    group_modify.egor:
      function(.tbl, .f, ..., keep)
    
    pull:
      function(.data, var, name, ...)
    pull.egor:
      function(.data, var)
    
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

# embed

<details>

* Version: 0.0.6
* Source code: https://github.com/cran/embed
* URL: https://tidymodels.github.io/embed, https://github.com/tidymodels/embed
* BugReports: https://github.com/tidymodels/embed/issues
* Date/Publication: 2020-03-17 16:10:02 UTC
* Number of recursive dependencies: 148

Run `cloud_details(, "embed")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Component 1: target is numeric, current is factor
      Component 2: Mean relative difference: 0.836517
      Component 3: Mean relative difference: 0.4840334
      Component 4: 'current' is not a factor
      Component 5: Attributes: < Component "levels": Lengths (2, 5) differ (string compare on first 2) >
      Component 5: Attributes: < Component "levels": 2 string mismatches >
      Component 5: 'is.NA' value mismatch: 1 in current 0 in target
      ...
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 154 | SKIPPED: 10 | WARNINGS: 0 | FAILED: 1 ]
      1. Failure: step_woe (@test_woe.R#142) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# episheet

<details>

* Version: 0.4.0
* Source code: https://github.com/cran/episheet
* URL: https://github.com/epijim/episheet
* BugReports: https://github.com/epijim/episheet/issues
* Date/Publication: 2019-01-23 20:30:03 UTC
* Number of recursive dependencies: 65

Run `cloud_details(, "episheet")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > risk(data = dat, exposure = exposure_var, outcome = outcome_var)
    Error: Problem with `summarise()` input `n`.
    ✖ could not find function "n"
    ℹ Input `n` is `n()`.
    Backtrace:
         █
      1. └─episheet::risk(data = dat, exposure = exposure_var, outcome = outcome_var)
      2.   └─`%>%`(...)
      3.     ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      4.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5.       └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      6.         └─episheet:::`_fseq`(`_lhs`)
      7.           └─magrittr::freduce(value, `_function_list`)
      8.             └─function_list[[i]](value)
      9.               ├─dplyr::summarise(., n = n())
     10.               └─dplyr:::summarise.data.frame(., n = n())
     11.                 └─dplyr:::summarise_cols(.data, ...)
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
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 6 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 9 ]
      1. Error: Test risk_ratio returns exected estimate (@test-risk.R#16) 
      2. Error: Test risk_ratio returns exected lci (@test-risk.R#21) 
      3. Error: Test risk_ratio returns exected uci (@test-risk.R#26) 
      4. Error: Test risk_diff returns exected estimate (@test-risk.R#31) 
      5. Error: Test risk_diff returns exected lci (@test-risk.R#36) 
      6. Error: Test risk_diff returns exected uci (@test-risk.R#41) 
      7. Error: rrmh returns expected value (@test-stratified_risk.R#6) 
      8. Error: lci return expected value (@test-stratified_risk.R#13) 
      9. Error: uci return expected value (@test-stratified_risk.R#28) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# expstudies

<details>

* Version: 0.0.5
* Source code: https://github.com/cran/expstudies
* Date/Publication: 2019-06-14 11:20:03 UTC
* Number of recursive dependencies: 53

Run `cloud_details(, "expstudies")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 9 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 17 ]
      1. Failure: Correct handling of end dates prior to start dates (@test-exposure_functions.R#20) 
      2. Failure: Policy year exposure calculation works (@test-exposure_functions.R#30) 
      3. Failure: Policy month exposure calculation works (@test-exposure_functions.R#33) 
      4. Failure: Policy year with calendar year exposure calculation works, mid and start (@test-exposure_functions.R#36) 
      5. Failure: Policy year with calendar year exposure calculation works, mid and start (@test-exposure_functions.R#37) 
      6. Failure: Policy year with calendar month exposure calculation works, mid and start (@test-exposure_functions.R#40) 
      7. Failure: Policy year with calendar month exposure calculation works, mid and start (@test-exposure_functions.R#41) 
      8. Failure: Policy month with calendar year exposure calculation works, mid and start (@test-exposure_functions.R#44) 
      9. Failure: Policy month with calendar year exposure calculation works, mid and start (@test-exposure_functions.R#45) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

# fable

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/fable
* URL: https://fable.tidyverts.org, https://github.com/tidyverts/fable
* BugReports: https://github.com/tidyverts/fable/issues
* Date/Publication: 2020-04-22 13:12:08 UTC
* Number of recursive dependencies: 99

Run `cloud_details(, "fable")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    ✖ `vars` must be a character vector.
    ℹ Input `cmp` is `map(.fit, components)`.
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
      9.             ├─generics::components(.)
     10.             └─fabletools:::components.mdl_df(.)
     11.               ├─dplyr::transmute(...)
     12.               └─dplyr:::transmute.data.frame(...)
     13.                 ├─dplyr::mutate(.data, ..., .keep = "none")
     14.                 └─dplyr:::mutate.data.frame(.data, ..., .keep = "none")
     15.                   └─dplyr:::mutate_cols(.data, ...)
    <parent: err
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Backtrace:
        1. testthat::expect_warning(...)
        2. tsibbledata::vic_elec
        2. tsibble::index_by(., date = as.Date(Time))
        2. dplyr::summarise(., demand = mean(Demand))
       10. fabletools::model(., SNAIVE(demand ~ lag("year")))
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 90 | SKIPPED: 1 | WARNINGS: 0 | FAILED: 3 ]
      1. Error: Manual ETS selection (@test-ets.R#44) 
      2. Error: Automatic NNETAR selection (@test-nnetar.R#13) 
      3. Error: SNAIVE (@test-rw.R#105) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# fabletools

<details>

* Version: 0.1.3
* Source code: https://github.com/cran/fabletools
* URL: http://fabletools.tidyverts.org/, https://github.com/tidyverts/fabletools
* BugReports: https://github.com/tidyverts/fabletools/issues
* Date/Publication: 2020-03-24 07:10:02 UTC
* Number of recursive dependencies: 92

Run `cloud_details(, "fabletools")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    2 observations are missing between 2010 Q3 and 2010 Q4
    Error: `vars` must be a character vector.
    Backtrace:
         █
      1. └─fc %>% accuracy(aus_production)
      2.   ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      3.   └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      4.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5.       └─`_fseq`(`_lhs`)
      6.         └─magrittr::freduce(value, `_function_list`)
      7.           ├─base::withVisible(function_list[[k]](value))
      8.           └─function_list[[k]](value)
      9.             ├─fabletools::accuracy(., aus_production)
     10.             └─fabletools:::accuracy.fbl_ts(., aus_production)
     11.               ├─dplyr::transmute(object, .fc = !!resp, .dist = !!dist, !!!syms(by))
     12.               └─tsibble:::transmute.tbl_ts(...)
     13.                 ├─dplyr::mutate(.data, !!!lst_quos)
     14.                 ├─fabletools:::mutate.fbl_ts(.data, !!!lst_quos)
     15.                 │ └─fabletools::as_fable(...)
     16.                 
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       21. tsibble:::retain_tsibble(mut_data, key(.data), index(.data))
       22. tsibble:::duplicated_key_index(data, key, index)
       23. dplyr::grouped_df(as_tibble(data), key)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 264 | SKIPPED: 1 | WARNINGS: 0 | FAILED: 6 ]
      1. Error: Out-of-sample accuracy (@test-accuracy.R#52) 
      2. Error: fable dplyr verbs (@test-fable.R#32) 
      3. Failure: features() (@test-features.R#23) 
      4. Error: generate (@test-generate.R#6) 
      5. Error: generate seed setting (@test-generate.R#31) 
      6. Error: reconciliation (@test-reconciliation.R#4) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# finalfit

<details>

* Version: 1.0.1
* Source code: https://github.com/cran/finalfit
* URL: https://github.com/ewenharrison/finalfit
* BugReports: https://github.com/ewenharrison/finalfit/issues
* Date/Publication: 2020-04-21 11:50:02 UTC
* Number of recursive dependencies: 134

Run `cloud_details(, "finalfit")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
      mort_5yr mort_5yr.num   n
    1    Alive            1 511
    2     Died            2 404
    3     <NA>           NA  14
    
    $counts[[19]]
      sex.factor2 age.factor2   n
    1           M   <60 years 204
    2           M   60+ years 241
    3           F   <60 years 210
    4           F   60+ years 274
    
    
    > 
    > # Select a tibble and expand
    > out$counts[[9]] %>%
    +   print(n = Inf)
    Error in print.default(m, ..., quote = quote, right = right, max = max) : 
      invalid 'na.print' specification
    Calls: %>% ... print -> print.data.frame -> print -> print.default
    Execution halted
    ```

# fxtract

<details>

* Version: 0.9.2
* Source code: https://github.com/cran/fxtract
* URL: https://github.com/QuayAu/fxtract
* BugReports: https://github.com/QuayAu/fxtract/issues
* Date/Publication: 2019-07-03 15:50:06 UTC
* Number of recursive dependencies: 72

Run `cloud_details(, "fxtract")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > xtractor$ids
    [1] "setosa"     "versicolor" "virginica" 
    > fun = function(data) {
    +   c(mean_sepal_length = mean(data$Sepal.Length))
    + }
    > xtractor$add_feature(fun)
    > xtractor$features
    [1] "fun"
    > xtractor$calc_features()
    Error: Can't combine `..1$Species` <logical> and `..3$Species` <character>.
    Backtrace:
        █
     1. ├─xtractor$calc_features()
     2. ├─(function () ...
     3. │ └─dplyr::bind_rows(done_df, error_df, not_done_df)
     4. │   └─vctrs::vec_rbind(!!!dots, .names_to = .id)
     5. └─vctrs::vec_default_ptype2(...)
     6.   └─vctrs::stop_incompatible_type(...)
     7.     └─vctrs:::stop_incompatible(...)
     8.       └─vctrs:::stop_vctrs(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 71 | SKIPPED: 0 | WARNINGS: 2 | FAILED: 12 ]
      1. Error: preprocess_data (@test_xtractor.R#103) 
      2. Error: remove_data (@test_xtractor.R#144) 
      3. Error: add_feature 
      4. Error: calculate features (@test_xtractor.R#268) 
      5. Error: error handling (@test_xtractor.R#321) 
      6. Error: wrong function returns (@test_xtractor.R#350) 
      7. Error: right function returns (@test_xtractor.R#385) 
      8. Error: add new dataset after features were already calculated (@test_xtractor.R#421) 
      9. Error: test retry failed features (@test_xtractor.R#450) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

# gaiah

<details>

* Version: 0.0.2
* Source code: https://github.com/cran/gaiah
* Date/Publication: 2017-03-02 18:54:59
* Number of recursive dependencies: 72

Run `cloud_details(, "gaiah")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    ✖ could not find function "n"
    ℹ Input `cnt` is `n()`.
    ℹ The error occured in group 1: Location = "100 Mile House".
    Backtrace:
         █
      1. └─gaiah::group_birds_by_location(...)
      2.   └─`%>%`(...)
      3.     ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      4.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5.       └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      6.         └─gaiah:::`_fseq`(`_lhs`)
      7.           └─magrittr::freduce(value, `_function_list`)
      8.             ├─base::withVisible(function_list[[k]](value))
      9.             └─function_list[[k]](value)
     10.               ├─dplyr::summarise_(...)
     11.               └─dplyr:::summarise_.tbl_df(...)
     12.                 ├─dplyr::summarise(.data, !!!dots)
     13.                 └─dplyr:::summarise.grouped_df(.data, !!!dots)
     14.                   └─dplyr:::summarise_cols(.data, ...)
    <parent: error/simple
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘ggplot2’ ‘maptools’ ‘rgeos’ ‘stringr’ ‘tidyr’
      All declared Imports should be used.
    ```

# getTBinR

<details>

* Version: 0.7.0
* Source code: https://github.com/cran/getTBinR
* URL: https://www.samabbott.co.uk/getTBinR, https://github.com/seabbs/getTBinR
* BugReports: https://github.com/seabbs/getTBinR/issues
* Date/Publication: 2019-09-03 13:50:06 UTC
* Number of recursive dependencies: 148

Run `cloud_details(, "getTBinR")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Attributes: < Names: 2 string mismatches >
      Attributes: < Length mismatch: comparison on first 2 components >
      Attributes: < Component 1: Modes: character, externalptr >
      Attributes: < Component 1: Lengths: 3, 1 >
      Attributes: < Component 1: target is character, current is externalptr >
      Attributes: < Component 2: Modes: numeric, character >
      Attributes: < Component 2: Lengths: 1, 3 >
      Attributes: < Component 2: target is numeric, current is character >
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 95 | SKIPPED: 50 | WARNINGS: 2 | FAILED: 1 ]
      1. Failure: Variable search for a known variable returns expected results (@test-search_data_dict.R#28) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# ggedit

<details>

* Version: 0.3.0
* Source code: https://github.com/cran/ggedit
* URL: https://github.com/metrumresearchgroup/ggedit
* BugReports: https://github.com/metrumresearchgroup/ggedit/issues
* Date/Publication: 2018-07-03 21:50:02 UTC
* Number of recursive dependencies: 79

Run `cloud_details(, "ggedit")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Error: Problem with `filter()` input `..1`.
    ✖ object 'VAR' not found
    ℹ Input `..1` is `!is.null(VAR)`.
    Backtrace:
         █
      1. └─ggedit::layersList(p)
      2.   ├─ggedit:::rmNullObs(lapply(obj, layersListFull))
      3.   └─base::lapply(obj, layersListFull)
      4.     └─ggedit:::FUN(X[[i]], ...)
      5.       └─ggedit:::fetch_aes_ggplotBuild(obj, geom_list = geom_list(obj))
      6.         └─ggedit:::class_layer(p)
      7.           └─base::lapply(...)
      8.             └─ggedit:::FUN(X[[i]], ...)
      9.               └─`%>%`(...)
     10.                 ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
     11.                 └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
     12.                   └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
     13.                     └─ggedit:::`_fseq`(`_lhs`)
     14.                       └─magrittr::freduce(value, `_function_list`)
     15.                         ├─base::withVisible(function_list[[k]](value))
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘magrittr’
      All declared Imports should be used.
    ```

# gratia

<details>

* Version: 0.3.1
* Source code: https://github.com/cran/gratia
* URL: https://gavinsimpson.github.io/gratia
* BugReports: https://github.com/gavinsimpson/gratia/issues
* Date/Publication: 2020-03-29 18:30:05 UTC
* Number of recursive dependencies: 111

Run `cloud_details(, "gratia")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘test-all.R’
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      Backtrace:
        1. testthat::expect_silent(d <- derivatives(m))
       14. vctrs::stop_incompatible_size(...)
       15. vctrs:::stop_incompatible(...)
       16. vctrs:::stop_vctrs(...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 697 | SKIPPED: 74 | WARNINGS: 0 | FAILED: 4 ]
      1. Error: derivatives() returns derivatives for all smooths in a factor by GAM (@test-derivatives.R#183) 
      2. Error: derivatives() returns derivatives for all smooths in a factor by GAM (@test-derivatives.R#225) 
      3. Error: derivatives() works for factor by smooths issue 47 (@test-derivatives.R#339) 
      4. Error: derivatives() works for fs smooths issue 57 (@test-derivatives.R#389) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# groupedstats

<details>

* Version: 1.0.0
* Source code: https://github.com/cran/groupedstats
* URL: https://indrajeetpatil.github.io/groupedstats/, https://github.com/IndrajeetPatil/groupedstats/
* BugReports: https://github.com/IndrajeetPatil/groupedstats/issues/
* Date/Publication: 2020-05-05 16:20:03 UTC
* Number of recursive dependencies: 119

Run `cloud_details(, "groupedstats")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      ── 5. Failure: grouped_proptest works - with NAs (@test-grouped_proptest.R#111) 
      df2$statistic not equal to c(...).
      names for target but not for current
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 30 | SKIPPED: 4 | WARNINGS: 0 | FAILED: 5 ]
      1. Failure: grouped_proptest works - without NAs (@test-grouped_proptest.R#37) 
      2. Failure: grouped_proptest works - without NAs (@test-grouped_proptest.R#49) 
      3. Failure: grouped_proptest works - without NAs (@test-grouped_proptest.R#52) 
      4. Failure: grouped_proptest works - with NAs (@test-grouped_proptest.R#87) 
      5. Failure: grouped_proptest works - with NAs (@test-grouped_proptest.R#111) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# gt

<details>

* Version: 0.2.0.5
* Source code: https://github.com/cran/gt
* URL: https://github.com/rstudio/gt
* BugReports: https://github.com/rstudio/gt/issues
* Date/Publication: 2020-03-31 10:10:02 UTC
* Number of recursive dependencies: 101

Run `cloud_details(, "gt")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    +   )
    Error: Problem with `summarise()` input `pizzas_sold`.
    ✖ could not find function "n"
    ℹ Input `pizzas_sold` is `n()`.
    ℹ The error occured in group 1: month = 1.
    Backtrace:
         █
      1. └─`%>%`(...)
      2.   ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      3.   └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      4.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5.       └─`_fseq`(`_lhs`)
      6.         └─magrittr::freduce(value, `_function_list`)
      7.           └─function_list[[i]](value)
      8.             ├─dplyr::summarize(., pizzas_sold = n())
      9.             └─dplyr:::summarise.grouped_df(., pizzas_sold = n())
     10.               └─dplyr:::summarise_cols(.data, ...)
    <parent: error/simpleError>
    Backtrace:
    █
    Execution halted
    ```

## In both

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      The following object is masked from 'package:testthat':
      
          matches
      
      > 
      > test_check("gt")
      ── 1. Failure: the correct color values are obtained when defining a palette (@t
      ``%>%`(...)` did not throw an error.
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 2063 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Failure: the correct color values are obtained when defining a palette (@test-data_color.R#630) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.5Mb
      sub-directories of 1Mb or more:
        help   5.1Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 116 marked UTF-8 strings
    ```

# HaDeX

<details>

* Version: 1.1
* Source code: https://github.com/cran/HaDeX
* Date/Publication: 2020-02-06 13:50:02 UTC
* Number of recursive dependencies: 127

Run `cloud_details(, "HaDeX")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    The error most likely occurred in:
    
    > ### Name: quality_control
    > ### Title: Experiment quality control
    > ### Aliases: quality_control
    > 
    > ### ** Examples
    > 
    > # load example data
    > dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
    > 
    > # calculate mean uncertainty 
    > (result <- quality_control(dat = dat,
    +                            state_first = "CD160",
    +                            state_second = "CD160_HVEM", 
    +                            chosen_time = 1, 
    +                            in_time = 0.001))    
    Error in `[.data.table`(dat, "Exposure") : 
      When i is a data.table (or character vector), the columns to join by must be specified using 'on=' argument (see ?data.table), by keying x (i.e. sorted, and, marked as sorted, see ?setkey), or by sharing column names between x and i (i.e., a natural join). Keyed joins might have further speed benefits on very large data due to x being sorted in RAM.
    Calls: quality_control -> unique -> [ -> [.data.table
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      ── 2. Error: size is right  ────────────────────────────────────────────────────
      When i is a data.table (or character vector), the columns to join by must be specified using 'on=' argument (see ?data.table), by keying x (i.e. sorted, and, marked as sorted, see ?setkey), or by sharing column names between x and i (i.e., a natural join). Keyed joins might have further speed benefits on very large data due to x being sorted in RAM.
      Backtrace:
       1. testthat::expect_equal(...)
       6. HaDeX::quality_control(...)
       9. data.table:::`[.data.table`(dat, "Exposure")
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 16 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 2 ]
      1. Error: class is right 
      2. Error: size is right 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘DT’ ‘gsubfn’ ‘stringr’
      All declared Imports should be used.
    ```

# healthcareai

<details>

* Version: 2.4.0
* Source code: https://github.com/cran/healthcareai
* URL: http://docs.healthcare.ai
* BugReports: https://github.com/HealthCatalyst/healthcareai-r/issues
* Date/Publication: 2020-02-28 18:00:05 UTC
* Number of recursive dependencies: 119

Run `cloud_details(, "healthcareai")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Error: Problem with `filter()` input `..1`.
    ✖ object 'patient' not found
    ℹ Input `..1` is `n_distinct(patient) >= min_obs`.
    ℹ The error occured in group 1: drug = "Dexamethasone".
    Backtrace:
         █
      1. └─healthcareai::get_best_levels(...)
      2.   └─`%>%`(...)
      3.     ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      4.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5.       └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      6.         └─healthcareai:::`_fseq`(`_lhs`)
      7.           └─magrittr::freduce(value, `_function_list`)
      8.             └─function_list[[i]](value)
      9.               ├─dplyr::filter(., n_distinct(!!id) >= min_obs)
     10.               └─dplyr:::filter.data.frame(., n_distinct(!!id) >= min_obs)
     11.                 └─dplyr:::filter_rows(.data, ...)
     12.                   └─base::tryCatch(...)
     13.                     └─base:::tryCatchList(expr, classes, parentenv, handlers)
     14.            
    Execution halted
    ```

# holodeck

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/holodeck
* URL: https://github.com/Aariq/holodeck
* BugReports: https://github.com/Aariq/holodeck/issues
* Date/Publication: 2019-04-16 12:12:40 UTC
* Number of recursive dependencies: 108

Run `cloud_details(, "holodeck")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Attributes: < Component "class": 3 string mismatches >
      Attributes: < Component 2: Modes: list, numeric >
      Attributes: < Component 2: names for target but not for current >
      Attributes: < Component 2: Attributes: < Modes: list, NULL > >
      Attributes: < Component 2: Attributes: < names for target but not for current > >
      Attributes: < Component 2: Attributes: < current is not list-like > >
      ...
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 19 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 2 ]
      1. Failure: group_by() doesn't screw up sim_covar (@test-sim_multvar.R#48) 
      2. Failure: sim_missing() works with grouped dataframes (@test-sim_multvar.R#53) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# HydeNet

<details>

* Version: 0.10.9
* Source code: https://github.com/cran/HydeNet
* URL: https://github.com/nutterb/HydeNet,
* BugReports: https://github.com/nutterb/HydeNet/issues
* Date/Publication: 2019-01-11 17:00:20 UTC
* Number of recursive dependencies: 91

Run `cloud_details(, "HydeNet")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      iter  40 value 14.021414
      iter  50 value 14.019824
      iter  60 value 14.019278
      iter  70 value 14.018601
      iter  80 value 14.018282
      iter  80 value 14.018282
      iter  90 value 14.017126
      final  value 14.015374 
      converged
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 68 | SKIPPED: 0 | WARNINGS: 5 | FAILED: 1 ]
      1. Error: HydePlotOptions (@test-plot.HydeNetwork.R#21) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# idmodelr

<details>

* Version: 0.3.1
* Source code: https://github.com/cran/idmodelr
* URL: http://www.samabbott.co.uk/idmodelr, https://github.com/seabbs/idmodelr
* BugReports: https://github.com/seabbs/idmodelr/issues
* Date/Publication: 2019-09-10 22:50:10 UTC
* Number of recursive dependencies: 126

Run `cloud_details(, "idmodelr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── 4. Failure: generate_parameter_permutations can use a single parameter sample
      `df_results` not equal to `df_check`.
      Names: 2 string mismatches
      Component 3: Mean absolute difference: 1
      Component 4: Mean relative difference: 1
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 48 | SKIPPED: 41 | WARNINGS: 0 | FAILED: 4 ]
      1. Failure: Holding out of time works as expected (@test-combine_to_age_model.R#25) 
      2. Failure: Specifying compartments, automatically specifies hold out variables (@test-combine_to_age_model.R#31) 
      3. Failure: Specifying hold out variables, automatically specifies compartments (@test-combine_to_age_model.R#37) 
      4. Failure: generate_parameter_permutations can use a single parameter sample (@test-generate_parameter_permutations.R#26) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# immunarch

<details>

* Version: 0.6.4
* Source code: https://github.com/cran/immunarch
* URL: https://immunarch.com/, https://github.com/immunomind/immunarch
* BugReports: https://github.com/immunomind/immunarch/issues
* Date/Publication: 2020-05-13 08:00:02 UTC
* Number of recursive dependencies: 166

Run `cloud_details(, "immunarch")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘immunarch-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: pubRep
    > ### Title: Create a repertoire of public clonotypes
    > ### Aliases: pubRep publicRepertoire
    > 
    > ### ** Examples
    > 
    > # Subset the data to make the example faster to run
    > immdata$data <- lapply(immdata$data, head, 2000)
    > pr <- pubRep(immdata$data, .verbose=FALSE)
    > vis(pr, "clonotypes", 1, 2)
    Warning: You are using a dplyr method on a raw data.table, which will call the
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.8Mb
      sub-directories of 1Mb or more:
        data   2.1Mb
        doc    4.2Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dbplyr’
      All declared Imports should be used.
    ```

# IncucyteDRC

<details>

* Version: 0.5.4
* Source code: https://github.com/cran/IncucyteDRC
* URL: https://github.com/chapmandu2/IncucyteDRC
* BugReports: https://github.com/chapmandu2/IncucyteDRC/issues
* Date/Publication: 2016-04-23 14:21:03
* Number of recursive dependencies: 119

Run `cloud_details(, "IncucyteDRC")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    ✖ could not find function "row_number"
    ℹ Input `idx` is `row_number()`.
    ℹ The error occured in group 1: sampleid = "PDD00017273", conc = 0.3703704.
    Backtrace:
         █
      1. └─IncucyteDRC::fitDoseResponseCurve(test_idrc_set)
      2.   └─IncucyteDRC::exportDRCDataToDataFrame(idrc_set, include_control)
      3.     └─`%>%`(...)
      4.       ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      5.       └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      6.         └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      7.           └─IncucyteDRC:::`_fseq`(`_lhs`)
      8.             └─magrittr::freduce(value, `_function_list`)
      9.               └─function_list[[i]](value)
     10.                 ├─dplyr::mutate(., idx = row_number())
     11.                 └─dplyr:::mutate.data.frame(., idx = row_number())
     12.                   └─dplyr:::mutate_cols(.data, ...)
    <parent: error/simpleError>
    Backtrace:
    █
    Execution halted
    ```

# infer

<details>

* Version: 0.5.1
* Source code: https://github.com/cran/infer
* URL: https://github.com/tidymodels/infer, https://infer.netlify.com/
* BugReports: https://github.com/tidymodels/infer/issues
* Date/Publication: 2019-11-19 10:30:02 UTC
* Number of recursive dependencies: 115

Run `cloud_details(, "infer")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      gen_iris12 %>% calculate(stat = "count") not equal to `%>%`(...).
      Attributes: < Names: 1 string mismatch >
      Attributes: < Length mismatch: comparison on first 2 components >
      Attributes: < Component 2: Modes: logical, numeric >
      Attributes: < Component 2: Lengths: 1, 10 >
      Attributes: < Component 2: target is logical, current is numeric >
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 339 | SKIPPED: 60 | WARNINGS: 21 | FAILED: 3 ]
      1. Failure: chi-square matches chisq.test value (@test-calculate.R#219) 
      2. Failure: chi-square matches chisq.test value (@test-calculate.R#236) 
      3. Failure: calc_impl.count works (@test-calculate.R#473) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘lifecycle’
      All declared Imports should be used.
    ```

# lpirfs

<details>

* Version: 0.1.7
* Source code: https://github.com/cran/lpirfs
* BugReports: https://github.com/adaemmerp/lpirfs/issues
* Date/Publication: 2019-11-25 09:20:06 UTC
* Number of recursive dependencies: 135

Run `cloud_details(, "lpirfs")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Backtrace:
        1. lpirfs::lp_nl_panel(...)
        6. plm:::model.frame.pdata.frame(...)
        8. Formula:::model.frame.Formula(...)
       10. stats::model.frame.default(...)
       11. [ base::eval(...) ] with 1 more call
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 168 | SKIPPED: 0 | WARNINGS: 1 | FAILED: 3 ]
      1. Failure: Test that data is correctly computed. (@test-lp_lin_panel.R#737) 
      2. Failure: Test that data is correctly computed. (@test-lp_lin_panel.R#783) 
      3. Error: Check output of switching variable I (@test-lp_nl_panel.R#948) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.4Mb
      sub-directories of 1Mb or more:
        libs   6.9Mb
    ```

# mason

<details>

* Version: 0.2.6
* Source code: https://github.com/cran/mason
* URL: https://github.com/lwjohnst86/mason
* BugReports: https://github.com/lwjohnst86/mason/issues
* Date/Publication: 2018-07-05 12:20:02 UTC
* Number of recursive dependencies: 67

Run `cloud_details(, "mason")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Component "estimate": Mean relative difference: 1.999997
      Component "std.error": Mean relative difference: 0.2627753
      Component "statistic": Mean relative difference: 1.789258
      Component "p.value": Mean relative difference: 1.333262
      Component "conf.low": Mean relative difference: 1.751052
      Component "conf.high": Mean relative difference: 1.316709
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 76 | SKIPPED: 1 | WARNINGS: 7 | FAILED: 3 ]
      1. Failure: (for glm bi) results are equal to real results (no covar) (@test-glm-binomial.R#59) 
      2. Failure: (for glm bi) results are equal to real results (with covar) (@test-glm-binomial.R#73) 
      3. Failure: (for glm) results are equal to real results (with covar + int) (@test-glm-binomial.R#88) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘ggplot2’, ‘pander’, ‘pixiedust’
    ```

# mcp

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/mcp
* URL: http://lindeloev.github.io/mcp/, https://github.com/lindeloev/mcp
* BugReports: https://github.com/lindeloev/mcp/issues
* Date/Publication: 2020-01-09 16:30:02 UTC
* Number of recursive dependencies: 94

Run `cloud_details(, "mcp")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      2. Failure: Good variance:
          y ~ 1 + sigma(1 + sin(x)) (@test-runs.R#277) 
      3. Failure: Good variance:
          y ~ 1, ~0 + sigma(rel(1)), ~x + sigma(x), ~0 + sigma(rel(x)) (@test-runs.R#277) 
      4. Failure: Good variance:
          y ~ 1, 1 + (1 | id) ~ rel(1) + I(x^2) + sigma(rel(1) + x) (@test-runs.R#277) 
      5. Failure: Good variance:
          y ~ 1, 1 + (1 | id) ~ rel(1) + I(x^2) + sigma(rel(1) + x) (@test-runs.R#277) 
      6. Failure: Good ARMA:
          y ~ ar(1) + sigma(1 + x), ~ar(2, 1 + I(x^2)) + sigma(1) (@test-runs.R#277) 
      7. Failure: Good Poisson:
          y ~ 1 + ar(1), ~1 + x + ar(2, 1 + x + I(x^3)) (@test-runs.R#277) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘bayesplot’ ‘methods’ ‘purrr’
      All declared Imports should be used.
    ```

# metamicrobiomeR

<details>

* Version: 1.1
* Source code: https://github.com/cran/metamicrobiomeR
* URL: https://github.com/nhanhocu/metamicrobiomeR
* BugReports: https://github.com/nhanhocu/metamicrobiomeR/issues
* Date/Publication: 2019-09-03 07:20:02 UTC
* Number of recursive dependencies: 131

Run `cloud_details(, "metamicrobiomeR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > tl<-colnames(taxtab6)[grep("k__bacteria.p__fusobacteria",colnames(taxtab6))]
    > taxacom.ex<-taxa.compare(taxtab=taxtab6[,c("personid","x.sampleid","bf","age.sample",tl)],
    + propmed.rel="gamlss",comvar="bf",adjustvar="age.sample",
    + longitudinal="yes",p.adjust.method="fdr")
    Error: Corrupt `grouped_df` using old (< 0.8.0) format.
    ℹ Strip off old grouping with `ungroup()`.
    Backtrace:
         █
      1. ├─metamicrobiomeR::taxa.compare(...)
      2. │ └─base::as.data.frame(taxtab)
      3. ├─taxtab6[, c("personid", "x.sampleid", "bf", "age.sample", tl)]
      4. └─dplyr:::`[.grouped_df`(...)
      5.   └─dplyr:::group_intersect(x, out)
      6.     ├─generics::intersect(group_vars(x), names(new))
      7.     ├─dplyr::group_vars(x)
      8.     └─dplyr:::group_vars.data.frame(x)
      9.       ├─generics::setdiff(names(group_data(x)), ".rows")
     10.       ├─dplyr::group_data(x)
     11.       └─dplyr:::group_data.grouped_df(x)
     12.         └─dplyr::validate_grouped_df(.data)
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘RCurl’ ‘foreign’ ‘gplots’ ‘httr’ ‘jsonlite’ ‘knitr’ ‘lmerTest’
      ‘magrittr’ ‘mgcv’ ‘repmis’ ‘reshape2’ ‘rmarkdown’
      All declared Imports should be used.
    ```

# mmetrics

<details>

* Version: 0.3.0
* Source code: https://github.com/cran/mmetrics
* URL: https://github.com/y-bar/mmetrics
* BugReports: https://github.com/y-bar/mmetrics/issues
* Date/Publication: 2019-07-26 08:50:02 UTC
* Number of recursive dependencies: 96

Run `cloud_details(, "mmetrics")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 12 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 9 ]
      1. Error: summarize one key (@test-mmetrics.R#16) 
      2. Error: summarize two keys (@test-mmetrics.R#21) 
      3. Error: summarize all (@test-mmetrics.R#26) 
      4. Error: mutate one key (@test-mmetrics.R#31) 
      5. Error: mutate two keys (@test-mmetrics.R#36) 
      6. Error: mutate all with (@test-mmetrics.R#41) 
      7. Failure: mutate with non summarize mode to evaluate ratio (@test-mmetrics.R#48) 
      8. Failure: not evaluatable metrics must be removed without error (@test-mmetrics.R#55) 
      9. Error: not evaluatable metrics must be removed without error (@test-mmetrics.R#63) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘magrittr’ ‘stringr’
      All declared Imports should be used.
    ```

# MPTmultiverse

<details>

* Version: 0.4-0
* Source code: https://github.com/cran/MPTmultiverse
* URL: https://github.com/mpt-network/MPTmultiverse
* BugReports: https://github.com/mpt-network/MPTmultiverse/issues
* Date/Publication: 2020-03-28 01:20:02 UTC
* Number of recursive dependencies: 87

Run `cloud_details(, "MPTmultiverse")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      [3] 0.912 - 0.701 ==  0.2109
      [4] 0.750 - 0.887 == -0.1370
      [5] 0.933 - 0.922 ==  0.0109
      [6] 0.903 - 0.972 == -0.0693
      [7] 0.701 - 0.912 == -0.2108
      [8] 0.887 - 0.750 ==  0.1375
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 38 | SKIPPED: 4 | WARNINGS: 0 | FAILED: 3 ]
      1. Failure: No-pooling approaches work (@test-mptinr.R#35) 
      2. Failure: No-pooling approaches work (@test-mptinr.R#68) 
      3. Failure: No-pooling approaches work (@test-mptinr.R#112) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# neuropsychology

<details>

* Version: 0.5.0
* Source code: https://github.com/cran/neuropsychology
* URL: https://github.com/neuropsychology/neuropsychology.R
* BugReports: https://github.com/neuropsychology/neuropsychology.R/issues
* Date/Publication: 2017-03-22 19:17:18 UTC
* Number of recursive dependencies: 153

Run `cloud_details(, "neuropsychology")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    
    > ### Name: describe
    > ### Title: Description of dataframes.
    > ### Aliases: describe
    > 
    > ### ** Examples
    > 
    > require(neuropsychology)
    > 
    > df <- personality
    > 
    > describe(df)
    Warning: `transmute_()` is deprecated as of dplyr 0.7.0.
    Please use `transmute()` instead.
    See vignette('programming') for more help
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_warnings()` to see where this warning was generated.
    Error in round(df[2:10], 2) : 
      non-numeric argument to mathematical function
    Calls: describe
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘htmlTable’ ‘lme4’ ‘stringi’
      All declared Imports should be used.
    ```

# omu

<details>

* Version: 1.0.2
* Source code: https://github.com/cran/omu
* URL: https://github.com/connor-reid-tiffany/Omu, https://www.kegg.jp/kegg/rest/keggapi.html
* BugReports: https://github.com/connor-reid-tiffany/Omu/issues
* Date/Publication: 2018-08-02 12:40:03 UTC
* Number of recursive dependencies: 82

Run `cloud_details(, "omu")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### Title: Gather metadata from KEGG for metabolites
    > ### Aliases: KEGG_gather KEGG_gather.cpd KEGG_gather.rxn KEGG_gather.KO
    > 
    > ### ** Examples
    > 
    > count_data <- assign_hierarchy(count_data = c57_nos2KO_mouse_countDF,
    + keep_unknowns = TRUE, identifier = "KEGG")
    Error: Input must be a vector, not a `data.frame/cpd` object.
    Backtrace:
         █
      1. ├─omu::assign_hierarchy(...)
      2. │ ├─dplyr::distinct(count_data, Metabolite, .keep_all = TRUE)
      3. │ └─dplyr:::distinct.data.frame(count_data, Metabolite, .keep_all = TRUE)
      4. │   ├─dplyr::dplyr_row_slice(out[prep$keep], loc)
      5. │   └─dplyr:::dplyr_row_slice.data.frame(out[prep$keep], loc)
      6. │     ├─dplyr::dplyr_reconstruct(vec_slice(data, i), data)
      7. │     ├─dplyr:::dplyr_reconstruct.data.frame(vec_slice(data, i), data)
      8. │     └─vctrs::vec_slice(data, i)
      9. └─vctrs:::stop_scalar_type(...)
     10.   └─vctrs:::stop_vctrs(msg, "vctrs_error_scalar_type", actual = x)
    Execution halted
    ```

# pammtools

<details>

* Version: 0.2.2
* Source code: https://github.com/cran/pammtools
* URL: https://github.com/adibender/pammtools
* BugReports: https://github.com/adibender/pammtools/issues
* Date/Publication: 2020-03-12 21:00:02 UTC
* Number of recursive dependencies: 101

Run `cloud_details(, "pammtools")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Error: `vars` must be a character vector.
    Backtrace:
         █
      1. └─tumor %>% group_by(sex) %>% make_newdata()
      2.   ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      3.   └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      4.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5.       └─`_fseq`(`_lhs`)
      6.         └─magrittr::freduce(value, `_function_list`)
      7.           ├─base::withVisible(function_list[[k]](value))
      8.           └─function_list[[k]](value)
      9.             ├─pammtools::make_newdata(.)
     10.             └─pammtools:::make_newdata.default(.)
     11.               ├─sample_info(rest) %>% ungroup()
     12.               │ └─base::eval(lhs, parent, parent)
     13.               │   └─base::eval(lhs, parent, parent)
     14.               ├─pammtools::sample_info(rest)
     15.               └─pammtools:::sample_info.data.frame(rest)
     16.                 ├─base::suppressMessages(...)
     17.                 
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 242 | SKIPPED: 0 | WARNINGS: 2 | FAILED: 13 ]
      1. Error: cumulative hazard functions work for PAM (@test-add-functions.R#101) 
      2. Failure: adding terms works for PAM (@test-add-functions.R#145) 
      3. Failure: adding terms works for PAM (@test-add-functions.R#150) 
      4. Failure: adding terms works for PAM (@test-add-functions.R#152) 
      5. Failure: adding terms works for PAM (@test-add-functions.R#155) 
      6. Failure: adding terms works for PAM (@test-add-functions.R#157) 
      7. Error: survival probabilities functions work for PAM (@test-add-functions.R#236) 
      8. Error: Cumulative effects are calculated correctly (@test-cumulative-effect.R#102) 
      9. Error: Sample info returned for data frame (@test-interval-functions.R#23) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

# panelr

<details>

* Version: 0.7.2
* Source code: https://github.com/cran/panelr
* URL: https://panelr.jacob-long.com
* BugReports: https://github.com/jacob-long/panelr
* Date/Publication: 2020-03-08 22:10:02 UTC
* Number of recursive dependencies: 168

Run `cloud_details(, "panelr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘panelr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: are_varying
    > ### Title: Check if variables are constant or variable over time.
    > ### Aliases: are_varying
    > 
    > ### ** Examples
    > 
    > 
    > wages <- panel_data(WageData, id = id, wave = t)
    Warning: The `add` argument of `group_by()` is deprecated as of dplyr 1.0.0.
    Please use the `.add` argument instead.
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_warnings()` to see where this warning was generated.
    > wages %>% are_varying(occ, ind, fem, blk)
    Error in if (get_wave(data) %in% dots) NULL else get_wave(data) : 
      argument is of length zero
    Calls: %>% ... freduce -> withVisible -> <Anonymous> -> are_varying
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 22 | SKIPPED: 0 | WARNINGS: 1 | FAILED: 17 ]
      1. Failure: dplyr functions return panel_data objects (@test-utils.R#29) 
      2. Error: widen_panel works (@test-utils.R#46) 
      3. Error: long_panel works (basic case) (@test-utils.R#72) 
      4. Error: long_panel works (unbalanced data) (@test-utils.R#96) 
      5. Error: long_panel works (unbalanced data, numeric waves not begin w/ 1) (@test-utils.R#120) 
      6. Error: long_panel works (character periods) (@test-utils.R#146) 
      7. Error: long_panel works (beginning label) (@test-utils.R#171) 
      8. Error: long_panel works (beginning label/character periods) (@test-utils.R#198) 
      9. Error: long_panel works (prefix and suffix/character periods) (@test-utils.R#225) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘AER’
    ```

# pccc

<details>

* Version: 1.0.3
* Source code: https://github.com/cran/pccc
* URL: https://github.com/CUD2V/pccc
* BugReports: https://github.com/CUD2V/pccc/issues
* Date/Publication: 2019-08-21 21:50:06 UTC
* Number of recursive dependencies: 60

Run `cloud_details(, "pccc")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       17. vctrs:::as.data.table(df_ptype2(x, y, ...))
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 110 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 8 ]
      1. Error: icd 10 data set with all parameters - result should be unchanged. (@test_ccc_icd10.R#36) 
      2. Error: Correct number of rows (1 per patient) returned? (@test_ccc_icd9.R#26) 
      3. Error: Correct number of columns (1 per category + Id column + summary column) returned? (@test_ccc_icd9.R#36) 
      4. Error: icd 9 data set with all parameters - result should be unchanged. (@test_ccc_icd9.R#48) 
      5. Error: icd 9 data set with ICD10 parameter (@test_ccc_icd9.R#61) 
      6. Error: icd 9 data set with missing dx parameter (@test_ccc_icd9.R#85) 
      7. Error: icd 9 data set with missing pc parameter (@test_ccc_icd9.R#97) 
      8. Error: icd 10 data set with ICD9 parameter (@test_ccc_icd9.R#110) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# photosynthesis

<details>

* Version: 1.0.0
* Source code: https://github.com/cran/photosynthesis
* Date/Publication: 2019-05-09 15:10:03 UTC
* Number of recursive dependencies: 73

Run `cloud_details(, "photosynthesis")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
                          A            g_mc25              g_sc                g_uc
    1 27.48581 [umol/m^2/s] 4 [umol/Pa/m^2/s] 4 [umol/Pa/m^2/s] 0.1 [umol/Pa/m^2/s]
      gamma_star25          J_max25       K_C25        K_O25  k_mc  k_sc  k_uc
    1   3.743 [Pa] 200 [umol/m^2/s] 27.238 [Pa] 16.582 [kPa] 1 [1] 1 [1] 1 [1]
      leafsize     phi_J          R_d25     T_leaf   theta_J         V_cmax25
    1  0.1 [m] 0.331 [1] 2 [umol/m^2/s] 298.15 [K] 0.825 [1] 150 [umol/m^2/s]
               V_tpu25 g_mc gamma_star J_max    K_C    K_O R_d V_cmax V_tpu   C_air
    1 200 [umol/m^2/s]    4      3.743   200 27.238 16.582   2    150   200 41 [Pa]
                   O              P              PPFD      RH    wind
    1 21.27565 [kPa] 101.3246 [kPa] 1500 [umol/m^2/s] 0.5 [1] 2 [m/s]
    > 
    > # Multiple parameter sets with 'photosynthesis'
    > 
    > leaf_par <- make_leafpar(
    +   replace = list(
    +     T_leaf = set_units(c(293.14, 298.15), "K")
    +     ), use_tealeaves = FALSE
    +   )
    > photosynthesis(leaf_par, enviro_par, bake_par, constants,
    +                use_tealeaves = FALSE)
    Solving for photosynthetic rate from 2 parameter sets ...New names:
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tidyr’
      All declared Imports should be used.
    ```

# pmdplyr

<details>

* Version: 0.3.1.1
* Source code: https://github.com/cran/pmdplyr
* URL: https://nickch-k.github.io/pmdplyr, https://github.com/NickCH-K/pmdplyr
* BugReports: https://github.com/NickCH-K/pmdplyr/issues
* Date/Publication: 2020-05-15 08:47:17 UTC
* Number of recursive dependencies: 106

Run `cloud_details(, "pmdplyr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Error: Problem with `mutate()` input `changed`.
    ✖ Input `changed` can't be recycled to size 8.
    ℹ Input `changed` is `<lgl>`.
    ℹ Input `changed` must be size 8 or 1, not 48445.
    ℹ The error occured in group 1: unitid = 100654.
    Backtrace:
         █
      1. └─pmdplyr::fixed_force(...)
      2.   └─.df %>% dplyr::mutate(`:=`(!!.flag, !!newflag))
      3.     ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      4.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5.       └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      6.         └─pmdplyr:::`_fseq`(`_lhs`)
      7.           └─magrittr::freduce(value, `_function_list`)
      8.             ├─base::withVisible(function_list[[k]](value))
      9.             └─function_list[[k]](value)
     10.               ├─dplyr::mutate(., `:=`(!!.flag, !!newflag))
     11.               └─dplyr:::mutate.data.frame(., `:=`(!!.flag, !!newflag))
     12.                 └─dplyr:::mutate_cols(.data, ...)
     13.                 
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ℹ The error occured in group 1: i = 1.
      Backtrace:
        1. pmdplyr::fixed_force(df, .var = x, .within = i, .flag = "changed")
       10. dplyr::mutate(., `:=`(!!.flag, !!newflag))
       15. dplyr:::mutate.data.frame(.data, ...)
       16. dplyr:::mutate_cols(.data, ...)
       17. base::tryCatch(...)
       18. base:::tryCatchList(expr, classes, parentenv, handlers)
       19. base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
       20. value[[3L]](cond)
       21. dplyr:::stop_mutate_recycle_incompatible_size(e, index = i, dots = dots)
       22. dplyr:::stop_dplyr(...)
      
      Error: C stack usage  7970468 is too close to the limit
      Execution halted
    ```

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in documentation object 'join.Rd':
      ‘[dplyr]{join.tbl_df}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

# ptstem

<details>

* Version: 0.0.7
* Source code: https://github.com/cran/ptstem
* URL: https://github.com/dfalbel/ptstem
* Date/Publication: 2020-05-12 23:40:03 UTC
* Number of recursive dependencies: 63

Run `cloud_details(, "ptstem")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      y[4]: "gostou"
      
      x[5]: "gostaram"
      y[5]: "gostou"
      
      x[6]: "gostaram"
      y[6]: "gostou"
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 17 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 2 ]
      1. Failure: Stemming Hunspell Works (@test-ptstem.R#15) 
      2. Failure: Stemming Hunspell Works (@test-ptstem.R#19) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        dict   5.1Mb
    ```

# PupillometryR

<details>

* Version: 0.0.1
* Source code: https://github.com/cran/PupillometryR
* BugReports: https://github.com/samhforbes/PupillometryR/issues
* Date/Publication: 2020-02-02 16:00:05 UTC
* Number of recursive dependencies: 64

Run `cloud_details(, "PupillometryR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Calculating mean pupil size in each timebin 
    
    Error: Column name `Trial` must not be duplicated.
    Backtrace:
         █
      1. └─PupillometryR::downsample_time_data(...)
      2.   └─`%>%`(...)
      3.     ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      4.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5.       └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      6.         └─PupillometryR:::`_fseq`(`_lhs`)
      7.           └─magrittr::freduce(value, `_function_list`)
      8.             └─function_list[[i]](value)
      9.               ├─dplyr::group_by(...)
     10.               └─dplyr:::group_by.data.frame(...)
     11.                 └─dplyr::grouped_df(groups$data, groups$group_names, .drop)
     12.                   └─dplyr:::compute_groups(data, vars, drop = drop)
     13.                     └─tibble::tibble(!!!old_keys, `:=`(".rows", old_rows))
     14.                       └─tibble:::tibble_quos(xs[!is_null], .rows, .name_repair)
     15.                         └─tibble:::set_repair
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘data.table’ ‘fractal’ ‘mgcv’
      All declared Imports should be used.
    ```

# purrrogress

<details>

* Version: 0.1.1
* Source code: https://github.com/cran/purrrogress
* URL: https://github.com/halpo/purrrogress
* BugReports: https://github.com/halpo/purrrogress/issues
* Date/Publication: 2019-07-22 21:10:08 UTC
* Number of recursive dependencies: 48

Run `cloud_details(, "purrrogress")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(purrrogress)
      > 
      > test_check("purrrogress")
      ── 1. Error: with_progress_group_map (@group_map.R#56)  ────────────────────────
      object '.tbl' not found
      Backtrace:
        1. dplyr::group_map(...)
       13. dplyr::count(.tbl)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 224 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Error: with_progress_group_map (@group_map.R#56) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘R6’ ‘hms’
      All declared Imports should be used.
    ```

# rabhit

<details>

* Version: 0.1.4
* Source code: https://github.com/cran/rabhit
* URL: https://yaarilab.bitbucket.io/RAbHIT/
* BugReports: https://bitbucket.org/yaarilab/haplotyper/issues
* Date/Publication: 2020-01-29 20:20:02 UTC
* Number of recursive dependencies: 109

Run `cloud_details(, "rabhit")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_warnings()` to see where this warning was generated.
    Using `n` as weighting variable
    Error: Column `n` is already present in output.
    ℹ Use `name = "new_name"` to pick a new name.
    Backtrace:
         █
      1. └─rabhit::deletionHeatmap(samplesHaplotype)
      2.   └─`%>%`(...)
      3.     ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      4.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5.       └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      6.         └─rabhit:::`_fseq`(`_lhs`)
      7.           └─magrittr::freduce(value, `_function_list`)
      8.             ├─base::withVisible(function_list[[k]](value))
      9.             └─function_list[[k]](value)
     10.               └─dplyr::count_(.)
     11.                 └─dplyr::count(x, !!!vars, wt = !!wt, sort = sort, .drop = .drop)
     12.                   └─dplyr::tally(out, wt = !!enquo(wt), sort = sort, name = name)
     13.                     └─dplyr:::check_name(x, name)
    Execution halted
    ```

# rainette

<details>

* Version: 0.1.1
* Source code: https://github.com/cran/rainette
* URL: https://juba.github.io/rainette/
* BugReports: https://github.com/juba/rainette/issues
* Date/Publication: 2020-05-09 12:00:03 UTC
* Number of recursive dependencies: 155

Run `cloud_details(, "rainette")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        4. rainette:::filter_crosstab(...)
        3. dplyr::filter(., chi2 > min_chi2, n_both > min_members)
        3. dplyr::select(., g1, g2, level1, level2, n_both, chi2)
       10. dplyr::mutate(...)
       14. dplyr:::mutate_cols(.data, ...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 106 | SKIPPED: 1 | WARNINGS: 0 | FAILED: 2 ]
      1. Error: rainette2_complete_groups (@test_cutree.R#28) 
      2. Error: (unknown) (@test_rainette2.R#12) 
      
      Error: testthat unit tests failed
      In addition: Warning message:
      In ns_env(x) : closing unused connection 4 (doesnt/exist.txtt)
      Execution halted
    ```

# ratPASTA

<details>

* Version: 0.1.0
* Source code: https://github.com/cran/ratPASTA
* URL: https://github.com/ikodvanj/ratPASTA
* BugReports: https://github.com/ikodvanj/ratPASTA/issues
* Date/Publication: 2020-04-28 11:40:02 UTC
* Number of recursive dependencies: 118

Run `cloud_details(, "ratPASTA")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Attributes: < Length mismatch: comparison on first 2 components >
      Attributes: < Component 2: Lengths: 1708, 33000 >
      Attributes: < Component 2: names for target but not for current >
      Attributes: < Component 2: Attributes: < Modes: list, NULL > >
      Attributes: < Component 2: Attributes: < Lengths: 1, 0 > >
      Attributes: < Component 2: Attributes: < names for target but not for current > >
      Attributes: < Component 2: Attributes: < current is not list-like > >
      Attributes: < Component 2: target is omit, current is numeric >
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 22 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Failure: Testing automated data loading (@test-loadstartledata.R#7) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘hms’
      All declared Imports should be used.
    ```

# RCMIP5

<details>

* Version: 1.2.0
* Source code: https://github.com/cran/RCMIP5
* Date/Publication: 2016-07-30 18:53:27
* Number of recursive dependencies: 61

Run `cloud_details(, "RCMIP5")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Component 4: target is logical, current is numeric
      Component 5: Modes: logical, numeric
      Component 5: target is logical, current is numeric
      data.frame
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 799 | SKIPPED: 29 | WARNINGS: 1 | FAILED: 5 ]
      1. Failure: monthly data (@test_chainedOperations.R#42) 
      2. Failure: monthly data (@test_chainedOperations.R#63) 
      3. Failure: annual data (@test_chainedOperations.R#97) 
      4. Failure: four-D data (@test_chainedOperations.R#132) 
      5. Failure: four-D data (@test_chainedOperations.R#159) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# rcv

<details>

* Version: 0.2.1
* Source code: https://github.com/cran/rcv
* URL: https://github.com/ds-elections/rcv
* BugReports: https://github.com/ds-elections/rcv/issues
* Date/Publication: 2017-08-11 08:11:33 UTC
* Number of recursive dependencies: 46

Run `cloud_details(, "rcv")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Error: Problem with `summarise()` input `total`.
    ✖ could not find function "n"
    ℹ Input `total` is `n()`.
    ℹ The error occured in group 1: candidate = "BEN MATRANGA".
    Backtrace:
         █
      1. └─rcv::rcv_tally(image = sf_bos_clean, rcvcontest = "Board of Supervisors, District 7")
      2.   └─`%>%`(...)
      3.     ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      4.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5.       └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      6.         └─rcv:::`_fseq`(`_lhs`)
      7.           └─magrittr::freduce(value, `_function_list`)
      8.             └─function_list[[i]](value)
      9.               ├─dplyr::summarise(., total = n())
     10.               └─dplyr:::summarise.grouped_df(., total = n())
     11.                 └─dplyr:::summarise_cols(.data, ...)
    <parent: error/simpleError>
    Backtrace:
    █
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 6543 marked UTF-8 strings
    ```

# RNeXML

<details>

* Version: 2.4.4
* Source code: https://github.com/cran/RNeXML
* URL: https://docs.ropensci.org/RNeXML, https://github.com/ropensci/RNeXML
* BugReports: https://github.com/ropensci/RNeXML/issues
* Date/Publication: 2020-05-10 07:20:06 UTC
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
      ── 1. Failure: coalesce_() works correctly (@test_01_utils.R#129)  ─────────────
      `dplyr::coalesce(dta$col1, dta$col3, last)` did not throw an error.
      
      ── 2. Failure: coalesce_() works correctly (@test_01_utils.R#130)  ─────────────
      `dplyr::coalesce(dta$col3, last)` did not throw an error.
      
      Done simulation(s).
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 300 | SKIPPED: 45 | WARNINGS: 2 | FAILED: 2 ]
      1. Failure: coalesce_() works correctly (@test_01_utils.R#129) 
      2. Failure: coalesce_() works correctly (@test_01_utils.R#130) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# rsample

<details>

* Version: 0.0.6
* Source code: https://github.com/cran/rsample
* URL: https://tidymodels.github.io/rsample, https://github.com/tidymodels/rsample
* BugReports: https://github.com/tidymodels/rsample/issues
* Date/Publication: 2020-03-31 19:50:02 UTC
* Number of recursive dependencies: 98

Run `cloud_details(, "rsample")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Attributes: < Component 2: Numeric: lengths (40, 1) differ >
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 574 | SKIPPED: 2 | WARNINGS: 0 | FAILED: 8 ]
      1. Failure: rsplit labels (@test_boot.R#89) 
      2. Failure: rsplit labels (@test_group.R#104) 
      3. Failure: rsplit labels (@test_mc.R#86) 
      4. Failure: rsplit labels (@test_nesting.R#71) 
      5. Failure: rsplit labels (@test_rolling.R#102) 
      6. Failure: rsplit labels (@test_validation.R#90) 
      7. Failure: rsplit labels (@test_vfold.R#85) 
      8. Failure: rsplit labels (@test_vfold.R#90) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# RTL

<details>

* Version: 0.1.1
* Source code: https://github.com/cran/RTL
* URL: https://github.com/risktoollib/RTL
* Date/Publication: 2020-02-23 18:50:02 UTC
* Number of recursive dependencies: 138

Run `cloud_details(, "RTL")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘RTL-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: chart_zscore
    > ### Title: 'chart_zscore'
    > ### Aliases: chart_zscore
    > 
    > ### ** Examples
    > 
    > chart_zscore(df = ng_storage, title = "NG Storage Z Score",
    + per = "yearweek", output = "stl", chart = "seasons")
    Warning: The `add` argument of `group_by()` is deprecated as of dplyr 1.0.0.
    Please use the `.add` argument instead.
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_warnings()` to see where this warning was generated.
    Error in UseMethod("model") : 
      no applicable method for 'model' applied to an object of class "c('grouped_df', 'tbl_df', 'tbl', 'data.frame')"
    Calls: chart_zscore ... eval -> _fseq -> freduce -> <Anonymous> -> <Anonymous>
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘quantmod’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 15456 marked UTF-8 strings
    ```

# saotd

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/saotd
* BugReports: https://github.com/evan-l-munson/saotd/issues
* Date/Publication: 2019-04-04 16:30:03 UTC
* Number of recursive dependencies: 118

Run `cloud_details(, "saotd")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── 5. Failure: unigrams are computed properly (@test_unigram.R#18)  ────────────
      saotd::unigram(DataFrame = test_unigram_df) not equal to `correct_unigram_df`.
      Attributes: < Component "class": Lengths (1, 3) differ (string compare on first 1) >
      Attributes: < Component "class": 1 string mismatch >
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 63 | SKIPPED: 0 | WARNINGS: 6 | FAILED: 5 ]
      1. Failure: bigrams are computed properly (@test_bigram.R#19) 
      2. Error: (unknown) (@test_number_topics.R#12) 
      3. Failure: Trigrams are computed properly (@test_trigram.R#21) 
      4. Error: (unknown) (@test_tweet_topics.R#12) 
      5. Failure: unigrams are computed properly (@test_unigram.R#18) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 826 marked UTF-8 strings
    ```

# sergeant

<details>

* Version: 0.5.2
* Source code: https://github.com/cran/sergeant
* URL: https://github.com/hrbrmstr/sergeant
* BugReports: https://github.com/hrbrmstr/sergeant/issues
* Date/Publication: 2017-07-17 22:36:26 UTC
* Number of recursive dependencies: 62

Run `cloud_details(, "sergeant")` for more info

</details>

## Newly broken

*   checking S3 generic/method consistency ... WARNING
    ```
    src_tbls:
      function(x, ...)
    src_tbls.src_drill:
      function(x)
    
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

# simglm

<details>

* Version: 0.7.4
* Source code: https://github.com/cran/simglm
* Date/Publication: 2019-05-31 17:10:03 UTC
* Number of recursive dependencies: 90

Run `cloud_details(, "simglm")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── 1. Failure: interupt TS (@test_knots.r#69)  ─────────────────────────────────
      table(temp_long$time_k) not equal to table(temp_long$time >= 3).
      Numeric: lengths (2, 0) differ
      
      ── 2. Failure: interupt TS (@test_knots.r#96)  ─────────────────────────────────
      table(temp_long$time_k) not equal to table(temp_long$time >= 3).
      Numeric: lengths (2, 0) differ
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 129 | SKIPPED: 0 | WARNINGS: 2 | FAILED: 2 ]
      1. Failure: interupt TS (@test_knots.r#69) 
      2. Failure: interupt TS (@test_knots.r#96) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# simITS

<details>

* Version: 0.1.0
* Source code: https://github.com/cran/simITS
* Date/Publication: 2020-04-28 09:40:02 UTC
* Number of recursive dependencies: 75

Run `cloud_details(, "simITS")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### Aliases: adjust_data
    > 
    > ### ** Examples
    > 
    > data( "meck_subgroup" )
    > head( meck_subgroup )
    # A tibble: 6 x 5
    # Groups:   month [2]
      month n.cases n.bail pbail category
      <dbl>   <int>  <dbl> <dbl> <fct>   
    1   -29     550    349 0.635 felony  
    2   -29    1316    699 0.531 misdem  
    3   -29     305    174 0.570 traffic 
    4   -28     549    378 0.689 felony  
    5   -28    1338    708 0.529 misdem  
    6   -28     334    193 0.578 traffic 
    > pis = calculate_group_weights( "category", Nname="n.cases", 
    +                                meck_subgroup, t_min=0, t_max= max( meck_subgroup$month ) )
    > pis
    # A tibble: 3 x 3
      category     N pi_star
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
          is_false, is_null, is_true
      
      ── 1. Error: vague tests of post_stratified_ITS (@test-post_stratified_ITS.R#14)
      'by' must specify a uniquely valid column
      Backtrace:
       1. simITS::adjust_data(...)
       3. base::merge.data.frame(adj.dat, sdat, by = c("N", "month"), all = TRUE)
       4. base:::fix.by(by.y, y)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 81 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Error: vague tests of post_stratified_ITS (@test-post_stratified_ITS.R#14) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# sistec

<details>

* Version: 0.0.2
* Source code: https://github.com/cran/sistec
* URL: https://github.com/r-ifpe/sistec
* BugReports: https://github.com/r-ifpe/sistec/issues
* Date/Publication: 2020-05-11 12:50:03 UTC
* Number of recursive dependencies: 50

Run `cloud_details(, "sistec")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > sistec <- read_sistec(system.file("extdata/examples/sistec",
    +                                   package = "sistec"))
    >                                   
    > compare_sistec(sistec, qacademico)                                   
    Error: Input must be a vector, not a `data.frame/sistec_data_frame` object.
    Backtrace:
         █
      1. ├─sistec::compare_sistec(sistec, qacademico)
      2. ├─sistec:::compare_sistec.qacademico_data_frame(sistec, qacademico)
      3. │ └─sistec:::compare_sistec_qacademico(sistec, student_registration)
      4. │   └─sistec:::filter_cpf_sistec(sistec)
      5. │     ├─dplyr::filter(x, !!sym("NU_CPF") == "")
      6. │     └─dplyr:::filter.data.frame(x, !!sym("NU_CPF") == "")
      7. │       ├─dplyr::dplyr_row_slice(.data, loc, preserve = .preserve)
      8. │       └─dplyr:::dplyr_row_slice.data.frame(.data, loc, preserve = .preserve)
      9. │         ├─dplyr::dplyr_reconstruct(vec_slice(data, i), data)
     10. │         ├─dplyr:::dplyr_reconstruct.data.frame(vec_slice(data, i), data)
     11. │         └─vctrs::vec_slice(data, i)
     12. └─vctrs:::stop_scalar_type(...)
     13.   └─vctrs:::stop_vctrs(msg, "vctrs_error_scalar_type", actual = x)
    Execution halted
    ```

# skynet

<details>

* Version: 1.3.0
* Source code: https://github.com/cran/skynet
* URL: https://github.com/FilipeamTeixeira/skynet
* BugReports: https://github.com/FilipeamTeixeira/skynet/issues
* Date/Publication: 2018-12-12 10:20:03 UTC
* Number of recursive dependencies: 84

Run `cloud_details(, "skynet")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(testthat)
      > library(skynet)
      > 
      > test_check("skynet")
      ── 1. Failure: Find Airport (@test_smallerfunctions.R#7)  ──────────────────────
      `print\(findAirport\("ATL"\)\[2\]\)` does not match "30397".
      Actual value: "   origin city_mkt_id city latitude longitude\\n1:   <NA>          NA <NA>       NA        NA"
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 67 | SKIPPED: 0 | WARNINGS: 6 | FAILED: 1 ]
      1. Failure: Find Airport (@test_smallerfunctions.R#7) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘maps’
      All declared Imports should be used.
    ```

# srvyr

<details>

* Version: 0.3.9
* Source code: https://github.com/cran/srvyr
* URL: http://gdfe.co/srvyr, https://github.com/gergness/srvyr
* BugReports: https://github.com/gergness/srvyr/issues
* Date/Publication: 2020-05-04 05:20:12 UTC
* Number of recursive dependencies: 88

Run `cloud_details(, "srvyr")` for more info

</details>

## Newly broken

*   checking for code/documentation mismatches ... WARNING
    ```
    Codoc mismatches from documentation object 'summarise':
    summarise
      Code: function(.data, ..., .groups = NULL)
      Docs: function(.data, ...)
      Argument names in code not in docs:
        .groups
    summarize
      Code: function(.data, ..., .groups = NULL)
      Docs: function(.data, ...)
      Argument names in code not in docs:
        .groups
    ```

# strapgod

<details>

* Version: 0.0.4
* Source code: https://github.com/cran/strapgod
* URL: https://github.com/DavisVaughan/strapgod
* BugReports: https://github.com/DavisVaughan/strapgod/issues
* Date/Publication: 2019-09-20 04:50:02 UTC
* Number of recursive dependencies: 64

Run `cloud_details(, "strapgod")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Component 5: Attributes: < target is NULL, current is list >
      Component 5: target is numeric, current is factor
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 146 | SKIPPED: 0 | WARNINGS: 3 | FAILED: 7 ]
      1. Failure: bind_rows() fails sadly (@test-dplyr-compat.R#341) 
      2. Failure: bind_cols() works (@test-dplyr-compat.R#354) 
      3. Failure: bind_cols() works (@test-dplyr-compat.R#366) 
      4. Failure: bind_cols() works (@test-dplyr-compat.R#374) 
      5. Error: group_modify() (@test-dplyr-group-funs.R#43) 
      6. Failure: group_map() (@test-dplyr-group-funs.R#66) 
      7. Failure: group_walk() (@test-dplyr-group-funs.R#106) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# StratigrapheR

<details>

* Version: 1.0.1
* Source code: https://github.com/cran/StratigrapheR
* Date/Publication: 2020-03-20 13:50:06 UTC
* Number of recursive dependencies: 123

Run `cloud_details(, "StratigrapheR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### ** Examples
    > 
    > l  <- matrix(1:30, ncol = 3, byrow = FALSE)
    > r  <- matrix(2:31, ncol = 3, byrow = FALSE)
    > id <- matrix(rep(c("C1", "C2", "C3"),10), ncol = 3, byrow = TRUE)
    > y  <- matrix(rep(1:10,3), ncol = 3, byrow = FALSE)
    > xout <- seq(-2,32,0.5)
    > 
    > res  <- tie.lim(l = l, r = r,  y = y, xout = xout, id = id)
    > 
    > cont <- tie.lim(l = l, r = r,  y = y, id = id)
    Error: Input must be a vector, not NULL.
    Backtrace:
        █
     1. ├─StratigrapheR::tie.lim(l = l, r = r, y = y, id = id)
     2. │ └─dplyr::lag(xout)
     3. │   ├─vctrs::vec_c(...)
     4. │   └─vctrs::vec_slice(inputs$x, seq_len(xlen - n))
     5. └─vctrs:::stop_scalar_type(.Primitive("quote")(NULL), "")
     6.   └─vctrs:::stop_vctrs(msg, "vctrs_error_scalar_type", actual = x)
    Execution halted
    ```

# survminer

<details>

* Version: 0.4.6
* Source code: https://github.com/cran/survminer
* URL: http://www.sthda.com/english/rpkgs/survminer/
* BugReports: https://github.com/kassambara/survminer/issues
* Date/Publication: 2019-09-03 23:00:02 UTC
* Number of recursive dependencies: 119

Run `cloud_details(, "survminer")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_warnings()` to see where this warning was generated.
    Error: `x` must be a vector, not a `grouped_df/tbl_df/tbl/data.frame/surv_group_by` object.
    Backtrace:
         █
      1. └─survminer::ggsurvplot_facet(...)
      2.   └─grouped.d %>% tibble::add_column(fit = sf)
      3.     ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      4.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5.       └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      6.         └─survminer:::`_fseq`(`_lhs`)
      7.           └─magrittr::freduce(value, `_function_list`)
      8.             ├─base::withVisible(function_list[[k]](value))
      9.             └─function_list[[k]](value)
     10.               └─tibble::add_column(., fit = sf)
     11.                 ├─base::`[<-`(...)
     12.                 └─dplyr:::`[<-.grouped_df`(...)
     13.                   └─dplyr::grouped_df(out, group_intersect(x, out), group_by_drop_default(x))
     14.                     └─dplyr:::compute_groups(data, vars, drop = drop)
     15.                    
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        doc   5.1Mb
    ```

# textreuse

<details>

* Version: 0.1.4
* Source code: https://github.com/cran/textreuse
* URL: https://github.com/ropensci/textreuse
* BugReports: https://github.com/ropensci/textreuse/issues
* Date/Publication: 2016-11-28 16:54:10
* Number of recursive dependencies: 60

Run `cloud_details(, "textreuse")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_warnings()` to see where this warning was generated.
    Error: Input must be a vector, not a `tbl_df/tbl/data.frame/lsh_buckets` object.
    Backtrace:
         █
      1. ├─textreuse::lsh_candidates(buckets)
      2. │ └─`%>%`(...)
      3. │   ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      4. │   └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5. │     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      6. │       └─textreuse:::`_fseq`(`_lhs`)
      7. │         └─magrittr::freduce(value, `_function_list`)
      8. │           └─function_list[[i]](value)
      9. │             ├─dplyr::filter_(., ~doc.x != doc.y)
     10. │             └─dplyr:::filter_.tbl_df(., ~doc.x != doc.y)
     11. │               ├─dplyr::filter(.data, !!!dots)
     12. │               └─dplyr:::filter.data.frame(.data, !!!dots)
     13. │                 ├─dplyr::dplyr_row_slice(.data, loc, preserve = .preserve)
     14. │                 └─dplyr:::dplyr_row_slice.data.frame(.data, loc, preserve = .preserve)
     15. │ 
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > 
      > test_check("textreuse")
      ── 1. Error: (unknown) (@test-lsh.R#10)  ───────────────────────────────────────
      Input must be a vector, not a `tbl_df/tbl/data.frame/lsh_buckets` object.
      Backtrace:
        1. textreuse::lsh_candidates(buckets)
       18. vctrs:::stop_scalar_type(...)
       19. vctrs:::stop_vctrs(msg, "vctrs_error_scalar_type", actual = x)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 129 | SKIPPED: 2 | WARNINGS: 7 | FAILED: 1 ]
      1. Error: (unknown) (@test-lsh.R#10) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        extdata   2.9Mb
        libs      1.8Mb
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘tm’
    ```

# tidyjson

<details>

* Version: 0.2.4
* Source code: https://github.com/cran/tidyjson
* URL: https://github.com/colearendt/tidyjson
* BugReports: https://github.com/colearendt/tidyjson/issues
* Date/Publication: 2019-12-02 21:39:30
* Number of recursive dependencies: 89

Run `cloud_details(, "tidyjson")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > # Stack names
    > '{"first": "bob", "last": "jones"}' %>%
    +   gather_object %>%
    +   append_values_string
    # A tbl_json: 2 x 3 tibble with a "JSON" attribute
      `attr(., "JSON")` document.id name  string
      <chr>                   <int> <chr> <chr> 
    1 "\"bob\""                   1 first bob   
    2 "\"jones\""                 1 last  jones 
    > 
    > # This is most useful when data is stored in name-value pairs
    > # For example, tags in recipes:
    > recipes <- c('{"name": "pie", "tags": {"apple": 10, "pie": 2, "flour": 5}}',
    +              '{"name": "cookie", "tags": {"chocolate": 2, "cookie": 1}}')
    > recipes %>%
    +   spread_values(name = jstring(name)) %>%
    +   enter_object(tags) %>%
    +   gather_object("tag") %>%
    +   append_values_number("count")
    Error: nrow(df) not equal to length(json.list)
    Execution halted
    ```

## In both

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 182 | SKIPPED: 2 | WARNINGS: 0 | FAILED: 39 ]
      1. Failure: has correct complete structure with simple input (@test-append_values.R#7) 
      2. Failure: recursive works as expected (@test-append_values.R#191) 
      3. Failure: recursive works as expected (@test-append_values.R#206) 
      4. Error: filter removes records with missing path (@test-enter_object.R#52) 
      5. Error: works if no paths exist (@test-enter_object.R#71) 
      6. Failure: works in a simple case (@test-gather_object.R#7) 
      7. Failure: works with compound values (@test-gather_object.R#31) 
      8. Failure: column.name works and doesn't clobber existing name (@test-gather_object.R#80) 
      9. Failure: preserves a NULL column (@test-gather_object.R#100) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

# tidyr

<details>

* Version: 1.0.3
* Source code: https://github.com/cran/tidyr
* URL: https://tidyr.tidyverse.org, https://github.com/tidyverse/tidyr
* BugReports: https://github.com/tidyverse/tidyr/issues
* Date/Publication: 2020-05-07 08:40:02 UTC
* Number of recursive dependencies: 61

Run `cloud_details(, "tidyr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 546 | SKIPPED: 2 | WARNINGS: 3 | FAILED: 13 ]
      1. Failure: not drop unspecified levels in complete (@test-complete.R#42) 
      2. Failure: nesting doesn't expand values (@test-expand.R#17) 
      3. Failure: named data frames are not flattened (@test-expand.R#32) 
      4. Failure: nest turns grouped values into one list-df (@test-nest-legacy.R#8) 
      5. Failure: nest works with data frames too (@test-nest-legacy.R#16) 
      6. Failure: nest doesn't include grouping vars in nested data (@test-nest-legacy.R#30) 
      7. Failure: elements must all be of same type (@test-nest-legacy.R#139) 
      8. Failure: unnesting zero row column preserves names (@test-nest-legacy.R#275) 
      9. Failure: nest turns grouped values into one list-df (@test-nest.R#10) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 24 marked UTF-8 strings
    ```

# tidyRSS

<details>

* Version: 2.0.1
* Source code: https://github.com/cran/tidyRSS
* URL: https://github.com/RobertMyles/tidyrss
* BugReports: https://github.com/RobertMyles/tidyrss/issues
* Date/Publication: 2020-03-07 16:00:02 UTC
* Number of recursive dependencies: 60

Run `cloud_details(, "tidyRSS")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── 2. Error: df is cleaned properly (@test_general.R#84)  ──────────────────────
      Argument 1 must have names.
      Backtrace:
        1. testthat::expect_equal(...)
        4. tidyRSS:::clean_up(df, "rss", clean_tags = TRUE, parse_dates = TRUE)
       10. purrr::map_df(...)
       13. dplyr::bind_rows(res, .id = .id)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 34 | SKIPPED: 0 | WARNINGS: 2 | FAILED: 2 ]
      1. Error: RSS responses are parsed (@test_general.R#35) 
      2. Error: df is cleaned properly (@test_general.R#84) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# tidystats

<details>

* Version: 0.4
* Source code: https://github.com/cran/tidystats
* Date/Publication: 2019-09-12 07:20:02 UTC
* Number of recursive dependencies: 29

Run `cloud_details(, "tidystats")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    +   group_by(source) %>%
    +   describe_data(response)
    Error: `x` must be a vector, not a `grouped_df/tbl_df/tbl/data.frame/tidystats_descriptives` object.
    Backtrace:
         █
      1. ├─(function (x, ...) ...
      2. └─tibble:::print.tbl(x)
      3.   ├─cli::cat_line(format(x, ..., n = n, width = width, n_extra = n_extra))
      4.   │ └─base::paste0(..., collapse = "\n")
      5.   ├─base::format(x, ..., n = n, width = width, n_extra = n_extra)
      6.   └─tibble:::format.tbl(x, ..., n = n, width = width, n_extra = n_extra)
      7.     └─tibble::trunc_mat(x, n = n, width = width, n_extra = n_extra)
      8.       ├─base::as.data.frame(head(x, n))
      9.       ├─utils::head(x, n)
     10.       └─utils:::head.data.frame(x, n)
     11.         ├─x[seq_len(n), , drop = FALSE]
     12.         └─dplyr:::`[.grouped_df`(x, seq_len(n), , drop = FALSE)
     13.           └─dplyr::grouped_df(out, groups, group_by_drop_default(x))
     14.             └─dplyr:::compute_groups(data, vars, drop = drop)
     15.               ├─tibble::as_tib
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tidyr’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 113 marked UTF-8 strings
    ```

# tidystopwords

<details>

* Version: 0.9.0
* Source code: https://github.com/cran/tidystopwords
* Date/Publication: 2019-02-12 17:20:02 UTC
* Number of recursive dependencies: 36

Run `cloud_details(, "tidystopwords")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    
    > ### Name: generate_stoplist
    > ### Title: Listing of stop words with control over language and part of
    > ###   speech.
    > ### Aliases: generate_stoplist
    > 
    > ### ** Examples
    > 
    >     # standard usage (might return some non-ASCII characters):
    >     generate_stoplist(lang_name = "English")
    Error: Can't combine `..1$language_id` <logical> and `..2$language_id` <character>.
    Backtrace:
        █
     1. ├─tidystopwords::generate_stoplist(lang_name = "English")
     2. │ └─dplyr::bind_rows(stoplist_db, ling_filter_db)
     3. │   └─vctrs::vec_rbind(!!!dots, .names_to = .id)
     4. └─vctrs::vec_default_ptype2(...)
     5.   └─vctrs::stop_incompatible_type(...)
     6.     └─vctrs:::stop_incompatible(...)
     7.       └─vctrs:::stop_vctrs(...)
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 229801 marked UTF-8 strings
    ```

# timetk

<details>

* Version: 1.0.0
* Source code: https://github.com/cran/timetk
* URL: https://github.com/business-science/timetk
* BugReports: https://github.com/business-science/timetk/issues
* Date/Publication: 2020-04-19 17:50:02 UTC
* Number of recursive dependencies: 150

Run `cloud_details(, "timetk")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Error: Problem with `mutate()` input `nested.col`.
    ✖ Can't recycle `..1` (size 169) to match `..2` (size 0).
    ℹ Input `nested.col` is `purrr::map(...)`.
    ℹ The error occured in group 1: id = "H10".
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
      9.             ├─timetk::plot_acf_diagnostics(...)
     10.             └─timetk:::plot_acf_diagnostics.grouped_df(...)
     11.               ├─timetk::tk_acf_diagnostics(...)
     12.               └─timetk:::tk_acf_diagnostics.grouped_df(...)
     13.                 └─`%>%`(...)
     14.                   ├─base::withVisible(eval(quote(`_fseq`(`_
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.5Mb
      sub-directories of 1Mb or more:
        doc   5.1Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2750 marked UTF-8 strings
    ```

# tree.bins

<details>

* Version: 0.1.1
* Source code: https://github.com/cran/tree.bins
* Date/Publication: 2018-06-14 05:33:53 UTC
* Number of recursive dependencies: 69

Run `cloud_details(, "tree.bins")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Component 2: Attributes: < Component "row.names": target is character, current is numeric >
      Testing that the df is the same
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 9 | SKIPPED: 0 | WARNINGS: 1 | FAILED: 4 ]
      1. Error: Test to see if lookup tables and joins are performed correctly (@test-bin.oth.R#63) 
      2. Failure: Testing for 2 predictors,
                one will provide a null list and the other will recategorize the variable. (@test-tree.bins.R#127) 
      3. Error: Testing for 2 predictors,
                both will recategorize the variable.
                Recategorized variable will contain multiple leaves. (@test-tree.bins.R#169) 
      4. Failure: Check for correct classes and that order does not affect y (SalePrice here) (@test-tree.bins.R#239) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘forcats’
    ```

# treeplyr

<details>

* Version: 0.1.7
* Source code: https://github.com/cran/treeplyr
* URL: https://github.com/uyedaj/treeplyr
* BugReports: https://github.com/uyedaj/treeplyr/issues
* Date/Publication: 2019-07-25 22:50:02 UTC
* Number of recursive dependencies: 52

Run `cloud_details(, "treeplyr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### Aliases: group_by_.treedata group_by.treedata ungroup.grouped_treedata
    > 
    > ### ** Examples
    > 
    > data(anolis)
    > td <- make.treedata(anolis$phy, anolis$dat)
    > tdGrouped <- group_by(td, ecomorph)
    Warning: `group_by_()` is deprecated as of dplyr 0.7.0.
    Please use `group_by()` instead.
    See vignette('programming') for more help
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_warnings()` to see where this warning was generated.
    Error: `vars` must be a character vector.
    Backtrace:
        █
     1. ├─dplyr::group_by(td, ecomorph)
     2. └─dplyr:::group_by.default(td, ecomorph)
     3.   ├─dplyr::group_by_(.data, .dots = compat_as_lazy_dots(...), add = add)
     4.   └─treeplyr:::group_by_.treedata(...)
     5.     └─dplyr::grouped_df(groups$data, groups$groups)
    Execution halted
    ```

# trelliscopejs

<details>

* Version: 0.2.4
* Source code: https://github.com/cran/trelliscopejs
* URL: https://github.com/hafen/trelliscopejs
* BugReports: https://github.com/hafen/trelliscopejs/issues
* Date/Publication: 2020-02-10 22:40:02 UTC
* Number of recursive dependencies: 100

Run `cloud_details(, "trelliscopejs")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
      Calling 'structure(NULL, *)' is deprecated, as NULL cannot have attributes.
      Consider 'structure(list(), *)' instead.
    Warning in structure(x, class = unique(c("AsIs", oldClass(x)))) :
      Calling 'structure(NULL, *)' is deprecated, as NULL cannot have attributes.
      Consider 'structure(list(), *)' instead.
    Warning in structure(x, class = unique(c("AsIs", oldClass(x)))) :
      Calling 'structure(NULL, *)' is deprecated, as NULL cannot have attributes.
      Consider 'structure(list(), *)' instead.
    > 
    > trelliscope(mpg_cog, name = "city_vs_highway_mpg", nrow = 1, ncol = 2)
    Error: Input must be a vector, not a `grouped_df/tbl_df/tbl/data.frame/cognostics` object.
    Backtrace:
        █
     1. ├─trelliscopejs::trelliscope(...)
     2. ├─trelliscopejs:::trelliscope.data.frame(...)
     3. │ └─trelliscopejs:::cog_df_info(...)
     4. │   └─dplyr::bind_cols(cogs)
     5. │     └─vctrs::vec_cbind(!!!dots)
     6. └─vctrs:::stop_scalar_type(...)
     7.   └─vctrs:::stop_vctrs(msg, "vctrs_error_scalar_type", actual = x)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       47. testthat:::failure_summary(result, self$n_fail)
       50. testthat:::format.expectation(x)
       51. testthat:::format_with_trace(x)
       53. rlang:::format.rlang_trace(...)
       54. rlang:::trace_format_branch(x, max_frames, dir, srcrefs)
       55. rlang:::branch_uncollapse_pipe(trace)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 0 | SKIPPED: 0 | WARNINGS: 224 | FAILED: 3 ]
      1. Error: examples run without barfing (@test-trelliscope.R#22) 
      2. Error: examples run without barfing (@test-trelliscope.R#3) 
      3. Error: (unknown) (@test-trelliscope.R#3) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# tsibble

<details>

* Version: 0.8.6
* Source code: https://github.com/cran/tsibble
* URL: https://tsibble.tidyverts.org
* BugReports: https://github.com/tidyverts/tsibble/issues
* Date/Publication: 2020-01-31 06:20:11 UTC
* Number of recursive dependencies: 93

Run `cloud_details(, "tsibble")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    +   group_by_key() %>%
    +   fill_gaps(kilo = sum(kilo))
    Error: `vars` must be a character vector.
    Backtrace:
         █
      1. └─harvest %>% group_by_key() %>% fill_gaps(kilo = sum(kilo))
      2.   ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      3.   └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      4.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5.       └─`_fseq`(`_lhs`)
      6.         └─magrittr::freduce(value, `_function_list`)
      7.           ├─base::withVisible(function_list[[k]](value))
      8.           └─function_list[[k]](value)
      9.             ├─tsibble::fill_gaps(., kilo = sum(kilo))
     10.             └─tsibble:::fill_gaps.tbl_ts(., kilo = sum(kilo))
     11.               ├─dplyr::left_join(gap_data, replaced_df, by = by_name)
     12.               └─tsibble:::left_join.tbl_ts(gap_data, replaced_df, by = by_name)
     13.                 └─tsibble:::update_meta(...)
     14.                   └─tsibble:::retain_tsibble(new, key = key(old), index = index(old))
     15.               
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 545 | SKIPPED: 2 | WARNINGS: 4 | FAILED: 35 ]
      1. Failure: 4 day interval (@test-append.R#27) 
      2. Error: (unknown) (@test-append.R#31) 
      3. Error: (unknown) (@test-bind.R#11) 
      4. Error: (unknown) (@test-dplyr.R#5) 
      5. Error: (unknown) (@test-empty.R#32) 
      6. Error: (unknown) (@test-gaps.R#93) 
      7. Error: (unknown) (@test-groups.R#3) 
      8. Error: From Date to year-week, year-month, year-quarter and year (@test-indexby.R#84) 
      9. Failure: index_by() with group_by() (@test-indexby.R#106) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘dtplyr’, ‘dbplyr’
    ```

# TSstudio

<details>

* Version: 0.1.6
* Source code: https://github.com/cran/TSstudio
* URL: https://github.com/RamiKrispin/TSstudio
* BugReports: https://github.com/RamiKrispin/TSstudio/issues
* Date/Publication: 2020-01-21 05:30:02 UTC
* Number of recursive dependencies: 136

Run `cloud_details(, "TSstudio")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘TSstudio-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: EURO_Brent
    > ### Title: Crude Oil Prices: Brent - Europe
    > ### Aliases: EURO_Brent
    > ### Keywords: datasets
    > 
    > ### ** Examples
    > 
    > ts_plot(EURO_Brent)
    > ts_decompose(EURO_Brent, type = "both")
    Error in attributes(.Data) <- c(attributes(.Data), attrib) : 
      invalid time series parameters specified
    Calls: ts_decompose ... plotly_build -> plotly_build.plotly -> lapply -> FUN -> structure
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘forecastHybrid’
      All declared Imports should be used.
    ```

# vcfR

<details>

* Version: 1.10.0
* Source code: https://github.com/cran/vcfR
* URL: https://github.com/knausb/vcfR, https://knausb.github.io/vcfR_documentation/
* Date/Publication: 2020-02-06 09:50:02 UTC
* Number of recursive dependencies: 111

Run `cloud_details(, "vcfR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > # to integer or numeric types...
    > Z <- vcfR2tidy(vcf)
    Error: Can't combine `..1$tt` <logical> and `..2$tt` <character>.
    Backtrace:
         █
      1. ├─vcfR::vcfR2tidy(vcf)
      2. │ ├─base::do.call(what = extract_gt_tidy, args = format_dots)
      3. │ └─(function (x, format_fields = NULL, format_types = TRUE, dot_is_NA = TRUE, ...
      4. │   └─vcfR:::guess_types(format_df %>% dplyr::filter(ID %in% format_fields))
      5. │     └─`%>%`(...)
      6. │       ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      7. │       └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      8. │         └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      9. │           └─vcfR:::`_fseq`(`_lhs`)
     10. │             └─magrittr::freduce(value, `_function_list`)
     11. │               ├─base::withVisible(function_list[[k]](value))
     12. │               └─function_list[[k]](value)
     13. │                 └─dplyr::bind_rows(., tmp)
     14. │                   └─vctrs::vec_rbind(!!!dots, .names_to = .id)
     15.
    Execution halted
    ```

## In both

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       17. vctrs:::stop_incompatible(...)
       18. vctrs:::stop_vctrs(...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 471 | SKIPPED: 0 | WARNINGS: 2 | FAILED: 7 ]
      1. Failure: vcfR2genind works (@test_conversion.R#15) 
      2. Failure: vcfR2genind works, return.alleles = TRUE (@test_conversion.R#22) 
      3. Error: vcfR2genind works, return.alleles = TRUE (@test_conversion.R#23) 
      4. Error: vcfR2genlight works (@test_conversion.R#31) 
      5. Error: extract_gt_tidy works for GT element (@test_vcfRtidy.R#55) 
      6. Error: extract_gt_tidy works for all elements (@test_vcfRtidy.R#70) 
      7. Error: vcfR2tidy works (@test_vcfRtidy.R#81) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 10.3Mb
      sub-directories of 1Mb or more:
        libs   8.4Mb
    ```

# vpc

<details>

* Version: 1.2.0
* Source code: https://github.com/cran/vpc
* URL: https://github.com/ronkeizer/vpc
* Date/Publication: 2020-05-07 15:10:02 UTC
* Number of recursive dependencies: 70

Run `cloud_details(, "vpc")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > # simple function to simulate categorical data for single individual
    > sim_id <- function(id = 1) {
    +   n <- 10
    +   logit <- function(x) exp(x) / (1+exp(x))
    +   data.frame(id = id, time = seq(1, n, length.out = n),
    +              dv = round(logit((1:n) - n/2 + rnorm(n, 0, 1.5))) )
    + }
    > ## simple function to simulate categorical data for a trial
    > sim_trial <- function(i = 1, n = 20) { # function to simulate categorical data for a trial
    +   data.frame(sim = i, do.call("rbind", lapply(1:n, sim_id)))
    + }
    > 
    > ## simulate single trial for 20 individuals
    > obs <- sim_trial(n = 20)
    > 
    > ## simulate 200 trials of 20 individuals
    > sim <- do.call("rbind", lapply(1:200, sim_trial, n = 20))
    > 
    > ## Plot categorical VPC
    > vpc_cat(sim = sim, obs = obs)
    New names:
    ```

*   checking tests ... ERROR
    ```
    ...
      Running ‘test-softwaretype.R’
      Running ‘test-strat-col-detection.R’
      Running ‘test-vpc.R’
      Running ‘test-vpc_cat.R’
    Running the tests in ‘tests/test-vpc_cat.R’ failed.
    Last 13 lines of output:
      * `fact_perc(dv, lev[i])` -> `fact_perc(dv, lev[i])...3`
      * sim -> sim...4
      * bin -> bin...5
      * ...
      New names:
      * NA -> ...1
      * NA -> ...2
      * NA -> ...3
      * NA -> ...4
      * NA -> ...5
      * ...
      Error in names(x) <- value : 
        'names' attribute [6] must be the same length as the vector [0]
      Calls: vpc_cat -> colnames<-
      Execution halted
    ```

# yamlet

<details>

* Version: 0.4.6
* Source code: https://github.com/cran/yamlet
* Date/Publication: 2020-03-14 05:40:02 UTC
* Number of recursive dependencies: 69

Run `cloud_details(, "yamlet")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Packages unavailable to check Rd xrefs: ‘dtplyr’, ‘dbplyr’
    Missing link or links in documentation object 'anti_join.decorated.Rd':
      ‘join.tbl_df’
    
    Missing link or links in documentation object 'full_join.decorated.Rd':
      ‘join.tbl_df’
    
    Missing link or links in documentation object 'inner_join.decorated.Rd':
      ‘join.tbl_df’
    
    Missing link or links in documentation object 'left_join.decorated.Rd':
      ‘join.tbl_df’
    
    Missing link or links in documentation object 'right_join.decorated.Rd':
      ‘join.tbl_df’
    
    Missing link or links in documentation object 'semi_join.decorated.Rd':
      ‘join.tbl_df’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

## Newly fixed

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘dtplyr’, ‘dbplyr’
    ```

