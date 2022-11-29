# admiral

<details>

* Version: 0.8.4
* GitHub: https://github.com/pharmaverse/admiral
* Source code: https://github.com/cran/admiral
* Date/Publication: 2022-10-14 12:32:33 UTC
* Number of recursive dependencies: 127

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
      
      
      [ FAIL 11 | WARN 15 | SKIP 0 | PASS 487 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘admiral-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: create_query_data
    > ### Title: Creates a queries dataset as input dataset to the
    > ###   'dataset_queries' argument in 'derive_vars_query()'
    > ### Aliases: create_query_data
    > ### Keywords: der_occds
    > 
    > ### ** Examples
    ...
    > # In a real application a company-specific function must be used.
    > create_query_data(
    +   queries = list(pregsmq, bilismq),
    +   get_smq_fun = admiral.test:::get_smq_terms,
    +   meddra_version = "20.1"
    + )
    Error in assert_function(get_smq_fun, params = c("smq_select", "version",  : 
      object 'get_smq_terms' not found
    Calls: create_query_data -> assert_function
    Execution halted
    ```

# APCI

<details>

* Version: 1.0.6
* GitHub: NA
* Source code: https://github.com/cran/APCI
* Date/Publication: 2022-11-11 08:00:02 UTC
* Number of recursive dependencies: 90

Run `cloud_details(, "APCI")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘1_tests.R’
    Running the tests in ‘tests/1_tests.R’ failed.
    Last 13 lines of output:
          acc2:pcc2     acc3:pcc2     acc4:pcc2     acc5:pcc2     acc6:pcc2 
       -0.246807300  -0.009984113  -0.287185204   0.059172065   0.479577551 
          acc7:pcc2     acc8:pcc2     acc9:pcc2     acc1:pcc3     acc2:pcc3 
        0.172601922  -0.331601706   0.180611191   0.027875596   0.011375660 
          acc3:pcc3     acc4:pcc3     acc5:pcc3     acc6:pcc3     acc7:pcc3 
       -0.401123536   0.070485179   0.188973406  -0.314219322   0.166869822 
          acc8:pcc3     acc9:pcc3     acc1:pcc4     acc2:pcc4     acc3:pcc4 
       -0.057897624   0.191496011  -0.386928713  -0.019160692   0.282787236 
          acc4:pcc4     acc5:pcc4     acc6:pcc4     acc7:pcc4     acc8:pcc4 
        0.333615167   0.288328553  -0.156792638  -0.046689464  -0.387160103 
          acc9:pcc4     acc1:pcc5     acc2:pcc5     acc3:pcc5     acc4:pcc5 
        0.079468643   0.074017894   0.230123240  -0.034270729  -0.072397670 
          acc5:pcc5     acc6:pcc5     acc7:pcc5     acc8:pcc5     acc9:pcc5 
       -0.602687962  -0.352411413   0.315452200   0.674444551  -0.320119535 
      Killed
    ```

# AQuadtree

<details>

* Version: 1.0.2
* GitHub: NA
* Source code: https://github.com/cran/AQuadtree
* Date/Publication: 2020-09-08 07:50:04 UTC
* Number of recursive dependencies: 102

Run `cloud_details(, "AQuadtree")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘AQuadtree-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: joinAQuadtrees
    > ### Title: Join two AQuadtree objects from the same area, to compare their
    > ###   data
    > ### Aliases: joinAQuadtrees
    > 
    > ### ** Examples
    > 
    ...
     1. └─AQuadtree::joinAQuadtrees(CharlestonPop.AQT_1, CharlestonPop.AQT_2)
     2.   ├─dplyr::summarise_at(qt2.act@data, sum.2, funs(sum))
     3.   │ └─dplyr:::manip_at(...)
     4.   │   └─dplyr:::as_fun_list(...)
     5.   │     └─dplyr:::is_fun_list(.funs)
     6.   └─dplyr::funs(sum)
     7.     └─lifecycle::deprecate_stop(...)
     8.       └─lifecycle:::deprecate_stop0(msg)
     9.         └─rlang::cnd_signal(...)
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        data   3.8Mb
    ```

# blsR

<details>

* Version: 0.4.0
* GitHub: https://github.com/groditi/blsR
* Source code: https://github.com/cran/blsR
* Date/Publication: 2022-10-13 19:22:32 UTC
* Number of recursive dependencies: 55

Run `cloud_details(, "blsR")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in documentation object 'data_as_table.Rd':
      ‘[dplyr:bind]{dplyr::bind_rows()}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

# bootnet

<details>

* Version: 1.5
* GitHub: NA
* Source code: https://github.com/cran/bootnet
* Date/Publication: 2021-10-25 16:20:09 UTC
* Number of recursive dependencies: 170

Run `cloud_details(, "bootnet")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘bootnet-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: netSimulator
    > ### Title: Network Estimation Performance
    > ### Aliases: netSimulator replicationSimulator
    > 
    > ### ** Examples
    > 
    > # 5-node GGM chain graph:
    ...
      8. ├─base::as.data.frame(.)
      9. ├─dplyr::arrange_(., ~nCases)
     10. ├─dplyr::summarize_each(., funs(fun(., digits = digits)))
     11. │ └─dplyr::summarise_each_(tbl, funs, enquos(...))
     12. │   └─rlang::is_character(funs)
     13. └─dplyr::funs(fun(., digits = digits))
     14.   └─lifecycle::deprecate_stop(...)
     15.     └─lifecycle:::deprecate_stop0(msg)
     16.       └─rlang::cnd_signal(...)
    Execution halted
    ```

# caffsim

<details>

* Version: 0.2.2
* GitHub: https://github.com/asancpt/caffsim
* Source code: https://github.com/cran/caffsim
* Date/Publication: 2017-08-28 03:36:21 UTC
* Number of recursive dependencies: 60

Run `cloud_details(, "caffsim")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘caffsim-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: caffDescstat
    > ### Title: Calculate descriptive statistics of simulated PK parameters
    > ### Aliases: caffDescstat
    > 
    > ### ** Examples
    > 
    > caffDescstat(caffPkparam(20,500))
    ...
      4. ├─dplyr::mutate(...)
      5. ├─dplyr::summarise_at(., vars(value), funs(mean, sd, min, max))
      6. │ └─dplyr:::manip_at(...)
      7. │   └─dplyr:::as_fun_list(...)
      8. │     └─dplyr:::is_fun_list(.funs)
      9. └─dplyr::funs(mean, sd, min, max)
     10.   └─lifecycle::deprecate_stop(...)
     11.     └─lifecycle:::deprecate_stop0(msg)
     12.       └─rlang::cnd_signal(...)
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘markdown’
      All declared Imports should be used.
    ```

# carpenter

<details>

* Version: 0.2.2
* GitHub: https://github.com/lwjohnst86/carpenter
* Source code: https://github.com/cran/carpenter
* Date/Publication: 2019-02-05 08:43:30 UTC
* Number of recursive dependencies: 65

Run `cloud_details(, "carpenter")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘carpenter-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: carpenter
    > ### Title: Build common tables for your research needs!
    > ### Aliases: carpenter
    > 
    > ### ** Examples
    > 
    > 
    ...
      8. │       └─... %>% dplyr::mutate_all(dplyr::funs(as.character))
      9. ├─dplyr::mutate_all(., dplyr::funs(as.character))
     10. │ └─dplyr:::manip_all(...)
     11. │   └─dplyr:::as_fun_list(...)
     12. │     └─dplyr:::is_fun_list(.funs)
     13. └─dplyr::funs(as.character)
     14.   └─lifecycle::deprecate_stop(...)
     15.     └─lifecycle:::deprecate_stop0(msg)
     16.       └─rlang::cnd_signal(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        5. │   └─carpenter (local) FUN(X[[i]], ...)
        6. │     └─carpenter (local) row_create(...)
        7. │       └─... %>% dplyr::mutate_all(dplyr::funs(as.character))
        8. ├─dplyr::mutate_all(., dplyr::funs(as.character))
        9. │ └─dplyr:::manip_all(...)
       10. │   └─dplyr:::as_fun_list(...)
       11. │     └─dplyr:::is_fun_list(.funs)
       12. └─dplyr::funs(as.character)
       13.   └─lifecycle::deprecate_stop(...)
       14.     └─lifecycle:::deprecate_stop0(msg)
       15.       └─rlang::cnd_signal(...)
      
      [ FAIL 1 | WARN 4 | SKIP 0 | PASS 20 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘carpenter.Rmd’ using rmarkdown
    Quitting from lines 87-89 (carpenter.Rmd) 
    Error: processing vignette 'carpenter.Rmd' failed with diagnostics:
    `funs()` was deprecated in dplyr 0.8.0 and is now defunct.
    Please use a list of either functions or lambdas:
    
      # Simple named list:
      list(mean = mean, median = median)
    ...
    
      # Using lambdas
      list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
    --- failed re-building ‘carpenter.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘carpenter.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
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

# clustrd

<details>

* Version: 1.4.0
* GitHub: NA
* Source code: https://github.com/cran/clustrd
* Date/Publication: 2022-07-16 23:20:06 UTC
* Number of recursive dependencies: 62

Run `cloud_details(, "clustrd")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘clustrd-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: local_bootclus
    > ### Title: Cluster-wise stability assessment of Joint Dimension Reduction
    > ###   and Clustering methods by bootstrapping.
    > ### Aliases: local_bootclus
    > 
    > ### ** Examples
    > 
    ...
      4. │     └─x1 %>% group_by(clu) %>% summarise_all(funs(mean))
      5. ├─dplyr::summarise_all(., funs(mean))
      6. │ └─dplyr:::manip_all(...)
      7. │   └─dplyr:::as_fun_list(...)
      8. │     └─dplyr:::is_fun_list(.funs)
      9. └─dplyr::funs(mean)
     10.   └─lifecycle::deprecate_stop(...)
     11.     └─lifecycle:::deprecate_stop0(msg)
     12.       └─rlang::cnd_signal(...)
    Execution halted
    ```

# coefplot

<details>

* Version: 1.2.8
* GitHub: NA
* Source code: https://github.com/cran/coefplot
* Date/Publication: 2022-01-14 09:42:47 UTC
* Number of recursive dependencies: 109

Run `cloud_details(, "coefplot")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘coefplot-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: coefplot
    > ### Title: Plotting Model Coefficients
    > ### Aliases: coefplot coefplot-package plotcoef coefplot.default
    > ###   coefplot.lm coefplot.glm coefplot.workflow coefplot.model_fit
    > ###   coefplot.rxGlm coefplot.rxLinMod coefplot.rxLogit
    > ### Keywords: coefficient coefficients coefplot dotplot glm linear lm model
    > ###   rxLinMod
    ...
      5.     └─coefplot::buildModelCI.default(...)
      6.       ├─dplyr::mutate_at(...)
      7.       │ └─dplyr:::manip_at(...)
      8.       │   └─dplyr:::as_fun_list(...)
      9.       │     └─dplyr:::is_fun_list(.funs)
     10.       └─dplyr::funs(trans)
     11.         └─lifecycle::deprecate_stop(...)
     12.           └─lifecycle:::deprecate_stop0(msg)
     13.             └─rlang::cnd_signal(...)
    Execution halted
    ```

# cometExactTest

<details>

* Version: 0.1.5
* GitHub: NA
* Source code: https://github.com/cran/cometExactTest
* Date/Publication: 2018-01-29 19:30:12 UTC
* Number of recursive dependencies: 16

Run `cloud_details(, "cometExactTest")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘cometExactTest-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: comet_exact_test
    > ### Title: CoMEt exact test for 2^k contingency tables
    > ### Aliases: comet_exact_test
    > 
    > ### ** Examples
    > 
    >   comet_exact_test(c(33, 10, 10, 1, 10, 0, 0, 1))  # 2^3 test => 0.02303503
    ...
      3. │   └─tmp_alts %>% summarise_all(funs(sum))
      4. ├─dplyr::summarise_all(., funs(sum))
      5. │ └─dplyr:::manip_all(...)
      6. │   └─dplyr:::as_fun_list(...)
      7. │     └─dplyr:::is_fun_list(.funs)
      8. └─dplyr::funs(sum)
      9.   └─lifecycle::deprecate_stop(...)
     10.     └─lifecycle:::deprecate_stop0(msg)
     11.       └─rlang::cnd_signal(...)
    Execution halted
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
    New names:
    ...
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
    Appending disk.frames: 
    Error in anti_join(.x, .y, by = by, copy = copy, ..., overwrite = overwrite) : 
      Arguments in `...` must be used.
    ✖ Problematic arguments:
    • ..1 = xch
    • ..2 = ych
    • overwrite = overwrite
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

*   checking examples ... ERROR
    ```
    Running examples in ‘dm-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: dplyr_join
    > ### Title: 'dplyr' join methods for zoomed dm objects
    > ### Aliases: dplyr_join left_join.dm_zoomed left_join.dm_keyed_tbl
    > ###   inner_join.dm_zoomed inner_join.dm_keyed_tbl full_join.dm_zoomed
    > ###   full_join.dm_keyed_tbl right_join.dm_zoomed right_join.dm_keyed_tbl
    > ###   semi_join.dm_zoomed semi_join.dm_keyed_tbl anti_join.dm_zoomed
    > ###   anti_join.dm_keyed_tbl nest_join.dm_zoomed
    ...
      4. │   └─base::eval(ei, envir)
      5. │     └─base::eval(ei, envir)
      6. ├─dm_zoom_to(flights_dm, flights) %>% ...
      7. └─dplyr::left_join(., airports, select = c(faa, name))
      8.   └─rlang (local) `<fn>`()
      9.     └─rlang:::check_dots(env, error, action, call)
     10.       └─rlang:::action_dots(...)
     11.         ├─base (local) try_dots(...)
     12.         └─rlang (local) action(...)
    Execution halted
    ```

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
      
      [ FAIL 6 | WARN 15 | SKIP 189 | PASS 1307 ]
      Error: Test failures
      Execution halted
    ```

# dodgr

<details>

* Version: 0.2.17
* GitHub: https://github.com/ATFutures/dodgr
* Source code: https://github.com/cran/dodgr
* Date/Publication: 2022-11-04 17:40:06 UTC
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
        doc    6.4Mb
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
       8.   └─dplyr:::check_slice_dots(..., n = n, prop = prop)
       9.     └─rlang::abort(bullets, call = error_call)
      
      [ FAIL 1 | WARN 0 | SKIP 25 | PASS 603 ]
      Error: Test failures
      Execution halted
    ```

# dtrackr

<details>

* Version: 0.2.4
* GitHub: NA
* Source code: https://github.com/cran/dtrackr
* Date/Publication: 2022-07-05 21:00:09 UTC
* Number of recursive dependencies: 153

Run `cloud_details(, "dtrackr")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in documentation object 'bind_rows.Rd':
      ‘[dplyr:bind]{dplyr::bind_rows()}’
    
    Missing link or links in documentation object 'p_bind_rows.Rd':
      ‘[dplyr:bind]{dplyr::bind_rows()}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

# eHDPrep

<details>

* Version: 1.2.1
* GitHub: NA
* Source code: https://github.com/cran/eHDPrep
* Date/Publication: 2022-09-07 07:50:14 UTC
* Number of recursive dependencies: 120

Run `cloud_details(, "eHDPrep")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘eHDPrep-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: identify_inconsistency
    > ### Title: Identify inconsistencies in a dataset
    > ### Aliases: identify_inconsistency
    > 
    > ### ** Examples
    > 
    > require(tibble)
    ...
     13.   └─vctrs::vec_default_cast(...)
     14.     ├─base::withRestarts(...)
     15.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     16.     │   └─base (local) doWithOneRestart(return(expr), restart)
     17.     └─vctrs::stop_incompatible_cast(...)
     18.       └─vctrs::stop_incompatible_type(...)
     19.         └─vctrs:::stop_incompatible(...)
     20.           └─vctrs:::stop_vctrs(...)
     21.             └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 7 marked UTF-8 strings
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

# ffp

<details>

* Version: 0.2.2
* GitHub: https://github.com/Reckziegel/FFP
* Source code: https://github.com/cran/ffp
* Date/Publication: 2022-09-29 15:10:06 UTC
* Number of recursive dependencies: 107

Run `cloud_details(, "ffp")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in documentation object 'bind_probs.Rd':
      ‘[dplyr]{bind}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
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
        ▆
     1. ├─forceR::find_strongest_peaks(df = df.all.200.tax_filtered, no.of.peaks = 5)
     2. │ └─curr.plot.window %>% slice(n = 1) %>% pull(specimen)
     3. ├─dplyr::pull(., specimen)
     4. └─dplyr::slice(., n = 1)
     5.   └─rlang::check_dots_unnamed()
     6.     └─rlang:::action_dots(...)
     7.       ├─base (local) try_dots(...)
     8.       └─rlang (local) action(...)
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘forceR.Rmd’ using rmarkdown
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    ...
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
      
      [ FAIL 2 | WARN 7 | SKIP 0 | PASS 203 ]
      Error: Test failures
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
      installed size is 77.9Mb
      sub-directories of 1Mb or more:
        data   5.0Mb
        doc    1.6Mb
        libs  70.3Mb
    ```

# funModeling

<details>

* Version: 1.9.4
* GitHub: https://github.com/pablo14/funModeling
* Source code: https://github.com/cran/funModeling
* Date/Publication: 2020-06-15 05:10:02 UTC
* Number of recursive dependencies: 86

Run `cloud_details(, "funModeling")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘funModeling-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: desc_groups
    > ### Title: Profiling categorical variable
    > ### Aliases: desc_groups
    > 
    > ### ** Examples
    > 
    > # default grouping function: mean
    ...
      9. │     ├─dplyr:::new_sel_vars(tbl_vars_dispatch(x), group_vars(x))
     10. │     │ └─base::structure(...)
     11. │     └─dplyr:::tbl_vars_dispatch(x)
     12. ├─dplyr::summarise_each_(., funs(group_func), vars_to_keep)
     13. │ └─rlang::is_character(funs)
     14. └─dplyr::funs(group_func)
     15.   └─lifecycle::deprecate_stop(...)
     16.     └─lifecycle:::deprecate_stop0(msg)
     17.       └─rlang::cnd_signal(...)
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘funModeling_quickstart.Rmd’ using rmarkdown
    Loading required package: Hmisc
    Loading required package: lattice
    Loading required package: survival
    Loading required package: Formula
    Loading required package: ggplot2
    
    Attaching package: 'Hmisc'
    
    ...
    
      # Using lambdas
      list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
    --- failed re-building ‘funModeling_quickstart.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘funModeling_quickstart.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
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

# geosimilarity

<details>

* Version: 2.2
* GitHub: NA
* Source code: https://github.com/cran/geosimilarity
* Date/Publication: 2022-11-08 16:00:02 UTC
* Number of recursive dependencies: 85

Run `cloud_details(, "geosimilarity")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘geosimilarity-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: bestkappa
    > ### Title: Function for the best kappa parameter
    > ### Aliases: bestkappa
    > 
    > ### ** Examples
    > 
    > data("zn")
    ...
      4. ├─dplyr::summarise_all(., funs(mean))
      5. │ └─dplyr:::manip_all(...)
      6. │   └─dplyr:::as_fun_list(...)
      7. │     └─dplyr:::is_fun_list(.funs)
      8. └─dplyr::funs(mean)
      9.   └─lifecycle::deprecate_stop(...)
     10.     └─lifecycle:::deprecate_stop0(msg)
     11.       └─rlang::cnd_signal(...)
    Timing stopped at: 1.746 0.015 1.761
    Execution halted
    ```

# getLattes

<details>

* Version: 0.2.0
* GitHub: https://github.com/roneyfraga/getLattes
* Source code: https://github.com/cran/getLattes
* Date/Publication: 2021-06-11 08:40:05 UTC
* Number of recursive dependencies: 47

Run `cloud_details(, "getLattes")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in documentation object 'getAreasAtuacao.Rd':
      ‘[dplyr]{bind}’
    
    Missing link or links in documentation object 'getArtigosPublicados.Rd':
      ‘[dplyr]{bind}’
    
    Missing link or links in documentation object 'getAtuacoesProfissionais.Rd':
      ‘[dplyr]{bind}’
    
    Missing link or links in documentation object 'getBancasDoutorado.Rd':
    ...
    Missing link or links in documentation object 'getParticipacaoProjeto.Rd':
      ‘[dplyr]{bind}’
    
    Missing link or links in documentation object 'getPatentes.Rd':
      ‘[dplyr]{bind}’
    
    Missing link or links in documentation object 'getProducaoTecnica.Rd':
      ‘[dplyr]{bind}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘pipeR’ ‘rlang’
      All declared Imports should be used.
    ```

# GFE

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/GFE
* Date/Publication: 2018-08-02 12:10:10 UTC
* Number of recursive dependencies: 18

Run `cloud_details(, "GFE")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘GFE-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: reSamGF
    > ### Title: Gross flows variance estimation.
    > ### Aliases: reSamGF
    > 
    > ### ** Examples
    > 
    > library(TeachingSampling)
    ...
      5. ├─dplyr::summarize_all(...)
      6. │ └─dplyr:::manip_all(...)
      7. │   └─dplyr:::as_fun_list(...)
      8. │     └─dplyr:::is_fun_list(.funs)
      9. ├─base::ifelse(type == "Jackknife", funs(varJack), funs(var))
     10. └─dplyr::funs(var)
     11.   └─lifecycle::deprecate_stop(...)
     12.     └─lifecycle:::deprecate_stop0(msg)
     13.       └─rlang::cnd_signal(...)
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
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
    --- re-building ‘ggblanket.Rmd’ using rmarkdown
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    ...
    ! Can't compute column `Species`.
    Caused by error in `dplyr::na_if()`:
    ! Can't convert `y` <double> to match type of `x` <character>.
    --- failed re-building ‘ggblanket.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘ggblanket.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# glmdisc

<details>

* Version: 0.6
* GitHub: https://github.com/adimajo/glmdisc
* Source code: https://github.com/cran/glmdisc
* Date/Publication: 2020-09-30 21:10:02 UTC
* Number of recursive dependencies: 117

Run `cloud_details(, "glmdisc")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Backtrace:
          ▆
       1. ├─data.frame(xd[, 1:2]) %>% ... at test-discretize-link.R:88:2
       2. ├─dplyr::mutate_at(., dplyr::vars("X1", "X2"), dplyr::funs(as.integer))
       3. │ └─dplyr:::manip_at(...)
       4. │   └─dplyr:::as_fun_list(...)
       5. │     └─dplyr:::is_fun_list(.funs)
       6. └─dplyr::funs(as.integer)
       7.   └─lifecycle::deprecate_stop(...)
       8.     └─lifecycle:::deprecate_stop0(msg)
       9.       └─rlang::cnd_signal(...)
      
      [ FAIL 1 | WARN 72 | SKIP 3 | PASS 795 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        libs   4.4Mb
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# graphicalVAR

<details>

* Version: 0.3
* GitHub: NA
* Source code: https://github.com/cran/graphicalVAR
* Date/Publication: 2021-10-19 19:40:02 UTC
* Number of recursive dependencies: 87

Run `cloud_details(, "graphicalVAR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘graphicalVAR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: graphicalVAR
    > ### Title: Estimate the graphical VAR model.
    > ### Aliases: graphicalVAR
    > 
    > ### ** Examples
    > 
    > # Simulate model:
    ...
      8. ├─dplyr::ungroup(.)
      9. ├─dplyr::mutate_at(., funs(shift), .vars = vars)
     10. │ └─dplyr:::manip_at(...)
     11. │   └─dplyr:::as_fun_list(...)
     12. │     └─dplyr:::is_fun_list(.funs)
     13. └─dplyr::funs(shift)
     14.   └─lifecycle::deprecate_stop(...)
     15.     └─lifecycle:::deprecate_stop0(msg)
     16.       └─rlang::cnd_signal(...)
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

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘convert.Rmd’ using rmarkdown
    Quitting from lines 104-109 (convert.Rmd) 
    Error: processing vignette 'convert.Rmd' failed with diagnostics:
    `funs()` was deprecated in dplyr 0.8.0 and is now defunct.
    Please use a list of either functions or lambdas:
    
      # Simple named list:
      list(mean = mean, median = median)
    
    ...
    Caused by warning in `max()`:
    ! no non-missing arguments to max; returning -Inf
    ℹ Run `dplyr::last_dplyr_warnings()` to see the 31 remaining warnings.
    --- finished re-building ‘s.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘convert.Rmd’
    
    Error: Vignette re-building failed.
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
    --- re-building ‘allele-haplotype.Rmd’ using rmarkdown
    ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ✔ ggplot2 3.4.0           ✔ dplyr   1.0.99.9000
    ✔ tibble  3.1.8           ✔ stringr 1.4.1      
    ✔ tidyr   1.2.1           ✔ forcats 0.5.2      
    ✔ purrr   0.3.5           
    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    ...
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

# IBCF.MTME

<details>

* Version: 1.6-0
* GitHub: https://github.com/frahik/IBCF.MTME
* Source code: https://github.com/cran/IBCF.MTME
* Date/Publication: 2019-03-23 05:18:07 UTC
* Number of recursive dependencies: 65

Run `cloud_details(, "IBCF.MTME")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        5. ├─IBCF.MTME:::summary.IBCFY(pm)
        6. │ └─... %>% as.data.frame()
        7. ├─base::as.data.frame(.)
        8. ├─dplyr::mutate_if(., is.numeric, funs(round(., digits)))
        9. │ └─dplyr:::manip_if(...)
       10. │   └─dplyr:::as_fun_list(...)
       11. │     └─dplyr:::is_fun_list(.funs)
       12. └─dplyr::funs(round(., digits))
       13.   └─lifecycle::deprecate_stop(...)
       14.     └─lifecycle:::deprecate_stop0(msg)
       15.       └─rlang::cnd_signal(...)
      
      [ FAIL 8 | WARN 0 | SKIP 0 | PASS 108 ]
      Error: Test failures
      Execution halted
    ```

# interplot

<details>

* Version: 0.2.3
* GitHub: https://github.com/sammo3182/interplot
* Source code: https://github.com/cran/interplot
* Date/Publication: 2021-02-18 06:20:07 UTC
* Number of recursive dependencies: 80

Run `cloud_details(, "interplot")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘interplot-vignette.Rmd’ using rmarkdown
    Loading required package: ggplot2
    Loading required package: abind
    Loading required package: arm
    Loading required package: MASS
    Loading required package: Matrix
    Loading required package: lme4
    
    arm (Version 1.13-1, built: 2022-8-25)
    ...
    
      # Using lambdas
      list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
    --- failed re-building ‘interplot-vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘interplot-vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
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
      x Problematic argument:
      * suffix = c(".x", ".y")
      Backtrace:
          ▆
       1. ├─d1 %>% ...
       2. └─dplyr::anti_join(., d3, by = c(x1 = "x1", x2 = "x6"), suffix = c(".x", ".y"))
       3.   └─rlang (local) `<fn>`()
       4.     └─rlang:::check_dots(env, error, action, call)
       5.       └─rlang:::action_dots(...)
       6.         ├─base (local) try_dots(...)
       7.         └─rlang (local) action(...)
      
      [ FAIL 2 | WARN 0 | SKIP 5 | PASS 331 ]
      Error: Test failures
      Execution halted
    ```

# ipft

<details>

* Version: 0.7.2
* GitHub: NA
* Source code: https://github.com/cran/ipft
* Date/Publication: 2018-01-04 09:36:52 UTC
* Number of recursive dependencies: 34

Run `cloud_details(, "ipft")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ipft-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ipfPlotLocation
    > ### Title: Plots the spatial location of the observations
    > ### Aliases: ipfPlotLocation
    > 
    > ### ** Examples
    > 
    > 
    ...
      4. ├─dplyr::arrange(., GROUP)
      5. ├─dplyr::summarise_all(., funs(mean))
      6. │ └─dplyr:::manip_all(...)
      7. │   └─dplyr:::as_fun_list(...)
      8. │     └─dplyr:::is_fun_list(.funs)
      9. └─dplyr::funs(mean)
     10.   └─lifecycle::deprecate_stop(...)
     11.     └─lifecycle:::deprecate_stop0(msg)
     12.       └─rlang::cnd_signal(...)
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

# jpgrid

<details>

* Version: 0.2.0
* GitHub: https://github.com/UchidaMizuki/jpgrid
* Source code: https://github.com/cran/jpgrid
* Date/Publication: 2022-05-03 10:20:02 UTC
* Number of recursive dependencies: 57

Run `cloud_details(, "jpgrid")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      * drop = FALSE
      Backtrace:
          ▆
       1. ├─... %>% grid_as_stars() at test-stars.R:2:2
       2. ├─jpgrid::grid_as_stars(.)
       3. │ └─... %>% ...
       4. └─dplyr::slice(., "Y", 1L:(dim_x[["Y"]] - 1L), drop = FALSE)
       5.   └─rlang::check_dots_unnamed()
       6.     └─rlang:::action_dots(...)
       7.       ├─base (local) try_dots(...)
       8.       └─rlang (local) action(...)
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 90 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 461377 marked UTF-8 strings
    ```

# jstable

<details>

* Version: 1.0.7
* GitHub: https://github.com/jinseob2kim/jstable
* Source code: https://github.com/cran/jstable
* Date/Publication: 2021-10-19 13:00:02 UTC
* Number of recursive dependencies: 106

Run `cloud_details(, "jstable")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in documentation object 'TableSubgroupMultiCox.Rd':
      ‘[dplyr]{bind}’
    
    Missing link or links in documentation object 'TableSubgroupMultiGLM.Rd':
      ‘[dplyr]{bind}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
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

# Lahman

<details>

* Version: 10.0-1
* GitHub: https://github.com/cdalzell/Lahman
* Source code: https://github.com/cran/Lahman
* Date/Publication: 2022-04-26 08:20:10 UTC
* Number of recursive dependencies: 94

Run `cloud_details(, "Lahman")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘Lahman-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: Fielding
    > ### Title: Fielding table
    > ### Aliases: Fielding
    > ### Keywords: datasets
    > 
    > ### ** Examples
    > 
    ...
        ▆
     1. ├─... %>% summarise_each(funs(sum))
     2. ├─dplyr::summarise_each(., funs(sum))
     3. │ └─dplyr::summarise_each_(tbl, funs, enquos(...))
     4. │   └─rlang::is_character(funs)
     5. └─dplyr::funs(sum)
     6.   └─lifecycle::deprecate_stop(...)
     7.     └─lifecycle:::deprecate_stop0(msg)
     8.       └─rlang::cnd_signal(...)
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘hits-by-type.Rmd’ using rmarkdown
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    ...
    --- failed re-building ‘strikeoutsandhr.Rmd’
    
    --- re-building ‘vignette-intro.Rmd’ using rmarkdown
    --- finished re-building ‘vignette-intro.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘hits-by-type.Rmd’ ‘strikeoutsandhr.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.4Mb
      sub-directories of 1Mb or more:
        data   5.5Mb
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

# mark

<details>

* Version: 0.5.3
* GitHub: https://github.com/jmbarbone/mark
* Source code: https://github.com/cran/mark
* Date/Publication: 2022-10-16 13:20:02 UTC
* Number of recursive dependencies: 74

Run `cloud_details(, "mark")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in documentation object 'row_bind.Rd':
      ‘[dplyr:bind]{dplyr::bind_rows()}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
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
    --- re-building ‘MetAlyzer_User_Guide.Rmd’ using rmarkdown
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    ...
    ℹ In group 1: `Method = 1`, `Tissue = Drosophila`, `Metabolite = C0`.
    Caused by error in `contrasts<-`:
    ! contrasts can be applied only to factors with 2 or more levels
    --- failed re-building ‘MetAlyzer_User_Guide.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘MetAlyzer_User_Guide.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# missCompare

<details>

* Version: 1.0.3
* GitHub: https://github.com/Tirgit/missCompare
* Source code: https://github.com/cran/missCompare
* Date/Publication: 2020-12-01 08:50:03 UTC
* Number of recursive dependencies: 197

Run `cloud_details(, "missCompare")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        7. │   └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
        8. ├─missCompare::impute_simulated(...)
        9. │ └─collect_res %>% group_by(Method) %>% ...
       10. ├─dplyr::summarise_all(., funs(mymean))
       11. │ └─dplyr:::manip_all(...)
       12. │   └─dplyr:::as_fun_list(...)
       13. │     └─dplyr:::is_fun_list(.funs)
       14. └─dplyr::funs(mymean)
       15.   └─lifecycle::deprecate_stop(...)
       16.     └─lifecycle:::deprecate_stop0(msg)
       17.       └─rlang::cnd_signal(...)
      
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 35 ]
      Error: Test failures
      Execution halted
    ```

# MRFcov

<details>

* Version: 1.0.38
* GitHub: https://github.com/nicholasjclark/MRFcov
* Source code: https://github.com/cran/MRFcov
* Date/Publication: 2021-03-18 06:40:03 UTC
* Number of recursive dependencies: 112

Run `cloud_details(, "MRFcov")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘MRFcov-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: MRFcov
    > ### Title: Markov Random Fields with covariates
    > ### Aliases: MRFcov
    > 
    > ### ** Examples
    > 
    > data("Bird.parasites")
    ...
      4. │ └─data.frame(mrf_data) %>% ...
      5. ├─dplyr::summarise_all(., dplyr::funs(sd(.)))
      6. │ └─dplyr:::manip_all(...)
      7. │   └─dplyr:::as_fun_list(...)
      8. │     └─dplyr:::is_fun_list(.funs)
      9. └─dplyr::funs(sd(.))
     10.   └─lifecycle::deprecate_stop(...)
     11.     └─lifecycle:::deprecate_stop0(msg)
     12.       └─rlang::cnd_signal(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        3. │   ├─base::as.vector(t(data.frame(mrf_data) %>% dplyr::summarise_all(dplyr::funs(sd(.)))))
        4. │   ├─base::t(data.frame(mrf_data) %>% dplyr::summarise_all(dplyr::funs(sd(.))))
        5. │   └─data.frame(mrf_data) %>% ...
        6. ├─dplyr::summarise_all(., dplyr::funs(sd(.)))
        7. │ └─dplyr:::manip_all(...)
        8. │   └─dplyr:::as_fun_list(...)
        9. │     └─dplyr:::is_fun_list(.funs)
       10. └─dplyr::funs(sd(.))
       11.   └─lifecycle::deprecate_stop(...)
       12.     └─lifecycle:::deprecate_stop0(msg)
       13.       └─rlang::cnd_signal(...)
      
      [ FAIL 4 | WARN 0 | SKIP 0 | PASS 10 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Bird_Parasite_CRF.Rmd’ using rmarkdown
    Loading required package: glmnet
    Loading required package: Matrix
    Loaded glmnet 4.1-6
    Welcome to MRFcov. If you use the package, please cite as: Clark, NJ, Wells, K and Lindberg, O. 2018. Unravelling changing interspecific interactions across environmental gradients using Markov random fields. Ecology doi: 10.1002/ecy.2221 
    Quitting from lines 39-40 (Bird_Parasite_CRF.Rmd) 
    Error: processing vignette 'Bird_Parasite_CRF.Rmd' failed with diagnostics:
    `funs()` was deprecated in dplyr 0.8.0 and is now defunct.
    Please use a list of either functions or lambdas:
    ...
    
      # Using lambdas
      list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
    --- failed re-building ‘Gaussian_Poisson_CRFs.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘Bird_Parasite_CRF.Rmd’ ‘Gaussian_Poisson_CRFs.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# mrgsim.parallel

<details>

* Version: 0.2.1
* GitHub: https://github.com/kylebaron/mrgsim.parallel
* Source code: https://github.com/cran/mrgsim.parallel
* Date/Publication: 2022-03-17 19:50:05 UTC
* Number of recursive dependencies: 73

Run `cloud_details(, "mrgsim.parallel")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in documentation object 'bg_mrgsim_d.Rd':
      ‘[dplyr:bind]{dplyr::bind_rows()}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

# MSiP

<details>

* Version: 1.3.7
* GitHub: NA
* Source code: https://github.com/cran/MSiP
* Date/Publication: 2021-06-17 08:20:05 UTC
* Number of recursive dependencies: 88

Run `cloud_details(, "MSiP")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘MSiP-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: Weighted.matrixModel
    > ### Title: Weighted.matrixModel
    > ### Aliases: Weighted.matrixModel
    > 
    > ### ** Examples
    > 
    > data(SampleDatInput)
    ...
      5. │ └─base::is.data.frame(x)
      6. ├─base::as.data.frame(.)
      7. ├─dplyr::summarise_each(., funs(max(.)))
      8. │ └─dplyr::summarise_each_(tbl, funs, enquos(...))
      9. │   └─rlang::is_character(funs)
     10. └─dplyr::funs(max(.))
     11.   └─lifecycle::deprecate_stop(...)
     12.     └─lifecycle:::deprecate_stop0(msg)
     13.       └─rlang::cnd_signal(...)
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘MSiP_tutorial.Rmd’ using knitr
    Quitting from lines 46-49 (MSiP_tutorial.Rmd) 
    Error: processing vignette 'MSiP_tutorial.Rmd' failed with diagnostics:
    `funs()` was deprecated in dplyr 0.8.0 and is now defunct.
    Please use a list of either functions or lambdas:
    
      # Simple named list:
      list(mean = mean, median = median)
    ...
    
      # Using lambdas
      list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
    --- failed re-building ‘MSiP_tutorial.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘MSiP_tutorial.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘ranger’
      All declared Imports should be used.
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
    --- re-building ‘exploring-imputed-values.Rmd’ using rmarkdown
    Quitting from lines 125-129 (exploring-imputed-values.Rmd) 
    Error: processing vignette 'exploring-imputed-values.Rmd' failed with diagnostics:
    `funs()` was deprecated in dplyr 0.8.0 and is now defunct.
    Please use a list of either functions or lambdas:
    
      # Simple named list:
      list(mean = mean, median = median)
    
    ...
    --- failed re-building ‘replace-with-na.Rmd’
    
    --- re-building ‘special-missing-values.Rmd’ using rmarkdown
    --- finished re-building ‘special-missing-values.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘exploring-imputed-values.Rmd’ ‘replace-with-na.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# ncappc

<details>

* Version: 0.3.0
* GitHub: NA
* Source code: https://github.com/cran/ncappc
* Date/Publication: 2018-08-24 20:30:03 UTC
* Number of recursive dependencies: 91

Run `cloud_details(, "ncappc")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Backtrace:
          ▆
       1. ├─out$ncaOutput %>% tibble::as_tibble() %>% ... at test-ncappc.R:404:2
       2. ├─dplyr::mutate_all(., dplyr::funs(as.numeric(as.character(.))))
       3. │ └─dplyr:::manip_all(...)
       4. │   └─dplyr:::as_fun_list(...)
       5. │     └─dplyr:::is_fun_list(.funs)
       6. └─dplyr::funs(as.numeric(as.character(.)))
       7.   └─lifecycle::deprecate_stop(...)
       8.     └─lifecycle:::deprecate_stop0(msg)
       9.       └─rlang::cnd_signal(...)
      
      [ FAIL 6 | WARN 1 | SKIP 0 | PASS 5 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘bookdown’
      All declared Imports should be used.
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
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

# openalexR

<details>

* Version: 1.0.0
* GitHub: https://github.com/massimoaria/openalexR
* Source code: https://github.com/cran/openalexR
* Date/Publication: 2022-10-06 10:40:02 UTC
* Number of recursive dependencies: 78

Run `cloud_details(, "openalexR")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘A_Brief_Introduction_to_openalexR.Rmd’ using rmarkdown
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    ...
    Quitting from lines 207-213 (A_Brief_Introduction_to_openalexR.Rmd) 
    Error: processing vignette 'A_Brief_Introduction_to_openalexR.Rmd' failed with diagnostics:
    $ operator is invalid for atomic vectors
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
       1. └─overviewR::overview_overlap(...) at test-check_output.R:320:2
       2.   └─dplyr::full_join(...)
       3.     └─rlang (local) `<fn>`()
       4.       └─rlang:::check_dots(env, error, action, call)
       5.         └─rlang:::action_dots(...)
       6.           ├─base (local) try_dots(...)
       7.           └─rlang (local) action(...)
      
      [ FAIL 1 | WARN 1 | SKIP 4 | PASS 52 ]
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

# PAC

<details>

* Version: 1.1.4
* GitHub: NA
* Source code: https://github.com/cran/PAC
* Date/Publication: 2021-02-18 07:00:02 UTC
* Number of recursive dependencies: 58

Run `cloud_details(, "PAC")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘PAC-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: aggregateData
    > ### Title: Aggregates results from the clustering and merging step.
    > ### Aliases: aggregateData
    > 
    > ### ** Examples
    > 
    > n = 5e3                       # number of observations
    ...
      2. │ └─data %>% group_by(ClusterID, SampleID) %>% ...
      3. ├─dplyr::summarise_all(., funs(sum))
      4. │ └─dplyr:::manip_all(...)
      5. │   └─dplyr:::as_fun_list(...)
      6. │     └─dplyr:::is_fun_list(.funs)
      7. └─dplyr::funs(sum)
      8.   └─lifecycle::deprecate_stop(...)
      9.     └─lifecycle:::deprecate_stop0(msg)
     10.       └─rlang::cnd_signal(...)
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘intro.Rmd’ using rmarkdown
    Quitting from lines 42-43 (intro.Rmd) 
    Error: processing vignette 'intro.Rmd' failed with diagnostics:
    `funs()` was deprecated in dplyr 0.8.0 and is now defunct.
    Please use a list of either functions or lambdas:
    
      # Simple named list:
      list(mean = mean, median = median)
    ...
    
      # Using lambdas
      list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
    --- failed re-building ‘intro.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘intro.Rmd’
    
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

# PDtoolkit

<details>

* Version: 1.0.0
* GitHub: https://github.com/andrija-djurovic/PDtoolkit
* Source code: https://github.com/cran/PDtoolkit
* Date/Publication: 2022-11-16 12:11:37 UTC
* Number of recursive dependencies: 66

Run `cloud_details(, "PDtoolkit")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘PDtoolkit-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: univariate
    > ### Title: Univariate analysis
    > ### Aliases: univariate
    > 
    > ### ** Examples
    > 
    > suppressMessages(library(PDtoolkit))
    ...
      7. ├─dplyr::ungroup(.)
      8. ├─dplyr::summarise_at(...)
      9. │ └─dplyr:::manip_at(...)
     10. │   └─dplyr:::as_fun_list(...)
     11. │     └─dplyr:::is_fun_list(.funs)
     12. └─dplyr::funs(...)
     13.   └─lifecycle::deprecate_stop(...)
     14.     └─lifecycle:::deprecate_stop0(msg)
     15.       └─rlang::cnd_signal(...)
    Execution halted
    ```

# phase1PRMD

<details>

* Version: 1.0.2
* GitHub: NA
* Source code: https://github.com/cran/phase1PRMD
* Date/Publication: 2020-03-09 06:50:03 UTC
* Number of recursive dependencies: 78

Run `cloud_details(, "phase1PRMD")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘phase1PRMD-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: RunPRMD
    > ### Title: Implement a Multi-Stage Phase I Dose-Finding Design to recommend
    > ###   dosage selection based on the data collected in the available patient
    > ###   cohorts
    > ### Aliases: RunPRMD
    > 
    > ### ** Examples
    ...
      9. ├─dplyr::select(., PatID, starts_with("cycle"))
     10. ├─dplyr::mutate_at(...)
     11. │ └─dplyr:::manip_at(...)
     12. │   └─dplyr:::as_fun_list(...)
     13. │     └─dplyr:::is_fun_list(.funs)
     14. └─dplyr::funs(...)
     15.   └─lifecycle::deprecate_stop(...)
     16.     └─lifecycle:::deprecate_stop0(msg)
     17.       └─rlang::cnd_signal(...)
    Execution halted
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘phase1RMD’
    ```

# PHEindicatormethods

<details>

* Version: 1.4.1
* GitHub: https://github.com/PublicHealthEngland/PHEindicatormethods
* Source code: https://github.com/cran/PHEindicatormethods
* Date/Publication: 2022-08-08 11:40:17 UTC
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
      [ FAIL 1 | WARN 3 | SKIP 0 | PASS 452 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('testLifeExpectancy.R:315'): LE - warnings are generated when invalid arguments are used ──
      wideci_warning\[2\] does not match "some life expectancy values have a 95% confidence interval > 20 years; these values have been suppressed to NAs".
      Actual value: "Each row in `x` is expected to match at most 1 row in `y`\.\\nℹ Row 1 of `x` matches multiple rows\.\\nℹ If multiple matches are expected, set `multiple = "all"` to silence this\\n  warning\."
      Backtrace:
          ▆
       1. └─testthat::expect_match(wideci_warning[2], "some life expectancy values have a 95% confidence interval > 20 years; these values have been suppressed to NAs") at testLifeExpectancy.R:315:2
       2.   └─testthat:::expect_match_(...)
      
      [ FAIL 1 | WARN 3 | SKIP 0 | PASS 452 ]
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

# PPforest

<details>

* Version: 0.1.3
* GitHub: https://github.com/natydasilva/PPforest
* Source code: https://github.com/cran/PPforest
* Date/Publication: 2022-09-09 23:32:55 UTC
* Number of recursive dependencies: 86

Run `cloud_details(, "PPforest")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘PPforest-vignette.Rmd’ using rmarkdown
    Loading required package: PPforest
    Loading required package: dplyr
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    ...
    
      # Using lambdas
      list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
    --- failed re-building ‘PPforest-vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘PPforest-vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.4Mb
      sub-directories of 1Mb or more:
        libs   7.2Mb
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

# prevtoinc

<details>

* Version: 0.12.0
* GitHub: NA
* Source code: https://github.com/cran/prevtoinc
* Date/Publication: 2019-06-18 13:50:04 UTC
* Number of recursive dependencies: 70

Run `cloud_details(, "prevtoinc")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘prevtoinc-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: simulate_incidence_stats
    > ### Title: Calculate theoretical values like x.los, x.loi and other
    > ###   characteristics of the patient population
    > ### Aliases: simulate_incidence_stats
    > 
    > ### ** Examples
    > 
    ...
      2. │ └─... %>% dplyr::mutate_all(dplyr::funs(unlist))
      3. ├─dplyr::mutate_all(., dplyr::funs(unlist))
      4. │ └─dplyr:::manip_all(...)
      5. │   └─dplyr:::as_fun_list(...)
      6. │     └─dplyr:::is_fun_list(.funs)
      7. └─dplyr::funs(unlist)
      8.   └─lifecycle::deprecate_stop(...)
      9.     └─lifecycle:::deprecate_stop0(msg)
     10.       └─rlang::cnd_signal(...)
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘prevtoinc_vignette.Rmd’ using rmarkdown
    Loading required package: prevtoinc
    Quitting from lines 170-174 (prevtoinc_vignette.Rmd) 
    Error: processing vignette 'prevtoinc_vignette.Rmd' failed with diagnostics:
    `funs()` was deprecated in dplyr 0.8.0 and is now defunct.
    Please use a list of either functions or lambdas:
    
      # Simple named list:
    ...
    
      # Using lambdas
      list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
    --- failed re-building ‘prevtoinc_vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘prevtoinc_vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# prozor

<details>

* Version: 0.3.1
* GitHub: https://github.com/protviz/prozor
* Source code: https://github.com/cran/prozor
* Date/Publication: 2021-12-07 16:20:02 UTC
* Number of recursive dependencies: 62

Run `cloud_details(, "prozor")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘prozor-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: annotatePeptides
    > ### Title: Annotate peptides with protein ids
    > ### Aliases: annotatePeptides
    > 
    > ### ** Examples
    > 
    > 
    ...
     1. └─prozor::annotatePeptides(upeptide[seq_len(20)], resCan)
     2.   ├─dplyr::mutate_at(pepinfo, peptide, dplyr::funs(as.character))
     3.   │ └─dplyr:::manip_at(...)
     4.   │   └─dplyr:::as_fun_list(...)
     5.   │     └─dplyr:::is_fun_list(.funs)
     6.   └─dplyr::funs(as.character)
     7.     └─lifecycle::deprecate_stop(...)
     8.       └─lifecycle:::deprecate_stop0(msg)
     9.         └─rlang::cnd_signal(...)
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘CreatingDecoyDB.Rmd’ using rmarkdown
    --- finished re-building ‘CreatingDecoyDB.Rmd’
    
    --- re-building ‘PeptideAnnotationwithProzor.Rmd’ using rmarkdown
    Rows: 5000 Columns: 12
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: "\t"
    chr  (2): peptideSeq, peptideModSeq
    dbl (10): RefSpectraId, numPeaks, precursorCharge, precursorMZ, retentionTim...
    ...
    --- finished re-building ‘TargetDecoyFDR_Example.Rmd’
    
    --- re-building ‘cdsw.Rmd’ using rmarkdown
    --- finished re-building ‘cdsw.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘PeptideAnnotationwithProzor.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# psychonetrics

<details>

* Version: 0.10
* GitHub: https://github.com/SachaEpskamp/psychonetrics
* Source code: https://github.com/cran/psychonetrics
* Date/Publication: 2021-10-25 21:30:02 UTC
* Number of recursive dependencies: 139

Run `cloud_details(, "psychonetrics")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘psychonetrics-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: CIplot
    > ### Title: Plot Analytic Confidence Intervals
    > ### Aliases: CIplot
    > 
    > ### ** Examples
    > 
    > ### Example from ?ggm ###
    ...
      4. │     └─data %>% dplyr::group_by(.data[[idvar]]) %>% ...
      5. ├─dplyr::summarise_at(., funs(mean(., na.rm = TRUE)), .vars = vars)
      6. │ └─dplyr:::manip_at(...)
      7. │   └─dplyr:::as_fun_list(...)
      8. │     └─dplyr:::is_fun_list(.funs)
      9. └─dplyr::funs(mean(., na.rm = TRUE))
     10.   └─lifecycle::deprecate_stop(...)
     11.     └─lifecycle:::deprecate_stop0(msg)
     12.       └─rlang::cnd_signal(...)
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 101.0Mb
      sub-directories of 1Mb or more:
        libs  99.9Mb
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
# randomForestExplainer

<details>

* Version: 0.10.1
* GitHub: https://github.com/ModelOriented/randomForestExplainer
* Source code: https://github.com/cran/randomForestExplainer
* Date/Publication: 2020-07-11 20:30:02 UTC
* Number of recursive dependencies: 89

Run `cloud_details(, "randomForestExplainer")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘randomForestExplainer-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: min_depth_interactions
    > ### Title: Calculate mean conditional minimal depth
    > ### Aliases: min_depth_interactions
    > 
    > ### ** Examples
    > 
    > forest <- randomForest::randomForest(Species ~ ., data = iris, ntree = 100)
    ...
      5. ├─base::as.data.frame(.)
      6. ├─dplyr::summarize_at(., vars, funs(max(., na.rm = TRUE)))
      7. │ └─dplyr:::manip_at(...)
      8. │   └─dplyr:::as_fun_list(...)
      9. │     └─dplyr:::is_fun_list(.funs)
     10. └─dplyr::funs(max(., na.rm = TRUE))
     11.   └─lifecycle::deprecate_stop(...)
     12.     └─lifecycle:::deprecate_stop0(msg)
     13.       └─rlang::cnd_signal(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        4. │   └─randomForestExplainer:::min_depth_interactions_values_ranger(...)
        5. │     └─... %>% as.data.frame()
        6. ├─base::as.data.frame(.)
        7. ├─dplyr::summarize_at(., vars, funs(max(., na.rm = TRUE)))
        8. │ └─dplyr:::manip_at(...)
        9. │   └─dplyr:::as_fun_list(...)
       10. │     └─dplyr:::is_fun_list(.funs)
       11. └─dplyr::funs(max(., na.rm = TRUE))
       12.   └─lifecycle::deprecate_stop(...)
       13.     └─lifecycle:::deprecate_stop0(msg)
       14.       └─rlang::cnd_signal(...)
      
      [ FAIL 12 | WARN 5 | SKIP 0 | PASS 49 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
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
      
      [ FAIL 1 | WARN 31 | SKIP 401 | PASS 1891 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘fastICA’, ‘dimRed’
    ```

# RKorAPClient

<details>

* Version: 0.7.5
* GitHub: https://github.com/KorAP/RKorAPClient
* Source code: https://github.com/cran/RKorAPClient
* Date/Publication: 2022-09-07 18:40:09 UTC
* Number of recursive dependencies: 119

Run `cloud_details(, "RKorAPClient")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in documentation object 'reexports.Rd':
      ‘[dplyr:bind]{bind_cols}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
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

# rsample

<details>

* Version: 1.1.0
* GitHub: https://github.com/tidymodels/rsample
* Source code: https://github.com/cran/rsample
* Date/Publication: 2022-08-08 07:00:02 UTC
* Number of recursive dependencies: 112

Run `cloud_details(, "rsample")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      `x` inherits from 'tbl_df'/'tbl'/'data.frame' not 'rset'.
      Backtrace:
          ▆
       1. └─rsample:::expect_s3_class_rset(slice(x)) at test-compat-dplyr.R:254:4
       2.   └─testthat::expect_s3_class(x, "rset") at tests/testthat/helpers-rsample.R:7:2
      ── Failure ('test-compat-dplyr.R:254'): slice() keeps rset class when rows are untouched ──
      `x` inherits from 'tbl_df'/'tbl'/'data.frame' not 'rset'.
      Backtrace:
          ▆
       1. └─rsample:::expect_s3_class_rset(slice(x)) at test-compat-dplyr.R:254:4
       2.   └─testthat::expect_s3_class(x, "rset") at tests/testthat/helpers-rsample.R:7:2
      
      [ FAIL 18 | WARN 108 | SKIP 34 | PASS 2736 ]
      Error: Test failures
      Execution halted
    ```

# ruler

<details>

* Version: 0.2.4
* GitHub: https://github.com/echasnovski/ruler
* Source code: https://github.com/cran/ruler
* Date/Publication: 2020-11-25 08:00:03 UTC
* Number of recursive dependencies: 64

Run `cloud_details(, "ruler")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘design-and-format.Rmd’ using rmarkdown
    --- finished re-building ‘design-and-format.Rmd’
    
    --- re-building ‘rule-packs.Rmd’ using rmarkdown
    Quitting from lines 109-115 (rule-packs.Rmd) 
    Error: processing vignette 'rule-packs.Rmd' failed with diagnostics:
    `funs()` was deprecated in dplyr 0.8.0 and is now defunct.
    Please use a list of either functions or lambdas:
    
    ...
    --- failed re-building ‘rule-packs.Rmd’
    
    --- re-building ‘validation.Rmd’ using rmarkdown
    --- finished re-building ‘validation.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘rule-packs.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# salesforcer

<details>

* Version: 1.0.1
* GitHub: https://github.com/StevenMMortimer/salesforcer
* Source code: https://github.com/cran/salesforcer
* Date/Publication: 2022-03-01 21:50:02 UTC
* Number of recursive dependencies: 99

Run `cloud_details(, "salesforcer")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in documentation object 'format_report_row.Rd':
      ‘[dplyr:bind]{bind_rows}’
    
    Missing link or links in documentation object 'parse_report_detail_rows.Rd':
      ‘[dplyr:bind]{bind_rows}’
    
    Missing link or links in documentation object 'sf_execute_report.Rd':
      ‘[dplyr:bind]{bind_rows}’
    
    Missing link or links in documentation object 'sf_get_report_instance_results.Rd':
    ...
    Missing link or links in documentation object 'sf_query_result_bulk_v2.Rd':
      ‘[dplyr:bind]{bind_rows}’
    
    Missing link or links in documentation object 'sf_run_bulk_query.Rd':
      ‘[dplyr:bind]{bind_rows}’
    
    Missing link or links in documentation object 'sf_run_report.Rd':
      ‘[dplyr:bind]{bind_rows}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

# seecolor

<details>

* Version: 0.1.0
* GitHub: https://github.com/lovestat/seecolor
* Source code: https://github.com/cran/seecolor
* Date/Publication: 2020-12-07 17:40:03 UTC
* Number of recursive dependencies: 64

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

# sf

<details>

* Version: 1.0-9
* GitHub: https://github.com/r-spatial/sf
* Source code: https://github.com/cran/sf
* Date/Publication: 2022-11-08 14:20:02 UTC
* Number of recursive dependencies: 157

Run `cloud_details(, "sf")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in documentation object 'bind.Rd':
      ‘[dplyr:bind]{bind_cols}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 25.5Mb
      sub-directories of 1Mb or more:
        doc      1.7Mb
        libs    19.1Mb
        sqlite   1.5Mb
    ```

# sfc

<details>

* Version: 0.1.0
* GitHub: https://github.com/ctfysh/sfc
* Source code: https://github.com/cran/sfc
* Date/Publication: 2016-08-25 10:01:01
* Number of recursive dependencies: 27

Run `cloud_details(, "sfc")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘sfc-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: sfc
    > ### Title: Substance Flow Computation
    > ### Aliases: sfc
    > 
    > ### ** Examples
    > 
    > library(sfc)
    ...
     1. ├─sfc::sfc(data, model, sample.size = 100, fileEncoding = "UTF-8")
     2. │ └─sfc:::impute_data(data)
     3. │   └─... %>% mutate_each(funs(as.character(.)))
     4. ├─dplyr::mutate_each(., funs(as.character(.)))
     5. │ └─rlang::is_character(funs)
     6. └─dplyr::funs(as.character(.))
     7.   └─lifecycle::deprecate_stop(...)
     8.     └─lifecycle:::deprecate_stop0(msg)
     9.       └─rlang::cnd_signal(...)
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# sftime

<details>

* Version: 0.2-0
* GitHub: NA
* Source code: https://github.com/cran/sftime
* Date/Publication: 2022-03-17 08:50:01 UTC
* Number of recursive dependencies: 78

Run `cloud_details(, "sftime")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in documentation object 'bind.Rd':
      ‘[dplyr:bind]{bind_cols}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
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
    --- re-building ‘simplevis.Rmd’ using rmarkdown
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    ...
    Quitting from lines 412-437 (simplevis.Rmd) 
    Error: processing vignette 'simplevis.Rmd' failed with diagnostics:
    'names' attribute [9] must be the same length as the vector [3]
    --- failed re-building ‘simplevis.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘simplevis.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# sjmisc

<details>

* Version: 2.8.9
* GitHub: https://github.com/strengejacke/sjmisc
* Source code: https://github.com/cran/sjmisc
* Date/Publication: 2021-12-03 10:40:02 UTC
* Number of recursive dependencies: 107

Run `cloud_details(, "sjmisc")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘sjmisc-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: descr
    > ### Title: Basic descriptive statistics
    > ### Aliases: descr
    > 
    > ### ** Examples
    > 
    > data(efc)
    ...
      7. ├─base::as.data.frame(.)
      8. ├─dplyr::summarise_all(...)
      9. │ └─dplyr:::manip_all(...)
     10. │   └─dplyr:::as_fun_list(...)
     11. │     └─dplyr:::is_fun_list(.funs)
     12. └─dplyr::funs(...)
     13.   └─lifecycle::deprecate_stop(...)
     14.     └─lifecycle:::deprecate_stop0(msg)
     15.       └─rlang::cnd_signal(...)
    Execution halted
    ```

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in documentation object 'add_columns.Rd':
      ‘[dplyr:bind]{dplyr::bind_cols()}’
    
    Missing link or links in documentation object 'add_rows.Rd':
      ‘[dplyr:bind]{dplyr::bind_rows()}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

# sjPlot

<details>

* Version: 2.8.12
* GitHub: https://github.com/strengejacke/sjPlot
* Source code: https://github.com/cran/sjPlot
* Date/Publication: 2022-11-19 22:20:02 UTC
* Number of recursive dependencies: 186

Run `cloud_details(, "sjPlot")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘blackwhitefigures.Rmd’ using rmarkdown
    
    Attaching package: 'ggplot2'
    
    The following object is masked from 'package:sjlabelled':
    
        as_label
    
    Data were 'prettified'. Consider using `terms="barthel [all]"` to get
    ...
    --- finished re-building ‘tab_model_robust.Rmd’
    
    --- re-building ‘table_css.Rmd’ using rmarkdown
    --- finished re-building ‘table_css.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘sjtitemanalysis.Rmd’
    
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

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in documentation object 'skimr-vctrs.Rd':
      ‘[dplyr:bind]{dplyr::bind_rows()}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

# skynet

<details>

* Version: 1.4.3
* GitHub: https://github.com/ropensci/skynet
* Source code: https://github.com/cran/skynet
* Date/Publication: 2022-06-17 13:00:02 UTC
* Number of recursive dependencies: 104

Run `cloud_details(, "skynet")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘skynet-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: summary.skynet
    > ### Title: Displays a summary of a skynet object
    > ### Aliases: summary.skynet
    > 
    > ### ** Examples
    > 
    > net <- make_net_dir(OD_Sample)
    ...
      3. │   └─... %>% mutate_all(funs(ifelse(is.na(.), 0, .)))
      4. ├─dplyr::mutate_all(., funs(ifelse(is.na(.), 0, .)))
      5. │ └─dplyr:::manip_all(...)
      6. │   └─dplyr:::as_fun_list(...)
      7. │     └─dplyr:::is_fun_list(.funs)
      8. └─dplyr::funs(ifelse(is.na(.), 0, .))
      9.   └─lifecycle::deprecate_stop(...)
     10.     └─lifecycle:::deprecate_stop0(msg)
     11.       └─rlang::cnd_signal(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        2. ├─skynet::make_net_dir(OD_Sample)
        3. │ └─skynet::node_stats(x)
        4. │   └─... %>% mutate_all(funs(ifelse(is.na(.), 0, .)))
        5. ├─dplyr::mutate_all(., funs(ifelse(is.na(.), 0, .)))
        6. │ └─dplyr:::manip_all(...)
        7. │   └─dplyr:::as_fun_list(...)
        8. │     └─dplyr:::is_fun_list(.funs)
        9. └─dplyr::funs(ifelse(is.na(.), 0, .))
       10.   └─lifecycle::deprecate_stop(...)
       11.     └─lifecycle:::deprecate_stop0(msg)
       12.       └─rlang::cnd_signal(...)
      
      [ FAIL 14 | WARN 0 | SKIP 4 | PASS 20 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘skynet.Rmd’ using rmarkdown
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    ...
    
      # Using lambdas
      list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
    --- failed re-building ‘skynet.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘skynet.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘maps’
      All declared Imports should be used.
    ```

# slackr

<details>

* Version: 3.2.0
* GitHub: https://github.com/mrkaye97/slackr
* Source code: https://github.com/cran/slackr
* Date/Publication: 2021-09-20 15:10:02 UTC
* Number of recursive dependencies: 89

Run `cloud_details(, "slackr")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in documentation object 'with_pagination.Rd':
      ‘[dplyr:bind]{dplyr::bind_rows()}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘methods’
      All declared Imports should be used.
    ```

# soiltestcorr

<details>

* Version: 2.1.2
* GitHub: https://github.com/adriancorrendo/soiltestcorr
* Source code: https://github.com/cran/soiltestcorr
* Date/Publication: 2022-06-12 15:50:02 UTC
* Number of recursive dependencies: 97

Run `cloud_details(, "soiltestcorr")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in documentation object 'linear_plateau.Rd':
      ‘[dplyr]{bind}’
    
    Missing link or links in documentation object 'mitscherlich.Rd':
      ‘[dplyr]{bind}’
    
    Missing link or links in documentation object 'mod_alcc.Rd':
      ‘[dplyr]{bind}’
    
    Missing link or links in documentation object 'quadratic_plateau.Rd':
      ‘[dplyr]{bind}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

# staRdom

<details>

* Version: 1.1.25
* GitHub: https://github.com/MatthiasPucher/staRdom
* Source code: https://github.com/cran/staRdom
* Date/Publication: 2022-03-21 15:50:02 UTC
* Number of recursive dependencies: 155

Run `cloud_details(, "staRdom")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Basic_analysis_of_DOM_samples.Rmd’ using rmarkdown
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    ...
    
      # Using lambdas
      list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
    --- failed re-building ‘PARAFAC_analysis_of_EEM.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘PARAFAC_analysis_of_EEM.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# stars

<details>

* Version: 0.6-0
* GitHub: https://github.com/r-spatial/stars
* Source code: https://github.com/cran/stars
* Date/Publication: 2022-11-21 13:10:02 UTC
* Number of recursive dependencies: 153

Run `cloud_details(, "stars")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘stars-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: aggregate.stars
    > ### Title: spatially or temporally aggregate stars object
    > ### Aliases: aggregate.stars aggregate
    > 
    > ### ** Examples
    > 
    > # aggregate time dimension in format Date
    ...
    • index = 17
    • along = "time"
    Backtrace:
        ▆
     1. └─dplyr::slice(prec, index = 17, along = "time")
     2.   └─rlang::check_dots_unnamed()
     3.     └─rlang:::action_dots(...)
     4.       ├─base (local) try_dots(...)
     5.       └─rlang (local) action(...)
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘stars1.Rmd’ using rmarkdown
    Quitting from lines 258-273 (stars1.Rmd) 
    Error: processing vignette 'stars1.Rmd' failed with diagnostics:
    Arguments in `...` must be passed by position, not name.
    ✖ Problematic arguments:
    • index = 17
    • along = "time"
    --- failed re-building ‘stars1.Rmd’
    
    ...
    --- finished re-building ‘stars7.Rmd’
    
    --- re-building ‘stars8.Rmd’ using rmarkdown
    --- finished re-building ‘stars8.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘stars1.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.3Mb
      sub-directories of 1Mb or more:
        doc   2.4Mb
        nc    1.7Mb
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

# statVisual

<details>

* Version: 1.2.1
* GitHub: NA
* Source code: https://github.com/cran/statVisual
* Date/Publication: 2020-02-20 19:30:02 UTC
* Number of recursive dependencies: 185

Run `cloud_details(, "statVisual")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘statVisual-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: BiAxisErrBar
    > ### Title: Compare Patterns of Two Outcomes in One Scatter Plot
    > ### Aliases: BiAxisErrBar
    > ### Keywords: method
    > 
    > ### ** Examples
    > 
    ...
      6. │ └─base::is.data.frame(x)
      7. ├─dplyr::summarise_at(...)
      8. │ └─dplyr:::manip_at(...)
      9. │   └─dplyr:::as_fun_list(...)
     10. │     └─dplyr:::is_fun_list(.funs)
     11. └─dplyr::funs(mean, seFunc)
     12.   └─lifecycle::deprecate_stop(...)
     13.     └─lifecycle:::deprecate_stop0(msg)
     14.       └─rlang::cnd_signal(...)
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘statVisual.Rmd’ using rmarkdown
    Loading required package: BiocGenerics
    
    Attaching package: 'BiocGenerics'
    
    The following objects are masked from 'package:stats':
    
        IQR, mad, sd, var, xtabs
    
    ...
    
      # Using lambdas
      list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
    --- failed re-building ‘statVisual.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘statVisual.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘gbm’ ‘ggfortify’ ‘tibble’ ‘tidyverse’
      All declared Imports should be used.
    ```

# styler

<details>

* Version: 1.8.1
* GitHub: https://github.com/r-lib/styler
* Source code: https://github.com/cran/styler
* Date/Publication: 2022-11-07 23:40:02 UTC
* Number of recursive dependencies: 65

Run `cloud_details(, "styler")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in documentation object 'combine_children.Rd':
      ‘[dplyr:bind]{dplyr::bind_rows()}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
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

* Version: 0.4.1
* GitHub: https://github.com/asardaes/table.express
* Source code: https://github.com/cran/table.express
* Date/Publication: 2022-08-24 22:10:02 UTC
* Number of recursive dependencies: 54

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
      
      [ FAIL 11 | WARN 0 | SKIP 1 | PASS 591 ]
      Error: Test failures
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

# tabularaster

<details>

* Version: 0.7.1
* GitHub: https://github.com/hypertidy/tabularaster
* Source code: https://github.com/cran/tabularaster
* Date/Publication: 2022-10-18 03:00:02 UTC
* Number of recursive dependencies: 84

Run `cloud_details(, "tabularaster")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘cell-index-raster-abstraction.Rmd’ using rmarkdown
    --- finished re-building ‘cell-index-raster-abstraction.Rmd’
    
    --- re-building ‘tabularaster-usage.Rmd’ using rmarkdown
    Quitting from lines 176-187 (tabularaster-usage.Rmd) 
    Error: processing vignette 'tabularaster-usage.Rmd' failed with diagnostics:
    `funs()` was deprecated in dplyr 0.8.0 and is now defunct.
    Please use a list of either functions or lambdas:
    ...
    
      # Using lambdas
      list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
    --- failed re-building ‘tabularaster-usage.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘tabularaster-usage.Rmd’
    
    Error: Vignette re-building failed.
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

# tidyboot

<details>

* Version: 0.1.1
* GitHub: https://github.com/langcog/tidyboot
* Source code: https://github.com/cran/tidyboot
* Date/Publication: 2018-03-14 04:13:49 UTC
* Number of recursive dependencies: 40

Run `cloud_details(, "tidyboot")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘tidyboot-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: tidyboot.data.frame
    > ### Title: Non-parametric bootstrap for data frames
    > ### Aliases: tidyboot.data.frame
    > 
    > ### ** Examples
    > 
    > ## Mean and 95% confidence interval for 500 samples from two different normal distributions
    ...
      5. │   └─x %>% ...
      6. ├─dplyr::summarise_at(., vars(mean), funs(ci_upper, mean, ci_lower))
      7. │ └─dplyr:::manip_at(...)
      8. │   └─dplyr:::as_fun_list(...)
      9. │     └─dplyr:::is_fun_list(.funs)
     10. └─dplyr::funs(ci_upper, mean, ci_lower)
     11.   └─lifecycle::deprecate_stop(...)
     12.     └─lifecycle:::deprecate_stop0(msg)
     13.       └─rlang::cnd_signal(...)
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
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

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in documentation object 'bind_graphs.Rd':
      ‘[dplyr:bind]{dplyr::bind_rows()}’
    
    Missing link or links in documentation object 'graph_join.Rd':
      ‘[dplyr:bind]{dplyr::bind_rows()}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
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

# tidyterra

<details>

* Version: 0.3.1
* GitHub: https://github.com/dieghernan/tidyterra
* Source code: https://github.com/cran/tidyterra
* Date/Publication: 2022-11-09 12:40:02 UTC
* Number of recursive dependencies: 128

Run `cloud_details(, "tidyterra")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Error in `slice_sample(r, n = 1, replace = FALSE, .keep_extent = TRUE)`: `...` must be empty.
      x Problematic argument:
      * .keep_extent = TRUE
      Backtrace:
          ▆
       1. └─dplyr::slice_sample(r, n = 1, replace = FALSE, .keep_extent = TRUE) at test-slice-SpatRaster.R:426:2
       2.   └─dplyr:::check_slice_dots(..., n = n, prop = prop)
       3.     └─rlang::check_dots_empty(call = error_call)
       4.       └─rlang:::action_dots(...)
       5.         ├─base (local) try_dots(...)
       6.         └─rlang (local) action(...)
      
      [ FAIL 6 | WARN 0 | SKIP 18 | PASS 899 ]
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
           ▆
        1. ├─tree %<>% left_join(dat, by = "node") at test-dplyr-methods.R:125:4
        2. ├─dplyr::left_join(., dat, by = "node")
        3. ├─tidytree:::left_join.treedata(., dat, by = "node")
        4. │ └─dat %>% ...
        5. └─dplyr::left_join(., y, by = by, copy = copy, suffix = suffix, !!!dots)
        6.   └─rlang (local) `<fn>`()
        7.     └─rlang:::check_dots(env, error, action, call)
        8.       └─rlang:::action_dots(...)
        9.         ├─base (local) try_dots(...)
       10.         └─rlang (local) action(...)
      
      [ FAIL 2 | WARN 2 | SKIP 0 | PASS 62 ]
      Error: Test failures
      Execution halted
    ```

# TKCat

<details>

* Version: 1.0.6
* GitHub: https://github.com/patzaw/TKCat
* Source code: https://github.com/cran/TKCat
* Date/Publication: 2022-10-21 11:35:09 UTC
* Number of recursive dependencies: 116

Run `cloud_details(, "TKCat")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘TKCat-User-guide.Rmd’ using rmarkdown
    Loading required package: ReDaMoR
    Loading required package: dplyr
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    ...
    Arguments in `...` must be passed by position, not name.
    ✖ Problematic argument:
    • ReferenceClinVarAssertion = grep(...)
    --- failed re-building ‘TKCat-User-guide.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘TKCat-User-guide.Rmd’
    
    Error: Vignette re-building failed.
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

# tsibble

<details>

* Version: 1.1.3
* GitHub: https://github.com/tidyverts/tsibble
* Source code: https://github.com/cran/tsibble
* Date/Publication: 2022-10-09 03:20:02 UTC
* Number of recursive dependencies: 96

Run `cloud_details(, "tsibble")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in documentation object 'tsibble-tidyverse.Rd':
      ‘[dplyr:bind]{dplyr::bind_rows()}’ ‘[dplyr:bind]{dplyr::bind_cols()}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
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

# useful

<details>

* Version: 1.2.6
* GitHub: https://github.com/jaredlander/useful
* Source code: https://github.com/cran/useful
* Date/Publication: 2018-10-08 16:00:03 UTC
* Number of recursive dependencies: 63

Run `cloud_details(, "useful")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘useful-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: constant
    > ### Title: constant
    > ### Aliases: constant
    > 
    > ### ** Examples
    > 
    > constant(4)(1:10)
    ...
      2. └─useful::simple.impute.data.frame(theDF, constant(4))
      3.   ├─dplyr::mutate_at(...)
      4.   │ └─dplyr:::manip_at(...)
      5.   │   └─dplyr:::as_fun_list(...)
      6.   │     └─dplyr:::is_fun_list(.funs)
      7.   └─dplyr::funs(simple.impute(., fun = fun))
      8.     └─lifecycle::deprecate_stop(...)
      9.       └─lifecycle:::deprecate_stop0(msg)
     10.         └─rlang::cnd_signal(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
        4. ├─useful::simple.impute(theDF)
        5. └─useful::simple.impute.data.frame(theDF)
        6.   ├─dplyr::mutate_at(...)
        7.   │ └─dplyr:::manip_at(...)
        8.   │   └─dplyr:::as_fun_list(...)
        9.   │     └─dplyr:::is_fun_list(.funs)
       10.   └─dplyr::funs(simple.impute(., fun = fun))
       11.     └─lifecycle::deprecate_stop(...)
       12.       └─lifecycle:::deprecate_stop0(msg)
       13.         └─rlang::cnd_signal(...)
      
      [ FAIL 1 | WARN 9 | SKIP 2 | PASS 742 ]
      Error: Test failures
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

# xpose

<details>

* Version: 0.4.14
* GitHub: https://github.com/UUPharmacometrics/xpose
* Source code: https://github.com/cran/xpose
* Date/Publication: 2022-11-07 22:30:02 UTC
* Number of recursive dependencies: 108

Run `cloud_details(, "xpose")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘xpose-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: subset_xpdb
    > ### Title: Subset datasets in an xpdb
    > ### Aliases: subset_xpdb filter.xpose_data slice.xpose_data
    > ###   distinct.xpose_data
    > 
    > ### ** Examples
    > 
    ...
     1. ├─xpdb_ex_pk %>% slice(1:100, .problem = 1) %>% dv_vs_ipred()
     2. ├─xpose::dv_vs_ipred(.)
     3. │ └─xpose::check_xpdb(xpdb, check = "data")
     4. │   └─xpose::is.xpdb(xpdb)
     5. └─dplyr::slice(., 1:100, .problem = 1)
     6.   └─rlang::check_dots_unnamed()
     7.     └─rlang:::action_dots(...)
     8.       ├─base (local) try_dots(...)
     9.       └─rlang (local) action(...)
    Execution halted
    ```

# xray

<details>

* Version: 0.2
* GitHub: https://github.com/sicarul/xray
* Source code: https://github.com/cran/xray
* Date/Publication: 2017-12-08 05:15:59 UTC
* Number of recursive dependencies: 37

Run `cloud_details(, "xray")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘xray-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: anomalies
    > ### Title: Analyze a dataset and search for anomalies
    > ### Aliases: anomalies
    > 
    > ### ** Examples
    > 
    > 
    ...
     14. │       └─dplyr:::tbl_vars_dispatch(x)
     15. ├─dplyr::mutate_all(...)
     16. │ └─dplyr:::manip_all(...)
     17. │   └─dplyr:::as_fun_list(...)
     18. │     └─dplyr:::is_fun_list(.funs)
     19. └─dplyr::funs(...)
     20.   └─lifecycle::deprecate_stop(...)
     21.     └─lifecycle:::deprecate_stop0(msg)
     22.       └─rlang::cnd_signal(...)
    Execution halted
    ```

