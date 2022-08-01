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
    
    > ### Name: AQuadtree-class
    > ### Title: Class "AQuadtree".
    > ### Aliases: AQuadtree-class AQuadtree
    > 
    > ### ** Examples
    > 
    > data("BarcelonaPop", "BarcelonaCensusTracts")
    > aquadtree.Barcelona<-AQuadtree(BarcelonaPop, layers = 3)
    Error: `summarise_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `summarise()` instead.
    Execution halted
    ```

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘AQuadtreeUse.Rmd’ using rmarkdown
    Warning in eng_r(options) :
      Failed to tidy R code in chunk 'unnamed-chunk-5'. Reason:
    Error : The formatR package is required by the chunk option tidy = TRUE but not installed; tidy = TRUE will be ignored.
    
    Quitting from lines 97-99 (AQuadtreeUse.Rmd) 
    Error: processing vignette 'AQuadtreeUse.Rmd' failed with diagnostics:
    `summarise_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `summarise()` instead.
    --- failed re-building ‘AQuadtreeUse.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘AQuadtreeUse.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        data   3.8Mb
    ```

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

# auctestr

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/auctestr
* Date/Publication: 2017-11-13 09:46:18 UTC
* Number of recursive dependencies: 54

Run `cloud_details(, "auctestr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘auctestr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: auc_compare
    > ### Title: Compare AUC values using the FBH method.
    > ### Aliases: auc_compare
    > 
    > ### ** Examples
    > 
    > ## load sample experiment data
    ...
    +     compare_values = c('ModelA', 'ModelB'),
    +     filter_value = c('VariantA'),
    +     time_col = 'time',
    +     outcome_col = 'auc',
    +     compare_col = 'model_id',
    +     over_col = 'dataset',
    +     filter_col = 'model_variant')
    Error: `filter_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `filter()` instead.
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Backtrace:
          ▆
       1. ├─testthat::expect_equal(...) at test-auc-compare.R:5:2
       2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
       3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
       4. ├─auctestr::auc_compare(...)
       5. │ └─... %>% dplyr::filter_(filter_str)
       6. └─dplyr::filter_(., filter_str)
       7.   └─dplyr:::lazy_deprec("filter")
       8.     └─lifecycle::deprecate_stop(...)
       9.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 10 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘basic-auctestr-vignette.Rmd’ using rmarkdown
    Quitting from lines 39-44 (basic-auctestr-vignette.Rmd) 
    Error: processing vignette 'basic-auctestr-vignette.Rmd' failed with diagnostics:
    `filter_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `filter()` instead.
    --- failed re-building ‘basic-auctestr-vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘basic-auctestr-vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tidyr’
      All declared Imports should be used.
    ```

# AzureKusto

<details>

* Version: 1.0.6
* GitHub: https://github.com/Azure/AzureKusto
* Source code: https://github.com/cran/AzureKusto
* Date/Publication: 2020-04-27 05:30:02 UTC
* Number of recursive dependencies: 70

Run `cloud_details(, "AzureKusto")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Backtrace:
          ▆
       1. ├─tbl_iris %>% tidyr::nest(data = c(-Species)) at test_translate.r:645:8
       2. ├─tidyr::nest(., data = c(-Species))
       3. └─AzureKusto::nest.tbl_kusto_abstract(., data = c(-Species))
       4.   ├─dplyr::summarise(group_by(.data, !!as.name(group_vars)), !!!dot_calls)
       5.   ├─dplyr::group_by(.data, !!as.name(group_vars))
       6.   └─AzureKusto:::group_by.tbl_kusto_abstract(.data, !!as.name(group_vars))
       7.     └─dplyr::group_by_prepare(.data, .dots = dots, add = add)
       8.       └─lifecycle::deprecate_stop("1.0.0", "group_by(.dots = )")
       9.         └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 9 | WARN 1 | SKIP 7 | PASS 91 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tibble’
      All declared Imports should be used.
    ```

# benthos

<details>

* Version: 1.3-6
* GitHub: NA
* Source code: https://github.com/cran/benthos
* Date/Publication: 2019-03-17 22:43:20 UTC
* Number of recursive dependencies: 79

Run `cloud_details(, "benthos")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘benthos-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ambi
    > ### Title: AZTI Marine Biotic Index (AMBI)
    > ### Aliases: ambi ambi_ has_ambi has_ambi_
    > 
    > ### ** Examples
    > 
    >  ambi(
    ...
    +      taxon = c("Euspira pulchella", "Nephtys cirrosa"), 
    +      count = c(4, 6)
    +  )
    Warning: `data_frame()` was deprecated in tibble 1.1.0.
    Please use `tibble()` instead.
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.
    Error: `filter_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `filter()` instead.
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘test-all.R’
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
           ▆
        1. ├─testthat::expect_equal(...) at test-indicators.R:236:4
        2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
        3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
        4. ├─benthos::iti(...)
        5. │ └─benthos::iti_(.data, lazy(taxon), lazy(count), lazy(group))
        6. │   └─d %>% select_(~GROUP, ~COUNT) %>% filter_(~!is.na(GROUP))
        7. └─dplyr::filter_(., ~!is.na(GROUP))
        8.   └─dplyr:::lazy_deprec("filter")
        9.     └─lifecycle::deprecate_stop(...)
       10.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 2 | WARN 31 | SKIP 1 | PASS 136 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘benthos.Rmd’ using rmarkdown
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    ...
    Error: processing vignette 'benthos.Rmd' failed with diagnostics:
    `filter_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `filter()` instead.
    --- failed re-building ‘benthos.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘benthos.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# bioinactivation

<details>

* Version: 1.2.3
* GitHub: NA
* Source code: https://github.com/cran/bioinactivation
* Date/Publication: 2019-08-01 16:40:15 UTC
* Number of recursive dependencies: 75

Run `cloud_details(, "bioinactivation")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘bioinactivation-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: fit_dynamic_inactivation
    > ### Title: Fitting of Dynamic Inactivation Models
    > ### Aliases: fit_dynamic_inactivation
    > 
    > ### ** Examples
    > 
    > ## EXAMPLE 1 ------
    ...
    > upper_bounds <- c(n = 2, k_b = 1, N0 = Inf)
    > lower_bounds <- c(n = 0, k_b = 0, N0 = 1e4)
    > 
    > dynamic_fit <- fit_dynamic_inactivation(dynamic_inactivation, simulation_model,
    +                                         dummy_temp, starting_points,
    +                                         upper_bounds, lower_bounds,
    +                                         known_params)
    Error: `mutate_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `mutate()` instead.
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      + })
      ── Error (???): Prediction Bigelow ─────────────────────────────────────────────
      <lifecycle_error_deprecated/defunctError/error/condition/condition>
      Error: `mutate_()` was deprecated in dplyr 0.7.0 and is now defunct.
      Please use `mutate()` instead.
      Backtrace:
       1. bioinactivation::predict_inactivation(...)
       2. dplyr::mutate_(...)
       3. dplyr:::lazy_deprec("mutate")
       4. lifecycle::deprecate_stop(...)
       5. lifecycle:::deprecate_stop0(msg)
      
      Error in reporter$stop_if_needed() : Test failed
      Calls: test_that -> <Anonymous>
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘inactivation.Rmd’ using rmarkdown
    Quitting from lines 375-376 (inactivation.Rmd) 
    Error: processing vignette 'inactivation.Rmd' failed with diagnostics:
    `mutate_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `mutate()` instead.
    --- failed re-building ‘inactivation.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘inactivation.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# bioOED

<details>

* Version: 0.2.1
* GitHub: NA
* Source code: https://github.com/cran/bioOED
* Date/Publication: 2019-08-07 15:00:02 UTC
* Number of recursive dependencies: 138

Run `cloud_details(, "bioOED")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘bioOED-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: calculate_pars_correlation
    > ### Title: Correlation Between Model Parameters Sensitivities
    > ### Aliases: calculate_pars_correlation
    > 
    > ### ** Examples
    > 
    > 
    > parms_fix <- c(temp_ref = 57.5)
    > parms <- c(delta_ref = 3.9, z = 4.2, p = 1, N0 = 1e6)
    > temp_profile <- data.frame(time = c(0, 60), temperature = c(30, 60))
    > correlations <- calculate_pars_correlation("Mafart", parms,
    +                                             temp_profile, parms_fix)
    Error: `mutate_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `mutate()` instead.
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘vignette_bioOED.Rmd’ using rmarkdown
    Quitting from lines 100-103 (vignette_bioOED.Rmd) 
    Error: processing vignette 'vignette_bioOED.Rmd' failed with diagnostics:
    `mutate_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `mutate()` instead.
    --- failed re-building ‘vignette_bioOED.Rmd’
    
    --- re-building ‘vignette_bioOED_isothermal.Rmd’ using rmarkdown
    --- finished re-building ‘vignette_bioOED_isothermal.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘vignette_bioOED.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# bootnet

<details>

* Version: 1.5
* GitHub: NA
* Source code: https://github.com/cran/bootnet
* Date/Publication: 2021-10-25 16:20:09 UTC
* Number of recursive dependencies: 171

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
    Warning in EBICglassoCore(S = S, n = n, gamma = gamma, penalize.diagonal = penalize.diagonal,  :
      A dense regularized network was selected (lambda < 0.1 * lambda.max). Recent work indicates a possible drop in specificity. Interpret the presence of the smallest edges with care. Setting threshold = TRUE will enforce higher specificity, at the cost of sensitivity.
    Warning in EBICglassoCore(S = S, n = n, gamma = gamma, penalize.diagonal = penalize.diagonal,  :
      A dense regularized network was selected (lambda < 0.1 * lambda.max). Recent work indicates a possible drop in specificity. Interpret the presence of the smallest edges with care. Setting threshold = TRUE will enforce higher specificity, at the cost of sensitivity.
    > 
    > # Results:
    > Res
    Error: `arrange_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `arrange()` instead.
    Execution halted
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

# capm

<details>

* Version: 0.14.0
* GitHub: NA
* Source code: https://github.com/cran/capm
* Date/Publication: 2019-10-24 16:50:05 UTC
* Number of recursive dependencies: 60

Run `cloud_details(, "capm")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘capm-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: FreqTab
    > ### Title: Frequency table of categorical variables
    > ### Aliases: FreqTab
    > 
    > ### ** Examples
    > 
    > data(cluster_sample)
    ...
    9         9     2      0.002
    10       10     2      0.002
    11        8     1      0.001
    12       12     1      0.001
    > 
    > data(dogs)
    > FreqTab(dogs, c("species", "sex"))
    Error: `select_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `select()` instead.
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 59 marked UTF-8 strings
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
    > library(magrittr)
    > outline_table(iris, 'Species') %>%
    +  add_rows(c('Sepal.Length', 'Petal.Length'), stat_meanSD) %>%
    +  add_rows('Sepal.Width', stat_medianIQR) %>%
    +  renaming('rows', function(x) gsub('Sepal\\.', 'Sepal ', x)) %>%
    +  renaming('header', c('Measures', 'Setosa', 'Versicolor', 'Virginica')) %>%
    +  build_table(caption = 'A caption for the table')
    Error: `summarize_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `summarize()` instead.
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       12. │       └─dplyr::tbl_vars(tbl)
       13. │         ├─dplyr:::new_sel_vars(tbl_vars_dispatch(x), group_vars(x))
       14. │         │ └─base::structure(...)
       15. │         └─dplyr:::tbl_vars_dispatch(x)
       16. ├─dplyr::full_join(., variable_names_order, by = "Variables")
       17. ├─dplyr::ungroup(.)
       18. ├─tidyr::spread(., header, "n")
       19. └─dplyr::mutate_(...)
       20.   └─dplyr:::lazy_deprec("mutate")
       21.     └─lifecycle::deprecate_stop(...)
       22.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 2 | WARN 1 | SKIP 0 | PASS 19 ]
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
    `summarize_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `summarize()` instead.
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

# CB2

<details>

* Version: 1.3.4
* GitHub: NA
* Source code: https://github.com/cran/CB2
* Date/Publication: 2020-07-24 09:42:24 UTC
* Number of recursive dependencies: 110

Run `cloud_details(, "CB2")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘CB2-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: calc_mappability
    > ### Title: A function to calculate the mappabilities of each NGS sample.
    > ### Aliases: calc_mappability
    > 
    > ### ** Examples
    > 
    > library(CB2)
    ...
    +   "Base", "Base2", 
    +   "High", "High1",
    +   "High", "High2") %>% 
    +     mutate(fastq_path = glue("{ex_path}/{sample_name}.fastq"))
    > 
    > cb2_count <- run_sgrna_quant(FASTA, df_design)
    > calc_mappability(cb2_count, df_design)
    Error: `select_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `select()` instead.
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘cb2-input-handling.Rmd’ using rmarkdown
    --- finished re-building ‘cb2-input-handling.Rmd’
    
    --- re-building ‘cb2-tutorial.Rmd’ using rmarkdown
    >RAB_3
    CTGTAGAAGCTACATCGGCT
    >RAB_4
    AGCACATACACGAGCGGAAA
    >RAB_5
    ...
    Error: processing vignette 'cb2-tutorial.Rmd' failed with diagnostics:
    `mutate_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `mutate()` instead.
    --- failed re-building ‘cb2-tutorial.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘cb2-tutorial.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.8Mb
      sub-directories of 1Mb or more:
        data      1.6Mb
        extdata   1.1Mb
        libs      4.6Mb
    ```

# cbsodataR

<details>

* Version: 0.5.1
* GitHub: https://github.com/edwindj/cbsodataR
* Source code: https://github.com/cran/cbsodataR
* Date/Publication: 2020-09-14 12:40:02 UTC
* Number of recursive dependencies: 63

Run `cloud_details(, "cbsodataR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘cbsodataR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: cbs_get_catalogs
    > ### Title: Retrieves the possible catalog values that can be used for
    > ###   retrieving data
    > ### Aliases: cbs_get_catalogs
    > 
    > ### ** Examples
    > 
    ...
    Warning in open.connection(con, "rb") :
      URL 'http://opendata.cbs.nl/ODataCatalog/Tables?$format=json': Timeout of 60 seconds was reached
    Warning: Failing: http://opendata.cbs.nl/ODataCatalog/Tables?$format=json
    Retrying...
    Warning in open.connection(con, "rb") :
      URL 'http://opendata.cbs.nl/ODataCatalog/Tables?$format=json': Timeout of 60 seconds was reached
    Error in open.connection(con, "rb") : 
      cannot open the connection to 'http://opendata.cbs.nl/ODataCatalog/Tables?$format=json'
    Calls: cbs_get_catalogs ... parse_and_simplify -> parseJSON -> parse_con -> open -> open.connection
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

# codified

<details>

* Version: 0.2.0
* GitHub: https://github.com/OuhscBbmc/codified
* Source code: https://github.com/cran/codified
* Date/Publication: 2018-09-30 16:10:02 UTC
* Number of recursive dependencies: 121

Run `cloud_details(, "codified")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘codified-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: table_nih_enrollment
    > ### Title: Produce an NIH-compliant enrollment table.
    > ### Aliases: table_nih_enrollment table_nih_enrollment_pretty
    > 
    > ### ** Examples
    > 
    > library(magrittr)
    ...
    +            5L,   "Male"   , "White"                    ,  "Not Hispanic or Latino"        ,
    +            6L,   "Female" , "White"                    ,  "Not Hispanic or Latino"        ,
    +            7L,   "Male"   , "White"                    ,  "Hispanic or Latino"            ,
    +            8L,   "Male"   , "White"                    ,  "Hispanic or Latino"
    + )
    > 
    > table_nih_enrollment(ds_1)
    Error: `select_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `select()` instead.
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      <lifecycle_error_deprecated/defunctError/error/condition/condition>
      Error: `select_()` was deprecated in dplyr 0.7.0 and is now defunct.
      Please use `select()` instead.
      Backtrace:
          ▆
       1. ├─codified::table_nih_enrollment(...) at test-table-nih-enrollment.R:163:2
       2. │ └─d %>% ...
       3. └─dplyr::select_(...)
       4.   └─dplyr:::lazy_deprec("select")
       5.     └─lifecycle::deprecate_stop(...)
       6.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 7 | WARN 0 | SKIP 0 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘nih-enrollment-html.Rmd’ using rmarkdown
    Quitting from lines 100-108 (nih-enrollment-html.Rmd) 
    Error: processing vignette 'nih-enrollment-html.Rmd' failed with diagnostics:
    `select_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `select()` instead.
    --- failed re-building ‘nih-enrollment-html.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘nih-enrollment-html.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘methods’ ‘readr’
      All declared Imports should be used.
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# cofeatureR

<details>

* Version: 1.1.1
* GitHub: https://github.com/tinyheero/cofeatureR
* Source code: https://github.com/cran/cofeatureR
* Date/Publication: 2018-06-24 15:09:05 UTC
* Number of recursive dependencies: 51

Run `cloud_details(, "cofeatureR")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── Error (test_plot_cofeature_mat.R:23:5): plot_cofeature_mat correctly adds tile borders ──
      <lifecycle_error_deprecated/defunctError/error/condition/condition>
      Error: `mutate_()` was deprecated in dplyr 0.7.0 and is now defunct.
      Please use `mutate()` instead.
      Backtrace:
          ▆
       1. └─cofeatureR::plot_cofeature_mat(in_df, tile.col = cur_border_col) at test_plot_cofeature_mat.R:23:4
       2.   └─dplyr::mutate_(in.df, .dots = setNames(list(mutate.call), "feature"))
       3.     └─dplyr:::lazy_deprec("mutate")
       4.       └─lifecycle::deprecate_stop(...)
       5.         └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tibble’
      All declared Imports should be used.
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# cogmapr

<details>

* Version: 0.9.3
* GitHub: NA
* Source code: https://github.com/cran/cogmapr
* Date/Publication: 2022-01-04 15:40:07 UTC
* Number of recursive dependencies: 82

Run `cloud_details(, "cogmapr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘cogmapr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: RelationshipTestSummary
    > ### Title: Summary table on relationship comparisons
    > ### Aliases: RelationshipTestSummary
    > 
    > ### ** Examples
    > 
    > project_name <- "a_new_project"
    ...
    Warning: Each row in `x` should match at most 1 row in `y`.
    ℹ Row 1 of `x` matches multiple rows.
    ℹ If multiple matches are expected, specify `multiple = "all"` in the join call
      to silence this warning.
    > 
    > ## Here 0.6 is used only for producing an output. No signif. diff. is reported.
    > RelationshipTestSummary(my.project, units = c("Belgium", "Québec"), 0.6)
    Error: `rename__()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `rename_()` instead.
    Execution halted
    ```

# CollapseLevels

<details>

* Version: 0.3.0
* GitHub: NA
* Source code: https://github.com/cran/CollapseLevels
* Date/Publication: 2020-06-04 13:20:02 UTC
* Number of recursive dependencies: 55

Run `cloud_details(, "CollapseLevels")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘CollapseLevels-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: IVCalc
    > ### Title: IVCalc
    > ### Aliases: IVCalc
    > 
    > ### ** Examples
    > 
    > 
    ...
    > data("German_Credit")
    > 
    > l<-list()
    > 
    > # Call the function as follows
    > 
    > l<-IVCalc(German_Credit,resp="Good_Bad",bins=10)
    Error: `group_by_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `group_by()` instead.
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘CollapseLevels.Rmd’ using rmarkdown
    Quitting from lines 54-76 (CollapseLevels.Rmd) 
    Error: processing vignette 'CollapseLevels.Rmd' failed with diagnostics:
    `group_by_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `group_by()` instead.
    --- failed re-building ‘CollapseLevels.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘CollapseLevels.Rmd’
    
    Error: Vignette re-building failed.
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

# comtradr

<details>

* Version: 0.3.0
* GitHub: https://github.com/ropensci/comtradr
* Source code: https://github.com/cran/comtradr
* Date/Publication: 2022-04-20 06:32:29 UTC
* Number of recursive dependencies: 73

Run `cloud_details(, "comtradr")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘comtradr-vignette.Rmd’ using rmarkdown
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    ...
    Error: processing vignette 'comtradr-vignette.Rmd' failed with diagnostics:
    `group_by_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `group_by()` instead.
    --- failed re-building ‘comtradr-vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘comtradr-vignette.Rmd’
    
    Error: Vignette re-building failed.
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

# cpr

<details>

* Version: 0.2.3
* GitHub: https://github.com/dewittpe/cpr
* Source code: https://github.com/cran/cpr
* Date/Publication: 2017-03-07 13:41:34
* Number of recursive dependencies: 92

Run `cloud_details(, "cpr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘cpr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: cp
    > ### Title: Control Polygons
    > ### Aliases: cp cp.cpr_bs cp.formula print.cpr_cp summary.cpr_cp
    > ###   plot.cpr_cp
    > 
    > ### ** Examples
    > 
    ...
    > 
    > # build the two control polygons
    > cp1 <- cp(bmat1, theta1)
    > cp2 <- cp(bmat2, theta2)
    > 
    > # black and white plot
    > plot(cp1)
    Error: `mutate_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `mutate()` instead.
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        4. └─cpr (local) recover_spline(start_with = 40L, progress = FALSE)
        5.   ├─base::suppressWarnings(do.call(cp, list(formula = f, data = s_data))) at test-recover-know-spline.R:28:2
        6.   │ └─base::withCallingHandlers(...)
        7.   ├─base::do.call(cp, list(formula = f, data = s_data))
        8.   ├─cpr (local) `<fn>`(...)
        9.   └─cpr:::cp.formula(...)
       10.     └─cpr:::generate_cp_formula_data(formula, data)
       11.       └─dplyr::select_(.data, .dots = vars_nobsplines_nobars)
       12.         └─dplyr:::lazy_deprec("select")
       13.           └─lifecycle::deprecate_stop(...)
       14.             └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 18 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘cpr-pkg.Rmd’ using rmarkdown
    Quitting from lines 79-84 (cpr-pkg.Rmd) 
    Error: processing vignette 'cpr-pkg.Rmd' failed with diagnostics:
    `select_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `select()` instead.
    --- failed re-building ‘cpr-pkg.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘cpr-pkg.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.5Mb
      sub-directories of 1Mb or more:
        libs   5.9Mb
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
      
      [ FAIL 1 | WARN 0 | SKIP 26 | PASS 331 ]
      Error: Test failures
      Execution halted
    ```

# crplyr

<details>

* Version: 0.3.9
* GitHub: https://github.com/Crunch-io/crplyr
* Source code: https://github.com/cran/crplyr
* Date/Publication: 2022-05-01 15:10:02 UTC
* Number of recursive dependencies: 87

Run `cloud_details(, "crplyr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        1. ├─testthat::expect_error(...) at test-summarize.R:33:8
        2. │ └─testthat:::quasi_capture(...)
        3. │   ├─testthat (local) .capture(...)
        4. │   │ └─base::withCallingHandlers(...)
        5. │   └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
        6. ├─base::suppressWarnings(...)
        7. │ └─base::withCallingHandlers(...)
        8. └─dplyr::summarise_(...)
        9.   └─dplyr:::lazy_deprec("summarise")
       10.     └─lifecycle::deprecate_stop(...)
       11.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 5 | WARN 0 | SKIP 1 | PASS 167 ]
      Error: Test failures
      Execution halted
    ```

# crsra

<details>

* Version: 0.2.3
* GitHub: https://github.com/jhudsl/crsra
* Source code: https://github.com/cran/crsra
* Date/Publication: 2018-05-05 06:25:58 UTC
* Number of recursive dependencies: 71

Run `cloud_details(, "crsra")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘crsra-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: crsra_import
    > ### Title: Imports all the .csv files into one list consisting of all the
    > ###   courses and all the tables within each course.
    > ### Aliases: crsra_import
    > 
    > ### ** Examples
    > 
    ...
    > zip_file = system.file("extdata", "fake_course_7051862327916.zip",
    + package = "crsra")
    > bn = basename(zip_file)
    > bn = sub("[.]zip$", "", bn)
    > res = unzip(zip_file, exdir = tempdir(), overwrite = TRUE)
    > example_import = crsra_import(workdir = tempdir(),
    + check_problems = FALSE)
    Error: `select_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `select()` instead.
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 500 marked UTF-8 strings
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

# dat

<details>

* Version: 0.5.0
* GitHub: https://github.com/wahani/dat
* Source code: https://github.com/cran/dat
* Date/Publication: 2020-05-15 19:40:03 UTC
* Number of recursive dependencies: 73

Run `cloud_details(, "dat")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Please use `mutate()` instead.
      Backtrace:
          ▆
       1. ├─dat:::WITH_DPLYR(...) at test-mutar.R:123:2
       2. └─dat::mutar(...) at test-mutar.R:131:4
       3.   ├─dat:::handleCols(...)
       4.   └─dat:::handleCols(...)
       5.     └─dplyr::mutate_(x, .dots = args)
       6.       └─dplyr:::lazy_deprec("mutate")
       7.         └─lifecycle::deprecate_stop(...)
       8.           └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 10 | WARN 0 | SKIP 1 | PASS 69 ]
      Error: Test failures
      Execution halted
    ```

# DataCombine

<details>

* Version: 0.2.21
* GitHub: https://github.com/christophergandrud/DataCombine
* Source code: https://github.com/cran/DataCombine
* Date/Publication: 2016-04-13 17:59:09
* Number of recursive dependencies: 99

Run `cloud_details(, "DataCombine")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘DataCombine-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: CasesTable
    > ### Title: Create reports cases after listwise deletion of missing values
    > ###   for time-series cross-sectional data.
    > ### Aliases: CasesTable
    > 
    > ### ** Examples
    > 
    ...
    > 
    > # Find cases that have not been listwise deleted
    > CasesTable(Data, GroupVar = 'ID')
    [1] 1 3
    > CasesTable(Data, GroupVar = 'ID', Vars = 'a')
    [1] 1 2 3
    > CasesTable(Data, GroupVar = 'ID', TimeVar = 'time', Vars = 'a')
    Error: `group_by_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `group_by()` instead.
    Execution halted
    ```

# dataCompareR

<details>

* Version: 0.1.4
* GitHub: https://github.com/capitalone/dataCompareR
* Source code: https://github.com/cran/dataCompareR
* Date/Publication: 2021-11-23 05:20:02 UTC
* Number of recursive dependencies: 58

Run `cloud_details(, "dataCompareR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘dataCompareR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: print.dataCompareRobject
    > ### Title: Printing RCompare Output
    > ### Aliases: print.dataCompareRobject
    > 
    > ### ** Examples
    > 
    > rc1 <- rCompare(iris,iris)
    Running rCompare...
    Error: `select_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `select()` instead.
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
           ▆
        1. ├─dataCompareR::rCompare(pressure, pressure2) at testcreateReportText.R:158:2
        2. │ └─dataCompareR:::processFlow(...)
        3. │   └─dataCompareR:::prepareData(dfa, dfb, keys, trimChars)
        4. │     └─dataCompareR:::matchColumns(dfA, dfB)
        5. │       └─dataCompareR:::subsetDataColumns(DFA, DFB, colInfoList)
        6. │         └─DFA %>% select_(.dots = colInfoList[["commonCols"]])
        7. └─dplyr::select_(., .dots = colInfoList[["commonCols"]])
        8.   └─dplyr:::lazy_deprec("select")
        9.     └─lifecycle::deprecate_stop(...)
       10.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 70 | WARN 0 | SKIP 3 | PASS 180 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘dataCompareR.Rmd’ using rmarkdown
    Quitting from lines 61-63 (dataCompareR.Rmd) 
    Error: processing vignette 'dataCompareR.Rmd' failed with diagnostics:
    `select_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `select()` instead.
    --- failed re-building ‘dataCompareR.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘dataCompareR.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# ddpcr

<details>

* Version: 1.15
* GitHub: https://github.com/daattali/ddpcr
* Source code: https://github.com/cran/ddpcr
* Date/Publication: 2020-06-02 07:10:07 UTC
* Number of recursive dependencies: 101

Run `cloud_details(, "ddpcr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ddpcr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: calculate_neg_freq_single
    > ### Title: Calculate negative frequency of a single well
    > ### Aliases: calculate_neg_freq_single
    > ### Keywords: internal
    > 
    > ### ** Examples
    > 
    > file <- system.file("sample_data", "small", "analyzed_pnpp.rds", package = "ddpcr")
    > plate <- load_plate(file)
    > plate %>% calculate_neg_freq_single("A05")
    Error: `select_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `select()` instead.
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Backtrace:
          ▆
       1. └─ddpcr::new_plate(dir) at test-v174.R:5:2
       2.   └─ddpcr:::read_plate(plate, dir, data_files, meta_file)
       3.     └─ddpcr:::read_dir(plate, dir)
       4.       └─ddpcr:::read_files(plate, data_files, meta_file)
       5.         └─base::tryCatch(...)
       6.           └─base (local) tryCatchList(expr, classes, parentenv, handlers)
       7.             └─base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
       8.               └─value[[3L]](cond)
       9.                 └─ddpcr::err_msg("there was a problem reading one or more of the data files")
      
      [ FAIL 36 | WARN 0 | SKIP 0 | PASS 106 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘algorithm.Rmd’ using rmarkdown
    --- finished re-building ‘algorithm.Rmd’
    
    --- re-building ‘extend.Rmd’ using rmarkdown
    Quitting from lines 205-213 (extend.Rmd) 
    Error: processing vignette 'extend.Rmd' failed with diagnostics:
    ddpcr: there was a problem reading one or more of the data files
    --- failed re-building ‘extend.Rmd’
    
    ...
    --- failed re-building ‘overview.Rmd’
    
    --- re-building ‘technical_details.Rmd’ using rmarkdown
    --- finished re-building ‘technical_details.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘extend.Rmd’ ‘overview.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
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

# dmutate

<details>

* Version: 0.1.3
* GitHub: https://github.com/kylebaron/dmutate
* Source code: https://github.com/cran/dmutate
* Date/Publication: 2021-04-22 22:20:03 UTC
* Number of recursive dependencies: 36

Run `cloud_details(, "dmutate")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘dmutate-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: mutate_random
    > ### Title: Add random variates to a data frame.
    > ### Aliases: mutate_random mutate_random,data.frame,formula-method
    > ###   mutate_random,data.frame,character-method
    > ###   mutate_random,data.frame,list-method
    > ###   mutate_random,data.frame,covset-method
    > ###   mutate_random,data.frame,covobj-method
    ...
    5   5     1 80.18040
    6   6     3 57.33631
    7   7     3 43.21467
    8   8     2 40.26398
    9   9     2 54.71164
    10 10     3 40.52692
    > mutate_random(data, RE ~ rbeta(1,1) | GROUP)
    Error: `distinct_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `distinct()` instead.
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Backtrace:
       1. dmutate::mutate_random(idata, cov1, envir = En)
            at test-dmutate.R:17:2
       2. dmutate::mutate_random(idata, cov1, envir = En)
       3. dmutate:::apply_covset(data, input, ...)
       4. dmutate:::do_mutate(data, .covset[[i]], ...)
       6. dplyr::mutate_(data, .dots = .dots)
       7. dplyr:::lazy_deprec("mutate")
       8. lifecycle::deprecate_stop(...)
       9. lifecycle:::deprecate_stop0(msg)
      
      ══ DONE ════════════════════════════════════════════════════════════════════════
      Frustration is a natural part of programming :)
      Error: Test failures
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

# eesim

<details>

* Version: 0.1.0
* GitHub: https://github.com/sakoehler7/eesim
* Source code: https://github.com/cran/eesim
* Date/Publication: 2017-06-03 17:55:52 UTC
* Number of recursive dependencies: 61

Run `cloud_details(, "eesim")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘eesim-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: calendar_plot
    > ### Title: Create calendar plot
    > ### Aliases: calendar_plot
    > 
    > ### ** Examples
    > 
    > testdat <- sim_exposure(n = 1000, central = 0.1,
    +            exposure_type = "binary")
    > testdat$x[c(89,101,367,500,502,598,678,700,895)] <- 3
    > calendar_plot(testdat, type = "discrete", labels = c("no", "yes", "maybe"))
    Error: `mutate_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `mutate()` instead.
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘eesim.Rmd’ using rmarkdown
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    ...
    Error: processing vignette 'eesim.Rmd' failed with diagnostics:
    `mutate_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `mutate()` instead.
    --- failed re-building ‘eesim.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘eesim.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# emuR

<details>

* Version: 2.3.0
* GitHub: https://github.com/IPS-LMU/emuR
* Source code: https://github.com/cran/emuR
* Date/Publication: 2021-06-11 14:50:01 UTC
* Number of recursive dependencies: 102

Run `cloud_details(, "emuR")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        2. │ └─testthat:::quasi_capture(enquo(object), label, capture_warnings)
        3. │   ├─testthat (local) .capture(...)
        4. │   │ └─base::withCallingHandlers(...)
        5. │   └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
        6. └─emuR:::check_emuRsegsForRequery(sl)
        7.   └─dplyr::arrange_(sl_df, "session", "bundle", "sample_start")
        8.     └─dplyr:::lazy_deprec("arrange")
        9.       └─lifecycle::deprecate_stop(...)
       10.         └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 1 | WARN 220 | SKIP 37 | PASS 845 ]
      Error: Test failures
      In addition: Warning message:
      call dbDisconnect() when finished working with a connection 
      Execution halted
    ```

# eoffice

<details>

* Version: 0.2.1
* GitHub: NA
* Source code: https://github.com/cran/eoffice
* Date/Publication: 2020-11-18 21:40:02 UTC
* Number of recursive dependencies: 90

Run `cloud_details(, "eoffice")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘eoffice-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: indocx
    > ### Title: read table from docx
    > ### Aliases: indocx
    > 
    > ### ** Examples
    > 
    > totable(t.test(wt ~ am, mtcars), filename = file.path(tempdir(), "mtcars.docx"))
    > tabs <- indocx(filename = file.path(tempdir(), "mtcars.docx"), header = TRUE)
    Error: `filter_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `filter()` instead.
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘eoffice.Rmd’ using knitr
    Quitting from lines 63-94 (eoffice.Rmd) 
    Error: processing vignette 'eoffice.Rmd' failed with diagnostics:
    `filter_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `filter()` instead.
    --- failed re-building ‘eoffice.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘eoffice.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# EpiSignalDetection

<details>

* Version: 0.1.2
* GitHub: https://github.com/EU-ECDC/EpiSignalDetection
* Source code: https://github.com/cran/EpiSignalDetection
* Date/Publication: 2021-11-30 15:00:02 UTC
* Number of recursive dependencies: 79

Run `cloud_details(, "EpiSignalDetection")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘EpiSignalDetection-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: aggAtlasExport
    > ### Title: Aggregate filtered final Atlas export
    > ### Aliases: aggAtlasExport
    > 
    > ### ** Examples
    > 
    > #-- Setting the parameters to run the report for
    ...
    > dataset <- EpiSignalDetection::SignalData
    > 
    > #-- Filtering on declared input parameters
    > dataset <- filterAtlasExport(dataset, input)
    > 
    > #-- Aggregating the data by geographical level and time point
    > dataset <- aggAtlasExport(dataset, input)
    Error: `group_by_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `group_by()` instead.
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘EpiSignalDetection_Vignette.Rmd’ using rmarkdown
    Quitting from lines 180-191 (EpiSignalDetection_Vignette.Rmd) 
    Error: processing vignette 'EpiSignalDetection_Vignette.Rmd' failed with diagnostics:
    `group_by_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `group_by()` instead.
    --- failed re-building ‘EpiSignalDetection_Vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘EpiSignalDetection_Vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
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

# ESTER

<details>

* Version: 0.2.0
* GitHub: https://github.com/lnalborczyk/ESTER
* Source code: https://github.com/cran/ESTER
* Date/Publication: 2017-12-10 14:21:14 UTC
* Number of recursive dependencies: 137

Run `cloud_details(, "ESTER")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘ESTER.Rmd’ using rmarkdown
    Quitting from lines 95-102 (ESTER.Rmd) 
    Error: processing vignette 'ESTER.Rmd' failed with diagnostics:
    `mutate_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `mutate()` instead.
    --- failed re-building ‘ESTER.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘ESTER.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
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
    
    > ### Name: grp_routine
    > ### Title: Mutate a character/factor based on conditions.
    > ### Aliases: grp_routine grp_routine_
    > 
    > ### ** Examples
    > 
    > df <- data.frame(v1 = letters[1:5], v2 = 1:5)
    ...
    5  e  5
    > 
    > # By default, it creates new groups
    > grp_routine(df, "group",
    +                first = v1 %in% c("a", "b"),
    +                second = v2 == 3,
    +                third = v2 >= 4)
    Error: `mutate_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `mutate()` instead.
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
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

# eyetrackingR

<details>

* Version: 0.2.0
* GitHub: https://github.com/samhforbes/eyetrackingR
* Source code: https://github.com/cran/eyetrackingR
* Date/Publication: 2021-09-27 10:00:14 UTC
* Number of recursive dependencies: 95

Run `cloud_details(, "eyetrackingR")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘divergence_vignette.Rmd’ using rmarkdown
    Warning: Removed 37 rows containing non-finite values (stat_summary).
    Warning: Removed 37 rows containing non-finite values (stat_summary).
    Warning: `gather_()` was deprecated in tidyr 1.2.0.
    Please use `gather()` instead.
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.
    --- finished re-building ‘divergence_vignette.Rmd’
    
    ...
    --- re-building ‘window_analysis_vignette.Rmd’ using rmarkdown
    Warning: `fun.y` is deprecated. Use `fun` instead.
    Warning: `fun.y` is deprecated. Use `fun` instead.
    --- finished re-building ‘window_analysis_vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘growth_curve_analysis_vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘rlang’
      All declared Imports should be used.
    ```

# ezec

<details>

* Version: 1.0.1
* GitHub: https://github.com/grunwaldlab/ezec
* Source code: https://github.com/cran/ezec
* Date/Publication: 2016-12-05 08:27:32
* Number of recursive dependencies: 105

Run `cloud_details(, "ezec")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ezec-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: EC_table
    > ### Title: Function to generate a table of EC values from a data frame of
    > ###   multiple isolates.
    > ### Aliases: EC_table
    > 
    > ### ** Examples
    > 
    > data(dummydata)
    > # Using 3 parameter Log-Logistic Model (default)
    > EC_table(dummydata, form = response ~ dose)
    Error: `do_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `do()` instead.
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        5. │   │   └─testthat:::eval_with_output(code, print = print, width = width)
        6. │   │     ├─withr::with_output_sink(path, withVisible(code))
        7. │   │     │ └─base::force(code)
        8. │   │     └─base::withVisible(code)
        9. │   └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
       10. ├─ezec::EC_table(...)
       11. │ └─dat %>% dplyr::group_by_(idcol) %>% ...
       12. └─dplyr::do_(...)
       13.   └─dplyr:::lazy_deprec("do")
       14.     └─lifecycle::deprecate_stop(...)
       15.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 2 | WARN 0 | SKIP 1 | PASS 2 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘getting_started.Rmd’ using rmarkdown
    Quitting from lines 77-81 (getting_started.Rmd) 
    Error: processing vignette 'getting_started.Rmd' failed with diagnostics:
    `do_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `do()` instead.
    --- failed re-building ‘getting_started.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘getting_started.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# factorMerger

<details>

* Version: 0.4.0
* GitHub: https://github.com/MI2DataLab/factorMerger
* Source code: https://github.com/cran/factorMerger
* Date/Publication: 2019-07-03 22:50:26 UTC
* Number of recursive dependencies: 160

Run `cloud_details(, "factorMerger")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘factorMerger-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: groupsStats
    > ### Title: Groups statistic
    > ### Aliases: groupsStats
    > 
    > ### ** Examples
    > 
    > randSample <- generateMultivariateSample(N = 100, k = 10, d = 3)
    > fm <- mergeFactors(randSample$response, randSample$factor)
    Error: `arrange_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `arrange()` instead.
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
           ▆
        1. ├─factorMerger::mergeFactors(dfWithoutCovariates$response, dfWithoutCovariates$factor) at testvisualizations.R:4:0
        2. ├─factorMerger:::mergeFactors.default(...)
        3. │ └─factorMerger:::mergeLRT(fm, successive)
        4. │   └─factorMerger:::startMerging(factorMerger, successive, "LRT")
        5. │     └─factorMerger:::getIncreasingFactor(factorMerger)
        6. │       └─stats %>% arrange_("stat")
        7. └─dplyr::arrange_(., "stat")
        8.   └─dplyr:::lazy_deprec("arrange")
        9.     └─lifecycle::deprecate_stop(...)
       10.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 4 | WARN 0 | SKIP 0 | PASS 5 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘brca.Rmd’ using rmarkdown
    Welcome to factorMerger 0.4.0!
    
    Loading required package: forcats
    Quitting from lines 51-56 (brca.Rmd) 
    Error: processing vignette 'brca.Rmd' failed with diagnostics:
    `arrange_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `arrange()` instead.
    --- failed re-building ‘brca.Rmd’
    ...
    Error: processing vignette 'pisa2012.Rmd' failed with diagnostics:
    `arrange_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `arrange()` instead.
    --- failed re-building ‘pisa2012.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘brca.Rmd’ ‘factorMerger.Rmd’ ‘pisa2012.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘forcats’ ‘formula.tools’
      All declared Imports should be used.
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
    Rows: 0 Columns: 2
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: "\t"
    chr (2): Total Deduplicated Percentage, 81.85424272012848
    
    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    Error: `select_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `select()` instead.
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
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

# funModeling

<details>

* Version: 1.9.4
* GitHub: https://github.com/pablo14/funModeling
* Source code: https://github.com/cran/funModeling
* Date/Publication: 2020-06-15 05:10:02 UTC
* Number of recursive dependencies: 88

Run `cloud_details(, "funModeling")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘funModeling-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: categ_analysis
    > ### Title: Profiling analysis of categorical vs. target variable
    > ### Aliases: categ_analysis
    > 
    > ### ** Examples
    > 
    > categ_analysis(data_country, "country", "has_flu")
    Error: `summarise_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `summarise()` instead.
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
    Error: processing vignette 'funModeling_quickstart.Rmd' failed with diagnostics:
    `summarise_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `summarise()` instead.
    --- failed re-building ‘funModeling_quickstart.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘funModeling_quickstart.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# futureheatwaves

<details>

* Version: 1.0.3
* GitHub: NA
* Source code: https://github.com/cran/futureheatwaves
* Date/Publication: 2016-12-31 08:44:48
* Number of recursive dependencies: 88

Run `cloud_details(, "futureheatwaves")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘futureheatwaves-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: apply_all_models
    > ### Title: Apply a function across heat waves from all projections
    > ### Aliases: apply_all_models
    > 
    > ### ** Examples
    > 
    > ex_results <- system.file("extdata/example_results",
    +                           package = "futureheatwaves")
    > apply_all_models(ex_results, FUN = average_mean_temp)
    Error: `mutate_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `mutate()` instead.
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      <lifecycle_error_deprecated/defunctError/error/condition/condition>
      Error: `select_()` was deprecated in dplyr 0.7.0 and is now defunct.
      Please use `select()` instead.
      Backtrace:
          ▆
       1. ├─futureheatwaves:::acquireDirectoryStructure(...) at test_preparation.R:13:0
       2. │ └─... %>% dplyr::select_(~exp, ~model, ~ens, ~type)
       3. └─dplyr::select_(., ~exp, ~model, ~ens, ~type)
       4.   └─dplyr:::lazy_deprec("select")
       5.     └─lifecycle::deprecate_stop(...)
       6.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 6 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘futureheatwaves.Rmd’ using rmarkdown
    Quitting from lines 326-328 (futureheatwaves.Rmd) 
    Error: processing vignette 'futureheatwaves.Rmd' failed with diagnostics:
    `mutate_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `mutate()` instead.
    --- failed re-building ‘futureheatwaves.Rmd’
    
    --- re-building ‘starting_from_netcdf.Rmd’ using rmarkdown
    --- finished re-building ‘starting_from_netcdf.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘futureheatwaves.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        doc       1.8Mb
        extdata   1.6Mb
        libs      1.6Mb
    ```

# GADMTools

<details>

* Version: 3.9-1
* GitHub: https://github.com/IamKDO/GADMTools
* Source code: https://github.com/cran/GADMTools
* Date/Publication: 2021-08-05 00:10:02 UTC
* Number of recursive dependencies: 115

Run `cloud_details(, "GADMTools")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘GADMTools-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: gadm_union
    > ### Title: Merges regions
    > ### Aliases: gadm_union gadm.union
    > ### Keywords: ~documentation ~utilities
    > 
    > ### ** Examples
    > 
    ...
    old-style crs object detected; please recreate object with a recent sf::st_crs()
    old-style crs object detected; please recreate object with a recent sf::st_crs()
    > 
    > Corse <- gadm_union(Corsica, level=2)
    old-style crs object detected; please recreate object with a recent sf::st_crs()
    old-style crs object detected; please recreate object with a recent sf::st_crs()
    old-style crs object detected; please recreate object with a recent sf::st_crs()
    Error: `select_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `select()` instead.
    Execution halted
    ```

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘GADMTools_GRAPHICS.Rmd’ using rmarkdown
    Loading required package: sp
    Loading required package: classInt
    Loading required package: sf
    Linking to GEOS 3.8.0, GDAL 3.0.4, PROJ 6.3.1; sf_use_s2() is TRUE
    Loading required package: rgdal
    Please note that rgdal will be retired by the end of 2023,
    plan transition to sf/stars/terra functions using GDAL and PROJ
    at your earliest convenience.
    ...
    `select_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `select()` instead.
    --- failed re-building ‘GADMTools_Manipulating_Shapefiles.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘GADMTools_ISO_3166-1_alpha-3.Rmd’
      ‘GADMTools_Manipulating_Shapefiles.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 19 marked UTF-8 strings
    ```

# genogeographer

<details>

* Version: 0.1.19
* GitHub: NA
* Source code: https://github.com/cran/genogeographer
* Date/Publication: 2019-09-27 10:20:08 UTC
* Number of recursive dependencies: 132

Run `cloud_details(, "genogeographer")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘genogeographer-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: LR_table
    > ### Title: Compute pairwise likelihood ratios
    > ### Aliases: LR_table
    > 
    > ### ** Examples
    > 
    > df_ <- simulate_pops(pop_n = 4, aims_n = 50)
    > df_db <- pops_to_DB(df_)
    Error: `select_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `select()` instead.
    Execution halted
    ```

# GFE

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/GFE
* Date/Publication: 2018-08-02 12:10:10 UTC
* Number of recursive dependencies: 19

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
    > DBcitaModISam[,Pik := pik]
    > 
    > # Gross flows estimation
    > estima <- estGF(sampleBase = DBcitaModISam, niter = 500, model = "II", colWeights = "Pik")
    > # gross flows variance estimation
    > varEstima <- reSamGF(sampleBase = DBcitaModISam, type = "Bootstrap", nRepBoot = 100,
    + 						model = "II", niter = 101,  colWeights = "Pik")
    Error: `group_by_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `group_by()` instead.
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# ggalluvial

<details>

* Version: 0.12.3
* GitHub: https://github.com/corybrunson/ggalluvial
* Source code: https://github.com/cran/ggalluvial
* Date/Publication: 2020-12-05 16:20:02 UTC
* Number of recursive dependencies: 89

Run `cloud_details(, "ggalluvial")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggalluvial-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: stat_flow
    > ### Title: Flow positions
    > ### Aliases: stat_flow
    > 
    > ### ** Examples
    > 
    > # illustrate positioning
    ...
    +   geom_line(stat = "flow") +
    +   stat_flow(geom = "pointrange") +
    +   geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
    +   scale_x_discrete(limits = c("Class", "Sex", "Age"))
    Warning: Computation failed in `stat_flow()`:
    The `.dots` argument of `group_by()` was deprecated in dplyr 1.0.0 and is now defunct.
    Warning: Computation failed in `stat_flow()`:
    The `.dots` argument of `group_by()` was deprecated in dplyr 1.0.0 and is now defunct.
    Error: Must request at least one colour from a hue palette.
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Backtrace:
          ▆
       1. └─StatFlow$compute_panel(data) at test-stat-flow.r:78:2
       2.   └─ggalluvial (local) f(..., self = self)
       3.     ├─dplyr::group_by(data, .dots = by_vars)
       4.     └─dplyr:::group_by.data.frame(data, .dots = by_vars)
       5.       └─dplyr::group_by_prepare(.data, ..., .add = .add, caller_env = caller_env())
       6.         └─lifecycle::deprecate_stop("1.0.0", "group_by(.dots = )")
       7.           └─lifecycle:::deprecate_stop0(msg)
      ── Failure (test-stat-flow.r:90:3): `stat_flow` handles exceptional data with out errors ──
      `ggplot_build(gg)` produced warnings.
      
      [ FAIL 5 | WARN 1 | SKIP 7 | PASS 47 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘ggalluvial.rmd’ using rmarkdown
    Loading required package: ggplot2
    --- finished re-building ‘ggalluvial.rmd’
    
    --- re-building ‘labels.rmd’ using rmarkdown
    --- finished re-building ‘labels.rmd’
    
    --- re-building ‘order-rectangles.rmd’ using rmarkdown
    ...
    --- failed re-building ‘order-rectangles.rmd’
    
    --- re-building ‘shiny.Rmd’ using rmarkdown
    --- finished re-building ‘shiny.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘order-rectangles.rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# ggedit

<details>

* Version: 0.3.1
* GitHub: https://github.com/yonicd/ggedit
* Source code: https://github.com/cran/ggedit
* Date/Publication: 2020-06-02 11:50:06 UTC
* Number of recursive dependencies: 88

Run `cloud_details(, "ggedit")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggedit-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: compare
    > ### Title: compare
    > ### Aliases: compare
    > 
    > ### ** Examples
    > 
    > compare(ggplot2::theme_bw(),ggplot2::theme_get())
    ...
    The signature and semantics have changed, see `?as_tibble`.
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.
    Warning: Each row in `x` should match at most 1 row in `y`.
    ℹ Row 1 of `x` matches multiple rows.
    ℹ If multiple matches are expected, specify `multiple = "all"` in the join call
      to silence this warning.
    Error: `summarise_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `summarise()` instead.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘magrittr’
      All declared Imports should be used.
    ```

# ggfocus

<details>

* Version: 1.0.0
* GitHub: https://github.com/Freguglia/ggfocus
* Source code: https://github.com/cran/ggfocus
* Date/Publication: 2020-01-23 13:20:02 UTC
* Number of recursive dependencies: 57

Run `cloud_details(, "ggfocus")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggfocus-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggfocus
    > ### Title: (Deprecated) Sets focus scales to an existing 'ggplot' object
    > ### Aliases: ggfocus
    > 
    > ### ** Examples
    > 
    > library(ggplot2)
    > p <- ggplot(iris,aes(x=Sepal.Length,y=Petal.Length)) + geom_point()
    > ggfocus(p, Species, "versicolor")
    The function 'ggfocus()' is deprecated, consider using the family scale_*_focus() instead.
    Error: `select_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `select()` instead.
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# ggfortify

<details>

* Version: 0.4.14
* GitHub: https://github.com/sinhrks/ggfortify
* Source code: https://github.com/cran/ggfortify
* Date/Publication: 2022-01-03 15:30:02 UTC
* Number of recursive dependencies: 122

Run `cloud_details(, "ggfortify")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggfortify-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: autoplot.tsmodel
    > ### Title: Autoplot time series models (like AR, ARIMA)
    > ### Aliases: autoplot.tsmodel autoplot.ar autoplot.fracdiff autoplot.nnetar
    > ###   autoplot.HoltWinters autoplot.fGARCH
    > 
    > ### ** Examples
    > 
    ...
    > autoplot(forecast::nnetar(UKgas), is.date = FALSE)
    Warning: Removed 4 row(s) containing missing values (geom_path).
    > 
    > d.holt <- stats::HoltWinters(USAccDeaths)
    > autoplot(d.holt)
    Warning: Removed 12 row(s) containing missing values (geom_path).
    > autoplot(d.holt, predict = predict(d.holt, n.ahead = 5))
    Error: `filter_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `filter()` instead.
    Execution halted
    ```

# ggmuller

<details>

* Version: 0.5.4
* GitHub: NA
* Source code: https://github.com/cran/ggmuller
* Date/Publication: 2019-09-05 02:10:17 UTC
* Number of recursive dependencies: 56

Run `cloud_details(, "ggmuller")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggmuller-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: Muller_plot
    > ### Title: Draw a Muller plot of frequencies using ggplot2
    > ### Aliases: Muller_plot
    > 
    > ### ** Examples
    > 
    > # include all genotypes:
    > Muller_df1 <- get_Muller_df(example_edges, example_pop_df)
    Error: `filter_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `filter()` instead.
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘ggmuller.Rmd’ using rmarkdown
    Quitting from lines 26-29 (ggmuller.Rmd) 
    Error: processing vignette 'ggmuller.Rmd' failed with diagnostics:
    `filter_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `filter()` instead.
    --- failed re-building ‘ggmuller.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘ggmuller.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# gmgm

<details>

* Version: 1.1.1
* GitHub: NA
* Source code: https://github.com/cran/gmgm
* Date/Publication: 2022-05-27 18:40:02 UTC
* Number of recursive dependencies: 60

Run `cloud_details(, "gmgm")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       21.         └─vctrs:::vec_cast.integer.double(...)
       22.           └─vctrs::maybe_lossy_cast(...)
       23.             ├─base::withRestarts(...)
       24.             │ └─base (local) withOneRestart(expr, restarts[[1L]])
       25.             │   └─base (local) doWithOneRestart(return(expr), restart)
       26.             └─vctrs:::stop_lossy_cast(...)
       27.               └─vctrs::stop_incompatible_cast(...)
       28.                 └─vctrs::stop_incompatible_type(...)
       29.                   └─vctrs:::stop_incompatible(...)
       30.                     └─vctrs:::stop_vctrs(...)
       31.                       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
      
      [ FAIL 57 | WARN 6 | SKIP 0 | PASS 353 ]
      Error: Test failures
      Execution halted
    ```

# graphTweets

<details>

* Version: 0.5.3
* GitHub: https://github.com/JohnCoene/graphTweets
* Source code: https://github.com/cran/graphTweets
* Date/Publication: 2020-01-08 09:00:08 UTC
* Number of recursive dependencies: 54

Run `cloud_details(, "graphTweets")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘graphTweets-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: gt_edges_from_text
    > ### Title: Edges from text
    > ### Aliases: gt_edges_from_text gt_edges_from_text_
    > 
    > ### ** Examples
    > 
    > # simulate dataset
    ...
    +   status_id = c(1, 2),
    +   hashtags = c("rstats", "Python"),
    +   stringsAsFactors = FALSE
    + )
    > 
    > tweets %>% 
    +   gt_edges_from_text(status_id, screen_name, text)
    Error: `select_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `select()` instead.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘utils’
      All declared Imports should be used.
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
# gunsales

<details>

* Version: 0.1.2
* GitHub: NA
* Source code: https://github.com/cran/gunsales
* Date/Publication: 2017-01-30 15:35:35
* Number of recursive dependencies: 58

Run `cloud_details(, "gunsales")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘gunsales.Rmd’ using rmarkdown
    Quitting from lines 33-34 (gunsales.Rmd) 
    Error: processing vignette 'gunsales.Rmd' failed with diagnostics:
    `select_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `select()` instead.
    --- failed re-building ‘gunsales.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘gunsales.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# hablar

<details>

* Version: 0.3.0
* GitHub: https://github.com/davidsjoberg/hablar
* Source code: https://github.com/cran/hablar
* Date/Publication: 2020-03-19 22:40:02 UTC
* Number of recursive dependencies: 90

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
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 416 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test.ifs.R:53:3): if_else_ ─────────────────────────────────────────
      `if_else_(c(T, F, NA), 1, 1L)` did not throw an error.
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 416 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# harmony

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/harmony
* Date/Publication: 2021-06-02 08:10:02 UTC
* Number of recursive dependencies: 155

Run `cloud_details(, "harmony")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘quickstart.Rmd’ using rmarkdown
    Quitting from lines 132-139 (quickstart.Rmd) 
    Error: processing vignette 'quickstart.Rmd' failed with diagnostics:
    `group_by_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `group_by()` instead.
    --- failed re-building ‘quickstart.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘quickstart.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.9Mb
      sub-directories of 1Mb or more:
        doc    1.4Mb
        libs   6.8Mb
    ```

# headliner

<details>

* Version: 0.0.2
* GitHub: https://github.com/rjake/headliner
* Source code: https://github.com/cran/headliner
* Date/Publication: 2022-06-26 23:40:02 UTC
* Number of recursive dependencies: 71

Run `cloud_details(, "headliner")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        5. │   └─dplyr:::filter_rows(.data, ..., caller_env = caller_env())
        6. │     └─dplyr:::filter_eval(dots, mask = mask, error_call = error_call)
        7. │       ├─base::withCallingHandlers(...)
        8. │       └─mask$eval_all_filter(dots, env_filter)
        9. │         └─dplyr (local) eval()
       10. └─base::.handleSimpleError(`<fn>`, "object '' not found", base::quote(eval()))
       11.   └─dplyr (local) h(simpleError(msg, call))
       12.     └─dplyr:::local_error_context(...)
       13.       └─dplyr:::quo_as_label(dots[[.index]])
       14.         └─dplyr:::is_data_pronoun(expr)
       15.           └─rlang::is_call(x, c("[[", "$"))
      
      [ FAIL 3 | WARN 0 | SKIP 0 | PASS 101 ]
      Error: Test failures
      Execution halted
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
        intersect, setdiff, setequal, union
    
    > data <- iris %>% select(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
    > features <- colnames(data)
    > result <- kmeans_procedure(data = data, columns = features, threshold_min = 2, threshold = 10,
    + verbose=FALSE, seed=10)
    Error in `$<-.data.frame`(`*tmp*`, "call_text", value = c("helda::kmeans_procedure(...)",  : 
      replacement has 11 rows, data has 10
    Calls: kmeans_procedure ... trace_format -> trace_as_tree -> $<- -> $<-.data.frame
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked UTF-8 string
    ```

# hlaR

<details>

* Version: 0.1.3
* GitHub: https://github.com/LarsenLab/hlaR
* Source code: https://github.com/cran/hlaR
* Date/Publication: 2022-05-13 21:00:02 UTC
* Number of recursive dependencies: 152

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
     10. └─dplyr (local) `<fn>`(index, "")
     11.   └─vctrs::vec_cast(x = y, to = x, x_arg = "y", to_arg = "x")
     12.     └─vctrs (local) `<fn>`()
     13.       └─vctrs::vec_default_cast(...)
     14.         └─vctrs::stop_incompatible_cast(...)
     15.           └─vctrs::stop_incompatible_type(...)
     16.             └─vctrs:::stop_incompatible(...)
     17.               └─vctrs:::stop_vctrs(...)
     18.                 └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘allele-haplotype.Rmd’ using rmarkdown
    ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ✔ ggplot2 3.3.6           ✔ dplyr   1.0.99.9000
    ✔ tibble  3.1.8           ✔ stringr 1.4.0      
    ✔ tidyr   1.2.0           ✔ forcats 0.5.1      
    ✔ purrr   0.3.4           
    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    ...
    Problem while computing `index = (function (x, y) ...`.
    Caused by error:
    ! Can't convert `y` <character> to match type of `x` <integer>.
    --- failed re-building ‘eplet-mm.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘eplet-mm.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘here’ ‘readxlsb’ ‘usethis’
      All declared Imports should be used.
    ```

# HTSSIP

<details>

* Version: 1.4.1
* GitHub: NA
* Source code: https://github.com/cran/HTSSIP
* Date/Publication: 2019-09-13 22:30:02 UTC
* Number of recursive dependencies: 140

Run `cloud_details(, "HTSSIP")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘HTSSIP-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: get_treatment_params
    > ### Title: Get parameters for subsetting the phyloseq dataset
    > ### Aliases: get_treatment_params
    > 
    > ### ** Examples
    > 
    > data(physeq_S2D2)
    > # Here, the treatment/controls (12C & 13C) are listed in substrate,
    > # and should be matched by 'Day'. The 13C-treatments can be identified by
    > # the expression: "Substrate != '12C-Con'"
    > get_treatment_params(physeq_S2D2, c('Substrate', 'Day'), "Substrate != '12C-Con'")
    Error: `filter_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `filter()` instead.
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Please use `mutate()` instead.
      Backtrace:
          ▆
       1. ├─HTSSIP::qSIP_atom_excess(...) at test-qSIP_atom_excess.R:36:2
       2. │ └─HTSSIP:::qSIP_atom_excess_format(physeq, control_expr, treatment_rep)
       3. │   └─HTSSIP::phyloseq2table(...)
       4. │     └─df_meta %>% dplyr::mutate_(IS_CONTROL = control_expr)
       5. └─dplyr::mutate_(., IS_CONTROL = control_expr)
       6.   └─dplyr:::lazy_deprec("mutate")
       7.     └─lifecycle::deprecate_stop(...)
       8.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 11 | WARN 1 | SKIP 17 | PASS 79 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘BD_shifts.Rmd’ using rmarkdown
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    ...
    Please use `mutate()` instead.
    --- failed re-building ‘quant_incorp.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘BD_shifts.Rmd’ ‘HTSSIP_intro.Rmd’ ‘HTSSIP_sim.Rmd’ ‘MW_HR_SIP.Rmd’
      ‘beta_diversity_ordinations.Rmd’ ‘heavy_SIP.Rmd’ ‘qSIP.Rmd’
      ‘quant_incorp.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘igraph’
      All declared Imports should be used.
    ```

# huito

<details>

* Version: 0.2.0
* GitHub: https://github.com/flavjack/huito
* Source code: https://github.com/cran/huito
* Date/Publication: 2022-06-24 10:40:02 UTC
* Number of recursive dependencies: 159

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
    
    SUMMARY: processing the following files failed:
      ‘huito.Rmd’ ‘labels.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# hurricaneexposure

<details>

* Version: 0.1.1
* GitHub: https://github.com/geanders/hurricaneexposure
* Source code: https://github.com/cran/hurricaneexposure
* Date/Publication: 2020-02-13 14:30:02 UTC
* Number of recursive dependencies: 79

Run `cloud_details(, "hurricaneexposure")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘hurricaneexposure-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: map_counties
    > ### Title: Map counties
    > ### Aliases: map_counties
    > 
    > ### ** Examples
    > 
    > # Ensure that data package is available before running the example.
    ...
    + map_counties("Michael-2018", metric = "wind", wind_var = "vmax_gust")
    + map_counties("Michael-2018", metric = "wind", wind_source = "ext_tracks")
    + }
    Warning: `tbl_df()` was deprecated in dplyr 1.0.0.
    Please use `tibble::as_tibble()` instead.
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.
    Error: `mutate_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `mutate()` instead.
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘hurricaneexposure.Rmd’ using rmarkdown
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    ...
    Error: processing vignette 'hurricaneexposure.Rmd' failed with diagnostics:
    `mutate_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `mutate()` instead.
    --- failed re-building ‘hurricaneexposure.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘hurricaneexposure.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘mapproj’
      All declared Imports should be used.
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# huxtable

<details>

* Version: 5.5.0
* GitHub: https://github.com/hughjonesd/huxtable
* Source code: https://github.com/cran/huxtable
* Date/Publication: 2022-06-15 11:30:02 UTC
* Number of recursive dependencies: 165

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
     1. ├─huxtable::map_background_color(...)
     2. │ └─huxtable (local) fn(ht, rc$row, rc$col, current)
     3. │   └─dplyr::case_when(!!!cases)
     4. │     └─dplyr:::case_when_formula_evaluate(...)
     5. │       └─vctrs::vec_size_common(...)
     6. └─vctrs::stop_incompatible_size(...)
     7.   └─vctrs:::stop_incompatible(...)
     8.     └─vctrs:::stop_vctrs(...)
     9.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        1. ├─testthat::expect_equivalent(...) at test-mapping-functions.R:123:2
        2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
        3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
        4. ├─huxtable (local) f(m, 1:3, 1:2, ct)
        5. │ └─dplyr::case_when(!!!cases)
        6. │   └─dplyr:::case_when_formula_evaluate(...)
        7. │     └─vctrs::vec_size_common(...)
        8. └─vctrs::stop_incompatible_size(...)
        9.   └─vctrs:::stop_incompatible(...)
       10.     └─vctrs:::stop_vctrs(...)
       11.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
      
      [ FAIL 3 | WARN 1 | SKIP 25 | PASS 1225 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘design-principles.Rmd’ using rmarkdown
    --- finished re-building ‘design-principles.Rmd’
    
    --- re-building ‘huxreg.Rmd’ using rmarkdown
    --- finished re-building ‘huxreg.Rmd’
    
    --- re-building ‘huxtable.Rmd’ using rmarkdown
    
    Attaching package: 'dplyr'
    ...
    --- failed re-building ‘huxtable.Rmd’
    
    --- re-building ‘themes.Rhtml’ using knitr
    --- finished re-building ‘themes.Rhtml’
    
    SUMMARY: processing the following file failed:
      ‘huxtable.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘R6’ ‘xml2’
      All declared Imports should be used.
    ```

# HydeNet

<details>

* Version: 0.10.11
* GitHub: https://github.com/nutterb/HydeNet
* Source code: https://github.com/cran/HydeNet
* Date/Publication: 2020-07-06 15:20:13 UTC
* Number of recursive dependencies: 103

Run `cloud_details(, "HydeNet")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘HydeNet-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: HydeSim
    > ### Title: Simulated Distributions of a Decision Network
    > ### Aliases: HydeSim HydePosterior
    > 
    > ### ** Examples
    > 
    > data(PE, package="HydeNet")
    ...
    +                      angio | pe + 
    +                      treat | d.dimer*angio + 
    +                      death | pe*treat,
    +                      data = PE) 
    >   
    >                  
    > compiledNet <- compileJagsModel(Net, n.chains=5)
    Error: `summarise_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `summarise()` instead.
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       12. │   └─base::lapply(...)
       13. │     └─HydeNet (local) FUN(X[[i]], ...)
       14. │       ├─base::do.call("cpt", args)
       15. │       ├─HydeNet::cpt(formula = angio ~ pe, data = `<df[,7]>`)
       16. │       └─HydeNet:::cpt.formula(formula = angio ~ pe, data = `<df[,7]>`)
       17. │         └─HydeNet:::cpt_workhorse(...)
       18. │           └─data %>% dplyr::group_by_(.dots = ..vars) %>% ...
       19. └─dplyr::summarise_(., wt = ~sum(wt))
       20.   └─dplyr:::lazy_deprec("summarise")
       21.     └─lifecycle::deprecate_stop(...)
       22.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 12 | WARN 0 | SKIP 0 | PASS 49 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘DecisionNetworks.Rmd’ using rmarkdown
    Loading required package: nnet
    Quitting from lines 227-233 (DecisionNetworks.Rmd) 
    Error: processing vignette 'DecisionNetworks.Rmd' failed with diagnostics:
    `summarise_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `summarise()` instead.
    --- failed re-building ‘DecisionNetworks.Rmd’
    
    --- re-building ‘GettingStartedWithHydeNet.Rmd’ using rmarkdown
    ...
    Error: processing vignette 'WorkingWithHydeNetObjects.Rmd' failed with diagnostics:
    `summarise_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `summarise()` instead.
    --- failed re-building ‘WorkingWithHydeNetObjects.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘DecisionNetworks.Rmd’ ‘WorkingWithHydeNetObjects.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘graph’
      All declared Imports should be used.
    ```

# iadf

<details>

* Version: 0.1.2
* GitHub: https://github.com/konradmayer/iadf
* Source code: https://github.com/cran/iadf
* Date/Publication: 2021-05-24 15:40:02 UTC
* Number of recursive dependencies: 70

Run `cloud_details(, "iadf")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘iadf-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: campelo_chapman
    > ### Title: campelo_chapman
    > ### Aliases: campelo_chapman
    > 
    > ### ** Examples
    > 
    > data('example_iadf')
    > data('example_rwl')
    > model <- campelo_chapman(campelo_freq(example_iadf, example_rwl))
    > campelo_index(example_iadf, example_rwl, model)
    Error: `select_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `select()` instead.
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘falsering-proportion.Rmd’ using rmarkdown
    Quitting from lines 140-141 (falsering-proportion.Rmd) 
    Error: processing vignette 'falsering-proportion.Rmd' failed with diagnostics:
    `select_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `select()` instead.
    --- failed re-building ‘falsering-proportion.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘falsering-proportion.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# IAT

<details>

* Version: 0.3
* GitHub: NA
* Source code: https://github.com/cran/IAT
* Date/Publication: 2016-04-30 00:51:43
* Number of recursive dependencies: 35

Run `cloud_details(, "IAT")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘IAT-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: cleanIAT
    > ### Title: Clean IAT data using the updated D-Scoring algorithm
    > ### Aliases: cleanIAT
    > 
    > ### ** Examples
    > 
    > # Get Ps who recieve Math-Male sorting task in first blocks
    ...
    > dscore_first <- cleanIAT(my_data = cong_first,
    +                          block_name = "BLOCK_NAME_S",
    +                          trial_blocks = c("BLOCK2", "BLOCK3", "BLOCK5", "BLOCK6"),
    +                          session_id = "SESSION_ID",
    +                          trial_latency = "TRIAL_LATENCY",
    +                          trial_error = "TRIAL_ERROR",
    +                          v_error = 1, v_extreme = 2, v_std = 1)
    Error: `filter_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `filter()` instead.
    Execution halted
    ```

# idmodelr

<details>

* Version: 0.3.2
* GitHub: https://github.com/seabbs/idmodelr
* Source code: https://github.com/cran/idmodelr
* Date/Publication: 2020-06-11 14:20:03 UTC
* Number of recursive dependencies: 107

Run `cloud_details(, "idmodelr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘idmodelr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: model_df_to_vector
    > ### Title: Extracts a Single Column, Summarises if from Simulation
    > ### Aliases: model_df_to_vector
    > 
    > ### ** Examples
    > 
    > library(dplyr)
    ...
    [127] 4.8 4.9 5.6 5.8 6.1 6.4 5.6 5.1 5.6 6.1 5.6 5.5 4.8 5.4 5.6 5.1 5.1 5.9
    [145] 5.7 5.2 5.0 5.2 5.4 5.1
    > 
    > ## Extract a vector and summarise
    > df <- bind_rows(iris %>% mutate(sim = 1, id = 1:length(sim)),
    +  iris %>% mutate(sim = 2 , id = 1:length(sim)))
    > 
    > model_df_to_vector(df, Petal.Length, "id", sum_fn = mean)
    Error: The `.dots` argument of `group_by()` was deprecated in dplyr 1.0.0 and is now defunct.
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Backtrace:
          ▆
       1. ├─idmodelr::scenario_analysis(...) at test-scenario_analysis.R:45:12
       2. │ └─... %>% rename(parameters = data)
       3. ├─dplyr::rename(., parameters = data)
       4. ├─tidyr::nest(.)
       5. ├─dplyr::group_by(., .dots = group_var_string)
       6. └─dplyr:::group_by.data.frame(., .dots = group_var_string)
       7.   └─dplyr::group_by_prepare(.data, ..., .add = .add, caller_env = caller_env())
       8.     └─lifecycle::deprecate_stop("1.0.0", "group_by(.dots = )")
       9.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 3 | WARN 13 | SKIP 41 | PASS 45 ]
      Error: Test failures
      Execution halted
    ```

# ipft

<details>

* Version: 0.7.2
* GitHub: NA
* Source code: https://github.com/cran/ipft
* Date/Publication: 2018-01-04 09:36:52 UTC
* Number of recursive dependencies: 37

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
    >     ipfPlotLocation(ipftrain[, 169:170])
    > 
    >     ipfPlotLocation(ipftrain[, 169:170], plabel = TRUE, reverseAxis = TRUE,
    +                     title = 'Position of training set observations')
    Error: `group_indices_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `group_indices()` instead.
    Execution halted
    ```

# itraxR

<details>

* Version: 1.4
* GitHub: https://github.com/tombishop1/itraxR
* Source code: https://github.com/cran/itraxR
* Date/Publication: 2021-08-17 09:10:02 UTC
* Number of recursive dependencies: 66

Run `cloud_details(, "itraxR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘itraxR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: itrax_import
    > ### Title: Import Itrax core-scanner result file
    > ### Aliases: itrax_import
    > 
    > ### ** Examples
    > 
    > itrax_import(
    ...
     15. └─dplyr::na_if(., 0)
     16.   └─vctrs::vec_cast(x = y, to = x, x_arg = "y", to_arg = "x")
     17.     └─vctrs (local) `<fn>`()
     18.       └─vctrs::vec_default_cast(...)
     19.         └─vctrs::stop_incompatible_cast(...)
     20.           └─vctrs::stop_incompatible_type(...)
     21.             └─vctrs:::stop_incompatible(...)
     22.               └─vctrs:::stop_vctrs(...)
     23.                 └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
    Execution halted
    ```

# janitor

<details>

* Version: 2.1.0
* GitHub: https://github.com/sfirke/janitor
* Source code: https://github.com/cran/janitor
* Date/Publication: 2021-01-05 01:10:04 UTC
* Number of recursive dependencies: 72

Run `cloud_details(, "janitor")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error (test-adorn-percentages.R:69:3): works with totals row/col when denominator = col or all, #357 ──
      Error in `FUN(X[[i]], ...)`: only defined on a data frame with all numeric-alike variables
      Backtrace:
          ▆
       1. ├─source1 %>% adorn_totals(where = c("col", "row")) %>% ... at test-adorn-percentages.R:69:2
       2. └─janitor::adorn_percentages(., denominator = "col")
       3.   └─base::Summary.data.frame(`<tabyl[,3]>`, na.rm = FALSE)
       4.     └─base::lapply(...)
       5.       └─base (local) FUN(X[[i]], ...)
      
      [ FAIL 1 | WARN 0 | SKIP 1 | PASS 639 ]
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

# kernelPhil

<details>

* Version: 0.1
* GitHub: NA
* Source code: https://github.com/cran/kernelPhil
* Date/Publication: 2021-02-25 10:50:02 UTC
* Number of recursive dependencies: 89

Run `cloud_details(, "kernelPhil")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘kernelPhil-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: kernel.smooth.in.space
    > ### Title: Kernel smooth data in space alone
    > ### Aliases: kernel.smooth.in.space
    > 
    > ### ** Examples
    > 
    > n=400;
    ...
    +     year=stats::runif(n,0,sqrt(2)));
    > synthesised.data$dependent.variable<-unlist(lapply(1:nrow(synthesised.data),
    +     function(X){
    +     stats::dist(as.matrix(synthesised.data[c(1,X),1:2]),method =
    +         "euclidean")<synthesised.data$year[X];
    + }))
    > result<-kernelPhil::kernel.smooth.in.space(dataset = synthesised.data);
    Error: `group_by_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `group_by()` instead.
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# LAGOSNE

<details>

* Version: 2.0.2
* GitHub: https://github.com/cont-limno/LAGOSNE
* Source code: https://github.com/cran/LAGOSNE
* Date/Publication: 2020-11-29 00:50:02 UTC
* Number of recursive dependencies: 137

Run `cloud_details(, "LAGOSNE")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        4. │   │ └─base::withCallingHandlers(...)
        5. │   └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
        6. └─LAGOSNE::lake_info(name = "gibberish", state = "Wisconsin", dt = dt)
        7.   ├─base::do.call(...)
        8.   └─base::apply(...)
        9.     └─LAGOSNE (local) FUN(newX[, i], ...)
       10.       └─LAGOSNE:::lake_info_(...)
       11.         └─dplyr::filter_(dt_filter, filter_criteria)
       12.           └─dplyr:::lazy_deprec("filter")
       13.             └─lifecycle::deprecate_stop(...)
       14.               └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 2 | WARN 0 | SKIP 11 | PASS 3 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# Lahman

<details>

* Version: 10.0-1
* GitHub: https://github.com/cdalzell/Lahman
* Source code: https://github.com/cran/Lahman
* Date/Publication: 2022-04-26 08:20:10 UTC
* Number of recursive dependencies: 97

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
     939  628  634 1327 
    > 
    > # Career total G/PO/A/E/DP for Luis Aparicio
    > Fielding %>%
    +     filter(playerID == "aparilu01") %>% 
    +     select(G, PO, A, E, DP) %>%
    +     summarise_each(funs(sum))
    Error: `summarise_each()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `across()` instead.
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
    
    SUMMARY: processing the following file failed:
      ‘strikeoutsandhr.Rmd’
    
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
      [ FAIL 4 | WARN 5 | SKIP 0 | PASS 140 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test-load-data.R:24:3): test that it is possible to load multiple LANS summaries ──
      `... <- NULL` produced warnings.
      ── Failure (test-load-data.R:43:3): test that it is possible to load multiple LANS summaries ──
      `... <- NULL` produced warnings.
      ── Failure (test-load-data.R:58:3): test that it is possible to load multiple LANS summaries ──
      `... <- NULL` produced warnings.
      ── Failure (test-load-data.R:81:3): test that it is possible to load LANS maps ──
      `... <- NULL` produced warnings.
      
      [ FAIL 4 | WARN 5 | SKIP 0 | PASS 140 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# lifelogr

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/lifelogr
* Date/Publication: 2017-05-12 23:23:16 UTC
* Number of recursive dependencies: 91

Run `cloud_details(, "lifelogr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘lifelogr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: compare_groups
    > ### Title: Prints statistics on dataset, grouped by group assignments
    > ### Aliases: compare_groups
    > 
    > ### ** Examples
    > 
    > data(EX)
    ...
    +                            "group" = c(1:12))
    > compare_groups(dataset, person = EX, 
    +             addl_grouping_assignments = list("indiv_months" = indiv_months), 
    +             names_of_groupings = c("indiv_months"),
    +             variables_to_compare = c("steps", "restingHeartRate"))
    [1] "month"
    [1] "steps"
    Error: `summarise_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `summarise()` instead.
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        4. │   │ └─testthat::capture_output_lines(code, print, width = width)
        5. │   │   └─testthat:::eval_with_output(code, print = print, width = width)
        6. │   │     ├─withr::with_output_sink(path, withVisible(code))
        7. │   │     │ └─base::force(code)
        8. │   │     └─base::withVisible(code)
        9. │   └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
       10. └─lifelogr::plot_i(EX, "steps")
       11.   └─dplyr::summarize_(...)
       12.     └─dplyr:::lazy_deprec("summarize")
       13.       └─lifecycle::deprecate_stop(...)
       14.         └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 1 | WARN 26 | SKIP 0 | PASS 53 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘summary.Rmd’ using rmarkdown
    Quitting from lines 153-154 (summary.Rmd) 
    Error: processing vignette 'summary.Rmd' failed with diagnostics:
    `summarize_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `summarize()` instead.
    --- failed re-building ‘summary.Rmd’
    
    --- re-building ‘vignette_viz.Rmd’ using rmarkdown
    ...
    Error: processing vignette 'vignette_viz.Rmd' failed with diagnostics:
    `summarize_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `summarize()` instead.
    --- failed re-building ‘vignette_viz.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘summary.Rmd’ ‘vignette_viz.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# linea

<details>

* Version: 0.0.2
* GitHub: NA
* Source code: https://github.com/cran/linea
* Date/Publication: 2022-06-22 22:20:02 UTC
* Number of recursive dependencies: 130

Run `cloud_details(, "linea")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘linea-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: gt_f
    > ### Title: apply_normalisation
    > ### Aliases: gt_f
    > 
    > ### ** Examples
    > 
    > data = read_xcsv("https://raw.githubusercontent.com/paladinic/data/main/ecomm_data.csv") %>% 
    ...
    Delimiter: ","
    dbl  (7): ecommerce, black.friday, christmas, covid, online_media, offline_m...
    date (1): date
    
    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    Error in interest_over_time(widget, comparison_item, tz) : 
      Status code was not 200. Returned status code:429
    Calls: %>% ... as.Date -> pull -> gt_f -> gtrends -> interest_over_time
    Execution halted
    ```

## Newly fixed

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      +        date_col = pooled_id_var) %>%
      +     is.data.frame() %>%
      +     expect_equal(TRUE)
      + })
      ── Error (???): gtrends_f - pooled - output dataframe ──────────────────────────
      Error in `interest_over_time(widget, comparison_item, tz)`: Status code was not 200. Returned status code:429
      Backtrace:
       1. ... %>% expect_equal(TRUE)
       6. linea::gt_f(data = pooled_data, kw = "bitcoin", date_col = pooled_id_var)
       7. gtrendsR::gtrends(keyword = kw, time = time_str, onlyInterest = TRUE)
       8. gtrendsR:::interest_over_time(widget, comparison_item, tz)
      
      Error in reporter$stop_if_needed() : Test failed
      Calls: test_that -> <Anonymous>
      Execution halted
    ```

# lookupTable

<details>

* Version: 0.1
* GitHub: NA
* Source code: https://github.com/cran/lookupTable
* Date/Publication: 2015-08-28 01:21:23
* Number of recursive dependencies: 36

Run `cloud_details(, "lookupTable")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘lookupTable-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: predict.lookupTable
    > ### Title: Predictions from a look-up table
    > ### Aliases: predict.lookupTable
    > 
    > ### ** Examples
    > 
    > df.input <- cars
    > response <- 'dist'
    > feature.boundaries <- list(c(-Inf, 5, 10, 15, 20, 25, Inf))
    > features.con <- c('speed')
    > dist.table <- lookupTable(df.input, response, feature.boundaries, features.con)
    Error: `summarise_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `summarise()` instead.
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Backtrace:
          ▆
       1. └─lookupTable::lookupTable(...) at test.lookupTable.R:8:0
       2.   └─methods::new(`<chr>`, ...)
       3.     ├─methods::initialize(value, ...)
       4.     └─lookupTable (local) initialize(value, ...)
       5.       └─lookupTable (local) .local(.Object, ...)
       6.         └─dplyr::summarise_(...)
       7.           └─dplyr:::lazy_deprec("summarise")
       8.             └─lifecycle::deprecate_stop(...)
       9.               └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# mase

<details>

* Version: 0.1.3
* GitHub: NA
* Source code: https://github.com/cran/mase
* Date/Publication: 2021-07-09 23:00:07 UTC
* Number of recursive dependencies: 77

Run `cloud_details(, "mase")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘mase-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: gregElasticNet
    > ### Title: Compute an elastic net regression estimator
    > ### Aliases: gregElasticNet
    > 
    > ### ** Examples
    > 
    > library(survey)
    ...
        dotchart
    
    > data(api)
    > gregElasticNet(y = apisrs$api00, 
    + xsample = apisrs[c("col.grad", "awards", "snum", "dnum", "cnum", "pcttest", "meals", "sch.wide")], 
    + xpop = apipop[c("col.grad", "awards", "snum", "dnum", "cnum", "pcttest", "meals", "sch.wide")], 
    + pi = apisrs$pw^(-1), var_est = TRUE, alpha = .5)
    Error: `select_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `select()` instead.
    Execution halted
    ```

# matman

<details>

* Version: 1.1.3
* GitHub: NA
* Source code: https://github.com/cran/matman
* Date/Publication: 2021-12-13 09:30:02 UTC
* Number of recursive dependencies: 87

Run `cloud_details(, "matman")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘matman-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: summary,ABCXYZData-method
    > ### Title: Prints the result summary of an ABC/XYZ analysis
    > ### Aliases: summary,ABCXYZData-method
    > ### Keywords: methods
    > 
    > ### ** Examples
    > 
    > # ABC Analysis
    > data("Amount")
    > abcResult = computeABCXYZAnalysis(data = Amount,
    +     value = "value",
    +     item = "item",
    +     timestamp = "date")
    > summary(abcResult)
    Error: The `.dots` argument of `group_by()` was deprecated in dplyr 1.0.0 and is now defunct.
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

# metaconfoundr

<details>

* Version: 0.1.0
* GitHub: https://github.com/malcolmbarrett/metaconfoundr
* Source code: https://github.com/cran/metaconfoundr
* Date/Publication: 2021-10-12 08:10:02 UTC
* Number of recursive dependencies: 117

Run `cloud_details(, "metaconfoundr")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘ellipsis’
      All declared Imports should be used.
    ```

# modeldb

<details>

* Version: 0.2.2
* GitHub: https://github.com/tidymodels/modeldb
* Source code: https://github.com/cran/modeldb
* Date/Publication: 2020-02-10 20:50:07 UTC
* Number of recursive dependencies: 92

Run `cloud_details(, "modeldb")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      > library(modeldb)
      > 
      > test_check("modeldb")
      [ FAIL 2 | WARN 1 | SKIP 0 | PASS 17 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test_kmeans.R:11:3): Not specifying variables works ────────────────
      `select(mtcars, wt, mpg) %>% simple_kmeans_db()` produced warnings.
      ── Failure (test_kmeans.R:23:3): Centroid argument is accepted ─────────────────
      `simple_kmeans_db(mtcars, mpg, wt, initial_kmeans = ik)` produced warnings.
      
      [ FAIL 2 | WARN 1 | SKIP 0 | PASS 17 ]
      Error: Test failures
      Execution halted
    ```

# modelplotr

<details>

* Version: 1.1.0
* GitHub: https://github.com/jurrr/modelplotr
* Source code: https://github.com/cran/modelplotr
* Date/Publication: 2020-10-13 04:20:05 UTC
* Number of recursive dependencies: 147

Run `cloud_details(, "modelplotr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘modelplotr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: build_input_yourself
    > ### Title: Example: build required input from a custom model
    > ### Aliases: build_input_yourself
    > 
    > ### ** Examples
    > 
    > # load example data (Bank clients with/without a term deposit - see ?bank_td for details)
    ...
    +            prob_no.term.deposit = prob_no.term.deposit,
    +            ntl_term.deposit = ntl_term.deposit,
    +            ntl_no.term.deposit = ntl_no.term.deposit) %>%
    +     select(-has_td)
    +     )
    > 
    > plot_input <- plotting_scope(prepared_input = scores_and_ntiles,scope='compare_datasets')
    Error: `group_by_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `group_by()` instead.
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘modelplotr.Rmd’ using rmarkdown
    Quitting from lines 274-284 (modelplotr.Rmd) 
    Error: processing vignette 'modelplotr.Rmd' failed with diagnostics:
    `select_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `select()` instead.
    --- failed re-building ‘modelplotr.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘modelplotr.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# Momocs

<details>

* Version: 1.4.0
* GitHub: https://github.com/MomX/Momocs
* Source code: https://github.com/cran/Momocs
* Date/Publication: 2022-04-04 16:50:02 UTC
* Number of recursive dependencies: 137

Run `cloud_details(, "Momocs")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘Momocs-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: PCcontrib
    > ### Title: Shape variation along PC axes
    > ### Aliases: PCcontrib PCcontrib.PCA
    > 
    > ### ** Examples
    > 
    > bot.p <- PCA(efourier(bot, 12))
    'norm=TRUE' is used and this may be troublesome. See ?efourier
    > PCcontrib(bot.p, nax=1:3)
    Error: `mutate_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `mutate()` instead.
    Execution halted
    ```

# mosaicModel

<details>

* Version: 0.3.0
* GitHub: NA
* Source code: https://github.com/cran/mosaicModel
* Date/Publication: 2017-09-22 16:21:41 UTC
* Number of recursive dependencies: 171

Run `cloud_details(, "mosaicModel")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘mosaicModel-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: df_props
    > ### Title: Joint and conditional proportions
    > ### Aliases: df_props
    > 
    > ### ** Examples
    > 
    > df_props(mtcars, ~ cyl + gear) 
    Error: `group_by_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `group_by()` instead.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘MASS’ ‘caret’ ‘ggformula’ ‘knitr’ ‘testthat’ ‘tidyverse’
      All declared Imports should be used.
    ```

# mpathsenser

<details>

* Version: 1.0.3
* GitHub: NA
* Source code: https://github.com/cran/mpathsenser
* Date/Publication: 2022-06-01 09:50:02 UTC
* Number of recursive dependencies: 98

Run `cloud_details(, "mpathsenser")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
          is_null
      
      Could not add data to the data base. Falling back to UPSERT method
        adding: tmp/workdir/mpathsenser/new/mpathsenser.Rcheck/mpathsenser/testdata/test.json (deflated 81%)
        adding: tmp/workdir/mpathsenser/new/mpathsenser.Rcheck/mpathsenser/testdata/test.json (deflated 81%)
      [ FAIL 1 | WARN 12 | SKIP 0 | PASS 249 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test-functions.R:9:3): import ──────────────────────────────────────
      `import(path = path, db = db, recursive = FALSE)` did not throw the expected message.
      
      [ FAIL 1 | WARN 12 | SKIP 0 | PASS 249 ]
      Error: Test failures
      Execution halted
    ```

# MSiP

<details>

* Version: 1.3.7
* GitHub: NA
* Source code: https://github.com/cran/MSiP
* Date/Publication: 2021-06-17 08:20:05 UTC
* Number of recursive dependencies: 84

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
    > datScoring <- Weighted.matrixModel(SampleDatInput)
    Error: `summarise_each()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `across()` instead.
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘MSiP_tutorial.Rmd’ using knitr
    Quitting from lines 46-49 (MSiP_tutorial.Rmd) 
    Error: processing vignette 'MSiP_tutorial.Rmd' failed with diagnostics:
    `summarise_each()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `across()` instead.
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

# mtconnectR

<details>

* Version: 1.2.1
* GitHub: NA
* Source code: https://github.com/cran/mtconnectR
* Date/Publication: 2019-01-07 19:00:22 UTC
* Number of recursive dependencies: 80

Run `cloud_details(, "mtconnectR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘mtconnectR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: convert_interval_to_ts
    > ### Title: Convert Interval to Time Series
    > ### Aliases: convert_interval_to_ts
    > 
    > ### ** Examples
    > 
    > test_interval =
    +   data.frame(start = as.POSIXct(c(0.5, 1, 1.008, 1.011),  tz = 'CST6CDT', origin = "1970-01-01"),
    +              end   = as.POSIXct(c(1, 1.008, 1.011, 2),  tz = 'CST6CDT', origin = "1970-01-01"),
    +              duration = c(0.50, 0.01, 0.00, 0.99),
    +              y     = c("e", "e", "e", "f"))
    > convert_interval_to_ts(test_interval)
    Error: `arrange_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `arrange()` instead.
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Please use `arrange()` instead.
      Backtrace:
          x
       1. +-mtconnectR::create_mtc_device_from_dmtcd(...) at test-dataExtraction_mtc.R:13:0
       2. | \-mtconnectR::read_dmtcd_file(...)
       3. |   \-... %>% as.data.frame()
       4. +-base::as.data.frame(.)
       5. \-dplyr::arrange_(., "timestamp")
       6.   \-dplyr:::lazy_deprec("arrange")
       7.     \-lifecycle::deprecate_stop(...)
       8.       \-lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 25 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘create_mtc_device.Rmd’ using rmarkdown
    Quitting from lines 21-30 (create_mtc_device.Rmd) 
    Error: processing vignette 'create_mtc_device.Rmd' failed with diagnostics:
    `arrange_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `arrange()` instead.
    --- failed re-building 'create_mtc_device.Rmd'
    
    --- re-building ‘simulate_map_gcode.Rmd’ using rmarkdown
    ...
    Error: processing vignette 'simulate_map_gcode.Rmd' failed with diagnostics:
    `arrange_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `arrange()` instead.
    --- failed re-building 'simulate_map_gcode.Rmd'
    
    SUMMARY: processing the following files failed:
      ‘create_mtc_device.Rmd’ ‘simulate_map_gcode.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
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
      ── Failure (test-multicolor.R:89:3): colors(), including grays, rainbow, and rbg work ──
      `suppressMessages(...)` produced warnings.
      ── Failure (test-multicolor.R:103:3): colors(), including grays, rainbow, and rbg work ──
      `suppressMessages(...)` produced warnings.
      ── Failure (test-multicolor.R:113:3): colors(), including grays, rainbow, and rbg work ──
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
* Number of recursive dependencies: 180

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

# ncdump

<details>

* Version: 0.0.3
* GitHub: https://github.com/r-gris/ncdump
* Source code: https://github.com/cran/ncdump
* Date/Publication: 2017-05-02 12:35:30 UTC
* Number of recursive dependencies: 62

Run `cloud_details(, "ncdump")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        2. │ └─testthat (local) condition(object)
        3. │   └─testthat::expect_is(x, class)
        4. │     └─testthat::quasi_label(enquo(object), label, arg = "object")
        5. │       └─rlang::eval_bare(expr, quo_get_env(quo))
        6. ├─ncdump:::dimvars(con)
        7. ├─ncdump:::dimvars.NetCDF(con)
        8. │ └─dims(x) %>% filter_("create_dimvar") %>% select_("name")
        9. └─dplyr::select_(., "name")
       10.   └─dplyr:::lazy_deprec("select")
       11.     └─lifecycle::deprecate_stop(...)
       12.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 1 | WARN 2 | SKIP 0 | PASS 6 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# networkreporting

<details>

* Version: 0.1.1
* GitHub: NA
* Source code: https://github.com/cran/networkreporting
* Date/Publication: 2016-12-05 18:28:47
* Number of recursive dependencies: 63

Run `cloud_details(, "networkreporting")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘test_all.R’
    Running the tests in ‘tests/test_all.R’ failed.
    Last 13 lines of output:
      <lifecycle_error_deprecated/defunctError/error/condition/condition>
      Error: `select_()` was deprecated in dplyr 0.7.0 and is now defunct.
      Please use `select()` instead.
      Backtrace:
          ▆
       1. └─networkreporting::kp.individual.estimator_(...) at test_variance.R:40:0
       2.   └─networkreporting::kp.estimator_(...)
       3.     └─dplyr::select_(resp.data, .dots = weights)
       4.       └─dplyr:::lazy_deprec("select")
       5.         └─lifecycle::deprecate_stop(...)
       6.           └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 12 ]
      Error: Test failures
      Execution halted
    ```

# NobBS

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/NobBS
* Date/Publication: 2020-03-03 10:40:02 UTC
* Number of recursive dependencies: 20

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
    > # Load the data
    > data(denguedat)
    > # Perform default 'NobBS' assuming Poisson distribution, vague priors, and default specifications.
    > nowcast <- NobBS(denguedat, as.Date("1990-04-09"),units="1 week",onset_date="onset_week",
    + report_date="report_week")
    Computing a nowcast for  1990-04-09
    Error: `group_by_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `group_by()` instead.
    Execution halted
    ```

# nonmemica

<details>

* Version: 0.9.7
* GitHub: NA
* Source code: https://github.com/cran/nonmemica
* Date/Publication: 2020-11-24 07:30:11 UTC
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

# panelr

<details>

* Version: 0.7.6
* GitHub: https://github.com/jacob-long/panelr
* Source code: https://github.com/cran/panelr
* Date/Publication: 2021-12-17 07:40:02 UTC
* Number of recursive dependencies: 172

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

# parsnip

<details>

* Version: 1.0.0
* GitHub: https://github.com/tidymodels/parsnip
* Source code: https://github.com/cran/parsnip
* Date/Publication: 2022-06-16 10:20:02 UTC
* Number of recursive dependencies: 134

Run `cloud_details(, "parsnip")` for more info

</details>

## Newly broken

*   checking Rd files ... WARNING
    ```
    prepare_Rd: Each row in `x` should match at most 1 row in `y`.
    ℹ Row 1 of `x` matches multiple rows.
    ℹ If multiple matches are expected, specify `multiple = "all"` in the join call
      to silence this warning.
    prepare_Rd: Each row in `x` should match at most 1 row in `y`.
    ℹ Row 1 of `x` matches multiple rows.
    ℹ If multiple matches are expected, specify `multiple = "all"` in the join call
      to silence this warning.
    prepare_Rd: Each row in `x` should match at most 1 row in `y`.
    ℹ Row 1 of `x` matches multiple rows.
    ...
    ℹ If multiple matches are expected, specify `multiple = "all"` in the join call
      to silence this warning.
    prepare_Rd: Each row in `x` should match at most 1 row in `y`.
    ℹ Row 1 of `x` matches multiple rows.
    ℹ If multiple matches are expected, specify `multiple = "all"` in the join call
      to silence this warning.
    prepare_Rd: Each row in `x` should match at most 1 row in `y`.
    ℹ Row 1 of `x` matches multiple rows.
    ℹ If multiple matches are expected, specify `multiple = "all"` in the join call
      to silence this warning.
    ```

*   checking for unstated dependencies in examples ... WARNING
    ```
    Warning: Each row in `x` should match at most 1 row in `y`.
    ℹ Row 1 of `x` matches multiple rows.
    ℹ If multiple matches are expected, specify `multiple = "all"` in the join call
      to silence this warning.
    Warning: Each row in `x` should match at most 1 row in `y`.
    ℹ Row 1 of `x` matches multiple rows.
    ℹ If multiple matches are expected, specify `multiple = "all"` in the join call
      to silence this warning.
    Warning: Each row in `x` should match at most 1 row in `y`.
    ℹ Row 1 of `x` matches multiple rows.
    ...
    ℹ If multiple matches are expected, specify `multiple = "all"` in the join call
      to silence this warning.
    Warning: Each row in `x` should match at most 1 row in `y`.
    ℹ Row 1 of `x` matches multiple rows.
    ℹ If multiple matches are expected, specify `multiple = "all"` in the join call
      to silence this warning.
    Warning: Each row in `x` should match at most 1 row in `y`.
    ℹ Row 1 of `x` matches multiple rows.
    ℹ If multiple matches are expected, specify `multiple = "all"` in the join call
      to silence this warning.
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘rules’, ‘baguette’, ‘ipred’, ‘dbarts’, ‘h2o’, ‘agua’, ‘lightgbm’, ‘bonsai’, ‘mboost’, ‘mda’, ‘sda’, ‘sparsediscrim’, ‘klaR’, ‘brulee’, ‘glmnet’, ‘rstan’, ‘rstanarm’, ‘naivebayes’, ‘plsmod’, ‘mixOmics’, ‘pscl’, ‘workflows’, ‘randomForest’, ‘xrf’, ‘flexsurv’, ‘broom’
    ```

# phenofit

<details>

* Version: 0.3.2
* GitHub: https://github.com/eco-hydro/phenofit
* Source code: https://github.com/cran/phenofit
* Date/Publication: 2021-10-15 10:50:02 UTC
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
    +         rFUN = smooth_wWHIT, wFUN = wFUN,
    +         r_min = 0.05, ypeak_min = 0.05,
    +         lambda = 10,
    +         verbose = FALSE
    +     ))
      [season_mov] running 1 ... 
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
       10. ├─phenofit (local) `<fn>`(INPUT = `<named list>`, options = `<named list>`, rFUN = `<fn>`)
       11. │ ├─base::do.call(opt_season, params_i)
       12. │ └─phenofit (local) `<fn>`(INPUT = `<named list>`, lambda = 14.1253754462275, rFUN = `<fn>`)
       13. │   └─phenofit::find_season.peaks(d_fit, info_peak)
       14. │     └─phenofit:::get_A(ypred, na.rm = FALSE)
       15. │       ├─range(x, na.rm = na.rm) %>% diff()
       16. │       └─base::Summary.data.frame(`<dt[,8]>`, na.rm = FALSE)
       17. │         └─base::lapply(...)
       18. │           └─base (local) FUN(X[[i]], ...)
       19. └─base::diff(.)
      
      [ FAIL 7 | WARN 0 | SKIP 0 | PASS 34 ]
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
      installed size is  7.4Mb
      sub-directories of 1Mb or more:
        libs   6.3Mb
    ```

# pkggraph

<details>

* Version: 0.2.3
* GitHub: https://github.com/talegari/pkggraph
* Source code: https://github.com/cran/pkggraph
* Date/Publication: 2018-11-15 09:50:03 UTC
* Number of recursive dependencies: 69

Run `cloud_details(, "pkggraph")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘pkggraph-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: get_all_dependencies
    > ### Title: get_all_dependencies
    > ### Aliases: get_all_dependencies
    > 
    > ### ** Examples
    > 
    > pkggraph::init(local = TRUE)
    Using 'local' data ...
    To fetch data from CRAN over internet, run:`pkggraph::init(local = FALSE)`
    Done!
    > # general use
    > pkggraph::get_all_dependencies("mlr")
    Error: `arrange_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `arrange()` instead.
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘vignette_pkggraph.Rmd’ using rmarkdown
    Quitting from lines 34-58 (vignette_pkggraph.Rmd) 
    Error: processing vignette 'vignette_pkggraph.Rmd' failed with diagnostics:
    `arrange_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `arrange()` instead.
    --- failed re-building ‘vignette_pkggraph.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘vignette_pkggraph.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# PKNCA

<details>

* Version: 0.9.5
* GitHub: https://github.com/billdenney/pknca
* Source code: https://github.com/cran/PKNCA
* Date/Publication: 2021-10-29 19:50:02 UTC
* Number of recursive dependencies: 79

Run `cloud_details(, "PKNCA")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        6. │   ├─testthat (local) .capture(...)
        7. │   │ └─base::withCallingHandlers(...)
        8. │   └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
        9. └─PKNCA::pk.tss(...)
       10.   └─PKNCA::pk.tss.monoexponential(..., check = check)
       11.     └─PKNCA:::pk.tss.monoexponential.population(...)
       12.       └─PKNCA:::tss.monoexponential.generate.formula(data)
       13.         └─dplyr::summarize_(dplyr::group_by_(data, "treatment"), .dots = list(conc.mean = ~mean(conc)))
       14.           └─dplyr:::lazy_deprec("summarize")
       15.             └─lifecycle::deprecate_stop(...)
       16.               └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 2 | WARN 1 | SKIP 0 | PASS 1322 ]
      Error: Test failures
      Execution halted
    ```

# plotly

<details>

* Version: 4.10.0
* GitHub: https://github.com/plotly/plotly.R
* Source code: https://github.com/cran/plotly
* Date/Publication: 2021-10-09 21:10:07 UTC
* Number of recursive dependencies: 164

Run `cloud_details(, "plotly")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      • plotly/plotly-bar-inference.svg
      • plotly/plotly-box-data-array.svg
      • plotly/plotly-character-axis.svg
      • plotly/plotly-factor-axis.svg
      • plotly/plotly-group-within-trace.svg
      • plotly/plotly-histogram-vert.svg
      • plotly/plotly-histogram.svg
      • plotly/plotly-inherit-false.svg
      • plotly/plotly-scatterplot.svg
      • plotly/plotly-time-series-summary.svg
      • ticktext-linebreaks/ticktext-linebreaks-no-linebreaks.svg
      • ticktext-linebreaks/ticktext-linebreaks-one-cat.svg
      • ticktext-linebreaks/ticktext-linebreaks.svg
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.1Mb
      sub-directories of 1Mb or more:
        htmlwidgets   4.0Mb
    ```

# PPforest

<details>

* Version: 0.1.2
* GitHub: https://github.com/natydasilva/PPforest
* Source code: https://github.com/cran/PPforest
* Date/Publication: 2021-10-14 14:40:05 UTC
* Number of recursive dependencies: 86

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
    > pprf.crab <- PPforest(data = crab, class = 'Type',
    +  std = FALSE, size.tr = 1, m = 100, size.p = .5, 
    +  PPmethod = 'LDA' , parallel = TRUE, cores = 2, rule=1)
    Warning: `data_frame()` was deprecated in tibble 1.1.0.
    Please use `tibble()` instead.
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.
    Error: `filter_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `filter()` instead.
    Execution halted
    ```

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
    Error: processing vignette 'PPforest-vignette.Rmd' failed with diagnostics:
    `filter_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `filter()` instead.
    --- failed re-building ‘PPforest-vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘PPforest-vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.3Mb
      sub-directories of 1Mb or more:
        libs   7.1Mb
    ```

# presenter

<details>

* Version: 0.1.1
* GitHub: https://github.com/Harrison4192/presenter
* Source code: https://github.com/cran/presenter
* Date/Publication: 2021-11-18 06:20:05 UTC
* Number of recursive dependencies: 120

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

# psrwe

<details>

* Version: 3.1
* GitHub: https://github.com/olssol/psrwe
* Source code: https://github.com/cran/psrwe
* Date/Publication: 2022-03-01 15:20:02 UTC
* Number of recursive dependencies: 78

Run `cloud_details(, "psrwe")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘vignette.Rmd’ using rmarkdown
    Quitting from lines 46-47 (vignette.Rmd) 
    Error: processing vignette 'vignette.Rmd' failed with diagnostics:
    The `.dots` argument of `group_by()` was deprecated in dplyr 1.0.0 and is now defunct.
    --- failed re-building ‘vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 97.3Mb
      sub-directories of 1Mb or more:
        libs  96.6Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# psycho

<details>

* Version: 0.6.1
* GitHub: https://github.com/neuropsychology/psycho.R
* Source code: https://github.com/cran/psycho
* Date/Publication: 2021-01-19 06:40:10 UTC
* Number of recursive dependencies: 83

Run `cloud_details(, "psycho")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘psycho-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: assess
    > ### Title: Compare a patient's score to a control group
    > ### Aliases: assess
    > 
    > ### ** Examples
    > 
    > result <- assess(patient = 124, mean = 100, sd = 15, n = 100)
    Error: `mutate_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `mutate()` instead.
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Error: `mutate_()` was deprecated in dplyr 0.7.0 and is now defunct.
      Please use `mutate()` instead.
      Backtrace:
          ▆
       1. ├─psycho::crawford.test(...) at test-assess.R:42:2
       2. │ └─... %>% ggplot(aes_string(x = "x", ymin = 0, ymax = "y"))
       3. ├─ggplot2::ggplot(., aes_string(x = "x", ymin = 0, ymax = "y"))
       4. └─dplyr::mutate_(., x = "scales::rescale(x, from=c(0, 1), to = c(sample_mean, sample_mean+sample_sd))")
       5.   └─dplyr:::lazy_deprec("mutate")
       6.     └─lifecycle::deprecate_stop(...)
       7.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 17 ]
      Error: Test failures
      Execution halted
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
* Number of recursive dependencies: 134

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
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 65 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 8504 marked UTF-8 strings
    ```

# quickpsy

<details>

* Version: 0.1.5.1
* GitHub: NA
* Source code: https://github.com/cran/quickpsy
* Date/Publication: 2019-10-02 15:54:02 UTC
* Number of recursive dependencies: 36

Run `cloud_details(, "quickpsy")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘quickpsy-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: deviance
    > ### Title: Calculates the deviances
    > ### Aliases: deviance
    > 
    > ### ** Examples
    > 
    > library(MPDiR) # contains the Vernier data
    > fit <- quickpsy(Vernier, Phaseshift, NumUpward, N,
    +                 grouping = .(Direction, WaveForm, TempFreq), B = 20)
    Error: `group_by_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `group_by()` instead.
    Execution halted
    ```

# rabhit

<details>

* Version: 0.2.1
* GitHub: NA
* Source code: https://github.com/cran/rabhit
* Date/Publication: 2022-02-16 14:10:02 UTC
* Number of recursive dependencies: 133

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
# representr

<details>

* Version: 0.1.3
* GitHub: NA
* Source code: https://github.com/cran/representr
* Date/Publication: 2022-02-03 22:00:02 UTC
* Number of recursive dependencies: 59

Run `cloud_details(, "representr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘representr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: emp_kl_div
    > ### Title: Calculate the empirical KL divergence for a representative
    > ###   dataset as compared to the true dataset
    > ### Aliases: emp_kl_div
    > 
    > ### ** Examples
    > 
    ...
    > 
    > ## empirical KL divergence
    > cat_vars <- c("sex")
    > num_vars <- c("income", "bp")
    > emp_kl_div(rl_reg1[unique(identity.rl_reg1), c(cat_vars, num_vars)],
    +            rep_dat_random[, c(cat_vars, num_vars)],
    +            cat_vars, num_vars)
    Error: `group_by_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `group_by()` instead.
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘representr.Rmd’ using rmarkdown
    `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    Quitting from lines 333-338 (representr.Rmd) 
    Error: processing vignette 'representr.Rmd' failed with diagnostics:
    `group_by_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `group_by()` instead.
    --- failed re-building ‘representr.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘representr.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# respR

<details>

* Version: 2.0.2
* GitHub: https://github.com/januarharianto/respr
* Source code: https://github.com/cran/respR
* Date/Publication: 2022-03-23 15:50:02 UTC
* Number of recursive dependencies: 124

Run `cloud_details(, "respR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘respR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: calc_rate.ft
    > ### Title: Calculate rate of change in oxygen from flowthrough respirometry
    > ###   data
    > ### Aliases: calc_rate.ft
    > 
    > ### ** Examples
    > 
    ...
     15.                           ├─base::withRestarts(...)
     16.                           │ └─base (local) withOneRestart(expr, restarts[[1L]])
     17.                           │   └─base (local) doWithOneRestart(return(expr), restart)
     18.                           └─vctrs:::stop_lossy_cast(...)
     19.                             └─vctrs::stop_incompatible_cast(...)
     20.                               └─vctrs::stop_incompatible_type(...)
     21.                                 └─vctrs:::stop_incompatible(...)
     22.                                   └─vctrs:::stop_vctrs(...)
     23.                                     └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       12.                 └─vctrs:::vec_cast.integer.double(...)
       13.                   └─vctrs::maybe_lossy_cast(...)
       14.                     ├─base::withRestarts(...)
       15.                     │ └─base (local) withOneRestart(expr, restarts[[1L]])
       16.                     │   └─base (local) doWithOneRestart(return(expr), restart)
       17.                     └─vctrs:::stop_lossy_cast(...)
       18.                       └─vctrs::stop_incompatible_cast(...)
       19.                         └─vctrs::stop_incompatible_type(...)
       20.                           └─vctrs:::stop_incompatible(...)
       21.                             └─vctrs:::stop_vctrs(...)
       22.                               └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
      
      [ FAIL 10 | WARN 0 | SKIP 5 | PASS 4508 ]
      Error: Test failures
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

*   checking examples ... ERROR
    ```
    Running examples in ‘RNeXML-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: get_taxa
    > ### Title: get_taxa
    > ### Aliases: get_taxa get_otu
    > 
    > ### ** Examples
    > 
    > comp_analysis <- system.file("examples", "comp_analysis.xml", package="RNeXML")
    > nex <- nexml_read(comp_analysis)
    > get_taxa(nex)
    Error: `select_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `select()` instead.
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘test-all.R’
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      Please use `select()` instead.
      Backtrace:
          ▆
       1. ├─RNeXML::nexml_to_simmap(nex) at test_simmap.R:24:2
       2. │ └─RNeXML::get_characters(nexml)
       3. │   └─get_level(nex, "otus/otu") %>% dplyr::select_(drop) %>% ...
       4. ├─RNeXML:::optional_labels(., id_col = "otu")
       5. └─dplyr::select_(., drop)
       6.   └─dplyr:::lazy_deprec("select")
       7.     └─lifecycle::deprecate_stop(...)
       8.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 16 | WARN 1 | SKIP 42 | PASS 214 ]
      Error: Test failures
      Execution halted
    ```

# rollmatch

<details>

* Version: 2.0.2
* GitHub: https://github.com/RTIInternational/rollmatch
* Source code: https://github.com/cran/rollmatch
* Date/Publication: 2020-06-02 15:20:06 UTC
* Number of recursive dependencies: 35

Run `cloud_details(, "rollmatch")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘rollmatch-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: rollmatch
    > ### Title: Rolling entry matching
    > ### Aliases: rollmatch
    > 
    > ### ** Examples
    > 
    > data(package="rollmatch", "rem_synthdata_small")
    ...
    +                     vars = vars, lookback = 1, alpha = .2,
    +                     standard_deviation = "average", num_matches = 3,
    +                     replacement = TRUE)
    Warning: Each row in `x` should match at most 1 row in `y`.
    ℹ Row 1 of `x` matches multiple rows.
    ℹ If multiple matches are expected, specify `multiple = "all"` in the join call
      to silence this warning.
    Error: `group_by_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `group_by()` instead.
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
      ── Failure (test-conversion.R:22:3): triple <-> tidy ───────────────────────────
      `triple` not equal to `brauer_2008_triple`.
      Component "features": Names: 3 string mismatches
      Component "features": Component 2: 500 string mismatches
      Component "features": Component 3: 476 string mismatches
      Component "features": Component 4: 500 string mismatches
      
      [ FAIL 1 | WARN 4 | SKIP 1 | PASS 15 ]
      Error: Test failures
      Execution halted
    ```

# rprev

<details>

* Version: 1.0.5
* GitHub: https://github.com/stulacy/rprev-dev
* Source code: https://github.com/cran/rprev
* Date/Publication: 2021-05-04 16:40:03 UTC
* Number of recursive dependencies: 128

Run `cloud_details(, "rprev")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘diagnostics.Rmd’ using rmarkdown
    Loading required package: survival
    Loading required package: Hmisc
    Loading required package: lattice
    Loading required package: Formula
    
    Attaching package: 'Hmisc'
    
    The following objects are masked from 'package:base':
    ...
    
    --- re-building ‘user_guide.Rmd’ using rmarkdown
    Loading required package: survival
    --- finished re-building ‘user_guide.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘diagnostics.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# rsample

<details>

* Version: 1.0.0
* GitHub: https://github.com/tidymodels/rsample
* Source code: https://github.com/cran/rsample
* Date/Publication: 2022-06-24 18:10:02 UTC
* Number of recursive dependencies: 108

Run `cloud_details(, "rsample")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘rsample-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: tidy.rsplit
    > ### Title: Tidy Resampling Object
    > ### Aliases: tidy.rsplit tidy.rset tidy.vfold_cv tidy.nested_cv
    > 
    > ### ** Examples
    > 
    > library(ggplot2)
    ...
        ▆
     1. ├─generics::tidy(mc_cv(mtcars, times = 5))
     2. ├─rsample:::tidy.rset(mc_cv(mtcars, times = 5))
     3. │ └─dplyr::if_else(is.null(dots$unique_ind), TRUE, dots$unique_ind)
     4. │   └─dplyr:::vec_case_when(...)
     5. │     └─vctrs::list_check_all_vectors(values, arg = values_arg, call = call)
     6. └─vctrs:::stop_scalar_type(`<fn>`(NULL), "false", `<env>`)
     7.   └─vctrs:::stop_vctrs(...)
     8.     └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Working_with_rsets.Rmd’ using rmarkdown
    Quitting from lines 209-211 (Working_with_rsets.Rmd) 
    Error: processing vignette 'Working_with_rsets.Rmd' failed with diagnostics:
    `false` must be a vector, not NULL.
    --- failed re-building ‘Working_with_rsets.Rmd’
    
    --- re-building ‘rsample.Rmd’ using rmarkdown
    --- finished re-building ‘rsample.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Working_with_rsets.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# rties

<details>

* Version: 5.0.0
* GitHub: NA
* Source code: https://github.com/cran/rties
* Date/Publication: 2020-05-11 16:00:02 UTC
* Number of recursive dependencies: 163

Run `cloud_details(, "rties")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘rties-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: dataPrep
    > ### Title: Reformat a user-provided dataframe in a generic form appropriate
    > ###   for _rties_ modeling
    > ### Aliases: dataPrep
    > 
    > ### ** Examples
    > 
    > data <- rties_ExampleDataShort
    > newData <- dataPrep(basedata=data, dyadId="couple", personId="person", obs_name="dial", 
    + dist_name="female", time_name="time", time_lag=2)
    Error: `group_by_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `group_by()` instead.
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘coupled_oscillator_V05.Rmd’ using rmarkdown
    --- finished re-building ‘coupled_oscillator_V05.Rmd’
    
    --- re-building ‘inertia_coordination_V05.Rmd’ using rmarkdown
    Quitting from lines 59-61 (inertia_coordination_V05.Rmd) 
    Error: processing vignette 'inertia_coordination_V05.Rmd' failed with diagnostics:
    `group_by_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `group_by()` instead.
    --- failed re-building ‘inertia_coordination_V05.Rmd’
    ...
    `group_by_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `group_by()` instead.
    --- failed re-building ‘sysVar_inOut_V05.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘inertia_coordination_V05.Rmd’ ‘overview_data_prep_V05.Rmd’
      ‘sysVar_inOut_V05.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘DescTools’ ‘MASS’ ‘gridExtra’
      All declared Imports should be used.
    ```

# saeSim

<details>

* Version: 0.11.0
* GitHub: https://github.com/wahani/saeSim
* Source code: https://github.com/cran/saeSim
* Date/Publication: 2022-02-07 16:40:02 UTC
* Number of recursive dependencies: 100

Run `cloud_details(, "saeSim")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘saeSim-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: agg_all
    > ### Title: Aggregation function
    > ### Aliases: agg_all
    > 
    > ### ** Examples
    > 
    > sim_base() %>% sim_gen_x() %>% sim_gen_e() %>% sim_agg(agg_all())
    Error: `arrange_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `arrange()` instead.
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘test-all.R’
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
       25. │ └─saeSim:::sim_setup(...)
       26. ├─saeSim::sim_gen_e(., 0, 4, name = "e")
       27. │ └─saeSim::sim_gen(simSetup, generator = gen_norm(mean, sd, name))
       28. │   └─saeSim:::sim_setup(...)
       29. ├─saeSim::sim_gen_x(., 0, 4, name = "x")
       30. │ └─saeSim::sim_gen(simSetup, generator = gen_norm(mean, sd, name))
       31. │   └─saeSim:::sim_setup(...)
       32. └─dplyr::arrange_(., "idD", "idU")
       33.   └─dplyr:::lazy_deprec("arrange")
       34.     └─lifecycle::deprecate_stop(...)
       35.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 32 | WARN 0 | SKIP 0 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Introduction.Rmd’ using rmarkdown
    Quitting from lines 31-39 (Introduction.Rmd) 
    Error: processing vignette 'Introduction.Rmd' failed with diagnostics:
    `arrange_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `arrange()` instead.
    --- failed re-building ‘Introduction.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Introduction.Rmd’
    
    Error: Vignette re-building failed.
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

# sf

<details>

* Version: 1.0-8
* GitHub: https://github.com/r-spatial/sf
* Source code: https://github.com/cran/sf
* Date/Publication: 2022-07-14 11:40:02 UTC
* Number of recursive dependencies: 154

Run `cloud_details(, "sf")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘aggregate.R’
      Comparing ‘aggregate.Rout’ to ‘aggregate.Rout.save’ ...4c4
    < Linking to GEOS 3.8.0, GDAL 3.0.4, PROJ 6.3.1; sf_use_s2() is TRUE
    ---
    > Linking to GEOS 3.10.2, GDAL 3.4.3, PROJ 8.2.0; sf_use_s2() is TRUE
    26c26
    < 1       1 1.5 POLYGON ((1 1, 1 0, 0 0, 0 ...
    ---
    > 1       1 1.5 POLYGON ((1 0, 0 0, 0 1, 1 ...
      Running ‘cast.R’
    ...
       4. ├─nc %>% select_("AREA", attr(., "sf_column")) %>% ...
       5. ├─base::inherits(., "sf")
       6. └─dplyr::select_(., "AREA", attr(., "sf_column"))
       7.   └─dplyr:::lazy_deprec("select")
       8.     └─lifecycle::deprecate_stop(...)
       9.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 1 | WARN 7 | SKIP 61 | PASS 714 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 25.1Mb
      sub-directories of 1Mb or more:
        doc      1.7Mb
        libs    18.7Mb
        sqlite   1.5Mb
    ```

# sfc

<details>

* Version: 0.1.0
* GitHub: https://github.com/ctfysh/sfc
* Source code: https://github.com/cran/sfc
* Date/Publication: 2016-08-25 10:01:01
* Number of recursive dependencies: 26

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
    > model <- system.file("extdata", "model_utf8.txt", package = "sfc")
    > sfc(data, model, sample.size = 100, fileEncoding = "UTF-8")
    Read 12 items
    Warning: `mutate_each()` was deprecated in dplyr 0.7.0.
    Please use `across()` instead.
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.
    Error in names(x0) <- w : attempt to set an attribute on NULL
    Calls: sfc -> impute_data
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# sfo

<details>

* Version: 0.1.1
* GitHub: NA
* Source code: https://github.com/cran/sfo
* Date/Publication: 2021-03-06 20:50:02 UTC
* Number of recursive dependencies: 70

Run `cloud_details(, "sfo")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘v1_intro.Rmd’ using rmarkdown
    --- finished re-building ‘v1_intro.Rmd’
    
    --- re-building ‘v3_analyzing_landing.Rmd’ using rmarkdown
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
    ...
    Error: processing vignette 'v3_analyzing_landing.Rmd' failed with diagnostics:
    `summarise_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `summarise()` instead.
    --- failed re-building ‘v3_analyzing_landing.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘v3_analyzing_landing.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# simPH

<details>

* Version: 1.3.13
* GitHub: https://github.com/christophergandrud/simPH
* Source code: https://github.com/cran/simPH
* Date/Publication: 2021-01-10 14:50:05 UTC
* Number of recursive dependencies: 69

Run `cloud_details(, "simPH")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘simPH-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: MinMaxLines
    > ### Title: Transform the simulation object to include only the min and max
    > ###   of the constricted intervals, as well as the lower and upper bounds
    > ###   of the middle 50 percent of the constricted intervals
    > ### Aliases: MinMaxLines
    > ### Keywords: internals
    > 
    ...
    +                       Xj = c(1237, 1600),
    +                       Xl = c(1000, 1000),
    +                       qi = "Hazard Ratio",
    +                       spin = TRUE, ci = 0.99)
    > 
    > # Find summary statistics of the constricted interval
    > Sum <- MinMaxLines(Sim1, clean = TRUE)
    Error: `group_by_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `group_by()` instead.
    Execution halted
    ```

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘simPH-overview.Rnw’ using knitr
    All Xl set to 0.
    Quitting from lines 309-319 (simPH-overview.Rnw) 
    Error: processing vignette 'simPH-overview.Rnw' failed with diagnostics:
    `group_by_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `group_by()` instead.
    --- failed re-building ‘simPH-overview.Rnw’
    
    SUMMARY: processing the following file failed:
      ‘simPH-overview.Rnw’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# simTool

<details>

* Version: 1.1.7
* GitHub: https://github.com/MarselScheer/simTool
* Source code: https://github.com/cran/simTool
* Date/Publication: 2020-09-22 16:00:03 UTC
* Number of recursive dependencies: 60

Run `cloud_details(, "simTool")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘simTool-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: eval_tibbles
    > ### Title: Workhorse for simulation studies
    > ### Aliases: eval_tibbles
    > 
    > ### ** Examples
    > 
    > rng <- function(data, ...) {
    ...
    +   expand_tibble(fun = "regData", n = 5L, SD = 1:2),
    +   expand_tibble(proc = "lm", formula = c("y~x", "y~I(x^2)")),
    +   post_analyze = purrr::compose(presever_rownames, coef, summary),
    +   # post_analyze = broom::tidy, # is a nice out of the box alternative
    +   summary_fun = list(mean = mean, sd = sd),
    +   group_for_summary = "term",
    +   replications = 3
    + )
    Error: The `.dots` argument of `group_by()` was deprecated in dplyr 1.0.0 and is now defunct.
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘tinytest.R’
    Running the tests in ‘tests/tinytest.R’ failed.
    Last 13 lines of output:
      test_eval_tibbles.R...........   18 tests [0;32mOK[0m 
      test_eval_tibbles.R...........   18 tests [0;32mOK[0m 
      test_eval_tibbles.R...........   19 tests [0;32mOK[0m 
      test_eval_tibbles.R...........   19 tests [0;32mOK[0m 
      test_eval_tibbles.R...........   20 tests [0;32mOK[0m 
      test_eval_tibbles.R...........   20 tests [0;32mOK[0m 
      test_eval_tibbles.R...........   21 tests [0;32mOK[0m 
      test_eval_tibbles.R...........   21 tests [0;32mOK[0m 
      test_eval_tibbles.R...........   22 tests [0;32mOK[0m 
      test_eval_tibbles.R...........   22 tests [0;32mOK[0m 
      test_eval_tibbles.R...........   23 tests [0;32mOK[0m 
      test_eval_tibbles.R...........   23 tests [0;32mOK[0m 
      test_eval_tibbles.R...........   24 tests [0;32mOK[0m 
      test_eval_tibbles.R...........   24 tests [0;32mOK[0m Error: The `.dots` argument of `group_by()` was deprecated in dplyr 1.0.0 and is now defunct.
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘simTool.Rmd’ using rmarkdown
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    ...
    Quitting from lines 45-59 (simTool.Rmd) 
    Error: processing vignette 'simTool.Rmd' failed with diagnostics:
    The `.dots` argument of `group_by()` was deprecated in dplyr 1.0.0 and is now defunct.
    --- failed re-building ‘simTool.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘simTool.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# simts

<details>

* Version: 0.2.0
* GitHub: https://github.com/SMAC-Group/simts
* Source code: https://github.com/cran/simts
* Date/Publication: 2022-01-03 17:50:02 UTC
* Number of recursive dependencies: 61

Run `cloud_details(, "simts")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘simts-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: best_model
    > ### Title: Select the Best Model
    > ### Aliases: best_model
    > 
    > ### ** Examples
    > 
    >  
    > set.seed(18)
    > xt = gen_arima(N=100, ar=0.3, d=1, ma=0.3)
    > x = select_arima(xt, d=1L)
    > best_model(x, ic = "aic")
    Error: `filter_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `filter()` instead.
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘vignettes.Rmd’ using rmarkdown
    
    warning: solve(): system is singular; attempting approx solution
    
    warning: solve(): system is singular; attempting approx solution
    
    warning: solve(): system is singular; attempting approx solution
    Quitting from lines 292-293 (vignettes.Rmd) 
    ...
    Error: processing vignette 'vignettes.Rmd' failed with diagnostics:
    `filter_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `filter()` instead.
    --- failed re-building ‘vignettes.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘vignettes.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 42.3Mb
      sub-directories of 1Mb or more:
        doc    1.5Mb
        libs  39.9Mb
    ```

# skater

<details>

* Version: 0.1.1
* GitHub: https://github.com/signaturescience/skater
* Source code: https://github.com/cran/skater
* Date/Publication: 2022-02-01 16:00:02 UTC
* Number of recursive dependencies: 94

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
     11.   └─dplyr (local) fn(col, ...)
     12.     └─vctrs::vec_cast(x = y, to = x, x_arg = "y", to_arg = "x")
     13.       └─vctrs (local) `<fn>`()
     14.         └─vctrs::vec_default_cast(...)
     15.           └─vctrs::stop_incompatible_cast(...)
     16.             └─vctrs::stop_incompatible_type(...)
     17.               └─vctrs:::stop_incompatible(...)
     18.                 └─vctrs:::stop_vctrs(...)
     19.                   └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       17. │             └─vctrs:::stop_incompatible(...)
       18. │               └─vctrs:::stop_vctrs(...)
       19. │                 └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
       20. │                   └─rlang:::signal_abort(cnd, .file)
       21. │                     └─base::signalCondition(cnd)
       22. ├─dplyr (local) `<fn>`(`<vctrs___>`)
       23. │ └─rlang::abort(bullets, call = call(setup$across_if_fn), parent = cnd)
       24. │   └─rlang:::signal_abort(cnd, .file)
       25. │     └─base::signalCondition(cnd)
       26. └─dplyr (local) `<fn>`(`<rlng_rrr>`)
       27.   └─rlang::abort(...)
      
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
    Problem while computing `..1 = dplyr::across(c(dadid, momid),
    dplyr::na_if, 0)`.
    Caused by error in `across()`:
    ! Problem while computing column `dadid`.
    Caused by error in `fn()`:
    ! Can't convert `y` <double> to match type of `x` <character>.
    --- failed re-building ‘basic_usage.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘basic_usage.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# spatPomp

<details>

* Version: 0.29.0.0
* GitHub: https://github.com/kidusasfaw/spatPomp
* Source code: https://github.com/cran/spatPomp
* Date/Publication: 2022-01-15 16:30:02 UTC
* Number of recursive dependencies: 52

Run `cloud_details(, "spatPomp")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘spatPomp-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: simulate
    > ### Title: Simulation of a spatiotemporal partially-observed Markov process
    > ### Aliases: simulate simulate-spatPomp simulate,spatPomp-method
    > 
    > ### ** Examples
    > 
    > # Get a spatPomp object
    > b <- bm(U=5, N=10)
    > # Get 10 simulations from same model as data.frame
    > sims <- simulate(b, nsim=10, format='data.frame')
    Error: `arrange_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `arrange()` instead.
    Execution halted
    ```

# stars

<details>

* Version: 0.5-6
* GitHub: https://github.com/r-spatial/stars
* Source code: https://github.com/cran/stars
* Date/Publication: 2022-07-21 12:10:02 UTC
* Number of recursive dependencies: 149

Run `cloud_details(, "stars")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘aggregate.R’
      Comparing ‘aggregate.Rout’ to ‘aggregate.Rout.save’ ...4c4
    < Linking to GEOS 3.8.0, GDAL 3.0.4, PROJ 6.3.1; sf_use_s2() is TRUE
    ---
    > Linking to GEOS 3.10.2, GDAL 3.4.3, PROJ 8.2.0; sf_use_s2() is TRUE
      Running ‘align.R’
      Comparing ‘align.Rout’ to ‘align.Rout.save’ ... OK
      Running ‘area.R’
      Comparing ‘area.Rout’ to ‘area.Rout.save’ ...52,64d51
    < /PRODUCT/longitude, 
    ...
      warn\[1\] does not match "Non-canonical axis order found, attempting to correct.".
      Actual value: "Each row in `x` should match at most 1 row in `y`\.\\nℹ Row 2 of `x` matches multiple rows\.\\nℹ If multiple matches are expected, specify `multiple = "all"` in the join call\\n  to silence this warning\."
      Backtrace:
          ▆
       1. └─testthat::expect_match(warn[1], "Non-canonical axis order found, attempting to correct.") at test_ncdf.R:155:2
       2.   └─testthat:::expect_match_(...)
      
      [ FAIL 1 | WARN 102 | SKIP 0 | PASS 161 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.5Mb
      sub-directories of 1Mb or more:
        doc   1.7Mb
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
    > st <- star_schema(mrs_age_test, dm_mrs_age) %>%
    +   role_playing_dimension(
    +     dim_names = c("when", "when_available"),
    +     name = "When Common",
    +     attributes = c("date", "week", "year")
    +   ) %>%
    +   character_dimensions(length_integers = list(week = 2),
    +                        NA_replacement_value = "Unknown")
    Error: The `.dots` argument of `group_by()` was deprecated in dplyr 1.0.0 and is now defunct.
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Backtrace:
          ▆
       1. ├─starschemar:::update_facts_with_dimensions(st_mrs_age_test, mod_dim) at test-update_facts_with_dimensions.R:6:2
       2. └─starschemar:::update_facts_with_dimensions.star_schema(...)
       3.   ├─starschemar:::group_table(st$fact[[1]])
       4.   └─starschemar:::group_table.fact_table(st$fact[[1]])
       5.     ├─dplyr::group_by(as.data.frame(ft), .dots = dim_keys)
       6.     └─dplyr:::group_by.data.frame(as.data.frame(ft), .dots = dim_keys)
       7.       └─dplyr::group_by_prepare(.data, ..., .add = .add, caller_env = caller_env())
       8.         └─lifecycle::deprecate_stop("1.0.0", "group_by(.dots = )")
       9.           └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 13 | WARN 0 | SKIP 0 | PASS 138 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘starschemar.Rmd’ using rmarkdown
    Quitting from lines 336-337 (starschemar.Rmd) 
    Error: processing vignette 'starschemar.Rmd' failed with diagnostics:
    The `.dots` argument of `group_by()` was deprecated in dplyr 1.0.0 and is now defunct.
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
* Number of recursive dependencies: 182

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
    15 12  5 
    > 
    > statVisual(type = "BiAxisErrBar",
    +   dat= mtcars,
    +   group = "gear",
    +   y.left = "mpg",
    +   y.right = "wt")
    Error: `group_by_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `group_by()` instead.
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
    Error: processing vignette 'statVisual.Rmd' failed with diagnostics:
    `group_by_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `group_by()` instead.
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

# strand

<details>

* Version: 0.2.0
* GitHub: https://github.com/strand-tech/strand
* Source code: https://github.com/cran/strand
* Date/Publication: 2020-11-19 21:40:06 UTC
* Number of recursive dependencies: 107

Run `cloud_details(, "strand")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘strand-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: strand-package
    > ### Title: strand: a framework for investment strategy simulation
    > ### Aliases: strand-package strand
    > 
    > ### ** Examples
    > 
    > # Load up sample data
    ...
    > config$to <- as.Date("2020-06-05")
    > 
    > # Create the Simulation object and run
    > sim <- Simulation$new(config,
    +                       raw_input_data = sample_inputs,
    +                       raw_pricing_data = sample_pricing,
    +                       security_reference_data = sample_secref)
    > sim$run()
    Error: The `.dots` argument of `group_by()` was deprecated in dplyr 1.0.0 and is now defunct.
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        3. │   └─... %>% select(-"weight_divisor")
        4. ├─dplyr::select(., -"weight_divisor")
        5. ├─tidyr::pivot_wider(...)
        6. ├─dplyr::mutate(., exposure = .data$exposure/.data$weight_divisor)
        7. ├─dplyr::left_join(., weight_divisor_df, by = "strategy")
        8. ├─dplyr::summarise(., exposure = sum(.data[[in_var]]))
        9. ├─dplyr::group_by(., .dots = c("strategy", cat_var))
       10. └─dplyr:::group_by.data.frame(., .dots = c("strategy", cat_var))
       11.   └─dplyr::group_by_prepare(.data, ..., .add = .add, caller_env = caller_env())
       12.     └─lifecycle::deprecate_stop("1.0.0", "group_by(.dots = )")
       13.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 61 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘strand.Rmd’ using rmarkdown
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    ...
    Quitting from lines 439-450 (strand.Rmd) 
    Error: processing vignette 'strand.Rmd' failed with diagnostics:
    The `.dots` argument of `group_by()` was deprecated in dplyr 1.0.0 and is now defunct.
    --- failed re-building ‘strand.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘strand.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked UTF-8 string
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
    +                                 "bdi_s7", "bdi_s8", "bdi_s9",
    +                                 "bdi_s10", "bdi_s11", "bdi_s12"),
    +                 sg_measure_name = "bdi",
    +                 multiple_sg_select = "largest")
    First, second, and third sudden gains criteria were applied.
    The critical value for the third criterion was adjusted for missingness.
    Error in `$<-.data.frame`(`*tmp*`, "call_text", value = c("suddengains::create_byperson(...)",  : 
      replacement has 22 rows, data has 18
    Calls: create_byperson ... trace_format -> trace_as_tree -> $<- -> $<-.data.frame
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
    Problem while computing `sg_change_proportion =
    dplyr::na_if(sg_change_proportion, "-Inf")`.
    Caused by error in `dplyr::na_if()`:
    ! Can't convert `y` <character> to match type of `x` <double>.
    --- failed re-building ‘suddengains-tutorial.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘suddengains-tutorial.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# surveybootstrap

<details>

* Version: 0.0.1
* GitHub: NA
* Source code: https://github.com/cran/surveybootstrap
* Date/Publication: 2016-05-04 12:14:27
* Number of recursive dependencies: 45

Run `cloud_details(, "surveybootstrap")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        5.       ├─base::do.call("rbind", rbsfn(survey.data = svy)) at test_variance.r:47:35
        6.       └─functional (local) rbsfn(survey.data = svy)
        7.         ├─base::do.call(FUN, c(.orig, list(...)))
        8.         ├─surveybootstrap (local) `<fn>`(...)
        9.         │ └─base::eval(boot.call, parent.frame())
       10.         │   └─base::eval(boot.call, parent.frame())
       11.         └─surveybootstrap (local) `<fn>`(survey.data = `<df[,12]>`, survey.design = ~CL, num.reps = 2000)
       12.           └─dplyr::group_indices_(survey.data, .dots = all.vars(psu.vars))
       13.             └─dplyr:::lazy_deprec("group_indices")
       14.               └─lifecycle::deprecate_stop(...)
       15.                 └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

# SurvivalPath

<details>

* Version: 1.3.2
* GitHub: https://github.com/zhangt369/SurvivalPath
* Source code: https://github.com/cran/SurvivalPath
* Date/Publication: 2022-07-03 18:30:05 UTC
* Number of recursive dependencies: 140

Run `cloud_details(, "SurvivalPath")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘SurvivalPath-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: EvolutionAfterTreatment
    > ### Title: Display node transition with specified treatment plan or
    > ###   exposure
    > ### Aliases: EvolutionAfterTreatment
    > 
    > ### ** Examples
    > 
    ...
      The number of classes in 3-th variable:Largest.Diameter.of.Hepatic.Lesions is 1,is lower than2
    Warning in FUN(X[[i]], ...) :
      The number of classes in 5-th variable:Vascular.Invasion is 1,is lower than2
    Warning in FUN(X[[i]], ...) :
      The number of classes in 7-th variable:Distant.Metastasis is 1,is lower than2
    Warning in FUN(X[[i]], ...) :
      The number of classes in 9-th variable:AFP is 1,is lower than2
    Error: `select_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `select()` instead.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘ggtree’
      All declared Imports should be used.
    ```

# survminer

<details>

* Version: 0.4.9
* GitHub: https://github.com/kassambara/survminer
* Source code: https://github.com/cran/survminer
* Date/Publication: 2021-03-09 09:50:03 UTC
* Number of recursive dependencies: 131

Run `cloud_details(, "survminer")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘survminer-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggsurvplot_combine
    > ### Title: Combine a List of Survfit Objects on the Same Plot
    > ### Aliases: ggsurvplot_combine
    > 
    > ### ** Examples
    > 
    > library(survival)
    ...
    >  # Fit
    >  pfs <- survfit( Surv(pfs.time, pfs.status) ~ 1, data = demo.data)
    >  os <- survfit( Surv(os.time, os.status) ~ 1, data = demo.data)
    >  # Combine on the same plot
    >  fit <- list(PFS = pfs, OS = os)
    >  ggsurvplot_combine(fit, demo.data)
    Error in `$<-.data.frame`(`*tmp*`, "call_text", value = c("survminer::ggsurvplot_combine(fit, demo.data)",  : 
      replacement has 19 rows, data has 18
    Calls: ggsurvplot_combine ... <Anonymous> -> lazy_deprec -> <Anonymous> -> deprecate_stop0
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.0Mb
      sub-directories of 1Mb or more:
        doc   5.5Mb
    ```

# SVMMaj

<details>

* Version: 0.2.9.1
* GitHub: NA
* Source code: https://github.com/cran/SVMMaj
* Date/Publication: 2022-05-23 08:02:18 UTC
* Number of recursive dependencies: 58

Run `cloud_details(, "SVMMaj")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘SVMMaj-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: svmmajcrossval
    > ### Title: k-fold Cross-Validation of SVM-Maj
    > ### Aliases: svmmajcrossval
    > 
    > ### ** Examples
    > 
    > 
    ...
        attribute dimension                768 8 
        degrees of freedom                 8 
        number of iterations               82 
        loss value                         398.6742 
        number of support vectors          403 
    > plot(results)
    > plot(results, 'profile')
    Error: `group_by_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `group_by()` instead.
    Execution halted
    ```

## In both

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘paper.Rnw’ using Sweave
    Scale for 'y' is already present. Adding another scale for 'y', which will
    replace the existing scale.
    Scale for 'y' is already present. Adding another scale for 'y', which will
    replace the existing scale.
    
    Attaching package: ‘kernlab’
    
    The following object is masked from ‘package:ggplot2’:
    ...
    l.55 \RequirePackage
                        [T1]{fontenc}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    --- failed re-building ‘paper.Rnw’
    
    SUMMARY: processing the following file failed:
      ‘paper.Rnw’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# sweep

<details>

* Version: 0.2.3
* GitHub: https://github.com/business-science/sweep
* Source code: https://github.com/cran/sweep
* Date/Publication: 2020-07-10 12:10:03 UTC
* Number of recursive dependencies: 165

Run `cloud_details(, "sweep")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘sweep-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: tidiers_StructTS
    > ### Title: Tidying methods for StructTS (Error, Trend, Seasonal) /
    > ###   exponential smoothing modeling of time series
    > ### Aliases: tidiers_StructTS sw_tidy.StructTS sw_glance.StructTS
    > ###   sw_augment.StructTS
    > 
    > ### ** Examples
    ...
    > sw_glance(fit_StructTS)
    # A tibble: 1 × 12
      model.d…¹ sigma logLik   AIC   BIC      ME  RMSE   MAE   MPE  MAPE  MASE  ACF1
      <chr>     <dbl>  <dbl> <dbl> <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    1 Local li… 0.995  -277.  559.  564. -0.0200  3.59  2.96 0.140  2.32 0.654 0.156
    # … with abbreviated variable name ¹​model.desc
    > sw_augment(fit_StructTS)
    Error: `select_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `select()` instead.
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Backtrace:
          ▆
       1. ├─sweep::sw_augment(fit_robets, rename_index = "date") at test_tidiers_robets.R:26:4
       2. ├─sweep:::sw_augment.robets(fit_robets, rename_index = "date")
       3. │ └─sweep:::sw_augment_columns(ret, data, rename_index, timetk_idx)
       4. │   └─sweep:::add_index(ret_2, rename_index)
       5. │     └─ret %>% dplyr::select_(rename_index, "dplyr::everything()")
       6. └─dplyr::select_(., rename_index, "dplyr::everything()")
       7.   └─dplyr:::lazy_deprec("select")
       8.     └─lifecycle::deprecate_stop(...)
       9.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 6 | WARN 0 | SKIP 0 | PASS 117 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘lubridate’ ‘tidyr’
      All declared Imports should be used.
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

# SWMPr

<details>

* Version: 2.4.1
* GitHub: https://github.com/fawda123/SWMPr
* Source code: https://github.com/cran/SWMPr
* Date/Publication: 2021-09-02 21:10:39 UTC
* Number of recursive dependencies: 101

Run `cloud_details(, "SWMPr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘SWMPr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_quants
    > ### Title: Create a plot of data for a single year overlaid on historical
    > ###   data.
    > ### Aliases: plot_quants plot_quants.swmpr
    > 
    > ### ** Examples
    > 
    > # qaqc
    > dat <- qaqc(apacpwq)
    > 
    > # generate a plot of salinity for 2013 overlaid on 2012-2013 data
    > plot_quants(dat, 'sal', yr = 2013, yrstart = 2012, yrend = 2013)
    Error: `select_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `select()` instead.
    Execution halted
    ```

# Tendril

<details>

* Version: 2.0.4
* GitHub: https://github.com/Karpefors/Tendril
* Source code: https://github.com/cran/Tendril
* Date/Publication: 2020-02-11 11:00:02 UTC
* Number of recursive dependencies: 120

Run `cloud_details(, "Tendril")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘Tendril-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: Tendril
    > ### Title: Tendril
    > ### Aliases: Tendril
    > 
    > ### ** Examples
    > 
    > data <- Tendril(mydata = TendrilData,
    ...
    + Terms = "ae",
    + Treat = "treatment",
    + StartDay = "day",
    + SubjList = SubjList,
    + SubjList.subject = "subjid",
    + SubjList.treatment = "treatment"
    + )
    Error: `rename__()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `rename_()` instead.
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        1. ├─testthat::expect_warning(...) at test_tendrilPerm.R:125:2
        2. │ └─testthat:::quasi_capture(enquo(object), label, capture_warnings)
        3. │   ├─testthat (local) .capture(...)
        4. │   │ └─base::withCallingHandlers(...)
        5. │   └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
        6. └─Tendril::Tendril(...) at test_tendrilPerm.R:126:4
        7.   └─Tendril:::dataSetup(...)
        8.     └─dplyr::rename_(data, Unique.Subject.Identifier = Unique.Subject.Identifier)
        9.       └─dplyr:::lazy_deprec("rename_")
       10.         └─lifecycle::deprecate_stop(...)
       11.           └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 11 | WARN 0 | SKIP 0 | PASS 12 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘TendrilUsage.Rmd’ using rmarkdown
    Quitting from lines 29-51 (TendrilUsage.Rmd) 
    Error: processing vignette 'TendrilUsage.Rmd' failed with diagnostics:
    `rename__()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `rename_()` instead.
    --- failed re-building ‘TendrilUsage.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘TendrilUsage.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘graphics’
      All declared Imports should be used.
    ```

# tidygenomics

<details>

* Version: 0.1.2
* GitHub: https://github.com/const-ae/tidygenomics
* Source code: https://github.com/cran/tidygenomics
* Date/Publication: 2019-08-08 11:50:02 UTC
* Number of recursive dependencies: 63

Run `cloud_details(, "tidygenomics")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘tidygenomics-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: genome_cluster
    > ### Title: Intersect data frames based on chromosome, start and end.
    > ### Aliases: genome_cluster
    > 
    > ### ** Examples
    > 
    > 
    ...
    
    > 
    > x1 <- data.frame(id = 1:4, bla=letters[1:4],
    +                  chromosome = c("chr1", "chr1", "chr2", "chr1"),
    +                  start = c(100, 120, 300, 260),
    +                  end = c(150, 250, 350, 450))
    > genome_cluster(x1, by=c("chromosome", "start", "end"))
    Error: `group_by_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `group_by()` instead.
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Error: `group_by_()` was deprecated in dplyr 0.7.0 and is now defunct.
      Please use `group_by()` instead.
      Backtrace:
          ▆
       1. ├─tidygenomics::genome_subtract(...) at test_subtract.R:57:2
       2. │ └─... %>% regroup()
       3. └─tidygenomics (local) regroup(.)
       4.   └─dplyr::group_by_(d, .dots = g)
       5.     └─dplyr:::lazy_deprec("group_by")
       6.       └─lifecycle::deprecate_stop(...)
       7.         └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 8 | WARN 5 | SKIP 1 | PASS 11 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘intro.Rmd’ using rmarkdown
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    ...
    Error: processing vignette 'intro.Rmd' failed with diagnostics:
    `group_by_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `group_by()` instead.
    --- failed re-building ‘intro.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘intro.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# tidygraph

<details>

* Version: 1.2.1
* GitHub: https://github.com/thomasp85/tidygraph
* Source code: https://github.com/cran/tidygraph
* Date/Publication: 2022-04-05 16:00:02 UTC
* Number of recursive dependencies: 75

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
      ── Failure (test-group.R:77:3): grouping with fixed number of groups ───────────
      get_number_of_groups(gr, group_walktrap(n_groups = 7)) not equal to 7.
      Modes: list, numeric
      names for target but not for current
      Attributes: < Modes: list, NULL >
      Attributes: < Lengths: 2, 0 >
      Attributes: < names for target but not for current >
      Attributes: < current is not list-like >
      
      [ FAIL 4 | WARN 23 | SKIP 0 | PASS 276 ]
      Error: Test failures
      Execution halted
    ```

# tidypredict

<details>

* Version: 0.4.9
* GitHub: https://github.com/tidymodels/tidypredict
* Source code: https://github.com/cran/tidypredict
* Date/Publication: 2022-05-25 19:20:02 UTC
* Number of recursive dependencies: 114

Run `cloud_details(, "tidypredict")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      <lifecycle_error_deprecated/defunctError/error/condition/condition>
      Error: `mutate_()` was deprecated in dplyr 0.7.0 and is now defunct.
      Please use `mutate()` instead.
      Backtrace:
          ▆
       1. ├─... %>% round(5) at test_xgboost.R:158:0
       2. ├─dplyr::mutate(., yhat_sql = 1 - yhat_sql)
       3. └─dplyr::mutate_(., yhat_sql = as.character(xgb_reglogistic_basescore_sql)[[3]])
       4.   └─dplyr:::lazy_deprec("mutate")
       5.     └─lifecycle::deprecate_stop(...)
       6.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 1 | WARN 0 | SKIP 11 | PASS 105 ]
      Error: Test failures
      Execution halted
    ```

# tidysmd

<details>

* Version: 0.1.0
* GitHub: https://github.com/malcolmbarrett/tidysmd
* Source code: https://github.com/cran/tidysmd
* Date/Publication: 2021-10-25 07:00:02 UTC
* Number of recursive dependencies: 59

Run `cloud_details(, "tidysmd")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘ellipsis’
      All declared Imports should be used.
    ```

# timelineS

<details>

* Version: 0.1.1
* GitHub: https://github.com/daheelee/timelineS
* Source code: https://github.com/cran/timelineS
* Date/Publication: 2016-08-22 14:13:31
* Number of recursive dependencies: 36

Run `cloud_details(, "timelineS")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘timelineS-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: durPlot
    > ### Title: Graphs and Summary for Date Durations
    > ### Aliases: durPlot
    > 
    > ### ** Examples
    > 
    > durPlot(life_exp, start = "Birth", end = "Death", group = "Country",
    + timeunit = "years", facet = TRUE, binwidth = 3, alpha = 0.7, title = TRUE)
    Error: `group_by_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `group_by()` instead.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘base’
      All declared Imports should be used.
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

# traineR

<details>

* Version: 1.7.4
* GitHub: NA
* Source code: https://github.com/cran/traineR
* Date/Publication: 2022-04-29 23:20:02 UTC
* Number of recursive dependencies: 107

Run `cloud_details(, "traineR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘traineR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: categorical.predictive.power
    > ### Title: categorical.predictive.power
    > ### Aliases: categorical.predictive.power
    > 
    > ### ** Examples
    > 
    > 
    > cars <- datasets::mtcars
    > cars$cyl <- as.factor(cars$cyl)
    > cars$vs <- as.factor(cars$vs)
    > categorical.predictive.power(cars,"vs","cyl")
    Error: `group_by_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `group_by()` instead.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘knitr’ ‘rmarkdown’ ‘rpart.plot’
      All declared Imports should be used.
    ```

# trelliscopejs

<details>

* Version: 0.2.6
* GitHub: https://github.com/hafen/trelliscopejs
* Source code: https://github.com/cran/trelliscopejs
* Date/Publication: 2021-02-01 08:00:02 UTC
* Number of recursive dependencies: 106

Run `cloud_details(, "trelliscopejs")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
           ▆
        1. ├─d %>% arrange(-mean_city_mpg) %>% ... at test-trelliscope.R:41:2
        2. ├─trelliscopejs::trelliscope(., name = "city_vs_highway_mpg", thumb = FALSE)
        3. ├─trelliscopejs:::trelliscope.data.frame(...)
        4. │ └─trelliscopejs:::cog_df_info(...)
        5. │   └─trelliscopejs:::find_sort_cols(x[setdiff(atomic_cols, cond_cols)])
        6. │     └─res %>% filter_(~!is.na(dir))
        7. └─dplyr::filter_(., ~!is.na(dir))
        8.   └─dplyr:::lazy_deprec("filter")
        9.     └─lifecycle::deprecate_stop(...)
       10.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

# tvgeom

<details>

* Version: 1.0.1
* GitHub: NA
* Source code: https://github.com/cran/tvgeom
* Date/Publication: 2019-12-07 01:40:02 UTC
* Number of recursive dependencies: 71

Run `cloud_details(, "tvgeom")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘introduction.Rmd’ using rmarkdown
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    ...
    Error: processing vignette 'introduction.Rmd' failed with diagnostics:
    `filter_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `filter()` instead.
    --- failed re-building ‘introduction.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘introduction.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# TwoRegression

<details>

* Version: 0.1.2
* GitHub: https://github.com/paulhibbing/TwoRegression
* Source code: https://github.com/cran/TwoRegression
* Date/Publication: 2018-03-19 11:16:07 UTC
* Number of recursive dependencies: 57

Run `cloud_details(, "TwoRegression")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Please use `summarise()` instead.
      Backtrace:
          ▆
       1. ├─TwoRegression::hibbing18_twoReg_process(...) at test-IMU_master_function.R:55:2
       2. │ └─TwoRegression::read_IMU(IMU, verbose = verbose)
       3. │   └─TwoRegression:::imu_collapse(AG, meta$block_size, verbose = verbose)
       4. │     └─AG %>% dplyr::group_by_(~epoch) %>% ...
       5. └─dplyr::summarise_(...)
       6.   └─dplyr:::lazy_deprec("summarise")
       7.     └─lifecycle::deprecate_stop(...)
       8.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 15 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘TwoRegression.Rmd’ using rmarkdown
    Quitting from lines 40-54 (TwoRegression.Rmd) 
    Error: processing vignette 'TwoRegression.Rmd' failed with diagnostics:
    `summarise_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `summarise()` instead.
    --- failed re-building ‘TwoRegression.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘TwoRegression.Rmd’
    
    Error: Vignette re-building failed.
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
    4     1     2 chr       b        NA
    5     2     2 int       <NA>      3
    6     3     2 int       <NA>      4
    > 
    > # Strip the cells in row 1 (the original headers) and use them as data
    > behead(cells, "N", foo)
    Error in `$<-.data.frame`(`*tmp*`, "call_text", value = c("unpivotr::behead(cells, \"N\", foo)",  : 
      replacement has 13 rows, data has 10
    Calls: behead ... trace_format -> trace_as_tree -> $<- -> $<-.data.frame
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(unpivotr)
      > 
      > test_check("unpivotr")
      [ FAIL 13 | WARN 0 | SKIP 0 | PASS 153 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      Error in `$<-.data.frame`(`*tmp*`, "call_text", value = c("... %>% dplyr::select(-row, -col, -data_type, -chr)",  : 
        replacement has 18 rows, data has 15
      Calls: test_check ... trace_format -> trace_as_tree -> $<- -> $<-.data.frame
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
    
    > ### Name: moveToFront
    > ### Title: moveToFront
    > ### Aliases: moveToFront moveToBack
    > 
    > ### ** Examples
    > 
    > theDF <- data.frame(A=1:10, B=11:20, C=1:10, D=11:20)
    > moveToFront(theDF, c('B', 'C'))
    Error: `select_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `select()` instead.
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Backtrace:
          ▆
       1. ├─testthat::expect_equal(...) at test-move-cols.r:55:4
       2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
       3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
       4. ├─useful::moveToFront(theDF, c("B", "C"))
       5. │ └─data %>% select_(.dots = colOrder)
       6. └─dplyr::select_(., .dots = colOrder)
       7.   └─dplyr:::lazy_deprec("select")
       8.     └─lifecycle::deprecate_stop(...)
       9.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 3 | WARN 3 | SKIP 2 | PASS 728 ]
      Error: Test failures
      Execution halted
    ```

# vctrs

<details>

* Version: 0.4.1
* GitHub: https://github.com/r-lib/vctrs
* Source code: https://github.com/cran/vctrs
* Date/Publication: 2022-04-13 10:30:02 UTC
* Number of recursive dependencies: 71

Run `cloud_details(, "vctrs")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        5. │   └─testthat:::expect_condition_matching(...)
        6. │     └─testthat:::quasi_capture(...)
        7. │       ├─testthat (local) .capture(...)
        8. │       │ └─base::withCallingHandlers(...)
        9. │       └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
       10. ├─vctrs:::vec_ptype_common_df_fallback(...)
       11. │ └─vctrs:::vec_ptype_common_params(..., .ptype = .ptype, .df_fallback = DF_FALLBACK_warn) at tests/testthat/helper-vctrs.R:10:2
       12. │   └─vctrs:::vec_ptype_common_opts(...)
       13. └─base::loadNamespace(x)
       14.   ├─base::namespaceImport(...)
       15.   └─base::loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]])
      
      [ FAIL 12 | WARN 0 | SKIP 196 | PASS 4767 ]
      Error: Test failures
      Execution halted
    ```

# vpc

<details>

* Version: 1.2.2
* GitHub: https://github.com/ronkeizer/vpc
* Source code: https://github.com/cran/vpc
* Date/Publication: 2021-01-11 20:20:02 UTC
* Number of recursive dependencies: 60

Run `cloud_details(, "vpc")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘vpc-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: vpc_tte
    > ### Title: VPC function for time-to-event (survival) data
    > ### Aliases: vpc_tte
    > 
    > ### ** Examples
    > 
    > ## See vpc-docs.ronkeizer.com for more documentation and examples.
    ...
    > 
    > # treat RTTE as TTE, no stratification
    > vpc_tte(sim = rtte_sim_nm[rtte_sim_nm$sim <= 20,],
    +        obs = rtte_obs_nm,
    +        rtte = FALSE,
    +        sim_cols=list(dv = "dv", idv = "t"), obs_cols=list(idv = "t"))
    Initializing.
    Error: `group_by_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `group_by()` instead.
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘test-add_sim_index_number.R’
    Running the tests in ‘tests/test-add_sim_index_number.R’ failed.
    Last 13 lines of output:
      +   mutate(sex = round(runif(1))) # randomly assign a "sex" covariate
      > sim <- sim_data(obs, # the design of the dataset
      +                 model = function(x) { # the model
      +                   vpc:::pk_oral_1cmt (t = x$time, dose=x$dose * x$wt, ka = x$ka, ke = x$ke, cl = x$cl * x$wt)
      +                 },
      +                 error = list(additive = 0.1),
      +                 theta = c(2.774, 0.0718, .0361),                 # parameter values
      +                 omega_mat = c(0.08854,                           # specified as lower triangle by default;
      +                               0.02421, 0.02241,                  # note: assumed that every theta has iiv, set to 0 if no iiv.
      +                               0.008069, 0.008639, 0.02862),
      +                 par_names = c("ka", "ke", "cl"),                 # link the parameters in the model to the thetas/omegas
      +                 n = 500)
      Error: `arrange_()` was deprecated in dplyr 0.7.0 and is now defunct.
      Please use `arrange()` instead.
      Execution halted
    ```

# widyr

<details>

* Version: 0.1.4
* GitHub: https://github.com/dgrtwo/widyr
* Source code: https://github.com/cran/widyr
* Date/Publication: 2021-08-12 17:10:02 UTC
* Number of recursive dependencies: 109

Run `cloud_details(, "widyr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘widyr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: pairwise_count
    > ### Title: Count pairs of items within a group
    > ### Aliases: pairwise_count pairwise_count_
    > 
    > ### ** Examples
    > 
    > 
    ...
    +                          "a", "c",
    +                          "a", "c",
    +                          "b", "e",
    +                          "b", "f"))
    > 
    > # count the number of times two letters appear together
    > pairwise_count(dat, letter, group)
    Error: `distinct_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `distinct()` instead.
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        1. ├─mtcars %>% group_by(cyl) %>% pairwise_count(vs, am) at test-pairwise-count.R:104:2
        2. ├─widyr::pairwise_count(., vs, am)
        3. │ └─widyr::pairwise_count_(...)
        4. │   └─... %>% rename(n = value)
        5. ├─dplyr::rename(., n = value)
        6. ├─widyr (local) func(., item, feature, wt)
        7. ├─dplyr::mutate(., ..value = 1)
        8. └─dplyr::distinct_(., .dots = c(item, feature), .keep_all = TRUE)
        9.   └─dplyr:::lazy_deprec("distinct")
       10.     └─lifecycle::deprecate_stop(...)
       11.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 4 | WARN 0 | SKIP 0 | PASS 47 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘gapminder’
      All declared Imports should be used.
    ```

# wrangle

<details>

* Version: 0.5.7
* GitHub: NA
* Source code: https://github.com/cran/wrangle
* Date/Publication: 2022-01-03 18:20:02 UTC
* Number of recursive dependencies: 19

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

# xspliner

<details>

* Version: 0.0.4
* GitHub: https://github.com/ModelOriented/xspliner
* Source code: https://github.com/cran/xspliner
* Date/Publication: 2019-09-25 20:20:02 UTC
* Number of recursive dependencies: 180

Run `cloud_details(, "xspliner")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘xspliner-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_variable_transition
    > ### Title: Plot variable profile
    > ### Aliases: plot_variable_transition
    > 
    > ### ** Examples
    > 
    > library(randomForest)
    ...
    randomForest 4.7-1.1
    Type rfNews() to see new features/changes/bug fixes.
    > set.seed(1)
    > data <- iris
    > # regression model
    > iris.rf <- randomForest(Petal.Width ~  Sepal.Length + Petal.Length + Species, data = data)
    > iris.xs <- xspline(iris.rf)
    Error: `arrange_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `arrange()` instead.
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘automation.Rmd’ using rmarkdown
    
    Attaching package: 'xspliner'
    
    The following object is masked from 'package:graphics':
    
        xspline
    
    randomForest 4.7-1.1
    ...
    
        margin
    
    --- finished re-building ‘xspliner.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘automation.Rmd’ ‘cases.Rmd’ ‘discrete.Rmd’ ‘methods.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

