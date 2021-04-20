# anomalize

<details>

* Version: 0.2.2
* GitHub: https://github.com/business-science/anomalize
* Source code: https://github.com/cran/anomalize
* Date/Publication: 2020-10-20 18:50:03 UTC
* Number of recursive dependencies: 177

Run `cloud_details(, "anomalize")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
     19. ├─dplyr:::mutate.data.frame(...)
     20. │ └─dplyr:::mutate_cols(.data, ...)
     21. │   ├─base::withCallingHandlers(...)
     22. │   └─mask$eval_all_mutate(quo)
     23. ├─purrr::map(.x = data, .f = .f, target = count, ...)
     24. │ ├─anomalize:::.f(.x[[i]], ...)
     25. │ └─anomalize:::time_decompose.tbl_df(.x[[i]], ...)
     26. │   ├─anomalize::prep_tbl_time(data, message = message)
     27. │   └─anomalize:::prep_tbl_time.data.frame(data, message = message)
     28. │     └─data %>% tibbletime::as_tbl_time(index = !!rlang::sym(idx))
     29. ├─tibbletime::as_tbl_time
     30. │ └─base::getExportedValue(pkg, name)
     31. │   └─base::asNamespace(ns)
     32. │     └─base::getNamespace(ns)
     33. │       └─base::loadNamespace(name)
     34. │         └─base::namespaceImportFrom(...)
     35. │           └─base::importIntoEnv(impenv, impnames, ns, impvars)
     36. │             └─base::stop(...)
     37. └─base::.handleSimpleError(...)
     38.   └─dplyr:::h(simpleError(msg, call))
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       27. │     └─data %>% tibbletime::as_tbl_time(index = !!rlang::sym(idx))
       28. ├─tibbletime::as_tbl_time
       29. │ └─base::getExportedValue(pkg, name)
       30. │   └─base::asNamespace(ns)
       31. │     └─base::getNamespace(ns)
       32. │       └─base::loadNamespace(name)
       33. │         └─base::namespaceImportFrom(...)
       34. │           └─base::importIntoEnv(impenv, impnames, ns, impvars)
       35. │             └─base::stop(...)
       36. └─base::.handleSimpleError(...)
       37.   └─dplyr:::h(simpleError(msg, call))
      
      [ FAIL 29 | WARN 0 | SKIP 0 | PASS 18 ]
      Error: Test failures
      Execution halted
    ```

# AQuadtree

<details>

* Version: 1.0.2
* GitHub: NA
* Source code: https://github.com/cran/AQuadtree
* Date/Publication: 2020-09-08 07:50:04 UTC
* Number of recursive dependencies: 83

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

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        data   3.8Mb
    ```

# arealDB

<details>

* Version: 0.3.4
* GitHub: NA
* Source code: https://github.com/cran/arealDB
* Date/Publication: 2020-07-01 09:10:06 UTC
* Number of recursive dependencies: 74

Run `cloud_details(, "arealDB")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘arealDB-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: makeExampleDB
    > ### Title: Build an example database
    > ### Aliases: makeExampleDB
    > 
    > ### ** Examples
    > 
    > # to merely register a set of files
    > makeExampleDB(until = "regTable")
    I have created a new project directory.
      -> please run 'setVariables()' to create a translation table for territories.
    Warning: The `path` argument of `write_csv()` is deprecated as of readr 1.4.0.
    Please use the `file` argument instead.
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_warnings()` to see where this warning was generated.
    please type in the path to the local folder where the licence is stored: 
    Error: The `.dots` argument of `group_by()` was deprecated in dplyr 1.0.0 and is now defunct.
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        1. ├─arealDB::makeExampleDB(until = "normGeometry") test_10_normTable.R:26:2
        2. │ └─arealDB::regDataseries(...)
        3. │   └─arealDB:::updateTable(index = temp, name = "inv_dataseries")
        4. │     └─`%>%`(...)
        5. ├─dplyr::arrange(., !!as.name(colnames(index)[1]))
        6. ├─dplyr::filter(., row_number() == 1)
        7. ├─dplyr::group_by(., .dots = keepCols)
        8. └─dplyr:::group_by.data.frame(., .dots = keepCols)
        9.   └─dplyr::group_by_prepare(.data, ..., .add = .add)
       10.     └─lifecycle::deprecate_stop("1.0.0", "group_by(.dots = )")
       11.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 12 | WARN 3 | SKIP 0 | PASS 31 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 8 marked UTF-8 strings
    ```

# auctestr

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/auctestr
* Date/Publication: 2017-11-13 09:46:18 UTC
* Number of recursive dependencies: 49

Run `cloud_details(, "auctestr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    
    > ### Name: auc_compare
    > ### Title: Compare AUC values using the FBH method.
    > ### Aliases: auc_compare
    > 
    > ### ** Examples
    > 
    > ## load sample experiment data
    > data(sample_experiment_data)
    > ## compare VariantA of ModelA and ModelB
    > auc_compare(sample_experiment_data,
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
          █
       1. ├─testthat::expect_equal(...) test-auc-compare.R:5:2
       2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
       3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
       4. ├─auctestr::auc_compare(...)
       5. │ └─`%>%`(...)
       6. └─dplyr::filter_(., filter_str)
       7.   └─dplyr:::lazy_defunct("filter")
       8.     └─lifecycle::deprecate_stop(...)
       9.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 10 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking whether package ‘auctestr’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘tidyr’ was built under R version 4.0.4
    See ‘/tmp/workdir/auctestr/new/auctestr.Rcheck/00install.out’ for details.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tidyr’
      All declared Imports should be used.
    ```

# autoMrP

<details>

* Version: 0.98
* GitHub: https://github.com/retowuest/autoMrP
* Source code: https://github.com/cran/autoMrP
* Date/Publication: 2021-01-21 11:00:09 UTC
* Number of recursive dependencies: 97

Run `cloud_details(, "autoMrP")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > # Minimal example without machine learning
    > m <- auto_MrP(
    +   y = "YES",
    +   L1.x = c("L1x1"),
    +   L2.x = c("L2.x1", "L2.x2"),
    +   L2.unit = "state",
    +   bin.proportion = "proportion",
    +   survey = taxes_survey,
    +   census = taxes_census,
    +   ebma.size = 0,
    +   cores = max_cores,
    +   best.subset = FALSE,
    +   lasso = FALSE,
    +   pca = FALSE,
    +   gb = FALSE,
    +   svm = FALSE,
    +   mrp = TRUE
    + )
    Starting post-stratification
    Error: The `.dots` argument of `group_by()` was deprecated in dplyr 1.0.0 and is now defunct.
    Execution halted
    ```

# AzureKusto

<details>

* Version: 1.0.6
* GitHub: https://github.com/Azure/AzureKusto
* Source code: https://github.com/cran/AzureKusto
* Date/Publication: 2020-04-27 05:30:02 UTC
* Number of recursive dependencies: 65

Run `cloud_details(, "AzureKusto")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Backtrace:
          █
       1. ├─tbl_iris %>% tidyr::nest(data = c(-Species)) test_translate.r:645:8
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

# banter

<details>

* Version: 0.9.3
* GitHub: NA
* Source code: https://github.com/cran/banter
* Date/Publication: 2018-07-10 15:20:06 UTC
* Number of recursive dependencies: 64

Run `cloud_details(, "banter")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Running examples in ‘banter-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: getBanterModel
    > ### Title: Extract Random Forest Model
    > ### Aliases: getBanterModel
    > 
    > ### ** Examples
    > 
    > data(train.data)
    > # initialize BANTER model with event data
    > bant.mdl <- initBanterModel(train.data$events)
    > # add all detector models
    > bant.mdl <- addBanterDetector(
    +   bant.mdl, train.data$detectors, 
    +   ntree = 50, sampsize = 1, num.cores = 1
    + )
    > # run BANTER event model
    > bant.mdl <- runBanterModel(bant.mdl, ntree = 1000, sampsize = 1)
    Error: The `.dots` argument of `group_by()` was deprecated in dplyr 1.0.0 and is now defunct.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘ranger’
      All declared Imports should be used.
    ```

# benthos

<details>

* Version: 1.3-6
* GitHub: NA
* Source code: https://github.com/cran/benthos
* Date/Publication: 2019-03-17 22:43:20 UTC
* Number of recursive dependencies: 69

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
    +      taxon = c("Euspira pulchella", "Nephtys cirrosa"), 
    +      count = c(4, 6)
    +  )
    Warning: `data_frame()` was deprecated in tibble 1.1.0.
    Please use `tibble()` instead.
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_warnings()` to see where this warning was generated.
    Error: `filter_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `filter()` instead.
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘test-all.R’
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
           █
        1. ├─testthat::expect_equal(...) test-indicators.R:236:4
        2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
        3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
        4. ├─benthos::iti(...)
        5. │ └─benthos::iti_(.data, lazy(taxon), lazy(count), lazy(group))
        6. │   └─d %>% select_(~GROUP, ~COUNT) %>% filter_(~!is.na(GROUP))
        7. └─dplyr::filter_(., ~!is.na(GROUP))
        8.   └─dplyr:::lazy_defunct("filter")
        9.     └─lifecycle::deprecate_stop(...)
       10.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 2 | WARN 31 | SKIP 1 | PASS 136 ]
      Error: Test failures
      Execution halted
    ```

# bioinactivation

<details>

* Version: 1.2.3
* GitHub: NA
* Source code: https://github.com/cran/bioinactivation
* Date/Publication: 2019-08-01 16:40:15 UTC
* Number of recursive dependencies: 70

Run `cloud_details(, "bioinactivation")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > model_data$parameters  # Set the model parameters
    [1] "k_b"       "temp_crit" "n"        
    > 
    > dummy_temp <- data.frame(time = c(0, 1.25, 2.25, 4.6),
    +                          temperature = c(70, 105, 105, 70))  # Dummy temp. profile
    > 
    > ## Set known parameters and initial points/bounds for unknown ones
    > 
    > known_params = c(temp_crit = 100)
    > 
    > starting_points <- c(n = 1, k_b = 0.25, N0 = 1e+05)
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

# bioOED

<details>

* Version: 0.2.1
* GitHub: NA
* Source code: https://github.com/cran/bioOED
* Date/Publication: 2019-08-07 15:00:02 UTC
* Number of recursive dependencies: 129

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

# bmlm

<details>

* Version: 1.3.11
* GitHub: https://github.com/mvuorre/bmlm
* Source code: https://github.com/cran/bmlm
* Date/Publication: 2019-02-21 21:30:03 UTC
* Number of recursive dependencies: 101

Run `cloud_details(, "bmlm")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... NOTE
    ```
    Missing or unexported object: ‘dplyr::summarize_’
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 60.7Mb
      sub-directories of 1Mb or more:
        libs  60.3Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# bootnet

<details>

* Version: 1.4.3
* GitHub: NA
* Source code: https://github.com/cran/bootnet
* Date/Publication: 2020-06-02 11:20:03 UTC
* Number of recursive dependencies: 162

Run `cloud_details(, "bootnet")` for more info

</details>

## Newly broken

*   checking whether package ‘bootnet’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/bootnet/new/bootnet.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘psychTools’
      All declared Imports should be used.
    ```

## Installation

### Devel

```
* installing *source* package ‘bootnet’ ...
** package ‘bootnet’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘summarize_’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘bootnet’
* removing ‘/tmp/workdir/bootnet/new/bootnet.Rcheck/bootnet’

```
### CRAN

```
* installing *source* package ‘bootnet’ ...
** package ‘bootnet’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (bootnet)

```
# capm

<details>

* Version: 0.14.0
* GitHub: NA
* Source code: https://github.com/cran/capm
* Date/Publication: 2019-10-24 16:50:05 UTC
* Number of recursive dependencies: 59

Run `cloud_details(, "capm")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > data(cluster_sample)
    > FreqTab(cluster_sample$number_of_dogs)
       Category Count Proportion
    1         0   327      0.338
    2         1   298      0.308
    3         2   201      0.208
    4         3    78      0.081
    5         4    33      0.034
    6         5    16      0.017
    7         7     6      0.006
    8         6     2      0.002
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
* Number of recursive dependencies: 59

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
    > library(magrittr)
    > outline_table(iris, 'Species') %>%
    +  add_rows(c('Sepal.Length', 'Petal.Length'), stat_meanSD) %>%
    +  add_rows('Sepal.Width', stat_medianIQR) %>%
    +  renaming('rows', function(x) gsub('Sepal\\.', 'Sepal ', x)) %>%
    +  renaming('header', c('Measures', 'Setosa', 'Versicolor', 'Virginica')) %>%
    +  build_table(caption = 'A caption for the table')
    Error: 'summarize_' is not an exported object from 'namespace:dplyr'
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
       20.   └─dplyr:::lazy_defunct("mutate")
       21.     └─lifecycle::deprecate_stop(...)
       22.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 2 | WARN 1 | SKIP 0 | PASS 19 ]
      Error: Test failures
      Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Missing or unexported object: ‘dplyr::summarize_’
    ```

# casen

<details>

* Version: 0.1.4
* GitHub: https://github.com/pachamaltese/casen
* Source code: https://github.com/cran/casen
* Date/Publication: 2020-04-08 06:00:02 UTC
* Number of recursive dependencies: 79

Run `cloud_details(, "casen")` for more info

</details>

## Newly broken

*   checking whether package ‘casen’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/casen/new/casen.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘janitor’ ‘tibble’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 75 marked UTF-8 strings
    ```

## Installation

### Devel

```
* installing *source* package ‘casen’ ...
** package ‘casen’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘summarize_’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘casen’
* removing ‘/tmp/workdir/casen/new/casen.Rcheck/casen’

```
### CRAN

```
* installing *source* package ‘casen’ ...
** package ‘casen’ successfully unpacked and MD5 sums checked
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
* DONE (casen)

```
# codified

<details>

* Version: 0.2.0
* GitHub: https://github.com/OuhscBbmc/codified
* Source code: https://github.com/cran/codified
* Date/Publication: 2018-09-30 16:10:02 UTC
* Number of recursive dependencies: 100

Run `cloud_details(, "codified")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### Aliases: table_nih_enrollment table_nih_enrollment_pretty
    > 
    > ### ** Examples
    > 
    > library(magrittr)
    > ds_1 <- tibble::tribble(
    +   ~subject_id,   ~gender  , ~race                      ,   ~ethnicity                     ,
    +            1L,   "Male"   , "Black or African American",  "Not Hispanic or Latino"        ,
    +            2L,   "Male"   , "Black or African American",  "Not Hispanic or Latino"        ,
    +            3L,   "Female" , "Black or African American",  "Unknown/Not Reported Ethnicity",
    +            4L,   "Male"   , "White"                    ,  "Not Hispanic or Latino"        ,
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
      ── Error (test-table-nih-enrollment.R:163:3): ds_2 --500 patients w/ numeric codes ──
      Error: `select_()` was deprecated in dplyr 0.7.0 and is now defunct.
      Please use `select()` instead.
      Backtrace:
          █
       1. ├─codified::table_nih_enrollment(...) test-table-nih-enrollment.R:163:2
       2. │ └─`%>%`(...)
       3. └─dplyr::select_(...)
       4.   └─dplyr:::lazy_defunct("select")
       5.     └─lifecycle::deprecate_stop(...)
       6.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 7 | WARN 0 | SKIP 0 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘methods’ ‘readr’
      All declared Imports should be used.
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
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error (test_plot_cofeature_mat.R:23:5): plot_cofeature_mat correctly adds tile borders ──
      Error: `mutate_()` was deprecated in dplyr 0.7.0 and is now defunct.
      Please use `mutate()` instead.
      Backtrace:
          █
       1. └─cofeatureR::plot_cofeature_mat(in_df, tile.col = cur_border_col) test_plot_cofeature_mat.R:23:4
       2.   └─dplyr::mutate_(in.df, .dots = setNames(list(mutate.call), "feature"))
       3.     └─dplyr:::lazy_defunct("mutate")
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

# CollapseLevels

<details>

* Version: 0.3.0
* GitHub: NA
* Source code: https://github.com/cran/CollapseLevels
* Date/Publication: 2020-06-04 13:20:02 UTC
* Number of recursive dependencies: 50

Run `cloud_details(, "CollapseLevels")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    The error most likely occurred in:
    
    > ### Name: IVCalc
    > ### Title: IVCalc
    > ### Aliases: IVCalc
    > 
    > ### ** Examples
    > 
    > 
    > # Load the German_Credit data set supplied with this package
    > 
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

# corporaexplorer

<details>

* Version: 0.8.4
* GitHub: https://github.com/kgjerde/corporaexplorer
* Source code: https://github.com/cran/corporaexplorer
* Date/Publication: 2021-03-18 02:40:02 UTC
* Number of recursive dependencies: 104

Run `cloud_details(, "corporaexplorer")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### Name: explore
    > ### Title: Launch Shiny app for exploration of text collection
    > ### Aliases: explore explore0
    > 
    > ### ** Examples
    > 
    > # Constructing test data frame:
    > dates <- as.Date(paste(2011:2020, 1:10, 21:30, sep = "-"))
    > texts <- paste0(
    +   "This is a document about ", month.name[1:10], ". ",
    +   "This is not a document about ", rev(month.name[1:10]), "."
    + )
    > titles <- paste("Text", 1:10)
    > test_df <- tibble::tibble(Date = dates, Text = texts, Title = titles)
    > 
    > # Converting to corporaexplorerobject:
    > corpus <- prepare_data(test_df, corpus_name = "Test corpus")
    Starting.
    Document data frame done.
    Error: The `.dots` argument of `group_by()` was deprecated in dplyr 1.0.0 and is now defunct.
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        1. └─corporaexplorer:::create_test_data() test_prep_func.R:9:2
        2.   ├─corporaexplorer::prepare_data(test_df)
        3.   └─corporaexplorer:::prepare_data.data.frame(test_df)
        4.     └─corporaexplorer:::transform_365(abc)
        5.       └─padr::pad(...)
        6.         └─padr:::set_to_original_type(return_frame, original_data_frame)
        7.           ├─dplyr::group_by(x, .dots = grps)
        8.           └─dplyr:::group_by.data.frame(x, .dots = grps)
        9.             └─dplyr::group_by_prepare(.data, ..., .add = .add)
       10.               └─lifecycle::deprecate_stop("1.0.0", "group_by(.dots = )")
       11.                 └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 1 | WARN 0 | SKIP 2 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘RColorBrewer’ ‘ggplot2’ ‘rmarkdown’ ‘shinyWidgets’ ‘shinydashboard’
      ‘shinyjs’
      All declared Imports should be used.
    ```

# countyfloods

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/countyfloods
* Date/Publication: 2017-10-26 03:22:55 UTC
* Number of recursive dependencies: 93

Run `cloud_details(, "countyfloods")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘countyfloods-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: find_nws
    > ### Title: Get National Weather Service (NWS) flood stage/discharge levels
    > ###   for gages.
    > ### Aliases: find_nws
    > 
    > ### ** Examples
    > 
    > 
    > va_counties <- get_county_cd("Virginia")
    Error: `filter_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `filter()` instead.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘maps’
      All declared Imports should be used.
    Missing or unexported object: ‘dplyr::summarize_’
    ```

# countytimezones

<details>

* Version: 1.0.0
* GitHub: https://github.com/geanders/countytimezones
* Source code: https://github.com/cran/countytimezones
* Date/Publication: 2016-10-22 12:17:29
* Number of recursive dependencies: 109

Run `cloud_details(, "countytimezones")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘countytimezones-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: add_local_time
    > ### Title: Add local time to dataset
    > ### Aliases: add_local_time
    > 
    > ### ** Examples
    > 
    > ex_df <- data.frame(datetime = c("1999-01-01 08:00", "1999-01-01 09:00",
    +                                  "1999-01-01 10:00"),
    +                     fips = c("36061", "17031", "06037"))
    > add_local_time(df = ex_df, fips = ex_df$fips,
    +                datetime_colname = "datetime")
    Error: `select_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `select()` instead.
    Execution halted
    ```

# countyweather

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/countyweather
* Date/Publication: 2016-10-26 00:17:31
* Number of recursive dependencies: 0

Run `cloud_details(, "countyweather")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... NOTE
    ```
    Missing or unexported object: ‘dplyr::summarize_’
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked UTF-8 string
    ```

# cpr

<details>

* Version: 0.2.3
* GitHub: https://github.com/dewittpe/cpr
* Source code: https://github.com/cran/cpr
* Date/Publication: 2017-03-07 13:41:34
* Number of recursive dependencies: 107

Run `cloud_details(, "cpr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > 
    > # Support
    > xvec <- seq(0, 6, length = 500)
    > 
    > # Define the basis matrix
    > bmat1 <- cpr::bsplines(x = xvec, iknots = c(1, 1.5, 2.3, 4, 4.5))
    > bmat2 <- cpr::bsplines(x = xvec)
    > 
    > # Define the control vertices ordinates
    > theta1 <- c(1, 0, 3.5, 4.2, 3.7, -0.5, -0.7, 2, 1.5)
    > theta2 <- c(1, 3.4, -2, 1.7)
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
        4. └─cpr:::recover_spline(start_with = 40L, progress = FALSE)
        5.   ├─base::suppressWarnings(do.call(cp, list(formula = f, data = s_data))) test-recover-know-spline.R:28:2
        6.   │ └─base::withCallingHandlers(...)
        7.   ├─base::do.call(cp, list(formula = f, data = s_data))
        8.   ├─(function (x, ...) ...
        9.   └─cpr:::cp.formula(...)
       10.     └─cpr:::generate_cp_formula_data(formula, data)
       11.       └─dplyr::select_(.data, .dots = vars_nobsplines_nobars)
       12.         └─dplyr:::lazy_defunct("select")
       13.           └─lifecycle::deprecate_stop(...)
       14.             └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 18 ]
      Error: Test failures
      Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Missing or unexported object: ‘dplyr::summarize_’
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.3Mb
      sub-directories of 1Mb or more:
        libs   5.7Mb
    ```

# crplyr

<details>

* Version: 0.3.8
* GitHub: https://github.com/Crunch-io/crplyr
* Source code: https://github.com/cran/crplyr
* Date/Publication: 2021-02-02 02:40:03 UTC
* Number of recursive dependencies: 122

Run `cloud_details(, "crplyr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        3. │   ├─testthat:::.capture(...)
        4. │   │ └─base::withCallingHandlers(...)
        5. │   └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
        6. ├─base::suppressWarnings(...)
        7. │ └─base::withCallingHandlers(...)
        8. └─dplyr::summarise_(...)
        9.   └─dplyr:::lazy_defunct("summarise")
       10.     └─lifecycle::deprecate_stop(...)
       11.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 5 | WARN 0 | SKIP 23 | PASS 168 ]
      Error: Test failures
      In addition: Warning message:
      package 'crunch' was built under R version 4.0.4 
      Execution halted
    ```

## In both

*   checking whether package ‘crplyr’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘crunch’ was built under R version 4.0.4
    See ‘/tmp/workdir/crplyr/new/crplyr.Rcheck/00install.out’ for details.
    ```

# crsra

<details>

* Version: 0.2.3
* GitHub: https://github.com/jhudsl/crsra
* Source code: https://github.com/cran/crsra
* Date/Publication: 2018-05-05 06:25:58 UTC
* Number of recursive dependencies: 61

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

# dat

<details>

* Version: 0.5.0
* GitHub: https://github.com/wahani/dat
* Source code: https://github.com/cran/dat
* Date/Publication: 2020-05-15 19:40:03 UTC
* Number of recursive dependencies: 72

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
          █
       1. ├─dat:::WITH_DPLYR(...) test-mutar.R:123:2
       2. └─dat::mutar(...) test-mutar.R:131:4
       3.   ├─dat:::handleCols(...)
       4.   └─dat:::handleCols(...)
       5.     └─dplyr::mutate_(x, .dots = args)
       6.       └─dplyr:::lazy_defunct("mutate")
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
* Number of recursive dependencies: 76

Run `cloud_details(, "DataCombine")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ###   for time-series cross-sectional data.
    > ### Aliases: CasesTable
    > 
    > ### ** Examples
    > 
    > # Create dummy data
    > ID <- rep(1:4, 4)
    > time <- rep(2000:2003, 4)
    > a <- rep(c(1:3, NA), 4)
    > b <- rep(c(1, NA, 3:4), 4)
    > Data <- data.frame(ID, time, a, b)
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

* Version: 0.1.3
* GitHub: https://github.com/capitalone/dataCompareR
* Source code: https://github.com/cran/dataCompareR
* Date/Publication: 2020-04-30 22:30:07 UTC
* Number of recursive dependencies: 51

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
           █
        1. ├─dataCompareR::rCompare(pressure, pressure2) testcreateReportText.R:158:2
        2. │ └─dataCompareR:::processFlow(...)
        3. │   └─dataCompareR:::prepareData(dfa, dfb, keys, trimChars)
        4. │     └─dataCompareR:::matchColumns(dfA, dfB)
        5. │       └─dataCompareR:::subsetDataColumns(DFA, DFB, colInfoList)
        6. │         └─DFA %>% select_(.dots = colInfoList[["commonCols"]])
        7. └─dplyr::select_(., .dots = colInfoList[["commonCols"]])
        8.   └─dplyr:::lazy_defunct("select")
        9.     └─lifecycle::deprecate_stop(...)
       10.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 70 | WARN 0 | SKIP 3 | PASS 170 ]
      Error: Test failures
      Execution halted
    ```

# ddpcr

<details>

* Version: 1.15
* GitHub: https://github.com/daattali/ddpcr
* Source code: https://github.com/cran/ddpcr
* Date/Publication: 2020-06-02 07:10:07 UTC
* Number of recursive dependencies: 96

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
          █
       1. └─ddpcr::new_plate(dir) test-v174.R:5:2
       2.   └─ddpcr:::read_plate(plate, dir, data_files, meta_file)
       3.     └─ddpcr:::read_dir(plate, dir)
       4.       └─ddpcr:::read_files(plate, data_files, meta_file)
       5.         └─base::tryCatch(...)
       6.           └─base:::tryCatchList(expr, classes, parentenv, handlers)
       7.             └─base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
       8.               └─value[[3L]](cond)
       9.                 └─ddpcr::err_msg("there was a problem reading one or more of the data files")
      
      [ FAIL 36 | WARN 0 | SKIP 0 | PASS 106 ]
      Error: Test failures
      Execution halted
    ```

# dmutate

<details>

* Version: 0.1.2
* GitHub: https://github.com/kylebmetrum/dmutate
* Source code: https://github.com/cran/dmutate
* Date/Publication: 2017-08-28 16:26:53 UTC
* Number of recursive dependencies: 36

Run `cloud_details(, "dmutate")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > 
    > mutate_random(data, AGE[40,90] ~ rnorm(55,50))
    Warning: `data_frame()` was deprecated in tibble 1.1.0.
    Please use `tibble()` instead.
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_warnings()` to see where this warning was generated.
       ID GROUP      AGE
    1   1     1 43.78661
    2   2     3 73.86978
    3   3     1 61.66682
    4   4     2 52.14466
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

# ech

<details>

* Version: 0.1.1.2
* GitHub: NA
* Source code: https://github.com/cran/ech
* Date/Publication: 2021-02-12 14:30:02 UTC
* Number of recursive dependencies: 123

Run `cloud_details(, "ech")` for more info

</details>

## Newly broken

*   checking whether package ‘ech’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/ech/new/ech.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1409 marked UTF-8 strings
    ```

## Installation

### Devel

```
* installing *source* package ‘ech’ ...
** package ‘ech’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘summarize_’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘ech’
* removing ‘/tmp/workdir/ech/new/ech.Rcheck/ech’

```
### CRAN

```
* installing *source* package ‘ech’ ...
** package ‘ech’ successfully unpacked and MD5 sums checked
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
* DONE (ech)

```
# eesim

<details>

* Version: 0.1.0
* GitHub: https://github.com/sakoehler7/eesim
* Source code: https://github.com/cran/eesim
* Date/Publication: 2017-06-03 17:55:52 UTC
* Number of recursive dependencies: 57

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

# egor

<details>

* Version: 0.21.02
* GitHub: https://github.com/tilltnet/egor
* Source code: https://github.com/cran/egor
* Date/Publication: 2021-02-12 14:20:02 UTC
* Number of recursive dependencies: 82

Run `cloud_details(, "egor")` for more info

</details>

## Newly broken

*   checking whether package ‘egor’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/egor/new/egor.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘egor’ ...
** package ‘egor’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘summarize_’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘egor’
* removing ‘/tmp/workdir/egor/new/egor.Rcheck/egor’

```
### CRAN

```
* installing *source* package ‘egor’ ...
** package ‘egor’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
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
* DONE (egor)

```
# emuR

<details>

* Version: 2.2.0
* GitHub: https://github.com/IPS-LMU/emuR
* Source code: https://github.com/cran/emuR
* Date/Publication: 2021-03-30 14:30:02 UTC
* Number of recursive dependencies: 99

Run `cloud_details(, "emuR")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        2. │ └─testthat:::quasi_capture(enquo(object), label, capture_warnings)
        3. │   ├─testthat:::.capture(...)
        4. │   │ └─base::withCallingHandlers(...)
        5. │   └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
        6. └─emuR:::check_emuRsegsForRequery(sl)
        7.   └─dplyr::arrange_(sl_df, "session", "bundle", "sample_start")
        8.     └─dplyr:::lazy_defunct("arrange")
        9.       └─lifecycle::deprecate_stop(...)
       10.         └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 1 | WARN 0 | SKIP 37 | PASS 841 ]
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
* Number of recursive dependencies: 85

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

# EpiSignalDetection

<details>

* Version: 0.1.1
* GitHub: NA
* Source code: https://github.com/cran/EpiSignalDetection
* Date/Publication: 2018-11-15 22:00:04 UTC
* Number of recursive dependencies: 87

Run `cloud_details(, "EpiSignalDetection")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    + disease = "Salmonellosis",
    + country = "EU-EEA - complete series",
    + indicator = "Reported cases",
    + stratification = "Confirmed cases",
    + unit = "Month",
    + daterange = c("2010-01-01", "2016-12-31"),
    + algo = "FarringtonFlexible",
    + testingperiod = 5
    + )
    > 
    > #-- Example dataset
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

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘DT’ ‘ggplot2’ ‘knitr’ ‘pander’
      All declared Imports should be used.
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
    ...
    > ### Aliases: grp_routine grp_routine_
    > 
    > ### ** Examples
    > 
    > df <- data.frame(v1 = letters[1:5], v2 = 1:5)
    > df
      v1 v2
    1  a  1
    2  b  2
    3  c  3
    4  d  4
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

# ezec

<details>

* Version: 1.0.1
* GitHub: https://github.com/grunwaldlab/ezec
* Source code: https://github.com/cran/ezec
* Date/Publication: 2016-12-05 08:27:32
* Number of recursive dependencies: 111

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
        6. │   │     ├─withr::with_output_sink(temp, withVisible(code))
        7. │   │     │ └─base::force(code)
        8. │   │     └─base::withVisible(code)
        9. │   └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
       10. ├─ezec::EC_table(...)
       11. │ └─`%>%`(...)
       12. └─dplyr::do_(...)
       13.   └─dplyr:::lazy_defunct("do")
       14.     └─lifecycle::deprecate_stop(...)
       15.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 2 | WARN 0 | SKIP 1 | PASS 2 ]
      Error: Test failures
      Execution halted
    ```

# factorMerger

<details>

* Version: 0.4.0
* GitHub: https://github.com/MI2DataLab/factorMerger
* Source code: https://github.com/cran/factorMerger
* Date/Publication: 2019-07-03 22:50:26 UTC
* Number of recursive dependencies: 166

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
           █
        1. ├─factorMerger::mergeFactors(dfWithoutCovariates$response, dfWithoutCovariates$factor) testvisualizations.R:4:0
        2. ├─factorMerger:::mergeFactors.default(...)
        3. │ └─factorMerger:::mergeLRT(fm, successive)
        4. │   └─factorMerger:::startMerging(factorMerger, successive, "LRT")
        5. │     └─factorMerger:::getIncreasingFactor(factorMerger)
        6. │       └─stats %>% arrange_("stat")
        7. └─dplyr::arrange_(., "stat")
        8.   └─dplyr:::lazy_defunct("arrange")
        9.     └─lifecycle::deprecate_stop(...)
       10.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 4 | WARN 0 | SKIP 0 | PASS 5 ]
      Error: Test failures
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
* Number of recursive dependencies: 64

Run `cloud_details(, "fastqcr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### Name: qc_aggregate
    > ### Title: Aggregate FastQC Reports for Multiple Samples
    > ### Aliases: qc_aggregate summary.qc_aggregate qc_stats
    > 
    > ### ** Examples
    > 
    > # Demo QC dir
    > qc.dir <- system.file("fastqc_results", package = "fastqcr")
    > qc.dir
    [1] "/tmp/workdir/fastqcr/new/fastqcr.Rcheck/fastqcr/fastqc_results"
    > 
    > # List of files in the directory
    > list.files(qc.dir)
    [1] "S1_fastqc.zip" "S2_fastqc.zip" "S3_fastqc.zip" "S4_fastqc.zip"
    [5] "S5_fastqc.zip"
    > 
    > # Aggregate the report
    > qc <- qc_aggregate(qc.dir, progressbar = FALSE)
    Error: `select_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `select()` instead.
    Execution halted
    ```

# funModeling

<details>

* Version: 1.9.4
* GitHub: https://github.com/pablo14/funModeling
* Source code: https://github.com/cran/funModeling
* Date/Publication: 2020-06-15 05:10:02 UTC
* Number of recursive dependencies: 80

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

# futureheatwaves

<details>

* Version: 1.0.3
* GitHub: NA
* Source code: https://github.com/cran/futureheatwaves
* Date/Publication: 2016-12-31 08:44:48
* Number of recursive dependencies: 80

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
      ── Error (test_preparation.R:13:1): (code run outside of `test_that()`) ────────
      Error: `select_()` was deprecated in dplyr 0.7.0 and is now defunct.
      Please use `select()` instead.
      Backtrace:
          █
       1. ├─futureheatwaves:::acquireDirectoryStructure(...) test_preparation.R:13:0
       2. │ └─`%>%`(...)
       3. └─dplyr::select_(., ~exp, ~model, ~ens, ~type)
       4.   └─dplyr:::lazy_defunct("select")
       5.     └─lifecycle::deprecate_stop(...)
       6.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 6 ]
      Error: Test failures
      Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Missing or unexported object: ‘dplyr::summarize_’
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

* Version: 3.8-1
* GitHub: https://github.com/IamKDO/GADMTools
* Source code: https://github.com/cran/GADMTools
* Date/Publication: 2020-03-05 12:30:08 UTC
* Number of recursive dependencies: 105

Run `cloud_details(, "GADMTools")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Running examples in ‘GADMTools-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: gadm_union
    > ### Title: Merges regions
    > ### Aliases: gadm_union gadm.union
    > ### Keywords: ~documentation ~utilities
    > 
    > ### ** Examples
    > 
    > library(GADMTools)
    > data("Corsica")
    > 
    > plotmap(Corsica)
    > 
    > Corse <- gadm_union(Corsica, level=2)
    although coordinates are longitude/latitude, st_union assumes that they are planar
    although coordinates are longitude/latitude, st_union assumes that they are planar
    Error: `select_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `select()` instead.
    Execution halted
    ```

## In both

*   checking whether package ‘GADMTools’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘sf’ was built under R version 4.0.4
    See ‘/tmp/workdir/GADMTools/new/GADMTools.Rcheck/00install.out’ for details.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 19 marked UTF-8 strings
    ```

# gender

<details>

* Version: 0.5.4
* GitHub: https://github.com/ropensci/gender
* Source code: https://github.com/cran/gender
* Date/Publication: 2020-05-15 09:10:02 UTC
* Number of recursive dependencies: 71

Run `cloud_details(, "gender")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    The following objects are masked from ‘package:stats’:
    
        filter, lag
    
    The following objects are masked from ‘package:base’:
    
        intersect, setdiff, setequal, union
    
    > demo_df <- tibble(names = c("Hillary", "Hillary", "Hillary",
    +                                 "Madison", "Madison"),
    +                       birth_year = c(1930, 2000, 1930, 1930, 2000),
    +                       min_year = birth_year - 1,
    +                       max_year = birth_year + 1,
    +                       stringsAsFactors = FALSE)
    > 
    > # Using the birth year for the predictions.
    > # Notice that the duplicate value for Hillary in 1930 is removed
    > gender_df(demo_df, method = "demo",
    +           name_col = "names", year_col = "birth_year")
    Error: The `.dots` argument of `group_by()` was deprecated in dplyr 1.0.0 and is now defunct.
    Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘genderdata’
    ```

# genogeographer

<details>

* Version: 0.1.19
* GitHub: NA
* Source code: https://github.com/cran/genogeographer
* Date/Publication: 2019-09-27 10:20:08 UTC
* Number of recursive dependencies: 126

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

# geogenr

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/geogenr
* Date/Publication: 2020-11-19 11:20:03 UTC
* Number of recursive dependencies: 88

Run `cloud_details(, "geogenr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > 
    > folder <- system.file("extdata", package = "geogenr")
    > folder <- stringr::str_replace_all(paste(folder, "/", ""), " ", "")
    > ua <- uscb_acs_5ye(folder = folder)
    > sa <- ua %>% get_statistical_areas()
    > # sa[6]
    > # [1] "New England City and Town Area Division"
    > ul <- uscb_layer(uscb_acs_metadata, ua = ua, geodatabase = sa[6], year = 2015)
    > layers <- ul %>% get_layer_names()
    > # layers[3]
    > # [1] "X02_RACE"
    > ul <- ul %>% get_layer(layers[3])
    > lg <- ul %>% get_layer_group_names()
    > # lg[2]
    > # [1] "003 - DETAILED RACE"
    > ul <- ul %>% get_layer_group(lg[2])
    > uf <- uscb_folder(ul)
    > 
    > gms <- uf %>% get_common_geomultistar()
    Error: The `.dots` argument of `group_by()` was deprecated in dplyr 1.0.0 and is now defunct.
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        5. │   ├─starschemar::star_schema(ft, dm) %>% starschemar::snake_case()
        6. │   └─starschemar::star_schema(ft, dm)
        7. │     └─starschemar:::new_star_schema(tibble::as_tibble(ft), sd)
        8. │       ├─starschemar:::group_table(star$fact[[1]])
        9. │       └─starschemar:::group_table.fact_table(star$fact[[1]])
       10. │         ├─dplyr::group_by(as.data.frame(ft), .dots = dim_keys)
       11. │         └─dplyr:::group_by.data.frame(as.data.frame(ft), .dots = dim_keys)
       12. │           └─dplyr::group_by_prepare(.data, ..., .add = .add)
       13. │             └─lifecycle::deprecate_stop("1.0.0", "group_by(.dots = )")
       14. │               └─lifecycle:::deprecate_stop0(msg)
       15. └─starschemar::snake_case(.)
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 22 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘generics’ ‘pander’ ‘purrr’ ‘readr’ ‘rlang’
      All declared Imports should be used.
    ```

# geomultistar

<details>

* Version: 1.1.0
* GitHub: NA
* Source code: https://github.com/cran/geomultistar
* Date/Publication: 2020-11-15 07:30:02 UTC
* Number of recursive dependencies: 77

Run `cloud_details(, "geomultistar")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    although coordinates are longitude/latitude, st_union assumes that they are planar
    although coordinates are longitude/latitude, st_union assumes that they are planar
    > 
    > gdq <- dimensional_query(gms) %>%
    +   select_dimension(name = "where",
    +                    attributes = c("state", "city")) %>%
    +   select_dimension(name = "when",
    +                    attributes = c("when_happened_year", "when_happened_week")) %>%
    +   select_fact(
    +     name = "mrs_age",
    +     measures = c("n_deaths")
    +   ) %>%
    +   select_fact(name = "mrs_cause",
    +               measures = c("pneumonia_and_influenza_deaths", "other_deaths")) %>%
    +   filter_dimension(name = "when", when_happened_week <= "03") %>%
    +   filter_dimension(name = "where", state == "MA")
    > 
    > sf <- gdq %>%
    +   run_geoquery()
    Error: The `.dots` argument of `group_by()` was deprecated in dplyr 1.0.0 and is now defunct.
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        3. └─geomultistar:::run_geoquery.dimensional_query(., attribute = "city")
        4.   ├─starschemar::run_query(dq, unify_by_grain)
        5.   └─starschemar:::run_query.dimensional_query(dq, unify_by_grain)
        6.     └─starschemar:::group_facts(dq)
        7.       ├─starschemar:::group_table(dq$output$fact[[name]])
        8.       └─starschemar:::group_table.fact_table(dq$output$fact[[name]])
        9.         ├─dplyr::group_by(as.data.frame(ft), .dots = dim_keys)
       10.         └─dplyr:::group_by.data.frame(as.data.frame(ft), .dots = dim_keys)
       11.           └─dplyr::group_by_prepare(.data, ..., .add = .add)
       12.             └─lifecycle::deprecate_stop("1.0.0", "group_by(.dots = )")
       13.               └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 16 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘pander’ ‘rgdal’ ‘snakecase’ ‘stringr’
      All declared Imports should be used.
    ```

# GFE

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/GFE
* Date/Publication: 2018-08-02 12:10:10 UTC
* Number of recursive dependencies: 20

Run `cloud_details(, "GFE")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > citaModI[(nCanT0 + 1), 1:nCanT1 ] 	 <-  (1-rhoMMI) * (1-psiI) * colSums(P * c(eta))
    > citaModI   <- round_preserve_sum(citaModI * N)
    > DBcitaModI <- createBase(citaModI)
    > 
    > # Creating auxiliary information
    > DBcitaModI[,AuxVar := rnorm(nrow(DBcitaModI), mean = 45, sd = 10)]
    > # Selects a sample with unequal probabilities
    > res <- S.piPS(n = 1200, as.data.frame(DBcitaModI)[,"AuxVar"])
    > sam <- res[,1]
    > pik <- res[,2]
    > DBcitaModISam <- copy(DBcitaModI[sam,])
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

# ggalluvial

<details>

* Version: 0.12.3
* GitHub: https://github.com/corybrunson/ggalluvial
* Source code: https://github.com/cran/ggalluvial
* Date/Publication: 2020-12-05 16:20:02 UTC
* Number of recursive dependencies: 121

Run `cloud_details(, "ggalluvial")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### Title: Flow positions
    > ### Aliases: stat_flow
    > 
    > ### ** Examples
    > 
    > # illustrate positioning
    > ggplot(as.data.frame(Titanic),
    +        aes(y = Freq,
    +            axis1 = Class, axis2 = Sex, axis3 = Age,
    +            color = Survived)) +
    +   stat_stratum(geom = "errorbar") +
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
          █
       1. └─StatFlow$compute_panel(data) test-stat-flow.r:78:2
       2.   └─ggalluvial:::f(..., self = self)
       3.     ├─dplyr::group_by(data, .dots = by_vars)
       4.     └─dplyr:::group_by.data.frame(data, .dots = by_vars)
       5.       └─dplyr::group_by_prepare(.data, ..., .add = .add)
       6.         └─lifecycle::deprecate_stop("1.0.0", "group_by(.dots = )")
       7.           └─lifecycle:::deprecate_stop0(msg)
      ── Failure (test-stat-flow.r:90:3): `stat_flow` handles exceptional data with out errors ──
      `ggplot_build(gg)` produced warnings.
      
      [ FAIL 5 | WARN 0 | SKIP 7 | PASS 47 ]
      Error: Test failures
      Execution halted
    ```

# ggedit

<details>

* Version: 0.3.1
* GitHub: https://github.com/yonicd/ggedit
* Source code: https://github.com/cran/ggedit
* Date/Publication: 2020-06-02 11:50:06 UTC
* Number of recursive dependencies: 87

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
    Warning: `as_data_frame()` was deprecated in tibble 2.0.0.
    Please use `as_tibble()` instead.
    The signature and semantics have changed, see `?as_tibble`.
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_warnings()` to see where this warning was generated.
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
* Number of recursive dependencies: 52

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

# ggfortify

<details>

* Version: 0.4.11
* GitHub: https://github.com/sinhrks/ggfortify
* Source code: https://github.com/cran/ggfortify
* Date/Publication: 2020-10-02 15:52:06 UTC
* Number of recursive dependencies: 124

Run `cloud_details(, "ggfortify")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggfortify-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: autoplot.aareg
    > ### Title: Autoplot 'survival::aareg'
    > ### Aliases: autoplot.aareg
    > 
    > ### ** Examples
    > 
    > library(survival)
    > autoplot(aareg(Surv(time, status) ~ age + sex + ph.ecog, data = lung, nmin = 1))
    Error: `mutate_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `mutate()` instead.
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘test-all.R’
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      Backtrace:
          █
       1. ├─ggplot2::autoplot(predict(d.var, n.ahead = 50)) test-vars.R:48:2
       2. └─ggfortify:::autoplot.varprd(predict(d.var, n.ahead = 50))
       3.   └─dplyr::filter_(plot.data, "!is.na(Data)")
       4.     └─dplyr:::lazy_defunct("filter")
       5.       └─lifecycle::deprecate_stop(...)
       6.         └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 14 | WARN 2 | SKIP 49 | PASS 618 ]
      Error: Test failures
      In addition: Warning message:
      In (function (kind = NULL, normal.kind = NULL, sample.kind = NULL)  :
        non-uniform 'Rounding' sampler used
      Execution halted
    ```

# ggguitar

<details>

* Version: 0.1.1
* GitHub: https://github.com/ezgraphs/ggguitar
* Source code: https://github.com/cran/ggguitar
* Date/Publication: 2016-12-24 19:51:07
* Number of recursive dependencies: 69

Run `cloud_details(, "ggguitar")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggguitar-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: select_chords
    > ### Title: Select chords by criteria
    > ### Aliases: select_chords
    > 
    > ### ** Examples
    > 
    > select_chords(chord_name = 'Major', string_5_fret = 3)
    Error: `filter_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `filter()` instead.
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘test_tablature.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── Error (test_chords.R:32:3): Selected chords include third fret ──────────────
      Error: `filter_()` was deprecated in dplyr 0.7.0 and is now defunct.
      Please use `filter()` instead.
      Backtrace:
          █
       1. └─ggguitar::select_chords("g", "Major", string_5_fret = 3) test_chords.R:32:2
       2.   └─ggguitar:::narrow_chord(chord, shQuote(chord_name), "chord")
       3.     └─dplyr::filter_(chords, filter_expr)
       4.       └─dplyr:::lazy_defunct("filter")
       5.         └─lifecycle::deprecate_stop(...)
       6.           └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 6 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘gridExtra’ ‘lazyeval’ ‘readr’
      All declared Imports should be used.
    ```

# ggmuller

<details>

* Version: 0.5.4
* GitHub: NA
* Source code: https://github.com/cran/ggmuller
* Date/Publication: 2019-09-05 02:10:17 UTC
* Number of recursive dependencies: 51

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

# glycanr

<details>

* Version: 0.4.0
* GitHub: https://github.com/iugrina/glycanr
* Source code: https://github.com/cran/glycanr
* Date/Publication: 2021-03-29 14:40:02 UTC
* Number of recursive dependencies: 72

Run `cloud_details(, "glycanr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    # A tibble: 261 x 4
       Plate gid   glycan    value
       <int> <chr> <fct>     <dbl>
     1     1 1_1   GP22      61102
     2     1 1_1   GP23     888300
     3     1 1_1   GP4    11223308
     4     1 1_2   GP16    1652175
     5     1 1_2   GP19     807443
     6     1 1_2   GP20     172025
     7     1 1_2   GP21     247997
     8     1 1_2   GP22      77734
     9     1 1_2   GP23    1030840
    10     1 1_2   GP24     733160
    # … with 251 more rows
    > 
    > # outliers per plate
    > glyco.outliers(mpiu, group="Plate")
    Warning: Version 0.3 of glycanr introduces a change in glyco.outliers function. It expects data frame input in long format now.
    Error: `group_by_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `group_by()` instead.
    Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘preprocessCore’
    ```

# goldi

<details>

* Version: 1.0.1
* GitHub: https://github.com/Chris1221/goldi
* Source code: https://github.com/cran/goldi
* Date/Publication: 2017-06-28 15:06:20 UTC
* Number of recursive dependencies: 70

Run `cloud_details(, "goldi")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Backtrace:
          █
       1. ├─testthat::expect_equal(...) test-goldi.R:23:2
       2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
       3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
       4. ├─goldi::goldi(...)
       5. │ └─TDM.df %>% select_("words", "counts")
       6. └─dplyr::select_(., "words", "counts")
       7.   └─dplyr:::lazy_defunct("select")
       8.     └─lifecycle::deprecate_stop(...)
       9.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 1 | WARN 7 | SKIP 0 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

# graphicalVAR

<details>

* Version: 0.2.4
* GitHub: NA
* Source code: https://github.com/cran/graphicalVAR
* Date/Publication: 2020-10-23 17:50:07 UTC
* Number of recursive dependencies: 89

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
    > Mod <- randomGVARmodel(4,probKappaEdge = 0.8,probBetaEdge = 0.8)
    > 
    > # Simulate data:
    > Data <- graphicalVARsim(100,Mod$beta,Mod$kappa)
    > 
    > # Estimate model:
    > Res <- graphicalVAR(Data, gamma = 0, nLambda = 10)
    Error: `mutate_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `mutate()` instead.
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Missing or unexported object: ‘dplyr::summarize_’
    ```

# graphTweets

<details>

* Version: 0.5.3
* GitHub: https://github.com/JohnCoene/graphTweets
* Source code: https://github.com/cran/graphTweets
* Date/Publication: 2020-01-08 09:00:08 UTC
* Number of recursive dependencies: 57

Run `cloud_details(, "graphTweets")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### Title: Edges from text
    > ### Aliases: gt_edges_from_text gt_edges_from_text_
    > 
    > ### ** Examples
    > 
    > # simulate dataset
    > tweets <- data.frame(
    +   text = c("I tweet @you about @him and @her", 
    +            "I tweet @me about @you"),
    +   screen_name = c("me", "him"),
    +   retweet_count = c(19, 5),
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

# gt

<details>

* Version: 0.2.2
* GitHub: https://github.com/rstudio/gt
* Source code: https://github.com/cran/gt
* Date/Publication: 2020-08-05 22:00:02 UTC
* Number of recursive dependencies: 96

Run `cloud_details(, "gt")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        5. ├─gt:::build_data(., context = "html")
        6. │ └─`%>%`(...)
        7. ├─gt:::resolve_footnotes_styles(., tbl_type = "styles")
        8. │ └─`%>%`(...)
        9. ├─dplyr::ungroup(.)
       10. ├─dplyr::summarize(., styles = list(as_style(styles)))
       11. ├─dplyr::group_by(...)
       12. └─dplyr:::group_by.data.frame(...)
       13.   └─dplyr::group_by_prepare(.data, ..., .add = .add)
       14.     └─lifecycle::deprecate_stop("1.0.0", "group_by(.dots = )")
       15.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 9 | WARN 0 | SKIP 0 | PASS 1981 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.1Mb
      sub-directories of 1Mb or more:
        help   4.0Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 116 marked UTF-8 strings
    ```

# highcharter

<details>

* Version: 0.8.2
* GitHub: https://github.com/jbkunst/highcharter
* Source code: https://github.com/cran/highcharter
* Date/Publication: 2020-07-26 08:50:12 UTC
* Number of recursive dependencies: 145

Run `cloud_details(, "highcharter")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘highcharter-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: hciconarray
    > ### Title: Shortcut to make icon arrays charts
    > ### Aliases: hciconarray
    > 
    > ### ** Examples
    > 
    > 
    > hciconarray(c("nice", "good"), c(10, 20))
    Warning: Use type 'item' instead (`hchart(df, "item", hcaes(name = labels, y = counts))`).
    Item chart provides better behaviour beside is a specific type of chart of HighchartsJS.
    Error: `arrange_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `arrange()` instead.
    Execution halted
    ```

# hurricaneexposure

<details>

* Version: 0.1.1
* GitHub: https://github.com/geanders/hurricaneexposure
* Source code: https://github.com/cran/hurricaneexposure
* Date/Publication: 2020-02-13 14:30:02 UTC
* Number of recursive dependencies: 73

Run `cloud_details(, "hurricaneexposure")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > # on installing the required data package.
    > if (requireNamespace("hurricaneexposuredata", quietly = TRUE)) {
    + 
    + map_counties("Floyd-1999", metric = "rainfall", days_included = c(-2, -1, 0, 1))
    + 
    + map_counties("Katrina-2005", metric = "wind")
    + map_counties("Katrina-2005", metric = "wind", wind_var = "vmax_gust")
    + map_counties("Katrina-2005", metric = "wind", wind_var = "sust_dur")
    + map_counties("Katrina-2005", metric = "wind", wind_source = "ext_tracks")
    + 
    + #' map_counties("Michael-2018", metric = "wind")
    + map_counties("Michael-2018", metric = "wind", wind_var = "vmax_gust")
    + map_counties("Michael-2018", metric = "wind", wind_source = "ext_tracks")
    + }
    Warning: `tbl_df()` was deprecated in dplyr 1.0.0.
    Please use `tibble::as_tibble()` instead.
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_warnings()` to see where this warning was generated.
    Error: `mutate_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `mutate()` instead.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘mapproj’
      All declared Imports should be used.
    ```

# huxtable

<details>

* Version: 5.2.0
* GitHub: https://github.com/hughjonesd/huxtable
* Source code: https://github.com/cran/huxtable
* Date/Publication: 2021-02-14 22:10:06 UTC
* Number of recursive dependencies: 157

Run `cloud_details(, "huxtable")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error (test-dplyr.R:62:3): mutate, mutate_ and transmute work ───────────────
      Error: `mutate_()` was deprecated in dplyr 0.7.0 and is now defunct.
      Please use `mutate()` instead.
      Backtrace:
          █
       1. └─dplyr::mutate_(ht, .dots = list(x = quote(a + b))) test-dplyr.R:62:2
       2.   └─dplyr:::lazy_defunct("mutate")
       3.     └─lifecycle::deprecate_stop(...)
       4.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 1 | WARN 2 | SKIP 23 | PASS 1191 ]
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

# iadf

<details>

* Version: 0.1.1
* GitHub: NA
* Source code: https://github.com/cran/iadf
* Date/Publication: 2020-03-26 10:10:19 UTC
* Number of recursive dependencies: 64

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

# IAT

<details>

* Version: 0.3
* GitHub: NA
* Source code: https://github.com/cran/IAT
* Date/Publication: 2016-04-30 00:51:43
* Number of recursive dependencies: 36

Run `cloud_details(, "IAT")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    
    > ### Name: cleanIAT
    > ### Title: Clean IAT data using the updated D-Scoring algorithm
    > ### Aliases: cleanIAT
    > 
    > ### ** Examples
    > 
    > # Get Ps who recieve Math-Male sorting task in first blocks
    > 
    > cong_first <- IATData[IATData$isCongruentFirst == 1, ]
    > 
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
* Number of recursive dependencies: 139

Run `cloud_details(, "idmodelr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
        intersect, setdiff, setequal, union
    
    > ## Extract a vector with no repeats
    > model_df_to_vector(iris, Petal.Length)
      [1] 1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 1.5 1.6 1.4 1.1 1.2 1.5 1.3 1.4
     [19] 1.7 1.5 1.7 1.5 1.0 1.7 1.9 1.6 1.6 1.5 1.4 1.6 1.6 1.5 1.5 1.4 1.5 1.2
     [37] 1.3 1.4 1.3 1.5 1.3 1.3 1.3 1.6 1.9 1.4 1.6 1.4 1.5 1.4 4.7 4.5 4.9 4.0
     [55] 4.6 4.5 4.7 3.3 4.6 3.9 3.5 4.2 4.0 4.7 3.6 4.4 4.5 4.1 4.5 3.9 4.8 4.0
     [73] 4.9 4.7 4.3 4.4 4.8 5.0 4.5 3.5 3.8 3.7 3.9 5.1 4.5 4.5 4.7 4.4 4.1 4.0
     [91] 4.4 4.6 4.0 3.3 4.2 4.2 4.2 4.3 3.0 4.1 6.0 5.1 5.9 5.6 5.8 6.6 4.5 6.3
    [109] 5.8 6.1 5.1 5.3 5.5 5.0 5.1 5.3 5.5 6.7 6.9 5.0 5.7 4.9 6.7 4.9 5.7 6.0
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
          █
       1. ├─idmodelr::scenario_analysis(...) test-scenario_analysis.R:45:12
       2. │ └─`%>%`(...)
       3. ├─dplyr::rename(., parameters = data)
       4. ├─tidyr::nest(.)
       5. ├─dplyr::group_by(., .dots = group_var_string)
       6. └─dplyr:::group_by.data.frame(., .dots = group_var_string)
       7.   └─dplyr::group_by_prepare(.data, ..., .add = .add)
       8.     └─lifecycle::deprecate_stop("1.0.0", "group_by(.dots = )")
       9.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 3 | WARN 0 | SKIP 41 | PASS 45 ]
      Error: Test failures
      Execution halted
    ```

# ipeadatar

<details>

* Version: 0.1.2
* GitHub: https://github.com/gomesleduardo/ipeadatar
* Source code: https://github.com/cran/ipeadatar
* Date/Publication: 2021-04-13 13:30:02 UTC
* Number of recursive dependencies: 26

Run `cloud_details(, "ipeadatar")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ipeadatar-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: metadata
    > ### Title: Returns a metadata about the requested series
    > ### Aliases: metadata
    > 
    > ### ** Examples
    > 
    > # Metadata about
    > # "JPM366_EMBI366": J.P. Morgan Emerging Markets Bond Index (EMBI+), Brazil
    > # "SGS366_NASDAQ366": Nasdaq Composite Index - closed
    > meta <- metadata(code = c("JPM366_EMBI366", "SGS366_NASDAQ366"))
    Requesting Ipeadata API <http://www.ipeadata.gov.br/>
    
      |                                                                            
      |                                                                      |   0%Error: `select_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `select()` instead.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘curl’
      All declared Imports should be used.
    ```

# ipft

<details>

* Version: 0.7.2
* GitHub: NA
* Source code: https://github.com/cran/ipft
* Date/Publication: 2018-01-04 09:36:52 UTC
* Number of recursive dependencies: 38

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

# iRF

<details>

* Version: 2.0.0
* GitHub: NA
* Source code: https://github.com/cran/iRF
* Date/Publication: 2017-07-26 04:57:45 UTC
* Number of recursive dependencies: 68

Run `cloud_details(, "iRF")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ###   [works for binary classification with continuous variables]
    > ### Aliases: readForest
    > 
    > ### ** Examples
    > 
    >   n = 50; p = 10
    >   X = array(rnorm(n*p), c(n, p))
    >   Y = (X[,1]>0.35 & X[,2]>0.35)|(X[,5]>0.35 & X[,7]>0.35)
    >   Y = as.factor(as.numeric(Y>0))
    > 
    >   train.id = 1:(n/2)
    >   test.id = setdiff(1:n, train.id)
    >   
    >   rf <- randomForest(x=X, y=Y, keep.forest=TRUE, track.nodes=TRUE,
    +     ntree=100)
    >   rforest <- readForest(rfobj=rf, x=X, n.core=2)
    Error in { : 
      task 1 failed - "`select_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `select()` instead."
    Calls: readForest -> %dopar% -> <Anonymous>
    Execution halted
    ```

# IsingSampler

<details>

* Version: 0.2.1
* GitHub: NA
* Source code: https://github.com/cran/IsingSampler
* Date/Publication: 2020-01-25 13:20:02 UTC
* Number of recursive dependencies: 21

Run `cloud_details(, "IsingSampler")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... NOTE
    ```
    Missing or unexported object: ‘dplyr::summarize_’
    ```

# kernelPhil

<details>

* Version: 0.1
* GitHub: NA
* Source code: https://github.com/cran/kernelPhil
* Date/Publication: 2021-02-25 10:50:02 UTC
* Number of recursive dependencies: 87

Run `cloud_details(, "kernelPhil")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Running examples in ‘kernelPhil-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: kernel.smooth.in.space
    > ### Title: Kernel smooth data in space alone
    > ### Aliases: kernel.smooth.in.space
    > 
    > ### ** Examples
    > 
    > n=400;
    > synthesised.data<-data.frame(x=stats::runif(n),y=stats::runif(n),
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

# LAGOSNE

<details>

* Version: 2.0.2
* GitHub: https://github.com/cont-limno/LAGOSNE
* Source code: https://github.com/cran/LAGOSNE
* Date/Publication: 2020-11-29 00:50:02 UTC
* Number of recursive dependencies: 117

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
        9.     └─LAGOSNE:::FUN(newX[, i], ...)
       10.       └─LAGOSNE:::lake_info_(...)
       11.         └─dplyr::filter_(dt_filter, filter_criteria)
       12.           └─dplyr:::lazy_defunct("filter")
       13.             └─lifecycle::deprecate_stop(...)
       14.               └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 2 | WARN 0 | SKIP 11 | PASS 3 ]
      Error: Test failures
      Execution halted
    ```

# Lahman

<details>

* Version: 9.0-0
* GitHub: https://github.com/cdalzell/Lahman
* Source code: https://github.com/cran/Lahman
* Date/Publication: 2021-04-09 06:10:06 UTC
* Number of recursive dependencies: 97

Run `cloud_details(, "Lahman")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    114398 NA NA
    116085 NA NA
    117807 NA NA
    119595 NA NA
    121370 NA NA
    123122 NA NA
    > 
    > # Table of games played by Pete Rose at different positions
    > with(subset(Fielding, playerID == "rosepe01"), xtabs(G ~ POS))
    POS
      1B   2B   3B   OF 
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

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.6Mb
      sub-directories of 1Mb or more:
        data   6.0Mb
    ```

# lifelogr

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/lifelogr
* Date/Publication: 2017-05-12 23:23:16 UTC
* Number of recursive dependencies: 92

Run `cloud_details(, "lifelogr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### ** Examples
    > 
    > data(EX)
    > 
    > dataset <- create_dataset(person = EX, all_variables = list("util" = c("month"), 
    +                                   "fitbit_daily" = c("sleepDuration", "steps",
    +                                   "restingHeartRate")), time_var = c("date"))
    >                                   
    > indiv_months <- data.frame("month"= c("Jan", "Feb", "Mar", "Apr", "May",
    +                                       "Jun", "Jul", "Aug", "Sep", "Oct", 
    +                                       "Nov", "Dec"),
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
        2. │ └─testthat:::quasi_capture(...)
        3. │   ├─testthat:::.capture(...)
        4. │   │ └─testthat::capture_output_lines(code, print, width = width)
        5. │   │   └─testthat:::eval_with_output(code, print = print, width = width)
        6. │   │     ├─withr::with_output_sink(temp, withVisible(code))
        7. │   │     │ └─base::force(code)
        8. │   │     └─base::withVisible(code)
        9. │   └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
       10. └─lifelogr::plot_i(EX, "steps")
       11.   └─dplyr::summarize_
       12.     └─base::getExportedValue(pkg, name)
      
      [ FAIL 1 | WARN 26 | SKIP 0 | PASS 53 ]
      Error: Test failures
      Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Missing or unexported object: ‘dplyr::summarize_’
    ```

# lmeresampler

<details>

* Version: 0.1.1
* GitHub: https://github.com/aloy/lmeresampler
* Source code: https://github.com/cran/lmeresampler
* Date/Publication: 2020-01-31 17:00:06 UTC
* Number of recursive dependencies: 51

Run `cloud_details(, "lmeresampler")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       5.         └─lifecycle:::deprecate_stop0(msg)
      ── Error (test-case-lme4.R:143:1): (code run outside of `test_that()`) ─────────
      Error: `group_by_()` was deprecated in dplyr 0.7.0 and is now defunct.
      Please use `group_by()` instead.
      Backtrace:
          █
       1. └─lmeresampler:::.cases.resamp(...) test-case-lme4.R:143:0
       2.   └─dplyr::group_by_(res, dots)
       3.     └─dplyr:::lazy_defunct("group_by")
       4.       └─lifecycle::deprecate_stop(...)
       5.         └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 2 | WARN 0 | SKIP 58 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘boot’
      All declared Imports should be used.
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
          █
       1. └─lookupTable::lookupTable(...) test.lookupTable.R:8:0
       2.   └─methods::new(...)
       3.     ├─methods::initialize(value, ...)
       4.     └─lookupTable:::initialize(value, ...)
       5.       └─lookupTable:::.local(.Object, ...)
       6.         └─dplyr::summarise_(...)
       7.           └─dplyr:::lazy_defunct("summarise")
       8.             └─lifecycle::deprecate_stop(...)
       9.               └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

# mase

<details>

* Version: 0.1.2
* GitHub: NA
* Source code: https://github.com/cran/mase
* Date/Publication: 2018-10-12 23:00:03 UTC
* Number of recursive dependencies: 66

Run `cloud_details(, "mase")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### ** Examples
    > 
    > library(survey)
    Loading required package: grid
    Loading required package: Matrix
    Loading required package: survival
    
    Attaching package: ‘survey’
    
    The following object is masked from ‘package:graphics’:
    
        dotchart
    
    > data(api)
    > gregElasticNet(y = apisrs$api00, 
    + x_sample = apisrs[c("col.grad", "awards", "snum", "dnum", "cnum", "pcttest", "meals", "sch.wide")], 
    + x_pop = apipop[c("col.grad", "awards", "snum", "dnum", "cnum", "pcttest", "meals", "sch.wide")], 
    + pi = apisrs$pw^(-1), var_est = TRUE, alpha = .5)
    Error: `select_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `select()` instead.
    Execution halted
    ```

## In both

*   checking whether package ‘mase’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘rpms’ was built under R version 4.0.4
    See ‘/tmp/workdir/mase/new/mase.Rcheck/00install.out’ for details.
    ```

# matman

<details>

* Version: 1.1.2
* GitHub: NA
* Source code: https://github.com/cran/matman
* Date/Publication: 2020-11-27 11:10:02 UTC
* Number of recursive dependencies: 128

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

# mlVAR

<details>

* Version: 0.4.4
* GitHub: NA
* Source code: https://github.com/cran/mlVAR
* Date/Publication: 2019-10-21 09:30:02 UTC
* Number of recursive dependencies: 110

Run `cloud_details(, "mlVAR")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... NOTE
    ```
    Missing or unexported object: ‘dplyr::summarize_’
    ```

*   checking R code for possible problems ... NOTE
    ```
    Mplus_mlVAR: no visible global function definition for ‘summarize_’
    print.mlVARcompare: no visible global function definition for
      ‘summarize_’
    Undefined global functions or variables:
      summarize_
    ```

# modelplotr

<details>

* Version: 1.1.0
* GitHub: https://github.com/jurrr/modelplotr
* Source code: https://github.com/cran/modelplotr
* Date/Publication: 2020-10-13 04:20:05 UTC
* Number of recursive dependencies: 130

Run `cloud_details(, "modelplotr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > #calculate ntile values
    > ntl_term.deposit <- (ntiles+1)-as.numeric(cut(prob_term.deposit,breaks=cutoffs,include.lowest=TRUE))
    > ntl_no.term.deposit <- (ntiles+1)-ntl_term.deposit
    > scores_and_ntiles <- scores_and_ntiles %>%
    +   rbind(
    +    test %>%
    +     select(has_td) %>%
    +     mutate(model_label=factor('logistic regression'),
    +            dataset_label=factor('test data'),
    +            y_true=factor(has_td),
    +            prob_term.deposit = prob_term.deposit,
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

# Momocs

<details>

* Version: 1.3.2
* GitHub: https://github.com/MomX/Momocs
* Source code: https://github.com/cran/Momocs
* Date/Publication: 2020-10-06 15:20:11 UTC
* Number of recursive dependencies: 134

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
    `norm=TRUE` is used and this may be troublesome. See ?efourier
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
* Number of recursive dependencies: 150

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

# mtconnectR

<details>

* Version: 1.2.1
* GitHub: NA
* Source code: https://github.com/cran/mtconnectR
* Date/Publication: 2019-01-07 19:00:22 UTC
* Number of recursive dependencies: 75

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
       1. +-mtconnectR::create_mtc_device_from_dmtcd(...) test-dataExtraction_mtc.R:13:0
       2. | \-mtconnectR::read_dmtcd_file(...)
       3. |   \-`%>%`(...)
       4. +-base::as.data.frame(.)
       5. \-dplyr::arrange_(., "timestamp")
       6.   \-dplyr:::lazy_defunct("arrange")
       7.     \-lifecycle::deprecate_stop(...)
       8.       \-lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 25 ]
      Error: Test failures
      Execution halted
    ```

# ncdump

<details>

* Version: 0.0.3
* GitHub: https://github.com/r-gris/ncdump
* Source code: https://github.com/cran/ncdump
* Date/Publication: 2017-05-02 12:35:30 UTC
* Number of recursive dependencies: 56

Run `cloud_details(, "ncdump")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        2. │ └─testthat:::condition(object)
        3. │   └─testthat::expect_is(x, class)
        4. │     └─testthat::quasi_label(enquo(object), label, arg = "object")
        5. │       └─rlang::eval_bare(expr, quo_get_env(quo))
        6. ├─ncdump:::dimvars(con)
        7. ├─ncdump:::dimvars.NetCDF(con)
        8. │ └─dims(x) %>% filter_("create_dimvar") %>% select_("name")
        9. └─dplyr::select_(., "name")
       10.   └─dplyr:::lazy_defunct("select")
       11.     └─lifecycle::deprecate_stop(...)
       12.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 1 | WARN 2 | SKIP 0 | PASS 6 ]
      Error: Test failures
      Execution halted
    ```

# networkreporting

<details>

* Version: 0.1.1
* GitHub: NA
* Source code: https://github.com/cran/networkreporting
* Date/Publication: 2016-12-05 18:28:47
* Number of recursive dependencies: 65

Run `cloud_details(, "networkreporting")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘test_all.R’
    Running the tests in ‘tests/test_all.R’ failed.
    Last 13 lines of output:
      ── Error (test_variance.R:40:1): (code run outside of `test_that()`) ───────────
      Error: `select_()` was deprecated in dplyr 0.7.0 and is now defunct.
      Please use `select()` instead.
      Backtrace:
          █
       1. └─networkreporting::kp.individual.estimator_(...) test_variance.R:40:0
       2.   └─networkreporting::kp.estimator_(...)
       3.     └─dplyr::select_(resp.data, .dots = weights)
       4.       └─dplyr:::lazy_defunct("select")
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
* Number of recursive dependencies: 21

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
* Number of recursive dependencies: 62

Run `cloud_details(, "nonmemica")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    1      NA       NA         NA     NA     NA       NA     NA     NA        NA
    2 0.00000  0.00000  0.0000000 10.039 30.407 0.047146 3.4744 113.27 -0.015125
    3 0.72107 -0.35807 -0.0831970 10.039 30.407 0.047146 3.4744 113.27 -0.015125
    4 1.32920 -0.41519  0.2558400 10.039 30.407 0.047146 3.4744 113.27 -0.015125
    5 2.26880 -1.14880 -0.4076200 10.039 30.407 0.047146 3.4744 113.27 -0.015125
    6 3.36580 -1.08580  0.0038197 10.039 30.407 0.047146 3.4744 113.27 -0.015125
         ETA2     ETA3
    1      NA       NA
    2 0.28832 -0.41548
    3 0.28832 -0.41548
    4 0.28832 -0.41548
    5 0.28832 -0.41548
    6 0.28832 -0.41548
    > 1001 %>% superset %>% filter(VISIBLE == 1) %>% group_by(ID,TIME) %>% status
    Source: local data frame [550 x 39]
    Groups:  ID TIME 
    NAs: 0
    duplicates: 0
    Error: `arrange_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `arrange()` instead.
    Execution halted
    ```

# nosoi

<details>

* Version: 1.0.3
* GitHub: https://github.com/slequime/nosoi
* Source code: https://github.com/cran/nosoi
* Date/Publication: 2020-05-12 21:30:02 UTC
* Number of recursive dependencies: 130

Run `cloud_details(, "nosoi")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        2. │ ├─tidytree::as.treedata(treeTable)
        3. │ └─tidytree:::as.treedata.tbl_tree(treeTable)
        4. │   ├─methods::new("treedata", phylo = as.phylo.tbl_tree(data))
        5. │   │ ├─methods::initialize(value, ...)
        6. │   │ └─methods::initialize(value, ...)
        7. │   └─tidytree:::as.phylo.tbl_tree(data)
        8. │     └─x %<>% mutate_(isTip = ~(!node %in% parent))
        9. └─dplyr::mutate_(., isTip = ~(!node %in% parent))
       10.   └─dplyr:::lazy_defunct("mutate")
       11.     └─lifecycle::deprecate_stop(...)
       12.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 5 | WARN 0 | SKIP 0 | PASS 457 ]
      Error: Test failures
      Execution halted
    ```

# omu

<details>

* Version: 1.0.4
* GitHub: https://github.com/connor-reid-tiffany/Omu
* Source code: https://github.com/cran/omu
* Date/Publication: 2020-05-18 19:40:02 UTC
* Number of recursive dependencies: 69

Run `cloud_details(, "omu")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘omu-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: PCA_plot
    > ### Title: Create a PCA plot
    > ### Aliases: PCA_plot
    > 
    > ### ** Examples
    > 
    > PCA_plot(count_data = c57_nos2KO_mouse_countDF, metadata = c57_nos2KO_mouse_metadata,
    + variable = "Treatment", color = "Treatment", response_variable = "Metabolite")
    Error: `select_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `select()` instead.
    Execution halted
    ```

# padr

<details>

* Version: 0.5.3
* GitHub: https://github.com/EdwinTh/padr
* Source code: https://github.com/cran/padr
* Date/Publication: 2020-09-12 05:50:35 UTC
* Number of recursive dependencies: 68

Run `cloud_details(, "padr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### ** Examples
    > 
    > library(dplyr)
    
    Attaching package: ‘dplyr’
    
    The following objects are masked from ‘package:stats’:
    
        filter, lag
    
    The following objects are masked from ‘package:base’:
    
        intersect, setdiff, setequal, union
    
    > library(ggplot2)
    > plot_set <- emergency %>%
    +   thicken("hour", "h") %>%
    +   count(h) %>%
    +   head(24)
    Error: The `.dots` argument of `group_by()` was deprecated in dplyr 1.0.0 and is now defunct.
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
           █
        1. ├─testthat::expect_equal(...) test_thicken_integration.R:76:2
        2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
        3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
        4. └─padr::thicken(coffee_month, "month")
        5.   └─padr:::set_to_original_type(return_frame, original_data_frame)
        6.     ├─dplyr::group_by(x, .dots = grps)
        7.     └─dplyr:::group_by.data.frame(x, .dots = grps)
        8.       └─dplyr::group_by_prepare(.data, ..., .add = .add)
        9.         └─lifecycle::deprecate_stop("1.0.0", "group_by(.dots = )")
       10.           └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 17 | WARN 0 | SKIP 0 | PASS 501 ]
      Error: Test failures
      Execution halted
    ```

# PAMpal

<details>

* Version: 0.9.14
* GitHub: NA
* Source code: https://github.com/cran/PAMpal
* Date/Publication: 2020-12-17 10:10:06 UTC
* Number of recursive dependencies: 153

Run `cloud_details(, "PAMpal")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > 
    > exPps <- new('PAMpalSettings')
    > exPps <- addDatabase(exPps, system.file('extdata', 'Example.sqlite3', package='PAMpal'))
    Adding 1 databases:
      > exPps <- addBinaries(exPps, system.file('extdata', 'Binaries', package='PAMpal'))
    Getting list of all binary files in folder. This may take a while...
    Adding 3 binary files from 1 folders
    > exClick <- function(data) {
    +     standardClickCalcs(data, calibration=NULL, filterfrom_khz = 0)
    + }
    > exPps <- addFunction(exPps, exClick, module = 'ClickDetector')
    Adding function "exClick":
    > exPps <- addFunction(exPps, roccaWhistleCalcs, module='WhistlesMoans')
    Adding function "roccaWhistleCalcs":
    > exPps <- addFunction(exPps, standardCepstrumCalcs, module = 'Cepstrum')
    Adding function "standardCepstrumCalcs":
    > # process events labelled within the Pamguard database
    > exStudyDb <- processPgDetections(exPps, mode='db', id='Example')
    Error: `select_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `select()` instead.
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        7.             ├─base::suppressWarnings(...)
        8.             │ └─base::withCallingHandlers(...)
        9.             ├─dplyr::bind_rows(...)
       10.             │ └─rlang::list2(...)
       11.             └─base::lapply(...)
       12.               └─PAMpal:::FUN(X[[i]], ...)
       13.                 └─PAMpal:::getDbData(db, x)
       14.                   └─dplyr::select_(allEvents, .dots = eventColumns)
       15.                     └─dplyr:::lazy_defunct("select")
       16.                       └─lifecycle::deprecate_stop(...)
       17.                         └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 72 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘rjson’ ‘rstudioapi’
      All declared Imports should be used.
    ```

# panelr

<details>

* Version: 0.7.5
* GitHub: https://github.com/jacob-long/panelr
* Source code: https://github.com/cran/panelr
* Date/Publication: 2021-01-18 17:00:02 UTC
* Number of recursive dependencies: 206

Run `cloud_details(, "panelr")` for more info

</details>

## Newly broken

*   checking whether package ‘panelr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/panelr/new/panelr.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘AER’
    ```

## Installation

### Devel

```
* installing *source* package ‘panelr’ ...
** package ‘panelr’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘summarize_’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘panelr’
* removing ‘/tmp/workdir/panelr/new/panelr.Rcheck/panelr’

```
### CRAN

```
* installing *source* package ‘panelr’ ...
** package ‘panelr’ successfully unpacked and MD5 sums checked
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
* DONE (panelr)

```
# photobiologyInOut

<details>

* Version: 0.4.22-1
* GitHub: NA
* Source code: https://github.com/cran/photobiologyInOut
* Date/Publication: 2020-04-14 19:40:05 UTC
* Number of recursive dependencies: 112

Run `cloud_details(, "photobiologyInOut")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       5.         └─lifecycle:::deprecate_stop0(msg)
      ── Error (test-oo.R:275:3): SpectraSuite-comma ─────────────────────────────────
      Error: `mutate_()` was deprecated in dplyr 0.7.0 and is now defunct.
      Please use `mutate()` instead.
      Backtrace:
          █
       1. └─photobiologyInOut::read_oo_ssirrad(file = file.name, locale = my.locale) test-oo.R:275:2
       2.   └─dplyr::mutate_(z, .dots = stats::setNames(dots, "s.e.irrad"))
       3.     └─dplyr:::lazy_defunct("mutate")
       4.       └─lifecycle::deprecate_stop(...)
       5.         └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 8 | WARN 1 | SKIP 4 | PASS 294 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking whether package ‘photobiologyInOut’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘photobiology’ was built under R version 4.0.4
    See ‘/tmp/workdir/photobiologyInOut/new/photobiologyInOut.Rcheck/00install.out’ for details.
    ```

# pkggraph

<details>

* Version: 0.2.3
* GitHub: https://github.com/talegari/pkggraph
* Source code: https://github.com/cran/pkggraph
* Date/Publication: 2018-11-15 09:50:03 UTC
* Number of recursive dependencies: 65

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

# PKNCA

<details>

* Version: 0.9.4
* GitHub: https://github.com/billdenney/pknca
* Source code: https://github.com/cran/PKNCA
* Date/Publication: 2020-06-01 17:00:02 UTC
* Number of recursive dependencies: 73

Run `cloud_details(, "PKNCA")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        4. ├─testthat::expect_warning(...)
        5. │ └─testthat:::quasi_capture(enquo(object), label, capture_warnings)
        6. │   ├─testthat:::.capture(...)
        7. │   │ └─base::withCallingHandlers(...)
        8. │   └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
        9. └─PKNCA::pk.tss(...)
       10.   └─PKNCA::pk.tss.monoexponential(..., check = check)
       11.     └─PKNCA:::pk.tss.monoexponential.population(...)
       12.       └─PKNCA:::tss.monoexponential.generate.formula(data)
       13.         └─dplyr::summarize_
       14.           └─base::getExportedValue(pkg, name)
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 1290 ]
      Error: Test failures
      Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Missing or unexported object: ‘dplyr::summarize_’
    ```

# plinkQC

<details>

* Version: 0.3.3
* GitHub: https://github.com/meyer-lab-cshl/plinkQC
* Source code: https://github.com/cran/plinkQC
* Date/Publication: 2021-02-08 22:40:02 UTC
* Number of recursive dependencies: 81

Run `cloud_details(, "plinkQC")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       5.         └─lifecycle:::deprecate_stop0(msg)
      ── Error (test-utils.R:165:5): relatednessFilter only returns one fail ID for 4-way relatedness ──
      Error: `select_()` was deprecated in dplyr 0.7.0 and is now defunct.
      Please use `select()` instead.
      Backtrace:
          █
       1. └─plinkQC::relatednessFilter(...) test-utils.R:165:4
       2.   └─dplyr::select_(relatedness, ~IID1, ~IID2, ~M)
       3.     └─dplyr:::lazy_defunct("select")
       4.       └─lifecycle::deprecate_stop(...)
       5.         └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 15 | WARN 0 | SKIP 0 | PASS 57 ]
      Error: Test failures
      Execution halted
    ```

# plotly

<details>

* Version: 4.9.3
* GitHub: https://github.com/ropensci/plotly
* Source code: https://github.com/cran/plotly
* Date/Publication: 2021-01-10 14:30:02 UTC
* Number of recursive dependencies: 159

Run `cloud_details(, "plotly")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Backtrace:
          █
       1. ├─`%>%`(...) test-plotly-group.R:58:0
       2. ├─plotly::add_markers(.)
       3. │ └─x %||% p$x$attrs[[1]][["x"]]
       4. ├─plotly::plot_ly(., x = ~wt, y = ~mpg)
       5. │ └─base::is.data.frame(data)
       6. └─dplyr::group_by_(., "rowname")
       7.   └─dplyr:::lazy_defunct("group_by")
       8.     └─lifecycle::deprecate_stop(...)
       9.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 2 | WARN 74 | SKIP 56 | PASS 1384 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.0Mb
      sub-directories of 1Mb or more:
        htmlwidgets   3.8Mb
    ```

# pmdplyr

<details>

* Version: 0.3.3
* GitHub: https://github.com/NickCH-K/pmdplyr
* Source code: https://github.com/cran/pmdplyr
* Date/Publication: 2020-05-30 07:30:02 UTC
* Number of recursive dependencies: 105

Run `cloud_details(, "pmdplyr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
                pred_degree_awarded_ipeds year earnings_med count_not_working
    100654-2007                         3 2007        36600               116
    100654-2009                         3 2009        32600               202
    100654-2011                         3 2011        31400               214
    100654-2012                         3 2012        30300               239
    100654-2013                         3 2013        29900               246
    100654-2014                         3 2014        31000               212
                count_working repay_rate
    100654-2007          1139         NA
    100654-2009          1410         NA
    100654-2011          1532         NA
    100654-2012          1601         NA
    100654-2013          1741  0.5118364
    100654-2014          1784  0.4202658
    > 
    > # And finally panel_data
    > if ("panelr" %in% pkgs) {
    +   head(panel_convert(S_pibble, to = "panelr"))
    + }
    Error: object ‘summarize_’ is not exported by 'namespace:dplyr'
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── Error (test-panel_convert.R:99:3): (code run outside of `test_that()`) ──────
      Error: object 'summarize_' is not exported by 'namespace:dplyr'
      Backtrace:
          █
       1. └─panelr::panel_data test-panel_convert.R:99:2
       2.   └─base::getExportedValue(pkg, name)
       3.     └─base::asNamespace(ns)
       4.       └─base::getNamespace(ns)
       5.         └─base::loadNamespace(name)
       6.           └─base::namespaceImportFrom(...)
       7.             └─base::importIntoEnv(impenv, impnames, ns, impvars)
      
      [ FAIL 1 | WARN 13 | SKIP 0 | PASS 301 ]
      Error: Test failures
      Execution halted
    ```

# pmpp

<details>

* Version: 0.1.1
* GitHub: https://github.com/MichalOleszak/pmpp
* Source code: https://github.com/cran/pmpp
* Date/Publication: 2019-10-15 22:10:02 UTC
* Number of recursive dependencies: 86

Run `cloud_details(, "pmpp")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘pmpp-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: create_fframe
    > ### Title: Add empty rows with time stamps to each cress-sectional unit in
    > ###   the panel
    > ### Aliases: create_fframe
    > 
    > ### ** Examples
    > 
    > data(EmplUK, package = "plm")
    > EmplUK <- dplyr::filter(EmplUK, year %in% c(1978, 1979, 1980, 1981, 1982))
    > my_fframe <- create_fframe(EmplUK, 1983)
    Error: `arrange_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `arrange()` instead.
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── Error (test-predinterval.R:10:3): bootstrap for prediction intervals works ──
      Error: `arrange_()` was deprecated in dplyr 0.7.0 and is now defunct.
      Please use `arrange()` instead.
      Backtrace:
          █
       1. ├─pmpp::create_fframe(EmplUK, 1983:1985) test-predinterval.R:10:2
       2. │ └─`%>%`(...)
       3. └─dplyr::arrange_(., unit_id, time_id)
       4.   └─dplyr:::lazy_defunct("arrange")
       5.     └─lifecycle::deprecate_stop(...)
       6.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 3 | WARN 0 | SKIP 0 | PASS 47 ]
      Error: Test failures
      Execution halted
    ```

# pointblank

<details>

* Version: 0.7.0
* GitHub: https://github.com/rich-iannone/pointblank
* Source code: https://github.com/cran/pointblank
* Date/Publication: 2021-03-09 07:50:14 UTC
* Number of recursive dependencies: 119

Run `cloud_details(, "pointblank")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > # aren't available and the `to`
    > # and `from` email addresses are
    > # nonexistent
    > 
    > # To get a blastula email object
    > # instead of eagerly sending the
    > # message, we can use the 
    > # `email_create()` function
    > email_object <-
    +   create_agent(
    +     read_fn = ~ small_table,
    +     tbl_name = "small_table",
    +     label = "An example.",
    +     actions = al
    +   ) %>%
    +   col_vals_gt(vars(a), value = 5) %>%
    +   col_vals_lt(vars(a), value = 7) %>%
    +   interrogate() %>%
    +   email_create()
    Error: The `.dots` argument of `group_by()` was deprecated in dplyr 1.0.0 and is now defunct.
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        8. ├─gt:::resolve_footnotes_styles(., tbl_type = "styles")
        9. │ └─`%>%`(...)
       10. ├─dplyr::ungroup(.)
       11. ├─dplyr::summarize(., styles = list(as_style(styles)))
       12. ├─dplyr::group_by(...)
       13. └─dplyr:::group_by.data.frame(...)
       14.   └─dplyr::group_by_prepare(.data, ..., .add = .add)
       15.     └─lifecycle::deprecate_stop("1.0.0", "group_by(.dots = )")
       16.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 1 | WARN 1 | SKIP 18 | PASS 1144 ]
      Error: Test failures
      In addition: Warning message:
      call dbDisconnect() when finished working with a connection 
      Execution halted
    ```

# pollen

<details>

* Version: 0.72.0
* GitHub: https://github.com/Nowosad/pollen
* Source code: https://github.com/cran/pollen
* Date/Publication: 2020-03-15 22:00:08 UTC
* Number of recursive dependencies: 74

Run `cloud_details(, "pollen")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘pollen-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: pollen_season
    > ### Title: A Pollen Season Function
    > ### Aliases: pollen_season
    > 
    > ### ** Examples
    > 
    > 
    > data(pollen_count)
    > df <- subset(pollen_count, site=='Oz')
    > pollen_season(value=df$birch, date=df$date, method="95")
    Error: `arrange_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `arrange()` instead.
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        1. ├─pollen::pollen_season(value = df$birch, date = df$date, method = "95") test-seasons.R:9:2
        2. │ └─`%>%`(...)
        3. ├─purrr::map_df(., rbind)
        4. │ └─purrr::map(.x, .f, ...)
        5. ├─purrr::map(...)
        6. └─purrr::map(., ~arrange_(., "date"))
        7.   └─pollen:::.f(.x[[i]], ...)
        8.     └─dplyr::arrange_(., "date")
        9.       └─dplyr:::lazy_defunct("arrange")
       10.         └─lifecycle::deprecate_stop(...)
       11.           └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 1 | WARN 7 | SKIP 0 | PASS 9 ]
      Error: Test failures
      Execution halted
    ```

# poppr

<details>

* Version: 2.9.1
* GitHub: https://github.com/grunwaldlab/poppr
* Source code: https://github.com/cran/poppr
* Date/Publication: 2021-03-21 20:20:03 UTC
* Number of recursive dependencies: 120

Run `cloud_details(, "poppr")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... NOTE
    ```
    Missing or unexported object: ‘dplyr::summarize_’
    ```

# PPforest

<details>

* Version: 0.1.1
* GitHub: https://github.com/natydasilva/PPforest
* Source code: https://github.com/cran/PPforest
* Date/Publication: 2018-06-11 18:46:17 UTC
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
    > 
    > pprf.crab <- PPforest(data = crab, class = 'Type',
    +  std = FALSE, size.tr = 1, m = 200, size.p = .5, PPmethod = 'LDA' , parallel = TRUE, cores = 2)
    Warning: `data_frame()` was deprecated in tibble 1.1.0.
    Please use `tibble()` instead.
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_warnings()` to see where this warning was generated.
    Error: `filter_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `filter()` instead.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.4Mb
      sub-directories of 1Mb or more:
        libs   7.2Mb
    ```

# psycho

<details>

* Version: 0.6.1
* GitHub: https://github.com/neuropsychology/psycho.R
* Source code: https://github.com/cran/psycho
* Date/Publication: 2021-01-19 06:40:10 UTC
* Number of recursive dependencies: 75

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
          █
       1. ├─psycho::crawford.test(...) test-assess.R:42:2
       2. │ └─`%>%`(...)
       3. ├─ggplot2::ggplot(., aes_string(x = "x", ymin = 0, ymax = "y"))
       4. └─dplyr::mutate_(., x = "scales::rescale(x, from=c(0, 1), to = c(sample_mean, sample_mean+sample_sd))")
       5.   └─dplyr:::lazy_defunct("mutate")
       6.     └─lifecycle::deprecate_stop(...)
       7.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 17 ]
      Error: Test failures
      Execution halted
    ```

# psychonetrics

<details>

* Version: 0.9
* GitHub: https://github.com/SachaEpskamp/psychonetrics
* Source code: https://github.com/cran/psychonetrics
* Date/Publication: 2021-02-23 14:30:02 UTC
* Number of recursive dependencies: 131

Run `cloud_details(, "psychonetrics")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    
        filter, lag
    
    The following objects are masked from ‘package:base’:
    
        intersect, setdiff, setequal, union
    
    > 
    > # Let's take the agreeableness items, and gender:
    > ConsData <- bfi %>% 
    +   select(A1:A5, gender) %>% 
    +   na.omit # Let's remove missingness (otherwise use Estimator = "FIML)
    > 
    > # Define variables:
    > vars <- names(ConsData)[1:5]
    > 
    > # Let's fit a full GGM:
    > mod <- ggm(ConsData, vars = vars, omega = "empty")
    Error: `arrange_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `arrange()` instead.
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Missing or unexported object: ‘dplyr::summarize_’
    ```

*   checking R code for possible problems ... NOTE
    ```
    addMIs_inner_full: no visible global function definition for
      ‘summarize_’
    identify_Ising: no visible global function definition for ‘summarize_’
    identify_dlvm1: no visible global function definition for ‘summarize_’
    identify_lvm: no visible global function definition for ‘summarize_’
    identify_ml_lvm: no visible global function definition for ‘summarize_’
    identify_tsdlvm1: no visible global function definition for
      ‘summarize_’
    labtoind: no visible global function definition for ‘summarize_’
    Undefined global functions or variables:
      summarize_
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 101.1Mb
      sub-directories of 1Mb or more:
        libs  100.0Mb
    ```

# PupillometryR

<details>

* Version: 0.0.3
* GitHub: https://github.com/samhforbes/PupillometryR
* Source code: https://github.com/cran/PupillometryR
* Date/Publication: 2020-06-13 00:10:03 UTC
* Number of recursive dependencies: 77

Run `cloud_details(, "PupillometryR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Running examples in ‘PupillometryR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: baseline_data
    > ### Title: Baseline pupil data to the average pupil size within a window
    > ### Aliases: baseline_data
    > 
    > ### ** Examples
    > 
    > Sdata <- make_pupillometryr_data(data = pupil_data,
    +                                subject = ID,
    +                                trial = Trial,
    +                                time = Time,
    +                                condition = Type)
    > regressed_data <- regress_data(data = Sdata, pupil1 = RPupil, pupil2 = LPupil)
    > mean_data <- calculate_mean_pupil_size(data = regressed_data,
    + pupil1 = RPupil, pupil2 = LPupil)
    > base_data <- baseline_data(data = mean_data, pupil = mean_pupil, start = 0, stop = 100)
    Error: `summarise_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `summarise()` instead.
    Execution halted
    ```

*   checking R code for possible problems ... NOTE
    ```
    create_difference_data: no visible global function definition for
      ‘summarize_’
    create_time_windows: no visible global function definition for
      ‘summarize_’
    create_window_data: no visible global function definition for
      ‘summarize_’
    Undefined global functions or variables:
      summarize_
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘data.table’ ‘itsadug’ ‘mgcv’
      All declared Imports should be used.
    ```

# quickpsy

<details>

* Version: 0.1.5.1
* GitHub: NA
* Source code: https://github.com/cran/quickpsy
* Date/Publication: 2019-10-02 15:54:02 UTC
* Number of recursive dependencies: 37

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

# reclin

<details>

* Version: 0.1.1
* GitHub: https://github.com/djvanderlaan/reclin
* Source code: https://github.com/cran/reclin
* Date/Publication: 2018-08-09 14:30:03 UTC
* Number of recursive dependencies: 53

Run `cloud_details(, "reclin")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘reclin-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: problink_em
    > ### Title: Calculate EM-estimates of m- and u-probabilities
    > ### Aliases: problink_em
    > 
    > ### ** Examples
    > 
    > data("linkexample1", "linkexample2")
    > pairs <- pair_blocking(linkexample1, linkexample2, "postcode")
    > pairs <- compare_pairs(pairs, c("lastname", "firstname", "address", "sex"))
    > model <- problink_em(pairs)
    Error: `group_by_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `group_by()` instead.
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘tests.R’
    Running the tests in ‘tests/tests.R’ failed.
    Last 13 lines of output:
        4. │ └─reclin:::score_problink_impl(pairs, model, NULL, add, ...)
        5. │   └─reclin::problink_em(pairs)
        6. │     ├─reclin::tabulate_patterns(patterns)
        7. │     └─reclin:::tabulate_patterns.data.frame(patterns)
        8. │       └─reclin:::tabulate_patterns_impl(pairs, comparators, by)
        9. │         └─d %>% group_by_(.dots = by) %>% summarise(n = n())
       10. ├─dplyr::summarise(., n = n())
       11. └─dplyr::group_by_(., .dots = by)
       12.   └─dplyr:::lazy_defunct("group_by")
       13.     └─lifecycle::deprecate_stop(...)
       14.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 150 ]
      Error: Test failures
      Execution halted
    ```

# REddyProc

<details>

* Version: 1.2.2
* GitHub: https://github.com/bgctw/REddyProc
* Source code: https://github.com/cran/REddyProc
* Date/Publication: 2020-03-18 07:00:18 UTC
* Number of recursive dependencies: 74

Run `cloud_details(, "REddyProc")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘doRUnit.R’
    Running the tests in ‘tests/doRUnit.R’ failed.
    Last 13 lines of output:
      Please use `select()` instead.
      Backtrace:
          █
       1. ├─REddyProc::partitionNEEGL(dsNEE1, RadVar = "Rg_f", controlGLPart = partGLControl(useNightimeBasalRespiration = TRUE)) test_partGL.R:974:2
       2. │ └─REddyProc:::partGLInterpolateFluxes(...)
       3. │   └─resParms %>% select_(~-resOpt) %>% slice(iValidWin)
       4. ├─dplyr::slice(., iValidWin)
       5. └─dplyr::select_(., ~-resOpt)
       6.   └─dplyr:::lazy_defunct("select")
       7.     └─lifecycle::deprecate_stop(...)
       8.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 11 | WARN 2 | SKIP 19 | PASS 272 ]
      Error: Test failures
      Execution halted
    ```

# representr

<details>

* Version: 0.1.1
* GitHub: NA
* Source code: https://github.com/cran/representr
* Date/Publication: 2020-10-20 20:30:03 UTC
* Number of recursive dependencies: 57

Run `cloud_details(, "representr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### Title: Calculate the empirical KL divergence for a representative
    > ###   dataset as compared to the true dataset
    > ### Aliases: emp_kl_div
    > 
    > ### ** Examples
    > 
    > 
    > data("rl_reg1")
    > 
    > ## random prototyping
    > rep_dat_random <- represent(rl_reg1, identity.rl_reg1, "proto_random", id = FALSE, parallel = FALSE)
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

# rFIA

<details>

* Version: 0.3.1
* GitHub: NA
* Source code: https://github.com/cran/rFIA
* Date/Publication: 2021-01-15 05:30:03 UTC
* Number of recursive dependencies: 76

Run `cloud_details(, "rFIA")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘rFIA-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: area
    > ### Title: Estimate land area from FIADB
    > ### Aliases: area
    > 
    > ### ** Examples
    > 
    > ## Load data from the rFIA package
    > data(fiaRI)
    > data(countiesRI)
    > 
    > ## Most recents subset
    > fiaRI_mr <- clipFIA(fiaRI)
    > 
    > ## Most recent estimates of forested area in RI
    > area(db = fiaRI_mr)
    Error: The `.dots` argument of `group_by()` was deprecated in dplyr 1.0.0 and is now defunct.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘lifecycle’
      All declared Imports should be used.
    ```

# RNeXML

<details>

* Version: 2.4.5
* GitHub: https://github.com/ropensci/RNeXML
* Source code: https://github.com/cran/RNeXML
* Date/Publication: 2020-06-18 18:40:02 UTC
* Number of recursive dependencies: 141

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
          █
       1. ├─RNeXML::nexml_to_simmap(nex) test_simmap.R:24:2
       2. │ └─RNeXML::get_characters(nexml)
       3. │   └─get_level(nex, "otus/otu") %>% dplyr::select_(drop) %>% optional_labels(id_col = "otu")
       4. ├─RNeXML:::optional_labels(., id_col = "otu")
       5. └─dplyr::select_(., drop)
       6.   └─dplyr:::lazy_defunct("select")
       7.     └─lifecycle::deprecate_stop(...)
       8.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 16 | WARN 1 | SKIP 42 | PASS 214 ]
      Error: Test failures
      In addition: Warning message:
      package 'XML' was built under R version 4.0.4 
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
    ...
    > 
    > ### ** Examples
    > 
    > data(package="rollmatch", "rem_synthdata_small")
    > reduced_data <- reduce_data(data = rem_synthdata_small, treat = "treat",
    +                             tm = "quarter", entry = "entry_q",
    +                             id = "indiv_id", lookback = 1)
    > fm <- as.formula(treat ~ qtr_pmt + yr_pmt + age)
    > vars <- all.vars(fm)
    > scored_data <- score_data(reduced_data = reduced_data,
    +                           model_type = "logistic", match_on = "logit",
    +                           fm = fm, treat = "treat",
    +                           tm = "quarter", entry = "entry_q", id = "indiv_id")
    > output <- rollmatch(scored_data, data=rem_synthdata_small, treat = "treat",
    +                     tm = "quarter", entry = "entry_q", id = "indiv_id",
    +                     vars = vars, lookback = 1, alpha = .2,
    +                     standard_deviation = "average", num_matches = 3,
    +                     replacement = TRUE)
    Error: `group_by_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `group_by()` instead.
    Execution halted
    ```

# rties

<details>

* Version: 5.0.0
* GitHub: NA
* Source code: https://github.com/cran/rties
* Date/Publication: 2020-05-11 16:00:02 UTC
* Number of recursive dependencies: 141

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

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘DescTools’ ‘MASS’ ‘gridExtra’
      All declared Imports should be used.
    ```

# rtimicropem

<details>

* Version: 1.4.0
* GitHub: https://github.com/ropensci/rtimicropem
* Source code: https://github.com/cran/rtimicropem
* Date/Publication: 2019-05-15 09:40:03 UTC
* Number of recursive dependencies: 106

Run `cloud_details(, "rtimicropem")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘rtimicropem-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: clean_measures
    > ### Title: Outputs clean rh_corrected_nephelometer measures for analysis.
    > ### Aliases: clean_measures
    > 
    > ### ** Examples
    > 
    > data(micropemChai)
    > cleanMP <- clean_measures(micropemChai)
    Warning: `tbl_df()` was deprecated in dplyr 1.0.0.
    Please use `tibble::as_tibble()` instead.
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_warnings()` to see where this warning was generated.
    Error: `mutate_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `mutate()` instead.
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── Error (test-lags.R:5:3): identify_lags outputs a data table ─────────────────
      Error: `select_()` was deprecated in dplyr 0.7.0 and is now defunct.
      Please use `select()` instead.
      Backtrace:
          █
       1. ├─rtimicropem::convert_output(...) test-lags.R:5:2
       2. │ └─`%>%`(...)
       3. └─dplyr::select_(., .dots = list(quote(datetime), quote(dplyr::everything())))
       4.   └─dplyr:::lazy_defunct("select")
       5.     └─lifecycle::deprecate_stop(...)
       6.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 3 | WARN 1 | SKIP 5 | PASS 5 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘R6’
      All declared Imports should be used.
    ```

# RTL

<details>

* Version: 0.1.6
* GitHub: https://github.com/risktoollib/RTL
* Source code: https://github.com/cran/RTL
* Date/Publication: 2021-04-07 13:10:10 UTC
* Number of recursive dependencies: 136

Run `cloud_details(, "RTL")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘RTL-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: stl_decomp
    > ### Title: 'stl_decomp'
    > ### Aliases: stl_decomp
    > 
    > ### ** Examples
    > 
    > x <- dflong %>% dplyr::filter(series=="CL01")
    > stl_decomp(x,output="chart",s.window=13,s.degree=1)
    Error: object ‘summarize_’ is not exported by 'namespace:dplyr'
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘quantmod’ ‘sp’ ‘timetk’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 633 marked UTF-8 strings
    ```

# saeSim

<details>

* Version: 0.10.0
* GitHub: https://github.com/wahani/saeSim
* Source code: https://github.com/cran/saeSim
* Date/Publication: 2019-03-28 12:50:03 UTC
* Number of recursive dependencies: 96

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
       33.   └─dplyr:::lazy_defunct("arrange")
       34.     └─lifecycle::deprecate_stop(...)
       35.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 32 | WARN 0 | SKIP 0 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

# sf

<details>

* Version: 0.9-8
* GitHub: https://github.com/r-spatial/sf
* Source code: https://github.com/cran/sf
* Date/Publication: 2021-03-17 10:50:03 UTC
* Number of recursive dependencies: 143

Run `cloud_details(, "sf")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
    ...
    201,202d200
    < Warning message:
    < package 'stars' was built under R version 4.0.4 
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Backtrace:
          █
       1. ├─testthat::expect_true(...) test_tidy.R:7:2
       2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
       3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
       4. ├─nc %>% select_("AREA", attr(., "sf_column")) %>% inherits("sf")
       5. ├─base::inherits(., "sf")
       6. └─dplyr::select_(., "AREA", attr(., "sf_column"))
       7.   └─dplyr:::lazy_defunct("select")
       8.     └─lifecycle::deprecate_stop(...)
       9.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 1 | WARN 9 | SKIP 60 | PASS 732 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 25.3Mb
      sub-directories of 1Mb or more:
        doc      3.0Mb
        libs    17.7Mb
        sqlite   1.5Mb
    ```

# sfc

<details>

* Version: 0.1.0
* GitHub: https://github.com/ctfysh/sfc
* Source code: https://github.com/cran/sfc
* Date/Publication: 2016-08-25 10:01:01
* Number of recursive dependencies: 28

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
    > 
    > ## model as txt
    > data <- system.file("extdata", "data_utf8.csv", package = "sfc")
    > model <- system.file("extdata", "model_utf8.txt", package = "sfc")
    > sfc(data, model, sample.size = 100, fileEncoding = "UTF-8")
    Read 12 items
    Error: `mutate_each()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `across()` instead.
    Execution halted
    ```

## In both

*   checking whether package ‘sfc’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘tidyr’ was built under R version 4.0.4
    See ‘/tmp/workdir/sfc/new/sfc.Rcheck/00install.out’ for details.
    ```

# simglm

<details>

* Version: 0.8.0
* GitHub: https://github.com/lebebr01/simglm
* Source code: https://github.com/cran/simglm
* Date/Publication: 2020-06-12 20:10:02 UTC
* Number of recursive dependencies: 106

Run `cloud_details(, "simglm")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    +                               list(mean = 0, sd = 4)))
    > n <- 50
    > pow_param <- c('(Intercept)', 'act', 'diff')
    > alpha <- .01
    > pow_dist <- "z"
    > pow_tail <- 2
    > replicates <- 2
    > 
    > power_out <- sim_pow_glm(fixed = fixed, fixed_param = fixed_param, 
    +                          cov_param = cov_param, 
    +                          n = n, data_str = "single", 
    +                          outcome_type = 'logistic', 
    +                          pow_param = pow_param, alpha = alpha,
    +                          pow_dist = pow_dist, pow_tail = pow_tail, 
    +                          replicates = replicates, raw_power = FALSE)
    Warning: 'sim_pow_glm' is no longer supported and will be removed in v 1.0
    
                  See tidy_simulation vignette
    Error: `group_by_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `group_by()` instead.
    Execution halted
    ```

# simPH

<details>

* Version: 1.3.13
* GitHub: https://github.com/christophergandrud/simPH
* Source code: https://github.com/cran/simPH
* Date/Publication: 2021-01-10 14:50:05 UTC
* Number of recursive dependencies: 70

Run `cloud_details(, "simPH")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > library(survival)
    > 
    > # Run basic model
    > M1 <- coxph(Surv(acttime, censor) ~ prevgenx + lethal +
    +            deathrt1 + acutediz + hosp01  + hhosleng +
    +            mandiz01 + femdiz01 + peddiz01 + orphdum +
    +            vandavg3 + wpnoavg3 + condavg3 + orderent +
    +            stafcder, data = CarpenterFdaData)
    > 
    >  # Simulate Hazard Ratios
    >  Sim1 <- coxsimLinear(M1, b = "stafcder",
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

# simTool

<details>

* Version: 1.1.7
* GitHub: https://github.com/MarselScheer/simTool
* Source code: https://github.com/cran/simTool
* Date/Publication: 2020-09-22 16:00:03 UTC
* Number of recursive dependencies: 55

Run `cloud_details(, "simTool")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Start of the simulation: 2021-04-20 10:48:02
    End of the simulation: 2021-04-20 10:48:02
    > 
    > presever_rownames <- function(mat) {
    +   rn <- rownames(mat)
    +   ret <- tibble::as_tibble(mat)
    +   ret$term <- rn
    +   ret
    + }
    > 
    > eg <- eval_tibbles(
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
      Running test_eval_tibbles.R...........   18 tests [0;32mOK[0m 
      Running test_eval_tibbles.R...........   18 tests [0;32mOK[0m 
      Running test_eval_tibbles.R...........   19 tests [0;32mOK[0m 
      Running test_eval_tibbles.R...........   19 tests [0;32mOK[0m 
      Running test_eval_tibbles.R...........   20 tests [0;32mOK[0m 
      Running test_eval_tibbles.R...........   20 tests [0;32mOK[0m 
      Running test_eval_tibbles.R...........   21 tests [0;32mOK[0m 
      Running test_eval_tibbles.R...........   21 tests [0;32mOK[0m 
      Running test_eval_tibbles.R...........   22 tests [0;32mOK[0m 
      Running test_eval_tibbles.R...........   22 tests [0;32mOK[0m 
      Running test_eval_tibbles.R...........   23 tests [0;32mOK[0m 
      Running test_eval_tibbles.R...........   23 tests [0;32mOK[0m 
      Running test_eval_tibbles.R...........   24 tests [0;32mOK[0m 
      Running test_eval_tibbles.R...........   24 tests [0;32mOK[0m Error: The `.dots` argument of `group_by()` was deprecated in dplyr 1.0.0 and is now defunct.
      Execution halted
    ```

# simts

<details>

* Version: 0.1.1
* GitHub: https://github.com/SMAC-Group/simts
* Source code: https://github.com/cran/simts
* Date/Publication: 2019-07-21 22:20:02 UTC
* Number of recursive dependencies: 47

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
    Warning: attributes are not identical across measure variables;
    they will be dropped
    > best_model(x, ic = "aic")
    Error: `filter_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `filter()` instead.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 43.3Mb
      sub-directories of 1Mb or more:
        doc    2.1Mb
        libs  40.2Mb
    ```

# spatPomp

<details>

* Version: 0.21.0.0
* GitHub: https://github.com/kidusasfaw/spatPomp
* Source code: https://github.com/cran/spatPomp
* Date/Publication: 2021-04-12 08:40:02 UTC
* Number of recursive dependencies: 66

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

## In both

*   checking whether package ‘spatPomp’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘pomp’ was built under R version 4.0.4
    See ‘/tmp/workdir/spatPomp/new/spatPomp.Rcheck/00install.out’ for details.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘doParallel’ ‘ggplot2’ ‘parallel’
      All declared Imports should be used.
    ```

# srvyr

<details>

* Version: 1.0.1
* GitHub: https://github.com/gergness/srvyr
* Source code: https://github.com/cran/srvyr
* Date/Publication: 2021-03-28 21:40:02 UTC
* Number of recursive dependencies: 85

Run `cloud_details(, "srvyr")` for more info

</details>

## Newly broken

*   checking whether package ‘srvyr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/srvyr/new/srvyr.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘srvyr’ ...
** package ‘srvyr’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘summarize_’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘srvyr’
* removing ‘/tmp/workdir/srvyr/new/srvyr.Rcheck/srvyr’

```
### CRAN

```
* installing *source* package ‘srvyr’ ...
** package ‘srvyr’ successfully unpacked and MD5 sums checked
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
* DONE (srvyr)

```
# starschemar

<details>

* Version: 1.2.0
* GitHub: NA
* Source code: https://github.com/cran/starschemar
* Date/Publication: 2020-09-25 21:30:02 UTC
* Number of recursive dependencies: 56

Run `cloud_details(, "starschemar")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    The error most likely occurred in:
    
    > ### Name: character_dimensions
    > ### Title: Transform dimension numeric attributes to character
    > ### Aliases: character_dimensions character_dimensions.star_schema
    > 
    > ### ** Examples
    > 
    > library(tidyr)
    Warning: package ‘tidyr’ was built under R version 4.0.4
    > 
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
          █
       1. ├─starschemar:::update_facts_with_dimensions(st_mrs_age_test, mod_dim) test-update_facts_with_dimensions.R:6:2
       2. └─starschemar:::update_facts_with_dimensions.star_schema(...)
       3.   ├─starschemar:::group_table(st$fact[[1]])
       4.   └─starschemar:::group_table.fact_table(st$fact[[1]])
       5.     ├─dplyr::group_by(as.data.frame(ft), .dots = dim_keys)
       6.     └─dplyr:::group_by.data.frame(as.data.frame(ft), .dots = dim_keys)
       7.       └─dplyr::group_by_prepare(.data, ..., .add = .add)
       8.         └─lifecycle::deprecate_stop("1.0.0", "group_by(.dots = )")
       9.           └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 13 | WARN 1 | SKIP 0 | PASS 138 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘pander’ ‘readr’ ‘tidyselect’
      All declared Imports should be used.
    ```

# strand

<details>

* Version: 0.2.0
* GitHub: https://github.com/strand-tech/strand
* Source code: https://github.com/cran/strand
* Date/Publication: 2020-11-19 21:40:06 UTC
* Number of recursive dependencies: 108

Run `cloud_details(, "strand")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### ** Examples
    > 
    > # Load up sample data
    > data(sample_secref)
    > data(sample_pricing)
    > data(sample_inputs)
    > 
    > # Load sample configuration
    > config <- example_strategy_config()
    > 
    > # Override config file end date to run a one-week sim
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
        3. │   └─`%>%`(...)
        4. ├─dplyr::select(., -"weight_divisor")
        5. ├─tidyr::pivot_wider(...)
        6. ├─dplyr::mutate(., exposure = .data$exposure/.data$weight_divisor)
        7. ├─dplyr::left_join(., weight_divisor_df, by = "strategy")
        8. ├─dplyr::summarise(., exposure = sum(.data[[in_var]]))
        9. ├─dplyr::group_by(., .dots = c("strategy", cat_var))
       10. └─dplyr:::group_by.data.frame(., .dots = c("strategy", cat_var))
       11.   └─dplyr::group_by_prepare(.data, ..., .add = .add)
       12.     └─lifecycle::deprecate_stop("1.0.0", "group_by(.dots = )")
       13.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 61 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked UTF-8 string
    ```

# surveybootstrap

<details>

* Version: 0.0.1
* GitHub: NA
* Source code: https://github.com/cran/surveybootstrap
* Date/Publication: 2016-05-04 12:14:27
* Number of recursive dependencies: 47

Run `cloud_details(, "surveybootstrap")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        5.       ├─base::do.call("rbind", rbsfn(survey.data = svy)) test_variance.r:47:35
        6.       └─functional:::rbsfn(survey.data = svy)
        7.         ├─base::do.call(FUN, c(.orig, list(...)))
        8.         ├─(function (survey.data, survey.design, bootstrap.fn, estimator.fn, ...
        9.         │ └─base::eval(boot.call, parent.frame())
       10.         │   └─base::eval(boot.call, parent.frame())
       11.         └─(function (survey.data, survey.design, parallel = FALSE, paropts = NULL, ...
       12.           └─dplyr::group_indices_(survey.data, .dots = all.vars(psu.vars))
       13.             └─dplyr:::lazy_defunct("group_indices")
       14.               └─lifecycle::deprecate_stop(...)
       15.                 └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

# survminer

<details>

* Version: 0.4.9
* GitHub: https://github.com/kassambara/survminer
* Source code: https://github.com/cran/survminer
* Date/Publication: 2021-03-09 09:50:03 UTC
* Number of recursive dependencies: 141

Run `cloud_details(, "survminer")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Please use `select()` instead.
    ℹ Input `survtable` is `purrr::map2(...)`.
    Backtrace:
         █
      1. ├─survminer::ggsurvplot_combine(fit, demo.data)
      2. │ └─`%>%`(...)
      3. ├─dplyr::mutate(...)
      4. ├─dplyr:::mutate.data.frame(...)
      5. │ └─dplyr:::mutate_cols(.data, ...)
      6. │   ├─base::withCallingHandlers(...)
      7. │   └─mask$eval_all_mutate(quo)
      8. ├─purrr::map2(...)
      9. │ └─survminer:::.f(.x[[i]], .y[[i]], ...)
     10. │   └─`%>%`(...)
     11. ├─dplyr::select_(...)
     12. │ └─dplyr:::lazy_defunct("select")
     13. │   └─lifecycle::deprecate_stop(...)
     14. │     └─lifecycle:::deprecate_stop0(msg)
     15. │       └─base::stop(...)
     16. └─(function (e) ...
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.0Mb
      sub-directories of 1Mb or more:
        doc   5.5Mb
    ```

# sweep

<details>

* Version: 0.2.3
* GitHub: https://github.com/business-science/sweep
* Source code: https://github.com/cran/sweep
* Date/Publication: 2020-07-10 12:10:03 UTC
* Number of recursive dependencies: 153

Run `cloud_details(, "sweep")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > 
    > fit_StructTS <- WWWusage %>%
    +     StructTS()
    > 
    > sw_tidy(fit_StructTS)
    # A tibble: 3 x 2
      term    estimate
      <chr>      <dbl>
    1 level        0  
    2 slope       13.0
    3 epsilon      0  
    > sw_glance(fit_StructTS)
    # A tibble: 1 x 12
      model.desc      sigma logLik   AIC   BIC      ME  RMSE   MAE   MPE  MAPE  MASE
      <chr>           <dbl>  <dbl> <dbl> <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    1 Local linear s… 0.995  -277.  559.  564. -0.0200  3.59  2.96 0.140  2.32 0.654
    # … with 1 more variable: ACF1 <dbl>
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
          █
       1. ├─sweep::sw_augment(fit_robets, rename_index = "date") test_tidiers_robets.R:26:4
       2. ├─sweep:::sw_augment.robets(fit_robets, rename_index = "date")
       3. │ └─sweep:::sw_augment_columns(ret, data, rename_index, timetk_idx)
       4. │   └─sweep:::add_index(ret_2, rename_index)
       5. │     └─ret %>% dplyr::select_(rename_index, "dplyr::everything()")
       6. └─dplyr::select_(., rename_index, "dplyr::everything()")
       7.   └─dplyr:::lazy_defunct("select")
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

# SWMPr

<details>

* Version: 2.4.0
* GitHub: https://github.com/fawda123/SWMPr
* Source code: https://github.com/cran/SWMPr
* Date/Publication: 2021-01-05 20:00:32 UTC
* Number of recursive dependencies: 107

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
* Number of recursive dependencies: 106

Run `cloud_details(, "Tendril")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### Title: Tendril
    > ### Aliases: Tendril
    > 
    > ### ** Examples
    > 
    > data <- Tendril(mydata = TendrilData,
    + rotations = Rotations,
    + AEfreqThreshold=9,
    + Tag = "Comment",
    + Treatments = c("placebo", "active"),
    + Unique.Subject.Identifier = "subjid",
    + Terms = "ae",
    + Treat = "treatment",
    + StartDay = "day",
    + SubjList = SubjList,
    + SubjList.subject = "subjid",
    + SubjList.treatment = "treatment"
    + )
    Error: `rename_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `rename()` instead.
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        1. ├─testthat::expect_warning(...) test_tendrilPerm.R:125:2
        2. │ └─testthat:::quasi_capture(enquo(object), label, capture_warnings)
        3. │   ├─testthat:::.capture(...)
        4. │   │ └─base::withCallingHandlers(...)
        5. │   └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
        6. └─Tendril::Tendril(...) test_tendrilPerm.R:126:4
        7.   └─Tendril:::dataSetup(...)
        8.     └─dplyr::rename_(data, Unique.Subject.Identifier = Unique.Subject.Identifier)
        9.       └─dplyr:::lazy_defunct("rename")
       10.         └─lifecycle::deprecate_stop(...)
       11.           └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 11 | WARN 0 | SKIP 0 | PASS 12 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘graphics’
      All declared Imports should be used.
    ```

# tibbletime

<details>

* Version: 0.1.6
* GitHub: https://github.com/business-science/tibbletime
* Source code: https://github.com/cran/tibbletime
* Date/Publication: 2020-07-21 13:50:02 UTC
* Number of recursive dependencies: 66

Run `cloud_details(, "tibbletime")` for more info

</details>

## Newly broken

*   checking whether package ‘tibbletime’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/tibbletime/new/tibbletime.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘tibbletime’ ...
** package ‘tibbletime’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
g++ -std=gnu++11 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG  -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I/usr/local/include  -I../inst/include -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++11 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG  -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I/usr/local/include  -I../inst/include -fpic  -g -O2  -c is_ordered.cpp -o is_ordered.o
g++ -std=gnu++11 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG  -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I/usr/local/include  -I../inst/include -fpic  -g -O2  -c sorted_range_search.cpp -o sorted_range_search.o
g++ -std=gnu++11 -shared -L/opt/R/4.0.3/lib/R/lib -L/usr/local/lib -o tibbletime.so RcppExports.o is_ordered.o sorted_range_search.o -L/opt/R/4.0.3/lib/R/lib -lR
installing to /tmp/workdir/tibbletime/new/tibbletime.Rcheck/00LOCK-tibbletime/00new/tibbletime/libs
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘summarize_’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘tibbletime’
* removing ‘/tmp/workdir/tibbletime/new/tibbletime.Rcheck/tibbletime’

```
### CRAN

```
* installing *source* package ‘tibbletime’ ...
** package ‘tibbletime’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
g++ -std=gnu++11 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG  -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I/usr/local/include  -I../inst/include -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++11 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG  -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I/usr/local/include  -I../inst/include -fpic  -g -O2  -c is_ordered.cpp -o is_ordered.o
g++ -std=gnu++11 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG  -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I/usr/local/include  -I../inst/include -fpic  -g -O2  -c sorted_range_search.cpp -o sorted_range_search.o
g++ -std=gnu++11 -shared -L/opt/R/4.0.3/lib/R/lib -L/usr/local/lib -o tibbletime.so RcppExports.o is_ordered.o sorted_range_search.o -L/opt/R/4.0.3/lib/R/lib -lR
installing to /tmp/workdir/tibbletime/old/tibbletime.Rcheck/00LOCK-tibbletime/00new/tibbletime/libs
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** checking absolute paths in shared objects and dynamic libraries
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (tibbletime)

```
# tidypredict

<details>

* Version: 0.4.8
* GitHub: https://github.com/tidymodels/tidypredict
* Source code: https://github.com/cran/tidypredict
* Date/Publication: 2020-10-28 06:50:02 UTC
* Number of recursive dependencies: 97

Run `cloud_details(, "tidypredict")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── Error (test_xgboost.R:159:1): (code run outside of `test_that()`) ───────────
      Error: `mutate_()` was deprecated in dplyr 0.7.0 and is now defunct.
      Please use `mutate()` instead.
      Backtrace:
          █
       1. ├─`%>%`(...) test_xgboost.R:159:0
       2. ├─dplyr::mutate(., yhat_sql = 1 - yhat_sql)
       3. └─dplyr::mutate_(., yhat_sql = as.character(xgb_reglogistic_basescore_sql)[[3]])
       4.   └─dplyr:::lazy_defunct("mutate")
       5.     └─lifecycle::deprecate_stop(...)
       6.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 116 ]
      Error: Test failures
      Execution halted
    ```

# tidytree

<details>

* Version: 0.3.3
* GitHub: https://github.com/YuLab-SMU/tidytree
* Source code: https://github.com/cran/tidytree
* Date/Publication: 2020-04-02 09:10:03 UTC
* Number of recursive dependencies: 74

Run `cloud_details(, "tidytree")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘tidytree-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: as.treedata
    > ### Title: as.treedata
    > ### Aliases: as.treedata as.treedata.tbl_tree
    > 
    > ### ** Examples
    > 
    > library(ape)
    > set.seed(2017)
    > tree <- rtree(4)
    > d <- tibble(label = paste0('t', 1:4),
    +            trait = rnorm(4))
    > x <- as_tibble(tree)
    > full_join(x, d, by = 'label') %>% as.treedata
    Error: `mutate_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `mutate()` instead.
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Error: `filter_()` was deprecated in dplyr 0.7.0 and is now defunct.
      Please use `filter()` instead.
      Backtrace:
          █
       1. ├─tidytree::groupOTU(d, cls) test-grouping.R:21:0
       2. └─tidytree:::groupOTU.tbl_tree(d, cls)
       3.   └─tidytree:::groupOTU.tbl_tree_item(...)
       4.     └─dplyr::filter_(.data, ~(label %in% .node))
       5.       └─dplyr:::lazy_defunct("filter")
       6.         └─lifecycle::deprecate_stop(...)
       7.           └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 4 | WARN 0 | SKIP 0 | PASS 34 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking: 'ggtree', 'treeio'
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘utils’
      All declared Imports should be used.
    ```

# timelineS

<details>

* Version: 0.1.1
* GitHub: https://github.com/daheelee/timelineS
* Source code: https://github.com/cran/timelineS
* Date/Publication: 2016-08-22 14:13:31
* Number of recursive dependencies: 37

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

*   checking R code for possible problems ... NOTE
    ```
    durPlot: no visible global function definition for ‘summarize_’
    durSummary: no visible global function definition for ‘summarize_’
    Undefined global functions or variables:
      summarize_
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘base’
      All declared Imports should be used.
    ```

# timetk

<details>

* Version: 2.6.1
* GitHub: https://github.com/business-science/timetk
* Source code: https://github.com/cran/timetk
* Date/Publication: 2021-01-18 17:40:02 UTC
* Number of recursive dependencies: 199

Run `cloud_details(, "timetk")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > 
    > # Create a quarterly series with 1 missing value
    > missing_data_tbl <- tibble(
    +     date = tk_make_timeseries("2014-01-01", "2015-01-01", by = "quarter"),
    +     value = 1:5
    + ) %>%
    +     slice(-4) # Lose the 4th quarter on purpose
    > missing_data_tbl
    # A tibble: 4 x 2
      date       value
      <date>     <int>
    1 2014-01-01     1
    2 2014-04-01     2
    3 2014-07-01     3
    4 2015-01-01     5
    > 
    > 
    > # Detects missing quarter, and pads the missing regularly spaced quarter with NA
    > missing_data_tbl %>% pad_by_time(date, .by = "quarter")
    Error: The `.dots` argument of `group_by()` was deprecated in dplyr 1.0.0 and is now defunct.
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── Error (test-tk_make_timeseries_future.R:394:5): tk_make_future_timeseries(predict_every_three) test returns correct format. ──
      Error: object 'date_seq' not found
      Backtrace:
          █
       1. ├─testthat::expect_warning(...) test-tk_make_timeseries_future.R:394:4
       2. │ └─testthat:::quasi_capture(enquo(object), label, capture_warnings)
       3. │   ├─testthat:::.capture(...)
       4. │   │ └─base::withCallingHandlers(...)
       5. │   └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
       6. ├─timetk::tk_make_future_timeseries(...)
       7. └─timetk:::tk_make_future_timeseries.Date(...)
      
      [ FAIL 8 | WARN 1 | SKIP 0 | PASS 315 ]
      Error: Test failures
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

# trelliscopejs

<details>

* Version: 0.2.6
* GitHub: https://github.com/hafen/trelliscopejs
* Source code: https://github.com/cran/trelliscopejs
* Date/Publication: 2021-02-01 08:00:02 UTC
* Number of recursive dependencies: 107

Run `cloud_details(, "trelliscopejs")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
           █
        1. ├─`%>%`(...) test-trelliscope.R:41:2
        2. ├─trelliscopejs::trelliscope(., name = "city_vs_highway_mpg", thumb = FALSE)
        3. ├─trelliscopejs:::trelliscope.data.frame(...)
        4. │ └─trelliscopejs:::cog_df_info(...)
        5. │   └─trelliscopejs:::find_sort_cols(x[setdiff(atomic_cols, cond_cols)])
        6. │     └─res %>% filter_(~!is.na(dir))
        7. └─dplyr::filter_(., ~!is.na(dir))
        8.   └─dplyr:::lazy_defunct("filter")
        9.     └─lifecycle::deprecate_stop(...)
       10.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

# TwoRegression

<details>

* Version: 0.1.2
* GitHub: https://github.com/paulhibbing/TwoRegression
* Source code: https://github.com/cran/TwoRegression
* Date/Publication: 2018-03-19 11:16:07 UTC
* Number of recursive dependencies: 52

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
          █
       1. ├─TwoRegression::hibbing18_twoReg_process(...) test-IMU_master_function.R:55:2
       2. │ └─TwoRegression::read_IMU(IMU, verbose = verbose)
       3. │   └─TwoRegression:::imu_collapse(AG, meta$block_size, verbose = verbose)
       4. │     └─`%>%`(...)
       5. └─dplyr::summarise_(...)
       6.   └─dplyr:::lazy_defunct("summarise")
       7.     └─lifecycle::deprecate_stop(...)
       8.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 15 ]
      Error: Test failures
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
          █
       1. ├─testthat::expect_equal(...) test-move-cols.r:55:4
       2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
       3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
       4. ├─useful::moveToFront(theDF, c("B", "C"))
       5. │ └─data %>% select_(.dots = colOrder)
       6. └─dplyr::select_(., .dots = colOrder)
       7.   └─dplyr:::lazy_defunct("select")
       8.     └─lifecycle::deprecate_stop(...)
       9.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 3 | WARN 3 | SKIP 2 | PASS 728 ]
      Error: Test failures
      Execution halted
    ```

# vpc

<details>

* Version: 1.2.2
* GitHub: https://github.com/ronkeizer/vpc
* Source code: https://github.com/cran/vpc
* Date/Publication: 2021-01-11 20:20:02 UTC
* Number of recursive dependencies: 57

Run `cloud_details(, "vpc")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### Aliases: vpc_tte
    > 
    > ### ** Examples
    > 
    > ## See vpc-docs.ronkeizer.com for more documentation and examples.
    > 
    > ## Example for repeated) time-to-event data
    > ## with NONMEM-like data (e.g. simulated using a dense grid)
    > 
    > data(rtte_obs_nm)
    > data(rtte_sim_nm)
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

* Version: 0.1.3
* GitHub: https://github.com/dgrtwo/widyr
* Source code: https://github.com/cran/widyr
* Date/Publication: 2020-04-12 06:00:02 UTC
* Number of recursive dependencies: 103

Run `cloud_details(, "widyr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    
    The following objects are masked from ‘package:stats’:
    
        filter, lag
    
    The following objects are masked from ‘package:base’:
    
        intersect, setdiff, setequal, union
    
    > dat <- tibble(group = rep(1:5, each = 2),
    +               letter = c("a", "b",
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
        1. ├─mtcars %>% group_by(cyl) %>% pairwise_count(vs, am) test-pairwise-count.R:104:2
        2. ├─widyr::pairwise_count(., vs, am)
        3. │ └─widyr::pairwise_count_(...)
        4. │   └─`%>%`(...)
        5. ├─dplyr::rename(., n = value)
        6. ├─widyr:::func(., item, feature, wt)
        7. ├─dplyr::mutate(., ..value = 1)
        8. └─dplyr::distinct_(., .dots = c(item, feature), .keep_all = TRUE)
        9.   └─dplyr:::lazy_defunct("distinct")
       10.     └─lifecycle::deprecate_stop(...)
       11.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 4 | WARN 2 | SKIP 0 | PASS 47 ]
      Error: Test failures
      Execution halted
    ```

# wrangle

<details>

* Version: 0.5.2
* GitHub: NA
* Source code: https://github.com/cran/wrangle
* Date/Publication: 2019-04-25 05:00:03 UTC
* Number of recursive dependencies: 21

Run `cloud_details(, "wrangle")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘wrangle-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: enumerate
    > ### Title: Count unique combinations of items in specified columns.
    > ### Aliases: enumerate
    > 
    > ### ** Examples
    > 
    > enumerate(mtcars, cyl, gear, carb)
    Error: `arrange_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `arrange()` instead.
    Execution halted
    ```

# xspliner

<details>

* Version: 0.0.4
* GitHub: https://github.com/ModelOriented/xspliner
* Source code: https://github.com/cran/xspliner
* Date/Publication: 2019-09-25 20:20:02 UTC
* Number of recursive dependencies: 181

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
    randomForest 4.6-14
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

# Zelig

<details>

* Version: 5.1.7
* GitHub: https://github.com/IQSS/Zelig
* Source code: https://github.com/cran/Zelig
* Date/Publication: 2020-12-12 15:20:06 UTC
* Number of recursive dependencies: 136

Run `cloud_details(, "Zelig")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    
    The following objects are masked from ‘package:stats’:
    
        filter, lag
    
    The following objects are masked from ‘package:base’:
    
        intersect, setdiff, setequal, union
    
    > data(sanction)
    > z.att <- zelig(num ~ target + coop + mil, model = "poisson",
    +                  data = sanction) %>%
    +              ATT(treatment = "mil") %>%
    +              get_qi(qi = "ATT", xvalue = "TE")
    Warning: `tbl_df()` was deprecated in dplyr 1.0.0.
    Please use `tibble::as_tibble()` instead.
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_warnings()` to see where this warning was generated.
    Error: `group_by_()` was deprecated in dplyr 0.7.0 and is now defunct.
    Please use `group_by()` instead.
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Please use `group_by()` instead.
      Backtrace:
          █
       1. ├─z5$zelig(Fertility ~ Education, data = swiss) test-zelig.R:319:4
       2. │ └─Zelig:::callSuper(...)
       3. │   └─`%>%`(...)
       4. ├─dplyr::do(., z.out = eval(fn2(.self$model.call, quote(as.data.frame(.)))))
       5. └─dplyr::group_by_(., .self$by)
       6.   └─dplyr:::lazy_defunct("group_by")
       7.     └─lifecycle::deprecate_stop(...)
       8.       └─lifecycle:::deprecate_stop0(msg)
      
      [ FAIL 91 | WARN 11 | SKIP 17 | PASS 28 ]
      Error: Test failures
      Execution halted
    ```

