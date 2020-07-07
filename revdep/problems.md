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
    Complete output:
      > library(testthat)
      > library(anglr)
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

## Newly fixed

*   checking whether package ‘anglr’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: 'rgl.init' failed, running with 'rgl.useNULL = TRUE'.
    See ‘/tmp/workdir/anglr/old/anglr.Rcheck/00install.out’ for details.
    ```

# crimedata

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/crimedata
* URL: https://github.com/mpjashby/crimedata
* BugReports: https://github.com/mpjashby/crimedata/issues
* Date/Publication: 2019-03-21 23:23:31 UTC
* Number of recursive dependencies: 68

Run `cloud_details(, "crimedata")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        |====================================================                  |  75%
        |                                                                            
        |=========================================================             |  81%
        |                                                                            
        |=============================================================         |  88%
        |                                                                            
        |==================================================================    |  94%
        |                                                                            
        |======================================================================| 100%
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 34 | SKIPPED: 0 | WARNINGS: 3 | FAILED: 1 ]
      1. Failure: return value of get_file_urls is a tibble (@test_file_urls.R#4) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# dbparser

<details>

* Version: 1.1.2
* Source code: https://github.com/cran/dbparser
* URL: https://docs.ropensci.org/dbparser, https://github.com/ropensci/dbparser
* BugReports: https://github.com/ropensci/dbparser/issues
* Date/Publication: 2020-06-08 17:00:02 UTC
* Number of recursive dependencies: 62

Run `cloud_details(, "dbparser")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > 
      > test_check("dbparser")
      ── 1. Failure: Read darug classification attributes (@test_drug_main_node_parser
      is_tibble(drug_classification()) isn't true.
      
      ── 2. Failure: Read drug classification attributes (@test_drug_main_node_parser_
      is_tibble(drug_classification()) isn't true.
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 471 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 2 ]
      1. Failure: Read darug classification attributes (@test_drug_main_node_parser_biotech.R#91) 
      2. Failure: Read drug classification attributes (@test_drug_main_node_parser_small_molecule.R#96) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# DiversityOccupancy

<details>

* Version: 1.0.6
* Source code: https://github.com/cran/DiversityOccupancy
* Date/Publication: 2017-03-02 18:32:36
* Number of recursive dependencies: 97

Run `cloud_details(, "DiversityOccupancy")` for more info

</details>

## Newly broken

*   checking whether package ‘DiversityOccupancy’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: 'rgl.init' failed, running with 'rgl.useNULL = TRUE'.
    See ‘/tmp/workdir/DiversityOccupancy/new/DiversityOccupancy.Rcheck/00install.out’ for details.
    ```

# evaluator

<details>

* Version: 0.4.2
* Source code: https://github.com/cran/evaluator
* URL: https://evaluator.tidyrisk.org
* BugReports: https://github.com/davidski/evaluator/issues
* Date/Publication: 2020-04-16 09:20:09 UTC
* Number of recursive dependencies: 135

Run `cloud_details(, "evaluator")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      +   if (!shinytest::dependenciesInstalled()) shinytest:::installDependencies()
      +   message("Using phantom.js from ", shinytest:::find_phantom(), "\n")
      + }
      > 
      > test_check("evaluator")
      # Scenario model: openfair_tef_tc_diff_lm
      ── 1. Failure: Scenario object can be created and coerced to tibble (@test-tidyr
      as_tibble(scenario) inherits from `data.frame` not `tbl`.
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 137 | SKIPPED: 5 | WARNINGS: 0 | FAILED: 1 ]
      1. Failure: Scenario object can be created and coerced to tibble (@test-tidyrisk-scenario.R#19) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# ftExtra

<details>

* Version: 0.0.1
* Source code: https://github.com/cran/ftExtra
* URL: https://github.com/atusy/ftExtra
* BugReports: https://github.com/atusy/ftExtra/issues
* Date/Publication: 2020-03-20 09:50:02 UTC
* Number of recursive dependencies: 57

Run `cloud_details(, "ftExtra")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(testthat)
      > library(ftExtra)
      > 
      > test_check("ftExtra")
      ── 1. Failure: ast2df (@test-ast2df.R#51)  ─────────────────────────────────────
      ast2df(list(blocks = tree)) not identical to tibble::tibble(...).
      Attributes: < Component "class": Lengths (1, 3) differ (string compare on first 1) >
      Attributes: < Component "class": 1 string mismatch >
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 37 | SKIPPED: 1 | WARNINGS: 0 | FAILED: 1 ]
      1. Failure: ast2df (@test-ast2df.R#51) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# keyATM

<details>

* Version: 0.2.2
* Source code: https://github.com/cran/keyATM
* URL: https://keyatm.github.io/keyATM/
* BugReports: https://github.com/keyATM/keyATM/issues
* Date/Publication: 2020-06-02 23:30:02 UTC
* Number of recursive dependencies: 108

Run `cloud_details(, "keyATM")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Backtrace:
       1. keyATM::keyATM(...)
       2. keyATM:::keyATM_output(fitted)
       3. keyATM:::keyATM_output_theta(model, info)
       6. vctrs::vec_rbind(!!!dots, .names_to = .id)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 12 | SKIPPED: 6 | WARNINGS: 0 | FAILED: 4 ]
      1. Error: (unknown) (@test-keyATM.R#16) 
      2. Error: (unknown) (@test-keyATMHeterogeneity.R#51) 
      3. Error: (unknown) (@test-keyATMvb.R#14) 
      4. Error: (unknown) (@test-plot.R#11) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 16.5Mb
      sub-directories of 1Mb or more:
        libs  16.1Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘quanteda’
      All declared Imports should be used.
    ```

# kntnr

<details>

* Version: 0.4.4
* Source code: https://github.com/cran/kntnr
* URL: https://yutannihilation.github.io/kntnr/
* BugReports: https://github.com/yutannihilation/kntnr/issues
* Date/Publication: 2020-04-08 13:10:02 UTC
* Number of recursive dependencies: 47

Run `cloud_details(, "kntnr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── 4. Error: unnesting terriblly-nested records works (@test-parse-record.R#50) 
      Problem with `summarise()` input `na_count_fileKey`.
      ✖ object 'fileKey' not found
      ℹ Input `na_count_fileKey` is `sum(is.na(fileKey))`.
      ℹ The error occurred in group 1: record_id = "1".
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 70 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 4 ]
      1. Failure: parsing FILE works (@test-parse-field.R#8) 
      2. Failure: unnesting terriblly-nested records works (@test-parse-record.R#46) 
      3. Failure: unnesting terriblly-nested records works (@test-parse-record.R#47) 
      4. Error: unnesting terriblly-nested records works (@test-parse-record.R#50) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# nanny

<details>

* Version: 0.1.8
* Source code: https://github.com/cran/nanny
* Date/Publication: 2020-06-13 13:50:03 UTC
* Number of recursive dependencies: 150

Run `cloud_details(, "nanny")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘nanny-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: rotate_dimensions
    > ### Title: Rotate two dimensions (e.g., principal components) of an
    > ###   arbitrary angle
    > ### Aliases: rotate_dimensions rotate_dimensions,spec_tbl_df-method
    > ###   rotate_dimensions,tbl_df-method
    > 
    > ### ** Examples
    > 
    > 
    >  mtcars_tidy_MDS = reduce_dimensions(mtcars_tidy, car_model, feature, value, method="MDS")
    nanny says: to access the raw results do `attr(..., "internals")$MDS`
    >  
    >  rotate_dimensions(mtcars_tidy_MDS, `Dim1`, `Dim2`, .element = car_model, rotation_degrees = 45)
    Error in (function (classes, fdef, mtable)  : 
      unable to find an inherited method for function ‘as_matrix’ for signature ‘"data.frame"’
    Calls: rotate_dimensions ... withVisible -> <Anonymous> -> as_matrix -> <Anonymous>
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       11. nanny:::get_rotated_dimensions(...)
        4. dplyr::select(., !!.element, !!dimension_1_column, !!dimension_2_column)
        4. dplyr::distinct(.)
        4. nanny::as_matrix(., rownames = !!.element)
        4. base::t(.)
       11. nanny:::rotation(., rotation_degrees)
        9. dplyr::bind_rows(...)
       17. nanny::as_matrix(.)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 30 | SKIPPED: 0 | WARNINGS: 6 | FAILED: 1 ]
      1. Error: rotate dimensions (@test-methods.R#29) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘knitr’ ‘lifecycle’ ‘lme4’ ‘methods’
      All declared Imports should be used.
    ```

# predict3d

<details>

* Version: 0.1.3.3
* Source code: https://github.com/cran/predict3d
* URL: https://github.com/cardiomoon/predict3d
* BugReports: https://github.com/cardiomoon/predict3d/issues
* Date/Publication: 2019-09-03 13:00:02 UTC
* Number of recursive dependencies: 110

Run `cloud_details(, "predict3d")` for more info

</details>

## Newly broken

*   checking whether package ‘predict3d’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: 'rgl.init' failed, running with 'rgl.useNULL = TRUE'.
    See ‘/tmp/workdir/predict3d/new/predict3d.Rcheck/00install.out’ for details.
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘TH.data’ ‘moonBook’
      All declared Imports should be used.
    ```

# Rpolyhedra

<details>

* Version: 0.4.2
* Source code: https://github.com/cran/Rpolyhedra
* URL: https://github.com/ropensci/Rpolyhedra
* BugReports: https://github.com/ropensci/Rpolyhedra/issues
* Date/Publication: 2019-03-26 17:13:23 UTC
* Number of recursive dependencies: 84

Run `cloud_details(, "Rpolyhedra")` for more info

</details>

## Newly broken

*   checking whether package ‘Rpolyhedra’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: 'rgl.init' failed, running with 'rgl.useNULL = TRUE'.
    See ‘/tmp/workdir/Rpolyhedra/new/Rpolyhedra.Rcheck/00install.out’ for details.
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘testthat’
      All declared Imports should be used.
    ```

# simTool

<details>

* Version: 1.1.6
* Source code: https://github.com/cran/simTool
* URL: https://github.com/MarselScheer/simTool
* BugReports: https://github.com/MarselScheer/simTool/issues
* Date/Publication: 2020-05-17 14:20:03 UTC
* Number of recursive dependencies: 68

Run `cloud_details(, "simTool")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Component 7: Attributes: < Component "class": Lengths (1, 3) differ (string compare on first 1) >
      Component 7: Attributes: < Component "class": 1 string mismatch >
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 96 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 4 ]
      1. Failure: Named vectors combined to multiple columns (@test_bind_or_combine.R#9) 
      2. Failure: Three analyzing functions and one summary function.
                Results were created and stored in simulation (@test_eval_tibbles.R#585) 
      3. Failure: Three analyzing functions and three summary function.
                Results were created and stored in simulation (@test_eval_tibbles.R#743) 
      4. Failure: Three analyzing functions and one summary function over 2 cpus.
                Results were created and stored in simulation (@test_eval_tibbles.R#817) 
      
      Error: testthat unit tests failed
      Execution halted
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
    > # If the function returns >1 value, set the `unlist = FALSE` argument
    > # Running 5 number summary
    > summary_roll <- rollify(summary, window = 5, unlist = FALSE)
    > 
    > FB_summarised <- dplyr::mutate(FB, summary_roll = summary_roll(adjusted))
    > FB_summarised$summary_roll[[5]]
       Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
      27.77   28.00   28.76   28.60   29.06   29.42 
    > 
    > # dplyr::bind_rows() is often helpful in these cases to get
    > # meaningful output
    > 
    > summary_roll <- rollify(~dplyr::bind_rows(summary(.)), window = 5, unlist = FALSE)
    > FB_summarised <- dplyr::mutate(FB, summary_roll = summary_roll(adjusted))
    > FB_summarised %>%
    +   dplyr::filter(!is.na(summary_roll)) %>%
    +   tidyr::unnest(summary_roll)
    Error in vec_unchop(pieces, ptype = col_ptype) : 
      Internal error in `vec_proxy_assign_opts()`: `proxy` of type `integer` incompatible with `value` proxy of type `double`.
    Calls: %>% ... unnest.data.frame -> unchop -> df_unchop_info -> vec_unchop
    Execution halted
    ```

# timetk

<details>

* Version: 2.1.0
* Source code: https://github.com/cran/timetk
* URL: https://github.com/business-science/timetk
* BugReports: https://github.com/business-science/timetk/issues
* Date/Publication: 2020-07-03 11:40:02 UTC
* Number of recursive dependencies: 220

Run `cloud_details(, "timetk")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
     2 FB     2013-01-03     27.8           NA              NA  
     3 FB     2013-01-04     28.8           NA              NA  
     4 FB     2013-01-07     29.4           NA              NA  
     5 FB     2013-01-08     29.1           28.6            NA  
     6 FB     2013-01-09     30.6           29.1            NA  
     7 FB     2013-01-10     31.3           29.8            NA  
     8 FB     2013-01-11     31.7           30.4            NA  
     9 FB     2013-01-14     31.0           30.7            NA  
    10 FB     2013-01-15     30.1           30.9            29.8
    # … with 998 more rows
    > 
    > # For summary operations like rolling means, we can accomplish large-scale
    > # multi-rolls with tk_augment_slidify()
    > 
    > FB %>%
    +     select(symbol, date, adjusted) %>%
    +     tk_augment_slidify(
    +         adjusted, .period = 5:10, .f = mean, .align = "right",
    +         .names = str_c("MA_", 5:10)
    +     )
    New names:
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.4Mb
      sub-directories of 1Mb or more:
        doc    3.6Mb
        help   1.2Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2750 marked UTF-8 strings
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

*   checking examples ... ERROR
    ```
    ...
    > ### Name: get_prm
    > ### Title: Access model parameters
    > ### Aliases: get_prm
    > 
    > ### ** Examples
    > 
    > # Store the parameter table
    > prm <- get_prm(xpdb_ex_pk, .problem = 1)
    Returning parameter estimates from $prob no.1, subprob no.0, method foce
    > 
    > # Display parameters to the console
    > prm_table(xpdb_ex_pk, .problem = 1)
    
    Reporting transformed parameters:
    For the OMEGA and SIGMA matrices, values are reported as standard deviations for the diagonal elements and as correlations for the off-diagonal elements. The relative standard errors (RSE) for OMEGA and SIGMA are reported on the approximate standard deviation scale (SE/variance estimate)/2. Use `transform = FALSE` to report untransformed parameters.
    
    Estimates for $prob no., subprob no., method 
    Error in UseMethod("tbl_vars") : 
      no applicable method for 'tbl_vars' applied to an object of class "character"
    Calls: prm_table ... tbl_vars -> new_sel_vars -> structure -> tbl_vars_dispatch
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       30. dplyr::tbl_vars(x)
       33. dplyr:::tbl_vars_dispatch(x)
      
      ── 2. Failure: get_prm works properly (@test-xpdb_access.R#165)  ───────────────
      `get_prm_test_tr` not identical to `get_prm_ctrl_tr`.
      Attributes: < Component "class": Lengths (2, 4) differ (string compare on first 2) >
      Attributes: < Component "class": 1 string mismatch >
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 519 | SKIPPED: 7 | WARNINGS: 0 | FAILED: 2 ]
      1. Error: Check prm_table returns a proper message (@test-console_outputs.R#27) 
      2. Failure: get_prm works properly (@test-xpdb_access.R#165) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

