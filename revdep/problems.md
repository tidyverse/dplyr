# AlphaBeta

<details>

* Version: 1.1.2
* Source code: https://github.com/cran/AlphaBeta
* Date/Publication: 2020-04-26
* Number of recursive dependencies: 89

Run `revdep_details(,"AlphaBeta")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Running examples in ‘AlphaBeta-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: buildPedigree
    > ### Title: Building Pedigree
    > ### Aliases: buildPedigree
    > 
    > ### ** Examples
    > 
    > # Get some toy data
    > file <- system.file("extdata/dm/","nodelist.fn", package="AlphaBeta")
    > df<-read.csv(file)
    > df$filename <- gsub("^", paste0(dirname(dirname(file)),"/"), df$filename )
    > write.csv(df, file = paste0(dirname(file),"/", "tmp_nodelist.fn"), row.names=FALSE, quote=FALSE)
    > file <- system.file("extdata/dm/","tmp_nodelist.fn", package="AlphaBeta")
    > file2 <- system.file("extdata/dm/","edgelist.fn", package="AlphaBeta")
    > buildPedigree(nodelist = file, edgelist=file2, cytosine="CG", posteriorMaxFilter=0.99)
    constracting pedigree ...
    Error in .subset2(x, i, exact = exact) : subscript out of bounds
    Calls: buildPedigree ... cat -> paste0 -> [[ -> [[.data.frame -> <Anonymous>
    Execution halted
    ```

*   checking top-level files ... NOTE
    ```
    File
      LICENSE
    is not mentioned in the DESCRIPTION file.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ABplot: no visible binding for global variable ‘delta.t’
    ABplot: no visible binding for global variable ‘div.obs’
    BOOTmodel: multiple local function definitions for ‘divergence’ with
      different formal arguments
    plotPedigree: no visible binding for global variable ‘meth’
    plotPedigree: no visible binding for global variable ‘V1’
    plotPedigree: no visible binding for global variable ‘V2’
    Undefined global functions or variables:
      V1 V2 delta.t div.obs meth
    ```

## Newly fixed

*   checking whether package ‘AlphaBeta’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/AlphaBeta/old/AlphaBeta.Rcheck/00install.out’ for details.
    ```

# AMR

<details>

* Version: 1.1.0
* Source code: https://github.com/cran/AMR
* URL: https://msberends.gitlab.io/AMR, https://gitlab.com/msberends/AMR
* BugReports: https://gitlab.com/msberends/AMR/issues
* Date/Publication: 2020-04-15 14:00:19 UTC
* Number of recursive dependencies: 84

Run `revdep_details(,"AMR")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  6.7Mb
      sub-directories of 1Mb or more:
        data   3.9Mb
    ```

## Newly fixed

*   checking package dependencies ... ERROR
    ```
    Package required and available but unsuitable version: ‘vctrs’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# apyramid

<details>

* Version: 0.1.1
* Source code: https://github.com/cran/apyramid
* URL: https://github.com/R4EPI/apyramid, https://r4epis.netlify.com
* BugReports: https://github.com/R4EPI/apyramid/issues
* Date/Publication: 2020-03-11 20:00:02 UTC
* Number of recursive dependencies: 118

Run `revdep_details(,"apyramid")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > 
    > ### ** Examples
    > 
    > 
    > library(ggplot2)
    > old <- theme_set(theme_classic(base_size = 18))
    > 
    > # with pre-computed data ----------------------------------------------------
    > # 2018/2008 US census data by age and gender
    > data(us_2018)
    > data(us_2008)
    > age_pyramid(us_2018, age_group = age, split_by = gender, count = count)
    Error: Column 'n' is already present in output
     * Use `name = "new_name"` to pick a new name
    Backtrace:
        █
     1. └─apyramid::age_pyramid(...)
     2.   └─dplyr::tally(maxdata, wt = !!quote(n))
     3.     └─dplyr:::check_name(x, name)
     4.       └─dplyr:::glubort(...)
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── 1. Error: (unknown) (@test-age-pyramid.R#77)  ───────────────────────────────
      Column 'n' is already present in output
       * Use `name = "new_name"` to pick a new name
      Backtrace:
       1. apyramid::age_pyramid(dat, age_group = "AGE")
       2. dplyr::tally(maxdata, wt = !!quote(n))
       3. dplyr:::check_name(x, name)
       4. dplyr:::glubort(...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 51 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Error: (unknown) (@test-age-pyramid.R#77) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## Newly fixed

*   checking whether package ‘apyramid’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/apyramid/old/apyramid.Rcheck/00install.out’ for details.
    ```

# areal

<details>

* Version: 0.1.5
* Source code: https://github.com/cran/areal
* URL: https://github.com/slu-openGIS/areal
* BugReports: https://github.com/slu-openGIS/areal/issues
* Date/Publication: 2019-05-21 07:30:07 UTC
* Number of recursive dependencies: 65

Run `revdep_details(,"areal")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        6. areal::aw_interpolate(...)
        8. dplyr:::left_join.data.frame(exresults, inresults, by = tidQN)
        9. dplyr:::join_mutate(...)
       10. dplyr:::join_cols(...)
       11. dplyr:::standardise_join_by(by, x_names = x_names, y_names = y_names)
       12. dplyr:::check_join_vars(by$x, x_names)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 138 | SKIPPED: 0 | WARNINGS: 6 | FAILED: 3 ]
      1. Failure: correctly specified functions execute without error (@test_aw_interpolate.R#134) 
      2. Failure: correctly specified functions execute without error (@test_aw_interpolate.R#136) 
      3. Failure: correctly specified functions execute without error (@test_aw_interpolate.R#138) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## Newly fixed

*   checking whether package ‘areal’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/areal/old/areal.Rcheck/00install.out’ for details.
    ```

# AzureKusto

<details>

* Version: 1.0.6
* Source code: https://github.com/cran/AzureKusto
* URL: https://github.com/Azure/AzureKusto https://github.com/Azure/AzureR
* BugReports: https://github.com/Azure/AzureKusto/issues
* Date/Publication: 2020-04-27 05:30:02 UTC
* Number of recursive dependencies: 66

Run `revdep_details(,"AzureKusto")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tibble’
      All declared Imports should be used.
    ```

## Newly fixed

*   checking whether package ‘AzureKusto’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/AzureKusto/old/AzureKusto.Rcheck/00install.out’ for details.
    ```

# banR

<details>

* Version: 0.2.1
* Source code: https://github.com/cran/banR
* URL: http://joelgombin.github.io/banR/
* BugReports: http://github.com/joelgombin/banR/issues
* Date/Publication: 2019-12-05 07:00:03 UTC
* Number of recursive dependencies: 57

Run `revdep_details(,"banR")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library("testthat")
      > library("banR")
      > test_check("banR")
      ── 1. Failure: Code INSEE and Code postal return the same result (@test_geocodet
      geocode_tbl(tbl = table_check, adresse = num_voie, code_postal = cp) not equivalent to geocode_tbl(tbl = table_check, adresse = num_voie, code_insee = codecommune).
      Component 1: 2 string mismatches
      Component 2: 2 string mismatches
      Component 4: 2 string mismatches
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 7 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Failure: Code INSEE and Code postal return the same result (@test_geocodetbl.R#77) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## Newly fixed

*   checking whether package ‘banR’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/banR/old/banR.Rcheck/00install.out’ for details.
    ```

# BAwiR

<details>

* Version: 1.2.3
* Source code: https://github.com/cran/BAwiR
* URL: https://www.R-project.org, https://www.uv.es/vivigui, https://www.uv.es/vivigui/AppEuroACB.html
* Date/Publication: 2020-04-14 11:20:02 UTC
* Number of recursive dependencies: 126

Run `revdep_details(,"BAwiR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### ** Examples
    > 
    > df <- do_join_games_bio("ACB", acb_games_1718, acb_players_1718)
    > df1 <- do_add_adv_stats(df)
    > df_four_factors <- do_four_factors_df(df1, "Valencia")
    Error: Assigned data `paste(df6[df6$Team == i, 3:6], " (", orders_cols, ")", sep = "")` must be compatible with row subscript `df6$Team == i`.
    ✖ 1 row must be assigned.
    ✖ Assigned data has 4 rows.
    ℹ Row updates require a list value. Do you need `list()` or `as.list()`?
    Backtrace:
        █
     1. └─BAwiR::do_four_factors_df(df1, "Valencia")
     2.   ├─base::`[<-`(...)
     3.   └─tibble:::`[<-.tbl_df`(...)
     4.     └─tibble:::tbl_subassign(x, i, j, value, i_arg, j_arg, substitute(value))
     5.       └─tibble:::vectbl_recycle_rhs(...)
     6.         └─base::tryCatch(...)
     7.           └─base:::tryCatchList(expr, classes, parentenv, handlers)
     8.             └─base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
     9.               └─value[[3L]](cond)
    Execution halted
    ```

## Newly fixed

*   checking whether package ‘BAwiR’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/BAwiR/old/BAwiR.Rcheck/00install.out’ for details.
    ```

# bayesplot

<details>

* Version: 1.7.1
* Source code: https://github.com/cran/bayesplot
* URL: https://mc-stan.org/bayesplot
* BugReports: https://github.com/stan-dev/bayesplot/issues/
* Date/Publication: 2019-12-01 23:00:26 UTC
* Number of recursive dependencies: 143

Run `revdep_details(,"bayesplot")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 1010 | SKIPPED: 35 | WARNINGS: 16 | FAILED: 12 ]
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

*   checking installed package size ... NOTE
    ```
      installed size is  6.6Mb
      sub-directories of 1Mb or more:
        R     1.8Mb
        doc   4.1Mb
    ```

## Newly fixed

*   checking whether package ‘bayesplot’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/bayesplot/old/bayesplot.Rcheck/00install.out’ for details.
    ```

# BiocSet

<details>

* Version: 1.1.4
* Source code: https://github.com/cran/BiocSet
* Date/Publication: 2020-04-26
* Number of recursive dependencies: 125

Run `revdep_details(,"BiocSet")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > 
      > test_check("BiocSet")
      ── 1. Failure: 'summarise.tbl_elementset_base()' works (@test_tbl_elementset_bas
      `es %>% select(set) %>% summarise(set)` did not throw an error.
      
      ── 2. Failure: 'summarise.tbl_elementset_base()' works (@test_tbl_elementset_bas
      `es %>% select(element) %>% summarise(element)` did not throw an error.
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 486 | SKIPPED: 0 | WARNINGS: 2 | FAILED: 2 ]
      1. Failure: 'summarise.tbl_elementset_base()' works (@test_tbl_elementset_base-class.R#119) 
      2. Failure: 'summarise.tbl_elementset_base()' works (@test_tbl_elementset_base-class.R#127) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## Newly fixed

*   checking whether package ‘BiocSet’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/BiocSet/old/BiocSet.Rcheck/00install.out’ for details.
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘airway’
    ```

# biotmle

<details>

* Version: 1.11.3
* Source code: https://github.com/cran/biotmle
* URL: https://code.nimahejazi.org/biotmle
* BugReports: https://github.com/nhejazi/biotmle/issues
* Date/Publication: 2020-02-06
* Number of recursive dependencies: 143

Run `revdep_details(,"biotmle")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(biotmle)
      biotmle v1.11.3: Targeted Learning with Moderated Statistics for Biomarker
      Discovery
      > 
      > test_check("biotmle")
      ── 1. Failure: biomarkertmle output is consistent using example data (@test-biom
      assay(biomarkerTMLEout)[1, c(17, 83, 117)] not equal to c(360.7073, 375.9316, 319.3649).
      names for target but not for current
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 12 | SKIPPED: 0 | WARNINGS: 1 | FAILED: 1 ]
      1. Failure: biomarkertmle output is consistent using example data (@test-biomarkertmle.R#33) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## Newly fixed

*   checking whether package ‘biotmle’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/biotmle/old/biotmle.Rcheck/00install.out’ for details.
    ```

# brazilmaps

<details>

* Version: 0.1.0
* Source code: https://github.com/cran/brazilmaps
* URL: http://github.com/rpradosiqueira/brazilmaps
* BugReports: http://github.com/rpradosiqueira/brazilmaps/issues
* Date/Publication: 2017-09-21 17:02:52 UTC
* Number of recursive dependencies: 67

Run `revdep_details(,"brazilmaps")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘brazilmaps-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: get_brmap
    > ### Title: Get Brazilian maps from different geographic levels
    > ### Aliases: get_brmap
    > ### Keywords: IBGE geographic levels shapefile spatial
    > 
    > ### ** Examples
    > 
    > ## Retrieving the map from the State of Rio de Janeiro
    > rio_map <- get_brmap(geo = "State",
    +                      geo.filter = list(State = 33),
    +                      class = "sf")
    Error: Can't slice a scalar
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘sp’
      All declared Imports should be used.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘ggthemes’
    ```

## Newly fixed

*   checking whether package ‘brazilmaps’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/brazilmaps/old/brazilmaps.Rcheck/00install.out’ for details.
    ```

# broom.mixed

<details>

* Version: 0.2.5
* Source code: https://github.com/cran/broom.mixed
* URL: http://github.com/bbolker/broom.mixed
* BugReports: http://github.com/bbolker/broom.mixed/issues
* Date/Publication: 2020-04-19 04:50:08 UTC
* Number of recursive dependencies: 146

Run `revdep_details(,"broom.mixed")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      x[1]: "(Intercept)"
      y[1]: "1"
      
      x[2]: "sin(2 * pi * Time)"
      y[2]: "2"
      
      x[3]: "cos(2 * pi * Time)"
      y[3]: "3"
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 233 | SKIPPED: 0 | WARNINGS: 14 | FAILED: 1 ]
      1. Failure: basic gls tidying (@test-nlme.R#146) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Missing or unexported object: ‘dplyr::as.tbl_cube’
    ```

## Newly fixed

*   checking whether package ‘broom.mixed’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/broom.mixed/old/broom.mixed.Rcheck/00install.out’ for details.
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking: 'glmmADMB', 'R2jags'
    ```

# cattonum

<details>

* Version: 0.0.4
* Source code: https://github.com/cran/cattonum
* URL: https://github.com/bfgray3/cattonum
* BugReports: https://github.com/bfgray3/cattonum/issues
* Date/Publication: 2020-02-09 12:30:06 UTC
* Number of recursive dependencies: 71

Run `revdep_details(,"cattonum")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
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

## Newly fixed

*   checking whether package ‘cattonum’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/cattonum/old/cattonum.Rcheck/00install.out’ for details.
    ```

# codified

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/codified
* URL: https://ouhscbbmc.github.io/codified/, https://github.com/OuhscBbmc/codified, https://github.com/higgi13425/nih_enrollment_table
* BugReports: https://github.com/OuhscBbmc/codified/issues
* Date/Publication: 2018-09-30 16:10:02 UTC
* Number of recursive dependencies: 97

Run `revdep_details(,"codified")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Attributes: < Component 2: Attributes: < names for target but not for current > >
      Attributes: < Component 2: Attributes: < current is not list-like > >
      Attributes: < Component 2: Length mismatch: comparison on first 3 components >
      Attributes: < Component 2: Component 1: Modes: character, numeric >
      ...
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 2 | SKIPPED: 0 | WARNINGS: 1 | FAILED: 4 ]
      1. Failure: ds_1a --no metadata required (@test-table-nih-enrollment.R#52) 
      2. Failure: ds_1b --ethnicity metadata required (@test-table-nih-enrollment.R#76) 
      3. Failure: ds_1c --all metadata required (@test-table-nih-enrollment.R#116) 
      4. Failure: ds_1d --different variable names (@test-table-nih-enrollment.R#128) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘methods’ ‘readr’
      All declared Imports should be used.
    ```

## Newly fixed

*   checking whether package ‘codified’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/codified/old/codified.Rcheck/00install.out’ for details.
    ```

# CollapseLevels

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/CollapseLevels
* Date/Publication: 2017-12-04 10:30:12 UTC
* Number of recursive dependencies: 65

Run `revdep_details(,"CollapseLevels")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Error: `summarise()` argument `tot` errored.
    ℹ `tot` is `n()`.
    ℹ The error occured in group 1: Account_Balance = "A11".
    ✖ could not find function "n"
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
     11.               ├─dplyr:::summarise.grouped_df(., tot = n())
     12.               ├─base::NextMethod()
     13.               └─dplyr:::summarise.data.frame(., tot = n())
     14.  
    Execution halted
    ```

## Newly fixed

*   checking whether package ‘CollapseLevels’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/CollapseLevels/old/CollapseLevels.Rcheck/00install.out’ for details.
    ```

# comperes

<details>

* Version: 0.2.3
* Source code: https://github.com/cran/comperes
* URL: https://github.com/echasnovski/comperes
* BugReports: https://github.com/echasnovski/comperes/issues
* Date/Publication: 2019-12-14 21:40:03 UTC
* Number of recursive dependencies: 58

Run `revdep_details(,"comperes")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Attributes: < Component "class": Lengths (4, 1) differ (string compare on first 1) >
      Attributes: < Component "class": 1 string mismatch >
      
      ── 2. Failure: get_matchups works (@test-utils.R#177)  ─────────────────────────
      `output` not equal to `output_ref`.
      Attributes: < Component "class": Lengths (4, 3) differ (string compare on first 3) >
      Attributes: < Component "class": 3 string mismatches >
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 260 | SKIPPED: 0 | WARNINGS: 3 | FAILED: 2 ]
      1. Failure: as_widecr.default throws an error if no column is matched (@test-results-widecr.R#133) 
      2. Failure: get_matchups works (@test-utils.R#177) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## Newly fixed

*   checking whether package ‘comperes’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/comperes/old/comperes.Rcheck/00install.out’ for details.
    ```

# concaveman

<details>

* Version: 1.0.0
* Source code: https://github.com/cran/concaveman
* URL: http://www.github.com/joelgombin/concaveman
* BugReports: http://www.github.com/joelgombin/concaveman/issues
* Date/Publication: 2017-07-25 22:37:09 UTC
* Number of recursive dependencies: 49

Run `revdep_details(,"concaveman")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘concaveman-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: concaveman
    > ### Title: concaveman: A very fast 2D concave hull algorithm.
    > ### Aliases: concaveman concaveman-package concaveman concaveman.matrix
    > ###   concaveman.sf concaveman.SpatialPoints
    > 
    > ### ** Examples
    > 
    > data(points)
    > polygons <- concaveman(points)
    > plot(points)
    > plot(polygons, add = TRUE)
    Error in plot.sf(polygons, add = TRUE) : 
      plotting list-columns not supported
    Calls: plot -> plot.sf
    Execution halted
    ```

## Newly fixed

*   checking whether package ‘concaveman’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/concaveman/old/concaveman.Rcheck/00install.out’ for details.
    ```

# cTRAP

<details>

* Version: 1.4.0
* Source code: https://github.com/cran/cTRAP
* URL: https://github.com/nuno-agostinho/cTRAP
* BugReports: https://github.com/nuno-agostinho/cTRAP/issues
* Date/Publication: 2019-10-29
* Number of recursive dependencies: 114

Run `revdep_details(,"cTRAP")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > 
    > ## Not run: 
    > ##D # Download and load CMap perturbations to compare with
    > ##D cellLine <- c("HepG2", "HUH7")
    > ##D cmapMetadataCompounds <- filterCMapMetadata(
    > ##D     "cmapMetadata.txt", cellLine=cellLine, timepoint="24 h",
    > ##D     dosage="5 \u00B5M", perturbationType="Compound")
    > ##D 
    > ##D cmapPerturbationsCompounds <- prepareCMapPerturbations(
    > ##D     cmapMetadataCompounds, "cmapZscores.gctx", "cmapGeneInfo.txt",
    > ##D     "cmapCompoundInfo_drugs.txt", loadZscores=TRUE)
    > ## End(Not run)
    > perturbations <- cmapPerturbationsCompounds
    > 
    > # Rank similar CMap perturbations (by default, Spearman's and Pearson's
    > # correlation are used, as well as GSEA with the top and bottom 150 genes of
    > # the differential expression profile used as reference)
    > rankSimilarPerturbations(diffExprStat, perturbations)
    Subsetting data based on 8790 intersecting genes (65% of the 13451 input genes)...
    Correlating against 22 CMap perturbations (2 cell lines; Spearman's correlation)...
    New names:
    ```

## In both

*   R CMD check timed out
    

*   checking installed package size ... NOTE
    ```
      installed size is  6.9Mb
      sub-directories of 1Mb or more:
        data   4.8Mb
        doc    1.5Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked UTF-8 string
    ```

# cutpointr

<details>

* Version: 1.0.2
* Source code: https://github.com/cran/cutpointr
* URL: https://github.com/thie1e/cutpointr
* BugReports: https://github.com/thie1e/cutpointr/issues
* Date/Publication: 2020-04-14 08:50:10 UTC
* Number of recursive dependencies: 78

Run `revdep_details(,"cutpointr")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 489 | SKIPPED: 0 | WARNINGS: 1778 | FAILED: 14 ]
      1. Failure: LOESS smoothing does not return warnings or errors (@test-cutpointr.R#681) 
      2. Failure: LOESS smoothing does not return warnings or errors (@test-cutpointr.R#696) 
      3. Failure: LOESS smoothing does not return warnings or errors (@test-cutpointr.R#704) 
      4. Failure: LOESS smoothing does not return warnings or errors (@test-cutpointr.R#718) 
      5. Failure: LOESS smoothing does not return warnings or errors (@test-cutpointr.R#729) 
      6. Failure: cutpointr works if method / metric are called with :: (@test-cutpointr.R#1291) 
      7. Failure: cutpointr works if method / metric are called with :: (@test-cutpointr.R#1294) 
      8. Failure: Summary(multi_cutpointr) is silent (@test-cutpointr.R#1324) 
      9. Failure: Summary(multi_cutpointr) is silent (@test-cutpointr.R#1331) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

## Newly fixed

*   checking package dependencies ... ERROR
    ```
    Package required and available but unsuitable version: ‘tibble’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# dat

<details>

* Version: 0.4.0
* Source code: https://github.com/cran/dat
* BugReports: https://github.com/wahani/dat/issues
* Date/Publication: 2018-01-20 15:36:18 UTC
* Number of recursive dependencies: 72

Run `revdep_details(,"dat")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘dat-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: DataFrame
    > ### Title: DataFrame and methods
    > ### Aliases: DataFrame DataFrame as.DataFrame as.DataFrame.default
    > ###   as.DataFrame.data.frame [.DataFrame
    > 
    > ### ** Examples
    > 
    > data("airquality")
    > dat <- as.DataFrame(airquality)
    > dat[~ Month > 4, ][meanWind ~ mean(Wind), sby = "Month"]["meanWind"]
    Error in (function (classes, fdef, mtable)  : 
      unable to find an inherited method for function ‘handleCols’ for signature ‘"DataFrame", "NULL", "numeric", "NULL", "NULL"’
    Calls: [ ... freduce -> <Anonymous> -> handleCols -> <Anonymous>
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       36. dat:::`[.DataFrame`(.data, , 0)
       16. memClassHandler$memClass(.)
       16. dat:::handleRows(., dispatcher(i))
       23. dat:::handleCols(...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 76 | SKIPPED: 1 | WARNINGS: 5 | FAILED: 5 ]
      1. Error: Basic syntax of a DataFrame (@test-DataFrame.R#21) 
      2. Error: split-apply-combine 
      3. Error: mutars by and sby (@test-mutar.R#21) 
      4. Error: S4 stuff (@test-mutar.R#60) 
      5. Error: Scoping (@test-mutar.R#81) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## Newly fixed

*   checking whether package ‘dat’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/dat/old/dat.Rcheck/00install.out’ for details.
    ```

# datastepr

<details>

* Version: 0.0.2
* Source code: https://github.com/cran/datastepr
* URL: https://github.com/bramtayl/datastepr
* BugReports: https://github.com/bramtayl/datastepr/issues
* Date/Publication: 2016-08-20 10:31:35
* Number of recursive dependencies: 57

Run `revdep_details(,"datastepr")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
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

## Newly fixed

*   checking whether package ‘datastepr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/datastepr/old/datastepr.Rcheck/00install.out’ for details.
    ```

# dbparser

<details>

* Version: 1.1.0
* Source code: https://github.com/cran/dbparser
* URL: https://docs.ropensci.org/dbparser, https://github.com/ropensci/dbparser
* BugReports: https://github.com/ropensci/dbparser/issues
* Date/Publication: 2020-02-21 05:40:03 UTC
* Number of recursive dependencies: 61

Run `revdep_details(,"dbparser")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(dbparser)
      > 
      > test_check("dbparser")
      ── 1. Failure: Read drug sequences attributes (@test_drug_main_node_parser_small
      is_tibble(drug_sequences()) isn't true.
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 472 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Failure: Read drug sequences attributes (@test_drug_main_node_parser_small_molecule.R#319) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘RSQLite’ ‘dplyr’
      All declared Imports should be used.
    ```

## Newly fixed

*   checking whether package ‘dbparser’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/dbparser/old/dbparser.Rcheck/00install.out’ for details.
    ```

# dbplyr

<details>

* Version: 1.4.3
* Source code: https://github.com/cran/dbplyr
* URL: https://dbplyr.tidyverse.org/, https://github.com/tidyverse/dbplyr
* BugReports: https://github.com/tidyverse/dbplyr/issues
* Date/Publication: 2020-04-19 09:40:03 UTC
* Number of recursive dependencies: 68

Run `revdep_details(,"dbplyr")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        8. dplyr::group_by(., z = x + y)
       10. dplyr::group_by_prepare(.data, !!!dots, .add = .add)
       11. dplyr:::add_computed_columns(.data, new_groups)
       12. dplyr:::mutate_cols(.data, !!!vars)
       13. DataMask$new(.data, caller_env())
       14. .subset2(public_bind_env, "initialize")(...)
       15. dplyr::group_rows(data)
       16. dplyr::group_data(.data)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 638 | SKIPPED: 8 | WARNINGS: 0 | FAILED: 1 ]
      1. Error: group_by can perform mutate (@test-verb-group_by.R#41) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## Newly fixed

*   checking whether package ‘dbplyr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/dbplyr/old/dbplyr.Rcheck/00install.out’ for details.
    ```

# ddpcr

<details>

* Version: 1.14
* Source code: https://github.com/cran/ddpcr
* URL: https://github.com/daattali/ddpcr
* BugReports: https://github.com/daattali/ddpcr/issues
* Date/Publication: 2020-03-23 18:00:06 UTC
* Number of recursive dependencies: 88

Run `revdep_details(,"ddpcr")` for more info

</details>

## Newly broken

*   checking whether package ‘ddpcr’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: `mutate_()` is deprecated as of dplyr 0.7.0.
      Warning: `select_()` is deprecated as of dplyr 0.7.0.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/ddpcr/new/ddpcr.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking whether package ‘ddpcr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/ddpcr/old/ddpcr.Rcheck/00install.out’ for details.
    ```

# DEGreport

<details>

* Version: 1.23.3
* Source code: https://github.com/cran/DEGreport
* URL: http://lpantano.github.io/DEGreport/
* BugReports: https://github.com/lpantano/DEGreport/issues
* Date/Publication: 2020-03-09
* Number of recursive dependencies: 132

Run `revdep_details(,"DEGreport")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Working with 86 genes after filtering: minc > 15
    Joining, by = "merge"
    Error: `summarise()` argument `n_genes` errored.
    ℹ `n_genes` is `n()`.
    ℹ The error occured in group 1: merge = "aFemale", cluster = 1, group = "Female", other = "a".
    ✖ could not find function "n"
    Backtrace:
         █
      1. └─DEGreport::degPatterns(ma, des, time = "group", col = "other")
      2.   └─`%>%`(...)
      3.     ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      4.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5.       └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      6.         └─DEGreport:::`_fseq`(`_lhs`)
      7.           └─magrittr::freduce(value, `_function_list`)
      8.             └─function_list[[i]](value)
      9.               ├─dplyr::summarise(., abundance = median(value), n_genes = n())
     10.               ├─dplyr:::summarise.grouped_df(., abundance = median(value), n_genes = n())
     11.               ├─base::NextMethod()
     12.               └─dplyr:::summarise.data.frame(., abundance = median(value), n_genes = 
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        9. dplyr::summarise(., abundance = median(value), n_genes = n())
       13. dplyr:::summarise_cols(.data, ...)
       14. base::tryCatch(...)
       15. base:::tryCatchList(expr, classes, parentenv, handlers)
       16. base:::tryCatchOne(...)
       17. value[[3L]](cond)
       18. dplyr:::stop_eval_tidy(e, index = i, dots = dots, fn = "summarise")
       19. dplyr:::stop_dplyr(index, dots, fn, "errored", x = conditionMessage(e))
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 50 | SKIPPED: 0 | WARNINGS: 2 | FAILED: 1 ]
      1. Error: (unknown) (@test_cluster.R#13) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    degMV: no visible binding for global variable ‘max_sd’
    degPatterns: no visible global function definition for ‘rowMedians’
    degPatterns: no visible binding for global variable ‘genes’
    degPatterns: no visible global function definition for ‘n’
    degPlotCluster: no visible binding for global variable ‘genes’
    degPlotCluster: no visible binding for global variable ‘cluster’
    degPlotWide : <anonymous>: no visible binding for global variable
      ‘count’
    significants,TopTags: no visible binding for global variable ‘FDR’
    significants,TopTags: no visible binding for global variable ‘logFC’
    significants,list : <anonymous>: no visible binding for global variable
      ‘gene’
    Undefined global functions or variables:
      .x FDR base_mean boxplot cluster comp compare count counts covar
      cutoff desc enrichGO fdr fdrtool gene genes itemConsensus k keys lm
      log2FoldChange log2fc logFC max_sd min_median n p.value r ratios
      rowMedians score simplify value_fc value_fdr x xend y yend
    Consider adding
      importFrom("graphics", "boxplot")
      importFrom("stats", "lm")
    to your NAMESPACE file.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘fdrtool’
    ```

## Newly fixed

*   checking whether package ‘DEGreport’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/DEGreport/old/DEGreport.Rcheck/00install.out’ for details.
    ```

## In both

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .travis.yml
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

# DiagrammeR

<details>

* Version: 1.0.5
* Source code: https://github.com/cran/DiagrammeR
* URL: https://github.com/rich-iannone/DiagrammeR
* BugReports: https://github.com/rich-iannone/DiagrammeR/issues
* Date/Publication: 2020-01-16 17:20:03 UTC
* Number of recursive dependencies: 86

Run `revdep_details(,"DiagrammeR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Error: `filter()` argument `..1` errored.
    ℹ `..1` is `node_edge__ == "node"`.
    ✖ object 'node_edge__' not found
    Backtrace:
         █
      1. └─`%>%`(...)
      2.   ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      3.   └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      4.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5.       └─`_fseq`(`_lhs`)
      6.         └─magrittr::freduce(value, `_function_list`)
      7.           └─function_list[[i]](value)
      8.             └─DiagrammeR::set_df_as_node_attr(., node = 1, df = df_1)
      9.               └─`%>%`(...)
     10.                 ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
     11.                 └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
     12.                   └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
     13.                     └─DiagrammeR:::`_fseq`(`_lhs`)
     14.                       └─magrittr::freduce(value, `_function_list`)
     15.                         
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 2132 | SKIPPED: 0 | WARNINGS: 5 | FAILED: 10 ]
      1.  Error: Printing a summary line for stored data frames works (@test-print.R#520) 
      2.  Error: rescaling node attributes in a graph is possible (@test-rescale_node_edge_attrs.R#17) 
      3.  Error: rescaling edge attributes in a graph is possible (@test-rescale_node_edge_attrs.R#88) 
      4.  Error: setting DFs as node attributes is possible (@test-set_get_dfs_as_attrs.R#25) 
      5.  Error: setting DFs as edge attributes is possible (@test-set_get_dfs_as_attrs.R#115) 
      6.  Error: getting DFs as node/edge attributes is possible (@test-set_get_dfs_as_attrs.R#206) 
      7.  Error: Getting node attributes with a selection is possible (@test-set_get_node_edge_attrs.R#370) 
      8.  Failure: copying values with `trav_in_edge()` works (@test-traversals_copying_attr_vals.R#112) 
      9.  Failure: copying values with `trav_in_edge()` works (@test-traversals_copying_attr_vals.R#151) 
      10. Failure: copying values with `trav_in()` works (@test-traversals_copying_attr_vals.R#293) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 17.1Mb
      sub-directories of 1Mb or more:
        doc           9.7Mb
        htmlwidgets   5.9Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked UTF-8 string
    ```

## Newly fixed

*   checking whether package ‘DiagrammeR’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/DiagrammeR/old/DiagrammeR.Rcheck/00install.out’ for details.
    ```

# dials

<details>

* Version: 0.0.6
* Source code: https://github.com/cran/dials
* URL: https://tidymodels.github.io/dials, https://github.com/tidymodels/dials
* BugReports: https://github.com/tidymodels/dials/issues
* Date/Publication: 2020-04-03 15:40:05 UTC
* Number of recursive dependencies: 64

Run `revdep_details(,"dials")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       10. dplyr::filter(., id == "prune")
       18. dplyr:::filter_rows(.data, ...)
       19. DataMask$new(.data, caller_env())
       20. .subset2(public_bind_env, "initialize")(...)
       21. dplyr::group_rows(data)
       23. dplyr:::group_data.data.frame(.data)
       26. dials:::`[.parameters`(.data, , 0)
       27. dials:::check_new_names(res)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 315 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Error: dplyr ops (@test_dplyr_set_compat.R#32) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## Newly fixed

*   checking whether package ‘dials’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/dials/old/dials.Rcheck/00install.out’ for details.
    ```

# docxtools

<details>

* Version: 0.2.1
* Source code: https://github.com/cran/docxtools
* URL: https://graphdr.github.io/docxtools
* BugReports: https://github.com/graphdr/docxtools/issues
* Date/Publication: 2019-02-09 18:43:13 UTC
* Number of recursive dependencies: 75

Run `revdep_details(,"docxtools")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > format_engr(x)
    Error: `mutate()` argument `observ_index` must be recyclable.
    ℹ `observ_index` is `1:dplyr::n()`.
    ✖ `observ_index` can't be recycled to size 0.
    ℹ `observ_index` must be size 0 or 1, not 2.
    Backtrace:
         █
      1. └─docxtools::format_engr(x)
      2.   └─docxtools:::obs_add(numeric_as_is)
      3.     ├─dplyr::mutate(x, observ_index = 1:dplyr::n())
      4.     └─dplyr:::mutate.data.frame(x, observ_index = 1:dplyr::n())
      5.       └─dplyr:::mutate_cols(.data, ...)
      6.         └─base::tryCatch(...)
      7.           └─base:::tryCatchList(expr, classes, parentenv, handlers)
      8.             ├─base:::tryCatchOne(...)
      9.             │ └─base:::doTryCatch(return(expr), name, parentenv, handler)
     10.             └─base:::tryCatchList(expr, names[-nh], parentenv, handlers[-nh])
     11.               ├─base:::tryCatchOne(...)
     12.               │ └─base:::doTryCatch(return(expr), name, parentenv, handler)
     13.               └─base:::tryCatchList(
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       14. base:::tryCatchList(expr, names[-nh], parentenv, handlers[-nh])
       17. base:::tryCatchList(expr, names[-nh], parentenv, handlers[-nh])
       20. base:::tryCatchList(expr, names[-nh], parentenv, handlers[-nh])
       21. base:::tryCatchOne(...)
       22. value[[3L]](cond)
       23. dplyr:::stop_mutate_recycle_incompatible_size(e, index = i, dots = dots)
       24. dplyr:::stop_dplyr(...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 19 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 2 ]
      1. Error: Factors are returned unaffected (@test_format_engr.R#14) 
      2. Error: (unknown) (@test_format_engr.R#55) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## Newly fixed

*   checking whether package ‘docxtools’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/docxtools/old/docxtools.Rcheck/00install.out’ for details.
    ```

# DuoClustering2018

<details>

* Version: 1.4.0
* Source code: https://github.com/cran/DuoClustering2018
* Date/Publication: 2019-11-05
* Number of recursive dependencies: 158

Run `revdep_details(,"DuoClustering2018")` for more info

</details>

## Newly broken

*   checking whether package ‘DuoClustering2018’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: `select_()` is deprecated as of dplyr 0.7.0.
      Warning: `filter_()` is deprecated as of dplyr 0.7.0.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/DuoClustering2018/new/DuoClustering2018.Rcheck/00install.out’ for details.
    ```

*   checking whether the namespace can be loaded with stated dependencies ... NOTE
    ```
    Warning: `select_()` is deprecated as of dplyr 0.7.0.
    Please use `select()` instead.
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_warnings()` to see where this warning was generated.
    Warning: `filter_()` is deprecated as of dplyr 0.7.0.
    Please use `filter()` instead.
    See vignette('programming') for more help
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_warnings()` to see where this warning was generated.
    
    A namespace must be able to be loaded with just the base namespace
    loaded: otherwise if the namespace gets loaded by a saved object, the
    session will be unable to start.
    
    Probably some imports need to be declared in the NAMESPACE file.
    ```

## In both

*   R CMD check timed out
    

*   checking R code for possible problems ... NOTE
    ```
    ...
    plot_timing: no visible binding for global variable ‘dataset’
    plot_timing: no visible binding for global variable ‘method’
    plot_timing: no visible binding for global variable ‘run’
    plot_timing: no visible binding for global variable ‘k’
    plot_timing: no visible binding for global variable ‘cluster’
    plot_timing: no visible binding for global variable ‘trueclass’
    plot_timing: no visible binding for global variable ‘est_k’
    plot_timing: no visible binding for global variable ‘elapsed’
    plot_timing: no visible binding for global variable ‘sce’
    plot_timing: no visible binding for global variable ‘filtering’
    plot_timing: no visible binding for global variable ‘truenclust’
    plot_timing: no visible binding for global variable ‘median.elapsed’
    plot_timing: no visible binding for global variable ‘med.t’
    plot_timing: no visible binding for global variable ‘norm.time’
    plot_timing: no visible binding for global variable ‘medianelapsed’
    Undefined global functions or variables:
      ARI ari.stab cell cluster data.wide dataset ds ds.norm elapsed
      entropy est_k estnclust filtering k k_diff med.t medARI
      median.elapsed median.stability medianARI medianelapsed method
      norm.time run s s.norm s.true s.true.norm sce stability trueclass
      truenclust
    ```

# egor

<details>

* Version: 0.20.03
* Source code: https://github.com/cran/egor
* URL: https://github.com/tilltnet/egor, https://tilltnet.github.io/egor/
* BugReports: https://github.com/tilltnet/egor/issues
* Date/Publication: 2020-03-03 00:20:02 UTC
* Number of recursive dependencies: 73

Run `revdep_details(,"egor")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        NULL
      Alter survey design:
        Maximum nominations: 
      EI-Index: age
      EI-Index: sex
      EI-Index: sex
      EI-Index: int_var
      EI-Index: female
      EI-Index: female
      Sorting data by egoID: Transforming alters data to long format: Transforming wide dyad data to edgelist: Filtering out empty alter entries using provided network size values: ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 115 | SKIPPED: 1 | WARNINGS: 1 | FAILED: 1 ]
      1. Failure: methods for dplyr are working (@test_dplyr_methods.R#7) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking S3 generic/method consistency ... WARNING
    ```
    group_modify:
      function(.data, .f, ..., keep)
    group_modify.egor:
      function(.tbl, .f, ..., keep)
    
    pull:
      function(.data, var, name)
    pull.egor:
      function(.data, var)
    
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

## Newly fixed

*   checking whether package ‘egor’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/egor/old/egor.Rcheck/00install.out’ for details.
    ```

# episheet

<details>

* Version: 0.4.0
* Source code: https://github.com/cran/episheet
* URL: https://github.com/epijim/episheet
* BugReports: https://github.com/epijim/episheet/issues
* Date/Publication: 2019-01-23 20:30:03 UTC
* Number of recursive dependencies: 65

Run `revdep_details(,"episheet")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Error: `summarise()` argument `n` errored.
    ℹ `n` is `n()`.
    ✖ could not find function "n"
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
     12.                   └─base::tryCatch(...)
     13.                     └─base:::tryCatchList(expr, classes, parentenv, handlers)
     14.                       └─base:::tryCatchOne(...)
     15.                         └─value[[3L]](
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
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

## Newly fixed

*   checking whether package ‘episheet’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/episheet/old/episheet.Rcheck/00install.out’ for details.
    ```

# esvis

<details>

* Version: 0.3.0
* Source code: https://github.com/cran/esvis
* URL: https://github.com/datalorax/esvis
* BugReports: https://github.com/datalorax/esvis/issues
* Date/Publication: 2020-02-28 17:10:02 UTC
* Number of recursive dependencies: 81

Run `revdep_details(,"esvis")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### Aliases: auc
    > 
    > ### ** Examples
    > 
    > 
    > # Calculate AUC for all pairwise comparisons
    > auc(star, reading ~ condition) 
    Warning: `funs()` is deprecated as of dplyr 0.8.0.
    Please use a list of either functions or lambdas: 
    
      # Simple named list: 
      list(mean = mean, median = median)
    
      # Auto named with `tibble::lst()`: 
      tibble::lst(mean, median)
    
      # Using lambdas
      list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_warnings()` to see where this warning was generated.
    New names:
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 11 | SKIPPED: 0 | WARNINGS: 7 | FAILED: 9 ]
      1. Error: Area under the curve computes and outputs correctly (@test-auc.R#10) 
      2. Error: Reference group subsetting works correctly (@test-auc.R#16) 
      3. Error: Hedges g computes and outputs correctly (@test-coh_d.R#10) 
      4. Error: Reference group subsetting works correctly (@test-coh_d.R#16) 
      5. Error: Hedges g computes and outputs correctly (@test-hedge_g.R#10) 
      6. Error: Reference group subsetting works correctly (@test-hedge_g.R#16) 
      7. Error: `pp_plot` produces expected output (@test-pp_plot.R#2) 
      8. Error: V computes and outputs correctly (@test-v.R#10) 
      9. Error: Reference group subsetting works correctly (@test-v.R#16) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## Newly fixed

*   checking whether package ‘esvis’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/esvis/old/esvis.Rcheck/00install.out’ for details.
    ```

# expstudies

<details>

* Version: 0.0.5
* Source code: https://github.com/cran/expstudies
* Date/Publication: 2019-06-14 11:20:03 UTC
* Number of recursive dependencies: 53

Run `revdep_details(,"expstudies")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
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

## Newly fixed

*   checking whether package ‘expstudies’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/expstudies/old/expstudies.Rcheck/00install.out’ for details.
    ```

# ezplot

<details>

* Version: 0.4.1
* Source code: https://github.com/cran/ezplot
* Date/Publication: 2020-04-25 16:30:03 UTC
* Number of recursive dependencies: 99

Run `revdep_details(,"ezplot")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > variable_plot(ansett, "Week", "Passengers", facet_x = "Class")
    Error: Can't join on `x$x` x `y$x` because of incompatible types. 
    ℹ `x$x` is of type <date>>.
    ℹ `y$x` is of type <date>>.
    Backtrace:
         █
      1. └─ezplot::variable_plot(ansett, "Week", "Passengers", facet_x = "Class")
      2.   └─`%>%`(...)
      3.     ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      4.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5.       └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      6.         └─ezplot:::`_fseq`(`_lhs`)
      7.           └─magrittr::freduce(value, `_function_list`)
      8.             ├─base::withVisible(function_list[[k]](value))
      9.             └─function_list[[k]](value)
     10.               ├─dplyr::left_join(., gdata, by = setdiff(names(gdata), c("value")))
     11.               └─dplyr:::left_join.data.frame(...)
     12.                 └─dplyr:::join_mutate(...)
     13.                   └─dplyr:::join_rows(x_key, y_key, type = type, na_equal = na_equal)
     14.                     └
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        2. tidyr::unnest(., x)
       10. dplyr::left_join(., gdata, by = setdiff(names(gdata), c("value")))
       12. dplyr:::join_mutate(...)
       13. dplyr:::join_rows(x_key, y_key, type = type, na_equal = na_equal)
       14. base::tryCatch(...)
       15. base:::tryCatchList(expr, classes, parentenv, handlers)
       16. base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
       17. value[[3L]](cond)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 80 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Error: variable_plot line geom works (@test-variable_plot.R#10) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## Newly fixed

*   checking whether package ‘ezplot’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/ezplot/old/ezplot.Rcheck/00install.out’ for details.
    ```

# fabletools

<details>

* Version: 0.1.3
* Source code: https://github.com/cran/fabletools
* URL: http://fabletools.tidyverts.org/, https://github.com/tidyverts/fabletools
* BugReports: https://github.com/tidyverts/fabletools/issues
* Date/Publication: 2020-03-24 07:10:02 UTC
* Number of recursive dependencies: 92

Run `revdep_details(,"fabletools")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Error: Can't determine index and please specify argument `index`.
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
     11.               ├─dplyr::groups(object)
     12.               └─dplyr:::groups.data.frame(object)
     13.                 ├─rlang::syms(group_vars(x))
     14.                 │ └─rlang:::map(x, sym)
     15.                 │   └─base::lapply(.x, .f, ...)
     16.                 ├─dplyr::group_vars(x)
     17.                 
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       22. tsibble:::duplicated_key_index(data, key, index)
       23. dplyr::grouped_df(as_tibble(data), key)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 256 | SKIPPED: 1 | WARNINGS: 0 | FAILED: 7 ]
      1. Error: Out-of-sample accuracy (@test-accuracy.R#52) 
      2. Error: fable dplyr verbs (@test-fable.R#32) 
      3. Failure: features() (@test-features.R#23) 
      4. Error: generate (@test-generate.R#6) 
      5. Error: generate seed setting (@test-generate.R#31) 
      6. Error: autoplot.fbl_ts() (@test-graphics.R#227) 
      7. Error: reconciliation (@test-reconciliation.R#4) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## Newly fixed

*   checking whether package ‘fabletools’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/fabletools/old/fabletools.Rcheck/00install.out’ for details.
    ```

# finalfit

<details>

* Version: 1.0.1
* Source code: https://github.com/cran/finalfit
* URL: https://github.com/ewenharrison/finalfit
* BugReports: https://github.com/ewenharrison/finalfit/issues
* Date/Publication: 2020-04-21 11:50:02 UTC
* Number of recursive dependencies: 113

Run `revdep_details(,"finalfit")` for more info

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

## Newly fixed

*   checking whether package ‘finalfit’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/finalfit/old/finalfit.Rcheck/00install.out’ for details.
    ```

# foieGras

<details>

* Version: 0.4.0
* Source code: https://github.com/cran/foieGras
* URL: https://cran.r-project.org/package=foieGras
* BugReports: https://github.com/ianjonsen/foieGras/issues
* Date/Publication: 2019-10-07 22:10:03 UTC
* Number of recursive dependencies: 100

Run `revdep_details(,"foieGras")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    The error most likely occurred in:
    
    > ### Name: fit_ssm
    > ### Title: Fit a continuous-time state-space model to filter Argos
    > ###   satellite geolocation data
    > ### Aliases: fit_ssm
    > 
    > ### ** Examples
    > 
    > ## fit rw model to one seal with Argos KF data
    > data(ellie)
    > fit <- fit_ssm(ellie, model = "rw", time.step = 24)
    
    pre-filtering data...
    
    fitting SSM...
    Warning in sqrt(as.numeric(object$diag.cov.random)) : NaNs produced
    > 
    > ## time series plots of predicted value fits
    > plot(fit, what = "predicted", type = 1)
    New names:
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Can't subset columns that don't exist.
      ✖ Column `shut.up` doesn't exist.
      Backtrace:
        1. base::plot(fssm, what = "fitted")
       31. vctrs:::stop_subscript_oob(...)
       32. vctrs:::stop_subscript(...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 27 | SKIPPED: 14 | WARNINGS: 1 | FAILED: 3 ]
      1. Error: fit_ssm defaults + crw + KF return foieGras list w 15 elements (@test-fit_ssm.R#34) 
      2. Error: (unknown) (@test-join.R#7) 
      3. Error: (unknown) (@test-plot.R#7) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 13.7Mb
      sub-directories of 1Mb or more:
        libs  12.4Mb
    ```

## Newly fixed

*   checking whether package ‘foieGras’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/foieGras/old/foieGras.Rcheck/00install.out’ for details.
    ```

# forecastML

<details>

* Version: 0.8.0
* Source code: https://github.com/cran/forecastML
* URL: https://github.com/nredell/forecastML/
* Date/Publication: 2020-02-28 22:40:12 UTC
* Number of recursive dependencies: 93

Run `revdep_details(,"forecastML")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        1. forecastML::create_lagged_df(...)
       20. data.table:::`[.data.table`(...)
       21. [ base::eval(...) ] with 1 more call
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 22 | SKIPPED: 23 | WARNINGS: 0 | FAILED: 6 ]
      1. Error: lagged_df, training data, grouped with dates is correct (@test_create_lagged_df_grouped.R#43) 
      2. Error: lagged_df, training data, grouped with dates is correct (@test_create_lagged_df_grouped_multi_output.R#46) 
      3. Error: lagged_df, training and forecasting data lookback_control skips groups and static and dynamic features (@test_create_lagged_df_lookback.R#88) 
      4. Error: lagged_df, training data lookback_control appropriately drops lagged features (@test_create_lagged_df_lookback.R#133) 
      5. Error: multi_output, lagged_df, training and forecasting data lookback_control skips groups and static and dynamic features (@test_create_lagged_df_lookback_multi_output.R#88) 
      6. Error: multi_output, lagged_df, training data lookback_control appropriately drops lagged features (@test_create_lagged_df_lookback_multi_output.R#133) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## Newly fixed

*   checking whether package ‘forecastML’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/forecastML/old/forecastML.Rcheck/00install.out’ for details.
    ```

# fuzzyjoin

<details>

* Version: 0.1.5
* Source code: https://github.com/cran/fuzzyjoin
* URL: https://github.com/dgrtwo/fuzzyjoin
* BugReports: https://github.com/dgrtwo/fuzzyjoin/issues
* Date/Publication: 2019-09-07 12:00:02 UTC
* Number of recursive dependencies: 87

Run `revdep_details(,"fuzzyjoin")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    + 
    +   interval_inner_join(x1, x2)
    + 
    +   # Allow them to be separated by a gap with a maximum:
    +   interval_inner_join(x1, x2, maxgap = 1)   # let 1 join with 2
    +   interval_inner_join(x1, x2, maxgap = 20)  # everything joins each other
    + 
    +   # Require that they overlap by more than a particular amount
    +   interval_inner_join(x1, x2, minoverlap = 3)
    + 
    +   # other types of joins:
    +   interval_full_join(x1, x2)
    +   interval_left_join(x1, x2)
    +   interval_right_join(x1, x2)
    +   interval_semi_join(x1, x2)
    +   interval_anti_join(x1, x2)
    + }
    Joining by: c("start", "end")
    Joining by: c("start", "end")
    Error: Can't column-bind data frames with different row names.
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       2. fuzzyjoin::stringdist_join(...)
       4. fuzzyjoin::fuzzy_join(x, y, by = by, mode = mode, match_fun = match_fun)
       5. dplyr::bind_cols(...)
       6. vctrs::vec_cbind(!!!dots)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 267 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 5 ]
      1. Error: Can join multiple times to the same column (@test_fuzzy_join.R#4) 
      2. Error: fuzzy_join supports formula notation (@test_fuzzy_join.R#14) 
      3. Error: Can do non-inner joins on intervals (@test_interval_join.R#40) 
      4. Error: Can do inner joins on intervals with findOverlaps arguments (@test_interval_join.R#55) 
      5. Error: stringdist_join works on one-column data.frames (@test_stringdist_join.R#281) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## Newly fixed

*   checking whether package ‘fuzzyjoin’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/fuzzyjoin/old/fuzzyjoin.Rcheck/00install.out’ for details.
    ```

# fxtract

<details>

* Version: 0.9.2
* Source code: https://github.com/cran/fxtract
* URL: https://github.com/QuayAu/fxtract
* BugReports: https://github.com/QuayAu/fxtract/issues
* Date/Publication: 2019-07-03 15:50:06 UTC
* Number of recursive dependencies: 72

Run `revdep_details(,"fxtract")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
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
      7.     └─vctrs:::stop_incompatible_type_combine(...)
      8.       └─vctrs:::stop_incompatible_type_impl(...)
      9.         └─vctrs:::stop_incompatible(...)
     10.           └─vctrs:::stop_vctrs(...)
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
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

## Newly fixed

*   checking whether package ‘fxtract’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/fxtract/old/fxtract.Rcheck/00install.out’ for details.
    ```

# gaiah

<details>

* Version: 0.0.2
* Source code: https://github.com/cran/gaiah
* Date/Publication: 2017-03-02 18:54:59
* Number of recursive dependencies: 72

Run `revdep_details(,"gaiah")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    ℹ `cnt` is `n()`.
    ℹ The error occured in group 1: Location = "100 Mile House".
    ✖ could not find function "n"
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
     13.                 ├─dplyr:::summarise.grouped_df(.data, !!!dots)
     14.                 ├─base::NextMethod()
     15.                 └─dplyr:::summarise.da
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘ggplot2’ ‘maptools’ ‘rgeos’ ‘stringr’ ‘tidyr’
      All declared Imports should be used.
    ```

## Newly fixed

*   checking whether package ‘gaiah’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/gaiah/old/gaiah.Rcheck/00install.out’ for details.
    ```

# gemini

<details>

* Version: 1.1.0
* Source code: https://github.com/cran/gemini
* BugReports: https://github.com/sellerslab/gemini/issues
* Date/Publication: 2019-10-29
* Number of recursive dependencies: 69

Run `revdep_details(,"gemini")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(testthat)
      > library(gemini)
      > 
      > test_check("gemini")
      ── 1. Failure: Input object is reproducible (@test_data.R#31)  ─────────────────
      `Input.new` not equal to `Input`.
      Component "sample.annot": Attributes: < Component "row.names": Mean relative difference: 0.5454545 >
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 6 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Failure: Input object is reproducible (@test_data.R#31) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking R code for possible problems ... NOTE
    ```
    gemini_boxplot : <anonymous>: no visible binding for global variable
      ‘.’
    gemini_boxplot: no visible binding for global variable ‘.’
    gemini_boxplot : <anonymous>: no visible binding for global variable
      ‘gi’
    gemini_boxplot : <anonymous>: no visible binding for global variable
      ‘hj’
    gemini_boxplot: no visible binding for global variable ‘label’
    gemini_boxplot: no visible binding for global variable ‘y’
    gemini_calculate_lfc: no visible binding for global variable ‘.’
    update_s_pb: no visible binding for global variable ‘.’
    update_tau_pb: no visible binding for global variable ‘.’
    Undefined global functions or variables:
      . gi hj label y
    ```

## Newly fixed

*   checking whether package ‘gemini’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/gemini/old/gemini.Rcheck/00install.out’ for details.
    ```

## In both

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .github
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

# gender

<details>

* Version: 0.5.3
* Source code: https://github.com/cran/gender
* URL: https://github.com/ropensci/gender
* BugReports: https://github.com/ropensci/gender/issues
* Date/Publication: 2019-11-09 05:30:25 UTC
* Number of recursive dependencies: 73

Run `revdep_details(,"gender")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_warnings()` to see where this warning was generated.
    Error: Column name `birth_year` must not be duplicated.
    Backtrace:
         █
      1. └─gender::gender_df(...)
      2.   └─`%>%`(...)
      3.     ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      4.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5.       └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      6.         └─gender:::`_fseq`(`_lhs`)
      7.           └─magrittr::freduce(value, `_function_list`)
      8.             └─function_list[[i]](value)
      9.               ├─dplyr::group_by_(., .dots = year_grouping)
     10.               └─dplyr:::group_by_.data.frame(., .dots = year_grouping)
     11.                 ├─dplyr::group_by(.data, !!!dots, .add = add, .drop = .drop)
     12.                 └─dplyr:::group_by.data.frame(.data, !!!dots, .add = add, .drop = .drop)
     13.                   └─dplyr::grouped_df(groups$data, groups$group_names, .drop)
     14.                     └─dplyr:::compute_groups(data, vars, drop = drop)
     15.           
    Execution halted
    ```

## Newly fixed

*   checking whether package ‘gender’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/gender/old/gender.Rcheck/00install.out’ for details.
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘genderdata’
    ```

# ggedit

<details>

* Version: 0.3.0
* Source code: https://github.com/cran/ggedit
* URL: https://github.com/metrumresearchgroup/ggedit
* BugReports: https://github.com/metrumresearchgroup/ggedit/issues
* Date/Publication: 2018-07-03 21:50:02 UTC
* Number of recursive dependencies: 79

Run `revdep_details(,"ggedit")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    ℹ `..1` is `!is.null(VAR)`.
    ✖ object 'VAR' not found
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
     16.   
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘magrittr’
      All declared Imports should be used.
    ```

## Newly fixed

*   checking whether package ‘ggedit’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/ggedit/old/ggedit.Rcheck/00install.out’ for details.
    ```

# ggRandomForests

<details>

* Version: 2.0.1
* Source code: https://github.com/cran/ggRandomForests
* URL: https://github.com/ehrlinger/ggRandomForests
* BugReports: https://github.com/ehrlinger/ggRandomForests/issues
* Date/Publication: 2016-09-07 23:21:30
* Number of recursive dependencies: 72

Run `revdep_details(,"ggRandomForests")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > plot(gg_dta)
    Error: Input must be a vector, not a `gg_rfsrc/data.frame/class` object.
    Backtrace:
         █
      1. ├─base::plot(gg_dta)
      2. ├─ggRandomForests:::plot.gg_rfsrc(gg_dta)
      3. │ ├─tidyr::gather_(gg_dta, "variable", "value", gathercols)
      4. │ └─tidyr:::gather_.data.frame(gg_dta, "variable", "value", gathercols)
      5. │   ├─tidyr::gather(...)
      6. │   └─tidyr:::gather.data.frame(...)
      7. │     ├─base::unname(tidyselect::vars_select(tbl_vars(data), !!!quos))
      8. │     ├─tidyselect::vars_select(tbl_vars(data), !!!quos)
      9. │     │ └─tidyselect:::eval_select_impl(...)
     10. │     │   └─rlang::is_null(names)
     11. │     └─dplyr::tbl_vars(data)
     12. │       ├─dplyr:::new_sel_vars(tbl_vars_dispatch(x), group_vars(x))
     13. │       │ └─base::structure(...)
     14. │       ├─dplyr::group_vars(x)
     15. │       └─dplyr:::group_vars.data.frame(x)
     16. │         ├─generics::setdiff(names(group_data(x)), ".rows"
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘randomForest’
      All declared Imports should be used.
    ```

## Newly fixed

*   checking whether package ‘ggRandomForests’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/ggRandomForests/old/ggRandomForests.Rcheck/00install.out’ for details.
    ```

# ggspectra

<details>

* Version: 0.3.5
* Source code: https://github.com/cran/ggspectra
* URL: https://www.r4photobiology.info, https://bitbucket.org/aphalo/ggspectra
* BugReports: https://bitbucket.org/aphalo/ggspectra
* Date/Publication: 2020-01-16 16:30:02 UTC
* Number of recursive dependencies: 74

Run `revdep_details(,"ggspectra")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggspectra-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: autoplot.object_spct
    > ### Title: Create a complete ggplot for a object spectrum.
    > ### Aliases: autoplot.object_spct autoplot.object_mspct
    > ### Keywords: hplot
    > 
    > ### ** Examples
    > 
    > 
    > autoplot(Ler_leaf.spct)
    Error in check_spct.generic_spct(x, multiple.wl = multiple.wl) : 
      'w.length' must be sorted and have unique values
    Calls: autoplot ... check_spct -> check_spct.object_spct -> check_spct.generic_spct
    Execution halted
    ```

## Newly fixed

*   checking whether package ‘ggspectra’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/ggspectra/old/ggspectra.Rcheck/00install.out’ for details.
    ```

# gratia

<details>

* Version: 0.3.1
* Source code: https://github.com/cran/gratia
* URL: https://gavinsimpson.github.io/gratia
* BugReports: https://github.com/gavinsimpson/gratia/issues
* Date/Publication: 2020-03-29 18:30:05 UTC
* Number of recursive dependencies: 111

Run `revdep_details(,"gratia")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      Backtrace:
        1. testthat::expect_silent(d <- derivatives(m))
       14. vctrs::stop_incompatible_size(...)
       15. vctrs:::stop_incompatible(...)
       16. vctrs:::stop_vctrs(...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 697 | SKIPPED: 74 | WARNINGS: 54 | FAILED: 4 ]
      1. Error: derivatives() returns derivatives for all smooths in a factor by GAM (@test-derivatives.R#183) 
      2. Error: derivatives() returns derivatives for all smooths in a factor by GAM (@test-derivatives.R#225) 
      3. Error: derivatives() works for factor by smooths issue 47 (@test-derivatives.R#339) 
      4. Error: derivatives() works for fs smooths issue 57 (@test-derivatives.R#389) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## Newly fixed

*   checking whether package ‘gratia’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/gratia/old/gratia.Rcheck/00install.out’ for details.
    ```

# HaDeX

<details>

* Version: 1.1
* Source code: https://github.com/cran/HaDeX
* Date/Publication: 2020-02-06 13:50:02 UTC
* Number of recursive dependencies: 127

Run `revdep_details(,"HaDeX")` for more info

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

*   checking tests ...
    ```
     ERROR
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

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘DT’ ‘gsubfn’ ‘stringr’
      All declared Imports should be used.
    ```

## Newly fixed

*   checking whether package ‘HaDeX’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/HaDeX/old/HaDeX.Rcheck/00install.out’ for details.
    ```

# holodeck

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/holodeck
* URL: https://github.com/Aariq/holodeck
* BugReports: https://github.com/Aariq/holodeck/issues
* Date/Publication: 2019-04-16 12:12:40 UTC
* Number of recursive dependencies: 102

Run `revdep_details(,"holodeck")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
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

## Newly fixed

*   checking whether package ‘holodeck’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/holodeck/old/holodeck.Rcheck/00install.out’ for details.
    ```

# IATscores

<details>

* Version: 0.2.4
* Source code: https://github.com/cran/IATscores
* Date/Publication: 2019-12-18 16:50:02 UTC
* Number of recursive dependencies: 116

Run `revdep_details(,"IATscores")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    ℹ The error occured in group 1: subject = 1.
    ✖ could not find function "n"
    Backtrace:
         █
      1. └─IATscores::IATdescriptives(IATdata)
      2.   └─`%>%`(...)
      3.     ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      4.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5.       └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      6.         └─IATscores:::`_fseq`(`_lhs`)
      7.           └─magrittr::freduce(value, `_function_list`)
      8.             ├─base::withVisible(function_list[[k]](value))
      9.             └─function_list[[k]](value)
     10.               ├─dplyr::summarize(...)
     11.               ├─dplyr:::summarise.grouped_df(...)
     12.               ├─base::NextMethod()
     13.               └─dplyr:::summarise.data.frame(...)
     14.                 └─dplyr:::summarise_cols(.data, ...)
     15.                   └─base::tryCatch(...)
     16.       
    Execution halted
    ```

## Newly fixed

*   checking whether package ‘IATscores’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/IATscores/old/IATscores.Rcheck/00install.out’ for details.
    ```

# idmodelr

<details>

* Version: 0.3.1
* Source code: https://github.com/cran/idmodelr
* URL: http://www.samabbott.co.uk/idmodelr, https://github.com/seabbs/idmodelr
* BugReports: https://github.com/seabbs/idmodelr/issues
* Date/Publication: 2019-09-10 22:50:10 UTC
* Number of recursive dependencies: 126

Run `revdep_details(,"idmodelr")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
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

## Newly fixed

*   checking whether package ‘idmodelr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/idmodelr/old/idmodelr.Rcheck/00install.out’ for details.
    ```

# IncucyteDRC

<details>

* Version: 0.5.4
* Source code: https://github.com/cran/IncucyteDRC
* URL: https://github.com/chapmandu2/IncucyteDRC
* BugReports: https://github.com/chapmandu2/IncucyteDRC/issues
* Date/Publication: 2016-04-23 14:21:03
* Number of recursive dependencies: 119

Run `revdep_details(,"IncucyteDRC")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Error: `mutate()` argument `idx` errored.
    ℹ `idx` is `row_number()`.
    ℹ The error occured in group 1: sampleid = "PDD00017273", conc = 0.3703704.
    ✖ could not find function "row_number"
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
     13.                     └─base::tryCatch(...)
     14.
    Execution halted
    ```

## Newly fixed

*   checking whether package ‘IncucyteDRC’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/IncucyteDRC/old/IncucyteDRC.Rcheck/00install.out’ for details.
    ```

# INDperform

<details>

* Version: 0.2.2
* Source code: https://github.com/cran/INDperform
* URL: https://github.com/saskiaotto/INDperform
* BugReports: https://github.com/SaskiaAOtto/INDperform/issues
* Date/Publication: 2020-01-09 12:30:14 UTC
* Number of recursive dependencies: 83

Run `revdep_details(,"INDperform")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘INDperform-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: merge_models
    > ### Title: Merging two model output tibbles.
    > ### Aliases: merge_models
    > 
    > ### ** Examples
    > 
    > # Using some models of the Baltic Sea demo data:
    > # Merging GAM and GAMM tibbles
    > test_ids <- 47:50 # choose subset
    > gam_tbl <- model_gam_ex[test_ids,]
    > gamm_tbl <- model_gamm(ind_init_ex[test_ids,], filter= gam_tbl$tac)
    Error in model_gamm(ind_init_ex[test_ids, ], filter = gam_tbl$tac) : 
      No IND~pressure GAMM could be fitted! Check if you chose the correct error distribution (default is gaussian()).
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       4. tibble:::tbl_subassign_row(xj, i, value, value_arg)
       5. base::tryCatch(...)
       6. base:::tryCatchList(expr, classes, parentenv, handlers)
       7. base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
       8. value[[3L]](cond)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 475 | SKIPPED: 0 | WARNINGS: 10 | FAILED: 4 ]
      1. Error: (unknown) (@test_calc_deriv.R#6) 
      2. Error: (unknown) (@test_cond_boot.R#112) 
      3. Error: (unknown) (@test_model_gamm.R#4) 
      4. Error: (unknown) (@test_scoring.R#15) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        data   3.3Mb
        help   1.6Mb
    ```

## Newly fixed

*   checking whether package ‘INDperform’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/INDperform/old/INDperform.Rcheck/00install.out’ for details.
    ```

# ipumsr

<details>

* Version: 0.4.2
* Source code: https://github.com/cran/ipumsr
* URL: https://www.ipums.org, https://github.com/mnpopcenter/ipumsr
* BugReports: https://github.com/mnpopcenter/ipumsr/issues
* Date/Publication: 2019-06-04 17:00:03 UTC
* Number of recursive dependencies: 107

Run `revdep_details(,"ipumsr")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Backtrace:
        1. testthat::expect_warning(bound <- ipums_bind_rows(test1, test2))
        9. vctrs::vec_default_ptype2(...)
       10. vctrs::stop_incompatible_type(...)
       11. vctrs:::stop_incompatible_type_combine(...)
       12. vctrs:::stop_incompatible_type_impl(...)
       13. vctrs:::stop_incompatible(...)
       14. vctrs:::stop_vctrs(...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 211 | SKIPPED: 11 | WARNINGS: 0 | FAILED: 1 ]
      1. Error: mismatched attributes in bind rows (@test_ipums_bind_rows.r#39) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## Newly fixed

*   checking whether package ‘ipumsr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/ipumsr/old/ipumsr.Rcheck/00install.out’ for details.
    ```

# isomiRs

<details>

* Version: 1.15.2
* Source code: https://github.com/cran/isomiRs
* BugReports: https://github.com/lpantano/isomiRs/issues
* Date/Publication: 2020-03-17
* Number of recursive dependencies: 148

Run `revdep_details(,"isomiRs")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
      tibble::lst(mean, median)
    
      # Using lambdas
      list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_warnings()` to see where this warning was generated.
    Error: `.data` must be a data frame without row names.
    Backtrace:
         █
      1. └─isomiRs::IsomirDataSeqFromFiles(fn_list, coldata = de)
      2.   └─isomiRs::IsomirDataSeqFromRawData(rawData, coldata, ...)
      3.     └─isomiRs:::IsoCountsFromMatrix(rawdata, coldata)
      4.       └─`%>%`(...)
      5.         ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      6.         └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      7.           └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      8.             └─isomiRs:::`_fseq`(`_lhs`)
      9.               └─magrittr::freduce(value, `_function_list`)
     10.                 └─function_list[[i]](value)
     11.                   └─tibble::column_to_rownames(., "uid")
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       19. base::tryCatch(...)
       20. base:::tryCatchList(expr, classes, parentenv, handlers)
       21. base:::tryCatchOne(...)
       22. value[[3L]](cond)
       23. dplyr:::stop_eval_tidy(e, index = i, dots = dots, fn = "summarise")
       24. dplyr:::stop_dplyr(index, dots, fn, "errored", x = conditionMessage(e))
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 24 | SKIPPED: 0 | WARNINGS: 2 | FAILED: 3 ]
      1. Error: counts (@test_basic.R#13) 
      2. Error: accesor (@test_basic.R#41) 
      3. Error: matrix (@test_calculus.R#23) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      ‘targetscan.Hs.egMIRNA’
    mirna2targetscan: no visible binding for global variable
      ‘targetscan.Hs.egMIRBASE2FAMILY’
    mirna2targetscan: no visible binding for global variable
      ‘targetscan.Hs.egTARGETS’
    mirna2targetscan: no visible binding for global variable
      ‘targetscan.Hs.egTARGETSFULL’
    mirna2targetscan: no visible binding for global variable
      ‘targetscan.Mm.egMIRNA’
    mirna2targetscan: no visible binding for global variable
      ‘targetscan.Mm.egMIRBASE2FAMILY’
    mirna2targetscan: no visible binding for global variable
      ‘targetscan.Mm.egTARGETS’
    mirna2targetscan: no visible binding for global variable
      ‘targetscan.Mm.egTARGETSFULL’
    Undefined global functions or variables:
      as.tibble changes hits iso_sample pct targetscan.Hs.egMIRBASE2FAMILY
      targetscan.Hs.egMIRNA targetscan.Hs.egTARGETS
      targetscan.Hs.egTARGETSFULL targetscan.Mm.egMIRBASE2FAMILY
      targetscan.Mm.egMIRNA targetscan.Mm.egTARGETS
      targetscan.Mm.egTARGETSFULL total
    ```

## Newly fixed

*   checking whether package ‘isomiRs’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/isomiRs/old/isomiRs.Rcheck/00install.out’ for details.
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘targetscan.Hs.eg.db’
    ```

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .travis.yml
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

# janitor

<details>

* Version: 2.0.1
* Source code: https://github.com/cran/janitor
* URL: https://github.com/sfirke/janitor
* BugReports: https://github.com/sfirke/janitor/issues
* Date/Publication: 2020-04-12 05:40:02 UTC
* Number of recursive dependencies: 65

Run `revdep_details(,"janitor")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Backtrace:
        1. tidygraph::play_erdos_renyi(10, 0.5)
        1. tidygraph::bind_nodes(., test_df)
        9. tidygraph::mutate_all(., tidyr::replace_na, 1)
       10. dplyr:::manip_all(...)
       14. dplyr::tbl_nongroup_vars(.tbl)
       16. dplyr::tbl_vars(x)
       19. dplyr::group_vars(x)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 583 | SKIPPED: 1 | WARNINGS: 0 | FAILED: 1 ]
      1. Error: tbl_graph/tidygraph (@test-clean-names.R#417) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## Newly fixed

*   checking whether package ‘janitor’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/janitor/old/janitor.Rcheck/00install.out’ for details.
    ```

# jstor

<details>

* Version: 0.3.8
* Source code: https://github.com/cran/jstor
* URL: https://github.com/ropensci/jstor, https://docs.ropensci.org/jstor
* BugReports: https://github.com/ropensci/jstor/issues
* Date/Publication: 2020-04-03 14:10:23 UTC
* Number of recursive dependencies: 68

Run `revdep_details(,"jstor")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 239 | SKIPPED: 4 | WARNINGS: 5 | FAILED: 14 ]
      1. Failure: journal id is unified (@test-augment.R#75) 
      2. Failure: files with column names can be re-read (@test-re-import.R#212) 
      3. Failure: files with column names can be re-read (@test-re-import.R#216) 
      4. Failure: files with column names can be re-read (@test-re-import.R#220) 
      5. Failure: files with column names can be re-read (@test-re-import.R#224) 
      6. Failure: files with column names can be re-read (@test-re-import.R#228) 
      7. Failure: files without column names can be re-read (@test-re-import.R#244) 
      8. Failure: files without column names can be re-read (@test-re-import.R#248) 
      9. Failure: files without column names can be re-read (@test-re-import.R#252) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

## Newly fixed

*   checking package dependencies ... ERROR
    ```
    Package required and available but unsuitable version: ‘tibble’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# keyholder

<details>

* Version: 0.1.4
* Source code: https://github.com/cran/keyholder
* URL: https://echasnovski.github.io/keyholder/, https://github.com/echasnovski/keyholder/
* BugReports: https://github.com/echasnovski/keyholder/issues/
* Date/Publication: 2020-03-01 20:00:02 UTC
* Number of recursive dependencies: 57

Run `revdep_details(,"keyholder")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 297 | SKIPPED: 0 | WARNINGS: 1 | FAILED: 17 ]
      1. Failure: arrange.keyed_df works (@test-keyed-df-one-tbl.R#325) 
      2. Failure: arrange.keyed_df works (@test-keyed-df-one-tbl.R#337) 
      3. Failure: arrange_all works (@test-keyed-df-one-tbl.R#354) 
      4. Failure: arrange_if works (@test-keyed-df-one-tbl.R#375) 
      5. Failure: arrange_at works (@test-keyed-df-one-tbl.R#390) 
      6. Failure: filter.keyed_df works (@test-keyed-df-one-tbl.R#408) 
      7. Failure: filter_all works (@test-keyed-df-one-tbl.R#425) 
      8. Failure: filter_if works (@test-keyed-df-one-tbl.R#448) 
      9. Failure: filter_at works (@test-keyed-df-one-tbl.R#464) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

## Newly fixed

*   checking whether package ‘keyholder’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/keyholder/old/keyholder.Rcheck/00install.out’ for details.
    ```

# lans2r

<details>

* Version: 1.0.5
* Source code: https://github.com/cran/lans2r
* URL: https://github.com/KopfLab/lans2r
* BugReports: https://github.com/KopfLab/lans2r/issues
* Date/Publication: 2017-05-24 04:25:53 UTC
* Number of recursive dependencies: 79

Run `revdep_details(,"lans2r")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      `... <- NULL` produced warnings.
      
      ── 2. Failure: test that transformation safety checks are in place (@test-transf
      spread_data(bind_rows(a, b)) not equal to full_join(...).
      Names: 2 string mismatches
      Component 3: Mean relative difference: 1.015284
      Component 4: Mean relative difference: 13.28542
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 142 | SKIPPED: 0 | WARNINGS: 7 | FAILED: 2 ]
      1. Failure: test that it is possible to load LANS maps (@test-load-data.R#81) 
      2. Failure: test that transformation safety checks are in place (@test-transformations.R#13) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## Newly fixed

*   checking whether package ‘lans2r’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/lans2r/old/lans2r.Rcheck/00install.out’ for details.
    ```

# LexisNexisTools

<details>

* Version: 0.3.0
* Source code: https://github.com/cran/LexisNexisTools
* URL: https://github.com/JBGruber/LexisNexisTools
* BugReports: https://github.com/JBGruber/LexisNexisTools/issues
* Date/Publication: 2020-01-09 23:00:03 UTC
* Number of recursive dependencies: 134

Run `revdep_details(,"LexisNexisTools")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        |++++++++++                                        | 20% ~00s          
        |+++++++++++++++                                   | 30% ~00s          
        |++++++++++++++++++++                              | 40% ~00s          
        |+++++++++++++++++++++++++                         | 50% ~00s          
        |++++++++++++++++++++++++++++++                    | 60% ~00s          
        |+++++++++++++++++++++++++++++++++++               | 70% ~00s          
        |++++++++++++++++++++++++++++++++++++++++          | 80% ~00s          
        |+++++++++++++++++++++++++++++++++++++++++++++     | 90% ~00s          
        |++++++++++++++++++++++++++++++++++++++++++++++++++| 100% elapsed=00s  
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 97 | SKIPPED: 2 | WARNINGS: 0 | FAILED: 1 ]
      1. Failure: Convert LNToutput to tidytext (@test-lnt_convert.R#102) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## Newly fixed

*   checking whether package ‘LexisNexisTools’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/LexisNexisTools/old/LexisNexisTools.Rcheck/00install.out’ for details.
    ```

# metamicrobiomeR

<details>

* Version: 1.1
* Source code: https://github.com/cran/metamicrobiomeR
* URL: https://github.com/nhanhocu/metamicrobiomeR
* BugReports: https://github.com/nhanhocu/metamicrobiomeR/issues
* Date/Publication: 2019-09-03 07:20:02 UTC
* Number of recursive dependencies: 131

Run `revdep_details(,"metamicrobiomeR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > tl<-colnames(taxtab6)[grep("k__bacteria.p__fusobacteria",colnames(taxtab6))]
    > taxacom.ex<-taxa.compare(taxtab=taxtab6[,c("personid","x.sampleid","bf","age.sample",tl)],
    + propmed.rel="gamlss",comvar="bf",adjustvar="age.sample",
    + longitudinal="yes",p.adjust.method="fdr")
    Error: Corrupt `grouped_df` using old (< 0.8.0) format
    ℹ Strip off old grouping with `ungroup()`
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

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘RCurl’ ‘foreign’ ‘gplots’ ‘httr’ ‘jsonlite’ ‘knitr’ ‘lmerTest’
      ‘magrittr’ ‘mgcv’ ‘repmis’ ‘reshape2’ ‘rmarkdown’
      All declared Imports should be used.
    ```

## Newly fixed

*   checking whether package ‘metamicrobiomeR’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/metamicrobiomeR/old/metamicrobiomeR.Rcheck/00install.out’ for details.
    ```

# microbiome

<details>

* Version: 1.9.97
* Source code: https://github.com/cran/microbiome
* URL: http://microbiome.github.io/microbiome
* BugReports: https://github.com/microbiome/microbiome/issues
* Date/Publication: 2020-04-03
* Number of recursive dependencies: 108

Run `revdep_details(,"microbiome")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── 1. Failure: Divergence (@test_divergence.R#11)  ─────────────────────────────
      divergence(reference, reference, method = "bray") not equal to 0.
      Modes: list, numeric
      
      ── 2. Failure: Divergence (@test_divergence.R#12)  ─────────────────────────────
      divergence(abundances(pseq)[, 1], reference, method = "bray") not equal to 0.3449469.
      Modes: list, numeric
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 75 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 2 ]
      1. Failure: Divergence (@test_divergence.R#11) 
      2. Failure: Divergence (@test_divergence.R#12) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## Newly fixed

*   checking whether package ‘microbiome’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/microbiome/old/microbiome.Rcheck/00install.out’ for details.
    ```

# mmetrics

<details>

* Version: 0.3.0
* Source code: https://github.com/cran/mmetrics
* URL: https://github.com/y-bar/mmetrics
* BugReports: https://github.com/y-bar/mmetrics/issues
* Date/Publication: 2019-07-26 08:50:02 UTC
* Number of recursive dependencies: 96

Run `revdep_details(,"mmetrics")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
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

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘magrittr’ ‘stringr’
      All declared Imports should be used.
    ```

## Newly fixed

*   checking whether package ‘mmetrics’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/mmetrics/old/mmetrics.Rcheck/00install.out’ for details.
    ```

# mosaic

<details>

* Version: 1.6.0
* Source code: https://github.com/cran/mosaic
* URL: https://github.com/ProjectMOSAIC/mosaic, https://projectmosaic.github.io/mosaic/
* BugReports: https://github.com/ProjectMOSAIC/mosaic/issues
* Date/Publication: 2020-03-06 18:00:03 UTC
* Number of recursive dependencies: 152

Run `revdep_details(,"mosaic")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    +     method = c("se", "perc") )
    +     
    +   # bootstrap t method requires both mean and sd
    +   bootstrap2 <- do(500) * favstats(resample(1:10)) 
    +   confint(bootstrap2, method = "boot")
    + }
    Warning: confint: Unable to compute any of the desired CIs
    Error: Can't combine `..1$name` <logical> and `..2$name` <character>.
    Backtrace:
         █
      1. ├─stats::confint(bootstrap2, method = "boot")
      2. ├─mosaic:::confint.do.data.frame(bootstrap2, method = "boot")
      3. │ └─dplyr::bind_rows(res, boott(object))
      4. │   └─vctrs::vec_rbind(!!!dots, .names_to = .id)
      5. └─vctrs::vec_default_ptype2(...)
      6.   └─vctrs::stop_incompatible_type(...)
      7.     └─vctrs:::stop_incompatible_type_combine(...)
      8.       └─vctrs:::stop_incompatible_type_impl(...)
      9.         └─vctrs:::stop_incompatible(...)
     10.           └─vctrs:::stop_vctrs(...)
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 12.1Mb
      sub-directories of 1Mb or more:
        R     2.6Mb
        doc   9.0Mb
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘cubature’
    ```

## Newly fixed

*   checking whether package ‘mosaic’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/mosaic/old/mosaic.Rcheck/00install.out’ for details.
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘fastR’
    
    Package which this enhances but not available for checking: ‘manipulate’
    ```

# mosaicData

<details>

* Version: 0.17.0
* Source code: https://github.com/cran/mosaicData
* Date/Publication: 2018-06-23 18:37:55 UTC
* Number of recursive dependencies: 90

Run `revdep_details(,"mosaicData")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Error: Corrupt `grouped_df` using old (< 0.8.0) format
    ℹ Strip off old grouping with `ungroup()`
    Backtrace:
         █
      1. └─`%>%`(...)
      2.   ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      3.   └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      4.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5.       └─`_fseq`(`_lhs`)
      6.         └─magrittr::freduce(value, `_function_list`)
      7.           └─function_list[[i]](value)
      8.             ├─dplyr::group_by(., city, year)
      9.             └─dplyr:::group_by.data.frame(., city, year)
     10.               └─dplyr::group_by_prepare(.data, ..., .add = .add)
     11.                 ├─generics::setdiff(group_names, tbl_vars(out))
     12.                 ├─generics:::setdiff.default(group_names, tbl_vars(out))
     13.                 │ └─base::setdiff(x, y, ...)
     14.                 │   └─base::as.vector(y)
     15.                 └─dplyr::tbl_vars(out)
     16.                  
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 7 marked UTF-8 strings
    ```

# MSstatsTMT

<details>

* Version: 1.5.8
* Source code: https://github.com/cran/MSstatsTMT
* URL: http://msstats.org/msstatstmt/
* BugReports: https://groups.google.com/forum/#!forum/msstats
* Date/Publication: 2020-04-26
* Number of recursive dependencies: 101

Run `revdep_details(,"MSstatsTMT")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘MSstatsTMT-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: dataProcessPlotsTMT
    > ### Title: Visualization for explanatory data analysis - TMT experiment
    > ### Aliases: dataProcessPlotsTMT
    > 
    > ### ** Examples
    > 
    > data(input.pd)
    > quant.msstats <- proteinSummarization(input.pd,
    +                                       method="msstats",
    +                                       global_norm=TRUE,
    +                                       reference_norm=TRUE)
    Joining, by = c("Run", "Channel")
    Summarizing for Run : 161117_SILAC_HeLa_UPS1_TMT10_Mixture1_01.raw ( 1  of  15 )
    Error in `[.data.table`(raw, , require.col) : 
      j (the 2nd argument inside [...]) is a single symbol but column name 'require.col' is not found. Perhaps you intended DT[, ..require.col]. This difference to data.frame is deliberate and explained in FAQ 1.1.
    Calls: proteinSummarization ... .protein.summarization.function -> dataProcess -> [ -> [.data.table
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── 2. Error: proteinSummarization works (@test-proteinSummarization.R#5)  ──────
      j (the 2nd argument inside [...]) is a single symbol but column name 'require.col' is not found. Perhaps you intended DT[, ..require.col]. This difference to data.frame is deliberate and explained in FAQ 1.1.
      Backtrace:
       1. MSstatsTMT::proteinSummarization(...)
       2. MSstatsTMT:::.protein.summarization.function(...)
       3. MSstats::dataProcess(...)
       5. data.table:::`[.data.table`(raw, , require.col)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 31 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 2 ]
      1. Failure: groupComparision works (@test-groupComparisionTMT.R#6) 
      2. Error: proteinSummarization works (@test-proteinSummarization.R#5) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## Newly fixed

*   checking whether package ‘MSstatsTMT’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/MSstatsTMT/old/MSstatsTMT.Rcheck/00install.out’ for details.
    ```

# naniar

<details>

* Version: 0.5.0
* Source code: https://github.com/cran/naniar
* URL: https://github.com/njtierney/naniar
* BugReports: https://github.com/njtierney/naniar/issues
* Date/Publication: 2020-02-28 07:20:08 UTC
* Number of recursive dependencies: 154

Run `revdep_details(,"naniar")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(naniar)
      > 
      > test_check("naniar")
      ── 1. Failure: prop_miss_case returns same as mean_ (@test-prop-cases-not-zero.R
      `bad_na_df` not equal to `expected_bad_na_df`.
      Names: 2 string mismatches
      Component 3: Mean relative difference: 1
      Component 4: Mean absolute difference: 1
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 568 | SKIPPED: 22 | WARNINGS: 50 | FAILED: 1 ]
      1. Failure: prop_miss_case returns same as mean_ (@test-prop-cases-not-zero.R#53) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## Newly fixed

*   checking whether package ‘naniar’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/naniar/old/naniar.Rcheck/00install.out’ for details.
    ```

# ncdfgeom

<details>

* Version: 1.1.0
* Source code: https://github.com/cran/ncdfgeom
* URL: https://code.usgs.gov/water/ncdfgeom
* BugReports: https://github.com/USGS-R/ncdfgeom/issues
* Date/Publication: 2019-08-28 19:20:02 UTC
* Number of recursive dependencies: 109

Run `revdep_details(,"ncdfgeom")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Linking to GEOS 3.7.2, GDAL 2.4.2, PROJ 5.2.0
      ── 1. Error: multiline data frame works (@test_line.R#45)  ─────────────────────
      no applicable method for 'st_cast' applied to an object of class "data.frame"
      Backtrace:
       1. ncdfgeom:::compareSL(lineData, returnLineData)
       2. sf::as_Spatial(lineData) revdep/checks.noindex/ncdfgeom/new/ncdfgeom.Rcheck/tests/testthat/helper-functions.R:24:8
       3. sf:::.as_Spatial(from, cast, IDs)
       4. sf::st_cast(from)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 733 | SKIPPED: 2 | WARNINGS: 34 | FAILED: 1 ]
      1. Error: multiline data frame works (@test_line.R#45) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## Newly fixed

*   checking whether package ‘ncdfgeom’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/ncdfgeom/old/ncdfgeom.Rcheck/00install.out’ for details.
    ```

# ncmeta

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/ncmeta
* URL: https://github.com/hypertidy/ncmeta
* BugReports: https://github.com/hypertidy/ncmeta/issues
* Date/Publication: 2019-10-22 17:10:02 UTC
* Number of recursive dependencies: 50

Run `revdep_details(,"ncmeta")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Attributes: < Component "class": 1 string mismatch >
      
      ── 3. Failure: nc_coord_var brings back expected content for one variable (@test
      nc_coord_var(f, "RAINNC_present") not equal to data.frame(...).
      Attributes: < Component "class": Lengths (3, 1) differ (string compare on first 1) >
      Attributes: < Component "class": 1 string mismatch >
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 114 | SKIPPED: 9 | WARNINGS: 0 | FAILED: 3 ]
      1. Failure: nc_coord_var brings back expected content for one variable (@test-coord.R#8) 
      2. Failure: nc_coord_vars brings back expected content for sample (@test-coord.R#25) 
      3. Failure: nc_coord_var brings back expected content for one variable (@test-coord.R#38) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## Newly fixed

*   checking whether package ‘ncmeta’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/ncmeta/old/ncmeta.Rcheck/00install.out’ for details.
    ```

# nonmemica

<details>

* Version: 0.9.1
* Source code: https://github.com/cran/nonmemica
* Date/Publication: 2020-03-25 17:20:02 UTC
* Number of recursive dependencies: 74

Run `revdep_details(,"nonmemica")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    6  0 83.5 2.00    2 1000   0       0      0       1 1.95170 -0.10697  0.5265000
         PRED      RES       WRES     CL     V2       KA      Q     V3      ETA1
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
    Error: Can't convert from `default` <double> to `x` <integer> due to loss of precision.
    ```

## Newly fixed

*   checking whether package ‘nonmemica’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/nonmemica/old/nonmemica.Rcheck/00install.out’ for details.
    ```

# padr

<details>

* Version: 0.5.1
* Source code: https://github.com/cran/padr
* URL: https://github.com/EdwinTh/padr
* BugReports: https://github.com/EdwinTh/padr/issues
* Date/Publication: 2020-04-03 09:50:02 UTC
* Number of recursive dependencies: 69

Run `revdep_details(,"padr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    +   count(period) %>%
    +   pad_cust(spanning)
    Error: No common size for `..1`, size 0, and `..2`, size 1248.
    Backtrace:
         █
      1. ├─`%>%`(...)
      2. │ ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      3. │ └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      4. │   └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5. │     └─`_fseq`(`_lhs`)
      6. │       └─magrittr::freduce(value, `_function_list`)
      7. │         ├─base::withVisible(function_list[[k]](value))
      8. │         └─function_list[[k]](value)
      9. │           └─padr::pad_cust(., spanning)
     10. │             └─padr:::pad_cust_group_span(spanned, group_vars_un)
     11. │               └─dplyr::bind_cols(spanned_df, group_vars_un[ind, , drop = FALSE])
     12. │                 └─vctrs::vec_cbind(!!!dots)
     13. └─vctrs::stop_incompatible_size(...)
     14.   └─vctrs:::stop_incompatible(...)
     15.     └─vctrs:::stop_vctrs(...)
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 573 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 24 ]
      1. Failure: break_above prevents large output (@test_pad.R#56) 
      2. Failure: break_above prevents large output (@test_pad.R#58) 
      3. Failure: gives correct output when start or end with datetime range (@test_pad.R#100) 
      4. Failure: gives correct output when start or end with datetime range (@test_pad.R#102) 
      5. Failure: pad_multiple pads correctly with one group var (@test_pad.R#120) 
      6. Failure: pad_multiple pads correctly with one group var (@test_pad.R#121) 
      7. Failure: pad_multiple pads correctly with one group var (@test_pad.R#122) 
      8. Failure: pad pads correctly with two group vars (@test_pad.R#130) 
      9. Failure: pad pads correctly with two group vars (@test_pad.R#131) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

## Newly fixed

*   checking whether package ‘padr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/padr/old/padr.Rcheck/00install.out’ for details.
    ```

# pammtools

<details>

* Version: 0.2.2
* Source code: https://github.com/cran/pammtools
* URL: https://github.com/adibender/pammtools
* BugReports: https://github.com/adibender/pammtools/issues
* Date/Publication: 2020-03-12 21:00:02 UTC
* Number of recursive dependencies: 101

Run `revdep_details(,"pammtools")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Error: `vars` must be a character vector
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

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 242 | SKIPPED: 0 | WARNINGS: 7 | FAILED: 13 ]
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

## Newly fixed

*   checking whether package ‘pammtools’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/pammtools/old/pammtools.Rcheck/00install.out’ for details.
    ```

# PAST

<details>

* Version: 1.3.9
* Source code: https://github.com/cran/PAST
* URL: https://github.com/IGBB/past
* BugReports: https://github.com/IGBB/past/issues
* Date/Publication: 2020-04-02
* Number of recursive dependencies: 97

Run `revdep_details(,"PAST")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    ℹ `..1` is `.data`.
    ✖ `..1` must be a vector, not a `rlang_data_pronoun` object.
    Backtrace:
         █
      1. ├─utils::example("load_GWAS_data")
      2. │ └─base::source(...)
      3. │   ├─base::withVisible(eval(ei, envir))
      4. │   └─base::eval(ei, envir)
      5. │     └─base::eval(ei, envir)
      6. └─PAST::load_GWAS_data(demo_association_file, demo_effects_file) /var/folders/4b/hn4fq98s6810s4ccv2f9hm2h0000gn/T//RtmpfeHx5t/Rex156f37d22e640:11:0
      7.   └─`%>%`(...)
      8.     ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      9.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
     10.       └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
     11.         └─PAST:::`_fseq`(`_lhs`)
     12.           └─magrittr::freduce(value, `_function_list`)
     13.             └─function_list[[i]](value)
     14.               ├─dplyr::mutate(...)
     15.               └─dplyr:::mutate.data.frame(...)
     16
    Execution halted
    ```

*   checking whether package ‘PAST’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘S4Vectors::union’ by ‘dplyr::union’ when loading ‘PAST’
      Warning: replacing previous import ‘S4Vectors::intersect’ by ‘dplyr::intersect’ when loading ‘PAST’
      Warning: replacing previous import ‘S4Vectors::setdiff’ by ‘dplyr::setdiff’ when loading ‘PAST’
      Warning: replacing previous import ‘S4Vectors::first’ by ‘dplyr::first’ when loading ‘PAST’
      Warning: replacing previous import ‘S4Vectors::setequal’ by ‘dplyr::setequal’ when loading ‘PAST’
      Warning: replacing previous import ‘S4Vectors::rename’ by ‘dplyr::rename’ when loading ‘PAST’
      Warning: replacing previous import ‘S4Vectors::tail’ by ‘utils::tail’ when loading ‘PAST’
      Warning: replacing previous import ‘S4Vectors::head’ by ‘utils::head’ when loading ‘PAST’
      Warning: replacing previous import ‘S4Vectors::complete.cases’ by ‘stats::complete.cases’ when loading ‘PAST’
      Warning: replacing previous import ‘S4Vectors::sd’ by ‘stats::sd’ when loading ‘PAST’
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/PAST/new/PAST.Rcheck/00install.out’ for details.
    ```

*   checking Rd \usage sections ... WARNING
    ```
    Undocumented arguments in documentation object 'assign_SNPs_to_genes'
      ‘filter_type’
    
    Functions with \usage entries need to have the appropriate \alias
    entries, and all their arguments documented.
    The \usage entries must correspond to syntactically valid R code.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking R code for possible problems ... NOTE
    ```
    assign_chunk: no visible binding for global variable ‘chromosome’
    assign_chunk: no visible global function definition for ‘IRanges’
    assign_chunk: no visible binding for global variable ‘position’
    assign_chunk: no visible binding for global variable ‘seqid’
    assign_chunk: no visible binding for global variable ‘Name’
    plot_pathways: no visible binding for global variable
      ‘running_enrichment_score’
    Undefined global functions or variables:
      IRanges Name chromosome position running_enrichment_score seqid
    ```

## Newly fixed

*   checking whether package ‘PAST’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/PAST/old/PAST.Rcheck/00install.out’ for details.
    ```

# photosynthesis

<details>

* Version: 1.0.0
* Source code: https://github.com/cran/photosynthesis
* Date/Publication: 2019-05-09 15:10:03 UTC
* Number of recursive dependencies: 73

Run `revdep_details(,"photosynthesis")` for more info

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

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tidyr’
      All declared Imports should be used.
    ```

## Newly fixed

*   checking whether package ‘photosynthesis’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/photosynthesis/old/photosynthesis.Rcheck/00install.out’ for details.
    ```

# pmdplyr

<details>

* Version: 0.3.1
* Source code: https://github.com/cran/pmdplyr
* URL: https://nickch-k.github.io/pmdplyr, https://github.com/NickCH-K/pmdplyr
* BugReports: https://github.com/NickCH-K/pmdplyr/issues
* Date/Publication: 2020-03-09 19:30:02 UTC
* Number of recursive dependencies: 106

Run `revdep_details(,"pmdplyr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Error: `mutate()` argument `changed` must be recyclable.
    ℹ `changed` is `<lgl>`.
    ℹ The error occured in group 1: unitid = 100654.
    ✖ `changed` can't be recycled to size 8.
    ℹ `changed` must be size 8 or 1, not 48445.
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
     13.                   └─b
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       15. dplyr:::mutate.data.frame(.data, ...)
       16. dplyr:::mutate_cols(.data, ...)
       17. base::tryCatch(...)
       18. base:::tryCatchList(expr, classes, parentenv, handlers)
       21. base:::tryCatchList(expr, names[-nh], parentenv, handlers[-nh])
       24. base:::tryCatchList(expr, names[-nh], parentenv, handlers[-nh])
       27. base:::tryCatchList(expr, names[-nh], parentenv, handlers[-nh])
       30. base:::tryCatchList(expr, names[-nh], parentenv, handlers[-nh])
       31. base:::tryCatchOne(...)
       32. value[[3L]](cond)
       33. dplyr:::stop_mutate_recycle_incompatible_size(e, index = i, dots = dots)
       34. dplyr:::stop_dplyr(...)
      
      Error: C stack usage  7971424 is too close to the limit
      Execution halted
    ```

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in documentation object 'join.Rd':
      ‘[dplyr]{join.tbl_df}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

## Newly fixed

*   checking whether package ‘pmdplyr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/pmdplyr/old/pmdplyr.Rcheck/00install.out’ for details.
    ```

# portalr

<details>

* Version: 0.3.1
* Source code: https://github.com/cran/portalr
* URL: https://weecology.github.io/portalr/, https://github.com/weecology/portalr
* BugReports: https://github.com/weecology/portalr/issues
* Date/Publication: 2020-01-16 15:00:02 UTC
* Number of recursive dependencies: 104

Run `revdep_details(,"portalr")` for more info

</details>

## Newly broken

*   R CMD check timed out
    

# prophet

<details>

* Version: 0.6
* Source code: https://github.com/cran/prophet
* URL: https://github.com/facebook/prophet
* BugReports: https://github.com/facebook/prophet/issues
* Date/Publication: 2020-03-03 09:20:03 UTC
* Number of recursive dependencies: 81

Run `revdep_details(,"prophet")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/prophet/StanHeaders/include/src/stan/model/standalone_functions_header.hpp:4:
      In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/prophet/StanHeaders/include/stan/math.hpp:4:
      In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/prophet/StanHeaders/include/stan/math/rev/mat.hpp:12:
      In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/prophet/StanHeaders/include/stan/math/prim/mat.hpp:336:
      In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/prophet/StanHeaders/include/stan/math/prim/mat/prob/poisson_log_glm_log.hpp:5:
      /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/prophet/StanHeaders/include/stan/math/prim/mat/prob/poisson_log_glm_lpmf.hpp:64:59: warning: unused typedef 'T_alpha_val' [-Wunused-local-typedef]
            typename partials_return_type<T_alpha>::type>::type T_alpha_val;
                                                                ^
      20 warnings generated.
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 346 | SKIPPED: 1 | WARNINGS: 1 | FAILED: 1 ]
      1. Failure: conditional_custom_seasonality (@test_prophet.R#630) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## Newly fixed

*   checking whether package ‘prophet’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/prophet/old/prophet.Rcheck/00install.out’ for details.
    ```

# ptstem

<details>

* Version: 0.0.4
* Source code: https://github.com/cran/ptstem
* URL: https://github.com/dfalbel/ptstem
* Date/Publication: 2019-01-02 14:40:02 UTC
* Number of recursive dependencies: 63

Run `revdep_details(,"ptstem")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      y[4]: "gostou"
      
      x[5]: "gostaram"
      y[5]: "gostou"
      
      x[6]: "gostaram"
      y[6]: "gostou"
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 17 | SKIPPED: 0 | WARNINGS: 1 | FAILED: 2 ]
      1. Failure: Stemming Hunspell Works (@test-ptstem.R#15) 
      2. Failure: Stemming Hunspell Works (@test-ptstem.R#19) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        dict   5.1Mb
    ```

## Newly fixed

*   checking whether package ‘ptstem’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/ptstem/old/ptstem.Rcheck/00install.out’ for details.
    ```

# PupillometryR

<details>

* Version: 0.0.1
* Source code: https://github.com/cran/PupillometryR
* BugReports: https://github.com/samhforbes/PupillometryR/issues
* Date/Publication: 2020-02-02 16:00:05 UTC
* Number of recursive dependencies: 64

Run `revdep_details(,"PupillometryR")` for more info

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

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘data.table’ ‘fractal’ ‘mgcv’
      All declared Imports should be used.
    ```

## Newly fixed

*   checking whether package ‘PupillometryR’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/PupillometryR/old/PupillometryR.Rcheck/00install.out’ for details.
    ```

# purrrlyr

<details>

* Version: 0.0.5
* Source code: https://github.com/cran/purrrlyr
* URL: https://github.com/hadley/purrrlyr
* BugReports: https://github.com/hadley/purrrlyr/issues
* Date/Publication: 2019-03-15 23:40:02 UTC
* Number of recursive dependencies: 47

Run `revdep_details(,"purrrlyr")` for more info

</details>

## Newly broken

*   checking whether package ‘purrrlyr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/purrrlyr/new/purrrlyr.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘purrrlyr’ ...
** package ‘purrrlyr’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/purrrlyr/Rcpp/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/purrrlyr/BH/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c fast-copy.cpp -o fast-copy.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/purrrlyr/Rcpp/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/purrrlyr/BH/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -O3 -c init.c -o init.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/purrrlyr/Rcpp/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/purrrlyr/BH/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -O3 -c map.c -o map.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/purrrlyr/Rcpp/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/purrrlyr/BH/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c rows-data.cpp -o rows-data.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/purrrlyr/Rcpp/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/purrrlyr/BH/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c rows-formatter.cpp -o rows-formatter.o
In file included from rows-formatter.cpp:2:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/purrrlyr/BH/include/boost/lexical_cast.hpp:21:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/purrrlyr/BH/include/boost/config.hpp:57:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/purrrlyr/BH/include/boost/config/platform/macos.hpp:28:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/purrrlyr/BH/include/boost/config/detail/posix_features.hpp:18:
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:654:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/gethostuuid.h:39:17: error: C++ requires a type specifier for all declarations
int gethostuuid(uuid_t, const struct timespec *) __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA);
                ^
In file included from rows-formatter.cpp:2:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/purrrlyr/BH/include/boost/lexical_cast.hpp:21:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/purrrlyr/BH/include/boost/config.hpp:57:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/purrrlyr/BH/include/boost/config/platform/macos.hpp:28:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/purrrlyr/BH/include/boost/config/detail/posix_features.hpp:18:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:661:27: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      getsgroups_np(int *, uuid_t);
                              ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from rows-formatter.cpp:2:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/purrrlyr/BH/include/boost/lexical_cast.hpp:21:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/purrrlyr/BH/include/boost/config.hpp:57:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/purrrlyr/BH/include/boost/config/platform/macos.hpp:28:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/purrrlyr/BH/include/boost/config/detail/posix_features.hpp:18:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:663:27: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      getwgroups_np(int *, uuid_t);
                              ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from rows-formatter.cpp:2:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/purrrlyr/BH/include/boost/lexical_cast.hpp:21:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/purrrlyr/BH/include/boost/config.hpp:57:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/purrrlyr/BH/include/boost/config/platform/macos.hpp:28:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/purrrlyr/BH/include/boost/config/detail/posix_features.hpp:18:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:726:31: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      setsgroups_np(int, const uuid_t);
                                  ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from rows-formatter.cpp:2:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/purrrlyr/BH/include/boost/lexical_cast.hpp:21:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/purrrlyr/BH/include/boost/config.hpp:57:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/purrrlyr/BH/include/boost/config/platform/macos.hpp:28:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/purrrlyr/BH/include/boost/config/detail/posix_features.hpp:18:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:728:31: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      setwgroups_np(int, const uuid_t);
                                  ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
5 errors generated.
make: *** [rows-formatter.o] Error 1
ERROR: compilation failed for package ‘purrrlyr’
* removing ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/purrrlyr/new/purrrlyr.Rcheck/purrrlyr’

```
### CRAN

```
* installing *source* package ‘purrrlyr’ ...
** package ‘purrrlyr’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/BH/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c fast-copy.cpp -o fast-copy.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/BH/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -O3 -c init.c -o init.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/BH/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -O3 -c map.c -o map.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/BH/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c rows-data.cpp -o rows-data.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/BH/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c rows-formatter.cpp -o rows-formatter.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/BH/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c rows.cpp -o rows.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/BH/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c utils.cpp -o utils.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/BH/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -O3 -c vector.c -o vector.o
clang++ -std=gnu++11 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o purrrlyr.so fast-copy.o init.o map.o rows-data.o rows-formatter.o rows.o utils.o vector.o -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
installing to /Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/purrrlyr/old/purrrlyr.Rcheck/00LOCK-purrrlyr/00new/purrrlyr/libs
** R
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** checking absolute paths in shared objects and dynamic libraries
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (purrrlyr)

```
# qualmap

<details>

* Version: 0.1.1
* Source code: https://github.com/cran/qualmap
* URL: https://github.com/slu-openGIS/qualmap
* BugReports: https://github.com/slu-openGIS/qualmap/issues
* Date/Publication: 2018-09-12 15:10:14 UTC
* Number of recursive dependencies: 102

Run `revdep_details(,"qualmap")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > # create clusters
    > cluster1 <- qm_define(118600, 119101, 119300)
    > cluster2 <- qm_define(119300, 121200, 121100)
    > 
    > # create cluster objects
    > cluster_obj1 <- qm_create(ref = stl, key = TRACTCE, value = cluster1,
    +     rid = 1, cid = 1, category = "positive")
    > cluster_obj2 <- qm_create(ref = stl, key = TRACTCE, value = cluster2,
    +     rid = 1, cid = 2, category = "positive")
    > 
    > # combine cluster objects
    > clusters <- qm_combine(cluster_obj1, cluster_obj2)
    Error: Input must be a vector, not a `tbl_df/tbl/data.frame/qm_cluster` object.
    Backtrace:
        █
     1. ├─qualmap::qm_combine(cluster_obj1, cluster_obj2)
     2. │ └─dplyr::bind_rows(...)
     3. │   └─vctrs::vec_rbind(!!!dots, .names_to = .id)
     4. └─vctrs:::stop_scalar_type(...)
     5.   └─vctrs:::stop_vctrs(msg, "vctrs_error_scalar_type", actual = x)
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       1. qualmap::qm_combine(cluster1_obj, cluster2_obj, cluster3_obj)
       4. vctrs:::stop_scalar_type(...)
       5. vctrs:::stop_vctrs(msg, "vctrs_error_scalar_type", actual = x)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 47 | SKIPPED: 0 | WARNINGS: 1 | FAILED: 6 ]
      1. Failure: returns TRUE - test result 1 matches test_tbl2 (@test_4_qm_create.R#114) 
      2. Failure: returns TRUE - test result 2 matches test_tbl2 (@test_4_qm_create.R#118) 
      3. Failure: returns TRUE - test result 3 matches test_tbl3 (@test_4_qm_create.R#124) 
      4. Failure: (unknown) (@test_5_qm_combine.R#73) 
      5. Error: (unknown) (@test_5_qm_combine.R#78) 
      6. Error: (unknown) (@test_6_qm_summarize.R#17) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## Newly fixed

*   checking whether package ‘qualmap’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/qualmap/old/qualmap.Rcheck/00install.out’ for details.
    ```

# Rariant

<details>

* Version: 1.22.0
* Source code: https://github.com/cran/Rariant
* URL: https://github.com/juliangehring/Rariant
* BugReports: https://support.bioconductor.org
* Date/Publication: 2019-10-29
* Number of recursive dependencies: 167

Run `revdep_details(,"Rariant")` for more info

</details>

## Newly broken

*   checking whether package ‘Rariant’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/Rariant/new/Rariant.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking examples ... ERROR
    ```
    ...
    > ### Aliases: tallyPlot
    > 
    > ### ** Examples
    > 
    >   library(ggbio)
    Loading required package: ggplot2
    Need specific help about ggbio? try mailing 
     the maintainer or visit http://tengfei.github.com/ggbio/
    
    Attaching package: 'ggbio'
    
    The following objects are masked from 'package:ggplot2':
    
        geom_bar, geom_rect, geom_segment, ggsave, stat_bin, stat_identity,
        xlim
    
    >   library(GenomicRanges)
    >   library(BSgenome.Hsapiens.UCSC.hg19)
    Error in library(BSgenome.Hsapiens.UCSC.hg19) : 
      there is no package called 'BSgenome.Hsapiens.UCSC.hg19'
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  7.7Mb
      sub-directories of 1Mb or more:
        doc       2.2Mb
        extdata   5.2Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    tallyBamRegion: no visible global function definition for 'PileupParam'
    tallyBamRegion: no visible global function definition for
      'ScanBamParam'
    tallyBamRegion: no visible global function definition for 'pileup'
    Undefined global functions or variables:
      PileupParam ScanBamParam pileup
    ```

*   checking installed files from ‘inst/doc’ ... NOTE
    ```
    The following files should probably not be installed:
      ‘rariant-inspect-ci.png’, ‘rariant-inspect-shift.png’
    
    Consider the use of a .Rinstignore file: see ‘Writing R Extensions’,
    or move the vignette sources from ‘inst/doc’ to ‘vignettes’.
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘BSgenome.Hsapiens.UCSC.hg19’
    ```

## Installation

### Devel

```
* installing *source* package ‘Rariant’ ...
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object 'rbind_all' is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘Rariant’
* removing ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/Rariant/new/Rariant.Rcheck/Rariant’

```
### CRAN

```
* installing *source* package ‘Rariant’ ...
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
* DONE (Rariant)

```
# RCMIP5

<details>

* Version: 1.2.0
* Source code: https://github.com/cran/RCMIP5
* Date/Publication: 2016-07-30 18:53:27
* Number of recursive dependencies: 61

Run `revdep_details(,"RCMIP5")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
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

## Newly fixed

*   checking whether package ‘RCMIP5’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/RCMIP5/old/RCMIP5.Rcheck/00install.out’ for details.
    ```

# rcv

<details>

* Version: 0.2.1
* Source code: https://github.com/cran/rcv
* URL: https://github.com/ds-elections/rcv
* BugReports: https://github.com/ds-elections/rcv/issues
* Date/Publication: 2017-08-11 08:11:33 UTC
* Number of recursive dependencies: 46

Run `revdep_details(,"rcv")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Error: `summarise()` argument `total` errored.
    ℹ `total` is `n()`.
    ℹ The error occured in group 1: candidate = "BEN MATRANGA".
    ✖ could not find function "n"
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
     10.               ├─dplyr:::summarise.grouped_df(., total = n())
     11.               ├─base::NextMethod()
     12.               └─dplyr:::summarise.data.frame(., total = n())
     13.                 └─dplyr:::summarise_cols(.data, ...)
     14.                   └─base::tryCatc
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        data   5.0Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 6543 marked UTF-8 strings
    ```

## Newly fixed

*   checking whether package ‘rcv’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/rcv/old/rcv.Rcheck/00install.out’ for details.
    ```

# recipes

<details>

* Version: 0.1.10
* Source code: https://github.com/cran/recipes
* URL: https://github.com/tidymodels/recipes, https://tidymodels.github.io/recipes/
* BugReports: https://github.com/tidymodels/recipes/issues
* Date/Publication: 2020-03-18 14:50:09 UTC
* Number of recursive dependencies: 116

Run `revdep_details(,"recipes")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 1409 | SKIPPED: 8 | WARNINGS: 2 | FAILED: 33 ]
      1. Failure: correct means and std devs for step_norm (@test_center_scale_norm.R#135) 
      2. Failure: correct means and std devs for step_norm (@test_center_scale_norm.R#145) 
      3. Failure: check_col works in the bake stage (@test_colcheck.R#22) 
      4. Failure: check_col works in the bake stage (@test_colcheck.R#25) 
      5. Failure: printing and tidys (@test_discretized.R#84) 
      6. Failure: correct ICA values (@test_ica.R#96) 
      7. Failure: correct ICA values (@test_ica.R#111) 
      8. Failure: add appropriate column with default settings (@test_intercept.R#22) 
      9. Failure: adds arbitrary numeric column (@test_intercept.R#34) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

## Newly fixed

*   checking whether package ‘recipes’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/recipes/old/recipes.Rcheck/00install.out’ for details.
    ```

# rFIA

<details>

* Version: 0.2.2
* Source code: https://github.com/cran/rFIA
* Date/Publication: 2020-04-27 14:00:02 UTC
* Number of recursive dependencies: 81

Run `revdep_details(,"rFIA")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Running examples in ‘rFIA-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: diversity
    > ### Title: Estimate diversity from FIADB
    > ### Aliases: diversity
    > 
    > ### ** Examples
    > 
    > ## Load data from rFIA package
    > data(fiaRI)
    > data(countiesRI)
    > 
    > ## Make a most recent subset
    > fiaRI_mr <- clipFIA(fiaRI)
    > 
    > ## Most recent estimates for live stems on forest land
    > diversity(db = fiaRI_mr,
    +           landType = 'forest',
    +           treeType = 'live')
    Error: distinct() must use existing variables
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.7Mb
      sub-directories of 1Mb or more:
        R      2.9Mb
        data   1.9Mb
    ```

## Newly fixed

*   checking whether package ‘rFIA’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/rFIA/old/rFIA.Rcheck/00install.out’ for details.
    ```

# RNeXML

<details>

* Version: 2.4.3
* Source code: https://github.com/cran/RNeXML
* URL: https://docs.ropensci.org/RNeXML, https://github.com/ropensci/RNeXML
* BugReports: https://github.com/ropensci/RNeXML/issues
* Date/Publication: 2020-03-01 05:50:02 UTC
* Number of recursive dependencies: 135

Run `revdep_details(,"RNeXML")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
       26. base::lapply(.x, .f, ...)
       27. dplyr:::FUN(X[[i]], ...)
       28. tibble::tibble(!!!.x)
       29. tibble:::tibble_quos(xs[!is_null], .rows, .name_repair)
       30. tibble:::vectbl_recycle_rows(res, first_size, j, given_col_names[[j]])
      
      Done simulation(s).
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 281 | SKIPPED: 42 | WARNINGS: 2 | FAILED: 3 ]
      1. Error: we can correctly parse nested ResourceMeta annotations (@test_meta_extract.R#128) 
      2. Error: metadata tables can be requested in simplified form (@test_meta_extract.R#155) 
      3. Error: ID assignments are correct and complete when meta are nested (@test_meta_extract.R#168) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## Newly fixed

*   checking whether package ‘RNeXML’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/RNeXML/old/RNeXML.Rcheck/00install.out’ for details.
    ```

# RSQL

<details>

* Version: 0.1.1
* Source code: https://github.com/cran/RSQL
* URL: https://github.com/rOpenStats/rsql
* BugReports: https://github.com/rOpenStats/rsql/issues
* Date/Publication: 2020-02-05 16:10:11 UTC
* Number of recursive dependencies: 64

Run `revdep_details(,"RSQL")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Attributes: < Component "class": 1 string mismatch >
      Attributes: < Component 2: Modes: list, numeric >
      Attributes: < Component 2: names for target but not for current >
      Attributes: < Component 2: Attributes: < Modes: list, NULL > >
      Attributes: < Component 2: Attributes: < names for target but not for current > >
      Attributes: < Component 2: Attributes: < current is not list-like > >
      ...
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 15 | SKIPPED: 0 | WARNINGS: 6 | FAILED: 1 ]
      1. Failure: sql_lib select, insert and delete with dataframe (@test_sql_lib.R#97) 
      
      Error: testthat unit tests failed
      Execution halted
      Finalizing object and disconnecting
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘RSQLite’
      All declared Imports should be used.
    ```

## Newly fixed

*   checking whether package ‘RSQL’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/RSQL/old/RSQL.Rcheck/00install.out’ for details.
    ```

# RTD

<details>

* Version: 0.1.1
* Source code: https://github.com/cran/RTD
* Date/Publication: 2019-01-02 13:50:04 UTC
* Number of recursive dependencies: 54

Run `revdep_details(,"RTD")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > 
      > test_check("RTD")
      ── 1. Error: list_tables works with mock (@test-table.R#18)  ───────────────────
      `new` must be a tibble
      Backtrace:
       1. testthat::expect_equal(dplyr::count(tables)$n, 2)
       4. dplyr::count(tables)
       5. dplyr::dplyr_reconstruct(out, x)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 26 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Error: list_tables works with mock (@test-table.R#18) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘openssl’
      All declared Imports should be used.
    ```

## Newly fixed

*   checking whether package ‘RTD’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/RTD/old/RTD.Rcheck/00install.out’ for details.
    ```

# ruler

<details>

* Version: 0.2.2
* Source code: https://github.com/cran/ruler
* URL: https://echasnovski.github.io/ruler/, https://github.com/echasnovski/ruler
* BugReports: https://github.com/echasnovski/ruler/issues
* Date/Publication: 2020-03-02 21:20:03 UTC
* Number of recursive dependencies: 59

Run `revdep_details(,"ruler")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Actual value: "Tidy data validation report:\\n# A tibble: 20 x 5\\n   pack         rule      var      id value\\n \* <chr>        <chr>     <chr> <int> <lgl>\\n 1 data_pack__1 nrow_low  \.all      0 TRUE \\n 2 data_pack__1 nrow_high \.all      0 FALSE\\n 3 data_pack__1 nrow_low  \.all      0 TRUE \\n 4 data_pack__1 nrow_high \.all      0 FALSE\\n 5 data_pack__1 nrow_low  \.all      0 TRUE \\n 6 data_pack__1 nrow_high \.all      0 FALSE\\n 7 data_pack__1 nrow_low  \.all      0 TRUE \\n 8 data_pack__1 nrow_high \.all      0 FALSE\\n 9 data_pack__1 nrow_low  \.all      0 TRUE \\n10 data_pack__1 nrow_high \.all      0 FALSE\\n11 data_pack__1 nrow_low  \.all      0 TRUE \\n# … with 9 more rows"
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 297 | SKIPPED: 1 | WARNINGS: 1 | FAILED: 8 ]
      1. Failure: act_after_exposure works (@test-actions.R#66) 
      2. Failure: bind_exposures removes names from list-column `fun` (@test-expose-helpers.R#111) 
      3. Failure: bind_exposures removes names from list-column `fun` (@test-expose-helpers.R#117) 
      4. Failure: print.exposure passes tibble options (@test-exposure.R#365) 
      5. Failure: print.exposure passes tibble options (@test-exposure.R#380) 
      6. Failure: print.exposure passes tibble options (@test-exposure.R#395) 
      7. Failure: print.packs_info handles extra arguments (@test-exposure.R#433) 
      8. Failure: print.ruler_report handles extra arguments (@test-exposure.R#467) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## Newly fixed

*   checking whether package ‘ruler’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/ruler/old/ruler.Rcheck/00install.out’ for details.
    ```

# sampler

<details>

* Version: 0.2.4
* Source code: https://github.com/cran/sampler
* URL: https://github.com/mbaldassaro/sampler
* BugReports: https://github.com/mbaldassaro/sampler/issues
* Date/Publication: 2019-09-15 15:40:02 UTC
* Number of recursive dependencies: 26

Run `revdep_details(,"sampler")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Error: `new` must be a tibble
    Backtrace:
         █
      1. ├─sampler::ssamp(albania, 890, qarku)
      2. │ └─sampler::ssampcalc(df, n, !!strata)
      3. │   └─`%>%`(...)
      4. │     ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      5. │     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      6. │       └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      7. │         └─sampler:::`_fseq`(`_lhs`)
      8. │           └─magrittr::freduce(value, `_function_list`)
      9. │             ├─base::withVisible(function_list[[k]](value))
     10. │             └─function_list[[k]](value)
     11. │               ├─dplyr::summarise(...)
     12. │               ├─dplyr:::summarise.grouped_df(...)
     13. │               ├─base::NextMethod()
     14. │               └─dplyr:::summarise.data.frame(...)
     15. │                 └─dplyr:::summarise_cols(.data, ...)
     16. │                   ├─base::tryCatch(...)
     17. │                   │
    Execution halted
    ```

## Newly fixed

*   checking whether package ‘sampler’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/sampler/old/sampler.Rcheck/00install.out’ for details.
    ```

# saotd

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/saotd
* BugReports: https://github.com/evan-l-munson/saotd/issues
* Date/Publication: 2019-04-04 16:30:03 UTC
* Number of recursive dependencies: 118

Run `revdep_details(,"saotd")` for more info

</details>

## Newly broken

*   R CMD check timed out
    

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 826 marked UTF-8 strings
    ```

## Newly fixed

*   checking whether package ‘saotd’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/saotd/old/saotd.Rcheck/00install.out’ for details.
    ```

# scFeatureFilter

<details>

* Version: 1.7.5
* Source code: https://github.com/cran/scFeatureFilter
* Date/Publication: 2020-04-07
* Number of recursive dependencies: 110

Run `revdep_details(,"scFeatureFilter")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  6.6Mb
      sub-directories of 1Mb or more:
        data   3.7Mb
        doc    2.6Mb
    ```

## Newly fixed

*   checking whether package ‘scFeatureFilter’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/scFeatureFilter/old/scFeatureFilter.Rcheck/00install.out’ for details.
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘scRNAseq’
    ```

# seplyr

<details>

* Version: 0.8.5
* Source code: https://github.com/cran/seplyr
* URL: https://github.com/WinVector/seplyr/, https://winvector.github.io/seplyr/
* BugReports: https://github.com/WinVector/seplyr/issues
* Date/Publication: 2020-01-17 10:30:07 UTC
* Number of recursive dependencies: 51

Run `revdep_details(,"seplyr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Error: `mutate()` argument `rank` errored.
    ℹ `rank` is `row_number()`.
    ℹ The error occured in group 1: cyl = 4, gear = 3.
    ✖ could not find function "row_number"
    Backtrace:
         █
      1. ├─`%.>%`(...)
      2. │ └─wrapr::pipe_impl(...)
      3. │   └─base::eval(pipe_left_arg, envir = pipe_environment, enclos = pipe_environment)
      4. │     └─base::eval(pipe_left_arg, envir = pipe_environment, enclos = pipe_environment)
      5. └─seplyr::group_mutate(...)
      6.   ├─dplyr::mutate(dg, ...)
      7.   └─dplyr:::mutate.data.frame(dg, ...)
      8.     └─dplyr:::mutate_cols(.data, ...)
      9.       └─base::tryCatch(...)
     10.         └─base:::tryCatchList(expr, classes, parentenv, handlers)
     11.           └─base:::tryCatchOne(...)
     12.             └─value[[3L]](cond)
     13.               └─dplyr:::stop_eval_tidy(e, index = i, dots = dots, fn = "mutate")
     14.                 └─dplyr:::stop_dplyr(index, dots, fn, "errored", x = conditionMessage(e))
    Execution halted
    ```

## Newly fixed

*   checking whether package ‘seplyr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/seplyr/old/seplyr.Rcheck/00install.out’ for details.
    ```

# sevenbridges

<details>

* Version: 1.17.1
* Source code: https://github.com/cran/sevenbridges
* URL: https://www.sevenbridges.com, https://sbg.github.io/sevenbridges-r/, https://github.com/sbg/sevenbridges-r
* BugReports: https://github.com/sbg/sevenbridges-r/issues
* Date/Publication: 2020-03-26
* Number of recursive dependencies: 63

Run `revdep_details(,"sevenbridges")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > f1 <- convert_app(f1)
    > # input matrix
    > f1$input_matrix()
    Error: Can't combine `..1$category` <character> and `..4$category` <scalar>.
    Backtrace:
         █
      1. ├─f1$input_matrix()
      2. │ ├─base::suppressWarnings(as(inputs, "data.frame"))
      3. │ │ └─base::withCallingHandlers(...)
      4. │ └─methods::as(inputs, "data.frame")
      5. │   └─sevenbridges:::asMethod(object)
      6. │     ├─base::do.call("bind_rows", lst)
      7. │     └─dplyr::bind_rows(...)
      8. │       └─vctrs::vec_rbind(!!!dots, .names_to = .id)
      9. └─vctrs::vec_default_ptype2(...)
     10.   └─vctrs::stop_incompatible_type(...)
     11.     └─vctrs:::stop_incompatible_type_combine(...)
     12.       └─vctrs:::stop_incompatible_type_impl(...)
     13.         └─vctrs:::stop_incompatible(...)
     14.           └─vctrs:::stop_vctrs(...)
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 12.3Mb
      sub-directories of 1Mb or more:
        R     2.1Mb
        doc   9.5Mb
    ```

## Newly fixed

*   checking whether package ‘sevenbridges’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/sevenbridges/old/sevenbridges.Rcheck/00install.out’ for details.
    ```

# sf

<details>

* Version: 0.8-1
* Source code: https://github.com/cran/sf
* URL: https://github.com/r-spatial/sf/, https://r-spatial.github.io/sf/
* BugReports: https://github.com/r-spatial/sf/issues/
* Date/Publication: 2020-01-28 11:20:07 UTC
* Number of recursive dependencies: 145

Run `revdep_details(,"sf")` for more info

</details>

## Newly broken

*   checking whether package ‘sf’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/sf/new/sf.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking installed package size ... NOTE
    ```
      installed size is 16.9Mb
      sub-directories of 1Mb or more:
        doc     12.0Mb
        sqlite   1.5Mb
    ```

## Installation

### Devel

```
* installing *source* package ‘sf’ ...
** package ‘sf’ successfully unpacked and MD5 sums checked
** using staged installation
configure: CC: clang
configure: CXX: clang++ -std=gnu++11
checking for gdal-config... /usr/local/bin/gdal-config
checking gdal-config usability... yes
configure: GDAL: 2.4.4
checking GDAL version >= 2.0.1... yes
checking for gcc... clang
checking whether the C compiler works... yes
checking for C compiler default output file name... a.out
checking for suffix of executables... 
checking whether we are cross compiling... no
checking for suffix of object files... o
checking whether we are using the GNU C compiler... yes
checking whether clang accepts -g... yes
checking for clang option to accept ISO C89... none needed
checking how to run the C preprocessor... clang -E
checking for grep that handles long lines and -e... /usr/bin/grep
checking for egrep... /usr/bin/grep -E
checking for ANSI C header files... yes
checking for sys/types.h... yes
checking for sys/stat.h... yes
checking for stdlib.h... yes
checking for string.h... yes
checking for memory.h... yes
checking for strings.h... yes
checking for inttypes.h... yes
checking for stdint.h... yes
checking for unistd.h... yes
checking gdal.h usability... yes
checking gdal.h presence... yes
checking for gdal.h... yes
checking GDAL: linking with --libs only... yes
checking GDAL: /usr/local/Cellar/gdal/2.4.4/share/gdal/pcs.csv readable... yes
checking GDAL: checking whether PROJ is available for linking:... yes
checking GDAL: checking whether PROJ is available fur running:... yes
configure: pkg-config proj exists, will use it
configure: using proj.h.
configure: PROJ: 6.3.1
checking PROJ: checking whether PROJ and sqlite3 are available for linking:... yes
checking for geos-config... /usr/local/bin/geos-config
checking geos-config usability... yes
configure: GEOS: 3.8.0
checking GEOS version >= 3.4.0... yes
checking geos_c.h usability... yes
checking geos_c.h presence... yes
checking for geos_c.h... yes
checking geos: linking with -L/usr/local/Cellar/geos/3.8.0/lib -lgeos_c... yes
configure: Package CPP flags:  -I/usr/local/Cellar/proj/6.3.1/include -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.4/include -I/usr/local/Cellar/geos/3.8.0/include
configure: Package LIBS: -L/usr/local/Cellar/proj/6.3.1/lib -lproj   -L/usr/local/Cellar/gdal/2.4.4/lib -lgdal -L/usr/local/Cellar/geos/3.8.0/lib -lgeos_c
configure: creating ./config.status
config.status: creating src/Makevars
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/usr/local/Cellar/proj/6.3.1/include -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.4/include -I/usr/local/Cellar/geos/3.8.0/include -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/sf/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c RcppExports.cpp -o RcppExports.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/usr/local/Cellar/proj/6.3.1/include -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.4/include -I/usr/local/Cellar/geos/3.8.0/include -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/sf/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c bbox.cpp -o bbox.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/usr/local/Cellar/proj/6.3.1/include -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.4/include -I/usr/local/Cellar/geos/3.8.0/include -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/sf/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c gdal.cpp -o gdal.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/usr/local/Cellar/proj/6.3.1/include -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.4/include -I/usr/local/Cellar/geos/3.8.0/include -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/sf/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c gdal_geom.cpp -o gdal_geom.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/usr/local/Cellar/proj/6.3.1/include -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.4/include -I/usr/local/Cellar/geos/3.8.0/include -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/sf/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c gdal_read.cpp -o gdal_read.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/usr/local/Cellar/proj/6.3.1/include -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.4/include -I/usr/local/Cellar/geos/3.8.0/include -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/sf/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c gdal_utils.cpp -o gdal_utils.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/usr/local/Cellar/proj/6.3.1/include -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.4/include -I/usr/local/Cellar/geos/3.8.0/include -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/sf/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c gdal_write.cpp -o gdal_write.o
In file included from gdal_write.cpp:5:
In file included from /usr/local/Cellar/gdal/2.4.4/include/ogrsf_frmts.h:35:
In file included from /usr/local/Cellar/gdal/2.4.4/include/ogr_feature.h:35:
In file included from /usr/local/Cellar/gdal/2.4.4/include/ogr_featurestyle.h:33:
In file included from /usr/local/Cellar/gdal/2.4.4/include/cpl_conv.h:36:
In file included from /usr/local/Cellar/gdal/2.4.4/include/cpl_vsi.h:62:
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:654:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/gethostuuid.h:39:17: error: C++ requires a type specifier for all declarations
int gethostuuid(uuid_t, const struct timespec *) __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA);
                ^
In file included from gdal_write.cpp:5:
In file included from /usr/local/Cellar/gdal/2.4.4/include/ogrsf_frmts.h:35:
In file included from /usr/local/Cellar/gdal/2.4.4/include/ogr_feature.h:35:
In file included from /usr/local/Cellar/gdal/2.4.4/include/ogr_featurestyle.h:33:
In file included from /usr/local/Cellar/gdal/2.4.4/include/cpl_conv.h:36:
In file included from /usr/local/Cellar/gdal/2.4.4/include/cpl_vsi.h:62:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:661:27: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      getsgroups_np(int *, uuid_t);
                              ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from gdal_write.cpp:5:
In file included from /usr/local/Cellar/gdal/2.4.4/include/ogrsf_frmts.h:35:
In file included from /usr/local/Cellar/gdal/2.4.4/include/ogr_feature.h:35:
In file included from /usr/local/Cellar/gdal/2.4.4/include/ogr_featurestyle.h:33:
In file included from /usr/local/Cellar/gdal/2.4.4/include/cpl_conv.h:36:
In file included from /usr/local/Cellar/gdal/2.4.4/include/cpl_vsi.h:62:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:663:27: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      getwgroups_np(int *, uuid_t);
                              ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from gdal_write.cpp:5:
In file included from /usr/local/Cellar/gdal/2.4.4/include/ogrsf_frmts.h:35:
In file included from /usr/local/Cellar/gdal/2.4.4/include/ogr_feature.h:35:
In file included from /usr/local/Cellar/gdal/2.4.4/include/ogr_featurestyle.h:33:
In file included from /usr/local/Cellar/gdal/2.4.4/include/cpl_conv.h:36:
In file included from /usr/local/Cellar/gdal/2.4.4/include/cpl_vsi.h:62:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:726:31: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      setsgroups_np(int, const uuid_t);
                                  ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from gdal_write.cpp:5:
In file included from /usr/local/Cellar/gdal/2.4.4/include/ogrsf_frmts.h:35:
In file included from /usr/local/Cellar/gdal/2.4.4/include/ogr_feature.h:35:
In file included from /usr/local/Cellar/gdal/2.4.4/include/ogr_featurestyle.h:33:
In file included from /usr/local/Cellar/gdal/2.4.4/include/cpl_conv.h:36:
In file included from /usr/local/Cellar/gdal/2.4.4/include/cpl_vsi.h:62:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:728:31: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      setwgroups_np(int, const uuid_t);
                                  ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
5 errors generated.
make: *** [gdal_write.o] Error 1
ERROR: compilation failed for package ‘sf’
* removing ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/sf/new/sf.Rcheck/sf’

```
### CRAN

```
* installing *source* package ‘sf’ ...
** package ‘sf’ successfully unpacked and MD5 sums checked
** using staged installation
configure: CC: clang
configure: CXX: clang++ -std=gnu++11
checking for gdal-config... /usr/local/bin/gdal-config
checking gdal-config usability... yes
configure: GDAL: 2.4.4
checking GDAL version >= 2.0.1... yes
checking for gcc... clang
checking whether the C compiler works... yes
checking for C compiler default output file name... a.out
checking for suffix of executables... 
checking whether we are cross compiling... no
checking for suffix of object files... o
checking whether we are using the GNU C compiler... yes
checking whether clang accepts -g... yes
checking for clang option to accept ISO C89... none needed
checking how to run the C preprocessor... clang -E
checking for grep that handles long lines and -e... /usr/bin/grep
checking for egrep... /usr/bin/grep -E
checking for ANSI C header files... yes
checking for sys/types.h... yes
checking for sys/stat.h... yes
checking for stdlib.h... yes
checking for string.h... yes
checking for memory.h... yes
checking for strings.h... yes
checking for inttypes.h... yes
checking for stdint.h... yes
checking for unistd.h... yes
checking gdal.h usability... yes
checking gdal.h presence... yes
checking for gdal.h... yes
checking GDAL: linking with --libs only... yes
checking GDAL: /usr/local/Cellar/gdal/2.4.4/share/gdal/pcs.csv readable... yes
checking GDAL: checking whether PROJ is available for linking:... yes
checking GDAL: checking whether PROJ is available fur running:... yes
configure: pkg-config proj exists, will use it
configure: using proj.h.
configure: PROJ: 6.3.1
checking PROJ: checking whether PROJ and sqlite3 are available for linking:... yes
checking for geos-config... /usr/local/bin/geos-config
checking geos-config usability... yes
configure: GEOS: 3.8.0
checking GEOS version >= 3.4.0... yes
checking geos_c.h usability... yes
checking geos_c.h presence... yes
checking for geos_c.h... yes
checking geos: linking with -L/usr/local/Cellar/geos/3.8.0/lib -lgeos_c... yes
configure: Package CPP flags:  -I/usr/local/Cellar/proj/6.3.1/include -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.4/include -I/usr/local/Cellar/geos/3.8.0/include
configure: Package LIBS: -L/usr/local/Cellar/proj/6.3.1/lib -lproj   -L/usr/local/Cellar/gdal/2.4.4/lib -lgdal -L/usr/local/Cellar/geos/3.8.0/lib -lgeos_c
configure: creating ./config.status
config.status: creating src/Makevars
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/usr/local/Cellar/proj/6.3.1/include -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.4/include -I/usr/local/Cellar/geos/3.8.0/include -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c RcppExports.cpp -o RcppExports.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/usr/local/Cellar/proj/6.3.1/include -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.4/include -I/usr/local/Cellar/geos/3.8.0/include -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c bbox.cpp -o bbox.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/usr/local/Cellar/proj/6.3.1/include -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.4/include -I/usr/local/Cellar/geos/3.8.0/include -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c gdal.cpp -o gdal.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/usr/local/Cellar/proj/6.3.1/include -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.4/include -I/usr/local/Cellar/geos/3.8.0/include -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c gdal_geom.cpp -o gdal_geom.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/usr/local/Cellar/proj/6.3.1/include -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.4/include -I/usr/local/Cellar/geos/3.8.0/include -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c gdal_read.cpp -o gdal_read.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/usr/local/Cellar/proj/6.3.1/include -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.4/include -I/usr/local/Cellar/geos/3.8.0/include -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c gdal_utils.cpp -o gdal_utils.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/usr/local/Cellar/proj/6.3.1/include -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.4/include -I/usr/local/Cellar/geos/3.8.0/include -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c gdal_write.cpp -o gdal_write.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/usr/local/Cellar/proj/6.3.1/include -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.4/include -I/usr/local/Cellar/geos/3.8.0/include -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c geos.cpp -o geos.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/usr/local/Cellar/proj/6.3.1/include -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.4/include -I/usr/local/Cellar/geos/3.8.0/include -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c hex.cpp -o hex.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/usr/local/Cellar/proj/6.3.1/include -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.4/include -I/usr/local/Cellar/geos/3.8.0/include -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c ops.cpp -o ops.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/usr/local/Cellar/proj/6.3.1/include -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.4/include -I/usr/local/Cellar/geos/3.8.0/include -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c polygonize.cpp -o polygonize.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/usr/local/Cellar/proj/6.3.1/include -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.4/include -I/usr/local/Cellar/geos/3.8.0/include -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c proj.cpp -o proj.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/usr/local/Cellar/proj/6.3.1/include -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.4/include -I/usr/local/Cellar/geos/3.8.0/include -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c proj_info.cpp -o proj_info.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/usr/local/Cellar/proj/6.3.1/include -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.4/include -I/usr/local/Cellar/geos/3.8.0/include -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c raster2sf.cpp -o raster2sf.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/usr/local/Cellar/proj/6.3.1/include -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.4/include -I/usr/local/Cellar/geos/3.8.0/include -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c sfg.cpp -o sfg.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/usr/local/Cellar/proj/6.3.1/include -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.4/include -I/usr/local/Cellar/geos/3.8.0/include -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c signed_area.cpp -o signed_area.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/usr/local/Cellar/proj/6.3.1/include -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.4/include -I/usr/local/Cellar/geos/3.8.0/include -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c stars.cpp -o stars.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/usr/local/Cellar/proj/6.3.1/include -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.4/include -I/usr/local/Cellar/geos/3.8.0/include -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c wkb.cpp -o wkb.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/usr/local/Cellar/proj/6.3.1/include -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.4/include -I/usr/local/Cellar/geos/3.8.0/include -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c zm_range.cpp -o zm_range.o
clang++ -std=gnu++11 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o sf.so RcppExports.o bbox.o gdal.o gdal_geom.o gdal_read.o gdal_utils.o gdal_write.o geos.o hex.o ops.o polygonize.o proj.o proj_info.o raster2sf.o sfg.o signed_area.o stars.o wkb.o zm_range.o -L/usr/local/Cellar/proj/6.3.1/lib -lproj -L/usr/local/Cellar/gdal/2.4.4/lib -lgdal -L/usr/local/Cellar/geos/3.8.0/lib -lgeos_c -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
installing to /Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/sf/old/sf.Rcheck/00LOCK-sf/00new/sf/libs
** R
** demo
** inst
** byte-compile and prepare package for lazy loading
in method for ‘dbWriteTable’ with signature ‘"PostgreSQLConnection","character","sf"’: no definition for class “PostgreSQLConnection”
in method for ‘dbDataType’ with signature ‘"PostgreSQLConnection","sf"’: no definition for class “PostgreSQLConnection”
in method for ‘coerce’ with signature ‘"Spatial","sf"’: no definition for class “Spatial”
in method for ‘coerce’ with signature ‘"Spatial","sfc"’: no definition for class “Spatial”
in method for ‘coerce’ with signature ‘"sf","Spatial"’: no definition for class “Spatial”
in method for ‘coerce’ with signature ‘"sfc","Spatial"’: no definition for class “Spatial”
in method for ‘coerce’ with signature ‘"XY","Spatial"’: no definition for class “Spatial”
** help
*** installing help indices
*** copying figures
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** checking absolute paths in shared objects and dynamic libraries
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (sf)

```
# silicate

<details>

* Version: 0.4.0
* Source code: https://github.com/cran/silicate
* URL: https://github.com/hypertidy/silicate
* BugReports: https://github.com/hypertidy/silicate/issues
* Date/Publication: 2020-04-15 17:20:02 UTC
* Number of recursive dependencies: 131

Run `revdep_details(,"silicate")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### ** Examples
    > 
    > a <- ARC(minimal_mesh)
    Warning: `group_indices_()` is deprecated as of dplyr 0.7.0.
    Please use `group_indices()` instead.
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_warnings()` to see where this warning was generated.
    Warning: The `...` argument of `group_keys()` is deprecated as of dplyr 1.0.0.
    Please `group_by()` first
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_warnings()` to see where this warning was generated.
    Warning: `distinct_()` is deprecated as of dplyr 0.7.0.
    Please use `distinct()` instead.
    See vignette('programming') for more help
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_warnings()` to see where this warning was generated.
    Warning: `select_()` is deprecated as of dplyr 0.7.0.
    Please use `select()` instead.
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_warnings()` to see where this warning was generated.
    Error: Tibble columns must have compatible sizes.
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       15. base::lapply(.x, .f, ...)
       16. dplyr:::FUN(X[[i]], ...)
       17. tibble::tibble(!!!.x)
       18. tibble:::tibble_quos(xs[!is_null], .rows, .name_repair)
       19. tibble:::vectbl_recycle_rows(res, first_size, j, given_col_names[[j]])
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 105 | SKIPPED: 7 | WARNINGS: 6 | FAILED: 4 ]
      1. Error: ARC for non polygons is a warnable offence (@test-arc.R#5) 
      2. Failure: generic forms are understood (@test-generic-data.R#14) 
      3. Error: print works (@test-print.R#5) 
      4. Error: object and path names as expected (@test-sf-decomposition.R#36) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## Newly fixed

*   checking whether package ‘silicate’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/silicate/old/silicate.Rcheck/00install.out’ for details.
    ```

# simglm

<details>

* Version: 0.7.4
* Source code: https://github.com/cran/simglm
* Date/Publication: 2019-05-31 17:10:03 UTC
* Number of recursive dependencies: 90

Run `revdep_details(,"simglm")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── 1. Failure: interupt TS (@test_knots.r#69)  ─────────────────────────────────
      table(temp_long$time_k) not equal to table(temp_long$time >= 3).
      Numeric: lengths (2, 0) differ
      
      ── 2. Failure: interupt TS (@test_knots.r#96)  ─────────────────────────────────
      table(temp_long$time_k) not equal to table(temp_long$time >= 3).
      Numeric: lengths (2, 0) differ
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 129 | SKIPPED: 0 | WARNINGS: 1 | FAILED: 2 ]
      1. Failure: interupt TS (@test_knots.r#69) 
      2. Failure: interupt TS (@test_knots.r#96) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## Newly fixed

*   checking whether package ‘simglm’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/simglm/old/simglm.Rcheck/00install.out’ for details.
    ```

# simTool

<details>

* Version: 1.1.5
* Source code: https://github.com/cran/simTool
* URL: https://github.com/MarselScheer/simTool
* BugReports: https://github.com/MarselScheer/simTool/issues
* Date/Publication: 2020-03-15 20:10:02 UTC
* Number of recursive dependencies: 70

Run `revdep_details(,"simTool")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Component "simulation": Attributes: < Component 2: Modes: numeric, list >
      Component "simulation": Attributes: < Component 2: Lengths: 4, 2 >
      Component "simulation": Attributes: < Component 2: names for current but not for target >
      Component "simulation": Attributes: < Component 2: target is numeric, current is list >
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 95 | SKIPPED: 0 | WARNINGS: 1 | FAILED: 5 ]
      1. Failure: Simplify the simulation results (@test_eval_tibbles.R#381) 
      2. Failure: Two groups for summary_fun. Results were created and stored in simulation (@test_eval_tibbles.R#824) 
      3. Failure: Create a tibble containing the results for every replication (@test_frame_simulation.R#33) 
      4. Failure: Create a tibble containing the results sumamrized by one summary function (@test_frame_simulation.R#73) 
      5. Failure: Simple unnesting (@test_unnest_simulation.R#20) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## Newly fixed

*   checking whether package ‘simTool’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/simTool/old/simTool.Rcheck/00install.out’ for details.
    ```

# SIRItoGTFS

<details>

* Version: 0.2.4
* Source code: https://github.com/cran/SIRItoGTFS
* URL: https://github.com/bogind/SIRItoGTFS, http://user47094.vs.easily.co.uk/siri/documentation.htm, https://developers.google.com/transit/gtfs/
* BugReports: https://github.com/bogind/SIRItoGTFS/issues
* Date/Publication: 2018-05-21 18:36:10 UTC
* Number of recursive dependencies: 32

Run `revdep_details(,"SIRItoGTFS")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > SIRIsample$Longitude = as.numeric(SIRIsample$Longitude)
    > SIRIsample$Latitude = as.numeric(SIRIsample$Latitude)
    > # load your own GTFS data with `readGTFS()`
    > # or use the subset of GTFS data conformable to the SIRI sample, also included in the package
    > data("GTFSstops")
    > data("GTFSstop_times")
    > data("GTFScalendar")
    > data("GTFStrips")
    > data("GTFSagency")
    > data("GTFSroutes")
    > busesDF = STG(SIRIsample,
    +              GTFSstops. = GTFSstops,
    +              GTFSagency. = GTFSagency,
    +              GTFScalendar. = GTFScalendar,
    +              GTFSroutes. = GTFSroutes,
    +              GTFSstop_times. = GTFSstop_times,
    +              GTFStrips. = GTFStrips,
    +              linerefs = unique(SIRIsample$LineRef[1]))
    [1] "Strating"
    Error: $ operator is invalid for atomic vectors
    Execution halted
    ```

## Newly fixed

*   checking whether package ‘SIRItoGTFS’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/SIRItoGTFS/old/SIRItoGTFS.Rcheck/00install.out’ for details.
    ```

# skynet

<details>

* Version: 1.3.0
* Source code: https://github.com/cran/skynet
* URL: https://github.com/FilipeamTeixeira/skynet
* BugReports: https://github.com/FilipeamTeixeira/skynet/issues
* Date/Publication: 2018-12-12 10:20:03 UTC
* Number of recursive dependencies: 84

Run `revdep_details(,"skynet")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
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

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘maps’
      All declared Imports should be used.
    ```

## Newly fixed

*   checking whether package ‘skynet’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/skynet/old/skynet.Rcheck/00install.out’ for details.
    ```

# slider

<details>

* Version: 0.1.1
* Source code: https://github.com/cran/slider
* URL: https://github.com/DavisVaughan/slider
* BugReports: https://github.com/DavisVaughan/slider/issues
* Date/Publication: 2020-02-23 17:10:02 UTC
* Number of recursive dependencies: 61

Run `revdep_details(,"slider")` for more info

</details>

## Newly broken

*   checking whether package ‘slider’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/slider/new/slider.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘slider’ ...
** package ‘slider’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/new/vctrs/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -O3 -c block.c -o block.o
block.c:23:22: warning: implicit declaration of function 'vec_chop' is invalid in C99 [-Wimplicit-function-declaration]
  SEXP out = PROTECT(vec_chop(x, indices));
                     ^
block.c:23:22: warning: incompatible integer to pointer conversion passing 'int' to parameter of type 'SEXP' (aka 'struct SEXPREC *') [-Wint-conversion]
  SEXP out = PROTECT(vec_chop(x, indices));
                     ^~~~~~~~~~~~~~~~~~~~
/Library/Frameworks/R.framework/Resources/include/Rinternals.h:852:31: note: expanded from macro 'PROTECT'
#define PROTECT(s)      Rf_protect(s)
                                   ^
/Library/Frameworks/R.framework/Resources/include/Rinternals.h:1618:21: note: passing argument to parameter here
SEXP Rf_protect(SEXP);
                    ^
2 warnings generated.
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/new/vctrs/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -O3 -c compare.c -o compare.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/new/vctrs/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -O3 -c hop.c -o hop.o
hop.c:42:17: warning: implicit declaration of function 'vec_proxy' is invalid in C99 [-Wimplicit-function-declaration]
  out = PROTECT(vec_proxy(out));
                ^
hop.c:42:17: warning: incompatible integer to pointer conversion passing 'int' to parameter of type 'SEXP' (aka 'struct SEXPREC *') [-Wint-conversion]
  out = PROTECT(vec_proxy(out));
                ^~~~~~~~~~~~~~
/Library/Frameworks/R.framework/Resources/include/Rinternals.h:852:31: note: expanded from macro 'PROTECT'
#define PROTECT(s)      Rf_protect(s)
                                   ^
/Library/Frameworks/R.framework/Resources/include/Rinternals.h:1618:21: note: passing argument to parameter here
SEXP Rf_protect(SEXP);
                    ^
hop.c:95:11: warning: incompatible integer to pointer conversion assigning to 'SEXP' (aka 'struct SEXPREC *') from 'int' [-Wint-conversion]
      elt = vec_proxy(elt);
          ^ ~~~~~~~~~~~~~~
hop.c:104:7: warning: implicit declaration of function 'vec_assign_impl' is invalid in C99 [-Wimplicit-function-declaration]
      vec_assign_impl(out, index, elt, false);
      ^
hop.c:110:17: warning: implicit declaration of function 'vec_restore' is invalid in C99 [-Wimplicit-function-declaration]
  out = PROTECT(vec_restore(out, ptype, r_int(size)));
                ^
hop.c:110:17: warning: incompatible integer to pointer conversion passing 'int' to parameter of type 'SEXP' (aka 'struct SEXPREC *') [-Wint-conversion]
  out = PROTECT(vec_restore(out, ptype, r_int(size)));
        ~~~~~~~~^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/Library/Frameworks/R.framework/Resources/include/Rinternals.h:852:31: note: expanded from macro 'PROTECT'
#define PROTECT(s)      Rf_protect(s)
                                   ^
/Library/Frameworks/R.framework/Resources/include/Rinternals.h:1618:21: note: passing argument to parameter here
SEXP Rf_protect(SEXP);
                    ^
6 warnings generated.
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/new/vctrs/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -O3 -c index.c -o index.o
index.c:74:19: warning: implicit declaration of function 'vec_proxy' is invalid in C99 [-Wimplicit-function-declaration]
  out = PROTECT_N(vec_proxy(out), &n_prot);
                  ^
index.c:74:19: warning: incompatible integer to pointer conversion passing 'int' to parameter of type 'SEXP' (aka 'struct SEXPREC *') [-Wint-conversion]
  out = PROTECT_N(vec_proxy(out), &n_prot);
                  ^~~~~~~~~~~~~~
./utils.h:6:40: note: expanded from macro 'PROTECT_N'
#define PROTECT_N(x, n) (++*n, PROTECT(x))
                                       ^
/Library/Frameworks/R.framework/Resources/include/Rinternals.h:852:31: note: expanded from macro 'PROTECT'
#define PROTECT(s)      Rf_protect(s)
                                   ^
/Library/Frameworks/R.framework/Resources/include/Rinternals.h:1618:21: note: passing argument to parameter here
SEXP Rf_protect(SEXP);
                    ^
index.c:98:21: warning: incompatible integer to pointer conversion passing 'int' to parameter of type 'SEXP' (aka 'struct SEXPREC *') [-Wint-conversion]
      elt = PROTECT(vec_proxy(elt));
                    ^~~~~~~~~~~~~~
/Library/Frameworks/R.framework/Resources/include/Rinternals.h:852:31: note: expanded from macro 'PROTECT'
#define PROTECT(s)      Rf_protect(s)
                                   ^
/Library/Frameworks/R.framework/Resources/include/Rinternals.h:1618:21: note: passing argument to parameter here
SEXP Rf_protect(SEXP);
                    ^
index.c:112:7: warning: implicit declaration of function 'vec_assign_impl' is invalid in C99 [-Wimplicit-function-declaration]
      vec_assign_impl(out, out_index, elt, false);
      ^
index.c:125:19: warning: implicit declaration of function 'vec_restore' is invalid in C99 [-Wimplicit-function-declaration]
  out = PROTECT_N(vec_restore(out, ptype, size_), &n_prot);
                  ^
index.c:125:19: warning: incompatible integer to pointer conversion passing 'int' to parameter of type 'SEXP' (aka 'struct SEXPREC *') [-Wint-conversion]
  out = PROTECT_N(vec_restore(out, ptype, size_), &n_prot);
                  ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
./utils.h:6:40: note: expanded from macro 'PROTECT_N'
#define PROTECT_N(x, n) (++*n, PROTECT(x))
                                       ^
/Library/Frameworks/R.framework/Resources/include/Rinternals.h:852:31: note: expanded from macro 'PROTECT'
#define PROTECT(s)      Rf_protect(s)
                                   ^
/Library/Frameworks/R.framework/Resources/include/Rinternals.h:1618:21: note: passing argument to parameter here
SEXP Rf_protect(SEXP);
                    ^
index.c:172:19: warning: implicit declaration of function 'vec_proxy' is invalid in C99 [-Wimplicit-function-declaration]
  out = PROTECT_N(vec_proxy(out), &n_prot);
                  ^
index.c:172:19: warning: incompatible integer to pointer conversion passing 'int' to parameter of type 'SEXP' (aka 'struct SEXPREC *') [-Wint-conversion]
  out = PROTECT_N(vec_proxy(out), &n_prot);
                  ^~~~~~~~~~~~~~
./utils.h:6:40: note: expanded from macro 'PROTECT_N'
#define PROTECT_N(x, n) (++*n, PROTECT(x))
                                       ^
/Library/Frameworks/R.framework/Resources/include/Rinternals.h:852:31: note: expanded from macro 'PROTECT'
#define PROTECT(s)      Rf_protect(s)
                                   ^
/Library/Frameworks/R.framework/Resources/include/Rinternals.h:1618:21: note: passing argument to parameter here
SEXP Rf_protect(SEXP);
                    ^
index.c:202:21: warning: incompatible integer to pointer conversion passing 'int' to parameter of type 'SEXP' (aka 'struct SEXPREC *') [-Wint-conversion]
      elt = PROTECT(vec_proxy(elt));
                    ^~~~~~~~~~~~~~
/Library/Frameworks/R.framework/Resources/include/Rinternals.h:852:31: note: expanded from macro 'PROTECT'
#define PROTECT(s)      Rf_protect(s)
                                   ^
/Library/Frameworks/R.framework/Resources/include/Rinternals.h:1618:21: note: passing argument to parameter here
SEXP Rf_protect(SEXP);
                    ^
index.c:212:7: warning: implicit declaration of function 'vec_assign_impl' is invalid in C99 [-Wimplicit-function-declaration]
      vec_assign_impl(out, out_index, elt, false);
      ^
index.c:221:19: warning: implicit declaration of function 'vec_restore' is invalid in C99 [-Wimplicit-function-declaration]
  out = PROTECT_N(vec_restore(out, ptype, size_), &n_prot);
                  ^
index.c:221:19: warning: incompatible integer to pointer conversion passing 'int' to parameter of type 'SEXP' (aka 'struct SEXPREC *') [-Wint-conversion]
  out = PROTECT_N(vec_restore(out, ptype, size_), &n_prot);
                  ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
./utils.h:6:40: note: expanded from macro 'PROTECT_N'
#define PROTECT_N(x, n) (++*n, PROTECT(x))
                                       ^
/Library/Frameworks/R.framework/Resources/include/Rinternals.h:852:31: note: expanded from macro 'PROTECT'
#define PROTECT(s)      Rf_protect(s)
                                   ^
/Library/Frameworks/R.framework/Resources/include/Rinternals.h:1618:21: note: passing argument to parameter here
SEXP Rf_protect(SEXP);
                    ^
12 warnings generated.
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/new/vctrs/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -O3 -c init.c -o init.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/new/vctrs/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -O3 -c names.c -o names.o
names.c:5:10: warning: implicit declaration of function 'vec_set_names' is invalid in C99 [-Wimplicit-function-declaration]
  return vec_set_names(x, names);
         ^
names.c:5:10: warning: incompatible integer to pointer conversion returning 'int' from a function with result type 'SEXP' (aka 'struct SEXPREC *') [-Wint-conversion]
  return vec_set_names(x, names);
         ^~~~~~~~~~~~~~~~~~~~~~~
names.c:10:10: warning: implicit declaration of function 'vec_names' is invalid in C99 [-Wimplicit-function-declaration]
  return vec_names(x);
         ^
names.c:10:10: warning: incompatible integer to pointer conversion returning 'int' from a function with result type 'SEXP' (aka 'struct SEXPREC *') [-Wint-conversion]
  return vec_names(x);
         ^~~~~~~~~~~~
4 warnings generated.
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/new/vctrs/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -O3 -c params.c -o params.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/new/vctrs/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -O3 -c slide-period.c -o slide-period.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/new/vctrs/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -O3 -c slide.c -o slide.o
slide.c:97:9: warning: implicit declaration of function 'vec_proxy' is invalid in C99 [-Wimplicit-function-declaration]
  out = vec_proxy(out);
        ^
slide.c:97:7: warning: incompatible integer to pointer conversion assigning to 'SEXP' (aka 'struct SEXPREC *') from 'int' [-Wint-conversion]
  out = vec_proxy(out);
      ^ ~~~~~~~~~~~~~~
slide.c:144:11: warning: incompatible integer to pointer conversion assigning to 'SEXP' (aka 'struct SEXPREC *') from 'int' [-Wint-conversion]
      elt = vec_proxy(elt);
          ^ ~~~~~~~~~~~~~~
slide.c:153:7: warning: implicit declaration of function 'vec_assign_impl' is invalid in C99 [-Wimplicit-function-declaration]
      vec_assign_impl(out, index, elt, false);
      ^
slide.c:159:9: warning: implicit declaration of function 'vec_restore' is invalid in C99 [-Wimplicit-function-declaration]
  out = vec_restore(out, ptype, r_int(size));
        ^
slide.c:159:7: warning: incompatible integer to pointer conversion assigning to 'SEXP' (aka 'struct SEXPREC *') from 'int' [-Wint-conversion]
  out = vec_restore(out, ptype, r_int(size));
      ^ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
6 warnings generated.
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/new/vctrs/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -O3 -c slider-vctrs.c -o slider-vctrs.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/new/vctrs/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -O3 -c utils.c -o utils.o
utils.c:106:21: warning: implicit declaration of function 'vec_names' is invalid in C99 [-Wimplicit-function-declaration]
    names = PROTECT(vec_names(x));
                    ^
utils.c:106:21: warning: incompatible integer to pointer conversion passing 'int' to parameter of type 'SEXP' (aka 'struct SEXPREC *') [-Wint-conversion]
    names = PROTECT(vec_names(x));
                    ^~~~~~~~~~~~
/Library/Frameworks/R.framework/Resources/include/Rinternals.h:852:31: note: expanded from macro 'PROTECT'
#define PROTECT(s)      Rf_protect(s)
                                   ^
/Library/Frameworks/R.framework/Resources/include/Rinternals.h:1618:21: note: passing argument to parameter here
SEXP Rf_protect(SEXP);
                    ^
utils.c:110:21: warning: implicit declaration of function 'vec_names' is invalid in C99 [-Wimplicit-function-declaration]
    names = PROTECT(vec_names(r_lst_get(x, 0)));
                    ^
utils.c:110:21: warning: incompatible integer to pointer conversion passing 'int' to parameter of type 'SEXP' (aka 'struct SEXPREC *') [-Wint-conversion]
    names = PROTECT(vec_names(r_lst_get(x, 0)));
                    ^~~~~~~~~~~~~~~~~~~~~~~~~~
/Library/Frameworks/R.framework/Resources/include/Rinternals.h:852:31: note: expanded from macro 'PROTECT'
#define PROTECT(s)      Rf_protect(s)
                                   ^
/Library/Frameworks/R.framework/Resources/include/Rinternals.h:1618:21: note: passing argument to parameter here
SEXP Rf_protect(SEXP);
                    ^
utils.c:114:10: warning: implicit declaration of function 'vec_set_names' is invalid in C99 [-Wimplicit-function-declaration]
  return vec_set_names(out, names);
         ^
utils.c:114:10: warning: incompatible integer to pointer conversion returning 'int' from a function with result type 'SEXP' (aka 'struct SEXPREC *') [-Wint-conversion]
  return vec_set_names(out, names);
         ^~~~~~~~~~~~~~~~~~~~~~~~~
utils.c:153:17: warning: implicit declaration of function 'vec_slice_impl' is invalid in C99 [-Wimplicit-function-declaration]
    container = vec_slice_impl(x, window);
                ^
utils.c:153:15: warning: incompatible integer to pointer conversion assigning to 'SEXP' (aka 'struct SEXPREC *') from 'int' [-Wint-conversion]
    container = vec_slice_impl(x, window);
              ^ ~~~~~~~~~~~~~~~~~~~~~~~~~
utils.c:160:17: warning: implicit declaration of function 'vec_slice_impl' is invalid in C99 [-Wimplicit-function-declaration]
    container = vec_slice_impl(VECTOR_ELT(x, 0), window);
                ^
utils.c:160:15: warning: incompatible integer to pointer conversion assigning to 'SEXP' (aka 'struct SEXPREC *') from 'int' [-Wint-conversion]
    container = vec_slice_impl(VECTOR_ELT(x, 0), window);
              ^ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
utils.c:162:15: warning: incompatible integer to pointer conversion assigning to 'SEXP' (aka 'struct SEXPREC *') from 'int' [-Wint-conversion]
    container = vec_slice_impl(VECTOR_ELT(x, 1), window);
              ^ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
utils.c:171:13: warning: implicit declaration of function 'vec_slice_impl' is invalid in C99 [-Wimplicit-function-declaration]
    slice = vec_slice_impl(VECTOR_ELT(x, i), window);
            ^
utils.c:171:11: warning: incompatible integer to pointer conversion assigning to 'SEXP' (aka 'struct SEXPREC *') from 'int' [-Wint-conversion]
    slice = vec_slice_impl(VECTOR_ELT(x, i), window);
          ^ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
13 warnings generated.
clang -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o slider.so block.o compare.o hop.o index.o init.o names.o params.o slide-period.o slide.o slider-vctrs.o utils.o -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
installing to /Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/slider/new/slider.Rcheck/00LOCK-slider/00new/slider/libs
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
Error: package or namespace load failed for ‘slider’ in dyn.load(file, DLLpath = DLLpath, ...):
 unable to load shared object '/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/slider/new/slider.Rcheck/00LOCK-slider/00new/slider/libs/slider.so':
  dlopen(/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/slider/new/slider.Rcheck/00LOCK-slider/00new/slider/libs/slider.so, 6): Symbol not found: _vec_assign_impl
  Referenced from: /Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/slider/new/slider.Rcheck/00LOCK-slider/00new/slider/libs/slider.so
  Expected in: flat namespace
 in /Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/slider/new/slider.Rcheck/00LOCK-slider/00new/slider/libs/slider.so
Error: loading failed
Execution halted
ERROR: loading failed
* removing ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/slider/new/slider.Rcheck/slider’

```
### CRAN

```
* installing *source* package ‘slider’ ...
** package ‘slider’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/vctrs/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -O3 -c block.c -o block.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/vctrs/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -O3 -c compare.c -o compare.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/vctrs/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -O3 -c hop.c -o hop.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/vctrs/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -O3 -c index.c -o index.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/vctrs/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -O3 -c init.c -o init.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/vctrs/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -O3 -c names.c -o names.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/vctrs/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -O3 -c params.c -o params.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/vctrs/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -O3 -c slide-period.c -o slide-period.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/vctrs/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -O3 -c slide.c -o slide.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/vctrs/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -O3 -c slider-vctrs.c -o slider-vctrs.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/vctrs/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -O3 -c utils.c -o utils.o
clang -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o slider.so block.o compare.o hop.o index.o init.o names.o params.o slide-period.o slide.o slider-vctrs.o utils.o -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
installing to /Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/slider/old/slider.Rcheck/00LOCK-slider/00new/slider/libs
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** checking absolute paths in shared objects and dynamic libraries
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (slider)

```
# srvyr

<details>

* Version: 0.3.8
* Source code: https://github.com/cran/srvyr
* URL: http://gdfe.co/srvyr, https://github.com/gergness/srvyr
* BugReports: https://github.com/gergness/srvyr/issues
* Date/Publication: 2020-03-07 19:30:02 UTC
* Number of recursive dependencies: 88

Run `revdep_details(,"srvyr")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 226 | SKIPPED: 0 | WARNINGS: 37 | FAILED: 9 ]
      1. Error: DB backed survey tests - RSQLite (@test_database.R#70) 
      2. Failure: ungrouped proportion works correctly 
      3. Failure: median/ratio with CIs respect level parameter (grouped) 
      4. Failure: survey_var works for ungrouped surveys - with se (@test_survey_statistics.r#359) 
      5. Failure: survey_var works for ungrouped surveys - with ci (@test_survey_statistics.r#374) 
      6. Failure: survey_var works for ungrouped surveys - with vartype=NULL (@test_survey_statistics.r#388) 
      7. Failure: survey_var works for grouped surveys - with se (@test_survey_statistics.r#403) 
      8. Failure: survey_var works for grouped surveys - with ci (@test_survey_statistics.r#419) 
      9. Failure: survey_var works for grouped surveys - with vartype=NULL (@test_survey_statistics.r#433) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking S3 generic/method consistency ... WARNING
    ```
    pull:
      function(.data, var, name)
    pull.tbl_svy:
      function(.data, var)
    
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

## Newly fixed

*   checking whether package ‘srvyr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/srvyr/old/srvyr.Rcheck/00install.out’ for details.
    ```

# stacomiR

<details>

* Version: 0.5.4.3
* Source code: https://github.com/cran/stacomiR
* URL: http://stacomir.r-forge.r-project.org/
* BugReports: https://github.com/MarionLegrandLogrami/stacomiR/issues
* Date/Publication: 2020-03-18 15:20:13 UTC
* Number of recursive dependencies: 104

Run `revdep_details(,"stacomiR")` for more info

</details>

## Newly broken

*   checking whether the package can be loaded ... ERROR
    ```
    ...
    Error in dyn.load(file, DLLpath = DLLpath, ...) : 
      unable to load shared object '/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/stacomiR/cairoDevice/libs/cairoDevice.so':
      dlopen(/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/stacomiR/cairoDevice/libs/cairoDevice.so, 6): Library not loaded: /Library/Frameworks/GTK+.framework/Versions/2.24.X11/Resources/lib/libgtk-x11-2.0.0.dylib
      Referenced from: /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/stacomiR/cairoDevice/libs/cairoDevice.so
      Reason: image not found
    In addition: Warning message:
    Failed to load RGtk2 dynamic library, attempting to install it. 
    Please install GTK+ from http://r.research.att.com/libs/GTK_2.18.5-X11.pkg
    If the package still does not load, please ensure that GTK+ is installed and that it is on your PATH environment variable
    IN ANY CASE, RESTART R BEFORE TRYING TO LOAD THE PACKAGE AGAIN
    Warning message:
    In fun(libname, pkgname) :
      Failed to load cairoDevice, attempting to install itError in dyn.load(file, DLLpath = DLLpath, ...) : 
      unable to load shared object '/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/stacomiR/cairoDevice/libs/cairoDevice.so':
      dlopen(/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/stacomiR/cairoDevice/libs/cairoDevice.so, 6): Library not loaded: /Library/Frameworks/GTK+.framework/Versions/2.24.X11/Resources/lib/libgtk-x11-2.0.0.dylib
      Referenced from: /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/stacomiR/cairoDevice/libs/cairoDevice.so
      Reason: image not found
    
    
    It looks like this package has a loading problem: see the messages for
    details.
    ```

*   checking examples ... ERROR
    ```
    ...
    Error: `summarise()` argument `N` errored.
    ℹ `N` is `n()`.
    ℹ The error occured in group 1: ouv = "Anguillère Cléry-sur-Somme", annee = "2010", mois = "09", stage = "FIII".
    ✖ could not find function "n"
    Backtrace:
         █
      1. ├─base::plot(r_silver, plot.type = 2)
      2. └─stacomiR::plot(r_silver, plot.type = 2)
      3.   └─stacomiR:::.local(x, ...)
      4.     ├─dplyr::summarize(datdc1, N = n())
      5.     ├─dplyr:::summarise.grouped_df(datdc1, N = n())
      6.     ├─base::NextMethod()
      7.     └─dplyr:::summarise.data.frame(datdc1, N = n())
      8.       └─dplyr:::summarise_cols(.data, ...)
      9.         └─base::tryCatch(...)
     10.           └─base:::tryCatchList(expr, classes, parentenv, handlers)
     11.             └─base:::tryCatchOne(...)
     12.               └─value[[3L]](cond)
     13.                 └─dplyr:::stop_eval_tidy(e, index = i, dots = dots, fn = "summarise")
     14.                   └─dplyr:::stop_dplyr(index, dots, fn, "errored"
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       1. base::plot(r_dc, plot.type = "1", silent = TRUE)
       2. stacomiR::plot(r_dc, plot.type = "1", silent = TRUE)
       3. stacomiR:::.local(x, y, ...)
       4. stacomiR::progress_bar(...)
       5. RGtk2::gtkDialog(...)
       6. RGtk2::gtkDialogNewWithButtons(title, parent, flags, ..., show = show)
       7. RGtk2::.RGtkCall(...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 9 | SKIPPED: 67 | WARNINGS: 0 | FAILED: 2 ]
      1. Error: report_df plot method works (@test-03-report_df.R#59) 
      2. Error: report_dc plot method works (@test-04-report_dc.R#59) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking whether package ‘stacomiR’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: Failed to load RGtk2 dynamic library, attempting to install it.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/stacomiR/new/stacomiR.Rcheck/00install.out’ for details.
    ```

*   checking whether the package can be unloaded cleanly ... WARNING
    ```
    ---- unloading
    ```

*   checking whether the namespace can be loaded with stated dependencies ... WARNING
    ```
    Error in dyn.load(file, DLLpath = DLLpath, ...) : 
      unable to load shared object '/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/stacomiR/RGtk2/libs/RGtk2.so':
      dlopen(/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/stacomiR/RGtk2/libs/RGtk2.so, 6): Library not loaded: /Library/Frameworks/GTK+.framework/Versions/2.24.X11/Resources/lib/libgtk-x11-2.0.0.dylib
      Referenced from: /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/stacomiR/RGtk2/libs/RGtk2.so
      Reason: image not found
    Error in dyn.load(file, DLLpath = DLLpath, ...) : 
      unable to load shared object '/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/stacomiR/cairoDevice/libs/cairoDevice.so':
      dlopen(/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/stacomiR/cairoDevice/libs/cairoDevice.so, 6): Library not loaded: /Library/Frameworks/GTK+.framework/Versions/2.24.X11/Resources/lib/libgtk-x11-2.0.0.dylib
      Referenced from: /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/stacomiR/cairoDevice/libs/cairoDevice.so
      Reason: image not found
    
    A namespace must be able to be loaded with just the base namespace
    loaded: otherwise if the namespace gets loaded by a saved object, the
    session will be unable to start.
    
    Probably some imports need to be declared in the NAMESPACE file.
    ```

*   checking whether the namespace can be unloaded cleanly ... WARNING
    ```
    ---- unloading
    ```

*   checking loading without being on the library search path ... WARNING
    ```
    ...
    Error in dyn.load(file, DLLpath = DLLpath, ...) : 
      unable to load shared object '/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/stacomiR/cairoDevice/libs/cairoDevice.so':
      dlopen(/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/stacomiR/cairoDevice/libs/cairoDevice.so, 6): Library not loaded: /Library/Frameworks/GTK+.framework/Versions/2.24.X11/Resources/lib/libgtk-x11-2.0.0.dylib
      Referenced from: /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/stacomiR/cairoDevice/libs/cairoDevice.so
      Reason: image not found
    In addition: Warning message:
    Failed to load RGtk2 dynamic library, attempting to install it. 
    Please install GTK+ from http://r.research.att.com/libs/GTK_2.18.5-X11.pkg
    If the package still does not load, please ensure that GTK+ is installed and that it is on your PATH environment variable
    IN ANY CASE, RESTART R BEFORE TRYING TO LOAD THE PACKAGE AGAIN
    Warning message:
    In fun(libname, pkgname) :
      Failed to load cairoDevice, attempting to install itError in dyn.load(file, DLLpath = DLLpath, ...) : 
      unable to load shared object '/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/stacomiR/cairoDevice/libs/cairoDevice.so':
      dlopen(/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/stacomiR/cairoDevice/libs/cairoDevice.so, 6): Library not loaded: /Library/Frameworks/GTK+.framework/Versions/2.24.X11/Resources/lib/libgtk-x11-2.0.0.dylib
      Referenced from: /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/stacomiR/cairoDevice/libs/cairoDevice.so
      Reason: image not found
    
    
    It looks like this package has a loading problem when not on .libPaths:
    see the messages for details.
    ```

*   checking R code for possible problems ... NOTE
    ```
    Error in dyn.load(file, DLLpath = DLLpath, ...) : 
      unable to load shared object '/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/stacomiR/RGtk2/libs/RGtk2.so':
      dlopen(/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/stacomiR/RGtk2/libs/RGtk2.so, 6): Library not loaded: /Library/Frameworks/GTK+.framework/Versions/2.24.X11/Resources/lib/libgtk-x11-2.0.0.dylib
      Referenced from: /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/stacomiR/RGtk2/libs/RGtk2.so
      Reason: image not found
    Error in dyn.load(file, DLLpath = DLLpath, ...) : 
      unable to load shared object '/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/stacomiR/cairoDevice/libs/cairoDevice.so':
      dlopen(/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/stacomiR/cairoDevice/libs/cairoDevice.so, 6): Library not loaded: /Library/Frameworks/GTK+.framework/Versions/2.24.X11/Resources/lib/libgtk-x11-2.0.0.dylib
      Referenced from: /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/stacomiR/cairoDevice/libs/cairoDevice.so
      Reason: image not found
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Error in dyn.load(file, DLLpath = DLLpath, ...) : 
        unable to load shared object '/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/stacomiR/RGtk2/libs/RGtk2.so':
        dlopen(/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/stacomiR/RGtk2/libs/RGtk2.so, 6): Library not loaded: /Library/Frameworks/GTK+.framework/Versions/2.24.X11/Resources/lib/libgtk-x11-2.0.0.dylib
        Referenced from: /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/stacomiR/RGtk2/libs/RGtk2.so
        Reason: image not found
      Error in dyn.load(file, DLLpath = DLLpath, ...) : 
        unable to load shared object '/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/stacomiR/cairoDevice/libs/cairoDevice.so':
        dlopen(/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/stacomiR/cairoDevice/libs/cairoDevice.so, 6): Library not loaded: /Library/Frameworks/GTK+.framework/Versions/2.24.X11/Resources/lib/libgtk-x11-2.0.0.dylib
        Referenced from: /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/stacomiR/cairoDevice/libs/cairoDevice.so
        Reason: image not found
      Note: found 40588 marked UTF-8 strings
    ```

## Newly fixed

*   checking whether package ‘stacomiR’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/stacomiR/old/stacomiR.Rcheck/00install.out’ for details.
    ```

# strapgod

<details>

* Version: 0.0.4
* Source code: https://github.com/cran/strapgod
* URL: https://github.com/DavisVaughan/strapgod
* BugReports: https://github.com/DavisVaughan/strapgod/issues
* Date/Publication: 2019-09-20 04:50:02 UTC
* Number of recursive dependencies: 64

Run `revdep_details(,"strapgod")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      [1] 150 - 300 == -150
      
      ── 5. Failure: bind_cols() works (@test-dplyr-compat.R#374)  ───────────────────
      "tbl_df" %in% class(x_bc_2) isn't false.
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 150 | SKIPPED: 0 | WARNINGS: 1 | FAILED: 5 ]
      1. Error: add_count() (@test-dplyr-compat.R#293) 
      2. Failure: bind_rows() fails sadly (@test-dplyr-compat.R#341) 
      3. Failure: bind_cols() works (@test-dplyr-compat.R#354) 
      4. Failure: bind_cols() works (@test-dplyr-compat.R#366) 
      5. Failure: bind_cols() works (@test-dplyr-compat.R#374) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## Newly fixed

*   checking whether package ‘strapgod’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/strapgod/old/strapgod.Rcheck/00install.out’ for details.
    ```

# StratigrapheR

<details>

* Version: 1.0.1
* Source code: https://github.com/cran/StratigrapheR
* Date/Publication: 2020-03-20 13:50:06 UTC
* Number of recursive dependencies: 123

Run `revdep_details(,"StratigrapheR")` for more info

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
     4. │   └─vctrs::vec_slice(x, seq_len(xlen - n))
     5. └─vctrs:::stop_scalar_type(.Primitive("quote")(NULL), "")
     6.   └─vctrs:::stop_vctrs(msg, "vctrs_error_scalar_type", actual = x)
    Execution halted
    ```

## Newly fixed

*   checking whether package ‘StratigrapheR’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/StratigrapheR/old/StratigrapheR.Rcheck/00install.out’ for details.
    ```

# sugarbag

<details>

* Version: 0.1.2
* Source code: https://github.com/cran/sugarbag
* URL: https://srkobakian.github.io/sugarbag/, https://github.com/srkobakian/sugarbag
* Date/Publication: 2020-01-08 20:40:02 UTC
* Number of recursive dependencies: 108

Run `revdep_details(,"sugarbag")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > 
    > 
    > data(tas_sa2)
    > data(capital_cities)
    > hexmap <- create_hexmap(
    +   shp = tas_lga,
    +   sf_id = "LGA_CODE16",
    +   focal_points = capital_cities, verbose = TRUE
    + )
    Warning in st_centroid.sf(., of_largest_polygon = largest) :
      st_centroid assumes attributes are constant over geometries of x
    Warning in st_centroid.sfc(st_geometry(x), of_largest_polygon = of_largest_polygon) :
      st_centroid does not give correct centroids for longitude/latitude data
    Warning: st_crs<- : replacing crs does not reproject data; use st_transform for that
    Warning: st_crs<- : replacing crs does not reproject data; use st_transform for that
    Buffer set to 1.224 degrees.
    Converted hexagon size to 0.1205 degrees.
    Filter set to 1.2047 degrees.
    Finding closest point in focal_points data set.
    Closest points found.
    New names:
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘lwgeom’
      All declared Imports should be used.
    ```

## Newly fixed

*   checking whether package ‘sugarbag’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/sugarbag/old/sugarbag.Rcheck/00install.out’ for details.
    ```

# survminer

<details>

* Version: 0.4.6
* Source code: https://github.com/cran/survminer
* URL: http://www.sthda.com/english/rpkgs/survminer/
* BugReports: https://github.com/kassambara/survminer/issues
* Date/Publication: 2019-09-03 23:00:02 UTC
* Number of recursive dependencies: 87

Run `revdep_details(,"survminer")` for more info

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

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        doc   5.1Mb
    ```

## Newly fixed

*   checking whether package ‘survminer’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/survminer/old/survminer.Rcheck/00install.out’ for details.
    ```

# textreuse

<details>

* Version: 0.1.4
* Source code: https://github.com/cran/textreuse
* URL: https://github.com/ropensci/textreuse
* BugReports: https://github.com/ropensci/textreuse/issues
* Date/Publication: 2016-11-28 16:54:10
* Number of recursive dependencies: 60

Run `revdep_details(,"textreuse")` for more info

</details>

## Newly broken

*   checking whether package ‘textreuse’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/textreuse/new/textreuse.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘tm’
    ```

## Installation

### Devel

```
* installing *source* package ‘textreuse’ ...
** package ‘textreuse’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/textreuse/BH/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/textreuse/Rcpp/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/textreuse/RcppProgress/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c RcppExports.cpp -o RcppExports.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/textreuse/BH/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/textreuse/Rcpp/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/textreuse/RcppProgress/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c hash_string.cpp -o hash_string.o
In file included from hash_string.cpp:2:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/textreuse/BH/include/boost/functional/hash.hpp:6:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/textreuse/BH/include/boost/container_hash/hash.hpp:19:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/textreuse/BH/include/boost/container_hash/hash_fwd.hpp:13:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/textreuse/BH/include/boost/config/workaround.hpp:41:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/textreuse/BH/include/boost/config.hpp:57:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/textreuse/BH/include/boost/config/platform/macos.hpp:28:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/textreuse/BH/include/boost/config/detail/posix_features.hpp:18:
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:654:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/gethostuuid.h:39:17: error: C++ requires a type specifier for all declarations
int gethostuuid(uuid_t, const struct timespec *) __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA);
                ^
In file included from hash_string.cpp:2:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/textreuse/BH/include/boost/functional/hash.hpp:6:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/textreuse/BH/include/boost/container_hash/hash.hpp:19:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/textreuse/BH/include/boost/container_hash/hash_fwd.hpp:13:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/textreuse/BH/include/boost/config/workaround.hpp:41:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/textreuse/BH/include/boost/config.hpp:57:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/textreuse/BH/include/boost/config/platform/macos.hpp:28:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/textreuse/BH/include/boost/config/detail/posix_features.hpp:18:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:661:27: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      getsgroups_np(int *, uuid_t);
                              ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from hash_string.cpp:2:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/textreuse/BH/include/boost/functional/hash.hpp:6:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/textreuse/BH/include/boost/container_hash/hash.hpp:19:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/textreuse/BH/include/boost/container_hash/hash_fwd.hpp:13:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/textreuse/BH/include/boost/config/workaround.hpp:41:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/textreuse/BH/include/boost/config.hpp:57:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/textreuse/BH/include/boost/config/platform/macos.hpp:28:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/textreuse/BH/include/boost/config/detail/posix_features.hpp:18:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:663:27: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      getwgroups_np(int *, uuid_t);
                              ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from hash_string.cpp:2:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/textreuse/BH/include/boost/functional/hash.hpp:6:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/textreuse/BH/include/boost/container_hash/hash.hpp:19:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/textreuse/BH/include/boost/container_hash/hash_fwd.hpp:13:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/textreuse/BH/include/boost/config/workaround.hpp:41:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/textreuse/BH/include/boost/config.hpp:57:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/textreuse/BH/include/boost/config/platform/macos.hpp:28:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/textreuse/BH/include/boost/config/detail/posix_features.hpp:18:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:726:31: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      setsgroups_np(int, const uuid_t);
                                  ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from hash_string.cpp:2:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/textreuse/BH/include/boost/functional/hash.hpp:6:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/textreuse/BH/include/boost/container_hash/hash.hpp:19:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/textreuse/BH/include/boost/container_hash/hash_fwd.hpp:13:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/textreuse/BH/include/boost/config/workaround.hpp:41:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/textreuse/BH/include/boost/config.hpp:57:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/textreuse/BH/include/boost/config/platform/macos.hpp:28:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/textreuse/BH/include/boost/config/detail/posix_features.hpp:18:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:728:31: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      setwgroups_np(int, const uuid_t);
                                  ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
5 errors generated.
make: *** [hash_string.o] Error 1
ERROR: compilation failed for package ‘textreuse’
* removing ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/textreuse/new/textreuse.Rcheck/textreuse’

```
### CRAN

```
* installing *source* package ‘textreuse’ ...
** package ‘textreuse’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/BH/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/textreuse/RcppProgress/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c RcppExports.cpp -o RcppExports.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/BH/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/textreuse/RcppProgress/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c hash_string.cpp -o hash_string.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/BH/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/textreuse/RcppProgress/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c shingle_ngrams.cpp -o shingle_ngrams.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/BH/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/textreuse/RcppProgress/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c skip_ngrams.cpp -o skip_ngrams.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/BH/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/textreuse/RcppProgress/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c sw_matrix.cpp -o sw_matrix.o
clang++ -std=gnu++11 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o textreuse.so RcppExports.o hash_string.o shingle_ngrams.o skip_ngrams.o sw_matrix.o -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
installing to /Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/textreuse/old/textreuse.Rcheck/00LOCK-textreuse/00new/textreuse/libs
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** checking absolute paths in shared objects and dynamic libraries
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (textreuse)

```
# tidycells

<details>

* Version: 0.2.2
* Source code: https://github.com/cran/tidycells
* URL: https://r-rudra.github.io/tidycells/, https://github.com/r-rudra/tidycells
* BugReports: https://github.com/r-rudra/tidycells/issues
* Date/Publication: 2020-01-09 19:10:09 UTC
* Number of recursive dependencies: 115

Run `revdep_details(,"tidycells")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > cd <- as_cell_df(d) %>% numeric_values_classifier()
    Error: Input must be a vector, not a `tbl_df/tbl/data.frame/unpivotr` object.
    Backtrace:
         █
      1. ├─as_cell_df(d) %>% numeric_values_classifier()
      2. │ └─base::eval(lhs, parent, parent)
      3. │   └─base::eval(lhs, parent, parent)
      4. ├─tidycells::as_cell_df(d)
      5. ├─tidycells:::as_cell_df.data.frame(d)
      6. │ └─d %>% attach_intermediate_class() %>% as_cell_df_internal(...)
      7. │   ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      8. │   └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      9. │     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
     10. │       └─tidycells:::`_fseq`(`_lhs`)
     11. │         └─magrittr::freduce(value, `_function_list`)
     12. │           ├─base::withVisible(function_list[[k]](value))
     13. │           └─function_list[[k]](value)
     14. │             ├─tidycells:::as_cell_df_internal(., ...)
     15. │             └─tidycells:::as_cell_df_internal.default(., ...)
     16. 
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 34 | SKIPPED: 2 | WARNINGS: 7 | FAILED: 33 ]
      1. Error: numeric_values_classifier works (@test-VA_classifier.R#5) 
      2. Error: sample_based_classifier works (@test-VA_classifier.R#45) 
      3. Error: sample_based_classifier works (@test-VA_classifier.R#41) 
      4. Error: (unknown) (@test-VA_classifier.R#41) 
      5. Error: analyze_cell works: base 
      6. Error: analyze_cell works: tidyxl (@test-analyze_cells.R#28) 
      7. Error: analyze_cell works: tidyxl (@test-analyze_cells.R#25) 
      8. Error: (unknown) (@test-analyze_cells.R#25) 
      9. Error: as_cell_df() works on tidyxl::xlsx_cells on single sheet (@test-as_cell_df.R#6) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

## Newly fixed

*   checking whether package ‘tidycells’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/tidycells/old/tidycells.Rcheck/00install.out’ for details.
    ```

# tidygraph

<details>

* Version: 1.1.2
* Source code: https://github.com/cran/tidygraph
* URL: https://github.com/thomasp85/tidygraph
* BugReports: https://github.com/thomasp85/tidygraph/issues
* Date/Publication: 2019-02-18 22:30:03 UTC
* Number of recursive dependencies: 93

Run `revdep_details(,"tidygraph")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > 
      > test_check("tidygraph")
      ── 1. Failure: bind_nodes works (@test-bind.R#19)  ─────────────────────────────
      as_tibble(gr1) not equal to `tbl`.
      Attributes: < Component "class": Lengths (3, 1) differ (string compare on first 1) >
      Attributes: < Component "class": 1 string mismatch >
      Attributes: < Component "row.names": Modes: numeric, character >
      Attributes: < Component "row.names": target is numeric, current is character >
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 274 | SKIPPED: 0 | WARNINGS: 3 | FAILED: 1 ]
      1. Failure: bind_nodes works (@test-bind.R#19) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking S3 generic/method consistency ... WARNING
    ```
    pull:
      function(.data, var, name)
    pull.morphed_tbl_graph:
      function(.data, var)
    
    pull:
      function(.data, var, name)
    pull.tbl_graph:
      function(.data, var)
    
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in documentation object 'graph_join.Rd':
      ‘join.tbl_df’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

## Newly fixed

*   checking whether package ‘tidygraph’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/tidygraph/old/tidygraph.Rcheck/00install.out’ for details.
    ```

# tidyjson

<details>

* Version: 0.2.4
* Source code: https://github.com/cran/tidyjson
* URL: https://github.com/colearendt/tidyjson
* BugReports: https://github.com/colearendt/tidyjson/issues
* Date/Publication: 2019-12-02 21:39:30
* Number of recursive dependencies: 89

Run `revdep_details(,"tidyjson")` for more info

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

*   checking tests ...
    ```
     ERROR
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

## Newly fixed

*   checking whether package ‘tidyjson’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/tidyjson/old/tidyjson.Rcheck/00install.out’ for details.
    ```

# tidync

<details>

* Version: 0.2.3
* Source code: https://github.com/cran/tidync
* URL: https://docs.ropensci.org/tidync/
* BugReports: https://github.com/ropensci/tidync/issues
* Date/Publication: 2019-11-07 00:00:02 UTC
* Number of recursive dependencies: 77

Run `revdep_details(,"tidync")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in documentation object 'hyper_tbl_cube.Rd':
      ‘[dplyr:tbl_cube]{tbl_cube}’ ‘[dplyr:tbl_cube]{dplyr}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

## Newly fixed

*   checking whether package ‘tidync’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/tidync/old/tidync.Rcheck/00install.out’ for details.
    ```

# tidyr

<details>

* Version: 1.0.2
* Source code: https://github.com/cran/tidyr
* URL: https://tidyr.tidyverse.org, https://github.com/tidyverse/tidyr
* BugReports: https://github.com/tidyverse/tidyr/issues
* Date/Publication: 2020-01-24 14:30:02 UTC
* Number of recursive dependencies: 61

Run `revdep_details(,"tidyr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > df %>% unchop(y, ptype = tibble(y = integer()))
    Error: Can't convert `y` <character> to `y` <integer>.
    Backtrace:
         █
      1. ├─df %>% unchop(y, ptype = tibble(y = integer()))
      2. │ ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      3. │ └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      4. │   └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5. │     └─`_fseq`(`_lhs`)
      6. │       └─magrittr::freduce(value, `_function_list`)
      7. │         ├─base::withVisible(function_list[[k]](value))
      8. │         └─function_list[[k]](value)
      9. │           └─tidyr::unchop(., y, ptype = tibble(y = integer()))
     10. │             └─vctrs::vec_rbind(!!!x, .ptype = ptype)
     11. └─vctrs::vec_default_cast(...)
     12.   └─vctrs::stop_incompatible_cast(...)
     13.     └─vctrs:::stop_incompatible_type_convert(...)
     14.       └─vctrs:::stop_incompatible_type_impl(...)
     15.         └─vctrs:::stop_incompatible(...)
     16.           └─vctrs:::stop_vctrs(...)
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 497 | SKIPPED: 0 | WARNINGS: 3 | FAILED: 18 ]
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

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 24 marked UTF-8 strings
    ```

## Newly fixed

*   checking whether package ‘tidyr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/tidyr/old/tidyr.Rcheck/00install.out’ for details.
    ```

# tidyRSS

<details>

* Version: 2.0.1
* Source code: https://github.com/cran/tidyRSS
* URL: https://github.com/RobertMyles/tidyrss
* BugReports: https://github.com/RobertMyles/tidyrss/issues
* Date/Publication: 2020-03-07 16:00:02 UTC
* Number of recursive dependencies: 60

Run `revdep_details(,"tidyRSS")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── 2. Error: df is cleaned properly (@test_general.R#84)  ──────────────────────
      Argument 1 must have names
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

## Newly fixed

*   checking whether package ‘tidyRSS’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/tidyRSS/old/tidyRSS.Rcheck/00install.out’ for details.
    ```

# tidystats

<details>

* Version: 0.4
* Source code: https://github.com/cran/tidystats
* Date/Publication: 2019-09-12 07:20:02 UTC
* Number of recursive dependencies: 29

Run `revdep_details(,"tidystats")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
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
     11.         ├─base::do.call("[", args)
     12.         ├─x[1:2, , drop = FALSE]
     13.         └─dplyr:::`[.grouped_df`(x, 1:2, , drop = FALSE)
     14.           └─dplyr::grouped_df(out, groups, group_by_drop_default(x))
     15.             └─dplyr:::compute_groups(data, vars, drop = drop)
     16.     
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tidyr’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 113 marked UTF-8 strings
    ```

## Newly fixed

*   checking whether package ‘tidystats’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/tidystats/old/tidystats.Rcheck/00install.out’ for details.
    ```

# tidystopwords

<details>

* Version: 0.9.0
* Source code: https://github.com/cran/tidystopwords
* Date/Publication: 2019-02-12 17:20:02 UTC
* Number of recursive dependencies: 36

Run `revdep_details(,"tidystopwords")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
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
     6.     └─vctrs:::stop_incompatible_type_combine(...)
     7.       └─vctrs:::stop_incompatible_type_impl(...)
     8.         └─vctrs:::stop_incompatible(...)
     9.           └─vctrs:::stop_vctrs(...)
    Execution halted
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 229801 marked UTF-8 strings
    ```

## Newly fixed

*   checking whether package ‘tidystopwords’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/tidystopwords/old/tidystopwords.Rcheck/00install.out’ for details.
    ```

# tidyxl

<details>

* Version: 1.0.4
* Source code: https://github.com/cran/tidyxl
* URL: https://github.com/nacnudus/tidyxl
* BugReports: https://github.com/nacnudus/tidyxl/issues
* Date/Publication: 2019-01-02 11:30:04 UTC
* Number of recursive dependencies: 74

Run `revdep_details(,"tidyxl")` for more info

</details>

## Newly broken

*   checking whether package ‘tidyxl’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/tidyxl/new/tidyxl.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking whether package ‘tidyxl’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      ref.cpp:24:14: warning: explicitly assigning value of variable of type 'std::string::const_iterator' (aka '__wrap_iter<const char *>') to itself [-Wself-assign-overloaded]
      ref.cpp:35:14: warning: explicitly assigning value of variable of type 'std::string::const_iterator' (aka '__wrap_iter<const char *>') to itself [-Wself-assign-overloaded]
      ref.cpp:50:16: warning: explicitly assigning value of variable of type 'std::string::const_iterator' (aka '__wrap_iter<const char *>') to itself [-Wself-assign-overloaded]
      ref.cpp:61:16: warning: explicitly assigning value of variable of type 'std::string::const_iterator' (aka '__wrap_iter<const char *>') to itself [-Wself-assign-overloaded]
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/tidyxl/old/tidyxl.Rcheck/00install.out’ for details.
    ```

*   checking compiled code ... WARNING
    ```
    File ‘tidyxl/libs/tidyxl.so’:
      Found ‘_abort’, possibly from ‘abort’ (C)
        Object: ‘xlex.o’
    
    Compiled code should not call entry points which might terminate R nor
    write to stdout/stderr instead of to the console, nor use Fortran I/O
    nor system RNGs.
    
    See ‘Writing portable packages’ in the ‘Writing R Extensions’ manual.
    ```

## Installation

### Devel

```
* installing *source* package ‘tidyxl’ ...
** package ‘tidyxl’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/tidyxl/Rcpp/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/tidyxl/piton/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c RcppExports.cpp -o RcppExports.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/tidyxl/Rcpp/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/tidyxl/piton/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c border.cpp -o border.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/tidyxl/Rcpp/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/tidyxl/piton/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c color.cpp -o color.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/tidyxl/Rcpp/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/tidyxl/piton/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c fill.cpp -o fill.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/tidyxl/Rcpp/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/tidyxl/piton/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c font.cpp -o font.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/tidyxl/Rcpp/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/tidyxl/piton/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c gradientFill.cpp -o gradientFill.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/tidyxl/Rcpp/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/tidyxl/piton/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c gradientStop.cpp -o gradientStop.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/tidyxl/Rcpp/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/tidyxl/piton/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c patternFill.cpp -o patternFill.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/tidyxl/Rcpp/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/tidyxl/piton/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c ref.cpp -o ref.o
ref.cpp:24:14: warning: explicitly assigning value of variable of type 'std::string::const_iterator' (aka '__wrap_iter<const char *>') to itself [-Wself-assign-overloaded]
    for(iter = iter; (*iter >= 'A' && *iter <= 'Z'); ++iter) {
        ~~~~ ^ ~~~~
ref.cpp:35:14: warning: explicitly assigning value of variable of type 'std::string::const_iterator' (aka '__wrap_iter<const char *>') to itself [-Wself-assign-overloaded]
    for(iter = iter; (*iter >= '0' && *iter <= '9'); ++iter) {
        ~~~~ ^ ~~~~
ref.cpp:50:16: warning: explicitly assigning value of variable of type 'std::string::const_iterator' (aka '__wrap_iter<const char *>') to itself [-Wself-assign-overloaded]
      for(iter = iter; (*iter >= 'A' && *iter <= 'Z'); ++iter) {
          ~~~~ ^ ~~~~
ref.cpp:61:16: warning: explicitly assigning value of variable of type 'std::string::const_iterator' (aka '__wrap_iter<const char *>') to itself [-Wself-assign-overloaded]
      for(iter = iter; (*iter >= '0' && *iter <= '9'); ++iter) {
          ~~~~ ^ ~~~~
4 warnings generated.
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/tidyxl/Rcpp/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/tidyxl/piton/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c shared_formula.cpp -o shared_formula.o
In file included from shared_formula.cpp:2:
In file included from ./shared_formula.h:5:
In file included from ./ref_grammar.h:6:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/tidyxl/piton/include/pegtl.hpp:20:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/tidyxl/piton/include/pegtl/file_input.hpp:12:
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:654:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/gethostuuid.h:39:17: error: C++ requires a type specifier for all declarations
int gethostuuid(uuid_t, const struct timespec *) __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA);
                ^
In file included from shared_formula.cpp:2:
In file included from ./shared_formula.h:5:
In file included from ./ref_grammar.h:6:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/tidyxl/piton/include/pegtl.hpp:20:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/tidyxl/piton/include/pegtl/file_input.hpp:12:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:661:27: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      getsgroups_np(int *, uuid_t);
                              ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from shared_formula.cpp:2:
In file included from ./shared_formula.h:5:
In file included from ./ref_grammar.h:6:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/tidyxl/piton/include/pegtl.hpp:20:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/tidyxl/piton/include/pegtl/file_input.hpp:12:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:663:27: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      getwgroups_np(int *, uuid_t);
                              ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from shared_formula.cpp:2:
In file included from ./shared_formula.h:5:
In file included from ./ref_grammar.h:6:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/tidyxl/piton/include/pegtl.hpp:20:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/tidyxl/piton/include/pegtl/file_input.hpp:12:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:726:31: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      setsgroups_np(int, const uuid_t);
                                  ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from shared_formula.cpp:2:
In file included from ./shared_formula.h:5:
In file included from ./ref_grammar.h:6:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/tidyxl/piton/include/pegtl.hpp:20:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/tidyxl/piton/include/pegtl/file_input.hpp:12:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:728:31: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      setwgroups_np(int, const uuid_t);
                                  ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from shared_formula.cpp:1:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/tidyxl/Rcpp/include/Rcpp.h:27:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/tidyxl/Rcpp/include/RcppCommon.h:29:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/tidyxl/Rcpp/include/Rcpp/r/headers.h:67:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/tidyxl/Rcpp/include/Rcpp/platform/compiler.h:153:
In file included from /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/unordered_map:408:
In file included from /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/__hash_table:16:
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/memory:1881:58: warning: destructor called on non-final 'ref' that has virtual functions but non-virtual destructor [-Wdelete-non-abstract-non-virtual-dtor]
    _LIBCPP_INLINE_VISIBILITY void destroy(pointer __p) {__p->~_Tp();}
                                                         ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/memory:1743:18: note: in instantiation of member function 'std::__1::allocator<ref>::destroy' requested here
            {__a.destroy(__p);}
                 ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/memory:1596:14: note: in instantiation of function template specialization 'std::__1::allocator_traits<std::__1::allocator<ref> >::__destroy<ref>' requested here
            {__destroy(__has_destroy<allocator_type, _Tp*>(), __a, __p);}
             ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/vector:427:25: note: in instantiation of function template specialization 'std::__1::allocator_traits<std::__1::allocator<ref> >::destroy<ref>' requested here
        __alloc_traits::destroy(__alloc(), _VSTD::__to_raw_pointer(--__soon_to_be_end));
                        ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/vector:370:29: note: in instantiation of member function 'std::__1::__vector_base<ref, std::__1::allocator<ref> >::__destruct_at_end' requested here
    void clear() _NOEXCEPT {__destruct_at_end(__begin_);}
                            ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/vector:464:9: note: in instantiation of member function 'std::__1::__vector_base<ref, std::__1::allocator<ref> >::clear' requested here
        clear();
        ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/vector:496:5: note: in instantiation of member function 'std::__1::__vector_base<ref, std::__1::allocator<ref> >::~__vector_base' requested here
    vector() _NOEXCEPT_(is_nothrow_default_constructible<allocator_type>::value)
    ^
shared_formula.cpp:6:17: note: in instantiation of member function 'std::__1::vector<ref, std::__1::allocator<ref> >::vector' requested here
shared_formula::shared_formula(
                ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/memory:1881:64: note: qualify call to silence this warning
    _LIBCPP_INLINE_VISIBILITY void destroy(pointer __p) {__p->~_Tp();}
                                                               ^
1 warning and 5 errors generated.
make: *** [shared_formula.o] Error 1
ERROR: compilation failed for package ‘tidyxl’
* removing ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/tidyxl/new/tidyxl.Rcheck/tidyxl’

```
### CRAN

```
* installing *source* package ‘tidyxl’ ...
** package ‘tidyxl’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/tidyxl/piton/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c RcppExports.cpp -o RcppExports.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/tidyxl/piton/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c border.cpp -o border.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/tidyxl/piton/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c color.cpp -o color.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/tidyxl/piton/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c fill.cpp -o fill.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/tidyxl/piton/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c font.cpp -o font.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/tidyxl/piton/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c gradientFill.cpp -o gradientFill.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/tidyxl/piton/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c gradientStop.cpp -o gradientStop.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/tidyxl/piton/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c patternFill.cpp -o patternFill.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/tidyxl/piton/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c ref.cpp -o ref.o
ref.cpp:24:14: warning: explicitly assigning value of variable of type 'std::string::const_iterator' (aka '__wrap_iter<const char *>') to itself [-Wself-assign-overloaded]
    for(iter = iter; (*iter >= 'A' && *iter <= 'Z'); ++iter) {
        ~~~~ ^ ~~~~
ref.cpp:35:14: warning: explicitly assigning value of variable of type 'std::string::const_iterator' (aka '__wrap_iter<const char *>') to itself [-Wself-assign-overloaded]
    for(iter = iter; (*iter >= '0' && *iter <= '9'); ++iter) {
        ~~~~ ^ ~~~~
ref.cpp:50:16: warning: explicitly assigning value of variable of type 'std::string::const_iterator' (aka '__wrap_iter<const char *>') to itself [-Wself-assign-overloaded]
      for(iter = iter; (*iter >= 'A' && *iter <= 'Z'); ++iter) {
          ~~~~ ^ ~~~~
ref.cpp:61:16: warning: explicitly assigning value of variable of type 'std::string::const_iterator' (aka '__wrap_iter<const char *>') to itself [-Wself-assign-overloaded]
      for(iter = iter; (*iter >= '0' && *iter <= '9'); ++iter) {
          ~~~~ ^ ~~~~
4 warnings generated.
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/tidyxl/piton/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c shared_formula.cpp -o shared_formula.o
In file included from shared_formula.cpp:1:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include/Rcpp.h:27:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include/RcppCommon.h:29:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include/Rcpp/r/headers.h:67:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include/Rcpp/platform/compiler.h:153:
In file included from /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/unordered_map:408:
In file included from /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/__hash_table:16:
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/memory:1881:58: warning: destructor called on non-final 'ref' that has virtual functions but non-virtual destructor [-Wdelete-non-abstract-non-virtual-dtor]
    _LIBCPP_INLINE_VISIBILITY void destroy(pointer __p) {__p->~_Tp();}
                                                         ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/memory:1743:18: note: in instantiation of member function 'std::__1::allocator<ref>::destroy' requested here
            {__a.destroy(__p);}
                 ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/memory:1596:14: note: in instantiation of function template specialization 'std::__1::allocator_traits<std::__1::allocator<ref> >::__destroy<ref>' requested here
            {__destroy(__has_destroy<allocator_type, _Tp*>(), __a, __p);}
             ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/vector:427:25: note: in instantiation of function template specialization 'std::__1::allocator_traits<std::__1::allocator<ref> >::destroy<ref>' requested here
        __alloc_traits::destroy(__alloc(), _VSTD::__to_raw_pointer(--__soon_to_be_end));
                        ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/vector:370:29: note: in instantiation of member function 'std::__1::__vector_base<ref, std::__1::allocator<ref> >::__destruct_at_end' requested here
    void clear() _NOEXCEPT {__destruct_at_end(__begin_);}
                            ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/vector:464:9: note: in instantiation of member function 'std::__1::__vector_base<ref, std::__1::allocator<ref> >::clear' requested here
        clear();
        ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/vector:496:5: note: in instantiation of member function 'std::__1::__vector_base<ref, std::__1::allocator<ref> >::~__vector_base' requested here
    vector() _NOEXCEPT_(is_nothrow_default_constructible<allocator_type>::value)
    ^
shared_formula.cpp:6:17: note: in instantiation of member function 'std::__1::vector<ref, std::__1::allocator<ref> >::vector' requested here
shared_formula::shared_formula(
                ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/memory:1881:64: note: qualify call to silence this warning
    _LIBCPP_INLINE_VISIBILITY void destroy(pointer __p) {__p->~_Tp();}
                                                               ^
1 warning generated.
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/tidyxl/piton/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c stroke.cpp -o stroke.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/tidyxl/piton/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c tidyxl.cpp -o tidyxl.o
In file included from tidyxl.cpp:3:
In file included from /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/algorithm:644:
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/memory:1881:58: warning: destructor called on non-final 'ref' that has virtual functions but non-virtual destructor [-Wdelete-non-abstract-non-virtual-dtor]
    _LIBCPP_INLINE_VISIBILITY void destroy(pointer __p) {__p->~_Tp();}
                                                         ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/memory:1743:18: note: in instantiation of member function 'std::__1::allocator<ref>::destroy' requested here
            {__a.destroy(__p);}
                 ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/memory:1596:14: note: in instantiation of function template specialization 'std::__1::allocator_traits<std::__1::allocator<ref> >::__destroy<ref>' requested here
            {__destroy(__has_destroy<allocator_type, _Tp*>(), __a, __p);}
             ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/vector:427:25: note: in instantiation of function template specialization 'std::__1::allocator_traits<std::__1::allocator<ref> >::destroy<ref>' requested here
        __alloc_traits::destroy(__alloc(), _VSTD::__to_raw_pointer(--__soon_to_be_end));
                        ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/vector:370:29: note: in instantiation of member function 'std::__1::__vector_base<ref, std::__1::allocator<ref> >::__destruct_at_end' requested here
    void clear() _NOEXCEPT {__destruct_at_end(__begin_);}
                            ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/vector:464:9: note: in instantiation of member function 'std::__1::__vector_base<ref, std::__1::allocator<ref> >::clear' requested here
        clear();
        ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/vector:550:5: note: (skipping 8 contexts in backtrace; use -ftemplate-backtrace-limit=0 to see all)
    ~vector()
    ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/vector:427:25: note: in instantiation of function template specialization 'std::__1::allocator_traits<std::__1::allocator<xlsxsheet> >::destroy<xlsxsheet>' requested here
        __alloc_traits::destroy(__alloc(), _VSTD::__to_raw_pointer(--__soon_to_be_end));
                        ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/vector:370:29: note: in instantiation of member function 'std::__1::__vector_base<xlsxsheet, std::__1::allocator<xlsxsheet> >::__destruct_at_end' requested here
    void clear() _NOEXCEPT {__destruct_at_end(__begin_);}
                            ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/vector:464:9: note: in instantiation of member function 'std::__1::__vector_base<xlsxsheet, std::__1::allocator<xlsxsheet> >::clear' requested here
        clear();
        ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/vector:550:5: note: in instantiation of member function 'std::__1::__vector_base<xlsxsheet, std::__1::allocator<xlsxsheet> >::~__vector_base' requested here
    ~vector()
    ^
./xlsxbook.h:9:7: note: in instantiation of member function 'std::__1::vector<xlsxsheet, std::__1::allocator<xlsxsheet> >::~vector' requested here
class xlsxbook {
      ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/memory:1881:64: note: qualify call to silence this warning
    _LIBCPP_INLINE_VISIBILITY void destroy(pointer __p) {__p->~_Tp();}
                                                               ^
1 warning generated.
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/tidyxl/piton/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c xf.cpp -o xf.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/tidyxl/piton/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c xlex.cpp -o xlex.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/tidyxl/piton/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c xlsxbook.cpp -o xlsxbook.o
In file included from xlsxbook.cpp:1:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include/Rcpp.h:27:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include/RcppCommon.h:29:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include/Rcpp/r/headers.h:67:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include/Rcpp/platform/compiler.h:153:
In file included from /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/unordered_map:408:
In file included from /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/__hash_table:16:
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/memory:1881:58: warning: destructor called on non-final 'ref' that has virtual functions but non-virtual destructor [-Wdelete-non-abstract-non-virtual-dtor]
    _LIBCPP_INLINE_VISIBILITY void destroy(pointer __p) {__p->~_Tp();}
                                                         ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/memory:1743:18: note: in instantiation of member function 'std::__1::allocator<ref>::destroy' requested here
            {__a.destroy(__p);}
                 ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/memory:1596:14: note: in instantiation of function template specialization 'std::__1::allocator_traits<std::__1::allocator<ref> >::__destroy<ref>' requested here
            {__destroy(__has_destroy<allocator_type, _Tp*>(), __a, __p);}
             ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/vector:427:25: note: in instantiation of function template specialization 'std::__1::allocator_traits<std::__1::allocator<ref> >::destroy<ref>' requested here
        __alloc_traits::destroy(__alloc(), _VSTD::__to_raw_pointer(--__soon_to_be_end));
                        ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/vector:370:29: note: in instantiation of member function 'std::__1::__vector_base<ref, std::__1::allocator<ref> >::__destruct_at_end' requested here
    void clear() _NOEXCEPT {__destruct_at_end(__begin_);}
                            ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/vector:464:9: note: in instantiation of member function 'std::__1::__vector_base<ref, std::__1::allocator<ref> >::clear' requested here
        clear();
        ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/vector:550:5: note: (skipping 1 context in backtrace; use -ftemplate-backtrace-limit=0 to see all)
    ~vector()
    ^
./shared_formula.h:8:7: note: in instantiation of member function 'std::__1::vector<ref, std::__1::allocator<ref> >::~vector' requested here
class shared_formula {
      ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/memory:1596:14: note: in instantiation of function template specialization 'std::__1::allocator_traits<std::__1::allocator<std::__1::__tree_node<std::__1::__value_type<int, shared_formula>, void *> > >::__destroy<std::__1::pair<const int, shared_formula> >' requested here
            {__destroy(__has_destroy<allocator_type, _Tp*>(), __a, __p);}
             ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/__tree:1854:24: note: in instantiation of function template specialization 'std::__1::allocator_traits<std::__1::allocator<std::__1::__tree_node<std::__1::__value_type<int, shared_formula>, void *> > >::destroy<std::__1::pair<const int, shared_formula> >' requested here
        __node_traits::destroy(__na, _NodeTypes::__get_ptr(__nd->__value_));
                       ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/__tree:1842:3: note: in instantiation of member function 'std::__1::__tree<std::__1::__value_type<int, shared_formula>, std::__1::__map_value_compare<int, std::__1::__value_type<int, shared_formula>, std::__1::less<int>, true>, std::__1::allocator<std::__1::__value_type<int, shared_formula> > >::destroy' requested here
  destroy(__root());
  ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/map:830:77: note: in instantiation of member function 'std::__1::__tree<std::__1::__value_type<int, shared_formula>, std::__1::__map_value_compare<int, std::__1::__value_type<int, shared_formula>, std::__1::less<int>, true>, std::__1::allocator<std::__1::__value_type<int, shared_formula> > >::~__tree' requested here
    template <class, class, class, class> friend class _LIBCPP_TEMPLATE_VIS map;
                                                                            ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/memory:1881:64: note: qualify call to silence this warning
    _LIBCPP_INLINE_VISIBILITY void destroy(pointer __p) {__p->~_Tp();}
                                                               ^
1 warning generated.
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/tidyxl/piton/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c xlsxcell.cpp -o xlsxcell.o
In file included from xlsxcell.cpp:1:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include/Rcpp.h:27:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include/RcppCommon.h:29:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include/Rcpp/r/headers.h:67:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include/Rcpp/platform/compiler.h:153:
In file included from /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/unordered_map:408:
In file included from /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/__hash_table:16:
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/memory:1881:58: warning: destructor called on non-final 'ref' that has virtual functions but non-virtual destructor [-Wdelete-non-abstract-non-virtual-dtor]
    _LIBCPP_INLINE_VISIBILITY void destroy(pointer __p) {__p->~_Tp();}
                                                         ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/memory:1743:18: note: in instantiation of member function 'std::__1::allocator<ref>::destroy' requested here
            {__a.destroy(__p);}
                 ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/memory:1596:14: note: in instantiation of function template specialization 'std::__1::allocator_traits<std::__1::allocator<ref> >::__destroy<ref>' requested here
            {__destroy(__has_destroy<allocator_type, _Tp*>(), __a, __p);}
             ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/vector:427:25: note: in instantiation of function template specialization 'std::__1::allocator_traits<std::__1::allocator<ref> >::destroy<ref>' requested here
        __alloc_traits::destroy(__alloc(), _VSTD::__to_raw_pointer(--__soon_to_be_end));
                        ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/vector:370:29: note: in instantiation of member function 'std::__1::__vector_base<ref, std::__1::allocator<ref> >::__destruct_at_end' requested here
    void clear() _NOEXCEPT {__destruct_at_end(__begin_);}
                            ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/vector:464:9: note: in instantiation of member function 'std::__1::__vector_base<ref, std::__1::allocator<ref> >::clear' requested here
        clear();
        ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/vector:550:5: note: in instantiation of member function 'std::__1::__vector_base<ref, std::__1::allocator<ref> >::~__vector_base' requested here
    ~vector()
    ^
./shared_formula.h:8:7: note: in instantiation of member function 'std::__1::vector<ref, std::__1::allocator<ref> >::~vector' requested here
class shared_formula {
      ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/memory:1881:64: note: qualify call to silence this warning
    _LIBCPP_INLINE_VISIBILITY void destroy(pointer __p) {__p->~_Tp();}
                                                               ^
1 warning generated.
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/tidyxl/piton/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c xlsxnames.cpp -o xlsxnames.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/tidyxl/piton/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c xlsxsheet.cpp -o xlsxsheet.o
In file included from xlsxsheet.cpp:1:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include/Rcpp.h:27:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include/RcppCommon.h:29:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include/Rcpp/r/headers.h:67:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include/Rcpp/platform/compiler.h:153:
In file included from /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/unordered_map:408:
In file included from /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/__hash_table:16:
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/memory:1881:58: warning: destructor called on non-final 'ref' that has virtual functions but non-virtual destructor [-Wdelete-non-abstract-non-virtual-dtor]
    _LIBCPP_INLINE_VISIBILITY void destroy(pointer __p) {__p->~_Tp();}
                                                         ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/memory:1743:18: note: in instantiation of member function 'std::__1::allocator<ref>::destroy' requested here
            {__a.destroy(__p);}
                 ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/memory:1596:14: note: in instantiation of function template specialization 'std::__1::allocator_traits<std::__1::allocator<ref> >::__destroy<ref>' requested here
            {__destroy(__has_destroy<allocator_type, _Tp*>(), __a, __p);}
             ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/vector:427:25: note: in instantiation of function template specialization 'std::__1::allocator_traits<std::__1::allocator<ref> >::destroy<ref>' requested here
        __alloc_traits::destroy(__alloc(), _VSTD::__to_raw_pointer(--__soon_to_be_end));
                        ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/vector:370:29: note: in instantiation of member function 'std::__1::__vector_base<ref, std::__1::allocator<ref> >::__destruct_at_end' requested here
    void clear() _NOEXCEPT {__destruct_at_end(__begin_);}
                            ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/vector:464:9: note: in instantiation of member function 'std::__1::__vector_base<ref, std::__1::allocator<ref> >::clear' requested here
        clear();
        ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/vector:550:5: note: (skipping 1 context in backtrace; use -ftemplate-backtrace-limit=0 to see all)
    ~vector()
    ^
./shared_formula.h:8:7: note: in instantiation of member function 'std::__1::vector<ref, std::__1::allocator<ref> >::~vector' requested here
class shared_formula {
      ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/memory:1596:14: note: in instantiation of function template specialization 'std::__1::allocator_traits<std::__1::allocator<std::__1::__tree_node<std::__1::__value_type<int, shared_formula>, void *> > >::__destroy<std::__1::pair<const int, shared_formula> >' requested here
            {__destroy(__has_destroy<allocator_type, _Tp*>(), __a, __p);}
             ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/__tree:1854:24: note: in instantiation of function template specialization 'std::__1::allocator_traits<std::__1::allocator<std::__1::__tree_node<std::__1::__value_type<int, shared_formula>, void *> > >::destroy<std::__1::pair<const int, shared_formula> >' requested here
        __node_traits::destroy(__na, _NodeTypes::__get_ptr(__nd->__value_));
                       ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/__tree:1842:3: note: in instantiation of member function 'std::__1::__tree<std::__1::__value_type<int, shared_formula>, std::__1::__map_value_compare<int, std::__1::__value_type<int, shared_formula>, std::__1::less<int>, true>, std::__1::allocator<std::__1::__value_type<int, shared_formula> > >::destroy' requested here
  destroy(__root());
  ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/map:830:77: note: in instantiation of member function 'std::__1::__tree<std::__1::__value_type<int, shared_formula>, std::__1::__map_value_compare<int, std::__1::__value_type<int, shared_formula>, std::__1::less<int>, true>, std::__1::allocator<std::__1::__value_type<int, shared_formula> > >::~__tree' requested here
    template <class, class, class, class> friend class _LIBCPP_TEMPLATE_VIS map;
                                                                            ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/memory:1881:64: note: qualify call to silence this warning
    _LIBCPP_INLINE_VISIBILITY void destroy(pointer __p) {__p->~_Tp();}
                                                               ^
1 warning generated.
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/tidyxl/piton/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c xlsxstyles.cpp -o xlsxstyles.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/tidyxl/piton/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c xlsxvalidation.cpp -o xlsxvalidation.o
In file included from xlsxvalidation.cpp:1:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include/Rcpp.h:27:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include/RcppCommon.h:29:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include/Rcpp/r/headers.h:67:
In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include/Rcpp/platform/compiler.h:153:
In file included from /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/unordered_map:408:
In file included from /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/__hash_table:16:
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/memory:1881:58: warning: destructor called on non-final 'ref' that has virtual functions but non-virtual destructor [-Wdelete-non-abstract-non-virtual-dtor]
    _LIBCPP_INLINE_VISIBILITY void destroy(pointer __p) {__p->~_Tp();}
                                                         ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/memory:1743:18: note: in instantiation of member function 'std::__1::allocator<ref>::destroy' requested here
            {__a.destroy(__p);}
                 ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/memory:1596:14: note: in instantiation of function template specialization 'std::__1::allocator_traits<std::__1::allocator<ref> >::__destroy<ref>' requested here
            {__destroy(__has_destroy<allocator_type, _Tp*>(), __a, __p);}
             ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/vector:427:25: note: in instantiation of function template specialization 'std::__1::allocator_traits<std::__1::allocator<ref> >::destroy<ref>' requested here
        __alloc_traits::destroy(__alloc(), _VSTD::__to_raw_pointer(--__soon_to_be_end));
                        ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/vector:370:29: note: in instantiation of member function 'std::__1::__vector_base<ref, std::__1::allocator<ref> >::__destruct_at_end' requested here
    void clear() _NOEXCEPT {__destruct_at_end(__begin_);}
                            ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/vector:464:9: note: in instantiation of member function 'std::__1::__vector_base<ref, std::__1::allocator<ref> >::clear' requested here
        clear();
        ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/vector:550:5: note: (skipping 8 contexts in backtrace; use -ftemplate-backtrace-limit=0 to see all)
    ~vector()
    ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/vector:427:25: note: in instantiation of function template specialization 'std::__1::allocator_traits<std::__1::allocator<xlsxsheet> >::destroy<xlsxsheet>' requested here
        __alloc_traits::destroy(__alloc(), _VSTD::__to_raw_pointer(--__soon_to_be_end));
                        ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/vector:370:29: note: in instantiation of member function 'std::__1::__vector_base<xlsxsheet, std::__1::allocator<xlsxsheet> >::__destruct_at_end' requested here
    void clear() _NOEXCEPT {__destruct_at_end(__begin_);}
                            ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/vector:464:9: note: in instantiation of member function 'std::__1::__vector_base<xlsxsheet, std::__1::allocator<xlsxsheet> >::clear' requested here
        clear();
        ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/vector:550:5: note: in instantiation of member function 'std::__1::__vector_base<xlsxsheet, std::__1::allocator<xlsxsheet> >::~__vector_base' requested here
    ~vector()
    ^
./xlsxbook.h:9:7: note: in instantiation of member function 'std::__1::vector<xlsxsheet, std::__1::allocator<xlsxsheet> >::~vector' requested here
class xlsxbook {
      ^
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/memory:1881:64: note: qualify call to silence this warning
    _LIBCPP_INLINE_VISIBILITY void destroy(pointer __p) {__p->~_Tp();}
                                                               ^
1 warning generated.
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/tidyxl/piton/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c zip.cpp -o zip.o
clang++ -std=gnu++11 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o tidyxl.so RcppExports.o border.o color.o fill.o font.o gradientFill.o gradientStop.o patternFill.o ref.o shared_formula.o stroke.o tidyxl.o xf.o xlex.o xlsxbook.o xlsxcell.o xlsxnames.o xlsxsheet.o xlsxstyles.o xlsxvalidation.o zip.o -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
installing to /Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/tidyxl/old/tidyxl.Rcheck/00LOCK-tidyxl/00new/tidyxl/libs
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
** checking absolute paths in shared objects and dynamic libraries
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (tidyxl)

```
# TimeSeriesExperiment

<details>

* Version: 1.5.0
* Source code: https://github.com/cran/TimeSeriesExperiment
* URL: https://github.com/nlhuong/TimeSeriesExperiment
* BugReports: https://github.com/nlhuong/TimeSeriesExperiment/issues
* Date/Publication: 2019-10-29
* Number of recursive dependencies: 143

Run `revdep_details(,"TimeSeriesExperiment")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Aggregating across replicates...
    Converting to timeseries format...
    Averaging timecourses over all 'groups' selected and recomputing lags with coefficients: 0.5 0.25
    Warning: `data_frame()` is deprecated as of tibble 1.1.0.
    Please use `tibble()` instead.
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_warnings()` to see where this warning was generated.
    Error: `.data` must be a data frame without row names.
    Backtrace:
         █
      1. └─TimeSeriesExperiment::clusterTimeSeries(endoderm_small)
      2.   └─res_cluster_subset$clust_centroids %>% column_to_rownames("cluster")
      3.     ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      4.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5.       └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      6.         └─TimeSeriesExperiment:::`_fseq`(`_lhs`)
      7.           └─magrittr::freduce(value, `_function_list`)
      8.             ├─base::withVisible(function_list[[k]](value))
      9.             └─function_list[[k]](value)
     10.               └─tibble::column_to_rownames(., "cluster")
    Execution halted
    ```

*   checking Rd \usage sections ... WARNING
    ```
    Documented arguments not in \usage in documentation object 'plotHeatmap':
      ‘...’
    
    Functions with \usage entries need to have the appropriate \alias
    entries, and all their arguments documented.
    The \usage entries must correspond to syntactically valid R code.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unexported object imported by a ':::' call: ‘S4Vectors:::selectSome’
      See the note in ?`:::` about the use of this operator.
    ```

*   checking R code for possible problems ... NOTE
    ```
    clusterTimeSeries: no visible binding for global variable ‘freq’
    clusterTimeSeries: no visible binding for global variable ‘.’
    Undefined global functions or variables:
      . freq
    ```

## Newly fixed

*   checking whether package ‘TimeSeriesExperiment’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/TimeSeriesExperiment/old/TimeSeriesExperiment.Rcheck/00install.out’ for details.
    ```

# tree.bins

<details>

* Version: 0.1.1
* Source code: https://github.com/cran/tree.bins
* Date/Publication: 2018-06-14 05:33:53 UTC
* Number of recursive dependencies: 69

Run `revdep_details(,"tree.bins")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
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

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘forcats’
    ```

## Newly fixed

*   checking whether package ‘tree.bins’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/tree.bins/old/tree.bins.Rcheck/00install.out’ for details.
    ```

# treeplyr

<details>

* Version: 0.1.7
* Source code: https://github.com/cran/treeplyr
* URL: https://github.com/uyedaj/treeplyr
* BugReports: https://github.com/uyedaj/treeplyr/issues
* Date/Publication: 2019-07-25 22:50:02 UTC
* Number of recursive dependencies: 52

Run `revdep_details(,"treeplyr")` for more info

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
    Error: `vars` must be a character vector
    Backtrace:
        █
     1. ├─dplyr::group_by(td, ecomorph)
     2. └─dplyr:::group_by.default(td, ecomorph)
     3.   ├─dplyr::group_by_(.data, .dots = compat_as_lazy_dots(...), add = add)
     4.   └─treeplyr:::group_by_.treedata(...)
     5.     └─dplyr::grouped_df(groups$data, groups$groups)
    Execution halted
    ```

## Newly fixed

*   checking whether package ‘treeplyr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/treeplyr/old/treeplyr.Rcheck/00install.out’ for details.
    ```

# tsbox

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/tsbox
* URL: https://www.tsbox.help
* BugReports: https://github.com/christophsax/tsbox/issues
* Date/Publication: 2019-08-06 06:40:02 UTC
* Number of recursive dependencies: 93

Run `revdep_details(,"tsbox")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(tsbox)
      > 
      > test_check("tsbox")
      ── 1. Failure: df aggregation using date_ functions is working (@test_date_utils
      `x` not equal to ts_tbl(ts_frequency(ts_c(mdeaths, fdeaths), "quarter")).
      Component "id": 48 string mismatches
      Component "value": Mean relative difference: 0.9095133
      
      "blabla"
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 458 | SKIPPED: 13 | WARNINGS: 0 | FAILED: 1 ]
      1. Failure: df aggregation using date_ functions is working (@test_date_utils.R#26) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## Newly fixed

*   checking whether package ‘tsbox’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/tsbox/old/tsbox.Rcheck/00install.out’ for details.
    ```

# tsibble

<details>

* Version: 0.8.6
* Source code: https://github.com/cran/tsibble
* URL: https://tsibble.tidyverts.org
* BugReports: https://github.com/tidyverts/tsibble/issues
* Date/Publication: 2020-01-31 06:20:11 UTC
* Number of recursive dependencies: 93

Run `revdep_details(,"tsibble")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    +   group_by_key() %>%
    +   fill_gaps(kilo = sum(kilo))
    Error: `vars` must be a character vector
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

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 524 | SKIPPED: 2 | WARNINGS: 16 | FAILED: 34 ]
      1. Failure: 4 day interval (@test-append.R#27) 
      2. Error: (unknown) (@test-append.R#31) 
      3. Error: (unknown) (@test-bind.R#11) 
      4. Error: (unknown) (@test-dplyr.R#5) 
      5. Error: (unknown) (@test-empty.R#32) 
      6. Error: (unknown) (@test-gaps.R#93) 
      7. Error: (unknown) (@test-groups.R#3) 
      8. Error: From seconds to higher date (@test-indexby.R#25) 
      9. Error: From Date to year-week, year-month, year-quarter and year (@test-indexby.R#56) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘dtplyr’, ‘dbplyr’
    ```

## Newly fixed

*   checking whether package ‘tsibble’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/tsibble/old/tsibble.Rcheck/00install.out’ for details.
    ```

# unpivotr

<details>

* Version: 0.5.1
* Source code: https://github.com/cran/unpivotr
* URL: https://github.com/nacnudus/unpivotr
* BugReports: https://github.com/nacnudus/unpivotr/issues
* Date/Publication: 2019-03-30 19:10:03 UTC
* Number of recursive dependencies: 76

Run `revdep_details(,"unpivotr")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 232 | SKIPPED: 0 | WARNINGS: 159 | FAILED: 22 ]
      1. Failure: the `drop_na` argument of behead() works (@test-behead.R#32) 
      2. Failure: the `drop_na` argument of behead() works (@test-behead.R#42) 
      3. Failure: behead() works with all common datatypes (@test-behead.R#71) 
      4. Failure: behead() handles headers of mixed data types including dates (@test-behead.R#107) 
      5. Failure: behead() handles headers of factor and ordered-factor data types (@test-behead.R#126) 
      6. Failure: behead() supports custom formatters (@test-behead.R#134) 
      7. Failure: behead() supports custom formatters (@test-behead.R#135) 
      8. Failure: behead() can use row, col and data_type as headers (@test-behead.R#141) 
      9. Failure: behead() can use row, col and data_type as headers (@test-behead.R#144) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

## Newly fixed

*   checking whether package ‘unpivotr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/unpivotr/old/unpivotr.Rcheck/00install.out’ for details.
    ```

# valr

<details>

* Version: 0.5.0
* Source code: https://github.com/cran/valr
* URL: http://github.com/rnabioco/valr, http://rnabioco.github.io/valr
* BugReports: https://github.com/rnabioco/valr/issues
* Date/Publication: 2019-01-03 16:20:04 UTC
* Number of recursive dependencies: 118

Run `revdep_details(,"valr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > 
    > ### ** Examples
    > 
    > genome <- read_genome(valr_example('hg19.chrom.sizes.gz'))
    > 
    > x <- bed_random(genome, seed = 1010486)
    > y <- bed_random(genome, seed = 9203911)
    > 
    > bed_absdist(x, y, genome)
    Error: Join columns must be unique
    ✖ Problem at position 2
    Backtrace:
        █
     1. └─valr::bed_absdist(x, y, genome)
     2.   ├─dplyr::inner_join(genome, ref_points, by = c("chrom", groups_xy))
     3.   └─dplyr:::inner_join.data.frame(...)
     4.     └─dplyr:::join_mutate(...)
     5.       └─dplyr:::join_cols(...)
     6.         └─dplyr:::standardise_join_by(by, x_names = x_names, y_names = y_names)
     7.           └─dplyr:::check_join_vars(by$x, x_names)
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 428 | SKIPPED: 3 | WARNINGS: 27 | FAILED: 14 ]
      1. Error: absdist calculation is correct (@test_absdist.r#22) 
      2. Error: self absdist is 0 (@test_absdist.r#35) 
      3. Error: x ivls without matching y-ivls chroms are reported with absdist = NA (@test_absdist.r#56) 
      4. Error: ensure that absdist is calculated with respect to input tbls issue#108 (@test_absdist.r#87) 
      5. Error: old dataframe groupings (dplyr v. < 0.7.9.900) are tolerated (@test_groups.r#87) 
      6. Failure: unmatched groups are included when invert = TRUE (@test_intersect.r#329) 
      7. Failure: book-ended intervals are not reported (@test_map.r#104) 
      8. Failure: basic partition works (bedops partition1 test) (@test_partition.r#36) 
      9. Failure: extended partition works (bedops partition2 test) (@test_partition.r#119) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

## Newly fixed

*   checking whether package ‘valr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/valr/old/valr.Rcheck/00install.out’ for details.
    ```

# vcfR

<details>

* Version: 1.10.0
* Source code: https://github.com/cran/vcfR
* URL: https://github.com/knausb/vcfR, https://knausb.github.io/vcfR_documentation/
* Date/Publication: 2020-02-06 09:50:02 UTC
* Number of recursive dependencies: 111

Run `revdep_details(,"vcfR")` for more info

</details>

## Newly broken

*   checking whether package ‘vcfR’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/vcfR/new/vcfR.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘vcfR’ ...
** package ‘vcfR’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/vcfR/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c NM2winNM.cpp -o NM2winNM.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/vcfR/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c RcppExports.cpp -o RcppExports.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/vcfR/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c ad_frequency.cpp -o ad_frequency.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/vcfR/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c deprecated_funcs.cpp -o deprecated_funcs.o
In file included from deprecated_funcs.cpp:3:
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/zlib.h:34:
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/zconf.h:462:
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:654:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/gethostuuid.h:39:17: error: C++ requires a type specifier for all declarations
int gethostuuid(uuid_t, const struct timespec *) __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA);
                ^
In file included from deprecated_funcs.cpp:3:
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/zlib.h:34:
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/zconf.h:462:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:661:27: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      getsgroups_np(int *, uuid_t);
                              ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from deprecated_funcs.cpp:3:
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/zlib.h:34:
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/zconf.h:462:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:663:27: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      getwgroups_np(int *, uuid_t);
                              ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from deprecated_funcs.cpp:3:
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/zlib.h:34:
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/zconf.h:462:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:726:31: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      setsgroups_np(int, const uuid_t);
                                  ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from deprecated_funcs.cpp:3:
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/zlib.h:34:
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/zconf.h:462:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:728:31: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      setwgroups_np(int, const uuid_t);
                                  ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
5 errors generated.
make: *** [deprecated_funcs.o] Error 1
ERROR: compilation failed for package ‘vcfR’
* removing ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/vcfR/new/vcfR.Rcheck/vcfR’

```
### CRAN

```
* installing *source* package ‘vcfR’ ...
** package ‘vcfR’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c NM2winNM.cpp -o NM2winNM.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c RcppExports.cpp -o RcppExports.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c ad_frequency.cpp -o ad_frequency.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c deprecated_funcs.cpp -o deprecated_funcs.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c extract_gt.cpp -o extract_gt.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c freq_peak.cpp -o freq_peak.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c gt_to_popsum.cpp -o gt_to_popsum.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c is_het.cpp -o is_het.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c masplit.cpp -o masplit.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c pair_sort.cpp -o pair_sort.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c rank_variants.cpp -o rank_variants.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c read_vcfR.cpp -o read_vcfR.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c seq_to_rects.cpp -o seq_to_rects.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c var_window.cpp -o var_window.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c vcfRCommon.cpp -o vcfRCommon.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/dplyr/old/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -O3 -c write_vcfR.cpp -o write_vcfR.o
clang++ -std=gnu++11 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o vcfR.so NM2winNM.o RcppExports.o ad_frequency.o deprecated_funcs.o extract_gt.o freq_peak.o gt_to_popsum.o is_het.o masplit.o pair_sort.o rank_variants.o read_vcfR.o seq_to_rects.o var_window.o vcfRCommon.o write_vcfR.o -lz -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
installing to /Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/vcfR/old/vcfR.Rcheck/00LOCK-vcfR/00new/vcfR/libs
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Creating a generic function for ‘rbind’ from package ‘base’ in package ‘vcfR’
Creating a generic function for ‘nrow’ from package ‘base’ in package ‘vcfR’
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** checking absolute paths in shared objects and dynamic libraries
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (vcfR)

```
# vpc

<details>

* Version: 1.1.0
* Source code: https://github.com/cran/vpc
* URL: https://github.com/ronkeizer/vpc
* Date/Publication: 2018-08-27 21:00:03 UTC
* Number of recursive dependencies: 70

Run `revdep_details(,"vpc")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘vpc-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: new_vpc_theme
    > ### Title: Create a customized VPC theme
    > ### Aliases: new_vpc_theme
    > 
    > ### ** Examples
    > 
    > theme1 <- new_vpc_theme(update = list(
    +   obs_color = "red",
    +   obs_ci_color = "#aa0000",
    +   obs_alpha = .3,
    +   sim_pi_fill = "#cc8833",
    +   sim_pi_size = 2
    + ))
    > vpc(simple_data$sim, simple_data$obs, vpc_theme = theme1)
    New names:
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-add_sim_index_number.R’ failed.
    Last 13 lines of output:
       1. └─vpc::sim_data(...)
       2.   ├─base::`$<-`(...)
       3.   └─tibble:::`$<-.tbl_df`(...)
       4.     └─tibble:::tbl_subassign(...)
       5.       └─tibble:::vectbl_recycle_rhs(...)
       6.         └─base::tryCatch(...)
       7.           └─base:::tryCatchList(expr, classes, parentenv, handlers)
       8.             └─base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
       9.               └─value[[3L]](cond)
      In addition: Warning message:
      `tbl_df()` is deprecated as of dplyr 1.0.0.
      Please use `tibble::as_tibble()` instead.
      This warning is displayed once every 8 hours.
      Call `lifecycle::last_warnings()` to see where this warning was generated. 
      Execution halted
    ```

## Newly fixed

*   checking whether package ‘vpc’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/vpc/old/vpc.Rcheck/00install.out’ for details.
    ```

# xpose

<details>

* Version: 0.4.8
* Source code: https://github.com/cran/xpose
* URL: https://github.com/UUPharmacometrics/xpose
* BugReports: https://github.com/UUPharmacometrics/xpose/issues
* Date/Publication: 2020-03-17 12:20:02 UTC
* Number of recursive dependencies: 97

Run `revdep_details(,"xpose")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
        <int> <lgl>  <list>                 <lgl>   
    1       1 FALSE  <tibble [550 × 31]>    FALSE   
    2       2 TRUE   <tibble [11,000 × 12]> FALSE   
    > 
    > # List output files data
    > list_files(xpdb_ex_pk)
    Files:
      name       extension problem subprob method data               modified
      <chr>      <chr>       <dbl>   <dbl> <chr>  <list>             <lgl>   
    1 run001.cor cor             1       0 foce   <tibble [14 × 15]> FALSE   
    2 run001.cov cov             1       0 foce   <tibble [14 × 15]> FALSE   
    3 run001.ext ext             1       0 foce   <tibble [28 × 16]> FALSE   
    4 run001.grd grd             1       0 foce   <tibble [21 × 11]> FALSE   
    5 run001.phi phi             1       0 foce   <tibble [74 × 12]> FALSE   
    6 run001.shk shk             1       0 foce   <tibble [7 × 5]>   FALSE   
    > 
    > # List special data
    > xpdb_ex_pk %>% 
    + vpc_data(quiet = TRUE) %>% 
    + list_special()
    New names:
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 416 | SKIPPED: 6 | WARNINGS: 15 | FAILED: 10 ]
      1.  Error: (unknown) (@test-console_outputs.R#4) 
      2.  Error: (unknown) (@test-edits.R#17) 
      3.  Failure: fetch_data can get simple data (@test-fetch_data.R#39) 
      4.  Failure: fetch_data can tidy data (@test-fetch_data.R#49) 
      5.  Failure: fetch_data can get file data (@test-fetch_data.R#58) 
      6.  Failure: properly parses a model given via the file and dir arguments (@test-read_nm_model.R#21) 
      7.  Failure: properly parses a model given via the runno and dir arguments (@test-read_nm_model.R#25) 
      8.  Failure: properly handles missing code in output file (@test-read_nm_model.R#31) 
      9.  Error: (unknown) (@test-vpc.R#18) 
      10. Error: (unknown) (@test-xpdb_access.R#4) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## Newly fixed

*   checking whether package ‘xpose’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/xpose/old/xpose.Rcheck/00install.out’ for details.
    ```

# xrf

<details>

* Version: 0.1.2
* Source code: https://github.com/cran/xrf
* URL: https://github.com/holub008/xrf
* BugReports: https://github.com/holub008/xrf/issues
* Date/Publication: 2019-04-28 08:40:03 UTC
* Number of recursive dependencies: 52

Run `revdep_details(,"xrf")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(testthat)
      > library(xrf)
      > 
      > test_check("xrf")
      ── 1. Failure: non-overlapped rules are unchanged (@test_deoverlap.R#113)  ─────
      `deoverlapped_rules` not equal to data.frame(...).
      Attributes: < Component "class": Lengths (3, 1) differ (string compare on first 1) >
      Attributes: < Component "class": 1 string mismatch >
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 30 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Failure: non-overlapped rules are unchanged (@test_deoverlap.R#113) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## Newly fixed

*   checking whether package ‘xrf’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/xrf/old/xrf.Rcheck/00install.out’ for details.
    ```

# yamlet

<details>

* Version: 0.4.6
* Source code: https://github.com/cran/yamlet
* Date/Publication: 2020-03-14 05:40:02 UTC
* Number of recursive dependencies: 69

Run `revdep_details(,"yamlet")` for more info

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

*   checking whether package ‘yamlet’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/yamlet/old/yamlet.Rcheck/00install.out’ for details.
    ```

