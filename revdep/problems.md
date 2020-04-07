# AlphaBeta

<details>

* Version: 1.0.0
* Source code: https://github.com/cran/AlphaBeta
* Date/Publication: 2019-10-29
* Number of recursive dependencies: 49

Run `revdep_details(,"AlphaBeta")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘AlphaBeta-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: dMatrix
    > ### Title: Constructing D-Matrices
    > ### Aliases: dMatrix
    > 
    > ### ** Examples
    > 
    > ## Get some toy data
    > file <- system.file("extdata","generations.fn", package="AlphaBeta")
    > df<-read.csv(file)
    > df$filename<-sub("^",paste0(dirname(file),"/"),df$filename )
    > write.csv(df, file = paste0(dirname(file),"/tm_generations.fn"),row.names=FALSE,quote=FALSE)
    > file <- system.file("extdata","tm_generations.fn", package="AlphaBeta")
    > dMatrix(file, "CG", 0.99)
    Preparing data-sets...
    Running: 3-26 and 3-87 ( 1 out of 6 pairs ) 
    Error: $ operator is invalid for atomic vectors
    Execution halted
    ```

## In both

*   checking top-level files ... NOTE
    ```
    File
      LICENSE
    is not mentioned in the DESCRIPTION file.
    ```

*   checking R code for possible problems ... NOTE
    ```
    BOOTmodel: multiple local function definitions for ‘divergence’ with
      different formal arguments
    ```

# AMR

<details>

* Version: 1.0.1
* Source code: https://github.com/cran/AMR
* URL: https://msberends.gitlab.io/AMR, https://gitlab.com/msberends/AMR
* BugReports: https://gitlab.com/msberends/AMR/issues
* Date/Publication: 2020-02-23 15:10:06 UTC
* Number of recursive dependencies: 84

Run `revdep_details(,"AMR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > 
    > ### ** Examples
    > 
    > left_join_microorganisms(as.mo("K. pneumoniae"))
    Error: Can't join on `x$mo` x `y$mo` because of incompatible types. 
    ℹ `x$mo` is of type <character>>.
    ℹ `y$mo` is of type <mo>>.
    Backtrace:
         █
      1. └─AMR::left_join_microorganisms(as.mo("K. pneumoniae"))
      2.   ├─base::suppressWarnings(...)
      3.   │ └─base::withCallingHandlers(expr, warning = function(w) invokeRestart("muffleWarning"))
      4.   ├─dplyr::left_join(...)
      5.   └─dplyr:::left_join.data.frame(...)
      6.     └─dplyr:::join_mutate(...)
      7.       └─dplyr:::join_rows(x_key, y_key, type = type, na_equal = na_equal)
      8.         └─base::tryCatch(...)
      9.           └─base:::tryCatchList(expr, classes, parentenv, handlers)
     10.             └─base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
     11.               └─value[[3L]](cond)
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      (Dispersion parameter for binomial family taken to be 1)
      
          Null deviance: 42.518  on 13  degrees of freedom
      Residual deviance: 38.575  on 12  degrees of freedom
      AIC: 93.28
      
      Number of Fisher Scoring iterations: 3
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 284 | SKIPPED: 10 | WARNINGS: 0 | FAILED: 1 ]
      1. Error: joins work (@test-join_microorganisms.R#48) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.5Mb
      sub-directories of 1Mb or more:
        data   4.0Mb
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

# AzureKusto

<details>

* Version: 1.0.5
* Source code: https://github.com/cran/AzureKusto
* URL: https://github.com/Azure/AzureKusto https://github.com/Azure/AzureR
* BugReports: https://github.com/Azure/AzureKusto/issues
* Date/Publication: 2020-01-29 16:40:06 UTC
* Number of recursive dependencies: 66

Run `revdep_details(,"AzureKusto")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 77 | SKIPPED: 7 | WARNINGS: 1 | FAILED: 19 ]
      1. Error: group_by() followed by summarize() generates summarize clause (@test_translate.r#185) 
      2. Error: group_by() followed by summarize() with multiple summarizations generates one summarize clause (@test_translate.r#197) 
      3. Error: group_by() followed by summarize() with multiple summarizations generates one summarize clause in presence of hints (@test_translate.r#210) 
      4. Error: group_by() followed by ungroup() followed by summarize() generates summarize clause (@test_translate.r#223) 
      5. Error: group_by() followed by mutate() partitions the mutation by the grouping variables (@test_translate.r#237) 
      6. Error: inner_join() on a single column translates correctly (@test_translate.r#326) 
      7. Error: inner_join() on two columns translates correctly (@test_translate.r#336) 
      8. Error: inner_join() on one differently named column translates correctly (@test_translate.r#346) 
      9. Error: inner_join() on two differently named columns translates correctly (@test_translate.r#357) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tibble’
      All declared Imports should be used.
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

# BAwiR

<details>

* Version: 1.2.2
* Source code: https://github.com/cran/BAwiR
* URL: https://www.R-project.org, https://www.uv.es/vivigui, https://www.uv.es/vivigui/AppEuroACB.html
* Date/Publication: 2020-03-24 08:50:02 UTC
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
    ℹ Only vectors of size 1 are recycled.
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

# bayesplot

<details>

* Version: 1.7.1
* Source code: https://github.com/cran/bayesplot
* URL: https://mc-stan.org/bayesplot
* BugReports: https://github.com/stan-dev/bayesplot/issues/
* Date/Publication: 2019-12-01 23:00:26 UTC
* Number of recursive dependencies: 142

Run `revdep_details(,"bayesplot")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
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
        R     1.9Mb
        doc   4.1Mb
    ```

# bdl

<details>

* Version: 1.0.2
* Source code: https://github.com/cran/bdl
* URL: https://github.com/statisticspoland/R_Package_to_API_BDL
* BugReports: https://github.com/statisticspoland/R_Package_to_API_BDL/issues
* Date/Publication: 2020-04-01 13:40:03 UTC
* Number of recursive dependencies: 105

Run `revdep_details(,"bdl")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Component 4: Modes: numeric, character
      Component 4: target is numeric, current is character
      Component 5: Mean relative difference: 2.2
      Component 6: Modes: character, numeric
      ...
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 34 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 4 ]
      1. Failure: Proper data (@test-requests.R#72) 
      2. Failure: Proper data (@test-requests.R#127) 
      3. Failure: Proper data (@test-requests.R#179) 
      4. Failure: Proper data (@test-requests.R#244) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# benchmarkfdrData2019

<details>

* Version: 1.0.0
* Source code: https://github.com/cran/benchmarkfdrData2019
* BugReports: https://github.com/stephaniehicks/benchmarkfdrData2019/issues
* Date/Publication: 2019-11-05
* Number of recursive dependencies: 120

Run `revdep_details(,"benchmarkfdrData2019")` for more info

</details>

## Newly broken

*   checking whether package ‘benchmarkfdrData2019’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: `select_()` is deprecated as of dplyr 0.7.0.
      Warning: `filter_()` is deprecated as of dplyr 0.7.0.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/benchmarkfdrData2019/new/benchmarkfdrData2019.Rcheck/00install.out’ for details.
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

*   checking for portable file names ... NOTE
    ```
    Found the following non-portable file paths:
      benchmarkfdrData2019/inst/scripts/make-data/Simulations/simulations-varyinginformativeness-discrete.Rmd
      benchmarkfdrData2019/inst/scripts/make-data/Simulations/simulations-varyinginformativeness-smooth.Rmd
      benchmarkfdrData2019/inst/scripts/make-data/YeastInSilico/yeast-simulation-bimodalalternative-highpi0.Rmd
    
    Tarballs are only required to store paths of up to 100 bytes and cannot
    store those of more than 256 bytes, with restrictions including to 100
    bytes for the final component.
    See section ‘Package structure’ in the ‘Writing R Extensions’ manual.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Package in Depends field not imported from: ‘SummarizedExperiment’
      These packages need to be imported from (in the NAMESPACE file)
      for when this namespace is loaded but not attached.
    ```

# biclustermd

<details>

* Version: 0.2.1
* Source code: https://github.com/cran/biclustermd
* URL: http://github.com/jreisner/biclustermd
* BugReports: http://github.com/jreisner/biclustermd/issues
* Date/Publication: 2020-02-18 05:30:02 UTC
* Number of recursive dependencies: 79

Run `revdep_details(,"biclustermd")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── 2. Failure: row.names() is a subset of gather() (@test-row.names.R#19)  ─────
      row.names(sbc) not equal to gather(sbc) %>% distinct(row_cluster, row_name).
      Names: 2 string mismatches
      Component 1: Modes: numeric, character
      Component 1: target is numeric, current is character
      Component 2: Modes: character, numeric
      Component 2: target is character, current is numeric
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 66 | SKIPPED: 0 | WARNINGS: 1 | FAILED: 2 ]
      1. Failure: col.names() is a subset of gather() (@test-col.names.R#19) 
      2. Failure: row.names() is a subset of gather() (@test-row.names.R#19) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘nycflights13’
      All declared Imports should be used.
    ```

# BiocSet

<details>

* Version: 1.0.1
* Source code: https://github.com/cran/BiocSet
* Date/Publication: 2019-11-06
* Number of recursive dependencies: 140

Run `revdep_details(,"BiocSet")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── 3. Failure: 'summarise.tbl_elementset_base()' works (@test_tbl_elementset_bas
      `es %>% select(element) %>% summarise(element)` did not throw an error.
      
      ── 4. Failure: 'summarise.tbl_elementset_base()' works (@test_tbl_elementset_bas
      `es %>% select(set) %>% summarise(set)` did not throw an error.
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 481 | SKIPPED: 0 | WARNINGS: 2 | FAILED: 4 ]
      1. Failure: 'arrange.BiocSet()' works (@test_BiocSet-methods.R#103) 
      2. Failure: 'summarise.tbl_elementset_base()' works (@test_tbl_elementset_base-class.R#119) 
      3. Failure: 'summarise.tbl_elementset_base()' works (@test_tbl_elementset_base-class.R#125) 
      4. Failure: 'summarise.tbl_elementset_base()' works (@test_tbl_elementset_base-class.R#131) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# biotmle

<details>

* Version: 1.10.0
* Source code: https://github.com/cran/biotmle
* URL: https://code.nimahejazi.org/biotmle
* BugReports: https://github.com/nhejazi/biotmle/issues
* Date/Publication: 2019-10-30
* Number of recursive dependencies: 120

Run `revdep_details(,"biotmle")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(testthat)
      > library(biotmle)
      biotmle v1.10.0: Targeted Learning with Moderated Statistics for Biomarker Discovery
      > 
      > test_check("biotmle")
      ── 1. Failure: biomarkertmle output is consistent using example data (@test-biom
      assay(biomarkerTMLEout)[1, c(17, 83, 117)] not equal to c(360.7073, 375.9316, 319.3649).
      names for target but not for current
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 12 | SKIPPED: 0 | WARNINGS: 3 | FAILED: 1 ]
      1. Failure: biomarkertmle output is consistent using example data (@test-biomarkertmle.R#41) 
      
      Error: testthat unit tests failed
      Execution halted
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

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘sp’
      All declared Imports should be used.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘ggthemes’
    ```

# broom.mixed

<details>

* Version: 0.2.4
* Source code: https://github.com/cran/broom.mixed
* URL: http://github.com/bbolker/broom.mixed
* BugReports: http://github.com/bbolker/broom.mixed/issues
* Date/Publication: 2019-02-21 23:50:03 UTC
* Number of recursive dependencies: 143

Run `revdep_details(,"broom.mixed")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Error: Column name `term` must not be duplicated.
    Backtrace:
         █
      1. ├─generics::tidy(rstan_example, conf.int = TRUE, pars = "theta")
      2. └─broom.mixed:::tidy.stanfit(rstan_example, conf.int = TRUE, pars = "theta")
      3.   ├─fix_data_frame(ret) %>% reorder_cols()
      4.   │ └─base::eval(lhs, parent, parent)
      5.   │   └─base::eval(lhs, parent, parent)
      6.   └─broom::fix_data_frame(ret)
      7.     ├─tibble::as_tibble(ret)
      8.     └─tibble:::as_tibble.data.frame(ret)
      9.       └─tibble:::lst_to_tibble(unclass(x), .rows, .name_repair)
     10.         └─tibble:::set_repaired_names(x, .name_repair)
     11.           ├─rlang::set_names(...)
     12.           └─tibble:::repaired_names(...)
     13.             └─tibble:::subclass_name_repair_errors(...)
     14.               └─base::tryCatch(...)
     15.                 └─base:::tryCatchList(expr, classes, parentenv, handlers)
     16.                   └─base:::tryCatchOne(...)
     17.         
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      y[1]: "1"
      
      x[2]: "sin(2 * pi * Time)"
      y[2]: "2"
      
      x[3]: "cos(2 * pi * Time)"
      y[3]: "3"
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 193 | SKIPPED: 0 | WARNINGS: 7 | FAILED: 2 ]
      1. Error: tidy returns indexes if requested on rstanarm fits (@test-mcmc.R#21) 
      2. Failure: basic gls tidying (@test-nlme.R#146) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Missing or unexported object: ‘dplyr::as.tbl_cube’
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘glmmADMB’
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

# CellBench

<details>

* Version: 1.2.0
* Source code: https://github.com/cran/CellBench
* URL: https://github.com/shians/cellbench
* BugReports: https://github.com/Shians/CellBench/issues
* Date/Publication: 2019-10-29
* Number of recursive dependencies: 109

Run `revdep_details(,"CellBench")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      ── 4. Failure: make_combinations works properly (@test-utils.R#148)  ───────────
      make_combinations(horse = data.frame(x, y), shoe = z) not equal to tibble::tibble(...).
      Component "x": 4 string mismatches
      Component "y": 4 string mismatches
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 97 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 4 ]
      1. Failure: make_combinations works properly (@test-utils.R#121) 
      2. Failure: make_combinations works properly (@test-utils.R#130) 
      3. Failure: make_combinations works properly (@test-utils.R#139) 
      4. Failure: make_combinations works properly (@test-utils.R#148) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        case-study   1.6Mb
        doc          2.3Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    cellbench_case_study: no visible global function definition for
      ‘browseURL’
    Undefined global functions or variables:
      browseURL
    Consider adding
      importFrom("utils", "browseURL")
    to your NAMESPACE file.
    ```

*   checking for unstated dependencies in vignettes ... NOTE
    ```
    '::' or ':::' import not declared from: ‘DrImpute’
    ```

# CellMixS

<details>

* Version: 1.2.5
* Source code: https://github.com/cran/CellMixS
* URL: https://github.com/almutlue/CellMixS
* BugReports: https://github.com/almutlue/CellMixS/issues
* Date/Publication: 2020-03-16
* Number of recursive dependencies: 113

Run `revdep_details(,"CellMixS")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Attaching package: ‘DelayedArray’
    
    The following objects are masked from ‘package:matrixStats’:
    
        colMaxs, colMins, colRanges, rowMaxs, rowMins, rowRanges
    
    The following objects are masked from ‘package:base’:
    
        aperm, apply, rowsum
    
    > sim_list <- readRDS(system.file("extdata/sim50.rds", package = "CellMixS"))
    > sce <- sim_list[[1]][, c(1:15, 300:320, 16:30)]
    > sce_batch1 <- sce[,colData(sce)$batch == "1"]
    > sce_batch2 <- sce[,colData(sce)$batch == "2"]
    > pre <- list("1" = sce_batch1, "2" = sce_batch2)
    > 
    > sce <- evalIntegration(metrics = c("cms", "mixingMetric", "isi", "entropy"), sce, "batch", k = 20)
    Error in knn[["cms"]][cell_id, seq_len(k_smooth)] : 
      subscript out of bounds
    Calls: evalIntegration ... eval -> _fseq -> freduce -> <Anonymous> -> map -> .f
    Execution halted
    ```

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Backtrace:
        1. CellMixS::cms(sce, "batch", k = 20, n_dim = 2)
        2. CellMixS:::.smoothCms(knn, cms_raw, cell_names, k_min, k)
       10. purrr::map(...)
       11. CellMixS:::.f(.x[[i]], ...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 12 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 4 ]
      1. Error: test that output of cms is correct (@test_cms_functions.R#15) 
      2. Error: test that output of evalIntegration is correct (@test_evalIntegration_functions.R#28) 
      3. Error: (unknown) (@test_summary_functions.R#5) 
      4. Error: (unknown) (@test_vis_functions.R#5) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘Seurat’
    ```

# cheese

<details>

* Version: 0.0.3
* Source code: https://github.com/cran/cheese
* URL: https://github.com/zajichek/cheese
* Date/Publication: 2020-02-12 10:50:02 UTC
* Number of recursive dependencies: 96

Run `revdep_details(,"cheese")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > heart_disease %>%
    +     descriptives()
    Error: No common type for `length$.value` <integer> and `count$.value` <table>.
    Backtrace:
         █
      1. ├─heart_disease %>% descriptives()
      2. │ ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      3. │ └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      4. │   └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5. │     └─`_fseq`(`_lhs`)
      6. │       └─magrittr::freduce(value, `_function_list`)
      7. │         ├─base::withVisible(function_list[[k]](value))
      8. │         └─function_list[[k]](value)
      9. │           └─cheese::descriptives(.)
     10. │             └─`%>%`(...)
     11. │               ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
     12. │               └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
     13. │                 └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
     14. │                   └─cheese:::`_fseq`(`_lhs`)
     15. │                     └─magrittr::freduce(value, `_
    Execution halted
    ```

# codebook

<details>

* Version: 0.8.2
* Source code: https://github.com/cran/codebook
* URL: https://github.com/rubenarslan/codebook
* BugReports: https://github.com/rubenarslan/codebook/issues
* Date/Publication: 2020-01-09 16:20:07 UTC
* Number of recursive dependencies: 177

Run `revdep_details(,"codebook")` for more info

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
    Error: Argument 1 must be a data frame or a named atomic vector
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

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘methods’ ‘readr’
      All declared Imports should be used.
    ```

# cogmapr

<details>

* Version: 0.9.1
* Source code: https://github.com/cran/cogmapr
* URL: https://frdvnw.gitlab.io/cogmapr/
* BugReports: https://gitlab.com/FrdVnW/cogmapr/issues
* Date/Publication: 2019-04-11 21:57:07 UTC
* Number of recursive dependencies: 86

Run `revdep_details(,"cogmapr")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > 
      > test_check("cogmapr")
      ── 1. Failure: The functions are reactive to options (@test-cogmapr.R#55)  ─────
      ConceptTest(project, c("Belgium", "Québec"), "indegree", output = "raw.data") inherits from `vctrs_list_of/vctrs_vctr` not `list`.
      
      ── 2. Failure: The functions are reactive to options (@test-cogmapr.R#60)  ─────
      RelationshipTest(...) inherits from `vctrs_list_of/vctrs_vctr` not `list`.
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 20 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 2 ]
      1. Failure: The functions are reactive to options (@test-cogmapr.R#55) 
      2. Failure: The functions are reactive to options (@test-cogmapr.R#60) 
      
      Error: testthat unit tests failed
      Execution halted
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

# collateral

<details>

* Version: 0.4.2
* Source code: https://github.com/cran/collateral
* URL: https://rensa.co/collateral/index.html, https://github.com/rensa/collateral
* BugReports: https://github.com/rensa/collateral/issues
* Date/Publication: 2018-11-19 18:00:23 UTC
* Number of recursive dependencies: 94

Run `revdep_details(,"collateral")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Warning: All elements of `...` must be named.
    Did you want `data = c(car, disp, wt)`?
    Error: Input must be a vector, not a `quietly_mapped` object.
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
      9. │           ├─dplyr::mutate(., qlog = map_quietly(data, ~log(.$wt)))
     10. │           └─dplyr:::mutate.data.frame(., qlog = map_quietly(data, ~log(.$wt)))
     11. │             └─dplyr:::mutate_cols(.data, ...)
     12. │               ├─base::tryCatch(...)
     13. │               │ └─base:::tryCatchList(expr, classes, parentenv, handlers)
     14. │               │   ├─base:::tryCatchOne(...)
     15. │               │   │ └─base:::doTryCatch
    Execution halted
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

*   checking examples ... ERROR
    ```
    ...
    > 
    > to_pairgames(cr_data)
    Error: No common type for `..1` <longcr<>> and `..2` <data.frame<>>.
    Backtrace:
         █
      1. ├─comperes::to_pairgames(cr_data)
      2. │ └─`%>%`(...)
      3. │   ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      4. │   └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5. │     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      6. │       └─comperes:::`_fseq`(`_lhs`)
      7. │         └─magrittr::freduce(value, `_function_list`)
      8. │           └─function_list[[i]](value)
      9. │             ├─tidyr::nest(., data = -.data$game)
     10. │             └─tidyr:::nest.tbl_df(., data = -.data$game)
     11. │               └─vctrs::vec_cbind(u_keys, new_data_frame(out, n = nrow(u_keys)))
     12. └─vctrs::vec_default_ptype2(x = x, y = y, x_arg = x_arg, y_arg = y_arg)
     13.   └─vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
     14.     └─vctrs:::stop_incompatible(...)
     15.       └─vctrs:::stop_vctrs(...)
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── 5. Failure: get_matchups works (@test-utils.R#177)  ─────────────────────────
      `output` not equal to `output_ref`.
      Attributes: < Component "class": Lengths (4, 3) differ (string compare on first 3) >
      Attributes: < Component "class": 3 string mismatches >
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 257 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 5 ]
      1. Error: to_pairgames works (@test-pairgames.R#28) 
      2. Error: to_pairgames handles NA and NaN (@test-pairgames.R#46) 
      3. Error: to_pairgames doesn't change pairgames (@test-pairgames.R#50) 
      4. Failure: as_widecr.default throws an error if no column is matched (@test-results-widecr.R#133) 
      5. Failure: get_matchups works (@test-utils.R#177) 
      
      Error: testthat unit tests failed
      Execution halted
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

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      subscript out of bounds
      Backtrace:
        1. testthat::expect_s4_class(...)
        5. concaveman:::concaveman.SpatialPoints(as(points, "Spatial"))
        6. methods::as(...)
        7. sf:::asMethod(object)
        9. sf::as_Spatial(geom, IDs = row.names(from))
       10. sf:::.as_Spatial(from, cast, IDs)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 1 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Error: input and output formats works correctly (@test_concaveman.R#9) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# crplyr

<details>

* Version: 0.3.3
* Source code: https://github.com/cran/crplyr
* URL: https://crunch.io/r/crplyr/, https://github.com/Crunch-io/crplyr
* BugReports: https://github.com/Crunch-io/crplyr/issues
* Date/Publication: 2020-01-14 09:20:02 UTC
* Number of recursive dependencies: 114

Run `revdep_details(,"crplyr")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 110 | SKIPPED: 23 | WARNINGS: 8 | FAILED: 15 ]
      1. Failure: group_by errors if assigned columns not in dataset (@test-group-by.R#22) 
      2. Failure: group_by errors if assigned columns not in dataset (@test-group-by.R#23) 
      3. Error: group_by hidden variables (@test-group-by.R#29) 
      4. Failure: When group_by calls mutate, it also errors nicely (@test-mutate.R#17) 
      5. Error: summarize makes a cube request (@test-summarize.R#16) 
      6. Error: summarize can handle multiple measures (@test-summarize.R#24) 
      7. Error: summarize after filter (@test-summarize.R#40) 
      8. Error: summarize after filter (@test-summarize.R#3) 
      9. Error: (unknown) (@test-summarize.R#3) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
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

# cursory

<details>

* Version: 1.0.0
* Source code: https://github.com/cran/cursory
* URL: https://github.com/halpo/cursory
* BugReports: https://github.com/halpo/cursory/issues
* Date/Publication: 2019-08-22 08:40:02 UTC
* Number of recursive dependencies: 62

Run `revdep_details(,"cursory")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       24. base:::tryCatchList(expr, names[-nh], parentenv, handlers[-nh])
       27. base:::tryCatchList(expr, names[-nh], parentenv, handlers[-nh])
       28. base:::tryCatchOne(...)
       29. value[[3L]](cond)
       30. dplyr:::stop_mutate_not_vector(index = i, dots = dots, result = e$result)
       31. dplyr:::stop_dplyr(...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 120 | SKIPPED: 0 | WARNINGS: 2 | FAILED: 1 ]
      1. Error: confidence intervals in grouped data frame operations. (@ci.R#126) 
      
      Error: testthat unit tests failed
      In addition: Warning message:
      call dbDisconnect() when finished working with a connection 
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tibble’
      All declared Imports should be used.
    ```

# cutpointr

<details>

* Version: 1.0.1
* Source code: https://github.com/cran/cutpointr
* URL: https://github.com/thie1e/cutpointr
* BugReports: https://github.com/thie1e/cutpointr/issues
* Date/Publication: 2019-12-18 15:00:08 UTC
* Number of recursive dependencies: 74

Run `revdep_details(,"cutpointr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    +   select(optimal_cutpoint, subgroup, AUC, sum_sens_spec, ppv, npv)
    Assuming the positive class is yes
    Assuming the positive class has higher x values
    Error: No common type for `..1` <cutpointr<>> and `..2` <tbl_df<>>.
    Backtrace:
         █
      1. ├─`%>%`(...)
      2. │ ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      3. │ └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      4. │   └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5. │     └─`_fseq`(`_lhs`)
      6. │       └─magrittr::freduce(value, `_function_list`)
      7. │         └─function_list[[i]](value)
      8. │           └─cutpointr::add_metric(., list(ppv, npv))
      9. │             └─dplyr::bind_cols(object, met)
     10. │               └─vctrs::vec_cbind(!!!dots)
     11. └─vctrs::vec_default_ptype2(x = x, y = y, x_arg = x_arg, y_arg = y_arg)
     12.   └─vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
     13.     └─vctrs:::stop_incompatible(...)
     14.       └─vctrs:::stop_vctrs(...)
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 106 | SKIPPED: 0 | WARNINGS: 12 | FAILED: 24 ]
      1. Error: Plotting with bootstrapping is silent (@test-cutpointr.R#100) 
      2. Error: no duplicate column names are returned (@test-cutpointr.R#164) 
      3. Failure: Correct cutpoints with example data (@test-cutpointr.R#239) 
      4. Failure: Correct cutpoints with example data (@test-cutpointr.R#240) 
      5. Error: Metric colnames that are already in cutpointr are modified (@test-cutpointr.R#253) 
      6. Failure: Bootstrap returns plausible results (@test-cutpointr.R#335) 
      7. Failure: Bootstrap returns plausible results (@test-cutpointr.R#337) 
      8. Failure: Bootstrap returns plausible results (@test-cutpointr.R#339) 
      9. Failure: Bootstrap returns plausible results (@test-cutpointr.R#341) 
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

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘RSQLite’ ‘dplyr’
      All declared Imports should be used.
    ```

# dbplyr

<details>

* Version: 1.4.2
* Source code: https://github.com/cran/dbplyr
* URL: https://dbplyr.tidyverse.org/, https://github.com/tidyverse/dbplyr
* BugReports: https://github.com/tidyverse/dbplyr/issues
* Date/Publication: 2019-06-17 20:00:04 UTC
* Number of recursive dependencies: 69

Run `revdep_details(,"dbplyr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
     7 0.0995 0.181
     8 0.108  0.526
     9 0.122  0.724
    10 0.126  0.173
    # … with more rows
    > df %>% arrange(x) %>% show_query()
    <SQL>
    SELECT *
    FROM `dbplyr_005`
    ORDER BY `x`
    > 
    > mtcars_db <- tbl_memdb(mtcars)
    > mtcars_db %>% count(cyl) %>% show_query()
    Warning: The `add` argument of `group_by()` is deprecated as of dplyr 1.0.0.
    Please use the `.add` argument instead.
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_warnings()` to see where this warning was generated.
    Error in UseMethod("group_data") : 
      no applicable method for 'group_data' applied to an object of class "c('tbl_SQLiteConnection', 'tbl_dbi', 'tbl_sql', 'tbl_lazy', 'tbl')"
    Calls: %>% ... <Anonymous> -> <Anonymous> -> group_rows -> group_data
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 636 | SKIPPED: 12 | WARNINGS: 4 | FAILED: 10 ]
      1.  Failure: quoting for rendering ordered grouped table (@test-verb-arrange.R#17) 
      2.  Failure: can copy to from remote sources (@test-verb-copy-to.R#12) 
      3.  Failure: can copy to from remote sources (@test-verb-copy-to.R#18) 
      4.  Error: unnamed results bound together by row (@test-verb-do.R#25) 
      5.  Error: group_by can perform mutate (@test-verb-group_by.R#31) 
      6.  Failure: joining over arbitrary predicates (@test-verb-joins.R#41) 
      7.  Failure: join generates correct sql (@test-verb-joins.R#108) 
      8.  Failure: semi join generates correct sql (@test-verb-joins.R#119) 
      9.  Failure: set ops generates correct sql (@test-verb-joins.R#131) 
      10. Failure: missing columns filled with NULL (@test-verb-set-ops.R#16) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking Rd cross-references ... WARNING
    ```
    Package unavailable to check Rd xrefs: ‘dtplyr’
    Missing link or links in documentation object 'join.tbl_sql.Rd':
      ‘join.tbl_df’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

## Newly fixed

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘dtplyr’
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

# DEGreport

<details>

* Version: 1.22.0
* Source code: https://github.com/cran/DEGreport
* URL: http://lpantano.github.io/DEGreport/
* BugReports: https://github.com/lpantano/DEGreport/issues
* Date/Publication: 2019-10-29
* Number of recursive dependencies: 146

Run `revdep_details(,"DEGreport")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    The following objects are masked from ‘package:matrixStats’:
    
        colMaxs, colMins, colRanges, rowMaxs, rowMins, rowRanges
    
    The following objects are masked from ‘package:base’:
    
        aperm, apply, rowsum
    
    > library(ggplot2)
    > ma <- assays(humanGender)[[1]][1:100,]
    > des <- colData(humanGender)
    > des[["other"]] <- sample(c("a", "b"), 85, replace = TRUE)
    > res <- degPatterns(ma, des, time="group", col = "other")
    Working with 100 genes.
    Warning: `distinct_()` is deprecated as of dplyr 0.7.0.
    Please use `distinct()` instead.
    See vignette('programming') for more help
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_warnings()` to see where this warning was generated.
    Working with 86 genes after filtering: minc > 15
    New names:
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
      [ OK: 48 | SKIPPED: 0 | WARNINGS: 2 | FAILED: 1 ]
      1. Error: (unknown) (@test_cluster.R#13) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .travis.yml
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
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
      cutoff desc enrichGO fdr gene genes itemConsensus k keys lm
      log2FoldChange log2fc logFC max_sd min_median n p.value r ratios
      rowMedians score simplify value_fc value_fdr x xend y yend
    Consider adding
      importFrom("graphics", "boxplot")
      importFrom("stats", "lm")
    to your NAMESPACE file.
    ```

# depmap

<details>

* Version: 1.0.0
* Source code: https://github.com/cran/depmap
* Date/Publication: 2019-11-05
* Number of recursive dependencies: 103

Run `revdep_details(,"depmap")` for more info

</details>

## Newly broken

*   checking whether package ‘depmap’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: `select_()` is deprecated as of dplyr 0.7.0.
      Warning: `filter_()` is deprecated as of dplyr 0.7.0.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/depmap/new/depmap.Rcheck/00install.out’ for details.
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
      [ OK: 2128 | SKIPPED: 0 | WARNINGS: 5 | FAILED: 11 ]
      1. Error: combine_edges (@test-datatable_compatability.R#29) 
      2. Error: Printing a summary line for stored data frames works (@test-print.R#520) 
      3. Error: rescaling node attributes in a graph is possible (@test-rescale_node_edge_attrs.R#17) 
      4. Error: rescaling edge attributes in a graph is possible (@test-rescale_node_edge_attrs.R#88) 
      5. Error: setting DFs as node attributes is possible (@test-set_get_dfs_as_attrs.R#25) 
      6. Error: setting DFs as edge attributes is possible (@test-set_get_dfs_as_attrs.R#115) 
      7. Error: getting DFs as node/edge attributes is possible (@test-set_get_dfs_as_attrs.R#206) 
      8. Error: Getting node attributes with a selection is possible (@test-set_get_node_edge_attrs.R#370) 
      9. Failure: copying values with `trav_in_edge()` works (@test-traversals_copying_attr_vals.R#112) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

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

# disk.frame

<details>

* Version: 0.3.4
* Source code: https://github.com/cran/disk.frame
* URL: https://diskframe.com
* BugReports: https://github.com/xiaodaigh/disk.frame/issues
* Date/Publication: 2020-02-26 14:50:06 UTC
* Number of recursive dependencies: 101

Run `revdep_details(,"disk.frame")` for more info

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

# driftR

<details>

* Version: 1.1.0
* Source code: https://github.com/cran/driftR
* URL: https://github.com/shaughnessyar/driftR
* BugReports: https://github.com/shaughnessyar/driftR/issues
* Date/Publication: 2018-06-13 22:03:03 UTC
* Number of recursive dependencies: 69

Run `revdep_details(,"driftR")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
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

# DuoClustering2018

<details>

* Version: 1.4.0
* Source code: https://github.com/cran/DuoClustering2018
* Date/Publication: 2019-11-05
* Number of recursive dependencies: 151

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

# dupree

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/dupree
* URL: https://github.com/russHyde/dupree
* BugReports: https://github.com/russHyde/dupree/issues
* Date/Publication: 2019-11-10 19:00:02 UTC
* Number of recursive dependencies: 64

Run `revdep_details(,"dupree")` for more info

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
      block with a single code symbol
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 24 | SKIPPED: 1 | WARNINGS: 6 | FAILED: 1 ]
      1. Failure: summarise_enumerated_blocks (@test_dupree_code_enumeration.R#128) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# eda4treeR

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/eda4treeR
* URL: https://github.com/MYaseen208/eda4treeR
* Date/Publication: 2018-02-04 19:06:12 UTC
* Number of recursive dependencies: 106

Run `revdep_details(,"eda4treeR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    +  dplyr::summarize(Mean=mean(Mean))
    Error: Corrupt `grouped_df` using old (< 0.8.0) format
    ℹ Strip off old grouping with `ungroup()`
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
     15.
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

# eia

<details>

* Version: 0.3.4
* Source code: https://github.com/cran/eia
* URL: https://docs.ropensci.org/eia (website) https://github.com/ropensci/eia
* BugReports: https://github.com/ropensci/eia/issues
* Date/Publication: 2019-11-27 12:10:05 UTC
* Number of recursive dependencies: 81

Run `revdep_details(,"eia")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(testthat)
      > library(eia)
      > 
      > test_check("eia")
      ── 1. Failure: eia_report returns as expected (@test-reports.R#10)  ────────────
      names(x$data) not equal to `v`.
      Lengths differ: 9 is not 6
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 36 | SKIPPED: 6 | WARNINGS: 0 | FAILED: 1 ]
      1. Failure: eia_report returns as expected (@test-reports.R#10) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# eph

<details>

* Version: 0.3.0
* Source code: https://github.com/cran/eph
* URL: https://github.com/holatam/eph
* BugReports: https://github.com/holatam/eph/issues
* Date/Publication: 2020-03-08 16:10:02 UTC
* Number of recursive dependencies: 132

Run `revdep_details(,"eph")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    + map_agglomerates(agglomerates = AGLOMERADO,indicator = tasa_actividad)
    Error: All columns in a tibble must be vectors.
    ✖ Column `geometry` is a `sfc_POINT/sfc` object.
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
      9.             └─eph::map_agglomerates(., agglomerates = AGLOMERADO, indicator = tasa_actividad)
     10.               └─`%>%`(...)
     11.                 ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
     12.                 └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
     13.                   └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
     14.                     └─eph:::`_fseq`(`_lhs`)
     15.                       └─magri
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       16. dplyr:::summarise_cols(.data, ...)
       17. base::tryCatch(...)
       18. base:::tryCatchList(expr, classes, parentenv, handlers)
       19. base:::tryCatchOne(...)
       20. value[[3L]](cond)
       21. dplyr:::stop_eval_tidy(e, index = i, dots = dots, fn = "summarise")
       22. dplyr:::stop_dplyr(index, dots, fn, "errored", x = conditionMessage(e))
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 13 | SKIPPED: 3 | WARNINGS: 2 | FAILED: 2 ]
      1. Error: tabla simple (@test-map_agglomerates.R#6) 
      2. Error: consistencia constante (@test-organize_panels.R#6) 
      
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
      Note: found 122 marked UTF-8 strings
    ```

# epikit

<details>

* Version: 0.1.0
* Source code: https://github.com/cran/epikit
* URL: https://github.com/R4EPI/epikit, https://r4epis.netlify.com, https://r4epi.github.io/epikit
* BugReports: https://github.com/R4EPI/epikit/issues
* Date/Publication: 2020-03-05 20:40:02 UTC
* Number of recursive dependencies: 69

Run `revdep_details(,"epikit")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘epikit-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: fmt_count
    > ### Title: Counts and proportions inline
    > ### Aliases: fmt_count
    > 
    > ### ** Examples
    > 
    > 
    > fmt_count(mtcars, cyl > 3, hp < 100)
    Error: `new` must be a tibble
    Backtrace:
        █
     1. └─epikit::fmt_count(mtcars, cyl > 3, hp < 100)
     2.   └─dplyr::count(f)
     3.     └─dplyr::dplyr_reconstruct(out, x)
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── 4. Failure: case_fatality_rate_df will add a total row to stratified analysis
      `iris_res` not equal to `iris_n`.
      Component "Species": Modes: character, numeric
      Component "Species": Attributes: < target is NULL, current is list >
      Component "Species": target is character, current is factor
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 102 | SKIPPED: 1 | WARNINGS: 0 | FAILED: 4 ]
      1. Error: fmt_count() works as expected (@test-inline_fun.R#65) 
      2. Failure: case_fatality_rate_df is equivalent to the non-df version (@test-proportion.R#74) 
      3. Failure: case_fatality_rate_df will add a total row to stratified analysis (@test-proportion.R#152) 
      4. Failure: case_fatality_rate_df will add a total row to stratified analysis and merge CI (@test-proportion.R#173) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘epidict’
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

# estatapi

<details>

* Version: 0.3.0
* Source code: https://github.com/cran/estatapi
* URL: https://github.com/yutannihilation/estatapi
* BugReports: https://github.com/yutannihilation/estatapi/issues
* Date/Publication: 2016-08-11 13:04:42
* Number of recursive dependencies: 46

Run `revdep_details(,"estatapi")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Attributes: < names for target but not for current >
      Attributes: < Length mismatch: comparison on first 0 components >
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 16 | SKIPPED: 12 | WARNINGS: 2 | FAILED: 7 ]
      1. Failure: estat_getStatsData processes the API response as expected 
      2. Failure: test calc_range (@test-calc-range.R#30) 
      3. Failure: test calc_range (@test-calc-range.R#30) 
      4. Failure: test calc_range (@test-calc-range.R#30) 
      5. Failure: test calc_range (@test-calc-range.R#30) 
      6. Failure: test calc_range (@test-calc-range.R#30) 
      7. Failure: test calc_range (@test-calc-range.R#30) 
      
      Error: testthat unit tests failed
      Execution halted
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

# extdplyr

<details>

* Version: 0.1.4
* Source code: https://github.com/cran/extdplyr
* Date/Publication: 2017-02-27 08:15:28
* Number of recursive dependencies: 40

Run `revdep_details(,"extdplyr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    ℹ `pct` is `n()`.
    ℹ The error occured in group 1: agegp = "25-34", alcgp = "0-39g/day".
    ✖ could not find function "n"
    Backtrace:
         █
      1. └─extdplyr::pct_routine(esoph, agegp, alcgp)
      2.   └─extdplyr::pct_routine_(...)
      3.     └─extdplyr::tally_pct_(grouped, wt = wt, ret_name = ret_name, rebase = rebase)
      4.       ├─`%>%`(...)
      5.       │ └─base::eval(lhs, parent, parent)
      6.       │   └─base::eval(lhs, parent, parent)
      7.       ├─dplyr::summarise_(data, .dots = named_expr(ret_name, expr))
      8.       └─dplyr:::summarise_.tbl_df(...)
      9.         ├─dplyr::summarise(.data, !!!dots)
     10.         ├─dplyr:::summarise.grouped_df(.data, !!!dots)
     11.         ├─base::NextMethod()
     12.         └─dplyr:::summarise.data.frame(.data, !!!dots)
     13.           └─dplyr:::summarise_cols(.data, ...)
     14.             └─base::tryCatch(...)
     15.               └─base:::tryCatchList(
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        1. extdplyr::ind_to_char_(...)
        3. dplyr:::group_by_.data.frame(ret, .dots = dplyr::groups(data))
        5. dplyr:::group_by.data.frame(.data, !!!dots, .add = add, .drop = .drop)
        6. dplyr::group_by_prepare(.data, ..., .add = .add)
       11. dplyr::tbl_vars(out)
       15. dplyr:::group_vars.data.frame(x)
       18. dplyr:::group_data.grouped_df(x)
       19. dplyr::validate_grouped_df(.data)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 19 | SKIPPED: 0 | WARNINGS: 2 | FAILED: 1 ]
      1. Error: ind_to_char_ works with grouped_df, tbl_df, tbl, data.frame (@test_grp_routine.R#74) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# ezplot

<details>

* Version: 0.4.0
* Source code: https://github.com/cran/ezplot
* Date/Publication: 2020-03-29 11:00:10 UTC
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

# finalfit

<details>

* Version: 1.0.0
* Source code: https://github.com/cran/finalfit
* URL: https://github.com/ewenharrison/finalfit
* BugReports: https://github.com/ewenharrison/finalfit/issues
* Date/Publication: 2020-02-20 21:30:03 UTC
* Number of recursive dependencies: 112

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

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      age        53.0   61.0        69.0 85.0
      
      Categorical
                 label var_type   n missing_n missing_percent levels_n
      age.factor   Age    <fct> 929         0             0.0        3
                                                  levels levels_count
      age.factor "<40 years", "40-59 years", "60+ years" 70, 344, 515
                   levels_percent
      age.factor  7.5, 37.0, 55.4
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 130 | SKIPPED: 0 | WARNINGS: 16 | FAILED: 1 ]
      1. Error: ff_columns_totals gives data.frame (@test_ffs.R#4) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# fingertipscharts

<details>

* Version: 0.0.10
* Source code: https://github.com/cran/fingertipscharts
* BugReports: https://github.com/PublicHealthEngland/fingertipscharts/issues
* Date/Publication: 2019-10-07 15:00:03 UTC
* Number of recursive dependencies: 142

Run `revdep_details(,"fingertipscharts")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    +                                              -0.15, -0.05, 1.08),
    +                         dps = NA)
    Error: Column 5 must be named.
    Backtrace:
         █
      1. └─fingertipscharts::area_profiles(...)
      2.   └─fingertipscharts:::spine_rescaler(...)
      3.     └─`%>%`(...)
      4.       ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      5.       └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      6.         └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      7.           └─fingertipscharts:::`_fseq`(`_lhs`)
      8.             └─magrittr::freduce(value, `_function_list`)
      9.               └─function_list[[i]](value)
     10.                 └─tibble::rownames_to_column(., var = rlang::quo_text(indicator))
     11.                   └─tibble:::repaired_names(c(unique(names2(df)), var))
     12.                     └─tibble:::subclass_name_repair_errors(...)
     13.                       └─base::tryCatch(...)
     14.                         └─base:::tryCatchList(expr, classes, parentenv, handlers)
     15.                           ├─base:::tryCatchOne(...
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/a-l.R’ failed.
    Last 13 lines of output:
       12. tibble:::subclass_name_repair_errors(...)
       13. base::tryCatch(...)
       14. base:::tryCatchList(expr, classes, parentenv, handlers)
       17. base:::tryCatchList(expr, names[-nh], parentenv, handlers[-nh])
       20. base:::tryCatchList(expr, names[-nh], parentenv, handlers[-nh])
       21. base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
       22. value[[3L]](cond)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 1 | SKIPPED: 15 | WARNINGS: 0 | FAILED: 2 ]
      1. Error: (unknown) (@test-area-profiles.R#25) 
      2. Error: (unknown) (@test-examples.R#141) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘curl’ ‘mapproj’
      All declared Imports should be used.
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
        1. graphics::plot(fssm, what = "fitted")
       31. vctrs:::stop_subscript_oob(...)
       32. vctrs:::stop_subscript(...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 27 | SKIPPED: 14 | WARNINGS: 0 | FAILED: 3 ]
      1. Error: fit_ssm defaults + crw + KF return foieGras list w 15 elements (@test-fit_ssm.R#34) 
      2. Error: (unknown) (@test-join.R#7) 
      3. Error: (unknown) (@test-plot.R#7) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 13.7Mb
      sub-directories of 1Mb or more:
        libs  12.4Mb
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
      [ OK: 22 | SKIPPED: 21 | WARNINGS: 0 | FAILED: 6 ]
      1. Error: lagged_df, training data, grouped with dates is correct (@test_create_lagged_df_grouped.R#43) 
      2. Error: lagged_df, training data, grouped with dates is correct (@test_create_lagged_df_grouped_multi_output.R#46) 
      3. Error: lagged_df, training and forecasting data lookback_control skips groups and static and dynamic features (@test_create_lagged_df_lookback.R#88) 
      4. Error: lagged_df, training data lookback_control appropriately drops lagged features (@test_create_lagged_df_lookback.R#133) 
      5. Error: multi_output, lagged_df, training and forecasting data lookback_control skips groups and static and dynamic features (@test_create_lagged_df_lookback_multi_output.R#88) 
      6. Error: multi_output, lagged_df, training data lookback_control appropriately drops lagged features (@test_create_lagged_df_lookback_multi_output.R#133) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# funrar

<details>

* Version: 1.4.0
* Source code: https://github.com/cran/funrar
* URL: https://rekyt.github.io/funrar/
* BugReports: https://github.com/Rekyt/funrar/issues
* Date/Publication: 2020-03-05 14:50:02 UTC
* Number of recursive dependencies: 76

Run `revdep_details(,"funrar")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 265 | SKIPPED: 0 | WARNINGS: 1 | FAILED: 13 ]
      1. Failure: Correct Di computation with different comm. without abundance (@test-distinctiveness.R#177) 
      2. Failure: Di is undefined for a community with a single species (@test-distinctiveness.R#208) 
      3. Failure: Di is undefined for a community with a single species (@test-distinctiveness.R#215) 
      4. Failure: Relative distinctiveness can be computed (@test-distinctiveness.R#319) 
      5. Failure: Relative distinctiveness can be computed (@test-distinctiveness.R#327) 
      6. Failure: Correct Scarcity computation (@test-scarcity.R#118) 
      7. Failure: Correct Scarcity computation (@test-scarcity.R#125) 
      8. Failure: Conversion from matrix to tidy data.frame works (@test-tidy_matrix.R#75) 
      9. Failure: Conversion from matrix to tidy data.frame works (@test-tidy_matrix.R#78) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
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
    > xtractor$ids
    [1] "setosa"     "versicolor" "virginica" 
    > fun = function(data) {
    +   c(mean_sepal_length = mean(data$Sepal.Length))
    + }
    > xtractor$add_feature(fun)
    > xtractor$features
    [1] "fun"
    > xtractor$calc_features()
    Error: No common type for `..1$Species` <logical> and `..3$Species` <character>.
    Backtrace:
        █
     1. ├─xtractor$calc_features()
     2. ├─(function () ...
     3. │ └─dplyr::bind_rows(done_df, error_df, not_done_df)
     4. │   └─vctrs::vec_rbind(!!!dots, .names_to = .id)
     5. └─vctrs::vec_default_ptype2(x = x, y = y, x_arg = x_arg, y_arg = y_arg)
     6.   └─vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
     7.     └─vctrs:::stop_incompatible(...)
     8.       └─vctrs:::stop_vctrs(...)
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

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘ggplot2’ ‘maptools’ ‘rgeos’ ‘stringr’ ‘tidyr’
      All declared Imports should be used.
    ```

# gemini

<details>

* Version: 1.0.0
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

## In both

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .github
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
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

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘magrittr’
      All declared Imports should be used.
    ```

# ggformula

<details>

* Version: 0.9.4
* Source code: https://github.com/cran/ggformula
* URL: https://github.com/ProjectMOSAIC/ggformula
* BugReports: https://github.com/ProjectMOSAIC/ggformula/issues
* Date/Publication: 2020-03-04 09:40:08 UTC
* Number of recursive dependencies: 180

Run `revdep_details(,"ggformula")` for more info

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
      7.           ├─base::withVisible(function_list[[k]](value))
      8.           └─function_list[[k]](value)
      9.             ├─dplyr::filter(., city == "Chicago", year == 2016, month <= 4)
     10.             └─dplyr:::filter.data.frame(...)
     11.               └─dplyr:::filter_rows(.data, ...)
     12.                 └─DataMask$new(.data, caller_env())
     13.                   └─.subset2(public_bind_env, "initialize")(...)
     14.                     └─dplyr::group_rows(data)
     15.                       ├─dplyr::group_data(.data)
     16.             
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Backtrace:
        1. ggformula::gf_linerange(...)
        5. ggplot2:::fortify.grouped_df(data)
        7. dplyr:::group_indices.data.frame(model)
        8. dplyr::group_rows(.data)
       10. dplyr:::group_data.grouped_df(.data)
       11. dplyr::validate_grouped_df(.data)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 0 | SKIPPED: 110 | WARNINGS: 4 | FAILED: 2 ]
      1. Error: gf_area() & gf_ribbon() (@test-layer-factory.R#54) 
      2. Error: gf_linerange() and gf_pointrange() 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘quantreg’
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
      1. ├─graphics::plot(gg_dta)
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
     16. │         ├─generics::setdiff(names(group_data(x)), ".r
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘randomForest’
      All declared Imports should be used.
    ```

# ggspatial

<details>

* Version: 1.0.3
* Source code: https://github.com/cran/ggspatial
* URL: https://github.com/paleolimbot/ggspatial
* BugReports: https://github.com/paleolimbot/ggspatial/issues
* Date/Publication: 2018-12-14 21:10:04 UTC
* Number of recursive dependencies: 96

Run `revdep_details(,"ggspatial")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        error: object 'as.tbl_cube' not found
      Backtrace:
       1. stars::read_stars
       2. base::getExportedValue(pkg, name)
       3. base::asNamespace(ns)
       4. base::getNamespace(ns)
       5. base::loadNamespace(name)
       6. base:::runHook(".onLoad", env, package.lib, package)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 121 | SKIPPED: 0 | WARNINGS: 1 | FAILED: 1 ]
      1. Error: stars objects are converted properly by df_spatial (@test-df_spatial.R#123) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘reshape2’ ‘rosm’
      All declared Imports should be used.
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
    Error in check_spct.generic_spct(x) : 
      'w.length' must be sorted and have unique values
    Calls: autoplot ... setGenericSpct -> check_spct -> check_spct.generic_spct
    Execution halted
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

# gtsummary

<details>

* Version: 1.2.6
* Source code: https://github.com/cran/gtsummary
* URL: https://github.com/ddsjoberg/gtsummary, http://www.danieldsjoberg.com/gtsummary/
* BugReports: https://github.com/ddsjoberg/gtsummary/issues
* Date/Publication: 2020-02-13 14:40:05 UTC
* Number of recursive dependencies: 141

Run `revdep_details(,"gtsummary")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ``%>%`(...)` threw an error.
      Message: object 'Time' not found
      Class:   simpleError/error/condition
      Backtrace:
       1. stats::model.frame(...)
       2. stats::model.frame.default(...)
       3. [ base::eval(...) ] with 1 more call
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 343 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 2 ]
      1. Failure: combine_terms works without error (@test-combine_terms.R#55) 
      2. Failure: combine_terms works without error (@test-combine_terms.R#97) 
      
      Error: testthat unit tests failed
      Execution halted
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

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘DT’ ‘gsubfn’ ‘stringr’
      All declared Imports should be used.
    ```

# heemod

<details>

* Version: 0.11.0
* Source code: https://github.com/cran/heemod
* BugReports: https://github.com/pierucci/heemod/issues
* Date/Publication: 2019-10-22 08:40:05 UTC
* Number of recursive dependencies: 100

Run `revdep_details(,"heemod")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > 
    > define_dsa(
    +   a, 10, 45,
    +   b, .5, 1.5
    + )
    Error: All columns in a tibble must be vectors.
    ✖ Column `dots[i]` is a `lazy_dots` object.
    Backtrace:
         █
      1. └─heemod::define_dsa(a, 10, 45, b, 0.5, 1.5)
      2.   └─heemod:::define_dsa_(...)
      3.     ├─base::suppressWarnings(...)
      4.     │ └─base::withCallingHandlers(expr, warning = function(w) invokeRestart("muffleWarning"))
      5.     ├─dplyr::bind_rows(...)
      6.     │ └─rlang::list2(...)
      7.     ├─stats::setNames(tibble::tibble(dots[i]), names(dots)[i])
      8.     └─tibble::tibble(dots[i])
      9.       └─tibble:::tibble_quos(xs[!is_null], .rows, .name_repair)
     10.         └─tibble:::check_valid_col(res, col_names[[j]], j)
     11.           └─tibble:::check_valid_cols(set_names(list(x), name))
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 468 | SKIPPED: 0 | WARNINGS: 2 | FAILED: 14 ]
      1. Failure: Exactly match THR model (@test_dmhee.R#291) 
      2. Failure: Exactly match THR model (@test_dmhee.R#307) 
      3. Error: Same results using 1 core or 2. (@test_parallel.R#7) 
      4. Failure: Parameter evaluation (@test_parameters.R#81) 
      5. Error: we can run construct_part_surv_tib (@test_part_surv.R#298) 
      6. Error: define sensitivity (@test_sensitivity.R#5) 
      7. Error: run sensitivity (@test_sensitivity.R#101) 
      8. Error: discount rate as a parameter works (@test_sensitivity.R#173) 
      9. Error: sensitivity expression inputs (@test_sensitivity.R#236) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

# HMP16SData

<details>

* Version: 1.6.0
* Source code: https://github.com/cran/HMP16SData
* URL: https://github.com/waldronlab/HMP16SData
* BugReports: https://github.com/waldronlab/HMP16SData/issues
* Date/Publication: 2019-11-05
* Number of recursive dependencies: 184

Run `revdep_details(,"HMP16SData")` for more info

</details>

## Newly broken

*   checking whether package ‘HMP16SData’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: `select_()` is deprecated as of dplyr 0.7.0.
      Warning: `filter_()` is deprecated as of dplyr 0.7.0.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/HMP16SData/new/HMP16SData.Rcheck/00install.out’ for details.
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

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘curatedMetagenomicData’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 19.1Mb
      sub-directories of 1Mb or more:
        doc       1.5Mb
        extdata  17.4Mb
    ```

# holodeck

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/holodeck
* URL: https://github.com/Aariq/holodeck
* BugReports: https://github.com/Aariq/holodeck/issues
* Date/Publication: 2019-04-16 12:12:40 UTC
* Number of recursive dependencies: 108

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

# incadata

<details>

* Version: 0.8.2
* Source code: https://github.com/cran/incadata
* URL: https://cancercentrum.bitbucket.io/incadata
* BugReports: https://www.bitbucket.org/cancercentrum/incadata/issues
* Date/Publication: 2019-05-05 20:30:04 UTC
* Number of recursive dependencies: 67

Run `revdep_details(,"incadata")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      The following object is masked from 'package:stats':
      
          filter
      
      > 
      > test_check("incadata")
      ── 1. Failure: filter (@test-dplyr_methods.R#13)  ──────────────────────────────
      dplyr::filter(testdata, persnr == 191212121212)$persnr inherits from `AsIs` not `pin`.
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 109 | SKIPPED: 5 | WARNINGS: 0 | FAILED: 1 ]
      1. Failure: filter (@test-dplyr_methods.R#13) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# IncucyteDRC

<details>

* Version: 0.5.4
* Source code: https://github.com/cran/IncucyteDRC
* URL: https://github.com/chapmandu2/IncucyteDRC
* BugReports: https://github.com/chapmandu2/IncucyteDRC/issues
* Date/Publication: 2016-04-23 14:21:03
* Number of recursive dependencies: 118

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
       6. base:::tryCatchList(expr, classes, parentenv, handlers)
       7. base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
       8. value[[3L]](cond)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 473 | SKIPPED: 0 | WARNINGS: 10 | FAILED: 6 ]
      1. Error: (unknown) (@test_calc_deriv.R#6) 
      2. Error: (unknown) (@test_cond_boot.R#112) 
      3. Error: (unknown) (@test_model_gamm.R#4) 
      4. Failure: structure of returned object (@test_plot_diagnostics.R#19) 
      5. Failure: structure of returned object (@test_plot_diagnostics.R#20) 
      6. Error: (unknown) (@test_scoring.R#15) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        data   3.1Mb
        help   1.6Mb
    ```

# infer

<details>

* Version: 0.5.1
* Source code: https://github.com/cran/infer
* URL: https://github.com/tidymodels/infer, https://infer.netlify.com/
* BugReports: https://github.com/tidymodels/infer/issues
* Date/Publication: 2019-11-19 10:30:02 UTC
* Number of recursive dependencies: 115

Run `revdep_details(,"infer")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
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

# InjurySeverityScore

<details>

* Version: 0.0.0.2
* Source code: https://github.com/cran/InjurySeverityScore
* URL: https://github.com/dajuntian/InjurySeverityScore
* BugReports: https://github.com/dajuntian/InjurySeverityScore/issues
* Date/Publication: 2019-05-19 04:11:18 UTC
* Number of recursive dependencies: 24

Run `revdep_details(,"InjurySeverityScore")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### ** Examples
    > 
    > pat_id <- c(2,2,2,2,2,1,2,1,2,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1)
    > icd9 <- c('874.2', '874.8', '900.81', '900.82', '900.89', '805.06', 
    +           'E966', '805.07', 'V14.0', '807.02', 'V70.4', '821.01', '823.20', 
    +           '860.0', '861.01', '861.21', '861.22', '863.84', '864.04', '865.04', 
    +           '865.09', '866.02', '868.04', '958.4')
    > sample_data <- data.frame(subj = pat_id, code = icd9, stringsAsFactors = FALSE)
    > injury_score(sample_data, subj, code)
    Warning in max(severity) :
      no non-missing arguments to max; returning -Inf
    Error: Argument 2 must be an integer vector, not a double vector
    Backtrace:
        █
     1. └─InjurySeverityScore::injury_score(sample_data, subj, code)
     2.   ├─base::cbind(...)
     3.   └─dplyr::coalesce(iss_br$max_wo_9, iss_br$max_w_9, as.integer(iss_br$severity_default))
     4.     └─dplyr:::replace_with(...)
     5.       └─dplyr:::check_type(val, x, name)
     6.         └─dplyr:::glubort(header, "must be {friendly_type_of(template)}, not {friendly_type_of(x)}")
    Execution halted
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
      ── 1. Error: mismatched attributes in bind rows (@test_ipums_bind_rows.r#39)  ──
      No common type for `..1$x` <haven_labelled> and `..2$x` <haven_labelled>.
      Backtrace:
        1. testthat::expect_warning(bound <- ipums_bind_rows(test1, test2))
        9. vctrs::vec_default_ptype2(x = x, y = y, x_arg = x_arg, y_arg = y_arg)
       10. vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
       11. vctrs:::stop_incompatible(...)
       12. vctrs:::stop_vctrs(...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 211 | SKIPPED: 11 | WARNINGS: 0 | FAILED: 1 ]
      1. Error: mismatched attributes in bind rows (@test_ipums_bind_rows.r#39) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# isomiRs

<details>

* Version: 1.14.0
* Source code: https://github.com/cran/isomiRs
* BugReports: https://github.com/lpantano/isomiRs/issues
* Date/Publication: 2019-10-29
* Number of recursive dependencies: 161

Run `revdep_details(,"isomiRs")` for more info

</details>

## Newly broken

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

## In both

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
      as.tibble hits iso_sample pct targetscan.Hs.egMIRBASE2FAMILY
      targetscan.Hs.egMIRNA targetscan.Hs.egTARGETS
      targetscan.Hs.egTARGETSFULL targetscan.Mm.egMIRBASE2FAMILY
      targetscan.Mm.egMIRNA targetscan.Mm.egTARGETS
      targetscan.Mm.egTARGETSFULL total
    ```

# janitor

<details>

* Version: 1.2.1
* Source code: https://github.com/cran/janitor
* URL: https://github.com/sfirke/janitor
* BugReports: https://github.com/sfirke/janitor/issues
* Date/Publication: 2020-01-22 19:20:02 UTC
* Number of recursive dependencies: 59

Run `revdep_details(,"janitor")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    +   adorn_totals("row") %>%
    +   adorn_percentages()
    Error: Assigned data `name` must be compatible with existing data.
    ℹ Error occurred for column `am`.
    ✖ No common type for <character> and <double>.
    Backtrace:
         █
      1. └─mtcars %>% tabyl(am, cyl) %>% adorn_totals("row") %>% adorn_percentages()
      2.   ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      3.   └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      4.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5.       └─`_fseq`(`_lhs`)
      6.         └─magrittr::freduce(value, `_function_list`)
      7.           └─function_list[[i]](value)
      8.             └─janitor::adorn_totals(., "row")
      9.               ├─base::`[<-`(`*tmp*`, 1, 1, value = "Total")
     10.               └─tibble:::`[<-.tbl_df`(`*tmp*`, 1, 1, value = "Total")
     11.                 └─tibble:::tbl_subassign(x, i, j, value, i_arg, j_arg, substitute(value))
     12.                   └─tibble:::tbl_subassign_row(xj, i, value, value_arg)
     13.                     └
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 520 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 15 ]
      1. Error: grouped_df gets ungrouped and succeeds (@test-add-totals.R#122) 
      2. Error: na.rm value works correctly (@test-add-totals.R#129) 
      3. Error: add_totals respects if input was data.frame (@test-add-totals.R#141) 
      4. Error: add_totals respects if input was data_frame (@test-add-totals.R#148) 
      5. Error: works with non-numeric columns mixed in; fill character specification (@test-add-totals.R#192) 
      6. Error: automatically invokes purrr::map when called on a 3-way tabyl (@test-add-totals.R#251) 
      7. Error: deprecated functions adorn_totals_col and adorn_totals_row function as expected (@test-add-totals.R#283) 
      8. Failure: show_n can suppress Ns, digits parameter is correct (@test-adorn-crosstab.R#78) 
      9. Error: (unknown) (@test-adorn-percentages.R#44) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
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

*   checking examples ... ERROR
    ```
    ...
    Processing files for book_chapter with functions jst_get_book
    Error: No common type for `..1` <jstor_import_spec<>> and `..2` <tbl_df<>>.
    Backtrace:
         █
      1. ├─jstor::jst_import_zip(...)
      2. │ └─`%>%`(...)
      3. │   ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      4. │   └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5. │     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      6. │       └─jstor:::`_fseq`(`_lhs`)
      7. │         └─magrittr::freduce(value, `_function_list`)
      8. │           ├─base::withVisible(function_list[[k]](value))
      9. │           └─function_list[[k]](value)
     10. │             └─purrr::walk(...)
     11. │               └─purrr::map(.x, .f, ...)
     12. │                 └─jstor:::.f(.x[[i]], ...)
     13. │                   └─`%>%`(...)
     14. │                     ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
     15. │                     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
     16. │                       └
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 232 | SKIPPED: 4 | WARNINGS: 1 | FAILED: 16 ]
      1. Failure: journal id is unified (@test-augment.R#75) 
      2. Error: subsetting ngrams works (@test-ngram.R#32) 
      3. Failure: files with column names can be re-read (@test-re-import.R#212) 
      4. Failure: files with column names can be re-read (@test-re-import.R#216) 
      5. Failure: files with column names can be re-read (@test-re-import.R#220) 
      6. Failure: files with column names can be re-read (@test-re-import.R#224) 
      7. Failure: files with column names can be re-read (@test-re-import.R#228) 
      8. Failure: files without column names can be re-read (@test-re-import.R#244) 
      9. Failure: files without column names can be re-read (@test-re-import.R#248) 
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
      Component 3: Mean relative difference: 1.015697
      Component 4: Mean relative difference: 12.94108
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 142 | SKIPPED: 0 | WARNINGS: 7 | FAILED: 2 ]
      1. Failure: test that it is possible to load LANS maps (@test-load-data.R#81) 
      2. Failure: test that transformation safety checks are in place (@test-transformations.R#13) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# LexisNexisTools

<details>

* Version: 0.3.0
* Source code: https://github.com/cran/LexisNexisTools
* URL: https://github.com/JBGruber/LexisNexisTools
* BugReports: https://github.com/JBGruber/LexisNexisTools/issues
* Date/Publication: 2020-01-09 23:00:03 UTC
* Number of recursive dependencies: 127

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

# mason

<details>

* Version: 0.2.6
* Source code: https://github.com/cran/mason
* URL: https://github.com/lwjohnst86/mason
* BugReports: https://github.com/lwjohnst86/mason/issues
* Date/Publication: 2018-07-05 12:20:02 UTC
* Number of recursive dependencies: 67

Run `revdep_details(,"mason")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
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

# metamicrobiomeR

<details>

* Version: 1.1
* Source code: https://github.com/cran/metamicrobiomeR
* URL: https://github.com/nhanhocu/metamicrobiomeR
* BugReports: https://github.com/nhanhocu/metamicrobiomeR/issues
* Date/Publication: 2019-09-03 07:20:02 UTC
* Number of recursive dependencies: 130

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

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘RCurl’ ‘foreign’ ‘gplots’ ‘httr’ ‘jsonlite’ ‘knitr’ ‘lmerTest’
      ‘magrittr’ ‘mgcv’ ‘repmis’ ‘reshape2’ ‘rmarkdown’
      All declared Imports should be used.
    ```

# MIAmaxent

<details>

* Version: 1.1.0
* Source code: https://github.com/cran/MIAmaxent
* URL: https://github.com/julienvollering/MIAmaxent
* BugReports: https://github.com/julienvollering/MIAmaxent/issues
* Date/Publication: 2019-05-30 21:20:04 UTC
* Number of recursive dependencies: 47

Run `revdep_details(,"MIAmaxent")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    ℹ The error occured in group 1: int = "(-0.001,0.25]".
    ✖ could not find function "n"
    Backtrace:
         █
      1. └─MIAmaxent::deriveVars(...)
      2.   └─MIAmaxent:::.dvsfromev(...)
      3.     └─MIAmaxent:::.transfD(ev, rv, i)
      4.       └─MIAmaxent:::.fopoptimum(data.frame(rv, xnull))
      5.         ├─base::as.data.frame(...)
      6.         ├─dplyr::summarise(...)
      7.         ├─dplyr:::summarise.grouped_df(...)
      8.         ├─base::NextMethod()
      9.         └─dplyr:::summarise.data.frame(...)
     10.           └─dplyr:::summarise_cols(.data, ...)
     11.             └─base::tryCatch(...)
     12.               └─base:::tryCatchList(expr, classes, parentenv, handlers)
     13.                 └─base:::tryCatchOne(...)
     14.                   └─value[[3L]](cond)
     15.                     └─dplyr:::stop_eval_tidy(e, index = i, dots = dots, fn = "summarise")
     16.                       └─
    Execution halted
    ```

# microbiome

<details>

* Version: 1.8.0
* Source code: https://github.com/cran/microbiome
* URL: http://microbiome.github.io/microbiome
* BugReports: https://github.com/microbiome/microbiome/issues
* Date/Publication: 2019-10-29
* Number of recursive dependencies: 108

Run `revdep_details(,"microbiome")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > p <- boxplot_abundance(peerj32$phyloseq, x='time', y='Akkermansia',
    +    line='subject')
    Error: arrange() failed at implicit mutate() step. 
    ✖ invalid class “sample_data” object: Sample Data must have non-zero dimensions.
    Backtrace:
         █
      1. └─microbiome::boxplot_abundance(...)
      2.   ├─base::suppressWarnings(...)
      3.   │ └─base::withCallingHandlers(expr, warning = function(w) invokeRestart("muffleWarning"))
      4.   └─df %>% arrange(linevar, xvar) %>% group_by(linevar) %>% summarise(change = diff(yvar))
      5.     ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      6.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      7.       └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      8.         └─microbiome:::`_fseq`(`_lhs`)
      9.           └─magrittr::freduce(value, `_function_list`)
     10.             └─function_list[[i]](value)
     11.               ├─dplyr::arrange(., linevar, xvar)
     12.               └─dplyr:::arrange.data.frame(., linevar, xvar)
     13.                 └─dplyr:::arrange_rows(.data, dots)
     14. 
    Execution halted
    ```

## Newly fixed

*   checking examples ... WARNING
    ```
    Found the following significant warnings:
    
      Warning: 'noncore_abundance' is deprecated.
    Deprecated functions may be defunct as soon as of the next release of
    R.
    See ?Deprecated.
    ```

## In both

*   checking Rd \usage sections ... WARNING
    ```
    Documented arguments not in \usage in documentation object 'core':
      ‘...’
    
    Documented arguments not in \usage in documentation object 'rare':
      ‘...’
    
    Functions with \usage entries need to have the appropriate \alias
    entries, and all their arguments documented.
    The \usage entries must correspond to syntactically valid R code.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking data for ASCII and uncompressed saves ... WARNING
    ```
      
      Note: significantly better compression could be obtained
            by using R CMD build --resave-data
                           old_size new_size compress
      atlas1006.rda           233Kb    128Kb       xz
      dietswap.rda             45Kb     28Kb       xz
      hitchip.taxonomy.rda    402Kb    115Kb       xz
      peerj32.rda             113Kb     87Kb       xz
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

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘magrittr’ ‘stringr’
      All declared Imports should be used.
    ```

# Momocs

<details>

* Version: 1.2.9
* Source code: https://github.com/cran/Momocs
* URL: https://github.com/MomX/Momocs/
* BugReports: https://github.com/MomX/Momocs/issues
* Date/Publication: 2018-03-22 22:25:52 UTC
* Number of recursive dependencies: 120

Run `revdep_details(,"Momocs")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        2. Momocs::LDA(., 1)
       10. Momocs::as_df(.)
       15. dplyr::bind_cols(data.frame(f = x$fac), as.data.frame(x$x))
       16. vctrs::vec_cbind(!!!dots)
      
       * Extracting  1 ..txt coordinates...
       * Extracting  12 ..txt coordinates...
      [ 1 / 2 ]  beer_chimay.jpg
      [ 2 / 2 ]  whisky_jb.jpg
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 173 | SKIPPED: 0 | WARNINGS: 3 | FAILED: 1 ]
      1. Error: as_df converts all classes to data.frames (@test-babel-bridges.R#54) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking Rd \usage sections ... WARNING
    ```
    Documented arguments not in \usage in documentation object 'calibrate_reconstructions':
      ‘...’
    
    Documented arguments not in \usage in documentation object 'import_jpg1':
      ‘...’
    
    Functions with \usage entries need to have the appropriate \alias
    entries, and all their arguments documented.
    The \usage entries must correspond to syntactically valid R code.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
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
    > ### Aliases: confint confint.numeric confint.do.tbl_df
    > ###   confint.do.data.frame confint.data.frame confint.summary.lm
    > 
    > ### ** Examples
    > 
    > if (require(mosaicData)) {
    +   bootstrap <- do(500) * diffmean( age ~ sex, data = resample(HELPrct) )
    +   confint(bootstrap)
    +   confint(bootstrap, method = "percentile")
    +   confint(bootstrap, method = "boot")
    +   confint(bootstrap, method = "se", df = nrow(HELPrct) - 1)
    +   confint(bootstrap, margin.of.error = FALSE)
    +   confint(bootstrap, margin.of.error = TRUE, level = 0.99, 
    +     method = c("se", "perc") )
    +     
    +   # bootstrap t method requires both mean and sd
    +   bootstrap2 <- do(500) * favstats(resample(1:10)) 
    +   confint(bootstrap2, method = "boot")
    + }
    Warning: confint: Unable to compute any of the desired CIs
    New names:
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘fastR’
    
    Package which this enhances but not available for checking: ‘manipulate’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 12.2Mb
      sub-directories of 1Mb or more:
        R     2.6Mb
        doc   9.0Mb
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘cubature’
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

* Version: 1.4.5
* Source code: https://github.com/cran/MSstatsTMT
* URL: http://msstats.org/msstatstmt/
* BugReports: https://groups.google.com/forum/#!forum/msstats
* Date/Publication: 2020-03-01
* Number of recursive dependencies: 98

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
      > test_check("MSstatsTMT")
      ── 1. Error: proteinSummarization works (@test-proteinSummarization.R#5)  ──────
      j (the 2nd argument inside [...]) is a single symbol but column name 'require.col' is not found. Perhaps you intended DT[, ..require.col]. This difference to data.frame is deliberate and explained in FAQ 1.1.
      Backtrace:
       1. MSstatsTMT::proteinSummarization(...)
       2. MSstatsTMT:::.protein.summarization.function(...)
       3. MSstats::dataProcess(...)
       5. data.table:::`[.data.table`(raw, , require.col)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 32 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Error: proteinSummarization works (@test-proteinSummarization.R#5) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# muscData

<details>

* Version: 1.0.0
* Source code: https://github.com/cran/muscData
* URL: https://github.com/HelenaLC/muscData
* BugReports: https://github.com/HelenaLC/muscData/issues
* Date/Publication: 2019-11-05
* Number of recursive dependencies: 95

Run `revdep_details(,"muscData")` for more info

</details>

## Newly broken

*   checking whether package ‘muscData’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: `select_()` is deprecated as of dplyr 0.7.0.
      Warning: `filter_()` is deprecated as of dplyr 0.7.0.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/muscData/new/muscData.Rcheck/00install.out’ for details.
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

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .BBSoptions
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Package in Depends field not imported from: ‘SingleCellExperiment’
      These packages need to be imported from (in the NAMESPACE file)
      for when this namespace is loaded but not attached.
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

*   checking examples ... ERROR
    ```
    ...
    +   add_shadow(Ozone, Solar.R) %>%
    +   add_label_shadow()
    Error: No common type for `..1` <data.frame<>> and `..2` <shadow<>>.
    Backtrace:
         █
      1. ├─airquality %>% add_shadow(Ozone, Solar.R) %>% add_label_shadow()
      2. │ ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      3. │ └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      4. │   └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5. │     └─`_fseq`(`_lhs`)
      6. │       └─magrittr::freduce(value, `_function_list`)
      7. │         └─function_list[[i]](value)
      8. │           └─naniar::add_shadow(., Ozone, Solar.R)
      9. │             ├─dplyr::bind_cols(data, shadow_df) %>% dplyr::as_tibble()
     10. │             │ └─base::eval(lhs, parent, parent)
     11. │             │   └─base::eval(lhs, parent, parent)
     12. │             └─dplyr::bind_cols(data, shadow_df)
     13. │               └─vctrs::vec_cbind(!!!dots)
     14. └─vctrs::vec_default_ptype2(x = x, y = y, x_arg = x_arg, y_arg = y_arg)
     15.   └─vctrs:
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        5.         ├─testthat::with_reporter(...)
        6.         │ ├─base::withRestarts(...)
        7.         │ │ └─base:::withOneRestart(expr, restarts[[1L]])
        8.         │ │   └─base:::doWithOneRestart(return(expr), restart)
        9.         │ └─base::force(code)
       10.         └─base::lapply(...)
       11.           └─testthat:::FUN(X[[i]], ...)
       12.             ├─testthat::with_reporter(...)
       13.             │ ├─base::withRestarts(...)
       14.             │ │ └─base:::withOneRestart(expr, restarts[[1L]])
       15.             │ │   └─base:::doWithOneRestart(return(expr), restart)
       16.             │ └─base::force(code)
       17.             └─testthat::source_file(...)
       18.               └─testthat:::t
      Execution halted
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
      symbol: character
      label : character
      guide : character
    >>.
    Backtrace:
         █
      1. ├─1001 %>% meta
      2. │ ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      3. │ └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      4. │   └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5. │     └─`_fseq`(`_lhs`)
      6. │       └─magrittr::freduce(value, `_function_list`)
      7. │         ├─base::withVisible(function_list[[k]](value))
      8. │         └─function_list[[k]](value)
      9. │           ├─nonmemica::meta(.)
     10. │           └─nonmemica:::meta.numeric(.)
     11. │             ├─nonmemica::meta(as.character(x), ...)
     12. │             └─nonmemica:::meta.character(as.character(x), ...)
     13. │               └─dplyr::bind_rows(y, z)
     14. │                 └─vctrs::vec_rbin
    Execution halted
    ```

# omu

<details>

* Version: 1.0.2
* Source code: https://github.com/cran/omu
* URL: https://github.com/connor-reid-tiffany/Omu, https://www.kegg.jp/kegg/rest/keggapi.html
* BugReports: https://github.com/connor-reid-tiffany/Omu/issues
* Date/Publication: 2018-08-02 12:40:03 UTC
* Number of recursive dependencies: 82

Run `revdep_details(,"omu")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Backtrace:
         █
      1. ├─omu::assign_hierarchy(...)
      2. │ ├─dplyr::left_join(count_data, Metabolite_Hierarchy_Table, by = identifier)
      3. │ └─dplyr:::left_join.data.frame(...)
      4. │   └─dplyr:::join_mutate(...)
      5. │     ├─dplyr:::join_cols(...)
      6. │     │ └─dplyr:::check_duplicate_vars(x_names, "x")
      7. │     │   └─base::duplicated(vars)
      8. │     └─dplyr::tbl_vars(x)
      9. │       ├─dplyr:::new_sel_vars(tbl_vars_dispatch(x), group_vars(x))
     10. │       │ └─base::structure(...)
     11. │       ├─dplyr::group_vars(x)
     12. │       └─dplyr:::group_vars.data.frame(x)
     13. │         ├─generics::setdiff(names(group_data(x)), ".rows")
     14. │         ├─dplyr::group_data(x)
     15. │         └─dplyr:::group_data.data.frame(x)
     16. │           └─vctrs::vec_init(.data[, 0], 1)
     17. └─vctrs:::stop_scalar_type(...)
     18.   └─vc
    Execution halted
    ```

# Organism.dplyr

<details>

* Version: 1.14.0
* Source code: https://github.com/cran/Organism.dplyr
* Date/Publication: 2019-10-29
* Number of recursive dependencies: 118

Run `revdep_details(,"Organism.dplyr")` for more info

</details>

## Newly broken

*   checking S3 generic/method consistency ... WARNING
    ```
    src_tbls:
      function(x, ...)
    src_tbls.src_organism:
      function(x)
    
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── 2. Error: (unknown) (@test-src_organism-select.R#3)  ────────────────────────
      there is no package called 'TxDb.Hsapiens.UCSC.hg38.knownGene'
      Backtrace:
       1. base::suppressPackageStartupMessages(...)
       3. base::library(TxDb.Hsapiens.UCSC.hg38.knownGene)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 38 | SKIPPED: 0 | WARNINGS: 4 | FAILED: 2 ]
      1. Error: (unknown) (@test-GenomicFeatures-extractors.R#3) 
      2. Error: (unknown) (@test-src_organism-select.R#3) 
      
      Error: testthat unit tests failed
      In addition: Warning message:
      call dbDisconnect() when finished working with a connection 
      Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘TxDb.Hsapiens.UCSC.hg38.knownGene’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unexported objects imported by ':::' calls:
      ‘AnnotationDbi:::smartKeys’ ‘GenomicFeatures:::.exons_with_3utr’
      ‘GenomicFeatures:::.exons_with_5utr’
      ‘GenomicFeatures:::get_TxDb_seqinfo0’
      ‘S4Vectors:::extract_data_frame_rows’
      See the note in ?`:::` about the use of this operator.
    ```

*   checking R code for possible problems ... NOTE
    ```
    .toGRanges: no visible binding for global variable ‘.’
    intronsByTranscript,src_organism: no visible binding for global
      variable ‘.’
    orgPackageName,src_organism: no visible binding for global variable
      ‘name’
    orgPackageName,src_organism: no visible binding for global variable
      ‘organism’
    orgPackageName,src_organism: no visible binding for global variable
      ‘OrgDb’
    Undefined global functions or variables:
      . OrgDb name organism
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
      LL: integer
    >> to <LL_df<
      tz_var: character
      t     : integer
      tz    : integer
      LL    : integer
    >>.
    Backtrace:
         █
      1. ├─pammtools::gg_laglead(simdf_elra)
      2. ├─pammtools:::gg_laglead.nested_fdf(simdf_elra)
      3. │ ├─pammtools::get_laglead(x)
      4. │ └─pammtools:::get_laglead.data.frame(x)
      5. │   └─purrr::map2_dfr(...)
      6. │     └─dplyr::bind_rows(res, .id = .id)
      7. │       └─vctrs::vec_rbind(!!!dots, .names_to = .id)
      8. └─vctrs::vec_default_cast(x = x, to = to, x_arg = x_arg, to_arg = to_arg)
      9.   └─vctrs::stop_incompatible_cast(x, to, x_arg = x_arg, to_arg = to_arg)
     10.     └─vctrs:::stop_incompatible(...)
     11.       └─vctrs:::stop_vctrs(...)
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 229 | SKIPPED: 0 | WARNINGS: 2 | FAILED: 16 ]
      1. Error: cumulative hazard functions work for PAM (@test-add-functions.R#101) 
      2. Failure: adding terms works for PAM (@test-add-functions.R#145) 
      3. Failure: adding terms works for PAM (@test-add-functions.R#150) 
      4. Failure: adding terms works for PAM (@test-add-functions.R#152) 
      5. Failure: adding terms works for PAM (@test-add-functions.R#155) 
      6. Failure: adding terms works for PAM (@test-add-functions.R#157) 
      7. Error: survival probabilities functions work for PAM (@test-add-functions.R#236) 
      8. Error: LL helpers and as_ped produce equivalent LL windows 
      9. Error: Cumulative effects are calculated correctly (@test-cumulative-effect.R#102) 
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
* Number of recursive dependencies: 167

Run `revdep_details(,"panelr")` for more info

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

*   checking tests ...
    ```
     ERROR
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

# PAST

<details>

* Version: 1.2.8
* Source code: https://github.com/cran/PAST
* URL: https://github.com/IGBB/past
* BugReports: https://github.com/IGBB/past/issues
* Date/Publication: 2020-03-16
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
      6. └─PAST::load_GWAS_data(demo_association_file, demo_effects_file) /var/folders/4b/hn4fq98s6810s4ccv2f9hm2h0000gn/T//RtmprHINjW/Rexc9285a1e1f0:11:0
      7.   └─`%>%`(...)
      8.     ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      9.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
     10.       └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
     11.         └─PAST:::`_fseq`(`_lhs`)
     12.           └─magrittr::freduce(value, `_function_list`)
     13.             └─function_list[[i]](value)
     14.               ├─dplyr::mutate(...)
     15.               └─dplyr:::mutate.data.frame(...)
     16. 
    Execution halted
    ```

## In both

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

*   checking R code for possible problems ... NOTE
    ```
    assign_chunk: no visible binding for global variable ‘chromosome’
    assign_chunk: no visible global function definition for ‘IRanges’
    assign_chunk: no visible binding for global variable ‘position’
    assign_chunk: no visible binding for global variable ‘seqid’
    assign_chunk: no visible binding for global variable ‘Name’
    Undefined global functions or variables:
      IRanges Name chromosome position seqid
    ```

# PHEindicatormethods

<details>

* Version: 1.3.0
* Source code: https://github.com/cran/PHEindicatormethods
* BugReports: https://github.com/PublicHealthEngland/PHEindicatormethods/issues
* Date/Publication: 2020-03-12 14:20:02 UTC
* Number of recursive dependencies: 61

Run `revdep_details(,"PHEindicatormethods")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    +                               60L, 65L, 70L, 75L, 80L, 85L, 90L),
    +                  pops = c(7060L, 35059L, 46974L, 48489L, 43219L, 38561L, 46009L, 57208L,
    +                           61435L, 55601L, 50209L, 56416L, 46411L, 39820L, 37978L,
    +                           37039L, 33288L, 23306L, 11936L, 11936L),
    +                  deaths = c(17L, 9L, 4L, 8L, 20L, 15L, 24L, 33L, 50L, 71L, 100L, 163L,
    +                             263L, 304L, 536L, 872L, 1390L, 1605L, 1936L, 1937L))
    > phe_life_expectancy(df, deaths, pops, startage)
    Error: `new` must be a tibble
    Backtrace:
         █
      1. └─PHEindicatormethods::phe_life_expectancy(df, deaths, pops, startage)
      2.   └─`%>%`(...)
      3.     ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      4.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5.       └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      6.         └─PHEindicatormethods:::`_fseq`(`_lhs`)
      7.           └─magrittr::freduce(value, `_function_list`)
      8.             └─function_list[[i]](value)
      9.               └─dplyr::count(.)
     10.                 └─dplyr::dplyr_reconstruct(out, x)
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       17. base:::tryCatchOne(...)
       18. value[[3L]](cond)
       19. dplyr:::stop_eval_tidy(e, index = i, dots = dots, fn = "summarise")
       20. dplyr:::stop_dplyr(index, dots, fn, "errored", x = conditionMessage(e))
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 185 | SKIPPED: 0 | WARNINGS: 1 | FAILED: 5 ]
      1. Error: (unknown) (@testLifeExpectancy.R#139) 
      2. Error: proportions and CIs calculate correctly (@testProportions.R#7) 
      3. Error: quantiles calculate correctly (@testQuantiles.R#19) 
      4. Error: rates and CIs calculate correctly (@testRates.R#7) 
      5. Error: (unknown) (@testSII.R#22) 
      
      Error: testthat unit tests failed
      Execution halted
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

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tidyr’
      All declared Imports should be used.
    ```

# pixiedust

<details>

* Version: 0.8.6
* Source code: https://github.com/cran/pixiedust
* URL: https://github.com/nutterb/pixiedust
* BugReports: https://github.com/nutterb/pixiedust/issues
* Date/Publication: 2019-01-11 16:40:03 UTC
* Number of recursive dependencies: 65

Run `revdep_details(,"pixiedust")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 503 | SKIPPED: 120 | WARNINGS: 3 | FAILED: 10 ]
      1.  Failure: lowercase color (@test-colors.R#8) 
      2.  Failure: dust with glance_foot (@test-dust.R#79) 
      3.  Failure: dust with glance_foot and col_pairs a divisor of total_cols (@test-dust.R#85) 
      4.  Failure: glance_foot by column (@test-glance_foot.R#18) 
      5.  Failure: glance_foot by row (@test-glance_foot.R#23) 
      6.  Failure: glance_foot with subset of stats (@test-glance_foot.R#28) 
      7.  Failure: medley_model (@test-medley.R#17) 
      8.  Failure: print.dust for markdown output (@test-print.dust-explicit.R#12) 
      9.  Failure: print.dust for latex output with hhline = FALSE (@test-print.dust-explicit.R#26) 
      10. Failure: print.dust for latex output with hhline = TRUE (@test-print.dust-explicit.R#33) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘Hmisc’
    ```

# PKNCA

<details>

* Version: 0.9.2
* Source code: https://github.com/cran/PKNCA
* URL: https://github.com/billdenney/pknca
* BugReports: https://github.com/billdenney/pknca/issues
* Date/Publication: 2020-02-28 16:20:12 UTC
* Number of recursive dependencies: 74

Run `revdep_details(,"PKNCA")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    No dose information provided, calculations requiring dose will return NA.
    > my_result_excluded <- exclude(my_result,
    +                               FUN=exclude_nca_max.aucinf.pext())
    Error: Column name `start` must not be duplicated.
    Backtrace:
         █
      1. ├─PKNCA::exclude(my_result, FUN = exclude_nca_max.aucinf.pext())
      2. └─PKNCA:::exclude.default(my_result, FUN = exclude_nca_max.aucinf.pext())
      3.   └─`%>%`(...)
      4.     ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      5.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      6.       └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      7.         └─PKNCA:::`_fseq`(`_lhs`)
      8.           └─magrittr::freduce(value, `_function_list`)
      9.             └─function_list[[i]](value)
     10.               ├─dplyr::group_by(., !!!rlang::syms(groupnames))
     11.               └─dplyr:::group_by.data.frame(., !!!rlang::syms(groupnames))
     12.                 └─dplyr::grouped_df(groups$data, groups$group_names, .drop)
     13.                   └─dplyr:::compute_groups(data, vars, drop = drop)
     14.                     └─tibble::tibble(!!!old_keys, `:=`(".rows", old_r
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       18. tibble:::repaired_names(...)
       19. tibble:::subclass_name_repair_errors(...)
       20. base::tryCatch(...)
       21. base:::tryCatchList(expr, classes, parentenv, handlers)
       22. base:::tryCatchOne(...)
       23. value[[3L]](cond)
      
      Provenance hash a generated on b with c.
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 1295 | SKIPPED: 0 | WARNINGS: 1 | FAILED: 2 ]
      1. Error: exclude.default (@test-exclude.R#198) 
      2. Error: exclude_nca (@test-exclude_nca.R#8) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# pmdplyr

<details>

* Version: 0.3.1
* Source code: https://github.com/cran/pmdplyr
* URL: https://nickch-k.github.io/pmdplyr, https://github.com/NickCH-K/pmdplyr
* BugReports: https://github.com/NickCH-K/pmdplyr/issues
* Date/Publication: 2020-03-09 19:30:02 UTC
* Number of recursive dependencies: 105

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
      
      Error: C stack usage  7971296 is too close to the limit
      Execution halted
    ```

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in documentation object 'join.Rd':
      ‘[dplyr]{join.tbl_df}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
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
      In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/prophet/StanHeaders/include/stan/math.hpp:4:
      In file included from /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/prophet/StanHeaders/include/stan/math/rev/mat.hpp:51:
      /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/prophet/StanHeaders/include/stan/math/rev/mat/fun/squared_distance.hpp:27:11: warning: unused type alias 'idx_t' [-Wunused-local-typedef]
          using idx_t = typename index_type<matrix_v>::type;
                ^
      /Users/romainfrancois/git/revdep/dplyr/revdep/library.noindex/prophet/StanHeaders/include/stan/math/rev/mat/fun/squared_distance.hpp:64:11: warning: unused type alias 'idx_t' [-Wunused-local-typedef]
          using idx_t = typename index_type<matrix_d>::type;
                ^
      26 warnings generated.
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 346 | SKIPPED: 1 | WARNINGS: 1 | FAILED: 1 ]
      1. Failure: conditional_custom_seasonality (@test_prophet.R#630) 
      
      Error: testthat unit tests failed
      Execution halted
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

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘data.table’ ‘fractal’ ‘mgcv’
      All declared Imports should be used.
    ```

# purrr

<details>

* Version: 0.3.3
* Source code: https://github.com/cran/purrr
* URL: http://purrr.tidyverse.org, https://github.com/tidyverse/purrr
* BugReports: https://github.com/tidyverse/purrr/issues
* Date/Publication: 2019-10-18 12:40:05 UTC
* Number of recursive dependencies: 56

Run `revdep_details(,"purrr")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
          is_null
      
      > 
      > test_check("purrr")
      ── 1. Failure: can flatten to a data frame with named lists (@test-flatten.R#82)
      flatten_dfc(list(1)) not equal to tibble::tibble(V1 = 1).
      Names: 1 string mismatch
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 768 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Failure: can flatten to a data frame with named lists (@test-flatten.R#82) 
      
      Error: testthat unit tests failed
      Execution halted
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

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘tidycensus’
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

## In both

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

# rFIA

<details>

* Version: 0.2.1
* Source code: https://github.com/cran/rFIA
* Date/Publication: 2020-04-03 19:20:02 UTC
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

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.7Mb
      sub-directories of 1Mb or more:
        R      2.9Mb
        data   1.9Mb
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

*   checking whether the namespace can be unloaded cleanly ... WARNING
    ```
    ---- unloading
    Error in .mergeMethodsTable(generic, mtable, tt, attach) : 
      trying to get slot "defined" from an object of a basic class ("environment") with no slots
    Calls: unloadNamespace ... <Anonymous> -> .updateMethodsInTable -> .mergeMethodsTable
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

Run `revdep_details(,"rsample")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘rsample-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: bootstraps
    > ### Title: Bootstrap Sampling
    > ### Aliases: bootstraps
    > 
    > ### ** Examples
    > 
    > bootstraps(mtcars, times = 2)
    # Bootstrap sampling 
    # A tibble: 2 x 2
      splits          id        
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 539 | SKIPPED: 1 | WARNINGS: 0 | FAILED: 12 ]
      1. Error: apparent (@test_boot.R#28) 
      2. Error: strata (@test_boot.R#61) 
      3. Failure: rsplit labels (@test_boot.R#89) 
      4. Error: (unknown) 
      5. Failure: rsplit labels (@test_group.R#104) 
      6. Failure: rsplit labels (@test_loo.R#38) 
      7. Failure: rsplit labels (@test_mc.R#86) 
      8. Failure: rsplit labels (@test_nesting.R#71) 
      9. Failure: rsplit labels (@test_rolling.R#102) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
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

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘RSQLite’
      All declared Imports should be used.
    ```

# rstatix

<details>

* Version: 0.4.0
* Source code: https://github.com/cran/rstatix
* URL: https://rpkgs.datanovia.com/rstatix/
* BugReports: https://github.com/kassambara/rstatix/issues
* Date/Publication: 2020-02-13 07:40:03 UTC
* Number of recursive dependencies: 126

Run `revdep_details(,"rstatix")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    +  t_test(len ~ dose) %>%
    +  adjust_pvalue() %>%
    +  add_significance("p.adj")
    Error: Input must be a vector, not a `tbl_df/tbl/data.frame/rstatix_test/t_test` object.
    Backtrace:
         █
      1. ├─ToothGrowth %>% t_test(len ~ dose) %>% adjust_pvalue() %>% add_significance("p.adj")
      2. │ ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      3. │ └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      4. │   └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5. │     └─`_fseq`(`_lhs`)
      6. │       └─magrittr::freduce(value, `_function_list`)
      7. │         └─function_list[[i]](value)
      8. │           └─rstatix::adjust_pvalue(.)
      9. │             └─`%>%`(...)
     10. │               ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
     11. │               └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
     12. │                 └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
     13. │                   └─rstatix:::`_fseq`(`_lhs`)
     14. │                     └─magrittr::freduce(value, `_func
    Execution halted
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

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘openssl’
      All declared Imports should be used.
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
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 291 | SKIPPED: 1 | WARNINGS: 1 | FAILED: 14 ]
      1. Failure: act_after_exposure works (@test-actions.R#66) 
      2. Failure: bind_exposures removes names from list-column `fun` (@test-expose-helpers.R#111) 
      3. Failure: bind_exposures removes names from list-column `fun` (@test-expose-helpers.R#117) 
      4. Failure: expose works (@test-expose.R#159) 
      5. Failure: expose works (@test-expose.R#188) 
      6. Failure: expose removes obeyers (@test-expose.R#202) 
      7. Failure: expose removes obeyers (@test-expose.R#214) 
      8. Failure: expose preserves pack names (@test-expose.R#246) 
      9. Failure: expose accounts for rule separator (@test-expose.R#264) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

# RWDataPlyr

<details>

* Version: 0.6.3
* Source code: https://github.com/cran/RWDataPlyr
* URL: https://github.com/BoulderCodeHub/RWDataPlyr
* BugReports: https://github.com/BoulderCodeHub/RWDataPlyr/issues
* Date/Publication: 2020-03-03 05:40:02 UTC
* Number of recursive dependencies: 57

Run `revdep_details(,"RWDataPlyr")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 645 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 14 ]
      1. Failure: apply_eval works as expected (@test_apply_eval.R#23) 
      2. Failure: apply_summary works as expected (@test_apply_summary.R#24) 
      3. Failure: apply_summary errors correctly (@test_apply_summary.R#124) 
      4. Failure: 'all' keyword gets all data (@test_rdf_aggregate.R#141) 
      5. Failure: `cpp` parameters don't change results (@test_rdf_aggregate.R#314) 
      6. Failure: functions match (@test_rdf_to_tbl2.R#31) 
      7. Failure: different versions match (@test_rdf_to_tbl2.R#38) 
      8. Failure: methods match (@test_rdf_to_tbl2.R#65) 
      9. Failure: methods match (@test_rdf_to_tbl2.R#66) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
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

# saotd

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/saotd
* BugReports: https://github.com/evan-l-munson/saotd/issues
* Date/Publication: 2019-04-04 16:30:03 UTC
* Number of recursive dependencies: 110

Run `revdep_details(,"saotd")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
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

# scFeatureFilter

<details>

* Version: 1.6.0
* Source code: https://github.com/cran/scFeatureFilter
* Date/Publication: 2019-10-29
* Number of recursive dependencies: 114

Run `revdep_details(,"scFeatureFilter")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Component "restofgenes": Component "cv": Mean relative difference: 0.8
      Component "restofgenes": Component "x1": Mean relative difference: 0.8
      Component "restofgenes": Component "x2": Mean relative difference: 0.8
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 28 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 6 ]
      1. Failure: giving the right result (@test_calculate_cvs.R#18) 
      2. Failure: giving the right result (@test_calculate_cvs.R#22) 
      3. Failure: giving the right result (@test_calculate_cvs.R#26) 
      4. Failure: giving the right result (@test_calculate_cvs.R#30) 
      5. Failure: Giving the expected result (@test_extract_top_genes.R#26) 
      6. Failure: Giving the expected result (@test_extract_top_genes.R#30) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘scRNAseq’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.6Mb
      sub-directories of 1Mb or more:
        data   3.7Mb
        doc    2.6Mb
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

# sergeant

<details>

* Version: 0.5.2
* Source code: https://github.com/cran/sergeant
* URL: https://github.com/hrbrmstr/sergeant
* BugReports: https://github.com/hrbrmstr/sergeant/issues
* Date/Publication: 2017-07-17 22:36:26 UTC
* Number of recursive dependencies: 62

Run `revdep_details(,"sergeant")` for more info

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

# sevenbridges

<details>

* Version: 1.16.1
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
    #sjdbGTFfile                                 sjdbGTFfile File...    FALSE
    #fastq                                             fastq File...     TRUE
    #genomeFastaFiles                       genomeFastaFiles    File     TRUE
    #sjdbGTFtagExonParentTranscript      Exons' parents name  string    FALSE
    #sjdbGTFtagExonParentGene                      Gene name  string    FALSE
    #winAnchorMultimapNmax                  Max loci anchors     int    FALSE
    #winAnchorDistNbins             Max bins between anchors     int    FALSE
                                    fileTypes
    #sjdbGTFfile                         null
    #fastq                               null
    #genomeFastaFiles                    null
    #sjdbGTFtagExonParentTranscript      null
    #sjdbGTFtagExonParentGene            null
    #winAnchorMultimapNmax               null
    #winAnchorDistNbins                  null
    > # by name
    > f1$input_matrix(c("id", "type", "required", "link_to"))
    Error in Ops.factor(lm$source, sname) : 
      level sets of factors are different
    Calls: <Anonymous> ... FUN -> paste0 -> [ -> [.data.frame -> which -> Ops.factor
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 12.3Mb
      sub-directories of 1Mb or more:
        R     2.0Mb
        doc   9.5Mb
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

* Version: 0.3.0
* Source code: https://github.com/cran/silicate
* URL: https://github.com/hypertidy/silicate
* BugReports: https://github.com/hypertidy/silicate/issues
* Date/Publication: 2020-03-22 17:30:03 UTC
* Number of recursive dependencies: 132

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
      [ OK: 79 | SKIPPED: 7 | WARNINGS: 5 | FAILED: 4 ]
      1. Error: ARC for non polygons is a warnable offence (@test-arc-tests.R#5) 
      2. Failure: generic forms are understood (@test-generic-data.R#14) 
      3. Error: print works (@test-print.R#5) 
      4. Error: object and path names as expected (@test-sf-decomposition.R#36) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# simglm

<details>

* Version: 0.7.4
* Source code: https://github.com/cran/simglm
* Date/Publication: 2019-05-31 17:10:03 UTC
* Number of recursive dependencies: 89

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
      [ OK: 129 | SKIPPED: 0 | WARNINGS: 7 | FAILED: 2 ]
      1. Failure: interupt TS (@test_knots.r#69) 
      2. Failure: interupt TS (@test_knots.r#96) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# SimplifyStats

<details>

* Version: 2.0.2
* Source code: https://github.com/cran/SimplifyStats
* Date/Publication: 2019-03-12 06:53:05 UTC
* Number of recursive dependencies: 57

Run `revdep_details(,"SimplifyStats")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘SimplifyStats-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: group_summarize
    > ### Title: Calculate descriptive statistics for each group
    > ### Aliases: group_summarize group_summarise
    > 
    > ### ** Examples
    > 
    > group_summarize(iris, "Species", c("Sepal.Length", "Sepal.Width"))
    Error: Can't column-bind data frames with different row names.
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Backtrace:
       1. testthat::expect_true(...)
       4. SimplifyStats::pairwise_stats(...)
       5. base::lapply(...)
       6. SimplifyStats:::FUN(X[[i]], ...)
       7. dplyr::bind_cols(...)
       8. vctrs::vec_cbind(!!!dots)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 7 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 2 ]
      1. Error: valid input produce valid output (@test.group_summarize.R#33) 
      2. Error: valid input produce valid output (@test.pairwise_stats.R#32) 
      
      Error: testthat unit tests failed
      Execution halted
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

# skimr

<details>

* Version: 2.1
* Source code: https://github.com/cran/skimr
* URL: https://docs.ropensci.org/skimr (website), https://github.com/ropensci/skimr
* BugReports: https://github.com/ropensci/skimr/issues
* Date/Publication: 2020-02-01 19:00:02 UTC
* Number of recursive dependencies: 70

Run `revdep_details(,"skimr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
      complete_rate: double
      numeric.mean : double
      numeric.sd   : double
      numeric.p0   : double
      numeric.p25  : double
      numeric.p50  : double
      numeric.p75  : double
      numeric.p100 : double
      numeric.hist : character
    >>.
    Backtrace:
        █
     1. ├─base::identical(bind(separate), skimmed)
     2. ├─skimr::bind(separate)
     3. │ └─dplyr::bind_rows(!!!with_namespaces, .id = "skim_type")
     4. │   └─vctrs::vec_rbind(!!!dots, .names_to = .id)
     5. └─vctrs::vec_default_ptype2(x = x, y = y, x_arg = x_arg, y_arg = y_arg)
     6.   └─vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
     7.     └─vctrs:::stop_incompatible(...)
     8.       └─vctrs:::stop_vctr
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 549 | SKIPPED: 3 | WARNINGS: 0 | FAILED: 22 ]
      1. Failure: dplyr::filter works as expected (@test-dplyr.R#8) 
      2. Failure: dplyr::select works as expected (@test-dplyr.R#17) 
      3. Failure: dplyr::select works as expected (@test-dplyr.R#20) 
      4. Failure: dplyr::mutate works as expected (@test-dplyr.R#26) 
      5. Failure: dplyr::slice works as expected (@test-dplyr.R#35) 
      6. Failure: dplyr::arrange works as expected (@test-dplyr.R#41) 
      7. Error: Bind produces skim_df objects (@test-skim_obj.R#23) 
      8. Failure: Skim prints a header for the entire output and each type (@test-skim_print.R#6) 
      9. Failure: Skim prints a header for the entire output and each type (@test-skim_print.R#9) 
      1. ...
      
      Error: testthat unit tests failed
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

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘maps’
      All declared Imports should be used.
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
# spdplyr

<details>

* Version: 0.3.0
* Source code: https://github.com/cran/spdplyr
* URL: https://github.com/mdsumner/spdplyr
* BugReports: https://github.com/mdsumner/spdplyr/issues
* Date/Publication: 2019-05-13 10:30:02 UTC
* Number of recursive dependencies: 67

Run `revdep_details(,"spdplyr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    coord. ref. :  +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0 
    variables   : 11
    # A tibble: 246 x 11
       FIPS  ISO2  ISO3     UN NAME      AREA POP2005 REGION SUBREGION     LON   LAT
       <fct> <fct> <fct> <int> <chr>    <int>   <dbl>  <int>     <int>   <dbl> <dbl>
     1 AC    AG    ATG      28 allthe…     44  8.30e4      1        29  -61.8   17.1
     2 AG    DZ    DZA      12 allthe… 238174  3.29e7      2        15    2.63  28.2
     3 AJ    AZ    AZE      31 allthe…   8260  8.35e6      3       145   47.4   40.4
     4 AL    AL    ALB       8 allthe…   2740  3.15e6      4        39   20.1   41.1
     5 AM    AM    ARM      51 allthe…   2820  3.02e6      5       145   44.6   40.5
     6 AO    AO    AGO      24 allthe… 124670  1.61e7      6        17   17.5  -12.3
     7 AQ    AS    ASM      16 allthe…     20  6.41e4      7        61 -171.   -14.3
     8 AR    AR    ARG      32 allthe… 273669  3.87e7      8         5  -65.2  -35.4
     9 AS    AU    AUS      36 allthe… 768230  2.03e7      9        53  136.   -25.0
    10 BA    BH    BHR      48 allthe…     71  7.25e5     10       145   50.6   26.0
    # … with 236 more rows
    > wrld_simpl %>% transmute(alpha = paste0(FIPS, NAME))
    Error in UseMethod("transmute") : 
      no applicable method for 'transmute' applied to an object of class "c('SpatialPolygonsDataFrame', 'SpatialPolygons', 'Spatial', 'SpatialVector')"
    Calls: %>% ... _fseq -> freduce -> withVisible -> <Anonymous> -> transmute
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       12. dplyr:::tbl_at_vars(tbl, vars, .include_group_vars = .include_group_vars)
       13. dplyr::tbl_vars(tbl)
       16. dplyr::group_vars(x)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 37 | SKIPPED: 3 | WARNINGS: 3 | FAILED: 6 ]
      1. Failure: group by and summarize is quiet (@test-adv-dplyr.R#14) 
      2. Error: mutate works for all geometric types (@test-basic-dplyr.R#58) 
      3. Error: mutate works (@test-basic-dplyr.R#118) 
      4. Failure: joins work (@test-basic-dplyr.R#156) 
      5. Failure: joins work (@test-basic-dplyr.R#163) 
      6. Error: tibble requirements (@test-dplyr-0.6.0.R#5) 
      
      Error: testthat unit tests failed
      Execution halted
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

*   checking examples ... ERROR
    ```
    ...
    Error: No common type for `..1` <svyby<>> and `..2` <data.frame<>>.
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
      9. │           ├─srvyr::cascade(...)
     10. │           └─srvyr:::cascade.grouped_svy(...)
     11. │             └─base::lapply(...)
     12. │               └─srvyr:::FUN(X[[i]], ...)
     13. │                 ├─dplyr::summarise(group_by(.data, !!!rlang::syms(ggg)), !!!dots)
     14. │                 └─srvyr:::summarise.grouped_svy(...)
     15. │                   └─base::lapply(...)
     16. │                     └─srvyr:::FUN(X[[i]], ...)
     17.
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        5.         ├─testthat::with_reporter(...)
        6.         │ ├─base::withRestarts(...)
        7.         │ │ └─base:::withOneRestart(expr, restarts[[1L]])
        8.         │ │   └─base:::doWithOneRestart(return(expr), restart)
        9.         │ └─base::force(code)
       10.         └─base::lapply(...)
       11.           └─testthat:::FUN(X[[i]], ...)
       12.             ├─testthat::with_reporter(...)
       13.             │ ├─base::withRestarts(...)
       14.             │ │ └─base:::withOneRestart(expr, restarts[[1L]])
       15.             │ │   └─base:::doWithOneRestart(return(expr), restart)
       16.             │ └─base::force(code)
       17.             └─testthat::source_file(...)
       18.               └─testthat:::te
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

*   checking examples ... ERROR
    ```
    ...
    Error: `summarise()` argument `N` errored.
    ℹ `N` is `n()`.
    ℹ The error occured in group 1: ouv = "Anguillère Cléry-sur-Somme", annee = "2010", mois = "09", stage = "FIII".
    ✖ could not find function "n"
    Backtrace:
         █
      1. ├─graphics::plot(r_silver, plot.type = 2)
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
     14.                   └─dplyr:::stop_dplyr(index, dots, fn, "erro
    Execution halted
    ```

## In both

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

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       1. graphics::plot(r_dc, plot.type = "1", silent = TRUE)
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

# stars

<details>

* Version: 0.4-0
* Source code: https://github.com/cran/stars
* URL: https://r-spatial.github.io/stars/, https://github.com/r-spatial/stars/
* BugReports: https://github.com/r-spatial/stars/issues/
* Date/Publication: 2019-10-10 13:00:02 UTC
* Number of recursive dependencies: 122

Run `revdep_details(,"stars")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
    ...
    < [ OK: 98 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 0 ]
    ---
    > [ OK: 76 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 0 ]
     ERROR
    Running the tests in ‘tests/tidy.R’ failed.
    Last 13 lines of output:
        L7_ETMs.tif    
       Min.   :  1.00  
       1st Qu.: 54.00  
       Median : 69.00  
       Mean   : 68.91  
       3rd Qu.: 86.00  
       Max.   :255.00  
      dimension(s):
           from  to  offset delta                       refsys point values    
      x       1 349  288776  28.5 +proj=utm +zone=25 +south... FALSE   NULL [x]
      y       1 352 9120761 -28.5 +proj=utm +zone=25 +south... FALSE   NULL [y]
      band    1   6      NA    NA                           NA    NA   NULL    
      > (y <- x %>% filter(band > 2))
      Error: 'as.tbl_cube' is not an exported object from 'namespace:dplyr'
      Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Error in get(genname, envir = envir) : object 'as.tbl_cube' not found
    Missing or unexported objects:
      ‘dplyr::as.tbl_cube’ ‘dplyr::tbl_cube’
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘starsdata’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 16.1Mb
      sub-directories of 1Mb or more:
        doc  10.3Mb
        nc    4.5Mb
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

# stratamatch

<details>

* Version: 0.1.4
* Source code: https://github.com/cran/stratamatch
* URL: https://github.com/raikens1/stratamatch
* BugReports: https://github.com/raikens1/stratamatch/issues
* Date/Publication: 2020-02-19 00:10:02 UTC
* Number of recursive dependencies: 87

Run `revdep_details(,"stratamatch")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── 4. Failure: manual stratify works (@test-manual_stratify.R#119)  ────────────
      m.strat$issue_table not equal to `exp_issue_table`.
      Attributes: < Component "class": Lengths (3, 1) differ (string compare on first 1) >
      Attributes: < Component "class": 1 string mismatch >
      Component "Potential_Issues": names for target but not for current
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 219 | SKIPPED: 1 | WARNINGS: 0 | FAILED: 4 ]
      1. Failure: manual stratify with logical treatment works (@test-manual_stratify.R#64) 
      2. Failure: manual stratify with logical treatment works (@test-manual_stratify.R#80) 
      3. Failure: manual stratify works (@test-manual_stratify.R#103) 
      4. Failure: manual stratify works (@test-manual_stratify.R#119) 
      
      Error: testthat unit tests failed
      Execution halted
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
      st_centroid does not give correct centroids for longitude/latitude data
    Linking to GEOS 3.7.2, GDAL 2.4.2, PROJ 5.2.0
    > # Create hexagon location grid
    > data(capital_cities)
    > grid <- create_grid(centroids = centroids, hex_size = 0.2, buffer_dist = 1.2)
    Warning: `as.tibble()` is deprecated as of tibble 2.0.0.
    Please use `as_tibble()` instead.
    The signature and semantics have changed, see `?as_tibble`.
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_warnings()` to see where this warning was generated.
    > # Allocate polygon centroids to hexagon grid points
    > hex_allocated <- allocate(
    +   centroids = centroids,
    +   hex_grid = grid,
    +   hex_size = 0.2, # same size used in create_grid
    +   hex_filter = 10,
    +   use_neighbours = tas_lga,
    +   focal_points = capital_cities,
    +   width = 30, verbose = TRUE
    + )
    New names:
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘lwgeom’
      All declared Imports should be used.
    ```

# sugrrants

<details>

* Version: 0.2.6
* Source code: https://github.com/cran/sugrrants
* URL: https://pkg.earo.me/sugrrants
* BugReports: https://github.com/earowang/sugrrants/issues
* Date/Publication: 2020-03-10 06:40:02 UTC
* Number of recursive dependencies: 89

Run `revdep_details(,"sugrrants")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Component "Date_Time": Mean absolute difference: 29919.72
      Component "Time": Mean relative difference: 0.7226727
      Component "Sensor_ID": Mean relative difference: 0.7892755
      Component "Sensor_Name": 66944 string mismatches
      Component "Hourly_Counts": Mean relative difference: 1.152078
      Component ".Time": Mean relative difference: 0.0209727
      Component ".Hourly_Counts": Mean relative difference: 0.01471554
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 37 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 2 ]
      1. Failure: The tsibble data (@test-calendar.R#126) 
      2. Failure: The output (@test-calendar.R#225) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘plotly’
    ```

# survivalAnalysis

<details>

* Version: 0.1.1
* Source code: https://github.com/cran/survivalAnalysis
* Date/Publication: 2019-02-13 09:40:04 UTC
* Number of recursive dependencies: 116

Run `revdep_details(,"survivalAnalysis")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    x=Nonmaintained      NA
    Error: `.data` must be a data frame without row names.
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
      9.             ├─base::print(.)
     10.             └─survivalAnalysis:::print.SurvivalAnalysisUnivariateResult(.)
     11.               └─survivalAnalysis:::format.SurvivalAnalysisUnivariateResult(...)
     12.                 └─purrr::map(dfs, format_df)
     13.                   └─survivalAnalysis:::.f(.x[[i]], ...)
     14.                     └─`%>%`(...)
     15.                       ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
     16.                       └─base::eva
    Execution halted
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

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        doc   5.1Mb
    ```

# tabr

<details>

* Version: 0.4.2
* Source code: https://github.com/cran/tabr
* URL: https://github.com/leonawicz/tabr
* BugReports: https://github.com/leonawicz/tabr/issues
* Date/Publication: 2020-03-17 12:00:02 UTC
* Number of recursive dependencies: 85

Run `revdep_details(,"tabr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    ℹ `notes` is `as_noteworthy(.data[["notes"]], y$o, y$a, "vector")`.
    ✖ Cannot have zero timesteps.
    Backtrace:
         █
      1. ├─tabr::freq_ratio(x)
      2. └─tabr:::freq_ratio.music(x)
      3.   └─tabr:::freq_ratio.noteworthy(music_notes(x), ...)
      4.     └─tabr:::freq_ratio.numeric(...)
      5.       └─`%>%`(...)
      6.         ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      7.         └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      8.           └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      9.             └─tabr:::`_fseq`(`_lhs`)
     10.               └─magrittr::freduce(value, `_function_list`)
     11.                 └─function_list[[i]](value)
     12.                   ├─dplyr::mutate(...)
     13.                   └─dplyr:::mutate.data.frame(...)
     14.                     └─dplyr:::mutate_cols(.data, ...)
     15.                       └─base::tryCatch(...)
     16.                         
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       13. base:::tryCatchList(expr, classes, parentenv, handlers)
       14. base:::tryCatchOne(...)
       15. value[[3L]](cond)
       16. dplyr:::stop_eval_tidy(e, index = i, dots = dots, fn = "mutate")
       17. dplyr:::stop_dplyr(index, dots, fn, "errored", x = conditionMessage(e))
      
      sh: lilypond: command not found
      sh: lilypond: command not found
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 1179 | SKIPPED: 6 | WARNINGS: 1 | FAILED: 2 ]
      1. Error: frequency ratios compile correctly (@test-freq.R#31) 
      2. Error: Read midi files as expected (@test-read-midi.R#7) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# taxadb

<details>

* Version: 0.1.0
* Source code: https://github.com/cran/taxadb
* URL: https://docs.ropensci.org/taxadb, https://github.com/ropensci/taxadb
* BugReports: https://github.com/ropensci/taxadb/issues
* Date/Publication: 2020-02-19 07:50:02 UTC
* Number of recursive dependencies: 77

Run `revdep_details(,"taxadb")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       20. dbplyr:::group_by.tbl_lazy(x, ..., .add = TRUE, .drop = .drop)
       21. dplyr::group_by_prepare(.data, .dots = dots, add = add)
       22. dplyr:::add_computed_columns(.data, new_groups)
       23. dplyr:::mutate_cols(.data, !!!vars)
       24. DataMask$new(.data, caller_env())
       25. .subset2(public_bind_env, "initialize")(...)
       26. dplyr::group_rows(data)
       27. dplyr::group_data(.data)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 63 | SKIPPED: 4 | WARNINGS: 1 | FAILED: 1 ]
      1. Error: we can handle more intensive comparisons: ITIS test (@test-get_names.R#71) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking: 'MonetDBLite', 'duckdb'
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘RSQLite’
      All declared Imports should be used.
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
# tibbleOne

<details>

* Version: 0.1.1
* Source code: https://github.com/cran/tibbleOne
* Date/Publication: 2020-01-29 06:20:03 UTC
* Number of recursive dependencies: 86

Run `revdep_details(,"tibbleOne")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       11. dplyr:::arrange.data.frame(., variable)
       12. dplyr:::arrange_rows(.data, dots)
       13. base::tryCatch(...)
       14. base:::tryCatchList(expr, classes, parentenv, handlers)
       15. base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
       16. value[[3L]](cond)
       17. dplyr:::stop_arrange_transmute(cnd)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 30 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 2 ]
      1. Error: correct inputs work (@test-to_kable.R#15) 
      2. Error: correct inputs work (@test-to_word.R#12) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘lifecycle’
      All declared Imports should be used.
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

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘plotly’
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

# tidylog

<details>

* Version: 1.0.0
* Source code: https://github.com/cran/tidylog
* URL: https://github.com/elbersb/tidylog/
* BugReports: https://github.com/elbersb/tidylog/issues
* Date/Publication: 2020-01-07 14:10:02 UTC
* Number of recursive dependencies: 50

Run `revdep_details(,"tidylog")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Did you misspecify an argument?
      Backtrace:
        1. testthat::expect_message(...)
        6. tidylog::ungroup(mtcars, mpg)
        7. tidylog:::log_group_by(...)
        9. dplyr:::ungroup.data.frame(.data, ...)
       10. ellipsis::check_dots_empty()
       11. ellipsis:::action_dots(...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 261 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Error: ungroup (@test_group_by.R#38) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# tidymv

<details>

* Version: 2.2.0
* Source code: https://github.com/cran/tidymv
* URL: https://github.com/stefanocoretta/tidymv
* BugReports: https://github.com/stefanocoretta/tidymv/issues
* Date/Publication: 2019-06-17 11:40:03 UTC
* Number of recursive dependencies: 67

Run `revdep_details(,"tidymv")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    +   create_start_event(group)
    Error: Can't convert from `default` <logical> to `x` <character>.
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
      9. │           └─tidymv::create_start_event(., group)
     10. │             ├─dplyr::mutate(...)
     11. │             └─dplyr:::mutate.data.frame(...)
     12. │               └─dplyr:::mutate_cols(.data, ...)
     13. │                 ├─base::tryCatch(...)
     14. │                 │ └─base:::tryCatchList(expr, classes, parentenv, handlers)
     15. │                 │   ├─base:::tryCatchOne(...)
     16. │                 │   │ └─base:::do
    Execution halted
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

# tidyquery

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/tidyquery
* URL: https://github.com/ianmcook/tidyquery
* BugReports: https://github.com/ianmcook/tidyquery/issues
* Date/Publication: 2020-01-20 18:50:03 UTC
* Number of recursive dependencies: 62

Run `revdep_details(,"tidyquery")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(tidyquery)
      > 
      > test_check("tidyquery")
      ── 1. Failure: Aggregate example query #24 returns expected result (@test-aggreg
      query("SELECT min_age, round(AVG(list_price), 2) AS avg_list_price,\n              0.21 AS tax_rate, round(AVG(list_price) * 1.21, 2) AS avg_list_price_with_tax\n            FROM games\n            GROUP BY min_age;") not equal to `%>%`(...).
      Names: 2 string mismatches
      Component 3: Mean relative difference: 0.9903537
      Component 4: Mean relative difference: 102.6667
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 214 | SKIPPED: 0 | WARNINGS: 1 | FAILED: 1 ]
      1. Failure: Aggregate example query #24 returns expected result (@test-aggregate.R#279) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# tidyqwi

<details>

* Version: 0.1.1
* Source code: https://github.com/cran/tidyqwi
* BugReports: https://github.com/medewitt/tidyqwi/issues
* Date/Publication: 2019-06-19 15:50:03 UTC
* Number of recursive dependencies: 70

Run `revdep_details(,"tidyqwi")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > 
      > test_check("tidyqwi")
      ── 1. Error: Try that labels are added (@test.R#97)  ───────────────────────────
      Input must be a vector, not a `tbl_df/tbl/data.frame/qwi` object.
      Backtrace:
        1. tidyqwi::add_qwi_labels(nc_qwi)
       15. vctrs:::stop_scalar_type(...)
       16. vctrs:::stop_vctrs(msg, "vctrs_error_scalar_type", actual = x)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 19 | SKIPPED: 1 | WARNINGS: 0 | FAILED: 1 ]
      1. Error: Try that labels are added (@test.R#97) 
      
      Error: testthat unit tests failed
      Execution halted
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
    > try(df %>% unchop(y))
    Error : No common type for `..1$y` <character> and `..2$y` <integer>.
    > df %>% unchop(y, ptype = tibble(y = integer()))
    Error: Can't convert from `y` <character> to `y` <integer>.
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
     11. └─vctrs::vec_default_cast(x = x, to = to, x_arg = x_arg, to_arg = to_arg)
     12.   └─vctrs::stop_incompatible_cast(x, to, x_arg = x_arg, to_arg = to_arg)
     13.     └─vctrs:::stop_incompatible(...)
     14.       └─vctrs:::stop_vctrs(...)
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

Run `revdep_details(,"tidystopwords")` for more info

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
    Error: No common type for `..1$language_id` <logical> and `..2$language_id` <character>.
    Backtrace:
        █
     1. ├─tidystopwords::generate_stoplist(lang_name = "English")
     2. │ └─dplyr::bind_rows(stoplist_db, ling_filter_db)
     3. │   └─vctrs::vec_rbind(!!!dots, .names_to = .id)
     4. └─vctrs::vec_default_ptype2(x = x, y = y, x_arg = x_arg, y_arg = y_arg)
     5.   └─vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
     6.     └─vctrs:::stop_incompatible(...)
     7.       └─vctrs:::stop_vctrs(...)
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 229801 marked UTF-8 strings
    ```

# tidytransit

<details>

* Version: 0.7.0
* Source code: https://github.com/cran/tidytransit
* URL: https://github.com/r-transit/tidytransit
* BugReports: https://github.com/r-transit/tidytransit
* Date/Publication: 2020-03-15 17:30:02 UTC
* Number of recursive dependencies: 84

Run `revdep_details(,"tidytransit")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘tidytransit-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: get_route_geometry
    > ### Title: Get all trip shapes for a given route and service.
    > ### Aliases: get_route_geometry
    > 
    > ### ** Examples
    > 
    > data(gtfs_duke)
    > gtfs_duke_sf <- gtfs_as_sf(gtfs_duke)
    > routes_sf <- get_route_geometry(gtfs_duke_sf)
    Error in attributes(lst) <- a : 
      'names' attribute [1] must be the same length as the vector [0]
    Calls: get_route_geometry ... vec_init -> vec_restore_dispatch -> vec_restore.sfc -> st_sfc
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Backtrace:
        1. tidytransit::get_trip_geometry(duke_sf, trip_ids)
       22. sf:::vec_restore.sfc(x = x, to = to, n = n)
       23. sf::st_sfc(x, crs = st_crs(to), precision = st_precision(to))
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 96 | SKIPPED: 9 | WARNINGS: 0 | FAILED: 5 ]
      1. Error: travel_times from stop with departures from transfer stops (@test-raptor.R#210) 
      2. Error: get_route_geometry (@test-spatial.R#23) 
      3. Error: route_geometry behaves as before (@test-spatial.R#33) 
      4. Error: one shape per trip is returned (@test-spatial.R#47) 
      5. Error: two shapes are returned even if trips use the same shape_id (@test-spatial.R#56) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.3Mb
      sub-directories of 1Mb or more:
        doc       1.3Mb
        extdata   4.4Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 62 marked UTF-8 strings
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

* Version: 1.4.0
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

## In both

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

# TPP

<details>

* Version: 3.14.1
* Source code: https://github.com/cran/TPP
* Date/Publication: 2020-01-31
* Number of recursive dependencies: 95

Run `revdep_details(,"TPP")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    
    Filtering fold changes in treatment group: Panobinostat_2
      Column 7 between 0.4 and 0.6 -> 15 out of 261 proteins passed
      Column 9 between 0 and 0.3 -> 221 out of 261 proteins passed
      Column 10 between 0 and 0.2 -> 225 out of 261 proteins passed
    10 out of 261 proteins passed in total.
    
    Experiment with most remaining proteins after filtering: Vehicle_1
    -> NormP contains 22 proteins.
    -----------------------------------
    Computing normalization coefficients:
    1. Computing fold change medians for proteins in normP.
    2. Fitting melting curves to medians.
    -> Experiment with best model fit: Vehicle_1 (R2: 0.9919)
    3. Computing normalization coefficients
    Creating QC plots to illustrate median curve fits.
    -----------------------------------
    Normalizing all proteins in all experiments.
    Normalization successfully completed!
    
    New names:
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 169 | SKIPPED: 1 | WARNINGS: 255 | FAILED: 26 ]
      1. Error: NPARC_allok (@test_analyzeTPPTR.R#14) 
      2. Error: NPARC_allok_output (@test_analyzeTPPTR.R#34) 
      3. Error: NPARC_allok_plot (@test_analyzeTPPTR.R#61) 
      4. Error: NPARC_allok_files (@test_analyzeTPPTR.R#94) 
      5. Error: (unknown) (@test_compute_spline_auc.R#12) 
      6. Error: (unknown) (@test_create_spline_plots.R#12) 
      7. Error: (unknown) (@test_evalSplineModel.R#12) 
      8. Error: (unknown) (@test_extract_fit_factors.R#12) 
      9. Error: (unknown) (@test_invoke_spline_prediction.R#11) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 13.4Mb
      sub-directories of 1Mb or more:
        data           1.9Mb
        example_data   8.0Mb
        test_data      1.9Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘broom’
      All declared Imports should be used.
    Unexported objects imported by ':::' calls:
      ‘doParallel:::.options’ ‘mefa:::rep.data.frame’
      See the note in ?`:::` about the use of this operator.
    ```

*   checking R code for possible problems ... NOTE
    ```
    File ‘TPP/R/TPP.R’:
      .onLoad calls:
        packageStartupMessage(msgText, "\n")
    
    See section ‘Good practice’ in '?.onAttach'.
    
    plot_fSta_distribution: no visible binding for global variable
      ‘..density..’
    plot_pVal_distribution: no visible binding for global variable
      ‘..density..’
    Undefined global functions or variables:
      ..density..
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
    > fill_gaps(harvest, .full = TRUE)
    Error: No common type for `..1` <tbl_df<
      fruit: character
      year : double
    >> and `..2` <tbl_ts<
      year : double
      fruit: character
      kilo : integer
    >>.
    Backtrace:
        █
     1. ├─tsibble::fill_gaps(harvest, .full = TRUE)
     2. ├─tsibble:::fill_gaps.tbl_ts(harvest, .full = TRUE)
     3. │ ├─dplyr::group_by(bind_rows(as_tibble(gap_data), .data), !!!grps)
     4. │ └─dplyr::bind_rows(as_tibble(gap_data), .data)
     5. │   └─vctrs::vec_rbind(!!!dots, .names_to = .id)
     6. └─vctrs::vec_default_ptype2(x = x, y = y, x_arg = x_arg, y_arg = y_arg)
     7.   └─vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
     8.     └─vctrs:::stop_incompatible(...)
     9.       └─vctrs:::stop_vctrs(...)
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 505 | SKIPPED: 2 | WARNINGS: 2 | FAILED: 47 ]
      1. Error: 4 day interval (@test-append.R#27) 
      2. Error: (unknown) (@test-append.R#31) 
      3. Error: (unknown) (@test-bind.R#11) 
      4. Error: (unknown) (@test-dplyr.R#5) 
      5. Error: (unknown) (@test-empty.R#32) 
      6. Error: an irregular tbl_ts (@test-gaps.R#23) 
      7. Error: a tbl_ts without implicit missing values (@test-gaps.R#29) 
      8. Error: daylight saving (@test-gaps.R#49) 
      9. Error: a tbl_ts of 4 day interval with no replacement (@test-gaps.R#57) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘dtplyr’, ‘dbplyr’
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

# ushr

<details>

* Version: 0.2.2
* Source code: https://github.com/cran/ushr
* URL: https://github.com/SineadMorris/ushr
* Date/Publication: 2020-03-23 02:50:02 UTC
* Number of recursive dependencies: 70

Run `revdep_details(,"ushr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Joining, by = "id"
    Error: Assigned data `values` must be compatible with existing data.
    ℹ Error occurred for column `ShortLifespan`.
    ✖ No common type for <character> and <double>.
    Backtrace:
         █
      1. └─ushr::summarize_model(model_output, data = simulated_data)
      2.   └─`%>%`(...)
      3.     ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      4.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5.       └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      6.         └─ushr:::`_fseq`(`_lhs`)
      7.           └─magrittr::freduce(value, `_function_list`)
      8.             ├─base::withVisible(function_list[[k]](value))
      9.             └─function_list[[k]](value)
     10.               └─base::replace(., is.na(.), "")
     11.                 ├─base::`[<-`(`*tmp*`, list, value = "")
     12.                 └─tibble:::`[<-.tbl_df`(`*tmp*`, list, value = "")
     13.                   └─tibble:::tbl_subassign_matrix(x, j, value, j_arg, substitute(value))
     14.  
    Execution halted
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
      [ OK: 387 | SKIPPED: 3 | WARNINGS: 2 | FAILED: 43 ]
      1. Error: absdist calculation is correct (@test_absdist.r#22) 
      2. Error: self absdist is 0 (@test_absdist.r#35) 
      3. Error: x ivls without matching y-ivls chroms are reported with absdist = NA (@test_absdist.r#56) 
      4. Error: ensure that absdist is calculated with respect to input tbls issue#108 (@test_absdist.r#87) 
      5. Error: complement with covering interval (@test_complement.r#12) 
      6. Error: complement with middle interval (@test_complement.r#30) 
      7. Error: complement adds final interval (@test_complement.r#49) 
      8. Error: multiple chroms (@test_complement.r#69) 
      9. Error: multiple chroms, chr1 is covered (@test_complement.r#88) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
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
# viafr

<details>

* Version: 0.1.0
* Source code: https://github.com/cran/viafr
* URL: https://github.com/stefanieschneider/viafr
* BugReports: https://github.com/stefanieschneider/viafr/issues
* Date/Publication: 2019-07-01 11:40:03 UTC
* Number of recursive dependencies: 55

Run `revdep_details(,"viafr")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       55. dplyr:::tbl_if_syms(.tbl, .predicate, .env, .include_group_vars = .include_group_vars)
       59. dplyr:::tbl_if_vars(.tbl, .p, .env, ..., .include_group_vars = .include_group_vars)
       61. dplyr:::pull.data.frame(.tbl, tibble_vars[[i]])
       62. tidyselect::vars_pull(names(.data), !!enquo(var))
       63. tidyselect:::pull_as_location2(loc, n, vars)
       71. vctrs::vec_as_location2(i, n = n, names = names, arg = "var")
       72. vctrs:::result_get(...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 36 | SKIPPED: 0 | WARNINGS: 31 | FAILED: 2 ]
      1. Error: query list (@test_search.R#4) 
      2. Error: valid query (@test_search.R#33) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2 marked UTF-8 strings
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

# weathercan

<details>

* Version: 0.3.3
* Source code: https://github.com/cran/weathercan
* URL: https://docs.ropensci.org/weathercan, https://github.com/ropensci/weathercan
* BugReports: https://github.com/ropensci/weathercan/issues
* Date/Publication: 2020-02-05 14:10:02 UTC
* Number of recursive dependencies: 129

Run `revdep_details(,"weathercan")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 330 | SKIPPED: 17 | WARNINGS: 4 | FAILED: 39 ]
      1. Error: weather_interp (hour) interpolates particular columns (@test_01_interpolate.R#182) 
      2. Error: weather_interp (hour) interpolates 'all' (@test_01_interpolate.R#202) 
      3. Error: weather_interp (hour) fails on character columns (@test_01_interpolate.R#219) 
      4. Error: weather_interp (hour) quiet (@test_01_interpolate.R#228) 
      5. Failure: weather_interp (day) interpolates particular columns (@test_01_interpolate.R#265) 
      6. Failure: weather_interp (day) interpolates particular columns (@test_01_interpolate.R#265) 
      7. Error: weather_interp (day) interpolates particular columns (@test_01_interpolate.R#270) 
      8. Error: weather_interp (day) interpolates 'all' (@test_01_interpolate.R#289) 
      9. Error: weather_interp (day) skips character columns (@test_01_interpolate.R#313) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 72 marked UTF-8 strings
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
      rse     : character
      fixed   : character
      diagonal: character
      m       : character
      n       : character
    >>.
    Backtrace:
         █
      1. ├─xpose::prm_table(xpdb_ex_pk, .problem = 1)
      2. │ ├─`%>%`(...)
      3. │ │ └─base::eval(lhs, parent, parent)
      4. │ │   └─base::eval(lhs, parent, parent)
      5. │ └─purrr::map(...)
      6. │   └─xpose:::.f(.x[[i]], ...)
      7. │     └─`%>%`(...)
      8. │       ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      9. │       └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
     10. │         └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
     11. │           └─xpose:::`_fseq`(`_lhs`)
     12. │             └─magrittr::freduce(
    Execution halted
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

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘plotly’
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

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘dtplyr’, ‘dbplyr’
    ```

