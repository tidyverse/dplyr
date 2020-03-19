# aemo

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/aemo
* Date/Publication: 2016-08-20 15:33:40
* Number of recursive dependencies: 49

Run `revdep_details(,"aemo")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... NOTE
    ```
    Missing or unexported object: ‘dplyr::rbind_all’
    ```

# AlphaBeta

<details>

* Version: 1.0.0
* Source code: https://github.com/cran/AlphaBeta
* Date/Publication: 2019-10-29
* Number of recursive dependencies: 48

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
    > ### ** Examples
    > 
    > left_join_microorganisms(as.mo("K. pneumoniae"))
    Error: No common type for `needles$mo` <character> and `haystack$mo` <mo>.
    Backtrace:
         █
      1. ├─AMR::left_join_microorganisms(as.mo("K. pneumoniae"))
      2. │ ├─base::suppressWarnings(...)
      3. │ │ └─base::withCallingHandlers(expr, warning = function(w) invokeRestart("muffleWarning"))
      4. │ ├─dplyr::left_join(...)
      5. │ └─dplyr:::left_join.data.frame(...)
      6. │   └─dplyr:::join_mutate(...)
      7. │     └─dplyr:::join_rows(x_key, y_key, type = type, na_equal = na_equal)
      8. │       └─vctrs::vec_match(x_key, y_split$key, na_equal = na_equal)
      9. ├─vctrs:::vec_ptype2_dispatch_s3(x = x, y = y, x_arg = x_arg, y_arg = y_arg)
     10. ├─vctrs::vec_ptype2.character(x = x, y = y, x_arg = x_arg, y_arg = y_arg)
     11. └─vctrs:::vec_ptype2.character.default(...)
     12.   └─vctrs::vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
     13.     └─vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
     14.       └─vctr
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
      installed size is  5.8Mb
      sub-directories of 1Mb or more:
        data   3.4Mb
    ```

<<<<<<< Updated upstream
# anomalize

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/anomalize
* URL: https://github.com/business-science/anomalize
* BugReports: https://github.com/business-science/anomalize/issues
* Date/Publication: 2019-09-21 04:10:03 UTC
* Number of recursive dependencies: 150

Run `revdep_details(,"anomalize")` for more info

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

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 52 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 15 ]
      1. Error: returns a ggplot (@test-plot_anomalies.R#8) 
      2. Error: returns a ggplot (@test-plot_anomaly_decomposition.R#10) 
      3. Error: grouped_tbl_time works (@test-time_apply.R#11) 
      4. Error: grouped_tbl_time works (@test-time_apply.R#10) 
      5. Error: (unknown) (@test-time_apply.R#10) 
      6. Error: single tbl_df (@test-time_decompose.R#14) 
      7. Error: time_frequency works: period = 'auto' (@test-time_frequency.R#26) 
      8. Error: time_frequency works: period = '1 month' (@test-time_frequency.R#35) 
      9. Error: time_frequency works: period = 5 (@test-time_frequency.R#44) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

=======
>>>>>>> Stashed changes
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
* Number of recursive dependencies: 64

Run `revdep_details(,"areal")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    +     aw_calculate(value = "TOTAL_E", areaWeight = "areaWeight") -> intersect
    Error: `nm` must be `NULL` or a character vector the same length as `x`
    Backtrace:
         █
      1. └─`%>%`(...)
      2.   ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      3.   └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      4.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5.       └─`_fseq`(`_lhs`)
      6.         └─magrittr::freduce(value, `_function_list`)
      7.           └─function_list[[i]](value)
      8.             └─areal::aw_total(...)
      9.               ├─dplyr::left_join(.data, sum, by = idQN)
     10.               ├─sf:::left_join.sf(.data, sum, by = idQN)
     11.               │ └─sf:::sf_join(NextMethod(), attr(x, "sf_column"), suffix[1])
     12.               │   └─sf_column %in% names(g)
     13.               ├─base::NextMethod()
     14.               └─dplyr:::left_join.data.frame(.data, sum, by = idQN)
     15.                 └─dplyr:::join_mutate(...)
     16.                   └─rlang::set_names(x[vars$x$key], n
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       17. rlang::set_names(x[vars$x$key], names(vars$x$key))
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 44 | SKIPPED: 0 | WARNINGS: 2 | FAILED: 8 ]
      1. Error: (unknown) (@test_aw_aggregate.R#12) 
      2. Error: (unknown) (@test_aw_calculate.R#12) 
      3. Error: (unknown) (@test_aw_interpolate.R#23) 
      4. Error: (unknown) (@test_aw_preview_weights.R#22) 
      5. Failure: correctly specified functions execute without error (@test_aw_total.R#68) 
      6. Failure: correctly specified functions execute without error (@test_aw_total.R#70) 
      7. Error: (unknown) (@test_aw_verify.R#12) 
      8. Error: (unknown) (@test_aw_weight.R#12) 
      
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
* Number of recursive dependencies: 56

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

* Version: 1.2.1
* Source code: https://github.com/cran/BAwiR
* URL: https://www.R-project.org, https://www.uv.es/vivigui, https://www.uv.es/vivigui/AppEuroACB.html
* Date/Publication: 2020-02-05 14:00:03 UTC
* Number of recursive dependencies: 125

Run `revdep_details(,"BAwiR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Warning in archetypes(data, k = numArch[i], family = archetypesFamily("original",  :
      k=2: Error in qr.solve(alphas %*% t(alphas)): singular matrix 'a' in solve
    
    Warning in archetypes(data, k = numArch[i], family = archetypesFamily("original",  :
      k=2: Error in qr.solve(alphas %*% t(alphas)): singular matrix 'a' in solve
    
    > res <- archetypoids(2, preproc$data, huge = 200, step = FALSE, ArchObj = lass,
    +                     nearest = "cand_ns", sequ = TRUE)
    > cases <- anthrCases(res)
    > df3[cases,]
    # A tibble: 2 x 3
    # Groups:   Name [2]
         MP   PTS Name          
      <dbl> <int> <chr>         
    1    41     6 Alocen, Carlos
    2   409   172 Fisher, Corey 
    > alphas <- round(res$alphas, 4)
    > df3_aux <- df2[which(df2$Position == "Guard")[1:31], ]
    > get_similar_players(1, 0.99, alphas, cases, df3_aux, c("MP", "PTS"), 
    +                     unique(df3_aux$Compet), unique(df3_aux$Season))
    New names:
    ```

# BayesMallows

<details>

* Version: 0.4.1
* Source code: https://github.com/cran/BayesMallows
* URL: https://github.com/osorensen/BayesMallows
* Date/Publication: 2019-09-05 10:20:06 UTC
* Number of recursive dependencies: 92

Run `revdep_details(,"BayesMallows")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      > library(BayesMallows)
      > 
      > test_check("BayesMallows")
      ── 1. Failure: transitive closure generation works (@test_transitive_closure.R#3
      `pair_comp_tc` not equal to `pair_comp_returned`.
      Attributes: < Component "class": Lengths (4, 2) differ (string compare on first 2) >
      Attributes: < Component "class": 1 string mismatch >
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 171 | SKIPPED: 1 | WARNINGS: 0 | FAILED: 1 ]
      1. Failure: transitive closure generation works (@test_transitive_closure.R#39) 
      
      Error: testthat unit tests failed
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
      installed size is  6.5Mb
      sub-directories of 1Mb or more:
        R     1.8Mb
        doc   4.1Mb
    ```

# bdl

<details>

* Version: 1.0.1
* Source code: https://github.com/cran/bdl
* URL: https://github.com/statisticspoland/R_Package_to_API_BDL
* BugReports: https://github.com/statisticspoland/R_Package_to_API_BDL/issues
* Date/Publication: 2020-02-29 23:10:07 UTC
* Number of recursive dependencies: 105

Run `revdep_details(,"bdl")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(testthat)
      > library(bdl)
      > 
      > test_check("bdl")
      ── 1. Failure: Proper data (@test-requests.R#72)  ──────────────────────────────
      get_data_by_unit(...) not equal to `df`.
      Attributes: < Component "class": Lengths (4, 3) differ (string compare on first 3) >
      Attributes: < Component "class": 3 string mismatches >
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 38 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Failure: Proper data (@test-requests.R#72) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# beadplexr

<details>

* Version: 0.3.0
* Source code: https://github.com/cran/beadplexr
* URL: https://gitlab.com/ustervbo/beadplexr
* BugReports: https://gitlab.com/ustervbo/beadplexr/issues
* Date/Publication: 2020-02-05 17:00:02 UTC
* Number of recursive dependencies: 126

Run `revdep_details(,"beadplexr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    
    
    Attaching package: ‘drc’
    
    The following objects are masked from ‘package:stats’:
    
        gaussian, getInitial
    
    > data(ryegrass)
    > 
    > ryegrass_m <-
    +   fit_standard_curve(.data = ryegrass,
    +                      .parameter = "rootl",
    +                      .concentration = "conc")
    > 
    > sample_data <-
    +   calculate_concentration(.data = ryegrass[sample(1:nrow(ryegrass), 5),],
    +                           .model = ryegrass_m,
    +                           .parameter = "rootl")
    Error: Can't column-bind data frames with different row names.
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > 
      > test_check("beadplexr")
      ── 1. Failure: Despeckle works (@test_despeckle.R#21)  ─────────────────────────
      despeckle(.data, .parameters = c("FL6-H", "FL2-H"), .neighbours = 1L) not equal to `.data`.
      Component "FSC-A": Mean relative difference: 0.3187929
      Component "SSC-A": Mean relative difference: 0.3573438
      Component "FL6-H": Mean relative difference: 0.1363093
      Component "FL2-H": Mean relative difference: 0.05954157
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 401 | SKIPPED: 0 | WARNINGS: 6 | FAILED: 1 ]
      1. Failure: Despeckle works (@test_despeckle.R#21) 
      
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
      [ OK: 12 | SKIPPED: 0 | WARNINGS: 2 | FAILED: 1 ]
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

# breathtestcore

<details>

* Version: 0.5.0
* Source code: https://github.com/cran/breathtestcore
* URL: https://github.com/dmenne/breathtestcore
* BugReports: https://github.com/dmenne/breathtestcore/issues
* Date/Publication: 2020-03-01 10:40:03 UTC
* Number of recursive dependencies: 88

Run `revdep_details(,"breathtestcore")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
        2. breathtestcore:::cleanup_data.data.frame(data)
        4. dplyr:::distinct.data.frame(data)
        7. tibble:::as_tibble.data.frame(out)
        8. tibble:::as_tibble.list(unclass(x), ..., .rows = .rows, .name_repair = .name_repair)
        9. tibble:::lst_to_tibble(x, .rows, .name_repair, col_lengths(x))
       10. tibble:::set_repaired_names(x, .name_repair)
       12. tibble:::repaired_names(names(x), .name_repair = .name_repair)
       13. tibble:::check_unique(new_name)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 357 | SKIPPED: 5 | WARNINGS: 1 | FAILED: 1 ]
      1. Error: Columns without names are renamed (@test_cleanup_data.R#71) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘breathteststan’
    ```

# breathteststan

<details>

* Version: 0.4.7
* Source code: https://github.com/cran/breathteststan
* URL: https://github.com/dmenne/breathteststan
* BugReports: https://github.com/dmenne/breathteststan/issues
* Date/Publication: 2018-11-07 08:50:03 UTC
* Number of recursive dependencies: 129

Run `revdep_details(,"breathteststan")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        3. dplyr::mutate(., group = as.factor(.$group))
       10. dplyr::group_by_(., "parameter", "method")
       11. dplyr:::lazy_deprec("group_by")
       12. lifecycle::deprecate_warn(...)
       16. base::warning(wrn)
       17. base::withRestarts(...)
       18. base:::withOneRestart(expr, restarts[[1L]])
       19. base:::doWithOneRestart(return(expr), restart)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 20 | SKIPPED: 7 | WARNINGS: 0 | FAILED: 1 ]
      1. Error: Result with default parameters is tbl_df with required columns (@test_coef_by_group.R#18) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking line endings in C/C++/Fortran sources/headers ... NOTE
    ```
    Found the following sources/headers with CR or CRLF line endings:
      inst/include/meta_header.hpp
    Some Unix compilers require LF line endings.
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
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
        traceplot
    
    Error: Column name `term` must not be duplicated.
    Use .name_repair to specify repair.
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
      9.       └─tibble:::as_tibble.list(unclass(x), ..., .rows = .rows, .name_repair = .name_repair)
     10.         └─tibble:::lst_to_tibble(x, .rows, .name_repair, col_lengths(x))
     11.           └─tibble:::set_repaired_names(x, .name_repair)
     12.             ├─rlang::set_names(x, repaired_names(names(x), .name_repair = .name_repair))
     13.             └─tibble:::repaired_names(names(x), .name_repair = .name_repair)
     14.               
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      
      x[2]: "sin(2 * pi * Time)"
      y[2]: "2"
      
      x[3]: "cos(2 * pi * Time)"
      y[3]: "3"
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 187 | SKIPPED: 0 | WARNINGS: 6 | FAILED: 3 ]
      1. Error: tidy returns indexes if requested on rstanarm fits (@test-mcmc.R#21) 
      2. Error: mcmc with ess (@test-mcmc.R#86) 
      3. Failure: basic gls tidying (@test-nlme.R#146) 
      
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
* Number of recursive dependencies: 70

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
* Number of recursive dependencies: 108

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

# chilemapas

<details>

* Version: 0.1.4
* Source code: https://github.com/cran/chilemapas
* URL: https://pachamaltese.github.io/chilemapas/
* BugReports: https://github.com/pachamaltese/chilemapas/issues
* Date/Publication: 2020-01-24 18:50:02 UTC
* Number of recursive dependencies: 92

Run `revdep_details(,"chilemapas")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Joining, by = "codigo_comuna"
    Error: `nm` must be `NULL` or a character vector the same length as `x`
    Backtrace:
         █
      1. └─chilemapas::generar_circunscripciones()
      2.   └─`%>%`(...)
      3.     ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      4.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5.       └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      6.         └─chilemapas:::`_fseq`(`_lhs`)
      7.           └─magrittr::freduce(value, `_function_list`)
      8.             └─function_list[[i]](value)
      9.               ├─dplyr::left_join(...)
     10.               ├─sf:::left_join.sf(...)
     11.               │ └─sf:::sf_join(NextMethod(), attr(x, "sf_column"), suffix[1])
     12.               │   └─sf_column %in% names(g)
     13.               ├─base::NextMethod()
     14.               └─dplyr:::left_join.data.frame(...)
     15.                 └─dplyr:::join_mutate(...)
     16.                   └─rlang::set_names(x[vars$x$key], names(vars$x$key))
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── 1. Error: chilemapas datasets can be aggregated (@test-aggregation.R#8)  ────
      `nm` must be `NULL` or a character vector the same length as `x`
      Backtrace:
        1. chilemapas::generar_provincias(r14)
        2. rmapshaper::ms_dissolve(mapa, field = "codigo_provincia")
       10. dplyr::left_join(...)
       16. dplyr:::join_mutate(...)
       17. rlang::set_names(x[vars$x$key], names(vars$x$key))
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 1 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Error: chilemapas datasets can be aggregated (@test-aggregation.R#8) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# chunked

<details>

* Version: 0.4.1
* Source code: https://github.com/cran/chunked
* URL: https://github.com/edwindj/chunked
* BugReports: https://github.com/edwindj/chunked/issues
* Date/Publication: 2020-03-08 13:00:03 UTC
* Number of recursive dependencies: 43

Run `revdep_details(,"chunked")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── 2. Error: write_chunkwise to db works (@test-write.R#29)  ───────────────────
      `new` must be a tibble
      Backtrace:
       1. chunked::write_chunkwise(iris2, tmp, row.names = FALSE)
       2. chunked:::write_chunkwise.tbl_sql(iris2, tmp, row.names = FALSE)
       6. dplyr::count(x)
       7. dplyr::dplyr_reconstruct(out, x)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 35 | SKIPPED: 0 | WARNINGS: 2 | FAILED: 2 ]
      1. Error: (unknown) (@test-verbs.R#69) 
      2. Error: write_chunkwise to db works (@test-write.R#29) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking S3 generic/method consistency ... WARNING
    ```
    group_modify:
      function(.data, .f, ..., keep)
    group_modify.chunkwise:
      function(.tbl, .f, ..., keep)
    
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

# codebook

<details>

* Version: 0.8.2
* Source code: https://github.com/cran/codebook
* URL: https://github.com/rubenarslan/codebook
* BugReports: https://github.com/rubenarslan/codebook/issues
* Date/Publication: 2020-01-09 16:20:07 UTC
* Number of recursive dependencies: 176

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

# compareDF

<details>

* Version: 2.0.1
* Source code: https://github.com/cran/compareDF
* Date/Publication: 2020-02-28 14:40:02 UTC
* Number of recursive dependencies: 52

Run `revdep_details(,"compareDF")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── 5. Failure: (unknown) (@test-fnsComparison.R#219)  ──────────────────────────
      `expected_comparison_df` not equal to ctable$comparison_df.
      Attributes: < Component "row.names": Modes: numeric, character >
      Attributes: < Component "row.names": target is numeric, current is character >
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 50 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 5 ]
      1. Failure: (unknown) (@test-fnsComparison.R#151) 
      2. Failure: (unknown) (@test-fnsComparison.R#180) 
      3. Failure: (unknown) (@test-fnsComparison.R#191) 
      4. Failure: (unknown) (@test-fnsComparison.R#207) 
      5. Failure: (unknown) (@test-fnsComparison.R#219) 
      
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
      [ OK: 260 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 2 ]
      1. Failure: as_widecr.default throws an error if no column is matched (@test-results-widecr.R#133) 
      2. Failure: get_matchups works (@test-utils.R#177) 
      
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
* Number of recursive dependencies: 48

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

# confoundr

<details>

* Version: 1.2
* Source code: https://github.com/cran/confoundr
* BugReports: https://github.com/jwjackson/confoundr/issues
* Date/Publication: 2019-09-20 04:40:02 UTC
* Number of recursive dependencies: 80

Run `revdep_details(,"confoundr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Error: `summarise()` argument `nexpval` errored.
    ℹ `nexpval` is `n_distinct(.data$E)`.
    ℹ The error occured in group 1: H = "H", time.exposure = 0.
    ✖ Column `E` not found in `.data`
    Backtrace:
         █
      1. └─confoundr::balance(...)
      2.   └─`%>%`(...)
      3.     ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      4.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5.       └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      6.         └─confoundr:::`_fseq`(`_lhs`)
      7.           └─magrittr::freduce(value, `_function_list`)
      8.             └─function_list[[i]](value)
      9.               ├─dplyr::summarise(., nexpval = n_distinct(.data$E))
     10.               ├─dplyr:::summarise.grouped_df(., nexpval = n_distinct(.data$E))
     11.               ├─base::NextMethod()
     12.               └─dplyr:::summarise.data.frame(., nexpval = n_distinct(.data$E))
     13.                 └─dplyr:::summarise_cols(.data, ...)
     14.                
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 285 | SKIPPED: 0 | WARNINGS: 26 | FAILED: 16 ]
      1. Error: diagnostic 1, recent, recency 1 (@testBalanceWApplyScope.r#24) 
      2. Error: diagnostic 2, average over strata (@testBalanceWApplyScope.r#78) 
      3. Error: diagnostic 1, average over history (@testBalanceWApplyScope.r#130) 
      4. Error: diagnostic 1, average over time (@testBalanceWApplyScope.r#180) 
      5. Error: diagnostic 1, average within periods of distance (@testBalanceWApplyScope.r#228) 
      6. Error: diagnostic 1, average over distance (@testBalanceWApplyScope.r#277) 
      7. Error: diagnostic 1, no censoring, scope all (@testBalanceWOApplyScope.r#26) 
      8. Error: diagnostic 1, censoring, scope all (@testBalanceWOApplyScope.r#82) 
      9. Error: diagnostic 2, no censoring, weight, scope all (@testBalanceWOApplyScope.r#138) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

# corrr

<details>

* Version: 0.4.1
* Source code: https://github.com/cran/corrr
* URL: https://github.com/tidymodels/corrr
* BugReports: https://github.com/tidymodels/corrr/issues
* Date/Publication: 2020-02-10 21:50:13 UTC
* Number of recursive dependencies: 107

Run `revdep_details(,"corrr")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > 
      > test_check("corrr")
      ── 1. Error: tbl_sql routine's results are within the 0.01 threshold (@test-tbl_
      `x` must be a vector, not a `data.frame/tbl_sql` object.
      Backtrace:
        1. testthat::expect_false(...)
       17. vctrs:::stop_scalar_type(...)
       18. vctrs:::stop_vctrs(msg, "vctrs_error_scalar_type", actual = x)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 83 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Error: tbl_sql routine's results are within the 0.01 threshold (@test-tbl_sql.R#31) 
      
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
      [ OK: 116 | SKIPPED: 23 | WARNINGS: 7 | FAILED: 20 ]
      1. Failure: group_by errors if assigned columns not in dataset (@test-group-by.R#22) 
      2. Failure: group_by errors if assigned columns not in dataset (@test-group-by.R#23) 
      3. Error: group_by hidden variables (@test-group-by.R#29) 
      4. Failure: When group_by calls mutate, it also errors nicely (@test-mutate.R#17) 
      5. Failure: summarize makes a cube request (@test-summarize.R#18) 
      6. Failure: summarize makes a cube request (@test-summarize.R#20) 
      7. Failure: summarize can handle multiple measures (@test-summarize.R#26) 
      8. Failure: summarize can handle multiple measures (@test-summarize.R#28) 
      9. Failure: summarize can handle multiple measures (@test-summarize.R#29) 
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

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      ── 2. Failure: Summary(multi_cutpointr) is silent (@test-cutpointr.R#1306)  ────
      `... <- NULL` produced messages.
      
      ── 3. Failure: Summary(multi_cutpointr) is silent (@test-cutpointr.R#1320)  ────
      `... <- NULL` produced messages.
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 376 | SKIPPED: 0 | WARNINGS: 1 | FAILED: 3 ]
      1. Error: add_metric adds metrics correctly (@test-cutpointr.R#1231) 
      2. Failure: Summary(multi_cutpointr) is silent (@test-cutpointr.R#1306) 
      3. Failure: Summary(multi_cutpointr) is silent (@test-cutpointr.R#1320) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# dat

<details>

* Version: 0.4.0
* Source code: https://github.com/cran/dat
* BugReports: https://github.com/wahani/dat/issues
* Date/Publication: 2018-01-20 15:36:18 UTC
* Number of recursive dependencies: 71

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
       36. dat:::`[.DataFrame`(.data, 0)
       16. memClassHandler$memClass(.)
       16. dat:::handleRows(., dispatcher(i))
       23. dat:::handleCols(...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 76 | SKIPPED: 1 | WARNINGS: 4 | FAILED: 5 ]
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
* Number of recursive dependencies: 56

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

# datos

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/datos
* URL: https://github.com/cienciadedatos/datos
* BugReports: https://github.com/cienciadedatos/datos/issues
* Date/Publication: 2019-09-25 09:10:03 UTC
* Number of recursive dependencies: 71

Run `revdep_details(,"datos")` for more info

</details>

## Newly broken

*   checking for missing documentation entries ... ERROR
    ```
    Error: Corrupt grouped_df data using the old format
    Backtrace:
         █
      1. ├─tools::undoc(package = "datos")
      2. │ └─base::Filter(...)
      3. │   ├─base::unlist(lapply(x, f))
      4. │   └─base::lapply(x, f)
      5. │     └─tools:::FUN(X[[i]], ...)
      6. └─base::eval(...)
      7.   └─base::eval(...)
      8.     └─tools:::translate("common.yml")
      9.       ├─base::suppressWarnings(dplyr::group_vars(df))
     10.       │ └─base::withCallingHandlers(expr, warning = function(w) invokeRestart("muffleWarning"))
     11.       ├─dplyr::group_vars(df)
     12.       └─dplyr:::group_vars.data.frame(df)
     13.         ├─dplyr::setdiff(names(group_data(x)), ".rows")
     14.         ├─dplyr::group_data(x)
     15.         └─dplyr:::group_data.grouped_df(x)
     16.           └─dplyr::validate_grouped_df(.data)
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > test_check("datos")
      ── 1. Error: Load from spec works for:comunes (@test-translations.R#25)  ───────
      Corrupt grouped_df data using the old format
      Backtrace:
       1. datos:::translate(x$file)
       5. dplyr:::group_vars.data.frame(df)
       8. dplyr:::group_data.grouped_df(x)
       9. dplyr::validate_grouped_df(.data)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 153 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Error: Load from spec works for:comunes (@test-translations.R#25) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking S3 generic/method consistency ... WARNING
    ```
    ...
    Backtrace:
         █
      1. ├─tools::checkS3methods(package = "datos")
      2. │ └─base::Filter(function(f) is.function(code_env[[f]]), objects_in_code)
      3. │   ├─base::unlist(lapply(x, f))
      4. │   └─base::lapply(x, f)
      5. │     └─tools:::FUN(X[[i]], ...)
      6. └─base::eval(...)
      7.   └─base::eval(...)
      8.     └─tools:::translate("common.yml")
      9.       ├─base::suppressWarnings(dplyr::group_vars(df))
     10.       │ └─base::withCallingHandlers(expr, warning = function(w) invokeRestart("muffleWarning"))
     11.       ├─dplyr::group_vars(df)
     12.       └─dplyr:::group_vars.data.frame(df)
     13.         ├─dplyr::setdiff(names(group_data(x)), ".rows")
     14.         ├─dplyr::group_data(x)
     15.         └─dplyr:::group_data.grouped_df(x)
     16.           └─dplyr::validate_grouped_df(.data)
    Execution halted
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking for code/documentation mismatches ... WARNING
    ```
    ...
    Error: Corrupt grouped_df data using the old format
    Backtrace:
         █
      1. ├─tools::codoc(package = "datos")
      2. │ └─base::Filter(...)
      3. │   ├─base::unlist(lapply(x, f))
      4. │   └─base::lapply(x, f)
      5. │     └─tools:::FUN(X[[i]], ...)
      6. │       └─base::get(f, envir = code_env)
      7. └─base::eval(...)
      8.   └─base::eval(...)
      9.     └─tools:::translate("common.yml")
     10.       ├─base::suppressWarnings(dplyr::group_vars(df))
     11.       │ └─base::withCallingHandlers(expr, warning = function(w) invokeRestart("muffleWarning"))
     12.       ├─dplyr::group_vars(df)
     13.       └─dplyr:::group_vars.data.frame(df)
     14.         ├─dplyr::setdiff(names(group_data(x)), ".rows")
     15.         ├─dplyr::group_data(x)
     16.         └─dplyr:::group_data.grouped_df(x)
     17.           └─dplyr::validate_grouped_df(.data)
    Execution halted
    ```

*   checking Rd \usage sections ... NOTE
    ```
    ...
      2. │ └─base::Filter(...)
      3. │   ├─base::unlist(lapply(x, f))
      4. │   └─base::lapply(x, f)
      5. │     └─tools:::FUN(X[[i]], ...)
      6. │       └─base::get(f, envir = code_env)
      7. └─base::eval(...)
      8.   └─base::eval(...)
      9.     └─tools:::translate("common.yml")
     10.       ├─base::suppressWarnings(dplyr::group_vars(df))
     11.       │ └─base::withCallingHandlers(expr, warning = function(w) invokeRestart("muffleWarning"))
     12.       ├─dplyr::group_vars(df)
     13.       └─dplyr:::group_vars.data.frame(df)
     14.         ├─dplyr::setdiff(names(group_data(x)), ".rows")
     15.         ├─dplyr::group_data(x)
     16.         └─dplyr:::group_data.grouped_df(x)
     17.           └─dplyr::validate_grouped_df(.data)
    Execution halted
    The \usage entries for S3 methods should use the \method markup and not
    their full name.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Error loading dataset 'comunes':
       Error : Corrupt grouped_df data using the old format
      
      Note: found 259 marked UTF-8 strings
    ```

# dbparser

<details>

* Version: 1.1.0
* Source code: https://github.com/cran/dbparser
* URL: https://docs.ropensci.org/dbparser, https://github.com/ropensci/dbparser
* BugReports: https://github.com/ropensci/dbparser/issues
* Date/Publication: 2020-02-21 05:40:03 UTC
* Number of recursive dependencies: 60

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
* Number of recursive dependencies: 68

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

* Version: 1.13
* Source code: https://github.com/cran/ddpcr
* URL: https://github.com/daattali/ddpcr
* BugReports: https://github.com/daattali/ddpcr/issues
* Date/Publication: 2020-02-28 07:20:18 UTC
* Number of recursive dependencies: 88

Run `revdep_details(,"ddpcr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    ℹ `drops` is `n()`.
    ℹ The error occured in group 1: well = "A01".
    ✖ could not find function "n"
    Backtrace:
         █
      1. └─ddpcr::new_plate(sample_data_dir())
      2.   └─ddpcr:::init_plate(plate)
      3.     └─`%>%`(...)
      4.       ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      5.       └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      6.         └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      7.           └─ddpcr:::`_fseq`(`_lhs`)
      8.             └─magrittr::freduce(value, `_function_list`)
      9.               ├─base::withVisible(function_list[[k]](value))
     10.               └─function_list[[k]](value)
     11.                 └─ddpcr:::init_meta(.)
     12.                   └─`%>%`(...)
     13.                     ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
     14.                     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
     15.                       └
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 157 | SKIPPED: 0 | WARNINGS: 8 | FAILED: 19 ]
      1. Error: (unknown) (@test-custom_thresholds.R#4) 
      2. Error: get_empty_cutoff works (@test-empty.R#4) 
      3. Error: get_empty_cutoff for pnpp works (@test-empty.R#18) 
      4. Error: is_well_success works (@test-failures.R#4) 
      5. Error: get_outlier_cutoff works (@test-outliers.R#4) 
      6. Error: test basic plate attribute getters/setters (@test-plate-attribs.R#4) 
      7. Error: (unknown) (@test-plate.R#3) 
      8. Failure: get_filled_drops works (@test-pnpp_experiment-filled.R#22) 
      9. Error: (unknown) (@test-pnpp_experiment.R#4) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

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

# dexterMST

<details>

* Version: 0.1.2
* Source code: https://github.com/cran/dexterMST
* URL: http://dexterities.netlify.com
* BugReports: https://github.com/jessekps/dexter/issues
* Date/Publication: 2019-08-20 10:20:02 UTC
* Number of recursive dependencies: 79

Run `revdep_details(,"dexterMST")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    ℹ `distinct_scores` is `n_distinct(.data$item_score)`.
    ℹ The error occured in group 1: item_id = "item01".
    ✖ Column `item_score` not found in `.data`
    Backtrace:
         █
      1. └─dexterMST::add_scoring_rules_mst(db, scoring_rules)
      2.   └─`%>%`(...)
      3.     ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      4.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5.       └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      6.         └─dexterMST:::`_fseq`(`_lhs`)
      7.           └─magrittr::freduce(value, `_function_list`)
      8.             └─function_list[[i]](value)
      9.               ├─dplyr::summarise(...)
     10.               ├─dplyr:::summarise.grouped_df(...)
     11.               ├─base::NextMethod()
     12.               └─dplyr:::summarise.data.frame(...)
     13.                 └─dplyr:::summarise_cols(.data, ...)
     14.                   └─base::tryCatch(...)
     15.
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      =====
      ======
      ── 2. Failure: can import from dexter and calbration comparable to dexter (@test
      `.` isn't true.
      dexter and dexterMST profile tables not equivalent
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 8 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 2 ]
      1. Error: we can calibrate (@test_calibration.R#106) 
      2. Failure: can import from dexter and calbration comparable to dexter (@test_inputs.R#90) 
      
      Error: testthat unit tests failed
      In addition: Warning message:
      call dbDisconnect() when finished working with a connection 
      Execution halted
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

* Version: 0.0.4
* Source code: https://github.com/cran/dials
* URL: https://tidymodels.github.io/dials, https://github.com/tidymodels/dials
* BugReports: https://github.com/tidymodels/dials/issues
* Date/Publication: 2019-12-02 06:50:02 UTC
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
       26. dials:::`[.parameters`(.data, 0)
       27. dials:::check_new_names(res)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 270 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Error: dplyr ops (@test_dplyr_set_compat.R#33) 
      
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
    
    > ### Name: rechunk
    > ### Title: Increase or decrease the number of chunks in the disk.frame
    > ### Aliases: rechunk
    > 
    > ### ** Examples
    > 
    > # create a disk.frame with 2 chunks in tempdir()
    > cars.df = as.disk.frame(cars, nchunks = 2)
    > 
    > # re-chunking cars.df to 3 chunks, done "in-place" to the same folder as cars.df
    > rechunk(cars.df, 3)
<<<<<<< Updated upstream
    files have been backed up to temporary dir /var/folders/4b/hn4fq98s6810s4ccv2f9hm2h0000gn/T//RtmpDWr17Z/back_up_tmp_dir146c375ca2a2e. You can recover there files until you restart your R session
=======
    files have been backed up to temporary dir /var/folders/4b/hn4fq98s6810s4ccv2f9hm2h0000gn/T//RtmpGer1jY/back_up_tmp_dir2b3818fd18a0. You can recover there files until you restart your R session
>>>>>>> Stashed changes
    Error in .(colnames, coltypes) : could not find function "."
    Calls: rechunk -> <Anonymous> -> [ -> [.data.frame
    Execution halted
    ```

<<<<<<< Updated upstream
# dlookr

<details>

* Version: 0.3.13
* Source code: https://github.com/cran/dlookr
* BugReports: https://github.com/choonghyunryu/dlookr/issues
* Date/Publication: 2020-01-09 07:00:02 UTC
* Number of recursive dependencies: 165

Run `revdep_details(,"dlookr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
      [1] 11 16 10  4  3 13  0 15  0  0  9  4  2 11 11  5  0 13  0 16  2 12  6  0 16
     [26]  0 11  0  0 15  0 16 12 13  0 11  0  5  0  0  0  0  0 11  6  0 14  0  0  0
     [51] 18  0  3 13 13  5  0  0 15  4 19  0  0 10 12  0  0 14 20  0 15 16  0 10  5
     [76] 23 10 12  1  0 16  0  4  7  0  0  9  7  7  3  0 11  0  0  5 10 10  5 24  3
    [101] 11  0  0  0  0  8  0  0  2  0  7 12  5 11  9  0  0  0  2  8 11 10  5  0  0
    [126]  0  2  3  3  7 13  3  9  2  0 14  0  0 12 10 10  0  0  7  0 11  0  9  0 13
    [151]  8 17  0  7 10  0  0  8  1  0  0  5  0  0  0  7 17  0  0 15 12 12 13  5  0
    [176]  0  9  0 14  3 15  0  4  6  7 11  0  0  0 18 13 13  0  7 18  4  6  0  5  5
    [201]  0  0  4  0  0  1  0  0  0 11  2 14 19  5  3 15  0  0 12 19 15  0  6  9  0
    [226]  0  0 10 13  0  0  0 10 18 11  8 16  8  0  0  0  0  0 13  0  0 20  0  0  0
    [251] 10  5  0  5 23  8  0 14  0 10  8  4 15  6  5 10 12  7  0  0  0  0  0  8  2
    [276] 11 14 12  2 13 10  7  0  0 11 11 11  4  0 25 14  0 16  0  3 14 13 13  0 17
    [301]  1  0 13 16 12 26  1  0 19 13 29 12  5  3 10  8  5  0 10 19 12  5 10 18  4
    [326] 11  0 17  1  9  0 15 20  7  9 15  6  0  0  4  0  0 13 10  0  0  0  0 20 18
    [351] 17 16 14 12  1  0  0  3 10 11  7 10  0  1 16  0 11  0 10 22 22  0  0  0  7
    [376]  4 19  0  3  0 10 21 19  0 15 13  0 14 11  8  9  0 13 10 19 17  3 12  7  0
    attr(,"class")
    [1] "transform" "numeric"  
    > summary(advertising_minmax)
    Warning: `cols` is now required.
    Please use `cols = c(statistic)`
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        doc   4.4Mb
    ```

=======
>>>>>>> Stashed changes
# docxtools

<details>

* Version: 0.2.1
* Source code: https://github.com/cran/docxtools
* URL: https://graphdr.github.io/docxtools
* BugReports: https://github.com/graphdr/docxtools/issues
* Date/Publication: 2019-02-09 18:43:13 UTC
* Number of recursive dependencies: 74

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
* Number of recursive dependencies: 68

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
    Error: Corrupt grouped_df data using the old format
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
     11.                 ├─dplyr::setdiff(group_names, tbl_vars(out))
     12.                 ├─dplyr:::setdiff.default(group_names, tbl_vars(out))
     13.                 │ └─base::setdiff(x, y, ...)
     14.                 │   └─base::as.vector(y)
     15.                 └─dplyr::tbl_vars(out)
     16.     
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dae’ ‘dplyr’
      All declared Imports should be used.
    ```

# EdSurvey

<details>

* Version: 2.4.0
* Source code: https://github.com/cran/EdSurvey
* URL: https://www.air.org/project/nces-data-r-project-edsurvey
* Date/Publication: 2020-01-10 22:50:05 UTC
* Number of recursive dependencies: 118

Run `revdep_details(,"EdSurvey")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat_tests.R’ failed.
    Last 13 lines of output:
      ── 1. Error: dplyr integration (@test-0-main.R#872)  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
      dims [product 1150944] do not match the length of object [76]
      Backtrace:
        1. EdSurvey::getData(...)
        8. dplyr::mutate(...)
       11. dplyr:::dplyr_col_modify.data.frame(.data, cols)
       12. vctrs::vec_data(data)
       13. vctrs:::vec_set_attributes(x, list(dim = dim(x), dimnames = dimnames(x)))
      
      ══ testthat results  ═══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
      [ OK: 160 | SKIPPED: 47 | WARNINGS: 0 | FAILED: 1 ]
      1. Error: dplyr integration (@test-0-main.R#872) 
      
      Error: testthat unit tests failed
      Execution halted
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
* Number of recursive dependencies: 80

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
    Running examples in ‘eph-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: map_agglomerates
    > ### Title: Mapa de indicadores por aglomerado
    > ### Aliases: map_agglomerates
    > 
    > ### ** Examples
    > 
    > 
    > toybase_individual_2016_04 %>%
    + dplyr::group_by(AGLOMERADO) %>%
    + dplyr::summarise(tasa_actividad = sum(PONDERA[ESTADO==1])/sum(PONDERA)) %>%
    + map_agglomerates(agglomerates = AGLOMERADO,indicator = tasa_actividad)
    Error: Can't slice a scalar
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
       5. dplyr::count(f)
       6. dplyr::dplyr_reconstruct(out, x)
      
      ── 2. Failure: case_fatality_rate_df is equivalent to the non-df version (@test-
      `iris_res` not equal to `iris_expect`.
      Attributes: < Component "class": Lengths (3, 1) differ (string compare on first 1) >
      Attributes: < Component "class": 1 string mismatch >
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 104 | SKIPPED: 1 | WARNINGS: 0 | FAILED: 2 ]
      1. Error: fmt_count() works as expected (@test-inline_fun.R#65) 
      2. Failure: case_fatality_rate_df is equivalent to the non-df version (@test-proportion.R#74) 
      
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
* Number of recursive dependencies: 45

Run `revdep_details(,"estatapi")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      `r` not equal to param$expect.
      Attributes: < names for target but not for current >
      Attributes: < Length mismatch: comparison on first 0 components >
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 17 | SKIPPED: 12 | WARNINGS: 0 | FAILED: 6 ]
      1. Failure: test calc_range (@test-calc-range.R#30) 
      2. Failure: test calc_range (@test-calc-range.R#30) 
      3. Failure: test calc_range (@test-calc-range.R#30) 
      4. Failure: test calc_range (@test-calc-range.R#30) 
      5. Failure: test calc_range (@test-calc-range.R#30) 
      6. Failure: test calc_range (@test-calc-range.R#30) 
      
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
    Warning: `funs()` is deprecated as of dplyr 1.0.0.
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
* Number of recursive dependencies: 52

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
      [ OK: 19 | SKIPPED: 0 | WARNINGS: 1 | FAILED: 1 ]
      1. Error: ind_to_char_ works with grouped_df, tbl_df, tbl, data.frame (@test_grp_routine.R#74) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# ezplot

<details>

* Version: 0.3.1
* Source code: https://github.com/cran/ezplot
* Date/Publication: 2019-07-20 21:20:03 UTC
* Number of recursive dependencies: 87

Run `revdep_details(,"ezplot")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(testthat)
      > library(ezplot)
      > 
      > test_check("ezplot")
      ── 1. Failure: agg_data base cases (@test-agg_data.R#37)  ──────────────────────
      agg_data(mtcars, c(cyl = "as.character(cyl)")) not equal to mtcars %>% distinct(cyl = as.character(cyl)) %>% arrange(cyl).
      Attributes: < Component "row.names": Modes: numeric, character >
      Attributes: < Component "row.names": target is numeric, current is character >
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 60 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Failure: agg_data base cases (@test-agg_data.R#37) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# fabletools

<details>

* Version: 0.1.2
* Source code: https://github.com/cran/fabletools
* URL: http://fabletools.tidyverts.org/
* BugReports: https://github.com/tidyverts/fabletools/issues
* Date/Publication: 2020-01-29 09:00:02 UTC
* Number of recursive dependencies: 92

Run `revdep_details(,"fabletools")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
        intersect, setdiff, setequal, union
    
    Error: Input must be a vector, not a `fcdist` object.
    Backtrace:
         █
      1. ├─fit %>% forecast(h = "5 years")
      2. │ ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      3. │ └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      4. │   └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5. │     └─`_fseq`(`_lhs`)
      6. │       └─magrittr::freduce(value, `_function_list`)
      7. │         ├─base::withVisible(function_list[[k]](value))
      8. │         └─function_list[[k]](value)
      9. │           ├─fabletools::forecast(., h = "5 years")
     10. │           └─fabletools:::forecast.mdl_df(., h = "5 years")
     11. │             ├─base::suppressWarnings(...)
     12. │             │ └─base::withCallingHandlers(expr, warning = function(w) invokeRestart("muffleWarning"))
     13. │             └─fabletools:::unnest_tsbl(...)
     14. │               └─fabletools:::unnest_tbl(.data, tsbl_col)
     15. │                 └─fable
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        1. ├─testthat::test_check("fabletools")
        2. │ └─testthat:::test_package_dir(...)
        3. │   └─testthat::test_dir(...)
        4. │     └─testthat::source_test_setup(path, env)
        5. │       └─testthat::source_dir(path, "^setup.*\\.[rR]$", env = env, wrap = FALSE)
        6. │         └─base::lapply(files, source_file, env = env, chdir = chdir, wrap = wrap)
        7. │           └─testthat:::FUN(X[[i]], ...)
        8. │             └─base::eval(exprs, env)
        9. │               └─base::eval(exprs, env)
       10. │                 └─mbl %>% forecast(h = 12) setup-data.R:14:0
       11. │                   ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
       12. │                   └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
       13. │                     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
       14. │                       └─fabletools:::`_fseq`(`_lhs`)
      Execution halted
    ```

## In both
<<<<<<< Updated upstream

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘feasts’
    ```

# feasts

<details>

* Version: 0.1.3
* Source code: https://github.com/cran/feasts
* URL: http://feasts.tidyverts.org/, https://github.com/tidyverts/feasts/
* BugReports: https://github.com/tidyverts/feasts/issues
* Date/Publication: 2020-03-18 07:00:11 UTC
* Number of recursive dependencies: 92

Run `revdep_details(,"feasts")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    +   model(STL(value ~ trend(window = 10))) %>%
    +   components()
    Error: Can't cast <lst_mdl> to <lst_mdl>.
    Backtrace:
         █
      1. ├─`%>%`(...)
      2. │ ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      3. │ └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      4. │   └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5. │     └─`_fseq`(`_lhs`)
      6. │       └─magrittr::freduce(value, `_function_list`)
      7. │         └─function_list[[i]](value)
      8. │           ├─fabletools::model(., STL(value ~ trend(window = 10)))
      9. │           └─fabletools:::model.tbl_ts(., STL(value ~ trend(window = 10)))
     10. │             └─.data %>% transmute(!!!keys, !!!fits) %>% as_mable(keys, names(fits))
     11. │               ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
     12. │               └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
     13. │                 └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
     14. │                   └─fabletools:::`_fseq`(`_lhs`)
     15. │  
    Execution halted
    ```

## In both

*   checking tests ...
=======

*   checking package dependencies ... NOTE
>>>>>>> Stashed changes
    ```
    Package suggested but not available for checking: ‘feasts’
    ```

# fgeo.analyze

<details>

* Version: 1.1.12
* Source code: https://github.com/cran/fgeo.analyze
* URL: https://github.com/forestgeo/fgeo.analyze
* BugReports: https://github.com/forestgeo/fgeo.analyze/issues
* Date/Publication: 2020-03-15 13:50:02 UTC
* Number of recursive dependencies: 87

Run `revdep_details(,"fgeo.analyze")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > 
    > abundance_byyr(vft, DBH >= 10, DBH < 20)
    Error: `summarise()` argument `n` errored.
    ℹ `n` is `dplyr::n_distinct(.data$treeid)`.
    ℹ The error occured in group 1: plotname = "luq", year = 2001, family = "f", species = "Gn spp".
    ✖ Column `treeid` not found in `.data`
    Backtrace:
         █
      1. └─fgeo.analyze::abundance_byyr(vft, DBH >= 10, DBH < 20)
      2.   └─`%>%`(...)
      3.     ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      4.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5.       └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      6.         └─fgeo.analyze:::`_fseq`(`_lhs`)
      7.           └─magrittr::freduce(value, `_function_list`)
      8.             └─function_list[[i]](value)
      9.               ├─dplyr::summarize(., n = dplyr::n_distinct(.data$treeid))
     10.               ├─dplyr:::summarise.grouped_df(., n = dplyr::n_distinct(.data$treeid))
     11.               ├─base::NextMethod()
     12.               └─dplyr:::summarise.data.frame(., n = dplyr::n_distinct(.da
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 269 | SKIPPED: 14 | WARNINGS: 1 | FAILED: 10 ]
      1.  Failure: as_tibble.demography_ctfs and as.data.frame.* output equal (@test-as_tibble.R#113) 
      2.  Error: basal_area_byyr and abundance_byyr fail with informative errors (@test-byyr.R#26) 
      3.  Error: abundance_byyr and basa_area_byyr return expected output: outputs basal area multiplied by the abundance (@test-byyr.R#127) 
      4.  Error: abundance_byyr and basa_area_byyr return expected output: outputs equal to known output (@test-byyr.R#143) 
      5.  Error: abundance_byyr: lowercases dbh and only dbh from the expression passed to ... (@test-byyr.R#163) 
      6.  Error: abundance_byyr: is sensitive to DBH, so outputs none date-column if dbh is too big  (@test-byyr.R#171) 
      7.  Error: abundance_byyr: outputs as expected (@test-byyr.R#189) 
      8.  Error: abundance_byyr: warns if parsed dates are not from 1980 to present (@test-byyr.R#212) 
      9.  Error: *byyr(): makes no difference if status is picked before *byyr() (@test-byyr.R#226) 
      10. Failure: summary.tt_lst and summary.tt_df return equal (@test-summary.R#59) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# fgeo.tool

<details>

* Version: 1.2.5
* Source code: https://github.com/cran/fgeo.tool
* URL: https://github.com/forestgeo/fgeo.tool
* BugReports: https://github.com/forestgeo/fgeo.tool/issues
* Date/Publication: 2019-06-17 19:30:03 UTC
* Number of recursive dependencies: 66

Run `revdep_details(,"fgeo.tool")` for more info

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
      > test_check("fgeo.tool")
      ── 1. Failure: add_gxgy handles NA (@test-add_var.R#41)  ───────────────────────
      is.na(add_gxgy(tree)$gx1) isn't true.
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 288 | SKIPPED: 9 | WARNINGS: 4 | FAILED: 1 ]
      1. Failure: add_gxgy handles NA (@test-add_var.R#41) 
      
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
    +                         dps = NA)
    Error: Names repair functions can't return `NA` values.
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
      9.               ├─base::withVisible(function_list[[k]](value))
     10.               └─function_list[[k]](value)
     11.                 ├─dplyr::rename(., mean = regionalvalue)
     12.                 └─dplyr:::rename.data.frame(., mean = regionalvalue)
     13.                   └─tidyselect::eval_rename(expr(c(...)), .data)
     14.                     └─tidyselect:::rename_impl(...)
     15.                       └─tidyselect:::with_subscript_errors(...)
    <
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/a-l.R’ failed.
    Last 13 lines of output:
        2. fingertipscharts:::spine_rescaler(...)
        3. tibble::rownames_to_column(., var = rlang::quo_text(indicator))
        3. base::merge(., mean, by = rlang::quo_text(indicator), all.x = TRUE)
       11. dplyr::rename(., mean = regionalvalue)
       13. tidyselect::eval_rename(expr(c(...)), .data)
       14. tidyselect:::rename_impl(...)
       15. tidyselect:::with_subscript_errors(...)
      
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
* Number of recursive dependencies: 99

Run `revdep_details(,"foieGras")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Error: `nm` must be `NULL` or a character vector the same length as `x`
    Backtrace:
         █
      1. ├─foieGras::fit_ssm(ellie, model = "rw", time.step = 24)
      2. │ └─`%>%`(...)
      3. │   ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      4. │   └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5. │     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      6. │       └─foieGras:::`_fseq`(`_lhs`)
      7. │         └─magrittr::freduce(value, `_function_list`)
      8. │           ├─base::withVisible(function_list[[k]](value))
      9. │           └─function_list[[k]](value)
     10. │             ├─dplyr::do(...)
     11. │             └─dplyr:::do.grouped_df(...)
     12. │               └─rlang::eval_tidy(args[[j]], mask)
     13. └─foieGras::prefilter(...)
     14.   └─`%>%`(...)
     15.     ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
     16.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
     17.       └─base::eval
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        1. foieGras::prefilter(ellie_sf, vmax = 10, ang = -1, min.dt = 120)
        2. dplyr::mutate(., lc = as.character(lc))
        9. dplyr::left_join(., tmp, by = "lc")
       15. dplyr:::join_mutate(...)
       16. rlang::set_names(x[vars$x$key], names(vars$x$key))
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 20 | SKIPPED: 14 | WARNINGS: 0 | FAILED: 4 ]
      1. Error: fit_ssm defaults + crw + KF return foieGras list w 15 elements (@test-fit_ssm.R#34) 
      2. Error: (unknown) (@test-join.R#7) 
      3. Error: (unknown) (@test-plot.R#7) 
      4. Error: (unknown) (@test-prefilter.R#7) 
      
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
* Number of recursive dependencies: 92

Run `revdep_details(,"forecastML")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > 
    > # Example - Training data for 3 horizon-specific models w/ common lags per predictor.
    > horizons <- c(1, 6, 12)
    > lookback <- 1:15
    > 
    > data_train <- create_lagged_df(data_seatbelts, type = "train", outcome_col = 1,
    +                                lookback = lookback, horizon = horizons)
    > 
    > # All historical window lengths of 12 plus any partial windows at the end of the dataset.
    > windows <- create_windows(data_train, window_length = 12)
    > plot(windows, data_train)
    Error: Can't subset with `[` using an object of class NULL.
    Backtrace:
        █
     1. ├─graphics::plot(windows, data_train)
     2. └─forecastML:::plot.windows(windows, data_train)
     3.   ├─base::apply(...)
     4.   ├─data_plot[, groups, drop = FALSE]
     5.   └─tibble:::`[.tbl_df`(data_plot, , groups, drop = FALSE)
     6.     └─tibble:::check_names_df(j, x)
    Execution halted
    ```

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

# fueleconomy

<details>

* Version: 0.1
* Source code: https://github.com/cran/fueleconomy
* URL: http://github.com/hadley/fueleconomy
* Date/Publication: 2014-07-22 11:08:43
* Number of recursive dependencies: 21

Run `revdep_details(,"fueleconomy")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    
    Error: Corrupt grouped_df data using the old format
    Backtrace:
         █
      1. └─vehicles %>% semi_join(common)
      2.   ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      3.   └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      4.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5.       └─`_fseq`(`_lhs`)
      6.         └─magrittr::freduce(value, `_function_list`)
      7.           ├─base::withVisible(function_list[[k]](value))
      8.           └─function_list[[k]](value)
      9.             ├─dplyr::semi_join(., common)
     10.             └─dplyr:::semi_join.data.frame(., common)
     11.               └─dplyr:::join_filter(x, y, by = by, type = "semi", na_matches = na_matches)
     12.                 ├─dplyr:::join_cols(tbl_vars(x), tbl_vars(y), by = by)
     13.                 │ └─dplyr:::check_duplicate_vars(y_names, "y")
     14.                 │   └─base::duplicated(vars)
     15.                 └─dplyr::tbl_vars(y)
     16.              
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
      [ OK: 265 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 13 ]
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
* Number of recursive dependencies: 71

Run `revdep_details(,"fxtract")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
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
      5. ├─vctrs:::vec_ptype2_dispatch_s3(x = x, y = y, x_arg = x_arg, y_arg = y_arg)
      6. ├─vctrs::vec_ptype2.logical(x = x, y = y, x_arg = x_arg, y_arg = y_arg)
      7. └─vctrs:::vec_ptype2.logical.default(...)
      8.   └─vctrs::vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
      9.     └─vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
     10.       └─vctrs:::stop_incompatible(...)
     11.         └─vctrs:::stop_vctrs(...)
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

# GADMTools

<details>

* Version: 3.8-1
* Source code: https://github.com/cran/GADMTools
* URL: https://github.com/IamKDO/GADMTools
* Date/Publication: 2020-03-05 12:30:08 UTC
* Number of recursive dependencies: 106

Run `revdep_details(,"GADMTools")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > DF <- data.frame(Cantons, VAR_1, VAR_2, VAR_3, stringsAsFactors = FALSE)
    > 
    > dotDensity(Corsica,
    +                 DF,
    +                 adm.join="Cantons",
    +                 values = c("VAR_1", "VAR_2", "VAR_3"),
    +                 labels = c("H1N1", "H1N2", "H2N2"),
    +                 palette = c("#ffff00", "#ffaa00", "#FF3200"))
    Joining, by = "NAME_4"
    Error: `nm` must be `NULL` or a character vector the same length as `x`
    Backtrace:
        █
     1. └─GADMTools::dotDensity(...)
     2.   ├─dplyr::left_join(.sf_data, .map) %>% tidyr::drop_na()
     3.   │ └─base::eval(lhs, parent, parent)
     4.   │   └─base::eval(lhs, parent, parent)
     5.   ├─dplyr::left_join(.sf_data, .map)
     6.   └─dplyr:::left_join.data.frame(.sf_data, .map)
     7.     └─dplyr:::join_mutate(...)
     8.       └─rlang::set_names(y[vars$y$key], names(vars$y$key))
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 19 marked UTF-8 strings
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
    Use .name_repair to specify repair.
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
     14.                     └─dplyr:::compute_groups(data
    Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘genderdata’
    ```

# geomnet

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/geomnet
* URL: http://github.com/sctyner/geomnet
* BugReports: https://github.com/sctyner/geomnet/issues
* Date/Publication: 2016-12-08 20:38:18
* Number of recursive dependencies: 93

Run `revdep_details(,"geomnet")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    ℹ The error occured in group 1: from_id = "A-", to_id = "A+".
    ✖ could not find function "n"
    Backtrace:
         █
      1. ├─(function (x, ...) ...
      2. └─ggplot2:::print.ggplot(x)
      3.   ├─ggplot2::ggplot_build(x)
      4.   └─ggplot2:::ggplot_build.ggplot(x)
      5.     └─ggplot2:::by_layer(function(l, d) l$compute_statistic(d, layout))
      6.       └─ggplot2:::f(l = layers[[i]], d = data[[i]])
      7.         └─l$compute_statistic(d, layout)
      8.           └─ggplot2:::f(..., self = self)
      9.             └─self$stat$compute_layer(data, params, layout)
     10.               └─geomnet:::f(..., self = self)
     11.                 └─self$compute_panel(...)
     12.                   └─geomnet:::f(..., self = self)
     13.                     └─self$compute_network(...)
     14.                       └─geomnet:::f(...)
     15.                         ├─dplyr::summarise(edges, wt = n())
     16.        
    Execution halted
    ```

# GerminaR

<details>

* Version: 1.4
* Source code: https://github.com/cran/GerminaR
* URL: https://flavjack.github.io/germinaquant/
* BugReports: https://github.com/flavjack/germinar/issues
* Date/Publication: 2020-03-01 15:30:06 UTC
* Number of recursive dependencies: 105

Run `revdep_details(,"GerminaR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    ℹ `r` is `n()`.
    ℹ The error occured in group 1: nacl = 0, evaluation = "D0".
    ✖ could not find function "n"
    Backtrace:
         █
      1. └─GerminaR::ger_intime(...)
      2.   └─`%>%`(...)
      3.     ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      4.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5.       └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      6.         └─GerminaR:::`_fseq`(`_lhs`)
      7.           └─magrittr::freduce(value, `_function_list`)
      8.             └─function_list[[i]](value)
      9.               ├─dplyr::summarise(., mean = mean(germination), r = n(), std = sd(germination))
     10.               ├─dplyr:::summarise.grouped_df(...)
     11.               ├─base::NextMethod()
     12.               └─dplyr:::summarise.data.frame(...)
     13.                 └─dplyr:::summarise_cols(.data, ...)
     14.                   └─base::tryCatch(...)
     15.                     └─b
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘assertthat’ ‘shinydashboard’
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

Run `revdep_details(,"getTBinR")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
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

# ggasym

<details>

* Version: 0.1.2
* Source code: https://github.com/cran/ggasym
* URL: https://github.com/jhrcook/ggasym https://jhrcook.github.io/ggasym/
* BugReports: https://github.com/jhrcook/ggasym/issues
* Date/Publication: 2019-11-21 17:00:02 UTC
* Number of recursive dependencies: 101

Run `revdep_details(,"ggasym")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
      g1    g2    val_1 val_2
      <chr> <chr> <dbl> <dbl>
    1 A     B         1    -1
    2 A     C         2     0
    3 B     C         3     1
    > 
    > tib <- asymmetrise(tib, g1, g2)
    Warning: `...` must not be empty for ungrouped data frames.
    Did you want `data = everything()`?
    > ggplot(tib) +
    + geom_asymmat(aes(x = g1, y = g2, fill_tl = val_1, fill_br = val_2)) +
    +     scale_fill_tl_gradient(low = "lightpink", high = "tomato") +
    +     scale_fill_br_gradient(low = "lightblue1", high = "dodgerblue") +
    +     labs(fill_tl =  "top-left fill", fill_br = "bottom-right fill")
    Warning: `...` must not be empty for ungrouped data frames.
    Did you want `data = everything()`?
    Error in check_all_combinations(data) : 
      All combinations not present in data.
     Use "asymmetrize(data, x, y)" to fix.
    Calls: <Anonymous> ... <Anonymous> -> f -> <Anonymous> -> f -> check_all_combinations
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 99 | SKIPPED: 0 | WARNINGS: 20 | FAILED: 16 ]
      1. Failure: properly determine if a df is grouped (@test-asymmetrise.R#63) 
      2. Failure: properly determine if a df is grouped (@test-asymmetrise.R#67) 
      3. Failure: adding all combinations (@test-asymmetrise.R#81) 
      4. Failure: adding all combinations (@test-asymmetrise.R#82) 
      5. Failure: columns are swapped (@test-asymmetrise.R#122) 
      6. Failure: data frame is asymmeterized (@test-asymmetrise.R#139) 
      7. Failure: data frame is asymmeterized (@test-asymmetrise.R#140) 
      8. Failure: data frame is asymmeterized (@test-asymmetrise.R#142) 
      9. Failure: data frame is asymmeterized (@test-asymmetrise.R#143) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘corrr’
      All declared Imports should be used.
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
    Loading required package: mosaicData
    Error: Corrupt grouped_df data using the old format
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
     16.                       └─dplyr:::group_data.grouped_df(.
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

# gghighlight

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/gghighlight
* URL: https://github.com/yutannihilation/gghighlight/
* BugReports: https://github.com/yutannihilation/gghighlight/issues
* Date/Publication: 2020-01-25 12:20:02 UTC
* Number of recursive dependencies: 117

Run `revdep_details(,"gghighlight")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 225 | SKIPPED: 4 | WARNINGS: 1 | FAILED: 12 ]
      1. Failure: calculate_group_info() works (@test-calculate-group.R#35) 
      2. Failure: gghighlight() works with two layers, grouped (@test-gghighlight.R#139) 
      3. Failure: gghighlight() works with two layers, grouped (@test-gghighlight.R#143) 
      4. Failure: gghighlight() works with two layers, grouped (@test-gghighlight.R#150) 
      5. Error: gghighligt_line() without colour mapping works (@test-gghighligt_old_line.R#55) 
      6. Failure: gghighligt_line() raises error if use_group_by = TRUE but predicate returns multiple values per group (@test-gghighligt_old_line.R#102) 
      7. Failure: gghighligt_line() raises error if use_group_by = TRUE but predicate returns multiple values per group (@test-gghighligt_old_line.R#103) 
      8. Failure: generate_labelled_layer() geenrates a layer for label. (@test-label.R#77) 
      9. Failure: sieve_layer() works with intentionally ungrouped cases (@test-sieve.R#124) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘scales’
      All declared Imports should be used.
    ```

# ggmcmc

<details>

* Version: 1.3
* Source code: https://github.com/cran/ggmcmc
* URL: http://xavier-fim.net/packages/ggmcmc, https://github.com/xfim/ggmcmc
* BugReports: https://github.com/xfim/ggmcmc/issues
* Date/Publication: 2019-07-03 09:30:03 UTC
* Number of recursive dependencies: 84

Run `revdep_details(,"ggmcmc")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > data(binary)
    > ggs_rocplot(ggs(s.binary, family="mu"), outcome=y.binary)
    Error: `filter()` argument `..1` is incorrect.
    ✖ `..1` must be a logical vector, not a double.
    Backtrace:
         █
      1. ├─ggmcmc::ggs_rocplot(ggs(s.binary, family = "mu"), outcome = y.binary)
      2. │ └─dplyr::tbl_df(roc.df) %>% dplyr::filter(Sensitivity, Specificity)
      3. │   ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      4. │   └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5. │     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      6. │       └─ggmcmc:::`_fseq`(`_lhs`)
      7. │         └─magrittr::freduce(value, `_function_list`)
      8. │           ├─base::withVisible(function_list[[k]](value))
      9. │           └─function_list[[k]](value)
     10. │             ├─dplyr::filter(., Sensitivity, Specificity)
     11. │             └─dplyr:::filter.data.frame(., Sensitivity, Specificity)
     12. │               └─dplyr:::filter_rows(.data, ...)
     13. │                 ├─base::tryCatch(...)
     14. │              
    Execution halted
    ```

<<<<<<< Updated upstream
# ggperiodic

<details>

* Version: 1.0.0
* Source code: https://github.com/cran/ggperiodic
* URL: https://github.com/eliocamp/ggperiodic
* BugReports: https://github.com/eliocamp/ggperiodic/issues
* Date/Publication: 2019-03-12 20:02:50 UTC
* Number of recursive dependencies: 112

Run `revdep_details(,"ggperiodic")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        4. dplyr:::mutate.data.frame(df, z1 = z * x)
        5. dplyr:::mutate_cols(.data, ...)
        6. base::tryCatch(...)
        7. base:::tryCatchList(expr, classes, parentenv, handlers)
        8. base:::tryCatchOne(...)
        9. value[[3L]](cond)
       10. dplyr:::stop_eval_tidy(e, index = i, dots = dots, fn = "mutate")
       11. dplyr:::stop_dplyr(index, dots, fn, "errored", x = conditionMessage(e))
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 48 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Error: (unknown) (@test-dplyr.R#15) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

=======
>>>>>>> Stashed changes
# ggplot2

<details>

* Version: 3.3.0
* Source code: https://github.com/cran/ggplot2
* URL: http://ggplot2.tidyverse.org, https://github.com/tidyverse/ggplot2
* BugReports: https://github.com/tidyverse/ggplot2/issues
* Date/Publication: 2020-03-05 16:00:02 UTC
* Number of recursive dependencies: 149

Run `revdep_details(,"ggplot2")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... WARNING
    ```
    '::' or ':::' import not declared from: ‘lifecycle’
    Namespace in Imports field not imported from: ‘mgcv’
      All declared Imports should be used.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘mgcv’
      All declared Imports should be used.
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.8Mb
      sub-directories of 1Mb or more:
        R     2.1Mb
        doc   1.9Mb
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
    Error: `x` must be a vector, not a `gg_rfsrc/data.frame/class` object.
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
     16. │         ├─dplyr::setdiff(names(group_data(x)), ".rows")
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
* Number of recursive dependencies: 73

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
    Warning in min(x[["w.length"]], na.rm = TRUE) :
      no non-missing arguments to min; returning Inf
    Warning in check_spct.generic_spct(x, multiple.wl = multiple.wl) :
      No valid 'w.length' values found
    Error in check_spct.generic_spct(x, multiple.wl = multiple.wl) : 
      ASSERTION FAILED: invalid 'multiple.wl' value: 0
    Calls: autoplot ... check_spct -> check_spct.object_spct -> check_spct.generic_spct
    Execution halted
    ```

# gratia

<details>

* Version: 0.3.0
* Source code: https://github.com/cran/gratia
* URL: https://gavinsimpson.github.io/gratia
* BugReports: https://github.com/gavinsimpson/gratia/issues
* Date/Publication: 2020-01-19 20:20:03 UTC
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
      [ OK: 436 | SKIPPED: 73 | WARNINGS: 54 | FAILED: 4 ]
      1. Error: derivatives() returns derivatives for all smooths in a factor by GAM (@test-derivatives.R#183) 
      2. Error: derivatives() returns derivatives for all smooths in a factor by GAM (@test-derivatives.R#225) 
      3. Error: derivatives() works for factor by smooths issue 47 (@test-derivatives.R#309) 
      4. Error: derivatives() works for fs smooths issue 57 (@test-derivatives.R#359) 
      
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
* Number of recursive dependencies: 140

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
      [ OK: 335 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 2 ]
      1. Failure: combine_terms works without error (@test-combine_terms.R#55) 
      2. Failure: combine_terms works without error (@test-combine_terms.R#97) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘gt’
    ```

# hablar

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/hablar
* URL: https://davidsjoberg.github.io/
* BugReports: https://github.com/davidsjoberg/hablar/issues
* Date/Publication: 2019-06-09 17:20:03 UTC
* Number of recursive dependencies: 77

Run `revdep_details(,"hablar")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       20. dplyr:::stop_dplyr(index, dots, fn, "errored", x = conditionMessage(e))
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 389 | SKIPPED: 0 | WARNINGS: 1 | FAILED: 8 ]
      1. Failure: find_duplicates (@test.find_in_df.R#8) 
      2. Failure: find_duplicates (@test.find_in_df.R#15) 
      3. Failure: find_duplicates (@test.find_in_df.R#29) 
      4. Failure: find_duplicates (@test.find_in_df.R#36) 
      5. Error: row_sum (@test.fun_by_row.R#8) 
      6. Error: row_sum_ (@test.fun_by_row.R#28) 
      7. Error: row_mean (@test.fun_by_row.R#48) 
      8. Error: row_mean_ (@test.fun_by_row.R#68) 
      
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
    +                             out_state_second = "CD160_HVEM_1440") 
    Error: `mutate()` argument `err_abs_diff_theo_frac_exch` errored.
    ℹ `err_abs_diff_theo_frac_exch` is `sqrt(err_abs_avg_theo_in_time_1^2 + err_abs_avg_theo_in_time_2^2)`.
    ℹ The error occured in group 1: Sequence = "ARSQKSGIRLQGHF", Start = 88, End = 101, MaxUptake = 13.
    ✖ object 'err_abs_avg_theo_in_time_1' not found
    Backtrace:
         █
      1. └─HaDeX::prepare_dataset(...)
      2.   └─`%>%`(...)
      3.     ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      4.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5.       └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      6.         └─HaDeX:::`_fseq`(`_lhs`)
      7.           └─magrittr::freduce(value, `_function_list`)
      8.             └─function_list[[i]](value)
      9.               ├─dplyr::mutate(...)
     10.               └─dplyr:::mutate.data.frame(...)
     11.                 └─dplyr:::mutate_cols(.data, ...)
     12.                   └─base::tryCatch(...)
     13.                     └─ba
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       15. value[[3L]](cond)
       16. dplyr:::stop_eval_tidy(e, index = i, dots = dots, fn = "mutate")
       17. dplyr:::stop_dplyr(index, dots, fn, "errored", x = conditionMessage(e))
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 9 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 6 ]
      1. Error: (unknown) (@test_add_stat_dependency.R#7) 
      2. Error: (unknown) (@test_calculate_confidence_limit_values.R#7) 
      3. Error: (unknown) (@test_comparison_plot.R#9) 
      4. Error: class is right 
      5. Error: size is right 
      6. Error: (unknown) (@test_woods_plot.R#9) 
      
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
    > ### Aliases: define_dsa define_dsa_
    > 
    > ### ** Examples
    > 
    > 
    > define_dsa(
    +   a, 10, 45,
    +   b, .5, 1.5
    + )
    Error: Input must be a vector, not a `lazy_dots` object.
    Backtrace:
        █
     1. ├─heemod::define_dsa(a, 10, 45, b, 0.5, 1.5)
     2. │ └─heemod:::define_dsa_(...)
     3. │   ├─base::suppressWarnings(...)
     4. │   │ └─base::withCallingHandlers(expr, warning = function(w) invokeRestart("muffleWarning"))
     5. │   └─dplyr::bind_rows(...)
     6. │     └─vctrs::vec_rbind(!!!dots, .names_to = .id)
     7. └─vctrs:::stop_scalar_type(...)
     8.   └─vctrs:::stop_vctrs(msg, "vctrs_error_scalar_type", actual = x)
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 475 | SKIPPED: 0 | WARNINGS: 2 | FAILED: 13 ]
      1. Error: exactly match HIV model (@test_dmhee.R#102) 
      2. Failure: Exactly match THR model (@test_dmhee.R#291) 
      3. Failure: Exactly match THR model (@test_dmhee.R#307) 
      4. Error: Same results using 1 core or 2. (@test_parallel.R#7) 
      5. Error: define sensitivity (@test_sensitivity.R#5) 
      6. Error: run sensitivity (@test_sensitivity.R#101) 
      7. Error: discount rate as a parameter works (@test_sensitivity.R#173) 
      8. Error: sensitivity expression inputs (@test_sensitivity.R#236) 
      9. Error: can read multinomial parameters from file (@test_tabular_input.R#110) 
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

# hpiR

<details>

* Version: 0.3.1
* Source code: https://github.com/cran/hpiR
* URL: https://www.github.com/andykrause/hpiR
* Date/Publication: 2020-02-12 16:00:07 UTC
* Number of recursive dependencies: 92

Run `revdep_details(,"hpiR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    +                      periodicity = 'monthly',
    +                      min_date = '2010-06-01',
    +                      max_date = '2015-11-30',
    +                      adj_type = 'clip',
    +                      date = 'sale_date',
    +                      price = 'sale_price',
    +                      trans_id = 'sale_id',
    +                      prop_id = 'pinx',
    +                      estimator = 'robust',
    +                      log_dep = TRUE,
    +                      trim_model = TRUE,
    +                      max_period = 48,
    +                      smooth = FALSE)
    Supplied "min_date" date is greater than minimum of transactions. Clipping transactions.
    
    Supplied "max_date" is less than maximum of transactions. Clipping transactions.
    
    Error in if (nrow(rt_df) < nrow(attr(rt_df, "period_table"))) { : 
      argument is of length zero
    Calls: rtIndex -> hpiModel -> hpiModel.rt -> rtModel
    Execution halted
    ```

# iadf

<details>

* Version: 0.1.0
* Source code: https://github.com/cran/iadf
* Date/Publication: 2017-06-06 10:17:00 UTC
* Number of recursive dependencies: 64

Run `revdep_details(,"iadf")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Please use `tidyselect::vars_select()` instead.
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_warnings()` to see where this warning was generated.
    Warning: `filter_()` is deprecated as of dplyr 0.7.0.
    Please use `filter()` instead.
    See vignette('programming') for more help
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_warnings()` to see where this warning was generated.
    Error: Join columns must be present in data
    ✖ Problem with `year`
    Backtrace:
        █
     1. ├─iadf::campelo_chapman(campelo_freq(example_iadf, example_rwl))
     2. └─iadf::campelo_freq(example_iadf, example_rwl)
     3.   ├─dplyr::left_join(iadf_tidy, rwl_tidy, by = c("year", "series"))
     4.   └─dplyr:::left_join.data.frame(...)
     5.     └─dplyr:::join_mutate(...)
     6.       └─dplyr:::join_cols(...)
     7.         └─dplyr:::standardise_join_by(by, x_names = x_names, y_names = y_names)
     8.           └─dplyr:::check_join_vars(by$y, y_names)
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        1. testthat::expect_that(...)
        7. iadf::campelo_freq(example_iadf, example_rwl)
        9. dplyr:::left_join.data.frame(...)
       10. dplyr:::join_mutate(...)
       11. dplyr:::join_cols(...)
       12. dplyr:::standardise_join_by(by, x_names = x_names, y_names = y_names)
       13. dplyr:::check_join_vars(by$y, y_names)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 34 | SKIPPED: 0 | WARNINGS: 3 | FAILED: 2 ]
      1. Error: campelo_freq behaves as expected (@test-campelo.R#12) 
      2. Error: campelo_chapman behaves as expected (@test-campelo.R#32) 
      
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

# implicitMeasures

<details>

* Version: 0.1.1
* Source code: https://github.com/cran/implicitMeasures
* Date/Publication: 2020-02-28 19:10:02 UTC
* Number of recursive dependencies: 77

Run `revdep_details(,"implicitMeasures")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    >  sciat1 <- sciat_data[[1]] # compute D for the first SC-IAT
    >  d_sciat1 <- Dsciat(sciat1,
    +                     mappingA = "test.sc_dark.Darkbad",
    +                     mappingB = "test.sc_dark.Darkgood",
    +                     non_response = "alert")
    Error: `x` must be a vector, not a `data.frame/sciat_clean` object.
    Backtrace:
         █
      1. ├─implicitMeasures::Dsciat(...)
      2. │ ├─dplyr::mutate(...)
      3. │ └─dplyr:::mutate.data.frame(...)
      4. │   └─dplyr:::mutate_cols(.data, ...)
      5. │     └─DataMask$new(.data, caller_env())
      6. │       └─.subset2(public_bind_env, "initialize")(...)
      7. │         └─dplyr::group_rows(data)
      8. │           ├─dplyr::group_data(.data)
      9. │           └─dplyr:::group_data.data.frame(.data)
     10. │             └─vctrs::vec_init(.data[0], 1)
     11. └─vctrs:::stop_scalar_type(...)
     12.   └─vctrs:::stop_vctrs(msg, "vctrs_error_scalar_type", actual = x)
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 45 | SKIPPED: 2 | WARNINGS: 0 | FAILED: 12 ]
      1. Error: Dsciat returns the right object (@test-Dsciat.R#69) 
      2. Error: IATrel results in a list of 2 elements with class IATrel (@test-IATrel_descript_d.R#36) 
      3. Error: descript_d recognizies the class of the object for the SC-IAT (@test-IATrel_descript_d.R#61) 
      4. Error: d_plot produces a ggplot fot the IAT (@test-d_distr_d_plot.R#23) 
      5. Error: d_plot produces a ggplot fot the SC-IAT (@test-d_distr_d_plot.R#43) 
      6. Error: d_distr produces a ggplot fot the IAT (@test-d_distr_d_plot.R#75) 
      7. Error: d_distr produces a ggplot fot the SC-IAT (@test-d_distr_d_plot.R#95) 
      8. Error: multi_dscore recognizes the correct class of the object (@test-multi_dscore_multi_dsciat.R#16) 
      9. Error: multi_dscore results in a List of 2 (data.frame and list) (@test-multi_dscore_multi_dsciat.R#34) 
      1. ...
      
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
* Number of recursive dependencies: 64

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

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      C9_1
      are not unique, i.e. conditions are met multiple times!
      Please correct your crit_score table before you continue.  variable required_data_type
      1      edf            numeric
        variable required_data_type
      1     r_sq            numeric
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 557 | SKIPPED: 0 | WARNINGS: 10 | FAILED: 4 ]
      1. Error: (unknown) (@test_calc_deriv.R#6) 
      2. Error: (unknown) (@test_cond_boot.R#112) 
      3. Failure: structure of returned object (@test_plot_diagnostics.R#19) 
      4. Failure: structure of returned object (@test_plot_diagnostics.R#20) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.0Mb
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

*   checking examples ... ERROR
    ```
    Running examples in ‘ipumsr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ipums_shape_left_join
    > ### Title: Join data to geographic boundaries
    > ### Aliases: ipums_shape_left_join ipums_shape_right_join
    > ###   ipums_shape_inner_join ipums_shape_full_join
    > 
    > ### ** Examples
    > 
    > # Note that these examples use NHGIS data so that they use the example data provided,
    > # but the functions read_nhgis_sf/read_nhgis_sp perform this merge for you.
    > 
    > data <- read_nhgis(ipums_example("nhgis0008_csv.zip"))
    New names:
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        9. dplyr:::inner_join.data.frame(...)
       10. dplyr:::join_mutate(...)
       11. rlang::set_names(x[vars$x$key], names(vars$x$key))
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 195 | SKIPPED: 11 | WARNINGS: 0 | FAILED: 6 ]
      1. Error: mismatched attributes in bind rows (@test_ipums_bind_rows.r#39) 
      2. Error: Basic join works (sf) (@test_shape_join.r#8) 
      3. Error: suffix argument works (sf) (@test_shape_join.r#35) 
      4. Error: complicated by works (sf) (@test_shape_join.r#46) 
      5. Error: Join failures are mentioned (@test_shape_join.r#77) 
      6. Error: Character -> Integer conversion works (#16) (@test_shape_join.r#107) 
      
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
    Error: `df` must be a data frame without row names in `column_to_rownames()`.
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

# ISRaD

<details>

* Version: 1.2.3
* Source code: https://github.com/cran/ISRaD
* Date/Publication: 2020-02-09 16:10:05 UTC
* Number of recursive dependencies: 142

Run `revdep_details(,"ISRaD")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > database <- ISRaD::Gaudinski_2001
    > ISRaD.rep.count.all(database)
    Error: `summarise()` argument `entries` errored.
    ℹ `entries` is `n_distinct(.data$entry_name)`.
    ✖ Column `entry_name` not found in `.data`
    Backtrace:
         █
      1. └─ISRaD::ISRaD.rep.count.all(database)
      2.   └─mutate_all(database$metadata, as.character) %>% summarise(entries = n_distinct(.data$entry_name))
      3.     ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      4.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5.       └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      6.         └─ISRaD:::`_fseq`(`_lhs`)
      7.           └─magrittr::freduce(value, `_function_list`)
      8.             ├─base::withVisible(function_list[[k]](value))
      9.             └─function_list[[k]](value)
     10.               ├─dplyr::summarise(., entries = n_distinct(.data$entry_name))
     11.               └─dplyr:::summarise.data.frame(., entries = n_distinct(.data$entry_name))
     12.                 └─dplyr:::summarise_cols(.data, ...)
     13.          
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘devtools’ ‘pangaear’ ‘rcrossref’ ‘rgdal’ ‘stringr’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked UTF-8 string
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

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 548 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 9 ]
      1. Failure: show_n can suppress Ns, digits parameter is correct (@test-adorn-crosstab.R#78) 
      2. Failure: for printing purposes: tabyl class stays tabyl, data.frame stays data.frame, tibble is downgraded to data.frame (@test-adorn-title.R#111) 
      3. Failure: Correct combinations of duplicates are found (@test-get-dupes.R#10) 
      4. Failure: Correct combinations of duplicates are found (@test-get-dupes.R#11) 
      5. Failure: instances of no dupes throw correct messages, return empty df (@test-get-dupes.R#23) 
      6. Failure: NA levels get moved to the last column in the data.frame, are suppressed properly (@test-tabyl.R#309) 
      7. Failure: NA levels get moved to the last column in the data.frame, are suppressed properly (@test-tabyl.R#311) 
      8. Failure: NA levels get moved to the last column in the data.frame, are suppressed properly (@test-tabyl.R#315) 
      9. Failure: NA levels get moved to the last column in the data.frame, are suppressed properly (@test-tabyl.R#317) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# jstor

<details>

* Version: 0.3.7
* Source code: https://github.com/cran/jstor
* URL: https://github.com/ropensci/jstor, https://ropensci.github.io/jstor/
* BugReports: https://github.com/ropensci/jstor/issues
* Date/Publication: 2019-09-05 02:10:11 UTC
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
      [ OK: 235 | SKIPPED: 4 | WARNINGS: 1 | FAILED: 17 ]
      1. Failure: journal id is unified (@test-augment.R#75) 
      2. Failure: reading ngrams works (@test-ngram.R#12) 
      3. Failure: reading ngrams works (@test-ngram.R#13) 
      4. Failure: subsetting ngrams works (@test-ngram.R#44) 
      5. Failure: files with column names can be re-read (@test-re-import.R#212) 
      6. Failure: files with column names can be re-read (@test-re-import.R#216) 
      7. Failure: files with column names can be re-read (@test-re-import.R#220) 
      8. Failure: files with column names can be re-read (@test-re-import.R#224) 
      9. Failure: files with column names can be re-read (@test-re-import.R#228) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

# keyholder

<details>

* Version: 0.1.4
* Source code: https://github.com/cran/keyholder
* URL: https://echasnovski.github.io/keyholder/, https://github.com/echasnovski/keyholder/
* BugReports: https://github.com/echasnovski/keyholder/issues/
* Date/Publication: 2020-03-01 20:00:02 UTC
* Number of recursive dependencies: 56

Run `revdep_details(,"keyholder")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 176 | SKIPPED: 0 | WARNINGS: 1 | FAILED: 9 ]
      1. Error: select_all works (@test-keyed-df-one-tbl.R#58) 
      2. Error: select_all works (@test-keyed-df-one-tbl.R#55) 
      3. Error: (unknown) (@test-keyed-df-one-tbl.R#55) 
      4. Failure: inner_join.keyed_df works (@test-keyed-df-two-tbl.R#55) 
      5. Failure: left_join.keyed_df works (@test-keyed-df-two-tbl.R#72) 
      6. Failure: right_join.keyed_df works (@test-keyed-df-two-tbl.R#89) 
      7. Failure: full_join.keyed_df works (@test-keyed-df-two-tbl.R#106) 
      8. Failure: semi_join.keyed_df works (@test-keyed-df-two-tbl.R#123) 
      9. Failure: anti_join.keyed_df works (@test-keyed-df-two-tbl.R#140) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# kntnr

<details>

* Version: 0.4.2
* Source code: https://github.com/cran/kntnr
* URL: https://yutannihilation.github.io/kntnr/
* BugReports: https://github.com/yutannihilation/kntnr/issues
* Date/Publication: 2019-08-25 09:10:02 UTC
* Number of recursive dependencies: 48

Run `revdep_details(,"kntnr")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(testthat)
      > library(kntnr)
      > 
      > test_check("kntnr")
      ── 1. Failure: parsing FILE works (@test-parse-field.R#10)  ────────────────────
      x$test not identical to `expect`.
      Component 1: Attributes: < Component "class": Lengths (1, 3) differ (string compare on first 1) >
      Component 1: Attributes: < Component "class": 1 string mismatch >
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 74 | SKIPPED: 0 | WARNINGS: 1 | FAILED: 1 ]
      1. Failure: parsing FILE works (@test-parse-field.R#10) 
      
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
      spread_data(bind_rows(a, b)) not equal to full_join(...).
      Names: 2 string mismatches
<<<<<<< Updated upstream
      Component 3: Mean relative difference: 1.154801
      Component 4: Mean relative difference: 4.475955
=======
      Component 3: Mean relative difference: 1.024952
      Component 4: Mean relative difference: 8.215419
>>>>>>> Stashed changes
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 114 | SKIPPED: 0 | WARNINGS: 4 | FAILED: 5 ]
      1. Error: test that it is possible to load LANS maps (@test-load-data.R#81) 
      2. Error: test that plotting is working correctly (@test-plotting.R#8) 
      3. Error: test that it is possible to read full ion data file (@test-read-files.R#57) 
      4. Error: test that it is possible to read all map data in a folder (@test-read-files.R#79) 
      5. Failure: test that transformation safety checks are in place (@test-transformations.R#13) 
      
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

# matsbyname

<details>

* Version: 0.4.11
* Source code: https://github.com/cran/matsbyname
* URL: https://github.com/MatthewHeun/matsbyname
* BugReports: https://github.com/MatthewHeun/matsbyname/issues
* Date/Publication: 2019-12-05 08:00:13 UTC
* Number of recursive dependencies: 90

Run `revdep_details(,"matsbyname")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Error: `mutate()` argument `m2` must return compatible vectors across groups.
    ℹ `m2` is `cumsum_byname(m)`.
    ℹ Result type for group 1 (grp = "A") : <list>.
    ℹ Result type for group 2 (grp = "B") : <I<list>>.
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
      9.             ├─dplyr::mutate(., m2 = cumsum_byname(m))
     10.             └─dplyr:::mutate.data.frame(., m2 = cumsum_byname(m))
     11.               └─dplyr:::mutate_cols(.data, ...)
     12.                 └─base::tryCatch(...)
     13.                   └─base:::tryCatchList(expr, classes, parentenv, handlers)
     14.                
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       12. base::tryCatch(...)
       13. base:::tryCatchList(expr, classes, parentenv, handlers)
       16. base:::tryCatchList(expr, names[-nh], parentenv, handlers[-nh])
       17. base:::tryCatchOne(...)
       18. value[[3L]](cond)
       19. dplyr:::stop_combine(e, index = i, dots = dots, fn = "mutate")
       20. dplyr:::stop_dplyr(...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 732 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 2 ]
      1. Error: cumsum_byname works as expected (@test_Unary.R#1098) 
      2. Error: cumprod_byname works as expected (@test_Unary.R#1153) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# matsindf

<details>

* Version: 0.3.3
* Source code: https://github.com/cran/matsindf
* URL: https://github.com/MatthewHeun/matsindf
* BugReports: https://github.com/MatthewHeun/matsindf/issues
* Date/Publication: 2020-03-04 05:40:09 UTC
* Number of recursive dependencies: 93

Run `revdep_details(,"matsindf")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ...
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 155 | SKIPPED: 0 | WARNINGS: 1 | FAILED: 8 ]
      1. Failure: small example works as expected (@test_collapse.R#64) 
      2. Failure: expand_to_tidy works with a list of matrices (@test_expand.R#134) 
      3. Failure: matsindf_apply works as expected using .DF with single numbers (@test_matsindf_apply.R#58) 
      4. Failure: matsindf_apply works for single numbers in data frame columns (@test_matsindf_apply.R#149) 
      5. Failure: matsindf_apply works for single numbers in data frame columns (@test_matsindf_apply.R#150) 
      6. Failure: index_column works as expected (@test_utilities.R#31) 
      7. Failure: index_column works as expected (@test_utilities.R#45) 
      8. Failure: index_column works as expected (@test_utilities.R#55) 
      
      Error: testthat unit tests failed
      Execution halted
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
    > taxacom.ex<-taxa.compare(taxtab=taxtab6[,c("personid","x.sampleid","bf","age.sample",tl)],
    + propmed.rel="gamlss",comvar="bf",adjustvar="age.sample",
    + longitudinal="yes",p.adjust.method="fdr")
    Error: Corrupt grouped_df data using the old format
    Backtrace:
         █
      1. ├─metamicrobiomeR::taxa.compare(...)
      2. │ └─base::as.data.frame(taxtab)
      3. ├─taxtab6[, c("personid", "x.sampleid", "bf", "age.sample", tl)]
      4. └─dplyr:::`[.grouped_df`(...)
      5.   ├─dplyr::intersect(names(out), group_vars(x))
      6.   ├─dplyr:::intersect.default(names(out), group_vars(x))
      7.   │ └─base::intersect(x, y, ...)
      8.   │   └─base::as.vector(y)
      9.   ├─dplyr::group_vars(x)
     10.   └─dplyr:::group_vars.data.frame(x)
     11.     ├─dplyr::setdiff(names(group_data(x)), ".rows")
     12.     ├─dplyr::group_data(x)
     13.     └─dplyr:::group_data.grouped_df(x)
     14.       └─dplyr::validate_grouped_df(.data)
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
* Number of recursive dependencies: 46

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
* Number of recursive dependencies: 107

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
      [ OK: 173 | SKIPPED: 0 | WARNINGS: 1 | FAILED: 1 ]
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
    
    Error: Corrupt grouped_df data using the old format
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
     11.                 ├─dplyr::setdiff(group_names, tbl_vars(out))
     12.                 ├─dplyr:::setdiff.default(group_names, tbl_vars(out))
     13.                 │ └─base::setdiff(x, y, ...)
     14.                 │   └─base::as.vector(y)
     15.                 └─dplyr::tbl_vars(out)
     16.                   ├─dplyr:::new_sel_vars(tbl_vars_dispatch(x), gro
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

# mudata2

<details>

* Version: 1.1.1
* Source code: https://github.com/cran/mudata2
* URL: https://paleolimbot.github.io/mudata2, https://github.com/paleolimbot/mudata2
* BugReports: https://github.com/paleolimbot/mudata2/issues
* Date/Publication: 2020-02-02 15:40:02 UTC
* Number of recursive dependencies: 85

Run `revdep_details(,"mudata2")` for more info

</details>

## Newly broken

*   checking S3 generic/method consistency ... WARNING
    ```
    src_tbls:
      function(x, ...)
    src_tbls.mudata:
      function(x)
    
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘ggplot2’
      All declared Imports should be used.
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

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Component 3: Mean relative difference: 1
      Component 4: Mean absolute difference: 1
      
      ── 2. Failure: special missings are the same for grouped and ungrouped data (@te
      aq_grouped_recoded$Ozone_NA not equal to aq_recoded$Ozone_NA.
      Attributes: < Component "class": Lengths (1, 2) differ (string compare on first 1) >
      Attributes: < Component "class": 1 string mismatch >
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 567 | SKIPPED: 22 | WARNINGS: 0 | FAILED: 2 ]
      1. Failure: prop_miss_case returns same as mean_ (@test-prop-cases-not-zero.R#53) 
      2. Failure: special missings are the same for grouped and ungrouped data (@test-special-missing-values.R#137) 
      
      Error: testthat unit tests failed
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

# ngsReports

<details>

* Version: 1.2.0
* Source code: https://github.com/cran/ngsReports
* URL: https://github.com/UofABioinformaticsHub/ngsReports
* BugReports: https://github.com/UofABioinformaticsHub/ngsReports/issues
* Date/Publication: 2019-10-29
* Number of recursive dependencies: 159

Run `revdep_details(,"ngsReports")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > overRep2Fasta(fdl, path = faOut)
    Error: `summarise()` argument `nFiles` errored.
    ℹ `nFiles` is `n()`.
    ℹ The error occured in group 1: Sequence = "AAAAATATGGAACGCTTCACGAATTTGCGTCATCCTTGCGCAGGGGCCAT".
    ✖ could not find function "n"
    Backtrace:
         █
      1. ├─ngsReports::overRep2Fasta(fdl, path = faOut)
      2. └─ngsReports::overRep2Fasta(fdl, path = faOut)
      3.   ├─dplyr::summarise(...)
      4.   ├─dplyr:::summarise.grouped_df(...)
      5.   ├─base::NextMethod()
      6.   └─dplyr:::summarise.data.frame(...)
      7.     └─dplyr:::summarise_cols(.data, ...)
      8.       └─base::tryCatch(...)
      9.         └─base:::tryCatchList(expr, classes, parentenv, handlers)
     10.           └─base:::tryCatchOne(...)
     11.             └─value[[3L]](cond)
     12.               └─dplyr:::stop_eval_tidy(e, index = i, dots = dots, fn = "summarise")
     13.                 └─dplyr:::stop_dplyr(index, dots, fn, "errored", x = conditionMessage(e))
    Execution halted
    ```

# nhdplusTools

<details>

* Version: 0.3.12
* Source code: https://github.com/cran/nhdplusTools
* URL: https://usgs-r.github.io/nhdplusTools/ https://github.com/usgs-r/nhdplusTools/
* BugReports: https://github.com/usgs-r/nhdplusTools/issues/
* Date/Publication: 2020-01-11 13:00:02 UTC
* Number of recursive dependencies: 123

Run `revdep_details(,"nhdplusTools")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    
    > source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))
    > catchment_length <- prepare_nhdplus(walker_flowline, 0, 0,
    +                              purge_non_dendritic = FALSE, warn = FALSE) %>%
    +   left_join(select(walker_flowline, COMID), by = "COMID") %>%
    +   select(ID = COMID, toID = toCOMID, length = LENGTHKM)
    Error: `nm` must be `NULL` or a character vector the same length as `x`
    Backtrace:
         █
      1. └─`%>%`(...)
      2.   ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      3.   └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      4.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5.       └─`_fseq`(`_lhs`)
      6.         └─magrittr::freduce(value, `_function_list`)
      7.           └─function_list[[i]](value)
      8.             ├─dplyr::left_join(., select(walker_flowline, COMID), by = "COMID")
      9.             └─dplyr:::left_join.data.frame(...)
     10.               └─dplyr:::join_mutate(...)
     11.                 └─rlang::set_names(y[vars$y$key], names(vars$y$key))
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 156 | SKIPPED: 20 | WARNINGS: 0 | FAILED: 9 ]
      1. Error: total drainage area works (@test_calc_network.R#6) 
      2. Error: arbolate sum works (@test_calc_network.R#22) 
      3. Error: calculate level path (@test_calc_network.R#76) 
      4. Error: get_pfaf (@test_calc_network.R#98) 
      5. Error: get_terminal (@test_calc_network.R#154) 
      6. Error: get_terminal (@test_calc_network.R#194) 
      7. Error: get_nhdplushr simp and proj (@test_get_nhdplushr.R#79) 
      8. Error: get_nhdplushr rename and keep_cols (@test_get_nhdplushr.R#99) 
      9. Error: make_standalone (@test_get_nhdplushr.R#111) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.0Mb
      sub-directories of 1Mb or more:
        extdata   5.5Mb
    ```

# nonmemica

<details>

* Version: 0.9.0
* Source code: https://github.com/cran/nonmemica
* Date/Publication: 2019-04-25 12:10:02 UTC
* Number of recursive dependencies: 74

Run `revdep_details(,"nonmemica")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > 1001 %>% parameters
    Error: No common type for `..1$estimate` <double> and `..2$estimate` <halfmatrix>.
    Backtrace:
         █
      1. ├─1001 %>% parameters
      2. │ ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      3. │ └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      4. │   └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5. │     └─`_fseq`(`_lhs`)
      6. │       └─magrittr::freduce(value, `_function_list`)
      7. │         ├─base::withVisible(function_list[[k]](value))
      8. │         └─function_list[[k]](value)
      9. │           ├─nonmemica::parameters(.)
     10. │           └─nonmemica:::parameters.numeric(.)
     11. │             ├─nonmemica::parameters(as.character(x), ...)
     12. │             └─nonmemica:::parameters.character(as.character(x), ...)
     13. │               └─base::lapply(x, run, ...)
     14. │                 └─nonmemica:::FUN(X[[i]], ...)
     15. │                   └─nonmemica:::.parameters(x, ...)
     16. │    
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
     13. │         ├─dplyr::setdiff(names(group_data(x)), ".rows")
     14. │         ├─dplyr::group_data(x)
     15. │         └─dplyr:::group_data.data.frame(x)
     16. │           └─vctrs::vec_init(.data[0], 1)
     17. └─vctrs:::stop_scalar_type(...)
     18.   └─vctrs:::s
    Execution halted
    ```

# opensensmapr

<details>

* Version: 0.5.1
* Source code: https://github.com/cran/opensensmapr
* URL: http://github.com/sensebox/opensensmapR
* BugReports: http://github.com/sensebox/opensensmapR/issues
* Date/Publication: 2019-03-10 20:50:21 UTC
* Number of recursive dependencies: 95

Run `revdep_details(,"opensensmapr")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > test_check('opensensmapr')
      ── 1. Failure: osem_measurements_archive fails for multiple boxes (@test_archive
      `osem_measurements_archive(boxes, as.POSIXlt("2018-08-08"))` threw an error with unexpected message.
      Expected match: "this function only works for exactly one senseBox!"
      Actual message: "not implemented for class data.frame"
      Backtrace:
       1. testthat::expect_error(...)
       7. opensensmapr:::osem_measurements_archive.default(boxes, as.POSIXlt("2018-08-08"))
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 47 | SKIPPED: 35 | WARNINGS: 0 | FAILED: 1 ]
      1. Failure: osem_measurements_archive fails for multiple boxes (@test_archive.R#49) 
      
      Error: testthat unit tests failed
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

* Version: 0.5.0
* Source code: https://github.com/cran/padr
* URL: https://github.com/EdwinTh/padr
* BugReports: https://github.com/EdwinTh/padr/issues
* Date/Publication: 2019-06-11 13:20:03 UTC
* Number of recursive dependencies: 80

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
      [ OK: 573 | SKIPPED: 1 | WARNINGS: 1 | FAILED: 24 ]
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
       11. pammtools:::sample_info.ped(.)
       12. dplyr::group_by(., !!sym(id_var))
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 248 | SKIPPED: 0 | WARNINGS: 2 | FAILED: 7 ]
      1. Error: cumulative hazard functions work for PAM (@test-add-functions.R#101) 
      2. Error: survival probabilities functions work for PAM (@test-add-functions.R#236) 
      3. Error: Cumulative effects are calculated correctly (@test-cumulative-effect.R#102) 
      4. Error: Sample info returned for data frame (@test-interval-functions.R#23) 
      5. Error: Sample info returned for grouped ped objects (@test-interval-functions.R#33) 
      6. Error: ped info returned for (grouped) ped objects (@test-interval-functions.R#42) 
      7. Error: make_newdata works for PED with matrix columns (@test-newdata.R#68) 
      
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
      [ OK: 7 | SKIPPED: 0 | WARNINGS: 1 | FAILED: 17 ]
      1. Error: dplyr functions return panel_data objects (@test-utils.R#15) 
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
<<<<<<< Updated upstream
      6. └─PAST::load_GWAS_data(demo_association_file, demo_effects_file) /var/folders/4b/hn4fq98s6810s4ccv2f9hm2h0000gn/T//RtmpNzneFJ/Rex12dfc4bd2baab:11:0
=======
      6. └─PAST::load_GWAS_data(demo_association_file, demo_effects_file) /var/folders/4b/hn4fq98s6810s4ccv2f9hm2h0000gn/T//RtmpMSrzXw/Rex16fcf3a3eb109:11:0
>>>>>>> Stashed changes
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

# photobiology

<details>

* Version: 0.9.30
* Source code: https://github.com/cran/photobiology
* URL: https://www.r4photobiology.info/, https://bitbucket.org/aphalo/photobiology
* BugReports: https://bitbucket.org/aphalo/photobiology/issues
* Date/Publication: 2020-01-09 14:20:02 UTC
* Number of recursive dependencies: 58

Run `revdep_details(,"photobiology")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘photobiology-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: is_photon_based
    > ### Title: Query if a spectrum contains photon- or energy-based data.
    > ### Aliases: is_photon_based is_energy_based
    > 
    > ### ** Examples
    > 
    > is_photon_based(sun.spct)
    [1] TRUE
    > my.spct <- dplyr::select(sun.spct, w.length, s.e.irrad)
    Warning in min(x[["w.length"]], na.rm = TRUE) :
      no non-missing arguments to min; returning Inf
    Warning in check_spct.generic_spct(x, multiple.wl = multiple.wl) :
      No valid 'w.length' values found
    Error in check_spct.generic_spct(x, multiple.wl = multiple.wl) : 
      ASSERTION FAILED: invalid 'multiple.wl' value: 0
    Calls: <Anonymous> ... check_spct -> check_spct.source_spct -> check_spct.generic_spct
    Execution halted
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘fishmethods’, ‘photobiologyWavebands’
    ```

# photobiologyInOut

<details>

* Version: 0.4.21-1
* Source code: https://github.com/cran/photobiologyInOut
* URL: http://www.r4photobiology.info/
* BugReports: https://bitbucket.org/aphalo/photobiologyinout/issues/
* Date/Publication: 2020-01-11 20:10:02 UTC
* Number of recursive dependencies: 109

Run `revdep_details(,"photobiologyInOut")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 433 | SKIPPED: 4 | WARNINGS: 0 | FAILED: 18 ]
      1. Failure: single spectrum (@test-fred.R#27) 
      2. Failure: single spectrum (quantum) (@test-licor.R#30) 
      3. Failure: single spectrum Tfr (@test-licor.R#77) 
      4. Failure: single spectrum Rfr (@test-licor.R#108) 
      5. Failure: single spectrum (quantum) (@test-macam.R#29) 
      6. Failure: jaz (@test-oo.R#31) 
      7. Failure: jaz (@test-oo.R#57) 
      8. Failure: jaz_Tpc (@test-oo.R#90) 
      9. Failure: jaz_Rpc (@test-oo.R#122) 
      1. ...
      
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
    > my_result_excluded <- exclude(my_result,
    +                               FUN=exclude_nca_max.aucinf.pext())
    Error: Column name `start` must not be duplicated.
    Use .name_repair to specify repair.
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
     14.                     └─tibble::tib
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       13. dplyr:::compute_groups(data, vars, drop = drop)
       14. tibble::tibble(!!!old_keys, `:=`(".rows", old_rows))
       15. tibble:::lst_to_tibble(xlq$output, .rows, .name_repair, lengths = xlq$lengths)
       16. tibble:::set_repaired_names(x, .name_repair)
       18. tibble:::repaired_names(names(x), .name_repair = .name_repair)
       19. tibble:::check_unique(new_name)
      
      Provenance hash a generated on b with c.
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 1295 | SKIPPED: 0 | WARNINGS: 1 | FAILED: 2 ]
      1. Error: exclude.default (@test-exclude.R#198) 
      2. Error: exclude_nca (@test-exclude_nca.R#8) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# plater

<details>

* Version: 1.0.1
* Source code: https://github.com/cran/plater
* URL: https://github.com/ropenscilabs/plater
* BugReports: https://github.com/ropenscilabs/plater/issues
* Date/Publication: 2017-06-26 22:16:11 UTC
* Number of recursive dependencies: 48

Run `revdep_details(,"plater")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > test_check("plater")
      Loading required package: plater
      ── 1. Failure: valid file with read_plates  ────────────────────────────────────
      `read_plates(c("testData/96/allWellIds.csv", "testData/96/allWellIds.csv"))` produced warnings, messages.
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 744 | SKIPPED: 10 | WARNINGS: 95 | FAILED: 1 ]
      1. Failure: valid file with read_plates 
      
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
* Number of recursive dependencies: 103

Run `revdep_details(,"portalr")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      57: withOneRestart(expr, restarts[[1L]])
      58: withRestarts(testthat_abort_reporter = function() NULL, force(code))
      59: with_reporter(reporter = reporter, start_end_reporter = start_end_reporter,     {        reporter$start_file(basename(path))        lister$start_file(basename(path))        source_file(path, new.env(parent = env), chdir = TRUE,             wrap = wrap)        reporter$.end_context()        reporter$end_file()    })
      60: FUN(X[[i]], ...)
      61: lapply(paths, test_file, env = env, reporter = current_reporter,     start_end_reporter = FALSE, load_helpers = FALSE, wrap = wrap)
      62: force(code)
      63: doWithOneRestart(return(expr), restart)
      64: withOneRestart(expr, restarts[[1L]])
      65: withRestarts(testthat_abort_reporter = function() NULL, force(code))
      66: with_reporter(reporter = current_reporter, results <- lapply(paths,     test_file, env = env, reporter = current_reporter, start_end_reporter = FALSE,     load_helpers = FALSE, wrap = wrap))
      67: test_files(paths, reporter = reporter, env = env, stop_on_failure = stop_on_failure,     stop_on_warning = stop_on_warning, wrap = wrap)
      68: test_dir(path = test_path, reporter = reporter, env = env, filter = filter,     ..., stop_on_failure = stop_on_failure, stop_on_warning = stop_on_warning,     wrap = wrap)
      69: test_package_dir(package = package, test_path = test_path, filter = filter,     reporter = reporter, ..., stop_on_failure = stop_on_failure,     stop_on_warning = stop_on_warning, wrap = wrap)
      70: test_check("portalr")
      An irrecoverable exception occurred. R is aborting now ...
    ```

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

Run `revdep_details(,"PupillometryR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Calculating mean pupil size in each timebin 
    
    Error: Column name `Trial` must not be duplicated.
    Use .name_repair to specify repair.
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
     14.                       └─tibble:::lst_to_tibble(xlq$output, .rows, .name_repair, lengths = xlq$leng
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
* Number of recursive dependencies: 55

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
* Number of recursive dependencies: 46

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
    > 
    > # create clusters
    > cluster1 <- qm_define(118600, 119101, 119300)
    > cluster2 <- qm_define(119300, 121200, 121100)
    > 
    > # create cluster objects
    > cluster_obj1 <- qm_create(ref = stl, key = TRACTCE, value = cluster1,
    +     rid = 1, cid = 1, category = "positive")
    Error: `nm` must be `NULL` or a character vector the same length as `x`
    Backtrace:
        █
     1. └─qualmap::qm_create(...)
     2.   ├─dplyr::left_join(ref, value_df, by = keyVarQ)
     3.   ├─sf:::left_join.sf(ref, value_df, by = keyVarQ)
     4.   │ └─sf:::sf_join(NextMethod(), attr(x, "sf_column"), suffix[1])
     5.   │   └─sf_column %in% names(g)
     6.   ├─base::NextMethod()
     7.   └─dplyr:::left_join.data.frame(ref, value_df, by = keyVarQ)
     8.     └─dplyr:::join_mutate(...)
     9.       └─rlang::set_names(x[vars$x$key], names(vars$x$key))
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Backtrace:
       1. qualmap::qm_create(...)
       7. dplyr:::left_join.data.frame(ref, value_df, by = keyVarQ)
       8. dplyr:::join_mutate(...)
       9. rlang::set_names(x[vars$x$key], names(vars$x$key))
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 41 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 4 ]
      1. Error: (unknown) (@test_3_qm_preview.R#54) 
      2. Error: (unknown) (@test_4_qm_create.R#110) 
      3. Error: (unknown) (@test_5_qm_combine.R#9) 
      4. Error: (unknown) (@test_6_qm_summarize.R#9) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# rabhit

<details>

* Version: 0.1.4
* Source code: https://github.com/cran/rabhit
* URL: https://yaarilab.bitbucket.io/RAbHIT/
* BugReports: https://bitbucket.org/yaarilab/haplotyper/issues
* Date/Publication: 2020-01-29 20:20:02 UTC
* Number of recursive dependencies: 109

Run `revdep_details(,"rabhit")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Call `lifecycle::last_warnings()` to see where this warning was generated.
    Using `n` as weighting variable
    Error: Column 'n' is already present in output
     * Use `name = "new_name"` to pick a new name
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
     14.                       └─dplyr:::glubort(...)
    Execution halted
    ```

# raceland

<details>

* Version: 1.0.5
* Source code: https://github.com/cran/raceland
* URL: https://nowosad.github.io/raceland/
* BugReports: https://github.com/Nowosad/raceland/issues
* Date/Publication: 2020-03-01 12:30:06 UTC
* Number of recursive dependencies: 74

Run `revdep_details(,"raceland")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > #1
    > df = calculate_metrics(x, w, neighbourhood = 4, fun = "mean")
    > 
    > #2
    > df2 = calculate_metrics(x, w, neighbourhood = 4, fun = "mean", size = 10, threshold = 0.5)
    > my_grid = create_grid(x, size = 10)
    > 
    > df3 = dplyr::filter(df2, realization == 2)
    > result = dplyr::left_join(my_grid, df2, by = c("row", "col"))
    Error: `nm` must be `NULL` or a character vector the same length as `x`
    Backtrace:
        █
     1. ├─dplyr::left_join(my_grid, df2, by = c("row", "col"))
     2. ├─sf:::left_join.sf(my_grid, df2, by = c("row", "col"))
     3. │ └─sf:::sf_join(NextMethod(), attr(x, "sf_column"), suffix[1])
     4. │   └─sf_column %in% names(g)
     5. ├─base::NextMethod()
     6. └─dplyr:::left_join.data.frame(my_grid, df2, by = c("row", "col"))
     7.   └─dplyr:::join_mutate(...)
     8.     └─rlang::set_names(x[vars$x$key], names(vars$x$key))
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘comat’ ‘rgdal’
      All declared Imports should be used.
    ```

# radiant.basics

<details>

* Version: 1.0.0
* Source code: https://github.com/cran/radiant.basics
* URL: https://github.com/radiant-rstats/radiant.basics, https://radiant-rstats.github.io/radiant.basics, https://radiant-rstats.github.io/docs
* BugReports: https://github.com/radiant-rstats/radiant.basics/issues
* Date/Publication: 2019-07-30 04:40:06 UTC
* Number of recursive dependencies: 132

Run `revdep_details(,"radiant.basics")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      x[1]:  clarity not equal to 0.05 \n\n  prop mean    sd     n n_missing\n 0.033  
      x[1]:  99 9.784 3,000         0\n\n# A tibble: 1 x 6\n  diff   ns    p.value ...
      y[1]: "Single proportion test (binomial exact)\nData      : diamonds \nVariable 
      y[1]:  : clarity \nLevel     : IF in clarity \nConfidence: 0.95 \nNull hyp. : th
      y[1]: e proportion of IF in clarity = 0.05 \nAlt. hyp. : the proportion of IF in
      y[1]:  clarity not equal to 0.05 \n\n  prop mean    sd     n n_missing\n 0.033  
      y[1]:  99 9.784 3,000         0\n\n   diff ns p.value  2.5% 97.5%    \n -0.01...
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 8 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 2 ]
      1. Failure: single_prop 1 (@test_stats.R#48) 
      2. Failure: single_prop 2 (@test_stats.R#56) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# radiant.data

<details>

* Version: 1.0.6
* Source code: https://github.com/cran/radiant.data
* URL: https://github.com/radiant-rstats/radiant.data, https://radiant-rstats.github.io/radiant.data, https://radiant-rstats.github.io/docs
* BugReports: https://github.com/radiant-rstats/radiant.data/issues
* Date/Publication: 2019-08-22 06:30:08 UTC
* Number of recursive dependencies: 142

Run `revdep_details(,"radiant.data")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      The following object is masked from 'package:testthat':
      
          describe
      
      ── 1. Failure: get_data (@test_funs.R#41)  ─────────────────────────────────────
      `.` not equal to `%>%`(...).
      Attributes: < Component "row.names": Modes: character, numeric >
      Attributes: < Component "row.names": target is character, current is numeric >
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 33 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Failure: get_data (@test_funs.R#41) 
      
      Error: testthat unit tests failed
      Execution halted
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
      [ OK: 1410 | SKIPPED: 8 | WARNINGS: 0 | FAILED: 32 ]
      1. Failure: correct means and std devs for step_norm (@test_center_scale_norm.R#135) 
      2. Failure: correct means and std devs for step_norm (@test_center_scale_norm.R#145) 
      3. Failure: defaults (@test_classdist.R#47) 
      4. Failure: check_col works in the bake stage (@test_colcheck.R#22) 
      5. Failure: check_col works in the bake stage (@test_colcheck.R#25) 
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

* Version: 0.2.0
* Source code: https://github.com/cran/rFIA
* Date/Publication: 2020-01-09 17:50:05 UTC
* Number of recursive dependencies: 81

Run `revdep_details(,"rFIA")` for more info

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
    New names:
    ```

# RNeXML

<details>

* Version: 2.4.3
* Source code: https://github.com/cran/RNeXML
* URL: https://docs.ropensci.org/RNeXML, https://github.com/ropensci/RNeXML
* BugReports: https://github.com/ropensci/RNeXML/issues
* Date/Publication: 2020-03-01 05:50:02 UTC
* Number of recursive dependencies: 134

Run `revdep_details(,"RNeXML")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
<<<<<<< Updated upstream
       50. vctrs::vec_default_cast(x, to, x_arg = x_arg, to_arg = to_arg)
       51. vctrs::stop_incompatible_cast(x, to, x_arg = x_arg, to_arg = to_arg)
       52. vctrs:::stop_incompatible(...)
       53. vctrs:::stop_vctrs(...)
=======
       28. tibble::tibble(!!!.x)
       29. tibble:::lst_to_tibble(xlq$output, .rows, .name_repair, lengths = xlq$lengths)
       30. tibble:::set_repaired_names(x, .name_repair)
       32. tibble:::repaired_names(names(x), .name_repair = .name_repair)
       33. tibble:::check_unique(new_name)
>>>>>>> Stashed changes
      
      Done simulation(s).
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 281 | SKIPPED: 42 | WARNINGS: 2 | FAILED: 3 ]
      1. Error: we can correctly parse nested ResourceMeta annotations (@test_meta_extract.R#128) 
      2. Error: metadata tables can be requested in simplified form (@test_meta_extract.R#155) 
      3. Error: ID assignments are correct and complete when meta are nested (@test_meta_extract.R#168) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# rsample

<details>

* Version: 0.0.5
* Source code: https://github.com/cran/rsample
* URL: https://tidymodels.github.io/rsample
* BugReports: https://github.com/tidymodels/rsample/issues
* Date/Publication: 2019-07-12 22:20:11 UTC
* Number of recursive dependencies: 89

Run `revdep_details(,"rsample")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Attributes: < Component 2: target is character, current is numeric >
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 524 | SKIPPED: 0 | WARNINGS: 13 | FAILED: 8 ]
      1. Failure: rsplit labels (@test_boot.R#89) 
      2. Failure: rsplit labels (@test_group.R#99) 
      3. Failure: rsplit labels (@test_loo.R#38) 
      4. Failure: rsplit labels (@test_mc.R#86) 
      5. Failure: rsplit labels (@test_nesting.R#71) 
      6. Failure: rsplit labels (@test_rolling.R#88) 
      7. Failure: rsplit labels (@test_vfold.R#85) 
      8. Failure: rsplit labels (@test_vfold.R#90) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘recipes’
    ```

# RSQL

<details>

* Version: 0.1.1
* Source code: https://github.com/cran/RSQL
* URL: https://github.com/rOpenStats/rsql
* BugReports: https://github.com/rOpenStats/rsql/issues
* Date/Publication: 2020-02-05 16:10:11 UTC
* Number of recursive dependencies: 63

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
    Error: `x` must be a vector, not a `tbl_df/tbl/data.frame/rstatix_test/t_test` object.
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
     14. │                     └─magrittr::freduce(value, `_functi
    Execution halted
    ```

# RTD

<details>

* Version: 0.1.1
* Source code: https://github.com/cran/RTD
* Date/Publication: 2019-01-02 13:50:04 UTC
* Number of recursive dependencies: 53

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

*   checking examples ... ERROR
    ```
    ...
    Use 'functions' to extract the individual functions. 
    
    > 
    > # Dealing with one column edge case
    > improper_pack <- . %>% dplyr::transmute_at(
    +   dplyr::vars(vs),
    +   rules(improper_is_neg = . < 0)
    + )
    > 
    > proper_pack <- . %>% dplyr::transmute_at(
    +   dplyr::vars(vs = vs),
    +   rules(proper_is_neg = . < 0)
    + )
    > 
    > mtcars[1:2, ] %>%
    +   expose(cell_packs(improper_pack, proper_pack)) %>%
    +   get_report()
    Error in `keys<-`(`*tmp*`, value = keys(x)[i, , drop = FALSE]) : 
      Keys object should have the same number of rows as data.
    Calls: %>% ... group_data.data.frame -> vec_init -> [ -> [.keyed_df -> keys<-
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 216 | SKIPPED: 1 | WARNINGS: 0 | FAILED: 11 ]
      1. Failure: act_after_exposure works (@test-actions.R#66) 
      2. Failure: bind_exposures removes names from list-column `fun` (@test-expose-helpers.R#111) 
      3. Failure: expose works (@test-expose.R#159) 
      4. Error: expose works (@test-expose.R#162) 
      5. Error: expose works (@test-expose.R#134) 
      6. Error: (unknown) (@test-expose.R#134) 
      7. Failure: print.exposure passes tibble options (@test-exposure.R#365) 
      8. Failure: print.exposure passes tibble options (@test-exposure.R#380) 
      9. Failure: print.exposure passes tibble options (@test-exposure.R#395) 
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

# sabre

<details>

* Version: 0.3.2
* Source code: https://github.com/cran/sabre
* URL: https://nowosad.github.io/sabre/
* BugReports: https://github.com/Nowosad/sabre/issues
* Date/Publication: 2019-10-17 16:20:03 UTC
* Number of recursive dependencies: 70

Run `revdep_details(,"sabre")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### ** Examples
    > 
    > library(sf)
    Linking to GEOS 3.7.2, GDAL 2.4.2, PROJ 5.2.0
    > data("regions1")
    > data("regions2")
    > vm = vmeasure_calc(x = regions1, y = regions2, x_name = z, y_name = z)
    Error: `nm` must be `NULL` or a character vector the same length as `x`
    Backtrace:
         █
      1. ├─sabre::vmeasure_calc(...)
      2. └─sabre:::vmeasure_calc.sf(...)
      3.   ├─dplyr::left_join(x, x_df, by = "map1")
      4.   ├─sf:::left_join.sf(x, x_df, by = "map1")
      5.   │ └─sf:::sf_join(NextMethod(), attr(x, "sf_column"), suffix[1])
      6.   │   └─sf_column %in% names(g)
      7.   ├─base::NextMethod()
      8.   └─dplyr:::left_join.data.frame(x, x_df, by = "map1")
      9.     └─dplyr:::join_mutate(...)
     10.       └─rlang::set_names(x[vars$x$key], names(vars$x$key))
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      `nm` must be `NULL` or a character vector the same length as `x`
      Backtrace:
        1. sabre::vmeasure_calc(...)
        2. sabre:::vmeasure_calc.sf(...)
        8. dplyr:::left_join.data.frame(x, x_df, by = "map1")
        9. dplyr:::join_mutate(...)
       10. rlang::set_names(x[vars$x$key], names(vars$x$key))
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 13 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 2 ]
      1. Error: (unknown) (@test-eco-us.R#3) 
      2. Error: (unknown) (@test-fig1.R#5) 
      
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
<<<<<<< Updated upstream
      ── 5. Failure: unigrams are computed properly (@test_unigram.R#18)  ────────────
=======
      
      ── 4. Failure: unigrams are computed properly (@test_unigram.R#18)  ────────────
>>>>>>> Stashed changes
      saotd::unigram(DataFrame = test_unigram_df) not equal to `correct_unigram_df`.
      Attributes: < Component "class": Lengths (1, 3) differ (string compare on first 1) >
      Attributes: < Component "class": 1 string mismatch >
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
<<<<<<< Updated upstream
      [ OK: 63 | SKIPPED: 0 | WARNINGS: 6 | FAILED: 5 ]
      1. Failure: bigrams are computed properly (@test_bigram.R#19) 
      2. Error: (unknown) (@test_number_topics.R#12) 
      3. Failure: Trigrams are computed properly (@test_trigram.R#21) 
      4. Error: (unknown) (@test_tweet_topics.R#12) 
      5. Failure: unigrams are computed properly (@test_unigram.R#18) 
=======
      [ OK: 65 | SKIPPED: 0 | WARNINGS: 7 | FAILED: 4 ]
      1. Failure: bigrams are computed properly (@test_bigram.R#19) 
      2. Failure: Trigrams are computed properly (@test_trigram.R#21) 
      3. Error: (unknown) (@test_tweet_topics.R#12) 
      4. Failure: unigrams are computed properly (@test_unigram.R#18) 
>>>>>>> Stashed changes
      
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
      ── 2. Failure: Giving the expected result (@test_extract_top_genes.R#30)  ──────
      suppressMessages(define_top_genes(input1, min_expression = 4)) not equal to `result1`.
      Component "restofgenes": Component "geneName": 4 string mismatches
      Component "restofgenes": Component "mean": Mean relative difference: 0.8
      Component "restofgenes": Component "cv": Mean relative difference: 0.8
      Component "restofgenes": Component "x1": Mean relative difference: 0.8
      Component "restofgenes": Component "x2": Mean relative difference: 0.8
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 32 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 2 ]
      1. Failure: Giving the expected result (@test_extract_top_genes.R#26) 
      2. Failure: Giving the expected result (@test_extract_top_genes.R#30) 
      
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

* Version: 1.16.0
* Source code: https://github.com/cran/sevenbridges
* URL: https://www.sevenbridges.com, https://sbg.github.io/sevenbridges-r/, https://github.com/sbg/sevenbridges-r
* BugReports: https://github.com/sbg/sevenbridges-r/issues
* Date/Publication: 2019-10-29
* Number of recursive dependencies: 62

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
        R     2.1Mb
        doc   9.5Mb
    ```

# sf

<details>

* Version: 0.8-1
* Source code: https://github.com/cran/sf
* URL: https://github.com/r-spatial/sf/, https://r-spatial.github.io/sf/
* BugReports: https://github.com/r-spatial/sf/issues/
* Date/Publication: 2020-01-28 11:20:07 UTC
* Number of recursive dependencies: 131

Run `revdep_details(,"sf")` for more info

</details>

## Newly broken

*   checking whether package ‘sf’ can be installed ... ERROR
    ```
<<<<<<< Updated upstream
    ...
    ---
    > MULTIPOINT (0 501.4646, 0 1504.394, 0 2507.323,...
    > MULTIPOINT (505.9977 0, 1517.993 0, 2529.988 0,...
     ERROR
    Running the tests in ‘tests/dplyr.R’ failed.
    Last 13 lines of output:
      > 
      > nrow(distinct(nc[c(1,1,1,2,2,3:100),]))
      [1] 100
      > 
      > # set.seed(1331)
      > nc$gp <- sample(1:10, replace=T)
      > # Get centroid of each group of polygons; https://github.com/r-spatial/sf/issues/969
      > nc_gp_cent <- nc %>%
      +                 group_by(gp) %>%
      +                 group_map(st_centroid)
      Error in UseMethod("st_as_sf") : 
        no applicable method for 'st_as_sf' applied to an object of class "list"
      Calls: %>% ... <Anonymous> -> group_map -> group_map.sf -> st_as_sf
      In addition: There were 14 warnings (use warnings() to see them)
      Execution halted
=======
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/sf/new/sf.Rcheck/00install.out’ for details.
>>>>>>> Stashed changes
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

* Version: 0.2.0
* Source code: https://github.com/cran/silicate
* Date/Publication: 2019-10-09 11:30:02 UTC
* Number of recursive dependencies: 136

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
    Error: Tibble columns must have consistent lengths, only values of length one are recycled:
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       15. base::lapply(.x, .f, ...)
       16. dplyr:::FUN(X[[i]], ...)
       17. tibble::tibble(!!!.x)
       18. tibble:::lst_to_tibble(xlq$output, .rows, .name_repair, lengths = xlq$lengths)
       19. tibble:::recycle_columns(x, .rows, lengths)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 79 | SKIPPED: 7 | WARNINGS: 1 | FAILED: 4 ]
      1. Error: ARC for non polygons is a warnable offence (@test-arc-tests.R#4) 
      2. Failure: generic forms are understood (@test-generic-data.R#14) 
      3. Error: print works (@test-print.R#5) 
      4. Error: object and path names as expected (@test-sf-decomposition.R#34) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘geometry’
      All declared Imports should be used.
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
      simTool:::unnest_simulation(e) not equal to `expected_df`.
      Component "simulation": Attributes: < Names: 1 string mismatch >
      Component "simulation": Attributes: < Length mismatch: comparison on first 2 components >
      Component "simulation": Attributes: < Component 2: Modes: numeric, list >
      Component "simulation": Attributes: < Component 2: Lengths: 4, 2 >
      Component "simulation": Attributes: < Component 2: names for current but not for target >
      Component "simulation": Attributes: < Component 2: target is numeric, current is list >
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 98 | SKIPPED: 0 | WARNINGS: 1 | FAILED: 2 ]
      1. Failure: Two groups for summary_fun. Results were created and stored in simulation (@test_eval_tibbles.R#824) 
      2. Failure: Simple unnesting (@test_unnest_simulation.R#20) 
      
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
* Number of recursive dependencies: 31

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

# sjmisc

<details>

* Version: 2.8.3
* Source code: https://github.com/cran/sjmisc
* URL: https://strengejacke.github.io/sjmisc
* BugReports: https://github.com/strengejacke/sjmisc/issues
* Date/Publication: 2020-01-10 05:30:14 UTC
* Number of recursive dependencies: 96

Run `revdep_details(,"sjmisc")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > 
    > # create sample
    > mydat <- data.frame(age = c(20, 30, 40),
    +                     sex = c("Female", "Male", "Male"),
    +                     score_t1 = c(30, 35, 32),
    +                     score_t2 = c(33, 34, 37),
    +                     score_t3 = c(36, 35, 38),
    +                     speed_t1 = c(2, 3, 1),
    +                     speed_t2 = c(3, 4, 5),
    +                     speed_t3 = c(1, 8, 6))
    > 
    > # gather multiple columns. both time and speed are gathered.
    > to_long(
    +   data = mydat,
    +   keys = "time",
    +   values = c("score", "speed"),
    +   c("score_t1", "score_t2", "score_t3"),
    +   c("speed_t1", "speed_t2", "speed_t3")
    + )
    Error: Can't column-bind data frames with different row names.
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

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 553 | SKIPPED: 3 | WARNINGS: 0 | FAILED: 21 ]
      1. Failure: dplyr::filter works as expected (@test-dplyr.R#8) 
      2. Failure: dplyr::select works as expected (@test-dplyr.R#17) 
      3. Failure: dplyr::select works as expected (@test-dplyr.R#20) 
      4. Failure: dplyr::mutate works as expected (@test-dplyr.R#26) 
      5. Failure: dplyr::slice works as expected (@test-dplyr.R#35) 
      6. Failure: dplyr::arrange works as expected (@test-dplyr.R#41) 
      7. Failure: Skim prints a header for the entire output and each type (@test-skim_print.R#6) 
      8. Failure: Skim prints a header for the entire output and each type (@test-skim_print.R#9) 
      9. Failure: knit_print produces expected results (@test-skim_print.R#33) 
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
* Number of recursive dependencies: 59

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
# spatialrisk

<details>

* Version: 0.6.5
* Source code: https://github.com/cran/spatialrisk
* Date/Publication: 2019-11-06 14:40:03 UTC
* Number of recursive dependencies: 106

Run `revdep_details(,"spatialrisk")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### Name: choropleth
    > ### Title: Create choropleth map
    > ### Aliases: choropleth
    > 
    > ### ** Examples
    > 
    > test <- points_to_polygon(nl_provincie, insurance, sum(amount, na.rm = TRUE))
    71 points fall not within a polygon.
    Error: `nm` must be `NULL` or a character vector the same length as `x`
    Backtrace:
        █
     1. └─spatialrisk::points_to_polygon(...)
     2.   ├─dplyr::left_join(shp_wgs84, df_map_sf2, by = "id")
     3.   ├─sf:::left_join.sf(shp_wgs84, df_map_sf2, by = "id")
     4.   │ └─sf:::sf_join(NextMethod(), attr(x, "sf_column"), suffix[1])
     5.   │   └─sf_column %in% names(g)
     6.   ├─base::NextMethod()
     7.   └─dplyr:::left_join.data.frame(shp_wgs84, df_map_sf2, by = "id")
     8.     └─dplyr:::join_mutate(...)
     9.       └─rlang::set_names(x[vars$x$key], names(vars$x$key))
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        data   5.0Mb
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘sp’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 25 marked Latin-1 strings
      Note: found 658 marked UTF-8 strings
    ```

# spdplyr

<details>

* Version: 0.3.0
* Source code: https://github.com/cran/spdplyr
* URL: https://github.com/mdsumner/spdplyr
* BugReports: https://github.com/mdsumner/spdplyr/issues
* Date/Publication: 2019-05-13 10:30:02 UTC
* Number of recursive dependencies: 66

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

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 226 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 9 ]
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

# stacomiR

<details>

* Version: 0.5.4.3
* Source code: https://github.com/cran/stacomiR
* URL: http://stacomir.r-forge.r-project.org/
* BugReports: https://github.com/MarionLegrandLogrami/stacomiR/issues
* Date/Publication: 2020-03-18 15:20:13 UTC
* Number of recursive dependencies: 103

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
      installed size is 16.2Mb
      sub-directories of 1Mb or more:
        doc  10.3Mb
        nc    4.5Mb
    ```

# stplanr

<details>

* Version: 0.5.1
* Source code: https://github.com/cran/stplanr
* URL: https://github.com/ropensci/stplanr, https://docs.ropensci.org/stplanr/
* BugReports: https://github.com/ropensci/stplanr/issues
* Date/Publication: 2020-03-01 22:20:02 UTC
* Number of recursive dependencies: 124

Run `revdep_details(,"stplanr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
                                    TRUE                                 TRUE 
            Motorcycle..scooter.or.moped                 Driving.a.car.or.van 
                                    TRUE                                 TRUE 
               Passenger.in.a.car.or.van                              Bicycle 
                                    TRUE                                 TRUE 
                                 On.foot       Other.method.of.travel.to.work 
                                    TRUE                                 TRUE 
    > # Demonstrate the results from oneway and onewaygeo are identical
    > flow_oneway_geo <- onewaygeo(flowlines, attrib = attrib)
    > flow_oneway_sf <- od_oneway(flowlines_sf)
    Joining, by = c("Area.of.residence", "Area.of.workplace")
    Error: `nm` must be `NULL` or a character vector the same length as `x`
    Backtrace:
        █
     1. └─stplanr::od_oneway(flowlines_sf)
     2.   ├─sf::st_as_sf(dplyr::left_join(x_oneway, x_sf))
     3.   ├─dplyr::left_join(x_oneway, x_sf)
     4.   └─dplyr:::left_join.data.frame(x_oneway, x_sf)
     5.     └─dplyr:::join_mutate(...)
     6.       └─rlang::set_names(y[vars$y$key], names(vars$y$key))
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        doc   3.0Mb
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

# strataG

<details>

* Version: 2.4.905
* Source code: https://github.com/cran/strataG
* URL: https://github.com/EricArcher/strataG
* BugReports: https://github.com/EricArcher/strataG/issues
* Date/Publication: 2020-02-28 07:10:02 UTC
* Number of recursive dependencies: 174

Run `revdep_details(,"strataG")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    ℹ The error occured in group 1: stratum = "Coastal".
    ✖ Column `id` not found in `.data`
    Backtrace:
         █
      1. ├─(new("standardGeneric", .Data = function (object) ...
      2. └─(new("standardGeneric", .Data = function (object) ...
      3.   ├─strataG:::.printBaseSmry(.baseSmry(object))
      4.   │ └─strataG:::.printSmryHeader(x)
      5.   │   └─base::paste(...)
      6.   └─strataG:::.baseSmry(object)
      7.     ├─`%>%`(...)
      8.     │ └─base::eval(lhs, parent, parent)
      9.     │   └─base::eval(lhs, parent, parent)
     10.     ├─apex::getNumInd(g, TRUE)
     11.     └─strataG::getNumInd(g, TRUE)
     12.       └─strataG:::.local(x, ...)
     13.         └─`%>%`(...)
     14.           ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
     15.           └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
     16.             └─base::eval(quote(`_fseq`(`
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       18. base:::tryCatchList(expr, names[-nh], parentenv, handlers[-nh])
       21. base:::tryCatchList(expr, names[-nh], parentenv, handlers[-nh])
       24. base:::tryCatchList(expr, names[-nh], parentenv, handlers[-nh])
       27. base:::tryCatchList(expr, names[-nh], parentenv, handlers[-nh])
       28. base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
       29. value[[3L]](cond)
       30. dplyr:::stop_error_data_pronoun_not_found(...)
       31. dplyr:::stop_dplyr(...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 108 | SKIPPED: 0 | WARNINGS: 5 | FAILED: 1 ]
      1. Error: heterozygosity is computed and formed correctly (@test-locus.summaries.R#93) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘DT’ ‘Hmisc’ ‘RColorBrewer’ ‘Rcpp’ ‘readr’
      All declared Imports should be used.
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

* Version: 1.0.0
* Source code: https://github.com/cran/StratigrapheR
* Date/Publication: 2020-02-26 05:40:02 UTC
* Number of recursive dependencies: 123

Run `revdep_details(,"StratigrapheR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### Name: mat.lag
    > ### Title: Find the "next" or "previous" values in a matrix.
    > ### Aliases: mat.lag mat.lead
    > 
    > ### ** Examples
    > 
    > m <- matrix(1:120, ncol = 12)
    > 
    > mat.lag(m)
    Error: Can't subset elements that don't exist.
    ✖ The locations 11, 12, 13, 14, 15, etc. don't exist.
    ℹ There are only 10 elements.
    Backtrace:
        █
     1. ├─StratigrapheR::mat.lag(m)
     2. │ └─dplyr::lag(m, n, default = default)
     3. │   ├─vctrs::vec_c(...)
     4. │   └─vctrs::vec_slice(x, seq_len(xlen - n))
     5. └─vctrs:::stop_subscript_oob(...)
     6.   └─vctrs:::stop_subscript(...)
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
    > # Create centroids set
    > centroids <- create_centroids(tas_lga, sf_id = "LGA_CODE16")
    Warning in st_centroid.sf(., of_largest_polygon = largest) :
      st_centroid assumes attributes are constant over geometries of x
    Warning in st_centroid.sfc(st_geometry(x), of_largest_polygon = of_largest_polygon) :
      st_centroid does not give correct centroids for longitude/latitude data
    Linking to GEOS 3.7.2, GDAL 2.4.2, PROJ 5.2.0
    > # Create hexagon location grid
    > data(capital_cities)
    > grid <- create_grid(centroids = centroids, hex_size = 0.2, buffer_dist = 1.2)
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
* Number of recursive dependencies: 88

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

# summariser

<details>

* Version: 2.0.0
* Source code: https://github.com/cran/summariser
* URL: https://github.com/condwanaland/summariser
* Date/Publication: 2020-02-23 17:50:03 UTC
* Number of recursive dependencies: 46

Run `revdep_details(,"summariser")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── 1. Error: Three levels of grouping calculates correctly (@test_calculations.R
      Corrupt grouped_df data using the old format
      Backtrace:
        1. testthat::expect_equivalent(...)
        5. dplyr:::`[.grouped_df`(triple_grouped_df, , 1)
       11. dplyr:::group_vars.data.frame(x)
       14. dplyr:::group_data.grouped_df(x)
       15. dplyr::validate_grouped_df(.data)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 17 | SKIPPED: 0 | WARNINGS: 16 | FAILED: 1 ]
      1. Error: Three levels of grouping calculates correctly (@test_calculations.R#48) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘rlang’
      All declared Imports should be used.
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
    Error: `df` must be a data frame without row names in `column_to_rownames()`.
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
     16.               
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
     13.                   └─dplyr::grouped_df(...)
     14.                     └─dplyr:::compute_groups(data, vars, drop = drop)
     15.                       ├─tibble::as_tibble(data)
     16.              
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
* Number of recursive dependencies: 76

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
       23. dplyr:::mutate_cols(.data, !!!vars[i])
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

# tibbletime

<details>

* Version: 0.1.3
* Source code: https://github.com/cran/tibbletime
* URL: https://github.com/business-science/tibbletime
* BugReports: https://github.com/business-science/tibbletime/issues
* Date/Publication: 2019-09-20 05:00:02 UTC
* Number of recursive dependencies: 64

Run `revdep_details(,"tibbletime")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Backtrace:
        1. testthat::expect_equal(get_index_col(FANG_g_time), get_index_col(FANG_unnested))
        4. tibbletime::get_index_col(FANG_unnested)
        7. tibbletime::get_index_char(.tbl_time)
       13. tibbletime::get_index_quo(.tbl_time)
       14. tibbletime:::glue_stop("Object is not of class `tbl_time`.")
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 137 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 3 ]
      1. Failure: nest() with index creates tbl_df (@test_compat-tidyr.R#25) 
      2. Failure: unnest() with index goes back to tbl_time (@test_compat-tidyr.R#49) 
      3. Error: unnest() with index goes back to tbl_time (@test_compat-tidyr.R#50) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# tidybayes

<details>

* Version: 2.0.1
* Source code: https://github.com/cran/tidybayes
* URL: http://mjskay.github.io/tidybayes, https://github.com/mjskay/tidybayes
* BugReports: https://github.com/mjskay/tidybayes/issues/new
* Date/Publication: 2020-01-28 21:20:02 UTC
* Number of recursive dependencies: 194

Run `revdep_details(,"tidybayes")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 246 | SKIPPED: 128 | WARNINGS: 2 | FAILED: 83 ]
      1. Failure: add_draws works on fit from a simple rstanarm model (@test.add_draws.R#36) 
      2. Failure: add_draws works on fit from a simple rstanarm model (@test.add_draws.R#37) 
      3. Failure: pairwise level comparison works (@test.compare_levels.R#41) 
      4. Failure: pairwise level comparison works (@test.compare_levels.R#43) 
      5. Failure: pairwise level comparison works (@test.compare_levels.R#44) 
      6. Failure: ordered level comparison works (@test.compare_levels.R#59) 
      7. Failure: ordered level comparison works (@test.compare_levels.R#60) 
      8. Failure: control level comparison works (@test.compare_levels.R#75) 
      9. Failure: named functions are supported and named with their own name (@test.compare_levels.R#102) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
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
    Error: `x` must be a vector, not a `tbl_df/tbl/data.frame/unpivotr` object.
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
     16. │ 
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 34 | SKIPPED: 2 | WARNINGS: 5 | FAILED: 32 ]
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
* Number of recursive dependencies: 88

Run `revdep_details(,"tidyjson")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### Title: Bind Rows (tidyjson)
    > ### Aliases: bind_rows
    > 
    > ### ** Examples
    > 
    > 
    > ## Simple example
    > a <- as.tbl_json('{"a": 1, "b": 2}')
    > b <- as.tbl_json('{"a": 3, "b": 4}')
    > 
    > bind_rows(a,b) %>% spread_values(a=jnumber(a),b=jnumber(b))
    # A tbl_json: 2 x 3 tibble with a "JSON" attribute
      `attr(., "JSON")`   document.id     a     b
      <chr>                     <int> <dbl> <dbl>
    1 "{\"a\":1,\"b\":2}"           1     1     2
    2 "{\"a\":3,\"b\":4}"           1     3     4
    > 
    > ## as a list
    > bind_rows(list(a,b)) %>% spread_all()
    Error: nrow(df) not equal to length(json.list)
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 220 | SKIPPED: 7 | WARNINGS: 0 | FAILED: 30 ]
      1. Error: json_schema works for simple examples (@test-json_schema.R#10) 
      2. Error: json_schema works for a more complex object (@test-json_schema.R#23) 
      3. Error: json_schema works for a more complex array (@test-json_schema.R#42) 
      4. Error: works for empty arrays (@test-json_schema.R#52) 
      5. Error: works for complex nested types (@test-json_schema.R#59) 
      6. Error: json_schema works for real examples (@test-json_schema.R#94) 
      7. Error: types = 'value' works as intended (@test-json_schema.R#106) 
      8. Error: simple object works (@test-json_structure.R#27) 
      9. Error: simple array works (@test-json_structure.R#49) 
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
    Error: `mutate()` argument `start_event` must return compatible vectors across groups.
    ℹ `start_event` is `ifelse(...)`.
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
      9.             └─tidymv::create_start_event(., group)
     10.               ├─dplyr::mutate(...)
     11.               └─dplyr:::mutate.data.frame(...)
     12.                 └─dplyr:::mutate_cols(.data, ...)
     13.                   └─base::tryCatch(...)
     14.                     └─base:::tryCatchList(expr, classes, parentenv, handlers)
     15.                       ├─base:::tryCatchOne(...)
     16.                     
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
* Number of recursive dependencies: 60

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
      `x` must be a vector, not a `tbl_df/tbl/data.frame/qwi` object.
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

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 546 | SKIPPED: 0 | WARNINGS: 3 | FAILED: 15 ]
      1. Failure: not drop unspecified levels in complete (@test-complete.R#42) 
      2. Failure: groups are preserved (@test-drop-na.R#28) 
      3. Failure: nesting doesn't expand values (@test-expand.R#17) 
      4. Failure: named data frames are not flattened (@test-expand.R#32) 
      5. Failure: nest turns grouped values into one list-df (@test-nest-legacy.R#8) 
      6. Failure: nest works with data frames too (@test-nest-legacy.R#16) 
      7. Failure: nest doesn't include grouping vars in nested data (@test-nest-legacy.R#30) 
      8. Failure: elements must all be of same type (@test-nest-legacy.R#139) 
      9. Failure: unnesting zero row column preserves names (@test-nest-legacy.R#275) 
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
* Number of recursive dependencies: 59

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
      3.   ├─tibble:::cat_line(format(x, ..., n = n, width = width, n_extra = n_extra))
      4.   │ ├─base::cat(paste0(..., "\n"), sep = "")
      5.   │ └─base::paste0(..., "\n")
      6.   ├─base::format(x, ..., n = n, width = width, n_extra = n_extra)
      7.   └─tibble:::format.tbl(x, ..., n = n, width = width, n_extra = n_extra)
      8.     └─tibble::trunc_mat(x, n = n, width = width, n_extra = n_extra)
      9.       ├─base::as.data.frame(head(x, n))
     10.       ├─utils::head(x, n)
     11.       └─utils:::head.data.frame(x, n)
     12.         ├─x[seq_len(n), , drop = FALSE]
     13.         └─dplyr:::`[.grouped_df`(x, seq_len(n), , drop = FALSE)
     14.           └─dplyr::grouped_df(out, groups, group_by_drop_default(x))
     15.             └─dplyr:::compute_groups(data, vars, dro
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
* Number of recursive dependencies: 35

Run `revdep_details(,"tidystopwords")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
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
      4. ├─vctrs:::vec_ptype2_dispatch_s3(x = x, y = y, x_arg = x_arg, y_arg = y_arg)
      5. ├─vctrs::vec_ptype2.logical(x = x, y = y, x_arg = x_arg, y_arg = y_arg)
      6. └─vctrs:::vec_ptype2.logical.default(...)
      7.   └─vctrs::vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
      8.     └─vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
      9.       └─vctrs:::stop_incompatible(...)
     10.         └─vctrs:::stop_vctrs(...)
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
* Number of recursive dependencies: 83

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
      'names' attribute [1] must be the same length as the vector [0]
      Backtrace:
        1. tidytransit::get_trip_geometry(duke_sf, trip_ids)
       22. sf:::vec_restore.sfc(x = x, to = to, n = n)
       23. sf::st_sfc(x, crs = st_crs(to), precision = st_precision(to))
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 97 | SKIPPED: 9 | WARNINGS: 0 | FAILED: 4 ]
      1. Error: get_route_geometry (@test-spatial.R#23) 
      2. Error: route_geometry behaves as before (@test-spatial.R#33) 
      3. Error: one shape per trip is returned (@test-spatial.R#47) 
      4. Error: two shapes are returned even if trips use the same shape_id (@test-spatial.R#56) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.2Mb
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
    > 
    > data("endoderm_small")
    > endoderm_small <- clusterTimeSeries(endoderm_small)
    Normalizing data...
    Aggregating across replicates...
    Converting to timeseries format...
    Averaging timecourses over all 'groups' selected and recomputing lags with coefficients: 0.5 0.25
    Error: `df` must be a data frame without row names in `column_to_rownames()`.
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
      installed size is 13.5Mb
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
      Items of 'old' not found in column names: [Categories]. Consider skip_absent=TRUE.
      Backtrace:
       1. test.df.adj.Nei[, !(names(test.df.adj.Nei) %in% "Neighborhood")]
       1. data.table::as.data.table(.)
       9. data.table::setnames(., "Categories", "Neighborhood")
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 11 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 2 ]
      1. Error: Test to see if lookup tables and joins are performed correctly (@test-bin.oth.R#63) 
      2. Error: Testing for 2 predictors,
                both will recategorize the variable.
                Recategorized variable will contain multiple leaves. (@test-tree.bins.R#169) 
      
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
* Number of recursive dependencies: 51

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

# trelliscopejs

<details>

* Version: 0.2.4
* Source code: https://github.com/cran/trelliscopejs
* URL: https://github.com/hafen/trelliscopejs
* BugReports: https://github.com/hafen/trelliscopejs/issues
* Date/Publication: 2020-02-10 22:40:02 UTC
* Number of recursive dependencies: 100

Run `revdep_details(,"trelliscopejs")` for more info

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
    Error: `x` must be a vector, not a `grouped_df/tbl_df/tbl/data.frame/cognostics` object.
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

*   checking tests ...
    ```
     ERROR
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

# tsbox

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/tsbox
* URL: https://www.tsbox.help
* BugReports: https://github.com/christophsax/tsbox/issues
* Date/Publication: 2019-08-06 06:40:02 UTC
* Number of recursive dependencies: 92

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
* Number of recursive dependencies: 92

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
      [ OK: 524 | SKIPPED: 2 | WARNINGS: 2 | FAILED: 34 ]
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
      [ OK: 244 | SKIPPED: 0 | WARNINGS: 159 | FAILED: 10 ]
      1.  Failure: rectify() has the right row/col headers (@test-rectify.R#18) 
      2.  Failure: rectify() has the right row/col headers (@test-rectify.R#26) 
      3.  Failure: rectify() isn't confused by same-named variables in context (@test-rectify.R#46) 
      4.  Failure: rectify() can use 'row', 'col' and 'data_type' as values (@test-rectify.R#66) 
      5.  Failure: rectify() can use 'row', 'col' and 'data_type' as values (@test-rectify.R#75) 
      6.  Failure: rectify() can use 'row', 'col' and 'data_type' as values (@test-rectify.R#84) 
      7.  Failure: Blank initial rows and columns are handled (@test-rectify.R#103) 
      8.  Failure: rectify() allows formatting formulas (@test-rectify.R#123) 
      9.  Failure: Extraneous columns on the edges are dropped (@test-rectify.R#146) 
      10. Failure: Blank columns amongst the data are retained (@test-rectify.R#166) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# ushr

<details>

* Version: 0.2.1
* Source code: https://github.com/cran/ushr
* URL: https://github.com/SineadMorris/ushr
* Date/Publication: 2020-02-20 17:50:02 UTC
* Number of recursive dependencies: 70

Run `revdep_details(,"ushr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Error: `filter()` argument `..1` errored.
    ℹ `..1` is `!is.na(index)`.
    ✖ object 'index' not found
    Backtrace:
         █
      1. └─ushr::ushr(data = simulated_data)
      2.   └─ushr:::fit_model(...)
      3.     └─`%>%`(...)
      4.       ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      5.       └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      6.         └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      7.           └─ushr:::`_fseq`(`_lhs`)
      8.             └─magrittr::freduce(value, `_function_list`)
      9.               └─function_list[[i]](value)
     10.                 ├─dplyr::filter(., !is.na(index))
     11.                 └─dplyr:::filter.data.frame(., !is.na(index))
     12.                   └─dplyr:::filter_rows(.data, ...)
     13.                     └─base::tryCatch(...)
     14.                       └─base:::tryCatchList(expr, classes, parentenv, handlers)
     15.                         └─base:::tryCatchOne(expr, names, parentenv, h
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       17. dplyr:::filter_rows(.data, ...)
       18. base::tryCatch(...)
       19. base:::tryCatchList(expr, classes, parentenv, handlers)
       20. base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
       21. value[[3L]](cond)
       22. dplyr:::stop_eval_tidy(...)
       23. dplyr:::stop_dplyr(index, dots, fn, "errored", x = conditionMessage(e))
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 11 | SKIPPED: 0 | WARNINGS: 1 | FAILED: 2 ]
      1. Error: test correct input data (@test-filtering.R#18) 
      2. Error: test input arguments (@test-fitting.R#8) 
      
      Error: testthat unit tests failed
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
      [ OK: 424 | SKIPPED: 3 | WARNINGS: 2 | FAILED: 15 ]
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
    Running the tests in ‘tests/test-labeller.R’ failed.
    Last 13 lines of output:
        2. ├─vpc:::vpc.default(...)
        3. │ └─base::do.call(...)
        4. └─(function (sim = NULL, obs = NULL, psn_folder = NULL, bins = "jenks", ...
        5.   └─aggr_sim %>% dplyr::group_by(strat, bin)
        6.     ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
        7.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
        8.       └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
        9.         └─vpc:::`_fseq`(`_lhs`)
       10.           └─magrittr::freduce(value, `_function_list`)
       11.             ├─base::withVisible(function_list[[k]](value))
       12.             └─function_list[[k]](value)
       13.               ├─dplyr::group_by(., strat, bin)
       14.               └─dplyr:::group_by.data.frame(., strat, bin)
       15.                 └─dplyr::group_by_prepare(.data, ..., .add = .add)
      Execution halted
    ```

# weathercan

<details>

* Version: 0.3.3
* Source code: https://github.com/cran/weathercan
* URL: https://docs.ropensci.org/weathercan, https://github.com/ropensci/weathercan
* BugReports: https://github.com/ropensci/weathercan/issues
* Date/Publication: 2020-02-05 14:10:02 UTC
* Number of recursive dependencies: 128

Run `revdep_details(,"weathercan")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 477 | SKIPPED: 17 | WARNINGS: 17 | FAILED: 19 ]
      1. Failure: weather (hour) returns a data frame (@test_06_weather_dl.R#25) 
      2. Failure: weather (hour) formats timezone display (@test_06_weather_dl.R#58) 
      3. Failure: weather (hour) formats NL timezone 
      4. Failure: weather (hour) multiple stations (@test_06_weather_dl.R#86) 
      5. Failure: weather (hour) formats time_disp with multiple stations (@test_06_weather_dl.R#101) 
      6. Failure: weather (hour) formats time_disp with multiple stations (@test_06_weather_dl.R#108) 
      7. Failure: weather (hour) gets all (@test_06_weather_dl.R#122) 
      8. Failure: weather (hour) verbose and quiet (@test_06_weather_dl.R#198) 
      9. Failure: weather (day) gets all, one year (@test_06_weather_dl.R#288) 
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
      [ OK: 416 | SKIPPED: 6 | WARNINGS: 12 | FAILED: 10 ]
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
      
      ── 3. Error: non-overlapped rules are unchanged  ───────────────────────────────
      Column `dimension.x` not found in `.data`
      Backtrace:
        1. xrf:::xrf_deoverlap_rules(rules)
       27. rlang:::abort_data_pronoun(x)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 25 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 3 ]
      1. Error: single dimnension overlapped rules are deoverlapped 
      2. Error: multi dimension overlapped rules are deoverlapped 
      3. Error: non-overlapped rules are unchanged 
      
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

