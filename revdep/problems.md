# Cardinal

<details>

* Version: 2.0.4
* Source code: https://github.com/cran/Cardinal
* URL: http://www.cardinalmsi.org
* Date/Publication: 2019-02-21
* Number of recursive dependencies: 68

Run `revdep_details(,"Cardinal")` for more info

</details>

## Newly broken

*   checking R code for possible problems ... NOTE
    ```
    group_by: no visible global function definition for
      ‘group_by_drop_default’
    Undefined global functions or variables:
      group_by_drop_default
    ```

## In both

*   checking for missing documentation entries ... WARNING
    ```
    Undocumented code objects:
      ‘filter’
    All user-level objects in a package should have documentation entries.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.0Mb
      sub-directories of 1Mb or more:
        R     3.1Mb
        doc   2.2Mb
    ```

# CluMP

<details>

* Version: 0.7.1
* Source code: https://github.com/cran/CluMP
* URL: https://arxiv.org/ftp/arxiv/papers/1807/1807.05926.pdf
* Date/Publication: 2019-04-05 09:42:55 UTC
* Number of recursive dependencies: 77

Run `revdep_details(,"CluMP")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > 
    > set.seed(123)
    > dataMale <- GeneratePanel(n = 50, Param = ParamLinear, NbVisit = 10)
    > dataMale$Gender <- "M"
    > dataFemale <- GeneratePanel(n = 50, Param = ParamLinear, NbVisit = 10)
    > dataFemale$ID <- dataFemale$ID + 50
    > dataFemale$Gender <- "F"
    > data <- rbind(dataMale, dataFemale)
    > 
    > CLUMP3 <- CluMP(formula = Y ~ Time, group = "ID", data = data, cl_numb = 3)
    > CluMP_profiles(CLUMP3, cat_vars = "Gender")
                    Stratified by memb_CluMP
                     1          2          3          p      test
      n              46         20         34                    
      Gender = M (%) 22 (47.8)  10 (50.0)  18 (52.9)   0.903     
                    Stratified by memb_CluMP
                     1          2          3          p      test
      n              46         20         34                    
      Gender = M (%) 22 (47.8)  10 (50.0)  18 (52.9)   0.903     
    Error: Column `mean_triangle_fn1` is of unsupported type function
    Execution halted
    ```

# compareDF

<details>

* Version: 1.7.2
* Source code: https://github.com/cran/compareDF
* Date/Publication: 2019-04-02 10:30:03 UTC
* Number of recursive dependencies: 39

Run `revdep_details(,"compareDF")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(testthat)
      > library(compareDF)
      > 
      > test_check("compareDF")
      ── 1. Failure: (unknown) (@test-fnsComparison.R#369)  ──────────────────────────
      `expected_change_count` not equivalent to actual_comparison_summary$change_count.
      Incompatible type for column `changes`: x numeric, y integer
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 48 SKIPPED: 0 WARNINGS: 0 FAILED: 1
      1. Failure: (unknown) (@test-fnsComparison.R#369) 
      
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

# idealstan

<details>

* Version: 0.7.1
* Source code: https://github.com/cran/idealstan
* BugReports: https://github.com/saudiwin/idealstan/issues
* Date/Publication: 2019-02-19 21:40:09 UTC
* Number of recursive dependencies: 103

Run `revdep_details(,"idealstan")` for more info

</details>

## Newly broken

*   checking whether package ‘idealstan’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘dplyr::as_name’ by ‘lazyeval::as_name’ when loading ‘idealstan’
    See ‘/Users/romainfrancois/git/dplyr-revdep/dplyr/revdep/checks.noindex/idealstan/new/idealstan.Rcheck/00install.out’ for details.
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.1Mb
      sub-directories of 1Mb or more:
        libs   5.0Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# InjurySeverityScore

<details>

* Version: 0.0.0.1
* Source code: https://github.com/cran/InjurySeverityScore
* Date/Publication: 2018-03-28 08:26:07 UTC
* Number of recursive dependencies: 20

Run `revdep_details(,"InjurySeverityScore")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### Aliases: injury_score
    > 
    > ### ** Examples
    > 
    > pat_id <- c(2,2,2,2,2,1,2,1,2,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1)
    > icd9 <- c('874.2', '874.8', '900.81', '900.82', '900.89', '805.06', 
    +           'E966', '805.07', 'V14.0', '807.02', 'V70.4', '821.01', '823.20', 
    +           '860.0', '861.01', '861.21', '861.22', '863.84', '864.04', '865.04', 
    +           '865.09', '866.02', '868.04', '958.4')
    > sample_data <- data.frame(subj = pat_id, code = icd9, stringsAsFactors = FALSE)
    > injury_score(sample_data, subj, code)
    Error: Argument 3 must be an integer vector, not a double vector
    Backtrace:
        █
     1. └─InjurySeverityScore::injury_score(sample_data, subj, code)
     2.   ├─base::cbind(...)
     3.   └─dplyr::coalesce(iss_br$max_wo_9, iss_br$max_w_9, iss_br$severity_default)
     4.     └─dplyr:::replace_with(...)
     5.       └─dplyr:::check_type(val, x, name)
     6.         └─dplyr:::glubort(header, "must be {friendly_type_of(template)}, not {friendly_type_of(x)}")
    Execution halted
    ```

# MonetDBLite

<details>

* Version: 0.6.0
* Source code: https://github.com/cran/MonetDBLite
* URL: https://github.com/hannesmuehleisen/MonetDBLite-R
* BugReports: https://github.com/hannesmuehleisen/MonetDBLite-R/issues
* Date/Publication: 2018-07-27 09:40:03 UTC
* Number of recursive dependencies: 73

Run `revdep_details(,"MonetDBLite")` for more info

</details>

## Newly broken

*   R CMD check timed out
    

## Newly fixed

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      round(confint(a, df = degf(rclus1))[2, 2], 2) not equal to 30808.26.
      1/1 mismatches
      [1] 33252 - 30808 == 2443
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 292 SKIPPED: 13 WARNINGS: 10 FAILED: 6
      1. Error: sample works (@test_04_dplyr.R#41) 
      2. Failure: db allows svyby commands (@test_05_survey.R#60) 
      3. Failure: db allows svyby commands (@test_05_survey.R#62) 
      4. Failure: db allows svyby commands (@test_05_survey.R#66) 
      5. Failure: db allows svyby commands (@test_05_survey.R#67) 
      6. Failure: db allows svyby commands (@test_05_survey.R#69) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.8Mb
      sub-directories of 1Mb or more:
        libs   5.4Mb
    ```

# perturbatr

<details>

* Version: 1.2.1
* Source code: https://github.com/cran/perturbatr
* URL: https://github.com/cbg-ethz/perturbatr
* BugReports: https://github.com/cbg-ethz/perturbatr/issues
* Date/Publication: 2019-01-04
* Number of recursive dependencies: 78

Run `revdep_details(,"perturbatr")` for more info

</details>

## Newly broken

*   R CMD check timed out
    

# ruler

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/ruler
* URL: https://echasnovski.github.io/ruler/, https://github.com/echasnovski/ruler
* BugReports: https://github.com/echasnovski/ruler/issues
* Date/Publication: 2019-02-15 21:00:03 UTC
* Number of recursive dependencies: 46

Run `revdep_details(,"ruler")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
     1. dplyr::transmute_at(., c("disp", "qsec"), rules(z_score = abs(. -     mean(.))/sd(.) > 1))
    
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
    Error: Column `._.improper_is_neg` must be length 2 (the number of rows) or one, not 22
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      37: is.data.frame(x)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 278 SKIPPED: 1 WARNINGS: 3 FAILED: 8
      1. Error: expose works (@test-expose.R#188) 
      2. Error: expose preserves pack names (@test-expose.R#246) 
      3. Error: expose accounts for rule separator (@test-expose.R#264) 
      4. Error: expose guesses (@test-expose.R#271) 
      5. Error: expose_single.default guesses col pack (@test-expose.R#309) 
      6. Error: expose_single.default guesses cell pack (@test-expose.R#335) 
      7. Error: expose_single.col_pack works (@test-expose.R#402) 
      8. Error: expose_single.cell_pack works (@test-expose.R#453) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# spdplyr

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/spdplyr
* URL: https://github.com/mdsumner/spdplyr
* BugReports: https://github.com/mdsumner/spdplyr/issues
* Date/Publication: 2019-02-04 09:50:02 UTC
* Number of recursive dependencies: 50

Run `revdep_details(,"spdplyr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    + spmap %>% mutate_all(funs(as.character))
    + spmap %>% mutate_at(vars(starts_with("L")), funs(as.integer))
    + }
    Error: `data` must be a vector, list, data frame, or environment
    Backtrace:
         █
      1. └─spmap %>% mutate_if(is.numeric, as.character)
      2.   ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      3.   └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      4.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5.       └─global::`_fseq`(`_lhs`)
      6.         └─magrittr::freduce(value, `_function_list`)
      7.           ├─base::withVisible(function_list[[k]](value))
      8.           └─function_list[[k]](value)
      9.             └─dplyr::mutate_if(., is.numeric, as.character)
     10.               └─dplyr:::manip_if(...)
     11.                 └─dplyr:::tbl_if_syms(.tbl, .predicate, .env, .include_group_vars = .include_group_vars)
     12.                   ├─rlang::syms(tbl_if_vars(.tbl, .p, .env, ..., .include_group_vars = .include_group_vars))
     13.                   │ └─rlang:::map(x, sym)
     14.                   │   └─base::lapply(.x,
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      10: manip_if(.tbl, .predicate, .funs, enquo(.funs), caller_env(), ...)
      11: tbl_if_syms(.tbl, .predicate, .env, .include_group_vars = .include_group_vars)
      12: syms(tbl_if_vars(.tbl, .p, .env, ..., .include_group_vars = .include_group_vars))
      13: map(x, sym)
      14: lapply(.x, .f, ...)
      15: tbl_if_vars(.tbl, .p, .env, ..., .include_group_vars = .include_group_vars)
      16: as_data_mask(.tbl)
      17: rlang::abort(x)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 46 SKIPPED: 4 WARNINGS: 2 FAILED: 1
      1. Error: mutate_all, mutate_at (@test-adv-dplyr.R#83) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# strapgod

<details>

* Version: 0.0.1
* Source code: https://github.com/cran/strapgod
* URL: https://github.com/DavisVaughan/strapgod
* BugReports: https://github.com/DavisVaughan/strapgod/issues
* Date/Publication: 2019-03-16 14:00:03 UTC
* Number of recursive dependencies: 52

Run `revdep_details(,"strapgod")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      nrow(x_gm) not equal to 5.
      target is NULL, current is numeric
      
      ── 3. Failure: group_map() (@test-dplyr-group-funs.R#52)  ──────────────────────
      x_gm$.g[[1]] not equal to dplyr::tibble(.bootstrap = 1L).
      target is NULL, current is tbl_df
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 146 SKIPPED: 0 WARNINGS: 0 FAILED: 3
      1. Failure: group_map() (@test-dplyr-group-funs.R#43) 
      2. Failure: group_map() (@test-dplyr-group-funs.R#50) 
      3. Failure: group_map() (@test-dplyr-group-funs.R#52) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

