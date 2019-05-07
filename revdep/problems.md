# benthos

<details>

* Version: 1.3-6
* Source code: https://github.com/cran/benthos
* Date/Publication: 2019-03-17 22:43:20 UTC
* Number of recursive dependencies: 56

Run `revdep_details(,"benthos")` for more info

</details>

## Newly broken

*   checking whether package ‘benthos’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘dplyr::as_name’ by ‘lazyeval::as_name’ when loading ‘benthos’
    See ‘/Users/romainfrancois/git/dplyr-revdep/dplyr/revdep/checks.noindex/benthos/new/benthos.Rcheck/00install.out’ for details.
    ```

# bomrang

<details>

* Version: 0.5.0
* Source code: https://github.com/cran/bomrang
* URL: https://github.com/ropensci/bomrang, https://ropensci.github.io/bomrang/
* BugReports: https://github.com/ropensci/bomrang/issues
* Date/Publication: 2019-03-21 12:13:23 UTC
* Number of recursive dependencies: 104

Run `revdep_details(,"bomrang")` for more info

</details>

## Newly broken

*   checking whether package ‘bomrang’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘dplyr:::=’ by ‘data.table:::=’ when loading ‘bomrang’
    See ‘/Users/romainfrancois/git/dplyr-revdep/dplyr/revdep/checks.noindex/bomrang/new/bomrang.Rcheck/00install.out’ for details.
    ```

# bupaR

<details>

* Version: 0.4.2
* Source code: https://github.com/cran/bupaR
* URL: https://www.bupar.net
* Date/Publication: 2019-02-19 11:40:03 UTC
* Number of recursive dependencies: 38

Run `revdep_details(,"bupaR")` for more info

</details>

## Newly broken

*   checking whether package ‘bupaR’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘dplyr:::=’ by ‘data.table:::=’ when loading ‘bupaR’
    See ‘/Users/romainfrancois/git/dplyr-revdep/dplyr/revdep/checks.noindex/bupaR/new/bupaR.Rcheck/00install.out’ for details.
    ```

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

# edeaR

<details>

* Version: 0.8.2
* Source code: https://github.com/cran/edeaR
* URL: https://www.bupar.net
* BugReports: https://github.com/gertjanssenswillen/edeaR/issues
* Date/Publication: 2019-02-22 14:40:13 UTC
* Number of recursive dependencies: 68

Run `revdep_details(,"edeaR")` for more info

</details>

## Newly broken

*   checking whether package ‘edeaR’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘dplyr:::=’ by ‘data.table:::=’ when loading ‘edeaR’
    See ‘/Users/romainfrancois/git/dplyr-revdep/dplyr/revdep/checks.noindex/edeaR/new/edeaR.Rcheck/00install.out’ for details.
    ```

# heuristicsmineR

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/heuristicsmineR
* URL: https://github.com/fmannhardt/heuristicsmineR
* BugReports: https://github.com/fmannhardt/heuristicsmineR/issues
* Date/Publication: 2019-05-02 08:20:04 UTC
* Number of recursive dependencies: 92

Run `revdep_details(,"heuristicsmineR")` for more info

</details>

## Newly broken

*   checking whether package ‘heuristicsmineR’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘dplyr:::=’ by ‘data.table:::=’ when loading ‘heuristicsmineR’
    See ‘/Users/romainfrancois/git/dplyr-revdep/dplyr/revdep/checks.noindex/heuristicsmineR/new/heuristicsmineR.Rcheck/00install.out’ for details.
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘Rcpp11’ ‘magrittr’
      All declared Imports should be used.
    ```

# IAT

<details>

* Version: 0.3
* Source code: https://github.com/cran/IAT
* Date/Publication: 2016-04-30 00:51:43
* Number of recursive dependencies: 38

Run `revdep_details(,"IAT")` for more info

</details>

## Newly broken

*   checking whether package ‘IAT’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘dplyr::as_name’ by ‘lazyeval::as_name’ when loading ‘IAT’
    See ‘/Users/romainfrancois/git/dplyr-revdep/dplyr/revdep/checks.noindex/IAT/new/IAT.Rcheck/00install.out’ for details.
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

# INDperform

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/INDperform
* URL: https://github.com/saskiaotto/INDperform
* BugReports: https://github.com/SaskiaAOtto/INDperform/issues
* Date/Publication: 2019-02-10 03:53:24 UTC
* Number of recursive dependencies: 70

Run `revdep_details(,"INDperform")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  5.0Mb
      sub-directories of 1Mb or more:
        data   3.5Mb
        help   1.1Mb
    ```

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      The conditions set in the crit_scores table for sub-criterion
      C9_1
      are not unique, i.e. conditions are met multiple times!
      Please correct your crit_score table before you continue.  variable required_data_type
      1      edf            numeric
        variable required_data_type
      1     r_sq            numeric
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 618 SKIPPED: 0 WARNINGS: 12 FAILED: 3
      1. Failure: check gams under different distributions (@test_calc_deriv.R#82) 
      2. Failure: test sample_boot (@test_cond_boot.R#69) 
      3. Failure: test sample_boot (@test_cond_boot.R#70) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘lazyeval’
      All declared Imports should be used.
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

# lans2r

<details>

* Version: 1.0.5
* Source code: https://github.com/cran/lans2r
* URL: https://github.com/KopfLab/lans2r
* BugReports: https://github.com/KopfLab/lans2r/issues
* Date/Publication: 2017-05-24 04:25:53 UTC
* Number of recursive dependencies: 64

Run `revdep_details(,"lans2r")` for more info

</details>

## Newly broken

*   checking whether package ‘lans2r’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘dplyr::as_name’ by ‘lazyeval::as_name’ when loading ‘lans2r’
    See ‘/Users/romainfrancois/git/dplyr-revdep/dplyr/revdep/checks.noindex/lans2r/new/lans2r.Rcheck/00install.out’ for details.
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

# MSstatsTMT

<details>

* Version: 1.1.2
* Source code: https://github.com/cran/MSstatsTMT
* URL: http://msstats.org/msstatstmt/
* BugReports: https://groups.google.com/forum/#!forum/msstats
* Date/Publication: 2019-02-27
* Number of recursive dependencies: 82

Run `revdep_details(,"MSstatsTMT")` for more info

</details>

## Newly broken

*   checking whether package ‘MSstatsTMT’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘dplyr:::=’ by ‘data.table:::=’ when loading ‘MSstatsTMT’
    See ‘/Users/romainfrancois/git/dplyr-revdep/dplyr/revdep/checks.noindex/MSstatsTMT/new/MSstatsTMT.Rcheck/00install.out’ for details.
    ```

# networkreporting

<details>

* Version: 0.1.1
* Source code: https://github.com/cran/networkreporting
* Date/Publication: 2016-12-05 18:28:47
* Number of recursive dependencies: 50

Run `revdep_details(,"networkreporting")` for more info

</details>

## Newly broken

*   checking whether package ‘networkreporting’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘dplyr::as_name’ by ‘lazyeval::as_name’ when loading ‘networkreporting’
    See ‘/Users/romainfrancois/git/dplyr-revdep/dplyr/revdep/checks.noindex/networkreporting/new/networkreporting.Rcheck/00install.out’ for details.
    ```

# nonmemica

<details>

* Version: 0.9.0
* Source code: https://github.com/cran/nonmemica
* Date/Publication: 2019-04-25 12:10:02 UTC
* Number of recursive dependencies: 59

Run `revdep_details(,"nonmemica")` for more info

</details>

## Newly broken

*   checking whether package ‘nonmemica’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘dplyr::as_name’ by ‘lazyeval::as_name’ when loading ‘nonmemica’
    See ‘/Users/romainfrancois/git/dplyr-revdep/dplyr/revdep/checks.noindex/nonmemica/new/nonmemica.Rcheck/00install.out’ for details.
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
    

# processmapR

<details>

* Version: 0.3.3
* Source code: https://github.com/cran/processmapR
* URL: https://www.bupar.net
* Date/Publication: 2019-02-24 18:00:15 UTC
* Number of recursive dependencies: 92

Run `revdep_details(,"processmapR")` for more info

</details>

## Newly broken

*   checking whether package ‘processmapR’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘dplyr:::=’ by ‘data.table:::=’ when loading ‘processmapR’
    See ‘/Users/romainfrancois/git/dplyr-revdep/dplyr/revdep/checks.noindex/processmapR/new/processmapR.Rcheck/00install.out’ for details.
    ```

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

# seplyr

<details>

* Version: 0.8.3
* Source code: https://github.com/cran/seplyr
* URL: https://github.com/WinVector/seplyr/, https://winvector.github.io/seplyr/
* BugReports: https://github.com/WinVector/seplyr/issues
* Date/Publication: 2019-01-02 23:10:09 UTC
* Number of recursive dependencies: 38

Run `revdep_details(,"seplyr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    >  suppressPackageStartupMessages(library("dplyr"))
    >  # Example: clear one of a or b in any row where both are set.
    >  d <- data.frame(a = c(0, 0, 1, 1, 1, 1, 1, 1, 1, 1),
    +                  b = c(0, 1, 0, 1, 1, 1, 1, 1, 1, 1),
    +                  edited = FALSE)
    > 
    >  program <- if_else_device(
    +    testexpr = '(a+b)>1',
    +    thenexprs = c(
    +      if_else_device(
    +        testexpr = 'runif(n()) >= 0.5',
    +        thenexprs = 'a' := '0',
    +        elseexprs = 'b' := '0'),
    +      'edited' := 'TRUE'))
    Error: `:=` can only be used within a quasiquoted argument
    Backtrace:
        █
     1. ├─seplyr::if_else_device(...)
     2. ├─seplyr::if_else_device(...)
     3. └─`:=`("a", "0")
    Execution halted
    ```

# socviz

<details>

* Version: 1.0.0
* Source code: https://github.com/cran/socviz
* URL: https://github.com/kjhealy/socviz
* BugReports: https://github.com/kjhealy/socviz/issues
* Date/Publication: 2019-04-23 12:00:03 UTC
* Number of recursive dependencies: 39

Run `revdep_details(,"socviz")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        data   4.5Mb
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

# summariser

<details>

* Version: 0.1.0
* Source code: https://github.com/cran/summariser
* URL: https://github.com/condwanaland/summariser
* Date/Publication: 2017-03-23 13:10:18 UTC
* Number of recursive dependencies: 41

Run `revdep_details(,"summariser")` for more info

</details>

## Newly broken

*   checking whether package ‘summariser’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘dplyr::as_name’ by ‘lazyeval::as_name’ when loading ‘summariser’
    See ‘/Users/romainfrancois/git/dplyr-revdep/dplyr/revdep/checks.noindex/summariser/new/summariser.Rcheck/00install.out’ for details.
    ```

# tbl2xts

<details>

* Version: 0.1.2
* Source code: https://github.com/cran/tbl2xts
* Date/Publication: 2017-08-02 13:50:58 UTC
* Number of recursive dependencies: 39

Run `revdep_details(,"tbl2xts")` for more info

</details>

## Newly broken

*   checking whether package ‘tbl2xts’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘dplyr::as_name’ by ‘lazyeval::as_name’ when loading ‘tbl2xts’
    See ‘/Users/romainfrancois/git/dplyr-revdep/dplyr/revdep/checks.noindex/tbl2xts/new/tbl2xts.Rcheck/00install.out’ for details.
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘PerformanceAnalytics’
      All declared Imports should be used.
    ```

# TPP

<details>

* Version: 3.10.1
* Source code: https://github.com/cran/TPP
* Date/Publication: 2019-01-04
* Number of recursive dependencies: 83

Run `revdep_details(,"TPP")` for more info

</details>

## Newly broken

*   checking whether package ‘TPP’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘data.table:::=’ by ‘dplyr:::=’ when loading ‘TPP’
    See ‘/Users/romainfrancois/git/dplyr-revdep/dplyr/revdep/checks.noindex/TPP/new/TPP.Rcheck/00install.out’ for details.
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

# xesreadR

<details>

* Version: 0.2.3
* Source code: https://github.com/cran/xesreadR
* URL: http://www.bupar.net
* Date/Publication: 2019-03-19 12:50:03 UTC
* Number of recursive dependencies: 49

Run `revdep_details(,"xesreadR")` for more info

</details>

## Newly broken

*   checking whether package ‘xesreadR’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘dplyr:::=’ by ‘data.table:::=’ when loading ‘xesreadR’
    See ‘/Users/romainfrancois/git/dplyr-revdep/dplyr/revdep/checks.noindex/xesreadR/new/xesreadR.Rcheck/00install.out’ for details.
    ```

