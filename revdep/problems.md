# AlphaBeta

<details>

* Version: 1.2.0
* Source code: https://github.com/cran/AlphaBeta
* Date/Publication: 2020-04-27
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

## In both

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

# amt

<details>

* Version: 0.1.1
* Source code: https://github.com/cran/amt
* URL: https://github.com/jmsigner/amt
* Date/Publication: 2020-04-28 12:10:02 UTC
* Number of recursive dependencies: 167

Run `revdep_details(,"amt")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/tinytest.R’ failed.
    Last 13 lines of output:
      Running test_track.R..................   56 tests [0;32mOK[0m 
      Running test_track.R..................   57 tests [0;32mOK[0m 
      Running test_track.R..................   58 tests [0;32mOK[0m 
      Running test_track.R..................   59 tests [0;32mOK[0m 
      Running test_track.R..................   60 tests [0;32mOK[0m 
      Running test_track.R..................   61 tests [0;32mOK[0m 
      Running test_track.R..................   62 tests [0;32mOK[0m 
      Running test_track.R..................   63 tests [0;32mOK[0m 
      Error: ----- FAILED[data]: test_random_steps.R<89--89>
       call| expect_equal(rs[1, 1:6], rs[2, 1:6])
       diff| Component "x2_": Names: 1 string mismatch
       diff| Component "y2_": Names: 1 string mismatch
       diff| Component "sl_": Names: 1 string mismatch
       diff| Component "ta_": Names: 1 string mismatch 
      Execution halted
    ```

# BMSC

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/BMSC
* Date/Publication: 2019-04-16 15:25:42 UTC
* Number of recursive dependencies: 85

Run `revdep_details(,"BMSC")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── 2. Error: predict inserts NA values at correct positions (not all variables i
      argument is of length zero
      Backtrace:
       1. testthat::expect_true(...)
       4. dplyr::all_equal(...)
       5. dplyr:::equal_data_frame(...)
       6. dplyr:::is_compatible_data_frame(...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 57 | SKIPPED: 1 | WARNINGS: 0 | FAILED: 2 ]
      1. Error: predict inserts NA values at correct positions (@test-predict.R#58) 
      2. Error: predict inserts NA values at correct positions (not all variables in model) (@test-predict.R#74) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.7Mb
      sub-directories of 1Mb or more:
        libs   6.2Mb
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

# circRNAprofiler

<details>

* Version: 1.2.0
* Source code: https://github.com/cran/circRNAprofiler
* URL: https://github.com/Aufiero/circRNAprofiler
* BugReports: https://github.com/Aufiero/circRNAprofiler/issues
* Date/Publication: 2020-04-27
* Number of recursive dependencies: 214

Run `revdep_details(,"circRNAprofiler")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    +     width = 6,
    +     species = "Hsapiens",
    +     rbp = TRUE,
    +     reverse = FALSE)
    trying URL 'https://attract.cnic.es/attract/static/ATtRACT.zip'
    downloaded 113 KB
    
    motifs.txt is empty or absent. Only
                ATtRACT motifs will be analyzedError: Can't combine `..1$id` <logical> and `..2$id` <character>.
    Backtrace:
        █
     1. ├─circRNAprofiler::getMotifs(...)
     2. │ └─circRNAprofiler:::.filterMotifs(...)
     3. │   └─circRNAprofiler:::.getUserAttractMotifs(species, reverse, pathToMotifs)
     4. │     └─dplyr::bind_rows(newMotifsFromFile[, c(1, 2)], newAttractRBPmotifs)
     5. │       └─vctrs::vec_rbind(!!!dots, .names_to = .id)
     6. └─vctrs::vec_default_ptype2(...)
     7.   └─vctrs::stop_incompatible_type(...)
     8.     └─vctrs:::stop_incompatible(...)
     9.       └─vctrs:::stop_vctrs(...)
    Execution halted
    ```

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      1. Error: annotateBSJs() generates the correct data structure (@test_annotateBSJs.R#10) 
      2. Error: annotateBSJs() generates a data frame with the correct content (@test_annotateBSJs.R#33) 
      3. Error: annotateBSJs() generates the correct data structure
          when using random back-spliced junctions (@test_annotateBSJs.R#80) 
      4. Error: annotateBSJs() generates a data frame with the correct content when using
          random back-spliced junctions (@test_annotateBSJs.R#102) 
      5. Error: annotateRepeats() generates the correct data structure (@test_annotateRepeats.R#8) 
      6. Error: filterCirc() filters the data frame containing circRNA counts (@test_filterBSJs.R#13) 
      7. Error: getBackSplicedJunctions() generates the correct data structure (@test_getBackSplicedJunctions.R#14) 
      8. Error: mergeBSJunctions() generates a data frame with the correct content (@test_getBackSplicedJunctions.R#43) 
      9. Error: getCircSeqs() generate the correct data structure (@test_getCircSeqs.R#9) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      'BSgenome.Mmusculus.UCSC.mm9', 'BSgenome.Mmusculus.UCSC.mm10'
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

# cvms

<details>

* Version: 1.0.1
* Source code: https://github.com/cran/cvms
* URL: https://github.com/ludvigolsen/cvms
* BugReports: https://github.com/ludvigolsen/cvms/issues
* Date/Publication: 2020-04-19 09:30:02 UTC
* Number of recursive dependencies: 120

Run `revdep_details(,"cvms")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 3757 | SKIPPED: 61 | WARNINGS: 5 | FAILED: 12 ]
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

* Version: 1.24.0
* Source code: https://github.com/cran/DEGreport
* URL: http://lpantano.github.io/DEGreport/
* BugReports: https://github.com/lpantano/DEGreport/issues
* Date/Publication: 2020-04-27
* Number of recursive dependencies: 132

Run `revdep_details(,"DEGreport")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Error: Problem with `summarise()` input `n_genes`.
    ✖ could not find function "n"
    ℹ Input `n_genes` is `n()`.
    ℹ The error occured in group 1: merge = "aFemale", cluster = 1, group = "Female", other = "a".
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
     10.               └─dplyr:::summarise.grouped_df(., abundance = median(value), n_genes = n())
     11.                 └─dplyr:::summarise_cols(.data, ...)
    <parent: error/simpleError>
    Backtrace:
    █
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ✖ could not find function "n"
      ℹ Input `n_genes` is `n()`.
      ℹ The error occured in group 1: merge = "aFemale", cluster = 1, group = "Female", other = "a".
      Backtrace:
        1. DEGreport::degPatterns(...)
        2. dplyr::group_by(...)
        9. dplyr::summarise(., abundance = median(value), n_genes = n())
       11. dplyr:::summarise_cols(.data, ...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 50 | SKIPPED: 0 | WARNINGS: 2 | FAILED: 1 ]
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

# disk.frame

<details>

* Version: 0.3.5
* Source code: https://github.com/cran/disk.frame
* URL: https://diskframe.com
* BugReports: https://github.com/xiaodaigh/disk.frame/issues
* Date/Publication: 2020-05-08 13:10:10 UTC
* Number of recursive dependencies: 105

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

*   checking S3 generic/method consistency ... WARNING
    ```
    pull:
      function(.data, var, name, ...)
    pull.disk.frame:
      function(.data, var)
    
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

# DLMtool

<details>

* Version: 5.4.3
* Source code: https://github.com/cran/DLMtool
* URL: http://www.datalimitedtoolkit.org/
* BugReports: https://github.com/DLMtool/DLMtool/issues
* Date/Publication: 2020-04-16 21:20:10 UTC
* Number of recursive dependencies: 128

Run `revdep_details(,"DLMtool")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > 
    > DCACs(1, DLMtool::Atlantic_mackerel, plot=TRUE) 
    TAC (median) 
        5.885724 
    > 
    > DCAC(1, DLMtool::Atlantic_mackerel, plot=TRUE) 
    TAC (median) 
        5.299291 
    > 
    > DCAC_40(1, DLMtool::Atlantic_mackerel, plot=TRUE) 
    TAC (median) 
        6.860386 
    > 
    > Data <- DLMtool::Atlantic_mackerel
    > Data@LHYear <- 2005
    > DCAC4010(1, Data, plot=TRUE) 
    TAC (median) 
        1.576309 
    > 
    > DCAC_ML(1, DLMtool::SimulatedData, plot=TRUE) 
    libc++abi.dylib: __cxa_guard_acquire detected deadlock
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        R      2.0Mb
        data   2.1Mb
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

*   checking tests ...
    ```
     ERROR
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

* Version: 1.6.0
* Source code: https://github.com/cran/DuoClustering2018
* Date/Publication: 2020-05-07
* Number of recursive dependencies: 158

Run `revdep_details(,"DuoClustering2018")` for more info

</details>

## Newly broken

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

## Newly fixed

*   checking whether package ‘DuoClustering2018’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/DuoClustering2018/old/DuoClustering2018.Rcheck/00install.out’ for details.
    ```

# easyr

<details>

* Version: 0.3-1
* Source code: https://github.com/cran/easyr
* URL: https://github.com/oliver-wyman-actuarial/easyr
* BugReports: https://github.com/oliver-wyman-actuarial/easyr/issues
* Date/Publication: 2020-03-20 18:10:05 UTC
* Number of recursive dependencies: 92

Run `revdep_details(,"easyr")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      trying URL 'https://cloud.r-project.org/bin/macosx/contrib/4.0/plyr_1.8.6.tgz'
      Content type 'application/x-gzip' length 1010994 bytes (987 KB)
      ==================================================
      downloaded 987 KB
      
      
      The downloaded binary packages are in
      	/var/folders/4b/hn4fq98s6810s4ccv2f9hm2h0000gn/T//RtmpZ3Qqhi/downloaded_packages
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
* Number of recursive dependencies: 108

Run `revdep_details(,"eda4treeR")` for more info

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

Run `revdep_details(,"egor")` for more info

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

*   checking tests ...
    ```
     ERROR
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
      ── 2. Error: (unknown) (@test-plot.R#7)  ───────────────────────────────────────
      Can't subset columns that don't exist.
      ✖ Column `shut.up` doesn't exist.
      Backtrace:
        1. base::plot(fssm, what = "fitted")
       31. vctrs:::stop_subscript_oob(...)
       32. vctrs:::stop_subscript(...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 30 | SKIPPED: 14 | WARNINGS: 1 | FAILED: 2 ]
      1. Error: (unknown) (@test-join.R#7) 
      2. Error: (unknown) (@test-plot.R#7) 
      
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

# geometr

<details>

* Version: 0.2.4
* Source code: https://github.com/cran/geometr
* URL: https://github.com/EhrmannS/geometr
* BugReports: https://github.com/EhrmannS/geometr/issues
* Date/Publication: 2020-03-30 10:20:02 UTC
* Number of recursive dependencies: 86

Run `revdep_details(,"geometr")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Simple feature collection with 100 features and 14 fields
      geometry type:  MULTIPOLYGON
      dimension:      XY
      bbox:           xmin: -84.32385 ymin: 33.88199 xmax: -75.45698 ymax: 36.58965
      CRS:            4267
      ── 1. Failure: output the history of a plotted object (@test_visualise.R#104)  ─
      Check on output isn't true.
      Must inherit from class 'simpleMessage', but has classes 'dplyr_regroup','condition'
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 731 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Failure: output the history of a plotted object (@test_visualise.R#104) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘readr’
      All declared Imports should be used.
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
    ℹ Input `wt` is `n()`.
    ℹ The error occured in group 1: from_id = "A-", to_id = "A+".
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

# getTBinR

<details>

* Version: 0.7.0
* Source code: https://github.com/cran/getTBinR
* URL: https://www.samabbott.co.uk/getTBinR, https://github.com/seabbs/getTBinR
* BugReports: https://github.com/seabbs/getTBinR/issues
* Date/Publication: 2019-09-03 13:50:06 UTC
* Number of recursive dependencies: 149

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
      [ OK: 697 | SKIPPED: 74 | WARNINGS: 0 | FAILED: 4 ]
      1. Error: derivatives() returns derivatives for all smooths in a factor by GAM (@test-derivatives.R#183) 
      2. Error: derivatives() returns derivatives for all smooths in a factor by GAM (@test-derivatives.R#225) 
      3. Error: derivatives() works for factor by smooths issue 47 (@test-derivatives.R#339) 
      4. Error: derivatives() works for fs smooths issue 57 (@test-derivatives.R#389) 
      
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

# healthcareai

<details>

* Version: 2.4.0
* Source code: https://github.com/cran/healthcareai
* URL: http://docs.healthcare.ai
* BugReports: https://github.com/HealthCatalyst/healthcareai-r/issues
* Date/Publication: 2020-02-28 18:00:05 UTC
* Number of recursive dependencies: 119

Run `revdep_details(,"healthcareai")` for more info

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

# isomiRs

<details>

* Version: 1.16.1
* Source code: https://github.com/cran/isomiRs
* BugReports: https://github.com/lpantano/isomiRs/issues
* Date/Publication: 2020-05-26
* Number of recursive dependencies: 148

Run `revdep_details(,"isomiRs")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ℹ The error occured in group 1: merge = "one_groupcontrol_1", cluster = 1, xaxis = "control_1", colored = "one_group".
      Backtrace:
        1. testthat::expect_equal(unique(.cluster_exp(gene)), c(1, 2))
        5. isomiRs:::.cluster_exp(gene)
        6. DEGreport::degPatterns(ma, meta, time = "xaxis", minc = 0, plot = FALSE)
        4. dplyr::group_by(...)
       11. dplyr::summarise(., abundance = median(value), n_genes = n())
       16. dplyr:::summarise_cols(.data, ...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 27 | SKIPPED: 0 | WARNINGS: 2 | FAILED: 1 ]
      1. Error: matrix (@test_calculus.R#23) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking examples ... ERROR
    ```
    ...
    Joining, by = "merge"
    Error: Problem with `summarise()` input `n_genes`.
    ✖ could not find function "n"
    ℹ Input `n_genes` is `n()`.
    ℹ The error occured in group 1: merge = "one_groupcontrol", cluster = 1, xaxis = "control", colored = "one_group".
    Backtrace:
         █
      1. └─isomiRs::isoNetwork(...)
      2.   └─isomiRs:::.viz_mirna_gene_enrichment(...)
      3.     └─isomiRs:::.cluster_exp(...)
      4.       └─DEGreport::degPatterns(ma, meta, time = "xaxis", minc = 0, plot = FALSE)
      5.         └─`%>%`(...)
      6.           ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      7.           └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      8.             └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      9.               └─DEGreport:::`_fseq`(`_lhs`)
     10.                 └─magrittr::freduce(value, `_function_list`)
     11.                   └─function_list[[i]](value)
     12.                     ├─dplyr::summarise(., abundance = median(value), n_genes = n())
     13.       
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
      as.tibble changes hits iso_sample pct targetscan.Hs.egMIRBASE2FAMILY
      targetscan.Hs.egMIRNA targetscan.Hs.egTARGETS
      targetscan.Hs.egTARGETSFULL targetscan.Mm.egMIRBASE2FAMILY
      targetscan.Mm.egMIRNA targetscan.Mm.egTARGETS
      targetscan.Mm.egTARGETSFULL total
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
      Component 3: Mean relative difference: 0.9281371
      Component 4: Mean relative difference: 7.749237
      
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

# MSstatsTMT

<details>

* Version: 1.6.1
* Source code: https://github.com/cran/MSstatsTMT
* URL: http://msstats.org/msstatstmt/
* BugReports: https://groups.google.com/forum/#!forum/msstats
* Date/Publication: 2020-05-12
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

# neuropsychology

<details>

* Version: 0.5.0
* Source code: https://github.com/cran/neuropsychology
* URL: https://github.com/neuropsychology/neuropsychology.R
* BugReports: https://github.com/neuropsychology/neuropsychology.R/issues
* Date/Publication: 2017-03-22 19:17:18 UTC
* Number of recursive dependencies: 154

Run `revdep_details(,"neuropsychology")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Error: Can't reconstruct data frame.
    ✖ The `[` method for class <psych/describe/data.frame> must return a data frame.
    ℹ It returned a <describe>.
    Backtrace:
         █
      1. └─neuropsychology::describe(df)
      2.   └─`%>%`(...)
      3.     ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      4.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5.       └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      6.         └─neuropsychology:::`_fseq`(`_lhs`)
      7.           └─magrittr::freduce(value, `_function_list`)
      8.             ├─base::withVisible(function_list[[k]](value))
      9.             └─function_list[[k]](value)
     10.               ├─dplyr::transmute_(...)
     11.               └─dplyr:::transmute_.default(...)
     12.                 ├─dplyr::transmute(.data, !!!dots)
     13.                 └─dplyr:::transmute.data.frame(.data, !!!dots)
     14.                   ├─dplyr::mutate(.data, ..., .keep = "none")
     15.                   └
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘htmlTable’ ‘lme4’ ‘stringi’
      All declared Imports should be used.
    ```

# ngsReports

<details>

* Version: 1.4.1
* Source code: https://github.com/cran/ngsReports
* URL: https://github.com/steveped/ngsReports
* BugReports: https://github.com/steveped/ngsReports/issues
* Date/Publication: 2020-05-21
* Number of recursive dependencies: 157

Run `revdep_details(,"ngsReports")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > # Load the FASTQC data as a FastqcDataList object
    > fdl <- FastqcDataList(fl)
    > 
    > # Export the top10 Overrepresented Sequences as a single fasta file
    > faOut <- file.path(tempdir(), "top10.fa")
    > overRep2Fasta(fdl, path = faOut)
    Error: Problem with `summarise()` input `nFiles`.
    ✖ could not find function "n"
    ℹ Input `nFiles` is `n()`.
    ℹ The error occured in group 1: Sequence = "AAAAATATGGAACGCTTCACGAATTTGCGTCATCCTTGCGCAGGGGCCAT".
    Backtrace:
        █
     1. ├─ngsReports::overRep2Fasta(fdl, path = faOut)
     2. └─ngsReports::overRep2Fasta(fdl, path = faOut)
     3.   ├─dplyr::summarise(...)
     4.   └─dplyr:::summarise.grouped_df(...)
     5.     └─dplyr:::summarise_cols(.data, ...)
    <parent: error/simpleError>
    Backtrace:
    █
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.2Mb
      sub-directories of 1Mb or more:
        R     2.0Mb
        doc   3.0Mb
    ```

# opentripplanner

<details>

* Version: 0.2.1.0
* Source code: https://github.com/cran/opentripplanner
* URL: https://github.com/ropensci/opentripplanner, https://docs.ropensci.org/opentripplanner/
* BugReports: https://github.com/ropensci/opentripplanner/issues
* Date/Publication: 2020-04-14 17:20:03 UTC
* Number of recursive dependencies: 69

Run `revdep_details(,"opentripplanner")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Content type 'application/octet-stream' length 126976 bytes (124 KB)
      ==================================================
      downloaded 124 KB
      
      trying URL 'https://github.com/ropensci/opentripplanner/releases/download/0.1/test_data.zip'
      Content type 'application/octet-stream' length 4289597 bytes (4.1 MB)
      ==================================================
      downloaded 4.1 MB
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 74 | SKIPPED: 22 | WARNINGS: 2 | FAILED: 1 ]
      1. Error: test otp_json2sf (@test_01_internal_funcs.R#64) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 6 marked UTF-8 strings
    ```

# pammtools

<details>

* Version: 0.2.3
* Source code: https://github.com/cran/pammtools
* URL: https://github.com/adibender/pammtools
* BugReports: https://github.com/adibender/pammtools/issues
* Date/Publication: 2020-05-27 10:00:03 UTC
* Number of recursive dependencies: 101

Run `revdep_details(,"pammtools")` for more info

</details>

## Newly broken

*   checking for code/documentation mismatches ... WARNING
    ```
    Codoc mismatches from documentation object 'dplyr_verbs':
    group_by.ped
      Code: function(.data, ..., .add = FALSE)
      Docs: function(.data, ..., add = FALSE)
      Argument names in code not in docs:
        .add
      Argument names in docs not in code:
        add
      Mismatches in argument names:
        Position: 3 Code: .add Docs: add
    group_by.nested_fdf
      Code: function(.data, ..., .add = FALSE)
      Docs: function(.data, ..., add = FALSE)
      Argument names in code not in docs:
        .add
      Argument names in docs not in code:
        add
      Mismatches in argument names:
        Position: 3 Code: .add Docs: add
    ```

# PAST

<details>

* Version: 1.4.1
* Source code: https://github.com/cran/PAST
* URL: https://github.com/IGBB/past
* BugReports: https://github.com/IGBB/past/issues
* Date/Publication: 2020-04-30
* Number of recursive dependencies: 97

Run `revdep_details(,"PAST")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Error: Problem with `mutate()` input `..1`.
    ✖ Input `..1` must be a vector, not a `rlang_data_pronoun` object.
    ℹ Input `..1` is `.data`.
    Backtrace:
         █
      1. ├─utils::example("load_GWAS_data")
      2. │ └─base::source(...)
      3. │   ├─base::withVisible(eval(ei, envir))
      4. │   └─base::eval(ei, envir)
      5. │     └─base::eval(ei, envir)
      6. └─PAST::load_GWAS_data(demo_association_file, demo_effects_file) /var/folders/4b/hn4fq98s6810s4ccv2f9hm2h0000gn/T//Rtmpo6QVx8/Rex350b4cf16ce3:11:0
      7.   └─`%>%`(...)
      8.     ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      9.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
     10.       └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
     11.         └─PAST:::`_fseq`(`_lhs`)
     12.           └─magrittr::freduce(value, `_function_list`)
     13.             └─function_list[[i]](value)
     14.               ├─dplyr::mutate(...)
     15.               └─dplyr:::mutate.data.frame(...)
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
    find_pathway_significance: no visible binding for global variable
      ‘gene_id’
    plot_pathways: no visible binding for global variable
      ‘running_enrichment_score’
    Undefined global functions or variables:
      IRanges Name chromosome gene_id position running_enrichment_score
      seqid
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

# pmdplyr

<details>

* Version: 0.3.1.1
* Source code: https://github.com/cran/pmdplyr
* URL: https://nickch-k.github.io/pmdplyr, https://github.com/NickCH-K/pmdplyr
* BugReports: https://github.com/NickCH-K/pmdplyr/issues
* Date/Publication: 2020-05-15 08:47:17 UTC
* Number of recursive dependencies: 106

Run `revdep_details(,"pmdplyr")` for more info

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

*   checking tests ...
    ```
     ERROR
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
      
      Error: C stack usage  7971360 is too close to the limit
      Execution halted
    ```

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in documentation object 'join.Rd':
      ‘[dplyr]{join.tbl_df}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
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
    [46] 0.50 0.50 1.00 1.00 1.00 1.00 1.00 0.50  NaN 0.50 0.25 1.00 1.00 1.00 0.50
    [61] 1.00 0.50 0.50 0.50 1.00 1.00 1.00 0.50  NaN  NaN 0.50 1.50 1.00 1.00
     [1] 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00
    [16] 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00
    [31] 1.00 0.75 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 0.50 1.00 1.00 1.00
    [46] 1.00 1.00 1.00 1.00 1.00 1.00 1.00 0.50  NaN 1.00 0.75 1.00 1.00 1.00 1.00
    [61] 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00  NaN  NaN 1.00 1.50 1.00 1.00
    Error: Can't combine `..1$SUBJECT` <logical> and `..2$SUBJECT` <character>.
    Backtrace:
         █
      1. ├─rabhit::hapDendo(samplesHaplotype)
      2. │ └─dplyr::bind_rows(haplo_db_clust_texture, tmp_point)
      3. │   └─vctrs::vec_rbind(!!!dots, .names_to = .id)
      4. ├─vctrs:::vec_ptype2.data.frame.grouped_df(...)
      5. │ └─vctrs:::gdf_ptype2(x, y, ...)
      6. │   └─vctrs::df_ptype2(x, y, ...)
      7. └─vctrs::vec_default_ptype2(...)
      8.   └─vctrs::stop_incompatible_type(...)
      9.     └─vctrs:::stop_incompatible(...)
     10.       └─vctrs:::stop_vctrs(...)
    Execution halted
    ```

# Rariant

<details>

* Version: 1.24.0
* Source code: https://github.com/cran/Rariant
* URL: https://github.com/juliangehring/Rariant
* BugReports: https://support.bioconductor.org
* Date/Publication: 2020-04-27
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

*   checking examples ... WARNING
    ```
    Found the following significant warnings:
    
      Warning: 'rbind_all' is deprecated.
    Deprecated functions may be defunct as soon as of the next release of
    R.
    See ?Deprecated.
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
# ratPASTA

<details>

* Version: 0.1.0
* Source code: https://github.com/cran/ratPASTA
* URL: https://github.com/ikodvanj/ratPASTA
* BugReports: https://github.com/ikodvanj/ratPASTA/issues
* Date/Publication: 2020-04-28 11:40:02 UTC
* Number of recursive dependencies: 118

Run `revdep_details(,"ratPASTA")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
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

*   checking tests ...
    ```
     ERROR
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
      [ OK: 63 | SKIPPED: 0 | WARNINGS: 7 | FAILED: 5 ]
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

# SCORPIUS

<details>

* Version: 1.0.6
* Source code: https://github.com/cran/SCORPIUS
* URL: http://github.com/rcannood/SCORPIUS
* BugReports: https://github.com/rcannood/SCORPIUS/issues
* Date/Publication: 2020-03-16 16:20:02 UTC
* Number of recursive dependencies: 116

Run `revdep_details(,"SCORPIUS")` for more info

</details>

## Newly broken

*   checking whether package ‘SCORPIUS’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/SCORPIUS/new/SCORPIUS.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘SCORPIUS’ ...
** package ‘SCORPIUS’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as.tbl_cube’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘SCORPIUS’
* removing ‘/Users/romainfrancois/git/revdep/dplyr/revdep/checks.noindex/SCORPIUS/new/SCORPIUS.Rcheck/SCORPIUS’

```
### CRAN

```
* installing *source* package ‘SCORPIUS’ ...
** package ‘SCORPIUS’ successfully unpacked and MD5 sums checked
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
* DONE (SCORPIUS)

```
# sergeant

<details>

* Version: 0.5.2
* Source code: https://github.com/cran/sergeant
* URL: https://github.com/hrbrmstr/sergeant
* BugReports: https://github.com/hrbrmstr/sergeant/issues
* Date/Publication: 2017-07-17 22:36:26 UTC
* Number of recursive dependencies: 63

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

* Version: 1.18.0
* Source code: https://github.com/cran/sevenbridges
* URL: https://www.sevenbridges.com, https://sbg.github.io/sevenbridges-r/, https://github.com/sbg/sevenbridges-r
* BugReports: https://github.com/sbg/sevenbridges-r/issues
* Date/Publication: 2020-04-27
* Number of recursive dependencies: 63

Run `revdep_details(,"sevenbridges")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > 
    > f1 <- system.file("extdata/app", "flow_star.json", package = "sevenbridges")
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
     11.     └─vctrs:::stop_incompatible(...)
     12.       └─vctrs:::stop_vctrs(...)
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
      Component 4: Mean relative difference: 0.6802195
      Component 5: Attributes: < target is NULL, current is list >
      Component 5: target is numeric, current is factor
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 147 | SKIPPED: 0 | WARNINGS: 3 | FAILED: 6 ]
      1. Failure: bind_rows() fails sadly (@test-dplyr-compat.R#341) 
      2. Failure: bind_cols() works (@test-dplyr-compat.R#354) 
      3. Failure: bind_cols() works (@test-dplyr-compat.R#366) 
      4. Error: group_modify() (@test-dplyr-group-funs.R#43) 
      5. Failure: group_map() (@test-dplyr-group-funs.R#66) 
      6. Failure: group_walk() (@test-dplyr-group-funs.R#106) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# StratigrapheR

<details>

* Version: 1.0.1
* Source code: https://github.com/cran/StratigrapheR
* Date/Publication: 2020-03-20 13:50:06 UTC
* Number of recursive dependencies: 124

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
     4. │   └─vctrs::vec_slice(inputs$x, seq_len(xlen - n))
     5. └─vctrs:::stop_scalar_type(.Primitive("quote")(NULL), "")
     6.   └─vctrs:::stop_vctrs(msg, "vctrs_error_scalar_type", actual = x)
    Execution halted
    ```

# tidyjson

<details>

* Version: 0.3.0
* Source code: https://github.com/cran/tidyjson
* URL: https://github.com/colearendt/tidyjson
* BugReports: https://github.com/colearendt/tidyjson/issues
* Date/Publication: 2020-05-28 13:40:02 UTC
* Number of recursive dependencies: 89

Run `revdep_details(,"tidyjson")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > 
    > key_employees %>% glimpse
    Rows: 527
    Columns: 8
    $ name              <chr> "OutSmart Power Systems", "Firewall Script", "Firew…
    $ array.index       <int> 1, 1, 2, 3, 1, 2, 1, 2, 3, 4, 5, 1, 1, 2, 3, 1, 1, …
    $ is_past           <lgl> FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FAL…
    $ title             <chr> "Board Observer", "", "", "Founder", "CEO", "CBO", …
    $ person.first_name <chr> "Jeffrey", "Ron", "Brandon", "Daniel", "Miguel", "M…
    $ person.last_name  <chr> "Weiss", "Myers", "Farber", "Blake Saltman", "Olive…
    $ person.permalink  <chr> "jeffrey-weiss", "ron-myers", "brandon-farber", "da…
    $ ..JSON            <list> [[FALSE, "Board Observer", ["Jeffrey", "Weiss", "j…
    > 
    > # Show the top 10 titles
    > key_employees %>%
    +   filter(!is_past) %>%
    +   count(title) %>%
    +   arrange(desc(n)) %>%
    +   top_n(10)
    Error: json.list is not a list or json.list is not an atomic vector without attributes
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       16. dplyr:::mutate.data.frame(.data, ..., .keep = "none")
       17. dplyr:::dplyr_col_select(out, keep)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 302 | SKIPPED: 18 | WARNINGS: 0 | FAILED: 7 ]
      1. Error: simple array works (@test-json_structure.R#54) 
      2. Error: seq works for a deeply nested sequence (@test-json_structure.R#145) 
      3. Error: imputes document.id when not present (@test-json_structure.R#168) 
      4. Error: imputed document.id works (@test-json_structure.R#181) 
      5. Error: dplyr::transmute works (@test-tbl_json.R#552) 
      6. Error: dplyr::transmute works (@test-tbl_json.R#601) 
      7. Error: dplyr::transmute works with programming (@test-tbl_json.R#720) 
      
      Error: testthat unit tests failed
      Execution halted
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

# timeOmics

<details>

* Version: 1.0.0
* Source code: https://github.com/cran/timeOmics
* BugReports: https://github.com/abodein/timeOmics/issues
* Date/Publication: 2020-04-27
* Number of recursive dependencies: 121

Run `revdep_details(,"timeOmics")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > block.pls.cluster <- getCluster(demo$block.pls)
    Error: Join columns must be unique.
    ✖ Problem at position 2.
    Backtrace:
         █
      1. ├─timeOmics::getCluster(demo$block.pls)
      2. └─timeOmics:::getCluster.block.pls(demo$block.pls)
      3.   └─`%>%`(...)
      4.     ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      5.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      6.       └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      7.         └─timeOmics:::`_fseq`(`_lhs`)
      8.           └─magrittr::freduce(value, `_function_list`)
      9.             └─function_list[[i]](value)
     10.               ├─dplyr::left_join(., block.info, by = c("molecule", "molecule"))
     11.               └─dplyr:::left_join.data.frame(...)
     12.                 └─dplyr:::join_mutate(...)
     13.                   └─dplyr:::join_cols(...)
     14.                     └─dplyr:::standardise_join_by(by, x_names = x_names, y_names = y_names)
     15.                       └─dplyr:::check_join_vars(by$x, x_names)
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 174 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 17 ]
      1. Error: getCluster works and return a valid output (@test-getCluster.R#19) 
      2. Error: getNcomp works (@test-getNcomp.R#57) 
      3. Error: getNcomp plot works (@test-getNcomp.R#69) 
      4. Error: getSilhouette works (@test-getSilhouette.R#20) 
      5. Error: lmms.filter.lines works (@test-lmms.filter.lines.R#57) 
      6. Failure: plotLong failed on invalid - time (@test-plotLong.R#41) 
      7. Failure: plotLong failed on invalid - time (@test-plotLong.R#42) 
      8. Failure: plotLong failed on invalid - time (@test-plotLong.R#43) 
      9. Failure: plotLong failed on invalid - time (@test-plotLong.R#44) 
      1. ...
      
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

*   checking dependencies in R code ... NOTE
    ```
    Package in Depends field not imported from: ‘lmms’
      These packages need to be imported from (in the NAMESPACE file)
      for when this namespace is loaded but not attached.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    tune.silhouette.get_slopes: no visible binding for global variable
      ‘Z_score.neg’
    tuneCluster.block.spls: no visible binding for global variable
      ‘silhouette.coef’
    tuneCluster.spca: no visible binding for global variable
      ‘silhouette.coef’
    tuneCluster.spls: no visible binding for global variable
      ‘silhouette.coef’
    unscale: no visible global function definition for ‘is’
    Undefined global functions or variables:
      . BP.test MSE Pval.dir Pval.neg Pval.pos Pval.value Pvalue X Y_hat
      Y_i Z_score.neg Z_score.pos block cluster cluster1 cluster2 comp
      contrib contrib.max cor destination direction distance_from_origin
      error feature feature1 feature2 insideout is median molecule na.omit
      ncomp origin pnorm sd silhouette.coef slope.neg slope.pos slot val
      value
    Consider adding
      importFrom("methods", "is", "slot")
      importFrom("stats", "cor", "median", "na.omit", "pnorm", "sd")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

# TPP

<details>

* Version: 3.16.0
* Source code: https://github.com/cran/TPP
* Date/Publication: 2020-04-27
* Number of recursive dependencies: 95

Run `revdep_details(,"TPP")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Normalization successfully completed!
    
    Warning: `as.tbl()` is deprecated as of dplyr 1.0.0.
    Please use `tibble::as_tibble()` instead.
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_warnings()` to see where this warning was generated.
    Fitting smoothing splines and AICc values for the following degrees of freedom: 3, 4, 5, 6, 7
    Fitting null models to 510 proteins (using 3 degrees of freedom)
    Fitting alternative models to 510 proteins (using 3 degrees of freedom)
    Fitting null models to 510 proteins (using 4 degrees of freedom)
    Fitting alternative models to 510 proteins (using 4 degrees of freedom)
    Fitting null models to 510 proteins (using 5 degrees of freedom)
    Fitting alternative models to 510 proteins (using 5 degrees of freedom)
    Fitting null models to 510 proteins (using 6 degrees of freedom)
    Fitting alternative models to 510 proteins (using 6 degrees of freedom)
    Fitting null models to 510 proteins (using 7 degrees of freedom)
    Fitting alternative models to 510 proteins (using 7 degrees of freedom)
    Error in { : 
      task 1 failed - "Can't recycle input of size 510 to size 1020."
    Calls: analyzeTPPTR ... tpptrSplineFitAndTest -> tpptrFitSplines -> %dopar% -> <Anonymous>
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 167 | SKIPPED: 1 | WARNINGS: 255 | FAILED: 26 ]
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
      [ OK: 11 | SKIPPED: 0 | WARNINGS: 1 | FAILED: 2 ]
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

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Backtrace:
        1. vcfR::vcfR2tidy(vcfR_test, info_only = FALSE)
       15. vctrs::vec_default_ptype2(...)
       16. vctrs::stop_incompatible_type(...)
       17. vctrs:::stop_incompatible(...)
       18. vctrs:::stop_vctrs(...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 475 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 3 ]
      1. Error: extract_gt_tidy works for GT element (@test_vcfRtidy.R#55) 
      2. Error: extract_gt_tidy works for all elements (@test_vcfRtidy.R#70) 
      3. Error: vcfR2tidy works (@test_vcfRtidy.R#81) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# vpc

<details>

* Version: 1.2.0
* Source code: https://github.com/cran/vpc
* URL: https://github.com/ronkeizer/vpc
* Date/Publication: 2020-05-07 15:10:02 UTC
* Number of recursive dependencies: 70

Run `revdep_details(,"vpc")` for more info

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

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-vpc_cat.R’ failed.
    Last 13 lines of output:
      * `fact_perc(dv, lev[i])` -> `fact_perc(dv, lev[i])...3`
      * sim -> sim...4
      * bin -> bin...5
      * ...
      New names:
      * strat -> strat...1
      * bin -> bin...2
      * strat -> strat...4
      * bin -> bin...5
      * strat -> strat...7
      * ...
      Error in names(x) <- value : 
        'names' attribute [6] must be the same length as the vector [4]
      Calls: vpc_cat -> colnames<-
      Execution halted
    ```

# yamlet

<details>

* Version: 0.4.7
* Source code: https://github.com/cran/yamlet
* Date/Publication: 2020-05-18 17:50:02 UTC
* Number of recursive dependencies: 70

Run `revdep_details(,"yamlet")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Packages unavailable to check Rd xrefs: ‘dtplyr’, ‘dbplyr’
    Missing link or links in documentation object 'anti_join.decorated.Rd':
      ‘[dplyr]{join.tbl_df}’
    
    Missing link or links in documentation object 'full_join.decorated.Rd':
      ‘[dplyr]{join.tbl_df}’
    
    Missing link or links in documentation object 'inner_join.decorated.Rd':
      ‘[dplyr]{join.tbl_df}’
    
    Missing link or links in documentation object 'left_join.decorated.Rd':
      ‘[dplyr]{join.tbl_df}’
    
    Missing link or links in documentation object 'right_join.decorated.Rd':
      ‘[dplyr]{join.tbl_df}’
    
    Missing link or links in documentation object 'semi_join.decorated.Rd':
      ‘[dplyr]{join.tbl_df}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

## Newly fixed

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘dtplyr’, ‘dbplyr’
    ```

