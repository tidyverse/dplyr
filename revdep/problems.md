# broomExtra

Version: 0.0.1

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### Title: Augmented data from grouped analysis of any function that has
    > ###   'data' argument in its function call.
    > ### Aliases: grouped_augment
    > 
    > ### ** Examples
    > 
    > set.seed(123)
    > # to speed up computation, let's use only 50% of the data
    > 
    > # linear model
    > broomExtra::grouped_augment(
    +   data = dplyr::sample_frac(tbl = ggplot2::diamonds, size = 0.5),
    +   grouping.vars = c(cut, color),
    +   formula = price ~ carat - 1,
    +   ..f = stats::lm,
    +   na.action = na.omit
    + )
    Error in UseMethod("ungroup") : 
      no applicable method for 'ungroup' applied to an object of class "list"
    Calls: <Anonymous> ... freduce -> withVisible -> <Anonymous> -> <Anonymous>
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      5: eval(quote(`_fseq`(`_lhs`)), env, env)
      6: `_fseq`(`_lhs`)
      7: freduce(value, `_function_list`)
      8: withVisible(function_list[[k]](value))
      9: function_list[[k]](value)
      10: dplyr::ungroup(x = .)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 9 SKIPPED: 0 FAILED: 3
      1. Error: `grouped_augment()` works (@test_grouped_augment.R#12) 
      2. Error: `grouped_glance()` works (@test_grouped_glance.R#12) 
      3. Error: `grouped_tidy()` works (@test_grouped_tidy.R#12) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# Cardinal

Version: 2.0.4

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

# compareDF

Version: 1.7.2

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
      [31m──[39m [31m1. Failure: (unknown) (@test-fnsComparison.R#369) [39m [31m───────────────────────────────────────────────────────────────────────[39m
      `expected_change_count` not equivalent to actual_comparison_summary$change_count.
      Incompatible type for column `changes`: x numeric, y integer
      
      ══ testthat results  ════════════════════════════════════════════════════════════════════════════════════════════════════════
      OK: 48 SKIPPED: 0 FAILED: 1
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

# corrr

Version: 0.3.1

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 39-44 (corrr-databases.Rmd) 
    Error: processing vignette 'corrr-databases.Rmd' failed with diagnostics:
    object 'mpg' not found
    Execution halted
    ```

# cytominer

Version: 0.1.0

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    
    > ### Name: normalize
    > ### Title: Normalize observation variables.
    > ### Aliases: normalize
    > 
    > ### ** Examples
    > 
    > suppressMessages(suppressWarnings(library(magrittr)))
    > population <- tibble::data_frame(
    +    Metadata_group = c("control", "control", "control", "control",
    +                       "experiment", "experiment", "experiment", "experiment"),
    +    Metadata_batch = c("a", "a", "b", "b", "a", "a", "b", "b"),
    +    AreaShape_Area = c(10, 12, 15, 16, 8, 8, 7, 7)
    +  )
    > variables <- c('AreaShape_Area')
    > strata <- c('Metadata_batch')
    > sample <- population %>% dplyr::filter(Metadata_group == 'control')
    > cytominer::normalize(population, variables, strata, sample, operation = "standardize")
    Error in mean(., na.rm = TRUE) : object '.' not found
    Calls: <Anonymous> ... summarise.tbl_df -> summarise_impl -> <Anonymous> -> eval_tidy -> mean
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      32: eval_bare(call, env)
      33: (function (...) 
         eval_tidy(~mean(.)))(x)
      34: eval_tidy(~mean(.))
      35: mean(.)
      
      ══ testthat results  ════════════════════════════════════════════════════════════════════════════════════════════════════════
      OK: 28 SKIPPED: 0 FAILED: 4
      1. Error: `aggregate` aggregates data (@test-aggregate.R#37) 
      2. Error: cytominer can process dataset with a normalized schema (@test-cytominer.R#71) 
      3. Error: cytominer can process dataset with a CellProfiler schema (@test-cytominer.R#227) 
      4. Error: `normalize' normalizes data (@test-normalize.R#49) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 149-165 (cytominer-pipeline.Rmd) 
    Error: processing vignette 'cytominer-pipeline.Rmd' failed with diagnostics:
    object '.' not found
    Execution halted
    ```

# dbplyr

Version: 1.3.0

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      13: eval_bare(call, env)
      
      Error in connection_create(host, username, password, dbname, as.integer(port),  : 
        Failed to connect: Can't connect to local MySQL server through socket '/tmp/mysql.sock' (2)
      Calls: <Anonymous> ... <Anonymous> -> <Anonymous> -> .local -> connection_create
      In addition: Warning message:
      In dbDisconnect(con) : restarting interrupted promise evaluation
      ══ testthat results  ════════════════════════════════════════════════════════════════════════════════════════════════════════
      OK: 565 SKIPPED: 11 FAILED: 1
      1. Error: tbl_dbi support colwise variants (@test-colwise.R#13) 
      
      Error: testthat unit tests failed
      In addition: Warning message:
      call dbDisconnect() when finished working with a connection 
      Execution halted
    ```

# FindMyFriends

Version: 1.12.0

## In both

*   R CMD check timed out
    

*   checking for code/documentation mismatches ... WARNING
    ```
    Functions or methods with usage in documentation object 'pgVirtual-class' but not in code:
      as
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.8Mb
      sub-directories of 1Mb or more:
        R         1.0Mb
        doc       1.5Mb
        extdata   1.8Mb
        libs      1.2Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unexported objects imported by ':::' calls:
      ‘gtable:::insert.unit’ ‘gtable:::z_arrange_gtables’
      See the note in ?`:::` about the use of this operator.
    ```

# GFE

Version: 0.1.0

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
    Warning in var(.) : NAs introduced by coercion
    Error: Column `t1_Santos` must be length 1 (a summary value), not 25
    Execution halted
    ```

# grasp2db

Version: 1.1.0

## In both

*   R CMD check timed out
    

*   checking for missing documentation entries ... WARNING
    ```
    Undocumented code objects:
      ‘GRASP2’ ‘checkAnti’ ‘getJoinCompatible’
    Undocumented data sets:
      ‘mml10p_nox’ ‘uniqueGexNames2.0’ ‘uniquePPDnames2.0’
    All user-level objects in a package should have documentation entries.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking data for non-ASCII characters ... WARNING
    ```
      Warning: found non-ASCII string
      'Beh<e7>et's disease' in object 'uniquePPDnames2.0'
    ```

*   checking data for ASCII and uncompressed saves ... WARNING
    ```
      
      Note: significantly better compression could be obtained
            by using R CMD build --resave-data
                            old_size new_size compress
      mml10p_nox.rda           7.1Mb    2.8Mb       xz
      uniquePPDnames2.0.rda     17Kb     15Kb    bzip2
    ```

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘AnnotationHubData’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  7.6Mb
      sub-directories of 1Mb or more:
        data   7.1Mb
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    License components with restrictions not permitted:
      Artistic-2.0 + file LICENSE
    ```

*   checking R code for possible problems ... NOTE
    ```
    .grasp2ToAnnotationHub: no visible global function definition for
      ‘outputFile’
    checkAnti: no visible binding for global variable ‘chr_hg19’
    getJoinCompatible: no visible binding for global variable ‘gwrngs19’
    Undefined global functions or variables:
      chr_hg19 gwrngs19 outputFile
    ```

# HTSSIP

Version: 1.4.0

## In both

*   R CMD check timed out
    

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘igraph’
      All declared Imports should be used.
    ```

# InjurySeverityScore

Version: 0.0.0.1

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
    [90m    [39m█
    [90m 1. [39m└─InjurySeverityScore::injury_score(sample_data, subj, code)
    [90m 2. [39m  ├─base::cbind(...)
    [90m 3. [39m  └─dplyr::coalesce(iss_br$max_wo_9, iss_br$max_w_9, iss_br$severity_default)
    [90m 4. [39m    └─dplyr:::replace_with(...) [90m/Users/romainfrancois/git/dplyr-revdep/dplyr/R/coalesce.R:42:4[39m
    [90m 5. [39m      └─dplyr:::check_type(val, x, name) [90m/Users/romainfrancois/git/dplyr-revdep/dplyr/R/utils-replace-with.R:7:2[39m
    [90m 6. [39m        └─dplyr:::glubort(header, "must be {friendly_type_of(template)}, not {friendly_type_of(x)}") [90m/Users/romainfrancois/git/dplyr-revdep/dplyr/R/utils-replace-with.R:52:2[39m
    Execution halted
    ```

# modeldb

Version: 0.1.2

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 45-49 (kmeans.Rmd) 
    Error: processing vignette 'kmeans.Rmd' failed with diagnostics:
    object 'dep_time' not found
    Execution halted
    ```

# MonetDBLite

Version: 0.6.0

## In both

*   R CMD check timed out
    

*   checking installed package size ... NOTE
    ```
      installed size is  5.7Mb
      sub-directories of 1Mb or more:
        libs   5.4Mb
    ```

# perturbatr

Version: 1.2.1

## Newly broken

*   R CMD check timed out
    

# pmc

Version: 1.0.3

## In both

*   R CMD check timed out
    

# rubias

Version: 0.2.0

## In both

*   R CMD check timed out
    

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘ggplot2’
      All declared Imports should be used.
    ```

# ruler

Version: 0.2.0

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
      34: is.data.frame(x)
      
      ══ testthat results  ════════════════════════════════════════════════════════════════════════════════════════════════════════
      OK: 281 SKIPPED: 1 FAILED: 8
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

# strapgod

Version: 0.0.1

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      nrow(x_gm) not equal to 5.
      target is NULL, current is numeric
      
      [31m──[39m [31m3. Failure: group_map() (@test-dplyr-group-funs.R#52) [39m [31m───────────────────────────────────────────────────────────────────[39m
      x_gm$.g[[1]] not equal to dplyr::tibble(.bootstrap = 1L).
      target is NULL, current is tbl_df
      
      ══ testthat results  ════════════════════════════════════════════════════════════════════════════════════════════════════════
      OK: 146 SKIPPED: 0 FAILED: 3
      1. Failure: group_map() (@test-dplyr-group-funs.R#43) 
      2. Failure: group_map() (@test-dplyr-group-funs.R#50) 
      3. Failure: group_map() (@test-dplyr-group-funs.R#52) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# XBSeq

Version: 1.14.1

## In both

*   R CMD check timed out
    

*   checking whether the namespace can be loaded with stated dependencies ... NOTE
    ```
    Warning: no function found corresponding to methods exports from ‘XBSeq’ for: ‘conditions’, ‘conditions<-’, ‘dispTable’
    
    A namespace must be able to be loaded with just the base namespace
    loaded: otherwise if the namespace gets loaded by a saved object, the
    session will be unable to start.
    
    Probably some imports need to be declared in the NAMESPACE file.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    estimateRealCount,XBSeqDataSet: no visible global function definition
      for ‘assay’
    estimateRealCount,XBSeqDataSet: no visible global function definition
      for ‘assay<-’
    estimateSCV,XBSeqDataSet: no visible global function definition for
      ‘conditions’
    estimateSCV,XBSeqDataSet: no visible global function definition for
      ‘dispTable<-’
    Undefined global functions or variables:
      ..count.. DataFrame Gamma Group Sample SummarizedExperiment assay
      assay<- assays baseMean coefficients complete.cases conditions cor
      data ddelap dispTable dispTable<- dnbinom dpois formula glm
      log2FoldChange median optim p.adjust pbeta predict qbeta quantile
      rnbinom scvBiasCorrectionFits
    Consider adding
      importFrom("stats", "Gamma", "coefficients", "complete.cases", "cor",
                 "dnbinom", "dpois", "formula", "glm", "median", "optim",
                 "p.adjust", "pbeta", "predict", "qbeta", "quantile",
                 "rnbinom")
      importFrom("utils", "data")
    to your NAMESPACE file.
    ```

