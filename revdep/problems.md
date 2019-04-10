# banR

Version: 0.2.0

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### Name: geocode_tbl
    > ### Title: Geocode tbl
    > ### Aliases: geocode_tbl
    > 
    > ### ** Examples
    > 
    > 
    > table_test <- tibble::tibble(
    + x = c("39 quai Andre Citroen", "64 Allee de Bercy", "20 avenue de Segur"), 
    + y = c("75015", "75012", "75007"), 
    + z = rnorm(3)
    + )
    > 
    > geocode_tbl(tbl = table_test, adresse = x)
    Writing tempfile to.../var/folders/4b/hn4fq98s6810s4ccv2f9hm2h0000gn/T//RtmpLabKmK/file8ed277e80d06.csv
    If file is larger than 8 MB, it must be splitted
    Size is : 61 bytes
    Server errorService UnavailableServer error: (503) Service Unavailable
    Error in geocode_tbl(tbl = table_test, adresse = x) : 
      The API sent back an error 503
    Execution halted
    ```

## Newly fixed

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 47-49 (geocode.Rmd) 
    Erreur : le traitement de la vignette 'geocode.Rmd' a échoué avec le diagnostic :
    The API sent back an error 503
    Exécution arrêtée
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘stringr’
      All declared Imports should be used.
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
        R     3.0Mb
        doc   2.2Mb
    ```

# CluMP

Version: 0.7.1

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
      ── 1. Failure: (unknown) (@test-fnsComparison.R#369)  ──────────────────────────
      `expected_change_count` not equivalent to actual_comparison_summary$change_count.
      Incompatible type for column `changes`: x numeric, y integer
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
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
    Erreur : le traitement de la vignette 'corrr-databases.Rmd' a échoué avec le diagnostic :
    objet 'mpg' introuvable
    Exécution arrêtée
    ```

# cytominer

Version: 0.1.0

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
             base::pairlist(...)
             `_quo` <- rlang::quo(eval_tidy(~mean(.)))
             rlang::eval_bare(`_quo`, base::parent.frame())
         }, class = "inline_colwise_function", formula = ~mean(.)))(x)
      34: base::pairlist(...) at /Users/romainfrancois/git/tidyverse/dplyr/R/colwise.R:238
      35: as.pairlist(list(...))
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 28 SKIPPED: 0 FAILED: 3
      1. Error: `aggregate` aggregates data (@test-aggregate.R#37) 
      2. Error: cytominer can process dataset with a normalized schema (@test-cytominer.R#71) 
      3. Error: `normalize' normalizes data (@test-normalize.R#49) 
      
      Error: testthat unit tests failed
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
      ══ testthat results  ═══════════════════════════════════════════════════════════
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

# GEOmetadb

Version: 1.44.0

## In both

*   R CMD check timed out
    

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .travis.yml
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

*   checking R code for possible problems ... NOTE
    ```
    getSQLiteFile: no visible global function definition for
      ‘download.file’
    Undefined global functions or variables:
      download.file
    Consider adding
      importFrom("utils", "download.file")
    to your NAMESPACE file.
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
        █
     1. └─InjurySeverityScore::injury_score(sample_data, subj, code)
     2.   ├─base::cbind(...)
     3.   └─dplyr::coalesce(iss_br$max_wo_9, iss_br$max_w_9, iss_br$severity_default)
     4.     └─dplyr:::replace_with(...) /Users/romainfrancois/git/tidyverse/dplyr/R/coalesce.R:42:4
     5.       └─dplyr:::check_type(val, x, name) /Users/romainfrancois/git/tidyverse/dplyr/R/utils-replace-with.R:7:2
     6.         └─dplyr:::glubort(header, "must be {friendly_type_of(template)}, not {friendly_type_of(x)}") /Users/romainfrancois/git/tidyverse/dplyr/R/utils-replace-with.R:52:2
    Execution halted
    ```

# IrisSpatialFeatures

Version: 1.3.0

## In both

*   R CMD check timed out
    

# modeldb

Version: 0.1.2

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 45-49 (kmeans.Rmd) 
    Erreur : le traitement de la vignette 'kmeans.Rmd' a échoué avec le diagnostic :
    objet 'dep_time' introuvable
    Exécution arrêtée
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

# pammtools

Version: 0.1.9

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      41: tryCatchList(expr, classes, parentenv, handlers)
      42: tryCatch(withCallingHandlers({    eval(code, test_env)    if (!handled && !is.null(test)) {        skip_empty()    }}, expectation = handle_expectation, skip = handle_skip, warning = handle_warning,     message = handle_message, error = handle_error), error = handle_fatal,     skip = function(e) {    })
      43: test_code(NULL, exprs, env)
      44: source_file(path, new.env(parent = env), chdir = TRUE, wrap = wrap)
      45: force(code)
      46: with_reporter(reporter = reporter, start_end_reporter = start_end_reporter,     {        lister$start_file(basename(path))        source_file(path, new.env(parent = env), chdir = TRUE,             wrap = wrap)        end_context()    })
      47: FUN(X[[i]], ...)
      48: lapply(paths, test_file, env = env, reporter = current_reporter,     start_end_reporter = FALSE, load_helpers = FALSE, wrap = wrap)
      49: force(code)
      50: with_reporter(reporter = current_reporter, results <- lapply(paths,     test_file, env = env, reporter = current_reporter, start_end_reporter = FALSE,     load_helpers = FALSE, wrap = wrap))
      51: test_files(paths, reporter = reporter, env = env, stop_on_failure = stop_on_failure,     stop_on_warning = stop_on_warning, wrap = wrap)
      52: test_dir(path = test_path, reporter = reporter, env = env, filter = filter,     ..., stop_on_failure = stop_on_failure, stop_on_warning = stop_on_warning,     wrap = wrap)
      53: test_package_dir(package = package, test_path = test_path, filter = filter,     reporter = reporter, ..., stop_on_failure = stop_on_failure,     stop_on_warning = stop_on_warning, wrap = wrap)
      54: test_check("pammtools")
      An irrecoverable exception occurred. R is aborting now ...
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
      37: is.data.frame(x)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
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

# spdplyr

Version: 0.2.0

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    + 
    + spmap %>% mutate_if(is.numeric, as.character)
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
     10.               └─dplyr:::manip_if(...) /Users/romainfrancois/git/tidyverse/dplyr/R/colwise-mutate.R:240:2
     11.                 └─dplyr:::tbl_if_syms(.tbl, .predicate, .env, .include_group_vars = .include_group_vars) /Users/romainfrancois/git/tidyverse/dplyr/R/colwise-mutate.R:285:2
     12.                   ├─rlang::syms(tbl_if_vars(.tbl, .p, .env, ..., .include_group
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      10: manip_if(.tbl, .predicate, .funs, enquo(.funs), caller_env(), ...) at /Users/romainfrancois/git/tidyverse/dplyr/R/colwise-mutate.R:240
      11: tbl_if_syms(.tbl, .predicate, .env, .include_group_vars = .include_group_vars) at /Users/romainfrancois/git/tidyverse/dplyr/R/colwise-mutate.R:285
      12: syms(tbl_if_vars(.tbl, .p, .env, ..., .include_group_vars = .include_group_vars)) at /Users/romainfrancois/git/tidyverse/dplyr/R/colwise.R:218
      13: map(x, sym)
      14: lapply(.x, .f, ...)
      15: tbl_if_vars(.tbl, .p, .env, ..., .include_group_vars = .include_group_vars) at /Users/romainfrancois/git/tidyverse/dplyr/R/colwise.R:218
      16: as_data_mask(.tbl) at /Users/romainfrancois/git/tidyverse/dplyr/R/colwise.R:208
      17: rlang::abort(x)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 46 SKIPPED: 4 FAILED: 1
      1. Error: mutate_all, mutate_at (@test-adv-dplyr.R#83) 
      
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
      
      ── 3. Failure: group_map() (@test-dplyr-group-funs.R#52)  ──────────────────────
      x_gm$.g[[1]] not equal to dplyr::tibble(.bootstrap = 1L).
      target is NULL, current is tbl_df
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
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

