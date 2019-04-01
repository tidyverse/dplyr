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

# chunked

Version: 0.4

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      3: count(x) %>% collect
      4: eval(lhs, parent, parent)
      5: eval(lhs, parent, parent)
      6: count(x)
      7: .group_by_static_drop(x, !!!syms(groups), add = FALSE, .drop = .drop) at /Users/romainfrancois/git/tidyverse/dplyr/R/count-tally.R:136
      8: group_by_drop_default(x) at /Users/romainfrancois/git/tidyverse/dplyr/R/count-tally.R:136
      9: group_by_drop_default.default(x) at /Users/romainfrancois/git/tidyverse/dplyr/R/group-by.r:220
      10: group_data(.tbl) at /Users/romainfrancois/git/tidyverse/dplyr/R/group-by.r:225
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 39 SKIPPED: 0 FAILED: 1
      1. Error: write_chunkwise to db works (@test-write.R#29) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# ClinReport

Version: 0.9.1.11

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > 
    > 
    > # Load the data
    > 
    > data(data)
    > 
    > # The default statistics are given here:
    > 
    > tab1=report.quanti(data=data,y="y_numeric",x1="GROUP",total=TRUE,subjid="SUBJID")
    > 
    > # Define the function corresponding to the coefficient of variation for example
    > 
    > cv=function(y) sd(y,na.rm=TRUE)/mean(y,na.rm=TRUE)
    > 
    > # We use the add.stat function to add CV at the second row:
    > 
    > tab1.cv=add.stat(tab1,data,func.stat=cv,func.stat.name="Coef. Var",
    + pos=2)
    Error in func.stat(y_numeric) : could not find function "func.stat"
    Calls: add.stat ... summarise -> summarise.tbl_df -> summarise_impl -> <Anonymous>
    Execution halted
    ```

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Fontconfig error: "/Users/romainfrancois/git/dplyr-revdep/dplyr/revdep/library.noindex/ClinReport/magick/etc/fonts/conf.d/69-unifont.conf", line 5: invalid attribute 'translate'
    Fontconfig error: "/Users/romainfrancois/git/dplyr-revdep/dplyr/revdep/library.noindex/ClinReport/magick/etc/fonts/conf.d/69-unifont.conf", line 5: invalid attribute 'selector'
    Fontconfig error: "/Users/romainfrancois/git/dplyr-revdep/dplyr/revdep/library.noindex/ClinReport/magick/etc/fonts/conf.d/69-unifont.conf", line 6: invalid attribute 'xmlns:its'
    Fontconfig error: "/Users/romainfrancois/git/dplyr-revdep/dplyr/revdep/library.noindex/ClinReport/magick/etc/fonts/conf.d/69-unifont.conf", line 6: invalid attribute 'version'
    Fontconfig warning: "/Users/romainfrancois/git/dplyr-revdep/dplyr/revdep/library.noindex/ClinReport/magick/etc/fonts/conf.d/80-delicious.conf", line 4: unknown element "its:rules"
    Fontconfig warning: "/Users/romainfrancois/git/dplyr-revdep/dplyr/revdep/library.noindex/ClinReport/magick/etc/fonts/conf.d/80-delicious.conf", line 5: unknown element "its:translateRule"
    Fontconfig error: "/Users/romainfrancois/git/dplyr-revdep/dplyr/revdep/library.noindex/ClinReport/magick/etc/fonts/conf.d/80-delicious.conf", line 5: invalid attribute 'translate'
    Fontconfig error: "/Users/romainfrancois/git/dplyr-revdep/dplyr/revdep/library.noindex/ClinReport/magick/etc/fonts/conf.d/80-delicious.conf", line 5: invalid attribute 'selector'
    Fontconfig error: "/Users/romainfrancois/git/dplyr-revdep/dplyr/revdep/library.noindex/ClinReport/magick/etc/fonts/conf.d/80-delicious.conf", line 6: invalid attribute 'xmlns:its'
    Fontconfig error: "/Users/romainfrancois/git/dplyr-revdep/dplyr/revdep/library.noindex/ClinReport/magick/etc/fonts/conf.d/80-delicious.conf", line 6: invalid attribute 'version'
    Fontconfig warning: "/Users/romainfrancois/git/dplyr-revdep/dplyr/revdep/library.noindex/ClinReport/magick/etc/fonts/conf.d/90-synthetic.conf", line 4: unknown element "its:rules"
    Fontconfig warning: "/Users/romainfrancois/git/dplyr-revdep/dplyr/revdep/library.noindex/ClinReport/magick/etc/fonts/conf.d/90-synthetic.conf", line 5: unknown element "its:translateRule"
    Fontconfig error: "/Users/romainfrancois/git/dplyr-revdep/dplyr/revdep/library.noindex/ClinReport/magick/etc/fonts/conf.d/90-synthetic.conf", line 5: invalid attribute 'translate'
    Fontconfig error: "/Users/romainfrancois/git/dplyr-revdep/dplyr/revdep/library.noindex/ClinReport/magick/etc/fonts/conf.d/90-synthetic.conf", line 5: invalid attribute 'selector'
    Fontconfig error: "/Users/romainfrancois/git/dplyr-revdep/dplyr/revdep/library.noindex/ClinReport/magick/etc/fonts/conf.d/90-synthetic.conf", line 6: invalid attribute 'xmlns:its'
    Fontconfig error: "/Users/romainfrancois/git/dplyr-revdep/dplyr/revdep/library.noindex/ClinReport/magick/etc/fonts/conf.d/90-synthetic.conf", line 6: invalid attribute 'version'
    Fontconfig error: Cannot load default config file
    Quitting from lines 60-71 (clinreport_modify_outputs.Rmd) 
    Erreur : le traitement de la vignette 'clinreport_modify_outputs.Rmd' a échoué avec le diagnostic :
    `render_flextable` needs to be used as a renderer for a knitr/rmarkdown R code chunk (render by rmarkdown)
    Exécution arrêtée
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘emmeans’ ‘utils’
      All declared Imports should be used.
    ```

# coalitions

Version: 0.6.5

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘coalitions-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: calculate_prob
    > ### Title: Calculate coalition probability from majority table
    > ### Aliases: calculate_prob
    > 
    > ### ** Examples
    > 
    > test_df <- data.frame(
    +  cdu            = c(rep(FALSE, 9), TRUE),
    +  cdu_fdp        = c(rep(FALSE, 8), TRUE, TRUE),
    +  cdu_fdp_greens = c(TRUE, TRUE, rep(FALSE, 6), TRUE, TRUE))
    > calculate_prob(test_df, "cdu_fdp_greens") # exclude_superior defaults to TRUE
    Error in (function (..., .x = ..1, .y = ..2, . = ..1)  : 
      object 'n_all' not found
    Calls: calculate_prob ... summarise -> summarise.tbl_df -> summarise_impl -> <Anonymous>
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      30: function_list[[k]](value)
      31: summarize_at(., coalition, ~sum(.)/n_all * 100)
      32: summarise(.tbl, !!!funs) at /Users/romainfrancois/git/tidyverse/dplyr/R/colwise-mutate.R:123
      33: summarise.tbl_df(.tbl, !!!funs) at /Users/romainfrancois/git/tidyverse/dplyr/R/manip.r:269
      34: summarise_impl(.data, dots, environment(), caller_env()) at /Users/romainfrancois/git/tidyverse/dplyr/R/tbl-df.r:102
      35: (structure(function (..., .x = ..1, .y = ..2, . = ..1) 
         sum(.)/n_all * 100, class = "rlang_lambda_function"))(cdu) at /Users/romainfrancois/git/tidyverse/dplyr/R/RcppExports.R:188
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 81 SKIPPED: 0 FAILED: 2
      1. Error: Pooling works as expected (@test-pooling.R#12) 
      2. Error: workflow stable (@test-workflow.R#64) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Warning in engine$weave(file, quiet = quiet, encoding = enc) :
      Pandoc (>= 1.12.3) and/or pandoc-citeproc not available. Falling back to R Markdown v1.
    Warning in engine$weave(file, quiet = quiet, encoding = enc) :
      Pandoc (>= 1.12.3) and/or pandoc-citeproc not available. Falling back to R Markdown v1.
    Warning:  1 failed to parse.
    Warning:  1 failed to parse.
    Quitting from lines 155-161 (workflow.Rmd) 
    Erreur : le traitement de la vignette 'workflow.Rmd' a échoué avec le diagnostic :
    objet 'n_all' introuvable
    Exécution arrêtée
    ```

# compareDF

Version: 1.7.1

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── 1. Failure: (unknown) (@test-fnsComparison.R#74)  ───────────────────────────
      output$change_count not equivalent to `expected_change_count`.
      Incompatible type for column `changes`: x integer, y numeric
      
      ── 2. Failure: (unknown) (@test-fnsComparison.R#357)  ──────────────────────────
      `expected_change_count` not equivalent to actual_comparison_summary$change_count.
      Incompatible type for column `changes`: x numeric, y integer
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 46 SKIPPED: 0 FAILED: 2
      1. Failure: (unknown) (@test-fnsComparison.R#74) 
      2. Failure: (unknown) (@test-fnsComparison.R#357) 
      
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
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
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
    Warning in engine$weave(file, quiet = quiet, encoding = enc) :
      Pandoc (>= 1.12.3) and/or pandoc-citeproc not available. Falling back to R Markdown v1.
    Quitting from lines 149-165 (cytominer-pipeline.Rmd) 
    Erreur : le traitement de la vignette 'cytominer-pipeline.Rmd' a échoué avec le diagnostic :
    objet '.' introuvable
    Exécution arrêtée
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

# desctable

Version: 0.1.5

## Newly broken

*   checking for code/documentation mismatches ... WARNING
    ```
    Codoc mismatches from documentation object 'group_by':
    group_by
      Code: function(.data, ..., add = FALSE, .drop =
                     group_by_drop_default(.data))
      Docs: function(.data, ..., add = FALSE, .drop = FALSE)
      Mismatches in argument default values:
        Name: '.drop' Code: group_by_drop_default(.data) Docs: FALSE
    ```

# ELMER

Version: 2.4.4

## In both

*   R CMD check timed out
    

*   checking dependencies in R code ... WARNING
    ```
    '::' or ':::' import not declared from: 'data.table'
    ':::' call which should be '::': 'TCGAbiolinks:::TCGAVisualize_volcano'
      See the note in ?`:::` about the use of this operator.
    Unexported objects imported by ':::' calls:
      'TCGAbiolinks:::colDataPrepare' 'TCGAbiolinks:::get.GRCh.bioMart'
      See the note in ?`:::` about the use of this operator.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 44.5Mb
      sub-directories of 1Mb or more:
        doc  44.1Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    heatmapGene: no visible global function definition for
      'subsetByOverlaps'
    heatmapGene: no visible binding for global variable 'mae'
    motif.enrichment.plot: no visible binding for global variable 'y'
    motif.enrichment.plot: no visible binding for global variable 'x'
    motif.enrichment.plot: no visible binding for global variable 'z'
    motif.enrichment.plot: no visible binding for global variable 'upperOR'
    motif.enrichment.plot: no visible binding for global variable 'lowerOR'
    motif.enrichment.plot: no visible binding for global variable 'motif'
    motif.enrichment.plot: no visible binding for global variable 'OR'
    scatter: no visible binding for global variable 'value'
    scatter: no visible global function definition for 'cor.test'
    scatter: no visible binding for global variable 'mae'
    Undefined global functions or variables:
      Gene GeneID Hugo_Symbol OR Probe TF Target cor.test fisher.test gr
      hm450.hg38.manifest label lowerOR mae motif precede pvalue
      subsetByOverlaps upperOR value write.table x y z
    Consider adding
      importFrom("stats", "cor.test", "fisher.test")
      importFrom("utils", "write.table")
    to your NAMESPACE file.
    ```

*   checking for unstated dependencies in vignettes ... NOTE
    ```
    '::' or ':::' import not declared from: ‘devtools’
    ```

# extdplyr

Version: 0.1.4

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      5: group_by.data.frame(.data, !!!dots, add = add) at /Users/romainfrancois/git/tidyverse/dplyr/R/group-by.r:94
      6: grouped_df(groups$data, groups$group_names, .drop) at /Users/romainfrancois/git/tidyverse/dplyr/R/dataframe.R:34
      7: grouped_df_impl(data, unname(vars), drop) at /Users/romainfrancois/git/tidyverse/dplyr/R/grouped-df.r:20
      8: group_by_drop_default(.data) at /Users/romainfrancois/git/tidyverse/dplyr/R/dataframe.R:34
      9: group_by_drop_default.default(.data) at /Users/romainfrancois/git/tidyverse/dplyr/R/group-by.r:220
      10: group_data(.tbl) at /Users/romainfrancois/git/tidyverse/dplyr/R/group-by.r:225
      11: group_data.grouped_df(.tbl) at /Users/romainfrancois/git/tidyverse/dplyr/R/group_data.R:30
      12: group_data_grouped_df(.data) at /Users/romainfrancois/git/tidyverse/dplyr/R/group_data.R:47
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 19 SKIPPED: 0 FAILED: 1
      1. Error: ind_to_char_ works with grouped_df, tbl_df, tbl, data.frame (@test_grp_routine.R#74) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# FindMyFriends

Version: 1.10.0

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

# groupedstats

Version: 0.0.6

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    The error most likely occurred in:
    
    > ### Name: grouped_glm
    > ### Title: Function to run generalized linear model (glm) across multiple
    > ###   grouping variables.
    > ### Aliases: grouped_glm
    > 
    > ### ** Examples
    > 
    > 
    > # to get tidy output
    > groupedstats::grouped_glm(
    +   data = groupedstats::Titanic_full,
    +   formula = Survived ~ Sex,
    +   grouping.vars = Class,
    +   family = stats::binomial(link = "logit")
    + )
    Error in UseMethod("ungroup") : 
      no applicable method for 'ungroup' applied to an object of class "list"
    Calls: <Anonymous> ... freduce -> withVisible -> <Anonymous> -> <Anonymous>
    Execution halted
    ```

# healthcareai

Version: 2.3.0

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    
    > ### Name: explore
    > ### Title: Explore a model's "reasoning" via counterfactual predictions
    > ### Aliases: explore
    > 
    > ### ** Examples
    > 
    > # First, we need a model on which to make counterfactual predictions
    > set.seed(5176)
    > m <- machine_learn(pima_diabetes, patient_id, outcome = diabetes,
    +                    tune = FALSE, models = "xgb")
    Training new data prep recipe...
    
    Variable(s) ignored in prep_data won't be used to tune models: patient_id
    
    diabetes looks categorical, so training classification algorithms.
    
    After data processing, models are being trained on 12 features with 768 observations.
    Based on n_folds = 5 and hyperparameter settings, the following number of models will be trained: 5 xgb's 
    
    Training at fixed values: eXtreme Gradient Boosting
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

# kayadata

Version: 0.4.0

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘kayadata-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: project_top_down
    > ### Title: Get top-down projections of Kaya variables for a country or
    > ###   region for a given year
    > ### Aliases: project_top_down
    > 
    > ### ** Examples
    > 
    > project_top_down("China", 2037)
    Error in approx(x = year, y = ., xout = ytmp) : object 'ytmp' not found
    Calls: project_top_down ... summarise.tbl_df -> summarise_impl -> <Anonymous> -> approx
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 165-181 (policy_analysis.Rmd) 
    Error: processing vignette 'policy_analysis.Rmd' failed with diagnostics:
    object '.f' of mode 'function' was not found
    Execution halted
    ```

# merTools

Version: 0.4.1

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘merTools-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: REsim
    > ### Title: Simulate random effects from merMod 'REsim' simulates random
    > ###   effects from merMod object posterior distributions
    > ### Aliases: REsim
    > 
    > ### ** Examples
    > 
    > require(lme4)
    > m2 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
    > re2 <- REsim(m2, 25)
    Error in `[.data.frame`(dat, , c("groupFctr", "groupID", "term", "mean",  : 
      undefined columns selected
    Calls: REsim -> [ -> [.data.frame
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat-a_m.R’ failed.
    Last 13 lines of output:
      undefined columns selected
      1: expect_is(REsim(lmerSlope1, n.sims = 100), "data.frame") at testthat/test-merExtract.R:90
      2: quasi_label(enquo(object), label)
      3: eval_bare(get_expr(quo), get_env(quo))
      4: REsim(lmerSlope1, n.sims = 100)
      5: dat[, c("groupFctr", "groupID", "term", "mean", "median", "sd")]
      6: `[.data.frame`(dat, , c("groupFctr", "groupID", "term", "mean", "median", "sd"))
      7: stop("undefined columns selected")
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 358 SKIPPED: 20 FAILED: 1
      1. Error: REsim produces data.frames (@test-merExtract.R#90) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Loading required package: Rcpp
    rstanarm (Version 2.18.2, packaged: 2018-11-08 22:19:38 UTC)
    - Do not expect the default priors to remain the same in future rstanarm versions.
    Thus, R scripts should specify priors explicitly, even if they are just the defaults.
    - For execution on a local, multicore CPU with excess RAM we recommend calling
    options(mc.cores = parallel::detectCores())
    - Plotting theme set to bayesplot::theme_default().
    Warning in engine$weave(file, quiet = quiet, encoding = enc) :
      Pandoc (>= 1.12.3) and/or pandoc-citeproc not available. Falling back to R Markdown v1.
    ## 
    ## Amelia II: Multiple Imputation
    ## (Version 1.7.5, built: 2018-05-07)
    ## Copyright (C) 2005-2019 James Honaker, Gary King and Matthew Blackwell
    ## Refer to http://gking.harvard.edu/amelia/ for more information
    ## 
    Warning in engine$weave(file, quiet = quiet, encoding = enc) :
      Pandoc (>= 1.12.3) and/or pandoc-citeproc not available. Falling back to R Markdown v1.
    Quitting from lines 115-117 (merToolsIntro.Rmd) 
    Erreur : le traitement de la vignette 'merToolsIntro.Rmd' a échoué avec le diagnostic :
    undefined columns selected
    Exécution arrêtée
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

## Newly broken

*   R CMD check timed out
    

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.8Mb
      sub-directories of 1Mb or more:
        libs   5.4Mb
    ```

# perturbatr

Version: 1.0.0

## Newly broken

*   R CMD check timed out
    

# pmc

Version: 1.0.3

## In both

*   R CMD check timed out
    

# QuaternaryProd

Version: 1.14.0

## In both

*   R CMD check timed out
    

*   checking installed package size ... NOTE
    ```
      installed size is 17.0Mb
      sub-directories of 1Mb or more:
        extdata  16.2Mb
    ```

# radiant.basics

Version: 0.9.9

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘radiant.basics-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: compare_means
    > ### Title: Compare sample means
    > ### Aliases: compare_means
    > 
    > ### ** Examples
    > 
    > compare_means(diamonds, "cut", "price") %>% str()
    Error in me_calc(se, n, conf_lev) : could not find function "me_calc"
    Calls: %>% ... summarise -> summarise.tbl_df -> summarise_impl -> <Anonymous>
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      3: summarise(.tbl, !!!funs) at /Users/romainfrancois/git/tidyverse/dplyr/R/colwise-mutate.R:110
      4: summarise.tbl_df(.tbl, !!!funs) at /Users/romainfrancois/git/tidyverse/dplyr/R/manip.r:269
      5: summarise_impl(.data, dots, environment(), caller_env()) at /Users/romainfrancois/git/tidyverse/dplyr/R/tbl-df.r:102
      6: (structure(function (..., .x = ..1, .y = ..2, . = ..1) 
         mean(.) - comp_value, class = "rlang_lambda_function"))(age) at /Users/romainfrancois/git/tidyverse/dplyr/R/RcppExports.R:188
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 6 SKIPPED: 0 FAILED: 4
      1. Error: compare_means 1 (@test_stats.R#9) 
      2. Error: compare_means 2 (@test_stats.R#17) 
      3. Error: single_mean 1 (@test_stats.R#62) 
      4. Error: single_mean 2 (@test_stats.R#70) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# radiant.data

Version: 0.9.9

## Newly broken

*   checking examples ... ERROR
    ```
    ...
     $ byvar      : chr ""
     $ fun        : chr [1:2] "mean" "sd"
     $ top        : chr "fun"
     $ tabfilt    : chr ""
     $ tabsort    : chr ""
     $ nr         : NULL
     $ data_filter: chr ""
     - attr(*, "class")= chr [1:2] "explore" "list"
    > explore(diamonds, "price:x")$tab
      variable          fn1          fn2
    1    price 3.907186e+03 3956.9154001
    2    carat 7.942833e-01    0.4738263
    3  clarity 1.333333e-02    0.1147168
    4      cut 3.366667e-02    0.1803998
    5    color 1.273333e-01    0.3334016
    6    depth 6.175267e+01    1.4460279
    7    table 5.746533e+01    2.2411022
    8        x 5.721823e+00    1.1240545
    > explore(diamonds, c("price","carat"), byvar = "cut", fun = c("n_missing", "skew"))$tab
    Error: Each row of output must be identified by a unique combination of keys.
    Keys are shared for 20 rows:
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      9: function_list[[k]](value)
      10: spread(., "fun", "value")
      11: spread.data.frame(., "fun", "value")
      12: abort(glue("Each row of output must be identified by a unique combination of keys.", 
             "\nKeys are shared for {shared} rows:", "\n{rows}", "Do you need to create unique ID with tibble::rowid_to_column()?"))
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 27 SKIPPED: 0 FAILED: 4
      1. Failure: explore 8 x 2 (@test_funs.R#89) 
      2. Failure: explore 8 x 2 (@test_funs.R#90) 
      3. Failure: explore 1 x 2 (@test_funs.R#108) 
      4. Error: explore 2 x 2 x 2 (@test_funs.R#142) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# radiant.model

Version: 0.9.9

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘radiant.model-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: evalreg
    > ### Title: Evaluate the performance of different regression models
    > ### Aliases: evalreg
    > 
    > ### ** Examples
    > 
    > data.frame(price = diamonds$price, pred1 = rnorm(3000), pred2 = diamonds$price) %>%
    +   evalreg(pred = c("pred1", "pred2"), "price") %>%
    +   str()
    Error in mean((rv - .)^2, na.rm = TRUE) : object 'rv' not found
    Calls: %>% ... summarise.tbl_df -> summarise_impl -> <Anonymous> -> mean
    Execution halted
    ```

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

# survivalAnalysis

Version: 0.1.1

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    The following objects are masked from ‘package:stats’:
    
        filter, lag
    
    The following objects are masked from ‘package:base’:
    
        intersect, setdiff, setequal, union
    
    > survival::aml %>%
    +   analyse_survival(vars(time, status), x) %>%
    +   print
                    records n.max n.start events   *rmean *se(rmean) median 0.95LCL
    x=Maintained         11    11      11      7 41.96818  11.257627     31      18
    x=Nonmaintained      12    12      12     11 22.70833   4.180942     23       8
                    0.95UCL
    x=Maintained         NA
    x=Nonmaintained      NA
    Error in (function (..., .x = ..1, .y = ..2, . = ..1)  : 
      object 'timespanScaling' not found
    Calls: %>% ... mutate -> mutate.tbl_df -> mutate_impl -> <Anonymous>
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    ── Attaching packages ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──
    ✔ ggplot2 3.1.0          ✔ purrr   0.3.2     
    ✔ tibble  2.1.1          ✔ dplyr   0.8.0.9010
    ✔ tidyr   0.8.3          ✔ stringr 1.4.0     
    ✔ readr   1.3.1          ✔ forcats 0.4.0     
    ── Conflicts ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    Quitting from lines 79-84 (univariate.Rmd) 
    Error: processing vignette 'univariate.Rmd' failed with diagnostics:
    object 'timespanScaling' not found
    Execution halted
    ```

# TCGAbiolinks

Version: 2.8.4

## In both

*   R CMD check timed out
    

*   checking dependencies in R code ... WARNING
    ```
    '::' or ':::' import not declared from: ‘tidyr’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 72.6Mb
      sub-directories of 1Mb or more:
        R      2.4Mb
        data   3.5Mb
        doc   66.4Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    TCGAanalyze_networkInference: no visible global function definition for
      ‘minet’
    TCGAquery_recount2: no visible binding for global variable ‘rse_gene’
    TCGAtumor_purity: no visible binding for global variable ‘Tumor.purity’
    TCGAvisualize_SurvivalCoxNET: no visible global function definition for
      ‘dNetInduce’
    TCGAvisualize_SurvivalCoxNET: no visible global function definition for
      ‘dNetPipeline’
    TCGAvisualize_SurvivalCoxNET: no visible global function definition for
      ‘dCommSignif’
    TCGAvisualize_SurvivalCoxNET: no visible global function definition for
      ‘visNet’
    TCGAvisualize_oncoprint: no visible binding for global variable ‘value’
    readExonQuantification: no visible binding for global variable ‘exon’
    readExonQuantification: no visible binding for global variable
      ‘coordinates’
    Undefined global functions or variables:
      TabSubtypesCol_merged Tumor.purity barcode c3net clinical coordinates
      dCommSignif dNetInduce dNetPipeline exon knnmi.cross
      limmacontrasts.fit limmamakeContrasts minet portions rse_gene value
      visNet
    ```

# XBSeq

Version: 1.12.0

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

