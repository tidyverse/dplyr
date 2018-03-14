# unknown

## DiagrammeR (introduced in 89a6476b)

- `library(DiagrammeR); example(add_balanced_tree)`
- Pinged maintainer on https://github.com/tidyverse/dplyr/commit/89a6476b
- #3307

Version: 1.0.0

### Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### ** Examples
    > 
    > # Create a new graph and
    > # add 2 different types of
    > # balanced trees of height
    > # 2 (branching twice) and
    > # different branching ratios
    > graph <-
    +   create_graph() %>%
    +   add_balanced_tree(
    +     k = 2,
    +     h = 2,
    +     type = "binary") %>%
    +   add_balanced_tree(
    +     k = 3,
    +     h = 2,
    +     type = "tertiary")
    Error in `$<-.data.frame`(`*tmp*`, id, value = numeric(0)) : 
      replacement has 0 rows, data has 12
    Calls: %>% ... add_balanced_tree -> combine_graphs -> $<- -> $<-.data.frame
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 1926 SKIPPED: 0 FAILED: 24
      1. Error: Adding a balanced tree is possible (@test-add_graphs.R#61) 
      2. Error: Adding a cycle is possible (@test-add_graphs.R#144) 
      3. Error: Adding a path is possible (@test-add_graphs.R#281) 
      4. Error: Adding a prism is possible (@test-add_graphs.R#418) 
      5. Error: Adding a star is possible (@test-add_graphs.R#555) 
      6. Error: Adding a 2D grid is possible (@test-add_graphs.R#703) 
      7. Error: Adding a 3D grid is possible (@test-add_graphs.R#857) 
      8. Error: Adding a full graph is possible (@test-add_graphs.R#1357) 
      9. Error: Adding a G(n, m) Erdos-Renyi graph is possible (@test-add_graphs.R#1429) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

## mudata2 (introduced in 68d90b4d)

- `library(mudata2); example(ns_climate)`
- https://github.com/paleolimbot/mudata/issues/26

Version: 1.0.0

### Newly broken

*   checking examples ... ERROR
    ```
    ...
    
    > ns_climate %>% 
    +   select_locations(sable_island = starts_with("SABLE"),
    +                    nappan = starts_with("NAPPAN"), 
    +                    baddeck = starts_with("BADDECK")) %>% 
    +   select_params(ends_with("temp")) %>%
    +   filter_data(month(date) == 6) %>% 
    +   autoplot()
    Warning: Unknown or uninitialised column: 'param'.
    Warning: Unknown or uninitialised column: 'location'.
    Warning: Unknown or uninitialised column: 'dataset'.
    Not all values were found: SABLE ISLAND 6454, NAPPAN CDA 6414, BADDECK 6297
    Warning: Unknown or uninitialised column: 'param'.
    Warning: Unknown or uninitialised column: 'location'.
    Warning: Unknown or uninitialised column: 'dataset'.
    Warning: Unknown or uninitialised column: 'param'.
    Warning: Unknown or uninitialised column: 'location'.
    Warning: Unknown or uninitialised column: 'dataset'.
    Error in long_plot_base(.data, ...) : .data contains no data
    Calls: %>% ... autoplot -> autoplot.mudata -> long_ggplot -> long_plot_base
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      OGR: Unsupported geometry type
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 836 SKIPPED: 0 FAILED: 9
      1. Error: read/write JSON functions work (@test_mudata.io.R#141) 
      2. Error: read/write directory functions work (@test_mudata.io.R#435) 
      3. Failure: mudata objects subset properly (@test_mudata_subset.R#14) 
      4. Failure: mudata objects subset properly (@test_mudata_subset.R#23) 
      5. Failure: mudata objects subset properly (@test_mudata_subset.R#28) 
      6. Failure: mudata objects subset properly (@test_mudata_subset.R#30) 
      7. Error: recombined subsetted objects are the same as the original (@test_mudata_subset.R#72) 
      8. Error: filter_* functions work as expected (@test_mudata_subset.R#79) 
      9. Error: rename works when some values are factors (@test_rename.R#68) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Warning: Removed 85 rows containing missing values (geom_path).
    Quitting from lines 176-184 (mudata.Rmd) 
    Error: processing vignette 'mudata.Rmd' failed with diagnostics:
    object 'mean_temp' not found
    Execution halted
    ```

## nonmemica

- `library(nonmemica); example(nonmemica)`
- reason unclear
- e-mail sent

Version: 0.7.9

### Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### ** Examples
    > 
    > library(magrittr)
    > library(fold)
    
    Attaching package: ‘fold’
    
    The following object is masked from ‘package:stats’:
    
        filter
    
    > options(project = system.file('project/model',package='nonmemica'))
    > 1001 %>% fold(ID,TIME,subset='MDV==0') %>% head
    Warning in as.folded.data.frame(y) :
      removing unique values where keys are duplicated
    Warning in as.folded.data.frame(y) :
      removing unique values where keys are duplicated
    Error in `[.data.frame`(res, , c("VARIABLE", "META")) : 
      undefined columns selected
    Calls: %>% ... fold.character -> <Anonymous> -> meta.character -> [ -> [.data.frame
    Execution halted
    ```

## PPforest

- reason unclear
- e-mail sent

Version: 0.1.0

### Newly broken

*   checking examples ... ERROR
    ```
    ...
    Warning in PPclassify2(Tree.result = x[[1]], test.data = xnew, Rule = 1) :
      restarting interrupted promise evaluation
    Warning in PPclassify2(Tree.result = x[[1]], test.data = xnew, Rule = 1) :
      restarting interrupted promise evaluation
    Warning in PPclassify2(Tree.result = x[[1]], test.data = xnew, Rule = 1) :Warning in PPclassify2(Tree.result = x[[1]], test.data = xnew, Rule = 1) :
      restarting interrupted promise evaluation
    Warning in PPclassify2(Tree.result = x[[1]], test.data = xnew, Rule = 1) :
      restarting interrupted promise evaluation
    
      restarting interrupted promise evaluation
    Warning in PPclassify2(Tree.result = x[[1]], test.data = xnew, Rule = 1) :
      restarting interrupted promise evaluation
    Warning in PPclassify2(Tree.result = x[[1]], test.data = xnew, Rule = 1) :
      restarting interrupted promise evaluation
    Warning in PPclassify2(Tree.result = x[[1]], test.data = xnew, Rule = 1) :
      restarting interrupted promise evaluation
    Warning in PPclassify2(Tree.result = x[[1]], test.data = xnew, Rule = 1) :
      restarting interrupted promise evaluation
    Error in do.ply(i) : task 1 failed - "object 'Type' not found"
    Calls: PPforest ... trees_pred -> <Anonymous> -> llply -> <Anonymous> -> <Anonymous>
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
        nasa
    
    Loading required package: gridExtra
    
    Attaching package: 'gridExtra'
    
    The following object is masked from 'package:dplyr':
    
        combine
    
    Loading required package: PPtreeViz
    Loading required package: ggplot2
    Loading required package: partykit
    Loading required package: grid
    Loading required package: libcoin
    Loading required package: mvtnorm
    Loading required package: rpart
    Quitting from lines 155-160 (PPforest-vignette.Rmd) 
    Error: processing vignette 'PPforest-vignette.Rmd' failed with diagnostics:
    object 'Type' not found
    Execution halted
    ```

## rusk

- reason unclear
- e-mail sent

Version: 0.1

### Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘rusk-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: draw
    > ### Title: Beautiful graphical representation of multiplication tables
    > ### Aliases: draw
    > 
    > ### ** Examples
    > 
    > draw(table=2,modulo = 10, label=TRUE)
    Error in filter_impl(.data, quo) : 
      Evaluation error: object 'depart' not found.
    Calls: draw ... as.data.frame -> filter -> filter.tbl_df -> filter_impl -> .Call
    Execution halted
    ```

# dimensions

## pRoloc

- reason unclear
- e-mail sent

Version: 1.16.1

### Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
    This is pRoloc version 1.16.1 
      Read '?pRoloc' and references therein for information
      about the package and how to get started.
    
    
    This is pRolocdata version 1.14.0.
    Use 'pRolocdata()' to list available data sets.
    Loading required namespace: GO.db
    
    Loading required package: GO.db
    Retaining 85 out of 530 in GOAnnotations
    Retaining 80 out of 85 in GOAnnotations
    Warning in lapply(X = X, FUN = FUN, ...) :
      NaNs found in 'precision' with hyperparameters cost:8 sigma:0.1.
    Warning in lapply(X = X, FUN = FUN, ...) :
      NaNs found in 'precision' with hyperparameters cost:8 sigma:1.
    Quitting from lines 220-223 (pRoloc-transfer-learning.Rnw) 
    Error: processing vignette 'pRoloc-transfer-learning.Rnw' failed with diagnostics:
    incorrect number of dimensions
    Execution halted
    ```

## TPP

- reason unclear
- e-mail sent

Version: 3.4.3

### Newly broken

*   checking examples ... ERROR
    ```
    ...
    Removing duplicate identifiers using quality column 'qupm'...
    261 out of 261 rows kept for further analysis.
    Reformating data for input into function 'analyzeTPPCCR' ...
    Done.
    No output directory specified. No result files or plots will be produced.
    Looking for intensity column prefix: 'sumionarea_protein_'
    Computing fold changes...
    Done.
    Found the following column name in attr(data, 'importSettings')$proteinIdCol: 'representative'
    Found the following column name in attr(data, 'importSettings')$fcStr: 'rel_fc_protein_'
    Performing median normalization per temperature...
    Done.
    Looking for unique ID column: 'unique_ID'
    Looking for nonZeroCols: 'qusm'
    Checking which columns in the data table contain the fold change values for fitting and plotting...
    Normalized data columns detected with prefix 'norm_rel_fc_protein_'. Analysis will be based on these values.
    This information was found in the attributes of the input data (access with attr(dataTable, 'importSettings'))
    Performing TPP-CCR dose response curve fitting and generating result table...
    Error in foldChanges[, refCol] : incorrect number of dimensions
    Calls: analyze2DTPP ... withCallingHandlers -> analyzeTPPCCR -> tppccrNormalizeToReference
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 381 SKIPPED: 0 FAILED: 35
      1. Error: allOK (@test_analyze2DTPP.R#14) 
      2. Error: allOK_scientific_drug_concentration_format (@test_analyze2DTPP.R#37) 
      3. Error: warning_deprecated_fct_arg (@test_analyze2DTPP.R#62) 
      4. Error: NPARC_allok (@test_analyzeTPPTR.R#14) 
      5. Error: NPARC_allok_output (@test_analyzeTPPTR.R#34) 
      6. Error: NPARC_allok_plot (@test_analyzeTPPTR.R#61) 
      7. Error: NPARC_allok_files (@test_analyzeTPPTR.R#94) 
      8. Error: meltCurves_allOK_no_conditions (@test_analyzeTPPTR.R#153) 
      9. Error: testApplyCoeffs (@test_applyCoeffs.R#9) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking whether package ‘TPP’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘Biobase::exprs’ by ‘dplyr::exprs’ when loading ‘TPP’
    See ‘/home/muelleki/tmp/Rtmp8tFe4G/file182cc725e0494/TPP.Rcheck/00install.out’ for details.
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
    Filtering by annotation column(s) 'qssm' in treatment group: Panobinostat_1
      Column qssm between 4 and Inf-> 333 out of 508 proteins passed.
    
    333 out of 508 proteins passed in total.
    
    Filtering by annotation column(s) 'qssm' in treatment group: Panobinostat_2
      Column qssm between 4 and Inf-> 364 out of 509 proteins passed.
    
    364 out of 509 proteins passed in total.
    
    	2. Find jointP:
    Detecting intersect between treatment groups (jointP).
    -> JointP contains 261 proteins.
    
    	3. Filtering fold changes:
    Filtering fold changes in treatment group: Vehicle_1
    Quitting from lines 73-76 (NPARC_analysis_of_TPP_TR_data.Rnw) 
    Error: processing vignette 'NPARC_analysis_of_TPP_TR_data.Rnw' failed with diagnostics:
    incorrect number of dimensions
    Execution halted
    ```

# tidyselect problems

## breathtestcore

Version: 0.4.0

### Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      12: map_mold(.x, .f, logical(1), ...)
      13: vapply(.x, .f, .mold, ..., USE.NAMES = FALSE)
      14: df[keep_cols]
      15: `[.tbl_df`(df, keep_cols)
      16: check_names_df(i, x)
      17: check_names_df.default(i, x)
      18: stopc("Unsupported index type: ", class(j)[[1L]])
      19: abort(paste0(...))
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 350 SKIPPED: 4 FAILED: 1
      1. Error: Columns without names are renamed (@test_cleanup_data.R#72) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## desctable

- `example(desctable)`
- Use of `eval()` a `select()` statement in `desctable:::subTable`
- https://github.com/MaximeWack/desctable/issues/8

Version: 0.1.1

### Newly broken

*   checking examples ... ERROR
    ```
    ...
    +                                 mpg = "Miles per gallon"))
                         N      Mean        sd     Med       IQR
    1  Miles per gallon 32 20.090625 6.0269481      NA        NA
    2         Cylinders 32        NA        NA   6.000   4.00000
    3              disp 32        NA        NA 196.300 205.17500
    4       Horse Power 32        NA        NA 123.000  83.50000
    5              drat 32  3.596563 0.5346787      NA        NA
    6                wt 32        NA        NA   3.325   1.02875
    7              qsec 32 17.848750 1.7869432      NA        NA
    8                vs 32        NA        NA   0.000   1.00000
    9                am 32        NA        NA   0.000   1.00000
    10             gear 32        NA        NA   4.000   1.00000
    11             carb 32        NA        NA   2.000   2.00000
    > 
    > # With grouping on a factor
    > iris %>%
    +   group_by(Species) %>%
    +   desctable(stats = stats_default)
    Error in eval(grps[[1]]) : object 'Species' not found
    Calls: %>% ... vars_select_eval -> map_if -> map -> .Call -> .f -> eval -> eval
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    
    Attaching package: 'desctable'
    
    The following object is masked from 'package:DT':
    
        datatable
    
    The following objects are masked from 'package:stats':
    
        IQR, chisq.test, fisher.test
    
    Quitting from lines 217-222 (desctable.Rmd) 
    Error: processing vignette 'desctable.Rmd' failed with diagnostics:
    object 'Species' not found
    Execution halted
    ```

## InformativeCensoring

```
  data(ScoreInd)
  data(ScoreTimeDep)
  set.seed(25)
  
  time.dep <- MakeTimeDepScore(ScoreTimeDep,Id="Id",
                             time.start="start",
                             time.end="end")
  
  #ok if time dep not used 
  #note do not get same answer without timedep as still use separate
  #model fits for each censored observation if timedep is not NULL
  expect_warning(ans <- ScoreImpute(data=ScoreInd,event.model=~Z1+Z3+Z5,
                        col.control=col.headings(has.event="event",
                                                 time="time",
                                                 Id="Id",
                                                 arm="arm",
                                                 DCO.time="DCO.time",
                                                 to.impute="to.impute"),
                        NN.control=NN.options(NN=5,w.censoring = 0.2),
                        time.dep = time.dep,m=5,
                        bootstrap.strata=ScoreInd$arm))
```

- very likely #3307

Version: 0.3.4

### Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      15: lapply(X = ans[index], FUN = FUN, ...)
      16: FUN(X[[i]], ...)
      17: FUN(data[x, , drop = FALSE], ...)
      18: .imputeTimes(x, data[indices, ], event.model, censor.model, col.control, NN.control, 
             time.dep = time.dep, ...)
      19: data[, col.control$Id]
      20: `[.data.frame`(data, , col.control$Id)
      21: stop("undefined columns selected")
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 516 SKIPPED: 0 FAILED: 1
      1. Error: Sfn_time_dep (@test-scoreSystem.R#207) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Loading required package: survival
    
    Attaching package: 'InformativeCensoring'
    
    The following object is masked from 'package:survival':
    
        cox.zph
    
    Quitting from lines 289-297 (risk_score_imputation_Hsu_2009.Rnw) 
    Error: processing vignette 'risk_score_imputation_Hsu_2009.Rnw' failed with diagnostics:
    undefined columns selected
    Execution halted
    ```

# funs, vars

## keyholder

- `keyholder::key_by_all(iris)`
- fix in `R/funs.R`?

Version: 0.1.1

### Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘keyholder-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: key-by-scoped
    > ### Title: Key by selection of variables
    > ### Aliases: key-by-scoped key_by_all key_by_if key_by_at
    > 
    > ### ** Examples
    > 
    > mtcars %>% key_by_all(.funs = toupper)
    Error in .funs(.) : could not find function ".funs"
    Calls: %>% ... set_names -> set_names_impl -> is_function -> is_closure -> fun
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 293 SKIPPED: 0 FAILED: 10
      1.  Error: distinct works (@test-keyed-df-one-tbl.R#298) 
      2.  Error: key_by_all works (@test-scoped.R#16) 
      3.  Error: key_by_if works (@test-scoped.R#30) 
      4.  Error: key_by_at works (@test-scoped.R#44) 
      5.  Error: restore_keys_all works (@test-scoped.R#89) 
      6.  Error: restore_keys_if works (@test-scoped.R#114) 
      7.  Error: restore_keys_at works (@test-scoped.R#143) 
      8.  Error: rename_keys_all works (@test-scoped.R#172) 
      9.  Error: rename_keys_if works (@test-scoped.R#181) 
      10. Error: rename_keys_at works (@test-scoped.R#191) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 69-82 (introduction.Rmd) 
    Error: processing vignette 'introduction.Rmd' failed with diagnostics:
    could not find function ".funs"
    Execution halted
    ```

## neuropsychology

- `example(fa_loadings)`
- `select_vars()`
- Fixed by patch

Version: 0.5.0

### Newly broken

*   checking examples ... ERROR
    ```
    ...
    > require(psych)
    Loading required package: psych
    
    Attaching package: ‘psych’
    
    The following object is masked from ‘package:neuropsychology’:
    
        describe
    
    The following objects are masked from ‘package:ggplot2’:
    
        %+%, alpha
    
    > 
    > df <- select_numeric(personality)
    > fa <- psych::fa(df)
    > 
    > fa_loadings(fa)$max
    Error in is_null(vars) : argument "vars" is missing, with no default
    Calls: fa_loadings ... vars_select_eval -> scoped_vars -> poke_vars -> is_null
    Execution halted
    ```

# list

## fold

- likely responsible for *nonmemica* failures
- `example("fold.data.frame")`

Version: 0.2.5

### Newly broken

*   checking examples ... ERROR
    ```
    ...
    1   VARIABLE   META   ID   TIME
    > 
    > # another example
    > x <- Theoph
    > x %<>% mutate(
    +   conc_LABEL = 'theophylline concentration',
    +   conc_GUIDE = 'mg/L',
    +   Time_LABEL = 'time since drug administration',
    +   Time_GUIDE = 'hr',
    +   Time_HALF = Time / 2 # to demonstrate variant attribute of key column
    + )
    > x %<>% fold(Subject, Time)
    Warning in as.folded.data.frame(d, sort = sort, ...) :
      removing unique values where keys are duplicated
    > x %>% unfold %>% head
    Warning in is.na(x$META) :
      is.na() applied to non-(list or vector) of type 'NULL'
    Error in y[sapply(y, function(i) nrow(i) > 0)] : 
      invalid subscript type 'list'
    Calls: %>% ... _fseq -> freduce -> <Anonymous> -> unfold -> unfold.folded
    Execution halted
    ```

## mrgsolve

Version: 0.8.10

### Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### Aliases: as_deslist
    > 
    > ### ** Examples
    > 
    > idata <- dplyr::data_frame(ID=1:4, end=seq(24,96,24), delta=6,
    + add=list(c(122,124,135),c(111), c(99),c(88)))
    > 
    > idata <- dplyr::mutate(idata, GRP = ID %%2)
    > 
    > idata
    # A tibble: 4 x 5
         ID   end delta add         GRP
      <int> <dbl> <dbl> <list>    <dbl>
    1     1   24.    6. <dbl [3]>    1.
    2     2   48.    6. <dbl [1]>    0.
    3     3   72.    6. <dbl [1]>    1.
    4     4   96.    6. <dbl [1]>    0.
    > 
    > l <- as_deslist(idata,"GRP")
    Error: distinct() does not support columns of type `list`
    Execution halted
    ```

## sf

Version: 0.6-0

### Newly broken

*   checking examples ... ERROR
    ```
    ...
    The following objects are masked from ‘package:base’:
    
        intersect, setdiff, setequal, union
    
    > nc = st_read(system.file("shape/nc.shp", package="sf"))
    Reading layer `nc' from data source `/home/muelleki/tmp/Rtmp8tFe4G/filec0341962a9ee/sf.Rcheck/sf/shape/nc.shp' using driver `ESRI Shapefile'
    Simple feature collection with 100 features and 14 fields
    geometry type:  MULTIPOLYGON
    dimension:      XY
    bbox:           xmin: -84.32385 ymin: 33.88199 xmax: -75.45698 ymax: 36.58965
    epsg (SRID):    4267
    proj4string:    +proj=longlat +datum=NAD27 +no_defs
    > nc %>% filter(AREA > .1) %>% plot()
    Warning: plotting the first 10 out of 14 attributes; use max.plot = 14 to plot all
    > # plot 10 smallest counties in grey:
    > st_geometry(nc) %>% plot()
    > nc %>% select(AREA) %>% arrange(AREA) %>% slice(1:10) %>% plot(add = TRUE, col = 'grey')
    > title("the ten counties with smallest area")
    > nc[c(1:100, 1:10), ] %>% distinct() %>% nrow()
    Error: distinct() does not support columns of type `list`
    Execution halted
    ```

## xpose

Version: 0.4.2

### Newly broken

*   checking examples ... ERROR
    ```
    ...
       ID    SEX   MED1  MED2   DOSE   AMT    SS    II  TIME   TAD IPRED   CWRES
       <fct> <fct> <fct> <fct> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl>
     1 110   1     0     1      200.  100.    0.    0. 0.    0.    0.     0.    
     2 110   1     0     1      200.    0.    0.    0. 1.00  1.00  0.214 -2.83  
     3 110   1     0     1      200.    0.    0.    0. 1.50  1.50  0.296 -1.62  
     4 110   1     0     1      200.    0.    0.    0. 2.00  2.00  0.349  1.10  
     5 110   1     0     1      200.    0.    0.    0. 4.00  4.00  0.392  0.585 
     6 110   1     0     1      200.    0.    0.    0. 6.00  6.00  0.329 -0.0262
     7 110   1     0     1      200.    0.    0.    0. 8.00  8.00  0.249  0.0940
     8 112   1     1     1      200.  100.    0.    0. 0.    0.    0.     0.    
     9 112   1     1     1      200.    0.    0.    0. 0.500 0.500 0.556  0.0341
    10 112   1     1     1      200.    0.    0.    0. 1.00  1.00  0.712 -0.417 
    # ... with 540 more rows, and 19 more variables: CPRED <dbl>, IWRES <dbl>,
    #   EVID <dbl>, A1 <dbl>, A2 <dbl>, DV <dbl>, PRED <dbl>, RES <dbl>,
    #   WRES <dbl>, CLCR <dbl>, AGE <dbl>, WT <dbl>, KA <dbl>, CL <dbl>, V <dbl>,
    #   ALAG1 <dbl>, ETA1 <dbl>, ETA2 <dbl>, ETA3 <dbl>
    > 
    > # Tip to list available tables in the xpdb
    > print(xpdb_ex_pk)
    Error: distinct() does not support columns of type `list`
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      27: NextMethod()
      28: distinct.data.frame(.data, !(!(!dots)), .keep_all = .keep_all)
      29: distinct_vars(.data, quos(...), .keep_all = .keep_all)
      30: list_cols_error(.data, keep)
      31: abort("distinct() does not support columns of type `list`")
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 421 SKIPPED: 6 FAILED: 4
      1. Error: (unknown) (@test-console_outputs.R#4) 
      2. Error: (unknown) (@test-edits.R#17) 
      3. Error: (unknown) (@test-vpc.R#17) 
      4. Error: (unknown) (@test-xpdb_access.R#4) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 33-34 (access_xpdb_data.Rmd) 
    Error: processing vignette 'access_xpdb_data.Rmd' failed with diagnostics:
    distinct() does not support columns of type `list`
    Execution halted
    ```

# join failure

## rubias

- maybe #3307? Haven't checked.

Version: 0.1.0

### Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Doing LOO simulations rep 34 of 50
    Doing LOO simulations rep 35 of 50
    Doing LOO simulations rep 36 of 50
    Doing LOO simulations rep 37 of 50
    Doing LOO simulations rep 38 of 50
    Doing LOO simulations rep 39 of 50
    Doing LOO simulations rep 40 of 50
    Doing LOO simulations rep 41 of 50
    Doing LOO simulations rep 42 of 50
    Doing LOO simulations rep 43 of 50
    Doing LOO simulations rep 44 of 50
    Doing LOO simulations rep 45 of 50
    Doing LOO simulations rep 46 of 50
    Doing LOO simulations rep 47 of 50
    Doing LOO simulations rep 48 of 50
    Doing LOO simulations rep 49 of 50
    Doing LOO simulations rep 50 of 50
    Quitting from lines 438-448 (rubias-overview.Rmd) 
    Error: processing vignette 'rubias-overview.Rmd' failed with diagnostics:
    `by` can't contain join column `collection` which is missing from LHS
    Execution halted
    ```

# exprs

## biobroom

Version: 1.8.0

### Newly broken

*   checking whether package ‘biobroom’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘dplyr::exprs’ by ‘Biobase::exprs’ when loading ‘biobroom’
    See ‘/home/muelleki/tmp/Rtmp8tFe4G/file182cf70acbf91/biobroom.Rcheck/00install.out’ for details.
    ```

## switchde

Version: 1.2.0

### Newly broken

*   checking whether package ‘switchde’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘dplyr::exprs’ by ‘Biobase::exprs’ when loading ‘switchde’
    See ‘/home/muelleki/tmp/Rtmp8tFe4G/file182cb467e9ca/switchde.Rcheck/00install.out’ for details.
    ```

## IHWpaper

Version: 1.4.0

### Newly broken

*   checking whether package ‘IHWpaper’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘dplyr::exprs’ by ‘Biobase::exprs’ when loading ‘IHWpaper’
    See ‘/home/muelleki/tmp/Rtmp8tFe4G/file182ca33b1d350/IHWpaper.Rcheck/00install.out’ for details.
    ```
