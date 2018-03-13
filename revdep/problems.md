# abjutils

Version: 0.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘devtools’ ‘httr’
      All declared Imports should be used.
    ```

# ADPclust

Version: 0.7

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘fields’ ‘knitr’
      All declared Imports should be used.
    ```

# afex

Version: 0.18-0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘ez’, ‘ascii’
    ```

# afmToolkit

Version: 0.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘DBI’ ‘assertthat’ ‘tibble’
      All declared Imports should be used.
    ```

# aire.zmvm

Version: 0.5.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      testthat results ================================================================
      OK: 58 SKIPPED: 0 FAILED: 9
      1. Failure: .convert_time correctly parses string (@test_data.R#7) 
      2. Failure: .convert_time correctly parses string (@test_data.R#11) 
      3. Failure: .convert_time correctly parses string (@test_data.R#13) 
      4. Failure: .convert_time correctly parses string (@test_data.R#16) 
      5. Failure: .convert_time correctly parses string (@test_data.R#20) 
      6. Failure: .convert_time correctly parses string (@test_data.R#23) 
      7. Failure: .convert_time correctly parses string (@test_data.R#26) 
      8. Failure: .convert_time correctly parses string (@test_data.R#30) 
      9. Failure: .convert_time correctly parses string (@test_data.R#33) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 38 marked UTF-8 strings
    ```

# alphavantager

Version: 0.1.0

## Newly fixed

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(testthat)
      > library(alphavantager)
      > 
      > test_check("alphavantager")
      1. Error: call SECTOR (@test_av_get.R#38) --------------------------------------
      missing value where TRUE/FALSE needed
      1: av_get(symbol, av_fun) at testthat/test_av_get.R:38
      
      testthat results ================================================================
      OK: 11 SKIPPED: 0 FAILED: 1
      1. Error: call SECTOR (@test_av_get.R#38) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘devtools’
      All declared Imports should be used.
    ```

# ameco

Version: 0.2.8

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 15.7Mb
      sub-directories of 1Mb or more:
        data  15.6Mb
    ```

# ANLP

Version: 1.3

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 3597 marked UTF-8 strings
    ```

# annotatr

Version: 1.2.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Building 1to5kb upstream of TSS...
    Building intergenic...
    Building 5UTRs...
    Building 3UTRs...
    Building exons...
    Building introns...
    Building intron exon boundaries...
    snapshotDate(): 2017-04-25
    Building CpG islands...
    loading from cache '/home/muelleki//.AnnotationHub/5086'
    Building CpG shores...
    Building CpG shelves...
    Building inter-CpG-islands...
    Annotating...
    Randomizing regions...
    `summarise_each()` is deprecated.
    Use `summarise_all()`, `summarise_at()` or `summarise_if()` instead.
    To map `funs` over a selection of variables, use `summarise_at()`
    Error: processing vignette 'annotatr-vignette.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

*   checking R code for possible problems ... NOTE
    ```
    plot_coannotations: no visible binding for global variable ‘.’
    plot_numerical_coannotations: no visible binding for global variable
      ‘.’
    Undefined global functions or variables:
      .
    ```

# anomalyDetection

Version: 0.2.4

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘caret’
      All declared Imports should be used.
    ```

# archivist

Version: 2.1.2

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘archivist.github’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘rmarkdown’, ‘archivist.github’
    ```

# assertive.types

Version: 0.0-3

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘pryr’
    ```

# assertr

Version: 2.0.2.2

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Actual value: "Unknown column `tree` "
      
      
      2. Failure: assert_rows breaks appropriately (@test-assertions.R#334) ----------
      error$message does not match "All select\\(\\) inputs must resolve to integer column positions|\"tree\": must resolve to integer column positions, not string|Strings must match column names. Unkown columns: tree|Strings must match column names. Unknown columns: tree".
      Actual value: "Unknown column `tree` "
      
      
      testthat results ================================================================
      OK: 374 SKIPPED: 0 FAILED: 2
      1. Failure: assert breaks appropriately (@test-assertions.R#232) 
      2. Failure: assert_rows breaks appropriately (@test-assertions.R#334) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# auctestr

Version: 1.0.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tidyr’
      All declared Imports should be used.
    ```

# backtestGraphics

Version: 0.1.6

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      > library(testthat)
      > test_check("backtestGraphics")
      Loading required package: backtestGraphics
      1. Failure: stat_calculation function (@test_stat_calculation.R#8) -------------
      Failed the test for calculating summary statistics not equal to `truth.1`.
      Component "pnl": Component "pnl.drawdown": Component "start": Attributes: < Component "levels": 1 string mismatch >
      Component "pnl": Component "pnl.drawdown": Component "start": 1 string mismatch
      
      
      testthat results ================================================================
      OK: 9 SKIPPED: 0 FAILED: 1
      1. Failure: stat_calculation function (@test_stat_calculation.R#8) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# ballr

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘XML’ ‘devtools’ ‘ggplot2’ ‘scales’
      All declared Imports should be used.
    ```

# banR

Version: 0.2.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      1. Error: Geocode tbl works  (@test_geocodetbl.R#16) ---------------------------
      The API sent back an error 503
      1: expect_is(object = banR::geocode_tbl(tbl = table_test, adresse = x, code_postal = y), 
             class = "tbl_df") at testthat/test_geocodetbl.R:16
      2: klass(object)
      3: paste(class(x), collapse = "/")
      4: banR::geocode_tbl(tbl = table_test, adresse = x, code_postal = y)
      5: stop("The API sent back an error ", httr::status_code(query_results))
      
      testthat results ================================================================
      OK: 6 SKIPPED: 0 FAILED: 1
      1. Error: Geocode tbl works  (@test_geocodetbl.R#16) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘stringr’
      All declared Imports should be used.
    ```

# basictabler

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# bayesplot

Version: 1.4.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.8Mb
      sub-directories of 1Mb or more:
        R     1.6Mb
        doc   3.6Mb
    ```

# billboard

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tibble’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 660 marked UTF-8 strings
    ```

# biobroom

Version: 1.8.0

## Newly broken

*   checking whether package ‘biobroom’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘dplyr::exprs’ by ‘Biobase::exprs’ when loading ‘biobroom’
    See ‘/home/muelleki/git/R/dplyr/revdep/checks/biobroom/new/biobroom.Rcheck/00install.out’ for details.
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    'library' or 'require' call to ‘DESeq2’ in package code.
      Please use :: or requireNamespace() instead.
      See section 'Suggested packages' in the 'Writing R Extensions' manual.
    Missing or unexported object: ‘dplyr::tbl_dt’
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      for ‘colData’
    tidy.deSet: no visible global function definition for ‘exprs<-’
    tidy.deSet: no visible binding for global variable ‘value’
    tidy.deSet: no visible binding for global variable ‘gene’
    tidy.deSet: no visible global function definition for ‘pData’
    tidy.qvalue: no visible binding for global variable ‘smoothed’
    tidy.qvalue: no visible binding for global variable ‘pi0’
    tidy.qvalue: no visible binding for global variable ‘lambda’
    tidy_matrix: no visible binding for global variable ‘value’
    tidy_matrix: no visible binding for global variable ‘gene’
    Undefined global functions or variables:
      . DGEList calcNormFactors colData counts design end estimate
      estimateSizeFactors exprs<- fData<- gene gr is lambda model.matrix
      p.adjust pData pData<- pi0 protein rowRanges sample.id seqnames
      setNames smoothed start tbl_dt term value voom voomWithQualityWeights
    Consider adding
      importFrom("methods", "is")
      importFrom("stats", "end", "model.matrix", "p.adjust", "setNames",
                 "start")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

# bioCancer

Version: 1.4.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Error: processing vignette 'bioCancer.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Depends: includes the non-default packages:
      ‘magrittr’ ‘ggplot2’ ‘lubridate’ ‘tidyr’ ‘cgdsr’ ‘RCurl’ ‘XML’
    Adding so many packages to the search path is excessive and importing
    selectively is preferable.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 22.3Mb
      sub-directories of 1Mb or more:
        base        6.9Mb
        bioCancer   3.1Mb
        doc         2.8Mb
        quant       7.7Mb
    ```

# BiocFileCache

Version: 1.0.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Auto-disconnecting SQLiteConnection
    Auto-disconnecting SQLiteConnection
    Auto-disconnecting SQLiteConnection
    Auto-disconnecting SQLiteConnection
    Auto-disconnecting SQLiteConnection
    Auto-disconnecting SQLiteConnection
    Auto-disconnecting SQLiteConnection
    Auto-disconnecting SQLiteConnection
    Auto-disconnecting SQLiteConnection
    Auto-disconnecting SQLiteConnection
    Auto-disconnecting SQLiteConnection
    Auto-disconnecting SQLiteConnection
    Auto-disconnecting SQLiteConnection
    Auto-disconnecting SQLiteConnection
    Auto-disconnecting SQLiteConnection
    Auto-disconnecting SQLiteConnection
    Auto-disconnecting SQLiteConnection
    Auto-disconnecting SQLiteConnection
    Error: processing vignette 'BiocFileCache.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

# biomartr

Version: 0.5.2

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
             download_url, "' currently available?", call. = FALSE)
      
      trying URL 'ftp://ftp.ncbi.nlm.nih.gov/genomes/GENOME_REPORTS/overview.txt'
      Content type 'unknown' length 2921997 bytes (2.8 MB)
      ==================================================
      trying URL 'ftp://ftp.ncbi.nlm.nih.gov/genomes/genbank/metagenomes/assembly_summary.txt'
      Content type 'unknown' length 362999 bytes (354 KB)
      ==================================================
      testthat results ================================================================
      OK: 40 SKIPPED: 0 FAILED: 1
      1. Error: The getAssemblyStats() downloads assembly stats file and reads raw
                input: NCBI Genbank .. (@test-getAssemblyStats.R#34) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# biotmle

Version: 1.0.4

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘Matrix’ ‘SuperLearner’ ‘biotmleData’
      All declared Imports should be used.
    ```

# blkbox

Version: 1.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘bigrf’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘glmnet’ ‘gtools’ ‘knitr’ ‘nnet’ ‘parallel’ ‘rJava’ ‘reshape’
      ‘rmarkdown’ ‘shinyjs’
      All declared Imports should be used.
    Missing or unexported object: ‘xgboost::predict’
    ```

# bmlm

Version: 1.3.3

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 32.9Mb
      sub-directories of 1Mb or more:
        libs  32.6Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘methods’
      All declared Imports should be used.
    ```

# bodenmiller

Version: 0.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.9Mb
      sub-directories of 1Mb or more:
        data   8.7Mb
    ```

# bomrang

Version: 0.0.8

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      [2055] "IDE00437.201711152040.tif" "IDE00437.201711152050.tif"
      [2057] "IDE00437.201711152100.tif" "IDE00437.201711152110.tif"
      [2059] "IDE00437.201711152120.tif" "IDE00437.201711152130.tif"
      [2061] "IDE00437.201711152140.tif" "IDE00437.201711152150.tif"
      2. Error: get_available_imagery functions properly (@test-get_satellite_imagery.R#21) 
      argument is of length zero
      1: get_satellite_imagery(product_id = "IDE00425", scans = 1, cache = TRUE) at testthat/test-get_satellite_imagery.R:21
      
      testthat results ================================================================
      OK: 102 SKIPPED: 0 FAILED: 2
      1. Error: caching utils list files in cache and delete when asked (@test-check_cache.R#47) 
      2. Error: get_available_imagery functions properly (@test-get_satellite_imagery.R#21) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# bossMaps

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘rgdal’ ‘tidyr’
      All declared Imports should be used.
    ```

# BradleyTerryScalable

Version: 0.1.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.5Mb
      sub-directories of 1Mb or more:
        libs   6.1Mb
    ```

# brazilmaps

Version: 0.1.0

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

Version: 0.4.0

## Newly broken

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
      
      testthat results ================================================================
      OK: 359 SKIPPED: 0 FAILED: 1
      1. Error: Columns without names are renamed (@test_cleanup_data.R#72) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘breathteststan’
    ```

# breathteststan

Version: 0.4.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 28.9Mb
      sub-directories of 1Mb or more:
        libs  28.7Mb
    ```

# broom

Version: 0.4.2

## In both

*   checking examples ... ERROR
    ```
    ...
    +   f2 <- Finance[1:300, "hml"] - rf
    +   f3 <- Finance[1:300, "smb"] - rf
    +   h <- cbind(f1, f2, f3)
    +   res2 <- gmm(z ~ f1 + f2 + f3, x = h)
    +   
    +   td2 <- tidy(res2, conf.int = TRUE)
    +   td2
    +   
    +   # coefficient plot
    +   td2 %>%
    +     mutate(variable = reorder(variable, estimate)) %>%
    +     ggplot(aes(estimate, variable)) +
    +     geom_point() +
    +     geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
    +     facet_wrap(~ term) +
    +     geom_vline(xintercept = 0, color = "red", lty = 2)
    + }
    Error in `colnames<-`(`*tmp*`, value = c("conf.low", "conf.high")) : 
      attempt to set 'colnames' on an object with less than two dimensions
    Calls: tidy -> tidy.gmm -> process_lm -> colnames<-
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Complete output:
      > library(testthat)
      > test_check("broom")
      Loading required package: broom
      Error in lahman_df() : could not find function "lahman_df"
      Calls: test_check ... with_reporter -> force -> source_file -> eval -> eval -> tbl
      testthat results ================================================================
      OK: 621 SKIPPED: 0 FAILED: 0
      Execution halted
    ```

# bsam

Version: 1.1.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘rworldxtra’ ‘sp’
      All declared Imports should be used.
    ```

# BubbleTree

Version: 2.6.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Warning in get_engine(options$engine) :
      Unknown language engine 'rr' (must be registered via knit_engines$set()).
    Warning in get_engine(options$engine) :
      Unknown language engine 'rr' (must be registered via knit_engines$set()).
    Warning in get_engine(options$engine) :
      Unknown language engine 'rr' (must be registered via knit_engines$set()).
    Error: processing vignette 'BubbleTree-vignette.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 25.4Mb
      sub-directories of 1Mb or more:
        data  22.9Mb
        doc    2.2Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    annoByOverlap,Annotate: no visible binding for global variable
      'queryHits'
    Undefined global functions or variables:
      queryHits
    ```

# caffsim

Version: 0.2.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘markdown’
      All declared Imports should be used.
    ```

# canvasXpress

Version: 1.17.4

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘magrittr’
      All declared Imports should be used.
    ```

# cbsodataR

Version: 0.2.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Retrieving tables from http://opendata.cbs.nl/ODataCatalog/Tables?$format=json&$filter=(Language%20eq%20'en')
    Quitting from lines 29-34 (cbsodata.Rmd) 
    Error: processing vignette 'cbsodata.Rmd' failed with diagnostics:
    Required package curl not found. Please run: install.packages('curl')
    Execution halted
    ```

# cdcfluview

Version: 0.7.0

## In both

*   checking whether package ‘cdcfluview’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/home/muelleki/git/R/dplyr/revdep/checks/cdcfluview/new/cdcfluview.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘cdcfluview’ ...
** package ‘cdcfluview’ successfully unpacked and MD5 sums checked
** R
** data
*** moving datasets to lazyload DB
** preparing package for lazy loading
Error in seq.int(0, to0 - from, by) : 'to' must be a finite number
Error : unable to load R code in package ‘cdcfluview’
ERROR: lazy loading failed for package ‘cdcfluview’
* removing ‘/home/muelleki/git/R/dplyr/revdep/checks/cdcfluview/new/cdcfluview.Rcheck/cdcfluview’

```
### CRAN

```
* installing *source* package ‘cdcfluview’ ...
** package ‘cdcfluview’ successfully unpacked and MD5 sums checked
** R
** data
*** moving datasets to lazyload DB
** preparing package for lazy loading
Error in seq.int(0, to0 - from, by) : 'to' must be a finite number
Error : unable to load R code in package ‘cdcfluview’
ERROR: lazy loading failed for package ‘cdcfluview’
* removing ‘/home/muelleki/git/R/dplyr/revdep/checks/cdcfluview/old/cdcfluview.Rcheck/cdcfluview’

```
# cellscape

Version: 1.0.0

## In both

*   checking Rd \usage sections ... WARNING
    ```
    Duplicated \argument entries in documentation object 'dfs_tree':
      ‘chrom_bounds’ ‘ncols’ ‘chrom_bounds’ ‘cnv_data’ ‘chrom_bounds’
      ‘n_bp_per_pixel’ ‘mut_data’ ‘width’ ‘height’ ‘mutations’ ‘height’
      ‘width’ ‘clonal_prev’ ‘tree_edges’ ‘alpha’ ‘clonal_prev’ ‘tree_edges’
      ‘genotype_position’ ‘clone_colours’ ‘perturbations’ ‘mutations’
      ‘tree_edges’ ‘clonal_prev’ ‘clonal_prev’ ‘tree_edges’ ‘clone_colours’
      ‘mutations’
    
    Functions with \usage entries need to have the appropriate \alias
    entries, and all their arguments documented.
    The \usage entries must correspond to syntactically valid R code.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 76-92 (cellscape_vignette.Rmd) 
    Error: processing vignette 'cellscape_vignette.Rmd' failed with diagnostics:
    there is no package called 'devtools'
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘plyr’
      All declared Imports should be used.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    getMutOrder: no visible binding for global variable ‘VAF’
    getMutOrder: no visible global function definition for ‘lm’
    getMutOrder: no visible binding for global variable ‘na.omit’
    getMutOrder: no visible global function definition for ‘coef’
    getMutationsData: no visible binding for global variable
      ‘show_warnings’
    getTargetedHeatmapForEachSC: no visible binding for global variable
      ‘single_cell_id’
    getTargetedHeatmapForEachSC: no visible binding for global variable
      ‘chr’
    getTargetedHeatmapForEachSC: no visible binding for global variable
      ‘coord’
    Undefined global functions or variables:
      VAF chr chrom_index coef combn coord copy_number cumsum_values dist
      genotype hclust lm melt mode_cnv n n_gt na.omit px px_width sc_id
      setNames show_warnings single_cell_id site timepoint
    Consider adding
      importFrom("stats", "coef", "dist", "hclust", "lm", "na.omit",
                 "setNames")
      importFrom("utils", "combn")
    to your NAMESPACE file.
    ```

*   checking for unstated dependencies in vignettes ... NOTE
    ```
    'library' or 'require' call not declared from: ‘devtools’
    ```

# cepR

Version: 0.1.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 287 marked UTF-8 strings
    ```

# childsds

Version: 0.6.7

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘gamlss.dist’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 20 marked UTF-8 strings
    ```

# ChIPseeker

Version: 1.12.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    DOSE v3.2.0  For help: https://guangchuangyu.github.io/DOSE
    
    If you use DOSE in published research, please cite:
    Guangchuang Yu, Li-Gen Wang, Guang-Rong Yan, Qing-Yu He. DOSE: an R/Bioconductor package for Disease Ontology Semantic and Enrichment analysis. Bioinformatics 2015, 31(4):608-609
    
    clusterProfiler v3.4.4  For help: https://guangchuangyu.github.io/clusterProfiler
    
    If you use clusterProfiler in published research, please cite:
    Guangchuang Yu., Li-Gen Wang, Yanyan Han, Qing-Yu He. clusterProfiler: an R package for comparing biological themes among gene clusters. OMICS: A Journal of Integrative Biology. 2012, 16(5):284-287.
    ReactomePA v1.20.2  For help: https://guangchuangyu.github.io/ReactomePA
    
    If you use ReactomePA in published research, please cite:
    Guangchuang Yu, Qing-Yu He. ReactomePA: an R/Bioconductor package for reactome pathway analysis and visualization. Molecular BioSystems 2016, 12(2):477-479
    ChIPseeker v1.12.1  For help: https://guangchuangyu.github.io/ChIPseeker
    
    If you use ChIPseeker in published research, please cite:
    Guangchuang Yu, Li-Gen Wang, Qing-Yu He. ChIPseeker: an R/Bioconductor package for ChIP peak annotation, comparison and visualization. Bioinformatics 2015, 31(14):2382-2383
    'select()' returned 1:many mapping between keys and columns
    Error: processing vignette 'ChIPseeker.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

# chromer

Version: 0.1

## In both

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Malformed Description field: should contain one or more complete sentences.
    ```

*   checking R code for possible problems ... NOTE
    ```
    parse_counts: no visible global function definition for ‘na.omit’
    Undefined global functions or variables:
      na.omit
    Consider adding
      importFrom("stats", "na.omit")
    to your NAMESPACE file.
    ```

# CINdex

Version: 1.4.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
    Loading required package: org.Hs.eg.db
    
    Loading required package: TxDb.Hsapiens.UCSC.hg19.knownGene
    Warning: call dbDisconnect() when finished working with a connection
    Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
      Running 'texi2dvi' on 'CINdex.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `biblatex.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    
    ! Emergency stop.
    <read *> 
             
    l.76 \usepackage
                    [english]{babel}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    Calls: buildVignettes -> texi2pdf -> texi2dvi
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 18.4Mb
      sub-directories of 1Mb or more:
        data  17.7Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    comp.heatmap: no visible binding for global variable ‘dataMatrix’
    process.probe.anno: no visible binding for global variable ‘ID’
    process.probe.anno: no visible binding for global variable ‘midpoint’
    process.reference.genome: no visible binding for global variable
      ‘chrom’
    process.reference.genome: no visible binding for global variable ‘name’
    process.reference.genome: no visible binding for global variable
      ‘stain’
    Undefined global functions or variables:
      ID chrom dataMatrix midpoint name stain
    ```

# clustermq

Version: 0.8.0

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘rzmq’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# clustRcompaR

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tidyr’
      All declared Imports should be used.
    ```

# CNPBayes

Version: 1.6.1

## In both

*   checking for code/documentation mismatches ... WARNING
    ```
    Codoc mismatches from documentation object 'labelSwitching':
    labelSwitching
      Code: function(object, ...)
      Docs: function(object, merge = TRUE)
      Argument names in code not in docs:
        ...
      Argument names in docs not in code:
        merge
      Mismatches in argument names:
        Position: 2 Code: ... Docs: merge
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 12.7Mb
      sub-directories of 1Mb or more:
        libs  10.6Mb
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Package listed in more than one of Depends, Imports, Suggests, Enhances:
      ‘GenomicRanges’
    A package should be listed in only one of these fields.
    ```

*   checking R code for possible problems ... NOTE
    ```
    consensusRegion: no visible global function definition for
      ‘elementLengths’
    Undefined global functions or variables:
      elementLengths
    ```

# codingMatrices

Version: 0.3.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 478-486 (codingMatrices.Rnw) 
    Error: processing vignette 'codingMatrices.Rnw' failed with diagnostics:
    xtable not installed.
    Execution halted
    ```

# cogena

Version: 1.10.0

## In both

*   checking whether package ‘cogena’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘class::somgrid’ by ‘kohonen::somgrid’ when loading ‘cogena’
      Warning: replacing previous import ‘dplyr::exprs’ by ‘Biobase::exprs’ when loading ‘cogena’
    See ‘/home/muelleki/git/R/dplyr/revdep/checks/cogena/new/cogena.Rcheck/00install.out’ for details.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.0Mb
      sub-directories of 1Mb or more:
        doc       1.9Mb
        extdata   3.1Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    corInCluster,cogena: no visible global function definition for ‘cor’
    heatmapCluster,cogena: no visible global function definition for
      ‘topo.colors’
    heatmapCluster,cogena: no visible global function definition for
      ‘rainbow’
    heatmapCluster,cogena: no visible global function definition for ‘par’
    heatmapCluster,cogena: no visible global function definition for
      ‘legend’
    Undefined global functions or variables:
      abline as.dist axis cor data density dist hist image layout legend
      lines median mtext order.dendrogram p.adjust par phyper plot.new
      rainbow rect reorder sd text title topo.colors
    Consider adding
      importFrom("grDevices", "rainbow", "topo.colors")
      importFrom("graphics", "abline", "axis", "hist", "image", "layout",
                 "legend", "lines", "mtext", "par", "plot.new", "rect",
                 "text", "title")
      importFrom("stats", "as.dist", "cor", "density", "dist", "median",
                 "order.dendrogram", "p.adjust", "phyper", "reorder", "sd")
      importFrom("utils", "data")
    to your NAMESPACE file.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘clValid’
    ```

# CollapseLevels

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘magrittr’
      All declared Imports should be used.
    ```

# collapsibleTree

Version: 0.1.6

## In both

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      ‘knitr’ ‘shiny’
    ```

# comtradr

Version: 0.1.0

## Newly fixed

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 93-97 (comtradr-vignette.Rmd) 
    Error: processing vignette 'comtradr-vignette.Rmd' failed with diagnostics:
    too few arguments
    Execution halted
    ```

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      1. Error: correct api vals given: 1 reporter, 2 partners, all trade directions, annual, only shrimp (@test-ct_search.R#51) 
      too few arguments
      1: ct_search(reporters = "USA", partners = c("Germany", "Thailand"), trade_direction = "exports", 
             freq = "annual", start_date = "2011-01-01", end_date = "2015-01-01", commod_codes = shrimp_codes) at testthat/test-ct_search.R:51
      2: execute_api_request(url)
      3: stop(sprintf("Comtrade API request failed, with status code [%s]\n%s", httr::status_code(res)), 
             call. = FALSE)
      4: sprintf("Comtrade API request failed, with status code [%s]\n%s", httr::status_code(res))
      
      testthat results ================================================================
      OK: 51 SKIPPED: 0 FAILED: 1
      1. Error: correct api vals given: 1 reporter, 2 partners, all trade directions, annual, only shrimp (@test-ct_search.R#51) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# congressbr

Version: 0.1.1

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked UTF-8 string
    ```

# countyfloods

Version: 0.1.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    To use this package, you must install the hurricaneexposuredata
    package. To install that package, run
    `install.packages('hurricaneexposuredata',
    repos='https://geanders.github.io/drat/', type='source')`. See the
    `hurricaneexposure` vignette for more details.
    To use this function, you must have the `hurricaneexposuredata`
    package installed. See the `hurricaneexposure` package vignette
    for more details.
    Quitting from lines 311-313 (countyflood.Rmd) 
    Error: processing vignette 'countyflood.Rmd' failed with diagnostics:
    there is no package called 'hurricaneexposuredata'
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘maps’
      All declared Imports should be used.
    ```

# countyweather

Version: 0.1.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked UTF-8 string
    ```

# CRANsearcher

Version: 1.0.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 11 marked Latin-1 strings
      Note: found 57 marked UTF-8 strings
    ```

# crawl

Version: 2.1.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.1Mb
      sub-directories of 1Mb or more:
        libs   6.5Mb
    ```

# crosswalkr

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# ctsGE

Version: 1.2.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
        html_document, md_document, pdf_document
    
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Using tags as id variables
    Using tags as id variables
    Using tags as id variables
    Using tags as id variables
    Error: processing vignette 'ctsGE.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

# curatedMetagenomicData

Version: 1.2.2

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
      The data you have provided does not have
    any singletons. This is highly suspicious. Results of richness
    estimates (for example) are probably unreliable, or wrong, if you have already
    trimmed low-abundance taxa from the data.
    
    We recommended that you find the un-trimmed data and retry.
    Warning in graphics:::plotHclust(n1, merge, height, order(x$order), hang,  :
      "method" is not a graphical parameter
    Warning in graphics:::plotHclust(n1, merge, height, order(x$order), hang,  :
      "method" is not a graphical parameter
    Warning in axis(2, at = pretty(range(height)), ...) :
      "method" is not a graphical parameter
    Warning in title(main = main, sub = sub, xlab = xlab, ylab = ylab, ...) :
      "method" is not a graphical parameter
    Warning in replayPlot(x) : "method" is not a graphical parameter
    Warning in replayPlot(x) : "method" is not a graphical parameter
    Warning in replayPlot(x) : "method" is not a graphical parameter
    Warning in replayPlot(x) : "method" is not a graphical parameter
    Error: processing vignette 'curatedMetagenomicData.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Depends: includes the non-default packages:
      ‘dplyr’ ‘phyloseq’ ‘Biobase’ ‘ExperimentHub’ ‘AnnotationHub’
      ‘magrittr’
    Adding so many packages to the search path is excessive and importing
    selectively is preferable.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  8.6Mb
      sub-directories of 1Mb or more:
        help   7.9Mb
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Package listed in more than one of Depends, Imports, Suggests, Enhances:
      ‘BiocInstaller’
    A package should be listed in only one of these fields.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘BiocInstaller’
      All declared Imports should be used.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ExpressionSet2MRexperiment: no visible global function definition for
      ‘AnnotatedDataFrame’
    ExpressionSet2MRexperiment: no visible global function definition for
      ‘phenoData’
    curatedMetagenomicData : <anonymous>: no visible global function
      definition for ‘exprs<-’
    Undefined global functions or variables:
      AnnotatedDataFrame exprs<- phenoData
    ```

*   checking Rd files ... NOTE
    ```
    prepare_Rd: HMP_2012.Rd:540-542: Dropping empty section \seealso
    prepare_Rd: KarlssonFH_2013.Rd:90-92: Dropping empty section \seealso
    prepare_Rd: LeChatelierE_2013.Rd:86-88: Dropping empty section \seealso
    prepare_Rd: LomanNJ_2013_Hi.Rd:82-84: Dropping empty section \seealso
    prepare_Rd: LomanNJ_2013_Mi.Rd:82-84: Dropping empty section \seealso
    prepare_Rd: NielsenHB_2014.Rd:94-96: Dropping empty section \seealso
    prepare_Rd: Obregon_TitoAJ_2015.Rd:94-96: Dropping empty section \seealso
    prepare_Rd: OhJ_2014.Rd:86-88: Dropping empty section \seealso
    prepare_Rd: QinJ_2012.Rd:106-108: Dropping empty section \seealso
    prepare_Rd: QinN_2014.Rd:94-96: Dropping empty section \seealso
    prepare_Rd: RampelliS_2015.Rd:90-92: Dropping empty section \seealso
    prepare_Rd: TettAJ_2016.Rd:184-186: Dropping empty section \seealso
    prepare_Rd: ZellerG_2014.Rd:94-96: Dropping empty section \seealso
    ```

# d3r

Version: 0.7.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Failed with error:  'there is no package called 'V8''
      2. Error: d3_v8 has d3 (@test_v8.R#9) ------------------------------------------
      The V8 package must be installed for this function.  Please install.packages('V8').
      1: expect_identical(d3_v8()$get("global.d3.version"), d3_dep_v4()$version) at testthat/test_v8.R:9
      2: identical(object, expected)
      3: d3_v8()
      4: stop("The V8 package must be installed for this function.  Please install.packages('V8').")
      
      testthat results ================================================================
      OK: 11 SKIPPED: 3 FAILED: 2
      1. Error: d3-jetpack on latest release (@test_deps.R#47) 
      2. Error: d3_v8 has d3 (@test_v8.R#9) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      ‘igraph’ ‘partykit’ ‘treemap’ ‘V8’
    ```

# d3Tree

Version: 0.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘magrittr’
      All declared Imports should be used.
    ```

# dat

Version: 0.3.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      tests/testthat/test-summarise.r:963:1: style: lines should not be more than 100 characters.
        expect_identical(summarise(group_by(mtcars, cyl), x = n(), z = x)[2:3], tibble(x = c(11L, 7L, 14L), z = x))
      ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      tests/testthat/test-summarise.r:987:1: style: lines should not be more than 100 characters.
        expect_error(summarise(gdf, out = !! 1:5), "must be length 2 (the number of groups)", fixed = TRUE)
      ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      
      testthat results ================================================================
      OK: 108 SKIPPED: 0 FAILED: 1
      1. Failure: Package Style (@test-lintr.R#5) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# datadr

Version: 0.8.6

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘Rhipe’
    ```

# dataRetrieval

Version: 2.7.3

## In both

*   R CMD check timed out
    

# datasus

Version: 0.4.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Quitting from lines 78-85 (Introduction_to_datasus.Rmd) 
    Error: processing vignette 'Introduction_to_datasus.Rmd' failed with diagnostics:
    Timeout was reached: Connection timed out after 10000 milliseconds
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘RCurl’
      All declared Imports should be used.
    ```

# dbplyr

Version: 1.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tibble’
      All declared Imports should be used.
    ```

# DeepBlueR

Version: 1.2.10

## In both

*   checking examples ... ERROR
    ```
    ...
    + 
    +     experiment_names = deepblue_extract_names(experiments_list)
    +     histones_datasets[[epigenetic_marks[[i]]]] = experiment_names
    + }
    Called method: deepblue_list_experiments
    Reported status was: okay
    Called method: deepblue_list_experiments
    Reported status was: okay
    Called method: deepblue_list_experiments
    Reported status was: okay
    > 
    > deepblue_enrich_region_overlap(
    +   query_id=filtered_query_id,
    +   background_query=rg_10kb_tilling,
    +   datasets=histones_datasets,
    +   genome="grch38")
    Called method: deepblue_enrich_region_overlap
    Reported status was: error
    Error in deepblue_enrich_region_overlap(query_id = filtered_query_id,  : 
      Command enrich_region_overlap does not exists.
    Execution halted
    ```

# DEGreport

Version: 1.12.0

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
      ‘plotCounts’
    degPlotWide: no visible binding for global variable ‘gene’
    degPlotWide: no visible binding for global variable ‘count’
    degPlotWide: no visible binding for global variable ‘treatment’
    degResults: no visible global function definition for ‘assay’
    degResults: no visible global function definition for ‘rlog’
    degResults: no visible global function definition for ‘results’
    degResults: no visible global function definition for ‘colData’
    degResults: no visible global function definition for ‘rowMax’
    degVolcano: no visible binding for global variable ‘logFC’
    degVolcano: no visible binding for global variable ‘V1’
    degVolcano: no visible binding for global variable ‘V2’
    degVolcano: no visible binding for global variable ‘adj.P.Val’
    degVolcano: no visible binding for global variable ‘x’
    degVolcano: no visible binding for global variable ‘y’
    degVolcano: no visible binding for global variable ‘name’
    Undefined global functions or variables:
      MulticoreParam V1 V2 adj.P.Val assay bplapply coda.samples colData
      comp count enrichGO gene group jags.model keys label log2FoldChange
      logFC name one plotCounts results rlog rowMax simplify treatment two
      value variable x y
    ```

# DeLorean

Version: 1.2.5

## In both

*   checking S3 generic/method consistency ... WARNING
    ```
    filter:
      function(x, filter, method, sides, circular, init)
    filter.cells:
      function(dl, .filter, number, cells)
    
    filter:
      function(x, filter, method, sides, circular, init)
    filter.genes:
      function(dl, .filter, number, genes)
    
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

# DepthProc

Version: 2.0.2

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.9Mb
      sub-directories of 1Mb or more:
        libs   5.4Mb
    ```

# describer

Version: 0.2.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      tests/testthat/test-ts.R:8:1: style: lines should not be more than 80 characters.
          "`.data` must be a data source, not a ts object, do you want `stats::filter()`?",
      ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      tests/testthat/test-window.R:63:1: style: lines should not be more than 80 characters.
        expect_error(order_by(NULL, !! 1L), "`call` must be a function call, not an integer vector")
      ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      
      testthat results ================================================================
      OK: 7 SKIPPED: 0 FAILED: 1
      1. Failure: Package Style (@test-styling.R#4) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# desctable

Version: 0.1.0

## Newly broken

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
    Calls: %>% ... .Call -> .f -> overscope_eval_next -> .Call -> eval -> eval
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

# detrendr

Version: 0.1.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      4. Failure: detrending works (@test_detrend.R#29) ------------------------------------------------------------------------------------------------------------------------------------------------------
      round(mean(brightness_pillars(corrected), na.rm = TRUE), 1) not equal to 2.1.
      1/1 mismatches
      [1] 1.9 - 2.1 == -0.2
      
      
      testthat results ========================================================================================================================================================================================
      OK: 54 SKIPPED: 0 FAILED: 4
      1. Failure: myrpois works (@test-myrs.R#7) 
      2. Failure: best_tau works (@test_best_tau.R#7) 
      3. Failure: detrending works (@test_detrend.R#19) 
      4. Failure: detrending works (@test_detrend.R#29) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.8Mb
      sub-directories of 1Mb or more:
        extdata   2.4Mb
        libs      4.1Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# dexter

Version: 0.5.1

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 109 marked UTF-8 strings
    ```

# dggridR

Version: 2.0.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 27.5Mb
      sub-directories of 1Mb or more:
        doc    1.9Mb
        libs  25.2Mb
    ```

# DiffBind

Version: 2.4.8

## In both

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      ‘rgl’ ‘XLConnect’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.5Mb
      sub-directories of 1Mb or more:
        libs   3.3Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    pv.DBAplotVolcano: no visible binding for global variable ‘Fold’
    pv.DBAplotVolcano: no visible binding for global variable ‘Legend’
    Undefined global functions or variables:
      Fold Legend
    ```

# diffloop

Version: 1.4.0

## In both

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .travis.yml
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

# dotwhisker

Version: 0.3.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘arm’
    ```

# ecoengine

Version: 1.11.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘magrittr’
      All declared Imports should be used.
    ```

# eechidna

Version: 1.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.3Mb
      sub-directories of 1Mb or more:
        data   4.9Mb
        doc    1.2Mb
    ```

# EFDR

Version: 0.1.1

## In both

*   checking R code for possible problems ... NOTE
    ```
    .gdf : find_loss: no visible global function definition for ‘rnorm’
    .p.values : <anonymous>: no visible global function definition for
      ‘pnorm’
    .relist.dwt: no visible global function definition for ‘relist’
    .relist.dwt: no visible global function definition for ‘as’
    .std.wav.coeff : <anonymous>: no visible global function definition for
      ‘mad’
    regrid: no visible global function definition for ‘predict’
    regrid: no visible global function definition for ‘var’
    regrid: no visible global function definition for ‘medpolish’
    Undefined global functions or variables:
      as mad medpolish pnorm predict relist rnorm var
    Consider adding
      importFrom("methods", "as")
      importFrom("stats", "mad", "medpolish", "pnorm", "predict", "rnorm",
                 "var")
      importFrom("utils", "relist")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

# emil

Version: 2.2.8

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘Rcpp’
      All declared Imports should be used.
    ```

# ENCODExplorer

Version: 2.2.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 73.6Mb
      sub-directories of 1Mb or more:
        data     23.9Mb
        doc       1.5Mb
        extdata  48.0Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      ‘biosample_type’
    step6_control: no visible binding for global variable ‘controls’
    step6_date_released: no visible binding for global variable
      ‘date_released’
    step6_status: no visible binding for global variable ‘status’
    step6_target: no visible binding for global variable ‘target’
    step7: no visible binding for global variable ‘organism’
    step8: no visible binding for global variable ‘investigated_as’
    step8: no visible binding for global variable ‘target’
    step9: no visible binding for global variable ‘organism’
    Undefined global functions or variables:
      . Experiment Value accession antibody_caption
      antibody_characterization antibody_target assay
      biological_replicate_number biosample_name biosample_type col_name
      controls data date_released download.file encode_df file_accession
      file_format href investigated_as lab nucleic_acid_term organism
      platform project replicate_antibody replicate_library server status
      submitted_by target technical_replicate_number treatment ui value
    Consider adding
      importFrom("utils", "data", "download.file")
    to your NAMESPACE file.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 771 marked UTF-8 strings
    ```

# epicontacts

Version: 1.0.1

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      testthat results ================================================================
      OK: 146 SKIPPED: 0 FAILED: 14
      1. Failure: graph3D produces json that is not null (@test_graph3D.R#12) 
      2. Failure: graph3D errors as expected on bad annotation and group specification (@test_graph3D.R#38) 
      3. Failure: graph3D errors as expected on bad annotation and group specification (@test_graph3D.R#41) 
      4. Failure: graph3D errors as expected on bad annotation and group specification (@test_graph3D.R#44) 
      5. Failure: graph3D errors as expected on bad annotation and group specification (@test_graph3D.R#47) 
      6. Failure: graph3D object includes annotation (@test_graph3D.R#61) 
      7. Failure: Printing objects works (@test_print.epicontacts.R#11) 
      8. Failure: Plotting groups as color (@test_vis_epicontacts.R#24) 
      9. Failure: Plotting groups as color (@test_vis_epicontacts.R#25) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘colorspace’
      All declared Imports should be used.
    ```

# epitable

Version: 0.1.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘magrittr’ ‘readr’
      All declared Imports should be used.
    ```

# esc

Version: 0.4.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘metafor’
    ```

# evaluator

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘ggalt’ ‘pander’ ‘psych’
      All declared Imports should be used.
    Missing or unexported object: ‘purrr::by_row’
    ```

# EventStudy

Version: 0.34

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.5Mb
      sub-directories of 1Mb or more:
        doc   5.8Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘curl’ ‘openxlsx’ ‘stringr’
      All declared Imports should be used.
    ```

# exifr

Version: 0.2.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 13.3Mb
      sub-directories of 1Mb or more:
        exiftool  12.3Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# eyetrackingR

Version: 0.1.6

## In both

*   checking examples ... ERROR
    ```
    ...
    +                                time_column = "TimeFromTrialOnset",
    +                                trackloss_column = "TrackLoss",
    +                                aoi_columns = c('Animate','Inanimate'),
    +                                treat_non_aoi_looks_as_missing = TRUE )
    `mutate_each()` is deprecated.
    Use `mutate_all()`, `mutate_at()` or `mutate_if()` instead.
    To map `funs` over a selection of variables, use `mutate_at()`
    > response_window <- subset_by_window(data, window_start_time = 15500, window_end_time = 21000, 
    +                                     rezero = FALSE)
    Avg. window length in new data will be 5500
    > response_time <- make_time_sequence_data(response_window, time_bin_size = 500, aois = "Animate", 
    +                                          predictor_columns = "Sex")
    > 
    > time_cluster_data <- make_time_cluster_data(data = response_time, predictor_column = "SexM", 
    +                          aoi = "Animate", test = "lmer", 
    +                          threshold = 1.5, 
    +                          formula = LogitAdjusted ~ Sex + (1|Trial) + (1|ParticipantName))
    Error in UseMethod("analyze_time_bins") : 
      no applicable method for 'analyze_time_bins' applied to an object of class "data.frame"
    Calls: make_time_cluster_data ... make_time_cluster_data.time_sequence_data -> do.call -> <Anonymous>
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Computing t.test for each time bin...
      Computing t.test for each time bin...
      `mutate_each()` is deprecated.
      Use `mutate_all()`, `mutate_at()` or `mutate_if()` instead.
      To map `funs` over a selection of variables, use `mutate_at()`
      Avg. window length in new data will be 5500
      Performing Trackloss Analysis...
      Will exclude trials whose trackloss proportion is greater than : 0.25
      	...removed  33  trials.
      Error in UseMethod("make_time_cluster_data") : 
        no applicable method for 'make_time_cluster_data' applied to an object of class "data.frame"
      Calls: test_check ... source_file -> eval -> eval -> make_time_cluster_data
      testthat results ================================================================
      OK: 38 SKIPPED: 0 FAILED: 0
      Execution halted
    ```

# fastLink

Version: 0.2.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.4Mb
      sub-directories of 1Mb or more:
        data   4.8Mb
        libs   2.4Mb
    ```

# fastR2

Version: 0.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘magrittr’
      All declared Imports should be used.
    ```

# FindMyFriends

Version: 1.6.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Found more than one class "file" in cache; using the first, from namespace 'BiocGenerics'
    Also defined by 'filehash'
    Found more than one class "file" in cache; using the first, from namespace 'BiocGenerics'
    Also defined by 'filehash'
    Preclustering resulted in 3326 gene groups (1.367 seconds elapsed)
    Grouping resulted in 3140 gene groups (12.369 seconds elapsed)
    Total time elapsed was 13.76 seconds
    Presplitting resulted in 3161 gene groups (0.091 seconds elapsed)
    Adding missing grouping variables: `org`, `contig`
    Splitting resulted in 3602 gene groups (3 minutes and 23.428 seconds elapsed)
    Adding missing grouping variables: `org`, `contig`
    Adding missing grouping variables: `org`, `contig`
    Adding missing grouping variables: `org`, `contig`
    Adding missing grouping variables: `org`, `contig`
    Adding missing grouping variables: `org`, `contig`
    Adding missing grouping variables: `org`, `contig`
    Merging resulted in 3414 gene groups (3.44 seconds elapsed)
    Total time elapsed was 3 minutes and 26.959 seconds
    Error: processing vignette 'FindMyFriends_intro.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  8.6Mb
      sub-directories of 1Mb or more:
        extdata   1.8Mb
        libs      5.6Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unexported objects imported by ':::' calls:
      ‘gtable:::insert.unit’ ‘gtable:::z_arrange_gtables’
      See the note in ?`:::` about the use of this operator.
    ```

# fingertipsR

Version: 0.1.3

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    pandoc: Could not find data file /usr/share/pandoc/data/templates/--highlight-style.html
    Error: processing vignette 'lifeExpectancy.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 97
    Execution halted
    ```

# flextable

Version: 0.3.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      6: pmap_chr(x, function(image_src, width, height) {
             format(external_img(src = image_src, width = width, height = height), type = type)
         })
      7: .f(image_src = .l[[c(1L, i)]], width = .l[[c(2L, i)]], height = .l[[c(3L, i)]], ...)
      8: format(external_img(src = image_src, width = width, height = height), type = type)
      9: external_img(src = image_src, width = width, height = height)
      10: stopifnot(file.exists(src))
      11: stop(msg, call. = FALSE, domain = NA)
      
      testthat results ================================================================
      OK: 67 SKIPPED: 0 FAILED: 1
      1. Error: images (@test-images.R#44) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Loading required package: officer
    Quitting from lines 323-336 (format.Rmd) 
    Error: processing vignette 'format.Rmd' failed with diagnostics:
    file.exists(src) is not TRUE
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘magrittr’
      All declared Imports should be used.
    ```

# flowWorkspace

Version: 3.24.4

## In both

*   checking if this is a source package ... NOTE
    ```
    ...
      src/pb_build/src/google/protobuf/io/zero_copy_stream_impl_lite.lo
      src/pb_build/src/google/protobuf/message.lo
      src/pb_build/src/google/protobuf/message_lite.lo
      src/pb_build/src/google/protobuf/reflection_ops.lo
      src/pb_build/src/google/protobuf/repeated_field.lo
      src/pb_build/src/google/protobuf/service.lo
      src/pb_build/src/google/protobuf/stubs/atomicops_internals_x86_gcc.lo
      src/pb_build/src/google/protobuf/stubs/atomicops_internals_x86_msvc.lo
      src/pb_build/src/google/protobuf/stubs/common.lo
      src/pb_build/src/google/protobuf/stubs/once.lo
      src/pb_build/src/google/protobuf/stubs/stringprintf.lo
      src/pb_build/src/google/protobuf/stubs/structurally_valid.lo
      src/pb_build/src/google/protobuf/stubs/strutil.lo
      src/pb_build/src/google/protobuf/stubs/substitute.lo
      src/pb_build/src/google/protobuf/text_format.lo
      src/pb_build/src/google/protobuf/unknown_field_set.lo
      src/pb_build/src/google/protobuf/wire_format.lo
      src/pb_build/src/google/protobuf/wire_format_lite.lo
      src/pb_build/src/libprotobuf.la
      src/protobuf-2.6.0/src/solaris/libstdc++.la
    Object files/libraries should not be included in a source package.
    ```

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      src/pb_build/src/google/protobuf/.dirstamp
      src/pb_build/src/google/protobuf/io/.dirstamp
      src/pb_build/src/google/protobuf/stubs/.dirstamp
      src/pb_build/src/.libs
      src/pb_build/src/google/protobuf/.libs
      src/pb_build/src/google/protobuf/io/.libs
      src/pb_build/src/google/protobuf/stubs/.libs
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 108.0Mb
      sub-directories of 1Mb or more:
        doc       1.1Mb
        include   2.7Mb
        lib      72.9Mb
        libs     30.6Mb
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Versioned 'LinkingTo' value for ‘BH’ is only usable in R >= 3.0.2
    ```

*   checking dependencies in R code ... NOTE
    ```
    'library' or 'require' calls in package code:
      ‘RSVGTipsDevice’ ‘parallel’
      Please use :: or requireNamespace() instead.
      See section 'Suggested packages' in the 'Writing R Extensions' manual.
    Namespaces in Imports field not imported from:
      ‘RBGL’ ‘graphics’
      All declared Imports should be used.
    Unexported objects imported by ':::' calls:
      ‘Rgraphviz:::getRenderPar’ ‘flowCore:::.estimateLogicle’
      ‘flowCore:::checkClass’ ‘flowCore:::copyFlowSet’ ‘flowCore:::guid’
      ‘flowCore:::logicle_transform’ ‘graph:::.makeEdgeKeys’
      ‘lattice:::updateList’ ‘ncdfFlow:::.isValidSamples’
      ‘stats:::.splinefun’
      See the note in ?`:::` about the use of this operator.
    There are ::: calls to the package's namespace in its code. A package
      almost never needs to use ::: for its own objects:
      ‘.load_gs’ ‘.preprocessMap’ ‘.uuid_gen’ ‘isNegated’
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    rbind2,GatingSetList-missing: no visible global function definition for
      ‘new’
    rbind2,GatingSetList-missing: no visible binding for global variable
      ‘slot’
    recompute,GatingSetList: no visible global function definition for
      ‘selectMethod’
    transform,GatingSet: no visible global function definition for ‘is’
    Undefined global functions or variables:
      . .hasSlot IQR as as.formula callNextMethod decade dev.off dev.prev
      dev.set extends gray groupName is max_val median min_val new node
      offset old openCyto.count polygon rect sampleName selectMethod slot
      strheight strwidth symbols text validObject xml.count
    Consider adding
      importFrom("grDevices", "dev.off", "dev.prev", "dev.set", "gray")
      importFrom("graphics", "polygon", "rect", "strheight", "strwidth",
                 "symbols", "text")
      importFrom("methods", ".hasSlot", "as", "callNextMethod", "extends",
                 "is", "new", "selectMethod", "slot", "validObject")
      importFrom("stats", "IQR", "as.formula", "median", "offset")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# foghorn

Version: 0.4.4

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      testthat results ================================================================
      OK: 49 SKIPPED: 0 FAILED: 10
      1.  Failure: output of summary cran results (@test-foghorn.R#176) 
      2.  Failure: output of summary cran results (@test-foghorn.R#178) 
      3.  Failure: output of summary cran results (@test-foghorn.R#189) 
      4.  Failure: output of summary cran results (@test-foghorn.R#201) 
      5.  Failure: output of summary cran results (@test-foghorn.R#213) 
      6.  Failure: output of summary cran results (@test-foghorn.R#226) 
      7.  Failure: output of summary cran results (@test-foghorn.R#233) 
      8.  Failure: output of summary cran results (@test-foghorn.R#239) 
      9.  Failure: output of show cran results (@test-foghorn.R#264) 
      10. Failure: output of show cran results (@test-foghorn.R#266) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# fourierin

Version: 0.2.2

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.0Mb
      sub-directories of 1Mb or more:
        libs   4.5Mb
    ```

# freqweights

Version: 1.0.4

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘Hmisc’
    ```

# FRK

Version: 0.1.6

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘INLA’
    
    Package which this enhances but not available for checking: ‘dggrids’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  7.6Mb
      sub-directories of 1Mb or more:
        data   4.8Mb
        doc    1.6Mb
    ```

# FSA

Version: 0.8.17

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘alr3’, ‘doBy’, ‘psych’, ‘prettyR’, ‘fBasics’, ‘RMark’, ‘asbio’, ‘PMCMR’, ‘pgirmess’, ‘agricolae’, ‘DescTools’
    ```

# FSelectorRcpp

Version: 0.1.8

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 11.1Mb
      sub-directories of 1Mb or more:
        doc    2.2Mb
        libs   8.7Mb
    ```

# ftDK

Version: 1.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 39 marked UTF-8 strings
    ```

# furrowSeg

Version: 1.4.0

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘EBImage’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# GA4GHclient

Version: 1.0.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      testthat results ================================================================
      OK: 129 SKIPPED: 0 FAILED: 15
      1. Error: getVariant works (@test-getVariant.R#8) 
      2. Error: getVariant asVCF=FALSE works (@test-getVariant.R#19) 
      3. Error: getVariant multiple alt bases works (@test-getVariant.R#32) 
      4. Error: makeVCFFromGA4GHResponse works (@test-makeVCFFromGA4GHResponse.R#10) 
      5. Failure: searchCallSets works (@test-searchCallSets.R#10) 
      6. Failure: searchCallSets responseSize parameter works (@test-searchCallSets.R#28) 
      7. Error: searchReads works (@test-searchReads.R#11) 
      8. Failure: searchVariantSets works (@test-searchVariantSets.R#12) 
      9. Error: searchVariants works (@test-searchVariants.R#8) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
    Attaching package: 'Biostrings'
    
    The following object is masked from 'package:DelayedArray':
    
        type
    
    The following object is masked from 'package:base':
    
        strsplit
    
    
    Attaching package: 'VariantAnnotation'
    
    The following object is masked from 'package:base':
    
        tabulate
    
    Error: processing vignette 'GA4GHclient.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

# GADMTools

Version: 2.1-1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘scales’
      All declared Imports should be used.
    ```

# gaiah

Version: 0.0.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘ggplot2’ ‘maptools’ ‘rgeos’ ‘stringr’ ‘tidyr’
      All declared Imports should be used.
    ```

# gastempt

Version: 0.4.01

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 58.6Mb
      sub-directories of 1Mb or more:
        libs  58.2Mb
    ```

# geex

Version: 1.0.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘rootSolve’
      All declared Imports should be used.
    ```

# gender

Version: 0.5.1

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘genderdata’
    ```

# geneXtendeR

Version: 1.2.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.8Mb
      sub-directories of 1Mb or more:
        data   5.8Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    barChart : geneXtender: no visible binding for global variable ‘type’
    barChart : geneXtender: no visible binding for global variable ‘seqid’
    barChart : geneXtender: no visible binding for global variable
      ‘gene_id’
    barChart : geneXtender: no visible binding for global variable
      ‘gene_name’
    distinct : geneXtender: no visible binding for global variable ‘type’
    distinct : geneXtender: no visible binding for global variable ‘seqid’
    distinct : geneXtender: no visible binding for global variable
      ‘gene_id’
    distinct : geneXtender: no visible binding for global variable
      ‘gene_name’
    linePlot : geneXtender: no visible binding for global variable ‘type’
    linePlot : geneXtender: no visible binding for global variable ‘seqid’
    linePlot : geneXtender: no visible binding for global variable
      ‘gene_id’
    linePlot : geneXtender: no visible binding for global variable
      ‘gene_name’
    peaksInput: no visible binding for global variable ‘chr’
    Undefined global functions or variables:
      chr gene_id gene_name seqid type
    ```

# GenomicInteractions

Version: 1.10.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 11.0Mb
      sub-directories of 1Mb or more:
        doc       2.0Mb
        extdata   7.9Mb
    ```

# geoknife

Version: 1.5.5

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      StartTag: invalid element name
      Extra content at the end of the document
      testthat results ================================================================
      OK: 174 SKIPPED: 0 FAILED: 7
      1. Error: download works (@test-download_result.R#8) 
      2. Error: load result works (@test-download_result.R#14) 
      3. Error: load result works from job id URL (@test-download_result.R#19) 
      4. Error: download result works from job id URL (@test-download_result.R#24) 
      5. Error: download result works from job (@test-download_result.R#29) 
      6. Failure: error on url (@test-geoknife_utils.R#15) 
      7. Error: you can set booleans as pass through (@test-webprocess_input.R#56) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 360-364 (geoknife.Rmd) 
    Error: processing vignette 'geoknife.Rmd' failed with diagnostics:
    need finite 'xlim' values
    Execution halted
    ```

# GEOmetadb

Version: 1.36.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
    The following objects are masked from 'package:Biobase':
    
        combine, exprs
    
    The following objects are masked from 'package:BiocGenerics':
    
        combine, intersect, setdiff, union
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Quitting from lines 273-277 (GEOmetadb.Rmd) 
    Error: processing vignette 'GEOmetadb.Rmd' failed with diagnostics:
    Condition message must be a string
    Execution halted
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
    getSQLiteFile: no visible global function definition for
      ‘download.file’
    Undefined global functions or variables:
      download.file
    Consider adding
      importFrom("utils", "download.file")
    to your NAMESPACE file.
    ```

# geoparser

Version: 0.1.1

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      2. Error: no problems if no results (@test_query.R#26) -------------------------
      HTTP failure: 401
      1: geoparser_q(text_input = "no placename here") at testthat/test_query.R:26
      2: lapply(text_input, total, key = key)
      3: FUN(X[[i]], ...)
      4: geoparser_check(temp)
      5: stop("HTTP failure: ", status, call. = FALSE)
      
      testthat results ================================================================
      OK: 3 SKIPPED: 0 FAILED: 2
      1. Error: query returns a list of data.frames (@test_query.R#4) 
      2. Error: no problems if no results (@test_query.R#26) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 34-37 (geoparser.Rmd) 
    Error: processing vignette 'geoparser.Rmd' failed with diagnostics:
    HTTP failure: 401
    Execution halted
    ```

# geotoolsR

Version: 1.0

## In both

*   checking whether package ‘geotoolsR’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: no DISPLAY variable so Tk is not available
    See ‘/home/muelleki/git/R/dplyr/revdep/checks/geotoolsR/new/geotoolsR.Rcheck/00install.out’ for details.
    ```

# GerminaR

Version: 1.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘DT’ ‘shinydashboard’
      All declared Imports should be used.
    ```

# gespeR

Version: 1.8.0

## In both

*   checking for code/documentation mismatches ... WARNING
    ```
    Codoc mismatches from documentation object 'c,Phenotypes-method':
    \S4method{c}{Phenotypes}
      Code: function(x, ...)
      Docs: function(x, ..., recursive = FALSE)
      Argument names in docs not in code:
        recursive
    ```

*   checking R code for possible problems ... NOTE
    ```
    .gespeR.cv: no visible global function definition for ‘coef’
    .select.model: no visible global function definition for ‘predict’
    concordance: no visible global function definition for ‘cor’
    lasso.rand: no visible global function definition for ‘runif’
    plot.gespeR: no visible global function definition for ‘hist’
    stability.selection: no visible global function definition for ‘lm’
    Phenotypes,character: no visible global function definition for
      ‘read.delim’
    Undefined global functions or variables:
      coef cor hist lm predict read.delim runif
    Consider adding
      importFrom("graphics", "hist")
      importFrom("stats", "coef", "cor", "lm", "predict", "runif")
      importFrom("utils", "read.delim")
    to your NAMESPACE file.
    ```

# getCRUCLdata

Version: 0.1.10

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      6: .stopAsciiWriting(x)
      7: raster(x@file@name)
      8: raster(x@file@name)
      9: .local(x, ...)
      10: .rasterObjectFromFile(x, band = band, objecttype = "RasterLayer", ...)
      11: .rasterFromASCIIFile(x, ...)
      12: stop("\"NROWS\" not detected")
      
      testthat results ================================================================
      OK: 107 SKIPPED: 0 FAILED: 2
      1. Failure: cache directory is created if necessary (@test_caching.R#30) 
      2. Error: caching utils list files in cache and delete when asked (@test_caching.R#46) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# GetLattesData

Version: 0.8

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘GetLattesData-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: gld_get_lattes_data
    > ### Title: Downloads and reads Lattes data based on a vector of Lattes ids
    > ### Aliases: gld_get_lattes_data
    > 
    > ### ** Examples
    > 
    > 
    > l.out <- gld_get_lattes_data(id.vec = 'K4713546D3',
    +                              field.qualis = 'ECONOMIA')
    
    Downloading file  /home/muelleki/tmp/Rtmp5NXsJR/K4713546D3_2017-11-16.zipWarning in utils::download.file(url = my.link, destfile = dest.file, quiet = T,  :
      unable to resolve 'buscacv.cnpq.br'
    Error in utils::download.file(url = my.link, destfile = dest.file, quiet = T,  : 
      cannot open URL 'http://buscacv.cnpq.br/buscacv/rest/download/curriculo/K4713546D3'
    Calls: gld_get_lattes_data -> sapply -> lapply -> FUN -> <Anonymous>
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      4: identical(as.vector(object), TRUE)
      5: as.vector(object)
      6: gld_get_lattes_data("K4713546D3", folder.dl = "lattes files")
      7: sapply(X = id.vec, FUN = gld_download_lattes_files, folder.dl = folder.dl)
      8: lapply(X = X, FUN = FUN, ...)
      9: FUN(X[[i]], ...)
      10: utils::download.file(url = my.link, destfile = dest.file, quiet = T, mode = "wb", 
             method = "internal")
      
      testthat results ================================================================
      OK: 1 SKIPPED: 0 FAILED: 1
      1. Error: Test of main function (@test_gld.R#5) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 36-45 (gld_vignette-ReadLattes.Rmd) 
    Error: processing vignette 'gld_vignette-ReadLattes.Rmd' failed with diagnostics:
    cannot open URL 'http://buscacv.cnpq.br/buscacv/rest/download/curriculo/K4713546D3'
    Execution halted
    ```

# ggalt

Version: 0.4.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘plotly’
      All declared Imports should be used.
    ```

# ggCompNet

Version: 0.1.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.4Mb
      sub-directories of 1Mb or more:
        doc   6.1Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘ggmap’ ‘gridExtra’ ‘scales’ ‘tnet’
      All declared Imports should be used.
    ```

# ggconf

Version: 0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tibble’
      All declared Imports should be used.
    ```

# ggenealogy

Version: 0.3.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tibble’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2356 marked UTF-8 strings
    ```

# ggfan

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘colorspace’ ‘grid’
      All declared Imports should be used.
    ```

# ggfortify

Version: 0.4.1

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘ggfortify-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: gglagplot
    > ### Title: Plot time series against lagged versions of themselves
    > ### Aliases: gglagplot
    > 
    > ### ** Examples
    > 
    > gglagplot(AirPassengers)
    Error: `x` must be a vector, not a ts object, do you want `stats::lag()`?
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
        expect_equal(class(res$max_g) , c("POSIXct", "POSIXt"))
                                     ~^
      
      
      testthat results ================================================================
      OK: 1616 SKIPPED: 0 FAILED: 6
      1. Failure: autoplot.aareg works for lung (@test-surv.R#220) 
      2. Failure: autoplot.aareg works for lung (@test-surv.R#221) 
      3. Failure: autoplot.aareg works for lung (@test-surv.R#222) 
      4. Failure: autoplot.aareg works for lung (@test-surv.R#223) 
      5. Error: gglagplot (@test-tslib.R#103) 
      6. Failure: Code Lint (@test_lint.R#27) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.4Mb
      sub-directories of 1Mb or more:
        doc   5.0Mb
    ```

# ggguitar

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘gridExtra’ ‘lazyeval’ ‘readr’
      All declared Imports should be used.
    ```

# gglogo

Version: 0.1.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘knitr’
      All declared Imports should be used.
    ```

# ggmosaic

Version: 0.1.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘NHANES’ ‘gridExtra’
      All declared Imports should be used.
    ```

# ggplotAssist

Version: 0.1.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘gcookbook’ ‘ggthemes’ ‘moonBook’ ‘tidyverse’
      All declared Imports should be used.
    ```

# ggpubr

Version: 0.1.6

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘FactoMineR’
    ```

# ggRandomForests

Version: 2.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘randomForest’
      All declared Imports should be used.
    ```

# ggraph

Version: 1.0.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.4Mb
      sub-directories of 1Mb or more:
        doc    3.0Mb
        libs   2.8Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘Rcpp’
      All declared Imports should be used.
    ```

# ggraptR

Version: 1.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > 
      > if (Sys.getenv("NOT_CRAN") == "true") {  # like global skip_on_cran
      +   Sys.setenv("R_TESTS" = "")  # accroding to https://github.com/hadley/testthat/issues/144
      +   test_check("ggraptR")
      + }
      
      Initial plotError in file(filename, "r", encoding = encoding) : 
        cannot open the connection
      Calls: test_check ... source -> withVisible -> eval -> eval -> source -> file
      In addition: Warning message:
      In file(filename, "r", encoding = encoding) :
        cannot open file '../../inst/ggraptR/functions/helper.R': No such file or directory
      testthat results ================================================================
      OK: 0 SKIPPED: 0 FAILED: 0
      Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘DBI’ ‘GGally’ ‘RColorBrewer’ ‘Rcpp’ ‘assertthat’ ‘backports’
      ‘colorspace’ ‘colourpicker’ ‘evaluate’ ‘futile.options’ ‘gdtools’
      ‘gtable’ ‘htmltools’ ‘htmlwidgets’ ‘httpuv’ ‘labeling’ ‘lambda.r’
      ‘lazyeval’ ‘magrittr’ ‘miniUI’ ‘munsell’ ‘plyr’ ‘reshape’ ‘rprojroot’
      ‘scales’ ‘stringi’ ‘stringr’ ‘svglite’ ‘tibble’ ‘xtable’ ‘yaml’
      All declared Imports should be used.
    ```

# ggvis

Version: 0.4.3

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘plyr’
    ```

# gistr

Version: 0.4.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      testthat results ================================================================
      OK: 4 SKIPPED: 0 FAILED: 19
      1. Error: commits works (@test-commits.R#7) 
      2. Error: config options work (@test-commits.R#20) 
      3. Error: delete returns NULL (@test-delete.R#10) 
      4. Error: delete returns correct message (@test-delete.R#19) 
      5. Error: embed returns correct string and is character (@test-embed.R#7) 
      6. Error: fork works (@test-fork.R#7) 
      7. Error: forks works (@test-fork.R#19) 
      8. Error: gist works (@test-gist.R#7) 
      9. Error: as.gist works - character gist ID input (@test-gist.R#13) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

# googlesheets

Version: 0.2.2

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(googlesheets)
      > 
      > if (identical(tolower(Sys.getenv("NOT_CRAN")), "true")) {
      +   test_check("googlesheets")
      + }
      Error: Cannot read token from alleged .rds file:
      googlesheets_token.rds
      testthat results ================================================================
      OK: 2 SKIPPED: 0 FAILED: 0
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 37-41 (basic-usage.Rmd) 
    Error: processing vignette 'basic-usage.Rmd' failed with diagnostics:
    Cannot read token from alleged .rds file:
    ../tests/testthat/googlesheets_token.rds
    Execution halted
    ```

# gQTLstats

Version: 1.8.0

## In both

*   checking whether the package can be unloaded cleanly ... WARNING
    ```
    Error: package or namespace load failed for ‘gQTLstats’:
     .onLoad failed in loadNamespace() for 'Homo.sapiens', details:
      call: loadMethod(structure(function (object) 
      error: could not find function "loadMethod"
    Execution halted
    ```

*   checking whether the namespace can be loaded with stated dependencies ... WARNING
    ```
    Error: .onLoad failed in loadNamespace() for 'Homo.sapiens', details:
      call: loadMethod(structure(function (object) 
      error: could not find function "loadMethod"
    Execution halted
    
    A namespace must be able to be loaded with just the base namespace
    loaded: otherwise if the namespace gets loaded by a saved object, the
    session will be unable to start.
    
    Probably some imports need to be declared in the NAMESPACE file.
    ```

*   checking S3 generic/method consistency ... WARNING
    ```
    ...
    5: value[[3L]](cond)
    4: tryCatchOne(expr, names, parentenv, handlers[[1L]])
    3: tryCatchList(expr, classes, parentenv, handlers)
    2: tryCatch({
           attr(package, "LibPath") <- which.lib.loc
           ns <- loadNamespace(package, lib.loc)
           env <- attachNamespace(ns, pos = pos, deps)
       }, error = function(e) {
           P <- if (!is.null(cc <- conditionCall(e))) 
               paste(" in", deparse(cc)[1L])
           else ""
           msg <- gettextf("package or namespace load failed for %s%s:\n %s", 
               sQuote(package), P, conditionMessage(e))
           if (logical.return) 
               message(paste("Error:", msg), domain = NA)
           else stop(msg, call. = FALSE, domain = NA)
       })
    1: library(package, lib.loc = lib.loc
    Execution halted
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking replacement functions ... WARNING
    ```
    ...
    5: value[[3L]](cond)
    4: tryCatchOne(expr, names, parentenv, handlers[[1L]])
    3: tryCatchList(expr, classes, parentenv, handlers)
    2: tryCatch({
           attr(package, "LibPath") <- which.lib.loc
           ns <- loadNamespace(package, lib.loc)
           env <- attachNamespace(ns, pos = pos, deps)
       }, error = function(e) {
           P <- if (!is.null(cc <- conditionCall(e))) 
               paste(" in", deparse(cc)[1L])
           else ""
           msg <- gettextf("package or namespace load failed for %s%s:\n %s", 
               sQuote(package), P, conditionMessage(e))
           if (logical.return) 
               message(paste("Error:", msg), domain = NA)
           else stop(msg, call. = FALSE, domain = NA)
       })
    1: library(package, lib.loc = lib.loc
    Execution halted
    The argument of a replacement function which corresponds to the right
    hand side must be named ‘value’.
    ```

*   checking for code/documentation mismatches ... WARNING
    ```
    ...
    Call sequence:
    6: stop(msg, call. = FALSE, domain = NA)
    5: value[[3L]](cond)
    4: tryCatchOne(expr, names, parentenv, handlers[[1L]])
    3: tryCatchList(expr, classes, parentenv, handlers)
    2: tryCatch({
           attr(package, "LibPath") <- which.lib.loc
           ns <- loadNamespace(package, lib.loc)
           env <- attachNamespace(ns, pos = pos, deps)
       }, error = function(e) {
           P <- if (!is.null(cc <- conditionCall(e))) 
               paste(" in", deparse(cc)[1L])
           else ""
           msg <- gettextf("package or namespace load failed for %s%s:\n %s", 
               sQuote(package), P, conditionMessage(e))
           if (logical.return) 
               message(paste("Error:", msg), domain = NA)
           else stop(msg, call. = FALSE, domain = NA)
       })
    1: library(package, lib.loc = lib.loc
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 65.8Mb
      sub-directories of 1Mb or more:
        data        11.0Mb
        doc          1.1Mb
        registries  18.9Mb
        vcf         33.8Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    ...
    Call sequence:
    6: stop(msg, call. = FALSE, domain = NA)
    5: value[[3L]](cond)
    4: tryCatchOne(expr, names, parentenv, handlers[[1L]])
    3: tryCatchList(expr, classes, parentenv, handlers)
    2: tryCatch({
           attr(package, "LibPath") <- which.lib.loc
           ns <- loadNamespace(package, lib.loc)
           env <- attachNamespace(ns, pos = pos, deps)
       }, error = function(e) {
           P <- if (!is.null(cc <- conditionCall(e))) 
               paste(" in", deparse(cc)[1L])
           else ""
           msg <- gettextf("package or namespace load failed for %s%s:\n %s", 
               sQuote(package), P, conditionMessage(e))
           if (logical.return) 
               message(paste("Error:", msg), domain = NA)
           else stop(msg, call. = FALSE, domain = NA)
       })
    1: library(package, lib.loc = lib.loc
    Execution halted
    ```

*   checking foreign function calls ... NOTE
    ```
    ...
    5: value[[3L]](cond)
    4: tryCatchOne(expr, names, parentenv, handlers[[1L]])
    3: tryCatchList(expr, classes, parentenv, handlers)
    2: tryCatch({
           attr(package, "LibPath") <- which.lib.loc
           ns <- loadNamespace(package, lib.loc)
           env <- attachNamespace(ns, pos = pos, deps)
       }, error = function(e) {
           P <- if (!is.null(cc <- conditionCall(e))) 
               paste(" in", deparse(cc)[1L])
           else ""
           msg <- gettextf("package or namespace load failed for %s%s:\n %s", 
               sQuote(package), P, conditionMessage(e))
           if (logical.return) 
               message(paste("Error:", msg), domain = NA)
           else stop(msg, call. = FALSE, domain = NA)
       })
    1: library(package, lib.loc = lib.loc
    Execution halted
    See chapter ‘System and foreign language interfaces’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking R code for possible problems ... NOTE
    ```
    Error: .onLoad failed in loadNamespace() for 'Homo.sapiens', details:
      call: loadMethod(structure(function (object) 
      error: could not find function "loadMethod"
    Execution halted
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘MultiAssayExperiment’
    ```

*   checking Rd \usage sections ... NOTE
    ```
    ...
    3: tryCatchList(expr, classes, parentenv, handlers)
    2: tryCatch({
           attr(package, "LibPath") <- which.lib.loc
           ns <- loadNamespace(package, lib.loc)
           env <- attachNamespace(ns, pos = pos, deps)
       }, error = function(e) {
           P <- if (!is.null(cc <- conditionCall(e))) 
               paste(" in", deparse(cc)[1L])
           else ""
           msg <- gettextf("package or namespace load failed for %s%s:\n %s", 
               sQuote(package), P, conditionMessage(e))
           if (logical.return) 
               message(paste("Error:", msg), domain = NA)
           else stop(msg, call. = FALSE, domain = NA)
       })
    1: library(package, lib.loc = lib.loc
    Execution halted
    The \usage entries for S3 methods should use the \method markup and not
    their full name.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 8 marked Latin-1 strings
      Note: found 12 marked UTF-8 strings
    ```

*   checking for unstated dependencies in vignettes ... NOTE
    ```
    'library' or 'require' calls not declared from:
      ‘TxDb.Hsapiens.UCSC.hg19.knownGene’ ‘org.Hs.eg.db’
    ```

# GRANBase

Version: 1.6.5

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘hexSticker’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# grasp2db

Version: 1.0.0

## In both

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

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Warning in engine$weave(file, quiet = quiet, encoding = enc) :
      The vignette engine knitr::rmarkdown is not available, because the rmarkdown package is not installed. Please install it.
    Quitting from lines 15-16 (BiocGRASP2.Rmd) 
    Error: processing vignette 'BiocGRASP2.Rmd' failed with diagnostics:
    there is no package called 'BiocStyle'
    Execution halted
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

*   checking for unstated dependencies in vignettes ... NOTE
    ```
    '::' or ':::' import not declared from: ‘BiocStyle’
    ```

# grattan

Version: 1.5.2.4

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘taxstats’
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# Greg

Version: 1.2

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘rmeta’
    ```

*   checking Rd \usage sections ... NOTE
    ```
    S3 methods shown with full name in documentation object 'printCrudeAndAdjustedModel':
      ‘rbind.printCrudeAndAdjusted’ ‘cbind.printCrudeAndAdjusted’
    
    The \usage entries for S3 methods should use the \method markup and not
    their full name.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

# GSODR

Version: 1.1.0

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
             skip = skip, comment = comment, n_max = n_max, guess_max = guess_max, progress = progress)
      4: read_connection(file)
      5: open(con, "rb")
      6: open.connection(con, "rb")
      
      testthat results ================================================================
      OK: 50 SKIPPED: 0 FAILED: 4
      1. Error: .download_files properly works, subsetting for country and
                  agroclimatology works and .process_gz returns a data table (@test-process_gz.R#23) 
      2. Error: reformat_GSOD file_list parameter reformats data properly (@test-reformat_GSOD.R#15) 
      3. Error: Timeout options are reset on update_station_list() exit (@test-update_station_list.R#6) 
      4. Error: update_station_list() downloads and imports proper file (@test-update_station_list.R#13) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# gutenbergr

Version: 0.1.3

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 13617 marked UTF-8 strings
    ```

# harrietr

Version: 0.2.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘ggplot2’
      All declared Imports should be used.
    ```

# hdr

Version: 0.1

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘hdr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: get_data
    > ### Title: Fetch data from the UNDP Human Development Report
    > ### Aliases: get_data
    > 
    > ### ** Examples
    > 
    > # Get the Human Development Index for Germany in 2013
    > df <- get_data(indicator = 137506, country = "DEU", year = 2013)
    Error in curl::curl_fetch_memory(url, handle = handle) : 
      Failed to connect to ec2-52-1-168-42.compute-1.amazonaws.com port 80: Connection refused
    Calls: get_data ... request_fetch -> request_fetch.write_memory -> <Anonymous> -> .Call
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 31-33 (undp_hdr.Rmd) 
    Error: processing vignette 'undp_hdr.Rmd' failed with diagnostics:
    Failed to connect to ec2-52-1-168-42.compute-1.amazonaws.com port 80: Connection refused
    Execution halted
    ```

# hiAnnotator

Version: 1.11.1

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    makeChunks: no visible global function definition for ‘breakInChunks’
    makeChunks: no visible global function definition for ‘detectCores’
    makeChunks : <anonymous>: no visible global function definition for
      ‘keepSeqlevels’
    makeChunks : <anonymous>: no visible global function definition for
      ‘seqlevelsInUse’
    makeGRanges: no visible global function definition for ‘IRanges’
    makeGRanges: no visible global function definition for ‘seqlengths’
    makeGRanges: no visible global function definition for ‘seqlevels<-’
    makeGRanges: no visible global function definition for ‘sortSeqlevels’
    makeGRanges: no visible global function definition for ‘seqlevelsInUse’
    makeGRanges: no visible global function definition for ‘seqlengths<-’
    makeGRanges: no visible global function definition for ‘seqlevels’
    Undefined global functions or variables:
      IRanges breakInChunks countQueryHits detectCores dist featureName
      keepSeqlevels mid n overlapsAny qStrand queryHits seqlengths
      seqlengths<- seqlevels seqlevels<- seqlevelsInUse sortSeqlevels
      subjectHits
    Consider adding
      importFrom("stats", "dist")
    to your NAMESPACE file.
    ```

*   checking files in ‘vignettes’ ... NOTE
    ```
    The following directory looks like a leftover from 'knitr':
      ‘figure’
    Please remove from your package.
    ```

# highcharter

Version: 0.5.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 16.5Mb
      sub-directories of 1Mb or more:
        doc          13.7Mb
        htmlwidgets   1.9Mb
    ```

# hiReadsProcessor

Version: 1.12.0

## In both

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .BBSoptions
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      ‘qBaseInsert’
    read.psl : <anonymous>: no visible binding for global variable
      ‘tBaseInsert’
    read.psl: no visible binding for global variable ‘matches’
    read.psl: no visible binding for global variable ‘misMatches’
    read.psl: no visible binding for global variable ‘qBaseInsert’
    read.psl: no visible binding for global variable ‘tBaseInsert’
    read.sampleInfo: no visible global function definition for ‘SimpleList’
    splitSeqsToFiles: no visible global function definition for
      ‘fasta.info’
    vpairwiseAlignSeqs: no visible global function definition for ‘Rle’
    vpairwiseAlignSeqs: no visible global function definition for
      ‘runLength’
    vpairwiseAlignSeqs: no visible global function definition for ‘IRanges’
    vpairwiseAlignSeqs: no visible global function definition for
      ‘runValue’
    Undefined global functions or variables:
      DataFrame IRanges IRangesList Rle ScanBamParam SimpleList
      breakInChunks clusteredValue clusteredValue.freq detectCores
      fasta.info matches mclapply metadata metadata<- misMatches
      qBaseInsert queryHits runLength runValue scanBamFlag tBaseInsert
    ```

# HMMoce

Version: 1.0.0

## In both

*   checking examples ... ERROR
    ```
    ...
    > 
    > ### ** Examples
    > 
    > # example data in the package
    > sstFile <- system.file("extdata", "141259-SST.csv", package = "HMMoce")
    > ptt <- 141259
    > 
    > # set temporal and spatial bounds
    > iniloc <- data.frame(matrix(c(13, 10, 2015, 41.3, -69.27, 10, 4, 2016, 40.251, -36.061),
    +  nrow = 2, ncol = 5, byrow = TRUE))
    >  names(iniloc) <- list('day','month','year','lat','lon')
    >  tag <- as.POSIXct(paste(iniloc[1,1], '/', iniloc[1,2], '/', iniloc[1,3], sep=''), 
    +  format = '%d/%m/%Y', tz='UTC')
    >  pop <- as.POSIXct(paste(iniloc[2,1], '/', iniloc[2,2], '/', iniloc[2,3], sep=''), 
    +  format = '%d/%m/%Y', tz='UTC')
    > 
    > # read and format the example data
    > tag.sst <- read.wc(ptt, sstFile, type = 'sst', tag=tag, pop=pop)
    Error in findDateFormat(data$Date) : No correct date format was found.
    Calls: read.wc ... as.POSIXlt -> as.POSIXlt.character -> strptime -> findDateFormat
    Execution halted
    ```

# HTSSIP

Version: 1.3.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Sparsity threshold: 0.3 
      Density window: 1.72-1.75 
      Sparsity threshold: 0.2 
      Density window: 1.72-1.75 
      Sparsity threshold with the most rejected hypotheses: 0 
      testthat results ================================================================
      OK: 189 SKIPPED: 0 FAILED: 5
      1. Error: Beta diversity from a list of phyloseq objects (@test-BD_ordinations.R#2) 
      2. Error: Beta diversity from a list of phyloseq objects (parallel) (@test-BD_ordinations.R#9) 
      3. Error: Make a data.frame for ordination plotting (parallel) (@test-BD_ordinations.R#20) 
      4. Error: Plots created from phyloseq object (@test-BD_ordinations.R#46) 
      5. Error: Plot comparing all (@test-BD_ordinations.R#70) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    converting counts to integer mode
    converting counts to integer mode
    converting counts to integer mode
    converting counts to integer mode
    converting counts to integer mode
    converting counts to integer mode
    converting counts to integer mode
    converting counts to integer mode
    converting counts to integer mode
    converting counts to integer mode
    converting counts to integer mode
    converting counts to integer mode
    converting counts to integer mode
    converting counts to integer mode
    converting counts to integer mode
    converting counts to integer mode
    converting counts to integer mode
    Quitting from lines 68-72 (beta_diversity_ordinations.Rmd) 
    Error: processing vignette 'beta_diversity_ordinations.Rmd' failed with diagnostics:
    Incorrect number of arguments (7), expecting 5 for 'node_depth_edgelength'
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘igraph’
      All declared Imports should be used.
    ```

# hurricaneexposure

Version: 0.0.1

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘hurricaneexposuredata’
    ```

# huxtable

Version: 1.1.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      4: withCallingHandlers(withVisible(code), warning = handle_warning, message = handle_message)
      5: withVisible(code)
      6: rmarkdown::render("table-tester-2.Rmd", quiet = TRUE, output_format = "pdf_document")
      7: convert(output_file, run_citeproc)
      8: pandoc_convert(utf8_input, pandoc_to, output_format$pandoc$from, output, citeproc, 
             output_format$pandoc$args, !quiet)
      9: stop("pandoc document conversion failed with error ", result, call. = FALSE)
      
      testthat results ================================================================
      OK: 289 SKIPPED: 48 FAILED: 2
      1. Error: Row heights do not screw up LaTeX multicol (@test-with-pandoc.R#20) 
      2. Error: table-tester-2.Rmd renders without errors in LaTeX (@test-with-pandoc.R#27) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘xtable’
    ```

# hydrolinks

Version: 0.5

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      epsg (SRID):    NA
      proj4string:    +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs
      Reading layer `NHDWaterbody_projected' from data source `/home/muelleki/.local/share/hydrolinks/unzip/NHD_H_0709_Shape.zip/NHDWaterbody_projected.shp' using driver `ESRI Shapefile'
      Simple feature collection with 11444 features and 15 fields
      geometry type:  POLYGON
      dimension:      XY
      bbox:           xmin: 440230.1 ymin: 2040969 xmax: 630976.5 ymax: 2330831
      epsg (SRID):    NA
      proj4string:    +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs
      testthat results ================================================================
      OK: 8 SKIPPED: 0 FAILED: 1
      1. Error: test PIP linking across different datasets (@test_lake_linking.R#25) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘sp’
      All declared Imports should be used.
    ```

# IATscores

Version: 0.1-2

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    Gscores: no visible global function definition for ‘filter’
    MiniDscores: no visible global function definition for ‘filter’
    MiniDscores: no visible global function definition for ‘object.size’
    RobustScores: no visible global function definition for ‘filter’
    SplitHalf: no visible global function definition for ‘cor’
    TestRetest: no visible global function definition for ‘filter’
    TestRetest: no visible global function definition for ‘cor’
    WPRscores: no visible global function definition for ‘filter’
    WPRscores: no visible global function definition for ‘quantile’
    WPRscores: no visible global function definition for ‘sd’
    computeMinid: no visible global function definition for ‘sd’
    doP1P2: no visible global function definition for ‘filter’
    doP1P2: no visible global function definition for ‘sd’
    doP1P2P3P4: no visible global function definition for ‘filter’
    specialvar: no visible global function definition for ‘var’
    Undefined global functions or variables:
      cor filter object.size quantile sd var
    Consider adding
      importFrom("stats", "cor", "filter", "quantile", "sd", "var")
      importFrom("utils", "object.size")
    to your NAMESPACE file.
    ```

# ideal

Version: 1.0.0

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
    ideal: no visible binding for '<<-' assignment to ‘ideal_env’
    ideal : <anonymous>: no visible binding for global variable ‘airway’
    ideal : <anonymous>: no visible binding for global variable ‘ideal_env’
    Undefined global functions or variables:
      airway ideal_env
    ```

# idefix

Version: 0.1.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘MASS’ ‘Rdpack’
      All declared Imports should be used.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘shiny’, ‘bayesm’, ‘ChoiceModelR’, ‘RSGHB’
    ```

# IHW

Version: 1.4.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
        intersect, setdiff, setequal, union
    
    estimating size factors
    estimating dispersions
    gene-wise dispersion estimates
    mean-dispersion relationship
    final dispersion estimates
    fitting model and testing
    
    Attaching package: 'ggplot2'
    
    The following object is masked from 'package:IHW':
    
        alpha
    
    Warning: Removed 30633 rows containing missing values (geom_point).
    Warning: Removed 13044 rows containing missing values (geom_point).
    Error: processing vignette 'introduction_to_ihw.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    ihw.default: no visible global function definition for ‘p.adjust’
    ihw_convex: no visible global function definition for ‘gurobi’
    ihw_internal: no visible global function definition for ‘p.adjust’
    ihw_milp: no visible global function definition for ‘str’
    ihw_milp: no visible global function definition for ‘gurobi’
    plot_decisionboundary: no visible binding for global variable ‘stratum’
    plot_decisionboundary: no visible binding for global variable
      ‘covariate’
    plot_decisionboundary: no visible binding for global variable ‘pvalue’
    plot_decisionboundary: no visible binding for global variable ‘fold’
    thresholds_ihwResult: no visible global function definition for
      ‘na.exclude’
    thresholds,ihwResult: no visible global function definition for
      ‘na.exclude’
    Undefined global functions or variables:
      covariate fold gurobi mcols mcols<- metadata metadata<- na.exclude
      p.adjust pvalue runif str stratum
    Consider adding
      importFrom("stats", "na.exclude", "p.adjust", "runif")
      importFrom("utils", "str")
    to your NAMESPACE file.
    ```

# IHWpaper

Version: 1.4.0

## Newly broken

*   checking whether package ‘IHWpaper’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘dplyr::exprs’ by ‘Biobase::exprs’ when loading ‘IHWpaper’
    See ‘/home/muelleki/git/R/dplyr/revdep/checks/IHWpaper/new/IHWpaper.Rcheck/00install.out’ for details.
    ```

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Error: processing vignette 'BH-explanation.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 14.9Mb
      sub-directories of 1Mb or more:
        doc       3.4Mb
        extdata   9.8Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    scott_fdrreg: no visible global function definition for ‘FDRreg’
    scott_fdrreg: no visible global function definition for ‘getFDR’
    sim_fun_eval: no visible binding for global variable ‘fdr_method’
    sim_fun_eval: no visible binding for global variable ‘fdr_pars’
    sim_fun_eval: no visible binding for global variable ‘FDP’
    sim_fun_eval: no visible binding for global variable ‘rj_ratio’
    sim_fun_eval: no visible binding for global variable ‘FPR’
    sim_fun_eval: no visible binding for global variable ‘FWER’
    Undefined global functions or variables:
      FDP FDRreg FPR FWER fdr_method fdr_pars getFDR rj_ratio
    ```

# imager

Version: 0.40.2

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘spatstat’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 43.7Mb
      sub-directories of 1Mb or more:
        data      1.4Mb
        doc       4.9Mb
        extdata   1.0Mb
        include   2.8Mb
        libs     33.0Mb
    ```

# imfr

Version: 0.1.4

## In both

*   checking examples ... ERROR
    ```
    ...
    > ### Aliases: imf_ids
    > 
    > ### ** Examples
    > 
    > imf_ids()
    
    Downloading: 860 B     
    Downloading: 860 B     
    Downloading: 860 B     
    Downloading: 860 B     Request failed [404]. Retrying in 1 seconds...
    
    Downloading: 860 B     
    Downloading: 860 B     
    Downloading: 860 B     
    Downloading: 860 B     Request failed [404]. Retrying in 1.5 seconds...
    
    Downloading: 860 B     
    Downloading: 860 B     
    Downloading: 860 B     
    Downloading: 860 B     Error: data.imf.org appears to be down.
    Execution halted
    ```

# incadata

Version: 0.6.1

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > test_check("incadata")
      1. Failure: documents (@test-find_register.R#32) -------------------------------
      `messages` does not match "2 files downloaded to".
      Actual value: "1 files downloaded to /home/muelleki/tmp/Rtmp6Gv7wK/doc\n"
      
      
      Using tempfile: /home/muelleki/tmp/Rtmp6Gv7wK/ex_data925d28731f65.csv2
      [1] "ex_data925d28731f65.csv2"
      file size: 1103 kb
      testthat results ================================================================
      OK: 49 SKIPPED: 0 FAILED: 1
      1. Failure: documents (@test-find_register.R#32) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 568 marked UTF-8 strings
    ```

# incgraph

Version: 1.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘testthat’
      All declared Imports should be used.
    ```

# incR

Version: 1.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘rgeos’
      All declared Imports should be used.
    ```

# influxdbr

Version: 0.14.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      testthat results ================================================================
      OK: 0 SKIPPED: 0 FAILED: 12
      1. Error: connection (@test_query.R#12) 
      2. Error: single query no chunking (@test_query.R#24) 
      3. Error: multiple query no chunking (@test_query.R#65) 
      4. Error: single query with chunking (@test_query.R#84) 
      5. Error: multiple query with chunking (@test_query.R#101) 
      6. Error: multiple query with chunking and xts result (@test_query.R#118) 
      7. Error: connection (@test_schema_exploration.R#12) 
      8. Error: show commands (@test_schema_exploration.R#25) 
      9. Error: connection (@test_write.R#12) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

# inlabru

Version: 2.1.2

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘inlabru-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: cprod
    > ### Title: Cross product of integration points
    > ### Aliases: cprod
    > 
    > ### ** Examples
    > 
    > 
    > # Create integration points in dimension 'myDim' and 'myDiscreteDim' 
    > ips1 = ipoints(c(0,8), name = "myDim")
    Error in loadNamespace(name) : there is no package called ‘INLA’
    Calls: ipoints ... tryCatch -> tryCatchList -> tryCatchOne -> <Anonymous>
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘INLA’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘INLA’
    ```

# internetarchive

Version: 0.1.6

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(testthat)
      > library(internetarchive)
      > 
      > test_check("internetarchive")
      1. Failure: ia_metadata() returns a data frame (@test-files-download.R#32) -----
      `meta` not equal to reference from `hecker_meta.rds`.
      Different number of rows
      
      
      testthat results ================================================================
      OK: 21 SKIPPED: 0 FAILED: 1
      1. Failure: ia_metadata() returns a data frame (@test-files-download.R#32) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# IONiseR

Version: 2.0.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    The following object is masked from 'package:base':
    
        apply
    
    
    Attaching package: 'GenomicAlignments'
    
    The following object is masked from 'package:dplyr':
    
        last
    
    
    Attaching package: 'ShortRead'
    
    The following object is masked from 'package:dplyr':
    
        id
    
    Error: processing vignette 'IONiseR.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.4Mb
      sub-directories of 1Mb or more:
        doc       3.6Mb
        extdata   1.5Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      ‘start_time’
    readFast5Summary.mc: no visible binding for global variable ‘duration’
    readFast5Summary.mc: no visible binding for global variable
      ‘num_events’
    [,Fast5Summary-ANY-ANY-ANY: no visible binding for global variable
      ‘baseCalledTemplate’
    [,Fast5Summary-ANY-ANY-ANY: no visible binding for global variable
      ‘baseCalledComplement’
    [,Fast5Summary-ANY-ANY-ANY: no visible binding for global variable
      ‘component’
    [,Fast5Summary-ANY-ANY-ANY: no visible binding for global variable
      ‘idx’
    show,Fast5Summary: no visible binding for global variable ‘full_2D’
    show,Fast5Summary: no visible binding for global variable ‘pass’
    Undefined global functions or variables:
      := AAAAA TTTTT accumulation baseCalledComplement baseCalledTemplate
      bases_called category channel circleFun component duration error freq
      full_2D group hour idx matrixCol matrixRow meanZValue mean_value
      median_signal minute mux name nbases new_reads num_events oddEven
      pass pentamer rbindlist readIDs seq_length start_time time_bin
      time_group x y zvalue
    ```

# isomiRs

Version: 1.4.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    isoSelect.IsomirDataSeq: no visible binding for global variable ‘freq’
    isoSelect,IsomirDataSeq : <anonymous>: no visible binding for global
      variable ‘mir’
    isoSelect,IsomirDataSeq : <anonymous>: no visible binding for global
      variable ‘mism’
    isoSelect,IsomirDataSeq : <anonymous>: no visible binding for global
      variable ‘add’
    isoSelect,IsomirDataSeq : <anonymous>: no visible binding for global
      variable ‘t5’
    isoSelect,IsomirDataSeq : <anonymous>: no visible binding for global
      variable ‘t3’
    isoSelect,IsomirDataSeq : <anonymous>: no visible binding for global
      variable ‘id’
    isoSelect,IsomirDataSeq : <anonymous>: no visible binding for global
      variable ‘freq’
    isoSelect,IsomirDataSeq: no visible binding for global variable ‘freq’
    Undefined global functions or variables:
      Count DB X1 X2 add af ambiguity average change condition current
      enrich enrichGO error freq gene go group id mir mir_f mir_n mism
      mism_f mism_n ngene pct_abundance reference rowMax rowMin sel_genes
      t3 t5 term term_short type value y
    ```

# janeaustenr

Version: 0.1.5

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked UTF-8 string
    ```

# jpmesh

Version: 0.4.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘readr’
      All declared Imports should be used.
    ```

# jpndistrict

Version: 0.2.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 188 marked UTF-8 strings
    ```

# kableExtra

Version: 0.6.1

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘magick’
    ```

# keyholder

Version: 0.1.1

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      20: map_lgl(df[keep_cols], is.list)
      21: map_mold(.x, .f, logical(1), ...)
      22: vapply(.x, .f, .mold, ..., USE.NAMES = FALSE)
      23: df[keep_cols]
      24: `[.keyed_df`(df, keep_cols)
      25: `keys<-`(`*tmp*`, value = structure(list(vs = NA_real_, am = NA_real_), .Names = c("vs", 
         "am"), row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")))
      26: stop("Keys object should have the same number of rows as data.")
      
      testthat results ================================================================
      OK: 308 SKIPPED: 0 FAILED: 1
      1. Error: distinct works (@test-keyed-df-one-tbl.R#298) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# kokudosuuchi

Version: 0.4.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘stringi’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 52458 marked UTF-8 strings
    ```

# KraljicMatrix

Version: 0.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘tibble’
      All declared Imports should be used.
    ```

# labelled

Version: 1.0.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘memisc’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘memisc’
    ```

# Lahman

Version: 6.0-0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.5Mb
      sub-directories of 1Mb or more:
        data   7.2Mb
    ```

# leaflet.esri

Version: 0.2

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘geojsonio’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘htmlwidgets’ ‘shiny’
      All declared Imports should be used.
    ```

# leaflet.extras

Version: 0.2

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘geojsonio’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘shiny’
      All declared Imports should be used.
    ```

# lmeresampler

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘boot’
      All declared Imports should be used.
    ```

# LocFDRPois

Version: 1.0.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    AnalyticalOptim: no visible global function definition for ‘optim’
    LLConstructor : LL: no visible global function definition for ‘dpois’
    MixtureDensity: no visible global function definition for ‘glm’
    MixtureDensity : f_hat: no visible global function definition for
      ‘predict’
    NullDensity : f0: no visible global function definition for ‘dpois’
    Undefined global functions or variables:
      dpois glm optim predict
    Consider adding
      importFrom("stats", "dpois", "glm", "optim", "predict")
    to your NAMESPACE file.
    ```

# loon

Version: 1.1.0

## In both

*   checking whether package ‘loon’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/home/muelleki/git/R/dplyr/revdep/checks/loon/new/loon.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘loon’ ...
** package ‘loon’ successfully unpacked and MD5 sums checked
** R
** data
*** moving datasets to lazyload DB
** demo
** inst
** preparing package for lazy loading
Warning: no DISPLAY variable so Tk is not available
** help
*** installing help indices
*** copying figures
** building package indices
** installing vignettes
** testing if installed package can be loaded
Warning: no DISPLAY variable so Tk is not available
Error: package or namespace load failed for ‘loon’:
 .onLoad failed in loadNamespace() for 'loon', details:
  call: structure(.External(.C_dotTcl, ...), class = "tclObj")
  error: [tcl] couldn't connect to display "".

Error: loading failed
Execution halted
ERROR: loading failed
* removing ‘/home/muelleki/git/R/dplyr/revdep/checks/loon/new/loon.Rcheck/loon’

```
### CRAN

```
* installing *source* package ‘loon’ ...
** package ‘loon’ successfully unpacked and MD5 sums checked
** R
** data
*** moving datasets to lazyload DB
** demo
** inst
** preparing package for lazy loading
Warning: no DISPLAY variable so Tk is not available
** help
*** installing help indices
*** copying figures
** building package indices
** installing vignettes
** testing if installed package can be loaded
Warning: no DISPLAY variable so Tk is not available
Error: package or namespace load failed for ‘loon’:
 .onLoad failed in loadNamespace() for 'loon', details:
  call: structure(.External(.C_dotTcl, ...), class = "tclObj")
  error: [tcl] couldn't connect to display "".

Error: loading failed
Execution halted
ERROR: loading failed
* removing ‘/home/muelleki/git/R/dplyr/revdep/checks/loon/old/loon.Rcheck/loon’

```
# loopr

Version: 1.0.1

## In both

*   checking R code for possible problems ... NOTE
    ```
    amendColumns: no visible global function definition for ‘setNames’
    fillColumns: no visible global function definition for ‘setNames’
    Undefined global functions or variables:
      setNames
    Consider adding
      importFrom("stats", "setNames")
    to your NAMESPACE file.
    ```

# LymphoSeq

Version: 1.4.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.3Mb
      sub-directories of 1Mb or more:
        doc       2.6Mb
        extdata   5.5Mb
    ```

# macleish

Version: 0.3.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘DBI’
      All declared Imports should be used.
    ```

# mapedit

Version: 0.3.2

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘geojsonio’
    ```

# mason

Version: 0.2.5

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘ggplot2’, ‘pander’, ‘pixiedust’
    ```

# MCbiclust

Version: 1.0.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.0Mb
      sub-directories of 1Mb or more:
        data   3.2Mb
        doc    4.9Mb
    ```

# mdsr

Version: 0.1.4

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.5Mb
      sub-directories of 1Mb or more:
        data   5.4Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tidyverse’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2698 marked UTF-8 strings
    ```

# metagenomeFeatures

Version: 1.8.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.8Mb
      sub-directories of 1Mb or more:
        extdata   6.7Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    .mgDb_annotateFeatures: no visible binding for global variable
      ‘db_keys’
    .select.taxa: no visible binding for global variable ‘Keys’
    .select.taxa: no visible binding for global variable ‘.’
    aggregate_taxa: no visible binding for global variable ‘.’
    aggregate_taxa: no visible binding for global variable ‘index’
    vignette_pheno_data: no visible global function definition for
      ‘read.csv’
    Undefined global functions or variables:
      . Keys db_keys index read.csv
    Consider adding
      importFrom("utils", "read.csv")
    to your NAMESPACE file.
    ```

# MlBayesOpt

Version: 0.3.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘data.table’ ‘foreach’
      All declared Imports should be used.
    ```

# mnis

Version: 0.2.7

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat-a.R’ failed.
    Last 13 lines of output:
      1. Failure: mnis_additional returns expected format (@test_additional.R#80) ----
      `xi` has length 296, not length 285.
      
      
      2. Failure: mnis_extra returns expected format (@test_extra.R#10) --------------
      `xmnise` has length 192, not length 188.
      
      
      testthat results ================================================================
      OK: 97 SKIPPED: 0 FAILED: 2
      1. Failure: mnis_additional returns expected format (@test_additional.R#80) 
      2. Failure: mnis_extra returns expected format (@test_extra.R#10) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# modelr

Version: 0.1.1

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘lme4’, ‘rstanarm’
    ```

# modeval

Version: 0.1.3

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Warning in engine$weave(file, quiet = quiet, encoding = enc) :
      The vignette engine knitr::rmarkdown is not available, because the rmarkdown package is not installed. Please install it.
    Warning: Deprecated
    Quitting from lines 112-115 (modeval.Rmd) 
    Error: processing vignette 'modeval.Rmd' failed with diagnostics:
    the argument has already been evaluated
    Execution halted
    ```

# Momocs

Version: 1.2.2

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.0Mb
      sub-directories of 1Mb or more:
        data   1.5Mb
        doc    2.3Mb
    ```

# MonetDB.R

Version: 1.0.1

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘MonetDBLite’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Missing or unexported objects:
      ‘dplyr::base_agg’ ‘dplyr::base_scalar’ ‘dplyr::build_sql’
      ‘dplyr::is.ident’ ‘dplyr::sql_infix’ ‘dplyr::sql_prefix’
      ‘dplyr::sql_translator’ ‘dplyr::sql_variant’ ‘dplyr::src_sql’
      ‘dplyr::tbl_sql’
    ```

# MonetDBLite

Version: 0.5.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 18.9Mb
      sub-directories of 1Mb or more:
        libs  18.6Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# monkeylearn

Version: 0.1.3

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
             out$push(condition)
             invokeRestart("muffleMessage")
         })
      4: monkeylearn_extract(request = c("  ", "I do not know what the horse is doing in the house"))
      5: monkeylearn_check(output)
      6: stop("HTTP failure: ", req$status_code, "\n", content(req)$detail, call. = FALSE)
      
      testthat results ================================================================
      OK: 4 SKIPPED: 0 FAILED: 3
      1. Error: monkeylearn_parse returns a data.frame with a data.frame as attribute (@test_output.R#7) 
      2. Error: No error if no results from the extractor call (@test_output.R#55) 
      3. Error: Blank texts are handled properly (@test_text_size.R#10) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# monocle

Version: 2.4.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
      the condition has length > 1 and only the first element will be used
    Warning in if (method == "num_genes") { :
      the condition has length > 1 and only the first element will be used
    Warning in if (method == "num_genes") { :
      the condition has length > 1 and only the first element will be used
    Warning in if (method == "num_genes") { :
      the condition has length > 1 and only the first element will be used
    Warning in if (method == "num_genes") { :
      the condition has length > 1 and only the first element will be used
    Warning in if (method == "num_genes") { :
      the condition has length > 1 and only the first element will be used
    Warning: Deprecated, use tibble::rownames_to_column() instead.
    Warning: Removed 3576038 rows containing non-finite values (stat_density).
    Warning: Deprecated, use tibble::rownames_to_column() instead.
    Warning: Deprecated, use tibble::rownames_to_column() instead.
    Warning: Transformation introduced infinite values in continuous y-axis
    Warning: Transformation introduced infinite values in continuous y-axis
    Quitting from lines 327-334 (monocle-vignette.Rnw) 
    Error: processing vignette 'monocle-vignette.Rnw' failed with diagnostics:
    BLAS/LAPACK routine 'DLASCL' gave error code -4
    Execution halted
    ```

*   checking R code for possible problems ... NOTE
    ```
    assign_cell_lineage: no visible global function definition for ‘nei’
    buildBranchCellDataSet: no visible global function definition for ‘nei’
    clusterCells: no visible binding for global variable ‘rho’
    clusterCells: no visible binding for global variable ‘delta’
    count_leaf_descendents: no visible global function definition for ‘nei’
    cth_classifier_cds: no visible global function definition for ‘nei’
    cth_classifier_cell: no visible global function definition for ‘nei’
    diff_test_helper: no visible binding for global variable ‘Size_Factor’
    extract_good_ordering: no visible global function definition for ‘nei’
    fit_model_helper: no visible binding for global variable ‘Size_Factor’
    get_next_node_id: no visible binding for '<<-' assignment to
      ‘next_node’
    get_next_node_id: no visible binding for global variable ‘next_node’
    make_canonical: no visible global function definition for ‘nei’
    measure_diameter_path: no visible global function definition for ‘nei’
    orderCells: no visible binding for '<<-' assignment to ‘next_node’
    project2MST: no visible global function definition for ‘nei’
    Undefined global functions or variables:
      Size_Factor delta nei next_node rho
    ```

# mosaic

Version: 1.1.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘manipulate’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘cubature’
    ```

# mosaicData

Version: 0.14.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 7 marked UTF-8 strings
    ```

# mosaicModel

Version: 0.3.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘MASS’ ‘caret’ ‘ggformula’ ‘knitr’ ‘testthat’ ‘tidyverse’
      All declared Imports should be used.
    ```

# mousetrap

Version: 3.1.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.9Mb
      sub-directories of 1Mb or more:
        data   2.0Mb
        libs   4.5Mb
    ```

# mrgsolve

Version: 0.8.10

## Newly broken

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
         ID   end delta       add   GRP
      <int> <dbl> <dbl>    <list> <dbl>
    1     1    24     6 <dbl [3]>     1
    2     2    48     6 <dbl [1]>     0
    3     3    72     6 <dbl [1]>     1
    4     4    96     6 <dbl [1]>     0
    > 
    > l <- as_deslist(idata,"GRP")
    Error: distinct() does not support columns of type `list`
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.9Mb
      sub-directories of 1Mb or more:
        libs   5.7Mb
    ```

# mscstexta4r

Version: 0.1.2

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > # quick notes:
      > #   put all your tests in tests/testthat folder
      > #   each test file should start with test and end in .R
      > #   since we use secret API keys, don't run the tests on CRAN
      > 
      > library("testthat")
      > 
      > if (identical(tolower(Sys.getenv("NOT_CRAN")), "true")) {
      +   library("mscstexta4r")
      +   test_check("mscstexta4r")
      + }
      Error: mscstexta4r: could not load config info from Sys env nor from file
      testthat results ================================================================
      OK: 0 SKIPPED: 0 FAILED: 0
      Execution halted
    ```

# MSnID

Version: 1.10.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    .read_mzIDs.mzR.engine.single.file: no visible binding for global
      variable ‘modification’
    .read_mzIDs.mzR.engine.single.file: no visible binding for global
      variable ‘DatabaseAccess’
    .read_mzIDs.mzR.engine.single.file: no visible binding for global
      variable ‘DatabaseDescription’
    .read_mzIDs.mzR.engine.single.file: no visible binding for global
      variable ‘DBseqLength’
    infer_parsimonious_accessions,MSnID : infer_acc: no visible binding for
      global variable ‘accession’
    infer_parsimonious_accessions,MSnID : infer_acc: no visible binding for
      global variable ‘pepSeq’
    recalibrate,MSnID: no visible global function definition for ‘median’
    recalibrate,MSnID: no visible global function definition for ‘density’
    Undefined global functions or variables:
      DBseqLength DatabaseAccess DatabaseDescription accession density i
      location mass median modification name optim pepSeq quantile rnorm
      spectrumID
    Consider adding
      importFrom("stats", "density", "median", "optim", "quantile", "rnorm")
    to your NAMESPACE file.
    ```

# mudata2

Version: 1.0.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘hms’ ‘methods’
      All declared Imports should be used.
    ```

# myTAI

Version: 0.6.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.8Mb
      sub-directories of 1Mb or more:
        data   2.0Mb
        doc    2.7Mb
    ```

# nandb

Version: 0.2.1

## In both

*   checking package dependencies ... ERROR
    ```
    Packages required but not available: ‘autothresholdr’ ‘EBImage’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# nesRdata

Version: 0.1.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 5 marked UTF-8 strings
    ```

# netprioR

Version: 1.2.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
    Warning: executing %dopar% sequentially: no parallel backend registered
    Warning in block_exec(params) :
      failed to tidy R code in chunk <unnamed-chunk-10>
    reason: Error in loadNamespace(name) : there is no package called 'formatR'
    
    Warning in block_exec(params) :
      failed to tidy R code in chunk <unnamed-chunk-11>
    reason: Error in loadNamespace(name) : there is no package called 'formatR'
    
    Warning in block_exec(params) :
      failed to tidy R code in chunk <unnamed-chunk-12>
    reason: Error in loadNamespace(name) : there is no package called 'formatR'
    
    Warning in block_exec(params) :
      failed to tidy R code in chunk <unnamed-chunk-13>
    reason: Error in loadNamespace(name) : there is no package called 'formatR'
    
    Error: processing vignette 'netprioR.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

# neuropsychology

Version: 0.5.0

## Newly broken

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
    Error in typeof(x) : argument "vars" is missing, with no default
    Calls: fa_loadings ... vars_select_eval -> scoped_vars -> poke_vars -> is_null -> typeof
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘htmlTable’ ‘lme4’ ‘stringi’
      All declared Imports should be used.
    ```

# NFP

Version: 0.99.2

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘NFPdata’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  8.1Mb
      sub-directories of 1Mb or more:
        data   7.5Mb
    ```

# nlmixr

Version: 0.9.0-1

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      testthat results ================================================================
      OK: 195 SKIPPED: 0 FAILED: 11
      1. Failure: ODE (@test-model68.R#64) 
      2. Failure: ODE (@test-model68.R#65) 
      3. Failure: ODE (@test-model68.R#66) 
      4. Failure: ODE (@test-model68.R#68) 
      5. Failure: ODE (@test-model68.R#69) 
      6. Failure: ODE (@test-model68.R#71) 
      7. Failure: ODE (@test-model68.R#72) 
      8. Failure: ODE (@test-model68.R#73) 
      9. Failure: ODE (@test-model68.R#76) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking compiled code ... WARNING
    ```
    File ‘nlmixr/libs/nlmixr.so’:
      Found ‘__assert_fail’, possibly from ‘assert’ (C)
        Objects: ‘RcppExportMod.o’, ‘RcppExports.o’
    
    Compiled code should not call entry points which might terminate R nor
    write to stdout/stderr instead of to the console, nor the system RNG.
    
    See ‘Writing portable packages’ in the ‘Writing R Extensions’ manual.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 13.7Mb
      sub-directories of 1Mb or more:
        libs  12.5Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dparser’ ‘inline’ ‘n1qn1’
      All declared Imports should be used.
    ```

# noaastormevents

Version: 0.1.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘hurricaneexposuredata’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘RColorBrewer’ ‘XML’ ‘choroplethr’ ‘choroplethrMaps’ ‘data.table’
      ‘forcats’ ‘hurricaneexposure’ ‘plyr’
      All declared Imports should be used.
    ```

# nos

Version: 1.1.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘bipartite’
    ```

# nullabor

Version: 0.3.1

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    reg_dist: no visible global function definition for ‘lm’
    resid_boot: no visible global function definition for ‘resid’
    resid_pboot: no visible global function definition for ‘rnorm’
    resid_rotate: no visible global function definition for ‘rnorm’
    resid_rotate: no visible global function definition for ‘update’
    resid_rotate: no visible global function definition for ‘resid’
    resid_sigma: no visible global function definition for ‘rnorm’
    rorschach: no visible global function definition for ‘rbinom’
    rss: no visible global function definition for ‘resid’
    sep_dist: no visible global function definition for ‘dist’
    sep_dist: no visible global function definition for ‘cutree’
    sep_dist: no visible global function definition for ‘hclust’
    uni_dist: no visible global function definition for ‘sd’
    Undefined global functions or variables:
      coef cutree dist filter hclust lm predict quantile rbinom resid rnorm
      sd update
    Consider adding
      importFrom("stats", "coef", "cutree", "dist", "filter", "hclust", "lm",
                 "predict", "quantile", "rbinom", "resid", "rnorm", "sd",
                 "update")
    to your NAMESPACE file.
    ```

# nycflights13

Version: 0.2.2

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.0Mb
      sub-directories of 1Mb or more:
        data   6.9Mb
    ```

# nzelect

Version: 0.4.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 6409 marked UTF-8 strings
    ```

# observer

Version: 0.1.2

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘ensurer’, ‘validate’
    ```

# oec

Version: 2.5

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 4 marked UTF-8 strings
    ```

# officer

Version: 0.1.8

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘officer-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: slip_in_img
    > ### Title: append an image
    > ### Aliases: slip_in_img
    > 
    > ### ** Examples
    > 
    > library(magrittr)
    > img.file <- file.path( Sys.getenv("R_HOME"), "doc", "html", "logo.jpg" )
    > x <- read_docx() %>%
    +   body_add_par("R logo: ", style = "Normal") %>%
    +   slip_in_img(src = img.file, style = "strong", width = .3, height = .3)
    Error: file.exists(src) is not TRUE
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      6: freduce(value, `_function_list`)
      7: withVisible(function_list[[k]](value))
      8: function_list[[k]](value)
      9: ph_with_img(., type = "body", src = img.file, height = 1.06, width = 1.39)
      10: external_img(src, width = width/914400, height = height/914400)
      11: stopifnot(file.exists(src))
      12: stop(msg, call. = FALSE, domain = NA)
      
      testthat results ================================================================
      OK: 341 SKIPPED: 1 FAILED: 2
      1. Error: image add  (@test-docx-add.R#68) 
      2. Error: add img into placeholder (@test-pptx-add.R#67) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Quitting from lines 180-190 (powerpoint.Rmd) 
    Error: processing vignette 'powerpoint.Rmd' failed with diagnostics:
    file.exists(src) is not TRUE
    Execution halted
    ```

# OncoSimulR

Version: 2.6.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 11.0Mb
      sub-directories of 1Mb or more:
        doc    5.4Mb
        libs   4.9Mb
    ```

# openadds

Version: 0.2.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      > library("testthat")
      > test_check("openadds")
      Loading required package: openadds
      1. Failure: oa_search works (@test-oa_search.R#39) -----------------------------
      length(dd$city) not equal to 2.
      1/1 mismatches
      [1] 3 - 2 == 1
      
      
      testthat results ================================================================
      OK: 47 SKIPPED: 0 FAILED: 1
      1. Failure: oa_search works (@test-oa_search.R#39) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# opencage

Version: 0.1.2

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      3: `_f`(placename = placename, key = key, bounds = bounds, countrycode = countrycode, 
             language = language, limit = limit, min_confidence = min_confidence, no_annotations = no_annotations, 
             no_dedupe = no_dedupe, no_record = no_record, abbrv = abbrv)
      4: opencage_check(temp)
      5: stop("HTTP failure: ", req$status_code, "\n", message, call. = FALSE)
      
      testthat results ================================================================
      OK: 23 SKIPPED: 0 FAILED: 4
      1. Error: opencage_parse returns what it should for both functions (@test-opencage_parse.R#6) 
      2. Error: opencage_parse returns what it should for both functions with several parameters (@test-opencage_parse.R#40) 
      3. Error: opencage_parse deals well with resuls being NULL (@test-opencage_parse.R#72) 
      4. Error: the bounds argument is well taken into account (@test-opencage_parse.R#86) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 41-47 (opencage.Rmd) 
    Error: processing vignette 'opencage.Rmd' failed with diagnostics:
    HTTP failure: 403
    Invalid or missing api key (forbidden)
    Execution halted
    ```

# opendotaR

Version: 0.1.4

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# openwindfarm

Version: 0.1.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Warning in engine$weave(file, quiet = quiet, encoding = enc) :
      The vignette engine knitr::rmarkdown is not available, because the rmarkdown package is not installed. Please install it.
    Quitting from lines 22-24 (openwindfarm-vignette.Rmd) 
    Error: processing vignette 'openwindfarm-vignette.Rmd' failed with diagnostics:
    there is no package called 'webshot'
    Execution halted
    ```

# Organism.dplyr

Version: 1.2.2

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Error: processing vignette 'Organism.dplyr.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
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

# parlitools

Version: 0.2.1

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 13 marked UTF-8 strings
    ```

# parsemsf

Version: 0.1.0

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘parsemsf-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: make_area_table
    > ### Title: Make a table of peptide areas
    > ### Aliases: make_area_table
    > 
    > ### ** Examples
    > 
    > make_area_table(parsemsf_example("test_db.msf"))
    Error: Condition message must be a string
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      13: glubort(NULL, "The {name} package is required to {reason}.", if (install) "\nPlease install it with `install.packages(\"{name}\")`")
      14: .abort(text)
      15: cnd_error(type, .msg = msg, .call = sys.call(-1))
      16: new_cnd(c(.type, "error"), ..., .msg = .msg)
      17: stop("Condition message must be a string", call. = FALSE)
      
      Error: Condition message must be a string
      testthat results ================================================================
      OK: 0 SKIPPED: 0 FAILED: 3
      1. Error: make_area_table creates a data frame with the correct column names (@test_make_area_table.R#16) 
      2. Error: make_pep_table creates a data frame with the correct column names (@test_make_pep_table.R#13) 
      3. Error: map_peptides creates a data frame with the correct column names (@test_map_peptides.R#16) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 20-25 (introduction.Rmd) 
    Error: processing vignette 'introduction.Rmd' failed with diagnostics:
    Condition message must be a string
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘RSQLite’
      All declared Imports should be used.
    ```

# PathoStat

Version: 1.2.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Warning: Transformation introduced infinite values in discrete y-axis
    Error: processing vignette 'PathoStatAdvanced.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

# patternplot

Version: 0.2

## In both

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# perccalc

Version: 1.0.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘MASS’ ‘devtools’ ‘ggplot2’ ‘haven’ ‘tidyverse’
      All declared Imports should be used.
    ```

# petro.One

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘Rgraphviz’ ‘cluster’ ‘graph’
      All declared Imports should be used.
    ```

# philr

Version: 1.2.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
        intersect, setdiff, setequal, union
    
    Found more than one class "phylo" in cache; using the first, from namespace 'treeio'
    Also defined by 'phyloseq'
    Found more than one class "phylo" in cache; using the first, from namespace 'treeio'
    Also defined by 'phyloseq'
    
    Attaching package: 'tidyr'
    
    The following object is masked from 'package:ggtree':
    
        expand
    
    The following object is masked from 'package:Matrix':
    
        expand
    
    Error: processing vignette 'philr-intro.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

*   checking R code for possible problems ... NOTE
    ```
    name.balance: no visible global function definition for ‘as’
    vote.annotation: no visible global function definition for ‘is’
    Undefined global functions or variables:
      as is
    Consider adding
      importFrom("methods", "as", "is")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘compositions’
    ```

# pitchRx

Version: 1.8.2

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘ggsubplot’
    ```

# PKNCA

Version: 0.8.1

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      The following objects are masked from 'package:base':
      
          intersect, setdiff, setequal, union
      
      1. Failure: provenance (@test-provenance.R#7) ----------------------------------
      <... isn't true.
      A correct time is set in provenance
      
      Provenance hash a generated on b with c.
      testthat results ================================================================
      OK: 1024 SKIPPED: 0 FAILED: 1
      1. Failure: provenance (@test-provenance.R#7) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# PKPDmisc

Version: 2.0.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘purrr’
      All declared Imports should be used.
    ```

# PogromcyDanych

Version: 1.5

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.2Mb
      sub-directories of 1Mb or more:
        data   6.0Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 7256 marked UTF-8 strings
    ```

# pointblank

Version: 0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘Hmisc’ ‘digest’ ‘htmltools’ ‘knitr’ ‘lazyWeave’ ‘lubridate’ ‘rJava’
      All declared Imports should be used.
    ```

# poio

Version: 0.0-3

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘ISOcodes’
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 8 marked UTF-8 strings
    ```

# powerlmm

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘testthat’
      All declared Imports should be used.
    ```

# prisonbrief

Version: 0.1.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2 marked UTF-8 strings
    ```

# pRoloc

Version: 1.16.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
        nChrom
    
    Loading required package: cluster
    Warning: replacing previous import 'BiocGenerics::var' by 'stats::var' when loading 'MLInterfaces'
    
    This is pRoloc version 1.16.1 
      Read '?pRoloc' and references therein for information
      about the package and how to get started.
    
    
    This is pRolocdata version 1.14.0.
    Use 'pRolocdata()' to list available data sets.
    Loading required namespace: GO.db
    
    Loading required package: GO.db
    Retaining 84 out of 524 in GOAnnotations
    Retaining 79 out of 84 in GOAnnotations
    Error: processing vignette 'pRoloc-goannotations.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.8Mb
      sub-directories of 1Mb or more:
        doc   3.3Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unexported objects imported by ':::' calls:
      ‘MLInterfaces:::.macroF1’ ‘MLInterfaces:::.precision’
      ‘MLInterfaces:::.recall’ ‘MLInterfaces:::es2df’
      ‘caret:::predict.plsda’
      See the note in ?`:::` about the use of this operator.
    There are ::: calls to the package's namespace in its code. A package
      almost never needs to use ::: for its own objects:
      ‘checkSortedFeatureNames’ ‘opt’
    ```

*   checking R code for possible problems ... NOTE
    ```
    Found the following possibly unsafe calls:
    File ‘pRoloc/R/annotation.R’:
      unlockBinding("params", .pRolocEnv)
    ```

*   checking files in ‘vignettes’ ... NOTE
    ```
    The following directory looks like a leftover from 'knitr':
      ‘figure’
    Please remove from your package.
    ```

# pRolocGUI

Version: 1.10.0

## In both

*   checking whether package ‘pRolocGUI’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: namespace ‘lme4’ is not available and has been replaced
      Warning: namespace ‘MatrixModels’ is not available and has been replaced
    See ‘/home/muelleki/git/R/dplyr/revdep/checks/pRolocGUI/new/pRolocGUI.Rcheck/00install.out’ for details.
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Authors@R field gives more than one person with maintainer role:
      Lisa Breckels <lms79@cam.ac.uk> [aut, cre]
      Laurent Gatto <lg390@cam.ac.uk> [aut, cre]
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unexported object imported by a ':::' call: ‘pRoloc:::remap’
      See the note in ?`:::` about the use of this operator.
    ```

# prophet

Version: 0.2.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 22.9Mb
      sub-directories of 1Mb or more:
        libs  21.7Mb
    ```

# proteoQC

Version: 1.12.3

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Error: processing vignette 'proteoQC.Rmd' failed with diagnostics:
    there is no package called ‘prettydoc’
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  8.0Mb
      sub-directories of 1Mb or more:
        doc       3.2Mb
        extdata   4.0Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    plotMS2boxplot: no visible binding for global variable ‘techRep’
    plotMS2boxplot: no visible binding for global variable ‘fraction’
    plotMS2boxplot: no visible binding for global variable ‘MS2QC’
    plotSampleIDResultErrorBar: no visible binding for global variable
      ‘fraction’
    plotSampleIDResultErrorBar: no visible binding for global variable
      ‘val’
    plotSampleIDResultErrorBar: no visible binding for global variable ‘se’
    plotSampleVenn: no visible global function definition for ‘grid.draw’
    plotTechRepVenn : <anonymous>: no visible global function definition
      for ‘grid.draw’
    qcHist: no visible binding for global variable ‘error’
    qcHist: no visible binding for global variable ‘techRep’
    qcHist: no visible binding for global variable ‘bioRep’
    qcHist2: no visible binding for global variable ‘error’
    qcHist2: no visible binding for global variable ‘fractile’
    Undefined global functions or variables:
      ..count.. Intensity MS1QC MS2QC TMT10 TMT6 Tag V1 V2 V3 V4 V5 bioRep
      curenv delta error exprs fractile fraction grid.draw iTRAQ4 iTRAQ8
      label peplength peptide_summary precursorCharge quantify ratio
      readMgfData se techRep val x y
    ```

# proustr

Version: 0.2.1

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 20105 marked UTF-8 strings
    ```

# psychmeta

Version: 0.1.1

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 5 marked UTF-8 strings
    ```

# psycho

Version: 0.0.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘rtf’ ‘tidyverse’
      All declared Imports should be used.
    ```

# ptstem

Version: 0.0.3

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        dict   5.1Mb
    ```

# purrrlyr

Version: 0.0.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘lazyeval’
      All declared Imports should be used.
    ```

# qdap

Version: 2.2.8

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘gplots’
    ```

# quadmesh

Version: 0.1.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘rgl’
    ```

# qualvar

Version: 0.1.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Warning in engine$weave(file, quiet = quiet, encoding = enc) :
      The vignette engine knitr::rmarkdown is not available, because the rmarkdown package is not installed. Please install it.
    Quitting from lines 92-106 (wilcox1973.Rmd) 
    Error: processing vignette 'wilcox1973.Rmd' failed with diagnostics:
    there is no package called 'webshot'
    Execution halted
    ```

# QuaternaryProd

Version: 1.4.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 12.1Mb
      sub-directories of 1Mb or more:
        extdata   9.7Mb
        libs      1.9Mb
    ```

# questionr

Version: 0.6.2

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘Hmisc’
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 4145 marked UTF-8 strings
    ```

# quickReg

Version: 1.5.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘psych’
      All declared Imports should be used.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘PredictABEL’
    ```

# quokar

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘MCMCpack’ ‘gridExtra’ ‘knitr’
      All declared Imports should be used.
    ```

# qwraps2

Version: 0.2.4

## In both

*   checking Rd \usage sections ... NOTE
    ```
    S3 methods shown with full name in documentation object 'summary_table':
      ‘cbind.qwraps2_summary_table’
    
    The \usage entries for S3 methods should use the \method markup and not
    their full name.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

# r2glmm

Version: 0.1.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘data.table’ ‘dplyr’ ‘lmerTest’
      All declared Imports should be used.
    ```

# R6Frame

Version: 0.1.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      testthat results ================================================================
      OK: 139 SKIPPED: 0 FAILED: 10
      1.  Error: gather works with R6 data.frame (@test-tidyr_reshape.R#11) 
      2.  Error: gather works with R6 data.table (@test-tidyr_reshape.R#25) 
      3.  Error: gather works with R6 tbl_df (@test-tidyr_reshape.R#38) 
      4.  Error: spread works with R6 data.frame (@test-tidyr_reshape.R#52) 
      5.  Error: spread works with R6 data.table (@test-tidyr_reshape.R#67) 
      6.  Error: spread works with R6 tbl_df (@test-tidyr_reshape.R#81) 
      7.  Error: complete works with R6 data.frame (@test-tidyr_verbs.R#13) 
      8.  Error: complete works with R6 data.table (@test-tidyr_verbs.R#27) 
      9.  Error: complete works with R6 tbl_df (@test-tidyr_verbs.R#41) 
      10. Error: fill works with R6 tbl_df (@test-tidyr_verbs.R#72) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# radiant.model

Version: 0.8.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2 marked UTF-8 strings
    ```

# radiant.multivariate

Version: 0.8.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked UTF-8 string
    ```

# randomForestExplainer

Version: 0.9

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘MASS’ ‘dtplyr’
      All declared Imports should be used.
    ```

# raptr

Version: 0.0.5

## In both

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking: ‘gurobi’ ‘rgurobi’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 17.9Mb
      sub-directories of 1Mb or more:
        data   3.6Mb
        libs  13.1Mb
    ```

# Rariant

Version: 1.12.0

## In both

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
      installed size is  7.8Mb
      sub-directories of 1Mb or more:
        doc       2.3Mb
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

# rattle

Version: 5.1.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
      Running 'texi2dvi' on 'rattle.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `algorithm2e.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    
    ! Emergency stop.
    <read *> 
             
    l.14 \usepackage
                    [^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    Calls: buildVignettes -> texi2pdf -> texi2dvi
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      ‘gWidgetsRGtk2’ ‘playwith’
    ```

*   checking dependencies in R code ... NOTE
    ```
    
    (R:71248): Gtk-WARNING **: gtk_disable_setlocale() must be called before gtk_init()
    ```

# RBesT

Version: 1.2-3

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 27.7Mb
      sub-directories of 1Mb or more:
        libs  26.4Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘lme4’
      All declared Imports should be used.
    ```

# rbison

Version: 0.5.4

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      
      
      3. Failure: bison_tax returns the correct ... (@test-bison_tax.R#14) -----------
      out3$names$vernacularName not equal to "black bear".
      Lengths differ: 6 vs 1
      
      
      testthat results ================================================================
      OK: 37 SKIPPED: 0 FAILED: 3
      1. Failure: bison returns the correct value (@test-bison.R#16) 
      2. Failure: bison_tax returns the correct ... (@test-bison_tax.R#12) 
      3. Failure: bison_tax returns the correct ... (@test-bison_tax.R#14) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# rccmisc

Version: 0.3.7

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# rcongresso

Version: 0.2.1

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(rcongresso)
      > 
      > test_check("rcongresso")
      Error: Column `id` must be a 1d atomic vector or a list
      In addition: Warning message:
      Unknown or uninitialised column: 'id'. 
      testthat results ================================================================
      OK: 18 SKIPPED: 0 FAILED: 0
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Quitting from lines 45-50 (introducao-rcongresso.Rmd) 
    Error: processing vignette 'introducao-rcongresso.Rmd' failed with diagnostics:
    Column `id` must be a 1d atomic vector or a list
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘utils’
      All declared Imports should be used.
    ```

# rcrossref

Version: 0.7.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      3. Failure: cr_members fails correctly (@test_cr_members.R#47) -----------------
      `warnings` does not match "500: java.lang.NumberFormatException: For input string: \"323234343434\"".
      Actual values:
      * 500: {"status":"error","message-type":"exception","message-version":"1.0.0","message":{"name":"class java.lang.NumberFormatException","description":"java.lang.NumberFormatException: For input string: \\"323234343434\\"","message":"For input string: \\"323234343434\\"","stack":["java.lang.NumberFormatException.forInputString(NumberFormatException.java:65)","java.lang.Integer.parseInt(Integer.java:583)","java.lang.Integer.parseInt(Integer.java:615)","cayenne.data.member$get_id_from_context.invokeStatic(member.clj:34)","cayenne.data.member$get_id_from_context.invoke(member.clj:30)","cayenne.data.member$fetch_one$fn__17569.invoke(member.clj:75)","cayenne.data.member$fetch_one.invokeStatic(member.clj:72)","cayenne.data.member$fetch_one.invoke(member.clj:71)","cayenne.api.v1.routes$member_works_resource$fn__22177$fn__22178.invoke(routes.clj:375)","liberator.core$decide.invokeStatic(core.clj:98)","liberator.core$decide.invoke(core.clj:91)","liberator.core$exists_QMARK_.invokeStatic(core.clj:406)","liberator.core$exists_QMARK_.invoke(core.clj:406)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$processable_QMARK_.invokeStatic(core.clj:409)","liberator.core$processable_QMARK_.invoke(core.clj:409)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$encoding_available_QMARK_.invokeStatic(core.clj:413)","liberator.core$encoding_available_QMARK_.invoke(core.clj:413)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$accept_encoding_exists_QMARK_.invokeStatic(core.clj:428)","liberator.core$accept_encoding_exists_QMARK_.invoke(core.clj:428)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$accept_charset_exists_QMARK_.invokeStatic(core.clj:441)","liberator.core$accept_charset_exists_QMARK_.invoke(core.clj:441)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$accept_language_exists_QMARK_.invokeStatic(core.clj:455)","liberator.core$accept_language_exists_QMARK_.invoke(core.clj:455)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$media_type_available_QMARK_.invokeStatic(core.clj:465)","liberator.core$media_type_available_QMARK_.invoke(core.clj:465)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$accept_exists_QMARK_.invokeStatic(core.clj:468)","liberator.core$accept_exists_QMARK_.invoke(core.clj:468)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$is_options_QMARK_.invokeStatic(core.clj:485)","liberator.core$is_options_QMARK_.invoke(core.clj:485)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$valid_entity_length_QMARK_.invokeStatic(core.clj:488)","liberator.core$valid_entity_length_QMARK_.invoke(core.clj:488)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$known_content_type_QMARK_.invokeStatic(core.clj:491)","liberator.core$known_content_type_QMARK_.invoke(core.clj:491)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$valid_content_header_QMARK_.invokeStatic(core.clj:493)","liberator.core$valid_content_header_QMARK_.invoke(core.clj:493)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$allowed_QMARK_.invokeStatic(core.clj:496)","liberator.core$allowed_QMARK_.invoke(core.clj:496)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$authorized_QMARK_.invokeStatic(core.clj:499)","liberator.core$authorized_QMARK_.invoke(core.clj:499)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$malformed_QMARK_.invokeStatic(core.clj:502)","liberator.core$malformed_QMARK_.invoke(core.clj:502)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$method_allowed_QMARK_.invokeStatic(core.clj:505)","liberator.core$method_allowed_QMARK_.invoke(core.clj:505)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$uri_too_long_QMARK_.invokeStatic(core.clj:508)","liberator.core$uri_too_long_QMARK_.invoke(core.clj:508)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$known_method_QMARK_.invokeStatic(core.clj:511)","liberator.core$known_method_QMARK_.invoke(core.clj:511)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$service_available_QMARK_.invokeStatic(core.clj:514)","liberator.core$service_available_QMARK_.invoke(core.clj:514)","liberator.core$run_resource.invokeStatic(core.clj:599)","liberator.core$run_resource.invoke(core.clj:597)","cayenne.api.v1.routes$member_works_resource$fn__22177.invoke(routes.clj:364)","compojure.response$eval21288$fn__21289.invoke(response.clj:47)","compojure.response$eval21210$fn__21211$G__21201__21218.invoke(response.clj:7)","compojure.core$wrap_response$fn__21880.invoke(core.clj:158)","compojure.core$wrap_route_middleware$fn__21864.invoke(core.clj:128)","compojure.core$wrap_route_info$fn__21869.invoke(core.clj:137)","compojure.core$wrap_route_matches$fn__21873.invoke(core.clj:146)","compojure.core$routing$fn__21888.invoke(core.clj:185)","clojure.core$some.invokeStatic(core.clj:2592)","clojure.core$some.invoke(core.clj:2583)","compojure.core$routing.invokeStatic(core.clj:185)","compojure.core$routing.doInvoke(core.clj:182)","clojure.lang.RestFn.applyTo(RestFn.java:139)","clojure.core$apply.invokeStatic(core.clj:648)","clojure.core$apply.invoke(core.clj:641)","compojure.core$routes$fn__21892.invoke(core.clj:192)","compojure.core$routing$fn__21888.invoke(core.clj:185)","clojure.core$some.invokeStatic(core.clj:2592)","clojure.core$some.invoke(core.clj:2583)","compojure.core$routing.invokeStatic(core.clj:185)","compojure.core$routing.doInvoke(core.clj:182)","clojure.lang.RestFn.applyTo(RestFn.java:139)","clojure.core$apply.invokeStatic(core.clj:648)","clojure.core$apply.invoke(core.clj:641)","compojure.core$routes$fn__21892.invoke(core.clj:192)","compojure.core$routing$fn__21888.invoke(core.clj:185)","clojure.core$some.invokeStatic(core.clj:2592)","clojure.core$some.invoke(core.clj:2583)","compojure.core$routing.invokeStatic(core.clj:185)","compojure.core$routing.doInvoke(core.clj:182)","clojure.lang.RestFn.applyTo(RestFn.java:139)","clojure.core$apply.invokeStatic(core.clj:648)","clojure.core$apply.invoke(core.clj:641)","compojure.core$routes$fn__21892.invoke(core.clj:192)","ring.middleware.logstash$wrap_logstash$fn__23209.invoke(logstash.clj:110)","ring.middleware.keyword_params$wrap_keyword_params$fn__24071.invoke(keyword_params.clj:36)","ring.middleware.nested_params$wrap_nested_params$fn__24119.invoke(nested_params.clj:89)","ring.middleware.params$wrap_params$fn__24035.invoke(params.clj:67)","cayenne.api.route$wrap_cors$fn__24655.invoke(route.clj:101)","metrics.ring.expose$expose_metrics_as_json$fn__23595.invoke(expose.clj:94)","metrics.ring.instrument$instrument$fn__23611$fn__23612.invoke(instrument.clj:44)","metrics.ring.instrument.proxy$java.lang.Object$Callable$7da976d4.call(Unknown Source)","com.yammer.metrics.core.Timer.time(Timer.java:91)","metrics.ring.instrument$instrument$fn__23611.invoke(instrument.clj:43)","heartbeat.ring$wrap_heartbeat$fn__23297.invoke(ring.clj:18)","cayenne.api.conneg$wrap_accept$fn__19751.invoke(conneg.clj:53)","cayenne.api.route$wrap_exception_handler$fn__24660.invoke(route.clj:110)","cayenne.api.route$wrap_ignore_trailing_slash$fn__24668.invoke(route.clj:136)","org.httpkit.server.HttpHandler.run(RingHandler.java:91)","java.util.concurrent.Executors$RunnableAdapter.call(Executors.java:511)","java.util.concurrent.Future
      * 500: {"status":"error","message-type":"exception","message-version":"1.0.0","message":{"name":"class java.lang.NumberFormatException","description":"java.lang.NumberFormatException: For input string: \\"3434343434\\"","message":"For input string: \\"3434343434\\"","stack":["java.lang.NumberFormatException.forInputString(NumberFormatException.java:65)","java.lang.Integer.parseInt(Integer.java:583)","java.lang.Integer.parseInt(Integer.java:615)","cayenne.data.member$get_id_from_context.invokeStatic(member.clj:34)","cayenne.data.member$get_id_from_context.invoke(member.clj:30)","cayenne.data.member$fetch_one$fn__17608.invoke(member.clj:75)","cayenne.data.member$fetch_one.invokeStatic(member.clj:72)","cayenne.data.member$fetch_one.invoke(member.clj:71)","cayenne.api.v1.routes$member_works_resource$fn__22216$fn__22217.invoke(routes.clj:375)","liberator.core$decide.invokeStatic(core.clj:98)","liberator.core$decide.invoke(core.clj:91)","liberator.core$exists_QMARK_.invokeStatic(core.clj:406)","liberator.core$exists_QMARK_.invoke(core.clj:406)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$processable_QMARK_.invokeStatic(core.clj:409)","liberator.core$processable_QMARK_.invoke(core.clj:409)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$encoding_available_QMARK_.invokeStatic(core.clj:413)","liberator.core$encoding_available_QMARK_.invoke(core.clj:413)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$accept_encoding_exists_QMARK_.invokeStatic(core.clj:428)","liberator.core$accept_encoding_exists_QMARK_.invoke(core.clj:428)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$accept_charset_exists_QMARK_.invokeStatic(core.clj:441)","liberator.core$accept_charset_exists_QMARK_.invoke(core.clj:441)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$accept_language_exists_QMARK_.invokeStatic(core.clj:455)","liberator.core$accept_language_exists_QMARK_.invoke(core.clj:455)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$media_type_available_QMARK_.invokeStatic(core.clj:465)","liberator.core$media_type_available_QMARK_.invoke(core.clj:465)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$accept_exists_QMARK_.invokeStatic(core.clj:468)","liberator.core$accept_exists_QMARK_.invoke(core.clj:468)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$is_options_QMARK_.invokeStatic(core.clj:485)","liberator.core$is_options_QMARK_.invoke(core.clj:485)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$valid_entity_length_QMARK_.invokeStatic(core.clj:488)","liberator.core$valid_entity_length_QMARK_.invoke(core.clj:488)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$known_content_type_QMARK_.invokeStatic(core.clj:491)","liberator.core$known_content_type_QMARK_.invoke(core.clj:491)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$valid_content_header_QMARK_.invokeStatic(core.clj:493)","liberator.core$valid_content_header_QMARK_.invoke(core.clj:493)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$allowed_QMARK_.invokeStatic(core.clj:496)","liberator.core$allowed_QMARK_.invoke(core.clj:496)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$authorized_QMARK_.invokeStatic(core.clj:499)","liberator.core$authorized_QMARK_.invoke(core.clj:499)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$malformed_QMARK_.invokeStatic(core.clj:502)","liberator.core$malformed_QMARK_.invoke(core.clj:502)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$method_allowed_QMARK_.invokeStatic(core.clj:505)","liberator.core$method_allowed_QMARK_.invoke(core.clj:505)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$uri_too_long_QMARK_.invokeStatic(core.clj:508)","liberator.core$uri_too_long_QMARK_.invoke(core.clj:508)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$known_method_QMARK_.invokeStatic(core.clj:511)","liberator.core$known_method_QMARK_.invoke(core.clj:511)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$service_available_QMARK_.invokeStatic(core.clj:514)","liberator.core$service_available_QMARK_.invoke(core.clj:514)","liberator.core$run_resource.invokeStatic(core.clj:599)","liberator.core$run_resource.invoke(core.clj:597)","cayenne.api.v1.routes$member_works_resource$fn__22216.invoke(routes.clj:364)","compojure.response$eval21327$fn__21328.invoke(response.clj:47)","compojure.response$eval21249$fn__21250$G__21240__21257.invoke(response.clj:7)","compojure.core$wrap_response$fn__21919.invoke(core.clj:158)","compojure.core$wrap_route_middleware$fn__21903.invoke(core.clj:128)","compojure.core$wrap_route_info$fn__21908.invoke(core.clj:137)","compojure.core$wrap_route_matches$fn__21912.invoke(core.clj:146)","compojure.core$routing$fn__21927.invoke(core.clj:185)","clojure.core$some.invokeStatic(core.clj:2592)","clojure.core$some.invoke(core.clj:2583)","compojure.core$routing.invokeStatic(core.clj:185)","compojure.core$routing.doInvoke(core.clj:182)","clojure.lang.RestFn.applyTo(RestFn.java:139)","clojure.core$apply.invokeStatic(core.clj:648)","clojure.core$apply.invoke(core.clj:641)","compojure.core$routes$fn__21931.invoke(core.clj:192)","compojure.core$routing$fn__21927.invoke(core.clj:185)","clojure.core$some.invokeStatic(core.clj:2592)","clojure.core$some.invoke(core.clj:2583)","compojure.core$routing.invokeStatic(core.clj:185)","compojure.core$routing.doInvoke(core.clj:182)","clojure.lang.RestFn.applyTo(RestFn.java:139)","clojure.core$apply.invokeStatic(core.clj:648)","clojure.core$apply.invoke(core.clj:641)","compojure.core$routes$fn__21931.invoke(core.clj:192)","compojure.core$routing$fn__21927.invoke(core.clj:185)","clojure.core$some.invokeStatic(core.clj:2592)","clojure.core$some.invoke(core.clj:2583)","compojure.core$routing.invokeStatic(core.clj:185)","compojure.core$routing.doInvoke(core.clj:182)","clojure.lang.RestFn.applyTo(RestFn.java:139)","clojure.core$apply.invokeStatic(core.clj:648)","clojure.core$apply.invoke(core.clj:641)","compojure.core$routes$fn__21931.invoke(core.clj:192)","ring.middleware.logstash$wrap_logstash$fn__23248.invoke(logstash.clj:110)","ring.middleware.keyword_params$wrap_keyword_params$fn__24110.invoke(keyword_params.clj:36)","ring.middleware.nested_params$wrap_nested_params$fn__24158.invoke(nested_params.clj:89)","ring.middleware.params$wrap_params$fn__24074.invoke(params.clj:67)","cayenne.api.route$wrap_cors$fn__24694.invoke(route.clj:101)","metrics.ring.expose$expose_metrics_as_json$fn__23634.invoke(expose.clj:94)","metrics.ring.instrument$instrument$fn__23650$fn__23651.invoke(instrument.clj:44)","metrics.ring.instrument.proxy$java.lang.Object$Callable$7da976d4.call(Unknown Source)","com.yammer.metrics.core.Timer.time(Timer.java:91)","metrics.ring.instrument$instrument$fn__23650.invoke(instrument.clj:43)","heartbeat.ring$wrap_heartbeat$fn__23336.invoke(ring.clj:18)","cayenne.api.conneg$wrap_accept$fn__19790.invoke(conneg.clj:53)","cayenne.api.route$wrap_exception_handler$fn__24699.invoke(route.clj:110)","cayenne.api.route$wrap_ignore_trailing_slash$fn__24707.invoke(route.clj:136)","org.httpkit.server.HttpHandler.run(RingHandler.java:91)","java.util.concurrent.Executors$RunnableAdapter.call(Executors.java:511)","java.util.concurrent.FutureTask
      
      
      testthat results ================================================================
      OK: 218 SKIPPED: 0 FAILED: 3
      1. Failure: DOIs with no agency found still work, at least some do (@test_cr_cn.R#38) 
      2. Failure: cr_members fails correctly (@test_cr_members.R#43) 
      3. Failure: cr_members fails correctly (@test_cr_members.R#47) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# rcv

Version: 0.2.1

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 6543 marked UTF-8 strings
    ```

# rdrop2

Version: 0.8.1

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      2: In checkSkipLoopCntxt(a, cntxt, breakOK) :
        closing unused connection 4 (/home/muelleki/git/R/dplyr/revdep/checks/rdrop2/new/rdrop2.Rcheck/tests/testthat/rdrop2_package_test_b300d02b-fd51-4ee6-9c5e-ea58e097960c_rdrop2_package_test_drop.png)
      3: In checkSkipLoopCntxt(a, cntxt, breakOK) :
        closing unused connection 3 (/home/muelleki/git/R/dplyr/revdep/checks/rdrop2/new/rdrop2.Rcheck/tests/testthat/rdrop2_package_test_b1e9bd54-d694-4b30-91b7-9703e85ebb10_file-ops.csv)
      4: In inherits(x, "factor") :
        closing unused connection 8 (/home/muelleki/git/R/dplyr/revdep/checks/rdrop2/new/rdrop2.Rcheck/tests/testthat/rdrop2_package_test_aa7c5330-af59-47a4-8a2b-fdb175cd3541_share.csv)
      5: In inherits(x, "factor") :
        closing unused connection 7 (/home/muelleki/git/R/dplyr/revdep/checks/rdrop2/new/rdrop2.Rcheck/tests/testthat/rdrop2_package_test_5abe876b-01a1-4f24-b878-34d06daf8c96_delete.csv)
      6: In inherits(x, "factor") :
        closing unused connection 6 (/home/muelleki/git/R/dplyr/revdep/checks/rdrop2/new/rdrop2.Rcheck/tests/testthat/rdrop2_package_test_678ca208-5c06-4dcc-b7a6-391db9674494_move.csv)
      7: In inherits(x, "factor") :
        closing unused connection 5 (/home/muelleki/git/R/dplyr/revdep/checks/rdrop2/new/rdrop2.Rcheck/tests/testthat/rdrop2_package_test_793b2ceb-6ccd-4963-88d1-b48c4fcdbb7c_iris-test-copy.csv)
      8: In inherits(x, "factor") :
        closing unused connection 4 (/home/muelleki/git/R/dplyr/revdep/checks/rdrop2/new/rdrop2.Rcheck/tests/testthat/rdrop2_package_test_b52cccf1-be04-49ec-8cc6-983cb9f82c5e_test-drop_download.csv)
      Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘digest’
      All declared Imports should be used.
    ```

# readat

Version: 1.2.1

## In both

*   checking R code for possible problems ... NOTE
    ```
    sfread: no visible binding for global variable ‘header’
    sfread: no visible binding for global variable ‘nrows’
    Undefined global functions or variables:
      header nrows
    ```

# recexcavAAR

Version: 0.3.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.4Mb
      sub-directories of 1Mb or more:
        doc    2.5Mb
        libs   4.8Mb
    ```

# REDCapR

Version: 0.9.8

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      testthat results ================================================================
      OK: 551 SKIPPED: 0 FAILED: 33
      1. Failure: All Records -Raw (@test-metadata-read.R#115) 
      2. Failure: All Records -Raw and DAG (@test-metadata-read.R#161) 
      3. Failure: All Records -label and DAG (@test-metadata-read.R#214) 
      4. Failure: All Records -label (@test-metadata-read.R#266) 
      5. Failure: All Records -Default (@test-read-batch-simple.R#70) 
      6. Failure: All Records -Default (@test-read-batch-simple.R#85) 
      7. Failure: All Records -Raw (@test-read-batch-simple.R#130) 
      8. Failure: All Records -Raw (@test-read-batch-simple.R#145) 
      9. Failure: All Records -Raw and DAG (@test-read-batch-simple.R#191) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

# replyr

Version: 0.9.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘magick’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘RSQLite’ ‘dbplyr’
      All declared Imports should be used.
    ```

# repr

Version: 0.12.0

## In both

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      ‘data.table’ ‘htmlwidgets’
    ```

# rerddap

Version: 0.4.2

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      testthat results ================================================================
      OK: 91 SKIPPED: 0 FAILED: 15
      1. Failure: ed_search_adv fails well (@test-ed_search_adv.R#58) 
      2. Failure: ed_search_adv fails well (@test-ed_search_adv.R#59) 
      3. Error: griddap returns the correct class (@test-griddap.r#6) 
      4. Error: griddap fixes incorrect user inputs (@test-griddap.r#24) 
      5. Error: griddap fields parameter works, and fails correctly (@test-griddap.r#65) 
      6. Failure: griddap fails well, in addition to above failure tests (@test-griddap.r#85) 
      7. Failure: griddap fails well, in addition to above failure tests (@test-griddap.r#86) 
      8. Failure: griddap fails well, in addition to above failure tests (@test-griddap.r#89) 
      9. Error: info returns the correct (@test-info.R#6) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘taxize’
    ```

# rfishbase

Version: 2.1.2

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 44 marked UTF-8 strings
    ```

# rmapzen

Version: 0.3.3

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘geojsonio’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# rmcfs

Version: 1.2.6

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        java   4.5Mb
    ```

# rmonad

Version: 0.3.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘R6’
      All declared Imports should be used.
    ```

# RNeXML

Version: 2.0.7

## In both

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking: ‘rrdf’ ‘Sxslt’
    ```

# RNHANES

Version: 1.1.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Content type 'text/html; charset=utf-8' length 504270 bytes (492 KB)
      ==================================================
      downloaded 492 KB
      
      testthat results ================================================================
      OK: 112 SKIPPED: 0 FAILED: 6
      1. Failure: nhanes_search on variables passes spot check (@test_search.R#13) 
      2. Failure: nhanes_search on variables passes spot check (@test_search.R#14) 
      3. Failure: nhanes_search on files passes spot check (@test_search.R#20) 
      4. Failure: nhanes_search on files passes spot check (@test_search.R#21) 
      5. Failure: fuzzy search works on files (@test_search.R#33) 
      6. Failure: fuzzy search works on files (@test_search.R#34) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# rnoaa

Version: 0.7.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      testthat results ================================================================
      OK: 176 SKIPPED: 1 FAILED: 11
      1. Failure: buoys fails well (@test-buoy.R#74) 
      2. Error: check_response returns an error (@test-check_response.r#7) 
      3. Error: check_response returns the correct error messages (@test-check_response.r#26) 
      4. Error: ncdc returns the correct ... (@test-ncdc.r#8) 
      5. Error: ncdc_datacats returns the correct ... (@test-ncdc_datacats.r#7) 
      6. Error: ncdc_datasets returns the correct class (@test-ncdc_datasets.r#7) 
      7. Error: ncdc_datatypes returns the correct class (@test-ncdc_datatypes.r#7) 
      8. Error: ncdc_locs returns the correct class (@test-ncdc_locs.r#7) 
      9. Error: ncdc_locs_cats returns the correct ... (@test-ncdc_locs_cats.r#7) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

# rODE

Version: 0.99.6

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘data.table’
      All declared Imports should be used.
    ```

# rolypoly

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘matrixcalc’
      All declared Imports should be used.
    ```

# ropenaq

Version: 0.2.2

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Quitting from lines 34-48 (using_openair_package_with_openaq_data.Rmd) 
    Error: processing vignette 'using_openair_package_with_openaq_data.Rmd' failed with diagnostics:
    uh oh, the OpenAQ API seems to be having some issues, try again later
    Execution halted
    ```

# rpdo

Version: 0.2.2

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      15: check_values(data, values = values, unique = TRUE, nulls = FALSE, data_name = data_name) at /tmp/RtmpHtcmq3/R.INSTALL6285486ea706/datacheckr/R/check-data3.R:33
      16: check_data_values(data, values, data_name) at /tmp/RtmpHtcmq3/R.INSTALL6285486ea706/datacheckr/R/check-values.R:20
      17: vapply(column_names, FUN = check_data_values_column, logical(1), data = data, values = values, 
             data_name = data_name) at /tmp/RtmpHtcmq3/R.INSTALL6285486ea706/datacheckr/R/check-data-values.R:20
      18: FUN(X[[i]], ...)
      19: check_vector_value_missing(vector, value, column_name, data_name) at /tmp/RtmpHtcmq3/R.INSTALL6285486ea706/datacheckr/R/check-data-values.R:9
      20: error(name_info(column_name, data_name), " cannot include missing values") at /tmp/RtmpHtcmq3/R.INSTALL6285486ea706/datacheckr/R/check-vector-value.R:7
      21: stop(..., call. = FALSE) at /tmp/RtmpHtcmq3/R.INSTALL6285486ea706/datacheckr/R/utils.R:20
      
      testthat results ================================================================
      OK: 1 SKIPPED: 0 FAILED: 1
      1. Error: download_pdo (@test-download-pdo.R#4) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# rpivotTable

Version: 0.2.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘shiny’
    ```

# rPref

Version: 1.2

## In both

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# rrr

Version: 1.0.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘Rcpp’
      All declared Imports should be used.
    ```

# rscorecard

Version: 0.5.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 22-29 (introduction.Rmd) 
    Error: processing vignette 'introduction.Rmd' failed with diagnostics:
    Missing API key; ?sc_key for details
    Execution halted
    ```

# rsoi

Version: 0.3.0

## Newly fixed

*   checking examples ... ERROR
    ```
    Running examples in ‘rsoi-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: download_enso
    > ### Title: Download Southern Oscillation Index and Oceanic Nino Index data
    > ### Aliases: download_enso
    > 
    > ### ** Examples
    > 
    > enso <- download_enso()
    Error in open.connection(con, "rb") : 
      Timeout was reached: Resolving timed out after 10000 milliseconds
    Calls: download_enso ... structure -> read_connection -> open -> open.connection
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘utils’
      All declared Imports should be used.
    ```

# rsparkling

Version: 0.2.2

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      7: compareVersion(currentRow$spark, "2.0")
      
      testthat results ================================================================
      OK: 1 SKIPPED: 0 FAILED: 8
      1. Error: Test transformation from h2o frame to data frame (@test_transforms.R#5) 
      2. Error: Test transformation of a spark data_frame of bools to an h2o frame of bools (@test_transforms.R#17) 
      3. Error: Test transformation of a spark data_frame of complex types to an h2o frame of complex types (@test_transforms.R#29) 
      4. Error: Test transformation of a spark data_frame of float types to an h2o frame of floats (@test_transforms.R#43) 
      5. Error: Test transformation of a spark data_frame of int types to an h2o frame of ints (@test_transforms.R#54) 
      6. Error: Test transformation of a spark data_frame of str types to an h2o frame of str (@test_transforms.R#65) 
      7. Error: Test transformation from dataframe to h2o frame (@test_transforms.R#76) 
      8. Error: Test transformation from dataframe to h2o frame (@test_transforms.R#86) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# RSSL

Version: 0.6.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.0Mb
      sub-directories of 1Mb or more:
        libs   2.7Mb
    ```

# RSwissMaps

Version: 0.1.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 18627 marked UTF-8 strings
    ```

# RTCGA

Version: 1.6.0

## In both

*   checking examples ... ERROR
    ```
    ...
    + 	MET = `MET|4233`) %>%  
    + 	#cancer samples
    + 	filter(substr(bcr_patient_barcode, 14, 15) == "01") -> ACC_BLCA_BRCA_OV.rnaseq
    > 	
    > 
    > boxplotTCGA(ACC_BLCA_BRCA_OV.rnaseq, "cohort", "MET")
    > boxplotTCGA(ACC_BLCA_BRCA_OV.rnaseq, "cohort", "log1p(MET)")
    > boxplotTCGA(ACC_BLCA_BRCA_OV.rnaseq, "reorder(cohort,log1p(MET), median)", "log1p(MET)")
    > boxplotTCGA(ACC_BLCA_BRCA_OV.rnaseq, "reorder(cohort,log1p(MET), max)", "log1p(MET)")
    > boxplotTCGA(ACC_BLCA_BRCA_OV.rnaseq, "reorder(cohort,log1p(MET), median)", "log1p(MET)",
    + xlab = "Cohort Type", ylab = "Logarithm of MET")
    > boxplotTCGA(ACC_BLCA_BRCA_OV.rnaseq, "reorder(cohort,log1p(MET), median)", "log1p(MET)", 
    + xlab = "Cohort Type", ylab = "Logarithm of MET", legend.title = "Cohorts")
    > boxplotTCGA(ACC_BLCA_BRCA_OV.rnaseq, "reorder(cohort,log1p(MET), median)", "log1p(MET)", 
    + xlab = "Cohort Type", ylab = "Logarithm of MET", legend.title = "Cohorts", legend = "bottom")
    > 
    > ## facet example
    > library(RTCGA.mutations)
    Error in library(RTCGA.mutations) : 
      there is no package called ‘RTCGA.mutations’
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      ‘RTCGA.clinical’ ‘RTCGA.mutations’ ‘RTCGA.RPPA’ ‘RTCGA.mRNA’
      ‘RTCGA.miRNASeq’ ‘RTCGA.methylation’ ‘RTCGA.CNV’
    ```

*   checking R code for possible problems ... NOTE
    ```
    availableDates: no visible binding for global variable ‘.’
    downloadTCGA: no visible binding for global variable ‘.’
    ggbiplot: no visible binding for global variable ‘xvar’
    ggbiplot: no visible binding for global variable ‘yvar’
    ggbiplot: no visible global function definition for ‘muted’
    ggbiplot: no visible binding for global variable ‘varname’
    ggbiplot: no visible binding for global variable ‘angle’
    ggbiplot: no visible binding for global variable ‘hjust’
    read.mutations: no visible binding for global variable ‘.’
    read.rnaseq: no visible binding for global variable ‘.’
    survivalTCGA: no visible binding for global variable ‘times’
    whichDateToUse: no visible binding for global variable ‘.’
    Undefined global functions or variables:
      . angle hjust muted times varname xvar yvar
    ```

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘RTCGA.clinical’, ‘RTCGA.mutations’, ‘RTCGA.CNV’, ‘RTCGA.RPPA’, ‘RTCGA.mRNA’, ‘RTCGA.miRNASeq’, ‘RTCGA.methylation’
    ```

# rtimes

Version: 0.5.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      6: Filter(Negate(is.null), x)
      7: unlist(lapply(x, f))
      8: lapply(x, f)
      9: check_key(key)
      10: stop("need an API key for ", y, call. = FALSE)
      
      testthat results ================================================================
      OK: 2 SKIPPED: 0 FAILED: 4
      1. Error: returns the correct stuff (@test-as_search.R#8) 
      2. Error: returns the correct stuff (@test-geo_search.R#8) 
      3. Failure: fails well (@test-geo_search.R#48) 
      4. Error: fails well (@test-geo_search.R#50) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# rtimicropem

Version: 1.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘R6’
      All declared Imports should be used.
    ```

# rtrends

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# rvertnet

Version: 0.6.2

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      grepl("Bufo debilis", aa$data$scientificname) isn't true.
      
      
      3. Failure: vert_id works (@test-vert_id.R#33) ---------------------------------
      any(grepl("Bufo", aa$data$scientificname)) isn't true.
      
      
      testthat results ================================================================
      OK: 107 SKIPPED: 0 FAILED: 3
      1. Failure: searchbyterm works correctly (@test-searchbyterm.R#12) 
      2. Failure: vert_id works (@test-vert_id.R#15) 
      3. Failure: vert_id works (@test-vert_id.R#33) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# RxODE

Version: 0.6-1

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘SnakeCharmR’
    ```

# SanFranBeachWater

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tibble’
      All declared Imports should be used.
    ```

# scanstatistics

Version: 1.0.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.9Mb
      sub-directories of 1Mb or more:
        libs   6.3Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘gamlss.dist’
      All declared Imports should be used.
    ```

# scater

Version: 1.4.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.7Mb
      sub-directories of 1Mb or more:
        doc   5.7Mb
    ```

*   checking Rd \usage sections ... NOTE
    ```
    S3 methods shown with full name in documentation object 'arrange':
      ‘arrange.SCESet’
    
    S3 methods shown with full name in documentation object 'filter':
      ‘filter.SCESet’
    
    S3 methods shown with full name in documentation object 'mutate':
      ‘mutate.SCESet’
    
    S3 methods shown with full name in documentation object 'rename':
      ‘rename.SCESet’
    
    The \usage entries for S3 methods should use the \method markup and not
    their full name.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

# SCORPIUS

Version: 1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘Rcpp’ ‘testthat’
      All declared Imports should be used.
    ```

# sejmRP

Version: 1.3.4

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘cluster’ ‘factoextra’ ‘tidyr’
      All declared Imports should be used.
    ```

# seplyr

Version: 0.1.5

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘datasets’
      All declared Imports should be used.
    ```

# sergeant

Version: 0.5.2

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      2: httr::POST(sprintf("%s/query.json", drill_server), encode = "json", body = list(queryType = "SQL", 
             query = query))
      3: request_perform(req, hu$handle$handle)
      4: request_fetch(req$output, req$url, handle)
      5: request_fetch.write_memory(req$output, req$url, handle)
      6: curl::curl_fetch_memory(url, handle = handle)
      
      testthat results ================================================================
      OK: 1 SKIPPED: 0 FAILED: 3
      1. Error: Core dbplyr ops work (@test-sergeant.R#12) 
      2. Failure: REST API works (@test-sergeant.R#25) 
      3. Error: REST API works (@test-sergeant.R#27) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# Seurat

Version: 2.1.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.0Mb
      sub-directories of 1Mb or more:
        libs   5.0Mb
    ```

# sf

Version: 0.5-5

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    The following objects are masked from ‘package:base’:
    
        intersect, setdiff, setequal, union
    
    > nc = st_read(system.file("shape/nc.shp", package="sf"))
    Reading layer `nc' from data source `/home/muelleki/git/R/dplyr/revdep/checks/sf/new/sf.Rcheck/sf/shape/nc.shp' using driver `ESRI Shapefile'
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

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Warning in engine$weave(file, quiet = quiet, encoding = enc) :
      The vignette engine knitr::rmarkdown is not available, because the rmarkdown package is not installed. Please install it.
    Warning in engine$weave(file, quiet = quiet, encoding = enc) :
      The vignette engine knitr::rmarkdown is not available, because the rmarkdown package is not installed. Please install it.
    Warning in engine$weave(file, quiet = quiet, encoding = enc) :
      The vignette engine knitr::rmarkdown is not available, because the rmarkdown package is not installed. Please install it.
    Warning in engine$weave(file, quiet = quiet, encoding = enc) :
      The vignette engine knitr::rmarkdown is not available, because the rmarkdown package is not installed. Please install it.
    Warning in engine$weave(file, quiet = quiet, encoding = enc) :
      The vignette engine knitr::rmarkdown is not available, because the rmarkdown package is not installed. Please install it.
    Quitting from lines 194-197 (sf5.Rmd) 
    Error: processing vignette 'sf5.Rmd' failed with diagnostics:
    cannot open the connection
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 20.2Mb
      sub-directories of 1Mb or more:
        doc     10.7Mb
        libs     5.7Mb
        sqlite   1.5Mb
    ```

# shazam

Version: 0.1.8

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2 marked UTF-8 strings
    ```

# shinyAce

Version: 0.2.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.9Mb
      sub-directories of 1Mb or more:
        www   9.7Mb
    ```

# shinyHeatmaply

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘RColorBrewer’ ‘htmlwidgets’ ‘jsonlite’ ‘viridis’
      All declared Imports should be used.
    ```

# SIBER

Version: 2.1.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘ggplot2’ ‘viridis’
      All declared Imports should be used.
    ```

# sicegar

Version: 0.2.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# sidrar

Version: 0.2.4

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# simmer

Version: 3.6.4

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 12.3Mb
      sub-directories of 1Mb or more:
        doc    1.0Mb
        libs  10.9Mb
    ```

# sjlabelled

Version: 1.0.5

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘sjPlot’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘sjPlot’
    ```

# sjmisc

Version: 2.6.2

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘sjPlot’
    ```

# sjPlot

Version: 2.4.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘prediction’
      All declared Imports should be used.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘plm’
    ```

# sjstats

Version: 0.12.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘sjPlot’, ‘MuMIn’, ‘piecewiseSEM’
    ```

# solrium

Version: 1.0.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      testthat results ================================================================
      OK: 193 SKIPPED: 0 FAILED: 26
      1. Error: add works with a list and data.frame (@test-add.R#6) 
      2. Error: add works with new interface (@test-add.R#30) 
      3. Error: collections works - no collections (@test-collections.R#5) 
      4. Error: collections works - with some collections (@test-collections.R#19) 
      5. Error: collections works - new way of using (@test-collections.R#33) 
      6. Error: core_create works (@test-core_create.R#6) 
      7. Error: delete by  (@test-delete.R#6) 
      8. Error: delete by many ids (@test-delete.R#36) 
      9. Error: ping works (@test-ping.R#4) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘XML’
    ```

# sophisthse

Version: 0.7.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1320 marked UTF-8 strings
    ```

# sorvi

Version: 0.7.26

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    regression_plot: no visible global function definition for
      ‘colorRampPalette’
    regression_plot: no visible global function definition for
      ‘loess.control’
    regression_plot: no visible global function definition for ‘predict’
    regression_plot : <anonymous>: no visible global function definition
      for ‘quantile’
    regression_plot : <anonymous>: no visible global function definition
      for ‘pnorm’
    regression_plot: no visible global function definition for
      ‘flush.console’
    regression_plot: no visible global function definition for ‘density’
    Undefined global functions or variables:
      colorRampPalette density flush.console loess loess.control pnorm
      predict quantile read.csv
    Consider adding
      importFrom("grDevices", "colorRampPalette")
      importFrom("stats", "density", "loess", "loess.control", "pnorm",
                 "predict", "quantile")
      importFrom("utils", "flush.console", "read.csv")
    to your NAMESPACE file.
    ```

# sourceR

Version: 1.0.1

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      
      testthat results ================================================================
      OK: 18 SKIPPED: 0 FAILED: 8
      1. Failure: HaldDP model construction (@testHaldDP.R#106) 
      2. Failure: HaldDP model construction (@testHaldDP.R#108) 
      3. Failure: HaldDP model construction (@testHaldDP.R#109) 
      4. Failure: HaldDP model construction (@testHaldDP.R#110) 
      5. Failure: HaldDP model construction (@testHaldDP.R#116) 
      6. Failure: Test time/location data structures (@testHaldDPTimeLoc.R#107) 
      7. Failure: Prior and init (@testHaldDPTimeLoc.R#194) 
      8. Failure: Prior and init (@testHaldDPTimeLoc.R#195) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘gtools’ ‘hashmap’ ‘reshape2’
      All declared Imports should be used.
    ```

# SpaDES.core

Version: 0.1.0

## In both

*   checking whether package ‘SpaDES.core’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: no DISPLAY variable so Tk is not available
    See ‘/home/muelleki/git/R/dplyr/revdep/checks/SpaDES.core/new/SpaDES.core.Rcheck/00install.out’ for details.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘CircStats’ ‘RandomFields’ ‘grDevices’ ‘sp’
      All declared Imports should be used.
    ```

# SpaDES.tools

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘fastdigest’
      All declared Imports should be used.
    ```

# sparklyr

Version: 0.6.4

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > Sys.setenv("R_TESTS" = "")
      > library(testthat)
      > library(sparklyr)
      > 
      > if (identical(Sys.getenv("NOT_CRAN"), "true")) {
      +   test_check("sparklyr")
      +   on.exit({ spark_disconnect_all() ; livy_service_stop() })
      + }
      Error in if (is.na(a)) return(-1L) : argument is of length zero
      Calls: test_check ... spark_install_find -> spark_versions -> lapply -> FUN -> compareVersion
      testthat results ================================================================
      OK: 0 SKIPPED: 0 FAILED: 0
      Execution halted
    ```

# sparseHessianFD

Version: 0.3.3.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
      Running 'texi2dvi' on 'sparseHessianFD.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `algorithm.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    
    ! Emergency stop.
    <read *> 
             
    l.27 \usepackage
                    {algorithmic}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    Calls: buildVignettes -> texi2pdf -> texi2dvi
    Execution halted
    ```

# SpatialEpiApp

Version: 0.3

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘INLA’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘RColorBrewer’ ‘SpatialEpi’ ‘dplyr’ ‘dygraphs’ ‘ggplot2’
      ‘htmlwidgets’ ‘knitr’ ‘leaflet’ ‘mapproj’ ‘maptools’ ‘rgdal’ ‘rgeos’
      ‘rmarkdown’ ‘shinyjs’ ‘spdep’ ‘xts’
      All declared Imports should be used.
    ```

# ss3sim

Version: 0.9.5

## In both

*   checking whether package ‘ss3sim’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: no DISPLAY variable so Tk is not available
    See ‘/home/muelleki/git/R/dplyr/revdep/checks/ss3sim/new/ss3sim.Rcheck/00install.out’ for details.
    ```

# stacomiR

Version: 0.5.3

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘gWidgetsRGtk2’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# statesRcontiguous

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘magrittr’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 34 marked UTF-8 strings
    ```

# stationaRy

Version: 0.4.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.1Mb
    ```

# statip

Version: 0.1.5

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘distrEx’
    ```

# statisticalModeling

Version: 0.3.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(statisticalModeling)
      Loading required package: ggplot2
      > 
      > test_check("statisticalModeling")
      Error: gwm() has been removed from `mosaic'.  
          It will be replaced by better tools in `mosaicModel'.
      testthat results ================================================================
      OK: 21 SKIPPED: 0 FAILED: 0
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 303-306 (modeling.Rmd) 
    Error: processing vignette 'modeling.Rmd' failed with diagnostics:
    Invalid formula type for gf_point.
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘magrittr’ ‘rpart’
      All declared Imports should be used.
    ```

# statsDK

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘ggplot2’ ‘stringr’
      All declared Imports should be used.
    ```

# stplanr

Version: 0.1.9

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘stplanr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: geo_code
    > ### Title: Convert text strings into points on the map
    > ### Aliases: geo_code
    > 
    > ### ** Examples
    > 
    > address = "LS7 3HB"
    > geo_code(address = address)
          lon       lat 
    -1.534372 53.819472 
    > geo_code(address = address, return_all = TRUE)
    Error: is.data.frame(x) is not TRUE
    Execution halted
    ```

# subSeq

Version: 1.6.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    summary.subsamples: no visible binding for global variable ‘o.padj’
    summary.subsamples: no visible binding for global variable
      ‘significant’
    summary.subsamples: no visible binding for global variable ‘estFDP’
    summary.subsamples: no visible binding for global variable ‘rFDP’
    summary.subsamples: no visible binding for global variable ‘metric’
    summary.subsamples: no visible binding for global variable ‘value’
    summary.subsamples: no visible binding for global variable ‘percent’
    voomLimma: no visible global function definition for ‘model.matrix’
    Undefined global functions or variables:
      . ID average.depth average.value coefficient cor count cov depth
      estFDP method metric model.matrix o.coefficient o.lfdr o.padj
      p.adjust padj percent plot proportion pvalue rFDP rbinom replication
      selectMethod significant valid value var
    Consider adding
      importFrom("graphics", "plot")
      importFrom("methods", "selectMethod")
      importFrom("stats", "cor", "cov", "model.matrix", "p.adjust", "rbinom",
                 "var")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

# sunburstR

Version: 1.0.2

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘treemap’
    ```

# survminer

Version: 0.4.0

## In both

*   checking examples ... ERROR
    ```
    ...
    LUNG no event    0.004340703 0.007643672 0.007643672 0.007643672
    OV no event      0.004994699 0.007767270 0.008089927 0.008089927
    BRCA death       0.003828336 0.005337151 0.005930541 0.005930541
    LUNG death       0.006637210 0.007337434 0.007337434 0.007337434
    OV death         0.004504398 0.005133510 0.005133510 0.005133510
    BRCA progression 0.004378377 0.006110680 0.006110680 0.006110680
    LUNG progression 0.006700958 0.008631668 0.009219678 0.009771658
    OV progression   0.005033271 0.006681731 0.006681731 0.006681731
    
    > ggcompetingrisks(fit)
    > ggcompetingrisks(fit, multiple_panels = FALSE)
    > ggcompetingrisks(fit, conf.int = TRUE)
    > ggcompetingrisks(fit, multiple_panels = FALSE, conf.int = TRUE)
    > 
    > # handles survfitms objects
    > library(survival)
    > df <- data.frame(time = ss, group = gg, status = cc, strt)
    > fit2 <- survfit(Surv(time, status, type="mstate") ~ 1, data=df)
    > ggcompetingrisks(fit2)
    Error: Aesthetics must be either length 1 or the same as the data (300): x, y, fill
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        doc   5.3Mb
    ```

# sweep

Version: 0.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘devtools’ ‘lazyeval’ ‘lubridate’ ‘tidyr’
      All declared Imports should be used.
    ```

# swfdr

Version: 1.0.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Error: processing vignette 'swfdrTutorial.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

# switchde

Version: 1.2.0

## Newly broken

*   checking whether package ‘switchde’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘dplyr::exprs’ by ‘Biobase::exprs’ when loading ‘switchde’
    See ‘/home/muelleki/git/R/dplyr/revdep/checks/switchde/new/switchde.Rcheck/00install.out’ for details.
    ```

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    The following objects are masked from 'package:Biobase':
    
        combine, exprs
    
    The following objects are masked from 'package:BiocGenerics':
    
        combine, intersect, setdiff, union
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Warning: replacing previous import 'dplyr::exprs' by 'Biobase::exprs' when loading 'switchde'
    Warning: Removed 68 rows containing missing values (geom_path).
    Error: processing vignette 'switchde_vignette.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

# synlet

Version: 1.6.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    siRNAPlot: no visible global function definition for ‘pdf’
    siRNAPlot: no visible global function definition for ‘dev.off’
    tTest: no visible global function definition for ‘p.adjust’
    zFactor: no visible binding for global variable ‘condition’
    zFactor: no visible binding for global variable ‘sd’
    zFactor: no visible binding for global variable ‘median’
    zFactor: no visible global function definition for ‘complete.cases’
    Undefined global functions or variables:
      COL_NAME EXPERIMENT_MODIFICATION EXPERIMENT_TYPE MASTER_PLATE PLATE
      READOUT ROW_NAME Var1 WELL_CONTENT_NAME colorRampPalette
      complete.cases condition dev.off experiments is mad median medpolish
      p.adjust pdf phyper rainbow sd siRNA t.test value write.table
    Consider adding
      importFrom("grDevices", "colorRampPalette", "dev.off", "pdf",
                 "rainbow")
      importFrom("methods", "is")
      importFrom("stats", "complete.cases", "mad", "median", "medpolish",
                 "p.adjust", "phyper", "sd", "t.test")
      importFrom("utils", "write.table")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

# tadaatoolbox

Version: 0.14.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘rmdformats’
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 3 marked UTF-8 strings
    ```

# taxa

Version: 0.1.0

## In both

*   checking examples ... ERROR
    ```
    ...
      4 data sets:
        info:
          # A tibble: 4 x 4
              name n_legs dangerous taxon_id
            <fctr>  <dbl>     <lgl>    <chr>
          1    cat      4     FALSE        n
          2   mole      4     FALSE        o
          3 tomato      0     FALSE        q
          # ... with 1 more rows
        phylopic_ids:  e148eabb-f138-43c6-b1e4-5cda2180485a ... 63604565-0406-460b-8cb8-1abe954b3f3a
        foods: a list with 6 items
        And 1 more data sets: abund
      1 functions:
     reaction
    > 
    > # Remove taxa whose obserservation were filtered out
    > filter_obs(ex_taxmap, "info", dangerous == FALSE, drop_taxa = TRUE)
    Error in names(selection) <- self$taxon_ids() : 
      'names' attribute [17] must be the same length as the vector [2]
    Calls: filter_obs ... filter_obs.Taxmap -> <Anonymous> -> <Anonymous> -> <Anonymous>
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      testthat results ================================================================
      OK: 317 SKIPPED: 0 FAILED: 31
      1. Error: NSE values can be found (@test--taxmap.R#214) 
      2. Error: All valid NSE values can be found (@test--taxmap.R#224) 
      3. Error: Mapping between table observations and the edge list works (@test--taxmap.R#233) 
      4. Error: Mapping between a subset of observations and the edge list works (@test--taxmap.R#242) 
      5. Error: Mapping non-recursivly between observations and the edge list works (@test--taxmap.R#247) 
      6. Error: Mapping simplification between observations and the edge list works (@test--taxmap.R#253) 
      7. Error: Mapping observations in external tables (@test--taxmap.R#259) 
      8. Error: Default taxon filtering works (@test--taxmap.R#271) 
      9. Error: Subtaxa can be included when filtering taxa (@test--taxmap.R#279) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 258-259 (taxa-vignette.Rmd) 
    Error: processing vignette 'taxa-vignette.Rmd' failed with diagnostics:
    'names' attribute [14] must be the same length as the vector [2]
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘knitr’ ‘lazyeval’ ‘rlang’
      All declared Imports should be used.
    ```

# taxizedb

Version: 0.1.4

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      > library(testthat)
      > test_check("taxizedb")
      Loading required package: taxizedb
      1. Failure: db_load fails as expected - more (@test-db_load.R#33) --------------
      error$message does not match "Failed to connect".
      Actual value: "\nmysql not found on your computer\nInstall the missing tool(s) and try again"
      
      
      testthat results ================================================================
      OK: 59 SKIPPED: 0 FAILED: 1
      1. Failure: db_load fails as expected - more (@test-db_load.R#33) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# tbl2xts

Version: 0.1.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘PerformanceAnalytics’
      All declared Imports should be used.
    ```

# TCGAbiolinks

Version: 2.5.9

## In both

*   checking examples ... ERROR
    ```
    ...
    |NA                          |NA                                  |NA                   |NA                           |
    |NA                          |NA                                  |NA                   |NA                           |
    |NA                          |NA                                  |NA                   |NA                           |
    |NA                          |NA                                  |NA                   |NA                           |
    |NA                          |NA                                  |NA                   |NA                           |
    |NA                          |NA                                  |NA                   |NA                           |
    |NA                          |NA                                  |NA                   |NA                           |
    |NA                          |NA                                  |NA                   |NA                           |
    |NA                          |NA                                  |NA                   |NA                           |
    |NA                          |NA                                  |NA                   |NA                           |
    |NA                          |NA                                  |NA                   |NA                           |
    |NA                          |NA                                  |NA                   |NA                           |
    |NA                          |NA                                  |NA                   |NA                           |
    |NA                          |NA                                  |NA                   |NA                           |
    |NA                          |NA                                  |NA                   |NA                           |
    |NA                          |NA                                  |NA                   |NA                           |
    |NA                          |NA                                  |NA                   |NA                           |
    Error in checkProjectInput(project) : 
      Please set a valid project argument from the column id above. Project TCGA-ACC was not found.
    Calls: GDCquery -> checkProjectInput
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      testthat results ================================================================
      OK: 114 SKIPPED: 0 FAILED: 16
      1. Error: TCGAanalyze_survival creates pdf (@test-analyse.R#4) 
      2. Error: Results from TCGAanalyze_DEA and DMR in starburst plot are correct (@test-analyse.R#195) 
      3. Error: GDCdownload API method for two files is working  (@test-prepare-download.R#4) 
      4. Error: GDCdownload API method for one files is working  (@test-prepare-download.R#20) 
      5. Error: GDCprepare accepts more than one project (@test-prepare-download.R#50) 
      6. Error: Accecpts more than one platform (@test-prepare-download.R#68) 
      7. Error: GDCquery can filter by data.category (@test-query.R#5) 
      8. Error: GDCquery accepts more than one project (@test-query.R#11) 
      9. Error: GDCquery can filter by sample.type (@test-query.R#23) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
    The following objects are masked from 'package:S4Vectors':
    
        first, intersect, rename, setdiff, setequal, union
    
    The following objects are masked from 'package:BiocGenerics':
    
        combine, intersect, setdiff, union
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Quitting from lines 16-20 (clinical.Rmd) 
    Error: processing vignette 'clinical.Rmd' failed with diagnostics:
    there is no package called 'DT'
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 61.0Mb
      sub-directories of 1Mb or more:
        R      1.1Mb
        data   2.3Mb
        doc   57.4Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      ‘limmacontrasts.fit’
    TCGAanalyze_analyseGRN: no visible global function definition for
      ‘knnmi.cross’
    TCGAanalyze_networkInference: no visible global function definition for
      ‘c3net’
    TCGAanalyze_networkInference: no visible global function definition for
      ‘minet’
    TCGAvisualize_SurvivalCoxNET: no visible global function definition for
      ‘dNetInduce’
    TCGAvisualize_SurvivalCoxNET: no visible global function definition for
      ‘dNetPipeline’
    TCGAvisualize_SurvivalCoxNET: no visible global function definition for
      ‘dCommSignif’
    TCGAvisualize_SurvivalCoxNET: no visible global function definition for
      ‘visNet’
    TCGAvisualize_oncoprint: no visible binding for global variable ‘value’
    getTSS: no visible global function definition for ‘promoters’
    Undefined global functions or variables:
      c3net dCommSignif dNetInduce dNetPipeline knnmi.cross
      limmacontrasts.fit limmamakeContrasts minet portions promoters value
      visNet
    ```

*   checking for unstated dependencies in vignettes ... NOTE
    ```
    'library' or 'require' call not declared from: ‘DT’
    ```

# tcR

Version: 2.2.1.11

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.6Mb
      sub-directories of 1Mb or more:
        data   1.2Mb
        doc    3.9Mb
        libs   1.1Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘scales’
      All declared Imports should be used.
    ```

*   checking Rd \usage sections ... NOTE
    ```
    S3 methods shown with full name in documentation object 'top.fun':
      ‘slice.fun’
    
    The \usage entries for S3 methods should use the \method markup and not
    their full name.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

# teachingApps

Version: 1.0.2

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        apps   2.6Mb
        libs   2.1Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘data.table’ ‘datasets’ ‘stats’
      All declared Imports should be used.
    ```

# tempcyclesdata

Version: 1.0.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.0Mb
      sub-directories of 1Mb or more:
        data   5.9Mb
    ```

# temperatureresponse

Version: 0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘nlme’ ‘tidyr’
      All declared Imports should be used.
    ```

# texmexseq

Version: 0.3

## In both

*   checking examples ... ERROR
    ```
    ...
    > 
    > # make up some data
    > sim.data <- function() rpoilog(1000, 1.0, 1.0, condS=TRUE)
    > otu <- data.frame(sample0=sim.data())
    > for (i in 1:10) otu[[paste('sample', i, sep='')]] <- sim.data()
    > otu.ids <- paste('otu', seq(1:1000), sep='')
    > rownames(otu) <- otu.ids
    > z.table <- z.transform.table(otu)
    Warning in value[[3L]](cond) : fit 1 failed
    Warning in value[[3L]](cond) : fit 1 failed
    Warning in value[[3L]](cond) : fit 2 failed
    > 
    > # pull out a quad, imagining that samples 1 and 2 were the control samples
    > # and 3 and 4 were the treatment
    > q <- quad.table(z.table, 'sample1', 'sample2', 'sample3', 'sample4')
    > 
    > # plot it
    > p <- quad.plot(q)
    Error in get("d.control") : object 'd.control' not found
    Calls: quad.plot ... map -> .Call -> .f -> overscope_eval_next -> .Call -> get
    Execution halted
    ```

# textmining

Version: 0.0.1

## In both

*   checking whether package ‘textmining’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: no DISPLAY variable so Tk is not available
    See ‘/home/muelleki/git/R/dplyr/revdep/checks/textmining/new/textmining.Rcheck/00install.out’ for details.
    ```

# textreuse

Version: 0.1.4

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘tm’
    ```

# TH.data

Version: 1.0-8

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        data   1.2Mb
        rda    3.8Mb
    ```

# tidyjson

Version: 0.2.2

## In both

*   checking examples ... ERROR
    ```
    ...
    > # companies[[1]] %>% prettify
    > 
    > # Get the key employees data
    > key_employees <- companies %>%
    +   spread_values(
    +     name = jstring("name")
    +   ) %>%
    +   mutate(
    +     company.sort_order = rank(name)
    +   ) %>%
    +   enter_object("relationships") %>%
    +   gather_array("relationship.index") %>%
    +   spread_values(
    +     is.past = jlogical("is_past"),
    +     name = jstring("person", "permalink"),
    +     title = jstring("title")
    +   )
    Error in eval(assertion, env) : 
      argument "json.column" is missing, with no default
    Calls: %>% ... tryCatchList -> tryCatchOne -> doTryCatch -> eval -> eval
    Execution halted
    ```

# tidyquant

Version: 0.5.3

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      
      testthat results ================================================================
      OK: 179 SKIPPED: 0 FAILED: 3
      1. Failure: Test returns tibble with correct rows and columns. (@test_tq_get_key_stats.R#15) 
      2. Failure: Test returns tibble with correct rows and columns. (@test_tq_get_key_stats.R#17) 
      3. Failure: Test returns tibble with correct rows and columns. (@test_tq_get_key_stats.R#19) 
      
      Error: testthat unit tests failed
      In addition: Warning messages:
      1: In download.file(url, destfile = tmp, quiet = TRUE) :
        cannot open URL 'http://download.finance.yahoo.com/d/quotes.csv?s=AAPL&f=aa2a5bb4b6c1c4dd1ee7e8e9f6ghjj1j2j4j5j6kk3k4k5ll1mm3m4m5m6m7m8nopp2p5p6qrr1r5r6r7s6s7t8vwxy&e=.csv': HTTP status was '403 Forbidden'
      2: x = 'AAPL', get = 'key.stats': Error in download.file(url, destfile = tmp, quiet = TRUE): cannot open URL 'http://download.finance.yahoo.com/d/quotes.csv?s=AAPL&f=aa2a5bb4b6c1c4dd1ee7e8e9f6ghjj1j2j4j5j6kk3k4k5ll1mm3m4m5m6m7m8nopp2p5p6qrr1r5r6r7s6s7t8vwxy&e=.csv'
       
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Warning in download.file(url, destfile = tmp, quiet = TRUE) :
      cannot open URL 'http://download.finance.yahoo.com/d/quotes.csv?s=AAPL&f=aa2a5bb4b6c1c4dd1ee7e8e9f6ghjj1j2j4j5j6kk3k4k5ll1mm3m4m5m6m7m8nopp2p5p6qrr1r5r6r7s6s7t8vwxy&e=.csv': HTTP status was '403 Forbidden'
    Warning: x = 'AAPL', get = 'key.stats': Error in download.file(url, destfile = tmp, quiet = TRUE): cannot open URL 'http://download.finance.yahoo.com/d/quotes.csv?s=AAPL&f=aa2a5bb4b6c1c4dd1ee7e8e9f6ghjj1j2j4j5j6kk3k4k5ll1mm3m4m5m6m7m8nopp2p5p6qrr1r5r6r7s6s7t8vwxy&e=.csv'
    
    Warning in download.file(url, destfile = tmp, quiet = TRUE) :
      cannot open URL 'http://download.finance.yahoo.com/d/quotes.csv?s=AAPL&f=aa2a5bb4b6c1c4dd1ee7e8e9f6ghjj1j2j4j5j6kk3k4k5ll1mm3m4m5m6m7m8nopp2p5p6qrr1r5r6r7s6s7t8vwxy&e=.csv': HTTP status was '403 Forbidden'
    Warning: x = 'AAPL', get = 'key.stats': Error in download.file(url, destfile = tmp, quiet = TRUE): cannot open URL 'http://download.finance.yahoo.com/d/quotes.csv?s=AAPL&f=aa2a5bb4b6c1c4dd1ee7e8e9f6ghjj1j2j4j5j6kk3k4k5ll1mm3m4m5m6m7m8nopp2p5p6qrr1r5r6r7s6s7t8vwxy&e=.csv'
     Removing AAPL.
    Warning in download.file(url, destfile = tmp, quiet = TRUE) :
      cannot open URL 'http://download.finance.yahoo.com/d/quotes.csv?s=FB&f=aa2a5bb4b6c1c4dd1ee7e8e9f6ghjj1j2j4j5j6kk3k4k5ll1mm3m4m5m6m7m8nopp2p5p6qrr1r5r6r7s6s7t8vwxy&e=.csv': HTTP status was '403 Forbidden'
    Warning: x = 'FB', get = 'key.stats': Error in download.file(url, destfile = tmp, quiet = TRUE): cannot open URL 'http://download.finance.yahoo.com/d/quotes.csv?s=FB&f=aa2a5bb4b6c1c4dd1ee7e8e9f6ghjj1j2j4j5j6kk3k4k5ll1mm3m4m5m6m7m8nopp2p5p6qrr1r5r6r7s6s7t8vwxy&e=.csv'
     Removing FB.
    Warning in download.file(url, destfile = tmp, quiet = TRUE) :
      cannot open URL 'http://download.finance.yahoo.com/d/quotes.csv?s=GOOG&f=aa2a5bb4b6c1c4dd1ee7e8e9f6ghjj1j2j4j5j6kk3k4k5ll1mm3m4m5m6m7m8nopp2p5p6qrr1r5r6r7s6s7t8vwxy&e=.csv': HTTP status was '403 Forbidden'
    Warning: x = 'GOOG', get = 'key.stats': Error in download.file(url, destfile = tmp, quiet = TRUE): cannot open URL 'http://download.finance.yahoo.com/d/quotes.csv?s=GOOG&f=aa2a5bb4b6c1c4dd1ee7e8e9f6ghjj1j2j4j5j6kk3k4k5ll1mm3m4m5m6m7m8nopp2p5p6qrr1r5r6r7s6s7t8vwxy&e=.csv'
     Removing GOOG.
    Warning in value[[3L]](cond) : Returning as nested data frame.
    Quitting from lines 211-214 (TQ01-core-functions-in-tidyquant.Rmd) 
    Error: processing vignette 'TQ01-core-functions-in-tidyquant.Rmd' failed with diagnostics:
    object 'Ask' not found
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘broom’ ‘curl’ ‘devtools’ ‘rvest’ ‘timeSeries’ ‘tseries’ ‘zoo’
      All declared Imports should be used.
    ```

# tidyr

Version: 0.7.2

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 23 marked UTF-8 strings
    ```

# tidyRSS

Version: 1.2.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘testthat’
      All declared Imports should be used.
    ```

# tidytext

Version: 0.1.4

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      data argument must either be a dfmSparse (from quanteda) or a table with document and term columns
      1: augment(stm_model, inaug) at testthat/test-stm-tidiers.R:57
      2: augment.STM(stm_model, inaug)
      3: stop("data argument must either be a dfmSparse ", "(from quanteda) or a table with document and term columns")
      
      testthat results ================================================================
      OK: 198 SKIPPED: 0 FAILED: 5
      1. Error: Can cast tables into a sparse dfm (@test-sparse-casters.R#79) 
      2. Failure: Can tidy dfm from quanteda (@test-sparse-tidiers.R#34) 
      3. Failure: Can tidy dfm from quanteda (@test-sparse-tidiers.R#35) 
      4. Failure: Can tidy dfm from quanteda (@test-sparse-tidiers.R#36) 
      5. Error: can augment an stm output (@test-stm-tidiers.R#57) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# tidyverse

Version: 1.2.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dbplyr’ ‘reprex’ ‘rlang’
      All declared Imports should be used.
    ```

# tilegramsR

Version: 0.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘sp’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 341 marked UTF-8 strings
    ```

# timekit

Version: 0.3.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘devtools’ ‘forecast’
      All declared Imports should be used.
    ```

# timelineS

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘base’
      All declared Imports should be used.
    ```

# TimerQuant

Version: 1.6.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
      ‘rainbow’
    plotPrimordiumProfile: no visible binding for global variable ‘median’
    plotPrimordiumProfile: no visible binding for global variable ‘mad’
    plotPrimordiumProfile: no visible global function definition for ‘par’
    plotPrimordiumProfile: no visible global function definition for ‘plot’
    plotPrimordiumProfile: no visible global function definition for ‘axis’
    plotPrimordiumProfile: no visible global function definition for
      ‘points’
    plotPrimordiumProfile: no visible global function definition for
      ‘polygon’
    plotPrimordiumProfile: no visible global function definition for ‘rgb’
    simulatedRatio: no visible global function definition for ‘rnorm’
    Undefined global functions or variables:
      approxfun axis mad median optimize par plot points polygon predict
      rainbow rgb rnorm
    Consider adding
      importFrom("grDevices", "rainbow", "rgb")
      importFrom("graphics", "axis", "par", "plot", "points", "polygon")
      importFrom("stats", "approxfun", "mad", "median", "optimize",
                 "predict", "rnorm")
    to your NAMESPACE file.
    ```

# timescape

Version: 1.0.0

## In both

*   checking Rd \usage sections ... WARNING
    ```
    Duplicated \argument entries in documentation object 'timescapeOutput':
      ‘width’ ‘height’ ‘mutations’ ‘height’ ‘width’ ‘clonal_prev’
      ‘tree_edges’ ‘alpha’ ‘clonal_prev’ ‘tree_edges’ ‘genotype_position’
      ‘clone_colours’ ‘perturbations’ ‘mutations’ ‘tree_edges’
      ‘clonal_prev’ ‘clonal_prev’ ‘tree_edges’ ‘clone_colours’ ‘mutations’
    
    Functions with \usage entries need to have the appropriate \alias
    entries, and all their arguments documented.
    The \usage entries must correspond to syntactically valid R code.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .vscode
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘gtools’
      All declared Imports should be used.
    ```

*   checking R code for possible problems ... NOTE
    ```
    getMutationsData: no visible binding for global variable
      ‘show_warnings’
    Undefined global functions or variables:
      show_warnings
    ```

# timetk

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘devtools’ ‘forecast’
      All declared Imports should be used.
    ```

# tmap

Version: 1.10

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        data   1.5Mb
        doc    3.3Mb
    ```

# toxplot

Version: 0.1.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Quitting from lines 48-52 (Using_ToxPlot_Package_to_Analyze_in_vitro_Screening_Data.Rmd) 
    Error: processing vignette 'Using_ToxPlot_Package_to_Analyze_in_vitro_Screening_Data.Rmd' failed with diagnostics:
    'roxygen2' >= 5.0.0 must be installed for this functionality.
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tidyr’
      All declared Imports should be used.
    ```

# TPP

Version: 3.4.3

## Newly broken

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
      Calls: test_check ... <Anonymous> -> lapply -> FUN -> cbind -> cbind -> data.frame
      testthat results ================================================================
      OK: 130 SKIPPED: 0 FAILED: 9
      1. Error: allOK (@test_analyze2DTPP.R#14) 
      2. Error: allOK_scientific_drug_concentration_format (@test_analyze2DTPP.R#37) 
      3. Error: warning_deprecated_fct_arg (@test_analyze2DTPP.R#62) 
      4. Error: NPARC_allok (@test_analyzeTPPTR.R#14) 
      5. Error: NPARC_allok_output (@test_analyzeTPPTR.R#34) 
      6. Error: NPARC_allok_plot (@test_analyzeTPPTR.R#61) 
      7. Error: NPARC_allok_files (@test_analyzeTPPTR.R#94) 
      8. Error: meltCurves_allOK_no_conditions (@test_analyzeTPPTR.R#153) 
      9. Error: testApplyCoeffs (@test_applyCoeffs.R#9) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking whether package ‘TPP’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘Biobase::exprs’ by ‘dplyr::exprs’ when loading ‘TPP’
    See ‘/home/muelleki/git/R/dplyr/revdep/checks/TPP/new/TPP.Rcheck/00install.out’ for details.
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

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 13.3Mb
      sub-directories of 1Mb or more:
        data           1.9Mb
        example_data   8.0Mb
        test_data      1.9Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
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

# turfR

Version: 0.8-7

## In both

*   checking R code for possible problems ... NOTE
    ```
    turf: no visible global function definition for ‘read.table’
    turf: no visible global function definition for ‘flush.console’
    turf.combos: no visible global function definition for ‘combn’
    Undefined global functions or variables:
      combn flush.console read.table
    Consider adding
      importFrom("utils", "combn", "flush.console", "read.table")
    to your NAMESPACE file.
    ```

# ukbtools

Version: 0.9.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 5 marked UTF-8 strings
    ```

# unvotes

Version: 0.2.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 4494 marked UTF-8 strings
    ```

# vaersNDvax

Version: 1.0.4

## In both

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking: ‘vaers’ ‘vaersND’
    ```

# vaersvax

Version: 1.0.4

## In both

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking: ‘vaers’ ‘vaersND’
    ```

# valr

Version: 0.3.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 15.5Mb
      sub-directories of 1Mb or more:
        libs  14.1Mb
    ```

# vcfR

Version: 1.5.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 11.3Mb
      sub-directories of 1Mb or more:
        doc    3.0Mb
        libs   7.5Mb
    ```

# vdmR

Version: 0.2.4

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘maptools’ ‘rgeos’
      All declared Imports should be used.
    ```

# VIM

Version: 4.7.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘mvoutlier’, ‘StatDA’, ‘mi’, ‘tkrplot’
    ```

# vqtl

Version: 1.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘iterators’ ‘knitr’ ‘testthat’
      All declared Imports should be used.
    ```

# vsn

Version: 3.44.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘hexbin’
      All declared Imports should be used.
    ```

*   checking R code for possible problems ... NOTE
    ```
    meanSdPlot,matrix: no visible binding for global variable ‘y’
    Undefined global functions or variables:
      y
    ```

# waccR

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘lubridate’ ‘tibble’
      All declared Imports should be used.
    ```

# walker

Version: 0.2.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 46.9Mb
      sub-directories of 1Mb or more:
        libs  46.2Mb
    ```

# wallace

Version: 0.6.4

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘DT’ ‘ENMeval’ ‘RColorBrewer’ ‘devtools’ ‘dismo’ ‘dplyr’ ‘maptools’
      ‘raster’ ‘rgdal’ ‘rgeos’ ‘rmarkdown’ ‘shinyBS’ ‘shinyjs’
      ‘shinythemes’ ‘spThin’ ‘spocc’
      All declared Imports should be used.
    ```

# widyr

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘methods’
      All declared Imports should be used.
    ```

# wiggleplotr

Version: 1.0.0

## In both

*   checking examples ... ERROR
    ```
    ...
    The following object is masked from 'package:base':
    
        expand.grid
    
    Loading required package: IRanges
    Loading required package: GenomeInfoDb
    Loading required package: GenomicFeatures
    Loading required package: AnnotationDbi
    Loading required package: Biobase
    Welcome to Bioconductor
    
        Vignettes contain introductory material; view with
        'browseVignettes()'. To cite Bioconductor, see
        'citation("Biobase")', and for packages 'citation("pkgname")'.
    
    Loading required package: AnnotationFilter
    > plotTranscriptsFromEnsembldb(EnsDb.Hsapiens.v86, "NCOA7", transcript_ids = c("ENST00000438495", "ENST00000392477"))
    Error in validObject(.Object) : 
      invalid class "AnnotationFilterList" object: superclass "vectorORfactor" not defined in the environment of the object's class
    Calls: plotTranscriptsFromEnsembldb ... .AnnotationFilterList -> new -> initialize -> initialize -> validObject
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 148-152 (wiggleplotr.Rmd) 
    Error: processing vignette 'wiggleplotr.Rmd' failed with diagnostics:
    invalid class "AnnotationFilterList" object: superclass "vectorORfactor" not defined in the environment of the object's class
    Execution halted
    ```

# windfarmGA

Version: 1.1.1

## In both

*   checking whether package ‘windfarmGA’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: no DISPLAY variable so Tk is not available
    See ‘/home/muelleki/git/R/dplyr/revdep/checks/windfarmGA/new/windfarmGA.Rcheck/00install.out’ for details.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘RandomFields’
      All declared Imports should be used.
    ```

# wordbankr

Version: 0.2.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 27-31 (wordbankr.Rmd) 
    Error: processing vignette 'wordbankr.Rmd' failed with diagnostics:
    Condition message must be a string
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘RMySQL’
      All declared Imports should be used.
    ```

# wrswoR

Version: 1.0-1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 622-635 (wrswoR.Rmd) 
    Error: processing vignette 'wrswoR.Rmd' failed with diagnostics:
    
    TeX was unable to calculate metrics for the following string
    or character:
    
    	77
    
    Common reasons for failure include:
      * The string contains a character which is special to LaTeX unless
        escaped properly, such as % or $.
      * The string makes use of LaTeX commands provided by a package and
        the tikzDevice was not told to load the package.
    
    The contents of the LaTeX log of the aborted run have been printed above,
    it may contain additional details as to why the metric calculation failed.
    Execution halted
    ```

# XBSeq

Version: 1.6.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
          colMaxs, colMins, colRanges, rowMaxs, rowMins, rowRanges
      
      The following object is masked from 'package:base':
      
          apply
      
          Welcome to 'XBSeq'.
      > 
      > test_check("XBSeq")
      estimating parameters using MLE for group one 
      estimating parameters using MLE for group two 
      Error: XBplot(XB, Samplenum = "Sample_54_WT") did not throw an error.
      testthat results ================================================================
      OK: 0 SKIPPED: 0 FAILED: 0
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Attaching package: 'DESeq'
    
    The following object is masked from 'package:XBSeq':
    
        fitInfo
    
    The following objects are masked from 'package:DESeq2':
    
        estimateSizeFactorsForMatrix, getVarianceStabilizedData,
        varianceStabilizingTransformation
    
    Warning in block_exec(params) :
      failed to tidy R code in chunk <unnamed-chunk-19>
    reason: Error in loadNamespace(name) : there is no package called 'formatR'
    
    Warning: Transformation introduced infinite values in continuous x-axis
    Warning: Removed 17 rows containing missing values (geom_point).
    Warning: Removed 2 rows containing missing values (geom_point).
    Error: processing vignette 'XBSeq.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

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

# XKCDdata

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tibble’
      All declared Imports should be used.
    ```

# xxIRT

Version: 2.0.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘graphics’
      All declared Imports should be used.
    ```

# zeligverse

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘Amelia’ ‘MatchIt’ ‘WhatIf’
      All declared Imports should be used.
    ```

# zFactor

Version: 0.1.7

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘rootSolve’
      All declared Imports should be used.
    ```

# ztype

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘ggplot2’ ‘lubridate’
      All declared Imports should be used.
    ```

