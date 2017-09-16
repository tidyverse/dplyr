# abjutils

Version: 0.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘devtools’ ‘httr’
      All declared Imports should be used.
    ```

# adegenet

Version: 2.0.1

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
       Converting data from a Genepop .gen file to a genind object... 
      
      
      File description:    Microsat on Chiracus radioactivus, a pest species  
      
      ...done.
      
      testthat results ================================================================
      OK: 173 SKIPPED: 0 FAILED: 2
      1. Failure: genlight objects do not take a mixture of positive and negative subscripts (@test_genlight.R#105) 
      2. Failure: df2genind works with haploids (@test_import.R#11) 
      
      Error: testthat unit tests failed
      Execution halted
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

# alphavantager

Version: 0.1.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(testthat)
      > library(alphavantager)
      > 
      > test_check("alphavantager")
      1. Error: call TIMES_SERIES_INTRADAY (@test_av_get.R#13) -----------------------
      The intraday time series encountered an internal or external error - we apologize for the inconvenience. If you would like to obtain the realtime stock quote for the current trading day, please refer to the latest data point of our DAILY and DAILY ADJUSTED functions (e.g., https://www.alphavantage.co/query?function=TIME_SERIES_DAILY&symbol=MSFT&apikey=demo | https://www.alphavantage.co/query?function=TIME_SERIES_DAILY_ADJUSTED&symbol=MSFT&apikey=demo).. API parameters used: symbol=MSFT, function=TIME_SERIES_INTRADAY, interval=15min, apikey=HIDDEN_FOR_YOUR_SAFETY
      1: av_get(symbol, av_fun, interval = interval) at testthat/test_av_get.R:13
      2: stop(content, call. = F)
      
      testthat results ================================================================
      OK: 9 SKIPPED: 0 FAILED: 1
      1. Error: call TIMES_SERIES_INTRADAY (@test_av_get.R#13) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘devtools’
      All declared Imports should be used.
    ```

# ameco

Version: 0.2.7

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(testthat)
      > library(ameco)
      > 
      > test_check("ameco")
      1. Failure: Test that current version is still latest version (@tests.R#16) ----
      `last_update` not equal to as.Date("2017-05-11").
      'is.NA' value mismatch: 0 in current 1 in target
      
      
      testthat results ================================================================
      OK: 0 SKIPPED: 0 FAILED: 1
      1. Failure: Test that current version is still latest version (@tests.R#16) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

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

*   checking R code for possible problems ... NOTE
    ```
    plot_coannotations: no visible binding for global variable ‘.’
    plot_numerical_coannotations: no visible binding for global variable
      ‘.’
    Undefined global functions or variables:
      .
    ```

# ArchaeoPhases

Version: 1.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘DT’ ‘dplyr’
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

# banR

Version: 0.2.0

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      The API sent back an error 503
      1: expect_is(banR::geocode_tbl(tbl = table_test, adresse = city), "tbl_df") at testthat/test_geocodetbl.R:48
      2: klass(object)
      3: paste(class(x), collapse = "/")
      4: banR::geocode_tbl(tbl = table_test, adresse = city)
      5: stop("The API sent back an error ", httr::status_code(query_results))
      
      testthat results ================================================================
      OK: 4 SKIPPED: 0 FAILED: 3
      1. Error: Geocode tbl works  (@test_geocodetbl.R#16) 
      2. Error: Input and output DFs have a similar number of rows (@test_geocodetbl.R#31) 
      3. Error: Geocode_tbl works with a single-column input data.frame (@test_geocodetbl.R#48) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘stringr’
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

# biomartr

Version: 0.5.1

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      1.  Error: The getAssemblyStats() downloads assembly stats file and reads
                raw input: NCBI RefSeq .. (@test-getAssemblyStats.R#19) 
      2.  Error: The getAssemblyStats() downloads assembly stats file and reads raw
                input: NCBI Genbank .. (@test-getAssemblyStats.R#41) 
      3.  Error: The getCDS() interface to NCBI RefSeq works properly.. (@test-getCDS.R#6) 
      4.  Error: The getCDS() interface to NCBI Genbank works properly.. (@test-getCDS.R#16) 
      5.  Error: The getGenome() interface works properly for NCBI RefSeq.. (@test-getGenome.R#7) 
      6.  Error: The getGenome() interface works properly for NCBI Genbank.. (@test-getGenome.R#31) 
      7.  Error: The getProteome() interface to NCBI RefSeq works properly.. (@test-getProteome.R#7) 
      8.  Error: The getProteome() interface to NCBI Genbank works properly.. (@test-getProteome.R#33) 
      9.  Error: The getRNA() interface to NCBI RefSeq works properly.. (@test-getRNA.R#8) 
      10. Error: The getRNA() interface to NCBI Genbank works properly.. (@test-getRNA.R#23) 
      
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

Version: 1.3.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 32.7Mb
      sub-directories of 1Mb or more:
        libs  32.4Mb
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
      installed size is  6.3Mb
      sub-directories of 1Mb or more:
        libs   5.9Mb
    ```

# breathteststan

Version: 0.3.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 24.7Mb
      sub-directories of 1Mb or more:
        libs  24.6Mb
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

Version: 0.6.6

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘gamlss.dist’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 18 marked UTF-8 strings
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
    Loading required package: GO.db
    
    Loading required package: org.Hs.eg.db
    
    Loading required package: TxDb.Hsapiens.UCSC.hg19.knownGene
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

# ciTools

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘methods’
      All declared Imports should be used.
    ```

# clustermq

Version: 0.7.0

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
        libs  10.5Mb
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

# collapsibleTree

Version: 0.1.5

## In both

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      ‘knitr’ ‘shiny’
    ```

# condformat

Version: 0.6.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘utils’
      All declared Imports should be used.
    ```

# congressbr

Version: 0.1.1

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked UTF-8 string
    ```

# countyfloods

Version: 0.0.2

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
    Quitting from lines 304-306 (countyflood.Rmd) 
    Error: processing vignette 'countyflood.Rmd' failed with diagnostics:
    there is no package called 'hurricaneexposuredata'
    Execution halted
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

# curatedMetagenomicData

Version: 1.2.0

## In both

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

Version: 0.6.9

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

# darksky

Version: 1.0.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      + }
      1. Error: the API call works (@test-darksky.R#4) -------------------------------
      Please set env var DARKSKY_API_KEY to your Dark Sky API key
      1: get_current_forecast(43.2672, -70.8617) at testthat/test-darksky.R:4
      2: sprintf("https://api.darksky.net/forecast/%s/%s,%s", darksky_api_key(), latitude, 
             longitude)
      3: darksky_api_key()
      4: stop("Please set env var DARKSKY_API_KEY to your Dark Sky API key", call. = FALSE)
      
      testthat results ================================================================
      OK: 0 SKIPPED: 0 FAILED: 1
      1. Error: the API call works (@test-darksky.R#4) 
      
      Error: testthat unit tests failed
      Execution halted
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

Version: 2.7.2

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      5: stop("Unrecognized response from the web service")
      
      3. Failure: Unit value data returns correct types (@tests_userFriendly_fxns.R#38) 
      all(...) isn't true.
      
      
      Read 7 items
      testthat results ================================================================
      OK: 161 SKIPPED: 0 FAILED: 3
      1. Error: General NWIS retrievals working (@tests_general.R#58) 
      2. Error: NGWMN functions working (@tests_general.R#262) 
      3. Failure: Unit value data returns correct types (@tests_userFriendly_fxns.R#38) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

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
      installed size is  6.8Mb
      sub-directories of 1Mb or more:
        libs   5.3Mb
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

# dexter

Version: 0.4.4

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘Rdpack’
      All declared Imports should be used.
    ```

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

Version: 2.2.0

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

# esc

Version: 0.3.1

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

Version: 0.33

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

*   R CMD check timed out
    

# factorMerger

Version: 0.3.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘forcats’
      All declared Imports should be used.
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

# FedData

Version: 2.4.6

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      Attaching package: 'raster'
      
      The following objects are masked from 'package:aqp':
      
          metadata, metadata<-
      
      testthat results ================================================================
      OK: 24 SKIPPED: 0 FAILED: 1
      1. Error: ITRDB version files are available (@test.ITRDB.R#23) 
      
      Error: testthat unit tests failed
      In addition: Warning message:
      closing unused connection 3 (ftp://ftp.ncdc.noaa.gov/pub/data/paleo/treering/chronologies/) 
      Execution halted
    ```

# FindMyFriends

Version: 1.6.0

## In both

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

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘ggcyto’
    ```

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
      installed size is 107.9Mb
      sub-directories of 1Mb or more:
        doc       1.1Mb
        include   2.7Mb
        lib      72.8Mb
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
      OK: 51 SKIPPED: 0 FAILED: 8
      1. Failure: output of summary cran results (@test-foghorn.R#176) 
      2. Failure: output of summary cran results (@test-foghorn.R#178) 
      3. Failure: output of summary cran results (@test-foghorn.R#189) 
      4. Failure: output of summary cran results (@test-foghorn.R#201) 
      5. Failure: output of summary cran results (@test-foghorn.R#213) 
      6. Failure: output of summary cran results (@test-foghorn.R#226) 
      7. Failure: output of summary cran results (@test-foghorn.R#233) 
      8. Failure: output of summary cran results (@test-foghorn.R#239) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# fourierin

Version: 0.2.2

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
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

Version: 0.1.5

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

Version: 0.8.16

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
      installed size is 11.0Mb
      sub-directories of 1Mb or more:
        doc    2.2Mb
        libs   8.6Mb
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
          url
      
      1. Failure: error on url (@test-geoknife_utils.R#15) ---------------------------
      error$message does not match "Couldn't resolve host name".
      Actual value: "Could not resolve host: bad.url.html"
      
      
      XPath error : Undefined namespace prefix
      XPath error : Invalid expression
      testthat results ================================================================
      OK: 181 SKIPPED: 0 FAILED: 1
      1. Failure: error on url (@test-geoknife_utils.R#15) 
      
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
    
    The following object is masked from 'package:Biobase':
    
        combine
    
    The following objects are masked from 'package:BiocGenerics':
    
        combine, intersect, setdiff, union
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Quitting from lines 273-277 (GEOmetadb.Rmd) 
    Error: processing vignette 'GEOmetadb.Rmd' failed with diagnostics:
    The dbplyr package is required to communicate with database backends.
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

Version: 1.1

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

# ggmap

Version: 2.6.1

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘ggmap-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: print.ggmap
    > ### Title: Print a map
    > ### Aliases: print.ggmap
    > 
    > ### ** Examples
    > 
    > 
    > get_map()
    Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=29.763284,-95.363271&zoom=10&size=640x640&scale=2&maptype=terrain&language=en-EN&sensor=false
    1280x1280 terrain map image from Google Maps.  see ?ggmap to plot it.> ggmap(get_map())
    Warning in download.file(url, destfile = tmp, quiet = !messaging, mode = "wb") :
      cannot open URL 'http://maps.googleapis.com/maps/api/staticmap?center=29.763284,-95.363271&zoom=10&size=640x640&scale=2&maptype=terrain&language=en-EN&sensor=false': HTTP status was '403 Forbidden'
    Error in download.file(url, destfile = tmp, quiet = !messaging, mode = "wb") : 
      cannot open URL 'http://maps.googleapis.com/maps/api/staticmap?center=29.763284,-95.363271&zoom=10&size=640x640&scale=2&maptype=terrain&language=en-EN&sensor=false'
    Calls: ggmap -> get_map -> get_googlemap -> download.file
    Execution halted
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

# ggpubr

Version: 0.1.5

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

Version: 1.5.1.2

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘taxstats’
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

# hei

Version: 0.1.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
      Files named as vignettes but with no recognized vignette engine:
       ‘vignettes/Analysis.Rmd’
    (Is a VignetteBuilder field missing?)
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    simpleWarning in download.file(url, tf, mode = "wb", quiet = TRUE): downloaded length 10292897 != reported length 13640000
    
    
    Quitting from lines 36-39 (GettingStarted.Rmd) 
    Error: processing vignette 'GettingStarted.Rmd' failed with diagnostics:
    attempt to set an attribute on NULL
    Execution halted
    ```

# hiAnnotator

Version: 1.10.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/runTests.R’ failed.
    Last 13 lines of output:
      Component "queryHits": Numeric: lengths (5, 4) differ
      Component "subjectHits": Numeric: lengths (5, 4) differ
      Component "dist": Numeric: lengths (5, 4) differ
      Component "qStrand": Lengths (5, 4) differ (string compare on first 4)
      Component "u2": Numeric: lengths (5, 4) differ
      Component "u1": Numeric: lengths (5, 4) differ
      Component "d1": Numeric: lengths (5, 4) differ
      Component "d2": Numeric: lengths (5, 4) differ
      
      
      DONE ===========================================================================
      Error: Test failures
      In addition: Warning message:
      Placing tests in `inst/tests/` is deprecated. Please use `tests/testthat/` instead 
      Execution halted
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      ‘seqlevelsInUse’
    makeGRanges: no visible global function definition for ‘IRanges’
    makeGRanges: no visible global function definition for ‘seqlengths’
    makeGRanges: no visible global function definition for ‘read.delim’
    makeGRanges: no visible global function definition for ‘seqlevels<-’
    makeGRanges: no visible global function definition for ‘sortSeqlevels’
    makeGRanges: no visible global function definition for ‘seqlevels’
    makeGRanges: no visible global function definition for ‘seqlengths<-’
    plotdisFeature: no visible global function definition for ‘is’
    plotdisFeature: no visible global function definition for ‘filter’
    Undefined global functions or variables:
      IRanges as breakInChunks countQueryHits detectCores dist featureName
      filter is keepSeqlevels mid n overlapsAny queryHits read.delim
      seqlengths seqlengths<- seqlevels seqlevels<- seqlevelsInUse
      sortSeqlevels
    Consider adding
      importFrom("methods", "as", "is")
      importFrom("stats", "dist", "filter")
      importFrom("utils", "read.delim")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
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

# hurricaneexposure

Version: 0.0.1

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘hurricaneexposuredata’
    ```

# huxtable

Version: 0.3.1

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
             message = handle_message))
      4: withCallingHandlers(withVisible(code), warning = handle_warning, message = handle_message)
      5: withVisible(code)
      6: rmarkdown::render("rowheight-multicol-test.Rmd", quiet = TRUE)
      7: convert(output_file, run_citeproc)
      8: pandoc_convert(utf8_input, pandoc_to, output_format$pandoc$from, output, citeproc, 
             output_format$pandoc$args, !quiet)
      9: stop("pandoc document conversion failed with error ", result, call. = FALSE)
      
      testthat results ================================================================
      OK: 218 SKIPPED: 49 FAILED: 1
      1. Error: Row heights do not screw up latex multicol (@test-with-pandoc.R#20) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# hypothesisr

Version: 0.1.1

## Newly broken

*   R CMD check timed out
    

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

## In both

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

# incadata

Version: 0.6.1

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 568 marked UTF-8 strings
    ```

# incR

Version: 0.3.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘rgeos’
      All declared Imports should be used.
    ```

# influxdbr

Version: 0.13.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      testthat results ================================================================
      OK: 0 SKIPPED: 0 FAILED: 11
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

Version: 0.3.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 206.0Mb
      sub-directories of 1Mb or more:
        extdata  205.2Mb
    ```

# jpndistrict

Version: 0.2.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 188 marked UTF-8 strings
    ```

# kokudosuuchi

Version: 0.2.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 47 marked UTF-8 strings
    ```

# KraljicMatrix

Version: 0.1.2

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

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘htmlwidgets’ ‘shiny’
      All declared Imports should be used.
    ```

# leaflet.extras

Version: 0.2

## In both

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

# lplyr

Version: 0.1.6

## In both

*   checking examples ... ERROR
    ```
    ...
    
    > rename(xs, x0 = x1)
    $x0
    [1] 1 2 3
    
    $x2
    [1] 2 3 4 5
    
    $x3
    $x3[[1]]
    [1] "alpha"
    
    $x3[[2]]
    [1] "beta"  "gamma"
    
    
    > select(xs, -x3)
    Error in select.list(xs, -x3) : 
      select.list() cannot be used non-interactively
    Calls: select -> select.list
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
    
    
    Attaching package: 'lplyr'
    
    The following object is masked from 'package:dplyr':
    
        pull
    
    Quitting from lines 22-30 (lplyr-vignette.Rmd) 
    Error: processing vignette 'lplyr-vignette.Rmd' failed with diagnostics:
    select.list() cannot be used non-interactively
    Execution halted
    ```

# LymphoSeq

Version: 1.4.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.3Mb
      sub-directories of 1Mb or more:
        doc       2.6Mb
        extdata   5.5Mb
    ```

# macleish

Version: 0.3.0

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘macleish-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: etl_extract.etl_macleish
    > ### Title: Extract weather data
    > ### Aliases: etl_extract.etl_macleish etl_load.etl_macleish
    > ###   etl_transform.etl_macleish
    > 
    > ### ** Examples
    > 
    > 
    > macleish <- etl("macleish")
    Not a valid src. Creating a src_sqlite for you at:
    /home/muelleki/tmp/RtmpA2EjS7/file7f0f2d70803.sqlite3
    Error: The dbplyr package is required to communicate with database backends.
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      ‘rgdal’ ‘rgeos’
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

# mnis

Version: 0.2.7

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat-a.R’ failed.
    Last 13 lines of output:
      > library(testthat)
      > library(mnis)
      > 
      > test_check("mnis", filter= "a")
      1. Failure: mnis_additional returns expected format (@test_additional.R#80) ----
      `xi` has length 339, not length 285.
      
      
      testthat results ================================================================
      OK: 98 SKIPPED: 0 FAILED: 1
      1. Failure: mnis_additional returns expected format (@test_additional.R#80) 
      
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

Version: 1.1.6

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘Momocs-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: transmute
    > ### Title: Transmutes (ala dplyr) on Momocs objects
    > ### Aliases: transmute
    > 
    > ### ** Examples
    > 
    > olea
    An Opn object with: 
     - $coo: 210 open outlines (99 +/- 4 coordinates)
     - $fac: 4 classifiers:
         'var' (factor 4): Aglan, Cypre, MouBo1, PicMa.
         'domes' (factor 2): cult, wild.
         'view' (factor 2): VD, VL.
         'ind' (factor 30): O1, O10, O11, O12, O13, O14, O15, O16, O17, O18, O19 ... + 19 more.
    > transmute(olea, id=factor(1:length(olea)))
    Error: evaluation nested too deeply: infinite recursion / options(expressions=)?
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
    method not provided. efourier is used
    method not provided. efourier is used
    method not provided. efourier is used
    'nb.h' not provided and set to 8 (99% harmonic power)
    'nb.h' not provided and set to 8 (99% harmonic power)
    'nb.pts' missing and set to 91
    'degree' missing and set to 5
    'nb.pts' missing and set to 91
    'degree' missing and set to 5
    'nb.pts' missing and set to 91
    'degree' missing and set to 5
    'nb.h' not provided and set to 10 (99% harmonic power)
    PC axes 1 to 7 were retained
    PC axes 1 to 7 were retained
    'nb.h' not provided and set to 10 (99% harmonic power)
    PC axes 1 to 7 were retained
    no 'fac' provided, returns meanshape
    Error: processing vignette 'Momocs_speed_dating.Rmd' failed with diagnostics:
    evaluation nested too deeply: infinite recursion / options(expressions=)?
    Execution halted
    ```

# MonetDBLite

Version: 0.4.1

## In both

*   R CMD check timed out
    

*   checking installed package size ... NOTE
    ```
      installed size is  6.6Mb
      sub-directories of 1Mb or more:
        libs   6.4Mb
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
    
    Warning in (ap %*% ab)/(ab %*% ab) * ab :
      Recycling array of length 1 in array-vector arithmetic is deprecated.
      Use c() or as.vector() instead.
    
    Warning in (ap %*% ab)/(ab %*% ab) * ab :
      Recycling array of length 1 in array-vector arithmetic is deprecated.
      Use c() or as.vector() instead.
    
    Warning in (ap %*% ab)/(ab %*% ab) * ab :
      Recycling array of length 1 in array-vector arithmetic is deprecated.
      Use c() or as.vector() instead.
    
    Warning in (ap %*% ab)/(ab %*% ab) * ab :
      Recycling array of length 1 in array-vector arithmetic is deprecated.
      Use c() or as.vector() instead.
    
    Quitting from lines 758-768 (monocle-vignette.Rnw) 
    Error: processing vignette 'monocle-vignette.Rnw' failed with diagnostics:
    Unknown vertex selected
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

Version: 0.8.9

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.8Mb
      sub-directories of 1Mb or more:
        libs   5.6Mb
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

# mudata

Version: 0.1

## In both

*   checking Rd \usage sections ... NOTE
    ```
    S3 methods shown with full name in documentation object 'rbind.mudata':
      ‘rbind.mudata’
    
    S3 methods shown with full name in documentation object 'rbind.qtag.long':
      ‘rbind.qtag.long’ ‘rbind.qtag’
    
    The \usage entries for S3 methods should use the \method markup and not
    their full name.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
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

# neuropsychology

Version: 0.5.0

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

Version: 0.3.3

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 208-225 (README.Rmd) 
    Error: processing vignette 'README.Rmd' failed with diagnostics:
    Evaluation error: votes, parties, and electorates should all be vectors of the same length.
    Execution halted
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 4204 marked UTF-8 strings
    ```

# observer

Version: 0.1.2

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘ensurer’, ‘validate’
    ```

# officer

Version: 0.1.6

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
      10: external_img(src, width = width, height = height)
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

Version: 1.2.1

## In both

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

Version: 0.2.0

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
    Error: The dbplyr package is required to communicate with database backends.
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      10: src_sqlite(msf_file)
      11: check_dbplyr()
      12: check_pkg("dbplyr", "communicate with database backends", install = FALSE)
      13: glubort(NULL, "The {name} package is required to {reason}.", if (install) "\nPlease install it with `install.packages(\"{name}\")`")
      14: .abort(text)
      
      Error: The dbplyr package is required to communicate with database backends.
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
    The dbplyr package is required to communicate with database backends.
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘RSQLite’
      All declared Imports should be used.
    ```

# patternplot

Version: 0.1

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

# philr

Version: 1.2.0

## In both

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

# poplite

Version: 0.99.17.3

## In both

*   checking examples ... ERROR
    ```
    ...
    +  columns(baseball.db)
    +  schema(baseball.db)
    +  
    +  populate(baseball.db, teams=Teams, team_franch=TeamsFranchises)
    +  
    +  examp.con <- dbConnect(SQLite(), dbFile(baseball.db))
    +  
    +  dbListTables(examp.con)
    +  
    +  head(dbReadTable(examp.con, "teams"))
    +  head(dbReadTable(examp.con, "team_franch"))
    +  
    +  dbDisconnect(examp.con)
    +  
    + }
    Loading required package: Lahman
    Loading required package: RSQLite
    Error in rsqlite_connection_valid(dbObj@ptr) : 
      external pointer is not valid
    Calls: populate ... dbIsValid -> dbIsValid -> rsqlite_connection_valid -> .Call
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      8: src_sqlite(dbFile(obj), create = F)
      9: check_dbplyr()
      10: check_pkg("dbplyr", "communicate with database backends", install = FALSE)
      11: glubort(NULL, "The {name} package is required to {reason}.", if (install) "\nPlease install it with `install.packages(\"{name}\")`")
      12: .abort(text)
      
      testthat results ================================================================
      OK: 109 SKIPPED: 0 FAILED: 4
      1. Error: Database population (@test-poplite.R#452) 
      2. Error: Querying with Database objects (@test-poplite.R#567) 
      3. Error: sample tracking example but with direct keys between dna and samples (@test-poplite.R#801) 
      4. Error: oligoMask queries that break poplite (@test-poplite.R#852) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    ...
    
    Loading required package: DBI
    
    Attaching package: ‘poplite’
    
    The following object is masked from ‘package:dplyr’:
    
        select
    
    The following object is masked from ‘package:stats’:
    
        filter
    
    Error in makeSchemaFromData(dna, "dna") : 
      ERROR: The names of the supplied data.frame need to be modified for the database see correct.df.names
    
    Error: processing vignette 'poplite.Rnw' failed with diagnostics:
     chunk 8 
    Error in rsqlite_connection_valid(dbObj@ptr) : 
      external pointer is not valid
    Execution halted
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

Version: 0.2

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

# psychmeta

Version: 0.1.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 5 marked UTF-8 strings
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

Version: 0.6.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘DT’
      All declared Imports should be used.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘Hmisc’
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 4145 marked UTF-8 strings
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
      installed size is 17.8Mb
      sub-directories of 1Mb or more:
        data   3.6Mb
        libs  13.0Mb
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
    
    (R:55283): Gtk-WARNING **: gtk_disable_setlocale() must be called before gtk_init()
    ```

# RBesT

Version: 1.2-3

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 27.6Mb
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
      * 500: {"status":"error","message-type":"exception","message-version":"1.0.0","message":{"name":"class java.lang.NumberFormatException","description":"java.lang.NumberFormatException: For input string: \\"323234343434\\"","message":"For input string: \\"323234343434\\"","stack":["java.lang.NumberFormatException.forInputString(NumberFormatException.java:65)","java.lang.Integer.parseInt(Integer.java:583)","java.lang.Integer.parseInt(Integer.java:615)","cayenne.data.member$get_id_from_context.invokeStatic(member.clj:34)","cayenne.data.member$get_id_from_context.invoke(member.clj:30)","cayenne.data.member$fetch_one$fn__17560.invoke(member.clj:75)","cayenne.data.member$fetch_one.invokeStatic(member.clj:72)","cayenne.data.member$fetch_one.invoke(member.clj:71)","cayenne.api.v1.routes$member_works_resource$fn__22168$fn__22169.invoke(routes.clj:375)","liberator.core$decide.invokeStatic(core.clj:98)","liberator.core$decide.invoke(core.clj:91)","liberator.core$exists_QMARK_.invokeStatic(core.clj:406)","liberator.core$exists_QMARK_.invoke(core.clj:406)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$processable_QMARK_.invokeStatic(core.clj:409)","liberator.core$processable_QMARK_.invoke(core.clj:409)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$encoding_available_QMARK_.invokeStatic(core.clj:413)","liberator.core$encoding_available_QMARK_.invoke(core.clj:413)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$accept_encoding_exists_QMARK_.invokeStatic(core.clj:428)","liberator.core$accept_encoding_exists_QMARK_.invoke(core.clj:428)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$accept_charset_exists_QMARK_.invokeStatic(core.clj:441)","liberator.core$accept_charset_exists_QMARK_.invoke(core.clj:441)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$accept_language_exists_QMARK_.invokeStatic(core.clj:455)","liberator.core$accept_language_exists_QMARK_.invoke(core.clj:455)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$media_type_available_QMARK_.invokeStatic(core.clj:465)","liberator.core$media_type_available_QMARK_.invoke(core.clj:465)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$accept_exists_QMARK_.invokeStatic(core.clj:468)","liberator.core$accept_exists_QMARK_.invoke(core.clj:468)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$is_options_QMARK_.invokeStatic(core.clj:485)","liberator.core$is_options_QMARK_.invoke(core.clj:485)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$valid_entity_length_QMARK_.invokeStatic(core.clj:488)","liberator.core$valid_entity_length_QMARK_.invoke(core.clj:488)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$known_content_type_QMARK_.invokeStatic(core.clj:491)","liberator.core$known_content_type_QMARK_.invoke(core.clj:491)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$valid_content_header_QMARK_.invokeStatic(core.clj:493)","liberator.core$valid_content_header_QMARK_.invoke(core.clj:493)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$allowed_QMARK_.invokeStatic(core.clj:496)","liberator.core$allowed_QMARK_.invoke(core.clj:496)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$authorized_QMARK_.invokeStatic(core.clj:499)","liberator.core$authorized_QMARK_.invoke(core.clj:499)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$malformed_QMARK_.invokeStatic(core.clj:502)","liberator.core$malformed_QMARK_.invoke(core.clj:502)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$method_allowed_QMARK_.invokeStatic(core.clj:505)","liberator.core$method_allowed_QMARK_.invoke(core.clj:505)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$uri_too_long_QMARK_.invokeStatic(core.clj:508)","liberator.core$uri_too_long_QMARK_.invoke(core.clj:508)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$known_method_QMARK_.invokeStatic(core.clj:511)","liberator.core$known_method_QMARK_.invoke(core.clj:511)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$service_available_QMARK_.invokeStatic(core.clj:514)","liberator.core$service_available_QMARK_.invoke(core.clj:514)","liberator.core$run_resource.invokeStatic(core.clj:599)","liberator.core$run_resource.invoke(core.clj:597)","cayenne.api.v1.routes$member_works_resource$fn__22168.invoke(routes.clj:364)","compojure.response$eval21279$fn__21280.invoke(response.clj:47)","compojure.response$eval21201$fn__21202$G__21192__21209.invoke(response.clj:7)","compojure.core$wrap_response$fn__21871.invoke(core.clj:158)","compojure.core$wrap_route_middleware$fn__21855.invoke(core.clj:128)","compojure.core$wrap_route_info$fn__21860.invoke(core.clj:137)","compojure.core$wrap_route_matches$fn__21864.invoke(core.clj:146)","compojure.core$routing$fn__21879.invoke(core.clj:185)","clojure.core$some.invokeStatic(core.clj:2592)","clojure.core$some.invoke(core.clj:2583)","compojure.core$routing.invokeStatic(core.clj:185)","compojure.core$routing.doInvoke(core.clj:182)","clojure.lang.RestFn.applyTo(RestFn.java:139)","clojure.core$apply.invokeStatic(core.clj:648)","clojure.core$apply.invoke(core.clj:641)","compojure.core$routes$fn__21883.invoke(core.clj:192)","compojure.core$routing$fn__21879.invoke(core.clj:185)","clojure.core$some.invokeStatic(core.clj:2592)","clojure.core$some.invoke(core.clj:2583)","compojure.core$routing.invokeStatic(core.clj:185)","compojure.core$routing.doInvoke(core.clj:182)","clojure.lang.RestFn.applyTo(RestFn.java:139)","clojure.core$apply.invokeStatic(core.clj:648)","clojure.core$apply.invoke(core.clj:641)","compojure.core$routes$fn__21883.invoke(core.clj:192)","compojure.core$routing$fn__21879.invoke(core.clj:185)","clojure.core$some.invokeStatic(core.clj:2592)","clojure.core$some.invoke(core.clj:2583)","compojure.core$routing.invokeStatic(core.clj:185)","compojure.core$routing.doInvoke(core.clj:182)","clojure.lang.RestFn.applyTo(RestFn.java:139)","clojure.core$apply.invokeStatic(core.clj:648)","clojure.core$apply.invoke(core.clj:641)","compojure.core$routes$fn__21883.invoke(core.clj:192)","ring.middleware.logstash$wrap_logstash$fn__23200.invoke(logstash.clj:110)","ring.middleware.keyword_params$wrap_keyword_params$fn__24062.invoke(keyword_params.clj:36)","ring.middleware.nested_params$wrap_nested_params$fn__24110.invoke(nested_params.clj:89)","ring.middleware.params$wrap_params$fn__24026.invoke(params.clj:67)","cayenne.api.route$wrap_cors$fn__24646.invoke(route.clj:101)","metrics.ring.expose$expose_metrics_as_json$fn__23586.invoke(expose.clj:94)","metrics.ring.instrument$instrument$fn__23602$fn__23603.invoke(instrument.clj:44)","metrics.ring.instrument.proxy$java.lang.Object$Callable$7da976d4.call(Unknown Source)","com.yammer.metrics.core.Timer.time(Timer.java:91)","metrics.ring.instrument$instrument$fn__23602.invoke(instrument.clj:43)","heartbeat.ring$wrap_heartbeat$fn__23288.invoke(ring.clj:18)","cayenne.api.conneg$wrap_accept$fn__19742.invoke(conneg.clj:53)","cayenne.api.route$wrap_exception_handler$fn__24651.invoke(route.clj:110)","cayenne.api.route$wrap_ignore_trailing_slash$fn__24659.invoke(route.clj:136)","org.httpkit.server.HttpHandler.run(RingHandler.java:91)","java.util.concurrent.Executors$RunnableAdapter.call(Executors.java:511)","java.util.concurrent.Future
      * 500: {"status":"error","message-type":"exception","message-version":"1.0.0","message":{"name":"class java.lang.NumberFormatException","description":"java.lang.NumberFormatException: For input string: \\"3434343434\\"","message":"For input string: \\"3434343434\\"","stack":["java.lang.NumberFormatException.forInputString(NumberFormatException.java:65)","java.lang.Integer.parseInt(Integer.java:583)","java.lang.Integer.parseInt(Integer.java:615)","cayenne.data.member$get_id_from_context.invokeStatic(member.clj:34)","cayenne.data.member$get_id_from_context.invoke(member.clj:30)","cayenne.data.member$fetch_one$fn__17560.invoke(member.clj:75)","cayenne.data.member$fetch_one.invokeStatic(member.clj:72)","cayenne.data.member$fetch_one.invoke(member.clj:71)","cayenne.api.v1.routes$member_works_resource$fn__22168$fn__22169.invoke(routes.clj:375)","liberator.core$decide.invokeStatic(core.clj:98)","liberator.core$decide.invoke(core.clj:91)","liberator.core$exists_QMARK_.invokeStatic(core.clj:406)","liberator.core$exists_QMARK_.invoke(core.clj:406)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$processable_QMARK_.invokeStatic(core.clj:409)","liberator.core$processable_QMARK_.invoke(core.clj:409)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$encoding_available_QMARK_.invokeStatic(core.clj:413)","liberator.core$encoding_available_QMARK_.invoke(core.clj:413)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$accept_encoding_exists_QMARK_.invokeStatic(core.clj:428)","liberator.core$accept_encoding_exists_QMARK_.invoke(core.clj:428)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$accept_charset_exists_QMARK_.invokeStatic(core.clj:441)","liberator.core$accept_charset_exists_QMARK_.invoke(core.clj:441)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$accept_language_exists_QMARK_.invokeStatic(core.clj:455)","liberator.core$accept_language_exists_QMARK_.invoke(core.clj:455)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$media_type_available_QMARK_.invokeStatic(core.clj:465)","liberator.core$media_type_available_QMARK_.invoke(core.clj:465)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$accept_exists_QMARK_.invokeStatic(core.clj:468)","liberator.core$accept_exists_QMARK_.invoke(core.clj:468)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$is_options_QMARK_.invokeStatic(core.clj:485)","liberator.core$is_options_QMARK_.invoke(core.clj:485)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$valid_entity_length_QMARK_.invokeStatic(core.clj:488)","liberator.core$valid_entity_length_QMARK_.invoke(core.clj:488)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$known_content_type_QMARK_.invokeStatic(core.clj:491)","liberator.core$known_content_type_QMARK_.invoke(core.clj:491)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$valid_content_header_QMARK_.invokeStatic(core.clj:493)","liberator.core$valid_content_header_QMARK_.invoke(core.clj:493)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$allowed_QMARK_.invokeStatic(core.clj:496)","liberator.core$allowed_QMARK_.invoke(core.clj:496)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$authorized_QMARK_.invokeStatic(core.clj:499)","liberator.core$authorized_QMARK_.invoke(core.clj:499)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$malformed_QMARK_.invokeStatic(core.clj:502)","liberator.core$malformed_QMARK_.invoke(core.clj:502)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$method_allowed_QMARK_.invokeStatic(core.clj:505)","liberator.core$method_allowed_QMARK_.invoke(core.clj:505)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$uri_too_long_QMARK_.invokeStatic(core.clj:508)","liberator.core$uri_too_long_QMARK_.invoke(core.clj:508)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$known_method_QMARK_.invokeStatic(core.clj:511)","liberator.core$known_method_QMARK_.invoke(core.clj:511)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$service_available_QMARK_.invokeStatic(core.clj:514)","liberator.core$service_available_QMARK_.invoke(core.clj:514)","liberator.core$run_resource.invokeStatic(core.clj:599)","liberator.core$run_resource.invoke(core.clj:597)","cayenne.api.v1.routes$member_works_resource$fn__22168.invoke(routes.clj:364)","compojure.response$eval21279$fn__21280.invoke(response.clj:47)","compojure.response$eval21201$fn__21202$G__21192__21209.invoke(response.clj:7)","compojure.core$wrap_response$fn__21871.invoke(core.clj:158)","compojure.core$wrap_route_middleware$fn__21855.invoke(core.clj:128)","compojure.core$wrap_route_info$fn__21860.invoke(core.clj:137)","compojure.core$wrap_route_matches$fn__21864.invoke(core.clj:146)","compojure.core$routing$fn__21879.invoke(core.clj:185)","clojure.core$some.invokeStatic(core.clj:2592)","clojure.core$some.invoke(core.clj:2583)","compojure.core$routing.invokeStatic(core.clj:185)","compojure.core$routing.doInvoke(core.clj:182)","clojure.lang.RestFn.applyTo(RestFn.java:139)","clojure.core$apply.invokeStatic(core.clj:648)","clojure.core$apply.invoke(core.clj:641)","compojure.core$routes$fn__21883.invoke(core.clj:192)","compojure.core$routing$fn__21879.invoke(core.clj:185)","clojure.core$some.invokeStatic(core.clj:2592)","clojure.core$some.invoke(core.clj:2583)","compojure.core$routing.invokeStatic(core.clj:185)","compojure.core$routing.doInvoke(core.clj:182)","clojure.lang.RestFn.applyTo(RestFn.java:139)","clojure.core$apply.invokeStatic(core.clj:648)","clojure.core$apply.invoke(core.clj:641)","compojure.core$routes$fn__21883.invoke(core.clj:192)","compojure.core$routing$fn__21879.invoke(core.clj:185)","clojure.core$some.invokeStatic(core.clj:2592)","clojure.core$some.invoke(core.clj:2583)","compojure.core$routing.invokeStatic(core.clj:185)","compojure.core$routing.doInvoke(core.clj:182)","clojure.lang.RestFn.applyTo(RestFn.java:139)","clojure.core$apply.invokeStatic(core.clj:648)","clojure.core$apply.invoke(core.clj:641)","compojure.core$routes$fn__21883.invoke(core.clj:192)","ring.middleware.logstash$wrap_logstash$fn__23200.invoke(logstash.clj:110)","ring.middleware.keyword_params$wrap_keyword_params$fn__24062.invoke(keyword_params.clj:36)","ring.middleware.nested_params$wrap_nested_params$fn__24110.invoke(nested_params.clj:89)","ring.middleware.params$wrap_params$fn__24026.invoke(params.clj:67)","cayenne.api.route$wrap_cors$fn__24646.invoke(route.clj:101)","metrics.ring.expose$expose_metrics_as_json$fn__23586.invoke(expose.clj:94)","metrics.ring.instrument$instrument$fn__23602$fn__23603.invoke(instrument.clj:44)","metrics.ring.instrument.proxy$java.lang.Object$Callable$7da976d4.call(Unknown Source)","com.yammer.metrics.core.Timer.time(Timer.java:91)","metrics.ring.instrument$instrument$fn__23602.invoke(instrument.clj:43)","heartbeat.ring$wrap_heartbeat$fn__23288.invoke(ring.clj:18)","cayenne.api.conneg$wrap_accept$fn__19742.invoke(conneg.clj:53)","cayenne.api.route$wrap_exception_handler$fn__24651.invoke(route.clj:110)","cayenne.api.route$wrap_ignore_trailing_slash$fn__24659.invoke(route.clj:136)","org.httpkit.server.HttpHandler.run(RingHandler.java:91)","java.util.concurrent.Executors$RunnableAdapter.call(Executors.java:511)","java.util.concurrent.FutureTask
      
      
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
      
      3. Failure: Write One Shot -Insert (@test-write-oneshot.R#52) ------------------
      The returned data.frame should be correct not equal to `expected_data_frame`.
      Component "address": Modes: numeric, character
      Component "address": target is numeric, current is character
      
      
      testthat results ================================================================
      OK: 567 SKIPPED: 0 FAILED: 3
      1. Error: Full Directory Specific (@test-file-oneshot.R#183) 
      2. Failure: Write Batch -Update Two Fields (@test-write-batch.R#173) 
      3. Failure: Write One Shot -Insert (@test-write-oneshot.R#52) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Failed with error:  'there is no package called 'kableExtra''
    Quitting from lines 70-73 (BasicREDCapROperations.Rmd) 
    Error: processing vignette 'BasicREDCapROperations.Rmd' failed with diagnostics:
    there is no package called 'kableExtra'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘kableExtra’
    ```

# replyr

Version: 0.5.3

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘magick’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘RSQLite’
      All declared Imports should be used.
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
      OK: 123 SKIPPED: 0 FAILED: 8
      1. Failure: ed_search_adv fails well (@test-ed_search_adv.R#58) 
      2. Failure: ed_search_adv fails well (@test-ed_search_adv.R#59) 
      3. Failure: info fails well (@test-info.R#48) 
      4. Failure: tabledap fields parameter works, and fails correctly (@test-tabledap.R#27) 
      5. Failure: tabledap units parameter works, and fails correctly (@test-tabledap.R#38) 
      6. Failure: tabledap fails well, in addition to above failure tests (@test-tabledap.R#47) 
      7. Failure: tabledap fails well, in addition to above failure tests (@test-tabledap.R#48) 
      8. Failure: tabledap fails well, in addition to above failure tests (@test-tabledap.R#50) 
      
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

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      3: FUN(X[[i]], ...)
      4: vector_url(x = tile_coordinates$x, y = tile_coordinates$y, z = tile_coordinates$z, 
             layers = "all", format = "json")
      5: structure(list(scheme = "https", hostname = "tile.mapzen.com", path = vector_path(layers, 
             x, y, z, format), query = list(api_key = api_key)), class = "url")
      6: mz_key()
      7: stop("Set the MAPZEN_KEY environment variable")
      
      testthat results ================================================================
      OK: 199 SKIPPED: 0 FAILED: 2
      1. Error: single tiles can be pulled (@test-mz-vector-tiles.R#14) 
      2. Error: multiple contiguous tiles can be pulled (@test-mz-vector-tiles.R#22) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 31 marked UTF-8 strings
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

Version: 0.2.0

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
      downloaded 470 KB
      
      testthat results ================================================================
      OK: 110 SKIPPED: 0 FAILED: 8
      1. Error: it can download a file with demographics (@test_data_files.R#132) 
      2. Failure: it can recode just data (@test_data_files.R#148) 
      3. Failure: nhanes_search on variables passes spot check (@test_search.R#13) 
      4. Failure: nhanes_search on variables passes spot check (@test_search.R#14) 
      5. Failure: nhanes_search on files passes spot check (@test_search.R#20) 
      6. Failure: nhanes_search on files passes spot check (@test_search.R#21) 
      7. Failure: fuzzy search works on files (@test_search.R#33) 
      8. Failure: fuzzy search works on files (@test_search.R#34) 
      
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
      OK: 181 SKIPPED: 1 FAILED: 9
      1. Error: check_response returns an error (@test-check_response.r#7) 
      2. Error: check_response returns the correct error messages (@test-check_response.r#26) 
      3. Error: ncdc returns the correct ... (@test-ncdc.r#8) 
      4. Error: ncdc_datacats returns the correct ... (@test-ncdc_datacats.r#7) 
      5. Error: ncdc_datasets returns the correct class (@test-ncdc_datasets.r#7) 
      6. Error: ncdc_datatypes returns the correct class (@test-ncdc_datatypes.r#7) 
      7. Error: ncdc_locs returns the correct class (@test-ncdc_locs.r#7) 
      8. Error: ncdc_locs_cats returns the correct ... (@test-ncdc_locs_cats.r#7) 
      9. Error: ncdc_stations returns the correct... (@test-ncdc_stations.r#7) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# rODE

Version: 0.99.5

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

# rpivotTable

Version: 0.2.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘shiny’
    ```

# rplos

Version: 0.6.4

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      1: searchplos(q = "*:*", fl = "id", fq = "cross_published_journal_key:PLoSONE", start = 0, 
             limit = 15) at testthat/test-searchplos.R:8
      2: check_response(tt)
      3: stop(sprintf("(%s) - %s", x$status_code, jsonlite::fromJSON(utf8cont(x), FALSE)$error$msg), 
             call. = FALSE)
      
      testthat results ================================================================
      OK: 224 SKIPPED: 0 FAILED: 4
      1. Error: journalnamekey returns the correct value (@test-journalnamekey.R#7) 
      2. Error: journalnamekey returns the correct class (@test-journalnamekey.R#13) 
      3. Error: journalnamekey returns the correct length vector (@test-journalnamekey.R#19) 
      4. Error: searchplos returns the correct (@test-searchplos.R#8) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘tm’
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

Version: 0.3.5

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

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘utils’
      All declared Imports should be used.
    ```

# rsparkling

Version: 0.2.1

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      target is logical, current is numeric
      
      
      testthat results ================================================================
      OK: 6 SKIPPED: 0 FAILED: 7
      1. Error: Test transformation from h2o frame to data frame (@test_transforms.R#5) 
      2. Error: Test transformation of a spark data_frame of bools to an h2o frame of bools (@test_transforms.R#17) 
      3. Error: Test transformation of a spark data_frame of complex types to an h2o frame of complex types (@test_transforms.R#29) 
      4. Error: Test transformation of a spark data_frame of float types to an h2o frame of floats (@test_transforms.R#43) 
      5. Error: Test transformation of a spark data_frame of int types to an h2o frame of ints (@test_transforms.R#54) 
      6. Error: Test transformation of a spark data_frame of str types to an h2o frame of str (@test_transforms.R#67) 
      7. Failure: Test transformation from dataframe to h2o frame (@test_transforms.R#81) 
      
      Error: testthat unit tests failed
      Execution halted
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
      ‘RTCGA.miRNASeq’ ‘RTCGA.methylation’ ‘RTCGA.CNV’ ‘RTCGA.PANCAN12’
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

Version: 0.6.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library("rvertnet")
      > 
      > test_check("rvertnet")
      1. Failure: searchbyterm works correctly (@test-searchbyterm.R#12) -------------
      NROW(searchbyterm(limit = 1, messages = FALSE)$data) not equal to 1.
      1/1 mismatches
      [1] 0 - 1 == -1
      
      
      testthat results ================================================================
      OK: 109 SKIPPED: 0 FAILED: 1
      1. Failure: searchbyterm works correctly (@test-searchbyterm.R#12) 
      
      Error: testthat unit tests failed
      Execution halted
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
      installed size is  6.8Mb
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

Version: 2.0.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.0Mb
      sub-directories of 1Mb or more:
        libs   5.0Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘NMOF’ ‘compositions’ ‘e1071’
      All declared Imports should be used.
    ```

# sf

Version: 0.5-4

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 14.2Mb
      sub-directories of 1Mb or more:
        doc      4.9Mb
        libs     5.6Mb
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

Version: 0.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘cowplot’ ‘dplyr’
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

Version: 3.6.3

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 12.3Mb
      sub-directories of 1Mb or more:
        libs  10.9Mb
    ```

# sjlabelled

Version: 1.0.2

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

Version: 2.6.1

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘sjPlot’
    ```

# sjPlot

Version: 2.3.3

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘effects’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# sjstats

Version: 0.11.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘sjPlot’, ‘MuMIn’, ‘piecewiseSEM’
    ```

# solrium

Version: 0.4.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
             url), call. = FALSE)
      
      testthat results ================================================================
      OK: 208 SKIPPED: 0 FAILED: 8
      1. Error: core_create works (@test-core_create.R#6) 
      2. Error: ping works against (@test-ping.R#7) 
      3. Error: ping gives raw data correctly (@test-ping.R#20) 
      4. Error: ping fails well (@test-ping.R#31) 
      5. Error: schema works against (@test-schema.R#7) 
      6. Error: schema fails well (@test-schema.R#32) 
      7. Error: solr_connect to local Solr server works (@test-solr_connect.R#19) 
      8. Error: solr_connect works with a proxy (@test-solr_connect.R#33) 
      
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

# sparseHessianFD

Version: 0.3.3

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

Version: 0.1.4

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

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘ggplot2’ ‘stringr’ ‘tidyverse’
      All declared Imports should be used.
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

Version: 0.13.0

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
        libs   2.0Mb
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
    Calls: quad.plot ... lapply -> FUN -> overscope_eval_next -> .Call -> get
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

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘broom’ ‘curl’ ‘devtools’ ‘rvest’ ‘timeSeries’ ‘tseries’ ‘zoo’
      All declared Imports should be used.
    ```

# tidyr

Version: 0.7.1

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

# TPP

Version: 3.4.3

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

# tRophicPosition

Version: 0.7.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘MCMCvis’ ‘dplyr’
      All declared Imports should be used.
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
        libs  14.0Mb
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

# wikipediatrend

Version: 1.1.10

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      2. Failure: setting cache file (@test_caching_gathering.R#39) ------------------
      all(dim(wp_get_cache()) > 0) isn't true.
      
      
      Error in curl::curl_fetch_memory(url, handle = handle) : 
        Timeout was reached: Connection timed out after 10001 milliseconds
      Error in curl::curl_fetch_memory(url, handle = handle) : 
        Timeout was reached: Connection timed out after 10001 milliseconds
      testthat results ================================================================
      OK: 66 SKIPPED: 0 FAILED: 2
      1. Failure: normal usage (@test_caching_gathering.R#27) 
      2. Failure: setting cache file (@test_caching_gathering.R#39) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
      The vignette engine knitr::rmarkdown is not available, because the rmarkdown package is not installed. Please install it.
    Error in curl::curl_fetch_memory(url, handle = handle) : 
      Timeout was reached: Connection timed out after 10000 milliseconds
    http://stats.grok.se/json/en/201510/Main_page
    
    data from server was: Error in curl::curl_fetch_memory(url, handle = handle) : 
      Timeout was reached: Connection timed out after 10000 milliseconds
    
    
    Error in curl::curl_fetch_memory(url, handle = handle) : 
      Timeout was reached: Connection timed out after 10001 milliseconds
    http://stats.grok.se/json/en/201511/Main_page
    
    data from server was: Error in curl::curl_fetch_memory(url, handle = handle) : 
      Timeout was reached: Connection timed out after 10001 milliseconds
    
    
    Quitting from lines 108-112 (using-wikipediatrend.Rmd) 
    Error: processing vignette 'using-wikipediatrend.Rmd' failed with diagnostics:
    need finite 'xlim' values
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      ‘AnomalyDetection’ ‘BreakoutDetection’
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
    The dbplyr package is required to communicate with database backends.
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

# wrswoR.benchmark

Version: 0.1-1

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘microbenchmark’
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

Version: 0.1.6

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘rootSolve’ ‘tibble’
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

