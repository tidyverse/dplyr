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

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(testthat)
      > library(annotatr)
      
      
      
      
      > 
      > test_check("annotatr")
      Error in function (type, msg, asError = TRUE)  : 
        Operation timed out after 0 milliseconds with 0 out of 0 bytes received
      Calls: test_check ... getURL -> curlPerform -> .Call -> <Anonymous> -> fun
      testthat results ================================================================
      OK: 43 SKIPPED: 0 FAILED: 0
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

Version: 0.1.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘RColorBrewer’ ‘gplots’ ‘tidyverse’
      All declared Imports should be used.
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

## Newly fixed

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 47-49 (geocode.Rmd) 
    Error: processing vignette 'geocode.Rmd' failed with diagnostics:
    The API sent back an error 503
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘stringr’
      All declared Imports should be used.
    ```

# bayesplot

Version: 1.3.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        R     1.6Mb
        doc   2.9Mb
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
      
      trying URL 'ftp://ftp.ncbi.nlm.nih.gov/genomes/GENOME_REPORTS/overview.txt'
      Content type 'unknown' length 2250186 bytes (2.1 MB)
      ==================================================
      trying URL 'ftp://ftp.ncbi.nlm.nih.gov/genomes/genbank/metagenomes/assembly_summary.txt'
      Content type 'unknown' length 360867 bytes (352 KB)
      ==================================================
      testthat results ================================================================
      OK: 49 SKIPPED: 0 FAILED: 2
      1. Error: The getAssemblyStats() downloads assembly stats file and reads raw
                input: NCBI Genbank .. (@test-getAssemblyStats.R#34) 
      2. Error: The getCDS() interface to NCBI Genbank works properly.. (@test-getCDS.R#16) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# biotmle

Version: 1.0.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    package 'methods' is used but not declared
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

Version: 0.6.5

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
      > library(testthat)
      > library(dataRetrieval)
      > test_check("dataRetrieval")
      1. Failure: Unit value data returns correct types (@tests_userFriendly_fxns.R#38) 
      all(...) isn't true.
      
      
      Read 7 items
      testthat results ================================================================
      OK: 187 SKIPPED: 0 FAILED: 1
      1. Failure: Unit value data returns correct types (@tests_userFriendly_fxns.R#38) 
      
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
    Timeout was reached: Connection timed out after 10001 milliseconds
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
      ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      tests/testthat/test-tbl-cube.R:119:1: style: lines should not be more than 80 characters.
        expect_identical(as.table(nasa, measure = "ozone"), as.table(select(nasa, ozone)))
      ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      tests/testthat/test-ts.R:8:1: style: lines should not be more than 80 characters.
          "`.data` must be a data source, not a ts object, do you want `stats::filter()`?",
      ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      
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

# docxtools

Version: 0.1.1

## In both

*   checking examples ... ERROR
    ```
    ...
    > set.seed(20161221)
    > n <- 5
    > a <- sample(letters, n)
    > b <- sample(letters, n)
    > w <- runif(n, min =  -5, max =  50) * 1e+5
    > y <- runif(n, min = -25, max =  40) / 1e+3
    > z <- runif(n, min =  -5, max = 100)
    > x <- data.frame(z, b, y, a, w, stringsAsFactors = FALSE)
    > 
    > # format different objects
    > print(x)
              z b            y a         w
    1  6.501440 c  0.001051893 q 2846529.3
    2 28.374092 o  0.000347614 y 4874357.1
    3 -3.849624 i  0.004599897 g -111651.4
    4 44.500979 a -0.003045062 a 1314715.7
    5 92.411835 x -0.001069473 i  417385.0
    > format_engr(x)
    Error in FUN(X[[i]], ...) : object 'm_numeric_cols' not found
    Calls: format_engr ... <Anonymous> -> map_if -> map -> lapply -> FUN -> .Call
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 63-64 (numbers-in-engineering-format.Rmd) 
    Error: processing vignette 'numbers-in-engineering-format.Rmd' failed with diagnostics:
    object 'm_numeric_cols' not found
    Execution halted
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

## Newly fixed

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
        intersect, setdiff, setequal, union
    
    
    Attaching package: 'purrr'
    
    The following object is masked from 'package:plyr':
    
        compact
    
    
    Attaching package: 'scales'
    
    The following object is masked from 'package:purrr':
    
        discard
    
    Quitting from lines 155-172 (exploring-election-data.Rmd) 
    Error: processing vignette 'exploring-election-data.Rmd' failed with diagnostics:
    Value of SET_STRING_ELT() must be a 'CHARSXP' not a 'character'
    Execution halted
    ```

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

Version: 0.32

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.4Mb
      sub-directories of 1Mb or more:
        doc   5.8Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘curl’ ‘stringr’
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

# factorMerger

Version: 0.3.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘forcats’
      All declared Imports should be used.
    ```

# fastLink

Version: 0.1.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.4Mb
      sub-directories of 1Mb or more:
        data   4.8Mb
        libs   2.4Mb
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

# fingertipsR

Version: 0.1.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      
      2. Failure: the dimensions of deprivation decile function are as expected (@test-deprivation.R#14) 
      dim(dep_101) not equal to c(326, 3).
      1/2 mismatches
      [1] 0 - 326 == -326
      
      
      testthat results ================================================================
      OK: 29 SKIPPED: 0 FAILED: 2
      1. Failure: the dimensions of deprivation decile function are as expected (@test-deprivation.R#12) 
      2. Failure: the dimensions of deprivation decile function are as expected (@test-deprivation.R#14) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 135-150 (lifeExpectancy.Rmd) 
    Error: processing vignette 'lifeExpectancy.Rmd' failed with diagnostics:
    Faceting variables must have at least one value
    Execution halted
    ```

# flextable

Version: 0.2.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Loading required package: officer
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Quitting from lines 280-293 (format.Rmd) 
    Error: processing vignette 'format.Rmd' failed with diagnostics:
    file.exists(src) is not TRUE
    Execution halted
    ```

# flowWorkspace

Version: 3.24.4

## In both

*   checking whether package ‘flowWorkspace’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/home/muelleki/git/R/dplyr/revdep/checks/flowWorkspace/new/flowWorkspace.Rcheck/00install.out’ for details.
    ```

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

## Installation

### Devel

```
* installing *source* package ‘flowWorkspace’ ...
checking whether the C++ compiler works... yes
checking for C++ compiler default output file name... a.out
checking for suffix of executables... 
checking whether we are cross compiling... no
checking for suffix of object files... o
checking whether we are using the GNU C++ compiler... yes
checking whether g++ -std=gnu++11 accepts -g... yes
checking for gcc... gcc -std=gnu99
checking whether we are using the GNU C compiler... yes
checking whether gcc -std=gnu99 accepts -g... yes
checking for gcc -std=gnu99 option to accept ISO C89... none needed
configure: setting xml2 flags...
configure: No directory was specified for --with-xml2. Trying to find xml2 using other methods.
checking for xml2-config... /usr/bin/xml2-config
configure: setting protobuf flags...
configure: No directory was specified for --with-protobuf. Using bundled protocol buffer.
found  protobuf-2.6.0  header sources and tar archive;using what is there.
libtoolize: putting auxiliary files in '.'.
libtoolize: copying file './ltmain.sh'
libtoolize: putting macros in AC_CONFIG_MACRO_DIRS, 'm4'.
libtoolize: copying file 'm4/libtool.m4'
libtoolize: copying file 'm4/ltoptions.m4'
libtoolize: copying file 'm4/ltsugar.m4'
libtoolize: copying file 'm4/ltversion.m4'
libtoolize: copying file 'm4/lt~obsolete.m4'
configure.ac:45: installing './compile'
configure.ac:32: installing './missing'
/usr/share/automake-1.15/am/ltlibrary.am: warning: 'libprotobuf.la': linking libtool libraries using a non-POSIX
/usr/share/automake-1.15/am/ltlibrary.am: archiver requires 'AM_PROG_AR' in 'configure.ac'
src/Makefile.am:81:   while processing Libtool library 'libprotobuf.la'
src/Makefile.am: installing './depcomp'
building protobuf...
found  pb_build  ;using what is there.
checking whether to enable maintainer-specific portions of Makefiles... yes
checking build system type... x86_64-pc-linux-gnu
checking host system type... x86_64-pc-linux-gnu
checking target system type... x86_64-pc-linux-gnu
checking for a BSD-compatible install... /usr/bin/install -c
checking whether build environment is sane... yes
checking for a thread-safe mkdir -p... /bin/mkdir -p
checking for gawk... gawk
checking whether make sets $(MAKE)... yes
checking whether make supports nested variables... yes
checking for gcc... gcc -std=gnu99
checking whether the C compiler works... yes
checking for C compiler default output file name... a.out
checking for suffix of executables... 
checking whether we are cross compiling... no
checking for suffix of object files... o
checking whether we are using the GNU C compiler... yes
checking whether gcc -std=gnu99 accepts -g... yes
checking for gcc -std=gnu99 option to accept ISO C89... none needed
checking whether gcc -std=gnu99 understands -c and -o together... yes
checking for style of include used by make... GNU
checking dependency style of gcc -std=gnu99... gcc3
checking whether we are using the GNU C++ compiler... yes
checking whether g++ -std=gnu++11 accepts -g... yes
checking dependency style of g++ -std=gnu++11... gcc3
checking how to run the C++ preprocessor... g++ -std=gnu++11 -E
checking for grep that handles long lines and -e... /bin/grep
checking for egrep... /bin/grep -E
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
checking minix/config.h usability... no
checking minix/config.h presence... no
checking for minix/config.h... no
checking whether it is safe to define __EXTENSIONS__... yes
checking C++ compiler flags...... use user-supplied: -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g
checking whether __SUNPRO_CC is declared... no
checking how to print strings... printf
checking for a sed that does not truncate output... /bin/sed
checking for fgrep... /bin/grep -F
checking for ld used by gcc -std=gnu99... /usr/bin/ld
checking if the linker (/usr/bin/ld) is GNU ld... yes
checking for BSD- or MS-compatible name lister (nm)... /usr/bin/nm -B
checking the name lister (/usr/bin/nm -B) interface... BSD nm
checking whether ln -s works... yes
checking the maximum length of command line arguments... 1572864
checking how to convert x86_64-pc-linux-gnu file names to x86_64-pc-linux-gnu format... func_convert_file_noop
checking how to convert x86_64-pc-linux-gnu file names to toolchain format... func_convert_file_noop
checking for /usr/bin/ld option to reload object files... -r
checking for objdump... objdump
checking how to recognize dependent libraries... pass_all
checking for dlltool... no
checking how to associate runtime and link libraries... printf %s\n
checking for ar... ar
checking for archiver @FILE support... @
checking for strip... strip
checking for ranlib... ranlib
checking command to parse /usr/bin/nm -B output from gcc -std=gnu99 object... ok
checking for sysroot... no
checking for a working dd... /bin/dd
checking how to truncate binary pipes... /bin/dd bs=4096 count=1
checking for mt... mt
checking if mt is a manifest tool... no
checking for dlfcn.h... yes
checking for objdir... .libs
checking if gcc -std=gnu99 supports -fno-rtti -fno-exceptions... no
checking for gcc -std=gnu99 option to produce PIC... -fPIC -DPIC
checking if gcc -std=gnu99 PIC flag -fPIC -DPIC works... yes
checking if gcc -std=gnu99 static flag -static works... yes
checking if gcc -std=gnu99 supports -c -o file.o... yes
checking if gcc -std=gnu99 supports -c -o file.o... (cached) yes
checking whether the gcc -std=gnu99 linker (/usr/bin/ld -m elf_x86_64) supports shared libraries... yes
checking whether -lc should be explicitly linked in... no
checking dynamic linker characteristics... GNU/Linux ld.so
checking how to hardcode library paths into programs... immediate
checking whether stripping libraries is possible... yes
checking if libtool supports shared libraries... yes
checking whether to build shared libraries... yes
checking whether to build static libraries... no
checking how to run the C++ preprocessor... g++ -std=gnu++11 -E
checking for ld used by g++ -std=gnu++11... /usr/bin/ld -m elf_x86_64
checking if the linker (/usr/bin/ld -m elf_x86_64) is GNU ld... yes
checking whether the g++ -std=gnu++11 linker (/usr/bin/ld -m elf_x86_64) supports shared libraries... yes
checking for g++ -std=gnu++11 option to produce PIC... -fPIC -DPIC
checking if g++ -std=gnu++11 PIC flag -fPIC -DPIC works... yes
checking if g++ -std=gnu++11 static flag -static works... yes
checking if g++ -std=gnu++11 supports -c -o file.o... yes
checking if g++ -std=gnu++11 supports -c -o file.o... (cached) yes
checking whether the g++ -std=gnu++11 linker (/usr/bin/ld -m elf_x86_64) supports shared libraries... yes
checking dynamic linker characteristics... (cached) GNU/Linux ld.so
checking how to hardcode library paths into programs... immediate
checking for ANSI C header files... (cached) yes
checking fcntl.h usability... yes
checking fcntl.h presence... yes
checking for fcntl.h... yes
checking for inttypes.h... (cached) yes
checking limits.h usability... yes
checking limits.h presence... yes
checking for limits.h... yes
checking for stdlib.h... (cached) yes
checking for unistd.h... (cached) yes
checking for working memcmp... yes
checking for working strtod... yes
checking for ftruncate... yes
checking for memset... yes
checking for mkdir... yes
checking for strchr... yes
checking for strerror... yes
checking for strtol... yes
checking zlib version... ok (1.2.0.4 or later)
checking for library containing zlibVersion... -lz
checking for the pthreads library -lpthreads... no
checking whether pthreads work without any flags... no
checking whether pthreads work with -Kthread... no
checking whether pthreads work with -kthread... no
checking for the pthreads library -llthread... no
checking whether pthreads work with -pthread... yes
checking for joinable pthread attribute... PTHREAD_CREATE_JOINABLE
checking if more special flags are required for pthreads... no
checking whether to check for GCC pthread/shared inconsistencies... yes
checking whether -pthread is sufficient with -shared... yes
checking whether what we have so far is sufficient with -nostdlib... no
checking whether -lpthread saves the day... yes
checking the location of hash_map... <unordered_map>
checking that generated files are newer than configure... done
configure: creating ./config.status
config.status: creating Makefile
config.status: creating src/Makefile
config.status: creating protobuf.pc
config.status: creating protobuf-lite.pc
config.status: creating config.h
config.status: config.h is unchanged
config.status: executing depfiles commands
config.status: executing libtool commands
CDPATH="${ZSH_VERSION+.}:" && cd ../protobuf-2.6.0 && /bin/bash /home/muelleki/git/R/dplyr/revdep/checks/flowWorkspace/new/flowWorkspace.Rcheck/00_pkg_src/flowWorkspace/src/protobuf-2.6.0/missing aclocal-1.15 -I m4
 cd ../protobuf-2.6.0 && /bin/bash /home/muelleki/git/R/dplyr/revdep/checks/flowWorkspace/new/flowWorkspace.Rcheck/00_pkg_src/flowWorkspace/src/protobuf-2.6.0/missing automake-1.15 --foreign
CDPATH="${ZSH_VERSION+.}:" && cd ../protobuf-2.6.0 && /bin/bash /home/muelleki/git/R/dplyr/revdep/checks/flowWorkspace/new/flowWorkspace.Rcheck/00_pkg_src/flowWorkspace/src/protobuf-2.6.0/missing autoconf
/bin/bash ./config.status --recheck
running CONFIG_SHELL=/bin/bash /bin/bash ../protobuf-2.6.0/configure --enable-static=no CXX=g++ -std=gnu++11 CXXFLAGS=-g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g CC=gcc -std=gnu99 CFLAGS=-g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g --no-create --no-recursion
checking whether to enable maintainer-specific portions of Makefiles... yes
checking build system type... x86_64-pc-linux-gnu
checking host system type... x86_64-pc-linux-gnu
checking target system type... x86_64-pc-linux-gnu
checking for a BSD-compatible install... /usr/bin/install -c
checking whether build environment is sane... yes
checking for a thread-safe mkdir -p... /bin/mkdir -p
checking for gawk... gawk
checking whether make sets $(MAKE)... yes
checking whether make supports nested variables... yes
checking for gcc... gcc -std=gnu99
checking whether the C compiler works... yes
checking for C compiler default output file name... a.out
checking for suffix of executables... 
checking whether we are cross compiling... no
checking for suffix of object files... o
checking whether we are using the GNU C compiler... yes
checking whether gcc -std=gnu99 accepts -g... yes
checking for gcc -std=gnu99 option to accept ISO C89... none needed
checking whether gcc -std=gnu99 understands -c and -o together... yes
checking for style of include used by make... GNU
checking dependency style of gcc -std=gnu99... gcc3
checking whether we are using the GNU C++ compiler... yes
checking whether g++ -std=gnu++11 accepts -g... yes
checking dependency style of g++ -std=gnu++11... gcc3
checking how to run the C++ preprocessor... g++ -std=gnu++11 -E
checking for grep that handles long lines and -e... /bin/grep
checking for egrep... /bin/grep -E
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
checking minix/config.h usability... no
checking minix/config.h presence... no
checking for minix/config.h... no
checking whether it is safe to define __EXTENSIONS__... yes
checking C++ compiler flags...... use user-supplied: -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g
checking whether __SUNPRO_CC is declared... no
checking how to print strings... printf
checking for a sed that does not truncate output... /bin/sed
checking for fgrep... /bin/grep -F
checking for ld used by gcc -std=gnu99... /usr/bin/ld
checking if the linker (/usr/bin/ld) is GNU ld... yes
checking for BSD- or MS-compatible name lister (nm)... /usr/bin/nm -B
checking the name lister (/usr/bin/nm -B) interface... BSD nm
checking whether ln -s works... yes
checking the maximum length of command line arguments... 1572864
checking how to convert x86_64-pc-linux-gnu file names to x86_64-pc-linux-gnu format... func_convert_file_noop
checking how to convert x86_64-pc-linux-gnu file names to toolchain format... func_convert_file_noop
checking for /usr/bin/ld option to reload object files... -r
checking for objdump... objdump
checking how to recognize dependent libraries... pass_all
checking for dlltool... no
checking how to associate runtime and link libraries... printf %s\n
checking for ar... ar
checking for archiver @FILE support... @
checking for strip... strip
checking for ranlib... ranlib
checking command to parse /usr/bin/nm -B output from gcc -std=gnu99 object... ok
checking for sysroot... no
checking for a working dd... /bin/dd
checking how to truncate binary pipes... /bin/dd bs=4096 count=1
checking for mt... mt
checking if mt is a manifest tool... no
checking for dlfcn.h... yes
checking for objdir... .libs
checking if gcc -std=gnu99 supports -fno-rtti -fno-exceptions... no
checking for gcc -std=gnu99 option to produce PIC... -fPIC -DPIC
checking if gcc -std=gnu99 PIC flag -fPIC -DPIC works... yes
checking if gcc -std=gnu99 static flag -static works... yes
checking if gcc -std=gnu99 supports -c -o file.o... yes
checking if gcc -std=gnu99 supports -c -o file.o... (cached) yes
checking whether the gcc -std=gnu99 linker (/usr/bin/ld -m elf_x86_64) supports shared libraries... yes
checking whether -lc should be explicitly linked in... no
checking dynamic linker characteristics... GNU/Linux ld.so
checking how to hardcode library paths into programs... immediate
checking whether stripping libraries is possible... yes
checking if libtool supports shared libraries... yes
checking whether to build shared libraries... yes
checking whether to build static libraries... no
checking how to run the C++ preprocessor... g++ -std=gnu++11 -E
checking for ld used by g++ -std=gnu++11... /usr/bin/ld -m elf_x86_64
checking if the linker (/usr/bin/ld -m elf_x86_64) is GNU ld... yes
checking whether the g++ -std=gnu++11 linker (/usr/bin/ld -m elf_x86_64) supports shared libraries... yes
checking for g++ -std=gnu++11 option to produce PIC... -fPIC -DPIC
checking if g++ -std=gnu++11 PIC flag -fPIC -DPIC works... yes
checking if g++ -std=gnu++11 static flag -static works... yes
checking if g++ -std=gnu++11 supports -c -o file.o... yes
checking if g++ -std=gnu++11 supports -c -o file.o... (cached) yes
checking whether the g++ -std=gnu++11 linker (/usr/bin/ld -m elf_x86_64) supports shared libraries... yes
checking dynamic linker characteristics... (cached) GNU/Linux ld.so
checking how to hardcode library paths into programs... immediate
checking for ANSI C header files... (cached) yes
checking fcntl.h usability... yes
checking fcntl.h presence... yes
checking for fcntl.h... yes
checking for inttypes.h... (cached) yes
checking limits.h usability... yes
checking limits.h presence... yes
checking for limits.h... yes
checking for stdlib.h... (cached) yes
checking for unistd.h... (cached) yes
checking for working memcmp... yes
checking for working strtod... yes
checking for ftruncate... yes
checking for memset... yes
checking for mkdir... yes
checking for strchr... yes
checking for strerror... yes
checking for strtol... yes
checking zlib version... ok (1.2.0.4 or later)
checking for library containing zlibVersion... -lz
checking for the pthreads library -lpthreads... no
checking whether pthreads work without any flags... no
checking whether pthreads work with -Kthread... no
checking whether pthreads work with -kthread... no
checking for the pthreads library -llthread... no
checking whether pthreads work with -pthread... yes
checking for joinable pthread attribute... PTHREAD_CREATE_JOINABLE
checking if more special flags are required for pthreads... no
checking whether to check for GCC pthread/shared inconsistencies... yes
checking whether -pthread is sufficient with -shared... yes
checking whether what we have so far is sufficient with -nostdlib... no
checking whether -lpthread saves the day... yes
checking the location of hash_map... <unordered_map>
checking that generated files are newer than configure... done
configure: creating ./config.status
 /bin/bash ./config.status
config.status: creating Makefile
config.status: creating src/Makefile
config.status: creating protobuf.pc
config.status: creating protobuf-lite.pc
config.status: creating config.h
config.status: config.h is unchanged
config.status: executing depfiles commands
config.status: executing libtool commands
(CDPATH="${ZSH_VERSION+.}:" && cd ../protobuf-2.6.0 && /bin/bash /home/muelleki/git/R/dplyr/revdep/checks/flowWorkspace/new/flowWorkspace.Rcheck/00_pkg_src/flowWorkspace/src/protobuf-2.6.0/missing autoheader)
rm -f stamp-h1
touch ../protobuf-2.6.0/config.h.in
cd . && /bin/bash ./config.status config.h
config.status: creating config.h
config.status: config.h is unchanged
make  all-recursive
make[1]: Entering directory '/home/muelleki/git/R/dplyr/revdep/checks/flowWorkspace/new/flowWorkspace.Rcheck/00_pkg_src/flowWorkspace/src/pb_build'
Making all in .
make[2]: Entering directory '/home/muelleki/git/R/dplyr/revdep/checks/flowWorkspace/new/flowWorkspace.Rcheck/00_pkg_src/flowWorkspace/src/pb_build'
make[2]: Leaving directory '/home/muelleki/git/R/dplyr/revdep/checks/flowWorkspace/new/flowWorkspace.Rcheck/00_pkg_src/flowWorkspace/src/pb_build'
Making all in src
make[2]: Entering directory '/home/muelleki/git/R/dplyr/revdep/checks/flowWorkspace/new/flowWorkspace.Rcheck/00_pkg_src/flowWorkspace/src/pb_build/src'
depbase=`echo google/protobuf/stubs/atomicops_internals_x86_gcc.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/stubs/atomicops_internals_x86_gcc.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/stubs/atomicops_internals_x86_gcc.lo ../../protobuf-2.6.0/src/google/protobuf/stubs/atomicops_internals_x86_gcc.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/stubs/atomicops_internals_x86_gcc.lo -MD -MP -MF google/protobuf/stubs/.deps/atomicops_internals_x86_gcc.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/stubs/atomicops_internals_x86_gcc.cc  -fPIC -DPIC -o google/protobuf/stubs/.libs/atomicops_internals_x86_gcc.o
depbase=`echo google/protobuf/stubs/atomicops_internals_x86_msvc.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/stubs/atomicops_internals_x86_msvc.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/stubs/atomicops_internals_x86_msvc.lo ../../protobuf-2.6.0/src/google/protobuf/stubs/atomicops_internals_x86_msvc.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/stubs/atomicops_internals_x86_msvc.lo -MD -MP -MF google/protobuf/stubs/.deps/atomicops_internals_x86_msvc.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/stubs/atomicops_internals_x86_msvc.cc  -fPIC -DPIC -o google/protobuf/stubs/.libs/atomicops_internals_x86_msvc.o
depbase=`echo google/protobuf/stubs/common.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/stubs/common.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/stubs/common.lo ../../protobuf-2.6.0/src/google/protobuf/stubs/common.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/stubs/common.lo -MD -MP -MF google/protobuf/stubs/.deps/common.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/stubs/common.cc  -fPIC -DPIC -o google/protobuf/stubs/.libs/common.o
depbase=`echo google/protobuf/stubs/once.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/stubs/once.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/stubs/once.lo ../../protobuf-2.6.0/src/google/protobuf/stubs/once.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/stubs/once.lo -MD -MP -MF google/protobuf/stubs/.deps/once.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/stubs/once.cc  -fPIC -DPIC -o google/protobuf/stubs/.libs/once.o
depbase=`echo google/protobuf/stubs/stringprintf.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/stubs/stringprintf.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/stubs/stringprintf.lo ../../protobuf-2.6.0/src/google/protobuf/stubs/stringprintf.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/stubs/stringprintf.lo -MD -MP -MF google/protobuf/stubs/.deps/stringprintf.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/stubs/stringprintf.cc  -fPIC -DPIC -o google/protobuf/stubs/.libs/stringprintf.o
../../protobuf-2.6.0/src/google/protobuf/stubs/stringprintf.cc: In function 'std::__cxx11::string google::protobuf::StringPrintfVector(const char*, const std::vector<std::__cxx11::basic_string<char> >&)':
../../protobuf-2.6.0/src/google/protobuf/stubs/stringprintf.cc:164:97: warning: typedef 'arg_count_mismatch' locally defined but not used [-Wunused-local-typedefs]
depbase=`echo google/protobuf/extension_set.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/extension_set.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/extension_set.lo ../../protobuf-2.6.0/src/google/protobuf/extension_set.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/extension_set.lo -MD -MP -MF google/protobuf/.deps/extension_set.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/extension_set.cc  -fPIC -DPIC -o google/protobuf/.libs/extension_set.o
In file included from ../../protobuf-2.6.0/src/google/protobuf/extension_set.cc:43:0:
../../protobuf-2.6.0/src/google/protobuf/stubs/map_util.h: In function 'void google::protobuf::InsertOrDie(Collection*, const typename Collection::value_type::first_type&, const typename Collection::value_type::second_type&)':
../../protobuf-2.6.0/src/google/protobuf/stubs/map_util.h:356:43: warning: typedef 'value_type' locally defined but not used [-Wunused-local-typedefs]
   typedef typename Collection::value_type value_type;
                                           ^
In file included from ../../protobuf-2.6.0/src/google/protobuf/extension_set.cc:43:0:
../../protobuf-2.6.0/src/google/protobuf/stubs/map_util.h: In function 'void google::protobuf::InsertOrDieNoPrint(Collection*, const typename Collection::value_type::first_type&, const typename Collection::value_type::second_type&)':
../../protobuf-2.6.0/src/google/protobuf/stubs/map_util.h:367:43: warning: typedef 'value_type' locally defined but not used [-Wunused-local-typedefs]
   typedef typename Collection::value_type value_type;
                                           ^
depbase=`echo google/protobuf/generated_message_util.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/generated_message_util.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/generated_message_util.lo ../../protobuf-2.6.0/src/google/protobuf/generated_message_util.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/generated_message_util.lo -MD -MP -MF google/protobuf/.deps/generated_message_util.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/generated_message_util.cc  -fPIC -DPIC -o google/protobuf/.libs/generated_message_util.o
depbase=`echo google/protobuf/message_lite.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/message_lite.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/message_lite.lo ../../protobuf-2.6.0/src/google/protobuf/message_lite.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/message_lite.lo -MD -MP -MF google/protobuf/.deps/message_lite.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/message_lite.cc  -fPIC -DPIC -o google/protobuf/.libs/message_lite.o
depbase=`echo google/protobuf/repeated_field.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/repeated_field.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/repeated_field.lo ../../protobuf-2.6.0/src/google/protobuf/repeated_field.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/repeated_field.lo -MD -MP -MF google/protobuf/.deps/repeated_field.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/repeated_field.cc  -fPIC -DPIC -o google/protobuf/.libs/repeated_field.o
depbase=`echo google/protobuf/wire_format_lite.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/wire_format_lite.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/wire_format_lite.lo ../../protobuf-2.6.0/src/google/protobuf/wire_format_lite.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/wire_format_lite.lo -MD -MP -MF google/protobuf/.deps/wire_format_lite.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/wire_format_lite.cc  -fPIC -DPIC -o google/protobuf/.libs/wire_format_lite.o
depbase=`echo google/protobuf/io/coded_stream.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/io/coded_stream.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/io/coded_stream.lo ../../protobuf-2.6.0/src/google/protobuf/io/coded_stream.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/io/coded_stream.lo -MD -MP -MF google/protobuf/io/.deps/coded_stream.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/io/coded_stream.cc  -fPIC -DPIC -o google/protobuf/io/.libs/coded_stream.o
depbase=`echo google/protobuf/io/zero_copy_stream.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/io/zero_copy_stream.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/io/zero_copy_stream.lo ../../protobuf-2.6.0/src/google/protobuf/io/zero_copy_stream.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/io/zero_copy_stream.lo -MD -MP -MF google/protobuf/io/.deps/zero_copy_stream.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/io/zero_copy_stream.cc  -fPIC -DPIC -o google/protobuf/io/.libs/zero_copy_stream.o
depbase=`echo google/protobuf/io/zero_copy_stream_impl_lite.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/io/zero_copy_stream_impl_lite.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/io/zero_copy_stream_impl_lite.lo ../../protobuf-2.6.0/src/google/protobuf/io/zero_copy_stream_impl_lite.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/io/zero_copy_stream_impl_lite.lo -MD -MP -MF google/protobuf/io/.deps/zero_copy_stream_impl_lite.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/io/zero_copy_stream_impl_lite.cc  -fPIC -DPIC -o google/protobuf/io/.libs/zero_copy_stream_impl_lite.o
depbase=`echo google/protobuf/stubs/strutil.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/stubs/strutil.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/stubs/strutil.lo ../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/stubs/strutil.lo -MD -MP -MF google/protobuf/stubs/.deps/strutil.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.cc  -fPIC -DPIC -o google/protobuf/stubs/.libs/strutil.o
In file included from ../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.cc:33:0:
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h: In function 'google::protobuf::int64 google::protobuf::strto64(const char*, char**, int)':
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h:340:99: warning: typedef 'sizeof_int64_is_not_sizeof_long_long' locally defined but not used [-Wunused-local-typedefs]
In file included from ../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.cc:33:0:
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h: In function 'google::protobuf::uint64 google::protobuf::strtou64(const char*, char**, int)':
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h:346:109: warning: typedef 'sizeof_uint64_is_not_sizeof_long_long' locally defined but not used [-Wunused-local-typedefs]
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.cc: In function 'char* google::protobuf::DoubleToBuffer(double, char*)':
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.cc:1172:72: warning: typedef 'DBL_DIG_is_too_big' locally defined but not used [-Wunused-local-typedefs]
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.cc: In function 'char* google::protobuf::FloatToBuffer(float, char*)':
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.cc:1227:71: warning: typedef 'FLT_DIG_is_too_big' locally defined but not used [-Wunused-local-typedefs]
depbase=`echo google/protobuf/stubs/substitute.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/stubs/substitute.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/stubs/substitute.lo ../../protobuf-2.6.0/src/google/protobuf/stubs/substitute.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/stubs/substitute.lo -MD -MP -MF google/protobuf/stubs/.deps/substitute.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/stubs/substitute.cc  -fPIC -DPIC -o google/protobuf/stubs/.libs/substitute.o
In file included from ../../protobuf-2.6.0/src/google/protobuf/stubs/substitute.h:36:0,
                 from ../../protobuf-2.6.0/src/google/protobuf/stubs/substitute.cc:33:
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h: In function 'google::protobuf::int64 google::protobuf::strto64(const char*, char**, int)':
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h:340:99: warning: typedef 'sizeof_int64_is_not_sizeof_long_long' locally defined but not used [-Wunused-local-typedefs]
In file included from ../../protobuf-2.6.0/src/google/protobuf/stubs/substitute.h:36:0,
                 from ../../protobuf-2.6.0/src/google/protobuf/stubs/substitute.cc:33:
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h: In function 'google::protobuf::uint64 google::protobuf::strtou64(const char*, char**, int)':
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h:346:109: warning: typedef 'sizeof_uint64_is_not_sizeof_long_long' locally defined but not used [-Wunused-local-typedefs]
depbase=`echo google/protobuf/stubs/structurally_valid.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/stubs/structurally_valid.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/stubs/structurally_valid.lo ../../protobuf-2.6.0/src/google/protobuf/stubs/structurally_valid.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/stubs/structurally_valid.lo -MD -MP -MF google/protobuf/stubs/.deps/structurally_valid.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/stubs/structurally_valid.cc  -fPIC -DPIC -o google/protobuf/stubs/.libs/structurally_valid.o
depbase=`echo google/protobuf/descriptor.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/descriptor.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/descriptor.lo ../../protobuf-2.6.0/src/google/protobuf/descriptor.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/descriptor.lo -MD -MP -MF google/protobuf/.deps/descriptor.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/descriptor.cc  -fPIC -DPIC -o google/protobuf/.libs/descriptor.o
In file included from ../../protobuf-2.6.0/src/google/protobuf/descriptor.cc:57:0:
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h: In function 'google::protobuf::int64 google::protobuf::strto64(const char*, char**, int)':
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h:340:99: warning: typedef 'sizeof_int64_is_not_sizeof_long_long' locally defined but not used [-Wunused-local-typedefs]
In file included from ../../protobuf-2.6.0/src/google/protobuf/descriptor.cc:57:0:
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h: In function 'google::protobuf::uint64 google::protobuf::strtou64(const char*, char**, int)':
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h:346:109: warning: typedef 'sizeof_uint64_is_not_sizeof_long_long' locally defined but not used [-Wunused-local-typedefs]
In file included from ../../protobuf-2.6.0/src/google/protobuf/descriptor.cc:59:0:
../../protobuf-2.6.0/src/google/protobuf/stubs/map_util.h: In function 'void google::protobuf::InsertOrDie(Collection*, const typename Collection::value_type::first_type&, const typename Collection::value_type::second_type&)':
../../protobuf-2.6.0/src/google/protobuf/stubs/map_util.h:356:43: warning: typedef 'value_type' locally defined but not used [-Wunused-local-typedefs]
   typedef typename Collection::value_type value_type;
                                           ^
In file included from ../../protobuf-2.6.0/src/google/protobuf/descriptor.cc:59:0:
../../protobuf-2.6.0/src/google/protobuf/stubs/map_util.h: In function 'void google::protobuf::InsertOrDieNoPrint(Collection*, const typename Collection::value_type::first_type&, const typename Collection::value_type::second_type&)':
../../protobuf-2.6.0/src/google/protobuf/stubs/map_util.h:367:43: warning: typedef 'value_type' locally defined but not used [-Wunused-local-typedefs]
   typedef typename Collection::value_type value_type;
                                           ^
depbase=`echo google/protobuf/descriptor.pb.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/descriptor.pb.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/descriptor.pb.lo ../../protobuf-2.6.0/src/google/protobuf/descriptor.pb.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/descriptor.pb.lo -MD -MP -MF google/protobuf/.deps/descriptor.pb.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/descriptor.pb.cc  -fPIC -DPIC -o google/protobuf/.libs/descriptor.pb.o
depbase=`echo google/protobuf/descriptor_database.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/descriptor_database.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/descriptor_database.lo ../../protobuf-2.6.0/src/google/protobuf/descriptor_database.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/descriptor_database.lo -MD -MP -MF google/protobuf/.deps/descriptor_database.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/descriptor_database.cc  -fPIC -DPIC -o google/protobuf/.libs/descriptor_database.o
In file included from ../../protobuf-2.6.0/src/google/protobuf/descriptor_database.cc:41:0:
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h: In function 'google::protobuf::int64 google::protobuf::strto64(const char*, char**, int)':
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h:340:99: warning: typedef 'sizeof_int64_is_not_sizeof_long_long' locally defined but not used [-Wunused-local-typedefs]
In file included from ../../protobuf-2.6.0/src/google/protobuf/descriptor_database.cc:41:0:
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h: In function 'google::protobuf::uint64 google::protobuf::strtou64(const char*, char**, int)':
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h:346:109: warning: typedef 'sizeof_uint64_is_not_sizeof_long_long' locally defined but not used [-Wunused-local-typedefs]
In file included from ../../protobuf-2.6.0/src/google/protobuf/descriptor_database.cc:43:0:
../../protobuf-2.6.0/src/google/protobuf/stubs/map_util.h: In function 'void google::protobuf::InsertOrDie(Collection*, const typename Collection::value_type::first_type&, const typename Collection::value_type::second_type&)':
../../protobuf-2.6.0/src/google/protobuf/stubs/map_util.h:356:43: warning: typedef 'value_type' locally defined but not used [-Wunused-local-typedefs]
   typedef typename Collection::value_type value_type;
                                           ^
In file included from ../../protobuf-2.6.0/src/google/protobuf/descriptor_database.cc:43:0:
../../protobuf-2.6.0/src/google/protobuf/stubs/map_util.h: In function 'void google::protobuf::InsertOrDieNoPrint(Collection*, const typename Collection::value_type::first_type&, const typename Collection::value_type::second_type&)':
../../protobuf-2.6.0/src/google/protobuf/stubs/map_util.h:367:43: warning: typedef 'value_type' locally defined but not used [-Wunused-local-typedefs]
   typedef typename Collection::value_type value_type;
                                           ^
depbase=`echo google/protobuf/dynamic_message.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/dynamic_message.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/dynamic_message.lo ../../protobuf-2.6.0/src/google/protobuf/dynamic_message.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/dynamic_message.lo -MD -MP -MF google/protobuf/.deps/dynamic_message.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/dynamic_message.cc  -fPIC -DPIC -o google/protobuf/.libs/dynamic_message.o
depbase=`echo google/protobuf/extension_set_heavy.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/extension_set_heavy.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/extension_set_heavy.lo ../../protobuf-2.6.0/src/google/protobuf/extension_set_heavy.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/extension_set_heavy.lo -MD -MP -MF google/protobuf/.deps/extension_set_heavy.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/extension_set_heavy.cc  -fPIC -DPIC -o google/protobuf/.libs/extension_set_heavy.o
depbase=`echo google/protobuf/generated_message_reflection.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/generated_message_reflection.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/generated_message_reflection.lo ../../protobuf-2.6.0/src/google/protobuf/generated_message_reflection.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/generated_message_reflection.lo -MD -MP -MF google/protobuf/.deps/generated_message_reflection.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/generated_message_reflection.cc  -fPIC -DPIC -o google/protobuf/.libs/generated_message_reflection.o
../../protobuf-2.6.0/src/google/protobuf/generated_message_reflection.cc: In member function 'void google::protobuf::internal::GeneratedMessageReflection::SwapOneofField(google::protobuf::Message*, google::protobuf::Message*, const google::protobuf::OneofDescriptor*) const':
../../protobuf-2.6.0/src/google/protobuf/generated_message_reflection.cc:506:89: warning: 'field1' may be used uninitialized in this function [-Wmaybe-uninitialized]
../../protobuf-2.6.0/src/google/protobuf/generated_message_reflection.cc:516:60: warning: 'temp_message' may be used uninitialized in this function [-Wmaybe-uninitialized]
         SetAllocatedMessage(message2, temp_message, field1);
                                                            ^
depbase=`echo google/protobuf/message.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/message.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/message.lo ../../protobuf-2.6.0/src/google/protobuf/message.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/message.lo -MD -MP -MF google/protobuf/.deps/message.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/message.cc  -fPIC -DPIC -o google/protobuf/.libs/message.o
In file included from ../../protobuf-2.6.0/src/google/protobuf/message.cc:50:0:
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h: In function 'google::protobuf::int64 google::protobuf::strto64(const char*, char**, int)':
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h:340:99: warning: typedef 'sizeof_int64_is_not_sizeof_long_long' locally defined but not used [-Wunused-local-typedefs]
In file included from ../../protobuf-2.6.0/src/google/protobuf/message.cc:50:0:
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h: In function 'google::protobuf::uint64 google::protobuf::strtou64(const char*, char**, int)':
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h:346:109: warning: typedef 'sizeof_uint64_is_not_sizeof_long_long' locally defined but not used [-Wunused-local-typedefs]
In file included from ../../protobuf-2.6.0/src/google/protobuf/message.cc:51:0:
../../protobuf-2.6.0/src/google/protobuf/stubs/map_util.h: In function 'void google::protobuf::InsertOrDie(Collection*, const typename Collection::value_type::first_type&, const typename Collection::value_type::second_type&)':
../../protobuf-2.6.0/src/google/protobuf/stubs/map_util.h:356:43: warning: typedef 'value_type' locally defined but not used [-Wunused-local-typedefs]
   typedef typename Collection::value_type value_type;
                                           ^
In file included from ../../protobuf-2.6.0/src/google/protobuf/message.cc:51:0:
../../protobuf-2.6.0/src/google/protobuf/stubs/map_util.h: In function 'void google::protobuf::InsertOrDieNoPrint(Collection*, const typename Collection::value_type::first_type&, const typename Collection::value_type::second_type&)':
../../protobuf-2.6.0/src/google/protobuf/stubs/map_util.h:367:43: warning: typedef 'value_type' locally defined but not used [-Wunused-local-typedefs]
   typedef typename Collection::value_type value_type;
                                           ^
depbase=`echo google/protobuf/reflection_ops.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/reflection_ops.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/reflection_ops.lo ../../protobuf-2.6.0/src/google/protobuf/reflection_ops.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/reflection_ops.lo -MD -MP -MF google/protobuf/.deps/reflection_ops.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/reflection_ops.cc  -fPIC -DPIC -o google/protobuf/.libs/reflection_ops.o
In file included from ../../protobuf-2.6.0/src/google/protobuf/reflection_ops.cc:42:0:
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h: In function 'google::protobuf::int64 google::protobuf::strto64(const char*, char**, int)':
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h:340:99: warning: typedef 'sizeof_int64_is_not_sizeof_long_long' locally defined but not used [-Wunused-local-typedefs]
In file included from ../../protobuf-2.6.0/src/google/protobuf/reflection_ops.cc:42:0:
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h: In function 'google::protobuf::uint64 google::protobuf::strtou64(const char*, char**, int)':
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h:346:109: warning: typedef 'sizeof_uint64_is_not_sizeof_long_long' locally defined but not used [-Wunused-local-typedefs]
depbase=`echo google/protobuf/service.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/service.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/service.lo ../../protobuf-2.6.0/src/google/protobuf/service.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/service.lo -MD -MP -MF google/protobuf/.deps/service.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/service.cc  -fPIC -DPIC -o google/protobuf/.libs/service.o
depbase=`echo google/protobuf/text_format.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/text_format.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/text_format.lo ../../protobuf-2.6.0/src/google/protobuf/text_format.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/text_format.lo -MD -MP -MF google/protobuf/.deps/text_format.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/text_format.cc  -fPIC -DPIC -o google/protobuf/.libs/text_format.o
In file included from ../../protobuf-2.6.0/src/google/protobuf/text_format.cc:53:0:
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h: In function 'google::protobuf::int64 google::protobuf::strto64(const char*, char**, int)':
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h:340:99: warning: typedef 'sizeof_int64_is_not_sizeof_long_long' locally defined but not used [-Wunused-local-typedefs]
In file included from ../../protobuf-2.6.0/src/google/protobuf/text_format.cc:53:0:
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h: In function 'google::protobuf::uint64 google::protobuf::strtou64(const char*, char**, int)':
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h:346:109: warning: typedef 'sizeof_uint64_is_not_sizeof_long_long' locally defined but not used [-Wunused-local-typedefs]
In file included from ../../protobuf-2.6.0/src/google/protobuf/text_format.cc:54:0:
../../protobuf-2.6.0/src/google/protobuf/stubs/map_util.h: In function 'void google::protobuf::InsertOrDie(Collection*, const typename Collection::value_type::first_type&, const typename Collection::value_type::second_type&)':
../../protobuf-2.6.0/src/google/protobuf/stubs/map_util.h:356:43: warning: typedef 'value_type' locally defined but not used [-Wunused-local-typedefs]
   typedef typename Collection::value_type value_type;
                                           ^
In file included from ../../protobuf-2.6.0/src/google/protobuf/text_format.cc:54:0:
../../protobuf-2.6.0/src/google/protobuf/stubs/map_util.h: In function 'void google::protobuf::InsertOrDieNoPrint(Collection*, const typename Collection::value_type::first_type&, const typename Collection::value_type::second_type&)':
../../protobuf-2.6.0/src/google/protobuf/stubs/map_util.h:367:43: warning: typedef 'value_type' locally defined but not used [-Wunused-local-typedefs]
   typedef typename Collection::value_type value_type;
                                           ^
depbase=`echo google/protobuf/unknown_field_set.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/unknown_field_set.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/unknown_field_set.lo ../../protobuf-2.6.0/src/google/protobuf/unknown_field_set.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/unknown_field_set.lo -MD -MP -MF google/protobuf/.deps/unknown_field_set.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/unknown_field_set.cc  -fPIC -DPIC -o google/protobuf/.libs/unknown_field_set.o
depbase=`echo google/protobuf/wire_format.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/wire_format.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/wire_format.lo ../../protobuf-2.6.0/src/google/protobuf/wire_format.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/wire_format.lo -MD -MP -MF google/protobuf/.deps/wire_format.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/wire_format.cc  -fPIC -DPIC -o google/protobuf/.libs/wire_format.o
depbase=`echo google/protobuf/io/gzip_stream.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/io/gzip_stream.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/io/gzip_stream.lo ../../protobuf-2.6.0/src/google/protobuf/io/gzip_stream.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/io/gzip_stream.lo -MD -MP -MF google/protobuf/io/.deps/gzip_stream.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/io/gzip_stream.cc  -fPIC -DPIC -o google/protobuf/io/.libs/gzip_stream.o
depbase=`echo google/protobuf/io/printer.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/io/printer.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/io/printer.lo ../../protobuf-2.6.0/src/google/protobuf/io/printer.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/io/printer.lo -MD -MP -MF google/protobuf/io/.deps/printer.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/io/printer.cc  -fPIC -DPIC -o google/protobuf/io/.libs/printer.o
depbase=`echo google/protobuf/io/strtod.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/io/strtod.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/io/strtod.lo ../../protobuf-2.6.0/src/google/protobuf/io/strtod.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/io/strtod.lo -MD -MP -MF google/protobuf/io/.deps/strtod.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/io/strtod.cc  -fPIC -DPIC -o google/protobuf/io/.libs/strtod.o
depbase=`echo google/protobuf/io/tokenizer.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/io/tokenizer.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/io/tokenizer.lo ../../protobuf-2.6.0/src/google/protobuf/io/tokenizer.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/io/tokenizer.lo -MD -MP -MF google/protobuf/io/.deps/tokenizer.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/io/tokenizer.cc  -fPIC -DPIC -o google/protobuf/io/.libs/tokenizer.o
In file included from ../../protobuf-2.6.0/src/google/protobuf/io/tokenizer.cc:96:0:
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h: In function 'google::protobuf::int64 google::protobuf::strto64(const char*, char**, int)':
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h:340:99: warning: typedef 'sizeof_int64_is_not_sizeof_long_long' locally defined but not used [-Wunused-local-typedefs]
In file included from ../../protobuf-2.6.0/src/google/protobuf/io/tokenizer.cc:96:0:
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h: In function 'google::protobuf::uint64 google::protobuf::strtou64(const char*, char**, int)':
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h:346:109: warning: typedef 'sizeof_uint64_is_not_sizeof_long_long' locally defined but not used [-Wunused-local-typedefs]
depbase=`echo google/protobuf/io/zero_copy_stream_impl.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/io/zero_copy_stream_impl.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/io/zero_copy_stream_impl.lo ../../protobuf-2.6.0/src/google/protobuf/io/zero_copy_stream_impl.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/io/zero_copy_stream_impl.lo -MD -MP -MF google/protobuf/io/.deps/zero_copy_stream_impl.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/io/zero_copy_stream_impl.cc  -fPIC -DPIC -o google/protobuf/io/.libs/zero_copy_stream_impl.o
/bin/bash ../libtool  --tag=CXX   --mode=link g++ -std=gnu++11 -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -version-info 9:0:0 -export-dynamic -no-undefined  -o libprotobuf.la -rpath /usr/local/lib google/protobuf/stubs/atomicops_internals_x86_gcc.lo google/protobuf/stubs/atomicops_internals_x86_msvc.lo google/protobuf/stubs/common.lo google/protobuf/stubs/once.lo google/protobuf/stubs/stringprintf.lo google/protobuf/extension_set.lo google/protobuf/generated_message_util.lo google/protobuf/message_lite.lo google/protobuf/repeated_field.lo google/protobuf/wire_format_lite.lo google/protobuf/io/coded_stream.lo google/protobuf/io/zero_copy_stream.lo google/protobuf/io/zero_copy_stream_impl_lite.lo google/protobuf/stubs/strutil.lo google/protobuf/stubs/substitute.lo google/protobuf/stubs/structurally_valid.lo google/protobuf/descriptor.lo google/protobuf/descriptor.pb.lo google/protobuf/descriptor_database.lo google/protobuf/dynamic_message.lo google/protobuf/extension_set_heavy.lo google/protobuf/generated_message_reflection.lo google/protobuf/message.lo google/protobuf/reflection_ops.lo google/protobuf/service.lo google/protobuf/text_format.lo google/protobuf/unknown_field_set.lo google/protobuf/wire_format.lo google/protobuf/io/gzip_stream.lo google/protobuf/io/printer.lo google/protobuf/io/strtod.lo google/protobuf/io/tokenizer.lo google/protobuf/io/zero_copy_stream_impl.lo -lpthread -lz 
libtool: link: rm -fr  .libs/libprotobuf.la .libs/libprotobuf.lai .libs/libprotobuf.so .libs/libprotobuf.so.9 .libs/libprotobuf.so.9.0.0
libtool: link: g++ -std=gnu++11  -fPIC -DPIC -shared -nostdlib /usr/lib/gcc/x86_64-linux-gnu/5/../../../x86_64-linux-gnu/crti.o /usr/lib/gcc/x86_64-linux-gnu/5/crtbeginS.o  google/protobuf/stubs/.libs/atomicops_internals_x86_gcc.o google/protobuf/stubs/.libs/atomicops_internals_x86_msvc.o google/protobuf/stubs/.libs/common.o google/protobuf/stubs/.libs/once.o google/protobuf/stubs/.libs/stringprintf.o google/protobuf/.libs/extension_set.o google/protobuf/.libs/generated_message_util.o google/protobuf/.libs/message_lite.o google/protobuf/.libs/repeated_field.o google/protobuf/.libs/wire_format_lite.o google/protobuf/io/.libs/coded_stream.o google/protobuf/io/.libs/zero_copy_stream.o google/protobuf/io/.libs/zero_copy_stream_impl_lite.o google/protobuf/stubs/.libs/strutil.o google/protobuf/stubs/.libs/substitute.o google/protobuf/stubs/.libs/structurally_valid.o google/protobuf/.libs/descriptor.o google/protobuf/.libs/descriptor.pb.o google/protobuf/.libs/descriptor_database.o google/protobuf/.libs/dynamic_message.o google/protobuf/.libs/extension_set_heavy.o google/protobuf/.libs/generated_message_reflection.o google/protobuf/.libs/message.o google/protobuf/.libs/reflection_ops.o google/protobuf/.libs/service.o google/protobuf/.libs/text_format.o google/protobuf/.libs/unknown_field_set.o google/protobuf/.libs/wire_format.o google/protobuf/io/.libs/gzip_stream.o google/protobuf/io/.libs/printer.o google/protobuf/io/.libs/strtod.o google/protobuf/io/.libs/tokenizer.o google/protobuf/io/.libs/zero_copy_stream_impl.o   -lpthread -lz -L/usr/lib/gcc/x86_64-linux-gnu/5 -L/usr/lib/gcc/x86_64-linux-gnu/5/../../../x86_64-linux-gnu -L/usr/lib/gcc/x86_64-linux-gnu/5/../../../../lib -L/lib/x86_64-linux-gnu -L/lib/../lib -L/usr/lib/x86_64-linux-gnu -L/usr/lib/../lib -L/usr/lib/gcc/x86_64-linux-gnu/5/../../.. -lstdc++ -lm -lc -lgcc_s /usr/lib/gcc/x86_64-linux-gnu/5/crtendS.o /usr/lib/gcc/x86_64-linux-gnu/5/../../../x86_64-linux-gnu/crtn.o  -pthread -g -O2 -fstack-protector-strong -g   -pthread -Wl,-soname -Wl,libprotobuf.so.9 -o .libs/libprotobuf.so.9.0.0
libtool: link: (cd ".libs" && rm -f "libprotobuf.so.9" && ln -s "libprotobuf.so.9.0.0" "libprotobuf.so.9")
libtool: link: (cd ".libs" && rm -f "libprotobuf.so" && ln -s "libprotobuf.so.9.0.0" "libprotobuf.so")
libtool: link: ( cd ".libs" && rm -f "libprotobuf.la" && ln -s "../libprotobuf.la" "libprotobuf.la" )
make[2]: Leaving directory '/home/muelleki/git/R/dplyr/revdep/checks/flowWorkspace/new/flowWorkspace.Rcheck/00_pkg_src/flowWorkspace/src/pb_build/src'
make[1]: Leaving directory '/home/muelleki/git/R/dplyr/revdep/checks/flowWorkspace/new/flowWorkspace.Rcheck/00_pkg_src/flowWorkspace/src/pb_build'
configure: Using the following compilation and linking flags for flowWorkspace
configure:    PKG_CPPFLAGS=-I/usr/include/libxml2 -Iprotobuf-2.6.0/src
configure:    PB_INCLUDE=protobuf-2.6.0/src
configure:    PKG_LIBS=-lxml2 
configure: creating ./config.status
config.status: creating src/Makevars
** libs
mkdir -p "/home/muelleki/git/R/dplyr/revdep/checks/flowWorkspace/new/flowWorkspace.Rcheck/flowWorkspace/include/flowWorkspace"
cp include/* /home/muelleki/git/R/dplyr/revdep/checks/flowWorkspace/new/flowWorkspace.Rcheck/flowWorkspace/include/flowWorkspace
cp -r protobuf-2.6.0/src/google /home/muelleki/git/R/dplyr/revdep/checks/flowWorkspace/new/flowWorkspace.Rcheck/flowWorkspace/include
cp GatingSet.pb.h /home/muelleki/git/R/dplyr/revdep/checks/flowWorkspace/new/flowWorkspace.Rcheck/flowWorkspace/include #GatingSet.pb.h is autogenerated by protoc and thus kept at the same folder as GatingSet.proto
g++ -std=gnu++11 -I/usr/share/R/include -DNDEBUG -DROUT -I/usr/include/libxml2 -Iprotobuf-2.6.0/src -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/new/Rcpp/include" -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/new/BH/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c delimitedMessage.cpp -o delimitedMessage.o
g++ -std=gnu++11 -I/usr/share/R/include -DNDEBUG -DROUT -I/usr/include/libxml2 -Iprotobuf-2.6.0/src -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/new/Rcpp/include" -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/new/BH/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c global.cpp -o global.o
g++ -std=gnu++11 -I/usr/share/R/include -DNDEBUG -DROUT -I/usr/include/libxml2 -Iprotobuf-2.6.0/src -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/new/Rcpp/include" -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/new/BH/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c calibrationTable.cpp -o calibrationTable.o
g++ -std=gnu++11 -I/usr/share/R/include -DNDEBUG -DROUT -I/usr/include/libxml2 -Iprotobuf-2.6.0/src -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/new/Rcpp/include" -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/new/BH/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c ellipse2points.cpp -o ellipse2points.o
g++ -std=gnu++11 -I/usr/share/R/include -DNDEBUG -DROUT -I/usr/include/libxml2 -Iprotobuf-2.6.0/src -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/new/Rcpp/include" -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/new/BH/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c compensation.cpp -o compensation.o
g++ -std=gnu++11 -I/usr/share/R/include -DNDEBUG -DROUT -I/usr/include/libxml2 -Iprotobuf-2.6.0/src -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/new/Rcpp/include" -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/new/BH/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c GatingSet.pb.cc -o GatingSet.pb.o
g++ -std=gnu++11 -I/usr/share/R/include -DNDEBUG -DROUT -I/usr/include/libxml2 -Iprotobuf-2.6.0/src -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/new/Rcpp/include" -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/new/BH/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c gate.cpp -o gate.o
g++ -std=gnu++11 -I/usr/share/R/include -DNDEBUG -DROUT -I/usr/include/libxml2 -Iprotobuf-2.6.0/src -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/new/Rcpp/include" -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/new/BH/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c POPINDICES.cpp -o POPINDICES.o
g++ -std=gnu++11 -I/usr/share/R/include -DNDEBUG -DROUT -I/usr/include/libxml2 -Iprotobuf-2.6.0/src -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/new/Rcpp/include" -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/new/BH/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c flowData.cpp -o flowData.o
mkdir -p "/home/muelleki/git/R/dplyr/revdep/checks/flowWorkspace/new/flowWorkspace.Rcheck/flowWorkspace/lib"
ar rs "/home/muelleki/git/R/dplyr/revdep/checks/flowWorkspace/new/flowWorkspace.Rcheck/flowWorkspace/lib/libprotobuf.a" pb_build/src/google/protobuf/.libs/*.o pb_build/src/google/protobuf/stubs/.libs/*.o pb_build/src/google/protobuf/io/.libs/*.o
ar: creating /home/muelleki/git/R/dplyr/revdep/checks/flowWorkspace/new/flowWorkspace.Rcheck/flowWorkspace/lib/libprotobuf.a
g++ -std=gnu++11 -I/usr/share/R/include -DNDEBUG -DROUT -I/usr/include/libxml2 -Iprotobuf-2.6.0/src -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/new/Rcpp/include" -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/new/BH/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c nodeProperties.cpp -o nodeProperties.o
g++ -std=gnu++11 -I/usr/share/R/include -DNDEBUG -DROUT -I/usr/include/libxml2 -Iprotobuf-2.6.0/src -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/new/Rcpp/include" -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/new/BH/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c wsNode.cpp -o wsNode.o
g++ -std=gnu++11 -I/usr/share/R/include -DNDEBUG -DROUT -I/usr/include/libxml2 -Iprotobuf-2.6.0/src -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/new/Rcpp/include" -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/new/BH/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c R_GatingSet.cpp -o R_GatingSet.o
g++ -std=gnu++11 -I/usr/share/R/include -DNDEBUG -DROUT -I/usr/include/libxml2 -Iprotobuf-2.6.0/src -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/new/Rcpp/include" -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/new/BH/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c spline.cpp -o spline.o
g++ -std=gnu++11 -I/usr/share/R/include -DNDEBUG -DROUT -I/usr/include/libxml2 -Iprotobuf-2.6.0/src -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/new/Rcpp/include" -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/new/BH/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c getSingleCellExpression.cpp -o getSingleCellExpression.o
g++ -std=gnu++11 -I/usr/share/R/include -DNDEBUG -DROUT -I/usr/include/libxml2 -Iprotobuf-2.6.0/src -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/new/Rcpp/include" -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/new/BH/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c workspace.cpp -o workspace.o
g++ -std=gnu++11 -I/usr/share/R/include -DNDEBUG -DROUT -I/usr/include/libxml2 -Iprotobuf-2.6.0/src -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/new/Rcpp/include" -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/new/BH/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c GatingSet.cpp -o GatingSet.o
g++ -std=gnu++11 -I/usr/share/R/include -DNDEBUG -DROUT -I/usr/include/libxml2 -Iprotobuf-2.6.0/src -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/new/Rcpp/include" -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/new/BH/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c transformation.cpp -o transformation.o
g++ -std=gnu++11 -I/usr/share/R/include -DNDEBUG -DROUT -I/usr/include/libxml2 -Iprotobuf-2.6.0/src -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/new/Rcpp/include" -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/new/BH/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c R_API.cpp -o R_API.o
g++ -std=gnu++11 -I/usr/share/R/include -DNDEBUG -DROUT -I/usr/include/libxml2 -Iprotobuf-2.6.0/src -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/new/Rcpp/include" -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/new/BH/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++11 -I/usr/share/R/include -DNDEBUG -DROUT -I/usr/include/libxml2 -Iprotobuf-2.6.0/src -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/new/Rcpp/include" -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/new/BH/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c flowJoWorkspace.cpp -o flowJoWorkspace.o
g++ -std=gnu++11 -I/usr/share/R/include -DNDEBUG -DROUT -I/usr/include/libxml2 -Iprotobuf-2.6.0/src -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/new/Rcpp/include" -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/new/BH/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c winFlowJoWorkspace.cpp -o winFlowJoWorkspace.o
g++ -std=gnu++11 -I/usr/share/R/include -DNDEBUG -DROUT -I/usr/include/libxml2 -Iprotobuf-2.6.0/src -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/new/Rcpp/include" -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/new/BH/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c macFlowJoWorkspace.cpp -o macFlowJoWorkspace.o
g++ -std=gnu++11 -I/usr/share/R/include -DNDEBUG -DROUT -I/usr/include/libxml2 -Iprotobuf-2.6.0/src -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/new/Rcpp/include" -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/new/BH/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c R_GatingHierarchy.cpp -o R_GatingHierarchy.o
g++ -std=gnu++11 -I/usr/share/R/include -DNDEBUG -DROUT -I/usr/include/libxml2 -Iprotobuf-2.6.0/src -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/new/Rcpp/include" -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/new/BH/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c GatingHierarchy.cpp -o GatingHierarchy.o
mkdir -p "/home/muelleki/git/R/dplyr/revdep/checks/flowWorkspace/new/flowWorkspace.Rcheck/flowWorkspace/lib"
ar rs "/home/muelleki/git/R/dplyr/revdep/checks/flowWorkspace/new/flowWorkspace.Rcheck/flowWorkspace/lib/libflowWorkspace.a" GatingHierarchy.o GatingSet.o GatingSet.pb.o POPINDICES.o R_API.o R_GatingHierarchy.o R_GatingSet.o RcppExports.o calibrationTable.o compensation.o delimitedMessage.o ellipse2points.o flowData.o flowJoWorkspace.o gate.o getSingleCellExpression.o global.o macFlowJoWorkspace.o nodeProperties.o spline.o transformation.o winFlowJoWorkspace.o workspace.o wsNode.o
ar: creating /home/muelleki/git/R/dplyr/revdep/checks/flowWorkspace/new/flowWorkspace.Rcheck/flowWorkspace/lib/libflowWorkspace.a
g++ -std=gnu++11 -shared -L/usr/lib/R/lib -Wl,-Bsymbolic-functions -Wl,-z,relro -o flowWorkspace.so GatingHierarchy.o GatingSet.o GatingSet.pb.o POPINDICES.o R_API.o R_GatingHierarchy.o R_GatingSet.o RcppExports.o calibrationTable.o compensation.o delimitedMessage.o ellipse2points.o flowData.o flowJoWorkspace.o gate.o getSingleCellExpression.o global.o macFlowJoWorkspace.o nodeProperties.o spline.o transformation.o winFlowJoWorkspace.o workspace.o wsNode.o /home/muelleki/git/R/dplyr/revdep/checks/flowWorkspace/new/flowWorkspace.Rcheck/flowWorkspace/lib/libprotobuf.a /home/muelleki/git/R/dplyr/revdep/library/flowWorkspace/flowCore/lib/libboost_regex.a -lxml2 -L/usr/lib/R/lib -lR
installing to /home/muelleki/git/R/dplyr/revdep/checks/flowWorkspace/new/flowWorkspace.Rcheck/flowWorkspace/libs
** R
** inst
** preparing package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded
Error: package or namespace load failed for ‘flowWorkspace’ in dyn.load(file, DLLpath = DLLpath, ...):
 unable to load shared object '/home/muelleki/git/R/dplyr/revdep/checks/flowWorkspace/new/flowWorkspace.Rcheck/flowWorkspace/libs/flowWorkspace.so':
  /home/muelleki/git/R/dplyr/revdep/checks/flowWorkspace/new/flowWorkspace.Rcheck/flowWorkspace/libs/flowWorkspace.so: undefined symbol: _ZN5boost16re_detail_10650012perl_matcherIN9__gnu_cxx17__normal_iteratorIPKcNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEEESaINS_9sub_matchISC_EEENS_12regex_traitsIcNS_16cpp_regex_traitsIcEEEEE14construct_initERKNS_11basic_regexIcSJ_EENS_15regex_constants12_match_flagsE
Error: loading failed
Execution halted
ERROR: loading failed
* removing ‘/home/muelleki/git/R/dplyr/revdep/checks/flowWorkspace/new/flowWorkspace.Rcheck/flowWorkspace’

```
### CRAN

```
* installing *source* package ‘flowWorkspace’ ...
checking whether the C++ compiler works... yes
checking for C++ compiler default output file name... a.out
checking for suffix of executables... 
checking whether we are cross compiling... no
checking for suffix of object files... o
checking whether we are using the GNU C++ compiler... yes
checking whether g++ -std=gnu++11 accepts -g... yes
checking for gcc... gcc -std=gnu99
checking whether we are using the GNU C compiler... yes
checking whether gcc -std=gnu99 accepts -g... yes
checking for gcc -std=gnu99 option to accept ISO C89... none needed
configure: setting xml2 flags...
configure: No directory was specified for --with-xml2. Trying to find xml2 using other methods.
checking for xml2-config... /usr/bin/xml2-config
configure: setting protobuf flags...
configure: No directory was specified for --with-protobuf. Using bundled protocol buffer.
found  protobuf-2.6.0  header sources and tar archive;using what is there.
libtoolize: putting auxiliary files in '.'.
libtoolize: copying file './ltmain.sh'
libtoolize: putting macros in AC_CONFIG_MACRO_DIRS, 'm4'.
libtoolize: copying file 'm4/libtool.m4'
libtoolize: copying file 'm4/ltoptions.m4'
libtoolize: copying file 'm4/ltsugar.m4'
libtoolize: copying file 'm4/ltversion.m4'
libtoolize: copying file 'm4/lt~obsolete.m4'
configure.ac:45: installing './compile'
configure.ac:32: installing './missing'
/usr/share/automake-1.15/am/ltlibrary.am: warning: 'libprotobuf.la': linking libtool libraries using a non-POSIX
/usr/share/automake-1.15/am/ltlibrary.am: archiver requires 'AM_PROG_AR' in 'configure.ac'
src/Makefile.am:81:   while processing Libtool library 'libprotobuf.la'
src/Makefile.am: installing './depcomp'
building protobuf...
found  pb_build  ;using what is there.
checking whether to enable maintainer-specific portions of Makefiles... yes
checking build system type... x86_64-pc-linux-gnu
checking host system type... x86_64-pc-linux-gnu
checking target system type... x86_64-pc-linux-gnu
checking for a BSD-compatible install... /usr/bin/install -c
checking whether build environment is sane... yes
checking for a thread-safe mkdir -p... /bin/mkdir -p
checking for gawk... gawk
checking whether make sets $(MAKE)... yes
checking whether make supports nested variables... yes
checking for gcc... gcc -std=gnu99
checking whether the C compiler works... yes
checking for C compiler default output file name... a.out
checking for suffix of executables... 
checking whether we are cross compiling... no
checking for suffix of object files... o
checking whether we are using the GNU C compiler... yes
checking whether gcc -std=gnu99 accepts -g... yes
checking for gcc -std=gnu99 option to accept ISO C89... none needed
checking whether gcc -std=gnu99 understands -c and -o together... yes
checking for style of include used by make... GNU
checking dependency style of gcc -std=gnu99... gcc3
checking whether we are using the GNU C++ compiler... yes
checking whether g++ -std=gnu++11 accepts -g... yes
checking dependency style of g++ -std=gnu++11... gcc3
checking how to run the C++ preprocessor... g++ -std=gnu++11 -E
checking for grep that handles long lines and -e... /bin/grep
checking for egrep... /bin/grep -E
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
checking minix/config.h usability... no
checking minix/config.h presence... no
checking for minix/config.h... no
checking whether it is safe to define __EXTENSIONS__... yes
checking C++ compiler flags...... use user-supplied: -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g
checking whether __SUNPRO_CC is declared... no
checking how to print strings... printf
checking for a sed that does not truncate output... /bin/sed
checking for fgrep... /bin/grep -F
checking for ld used by gcc -std=gnu99... /usr/bin/ld
checking if the linker (/usr/bin/ld) is GNU ld... yes
checking for BSD- or MS-compatible name lister (nm)... /usr/bin/nm -B
checking the name lister (/usr/bin/nm -B) interface... BSD nm
checking whether ln -s works... yes
checking the maximum length of command line arguments... 1572864
checking how to convert x86_64-pc-linux-gnu file names to x86_64-pc-linux-gnu format... func_convert_file_noop
checking how to convert x86_64-pc-linux-gnu file names to toolchain format... func_convert_file_noop
checking for /usr/bin/ld option to reload object files... -r
checking for objdump... objdump
checking how to recognize dependent libraries... pass_all
checking for dlltool... no
checking how to associate runtime and link libraries... printf %s\n
checking for ar... ar
checking for archiver @FILE support... @
checking for strip... strip
checking for ranlib... ranlib
checking command to parse /usr/bin/nm -B output from gcc -std=gnu99 object... ok
checking for sysroot... no
checking for a working dd... /bin/dd
checking how to truncate binary pipes... /bin/dd bs=4096 count=1
checking for mt... mt
checking if mt is a manifest tool... no
checking for dlfcn.h... yes
checking for objdir... .libs
checking if gcc -std=gnu99 supports -fno-rtti -fno-exceptions... no
checking for gcc -std=gnu99 option to produce PIC... -fPIC -DPIC
checking if gcc -std=gnu99 PIC flag -fPIC -DPIC works... yes
checking if gcc -std=gnu99 static flag -static works... yes
checking if gcc -std=gnu99 supports -c -o file.o... yes
checking if gcc -std=gnu99 supports -c -o file.o... (cached) yes
checking whether the gcc -std=gnu99 linker (/usr/bin/ld -m elf_x86_64) supports shared libraries... yes
checking whether -lc should be explicitly linked in... no
checking dynamic linker characteristics... GNU/Linux ld.so
checking how to hardcode library paths into programs... immediate
checking whether stripping libraries is possible... yes
checking if libtool supports shared libraries... yes
checking whether to build shared libraries... yes
checking whether to build static libraries... no
checking how to run the C++ preprocessor... g++ -std=gnu++11 -E
checking for ld used by g++ -std=gnu++11... /usr/bin/ld -m elf_x86_64
checking if the linker (/usr/bin/ld -m elf_x86_64) is GNU ld... yes
checking whether the g++ -std=gnu++11 linker (/usr/bin/ld -m elf_x86_64) supports shared libraries... yes
checking for g++ -std=gnu++11 option to produce PIC... -fPIC -DPIC
checking if g++ -std=gnu++11 PIC flag -fPIC -DPIC works... yes
checking if g++ -std=gnu++11 static flag -static works... yes
checking if g++ -std=gnu++11 supports -c -o file.o... yes
checking if g++ -std=gnu++11 supports -c -o file.o... (cached) yes
checking whether the g++ -std=gnu++11 linker (/usr/bin/ld -m elf_x86_64) supports shared libraries... yes
checking dynamic linker characteristics... (cached) GNU/Linux ld.so
checking how to hardcode library paths into programs... immediate
checking for ANSI C header files... (cached) yes
checking fcntl.h usability... yes
checking fcntl.h presence... yes
checking for fcntl.h... yes
checking for inttypes.h... (cached) yes
checking limits.h usability... yes
checking limits.h presence... yes
checking for limits.h... yes
checking for stdlib.h... (cached) yes
checking for unistd.h... (cached) yes
checking for working memcmp... yes
checking for working strtod... yes
checking for ftruncate... yes
checking for memset... yes
checking for mkdir... yes
checking for strchr... yes
checking for strerror... yes
checking for strtol... yes
checking zlib version... ok (1.2.0.4 or later)
checking for library containing zlibVersion... -lz
checking for the pthreads library -lpthreads... no
checking whether pthreads work without any flags... no
checking whether pthreads work with -Kthread... no
checking whether pthreads work with -kthread... no
checking for the pthreads library -llthread... no
checking whether pthreads work with -pthread... yes
checking for joinable pthread attribute... PTHREAD_CREATE_JOINABLE
checking if more special flags are required for pthreads... no
checking whether to check for GCC pthread/shared inconsistencies... yes
checking whether -pthread is sufficient with -shared... yes
checking whether what we have so far is sufficient with -nostdlib... no
checking whether -lpthread saves the day... yes
checking the location of hash_map... <unordered_map>
checking that generated files are newer than configure... done
configure: creating ./config.status
config.status: creating Makefile
config.status: creating src/Makefile
config.status: creating protobuf.pc
config.status: creating protobuf-lite.pc
config.status: creating config.h
config.status: config.h is unchanged
config.status: executing depfiles commands
config.status: executing libtool commands
CDPATH="${ZSH_VERSION+.}:" && cd ../protobuf-2.6.0 && /bin/bash /home/muelleki/git/R/dplyr/revdep/checks/flowWorkspace/old/flowWorkspace.Rcheck/00_pkg_src/flowWorkspace/src/protobuf-2.6.0/missing aclocal-1.15 -I m4
 cd ../protobuf-2.6.0 && /bin/bash /home/muelleki/git/R/dplyr/revdep/checks/flowWorkspace/old/flowWorkspace.Rcheck/00_pkg_src/flowWorkspace/src/protobuf-2.6.0/missing automake-1.15 --foreign
CDPATH="${ZSH_VERSION+.}:" && cd ../protobuf-2.6.0 && /bin/bash /home/muelleki/git/R/dplyr/revdep/checks/flowWorkspace/old/flowWorkspace.Rcheck/00_pkg_src/flowWorkspace/src/protobuf-2.6.0/missing autoconf
/bin/bash ./config.status --recheck
running CONFIG_SHELL=/bin/bash /bin/bash ../protobuf-2.6.0/configure --enable-static=no CXX=g++ -std=gnu++11 CXXFLAGS=-g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g CC=gcc -std=gnu99 CFLAGS=-g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g --no-create --no-recursion
checking whether to enable maintainer-specific portions of Makefiles... yes
checking build system type... x86_64-pc-linux-gnu
checking host system type... x86_64-pc-linux-gnu
checking target system type... x86_64-pc-linux-gnu
checking for a BSD-compatible install... /usr/bin/install -c
checking whether build environment is sane... yes
checking for a thread-safe mkdir -p... /bin/mkdir -p
checking for gawk... gawk
checking whether make sets $(MAKE)... yes
checking whether make supports nested variables... yes
checking for gcc... gcc -std=gnu99
checking whether the C compiler works... yes
checking for C compiler default output file name... a.out
checking for suffix of executables... 
checking whether we are cross compiling... no
checking for suffix of object files... o
checking whether we are using the GNU C compiler... yes
checking whether gcc -std=gnu99 accepts -g... yes
checking for gcc -std=gnu99 option to accept ISO C89... none needed
checking whether gcc -std=gnu99 understands -c and -o together... yes
checking for style of include used by make... GNU
checking dependency style of gcc -std=gnu99... gcc3
checking whether we are using the GNU C++ compiler... yes
checking whether g++ -std=gnu++11 accepts -g... yes
checking dependency style of g++ -std=gnu++11... gcc3
checking how to run the C++ preprocessor... g++ -std=gnu++11 -E
checking for grep that handles long lines and -e... /bin/grep
checking for egrep... /bin/grep -E
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
checking minix/config.h usability... no
checking minix/config.h presence... no
checking for minix/config.h... no
checking whether it is safe to define __EXTENSIONS__... yes
checking C++ compiler flags...... use user-supplied: -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g
checking whether __SUNPRO_CC is declared... no
checking how to print strings... printf
checking for a sed that does not truncate output... /bin/sed
checking for fgrep... /bin/grep -F
checking for ld used by gcc -std=gnu99... /usr/bin/ld
checking if the linker (/usr/bin/ld) is GNU ld... yes
checking for BSD- or MS-compatible name lister (nm)... /usr/bin/nm -B
checking the name lister (/usr/bin/nm -B) interface... BSD nm
checking whether ln -s works... yes
checking the maximum length of command line arguments... 1572864
checking how to convert x86_64-pc-linux-gnu file names to x86_64-pc-linux-gnu format... func_convert_file_noop
checking how to convert x86_64-pc-linux-gnu file names to toolchain format... func_convert_file_noop
checking for /usr/bin/ld option to reload object files... -r
checking for objdump... objdump
checking how to recognize dependent libraries... pass_all
checking for dlltool... no
checking how to associate runtime and link libraries... printf %s\n
checking for ar... ar
checking for archiver @FILE support... @
checking for strip... strip
checking for ranlib... ranlib
checking command to parse /usr/bin/nm -B output from gcc -std=gnu99 object... ok
checking for sysroot... no
checking for a working dd... /bin/dd
checking how to truncate binary pipes... /bin/dd bs=4096 count=1
checking for mt... mt
checking if mt is a manifest tool... no
checking for dlfcn.h... yes
checking for objdir... .libs
checking if gcc -std=gnu99 supports -fno-rtti -fno-exceptions... no
checking for gcc -std=gnu99 option to produce PIC... -fPIC -DPIC
checking if gcc -std=gnu99 PIC flag -fPIC -DPIC works... yes
checking if gcc -std=gnu99 static flag -static works... yes
checking if gcc -std=gnu99 supports -c -o file.o... yes
checking if gcc -std=gnu99 supports -c -o file.o... (cached) yes
checking whether the gcc -std=gnu99 linker (/usr/bin/ld -m elf_x86_64) supports shared libraries... yes
checking whether -lc should be explicitly linked in... no
checking dynamic linker characteristics... GNU/Linux ld.so
checking how to hardcode library paths into programs... immediate
checking whether stripping libraries is possible... yes
checking if libtool supports shared libraries... yes
checking whether to build shared libraries... yes
checking whether to build static libraries... no
checking how to run the C++ preprocessor... g++ -std=gnu++11 -E
checking for ld used by g++ -std=gnu++11... /usr/bin/ld -m elf_x86_64
checking if the linker (/usr/bin/ld -m elf_x86_64) is GNU ld... yes
checking whether the g++ -std=gnu++11 linker (/usr/bin/ld -m elf_x86_64) supports shared libraries... yes
checking for g++ -std=gnu++11 option to produce PIC... -fPIC -DPIC
checking if g++ -std=gnu++11 PIC flag -fPIC -DPIC works... yes
checking if g++ -std=gnu++11 static flag -static works... yes
checking if g++ -std=gnu++11 supports -c -o file.o... yes
checking if g++ -std=gnu++11 supports -c -o file.o... (cached) yes
checking whether the g++ -std=gnu++11 linker (/usr/bin/ld -m elf_x86_64) supports shared libraries... yes
checking dynamic linker characteristics... (cached) GNU/Linux ld.so
checking how to hardcode library paths into programs... immediate
checking for ANSI C header files... (cached) yes
checking fcntl.h usability... yes
checking fcntl.h presence... yes
checking for fcntl.h... yes
checking for inttypes.h... (cached) yes
checking limits.h usability... yes
checking limits.h presence... yes
checking for limits.h... yes
checking for stdlib.h... (cached) yes
checking for unistd.h... (cached) yes
checking for working memcmp... yes
checking for working strtod... yes
checking for ftruncate... yes
checking for memset... yes
checking for mkdir... yes
checking for strchr... yes
checking for strerror... yes
checking for strtol... yes
checking zlib version... ok (1.2.0.4 or later)
checking for library containing zlibVersion... -lz
checking for the pthreads library -lpthreads... no
checking whether pthreads work without any flags... no
checking whether pthreads work with -Kthread... no
checking whether pthreads work with -kthread... no
checking for the pthreads library -llthread... no
checking whether pthreads work with -pthread... yes
checking for joinable pthread attribute... PTHREAD_CREATE_JOINABLE
checking if more special flags are required for pthreads... no
checking whether to check for GCC pthread/shared inconsistencies... yes
checking whether -pthread is sufficient with -shared... yes
checking whether what we have so far is sufficient with -nostdlib... no
checking whether -lpthread saves the day... yes
checking the location of hash_map... <unordered_map>
checking that generated files are newer than configure... done
configure: creating ./config.status
 /bin/bash ./config.status
config.status: creating Makefile
config.status: creating src/Makefile
config.status: creating protobuf.pc
config.status: creating protobuf-lite.pc
config.status: creating config.h
config.status: config.h is unchanged
config.status: executing depfiles commands
config.status: executing libtool commands
(CDPATH="${ZSH_VERSION+.}:" && cd ../protobuf-2.6.0 && /bin/bash /home/muelleki/git/R/dplyr/revdep/checks/flowWorkspace/old/flowWorkspace.Rcheck/00_pkg_src/flowWorkspace/src/protobuf-2.6.0/missing autoheader)
rm -f stamp-h1
touch ../protobuf-2.6.0/config.h.in
cd . && /bin/bash ./config.status config.h
config.status: creating config.h
config.status: config.h is unchanged
make  all-recursive
make[1]: Entering directory '/home/muelleki/git/R/dplyr/revdep/checks/flowWorkspace/old/flowWorkspace.Rcheck/00_pkg_src/flowWorkspace/src/pb_build'
Making all in .
make[2]: Entering directory '/home/muelleki/git/R/dplyr/revdep/checks/flowWorkspace/old/flowWorkspace.Rcheck/00_pkg_src/flowWorkspace/src/pb_build'
make[2]: Leaving directory '/home/muelleki/git/R/dplyr/revdep/checks/flowWorkspace/old/flowWorkspace.Rcheck/00_pkg_src/flowWorkspace/src/pb_build'
Making all in src
make[2]: Entering directory '/home/muelleki/git/R/dplyr/revdep/checks/flowWorkspace/old/flowWorkspace.Rcheck/00_pkg_src/flowWorkspace/src/pb_build/src'
depbase=`echo google/protobuf/stubs/atomicops_internals_x86_gcc.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/stubs/atomicops_internals_x86_gcc.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/stubs/atomicops_internals_x86_gcc.lo ../../protobuf-2.6.0/src/google/protobuf/stubs/atomicops_internals_x86_gcc.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/stubs/atomicops_internals_x86_gcc.lo -MD -MP -MF google/protobuf/stubs/.deps/atomicops_internals_x86_gcc.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/stubs/atomicops_internals_x86_gcc.cc  -fPIC -DPIC -o google/protobuf/stubs/.libs/atomicops_internals_x86_gcc.o
depbase=`echo google/protobuf/stubs/atomicops_internals_x86_msvc.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/stubs/atomicops_internals_x86_msvc.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/stubs/atomicops_internals_x86_msvc.lo ../../protobuf-2.6.0/src/google/protobuf/stubs/atomicops_internals_x86_msvc.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/stubs/atomicops_internals_x86_msvc.lo -MD -MP -MF google/protobuf/stubs/.deps/atomicops_internals_x86_msvc.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/stubs/atomicops_internals_x86_msvc.cc  -fPIC -DPIC -o google/protobuf/stubs/.libs/atomicops_internals_x86_msvc.o
depbase=`echo google/protobuf/stubs/common.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/stubs/common.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/stubs/common.lo ../../protobuf-2.6.0/src/google/protobuf/stubs/common.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/stubs/common.lo -MD -MP -MF google/protobuf/stubs/.deps/common.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/stubs/common.cc  -fPIC -DPIC -o google/protobuf/stubs/.libs/common.o
depbase=`echo google/protobuf/stubs/once.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/stubs/once.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/stubs/once.lo ../../protobuf-2.6.0/src/google/protobuf/stubs/once.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/stubs/once.lo -MD -MP -MF google/protobuf/stubs/.deps/once.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/stubs/once.cc  -fPIC -DPIC -o google/protobuf/stubs/.libs/once.o
depbase=`echo google/protobuf/stubs/stringprintf.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/stubs/stringprintf.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/stubs/stringprintf.lo ../../protobuf-2.6.0/src/google/protobuf/stubs/stringprintf.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/stubs/stringprintf.lo -MD -MP -MF google/protobuf/stubs/.deps/stringprintf.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/stubs/stringprintf.cc  -fPIC -DPIC -o google/protobuf/stubs/.libs/stringprintf.o
../../protobuf-2.6.0/src/google/protobuf/stubs/stringprintf.cc: In function 'std::__cxx11::string google::protobuf::StringPrintfVector(const char*, const std::vector<std::__cxx11::basic_string<char> >&)':
../../protobuf-2.6.0/src/google/protobuf/stubs/stringprintf.cc:164:97: warning: typedef 'arg_count_mismatch' locally defined but not used [-Wunused-local-typedefs]
depbase=`echo google/protobuf/extension_set.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/extension_set.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/extension_set.lo ../../protobuf-2.6.0/src/google/protobuf/extension_set.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/extension_set.lo -MD -MP -MF google/protobuf/.deps/extension_set.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/extension_set.cc  -fPIC -DPIC -o google/protobuf/.libs/extension_set.o
In file included from ../../protobuf-2.6.0/src/google/protobuf/extension_set.cc:43:0:
../../protobuf-2.6.0/src/google/protobuf/stubs/map_util.h: In function 'void google::protobuf::InsertOrDie(Collection*, const typename Collection::value_type::first_type&, const typename Collection::value_type::second_type&)':
../../protobuf-2.6.0/src/google/protobuf/stubs/map_util.h:356:43: warning: typedef 'value_type' locally defined but not used [-Wunused-local-typedefs]
   typedef typename Collection::value_type value_type;
                                           ^
In file included from ../../protobuf-2.6.0/src/google/protobuf/extension_set.cc:43:0:
../../protobuf-2.6.0/src/google/protobuf/stubs/map_util.h: In function 'void google::protobuf::InsertOrDieNoPrint(Collection*, const typename Collection::value_type::first_type&, const typename Collection::value_type::second_type&)':
../../protobuf-2.6.0/src/google/protobuf/stubs/map_util.h:367:43: warning: typedef 'value_type' locally defined but not used [-Wunused-local-typedefs]
   typedef typename Collection::value_type value_type;
                                           ^
depbase=`echo google/protobuf/generated_message_util.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/generated_message_util.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/generated_message_util.lo ../../protobuf-2.6.0/src/google/protobuf/generated_message_util.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/generated_message_util.lo -MD -MP -MF google/protobuf/.deps/generated_message_util.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/generated_message_util.cc  -fPIC -DPIC -o google/protobuf/.libs/generated_message_util.o
depbase=`echo google/protobuf/message_lite.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/message_lite.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/message_lite.lo ../../protobuf-2.6.0/src/google/protobuf/message_lite.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/message_lite.lo -MD -MP -MF google/protobuf/.deps/message_lite.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/message_lite.cc  -fPIC -DPIC -o google/protobuf/.libs/message_lite.o
depbase=`echo google/protobuf/repeated_field.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/repeated_field.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/repeated_field.lo ../../protobuf-2.6.0/src/google/protobuf/repeated_field.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/repeated_field.lo -MD -MP -MF google/protobuf/.deps/repeated_field.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/repeated_field.cc  -fPIC -DPIC -o google/protobuf/.libs/repeated_field.o
depbase=`echo google/protobuf/wire_format_lite.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/wire_format_lite.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/wire_format_lite.lo ../../protobuf-2.6.0/src/google/protobuf/wire_format_lite.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/wire_format_lite.lo -MD -MP -MF google/protobuf/.deps/wire_format_lite.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/wire_format_lite.cc  -fPIC -DPIC -o google/protobuf/.libs/wire_format_lite.o
depbase=`echo google/protobuf/io/coded_stream.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/io/coded_stream.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/io/coded_stream.lo ../../protobuf-2.6.0/src/google/protobuf/io/coded_stream.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/io/coded_stream.lo -MD -MP -MF google/protobuf/io/.deps/coded_stream.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/io/coded_stream.cc  -fPIC -DPIC -o google/protobuf/io/.libs/coded_stream.o
depbase=`echo google/protobuf/io/zero_copy_stream.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/io/zero_copy_stream.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/io/zero_copy_stream.lo ../../protobuf-2.6.0/src/google/protobuf/io/zero_copy_stream.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/io/zero_copy_stream.lo -MD -MP -MF google/protobuf/io/.deps/zero_copy_stream.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/io/zero_copy_stream.cc  -fPIC -DPIC -o google/protobuf/io/.libs/zero_copy_stream.o
depbase=`echo google/protobuf/io/zero_copy_stream_impl_lite.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/io/zero_copy_stream_impl_lite.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/io/zero_copy_stream_impl_lite.lo ../../protobuf-2.6.0/src/google/protobuf/io/zero_copy_stream_impl_lite.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/io/zero_copy_stream_impl_lite.lo -MD -MP -MF google/protobuf/io/.deps/zero_copy_stream_impl_lite.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/io/zero_copy_stream_impl_lite.cc  -fPIC -DPIC -o google/protobuf/io/.libs/zero_copy_stream_impl_lite.o
depbase=`echo google/protobuf/stubs/strutil.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/stubs/strutil.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/stubs/strutil.lo ../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/stubs/strutil.lo -MD -MP -MF google/protobuf/stubs/.deps/strutil.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.cc  -fPIC -DPIC -o google/protobuf/stubs/.libs/strutil.o
In file included from ../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.cc:33:0:
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h: In function 'google::protobuf::int64 google::protobuf::strto64(const char*, char**, int)':
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h:340:99: warning: typedef 'sizeof_int64_is_not_sizeof_long_long' locally defined but not used [-Wunused-local-typedefs]
In file included from ../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.cc:33:0:
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h: In function 'google::protobuf::uint64 google::protobuf::strtou64(const char*, char**, int)':
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h:346:109: warning: typedef 'sizeof_uint64_is_not_sizeof_long_long' locally defined but not used [-Wunused-local-typedefs]
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.cc: In function 'char* google::protobuf::DoubleToBuffer(double, char*)':
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.cc:1172:72: warning: typedef 'DBL_DIG_is_too_big' locally defined but not used [-Wunused-local-typedefs]
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.cc: In function 'char* google::protobuf::FloatToBuffer(float, char*)':
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.cc:1227:71: warning: typedef 'FLT_DIG_is_too_big' locally defined but not used [-Wunused-local-typedefs]
depbase=`echo google/protobuf/stubs/substitute.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/stubs/substitute.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/stubs/substitute.lo ../../protobuf-2.6.0/src/google/protobuf/stubs/substitute.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/stubs/substitute.lo -MD -MP -MF google/protobuf/stubs/.deps/substitute.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/stubs/substitute.cc  -fPIC -DPIC -o google/protobuf/stubs/.libs/substitute.o
In file included from ../../protobuf-2.6.0/src/google/protobuf/stubs/substitute.h:36:0,
                 from ../../protobuf-2.6.0/src/google/protobuf/stubs/substitute.cc:33:
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h: In function 'google::protobuf::int64 google::protobuf::strto64(const char*, char**, int)':
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h:340:99: warning: typedef 'sizeof_int64_is_not_sizeof_long_long' locally defined but not used [-Wunused-local-typedefs]
In file included from ../../protobuf-2.6.0/src/google/protobuf/stubs/substitute.h:36:0,
                 from ../../protobuf-2.6.0/src/google/protobuf/stubs/substitute.cc:33:
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h: In function 'google::protobuf::uint64 google::protobuf::strtou64(const char*, char**, int)':
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h:346:109: warning: typedef 'sizeof_uint64_is_not_sizeof_long_long' locally defined but not used [-Wunused-local-typedefs]
depbase=`echo google/protobuf/stubs/structurally_valid.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/stubs/structurally_valid.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/stubs/structurally_valid.lo ../../protobuf-2.6.0/src/google/protobuf/stubs/structurally_valid.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/stubs/structurally_valid.lo -MD -MP -MF google/protobuf/stubs/.deps/structurally_valid.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/stubs/structurally_valid.cc  -fPIC -DPIC -o google/protobuf/stubs/.libs/structurally_valid.o
depbase=`echo google/protobuf/descriptor.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/descriptor.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/descriptor.lo ../../protobuf-2.6.0/src/google/protobuf/descriptor.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/descriptor.lo -MD -MP -MF google/protobuf/.deps/descriptor.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/descriptor.cc  -fPIC -DPIC -o google/protobuf/.libs/descriptor.o
In file included from ../../protobuf-2.6.0/src/google/protobuf/descriptor.cc:57:0:
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h: In function 'google::protobuf::int64 google::protobuf::strto64(const char*, char**, int)':
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h:340:99: warning: typedef 'sizeof_int64_is_not_sizeof_long_long' locally defined but not used [-Wunused-local-typedefs]
In file included from ../../protobuf-2.6.0/src/google/protobuf/descriptor.cc:57:0:
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h: In function 'google::protobuf::uint64 google::protobuf::strtou64(const char*, char**, int)':
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h:346:109: warning: typedef 'sizeof_uint64_is_not_sizeof_long_long' locally defined but not used [-Wunused-local-typedefs]
In file included from ../../protobuf-2.6.0/src/google/protobuf/descriptor.cc:59:0:
../../protobuf-2.6.0/src/google/protobuf/stubs/map_util.h: In function 'void google::protobuf::InsertOrDie(Collection*, const typename Collection::value_type::first_type&, const typename Collection::value_type::second_type&)':
../../protobuf-2.6.0/src/google/protobuf/stubs/map_util.h:356:43: warning: typedef 'value_type' locally defined but not used [-Wunused-local-typedefs]
   typedef typename Collection::value_type value_type;
                                           ^
In file included from ../../protobuf-2.6.0/src/google/protobuf/descriptor.cc:59:0:
../../protobuf-2.6.0/src/google/protobuf/stubs/map_util.h: In function 'void google::protobuf::InsertOrDieNoPrint(Collection*, const typename Collection::value_type::first_type&, const typename Collection::value_type::second_type&)':
../../protobuf-2.6.0/src/google/protobuf/stubs/map_util.h:367:43: warning: typedef 'value_type' locally defined but not used [-Wunused-local-typedefs]
   typedef typename Collection::value_type value_type;
                                           ^
depbase=`echo google/protobuf/descriptor.pb.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/descriptor.pb.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/descriptor.pb.lo ../../protobuf-2.6.0/src/google/protobuf/descriptor.pb.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/descriptor.pb.lo -MD -MP -MF google/protobuf/.deps/descriptor.pb.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/descriptor.pb.cc  -fPIC -DPIC -o google/protobuf/.libs/descriptor.pb.o
depbase=`echo google/protobuf/descriptor_database.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/descriptor_database.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/descriptor_database.lo ../../protobuf-2.6.0/src/google/protobuf/descriptor_database.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/descriptor_database.lo -MD -MP -MF google/protobuf/.deps/descriptor_database.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/descriptor_database.cc  -fPIC -DPIC -o google/protobuf/.libs/descriptor_database.o
In file included from ../../protobuf-2.6.0/src/google/protobuf/descriptor_database.cc:41:0:
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h: In function 'google::protobuf::int64 google::protobuf::strto64(const char*, char**, int)':
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h:340:99: warning: typedef 'sizeof_int64_is_not_sizeof_long_long' locally defined but not used [-Wunused-local-typedefs]
In file included from ../../protobuf-2.6.0/src/google/protobuf/descriptor_database.cc:41:0:
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h: In function 'google::protobuf::uint64 google::protobuf::strtou64(const char*, char**, int)':
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h:346:109: warning: typedef 'sizeof_uint64_is_not_sizeof_long_long' locally defined but not used [-Wunused-local-typedefs]
In file included from ../../protobuf-2.6.0/src/google/protobuf/descriptor_database.cc:43:0:
../../protobuf-2.6.0/src/google/protobuf/stubs/map_util.h: In function 'void google::protobuf::InsertOrDie(Collection*, const typename Collection::value_type::first_type&, const typename Collection::value_type::second_type&)':
../../protobuf-2.6.0/src/google/protobuf/stubs/map_util.h:356:43: warning: typedef 'value_type' locally defined but not used [-Wunused-local-typedefs]
   typedef typename Collection::value_type value_type;
                                           ^
In file included from ../../protobuf-2.6.0/src/google/protobuf/descriptor_database.cc:43:0:
../../protobuf-2.6.0/src/google/protobuf/stubs/map_util.h: In function 'void google::protobuf::InsertOrDieNoPrint(Collection*, const typename Collection::value_type::first_type&, const typename Collection::value_type::second_type&)':
../../protobuf-2.6.0/src/google/protobuf/stubs/map_util.h:367:43: warning: typedef 'value_type' locally defined but not used [-Wunused-local-typedefs]
   typedef typename Collection::value_type value_type;
                                           ^
depbase=`echo google/protobuf/dynamic_message.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/dynamic_message.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/dynamic_message.lo ../../protobuf-2.6.0/src/google/protobuf/dynamic_message.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/dynamic_message.lo -MD -MP -MF google/protobuf/.deps/dynamic_message.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/dynamic_message.cc  -fPIC -DPIC -o google/protobuf/.libs/dynamic_message.o
depbase=`echo google/protobuf/extension_set_heavy.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/extension_set_heavy.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/extension_set_heavy.lo ../../protobuf-2.6.0/src/google/protobuf/extension_set_heavy.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/extension_set_heavy.lo -MD -MP -MF google/protobuf/.deps/extension_set_heavy.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/extension_set_heavy.cc  -fPIC -DPIC -o google/protobuf/.libs/extension_set_heavy.o
depbase=`echo google/protobuf/generated_message_reflection.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/generated_message_reflection.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/generated_message_reflection.lo ../../protobuf-2.6.0/src/google/protobuf/generated_message_reflection.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/generated_message_reflection.lo -MD -MP -MF google/protobuf/.deps/generated_message_reflection.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/generated_message_reflection.cc  -fPIC -DPIC -o google/protobuf/.libs/generated_message_reflection.o
../../protobuf-2.6.0/src/google/protobuf/generated_message_reflection.cc: In member function 'void google::protobuf::internal::GeneratedMessageReflection::SwapOneofField(google::protobuf::Message*, google::protobuf::Message*, const google::protobuf::OneofDescriptor*) const':
../../protobuf-2.6.0/src/google/protobuf/generated_message_reflection.cc:506:89: warning: 'field1' may be used uninitialized in this function [-Wmaybe-uninitialized]
../../protobuf-2.6.0/src/google/protobuf/generated_message_reflection.cc:516:60: warning: 'temp_message' may be used uninitialized in this function [-Wmaybe-uninitialized]
         SetAllocatedMessage(message2, temp_message, field1);
                                                            ^
depbase=`echo google/protobuf/message.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/message.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/message.lo ../../protobuf-2.6.0/src/google/protobuf/message.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/message.lo -MD -MP -MF google/protobuf/.deps/message.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/message.cc  -fPIC -DPIC -o google/protobuf/.libs/message.o
In file included from ../../protobuf-2.6.0/src/google/protobuf/message.cc:50:0:
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h: In function 'google::protobuf::int64 google::protobuf::strto64(const char*, char**, int)':
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h:340:99: warning: typedef 'sizeof_int64_is_not_sizeof_long_long' locally defined but not used [-Wunused-local-typedefs]
In file included from ../../protobuf-2.6.0/src/google/protobuf/message.cc:50:0:
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h: In function 'google::protobuf::uint64 google::protobuf::strtou64(const char*, char**, int)':
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h:346:109: warning: typedef 'sizeof_uint64_is_not_sizeof_long_long' locally defined but not used [-Wunused-local-typedefs]
In file included from ../../protobuf-2.6.0/src/google/protobuf/message.cc:51:0:
../../protobuf-2.6.0/src/google/protobuf/stubs/map_util.h: In function 'void google::protobuf::InsertOrDie(Collection*, const typename Collection::value_type::first_type&, const typename Collection::value_type::second_type&)':
../../protobuf-2.6.0/src/google/protobuf/stubs/map_util.h:356:43: warning: typedef 'value_type' locally defined but not used [-Wunused-local-typedefs]
   typedef typename Collection::value_type value_type;
                                           ^
In file included from ../../protobuf-2.6.0/src/google/protobuf/message.cc:51:0:
../../protobuf-2.6.0/src/google/protobuf/stubs/map_util.h: In function 'void google::protobuf::InsertOrDieNoPrint(Collection*, const typename Collection::value_type::first_type&, const typename Collection::value_type::second_type&)':
../../protobuf-2.6.0/src/google/protobuf/stubs/map_util.h:367:43: warning: typedef 'value_type' locally defined but not used [-Wunused-local-typedefs]
   typedef typename Collection::value_type value_type;
                                           ^
depbase=`echo google/protobuf/reflection_ops.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/reflection_ops.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/reflection_ops.lo ../../protobuf-2.6.0/src/google/protobuf/reflection_ops.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/reflection_ops.lo -MD -MP -MF google/protobuf/.deps/reflection_ops.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/reflection_ops.cc  -fPIC -DPIC -o google/protobuf/.libs/reflection_ops.o
In file included from ../../protobuf-2.6.0/src/google/protobuf/reflection_ops.cc:42:0:
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h: In function 'google::protobuf::int64 google::protobuf::strto64(const char*, char**, int)':
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h:340:99: warning: typedef 'sizeof_int64_is_not_sizeof_long_long' locally defined but not used [-Wunused-local-typedefs]
In file included from ../../protobuf-2.6.0/src/google/protobuf/reflection_ops.cc:42:0:
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h: In function 'google::protobuf::uint64 google::protobuf::strtou64(const char*, char**, int)':
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h:346:109: warning: typedef 'sizeof_uint64_is_not_sizeof_long_long' locally defined but not used [-Wunused-local-typedefs]
depbase=`echo google/protobuf/service.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/service.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/service.lo ../../protobuf-2.6.0/src/google/protobuf/service.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/service.lo -MD -MP -MF google/protobuf/.deps/service.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/service.cc  -fPIC -DPIC -o google/protobuf/.libs/service.o
depbase=`echo google/protobuf/text_format.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/text_format.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/text_format.lo ../../protobuf-2.6.0/src/google/protobuf/text_format.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/text_format.lo -MD -MP -MF google/protobuf/.deps/text_format.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/text_format.cc  -fPIC -DPIC -o google/protobuf/.libs/text_format.o
In file included from ../../protobuf-2.6.0/src/google/protobuf/text_format.cc:53:0:
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h: In function 'google::protobuf::int64 google::protobuf::strto64(const char*, char**, int)':
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h:340:99: warning: typedef 'sizeof_int64_is_not_sizeof_long_long' locally defined but not used [-Wunused-local-typedefs]
In file included from ../../protobuf-2.6.0/src/google/protobuf/text_format.cc:53:0:
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h: In function 'google::protobuf::uint64 google::protobuf::strtou64(const char*, char**, int)':
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h:346:109: warning: typedef 'sizeof_uint64_is_not_sizeof_long_long' locally defined but not used [-Wunused-local-typedefs]
In file included from ../../protobuf-2.6.0/src/google/protobuf/text_format.cc:54:0:
../../protobuf-2.6.0/src/google/protobuf/stubs/map_util.h: In function 'void google::protobuf::InsertOrDie(Collection*, const typename Collection::value_type::first_type&, const typename Collection::value_type::second_type&)':
../../protobuf-2.6.0/src/google/protobuf/stubs/map_util.h:356:43: warning: typedef 'value_type' locally defined but not used [-Wunused-local-typedefs]
   typedef typename Collection::value_type value_type;
                                           ^
In file included from ../../protobuf-2.6.0/src/google/protobuf/text_format.cc:54:0:
../../protobuf-2.6.0/src/google/protobuf/stubs/map_util.h: In function 'void google::protobuf::InsertOrDieNoPrint(Collection*, const typename Collection::value_type::first_type&, const typename Collection::value_type::second_type&)':
../../protobuf-2.6.0/src/google/protobuf/stubs/map_util.h:367:43: warning: typedef 'value_type' locally defined but not used [-Wunused-local-typedefs]
   typedef typename Collection::value_type value_type;
                                           ^
depbase=`echo google/protobuf/unknown_field_set.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/unknown_field_set.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/unknown_field_set.lo ../../protobuf-2.6.0/src/google/protobuf/unknown_field_set.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/unknown_field_set.lo -MD -MP -MF google/protobuf/.deps/unknown_field_set.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/unknown_field_set.cc  -fPIC -DPIC -o google/protobuf/.libs/unknown_field_set.o
depbase=`echo google/protobuf/wire_format.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/wire_format.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/wire_format.lo ../../protobuf-2.6.0/src/google/protobuf/wire_format.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/wire_format.lo -MD -MP -MF google/protobuf/.deps/wire_format.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/wire_format.cc  -fPIC -DPIC -o google/protobuf/.libs/wire_format.o
depbase=`echo google/protobuf/io/gzip_stream.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/io/gzip_stream.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/io/gzip_stream.lo ../../protobuf-2.6.0/src/google/protobuf/io/gzip_stream.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/io/gzip_stream.lo -MD -MP -MF google/protobuf/io/.deps/gzip_stream.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/io/gzip_stream.cc  -fPIC -DPIC -o google/protobuf/io/.libs/gzip_stream.o
depbase=`echo google/protobuf/io/printer.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/io/printer.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/io/printer.lo ../../protobuf-2.6.0/src/google/protobuf/io/printer.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/io/printer.lo -MD -MP -MF google/protobuf/io/.deps/printer.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/io/printer.cc  -fPIC -DPIC -o google/protobuf/io/.libs/printer.o
depbase=`echo google/protobuf/io/strtod.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/io/strtod.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/io/strtod.lo ../../protobuf-2.6.0/src/google/protobuf/io/strtod.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/io/strtod.lo -MD -MP -MF google/protobuf/io/.deps/strtod.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/io/strtod.cc  -fPIC -DPIC -o google/protobuf/io/.libs/strtod.o
depbase=`echo google/protobuf/io/tokenizer.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/io/tokenizer.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/io/tokenizer.lo ../../protobuf-2.6.0/src/google/protobuf/io/tokenizer.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/io/tokenizer.lo -MD -MP -MF google/protobuf/io/.deps/tokenizer.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/io/tokenizer.cc  -fPIC -DPIC -o google/protobuf/io/.libs/tokenizer.o
In file included from ../../protobuf-2.6.0/src/google/protobuf/io/tokenizer.cc:96:0:
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h: In function 'google::protobuf::int64 google::protobuf::strto64(const char*, char**, int)':
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h:340:99: warning: typedef 'sizeof_int64_is_not_sizeof_long_long' locally defined but not used [-Wunused-local-typedefs]
In file included from ../../protobuf-2.6.0/src/google/protobuf/io/tokenizer.cc:96:0:
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h: In function 'google::protobuf::uint64 google::protobuf::strtou64(const char*, char**, int)':
../../protobuf-2.6.0/src/google/protobuf/stubs/strutil.h:346:109: warning: typedef 'sizeof_uint64_is_not_sizeof_long_long' locally defined but not used [-Wunused-local-typedefs]
depbase=`echo google/protobuf/io/zero_copy_stream_impl.lo | sed 's|[^/]*$|.deps/&|;s|\.lo$||'`;\
/bin/bash ../libtool  --tag=CXX   --mode=compile g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I..    -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/io/zero_copy_stream_impl.lo -MD -MP -MF $depbase.Tpo -c -o google/protobuf/io/zero_copy_stream_impl.lo ../../protobuf-2.6.0/src/google/protobuf/io/zero_copy_stream_impl.cc &&\
mv -f $depbase.Tpo $depbase.Plo
libtool: compile:  g++ -std=gnu++11 -DHAVE_CONFIG_H -I. -I../../protobuf-2.6.0/src -I.. -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -MT google/protobuf/io/zero_copy_stream_impl.lo -MD -MP -MF google/protobuf/io/.deps/zero_copy_stream_impl.Tpo -c ../../protobuf-2.6.0/src/google/protobuf/io/zero_copy_stream_impl.cc  -fPIC -DPIC -o google/protobuf/io/.libs/zero_copy_stream_impl.o
/bin/bash ../libtool  --tag=CXX   --mode=link g++ -std=gnu++11 -pthread -Wall -Wwrite-strings -Woverloaded-virtual -Wno-sign-compare -Wno-return-type  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -version-info 9:0:0 -export-dynamic -no-undefined  -o libprotobuf.la -rpath /usr/local/lib google/protobuf/stubs/atomicops_internals_x86_gcc.lo google/protobuf/stubs/atomicops_internals_x86_msvc.lo google/protobuf/stubs/common.lo google/protobuf/stubs/once.lo google/protobuf/stubs/stringprintf.lo google/protobuf/extension_set.lo google/protobuf/generated_message_util.lo google/protobuf/message_lite.lo google/protobuf/repeated_field.lo google/protobuf/wire_format_lite.lo google/protobuf/io/coded_stream.lo google/protobuf/io/zero_copy_stream.lo google/protobuf/io/zero_copy_stream_impl_lite.lo google/protobuf/stubs/strutil.lo google/protobuf/stubs/substitute.lo google/protobuf/stubs/structurally_valid.lo google/protobuf/descriptor.lo google/protobuf/descriptor.pb.lo google/protobuf/descriptor_database.lo google/protobuf/dynamic_message.lo google/protobuf/extension_set_heavy.lo google/protobuf/generated_message_reflection.lo google/protobuf/message.lo google/protobuf/reflection_ops.lo google/protobuf/service.lo google/protobuf/text_format.lo google/protobuf/unknown_field_set.lo google/protobuf/wire_format.lo google/protobuf/io/gzip_stream.lo google/protobuf/io/printer.lo google/protobuf/io/strtod.lo google/protobuf/io/tokenizer.lo google/protobuf/io/zero_copy_stream_impl.lo -lpthread -lz 
libtool: link: rm -fr  .libs/libprotobuf.la .libs/libprotobuf.lai .libs/libprotobuf.so .libs/libprotobuf.so.9 .libs/libprotobuf.so.9.0.0
libtool: link: g++ -std=gnu++11  -fPIC -DPIC -shared -nostdlib /usr/lib/gcc/x86_64-linux-gnu/5/../../../x86_64-linux-gnu/crti.o /usr/lib/gcc/x86_64-linux-gnu/5/crtbeginS.o  google/protobuf/stubs/.libs/atomicops_internals_x86_gcc.o google/protobuf/stubs/.libs/atomicops_internals_x86_msvc.o google/protobuf/stubs/.libs/common.o google/protobuf/stubs/.libs/once.o google/protobuf/stubs/.libs/stringprintf.o google/protobuf/.libs/extension_set.o google/protobuf/.libs/generated_message_util.o google/protobuf/.libs/message_lite.o google/protobuf/.libs/repeated_field.o google/protobuf/.libs/wire_format_lite.o google/protobuf/io/.libs/coded_stream.o google/protobuf/io/.libs/zero_copy_stream.o google/protobuf/io/.libs/zero_copy_stream_impl_lite.o google/protobuf/stubs/.libs/strutil.o google/protobuf/stubs/.libs/substitute.o google/protobuf/stubs/.libs/structurally_valid.o google/protobuf/.libs/descriptor.o google/protobuf/.libs/descriptor.pb.o google/protobuf/.libs/descriptor_database.o google/protobuf/.libs/dynamic_message.o google/protobuf/.libs/extension_set_heavy.o google/protobuf/.libs/generated_message_reflection.o google/protobuf/.libs/message.o google/protobuf/.libs/reflection_ops.o google/protobuf/.libs/service.o google/protobuf/.libs/text_format.o google/protobuf/.libs/unknown_field_set.o google/protobuf/.libs/wire_format.o google/protobuf/io/.libs/gzip_stream.o google/protobuf/io/.libs/printer.o google/protobuf/io/.libs/strtod.o google/protobuf/io/.libs/tokenizer.o google/protobuf/io/.libs/zero_copy_stream_impl.o   -lpthread -lz -L/usr/lib/gcc/x86_64-linux-gnu/5 -L/usr/lib/gcc/x86_64-linux-gnu/5/../../../x86_64-linux-gnu -L/usr/lib/gcc/x86_64-linux-gnu/5/../../../../lib -L/lib/x86_64-linux-gnu -L/lib/../lib -L/usr/lib/x86_64-linux-gnu -L/usr/lib/../lib -L/usr/lib/gcc/x86_64-linux-gnu/5/../../.. -lstdc++ -lm -lc -lgcc_s /usr/lib/gcc/x86_64-linux-gnu/5/crtendS.o /usr/lib/gcc/x86_64-linux-gnu/5/../../../x86_64-linux-gnu/crtn.o  -pthread -g -O2 -fstack-protector-strong -g   -pthread -Wl,-soname -Wl,libprotobuf.so.9 -o .libs/libprotobuf.so.9.0.0
libtool: link: (cd ".libs" && rm -f "libprotobuf.so.9" && ln -s "libprotobuf.so.9.0.0" "libprotobuf.so.9")
libtool: link: (cd ".libs" && rm -f "libprotobuf.so" && ln -s "libprotobuf.so.9.0.0" "libprotobuf.so")
libtool: link: ( cd ".libs" && rm -f "libprotobuf.la" && ln -s "../libprotobuf.la" "libprotobuf.la" )
make[2]: Leaving directory '/home/muelleki/git/R/dplyr/revdep/checks/flowWorkspace/old/flowWorkspace.Rcheck/00_pkg_src/flowWorkspace/src/pb_build/src'
make[1]: Leaving directory '/home/muelleki/git/R/dplyr/revdep/checks/flowWorkspace/old/flowWorkspace.Rcheck/00_pkg_src/flowWorkspace/src/pb_build'
configure: Using the following compilation and linking flags for flowWorkspace
configure:    PKG_CPPFLAGS=-I/usr/include/libxml2 -Iprotobuf-2.6.0/src
configure:    PB_INCLUDE=protobuf-2.6.0/src
configure:    PKG_LIBS=-lxml2 
configure: creating ./config.status
config.status: creating src/Makevars
** libs
mkdir -p "/home/muelleki/git/R/dplyr/revdep/checks/flowWorkspace/old/flowWorkspace.Rcheck/flowWorkspace/include/flowWorkspace"
cp include/* /home/muelleki/git/R/dplyr/revdep/checks/flowWorkspace/old/flowWorkspace.Rcheck/flowWorkspace/include/flowWorkspace
cp -r protobuf-2.6.0/src/google /home/muelleki/git/R/dplyr/revdep/checks/flowWorkspace/old/flowWorkspace.Rcheck/flowWorkspace/include
cp GatingSet.pb.h /home/muelleki/git/R/dplyr/revdep/checks/flowWorkspace/old/flowWorkspace.Rcheck/flowWorkspace/include #GatingSet.pb.h is autogenerated by protoc and thus kept at the same folder as GatingSet.proto
g++ -std=gnu++11 -I/usr/share/R/include -DNDEBUG -DROUT -I/usr/include/libxml2 -Iprotobuf-2.6.0/src -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/old/Rcpp/include" -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/old/BH/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c delimitedMessage.cpp -o delimitedMessage.o
g++ -std=gnu++11 -I/usr/share/R/include -DNDEBUG -DROUT -I/usr/include/libxml2 -Iprotobuf-2.6.0/src -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/old/Rcpp/include" -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/old/BH/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c calibrationTable.cpp -o calibrationTable.o
g++ -std=gnu++11 -I/usr/share/R/include -DNDEBUG -DROUT -I/usr/include/libxml2 -Iprotobuf-2.6.0/src -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/old/Rcpp/include" -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/old/BH/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c ellipse2points.cpp -o ellipse2points.o
g++ -std=gnu++11 -I/usr/share/R/include -DNDEBUG -DROUT -I/usr/include/libxml2 -Iprotobuf-2.6.0/src -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/old/Rcpp/include" -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/old/BH/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c POPINDICES.cpp -o POPINDICES.o
g++ -std=gnu++11 -I/usr/share/R/include -DNDEBUG -DROUT -I/usr/include/libxml2 -Iprotobuf-2.6.0/src -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/old/Rcpp/include" -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/old/BH/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c flowData.cpp -o flowData.o
g++ -std=gnu++11 -I/usr/share/R/include -DNDEBUG -DROUT -I/usr/include/libxml2 -Iprotobuf-2.6.0/src -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/old/Rcpp/include" -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/old/BH/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c GatingSet.pb.cc -o GatingSet.pb.o
g++ -std=gnu++11 -I/usr/share/R/include -DNDEBUG -DROUT -I/usr/include/libxml2 -Iprotobuf-2.6.0/src -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/old/Rcpp/include" -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/old/BH/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c gate.cpp -o gate.o
g++ -std=gnu++11 -I/usr/share/R/include -DNDEBUG -DROUT -I/usr/include/libxml2 -Iprotobuf-2.6.0/src -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/old/Rcpp/include" -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/old/BH/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c global.cpp -o global.o
g++ -std=gnu++11 -I/usr/share/R/include -DNDEBUG -DROUT -I/usr/include/libxml2 -Iprotobuf-2.6.0/src -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/old/Rcpp/include" -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/old/BH/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c compensation.cpp -o compensation.o
mkdir -p "/home/muelleki/git/R/dplyr/revdep/checks/flowWorkspace/old/flowWorkspace.Rcheck/flowWorkspace/lib"
ar rs "/home/muelleki/git/R/dplyr/revdep/checks/flowWorkspace/old/flowWorkspace.Rcheck/flowWorkspace/lib/libprotobuf.a" pb_build/src/google/protobuf/.libs/*.o pb_build/src/google/protobuf/stubs/.libs/*.o pb_build/src/google/protobuf/io/.libs/*.o
ar: creating /home/muelleki/git/R/dplyr/revdep/checks/flowWorkspace/old/flowWorkspace.Rcheck/flowWorkspace/lib/libprotobuf.a
g++ -std=gnu++11 -I/usr/share/R/include -DNDEBUG -DROUT -I/usr/include/libxml2 -Iprotobuf-2.6.0/src -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/old/Rcpp/include" -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/old/BH/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c spline.cpp -o spline.o
g++ -std=gnu++11 -I/usr/share/R/include -DNDEBUG -DROUT -I/usr/include/libxml2 -Iprotobuf-2.6.0/src -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/old/Rcpp/include" -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/old/BH/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c wsNode.cpp -o wsNode.o
g++ -std=gnu++11 -I/usr/share/R/include -DNDEBUG -DROUT -I/usr/include/libxml2 -Iprotobuf-2.6.0/src -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/old/Rcpp/include" -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/old/BH/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c getSingleCellExpression.cpp -o getSingleCellExpression.o
g++ -std=gnu++11 -I/usr/share/R/include -DNDEBUG -DROUT -I/usr/include/libxml2 -Iprotobuf-2.6.0/src -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/old/Rcpp/include" -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/old/BH/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c R_GatingSet.cpp -o R_GatingSet.o
g++ -std=gnu++11 -I/usr/share/R/include -DNDEBUG -DROUT -I/usr/include/libxml2 -Iprotobuf-2.6.0/src -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/old/Rcpp/include" -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/old/BH/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c nodeProperties.cpp -o nodeProperties.o
g++ -std=gnu++11 -I/usr/share/R/include -DNDEBUG -DROUT -I/usr/include/libxml2 -Iprotobuf-2.6.0/src -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/old/Rcpp/include" -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/old/BH/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c workspace.cpp -o workspace.o
g++ -std=gnu++11 -I/usr/share/R/include -DNDEBUG -DROUT -I/usr/include/libxml2 -Iprotobuf-2.6.0/src -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/old/Rcpp/include" -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/old/BH/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++11 -I/usr/share/R/include -DNDEBUG -DROUT -I/usr/include/libxml2 -Iprotobuf-2.6.0/src -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/old/Rcpp/include" -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/old/BH/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c transformation.cpp -o transformation.o
g++ -std=gnu++11 -I/usr/share/R/include -DNDEBUG -DROUT -I/usr/include/libxml2 -Iprotobuf-2.6.0/src -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/old/Rcpp/include" -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/old/BH/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c GatingSet.cpp -o GatingSet.o
g++ -std=gnu++11 -I/usr/share/R/include -DNDEBUG -DROUT -I/usr/include/libxml2 -Iprotobuf-2.6.0/src -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/old/Rcpp/include" -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/old/BH/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c R_API.cpp -o R_API.o
g++ -std=gnu++11 -I/usr/share/R/include -DNDEBUG -DROUT -I/usr/include/libxml2 -Iprotobuf-2.6.0/src -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/old/Rcpp/include" -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/old/BH/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c macFlowJoWorkspace.cpp -o macFlowJoWorkspace.o
g++ -std=gnu++11 -I/usr/share/R/include -DNDEBUG -DROUT -I/usr/include/libxml2 -Iprotobuf-2.6.0/src -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/old/Rcpp/include" -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/old/BH/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c flowJoWorkspace.cpp -o flowJoWorkspace.o
g++ -std=gnu++11 -I/usr/share/R/include -DNDEBUG -DROUT -I/usr/include/libxml2 -Iprotobuf-2.6.0/src -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/old/Rcpp/include" -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/old/BH/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c winFlowJoWorkspace.cpp -o winFlowJoWorkspace.o
g++ -std=gnu++11 -I/usr/share/R/include -DNDEBUG -DROUT -I/usr/include/libxml2 -Iprotobuf-2.6.0/src -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/old/Rcpp/include" -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/old/BH/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c R_GatingHierarchy.cpp -o R_GatingHierarchy.o
g++ -std=gnu++11 -I/usr/share/R/include -DNDEBUG -DROUT -I/usr/include/libxml2 -Iprotobuf-2.6.0/src -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/old/Rcpp/include" -I"/home/muelleki/git/R/dplyr/revdep/library/dplyr/old/BH/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c GatingHierarchy.cpp -o GatingHierarchy.o
mkdir -p "/home/muelleki/git/R/dplyr/revdep/checks/flowWorkspace/old/flowWorkspace.Rcheck/flowWorkspace/lib"
ar rs "/home/muelleki/git/R/dplyr/revdep/checks/flowWorkspace/old/flowWorkspace.Rcheck/flowWorkspace/lib/libflowWorkspace.a" GatingHierarchy.o GatingSet.o GatingSet.pb.o POPINDICES.o R_API.o R_GatingHierarchy.o R_GatingSet.o RcppExports.o calibrationTable.o compensation.o delimitedMessage.o ellipse2points.o flowData.o flowJoWorkspace.o gate.o getSingleCellExpression.o global.o macFlowJoWorkspace.o nodeProperties.o spline.o transformation.o winFlowJoWorkspace.o workspace.o wsNode.o
ar: creating /home/muelleki/git/R/dplyr/revdep/checks/flowWorkspace/old/flowWorkspace.Rcheck/flowWorkspace/lib/libflowWorkspace.a
g++ -std=gnu++11 -shared -L/usr/lib/R/lib -Wl,-Bsymbolic-functions -Wl,-z,relro -o flowWorkspace.so GatingHierarchy.o GatingSet.o GatingSet.pb.o POPINDICES.o R_API.o R_GatingHierarchy.o R_GatingSet.o RcppExports.o calibrationTable.o compensation.o delimitedMessage.o ellipse2points.o flowData.o flowJoWorkspace.o gate.o getSingleCellExpression.o global.o macFlowJoWorkspace.o nodeProperties.o spline.o transformation.o winFlowJoWorkspace.o workspace.o wsNode.o /home/muelleki/git/R/dplyr/revdep/checks/flowWorkspace/old/flowWorkspace.Rcheck/flowWorkspace/lib/libprotobuf.a /home/muelleki/git/R/dplyr/revdep/library/flowWorkspace/flowCore/lib/libboost_regex.a -lxml2 -L/usr/lib/R/lib -lR
installing to /home/muelleki/git/R/dplyr/revdep/checks/flowWorkspace/old/flowWorkspace.Rcheck/flowWorkspace/libs
** R
** inst
** preparing package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded
Error: package or namespace load failed for ‘flowWorkspace’ in dyn.load(file, DLLpath = DLLpath, ...):
 unable to load shared object '/home/muelleki/git/R/dplyr/revdep/checks/flowWorkspace/old/flowWorkspace.Rcheck/flowWorkspace/libs/flowWorkspace.so':
  /home/muelleki/git/R/dplyr/revdep/checks/flowWorkspace/old/flowWorkspace.Rcheck/flowWorkspace/libs/flowWorkspace.so: undefined symbol: _ZN5boost16re_detail_10650012perl_matcherIN9__gnu_cxx17__normal_iteratorIPKcNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEEESaINS_9sub_matchISC_EEENS_12regex_traitsIcNS_16cpp_regex_traitsIcEEEEE14construct_initERKNS_11basic_regexIcSJ_EENS_15regex_constants12_match_flagsE
Error: loading failed
Execution halted
ERROR: loading failed
* removing ‘/home/muelleki/git/R/dplyr/revdep/checks/flowWorkspace/old/flowWorkspace.Rcheck/flowWorkspace’

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
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        libs   4.5Mb
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

Version: 0.8.14

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘alr3’, ‘doBy’, ‘psych’, ‘prettyR’, ‘fBasics’, ‘RMark’, ‘asbio’, ‘PMCMR’, ‘pgirmess’, ‘agricolae’, ‘DescTools’
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
      27: eval(exprs, env)
      28: source_file(path, new.env(parent = env), chdir = TRUE)
      29: force(code)
      30: with_reporter(reporter = reporter, start_end_reporter = start_end_reporter,     {        lister$start_file(basename(path))        source_file(path, new.env(parent = env), chdir = TRUE)        end_context()    })
      31: FUN(X[[i]], ...)
      32: lapply(paths, test_file, env = env, reporter = current_reporter,     start_end_reporter = FALSE, load_helpers = FALSE)
      33: force(code)
      34: with_reporter(reporter = current_reporter, results <- lapply(paths,     test_file, env = env, reporter = current_reporter, start_end_reporter = FALSE,     load_helpers = FALSE))
      35: test_files(paths, reporter = reporter, env = env, ...)
      36: test_dir(test_path, reporter = reporter, env = env, filter = filter,     ...)
      37: with_top_env(env, {    test_dir(test_path, reporter = reporter, env = env, filter = filter,         ...)})
      38: run_tests(package, test_path, filter, reporter, ...)
      39: test_check("GA4GHclient")
      An irrecoverable exception occurred. R is aborting now ...
      Segmentation fault (core dumped)
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
      16: stop("cannot find defined namespace(s) with prefix(es) ", paste(namespaces[i][is.na(idx)], 
             collapse = ", "))
      
      testthat results ================================================================
      OK: 174 SKIPPED: 0 FAILED: 7
      1. Failure: error on url (@test-geoknife_utils.R#15) 
      2. Error: webdata query returns a datagroup (@test-query_webdatasets.R#6) 
      3. Error: webdata query returns a list (@test-query_webdatasets.R#12) 
      4. Error: webdata title returns a title (@test-query_webdatasets.R#18) 
      5. Error: webdata url returns a url (@test-query_webdatasets.R#23) 
      6. Error: webdata abstract returns an abstract (@test-query_webdatasets.R#30) 
      7. Error: show datagroup (@test-show_object.R#24) 
      
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

# ggformula

Version: 0.6

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mosaic’
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

Version: 1.5.1.1

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      The following object is masked from 'package:graphics':
      
          dotchart
      
      
      Attaching package: 'zoo'
      
      The following objects are masked from 'package:base':
      
          as.Date, as.Date.numeric
      
      
       *** caught segfault ***
      address (nil), cause 'memory not mapped'
      Segmentation fault (core dumped)
    ```

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

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(testthat)
      > library(gutenbergr)
      > 
      > test_check("gutenbergr")
      1. Failure: read_zip_url can download and read a zip file (@test-utils.R#9) ----
      any(z == "Congress shall make no law respecting an establishment of religion,") isn't true.
      
      
      testthat results ================================================================
      OK: 46 SKIPPED: 0 FAILED: 1
      1. Failure: read_zip_url can download and read a zip file (@test-utils.R#9) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 13617 marked UTF-8 strings
    ```

# hansard

Version: 0.5.2

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat-ep.R’ failed.
    Last 13 lines of output:
      3: jsonlite::fromJSON(paste0(baseurl, status_query, min_sig_query, max_sig_query, dates, 
             extra_args), flatten = TRUE)
      4: fromJSON_string(txt = txt, simplifyVector = simplifyVector, simplifyDataFrame = simplifyDataFrame, 
             simplifyMatrix = simplifyMatrix, flatten = flatten, ...)
      5: parseJSON(txt, bigint_as_char)
      6: parse_con(txt, bigint_as_char)
      7: open(con, "rb")
      8: open.connection(con, "rb")
      
      testthat results ================================================================
      OK: 6 SKIPPED: 0 FAILED: 1
      1. Error: epetitions functions return expected format (@test_epetition.R#18) 
      
      Error: testthat unit tests failed
      Execution halted
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

Version: 1.10.0

## In both

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

Version: 0.3.0

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
      OK: 208 SKIPPED: 14 FAILED: 1
      1. Error: Row heights do not screw up latex multicol (@test-with-pandoc.R#20) 
      
      Error: testthat unit tests failed
      Execution halted
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
    /home/muelleki/tmp/Rtmp2mAH00/file27e453979e3e.sqlite3
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
      1. Failure: mnis_additional returns expected format (@test_additional.R#80) ----
      `xi` has length 349, not length 285.
      
      
      2. Failure: mnis_mps_on_date return expected format (@test_members_on_dates.R#18) 
      nrow(xmpon2) == 1730 isn't true.
      
      
      testthat results ================================================================
      OK: 97 SKIPPED: 0 FAILED: 2
      1. Failure: mnis_additional returns expected format (@test_additional.R#80) 
      2. Failure: mnis_mps_on_date return expected format (@test_members_on_dates.R#18) 
      
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

Version: 0.8.6

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

Version: 0.1.5

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
      8: function_list[[k]](value)
      9: ph_with_img(., type = "body", src = img.file, height = 1.06, width = 1.39)
      10: external_img(src, width = width, height = height)
      11: stopifnot(file.exists(src))
      12: stop(msg, call. = FALSE, domain = NA)
      
      testthat results ================================================================
      OK: 342 SKIPPED: 0 FAILED: 4
      1. Error: image add  (@test-docx-add.R#68) 
      2. Error: pml fp_border (@test-fp_cell.R#75) 
      3. Error: css fp_border (@test-fp_cell.R#165) 
      4. Error: add img into placeholder (@test-pptx-add.R#67) 
      
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

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      1. Failure: mps_on_date return expected format (@test_mps_on_date.R#18) --------
      nrow(xmpon2) == 1730 isn't true.
      
      
      2. Failure: mps_on_date return expected format (@test_mps_on_date.R#24) --------
      nrow(xmpon3) == 1730 isn't true.
      
      
      testthat results ================================================================
      OK: 13 SKIPPED: 0 FAILED: 2
      1. Failure: mps_on_date return expected format (@test_mps_on_date.R#18) 
      2. Failure: mps_on_date return expected format (@test_mps_on_date.R#24) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

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

Version: 0.1.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 22.7Mb
      sub-directories of 1Mb or more:
        libs  21.5Mb
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

Version: 2.2.5

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

# radiant.data

Version: 0.8.1

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘radiant.data-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: dtab.explore
    > ### Title: Make a tabel of summary statistics in DT
    > ### Aliases: dtab.explore
    > 
    > ### ** Examples
    > 
    > tab <- explore("diamonds", "price:x") %>% dtab
    > tab <- explore("diamonds", "price", byvar = "cut", fun = c("length", "skew"), top = "byvar") %>%
    +   dtab
    Error in length(byvar) : could not find function "length"
    Calls: %>% ... <Anonymous> -> map_if -> map -> lapply -> FUN -> .Call
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

Version: 4.1.0

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

*   checking installed package size ... NOTE
    ```
      installed size is  6.9Mb
      sub-directories of 1Mb or more:
        data   2.5Mb
        etc    1.9Mb
        po     1.2Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    
    (R:25953): Gtk-WARNING **: gtk_disable_setlocale() must be called before gtk_init()
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
      * 500: {"status":"error","message-type":"exception","message-version":"1.0.0","message":{"name":"class java.lang.NumberFormatException","description":"java.lang.NumberFormatException: For input string: \\"323234343434\\"","message":"For input string: \\"323234343434\\"","stack":["java.lang.NumberFormatException.forInputString(NumberFormatException.java:65)","java.lang.Integer.parseInt(Integer.java:583)","java.lang.Integer.parseInt(Integer.java:615)","cayenne.data.member$get_id_from_context.invokeStatic(member.clj:34)","cayenne.data.member$get_id_from_context.invoke(member.clj:30)","cayenne.data.member$fetch_one$fn__17559.invoke(member.clj:75)","cayenne.data.member$fetch_one.invokeStatic(member.clj:72)","cayenne.data.member$fetch_one.invoke(member.clj:71)","cayenne.api.v1.routes$member_works_resource$fn__22165$fn__22166.invoke(routes.clj:374)","liberator.core$decide.invokeStatic(core.clj:98)","liberator.core$decide.invoke(core.clj:91)","liberator.core$exists_QMARK_.invokeStatic(core.clj:406)","liberator.core$exists_QMARK_.invoke(core.clj:406)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$processable_QMARK_.invokeStatic(core.clj:409)","liberator.core$processable_QMARK_.invoke(core.clj:409)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$encoding_available_QMARK_.invokeStatic(core.clj:413)","liberator.core$encoding_available_QMARK_.invoke(core.clj:413)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$accept_encoding_exists_QMARK_.invokeStatic(core.clj:428)","liberator.core$accept_encoding_exists_QMARK_.invoke(core.clj:428)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$accept_charset_exists_QMARK_.invokeStatic(core.clj:441)","liberator.core$accept_charset_exists_QMARK_.invoke(core.clj:441)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$accept_language_exists_QMARK_.invokeStatic(core.clj:455)","liberator.core$accept_language_exists_QMARK_.invoke(core.clj:455)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$media_type_available_QMARK_.invokeStatic(core.clj:465)","liberator.core$media_type_available_QMARK_.invoke(core.clj:465)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$accept_exists_QMARK_.invokeStatic(core.clj:468)","liberator.core$accept_exists_QMARK_.invoke(core.clj:468)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$is_options_QMARK_.invokeStatic(core.clj:485)","liberator.core$is_options_QMARK_.invoke(core.clj:485)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$valid_entity_length_QMARK_.invokeStatic(core.clj:488)","liberator.core$valid_entity_length_QMARK_.invoke(core.clj:488)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$known_content_type_QMARK_.invokeStatic(core.clj:491)","liberator.core$known_content_type_QMARK_.invoke(core.clj:491)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$valid_content_header_QMARK_.invokeStatic(core.clj:493)","liberator.core$valid_content_header_QMARK_.invoke(core.clj:493)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$allowed_QMARK_.invokeStatic(core.clj:496)","liberator.core$allowed_QMARK_.invoke(core.clj:496)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$authorized_QMARK_.invokeStatic(core.clj:499)","liberator.core$authorized_QMARK_.invoke(core.clj:499)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$malformed_QMARK_.invokeStatic(core.clj:502)","liberator.core$malformed_QMARK_.invoke(core.clj:502)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$method_allowed_QMARK_.invokeStatic(core.clj:505)","liberator.core$method_allowed_QMARK_.invoke(core.clj:505)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$uri_too_long_QMARK_.invokeStatic(core.clj:508)","liberator.core$uri_too_long_QMARK_.invoke(core.clj:508)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$known_method_QMARK_.invokeStatic(core.clj:511)","liberator.core$known_method_QMARK_.invoke(core.clj:511)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$service_available_QMARK_.invokeStatic(core.clj:514)","liberator.core$service_available_QMARK_.invoke(core.clj:514)","liberator.core$run_resource.invokeStatic(core.clj:599)","liberator.core$run_resource.invoke(core.clj:597)","cayenne.api.v1.routes$member_works_resource$fn__22165.invoke(routes.clj:363)","compojure.response$eval21276$fn__21277.invoke(response.clj:47)","compojure.response$eval21198$fn__21199$G__21189__21206.invoke(response.clj:7)","compojure.core$wrap_response$fn__21868.invoke(core.clj:158)","compojure.core$wrap_route_middleware$fn__21852.invoke(core.clj:128)","compojure.core$wrap_route_info$fn__21857.invoke(core.clj:137)","compojure.core$wrap_route_matches$fn__21861.invoke(core.clj:146)","compojure.core$routing$fn__21876.invoke(core.clj:185)","clojure.core$some.invokeStatic(core.clj:2592)","clojure.core$some.invoke(core.clj:2583)","compojure.core$routing.invokeStatic(core.clj:185)","compojure.core$routing.doInvoke(core.clj:182)","clojure.lang.RestFn.applyTo(RestFn.java:139)","clojure.core$apply.invokeStatic(core.clj:648)","clojure.core$apply.invoke(core.clj:641)","compojure.core$routes$fn__21880.invoke(core.clj:192)","compojure.core$routing$fn__21876.invoke(core.clj:185)","clojure.core$some.invokeStatic(core.clj:2592)","clojure.core$some.invoke(core.clj:2583)","compojure.core$routing.invokeStatic(core.clj:185)","compojure.core$routing.doInvoke(core.clj:182)","clojure.lang.RestFn.applyTo(RestFn.java:139)","clojure.core$apply.invokeStatic(core.clj:648)","clojure.core$apply.invoke(core.clj:641)","compojure.core$routes$fn__21880.invoke(core.clj:192)","compojure.core$routing$fn__21876.invoke(core.clj:185)","clojure.core$some.invokeStatic(core.clj:2592)","clojure.core$some.invoke(core.clj:2583)","compojure.core$routing.invokeStatic(core.clj:185)","compojure.core$routing.doInvoke(core.clj:182)","clojure.lang.RestFn.applyTo(RestFn.java:139)","clojure.core$apply.invokeStatic(core.clj:648)","clojure.core$apply.invoke(core.clj:641)","compojure.core$routes$fn__21880.invoke(core.clj:192)","ring.middleware.logstash$wrap_logstash$fn__23197.invoke(logstash.clj:110)","ring.middleware.keyword_params$wrap_keyword_params$fn__24059.invoke(keyword_params.clj:36)","ring.middleware.nested_params$wrap_nested_params$fn__24107.invoke(nested_params.clj:89)","ring.middleware.params$wrap_params$fn__24023.invoke(params.clj:67)","cayenne.api.route$wrap_cors$fn__24643.invoke(route.clj:101)","metrics.ring.expose$expose_metrics_as_json$fn__23583.invoke(expose.clj:94)","metrics.ring.instrument$instrument$fn__23599$fn__23600.invoke(instrument.clj:44)","metrics.ring.instrument.proxy$java.lang.Object$Callable$7da976d4.call(Unknown Source)","com.yammer.metrics.core.Timer.time(Timer.java:91)","metrics.ring.instrument$instrument$fn__23599.invoke(instrument.clj:43)","heartbeat.ring$wrap_heartbeat$fn__23285.invoke(ring.clj:18)","cayenne.api.conneg$wrap_accept$fn__19739.invoke(conneg.clj:53)","cayenne.api.route$wrap_exception_handler$fn__24648.invoke(route.clj:110)","cayenne.api.route$wrap_ignore_trailing_slash$fn__24656.invoke(route.clj:136)","org.httpkit.server.HttpHandler.run(RingHandler.java:91)","java.util.concurrent.Executors$RunnableAdapter.call(Executors.java:511)","java.util.concurrent.Future
      * 500: {"status":"error","message-type":"exception","message-version":"1.0.0","message":{"name":"class java.lang.NumberFormatException","description":"java.lang.NumberFormatException: For input string: \\"3434343434\\"","message":"For input string: \\"3434343434\\"","stack":["java.lang.NumberFormatException.forInputString(NumberFormatException.java:65)","java.lang.Integer.parseInt(Integer.java:583)","java.lang.Integer.parseInt(Integer.java:615)","cayenne.data.member$get_id_from_context.invokeStatic(member.clj:34)","cayenne.data.member$get_id_from_context.invoke(member.clj:30)","cayenne.data.member$fetch_one$fn__17559.invoke(member.clj:75)","cayenne.data.member$fetch_one.invokeStatic(member.clj:72)","cayenne.data.member$fetch_one.invoke(member.clj:71)","cayenne.api.v1.routes$member_works_resource$fn__22165$fn__22166.invoke(routes.clj:374)","liberator.core$decide.invokeStatic(core.clj:98)","liberator.core$decide.invoke(core.clj:91)","liberator.core$exists_QMARK_.invokeStatic(core.clj:406)","liberator.core$exists_QMARK_.invoke(core.clj:406)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$processable_QMARK_.invokeStatic(core.clj:409)","liberator.core$processable_QMARK_.invoke(core.clj:409)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$encoding_available_QMARK_.invokeStatic(core.clj:413)","liberator.core$encoding_available_QMARK_.invoke(core.clj:413)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$accept_encoding_exists_QMARK_.invokeStatic(core.clj:428)","liberator.core$accept_encoding_exists_QMARK_.invoke(core.clj:428)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$accept_charset_exists_QMARK_.invokeStatic(core.clj:441)","liberator.core$accept_charset_exists_QMARK_.invoke(core.clj:441)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$accept_language_exists_QMARK_.invokeStatic(core.clj:455)","liberator.core$accept_language_exists_QMARK_.invoke(core.clj:455)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$media_type_available_QMARK_.invokeStatic(core.clj:465)","liberator.core$media_type_available_QMARK_.invoke(core.clj:465)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$accept_exists_QMARK_.invokeStatic(core.clj:468)","liberator.core$accept_exists_QMARK_.invoke(core.clj:468)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$is_options_QMARK_.invokeStatic(core.clj:485)","liberator.core$is_options_QMARK_.invoke(core.clj:485)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$valid_entity_length_QMARK_.invokeStatic(core.clj:488)","liberator.core$valid_entity_length_QMARK_.invoke(core.clj:488)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$known_content_type_QMARK_.invokeStatic(core.clj:491)","liberator.core$known_content_type_QMARK_.invoke(core.clj:491)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$valid_content_header_QMARK_.invokeStatic(core.clj:493)","liberator.core$valid_content_header_QMARK_.invoke(core.clj:493)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$allowed_QMARK_.invokeStatic(core.clj:496)","liberator.core$allowed_QMARK_.invoke(core.clj:496)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$authorized_QMARK_.invokeStatic(core.clj:499)","liberator.core$authorized_QMARK_.invoke(core.clj:499)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$malformed_QMARK_.invokeStatic(core.clj:502)","liberator.core$malformed_QMARK_.invoke(core.clj:502)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$method_allowed_QMARK_.invokeStatic(core.clj:505)","liberator.core$method_allowed_QMARK_.invoke(core.clj:505)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$uri_too_long_QMARK_.invokeStatic(core.clj:508)","liberator.core$uri_too_long_QMARK_.invoke(core.clj:508)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$known_method_QMARK_.invokeStatic(core.clj:511)","liberator.core$known_method_QMARK_.invoke(core.clj:511)","liberator.core$decide.invokeStatic(core.clj:103)","liberator.core$decide.invoke(core.clj:91)","liberator.core$service_available_QMARK_.invokeStatic(core.clj:514)","liberator.core$service_available_QMARK_.invoke(core.clj:514)","liberator.core$run_resource.invokeStatic(core.clj:599)","liberator.core$run_resource.invoke(core.clj:597)","cayenne.api.v1.routes$member_works_resource$fn__22165.invoke(routes.clj:363)","compojure.response$eval21276$fn__21277.invoke(response.clj:47)","compojure.response$eval21198$fn__21199$G__21189__21206.invoke(response.clj:7)","compojure.core$wrap_response$fn__21868.invoke(core.clj:158)","compojure.core$wrap_route_middleware$fn__21852.invoke(core.clj:128)","compojure.core$wrap_route_info$fn__21857.invoke(core.clj:137)","compojure.core$wrap_route_matches$fn__21861.invoke(core.clj:146)","compojure.core$routing$fn__21876.invoke(core.clj:185)","clojure.core$some.invokeStatic(core.clj:2592)","clojure.core$some.invoke(core.clj:2583)","compojure.core$routing.invokeStatic(core.clj:185)","compojure.core$routing.doInvoke(core.clj:182)","clojure.lang.RestFn.applyTo(RestFn.java:139)","clojure.core$apply.invokeStatic(core.clj:648)","clojure.core$apply.invoke(core.clj:641)","compojure.core$routes$fn__21880.invoke(core.clj:192)","compojure.core$routing$fn__21876.invoke(core.clj:185)","clojure.core$some.invokeStatic(core.clj:2592)","clojure.core$some.invoke(core.clj:2583)","compojure.core$routing.invokeStatic(core.clj:185)","compojure.core$routing.doInvoke(core.clj:182)","clojure.lang.RestFn.applyTo(RestFn.java:139)","clojure.core$apply.invokeStatic(core.clj:648)","clojure.core$apply.invoke(core.clj:641)","compojure.core$routes$fn__21880.invoke(core.clj:192)","compojure.core$routing$fn__21876.invoke(core.clj:185)","clojure.core$some.invokeStatic(core.clj:2592)","clojure.core$some.invoke(core.clj:2583)","compojure.core$routing.invokeStatic(core.clj:185)","compojure.core$routing.doInvoke(core.clj:182)","clojure.lang.RestFn.applyTo(RestFn.java:139)","clojure.core$apply.invokeStatic(core.clj:648)","clojure.core$apply.invoke(core.clj:641)","compojure.core$routes$fn__21880.invoke(core.clj:192)","ring.middleware.logstash$wrap_logstash$fn__23197.invoke(logstash.clj:110)","ring.middleware.keyword_params$wrap_keyword_params$fn__24059.invoke(keyword_params.clj:36)","ring.middleware.nested_params$wrap_nested_params$fn__24107.invoke(nested_params.clj:89)","ring.middleware.params$wrap_params$fn__24023.invoke(params.clj:67)","cayenne.api.route$wrap_cors$fn__24643.invoke(route.clj:101)","metrics.ring.expose$expose_metrics_as_json$fn__23583.invoke(expose.clj:94)","metrics.ring.instrument$instrument$fn__23599$fn__23600.invoke(instrument.clj:44)","metrics.ring.instrument.proxy$java.lang.Object$Callable$7da976d4.call(Unknown Source)","com.yammer.metrics.core.Timer.time(Timer.java:91)","metrics.ring.instrument$instrument$fn__23599.invoke(instrument.clj:43)","heartbeat.ring$wrap_heartbeat$fn__23285.invoke(ring.clj:18)","cayenne.api.conneg$wrap_accept$fn__19739.invoke(conneg.clj:53)","cayenne.api.route$wrap_exception_handler$fn__24648.invoke(route.clj:110)","cayenne.api.route$wrap_ignore_trailing_slash$fn__24656.invoke(route.clj:136)","org.httpkit.server.HttpHandler.run(RingHandler.java:91)","java.util.concurrent.Executors$RunnableAdapter.call(Executors.java:511)","java.util.concurrent.FutureTask
      
      
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

# replyr

Version: 0.5.2

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
      ==================================================
      downloaded 470 KB
      
      testthat results ================================================================
      OK: 109 SKIPPED: 0 FAILED: 7
      1. Error: it can recode data and demographics (@test_data_files.R#180) 
      2. Failure: nhanes_search on variables passes spot check (@test_search.R#13) 
      3. Failure: nhanes_search on variables passes spot check (@test_search.R#14) 
      4. Failure: nhanes_search on files passes spot check (@test_search.R#20) 
      5. Failure: nhanes_search on files passes spot check (@test_search.R#21) 
      6. Failure: fuzzy search works on files (@test_search.R#33) 
      7. Failure: fuzzy search works on files (@test_search.R#34) 
      
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
      OK: 168 SKIPPED: 1 FAILED: 15
      1. Error: check_response returns an error (@test-check_response.r#7) 
      2. Error: check_response returns the correct error messages (@test-check_response.r#26) 
      3. Error: gefs time and ensemble selection returns correct indices. (@test-gefs.R#25) 
      4. Error: gefs_variables returns characters. (@test-gefs.R#52) 
      5. Error: gefs_latitudes returns numeric. (@test-gefs.R#63) 
      6. Error: gefs_longitudes returns numeric. (@test-gefs.R#73) 
      7. Error: gefs_dimensions returns character list. (@test-gefs.R#83) 
      8. Error: gefs_dimension_values returns numeric array. (@test-gefs.R#93) 
      9. Error: ncdc returns the correct ... (@test-ncdc.r#8) 
      1. ...
      
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
      4. Failure: Test transformation from dataframe to h2o frame (@test_transforms.R#81) 
      ncol(mtcars_tbl) not equal to ncol(mtcars_hf).
      Modes: logical, numeric
      target is logical, current is numeric
      
      
      testthat results ================================================================
      OK: 20 SKIPPED: 0 FAILED: 4
      1. Error: Test transformation from h2o frame to data frame (@test_transforms.R#5) 
      2. Error: Test transformation of a spark data_frame of bools to an h2o frame of bools (@test_transforms.R#17) 
      3. Error: Test transformation of a spark data_frame of complex types to an h2o frame of complex types (@test_transforms.R#29) 
      4. Failure: Test transformation from dataframe to h2o frame (@test_transforms.R#81) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# RTCGA

Version: 1.6.0

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘RTCGA-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: boxplotTCGA
    > ### Title: Create Boxplots for TCGA Datasets
    > ### Aliases: boxplotTCGA
    > 
    > ### ** Examples
    > 
    > library(RTCGA.rnaseq)
    Error in library(RTCGA.rnaseq) : 
      there is no package called ‘RTCGA.rnaseq’
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(RTCGA)
      Welcome to the RTCGA (version: 1.6.0).
      > library(RTCGA.rnaseq)
      Error in library(RTCGA.rnaseq) : 
        there is no package called 'RTCGA.rnaseq'
      Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      ‘RTCGA.rnaseq’ ‘RTCGA.clinical’ ‘RTCGA.mutations’ ‘RTCGA.RPPA’
      ‘RTCGA.mRNA’ ‘RTCGA.miRNASeq’ ‘RTCGA.methylation’ ‘RTCGA.CNV’
      ‘RTCGA.PANCAN12’
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
    Packages unavailable to check Rd xrefs: ‘RTCGA.rnaseq’, ‘RTCGA.clinical’, ‘RTCGA.mutations’, ‘RTCGA.CNV’, ‘RTCGA.RPPA’, ‘RTCGA.mRNA’, ‘RTCGA.miRNASeq’, ‘RTCGA.methylation’
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

# SanFranBeachWater

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tibble’
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

Version: 0.1.4

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

Version: 0.5-3

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

Version: 1.0.1

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

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘ggeffects’
      All declared Imports should be used.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘plm’
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

# SpaDES

Version: 1.3.1

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      RMarkdown syntax allows R code, outputs, and figures to be rendered in the documm[19;1Hentation.
      
      For help writing in RMarkdown, see http://rmarkdown.rstudio.com/.
      
      [35m# Usage[0m[24;63H1,1[11CTop[1;1H[34h[?25h[24;1H[?1l>[?1049lVim: Error reading input, exiting...
      
      Vim: Finished.
      [24;1Htestthat results ================================================================
      OK: 955 SKIPPED: 15 FAILED: 1
      1. Failure: downloadModule downloads and unzips a single module (@test-downloadModule.R#26) 
      
      Error: testthat unit tests failed
      In addition: Warning message:
      no DISPLAY variable so Tk is not available 
      Execution halted
    ```

*   checking whether package ‘SpaDES’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: no DISPLAY variable so Tk is not available
    See ‘/home/muelleki/git/R/dplyr/revdep/checks/SpaDES/new/SpaDES.Rcheck/00install.out’ for details.
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘fastshp’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘rstudioapi’
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

Version: 0.2

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

Version: 2.5.7

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
      installed size is 66.0Mb
      sub-directories of 1Mb or more:
        R      1.1Mb
        data   6.4Mb
        doc   58.2Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      ‘limmamakeContrasts’
    TCGAanalyze_DEA_Affy: no visible global function definition for
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
    Undefined global functions or variables:
      c3net dCommSignif dNetInduce dNetPipeline knnmi.cross
      limmacontrasts.fit limmamakeContrasts minet portions value visNet
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

Version: 0.7.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 23 marked UTF-8 strings
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

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    7f2b8c974000-7f2b8cb74000 ---p 002ae000 08:01 790438                     /usr/lib/R/lib/libR.so
    7f2b8cb74000-7f2b8cb91000 r--p 002ae000 08:01 790438                     /usr/lib/R/lib/libR.so
    7f2b8cb91000-7f2b8cb9c000 rw-p 002cb000 08:01 790438                     /usr/lib/R/lib/libR.so
    7f2b8cb9c000-7f2b8cced000 rw-p 00000000 00:00 0 
    7f2b8cced000-7f2b8cd13000 r-xp 00000000 08:01 16865                      /lib/x86_64-linux-gnu/ld-2.23.so
    7f2b8cd1d000-7f2b8cef5000 rw-p 00000000 00:00 0 
    7f2b8cef6000-7f2b8cef7000 rw-p 00000000 00:00 0 
    7f2b8cef7000-7f2b8cf06000 r-xp 00000000 08:21 10758034                   /home/muelleki/git/R/dplyr/revdep/library/waccR/htmltools/libs/htmltools.so
    7f2b8cf06000-7f2b8cf07000 ---p 0000f000 08:21 10758034                   /home/muelleki/git/R/dplyr/revdep/library/waccR/htmltools/libs/htmltools.so
    7f2b8cf07000-7f2b8cf08000 r--p 0000f000 08:21 10758034                   /home/muelleki/git/R/dplyr/revdep/library/waccR/htmltools/libs/htmltools.so
    7f2b8cf08000-7f2b8cf09000 rw-p 00010000 08:21 10758034                   /home/muelleki/git/R/dplyr/revdep/library/waccR/htmltools/libs/htmltools.so
    7f2b8cf09000-7f2b8cf10000 r--s 00000000 08:01 27320                      /usr/lib/x86_64-linux-gnu/gconv/gconv-modules.cache
    7f2b8cf10000-7f2b8cf12000 rw-p 00000000 00:00 0 
    7f2b8cf12000-7f2b8cf13000 r--p 00025000 08:01 16865                      /lib/x86_64-linux-gnu/ld-2.23.so
    7f2b8cf13000-7f2b8cf14000 rw-p 00026000 08:01 16865                      /lib/x86_64-linux-gnu/ld-2.23.so
    7f2b8cf14000-7f2b8cf15000 rw-p 00000000 00:00 0 
    7fff7fb1d000-7fff7fb95000 rw-p 00000000 00:00 0                          [stack]
    7fff7fbdc000-7fff7fbde000 r--p 00000000 00:00 0                          [vvar]
    7fff7fbde000-7fff7fbe0000 r-xp 00000000 00:00 0                          [vdso]
    ffffffffff600000-ffffffffff601000 r-xp 00000000 00:00 0                  [vsyscall]
    Aborted (core dumped)
    ```

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
      Timeout was reached: Connection timed out after 10001 milliseconds
    http://stats.grok.se/json/en/201510/Main_page
    
    data from server was: Error in curl::curl_fetch_memory(url, handle = handle) : 
      Timeout was reached: Connection timed out after 10001 milliseconds
    
    
    Error in curl::curl_fetch_memory(url, handle = handle) : 
      Timeout was reached: Connection timed out after 10000 milliseconds
    http://stats.grok.se/json/en/201511/Main_page
    
    data from server was: Error in curl::curl_fetch_memory(url, handle = handle) : 
      Timeout was reached: Connection timed out after 10000 milliseconds
    
    
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

# WRTDStidal

Version: 1.1.0

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘WRTDStidal-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: dynaplot
    > ### Title: Plot model response to salinity or flow as a lineplot for all
    > ###   months
    > ### Aliases: dynaplot dynaplot.tidal dynaplot.tidalmean
    > 
    > ### ** Examples
    > 
    > 
    > # load a fitted tidal object
    > data(tidfit)
    > 
    > # plot using defaults, 
    > # defaults to the fiftieth quantile for all years
    > dynaplot(tidfit)
    Error in ncol(to_plo) : could not find function "ncol"
    Calls: dynaplot ... <Anonymous> -> map_if -> map -> lapply -> FUN -> .Call
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

*   checking examples ... ERROR
    ```
    ...
    > ## calculate for one Tpr curve at a Ppr
    > z.DranchukPurvisRobinson(pres.pr = 1.5, temp.pr = 2.0)
    [1] 0.9546382
    > 
    > ## For vectors of Ppr and Tpr:
    > ppr <- c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5)
    > tpr <- c(1.3, 1.5, 1.7, 2)
    > z.DranchukPurvisRobinson(pres.pr = ppr, temp.pr = tpr)
              0.5       1.5       2.5       3.5       4.5       5.5       6.5
    1.3 0.9197157 0.7525940 0.6366665 0.6337883 0.6891997 0.7650171 0.8480804
    1.5 0.9504834 0.8583491 0.7926325 0.7720713 0.7914322 0.8348883 0.8915239
    1.7 0.9677844 0.9121791 0.8752677 0.8630002 0.8743271 0.9033216 0.9440582
    2   0.9822021 0.9546382 0.9399310 0.9391490 0.9512966 0.9740256 1.0047347
    > 
    > ## create a matrix of z values
    > tpr2 <- c(1.05, 1.1, 1.2, 1.3)
    > ppr2 <- c(0.5, 1.0, 1.5, 2, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0, 5.5, 6.0, 6.5)
    > sk_corr_2 <- createTidyFromMatrix(ppr2, tpr2, correlation = "DPR")
    Error in ncol(sk_df) : could not find function "ncol"
    Calls: createTidyFromMatrix ... <Anonymous> -> map_if -> map -> lapply -> FUN -> .Call
    Execution halted
    ```

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

