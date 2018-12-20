# abjutils

Version: 0.2.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘httr’ ‘progress’
      All declared Imports should be used.
    ```

# adegenet

Version: 2.1.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.7Mb
      sub-directories of 1Mb or more:
        data    1.3Mb
        files   1.7Mb
        R       3.1Mb
    ```

# admixturegraph

Version: 1.0.2

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.1Mb
      sub-directories of 1Mb or more:
        data   2.0Mb
        R      3.1Mb
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

Version: 0.22-1

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
      ‘assertthat’ ‘DBI’ ‘tibble’
      All declared Imports should be used.
    ```

# ahpsurvey

Version: 0.3.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘knitr’ ‘tidyr’
      All declared Imports should be used.
    ```

# aire.zmvm

Version: 0.8.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 52 marked UTF-8 strings
    ```

# airportr

Version: 0.1.2

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 676 marked UTF-8 strings
    ```

# alphavantager

Version: 0.1.0

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      2: stop(content, call. = F) at /Users/romain/git/release/dplyr/revdep/checks.noindex/alphavantager/new/alphavantager.Rcheck/00_pkg_src/alphavantager/R/av_get.R:103
      
      ── 3. Error: call Technical Indicators (@test_av_get.R#57)  ────────────────────
      Thank you for using Alpha Vantage! Our standard API call frequency is 5 calls per minute and 500 calls per day. Please visit https://www.alphavantage.co/premium/ if you would like to target a higher API call frequency.. API parameters used: symbol=MSFT, function=SMA, interval=monthly, time_period=60, series_type=close, apikey=HIDDEN_FOR_YOUR_SAFETY
      1: av_get(symbol, av_fun, interval = interval, time_period = time_period, series_type = series_type) at testthat/test_av_get.R:57
      2: stop(content, call. = F) at /Users/romain/git/release/dplyr/revdep/checks.noindex/alphavantager/new/alphavantager.Rcheck/00_pkg_src/alphavantager/R/av_get.R:103
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 5 SKIPPED: 0 FAILED: 3
      1. Error: call TIMES_SERIES_INTRADAY (@test_av_get.R#13) 
      2. Error: call SECTOR (@test_av_get.R#38) 
      3. Error: call Technical Indicators (@test_av_get.R#57) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘devtools’
      All declared Imports should be used.
    ```

# alternativeSplicingEvents.hg19

Version: 1.0.1

## In both

*   checking extension type ... ERROR
    ```
    Extensions with Type ‘Annotation package’ cannot be checked.
    ```

# ameco

Version: 0.2.9

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 16.2Mb
      sub-directories of 1Mb or more:
        data  16.1Mb
    ```

# amplican

Version: 1.2.1

## In both

*   checking whether package ‘amplican’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘IRanges’ was built under R version 3.5.1
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/amplican/new/amplican.Rcheck/00install.out’ for details.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 13.8Mb
      sub-directories of 1Mb or more:
        doc  12.5Mb
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘CrispRVariants’
    ```

# AMR

Version: 0.5.0

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > 
    > ### ** Examples
    > 
    > mic_data <- as.mic(c(">=32", "1.0", "1", "1.00", 8, "<=0.128", "8", "16", "16"))
    Warning: 1 results truncated (11%) that were invalid MICs: "1.00"
    > is.mic(mic_data)
    [1] TRUE
    > 
    > # this can also coerce combined MIC/RSI values:
    > as.mic("<=0.002; S") # will return <=0.002
    Class 'mic'
    [1] <=0.002
    > 
    > plot(mic_data)
    Warning: Factor `mic` contains implicit NA, consider using `forcats::fct_explicit_na`
    > barplot(mic_data)
    Warning: Factor `mic` contains implicit NA, consider using `forcats::fct_explicit_na`
    > freq(mic_data)
    Error in n() : could not find function "n"
    Calls: freq ... <Anonymous> -> summarise -> summarise.tbl_df -> summarise_impl
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      AIC: 4
      
      Number of Fisher Scoring iterations: 24
      
      ── 1. Failure: prediction of rsi works (@test-portion.R#174)  ──────────────────
      `resistance_predict(...)` did not throw an error.
      
      Class 'rsi'
      [1] S I R
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 448 SKIPPED: 1 FAILED: 1
      1. Failure: prediction of rsi works (@test-portion.R#174) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# amt

Version: 0.0.5.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘magrittr’ ‘Rcpp’
      All declared Imports should be used.
    ```

# annotatr

Version: 1.6.0

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
         } else {
             as.data.frame(t(utils::combn(sort(as.character(.$annot.type)), 2)), stringsAsFactors = FALSE)
         }) at /Users/romain/git/tidyverse/dplyr/R/do.r:91
      4: eval_tidy(args[[j]], mask) at /Users/romain/git/tidyverse/dplyr/R/grouped-df.r:331
      5: as.data.frame(t(utils::combn(sort(as.character(.$annot.type)), 2)), stringsAsFactors = FALSE) at /Users/romain/git/release/dplyr/revdep/checks.noindex/annotatr/new/annotatr.Rcheck/00_pkg_src/annotatr/R/visualize.R:473
      6: t(utils::combn(sort(as.character(.$annot.type)), 2))
      7: utils::combn(sort(as.character(.$annot.type)), 2)
      8: stop("n < m", domain = NA)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 70 SKIPPED: 0 FAILED: 1
      1. Error: Test plot_numerical_coannotations() (@test_7_visualize.R#163) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘annotatr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: build_annotations
    > ### Title: A function to build annotations from TxDb.* and AnnotationHub
    > ###   resources
    > ### Aliases: build_annotations
    > 
    > ### ** Examples
    > 
    > # Example with hg19 gene promoters
    > annots = c('hg19_genes_promoters')
    > annots_gr = build_annotations(genome = 'hg19', annotations = annots)
    Error in build_gene_annots(genome = genome, annotations = gene_annotations) : 
      The package TxDb.Hsapiens.UCSC.hg19.knownGene is not installed, please install it via Bioconductor.
    Calls: build_annotations
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    snapshotDate(): 2018-04-30
    Building annotation Gm12878 from AnnotationHub resource AH23256 ...
    require("rtracklayer")
    Warning: package 'rtracklayer' was built under R version 3.5.1
    Warning: package 'GenomicRanges' was built under R version 3.5.1
    Warning: package 'IRanges' was built under R version 3.5.1
    downloading 0 resources
    loading from cache 
        '/Users/romain//.AnnotationHub/28684'
    Quitting from lines 153-170 (annotatr-vignette.Rmd) 
    Error: processing vignette 'annotatr-vignette.Rmd' failed with diagnostics:
    The package TxDb.Hsapiens.UCSC.hg19.knownGene is not installed, please install it via Bioconductor.
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      ‘org.Dm.eg.db’ ‘org.Gg.eg.db’ ‘org.Hs.eg.db’ ‘org.Mm.eg.db’
      ‘org.Rn.eg.db’ ‘TxDb.Dmelanogaster.UCSC.dm3.ensGene’
      ‘TxDb.Dmelanogaster.UCSC.dm6.ensGene’
      ‘TxDb.Ggallus.UCSC.galGal5.refGene’
      ‘TxDb.Hsapiens.UCSC.hg19.knownGene’
      ‘TxDb.Hsapiens.UCSC.hg38.knownGene’
      ‘TxDb.Mmusculus.UCSC.mm9.knownGene’
      ‘TxDb.Mmusculus.UCSC.mm10.knownGene’
      ‘TxDb.Rnorvegicus.UCSC.rn4.ensGene’
      ‘TxDb.Rnorvegicus.UCSC.rn5.refGene’
      ‘TxDb.Rnorvegicus.UCSC.rn6.refGene’
    ```

*   checking R code for possible problems ... NOTE
    ```
    plot_coannotations: no visible binding for global variable ‘.’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/annotatr/new/annotatr.Rcheck/00_pkg_src/annotatr/R/visualize.R:176-178)
    plot_numerical_coannotations: no visible binding for global variable
      ‘.’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/annotatr/new/annotatr.Rcheck/00_pkg_src/annotatr/R/visualize.R:463-480)
    plot_numerical_coannotations: no visible binding for global variable
      ‘.’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/annotatr/new/annotatr.Rcheck/00_pkg_src/annotatr/R/visualize.R:466-471)
    plot_numerical_coannotations: no visible binding for global variable
      ‘.’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/annotatr/new/annotatr.Rcheck/00_pkg_src/annotatr/R/visualize.R:473-478)
    Undefined global functions or variables:
      .
    ```

# anomalize

Version: 0.1.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.5Mb
      sub-directories of 1Mb or more:
        help   4.7Mb
    ```

# anomalyDetection

Version: 0.2.5

## In both

*   checking whether package ‘anomalyDetection’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/anomalyDetection/new/anomalyDetection.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘anomalyDetection’ ...
** package ‘anomalyDetection’ successfully unpacked and MD5 sums checked
** libs
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romain/git/release/dplyr/revdep/library.noindex/anomalyDetection/Rcpp/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/anomalyDetection/RcppArmadillo/include" -I/usr/local/include  -fopenmp  -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘anomalyDetection’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/anomalyDetection/new/anomalyDetection.Rcheck/anomalyDetection’

```
### CRAN

```
* installing *source* package ‘anomalyDetection’ ...
** package ‘anomalyDetection’ successfully unpacked and MD5 sums checked
** libs
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romain/git/release/dplyr/revdep/library.noindex/anomalyDetection/Rcpp/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/anomalyDetection/RcppArmadillo/include" -I/usr/local/include  -fopenmp  -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘anomalyDetection’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/anomalyDetection/old/anomalyDetection.Rcheck/anomalyDetection’

```
# anyflights

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tibble’
      All declared Imports should be used.
    ```

# apa

Version: 0.3.0

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      1: anova_apa(ezANOVA(data, dv = rt, wid = subnum, within = c(cue, flank), between = group, 
             detailed = TRUE), print = FALSE) at testthat/test-anova-apa.R:18
      2: ezANOVA(data, dv = rt, wid = subnum, within = c(cue, flank), between = group, detailed = TRUE) at /Users/romain/git/release/dplyr/revdep/checks.noindex/apa/new/apa.Rcheck/00_pkg_src/apa/R/anova_apa.R:58
      3: ezANOVA_main(data = data, dv = dv, wid = wid, within = within, within_full = within_full, 
             within_covariates = within_covariates, between = between, between_covariates = between_covariates, 
             diff = diff, reverse_diff = reverse_diff, type = type, return_aov = return_aov, 
             white.adjust = white.adjust)
      4: stop("One or more cells returned NA when aggregated to a mean. Check your data.")
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 113 SKIPPED: 0 FAILED: 1
      1. Error: Formal structure for anova_apa output (@test-anova-apa.R#18) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# archivist

Version: 2.3.1

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘archivist.github’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘archivist.github’
    ```

# arena2r

Version: 1.0.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘purrr’ ‘shinyBS’ ‘shinydashboard’ ‘shinyjs’
      All declared Imports should be used.
    ```

# auctestr

Version: 1.0.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tidyr’
      All declared Imports should be used.
    ```

# augmentedRCBD

Version: 0.1.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘agricolae’
    ```

# auk

Version: 0.3.1

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 552 marked UTF-8 strings
    ```

# autocogs

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘broom’ ‘diptest’ ‘ggplot2’ ‘hexbin’ ‘MASS’ ‘moments’
      All declared Imports should be used.
    ```

# BALCONY

Version: 0.2.8

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘base’
      All declared Imports should be used.
    ```

# BANEScarparkinglite

Version: 0.1.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘zoo’
      All declared Imports should be used.
    ```

# banR

Version: 0.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘stringr’
      All declared Imports should be used.
    ```

# banter

Version: 0.9.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘ranger’
      All declared Imports should be used.
    ```

# basecallQC

Version: 1.4.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        doc       1.8Mb
        extdata   2.8Mb
    ```

# basictabler

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# bayesdfa

Version: 0.1.1

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Chain 2: Iteration:   1 / 500 [  0%]  (Warmup)
    Chain 2: Iteration:  50 / 500 [ 10%]  (Warmup)
    Chain 2: Iteration: 100 / 500 [ 20%]  (Warmup)
    Chain 2: Iteration: 150 / 500 [ 30%]  (Warmup)
    Chain 2: Iteration: 200 / 500 [ 40%]  (Warmup)
    Chain 2: Iteration: 250 / 500 [ 50%]  (Warmup)
    Chain 2: Iteration: 251 / 500 [ 50%]  (Sampling)
    Chain 2: Iteration: 300 / 500 [ 60%]  (Sampling)
    Chain 2: Iteration: 350 / 500 [ 70%]  (Sampling)
    Chain 2: Iteration: 400 / 500 [ 80%]  (Sampling)
    Chain 2: Iteration: 450 / 500 [ 90%]  (Sampling)
    Chain 2: Iteration: 500 / 500 [100%]  (Sampling)
    Chain 2: 
    Chain 2:  Elapsed Time: 0.836811 seconds (Warm-up)
    Chain 2:                0.604194 seconds (Sampling)
    Chain 2:                1.44101 seconds (Total)
    Chain 2: 
    Error in if ((sum((-1 * pred1_loadings - pred0_loadings)^2) + sum((-1 *  : 
      missing value where TRUE/FALSE needed
    Calls: fit_dfa -> invert_chains -> find_inverted_chains
    Execution halted
    ```

## In both

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# bayesplot

Version: 1.6.0

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > # bar plots
    > f <- function(N) {
    +   sample(1:4, size = N, replace = TRUE, prob = c(0.25, 0.4, 0.1, 0.25))
    + }
    > y <- f(100)
    > yrep <- t(replicate(500, f(100)))
    > dim(yrep)
    [1] 500 100
    > group <- gl(2, 50, length = 100, labels = c("GroupA", "GroupB"))
    > 
    > color_scheme_set("mix-pink-blue")
    > ppc_bars(y, yrep)
    > 
    > # split by group, change interval width, and display proportion
    > # instead of count on y-axis
    > color_scheme_set("mix-blue-pink")
    > ppc_bars_grouped(y, yrep, group, prob = 0.5, freq = FALSE)
    Error in quantile.default(x, probs[1]) : 
      missing values and NaN's not allowed if 'na.rm' is FALSE
    Calls: ppc_bars_grouped ... summarise_impl -> lo -> quantile -> quantile.default
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── 6. Failure: ppc_intervals_data does math correctly (@test-ppc-intervals.R#80)
      d$hh not equal to c(19, 19, 9, 9).
      Lengths differ: 8 is not 4
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 969 SKIPPED: 18 FAILED: 6
      1. Error: freq argument to ppc_bars works (@test-ppc-discrete.R#25) 
      2. Failure: ppc_intervals_data does math correctly (@test-ppc-intervals.R#76) 
      3. Failure: ppc_intervals_data does math correctly (@test-ppc-intervals.R#77) 
      4. Failure: ppc_intervals_data does math correctly (@test-ppc-intervals.R#78) 
      5. Failure: ppc_intervals_data does math correctly (@test-ppc-intervals.R#79) 
      6. Failure: ppc_intervals_data does math correctly (@test-ppc-intervals.R#80) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.1Mb
      sub-directories of 1Mb or more:
        doc   4.0Mb
        R     2.5Mb
    ```

# baystability

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘ggfortify’ ‘ggplot2’ ‘matrixStats’ ‘reshape2’ ‘scales’
      All declared Imports should be used.
    ```

# BgeeDB

Version: 2.6.2

## In both

*   checking whether package ‘BgeeDB’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/BgeeDB/new/BgeeDB.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘BgeeDB’ ...
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Warning: package ‘graph’ was built under R version 3.5.1
Error : package ‘GO.db’ required by ‘topGO’ could not be found
ERROR: lazy loading failed for package ‘BgeeDB’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/BgeeDB/new/BgeeDB.Rcheck/BgeeDB’

```
### CRAN

```
* installing *source* package ‘BgeeDB’ ...
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Warning: package ‘graph’ was built under R version 3.5.1
Error : package ‘GO.db’ required by ‘topGO’ could not be found
ERROR: lazy loading failed for package ‘BgeeDB’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/BgeeDB/old/BgeeDB.Rcheck/BgeeDB’

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

# binneR

Version: 2.0.5

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      15: eval(quote(`_fseq`(`_lhs`)), env, env)
      16: eval(quote(`_fseq`(`_lhs`)), env, env)
      17: `_fseq`(`_lhs`)
      18: freduce(value, `_function_list`)
      19: function_list[[i]](value)
      20: summarise(., Count = n())
      21: summarise.tbl_df(., Count = n()) at /Users/romain/git/tidyverse/dplyr/R/manip.r:255
      22: summarise_impl(.data, dots) at /Users/romain/git/tidyverse/dplyr/R/tbl-df.r:102
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 9 SKIPPED: 0 FAILED: 1
      1. Error: (unknown) (@test-binneRlyse.R#17) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘metaboData’
    ```

# biobroom

Version: 1.12.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
    The following object is masked from 'package:dplyr':
    
        count
    
    Loading required package: BiocParallel
    
    Attaching package: 'DelayedArray'
    
    The following objects are masked from 'package:matrixStats':
    
        colMaxs, colMins, colRanges, rowMaxs, rowMins, rowRanges
    
    The following objects are masked from 'package:base':
    
        aperm, apply
    
    Quitting from lines 134-139 (biobroom_vignette.Rmd) 
    Error: processing vignette 'biobroom_vignette.Rmd' failed with diagnostics:
    there is no package called 'airway'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘airway’
    ```

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
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/biobroom/new/biobroom.Rcheck/00_pkg_src/biobroom/R/qvalue_tidiers.R:65-66)
    tidy.RangedSummarizedExperiment: no visible binding for global variable
      ‘value’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/biobroom/new/biobroom.Rcheck/00_pkg_src/biobroom/R/SummarizedExperiment_tidiers.R:43-45)
    tidy.RangedSummarizedExperiment: no visible binding for global variable
      ‘gene’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/biobroom/new/biobroom.Rcheck/00_pkg_src/biobroom/R/SummarizedExperiment_tidiers.R:43-45)
    tidy.RangedSummarizedExperiment: no visible global function definition
      for ‘colData’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/biobroom/new/biobroom.Rcheck/00_pkg_src/biobroom/R/SummarizedExperiment_tidiers.R:48)
    Undefined global functions or variables:
      . calcNormFactors colData counts design DGEList end estimate
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

Version: 1.8.0

## In both

*   checking package dependencies ... ERROR
    ```
    Packages required but not available: ‘org.Hs.eg.db’ ‘reactome.db’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# BiocOncoTK

Version: 1.0.3

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 18-30 (BiocOncoTK.Rmd) 
    Error: processing vignette 'BiocOncoTK.Rmd' failed with diagnostics:
    there is no package called 'org.Hs.eg.db'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      ‘org.Hs.eg.db’ ‘TxDb.Hsapiens.UCSC.hg19.knownGene’
      ‘TxDb.Hsapiens.UCSC.hg18.knownGene’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.0Mb
      sub-directories of 1Mb or more:
        data   4.0Mb
        doc    1.7Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    rainfall: no visible global function definition for ‘ylab’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/BiocOncoTK/new/BiocOncoTK.Rcheck/00_pkg_src/BiocOncoTK/R/rainfall3.R:164-168)
    rainfall: no visible global function definition for ‘xlab’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/BiocOncoTK/new/BiocOncoTK.Rcheck/00_pkg_src/BiocOncoTK/R/rainfall3.R:164-168)
    rainfall: no visible global function definition for ‘geom_vline’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/BiocOncoTK/new/BiocOncoTK.Rcheck/00_pkg_src/BiocOncoTK/R/rainfall3.R:169-171)
    rainfall: no visible global function definition for ‘aes’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/BiocOncoTK/new/BiocOncoTK.Rcheck/00_pkg_src/BiocOncoTK/R/rainfall3.R:169-171)
    rainfall: no visible global function definition for
      ‘scale_x_continuous’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/BiocOncoTK/new/BiocOncoTK.Rcheck/00_pkg_src/BiocOncoTK/R/rainfall3.R:169-171)
    rainfall: no visible global function definition for ‘ggtitle’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/BiocOncoTK/new/BiocOncoTK.Rcheck/00_pkg_src/BiocOncoTK/R/rainfall3.R:172)
    rainfall: no visible global function definition for ‘geom_text’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/BiocOncoTK/new/BiocOncoTK.Rcheck/00_pkg_src/BiocOncoTK/R/rainfall3.R:173-174)
    rainfall: no visible global function definition for ‘aes’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/BiocOncoTK/new/BiocOncoTK.Rcheck/00_pkg_src/BiocOncoTK/R/rainfall3.R:173-174)
    Undefined global functions or variables:
      aes BiocFileCache element_blank genome geom_point geom_text
      geom_vline ggplot ggtitle scale_x_continuous seqlengths theme xlab
      ylab
    ```

# biotmle

Version: 1.4.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    .biotmle: no visible global function definition for ‘new’
    Undefined global functions or variables:
      new
    Consider adding
      importFrom("methods", "new")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
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
      ‘glmnet’ ‘gtools’ ‘knitr’ ‘nnet’ ‘parallel’ ‘reshape’ ‘rJava’
      ‘rmarkdown’ ‘shinyjs’
      All declared Imports should be used.
    Missing or unexported object: ‘xgboost::predict’
    ```

# BloodCancerMultiOmics2017

Version: 1.0.2

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
        anyMissing
    
    The following object is masked from 'package:Biobase':
    
        anyMissing
    
    
    Attaching package: 'genefilter'
    
    The following object is masked from 'package:readr':
    
        spec
    
    The following objects are masked from 'package:matrixStats':
    
        rowSds, rowVars
    
    Quitting from lines 46-92 (BloodCancerMultiOmics2017.Rmd) 
    Error: processing vignette 'BloodCancerMultiOmics2017.Rmd' failed with diagnostics:
    there is no package called 'ggtern'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking: ‘ggtern’ ‘org.Hs.eg.db’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 115.7Mb
      sub-directories of 1Mb or more:
        data     80.0Mb
        doc      26.5Mb
        extdata   8.5Mb
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘vsn’
    ```

# blorr

Version: 0.2.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘lmtest’
    ```

# bmlm

Version: 1.3.9

## In both

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
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

# bootnet

Version: 1.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘psych’
      All declared Imports should be used.
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

# bpbounds

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘methods’
      All declared Imports should be used.
    ```

# BrailleR

Version: 0.29.1

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘installr’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        doc     1.3Mb
        R       2.0Mb
        Sound   1.0Mb
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

Version: 0.4.6

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘breathteststan’
    ```

# breathteststan

Version: 0.4.7

## In both

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# broom.mixed

Version: 0.2.3

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘glmmADMB’
    ```

# bsam

Version: 1.1.2

## In both

*   checking whether package ‘bsam’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/bsam/new/bsam.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘bsam’ ...
** package ‘bsam’ successfully unpacked and MD5 sums checked
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rjags’:
 .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/romain/git/release/dplyr/revdep/library.noindex/bsam/rjags/libs/rjags.so':
  dlopen(/Users/romain/git/release/dplyr/revdep/library.noindex/bsam/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/romain/git/release/dplyr/revdep/library.noindex/bsam/rjags/libs/rjags.so
  Reason: image not found
Error : package ‘rjags’ could not be loaded
ERROR: lazy loading failed for package ‘bsam’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/bsam/new/bsam.Rcheck/bsam’

```
### CRAN

```
* installing *source* package ‘bsam’ ...
** package ‘bsam’ successfully unpacked and MD5 sums checked
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rjags’:
 .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/romain/git/release/dplyr/revdep/library.noindex/bsam/rjags/libs/rjags.so':
  dlopen(/Users/romain/git/release/dplyr/revdep/library.noindex/bsam/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/romain/git/release/dplyr/revdep/library.noindex/bsam/rjags/libs/rjags.so
  Reason: image not found
Error : package ‘rjags’ could not be loaded
ERROR: lazy loading failed for package ‘bsam’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/bsam/old/bsam.Rcheck/bsam’

```
# BubbleTree

Version: 2.10.0

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > # load sample files
    > load(system.file("data", "cnv.gr.rda", package="BubbleTree"))
    > load(system.file("data", "snp.gr.rda", package="BubbleTree"))
    > 
    > # load annotations
    > load(system.file("data", "centromere.dat.rda", package="BubbleTree"))
    > load(system.file("data", "cyto.gr.rda", package="BubbleTree"))
    > load(system.file("data", "cancer.genes.minus2.rda", package="BubbleTree"))
    > load(system.file("data", "vol.genes.rda", package="BubbleTree"))
    > load(system.file("data", "gene.uni.clean.gr.rda", package="BubbleTree"))
    > 
    > 
    > # initialize RBD object
    > r <- new("RBD", unimodal.kurtosis=-0.1)
    > 
    > # create new RBD object with GenomicRanges objects for SNPs and CNVs
    > rbd <- makeRBD(r, snp.gr, cnv.gr)
    Error in .Call2("solve_user_SEW0", start, end, width, PACKAGE = "IRanges") : 
      solving row 2: range cannot be determined from the supplied arguments (too many NAs)
    Calls: makeRBD ... GRanges -> new_GRanges -> IRanges -> solveUserSEW0 -> .Call2
    Execution halted
    ```

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
    Quitting from lines 126-206 (BubbleTree-vignette.Rmd) 
    Error: processing vignette 'BubbleTree-vignette.Rmd' failed with diagnostics:
    solving row 2: range cannot be determined from the supplied arguments (too many NAs)
    Execution halted
    ```

## In both

*   checking whether package ‘BubbleTree’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘IRanges’ was built under R version 3.5.1
      Warning: package ‘GenomicRanges’ was built under R version 3.5.1
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/BubbleTree/new/BubbleTree.Rcheck/00install.out’ for details.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 29.4Mb
      sub-directories of 1Mb or more:
        data  23.4Mb
        doc    5.3Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    annoByOverlap,Annotate: no visible binding for global variable
      'queryHits'
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/BubbleTree/new/BubbleTree.Rcheck/00_pkg_src/BubbleTree/R/Annotate.R:107)
    Undefined global functions or variables:
      queryHits
    ```

# bupaR

Version: 0.4.1

## Newly broken

*   checking S3 generic/method consistency ... WARNING
    ```
    sample_n:
      function(tbl, size, replace, weight, .env, ...)
    sample_n.eventlog:
      function(tbl, size, replace, weight, .env)
    
    sample_n:
      function(tbl, size, replace, weight, .env, ...)
    sample_n.grouped_eventlog:
      function(tbl, size, replace, weight, .env)
    
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

# c14bazAAR

Version: 1.0.2

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 76 marked UTF-8 strings
    ```

# caffsim

Version: 0.2.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘markdown’
      All declared Imports should be used.
    ```

# capm

Version: 0.13.9

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 59 marked UTF-8 strings
    ```

# CARBayesST

Version: 3.0

## In both

*   checking re-building of vignette outputs ... NOTE
    ```
    ...
    
    The following object is masked _by_ ‘.GlobalEnv’:
    
        x
    
    Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
      Running 'texi2dvi' on 'CARBayesST.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `multirow.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    
    ! Emergency stop.
    <read *> 
             
    l.13 \usepackage
                    {multicol}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    Calls: buildVignettes -> texi2pdf -> texi2dvi
    Execution halted
    ```

# caret

Version: 6.0-81

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.4Mb
      sub-directories of 1Mb or more:
        data     1.5Mb
        models   2.4Mb
        R        4.1Mb
    ```

# carpenter

Version: 0.2.1

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      11: eval(quote(`_fseq`(`_lhs`)), env, env)
      12: `_fseq`(`_lhs`)
      13: freduce(value, `_function_list`)
      14: withVisible(function_list[[k]](value))
      15: function_list[[k]](value)
      16: dplyr::mutate_(., id = lazyeval::interp("1:n()"))
      17: mutate_.tbl_df(., id = lazyeval::interp("1:n()")) at /Users/romain/git/tidyverse/dplyr/R/manip.r:425
      18: mutate_impl(.data, dots) at /Users/romain/git/tidyverse/dplyr/R/tbl-df.r:96
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 19 SKIPPED: 0 FAILED: 1
      1. Error: build_table outputs correct information (@test-build_tables.R#21) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# cartools

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘animation’ ‘devtools’ ‘gapminder’ ‘knitr’ ‘rlist’ ‘rmarkdown’
      ‘roxygen2’ ‘sde’ ‘shiny’ ‘tidyverse’ ‘usethis’ ‘utils’
      All declared Imports should be used.
    ```

# CaseBasedReasoning

Version: 0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘cowplot’ ‘dplyr’ ‘ranger’ ‘Rcpp’ ‘rms’ ‘survival’ ‘tidyverse’
      All declared Imports should be used.
    ```

# CATALYST

Version: 1.4.2

## In both

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .travis.yml
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 10.6Mb
      sub-directories of 1Mb or more:
        data   3.1Mb
        doc    5.1Mb
        R      2.1Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    plotDiffHeatmap,matrix-SummarizedExperiment: no visible binding for
      global variable ‘cluster_id’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/CATALYST/new/CATALYST.Rcheck/00_pkg_src/CATALYST/R/plotDiffHeatmap.R:136)
    plotDiffHeatmap,matrix-SummarizedExperiment: no visible binding for
      global variable ‘sample_id’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/CATALYST/new/CATALYST.Rcheck/00_pkg_src/CATALYST/R/plotDiffHeatmap.R:136)
    Undefined global functions or variables:
      cluster_id sample_id
    ```

# catenary

Version: 1.1.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tidyverse’
      All declared Imports should be used.
    ```

# causaldrf

Version: 0.3

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
      Running 'texi2dvi' on 'Using_causaldrf.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `utf8x.def' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: def)
    
    ! Emergency stop.
    <read *> 
             
    l.165 \endinput
                   ^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    Calls: buildVignettes -> texi2pdf -> texi2dvi
    Execution halted
    ```

# CausalImpact

Version: 1.2.3

## In both

*   checking R code for possible problems ... NOTE
    ```
    ConstructModel: warning in AddDynamicRegression(ss, formula, data =
      data, sigma.mean.prior = sigma.mean.prior): partial argument match of
      'sigma.mean.prior' to 'sigma.mean.prior.DEPRECATED'
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/CausalImpact/new/CausalImpact.Rcheck/00_pkg_src/CausalImpact/R/impact_model.R:232-233)
    ```

# cdcfluview

Version: 0.7.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘units’
      All declared Imports should be used.
    ```

# CDECRetrieve

Version: 0.1.2

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘CDECRetrieve-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: cdec_rt
    > ### Title: Get a rating table
    > ### Aliases: cdec_rt
    > 
    > ### ** Examples
    > 
    > cdec_rt("abj") # get the stage to rating curve for ABJ
    Error in open.connection(x, "rb") : 
      Timeout was reached: Connection timed out after 10001 milliseconds
    Calls: cdec_rt ... %in% -> cdec_rt_list -> <Anonymous> -> read_html.default
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 36-45 (advanced-queries.Rmd) 
    Error: processing vignette 'advanced-queries.Rmd' failed with diagnostics:
    cannot open URL 'http://cdec.water.ca.gov/cgi-progs/querySHEF?station_id=kwk&sensor_num=25&dur_code=h&start_date=2018-12-17&end_date=2018-12-20&data_wish=Download+SHEF+Data+Now'
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘lazyeval’ ‘purrr’ ‘roxygen2’
      All declared Imports should be used.
    ```

# cellscape

Version: 1.4.0

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

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘plyr’
      All declared Imports should be used.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/cellscape/new/cellscape.Rcheck/00_pkg_src/cellscape/R/cellscape.R:1134)
    getMutOrder: no visible global function definition for ‘coef’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/cellscape/new/cellscape.Rcheck/00_pkg_src/cellscape/R/cellscape.R:1135)
    getTargetedHeatmapForEachSC: no visible binding for global variable
      ‘single_cell_id’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/cellscape/new/cellscape.Rcheck/00_pkg_src/cellscape/R/cellscape.R:1156)
    getTargetedHeatmapForEachSC: no visible binding for global variable
      ‘chr’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/cellscape/new/cellscape.Rcheck/00_pkg_src/cellscape/R/cellscape.R:1156)
    getTargetedHeatmapForEachSC: no visible binding for global variable
      ‘coord’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/cellscape/new/cellscape.Rcheck/00_pkg_src/cellscape/R/cellscape.R:1156)
    Undefined global functions or variables:
      chr chrom_index coef combn coord copy_number cumsum_values dist
      genotype hclust lm melt mode_cnv n n_gt na.omit px px_width sc_id
      setNames show_warnings single_cell_id site timepoint VAF
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

# CGPfunctions

Version: 0.4

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘devtools’
      All declared Imports should be used.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘BSDA’, ‘janitor’
    ```

# childesr

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dbplyr’
      All declared Imports should be used.
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

# chimeraviz

Version: 1.6.2

## In both

*   checking package dependencies ... ERROR
    ```
    Packages required but not available: ‘org.Hs.eg.db’ ‘org.Mm.eg.db’
    
    Depends: includes the non-default packages:
      ‘Biostrings’ ‘GenomicRanges’ ‘IRanges’ ‘Gviz’ ‘S4Vectors’ ‘ensembldb’
      ‘AnnotationFilter’ ‘data.table’
    Adding so many packages to the search path is excessive and importing
    selectively is preferable.
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# ChIPexoQual

Version: 1.4.0

## In both

*   checking whether package ‘ChIPexoQual’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘IRanges’ was built under R version 3.5.1
      Warning: package ‘GenomicRanges’ was built under R version 3.5.1
      Warning: package ‘DelayedArray’ was built under R version 3.5.1
      Warning: package ‘Rsamtools’ was built under R version 3.5.1
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/ChIPexoQual/new/ChIPexoQual.Rcheck/00install.out’ for details.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Failed with error:  'package 'DelayedArray' could not be loaded'
      Error in .requirePackage(package) : 
        unable to find required package 'ChIPexoQual'
      Calls: <Anonymous> ... getClass -> getClassDef -> .classEnv -> .requirePackage
      Execution halted
    ```

# ChIPseeker

Version: 1.16.1

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘TxDb.Hsapiens.UCSC.hg19.knownGene’
    
    Package suggested but not available for checking: ‘org.Hs.eg.db’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# chorrrds

Version: 0.1.7

## Newly fixed

*   checking installed package size ... NOTE
    ```
      installed size is  5.5Mb
      sub-directories of 1Mb or more:
        data   5.2Mb
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 8008 marked UTF-8 strings
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
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/chromer/new/chromer.Rcheck/00_pkg_src/chromer/R/clean-data.R:77)
    Undefined global functions or variables:
      na.omit
    Consider adding
      importFrom("stats", "na.omit")
    to your NAMESPACE file.
    ```

# chromswitch

Version: 1.2.1

## In both

*   checking whether package ‘chromswitch’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘GenomicRanges’ was built under R version 3.5.1
      Warning: package ‘IRanges’ was built under R version 3.5.1
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/chromswitch/new/chromswitch.Rcheck/00install.out’ for details.
    ```

# chunked

Version: 0.4

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > 
    > ### ** Examples
    > 
    > # create csv file for demo purpose
    > in_file <- file.path(tempdir(), "in.csv")
    > write.csv(women, in_file, row.names = FALSE, quote = FALSE)
    > 
    > #
    > women_chunked <-
    +   read_chunkwise(in_file) %>%  #open chunkwise connection
    +   mutate(ratio = weight/height) %>%
    +   filter(ratio > 2) %>%
    +   select(height, ratio) %>%
    +   inner_join(data.frame(height=63:66)) # you can join with data.frames!
    > 
    > # no processing done until
    > out_file <- file.path(tempdir(), "processed.csv")
    > women_chunked %>%
    +   write_chunkwise(file=out_file)
    Error: `.preserve` (`.preserve = FALSE`) must not be named, do you need `==`?
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      25: filter(.data, !!!dots, .preserve = .preserve) at /Users/romain/git/tidyverse/dplyr/R/dataframe.R:66
      26: filter.data.frame(.data, !!!dots, .preserve = .preserve) at /Users/romain/git/tidyverse/dplyr/R/manip.r:113
      27: as.data.frame(filter(tbl_df(.data), ..., .preserve = .preserve)) at /Users/romain/git/tidyverse/dplyr/R/dataframe.R:61
      28: filter(tbl_df(.data), ..., .preserve = .preserve) at /Users/romain/git/tidyverse/dplyr/R/dataframe.R:61
      29: filter.tbl_df(tbl_df(.data), ..., .preserve = .preserve) at /Users/romain/git/tidyverse/dplyr/R/manip.r:113
      30: bad_eq_ops(bad, "must not be named, do you need `==`?") at /Users/romain/git/tidyverse/dplyr/R/tbl-df.r:50
      31: glubort(fmt_wrong_eq_ops(named_calls), ..., .envir = .envir) at /Users/romain/git/tidyverse/dplyr/R/error.R:37
      32: .abort(text) at /Users/romain/git/tidyverse/dplyr/R/error.R:51
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 39 SKIPPED: 0 FAILED: 1
      1. Error: filter(): can filter rows (@test-verbs.R#28) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# CINdex

Version: 1.8.0

## In both

*   checking whether package ‘CINdex’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘GenomicRanges’ was built under R version 3.5.1
      Warning: package ‘IRanges’ was built under R version 3.5.1
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/CINdex/new/CINdex.Rcheck/00install.out’ for details.
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
        eval, evalq, Filter, Find, get, grep, grepl, intersect,
        is.unsorted, lapply, lengths, Map, mapply, match, mget, order,
        paste, pmax, pmax.int, pmin, pmin.int, Position, rank, rbind,
        Reduce, rowMeans, rownames, rowSums, sapply, setdiff, sort,
        table, tapply, union, unique, unsplit, which, which.max,
        which.min
    
    Loading required package: S4Vectors
    
    Attaching package: 'S4Vectors'
    
    The following object is masked from 'package:base':
    
        expand.grid
    
    Loading required package: IRanges
    Loading required package: GenomeInfoDb
    Quitting from lines 33-42 (PrepareInputData.Rmd) 
    Error: processing vignette 'PrepareInputData.Rmd' failed with diagnostics:
    there is no package called 'pd.genomewidesnp.6'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      ‘pd.genomewidesnp.6’ ‘org.Hs.eg.db’
      ‘TxDb.Hsapiens.UCSC.hg18.knownGene’ ‘Homo.sapiens’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 18.9Mb
      sub-directories of 1Mb or more:
        data  18.0Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/CINdex/new/CINdex.Rcheck/00_pkg_src/CINdex/R/process.probe.anno.R:21)
    process.probe.anno: no visible binding for global variable ‘ID’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/CINdex/new/CINdex.Rcheck/00_pkg_src/CINdex/R/process.probe.anno.R:31)
    process.reference.genome: no visible binding for global variable
      ‘chrom’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/CINdex/new/CINdex.Rcheck/00_pkg_src/CINdex/R/process.reference.genome.R:21-23)
    process.reference.genome: no visible binding for global variable ‘name’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/CINdex/new/CINdex.Rcheck/00_pkg_src/CINdex/R/process.reference.genome.R:21-23)
    process.reference.genome: no visible binding for global variable
      ‘stain’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/CINdex/new/CINdex.Rcheck/00_pkg_src/CINdex/R/process.reference.genome.R:21-23)
    run.cin.chr: no visible global function definition for ‘is’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/CINdex/new/CINdex.Rcheck/00_pkg_src/CINdex/R/run.cin.chr.R:45-64)
    run.cin.cyto: no visible global function definition for ‘is’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/CINdex/new/CINdex.Rcheck/00_pkg_src/CINdex/R/run.cin.cyto.R:53-84)
    Undefined global functions or variables:
      chrom dataMatrix ID is midpoint name stain
    Consider adding
      importFrom("methods", "is")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

# circumplex

Version: 0.2.1

## In both

*   checking whether package ‘circumplex’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/circumplex/new/circumplex.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘circumplex’ ...
** package ‘circumplex’ successfully unpacked and MD5 sums checked
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romain/git/release/dplyr/revdep/library.noindex/circumplex/Rcpp/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/circumplex/RcppArmadillo/include" -I/usr/local/include  -fopenmp  -fPIC  -Wno-unused-result -Wall -O3 -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘circumplex’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/circumplex/new/circumplex.Rcheck/circumplex’

```
### CRAN

```
* installing *source* package ‘circumplex’ ...
** package ‘circumplex’ successfully unpacked and MD5 sums checked
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romain/git/release/dplyr/revdep/library.noindex/circumplex/Rcpp/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/circumplex/RcppArmadillo/include" -I/usr/local/include  -fopenmp  -fPIC  -Wno-unused-result -Wall -O3 -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘circumplex’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/circumplex/old/circumplex.Rcheck/circumplex’

```
# civis

Version: 1.5.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.2Mb
      sub-directories of 1Mb or more:
        help   2.5Mb
        R      3.2Mb
    ```

# cleanNLP

Version: 2.3.0

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      ── 2. Error: testing tidy_pca (@test-tools.R#36)  ──────────────────────────────
      could not find function "n"
      1: cnlp_utils_tfidf(obama) at testthat/test-tools.R:36
      2: dplyr::summarize_(possible_vocab, prop = "n()") at /Users/romain/git/release/dplyr/revdep/checks.noindex/cleanNLP/new/cleanNLP.Rcheck/00_pkg_src/cleanNLP/R/tools.R:172
      3: summarise_.tbl_df(possible_vocab, prop = "n()") at /Users/romain/git/tidyverse/dplyr/R/manip.r:264
      4: summarise_impl(.data, dots) at /Users/romain/git/tidyverse/dplyr/R/tbl-df.r:107
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 23 SKIPPED: 21 FAILED: 2
      1. Error: testing utils_tfidf (@test-tools.R#8) 
      2. Error: testing tidy_pca (@test-tools.R#36) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# clustermq

Version: 0.8.5

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘infuser’ ‘purrr’ ‘R6’
      All declared Imports should be used.
    ```

# cna

Version: 2.1.1

## In both

*   checking re-building of vignette outputs ... NOTE
    ```
    Error in re-building vignettes:
      ...
    Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
      Running 'texi2dvi' on 'cna_vignette.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `thumbpdf.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    
    ! Emergency stop.
    <read *> 
             
    l.9 ^^M
           
    !  ==> Fatal error occurred, no output PDF file produced!
    Calls: buildVignettes -> texi2pdf -> texi2dvi
    Execution halted
    ```

# CNPBayes

Version: 1.10.0

## In both

*   checking whether package ‘CNPBayes’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘IRanges’ was built under R version 3.5.1
      Warning: package ‘GenomicRanges’ was built under R version 3.5.1
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/CNPBayes/new/CNPBayes.Rcheck/00install.out’ for details.
    ```

*   checking Rd \usage sections ... WARNING
    ```
    Undocumented arguments in documentation object 'marginal_lik'
      ‘value’
    
    Functions with \usage entries need to have the appropriate \alias
    entries, and all their arguments documented.
    The \usage entries must correspond to syntactically valid R code.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    11855 memory locations dumped; current usage is 3651&7844
    1004 symbolic tokens
    Transcript written on mf.log.
    fmtutil [INFO]: /Users/romain/Library/TinyTeX/texmf-var/web2c/metafont/mf.base installed.
    fmtutil [INFO]: Disabled formats: 1
    fmtutil [INFO]: Successfully rebuilt formats: 14
    fmtutil [INFO]: Total formats: 15
    fmtutil [INFO]: exiting with status 0
    tlmgr install titlesec
    
    tlmgr: Fundamental package texlive.infra not present, uh oh, goodbyeShould not happen, texlive.infra not found at /usr/local/bin/tlmgr line 7344.
    tlmgr: package repository http://mirrors.standaloneinstaller.com/ctan/systems/texlive/tlnet (not verified: gpg unavailable)
    tlmgr path add
    ! LaTeX Error: File `titlesec.sty' not found.
    
    ! Emergency stop.
    <read *> 
    
    Error: processing vignette 'Convergence.Rmd' failed with diagnostics:
    Failed to compile Convergence.tex. See Convergence.log for more info.
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  8.7Mb
      sub-directories of 1Mb or more:
        doc    3.4Mb
        libs   1.3Mb
        R      3.0Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    copyNumber,SingleBatchCopyNumber: no visible binding for global
      variable ‘theta.star’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/CNPBayes/new/CNPBayes.Rcheck/00_pkg_src/CNPBayes/R/copynumber-models.R:148-149)
    copyNumber,SingleBatchCopyNumber: no visible binding for global
      variable ‘theta.star’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/CNPBayes/new/CNPBayes.Rcheck/00_pkg_src/CNPBayes/R/copynumber-models.R:150-151)
    Undefined global functions or variables:
      theta.star
    ```

# CNVScope

Version: 1.9.7

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘BSgenome.Hsapiens.UCSC.hg19’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# cocktailApp

Version: 0.2.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 14661 marked UTF-8 strings
    ```

# codebook

Version: 0.7.5

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘graphics’ ‘pander’
      All declared Imports should be used.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘mice’
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 65 marked UTF-8 strings
    ```

# codemetar

Version: 0.1.6

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘memoise’
      All declared Imports should be used.
    ```

# codified

Version: 0.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘methods’ ‘readr’
      All declared Imports should be used.
    ```

# codingMatrices

Version: 0.3.2

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
      Running 'texi2dvi' on 'codingMatrices.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `mathtools.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    
    ! Emergency stop.
    <read *> 
             
    l.12 ^^M
            
    !  ==> Fatal error occurred, no output PDF file produced!
    Calls: buildVignettes -> texi2pdf -> texi2dvi
    Execution halted
    ```

# cofeatureR

Version: 1.1.1

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > 
      > test_check("cofeatureR")
      ── 1. Error: plot_cofeature_mat correctly adds tile borders (@test_plot_cofeatur
      Column `shift` must be length 0 (the group size) or one, not 2
      1: plot_cofeature_mat(in_df, tile.col = cur_border_col) at testthat/test_plot_cofeature_mat.R:23
      2: dplyr::mutate_(in.df, .dots = setNames(list(mutate.call), "shift")) at /Users/romain/git/release/dplyr/revdep/checks.noindex/cofeatureR/new/cofeatureR.Rcheck/00_pkg_src/cofeatureR/R/plot_cofeature_mat.R:182
      3: mutate_.tbl_df(in.df, .dots = setNames(list(mutate.call), "shift")) at /Users/romain/git/tidyverse/dplyr/R/manip.r:425
      4: mutate_impl(.data, dots) at /Users/romain/git/tidyverse/dplyr/R/tbl-df.r:96
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 0 SKIPPED: 0 FAILED: 1
      1. Error: plot_cofeature_mat correctly adds tile borders (@test_plot_cofeature_mat.R#23) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tibble’
      All declared Imports should be used.
    ```

# cogena

Version: 1.14.0

## In both

*   checking whether package ‘cogena’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘class::somgrid’ by ‘kohonen::somgrid’ when loading ‘cogena’
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/cogena/new/cogena.Rcheck/00install.out’ for details.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.4Mb
      sub-directories of 1Mb or more:
        doc       1.9Mb
        extdata   3.1Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      ‘legend’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/cogena/new/cogena.Rcheck/00_pkg_src/cogena/R/heatmapCluster.R:151-153)
    heatmapCluster,cogena: no visible global function definition for
      ‘legend’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/cogena/new/cogena.Rcheck/00_pkg_src/cogena/R/heatmapCluster.R:155-157)
    heatmapCluster,cogena: no visible global function definition for
      ‘legend’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/cogena/new/cogena.Rcheck/00_pkg_src/cogena/R/heatmapCluster.R:159-160)
    Undefined global functions or variables:
      abline as.dist axis cor data density dist hist image layout legend
      lines median mtext order.dendrogram p.adjust par phyper plot.new
      rainbow rect reorder sd text title topo.colors
    Consider adding
      importFrom("graphics", "abline", "axis", "hist", "image", "layout",
                 "legend", "lines", "mtext", "par", "plot.new", "rect",
                 "text", "title")
      importFrom("grDevices", "rainbow", "topo.colors")
      importFrom("stats", "as.dist", "cor", "density", "dist", "median",
                 "order.dendrogram", "p.adjust", "phyper", "reorder", "sd")
      importFrom("utils", "data")
    to your NAMESPACE file.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘clValid’
    ```

# CollapsABEL

Version: 0.10.11

## In both

*   checking whether package ‘CollapsABEL’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/CollapsABEL/new/CollapsABEL.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘CollapsABEL’ ...
** package ‘CollapsABEL’ successfully unpacked and MD5 sums checked
** R
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rJava’:
 .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/romain/git/release/dplyr/revdep/library.noindex/CollapsABEL/rJava/libs/rJava.so':
  dlopen(/Users/romain/git/release/dplyr/revdep/library.noindex/CollapsABEL/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/romain/git/release/dplyr/revdep/library.noindex/CollapsABEL/rJava/libs/rJava.so
  Reason: image not found
Error : package ‘rJava’ could not be loaded
ERROR: lazy loading failed for package ‘CollapsABEL’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/CollapsABEL/new/CollapsABEL.Rcheck/CollapsABEL’

```
### CRAN

```
* installing *source* package ‘CollapsABEL’ ...
** package ‘CollapsABEL’ successfully unpacked and MD5 sums checked
** R
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rJava’:
 .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/romain/git/release/dplyr/revdep/library.noindex/CollapsABEL/rJava/libs/rJava.so':
  dlopen(/Users/romain/git/release/dplyr/revdep/library.noindex/CollapsABEL/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/romain/git/release/dplyr/revdep/library.noindex/CollapsABEL/rJava/libs/rJava.so
  Reason: image not found
Error : package ‘rJava’ could not be loaded
ERROR: lazy loading failed for package ‘CollapsABEL’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/CollapsABEL/old/CollapsABEL.Rcheck/CollapsABEL’

```
# CollapseLevels

Version: 0.2.0

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    The error most likely occurred in:
    
    > ### Name: IVCalc
    > ### Title: IVCalc
    > ### Aliases: IVCalc
    > 
    > ### ** Examples
    > 
    > 
    > # Load the German_Credit data set supplied with this package
    > 
    > data("German_Credit")
    > 
    > l<-list()
    > 
    > # Call the function as follows
    > 
    > l<-IVCalc(German_Credit,resp="Good_Bad",bins=10)
    Error in n() : could not find function "n"
    Calls: IVCalc ... <Anonymous> -> <Anonymous> -> summarise.tbl_df -> summarise_impl
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 54-76 (CollapseLevels.Rmd) 
    Error: processing vignette 'CollapseLevels.Rmd' failed with diagnostics:
    could not find function "n"
    Execution halted
    ```

# colorednoise

Version: 1.0.3

## In both

*   checking whether package ‘colorednoise’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/colorednoise/new/colorednoise.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘colorednoise’ ...
** package ‘colorednoise’ successfully unpacked and MD5 sums checked
** libs
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romain/git/release/dplyr/revdep/library.noindex/colorednoise/Rcpp/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/colorednoise/RcppArmadillo/include" -I/usr/local/include  -fopenmp -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘colorednoise’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/colorednoise/new/colorednoise.Rcheck/colorednoise’

```
### CRAN

```
* installing *source* package ‘colorednoise’ ...
** package ‘colorednoise’ successfully unpacked and MD5 sums checked
** libs
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romain/git/release/dplyr/revdep/library.noindex/colorednoise/Rcpp/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/colorednoise/RcppArmadillo/include" -I/usr/local/include  -fopenmp -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘colorednoise’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/colorednoise/old/colorednoise.Rcheck/colorednoise’

```
# compareDF

Version: 1.7.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘magrittr’ ‘stringr’
      All declared Imports should be used.
    ```

# COMPASS

Version: 1.18.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    14 preloaded fonts
    warning  (pdf backend): no pages of output.
    Transcript written on lualatex.log.
    fmtutil [INFO]: /Users/romain/Library/TinyTeX/texmf-var/web2c/luatex/lualatex.fmt installed.
    fmtutil [INFO]: Disabled formats: 1
    fmtutil [INFO]: Successfully rebuilt formats: 14
    fmtutil [INFO]: Total formats: 15
    fmtutil [INFO]: exiting with status 0
    tlmgr install fancyhdr
    
    tlmgr: Fundamental package texlive.infra not present, uh oh, goodbyeShould not happen, texlive.infra not found at /usr/local/bin/tlmgr line 7344.
    tlmgr: package repository http://mirrors.standaloneinstaller.com/ctan/systems/texlive/tlnet (not verified: gpg unavailable)
    tlmgr path add
    ! LaTeX Error: File `fancyhdr.sty' not found.
    
    ! Emergency stop.
    <read *> 
    
    Error: processing vignette 'SimpleCOMPASS.Rmd' failed with diagnostics:
    Failed to compile SimpleCOMPASS.tex. See SimpleCOMPASS.log for more info.
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘BiocStyle’ ‘rmarkdown’
      All declared Imports should be used.
    ':::' call which should be '::': ‘flowWorkspace:::.getNodeInd’
      See the note in ?`:::` about the use of this operator.
    ```

*   checking R code for possible problems ... NOTE
    ```
    COMPASSfitToCountsTable: no visible binding for global variable
      ‘population’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/COMPASS/new/COMPASS.Rcheck/00_pkg_src/COMPASS/R/utils.R:193)
    COMPASSfitToCountsTable: no visible binding for global variable ‘Count’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/COMPASS/new/COMPASS.Rcheck/00_pkg_src/COMPASS/R/utils.R:193)
    COMPASSfitToCountsTable: no visible binding for global variable
      ‘population’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/COMPASS/new/COMPASS.Rcheck/00_pkg_src/COMPASS/R/utils.R:194)
    COMPASSfitToCountsTable: no visible binding for global variable ‘Count’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/COMPASS/new/COMPASS.Rcheck/00_pkg_src/COMPASS/R/utils.R:194)
    COMPASSfitToCountsTable: no visible binding for global variable ‘id’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/COMPASS/new/COMPASS.Rcheck/00_pkg_src/COMPASS/R/utils.R:200)
    COMPASSfitToCountsTable: no visible binding for global variable ‘id’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/COMPASS/new/COMPASS.Rcheck/00_pkg_src/COMPASS/R/utils.R:206)
    Undefined global functions or variables:
      Count id population
    ```

*   checking for unstated dependencies in vignettes ... NOTE
    ```
    'library' or 'require' calls not declared from:
      ‘ggplot2’ ‘readxl’
    ```

# comperank

Version: 0.1.0

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 126-141 (methods-overview.Rmd) 
    Error: processing vignette 'methods-overview.Rmd' failed with diagnostics:
    could not find function "n"
    Execution halted
    ```

# comperes

Version: 0.2.1

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘comperes-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: h2h_funs
    > ### Title: Common Head-to-Head functions
    > ### Aliases: h2h_funs
    > ### Keywords: datasets
    > 
    > ### ** Examples
    > 
    > ncaa2005 %>% h2h_long(!!!h2h_funs)
    Error in n() : could not find function "n"
    Calls: %>% ... remove_class_cond -> NextMethod -> summarise.tbl_df -> summarise_impl
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Different number of rows
      
      ── 4. Failure: summarise_item works with argument `.prefix` (@test-item-summary.
      summarise_item(input, character(0), .prefix = "a_") not equal to tibble::tibble().
      Different number of rows
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 264 SKIPPED: 0 FAILED: 4
      1. Failure: h2h_long handles `player` as factor (@test-head-to-head.R#59) 
      2. Failure: h2h_mat handles `player` as factor (@test-head-to-head.R#157) 
      3. Failure: summarise_item works with item of length zero (@test-item-summary.R#15) 
      4. Failure: summarise_item works with argument `.prefix` (@test-item-summary.R#76) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# condformat

Version: 0.8.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(condformat)
      > 
      > test_check("condformat")
      -- 1. Error: condformat2excel generates a file (@test_rendering.R#42)  ---------
      Please install the xlsx package in order to export to excel
      1: condformat2excel(condformat(head(iris, n = rows_to_write)), filename = filename) at testthat/test_rendering.R:42
      2: require_xlsx() at /Users/romain/git/release/dplyr/revdep/checks.noindex/condformat/new/condformat.Rcheck/00_pkg_src/condformat/R/render_xlsx.R:19
      3: stop("Please install the xlsx package in order to export to excel") at /Users/romain/git/release/dplyr/revdep/checks.noindex/condformat/new/condformat.Rcheck/00_pkg_src/condformat/R/render_xlsx.R:3
      
      == testthat results  ===========================================================
      OK: 125 SKIPPED: 0 FAILED: 1
      1. Error: condformat2excel generates a file (@test_rendering.R#42) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘graphics’
      All declared Imports should be used.
    ```

# conflicted

Version: 1.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘memoise’
      All declared Imports should be used.
    ```

# congressbr

Version: 0.1.5

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘congressbr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: sen_senator_mandates
    > ### Title: Downloads and tidies information on senators' mandates in the
    > ###   Federal Senate
    > ### Aliases: sen_senator_mandates
    > 
    > ### ** Examples
    > 
    > terms <- sen_senator_mandates(id = 4763)
    Error: Can't create call to non-callable object
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked UTF-8 string
    ```

# corrr

Version: 0.3.0

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 39-44 (corrr-databases.Rmd) 
    Error: processing vignette 'corrr-databases.Rmd' failed with diagnostics:
    near "AS": syntax error
    Execution halted
    ```

# Countr

Version: 3.5.2

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
      Running 'texi2dvi' on 'ComputationalPerformance.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `pdfpages.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    
    ! Emergency stop.
    <read *> 
             
    l.5 ^^M
           
    !  ==> Fatal error occurred, no output PDF file produced!
    Calls: buildVignettes -> texi2pdf -> texi2dvi
    Execution halted
    ```

# countyfloods

Version: 0.1.0

## In both

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

# coveffectsplot

Version: 0.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘colourpicker’ ‘dplyr’ ‘markdown’ ‘shinyjs’ ‘tidyr’
      All declared Imports should be used.
    ```

# coxed

Version: 0.2.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘mediation’
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

Version: 2.2.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘gdistance’ ‘raster’
      All declared Imports should be used.
    ```

# CrossClustering

Version: 4.0.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘glue’
      All declared Imports should be used.
    ```

# crosswalkr

Version: 0.2.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# crossword.r

Version: 0.3.5

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘r6extended’
      All declared Imports should be used.
    ```

# crplyr

Version: 0.2.0

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 82 SKIPPED: 0 FAILED: 10
      1.  Failure: Basic filter method (@test-filter.R#11) 
      2.  Failure: Basic filter method (@test-filter.R#13) 
      3.  Failure: Basic filter method (@test-filter.R#15) 
      4.  Failure: Comma-separated filters (@test-filter.R#20) 
      5.  Failure: Filter then select (@test-filter.R#28) 
      6.  Failure: 'add' argument to group_by (@test-group-by.R#40) 
      7.  Failure: Select then filter (@test-select.R#23) 
      8.  Error: summarize after filter (@test-summarize.R#33) 
      9.  Error: filter, group_by, and summarize (@test-summarize.R#63) 
      10. Error: group_by, filter, group_by, and summarize (@test-summarize.R#74) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# crsra

Version: 0.2.3

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 500 marked UTF-8 strings
    ```

# crypto

Version: 1.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘keyring’
      All declared Imports should be used.
    ```

# curatedMetagenomicData

Version: 1.10.2

## In both

*   checking examples ... ERROR
    ```
    ...
    > ### Aliases: SmitsSA_2017 SmitsSA_2017.genefamilies_relab.stool
    > ###   SmitsSA_2017.marker_abundance.stool
    > ###   SmitsSA_2017.marker_presence.stool
    > ###   SmitsSA_2017.metaphlan_bugs_list.stool
    > ###   SmitsSA_2017.pathabundance_relab.stool
    > ###   SmitsSA_2017.pathcoverage.stool
    > 
    > ### ** Examples
    > 
    > SmitsSA_2017.metaphlan_bugs_list.stool()
    snapshotDate(): 2018-04-27
    see ?curatedMetagenomicData and browseVignettes('curatedMetagenomicData') for documentation
    downloading 1 resources
    retrieving 1 resource
    loading from cache 
        ‘/Users/romain//.ExperimentHub/1338’
    Error: failed to load resource
      name: EH1338
      title: 20180425.SmitsSA_2017.metaphlan_bugs_list.stool
      reason: ReadItem: unknown type 115, perhaps written by later version of R
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
             stop("failed to load resource", "\n  name: ", names(x), "\n  title: ", x$title, 
                 "\n  reason: ", conditionMessage(err), call. = FALSE)
         })
      14: tryCatchList(expr, classes, parentenv, handlers)
      15: tryCatchOne(expr, names, parentenv, handlers[[1L]])
      16: value[[3L]](cond)
      17: stop("failed to load resource", "\n  name: ", names(x), "\n  title: ", x$title, "\n  reason: ", 
             conditionMessage(err), call. = FALSE)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 33 SKIPPED: 0 FAILED: 1
      1. Error: countries and studies align. (@test-mergeData.R#27) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking whether package ‘curatedMetagenomicData’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘ExperimentHub’ was built under R version 3.5.1
      Warning: package ‘AnnotationHub’ was built under R version 3.5.1
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/curatedMetagenomicData/new/curatedMetagenomicData.Rcheck/00install.out’ for details.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.7Mb
      sub-directories of 1Mb or more:
        doc    1.4Mb
        help   2.7Mb
    ```

# customsteps

Version: 0.7.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘rlang’ ‘tidyselect’
      All declared Imports should be used.
    ```

# d3r

Version: 0.8.4

## In both

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

# dabestr

Version: 0.1.0

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    +                              paired = FALSE)
    > 
    > # Display the results in a user-friendly format.
    > unpaired_mean_diff
    DABEST (Data Analysis with Bootstrap Estimation) v0.1.0
    =======================================================
    
    Variable: Petal.Width 
    
    Unpaired mean difference of versicolor (n=50) minus setosa (n=50)
     1.08 [95CI  1.01; 1.14]
    
    
    5000 bootstrap resamples.
    All confidence intervals are bias-corrected and accelerated.
    
    > 
    > # Produce an estimation plot.
    > plot(unpaired_mean_diff)
    Error: `breaks` and `labels` must have the same length
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      2: plot.dabest(unpaired, color.col = Gender)
      3: ggplot2::scale_x_discrete(breaks = all.groups, labels = Ns$swarmticklabs) at /Users/romain/git/release/dplyr/revdep/checks.noindex/dabestr/new/dabestr.Rcheck/00_pkg_src/dabestr/R/plot.R:443
      4: discrete_scale(c("x", "xmin", "xmax", "xend"), "position_d", identity, ..., expand = expand, 
             guide = "none", position = position, super = ScaleDiscretePosition)
      5: check_breaks_labels(breaks, labels)
      6: stop("`breaks` and `labels` must have the same length", call. = FALSE)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 9 SKIPPED: 15 FAILED: 3
      1. Error: Gardner-Altman custom aesthetics (@test-aesthetics.R#24) 
      2. Error: Cumming two-groups unpaired (@test-cumming-plots.R#14) 
      3. Error: Gardner-Altman unpaired (@test-gardner-altman-plots.R#14) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y,  :
      no font could be found for family "Rubik Medium"
    Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y,  :
      no font could be found for family "Rubik Medium"
    Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y,  :
      no font could be found for family "Rubik Medium"
    Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y,  :
      no font could be found for family "Work Sans Medium"
    Quitting from lines 72-103 (bootstrap-confidence-intervals.Rmd) 
    Error: processing vignette 'bootstrap-confidence-intervals.Rmd' failed with diagnostics:
    polygon edge not found
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        doc   4.5Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘grid’
      All declared Imports should be used.
    ```

# dalmatian

Version: 0.3.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 36-71 (weights-1-simulate.Rmd) 
    Error: processing vignette 'weights-1-simulate.Rmd' failed with diagnostics:
    .onLoad failed in loadNamespace() for 'rjags', details:
      call: dyn.load(file, DLLpath = DLLpath, ...)
      error: unable to load shared object '/Users/romain/git/release/dplyr/revdep/library.noindex/dalmatian/rjags/libs/rjags.so':
      dlopen(/Users/romain/git/release/dplyr/revdep/library.noindex/dalmatian/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
      Referenced from: /Users/romain/git/release/dplyr/revdep/library.noindex/dalmatian/rjags/libs/rjags.so
      Reason: image not found
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        doc                  1.1Mb
        Pied_Flycatchers_1   2.4Mb
        Pied_Flycatchers_2   1.2Mb
    ```

# DAPAR

Version: 1.12.11

## In both

*   checking whether package ‘DAPAR’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/DAPAR/new/DAPAR.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘DAPAR’ ...
** R
** inst
** byte-compile and prepare package for lazy loading
Warning in fun(libname, pkgname) :
  mzR has been built against a different Rcpp version (0.12.16)
than is installed on your system (1.0.0). This might lead to errors
when loading mzR. If you encounter such issues, please send a report,
including the output of sessionInfo() to the Bioc support forum at 
https://support.bioconductor.org/. For details see also
https://github.com/sneumann/mzR/wiki/mzR-Rcpp-compiler-linker-issue.
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘DO.db’
ERROR: lazy loading failed for package ‘DAPAR’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/DAPAR/new/DAPAR.Rcheck/DAPAR’

```
### CRAN

```
* installing *source* package ‘DAPAR’ ...
** R
** inst
** byte-compile and prepare package for lazy loading
Warning in fun(libname, pkgname) :
  mzR has been built against a different Rcpp version (0.12.16)
than is installed on your system (1.0.0). This might lead to errors
when loading mzR. If you encounter such issues, please send a report,
including the output of sessionInfo() to the Bioc support forum at 
https://support.bioconductor.org/. For details see also
https://github.com/sneumann/mzR/wiki/mzR-Rcpp-compiler-linker-issue.
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘DO.db’
ERROR: lazy loading failed for package ‘DAPAR’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/DAPAR/old/DAPAR.Rcheck/DAPAR’

```
# dat

Version: 0.4.0

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      25: freduce(value, `_function_list`)
      26: withVisible(function_list[[k]](value))
      27: function_list[[k]](value)
      28: dplyr::mutate_(., .dots = args)
      29: mutate_.tbl_df(., .dots = args) at /Users/romain/git/tidyverse/dplyr/R/manip.r:425
      30: mutate_impl(.data, dots) at /Users/romain/git/tidyverse/dplyr/R/tbl-df.r:96
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 98 SKIPPED: 1 FAILED: 3
      1. Error: Basic syntax of a DataFrame (@test-DataFrame.R#127) 
      2. Error: split-apply-combine (@test-map.R#58) 
      3. Error: mutars by and sby (@test-mutar.R#21) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 106-107 (Introduction.Rmd) 
    Error: processing vignette 'Introduction.Rmd' failed with diagnostics:
    could not find function "n"
    Execution halted
    ```

# datadr

Version: 0.8.6.1

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘Rhipe’
    ```

# datasus

Version: 0.4.1

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

# dbparser

Version: 1.0.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘ggplot2’
      All declared Imports should be used.
    ```

# dbplyr

Version: 1.2.2

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      OK: 521 SKIPPED: 11 FAILED: 9
      1. Error: named argument become list columns (@test-do.R#14) 
      2. Error: unnamed results bound together by row (@test-do.R#25) 
      3. Error: Results respect select (@test-do.R#37) 
      4. Error: results independent of chunk_size (@test-do.R#52) 
      5. Failure: n_groups drops zero-length groups (@test-group-size.R#37) 
      6. Failure: summarise drops zero-length groups (@test-group-size.R#47) 
      7. Failure: summarise drops zero-length groups (@test-group-size.R#48) 
      8. Failure: summarise drops zero-length groups (@test-group-size.R#49) 
      9. Failure: queries translate correctly (@test-translate-oracle.R#22) 
      
      Error: testthat unit tests failed
      In addition: Warning message:
      call dbDisconnect() when finished working with a connection 
      Execution halted
    ```

# ddpcr

Version: 1.9

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ddpcr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: load_plate
    > ### Title: Load a previously saved ddPCR plate
    > ### Aliases: load_plate
    > 
    > ### ** Examples
    > 
    > plate <- new_plate(sample_data_dir())
    Error in n() : could not find function "n"
    Calls: new_plate ... <Anonymous> -> <Anonymous> -> summarise_.tbl_df -> summarise_impl
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 205-213 (extend.Rmd) 
    Error: processing vignette 'extend.Rmd' failed with diagnostics:
    could not find function "n"
    Execution halted
    ```

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 162 SKIPPED: 0 FAILED: 14
      1. Error: (unknown) (@test-custom_thresholds.R#4) 
      2. Error: get_empty_cutoff works (@test-empty.R#4) 
      3. Error: get_empty_cutoff for pnpp works (@test-empty.R#18) 
      4. Error: is_well_success works (@test-failures.R#4) 
      5. Error: get_outlier_cutoff works (@test-outliers.R#4) 
      6. Error: test basic plate attribute getters/setters (@test-plate-attribs.R#4) 
      7. Error: (unknown) (@test-plate.R#3) 
      8. Failure: get_filled_drops works (@test-pnpp_experiment-filled.R#22) 
      9. Error: (unknown) (@test-pnpp_experiment.R#4) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        sample_data   3.0Mb
    ```

# DeclareDesign

Version: 0.12.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(DeclareDesign)
      Loading required package: randomizr
      Loading required package: fabricatr
      Loading required package: estimatr
      > 
      > test_check("DeclareDesign")
      ── 1. Failure: gam (@test-model.R#247)  ────────────────────────────────────────
      `expect_equal(ncol(draw_estimates(des)), 7)` did not produce any warnings.
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 456 SKIPPED: 5 FAILED: 1
      1. Failure: gam (@test-model.R#247) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# DeepBlueR

Version: 1.6.0

## In both

*   checking Rd files ... NOTE
    ```
    prepare_Rd: deepblue_enrich_regions_fast.Rd:35-38: Dropping empty section \examples
    ```

# DEGreport

Version: 1.16.0

## In both

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .travis.yml
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Package listed in more than one of Depends, Imports, Suggests, Enhances:
      ‘knitr’
    A package should be listed in only one of these fields.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/DEGreport/new/DEGreport.Rcheck/00_pkg_src/DEGreport/R/methods.R:274-282)
    degMV: no visible binding for global variable ‘max_sd’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/DEGreport/new/DEGreport.Rcheck/00_pkg_src/DEGreport/R/methods.R:274-282)
    degPatterns: no visible global function definition for ‘rowMedians’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/DEGreport/new/DEGreport.Rcheck/00_pkg_src/DEGreport/R/clustering.R:785-787)
    degPatterns: no visible binding for global variable ‘genes’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/DEGreport/new/DEGreport.Rcheck/00_pkg_src/DEGreport/R/clustering.R:816-821)
    degPlotWide : <anonymous>: no visible binding for global variable
      ‘count’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/DEGreport/new/DEGreport.Rcheck/00_pkg_src/DEGreport/R/genePlots.R:155-158)
    significants,list : <anonymous>: no visible binding for global variable
      ‘gene’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/DEGreport/new/DEGreport.Rcheck/00_pkg_src/DEGreport/R/AllMethods.R:225)
    significants,TopTags: no visible binding for global variable ‘FDR’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/DEGreport/new/DEGreport.Rcheck/00_pkg_src/DEGreport/R/AllMethods.R:147-151)
    significants,TopTags: no visible binding for global variable ‘logFC’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/DEGreport/new/DEGreport.Rcheck/00_pkg_src/DEGreport/R/AllMethods.R:147-151)
    Undefined global functions or variables:
      .x base_mean comp compare count counts covar enrichGO FDR gene genes
      keys log2fc log2FoldChange logFC max_sd min_median ratios rowMedians
      simplify
    ```

# DeLorean

Version: 1.5.0

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Joining, by = "gene"
    Warning: Factor `cell` contains implicit NA, consider using `forcats::fct_explicit_na`
    Joining, by = "gene"
    Warning: Factor `cell` contains implicit NA, consider using `forcats::fct_explicit_na`
    Joining, by = "gene"
    Warning: Factor `cell` contains implicit NA, consider using `forcats::fct_explicit_na`
    Joining, by = "gene"
    Warning: Factor `cell` contains implicit NA, consider using `forcats::fct_explicit_na`
    Joining, by = "gene"
    Warning: Factor `cell` contains implicit NA, consider using `forcats::fct_explicit_na`
    Joining, by = "gene"
    Warning: Factor `cell` contains implicit NA, consider using `forcats::fct_explicit_na`
    Joining, by = "gene"
    Warning: Factor `cell` contains implicit NA, consider using `forcats::fct_explicit_na`
    Joining, by = "gene"
    Warning: Factor `cell` contains implicit NA, consider using `forcats::fct_explicit_na`
    Joining, by = "cell"
    Error in analyse.variance(dl, adjust.cell.sizes = adjust.cell.sizes) : 
      !is.na(expr.adj) are not all TRUE
    Calls: estimate.hyper -> analyse.variance -> stopifnot
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      options(mc.cores = parallel::detectCores())
      > 
      > test_check("DeLorean")
      ── 1. Error: (unknown) (@test-guo.r#7)  ────────────────────────────────────────
      !is.na(expr.adj) are not all TRUE
      1: estimate.hyper(dl, sigma.tau = 0.5) at testthat/test-guo.r:7
      2: analyse.variance(dl, adjust.cell.sizes = adjust.cell.sizes) at /Users/romain/git/release/dplyr/revdep/checks.noindex/DeLorean/new/DeLorean.Rcheck/00_pkg_src/DeLorean/R/model.r:184
      3: stopifnot(!is.na(expr.adj)) at /Users/romain/git/release/dplyr/revdep/checks.noindex/DeLorean/new/DeLorean.Rcheck/00_pkg_src/DeLorean/R/model.r:82
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 7 SKIPPED: 0 FAILED: 1
      1. Error: (unknown) (@test-guo.r#7) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Joining, by = "gene"
    Joining, by = "cell"
    Joining, by = "gene"
    Joining, by = "gene"
    Joining, by = "gene"
    Joining, by = "gene"
    Joining, by = "gene"
    Joining, by = "gene"
    Joining, by = "gene"
    Joining, by = "cell"
    Quitting from lines 91-96 (DeLorean.Rmd) 
    Error: processing vignette 'DeLorean.Rmd' failed with diagnostics:
    !is.na(expr.adj) are not all TRUE
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.9Mb
      sub-directories of 1Mb or more:
        libs   4.9Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘lattice’
      All declared Imports should be used.
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# DEP

Version: 1.2.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.0Mb
      sub-directories of 1Mb or more:
        data   1.4Mb
        doc    3.1Mb
        R      1.2Mb
    ```

# DepthProc

Version: 2.0.4

## In both

*   checking whether package ‘DepthProc’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/DepthProc/new/DepthProc.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘DepthProc’ ...
** package ‘DepthProc’ successfully unpacked and MD5 sums checked
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DepthProc/Rcpp/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DepthProc/RcppArmadillo/include" -I/usr/local/include  -fopenmp -fPIC  -Wno-unused-result -Wall -O3 -c Depth.cpp -o Depth.o
clang: error: unsupported option '-fopenmp'
make: *** [Depth.o] Error 1
ERROR: compilation failed for package ‘DepthProc’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/DepthProc/new/DepthProc.Rcheck/DepthProc’

```
### CRAN

```
* installing *source* package ‘DepthProc’ ...
** package ‘DepthProc’ successfully unpacked and MD5 sums checked
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DepthProc/Rcpp/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DepthProc/RcppArmadillo/include" -I/usr/local/include  -fopenmp -fPIC  -Wno-unused-result -Wall -O3 -c Depth.cpp -o Depth.o
clang: error: unsupported option '-fopenmp'
make: *** [Depth.o] Error 1
ERROR: compilation failed for package ‘DepthProc’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/DepthProc/old/DepthProc.Rcheck/DepthProc’

```
# DescriptiveStats.OBeu

Version: 1.3.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘curl’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 5764 marked UTF-8 strings
    ```

# descriptr

Version: 0.4.1

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    |              |         0.13 |         0.33 |          0.2 |              |
    ----------------------------------------------------------------------------
    |            8 |           12 |            0 |            2 |           14 |
    |              |        0.375 |            0 |        0.062 |              |
    |              |         0.86 |            0 |         0.14 |         0.44 |
    |              |          0.8 |            0 |          0.4 |              |
    ----------------------------------------------------------------------------
    | Column Total |           15 |           12 |            5 |           32 |
    |              |        0.468 |        0.375 |        0.155 |              |
    ----------------------------------------------------------------------------
    > 
    > # bar plots
    > plot(k)
    > plot(k, stacked = TRUE)
    > plot(k, proportional = TRUE)
    > 
    > # alternate
    > ds_twoway_table(mtcarz, cyl, gear)
    Error in n() : could not find function "n"
    Calls: ds_twoway_table ... <Anonymous> -> summarise -> summarise.tbl_df -> summarise_impl
    Execution halted
    ```

# detrendr

Version: 0.6.0

## In both

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# dextergui

Version: 0.1.5

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
    
    Loading required package: RSQLite
    no column `person_id` provided, automatically generating unique person id's
    File img/main_bar.PNG not found in resource path
    Error: processing vignette 'dextergui.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 99
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unexported objects imported by ':::' calls:
      ‘dexter:::get_resp_data’ ‘dexter:::qcolors’
      See the note in ?`:::` about the use of this operator.
    ```

# DiagrammeR

Version: 1.0.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.8Mb
      sub-directories of 1Mb or more:
        htmlwidgets   3.0Mb
        R             3.0Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked UTF-8 string
    ```

# DiffBind

Version: 2.8.0

## In both

*   checking whether package ‘DiffBind’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/DiffBind/new/DiffBind.Rcheck/00install.out’ for details.
    ```

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      ‘rgl’ ‘XLConnect’
    ```

## Installation

### Devel

```
* installing *source* package ‘DiffBind’ ...
** libs
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c RcppExports.cpp -o RcppExports.o
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c bamReader.cpp -o bamReader.o
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c bedReader.cpp -o bedReader.o
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c bitBucket.cpp -o bitBucket.o
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c croi_func.cpp -o croi_func.o
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c croi_main.cpp -o croi_main.o
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c densitySet.cpp -o densitySet.o
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c iBucket.cpp -o iBucket.o
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c interval.cpp -o interval.o
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c intervalDensity.cpp -o intervalDensity.o
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c intervalNode.cpp -o intervalNode.o
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c intervalSet.cpp -o intervalSet.o
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c intervalTree.cpp -o intervalTree.o
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c merge.cpp -o merge.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -Wall -g -O2  -c mergeOne.c -o mergeOne.o
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c nodeGroup.cpp -o nodeGroup.o
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c peakOrder.cpp -o peakOrder.o
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c reader.cpp -o reader.o
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c sequence.cpp -o sequence.o
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c util.cpp -o util.o
clang++ -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o DiffBind.so RcppExports.o bamReader.o bedReader.o bitBucket.o croi_func.o croi_main.o densitySet.o iBucket.o interval.o intervalDensity.o intervalNode.o intervalSet.o intervalTree.o merge.o mergeOne.o nodeGroup.o peakOrder.o reader.o sequence.o util.o /Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rsamtools/usrlib//libbam.a /Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rsamtools/usrlib//libbcf.a /Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rsamtools/usrlib//libtabix.a -lz -pthread -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
installing to /Users/romain/git/release/dplyr/revdep/checks.noindex/DiffBind/new/DiffBind.Rcheck/DiffBind/libs
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Warning: package ‘GenomicRanges’ was built under R version 3.5.1
Warning: package ‘IRanges’ was built under R version 3.5.1
Warning: package ‘DelayedArray’ was built under R version 3.5.1
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘GO.db’
ERROR: lazy loading failed for package ‘DiffBind’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/DiffBind/new/DiffBind.Rcheck/DiffBind’

```
### CRAN

```
* installing *source* package ‘DiffBind’ ...
** libs
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c RcppExports.cpp -o RcppExports.o
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c bamReader.cpp -o bamReader.o
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c bedReader.cpp -o bedReader.o
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c bitBucket.cpp -o bitBucket.o
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c croi_func.cpp -o croi_func.o
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c croi_main.cpp -o croi_main.o
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c densitySet.cpp -o densitySet.o
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c iBucket.cpp -o iBucket.o
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c interval.cpp -o interval.o
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c intervalDensity.cpp -o intervalDensity.o
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c intervalNode.cpp -o intervalNode.o
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c intervalSet.cpp -o intervalSet.o
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c intervalTree.cpp -o intervalTree.o
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c merge.cpp -o merge.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -Wall -g -O2  -c mergeOne.c -o mergeOne.o
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c nodeGroup.cpp -o nodeGroup.o
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c peakOrder.cpp -o peakOrder.o
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c reader.cpp -o reader.o
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c sequence.cpp -o sequence.o
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c util.cpp -o util.o
clang++ -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o DiffBind.so RcppExports.o bamReader.o bedReader.o bitBucket.o croi_func.o croi_main.o densitySet.o iBucket.o interval.o intervalDensity.o intervalNode.o intervalSet.o intervalTree.o merge.o mergeOne.o nodeGroup.o peakOrder.o reader.o sequence.o util.o /Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rsamtools/usrlib//libbam.a /Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rsamtools/usrlib//libbcf.a /Users/romain/git/release/dplyr/revdep/library.noindex/DiffBind/Rsamtools/usrlib//libtabix.a -lz -pthread -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
installing to /Users/romain/git/release/dplyr/revdep/checks.noindex/DiffBind/old/DiffBind.Rcheck/DiffBind/libs
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Warning: package ‘GenomicRanges’ was built under R version 3.5.1
Warning: package ‘IRanges’ was built under R version 3.5.1
Warning: package ‘DelayedArray’ was built under R version 3.5.1
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘GO.db’
ERROR: lazy loading failed for package ‘DiffBind’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/DiffBind/old/DiffBind.Rcheck/DiffBind’

```
# diffcyt

Version: 1.0.10

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 127-144 (diffcyt_workflow.Rmd) 
    Error: processing vignette 'diffcyt_workflow.Rmd' failed with diagnostics:
    there is no package called 'HDCytoData'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘HDCytoData’
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/diffcyt/new/diffcyt.Rcheck/00_pkg_src/diffcyt/R/calcMedians.R:133-136)
    calcMediansByClusterMarker: no visible binding for global variable
      ‘cluster_id’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/diffcyt/new/diffcyt.Rcheck/00_pkg_src/diffcyt/R/calcMediansByClusterMarker.R:123-126)
    calcMediansByClusterMarker: no visible binding for global variable
      ‘marker’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/diffcyt/new/diffcyt.Rcheck/00_pkg_src/diffcyt/R/calcMediansByClusterMarker.R:123-126)
    calcMediansByClusterMarker: no visible binding for global variable
      ‘value’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/diffcyt/new/diffcyt.Rcheck/00_pkg_src/diffcyt/R/calcMediansByClusterMarker.R:123-126)
    calcMediansBySampleMarker: no visible binding for global variable
      ‘sample_id’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/diffcyt/new/diffcyt.Rcheck/00_pkg_src/diffcyt/R/calcMediansBySampleMarker.R:119-122)
    calcMediansBySampleMarker: no visible binding for global variable
      ‘marker’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/diffcyt/new/diffcyt.Rcheck/00_pkg_src/diffcyt/R/calcMediansBySampleMarker.R:119-122)
    calcMediansBySampleMarker: no visible binding for global variable
      ‘value’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/diffcyt/new/diffcyt.Rcheck/00_pkg_src/diffcyt/R/calcMediansBySampleMarker.R:119-122)
    Undefined global functions or variables:
      cluster_id marker sample_id value
    ```

# diffloop

Version: 1.8.0

## In both

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .travis.yml
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

# dirichletprocess

Version: 0.2.2

## In both

*   checking re-building of vignette outputs ... NOTE
    ```
    Error in re-building vignettes:
      ...
    Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
      Running 'texi2dvi' on 'dirichletprocess.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `thumbpdf.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    
    ! Emergency stop.
    <read *> 
             
    l.11 ^^M
            
    !  ==> Fatal error occurred, no output PDF file produced!
    Calls: buildVignettes -> texi2pdf -> texi2dvi
    Execution halted
    ```

# DisImpact

Version: 0.0.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘magrittr’
      All declared Imports should be used.
    ```

# disto

Version: 0.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘proxy’
      All declared Imports should be used.
    ```

# distrr

Version: 0.0.4

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    The error most likely occurred in:
    
    > ### Name: dcc
    > ### Title: Data cube creation (dcc)
    > ### Aliases: dcc dcc2 dcc5
    > 
    > ### ** Examples
    > 
    > data("invented_wages")
    > str(invented_wages)
    Classes ‘tbl_df’ and 'data.frame':	1000 obs. of  5 variables:
     $ gender        : Factor w/ 2 levels "men","women": 1 2 1 2 1 1 1 2 2 2 ...
     $ sector        : Factor w/ 2 levels "secondary","tertiary": 2 1 2 2 1 1 2 1 2 1 ...
     $ education     : Factor w/ 3 levels "I","II","III": 3 2 2 2 2 1 3 1 2 2 ...
     $ wage          : num  8400 4200 5100 7400 4300 4900 5400 2900 4500 3000 ...
     $ sample_weights: num  105 32 36 12 21 46 79 113 34 32 ...
    > tmp <- dcc(.data = invented_wages, 
    +            .variables = c("gender", "sector"), .fun = jointfun_)
    Error in n() : could not find function "n"
    Calls: dcc ... summarise2_ -> <Anonymous> -> summarise_.tbl_df -> summarise_impl
    Execution halted
    ```

# DiversityOccupancy

Version: 1.0.6

## In both

*   checking whether package ‘DiversityOccupancy’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/DiversityOccupancy/new/DiversityOccupancy.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘DiversityOccupancy’ ...
** package ‘DiversityOccupancy’ successfully unpacked and MD5 sums checked
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error : .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/romain/git/release/dplyr/revdep/library.noindex/DiversityOccupancy/rJava/libs/rJava.so':
  dlopen(/Users/romain/git/release/dplyr/revdep/library.noindex/DiversityOccupancy/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/romain/git/release/dplyr/revdep/library.noindex/DiversityOccupancy/rJava/libs/rJava.so
  Reason: image not found
ERROR: lazy loading failed for package ‘DiversityOccupancy’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/DiversityOccupancy/new/DiversityOccupancy.Rcheck/DiversityOccupancy’

```
### CRAN

```
* installing *source* package ‘DiversityOccupancy’ ...
** package ‘DiversityOccupancy’ successfully unpacked and MD5 sums checked
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error : .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/romain/git/release/dplyr/revdep/library.noindex/DiversityOccupancy/rJava/libs/rJava.so':
  dlopen(/Users/romain/git/release/dplyr/revdep/library.noindex/DiversityOccupancy/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/romain/git/release/dplyr/revdep/library.noindex/DiversityOccupancy/rJava/libs/rJava.so
  Reason: image not found
ERROR: lazy loading failed for package ‘DiversityOccupancy’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/DiversityOccupancy/old/DiversityOccupancy.Rcheck/DiversityOccupancy’

```
# DLMtool

Version: 5.2.3

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.4Mb
      sub-directories of 1Mb or more:
        data   2.1Mb
        R      6.0Mb
    ```

# dlookr

Version: 0.3.2

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    [351]  91.00 105.00 122.00  92.00 145.00 146.00 164.00  72.00 118.00 130.00
    [361] 114.00 104.00 110.00 108.00 131.00 162.00 134.00  77.00  79.00 122.00
    [371] 119.00 126.00  98.00 116.00 118.00 124.00  92.00 125.00 119.00 107.00
    [381]  89.00 151.00 121.00  68.00 112.00 132.00 160.00 115.00  78.00 107.00
    [391] 111.00 124.00 130.00 120.00 139.00 128.00 120.00 159.00  95.00 120.00
    attr(,"method")
    [1] "capping"
    attr(,"var_type")
    [1] "numerical"
    attr(,"outlier_pos")
    [1]  43 126 166 175 368
    attr(,"outliers")
    [1]  24  49 191 185  53
    attr(,"type")
    [1] "outliers"
    attr(,"class")
    [1] "imputation" "numeric"   
    > summary(price)
    Error in is_character(x) : object 'key' not found
    Calls: summary ... vars_select_eval -> map_if -> map -> .f -> - -> is_character
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Loading required package: DBI
    Loading required package: RSQLite
    Loading required package: dbplyr
    
    Attaching package: 'dbplyr'
    
    The following objects are masked from 'package:dplyr':
    
        ident, sql
    
    Warning: Factor `Urban` contains implicit NA, consider using `forcats::fct_explicit_na`
    Quitting from lines 349-353 (EDA.Rmd) 
    Error: processing vignette 'EDA.Rmd' failed with diagnostics:
    Unsupported index type: NULL
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        doc   4.2Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dbplyr’ ‘randomForest’
      All declared Imports should be used.
    ```

# doBy

Version: 4.6-2

## In both

*   checking re-building of vignette outputs ... NOTE
    ```
    Error in re-building vignettes:
      ...
    Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
      Running 'texi2dvi' on 'linest-lsmeans.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `a4wide.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    
    ! Emergency stop.
    <read *> 
             
    l.62 \usepackage
                    {boxedminipage,color}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    Calls: buildVignettes -> texi2pdf -> texi2dvi
    Execution halted
    ```

# docxtools

Version: 0.2.0

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘docxtools-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: format_engr
    > ### Title: Format numerical variables in engineering notation.
    > ### Aliases: format_engr
    > 
    > ### ** Examples
    > 
    > # Factors unaffected; ambiguous trailing zeros.
    > data("CO2")
    > x <- head(CO2, n = 5L)
    > format_engr(x)
    Error in n() : could not find function "n"
    Calls: format_engr ... as.data.frame -> mutate -> mutate.tbl_df -> mutate_impl
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      5: mutate.data.frame(x, observ_index = 1:n()) at /Users/romain/git/tidyverse/dplyr/R/manip.r:416
      6: as.data.frame(mutate(tbl_df(.data), ...)) at /Users/romain/git/tidyverse/dplyr/R/dataframe.R:91
      7: mutate(tbl_df(.data), ...) at /Users/romain/git/tidyverse/dplyr/R/dataframe.R:91
      8: mutate.tbl_df(tbl_df(.data), ...) at /Users/romain/git/tidyverse/dplyr/R/manip.r:416
      9: mutate_impl(.data, dots) at /Users/romain/git/tidyverse/dplyr/R/tbl-df.r:91
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 14 SKIPPED: 0 FAILED: 4
      1. Error: Factors are returned unaffected (@test_format_engr.R#14) 
      2. Error: Integers are returned as characters but unformatted (@test_format_engr.R#27) 
      3. Error: sigdig vector is correct (@test_format_engr.R#35) 
      4. Error: (unknown) (@test_format_engr.R#55) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# dplyr.teradata

Version: 0.3.0

## Newly broken

*   checking S3 generic/method consistency ... WARNING
    ```
    sample_n:
      function(tbl, size, replace, weight, .env, ...)
    sample_n.tbl_teradata:
      function(tbl, size, replace, weight, .env)
    
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘bit64’ ‘rstudioapi’
      All declared Imports should be used.
    ```

# DSAIDE

Version: 0.7.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.4Mb
      sub-directories of 1Mb or more:
        media       2.2Mb
        shinyapps   2.6Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘knitr’ ‘rmarkdown’ ‘utils’
      All declared Imports should be used.
    ```

# DSAIRM

Version: 0.4.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘knitr’ ‘rmarkdown’
      All declared Imports should be used.
    ```

# dsr

Version: 0.1.0

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > 
    > #US standard population
    > df_ref  <- data.frame(age=c('00-14','15-24','25-44','45-64','65+'),
    +                      pop=c(23961000,15420000,21353000,19601000,10685000))
    > 
    > #Directly Standardized Rates (per 1000) - 95% CI's using the gamma method
    > my_results <- dsr(data=df_study,
    +                  event=deaths,
    +                  fu=fu,
    +                  subgroup=state,
    +                  age,
    +                  refdata=df_ref,
    +                  method="gamma",
    +                  sig=0.95,
    +                  mp=1000,
    +                  decimals=4)
    Joining, by = "age"
    `mutate_if()` ignored the following grouping variables:
    Column `state`
    Error: Column `state` is unknown
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 80-93 (dsr.Rmd) 
    Error: processing vignette 'dsr.Rmd' failed with diagnostics:
    Column `state` is unknown
    Execution halted
    ```

# dtplyr

Version: 0.0.2

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      13: `_dt`[, .SD[sample.int(.N, check_size(!!size, .N, replace = replace), replace = replace, 
             prob = !!weight)], by = `_vars`]
      14: `[.data.table`(`_dt`, , .SD[sample.int(.N, check_size(!!size, .N, replace = replace), 
             replace = replace, prob = !!weight)], by = `_vars`)
      15: `[.data.frame`(x, i, j)
      16: sample.int(.N, check_size(!!size, .N, replace = replace), replace = replace, prob = !!weight)
      17: Ops.quosure(weight)
      18: is_quosure(e2)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 127 SKIPPED: 0 FAILED: 1
      1. Error: sample preserves class (@test-sample.R#4) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# dtwclust

Version: 5.5.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
      Distance matrix is not symmetric, and hierarchical clustering assumes it is (it ignores the upper triangular).
    Loading required package: doParallel
    Loading required package: foreach
    Loading required package: iterators
    Loading required package: clue
    Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
      Running 'texi2dvi' on 'dtwclust.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `placeins.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    
    ! Emergency stop.
    <read *> 
             
    l.80 ^^M
            
    !  ==> Fatal error occurred, no output PDF file produced!
    Calls: buildVignettes -> texi2pdf -> texi2dvi
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        doc   2.5Mb
        R     2.0Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# duawranglr

Version: 0.6.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘digest’ ‘dplyr’
      All declared Imports should be used.
    ```

# dynfrail

Version: 0.5.2

## In both

*   checking whether package ‘dynfrail’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/dynfrail/new/dynfrail.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘dynfrail’ ...
** package ‘dynfrail’ successfully unpacked and MD5 sums checked
** libs
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romain/git/release/dplyr/revdep/library.noindex/dynfrail/Rcpp/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/dynfrail/RcppArmadillo/include" -I/usr/local/include  -fopenmp  -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘dynfrail’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/dynfrail/new/dynfrail.Rcheck/dynfrail’

```
### CRAN

```
* installing *source* package ‘dynfrail’ ...
** package ‘dynfrail’ successfully unpacked and MD5 sums checked
** libs
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romain/git/release/dplyr/revdep/library.noindex/dynfrail/Rcpp/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/dynfrail/RcppArmadillo/include" -I/usr/local/include  -fopenmp  -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘dynfrail’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/dynfrail/old/dynfrail.Rcheck/dynfrail’

```
# dynutils

Version: 1.0.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘processx’ ‘Rcpp’
      All declared Imports should be used.
    ```

# easyalluvial

Version: 0.1.7

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Attaching package: ‘dplyr’
    
    The following objects are masked from ‘package:stats’:
    
        filter, lag
    
    The following objects are masked from ‘package:base’:
    
        intersect, setdiff, setequal, union
    
    > 
    >  df = mtcars %>%
    +    mutate( cyl = as.factor(cyl)
    +           , gear = as.factor(gear)
    +           , vs = as.factor(vs)
    +           , am = as.factor(am))
    > 
    >  plot_condensation(df)
    Error: segfault from C stack overflow
    Error: segfault from C stack overflow
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(easyalluvial)
      > 
      > test_check("easyalluvial")
    ```

# easyformatr

Version: 0.1.2

## Newly broken

*   checking whether package ‘easyformatr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/easyformatr/new/easyformatr.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘easyformatr’ ...
** package ‘easyformatr’ successfully unpacked and MD5 sums checked
** R
** inst
** byte-compile and prepare package for lazy loading
Joining, by = "code"
Warning in materialize_binding(index, mask_proxy_xp) :
  Hybrid callback proxy out of scope
Warning in materialize_binding(index, mask_proxy_xp) :
  Hybrid callback proxy out of scope
Error in envlist(e) : object 'name' not found
ERROR: lazy loading failed for package ‘easyformatr’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/easyformatr/new/easyformatr.Rcheck/easyformatr’

```
### CRAN

```
* installing *source* package ‘easyformatr’ ...
** package ‘easyformatr’ successfully unpacked and MD5 sums checked
** R
** inst
** byte-compile and prepare package for lazy loading
Joining, by = "code"
Warning in fun(name, ...) : Hybrid callback proxy out of scope
Warning in fun(name, ...) : Hybrid callback proxy out of scope
Warning in fun(name, ...) : Hybrid callback proxy out of scope
Warning in fun(name, ...) : Hybrid callback proxy out of scope
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded
* DONE (easyformatr)

```
# echarts4r

Version: 0.2.0

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    +   e_add("itemStyle", color)
    > 
    > # Heatmap can take nested label
    > # https://ecomfe.github.io/echarts-doc/public/en/option.html#series-heatmap.data
    > v <- LETTERS[1:10]
    > matrix <- data.frame(
    +   x = sample(v, 300, replace = TRUE), 
    +   y = sample(v, 300, replace = TRUE), 
    +   z = rnorm(300, 10, 1),
    +   stringsAsFactors = FALSE
    + ) %>% 
    +   dplyr::group_by(x, y) %>% 
    +   dplyr::summarise(z = sum(z)) %>% 
    +   dplyr::ungroup() %>% 
    +   dplyr::mutate(
    +     show = TRUE,
    +     fontStyle = round(runif(n(), 5, 12))
    +   )
    Error in n() : could not find function "n"
    Calls: %>% ... <Anonymous> -> mutate.tbl_df -> mutate_impl -> runif
    Execution halted
    ```

# ecoengine

Version: 1.11.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘magrittr’
      All declared Imports should be used.
    ```

# econet

Version: 0.1.81

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Error: processing vignette 'econet.tex' failed with diagnostics:
    Running 'texi2dvi' on 'econet.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `thumbpdf.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    
    ! Emergency stop.
    <read *> 
             
    l.6 \usepackage
                   {framed}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tnet’
      All declared Imports should be used.
    ```

# eda4treeR

Version: 0.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dae’ ‘dplyr’
      All declared Imports should be used.
    ```

# edeaR

Version: 0.8.1

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 79-82 (filters.Rmd) 
    Error: processing vignette 'filters.Rmd' failed with diagnostics:
    lifecycle_id not found in data.frame
    Execution halted
    ```

# EdSurvey

Version: 2.2.2

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        R   4.0Mb
    ```

# eechidna

Version: 1.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.4Mb
      sub-directories of 1Mb or more:
        data   4.9Mb
        doc    1.2Mb
    ```

# EFDR

Version: 0.1.1

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/EFDR/new/EFDR.Rcheck/00_pkg_src/EFDR/R/EFDR_functions.R:686)
    .relist.dwt: no visible global function definition for ‘as’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/EFDR/new/EFDR.Rcheck/00_pkg_src/EFDR/R/EFDR_functions.R:686)
    .std.wav.coeff : <anonymous>: no visible global function definition for
      ‘mad’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/EFDR/new/EFDR.Rcheck/00_pkg_src/EFDR/R/EFDR_functions.R:698)
    regrid: no visible global function definition for ‘predict’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/EFDR/new/EFDR.Rcheck/00_pkg_src/EFDR/R/EFDR_functions.R:391-396)
    regrid: no visible global function definition for ‘var’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/EFDR/new/EFDR.Rcheck/00_pkg_src/EFDR/R/EFDR_functions.R:406)
    regrid: no visible global function definition for ‘medpolish’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/EFDR/new/EFDR.Rcheck/00_pkg_src/EFDR/R/EFDR_functions.R:427)
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

# egor

Version: 0.18.08-02

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      EI-Index: age
      EI-Index: sex
      EI-Index: sex
      EI-Index: int_var
      EI-Index: female
      EI-Index: female
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 68 SKIPPED: 0 FAILED: 4
      1. Failure: methods for dplyr verbs keep egor class/attributes (@test_dplyr_methods.R#31) 
      2. Failure: egor() works with missing alters/ aaties / egos. (@test_egor.R#39) 
      3. Failure: egor() works with missing alters/ aaties / egos. (@test_egor.R#42) 
      4. Failure: egor() works with missing alters/ aaties / egos. (@test_egor.R#46) 
      
      Error: testthat unit tests failed
      Execution halted
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
      installed size is 45.1Mb
      sub-directories of 1Mb or more:
        doc  44.1Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/ELMER/new/ELMER.Rcheck/00_pkg_src/ELMER/R/motif.TF.Plots.R:148-157)
    scatter: no visible binding for global variable 'value'
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/ELMER/new/ELMER.Rcheck/00_pkg_src/ELMER/R/Scatter.plot.R:217-231)
    scatter: no visible global function definition for 'cor.test'
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/ELMER/new/ELMER.Rcheck/00_pkg_src/ELMER/R/Scatter.plot.R:236-238)
    scatter: no visible binding for global variable 'mae'
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/ELMER/new/ELMER.Rcheck/00_pkg_src/ELMER/R/Scatter.plot.R:250-267)
    TF.rank.plot: no visible binding for global variable 'pvalue'
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/ELMER/new/ELMER.Rcheck/00_pkg_src/ELMER/R/motif.TF.Plots.R:291-304)
    TF.rank.plot: no visible binding for global variable 'label'
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/ELMER/new/ELMER.Rcheck/00_pkg_src/ELMER/R/motif.TF.Plots.R:291-304)
    TF.rank.plot: no visible binding for global variable 'Gene'
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/ELMER/new/ELMER.Rcheck/00_pkg_src/ELMER/R/motif.TF.Plots.R:308-320)
    Undefined global functions or variables:
      cor.test fisher.test Gene GeneID gr hm450.hg38.manifest Hugo_Symbol
      label lowerOR mae motif OR precede Probe pvalue subsetByOverlaps
      Target TF upperOR value write.table x y z
    Consider adding
      importFrom("stats", "cor.test", "fisher.test")
      importFrom("utils", "write.table")
    to your NAMESPACE file.
    ```

# ELMER.data

Version: 2.4.2

## In both

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .travis.yml
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 288.8Mb
      sub-directories of 1Mb or more:
        data  286.3Mb
        doc     2.4Mb
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Malformed Description field: should contain one or more complete sentences.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Error in .requirePackage(package) : 
        unable to find required package 'MultiAssayExperiment'
      Calls: <Anonymous> ... .extendsForS3 -> extends -> getClassDef -> .requirePackage
      Execution halted
    ```

# emuR

Version: 1.1.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.1Mb
      sub-directories of 1Mb or more:
        doc       1.2Mb
        extdata   1.5Mb
        R         3.0Mb
    ```

# ENCODExplorer

Version: 2.6.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 74.0Mb
      sub-directories of 1Mb or more:
        data     24.1Mb
        doc       1.5Mb
        extdata  48.0Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    step6_target: no visible binding for global variable ‘target’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/ENCODExplorer/new/ENCODExplorer.Rcheck/00_pkg_src/ENCODExplorer/R/prepare_data.R:354-355)
    step7: no visible binding for global variable ‘organism’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/ENCODExplorer/new/ENCODExplorer.Rcheck/00_pkg_src/ENCODExplorer/R/prepare_data.R:424-425)
    step8: no visible binding for global variable ‘investigated_as’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/ENCODExplorer/new/ENCODExplorer.Rcheck/00_pkg_src/ENCODExplorer/R/prepare_data.R:436-437)
    step8: no visible binding for global variable ‘target’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/ENCODExplorer/new/ENCODExplorer.Rcheck/00_pkg_src/ENCODExplorer/R/prepare_data.R:439-440)
    step9: no visible binding for global variable ‘organism’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/ENCODExplorer/new/ENCODExplorer.Rcheck/00_pkg_src/ENCODExplorer/R/prepare_data.R:449-450)
    Undefined global functions or variables:
      . accession antibody_caption antibody_characterization
      antibody_target assay biological_replicate_number biosample_name
      biosample_type col_name controls data date_released download.file
      encode_df Experiment file_accession file_format href investigated_as
      lab nucleic_acid_term organism platform project replicate_antibody
      replicate_library server status submitted_by target
      technical_replicate_number treatment ui value Value
    Consider adding
      importFrom("utils", "data", "download.file")
    to your NAMESPACE file.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 771 marked UTF-8 strings
    ```

# epicontacts

Version: 1.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘colorspace’
      All declared Imports should be used.
    ```

# EpiReport

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘extrafont’ ‘graphics’ ‘knitr’ ‘rmarkdown’ ‘utils’
      All declared Imports should be used.
    ```

# episheet

Version: 0.3.0

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘episheet-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: risk
    > ### Title: Calculate risk ratio and risk difference
    > ### Aliases: risk
    > 
    > ### ** Examples
    > 
    > # Data from stratum 1 of table 15-1., p260
    > dat <- data.frame(
    +     exposure_var = c(rep(1, 8), rep(0, 5), rep(1, 98), rep(0, 115)),
    +   outcome_var = c(rep(1, 8), rep(1, 5), rep(0, 98), rep(0, 115)),
    +   stringsAsFactors = FALSE
    + )
    > risk(data = dat, exposure = exposure_var, outcome = outcome_var)
    Error in n() : could not find function "n"
    Calls: risk ... as.data.frame -> summarise -> summarise.tbl_df -> summarise_impl
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 0 SKIPPED: 0 FAILED: 9
      1. Error: Test risk_ratio returns exected estimate (@test-risk.R#10) 
      2. Error: Test risk_ratio returns exected lci (@test-risk.R#15) 
      3. Error: Test risk_ratio returns exected uci (@test-risk.R#20) 
      4. Error: Test risk_diff returns exected estimate (@test-risk.R#25) 
      5. Error: Test risk_diff returns exected lci (@test-risk.R#30) 
      6. Error: Test risk_diff returns exected uci (@test-risk.R#35) 
      7. Error: rrmh returns expected value (@test-stratified_risk.R#6) 
      8. Error: lci return expected value (@test-stratified_risk.R#13) 
      9. Error: uci return expected value (@test-stratified_risk.R#28) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# EpiSignalDetection

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘DT’ ‘ggplot2’ ‘knitr’ ‘pander’
      All declared Imports should be used.
    ```

# epos

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘testthat’ ‘tidyr’
      All declared Imports should be used.
    ```

# ergm

Version: 3.9.4

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    samplk3 (samplk)      Longitudinal networks of positive affection
                          within a monastery as a "network" object
    
    Error in ergm.MCMLE(init, nw, model, initialfit = (initialfit <- NULL),  : 
      Number of edges in a simulated network exceeds that in the observed by a factor of more than 20. This is a strong indicator of model degeneracy or a very poor starting parameter configuration. If you are reasonably certain that neither of these is the case, increase the MCMLE.density.guard control.ergm() parameter.
    Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
      Running 'texi2dvi' on 'ergm.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `multirow.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    
    ! Emergency stop.
    <read *> 
             
    l.57 \newcommand
                    {\myverb}[1]{\mbox{\texttt{#1}}}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    Calls: buildVignettes -> texi2pdf -> texi2dvi
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘Rmpi’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  7.2Mb
      sub-directories of 1Mb or more:
        doc   1.7Mb
        R     4.2Mb
    ```

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘tergm’, ‘ergm.count’, ‘networkDynamic’
    ```

# esc

Version: 0.4.1

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘metafor’
    ```

# eurostat

Version: 3.3.1

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 595 marked UTF-8 strings
    ```

# evaluator

Version: 0.3.1

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      31: function_list[[i]](value)
      32: purrrlyr::by_slice(., ~arrange(.x, max_loss) %>% mutate(prob = 1 - percent_rank(max_loss)), 
             .collate = "rows")
      
      ── 2. Failure: Domain summary (@test-summarize.R#21)  ──────────────────────────
      summarize_domains(simulation_results) not equivalent to `domain_summary`.
      Rows in x but not y: 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4[...]. Rows in y but not x: 14, 13, 12, 11, 10, 8, 7, 6, 5, 4, 3[...]. 
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 85 SKIPPED: 5 FAILED: 2
      1. Error: Analyze report renders (@test-reports.R#35) 
      2. Failure: Domain summary (@test-summarize.R#21) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# EventStudy

Version: 0.34

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.9Mb
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

Version: 0.2.2

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
        0     0    0     0    0     0      0      0 --:--:-- --:--:-- --:--:--     0
       77 2850k   77 2213k    0     0  5871k      0 --:--:-- --:--:-- --:--:-- 5870k
      100 2850k  100 2850k    0     0  6697k      0 --:--:-- --:--:-- --:--:-- 6691k
      ── 1. Failure: exiftool install works properly (@test_utils.R#24)  ─────────────
      exiftool_version() not equal to 11.1.
      1/1 mismatches
      [1] 11.2 - 11.1 == 0.12
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 40 SKIPPED: 0 FAILED: 1
      1. Failure: exiftool install works properly (@test_utils.R#24) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 13.1Mb
      sub-directories of 1Mb or more:
        exiftool  13.0Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# extdplyr

Version: 0.1.4

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    72 65-74    80-119    20-29      2         3
    73 65-74    80-119      30+      1         1
    74 65-74      120+ 0-9g/day      3         4
    75 65-74      120+    10-19      1         2
    76 65-74      120+    20-29      1         1
    77 65-74      120+      30+      1         1
    78   75+ 0-39g/day 0-9g/day      1        18
    79   75+ 0-39g/day    10-19      2         6
    80   75+ 0-39g/day      30+      1         3
    81   75+     40-79 0-9g/day      2         5
    82   75+     40-79    10-19      1         3
    83   75+     40-79    20-29      0         3
    84   75+     40-79      30+      1         1
    85   75+    80-119 0-9g/day      1         1
    86   75+    80-119    10-19      1         1
    87   75+      120+ 0-9g/day      2         2
    88   75+      120+    10-19      1         1
    > pct_routine(esoph, agegp, alcgp)
    Error in n() : could not find function "n"
    Calls: pct_routine ... eval -> <Anonymous> -> summarise_.tbl_df -> summarise_impl
    Execution halted
    ```

# eyetrackingR

Version: 0.1.8

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    +                                treat_non_aoi_looks_as_missing = TRUE )
    > response_window <- subset_by_window(data, window_start_time = 15500, window_end_time = 21000, 
    +                                     rezero = FALSE)
    Avg. window length in new data will be 5500
    > response_time <- make_time_sequence_data(response_window, time_bin_size = 500, aois = "Animate", 
    +                                          predictor_columns = "Sex", 
    +                                          summarize_by = "ParticipantName")
    >                                          
    > # bootstrap resample 500 smoothed splines from the dataset,
    > # comparing females versus females at an alpha of .05                                         
    > df_bootstrapped <- make_boot_splines_data(response_time,
    +                                           predictor_column = 'Sex',
    +                                           within_subj = FALSE,
    +                                           bs_samples = 500,
    +                                           alpha = .05,
    +                                           smoother = "smooth.spline")
    Warning: Calling boot-splines from this function is deprecated. Please use `analyze_time_bins` for boot-splines.
    Error in smooth.spline(Time, Prop, cv = FALSE) : 
      missing or infinite values in inputs are not allowed
    Calls: make_boot_splines_data ... with -> with.default -> eval -> eval -> smooth.spline
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 45 SKIPPED: 0 FAILED: 9
      1. Error: The function analyze_time_bins has correct data (within) (@test_analyze_time_bins.R#69) 
      2. Error: The function analyze_time_bins has correct data (between) (@test_analyze_time_bins.R#93) 
      3. Error: The function analyze_time_bins has correct data (interaction) (@test_analyze_time_bins.R#104) 
      4. Failure: The function analyze_time_bins has necessary eyetrackingR attributes (@test_arbitrary_dv.R#67) 
      5. Error: The function analyze_time_clusters gives correct data with lm (@test_cluster_analysis.R#74) 
      6. Failure: The function make_time_cluster_data gives necessary eyetrackingR attributes with lmer (@test_cluster_analysis.R#95) 
      7. Failure: The function make_time_cluster_data gives necessary eyetrackingR attributes with t.test (@test_cluster_analysis.R#129) 
      8. Error: (unknown) (@test_cluster_analysis.R#135) 
      9. Failure: The function make_time_sequence_data has necessary eyetrackingR attributes (@test_time_sequence_data.R#33) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Warning in .check_fix_nobs(data = data, data_options = data_options, predictor_column = predictor_column,  :
      lm almost always requires no more than one observation per ParticipantName, per level of MCDI_Total. However, your data has more than this for some time-bins. Did you pass additional predictors to `make_time_sequence_data` that aren't being used here?
    Warning: Removed 97 rows containing non-finite values (stat_summary).
    Warning: Removed 97 rows containing non-finite values (stat_summary).
    Warning: Removed 1 rows containing missing values (geom_path).
    Warning: Removed 1 rows containing missing values (geom_path).
    Warning: Removed 1 rows containing missing values (geom_path).
    Warning: Removed 1 rows containing missing values (geom_path).
    Warning: Removed 1 rows containing missing values (geom_path).
    Warning: Removed 1 rows containing missing values (geom_path).
    Warning: Removed 1 rows containing missing values (geom_path).
    Warning: Removed 97 rows containing non-finite values (stat_summary).
    Warning: Removed 97 rows containing non-finite values (stat_summary).
    Warning: Removed 60 rows containing non-finite values (stat_summary).
    Warning: Removed 97 rows containing non-finite values (stat_summary).
    Warning: Removed 97 rows containing non-finite values (stat_summary).
    Warning: Removed 60 rows containing non-finite values (stat_summary).
    Quitting from lines 82-85 (onset_contingent_analysis_vignette.Rmd) 
    Error: processing vignette 'onset_contingent_analysis_vignette.Rmd' failed with diagnostics:
    fixation_window_length_rows >= 1 is not TRUE
    Execution halted
    ```

# ezsummary

Version: 0.2.1

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > 
    > library(dplyr)
    
    Attaching package: ‘dplyr’
    
    The following objects are masked from ‘package:stats’:
    
        filter, lag
    
    The following objects are masked from ‘package:base’:
    
        intersect, setdiff, setequal, union
    
    > dt <- mtcars %>% group_by(cyl) %>% select(gear, carb) %>% ezsummary_categorical(n=TRUE)
    Adding missing grouping variables: `cyl`
    > 
    > ezmarkup(dt, "...[.(.)]")
    Error in ezmarkup(dt, "...[.(.)]") : 
      The number of dots(.) you entered does not match up with the number of columns in the table. Please review your pattern expression. 
     Note: If you do want to Add an actual dot, please use `.` to denote that specific dot
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      19: stopc(pluralise_msg("Column(s) ", unknown_names), " not found")
      20: abort(paste0(...))
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 20 SKIPPED: 0 FAILED: 7
      1. Failure: ezsummary_c can't rename the variables for data with grouping info (@test-ezsummary_categorical.R#29) 
      2. Failure: ezsummary_c can't rename the variables for data with grouping info (@test-ezsummary_categorical.R#31) 
      3. Failure: ezsummary_c can't rename the variables for data with grouping info (@test-ezsummary_categorical.R#33) 
      4. Failure: ezsummary_c can't rename the variables for data with grouping info (@test-ezsummary_categorical.R#35) 
      5. Error: ezsummary_quantitative can evaluate grouping info correctly with 1 variable (@test-ezsummary_quantitative.R#19) 
      6. Error: ezsummary_quantitative can work with 2 variables with grouping info (@test-ezsummary_quantitative.R#33) 
      7. Error: ezsummary_quantitative can handle NAs when running with quantile=TRUE (@test-ezsummary_quantitative.R#40) 
      
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
    
    Quitting from lines 110-114 (intro_to_ezsummary_0.2.0.Rmd) 
    Error: processing vignette 'intro_to_ezsummary_0.2.0.Rmd' failed with diagnostics:
    Duplicate identifiers for rows (25, 26, 27), (31, 32, 33), (7, 8, 9), (13, 14, 15), (28, 29, 30), (10, 11, 12), (4, 5, 6), (19, 20, 21), (22, 23, 24), (16, 17, 18), (55, 56, 57), (61, 62, 63), (37, 38, 39), (43, 44, 45), (58, 59, 60), (40, 41, 42), (34, 35, 36), (49, 50, 51), (52, 53, 54), (46, 47, 48), (1, 2, 3)
    Execution halted
    ```

# factorMerger

Version: 0.3.6

## In both

*   checking PDF version of manual ... WARNING
    ```
    LaTeX errors when creating PDF version.
    This typically indicates Rd problems.
    LaTeX errors found:
    ! LaTeX Error: Command \k unavailable in encoding OT1.
    
    See the LaTeX manual or LaTeX Companion for explanation.
    Type  H <return>  for immediate help.
     ...                                              
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘forcats’ ‘formula.tools’
      All declared Imports should be used.
    ```

# fastLink

Version: 0.5.0

## In both

*   checking whether package ‘fastLink’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/fastLink/new/fastLink.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘fastLink’ ...
** package ‘fastLink’ successfully unpacked and MD5 sums checked
** libs
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romain/git/release/dplyr/revdep/library.noindex/fastLink/RcppArmadillo/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/fastLink/Rcpp/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/fastLink/RcppEigen/include" -I/usr/local/include  -fopenmp  -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘fastLink’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/fastLink/new/fastLink.Rcheck/fastLink’

```
### CRAN

```
* installing *source* package ‘fastLink’ ...
** package ‘fastLink’ successfully unpacked and MD5 sums checked
** libs
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romain/git/release/dplyr/revdep/library.noindex/fastLink/RcppArmadillo/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/fastLink/Rcpp/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/fastLink/RcppEigen/include" -I/usr/local/include  -fopenmp  -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘fastLink’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/fastLink/old/fastLink.Rcheck/fastLink’

```
# fastqcr

Version: 0.1.1

## In both

*   checking examples ... ERROR
    ```
    ...
      2  -- 1 columns 3 columns literal data
      3  -- 1 columns 3 columns literal data
      4  -- 1 columns 3 columns literal data
      5  -- 1 columns 3 columns literal data
    ... ... ......... ......... ............
    See problems(...) for more details.
    
    Warning: 17 parsing failures.
    row col  expected    actual         file
      1  -- 2 columns 3 columns literal data
      2  -- 2 columns 3 columns literal data
      3  -- 2 columns 3 columns literal data
      4  -- 2 columns 3 columns literal data
      5  -- 2 columns 3 columns literal data
    ... ... ......... ......... ............
    See problems(...) for more details.
    
    Warning in function_list[[i]](value) : NAs introduced by coercion
    Error in .f(.x[[i]], ...) : object 'module' not found
    Calls: qc_aggregate ... <Anonymous> -> vars_select_eval -> map_if -> map -> .f
    Execution halted
    ```

# fastR2

Version: 1.2.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.0Mb
      sub-directories of 1Mb or more:
        snippet   3.7Mb
    ```

# febr

Version: 1.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘knitr’
      All declared Imports should be used.
    ```

# fedregs

Version: 0.1.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── 4. Error: We can go all the way (@test-fedregs.R#145)  ──────────────────────
      There aren't any regulations for title 50 in 2012.
      1: cfr_text(good_year, good_title_number, good_chapter, good_part) at testthat/test-fedregs.R:145
      2: stop(sprintf("There aren't any regulations for title %s in %s.\n", title_number, 
             year)) at /Users/romain/git/release/dplyr/revdep/checks.noindex/fedregs/new/fedregs.Rcheck/00_pkg_src/fedregs/R/functions.R:244
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 19 SKIPPED: 0 FAILED: 4
      1. Failure: We get the best CFR URLs. (@test-fedregs.R#38) 
      2. Failure: We get the best CFR URLs. (@test-fedregs.R#39) 
      3. Error: We can parse some parts. (@test-fedregs.R#66) 
      4. Error: We can go all the way (@test-fedregs.R#145) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# FindMyFriends

Version: 1.10.0

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(testthat)
      > library(FindMyFriends)
      > 
      > test_check("FindMyFriends")
      ── 1. Failure: Neighborhood extraction works (@test-investigating.R#52)  ───────
      igraph::gsize(neigh) not equal to 12.
      1/1 mismatches
      [1] 121 - 12 == 109
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 295 SKIPPED: 1 FAILED: 1
      1. Failure: Neighborhood extraction works (@test-investigating.R#52) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking for code/documentation mismatches ... WARNING
    ```
    Functions or methods with usage in documentation object 'pgVirtual-class' but not in code:
      as
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.0Mb
      sub-directories of 1Mb or more:
        doc       1.5Mb
        extdata   1.8Mb
        R         2.1Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unexported objects imported by ':::' calls:
      ‘gtable:::insert.unit’ ‘gtable:::z_arrange_gtables’
      See the note in ?`:::` about the use of this operator.
    ```

# fingertipscharts

Version: 0.0.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘mapproj’
      All declared Imports should be used.
    ```

# fingertipsR

Version: 0.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘purrr’
      All declared Imports should be used.
    ```

# fitteR

Version: 0.1.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘ExtDist’
    ```

# flora

Version: 0.3.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.3Mb
      sub-directories of 1Mb or more:
        R   7.2Mb
    ```

# flowWorkspace

Version: 3.28.2

## In both

*   checking whether package ‘flowWorkspace’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘flowCore’ was built under R version 3.5.1
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/flowWorkspace/new/flowWorkspace.Rcheck/00install.out’ for details.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  8.7Mb
      sub-directories of 1Mb or more:
        doc    1.4Mb
        lib    2.1Mb
        libs   2.7Mb
        R      2.0Mb
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Versioned 'LinkingTo' values for
      ‘BH’ ‘cytolib’
    are only usable in R >= 3.0.2
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘graphics’ ‘grDevices’ ‘RBGL’
      All declared Imports should be used.
    Unexported objects imported by ':::' calls:
      ‘flowCore:::.estimateLogicle’ ‘flowCore:::checkClass’
      ‘flowCore:::copyFlowSet’ ‘flowCore:::guid’
      ‘flowCore:::logicle_transform’ ‘flowCore:::updateTransformKeywords’
      ‘graph:::.makeEdgeKeys’ ‘lattice:::updateList’
      ‘ncdfFlow:::.isValidSamples’ ‘stats:::.splinefun’
      See the note in ?`:::` about the use of this operator.
    There are ::: calls to the package's namespace in its code. A package
      almost never needs to use ::: for its own objects:
      ‘.cpp_setIndices’ ‘.getNodeInd’
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    show,flowJoWorkspace: no visible binding for global variable
      ‘groupName’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/flowWorkspace/new/flowWorkspace.Rcheck/00_pkg_src/flowWorkspace/R/flowJoWorkspace_Methods.R:66)
    transform,GatingSet: no visible global function definition for ‘is’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/flowWorkspace/new/flowWorkspace.Rcheck/00_pkg_src/flowWorkspace/R/GatingSet_Methods.R:2291-2296)
    transform,GatingSet: no visible global function definition for ‘is’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/flowWorkspace/new/flowWorkspace.Rcheck/00_pkg_src/flowWorkspace/R/GatingSet_Methods.R:2298-2308)
    transform,GatingSet : <anonymous>: no visible global function
      definition for ‘is’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/flowWorkspace/new/flowWorkspace.Rcheck/00_pkg_src/flowWorkspace/R/GatingSet_Methods.R:2301-2302)
    Undefined global functions or variables:
      . .hasSlot as as.formula callNextMethod desc extends gray groupName
      IQR is median new node old openCyto.count parallel sampleName
      selectMethod slot validObject xml.count
    Consider adding
      importFrom("grDevices", "gray")
      importFrom("methods", ".hasSlot", "as", "callNextMethod", "extends",
                 "is", "new", "selectMethod", "slot", "validObject")
      importFrom("stats", "as.formula", "IQR", "median")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

*   checking compiled code ... NOTE
    ```
    File ‘flowWorkspace/libs/flowWorkspace.so’:
      Found ‘__ZNSt3__14coutE’, possibly from ‘std::cout’ (C++)
        Object: ‘R_GatingSet.o’
    
    Compiled code should not call entry points which might terminate R nor
    write to stdout/stderr instead of to the console, nor use Fortran I/O
    nor system RNGs.
    
    See ‘Writing portable packages’ in the ‘Writing R Extensions’ manual.
    ```

# ForecastFramework

Version: 0.9.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
      number of items to replace is not a multiple of replacement length
    Warning in private$.dnames[[1]][rows] = rownames(data) :
      number of items to replace is not a multiple of replacement length
    Warning in private$.dnames[[1]][rows] = rownames(data) :
      number of items to replace is not a multiple of replacement length
    Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
      Running 'texi2dvi' on 'ClassDiagram.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `pbox.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    
    ! Emergency stop.
    <read *> 
             
    l.56 \usepackage
                    {adjustbox}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    Calls: buildVignettes -> texi2pdf -> texi2dvi
    Execution halted
    ```

# forestmangr

Version: 0.9.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘covr’ ‘curl’ ‘graphics’ ‘htmltools’ ‘psych’ ‘rmarkdown’ ‘scales’
      All declared Imports should be used.
    ```

# forwards

Version: 0.1.0

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Loading required package: xtable
    
    Attaching package: 'likert'
    
    The following object is masked from 'package:dplyr':
    
        recode
    
    Scale for 'x' is already present. Adding another scale for 'x', which
    will replace the existing scale.
    Quitting from lines 70-99 (Overview.Rmd) 
    Error: processing vignette 'Overview.Rmd' failed with diagnostics:
    invalid 'labels'; length 9 should be 1 or 0
    Execution halted
    ```

# fredr

Version: 1.0.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked UTF-8 string
    ```

# freqweights

Version: 1.0.4

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘freqweights-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: pcafreq
    > ### Title: Principal Component Analysis
    > ### Aliases: pcafreq .pcafreq
    > 
    > ### ** Examples
    > 
    > pcafreq(iris,  qualisup="Species", graph=TRUE)
    Error in plot.new() : figure margins too large
    Calls: pcafreq ... PCA -> plot.PCA -> plot -> plot.default -> plot.new
    Execution halted
    ```

# FRK

Version: 0.2.2

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Note: show_basis assumes spherical distance functions when plotting
    Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
      Running 'texi2dvi' on 'FRK_intro.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `subfig.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    
    ! Emergency stop.
    <read *> 
             
    l.58 \usepackage
                    {graphicx}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    Calls: buildVignettes -> texi2pdf -> texi2dvi
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking: ‘dggrids’ ‘INLA’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  9.7Mb
      sub-directories of 1Mb or more:
        data   5.3Mb
        doc    2.1Mb
        R      2.1Mb
    ```

# FSA

Version: 0.8.22

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘alr4’, ‘prettyR’, ‘RMark’, ‘PMCMR’, ‘pgirmess’, ‘agricolae’
    ```

# FSelectorRcpp

Version: 0.3.0

## In both

*   checking whether package ‘FSelectorRcpp’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/FSelectorRcpp/new/FSelectorRcpp.Rcheck/00install.out’ for details.
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘RTCGA.rnaseq’
    ```

## Installation

### Devel

```
* installing *source* package ‘FSelectorRcpp’ ...
** package ‘FSelectorRcpp’ successfully unpacked and MD5 sums checked
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romain/git/release/dplyr/revdep/library.noindex/FSelectorRcpp/Rcpp/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/FSelectorRcpp/BH/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/FSelectorRcpp/RcppArmadillo/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/FSelectorRcpp/testthat/include" -I/usr/local/include  -fopenmp -I../inst/include -fPIC  -Wno-unused-result -Wall -O3 -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘FSelectorRcpp’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/FSelectorRcpp/new/FSelectorRcpp.Rcheck/FSelectorRcpp’

```
### CRAN

```
* installing *source* package ‘FSelectorRcpp’ ...
** package ‘FSelectorRcpp’ successfully unpacked and MD5 sums checked
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romain/git/release/dplyr/revdep/library.noindex/FSelectorRcpp/Rcpp/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/FSelectorRcpp/BH/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/FSelectorRcpp/RcppArmadillo/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/FSelectorRcpp/testthat/include" -I/usr/local/include  -fopenmp -I../inst/include -fPIC  -Wno-unused-result -Wall -O3 -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘FSelectorRcpp’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/FSelectorRcpp/old/FSelectorRcpp.Rcheck/FSelectorRcpp’

```
# ftDK

Version: 1.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 39 marked UTF-8 strings
    ```

# furniture

Version: 1.8.7

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘magrittr’
      All declared Imports should be used.
    ```

# furrowSeg

Version: 1.8.0

## In both

*   checking whether package ‘furrowSeg’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘EBImage’ was built under R version 3.5.1
      Warning: replacing previous import ‘EBImage::abind’ by ‘abind::abind’ when loading ‘furrowSeg’
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/furrowSeg/new/furrowSeg.Rcheck/00install.out’ for details.
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Warning in has_utility("pdfcrop") :
      pdfcrop not installed or not in PATH
    Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
      Running 'texi2dvi' on 'exampleFurrowSegmentation.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `float.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    
    ! Emergency stop.
    <read *> 
             
    l.30 \date
              {}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    Calls: buildVignettes -> texi2pdf -> texi2dvi
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 362.4Mb
      sub-directories of 1Mb or more:
        data  361.3Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/furrowSeg/new/furrowSeg.Rcheck/00_pkg_src/furrowSeg/R/featureAnalysis.R:76-77)
    plotFeatureEvolution: no visible global function definition for
      ‘polygon’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/furrowSeg/new/furrowSeg.Rcheck/00_pkg_src/furrowSeg/R/featureAnalysis.R:78-79)
    plotFeatureEvolution: no visible global function definition for ‘rgb’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/furrowSeg/new/furrowSeg.Rcheck/00_pkg_src/furrowSeg/R/featureAnalysis.R:78-79)
    plotFeatureEvolution: no visible global function definition for ‘axis’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/furrowSeg/new/furrowSeg.Rcheck/00_pkg_src/furrowSeg/R/featureAnalysis.R:80)
    plotFeatureEvolution: no visible global function definition for ‘mtext’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/furrowSeg/new/furrowSeg.Rcheck/00_pkg_src/furrowSeg/R/featureAnalysis.R:81)
    plotFeatureEvolution: no visible global function definition for ‘title’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/furrowSeg/new/furrowSeg.Rcheck/00_pkg_src/furrowSeg/R/featureAnalysis.R:84)
    Undefined global functions or variables:
      abline axis median mtext par plot points polygon predict quantile rgb
      title
    Consider adding
      importFrom("graphics", "abline", "axis", "mtext", "par", "plot",
                 "points", "polygon", "title")
      importFrom("grDevices", "rgb")
      importFrom("stats", "median", "predict", "quantile")
    to your NAMESPACE file.
    ```

# fuzzyjoin

Version: 0.1.4

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Lengths differ: 6 is not 5
      
      ── 4. Failure: stringdist_inner_join works with multiple match functions (@test_
      result$cut2 not equal to c("Faiir", "VeryGood", "Premiom", "Premiums", "Idea").
      Lengths differ: 6 is not 5
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 222 SKIPPED: 0 FAILED: 4
      1. Failure: stringdist_inner_join works on a large df with multiples in each (@test_stringdist_join.R#16) 
      2. Failure: stringdist_inner_join works on a large df with multiples in each (@test_stringdist_join.R#17) 
      3. Failure: stringdist_inner_join works with multiple match functions (@test_stringdist_join.R#130) 
      4. Failure: stringdist_inner_join works with multiple match functions (@test_stringdist_join.R#131) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# GA4GHclient

Version: 1.4.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 18 SKIPPED: 0 FAILED: 82
      1. Error: getBiosample works (@test-getBiosample.R#6) 
      2. Error: getCallSet works (@test-getCallSet.R#6) 
      3. Error: getDataset works (@test-getDataset.R#6) 
      4. Error: getExpressionLevel works (@test-getExpressionLevel.R#6) 
      5. Error: getFeature works (@test-getFeature.R#6) 
      6. Error: getFeatureSet works (@test-getFeatureSet.R#6) 
      7. Error: getIndividual works (@test-getIndividual.R#6) 
      8. Error: getReadGroupSet works (@test-getReadGroupSet.R#6) 
      9. Error: getReference works (@test-getReference.R#6) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    The following objects are masked from 'package:base':
    
        anyDuplicated, append, as.data.frame, basename, cbind, colMeans,
        colnames, colSums, dirname, do.call, duplicated, eval, evalq,
        Filter, Find, get, grep, grepl, intersect, is.unsorted, lapply,
        lengths, Map, mapply, match, mget, order, paste, pmax, pmax.int,
        pmin, pmin.int, Position, rank, rbind, Reduce, rowMeans,
        rownames, rowSums, sapply, setdiff, sort, table, tapply, union,
        unique, unsplit, which, which.max, which.min
    
    
    Attaching package: 'S4Vectors'
    
    The following object is masked from 'package:base':
    
        expand.grid
    
    Quitting from lines 129-133 (GA4GHclient.Rmd) 
    Error: processing vignette 'GA4GHclient.Rmd' failed with diagnostics:
    there is no package called 'org.Hs.eg.db'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      ‘org.Hs.eg.db’ ‘TxDb.Hsapiens.UCSC.hg19.knownGene’
    ```

# GA4GHshiny

Version: 1.2.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      2: stop(txt, domain = NA)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 2 SKIPPED: 0 FAILED: 8
      1. Error: app works (@test-app.R#5) 
      2. Error: getGene works (@test-getGene.R#4) 
      3. Error: getGeneSymbols works (@test-getGeneSymbols.R#4) 
      4. Error: initializeReferences works (@test-initializeReferences.R#6) 
      5. Error: initializeVariantSet works (@test-initializeVariantSet.R#6) 
      6. Error: (unknown) (@test-searchVariantsByGeneSymbol.R#3) 
      7. Error: tidyVariants works with searchVariants output (@test-tidyVariants.R#6) 
      8. Error: tidyVariants works with searchVariantsByGeneSymbol output (@test-tidyVariants.R#16) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      ‘org.Hs.eg.db’ ‘TxDb.Hsapiens.UCSC.hg19.knownGene’
    ```

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .travis.yml
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

# gaiah

Version: 0.0.2

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    extrc_+                        pred = "predkrig",
    extrc_+                        sd = "stdkrig")
    > 
    > # If this were run it gives an error because there is only 1 bird at the
    > # location "Charlevoix"
    > ## Not run: 
    > ##D group_birds_by_location(x, feather_isotope_col = "Isotope.Value", location_col = "Location")
    > ## End(Not run)
    > 
    > 
    > # remove that one bird at Charlevoix and re-run
    > y <- x %>%
    +   dplyr::filter(Location != "Charlevoix")
    > 
    > # then group birds by location
    > gbl <- group_birds_by_location(D = y,
    +                                feather_isotope_col = "Isotope.Value",
    +                                location_col = "Location")
    Error in n() : could not find function "n"
    Calls: group_birds_by_location ... <Anonymous> -> <Anonymous> -> summarise_.tbl_df -> summarise_impl
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘ggplot2’ ‘maptools’ ‘rgeos’ ‘stringr’ ‘tidyr’
      All declared Imports should be used.
    ```

# gastempt

Version: 0.4.3

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.7Mb
      sub-directories of 1Mb or more:
        libs   7.2Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘methods’ ‘rstantools’
      All declared Imports should be used.
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# geex

Version: 1.0.11

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘rootSolve’
      All declared Imports should be used.
    ```

# genBaRcode

Version: 1.1.0

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘genBaRcode-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plotClusterGgTree
    > ### Title: Plotting a Cluster ggTree
    > ### Aliases: plotClusterGgTree
    > 
    > ### ** Examples
    > 
    > data(BC_dat)
    > plotClusterGgTree(BC_dat, tree_est = "UPGMA", type = "circular")
    Error: object ‘as_data_frame’ is not exported by 'namespace:tidytree'
    Execution halted
    ```

# gender

Version: 0.5.2

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘genderdata’
    ```

# GENESIS

Version: 2.10.1

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > 
    > # simulate some phenotype data
    > data(pedigree)
    > pedigree <- pedigree[match(seqGetData(gds, "sample.id"), pedigree$sample.id),]
    > pedigree$outcome <- rnorm(nrow(pedigree))
    > 
    > # construct a SeqVarData object
    > seqData <- SeqVarData(gds, sampleData=AnnotatedDataFrame(pedigree))
    > 
    > # fit the null model
    > nullmod <- fitNullModel(seqData, outcome="outcome", covars="sex")
    > 
    > # burden test - Range Iterator
    > gr <- GRanges(seqnames=rep(1,3), ranges=IRanges(start=c(1e6, 2e6, 3e6), width=1e6))
    > iterator <- SeqVarRangeIterator(seqData, variantRanges=gr)
    # of selected variants: 3
    > assoc <- assocTestAggregate(iterator, nullmod, test="Burden")
    # of selected samples: 90
    Error in n() : could not find function "n"
    Calls: assocTestAggregate ... variantInfo -> .local -> mutate_ -> mutate_.tbl_df -> mutate_impl
    Execution halted
    ```

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 224 SKIPPED: 0 FAILED: 16
      1. Error: window (@test_assocTestAggregate.R#10) 
      2. Error: ranges (@test_assocTestAggregate.R#24) 
      3. Error: list (@test_assocTestAggregate.R#40) 
      4. Error: user weights (@test_assocTestAggregate.R#56) 
      5. Error: exclude monomorphs (@test_assocTestAggregate.R#71) 
      6. Error: exclude common (@test_assocTestAggregate.R#84) 
      7. Error: select alleles (@test_assocTestAggregate.R#92) 
      8. Error: select alleles with mismatch (@test_assocTestAggregate.R#135) 
      9. Error: select alleles with duplicate variants (@test_assocTestAggregate.R#162) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘GWASdata’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unexported object imported by a ':::' call: ‘survey:::saddle’
      See the note in ?`:::` about the use of this operator.
    ```

# geneXtendeR

Version: 1.6.0

## In both

*   checking package dependencies ... ERROR
    ```
    Packages required but not available:
      ‘GO.db’ ‘org.Rn.eg.db’ ‘org.Ag.eg.db’ ‘org.Bt.eg.db’ ‘org.Ce.eg.db’
      ‘org.Cf.eg.db’ ‘org.Dm.eg.db’ ‘org.Dr.eg.db’ ‘org.Gg.eg.db’
      ‘org.Hs.eg.db’ ‘org.Mm.eg.db’ ‘org.Mmu.eg.db’ ‘org.Pt.eg.db’
      ‘org.Sc.sgd.db’ ‘org.Ss.eg.db’ ‘org.Xl.eg.db’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# GenomicDataCommons

Version: 1.4.3

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Loading required package: magrittr
    
    Attaching package: 'GenomicDataCommons'
    
    The following object is masked from 'package:stats':
    
        filter
    
    Quitting from lines 145-146 (overview.Rmd) 
    Error: processing vignette 'overview.Rmd' failed with diagnostics:
    failed to rename downloaded file:
    
      from: '/Users/romain/Library/Caches/GenomicDataCommons/82e1c15b-77f7-47c6-ad9f-d8d3cc980ffd/.partial_download'
      to: '/Users/romain/Library/Caches/GenomicDataCommons/82e1c15b-77f7-47c6-ad9f-d8d3cc980ffd/78362a69-6a41-4c14-8e08-4fd7773ce03a.htseq.counts.gz'
      reason:
        cannot rename file
        '/Users/romain/Library/Caches/GenomicDataCommons/82e1c15b-77f7-47c6-ad9f-d8d3cc980ffd/.partial_download'
        to
        '/Users/romain/Library/Caches/GenomicDataCommons/82e1c15b-77f7-47c6-ad9f-d8d3cc980ffd/78362a69-6a41-4c14-8e08-4fd7773ce03a.htseq.counts.gz',
        reason 'No such file or directory'
    Execution halted
    ```

## In both

*   checking Rd \usage sections ... WARNING
    ```
    Undocumented arguments in documentation object '.htseq_importer'
      ‘fnames’
    
    Functions with \usage entries need to have the appropriate \alias
    entries, and all their arguments documented.
    The \usage entries must correspond to syntactically valid R code.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking R code for possible problems ... NOTE
    ```
    default_fields.character: no visible binding for global variable
      ‘defaults’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/GenomicDataCommons/new/GenomicDataCommons.Rcheck/00_pkg_src/GenomicDataCommons/R/fields.R:51)
    gdc_rnaseq: no visible binding for global variable ‘case_id’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/GenomicDataCommons/new/GenomicDataCommons.Rcheck/00_pkg_src/GenomicDataCommons/R/gdc_rnaseq.R:106-107)
    gdc_rnaseq: no visible binding for global variable ‘file_id’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/GenomicDataCommons/new/GenomicDataCommons.Rcheck/00_pkg_src/GenomicDataCommons/R/gdc_rnaseq.R:106-107)
    Undefined global functions or variables:
      case_id defaults file_id
    ```

# GenomicInteractions

Version: 1.14.0

## In both

*   checking whether package ‘GenomicInteractions’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘GenomicRanges’ was built under R version 3.5.1
      Warning: package ‘IRanges’ was built under R version 3.5.1
      Warning: package ‘DelayedArray’ was built under R version 3.5.1
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/GenomicInteractions/new/GenomicInteractions.Rcheck/00install.out’ for details.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 11.4Mb
      sub-directories of 1Mb or more:
        doc       1.8Mb
        extdata   7.9Mb
    ```

# GenomicMating

Version: 2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# GEOmetadb

Version: 1.42.0

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
    getSQLiteFile: no visible global function definition for
      ‘download.file’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/GEOmetadb/new/GEOmetadb.Rcheck/00_pkg_src/GEOmetadb/R/getSQLiteFile.R:6)
    Undefined global functions or variables:
      download.file
    Consider adding
      importFrom("utils", "download.file")
    to your NAMESPACE file.
    ```

# geomnet

Version: 0.2.0

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘geomnet-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: jtt
    > ### Title: Number of JTTs in a graph
    > ### Aliases: jtt
    > 
    > ### ** Examples
    > 
    > data(blood)
    > ggplot(data= blood$edges) + geom_net(aes(from_id=from, to_id=to), directed=TRUE) + theme_net()
    Error in n() : could not find function "n"
    Calls: <Anonymous> ... f -> <Anonymous> -> summarise.tbl_df -> summarise_impl
    Execution halted
    ```

# GEOquery

Version: 2.48.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 13.9Mb
      sub-directories of 1Mb or more:
        extdata  12.8Mb
    ```

*   checking whether the namespace can be loaded with stated dependencies ... NOTE
    ```
    Warning: no function found corresponding to methods exports from ‘GEOquery’ for: ‘show’
    
    A namespace must be able to be loaded with just the base namespace
    loaded: otherwise if the namespace gets loaded by a saved object, the
    session will be unable to start.
    
    Probably some imports need to be declared in the NAMESPACE file.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘httr’
      All declared Imports should be used.
    Package in Depends field not imported from: ‘methods’
      These packages need to be imported from (in the NAMESPACE file)
      for when this namespace is loaded but not attached.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/GEOquery/new/GEOquery.Rcheck/00_pkg_src/GEOquery/R/parseGEO.R:531-539)
    parseGSEMatrix: no visible binding for global variable ‘accession’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/GEOquery/new/GEOquery.Rcheck/00_pkg_src/GEOquery/R/parseGEO.R:531-539)
    parseGSEMatrix: no visible binding for global variable ‘accession’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/GEOquery/new/GEOquery.Rcheck/00_pkg_src/GEOquery/R/parseGEO.R:541-542)
    parseGSEMatrix: no visible global function definition for ‘new’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/GEOquery/new/GEOquery.Rcheck/00_pkg_src/GEOquery/R/parseGEO.R:568)
    parseGSEMatrix: no visible global function definition for ‘new’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/GEOquery/new/GEOquery.Rcheck/00_pkg_src/GEOquery/R/parseGEO.R:590)
    parseGSEMatrix: no visible global function definition for ‘new’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/GEOquery/new/GEOquery.Rcheck/00_pkg_src/GEOquery/R/parseGEO.R:606-610)
    parseGSEMatrix: no visible global function definition for ‘as’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/GEOquery/new/GEOquery.Rcheck/00_pkg_src/GEOquery/R/parseGEO.R:606-610)
    Undefined global functions or variables:
      . accession as characteristics k kvpair MA new read.delim read.table
      v
    Consider adding
      importFrom("methods", "as", "new")
      importFrom("utils", "read.delim", "read.table")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

# geoSpectral

Version: 0.17.4

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        R           2.0Mb
        test_data   2.9Mb
    ```

# GerminaR

Version: 1.2

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘GerminaR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ger_intime
    > ### Title: Cumulative sum of germination by period of time for line graphic
    > ### Aliases: ger_intime
    > 
    > ### ** Examples
    > 
    > 
    > library(GerminaR)
    > dt <- prosopis
    > grt <- ger_intime(Factor = "nacl", SeedN = "seeds", 
    +                   evalName = "D", method = "percentage", data = dt)
    Error in n() : could not find function "n"
    Calls: ger_intime ... <Anonymous> -> <Anonymous> -> summarise.tbl_df -> summarise_impl
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 202-205 (GerminaR.Rmd) 
    Error: processing vignette 'GerminaR.Rmd' failed with diagnostics:
    could not find function "n"
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘DT’ ‘shinydashboard’
      All declared Imports should be used.
    ```

# gespeR

Version: 1.12.0

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
    ...
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/gespeR/new/gespeR.Rcheck/00_pkg_src/gespeR/R/gespeR-functions.R:42-43)
    .select.model: no visible global function definition for ‘predict’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/gespeR/new/gespeR.Rcheck/00_pkg_src/gespeR/R/gespeR-functions.R:236)
    concordance: no visible global function definition for ‘cor’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/gespeR/new/gespeR.Rcheck/00_pkg_src/gespeR/R/gespeR-concordance.R:65)
    lasso.rand: no visible global function definition for ‘runif’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/gespeR/new/gespeR.Rcheck/00_pkg_src/gespeR/R/gespeR-functions.R:137-145)
    plot.gespeR: no visible global function definition for ‘hist’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/gespeR/new/gespeR.Rcheck/00_pkg_src/gespeR/R/gespeR-class.R:218)
    stability.selection: no visible global function definition for ‘lm’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/gespeR/new/gespeR.Rcheck/00_pkg_src/gespeR/R/gespeR-functions.R:214)
    Phenotypes,character: no visible global function definition for
      ‘read.delim’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/gespeR/new/gespeR.Rcheck/00_pkg_src/gespeR/R/Phenotypes-class.R:75)
    Undefined global functions or variables:
      coef cor hist lm predict read.delim runif
    Consider adding
      importFrom("graphics", "hist")
      importFrom("stats", "coef", "cor", "lm", "predict", "runif")
      importFrom("utils", "read.delim")
    to your NAMESPACE file.
    ```

# GetHFData

Version: 1.6

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘GetHFData-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ghfd_read_file
    > ### Title: Reads zip file downloaded from Bovespa ftp (trades or orders)
    > ### Aliases: ghfd_read_file
    > 
    > ### ** Examples
    > 
    > 
    > my.assets <- c('ABEVA20', 'PETRL78')
    > 
    > ## getting data from local file (in practice it would be downloaded from ftp)
    > out.file <- system.file("extdata", 'NEG_OPCOES_20151126.zip', package = "GetHFData")
    > 
    > df.out <- ghfd_read_file(out.file, my.assets)
     - Imported  18301 lines, 770 unique tickers
       -> Processing file - Found 123 lines for 2 selected tickersError: Column `last.price` must be length 1 (a summary value), not 0
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
                 1] - 1, na.rm = T), sum.qtd = sum(TradedQuantity), sum.vol = sum(TradedQuantity * 
                 TradePrice), n.buys = sum(TradeSign == 1), n.sells = sum(TradeSign == -1)) at /Users/romain/git/release/dplyr/revdep/checks.noindex/GetHFData/new/GetHFData.Rcheck/00_pkg_src/GetHFData/R/ghfd_read_file.R:241
      4: summarise.tbl_df(my.G, n.trades = length(TradePrice), last.price = TradePrice[length(TradePrice)], 
             weighted.price = sum(TradePrice * (TradedQuantity * TradePrice)/sum(TradedQuantity * 
                 TradePrice)), period.ret = last.price/TradePrice[1] - 1, period.ret.volat = stats::sd(TradePrice[2:length(TradePrice)]/TradePrice[1:length(TradePrice) - 
                 1] - 1, na.rm = T), sum.qtd = sum(TradedQuantity), sum.vol = sum(TradedQuantity * 
                 TradePrice), n.buys = sum(TradeSign == 1), n.sells = sum(TradeSign == -1)) at /Users/romain/git/tidyverse/dplyr/R/manip.r:255
      5: summarise_impl(.data, dots) at /Users/romain/git/tidyverse/dplyr/R/tbl-df.r:102
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 0 SKIPPED: 0 FAILED: 1
      1. Error: (unknown) (@test_ghfd.R#10) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# GetITRData

Version: 0.7

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 99-112 (gitrd-vignette-introduction.Rmd) 
    Error: processing vignette 'gitrd-vignette-introduction.Rmd' failed with diagnostics:
    argument is of length zero
    Execution halted
    ```

# getProxy

Version: 1.12

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘bitops’ ‘data.table’ ‘dplyr’ ‘httr’
      All declared Imports should be used.
    ```

# getTBinR

Version: 0.5.6

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      8: function_list[[i]](value)
      9: mutate(., id = 1:n())
      10: mutate.tbl_df(., id = 1:n()) at /Users/romain/git/tidyverse/dplyr/R/manip.r:416
      11: mutate_impl(.data, dots) at /Users/romain/git/tidyverse/dplyr/R/tbl-df.r:91
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 78 SKIPPED: 27 FAILED: 2
      1. Error: summarise_tb_burden can summarise a variable
                with confidence intervals for all regions and globally (@test-summarise_tb_burden.R#100) 
      2. Error: summarise_tb_burden can summarise a variable
                with confidence intervals for all regions and globally then compare to a
                shortlist of countries (@test-summarise_tb_burden.R#116) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# ggalluvial

Version: 0.9.1

## Newly broken

*   R CMD check timed out
    

# ggalt

Version: 0.4.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘plotly’
      All declared Imports should be used.
    ```

# ggdag

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘ggforce’ ‘plyr’
      All declared Imports should be used.
    ```

# ggdistribute

Version: 1.0.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# ggedit

Version: 0.3.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘magrittr’
      All declared Imports should be used.
    ```

# ggeffects

Version: 0.7.0

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### ** Examples
    > 
    > data(efc)
    > fit <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = efc)
    > 
    > ggpredict(fit, terms = "c12hour")
    
    # Predicted values of Total score BARTHEL INDEX 
    # x = average number of hours of care per week 
    
       x predicted std.error conf.low conf.high
       0    75.444     1.116   73.257    77.630
      20    70.378     0.925   68.564    72.191
      45    64.045     0.843   62.393    65.697
      65    58.979     0.930   57.157    60.802
      85    53.913     1.122   51.713    56.113
     105    48.847     1.377   46.148    51.546
     125    43.781     1.665   40.517    47.045
     170    32.382     2.373   27.732    37.033
    
    Adjusted for:
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      9: function_list[[i]](value)
      10: tidyr::unnest(.)
      11: unnest.data.frame(.)
      12: abort("All nested columns must have the same number of elements.")
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 36 SKIPPED: 47 FAILED: 5
      1. Error: ggaverage, glm (@test-glm.R#30) 
      2. Error: ggaverage, glmer (@test-glmer.R#29) 
      3. Error: ggaverage, lm (@test-linear-models.R#54) 
      4. Error: ggaverage, lmer (@test-lmer.R#27) 
      5. Error: ggaverage, glm (@test-poisson.R#19) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Model has log-transformed predictors. Consider using `terms="hp [exp]"` to back-transform scale.
    Model has log-transformed predictors. Consider using `terms="hp [exp]"` to back-transform scale.
    Model has log-transformed predictors. Consider using `terms="hp [exp]"` to back-transform scale.
    Install package "strengejacke" from GitHub (`devtools::install_github("strengejacke/strengejacke")`) to load all sj-packages at once!
    Loading required package: Matrix
    singular fit
    Model contains polynomial or cubic / quadratic terms. Consider using `terms="c12hour [all]"` to if you want smooth plots. See also package-vignette 'Marginal Effects at Specific Values'.
    Model contains polynomial or cubic / quadratic terms. Consider using `terms="c12hour [all]"` to if you want smooth plots. See also package-vignette 'Marginal Effects at Specific Values'.
    Model contains polynomial or cubic / quadratic terms. Consider using `terms="c12hour [all]"` to if you want smooth plots. See also package-vignette 'Marginal Effects at Specific Values'.
    Model contains polynomial or cubic / quadratic terms. Consider using `terms="c12hour [all]"` to if you want smooth plots. See also package-vignette 'Marginal Effects at Specific Values'.
    Model contains polynomial or cubic / quadratic terms. Consider using `terms="c12hour [all]"` to if you want smooth plots. See also package-vignette 'Marginal Effects at Specific Values'.
    Model contains polynomial or cubic / quadratic terms. Consider using `terms="c12hour [all]"` to if you want smooth plots. See also package-vignette 'Marginal Effects at Specific Values'.
    Quitting from lines 91-93 (marginaleffects.Rmd) 
    Error: processing vignette 'marginaleffects.Rmd' failed with diagnostics:
    All nested columns must have the same number of elements.
    Execution halted
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘ordinal’
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

*   checking re-building of vignette outputs ... NOTE
    ```
    ...
      conversion failure on 'Ondřej Chochola' in 'mbcsToSbcs': dot substituted for <99>
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      conversion failure on 'Ondřej Chochola' in 'mbcsToSbcs': dot substituted for <c5>
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      conversion failure on 'Ondřej Chochola' in 'mbcsToSbcs': dot substituted for <99>
    Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
      Running 'texi2dvi' on 'ggenealogy.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `media9.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    
    ! Emergency stop.
    <read *> 
             
    l.4 ^^M
           
    !  ==> Fatal error occurred, no output PDF file produced!
    Calls: buildVignettes -> texi2pdf -> texi2dvi
    Execution halted
    ```

# ggfan

Version: 0.1.2

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    
    > ### Name: calc_quantiles
    > ### Title: Calculate quantiles of a tidy dataframe
    > ### Aliases: calc_quantiles
    > 
    > ### ** Examples
    > 
    > 
    > head(fake_df)
      x Sim           y
    1 1  X1  1.05953360
    2 2  X1 -1.00586200
    3 3  X1 -0.02297916
    4 4  X1  1.09988604
    5 5  X1  2.15035110
    6 6  X1  1.51674379
    > 
    > fake_q <- calc_quantiles(fake_df, intervals=c(0,0.5,0.8))
    Error in n() : could not find function "n"
    Calls: calc_quantiles -> <Anonymous> -> summarise.tbl_df -> summarise_impl
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      [1] 0 - 1 == -1
      
      ── 4. Failure: stat_interval filters precomputed quantiles to match interval arg
      unique(p_b$data[[1]]$interval) not equal to as.factor(0.2).
      target is NULL, current is factor
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 7 SKIPPED: 0 FAILED: 4
      1. Error: calc_quantile returns df with x and y (@test_calc_quantile.R#8) 
      2. Error: calc_quantile allows retention of original df names (@test_calc_quantile.R#17) 
      3. Failure: stat_interval filters precomputed quantiles to match interval args (@test_stat_interval.R#30) 
      4. Failure: stat_interval filters precomputed quantiles to match interval args (@test_stat_interval.R#31) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘colorspace’ ‘grid’ ‘rstan’
      All declared Imports should be used.
    ```

# ggformula

Version: 0.9.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.0Mb
      sub-directories of 1Mb or more:
        doc   2.7Mb
        R     2.0Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tidyr’
      All declared Imports should be used.
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

# gginnards

Version: 0.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘grid’ ‘tibble’
      All declared Imports should be used.
    ```

# ggiraph

Version: 0.6.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘rvg’
    ```

# ggiraphExtra

Version: 0.2.9

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘ggforce’ ‘webshot’ ‘ztable’
      All declared Imports should be used.
    ```

# gglogo

Version: 0.1.3

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    
    Attaching package: 'gglogo'
    
    The following object is masked from 'package:ggplot2':
    
        fortify
    
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      no font could be found for family "Garamond"
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      no font could be found for family "Garamond"
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      no font could be found for family "Garamond"
    Quitting from lines 46-52 (gglogo-alphabet.Rmd) 
    Error: processing vignette 'gglogo-alphabet.Rmd' failed with diagnostics:
    replacement has 1 row, data has 0
    Execution halted
    ```

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
    
    > ### Name: get_map
    > ### Title: Grab a map.
    > ### Aliases: get_map
    > 
    > ### ** Examples
    > 
    > map <- get_map()
    Warning in download.file(url, destfile = tmp, quiet = !messaging, mode = "wb") :
      cannot open URL 'http://maps.googleapis.com/maps/api/staticmap?center=29.763284,-95.363271&zoom=10&size=640x640&scale=2&maptype=terrain&language=en-EN&sensor=false': HTTP status was '403 Forbidden'
    Error in download.file(url, destfile = tmp, quiet = !messaging, mode = "wb") : 
      cannot open URL 'http://maps.googleapis.com/maps/api/staticmap?center=29.763284,-95.363271&zoom=10&size=640x640&scale=2&maptype=terrain&language=en-EN&sensor=false'
    Calls: get_map -> get_googlemap -> download.file
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        data   4.4Mb
    ```

# ggmcmc

Version: 1.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
      no font could be found for family "Source Sans Pro"
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      no font could be found for family "Source Sans Pro"
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      no font could be found for family "Source Sans Pro"
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      no font could be found for family "Source Sans Pro"
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      no font could be found for family "Source Sans Pro"
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      no font could be found for family "Source Sans Pro"
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      no font could be found for family "Source Sans Pro"
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      no font could be found for family "Source Sans Pro"
    Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y,  :
      no font could be found for family "Source Sans Pro"
    Quitting from lines 142-143 (using_ggmcmc.Rmd) 
    Error: processing vignette 'using_ggmcmc.Rmd' failed with diagnostics:
    polygon edge not found
    Execution halted
    ```

# ggnormalviolin

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘grid’ ‘scales’
      All declared Imports should be used.
    ```

# ggplot2

Version: 3.1.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.3Mb
      sub-directories of 1Mb or more:
        doc   1.8Mb
        R     3.8Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘mgcv’ ‘reshape2’ ‘viridisLite’
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

# ggpmisc

Version: 0.3.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘gginnards’
    ```

# ggpol

Version: 0.0.4

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘grDevices’
      All declared Imports should be used.
    ```

# ggpubr

Version: 0.2

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘FactoMineR’
    ```

# ggQQunif

Version: 0.1.5

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# ggquickeda

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘colourpicker’ ‘dplyr’ ‘DT’ ‘Formula’ ‘ggpmisc’ ‘ggrepel’ ‘grDevices’
      ‘gridExtra’ ‘Hmisc’ ‘lazyeval’ ‘markdown’ ‘plotly’ ‘quantreg’ ‘rlang’
      ‘shinyjs’ ‘table1’ ‘tidyr’
      All declared Imports should be used.
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

Version: 1.0.2

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.5Mb
      sub-directories of 1Mb or more:
        doc   3.0Mb
        R     2.0Mb
    ```

# ggridges

Version: 0.5.1

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 6242 marked UTF-8 strings
    ```

# ggsolvencyii

Version: 0.1.1

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > library(ggplot2)
    > 
    > ## see details about id and comparewithid
    > # sii_z_ex3_data[sii_z_ex3_data$description == "SCR", ]
    > 
    > ggplot()+
    + geom_sii_riskoutline(data = sii_z_ex3_data, mapping = aes(
    +   # comparewithid = comparewithid,
    +   x = time,
    +   y = ratio,
    +   value = value,
    +   id = id,
    +   description = description),
    + color = "red",
    + lwd = 0.7
    + )
    scaling is based on a max (level= 1) value of 138.064462809917
    comparewithid is set to reference itself
    Error in n() : could not find function "n"
    Calls: <Anonymous> ... <Anonymous> -> <Anonymous> -> summarise.tbl_df -> summarise_impl
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 285 SKIPPED: 0 FAILED: 9
      1. Error: (unknown) (@test_10_setupdata_A_expansion_integration.R#36) 
      2. Error: (unknown) (@test_10_setupdata_B_surface.R#54) 
      3. Error: (unknown) (@test_10_setupdata_C_outline.R#57) 
      4. Error: (unknown) (@test_20_computegroup_Aa_fncornerpoints.R#53) 
      5. Error: (unknown) (@test_20_computegroup_Ab_fnrotation.R#53) 
      6. Error: (unknown) (@test_20_computegroup_Ac_fnsquareddegrees.R#53) 
      7. Error: (unknown) (@test_20_computegroup_Ad_fnsquareconversion.R#53) 
      8. Error: (unknown) (@test_20_computegroup_Ae_fnpolygonpoints_and_subroutines.R#92) 
      9. Error: (unknown) (@test_50_statandgeom_.R#54) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 103-130 (coding_overview.Rmd) 
    Error: processing vignette 'coding_overview.Rmd' failed with diagnostics:
    could not find function "n"
    Execution halted
    ```

# ggspatial

Version: 1.0.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘reshape2’ ‘rosm’
      All declared Imports should be used.
    ```

# ggstatsplot

Version: 0.0.7

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘glmmTMB’
      All declared Imports should be used.
    ```

# ggthemes

Version: 4.0.1

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 138 marked UTF-8 strings
    ```

# ggtree

Version: 1.12.7

## In both

*   checking whether package ‘ggtree’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/ggtree/new/ggtree.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘ggtree’ ...
** R
** inst
** byte-compile and prepare package for lazy loading
Error : object ‘as_data_frame’ is not exported by 'namespace:tidytree'
ERROR: lazy loading failed for package ‘ggtree’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/ggtree/new/ggtree.Rcheck/ggtree’

```
### CRAN

```
* installing *source* package ‘ggtree’ ...
** R
** inst
** byte-compile and prepare package for lazy loading
Error : object ‘as_data_frame’ is not exported by 'namespace:tidytree'
ERROR: lazy loading failed for package ‘ggtree’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/ggtree/old/ggtree.Rcheck/ggtree’

```
# ggvis

Version: 0.4.4

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### ** Examples
    > 
    > library(dplyr)
    
    Attaching package: ‘dplyr’
    
    The following objects are masked from ‘package:stats’:
    
        filter, lag
    
    The following objects are masked from ‘package:base’:
    
        intersect, setdiff, setequal, union
    
    > base <- mtcars %>% ggvis(~mpg, ~cyl) %>% layer_points()
    > base %>% group_by(cyl) %>% summarise(mpg = mean(mpg)) %>%
    +   layer_points(fill := "red", size := 100)
    > 
    > base %>% filter(mpg > 25) %>% layer_points(fill := "red")
    Error: `.preserve` (`.preserve = FALSE`) must not be named, do you need `==`?
    Execution halted
    ```

# ggwordcloud

Version: 0.3.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 102 marked UTF-8 strings
    ```

# glmmfields

Version: 0.1.1

## In both

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# googlesheets

Version: 0.3.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tibble’
      All declared Imports should be used.
    ```

# gQTLstats

Version: 1.12.0

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘Homo.sapiens’
    
    Packages suggested but not available for checking:
      ‘geuvPack’ ‘geuvStore2’ ‘org.Hs.eg.db’
      ‘TxDb.Hsapiens.UCSC.hg19.knownGene’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# graphicalVAR

Version: 0.2.2

## In both

*   checking whether package ‘graphicalVAR’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/graphicalVAR/new/graphicalVAR.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘graphicalVAR’ ...
** package ‘graphicalVAR’ successfully unpacked and MD5 sums checked
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romain/git/release/dplyr/revdep/library.noindex/graphicalVAR/Rcpp/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/graphicalVAR/RcppArmadillo/include" -I/usr/local/include  -fopenmp  -fPIC  -Wno-unused-result -Wall -O3 -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘graphicalVAR’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/graphicalVAR/new/graphicalVAR.Rcheck/graphicalVAR’

```
### CRAN

```
* installing *source* package ‘graphicalVAR’ ...
** package ‘graphicalVAR’ successfully unpacked and MD5 sums checked
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romain/git/release/dplyr/revdep/library.noindex/graphicalVAR/Rcpp/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/graphicalVAR/RcppArmadillo/include" -I/usr/local/include  -fopenmp  -fPIC  -Wno-unused-result -Wall -O3 -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘graphicalVAR’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/graphicalVAR/old/graphicalVAR.Rcheck/graphicalVAR’

```
# graphTweets

Version: 0.5.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘utils’
      All declared Imports should be used.
    ```

# grasp2db

Version: 1.1.0

## In both

*   R CMD check timed out
    

*   checking for missing documentation entries ... WARNING
    ```
    Undocumented code objects:
      ‘checkAnti’ ‘getJoinCompatible’ ‘GRASP2’
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
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/grasp2db/new/grasp2db.Rcheck/00_pkg_src/grasp2db/R/db_AnnotationHub.R:39)
    .grasp2ToAnnotationHub: no visible global function definition for
      ‘outputFile’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/grasp2db/new/grasp2db.Rcheck/00_pkg_src/grasp2db/R/db_AnnotationHub.R:40)
    checkAnti: no visible binding for global variable ‘chr_hg19’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/grasp2db/new/grasp2db.Rcheck/00_pkg_src/grasp2db/R/doanti.R:19-20)
    getJoinCompatible: no visible binding for global variable ‘gwrngs19’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/grasp2db/new/grasp2db.Rcheck/00_pkg_src/grasp2db/R/doanti.R:7)
    Undefined global functions or variables:
      chr_hg19 gwrngs19 outputFile
    ```

# grattan

Version: 1.7.0.0

## In both

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      ‘taxstats’ ‘taxstats1516’
    ```

# Greg

Version: 1.2.2

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘rmeta’
    ```

# gutenbergr

Version: 0.1.4

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 13617 marked UTF-8 strings
    ```

# harrietr

Version: 0.2.3

## In both

*   checking whether package ‘harrietr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/harrietr/new/harrietr.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘harrietr’ ...
** package ‘harrietr’ successfully unpacked and MD5 sums checked
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error : object ‘as_data_frame’ is not exported by 'namespace:tidytree'
ERROR: lazy loading failed for package ‘harrietr’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/harrietr/new/harrietr.Rcheck/harrietr’

```
### CRAN

```
* installing *source* package ‘harrietr’ ...
** package ‘harrietr’ successfully unpacked and MD5 sums checked
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error : object ‘as_data_frame’ is not exported by 'namespace:tidytree'
ERROR: lazy loading failed for package ‘harrietr’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/harrietr/old/harrietr.Rcheck/harrietr’

```
# heatwaveR

Version: 0.3.3

## In both

*   checking whether package ‘heatwaveR’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/heatwaveR/new/heatwaveR.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘heatwaveR’ ...
** package ‘heatwaveR’ successfully unpacked and MD5 sums checked
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romain/git/release/dplyr/revdep/library.noindex/heatwaveR/Rcpp/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/heatwaveR/RcppArmadillo/include" -I/usr/local/include  -fopenmp -fPIC  -Wno-unused-result -Wall -O3 -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘heatwaveR’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/heatwaveR/new/heatwaveR.Rcheck/heatwaveR’

```
### CRAN

```
* installing *source* package ‘heatwaveR’ ...
** package ‘heatwaveR’ successfully unpacked and MD5 sums checked
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romain/git/release/dplyr/revdep/library.noindex/heatwaveR/Rcpp/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/heatwaveR/RcppArmadillo/include" -I/usr/local/include  -fopenmp -fPIC  -Wno-unused-result -Wall -O3 -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘heatwaveR’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/heatwaveR/old/heatwaveR.Rcheck/heatwaveR’

```
# heemod

Version: 0.9.2

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > 
    > res_mod <- run_model(
    +   mod = mod,
    +   parameters = param,
    +   init = c(1000L, 0L),
    +   cycles = 10,
    +   cost = cost,
    +   effect = effect,
    +   method = "end"
    + )
    > 
    > f <- function(x) {
    +   dplyr::filter(
    +     get_counts(x),
    +     state_names == "A" & markov_cycle == 10
    +   )$count
    + }
    > f(res_mod)
    Error in row_number() : could not find function "row_number"
    Calls: f ... <Anonymous> -> <Anonymous> -> mutate_.tbl_df -> mutate_impl
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 417 SKIPPED: 0 FAILED: 21
      1. Error: one-dimensional calibration (@test_calibration.R#41) 
      2. Error: multi-dimensional calibration (@test_calibration.R#263) 
      3. Error: exactly match HIV model (@test_dmhee.R#102) 
      4. Error: Exactly match THR model (@test_dmhee.R#267) 
      5. Error: Demographic analysis (@test_heterogeneity.R#73) 
      6. Error: Same results using 1 core or 2. (@test_parallel.R#7) 
      7. Error: part surv works (@test_part_surv.R#129) 
      8. Error: run_model behaves as expected (@test_run_model.R#241) 
      9. Error: Discounting (@test_run_model.R#350) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 114-115 (a_introduction.Rmd) 
    Error: processing vignette 'a_introduction.Rmd' failed with diagnostics:
    could not find function "row_number"
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        doc       1.5Mb
        R         2.1Mb
        tabular   1.2Mb
    ```

# hiAnnotator

Version: 1.14.0

## In both

*   checking whether package ‘hiAnnotator’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘GenomicRanges’ was built under R version 3.5.1
      Warning: package ‘IRanges’ was built under R version 3.5.1
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/hiAnnotator/new/hiAnnotator.Rcheck/00install.out’ for details.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/hiAnnotator/new/hiAnnotator.Rcheck/00_pkg_src/hiAnnotator/R/hiAnnotator.R:424-428)
    makeGRanges: no visible global function definition for ‘seqlengths’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/hiAnnotator/new/hiAnnotator.Rcheck/00_pkg_src/hiAnnotator/R/hiAnnotator.R:439-440)
    makeGRanges: no visible global function definition for ‘seqlevels<-’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/hiAnnotator/new/hiAnnotator.Rcheck/00_pkg_src/hiAnnotator/R/hiAnnotator.R:464)
    makeGRanges: no visible global function definition for ‘sortSeqlevels’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/hiAnnotator/new/hiAnnotator.Rcheck/00_pkg_src/hiAnnotator/R/hiAnnotator.R:464)
    makeGRanges: no visible global function definition for ‘seqlevelsInUse’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/hiAnnotator/new/hiAnnotator.Rcheck/00_pkg_src/hiAnnotator/R/hiAnnotator.R:464)
    makeGRanges: no visible global function definition for ‘seqlengths<-’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/hiAnnotator/new/hiAnnotator.Rcheck/00_pkg_src/hiAnnotator/R/hiAnnotator.R:465)
    makeGRanges: no visible global function definition for ‘seqlevels’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/hiAnnotator/new/hiAnnotator.Rcheck/00_pkg_src/hiAnnotator/R/hiAnnotator.R:465)
    Undefined global functions or variables:
      breakInChunks countQueryHits detectCores dist featureName IRanges
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

# HiCcompare

Version: 1.2.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.0Mb
      sub-directories of 1Mb or more:
        data   5.8Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/HiCcompare/new/HiCcompare.Rcheck/00_pkg_src/HiCcompare/R/total_sum.R:62)
    volcano: no visible binding for global variable ‘A’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/HiCcompare/new/HiCcompare.Rcheck/00_pkg_src/HiCcompare/R/volcano.R:2)
    volcano: no visible binding for global variable ‘adj.IF1’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/HiCcompare/new/HiCcompare.Rcheck/00_pkg_src/HiCcompare/R/volcano.R:2)
    volcano: no visible binding for global variable ‘adj.IF2’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/HiCcompare/new/HiCcompare.Rcheck/00_pkg_src/HiCcompare/R/volcano.R:2)
    volcano: no visible binding for global variable ‘p.value’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/HiCcompare/new/HiCcompare.Rcheck/00_pkg_src/HiCcompare/R/volcano.R:5)
    volcano: no visible binding for global variable ‘A’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/HiCcompare/new/HiCcompare.Rcheck/00_pkg_src/HiCcompare/R/volcano.R:5)
    volcano: no visible binding for global variable ‘D’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/HiCcompare/new/HiCcompare.Rcheck/00_pkg_src/HiCcompare/R/volcano.R:5)
    Undefined global functions or variables:
      A adj.IF1 adj.IF2 adj.M axis bias.slope bp centromere_locations chr1
      chr2 count D fold.change i IF IF1 IF2 j M p.adj p.value pnorm region1
      region2 start1 start2 Z
    Consider adding
      importFrom("graphics", "axis")
      importFrom("stats", "D", "pnorm")
    to your NAMESPACE file.
    ```

# highcharter

Version: 0.5.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 16.9Mb
      sub-directories of 1Mb or more:
        doc          13.7Mb
        htmlwidgets   1.8Mb
    ```

# hiReadsProcessor

Version: 1.16.0

## In both

*   checking whether package ‘hiReadsProcessor’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘IRanges’ was built under R version 3.5.1
      Warning: package ‘GenomicRanges’ was built under R version 3.5.1
      Warning: package ‘DelayedArray’ was built under R version 3.5.1
      Warning: package ‘Rsamtools’ was built under R version 3.5.1
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/hiReadsProcessor/new/hiReadsProcessor.Rcheck/00install.out’ for details.
    ```

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
    vpairwiseAlignSeqs: no visible global function definition for
      ‘runLength’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/hiReadsProcessor/new/hiReadsProcessor.Rcheck/00_pkg_src/hiReadsProcessor/R/hiReadsProcessor.R:1393-1396)
    vpairwiseAlignSeqs: no visible global function definition for ‘Rle’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/hiReadsProcessor/new/hiReadsProcessor.Rcheck/00_pkg_src/hiReadsProcessor/R/hiReadsProcessor.R:1398)
    vpairwiseAlignSeqs: no visible global function definition for
      ‘runLength’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/hiReadsProcessor/new/hiReadsProcessor.Rcheck/00_pkg_src/hiReadsProcessor/R/hiReadsProcessor.R:1399-1402)
    vpairwiseAlignSeqs: no visible global function definition for
      ‘runValue’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/hiReadsProcessor/new/hiReadsProcessor.Rcheck/00_pkg_src/hiReadsProcessor/R/hiReadsProcessor.R:1400-1401)
    vpairwiseAlignSeqs: no visible global function definition for
      ‘runLength’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/hiReadsProcessor/new/hiReadsProcessor.Rcheck/00_pkg_src/hiReadsProcessor/R/hiReadsProcessor.R:1400-1401)
    vpairwiseAlignSeqs: no visible global function definition for ‘IRanges’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/hiReadsProcessor/new/hiReadsProcessor.Rcheck/00_pkg_src/hiReadsProcessor/R/hiReadsProcessor.R:1420-1424)
    Undefined global functions or variables:
      breakInChunks clusteredValue clusteredValue.freq DataFrame
      detectCores fasta.info IRanges IRangesList matches mclapply metadata
      metadata<- misMatches qBaseInsert queryHits Rle runLength runValue
      scanBamFlag ScanBamParam SimpleList tBaseInsert
    ```

# HMP16SData

Version: 1.0.1

## In both

*   checking whether package ‘HMP16SData’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘GenomicRanges’ was built under R version 3.5.1
      Warning: package ‘IRanges’ was built under R version 3.5.1
      Warning: package ‘DelayedArray’ was built under R version 3.5.1
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/HMP16SData/new/HMP16SData.Rcheck/00install.out’ for details.
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Attaching package: 'dendextend'
    
    The following object is masked from 'package:stats':
    
        cutree
    
    ========================================
    circlize version 0.4.5
    CRAN page: https://cran.r-project.org/package=circlize
    Github page: https://github.com/jokergoo/circlize
    Documentation: http://jokergoo.github.io/circlize_book/book/
    
    If you use it in published research, please cite:
    Gu, Z. circlize implements and enhances circular visualization 
      in R. Bioinformatics 2014.
    ========================================
    
    Quitting from lines 58-71 (HMP16SData.Rmd) 
    Error: processing vignette 'HMP16SData.Rmd' failed with diagnostics:
    there is no package called 'curatedMetagenomicData'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘curatedMetagenomicData’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 19.1Mb
      sub-directories of 1Mb or more:
        doc       1.5Mb
        extdata  17.4Mb
    ```

# hpiR

Version: 0.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘knitr’
      All declared Imports should be used.
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

# hurricaneexposure

Version: 0.0.1

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘hurricaneexposuredata’
    ```

# huxtable

Version: 4.3.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      7: withVisible(code)
      8: eval_bare(get_expr(quo), get_env(quo))
      9: quick_pdf(m, dfr, ht, file = tf, height = "4in")
      10: tools::texi2pdf(latex_file, clean = TRUE) at /Users/romain/git/release/dplyr/revdep/checks.noindex/huxtable/new/huxtable.Rcheck/00_pkg_src/huxtable/R/quick-functions.R:83
      11: texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet, texi2dvi = texi2dvi, 
             texinputs = texinputs, index = index)
      12: stop(msg, domain = NA)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 841 SKIPPED: 57 FAILED: 2
      1. Error: quick_pdf works (@test-quick-output.R#41) 
      2. Error: quick_pdf works with height and width options (@test-quick-output.R#53) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        doc   2.9Mb
        R     2.0Mb
    ```

# HydeNet

Version: 0.10.8

## In both

*   checking examples ... ERROR
    ```
    ...
    dbern(p = fromData)
    glm: treat ~ d.dimer + angio
    
    death | pe * treat
    dcat(pi = fromData)
    cpt: death ~ pe + treat> 
    > #* Model Input
    > g1 <- lm(wells ~ 1, data=PE)
    > g2 <- glm(pe ~ wells, data=PE, family="binomial")
    > g3 <- lm(d.dimer ~ pe + pregnant, data=PE)
    > g4 <- xtabs(~ pregnant, data=PE)
    > g5 <- glm(angio ~ pe, data=PE, family="binomial")
    > g6 <- glm(treat ~ d.dimer + angio, data=PE, family="binomial")
    > g7 <- glm(death ~ pe + treat, data=PE, family="binomial")
    > 
    > bagOfModels <- list(g1,g2,g3,g4,g5,g6,g7)
    > 
    > bagNet <- HydeNetwork(bagOfModels)
    Error in n() : could not find function "n"
    Calls: HydeNetwork ... as.data.frame -> mutate -> mutate.tbl_df -> mutate_impl
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 49 SKIPPED: 0 FAILED: 16
      1. Error: compileDecisionModel (@test_compileDecisionModel.R#14) 
      2. Error: (unknown) (@test-bindPosterior.R#12) 
      3. Error: compileJagsModel returns an object of class 'compiledHydeNetwork' (@test-compileJagsModel.R#14) 
      4. Error: HydeNetwork.list returns expected attributes (@test-HydeNetwork.R#47) 
      5. Error: (unknown) (@test-HydePosterior.R#11) 
      6. Error: modelToNode: multinom (@test-modelToNode.R#17) 
      7. Error: (unknown) (@test-print.HydePosterior.R#11) 
      8. Error: fit the model for dbern (@test-setNode.R#28) 
      9. Error: fit the model for dnorm (@test-setNode.R#40) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Loading required package: nnet
    Quitting from lines 264-280 (DecisionNetworks.Rmd) 
    Error: processing vignette 'DecisionNetworks.Rmd' failed with diagnostics:
    could not find function "n"
    Execution halted
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Error in .requirePackage(package) : 
        unable to find required package 'graph'
      Calls: <Anonymous> ... .extendsForS3 -> extends -> getClassDef -> .requirePackage
      Execution halted
    ```

# hydrolinks

Version: 0.10.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dbplyr’
      All declared Imports should be used.
    ```

# iCNV

Version: 1.0.0

## In both

*   checking whether package ‘iCNV’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/iCNV/new/iCNV.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘iCNV’ ...
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Warning: package ‘Rsamtools’ was built under R version 3.5.1
Warning: package ‘IRanges’ was built under R version 3.5.1
Warning: package ‘GenomicRanges’ was built under R version 3.5.1
Error : package ‘BSgenome.Hsapiens.UCSC.hg19’ required by ‘CODEX’ could not be found
ERROR: lazy loading failed for package ‘iCNV’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/iCNV/new/iCNV.Rcheck/iCNV’

```
### CRAN

```
* installing *source* package ‘iCNV’ ...
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Warning: package ‘Rsamtools’ was built under R version 3.5.1
Warning: package ‘IRanges’ was built under R version 3.5.1
Warning: package ‘GenomicRanges’ was built under R version 3.5.1
Error : package ‘BSgenome.Hsapiens.UCSC.hg19’ required by ‘CODEX’ could not be found
ERROR: lazy loading failed for package ‘iCNV’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/iCNV/old/iCNV.Rcheck/iCNV’

```
# iCOBRA

Version: 1.8.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.7Mb
      sub-directories of 1Mb or more:
        doc       1.2Mb
        extdata   1.7Mb
        R         2.1Mb
    ```

# IDE

Version: 0.2.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
      Running 'texi2dvi' on 'IDE_intro.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `algorithm.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    
    ! Emergency stop.
    <read *> 
             
    l.58 \usepackage
                    {algpseudocode}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    Calls: buildVignettes -> texi2pdf -> texi2dvi
    Execution halted
    ```

# ideal

Version: 1.4.0

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘GO.db’
    
    Packages suggested but not available for checking:
      ‘airway’ ‘org.Hs.eg.db’ ‘TxDb.Hsapiens.UCSC.hg38.knownGene’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# idealstan

Version: 0.5.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        libs   3.5Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# idefix

Version: 0.3.3

## In both

*   checking whether package ‘idefix’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/idefix/new/idefix.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘idefix’ ...
** package ‘idefix’ successfully unpacked and MD5 sums checked
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romain/git/release/dplyr/revdep/library.noindex/idefix/Rcpp/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/idefix/RcppArmadillo/include" -I/usr/local/include  -fopenmp  -fPIC  -Wno-unused-result -Wall -O3 -c InfoDes_cpp.cpp -o InfoDes_cpp.o
clang: error: unsupported option '-fopenmp'
make: *** [InfoDes_cpp.o] Error 1
ERROR: compilation failed for package ‘idefix’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/idefix/new/idefix.Rcheck/idefix’

```
### CRAN

```
* installing *source* package ‘idefix’ ...
** package ‘idefix’ successfully unpacked and MD5 sums checked
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romain/git/release/dplyr/revdep/library.noindex/idefix/Rcpp/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/idefix/RcppArmadillo/include" -I/usr/local/include  -fopenmp  -fPIC  -Wno-unused-result -Wall -O3 -c InfoDes_cpp.cpp -o InfoDes_cpp.o
clang: error: unsupported option '-fopenmp'
make: *** [InfoDes_cpp.o] Error 1
ERROR: compilation failed for package ‘idefix’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/idefix/old/idefix.Rcheck/idefix’

```
# IHW

Version: 1.8.0

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
    
    Quitting from lines 42-47 (introduction_to_ihw.Rmd) 
    Error: processing vignette 'introduction_to_ihw.Rmd' failed with diagnostics:
    there is no package called 'airway'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘airway’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.2Mb
      sub-directories of 1Mb or more:
        doc   5.8Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/IHW/new/IHW.Rcheck/00_pkg_src/IHW/R/plots.R:101)
    plot_decisionboundary: no visible binding for global variable
      ‘covariate’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/IHW/new/IHW.Rcheck/00_pkg_src/IHW/R/plots.R:110-112)
    plot_decisionboundary: no visible binding for global variable ‘pvalue’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/IHW/new/IHW.Rcheck/00_pkg_src/IHW/R/plots.R:110-112)
    plot_decisionboundary: no visible binding for global variable ‘fold’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/IHW/new/IHW.Rcheck/00_pkg_src/IHW/R/plots.R:110-112)
    thresholds_ihwResult: no visible global function definition for
      ‘na.exclude’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/IHW/new/IHW.Rcheck/00_pkg_src/IHW/R/ihw_class.R:96-97)
    thresholds,ihwResult: no visible global function definition for
      ‘na.exclude’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/IHW/new/IHW.Rcheck/00_pkg_src/IHW/R/ihw_class.R:96-97)
    Undefined global functions or variables:
      covariate fold gurobi mcols mcols<- metadata metadata<- na.exclude
      p.adjust pvalue runif str stratum
    Consider adding
      importFrom("stats", "na.exclude", "p.adjust", "runif")
      importFrom("utils", "str")
    to your NAMESPACE file.
    ```

# IHWpaper

Version: 1.7.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      [1] "LSL GBH"
      [1] "TST GBH"
      [1] "SBH"
      [1] "Clfdr"
      [1] "Greedy Indep. Filt."
      [1] "IHW"
      [1] "IHW-Bonferroni E3"
      [1] "Bonferroni"
      [1] "qvalue"
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 1 SKIPPED: 0 FAILED: 1
      1. Error: (unknown) (@test_analyze_datasets.R#4) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 29-65 (BH-explanation.Rmd) 
    Error: processing vignette 'BH-explanation.Rmd' failed with diagnostics:
    Palette not found.
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘airway’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 24.0Mb
      sub-directories of 1Mb or more:
        doc      13.1Mb
        extdata   9.8Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    scott_fdrreg: no visible global function definition for ‘FDRreg’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/IHWpaper/new/IHWpaper.Rcheck/00_pkg_src/IHWpaper/R/covariate_methods.R:88)
    scott_fdrreg: no visible global function definition for ‘getFDR’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/IHWpaper/new/IHWpaper.Rcheck/00_pkg_src/IHWpaper/R/covariate_methods.R:97)
    sim_fun_eval: no visible binding for global variable ‘fdr_method’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/IHWpaper/new/IHWpaper.Rcheck/00_pkg_src/IHWpaper/R/benchmarking.R:61-63)
    sim_fun_eval: no visible binding for global variable ‘fdr_pars’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/IHWpaper/new/IHWpaper.Rcheck/00_pkg_src/IHWpaper/R/benchmarking.R:61-63)
    sim_fun_eval: no visible binding for global variable ‘FDP’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/IHWpaper/new/IHWpaper.Rcheck/00_pkg_src/IHWpaper/R/benchmarking.R:61-63)
    sim_fun_eval: no visible binding for global variable ‘rj_ratio’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/IHWpaper/new/IHWpaper.Rcheck/00_pkg_src/IHWpaper/R/benchmarking.R:61-63)
    sim_fun_eval: no visible binding for global variable ‘FPR’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/IHWpaper/new/IHWpaper.Rcheck/00_pkg_src/IHWpaper/R/benchmarking.R:61-63)
    sim_fun_eval: no visible binding for global variable ‘FWER’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/IHWpaper/new/IHWpaper.Rcheck/00_pkg_src/IHWpaper/R/benchmarking.R:61-63)
    Undefined global functions or variables:
      FDP fdr_method fdr_pars FDRreg FPR FWER getFDR rj_ratio
    ```

# ijtiff

Version: 1.5.0

## In both

*   checking whether package ‘ijtiff’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/ijtiff/new/ijtiff.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘ijtiff’ ...
** package ‘ijtiff’ successfully unpacked and MD5 sums checked
Package libtiff-4 was not found in the pkg-config search path.
Perhaps you should add the directory containing `libtiff-4.pc'
to the PKG_CONFIG_PATH environment variable
No package 'libtiff-4' found
Using PKG_CFLAGS=
Using PKG_LIBS=-ltiff -ljpeg -lz
------------------------- ANTICONF ERROR ---------------------------
Configuration failed because libtiff-4 was not found. Try installing:
 * deb: libtiff-dev (Debian, Ubuntu, etc)
 * rpm: libtiff-devel (Fedora, EPEL)
 * brew: libtiff (OSX)
If libtiff-4 is already installed, check that 'pkg-config' is in your
PATH and PKG_CONFIG_PATH contains a libtiff-4.pc file. If pkg-config
is unavailable you can set INCLUDE_DIR and LIB_DIR manually via:
R CMD INSTALL --configure-vars='INCLUDE_DIR=... LIB_DIR=...'
--------------------------------------------------------------------
ERROR: configuration failed for package ‘ijtiff’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/ijtiff/new/ijtiff.Rcheck/ijtiff’

```
### CRAN

```
* installing *source* package ‘ijtiff’ ...
** package ‘ijtiff’ successfully unpacked and MD5 sums checked
Package libtiff-4 was not found in the pkg-config search path.
Perhaps you should add the directory containing `libtiff-4.pc'
to the PKG_CONFIG_PATH environment variable
No package 'libtiff-4' found
Using PKG_CFLAGS=
Using PKG_LIBS=-ltiff -ljpeg -lz
------------------------- ANTICONF ERROR ---------------------------
Configuration failed because libtiff-4 was not found. Try installing:
 * deb: libtiff-dev (Debian, Ubuntu, etc)
 * rpm: libtiff-devel (Fedora, EPEL)
 * brew: libtiff (OSX)
If libtiff-4 is already installed, check that 'pkg-config' is in your
PATH and PKG_CONFIG_PATH contains a libtiff-4.pc file. If pkg-config
is unavailable you can set INCLUDE_DIR and LIB_DIR manually via:
R CMD INSTALL --configure-vars='INCLUDE_DIR=... LIB_DIR=...'
--------------------------------------------------------------------
ERROR: configuration failed for package ‘ijtiff’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/ijtiff/old/ijtiff.Rcheck/ijtiff’

```
# imager

Version: 0.41.1

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -Dcimg_r_mode -fpermissive -I/usr/X11R6/include -I/opt/X11/include  -I"/Users/romain/git/release/dplyr/revdep/library.noindex/imager/Rcpp/include" -I"/Users/romain/git/release/dplyr/revdep/checks.noindex/imager/new/imager.Rcheck/imager/include" -I"/private/var/folders/r_/1b2gjtsd7j92jbbpz4t7ps340000gn/T/Rtmp1j78AI/sourceCpp-x86_64-apple-darwin15.6.0-1.0.0" -I/usr/local/include   -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c filed72d2aa6e733.cpp -o filed72d2aa6e733.o
      clang++ -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o sourceCpp_2.so filed72d2aa6e733.o -lX11 -L/usr/X11R6/lib -L/opt/X11/include -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
      ── 1. Error: cpp_plugin (@test_cpp_api.R#14)  ──────────────────────────────────
      Error 1 occurred building shared library.
      1: cppFunction(foo.inline, depends = "imager") at testthat/test_cpp_api.R:14
      2: sourceCpp(code = code, env = env, rebuild = rebuild, cacheDir = cacheDir, showOutput = showOutput, 
             verbose = verbose)
      3: stop("Error ", status, " occurred building shared library.")
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 17 SKIPPED: 0 FAILED: 1
      1. Error: cpp_plugin (@test_cpp_api.R#14) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 10.6Mb
      sub-directories of 1Mb or more:
        data      1.4Mb
        doc       1.1Mb
        include   2.8Mb
        libs      3.1Mb
    ```

# implyr

Version: 0.2.4

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      The following objects are masked from 'package:base':
      
          intersect, setdiff, setequal, union
      
      > library(RJDBC)
      Loading required package: rJava
      Error: package or namespace load failed for 'rJava':
       .onLoad failed in loadNamespace() for 'rJava', details:
        call: dyn.load(file, DLLpath = DLLpath, ...)
        error: unable to load shared object '/Users/romain/git/release/dplyr/revdep/library.noindex/implyr/rJava/libs/rJava.so':
        dlopen(/Users/romain/git/release/dplyr/revdep/library.noindex/implyr/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
        Referenced from: /Users/romain/git/release/dplyr/revdep/library.noindex/implyr/rJava/libs/rJava.so
        Reason: image not found
      Error: package 'rJava' could not be loaded
      Execution halted
    ```

# incadata

Version: 0.6.4

## In both

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

Version: 1.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘rgeos’
      All declared Imports should be used.
    ```

# IncucyteDRC

Version: 0.5.4

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Importing platemap xml from /Users/romain/git/release/dplyr/revdep/checks.noindex/IncucyteDRC/new/IncucyteDRC.Rcheck/IncucyteDRC/extdata/example.PlateMap
    Plate map import successful!
    > data_file <- system.file(file='extdata/example_data.txt', package='IncucyteDRC')
    > test_data <- importIncucyteData(data_file, metric='pc')
    Getting pc data for plate example_data.txt from:
    /Users/romain/git/release/dplyr/revdep/checks.noindex/IncucyteDRC/new/IncucyteDRC.Rcheck/IncucyteDRC/extdata/example_data.txt
    > 
    > test_list <- splitIncucyteDRCPlateData(test_pm, test_data, group_columns='growthcondition')
    > 
    > print(test_list)
    ## This is an IncucyteDRCSetList S3 object containing 4 IncucyteDRCSet objects
    ## Access using standard list notation ie my_list[[1]]
    ## Try lapply(my_list, print) to see more information on each object> 
    > test_idrc_set <- fitGrowthCurvesGrouped(test_list[[2]])
    > test_idrc_set <- fitGrowthCurvesIndividual(test_idrc_set)
    > test_idrc_set <- calculateDRCData(test_idrc_set, cut_time=100)
    Using cut time provided to function
    > test_idrc_set <- fitDoseResponseCurve(test_idrc_set)
    Error in row_number() : could not find function "row_number"
    Calls: fitDoseResponseCurve ... <Anonymous> -> <Anonymous> -> mutate.tbl_df -> mutate_impl
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Importing platemap xml from /Users/romain/git/release/dplyr/revdep/checks.noindex/IncucyteDRC/new/IncucyteDRC.Rcheck/IncucyteDRC/extdata/example2.PlateMap
    Plate map import successful!
    Getting pc data for plate example_data2.txt from:
    /Users/romain/git/release/dplyr/revdep/checks.noindex/IncucyteDRC/new/IncucyteDRC.Rcheck/IncucyteDRC/extdata/example_data2.txt
    Using cut time provided to function
    Quitting from lines 94-99 (Overview.Rmd) 
    Error: processing vignette 'Overview.Rmd' failed with diagnostics:
    could not find function "row_number"
    Execution halted
    ```

# INDperform

Version: 0.1.1

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘INDperform-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_spiechart
    > ### Title: Create score-based spie chart
    > ### Aliases: plot_spiechart
    > 
    > ### ** Examples
    > 
    > # Using the Baltic Sea demo data in this package
    > scores_tbl <- scoring(trend_tbl = model_trend_ex,
    +   mod_tbl = all_results_ex, press_type = press_type_ex)
    > summary_tbl <- summary_sc(scores_tbl)
    > p <- plot_spiechart(summary_tbl)
    Error in unique(n_press) : object 'n_press' not found
    Calls: plot_spiechart ... <Anonymous> -> summarise_.tbl_df -> summarise_impl -> unique
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        data   3.0Mb
        help   1.1Mb
        R      1.0Mb
    ```

# inlabru

Version: 2.1.9

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘INLA’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        data   1.1Mb
        misc   1.8Mb
        R      2.1Mb
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘INLA’
    ```

# interplot

Version: 0.2.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘gridExtra’
      All declared Imports should be used.
    ```

# IONiseR

Version: 2.4.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.8Mb
      sub-directories of 1Mb or more:
        doc       3.7Mb
        extdata   1.5Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      ‘idx’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/IONiseR/new/IONiseR.Rcheck/00_pkg_src/IONiseR/R/Methods-subsetting.R:19-21)
    [,Fast5Summary-ANY-ANY-ANY: no visible binding for global variable
      ‘component’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/IONiseR/new/IONiseR.Rcheck/00_pkg_src/IONiseR/R/Methods-subsetting.R:24-26)
    [,Fast5Summary-ANY-ANY-ANY: no visible binding for global variable
      ‘idx’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/IONiseR/new/IONiseR.Rcheck/00_pkg_src/IONiseR/R/Methods-subsetting.R:24-26)
    show,Fast5Summary: no visible binding for global variable ‘full_2D’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/IONiseR/new/IONiseR.Rcheck/00_pkg_src/IONiseR/R/classes.R:70-71)
    show,Fast5Summary: no visible binding for global variable ‘pass’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/IONiseR/new/IONiseR.Rcheck/00_pkg_src/IONiseR/R/classes.R:75)
    show,Fast5Summary: no visible binding for global variable ‘pass’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/IONiseR/new/IONiseR.Rcheck/00_pkg_src/IONiseR/R/classes.R:77)
    Undefined global functions or variables:
      := AAAAA accumulation baseCalledComplement baseCalledTemplate
      bases_called category channel circleFun component duration error freq
      full_2D group hour idx matrixCol matrixRow mean_value meanZValue
      median_signal minute mux name nbases new_reads num_events oddEven
      pass pentamer rbindlist readIDs seq_length start_time time_bin
      time_group TTTTT x y zvalue
    ```

# ipumsr

Version: 0.3.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘R6’
      All declared Imports should be used.
    ```

# iRF

Version: 2.0.0

## In both

*   checking whether package ‘iRF’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/iRF/new/iRF.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘iRF’ ...
** package ‘iRF’ successfully unpacked and MD5 sums checked
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romain/git/release/dplyr/revdep/library.noindex/iRF/Rcpp/include" -I/usr/local/include  -fopenmp -fPIC  -Wno-unused-result -Wall -O3 -c ExportedFunctionsRIT.cpp -o ExportedFunctionsRIT.o
clang: error: unsupported option '-fopenmp'
make: *** [ExportedFunctionsRIT.o] Error 1
ERROR: compilation failed for package ‘iRF’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/iRF/new/iRF.Rcheck/iRF’

```
### CRAN

```
* installing *source* package ‘iRF’ ...
** package ‘iRF’ successfully unpacked and MD5 sums checked
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romain/git/release/dplyr/revdep/library.noindex/iRF/Rcpp/include" -I/usr/local/include  -fopenmp -fPIC  -Wno-unused-result -Wall -O3 -c ExportedFunctionsRIT.cpp -o ExportedFunctionsRIT.o
clang: error: unsupported option '-fopenmp'
make: *** [ExportedFunctionsRIT.o] Error 1
ERROR: compilation failed for package ‘iRF’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/iRF/old/iRF.Rcheck/iRF’

```
# IrisSpatialFeatures

Version: 1.3.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    ! LaTeX Error: Unknown graphics extension: .png?raw=true.
    
    Error: processing vignette 'IrisSpatialFeatures.Rmd' failed with diagnostics:
    Failed to compile IrisSpatialFeatures.tex. See IrisSpatialFeatures.log for more info.
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        data      2.1Mb
        extdata   1.9Mb
    ```

# iSEE

Version: 1.0.1

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘iSEE-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: annotateEnsembl
    > ### Title: Annotation via ENSEMBL database
    > ### Aliases: annotateEnsembl
    > 
    > ### ** Examples
    > 
    > library(scRNAseq)
    Error in library(scRNAseq) : there is no package called ‘scRNAseq’
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      Loading required package: SingleCellExperiment
      Warning messages:
      1: package 'GenomicRanges' was built under R version 3.5.1 
      2: package 'IRanges' was built under R version 3.5.1 
      3: package 'DelayedArray' was built under R version 3.5.1 
      > 
      > test_check("iSEE")
      Loading required package: scRNAseq
      Error in eval(exprs, env) : require(scRNAseq) is not TRUE
      Calls: test_check ... source_dir -> lapply -> FUN -> eval -> eval -> stopifnot
      In addition: Warning message:
      In library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE,  :
        there is no package called 'scRNAseq'
      Execution halted
    ```

*   checking whether package ‘iSEE’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘GenomicRanges’ was built under R version 3.5.1
      Warning: package ‘IRanges’ was built under R version 3.5.1
      Warning: package ‘DelayedArray’ was built under R version 3.5.1
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/iSEE/new/iSEE.Rcheck/00install.out’ for details.
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    The following objects are masked from 'package:Biobase':
    
        anyMissing, rowMedians
    
    Loading required package: BiocParallel
    
    Attaching package: 'DelayedArray'
    
    The following objects are masked from 'package:matrixStats':
    
        colMaxs, colMins, colRanges, rowMaxs, rowMins, rowRanges
    
    The following objects are masked from 'package:base':
    
        aperm, apply
    
    Loading required package: SingleCellExperiment
    Quitting from lines 89-98 (iSEE_vignette.Rmd) 
    Error: processing vignette 'iSEE_vignette.Rmd' failed with diagnostics:
    there is no package called 'scRNAseq'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      ‘scRNAseq’ ‘org.Mm.eg.db’
    
    Package which this enhances but not available for checking: ‘ExperimentHub’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unexported object imported by a ':::' call: ‘S4Vectors:::selectSome’
      See the note in ?`:::` about the use of this operator.
    ```

# isomiRs

Version: 1.8.0

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘targetscan.Hs.eg.db’
    
    Package suggested but not available for checking: ‘org.Mm.eg.db’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# ITNr

Version: 0.3.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘comtradr’
      All declared Imports should be used.
    ```

# jaccard

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘magrittr’ ‘Rcpp’
      All declared Imports should be used.
    ```

# janeaustenr

Version: 0.1.5

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked UTF-8 string
    ```

# jpndistrict

Version: 0.3.2

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 502 marked UTF-8 strings
    ```

# kableExtra

Version: 0.9.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Error in re-building vignettes:
      ...
    
    Attaching package: 'dplyr'
    
    The following object is masked from 'package:kableExtra':
    
        group_rows
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Quitting from lines 327-331 (awesome_table_in_html.Rmd) 
    Error: processing vignette 'awesome_table_in_html.Rmd' failed with diagnostics:
    unused arguments ("Group 1", 4, 7)
    Execution halted
    ```

# kitagawa

Version: 2.2-2

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    /Library/Frameworks/R.framework/Resources/bin/Rscript -e "if (getRversion() < '3.0.0') knitr::knit2pdf('ResponseModels.Rnw') else tools::texi2pdf('ResponseModels.tex')"
    Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
      Running 'texi2dvi' on 'ResponseModels.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `float.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    
    ! Emergency stop.
    <read *> 
             
    l.71 \usepackage
                    {natbib}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    Calls: <Anonymous> -> texi2dvi
    Execution halted
    make: *** [ResponseModels.pdf] Error 1
    Error in buildVignettes(dir = "/Users/romain/git/release/dplyr/revdep/checks.noindex/kitagawa/new/kitagawa.Rcheck/vign_test/kitagawa") : 
      running 'make' failed
    Execution halted
    ```

# kokudosuuchi

Version: 0.4.2

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 52458 marked UTF-8 strings
    ```

# KraljicMatrix

Version: 0.2.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘tibble’
      All declared Imports should be used.
    ```

# labelled

Version: 2.0.1

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
      installed size is  7.6Mb
      sub-directories of 1Mb or more:
        data   7.4Mb
    ```

# landscapemetrics

Version: 0.3

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘stars’
    ```

# lilikoi

Version: 0.1.0

## In both

*   checking whether package ‘lilikoi’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/lilikoi/new/lilikoi.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘lilikoi’ ...
** package ‘lilikoi’ successfully unpacked and MD5 sums checked
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error : .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/romain/git/release/dplyr/revdep/library.noindex/lilikoi/rJava/libs/rJava.so':
  dlopen(/Users/romain/git/release/dplyr/revdep/library.noindex/lilikoi/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/romain/git/release/dplyr/revdep/library.noindex/lilikoi/rJava/libs/rJava.so
  Reason: image not found
ERROR: lazy loading failed for package ‘lilikoi’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/lilikoi/new/lilikoi.Rcheck/lilikoi’

```
### CRAN

```
* installing *source* package ‘lilikoi’ ...
** package ‘lilikoi’ successfully unpacked and MD5 sums checked
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error : .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/romain/git/release/dplyr/revdep/library.noindex/lilikoi/rJava/libs/rJava.so':
  dlopen(/Users/romain/git/release/dplyr/revdep/library.noindex/lilikoi/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/romain/git/release/dplyr/revdep/library.noindex/lilikoi/rJava/libs/rJava.so
  Reason: image not found
ERROR: lazy loading failed for package ‘lilikoi’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/lilikoi/old/lilikoi.Rcheck/lilikoi’

```
# live

Version: 1.5.9

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘e1071’ ‘lubridate’
      All declared Imports should be used.
    ```

# LLSR

Version: 0.0.2.5

## In both

*   checking whether package ‘LLSR’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/LLSR/new/LLSR.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘LLSR’ ...
** package ‘LLSR’ successfully unpacked and MD5 sums checked
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error : .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/romain/git/release/dplyr/revdep/library.noindex/LLSR/rJava/libs/rJava.so':
  dlopen(/Users/romain/git/release/dplyr/revdep/library.noindex/LLSR/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/romain/git/release/dplyr/revdep/library.noindex/LLSR/rJava/libs/rJava.so
  Reason: image not found
ERROR: lazy loading failed for package ‘LLSR’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/LLSR/new/LLSR.Rcheck/LLSR’

```
### CRAN

```
* installing *source* package ‘LLSR’ ...
** package ‘LLSR’ successfully unpacked and MD5 sums checked
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error : .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/romain/git/release/dplyr/revdep/library.noindex/LLSR/rJava/libs/rJava.so':
  dlopen(/Users/romain/git/release/dplyr/revdep/library.noindex/LLSR/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/romain/git/release/dplyr/revdep/library.noindex/LLSR/rJava/libs/rJava.so
  Reason: image not found
ERROR: lazy loading failed for package ‘LLSR’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/LLSR/old/LLSR.Rcheck/LLSR’

```
# lmeresampler

Version: 0.1.0

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > 
      > test_check("lmeresampler")
      ── 1. Failure: two-level additive random intercept model (@test-case-lme4.R#30) 
      nrow(cr1) >= 48 * min(grouped$count) isn't true.
      
      ── 2. Failure: two-level additive random intercept model (@test-case-lme4.R#32) 
      nrow(cr3) >= 48 * min(grouped$count) isn't true.
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 14 SKIPPED: 58 FAILED: 2
      1. Failure: two-level additive random intercept model (@test-case-lme4.R#30) 
      2. Failure: two-level additive random intercept model (@test-case-lme4.R#32) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

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
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/LocFDRPois/new/LocFDRPois.Rcheck/00_pkg_src/LocFDRPois/R/locfdr.R:84)
    LLConstructor : LL: no visible global function definition for ‘dpois’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/LocFDRPois/new/LocFDRPois.Rcheck/00_pkg_src/LocFDRPois/R/locfdr.R:60)
    LLConstructor : LL: no visible global function definition for ‘dpois’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/LocFDRPois/new/LocFDRPois.Rcheck/00_pkg_src/LocFDRPois/R/locfdr.R:63-64)
    MixtureDensity: no visible global function definition for ‘glm’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/LocFDRPois/new/LocFDRPois.Rcheck/00_pkg_src/LocFDRPois/R/locfdr.R:35)
    MixtureDensity : f_hat: no visible global function definition for
      ‘predict’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/LocFDRPois/new/LocFDRPois.Rcheck/00_pkg_src/LocFDRPois/R/locfdr.R:42)
    NullDensity : f0: no visible global function definition for ‘dpois’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/LocFDRPois/new/LocFDRPois.Rcheck/00_pkg_src/LocFDRPois/R/locfdr.R:106)
    Undefined global functions or variables:
      dpois glm optim predict
    Consider adding
      importFrom("stats", "dpois", "glm", "optim", "predict")
    to your NAMESPACE file.
    ```

# loopr

Version: 1.0.1

## In both

*   checking R code for possible problems ... NOTE
    ```
    amendColumns: no visible global function definition for ‘setNames’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/loopr/new/loopr.Rcheck/00_pkg_src/loopr/R/loopr.R:96-104)
    fillColumns: no visible global function definition for ‘setNames’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/loopr/new/loopr.Rcheck/00_pkg_src/loopr/R/loopr.R:126-136)
    Undefined global functions or variables:
      setNames
    Consider adding
      importFrom("stats", "setNames")
    to your NAMESPACE file.
    ```

# loose.rock

Version: 1.0.9

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘futile.options’ ‘ggfortify’ ‘grDevices’ ‘stats’
      All declared Imports should be used.
    ```

# lpirfs

Version: 0.1.4

## In both

*   checking whether package ‘lpirfs’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/lpirfs/new/lpirfs.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘lpirfs’ ...
** package ‘lpirfs’ successfully unpacked and MD5 sums checked
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include/ -I"/Users/romain/git/release/dplyr/revdep/library.noindex/lpirfs/Rcpp/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/lpirfs/RcppArmadillo/include" -I/usr/local/include  -fopenmp -fPIC  -Wno-unused-result -Wall -O3 -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘lpirfs’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/lpirfs/new/lpirfs.Rcheck/lpirfs’

```
### CRAN

```
* installing *source* package ‘lpirfs’ ...
** package ‘lpirfs’ successfully unpacked and MD5 sums checked
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include/ -I"/Users/romain/git/release/dplyr/revdep/library.noindex/lpirfs/Rcpp/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/lpirfs/RcppArmadillo/include" -I/usr/local/include  -fopenmp -fPIC  -Wno-unused-result -Wall -O3 -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘lpirfs’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/lpirfs/old/lpirfs.Rcheck/lpirfs’

```
# lucid

Version: 1.6

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Loading required package: lucid
    Loading required package: lattice
    Loading required package: rjags
    Loading required package: coda
    Error: package or namespace load failed for 'rjags':
     .onLoad failed in loadNamespace() for 'rjags', details:
      call: dyn.load(file, DLLpath = DLLpath, ...)
      error: unable to load shared object '/Users/romain/git/release/dplyr/revdep/library.noindex/lucid/rjags/libs/rjags.so':
      dlopen(/Users/romain/git/release/dplyr/revdep/library.noindex/lucid/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
      Referenced from: /Users/romain/git/release/dplyr/revdep/library.noindex/lucid/rjags/libs/rjags.so
      Reason: image not found
    Quitting from lines 271-295 (lucid_examples.Rmd) 
    Error: processing vignette 'lucid_examples.Rmd' failed with diagnostics:
    could not find function "jags.model"
    Execution halted
    ```

# LymphoSeq

Version: 1.8.0

## In both

*   checking whether package ‘LymphoSeq’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/LymphoSeq/new/LymphoSeq.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘LymphoSeq’ ...
** R
** inst
** byte-compile and prepare package for lazy loading
Error : object ‘as_data_frame’ is not exported by 'namespace:tidytree'
ERROR: lazy loading failed for package ‘LymphoSeq’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/LymphoSeq/new/LymphoSeq.Rcheck/LymphoSeq’

```
### CRAN

```
* installing *source* package ‘LymphoSeq’ ...
** R
** inst
** byte-compile and prepare package for lazy loading
Error : object ‘as_data_frame’ is not exported by 'namespace:tidytree'
ERROR: lazy loading failed for package ‘LymphoSeq’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/LymphoSeq/old/LymphoSeq.Rcheck/LymphoSeq’

```
# madness

Version: 0.2.5

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Warning in madness(R) : no dimension given, turning val into a column
    Warning in madness(R) : no dimension given, turning val into a column
    Warning in madness(R) : no dimension given, turning val into a column
    Warning in madness(R) : no dimension given, turning val into a column
    Warning in madness(R) : no dimension given, turning val into a column
    Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
      Running 'texi2dvi' on 'introducing_madness.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `paralist.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    
    ! Emergency stop.
    <read *> 
             
    l.113 ^^M
             
    !  ==> Fatal error occurred, no output PDF file produced!
    Calls: buildVignettes -> texi2pdf -> texi2dvi
    Execution halted
    ```

# malariaAtlas

Version: 0.0.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘grid’
      All declared Imports should be used.
    ```

# manifestoR

Version: 1.3.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    mp_corpus: no visible binding for global variable ‘annotations’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/manifestoR/new/manifestoR.Rcheck/00_pkg_src/manifestoR/R/manifesto.R:456-457)
    print.ManifestoAvailability: no visible binding for global variable
      ‘annotations’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/manifestoR/new/manifestoR.Rcheck/00_pkg_src/manifestoR/R/manifesto.R:371-374)
    Undefined global functions or variables:
      annotations
    ```

# mapedit

Version: 0.4.3

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘geojsonio’
    ```

# mapview

Version: 2.6.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.0Mb
      sub-directories of 1Mb or more:
        extdata   1.0Mb
        R         2.0Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘stars’
      All declared Imports should be used.
    ```

# mason

Version: 0.2.6

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘pander’, ‘pixiedust’
    ```

# mbgraphic

Version: 1.0.0

## In both

*   checking whether package ‘mbgraphic’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/mbgraphic/new/mbgraphic.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘mbgraphic’ ...
** package ‘mbgraphic’ successfully unpacked and MD5 sums checked
** libs
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romain/git/release/dplyr/revdep/library.noindex/mbgraphic/Rcpp/include" -I/usr/local/include   -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c RcppExports.cpp -o RcppExports.o
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romain/git/release/dplyr/revdep/library.noindex/mbgraphic/Rcpp/include" -I/usr/local/include   -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c cmasum.cpp -o cmasum.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romain/git/release/dplyr/revdep/library.noindex/mbgraphic/Rcpp/include" -I/usr/local/include   -fPIC  -Wall -g -O2  -c mbgraphic_init.c -o mbgraphic_init.o
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romain/git/release/dplyr/revdep/library.noindex/mbgraphic/Rcpp/include" -I/usr/local/include   -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c variableflip.cpp -o variableflip.o
clang++ -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o mbgraphic.so RcppExports.o cmasum.o mbgraphic_init.o variableflip.o -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
installing to /Users/romain/git/release/dplyr/revdep/checks.noindex/mbgraphic/new/mbgraphic.Rcheck/mbgraphic/libs
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error : .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/romain/git/release/dplyr/revdep/library.noindex/mbgraphic/rJava/libs/rJava.so':
  dlopen(/Users/romain/git/release/dplyr/revdep/library.noindex/mbgraphic/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/romain/git/release/dplyr/revdep/library.noindex/mbgraphic/rJava/libs/rJava.so
  Reason: image not found
ERROR: lazy loading failed for package ‘mbgraphic’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/mbgraphic/new/mbgraphic.Rcheck/mbgraphic’

```
### CRAN

```
* installing *source* package ‘mbgraphic’ ...
** package ‘mbgraphic’ successfully unpacked and MD5 sums checked
** libs
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romain/git/release/dplyr/revdep/library.noindex/mbgraphic/Rcpp/include" -I/usr/local/include   -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c RcppExports.cpp -o RcppExports.o
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romain/git/release/dplyr/revdep/library.noindex/mbgraphic/Rcpp/include" -I/usr/local/include   -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c cmasum.cpp -o cmasum.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romain/git/release/dplyr/revdep/library.noindex/mbgraphic/Rcpp/include" -I/usr/local/include   -fPIC  -Wall -g -O2  -c mbgraphic_init.c -o mbgraphic_init.o
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romain/git/release/dplyr/revdep/library.noindex/mbgraphic/Rcpp/include" -I/usr/local/include   -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c variableflip.cpp -o variableflip.o
clang++ -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o mbgraphic.so RcppExports.o cmasum.o mbgraphic_init.o variableflip.o -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
installing to /Users/romain/git/release/dplyr/revdep/checks.noindex/mbgraphic/old/mbgraphic.Rcheck/mbgraphic/libs
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error : .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/romain/git/release/dplyr/revdep/library.noindex/mbgraphic/rJava/libs/rJava.so':
  dlopen(/Users/romain/git/release/dplyr/revdep/library.noindex/mbgraphic/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/romain/git/release/dplyr/revdep/library.noindex/mbgraphic/rJava/libs/rJava.so
  Reason: image not found
ERROR: lazy loading failed for package ‘mbgraphic’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/mbgraphic/old/mbgraphic.Rcheck/mbgraphic’

```
# MCbiclust

Version: 1.4.0

## In both

*   checking package dependencies ... ERROR
    ```
    Packages required but not available: ‘GO.db’ ‘org.Hs.eg.db’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# mdsr

Version: 0.1.6

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        data   5.5Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2694 marked UTF-8 strings
    ```

# memapp

Version: 2.11

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘DT’ ‘foreign’ ‘formattable’ ‘ggplot2’ ‘haven’ ‘mem’
      ‘openxlsx’ ‘plotly’ ‘RColorBrewer’ ‘readxl’ ‘RODBC’ ‘shinyBS’
      ‘shinydashboard’ ‘shinydashboardPlus’ ‘shinyjs’ ‘shinythemes’
      ‘stringi’ ‘stringr’ ‘tidyr’
      All declared Imports should be used.
    ```

# MetaCyto

Version: 1.2.1

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    collectData: no visible binding for global variable ‘value’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/MetaCyto/new/MetaCyto.Rcheck/00_pkg_src/MetaCyto/R/collectData.R:27)
    panelSummary: no visible binding for global variable ‘antibodies’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/MetaCyto/new/MetaCyto.Rcheck/00_pkg_src/MetaCyto/R/panelSummary.R:34)
    panelSummary: no visible binding for global variable ‘value’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/MetaCyto/new/MetaCyto.Rcheck/00_pkg_src/MetaCyto/R/panelSummary.R:34)
    plotGA: no visible binding for global variable ‘lower’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/MetaCyto/new/MetaCyto.Rcheck/00_pkg_src/MetaCyto/R/plotGA.R:33-39)
    plotGA: no visible binding for global variable ‘upper’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/MetaCyto/new/MetaCyto.Rcheck/00_pkg_src/MetaCyto/R/plotGA.R:33-39)
    searchCluster : <anonymous>: no visible binding for global variable
      ‘triS’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/MetaCyto/new/MetaCyto.Rcheck/00_pkg_src/MetaCyto/R/searchCluster.R:102)
    searchCluster : <anonymous>: no visible binding for global variable
      ‘triS’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/MetaCyto/new/MetaCyto.Rcheck/00_pkg_src/MetaCyto/R/searchCluster.R:103)
    searchCluster : <anonymous>: no visible binding for global variable
      ‘triS’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/MetaCyto/new/MetaCyto.Rcheck/00_pkg_src/MetaCyto/R/searchCluster.R:104)
    Undefined global functions or variables:
      antibodies lower parameter_name triS upper value
    ```

# metagenomeFeatures

Version: 2.0.0

## In both

*   checking whether package ‘metagenomeFeatures’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: subclass "QualityScaledDNAStringSet" of class "DNAStringSet" is not local and cannot be updated for new inheritance information; consider setClassUnion()
      Warning: replacing previous import ‘lazyeval::is_formula’ by ‘purrr::is_formula’ when loading ‘metagenomeFeatures’
      Warning: replacing previous import ‘lazyeval::is_atomic’ by ‘purrr::is_atomic’ when loading ‘metagenomeFeatures’
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/metagenomeFeatures/new/metagenomeFeatures.Rcheck/00install.out’ for details.
    ```

*   checking for missing documentation entries ... WARNING
    ```
    Undocumented S4 methods:
      generic '[' and siglist 'mgFeatures'
    All user-level objects in a package (including S4 classes and methods)
    should have documentation entries.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Welcome to Bioconductor
    
        Vignettes contain introductory material; view with
        'browseVignettes()'. To cite Bioconductor, see
        'citation("Biobase")', and for packages 'citation("pkgname")'.
    
    ── Attaching packages ────────────────────────────────── tidyverse 1.2.1 ──
    ✔ ggplot2 3.1.0           ✔ purrr   0.2.5      
    ✔ tibble  1.4.2           ✔ dplyr   0.7.99.9000
    ✔ tidyr   0.8.2           ✔ stringr 1.3.1      
    ✔ readr   1.3.0           ✔ forcats 0.3.0      
    ── Conflicts ───────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::combine()    masks Biobase::combine(), BiocGenerics::combine()
    ✖ dplyr::filter()     masks stats::filter()
    ✖ dplyr::lag()        masks stats::lag()
    ✖ ggplot2::Position() masks BiocGenerics::Position(), base::Position()
    Quitting from lines 106-120 (database-explore.Rmd) 
    Error: processing vignette 'database-explore.Rmd' failed with diagnostics:
    package or namespace load failed for 'ggtree':
     object 'as_data_frame' is not exported by 'namespace:tidytree'
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        extdata   3.5Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    .select: no visible binding for global variable ‘identifier’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/metagenomeFeatures/new/metagenomeFeatures.Rcheck/00_pkg_src/metagenomeFeatures/R/mgDb_method_select.R:96-97)
    .select.taxa: no visible binding for global variable ‘Keys’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/metagenomeFeatures/new/metagenomeFeatures.Rcheck/00_pkg_src/metagenomeFeatures/R/mgDb_method_select.R:21)
    .select.taxa: no visible binding for global variable ‘.’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/metagenomeFeatures/new/metagenomeFeatures.Rcheck/00_pkg_src/metagenomeFeatures/R/mgDb_method_select.R:21)
    get_gg13.8_85MgDb: no visible binding for global variable ‘metadata’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/metagenomeFeatures/new/metagenomeFeatures.Rcheck/00_pkg_src/metagenomeFeatures/R/gg13.8_85MgDb.R:23-25)
    Undefined global functions or variables:
      . identifier Keys metadata
    ```

# MetaIntegrator

Version: 2.0.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.4Mb
      sub-directories of 1Mb or more:
        data   1.9Mb
        doc    2.2Mb
        R      1.0Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘DT’ ‘GEOmetadb’ ‘gplots’ ‘pheatmap’ ‘readr’ ‘RMySQL’ ‘RSQLite’
      All declared Imports should be used.
    ```

# MetamapsDB

Version: 0.0.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘Matrix’ ‘shiny’
      All declared Imports should be used.
    ```

# metaviz

Version: 0.2.0

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘metaviz-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: viz_forest
    > ### Title: Forest plot variants for meta-analyses
    > ### Aliases: viz_forest
    > 
    > ### ** Examples
    > 
    > library(metaviz)
    > # Plotting the mozart data using a classic forest plot
    > viz_forest(x = mozart[, c("d", "se")],
    + study_labels = mozart[, "study_name"], xlab = "Cohen d")
    Error in eval(mf.yi, data, enclos = sys.frame(sys.parent())) : 
      object 'es' not found
    Calls: viz_forest ... summarise.tbl_df -> summarise_impl -> <Anonymous> -> eval -> eval
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 38-40 (metaviz.Rmd) 
    Error: processing vignette 'metaviz.Rmd' failed with diagnostics:
    object 'es' not found
    Execution halted
    ```

# methyvim

Version: 1.2.0

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘methyvim-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: methyheat
    > ### Title: Heatmap for methytmle objects
    > ### Aliases: methyheat
    > 
    > ### ** Examples
    > 
    > suppressMessages(library(SummarizedExperiment))
    Warning: package ‘GenomicRanges’ was built under R version 3.5.1
    Warning: package ‘IRanges’ was built under R version 3.5.1
    Warning: package ‘DelayedArray’ was built under R version 3.5.1
    > library(methyvimData)
    Error in library(methyvimData) : 
      there is no package called ‘methyvimData’
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── 5. Error: (unknown) (@test-tmle_classic.R#5)  ───────────────────────────────
      there is no package called 'methyvimData'
      1: library(methyvimData) at testthat/test-tmle_classic.R:5
      2: stop(txt, domain = NA)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 19 SKIPPED: 0 FAILED: 5
      1. Error: (unknown) (@test-cluster_sites.R#4) 
      2. Error: (unknown) (@test-methytmle_class.R#5) 
      3. Error: (unknown) (@test-methyvim.R#7) 
      4. Error: (unknown) (@test-screen_limma.R#4) 
      5. Error: (unknown) (@test-tmle_classic.R#5) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 174-177 (using_methyvim.Rmd) 
    Error: processing vignette 'using_methyvim.Rmd' failed with diagnostics:
    there is no package called 'methyvimData'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      ‘minfiData’ ‘methyvimData’
    ```

# metR

Version: 0.2.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.4Mb
      sub-directories of 1Mb or more:
        data   1.1Mb
        doc    1.5Mb
        R      2.1Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘curl’
      All declared Imports should be used.
    ```

# MIAmaxent

Version: 1.0.0

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘MIAmaxent-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: deriveVars
    > ### Title: Derive variables by transformation.
    > ### Aliases: deriveVars
    > 
    > ### ** Examples
    > 
    > toydata_dvs <- deriveVars(toydata_sp1po, c("L", "M", "D", "HF", "HR", "T", "B"))
    Error in n() : could not find function "n"
    Calls: deriveVars ... as.data.frame -> <Anonymous> -> summarise.tbl_df -> summarise_impl
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘grDevices’
      All declared Imports should be used.
    ```

# miceFast

Version: 0.2.3

## In both

*   checking whether package ‘miceFast’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/miceFast/new/miceFast.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘miceFast’ ...
** package ‘miceFast’ successfully unpacked and MD5 sums checked
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romain/git/release/dplyr/revdep/library.noindex/miceFast/Rcpp/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/miceFast/RcppArmadillo/include" -I/usr/local/include  -fopenmp -fPIC  -Wno-unused-result -Wall -O3 -c R_funs.cpp -o R_funs.o
clang: error: unsupported option '-fopenmp'
make: *** [R_funs.o] Error 1
ERROR: compilation failed for package ‘miceFast’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/miceFast/new/miceFast.Rcheck/miceFast’

```
### CRAN

```
* installing *source* package ‘miceFast’ ...
** package ‘miceFast’ successfully unpacked and MD5 sums checked
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romain/git/release/dplyr/revdep/library.noindex/miceFast/Rcpp/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/miceFast/RcppArmadillo/include" -I/usr/local/include  -fopenmp -fPIC  -Wno-unused-result -Wall -O3 -c R_funs.cpp -o R_funs.o
clang: error: unsupported option '-fopenmp'
make: *** [R_funs.o] Error 1
ERROR: compilation failed for package ‘miceFast’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/miceFast/old/miceFast.Rcheck/miceFast’

```
# Miso

Version: 0.1.5

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# mixOmics

Version: 6.3.2

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.1Mb
      sub-directories of 1Mb or more:
        data   4.0Mb
        R      4.5Mb
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

# mlbgameday

Version: 0.1.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘doParallel’ ‘iterators’ ‘parallel’
      All declared Imports should be used.
    ```

# mleap

Version: 0.1.3

## In both

*   checking whether package ‘mleap’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/mleap/new/mleap.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘mleap’ ...
** package ‘mleap’ successfully unpacked and MD5 sums checked
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded
Error: package or namespace load failed for ‘mleap’:
 .onLoad failed in loadNamespace() for 'mleap', details:
  call: NULL
  error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/romain/git/release/dplyr/revdep/library.noindex/mleap/rJava/libs/rJava.so':
  dlopen(/Users/romain/git/release/dplyr/revdep/library.noindex/mleap/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/romain/git/release/dplyr/revdep/library.noindex/mleap/rJava/libs/rJava.so
  Reason: image not found
Error: loading failed
Execution halted
ERROR: loading failed
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/mleap/new/mleap.Rcheck/mleap’

```
### CRAN

```
* installing *source* package ‘mleap’ ...
** package ‘mleap’ successfully unpacked and MD5 sums checked
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded
Error: package or namespace load failed for ‘mleap’:
 .onLoad failed in loadNamespace() for 'mleap', details:
  call: NULL
  error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/romain/git/release/dplyr/revdep/library.noindex/mleap/rJava/libs/rJava.so':
  dlopen(/Users/romain/git/release/dplyr/revdep/library.noindex/mleap/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/romain/git/release/dplyr/revdep/library.noindex/mleap/rJava/libs/rJava.so
  Reason: image not found
Error: loading failed
Execution halted
ERROR: loading failed
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/mleap/old/mleap.Rcheck/mleap’

```
# MLZ

Version: 0.1.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 14.3Mb
      sub-directories of 1Mb or more:
        libs  13.6Mb
    ```

# mnreadR

Version: 2.1.1

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > 
    > # run the parameters estimation
    > data_low_vision_MRS_CPS <- curveParam_RS(data_s1, ps, vd, rt, err)
    Remember to check the accuracy of MRS and CPS estimates by inspecting the MNREAD curve with mnreadCurve()
    > 
    > # inspect the newly created dataframe
    > data_low_vision_MRS_CPS
      err MRS CPS
    1   0  NA  NA
    2   1  NA  NA
    > 
    > #------
    > 
    > # run the parameters estimation on the whole dataset grouped by subject and polarity
    > data_low_vision_MRS_CPS <- curveParam_RS(data_low_vision, ps, vd, rt, err,
    +                                            subject, polarity)
    Remember to check the accuracy of MRS and CPS estimates by inspecting the MNREAD curve with mnreadCurve()
    Warning in min(logmar) : no non-missing arguments to min; returning Inf
    Error in if (unique(nb_row) > 3) { : argument is of length zero
    Calls: curveParam_RS ... <Anonymous> -> do -> do.grouped_df -> eval_tidy -> mansfield_algo
    Execution halted
    ```

# modeldb

Version: 0.1.0

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 46-50 (kmeans.Rmd) 
    Error: processing vignette 'kmeans.Rmd' failed with diagnostics:
    near "AS": syntax error
    Execution halted
    ```

# modelgrid

Version: 1.1.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘ggplot2’ ‘lattice’
      All declared Imports should be used.
    ```

# modelr

Version: 0.1.2

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘lme4’, ‘rstanarm’
    ```

# momentuHMM

Version: 1.4.3

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    1: DM$angle = list(mean = ~state2(angleFormula(d, strength = w)), 
    2:                 concentration= ~1))
                                         ^
    Warning in highr::hilight(x, format, prompt = options$prompt, markup = opts$markup) :
      the syntax of the source code is invalid; the fallback mode is used
    Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
      Running 'texi2dvi' on 'momentuHMM.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `setspace.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    
    ! Emergency stop.
    <read *> 
             
    l.58 \usepackage
                    {natbib}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    Calls: buildVignettes -> texi2pdf -> texi2dvi
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.7Mb
      sub-directories of 1Mb or more:
        data   1.2Mb
        doc    1.8Mb
        R      3.1Mb
    ```

# Momocs

Version: 1.2.9

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        R   3.1Mb
    ```

# MonetDBLite

Version: 0.6.0

## Newly fixed

*   R CMD check timed out
    

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.1Mb
      sub-directories of 1Mb or more:
        libs   5.4Mb
    ```

# monkeylearn

Version: 0.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘ratelimitr’
      All declared Imports should be used.
    ```

# monocle

Version: 2.8.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
      Running 'texi2dvi' on 'monocle-vignette.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `xcolor.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    
    ! Emergency stop.
    <read *> 
             
    l.28 \RequirePackage
                        [a4paper,left=1.9cm,top=1.9cm,bottom=2.5cm,right=1.9cm,i...
    !  ==> Fatal error occurred, no output PDF file produced!
    Calls: buildVignettes -> texi2pdf -> texi2dvi
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘biocViews’ ‘Rcpp’
      All declared Imports should be used.
    Missing or unexported objects:
      ‘scater::newSCESet’ ‘VGAM::gaussianff’
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    importCDS: no visible global function definition for ‘gaussianff’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/monocle/new/monocle.Rcheck/00_pkg_src/monocle/R/cds_conversion.R:235)
    make_canonical: no visible global function definition for ‘nei’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/monocle/new/monocle.Rcheck/00_pkg_src/monocle/R/order_cells.R:297)
    make_canonical: no visible global function definition for ‘nei’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/monocle/new/monocle.Rcheck/00_pkg_src/monocle/R/order_cells.R:298)
    measure_diameter_path: no visible global function definition for ‘nei’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/monocle/new/monocle.Rcheck/00_pkg_src/monocle/R/order_cells.R:470-481)
    orderCells: no visible binding for '<<-' assignment to ‘next_node’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/monocle/new/monocle.Rcheck/00_pkg_src/monocle/R/order_cells.R:1097)
    plot_multiple_branches_pseudotime: no visible binding for global
      variable ‘pseudocount’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/monocle/new/monocle.Rcheck/00_pkg_src/monocle/R/plotting.R:2740)
    plot_multiple_branches_pseudotime: no visible binding for global
      variable ‘Branch’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/monocle/new/monocle.Rcheck/00_pkg_src/monocle/R/plotting.R:2753)
    project2MST: no visible global function definition for ‘nei’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/monocle/new/monocle.Rcheck/00_pkg_src/monocle/R/order_cells.R:1606)
    Undefined global functions or variables:
      Branch gaussianff nei next_node pseudocount Size_Factor
      use_for_ordering
    ```

*   checking files in ‘vignettes’ ... NOTE
    ```
    The following directory looks like a leftover from 'knitr':
      ‘figure’
    Please remove from your package.
    ```

# morse

Version: 3.2.0

## In both

*   checking whether package ‘morse’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/morse/new/morse.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘morse’ ...
** package ‘morse’ successfully unpacked and MD5 sums checked
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error : .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/romain/git/release/dplyr/revdep/library.noindex/morse/rjags/libs/rjags.so':
  dlopen(/Users/romain/git/release/dplyr/revdep/library.noindex/morse/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/romain/git/release/dplyr/revdep/library.noindex/morse/rjags/libs/rjags.so
  Reason: image not found
ERROR: lazy loading failed for package ‘morse’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/morse/new/morse.Rcheck/morse’

```
### CRAN

```
* installing *source* package ‘morse’ ...
** package ‘morse’ successfully unpacked and MD5 sums checked
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error : .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/romain/git/release/dplyr/revdep/library.noindex/morse/rjags/libs/rjags.so':
  dlopen(/Users/romain/git/release/dplyr/revdep/library.noindex/morse/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/romain/git/release/dplyr/revdep/library.noindex/morse/rjags/libs/rjags.so
  Reason: image not found
ERROR: lazy loading failed for package ‘morse’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/morse/old/morse.Rcheck/morse’

```
# mosaic

Version: 1.4.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
    Warning in block_exec(params) :
      Failed to tidy R code in chunk 'unnamed-chunk-27'. Reason:
    Error in loadNamespace(name) : there is no package called 'formatR'
    
    Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
      Running 'texi2dvi' on 'MinimalRgg.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `xcolor.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    
    ! Emergency stop.
    <read *> 
             
    l.63 \usepackage
                    {hyperref}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    Calls: buildVignettes -> texi2pdf -> texi2dvi
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘manipulate’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.7Mb
      sub-directories of 1Mb or more:
        doc   1.9Mb
        R     4.3Mb
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘cubature’
    ```

# mosaicData

Version: 0.17.0

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
      ‘caret’ ‘ggformula’ ‘knitr’ ‘MASS’ ‘testthat’ ‘tidyverse’
      All declared Imports should be used.
    ```

# mousetrap

Version: 3.1.1

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘mousetrap-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: mt_import_long
    > ### Title: Import mouse-tracking data saved in long format.
    > ### Aliases: mt_import_long
    > 
    > ### ** Examples
    > 
    > # Create data in long format for test purposes
    > mt_data_long <- mt_export_long(mt_example,
    +   use2_variables=c("subject_nr","Condition"))
    > 
    > # Import the data using mt_import_long
    > mt_data <- mt_import_long(mt_data_long)
    Error in row_number() : could not find function "row_number"
    Calls: mt_import_long ... <Anonymous> -> <Anonymous> -> mutate_.tbl_df -> mutate_impl
    Execution halted
    ```

# mplot

Version: 1.0.1

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### Title: Model stability and variable importance plots for glmnet
    > ### Aliases: bglmnet
    > 
    > ### ** Examples
    > 
    > n = 100
    > set.seed(11)
    > e = rnorm(n)
    > x1 = rnorm(n)
    > x2 = rnorm(n)
    > x3 = x1^2
    > x4 = x2^2
    > x5 = x1*x2
    > y = 1 + x1 + x2 + e
    > dat = data.frame(y, x1, x2, x3, x4, x5)
    > lm1 = lm(y ~ ., data = dat)
    > ## Don't show: 
    > bg1 = bglmnet(lm1, seed = 1, B=10)
    Error in n() : could not find function "n"
    Calls: bglmnet ... <Anonymous> -> <Anonymous> -> summarise.tbl_df -> summarise_impl
    Execution halted
    ```

# MPTmultiverse

Version: 0.1

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘TreeBUGS’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# mRchmadness

Version: 1.0.0

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘mRchmadness-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: scrape.population.distribution
    > ### Title: Scrape the average rate of teams being picked to win across all
    > ###   ESPN brackets
    > ### Aliases: scrape.population.distribution
    > 
    > ### ** Examples
    > 
    > populationDistribution = scrape.population.distribution(2017)
    Error in open.connection(x, "rb") : 
      Recv failure: Connection reset by peer
    Calls: scrape.population.distribution ... %>% -> eval -> eval -> <Anonymous> -> read_html.default
    Execution halted
    ```

# msigdbr

Version: 6.2.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        R   5.1Mb
    ```

# MSnID

Version: 1.14.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/MSnID/new/MSnID.Rcheck/00_pkg_src/MSnID/R/MSnID-methods.R:600)
    infer_parsimonious_accessions,MSnID : infer_acc: no visible binding for
      global variable ‘N’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/MSnID/new/MSnID.Rcheck/00_pkg_src/MSnID/R/MSnID-methods.R:600)
    infer_parsimonious_accessions,MSnID : infer_acc: no visible binding for
      global variable ‘accession’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/MSnID/new/MSnID.Rcheck/00_pkg_src/MSnID/R/MSnID-methods.R:601)
    infer_parsimonious_accessions,MSnID : infer_acc: no visible binding for
      global variable ‘pepSeq’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/MSnID/new/MSnID.Rcheck/00_pkg_src/MSnID/R/MSnID-methods.R:603)
    recalibrate,MSnID: no visible global function definition for ‘median’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/MSnID/new/MSnID.Rcheck/00_pkg_src/MSnID/R/MSnID-methods.R:520)
    recalibrate,MSnID: no visible global function definition for ‘density’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/MSnID/new/MSnID.Rcheck/00_pkg_src/MSnID/R/MSnID-methods.R:529)
    Undefined global functions or variables:
      accession DatabaseAccess DatabaseDescription DBseqLength density i
      location mass median modification N name optim pepSeq quantile rnorm
      spectrumID
    Consider adding
      importFrom("stats", "density", "median", "optim", "quantile", "rnorm")
    to your NAMESPACE file.
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    ...
    
    The following object is masked from ‘package:base’:
    
        trimws
    
    Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
      Running 'texi2dvi' on 'msnid_vignette.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `fancyhdr.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    
    ! Emergency stop.
    <read *> 
             
    l.171 \pagestyle
                    {fancy}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    Calls: buildVignettes -> texi2pdf -> texi2dvi
    Execution halted
    ```

# MSstats

Version: 3.12.3

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    SpectronauttoMSstatsFormat: no visible binding for global variable
      ‘missing.col’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/MSstats/new/MSstats.Rcheck/00_pkg_src/MSstats/R/SpectronauttoMSstatsFormat.R:46-47)
    SpectronauttoMSstatsFormat: no visible binding for global variable
      ‘fea’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/MSstats/new/MSstats.Rcheck/00_pkg_src/MSstats/R/SpectronauttoMSstatsFormat.R:188)
    SpectronauttoMSstatsFormat: no visible binding for global variable
      ‘Intensity’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/MSstats/new/MSstats.Rcheck/00_pkg_src/MSstats/R/SpectronauttoMSstatsFormat.R:188)
    SpectronauttoMSstatsFormat: no visible binding for global variable
      ‘PeptideSequence’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/MSstats/new/MSstats.Rcheck/00_pkg_src/MSstats/R/SpectronauttoMSstatsFormat.R:214)
    SpectronauttoMSstatsFormat: no visible binding for global variable
      ‘ProteinName’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/MSstats/new/MSstats.Rcheck/00_pkg_src/MSstats/R/SpectronauttoMSstatsFormat.R:214)
    Undefined global functions or variables:
      ABUNDANCE aggr_Fragment_Annotation aggr_Peak_Area analysis ciw
      datafeature fea FEATURE FRACTION Intensity label LABEL logFC Mean
      missing.col Name ncount ount PeptideSequence Protein Protein_number
      ProteinName residual RUN Selected_fragments Selected_peptides shape
      Train_size weight x y ymax ymin
    ```

# MSstatsQC

Version: 1.2.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘RforProteomics’
    ```

# mudata2

Version: 1.0.4

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      nrow(dplyr::count(one_pair, .name_x, .name_y)) not equal to 1.
      1/1 mismatches
      [1] 4 - 1 == 3
      
      OGR: Unsupported geometry type
      OGR: Unsupported geometry type
      OGR: Unsupported geometry type
      OGR: Unsupported geometry type
      OGR: Unsupported geometry type
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 969 SKIPPED: 0 FAILED: 1
      1. Failure: long_pairs handles zero-row combinations gracefully (@test_biplot.R#152) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# multicolor

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘cowsay’
      All declared Imports should be used.
    ```

# multistateutils

Version: 1.2.0

## In both

*   checking examples ... ERROR
    ```
    ...
    + })
    > 
    > # New individual to estimate transition probabilities for
    > newdata <- data.frame(age="20-40", dissub="AML")
    > 
    > # Estimate length of stay in each state after a year, given starting in state 1
    > length_of_stay(models, 
    +                newdata=newdata,
    +                tmat, times=365.25,
    +                start=1)
    
     *** caught illegal operation ***
    address 0x104edf830, cause 'illegal opcode'
    
    Traceback:
     1: desCpp(transitions, trans_mat, newdata_mat, start_times, start_states -     1, tcovs)
     2: data.table::as.data.table(desCpp(transitions, trans_mat, newdata_mat,     start_times, start_states - 1, tcovs))
     3: run_sim(transition_list, attr_mat, trans_mat, tcovs, start_times,     start_states)
     4: state_occupancy(models, trans_mat, newdata_ext, tcovs, initial_times,     start_states, ci, M, agelimit, agecol, agescale)
     5: length_of_stay(models, newdata = newdata, tmat, times = 365.25,     start = 1)
    An irrecoverable exception occurred. R is aborting now ...
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      26: tryCatchList(expr, classes, parentenv, handlers)
      27: tryCatch(withCallingHandlers({    eval(code, test_env)    if (!handled && !is.null(test)) {        skip_empty()    }}, expectation = handle_expectation, skip = handle_skip, warning = handle_warning,     message = handle_message, error = handle_error), error = handle_fatal,     skip = function(e) {    })
      28: test_code(NULL, exprs, env)
      29: source_file(path, new.env(parent = env), chdir = TRUE, wrap = wrap)
      30: force(code)
      31: with_reporter(reporter = reporter, start_end_reporter = start_end_reporter,     {        lister$start_file(basename(path))        source_file(path, new.env(parent = env), chdir = TRUE,             wrap = wrap)        end_context()    })
      32: FUN(X[[i]], ...)
      33: lapply(paths, test_file, env = env, reporter = current_reporter,     start_end_reporter = FALSE, load_helpers = FALSE, wrap = wrap)
      34: force(code)
      35: with_reporter(reporter = current_reporter, results <- lapply(paths,     test_file, env = env, reporter = current_reporter, start_end_reporter = FALSE,     load_helpers = FALSE, wrap = wrap))
      36: test_files(paths, reporter = reporter, env = env, stop_on_failure = stop_on_failure,     stop_on_warning = stop_on_warning, wrap = wrap)
      37: test_dir(path = test_path, reporter = reporter, env = env, filter = filter,     ..., stop_on_failure = stop_on_failure, stop_on_warning = stop_on_warning,     wrap = wrap)
      38: test_package_dir(package = package, test_path = test_path, filter = filter,     reporter = reporter, ..., stop_on_failure = stop_on_failure,     stop_on_warning = stop_on_warning, wrap = wrap)
      39: test_check("multistateutils")
      An irrecoverable exception occurred. R is aborting now ...
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    11: timing_fn(handle(ev <- withCallingHandlers(withVisible(eval(expr,     envir, enclos)), warning = wHandler, error = eHandler, message = mHandler)))
    12: evaluate_call(expr, parsed$src[[i]], envir = envir, enclos = enclos,     debug = debug, last = i == length(out), use_try = stop_on_error !=         2L, keep_warning = keep_warning, keep_message = keep_message,     output_handler = output_handler, include_timing = include_timing)
    13: evaluate::evaluate(...)
    14: evaluate(code, envir = env, new_device = FALSE, keep_warning = !isFALSE(options$warning),     keep_message = !isFALSE(options$message), stop_on_error = if (options$error &&         options$include) 0L else 2L, output_handler = knit_handlers(options$render,         options))
    15: in_dir(input_dir(), evaluate(code, envir = env, new_device = FALSE,     keep_warning = !isFALSE(options$warning), keep_message = !isFALSE(options$message),     stop_on_error = if (options$error && options$include) 0L else 2L,     output_handler = knit_handlers(options$render, options)))
    16: block_exec(params)
    17: call_block(x)
    18: process_group.block(group)
    19: process_group(group)
    20: withCallingHandlers(if (tangle) process_tangle(group) else process_group(group),     error = function(e) {        setwd(wd)        cat(res, sep = "\n", file = output %n% "")        message("Quitting from lines ", paste(current_lines(i),             collapse = "-"), " (", knit_concord$get("infile"),             ") ")    })
    21: process_file(text, output)
    22: knitr::knit(knit_input, knit_output, envir = envir, quiet = quiet,     encoding = encoding)
    23: rmarkdown::render(file, encoding = encoding, quiet = quiet, envir = globalenv(),     ...)
    24: vweave_rmarkdown(...)
    25: engine$weave(file, quiet = quiet, encoding = enc)
    26: doTryCatch(return(expr), name, parentenv, handler)
    27: tryCatchOne(expr, names, parentenv, handlers[[1L]])
    28: tryCatchList(expr, classes, parentenv, handlers)
    29: tryCatch({    engine$weave(file, quiet = quiet, encoding = enc)    setwd(startdir)    find_vignette_product(name, by = "weave", engine = engine)}, error = function(e) {    stop(gettextf("processing vignette '%s' failed with diagnostics:\n%s",         file, conditionMessage(e)), domain = NA, call. = FALSE)})
    30: buildVignettes(dir = "/Users/romain/git/release/dplyr/revdep/checks.noindex/multistateutils/new/multistateutils.Rcheck/vign_test/multistateutils")
    An irrecoverable exception occurred. R is aborting now ...
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘Rcpp’ ‘survival’
      All declared Imports should be used.
    ```

# MXM

Version: 1.4.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Error: processing vignette 'article.ltx' failed with diagnostics:
    Running 'texi2dvi' on 'article.ltx' failed.
    LaTeX errors:
    ! LaTeX Error: File `algorithm.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    
    ! Emergency stop.
    <read *> 
             
    l.11 \usepackage
                    {algpseudocode}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 11.9Mb
      sub-directories of 1Mb or more:
        doc   1.3Mb
        R    10.0Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘knitr’
      All declared Imports should be used.
    ```

# myTAI

Version: 0.8.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        data   2.0Mb
        doc    2.4Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘biomartr’
      All declared Imports should be used.
    ```

# nandb

Version: 2.0.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘MASS’ ‘stats’
      All declared Imports should be used.
    ```

# naniar

Version: 0.4.1

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘naniar-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: gg_miss_case
    > ### Title: Plot the number of missings per case (row)
    > ### Aliases: gg_miss_case
    > 
    > ### ** Examples
    > 
    > 
    > gg_miss_case(airquality)
    Error in n() : could not find function "n"
    Calls: gg_miss_case ... <Anonymous> -> <Anonymous> -> mutate.tbl_df -> mutate_impl
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      14: `_fseq`(`_lhs`)
      15: freduce(value, `_function_list`)
      16: withVisible(function_list[[k]](value))
      17: function_list[[k]](value)
      18: dplyr::mutate(., !!!magic_shade_case_when)
      19: mutate.tbl_df(., !!!magic_shade_case_when) at /Users/romain/git/tidyverse/dplyr/R/manip.r:416
      20: mutate_impl(.data, dots) at /Users/romain/git/tidyverse/dplyr/R/tbl-df.r:91
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 469 SKIPPED: 15 FAILED: 2
      1. Error: (unknown) (@test-gg-miss-case.R#5) 
      2. Error: (unknown) (@test-special-missing-values.R#131) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# ncappc

Version: 0.3.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘bookdown’
      All declared Imports should be used.
    ```

# neonUtilities

Version: 1.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘devtools’ ‘dplyr’
      All declared Imports should be used.
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

*   checking whether package ‘NFP’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘graph’ was built under R version 3.5.1
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/NFP/new/NFP.Rcheck/00install.out’ for details.
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
        rowSums, sapply, setdiff, sort, table, tapply, union, unique,
        unsplit, which, which.max, which.min
    
    Loading required package: graphite
    Warning: package 'graphite' was built under R version 3.5.1
    Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
      Running 'texi2dvi' on 'NFP.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `fancyhdr.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    
    ! Emergency stop.
    <read *> 
             
    l.171 \pagestyle
                    {fancy}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    Calls: buildVignettes -> texi2pdf -> texi2dvi
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘NFPdata’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  8.6Mb
      sub-directories of 1Mb or more:
        data   8.0Mb
    ```

# nlmixr

Version: 1.0.0-7

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        R   3.1Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘numDeriv’ ‘PreciseSums’
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
      ‘choroplethr’ ‘choroplethrMaps’ ‘data.table’ ‘forcats’
      ‘hurricaneexposure’ ‘plyr’ ‘RColorBrewer’ ‘XML’
      All declared Imports should be used.
    ```

# NOAAWeather

Version: 0.1.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        data   4.6Mb
    ```

# nos

Version: 1.1.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘bipartite’
    ```

# nucleR

Version: 2.12.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
      181 for language 0
    No pages of output.
    Transcript written on xelatex.log.
    fmtutil [INFO]: /Users/romain/Library/TinyTeX/texmf-var/web2c/xetex/xelatex.fmt installed.
    fmtutil [INFO]: Disabled formats: 1
    fmtutil [INFO]: Successfully rebuilt formats: 14
    fmtutil [INFO]: Total formats: 15
    fmtutil [INFO]: exiting with status 0
    tlmgr install fancyhdr
    
    tlmgr: Fundamental package texlive.infra not present, uh oh, goodbyeShould not happen, texlive.infra not found at /usr/local/bin/tlmgr line 7344.
    tlmgr: package repository http://mirrors.standaloneinstaller.com/ctan/systems/texlive/tlnet (not verified: gpg unavailable)
    tlmgr path add
    ! LaTeX Error: File `fancyhdr.sty' not found.
    
    ! Emergency stop.
    <read *> 
    
    Error: processing vignette 'nucleR.Rmd' failed with diagnostics:
    Failed to compile nucleR.tex. See nucleR.log for more info.
    Execution halted
    ```

# nullabor

Version: 0.3.5

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘forecast’ ‘rlang’ ‘tidyverse’ ‘tsibble’
      All declared Imports should be used.
    ```

# nycflights13

Version: 1.0.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.1Mb
      sub-directories of 1Mb or more:
        data   7.0Mb
    ```

# nzelect

Version: 0.4.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.4Mb
      sub-directories of 1Mb or more:
        data   5.0Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 6409 marked UTF-8 strings
    ```

# observer

Version: 0.1.2

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘ensurer’
    ```

# oec

Version: 2.7.8

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘readr’
      All declared Imports should be used.
    ```

# OncoSimulR

Version: 2.10.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.1Mb
      sub-directories of 1Mb or more:
        doc   5.4Mb
    ```

# openair

Version: 2.6-1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.0Mb
      sub-directories of 1Mb or more:
        R   4.0Mb
    ```

# opendotaR

Version: 0.1.4

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# openPrimeR

Version: 1.2.0

## In both

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .travis.yml
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 15.2Mb
      sub-directories of 1Mb or more:
        extdata  10.2Mb
        R         3.9Mb
    ```

# opensensmapr

Version: 0.5.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
         }, "object 'boxes' not found", quote(eval(code, test_env))) at testthat/test_phenomena.R:30
      2: eval(code, test_env)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 9 SKIPPED: 35 FAILED: 7
      1. Error: osem_box_to_archive_name works for one box (@test_archive.R#23) 
      2. Error: osem_box_to_archive_name works for multiple boxes (@test_archive.R#29) 
      3. Error: osem_measurements_archive works for one box (@test_archive.R#41) 
      4. Error: osem_measurements_archive fails for multiple boxes (@test_archive.R#47) 
      5. Error: summary.sensebox outputs all metrics for a single box (@test_box.R#42) 
      6. Failure: data.frame can be converted to measurements data.frame (@test_measurements.R#128) 
      7. Error: phenomena from a not sensebox data.frame returns error (@test_phenomena.R#30) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
        intersect, setdiff, setequal, union
    
    
    Attaching package: 'lubridate'
    
    The following object is masked from 'package:base':
    
        date
    
    
    Attaching package: 'zoo'
    
    The following objects are masked from 'package:base':
    
        as.Date, as.Date.numeric
    
    Quitting from lines 44-47 (osem-history.Rmd) 
    Error: processing vignette 'osem-history.Rmd' failed with diagnostics:
    $ operator is invalid for atomic vectors
    Execution halted
    ```

# Organism.dplyr

Version: 1.8.1

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    +      ~symbol %startsWith% "SNORD" & tx_start < 25070000))
    # Source:     lazy query [?? x 7]
    # Database:   sqlite 3.22.0 []
    # Ordered by: tx_id
      tx_chrom tx_start   tx_end tx_strand  tx_id tx_name    symbol    
      <chr>       <int>    <int> <chr>      <int> <chr>      <chr>     
    1 chr15    25051477 25051571 +         123215 uc001yxg.4 SNORD116-1
    2 chr15    25054210 25054304 +         123216 uc001yxi.4 SNORD116-2
    3 chr15    25056860 25056954 +         123217 uc001yxk.4 SNORD116-3
    4 chr15    25059538 25059633 +         123218 uc001yxl.5 SNORD116-4
    5 chr15    25062333 25062427 +         123219 uc001yxo.4 SNORD116-5
    6 chr15    25065026 25065121 +         123221 uc001yxp.5 SNORD116-2
    7 chr15    25067788 25067882 +         123222 uc059gus.1 SNORD116-5
    > 
    > ## transcript coordinates with filter in granges format
    > filters <- GRangesFilter(GenomicRanges::GRanges("chr15:1-25070000"))
    > transcripts(src, filters)
    Warning in as.data.frame(mcols(x), ...) : Arguments in '...' ignored
    Error in result_create(conn@ptr, statement) : near "AS": syntax error
    Calls: transcripts ... .local -> new -> initialize -> initialize -> result_create
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Warning in has_utility("convert", "ImageMagick") :
      ImageMagick not installed or not in PATH
    Quitting from lines 262-264 (Organism.dplyr.Rmd) 
    Error: processing vignette 'Organism.dplyr.Rmd' failed with diagnostics:
    near "AS": syntax error
    Execution halted
    ```

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      1: suppressPackageStartupMessages({
             library(TxDb.Hsapiens.UCSC.hg38.knownGene)
         }) at testthat/test-src_organism-select.R:3
      2: withCallingHandlers(expr, packageStartupMessage = function(c) invokeRestart("muffleMessage"))
      3: library(TxDb.Hsapiens.UCSC.hg38.knownGene) at testthat/test-src_organism-select.R:4
      4: stop(txt, domain = NA)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 32 SKIPPED: 0 FAILED: 3
      1. Error: (unknown) (@test-GenomicFeatures-extractors.R#3) 
      2. Error: mouse (@test-src_organism-class.R#54) 
      3. Error: (unknown) (@test-src_organism-select.R#3) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      ‘org.Hs.eg.db’ ‘TxDb.Hsapiens.UCSC.hg38.knownGene’ ‘org.Mm.eg.db’
      ‘TxDb.Mmusculus.UCSC.mm10.ensGene’
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

*   checking R code for possible problems ... NOTE
    ```
    .toGRanges: no visible binding for global variable ‘.’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/Organism.dplyr/new/Organism.dplyr.Rcheck/00_pkg_src/Organism.dplyr/R/extractors.R:236)
    intronsByTranscript,src_organism: no visible binding for global
      variable ‘.’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/Organism.dplyr/new/Organism.dplyr.Rcheck/00_pkg_src/Organism.dplyr/R/extractor-methods.R:254-255)
    intronsByTranscript,src_organism: no visible binding for global
      variable ‘.’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/Organism.dplyr/new/Organism.dplyr.Rcheck/00_pkg_src/Organism.dplyr/R/extractor-methods.R:264-265)
    orgPackageName,src_organism: no visible binding for global variable
      ‘name’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/Organism.dplyr/new/Organism.dplyr.Rcheck/00_pkg_src/Organism.dplyr/R/src.R:432-433)
    orgPackageName,src_organism: no visible binding for global variable
      ‘organism’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/Organism.dplyr/new/Organism.dplyr.Rcheck/00_pkg_src/Organism.dplyr/R/src.R:434)
    orgPackageName,src_organism: no visible binding for global variable
      ‘OrgDb’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/Organism.dplyr/new/Organism.dplyr.Rcheck/00_pkg_src/Organism.dplyr/R/src.R:434)
    Undefined global functions or variables:
      . name organism OrgDb
    ```

# PakPC2017

Version: 1.0.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘stats’
      All declared Imports should be used.
    ```

# pammtools

Version: 0.1.7

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘pammtools-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: add_hazard
    > ### Title: Add predicted (cumulative) hazard to data set
    > ### Aliases: add_hazard add_cumu_hazard
    > 
    > ### ** Examples
    > 
    > ped <- tumor[1:50,] %>% as_ped(Surv(days, status)~ age)
    > pam <- mgcv::gam(ped_status ~ s(tend)+age, data = ped, family=poisson(), offset=offset)
    > ped_info(ped) %>% add_hazard(pam, type="link")
    Error: (is.list(vars) && all(sapply(vars, is.name))) is not TRUE or vars is not a character vector
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 189 SKIPPED: 0 FAILED: 12
      1. Error: hazard functions work for PAM (@test-add-functions.R#15) 
      2. Error: hazard functions work for PEM (@test-add-functions.R#50) 
      3. Error: cumulative hazard functions work for PAM (@test-add-functions.R#61) 
      4. Error: cumulative hazard functions work for PEM (@test-add-functions.R#108) 
      5. Error: adding terms works for PAM (@test-add-functions.R#117) 
      6. Error: adding terms works for PEM (@test-add-functions.R#128) 
      7. Error: warns about / aborts for unknown intervals (@test-add-functions.R#137) 
      8. Error: survival probabilities functions work for PAM (@test-add-functions.R#176) 
      9. Error: hazards and CI positive for type response (@test-add-functions.R#225) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking S3 generic/method consistency ... WARNING
    ```
    ...
      function(tbl, size, replace, weight, .env, ...)
    sample_frac.nested_fdf:
      function(tbl, size, replace, weight, .env)
    
    sample_frac:
      function(tbl, size, replace, weight, .env, ...)
    sample_frac.ped:
      function(tbl, size, replace, weight, .env)
    
    sample_n:
      function(tbl, size, replace, weight, .env, ...)
    sample_n.nested_fdf:
      function(tbl, size, replace, weight, .env)
    
    sample_n:
      function(tbl, size, replace, weight, .env, ...)
    sample_n.ped:
      function(tbl, size, replace, weight, .env)
    
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

# parlitools

Version: 0.2.1

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 13 marked UTF-8 strings
    ```

# parsemsf

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dbplyr’
      All declared Imports should be used.
    ```

# particles

Version: 0.2.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# PathwaySplice

Version: 1.4.0

## In both

*   checking package dependencies ... ERROR
    ```
    Packages required but not available: ‘org.Hs.eg.db’ ‘org.Mm.eg.db’ ‘GO.db’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# patternplot

Version: 0.2.1

## In both

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# PCRedux

Version: 0.2.6-4

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘caret’
      All declared Imports should be used.
    ```

# pdp

Version: 0.7.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘mlbench’, ‘ICEbox’
    ```

# philr

Version: 1.6.0

## In both

*   checking whether package ‘philr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/philr/new/philr.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘philr’ ...
** R
** inst
** byte-compile and prepare package for lazy loading
Error : object ‘as_data_frame’ is not exported by 'namespace:tidytree'
ERROR: lazy loading failed for package ‘philr’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/philr/new/philr.Rcheck/philr’

```
### CRAN

```
* installing *source* package ‘philr’ ...
** R
** inst
** byte-compile and prepare package for lazy loading
Error : object ‘as_data_frame’ is not exported by 'namespace:tidytree'
ERROR: lazy loading failed for package ‘philr’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/philr/old/philr.Rcheck/philr’

```
# phylopath

Version: 1.0.1

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘phylopath-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: average
    > ### Title: Extract and average the best supported models from a
    > ###   phylogenetic path analysis.
    > ### Aliases: average
    > 
    > ### ** Examples
    > 
    >   candidates <- list(
    +     A = DAG(LS ~ BM, NL ~ BM, DD ~ NL, RS ~ BM + NL),
    +     B = DAG(LS ~ BM, NL ~ BM + RS, DD ~ NL)
    +   )
    >   p <- phylo_path(candidates, rhino, rhino_tree)
    Error in n() : could not find function "n"
    Calls: phylo_path ... as.data.frame -> mutate -> mutate.tbl_df -> mutate_impl -> ifelse
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 77-78 (binary_models.Rmd) 
    Error: processing vignette 'binary_models.Rmd' failed with diagnostics:
    could not find function "n"
    Execution halted
    ```

# pitchRx

Version: 1.8.2

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘ggsubplot’
    ```

# pivot

Version: 18.4.17

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘odbc’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘colorspace’ ‘lubridate’
      All declared Imports should be used.
    ```

# pivottabler

Version: 1.0.0

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 272 SKIPPED: 0 FAILED: 90
      1. Failure: bhmtrains basic pivot total: batch dplyr (@test01_basicTests.R#86) 
      2. Failure: smoke tests:  bhmtrains basic pivot values: batch dplyr (@test02_smokeTests.R#86) 
      3. Failure: smoke tests:  bhmtrains basic pivot html: batch dplyr (@test02_smokeTests.R#112) 
      4. Failure: smoke tests:  bhmtrains basic pivot values two levels: batch dplyr (@test02_smokeTests.R#138) 
      5. Failure: smoke tests:  bhmtrains basic pivot html two levels: batch dplyr (@test02_smokeTests.R#164) 
      6. Failure: smoke tests:  bhmtrains basic pivot values two levels expanded: batch dplyr (@test02_smokeTests.R#190) 
      7. Failure: smoke tests:  bhmtrains basic pivot html two levels expanded: batch dplyr (@test02_smokeTests.R#216) 
      8. Failure: smoke tests:  ignoring parent groups: batch dplyr (@test02_smokeTests.R#292) 
      9. Failure: illegal names tests: batch dplyr (@test03_illegalNameTests.R#93) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.7Mb
      sub-directories of 1Mb or more:
        data   2.0Mb
        doc    2.9Mb
        R      2.6Mb
    ```

# pixiedust

Version: 0.8.5

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘pixiedust-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: as.data.frame.dust
    > ### Title: Convert 'dust' Object to Data Frame
    > ### Aliases: as.data.frame.dust as.data.frame.dust_list
    > 
    > ### ** Examples
    > 
    > fit <- lm(mpg ~ qsec + factor(am) + wt * factor(gear), data = mtcars)
    > Dust <- dust(fit) %>%
    +   sprinkle(cols = 2:4, round = 2) %>%
    +   sprinkle(cols = 5, fn = quote(pvalString(value))) %>%
    +   sprinkle(cols = 3, font_color = "#DA70D6") %>%
    +   sprinkle_print_method("html")
    Error in n() : could not find function "n"
    Calls: %>% ... as.data.frame -> mutate -> mutate.tbl_df -> mutate_impl
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 59 SKIPPED: 55 FAILED: 80
      1. Error: (unknown) (@test-colors.R#3) 
      2. Error: ungroup a grouped_df (@test-dust.grouped_df.R#5) 
      3. Error: split a grouped_df (@test-dust.grouped_df.R#13) 
      4. Error: Create a dust object (@test-dust.R#7) 
      5. Error: dust object has expected names (@test-dust.R#14) 
      6. Error: dust object body component has correct dimensions (@test-dust.R#29) 
      7. Error: dust runs when passed a data frame with tidy_df = FALSE (@test-dust.R#45) 
      8. Error: dust runs when passed a data frame with tidy_df = TRUE (@test-dust.R#52) 
      9. Error: dust with keep_rownames = TRUE adds rownames to object (@test-dust.R#60) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

# pkggraph

Version: 0.2.3

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.0Mb
      sub-directories of 1Mb or more:
        doc   4.3Mb
    ```

# PkgsFromFiles

Version: 0.5

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘PkgsFromFiles-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: pff_check_install_pkgs
    > ### Title: Checks and installs a single package
    > ### Aliases: pff_check_install_pkgs
    > 
    > ### ** Examples
    > 
    > pff_check_install_pkgs('dplyr')
    
    Installing dplyrError in pkg.in %in% my.available.packages : 
      argument "my.available.packages" is missing, with no default
    Calls: pff_check_install_pkgs -> %in%
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘curl’ ‘readr’ ‘stringdist’ ‘XML’
      All declared Imports should be used.
    ```

# PKNCA

Version: 0.8.5

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      [1] "approximate covariance matrix for parameter estimates not of full rank"
      [1] "approximate covariance matrix for parameter estimates not of full rank"
      [1] "approximate covariance matrix for parameter estimates not of full rank"
      [1] "approximate covariance matrix for parameter estimates not of full rank"
      [1] "approximate covariance matrix for parameter estimates not of full rank"
      [1] "approximate covariance matrix for parameter estimates not of full rank"
      [1] "approximate covariance matrix for parameter estimates not of full rank"
      [1] "approximate covariance matrix for parameter estimates not of full rank"
      [1] "approximate covariance matrix for parameter estimates not of full rank"
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 1266 SKIPPED: 2 FAILED: 1
      1. Failure: pk.tss.monoexponential (@test-time.to.steady.state.R#296) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# PKPDmisc

Version: 2.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘purrr’
      All declared Imports should be used.
    ```

# platetools

Version: 0.1.1

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘S4Vectors’
    ```

# plotly

Version: 4.8.0

## Newly broken

*   checking examples ... ERROR
    ```
    ...
     * <chr>   <int> <int> <dbl>    <dbl>  <dbl>    <dbl>     <dbl> <dbl>
     1 Abilene  2000     1    72  5380000  71400      701       6.3 2000 
     2 Abilene  2000     2    98  6505000  58700      746       6.6 2000.
     3 Abilene  2000     3   130  9285000  58100      784       6.8 2000.
     4 Abilene  2000     4    98  9730000  68600      785       6.9 2000.
     5 Abilene  2000     5   141 10590000  67300      794       6.8 2000.
     6 Abilene  2000     6   156 13910000  66900      780       6.6 2000.
     7 Abilene  2000     7   152 12635000  73500      742       6.2 2000.
     8 Abilene  2000     8   131 10710000  75000      765       6.4 2001.
     9 Abilene  2000     9   104  7615000  64500      771       6.5 2001.
    10 Abilene  2000    10   101  7040000  59300      764       6.6 2001.
    # ... with 8,592 more rows
    > 
    > # dplyr verbs operate on plotly objects as if they were data frames
    > p <- economics %>%
    +   plot_ly(x = ~date, y = ~unemploy / pop) %>%
    +   add_lines() %>%
    +   mutate(rate = unemploy / pop) %>% 
    +   filter(rate == max(rate))
    Error: `.preserve` (`.preserve = FALSE`) must not be named, do you need `==`?
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      [1] "Running test: plotly-group-within-trace"
      [1] "Running test: plotly-alpha-blending"
      [1] "Running test: plotly-alpha-no-color"
      [1] "Running test: plotly-factor-axis"
      [1] "Running test: plotly-character-axis"
      [1] "Running test: plotly-histogram"
      [1] "Running test: plotly-histogram-vert"
      [1] "Running test: plotly-inherit-FALSE"
      [1] "Running test: plotly-time-series-summary"
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 1353 SKIPPED: 38 FAILED: 1
      1. Error: can filter data returned by ggplotly (@test-ggplot-ggplotly.R#13) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.5Mb
      sub-directories of 1Mb or more:
        htmlwidgets   3.1Mb
        R             1.7Mb
    ```

# plotrr

Version: 1.0.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘stats’
      All declared Imports should be used.
    ```

# plyranges

Version: 1.0.3

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
             e$handled <- TRUE
             test_error <<- e
         }, "could not find function \"WIGFile\"", quote(WIGFile(test_wig))) at testthat/test-io-wig.R:24
      2: eval(code, test_env)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 271 SKIPPED: 0 FAILED: 5
      1. Error: read_bed returns correct GRanges (@test-io-bed.R#67) 
      2. Error: read_bed_graph returns correct GRanges (@test-io-bedGraph.R#39) 
      3. Error: reading/ writing bigwig files returns correct GRanges (@test-io-bw.R#19) 
      4. Error: reading GFF files returns correct GRanges (@test-io-gff.R#87) 
      5. Error: reading WIG files (@test-io-wig.R#24) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking whether package ‘plyranges’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘IRanges’ was built under R version 3.5.1
      Warning: package ‘GenomicRanges’ was built under R version 3.5.1
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/plyranges/new/plyranges.Rcheck/00install.out’ for details.
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘BSgenome.Hsapiens.UCSC.hg19’
    ```

# pmc

Version: 1.0.3

## In both

*   R CMD check timed out
    

# pmpp

Version: 0.1.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      tests/testthat/test-summarise.r:1063:20:style :Put spaces around all infix operators. 
      tests/testthat/test-summarise.r:1068:21:style :Put spaces around all infix operators. 
      tests/testthat/test-summarise.r:1068:26:style :Commas should always have a space after. 
      tests/testthat/test-summarise.r:1068:28:style :Commas should always have a space after. 
      tests/testthat/test-summarise.r:1103:48:style :Commas should never have a space before. 
      tests/testthat/test-summarise.r:1104:48:style :Commas should never have a space before. 
      tests/testthat/test-utils.R:6:1:style :lines should not be more than 120 characters. 
      tests/testthat/test-utils.R:15:53:style :Commas should always have a space after. 
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 64 SKIPPED: 0 FAILED: 1
      1. Failure: Package Style (@test-lintr.R#32) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# PogromcyDanych

Version: 1.5

## In both

*   checking PDF version of manual ... WARNING
    ```
    ...
     ...                                              
    ! LaTeX Error: Command \k unavailable in encoding OT1.
    
    See the LaTeX manual or LaTeX Companion for explanation.
    Type  H <return>  for immediate help.
     ...                                              
    ! LaTeX Error: Command \k unavailable in encoding OT1.
    
    See the LaTeX manual or LaTeX Companion for explanation.
    Type  H <return>  for immediate help.
     ...                                              
    ! LaTeX Error: Command \k unavailable in encoding OT1.
    
    See the LaTeX manual or LaTeX Companion for explanation.
    Type  H <return>  for immediate help.
     ...                                              
    ! LaTeX Error: Command \k unavailable in encoding OT1.
    
    See the LaTeX manual or LaTeX Companion for explanation.
    Type  H <return>  for immediate help.
     ...                                              
    ```

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

# PopED

Version: 0.4.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tidyr’
      All declared Imports should be used.
    ```

# poplite

Version: 0.99.19

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    +       baseball.db <- Database(baseball.teams, tempfile())
    +       
    +       populate(baseball.db, teams=Teams, team_franch=TeamsFranchises)
    +       
    +       select(baseball.db, .tables="teams")
    +       
    +       select(baseball.db, .tables=c("teams", "team_franch"))
    +       
    +       select(baseball.db, yearID:WCWin, franchName)
    +       
    +       filter(baseball.db, active == "Y")
    +       
    +       select(filter(baseball.db, active == "Y" & W > 50 & teamID == "CAL"), active, W, teamID)
    +   }
    Loading required package: Lahman
    Starting team_franch
    Starting teams
    Error in filter_.Database(.data, .dots = compat_as_lazy_dots(...), .preserve = .preserve) : 
      ERROR: Please supply a single statement which would result in a logical vector
    Calls: filter -> filter.default -> filter_ -> filter_.Database
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ⠏ | 140       | 0
      ⠋ | 141       | 0
      ⠙ | 142       | 0Error in x[[method]](...) : attempt to apply non-function
      Calls: test_dir ... <Anonymous> -> o_apply -> lapply -> FUN -> <Anonymous>
      In addition: Warning message:
      call dbDisconnect() when finished working with a connection 
      
      ══ Results ═════════════════════════════════════════════════════════════════════
      Duration: 2.0 s
      
      OK:       142
      Failed:   4
      Warnings: 0
      Skipped:  0
      Execution halted
    ```

*   checking running R code from vignettes ...
    ```
       ‘poplite.Rnw’ using ‘UTF-8’ ... failed
     ERROR
    Errors in running code in vignettes:
    when running code in ‘poplite.Rnw’
      ...
     9        10     1 dna_10_1
    10        15     1 dna_15_1
    # ... with more rows
    
    > head(filter(sample.tracking.db, sex == "M" & var_wave_1 > 
    +     0))
    
      When sourcing ‘poplite.R’:
    Error: ERROR: Please supply a single statement which would result in a logical vector
    Execution halted
    ```

*   checking for code/documentation mismatches ... WARNING
    ```
    Codoc mismatches from documentation object 'External methods':
    filter
      Code: function(.data, ..., .preserve = FALSE)
      Docs: function(.data, ...)
      Argument names in code not in docs:
        .preserve
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    ...
    Attaching package: ‘poplite’
    
    The following object is masked from ‘package:dplyr’:
    
        select
    
    The following object is masked from ‘package:stats’:
    
        filter
    
    Error in makeSchemaFromData(dna, "dna") : 
      ERROR: The names of the supplied data.frame need to be modified for the database see correct.df.names
    Starting clinical
    Starting samples
    Starting dna
    
    Error: processing vignette 'poplite.Rnw' failed with diagnostics:
     chunk 10 
    Error in filter_.Database(.data, .dots = compat_as_lazy_dots(...), .preserve = .preserve) : 
      ERROR: Please supply a single statement which would result in a logical vector
    Execution halted
    ```

# poppr

Version: 2.8.1

## In both

*   checking whether package ‘poppr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/poppr/new/poppr.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘poppr’ ...
** package ‘poppr’ successfully unpacked and MD5 sums checked
** libs
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include  -fopenmp -fPIC  -Wall -g -O2  -c adjust_missing.c -o adjust_missing.o
clang: error: unsupported option '-fopenmp'
make: *** [adjust_missing.o] Error 1
ERROR: compilation failed for package ‘poppr’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/poppr/new/poppr.Rcheck/poppr’

```
### CRAN

```
* installing *source* package ‘poppr’ ...
** package ‘poppr’ successfully unpacked and MD5 sums checked
** libs
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include  -fopenmp -fPIC  -Wall -g -O2  -c adjust_missing.c -o adjust_missing.o
clang: error: unsupported option '-fopenmp'
make: *** [adjust_missing.o] Error 1
ERROR: compilation failed for package ‘poppr’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/poppr/old/poppr.Rcheck/poppr’

```
# portalr

Version: 0.1.4

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      8: function_list[[i]](value)
      9: dplyr::summarize(., count = n())
      10: summarise.tbl_df(., count = n()) at /Users/romain/git/tidyverse/dplyr/R/manip.r:255
      11: summarise_impl(.data, dots) at /Users/romain/git/tidyverse/dplyr/R/tbl-df.r:102
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 127 SKIPPED: 14 FAILED: 5
      1. Failure: data generated by level = plot, time = newmoon, type = granivore, shape = flat is same (@test-99-regression.R#26) 
      2. Failure: data generated by default setting is same (plants) (@test-99-regression.R#72) 
      3. Failure: data generated by type = Shrubs, unknowns = T, correct_sp = F is same (plants) (@test-99-regression.R#82) 
      4. Failure: data generated by level = Plot, type = Annuals, plots = longterm is same (plants) (@test-99-regression.R#92) 
      5. Error: data generated by default setting is same (shrub_cover) (@test-99-regression.R#96) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# postal

Version: 0.1.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      20: stream_delim(x, path, delim = delim, col_names = col_names, append = append, na = na, 
             quote_escape = quote_escape)
      21: open(path, "ab")
      22: open.connection(path, "ab")
      
      ── 2. Failure: Extract date works (@test_utils.R#244)  ─────────────────────────
      extract_dates(future_date) %>% as.numeric() is not strictly more than lubridate::today() %>% as.numeric(). Difference: 0
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 108 SKIPPED: 0 FAILED: 2
      1. Error: (unknown) (@test_fetch_zones.R#144) 
      2. Failure: Extract date works (@test_utils.R#244) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# prisonbrief

Version: 0.1.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2 marked UTF-8 strings
    ```

# processanimateR

Version: 1.0.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 11.2Mb
      sub-directories of 1Mb or more:
        doc           6.5Mb
        help          2.1Mb
        htmlwidgets   2.5Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘zoo’
      All declared Imports should be used.
    ```

# processcheckR

Version: 0.1.0

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 53-62 (processcheckr.Rmd) 
    Error: processing vignette 'processcheckr.Rmd' failed with diagnostics:
    Column `activity_instance_id` is unknown
    Execution halted
    ```

# processmapR

Version: 0.3.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘viridis’
      All declared Imports should be used.
    ```

# progeny

Version: 1.2.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 42-60 (progeny.Rmd) 
    Error: processing vignette 'progeny.Rmd' failed with diagnostics:
    there is no package called 'airway'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘airway’
    ```

# pRoloc

Version: 1.20.2

## In both

*   checking examples ... ERROR
    ```
    ...
    > ### Title: Class '"ClustDist"'
    > ### Aliases: ClustDist class:ClustDist ClustDist-class
    > ###   plot,ClustDist,MSnSet-method show,ClustDist-method
    > ### Keywords: classes
    > 
    > ### ** Examples
    > 
    >   showClass("ClustDist")
    Class "ClustDist" [package "pRoloc"]
    
    Slots:
                                                                            
    Name:           k       dist       term         id       nrow    clustsz
    Class:    numeric       list  character  character    numeric       list
                                
    Name:  components       fcol
    Class:     vector  character
    >   
    >   library('pRolocdata')
    Error in library("pRolocdata") : there is no package called ‘pRolocdata’
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      Warning messages:
      1: package 'MSnbase' was built under R version 3.5.1 
      2: In fun(libname, pkgname) :
        mzR has been built against a different Rcpp version (0.12.16)
      than is installed on your system (1.0.0). This might lead to errors
      when loading mzR. If you encounter such issues, please send a report,
      including the output of sessionInfo() to the Bioc support forum at 
      https://support.bioconductor.org/. For details see also
      https://github.com/sneumann/mzR/wiki/mzR-Rcpp-compiler-linker-issue.
      3: package 'IRanges' was built under R version 3.5.1 
      4: replacing previous import 'BiocGenerics::var' by 'stats::var' when loading 'MLInterfaces' 
      > library("pRolocdata")
      Error in library("pRolocdata") : there is no package called 'pRolocdata'
      Execution halted
    ```

*   checking whether package ‘pRoloc’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘MSnbase’ was built under R version 3.5.1
      Warning: package ‘IRanges’ was built under R version 3.5.1
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/pRoloc/new/pRoloc.Rcheck/00install.out’ for details.
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    The following object is masked from 'package:tools':
    
        toHTML
    
    
    Attaching package: 'annotate'
    
    The following object is masked from 'package:mzR':
    
        nChrom
    
    Loading required package: cluster
    Warning: replacing previous import 'BiocGenerics::var' by 'stats::var' when loading 'MLInterfaces'
    
    This is pRoloc version 1.20.2 
      Visit https://lgatto.github.io/pRoloc/ to get started.
    
    Quitting from lines 87-93 (pRoloc-goannotations.Rmd) 
    Error: processing vignette 'pRoloc-goannotations.Rmd' failed with diagnostics:
    there is no package called 'pRolocdata'
    Execution halted
    ```

*   checking PDF version of manual ... WARNING
    ```
    ...
    LaTeX errors found:
    ! Please use \mathaccent for accents in math mode.
    \add@accent ...@spacefactor \spacefactor }\accent 
                                                      #1 #2\egroup \spacefactor ...
    l.931 ...{}2}{} protein correlations.}{empPvalues}
                                                      
    ! Missing { inserted.
    <to be read again> 
                       \egroup 
    l.931 ...{}2}{} protein correlations.}{empPvalues}
                                                      
    ! You can't use `\spacefactor' in math mode.
    \add@accent ...}\accent #1 #2\egroup \spacefactor 
                                                      \accent@spacefactor 
    l.931 ...{}2}{} protein correlations.}{empPvalues}
                                                      
    ! Missing } inserted.
    <inserted text> 
                    }
    l.931 ...{}2}{} protein correlations.}{empPvalues}
                                                      
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking: ‘pRolocdata’ ‘GO.db’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 14.0Mb
      sub-directories of 1Mb or more:
        doc  10.6Mb
        R     2.0Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unexported objects imported by ':::' calls:
      ‘caret:::predict.plsda’ ‘MLInterfaces:::.macroF1’
      ‘MLInterfaces:::.precision’ ‘MLInterfaces:::.recall’
      ‘MLInterfaces:::es2df’
      See the note in ?`:::` about the use of this operator.
    There are ::: calls to the package's namespace in its code. A package
      almost never needs to use ::: for its own objects:
      ‘opt’
    ```

*   checking R code for possible problems ... NOTE
    ```
    Found the following possibly unsafe calls:
    File ‘pRoloc/R/annotation.R’:
      unlockBinding("params", .pRolocEnv)
    ```

# pRolocGUI

Version: 1.14.0

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘pRolocGUI-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: pRolocVis
    > ### Title: Interactive visualisation of spatial proteomics data
    > ### Aliases: pRolocVis pRolocVis_aggregate pRolocVis_classify
    > ###   pRolocVis_compare pRolocVis_pca
    > 
    > ### ** Examples
    > 
    > library("pRoloc")
    > library("pRolocdata")
    Error in library("pRolocdata") : there is no package called ‘pRolocdata’
    Execution halted
    ```

*   checking whether package ‘pRolocGUI’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘pRoloc’ was built under R version 3.5.1
      Warning: package ‘MSnbase’ was built under R version 3.5.1
      Warning: package ‘IRanges’ was built under R version 3.5.1
      Warning: namespace ‘dimRed’ is not available and has been replaced
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/pRolocGUI/new/pRolocGUI.Rcheck/00install.out’ for details.
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Loading required package: cluster
    Warning: replacing previous import 'BiocGenerics::var' by 'stats::var' when loading 'MLInterfaces'
    Warning: namespace 'dimRed' is not available and has been replaced
    by .GlobalEnv when processing object ''
    Warning: namespace 'dimRed' is not available and has been replaced
    by .GlobalEnv when processing object ''
    Warning: namespace 'dimRed' is not available and has been replaced
    by .GlobalEnv when processing object ''
    Warning: namespace 'dimRed' is not available and has been replaced
    by .GlobalEnv when processing object ''
    
    This is pRoloc version 1.20.2 
      Visit https://lgatto.github.io/pRoloc/ to get started.
    
    
    This is pRolocGUI version 1.14.0
    
    Quitting from lines 77-79 (pRolocGUI.Rmd) 
    Error: processing vignette 'pRolocGUI.Rmd' failed with diagnostics:
    there is no package called 'pRolocdata'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘pRolocdata’
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

Version: 0.3.0.1

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 159 SKIPPED: 1 FAILED: 22
      1. Error: cross_validation (@test_diagnostics.R#13) 
      2. Error: cross_validation_logistic (@test_diagnostics.R#45) 
      3. Error: cross_validation_extra_regressors (@test_diagnostics.R#61) 
      4. Error: cross_validation_default_value_check (@test_diagnostics.R#76) 
      5. Error: performance_metrics (@test_diagnostics.R#86) 
      6. Error: copy (@test_diagnostics.R#176) 
      7. Error: fit_predict (@test_prophet.R#16) 
      8. Error: fit_predict_no_seasons (@test_prophet.R#22) 
      9. Error: fit_predict_no_changepoints (@test_prophet.R#28) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

# proteoQC

Version: 1.16.0

## In both

*   checking whether package ‘proteoQC’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘MSnbase’ was built under R version 3.5.1
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/proteoQC/new/proteoQC.Rcheck/00install.out’ for details.
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Error: processing vignette 'proteoQC.Rmd' failed with diagnostics:
    there is no package called ‘prettydoc’
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘RforProteomics’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  7.6Mb
      sub-directories of 1Mb or more:
        doc       2.5Mb
        extdata   3.9Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    qcHist: no visible binding for global variable ‘techRep’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/proteoQC/new/proteoQC.Rcheck/00_pkg_src/proteoQC/R/visualization.R:406-416)
    qcHist: no visible binding for global variable ‘bioRep’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/proteoQC/new/proteoQC.Rcheck/00_pkg_src/proteoQC/R/visualization.R:406-416)
    qcHist2: no visible binding for global variable ‘error’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/proteoQC/new/proteoQC.Rcheck/00_pkg_src/proteoQC/R/visualization.R:357-365)
    qcHist2: no visible binding for global variable ‘fractile’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/proteoQC/new/proteoQC.Rcheck/00_pkg_src/proteoQC/R/visualization.R:357-365)
    qcHist2: no visible binding for global variable ‘fractile’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/proteoQC/new/proteoQC.Rcheck/00_pkg_src/proteoQC/R/visualization.R:367-369)
    qcHist2: no visible binding for global variable ‘error’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/proteoQC/new/proteoQC.Rcheck/00_pkg_src/proteoQC/R/visualization.R:377-385)
    qcHist2: no visible binding for global variable ‘fractile’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/proteoQC/new/proteoQC.Rcheck/00_pkg_src/proteoQC/R/visualization.R:377-385)
    qcHist2: no visible binding for global variable ‘fractile’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/proteoQC/new/proteoQC.Rcheck/00_pkg_src/proteoQC/R/visualization.R:389-391)
    Undefined global functions or variables:
      ..count.. bioRep curenv delta error exprs fractile fraction grid.draw
      Intensity iTRAQ4 iTRAQ8 label MS1QC MS2QC peplength peptide_summary
      precursorCharge quantify ratio readMgfData se Tag techRep TMT10 TMT6
      V1 V2 V3 V4 V5 val x y
    ```

# proustr

Version: 0.2.1

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 20105 marked UTF-8 strings
    ```

# prozor

Version: 0.2.11

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.7Mb
      sub-directories of 1Mb or more:
        data      1.7Mb
        extdata   2.3Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘readr’
      All declared Imports should be used.
    ```

# psichomics

Version: 1.6.2

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.6Mb
      sub-directories of 1Mb or more:
        doc   5.6Mb
        R     2.9Mb
    ```

*   checking compiled code ... NOTE
    ```
    File ‘psichomics/libs/psichomics.so’:
      Found ‘___stdoutp’, possibly from ‘stdout’ (C)
        Object: ‘psiFastCalc.o’
      Found ‘_printf’, possibly from ‘printf’ (C)
        Object: ‘psiFastCalc.o’
      Found ‘_putchar’, possibly from ‘putchar’ (C)
        Object: ‘psiFastCalc.o’
    
    Compiled code should not call entry points which might terminate R nor
    write to stdout/stderr instead of to the console, nor use Fortran I/O
    nor system RNGs.
    
    See ‘Writing portable packages’ in the ‘Writing R Extensions’ manual.
    ```

# PSLM2015

Version: 0.2.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 26 marked Latin-1 strings
    ```

# psychmeta

Version: 2.2.1

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    ---------------------------------------- 
    
    Accounted for a total of 63.125% of variance. 
    
    Correlation between r values and artifactual perturbations: 0.759
    
    H value: 1.259
    
    I^2 value: 36.875
    
    Q statistic: 33.986 (p = 0.018, df = 19) 
    
    tau: 0.063, 95% CI = [0.017, 0.101] 
    
    tau^2: 0.004, 95% CI = [0.000, 0.010] 
    > 
    > ## d values
    > ma_obj <- ma_d_ic(d = d, n1 = n1, n2 = n2, ryy = ryyi,
    +                   data = data_d_meas_multi)
    Error: `.data` is a corrupt grouped_df, the `"groups"` attribute must be a data frame
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.7Mb
      sub-directories of 1Mb or more:
        R   7.0Mb
    ```

# psycho

Version: 0.3.7

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.5Mb
      sub-directories of 1Mb or more:
        doc   4.3Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘methods’
      All declared Imports should be used.
    ```

# ptstem

Version: 0.0.3

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ptstem-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ptstem_words
    > ### Title: Stem Words
    > ### Aliases: ptstem ptstem_words
    > 
    > ### ** Examples
    > 
    > words <- c("balões", "aviões", "avião", "gostou", "gosto", "gostaram")
    > ptstem_words(words, "hunspell")
    Error in n() : could not find function "n"
    Calls: ptstem_words ... <Anonymous> -> <Anonymous> -> mutate.tbl_df -> mutate_impl
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      16: freduce(value, `_function_list`)
      17: function_list[[i]](value)
      18: dplyr::mutate(., n_stem = n())
      19: mutate.tbl_df(., n_stem = n()) at /Users/romain/git/tidyverse/dplyr/R/manip.r:416
      20: mutate_impl(.data, dots) at /Users/romain/git/tidyverse/dplyr/R/tbl-df.r:91
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 12 SKIPPED: 0 FAILED: 4
      1. Error: Stemming Hunspell Works (@test-ptstem.R#15) 
      2. Error: Stemming RSLP Works (@test-ptstem.R#26) 
      3. Error: Stemming Porter Works (@test-ptstem.R#37) 
      4. Error: n_char argument (@test-ptstem.R#50) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 58-59 (ptstem.Rmd) 
    Error: processing vignette 'ptstem.Rmd' failed with diagnostics:
    could not find function "n"
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        dict   5.1Mb
    ```

# purrr

Version: 0.2.5

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
          is_null
      
      > 
      > test_check("purrr")
      ── 1. Failure: creates simple call (@test-map.R#10)  ───────────────────────────
      `out` not identical to quote(.f(.x[[i]], ...)).
      Objects equal but not identical
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 393 SKIPPED: 0 FAILED: 1
      1. Failure: creates simple call (@test-map.R#10) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# purrrlyr

Version: 0.0.3

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > # unique values of the column "cyl". The fitted models are returned
    > # in a list-column.
    > mtcars %>%
    +   slice_rows("cyl") %>%
    +   by_slice(purrr::partial(lm, mpg ~ disp))
    # tibble [0 × 1]
    # ... with 1 variable: .out <list>
    > 
    > # by_slice() is especially useful in combination with map().
    > 
    > # To modify the contents of a data frame, use rows collation. Note
    > # that unlike dplyr, Mutating and summarising operations can be
    > # used indistinctly.
    > 
    > # Mutating operation:
    > df <- mtcars %>% slice_rows(c("cyl", "am"))
    > df %>% by_slice(dmap, ~ .x / sum(.x), .collate = "rows")
    Error in by_slice(., dmap, ~.x/sum(.x), .collate = "rows") : 
      results are all NULL and can't be cols/rows collated
    Calls: %>% ... _fseq -> freduce -> withVisible -> <Anonymous> -> by_slice
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 66 SKIPPED: 0 FAILED: 9
      1. Error: dmap() works with sliced data frames (@test-dmap.R#9) 
      2. Error: dmap() works with no columns to map (@test-dmap.R#15) 
      3. Error: conditional sliced mapping recycles within groups (@test-dmap.R#26) 
      4. Error: empty data frames (@test-rows.R#154) 
      5. Error: some empty data frames (@test-rows.R#166) 
      6. Failure: by_slice() works with slicers of different types (@test-rows.R#207) 
      7. Failure: by_slice() works with slicers of different types (@test-rows.R#208) 
      8. Error: by_slice() does not create .row column (@test-rows.R#214) 
      9. Failure: error is thrown when no columns to map (@test-rows.R#233) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# pysd2r

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘knitr’
      All declared Imports should be used.
    ```

# qdap

Version: 2.3.0

## In both

*   checking whether package ‘qdap’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/qdap/new/qdap.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘qdap’ ...
** package ‘qdap’ successfully unpacked and MD5 sums checked
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error : .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/romain/git/release/dplyr/revdep/library.noindex/qdap/rJava/libs/rJava.so':
  dlopen(/Users/romain/git/release/dplyr/revdep/library.noindex/qdap/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/romain/git/release/dplyr/revdep/library.noindex/qdap/rJava/libs/rJava.so
  Reason: image not found
ERROR: lazy loading failed for package ‘qdap’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/qdap/new/qdap.Rcheck/qdap’

```
### CRAN

```
* installing *source* package ‘qdap’ ...
** package ‘qdap’ successfully unpacked and MD5 sums checked
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error : .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/romain/git/release/dplyr/revdep/library.noindex/qdap/rJava/libs/rJava.so':
  dlopen(/Users/romain/git/release/dplyr/revdep/library.noindex/qdap/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/romain/git/release/dplyr/revdep/library.noindex/qdap/rJava/libs/rJava.so
  Reason: image not found
ERROR: lazy loading failed for package ‘qdap’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/qdap/old/qdap.Rcheck/qdap’

```
# qqplotr

Version: 0.0.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘knitr’ ‘purrr’ ‘rmarkdown’
      All declared Imports should be used.
    ```

# quanteda

Version: 1.3.14

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      4: eval(quote(`_fseq`(`_lhs`)), env, env)
      5: `_fseq`(`_lhs`)
      6: freduce(value, `_function_list`)
      7: withVisible(function_list[[k]](value))
      8: function_list[[k]](value)
      9: dplyr::mutate(., n_group = n())
      10: mutate.tbl_df(., n_group = n()) at /Users/romain/git/tidyverse/dplyr/R/manip.r:416
      11: mutate_impl(.data, dots) at /Users/romain/git/tidyverse/dplyr/R/tbl-df.r:91
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 2492 SKIPPED: 10 FAILED: 1
      1. Error: corpus works on dplyr grouped data.frames (#1232) (@test-corpus.R#352) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.0Mb
      sub-directories of 1Mb or more:
        data   1.3Mb
        libs   1.1Mb
        R      2.7Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 71 marked UTF-8 strings
    ```

# QuaternaryProd

Version: 1.14.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 16.9Mb
      sub-directories of 1Mb or more:
        extdata  16.2Mb
    ```

# questionr

Version: 0.7.0

## In both

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
      ‘gridExtra’ ‘knitr’ ‘MCMCpack’
      All declared Imports should be used.
    ```

# qwraps2

Version: 0.3.0

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    |&nbsp;&nbsp; min              |10.4                |
    |&nbsp;&nbsp; mean             |20.09062            |
    |&nbsp;&nbsp; mean &plusmn; sd |20.09 &plusmn; 6.03 |
    |&nbsp;&nbsp; max              |33.9                |
    |**Weight**                    |&nbsp;&nbsp;        |
    |&nbsp;&nbsp; median           |3.325               |
    |**Cylinders**                 |&nbsp;&nbsp;        |
    |&nbsp;&nbsp; 4 cyl: n (%)     |11 (34)             |
    |&nbsp;&nbsp; 6 cyl: n (%)     |7 (22)              |
    |&nbsp;&nbsp; 8 cyl: n (%)     |14 (44)             |
    > 
    > # The summary table for mtcars grouped by am (automatic or manual transmission)
    > # This will generate one column for each level of mtcars$am
    > grouped_by_table <-
    +   summary_table(dplyr::group_by(mtcars, am), our_summaries)
    Warning in rep(names(lbs), each = nrow(lbs)) :
      first element used of 'each' argument
    Error in array(x, c(length(x), 1L), if (!is.null(names(x))) list(names(x),  : 
      'data' must be of a vector type, was 'NULL'
    Calls: summary_table ... matrix -> paste -> as.matrix -> as.matrix.default -> array
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 290-292 (summary-statistics.Rmd) 
    Error: processing vignette 'summary-statistics.Rmd' failed with diagnostics:
    'data' must be of a vector type, was 'NULL'
    Execution halted
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

# r511

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# radiant.data

Version: 0.9.7

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘shinyFiles’
      All declared Imports should be used.
    ```

# radiant.model

Version: 0.9.7

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘radiant.model-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.model.predict
    > ### Title: Plot method for model.predict functions
    > ### Aliases: plot.model.predict
    > 
    > ### ** Examples
    > 
    > regress(diamonds, "price", c("carat", "clarity")) %>%
    +   predict(pred_cmd = "carat = 1:10") %>%
    +   plot(xvar = "carat")
    Warning in group_data_grouped_df(.data) :
      Detecting old grouped_df format, replacing `vars` attribute by `groups`
    Error: Column `2.5%` is unknown
    Execution halted
    ```

# railtrails

Version: 0.1.1

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1557 marked UTF-8 strings
    ```

# randomForestExplainer

Version: 0.9

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘randomForestExplainer-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_multi_way_importance
    > ### Title: Multi-way importance plot
    > ### Aliases: plot_multi_way_importance
    > 
    > ### ** Examples
    > 
    > forest <- randomForest::randomForest(Species ~ ., data = iris, localImp = TRUE)
    > plot_multi_way_importance(measure_importance(forest))
    Warning: Factor `split var` contains implicit NA, consider using `forcats::fct_explicit_na`
    Warning: Factor `split var` contains implicit NA, consider using `forcats::fct_explicit_na`
    Error in grid.Call.graphics(C_setviewport, vp, TRUE) : 
      non-finite location and/or size for viewport
    Calls: <Anonymous> ... lapply -> FUN -> push.vp.viewport -> grid.Call.graphics
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dtplyr’ ‘MASS’
      All declared Imports should be used.
    ```

# rangeModelMetadata

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘biomod2’ ‘dismo’ ‘dplyr’ ‘ecospat’ ‘ENMeval’ ‘googlesheets’
      ‘jsonlite’ ‘MASS’ ‘rgdal’ ‘rgeos’ ‘sp’ ‘spatstat’
      All declared Imports should be used.
    ```

# raptr

Version: 0.1.3

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘gurobi’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.7Mb
      sub-directories of 1Mb or more:
        data   3.6Mb
        doc    1.4Mb
    ```

# Rariant

Version: 1.16.0

## In both

*   checking examples ... ERROR
    ```
    ...
    > 
    > ### ** Examples
    > 
    >   library(ggbio)
    Warning: package 'ggbio' was built under R version 3.5.1
    Loading required package: ggplot2
    Need specific help about ggbio? try mailing 
     the maintainer or visit http://tengfei.github.com/ggbio/
    
    Attaching package: 'ggbio'
    
    The following objects are masked from 'package:ggplot2':
    
        geom_bar, geom_rect, geom_segment, ggsave, stat_bin, stat_identity,
        xlim
    
    >   library(GenomicRanges)
    >   library(BSgenome.Hsapiens.UCSC.hg19)
    Error in library(BSgenome.Hsapiens.UCSC.hg19) : 
      there is no package called 'BSgenome.Hsapiens.UCSC.hg19'
    Execution halted
    ```

*   checking whether package ‘Rariant’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘GenomicRanges’ was built under R version 3.5.1
      Warning: package ‘IRanges’ was built under R version 3.5.1
      Warning: package ‘DelayedArray’ was built under R version 3.5.1
      Warning: package ‘Rsamtools’ was built under R version 3.5.1
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/Rariant/new/Rariant.Rcheck/00install.out’ for details.
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘BSgenome.Hsapiens.UCSC.hg19’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  7.9Mb
      sub-directories of 1Mb or more:
        doc       2.3Mb
        extdata   5.2Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    tallyBamRegion: no visible global function definition for 'PileupParam'
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/Rariant/new/Rariant.Rcheck/00_pkg_src/Rariant/R/tally.R:101-110)
    tallyBamRegion: no visible global function definition for
      'ScanBamParam'
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/Rariant/new/Rariant.Rcheck/00_pkg_src/Rariant/R/tally.R:112)
    tallyBamRegion: no visible global function definition for 'pileup'
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/Rariant/new/Rariant.Rcheck/00_pkg_src/Rariant/R/tally.R:114)
    Undefined global functions or variables:
      pileup PileupParam ScanBamParam
    ```

*   checking installed files from ‘inst/doc’ ... NOTE
    ```
    The following files should probably not be installed:
      ‘rariant-inspect-ci.png’, ‘rariant-inspect-shift.png’
    
    Consider the use of a .Rinstignore file: see ‘Writing R Extensions’,
    or move the vignette sources from ‘inst/doc’ to ‘vignettes’.
    ```

# rattle

Version: 5.2.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
      Running 'texi2dvi' on 'rattle.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `fancyhdr.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    
    ! Emergency stop.
    <read *> 
             
    l.12 \usepackage
                    {lastpage}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    Calls: buildVignettes -> texi2pdf -> texi2dvi
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      ‘cairoDevice’ ‘gWidgetsRGtk2’ ‘playwith’ ‘rggobi’ ‘RGtk2’
      ‘RGtk2Extras’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 10.4Mb
      sub-directories of 1Mb or more:
        data   3.0Mb
        etc    1.9Mb
        po     1.2Mb
        R      3.5Mb
    ```

# RBesT

Version: 1.3-7

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.5Mb
      sub-directories of 1Mb or more:
        doc    1.9Mb
        libs   2.2Mb
        R      1.1Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# rccmisc

Version: 0.3.7

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# rclimateca

Version: 1.0.2

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 24 marked UTF-8 strings
    ```

# rcv

Version: 0.2.1

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘rcv-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: rcv_tally
    > ### Title: Determines RCV round results in a dataframe
    > ### Aliases: rcv_tally
    > 
    > ### ** Examples
    > 
    > rcv_tally(image = sf_bos_clean, rcvcontest = "Board of Supervisors, District 7")
    Error in n() : could not find function "n"
    Calls: rcv_tally ... <Anonymous> -> <Anonymous> -> summarise.tbl_df -> summarise_impl
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 6543 marked UTF-8 strings
    ```

# RDML

Version: 0.9-9

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        doc   2.4Mb
        R     1.6Mb
    ```

# Rdrools

Version: 1.1.1

## In both

*   checking whether package ‘Rdrools’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/Rdrools/new/Rdrools.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘Rdrools’ ...
** package ‘Rdrools’ successfully unpacked and MD5 sums checked
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rJava’:
 .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/romain/git/release/dplyr/revdep/library.noindex/Rdrools/rJava/libs/rJava.so':
  dlopen(/Users/romain/git/release/dplyr/revdep/library.noindex/Rdrools/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/romain/git/release/dplyr/revdep/library.noindex/Rdrools/rJava/libs/rJava.so
  Reason: image not found
Error : package ‘rJava’ could not be loaded
ERROR: lazy loading failed for package ‘Rdrools’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/Rdrools/new/Rdrools.Rcheck/Rdrools’

```
### CRAN

```
* installing *source* package ‘Rdrools’ ...
** package ‘Rdrools’ successfully unpacked and MD5 sums checked
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rJava’:
 .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/romain/git/release/dplyr/revdep/library.noindex/Rdrools/rJava/libs/rJava.so':
  dlopen(/Users/romain/git/release/dplyr/revdep/library.noindex/Rdrools/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/romain/git/release/dplyr/revdep/library.noindex/Rdrools/rJava/libs/rJava.so
  Reason: image not found
Error : package ‘rJava’ could not be loaded
ERROR: lazy loading failed for package ‘Rdrools’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/Rdrools/old/Rdrools.Rcheck/Rdrools’

```
# rdrop2

Version: 0.8.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘digest’
      All declared Imports should be used.
    ```

# readabs

Version: 0.2.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘sjmisc’ ‘stringr’
      All declared Imports should be used.
    ```

# readat

Version: 1.6.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    sfread: no visible binding for global variable ‘header’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/readat/new/readat.Rcheck/00_pkg_src/readat/R/sfread.R:54)
    sfread: no visible binding for global variable ‘nrows’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/readat/new/readat.Rcheck/00_pkg_src/readat/R/sfread.R:54)
    Undefined global functions or variables:
      header nrows
    ```

# recipes

Version: 0.1.4

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      14: dimRed::FastICA at /Users/romain/git/release/dplyr/revdep/checks.noindex/recipes/new/recipes.Rcheck/00_pkg_src/recipes/R/ica.R:158
      15: getExportedValue(pkg, name)
      16: asNamespace(ns)
      17: getNamespace(ns)
      18: tryCatch(loadNamespace(name), error = function(e) stop(e))
      19: tryCatchList(expr, classes, parentenv, handlers)
      20: tryCatchOne(expr, names, parentenv, handlers[[1L]])
      21: value[[3L]](cond)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 1121 SKIPPED: 9 FAILED: 1
      1. Error: printing (@test_ica.R#127) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘dimRed’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘RcppRoll’
      All declared Imports should be used.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘dimRed’
    ```

# replyr

Version: 0.9.8

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘rquery’
    ```

# reproducible

Version: 0.2.5

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘devtools’
      All declared Imports should be used.
    ```

# rerddap

Version: 0.4.2

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘taxize’
    ```

# restfulSE

Version: 1.2.3

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘GO.db’
    
    Packages suggested but not available for checking:
      ‘org.Mm.eg.db’ ‘org.Hs.eg.db’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# revengc

Version: 1.0.3

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Error: processing vignette 'duchscherer-stewart-urban.ltx' failed with diagnostics:
    Running 'texi2dvi' on 'duchscherer-stewart-urban.ltx' failed.
    LaTeX errors:
    ! LaTeX Error: File `float.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    
    ! Emergency stop.
    <read *> 
             
    l.16 \usepackage
                    {graphicx}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    Execution halted
    ```

# rfacebookstat

Version: 1.8.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘bitops’
      All declared Imports should be used.
    ```

# rfbCNPJ

Version: 0.1.1

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 27 marked UTF-8 strings
    ```

# rfishbase

Version: 3.0.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 44 marked UTF-8 strings
    ```

# RGMQL

Version: 1.0.2

## In both

*   checking whether package ‘RGMQL’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/RGMQL/new/RGMQL.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘RGMQL’ ...
** R
** inst
** byte-compile and prepare package for lazy loading
Error : .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/romain/git/release/dplyr/revdep/library.noindex/RGMQL/rJava/libs/rJava.so':
  dlopen(/Users/romain/git/release/dplyr/revdep/library.noindex/RGMQL/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/romain/git/release/dplyr/revdep/library.noindex/RGMQL/rJava/libs/rJava.so
  Reason: image not found
ERROR: lazy loading failed for package ‘RGMQL’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/RGMQL/new/RGMQL.Rcheck/RGMQL’

```
### CRAN

```
* installing *source* package ‘RGMQL’ ...
** R
** inst
** byte-compile and prepare package for lazy loading
Error : .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/romain/git/release/dplyr/revdep/library.noindex/RGMQL/rJava/libs/rJava.so':
  dlopen(/Users/romain/git/release/dplyr/revdep/library.noindex/RGMQL/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/romain/git/release/dplyr/revdep/library.noindex/RGMQL/rJava/libs/rJava.so
  Reason: image not found
ERROR: lazy loading failed for package ‘RGMQL’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/RGMQL/old/RGMQL.Rcheck/RGMQL’

```
# rhmmer

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# riingo

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘rlang’
      All declared Imports should be used.
    ```

# RImmPort

Version: 1.8.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.9Mb
      sub-directories of 1Mb or more:
        extdata   3.8Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    buildNewSqliteDb: no visible global function definition for
      ‘dbListTables’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/RImmPort/new/RImmPort.Rcheck/00_pkg_src/RImmPort/R/ImmPortSqlite.R:1890)
    Undefined global functions or variables:
      dbListTables
    ```

# rmapzen

Version: 0.4.1

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 31 marked UTF-8 strings
    ```

# rmcfs

Version: 1.2.15

## In both

*   checking whether package ‘rmcfs’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/rmcfs/new/rmcfs.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘rmcfs’ ...
** package ‘rmcfs’ successfully unpacked and MD5 sums checked
** R
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rJava’:
 .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/romain/git/release/dplyr/revdep/library.noindex/rmcfs/rJava/libs/rJava.so':
  dlopen(/Users/romain/git/release/dplyr/revdep/library.noindex/rmcfs/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/romain/git/release/dplyr/revdep/library.noindex/rmcfs/rJava/libs/rJava.so
  Reason: image not found
Error : package ‘rJava’ could not be loaded
ERROR: lazy loading failed for package ‘rmcfs’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/rmcfs/new/rmcfs.Rcheck/rmcfs’

```
### CRAN

```
* installing *source* package ‘rmcfs’ ...
** package ‘rmcfs’ successfully unpacked and MD5 sums checked
** R
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rJava’:
 .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/romain/git/release/dplyr/revdep/library.noindex/rmcfs/rJava/libs/rJava.so':
  dlopen(/Users/romain/git/release/dplyr/revdep/library.noindex/rmcfs/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/romain/git/release/dplyr/revdep/library.noindex/rmcfs/rJava/libs/rJava.so
  Reason: image not found
Error : package ‘rJava’ could not be loaded
ERROR: lazy loading failed for package ‘rmcfs’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/rmcfs/old/rmcfs.Rcheck/rmcfs’

```
# RMCriteria

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# rmytarget

Version: 2.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘jsonlite’
      All declared Imports should be used.
    ```

# RNeXML

Version: 2.2.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    No ENTREZ API key provided
     Get one via use_entrez()
    See https://ncbiinsights.ncbi.nlm.nih.gov/2017/11/02/new-api-keys-for-the-e-utilities/
    
    Retrieving data for taxon 'Allenopithecus_nigroviridis'
    
    No ENTREZ API key provided
     Get one via use_entrez()
    See https://ncbiinsights.ncbi.nlm.nih.gov/2017/11/02/new-api-keys-for-the-e-utilities/
    
    Retrieving data for taxon 'Allocebus_trichotis'
    
    Quitting from lines 205-209 (metadata.Rmd) 
    Error: processing vignette 'metadata.Rmd' failed with diagnostics:
    '{"error":"API rate limit exceeded","api-key":"90.113.128.205","count":"4","limit":"3"}
    ' does not exist in current working directory ('/Users/romain/git/release/dplyr/revdep/checks.noindex/RNeXML/new/RNeXML.Rcheck/vign_test/RNeXML/vignettes').
    Execution halted
    ```

# roahd

Version: 1.4.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        data   2.9Mb
        doc    1.6Mb
    ```

# robotstxt

Version: 0.6.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘future’
      All declared Imports should be used.
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

# rpcdsearch

Version: 1.0

## In both

*   checking whether package ‘rpcdsearch’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/rpcdsearch/new/rpcdsearch.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘rpcdsearch’ ...
** package ‘rpcdsearch’ successfully unpacked and MD5 sums checked
** R
** inst
** byte-compile and prepare package for lazy loading
Error : .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/romain/git/release/dplyr/revdep/library.noindex/rpcdsearch/rJava/libs/rJava.so':
  dlopen(/Users/romain/git/release/dplyr/revdep/library.noindex/rpcdsearch/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/romain/git/release/dplyr/revdep/library.noindex/rpcdsearch/rJava/libs/rJava.so
  Reason: image not found
ERROR: lazy loading failed for package ‘rpcdsearch’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/rpcdsearch/new/rpcdsearch.Rcheck/rpcdsearch’

```
### CRAN

```
* installing *source* package ‘rpcdsearch’ ...
** package ‘rpcdsearch’ successfully unpacked and MD5 sums checked
** R
** inst
** byte-compile and prepare package for lazy loading
Error : .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/romain/git/release/dplyr/revdep/library.noindex/rpcdsearch/rJava/libs/rJava.so':
  dlopen(/Users/romain/git/release/dplyr/revdep/library.noindex/rpcdsearch/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/romain/git/release/dplyr/revdep/library.noindex/rpcdsearch/rJava/libs/rJava.so
  Reason: image not found
ERROR: lazy loading failed for package ‘rpcdsearch’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/rpcdsearch/old/rpcdsearch.Rcheck/rpcdsearch’

```
# rPref

Version: 1.2

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > library(dplyr)
    
    Attaching package: ‘dplyr’
    
    The following object is masked from ‘package:rPref’:
    
        between
    
    The following objects are masked from ‘package:stats’:
    
        filter, lag
    
    The following objects are masked from ‘package:base’:
    
        intersect, setdiff, setequal, union
    
    > psel(group_by(mtcars, cyl), low(mpg))
    Error in psel.indices(df, pref, .dots = vars) : 
      Could not find grouping indices in grouped data.frame!
    Calls: psel -> psel.indices
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 96-99 (introduction.Rmd) 
    Error: processing vignette 'introduction.Rmd' failed with diagnostics:
    Could not find grouping indices in grouped data.frame!
    Execution halted
    ```

## In both

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# rprev

Version: 1.0.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘doParallel’ ‘foreach’
      All declared Imports should be used.
    ```

# Rraven

Version: 1.0.4

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘warbleR’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
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

Version: 0.11.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tidyselect’
      All declared Imports should be used.
    ```

# RSDA

Version: 2.0.8

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘randomcoloR’
      All declared Imports should be used.
    ```

# rsimsum

Version: 0.3.3

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘ggthemes’
    ```

# rsinaica

Version: 0.6.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 467 marked UTF-8 strings
    ```

# Rspotify

Version: 0.1.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘magrittr’
      All declared Imports should be used.
    ```

# RSwissMaps

Version: 0.1.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 18627 marked UTF-8 strings
    ```

# RTCGA

Version: 1.10.0

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
      Welcome to the RTCGA (version: 1.10.0).
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
    ...
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/RTCGA/new/RTCGA.Rcheck/00_pkg_src/RTCGA/R/ggbiplot.R:157-161)
    ggbiplot: no visible binding for global variable ‘xvar’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/RTCGA/new/RTCGA.Rcheck/00_pkg_src/RTCGA/R/ggbiplot.R:157-161)
    ggbiplot: no visible binding for global variable ‘yvar’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/RTCGA/new/RTCGA.Rcheck/00_pkg_src/RTCGA/R/ggbiplot.R:157-161)
    ggbiplot: no visible binding for global variable ‘angle’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/RTCGA/new/RTCGA.Rcheck/00_pkg_src/RTCGA/R/ggbiplot.R:157-161)
    ggbiplot: no visible binding for global variable ‘hjust’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/RTCGA/new/RTCGA.Rcheck/00_pkg_src/RTCGA/R/ggbiplot.R:157-161)
    read.mutations: no visible binding for global variable ‘.’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/RTCGA/new/RTCGA.Rcheck/00_pkg_src/RTCGA/R/readTCGA.R:383)
    read.mutations: no visible binding for global variable ‘.’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/RTCGA/new/RTCGA.Rcheck/00_pkg_src/RTCGA/R/readTCGA.R:386)
    read.rnaseq: no visible binding for global variable ‘.’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/RTCGA/new/RTCGA.Rcheck/00_pkg_src/RTCGA/R/readTCGA.R:372-375)
    survivalTCGA: no visible binding for global variable ‘times’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/RTCGA/new/RTCGA.Rcheck/00_pkg_src/RTCGA/R/survivalTCGA.R:101-137)
    whichDateToUse: no visible binding for global variable ‘.’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/RTCGA/new/RTCGA.Rcheck/00_pkg_src/RTCGA/R/downloadTCGA.R:167-168)
    Undefined global functions or variables:
      . angle hjust muted times varname xvar yvar
    ```

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘RTCGA.rnaseq’, ‘RTCGA.clinical’, ‘RTCGA.mutations’, ‘RTCGA.CNV’, ‘RTCGA.RPPA’, ‘RTCGA.mRNA’, ‘RTCGA.miRNASeq’, ‘RTCGA.methylation’
    ```

# rtimicropem

Version: 1.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘R6’
      All declared Imports should be used.
    ```

# rtrek

Version: 0.1.0

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘rtrek-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: tile_coords
    > ### Title: Simple CRS coordinates
    > ### Aliases: tile_coords
    > 
    > ### ** Examples
    > 
    > d <- data.frame(row = c(0, 3222, 6445), col = c(0, 4000, 8000))
    > tile_coords(d, "galaxy1")
    Error: slice condition does not evaluate to an integer or numeric vector. 
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > test_check("rtrek")
      Total pages to retrieve all results: 64 
      ── 1. Error: tile_coords returns as expected (@test-tiles.R#5)  ────────────────
      slice condition does not evaluate to an integer or numeric vector. 
      1: tile_coords(d, "galaxy1") at testthat/test-tiles.R:5
      2: dplyr::slice(rtrek::stTiles, .data[["id"]] == id0) at /Users/romain/git/release/dplyr/revdep/checks.noindex/rtrek/new/rtrek.Rcheck/00_pkg_src/rtrek/R/tiles.R:26
      3: slice.tbl_df(rtrek::stTiles, .data[["id"]] == id0) at /Users/romain/git/tidyverse/dplyr/R/manip.r:169
      4: slice_impl(.data, quo) at /Users/romain/git/tidyverse/dplyr/R/tbl-df.r:76
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 21 SKIPPED: 3 FAILED: 1
      1. Error: tile_coords returns as expected (@test-tiles.R#5) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
    Warning in block_exec(params) :
      Failed to tidy R code in chunk 'st_book_series2'. Reason:
    Error in loadNamespace(name) : there is no package called 'formatR'
    
    Warning in block_exec(params) :
      Failed to tidy R code in chunk 'stTiles'. Reason:
    Error in loadNamespace(name) : there is no package called 'formatR'
    
    Warning in block_exec(params) :
      Failed to tidy R code in chunk 'stGeo'. Reason:
    Error in loadNamespace(name) : there is no package called 'formatR'
    
    Warning in block_exec(params) :
      Failed to tidy R code in chunk 'tile_coords'. Reason:
    Error in loadNamespace(name) : there is no package called 'formatR'
    
    Quitting from lines 76-79 (rtrek.Rmd) 
    Error: processing vignette 'rtrek.Rmd' failed with diagnostics:
    slice condition does not evaluate to an integer or numeric vector. 
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 992 marked UTF-8 strings
    ```

# rtrends

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# RtutoR

Version: 1.2

## In both

*   checking whether package ‘RtutoR’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/RtutoR/new/RtutoR.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘RtutoR’ ...
** package ‘RtutoR’ successfully unpacked and MD5 sums checked
** R
** inst
** byte-compile and prepare package for lazy loading
Error : .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/romain/git/release/dplyr/revdep/library.noindex/RtutoR/rJava/libs/rJava.so':
  dlopen(/Users/romain/git/release/dplyr/revdep/library.noindex/RtutoR/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/romain/git/release/dplyr/revdep/library.noindex/RtutoR/rJava/libs/rJava.so
  Reason: image not found
ERROR: lazy loading failed for package ‘RtutoR’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/RtutoR/new/RtutoR.Rcheck/RtutoR’

```
### CRAN

```
* installing *source* package ‘RtutoR’ ...
** package ‘RtutoR’ successfully unpacked and MD5 sums checked
** R
** inst
** byte-compile and prepare package for lazy loading
Error : .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/romain/git/release/dplyr/revdep/library.noindex/RtutoR/rJava/libs/rJava.so':
  dlopen(/Users/romain/git/release/dplyr/revdep/library.noindex/RtutoR/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/romain/git/release/dplyr/revdep/library.noindex/RtutoR/rJava/libs/rJava.so
  Reason: image not found
ERROR: lazy loading failed for package ‘RtutoR’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/RtutoR/old/RtutoR.Rcheck/RtutoR’

```
# rubias

Version: 0.1.0

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘rubias-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: assess_reference_loo
    > ### Title: Simulate mixtures and estimate reporting group and collection
    > ###   proportions.
    > ### Aliases: assess_reference_loo
    > 
    > ### ** Examples
    > 
    > # very small number of reps so it is quick enough for example
    > ale_dev <- assess_reference_loo(alewife, 17, reps = 5)
    Error in n() : could not find function "n"
    Calls: assess_reference_loo ... <Anonymous> -> <Anonymous> -> mutate.tbl_df -> mutate_impl
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 104-107 (rubias-overview.Rmd) 
    Error: processing vignette 'rubias-overview.Rmd' failed with diagnostics:
    Columns `indiv`, `log_likelihood`, `expected_mean`, `expected_var` must be length 1 or 2077452, not 53268, 53268, 53268, 53268
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘ggplot2’
      All declared Imports should be used.
    ```

# rwavelet

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# RxODE

Version: 0.8.0-9

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.5Mb
      sub-directories of 1Mb or more:
        doc    1.6Mb
        libs   2.0Mb
        R      1.6Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘n1qn1’
      All declared Imports should be used.
    ```

# rzeit2

Version: 0.2.0

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Quitting from lines 90-104 (sentiment.Rmd) 
    Error: processing vignette 'sentiment.Rmd' failed with diagnostics:
    Column `x` must be a 1d atomic vector or a list
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 841 marked UTF-8 strings
    ```

# SanFranBeachWater

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tibble’
      All declared Imports should be used.
    ```

# SanzCircos

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘purrr’ ‘tidyr’
      All declared Imports should be used.
    ```

# sars

Version: 1.0.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        refs   4.3Mb
    ```

# scater

Version: 1.8.4

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
             dims = ncomponents, check_duplicates = FALSE, ...) at /Users/romain/git/release/dplyr/revdep/checks.noindex/scater/new/scater.Rcheck/00_pkg_src/scater/R/runTSNE.R:90
      9: Rtsne.default(vals, initial_dims = initial_dims, pca = pca, perplexity = perplexity, 
             dims = ncomponents, check_duplicates = FALSE, ...)
      10: .check_tsne_params(nrow(X), dims = dims, perplexity = perplexity, theta = theta, 
             max_iter = max_iter, verbose = verbose, Y_init = Y_init, stop_lying_iter = stop_lying_iter, 
             mom_switch_iter = mom_switch_iter, momentum = momentum, final_momentum = final_momentum, 
             eta = eta, exaggeration_factor = exaggeration_factor)
      11: stop("dims should be either 1, 2 or 3")
      
      Collapsing expression to 500 features.Kallisto log not provided - assuming all runs successful══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 1012 SKIPPED: 0 FAILED: 1
      1. Error: we can produce TSNE plots (@test-plotting.R#330) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking whether package ‘scater’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘GenomicRanges’ was built under R version 3.5.1
      Warning: package ‘IRanges’ was built under R version 3.5.1
      Warning: package ‘DelayedArray’ was built under R version 3.5.1
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/scater/new/scater.Rcheck/00install.out’ for details.
    ```

*   checking examples ... WARNING
    ```
    Found the following significant warnings:
    
      Warning: 'read10xResults' is deprecated.
      Warning: 'downsampleCounts' is deprecated.
      Warning: 'normalizeExprs' is deprecated.
      Warning: 'normalizeExprs' is deprecated.
      Warning: 'normalizeExprs' is deprecated.
      Warning: 'normalizeExprs' is deprecated.
      Warning: 'normalizeExprs' is deprecated.
      Warning: 'normalizeExprs' is deprecated.
      Warning: 'read10xResults' is deprecated.
    Deprecated functions may be defunct as soon as of the next release of
    R.
    See ?Deprecated.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 15.3Mb
      sub-directories of 1Mb or more:
        doc       5.4Mb
        extdata   2.9Mb
        libs      4.8Mb
    ```

# scFeatureFilter

Version: 1.0.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Error: processing vignette 'Introduction.Rmd' failed with diagnostics:
    there is no package called ‘BiocStyle’
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘scRNAseq’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.5Mb
      sub-directories of 1Mb or more:
        data   3.7Mb
        doc    2.4Mb
    ```

# scfind

Version: 1.2.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Error: processing vignette 'scfind.Rmd' failed with diagnostics:
    there is no package called ‘BiocStyle’
    Execution halted
    ```

# scmap

Version: 1.2.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Error: processing vignette 'scmap.Rmd' failed with diagnostics:
    there is no package called ‘BiocStyle’
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘Biobase’
      All declared Imports should be used.
    ```

# scoper

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘shazam’
      All declared Imports should be used.
    ```

# sdStaf

Version: 1.0.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘rgdal’ ‘rgeos’ ‘tidyr’
      All declared Imports should be used.
    ```

# segclust2d

Version: 0.1.0

## In both

*   checking whether package ‘segclust2d’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/segclust2d/new/segclust2d.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘segclust2d’ ...
** package ‘segclust2d’ successfully unpacked and MD5 sums checked
** libs
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romain/git/release/dplyr/revdep/library.noindex/segclust2d/Rcpp/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/segclust2d/RcppArmadillo/include" -I/usr/local/include  -fopenmp -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘segclust2d’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/segclust2d/new/segclust2d.Rcheck/segclust2d’

```
### CRAN

```
* installing *source* package ‘segclust2d’ ...
** package ‘segclust2d’ successfully unpacked and MD5 sums checked
** libs
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/romain/git/release/dplyr/revdep/library.noindex/segclust2d/Rcpp/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/segclust2d/RcppArmadillo/include" -I/usr/local/include  -fopenmp -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘segclust2d’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/segclust2d/old/segclust2d.Rcheck/segclust2d’

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

# semdrw

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘lavaan’ ‘psych’ ‘semPlot’ ‘semTools’ ‘shinyAce’
      All declared Imports should be used.
    ```

# seplyr

Version: 0.8.2

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    >  plan <- partition_mutate_se(program)
    >  print(plan)
    $group00001
    ifebtest_yn1rz9n17hxe ifebtest_jnu6h68xwchg 
                "(a+b)>1"   "runif(n()) >= 0.5" 
    
    $group00002
                                                                                 a 
         "ifelse( ifebtest_yn1rz9n17hxe, ifelse( ifebtest_jnu6h68xwchg, 0, a), a)" 
                                                                                 b 
    "ifelse( ifebtest_yn1rz9n17hxe, ifelse( !( ifebtest_jnu6h68xwchg ), 0, b), b)" 
                                                                            edited 
                                    "ifelse( ifebtest_yn1rz9n17hxe, TRUE, edited)" 
    
    > 
    >  res <- d %.>%
    +    mutate_seb(., plan) %.>%
    +    select_se(., grepdf('^ifebtest_.*', ., invert=TRUE))
    Error in n() : could not find function "n"
    Calls: %.>% ... as.data.frame -> mutate -> mutate.tbl_df -> mutate_impl -> runif
    Execution halted
    ```

# seqCAT

Version: 1.2.1

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘seqCAT-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_impacts
    > ### Title: Plot SNV impact distribution
    > ### Aliases: plot_impacts
    > 
    > ### ** Examples
    > 
    > # Load test comparison data
    > data(test_comparison)
    > 
    > # Plot the impact distribution
    > impacts <- plot_impacts(test_comparison)
    Error in n() : could not find function "n"
    Calls: plot_impacts -> <Anonymous> -> summarise_.tbl_df -> summarise_impl
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > 
      > test_check("seqCAT")
      ── 1. Error: (unknown) (@test_06_plot_impacts.R#8)  ────────────────────────────
      could not find function "n"
      1: plot_impacts(test_comparison) at testthat/test_06_plot_impacts.R:8
      2: dplyr::summarise_(data, count = ~n()) at /Users/romain/git/release/dplyr/revdep/checks.noindex/seqCAT/new/seqCAT.Rcheck/00_pkg_src/seqCAT/R/plot_impacts.R:58
      3: summarise_.tbl_df(data, count = ~n()) at /Users/romain/git/tidyverse/dplyr/R/manip.r:264
      4: summarise_impl(.data, dots) at /Users/romain/git/tidyverse/dplyr/R/tbl-df.r:107
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 88 SKIPPED: 0 FAILED: 1
      1. Error: (unknown) (@test_06_plot_impacts.R#8) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Creating SNV profile ...
    Created and stored SNV profile for "HCT116" in [hct116_profile.txt].
    Reading VCF file ...
    Creating SNV profile ...
    Created and stored SNV profile for "RKO" in [rko_profile.txt].
    Reading COSMIC cell line data ...
    Reading COSMIC cell line data ...
    Reading profile for HCT116 in file hct116_profile.txt ...
    Reading profile for RKO in file rko_profile.txt ...
    Comparing HCT116 and RKO ...
    Comparing HCT116 and RKO ...
    Comparing HCT116 and COSMIC.HCT116 ...
    Reading VCF file ...
    Creating SNV profile ...
    Created and stored SNV profile for "HKE3" in [hke3_profile.txt].
    Reading profile for HKE3 in file hke3_profile.txt ...
    Comparing HCT116 and HKE3 ...
    Quitting from lines 343-345 (seqCAT.Rmd) 
    Error: processing vignette 'seqCAT.Rmd' failed with diagnostics:
    could not find function "n"
    Execution halted
    ```

## In both

*   checking whether package ‘seqCAT’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘GenomicRanges’ was built under R version 3.5.1
      Warning: package ‘IRanges’ was built under R version 3.5.1
      Warning: package ‘DelayedArray’ was built under R version 3.5.1
      Warning: package ‘Rsamtools’ was built under R version 3.5.1
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/seqCAT/new/seqCAT.Rcheck/00install.out’ for details.
    ```

# seqcombo

Version: 1.2.0

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘seqcombo-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot
    > ### Title: plot method for SeqDiff object
    > ### Aliases: plot plot,SeqDiff,ANY-method
    > 
    > ### ** Examples
    > 
    > fas <- list.files(system.file("examples","GVariation", package="seqcombo"), pattern="fas", full.names=TRUE)
    > x1 <- seqdiff(fas[1], reference=1)
    > plot(x1)
    Error in n() : could not find function "n"
    Calls: plot ... <Anonymous> -> summarize -> summarise.tbl_df -> summarise_impl
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
      ...
    Loading required package: igraph
    
    Attaching package: 'igraph'
    
    The following object is masked from 'package:tibble':
    
        as_data_frame
    
    The following objects are masked from 'package:stats':
    
        decompose, spectrum
    
    The following object is masked from 'package:base':
    
        union
    
    Quitting from lines 54-55 (seqcombo.Rmd) 
    Error: processing vignette 'seqcombo.Rmd' failed with diagnostics:
    could not find function "n"
    Execution halted
    ```

# SeqVarTools

Version: 1.18.0

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### Name: variantInfo
    > ### Title: Variant info
    > ### Aliases: variantInfo variantInfo,SeqVarGDSClass-method
    > ###   expandedVariantIndex expandedVariantIndex,SeqVarGDSClass-method
    > 
    > ### ** Examples
    > 
    > gds <- seqOpen(seqExampleFileName("gds"))
    > seqSetFilter(gds, variant.sel=1323:1327)
    # of selected variants: 5
    > variantInfo(gds, alleles=TRUE)
      variant.id chr      pos ref  alt
    1       1323  21 44213462   C T,CT
    2       1324  21 44214985   G    A
    3       1325  21 44215700   C    T
    4       1326  22 16042444   C    G
    5       1327  22 16042793   A    G
    > variantInfo(gds, alleles=TRUE, expanded=TRUE)
    Error in n() : could not find function "n"
    Calls: variantInfo ... variantInfo -> .local -> mutate_ -> mutate_.tbl_df -> mutate_impl
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test.R’ failed.
    Last 13 lines of output:
      
       
      1 Test Suite : 
      SeqVarTools RUnit Tests - 133 test functions, 1 error, 0 failures
      ERROR in test_variantInfo: Error in n() : could not find function "n"
      
      Test files with failing tests
      
         test_getData.R 
           test_variantInfo 
      
      
      Error in BiocGenerics:::testPackage("SeqVarTools") : 
        unit tests failed for package SeqVarTools
      Execution halted
    ```

## In both

*   checking whether package ‘SeqVarTools’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘SeqArray’ was built under R version 3.5.1
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/SeqVarTools/new/SeqVarTools.Rcheck/00install.out’ for details.
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    ...
    
        Vignettes contain introductory material; view with
        'browseVignettes()'. To cite Bioconductor, see
        'citation("Biobase")', and for packages 'citation("pkgname")'.
    
    Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
      Running 'texi2dvi' on 'SeqVarTools.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `fancyhdr.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    
    ! Emergency stop.
    <read *> 
             
    l.171 \pagestyle
                    {fancy}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    Calls: buildVignettes -> texi2pdf -> texi2dvi
    Execution halted
    ```

# Seurat

Version: 2.3.4

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘loomR’
    ```

# sevenbridges

Version: 1.10.5

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.0Mb
      sub-directories of 1Mb or more:
        doc   2.9Mb
        R     3.3Mb
    ```

# sf

Version: 0.7-1

## In both

*   checking whether package ‘sf’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/sf/new/sf.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘sf’ ...
** package ‘sf’ successfully unpacked and MD5 sums checked
configure: CC: clang
configure: CXX: clang++ -std=gnu++11
checking for gdal-config... no
no
configure: error: gdal-config not found or not executable.
ERROR: configuration failed for package ‘sf’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/sf/new/sf.Rcheck/sf’

```
### CRAN

```
* installing *source* package ‘sf’ ...
** package ‘sf’ successfully unpacked and MD5 sums checked
configure: CC: clang
configure: CXX: clang++ -std=gnu++11
checking for gdal-config... no
no
configure: error: gdal-config not found or not executable.
ERROR: configuration failed for package ‘sf’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/sf/old/sf.Rcheck/sf’

```
# shiny.semantic

Version: 0.2.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘utils’
      All declared Imports should be used.
    ```

# shinyAce

Version: 0.3.2

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.9Mb
      sub-directories of 1Mb or more:
        www   7.7Mb
    ```

# shinyaframe

Version: 1.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘shiny’
      All declared Imports should be used.
    ```

# shinyHeatmaply

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘htmlwidgets’ ‘jsonlite’ ‘RColorBrewer’ ‘viridis’
      All declared Imports should be used.
    ```

# SIBER

Version: 2.1.3

## In both

*   checking examples ... ERROR
    ```
    ...
    > ### ** Examples
    > 
    > x <- stats::rnorm(50)
    > y <- stats::rnorm(50)
    > parms <- list()
    > parms$n.iter <- 2 * 10^3
    > parms$n.burnin <- 500
    > parms$n.thin <- 2     
    > parms$n.chains <- 2    
    > priors <- list()
    > priors$R <- 1 * diag(2)
    > priors$k <- 2
    > priors$tau.mu <- 1.0E-3
    > fitEllipse(x, y, parms, priors)
    Error: .onLoad failed in loadNamespace() for 'rjags', details:
      call: dyn.load(file, DLLpath = DLLpath, ...)
      error: unable to load shared object '/Users/romain/git/release/dplyr/revdep/library.noindex/SIBER/rjags/libs/rjags.so':
      dlopen(/Users/romain/git/release/dplyr/revdep/library.noindex/SIBER/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
      Referenced from: /Users/romain/git/release/dplyr/revdep/library.noindex/SIBER/rjags/libs/rjags.so
      Reason: image not found
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 74-93 (Centroid-Vectors.Rmd) 
    Error: processing vignette 'Centroid-Vectors.Rmd' failed with diagnostics:
    .onLoad failed in loadNamespace() for 'rjags', details:
      call: dyn.load(file, DLLpath = DLLpath, ...)
      error: unable to load shared object '/Users/romain/git/release/dplyr/revdep/library.noindex/SIBER/rjags/libs/rjags.so':
      dlopen(/Users/romain/git/release/dplyr/revdep/library.noindex/SIBER/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
      Referenced from: /Users/romain/git/release/dplyr/revdep/library.noindex/SIBER/rjags/libs/rjags.so
      Reason: image not found
    Execution halted
    ```

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

# sigmajs

Version: 0.1.2

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.0Mb
      sub-directories of 1Mb or more:
        doc   5.3Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 28 marked UTF-8 strings
    ```

# SimDesign

Version: 1.11

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘doMPI’
    ```

# simglm

Version: 0.7.1

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      2: mar_missing(sim_data, resp_var, new_outcome, miss_cov, mar_prop) at /Users/romain/git/release/dplyr/revdep/checks.noindex/simglm/new/simglm.Rcheck/00_pkg_src/simglm/R/missing_data.r:31
      3: dplyr::slice(miss_per, rep(1:n(), times = num)) at /Users/romain/git/release/dplyr/revdep/checks.noindex/simglm/new/simglm.Rcheck/00_pkg_src/simglm/R/missing_data.r:214
      4: slice.data.frame(miss_per, rep(1:n(), times = num)) at /Users/romain/git/tidyverse/dplyr/R/manip.r:169
      5: as.data.frame(slice(tbl_df(.data), ..., .preserve = .preserve)) at /Users/romain/git/tidyverse/dplyr/R/dataframe.R:71
      6: slice(tbl_df(.data), ..., .preserve = .preserve) at /Users/romain/git/tidyverse/dplyr/R/dataframe.R:71
      7: slice.tbl_df(tbl_df(.data), ..., .preserve = .preserve) at /Users/romain/git/tidyverse/dplyr/R/manip.r:169
      8: slice_impl(.data, quo) at /Users/romain/git/tidyverse/dplyr/R/tbl-df.r:76
      9: ~rep(1:n(), times = num) at /Users/romain/git/tidyverse/dplyr/R/RcppExports.R:92
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 130 SKIPPED: 0 FAILED: 1
      1. Error: MAR missing (@test_missing.r#59) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# simmer

Version: 4.1.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    0 indirect objects
    No pages of output.
    Transcript written on pdfetex.log.
    fmtutil [INFO]: /Users/romain/Library/TinyTeX/texmf-var/web2c/pdftex/pdfetex.fmt installed.
    fmtutil [INFO]: Disabled formats: 1
    fmtutil [INFO]: Successfully rebuilt formats: 14
    fmtutil [INFO]: Total formats: 15
    fmtutil [INFO]: exiting with status 0
    tlmgr install subfig
    
    tlmgr: Fundamental package texlive.infra not present, uh oh, goodbyeShould not happen, texlive.infra not found at /usr/local/bin/tlmgr line 7344.
    tlmgr: package repository http://mirrors.standaloneinstaller.com/ctan/systems/texlive/tlnet (not verified: gpg unavailable)
    tlmgr path add
    ! LaTeX Error: File `subfig.sty' not found.
    
    ! Emergency stop.
    <read *> 
    
    Error: processing vignette 'simmer-02-jss.Rmd' failed with diagnostics:
    Failed to compile simmer-02-jss.tex. See simmer-02-jss.log for more info.
    Execution halted
    ```

# simputation

Version: 0.2.2

## In both

*   checking whether package ‘simputation’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/simputation/new/simputation.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘simputation’ ...
** package ‘simputation’ successfully unpacked and MD5 sums checked
** libs
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include  -fopenmp -fPIC  -Wall -g -O2  -c R_register_native.c -o R_register_native.o
clang: error: unsupported option '-fopenmp'
make: *** [R_register_native.o] Error 1
ERROR: compilation failed for package ‘simputation’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/simputation/new/simputation.Rcheck/simputation’

```
### CRAN

```
* installing *source* package ‘simputation’ ...
** package ‘simputation’ successfully unpacked and MD5 sums checked
** libs
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include  -fopenmp -fPIC  -Wall -g -O2  -c R_register_native.c -o R_register_native.o
clang: error: unsupported option '-fopenmp'
make: *** [R_register_native.o] Error 1
ERROR: compilation failed for package ‘simputation’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/simputation/old/simputation.Rcheck/simputation’

```
# SimRVPedigree

Version: 0.3.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# simTool

Version: 1.1.0

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
             convert = convert) at /Users/romain/git/tidyverse/dplyr/R/all-equal.r:35
      
      [1] "Estimated replications per hour:  48087561"
      [1] "Estimated replications per hour:  18549747"
      [1] "Estimated replications per hour:  224529"
      [1] "Estimated replications per hour:  5194005"
      [1] "Estimated replications per hour:  610490"
      [1] "Estimated replications per hour:  4290726"
      [1] "Estimated replications per hour:  672784"
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 56 SKIPPED: 0 FAILED: 1
      1. Error: Two groups for summary_fun. Results were created and stored in simulation (@test_eval_tibbles.R#665) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# SingleCaseES

Version: 0.4.0

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
             scale = .data[[scale]], intervals = .data[[intervals]], observation_length = .data[[observation_length]], 
             confidence = confidence, format = format, ..., warn = warn)) at /Users/romain/git/tidyverse/dplyr/R/do.r:91
      14: eval_tidy(args[[j]], mask) at /Users/romain/git/tidyverse/dplyr/R/grouped-df.r:331
      15: calc_ES(condition = .data[["Phase"]], outcome = .data[["outcome"]], baseline_phase = baseline_phase, 
             ES = ES_names, improvement = .data[["smd_improvement"]], scale = .data[["scale"]], 
             intervals = .data[["intervals"]], observation_length = .data[["Session_length"]], 
             confidence = confidence, format = format, ..., warn = warn)
      16: stop("The 'condition' variable must have exactly two unique values.") at /Users/romain/git/release/dplyr/revdep/checks.noindex/SingleCaseES/new/SingleCaseES.Rcheck/00_pkg_src/SingleCaseES/R/calculate-effect-sizes.R:77
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 113 SKIPPED: 0 FAILED: 1
      1. Error: (unknown) (@test-Pustejovsky-2015.R#17) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# sjPlot

Version: 2.6.2

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘sjPlot-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_gpt
    > ### Title: Plot grouped proportional tables
    > ### Aliases: plot_gpt
    > 
    > ### ** Examples
    > 
    > data(efc)
    > 
    > # the proportion of dependency levels in female
    > # elderly, for each family carer's relationship
    > # to elderly
    > plot_gpt(efc, e42dep, e16sex, e15relat)
    Error in n() : could not find function "n"
    Calls: plot_gpt ... <Anonymous> -> <Anonymous> -> summarise.tbl_df -> summarise_impl
    Execution halted
    ```

# sjstats

Version: 0.17.2

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘arm’
    ```

# skimr

Version: 1.0.3

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    53      Species  factor    ordered       .all   0.0000000     FALSE
    > skim(iris) %>% dplyr::filter(type == "factor")
    Skim summary statistics
     n obs: 150 
     n variables: 5 
    
    ── Variable type:factor ────────────────────────────────────────────────────────
     variable missing complete   n n_unique                       top_counts
      Species       0      150 150        3 set: 50, ver: 50, vir: 50, NA: 0
     ordered
       FALSE
    > 
    > # Which column as the greatest mean value?
    > skim(iris) %>%
    +   dplyr::filter(stat == "mean") %>%
    +   dplyr::arrange(dplyr::desc(value))
    Skim summary statistics
     n obs: 150 
     n variables: 5 
    Error: Columns `missing`, `complete`, `n`, `sd`, `p0`, `p25`, `p50`, `p75`, `p100`, `hist` not found
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 61-64 (Using_skimr.Rmd) 
    Error: processing vignette 'Using_skimr.Rmd' failed with diagnostics:
    Columns `missing`, `complete`, `n`, `sd`, `p0`, `p25`, `p50`, `p75`, `p100`, `hist` not found
    Execution halted
    ```

# skynet

Version: 1.3.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘maps’
      All declared Imports should be used.
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

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    convert bootstrapped spaghettis to long format
    Computing density estimates for each vertical cut ...
    vertical cross-sectional density estimate
    Tile approach
    Build ggplot figure ...
    Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
      Running 'texi2dvi' on 'sorvi.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `float.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    
    ! Emergency stop.
    <read *> 
             
    l.15 ^^M
            
    !  ==> Fatal error occurred, no output PDF file produced!
    Calls: buildVignettes -> texi2pdf -> texi2dvi
    Execution halted
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/sorvi/new/sorvi.Rcheck/00_pkg_src/sorvi/R/regression_plot.R:115)
    regression_plot : <anonymous>: no visible global function definition
      for ‘pnorm’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/sorvi/new/sorvi.Rcheck/00_pkg_src/sorvi/R/regression_plot.R:115)
    regression_plot: no visible global function definition for
      ‘flush.console’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/sorvi/new/sorvi.Rcheck/00_pkg_src/sorvi/R/regression_plot.R:138)
    regression_plot: no visible global function definition for ‘density’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/sorvi/new/sorvi.Rcheck/00_pkg_src/sorvi/R/regression_plot.R:147)
    regression_plot: no visible global function definition for
      ‘flush.console’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/sorvi/new/sorvi.Rcheck/00_pkg_src/sorvi/R/regression_plot.R:194)
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

Version: 0.2.3

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
    Attaching package: 'RandomFields'
    
    The following object is masked from 'package:RandomFieldsUtils':
    
        RFoptions
    
    The following objects are masked from 'package:base':
    
        abs, acosh, asin, asinh, atan, atan2, atanh, cos, cosh, exp,
        expm1, floor, gamma, lgamma, log, log1p, log2, logb, max, min,
        round, sin, sinh, sqrt, tan, tanh, trunc
    
    Quitting from lines 477-498 (ii-modules.Rmd) 
    Error: processing vignette 'ii-modules.Rmd' failed with diagnostics:
    GLPK not found on this system.
    igraph is used internally and requires a GLPK installation.
    It can be installed using, e.g., `brew install glpk`, after which you should reinstall igraph from source using:
    `install.packages('igraph', type = 'source')`
    For more info see https://github.com/igraph/rigraph/issues/273.
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        doc   1.4Mb
        R     2.9Mb
    ```

# sparklyr

Version: 0.9.3

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.3Mb
      sub-directories of 1Mb or more:
        java   1.9Mb
        R      3.3Mb
    ```

# sparseHessianFD

Version: 0.3.3.3

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

# sparseMVN

Version: 0.2.1.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
      Running 'texi2dvi' on 'sparseMVN.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `placeins.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    
    ! Emergency stop.
    <read *> 
             
    l.30 \usepackage
                    {array}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    Calls: buildVignettes -> texi2pdf -> texi2dvi
    Execution halted
    ```

# SpatialBall

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘lubridate’
      All declared Imports should be used.
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
      ‘dplyr’ ‘dygraphs’ ‘ggplot2’ ‘htmlwidgets’ ‘knitr’ ‘leaflet’
      ‘mapproj’ ‘maptools’ ‘RColorBrewer’ ‘rgdal’ ‘rgeos’ ‘rmarkdown’
      ‘shinyjs’ ‘SpatialEpi’ ‘spdep’ ‘xts’
      All declared Imports should be used.
    ```

# spbabel

Version: 0.4.8

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:raster':
    
        intersect, select, union
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Checking rgeos availability: TRUE
    Quitting from lines 131-153 (spbabel.Rmd) 
    Error: processing vignette 'spbabel.Rmd' failed with diagnostics:
    c("assignment of an object of class \"vertex_table\" is not valid for @'data' in an object of class \"SpatialPolygonsDataFrame\"; is(value, \"data.frame\") is not TRUE", "assignment of an object of class \"tbl_df\" is not valid for @'data' in an object of class \"SpatialPolygonsDataFrame\"; is(value, \"data.frame\") is not TRUE", "assignment of an object of class \"tbl\" is not valid for @'data' in an object of class \"SpatialPolygonsDataFrame\"; is(value, \"data.frame\") is not TRUE", "assignment of an object of class \"data.frame\" is not valid for @'data' in an object of class \"SpatialPolygonsDataFrame\"; is(value, \"data.frame\") is not TRUE"
    )
    Execution halted
    ```

# spdplyr

Version: 0.1.3

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
             inds = unlist(groups) + 1) at /Users/romain/git/release/dplyr/revdep/checks.noindex/spdplyr/new/spdplyr.Rcheck/00_pkg_src/spdplyr/R/sdplyr-Spatial.r:172
      20: as_tibble(lst_quos(xs, expand = TRUE))
      21: as_tibble.list(lst_quos(xs, expand = TRUE))
      22: list_to_tibble(x, validate)
      23: check_tibble(x)
      24: invalid_df("must be [a] 1d atomic vector(s) or [a] list(s)", x, !is_1d)
      25: stopc(pluralise_msg("Column(s) ", vars), " ", pluralise(problem, vars))
      26: abort(paste0(...))
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 43 SKIPPED: 5 FAILED: 1
      1. Error: group by and summarize is quiet (@test-adv-dplyr.R#14) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# sport

Version: 0.1.1

## In both

*   checking PDF version of manual ... WARNING
    ```
    LaTeX errors when creating PDF version.
    This typically indicates Rd problems.
    LaTeX errors found:
    ! LaTeX Error: Command \k unavailable in encoding OT1.
    
    See the LaTeX manual or LaTeX Companion for explanation.
    Type  H <return>  for immediate help.
     ...                                              
    ! LaTeX Error: Command \k unavailable in encoding OT1.
    
    See the LaTeX manual or LaTeX Companion for explanation.
    Type  H <return>  for immediate help.
     ...                                              
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 6863 marked UTF-8 strings
    ```

# srvyr

Version: 0.3.3

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘srvyr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: as_survey_twophase
    > ### Title: Create a tbl_svy survey object using two phase design
    > ### Aliases: as_survey_twophase as_survey_twophase.data.frame
    > ###   as_survey_twophase.twophase2
    > 
    > ### ** Examples
    > 
    > # Examples from ?survey::twophase
    > # two-phase simple random sampling.
    > data(pbc, package="survival")
    > 
    > pbc <- pbc %>%
    +   mutate(randomized = !is.na(trt) & trt > 0,
    +          id = row_number())
    Error in row_number() : could not find function "row_number"
    Calls: %>% ... as.data.frame -> mutate -> mutate.tbl_df -> mutate_impl
    Execution halted
    ```

# stacomiR

Version: 0.5.4.0

## In both

*   checking package dependencies ... ERROR
    ```
    Packages required but not available: ‘gWidgetsRGtk2’ ‘RGtk2’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# staRdom

Version: 1.0.8

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘readr’ ‘tools’
      All declared Imports should be used.
    ```

# stars

Version: 0.2-0

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘starsdata’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 10.8Mb
      sub-directories of 1Mb or more:
        doc   6.4Mb
        nc    2.9Mb
    ```

# states

Version: 0.2.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘lubridate’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 3 marked UTF-8 strings
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

# statsDK

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘ggplot2’ ‘stringr’
      All declared Imports should be used.
    ```

# stminsights

Version: 0.3.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘huge’ ‘readr’ ‘scales’ ‘shinyjs’
      All declared Imports should be used.
    ```

# stplanr

Version: 0.2.6

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘stplanr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: angle_diff
    > ### Title: Calculate the angular difference between lines and a predefined
    > ###   bearing
    > ### Aliases: angle_diff
    > 
    > ### ** Examples
    > 
    > data(flowlines)
    > # Find all routes going North-South
    > a <- angle_diff(flowlines, angle = 0, bidirectional = TRUE, absolute = TRUE)
    Error in first(x) : could not find function "first"
    Calls: angle_diff ... <Anonymous> -> <Anonymous> -> summarise_.tbl_df -> summarise_impl
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      3: `rownames<-`(`*tmp*`, value = as.character(unlist(lapply(targetintersectd@polygons, 
             function(x) {
                 x@ID
             })))) at /Users/romain/git/release/dplyr/revdep/checks.noindex/stplanr/new/stplanr.Rcheck/00_pkg_src/stplanr/R/catchmentArea.R:162
      4: `row.names<-`(`*tmp*`, value = value)
      5: `row.names<-.data.frame`(`*tmp*`, value = value)
      6: `.rowNamesDF<-`(x, value = value)
      7: stop("invalid 'row.names' length")
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 3 SKIPPED: 1 FAILED: 1
      1. Error: calc_catchment returns a SpatialPolygonsDataFrame (@test-calc_catchment.R#11) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Parsed with column specification:
    cols(
      Function = col_character(),
      `Input data type(s)` = col_character(),
      `Output data type` = col_character()
    )
    Transforming to CRS +proj=aeqd +lat_0=53.81840897 +lon_0=-1.52833461 +x_0=0 +y_0=0 +ellps=WGS84
    Quitting from lines 468-476 (stplanr-paper.Rmd) 
    Error: processing vignette 'stplanr-paper.Rmd' failed with diagnostics:
    invalid 'row.names' length
    Execution halted
    ```

# STRMPS

Version: 0.5.8

## In both

*   checking whether package ‘STRMPS’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘IRanges’ was built under R version 3.5.1
      Warning: package ‘Rsamtools’ was built under R version 3.5.1
      Warning: package ‘GenomicRanges’ was built under R version 3.5.1
      Warning: package ‘DelayedArray’ was built under R version 3.5.1
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/STRMPS/new/STRMPS.Rcheck/00install.out’ for details.
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘STRaitRazoR’
    ```

# SubgrPlots

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘alluvial’ ‘geoR’ ‘gridBase’ ‘UpSetR’
      All declared Imports should be used.
    ```

# subSeq

Version: 1.10.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/subSeq/new/subSeq.Rcheck/00_pkg_src/subSeq/R/summary.subsamples.R:127-129)
    summary.subsamples: no visible binding for global variable ‘percent’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/subSeq/new/subSeq.Rcheck/00_pkg_src/subSeq/R/summary.subsamples.R:127-129)
    summary.subsamples: no visible binding for global variable ‘proportion’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/subSeq/new/subSeq.Rcheck/00_pkg_src/subSeq/R/summary.subsamples.R:127-129)
    summary.subsamples: no visible binding for global variable ‘method’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/subSeq/new/subSeq.Rcheck/00_pkg_src/subSeq/R/summary.subsamples.R:127-129)
    voomLimma: no visible global function definition for ‘model.matrix’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/subSeq/new/subSeq.Rcheck/00_pkg_src/subSeq/R/handlers.R:41)
    Undefined global functions or variables:
      . average.depth average.value coefficient cor count cov depth estFDP
      ID method metric model.matrix o.coefficient o.lfdr o.padj p.adjust
      padj percent plot proportion pvalue rbinom replication rFDP
      selectMethod significant valid value var
    Consider adding
      importFrom("graphics", "plot")
      importFrom("methods", "selectMethod")
      importFrom("stats", "cor", "cov", "model.matrix", "p.adjust", "rbinom",
                 "var")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

# sugrrants

Version: 0.2.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘gtable’
      All declared Imports should be used.
    ```

# SummarizedBenchmark

Version: 1.0.4

## In both

*   checking whether package ‘SummarizedBenchmark’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘GenomicRanges’ was built under R version 3.5.1
      Warning: package ‘IRanges’ was built under R version 3.5.1
      Warning: package ‘DelayedArray’ was built under R version 3.5.1
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/SummarizedBenchmark/new/SummarizedBenchmark.Rcheck/00install.out’ for details.
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
    The following object is masked from 'package:SummarizedBenchmark':
    
        plotROC
    
    
    Attaching package: 'magrittr'
    
    The following object is masked from 'package:rlang':
    
        set_names
    
    The following object is masked from 'package:tidyr':
    
        extract
    
    Loading required package: SingleCellExperiment
    Quitting from lines 47-54 (SingleCellBenchmark.Rmd) 
    Error: processing vignette 'SingleCellBenchmark.Rmd' failed with diagnostics:
    there is no package called 'scRNAseq'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘scRNAseq’
    
    Depends: includes the non-default packages:
      ‘tidyr’ ‘SummarizedExperiment’ ‘S4Vectors’ ‘BiocGenerics’ ‘UpSetR’
      ‘rlang’ ‘stringr’ ‘BiocParallel’ ‘ggplot2’ ‘mclust’ ‘dplyr’
    Adding so many packages to the search path is excessive and importing
    selectively is preferable.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 12.7Mb
      sub-directories of 1Mb or more:
        data   8.9Mb
        doc    3.3Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unexported object imported by a ':::' call: ‘BiocGenerics:::replaceSlots’
      See the note in ?`:::` about the use of this operator.
    ```

*   checking R code for possible problems ... NOTE
    ```
    .list2mat : <anonymous>: no visible binding for global variable
      ‘.method’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/SummarizedBenchmark/new/SummarizedBenchmark.Rcheck/00_pkg_src/SummarizedBenchmark/R/buildBench.R:275)
    .list2mat : <anonymous>: no visible binding for global variable ‘.val’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/SummarizedBenchmark/new/SummarizedBenchmark.Rcheck/00_pkg_src/SummarizedBenchmark/R/buildBench.R:275)
    .list2mat : <anonymous>: no visible binding for global variable ‘.id’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/SummarizedBenchmark/new/SummarizedBenchmark.Rcheck/00_pkg_src/SummarizedBenchmark/R/buildBench.R:276-277)
    plotROC: no visible binding for global variable ‘FDR’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/SummarizedBenchmark/new/SummarizedBenchmark.Rcheck/00_pkg_src/SummarizedBenchmark/R/PlottingFunctions.R:81-82)
    plotROC: no visible binding for global variable ‘TPR’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/SummarizedBenchmark/new/SummarizedBenchmark.Rcheck/00_pkg_src/SummarizedBenchmark/R/PlottingFunctions.R:81-82)
    plotROC: no visible binding for global variable ‘method’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/SummarizedBenchmark/new/SummarizedBenchmark.Rcheck/00_pkg_src/SummarizedBenchmark/R/PlottingFunctions.R:81-82)
    Undefined global functions or variables:
      .id .method .val FDR method TPR
    ```

# sunburstR

Version: 2.1.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘treemap’
    ```

# suropt

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘DiceOptim’ ‘GPareto’ ‘rgenoud’ ‘tidyr’
      All declared Imports should be used.
    ```

# surveydata

Version: 0.2.2

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      The following object is masked from 'package:stats':
      
          filter
      
      > 
      > test_check("surveydata")
      ── 1. Failure: dplyr verbs retain surveydata class (@test-09-dplyr-verbs.R#14)  
      membersurvey %>% as.tbl() %>% summarize(n = n()) inherits from `tbl_df/tbl/data.frame` not `surveydata`.
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 213 SKIPPED: 1 FAILED: 1
      1. Failure: dplyr verbs retain surveydata class (@test-09-dplyr-verbs.R#14) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# survminer

Version: 0.4.3

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.1Mb
      sub-directories of 1Mb or more:
        doc   5.1Mb
    ```

# survsup

Version: 0.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘gridExtra’ ‘stats’ ‘survival’ ‘utils’
      All declared Imports should be used.
    ```

# SVMMaj

Version: 0.2-8

## In both

*   checking re-building of vignette outputs ... NOTE
    ```
    ...
    
    The following object is masked from ‘package:ggplot2’:
    
        alpha
    
    Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
      Running 'texi2dvi' on 'paper.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `relsize.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    
    ! Emergency stop.
    <read *> 
             
    l.987 \ifthenelse
                     {\boolean{algocf@slide}}{\RequirePackage{color}}{}%^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    Calls: buildVignettes -> texi2pdf -> texi2dvi
    Execution halted
    ```

# sweep

Version: 0.2.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘devtools’ ‘lazyeval’ ‘lubridate’ ‘tidyr’
      All declared Imports should be used.
    ```

# swfdr

Version: 1.6.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    0 indirect objects
    No pages of output.
    Transcript written on pdfetex.log.
    fmtutil [INFO]: /Users/romain/Library/TinyTeX/texmf-var/web2c/pdftex/pdfetex.fmt installed.
    fmtutil [INFO]: Disabled formats: 1
    fmtutil [INFO]: Successfully rebuilt formats: 14
    fmtutil [INFO]: Total formats: 15
    fmtutil [INFO]: exiting with status 0
    tlmgr install fancyhdr
    
    tlmgr: Fundamental package texlive.infra not present, uh oh, goodbyeShould not happen, texlive.infra not found at /usr/local/bin/tlmgr line 7344.
    tlmgr: package repository http://mirrors.standaloneinstaller.com/ctan/systems/texlive/tlnet (not verified: gpg unavailable)
    tlmgr path add
    ! LaTeX Error: File `fancyhdr.sty' not found.
    
    ! Emergency stop.
    <read *> 
    
    Error: processing vignette 'swfdrTutorial.Rmd' failed with diagnostics:
    Failed to compile swfdrTutorial.tex. See swfdrTutorial.log for more info.
    Execution halted
    ```

*   checking R code for possible problems ... NOTE
    ```
    lm_pi0: no visible global function definition for ‘glm’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/swfdr/new/swfdr.Rcheck/00_pkg_src/swfdr/R/lm_pi0.R:56)
    lm_pi0: no visible binding for global variable ‘binomial’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/swfdr/new/swfdr.Rcheck/00_pkg_src/swfdr/R/lm_pi0.R:56)
    Undefined global functions or variables:
      binomial glm
    Consider adding
      importFrom("stats", "binomial", "glm")
    to your NAMESPACE file.
    ```

# switchde

Version: 1.6.0

## In both

*   checking whether package ‘switchde’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘GenomicRanges’ was built under R version 3.5.1
      Warning: package ‘IRanges’ was built under R version 3.5.1
      Warning: package ‘DelayedArray’ was built under R version 3.5.1
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/switchde/new/switchde.Rcheck/00install.out’ for details.
    ```

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .travis.yml
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

# SWMPrExtension

Version: 0.3.16

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘SWMPrExtension-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: threshold_summary
    > ### Title: Summary Plots for Threshold Identification
    > ### Aliases: threshold_summary threshold_summary.swmpr
    > 
    > ### ** Examples
    > 
    > ## Water quality examples
    > dat_wq <- qaqc(apacpwq, qaqc_keep = c(0, 3, 5))
    > dat_wq <- setstep(dat_wq)
    > 
    > x <-
    +   threshold_summary(dat_wq, param = 'do_mgl', parameter_threshold = 2
    +   , threshold_type = '<', time_threshold = 2, summary_type = 'month'
    +   , plot_title = TRUE)
    Joining, by = c("grp_join", "year")
    Error: `breaks` and `labels` must have the same length
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘rgeos’
      All declared Imports should be used.
    ```

# synlet

Version: 1.10.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Error: processing vignette 'synlet-vignette.Rmd' failed with diagnostics:
    there is no package called ‘BiocStyle’
    Execution halted
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    zFactor: no visible binding for global variable ‘sd’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/synlet/new/synlet.Rcheck/00_pkg_src/synlet/R/zFactor.R:37-38)
    zFactor: no visible binding for global variable ‘median’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/synlet/new/synlet.Rcheck/00_pkg_src/synlet/R/zFactor.R:37-38)
    zFactor: no visible global function definition for ‘complete.cases’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/synlet/new/synlet.Rcheck/00_pkg_src/synlet/R/zFactor.R:50)
    Undefined global functions or variables:
      COL_NAME colorRampPalette complete.cases condition dev.off
      EXPERIMENT_MODIFICATION EXPERIMENT_TYPE experiments is mad
      MASTER_PLATE median medpolish p.adjust pdf phyper PLATE rainbow
      READOUT ROW_NAME sd siRNA t.test value Var1 WELL_CONTENT_NAME
      write.table
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

# syuzhet

Version: 1.0.4

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        extdata   3.1Mb
        R         1.3Mb
    ```

# tabula

Version: 1.0.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘tidyr’
      All declared Imports should be used.
    ```

# tabularaster

Version: 0.5.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘methods’
      All declared Imports should be used.
    ```

# TAShiny

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘igraph’ ‘SnowballC’ ‘tm’ ‘wordcloud2’
      All declared Imports should be used.
    ```

# taxa

Version: 0.3.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘knitr’ ‘lazyeval’ ‘rlang’ ‘tidyr’
      All declared Imports should be used.
    ```

# tbl2xts

Version: 0.1.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘PerformanceAnalytics’
      All declared Imports should be used.
    ```

# tbrf

Version: 0.1.0

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Expected match: "Evaluation error: unit must be one of years, months, weeks, days, hours, minutes, seconds."
      Actual message: "unit must be one of years, months, weeks, days, hours, minutes, seconds"
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 25 SKIPPED: 0 FAILED: 7
      1. Failure: tbr_binom returns error (@test-expectedMessages.R#10) 
      2. Failure: tbr_gmean returns error (@test-expectedMessages.R#26) 
      3. Failure: tbr_mean returns error (@test-expectedMessages.R#35) 
      4. Failure: tbr_median returns error (@test-expectedMessages.R#44) 
      5. Failure: tbr_sd returns error (@test-expectedMessages.R#53) 
      6. Failure: tbr_sum returns error (@test-expectedMessages.R#62) 
      7. Failure: tbr_misc returns error (@test-expectedMessages.R#72) 
      
      Error: testthat unit tests failed
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

*   checking for missing documentation entries ... WARNING
    ```
    Warning: package ‘maftools’ was built under R version 3.5.1
    All user-level objects in a package should have documentation entries.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 72.7Mb
      sub-directories of 1Mb or more:
        data   3.5Mb
        doc   66.4Mb
        R      2.5Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    TCGAtumor_purity: no visible binding for global variable ‘Tumor.purity’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/TCGAbiolinks/new/TCGAbiolinks.Rcheck/00_pkg_src/TCGAbiolinks/R/clinical.R:639-640)
    TCGAvisualize_oncoprint: no visible binding for global variable ‘value’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/TCGAbiolinks/new/TCGAbiolinks.Rcheck/00_pkg_src/TCGAbiolinks/R/visualize.R:944)
    TCGAvisualize_SurvivalCoxNET: no visible global function definition for
      ‘dNetInduce’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/TCGAbiolinks/new/TCGAbiolinks.Rcheck/00_pkg_src/TCGAbiolinks/R/visualize.R:156-157)
    TCGAvisualize_SurvivalCoxNET: no visible global function definition for
      ‘dNetPipeline’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/TCGAbiolinks/new/TCGAbiolinks.Rcheck/00_pkg_src/TCGAbiolinks/R/visualize.R:161-162)
    TCGAvisualize_SurvivalCoxNET: no visible global function definition for
      ‘dCommSignif’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/TCGAbiolinks/new/TCGAbiolinks.Rcheck/00_pkg_src/TCGAbiolinks/R/visualize.R:174)
    TCGAvisualize_SurvivalCoxNET: no visible global function definition for
      ‘visNet’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/TCGAbiolinks/new/TCGAbiolinks.Rcheck/00_pkg_src/TCGAbiolinks/R/visualize.R:184-189)
    Undefined global functions or variables:
      barcode c3net clinical coordinates dCommSignif dNetInduce
      dNetPipeline exon knnmi.cross limmacontrasts.fit limmamakeContrasts
      minet portions rse_gene TabSubtypesCol_merged Tumor.purity value
      visNet
    ```

# TCGAbiolinksGUI

Version: 1.6.1

## In both

*   checking package dependencies ... ERROR
    ```
    Packages required but not available:
      ‘IlluminaHumanMethylation450kanno.ilmn12.hg19’
      ‘IlluminaHumanMethylation450kmanifest’
      ‘IlluminaHumanMethylation27kmanifest’
      ‘IlluminaHumanMethylation27kanno.ilmn12.hg19’
      ‘IlluminaHumanMethylationEPICanno.ilm10b2.hg19’
      ‘IlluminaHumanMethylationEPICmanifest’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# TCGAbiolinksGUI.data

Version: 1.0.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 19.7Mb
      sub-directories of 1Mb or more:
        data  18.6Mb
        doc    1.0Mb
    ```

# tcR

Version: 2.2.2

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.8Mb
      sub-directories of 1Mb or more:
        data   1.4Mb
        doc    3.9Mb
        R      1.1Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘scales’
      All declared Imports should be used.
    ```

# tempcyclesdata

Version: 1.0.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.9Mb
      sub-directories of 1Mb or more:
        data   5.9Mb
    ```

# textfeatures

Version: 0.3.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘utils’
      All declared Imports should be used.
    ```

# textmining

Version: 0.0.1

## In both

*   checking whether package ‘textmining’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/textmining/new/textmining.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘textmining’ ...
** package ‘textmining’ successfully unpacked and MD5 sums checked
** R
** byte-compile and prepare package for lazy loading
Warning in fun(libname, pkgname) : couldn't connect to display ""
Error : .onLoad failed in loadNamespace() for 'mallet', details:
  call: NULL
  error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/romain/git/release/dplyr/revdep/library.noindex/textmining/rJava/libs/rJava.so':
  dlopen(/Users/romain/git/release/dplyr/revdep/library.noindex/textmining/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/romain/git/release/dplyr/revdep/library.noindex/textmining/rJava/libs/rJava.so
  Reason: image not found
ERROR: lazy loading failed for package ‘textmining’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/textmining/new/textmining.Rcheck/textmining’

```
### CRAN

```
* installing *source* package ‘textmining’ ...
** package ‘textmining’ successfully unpacked and MD5 sums checked
** R
** byte-compile and prepare package for lazy loading
Warning in fun(libname, pkgname) : couldn't connect to display ""
Error : .onLoad failed in loadNamespace() for 'mallet', details:
  call: NULL
  error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/romain/git/release/dplyr/revdep/library.noindex/textmining/rJava/libs/rJava.so':
  dlopen(/Users/romain/git/release/dplyr/revdep/library.noindex/textmining/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/romain/git/release/dplyr/revdep/library.noindex/textmining/rJava/libs/rJava.so
  Reason: image not found
ERROR: lazy loading failed for package ‘textmining’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/textmining/old/textmining.Rcheck/textmining’

```
# textrecipes

Version: 0.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘stringr’
      All declared Imports should be used.
    ```

# textreuse

Version: 0.1.4

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘tm’
    ```

# TFEA.ChIP

Version: 1.0.0

## In both

*   checking package dependencies ... ERROR
    ```
    Packages required but not available:
      ‘TxDb.Hsapiens.UCSC.hg19.knownGene’ ‘org.Hs.eg.db’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# TFutils

Version: 1.0.0

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘TFutils-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: genemodelDF
    > ### Title: use EnsDb to generate an exon-level model of genes identified by
    > ###   symbol
    > ### Aliases: genemodelDF
    > 
    > ### ** Examples
    > 
    > if (requireNamespace("EnsDb.Hsapiens.v75")) {
    +  orm = genemodelDF("ORMDL3", EnsDb.Hsapiens.v75::EnsDb.Hsapiens.v75)
    +  dim(orm)
    + }
    Loading required namespace: EnsDb.Hsapiens.v75
    Failed with error:  'there is no package called 'EnsDb.Hsapiens.v75''
    > head(orm)
    Error in head(orm) : object 'orm' not found
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      4: getExportedValue(pkg, name)
      5: asNamespace(ns)
      6: getNamespace(ns)
      7: tryCatch(loadNamespace(name), error = function(e) stop(e))
      8: tryCatchList(expr, classes, parentenv, handlers)
      9: tryCatchOne(expr, names, parentenv, handlers[[1L]])
      10: value[[3L]](cond)
      
      Failed with error:  'there is no package called 'EnsDb.Hsapiens.v75''
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 3 SKIPPED: 0 FAILED: 1
      1. Error: grabTab returns expected records (@test.R#6) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 26-40 (TFutils.Rmd) 
    Error: processing vignette 'TFutils.Rmd' failed with diagnostics:
    there is no package called 'org.Hs.eg.db'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      ‘Homo.sapiens’ ‘GO.db’ ‘org.Hs.eg.db’ ‘EnsDb.Hsapiens.v75’
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 62 marked UTF-8 strings
    ```

# TH.data

Version: 1.0-9

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.5Mb
      sub-directories of 1Mb or more:
        data   1.1Mb
        rda    7.0Mb
    ```

# theseus

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘gridExtra’ ‘splancs’ ‘tidyverse’
      All declared Imports should be used.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘DESeq2’, ‘dada2’
    ```

# tibbletime

Version: 0.1.1

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
          filter
      
      > 
      > test_check("tibbletime")
      ── 1. Failure: Converting to more granular throws error (@test_as_period.R#16)  
      `as_period(test_tbl_time, "hourly")` threw an error with unexpected message.
      Expected match: "Only year, quarter, month, week, and day periods are allowed for an index of class Date."
      Actual message: "Only year, quarter, month, week, and day periods are allowed for an index of class Date"
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 128 SKIPPED: 0 FAILED: 1
      1. Failure: Converting to more granular throws error (@test_as_period.R#16) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# tidybayes

Version: 1.0.3

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
             mutate = mutate)
      10: setup.jags(model = outmodel, monitor = outmonitor, data = outdata, n.chains = n.chains, 
             inits = outinits, modules = modules, factories = factories, response = response, 
             fitted = fitted, residual = residual, jags = jags, method = method, mutate = mutate)
      11: loadandcheckrjags()
      12: stop("Loading the rjags package failed (diagnostics are given above this error message)", 
             call. = FALSE)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 262 SKIPPED: 2 FAILED: 2
      1. Failure: groups from spread_draws retain factor level names (@test.spread_draws.R#251) 
      2. Error: tidy_draws works with runjags (@test.tidy_draws.R#87) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# tidygraph

Version: 1.1.1

## Newly broken

*   checking S3 generic/method consistency ... WARNING
    ```
    ...
      function(tbl, size, replace, weight, .env, ...)
    sample_frac.morphed_tbl_graph:
      function(tbl, size, replace, weight, .env)
    
    sample_frac:
      function(tbl, size, replace, weight, .env, ...)
    sample_frac.tbl_graph:
      function(tbl, size, replace, weight, .env)
    
    sample_n:
      function(tbl, size, replace, weight, .env, ...)
    sample_n.morphed_tbl_graph:
      function(tbl, size, replace, weight, .env)
    
    sample_n:
      function(tbl, size, replace, weight, .env, ...)
    sample_n.tbl_graph:
      function(tbl, size, replace, weight, .env)
    
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

# tidymodels

Version: 0.0.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘broom’ ‘dials’ ‘parsnip’
      All declared Imports should be used.
    ```

# tidyquant

Version: 0.5.5

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        doc   4.1Mb
    ```

# tidyr

Version: 0.8.2

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 23 marked UTF-8 strings
    ```

# tidyRSS

Version: 1.2.7

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘testthat’
      All declared Imports should be used.
    ```

# tidystats

Version: 0.2

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Running examples in ‘tidystats-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: tidy_stats.anova
    > ### Title: Create a tidy stats data frame from an anova object
    > ### Aliases: tidy_stats.anova
    > 
    > ### ** Examples
    > 
    > # Regression example
    > ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
    > trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
    > group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
    > weight <- c(ctl, trt)
    > 
    > model_0 <- lm(weight ~ 1)
    > model_1 <- lm(weight ~ group)
    > 
    > tidy_stats(anova(model_0))
    Error: Can't create call to non-callable object
    Execution halted
    ```

# tidytransit

Version: 0.3.5

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘tidytransit-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: get_route_frequency
    > ### Title: Get Route Frequency
    > ### Aliases: get_route_frequency
    > 
    > ### ** Examples
    > 
    > data(gtfs_obj)
    > gtfs_obj <- get_route_frequency(gtfs_obj)
    Calculating route and stop headways using defaults (6 am to 10 pm for weekday service).
    Error in n() : could not find function "n"
    Calls: get_route_frequency ... <Anonymous> -> <Anonymous> -> mutate.tbl_df -> mutate_impl
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      16: `_fseq`(`_lhs`)
      17: freduce(value, `_function_list`)
      18: function_list[[i]](value)
      19: dplyr::mutate(., service_trips = n())
      20: mutate.tbl_df(., service_trips = n()) at /Users/romain/git/tidyverse/dplyr/R/manip.r:416
      21: mutate_impl(.data, dots) at /Users/romain/git/tidyverse/dplyr/R/tbl-df.r:91
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 3 SKIPPED: 7 FAILED: 3
      1. Error: Stop frequencies (headways) for included data are as expected (@test_headways.R#4) 
      2. Error: Route frequencies (headways) for included data are as expected (@test_headways.R#11) 
      3. Error: Route frequencies (headways) can be calculated for included data for a particular service id (@test_headways.R#17) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        extdata   4.4Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘here’ ‘htmltools’ ‘scales’ ‘stringr’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 41 marked UTF-8 strings
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

# tidyxl

Version: 1.0.3

## In both

*   checking compiled code ... WARNING
    ```
    File ‘tidyxl/libs/tidyxl.so’:
      Found ‘_abort’, possibly from ‘abort’ (C)
        Object: ‘xlex.o’
    
    Compiled code should not call entry points which might terminate R nor
    write to stdout/stderr instead of to the console, nor use Fortran I/O
    nor system RNGs.
    
    See ‘Writing portable packages’ in the ‘Writing R Extensions’ manual.
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

# timelineS

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘base’
      All declared Imports should be used.
    ```

# TimerQuant

Version: 1.10.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
      no non-missing arguments to min; returning Inf
    Warning in min(x, na.rm = TRUE) :
      no non-missing arguments to min; returning Inf
    Warning in min(x, na.rm = TRUE) :
      no non-missing arguments to min; returning Inf
    Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
      Running 'texi2dvi' on 'genPaperFigures.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `float.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    
    ! Emergency stop.
    <read *> 
             
    l.30 \date
              {}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    Calls: buildVignettes -> texi2pdf -> texi2dvi
    Execution halted
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    plotPrimordiumProfile: no visible global function definition for
      ‘points’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/TimerQuant/new/TimerQuant.Rcheck/00_pkg_src/TimerQuant/R/plotPrimordiumProfile.R:24)
    plotPrimordiumProfile: no visible global function definition for
      ‘polygon’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/TimerQuant/new/TimerQuant.Rcheck/00_pkg_src/TimerQuant/R/plotPrimordiumProfile.R:26-27)
    plotPrimordiumProfile: no visible global function definition for ‘rgb’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/TimerQuant/new/TimerQuant.Rcheck/00_pkg_src/TimerQuant/R/plotPrimordiumProfile.R:26-27)
    simulatedRatio: no visible global function definition for ‘rnorm’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/TimerQuant/new/TimerQuant.Rcheck/00_pkg_src/TimerQuant/R/SAPSstochastic.R:4)
    simulatedRatio: no visible global function definition for ‘rnorm’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/TimerQuant/new/TimerQuant.Rcheck/00_pkg_src/TimerQuant/R/SAPSstochastic.R:5)
    Undefined global functions or variables:
      approxfun axis mad median optimize par plot points polygon predict
      rainbow rgb rnorm
    Consider adding
      importFrom("graphics", "axis", "par", "plot", "points", "polygon")
      importFrom("grDevices", "rainbow", "rgb")
      importFrom("stats", "approxfun", "mad", "median", "optimize",
                 "predict", "rnorm")
    to your NAMESPACE file.
    ```

# timescape

Version: 1.4.0

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
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/timescape/new/timescape.Rcheck/00_pkg_src/timescape/R/timescape.R:653-657)
    Undefined global functions or variables:
      show_warnings
    ```

# timetk

Version: 0.1.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘devtools’ ‘forecast’
      All declared Imports should be used.
    ```

# TissueEnrich

Version: 1.0.7

## In both

*   checking whether package ‘TissueEnrich’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘GenomicRanges’ was built under R version 3.5.1
      Warning: package ‘IRanges’ was built under R version 3.5.1
      Warning: package ‘DelayedArray’ was built under R version 3.5.1
      Warning: package ‘graph’ was built under R version 3.5.1
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/TissueEnrich/new/TissueEnrich.Rcheck/00install.out’ for details.
    ```

# TitanCNA

Version: 1.18.0

## In both

*   checking whether package ‘TitanCNA’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘GenomicRanges::shift’ by ‘data.table::shift’ when loading ‘TitanCNA’
      Warning: replacing previous import ‘IRanges::collapse’ by ‘dplyr::collapse’ when loading ‘TitanCNA’
      Warning: replacing previous import ‘data.table::last’ by ‘dplyr::last’ when loading ‘TitanCNA’
      Warning: replacing previous import ‘GenomicRanges::union’ by ‘dplyr::union’ when loading ‘TitanCNA’
      Warning: replacing previous import ‘IRanges::slice’ by ‘dplyr::slice’ when loading ‘TitanCNA’
      Warning: replacing previous import ‘GenomeInfoDb::intersect’ by ‘dplyr::intersect’ when loading ‘TitanCNA’
      Warning: replacing previous import ‘GenomicRanges::setdiff’ by ‘dplyr::setdiff’ when loading ‘TitanCNA’
      Warning: replacing previous import ‘data.table::first’ by ‘dplyr::first’ when loading ‘TitanCNA’
      Warning: replacing previous import ‘IRanges::desc’ by ‘dplyr::desc’ when loading ‘TitanCNA’
      Warning: replacing previous import ‘data.table::between’ by ‘dplyr::between’ when loading ‘TitanCNA’
      Warning: replacing previous import ‘dplyr::select’ by ‘VariantAnnotation::select’ when loading ‘TitanCNA’
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/TitanCNA/new/TitanCNA.Rcheck/00install.out’ for details.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  8.0Mb
      sub-directories of 1Mb or more:
        data      1.7Mb
        extdata   4.9Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      filterByTargetedSequences haplotypeBin HaplotypeBinDepth.mean
      HaplotypeBinDepth.sum HaplotypeDepth.mean
      HaplotypeDepth.mean.symmetric HaplotypeDepth.sum
      HaplotypeDepth.sum.symmetric HaplotypeFraction
      HaplotypeFraction.symmetric HaplotypeRatio HaplotypeRatio.1
      HaplotypeRatio.2 head keepChr Length.snp. lines loess
      logR_Copy_Number LogRatio lowess MajorCN Median_logR Median_Ratio
      MinorCN mtext na.omit nonRef par phasedAlleleFraction phasedCount
      phasedCount.haploSymmetric phaseSet phaseSet.aggr plot points predict
      queryHits read.delim rowRanges rowRanges<- Sample seq.info SNPs Start
      Start_Position.bp. Start.snp Start.telo subjectHits tail TITAN_call
      TITANcall TITANstate tumDepth uniroot unstrsplit write.table xtabs
    Consider adding
      importFrom("graphics", "abline", "axis", "lines", "mtext", "par",
                 "plot", "points")
      importFrom("methods", "as")
      importFrom("stats", "approxfun", "dunif", "loess", "lowess", "na.omit",
                 "predict", "uniroot", "xtabs")
      importFrom("utils", "head", "read.delim", "tail", "write.table")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘HMMcopy’, ‘list’
    ```

# tmap

Version: 2.1-1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.6Mb
      sub-directories of 1Mb or more:
        data   1.4Mb
        doc    2.1Mb
        R      2.8Mb
    ```

# TPP

Version: 3.8.5

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Warning in mutate_impl(.data, dots) :
      binding character and factor vector, coercing into character vector
    Warning in mutate_impl(.data, dots) :
      binding character and factor vector, coercing into character vector
    Warning in mutate_impl(.data, dots) :
      binding character and factor vector, coercing into character vector
    Warning in mutate_impl(.data, dots) :
      binding character and factor vector, coercing into character vector
    Warning in mutate_impl(.data, dots) :
      binding character and factor vector, coercing into character vector
    Warning in mutate_impl(.data, dots) :
      binding character and factor vector, coercing into character vector
    Warning in mutate_impl(.data, dots) :
      binding character and factor vector, coercing into character vector
    Warning in mutate_impl(.data, dots) :
      binding character and factor vector, coercing into character vector
    Warning in mutate_impl(.data, dots) :
      binding character and factor vector, coercing into character vector
    Error in { : task 1 failed - "subscript out of bounds"
    Calls: analyzeTPPTR ... tpptrSplineFitAndTest -> tpptrFitSplines -> %dopar% -> <Anonymous>
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 21861 SKIPPED: 1 FAILED: 25
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

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    3. Computing normalization coefficients
    Creating QC plots to illustrate median curve fits.
    -----------------------------------
    Normalizing all proteins in all experiments.
    Normalization successfully completed!
    
    Fitting smoothing splines and AICc values for the following degrees of freedom: 3, 4, 5, 6, 7
    Fitting null models to 510 proteins (using 3 degrees of freedom)
    Fitting null models to 510 proteins (using 4 degrees of freedom)
    Fitting alternative models to 510 proteins (using 3 degrees of freedom)
    Fitting alternative models to 510 proteins (using 4 degrees of freedom)
    Fitting null models to 510 proteins (using 5 degrees of freedom)
    Fitting null models to 510 proteins (using 6 degrees of freedom)
    Fitting alternative models to 510 proteins (using 6 degrees of freedom)
    Fitting alternative models to 510 proteins (using 5 degrees of freedom)
    Fitting null models to 510 proteins (using 7 degrees of freedom)
    Fitting alternative models to 510 proteins (using 7 degrees of freedom)
    Quitting from lines 73-76 (NPARC_analysis_of_TPP_TR_data.Rnw) 
    Error: processing vignette 'NPARC_analysis_of_TPP_TR_data.Rnw' failed with diagnostics:
    task 1 failed - "subscript out of bounds"
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 14.7Mb
      sub-directories of 1Mb or more:
        data           1.9Mb
        example_data   8.0Mb
        R              1.6Mb
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
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/TPP/new/TPP.Rcheck/00_pkg_src/TPP/R/plot_fSta_distribution.R:19-28)
    plot_pVal_distribution: no visible binding for global variable
      ‘..density..’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/TPP/new/TPP.Rcheck/00_pkg_src/TPP/R/plot_pVal_distribution.R:22-31)
    Undefined global functions or variables:
      ..density..
    ```

# trackr

Version: 0.10.5

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Error in re-building vignettes:
      ...
    Loading required package: histry
    Error in get(invar, envir = .GlobalEnv) : object 'score' not found
    Error in get(invar, envir = .GlobalEnv) : object 'score' not found
    Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
      Running 'texi2dvi' on 'Extending-trackr.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `times.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    
    ! Emergency stop.
    <read *> 
             
    l.60 \usepackage
                    {hyperref}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    Calls: buildVignettes -> texi2pdf -> texi2dvi
    Execution halted
    ```

# TrafficBDE

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘neuralnet’
      All declared Imports should be used.
    ```

# treeio

Version: 1.4.3

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      2: getExportedValue(pkg, name)
      3: asNamespace(ns)
      4: getNamespace(ns)
      5: tryCatch(loadNamespace(name), error = function(e) stop(e))
      6: tryCatchList(expr, classes, parentenv, handlers)
      7: tryCatchOne(expr, names, parentenv, handlers[[1L]])
      8: value[[3L]](cond)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 91 SKIPPED: 0 FAILED: 2
      1. Error: (unknown) (@test-conversion.R#4) 
      2. Error: (unknown) (@test-treedata-accessor.R#34) 
      
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
    
    
    Attaching package: 'tidytree'
    
    The following object is masked from 'package:stats':
    
        filter
    
    Quitting from lines 467-478 (Importer.Rmd) 
    Error: processing vignette 'Importer.Rmd' failed with diagnostics:
    cannot coerce class 'structure("treedata", package = "tidytree")' to a data.frame
    Execution halted
    ```

# treeplyr

Version: 0.1.4

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘treeplyr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: filter_.treedata
    > ### Title: Function for filtering rows from an object of class 'treedata'
    > ### Aliases: filter_.treedata filter.treedata filter_.grouped_treedata
    > ###   filter.grouped_treedata filter_.grouped_treedata
    > 
    > ### ** Examples
    > 
    > data(anolis)
    > td <- make.treedata(anolis$phy, anolis$dat, name_column=1)
    > tdfilter <- filter(td, island=="Cuba", SVL > 3.5)
    Error: `.preserve` (`.preserve = FALSE`) must not be named, do you need `==`?
    Execution halted
    ```

# trelliscopejs

Version: 0.1.14

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      5: `_fseq`(`_lhs`)
      6: freduce(value, `_function_list`)
      7: withVisible(function_list[[k]](value))
      8: function_list[[k]](value)
      9: trelliscope(., name = "city_vs_highway_mpg", thumb = FALSE)
      10: trelliscope.data.frame(., name = "city_vs_highway_mpg", thumb = FALSE) at /Users/romain/git/release/dplyr/revdep/checks.noindex/trelliscopejs/new/trelliscopejs.Rcheck/00_pkg_src/trelliscopejs/R/trelliscope.R:22
      11: stop_nice("A column containing the panel to be plotted must be specified", "using panel() or img_panel().") at /Users/romain/git/release/dplyr/revdep/checks.noindex/trelliscopejs/new/trelliscopejs.Rcheck/00_pkg_src/trelliscopejs/R/trelliscope.R:80
      12: stop(paste(strwrap(paste(...), exdent = 7), collapse = "\n"), call. = FALSE) at /Users/romain/git/release/dplyr/revdep/checks.noindex/trelliscopejs/new/trelliscopejs.Rcheck/00_pkg_src/trelliscopejs/R/auto_cogs.R:12
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 224 SKIPPED: 0 FAILED: 1
      1. Error: examples run without barfing (@test-trelliscope.R#45) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# trialr

Version: 0.0.5

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.3Mb
      sub-directories of 1Mb or more:
        libs   6.5Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# tricolore

Version: 1.2.0

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘ggtern’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# tRophicPosition

Version: 0.7.5

## In both

*   checking examples ... ERROR
    ```
    ...
    Running examples in ‘tRophicPosition-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: TPmodel
    > ### Title: Function to create a JAGS-based Bayesian model to calculate
    > ###   trophic position
    > ### Aliases: TPmodel
    > 
    > ### ** Examples
    > 
    > isotopeData <- generateTPData()
    > model.string <- jagsBayesianModel()
    > model <- TPmodel(data = isotopeData, model.string = model.string,
    + n.adapt = 500)
    Error: .onLoad failed in loadNamespace() for 'rjags', details:
      call: dyn.load(file, DLLpath = DLLpath, ...)
      error: unable to load shared object '/Users/romain/git/release/dplyr/revdep/library.noindex/tRophicPosition/rjags/libs/rjags.so':
      dlopen(/Users/romain/git/release/dplyr/revdep/library.noindex/tRophicPosition/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
      Referenced from: /Users/romain/git/release/dplyr/revdep/library.noindex/tRophicPosition/rjags/libs/rjags.so
      Reason: image not found
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 108-110 (Multiple_model_calculation_of_trophic_position_in_R.Rmd) 
    Error: processing vignette 'Multiple_model_calculation_of_trophic_position_in_R.Rmd' failed with diagnostics:
    .onLoad failed in loadNamespace() for 'rjags', details:
      call: dyn.load(file, DLLpath = DLLpath, ...)
      error: unable to load shared object '/Users/romain/git/release/dplyr/revdep/library.noindex/tRophicPosition/rjags/libs/rjags.so':
      dlopen(/Users/romain/git/release/dplyr/revdep/library.noindex/tRophicPosition/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
      Referenced from: /Users/romain/git/release/dplyr/revdep/library.noindex/tRophicPosition/rjags/libs/rjags.so
      Reason: image not found
    Execution halted
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘SIBER’
    ```

# trread

Version: 0.2.5

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘here’ ‘htmltools’ ‘magrittr’ ‘scales’ ‘stringr’
      All declared Imports should be used.
    ```

# tsibble

Version: 0.6.2

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 659 SKIPPED: 0 FAILED: 16
      1. Failure: key_by() (@test-key.R#4) 
      2. Error: key_by() (@test-key.R#6) 
      3. Error: rename_tsibble() (@test-key.R#12) 
      4. Failure: pillar S3 methods (@test-pillar.R#19) 
      5. Failure: nest() (@test-tidyr.R#66) 
      6. Failure: nest() (@test-tidyr.R#67) 
      7. Failure: nest() (@test-tidyr.R#77) 
      8. Error: nest() (@test-tidyr.R#78) 
      9. Error: POSIXt with 1 second interval (@test-tsibble.R#80) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

# TSstudio

Version: 0.1.3

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘forecastHybrid’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# ttestshiny

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘shinyAce’ ‘shinyjs’
      All declared Imports should be used.
    ```

# turfR

Version: 0.8-7

## In both

*   checking R code for possible problems ... NOTE
    ```
    turf: no visible global function definition for ‘read.table’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/turfR/new/turfR.Rcheck/00_pkg_src/turfR/R/turfR_0.8-7.R:12)
    turf: no visible global function definition for ‘flush.console’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/turfR/new/turfR.Rcheck/00_pkg_src/turfR/R/turfR_0.8-7.R:102)
    turf.combos: no visible global function definition for ‘combn’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/turfR/new/turfR.Rcheck/00_pkg_src/turfR/R/turfR_0.8-7.R:158)
    Undefined global functions or variables:
      combn flush.console read.table
    Consider adding
      importFrom("utils", "combn", "flush.console", "read.table")
    to your NAMESPACE file.
    ```

# ukbtools

Version: 0.11.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘plyr’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 5 marked UTF-8 strings
    ```

# UKgrid

Version: 0.1.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 12.0Mb
      sub-directories of 1Mb or more:
        data   3.6Mb
        doc    8.3Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘rlang’
      All declared Imports should be used.
    ```

# understandBPMN

Version: 1.1.0

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      [1] -Inf - 0.145 == -Inf
      
      ── 3. Failure: Cross connectivity (@testMetrics.R#261)  ────────────────────────
      cross_connectivity(doc_simple, generate_new_path_log = TRUE) not equal to 0.5.
      1/1 mismatches
      [1] -Inf - 0.5 == -Inf
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 291 SKIPPED: 0 FAILED: 3
      1. Failure: Cross connectivity (@testMetrics.R#257) 
      2. Failure: Cross connectivity (@testMetrics.R#258) 
      3. Failure: Cross connectivity (@testMetrics.R#261) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# unvotes

Version: 0.2.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 4494 marked UTF-8 strings
    ```

# USAboundaries

Version: 0.3.1

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘USAboundariesData’
    ```

# utilsIPEA

Version: 0.0.6

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘RCurl’ ‘stringdist’ ‘utils’
      All declared Imports should be used.
    ```

# vaersNDvax

Version: 1.0.4

## In both

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking: ‘vaers’ ‘vaersND’
    ```

# vaersvax

Version: 1.0.5

## In both

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking: ‘vaers’ ‘vaersND’
    ```

# valr

Version: 0.4.2

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > genome <- read_genome(valr_example('hg19.chrom.sizes.gz'))
    > 
    > x <- bed_random(genome, seed = 1010486)
    > y <- bed_random(genome, seed = 9203911)
    > 
    > bed_absdist(x, y, genome)
    
    Attaching package: ‘dplyr’
    
    The following objects are masked from ‘package:stats’:
    
        filter, lag
    
    The following objects are masked from ‘package:base’:
    
        intersect, setdiff, setequal, union
    
    Error in dist_impl(x, y, distcalc = "absdist") : 
      Function not exported: C++ function with signature 'DataFrame(*build_index_cpp)(DataFrame)' not found in dplyr.
    Calls: bed_absdist -> dist_impl
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 214 SKIPPED: 3 FAILED: 147
      1. Error: absdist calculation is correct (@test_absdist.r#22) 
      2. Error: self absdist is 0 (@test_absdist.r#35) 
      3. Error: x ivls without matching y-ivls chroms are reported with absdist = NA (@test_absdist.r#56) 
      4. Error: ensure that absdist is calculated with respect to input tbls issue#108 (@test_absdist.r#87) 
      5. Error: 1bp closer, check for off-by-one errors (@test_closest.r#17) 
      6. Error: reciprocal test of 1bp closer, check for off-by-one errors (@test_closest.r#36) 
      7. Error: 0bp apart closer, check for off-by-one errors (@test_closest.r#55) 
      8. Error: reciprocal of 0bp apart closer, check for off-by-one errors (@test_closest.r#74) 
      9. Error: check that first left interval at index 0 is not lost (@test_closest.r#92) 
      1. ...
      
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
    
    Quitting from lines 46-60 (valr.Rmd) 
    Error: processing vignette 'valr.Rmd' failed with diagnostics:
    no applicable method for 'tbl_vars' applied to an object of class "NULL"
    Execution halted
    ```

# vapour

Version: 0.1.0

## In both

*   checking whether package ‘vapour’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/vapour/new/vapour.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘vapour’ ...
** package ‘vapour’ successfully unpacked and MD5 sums checked
configure: CC: clang
configure: CXX: clang++
checking for gdal-config... no
no
configure: error: gdal-config not found or not executable.
ERROR: configuration failed for package ‘vapour’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/vapour/new/vapour.Rcheck/vapour’

```
### CRAN

```
* installing *source* package ‘vapour’ ...
** package ‘vapour’ successfully unpacked and MD5 sums checked
configure: CC: clang
configure: CXX: clang++
checking for gdal-config... no
no
configure: error: gdal-config not found or not executable.
ERROR: configuration failed for package ‘vapour’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/vapour/old/vapour.Rcheck/vapour’

```
# vdmR

Version: 0.2.6

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘maptools’ ‘Rdpack’ ‘rgeos’
      All declared Imports should be used.
    ```

# VIM

Version: 4.7.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘mvoutlier’, ‘StatDA’, ‘mi’, ‘tkrplot’
    ```

# visdat

Version: 0.5.2

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    The error most likely occurred in:
    
    > ### Name: vis_expect
    > ### Title: Visualise whether a value is in a data frame
    > ### Aliases: vis_expect
    > 
    > ### ** Examples
    > 
    > 
    > dat_test <- tibble::tribble(
    +             ~x, ~y,
    +             -1,  "A",
    +             0,  "B",
    +             1,  "C",
    +             NA, NA
    +             )
    > 
    > vis_expect(dat_test, ~.x == -1)
    Error in n() : could not find function "n"
    Calls: vis_expect ... <Anonymous> -> <Anonymous> -> mutate.tbl_df -> mutate_impl
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      4: eval(quote(`_fseq`(`_lhs`)), env, env)
      5: eval(quote(`_fseq`(`_lhs`)), env, env)
      6: `_fseq`(`_lhs`)
      7: freduce(value, `_function_list`)
      8: function_list[[i]](value)
      9: dplyr::mutate(., rows = 1:n())
      10: mutate.tbl_df(., rows = 1:n()) at /Users/romain/git/tidyverse/dplyr/R/manip.r:416
      11: mutate_impl(.data, dots) at /Users/romain/git/tidyverse/dplyr/R/tbl-df.r:91
      
      nrows =  1000 ncols =  1000══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 41 SKIPPED: 5 FAILED: 1
      1. Error: (unknown) (@test-vis-expect.R#13) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘plotly’ ‘rlang’
      All declared Imports should be used.
    ```

# vkR

Version: 0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Missing or unexported object: ‘jsonlite::rbind.pages’
    ```

# vlad

Version: 0.2.0

## In both

*   checking whether package ‘vlad’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/vlad/new/vlad.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘vlad’ ...
** package ‘vlad’ successfully unpacked and MD5 sums checked
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include/ -I"/Users/romain/git/release/dplyr/revdep/library.noindex/vlad/Rcpp/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/vlad/RcppArmadillo/include" -I/usr/local/include  -fopenmp -fPIC  -Wno-unused-result -Wall -O3 -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘vlad’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/vlad/new/vlad.Rcheck/vlad’

```
### CRAN

```
* installing *source* package ‘vlad’ ...
** package ‘vlad’ successfully unpacked and MD5 sums checked
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include/ -I"/Users/romain/git/release/dplyr/revdep/library.noindex/vlad/Rcpp/include" -I"/Users/romain/git/release/dplyr/revdep/library.noindex/vlad/RcppArmadillo/include" -I/usr/local/include  -fopenmp -fPIC  -Wno-unused-result -Wall -O3 -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘vlad’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/vlad/old/vlad.Rcheck/vlad’

```
# vqtl

Version: 2.0.4

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      11: function_list[[i]](value)
      12: dplyr::summarise_(., mean = lazyeval::interp(~mean(var, na.rm = TRUE), var = as.name(phenotype.name)), 
             sd = lazyeval::interp(~sd(var, na.rm = TRUE), var = as.name(phenotype.name)), 
             mean.se = quote(sd/sqrt(n())), sd.se = quote(sqrt(2) * sd^2/sqrt(n() - 1)))
      13: summarise_.tbl_df(., mean = lazyeval::interp(~mean(var, na.rm = TRUE), var = as.name(phenotype.name)), 
             sd = lazyeval::interp(~sd(var, na.rm = TRUE), var = as.name(phenotype.name)), 
             mean.se = quote(sd/sqrt(n())), sd.se = quote(sqrt(2) * sd^2/sqrt(n() - 1))) at /Users/romain/git/tidyverse/dplyr/R/manip.r:264
      14: summarise_impl(.data, dots) at /Users/romain/git/tidyverse/dplyr/R/tbl-df.r:107
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 118 SKIPPED: 2 FAILED: 1
      1. Error: mean_var_sample_plot (@test-4-plots.R#76) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘iterators’ ‘knitr’ ‘purrr’ ‘testthat’
      All declared Imports should be used.
    ```

# vsn

Version: 3.48.1

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘affydata’
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    ...
    
        anyDuplicated, append, as.data.frame, basename, cbind, colMeans,
        colnames, colSums, dirname, do.call, duplicated, eval, evalq,
        Filter, Find, get, grep, grepl, intersect, is.unsorted, lapply,
        lengths, Map, mapply, match, mget, order, paste, pmax, pmax.int,
        pmin, pmin.int, Position, rank, rbind, Reduce, rowMeans,
        rownames, rowSums, sapply, setdiff, sort, table, tapply, union,
        unique, unsplit, which, which.max, which.min
    
    Welcome to Bioconductor
    
        Vignettes contain introductory material; view with
        'browseVignettes()'. To cite Bioconductor, see
        'citation("Biobase")', and for packages 'citation("pkgname")'.
    
    Warning in has_utility("convert", "ImageMagick") :
      ImageMagick not installed or not in PATH
    Quitting from lines 256-259 (A-vsn.Rmd) 
    Error: processing vignette 'A-vsn.Rmd' failed with diagnostics:
    there is no package called 'affydata'
    Execution halted
    ```

# VWPre

Version: 1.1.0

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Binning...
    Calculating proportions...
    There are 320 data points with less than 50 samples per bin.
    These can be examined and/or removed using the column 'NSamples'.
    These occur at time bin(s): 1150
    Subsequent Empirical Logit calculations may be influenced by the number of samples (depending on the number of observations requested).
    Sampling rate(s) present in the data are: 20 Hz.
    
    There are 50 samples per bin.
    One data point every 50 millisecond(s)
    Preparing ELogit output...
    Number of Observations equal to Number of Samples. 
     Calculation will be based on Number of Samples.
    Grand average of Proportion Looks calculated using Event means.
    Grand average of Proportion Looks calculated using Event means.
    Grand average of Proportion Looks calculated using Event means.
    Grand average of Proportion Looks calculated using Event means.
    Quitting from lines 84-90 (SR_Plotting.Rmd) 
    Error: processing vignette 'SR_Plotting.Rmd' failed with diagnostics:
    missing value where TRUE/FALSE needed
    Execution halted
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

Version: 0.2.3-1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.0Mb
      sub-directories of 1Mb or more:
        libs   5.6Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# wallace

Version: 1.0.6

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dismo’ ‘dplyr’ ‘DT’ ‘ENMeval’ ‘leaflet.extras’ ‘maptools’ ‘raster’
      ‘RColorBrewer’ ‘rgdal’ ‘rgeos’ ‘rmarkdown’ ‘shinyjs’ ‘shinythemes’
      ‘spocc’ ‘spThin’ ‘XML’ ‘zip’
      All declared Imports should be used.
    ```

# wand

Version: 0.2.0

## In both

*   checking whether package ‘wand’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/wand/new/wand.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘wand’ ...
** package ‘wand’ successfully unpacked and MD5 sums checked
Checking to see if libmagic is available...
** libs
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -L/usr/local/include -I"/Users/romain/git/release/dplyr/revdep/library.noindex/wand/Rcpp/include" -I/usr/local/include   -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c RcppExports.cpp -o RcppExports.o
clang: warning: argument unused during compilation: '-L/usr/local/include' [-Wunused-command-line-argument]
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -L/usr/local/include -I"/Users/romain/git/release/dplyr/revdep/library.noindex/wand/Rcpp/include" -I/usr/local/include   -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c wand.cpp -o wand.o
clang: warning: argument unused during compilation: '-L/usr/local/include' [-Wunused-command-line-argument]
In file included from wand.cpp:13:
./magic.h:123:2: warning: extra ';' outside of a function is a C++11 extension [-Wc++11-extra-semi]
};
 ^
1 warning generated.
clang++ -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o wand.so RcppExports.o wand.o -L/usr/local/lib -L/usr/lib -lmagic -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
ld: library not found for -lmagic
clang: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [wand.so] Error 1
ERROR: compilation failed for package ‘wand’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/wand/new/wand.Rcheck/wand’

```
### CRAN

```
* installing *source* package ‘wand’ ...
** package ‘wand’ successfully unpacked and MD5 sums checked
Checking to see if libmagic is available...
** libs
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -L/usr/local/include -I"/Users/romain/git/release/dplyr/revdep/library.noindex/wand/Rcpp/include" -I/usr/local/include   -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c RcppExports.cpp -o RcppExports.o
clang: warning: argument unused during compilation: '-L/usr/local/include' [-Wunused-command-line-argument]
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -L/usr/local/include -I"/Users/romain/git/release/dplyr/revdep/library.noindex/wand/Rcpp/include" -I/usr/local/include   -fPIC  -Wno-unused-result -Wno-c++11-inline-namespace -O3 -Wsign-compare -Wall -pedantic -Wno-long-long -c wand.cpp -o wand.o
clang: warning: argument unused during compilation: '-L/usr/local/include' [-Wunused-command-line-argument]
In file included from wand.cpp:13:
./magic.h:123:2: warning: extra ';' outside of a function is a C++11 extension [-Wc++11-extra-semi]
};
 ^
1 warning generated.
clang++ -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o wand.so RcppExports.o wand.o -L/usr/local/lib -L/usr/lib -lmagic -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
ld: library not found for -lmagic
clang: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [wand.so] Error 1
ERROR: compilation failed for package ‘wand’
* removing ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/wand/old/wand.Rcheck/wand’

```
# weathercan

Version: 0.2.8

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘xml2’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 25 marked UTF-8 strings
    ```

# whereport

Version: 0.1

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 4102 marked UTF-8 strings
    ```

# wiggleplotr

Version: 1.4.0

## In both

*   checking examples ... ERROR
    ```
    ...
    Warning: package ‘IRanges’ was built under R version 3.5.1
    
    Attaching package: ‘IRanges’
    
    The following objects are masked from ‘package:dplyr’:
    
        collapse, desc, slice
    
    Loading required package: GenomeInfoDb
    > require("org.Hs.eg.db")
    Loading required package: org.Hs.eg.db
    Warning in library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE,  :
      there is no package called ‘org.Hs.eg.db’
    > require("TxDb.Hsapiens.UCSC.hg38.knownGene")
    Loading required package: TxDb.Hsapiens.UCSC.hg38.knownGene
    Warning in library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE,  :
      there is no package called ‘TxDb.Hsapiens.UCSC.hg38.knownGene’
    > 
    > orgdb = org.Hs.eg.db
    Error: object 'org.Hs.eg.db' not found
    Execution halted
    ```

*   checking for missing documentation entries ... WARNING
    ```
    Warning: package ‘GenomicRanges’ was built under R version 3.5.1
    Warning: package ‘IRanges’ was built under R version 3.5.1
    All user-level objects in a package should have documentation entries.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking for code/documentation mismatches ... WARNING
    ```
    Codoc mismatches from documentation object 'getGenotypePalette':
    getGenotypePalette
      Code: function(old = FALSE)
      Docs: function()
      Argument names in code not in docs:
        old
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 18-28 (wiggleplotr.Rmd) 
    Error: processing vignette 'wiggleplotr.Rmd' failed with diagnostics:
    there is no package called 'EnsDb.Hsapiens.v86'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      ‘EnsDb.Hsapiens.v86’ ‘org.Hs.eg.db’
      ‘TxDb.Hsapiens.UCSC.hg38.knownGene’
    ```

*   checking R code for possible problems ... NOTE
    ```
    plotCoverage: no visible global function definition for ‘is’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/wiggleplotr/new/wiggleplotr.Rcheck/00_pkg_src/wiggleplotr/R/wiggleplotr.R:184)
    plotCoverage: no visible global function definition for ‘is’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/wiggleplotr/new/wiggleplotr.Rcheck/00_pkg_src/wiggleplotr/R/wiggleplotr.R:185)
    plotTranscripts: no visible global function definition for ‘is’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/wiggleplotr/new/wiggleplotr.Rcheck/00_pkg_src/wiggleplotr/R/wiggleplotr.R:33)
    plotTranscripts: no visible global function definition for ‘is’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/wiggleplotr/new/wiggleplotr.Rcheck/00_pkg_src/wiggleplotr/R/wiggleplotr.R:34)
    Undefined global functions or variables:
      is
    Consider adding
      importFrom("methods", "is")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

# windfarmGA

Version: 1.2.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘data.table’ ‘RandomFields’
      All declared Imports should be used.
    ```

# wiseR

Version: 1.0.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.2Mb
      sub-directories of 1Mb or more:
        bn   7.1Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘arules’ ‘bnlearn’ ‘DescTools’ ‘dplyr’ ‘DT’ ‘graph’ ‘HydeNet’
      ‘igraph’ ‘linkcomm’ ‘missRanger’ ‘parallel’ ‘psych’ ‘RBGL’
      ‘Rgraphviz’ ‘rhandsontable’ ‘rintrojs’ ‘shinyalert’ ‘shinyBS’
      ‘shinycssloaders’ ‘shinydashboard’ ‘shinyWidgets’ ‘tools’
      ‘visNetwork’
      All declared Imports should be used.
    ```

# wordbankr

Version: 0.3.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dbplyr’
      All declared Imports should be used.
    ```

# WRTDStidal

Version: 1.1.1

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘WRTDStidal-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: sliceplot
    > ### Title: Plot time slices within a tidal object
    > ### Aliases: sliceplot sliceplot.tidal sliceplot.tidalmean
    > 
    > ### ** Examples
    > 
    > 
    > ## load a fitted tidal object
    > data(tidfit)
    > 
    > # plot using defaults
    > sliceplot(tidfit)
    Error: Insufficient values in manual scale. 12 needed but only 2 provided.
    Execution halted
    ```

# XBSeq

Version: 1.12.0

## In both

*   R CMD check timed out
    

*   checking whether package ‘XBSeq’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘IRanges’ was built under R version 3.5.1
      Warning: package ‘GenomicRanges’ was built under R version 3.5.1
      Warning: package ‘DelayedArray’ was built under R version 3.5.1
    See ‘/Users/romain/git/release/dplyr/revdep/checks.noindex/XBSeq/new/XBSeq.Rcheck/00install.out’ for details.
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
      ‘conditions’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/XBSeq/new/XBSeq.Rcheck/00_pkg_src/XBSeq/R/core_functions.R:106)
    estimateSCV,XBSeqDataSet: no visible global function definition for
      ‘conditions’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/XBSeq/new/XBSeq.Rcheck/00_pkg_src/XBSeq/R/core_functions.R:107)
    estimateSCV,XBSeqDataSet: no visible global function definition for
      ‘dispTable<-’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/XBSeq/new/XBSeq.Rcheck/00_pkg_src/XBSeq/R/core_functions.R:108)
    Undefined global functions or variables:
      ..count.. assay assay<- assays baseMean coefficients complete.cases
      conditions cor data DataFrame ddelap dispTable dispTable<- dnbinom
      dpois formula Gamma glm Group log2FoldChange median optim p.adjust
      pbeta predict qbeta quantile rnbinom Sample scvBiasCorrectionFits
      SummarizedExperiment
    Consider adding
      importFrom("stats", "coefficients", "complete.cases", "cor", "dnbinom",
                 "dpois", "formula", "Gamma", "glm", "median", "optim",
                 "p.adjust", "pbeta", "predict", "qbeta", "quantile",
                 "rnbinom")
      importFrom("utils", "data")
    to your NAMESPACE file.
    ```

# XGR

Version: 1.1.4

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.3Mb
      sub-directories of 1Mb or more:
        data   1.1Mb
        R      3.5Mb
    ```

# XKCDdata

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tibble’
      All declared Imports should be used.
    ```

# xpose

Version: 0.4.3

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
          filter
      
      > 
      > test_check("xpose")
      ── 1. Failure: dot arguments are properly passed to readr (@test-read_nm_tables.
      nrow(...) not equal to 3.
      1/1 mismatches
      [1] 2 - 3 == -1
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 523 SKIPPED: 6 FAILED: 1
      1. Failure: dot arguments are properly passed to readr (@test-read_nm_tables.R#57) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# xtractomatic

Version: 3.4.2

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    date	lon	lat	lowLon	higLon	lowLat	higLat
    4/23/2003	203.899	19.664	203.899	203.899	19.664	19.664
    4/24/2003	204.151	19.821	203.912597	204.389403	18.78051934	20.86148066
    4/30/2003	203.919	20.351	203.6793669	204.1586331	18.79728188	21.90471812
    5/1/2003	204.229	20.305	203.9943343	204.4636657	18.90440013	21.70559987
    Quitting from lines 818-843 (Usingxtractomatic.Rmd) 
    Error: processing vignette 'Usingxtractomatic.Rmd' failed with diagnostics:
    (converted from warning) Removed 4070 rows containing missing values (geom_raster).
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# Zelig

Version: 5.1.6

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.4Mb
      sub-directories of 1Mb or more:
        R   5.4Mb
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

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘zFactor-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: z.plot.range
    > ### Title: Tile plot of best fit area for a correlation
    > ### Aliases: z.plot.range
    > 
    > ### ** Examples
    > 
    > # plot Dranchuk-AbouKassem
    > z.plot.range("DAK")
    Error in n() : could not find function "n"
    Calls: z.plot.range ... z.stats -> summarise -> summarise.tbl_df -> summarise_impl
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      12: summarise.tbl_df(grouped, z.chart, z.calc, RMSE = sqrt(mean((z.chart - z.calc)^2)), 
             MPE = sum((z.chart - z.calc)/z.chart) * 100/n(), MAPE = sum(abs((z.chart - z.calc)/z.chart)) * 
                 100/n(), MSE = sum((z.chart - z.calc)^2)/n(), RSS = sum((z.chart - z.calc)^2), 
             MAE = sum(abs(z.chart - z.calc))/n(), MAAPE = sum(atan(abs((z.chart - z.calc)/z.chart)))/n()) at /Users/romain/git/tidyverse/dplyr/R/manip.r:255
      13: summarise_impl(.data, dots) at /Users/romain/git/tidyverse/dplyr/R/tbl-df.r:102
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 120 SKIPPED: 0 FAILED: 4
      1. Error: z.stats matches dimension and mean of MAPE. default interval = coarse (@test_stats.R#8) 
      2. Error: z.stats matches dimension and mean of MAPE, interval = fine (@test_stats.R#19) 
      3. Error: z.stats matches dimension and mean of MAPE, range = hp (@test_stats.R#30) 
      4. Error: z.plot.range work for Hall-Yarborough (@test_stats.R#62) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘rootSolve’
      All declared Imports should be used.
    ```

# zFPKM

Version: 1.2.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    The following object is masked from 'package:S4Vectors':
    
        expand
    
    trying URL 'ftp://ftp.ncbi.nlm.nih.gov/geo/series/GSE94nnn/GSE94802/suppl/GSE94802_Minkina_etal_normalized_FPKM.csv.gz'
    Content type 'unknown' length 800733 bytes (781 KB)
    ==================================================
    trying URL 'ftp://ftp.ncbi.nlm.nih.gov/geo/series/GSE94nnn/GSE94802/suppl/GSE94802_Minkina_etal_raw_counts.csv.gz'
    Content type 'unknown' length 574041 bytes (560 KB)
    ==================================================
    
    Attaching package: 'limma'
    
    The following object is masked from 'package:BiocGenerics':
    
        plotMA
    
    Quitting from lines 108-122 (zFPKM.Rmd) 
    Error: processing vignette 'zFPKM.Rmd' failed with diagnostics:
    statmod package required but is not installed
    Execution halted
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    PlotGaussianFitDF: no visible binding for global variable ‘density’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/zFPKM/new/zFPKM.Rcheck/00_pkg_src/zFPKM/R/zfpkm.R:223)
    PlotGaussianFitDF: no visible binding for global variable ‘log2fpkm’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/zFPKM/new/zFPKM.Rcheck/00_pkg_src/zFPKM/R/zfpkm.R:223)
    PlotGaussianFitDF: no visible binding for global variable ‘sample_name’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/zFPKM/new/zFPKM.Rcheck/00_pkg_src/zFPKM/R/zfpkm.R:223)
    PlotGaussianFitDF: no visible binding for global variable ‘log2fpkm’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/zFPKM/new/zFPKM.Rcheck/00_pkg_src/zFPKM/R/zfpkm.R:227-233)
    PlotGaussianFitDF: no visible binding for global variable ‘density’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/zFPKM/new/zFPKM.Rcheck/00_pkg_src/zFPKM/R/zfpkm.R:227-233)
    zFPKMCalc: no visible global function definition for ‘density’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/zFPKM/new/zFPKM.Rcheck/00_pkg_src/zFPKM/R/zfpkm.R:162)
    zFPKMTransform: no visible global function definition for ‘is’
      (/Users/romain/git/release/dplyr/revdep/checks.noindex/zFPKM/new/zFPKM.Rcheck/00_pkg_src/zFPKM/R/zfpkm.R:125-127)
    Undefined global functions or variables:
      density dnorm is log2fpkm sample_name
    Consider adding
      importFrom("methods", "is")
      importFrom("stats", "density", "dnorm")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

# ZipRadius

Version: 1.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘ggplot2’ ‘testthat’
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

