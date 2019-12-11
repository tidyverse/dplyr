# aemo

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/aemo
* Date/Publication: 2016-08-20 15:33:40
* Number of recursive dependencies: 41

Run `revdep_details(,"aemo")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... NOTE
    ```
    Missing or unexported object: ‘dplyr::rbind_all’
    ```

# boxr

<details>

* Version: 0.3.4
* Source code: https://github.com/cran/boxr
* URL: https://github.com/brendan-r/boxr/
* BugReports: https://github.com/brendan-r/boxr/issues
* Date/Publication: 2017-01-12 15:13:30
* Number of recursive dependencies: 65

Run `revdep_details(,"boxr")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... NOTE
    ```
    Missing or unexported object: ‘dplyr::rbind_all’
    ```

# broomExtra

<details>

* Version: 0.0.5
* Source code: https://github.com/cran/broomExtra
* URL: https://indrajeetpatil.github.io/broomExtra/, https://github.com/IndrajeetPatil/broomExtra
* BugReports: https://github.com/IndrajeetPatil/broomExtra/issues
* Date/Publication: 2019-08-19 13:30:06 UTC
* Number of recursive dependencies: 78

Run `revdep_details(,"broomExtra")` for more info

</details>

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
    +   na.action = na.omit,
    +   augment.args = list(se_fit = TRUE)
    + )
    Error in group_vars(.data) : argument ".data" is missing, with no default
    Calls: <Anonymous> ... <Anonymous> -> group_modify.grouped_df -> group_vars
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      6: `_fseq`(`_lhs`)
      7: freduce(value, `_function_list`)
      8: function_list[[i]](value)
      9: dplyr::group_modify(.tbl = ., .f = augment_group, keep = TRUE)
      10: group_modify.grouped_df(.tbl = ., .f = augment_group, keep = TRUE)
      11: group_vars(.data)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 21 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 3 ]
      1. Error: `grouped_tidy()` works (@test-grouped_generics.R#12) 
      2. Error: `grouped_glance()` works (@test-grouped_generics.R#43) 
      3. Error: `grouped_augment()` works (@test-grouped_generics.R#72) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘utils’
      All declared Imports should be used.
    ```

# DeLorean

<details>

* Version: 1.5.0
* Source code: https://github.com/cran/DeLorean
* Date/Publication: 2018-10-17 22:30:16 UTC
* Number of recursive dependencies: 116

Run `revdep_details(,"DeLorean")` for more info

</details>

## Newly broken

*   R CMD check timed out
    

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.5Mb
      sub-directories of 1Mb or more:
        libs   4.8Mb
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

# egor

<details>

* Version: 0.19.10
* Source code: https://github.com/cran/egor
* URL: https://github.com/tilltnet/egor, https://tilltnet.github.io/egor/
* BugReports: https://github.com/tilltnet/egor/issues
* Date/Publication: 2019-10-07 22:10:06 UTC
* Number of recursive dependencies: 61

Run `revdep_details(,"egor")` for more info

</details>

## Newly broken

*   checking S3 generic/method consistency ... WARNING
    ```
    group_modify:
      function(.data, .f, ..., keep)
    group_modify.egor:
      function(.tbl, .f, ...)
    
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

# ggstatsplot

<details>

* Version: 0.1.2
* Source code: https://github.com/cran/ggstatsplot
* URL: https://indrajeetpatil.github.io/ggstatsplot/, https://github.com/IndrajeetPatil/ggstatsplot
* BugReports: https://github.com/IndrajeetPatil/ggstatsplot/issues
* Date/Publication: 2019-09-17 10:40:02 UTC
* Number of recursive dependencies: 245

Run `revdep_details(,"ggstatsplot")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > 
    > 
    > # this internal function may not have much utility outside of the package
    > set.seed(123)
    > library(ggplot2)
    > 
    > # make a plot
    > p <- ggplot(data = iris, aes(x = Species, y = Sepal.Length)) +
    +   geom_boxplot()
    > 
    > # get a dataframe with means
    > mean_dat <- ggstatsplot:::mean_labeller(
    +   data = iris,
    +   x = Species,
    +   y = Sepal.Length,
    +   mean.ci = TRUE,
    +   k = 3
    + )
    Error in group_vars(.data) : argument ".data" is missing, with no default
    Calls: <Anonymous> ... <Anonymous> -> group_modify.grouped_df -> group_vars
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      # … with 1 more variable: significance <chr>
      Error: Row variable 'y' contains less than 2 levels.
      Chi-squared test can't be run; no subtitle displayed.# A tibble: 1 x 11
        class counts  perc N     `4`   `6`   statistic p.value parameter method
        <fct>  <int> <dbl> <chr> <chr> <chr>     <dbl>   <dbl>     <dbl> <chr> 
      1 mids…     18   100 (n =… 38.8… 61.1…     0.889   0.346         1 Chi-s…
      # … with 1 more variable: significance <chr>
      Warning: Individual plots in the combined `grouped_` plot
      can't be further modified with `ggplot2` functions.
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 83 | SKIPPED: 112 | WARNINGS: 0 | FAILED: 1 ]
      1. Error: outlier.labeling works across vector types (@test-ggbetweenstats.R#27) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# ggvis

<details>

* Version: 0.4.4
* Source code: https://github.com/cran/ggvis
* URL: http://ggvis.rstudio.com/
* Date/Publication: 2018-09-28 21:50:03 UTC
* Number of recursive dependencies: 54

Run `revdep_details(,"ggvis")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... NOTE
    ```
    Missing or unexported object: ‘dplyr::rbind_all’
    ```

# gQTLstats

<details>

* Version: 1.16.0
* Source code: https://github.com/cran/gQTLstats
* Date/Publication: 2019-05-02
* Number of recursive dependencies: 192

Run `revdep_details(,"gQTLstats")` for more info

</details>

## Newly broken

*   checking whether package ‘gQTLstats’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/tidyverse/dplyr/revdep/checks.noindex/gQTLstats/new/gQTLstats.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking whether package ‘gQTLstats’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘AnnotationDbi’ was built under R version 3.6.1
      Warning: package ‘IRanges’ was built under R version 3.6.1
      Warning: package ‘S4Vectors’ was built under R version 3.6.1
      Warning: package ‘GenomicRanges’ was built under R version 3.6.1
    See ‘/Users/romainfrancois/git/tidyverse/dplyr/revdep/checks.noindex/gQTLstats/old/gQTLstats.Rcheck/00install.out’ for details.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 65.7Mb
      sub-directories of 1Mb or more:
        data        11.0Mb
        doc          1.1Mb
        registries  18.8Mb
        vcf         33.8Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      ‘permScore_3’
    storeToMaxAssocBySNP: no visible global function definition for ‘nth’
    storeToMaxAssocBySNP: no visible binding for global variable ‘MAF’
    storeToMaxAssocBySNP: no visible binding for global variable ‘probeid’
    storeToMaxAssocBySNP: no visible binding for global variable ‘mindist’
    tqbrowser: no visible global function definition for ‘experiments’
    tqbrowser : server: no visible global function definition for
      ‘experiments’
    tqbrowser : server: no visible global function definition for
      ‘TabixFile’
    tqbrowser : server: no visible binding for global variable ‘assoc’
    tqbrowser : server: no visible binding for global variable ‘stateid’
    tqbrowser : server: no visible binding for global variable ‘state’
    transTable: no visible binding for global variable ‘i’
    tsByRank_sing: no visible binding for global variable ‘i’
    tsByRank_sing : <anonymous>: no visible binding for global variable ‘i’
    boxswarm,SnpToGeneQTL: no visible binding for global variable ‘g1’
    Undefined global functions or variables:
      DNAStringSetList MAF TabixFile TxDb assoc calls ch chisq criterion ex
      exonsBy experiments g1 geom_boxplot gt i id maf mindist ml10fdr nth
      permScore_1 permScore_2 permScore_3 probeid snp state stateid value x
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘MultiAssayExperiment’
    ```

*   checking contents of ‘data’ directory ... NOTE
    ```
    ...
      names
      class
          filterUsed
        <environment: namespace:base>
      x
          class
      package
              [2]
            [1]
            [2]
          names
          out.attrs
        [1]
        [2]
      [1]
      [2]
      names
          row.names
          class
            names
              names
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 8 marked Latin-1 strings
      Note: found 12 marked UTF-8 strings
    ```

## Installation

### Devel

```
* installing *source* package ‘gQTLstats’ ...
** using staged installation
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Warning messages:
1: package ‘AnnotationDbi’ was built under R version 3.6.1 
2: package ‘IRanges’ was built under R version 3.6.1 
3: package ‘S4Vectors’ was built under R version 3.6.1 
4: package ‘GenomicRanges’ was built under R version 3.6.1 
Error: object ‘rbind_all’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘gQTLstats’
* removing ‘/Users/romainfrancois/git/tidyverse/dplyr/revdep/checks.noindex/gQTLstats/new/gQTLstats.Rcheck/gQTLstats’

```
### CRAN

```
* installing *source* package ‘gQTLstats’ ...
** using staged installation
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Warning messages:
1: package ‘AnnotationDbi’ was built under R version 3.6.1 
2: package ‘IRanges’ was built under R version 3.6.1 
3: package ‘S4Vectors’ was built under R version 3.6.1 
4: package ‘GenomicRanges’ was built under R version 3.6.1 
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
Warning: package ‘AnnotationDbi’ was built under R version 3.6.1
Warning: package ‘IRanges’ was built under R version 3.6.1
Warning: package ‘S4Vectors’ was built under R version 3.6.1
Warning: package ‘GenomicRanges’ was built under R version 3.6.1
** testing if installed package can be loaded from final location
Warning: package ‘AnnotationDbi’ was built under R version 3.6.1
Warning: package ‘IRanges’ was built under R version 3.6.1
Warning: package ‘S4Vectors’ was built under R version 3.6.1
Warning: package ‘GenomicRanges’ was built under R version 3.6.1
** testing if installed package keeps a record of temporary installation path
* DONE (gQTLstats)

```
# groupedstats

<details>

* Version: 0.0.9
* Source code: https://github.com/cran/groupedstats
* URL: https://indrajeetpatil.github.io/groupedstats/, https://github.com/IndrajeetPatil/groupedstats/
* BugReports: https://github.com/IndrajeetPatil/groupedstats/issues/
* Date/Publication: 2019-08-28 13:30:02 UTC
* Number of recursive dependencies: 130

Run `revdep_details(,"groupedstats")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Running examples in ‘groupedstats-Ex.R’ failed
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
    Error in group_vars(.data) : argument ".data" is missing, with no default
    Calls: <Anonymous> ... <Anonymous> -> group_modify.grouped_df -> group_vars
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      8: function_list[[i]](value)
      9: dplyr::group_modify(.tbl = ., .f = ~tibble::as_tibble(skimr::skim_to_wide(purrr::keep(.x = ., 
             .p = ..f))), keep = FALSE)
      10: group_modify.grouped_df(.tbl = ., .f = ~tibble::as_tibble(skimr::skim_to_wide(purrr::keep(.x = ., 
             .p = ..f))), keep = FALSE)
      11: group_vars(.data)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 84 | SKIPPED: 4 | WARNINGS: 0 | FAILED: 3 ]
      1. Error: grouped_glmer works (@test-grouped_glmer.R#11) 
      2. Error: grouped_summary with numeric measures (@test-grouped_summary.R#11) 
      3. Error: grouped_summary with factor measures (@test-grouped_summary.R#58) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# mlVAR

<details>

* Version: 0.4.3
* Source code: https://github.com/cran/mlVAR
* Date/Publication: 2019-06-20 17:00:03 UTC
* Number of recursive dependencies: 110

Run `revdep_details(,"mlVAR")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... NOTE
    ```
    Missing or unexported object: ‘dplyr::rbind_all’
    ```

*   checking R code for possible problems ... NOTE
    ```
    mlVAR0: no visible global function definition for ‘rbind_all’
    mlVAR0 : <anonymous>: no visible global function definition for
      ‘rbind_all’
    movingWindow: no visible binding for global variable ‘rbind_all’
    movingWindow : <anonymous>: no visible binding for global variable
      ‘rbind_all’
    Undefined global functions or variables:
      rbind_all
    ```

# OncoSimulR

<details>

* Version: 2.14.0
* Source code: https://github.com/cran/OncoSimulR
* URL: https://github.com/rdiaz02/OncoSimul, https://popmodels.cancercontrol.cancer.gov/gsr/packages/oncosimulr/
* BugReports: https://github.com/rdiaz02/OncoSimul/issues
* Date/Publication: 2019-05-02
* Number of recursive dependencies: 106

Run `revdep_details(,"OncoSimulR")` for more info

</details>

## Newly broken

*   R CMD check timed out
    

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.4Mb
      sub-directories of 1Mb or more:
        doc    5.3Mb
        libs   1.3Mb
    ```

# perturbatr

<details>

* Version: 1.4.0
* Source code: https://github.com/cran/perturbatr
* URL: https://github.com/cbg-ethz/perturbatr
* BugReports: https://github.com/cbg-ethz/perturbatr/issues
* Date/Publication: 2019-05-02
* Number of recursive dependencies: 92

Run `revdep_details(,"perturbatr")` for more info

</details>

## Newly broken

*   R CMD check timed out
    

# Rariant

<details>

* Version: 1.20.0
* Source code: https://github.com/cran/Rariant
* URL: https://github.com/juliangehring/Rariant
* BugReports: https://support.bioconductor.org
* Date/Publication: 2019-05-02
* Number of recursive dependencies: 153

Run `revdep_details(,"Rariant")` for more info

</details>

## Newly broken

*   checking whether package ‘Rariant’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/romainfrancois/git/tidyverse/dplyr/revdep/checks.noindex/Rariant/new/Rariant.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking examples ... ERROR
    ```
    ...
    > ### Aliases: tallyPlot
    > 
    > ### ** Examples
    > 
    >   library(ggbio)
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
      Warning: package ‘GenomicRanges’ was built under R version 3.6.1
      Warning: package ‘S4Vectors’ was built under R version 3.6.1
      Warning: package ‘IRanges’ was built under R version 3.6.1
      Warning: package ‘SummarizedExperiment’ was built under R version 3.6.1
      Warning: package ‘BiocParallel’ was built under R version 3.6.1
      Warning: package ‘Rsamtools’ was built under R version 3.6.1
    See ‘/Users/romainfrancois/git/tidyverse/dplyr/revdep/checks.noindex/Rariant/old/Rariant.Rcheck/00install.out’ for details.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  7.7Mb
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

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘BSgenome.Hsapiens.UCSC.hg19’
    ```

## Installation

### Devel

```
* installing *source* package ‘Rariant’ ...
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Warning messages:
1: package ‘GenomicRanges’ was built under R version 3.6.1 
2: package ‘S4Vectors’ was built under R version 3.6.1 
3: package ‘IRanges’ was built under R version 3.6.1 
4: package ‘SummarizedExperiment’ was built under R version 3.6.1 
5: package ‘BiocParallel’ was built under R version 3.6.1 
6: package ‘Rsamtools’ was built under R version 3.6.1 
Error: object 'rbind_all' is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘Rariant’
* removing ‘/Users/romainfrancois/git/tidyverse/dplyr/revdep/checks.noindex/Rariant/new/Rariant.Rcheck/Rariant’

```
### CRAN

```
* installing *source* package ‘Rariant’ ...
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Warning messages:
1: package ‘GenomicRanges’ was built under R version 3.6.1 
2: package ‘S4Vectors’ was built under R version 3.6.1 
3: package ‘IRanges’ was built under R version 3.6.1 
4: package ‘SummarizedExperiment’ was built under R version 3.6.1 
5: package ‘BiocParallel’ was built under R version 3.6.1 
6: package ‘Rsamtools’ was built under R version 3.6.1 
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
Warning: package ‘GenomicRanges’ was built under R version 3.6.1
Warning: package ‘S4Vectors’ was built under R version 3.6.1
Warning: package ‘IRanges’ was built under R version 3.6.1
Warning: package ‘SummarizedExperiment’ was built under R version 3.6.1
Warning: package ‘BiocParallel’ was built under R version 3.6.1
Warning: package ‘Rsamtools’ was built under R version 3.6.1
** testing if installed package can be loaded from final location
Warning: package ‘GenomicRanges’ was built under R version 3.6.1
Warning: package ‘S4Vectors’ was built under R version 3.6.1
Warning: package ‘IRanges’ was built under R version 3.6.1
Warning: package ‘SummarizedExperiment’ was built under R version 3.6.1
Warning: package ‘BiocParallel’ was built under R version 3.6.1
Warning: package ‘Rsamtools’ was built under R version 3.6.1
** testing if installed package keeps a record of temporary installation path
* DONE (Rariant)

```
