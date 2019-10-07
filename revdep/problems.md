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

# dbplyr

<details>

* Version: 1.4.2
* Source code: https://github.com/cran/dbplyr
* URL: https://dbplyr.tidyverse.org/, https://github.com/tidyverse/dbplyr
* BugReports: https://github.com/tidyverse/dbplyr/issues
* Date/Publication: 2019-06-17 20:00:04 UTC
* Number of recursive dependencies: 61

Run `revdep_details(,"dbplyr")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... NOTE
    ```
    Unexported objects imported by ':::' calls:
      ‘dplyr:::compat_lazy_dots’ ‘dplyr:::find_var’
      See the note in ?`:::` about the use of this operator.
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

# explore

<details>

* Version: 0.5.0
* Source code: https://github.com/cran/explore
* URL: http://github.com/rolkra/explore
* Date/Publication: 2019-09-19 12:40:02 UTC
* Number of recursive dependencies: 78

Run `revdep_details(,"explore")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘explore-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: explore_tbl
    > ### Title: Explore table
    > ### Aliases: explore_tbl
    > 
    > ### ** Examples
    > 
    > explore_tbl(iris)
    Error in `$<-.data.frame`(`*tmp*`, "measure", value = "no variance") : 
      replacement has 1 row, data has 0
    Calls: explore_tbl -> $<- -> $<-.data.frame
    Execution halted
    ```

# getTBinR

<details>

* Version: 0.7.0
* Source code: https://github.com/cran/getTBinR
* URL: https://www.samabbott.co.uk/getTBinR, https://github.com/seabbs/getTBinR
* BugReports: https://github.com/seabbs/getTBinR/issues
* Date/Publication: 2019-09-03 13:50:06 UTC
* Number of recursive dependencies: 146

Run `revdep_details(,"getTBinR")` for more info

</details>

## Newly broken

*   R CMD check timed out
    

# ggvis

<details>

* Version: 0.4.4
* Source code: https://github.com/cran/ggvis
* URL: http://ggvis.rstudio.com/
* Date/Publication: 2018-09-28 21:50:03 UTC
* Number of recursive dependencies: 53

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
* Number of recursive dependencies: 191

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
      installed size is 65.8Mb
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

# MXM

<details>

* Version: 1.4.4
* Source code: https://github.com/cran/MXM
* URL: http://mensxmachina.org
* Date/Publication: 2019-06-19 13:40:03 UTC
* Number of recursive dependencies: 77

Run `revdep_details(,"MXM")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  5.0Mb
      sub-directories of 1Mb or more:
        R     3.1Mb
        doc   1.3Mb
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘knitr’
      All declared Imports should be used.
    ```

# Rariant

<details>

* Version: 1.20.0
* Source code: https://github.com/cran/Rariant
* URL: https://github.com/juliangehring/Rariant
* BugReports: https://support.bioconductor.org
* Date/Publication: 2019-05-02
* Number of recursive dependencies: 152

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
# rgho

<details>

* Version: 1.0.1
* Source code: https://github.com/cran/rgho
* URL: https://github.com/pierucci/rgho, https://pierucci.org
* BugReports: https://github.com/pierucci/rgho/issues
* Date/Publication: 2017-01-18 18:07:51
* Number of recursive dependencies: 52

Run `revdep_details(,"rgho")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
     4             2.1     2.1 NA    NA    NA      
     5             2.1     2.1 NA    NA    NA      
     6             2.1     2.1 NA    NA    NA      
     7             2.2     2.2 NA    NA    NA      
     8             2.2     2.2 NA    NA    NA      
     9             2.2     2.2 NA    NA    NA      
    10             2.2     2.2 NA    NA    NA      
    # … with 5,722 more rows
    > 
    > 
    > result <- get_gho_data(
    +   dimension = "GHO",
    +   code = "MDG_0000000001",
    +   filter = list(
    +     REGION = "EUR",
    +     YEAR = "2015"
    +   )
    + )
    Error in get_gho_data(dimension = "GHO", code = "MDG_0000000001", filter = list(REGION = "EUR",  : 
      No data returned by WHO GHO server.
    Execution halted
    ```

# TCGAutils

<details>

* Version: 1.4.0
* Source code: https://github.com/cran/TCGAutils
* BugReports: https://github.com/waldronlab/TCGAutils/issues
* Date/Publication: 2019-05-02
* Number of recursive dependencies: 197

Run `revdep_details(,"TCGAutils")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Using temporary cache /var/folders/4b/hn4fq98s6810s4ccv2f9hm2h0000gn/T//RtmpbJ1I0F/BiocFileCache
    downloading 1 resources
    retrieving 1 resource
    Using temporary cache /var/folders/4b/hn4fq98s6810s4ccv2f9hm2h0000gn/T//RtmpbJ1I0F/BiocFileCache
    Warning: download failed
      web resource path: ‘https://experimenthub.bioconductor.org/fetch/558’
      local file path: ‘/var/folders/4b/hn4fq98s6810s4ccv2f9hm2h0000gn/T//RtmpbJ1I0F/BiocFileCache/1b8357a328a1_558’
      reason: Internal Server Error (HTTP 500).
    Warning: bfcadd() failed; resource removed
      rid: BFC7
      fpath: ‘https://experimenthub.bioconductor.org/fetch/558’
      reason: download failed
    Warning: download failed
      hub path: ‘https://experimenthub.bioconductor.org/fetch/558’
      cache resource: ‘EH558 : 558’
      reason: bfcadd() failed; see warnings()
    Error: failed to load resource
      name: EH558
      title: ACC_CNASNP-20160128
      reason: 1 resources failed to download
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Unexported objects imported by ':::' calls:
      ‘BiocGenerics:::replaceSlots’ ‘GenomicRanges:::.normarg_field’
      See the note in ?`:::` about the use of this operator.
    ```

