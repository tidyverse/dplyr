# adductomicsR

<details>

* Version: 1.2.0
* Source code: https://github.com/cran/adductomicsR
* BugReports: https://github.com/JosieLHayes/adductomicsR/issues
* Date/Publication: 2019-10-29
* Number of recursive dependencies: 143

Run `revdep_details(,"adductomicsR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    
    ...DONE
    extracting metaData from MS2 file...
    
    1 of 1 complete (data42_21221_2.mzXML).
    
    Error in { : 
      task 1 failed - "[SpectrumList_mzXML::spectrum()] Error seeking to <scan>."
    Calls: rtDevModelling -> adductSpecGen -> %dopar% -> <Anonymous>
    Execution halted
    Error in unserialize(node$con) : error reading from connection
    Calls: <Anonymous> ... doTryCatch -> recvData -> recvData.SOCKnode -> unserialize
    In addition: Warning message:
    In fun(libname, pkgname) :
      mzR has been built against a different Rcpp version (1.0.2)
    than is installed on your system (1.0.3). This might lead to errors
    when loading mzR. If you encounter such issues, please send a report,
    including the output of sessionInfo() to the Bioc support forum at 
    https://support.bioconductor.org/. For details see also
    https://github.com/sneumann/mzR/wiki/mzR-Rcpp-compiler-linker-issue.
    Execution halted
    ```

# AMARETTO

<details>

* Version: 1.2.0
* Source code: https://github.com/cran/AMARETTO
* Date/Publication: 2019-12-09
* Number of recursive dependencies: 143

Run `revdep_details(,"AMARETTO")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Running examples in ‘AMARETTO-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: AMARETTO_Download
    > ### Title: AMARETTO_Download
    > ### Aliases: AMARETTO_Download
    > 
    > ### ** Examples
    > 
    > TargetDirectory <- file.path(getwd(),"Downloads/");dir.create(TargetDirectory)
    > CancerSite <- 'CHOL'
    > DataSetDirectories <- AMARETTO_Download(CancerSite,TargetDirectory = TargetDirectory)
    Downloading Gene Expression and Copy Number Variation data for: CHOL
    
    This TCGA cancer site/type was not tested, continue at your own risk.
    
    Error: failed to load resource
      name: EH622
      title: CHOL_RNASeq2GeneNorm-20160128
      reason: attempt to write a readonly database
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.4Mb
      sub-directories of 1Mb or more:
        data   1.5Mb
        doc    2.3Mb
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    License components with restrictions not permitted:
      Apache License (== 2.0) + file LICENSE
    Package listed in more than one of Depends, Imports, Suggests, Enhances:
      ‘knitr’
    A package should be listed in only one of these fields.
    'LinkingTo' field is unused: package has no 'src' directory
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      (/Users/lionel/Desktop/dplyr/revdep/checks.noindex/AMARETTO/new/AMARETTO.Rcheck/00_pkg_src/AMARETTO/R/amaretto_htmlreport.R:273)
    HyperGTestGeneEnrichment: no visible binding for global variable ‘j’
      (/Users/lionel/Desktop/dplyr/revdep/checks.noindex/AMARETTO/new/AMARETTO.Rcheck/00_pkg_src/AMARETTO/R/amaretto_htmlreport.R:273)
    HyperGTestGeneEnrichment: no visible binding for global variable ‘j’
      (/Users/lionel/Desktop/dplyr/revdep/checks.noindex/AMARETTO/new/AMARETTO.Rcheck/00_pkg_src/AMARETTO/R/amaretto_htmlreport.R:275)
    HyperGTestGeneEnrichment: no visible binding for global variable ‘i’
      (/Users/lionel/Desktop/dplyr/revdep/checks.noindex/AMARETTO/new/AMARETTO.Rcheck/00_pkg_src/AMARETTO/R/amaretto_htmlreport.R:279)
    HyperGTestGeneEnrichment: no visible binding for global variable ‘j’
      (/Users/lionel/Desktop/dplyr/revdep/checks.noindex/AMARETTO/new/AMARETTO.Rcheck/00_pkg_src/AMARETTO/R/amaretto_htmlreport.R:279)
    HyperGTestGeneEnrichment: no visible binding for global variable ‘i’
      (/Users/lionel/Desktop/dplyr/revdep/checks.noindex/AMARETTO/new/AMARETTO.Rcheck/00_pkg_src/AMARETTO/R/amaretto_htmlreport.R:282)
    HyperGTestGeneEnrichment: no visible binding for global variable ‘j’
      (/Users/lionel/Desktop/dplyr/revdep/checks.noindex/AMARETTO/new/AMARETTO.Rcheck/00_pkg_src/AMARETTO/R/amaretto_htmlreport.R:282)
    read_gct: no visible binding for global variable ‘Description’
      (/Users/lionel/Desktop/dplyr/revdep/checks.noindex/AMARETTO/new/AMARETTO.Rcheck/00_pkg_src/AMARETTO/R/amaretto_initialize.R:186)
    Undefined global functions or variables:
      Color Description dt_gensesetsall GeneNames Genes Geneset
      Geneset_length i j moduleNr ModuleNr Modules MsigdbMapping
      n_Overlapping NumberGenes overlap_perc Overlapping_genes p_value
      p.value padj q.value RegulatorIDs TargetIDs Testset Type value
      variable Weights
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 152 marked UTF-8 strings
    ```

# bibliometrix

<details>

* Version: 2.3.2
* Source code: https://github.com/cran/bibliometrix
* URL: http://www.bibliometrix.org
* Date/Publication: 2019-11-23 17:20:02 UTC
* Number of recursive dependencies: 143

Run `revdep_details(,"bibliometrix")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘bibliometrix-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plotThematicEvolution
    > ### Title: Plot a Thematic Evolution Analysis
    > ### Aliases: plotThematicEvolution
    > 
    > ### ** Examples
    > 
    > 
    > data(scientometrics)
    > years=c(2000)
    > 
    > nexus <- thematicEvolution(scientometrics,field="ID",years=years,n=100,minFreq=2)
    Error: No such binding: min.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        data   2.0Mb
        doc    1.9Mb
    ```

# comperes

<details>

* Version: 0.2.3
* Source code: https://github.com/cran/comperes
* URL: https://github.com/echasnovski/comperes
* BugReports: https://github.com/echasnovski/comperes/issues
* Date/Publication: 2019-12-14 21:40:03 UTC
* Number of recursive dependencies: 58

Run `revdep_details(,"comperes")` for more info

</details>

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
    Error: No such binding: num_wins.
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      No such binding: num_wins.
      Backtrace:
        1. comperes::h2h_long(cr_data, !!!h2h_funs[c("num_wins", "num_wins2")])
        2. comperes::get_matchups(.)
       10. comperes::summarise_item(., c("player1", "player2"), ...)
        4. dplyr::group_by(., !!!syms(item))
       11. dplyr::summarise(., ...)
       23. dplyr:::summarise_impl(.data, dots, environment(), caller_env())
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 260 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Error: h2h_funs can be used with !!! (@test-head-to-head.R#216) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# congressbr

<details>

* Version: 0.2.2
* Source code: https://github.com/cran/congressbr
* Date/Publication: 2019-12-12 11:20:02 UTC
* Number of recursive dependencies: 111

Run `revdep_details(,"congressbr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘congressbr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: sen_bills
    > ### Title: Downloads and tidies information on the legislation in the
    > ###   Federal Senate
    > ### Aliases: sen_bills
    > 
    > ### ** Examples
    > 
    > pls_5_2010 <- sen_bills(type = "PLS", number = 5, year = 2010)
    Error in status(request) : 
      Request failed. Please check the validity of the information you requested.
    Calls: sen_bills -> status
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked UTF-8 string
    ```

# ddpcr

<details>

* Version: 1.13
* Source code: https://github.com/cran/ddpcr
* URL: https://github.com/daattali/ddpcr
* BugReports: https://github.com/daattali/ddpcr/issues
* Date/Publication: 2020-02-28 07:20:18 UTC
* Number of recursive dependencies: 89

Run `revdep_details(,"ddpcr")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  5.0Mb
      sub-directories of 1Mb or more:
        sample_data   3.0Mb
    ```

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘ddpcr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: get_filled_border
    > ### Title: Get border of filled droplets in PNPP experiment
    > ### Aliases: get_filled_border
    > ### Keywords: internal
    > 
    > ### ** Examples
    > 
    > file <- system.file("sample_data", "small", "analyzed_pnpp.rds", package = "ddpcr")
    > plate <- load_plate(file)
    > get_filled_border(plate, "A05")
    Error: package ‘splines’ does not have a namespace
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       16. base::asNamespace(ns)
       17. base::getNamespace(ns)
       18. base::loadNamespace(name)
       20. base::loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]])
       22. base::loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]])
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 261 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 4 ]
      1. Error: get_empty_cutoff works (@test-empty.R#6) 
      2. Error: get_empty_cutoff for pnpp works (@test-empty.R#20) 
      3. Error: get_filled_border works (@test-pnpp_experiment-filled.R#7) 
      4. Error: get_filled_drops works (@test-pnpp_experiment-filled.R#18) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# descriptr

<details>

* Version: 0.5.1
* Source code: https://github.com/cran/descriptr
* URL: https://descriptr.rsquaredacademy.com/, https://github.com/rsquaredacademy/descriptr
* BugReports: https://github.com/rsquaredacademy/descriptr/issues
* Date/Publication: 2020-02-01 08:20:02 UTC
* Number of recursive dependencies: 115

Run `revdep_details(,"descriptr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘descriptr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ds_measures_location
    > ### Title: Measures of location
    > ### Aliases: ds_measures_location
    > 
    > ### ** Examples
    > 
    > ds_measures_location(mtcarz)
    libc++abi.dylib: terminating with uncaught exception of type Rcpp::no_such_binding: No such binding: values.
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(descriptr)
      > 
      > test_check("descriptr")
      libc++abi.dylib: terminating with uncaught exception of type Rcpp::no_such_binding: No such binding: values.
    ```

# DuoClustering2018

<details>

* Version: 1.4.0
* Source code: https://github.com/cran/DuoClustering2018
* Date/Publication: 2019-11-05
* Number of recursive dependencies: 139

Run `revdep_details(,"DuoClustering2018")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ###   clustering_summary_filteredExpr10_SimKumar8hard_v1
    > ###   clustering_summary_filteredHVG10_SimKumar8hard_v1
    > ###   clustering_summary_filteredM3Drop10_SimKumar8hard_v1
    > ### Keywords: datasets
    > 
    > ### ** Examples
    > 
    > clustering_summary_filteredExpr10_Koh_v1()
    snapshotDate(): 2019-10-22
    see ?DuoClustering2018 and browseVignettes('DuoClustering2018') for documentation
    downloading 1 resources
    retrieving 1 resource
    Warning: download failed
      hub path: ‘https://experimenthub.bioconductor.org/fetch/1548’
      cache resource: ‘EH1548 : 1548’
      reason: attempt to write a readonly database
    Error: failed to load resource
      name: EH1548
      title: clustering_summary_filteredExpr10_Koh_v1
      reason: 1 resources failed to download
    Execution halted
    ```

## Newly fixed

*   R CMD check timed out
    

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
      (/Users/lionel/Desktop/dplyr/revdep/checks.noindex/DuoClustering2018/new/DuoClustering2018.Rcheck/00_pkg_src/DuoClustering2018/R/plot_timing.R:98-112)
    plot_timing: no visible binding for global variable ‘norm.time’
      (/Users/lionel/Desktop/dplyr/revdep/checks.noindex/DuoClustering2018/new/DuoClustering2018.Rcheck/00_pkg_src/DuoClustering2018/R/plot_timing.R:98-112)
    plot_timing: no visible binding for global variable ‘dataset’
      (/Users/lionel/Desktop/dplyr/revdep/checks.noindex/DuoClustering2018/new/DuoClustering2018.Rcheck/00_pkg_src/DuoClustering2018/R/plot_timing.R:115-128)
    plot_timing: no visible binding for global variable ‘filtering’
      (/Users/lionel/Desktop/dplyr/revdep/checks.noindex/DuoClustering2018/new/DuoClustering2018.Rcheck/00_pkg_src/DuoClustering2018/R/plot_timing.R:115-128)
    plot_timing: no visible binding for global variable ‘method’
      (/Users/lionel/Desktop/dplyr/revdep/checks.noindex/DuoClustering2018/new/DuoClustering2018.Rcheck/00_pkg_src/DuoClustering2018/R/plot_timing.R:115-128)
    plot_timing: no visible binding for global variable ‘k’
      (/Users/lionel/Desktop/dplyr/revdep/checks.noindex/DuoClustering2018/new/DuoClustering2018.Rcheck/00_pkg_src/DuoClustering2018/R/plot_timing.R:115-128)
    plot_timing: no visible binding for global variable ‘elapsed’
      (/Users/lionel/Desktop/dplyr/revdep/checks.noindex/DuoClustering2018/new/DuoClustering2018.Rcheck/00_pkg_src/DuoClustering2018/R/plot_timing.R:115-128)
    plot_timing: no visible binding for global variable ‘medianelapsed’
      (/Users/lionel/Desktop/dplyr/revdep/checks.noindex/DuoClustering2018/new/DuoClustering2018.Rcheck/00_pkg_src/DuoClustering2018/R/plot_timing.R:115-128)
    Undefined global functions or variables:
      ARI ari.stab cell cluster data.wide dataset ds ds.norm elapsed
      entropy est_k estnclust filtering k k_diff med.t medARI
      median.elapsed median.stability medianARI medianelapsed method
      norm.time run s s.norm s.true s.true.norm sce stability trueclass
      truenclust
    ```

# Lahman

<details>

* Version: 7.0-1
* Source code: https://github.com/cran/Lahman
* URL: http://lahman.r-forge.r-project.org/
* BugReports: https://github.com/cdalzell/Lahman/issues
* Date/Publication: 2019-05-02 09:30:03 UTC
* Number of recursive dependencies: 92

Run `revdep_details(,"Lahman")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > 
    > ## Yogi's career efficiency in throwing out base stealers 
    > ## in his WS appearances and CS as a percentage of his 
    > ## overall assists
    > FieldingPost %>%
    +   filter(playerID == "berrayo01" & round == "WS" & POS == "C") %>%
    +   summarise(cs_pct = round(100 * sum(CS)/sum(SB + CS), 2),
    +             cs_assists = round(100 * sum(CS)/sum(A), 2))
      cs_pct cs_assists
    1     NA         NA
    > 
    > ## Innings per error for several selected shortstops in the WS
    > FieldingPost %>%
    +   filter(playerID %in% c("belanma01", "jeterde01", "campabe01",
    +                          "conceda01", "bowala01"), round == "WS") %>%
    +   group_by(playerID) %>%
    +   summarise(G = sum(G),
    +             InnOuts = sum(InnOuts),
    +             Eper9 = round(27 * sum(E)/sum(InnOuts), 3))
    Error: No such binding: round.
    Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘zipcode’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 10.1Mb
      sub-directories of 1Mb or more:
        data   9.4Mb
    ```

# MachineLearning

<details>

* Version: 0.1.3
* Source code: https://github.com/cran/MachineLearning
* URL: https://datascienceumh.github.io/MachineLearning/
* Date/Publication: 2019-03-15 16:23:37 UTC
* Number of recursive dependencies: 77

Run `revdep_details(,"MachineLearning")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      3     motivo 0.004326729
        attributes  importance
      1       pais 0.049161047
      2      aloja 0.008279454
      3     motivo 0.004326729
        attributes  importance
      1       pais 0.049161047
      2      aloja 0.008279454
      3     motivo 0.004326729
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 23 | SKIPPED: 0 | WARNINGS: 1 | FAILED: 1 ]
      1. Error: Test CART with EGATUR dataset. (@test-CART.R#14) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# metaplot

<details>

* Version: 0.8.3
* Source code: https://github.com/cran/metaplot
* Date/Publication: 2019-04-25 22:52:09 UTC
* Number of recursive dependencies: 47

Run `revdep_details(,"metaplot")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### ** Examples
    > 
    > 
    > library(magrittr)
    > library(dplyr)
    
    Attaching package: ‘dplyr’
    
    The following objects are masked from ‘package:stats’:
    
        filter, lag
    
    The following objects are masked from ‘package:base’:
    
        intersect, setdiff, setequal, union
    
    > library(csv)
    > x <- as.csv(system.file(package = 'metaplot', 'extdata/theoph.csv'))
    > x %<>% pack
    > x %>% metaplot(site)
    libc++abi.dylib: terminating with uncaught exception of type Rcpp::no_such_binding: No such binding: n.
    ```

# mvMonitoring

<details>

* Version: 0.1.0
* Source code: https://github.com/cran/mvMonitoring
* URL: https://github.com/gabrielodom/mvMonitoring
* Date/Publication: 2017-10-20 08:55:10 UTC
* Number of recursive dependencies: 43

Run `revdep_details(,"mvMonitoring")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        data   2.1Mb
    ```

# plotly

<details>

* Version: 4.9.2
* Source code: https://github.com/cran/plotly
* URL: https://plotly-r.com, https://github.com/ropensci/plotly#readme, https://plot.ly/r
* BugReports: https://github.com/ropensci/plotly/issues
* Date/Publication: 2020-02-12 18:50:02 UTC
* Number of recursive dependencies: 152

Run `revdep_details(,"plotly")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
          last_plot
      
      The following object is masked from 'package:stats':
      
          filter
      
      The following object is masked from 'package:graphics':
      
          layout
      
      > 
      > test_check("plotly")
      Visual testing is not enabled.
      OGR: Unsupported geometry type
      libc++abi.dylib: terminating with uncaught exception of type Rcpp::no_such_binding: No such binding: median.
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.9Mb
      sub-directories of 1Mb or more:
        htmlwidgets   3.7Mb
        R             1.2Mb
    ```

# qicharts2

<details>

* Version: 0.6.0
* Source code: https://github.com/cran/qicharts2
* URL: https://github.com/anhoej/qicharts2
* Date/Publication: 2019-03-16 20:40:03 UTC
* Number of recursive dependencies: 68

Run `revdep_details(,"qicharts2")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > # Generate vector of 24 random normal numbers
    > y <- rnorm(24)
    > 
    > # Run chart
    > qic(y)
    > 
    > # I control chart
    > qic(y, chart = 'i')
    > 
    > # U control chart from build-in data set of hospital infection rates faceted
    > #   by hospital and type of infection.
    > qic(month, n, 
    +     n        = days,
    +     data     = hospital_infections,
    +     facets   = infection ~ hospital,
    +     chart    = 'u',
    +     multiply = 10000,
    +     title     = 'Hospital infection rates',
    +     ylab     = 'Number of infections per 10.000 risk days',
    +     xlab     = 'Month')
    libc++abi.dylib: terminating with uncaught exception of type Rcpp::no_such_binding: No such binding: n.
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > Sys.setenv("R_TESTS" = "")
      > library(testthat)
      > library(qicharts2)
      > 
      > test_check("qicharts2")
      libc++abi.dylib: terminating with uncaught exception of type Rcpp::no_such_binding: No such binding: n.
    ```

# sismonr

<details>

* Version: 2.1.0
* Source code: https://github.com/cran/sismonr
* URL: https://oliviaab.github.io/sismonr/
* BugReports: https://github.com/oliviaAB/sismonr/issues
* Date/Publication: 2020-02-11 06:50:02 UTC
* Number of recursive dependencies: 81

Run `revdep_details(,"sismonr")` for more info

</details>

## Newly broken

*   checking for code/documentation mismatches ... WARNING
    ```
    ┌ Warning: `BioSimulator` not in project, ignoring
    └ @ Pkg.Operations /Users/osx/buildbot/slave/package_osx64/build/usr/share/julia/stdlib/v1.1/Pkg/src/Operations.jl:1131
    [ Info: No changes
    
    ```

## Newly fixed

*   checking S3 generic/method consistency ... WARNING
    ```
    ┌ Warning: `BioSimulator` not in project, ignoring
    └ @ Pkg.Operations /Users/osx/buildbot/slave/package_osx64/build/usr/share/julia/stdlib/v1.1/Pkg/src/Operations.jl:1131
    [ Info: No changes
    See section ‘Generic functions and methods’ in the ‘Writing R Extensions’
    manual.
    ```

*   checking replacement functions ... WARNING
    ```
    ┌ Warning: `BioSimulator` not in project, ignoring
    └ @ Pkg.Operations /Users/osx/buildbot/slave/package_osx64/build/usr/share/julia/stdlib/v1.1/Pkg/src/Operations.jl:1131
    [ Info: No changes
    The argument of a replacement function which corresponds to the right hand side
    must be named ‘value’.
    ```

*   checking dependencies in R code ... NOTE
    ```
    ┌ Warning: `BioSimulator` not in project, ignoring
    └ @ Pkg.Operations /Users/osx/buildbot/slave/package_osx64/build/usr/share/julia/stdlib/v1.1/Pkg/src/Operations.jl:1131
    [ Info: No changes
    ```

*   checking foreign function calls ... NOTE
    ```
    ┌ Warning: `BioSimulator` not in project, ignoring
    └ @ Pkg.Operations /Users/osx/buildbot/slave/package_osx64/build/usr/share/julia/stdlib/v1.1/Pkg/src/Operations.jl:1131
    [ Info: No changes
    See chapter ‘System and foreign language interfaces’ in the ‘Writing R
    Extensions’ manual.
    ```

## In both

*   R CMD check timed out
    

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘tcltk’
    ```

# ushr

<details>

* Version: 0.2.1
* Source code: https://github.com/cran/ushr
* URL: https://github.com/SineadMorris/ushr
* Date/Publication: 2020-02-20 17:50:02 UTC
* Number of recursive dependencies: 72

Run `revdep_details(,"ushr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ushr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: filter_data
    > ### Title: Prepare input data
    > ### Aliases: filter_data
    > 
    > ### ** Examples
    > 
    > 
    > set.seed(1234567)
    > 
    > simulated_data <- simulate_data(nsubjects = 20)
    Joining, by = "id"
    > 
    > filter_data(simulated_data)
    libc++abi.dylib: terminating with uncaught exception of type Rcpp::no_such_binding: No such binding: n.
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
          intersect, setdiff, setequal, union
      
      Loading required package: tidyr
      
      Attaching package: 'tidyr'
      
      The following object is masked from 'package:testthat':
      
          matches
      
      Loading required package: ggplot2
      > 
      > test_check("ushr")
      libc++abi.dylib: terminating with uncaught exception of type Rcpp::no_such_binding: No such binding: n.
    ```

