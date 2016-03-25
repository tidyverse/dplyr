# Setup

## Platform

|setting  |value                        |
|:--------|:----------------------------|
|version  |R version 3.2.4 (2016-03-10) |
|system   |x86_64, darwin13.4.0         |
|ui       |X11                          |
|language |(EN)                         |
|collate  |en_US.UTF-8                  |
|tz       |America/Chicago              |
|date     |2016-03-25                   |

## Packages

|package        |*  |version     |date       |source                           |
|:--------------|:--|:-----------|:----------|:--------------------------------|
|assertthat     |   |0.1         |2013-12-06 |CRAN (R 3.2.0)                   |
|BH             |   |1.60.0-1    |2015-12-28 |CRAN (R 3.2.3)                   |
|covr           |   |1.2.0       |2015-06-25 |CRAN (R 3.2.0)                   |
|DBI            |   |0.3.1       |2014-09-24 |CRAN (R 3.2.0)                   |
|dplyr          |   |0.4.3.9001  |2016-03-25 |local (hadley/dplyr)             |
|ggplot2        |   |2.1.0       |2016-03-01 |CRAN (R 3.2.4)                   |
|knitr          |   |1.12.3      |2016-01-22 |CRAN (R 3.2.3)                   |
|Lahman         |   |4.0-1       |2015-09-15 |CRAN (R 3.2.0)                   |
|lazyeval       |   |0.1.10      |2015-01-02 |CRAN (R 3.2.0)                   |
|magrittr       |   |1.5         |2014-11-22 |CRAN (R 3.2.0)                   |
|microbenchmark |   |1.4-2.1     |2015-11-25 |CRAN (R 3.2.2)                   |
|nycflights13   |   |0.1         |2014-07-22 |CRAN (R 3.2.0)                   |
|R6             |   |2.1.2       |2016-01-26 |CRAN (R 3.2.3)                   |
|Rcpp           |   |0.12.3      |2016-01-10 |CRAN (R 3.2.3)                   |
|rmarkdown      |   |0.9.5       |2016-02-22 |CRAN (R 3.2.3)                   |
|RMySQL         |   |0.10.8      |2016-01-29 |CRAN (R 3.2.3)                   |
|RPostgreSQL    |   |0.4         |2013-03-27 |CRAN (R 3.2.0)                   |
|RSQLite        |   |1.0.0       |2014-10-25 |CRAN (R 3.2.0)                   |
|testthat       |*  |0.11.0.9000 |2016-03-25 |Github (hadley/testthat@94fc45a) |
|tibble         |   |1.0         |2016-03-25 |Github (hadley/tibble@69b9a89)   |

# Check results
234 packages

## ACDm (1.0.3)
Maintainer: Markus Belfrage <markus.belfrage@gmail.com>

0 errors | 0 warnings | 0 notes

## adegenet (2.0.1)
Maintainer: Thibaut Jombart <thibautjombart@gmail.com>

0 errors | 0 warnings | 0 notes

## ADPclust (0.6.5)
Maintainer: Yifan "Ethan" Xu <ethan.yifanxu@gmail.com>  
Bug reports: https://github.com/ethanyxu/ADPclust/issues

0 errors | 0 warnings | 0 notes

## aemo (0.1.0)
Maintainer: Imanuel Costigan <i.costigan@me.com>

1 error  | 0 warnings | 0 notes

```
checking whether package ‘aemo’ can be installed ... ERROR
Installation failed.
See ‘/Users/hadley/Documents/dplyr/dplyr/revdep/checks/aemo.Rcheck/00install.out’ for details.
```

## afex (0.15-2)
Maintainer: Henrik Singmann <singmann+afex@gmail.com>

0 errors | 0 warnings | 1 note 

```
checking Rd cross-references ... NOTE
Package unavailable to check Rd xrefs: ‘ez’
```

## alakazam (0.2.3)
Maintainer: Jason Vander Heiden <jason.vanderheiden@yale.edu>  
Bug reports: https://bitbucket.org/kleinstein/alakazam/issues

0 errors | 0 warnings | 1 note 

```
checking re-building of vignette outputs ... NOTE
Error in re-building vignettes:
  ...
Loading required package: ggplot2
Quitting from lines 78-109 (AminoAcids-Vignette.Rmd) 
Error: processing vignette 'AminoAcids-Vignette.Rmd' failed with diagnostics:
could not find function "starts_with"
Execution halted

```

## ameco (0.2.2)
Maintainer: Eric Persson <expersso5@gmail.com>  
Bug reports: http://github.com/expersso/ameco/issues

0 errors | 0 warnings | 1 note 

```
checking installed package size ... NOTE
  installed size is 15.3Mb
  sub-directories of 1Mb or more:
    data  15.2Mb
```

## archivist (2.0.3)
Maintainer: Przemyslaw Biecek <przemyslaw.biecek@gmail.com>  
Bug reports: https://github.com/pbiecek/archivist/issues

0 errors | 0 warnings | 0 notes

## ARTool (0.10.0)
Maintainer: Matthew Kay <mjskay@uw.edu>  
Bug reports: https://github.com/mjskay/ARTool/issues/new

0 errors | 0 warnings | 0 notes

## assertive.types (0.0-2)
Maintainer: Richard Cotton <richierocks@gmail.com>  
Bug reports: https://bitbucket.org/richierocks/assertive.types/issues

0 errors | 0 warnings | 0 notes

## assertr (1.0.0)
Maintainer: Tony Fischetti <tony.fischetti@gmail.com>  
Bug reports: https://github.com/tonyfischetti/assertr/issues

2 errors | 0 warnings | 0 notes

```
checking examples ... ERROR
Running examples in ‘assertr-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: maha_dist
> ### Title: Computes mahalanobis distance for each row of data frame
> ### Aliases: maha_dist
> 
> ### ** Examples
... 33 lines ...
> 
> library(magrittr)            # for piping operator
> 
> # using every column from mtcars, compute mahalanobis distance
> # for each observation, and ensure that each distance is within 10
> # median absolute deviations from the median
> mtcars %>%
+   insist_rows(maha_dist, within_n_mads(10), everything())
Error in eval(expr, envir, enclos) : could not find function "everything"
Calls: %>% ... select_vars_ -> <Anonymous> -> lapply -> FUN -> eval -> eval
Execution halted

checking tests ... ERROR
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  1.  Error: assert_rows returns data if verification passes (@test-assertions.R#189) 
  2.  Failure: assert_rows raises error if verification fails (@test-assertions.R#217) 
  3.  Failure: assert_rows raises error if verification fails (@test-assertions.R#221) 
  4.  Failure: assert_rows raises *custom error* if verification fails (@test-assertions.R#237) 
  5.  Failure: assert_rows raises *custom error* if verification fails (@test-assertions.R#241) 
  6.  Error: insist_rows returns data if verification passes (@test-assertions.R#377) 
  7.  Failure: insist_rows raises error if verification fails (@test-assertions.R#391) 
  8.  Failure: insist_rows raises error if verification fails (@test-assertions.R#393) 
  9.  Failure: insist_rows raises *custom error* if verification fails (@test-assertions.R#414) 
  10. Failure: insist_rows raises *custom error* if verification fails (@test-assertions.R#416) 
  
  Error: testthat unit tests failed
  Execution halted
```

## AutoModel (0.4.9)
Maintainer: Alex Lishinski <alexlishinski@gmail.com>

0 errors | 0 warnings | 0 notes

## backtestGraphics (0.1.6)
Maintainer: Miller Zijie Zhu <zijie.miller.zhu@gmail.com>

0 errors | 0 warnings | 0 notes

## bayesGDS (0.6.2)
Maintainer: Michael Braun <braunm@smu.edu>

0 errors | 0 warnings | 0 notes

## bigrquery (0.2.0)
Maintainer: Hadley Wickham <hadley@rstudio.com>

1 error  | 0 warnings | 0 notes

```
checking whether package ‘bigrquery’ can be installed ... ERROR
Installation failed.
See ‘/Users/hadley/Documents/dplyr/dplyr/revdep/checks/bigrquery.Rcheck/00install.out’ for details.
```

## binomen (0.1.0)
Maintainer: Scott Chamberlain <myrmecocystus@gmail.com>  
Bug reports: https://github.com/ropensci/binomen/issues

0 errors | 0 warnings | 0 notes

## bioinactivation (1.1.2)
Maintainer: Alberto Garre <garre.alberto@gmail.com>

0 errors | 0 warnings | 0 notes

## biomartr (0.0.3)
Maintainer: Hajk-Georg Drost <hgd23@cam.ac.uk>  
Bug reports: https://github.com/HajkD/biomartr/issues

1 error  | 0 warnings | 0 notes

```
checking package dependencies ... ERROR
Packages required but not available: ‘biomaRt’ ‘Biostrings’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```

## bodenmiller (0.1)
Maintainer: Yann Abraham <yann.abraham@gmail.com>  
Bug reports: https://github.com/yannabraham/bodenmiller/issues

0 errors | 0 warnings | 1 note 

```
checking installed package size ... NOTE
  installed size is  8.9Mb
  sub-directories of 1Mb or more:
    data   8.7Mb
```

## bootnet (0.2)
Maintainer: Sacha Epskamp <mail@sachaepskamp.com>

0 errors | 0 warnings | 0 notes

## boxr (0.3.2)
Maintainer: Brendan Rocks <rocks.brendan@gmail.com>  
Bug reports: https://github.com/brendan-R/boxr/issues

0 errors | 0 warnings | 0 notes

## broom (0.4.0)
Maintainer: David Robinson <admiral.david@gmail.com>  
Bug reports: http://github.com/dgrtwo/broom/issues

1 error  | 0 warnings | 0 notes

```
checking tests ... ERROR
Running the tests in ‘tests/test-all.R’ failed.
Last 13 lines of output:
  3: as.expectation(exp, ..., srcref = srcref) at /private/tmp/Rtmp0b9LfI/devtoolscb0c76082acf/hadley-testthat-94fc45a/R/expectation.R:29
  4: identical(as.vector(object), TRUE) at /private/tmp/Rtmp0b9LfI/devtoolscb0c76082acf/hadley-testthat-94fc45a/R/expectation.R:103
  5: as.vector(object)
  6: augmented$disp
  7: `$.tbl_df`(augmented, disp)
  8: stop("Unknown column '", i, "'", call. = FALSE) at /private/tmp/Rtmp0b9LfI/devtoolscb0c1fd66020/hadley-tibble-69b9a89/R/tbl-df.r:67
  
  testthat results ================================================================
  OK: 490 SKIPPED: 0 FAILED: 1
  1. Error: rowwise tidiers can be applied to sub-models (@test-rowwise.R#21) 
  
  Error: testthat unit tests failed
  Execution halted
```

## causaldrf (0.3)
Maintainer: Douglas Galagate <galagated@gmail.com>

0 errors | 0 warnings | 0 notes

## cbsodataR (0.2.1)
Maintainer: Edwin de Jonge <edwindjonge@gmail.com>  
Bug reports: https://github.com/edwindj/cbsodataR/issues

0 errors | 0 warnings | 0 notes

## cdcfluview (0.4.0)
Maintainer: Bob Rudis <bob@rudis.net>  
Bug reports: https://github.com/hrbrmstr/cdcfluview/issues

0 errors | 0 warnings | 0 notes

## censusr (0.0.2)
Maintainer: Greg Macfarlane <gregmacfarlane@gmail.com>

0 errors | 0 warnings | 0 notes

## checkmate (1.7.3)
Maintainer: Michel Lang <michellang@gmail.com>  
Bug reports: https://github.com/mllg/checkmate/issues

0 errors | 0 warnings | 0 notes

## choroplethr (3.5.0)
Maintainer: Ari Lamstein <arilamstein@gmail.com>  
Bug reports: https://github.com/arilamstein/choroplethr/issues

0 errors | 0 warnings | 0 notes

## chromer (0.1)
Maintainer: Matthew Pennell <mwpennell@gmail.com>  
Bug reports: http://www.github.com/ropensci/chromer/issues/new

0 errors | 0 warnings | 1 note 

```
checking DESCRIPTION meta-information ... NOTE
Malformed Description field: should contain one or more complete sentences.
```

## chunked (0.2.0)
Maintainer: Edwin de Jonge <edwindjonge@gmail.com>  
Bug reports: https://github.com/edwindj/chunked/issues

1 error  | 0 warnings | 0 notes

```
checking tests ... ERROR
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  2: write_chunkwise.tbl_sql(iris2, tmp, row.names = FALSE)
  
  testthat results ================================================================
  OK: 23 SKIPPED: 0 FAILED: 6
  1. Failure: print: should print chunked info (@test-print.R#8) 
  2. Failure: print groups: should print groups chunked info (@test-print.R#15) 
  3. Failure: print groups: should print groups chunked info (@test-print.R#15) 
  4. Failure: print groups: should print groups chunked info (@test-print.R#16) 
  5. Failure: print groups: should print groups chunked info (@test-print.R#16) 
  6. Error: write_chunkwise to db works (@test-write.R#29) 
  
  Error: testthat unit tests failed
  Execution halted
```

## codingMatrices (0.2.0)
Maintainer: Bill Venables <Bill.Venables@gmail.com>

0 errors | 0 warnings | 0 notes

## codyn (1.0.1)
Maintainer: Matthew B. Jones <jones@nceas.ucsb.edu>  
Bug reports: https://github.com/laurenmh/codyn/issues

0 errors | 0 warnings | 0 notes

## cofeatureR (1.0.1)
Maintainer: Fong Chun Chan <fongchunchan@gmail.com>  
Bug reports: https://github.com/tinyheero/cofeatureR/issues

1 error  | 0 warnings | 0 notes

```
checking examples ... ERROR
Running examples in ‘cofeatureR-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: plot_cofeature_mat
> ### Title: Plot a Cofeature Matrix
> ### Aliases: plot_cofeature_mat
> 
> ### ** Examples
... 47 lines ...
Detected no type.order. Specifying type.order
Setting feature order
Setting sample order
> 
> # Specify each cell can only have one "feature type"
> plot_cofeature_mat(in.df, feature.order, sample.id.order, fill.colors = fill.colors,
+   type.display.mode = "single")
Detected no type.order. Specifying type.order
Using type.display.mode single
Error: No variables selected
Execution halted
```

## cometExactTest (0.1.3)
Maintainer: Max Leiserson <mdml@cs.brown.edu>

0 errors | 0 warnings | 0 notes

## compareDF (1.0.0)
Maintainer: Alex Joseph <alexsanjoseph@gmail.com>

0 errors | 0 warnings | 0 notes

## condformat (0.2.0)
Maintainer: Sergio Oller Moreno <sergioller@gmail.com>  
Bug reports: http://github.com/zeehio/condformat/issues

2 errors | 0 warnings | 0 notes

```
checking examples ... ERROR
Running examples in ‘condformat-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: show_columns
> ### Title: Selects the variables to be printed
> ### Aliases: show_columns show_columns_
> 
> ### ** Examples
... 94 lines ...
<td style='border-bottom: 2px solid grey; text-align: left;'>6</td>
<td style='border-bottom: 2px solid grey; text-align: center;'>5.4</td>
<td style='border-bottom: 2px solid grey; text-align: center;'>3.9</td>
<td style='border-bottom: 2px solid grey; text-align: center;'>setosa</td>
</tr>
</tbody>
</table>> condformat(x) + show_columns(starts_with("Petal"), Species)
Error in eval(expr, envir, enclos) : 
  could not find function "starts_with"
Calls: <Anonymous> ... <Anonymous> -> <Anonymous> -> lapply -> FUN -> eval -> eval
Execution halted

checking tests ... ERROR
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  1. Error: knitr returns an HTML table (@test_rendering.R#18) 
  2. Error: rule_fill_discrete works (@test_rule_fill_discrete.R#14) 
  3. Error: rule_fill_discrete lock cells (@test_rule_fill_discrete.R#39) 
  4. Error: rule_fill_discrete_ works (@test_rule_fill_discrete.R#67) 
  5. Error: rule_fill_gradient works (@test_rule_fill_gradient.R#12) 
  6. Error: rule_fill_gradient2 works (@test_rule_fill_gradient.R#24) 
  7. Error: show_column works (@test_show.R#12) 
  8. Error: show_column_ works (@test_show.R#24) 
  9. Error: show_column works with custom names (@test_show.R#36) 
  1. ...
  
  Error: testthat unit tests failed
  Execution halted
```

## crawl (2.0)
Maintainer: Devin S. Johnson <devin.johnson@noaa.gov>

0 errors | 0 warnings | 0 notes

## cricketr (0.0.12)
Maintainer: Tinniam V Ganesh <tvganesh.85@gmail.com>  
Bug reports: https://github.com/tvganesh/cricketr/issues

0 errors | 0 warnings | 0 notes

## datacheckr (0.1.1)
Maintainer: Joe Thorley <joe@poissonconsulting.ca>

0 errors | 0 warnings | 0 notes

## DataCombine (0.2.19)
Maintainer: Christopher Gandrud <christopher.gandrud@gmail.com>  
Bug reports: https://github.com/christophergandrud/DataCombine/issues

1 error  | 0 warnings | 0 notes

```
checking examples ... ERROR
Running examples in ‘DataCombine-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: slide
> ### Title: A function for creating lag and lead variables, including for
> ###   time-series cross-sectional data.
> ### Aliases: slide
> 
... 37 lines ...
  No valid lag/lead can be created.
  NA will be returned for these observations in the new lag/lead variable.
  They will be returned at the bottom of the data frame.

4


Error in UseMethod("ungroup") : 
  no applicable method for 'ungroup' applied to an object of class "c('matrix', 'list')"
Calls: slide -> ungroup
Execution halted
```

## datadr (0.8.5)
Maintainer: Ryan Hafen <rhafen@gmail.com>

0 errors | 0 warnings | 1 note 

```
checking package dependencies ... NOTE
Package suggested but not available for checking: ‘Rhipe’
```

## dataonderivatives (0.2.1)
Maintainer: Imanuel Costigan <i.costigan@me.com>  
Bug reports: https://github.com/imanuelcostigan/dataonderivatives/issues

0 errors | 0 warnings | 0 notes

## dataRetrieval (2.5.2)
Maintainer: Laura DeCicco <ldecicco@usgs.gov>  
Bug reports: https://github.com/USGS-R/dataRetrieval/issues

0 errors | 0 warnings | 0 notes

## datastepr (0.0.1)
Maintainer: Brandon Taylor <brandon.taylor221@gmail.com>

0 errors | 0 warnings | 0 notes

## ddpcr (1.1.2)
Maintainer: Dean Attali <daattali@gmail.com>  
Bug reports: https://github.com/daattali/ddpcr/issues

2 errors | 0 warnings | 1 note 

```
checking examples ... ERROR
Running examples in ‘ddpcr-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: calculate_negative_freqs
> ### Title: Calculate negative frequencies in whole plate
> ### Aliases: calculate_negative_freqs
> ### Keywords: internal
> 
> ### ** Examples
> 
> file <- system.file("sample_data", "small", "analyzed_pnpp.rds", package = "ddpcr")
> plate <- load_plate(file)
> plate %>% calculate_negative_freqs %>%
+   well_info(wells_success(plate), "negative_freq")
Error in eval(expr, envir, enclos) : could not find function "one_of"
Calls: %>% ... select_vars_ -> <Anonymous> -> lapply -> FUN -> eval -> eval
Execution halted

checking tests ... ERROR
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  
  Attaching package: 'ddpcr'
  
  The following object is masked from 'package:stats':
  
      step
  
  > 
  > test_check("ddpcr")
  Error: ddpcr: there was a problem reading one or more of the data files
  testthat results ================================================================
  OK: 0 SKIPPED: 0 FAILED: 0
  Execution halted

checking re-building of vignette outputs ... NOTE
Error in re-building vignettes:
  ...
Quitting from lines 205-213 (extend.Rmd) 
Error: processing vignette 'extend.Rmd' failed with diagnostics:
ddpcr: there was a problem reading one or more of the data files
Execution halted

```

## denovolyzeR (0.1.0)
Maintainer: James Ware <j.ware@imperial.ac.uk>  
Bug reports: http://github.com/jamesware/denovolyzeR/issues

1 error  | 0 warnings | 1 note 

```
checking examples ... ERROR
Running examples in ‘denovolyzeR-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: denovolyze
> ### Title: Evaluates burden of _de novo_ variation against expectation
> ### Aliases: denovolyze denovolyzeByClass denovolyzeByGene
> 
> ### ** Examples
> 
> ### denovolyze
> 
> denovolyze(genes=autismDeNovos$gene,
+            classes=autismDeNovos$class,
+            nsamples=1078)
Error: Unknown column 'obs'
Execution halted

checking re-building of vignette outputs ... NOTE
Error in re-building vignettes:
  ...
Quitting from lines 52-55 (denovolyzeR_intro.Rmd) 
Error: processing vignette 'denovolyzeR_intro.Rmd' failed with diagnostics:
Unknown column 'obs'
Execution halted

```

## DepthProc (1.0.7)
Maintainer: Zygmunt Zawadzki <zawadzkizygmunt@gmail.com>

0 errors | 0 warnings | 0 notes

## describer (0.2.0)
Maintainer: Paul Hendricks <paul.hendricks.2013@owu.edu>  
Bug reports: https://github.com/paulhendricks/describer/issues

1 error  | 0 warnings | 0 notes

```
checking tests ... ERROR
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  'arg' must be NULL or a character vector
  1: lintr::expect_lint_free() at testthat/test-styling.R:4
  2: testthat::expectation(!has_lints, paste(sep = "\n", "Not lint free", lint_output), 
         "lint free")
  3: match.arg(type, c("success", "failure", "error", "skip", "warning")) at /private/tmp/Rtmp0b9LfI/devtoolscb0c76082acf/hadley-testthat-94fc45a/R/expectation.R:13
  4: stop("'arg' must be NULL or a character vector")
  
  testthat results ================================================================
  OK: 7 SKIPPED: 0 FAILED: 1
  1. Error: Package Style (@test-styling.R#4) 
  
  Error: testthat unit tests failed
  Execution halted
```

## DisimForMixed (0.1)
Maintainer: Hasanthi A. Pathberiya <hasaanu@gmail.com>

0 errors | 0 warnings | 0 notes

## DiversityOccupancy (1.0.2)
Maintainer: Derek Corcoran <derek.corcoran.barrios@gmail.com>

1 error  | 0 warnings | 0 notes

```
checking whether package ‘DiversityOccupancy’ can be installed ... ERROR
Installation failed.
See ‘/Users/hadley/Documents/dplyr/dplyr/revdep/checks/DiversityOccupancy.Rcheck/00install.out’ for details.
```

## docxtractr (0.1.0.9000)
Maintainer: Bob Rudis <bob@rudis.net>

0 errors | 0 warnings | 0 notes

## dotwhisker (0.2.0.5)
Maintainer: Yue Hu <yue-hu-1@uiowa.edu>  
Bug reports: https://github.com/fsolt/dotwhisker/issues

1 error  | 0 warnings | 2 notes

```
checking examples ... ERROR
Running examples in ‘dotwhisker-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: small_multiple
> ### Title: Generate a 'Small Multiple' Plot of Regression Results
> ### Aliases: small_multiple
> 
> ### ** Examples
... 55 lines ...
+     theme(axis.text.x  = element_text(angle = 45, hjust = 1),
+           legend.position=c(0, 0), legend.justification=c(0, 0),
+           legend.title = element_text(size=9),
+           legend.background = element_rect(color="gray90"),
+           legend.margin = unit(-3, "pt"),
+           legend.key.size = unit(10, "pt")) +
+     scale_colour_hue(name = "Transmission",
+     breaks = c(0, 1),
+     labels = c("Automatic", "Manual"))
Error: cannot convert object to a data frame
Execution halted

checking dependencies in R code ... NOTE
Namespace in Imports field not imported from: ‘gridExtra’
  All declared Imports should be used.

checking re-building of vignette outputs ... NOTE
Error in re-building vignettes:
  ...
Warning: Deprecated, use tibble::rownames_to_column() instead.
Loading required package: sandwich
Loading required package: lmtest
Loading required package: zoo

Attaching package: 'zoo'

... 8 lines ...

The following object is masked from 'package:dplyr':

    select

Loading required package: betareg
Warning: Deprecated, use tibble::rownames_to_column() instead.
Quitting from lines 168-211 (dotwhisker-vignette.Rmd) 
Error: processing vignette 'dotwhisker-vignette.Rmd' failed with diagnostics:
no applicable method for 'group_by_' applied to an object of class "c('matrix', 'list')"
Execution halted
```

## ecb (0.2)
Maintainer: Eric Persson <expersso5@gmail.com>

0 errors | 0 warnings | 0 notes

## ecoengine (1.9.1)
Maintainer: Karthik Ram <karthik.ram@gmail.com>  
Bug reports: https://github.com/ropensci/ecoengine/issues

0 errors | 0 warnings | 0 notes

## edeaR (0.3.2)
Maintainer: Gert Janssenswillen <gert.janssenswillen@uhasselt.be>

0 errors | 0 warnings | 1 note 

```
checking re-building of vignette outputs ... NOTE
Error in re-building vignettes:
  ...

Attaching package: 'edeaR'

The following object is masked from 'package:utils':

    timestamp

Quitting from lines 92-94 (descriptives.Rmd) 
Error: processing vignette 'descriptives.Rmd' failed with diagnostics:
non-numeric argument to binary operator
Execution halted

```

## eemR (0.1.2)
Maintainer: Philippe Massicotte <pm@bios.au.dk>  
Bug reports: https://github.com/PMassicotte/eemR/issues

0 errors | 0 warnings | 0 notes

## EFDR (0.1.1)
Maintainer: Andrew Zammit-Mangion <andrewzm@gmail.com>

0 errors | 0 warnings | 0 notes

## efreadr (0.1.1)
Maintainer: Marco Bascietto <marco.bascietto@crea.gov.it>

0 errors | 0 warnings | 0 notes

## emil (2.2.3)
Maintainer: Christofer Backlin <emil@christofer.backlin.se>  
Bug reports: https://github.com/Molmed/emil/issues

0 errors | 1 warning  | 0 notes

```
checking Rd cross-references ... WARNING
Missing link or links in documentation object 'pre_process.Rd':
  ‘chain’

See section 'Cross-references' in the 'Writing R Extensions' manual.

```

## emuR (0.1.6)
Maintainer: Raphael Winkelmann <raphael@phonetik.uni-muenchen.de>  
Bug reports: https://github.com/IPS-LMU/emuR/issues

1 error  | 0 warnings | 1 note 

```
checking tests ... ERROR
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  Read 16 items
  Read 12 records
  Read 24 items
  Read 20 records
  testthat results ================================================================
  OK: 683 SKIPPED: 2 FAILED: 4
  1. Error: correct emuDB is created (@test_emuR-convert_TextGridCollection.R#39) 
  2. Error: only specified tiers are converted when tierNames is set (@test_emuR-convert_TextGridCollection.R#116) 
  3. Error: test that correct values are set for msajc003 (@test_emuR-create_DBconfigFromTextGrid.R#17) 
  4. Error: test only correct tiers are extracted if tierNames is set (@test_emuR-create_DBconfigFromTextGrid.R#32) 
  
  Error: testthat unit tests failed
  Execution halted

checking re-building of vignette outputs ... NOTE
Error in re-building vignettes:
  ...

Attaching package: 'emuR'

The following object is masked from 'package:base':

    norm

Quitting from lines 397-406 (emuDB.Rmd) 
Error: processing vignette 'emuDB.Rmd' failed with diagnostics:
$ operator is invalid for atomic vectors
Execution halted

```

## enigma (0.2.0)
Maintainer: Scott Chamberlain <myrmecocystus@gmail.com>  
Bug reports: https://github.com/ropengov/enigma/issues

0 errors | 0 warnings | 0 notes

## eurostat (1.2.21)
Maintainer: Lahti Leo <louhos@googlegroups.com>  
Bug reports: https://github.com/ropengov/eurostat/issues

0 errors | 0 warnings | 0 notes

## explor (0.2.1)
Maintainer: Julien Barnier <julien.barnier@ens-lyon.fr>  
Bug reports: https://github.com/juba/explor/issues

0 errors | 0 warnings | 0 notes

## eyetrackingR (0.1.6)
Maintainer: Jacob Dink <jacobwdink@gmail.com>  
Bug reports: https://github.com/jwdink/eyetrackingR/issues

0 errors | 0 warnings | 0 notes

## ezec (0.1.0)
Maintainer: Zhian N. Kamvar <kamvarz@science.oregonstate.edu>

0 errors | 0 warnings | 1 note 

```
checking re-building of vignette outputs ... NOTE
Error in re-building vignettes:
  ...
Warning in data("dummydata", library = "ezec") :
  data set 'ezec' not found
Quitting from lines 98-99 (getting_started.Rmd) 
Error: processing vignette 'getting_started.Rmd' failed with diagnostics:
Unknown column 'mod'
Execution halted

```

## ezsummary (0.1.9)
Maintainer: Hao Zhu <haozhu@hsl.harvard.edu>

0 errors | 0 warnings | 0 notes

## FactoMineR (1.32)
Maintainer: Francois Husson <francois.husson@agrocampus-ouest.fr>

0 errors | 0 warnings | 1 note 

```
checking data for non-ASCII characters ... NOTE
  Note: found 1 marked Latin-1 string
```

## flora (0.2.7)
Maintainer: Gustavo Carvalho <gustavo.bio@gmail.com>  
Bug reports: http://www.github.com/gustavobio/flora/issues

0 errors | 0 warnings | 0 notes

## forestmodel (0.4.0)
Maintainer: Nick Kennedy <r@nick-kennedy.com>

0 errors | 0 warnings | 0 notes

## fractional (0.1.3)
Maintainer: Bill Venables <bill.venables@gmail.com>

0 errors | 0 warnings | 0 notes

## freqweights (1.0.2)
Maintainer: Emilio Torres-Manzanera <torres@uniovi.es>

0 errors | 0 warnings | 0 notes

## FSA (0.8.6)
Maintainer: Derek Ogle <derek@derekogle.com>  
Bug reports: https://github.com/droglenc/FSA/issues

0 errors | 0 warnings | 1 note 

```
checking Rd cross-references ... NOTE
Packages unavailable to check Rd xrefs: ‘alr3’, ‘prettyR’, ‘epitools’, ‘RMark’, ‘asbio’, ‘agricolae’, ‘DescTools’
```

## fueleconomy (0.1)
Maintainer: 'Hadley Wickham' <h.wickham@gmail.com>

0 errors | 0 warnings | 0 notes

## futureheatwaves (1.0.0)
Maintainer: Brooke Anderson <brooke.anderson@colostate.edu>

0 errors | 0 warnings | 0 notes

## gapminder (0.2.0)
Maintainer: Jennifer Bryan <jenny@stat.ubc.ca>  
Bug reports: https://github.com/jennybc/gapminder/issues

0 errors | 0 warnings | 0 notes

## GenCAT (1.0.2)
Maintainer: Eric Reed <reeder@bu.edu>

1 error  | 0 warnings | 4 notes

```
checking examples ... ERROR
Running examples in ‘GenCAT-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: GenCAT
> ### Title: Running GenCAT
> ### Aliases: GenCAT
> ### Keywords: ~kwd1 ~kwd2
> 
... 15 lines ...
            SNP effect_allele other_allele   testStat chr position class
17232 rs9572807             C            T -0.3014771  13 72423959 DACH1
24151 rs4389009             G            A -1.4443860  13 99193519 STK24
37178 rs7151730             T            A  0.1387317  14 33531585 NPAS3
58941 rs1076958             G            A  2.0597766  14 91131857 TTC7B
13089 rs7987481             G            A -1.0024210  13 47318950 LRCH1
58302  rs411064             G            A  0.2132412  14 90008860 FOXN3
> 
> library(snpStats)
Error in library(snpStats) : there is no package called ‘snpStats’
Execution halted

checking package dependencies ... NOTE
Package suggested but not available for checking: ‘snpStats’

checking Rd cross-references ... NOTE
Package unavailable to check Rd xrefs: ‘snpStats’

checking data for non-ASCII characters ... NOTE
  Error in .requirePackage(package) : 
    unable to find required package 'snpStats'
  Calls: <Anonymous> ... .extendsForS3 -> extends -> getClassDef -> .requirePackage
  Execution halted

checking re-building of vignette outputs ... NOTE
Error in re-building vignettes:
  ...
Loading required package: dplyr

Attaching package: 'dplyr'

The following objects are masked from 'package:stats':

    filter, lag

The following objects are masked from 'package:base':

    intersect, setdiff, setequal, union

Loading required package: doParallel
Loading required package: foreach
Loading required package: iterators
Loading required package: parallel
Loading required package: ggplot2
Quitting from lines 62-69 (GenCAT-vignette.Rmd) 
Error: processing vignette 'GenCAT-vignette.Rmd' failed with diagnostics:
unable to find required package 'snpStats'
Execution halted

```

## gender (0.5.1)
Maintainer: Lincoln Mullen <lincoln@lincolnmullen.com>  
Bug reports: https://github.com/ropensci/gender/issues

0 errors | 0 warnings | 1 note 

```
checking package dependencies ... NOTE
Package suggested but not available for checking: ‘genderdata’
```

## geomnet (0.0.1)
Maintainer: Samantha Tyner <sctyner@iastate.edu>

1 error  | 0 warnings | 0 notes

```
checking examples ... ERROR
Running examples in ‘geomnet-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: football
> ### Title: College football games network (undirected)
> ### Aliases: football
> ### Keywords: datasets
> 
... 40 lines ...

Loading required package: sna
sna: Tools for Social Network Analysis
Version 2.3-2 created on 2014-01-13.
copyright (c) 2005, Carter T. Butts, University of California-Irvine
 For citation information, type citation("sna").
 Type help(package="sna") to get started.

Error in pmax(y, yend) : object 'y' not found
Calls: <Anonymous> ... <Anonymous> -> f -> with -> with.default -> eval -> eval -> pmax
Execution halted
```

## ggalt (0.1.1)
Maintainer: Bob Rudis <bob@rudis.net>  
Bug reports: https://github.com/hrbrmstr/ggalt/issues

0 errors | 0 warnings | 0 notes

## ggfortify (0.1.0)
Maintainer: Masaaki Horikoshi <sinhrks@gmail.com>

1 error  | 0 warnings | 0 notes

```
checking tests ... ERROR
Running the tests in ‘tests/test-all.R’ failed.
Last 13 lines of output:
  1. Failure: fortify.cpt works for AirPassengers (@test-changepoint.R#24) -------
  names(fortified) not equal to c("Index", "Data", "variance").
  Lengths differ: 4 vs 3
  
  
  Loading required package: urca
  Loading required package: lmtest
  testthat results ================================================================
  OK: 586 SKIPPED: 1 FAILED: 1
  1. Failure: fortify.cpt works for AirPassengers (@test-changepoint.R#24) 
  
  Error: testthat unit tests failed
  Execution halted
```

## ggmap (2.6.1)
Maintainer: David Kahle <david.kahle@gmail.com>  
Bug reports: https://github.com/dkahle/ggmap/issues

0 errors | 0 warnings | 0 notes

## ggmcmc (0.8)
Maintainer: Xavier Fernández i Marín <xavier.fim@gmail.com>  
Bug reports: https://github.com/xfim/ggmcmc/issues

1 error  | 0 warnings | 1 note 

```
checking examples ... ERROR
Running examples in ‘ggmcmc-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: ggmcmc
> ### Title: Wrapper function that creates a single pdf file with all plots
> ###   that ggmcmc can produce.
> ### Aliases: ggmcmc ggmcmc-package
> 
> ### ** Examples
> 
> data(linear)
> ggmcmc(ggs(s))  # Directly from a coda object
Plotting histograms
Error: cannot convert object to a data frame
Execution halted

checking re-building of vignette outputs ... NOTE
Error in re-building vignettes:
  ...
Loading required package: dplyr

Attaching package: 'dplyr'

The following objects are masked from 'package:stats':

    filter, lag

The following objects are masked from 'package:base':

    intersect, setdiff, setequal, union

Loading required package: tidyr
Loading required package: ggplot2
Registering fonts with R
Quitting from lines 133-134 (using_ggmcmc.Rmd) 
Error: processing vignette 'using_ggmcmc.Rmd' failed with diagnostics:
cannot convert object to a data frame
Execution halted

```

## ggpmisc (0.2.7)
Maintainer: Pedro J. Aphalo <pedro.aphalo@helsinki.fi>  
Bug reports: https://bitbucket.org/aphalo/ggpmisc

0 errors | 0 warnings | 0 notes

## ggRandomForests (1.2.1)
Maintainer: John Ehrlinger <john.ehrlinger@gmail.com>  
Bug reports: https://github.com/ehrlinger/ggRandomForests/issues

0 errors | 0 warnings | 1 note 

```
checking installed package size ... NOTE
  installed size is  5.9Mb
  sub-directories of 1Mb or more:
    data   2.6Mb
    doc    3.0Mb
```

## ggraptR (0.1)
Maintainer: Eugene Dubossarsky <eugene@presciient.com>  
Bug reports: https://github.com/cargomoose/raptR/issues

0 errors | 0 warnings | 0 notes

## ggspectra (0.1.6)
Maintainer: Pedro J. Aphalo <pedro.aphalo@helsinki.fi>  
Bug reports: https://bitbucket.org/aphalo/ggspectra

1 error  | 0 warnings | 0 notes

```
checking examples ... ERROR
Running examples in ‘ggspectra-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: plot.object_spct
> ### Title: Plot an object spectrum, especialization of generic plot
> ###   function
> ### Aliases: plot.object_spct
> ### Keywords: hplot
... 42 lines ...
  No valid 'w.length' values found
Warning in max(runs[["lengths"]]) :
  no non-missing arguments to max; returning -Inf
Warning: Removed 12 rows containing non-finite values (stat_color_guide).
Warning: Removed 12 rows containing non-finite values (stat_color_guide).
Warning: Removed 12 rows containing non-finite values (stat_wb_mean).
Warning: Removed 12 rows containing missing values (position_stack).
Error in matrix(value, n, p) : 
  'data' must be of a vector type, was 'NULL'
Calls: <Anonymous> ... map_position -> lapply -> FUN -> [<- -> [<-.data.frame -> matrix
Execution halted
```

## ggvis (0.4.2)
Maintainer: Winston Chang <winston@rstudio.com>

0 errors | 0 warnings | 0 notes

## gistr (0.3.6)
Maintainer: Scott Chamberlain <myrmecocystus@gmail.com>  
Bug reports: http://www.github.com/ropensci/gistr/issues

0 errors | 0 warnings | 0 notes

## gitlabr (0.6.4)
Maintainer: Jirka Lewandowski <jirka.lewandowski@wzb.eu>  
Bug reports: 
        http://gitlab.points-of-interest.cc/points-of-interest/gitlabr/
        issues/

0 errors | 0 warnings | 0 notes

## glycanr (0.2.0)
Maintainer: Ivo Ugrina <ivo@iugrina.com>  
Bug reports: https://github.com/iugrina/glycanr/issues

0 errors | 0 warnings | 0 notes

## googlesheets (0.2.0)
Maintainer: Jennifer Bryan <jenny@stat.ubc.ca>  
Bug reports: https://github.com/jennybc/googlesheets/issues

0 errors | 0 warnings | 0 notes

## graphTweets (0.3)
Maintainer: John Coene <jcoenep@gmail.com>  
Bug reports: https://github.com/JohnCoene/graphTweets/issues

0 errors | 0 warnings | 1 note 

```
checking Rd cross-references ... NOTE
Package unavailable to check Rd xrefs: ‘twitteR’
```

## growthcurver (0.2.0)
Maintainer: Kathleen sprouffske <sprouffske@gmail.com>  
Bug reports: https://github.com/sprouffske/growthcurver/issues

0 errors | 0 warnings | 0 notes

## gunsales (0.1.1)
Maintainer: Dirk Eddelbuettel <edd@debian.org>

0 errors | 0 warnings | 0 notes

## haven (0.2.0)
Maintainer: Hadley Wickham <hadley@rstudio.com>  
Bug reports: https://github.com/hadley/haven/issues

0 errors | 0 warnings | 0 notes

## hdr (0.1)
Maintainer: Eric Persson <expersso5@gmail.com>  
Bug reports: https://github.com/expersso/hdr

0 errors | 0 warnings | 0 notes

## heemod (0.3.0)
Maintainer: Antoine Filipovic-Pierucci <pierucci@gmail.com>  
Bug reports: https://github.com/pierucci/heemod/issues

1 error  | 0 warnings | 0 notes

```
checking tests ... ERROR
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  > 
  > test_check("heemod")
  1. Failure: Parameter evaluation (@test_parameters.R#75) -----------------------
  `output` does not match "2 evaluated parameters, 10 Markov cycles.\n\nSource: local data frame [10 x 3]\n\n   markov_cycle     a     b\n          (int) (dbl) (dbl)\n1             1     2     2\n2             2     2     4\n3             3     2     6".
  Actual value: "2 evaluated parameters, 10 Markov cycles.\n\nSource: local data frame [10 x 3]\n\n   markov_cycle     a     b\n          <int> <dbl> <dbl>\n1             1     2     2\n2             2     2     4\n3             3     2     6\n4             4     2     8\n5             5     2    10\n6             6     2    12\n7             7     2    14\n8             8     2    16\n9             9     2    18\n10           10     2    20"
  
  
  testthat results ================================================================
  OK: 80 SKIPPED: 0 FAILED: 1
  1. Failure: Parameter evaluation (@test_parameters.R#75) 
  
  Error: testthat unit tests failed
  Execution halted
```

## highcharter (0.2.0)
Maintainer: Joshua Kunst <jbkunst@gmail.com>  
Bug reports: https://github.com/jbkunst/highcharter/issues

1 error  | 0 warnings | 0 notes

```
checking examples ... ERROR
Running examples in ‘highcharter-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: hc_add_series_flags
> ### Title: Shorcut for add flags to highstock chart
> ### Aliases: hc_add_serie_flags hc_add_series_flags
> 
> ### ** Examples
... 19 lines ...

 This  behavior  will be  phased out in 0.5-0  when the call  will
 default to use auto.assign=FALSE. getOption("getSymbols.env") and 
 getOptions("getSymbols.auto.assign") are now checked for alternate defaults

 This message is shown once per session and may be disabled by setting 
 options("getSymbols.warning4.0"=FALSE). See ?getSymbols for more details.
Error in download.file(oanda.URL, destfile = tmp, quiet = !verbose) : 
  cannot open URL 'http://www.oanda.com/currency/historical-rates/download?quote_currency=USD&end_date=2016-03-25&start_date=2014-11-12&period=daily&display=absolute&rate=0&data_range=y2&price=mid&view=table&base_currency_0=JPY&base_currency_1=&base_currency_2=&base_currency_3=&base_currency_4=&download=csv'
Calls: getSymbols -> do.call -> getSymbols.oanda -> download.file
Execution halted
```

## HydeNet (0.10.3)
Maintainer: Benjamin Nutter <benjamin.nutter@gmail.com>  
Bug reports: https://github.com/nutterb/HydeNet/issues

1 error  | 0 warnings | 0 notes

```
checking package dependencies ... ERROR
Package required but not available: ‘graph’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```

## hydrostats (0.2.4)
Maintainer: Nick Bond <n.bond@griffith.edu.au>

0 errors | 0 warnings | 0 notes

## IATscores (0.1-2)
Maintainer: Giulio Costantini <costantinigiulio@gmail.com>

1 error  | 0 warnings | 0 notes

```
checking package dependencies ... ERROR
Package required but not available: ‘nem’

Package suggested but not available for checking: ‘nparcomp’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```

## idbr (0.1.2)
Maintainer: Kyle Walker <kyle.walker@tcu.edu>

0 errors | 0 warnings | 0 notes

## IMP (1.1)
Maintainer: Anup Nair <nairanup50695@gmail.com>

0 errors | 0 warnings | 0 notes

## internetarchive (0.1.5)
Maintainer: Lincoln Mullen <lincoln@lincolnmullen.com>  
Bug reports: https://github.com/ropensci/internetarchive/issues

0 errors | 0 warnings | 0 notes

## JacobiEigen (0.2-2)
Maintainer: Bill Venables <Bill.Venables@gmail.com>

0 errors | 0 warnings | 0 notes

## laketemps (0.5.1)
Maintainer: Jordan S Read <jread@usgs.gov>  
Bug reports: https://github.com/USGS-R/laketemps/issues

0 errors | 0 warnings | 0 notes

## livechatR (0.1.0)
Maintainer: Lawrence Wu <lwu@payoff.com>

0 errors | 0 warnings | 0 notes

## LocFDRPois (1.0.0)
Maintainer: Kris Sankaran <kriss1@stanford.edu>

0 errors | 0 warnings | 0 notes

## longurl (0.1.1)
Maintainer: Bob Rudis <bob@rudis.net>

0 errors | 0 warnings | 0 notes

## lookupTable (0.1)
Maintainer: Enzo Jia <enzo.jia@gmail.com>

0 errors | 0 warnings | 0 notes

## loopr (1.0.1)
Maintainer: Brandon Taylor <Brandon.Taylor221@gmail.com>

0 errors | 0 warnings | 0 notes

## manifestoR (1.2)
Maintainer: Jirka Lewandowski <jirka.lewandowski@wzb.eu>  
Bug reports: https://github.com/ManifestoProject/manifestoR/issues

0 errors | 0 warnings | 0 notes

## Matrix.utils (0.5)
Maintainer: Craig Varrichio <canthony427@gmail.com>

0 errors | 0 warnings | 0 notes

## MazamaSpatialUtils (0.4.3)
Maintainer: Jonathan Callahan <jonathan.s.callahan@gmail.com>

0 errors | 0 warnings | 0 notes

## merTools (0.2.0)
Maintainer: Jared E. Knowles <jknowles@gmail.com>  
Bug reports: http://www.github.com/jknowles/merTools

0 errors | 0 warnings | 0 notes

## metricsgraphics (0.9.0)
Maintainer: Bob Rudis <bob@rudis.net>  
Bug reports: https://github.com/hrbrmstr/metricsgraphics/issues

0 errors | 0 warnings | 0 notes

## Momocs (1.0.0)
Maintainer: Vincent Bonhomme <bonhomme.vincent@gmail.com>  
Bug reports: https://github.com/vbonhomme/Momocs/issues

0 errors | 0 warnings | 0 notes

## morse (2.1.1)
Maintainer: Philippe Ruiz <philippe.ruiz@univ-lyon1.fr>

1 error  | 0 warnings | 0 notes

```
checking whether package ‘morse’ can be installed ... ERROR
Installation failed.
See ‘/Users/hadley/Documents/dplyr/dplyr/revdep/checks/morse.Rcheck/00install.out’ for details.
```

## mosaic (0.13.0)
Maintainer: Randall Pruim <rpruim@calvin.edu>  
Bug reports: https://github.com/ProjectMOSAIC/mosaic/issues

0 errors | 0 warnings | 0 notes

## mtconnectR (0.2.0)
Maintainer: Alex Joseph <alex@systeminsights.com>

1 error  | 0 warnings | 0 notes

```
checking tests ... ERROR
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
    |===================================================================== |  98%
    |                                                                            
    |======================================================================| 100%
  Error: `dmtcd` not equal to lazyLoadDBfetch(c(0L, 1776L), datafile, compressed, envhook).
  Attributes: < Names: 2 string mismatches >
  Attributes: < Length mismatch: comparison on first 2 components >
  Attributes: < Component 1: target is externalptr, current is character >
  Attributes: < Component 2: Modes: character, numeric >
  Attributes: < Component 2: Lengths: 1, 251 >
  Attributes: < Component 2: target is character, current is numeric >
  testthat results ================================================================
  OK: 0 SKIPPED: 0 FAILED: 0
  Execution halted
```

## muir (0.1.0)
Maintainer: Justin Alford <justin.alford@gmail.com>  
Bug reports: https://github.com/alforj/muir/issues

0 errors | 0 warnings | 0 notes

## myTAI (0.3.0)
Maintainer: Hajk-Georg Drost <hajk-georg.drost@informatik.uni-halle.de>  
Bug reports: https://github.com/HajkD/myTAI/issues

1 error  | 0 warnings | 0 notes

```
checking package dependencies ... ERROR
Package required but not available: ‘edgeR’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```

## ncappc (0.2.1.1)
Maintainer: Chayan Acharya <chayan.acharya@farmbio.uu.se>

0 errors | 0 warnings | 0 notes

## NCmisc (1.1.4)
Maintainer: Nicholas Cooper <nick.cooper@cimr.cam.ac.uk>

0 errors | 0 warnings | 2 notes

```
checking package dependencies ... NOTE
Package suggested but not available for checking: ‘BiocInstaller’

checking Rd cross-references ... NOTE
Package unavailable to check Rd xrefs: ‘reader’
```

## NPC (1.0.2)
Maintainer: Devin Caughey <devin.caughey@gmail.com>

0 errors | 0 warnings | 0 notes

## nullabor (0.3.1)
Maintainer: Di Cook <dicook@iastate.edu>

0 errors | 0 warnings | 0 notes

## nycflights13 (0.1)
Maintainer: 'Hadley Wickham' <h.wickham@gmail.com>

0 errors | 0 warnings | 2 notes

```
checking installed package size ... NOTE
  installed size is  5.6Mb
  sub-directories of 1Mb or more:
    data   5.5Mb

checking DESCRIPTION meta-information ... NOTE
Malformed Title field: should not end in a period.
```

## openair (1.8-0)
Maintainer: David Carslaw <david.carslaw@york.ac.uk>  
Bug reports: https://github.com/davidcarslaw/openair/issues

1 error  | 0 warnings | 0 notes

```
checking examples ... ERROR
Running examples in ‘openair-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: aqStats
> ### Title: Calculate summary statistics for air pollution data by year
> ### Aliases: aqStats
> ### Keywords: methods
> 
> ### ** Examples
> 
> 
> ## Statistics for 2004. NOTE! these data are in ppb/ppm so the
> ## example is for illustrative purposes only
> aqStats(selectByDate(mydata, year = 2004), pollutant = "no2")
Error: Unknown column 'ws'
Execution halted
```

## PAC (1.0.0)
Maintainer: Dangna Li <ldangna@gmail.com>

0 errors | 0 warnings | 0 notes

## packagetrackr (0.1.1)
Maintainer: Jirka Lewandowski <jirka.lewandowski@wzb.eu>  
Bug reports: 
        http://gitlab.points-of-interest.cc/points-of-interest/packagetrackr/issues

0 errors | 0 warnings | 0 notes

## peptider (0.2.2)
Maintainer: Eric Hare <erichare@iastate.edu>  
Bug reports: https://github.com/heike/peptider/issues

0 errors | 0 warnings | 0 notes

## photobiology (0.9.5)
Maintainer: Pedro J. Aphalo <pedro.aphalo@helsinki.fi>  
Bug reports: https://bitbucket.org/aphalo/photobiology/issues

0 errors | 0 warnings | 0 notes

## pinnacle.API (1.90)
Maintainer: Marco Blume <marco.blume@pinnaclesports.com>

0 errors | 0 warnings | 0 notes

## pitchRx (1.8.2)
Maintainer: Carson Sievert <cpsievert1@gmail.com>  
Bug reports: http://github.com/cpsievert/pitchRx/issues

0 errors | 0 warnings | 1 note 

```
checking package dependencies ... NOTE
Package suggested but not available for checking: ‘ggsubplot’
```

## pixiedust (0.6.1)
Maintainer: Benjamin Nutter <nutter@battelle.org>  
Bug reports: https://github.com/nutterb/pixiedust/issues

1 error  | 0 warnings | 0 notes

```
checking tests ... ERROR
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  NOT(print_dust_latex(x) did not throw an error.
  )
  
  
  testthat results ================================================================
  OK: 148 SKIPPED: 0 FAILED: 4
  1. Failure: dust runs when passed a data frame with tidy_df = FALSE (@test-dust.R#41) 
  2. Failure: dust runs when passed a data frame with tidy_df = TRUE (@test-dust.R#46) 
  3. Error: dust with caption and non-floating environment gives warning (@test-dust.R#91) 
  4. Failure: print_dust_latex (@test-print_dust_methods.R#160) 
  
  Error: testthat unit tests failed
  Execution halted
```

## plotly (3.4.1)
Maintainer: Carson Sievert <cpsievert1@gmail.com>  
Bug reports: https://github.com/ropensci/plotly/issues

0 errors | 0 warnings | 0 notes

## pmc (1.0.1)
Maintainer: Carl Boettiger <cboettig@gmail.com>  
Bug reports: https://github.com/cboettig/pmc/issues

1 error  | 0 warnings | 0 notes

```
checking package dependencies ... ERROR
Package required but not available: ‘ouch’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```

## PogromcyDanych (1.5)
Maintainer: Przemyslaw Biecek <przemyslaw.biecek@gmail.com>

0 errors | 0 warnings | 2 notes

```
checking installed package size ... NOTE
  installed size is  6.1Mb
  sub-directories of 1Mb or more:
    data   6.0Mb

checking data for non-ASCII characters ... NOTE
  Note: found 7256 marked UTF-8 strings
```

## pollstR (1.2.2)
Maintainer: Jeffrey B. Arnold <jeffrey.arnold@gmail.com>  
Bug reports: https://github.com/rOpenGov/pollstR/issues

0 errors | 0 warnings | 0 notes

## PopED (0.3.0)
Maintainer: Andrew C. Hooker <andrew.hooker@farmbio.uu.se>  
Bug reports: https://github.com/andrewhooker/PopED/issues

0 errors | 0 warnings | 0 notes

## poplite (0.99.16)
Maintainer: Daniel Bottomly <bottomly@ohsu.edu>

0 errors | 1 warning  | 3 notes

```
checking running R code from vignettes ... WARNING
Errors in running code in vignettes:
when running code in ‘poplite.Rnw’
  ...
8             8         8    18      0  1.6844357  0.53539884          2     F
9             9         9    20      1  0.9113913 -0.55527835          2     F
10           10        10    12      1  0.2374303  1.77950291          1     M
..          ...       ...   ...    ...        ...         ...        ...   ...

> library(VariantAnnotation)

  When sourcing ‘poplite.R’:
Error: there is no package called ‘VariantAnnotation’
Execution halted


checking package dependencies ... NOTE
Package suggested but not available for checking: ‘VariantAnnotation’

checking R code for possible problems ... NOTE
join,Database : .local: no visible binding for global variable
  ‘new.ancil’

checking re-building of vignette outputs ... NOTE
Error in re-building vignettes:
  ...

    select

The following object is masked from ‘package:stats’:

    filter

... 8 lines ...
Loading required package: DBI
Starting gender
Starting clinical
Starting samples
Starting dna

Error: processing vignette 'poplite.Rnw' failed with diagnostics:
 chunk 15 
Error in library(VariantAnnotation) : 
  there is no package called ‘VariantAnnotation’
Execution halted
```

## poppr (2.1.1)
Maintainer: Zhian N. Kamvar <kamvarz@science.oregonstate.edu>  
Bug reports: https://github.com/grunwaldlab/poppr/issues

0 errors | 0 warnings | 1 note 

```
checking re-building of vignette outputs ... NOTE
Error in re-building vignettes:
  ...
Loading required package: adegenet
Loading required package: ade4

   /// adegenet 2.0.1 is loaded ////////////

   > overview: '?adegenet'
   > tutorials/doc/questions: 'adegenetWeb()' 
   > bug reports/feature requests: adegenetIssues()


This is poppr version 2.1.1. To get started, type package?poppr
OMP parallel support: unavailable
Loading required package: ape
Quitting from lines 277-282 (mlg.Rmd) 
Error: processing vignette 'mlg.Rmd' failed with diagnostics:
No variables selected
Execution halted

```

## prepdat (1.0.7)
Maintainer: Ayala S. Allon <ayalaallon@gmail.com>  
Bug reports: http://github.com/ayalaallon/prepdat/issues

0 errors | 0 warnings | 0 notes

## pRF (1.2)
Maintainer: Ankur Chakravarthy <ankur.chakravarthy.10@ucl.ac.uk>

1 error  | 0 warnings | 0 notes

```
checking package dependencies ... ERROR
Package required but not available: ‘multtest’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```

## purrr (0.2.1)
Maintainer: Hadley Wickham <hadley@rstudio.com>  
Bug reports: https://github.com/hadley/purrr/issues

0 errors | 0 warnings | 0 notes

## qdap (2.2.4)
Maintainer: Tyler Rinker <tyler.rinker@gmail.com>  
Bug reports: http://github.com/trinker/qdap/issues

0 errors | 1 warning  | 0 notes

```
checking Rd cross-references ... WARNING
Missing link or links in documentation object 'chain.Rd':
  ‘[dplyr]{%.%}’

See section 'Cross-references' in the 'Writing R Extensions' manual.

```

## qualvar (0.1.0)
Maintainer: Joel Gombin <joel.gombin@gmail.com>

0 errors | 0 warnings | 0 notes

## quickpsy (0.1.2)
Maintainer: Linares Daniel <danilinares@gmail.com>

1 error  | 0 warnings | 0 notes

```
checking examples ... ERROR
Running examples in ‘quickpsy-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: logliks
> ### Title: Calculates the loglikelihoods
> ### Aliases: logliks
> 
> ### ** Examples
> 
> library(MPDiR) # contains the Vernier data
> fit <- quickpsy(Vernier, Phaseshift, NumUpward, N,
+                 grouping = .(Direction, WaveForm, TempFreq), B = 20)
Estimating parameters...
Error: Unknown column 'y'
Execution halted
```

## qwraps2 (0.1.2)
Maintainer: Peter DeWitt <dewittpe@gmail.com>

0 errors | 0 warnings | 0 notes

## randNames (0.2.1)
Maintainer: Karthik Ram <karthik.ram@gmail.com>  
Bug reports: https://github.com/karthik/randNames/issues

0 errors | 0 warnings | 0 notes

## randomizr (0.3.0)
Maintainer: Alexander Coppock <ac3242@columbia.edu>

0 errors | 0 warnings | 0 notes

## rattle (4.1.0)
Maintainer: Graham Williams <Graham.Williams@togaware.com>

0 errors | 0 warnings | 3 notes

```
checking package dependencies ... NOTE
Packages suggested but not available for checking:
  ‘graph’ ‘RBGL’ ‘rggobi’ ‘RODBC’ ‘pkgDepTools’ ‘Rgraphviz’

checking installed package size ... NOTE
  installed size is  6.8Mb
  sub-directories of 1Mb or more:
    data   2.5Mb
    etc    1.9Mb
    po     1.2Mb

checking dependencies in R code ... NOTE

(R:87251): Gtk-WARNING **: gtk_disable_setlocale() must be called before gtk_init()
```

## rbison (0.4.8)
Maintainer: Scott Chamberlain <myrmecocystus@gmail.com>  
Bug reports: https://github.com/ropensci/rbison/issues

0 errors | 0 warnings | 0 notes

## rchess (0.1)
Maintainer: Joshua Kunst <jbkunst@gmail.com>  
Bug reports: https://github.com/jbkunst/rchess/issues

0 errors | 0 warnings | 0 notes

## rcicr (0.3.2.1)
Maintainer: Ron Dotsch <rdotsch@gmail.com>

0 errors | 0 warnings | 0 notes

## RCMIP5 (1.1)
Maintainer: Kathe Todd-Brown <ktoddbrown@gmail.com>

1 error  | 0 warnings | 2 notes

```
checking tests ... ERROR
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  1. Failure: makeGlobalStat sorts before computing (@test_makeGlobalStat.R#136) -
  res1$val$value not equal to ans$value.
  2/2 mismatches (average diff: 0.333)
  [1] 1.17 - 1.5 == -0.333
  [2] 1.17 - 1.5 == -0.333
  
  
  testthat results ================================================================
  OK: 422 SKIPPED: 20 FAILED: 1
  1. Failure: makeGlobalStat sorts before computing (@test_makeGlobalStat.R#136) 
  
  Error: testthat unit tests failed
  Execution halted

checking package dependencies ... NOTE
Package suggested but not available for checking: ‘ncdf’

checking dependencies in R code ... NOTE
'library' or 'require' calls in package code:
  ‘ggplot2’ ‘ncdf’ ‘ncdf4’
  Please use :: or requireNamespace() instead.
  See section 'Suggested packages' in the 'Writing R Extensions' manual.
```

## rcrossref (0.5.2)
Maintainer: Scott Chamberlain <myrmecocystus@gmail.com>  
Bug reports: https://github.com/ropensci/rcrossref/issues

0 errors | 0 warnings | 0 notes

## RDML (0.9-1)
Maintainer: Konstantin A. Blagodatskikh <k.blag@yandex.ru>

0 errors | 0 warnings | 0 notes

## rdrop2 (0.7.0)
Maintainer: Karthik Ram <karthik.ram@gmail.com>  
Bug reports: https://github.com/karthik/rdrop2/issues

0 errors | 0 warnings | 0 notes

## readODS (1.6.2)
Maintainer: Chung-hong Chan <chainsawtiney@gmail.com>

0 errors | 0 warnings | 0 notes

## rebird (0.3.0)
Maintainer: Sebastian Pardo <sebpardo@gmail.com>  
Bug reports: http://github.com/ropensci/rebird/issues

0 errors | 0 warnings | 0 notes

## refund.shiny (0.2.0)
Maintainer: Julia Wrobel <jw3134@cumc.columbia.edu>

1 error  | 0 warnings | 0 notes

```
checking whether package ‘refund.shiny’ can be installed ... ERROR
Installation failed.
See ‘/Users/hadley/Documents/dplyr/dplyr/revdep/checks/refund.shiny.Rcheck/00install.out’ for details.
```

## rerddap (0.3.4)
Maintainer: Scott Chamberlain <myrmecocystus@gmail.com>  
Bug reports: http://www.github.com/ropensci/rerddap/issues

0 errors | 0 warnings | 0 notes

## resumer (0.0.1)
Maintainer: Jared Lander <packages@jaredlander.com>  
Bug reports: https://github.com/jaredlander/resumer/issues

0 errors | 0 warnings | 0 notes

## reval (2.0.0)
Maintainer: Michael C Koohafkan <michael.koohafkan@gmail.com>  
Bug reports: https://github.com/mkoohafkan/reval/issues

0 errors | 0 warnings | 0 notes

## rex (1.1.1)
Maintainer: Jim Hester <james.f.hester@gmail.com>  
Bug reports: https://github.com/kevinushey/rex/issues

0 errors | 0 warnings | 0 notes

## rfishbase (2.1.0)
Maintainer: Carl Boettiger <cboettig@ropensci.org>  
Bug reports: https://github.com/ropensci/rfishbase/issues

0 errors | 0 warnings | 1 note 

```
checking data for non-ASCII characters ... NOTE
  Note: found 33 marked UTF-8 strings
```

## rgho (0.0.1)
Maintainer: Antoine Filipovic-Pierucci <pierucci@gmail.com>  
Bug reports: https://github.com/pierucci/rgho/issues

0 errors | 0 warnings | 0 notes

## rivr (1.2)
Maintainer: Michael C Koohafkan <michael.koohafkan@gmail.com>  
Bug reports: https://github.com/mkoohafkan/rivr/issues

0 errors | 0 warnings | 0 notes

## RNeXML (2.0.6)
Maintainer: Carl Boettiger <cboettig@gmail.com>  
Bug reports: https://github.com/ropensci/RNeXML/issues

2 errors | 0 warnings | 2 notes

```
checking examples ... ERROR
Running examples in ‘RNeXML-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: add_basic_meta
> ### Title: Add basic metadata
> ### Aliases: add_basic_meta
> 
> ### ** Examples
> 
> nex <- add_basic_meta(title = "My test title",
+              description = "A description of my test",
+              creator = "Carl Boettiger <cboettig@gmail.com>",
+              publisher = "unpublished data",
+              pubdate = "2012-04-01")
Error: Unknown column 'content'
Execution halted

checking tests ... ERROR
Running the tests in ‘tests/test-all.R’ failed.
Last 13 lines of output:
  1. Error: We can serialize the various versions of the ape format (@test_ape.R#40) 
  2. Error: We can read and write NeXML to phylo and back without edge.lengths (@test_ape.R#52) 
  3. Error: Rooted trees remain rooted on conversions (@test_ape.R#65) 
  4. Error: Unrooted trees remain unrooted on conversions (@test_ape.R#74) 
  5. Error: we can extract character matrix with get_characters (@test_characters.R#44) 
  6. Error: we can add characters to a nexml file using a data.frame (@test_characters.R#88) 
  7. Error: We can extract tree and trait data to run fitContinuous and fitDiscrete (@test_comp_analysis.R#9) 
  8. Error: We can serialize tree and trait data for a comparative analysis (@test_comp_analysis.R#25) 
  9. Error: Getting characters (@test_get_characters.R#7) 
  1. ...
  
  Error: testthat unit tests failed
  Execution halted

checking package dependencies ... NOTE
Packages suggested but not available for checking: ‘rrdf’ ‘Sxslt’

checking re-building of vignette outputs ... NOTE
Error in re-building vignettes:
  ...
Loading required package: ape
Quitting from lines 46-67 (metadata.Rmd) 
Error: processing vignette 'metadata.Rmd' failed with diagnostics:
Unknown column 'content'
Execution halted

```

## rnoaa (0.5.2)
Maintainer: Scott Chamberlain <myrmecocystus@gmail.com>  
Bug reports: http://www.github.com/ropensci/rnoaa/issues

0 errors | 0 warnings | 0 notes

## robotstxt (0.1.2)
Maintainer: Peter Meissner <retep.meissner@gmail.com>  
Bug reports: https://github.com/petermeissner/robotstxt/issues

0 errors | 0 warnings | 0 notes

## rpcdsearch (1.0)
Maintainer: David Springate <daspringate@gmail.com>

0 errors | 0 warnings | 0 notes

## rpdo (0.1.1)
Maintainer: Joe Thorley <joe@poissonconsulting.ca>

0 errors | 0 warnings | 0 notes

## rpivotTable (0.1.5.7)
Maintainer: Enzo Martoglio  <enzo@smartinsightsfromdata.com>

1 error  | 0 warnings | 0 notes

```
checking tests ... ERROR
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  1. Error: parameters handled correctly (@test_params.R#4) ----------------------
  is.character(object) is not TRUE
  1: expect_match(rpivotTable(data.frame(), rows = c("arow"))$x$params$rows, "arow") at testthat/test_params.R:4
  2: stopifnot(is.character(object)) at /private/tmp/Rtmp0b9LfI/devtoolscb0c76082acf/hadley-testthat-94fc45a/R/expectations-matches.R:26
  3: stop(sprintf(ngettext(length(r), "%s is not TRUE", "%s are not all TRUE"), ch), call. = FALSE, 
         domain = NA)
  
  testthat results ================================================================
  OK: 11 SKIPPED: 0 FAILED: 1
  1. Error: parameters handled correctly (@test_params.R#4) 
  
  Error: testthat unit tests failed
  Execution halted
```

## rplexos (1.1.4)
Maintainer: Eduardo Ibanez <edu.ibanez@gmail.com>  
Bug reports: https://github.com/NREL/rplexos/issues

1 error  | 0 warnings | 1 note 

```
checking examples ... ERROR
Running examples in ‘rplexos-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: process_folder
> ### Title: Convert PLEXOS files to SQLite databases
> ### Aliases: process_folder process_input process_solution
> 
> ### ** Examples
> 
> # Process the folder with the solution file provided by rplexos
> location <- location_solution_rplexos()
> process_folder(location)
Warning: `rbind_list()` is deprecated. Please use `bind_rows()` instead.
> 
> # Process the folder with the input file provided by rplexos
> location2 <- location_input_rplexos()
> process_folder(location2)
Error in sqliteSendQuery(con, statement, bind.data) : 
  error in statement: no such column: comp_collection
Calls: process_folder ... .local -> sqliteGetQuery -> sqliteSendQuery -> .Call
Execution halted

checking re-building of vignette outputs ... NOTE
Error in re-building vignettes:
  ...
Quitting from lines 186-187 (rplexos.Rmd) 
Error: processing vignette 'rplexos.Rmd' failed with diagnostics:
error in statement: no such column: comp_collection
Execution halted

```

## rplos (0.5.6)
Maintainer: Scott Chamberlain <myrmecocystus@gmail.com>  
Bug reports: https://github.com/ropensci/rplos/issues

0 errors | 0 warnings | 0 notes

## rPref (0.7)
Maintainer: Patrick Roocks <mail@p-roocks.de>

1 error  | 0 warnings | 0 notes

```
checking whether package ‘rPref’ can be installed ... ERROR
Installation failed.
See ‘/Users/hadley/Documents/dplyr/dplyr/revdep/checks/rPref.Rcheck/00install.out’ for details.
```

## RPresto (1.2.0)
Maintainer: Onur Ismail Filiz <onur@fb.com>  
Bug reports: https://github.com/prestodb/RPresto/issues

1 error  | 1 warning  | 0 notes

```
checking tests ... ERROR
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  1: dplyr::src_translate_env at testthat/test-src_translate_env.R:15
  2: getExportedValue(pkg, name)
  3: stop(gettextf("'%s' is not an exported object from 'namespace:%s'", name, getNamespaceName(ns)), 
         call. = FALSE, domain = NA)
  
  testthat results ================================================================
  OK: 205 SKIPPED: 23 FAILED: 3
  1. Failure: dbFetch works with mock (@test-dbFetch.R#37) 
  2. Failure: dbFetch works with mock (@test-dbFetch.R#37) 
  3. Error: as() works (@test-src_translate_env.R#15) 
  
  Error: testthat unit tests failed
  Execution halted

checking Rd cross-references ... WARNING
Missing link or links in documentation object 'dplyr_function_implementations.Rd':
  ‘[dplyr]{src_translate_env}’

See section 'Cross-references' in the 'Writing R Extensions' manual.

```

## RSQLServer (0.2.0)
Maintainer: Imanuel Costigan <i.costigan@me.com>  
Bug reports: https://github.com/imanuelcostigan/RSQLServer/issues

1 error  | 0 warnings | 0 notes

```
checking whether package ‘RSQLServer’ can be installed ... ERROR
Installation failed.
See ‘/Users/hadley/Documents/dplyr/dplyr/revdep/checks/RSQLServer.Rcheck/00install.out’ for details.
```

## rtable (0.1.5)
Maintainer: David Gohel <david.gohel@lysis-consultants.fr>  
Bug reports: https://github.com/davidgohel/rtable/issues

0 errors | 0 warnings | 0 notes

## rtimes (0.3.0)
Maintainer: Scott Chamberlain <myrmecocystus@gmail.com>  
Bug reports: https://github.com/ropengov/rtimes/issues

0 errors | 0 warnings | 0 notes

## RtutoR (0.1)
Maintainer: Anup Nair <nairanup50695@gmail.com>

0 errors | 0 warnings | 0 notes

## rvertnet (0.4.1)
Maintainer: Scott Chamberlain <myrmecocystus@gmail.com>  
Bug reports: https://github.com/ropensci/rvertnet/issues

0 errors | 0 warnings | 0 notes

## rwunderground (0.1.0)
Maintainer: Alex Shum <alex@ALShum.com>  
Bug reports: https://github.com/alshum/rwunderground/issues

0 errors | 0 warnings | 0 notes

## saeSim (0.7.0)
Maintainer: Sebastian Warnholz <Sebastian.Warnholz@fu-berlin.de>  
Bug reports: https://github.com/wahani/saeSim/issues

0 errors | 0 warnings | 0 notes

## scholar (0.1.4)
Maintainer: James Keirstead <james.keirstead@gmail.com>  
Bug reports: https://github.com/jkeirstead/scholar/issues

0 errors | 0 warnings | 0 notes

## SciencesPo (1.3.9)
Maintainer: Daniel Marcelino <dmarcelino@live.com>  
Bug reports: http://github.com/danielmarcelino/SciencesPo/issues

0 errors | 0 warnings | 1 note 

```
checking re-building of vignette outputs ... NOTE
Error in re-building vignettes:
  ...
Units                 Measurement System Units
bhodrick93            Bekaert's and Hodrick's (1993) Data
cathedrals            Cathedrals
cgreene76             Christensen's and Greene's (1976) Data
galton                Galton's Family Data on Human Stature.
griliches76           Griliches's (1976) Data
ltaylor96             Lothian's and Taylor's (1996) Data Set
... 8 lines ...
turnout               Turnout Data
twins                 Burt's twin data
words                 Word frequencies from Mosteller and Wallace

Loading required package: SciencesPo
initializing ... done

Quitting from lines 395-399 (SciencesPo.Rmd) 
Error: processing vignette 'SciencesPo.Rmd' failed with diagnostics:
polygon edge not found
Execution halted
```

## SEERaBomb (2015.2)
Maintainer: Tomas Radivoyevitch <radivot@ccf.org>

0 errors | 0 warnings | 0 notes

## sejmRP (1.3)
Maintainer: Piotr Smuda <piotrsmuda@gmail.com>  
Bug reports: http://github.com/mi2-warsaw/sejmRP/issues

0 errors | 0 warnings | 0 notes

## shazam (0.1.2)
Maintainer: Jason Vander Heiden <jason.vanderheiden@yale.edu>  
Bug reports: https://bitbucket.org/kleinstein/shazam/issues

0 errors | 0 warnings | 0 notes

## shinyAce (0.2.1)
Maintainer: Jeff Allen <cran@trestletechnology.net>  
Bug reports: https://github.com/trestletech/shinyAce/issues

0 errors | 0 warnings | 1 note 

```
checking installed package size ... NOTE
  installed size is  9.9Mb
  sub-directories of 1Mb or more:
    www   9.7Mb
```

## SimDesign (0.8)
Maintainer: Phil Chalmers <rphilip.chalmers@gmail.com>

0 errors | 0 warnings | 1 note 

```
checking package dependencies ... NOTE
Package suggested but not available for checking: ‘doMPI’
```

## simmer (3.1.2)
Maintainer: Iñaki Ucar <i.ucar86@gmail.com>  
Bug reports: https://github.com/Bart6114/simmer/issues

1 error  | 0 warnings | 0 notes

```
checking tests ... ERROR
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  > test_check("simmer")
  1. Failure: the trajectory stores the right number of activities (@test-trajectory.R#76) 
  `t0` produced no output
  
  
  { Activity: Timeout(none) | delay: 1 }
  Trajectory: anonymous, 1 activities
  testthat results ================================================================
  OK: 192 SKIPPED: 0 FAILED: 1
  1. Failure: the trajectory stores the right number of activities (@test-trajectory.R#76) 
  
  Error: testthat unit tests failed
  Execution halted
```

## simPH (1.3.5)
Maintainer: Christopher Gandrud <christopher.gandrud@gmail.com>  
Bug reports: https://github.com/christophergandrud/simPH/issues

1 error  | 0 warnings | 1 note 

```
checking examples ... ERROR
Running examples in ‘simPH-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: MinMaxLines
> ### Title: Transform the simulation object to include only the min and max
> ###   of the constricted intervals, as well as the lower and upper bounds
> ###   of the middle 50 percent of the constricted intervals
> ### Aliases: MinMaxLines
... 20 lines ...
+                       Xj = c(1237, 1600),
+                       Xl = c(1000, 1000),
+                       qi = "Hazard Ratio",
+                       spin = TRUE, ci = 0.99)
> 
> # Find summary statistics of the constricted interval
> Sum <- MinMaxLines(Sim1, clean = TRUE)
Error in .check_names_df(x, j) : 
  undefined columns: Min, Lower50, Median, Upper50, Max
Calls: MinMaxLines -> [ -> [.tbl_df -> .check_names_df
Execution halted

checking re-building of vignette outputs ... NOTE
Error in re-building vignettes:
  ...
All Xl set to 0.
Quitting from lines 311-321 (simPH-overview.Rnw) 
Error: processing vignette 'simPH-overview.Rnw' failed with diagnostics:
Aesthetics must be either length 1 or the same as the data (35): x, y
Execution halted

```

## sjmisc (1.6)
Maintainer: Daniel Lüdecke <d.luedecke@uke.de>

0 errors | 0 warnings | 0 notes

## sjPlot (1.9.3)
Maintainer: Daniel Lüdecke <d.luedecke@uke.de>  
Bug reports: https://github.com/sjPlot/devel/issues

0 errors | 0 warnings | 0 notes

## solrium (0.3.0)
Maintainer: Scott Chamberlain <myrmecocystus@gmail.com>  
Bug reports: http://www.github.com/ropensci/solrium/issues

0 errors | 0 warnings | 0 notes

## sorvi (0.7.26)
Maintainer: Leo Lahti <louhos@googlegroups.com>  
Bug reports: https://github.com/ropengov/sorvi/issues

0 errors | 0 warnings | 0 notes

## SpaDES (1.1.1)
Maintainer: Alex M Chubaty <alexander.chubaty@canada.ca>  
Bug reports: https://github.com/PredictiveEcology/SpaDES/issues

1 error  | 0 warnings | 0 notes

```
checking package dependencies ... ERROR
Package required but not available: ‘secr’

Package suggested but not available for checking: ‘fastshp’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```

## sprintfr (0.1.0)
Maintainer: Brandon Taylor <brandon.taylor221@gmail.com>  
Bug reports: https://github.com/bramtayl/sprintfr/issues

1 error  | 0 warnings | 1 note 

```
checking examples ... ERROR
Running examples in ‘sprintfr-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: string_base
> ### Title: Get a list of base string components for building string formats
> ### Aliases: string_base
> ### Keywords: datasets
> 
> ### ** Examples
> 
> string_base
  integer octal hex double scientific auto binary string percent
1       i     o   x      f          e    g      a      s       %
> string_format(integer, " ", double)
Error: object 'data_frame' not found
Execution halted

checking re-building of vignette outputs ... NOTE
Error in re-building vignettes:
  ...
Quitting from lines 30-31 (sprintfr.Rmd) 
Error: processing vignette 'sprintfr.Rmd' failed with diagnostics:
object 'data_frame' not found
Execution halted

```

## srvyr (0.1.0)
Maintainer: Greg Freedman <greg.freedman@gmail.com>  
Bug reports: https://github.com/gergness/srvyr/issues

2 errors | 1 warning  | 0 notes

```
checking examples ... ERROR
Running examples in ‘srvyr-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: as_survey
> ### Title: Create a tbl_svy from a data.frame
> ### Aliases: as_survey as_survey.data.frame as_survey.survey.design2
> ###   as_survey.svyrepdesign as_survey.twophase2 as_survey_
> 
... 23 lines ...
> scd$rep2 <- 2 * c(1, 0, 0, 1, 0, 1)
> scd$rep3 <- 2 * c(0, 1, 1, 0, 0, 1)
> scd$rep4 <- 2 * c(0, 1, 0, 1, 1, 0)
> 
> scdrep <- scd %>%
+   as_survey(type = "BRR", repweights = starts_with("rep"),
+                     combined_weights = FALSE)
Error in eval(expr, envir, enclos) : 
  could not find function "starts_with"
Calls: %>% ... <Anonymous> -> <Anonymous> -> lapply -> FUN -> eval -> eval
Execution halted

checking tests ... ERROR
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  
  The following object is masked from 'package:stats':
  
      filter
  
  > 
  > test_check("srvyr")
  Error in eval(expr, envir, enclos) : 
    could not find function "starts_with"
  Calls: test_check ... <Anonymous> -> <Anonymous> -> lapply -> FUN -> eval -> eval
  testthat results ================================================================
  OK: 8 SKIPPED: 0 FAILED: 0
  Execution halted

checking S3 generic/method consistency ... WARNING
ungroup:
  function(x, ...)
ungroup.grouped_svy:
  function(x)

ungroup:
  function(x, ...)
ungroup.tbl_svy:
  function(x)

See section ‘Generic functions and methods’ in the ‘Writing R
Extensions’ manual.
```

## ss3sim (0.9.0)
Maintainer: Sean Anderson <sean@seananderson.ca>

1 error  | 0 warnings | 0 notes

```
checking examples ... ERROR
Running examples in ‘ss3sim-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: change_em_binning
> ### Title: Change population and observed length composition bins in an SS
> ###   estimation model
> ### Aliases: change_em_binning
> 
... 48 lines ...
13 2001    1   1      0    0      1       8      -1     3  1  1  1
14 2002    1   1      0    0      1       4       5     3  1  1  1
15 2002    1   1      0    0      1       5       6     3  1  1  1
16 2002    1   1      0    0      1       6       7     3  1  1  1
17 2002    1   1      0    0      1       7       8     3  1  1  1
18 2002    1   1      0    0      1       8      -1     3  1  1  1
> newdat <- change_em_binning(olddat, dat_file_out = NULL, bin_vector = c(4, 6, 8),
+   lbin_method = 1, write_file = FALSE)
Error in eval(expr, envir, enclos) : could not find function "matches"
Calls: change_em_binning ... select_vars_ -> <Anonymous> -> lapply -> FUN -> eval -> eval
Execution halted
```

## statar (0.6.0)
Maintainer: Matthieu Gomez <mattg@princeton.edu>  
Bug reports: https://github.com/matthieugomez/statar/issues

1 error  | 0 warnings | 1 note 

```
checking examples ... ERROR
Running examples in ‘statar-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: find_duplicates
> ### Title: returns a data.frame with duplicated rows
> ### Aliases: find_duplicates find_duplicates_
> 
> ### ** Examples
> 
> df <- data.frame(a = rep(1:2, each = 3), b = 1:6)
> find_duplicates(df, a)
2 groups have duplicates
Error in eval(expr, envir, enclos) : could not find function "everything"
Calls: find_duplicates ... select_vars_ -> <Anonymous> -> lapply -> FUN -> eval -> eval
Execution halted

checking dependencies in R code ... NOTE
Missing or unexported object: ‘dplyr::tbl_dt’
```

## stationaRy (0.4.1)
Maintainer: Richard Iannone <riannone@me.com>  
Bug reports: https://github.com/rich-iannone/stationaRy/issues

0 errors | 0 warnings | 1 note 

```
checking installed package size ... NOTE
  installed size is  8.1Mb
```

## stplanr (0.1.1)
Maintainer: Robin Lovelace <rob00x@gmail.com>  
Bug reports: https://github.com/ropensci/stplanr/issues

0 errors | 0 warnings | 0 notes

## strptimer (0.1.0)
Maintainer: Brandon Taylor <brandon.taylor221@gmail.com>

0 errors | 0 warnings | 0 notes

## SWMPr (2.1.4)
Maintainer: Marcus W. Beck <mbafs2012@gmail.com>  
Bug reports: https://github.com/fawda123/SWMPr/issues

1 error  | 0 warnings | 0 notes

```
checking examples ... ERROR
Running examples in ‘SWMPr-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: decomp_cj
> ### Title: Simple trend decomposition of monthly swmpr data
> ### Aliases: decomp_cj decomp_cj.default decomp_cj.swmpr
> 
> ### ** Examples
... 153 lines ...
144 2013-12-01  5.900000 1.1408163 0.7244194 1.4569683
> 
> ## decomposition of chl, ggplot
> decomp_cj(dat, param = 'chla_n')
Warning: Removed 6 rows containing missing values (geom_path).
> 
> ## decomposition changing argumens passed to decompTs
> decomp_cj(dat, param = 'chla_n', startyr = 2008, type = 'add')
Error in wq::decompTs(dat_mts, ...) : unused argument (startyr = 2008)
Calls: decomp_cj ... decomp_cj.swmpr -> decomp_cj -> decomp_cj.default -> <Anonymous>
Execution halted
```

## taber (0.1.0)
Maintainer: Seth Wenchel <seth@wenchel.com>  
Bug reports: http://github.com/restonslacker/taber/issues

0 errors | 0 warnings | 0 notes

## tadaatoolbox (0.9.0)
Maintainer: Lukas Burk <lukas@quantenbrot.de>  
Bug reports: https://github.com/tadaadata/tadaatoolbox/issues

0 errors | 0 warnings | 1 note 

```
checking data for non-ASCII characters ... NOTE
  Note: found 1 marked UTF-8 string
```

## tcR (2.2.1.7)
Maintainer: Vadim Nazarov <vdm.nazarov@gmail.com>  
Bug reports: https://github.com/imminfo/tcr/issues

0 errors | 0 warnings | 2 notes

```
checking installed package size ... NOTE
  installed size is  5.6Mb
  sub-directories of 1Mb or more:
    data   1.2Mb
    doc    3.9Mb

checking dependencies in R code ... NOTE
Namespace in Imports field not imported from: ‘scales’
  All declared Imports should be used.
```

## tempcyclesdata (1.0.1)
Maintainer: George Wang <biogeek@gmail.com>

0 errors | 0 warnings | 1 note 

```
checking installed package size ... NOTE
  installed size is  5.9Mb
  sub-directories of 1Mb or more:
    data   5.9Mb
```

## textreuse (0.1.2)
Maintainer: Lincoln Mullen <lincoln@lincolnmullen.com>  
Bug reports: https://github.com/ropensci/textreuse/issues

2 errors | 0 warnings | 1 note 

```
checking examples ... ERROR
Running examples in ‘textreuse-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: lsh_candidates
> ### Title: Candidate pairs from LSH comparisons
> ### Aliases: lsh_candidates
> 
> ### ** Examples
> 
> dir <- system.file("extdata/legal", package = "textreuse")
> minhash <- minhash_generator(200, seed = 234)
> corpus <- TextReuseCorpus(dir = dir,
+                           tokenizer = tokenize_ngrams, n = 5,
+                           minhash_func = minhash)
> buckets <- lsh(corpus, bands = 50)
> lsh_candidates(buckets)
Error: No variables selected
Execution halted

checking tests ... ERROR
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  
  2. Failure: prints sensibly (@test-TextReuseCorpus.R#46) -----------------------
  `corpus_a` produced no output
  
  
  Error: No variables selected
  testthat results ================================================================
  OK: 83 SKIPPED: 2 FAILED: 2
  1. Failure: prints sensibly (@test-TextReuseCorpus.R#45) 
  2. Failure: prints sensibly (@test-TextReuseCorpus.R#46) 
  
  Error: testthat unit tests failed
  Execution halted

checking re-building of vignette outputs ... NOTE
Error in re-building vignettes:
  ...

Attaching package: 'dplyr'

The following objects are masked from 'package:stats':

    filter, lag

The following objects are masked from 'package:base':

    intersect, setdiff, setequal, union

Quitting from lines 75-79 (textreuse-minhash.Rmd) 
Error: processing vignette 'textreuse-minhash.Rmd' failed with diagnostics:
No variables selected
Execution halted

```

## TH.data (1.0-7)
Maintainer: Torsten Hothorn <Torsten.Hothorn@R-project.org>

0 errors | 0 warnings | 1 note 

```
checking installed package size ... NOTE
  installed size is  5.2Mb
  sub-directories of 1Mb or more:
    data   1.1Mb
    rda    3.8Mb
```

## tidyjson (0.2.1)
Maintainer: Jeremy Stanley <jeremy.stanley@gmail.com>

0 errors | 0 warnings | 0 notes

## tidyr (0.4.1)
Maintainer: Hadley Wickham <hadley@rstudio.com>  
Bug reports: https://github.com/hadley/tidyr/issues

2 errors | 0 warnings | 1 note 

```
checking examples ... ERROR
Running examples in ‘tidyr-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: complete
> ### Title: Complete a data frame with missing combinations of data.
> ### Aliases: complete
> 
> ### ** Examples
... 13 lines ...

> df <- data_frame(
+   group = c(1:2, 1),
+   item_id = c(1:2, 2),
+   item_name = c("a", "b", "b"),
+   value1 = 1:3,
+   value2 = 4:6
+ )
> df %>% complete(group, nesting(item_id, item_name))
Error: No variables selected
Execution halted

checking tests ... ERROR
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  1. Error: nesting doesn't expand values (@test-expand.R#24) 
  2. Error: expand respects groups (@test-expand.R#50) 
  3. Error: missings filled down for each atomic vector (@test-fill.R#42) 
  4. Error: missings filled up for each vector (@test-fill.R#59) 
  5. Error: nest turns grouped values into one list-df (@test-nest.R#5) 
  6. Error: can control output column name (@test-nest.R#12) 
  7. Error: nest doesn't include grouping vars in nested data (@test-nest.R#18) 
  8. Error: can restrict variables in grouped nest (@test-nest.R#28) 
  9. Error: puts data into the correct row (@test-nest.R#34) 
  1. ...
  
  Error: testthat unit tests failed
  Execution halted

checking dependencies in R code ... NOTE
Missing or unexported object: ‘dplyr::tbl_dt’
```

## tigris (0.2.2)
Maintainer: Kyle Walker <kyle.walker@tcu.edu>

0 errors | 1 warning  | 0 notes

```
checking whether package ‘tigris’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import ‘maptools::nowrapSpatialLines’ by ‘sp::nowrapSpatialLines’ when loading ‘tigris’
See ‘/Users/hadley/Documents/dplyr/dplyr/revdep/checks/tigris.Rcheck/00install.out’ for details.
```

## titanic (0.1.0)
Maintainer: Paul Hendricks <paul.hendricks.2013@owu.edu>  
Bug reports: https://github.com/paulhendricks/titanic/issues

0 errors | 0 warnings | 0 notes

## Tmisc (0.1.5)
Maintainer: Stephen Turner <vustephen@gmail.com>

0 errors | 0 warnings | 0 notes

## traits (0.2.0)
Maintainer: Scott Chamberlain <myrmecocystus@gmail.com>  
Bug reports: http://www.github.com/ropensci/traits/issues

0 errors | 0 warnings | 0 notes

## treeplyr (0.1)
Maintainer: Josef Uyeda <josef.uyeda@gmail.com>

1 error  | 1 warning  | 0 notes

```
checking examples ... ERROR
Running examples in ‘treeplyr-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: filter_.treedata
> ### Title: Function for filtering rows from an object of class 'treedata'
> ### Aliases: filter.grouped_treedata filter.treedata
> ###   filter_.grouped_treedata filter_.treedata
> 
> ### ** Examples
> 
> data(anolis)
> td <- make.treedata(anolis$phy, anolis$dat, name_column=1)
> tdfilter <- filter(td, island=="Cuba", SVL > 3.5)
Error: filter() takes unnamed arguments. Do you need `==`?
Execution halted

checking S3 generic/method consistency ... WARNING
ungroup:
  function(x, ...)
ungroup.grouped_treedata:
  function(x)

See section ‘Generic functions and methods’ in the ‘Writing R
Extensions’ manual.
```

## turfR (0.8-7)
Maintainer: Jack Horne <jack@jackhorne.net>

0 errors | 0 warnings | 0 notes

## useful (1.2.0)
Maintainer: Jared P. Lander <packages@jaredlander.com>

1 error  | 0 warnings | 0 notes

```
checking tests ... ERROR
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  4. Failure: simple.impute.default properly imputes the mean (@test-simple-impute.r#41) 
  all.equal(...) produced no output
  
  
  testthat results ================================================================
  OK: 324 SKIPPED: 0 FAILED: 4
  1. Failure: simple.impute.default properly imputes the mean (@test-simple-impute.r#32) 
  2. Failure: simple.impute.default properly imputes the mean (@test-simple-impute.r#35) 
  3. Failure: simple.impute.default properly imputes the mean (@test-simple-impute.r#38) 
  4. Failure: simple.impute.default properly imputes the mean (@test-simple-impute.r#41) 
  
  Error: testthat unit tests failed
  Execution halted
```

## vdmR (0.2.2)
Maintainer: Tomokazu Fujino <fujino@fwu.ac.jp>

0 errors | 0 warnings | 0 notes

## vqtl (1.0)
Maintainer: Robert Corty <rcorty@gmail.com>

0 errors | 0 warnings | 0 notes

## wakefield (0.2.1)
Maintainer: Tyler Rinker <tyler.rinker@gmail.com>  
Bug reports: https://github.com/trinker/wakefield/issues

1 error  | 0 warnings | 0 notes

```
checking examples ... ERROR
Running examples in ‘wakefield-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: r_insert
> ### Title: Insert Data Frames Into 'r_data_frame'
> ### Aliases: r_insert
> ### Keywords: insert
> 
> ### ** Examples
> 
> dat <- dplyr::data_frame(
+     Age = age(100), Age = age(100), Age = age(100),
+     Smokes = smokes(n=100),
+     Sick = ifelse(Smokes, sample(5:10, 100, TRUE), sample(0:4, 100, TRUE)),
+     Death = ifelse(Smokes, sample(0:1, 100, TRUE, prob = c(.2, .8)),
+         sample(0:1, 100, TRUE, prob = c(.7, .3)))
+ )
Error: Each variable must have a unique name.
Problem variables: Age, Age.
Execution halted
```

## WHO (0.1)
Maintainer: Eric Persson <expersso5@gmail.com>  
Bug reports: https://www.github.com/expersso/WHO/issues

1 error  | 0 warnings | 0 notes

```
checking tests ... ERROR
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
            than three columns (@tests.R#19) 
  class(codes) not equal to c("tbl_df", "data.frame").
  Lengths differ: 1 vs 2
  
  
  testthat results ================================================================
  OK: 2168 SKIPPED: 0 FAILED: 2
  1. Failure: get_codes returns a data frame with positive length (@tests.R#12) 
  2. Failure: get_codes(TRUE) returns a data frame with positive length and more
            than three columns (@tests.R#19) 
  
  Error: testthat unit tests failed
  Execution halted
```

## wikipediatrend (1.1.10)
Maintainer: Peter Meissner <retep.meissner@gmail.com>  
Bug reports: https://github.com/petermeissner/wikipediatrend/issues

0 errors | 0 warnings | 1 note 

```
checking package dependencies ... NOTE
Packages suggested but not available for checking:
  ‘AnomalyDetection’ ‘BreakoutDetection’
```

## wordbankr (0.1)
Maintainer: Mika Braginsky <mika.br@gmail.com>  
Bug reports: http://github.com/langcog/wordbankr/issues

0 errors | 0 warnings | 2 notes

```
checking dependencies in R code ... NOTE
Namespace in Imports field not imported from: ‘RMySQL’
  All declared Imports should be used.

checking re-building of vignette outputs ... NOTE
Error in re-building vignettes:
  ...
Auto-disconnecting mysql connection (0, 1)
Joining by: "momed_id"
Auto-disconnecting mysql connection (0, 0)
Joining by: "momed_id"
Auto-disconnecting mysql connection (0, 2)
Auto-disconnecting mysql connection (0, 4)
Auto-disconnecting mysql connection (0, 3)
Auto-disconnecting mysql connection (0, 5)
Quitting from lines 43-47 (wordbankr.Rmd) 
Error: processing vignette 'wordbankr.Rmd' failed with diagnostics:
Elements 1, 2 of items %in% instrument_table$select are not true
Execution halted

```

## wrswoR.benchmark (0.1-1)
Maintainer: Kirill Müller <krlmlr+r@mailbox.org>  
Bug reports: https://github.com/krlmlr/wrswoR.benchmark/issues

0 errors | 0 warnings | 0 notes

## wrswoR (1.0-1)
Maintainer: Kirill Müller <krlmlr+r@mailbox.org>  
Bug reports: https://github.com/krlmlr/wrswoR/issues

0 errors | 0 warnings | 0 notes

## WufooR (0.6.1)
Maintainer: John Malc <cincenko@outlook.com>  
Bug reports: http://github.com/dmpe/wufoor/issues

0 errors | 0 warnings | 0 notes

## Zelig (5.0-11)
Maintainer: James Honaker <zelig-zee@iq.harvard.edu>

1 error  | 0 warnings | 0 notes

```
checking whether package ‘Zelig’ can be installed ... ERROR
Installation failed.
See ‘/Users/hadley/Documents/dplyr/dplyr/revdep/checks/Zelig.Rcheck/00install.out’ for details.
```

## ZeligChoice (0.9-0)
Maintainer: James Honaker <zelig-zee@iq.harvard.edu>

1 error  | 0 warnings | 0 notes

```
checking whether package ‘ZeligChoice’ can be installed ... ERROR
Installation failed.
See ‘/Users/hadley/Documents/dplyr/dplyr/revdep/checks/ZeligChoice.Rcheck/00install.out’ for details.
```

