# Setup

## Platform

|setting  |value                        |
|:--------|:----------------------------|
|version  |R version 3.3.0 (2016-05-03) |
|system   |x86_64, darwin13.4.0         |
|ui       |X11                          |
|language |(EN)                         |
|collate  |en_US.UTF-8                  |
|tz       |America/Chicago              |
|date     |2016-06-23                   |

## Packages

|package        |*  |version    |date       |source                           |
|:--------------|:--|:----------|:----------|:--------------------------------|
|assertthat     |   |0.1        |2013-12-06 |CRAN (R 3.3.0)                   |
|BH             |   |1.60.0-2   |2016-05-07 |CRAN (R 3.3.0)                   |
|covr           |   |2.1.0      |2016-06-21 |cran (@2.1.0)                    |
|DBI            |   |0.4-1      |2016-05-08 |CRAN (R 3.3.0)                   |
|dplyr          |   |0.4.3.9001 |2016-06-23 |local (hadley/dplyr@NA)          |
|dtplyr         |   |0.0.0.9001 |2016-06-23 |Github (hadley/dtplyr@57bd16f)   |
|ggplot2        |   |2.1.0      |2016-03-01 |CRAN (R 3.3.0)                   |
|knitr          |   |1.13       |2016-05-09 |CRAN (R 3.3.0)                   |
|Lahman         |   |4.0-1      |2015-09-15 |CRAN (R 3.3.0)                   |
|lazyeval       |   |0.2.0.9000 |2016-06-17 |Github (hadley/lazyeval@c155c3d) |
|magrittr       |   |1.5        |2014-11-22 |CRAN (R 3.3.0)                   |
|microbenchmark |   |1.4-2.1    |2015-11-25 |CRAN (R 3.3.0)                   |
|nycflights13   |   |0.2.0      |2016-04-30 |CRAN (R 3.3.0)                   |
|R6             |   |2.1.2      |2016-01-26 |CRAN (R 3.3.0)                   |
|Rcpp           |   |0.12.5     |2016-05-14 |CRAN (R 3.3.0)                   |
|rmarkdown      |   |0.9.6      |2016-05-01 |CRAN (R 3.3.0)                   |
|RMySQL         |   |0.10.9     |2016-05-08 |CRAN (R 3.3.0)                   |
|RPostgreSQL    |   |0.4-1      |2016-05-08 |CRAN (R 3.3.0)                   |
|RSQLite        |   |1.0.0      |2014-10-25 |CRAN (R 3.3.0)                   |
|testthat       |*  |1.0.2.9000 |2016-06-16 |Github (hadley/testthat@d3e20b9) |
|tibble         |   |1.0-12     |2016-06-23 |Github (hadley/tibble@1e5b140)   |

# Check results
297 packages

## ACDm (1.0.3)
Maintainer: Markus Belfrage <markus.belfrage@gmail.com>

0 errors | 0 warnings | 0 notes

## adegenet (2.0.1)
Maintainer: Thibaut Jombart <thibautjombart@gmail.com>

0 errors | 0 warnings | 0 notes

## admixturegraph (1.0.0)
Maintainer: Thomas Mailund <mailund@birc.au.dk>  
Bug reports: https://github.com/mailund/admixture_graph/issues

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

## afex (0.16-1)
Maintainer: Henrik Singmann <singmann+afex@gmail.com>

0 errors | 0 warnings | 0 notes

## alakazam (0.2.3)
Maintainer: Jason Vander Heiden <jason.vanderheiden@yale.edu>  
Bug reports: https://bitbucket.org/kleinstein/alakazam/issues

0 errors | 1 warning  | 0 notes

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Loading required package: ggplot2
Quitting from lines 78-109 (AminoAcids-Vignette.Rmd) 
Error: processing vignette 'AminoAcids-Vignette.Rmd' failed with diagnostics:
could not find function "starts_with"
Execution halted

```

## ameco (0.2.3)
Maintainer: Eric Persson <expersso5@gmail.com>  
Bug reports: http://github.com/expersso/ameco/issues

0 errors | 0 warnings | 1 note 

```
checking installed package size ... NOTE
  installed size is 15.3Mb
  sub-directories of 1Mb or more:
    data  15.2Mb
```

## archivist (2.0.4)
Maintainer: Przemyslaw Biecek <przemyslaw.biecek@gmail.com>  
Bug reports: https://github.com/pbiecek/archivist/issues

0 errors | 0 warnings | 0 notes

## ARTool (0.10.2)
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

2 errors | 0 warnings | 1 note 

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

checking R code for possible problems ... NOTE
maha_dist: no visible global function definition for ‘cov’
within_n_mads : <anonymous>: no visible global function definition for
  ‘mad’
within_n_mads : <anonymous>: no visible global function definition for
  ‘median’
within_n_sds : <anonymous>: no visible global function definition for
  ‘sd’
Undefined global functions or variables:
  cov mad median sd
Consider adding
  importFrom("stats", "cov", "mad", "median", "sd")
to your NAMESPACE file.
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

## bibliometrix (0.8)
Maintainer: Massimo Aria <aria@unina.it>

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

2 errors | 0 warnings | 0 notes

```
checking examples ... ERROR
Running examples in ‘broom-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: survfit_tidiers
> ### Title: tidy survival curve fits
> ### Aliases: glance.survfit survfit_tidiers tidy.survfit
> 
> ### ** Examples
... 48 lines ...

The following objects are masked from ‘package:stats’:

    filter, lag

The following objects are masked from ‘package:base’:

    intersect, setdiff, setequal, union

Error: Unknown column 'median'
Execution halted

checking tests ... ERROR
Running the tests in ‘tests/test-all.R’ failed.
Last 13 lines of output:
  4: identical(as.vector(object), TRUE) at /private/tmp/RtmpjWok2u/devtools5ce3473eae2e/hadley-testthat-d3e20b9/R/expectation.R:112
  5: as.vector(object)
  6: augmented$disp
  7: `$.tbl_df`(augmented, disp)
  8: stopc("Unknown column '", i, "'") at /private/tmp/RtmpxqBRhg/devtools1785e2d679c7f/hadley-tibble-1e5b140/R/tbl-df.r:37
  9: stop(..., call. = FALSE, domain = NA) at /private/tmp/RtmpxqBRhg/devtools1785e2d679c7f/hadley-tibble-1e5b140/R/utils.r:63
  
  testthat results ================================================================
  OK: 490 SKIPPED: 0 FAILED: 1
  1. Error: rowwise tidiers can be applied to sub-models (@test-rowwise.R#21) 
  
  Error: testthat unit tests failed
  Execution halted
```

## CARBayesST (2.3)
Maintainer: Duncan Lee <Duncan.Lee@glasgow.ac.uk>

0 errors | 0 warnings | 0 notes

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

## checkmate (1.8.0)
Maintainer: Michel Lang <michellang@gmail.com>  
Bug reports: https://github.com/mllg/checkmate/issues

0 errors | 0 warnings | 0 notes

## choroplethr (3.5.2)
Maintainer: Ari Lamstein <arilamstein@gmail.com>  
Bug reports: https://github.com/arilamstein/choroplethr/issues

0 errors | 0 warnings | 0 notes

## chromer (0.1)
Maintainer: Matthew Pennell <mwpennell@gmail.com>  
Bug reports: http://www.github.com/ropensci/chromer/issues/new

0 errors | 0 warnings | 2 notes

```
checking DESCRIPTION meta-information ... NOTE
Malformed Description field: should contain one or more complete sentences.

checking R code for possible problems ... NOTE
parse_counts: no visible global function definition for ‘na.omit’
Undefined global functions or variables:
  na.omit
Consider adding
  importFrom("stats", "na.omit")
to your NAMESPACE file.
```

## chunked (0.2.1)
Maintainer: Edwin de Jonge <edwindjonge@gmail.com>  
Bug reports: https://github.com/edwindj/chunked/issues

1 error  | 0 warnings | 0 notes

```
checking tests ... ERROR
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  incorrect number of dimensions
  1: write_chunkwise(iris2, tmp, row.names = FALSE) at testthat/test-write.R:29
  2: write_chunkwise.tbl_sql(iris2, tmp, row.names = FALSE)
  3: utils::write.table(h[0, ], file = file, col.names = col.names, row.names = row.names, 
         sep = sep, dec = dec, ...)
  4: is.data.frame(x)
  
  testthat results ================================================================
  OK: 26 SKIPPED: 0 FAILED: 1
  1. Error: write_chunkwise to db works (@test-write.R#29) 
  
  Error: testthat unit tests failed
  Execution halted
```

## codingMatrices (0.2.0)
Maintainer: Bill Venables <Bill.Venables@gmail.com>

0 errors | 0 warnings | 0 notes

## codyn (1.1.0)
Maintainer: Matthew B. Jones <jones@nceas.ucsb.edu>  
Bug reports: https://github.com/laurenmh/codyn/issues

0 errors | 0 warnings | 0 notes

## cofeatureR (1.0.1)
Maintainer: Fong Chun Chan <fongchunchan@gmail.com>  
Bug reports: https://github.com/tinyheero/cofeatureR/issues

0 errors | 0 warnings | 0 notes

## CollapsABEL (0.10.8)
Maintainer: Kaiyin Zhong <kindlychung@gmail.com>  
Bug reports: https://bitbucket.org/kindlychung/collapsabel2/issues

0 errors | 0 warnings | 0 notes

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

## coreSim (0.2)
Maintainer: Christopher Gandrud <christopher.gandrud@gmail.com>  
Bug reports: https://github.com/christophergandrud/coreSim/issues

0 errors | 0 warnings | 0 notes

## countytimezones (0.1.0)
Maintainer: Brooke Anderson <brooke.anderson@colostate.edu>  
Bug reports: https://github.com/geanders/countytimezones/issues

0 errors | 0 warnings | 0 notes

## crawl (2.0)
Maintainer: Devin S. Johnson <devin.johnson@noaa.gov>

0 errors | 0 warnings | 0 notes

## cricketr (0.0.13)
Maintainer: Tinniam V Ganesh <tvganesh.85@gmail.com>  
Bug reports: https://github.com/tvganesh/cricketr/issues

0 errors | 0 warnings | 0 notes

## datacheckr (0.1.1)
Maintainer: Joe Thorley <joe@poissonconsulting.ca>

0 errors | 0 warnings | 0 notes

## DataCombine (0.2.21)
Maintainer: Christopher Gandrud <christopher.gandrud@gmail.com>  
Bug reports: https://github.com/christophergandrud/DataCombine/issues

0 errors | 0 warnings | 0 notes

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

## dataRetrieval (2.5.5)
Maintainer: Laura DeCicco <ldecicco@usgs.gov>  
Bug reports: https://github.com/USGS-R/dataRetrieval/issues

0 errors | 0 warnings | 0 notes

## datastepr (0.0.1)
Maintainer: Brandon Taylor <brandon.taylor221@gmail.com>

0 errors | 1 warning  | 0 notes

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
          recursively.

_E_x_a_m_p_l_e_s:

     step = dataStepClass$new()
     
     frame = data.frame(x = 1:10)
... 8 lines ...
     }
     
     stairs()
     
     step$results
     

Quitting from lines 55-84 (datastepping.Rmd) 
Error: processing vignette 'datastepping.Rmd' failed with diagnostics:
incorrect length (2), expecting: 10
Execution halted
```

## ddpcr (1.3)
Maintainer: Dean Attali <daattali@gmail.com>  
Bug reports: https://github.com/daattali/ddpcr/issues

2 errors | 1 warning  | 0 notes

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

checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Quitting from lines 205-213 (extend.Rmd) 
Error: processing vignette 'extend.Rmd' failed with diagnostics:
ddpcr: there was a problem reading one or more of the data files
Execution halted

```

## DeLorean (1.2.2)
Maintainer: John Reid <john.reid@mrc-bsu.cam.ac.uk>

0 errors | 1 warning  | 0 notes

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Joining, by = c("cell", "capture")
Have 35 cells after filtering
Joining, by = "cell"
Have 20 genes after filtering
Joining, by = "gene"
Joining, by = "gene"
Joining, by = "gene"
... 8 lines ...
In file included from /Users/hadley/R-revdep/StanHeaders/include/src/stan/io/dump.hpp:11:
In file included from /Users/hadley/R-revdep/StanHeaders/include/src/stan/io/validate_zero_buf.hpp:4:
/Users/hadley/R-revdep/BH/include/boost/lexical_cast/bad_lexical_cast.hpp:33:54: error: redefinition of 'bad_lexical_cast'
    class __attribute__((__visibility__("default"))) bad_lexical_cast :
                                                     ^
/usr/local/include/boost/lexical_cast.hpp:75:54: note: previous definition is here
    class __attribute__((__visibility__("default"))) bad_lexical_cast :
                                                     ^
1 error generated.
make: *** [file6da35a13b51f.o] Error 1
Execution halted
```

## denovolyzeR (0.1.1)
Maintainer: James Ware <j.ware@imperial.ac.uk>  
Bug reports: http://github.com/jamesware/denovolyzeR/issues

0 errors | 0 warnings | 0 notes

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
  expect_equal( strings_addresses(names(df)) ,  strings_addresses(names(df4)) )
                                            ~^
  tests/testthat/utf-8.R:17:44: style: Commas should never have a space before.
  expect_equal( strings_addresses(names(df)) ,  strings_addresses(names(gdf4)) )
                                            ~^
  
  
  testthat results ================================================================
  OK: 7 SKIPPED: 0 FAILED: 1
  1. Failure: Package Style (@test-styling.R#4) 
  
  Error: testthat unit tests failed
  Execution halted
```

## DisimForMixed (0.2)
Maintainer: Hasanthi A. Pathberiya <hasanthi@sjp.ac.lk>

0 errors | 0 warnings | 0 notes

## DiversityOccupancy (1.0.5)
Maintainer: Derek Corcoran <derek.corcoran.barrios@gmail.com>

0 errors | 0 warnings | 0 notes

## docxtractr (0.1.0.9000)
Maintainer: Bob Rudis <bob@rudis.net>

0 errors | 0 warnings | 0 notes

## dotwhisker (0.2.0.5)
Maintainer: Yue Hu <yue-hu-1@uiowa.edu>  
Bug reports: https://github.com/fsolt/dotwhisker/issues

1 error  | 1 warning  | 0 notes

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

checking re-building of vignette outputs ... WARNING
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

## easyformatr (0.1.1)
Maintainer: Brandon Taylor <brandon.taylor221@gmail.com>  
Bug reports: https://github.com/bramtayl/easyformatr/issues

2 errors | 1 warning  | 0 notes

```
checking examples ... ERROR
Running examples in ‘easyformatr-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: easy_format
> ### Title: Easily build format strings
> ### Aliases: easy_format
> 
> ### ** Examples
> 
> easy_format(year, month, day, integer, octal, double)
Error in eval(expr, envir, enclos) : could not find function "one_of"
Calls: easy_format ... select_vars_ -> <Anonymous> -> lapply -> FUN -> eval -> eval
Execution halted

checking tests ... ERROR
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  53: select_vars_(names(.data), dots) at /Users/hadley/Documents/dplyr/dplyr/R/dataframe.R:97
  54: lazyeval::lazy_eval(args, names_list) at /Users/hadley/Documents/dplyr/dplyr/R/select-vars.R:69
  55: lapply(x, lazy_eval, data = data) at /private/tmp/RtmpBZGgmu/devtools500b57ff4370/hadley-lazyeval-c155c3d/R/lazy-eval.R:21
  56: FUN(X[[i]], ...)
  57: eval(x$expr, data, x$env) at /private/tmp/RtmpBZGgmu/devtools500b57ff4370/hadley-lazyeval-c155c3d/R/lazy-eval.R:27
  58: eval(expr, envir, enclos)
  
  testthat results ================================================================
  OK: 7 SKIPPED: 0 FAILED: 1
  1. Error: easy_format (@test.R#48) 
  
  Error: testthat unit tests failed
  Execution halted

checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...

Attaching package: 'dplyr'

The following objects are masked from 'package:stats':

    filter, lag

The following objects are masked from 'package:base':

    intersect, setdiff, setequal, union

Quitting from lines 39-40 (easyformatr.Rmd) 
Error: processing vignette 'easyformatr.Rmd' failed with diagnostics:
object 'data_frame' not found
Execution halted

```

## ecb (0.2)
Maintainer: Eric Persson <expersso5@gmail.com>

0 errors | 0 warnings | 0 notes

## ecoengine (1.10.0)
Maintainer: Karthik Ram <karthik.ram@gmail.com>  
Bug reports: https://github.com/ropensci/ecoengine/issues

0 errors | 0 warnings | 0 notes

## edeaR (0.4.1)
Maintainer: Gert Janssenswillen <gert.janssenswillen@uhasselt.be>

0 errors | 1 warning  | 0 notes

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...

Attaching package: 'edeaR'

The following object is masked from 'package:utils':

    timestamp

Quitting from lines 92-94 (descriptives.Rmd) 
Error: processing vignette 'descriptives.Rmd' failed with diagnostics:
Can not automatically convert from numeric to factor in column "length".
Execution halted

```

## eechidna (0.1)
Maintainer: Ben Marwick <benmarwick@gmail.com>

0 errors | 0 warnings | 0 notes

## eemR (0.1.3)
Maintainer: Philippe Massicotte <pm@bios.au.dk>  
Bug reports: https://github.com/PMassicotte/eemR/issues

0 errors | 0 warnings | 0 notes

## EFDR (0.1.1)
Maintainer: Andrew Zammit-Mangion <andrewzm@gmail.com>

0 errors | 0 warnings | 1 note 

```
checking R code for possible problems ... NOTE
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

## efreadr (0.1.1)
Maintainer: Marco Bascietto <marco.bascietto@crea.gov.it>

0 errors | 0 warnings | 0 notes

## elpatron (0.0.2)
Maintainer: Jordan Mackie <jmackie@protonmail.com>

1 error  | 1 warning  | 0 notes

```
checking examples ... ERROR
Running examples in ‘elpatron-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: clean_bikedata
> ### Title: Clean raw cycling device data.
> ### Aliases: clean_bikedata
> 
> ### ** Examples
> 
> ride_file <- system.file("extdata/lufbra.fit", package = "elpatron")
> 
> parsed_ride <- import_ride(ride_file, make_laps = TRUE)
Error: Length of logical index vector must be 1 or 18, got: 17
Execution halted

checking Rd cross-references ... WARNING
Missing link or links in documentation object 'pipe.Rd':
  ‘[dplyr]{chain}’

See section 'Cross-references' in the 'Writing R Extensions' manual.

```

## emil (2.2.6)
Maintainer: Christofer Backlin <emil@christofer.backlin.se>  
Bug reports: https://github.com/Molmed/emil/issues

0 errors | 0 warnings | 0 notes

## emuR (0.1.8)
Maintainer: Raphael Winkelmann <raphael@phonetik.uni-muenchen.de>  
Bug reports: https://github.com/IPS-LMU/emuR/issues

0 errors | 0 warnings | 0 notes

## engsoccerdata (0.1.5)
Maintainer: James Curley <jc3181@columbia.edu>

0 errors | 0 warnings | 0 notes

## enigma (0.2.0)
Maintainer: Scott Chamberlain <myrmecocystus@gmail.com>  
Bug reports: https://github.com/ropengov/enigma/issues

0 errors | 0 warnings | 0 notes

## estatapi (0.2)
Maintainer: Hiroaki Yutani <yutani.ini@gmail.com>

0 errors | 0 warnings | 0 notes

## etl (0.3.1)
Maintainer: Ben Baumer <ben.baumer@gmail.com>  
Bug reports: https://github.com/beanumber/etl/issues

0 errors | 0 warnings | 0 notes

## eurostat (1.2.21)
Maintainer: Lahti Leo <louhos@googlegroups.com>  
Bug reports: https://github.com/ropengov/eurostat/issues

0 errors | 0 warnings | 0 notes

## explor (0.2.1)
Maintainer: Julien Barnier <julien.barnier@ens-lyon.fr>  
Bug reports: https://github.com/juba/explor/issues

0 errors | 0 warnings | 0 notes

## eyelinker (0.1)
Maintainer: Simon Barthelme <simon.barthelme@gipsa-lab.fr>

0 errors | 0 warnings | 0 notes

## eyetrackingR (0.1.6)
Maintainer: Jacob Dink <jacobwdink@gmail.com>  
Bug reports: https://github.com/jwdink/eyetrackingR/issues

0 errors | 0 warnings | 0 notes

## ezec (1.0.0)
Maintainer: Zhian N. Kamvar <kamvarz@science.oregonstate.edu>  
Bug reports: https://github.com/grunwaldlab/ezec/issues

0 errors | 0 warnings | 0 notes

## ezsummary (0.1.9)
Maintainer: Hao Zhu <haozhu@hsl.harvard.edu>

0 errors | 0 warnings | 0 notes

## FactoMineR (1.33)
Maintainer: Francois Husson <francois.husson@agrocampus-ouest.fr>

0 errors | 0 warnings | 0 notes

## feather (0.0.1)
Maintainer: Hadley Wickham <hadley@rstudio.com>  
Bug reports: https://github.com/wesm/feather/issues

0 errors | 0 warnings | 0 notes

## finreportr (1.0.0)
Maintainer: Seward Lee <sewardlee337@gmail.com>

0 errors | 0 warnings | 0 notes

## fitcoach (1.0)
Maintainer: Niraj Juneja <njuneja@gmail.com>

0 errors | 0 warnings | 0 notes

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

0 errors | 0 warnings | 1 note 

```
checking R code for possible problems ... NOTE
.corfreq: no visible global function definition for ‘complete.cases’
.covfreq: no visible global function definition for ‘complete.cases’
.hclustvfreq: no visible global function definition for
  ‘complete.cases’
.quantilefreq : <anonymous>: no visible global function definition for
  ‘approx’
coef.biglmfreq: no visible global function definition for ‘coef’
predict.biglmfreq: no visible global function definition for ‘predict’
predict.lmfreq: no visible global function definition for ‘predict’
summary.lmfreq: no visible global function definition for ‘coef’
summary.lmfreq: no visible global function definition for ‘pt’
summary.lmfreq: no visible global function definition for ‘AIC’
update.biglmfreq: no visible global function definition for ‘update’
Undefined global functions or variables:
  AIC approx coef complete.cases predict pt update
Consider adding
  importFrom("stats", "AIC", "approx", "coef", "complete.cases",
             "predict", "pt", "update")
to your NAMESPACE file.
```

## FSA (0.8.7)
Maintainer: Derek Ogle <derek@derekogle.com>  
Bug reports: https://github.com/droglenc/FSA/issues

0 errors | 0 warnings | 1 note 

```
checking Rd cross-references ... NOTE
Packages unavailable to check Rd xrefs: ‘alr3’, ‘prettyR’, ‘RMark’, ‘asbio’, ‘PMCMR’, ‘pgirmess’, ‘agricolae’
```

## fueleconomy (0.1)
Maintainer: 'Hadley Wickham' <h.wickham@gmail.com>

0 errors | 0 warnings | 0 notes

## futureheatwaves (1.0.1)
Maintainer: Brooke Anderson <brooke.anderson@colostate.edu>

0 errors | 0 warnings | 0 notes

## fuzzyjoin (0.1)
Maintainer: David Robinson <drobinson@stackoverflow.com>

0 errors | 0 warnings | 0 notes

## gapminder (0.2.0)
Maintainer: Jennifer Bryan <jenny@stat.ubc.ca>  
Bug reports: https://github.com/jennybc/gapminder/issues

0 errors | 0 warnings | 0 notes

## GenCAT (1.0.3)
Maintainer: Eric Reed <reeder@bu.edu>

1 error  | 1 warning  | 3 notes

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

checking re-building of vignette outputs ... WARNING
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


checking package dependencies ... NOTE
Package suggested but not available for checking: ‘snpStats’

checking Rd cross-references ... NOTE
Package unavailable to check Rd xrefs: ‘snpStats’

checking data for non-ASCII characters ... NOTE
  Error in .requirePackage(package) : 
    unable to find required package 'snpStats'
  Calls: <Anonymous> ... .extendsForS3 -> extends -> getClassDef -> .requirePackage
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

## geomnet (0.1.0)
Maintainer: Samantha Tyner <sctyner@iastate.edu>  
Bug reports: https://github.com/sctyner/geomnet/issues

0 errors | 0 warnings | 0 notes

## ggalt (0.1.1)
Maintainer: Bob Rudis <bob@rudis.net>  
Bug reports: https://github.com/hrbrmstr/ggalt/issues

0 errors | 0 warnings | 0 notes

## ggfortify (0.2.0)
Maintainer: Masaaki Horikoshi <sinhrks@gmail.com>

0 errors | 0 warnings | 1 note 

```
checking installed package size ... NOTE
  installed size is  5.3Mb
  sub-directories of 1Mb or more:
    doc   4.9Mb
```

## ggmap (2.6.1)
Maintainer: David Kahle <david.kahle@gmail.com>  
Bug reports: https://github.com/dkahle/ggmap/issues

0 errors | 0 warnings | 0 notes

## ggmcmc (1.0)
Maintainer: Xavier Fernández i Marín <xavier.fim@gmail.com>  
Bug reports: https://github.com/xfim/ggmcmc/issues

1 error  | 0 warnings | 0 notes

```
checking examples ... ERROR
Running examples in ‘ggmcmc-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: ggs_caterpillar
> ### Title: Caterpillar plot with thick and thin CI
> ### Aliases: ggs_caterpillar
> 
> ### ** Examples
> 
> data(linear)
> ggs_caterpillar(ggs(s))
> ggs_caterpillar(list(A=ggs(s), B=ggs(s))) # silly example duplicating the same model
Error: Unknown column 'description'
Execution halted
```

## ggpmisc (0.2.9)
Maintainer: Pedro J. Aphalo <pedro.aphalo@helsinki.fi>  
Bug reports: https://bitbucket.org/aphalo/ggpmisc

0 errors | 0 warnings | 0 notes

## ggRandomForests (2.0.0)
Maintainer: John Ehrlinger <john.ehrlinger@gmail.com>  
Bug reports: https://github.com/ehrlinger/ggRandomForests/issues

0 errors | 0 warnings | 1 note 

```
checking installed package size ... NOTE
  installed size is  5.1Mb
  sub-directories of 1Mb or more:
    data   2.6Mb
    doc    2.1Mb
```

## ggraptR (0.1)
Maintainer: Eugene Dubossarsky <eugene@presciient.com>  
Bug reports: https://github.com/cargomoose/raptR/issues

0 errors | 0 warnings | 0 notes

## ggspectra (0.1.7)
Maintainer: Pedro J. Aphalo <pedro.aphalo@helsinki.fi>  
Bug reports: https://bitbucket.org/aphalo/ggspectra

1 error  | 1 warning  | 0 notes

```
checking examples ... ERROR
Running examples in ‘ggspectra-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: ggspectra-package
> ### Title: Extensions to 'ggplot2' for Radiation Spectra
> ### Aliases: ggspectra ggspectra-package
> 
> ### ** Examples
... 17 lines ...
+   stat_peaks(span = 21, geom = "point", colour = "red") +
+   stat_peaks(span = 51, geom = "text", colour = "red", vjust = -0.3,
+              label.fmt = "%3.0f nm")
> 
> ggplot(sun.spct, aes(w.length, s.e.irrad)) + geom_line() +
+   stat_color() + scale_color_identity()
> 
> plot(sun.spct)
> plot(polyester.spct, UV_bands(), range = UV())
Error: evaluation nested too deeply: infinite recursion / options(expressions=)?
Execution halted

checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Error: processing vignette 'user-guide.Rmd' failed with diagnostics:
evaluation nested too deeply: infinite recursion / options(expressions=)?
Execution halted

```

## ggvis (0.4.2)
Maintainer: Winston Chang <winston@rstudio.com>

1 error  | 0 warnings | 1 note 

```
checking tests ... ERROR
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  [8] 7 - 2 == 5
  [9] 7 - 2 == 5
  ...
  
  
  testthat results ================================================================
  OK: 444 SKIPPED: 0 FAILED: 1
  1. Failure: Automatic width (@test-compute-bin.r#143) 
  
  Error: testthat unit tests failed
  In addition: Warning message:
  In bind_rows_(x, .id) : Unequal factor levels: coercing to character
  Execution halted

checking R code for possible problems ... NOTE
adjust_breaks: no visible global function definition for ‘median’
bin_params.POSIXct: no visible global function definition for ‘is’
bin_vector.POSIXct: no visible global function definition for ‘is’
combine_data_props: no visible global function definition for
  ‘setNames’
combine_data_props : <anonymous>: no visible global function definition
  for ‘setNames’
compute_boxplot.data.frame: no visible global function definition for
  ‘quantile’
... 24 lines ...
  ‘packageVersion’
Undefined global functions or variables:
  complete.cases formula is median na.omit packageVersion predict qt
  quantile runif setNames terms
Consider adding
  importFrom("methods", "is")
  importFrom("stats", "complete.cases", "formula", "median", "na.omit",
             "predict", "qt", "quantile", "runif", "setNames", "terms")
  importFrom("utils", "packageVersion")
to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
contains 'methods').
```

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

## glycanr (0.3.0)
Maintainer: Ivo Ugrina <ivo@iugrina.com>  
Bug reports: https://github.com/iugrina/glycanr/issues

2 errors | 0 warnings | 1 note 

```
checking examples ... ERROR
Running examples in ‘glycanr-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: quantilenorm
> ### Title: Quantile Normalization of glycan data
> ### Aliases: quantilenorm
> 
> ### ** Examples
> 
> data(mpiu)
> mpiun <- quantilenorm(mpiu)
Error in quantilenorm(mpiu) : 
  Unable to proceed since package preprocessCore from
        BioConductor is not available on this system. This
        package is a prerequisite to use the quantilenorm function!
Execution halted

checking tests ... ERROR
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  
  The following objects are masked from 'package:base':
  
      intersect, setdiff, setequal, union
  
  Error in quantilenorm(mpiu, transpose = TRUE) : 
    Unable to proceed since package preprocessCore from
          BioConductor is not available on this system. This
          package is a prerequisite to use the quantilenorm function!
  Calls: test_check ... force -> source_file -> eval -> eval -> quantilenorm
  testthat results ================================================================
  OK: 5 SKIPPED: 0 FAILED: 0
  Execution halted

checking package dependencies ... NOTE
Package suggested but not available for checking: ‘preprocessCore’
```

## googlesheets (0.2.0)
Maintainer: Jennifer Bryan <jenny@stat.ubc.ca>  
Bug reports: https://github.com/jennybc/googlesheets/issues

0 errors | 0 warnings | 0 notes

## graphTweets (0.3.2)
Maintainer: John Coene <jcoenep@gmail.com>  
Bug reports: https://github.com/JohnCoene/graphTweets/issues

0 errors | 0 warnings | 0 notes

## Greg (1.2)
Maintainer: Max Gordon <max@gforge.se>

0 errors | 0 warnings | 1 note 

```
checking Rd cross-references ... NOTE
Package unavailable to check Rd xrefs: ‘rmeta’
```

## growthcurver (0.2.1)
Maintainer: Kathleen sprouffske <sprouffske@gmail.com>  
Bug reports: https://github.com/sprouffske/growthcurver/issues

0 errors | 0 warnings | 0 notes

## GSODR (0.1.7)
Maintainer: Adam Sparks <adamhsparks@gmail.com>  
Bug reports: https://github.com/adamhsparks/GSODR/issues

0 errors | 0 warnings | 0 notes

## gunsales (0.1.1)
Maintainer: Dirk Eddelbuettel <edd@debian.org>

0 errors | 0 warnings | 0 notes

## gutenbergr (0.1.1)
Maintainer: David Robinson <admiral.david@gmail.com>  
Bug reports: http://github.com/ropenscilabs/gutenbergr/issues

2 errors | 0 warnings | 0 notes

```
checking examples ... ERROR
Running examples in ‘gutenbergr-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: gutenberg_strip
> ### Title: Strip header and footer content from a Project Gutenberg book
> ### Aliases: gutenberg_strip
> 
> ### ** Examples
... 10 lines ...

The following objects are masked from ‘package:base’:

    intersect, setdiff, setequal, union

> book <- gutenberg_works(title == "Pride and Prejudice") %>%
+   gutenberg_download(strip = FALSE)
Determining mirror for Project Gutenberg from http://www.gutenberg.org/robot/harvest
Using mirror http://www.gutenberg.lib.md.us
Error: Unknown column 'gutenberg_id'
Execution halted

checking tests ... ERROR
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  7: `$.tbl_df`(w_de, language)
  8: stopc("Unknown column '", i, "'") at /private/tmp/RtmpxqBRhg/devtools1785e2d679c7f/hadley-tibble-1e5b140/R/tbl-df.r:37
  9: stop(..., call. = FALSE, domain = NA) at /private/tmp/RtmpxqBRhg/devtools1785e2d679c7f/hadley-tibble-1e5b140/R/utils.r:63
  
  testthat results ================================================================
  OK: 31 SKIPPED: 0 FAILED: 4
  1. Error: Can download books from a data frame with gutenberg_id column (@test-download.R#40) 
  2. Error: gutenberg_works does appropriate filtering by default (@test-metadata.R#8) 
  3. Error: gutenberg_works takes filtering conditions (@test-metadata.R#17) 
  4. Error: gutenberg_works does appropriate filtering by language (@test-metadata.R#23) 
  
  Error: testthat unit tests failed
  Execution halted
```

## haven (0.2.0)
Maintainer: Hadley Wickham <hadley@rstudio.com>  
Bug reports: https://github.com/hadley/haven/issues

1 error  | 0 warnings | 1 note 

```
checking examples ... ERROR
Running examples in ‘haven-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: read_sas
> ### Title: Read SAS files.
> ### Aliases: read_sas
> 
> ### ** Examples
> 
> read_sas("http://crn.cancer.gov/resources/ctcodes-procedures.sas7bdat")
Error in download.file(path, tmp, quiet = TRUE, mode = "wb") : 
  cannot open URL 'http://crn.cancer.gov/resources/ctcodes-procedures.sas7bdat'
Calls: read_sas -> df_parse_sas -> clean_path -> download.file
Execution halted

checking R code for possible problems ... NOTE
clean_path: no visible global function definition for ‘download.file’
Undefined global functions or variables:
  download.file
Consider adding
  importFrom("utils", "download.file")
to your NAMESPACE file.
```

## hdr (0.1)
Maintainer: Eric Persson <expersso5@gmail.com>  
Bug reports: https://github.com/expersso/hdr

0 errors | 0 warnings | 0 notes

## heemod (0.3.2)
Maintainer: Antoine Filipovic-Pierucci <pierucci@gmail.com>  
Bug reports: https://github.com/pierucci/heemod/issues

0 errors | 0 warnings | 0 notes

## highcharter (0.3.0)
Maintainer: Joshua Kunst <jbkunst@gmail.com>  
Bug reports: https://github.com/jbkunst/highcharter/issues

0 errors | 0 warnings | 0 notes

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

## hypothesisr (0.1.0)
Maintainer: Matthew Lincoln <matthew.d.lincoln@gmail.com>  
Bug reports: https://github.com/mdlincoln/hypothesisr/issues

0 errors | 0 warnings | 0 notes

## IAT (0.3)
Maintainer: Dan Martin <dpmartin42@gmail.com>

0 errors | 0 warnings | 0 notes

## IATscores (0.1-2)
Maintainer: Giulio Costantini <costantinigiulio@gmail.com>

1 error  | 0 warnings | 0 notes

```
checking package dependencies ... ERROR
Package required but not available: ‘nem’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```

## idbr (0.1.2)
Maintainer: Kyle Walker <kyle.walker@tcu.edu>

0 errors | 0 warnings | 0 notes

## imager (0.20)
Maintainer: Simon Barthelme <simon.barthelme@gipsa-lab.fr>

1 error  | 0 warnings | 0 notes

```
checking whether package ‘imager’ can be installed ... ERROR
Installation failed.
See ‘/Users/hadley/Documents/dplyr/dplyr/revdep/checks/imager.Rcheck/00install.out’ for details.
```

## IMP (1.1)
Maintainer: Anup Nair <nairanup50695@gmail.com>

0 errors | 0 warnings | 0 notes

## IncucyteDRC (0.5.4)
Maintainer: Phil Chapman <phil.chapman@cruk.manchester.ac.uk>  
Bug reports: https://github.com/chapmandu2/IncucyteDRC/issues

0 errors | 0 warnings | 0 notes

## internetarchive (0.1.5)
Maintainer: Lincoln Mullen <lincoln@lincolnmullen.com>  
Bug reports: https://github.com/ropensci/internetarchive/issues

0 errors | 0 warnings | 0 notes

## JacobiEigen (0.2-2)
Maintainer: Bill Venables <Bill.Venables@gmail.com>

0 errors | 0 warnings | 0 notes

## janeaustenr (0.1.1)
Maintainer: Julia Silge <julia.silge@gmail.com>  
Bug reports: https://github.com/juliasilge/janeaustenr/issues

0 errors | 0 warnings | 0 notes

## labelled (0.2.3)
Maintainer: Joseph Larmarange <joseph@larmarange.net>  
Bug reports: https://github.com/larmarange/labelled/issues

0 errors | 0 warnings | 2 notes

```
checking package dependencies ... NOTE
Package which this enhances but not available for checking: ‘memisc’

checking Rd cross-references ... NOTE
Package unavailable to check Rd xrefs: ‘memisc’
```

## laketemps (0.5.1)
Maintainer: Jordan S Read <jread@usgs.gov>  
Bug reports: https://github.com/USGS-R/laketemps/issues

0 errors | 0 warnings | 0 notes

## livechatR (0.1.0)
Maintainer: Lawrence Wu <lwu@payoff.com>

0 errors | 0 warnings | 0 notes

## LocFDRPois (1.0.0)
Maintainer: Kris Sankaran <kriss1@stanford.edu>

0 errors | 0 warnings | 1 note 

```
checking R code for possible problems ... NOTE
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

## longurl (0.1.1)
Maintainer: Bob Rudis <bob@rudis.net>

0 errors | 0 warnings | 0 notes

## lookupTable (0.1)
Maintainer: Enzo Jia <enzo.jia@gmail.com>

0 errors | 0 warnings | 0 notes

## loopr (1.0.1)
Maintainer: Brandon Taylor <Brandon.Taylor221@gmail.com>

0 errors | 0 warnings | 1 note 

```
checking R code for possible problems ... NOTE
amendColumns: no visible global function definition for ‘setNames’
fillColumns: no visible global function definition for ‘setNames’
Undefined global functions or variables:
  setNames
Consider adding
  importFrom("stats", "setNames")
to your NAMESPACE file.
```

## macleish (0.3.0)
Maintainer: Ben Baumer <ben.baumer@gmail.com>

0 errors | 0 warnings | 0 notes

## manifestoR (1.2.1)
Maintainer: Jirka Lewandowski <jirka.lewandowski@wzb.eu>  
Bug reports: https://github.com/ManifestoProject/manifestoR/issues

0 errors | 0 warnings | 0 notes

## Matrix.utils (0.9.1)
Maintainer: Craig Varrichio <canthony427@gmail.com>

0 errors | 0 warnings | 0 notes

## MazamaSpatialUtils (0.4.3)
Maintainer: Jonathan Callahan <jonathan.s.callahan@gmail.com>

0 errors | 0 warnings | 0 notes

## merTools (0.2.1)
Maintainer: Jared E. Knowles <jknowles@gmail.com>  
Bug reports: https://www.github.com/jknowles/merTools

0 errors | 0 warnings | 0 notes

## metricsgraphics (0.9.0)
Maintainer: Bob Rudis <bob@rudis.net>  
Bug reports: https://github.com/hrbrmstr/metricsgraphics/issues

0 errors | 0 warnings | 0 notes

## metricTester (1.0.2)
Maintainer: Eliot Miller <eliot.isaac@gmail.com>  
Bug reports: https://github.com/eliotmiller/metricTester/issues

0 errors | 0 warnings | 0 notes

## mixOmics (6.0.0)
Maintainer: Kim-Anh Le Cao <k.lecao@uq.edu.au>  
Bug reports: mixomics@math.univ-toulouse.fr or
        https://bitbucket.org/klecao/package-mixomics/issues

0 errors | 0 warnings | 0 notes

## mlVAR (0.3.0)
Maintainer: Sacha Epskamp <mail@sachaepskamp.com>

0 errors | 0 warnings | 0 notes

## modellingTools (0.1.0)
Maintainer: Alex Stringer <alex@alexstringer.ca>

2 errors | 1 warning  | 0 notes

```
checking examples ... ERROR
Running examples in ‘modellingTools-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: create_model_matrix
> ### Title: Create a usable model matrix from a data frame containing a mix
> ###   of continuous and categorical variables
> ### Aliases: create_model_matrix
> 
... 7 lines ...
The following objects are masked from ‘package:stats’:

    filter, lag

The following objects are masked from ‘package:base’:

    intersect, setdiff, setequal, union

Error: Each variable must have a unique name.
Problem variables: 'var1', 'var1', 'var1', 'var1'
Execution halted

checking tests ... ERROR
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  Type 'demo()' for some demos, 'help()' for on-line help, or
  'help.start()' for an HTML browser interface to help.
  Type 'q()' to quit R.
  
  > library(testthat)
  > library(modellingTools)
  > 
  > test_check("modellingTools")
  Error: Each variable must have a unique name.
  Problem variables: 'var1', 'var1', 'var1'
  testthat results ================================================================
  OK: 53 SKIPPED: 0 FAILED: 0
  Execution halted

checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...

Attaching package: 'dplyr'

The following objects are masked from 'package:stats':

    filter, lag

The following objects are masked from 'package:base':

    intersect, setdiff, setequal, union

Quitting from lines 313-319 (modellingTools.Rmd) 
Error: processing vignette 'modellingTools.Rmd' failed with diagnostics:
Each variable must have a unique name.
Problem variables: 'var1', 'var1', 'var1', 'var1', 'var1', 'var1', 'var1'
Execution halted

```

## Momocs (1.0.0)
Maintainer: Vincent Bonhomme <bonhomme.vincent@gmail.com>  
Bug reports: https://github.com/vbonhomme/Momocs/issues

0 errors | 0 warnings | 0 notes

## MonetDBLite (0.3.1)
Maintainer: Hannes Muehleisen <hannes@cwi.nl>  
Bug reports: https://github.com/hannesmuehleisen/MonetDBLite/issues

0 errors | 0 warnings | 1 note 

```
checking installed package size ... NOTE
  installed size is  5.9Mb
  sub-directories of 1Mb or more:
    libs   5.7Mb
```

## morse (2.2.0)
Maintainer: Philippe Veber <philippe.veber@univ-lyon1.fr>

1 error  | 0 warnings | 0 notes

```
checking whether package ‘morse’ can be installed ... ERROR
Installation failed.
See ‘/Users/hadley/Documents/dplyr/dplyr/revdep/checks/morse.Rcheck/00install.out’ for details.
```

## mosaic (0.14)
Maintainer: Randall Pruim <rpruim@calvin.edu>  
Bug reports: https://github.com/ProjectMOSAIC/mosaic/issues

0 errors | 0 warnings | 1 note 

```
checking installed package size ... NOTE
  installed size is  9.2Mb
  sub-directories of 1Mb or more:
    R     1.8Mb
    doc   6.8Mb
```

## mosaicData (0.14.0)
Maintainer: Randall Pruim <rpruim@calvin.edu>

0 errors | 0 warnings | 0 notes

## mscstexta4r (0.1.2)
Maintainer: Phil Ferriere <pferriere@hotmail.com>  
Bug reports: http://www.github.com/philferriere/mscstexta4r/issues

0 errors | 0 warnings | 0 notes

## mtconnectR (1.0.0)
Maintainer: Subramanyam Ravishankar <subramanyam@systeminsights.com>

1 error  | 0 warnings | 0 notes

```
checking examples ... ERROR
Running examples in ‘mtconnectR-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: map_gcode_mtc
> ### Title: Create a mapping between simulated and actual data
> ### Aliases: map_gcode_mtc
> 
> ### ** Examples
> 
> data("example_gcode_parsed") # Parsed gcode
> data("example_mtc_device_3") # MTCDevice object of actual log data
> simulated_gcode_data = na.omit(simulate_data_from_gcode(example_gcode_parsed, 
+ start_time = 0, data_res = 0.1, data_type = "HH"))
Error in eval(expr, envir, enclos) : could not find function "one_of"
Calls: na.omit ... select_vars_ -> <Anonymous> -> lapply -> FUN -> eval -> eval
Execution halted
```

## muir (0.1.0)
Maintainer: Justin Alford <justin.alford@gmail.com>  
Bug reports: https://github.com/alforj/muir/issues

0 errors | 0 warnings | 0 notes

## myTAI (0.4.0)
Maintainer: Hajk-Georg Drost <hgd23@cam.ac.uk>  
Bug reports: https://github.com/HajkD/myTAI/issues

1 error  | 0 warnings | 0 notes

```
checking package dependencies ... ERROR
Package required but not available: ‘edgeR’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```

## nasadata (0.9.0)
Maintainer: Eduardo Flores <eduardo@enelmargen.org>

0 errors | 0 warnings | 0 notes

## ncappc (0.2.1.1)
Maintainer: Chayan Acharya <chayan.acharya@farmbio.uu.se>

0 errors | 0 warnings | 0 notes

## NCmisc (1.1.4)
Maintainer: Nicholas Cooper <nick.cooper@cimr.cam.ac.uk>

0 errors | 0 warnings | 3 notes

```
checking package dependencies ... NOTE
Package suggested but not available for checking: ‘BiocInstaller’

checking R code for possible problems ... NOTE
Dim: no visible global function definition for ‘is’
display.var: no visible global function definition for ‘is’
has.method: no visible global function definition for ‘showMethods’
headl: no visible global function definition for ‘is’
prv: no visible global function definition for ‘is’
summarise.r.datasets: no visible global function definition for ‘is’
Undefined global functions or variables:
  is showMethods
Consider adding
  importFrom("methods", "is", "showMethods")
to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
contains 'methods').

checking Rd cross-references ... NOTE
Package unavailable to check Rd xrefs: ‘reader’
```

## networkreporting (0.1.1)
Maintainer: Dennis M. Feehan <feehan@berkeley.edu>

0 errors | 0 warnings | 0 notes

## NPC (1.1.0)
Maintainer: Devin Caughey <devin.caughey@gmail.com>

0 errors | 0 warnings | 0 notes

## nullabor (0.3.1)
Maintainer: Di Cook <dicook@iastate.edu>

0 errors | 0 warnings | 1 note 

```
checking R code for possible problems ... NOTE
box_dist: no visible global function definition for ‘quantile’
calc_mean_dist: no visible global function definition for ‘filter’
n: no visible global function definition for ‘resid’
null_dist : <anonymous>: no visible global function definition for
  ‘coef’
null_lm : <anonymous>: no visible global function definition for
  ‘predict’
reg_dist: no visible global function definition for ‘coef’
reg_dist: no visible global function definition for ‘lm’
... 10 lines ...
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

## nycflights13 (0.2.0)
Maintainer: Hadley Wickham <hadley@rstudio.com>  
Bug reports: https://github.com/hadley/nycflights13/issues

0 errors | 0 warnings | 1 note 

```
checking installed package size ... NOTE
  installed size is  7.0Mb
  sub-directories of 1Mb or more:
    data   6.9Mb
```

## openair (1.8-6)
Maintainer: David Carslaw <david.carslaw@york.ac.uk>  
Bug reports: https://github.com/davidcarslaw/openair/issues

0 errors | 0 warnings | 0 notes

## opencage (0.1.0)
Maintainer: Maëlle Salmon <maelle.salmon@yahoo.se>  
Bug reports: http://github.com/ropenscilabs/opencage/issues

0 errors | 0 warnings | 0 notes

## PAC (1.0.5)
Maintainer: Dangna Li <ldangna@gmail.com>

0 errors | 0 warnings | 1 note 

```
checking R code for possible problems ... NOTE
PAC : clusterDist: no visible global function definition for ‘var’
PAC: no visible global function definition for ‘complete.cases’
Undefined global functions or variables:
  complete.cases var
Consider adding
  importFrom("stats", "complete.cases", "var")
to your NAMESPACE file.
```

## packagetrackr (0.1.1)
Maintainer: Jirka Lewandowski <jirka.lewandowski@wzb.eu>  
Bug reports: 
        http://gitlab.points-of-interest.cc/points-of-interest/packagetrackr/issues

0 errors | 0 warnings | 0 notes

## peptider (0.2.2)
Maintainer: Eric Hare <erichare@iastate.edu>  
Bug reports: https://github.com/heike/peptider/issues

0 errors | 0 warnings | 0 notes

## photobiology (0.9.8)
Maintainer: Pedro J. Aphalo <pedro.aphalo@helsinki.fi>  
Bug reports: https://bitbucket.org/aphalo/photobiology/issues

1 error  | 1 warning  | 0 notes

```
checking examples ... ERROR
Running examples in ‘photobiology-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: absorbance
> ### Title: Absorbance
> ### Aliases: absorbance absorbance.default absorbance.filter_mspct
> ###   absorbance.filter_spct absorbance.object_mspct absorbance.object_spct
> 
> ### ** Examples
> 
> absorbance(polyester.spct, new_waveband(400,700))
Error: evaluation nested too deeply: infinite recursion / options(expressions=)?
Execution halted

checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Error: processing vignette 'userguide.Rnw' failed with diagnostics:
evaluation nested too deeply: infinite recursion / options(expressions=)?
Execution halted

```

## photobiologyInOut (0.4.6)
Maintainer: Pedro J. Aphalo <pedro.aphalo@helsinki.fi>  
Bug reports: https://bitbucket.org/aphalo/photobiologyinout/

0 errors | 1 warning  | 0 notes

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Read 3 items
Read 16 items
Error: processing vignette 'user-guide.Rnw' failed with diagnostics:
evaluation nested too deeply: infinite recursion / options(expressions=)?
Execution halted

```

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

## pixiedust (0.7.4)
Maintainer: Benjamin Nutter <nutter@battelle.org>  
Bug reports: https://github.com/nutterb/pixiedust/issues

0 errors | 0 warnings | 0 notes

## platetools (0.0.1)
Maintainer: Scott Warchal <s.warchal@sms.ed.ac.uk>

0 errors | 0 warnings | 0 notes

## plotly (3.6.0)
Maintainer: Carson Sievert <cpsievert1@gmail.com>  
Bug reports: https://github.com/ropensci/plotly/issues

0 errors | 0 warnings | 0 notes

## pmc (1.0.2)
Maintainer: Carl Boettiger <cboettig@gmail.com>  
Bug reports: https://github.com/cboettig/pmc/issues

0 errors | 0 warnings | 0 notes

## PogromcyDanych (1.5)
Maintainer: Przemyslaw Biecek <przemyslaw.biecek@gmail.com>

0 errors | 0 warnings | 1 note 

```
checking installed package size ... NOTE
  installed size is  6.1Mb
  sub-directories of 1Mb or more:
    data   6.0Mb
```

## pollstR (1.3.0)
Maintainer: Jeffrey B. Arnold <jeffrey.arnold@gmail.com>  
Bug reports: https://github.com/rOpenGov/pollstR/issues

0 errors | 0 warnings | 0 notes

## PopED (0.3.0)
Maintainer: Andrew C. Hooker <andrew.hooker@farmbio.uu.se>  
Bug reports: https://github.com/andrewhooker/PopED/issues

0 errors | 0 warnings | 0 notes

## poplite (0.99.16)
Maintainer: Daniel Bottomly <bottomly@ohsu.edu>

0 errors | 1 warning  | 2 notes

```
checking re-building of vignette outputs ... WARNING
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

checking package dependencies ... NOTE
Package suggested but not available for checking: ‘VariantAnnotation’

checking R code for possible problems ... NOTE
filter_.Database: no visible global function definition for ‘stack’
get.starting.point : <anonymous>: no visible global function definition
  for ‘na.omit’
select_.Database: no visible global function definition for ‘stack’
tsl.to.graph: no visible global function definition for ‘stack’
join,Database: no visible global function definition for ‘stack’
join,Database : .get.select.cols: no visible global function definition
  for ‘setNames’
join,Database: no visible binding for global variable ‘new.ancil’
join,Database: no visible global function definition for ‘setNames’
Undefined global functions or variables:
  na.omit new.ancil setNames stack
Consider adding
  importFrom("stats", "na.omit", "setNames")
  importFrom("utils", "stack")
to your NAMESPACE file.
```

## poppr (2.2.0)
Maintainer: Zhian N. Kamvar <kamvarz@science.oregonstate.edu>  
Bug reports: https://github.com/grunwaldlab/poppr/issues

0 errors | 0 warnings | 0 notes

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

## purrr (0.2.2)
Maintainer: Hadley Wickham <hadley@rstudio.com>  
Bug reports: https://github.com/hadley/purrr/issues

0 errors | 0 warnings | 0 notes

## qdap (2.2.5)
Maintainer: Tyler Rinker <tyler.rinker@gmail.com>  
Bug reports: http://github.com/trinker/qdap/issues

0 errors | 0 warnings | 0 notes

## quadmesh (0.1.0)
Maintainer: Michael D. Sumner <mdsumner@gmail.com>

0 errors | 0 warnings | 0 notes

## qualvar (0.1.0)
Maintainer: Joel Gombin <joel.gombin@gmail.com>

0 errors | 0 warnings | 0 notes

## quickpsy (0.1.3)
Maintainer: Linares Daniel <danilinares@gmail.com>

0 errors | 0 warnings | 0 notes

## qwraps2 (0.2.1)
Maintainer: Peter DeWitt <dewittpe@gmail.com>

0 errors | 0 warnings | 0 notes

## R6Frame (0.1.0)
Maintainer: Kristian D. Olsen <kristian@doingit.no>

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

(R:75948): Gtk-WARNING **: gtk_disable_setlocale() must be called before gtk_init()
```

## rbison (0.4.8)
Maintainer: Scott Chamberlain <myrmecocystus@gmail.com>  
Bug reports: https://github.com/ropensci/rbison/issues

0 errors | 0 warnings | 0 notes

## rccmisc (0.3.7)
Maintainer: Erik Bulow <erik.bulow@rccvast.se>  
Bug reports: https://bitbucket.com/cancercentrum/rccmisc/issues

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

1 error  | 0 warnings | 3 notes

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

checking R code for possible problems ... NOTE
addProvenance: no visible global function definition for
  ‘packageVersion’
addProvenance: no visible global function definition for
  ‘capture.output’
cmip5data: no visible global function definition for ‘runif’
filterDimensionTimeMonths: no visible binding for global variable
  ‘time’
makeGlobalStat: no visible binding for global variable ‘weighted.mean’
makePackageData: no visible global function definition for
... 7 lines ...
worldPlot: no visible global function definition for ‘quantile’
worldPlot: no visible global function definition for ‘rainbow’
Undefined global functions or variables:
  capture.output object.size packageVersion quantile rainbow runif time
  weighted.mean write.csv
Consider adding
  importFrom("grDevices", "rainbow")
  importFrom("stats", "quantile", "runif", "time", "weighted.mean")
  importFrom("utils", "capture.output", "object.size", "packageVersion",
             "write.csv")
to your NAMESPACE file.
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

0 errors | 0 warnings | 0 notes

## rerddap (0.3.4)
Maintainer: Scott Chamberlain <myrmecocystus@gmail.com>  
Bug reports: http://www.github.com/ropensci/rerddap/issues

0 errors | 0 warnings | 0 notes

## resumer (0.0.1)
Maintainer: Jared Lander <packages@jaredlander.com>  
Bug reports: https://github.com/jaredlander/resumer/issues

1 error  | 0 warnings | 0 notes

```
checking tests ... ERROR
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  
  2. Failure: resumer returns an rmakdown object with the correct length and names (@test-resumerStructure.R#9) 
  names(resBlank) not equal to c(...).
  Lengths differ: 9 vs 7
  
  
  testthat results ================================================================
  OK: 37 SKIPPED: 0 FAILED: 2
  1. Failure: resumer returns an rmakdown object with the correct length and names (@test-resumerStructure.R#6) 
  2. Failure: resumer returns an rmakdown object with the correct length and names (@test-resumerStructure.R#9) 
  
  Error: testthat unit tests failed
  Execution halted
```

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

0 errors | 0 warnings | 0 notes

## rgho (0.1.0)
Maintainer: Antoine Filipovic-Pierucci <pierucci@gmail.com>  
Bug reports: https://github.com/pierucci/rgho/issues

0 errors | 0 warnings | 0 notes

## riem (0.1.0)
Maintainer: Maëlle Salmon <maelle.salmon@yahoo.se>  
Bug reports: http://github.com/ropenscilabs/riem/issues

0 errors | 0 warnings | 1 note 

```
checking DESCRIPTION meta-information ... NOTE
Authors@R field gives persons with no valid roles:
  Brooke Anderson [rev] (Brooke Anderson reviewed the package for rOpenSci, see https://github.com/ropensci/onboarding/issues/39.)
```

## rivr (1.2)
Maintainer: Michael C Koohafkan <michael.koohafkan@gmail.com>  
Bug reports: https://github.com/mkoohafkan/rivr/issues

0 errors | 0 warnings | 0 notes

## RmarineHeatWaves (0.13.1)
Maintainer: Albertus J. Smit <albertus.smit@gmail.com>

0 errors | 0 warnings | 0 notes

## rmcfs (1.1.1)
Maintainer: Michal Draminski <mdramins@ipipan.waw.pl>

0 errors | 0 warnings | 0 notes

## RNeXML (2.0.6)
Maintainer: Carl Boettiger <cboettig@gmail.com>  
Bug reports: https://github.com/ropensci/RNeXML/issues

2 errors | 1 warning  | 1 note 

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

checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Loading required package: ape
Quitting from lines 46-67 (metadata.Rmd) 
Error: processing vignette 'metadata.Rmd' failed with diagnostics:
Unknown column 'content'
Execution halted


checking package dependencies ... NOTE
Packages suggested but not available for checking: ‘rrdf’ ‘Sxslt’
```

## rnoaa (0.5.6)
Maintainer: Scott Chamberlain <myrmecocystus@gmail.com>  
Bug reports: http://www.github.com/ropensci/rnoaa/issues

0 errors | 0 warnings | 0 notes

## robotstxt (0.3.2)
Maintainer: Peter Meissner <retep.meissner@gmail.com>  
Bug reports: https://github.com/ropenscilabs/robotstxt/issues

0 errors | 0 warnings | 0 notes

## ROpenFIGI (0.2.8)
Maintainer: Ruokun Huang <hruokun.2008@gmail.com>

0 errors | 0 warnings | 0 notes

## rpcdsearch (1.0)
Maintainer: David Springate <daspringate@gmail.com>

0 errors | 0 warnings | 0 notes

## rpdo (0.2.0)
Maintainer: Joe Thorley <joe@poissonconsulting.ca>

0 errors | 0 warnings | 0 notes

## rpivotTable (0.1.5.20)
Maintainer: Enzo Martoglio  <enzo@smartinsightsfromdata.com>

0 errors | 0 warnings | 0 notes

## rplexos (1.1.4)
Maintainer: Eduardo Ibanez <edu.ibanez@gmail.com>  
Bug reports: https://github.com/NREL/rplexos/issues

1 error  | 1 warning  | 0 notes

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

checking re-building of vignette outputs ... WARNING
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

## rPref (1.0.0)
Maintainer: Patrick Roocks <mail@p-roocks.de>

0 errors | 0 warnings | 1 note 

```
checking for GNU extensions in Makefiles ... NOTE
GNU make is a SystemRequirements.
```

## RPresto (1.2.1)
Maintainer: Onur Ismail Filiz <onur@fb.com>  
Bug reports: https://github.com/prestodb/RPresto/issues

0 errors | 0 warnings | 0 notes

## rprev (0.1.0)
Maintainer: Stuart Lacy <stuart.lacy@york.ac.uk>

0 errors | 0 warnings | 0 notes

## rscorecard (0.2.5)
Maintainer: Benjamin Skinner <b.skinner@vanderbilt.edu>  
Bug reports: http://github.com/btskinner/rscorecard/issues

0 errors | 0 warnings | 0 notes

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

## rtdists (0.5-2)
Maintainer: Henrik Singmann <singmann+rtdists@gmail.com>

0 errors | 0 warnings | 0 notes

## rtimes (0.3.0)
Maintainer: Scott Chamberlain <myrmecocystus@gmail.com>  
Bug reports: https://github.com/ropengov/rtimes/issues

0 errors | 0 warnings | 0 notes

## RtutoR (0.3)
Maintainer: Anup Nair <nairanup50695@gmail.com>

0 errors | 0 warnings | 0 notes

## rvertnet (0.4.4)
Maintainer: Scott Chamberlain <myrmecocystus@gmail.com>  
Bug reports: https://github.com/ropensci/rvertnet/issues

0 errors | 0 warnings | 0 notes

## rwunderground (0.1.0)
Maintainer: Alex Shum <alex@ALShum.com>  
Bug reports: https://github.com/alshum/rwunderground/issues

0 errors | 0 warnings | 0 notes

## saeSim (0.8.0)
Maintainer: Sebastian Warnholz <Sebastian.Warnholz@fu-berlin.de>  
Bug reports: https://github.com/wahani/saeSim/issues

0 errors | 0 warnings | 0 notes

## scholar (0.1.4)
Maintainer: James Keirstead <james.keirstead@gmail.com>  
Bug reports: https://github.com/jkeirstead/scholar/issues

0 errors | 0 warnings | 0 notes

## SEERaBomb (2016.1)
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

## SimDesign (1.0)
Maintainer: Phil Chalmers <rphilip.chalmers@gmail.com>

0 errors | 0 warnings | 1 note 

```
checking package dependencies ... NOTE
Package suggested but not available for checking: ‘doMPI’
```

## simmer (3.2.1)
Maintainer: Iñaki Ucar <i.ucar86@gmail.com>  
Bug reports: https://github.com/Enchufa2/simmer/issues

0 errors | 0 warnings | 0 notes

## simPH (1.3.9)
Maintainer: Christopher Gandrud <christopher.gandrud@gmail.com>  
Bug reports: https://github.com/christophergandrud/simPH/issues

0 errors | 0 warnings | 0 notes

## sjmisc (1.8)
Maintainer: Daniel Lüdecke <d.luedecke@uke.de>

0 errors | 0 warnings | 0 notes

## sjPlot (2.0.1)
Maintainer: Daniel Lüdecke <d.luedecke@uke.de>  
Bug reports: https://github.com/sjPlot/devel/issues

0 errors | 0 warnings | 0 notes

## slackr (1.4.1)
Maintainer: Bob Rudis <bob@rudis.net>  
Bug reports: https://github.com/hrbrmstr/slackr/issues

0 errors | 0 warnings | 0 notes

## solrium (0.3.0)
Maintainer: Scott Chamberlain <myrmecocystus@gmail.com>  
Bug reports: http://www.github.com/ropensci/solrium/issues

0 errors | 0 warnings | 0 notes

## sorvi (0.7.26)
Maintainer: Leo Lahti <louhos@googlegroups.com>  
Bug reports: https://github.com/ropengov/sorvi/issues

0 errors | 0 warnings | 1 note 

```
checking R code for possible problems ... NOTE
convert_municipality_names: no visible global function definition for
  ‘read.csv’
regression_plot: no visible binding for global variable ‘loess’
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

## sp500SlidingWindow (0.1.0)
Maintainer: George Fisher <george@georgefisher.com>

0 errors | 0 warnings | 0 notes

## SpaDES (1.1.4)
Maintainer: Alex M Chubaty <alexander.chubaty@canada.ca>  
Bug reports: https://github.com/PredictiveEcology/SpaDES/issues

0 errors | 1 warning  | 1 note 

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...

Loading required package: grid

Attaching package: 'grid'

The following object is masked from 'package:SpaDES':

... 8 lines ...
    shift

The following object is masked from 'package:SpaDES':

    copy

Files saved. Use outputs(your simList) for details
Quitting from lines 581-622 (ii-modules.Rmd) 
Error: processing vignette 'ii-modules.Rmd' failed with diagnostics:
missing value where TRUE/FALSE needed
Execution halted

checking package dependencies ... NOTE
Package suggested but not available for checking: ‘fastshp’
```

## spbabel (0.3.2)
Maintainer: Michael D. Sumner <mdsumner@gmail.com>

0 errors | 0 warnings | 0 notes

## sprintfr (0.1.0)
Maintainer: Brandon Taylor <brandon.taylor221@gmail.com>  
Bug reports: https://github.com/bramtayl/sprintfr/issues

1 error  | 1 warning  | 0 notes

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

checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Quitting from lines 30-31 (sprintfr.Rmd) 
Error: processing vignette 'sprintfr.Rmd' failed with diagnostics:
object 'data_frame' not found
Execution halted

```

## srvyr (0.1.1)
Maintainer: Greg Freedman <greg.freedman@gmail.com>  
Bug reports: https://github.com/gergness/srvyr/issues

0 errors | 0 warnings | 0 notes

## ss3sim (0.9.2)
Maintainer: Sean Anderson <sean@seananderson.ca>

0 errors | 0 warnings | 0 notes

## statar (0.6.1)
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

## stplanr (0.1.2)
Maintainer: Robin Lovelace <rob00x@gmail.com>  
Bug reports: https://github.com/ropensci/stplanr/issues

0 errors | 0 warnings | 0 notes

## strptimer (0.1.0)
Maintainer: Brandon Taylor <brandon.taylor221@gmail.com>

0 errors | 0 warnings | 0 notes

## surveybootstrap (0.0.1)
Maintainer: Dennis M. Feehan <feehan@berkeley.edu>

0 errors | 0 warnings | 0 notes

## SWMPr (2.1.5)
Maintainer: Marcus W. Beck <mbafs2012@gmail.com>  
Bug reports: https://github.com/fawda123/SWMPr/issues

0 errors | 0 warnings | 0 notes

## taber (0.1.0)
Maintainer: Seth Wenchel <seth@wenchel.com>  
Bug reports: http://github.com/restonslacker/taber/issues

0 errors | 0 warnings | 0 notes

## tadaatoolbox (0.9.0)
Maintainer: Lukas Burk <lukas@quantenbrot.de>  
Bug reports: https://github.com/tadaadata/tadaatoolbox/issues

0 errors | 0 warnings | 0 notes

## tcR (2.2.1.11)
Maintainer: Vadim Nazarov <vdm.nazarov@gmail.com>  
Bug reports: https://github.com/imminfo/tcr/issues

0 errors | 0 warnings | 1 note 

```
checking installed package size ... NOTE
  installed size is  5.6Mb
  sub-directories of 1Mb or more:
    data   1.2Mb
    doc    3.9Mb
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

## texmexseq (0.2)
Maintainer: Scott Olesen <swo@mit.edu>

0 errors | 1 warning  | 0 notes

```
checking whether package ‘texmexseq’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import ‘dplyr::matches’ by ‘testthat::matches’ when loading ‘texmexseq’
See ‘/Users/hadley/Documents/dplyr/dplyr/revdep/checks/texmexseq.Rcheck/00install.out’ for details.
```

## textreuse (0.1.3)
Maintainer: Lincoln Mullen <lincoln@lincolnmullen.com>  
Bug reports: https://github.com/ropensci/textreuse/issues

0 errors | 0 warnings | 0 notes

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

0 errors | 0 warnings | 1 note 

```
checking R code for possible problems ... NOTE
read_json: no visible global function definition for ‘tail’
Undefined global functions or variables:
  tail
Consider adding
  importFrom("utils", "tail")
to your NAMESPACE file.
```

## tidyr (0.5.1)
Maintainer: Hadley Wickham <hadley@rstudio.com>  
Bug reports: https://github.com/hadley/tidyr/issues

0 errors | 0 warnings | 0 notes

## tidytext (0.1.0)
Maintainer: Julia Silge <julia.silge@gmail.com>  
Bug reports: http://github.com/juliasilge/tidytext/issues

2 errors | 1 warning  | 0 notes

```
checking examples ... ERROR
Running examples in ‘tidytext-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: cast_sparse
> ### Title: Create a sparse matrix from row names, column names, and values
> ###   in a table.
> ### Aliases: cast_sparse
> 
... 7 lines ...
> 
> cast_sparse(dat, a, b)
2 x 4 sparse Matrix of class "dgCMatrix"
     col1 col2 col3 col4
row1    1    1    .    .
row2    1    .    1    1
> 
> cast_sparse(dat, a, b, val)
Error in .M.kind(x) : not yet implemented for matrix with typeof NULL
Calls: cast_sparse -> cast_sparse_ -> <Anonymous> -> .M.kind
Execution halted

checking tests ... ERROR
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  1: cast_sparse(dat, a, b, val) at testthat/test-sparse-casters.R:20
  2: cast_sparse_(data, col_name(substitute(row)), col_name(substitute(column)), value_col)
  3: data[[value_col]]
  4: `[[.tbl_df`(data, value_col)
  5: stopc("Unknown column '", colname, "'") at /private/tmp/RtmpxqBRhg/devtools1785e2d679c7f/hadley-tibble-1e5b140/R/tbl-df.r:25
  6: stop(..., call. = FALSE, domain = NA) at /private/tmp/RtmpxqBRhg/devtools1785e2d679c7f/hadley-tibble-1e5b140/R/utils.r:63
  
  testthat results ================================================================
  OK: 41 SKIPPED: 0 FAILED: 1
  1. Error: Can cast tables into a sparse Matrix (@test-sparse-casters.R#20) 
  
  Error: testthat unit tests failed
  Execution halted

checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Warning: Stacking not well defined when ymin != 0
Quitting from lines 94-114 (tidying_casting.Rmd) 
Error: processing vignette 'tidying_casting.Rmd' failed with diagnostics:
Unknown column 'count'
Execution halted

```

## tigger (0.2.5)
Maintainer: Daniel Gadala-Maria <daniel.gadala-maria@yale.edu>  
Bug reports: https://bitbucket.org/kleinstein/tigger/issues

1 error  | 1 warning  | 0 notes

```
checking examples ... ERROR
Running examples in ‘tigger-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: getPopularMutationCount
> ### Title: Find Frequent Sequences' Mutation Counts
> ### Aliases: getPopularMutationCount
> 
> ### ** Examples
> 
> data(sample_db, germline_ighv)
> getPopularMutationCount(sample_db, germline_ighv)
Error: object 'V_GENE_N' not found
Execution halted

checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Loading required package: alakazam
Loading required package: ggplot2
Loading required package: shazam

Attaching package: 'dplyr'

The following objects are masked from 'package:stats':

    filter, lag

The following objects are masked from 'package:base':

    intersect, setdiff, setequal, union

Quitting from lines 110-115 (Tigger-Vignette.Rmd) 
Error: processing vignette 'Tigger-Vignette.Rmd' failed with diagnostics:
there is no package called 'snow'
Execution halted

```

## tigris (0.3)
Maintainer: Kyle Walker <kyle.walker@tcu.edu>

0 errors | 0 warnings | 0 notes

## titanic (0.1.0)
Maintainer: Paul Hendricks <paul.hendricks.2013@owu.edu>  
Bug reports: https://github.com/paulhendricks/titanic/issues

0 errors | 0 warnings | 0 notes

## Tmisc (0.1.7)
Maintainer: Stephen Turner <vustephen@gmail.com>

0 errors | 0 warnings | 0 notes

## traits (0.2.0)
Maintainer: Scott Chamberlain <myrmecocystus@gmail.com>  
Bug reports: http://www.github.com/ropensci/traits/issues

0 errors | 0 warnings | 0 notes

## treeplyr (0.1.1)
Maintainer: Josef Uyeda <josef.uyeda@gmail.com>

0 errors | 1 warning  | 0 notes

```
checking examples ... WARNING
Found the following significant warnings:

  Warning: Setting row names on a tibble is deprecated.
  Warning: Setting row names on a tibble is deprecated.
  Warning: Setting row names on a tibble is deprecated.
  Warning: Setting row names on a tibble is deprecated.
  Warning: Setting row names on a tibble is deprecated.
  Warning: Setting row names on a tibble is deprecated.
  Warning: Setting row names on a tibble is deprecated.
  Warning: Setting row names on a tibble is deprecated.
  Warning: Setting row names on a tibble is deprecated.
  Warning: Setting row names on a tibble is deprecated.
  Warning: Setting row names on a tibble is deprecated.
  Warning: Setting row names on a tibble is deprecated.
  Warning: Setting row names on a tibble is deprecated.
  Warning: Setting row names on a tibble is deprecated.
  Warning: Setting row names on a tibble is deprecated.
  Warning: Setting row names on a tibble is deprecated.
  Warning: Setting row names on a tibble is deprecated.
  Warning: Setting row names on a tibble is deprecated.
Deprecated functions may be defunct as soon as of the next release of
R.
See ?Deprecated.
```

## trelliscope (0.9.4)
Maintainer: Ryan Hafen <rhafen@gmail.com>  
Bug reports: https://github.com/tesseradata/trelliscope/issues

0 errors | 0 warnings | 0 notes

## turfR (0.8-7)
Maintainer: Jack Horne <jack@jackhorne.net>

0 errors | 0 warnings | 1 note 

```
checking R code for possible problems ... NOTE
turf: no visible global function definition for ‘read.table’
turf: no visible global function definition for ‘flush.console’
turf.combos: no visible global function definition for ‘combn’
Undefined global functions or variables:
  combn flush.console read.table
Consider adding
  importFrom("utils", "combn", "flush.console", "read.table")
to your NAMESPACE file.
```

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

## vaersNDvax (1.0.1)
Maintainer: Irucka Embry <iembry@ecoccs.com>  
Bug reports: https://gitlab.com/iembry/vaersND/issues

0 errors | 0 warnings | 0 notes

## vaersvax (1.0.1)
Maintainer: Irucka Embry <iembry@ecoccs.com>  
Bug reports: https://gitlab.com/iembry/vaers/issues

0 errors | 0 warnings | 0 notes

## vcfR (1.1.0)
Maintainer: Brian J. Knaus <briank.lists@gmail.com>

1 error  | 0 warnings | 0 notes

```
checking examples ... ERROR
Running examples in ‘vcfR-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: Convert to tidy data frames
> ### Title: Convert vcfR objects to tidy data frames
> ### Aliases: 'Convert to tidy data frames' extract_gt_tidy
> ###   extract_info_tidy vcfR2tidy vcf_field_names
> 
... 8 lines ...
> # data frames: fix, gt, and meta. Here we don't coerce columns
> # to integer or numeric types...
> Z <- vcfR2tidy(vcf)
Extracting gt element AD
Extracting gt element DP
Extracting gt element GQ
Extracting gt element GT
Extracting gt element PL
Error in eval(expr, envir, enclos) : could not find function "everything"
Calls: vcfR2tidy ... select_vars_ -> <Anonymous> -> lapply -> FUN -> eval -> eval
Execution halted
```

## vdmR (0.2.2)
Maintainer: Tomokazu Fujino <fujino@fwu.ac.jp>

0 errors | 0 warnings | 0 notes

## vqtl (1.0)
Maintainer: Robert Corty <rcorty@gmail.com>

0 errors | 0 warnings | 0 notes

## vtreat (0.5.25)
Maintainer: John Mount <jmount@win-vector.com>

0 errors | 0 warnings | 0 notes

## VWPre (0.7.0)
Maintainer: Vincent Porretta <vincentporretta@gmail.com>

0 errors | 0 warnings | 0 notes

## wakefield (0.3.0)
Maintainer: Tyler Rinker <tyler.rinker@gmail.com>  
Bug reports: https://github.com/trinker/wakefield/issues

0 errors | 0 warnings | 0 notes

## WHO (0.2)
Maintainer: Eric Persson <expersso5@gmail.com>  
Bug reports: https://www.github.com/expersso/WHO/issues

0 errors | 0 warnings | 0 notes

## wikipediatrend (1.1.10)
Maintainer: Peter Meissner <retep.meissner@gmail.com>  
Bug reports: https://github.com/petermeissner/wikipediatrend/issues

0 errors | 0 warnings | 1 note 

```
checking package dependencies ... NOTE
Packages suggested but not available for checking:
  ‘AnomalyDetection’ ‘BreakoutDetection’
```

## wordbankr (0.2.0)
Maintainer: Mika Braginsky <mika.br@gmail.com>  
Bug reports: http://github.com/langcog/wordbankr/issues

0 errors | 0 warnings | 0 notes

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

## yorkr (0.0.5)
Maintainer: Tinniam V Ganesh <tvganesh.85@gmail.com>

0 errors | 0 warnings | 0 notes

## Zelig (5.0-12)
Maintainer: James Honaker <zelig-zee@iq.harvard.edu>

0 errors | 0 warnings | 0 notes

## ZeligChoice (0.9-0)
Maintainer: James Honaker <zelig-zee@iq.harvard.edu>

0 errors | 0 warnings | 0 notes

## ZeligEI (0.1-0)
Maintainer: James Honaker <zelig.zee@gmail.com>

0 errors | 0 warnings | 0 notes

