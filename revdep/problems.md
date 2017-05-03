# Setup

## Platform

|setting  |value                        |
|:--------|:----------------------------|
|version  |R version 3.4.0 (2017-04-21) |
|system   |x86_64, linux-gnu            |
|ui       |X11                          |
|language |(EN)                         |
|collate  |en_US.UTF-8                  |
|tz       |Zulu                         |
|date     |2017-05-03                   |

## Packages

|package        |*  |version    |date       |source                            |
|:--------------|:--|:----------|:----------|:---------------------------------|
|assertthat     |   |0.2.0      |2017-04-11 |cran (@0.2.0)                     |
|BH             |   |1.62.0-1   |2016-11-19 |cran (@1.62.0-)                   |
|covr           |   |2.2.2      |2017-01-05 |cran (@2.2.2)                     |
|DBI            |   |0.6-11     |2017-04-24 |Github (rstats-db/DBI@20f1f02)    |
|dplyr          |   |0.5.0      |2017-05-02 |local (tidyverse/dplyr@NA)        |
|dtplyr         |   |0.0.2      |2017-04-21 |cran (@0.0.2)                     |
|ggplot2        |   |2.2.1      |2016-12-30 |cran (@2.2.1)                     |
|knitr          |   |1.15.1     |2016-11-22 |cran (@1.15.1)                    |
|Lahman         |   |5.0-0      |2016-08-27 |cran (@5.0-0)                     |
|lazyeval       |   |0.2.0      |2016-06-12 |cran (@0.2.0)                     |
|magrittr       |   |1.5        |2014-11-22 |CRAN (R 3.4.0)                    |
|microbenchmark |   |1.4-2.1    |2015-11-25 |cran (@1.4-2.1)                   |
|nycflights13   |   |0.2.2      |2017-01-27 |cran (@0.2.2)                     |
|R6             |   |2.2.0      |2016-10-05 |CRAN (R 3.4.0)                    |
|Rcpp           |   |0.12.10    |2017-03-19 |CRAN (R 3.4.0)                    |
|rmarkdown      |   |1.5        |2017-04-26 |cran (@1.5)                       |
|RMySQL         |   |0.10.11    |2017-03-29 |cran (@0.10.11)                   |
|RPostgreSQL    |   |0.4-1      |2016-05-08 |cran (@0.4-1)                     |
|RSQLite        |   |1.1-2      |2017-01-08 |CRAN (R 3.4.0)                    |
|testthat       |   |1.0.2      |2016-04-23 |cran (@1.0.2)                     |
|tibble         |   |1.3.0.9001 |2017-05-02 |Github (tidyverse/tibble@08af6b0) |

# Check results

33 packages with problems

|package         |version | errors| warnings| notes|
|:---------------|:-------|------:|--------:|-----:|
|bioOED          |0.1.1   |      1|        0|     0|
|blscrapeR       |2.1.2   |      1|        0|     0|
|carpenter       |0.2.0   |      1|        1|     0|
|codingMatrices  |0.3.0   |      0|        1|     0|
|condformat      |0.5.0   |      2|        1|     0|
|d3r             |0.6.4   |      1|        0|     1|
|dat             |0.2.0   |      1|        0|     0|
|describer       |0.2.0   |      1|        0|     0|
|FSelectorRcpp   |0.1.3   |      1|        0|     2|
|geoknife        |1.5.4   |      0|        1|     0|
|gitgadget       |0.2.1   |      0|        1|     0|
|gutenbergr      |0.1.2   |      1|        0|     0|
|harrietr        |0.2.2   |      1|        0|     0|
|HTSSIP          |1.0.3   |      1|        0|     0|
|huxtable        |0.2.2   |      1|        0|     0|
|IATscores       |0.1-2   |      1|        0|     0|
|imfr            |0.1.4   |      1|        0|     0|
|NFP             |0.99.2  |      0|        1|     2|
|nzelect         |0.3.3   |      0|        1|     0|
|officer         |0.1.3   |      2|        1|     0|
|padr            |0.2.1   |      1|        0|     0|
|pivottabler     |0.2.0   |      1|        0|     0|
|rattle          |4.1.0   |      0|        1|     3|
|REDCapR         |0.9.7   |      1|        1|     1|
|rerddap         |0.4.0   |      0|        1|     0|
|sjPlot          |2.3.1   |      0|        1|     1|
|SpaDES          |1.3.1   |      0|        1|     2|
|sparseHessianFD |0.3.3   |      0|        1|     0|
|ss3sim          |0.9.5   |      0|        1|     0|
|textmining      |0.0.1   |      0|        1|     0|
|tidytext        |0.1.2   |      2|        1|     0|
|vdmR            |0.2.3   |      0|        1|     0|
|wrswoR          |1.0-1   |      0|        1|     1|

## bioOED (0.1.1)
Maintainer: Alberto Garre <garre.alberto@gmail.com>

1 error  | 0 warnings | 0 notes

```
checking package dependencies ... ERROR
Package required but not available: ‘MEIGOR’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```

## blscrapeR (2.1.2)
Maintainer: Kris Eberwein <eberwein@knights.ucf.edu>  
Bug reports: https://github.com/keberwein/blscrapeR/issues

1 error  | 0 warnings | 0 notes

```
checking tests ... ERROR
  Running ‘testthat.R’
Running the tests in ‘tests/testthat.R’ failed.
Complete output:
  > library(testthat)
  > library(blscrapeR)
  > 
  > test_check("blscrapeR")
  trying URL 'https://www.bls.gov/web/metro/laucntycur14.txt'
  downloaded 5.8 MB
  
  Error in datalist[[i]] <- mth_vals : 
    attempt to select less than one element in integerOneIndex
  Calls: test_check ... with_reporter -> force -> source_file -> eval -> eval
  testthat results ================================================================
  OK: 0 SKIPPED: 0 FAILED: 0
  Execution halted
```

## carpenter (0.2.0)
Maintainer: Luke Johnston <lwjohnst@gmail.com>  
Bug reports: https://github.com/lwjohnst86/carpenter/issues

1 error  | 1 warning  | 0 notes

```
checking examples ... ERROR
Running examples in ‘carpenter-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: carpenter
> ### Title: Build common tables for your research needs!
> ### Aliases: carpenter
> 
> ### ** Examples
> 
> 
> library(magrittr)
> outline_table(iris, 'Species') %>%
+  add_rows(c('Sepal.Length', 'Petal.Length'), stat_meanSD) %>%
+  add_rows('Sepal.Width', stat_medianIQR) %>%
+  renaming('rows', function(x) gsub('Sepal\\.', 'Sepal ', x)) %>%
+  renaming('header', c('Measures', 'Setosa', 'Versicolor', 'Virginica')) %>%
+  build_table(caption = 'A caption for the table')
Error in dimnames(x) <- dn : 
  length of 'dimnames' [2] not equal to array extent
Calls: %>% ... pandoc.table -> cat -> pandoc.table.return -> colnames<-
Execution halted

checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Error in value[[3L]] : subscript out of bounds
Calls: buildVignettes -> tryCatch -> tryCatchList -> tryCatchOne
Execution halted

```

## codingMatrices (0.3.0)
Maintainer: Bill Venables <Bill.Venables@gmail.com>

0 errors | 1 warning  | 0 notes

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
  Running 'texi2dvi' on 'codingMatrices.tex' failed.
BibTeX errors:
The top-level auxiliary file: codingMatrices.aux
I couldn't open style file chicago.bst
---line 19 of file codingMatrices.aux
 : \bibstyle{chicago
 :                  }
I'm skipping whatever remains of this command
I found no style file---while reading file codingMatrices.aux
Calls: buildVignettes -> texi2pdf -> texi2dvi
Execution halted

```

## condformat (0.5.0)
Maintainer: Sergio Oller Moreno <sergioller@gmail.com>  
Bug reports: http://github.com/zeehio/condformat/issues

2 errors | 1 warning  | 0 notes

```
checking examples ... ERROR
Running examples in ‘condformat-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: condformat
> ### Title: Converts a data frame or matrix to a condformat_tbl class.
> ### Aliases: condformat
> 
> ### ** Examples
> 
> data(iris)
> condformat(iris[1:5,])
Error in prPrepareCss(x, css = css.cell, rnames = rnames, header = header) : 
  There is an invalid number of columns for the css.cell matrix. Your x argument has '8' columns while your css.cell has '5' columns and there are no rownames.
Calls: <Anonymous> ... do.call -> <Anonymous> -> htmlTable.default -> prPrepareCss
Execution halted

checking tests ... ERROR
  Running ‘testthat.R’ [6s/12s]
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  testthat results ================================================================
  OK: 13 SKIPPED: 0 FAILED: 17
  1. Error: print.condformat_tbl returns its input (@test_rendering.R#7) 
  2. Error: knitr returns an HTML table (@test_rendering.R#14) 
  3. Error: rule_fill_discrete works (@test_rule_fill_discrete.R#10) 
  4. Error: rule_fill_discrete lock cells (@test_rule_fill_discrete.R#35) 
  5. Error: rule_fill_discrete_ works (@test_rule_fill_discrete.R#64) 
  6. Error: rule_fill_discrete_ works with formula (@test_rule_fill_discrete.R#86) 
  7. Error: custom rule_ passes doing nothing (@test_rule_fill_discrete.R#110) 
  8. Error: rule_fill_gradient works in the limits (@test_rule_fill_gradient.R#8) 
  9. Error: rule_fill_gradient_ works in the limits (@test_rule_fill_gradient.R#24) 
  1. ...
  
  Error: testthat unit tests failed
  Execution halted

checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Quitting from lines 31-39 (introduction.Rmd) 
Error: processing vignette 'introduction.Rmd' failed with diagnostics:
There is an invalid number of columns for the structure(c("; background-color: #7D00FF", "; background-color: #7D00FF", "; background-color: #7D00FF", "; background-color: #7D00FF", "; background-color: #7D00FF", "; background-color: #FFFFFF", "; background-color: #FFFFFF", "; background-color: #FFFFFF", "; background-color: #FFFFFF", "; background-color: #FFFFFF", "; background-color: #FFFFFF", "; background-color: #FFFFFF", "; background-color: #FFFFFF", "; background-color: #FFFFFF", "; background-color: #FFFFFF", "; background-color: #FFFFFF", "; background-color: #FFFFFF", "; background-color: #7D00FF", "; background-color: #7D00FF", "; background-color: #7D00FF", "; background-color: #7D00FF", "; background-color: #7D00FF", "; background-color: #FFFFFF", "; background-color: #FFFFFF", "; background-color: #FFFFFF", "; background-color: #FFFFFF", "; background-color: #FFFFFF", "; background-color: #FFFFFF", ";
Execution halted

```

## d3r (0.6.4)
Maintainer: Kent Russell <kent.russell@timelyportfolio.com>  
Bug reports: https://github.com/timelyportfolio/d3r/issues

1 error  | 0 warnings | 1 note 

```
checking tests ... ERROR
  Running ‘testthat.R’
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  > library(d3r)
  > 
  > test_check("d3r")
  1. Failure: d3_igraph works (@test_igraph.R#36) --------------------------------
  d3_igraph(bull_node_attr, json = FALSE) not equal to list(...).
  Component "links": Component "weight": Modes: character, numeric
  Component "links": Component "weight": target is character, current is numeric
  
  
  testthat results ================================================================
  OK: 9 SKIPPED: 4 FAILED: 1
  1. Failure: d3_igraph works (@test_igraph.R#36) 
  
  Error: testthat unit tests failed
  Execution halted

checking package dependencies ... NOTE
Package which this enhances but not available for checking: ‘treemap’
```

## dat (0.2.0)
Maintainer: Sebastian Warnholz <wahani@gmail.com>  
Bug reports: https://github.com/wahani/dat/issues

1 error  | 0 warnings | 0 notes

```
checking tests ... ERROR
  Running ‘testthat.R’ [55s/56s]
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
                                            ~^
  tests/testthat/utf-8.R:16:44: style: Commas should never have a space before.
  expect_equal( strings_addresses(names(df)) ,  strings_addresses(names(df4)) )
                                            ~^
  tests/testthat/utf-8.R:17:44: style: Commas should never have a space before.
  expect_equal( strings_addresses(names(df)) ,  strings_addresses(names(gdf4)) )
                                            ~^
  
  
  testthat results ================================================================
  OK: 108 SKIPPED: 0 FAILED: 1
  1. Failure: Package Style (@test-lintr.R#5) 
  
  Error: testthat unit tests failed
  Execution halted
```

## describer (0.2.0)
Maintainer: Paul Hendricks <paul.hendricks.2013@owu.edu>  
Bug reports: https://github.com/paulhendricks/describer/issues

1 error  | 0 warnings | 0 notes

```
checking tests ... ERROR
  Running ‘testthat.R’ [134s/136s]
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
                                            ~^
  tests/testthat/utf-8.R:16:44: style: Commas should never have a space before.
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

## FSelectorRcpp (0.1.3)
Maintainer: Zygmunt Zawadzki <zygmunt@zstat.pl>  
Bug reports: https://github.com/mi2-warsaw/FSelectorRcpp/issues

1 error  | 0 warnings | 2 notes

```
checking tests ... ERROR
  Running ‘testthat.R’
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  The following objects are masked from 'package:base':
  
      intersect, setdiff, setequal, union
  
  > library(entropy)
  
  Attaching package: 'entropy'
  
  The following object is masked from 'package:FSelectorRcpp':
  
      discretize
  
  > 
  > test_check("FSelectorRcpp")
  Segmentation fault (core dumped)

checking package dependencies ... NOTE
Package suggested but not available for checking: ‘RTCGA.rnaseq’

checking installed package size ... NOTE
  installed size is  9.6Mb
  sub-directories of 1Mb or more:
    doc    2.2Mb
    libs   7.2Mb
```

## geoknife (1.5.4)
Maintainer: Jordan Read <jread@usgs.gov>  
Bug reports: https://github.com/USGS-R/geoknife/issues

0 errors | 1 warning  | 0 notes

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Quitting from lines 360-364 (geoknife.Rmd) 
Error: processing vignette 'geoknife.Rmd' failed with diagnostics:
need finite 'xlim' values
Execution halted

```

## gitgadget (0.2.1)
Maintainer: Vincent Nijs <vnijs@ucsd.edu>  
Bug reports: https://github.com/vnijs/gitgadget/issues

0 errors | 1 warning  | 0 notes

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
pandoc: Could not fetch https://travis-ci.org/vnijs/gitgadget.png?branch=master
TlsExceptionHostPort (HandshakeFailed (Error_Packet_unexpected "Alert [(AlertLevel_Fatal,UnexpectedMessage)]" " expected: change cipher")) "travis-ci.org" 443
Error: processing vignette 'gitgadget.Rmd' failed with diagnostics:
pandoc document conversion failed with error 67
Execution halted

```

## gutenbergr (0.1.2)
Maintainer: David Robinson <admiral.david@gmail.com>  
Bug reports: http://github.com/ropenscilabs/gutenbergr/issues

1 error  | 0 warnings | 0 notes

```
checking tests ... ERROR
  Running ‘testthat.R’ [7s/14s]
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  > library(testthat)
  > library(gutenbergr)
  > 
  > test_check("gutenbergr")
  1. Failure: read_zip_url can download and read a zip file (@test-utils.R#7) ----
  any(z == "Congress shall make no law respecting an establishment of religion,") isn't true.
  
  
  testthat results ================================================================
  OK: 46 SKIPPED: 0 FAILED: 1
  1. Failure: read_zip_url can download and read a zip file (@test-utils.R#7) 
  
  Error: testthat unit tests failed
  Execution halted
```

## harrietr (0.2.2)
Maintainer: Anders Gonçalves da Silva <andersgs@gmail.com>  
Bug reports: https://github.com/andersgs/harrietr/issues

1 error  | 0 warnings | 0 notes

```
checking package dependencies ... ERROR
Package required but not available: ‘ggtree’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```

## HTSSIP (1.0.3)
Maintainer: Nicholas Youngblut <nyoungb2@gmail.com>

1 error  | 0 warnings | 0 notes

```
checking package dependencies ... ERROR
Package required but not available: ‘phyloseq’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```

## huxtable (0.2.2)
Maintainer: David Hugh-Jones <davidhughjones@gmail.com>  
Bug reports: https://github.com/hughjonesd/huxtable/issues

1 error  | 0 warnings | 0 notes

```
checking tests ... ERROR
  Running ‘testthat.R’ [12s/23s]
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
         message = handle_message)) at /tmp/Rtmpf6YF8j/devtools1eed5a0addc9/testthat/R/evaluate-promise.R:42
  4: withCallingHandlers(withVisible(code), warning = handle_warning, message = handle_message) at /tmp/Rtmpf6YF8j/devtools1eed5a0addc9/testthat/R/evaluate-promise.R:42
  5: withVisible(code) at /tmp/Rtmpf6YF8j/devtools1eed5a0addc9/testthat/R/evaluate-promise.R:42
  6: rmarkdown::render("rowheight-multicol-test.Rmd", quiet = TRUE) at /tmp/Rtmpf6YF8j/devtools1eed5a0addc9/testthat/R/evaluate-promise.R:42
  7: convert(output_file, run_citeproc) at /tmp/Rtmp2UybSq/devtoolsc0a2aa8de4c/rmarkdown/R/render.R:655
  8: pandoc_convert(utf8_input, pandoc_to, output_format$pandoc$from, output, citeproc, 
         output_format$pandoc$args, !quiet) at /tmp/Rtmp2UybSq/devtoolsc0a2aa8de4c/rmarkdown/R/render.R:585
  9: stop("pandoc document conversion failed with error ", result, call. = FALSE) at /tmp/Rtmp2UybSq/devtoolsc0a2aa8de4c/rmarkdown/R/pandoc.R:100
  
  testthat results ================================================================
  OK: 203 SKIPPED: 14 FAILED: 1
  1. Error: Row heights do not screw up latex multicol (@test-with-pandoc.R#20) 
  
  Error: testthat unit tests failed
  Execution halted
```

## IATscores (0.1-2)
Maintainer: Giulio Costantini <costantinigiulio@gmail.com>

1 error  | 0 warnings | 0 notes

```
checking package dependencies ... ERROR
Package required but not available: ‘nem’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```

## imfr (0.1.4)
Maintainer: Christopher Gandrud <christopher.gandrud@gmail.com>  
Bug reports: https://github.com/christophergandrud/imfr/issues

1 error  | 0 warnings | 0 notes

```
checking examples ... ERROR
Running examples in ‘imfr-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: imf_codes
> ### Title: Retrieve individual database codes
> ### Aliases: imf_codes
> 
> ### ** Examples
... 79 lines ...
 *** caught segfault ***
address (nil), cause 'unknown'

Traceback:
 1: simplify(obj, simplifyVector = simplifyVector, simplifyDataFrame = simplifyDataFrame,     simplifyMatrix = simplifyMatrix, flatten = flatten, ...)
 2: fromJSON_string(txt = txt, simplifyVector = simplifyVector, simplifyDataFrame = simplifyDataFrame,     simplifyMatrix = simplifyMatrix, flatten = flatten, ...)
 3: fromJSON(raw_download)
 4: download_parse(URL, times = times)
 5: imf_codes(codelist = "CL_INDICATOR_BOP")
An irrecoverable exception occurred. R is aborting now ...
Segmentation fault (core dumped)
```

## NFP (0.99.2)
Maintainer: Yang Cao <yiluheihei@gmail.com>  
Bug reports: https://github.com/yiluheihei/NFP/issues

0 errors | 1 warning  | 2 notes

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
    clusterApply, clusterApplyLB, clusterCall, clusterEvalQ,
    clusterExport, clusterMap, parApply, parCapply, parLapply,
    parLapplyLB, parRapply, parSapply, parSapplyLB

The following objects are masked from 'package:stats':

    IQR, mad, sd, var, xtabs
... 8 lines ...
    pmin, pmin.int, rank, rbind, rowMeans, rowSums, rownames, sapply,
    setdiff, sort, table, tapply, union, unique, unsplit, which,
    which.max, which.min

Loading required package: graphite
Warning in library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE,  :
  there is no package called 'graphite'
Quitting from lines 198-206 (NFP.Rnw) 
Error: processing vignette 'NFP.Rnw' failed with diagnostics:
could not find function "pathways"
Execution halted

checking package dependencies ... NOTE
Packages suggested but not available for checking: ‘graphite’ ‘NFPdata’

checking installed package size ... NOTE
  installed size is  8.2Mb
  sub-directories of 1Mb or more:
    data   7.5Mb
```

## nzelect (0.3.3)
Maintainer: Peter Ellis <peter.ellis2013nz@gmail.com>

0 errors | 1 warning  | 0 notes

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Terminated

```

## officer (0.1.3)
Maintainer: David Gohel <david.gohel@ardata.fr>  
Bug reports: https://github.com/davidgohel/officer/issues

2 errors | 1 warning  | 0 notes

```
checking examples ... ERROR
Running examples in ‘officer-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
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

checking tests ... ERROR
  Running ‘testthat.R’ [148s/150s]
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  8: function_list[[k]](value) at /tmp/RtmpT6Czo8/R.INSTALL13c163d710cb/magrittr/R/freduce.R:20
  9: ph_with_img(., type = "body", src = img.file, height = 1.06, width = 1.39)
  10: external_img(src, width = width, height = height)
  11: stopifnot(file.exists(src))
  12: stop(msg, call. = FALSE, domain = NA)
  
  testthat results ================================================================
  OK: 341 SKIPPED: 0 FAILED: 4
  1. Error: image add  (@test-docx-add.R#68) 
  2. Error: pml fp_border (@test-fp_cell.R#75) 
  3. Error: css fp_border (@test-fp_cell.R#165) 
  4. Error: add img into placeholder (@test-pptx-add.R#67) 
  
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

Quitting from lines 180-190 (powerpoint.Rmd) 
Error: processing vignette 'powerpoint.Rmd' failed with diagnostics:
file.exists(src) is not TRUE
Execution halted

```

## padr (0.2.1)
Maintainer: Edwin Thoen <edwinthoen@gmail.com>  
Bug reports: https://github.com/EdwinTh/padr/issues

1 error  | 0 warnings | 0 notes

```
checking tests ... ERROR
  Running ‘testthat.R’ [142s/145s]
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
                                            ~^
  tests/testthat/utf-8.R:16:44: style: Commas should never have a space before.
  expect_equal( strings_addresses(names(df)) ,  strings_addresses(names(df4)) )
                                            ~^
  tests/testthat/utf-8.R:17:44: style: Commas should never have a space before.
  expect_equal( strings_addresses(names(df)) ,  strings_addresses(names(gdf4)) )
                                            ~^
  
  
  testthat results ================================================================
  OK: 192 SKIPPED: 0 FAILED: 1
  1. Failure: Package Style (@test_zzz_lintr.R#5) 
  
  Error: testthat unit tests failed
  Execution halted
```

## pivottabler (0.2.0)
Maintainer: Christopher Bailiss <cbailiss@gmail.com>  
Bug reports: https://github.com/cbailiss/pivottabler/issues

1 error  | 0 warnings | 0 notes

```
checking tests ... ERROR
  Running ‘testthat.R’ [54s/54s]
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  x[1]: 17</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">Februar 2017</th>\n 
  x[1]:    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>...
  y[1]: "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\
  y[1]: " colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">
  y[1]: December 2016</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">January 2
  y[1]: 017</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">February 2017</th>\
  y[1]: n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <t...
  
  
  testthat results ================================================================
  OK: 189 SKIPPED: 0 FAILED: 1
  1. Failure: data groups tests:  formatting data groups (@testGeneral.R#595) 
  
  Error: testthat unit tests failed
  Execution halted
```

## rattle (4.1.0)
Maintainer: Graham Williams <Graham.Williams@togaware.com>

0 errors | 1 warning  | 3 notes

```
checking re-building of vignette outputs ... WARNING
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


checking package dependencies ... NOTE
Package suggested but not available for checking: ‘pkgDepTools’

checking installed package size ... NOTE
  installed size is  6.9Mb
  sub-directories of 1Mb or more:
    data   2.5Mb
    etc    1.9Mb
    po     1.2Mb

checking dependencies in R code ... NOTE

(R:1343): Gtk-WARNING **: gtk_disable_setlocale() must be called before gtk_init()
```

## REDCapR (0.9.7)
Maintainer: Will Beasley <wibeasley@hotmail.com>  
Bug reports: https://github.com/OuhscBbmc/REDCapR/issues

1 error  | 1 warning  | 1 note 

```
checking tests ... ERROR
  Running ‘test-all.R’
Running the tests in ‘tests/test-all.R’ failed.
Complete output:
  > #Modeled after the R6 testing structure: https://github.com/wch/R6/blob/master/tests/testthat.R
  > library(testthat)
  > library(REDCapR)
  > 
  > testthat::test_check("REDCapR")
  Error: 'inst' is not an exported object from 'namespace:devtools'
  testthat results ================================================================
  OK: 10 SKIPPED: 5 FAILED: 0
  Execution halted

checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Quitting from lines 42-45 (BasicREDCapROperations.Rmd) 
Error: processing vignette 'BasicREDCapROperations.Rmd' failed with diagnostics:
object 'ds' not found
Execution halted


checking dependencies in R code ... NOTE
Missing or unexported object: ‘devtools::inst’
```

## rerddap (0.4.0)
Maintainer: Scott Chamberlain <myrmecocystus@gmail.com>  
Bug reports: https://github.com/ropensci/rerddap/issues

0 errors | 1 warning  | 0 notes

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...

Attaching package: 'dplyr'

The following objects are masked from 'package:stats':

    filter, lag

The following objects are masked from 'package:base':

    intersect, setdiff, setequal, union

Loading required package: maps
Warning: Removed 4015788 rows containing missing values (geom_raster).
Warning: Removed 902205 rows containing missing values (geom_raster).
Quitting from lines 328-337 (Using_rerddap.Rmd) 
Error: processing vignette 'Using_rerddap.Rmd' failed with diagnostics:
HTTP Status 500 - There was a (temporary?) problem.  Wait a minute, then try again.  (In a browser, click the Reload button.)
(ERROR from data source: dods.dap.DODSException: Connection cannot be opened)
(Cause: dods.dap.DODSException: Connection cannot be opened)
Execution halted

```

## sjPlot (2.3.1)
Maintainer: Daniel Lüdecke <d.luedecke@uke.de>  
Bug reports: https://github.com/sjPlot/devel/issues

0 errors | 1 warning  | 1 note 

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
17: eval(expr, envir, enclos)
18: withVisible(eval(expr, envir, enclos))
19: withCallingHandlers(withVisible(eval(expr, envir, enclos)), warning = wHandler,     error = eHandler, message = mHandler)
20: handle(ev <- withCallingHandlers(withVisible(eval(expr, envir,     enclos)), warning = wHandler, error = eHandler, message = mHandler))
21: timing_fn(handle(ev <- withCallingHandlers(withVisible(eval(expr,     envir, enclos)), warning = wHandler, error = eHandler, message = mHandler)))
22: evaluate_call(expr, parsed$src[[i]], envir = envir, enclos = enclos,     debug = debug, last = i == length(out), use_try = stop_on_error !=         2L, keep_warning = keep_warning, keep_message = keep_message,     output_handler = output_handler, include_timing = include_timing)
23: evaluate(code, envir = env, new_device = FALSE, keep_warning = !isFALSE(options$warning),     keep_message = !isFALSE(options$message), stop_on_error = if (options$error &&         options$include) 0L else 2L, output_handler = knit_handlers(options$render,         options))
... 8 lines ...
31: knitr::knit(knit_input, knit_output, envir = envir, quiet = quiet,     encoding = encoding)
32: rmarkdown::render(file, encoding = encoding, quiet = quiet, envir = globalenv())
33: vweave_rmarkdown(...)
34: engine$weave(file, quiet = quiet, encoding = enc)
35: doTryCatch(return(expr), name, parentenv, handler)
36: tryCatchOne(expr, names, parentenv, handlers[[1L]])
37: tryCatchList(expr, classes, parentenv, handlers)
38: tryCatch({    engine$weave(file, quiet = quiet, encoding = enc)    setwd(startdir)    find_vignette_product(name, by = "weave", engine = engine)}, error = function(e) {    stop(gettextf("processing vignette '%s' failed with diagnostics:\n%s",         file, conditionMessage(e)), domain = NA, call. = FALSE)})
39: buildVignettes(dir = "/home/muelleki/git/R/dplyr/revdep/checks/sjPlot.Rcheck/vign_test/sjPlot")
An irrecoverable exception occurred. R is aborting now ...
Segmentation fault (core dumped)

checking Rd cross-references ... NOTE
Package unavailable to check Rd xrefs: ‘plm’
```

## SpaDES (1.3.1)
Maintainer: Alex M Chubaty <alexander.chubaty@canada.ca>  
Bug reports: https://github.com/PredictiveEcology/SpaDES/issues

0 errors | 1 warning  | 2 notes

```
checking whether package ‘SpaDES’ can be installed ... WARNING
Found the following significant warnings:
  Warning: no DISPLAY variable so Tk is not available
See ‘/home/muelleki/git/R/dplyr/revdep/checks/SpaDES.Rcheck/00install.out’ for details.

checking package dependencies ... NOTE
Package suggested but not available for checking: ‘fastshp’

checking installed package size ... NOTE
  installed size is  5.9Mb
  sub-directories of 1Mb or more:
    R     2.8Mb
    doc   2.1Mb
```

## sparseHessianFD (0.3.3)
Maintainer: Michael Braun <braunm@smu.edu>

0 errors | 1 warning  | 0 notes

```
checking re-building of vignette outputs ... WARNING
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

## ss3sim (0.9.5)
Maintainer: Sean Anderson <sean@seananderson.ca>  
Bug reports: https://github.com/ss3sim/ss3sim/issues

0 errors | 1 warning  | 0 notes

```
checking whether package ‘ss3sim’ can be installed ... WARNING
Found the following significant warnings:
  Warning: no DISPLAY variable so Tk is not available
See ‘/home/muelleki/git/R/dplyr/revdep/checks/ss3sim.Rcheck/00install.out’ for details.
```

## textmining (0.0.1)
Maintainer: Jan Idziak <JanIdziak@gmail.com>

0 errors | 1 warning  | 0 notes

```
checking whether package ‘textmining’ can be installed ... WARNING
Found the following significant warnings:
  Warning: no DISPLAY variable so Tk is not available
See ‘/home/muelleki/git/R/dplyr/revdep/checks/textmining.Rcheck/00install.out’ for details.
```

## tidytext (0.1.2)
Maintainer: Julia Silge <julia.silge@gmail.com>  
Bug reports: http://github.com/juliasilge/tidytext/issues

2 errors | 1 warning  | 0 notes

```
checking examples ... ERROR
Running examples in ‘tidytext-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: corpus_tidiers
> ### Title: Tidiers for a corpus object from the quanteda package
> ### Aliases: corpus_tidiers glance.corpus tidy.corpus
> 
> ### ** Examples
> 
> 
> if (requireNamespace("quanteda", quietly = FALSE)) {
+  data("inaugCorpus", package = "quanteda")
+ 
+  inaugCorpus
+ 
+  tidy(inaugCorpus)
+ }
Loading required namespace: quanteda
Warning in data("inaugCorpus", package = "quanteda") :
  data set ‘inaugCorpus’ not found
Error: object 'inaugCorpus' not found
Execution halted

checking tests ... ERROR
  Running ‘testthat.R’ [11s/11s]
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  5: eval(expr, pf)
  6: quanteda::dfm(quanteda::inaugCorpus)
  7: quanteda::inaugCorpus at /tmp/RtmpuKlkGu/devtools18335daf6176/quanteda/R/dfm.R:128
  8: getExportedValue(pkg, name)
  9: stop(gettextf("'%s' is not an exported object from 'namespace:%s'", name, getNamespaceName(ns)), 
         call. = FALSE, domain = NA)
  
  testthat results ================================================================
  OK: 126 SKIPPED: 0 FAILED: 3
  1. Error: Can tidy corpus from quanteda package (@test-corpus-tidiers.R#23) 
  2. Error: can tidy a quanteda dictionary (@test-dictionary-tidiers.R#9) 
  3. Error: Can tidy dfm from quanteda (@test-sparse-tidiers.R#28) 
  
  Error: testthat unit tests failed
  Execution halted

checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
The following objects are masked from 'package:stats':

    filter, lag

The following objects are masked from 'package:base':

    intersect, setdiff, setequal, union
... 8 lines ...

The following object is masked from 'package:ggplot2':

    annotate

Warning in data("inaugCorpus", package = "quanteda") :
  data set 'inaugCorpus' not found
Quitting from lines 79-85 (tidying_casting.Rmd) 
Error: processing vignette 'tidying_casting.Rmd' failed with diagnostics:
object 'inaugCorpus' not found
Execution halted
```

## vdmR (0.2.3)
Maintainer: Tomokazu Fujino <fujino@fwu.ac.jp>

0 errors | 1 warning  | 0 notes

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
 7: vscat(MortalityRate, FertilityRate, vsfuk2012, "scat01", "vsfuk2012")
 8: eval(expr, envir, enclos)
 9: eval(expr, envir, enclos)
10: withVisible(eval(expr, envir, enclos))
11: withCallingHandlers(withVisible(eval(expr, envir, enclos)), warning = wHandler,     error = eHandler, message = mHandler)
12: handle(ev <- withCallingHandlers(withVisible(eval(expr, envir,     enclos)), warning = wHandler, error = eHandler, message = mHandler))
13: timing_fn(handle(ev <- withCallingHandlers(withVisible(eval(expr,     envir, enclos)), warning = wHandler, error = eHandler, message = mHandler)))
... 8 lines ...
21: withCallingHandlers(if (tangle) process_tangle(group) else process_group(group),     error = function(e) {        setwd(wd)        cat(res, sep = "\n", file = output %n% "")        message("Quitting from lines ", paste(current_lines(i),             collapse = "-"), " (", knit_concord$get("infile"),             ") ")    })
22: process_file(text, output)
23: (if (grepl("\\.[Rr]md$", file)) knit2html_v1 else if (grepl("\\.[Rr]rst$",     file)) knit2pdf else knit)(file, encoding = encoding, quiet = quiet,     envir = globalenv())
24: engine$weave(file, quiet = quiet, encoding = enc)
25: doTryCatch(return(expr), name, parentenv, handler)
26: tryCatchOne(expr, names, parentenv, handlers[[1L]])
27: tryCatchList(expr, classes, parentenv, handlers)
28: tryCatch({    engine$weave(file, quiet = quiet, encoding = enc)    setwd(startdir)    find_vignette_product(name, by = "weave", engine = engine)}, error = function(e) {    stop(gettextf("processing vignette '%s' failed with diagnostics:\n%s",         file, conditionMessage(e)), domain = NA, call. = FALSE)})
29: buildVignettes(dir = "/home/muelleki/git/R/dplyr/revdep/checks/vdmR.Rcheck/vign_test/vdmR")
An irrecoverable exception occurred. R is aborting now ...
Segmentation fault (core dumped)
```

## wrswoR (1.0-1)
Maintainer: Kirill Müller <krlmlr+r@mailbox.org>  
Bug reports: https://github.com/krlmlr/wrswoR/issues

0 errors | 1 warning  | 1 note 

```
checking re-building of vignette outputs ... WARNING
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


checking compiled code ... NOTE
File ‘wrswoR/libs/wrswoR.so’:
  Found no calls to: ‘R_registerRoutines’, ‘R_useDynamicSymbols’

It is good practice to register native routines and to disable symbol
search.

See ‘Writing portable packages’ in the ‘Writing R Extensions’ manual.
```

