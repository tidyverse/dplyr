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
|date     |2017-05-12                   |

## Packages

|package        |*  |version    |date       |source                            |
|:--------------|:--|:----------|:----------|:---------------------------------|
|assertthat     |   |0.2.0      |2017-04-11 |cran (@0.2.0)                     |
|BH             |   |1.62.0-1   |2016-11-19 |cran (@1.62.0-)                   |
|covr           |   |2.2.2      |2017-01-05 |cran (@2.2.2)                     |
|DBI            |   |0.6-13     |2017-05-08 |Github (rstats-db/DBI@f6500a5)    |
|dplyr          |   |0.5.0.9004 |2017-05-03 |Github (tidyverse/dplyr@6712f05)  |
|dtplyr         |   |0.0.2      |2017-04-21 |cran (@0.0.2)                     |
|ggplot2        |   |2.2.1      |2016-12-30 |cran (@2.2.1)                     |
|knitr          |   |1.15.1     |2016-11-22 |cran (@1.15.1)                    |
|Lahman         |   |5.0-0      |2016-08-27 |cran (@5.0-0)                     |
|lazyeval       |   |0.2.0      |2016-06-12 |cran (@0.2.0)                     |
|magrittr       |   |1.5        |2014-11-22 |CRAN (R 3.4.0)                    |
|microbenchmark |   |1.4-2.1    |2015-11-25 |cran (@1.4-2.1)                   |
|nycflights13   |   |0.2.2      |2017-01-27 |cran (@0.2.2)                     |
|R6             |   |2.2.1      |2017-05-10 |cran (@2.2.1)                     |
|Rcpp           |   |0.12.10    |2017-03-19 |CRAN (R 3.4.0)                    |
|rmarkdown      |   |1.5        |2017-04-26 |cran (@1.5)                       |
|RMySQL         |   |0.10.11    |2017-03-29 |cran (@0.10.11)                   |
|RPostgreSQL    |   |0.4-1      |2016-05-08 |cran (@0.4-1)                     |
|RSQLite        |   |1.1-2      |2017-01-08 |CRAN (R 3.4.0)                    |
|testthat       |   |1.0.2      |2016-04-23 |cran (@1.0.2)                     |
|tibble         |   |1.3.0.9002 |2017-05-12 |Github (tidyverse/tibble@9103a30) |

# Check results

41 packages with problems

|package          |version | errors| warnings| notes|
|:----------------|:-------|------:|--------:|-----:|
|ameco            |0.2.6   |      1|        0|     1|
|atlantistools    |0.4.1   |      0|        1|     1|
|backtestGraphics |0.1.6   |      1|        0|     0|
|bayesplot        |1.2.0   |      0|        1|     1|
|bioOED           |0.1.1   |      1|        0|     0|
|carpenter        |0.2.0   |      1|        1|     0|
|condformat       |0.5.0   |      2|        1|     0|
|congressbr       |0.1.0   |      1|        0|     0|
|coreSim          |0.2.3   |      2|        0|     0|
|d3r              |0.6.4   |      2|        0|     1|
|dat              |0.2.0   |      1|        0|     0|
|describer        |0.2.0   |      1|        0|     0|
|eechidna         |0.1     |      0|        1|     0|
|epicontacts      |1.0.0   |      2|        1|     0|
|filesstrings     |0.4.0   |      1|        0|     0|
|FSelectorRcpp    |0.1.3   |      1|        0|     2|
|geoknife         |1.5.4   |      1|        1|     0|
|ggfortify        |0.4.1   |      1|        0|     1|
|harrietr         |0.2.2   |      1|        0|     0|
|HTSSIP           |1.0.3   |      1|        0|     0|
|huxtable         |0.2.2   |      1|        1|     0|
|IATscores        |0.1-2   |      1|        0|     0|
|imfr             |0.1.4   |      1|        0|     0|
|monkeylearn      |0.1.1   |      0|        1|     0|
|NFP              |0.99.2  |      0|        1|     2|
|nzelect          |0.3.3   |      0|        1|     0|
|officer          |0.1.3   |      2|        1|     0|
|padr             |0.2.1   |      1|        0|     0|
|pivottabler      |0.2.0   |      1|        0|     0|
|purrrlyr         |0.0.1   |      2|        0|     1|
|rattle           |4.1.0   |      0|        1|     3|
|REDCapR          |0.9.7   |      0|        1|     0|
|ropenaq          |0.2.0   |      0|        1|     0|
|simPH            |1.3.9   |      1|        1|     0|
|SpaDES           |1.3.1   |      0|        1|     2|
|sparseHessianFD  |0.3.3   |      0|        1|     0|
|sqlscore         |0.1.1   |      1|        0|     0|
|ss3sim           |0.9.5   |      0|        1|     0|
|textmining       |0.0.1   |      0|        1|     0|
|tidytext         |0.1.2   |      2|        1|     0|
|wrswoR           |1.0-1   |      0|        1|     1|

## ameco (0.2.6)
Maintainer: Eric Persson <expersso5@gmail.com>  
Bug reports: http://github.com/expersso/ameco/issues

1 error  | 0 warnings | 1 note 

```
checking tests ... ERROR
  Running ‘testthat.R’
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  > library(testthat)
  > library(ameco)
  > 
  > test_check("ameco")
  1. Failure: Test that current version is still latest version (@tests.R#16) ----
  `last_update` not equal to as.Date("2017-02-13").
  'is.NA' value mismatch: 0 in current 1 in target
  
  
  testthat results ================================================================
  OK: 0 SKIPPED: 0 FAILED: 1
  1. Failure: Test that current version is still latest version (@tests.R#16) 
  
  Error: testthat unit tests failed
  Execution halted

checking installed package size ... NOTE
  installed size is 15.7Mb
  sub-directories of 1Mb or more:
    data  15.6Mb
```

## atlantistools (0.4.1)
Maintainer: Alexander Keth <alexander.keth@uni-hamburg.de>  
Bug reports: https://github.com/alketh/atlantistools/issues

0 errors | 1 warning  | 1 note 

```
checking sizes of PDF files under ‘inst/doc’ ... WARNING
  ‘gs+qpdf’ made some significant size reductions:
     compacted ‘model-calibration.pdf’ from 766Kb to 493Kb
  consider running tools::compactPDF(gs_quality = "ebook") on these files

checking installed package size ... NOTE
  installed size is  5.6Mb
  sub-directories of 1Mb or more:
    doc       1.5Mb
    extdata   3.0Mb
```

## backtestGraphics (0.1.6)
Maintainer: Miller Zijie Zhu <zijie.miller.zhu@gmail.com>

1 error  | 0 warnings | 0 notes

```
checking tests ... ERROR
  Running ‘test-all.R’
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

## bayesplot (1.2.0)
Maintainer: Jonah Gabry <jsg2201@columbia.edu>  
Bug reports: https://github.com/stan-dev/bayesplot/issues/

0 errors | 1 warning  | 1 note 

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...

 *** caught segfault ***
address 0x1d0000003c, cause 'memory not mapped'

Traceback:
 1: knitr::knit(knit_input, knit_output, envir = envir, quiet = quiet,     encoding = encoding)
 2: rmarkdown::render(file, encoding = encoding, quiet = quiet, envir = globalenv())
 3: vweave_rmarkdown(...)
 4: engine$weave(file, quiet = quiet, encoding = enc)
 5: doTryCatch(return(expr), name, parentenv, handler)
 6: tryCatchOne(expr, names, parentenv, handlers[[1L]])
 7: tryCatchList(expr, classes, parentenv, handlers)
 8: tryCatch({    engine$weave(file, quiet = quiet, encoding = enc)    setwd(startdir)    find_vignette_product(name, by = "weave", engine = engine)}, error = function(e) {    stop(gettextf("processing vignette '%s' failed with diagnostics:\n%s",         file, conditionMessage(e)), domain = NA, call. = FALSE)})
 9: buildVignettes(dir = "/home/muelleki/git/R/dplyr/revdep/checks/bayesplot.Rcheck/vign_test/bayesplot")
An irrecoverable exception occurred. R is aborting now ...
Segmentation fault (core dumped)


checking installed package size ... NOTE
  installed size is  5.0Mb
  sub-directories of 1Mb or more:
    R     1.6Mb
    doc   2.9Mb
```

## bioOED (0.1.1)
Maintainer: Alberto Garre <garre.alberto@gmail.com>

1 error  | 0 warnings | 0 notes

```
checking package dependencies ... ERROR
Package required but not available: ‘MEIGOR’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
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
  Running ‘testthat.R’
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

## congressbr (0.1.0)
Maintainer: Robert Myles McDonnell <robertmylesmcdonnell@gmail.com>

1 error  | 0 warnings | 0 notes

```
checking examples ... ERROR
Running examples in ‘congressbr-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: sen_bills_situations
> ### Title: Downloads and tidies information on the possible situations a
> ###   bill can be in
> ### Aliases: sen_bills_situations
> 
> ### ** Examples
> 
> sen_bills_situations()
Error in curl::curl_fetch_memory(url, handle = handle) : 
  Timeout was reached
Calls: sen_bills_situations ... request_fetch -> request_fetch.write_memory -> <Anonymous> -> .Call
Execution halted
```

## coreSim (0.2.3)
Maintainer: Christopher Gandrud <christopher.gandrud@gmail.com>  
Bug reports: https://github.com/christophergandrud/coreSim/issues

2 errors | 0 warnings | 0 notes

```
checking examples ... ERROR
Running examples in ‘coreSim-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: qi_builder
> ### Title: Find quantities of interest from generalized linear models
> ### Aliases: qi_builder
> 
> ### ** Examples
... 26 lines ...

> 
> ## Logistic regression
> # Download data
> URL <- 'http://www.ats.ucla.edu/stat/data/binary.csv'
> Admission <- read.csv(URL)
> Admission$rank <- as.factor(Admission$rank)
Error in `$<-.data.frame`(`*tmp*`, rank, value = integer(0)) : 
  replacement has 0 rows, data has 526
Calls: $<- -> $<-.data.frame
Execution halted

checking tests ... ERROR
  Running ‘testthat.R’
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  linear_systematic: .......
  qi_builder: 1
  qi_slimmer: ..
  
  Failed -------------------------------------------------------------------------
  1. Error: qi_builder output validity (@test_qi_builder.R#31) -------------------
  replacement has 0 rows, data has 526
  1: `$<-`(`*tmp*`, rank, value = structure(integer(0), .Label = character(0), class = "factor")) at testthat/test_qi_builder.R:31
  2: `$<-.data.frame`(`*tmp*`, rank, value = structure(integer(0), .Label = character(0), class = "factor")) at testthat/test_qi_builder.R:31
  3: stop(sprintf(ngettext(N, "replacement has %d row, data has %d", "replacement has %d rows, data has %d"), 
         N, nrows), domain = NA)
  
  DONE ===========================================================================
  Error: Test failures
  Execution halted
```

## d3r (0.6.4)
Maintainer: Kent Russell <kent.russell@timelyportfolio.com>  
Bug reports: https://github.com/timelyportfolio/d3r/issues

2 errors | 0 warnings | 1 note 

```
checking examples ... ERROR
Running examples in ‘d3r-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: d3_nest
> ### Title: Convert a 'data.frame' to a 'd3.js' Hierarchy
> ### Aliases: d3_nest
> 
> ### ** Examples
... 33 lines ...
 6: d3_nest(., value_cols = "Freq", root = "titanic")
 7: function_list[[k]](value)
 8: withVisible(function_list[[k]](value))
 9: freduce(value, `_function_list`)
10: `_fseq`(`_lhs`)
11: eval(quote(`_fseq`(`_lhs`)), env, env)
12: eval(quote(`_fseq`(`_lhs`)), env, env)
13: withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
14: titanic_df %>% select(Class, Age, Survived, Sex, Freq) %>% d3_nest(value_cols = "Freq",     root = "titanic")
An irrecoverable exception occurred. R is aborting now ...
Segmentation fault (core dumped)

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
  Running ‘testthat.R’ [43s/44s]
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
  Running ‘testthat.R’ [108s/108s]
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

## eechidna (0.1)
Maintainer: Ben Marwick <benmarwick@gmail.com>

0 errors | 1 warning  | 0 notes

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...

Attaching package: 'scales'

The following object is masked from 'package:purrr':

    discard

... 8 lines ...

Attaching package: 'GGally'

The following object is masked from 'package:dplyr':

    nasa

Quitting from lines 337-354 (exploring-election-data.Rmd) 
Error: processing vignette 'exploring-election-data.Rmd' failed with diagnostics:
could not find function "dmap"
Execution halted
```

## epicontacts (1.0.0)
Maintainer: VP Nagraj <vpnagraj@virginia.edu>

2 errors | 1 warning  | 0 notes

```
checking examples ... ERROR
Running examples in ‘epicontacts-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: as.igraph.epicontacts
> ### Title: Create igraph object from contact data
> ### Aliases: as.igraph.epicontacts
> 
> ### ** Examples
... 30 lines ...
The following objects are masked from ‘package:stats’:

    decompose, spectrum

The following object is masked from ‘package:base’:

    union

Error in `[.data.frame`(linelist, , id) : undefined columns selected
Calls: make_epicontacts -> [ -> [.data.frame
Execution halted

checking tests ... ERROR
  Running ‘testthat.R’
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  1: make_epicontacts(ebola_sim$linelist, ebola_sim$contacts, id = "case.id", to = "case.id", 
         from = "infector", directed = TRUE) at testthat/test_thin.R:4
  2: linelist[, id]
  3: `[.data.frame`(linelist, , id)
  4: stop("undefined columns selected")
  
  testthat results ================================================================
  OK: 8 SKIPPED: 34 FAILED: 4
  1. Error: Various subsetting using [ (@test_handling.R#4) 
  2. Error: Errors / warnings happen when they should (@test_handling.R#55) 
  3. Error: Reordering of columns works (@test_make_epicontacts.R#84) 
  4. Error: Thin ouputs are correct (@test_thin.R#4) 
  
  Error: testthat unit tests failed
  Execution halted

checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Warning in structure(x, class = unique(c("AsIs", oldClass(x)))) :
  Calling 'structure(NULL, *)' is deprecated, as NULL cannot have attributes.
  Consider 'structure(list(), *)' instead.
Quitting from lines 219-233 (epicontacts.Rmd) 
Error: processing vignette 'epicontacts.Rmd' failed with diagnostics:
undefined columns selected
Execution halted

```

## filesstrings (0.4.0)
Maintainer: Rory Nolan <rorynoolan@gmail.com>  
Bug reports: https://www.github.com/rorynolan/filesstrings/issues

1 error  | 0 warnings | 0 notes

```
checking tests ... ERROR
  Running ‘testthat.R’
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  Attributes: < Length mismatch: comparison on first 2 components >
  
  
  2. Failure: MergeTablesOnDisk works (@test_files.R#82) -------------------------
  readr::read_csv("merged.csv") not equal to tibble::tibble(x = c(1.5, 1.5), y = c(2.5, 29.5)).
  Attributes: < Length mismatch: comparison on first 2 components >
  
  
  testthat results ================================================================
  OK: 141 SKIPPED: 0 FAILED: 2
  1. Failure: MergeTablesOnDisk works (@test_files.R#80) 
  2. Failure: MergeTablesOnDisk works (@test_files.R#82) 
  
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

1 error  | 1 warning  | 0 notes

```
checking tests ... ERROR
  Running ‘testthat.R’ [5s/22s]
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  6: getNodeSet(response, xpath, namespaces = namespaces)
  7: xpathApply(doc, path, fun, ..., namespaces = namespaces, sessionEncoding = sessionEncoding, 
         addFinalizer = addFinalizer) at /tmp/Rtmp6kRorR/devtoolsd9314237cd8/XML/R/XMLClasses.R:663
  8: xpathApply.XMLInternalDocument(doc, path, fun, ..., namespaces = namespaces, sessionEncoding = sessionEncoding, 
         addFinalizer = addFinalizer) at /tmp/Rtmp6kRorR/devtoolsd9314237cd8/XML/R/XMLClasses.R:700
  9: matchNamespaces(doc, namespaces) at /tmp/Rtmp6kRorR/devtoolsd9314237cd8/XML/R/XMLClasses.R:740
  10: stop("cannot find defined namespace(s) with prefix(es) ", paste(namespaces[i][is.na(idx)], 
         collapse = ", ")) at /tmp/Rtmp6kRorR/devtoolsd9314237cd8/XML/R/XMLClasses.R:638
  
  testthat results ================================================================
  OK: 53 SKIPPED: 62 FAILED: 1
  1. Error: show datagroup (@test-show_object.R#23) 
  
  Error: testthat unit tests failed
  Execution halted

checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Quitting from lines 360-364 (geoknife.Rmd) 
Error: processing vignette 'geoknife.Rmd' failed with diagnostics:
need finite 'xlim' values
Execution halted

```

## ggfortify (0.4.1)
Maintainer: Masaaki Horikoshi <sinhrks@gmail.com>  
Bug reports: https://github.com/sinhrks/ggfortify/issues

1 error  | 0 warnings | 1 note 

```
checking tests ... ERROR
  Running ‘test-all.R’
Running the tests in ‘tests/test-all.R’ failed.
Last 13 lines of output:
  16: eval(exprs, env)
  17: source_file(path, new.env(parent = env), chdir = TRUE)
  18: force(code)
  19: with_reporter(reporter = reporter, start_end_reporter = start_end_reporter,     {        lister$start_file(basename(path))        source_file(path, new.env(parent = env), chdir = TRUE)        end_context()    })
  20: FUN(X[[i]], ...)
  21: lapply(paths, test_file, env = env, reporter = current_reporter,     start_end_reporter = FALSE, load_helpers = FALSE)
  22: force(code)
  23: with_reporter(reporter = current_reporter, results <- lapply(paths,     test_file, env = env, reporter = current_reporter, start_end_reporter = FALSE,     load_helpers = FALSE))
  24: test_files(paths, reporter = reporter, env = env, ...)
  25: test_dir(test_path, reporter = reporter, env = env, filter = filter,     ...)
  26: with_top_env(env, {    test_dir(test_path, reporter = reporter, env = env, filter = filter,         ...)})
  27: run_tests(package, test_path, filter, reporter, ...)
  28: test_check("ggfortify")
  An irrecoverable exception occurred. R is aborting now ...
  Segmentation fault (core dumped)

checking installed package size ... NOTE
  installed size is  5.7Mb
  sub-directories of 1Mb or more:
    doc   5.0Mb
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

1 error  | 1 warning  | 0 notes

```
checking tests ... ERROR
  Running ‘testthat.R’ [12s/14s]
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  4: withCallingHandlers(withVisible(code), warning = handle_warning, message = handle_message) at /tmp/Rtmpf6YF8j/devtools1eed5a0addc9/testthat/R/evaluate-promise.R:42
  5: withVisible(code) at /tmp/Rtmpf6YF8j/devtools1eed5a0addc9/testthat/R/evaluate-promise.R:42
  6: rmarkdown::render("rowheight-multicol-test.Rmd", quiet = TRUE) at /tmp/Rtmpf6YF8j/devtools1eed5a0addc9/testthat/R/evaluate-promise.R:42
  7: convert(output_file, run_citeproc) at /tmp/Rtmp2UybSq/devtoolsc0a2aa8de4c/rmarkdown/R/render.R:655
  8: pandoc_convert(utf8_input, pandoc_to, output_format$pandoc$from, output, citeproc, 
         output_format$pandoc$args, !quiet) at /tmp/Rtmp2UybSq/devtoolsc0a2aa8de4c/rmarkdown/R/render.R:585
  9: stop("pandoc document conversion failed with error ", result, call. = FALSE) at /tmp/Rtmp2UybSq/devtoolsc0a2aa8de4c/rmarkdown/R/pandoc.R:100
  
  testthat results ================================================================
  OK: 200 SKIPPED: 14 FAILED: 2
  1. Error: slice, filter and arrange work (@test-dplyr.R#24) 
  2. Error: Row heights do not screw up latex multicol (@test-with-pandoc.R#20) 
  
  Error: testthat unit tests failed
  Execution halted

checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Quitting from lines 320-337 (huxtable.Rmd) 
Error: processing vignette 'huxtable.Rmd' failed with diagnostics:
is.data.frame(df) is not TRUE
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
... 93 lines ...
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

## monkeylearn (0.1.1)
Maintainer: Maëlle Salmon <maelle.salmon@yahoo.se>  
Bug reports: http://github.com/ropenscilabs/monkeylearn/issues

0 errors | 1 warning  | 0 notes

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
pandoc: Could not fetch http://ropensci.org/public_images/github_footer.png
TlsExceptionHostPort (HandshakeFailed Error_EOF) "ropensci.org" 80
Error: processing vignette 'monkeylearn_intro.Rmd' failed with diagnostics:
pandoc document conversion failed with error 67
Execution halted

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
  Running ‘testthat.R’ [131s/131s]
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
  Running ‘testthat.R’ [115s/116s]
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
  Running ‘testthat.R’ [50s/50s]
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

## purrrlyr (0.0.1)
Maintainer: Hadley Wickham <hadley@rstudio.com>  
Bug reports: https://github.com/hadley/purrrlyr/issues

2 errors | 0 warnings | 1 note 

```
checking examples ... ERROR
Running examples in ‘purrrlyr-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: by_slice
> ### Title: Apply a function to slices of a data frame
> ### Aliases: by_slice
> 
> ### ** Examples
... 15 lines ...
> # by_slice() is especially useful in combination with map().
> 
> # To modify the contents of a data frame, use rows collation. Note
> # that unlike dplyr, Mutating and summarising operations can be
> # used indistinctly.
> 
> # Mutating operation:
> df <- mtcars %>% slice_rows(c("cyl", "am"))
> df %>% by_slice(dmap, ~ .x / sum(.x), .collate = "rows")
Error: 'as_mapper' is not an exported object from 'namespace:purrr'
Execution halted

checking tests ... ERROR
  Running ‘testthat.R’
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
         call. = FALSE, domain = NA)
  
  testthat results ================================================================
  OK: 69 SKIPPED: 0 FAILED: 8
  1. Error: dmap() returns a data frame (@test-dmap.R#4) 
  2. Error: dmap() works with sliced data frames (@test-dmap.R#9) 
  3. Error: dmap() works with no columns to map (@test-dmap.R#15) 
  4. Error: dmap() recycles only vectors of length 1 (@test-dmap.R#20) 
  5. Error: conditional sliced mapping recycles within groups (@test-dmap.R#26) 
  6. Error: output column is named according to .to (@test-rows.R#21) 
  7. Error: by_slice() works with slicers of different types (@test-rows.R#203) 
  8. Error: by_row() creates indices with c++ style indexing (@test-rows.R#228) 
  
  Error: testthat unit tests failed
  Execution halted

checking dependencies in R code ... NOTE
Missing or unexported object: ‘purrr::as_mapper’
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

(R:18122): Gtk-WARNING **: gtk_disable_setlocale() must be called before gtk_init()
```

## REDCapR (0.9.7)
Maintainer: Will Beasley <wibeasley@hotmail.com>  
Bug reports: https://github.com/OuhscBbmc/REDCapR/issues

0 errors | 1 warning  | 0 notes

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Quitting from lines 42-45 (BasicREDCapROperations.Rmd) 
Error: processing vignette 'BasicREDCapROperations.Rmd' failed with diagnostics:
object 'ds' not found
Execution halted

```

## ropenaq (0.2.0)
Maintainer: Maëlle Salmon <maelle.salmon@yahoo.se>  
Bug reports: http://github.com/ropensci/ropenaq/issues

0 errors | 1 warning  | 0 notes

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
pandoc: Could not fetch http://ropensci.org/public_images/github_footer.png
TlsExceptionHostPort (HandshakeFailed Error_EOF) "ropensci.org" 80
Error: processing vignette 'Ropenaq-vignette.Rmd' failed with diagnostics:
pandoc document conversion failed with error 67
Execution halted

```

## simPH (1.3.9)
Maintainer: Christopher Gandrud <christopher.gandrud@gmail.com>  
Bug reports: https://github.com/christophergandrud/simPH/issues

1 error  | 1 warning  | 0 notes

```
checking examples ... ERROR
Running examples in ‘simPH-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: coxsimInteract
> ### Title: Simulate quantities of interest for linear multiplicative
> ###   interactions */ from Cox Proportional Hazards models
> ### Aliases: coxsimInteract
> 
... 42 lines ...
> 
> # Example with a categorical variable
> # Download data
> hmohiv <- read.csv("http://www.ats.ucla.edu/stat/r/examples/asa/hmohiv.csv",
+                      stringsAsFactors = FALSE)
> 
> # Create category lables
> hmohiv$drug <- factor(hmohiv$drug, labels = c('not treated', 'treated'))
Error in factor(hmohiv$drug, labels = c("not treated", "treated")) : 
  invalid 'labels'; length 2 should be 1 or 0
Execution halted

checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Quitting from lines 277-278 (simPH-overview.Rnw) 
Error: processing vignette 'simPH-overview.Rnw' failed with diagnostics:
replacement has 0 rows, data has 526
Execution halted

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

## sqlscore (0.1.1)
Maintainer: William Brannon <wwbrannon@email.wm.edu>  
Bug reports: https://github.com/wwbrannon/sqlscore/issues

1 error  | 0 warnings | 0 notes

```
checking tests ... ERROR
  Running ‘testthat.R’ [12s/12s]
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  2: glmboost.formula(as.factor(Sepal.Length > 5.1) ~ Sepal.Width + Petal.Length + Petal.Width + 
         Species, data = datasets::iris, family = mboost::Binomial("logit")) at /tmp/Rtmp15vxzA/devtoolsb3732e07af3/mboost/R/mboost.R:619
  3: mboost_fit(bl, response = response, weights = weights, control = control, ...) at /tmp/Rtmp15vxzA/devtoolsb3732e07af3/mboost/R/mboost.R:674
  4: mboost::Binomial("logit") at /tmp/Rtmp15vxzA/devtoolsb3732e07af3/mboost/R/mboost.R:22
  5: match.arg(type) at /tmp/Rtmp15vxzA/devtoolsb3732e07af3/mboost/R/family.R:114
  6: stop(gettextf("'arg' should be one of %s", paste(dQuote(choices), collapse = ", ")), 
         domain = NA)
  
  testthat results ================================================================
  OK: 94 SKIPPED: 0 FAILED: 2
  1. Error: glmboost coefficients are extracted correctly (@test-extract_coef.R#53) 
  2. Error: Logit glmboost is handled correctly (@test-score_expression.R#140) 
  
  Error: testthat unit tests failed
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
  Running ‘testthat.R’ [10s/11s]
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

