# Setup

## Platform

|setting  |value                        |
|:--------|:----------------------------|
|version  |R version 3.3.3 (2017-03-06) |
|system   |x86_64, linux-gnu            |
|ui       |X11                          |
|language |(EN)                         |
|collate  |en_US.UTF-8                  |
|tz       |Zulu                         |
|date     |2017-04-11                   |

## Packages

|package        |*  |version    |date       |source                           |
|:--------------|:--|:----------|:----------|:--------------------------------|
|assertthat     |   |0.1        |2013-12-06 |cran (@0.1)                      |
|BH             |   |1.62.0-1   |2016-11-19 |cran (@1.62.0-)                  |
|covr           |   |2.2.2      |2017-01-05 |cran (@2.2.2)                    |
|DBI            |   |0.6-11     |2017-04-06 |Github (rstats-db/DBI@20f1f02)   |
|dplyr          |   |0.5.0      |2016-06-24 |cran (@0.5.0)                    |
|dtplyr         |   |0.0.1      |2016-06-27 |cran (@0.0.1)                    |
|ggplot2        |   |2.2.1      |2016-12-30 |cran (@2.2.1)                    |
|knitr          |   |1.15.1     |2016-11-22 |cran (@1.15.1)                   |
|Lahman         |   |5.0-0      |2016-08-27 |cran (@5.0-0)                    |
|lazyeval       |   |0.2.0      |2016-06-12 |cran (@0.2.0)                    |
|magrittr       |   |1.5        |2014-11-22 |CRAN (R 3.3.1)                   |
|microbenchmark |   |1.4-2.1    |2015-11-25 |cran (@1.4-2.1)                  |
|nycflights13   |   |0.2.2      |2017-01-27 |cran (@0.2.2)                    |
|R6             |   |2.2.0      |2016-10-05 |cran (@2.2.0)                    |
|Rcpp           |   |0.12.10    |2017-04-11 |Github (RcppCore/Rcpp@43e53b0)   |
|rmarkdown      |   |1.4        |2017-03-24 |cran (@1.4)                      |
|RMySQL         |   |0.10.11    |2017-03-29 |cran (@0.10.11)                  |
|RPostgreSQL    |   |0.4-1      |2016-05-08 |cran (@0.4-1)                    |
|RSQLite        |   |1.1-2      |2017-01-08 |cran (@1.1-2)                    |
|testthat       |   |1.0.2.9000 |2017-02-27 |Github (hadley/testthat@b72a228) |
|tibble         |   |1.3.0      |2017-03-31 |local                            |

# Check results

41 packages with problems

|package          |version | errors| warnings| notes|
|:----------------|:-------|------:|--------:|-----:|
|ameco            |0.2.6   |      1|        0|     1|
|backtestGraphics |0.1.6   |      1|        0|     0|
|bayesplot        |1.1.0   |      0|        1|     0|
|bioOED           |0.1.1   |      1|        0|     0|
|blscrapeR        |2.1.2   |      1|        0|     0|
|codingMatrices   |0.2.2   |      0|        1|     0|
|d3r              |0.6.2   |      1|        0|     1|
|dat              |0.1.0   |      1|        0|     0|
|decoder          |1.1.12  |      0|        1|     0|
|describer        |0.2.0   |      1|        0|     0|
|fitcoach         |1.0     |      1|        0|     0|
|flextable        |0.1.0   |      0|        1|     0|
|FSelectorRcpp    |0.1.2   |      1|        0|     2|
|futureheatwaves  |1.0.3   |      0|        1|     0|
|geoknife         |1.5.4   |      0|        1|     0|
|ggCompNet        |0.1.0   |      0|        1|     1|
|groupdata2       |0.1.0   |      0|        1|     0|
|gutenbergr       |0.1.2   |      1|        0|     0|
|harrietr         |0.2.2   |      1|        0|     0|
|highcharter      |0.5.0   |      0|        1|     1|
|HTSSIP           |1.0.3   |      1|        0|     0|
|huxtable         |0.1.1   |      1|        1|     0|
|IATscores        |0.1-2   |      1|        0|     0|
|NFP              |0.99.2  |      0|        1|     2|
|officer          |0.1.1   |      2|        1|     0|
|padr             |0.2.1   |      1|        0|     0|
|pivottabler      |0.1.0   |      0|        1|     0|
|plotly           |4.5.6   |      1|        0|     0|
|qualvar          |0.1.0   |      0|        1|     0|
|radiant.model    |0.6.0   |      1|        0|     0|
|rattle           |4.1.0   |      0|        1|     3|
|recexcavAAR      |0.3.0   |      0|        1|     1|
|REDCapR          |0.9.7   |      0|        1|     0|
|rgho             |1.0.1   |      1|        1|     0|
|SpaDES           |1.3.1   |      0|        2|     2|
|sparseHessianFD  |0.3.2   |      0|        1|     0|
|ss3sim           |0.9.3   |      0|        1|     0|
|stormwindmodel   |0.1.0   |      0|        1|     0|
|textmining       |0.0.1   |      0|        1|     0|
|tidyquant        |0.5.0   |      1|        0|     0|
|wrswoR           |1.0-1   |      0|        1|     0|

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

## bayesplot (1.1.0)
Maintainer: Jonah Gabry <jsg2201@columbia.edu>  
Bug reports: https://github.com/stan-dev/bayesplot/issues/

0 errors | 1 warning  | 0 notes

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Warning in engine$weave(file, quiet = quiet, encoding = enc) :
  Pandoc (>= 1.12.3) and/or pandoc-citeproc not available. Falling back to R Markdown v1.
Quitting from lines 2-14 (./children/SETTINGS-knitr.txt) 
Quitting from lines NA-14 (./children/SETTINGS-knitr.txt) 
Error: processing vignette 'MCMC-diagnostics.Rmd' failed with diagnostics:
object 'params' not found
Execution halted

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

## blscrapeR (2.1.2)
Maintainer: Kris Eberwein <eberwein@knights.ucf.edu>  
Bug reports: https://github.com/keberwein/blscrapeR/issues

1 error  | 0 warnings | 0 notes

```
checking tests ... ERROR
  Running ‘testthat.R’
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
         on.exit(options(expressions = expressions_opt), add = TRUE)
         e$expectation_calls <- frame_calls(11, 2)
         test_error <<- e
         register_expectation(e)
         e$handled <- TRUE
         test_error <<- e
     }, "attempt to select less than one element in integerOneIndex", quote(datalist[[i]] <- mth_vals)) at testthat/test_bls_csv.R:76
  2: eval(expr, envir, enclos)
  
  testthat results ================================================================
  OK: 2 SKIPPED: 0 FAILED: 1
  1. Error: (unknown) (@test_bls_csv.R#76) 
  
  Error: testthat unit tests failed
  Execution halted
```

## codingMatrices (0.2.2)
Maintainer: Bill Venables <Bill.Venables@gmail.com>

0 errors | 1 warning  | 0 notes

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...

Attaching package: 'car'

The following object is masked from 'package:dplyr':

    recode

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

## d3r (0.6.2)
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
  OK: 9 SKIPPED: 3 FAILED: 1
  1. Failure: d3_igraph works (@test_igraph.R#36) 
  
  Error: testthat unit tests failed
  Execution halted

checking package dependencies ... NOTE
Package which this enhances but not available for checking: ‘treemap’
```

## dat (0.1.0)
Maintainer: Sebastian Warnholz <wahani@gmail.com>  
Bug reports: https://github.com/wahani/dat/issues

1 error  | 0 warnings | 0 notes

```
checking tests ... ERROR
  Running ‘testthat.R’ [61s/64s]
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
  OK: 107 SKIPPED: 0 FAILED: 1
  1. Failure: Package Style (@test-lintr.R#5) 
  
  Error: testthat unit tests failed
  Execution halted
```

## decoder (1.1.12)
Maintainer: Erik Bulow <erik.bulow@rccvast.se>  
Bug reports: https://www.bitbucket.com/cancercentrum/decoder/issues

0 errors | 1 warning  | 0 notes

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Warning in engine$weave(file, quiet = quiet, encoding = enc) :
  Pandoc (>= 1.12.3) and/or pandoc-citeproc not available. Falling back to R Markdown v1.
Warning in engine$weave(file, quiet = quiet, encoding = enc) :
  Pandoc (>= 1.12.3) and/or pandoc-citeproc not available. Falling back to R Markdown v1.
Warning in engine$weave(file, quiet = quiet, encoding = enc) :
  Pandoc (>= 1.12.3) and/or pandoc-citeproc not available. Falling back to R Markdown v1.
Joining, by = "key"
Joining, by = "key"
PhantomJS not found. You can install it with webshot::install_phantomjs(). If it is installed, please make sure the phantomjs executable can be found via the PATH variable.
Quitting from lines 102-122 (sjukhus.Rmd) 
Error: processing vignette 'sjukhus.Rmd' failed with diagnostics:
cannot open the connection
Execution halted

```

## describer (0.2.0)
Maintainer: Paul Hendricks <paul.hendricks.2013@owu.edu>  
Bug reports: https://github.com/paulhendricks/describer/issues

1 error  | 0 warnings | 0 notes

```
checking tests ... ERROR
  Running ‘testthat.R’ [156s/158s]
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

## fitcoach (1.0)
Maintainer: Niraj Juneja <njuneja@gmail.com>

1 error  | 0 warnings | 0 notes

```
checking tests ... ERROR
  Running ‘testthat.R’
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  1. Failure: FitAnalyzer test cases (@test-fitanalyzer.R#28) --------------------
  res[1] is not strictly less than 2820. Difference: 29.2
  
  
  2. Failure: FitAnalyzer test cases (@test-fitanalyzer.R#49) --------------------
  `res` is not strictly less than 2518. Difference: 2.3
  
  
  testthat results ================================================================
  OK: 17 SKIPPED: 0 FAILED: 2
  1. Failure: FitAnalyzer test cases (@test-fitanalyzer.R#28) 
  2. Failure: FitAnalyzer test cases (@test-fitanalyzer.R#49) 
  
  Error: testthat unit tests failed
  Execution halted
```

## flextable (0.1.0)
Maintainer: David Gohel <david.gohel@ardata.fr>

0 errors | 1 warning  | 0 notes

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Warning in engine$weave(file, quiet = quiet, encoding = enc) :
  Pandoc (>= 1.12.3) and/or pandoc-citeproc not available. Falling back to R Markdown v1.
Loading required package: officer

Attaching package: 'dplyr'

The following objects are masked from 'package:stats':

    filter, lag

The following objects are masked from 'package:base':

    intersect, setdiff, setequal, union

PhantomJS not found. You can install it with webshot::install_phantomjs(). If it is installed, please make sure the phantomjs executable can be found via the PATH variable.
Quitting from lines 39-41 (format.Rmd) 
Error: processing vignette 'format.Rmd' failed with diagnostics:
cannot open the connection
Execution halted

```

## FSelectorRcpp (0.1.2)
Maintainer: Zygmunt Zawadzki <zygmunt@zstat.pl>  
Bug reports: https://github.com/mi2-warsaw/FSelectorRcpp/issues

1 error  | 0 warnings | 2 notes

```
checking examples ... ERROR
Running examples in ‘FSelectorRcpp-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: feature_search
> ### Title: General Feature Searching Engine
> ### Aliases: feature_search
> 
> ### ** Examples
> 
> 
> # Enable parallelization in examples
>  library(doSNOW) # doSNOW has an option for progress bar
Loading required package: foreach
Loading required package: iterators
Loading required package: snow
>  cl <- makeCluster(2)
Loading required namespace: Rmpi
Error in Rmpi::mpi.comm.spawn(slave = mpitask, slavearg = args, nslaves = count,  : 
  MPI_Comm_spawn is not supported.
Calls: makeCluster -> makeMPIcluster -> <Anonymous>
Execution halted

checking package dependencies ... NOTE
Package suggested but not available for checking: ‘RTCGA.rnaseq’

checking installed package size ... NOTE
  installed size is  9.7Mb
  sub-directories of 1Mb or more:
    doc    2.2Mb
    libs   7.3Mb
```

## futureheatwaves (1.0.3)
Maintainer: Brooke Anderson <brooke.anderson@colostate.edu>

0 errors | 1 warning  | 0 notes

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Warning in engine$weave(file, quiet = quiet, encoding = enc) :
  Pandoc (>= 1.12.3) and/or pandoc-citeproc not available. Falling back to R Markdown v1.
Quitting from lines 604-605 (futureheatwaves.Rmd) 
Error: processing vignette 'futureheatwaves.Rmd' failed with diagnostics:
cannot open the connection
Execution halted

```

## geoknife (1.5.4)
Maintainer: Jordan Read <jread@usgs.gov>  
Bug reports: https://github.com/USGS-R/geoknife/issues

0 errors | 1 warning  | 0 notes

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Warning in engine$weave(file, quiet = quiet, encoding = enc) :
  Pandoc (>= 1.12.3) and/or pandoc-citeproc not available. Falling back to R Markdown v1.
Quitting from lines 360-364 (geoknife.Rmd) 
Error: processing vignette 'geoknife.Rmd' failed with diagnostics:
need finite 'xlim' values
Execution halted

```

## ggCompNet (0.1.0)
Maintainer: Sam Tyner <sctyner@iastate.edu>  
Bug reports: https://github.com/sctyner/ggCompNet/issues

0 errors | 1 warning  | 1 note 

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Warning in engine$weave(file, quiet = quiet, encoding = enc) :
  Pandoc (>= 1.12.3) and/or pandoc-citeproc not available. Falling back to R Markdown v1.

Attaching package: 'dplyr'

The following objects are masked from 'package:stats':

    filter, lag

The following objects are masked from 'package:base':

    intersect, setdiff, setequal, union

Warning in engine$weave(file, quiet = quiet, encoding = enc) :
  Pandoc (>= 1.12.3) and/or pandoc-citeproc not available. Falling back to R Markdown v1.
Warning: Removed 8 rows containing missing values (geom_segment).
Quitting from lines 617-629 (examples-from-paper.Rmd) 
Error: processing vignette 'examples-from-paper.Rmd' failed with diagnostics:
GeomRasterAnn was built with an incompatible version of ggproto.
Please reinstall the package that provides this extension.
Execution halted


checking installed package size ... NOTE
  installed size is  6.4Mb
  sub-directories of 1Mb or more:
    doc   6.1Mb
```

## groupdata2 (0.1.0)
Maintainer: Ludvig Renbo Olsen <r-pkgs@ludvigolsen.dk>

0 errors | 1 warning  | 0 notes

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...

Loading required package: lme4

Attaching package: 'lmerTest'

The following object is masked from 'package:lme4':

... 8 lines ...

Attaching package: 'zoo'

The following objects are masked from 'package:base':

    as.Date, as.Date.numeric

Quitting from lines 293-301 (introduction_to_groupdata2.Rmd) 
Error: processing vignette 'introduction_to_groupdata2.Rmd' failed with diagnostics:
the table must have a header (column names)
Execution halted
```

## gutenbergr (0.1.2)
Maintainer: David Robinson <admiral.david@gmail.com>  
Bug reports: http://github.com/ropenscilabs/gutenbergr/issues

1 error  | 0 warnings | 0 notes

```
checking tests ... ERROR
  Running ‘testthat.R’ [7s/11s]
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  
  
  testthat results ================================================================
  OK: 50 SKIPPED: 0 FAILED: 8
  1. Error: gutenberg_get_mirror works (@test-download.R#13) 
  2. Failure: Can download Charles Dickens' Christmas Carol and Jane Austen's Persuasion (@test-download.R#22) 
  3. Failure: Can download Charles Dickens' Christmas Carol and Jane Austen's Persuasion (@test-download.R#23) 
  4. Failure: Can download Charles Dickens' Christmas Carol and Jane Austen's Persuasion (@test-download.R#31) 
  5. Failure: Can download Charles Dickens' Christmas Carol and Jane Austen's Persuasion (@test-download.R#32) 
  6. Failure: Can download books from a data frame with gutenberg_id column (@test-download.R#43) 
  7. Failure: We can download a file that only has a -8 version (@test-download.R#50) 
  8. Failure: read_zip_url can download and read a zip file (@test-utils.R#7) 
  
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

## highcharter (0.5.0)
Maintainer: Joshua Kunst <jbkunst@gmail.com>  
Bug reports: https://github.com/jbkunst/highcharter/issues

0 errors | 1 warning  | 1 note 

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Warning in engine$weave(file, quiet = quiet, encoding = enc) :
  Pandoc (>= 1.12.3) and/or pandoc-citeproc not available. Falling back to R Markdown v1.
Highcharts (www.highcharts.com) is a Highsoft software product which is
not free for commercial and Governmental use
Quitting from lines 45-46 (charting-data-frames.Rmd) 
Error: processing vignette 'charting-data-frames.Rmd' failed with diagnostics:
cannot open the connection
Execution halted


checking installed package size ... NOTE
  installed size is 16.5Mb
  sub-directories of 1Mb or more:
    doc          13.7Mb
    htmlwidgets   1.9Mb
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

## huxtable (0.1.1)
Maintainer: David Hugh-Jones <davidhughjones@gmail.com>  
Bug reports: https://github.com/hughjonesd/huxtable/issues

1 error  | 1 warning  | 0 notes

```
checking tests ... ERROR
  Running ‘testthat.R’
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
         message = handle_message)) at /tmp/Rtmp8QnkSI/devtoolse928789ce822/hadley-testthat-b72a228/R/evaluate-promise.R:42
  4: withCallingHandlers(withVisible(code), warning = handle_warning, message = handle_message) at /tmp/Rtmp8QnkSI/devtoolse928789ce822/hadley-testthat-b72a228/R/evaluate-promise.R:130
  5: withVisible(code)
  6: rmarkdown::render("rowheight-multicol-test.Rmd", quiet = TRUE)
  7: convert(output_file, run_citeproc) at /tmp/Rtmp3nAXji/devtools8a9b3238f6f1/rmarkdown/R/render.R:655
  8: pandoc_convert(utf8_input, pandoc_to, output_format$pandoc$from, output, citeproc, 
         output_format$pandoc$args, !quiet) at /tmp/Rtmp3nAXji/devtools8a9b3238f6f1/rmarkdown/R/render.R:585
  9: stop("pandoc document conversion failed with error ", result, call. = FALSE) at /tmp/Rtmp3nAXji/devtools8a9b3238f6f1/rmarkdown/R/pandoc.R:100
  
  testthat results ================================================================
  OK: 9 SKIPPED: 10 FAILED: 1
  1. Error: Row heights do not screw up latex multicol (@test-output.R#44) 
  
  Error: testthat unit tests failed
  Execution halted

checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Warning in engine$weave(file, quiet = quiet, encoding = enc) :
  Pandoc (>= 1.12.3) and/or pandoc-citeproc not available. Falling back to R Markdown v1.
Quitting from lines 51-102 (design-principles.Rmd) 
Error: processing vignette 'design-principles.Rmd' failed with diagnostics:
argument is of length zero
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

... 8 lines ...
    match, mget, order, paste, pmax, pmax.int, pmin, pmin.int, rank,
    rbind, rownames, sapply, setdiff, sort, table, tapply, union,
    unique, unsplit, which, which.max, which.min

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
  installed size is  8.4Mb
  sub-directories of 1Mb or more:
    data   7.5Mb
```

## officer (0.1.1)
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
  Running ‘testthat.R’ [30s/30s]
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  9: ph_with_img(., type = "body", src = img.file, height = 1.06, width = 1.39)
  10: external_img(src, width = width, height = height)
  11: stopifnot(file.exists(src))
  12: stop(sprintf(ngettext(length(r), "%s is not TRUE", "%s are not all TRUE"), ch), call. = FALSE, 
         domain = NA)
  
  testthat results ================================================================
  OK: 317 SKIPPED: 0 FAILED: 4
  1. Error: image add  (@test-docx-add.R#68) 
  2. Error: pml fp_border (@test-fp_cell.R#75) 
  3. Error: css fp_border (@test-fp_cell.R#165) 
  4. Error: add img into placeholder (@test-pptx-add.R#67) 
  
  Error: testthat unit tests failed
  Execution halted

checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Warning in engine$weave(file, quiet = quiet, encoding = enc) :
  Pandoc (>= 1.12.3) and/or pandoc-citeproc not available. Falling back to R Markdown v1.
Quitting from lines 181-191 (powerpoint.Rmd) 
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
  Running ‘testthat.R’ [168s/168s]
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

## pivottabler (0.1.0)
Maintainer: Christopher Bailiss <cbailiss@gmail.com>  
Bug reports: https://github.com/cbailiss/pivottabler/issues

0 errors | 1 warning  | 0 notes

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Warning in engine$weave(file, quiet = quiet, encoding = enc) :
  Pandoc (>= 1.12.3) and/or pandoc-citeproc not available. Falling back to R Markdown v1.
Warning in normalizePath(f2) :
  path[1]="./webshot12c1217d01f9.png": No such file or directory
Warning in file(con, "rb") :
  cannot open file './webshot12c1217d01f9.png': No such file or directory
Quitting from lines 53-60 (calculations.Rmd) 
Error: processing vignette 'calculations.Rmd' failed with diagnostics:
cannot open the connection
Execution halted

```

## plotly (4.5.6)
Maintainer: Carson Sievert <cpsievert1@gmail.com>  
Bug reports: https://github.com/ropensci/plotly/issues

1 error  | 0 warnings | 0 notes

```
checking tests ... ERROR
  Running ‘testthat.R’ [71s/71s]
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
  testthat results ================================================================
  OK: 813 SKIPPED: 17 FAILED: 1
  1. Error: datetimes are converted to e.g. 2013-01-02 05:00:00 (@test-ggplot-date.R#11) 
  
  Error: testthat unit tests failed
  Execution halted
```

## qualvar (0.1.0)
Maintainer: Joel Gombin <joel.gombin@gmail.com>

0 errors | 1 warning  | 0 notes

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Warning in engine$weave(file, quiet = quiet, encoding = enc) :
  Pandoc (>= 1.12.3) and/or pandoc-citeproc not available. Falling back to R Markdown v1.
Quitting from lines 92-106 (wilcox1973.Rmd) 
Error: processing vignette 'wilcox1973.Rmd' failed with diagnostics:
cannot open the connection
Execution halted

```

## radiant.model (0.6.0)
Maintainer: Vincent Nijs <radiant@rady.ucsd.edu>  
Bug reports: https://github.com/radiant-rstats/radiant.model/issues

1 error  | 0 warnings | 0 notes

```
checking examples ... ERROR
Running examples in ‘radiant.model-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: plot.model.predict
> ### Title: Plot method for model.predict functions
> ### Aliases: plot.model.predict
> 
> ### ** Examples
> 
> regress("diamonds", "price", c("carat","clarity")) %>%
+   predict(pred_cmd = "carat = 1:10") %>%
+   plot(xvar = "carat")
> logistic("titanic", "survived", c("pclass","sex","age"), lev = "Yes") %>%
+   predict(pred_cmd="pclass=levels(pclass), sex=levels(sex), age=seq(0,100,20)") %>%
+   plot(xvar = "age", color = "sex", facet_col = "pclass")
Error in rep(yes, length.out = length(ans)) : 
  attempt to replicate an object of type 'closure'
Calls: %>% ... <Anonymous> -> plot -> plot.model.predict -> ifelse
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

(R:13863): Gtk-WARNING **: gtk_disable_setlocale() must be called before gtk_init()
```

## recexcavAAR (0.3.0)
Maintainer: Clemens Schmid <clemens@nevrome.de>

0 errors | 1 warning  | 1 note 

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Warning in engine$weave(file, quiet = quiet, encoding = enc) :
  Pandoc (>= 1.12.3) and/or pandoc-citeproc not available. Falling back to R Markdown v1.
Loading required package: kriging

Attaching package: 'dplyr'

The following objects are masked from 'package:stats':

    filter, lag

The following objects are masked from 'package:base':

    intersect, setdiff, setequal, union

Quitting from lines 70-71 (recexcavAAR-vignette-1.Rmd) 
Error: processing vignette 'recexcavAAR-vignette-1.Rmd' failed with diagnostics:
cannot open the connection
Execution halted


checking installed package size ... NOTE
  installed size is  5.9Mb
  sub-directories of 1Mb or more:
    doc    2.5Mb
    libs   3.3Mb
```

## REDCapR (0.9.7)
Maintainer: Will Beasley <wibeasley@hotmail.com>  
Bug reports: https://github.com/OuhscBbmc/REDCapR/issues

0 errors | 1 warning  | 0 notes

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Warning in engine$weave(file, quiet = quiet, encoding = enc) :
  Pandoc (>= 1.12.3) and/or pandoc-citeproc not available. Falling back to R Markdown v1.
Quitting from lines 42-45 (BasicREDCapROperations.Rmd) 
Error: processing vignette 'BasicREDCapROperations.Rmd' failed with diagnostics:
object 'ds' not found
Execution halted

```

## rgho (1.0.1)
Maintainer: Antoine Filipovic-Pierucci <pierucci@gmail.com>  
Bug reports: https://github.com/pierucci/rgho/issues

1 error  | 1 warning  | 0 notes

```
checking examples ... ERROR
Running examples in ‘rgho-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: get_gho_data
> ### Title: Returns GHO Data
> ### Aliases: get_gho_data
> 
> ### ** Examples
... 36 lines ...
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

checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Warning in engine$weave(file, quiet = quiet, encoding = enc) :
  Pandoc (>= 1.12.3) and/or pandoc-citeproc not available. Falling back to R Markdown v1.
Quitting from lines 101-111 (a-intro.Rmd) 
Error: processing vignette 'a-intro.Rmd' failed with diagnostics:
No data returned by WHO GHO server.
Execution halted

```

## SpaDES (1.3.1)
Maintainer: Alex M Chubaty <alexander.chubaty@canada.ca>  
Bug reports: https://github.com/PredictiveEcology/SpaDES/issues

0 errors | 2 warnings | 2 notes

```
checking whether package ‘SpaDES’ can be installed ... WARNING
Found the following significant warnings:
  Warning: no DISPLAY variable so Tk is not available
See ‘/home/muelleki/git/R/dplyr/revdep/checks/SpaDES.Rcheck/00install.out’ for details.

checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...

    RFoptions

The following object is masked from 'package:raster':

    atan2

... 8 lines ...

Attaching package: 'grid'

The following object is masked from 'package:SpaDES':

    gpar

Quitting from lines 352-356 (ii-modules.Rmd) 
Error: processing vignette 'ii-modules.Rmd' failed with diagnostics:
cannot open the connection
Execution halted

checking package dependencies ... NOTE
Package suggested but not available for checking: ‘fastshp’

checking installed package size ... NOTE
  installed size is  5.6Mb
  sub-directories of 1Mb or more:
    R     2.5Mb
    doc   2.1Mb
```

## sparseHessianFD (0.3.2)
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

## ss3sim (0.9.3)
Maintainer: Sean Anderson <sean@seananderson.ca>

0 errors | 1 warning  | 0 notes

```
checking whether package ‘ss3sim’ can be installed ... WARNING
Found the following significant warnings:
  Warning: no DISPLAY variable so Tk is not available
See ‘/home/muelleki/git/R/dplyr/revdep/checks/ss3sim.Rcheck/00install.out’ for details.
```

## stormwindmodel (0.1.0)
Maintainer: Brooke Anderson <brooke.anderson@colostate.edu>  
Bug reports: https://github.com/geanders/stormwindmodel/issues

0 errors | 1 warning  | 0 notes

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...

Attaching package: 'dplyr'

The following objects are masked from 'package:stats':

    filter, lag

... 8 lines ...
The following object is masked from 'package:dplyr':

    combine

Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=georgia&zoom=5&size=640x640&scale=2&maptype=terrain&language=en-EN&sensor=false
Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=georgia&sensor=false
Quitting from lines 220-233 (Details.Rmd) 
Error: processing vignette 'Details.Rmd' failed with diagnostics:
GeomRasterAnn was built with an incompatible version of ggproto.
Please reinstall the package that provides this extension.
Execution halted
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

## tidyquant (0.5.0)
Maintainer: Matt Dancho <mdancho@business-science.io>  
Bug reports: https://github.com/business-science/tidyquant/issues

1 error  | 0 warnings | 0 notes

```
checking tests ... ERROR
  Running ‘testthat.R’ [16s/45s]
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  
  
  testthat results ================================================================
  OK: 183 SKIPPED: 2 FAILED: 8
  1. Failure: Test returns tibble with correct rows and columns. (@test_tq_get_exchange_rates.R#20) 
  2. Failure: Test returns tibble with correct rows and columns. (@test_tq_get_exchange_rates.R#21) 
  3. Failure: Test returns tibble with correct rows and columns. (@test_tq_get_exchange_rates.R#25) 
  4. Failure: Test returns tibble with correct rows and columns. (@test_tq_get_exchange_rates.R#27) 
  5. Failure: Test returns tibble with correct rows and columns. (@test_tq_get_metal_prices.R#20) 
  6. Failure: Test returns tibble with correct rows and columns. (@test_tq_get_metal_prices.R#21) 
  7. Failure: Test returns tibble with correct rows and columns. (@test_tq_get_metal_prices.R#25) 
  8. Failure: Test returns tibble with correct rows and columns. (@test_tq_get_metal_prices.R#27) 
  
  Error: testthat unit tests failed
  Execution halted
```

## wrswoR (1.0-1)
Maintainer: Kirill Müller <krlmlr+r@mailbox.org>  
Bug reports: https://github.com/krlmlr/wrswoR/issues

0 errors | 1 warning  | 0 notes

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Warning in engine$weave(file, quiet = quiet, encoding = enc) :
  Pandoc (>= 1.12.3) and/or pandoc-citeproc not available. Falling back to R Markdown v1.
Quitting from lines 622-635 (wrswoR.Rmd) 
Error: processing vignette 'wrswoR.Rmd' failed with diagnostics:

TeX was unable to calculate metrics for the following string
or character:

	36

Common reasons for failure include:
  * The string contains a character which is special to LaTeX unless
    escaped properly, such as % or $.
  * The string makes use of LaTeX commands provided by a package and
    the tikzDevice was not told to load the package.

The contents of the LaTeX log of the aborted run have been printed above,
it may contain additional details as to why the metric calculation failed.
Execution halted

```

