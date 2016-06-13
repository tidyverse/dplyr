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
|date     |2016-06-09                   |

## Packages

|package        |*  |version    |date       |source                   |
|:--------------|:--|:----------|:----------|:------------------------|
|assertthat     |   |0.1        |2013-12-06 |CRAN (R 3.3.0)           |
|BH             |   |1.60.0-2   |2016-05-07 |CRAN (R 3.3.0)           |
|covr           |   |2.0.1      |2016-04-06 |CRAN (R 3.3.0)           |
|DBI            |   |0.4-1      |2016-05-08 |CRAN (R 3.3.0)           |
|dplyr          |   |0.4.3.9001 |2016-06-09 |local (hadley/dplyr@NA)  |
|dtplyr         |   |0.0.0.9000 |2016-06-09 |github (hadley/dtplyr)   |
|ggplot2        |   |2.1.0      |2016-03-01 |CRAN (R 3.3.0)           |
|knitr          |   |1.13       |2016-05-09 |CRAN (R 3.3.0)           |
|Lahman         |   |4.0-1      |2015-09-15 |CRAN (R 3.3.0)           |
|lazyeval       |   |0.1.10     |2015-01-02 |cran (@0.1.10)           |
|magrittr       |   |1.5        |2014-11-22 |CRAN (R 3.3.0)           |
|microbenchmark |   |1.4-2.1    |2015-11-25 |CRAN (R 3.3.0)           |
|nycflights13   |   |0.2.0      |2016-04-30 |CRAN (R 3.3.0)           |
|R6             |   |2.1.2      |2016-01-26 |CRAN (R 3.3.0)           |
|Rcpp           |   |0.12.5     |2016-05-14 |CRAN (R 3.3.0)           |
|rmarkdown      |   |0.9.6      |2016-05-01 |CRAN (R 3.3.0)           |
|RMySQL         |   |0.10.9     |2016-05-08 |CRAN (R 3.3.0)           |
|RPostgreSQL    |   |0.4-1      |2016-05-08 |CRAN (R 3.3.0)           |
|RSQLite        |   |1.0.0      |2014-10-25 |CRAN (R 3.3.0)           |
|testthat       |*  |1.0.2.9000 |2016-06-09 |github (hadley/testthat) |
|tibble         |   |1.0-5      |2016-06-09 |github (hadley/tibble)   |

# Check results
57 packages with problems

## aemo (0.1.0)
Maintainer: Imanuel Costigan <i.costigan@me.com>

1 error  | 0 warnings | 0 notes

```
checking whether package ‘aemo’ can be installed ... ERROR
Installation failed.
See ‘/Users/hadley/Documents/dplyr/dplyr/revdep/checks/aemo.Rcheck/00install.out’ for details.
```

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

## bigrquery (0.2.0)
Maintainer: Hadley Wickham <hadley@rstudio.com>

1 error  | 0 warnings | 0 notes

```
checking whether package ‘bigrquery’ can be installed ... ERROR
Installation failed.
See ‘/Users/hadley/Documents/dplyr/dplyr/revdep/checks/bigrquery.Rcheck/00install.out’ for details.
```

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
  3: as.expectation(exp, ..., srcref = srcref) at /private/tmp/Rtmp0KDkde/devtools812db49901f/hadley-testthat-d3e20b9/R/expectation.R:33
  4: identical(as.vector(object), TRUE) at /private/tmp/Rtmp0KDkde/devtools812db49901f/hadley-testthat-d3e20b9/R/expectation.R:112
  5: as.vector(object)
  6: augmented$disp
  7: `$.tbl_df`(augmented, disp)
  8: stop("Unknown column '", i, "'", call. = FALSE) at /private/tmp/Rtmp0KDkde/devtools812d2953cfa6/hadley-tibble-64175a8/R/tbl-df.r:47
  
  testthat results ================================================================
  OK: 490 SKIPPED: 0 FAILED: 1
  1. Error: rowwise tidiers can be applied to sub-models (@test-rowwise.R#21) 
  
  Error: testthat unit tests failed
  Execution halted
```

## chunked (0.2.1)
Maintainer: Edwin de Jonge <edwindjonge@gmail.com>  
Bug reports: https://github.com/edwindj/chunked/issues

1 error  | 0 warnings | 0 notes

```
checking tests ... ERROR
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  > 
  > test_check("chunked")
  1. Error: write_chunkwise to db works (@test-write.R#29) -----------------------
  attempt to apply non-function
  1: write_chunkwise(iris2, tmp, row.names = FALSE) at testthat/test-write.R:29
  2: write_chunkwise.tbl_sql(iris2, tmp, row.names = FALSE)
  
  testthat results ================================================================
  OK: 26 SKIPPED: 0 FAILED: 1
  1. Error: write_chunkwise to db works (@test-write.R#29) 
  
  Error: testthat unit tests failed
  Execution halted
```

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
make: *** [fileb7ba58fab1bb.o] Error 1
Execution halted
```

## denovolyzeR (0.1.0)
Maintainer: James Ware <j.ware@imperial.ac.uk>  
Bug reports: http://github.com/jamesware/denovolyzeR/issues

1 error  | 1 warning  | 1 note 

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

checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Quitting from lines 52-55 (denovolyzeR_intro.Rmd) 
Error: processing vignette 'denovolyzeR_intro.Rmd' failed with diagnostics:
Unknown column 'obs'
Execution halted


checking R code for possible problems ... NOTE
denovolyze: no visible global function definition for ‘ppois’
Undefined global functions or variables:
  ppois
Consider adding
  importFrom("stats", "ppois")
to your NAMESPACE file.
```

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
  55: lapply(x, lazy_eval, data = data)
  56: FUN(X[[i]], ...)
  57: eval(x$expr, data, x$env)
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
Error in eval(expr, envir, enclos) : could not find function "contains"
Calls: import_ride ... select_vars_ -> <Anonymous> -> lapply -> FUN -> eval -> eval
Execution halted

checking Rd cross-references ... WARNING
Missing link or links in documentation object 'pipe.Rd':
  ‘[dplyr]{chain}’

See section 'Cross-references' in the 'Writing R Extensions' manual.

```

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

## ezec (0.1.0)
Maintainer: Zhian N. Kamvar <kamvarz@science.oregonstate.edu>

0 errors | 1 warning  | 0 notes

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Warning in data("dummydata", library = "ezec") :
  data set 'ezec' not found
Quitting from lines 98-99 (getting_started.Rmd) 
Error: processing vignette 'getting_started.Rmd' failed with diagnostics:
Unknown column 'mod'
In addition: Warning messages:
1: Setting row names on a tibble is deprecated. 
2: Setting row names on a tibble is deprecated. 
3: Setting row names on a tibble is deprecated. 
Execution halted

```

## GenCAT (1.0.2)
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

## ggRandomForests (1.2.1)
Maintainer: John Ehrlinger <john.ehrlinger@gmail.com>  
Bug reports: https://github.com/ehrlinger/ggRandomForests/issues

2 errors | 0 warnings | 1 note 

```
checking examples ... ERROR
Running examples in ‘ggRandomForests-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: gg_rfsrc.rfsrc
> ### Title: Predicted response data object
> ### Aliases: gg_rfsrc gg_rfsrc.rfsrc
> 
> ### ** Examples
... 19 lines ...
> ##D gg_dta<- gg_rfsrc(rfsrc_airq)
> ##D 
> ##D plot(gg_dta)
> ## End(Not run)
> 
> ## -------- Boston data
> data(rfsrc_Boston, package="ggRandomForests")
> plot(rfsrc_Boston) 
Error in if (x$tree.err == FALSE) { : argument is of length zero
Calls: plot -> plot.rfsrc
Execution halted

checking tests ... ERROR
Running the tests in ‘tests/test-all.R’ failed.
Last 13 lines of output:
  6: sort(randomForestSRC::vimp(object)$importance, decreasing = TRUE)
  7: randomForestSRC::vimp(object)
  8: generic.predict.rfsrc(object, outcome.target = outcome.target, importance = importance, 
         importance.xvar = xvar.names, seed = seed, do.trace = do.trace, membership = FALSE, 
         subset = subset, ...)
  9: stop()
  
  testthat results ================================================================
  OK: 412 SKIPPED: 0 FAILED: 1
  1. Error: gg_vimp survival (@test_gg_vimp.R#139) 
  
  Error: testthat unit tests failed
  Execution halted

checking installed package size ... NOTE
  installed size is  5.9Mb
  sub-directories of 1Mb or more:
    data   2.6Mb
    doc    3.0Mb
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
  6: w_de$language
  7: `$.tbl_df`(w_de, language)
  8: stop("Unknown column '", i, "'", call. = FALSE) at /private/tmp/Rtmp0KDkde/devtools812d2953cfa6/hadley-tibble-64175a8/R/tbl-df.r:47
  
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

## IATscores (0.1-2)
Maintainer: Giulio Costantini <costantinigiulio@gmail.com>

1 error  | 0 warnings | 0 notes

```
checking package dependencies ... ERROR
Package required but not available: ‘nem’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```

## imager (0.20)
Maintainer: Simon Barthelme <simon.barthelme@gipsa-lab.fr>

1 error  | 0 warnings | 0 notes

```
checking whether package ‘imager’ can be installed ... ERROR
Installation failed.
See ‘/Users/hadley/Documents/dplyr/dplyr/revdep/checks/imager.Rcheck/00install.out’ for details.
```

## macleish (0.3.0)
Maintainer: Ben Baumer <ben.baumer@gmail.com>

0 errors | 1 warning  | 1 note 

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...

Attaching package: 'dplyr'

The following objects are masked from 'package:stats':

    filter, lag

... 8 lines ...
The following object is masked from 'package:base':

    date

Loading required package: clifro
Warning in library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE,  :
  there is no package called 'clifro'
Quitting from lines 187-190 (macleish.Rmd) 
Error: processing vignette 'macleish.Rmd' failed with diagnostics:
could not find function "windrose"
Execution halted

checking package dependencies ... NOTE
Package suggested but not available for checking: ‘clifro’
```

## MonetDBLite (0.3.0)
Maintainer: Hannes Muehleisen <hannes@cwi.nl>  
Bug reports: https://github.com/hannesmuehleisen/MonetDBLite/issues

1 error  | 0 warnings | 1 note 

```
checking tests ... ERROR
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  [1] "M"
  testthat results ================================================================
  OK: 194 SKIPPED: 0 FAILED: 7
  1. Error: dplyr tbl( sql() ) (@test_03_dplyr.R#35) 
  2. Error: dplyr filter() (@test_03_dplyr.R#51) 
  3. Error: dplyr arrange() (@test_03_dplyr.R#59) 
  4. Error: dplyr mutate() (@test_03_dplyr.R#67) 
  5. Error: dplyr summarise() (@test_03_dplyr.R#75) 
  6. Error: dplyr multiple objects (@test_03_dplyr.R#84) 
  7. Failure: dplyr group_by (@test_03_dplyr.R#120) 
  
  Error: testthat unit tests failed
  Execution halted

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
  Error: `dmtcd` not equal to lazyLoadDBfetch(c(0L, 1775L), datafile, compressed, envhook).
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

## networkreporting (0.1.0)
Maintainer: Dennis M. Feehan <feehan@berkeley.edu>

1 error  | 0 warnings | 0 notes

```
checking tests ... ERROR
Running the tests in ‘tests/test_all.R’ failed.
Last 13 lines of output:
  > test_check("networkreporting")
  Loading required package: networkreporting
  Taking N.F value implied by weights: 0
  
  Error in UseMethod("mutate_") : 
    no applicable method for 'mutate_' applied to an object of class "c('matrix', 'list')"
  Calls: test_check ... withVisible -> <Anonymous> -> <Anonymous> -> mutate_
  In addition: Warning message:
  In cbind(x[ids$x, , drop = FALSE], y[ids$y, y.cols, drop = FALSE]) :
    number of rows of result is not a multiple of vector length (arg 2)
  testthat results ================================================================
  OK: 0 SKIPPED: 0 FAILED: 0
  Execution halted
```

## photobiologyInOut (0.4.4)
Maintainer: Pedro J. Aphalo <pedro.aphalo@helsinki.fi>  
Bug reports: https://bitbucket.org/aphalo/photobiologyinout/

0 errors | 1 warning  | 0 notes

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Read 16 items
Read 4 items
Read 18 items
Read 4 items
Read 18 items
Read 4 items
Read 18 items
... 8 lines ...
Read 7 items
Read 5 items
Read 8 items
Read 8 items
Read 5 items
Read 8 items
Read 8 items
Quitting from lines 490-492 (user-guide.Rnw) 
Error: processing vignette 'user-guide.Rnw' failed with diagnostics:
could not find function "starts_with"
Execution halted
```

## pmc (1.0.1)
Maintainer: Carl Boettiger <cboettig@gmail.com>  
Bug reports: https://github.com/cboettig/pmc/issues

1 error  | 1 warning  | 0 notes

```
checking tests ... ERROR
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
         sQuote("tree"))
  
  Loading required package: ape
  testthat results ================================================================
  OK: 9 SKIPPED: 0 FAILED: 1
  1. Error: we can use ouch-based functions (@test_ouch.R#6) 
  
  Error: testthat unit tests failed
  In addition: Warning message:
  In treedata(geo$phy, geo$dat) :
    The following tips were not found in 'data' and were dropped from 'phy':
  	olivacea
  Execution halted

checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
	lambda
Warning in fitContinuous(phy = att$cache$phy, model = att$model, ..., ncores = 1) :
  Parameter estimates appear at bounds:
	lambda
Warning in fitContinuous(phy = att$cache$phy, model = att$model, ..., ncores = 1) :
  Parameter estimates appear at bounds:
	lambda
... 8 lines ...
  Parameter estimates appear at bounds:
	lambda
Warning in fitContinuous(phy = att$cache$phy, model = att$model, ..., ncores = 1) :
  Parameter estimates appear at bounds:
	lambda
`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
Quitting from lines 141-163 (pmc_tutorial.Rmd) 
Error: processing vignette 'pmc_tutorial.Rmd' failed with diagnostics:
'data' vector names (or data-frame row names) must match node names of 'tree'
Execution halted
```

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

## pRF (1.2)
Maintainer: Ankur Chakravarthy <ankur.chakravarthy.10@ucl.ac.uk>

1 error  | 0 warnings | 0 notes

```
checking package dependencies ... ERROR
Package required but not available: ‘multtest’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```

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

## RSQLServer (0.2.0)
Maintainer: Imanuel Costigan <i.costigan@me.com>  
Bug reports: https://github.com/imanuelcostigan/RSQLServer/issues

1 error  | 0 warnings | 0 notes

```
checking whether package ‘RSQLServer’ can be installed ... ERROR
Installation failed.
See ‘/Users/hadley/Documents/dplyr/dplyr/revdep/checks/RSQLServer.Rcheck/00install.out’ for details.
```

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

## texmexseq (0.2)
Maintainer: Scott Olesen <swo@mit.edu>

0 errors | 1 warning  | 0 notes

```
checking whether package ‘texmexseq’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import ‘dplyr::matches’ by ‘testthat::matches’ when loading ‘texmexseq’
See ‘/Users/hadley/Documents/dplyr/dplyr/revdep/checks/texmexseq.Rcheck/00install.out’ for details.
```

## tidyr (0.4.1)
Maintainer: Hadley Wickham <hadley@rstudio.com>  
Bug reports: https://github.com/hadley/tidyr/issues

1 error  | 0 warnings | 1 note 

```
checking tests ... ERROR
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  error$message does not match "incompatible type".
  Actual value: "Can not automatically convert from numeric to character."
  
  
  testthat results ================================================================
  OK: 131 SKIPPED: 0 FAILED: 4
  1. Error: missings filled down for each atomic vector (@test-fill.R#42) 
  2. Error: missings filled up for each vector (@test-fill.R#59) 
  3. Error: preserve class of input (@test-spread.R#82) 
  4. Failure: elements must all be of same type (@test-unnest.R#18) 
  
  Error: testthat unit tests failed
  Execution halted

checking dependencies in R code ... NOTE
Missing or unexported object: ‘dplyr::tbl_dt’
```

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
  Unknown column 'val'
  1: cast_sparse(dat, a, b, val) at testthat/test-sparse-casters.R:20
  2: cast_sparse_(data, col_name(substitute(row)), col_name(substitute(column)), value_col)
  3: data[[value_col]]
  4: `[[.tbl_df`(data, value_col)
  5: stop("Unknown column '", colname, "'", call. = FALSE) at /private/tmp/Rtmp0KDkde/devtools812d2953cfa6/hadley-tibble-64175a8/R/tbl-df.r:35
  
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

## wordbankr (0.1)
Maintainer: Mika Braginsky <mika.br@gmail.com>  
Bug reports: http://github.com/langcog/wordbankr/issues

0 errors | 1 warning  | 0 notes

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Auto-disconnecting mysql connection (0, 1)
Joining, by = "momed_id"
Auto-disconnecting mysql connection (0, 0)
Joining, by = "momed_id"
Auto-disconnecting mysql connection (0, 2)
Auto-disconnecting mysql connection (0, 4)
Auto-disconnecting mysql connection (0, 3)
Auto-disconnecting mysql connection (0, 5)
Quitting from lines 43-47 (wordbankr.Rmd) 
Error: processing vignette 'wordbankr.Rmd' failed with diagnostics:
Elements 1, 2 of items %in% instrument_table$select are not true
Execution halted

```

