# Setup

## Platform

|setting  |value                        |
|:--------|:----------------------------|
|version  |R version 3.1.2 (2014-10-31) |
|system   |x86_64, darwin13.4.0         |
|ui       |RStudio (0.99.90)            |
|language |(EN)                         |
|collate  |en_US.UTF-8                  |
|tz       |America/Chicago              |

## Packages

|package        |*  |version  |date       |source         |
|:--------------|:--|:--------|:----------|:--------------|
|assertthat     |*  |0.1      |2013-12-06 |CRAN (R 3.1.0) |
|BH             |*  |1.54.0-5 |2014-11-10 |CRAN (R 3.1.2) |
|data.table     |*  |1.9.4    |2014-10-02 |CRAN (R 3.1.1) |
|DBI            |*  |0.3.1    |2014-09-24 |CRAN (R 3.1.1) |
|ggplot2        |*  |1.0.0    |2014-05-21 |CRAN (R 3.1.0) |
|knitr          |*  |1.8      |2014-11-11 |CRAN (R 3.1.2) |
|Lahman         |*  |3.0-1    |2014-09-13 |CRAN (R 3.1.1) |
|lazyeval       |*  |0.1.9    |2014-10-01 |CRAN (R 3.1.1) |
|magrittr       |*  |1.5      |2014-11-22 |CRAN (R 3.1.2) |
|mgcv           |*  |1.8-4    |2014-11-27 |CRAN (R 3.1.2) |
|microbenchmark |*  |1.4-2    |2014-09-28 |CRAN (R 3.1.1) |
|R6             |*  |2.0.1    |2014-10-29 |CRAN (R 3.1.2) |
|Rcpp           |*  |0.11.3   |2014-09-29 |CRAN (R 3.1.1) |
|RSQLite        |*  |1.0.0    |2014-10-25 |CRAN (R 3.1.2) |
|testthat       |   |0.9.1    |2014-10-01 |CRAN (R 3.1.1) |

# Check results
38 checked out of 38 dependencies 

## aemo (0.1.0)
Maintainer: Imanuel Costigan <i.costigan@me.com>

__OK__

## broom (0.3.4)
Maintainer: David Robinson <admiral.david@gmail.com>  
Bug reports: http://github.com/dgrtwo/broom/issues

```
checking examples ... ERROR
Running examples in ‘broom-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: felm_tidiers
> ### Title: Tidying methods for models with multiple group fixed effects
> ### Aliases: augment.felm felm_tidiers glance.felm tidy.felm
> 
> ### ** Examples
> 
> if (require("lfe", quietly = TRUE)) {
+     N=1e2
+     DT <- data.frame(
+       id = sample(5, N, TRUE),
+       v1 =  sample(5, N, TRUE),
+       v2 =  sample(1e6, N, TRUE),
+       v3 =  sample(round(runif(100,max=100),4), N, TRUE),
+       v4 =  sample(round(runif(100,max=100),4), N, TRUE)
+     )
+ 
+     result_felm <- felm(v2~v3, DT)
+     tidy(result_felm)
+     augment(result_felm)
+     result_felm <- felm(v2~v3|id+v1, DT)
+     tidy(result_felm, fe = TRUE)
+     augment(result_felm)
+     v1<-DT$v1
+     v2 <- DT$v2
+     v3 <- DT$v3
+     id <- DT$id
+     result_felm <- felm(v2~v3|id+v1)
+     tidy(result_felm)
+     augment(result_felm)
+     glance(result_felm)
+ }
Warning in FUN(newX[, i], ...) : non-factor id coerced to factor
Warning in FUN(newX[, i], ...) : non-factor v1 coerced to factor
Error: data_frames can not contain data.frames, matrices or arrays
Execution halted
```

## choroplethr (2.1.0)
Maintainer: Ari Lamstein <arilamstein@gmail.com>  
Bug reports: https://github.com/trulia/choroplethr/issues

__OK__

## COPASutils (0.1.4)
Maintainer: Erik Andersen <erik.andersen@northwestern.edu>

```
checking running R code from vignettes ... [23s/23s] ERROR
Errors in running code in vignettes:
when running code in ‘Overview.Rmd’
  ...
> dosesRaw <- doseData

> strains <- rep(c("Strain 1", "Strain 2", "Strain 3", 
+     "Strain 4"), each = 6, times = 4)

> dosesSummarized <- summarizePlate(dosesRaw, strains)

  When sourcing ‘Overview.R’:
Error: loss of precision when attempting to convert a numeric to an integer
Execution halted

```

## DataCombine (0.2.7.1)
Maintainer: Christopher Gandrud <christopher.gandrud@gmail.com>  
Bug reports: https://github.com/christophergandrud/DataCombine/issues

__OK__

## DepthProc (1.0.3)
Maintainer: Zygmunt Zawadzki <zawadzkizygmunt@gmail.com>

```
checking whether package ‘DepthProc’ can be installed ... [41s/42s] ERROR
Installation failed.
See ‘/private/tmp/Rtmpkx3JxP/check_crand7df75a96fc5/DepthProc.Rcheck/00install.out’ for details.
```

## ecoengine (1.6)
Maintainer: Karthik Ram <karthik.ram@gmail.com>  
Bug reports: https://github.com/ropensci/ecoengine/issues

__OK__

## freqweights (1.0.1)
Maintainer: Emilio Torres-Manzanera <torres@uniovi.es>

__OK__

## fueleconomy (0.1)
Maintainer: 'Hadley Wickham' <h.wickham@gmail.com>

__OK__

## gender (0.4.2)
Maintainer: Lincoln Mullen <lincoln@lincolnmullen.com>  
Bug reports: https://github.com/ropensci/gender/issues

__OK__

## ggRandomForests (1.1.0)
Maintainer: John Ehrlinger <john.ehrlinger@gmail.com>

__OK__

## ggvis (0.4)
Maintainer: Winston Chang <winston@rstudio.com>

```
checking examples ... ERROR
Running examples in ‘ggvis-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: layer_boxplots
> ### Title: Display data with a boxplot.
> ### Aliases: layer_boxplots
> 
> ### ** Examples
> 
> library(dplyr)

Attaching package: ‘dplyr’

The following object is masked from ‘package:stats’:

    filter

The following objects are masked from ‘package:base’:

    intersect, setdiff, setequal, union

> 
> mtcars %>% ggvis(~factor(cyl), ~mpg) %>% layer_boxplots()
Error in factor(cyl) : object 'cyl' not found
Calls: %>% ... prop_value -> prop_value.prop_variable -> eval -> eval -> factor
Execution halted
```

## mosaic (0.9.1-3)
Maintainer: Randall Pruim <rpruim@calvin.edu>

```
checking package dependencies ... NOTE
Package which this enhances but not available for checking: ‘manipulate’
```
```
checking installed package size ... NOTE
  installed size is  9.0Mb
  sub-directories of 1Mb or more:
    R     1.8Mb
    doc   6.7Mb
```

## nullabor (0.3.0)
Maintainer: Di Cook <dicook@iastate.edu>

__OK__

## nycflights13 (0.1)
Maintainer: 'Hadley Wickham' <h.wickham@gmail.com>

```
checking installed package size ... NOTE
  installed size is  5.6Mb
  sub-directories of 1Mb or more:
    data   5.5Mb
```

## peptider (0.1.6)
Maintainer: Eric Hare <erichare@iastate.edu>

__OK__

## pitchRx (1.6)
Maintainer: Carson Sievert <sievert@iastate.edu>  
Bug reports: http://github.com/cpsievert/pitchRx/issues

__OK__

## pollstR (1.1.1)
Maintainer: Jeffrey B. Arnold <jeffrey.arnold@gmail.com>  
Bug reports: https://github.com/rOpenGov/pollstR/issues

__OK__

## PopED (0.1.2)
Maintainer: Andrew C. Hooker <andrew.hooker@farmbio.uu.se>

```
checking Rd cross-references ... NOTE
Package unavailable to check Rd xrefs: ‘matlab’
```

## poplite (0.99.5)
Maintainer: Daniel Bottomly <bottomly@ohsu.edu>

__OK__

## qdap (2.2.0)
Maintainer: Tyler Rinker <tyler.rinker@gmail.com>  
Bug reports: http://github.com/trinker/qdap/issues

```
checking whether package ‘qdap’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/Rtmpkx3JxP/check_crand7df75a96fc5/qdap.Rcheck/00install.out’ for details.
```

## rattle (3.3.0)
Maintainer: Graham Williams <Graham.Williams@togaware.com>

```
checking package dependencies ... NOTE
Packages suggested but not available for checking:
  ‘RGtk2’ ‘cairoDevice’ ‘gWidgetsRGtk2’ ‘latticist’ ‘playwith’ ‘rggobi’
  ‘RGtk2Extras’ ‘RODBC’ ‘pkgDepTools’
```
```
checking installed package size ... NOTE
  installed size is  6.8Mb
  sub-directories of 1Mb or more:
    data   2.0Mb
    etc    2.5Mb
    po     1.2Mb
```
```
checking dependencies in R code ... NOTE
Unavailable namespace imported from by a ':::' call: ‘pkgDepTools’
  See the note in ?`:::` about the use of this operator.
See the information on DESCRIPTION files in the chapter ‘Creating R
packages’ of the ‘Writing R Extensions’ manual.
```
```
checking R code for possible problems ... NOTE
Found an obsolete/platform-specific call in the following functions:
  ‘openMyDevice’ ‘printPlot’ ‘savePlotToFile’
Found the platform-specific devices:
  ‘win.metafile’ ‘win.print’
dev.new() is the preferred way to open a new device, in the unlikely
event one is needed.
```

## rbison (0.4.3)
Maintainer: Scott Chamberlain <myrmecocystus@gmail.com>  
Bug reports: https://github.com/ropensci/rbison/issues

```
checking tests ... ERROR
Running the tests in ‘tests/test-all.R’ failed.
Last 13 lines of output:
  bison_datause() for data use and bison_citation() for how to cite data from BISON
  Use suppressPackageStartupMessages() to suppress these startup messages in the future
  
  bison : .............
  bison_solr : ........
  bison_tax : ........
  bisonmap : 12..
  is.bison : ....
  Error in vapply(failures, "[[", "failure_msg", FUN.VALUE = character(1)) : 
    values must be length 1,
   but FUN(X[[2]]) result is length 2
  Calls: test_check ... <Anonymous> -> .oapply -> eval -> eval -> <Anonymous> -> vapply
  Execution halted
```

## RCMIP5 (1.1)
Maintainer: Kathe Todd-Brown <ktoddbrown@gmail.com>

__OK__

## rcrossref (0.2.0)
Maintainer: Scott Chamberlain <myrmecocystus@gmail.com>  
Bug reports: https://github.com/ropensci/rcrossref/issues

__OK__

## repra (0.4.2)
Maintainer: Eduardo Ibanez <eduardo.ibanez@nrel.gov>  
Bug reports: https://github.com/NREL/repra/issues

__OK__

## rex (0.2.0)
Maintainer: Jim Hester <james.f.hester@gmail.com>  
Bug reports: https://github.com/kevinushey/rex/issues

__OK__

## rplexos (0.10.0.1)
Maintainer: Eduardo Ibanez <eduardo.ibanez@nrel.gov>  
Bug reports: https://github.com/NREL/rplexos/issues

```
checking R code for possible problems ... NOTE
log_steps: possible error in stri_split_regex(steps2, " step| of |time:
  |elapsed: ", n_max = 5, opts_regex = caseins): unused argument (n_max
  = 5)
```

## rPref (0.3)
Maintainer: Patrick Roocks <mail@p-roocks.de>

__OK__

## saeSim (0.6.0)
Maintainer: Sebastian Warnholz <Sebastian.Warnholz@fu-berlin.de>  
Bug reports: https://github.com/wahani/saeSim/issues

__OK__

## SciencesPo (0.11.21)
Maintainer: Daniel Marcelino <dmarcelino@live.com>  
Bug reports: http://github.com/danielmarcelino/SciencesPo

__OK__

## simPH (1.2.4)
Maintainer: Christopher Gandrud <christopher.gandrud@gmail.com>  
Bug reports: https://github.com/christophergandrud/simPH/issues

__OK__

## statar (0.1.2)
Maintainer: Matthieu Gomez <mattg@princeton.edu>  
Bug reports: https://github.com/matthieugomez/statar/issues

```
checking examples ... ERROR
Running examples in ‘statar-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: duplicates
> ### Title: returns duplicated rows
> ### Aliases: duplicates duplicates_
> 
> ### ** Examples
> 
> library(data.table)
> DT <- data.table(a = rep(1:2, each = 3), b = 1:6)
> duplicates(DT, by = "a")
2 groups have duplicates
   N a b
1: 3 1 1
2: 3 1 2
3: 3 1 3
4: 3 2 4
5: 3 2 5
6: 3 2 6
> duplicates(DT, by = list(a,b))
Error: All select() inputs must resolve to integer column positions.
The following do not:
```

## TH.data (1.0-5)
Maintainer: Torsten Hothorn <Torsten.Hothorn@R-project.org>

```
checking installed package size ... NOTE
  installed size is  5.2Mb
  sub-directories of 1Mb or more:
    data   1.1Mb
    rda    3.8Mb
```

## tidyr (0.2.0)
Maintainer: Hadley Wickham <hadley@rstudio.com>  
Bug reports: https://github.com/hadley/tidyr/issues

__OK__

## turfR (0.8-7)
Maintainer: Jack Horne <jack@jackhorne.net>

__OK__

## USAboundaries (0.1)
Maintainer: Lincoln Mullen <lincoln@lincolnmullen.com>  
Bug reports: https://github.com/ropensci/USAboundaries/issues

__OK__

## vdmR (0.1.0)
Maintainer: Tomokazu Fujino <fujino@fwu.ac.jp>

__OK__

