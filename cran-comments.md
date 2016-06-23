## Test environments

* local OS X install, R 3.3.1
* ubuntu 12.04 (on travis-ci), R 3.3.1, R 3.2.5, R-devel.
* win-builder (devel)

## R CMD check results

0 ERRORs | 1 WARNING | 4 NOTEs

* checking installed package size ... NOTE. 

  This is all compiled code in the libs/ directory.

* checking CRAN incoming feasibility ... NOTE
  This package is MIT licensed.

* checking package dependencies ... NOTE
  Package suggested but not available for checking: 'dtplyr'

  Suggests or Enhances not in mainstream repositories: dtplyr 
  checking Rd cross-references ... WARNING
   
  I'll submit this once dplyr has been accepted (unless you'd prefer a 
  parallel submission)

* checking dependencies in R code ... NOTE
  Missing or unexported object: 'RSQLite::rsqliteVersion' 
  
  This is used for compatiblity with both current and next version of 
  RSQLite.

## Downstream dependencies

I ran `R CMD check` on all 297 reverse dependencies (https://github.com/hadley/dplyr/tree/master/revdep/). There are unfortunately a number of new failures described below. There were two sets of failures caused by deliberate changes to the API:

* select() previously used NSE and "virtual" functions; now it uses real functions:
  
  * alakazam: checking re-building of vignette outputs ... WARNING
  * assertr: checking examples ... ERROR
  * condformat: checking examples ... ERROR
  * ddpcr: checking examples ... ERROR
  * easyformatr: checking examples ... ERROR
  * mtconnectR: checking examples ... ERROR
  * statar: checking examples ... ERROR

* $.tbl_df is now stricter, throwing an error if the column does not exist. 
  This causes some code that previously failed silently to now throw an error:
  
  * broom: checking examples ... ERROR
  * ggmcmc: checking examples ... ERROR
  * gutenbergr: checking examples ... ERROR
  * rplexos: checking examples ... ERROR

bigrquery fails - I have a 0.5 compatible version ready for submission once dplyr is through (unfortunately it was too difficult to make a package that worked with both dplyr 0.4 and 0.5.)

There were also a bunch of errors that don't seem related to dplyr (as far as I can tell)

* chunked: checking tests ... ERROR

* datastepr: checking re-building of vignette outputs ... WARNING

* DeLorean: checking re-building of vignette outputs ... WARNING
  This looks like some C++ compilation problem

* describer: checking tests ... ERROR
  Looks like automatic code linting error

* dotwhisker: checking examples ... ERROR

* edeaR: checking re-building of vignette outputs ... WARNING
  Stricter coercion check when rbinding

* elpatron: checking examples ... ERROR

* ggspectra: checking examples ... ERROR

* ggvis: checking tests ... ERROR
  Failing on CRAN (we're still working on a fix)

* haven: checking examples ... ERROR
  Failing on CRAN (have fix in progress)

* modellingTools: checking examples ... ERROR
  Now throw error if you try and create a tibble with multiple columns
  that have the same name.

* photobiology: checking examples ... ERROR

* photobiologyInOut: checking re-building of vignette outputs ... WARNING

* RCMIP5: checking tests ... ERROR
  Numerical failure in tests, seems unrelated to dplyr.

* resumer: checking tests ... ERROR

* RNeXML: checking examples ... ERROR

* SpaDES: checking re-building of vignette outputs ... WARNING
  ???
  
* sprintfr: checking examples ... ERROR
  Fails to import data_frame function

* tidytext: checking examples ... ERROR

* treeplyr: checking examples ... WARNING
  Uses deprecated function from tibble package

* useful: checking tests ... ERROR

* vcfR: checking examples ... ERROR

Finally, there were a few failures because of install problems:

* Failed to install dependencies for: biomartr, HydeNet, IATscores, myTAI, pRF

* Failed to install: aemo, bigrquery, imager, morse, RSQLServer, texmexseq

* GenCAT: checking examples ... ERROR
  Doesn't correctly check for non-installed suggested package

* glycanr: checking examples ... ERROR
  Doesn't correctly check for non-installed suggested package

* poplite: checking re-building of vignette outputs ... WARNING
  Doesn't correctly check for non-installed suggested package

* tigger: checking examples ... ERROR
  Doesn't correctly check for non-installed suggested package


Authors were notified on June 9, June 14, and again today.
