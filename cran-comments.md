## Test environments

* local OS X install, R 3.2.0
* ubuntu 12.04 (on travis-ci), R 3.2.0
* win-builder (devel and release)

## R CMD check results

There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* checking installed package size ... NOTE. 

  This is all compiled code in the libs/ directory.

This release contains a number of fixes for problems spotted by UBSAN and valgrind. Some problems are still flagged, but to the best of our knowledge are coming from Rcpp. We have submitted patches upstream and they will be fixed in the next release of Rcpp.

## Downstream dependencies

I ran `R CMD check` on all 43 reverse dependencies (https://github.com/hadley/dplyr/tree/master/revdep/summary.md). 
