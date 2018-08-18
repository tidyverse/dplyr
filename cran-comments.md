## Release summary

Fix for NEWS.md file.

The remaining rchk errors are most likely false positives from Rcpp, see Rcpp issue https://github.com/RcppCore/Rcpp/issues/892 .

## Test environments

* local OS X install, R 3.5.1
* ubuntu 12.04 (on travis-ci), R 3.5.1, R-oldrel, R-devel.
* win-builder (devel)

## R CMD check results

0 ERRORs | 0 WARNINGs | 1 NOTE

* checking installed package size ... NOTE

  This is all compiled code in the libs/ directory.

## Reverse dependencies

Reverse dependencies not checked, because we only changed NEWS.md (and the file that contains the submission notes): https://github.com/tidyverse/dplyr/compare/v0.7.6...r-0.7.7 .
