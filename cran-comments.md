## Release summary

Minor update to address CRAN warnings and conflict for exported symbols.  Now also with rchk errors fixed.

## Test environments

* local OS X install, R 3.5.0
* ubuntu 12.04 (on travis-ci), R 3.5.0, R-oldrel, R-devel.
* win-builder (devel)

## R CMD check results

0 ERRORs | 0 WARNINGs | 1 NOTE

* checking installed package size ... NOTE

  This is all compiled code in the libs/ directory.

## Reverse dependencies

We checked more than 900 reverse dependencies by running R CMD check twice, once with the CRAN version installed, and once with this version installed. We did not see any new problems.
