## Release summary

Compatibility release. Fixes problems on the package check page (including the compiler warning on some platforms), and a regression introduced in dplyr 0.7.5.

## Test environments

* local OS X install, R 3.5.1
* ubuntu 12.04 (on travis-ci), R 3.5.1, R-oldrel, R-devel.
* win-builder (devel)

## R CMD check results

0 ERRORs | 0 WARNINGs | 1 NOTE

* checking installed package size ... NOTE

  This is all compiled code in the libs/ directory.

## Reverse dependencies

Checked reverse dependencies on CRAN (could not check 81 out of >1200), problems found:

- banr, classyfireR, codemetar, fingertipsR, rdefra: failing access to a web API, successful when rechechking
- Nmisc 0.3.4: brittle checks when non-CRAN version of dplyr is installed, Nmisc maintainer states that the problem is fixed in Nmisc 0.3.5 which is on CRAN now: https://github.com/numeract/Nmisc/issues/9
