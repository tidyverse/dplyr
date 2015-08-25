## Release summary

This is a resubmission:

* To use a canoncial package URL
* To reduce example run time to less than 5s for two examples

---

This is a resubmission to fix a broken url.

---

This release adds one minor feature, but is mostly concerned with fixing crashing bugs, memory errors and R CMD checks problem.  I'm pretty sure we caught all the valgrind and UBSAN problems. Some problems are still flagged, but to the best of our knowledge are coming from Rcpp. We have submitted patches upstream and they will be fixed in the next release of Rcpp.

## Test environments

* local OS X install, R 3.2.0
* ubuntu 12.04 (on travis-ci), R 3.2.0
* win-builder (devel and release)

## R CMD check results

There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* checking installed package size ... NOTE. 

  This is all compiled code in the libs/ directory.

## Downstream dependencies

I ran `R CMD check` on all 85 reverse dependencies (https://github.com/hadley/dplyr/tree/master/revdep/summary.md). 
I did not find any errors related to dplyr changes.
