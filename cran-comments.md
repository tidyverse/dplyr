## Release summary

This release add a few minor features, but is mostly concerned with fixing crashing bugs and memory errors. (It also fixes the new NOTEs about functions imported from "base" packages).

## Test environments

* local OS X install, R 3.2.2
* ubuntu 12.04 (on travis-ci), R 3.2.2
* win-builder (devel and release)

## R CMD check results

There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* checking installed package size ... NOTE. 

  This is all compiled code in the libs/ directory.

## Downstream dependencies

I ran `R CMD check` on all 119 reverse dependencies (https://github.com/hadley/dplyr/tree/master/revdep/summary.md). 
I did not find any errors related to dplyr changes.
