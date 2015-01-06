Resubmission: compared to the previous submission this should fix the problem reported by BDR.
---

## Test environments

* local OS X install, R 3.1.2
* ubuntu 12.04 (on travis-ci), R 3.1.2
* win-builder (devel and release)

## R CMD check results

There were no ERRORs or WARNINGs.

There was 1 NOTEs:

* checking dependencies in R code ... NOTE
  
  Namespaces in Imports field not imported from: 'R6'
  R6 is a build time dependency.

## Downstream dependencies

I ran `R CMD check` on all 43 reverse dependencies (https://github.com/hadley/dplyr/tree/master/revdep/summary.md). As far as I can tell, there is only one failure caused by changes to dplyr:

* statar: I informed the maintainer of the problem almost a month ago,
  and have heard nothing back (https://github.com/matthieugomez/statar/issues/1)
  
  This failure is related to improved error checking in dplyr: the fact that 
  the code worked previously is the bug.

* (broom: this has been fixed by a recent submission)
