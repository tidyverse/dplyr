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

I ran `R CMD check` on all 43 reverse dependencies (https://github.com/hadley/dplyr/tree/master/revdep/summary.md). As far as I can tell, there are two failures caused by changes to dplyr:

* broom: I informed the maintainer a month ago; the bug is fixed and they are 
  planning a CRAN release in the next couple of days.
  (https://github.com/dgrtwo/broom/issues/18)

* statar: I informed the maintainer of the problem almost a month ago,
  and have heard nothing back (https://github.com/matthieugomez/statar/issues/1)

(Both failures are due to improve error checking in dplyr: the fact that the code worked previously is the bug.)
