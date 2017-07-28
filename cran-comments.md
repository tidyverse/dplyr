## Release summary

This submission moves run-time vs build-time checks out of .onLoad and into a new function that can be run on demand (`dr_dplyr()`)

## Test environments

* local OS X install, R 3.4.0
* ubuntu 12.04 (on travis-ci), R 3.4.0, R 3.2.5, R-devel.
* win-builder (devel)

## R CMD check results

0 ERRORs | 0 WARNINGs | 1 NOTEs

* checking installed package size ... NOTE

  This is all compiled code in the libs/ directory.

## Reverse dependencies

As no changes affected behaviour I did not re-run the revdep checks.
