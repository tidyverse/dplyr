## Release summary

This submission includes a number of fixes to C/C++ bugs causing various problems detected by valgrind and rchk, and an C++ problem only detected by Mac CRAN.

## Test environments

* local OS X install, R 3.4.0
* ubuntu 12.04 (on travis-ci), R 3.4.0, R 3.2.5, R-devel.
* win-builder (devel)

## R CMD check results

0 ERRORs | 0 WARNINGs | 1 NOTEs

* checking installed package size ... NOTE

  This is all compiled code in the libs/ directory.
    

## Reverse dependencies

As no changes affected behaviour (except to fix crashes!) we only casually checked the revdeps. However, we still ran R CMD check on all reverse dependencies, and did not see any new problems compared to the CRAN release.
