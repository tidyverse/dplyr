## Release summary

This is a minor bugfix release in response to errors with ASAN, UBSAN, and Fedora+clang builds on CRAN.  We now were able to finally replicate the problems we were seeing on CRAN in a Docker container, our minimal fix resolves these problems.

## Test environments

* local OS X install, R 3.4.1
* ubuntu 12.04 (on travis-ci), R 3.4.1, R-oldrel, R-devel.
* win-builder (devel)

## R CMD check results

0 ERRORs | 0 WARNINGs | 2 NOTEs

* 6 days since last update

  The UB errors seem important to fix now.

* checking installed package size ... NOTE

  This is all compiled code in the libs/ directory.

## Reverse dependencies

We checked 795 reverse dependencies (721 from CRAN + 74 from BioConductor) by running R CMD check twice, once with the CRAN version installed, and once with this version installed. We saw 3 new problems. We failed to check 4 packages. Issues are summarised below.

### New problems

Most likely web API problems:

* censusr
* hansard
* roadoi

### Failed to check

* flowWorkspace (failed to install)
* loon          (failed to install)
* naniar        (check timed out)
* visdat        (check timed out)
