## Release summary

This is a minor bugfix release in response to CRAN's request. This is the second release attempt (with the same version), following up test failures with reverse dependencies.

## Test environments

* local OS X install, R 3.4.1
* ubuntu 12.04 (on travis-ci), R 3.4.1, R-oldrel, R-devel.
* win-builder (devel)

## R CMD check results

0 ERRORs | 0 WARNINGs | 1 NOTEs

* checking installed package size ... NOTE

  This is all compiled code in the libs/ directory.

## ASAN errors on CRAN

We could not replicate the ASAN errors on Fedora with clang.  We changed code that we believe may be responsible for the problem, and we'll watch the check results once the package is released.

## Reverse dependencies

We checked 779 reverse dependencies (702 from CRAN + 77 from BioConductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 2 packages

Issues with CRAN packages are summarised below.

### Failed to check

* loon        (failed to install)
* MonetDBLite (check timed out)
