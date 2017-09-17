## Release summary

This is a minor bugfix release in response to CRAN's request. This is the third release attempt (with the same version), following up test failures with reverse dependencies from CRAN's incoming checks.

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
