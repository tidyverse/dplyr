## Release summary

This is a minor release. 

## Test environments

* local mac OS install, R 4.0.3
* ubuntu 16.04 (on github actions), R-devel, R 4.0.3, R 3.6.3, 3.5.3, R 3.4.4, R 3.3.3
* mac OS 10.15.4 (on github actions) R-devel, R 3.6.0
* Microsoft Windows Server 2019 10.0.17763 (on github actions) R 4.0.3
* win-builder (devel)

## R CMD check results

0 errors v | 0 warnings v | 0 notes v

R CMD check succeeded

## revdepcheck results

We checked 2426 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 7 new problems
 * We failed to check 54 packages

Newly failing R packages are listed below. 

### New problems

* disk.frame: S3 generic/method inconsistency, as count() and tally() are now generics
* gtfs2gps: unrelated
* lplyr: Needs to define a mutate.list() methods because mutate.default() is no longer defined
* mosaicCore: pull request https://github.com/ProjectMOSAIC/mosaicCore/pull/37
* tcR: S3 generic/method inconsistency, as count() is now generic
* TextMiningGUI: unrelated tcl/tk issue
* timetk: unrelated
