## Release summary

This is a minor release that adress new findings since the major release 1.0.0 and also 
is forward compatible with the next version of vctrs. 

## Test environments

* local mac OS install, R 4.0.0
* ubuntu 16.04 (on github actions), R-devel, R 4.0.0, R 3.6.3, 3.5.3, R 3.4.4, R 3.3.3
* mac OS 10.15.4 (on github actions) R-devel, R 3.6.0
* Microsoft Windows Server 2019 10.0.17763 (on github actions) R 4.0.0
* win-builder (devel)

## R CMD check results

0 errors v | 0 warnings v | 0 notes v

R CMD check succeeded

Newly failing R packages are listed below. 

## revdepcheck results

We checked 2132 reverse dependencies (2111 from CRAN + 21 from BioConductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 2 new problems
 * We failed to check 129 packages

Issues with CRAN packages are summarised below.

### New problems
(This reports the first line of each new failure)

* jstor: We could not reproduce locally
* ralger: A pull request has been prepared https://github.com/feddelegrand7/ralger/pull/3
