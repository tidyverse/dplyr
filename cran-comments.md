## Release summary

Bug fixes, minor changes, now importing the tidyselect package.

Double-checked with UBSAN, unable to replicate errors seen on CRAN.

## Test environments

* local OS X install, R 3.4.4
* ubuntu 12.04 (on travis-ci), R 3.4.4, R-oldrel, R-devel.
* win-builder (devel)

## R CMD check results

0 ERRORs | 0 WARNINGs | 1 NOTE

* checking installed package size ... NOTE

  This is all compiled code in the libs/ directory.

## Reverse dependencies

We checked more than 900 reverse dependencies by running R CMD check twice, once with the CRAN version installed, and once with this version installed. We saw new problems. We failed to check about 50 packages. Issues are summarised below.

### New problems

* amt: Package invokes operation that was always broken but now gives an error. Notified author, an update has been submitted to CRAN.
* bioset, replyr: Defines an `n()` function which is now called, instead of doing hybrid evaluation: https://github.com/randomchars42/bioset/issues/1, https://github.com/WinVector/replyr/issues/10. replyr has been updated on CRAN.
* desctable: Already fixed in devel, https://github.com/MaximeWack/desctable/issues/8.
* ddpcr: Calling `select()` with `NA` in a column name, https://github.com/daattali/ddpcr/issues/22.
* fold, PPforest: Unknown reason, reported to maintainer.
* keyholder, ruler: Unknown reason, reported to maintainer: https://github.com/echasnovski/keyholder/issues/3.
* purrr: spurious test failure, sent patch: https://github.com/tidyverse/purrr/pull/494
