## Test environments

* local OS X install, R 3.4.0
* ubuntu 12.04 (on travis-ci), R 3.4.0, R 3.2.5, R-devel.
* win-builder (devel)

## R CMD check results

0 ERRORs | 1 WARNINGs | 1 NOTEs

* checking Rd cross-references ... WARNING
  Unknown package 'dbplyr' in Rd xrefs

* Suggests or Enhances not in mainstream repositories:  dbplyr

  dplyr suggests dbplyr which is not yet on CRAN. Unless you'd prefer 
  otherwise, I will submit once dplyr is on CRAN.

## Reverse dependencies

We ran R CMD check on all 588 reverse dependencies. We see 80 problems. The majority of maintainers have been informed of problems twice, on April 18 and May 4. Unfortunately there were a few packages added to CRAN between May 4 and today. We informed those maintainers on May 17, including suggested fixes so they could be remedied as quickly as possible.

Summary of problems below. We have reviewed all problems and taken remedial action where possible.

* tatoo: ?

* bayesplot: During vignette building, not reproducible but likely unrelated to dplyr changes

* openair, vdmR: segfaults for unknown reasons

* assertr: Related to changes in nonstandard evaluation

* broom: Access of deprecated function `lahman_df()`

* carpenter, geoSpectral, valr: fail with "Variable context not set" due
  to a deliberate API change.

* chunked: Access of function `sql_render()` now in the dbplyr package

* ddcpr: Error building vignette

* DeLorean, incadata, rmcfs: `filter()` is now an S3 method, the package 
  defines functions named `filter.xxx()`

* emil, lplyr: `select()` is now an S3 method, the package implicitly calls 
  `select.list()`

* etl, infuser, MonetDBLite, RPresto, sqlscore: Bad documentation link to/usage 
  of function `src_sql()` or `src_desc()` or `build_sql()` now in dbplyr. 
  These maintainers are waiting for dbplyr release which will be submitted
  once dplyr is accepted.

* eyetrackingR: Tests running too long

* ggfortify: fails stricter behaviour of `dplyr::lag()` which now errors 
  if it looks like you wanted `stats::lag()`.

* highcharter: Invalid use of dplyr API

* metaplot: ?

* parlitools: Vignette building error, ?

* Momocs: `transmute()` is now an S3 generic, 
  https://github.com/vbonhomme/Momocs/issues/178

* myTAI: ?

* RNeXML: ?

* sf: seems to be buglet in our check script as not finding dplyr 0.6.0.

* sjstats: ?

* taxizedb, spdplyr: Too strict checking wording of dplyr error message or
  internals

* tcR: `slice()` is now an S3 method.

* texmexseq: ?

* tidyjson: ?, orphaned

* treeplyr: unsupported usage of `mutate()`

* VWPre: Vignette building error, ?
