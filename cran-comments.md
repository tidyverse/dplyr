## Test environments

* local OS X install, R 3.4.0
* ubuntu 12.04 (on travis-ci), R 3.4.0, R 3.2.5, R-devel.
* win-builder (devel)


## R CMD check results

0 ERRORs | 0 WARNINGs | 0 NOTEs


## Downstream dependencies

New problems that appeared after the emails sent on April 18:

* Three packages (carpenter, geoSpectral, valr) now fail with "Variable context not
  set". Package authors were notified on May 17 with a suggested fix.

* parlitools: Vignette building error, ?

* sf: ?

* tatoo: ?

* taxizedb: Too strict checking on dplyr's and dbplyr's internals

Segfaults:

* bayesplot: During vignette building, not reproducible but likely unrelated to dplyr changes

* openair, vdmR: Reason unclear

Remaining problems, downstream maintainers have not updated their package after April 18:

* assertr: Related to changes in nonstandard evaluation

* broom: Access of deprecated function `lahman_df()`

* chunked: Access of function `sql_render()` now in the dbplyr package

* ddcpr: Error building vignette

* DeLorean, incadata, rmcfs: `filter()` is now an S3 method, the package defines functions named `filter.xxx()`

* emil, lplyr: `select()` is now an S3 method, the package implicitly calls `select.list()`

* etl, infuser, MonetDBLite, RPresto, sqlscore: Bad documentation link to/usage of function `src_sql()` or `src_desc()` or `build_sql()` now in dbplyr

* eyetrackingR: Tests running too long

* ggfortify: `lag()` is now an S3 method, the package calls `dplyr::lag()` instead of `stats::lag()`

* highcharter: Invalid use of dplyr API

* metaplot: ?

* Momocs: `transmute()` is now an S3 generic, https://github.com/vbonhomme/Momocs/issues/178

* myTAI: ?

* RNeXML: ?

* sjstats: ?

* spdplyr: Too strict checking wording of dplyr error message

* tcR: `slice()` is now an S3 method.

* texmexseq: ?

* tidyjson: ?, orphaned

* treeplyr: unsupported usage of `mutate()`

* VWPre: Vignette building error, ?
