This is a minor update to fix problem on CRAN mavericks builder. Compared to the previous submission I have:

* Fixed two unqualified function calls.

* Ensured that tests work even when RPostgreSQL is not available.

--------------------------------------------------------------------------------

The following notes were generated across my local OS X install and ubuntu running on travis-ci. Response to NOTEs across three platforms below.

* checking dependencies in R code ... NOTE
  
  Namespaces in Imports field not imported from: 'R6'
  R6 is a build time dependency.
  
  Missing or unexported object: ‘RSQLite::initExtension’
  This is used for compatibility with RSQlite 1.0 (not yet on CRAN), and is
  only called if packageVersion("RSQLite") >= 1.

I couldn't check on win-builder because it doesn't have the latest Rcpp and appears to be missing RMySQL.

Important reverse dependency check notes (summary at https://github.com/wch/checkresults/blob/master/dplyr/r-release/00check-summary.txt);

* COPASutils, freqweights, qdap, simPH: fail for various reasons. All package 
  authors were informed of the upcoming release and shown R CMD check issues 
  over a week ago.

* ecoengine: same problem as always on our test machine.

* ggvis: You'll be recieving a submission that fixes these issues very shortly
  from Winston.

* repra, rPref: uses a deprecated function.

