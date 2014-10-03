The following notes were generated across my local OS X install and ubuntu running on travis-ci. Response to NOTEs across three platforms below.

* checking CRAN incoming feasibility ... NOTE

  I changed my email address to hadley@rstudio.com.

* checking dependencies in R code ... NOTE
  
  Namespaces in Imports field not imported from: 'R6'
  R6 is a build time dependency.
  
  Missing or unexported object: ‘RSQLite::initExtension’
  This is used for compatibility with RSQlite 1.0 (not yet on CRAN), and is
  only called if packageVersion("RSQLite") >= 1.

* checking R code for possible problems ... NOTE

  src_mysql: no visible global function definition for ‘MySQL’
  src_postgres: no visible global function definition for ‘PostgreSQL’
  
  These packages currently need to be attached in order to work.

I couldn't check on win-builder because it doesn't have the latest Rcpp and appears to be missing RMySQL.

Important reverse dependency check notes (summary at https://github.com/wch/checkresults/blob/master/dplyr/r-release/00check-summary.txt);

* COPASutils, freqweights, qdap, simPH: fail for various reasons. All package 
  authors were informed of the upcoming release and shown R CMD check issues 
  over a week ago.

* ecoengine: same problem as always on our test machine.

* ggvis: You'll be recieving a submission that fixes these issues very shortly
  from Winston.

* repra, rPref: uses a deprecated function.

