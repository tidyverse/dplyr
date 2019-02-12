## Release summary

Fairly major update. We tried to limit the disruption as much as possible, 
but this introduces a few breaking changes that are documented in the NEWS file. 

Maintainers have been notified twice: a month ago and a week ago. 
We confirmed that all changes were deliberate (either bug fixes or deliberate API changes) and provided guidance on how to update existing code

## Test environments

* local mac OS install, R 3.5.0
* ubuntu 14.04 (on travis-ci), R-devel, R 3.5.2, R 3.4.4, R 3.3.3, R 3.2.5, R 3.1.3.
* mac OS 10.3.3 (on travis-ci) R 3.5.2
* win-builder (devel)

## R CMD check results

0 ERRORs | 0 WARNINGs | 1 NOTE

*  checking installed package size ...
     installed size is  6.3Mb
     sub-directories of 1Mb or more:
       R      2.1Mb
       libs   2.4Mb
       
## revdepcheck results

We checked 1509 reverse dependencies (1379 from CRAN + 130 from BioConductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 40 new problems
 * We failed to check 46 packages

Issues with CRAN packages are summarised below.

### New problems
(This reports the first line of each new failure)

* alphavantager: API rate limit issue

 * bupaR
 * dplyr.teradata
 * dtplyr
 * tidygraph
  S3 generic/method consistency: sample_n() gained ... 

 * desctable
  code/documentation mismatches ... group_by() gained .drop=

 * zFactor
 * visdat
 * heemod: 
 * naniar: Needs to import dplyr::n()

 * cytominer: related to failures from dbplyr

 * ggmap: NOTE, sub directory data/ > 1Mb

 * portalr: test relies on digest()ing a data frame, probably related to changes in internals of grouped data frames

 * postal: IO problem, probably unrelated

 * purrrlyr: Work is being done in the dev version

 * simTool: test uses internal format of grouped tibbles, which has changed. PR sent. 
  
 * tibbletime: test problem. PR sent. 
 
 * treeplyr: The package uses internal details. PR sent. 

 * tsibble: Fixed in the dev version
 * vqtl: Fixed in the dev version
 * modeldb: Fixed in the dev version
 * rPref: Fixed in the dev version
 * ruler: Fixed in the dev version 
 * dbplyr: fixed in dev version
 * lmeresampler: Fixed in dev version
 * ggfan: Fixed in dev version

 * dexter: Fails to build the vignette, not sure why

 * processcheckR: needs an update of the bupaR package
 * edeaR: needs an update of the bupaR package

 * ezsummary: Needs to use api instead of attributes. PR sent. 

 * gravity: Does not correctly use tidyeval, PR sent. 

 * poplite: Not sure what the problem is. 

 * corrr: Not sure what the problem is. Issue sent. 

## Need to deal with #4094, #4174

 * psychmeta: Using `.data$.` instead of `.`, Pull request sent. 
 * radiant.model
 * replyr
 * safetyGraphics

### Failed to check

* anomalyDetection   (failed to install)
* arkdb              (check timed out)
* bsam               (failed to install)
* circumplex         (failed to install)
* clustermq          (check timed out)
* CollapsABEL        (failed to install)
* colorednoise       (failed to install)
* DepthProc          (failed to install)
* DiversityOccupancy (failed to install)
* dynfrail           (failed to install)
* easyformatr        (failed to install)
* fastLink           (failed to install)
* FSelectorRcpp      (failed to install)
* graphicalVAR       (failed to install)
* harrietr           (failed to install)
* heatwaveR          (failed to install)
* HTSSIP             (check timed out)
* idefix             (failed to install)
* ijtiff             (failed to install)
* iRF                (failed to install)
* lilikoi            (failed to install)
* LLSR               (failed to install)
* lpirfs             (failed to install)
* mapfuser           (failed to install)
* mbgraphic          (failed to install)
* miceFast           (failed to install)
* MonetDBLite        (check timed out)
* morse              (failed to install)
* petro.One          (failed to install)
* phase1PRMD         (failed to install)
* pmc                (check timed out)
* poppr              (failed to install)
* qdap               (failed to install)
* Rdrools            (failed to install)
* rmcfs              (failed to install)
* rpcdsearch         (failed to install)
* RtutoR             (failed to install)
* segclust2d         (failed to install)
* sf                 (failed to install)
* simputation        (failed to install)
* textmining         (failed to install)
* vapour             (failed to install)
* vlad               (failed to install)
* wand               (failed to install)
* weibulltools       (failed to install)
* windfarmGA         (failed to install)
