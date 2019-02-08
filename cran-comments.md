## Release summary

Fairly major update. We tried to limit the disruption as much as possible, 
but this introduces a few breaking changes that are documented in the NEWS file. 

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
       
## Reverse dependencies

We checked 1491 reverse dependencies (1361 from CRAN + 130 from BioConductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 42 new problems
 * We failed to check 43 packages

Issues with CRAN packages are summarised below. Package maintainers have been notified twice: a month ago 
and a week ago, with guidance on how to update their code when a change cannot be done here. 

### New problems
(This reports the first line of each new failure)

* alphavantager
  checking tests ...

* BMTME
  checking tests ...

* bupaR
  checking S3 generic/method consistency ... WARNING

* carpenter
  checking tests ...

* compareDF
  checking tests ...

* cytominer
  checking tests ...

* dbplyr
  checking tests ...

* desctable
  checking for code/documentation mismatches ... WARNING

* dexter
  checking re-building of vignette outputs ... WARNING

* dlookr
  checking examples ... ERROR
  checking re-building of vignette outputs ... WARNING

* docxtools
  checking examples ... ERROR
  checking tests ...

* dplyr.teradata
  checking S3 generic/method consistency ... WARNING

* dtplyr
  checking tests ...

* edeaR
  checking re-building of vignette outputs ... WARNING

* evaluator
  checking tests ...

* ezsummary
  checking examples ... ERROR
  checking tests ...
  checking re-building of vignette outputs ... WARNING

* ggfan
  checking tests ...

* heemod
  checking examples ... ERROR
  checking tests ...
  checking re-building of vignette outputs ... WARNING

* INDperform
  checking examples ... ERROR

* lmeresampler
  checking tests ...

* modeldb
  checking re-building of vignette outputs ... WARNING

* naniar
  checking examples ... ERROR
  checking tests ...

* pixiedust
  checking tests ...

* poplite
  checking tests ...
  checking for code/documentation mismatches ... WARNING

* portalr
  checking tests ...

* processcheckR
  checking re-building of vignette outputs ... WARNING

* psychmeta
  checking examples ... ERROR

* purrrlyr
  checking examples ... ERROR
  checking tests ...

* radiant.model
  checking examples ... ERROR
  checking tests ...

* replyr
  checking examples ... ERROR

* rPref
  checking examples ... ERROR
  checking re-building of vignette outputs ... WARNING

* ruler
  checking tests ...
  checking re-building of vignette outputs ... WARNING

* RWDataPlyr
  checking tests ...

* spdplyr
  checking tests ...

* tbrf
  checking tests ...

* tibbletime
  checking tests ...

* tidygraph
  checking S3 generic/method consistency ... WARNING

* treeplyr
  checking examples ... ERROR

* tsibble
  checking tests ...

* visdat
  checking examples ... ERROR
  checking tests ...

* vqtl
  checking tests ...

* zFactor
  checking examples ... ERROR
  checking tests ...

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
* mbgraphic          (failed to install)
* miceFast           (failed to install)
* morse              (failed to install)
* petro.One          (failed to install)
* pmc                (check timed out)
* poppr              (failed to install)
* qdap               (failed to install)
* Rdrools            (failed to install)
* rmcfs              (failed to install)
* rpcdsearch         (failed to install)
* RtutoR             (failed to install)
* SEERaBomb          (failed to install)
* segclust2d         (failed to install)
* sf                 (failed to install)
* simputation        (failed to install)
* textmining         (failed to install)
* vapour             (failed to install)
* vlad               (failed to install)
* wand               (failed to install)
* weibulltools       (failed to install)
