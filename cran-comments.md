## Release summary

This is an emergency release due to community finding of a 
major performance regression. 

## Test environments

* local mac OS install, R 3.6.0
* ubuntu 14.04 (on travis-ci), R-devel, R 3.6.0, R 3.5.2, R 3.4.4, R 3.3.3, R 3.2.5.
* mac OS 10.3.3 (on travis-ci) R 3.6.0
* win-builder (devel)

## R CMD check results

0 ERRORs | 0 WARNINGs | 1 NOTE

*   installed size is  8.3Mb
    sub-directories of 1Mb or more:
      libs   6.1Mb
       
## revdepcheck results

We checked 1739 reverse dependencies (1578 from CRAN + 161 from BioConductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 5 new problems
 * We failed to check 78 packages

Issues with CRAN packages are summarised below.

### New problems
(This reports the first line of each new failure)

Packages ggmap, oec, portalr, solrium, tidyquant appear to have false errors. 

* ggmap
  checking installed package size ... NOTE

* oec
  checking dependencies in R code ... NOTE

* portalr
  checking tests ...

* solrium
  checking Rd cross-references ... NOTE

* tidyquant
  checking tests ...

### Failed to check

* ACDm                 (NA)
* ADMMsigma            (NA)
* airGR                (NA)
* BAS                  (NA)
* BMTME                (NA)
* BradleyTerryScalable (NA)
* breathtestcore       (NA)
* brunnermunzel        (NA)
* bsam                 (NA)
* CaseBasedReasoning   (NA)
* CB2                  (NA)
* circumplex           (NA)
* clustermq            (NA)
* colorednoise         (NA)
* Countr               (NA)
* cpr                  (NA)
* crawl                (NA)
* DataVisualizations   (NA)
* DepthProc            (NA)
* dodgr                (NA)
* dtwclust             (NA)
* dynfrail             (NA)
* exuber               (NA)
* fastLink             (NA)
* fourierin            (NA)
* FSelectorRcpp        (NA)
* gap                  (NA)
* genogeographer       (NA)
* GenomicMating        (NA)
* goldi                (NA)
* graphicalVAR         (NA)
* heatwaveR            (NA)
* idealstan            (NA)
* idefix               (NA)
* iRF                  (NA)
* loose.rock           (NA)
* lpirfs               (NA)
* MetaboList           (NA)
* miceFast             (NA)
* missCompare          (NA)
* momentuHMM           (NA)
* morse                (NA)
* mrgsolve             (NA)
* neonUtilities        (NA)
* nlmixr               (NA)
* openair              (NA)
* OutliersO3           (NA)
* parSim               (NA)
* partition            (NA)
* pccc                 (NA)
* phase1PRMD           (NA)
* phenofit             (NA)
* PLNmodels            (NA)
* pomp                 (NA)
* poppr                (NA)
* PPforest             (NA)
* quanteda             (NA)
* quokar               (NA)
* qwraps2              (NA)
* radtools             (NA)
* resautonet           (NA)
* RSSL                 (NA)
* RxODE                (NA)
* sarima               (NA)
* scanstatistics       (NA)
* SCPME                (NA)
* sf                   (NA)
* simglm               (NA)
* simputation          (NA)
* solvebio             (NA)
* SpaCCr               (NA)
* telefit              (NA)
* vapour               (NA)
* VarSelLCM            (NA)
* vlad                 (NA)
* walker               (NA)
* wand                 (NA)
* weibulltools         (NA)
