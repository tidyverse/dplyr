## Release summary

Minor release

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

## revdepcheck results

We checked 1739 reverse dependencies (1578 from CRAN + 161 from BioConductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 2 new problems
 * We failed to check 77 packages

Issues with CRAN packages are summarised below.

### New problems

Packages crplyr and strapgod fail because their tests use tbl_vars() and compare the results
to a character vector, and tbl_vars() now returns an object of class "dplyr_sel_vars" that 
has an additional attribute to host information about grouping variable. 

Pull requests have been sent to both packages. 

### Failed to check

* ACDm                 (NA)
* ADMMsigma            (NA)
* airGR                (NA)
* BAS                  (NA)
* BMTME                (NA)
* BradleyTerryScalable (NA)
* brunnermunzel        (NA)
* bsam                 (NA)
* CaseBasedReasoning   (NA)
* CB2                  (NA)
* circumplex           (NA)
* cocktailApp          (NA)
* colorednoise         (NA)
* Countr               (NA)
* cpr                  (NA)
* crawl                (NA)
* creditmodel          (NA)
* DataVisualizations   (NA)
* DepthProc            (NA)
* dodgr                (NA)
* dtwclust             (NA)
* dynfrail             (NA)
* exuber               (NA)
* fastLink             (NA)
* fastR2               (NA)
* fourierin            (NA)
* FSelectorRcpp        (NA)
* gap                  (NA)
* GenomicMating        (NA)
* goldi                (NA)
* graphicalVAR         (NA)
* heatwaveR            (NA)
* idealstan            (NA)
* idefix               (NA)
* infer                (NA)
* iRF                  (NA)
* lpirfs               (NA)
* mdsr                 (NA)
* miceFast             (NA)
* momentuHMM           (NA)
* morse                (NA)
* mosaic               (NA)
* mrgsolve             (NA)
* nlmixr               (NA)
* openair              (NA)
* partition            (NA)
* pccc                 (NA)
* phase1PRMD           (NA)
* phenofit             (NA)
* PLNmodels            (NA)
* pomp                 (NA)
* poppr                (NA)
* portalr              (NA)
* PPforest             (NA)
* quanteda             (NA)
* quokar               (NA)
* qwraps2              (NA)
* RCMIP5               (NA)
* rcv                  (NA)
* resautonet           (NA)
* RSSL                 (NA)
* RxODE                (NA)
* sarima               (NA)
* scanstatistics       (NA)
* SCPME                (NA)
* sf                   (NA)
* simputation          (NA)
* SpaCCr               (NA)
* telefit              (NA)
* uncmbb               (NA)
* understandBPMN       (NA)
* vapour               (NA)
* VarSelLCM            (NA)
* vlad                 (NA)
* walker               (NA)
* wand                 (NA)
* weibulltools         (NA)
