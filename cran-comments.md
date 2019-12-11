## Release summary

This is an minor release

## Test environments

* local mac OS install, R 3.6.0
* ubuntu 14.04 (on travis-ci), R-devel, R 3.6.0, R 3.5.2, R 3.4.4, R 3.3.3, R 3.2.5.
* mac OS 10.3.3 (on travis-ci) R 3.6.0
* win-builder (devel)

## R CMD check results

0 ERRORs | 0 WARNINGs | 1 NOTE

*   checking installed package size ... NOTE
    installed size is  8.4Mb
    sub-directories of 1Mb or more:
      libs   6.1Mb

## revdepcheck results

We checked 1983 reverse dependencies (1765 from CRAN + 218 from BioConductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 8 new problems
 * We failed to check 82 packages

Issues with CRAN packages are summarised below.

### New problems

## Missing or unexported object: ‘dplyr::rbind_all’

aemo
boxr
ggvis
gQTLstats
mlVAR
Rariant

Emails or pull requests sent. The `rbind_all()` function has finally been 
removed after being deprecated for a long time. 

## Unexported objects imported by ':::' calls: ‘dplyr:::compat_lazy_dots’ ‘dplyr:::find_var’

dbplyr

Pull request sent. 

## explore

Package `explore` fails because of this fix: 

* `group_by()` does not create an arbitrary NA group when grouping by factors with `drop = TRUE` (#4460).

We've sent a pull request. 

## checking S3 generic/method consistency ... WARNING

group_modify() formals have changed, this affects package egor, pull request sent

## Already fixed

broomExtra development version appears to work, the failures of packages
ggstatsplot and groupedstats are related. 

## Other issues

- Packages DeLorean, geneXtendeR, MonetDBLite, OncoSimulR, perturbatr, sigmajs timed out
- Package MXM had a NOTE about install size
- Packages rgho and TCGAutils failed because of a remote api call

### Failed to check

* ACDm                 (NA)
* ADMMsigma            (NA)
* airGR                (NA)
* BAS                  (NA)
* BayesPostEst         (NA)
* BMTME                (NA)
* BradleyTerryScalable (NA)
* brunnermunzel        (NA)
* bsam                 (NA)
* CaseBasedReasoning   (NA)
* CB2                  (NA)
* circumplex           (NA)
* clustermq            (NA)
* colorednoise         (NA)
* corrcoverage         (NA)
* Countr               (NA)
* cpr                  (NA)
* crawl                (NA)
* DataVisualizations   (NA)
* DeLorean             (NA)
* DepthProc            (NA)
* dexter               (NA)
* dodgr                (NA)
* dtwclust             (NA)
* dynfrail             (NA)
* exuber               (NA)
* fable                (NA)
* fastLink             (NA)
* fingertipsR          (NA)
* fourierin            (NA)
* FSelectorRcpp        (NA)
* gap                  (NA)
* GenomicMating        (NA)
* goldi                (NA)
* graphicalVAR         (NA)
* heatwaveR            (NA)
* idefix               (NA)
* iRF                  (NA)
* lpirfs               (NA)
* mfbvar               (NA)
* miceFast             (NA)
* MixMatrix            (NA)
* momentuHMM           (NA)
* morse                (NA)
* mrgsolve             (NA)
* nlmixr               (NA)
* oceanis              (NA)
* openair              (NA)
* partition            (NA)
* pccc                 (NA)
* phase1PRMD           (NA)
* phenofit             (NA)
* PLNmodels            (NA)
* pomp                 (NA)
* poppr                (NA)
* PPforest             (NA)
* psychonetrics        (NA)
* quanteda             (NA)
* quokar               (NA)
* qwraps2              (NA)
* resautonet           (NA)
* rgho                 (NA)
* RSSL                 (NA)
* RxODE                (NA)
* sarima               (NA)
* scanstatistics       (NA)
* SCPME                (NA)
* sf                   (NA)
* sigminer             (NA)
* SimBIID              (NA)
* simputation          (NA)
* simts                (NA)
* SpaCCr               (NA)
* stcos                (NA)
* telefit              (NA)
* vapour               (NA)
* VarSelLCM            (NA)
* vlad                 (NA)
* walker               (NA)
* weibulltools         (NA)
* WHO                  (NA)
* windfarmGA           (NA)
