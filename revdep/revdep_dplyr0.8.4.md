## Missing or unexported object: ‘dplyr::rbind_all’

aemo
boxr
ggvis
gQTLstats
mlVAR
Rariant

Emails or pull requests sent. 

## Unexported objects imported by ':::' calls: ‘dplyr:::compat_lazy_dots’ ‘dplyr:::find_var’

dbplyr

Pull request sent. 

## explore

Package `explore` fails because of this fix: 

* `group_by()` does not create an arbitrary NA group when grouping by factors with `drop = TRUE` (#4460).

We've sent a pull request. 

## checking S3 generic/method consistency ... WARNING

group_modify() formals have changed, this affects packages: 

- egor

## Already fixed

broomExtra development version appears to work, the failures of packages
ggstatsplot and groupedstats are related. 

## Other issues

- Packages DeLorean, geneXtendeR, MonetDBLite, OncoSimulR, perturbatr, sigmajs timed out
- Package MXM had a NOTE about install size
- Packages rgho and TCGAutils failed because of a remote api call
