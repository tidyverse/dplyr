# accept

<details>

* Version: 0.8.3
* GitHub: NA
* Source code: https://github.com/cran/accept
* Date/Publication: 2021-06-18 10:00:07 UTC
* Number of recursive dependencies: 96

Run `cloud_details(, "accept")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/accept/new/accept.Rcheck’
* using R version 4.1.1 (2021-08-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘accept/DESCRIPTION’ ... OK
* this is package ‘accept’ version ‘0.8.3’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘reldist’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/accept/old/accept.Rcheck’
* using R version 4.1.1 (2021-08-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘accept/DESCRIPTION’ ... OK
* this is package ‘accept’ version ‘0.8.3’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘reldist’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# CausalImpact

<details>

* Version: 1.2.7
* GitHub: NA
* Source code: https://github.com/cran/CausalImpact
* Date/Publication: 2021-06-07 06:40:02 UTC
* Number of recursive dependencies: 68

Run `cloud_details(, "CausalImpact")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/CausalImpact/new/CausalImpact.Rcheck’
* using R version 4.1.1 (2021-08-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘CausalImpact/DESCRIPTION’ ... OK
* this is package ‘CausalImpact’ version ‘1.2.7’
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'bsts', 'Boom'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/CausalImpact/old/CausalImpact.Rcheck’
* using R version 4.1.1 (2021-08-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘CausalImpact/DESCRIPTION’ ... OK
* this is package ‘CausalImpact’ version ‘1.2.7’
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'bsts', 'Boom'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# cbar

<details>

* Version: 0.1.3
* GitHub: https://github.com/zedoul/cbar
* Source code: https://github.com/cran/cbar
* Date/Publication: 2017-10-24 13:20:22 UTC
* Number of recursive dependencies: 61

Run `cloud_details(, "cbar")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/cbar/new/cbar.Rcheck’
* using R version 4.1.1 (2021-08-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘cbar/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘cbar’ version ‘0.1.3’
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'Boom', 'bsts'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/cbar/old/cbar.Rcheck’
* using R version 4.1.1 (2021-08-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘cbar/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘cbar’ version ‘0.1.3’
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'Boom', 'bsts'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# conos

<details>

* Version: 1.4.5
* GitHub: https://github.com/kharchenkolab/conos
* Source code: https://github.com/cran/conos
* Date/Publication: 2022-01-21 09:12:56 UTC
* Number of recursive dependencies: 238

Run `cloud_details(, "conos")` for more info

</details>

## In both

*   checking whether package ‘conos’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/conos/new/conos.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘conos’ ...
** package ‘conos’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppEigen/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppProgress/include' -I/usr/local/include  -I"./include" -fopenmp  -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
In file included from /opt/R/4.1.1/lib/R/site-library/RcppEigen/include/Eigen/Core:397,
                 from /opt/R/4.1.1/lib/R/site-library/RcppEigen/include/Eigen/Dense:1,
                 from /opt/R/4.1.1/lib/R/site-library/RcppEigen/include/RcppEigenForward.h:30,
                 from /opt/R/4.1.1/lib/R/site-library/RcppEigen/include/RcppEigen.h:25,
                 from RcppExports.cpp:6:
...
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in dyn.load(file, DLLpath = DLLpath, ...) : 
  unable to load shared object '/opt/R/4.1.1/lib/R/site-library/leidenAlg/libs/leidenAlg.so':
  /rspm_builder/tmp/tmp.vYZcnqsJUt/igraph/libs/igraph.so: cannot open shared object file: No such file or directory
Calls: <Anonymous> ... namespaceImport -> loadNamespace -> library.dynam -> dyn.load
Execution halted
ERROR: lazy loading failed for package ‘conos’
* removing ‘/tmp/workdir/conos/new/conos.Rcheck/conos’


```
### CRAN

```
* installing *source* package ‘conos’ ...
** package ‘conos’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppEigen/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppProgress/include' -I/usr/local/include  -I"./include" -fopenmp  -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
In file included from /opt/R/4.1.1/lib/R/site-library/RcppEigen/include/Eigen/Core:397,
                 from /opt/R/4.1.1/lib/R/site-library/RcppEigen/include/Eigen/Dense:1,
                 from /opt/R/4.1.1/lib/R/site-library/RcppEigen/include/RcppEigenForward.h:30,
                 from /opt/R/4.1.1/lib/R/site-library/RcppEigen/include/RcppEigen.h:25,
                 from RcppExports.cpp:6:
...
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in dyn.load(file, DLLpath = DLLpath, ...) : 
  unable to load shared object '/opt/R/4.1.1/lib/R/site-library/leidenAlg/libs/leidenAlg.so':
  /rspm_builder/tmp/tmp.vYZcnqsJUt/igraph/libs/igraph.so: cannot open shared object file: No such file or directory
Calls: <Anonymous> ... namespaceImport -> loadNamespace -> library.dynam -> dyn.load
Execution halted
ERROR: lazy loading failed for package ‘conos’
* removing ‘/tmp/workdir/conos/old/conos.Rcheck/conos’


```
# geocmeans

<details>

* Version: 0.2.0
* GitHub: https://github.com/JeremyGelb/geocmeans
* Source code: https://github.com/cran/geocmeans
* Date/Publication: 2021-08-23 07:11:35 UTC
* Number of recursive dependencies: 203

Run `cloud_details(, "geocmeans")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/geocmeans/new/geocmeans.Rcheck’
* using R version 4.1.1 (2021-08-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘geocmeans/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘geocmeans’ version ‘0.2.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘reldist’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/geocmeans/old/geocmeans.Rcheck’
* using R version 4.1.1 (2021-08-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘geocmeans/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘geocmeans’ version ‘0.2.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘reldist’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# loon.ggplot

<details>

* Version: 1.3.0
* GitHub: https://github.com/great-northern-diver/loon.ggplot
* Source code: https://github.com/cran/loon.ggplot
* Date/Publication: 2021-09-28 14:00:05 UTC
* Number of recursive dependencies: 99

Run `cloud_details(, "loon.ggplot")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/loon.ggplot/new/loon.ggplot.Rcheck’
* using R version 4.1.1 (2021-08-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘loon.ggplot/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘loon.ggplot’ version ‘1.3.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘loon’

Package suggested but not available for checking: ‘zenplots’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/loon.ggplot/old/loon.ggplot.Rcheck’
* using R version 4.1.1 (2021-08-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘loon.ggplot/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘loon.ggplot’ version ‘1.3.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘loon’

Package suggested but not available for checking: ‘zenplots’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# loon.shiny

<details>

* Version: 1.0.1
* GitHub: NA
* Source code: https://github.com/cran/loon.shiny
* Date/Publication: 2021-09-27 19:40:02 UTC
* Number of recursive dependencies: 131

Run `cloud_details(, "loon.shiny")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/loon.shiny/new/loon.shiny.Rcheck’
* using R version 4.1.1 (2021-08-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘loon.shiny/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘loon.shiny’ version ‘1.0.1’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'loon', 'loon.ggplot'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/loon.shiny/old/loon.shiny.Rcheck’
* using R version 4.1.1 (2021-08-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘loon.shiny/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘loon.shiny’ version ‘1.0.1’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'loon', 'loon.ggplot'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# MarketMatching

<details>

* Version: 1.2.0
* GitHub: NA
* Source code: https://github.com/cran/MarketMatching
* Date/Publication: 2021-01-08 20:10:02 UTC
* Number of recursive dependencies: 67

Run `cloud_details(, "MarketMatching")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/MarketMatching/new/MarketMatching.Rcheck’
* using R version 4.1.1 (2021-08-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘MarketMatching/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘MarketMatching’ version ‘1.2.0’
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'CausalImpact', 'bsts', 'Boom'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/MarketMatching/old/MarketMatching.Rcheck’
* using R version 4.1.1 (2021-08-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘MarketMatching/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘MarketMatching’ version ‘1.2.0’
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'CausalImpact', 'bsts', 'Boom'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# PLNmodels

<details>

* Version: 0.11.5
* GitHub: https://github.com/pln-team/PLNmodels
* Source code: https://github.com/cran/PLNmodels
* Date/Publication: 2022-01-20 22:42:49 UTC
* Number of recursive dependencies: 173

Run `cloud_details(, "PLNmodels")` for more info

</details>

## In both

*   checking whether package ‘PLNmodels’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/PLNmodels/new/PLNmodels.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘PLNmodels’ ...
** package ‘PLNmodels’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.1.1/lib/R/site-library/nloptr/include' -I/usr/local/include   -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.1.1/lib/R/site-library/nloptr/include' -I/usr/local/include   -fpic  -g -O2  -c nlopt_wrapper.cpp -o nlopt_wrapper.o
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.1.1/lib/R/site-library/nloptr/include' -I/usr/local/include   -fpic  -g -O2  -c optim_diag_cov.cpp -o optim_diag_cov.o
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.1.1/lib/R/site-library/nloptr/include' -I/usr/local/include   -fpic  -g -O2  -c optim_full_cov.cpp -o optim_full_cov.o
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.1.1/lib/R/site-library/nloptr/include' -I/usr/local/include   -fpic  -g -O2  -c optim_genet_cov.cpp -o optim_genet_cov.o
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.1.1/lib/R/site-library/nloptr/include' -I/usr/local/include   -fpic  -g -O2  -c optim_rank_cov.cpp -o optim_rank_cov.o
...
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
Error: package or namespace load failed for ‘PLNmodels’ in dyn.load(file, DLLpath = DLLpath, ...):
 unable to load shared object '/tmp/workdir/PLNmodels/new/PLNmodels.Rcheck/00LOCK-PLNmodels/00new/PLNmodels/libs/PLNmodels.so':
  /tmp/workdir/PLNmodels/new/PLNmodels.Rcheck/00LOCK-PLNmodels/00new/PLNmodels/libs/PLNmodels.so: undefined symbol: nlopt_set_min_objective
Error: loading failed
Execution halted
ERROR: loading failed
* removing ‘/tmp/workdir/PLNmodels/new/PLNmodels.Rcheck/PLNmodels’


```
### CRAN

```
* installing *source* package ‘PLNmodels’ ...
** package ‘PLNmodels’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.1.1/lib/R/site-library/nloptr/include' -I/usr/local/include   -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.1.1/lib/R/site-library/nloptr/include' -I/usr/local/include   -fpic  -g -O2  -c nlopt_wrapper.cpp -o nlopt_wrapper.o
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.1.1/lib/R/site-library/nloptr/include' -I/usr/local/include   -fpic  -g -O2  -c optim_diag_cov.cpp -o optim_diag_cov.o
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.1.1/lib/R/site-library/nloptr/include' -I/usr/local/include   -fpic  -g -O2  -c optim_full_cov.cpp -o optim_full_cov.o
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.1.1/lib/R/site-library/nloptr/include' -I/usr/local/include   -fpic  -g -O2  -c optim_genet_cov.cpp -o optim_genet_cov.o
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.1.1/lib/R/site-library/nloptr/include' -I/usr/local/include   -fpic  -g -O2  -c optim_rank_cov.cpp -o optim_rank_cov.o
...
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
Error: package or namespace load failed for ‘PLNmodels’ in dyn.load(file, DLLpath = DLLpath, ...):
 unable to load shared object '/tmp/workdir/PLNmodels/old/PLNmodels.Rcheck/00LOCK-PLNmodels/00new/PLNmodels/libs/PLNmodels.so':
  /tmp/workdir/PLNmodels/old/PLNmodels.Rcheck/00LOCK-PLNmodels/00new/PLNmodels/libs/PLNmodels.so: undefined symbol: nlopt_set_min_objective
Error: loading failed
Execution halted
ERROR: loading failed
* removing ‘/tmp/workdir/PLNmodels/old/PLNmodels.Rcheck/PLNmodels’


```
# RAQSAPI

<details>

* Version: 2.0.2
* GitHub: https://github.com/USEPA/RAQSAPI
* Source code: https://github.com/cran/RAQSAPI
* Date/Publication: 2021-11-29 18:40:06 UTC
* Number of recursive dependencies: 99

Run `cloud_details(, "RAQSAPI")` for more info

</details>

## In both

*   checking whether package ‘RAQSAPI’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/RAQSAPI/new/RAQSAPI.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘RAQSAPI’ ...
** package ‘RAQSAPI’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘call_frame’ is not exported by 'namespace:rlang'
Execution halted
ERROR: lazy loading failed for package ‘RAQSAPI’
* removing ‘/tmp/workdir/RAQSAPI/new/RAQSAPI.Rcheck/RAQSAPI’


```
### CRAN

```
* installing *source* package ‘RAQSAPI’ ...
** package ‘RAQSAPI’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘call_frame’ is not exported by 'namespace:rlang'
Execution halted
ERROR: lazy loading failed for package ‘RAQSAPI’
* removing ‘/tmp/workdir/RAQSAPI/old/RAQSAPI.Rcheck/RAQSAPI’


```
# vivid

<details>

* Version: 0.2.3
* GitHub: NA
* Source code: https://github.com/cran/vivid
* Date/Publication: 2021-11-20 01:30:02 UTC
* Number of recursive dependencies: 202

Run `cloud_details(, "vivid")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/vivid/new/vivid.Rcheck’
* using R version 4.1.1 (2021-08-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘vivid/DESCRIPTION’ ... OK
* this is package ‘vivid’ version ‘0.2.3’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... NOTE
...
* checking tests ... OK
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘vivid.Rmd’ using ‘UTF-8’... OK
  ‘vividQStart.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 2 NOTEs





```
### CRAN

```
* using log directory ‘/tmp/workdir/vivid/old/vivid.Rcheck’
* using R version 4.1.1 (2021-08-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘vivid/DESCRIPTION’ ... OK
* this is package ‘vivid’ version ‘0.2.3’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... NOTE
...
* checking tests ... OK
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘vivid.Rmd’ using ‘UTF-8’... OK
  ‘vividQStart.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 2 NOTEs





```
