# bayesdfa

<details>

* Version: 1.2.0
* GitHub: https://github.com/fate-ewi/bayesdfa
* Source code: https://github.com/cran/bayesdfa
* Date/Publication: 2021-09-28 13:20:02 UTC
* Number of recursive dependencies: 81

Run `cloud_details(, "bayesdfa")` for more info

</details>

## In both

*   checking whether package ‘bayesdfa’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/bayesdfa/new/bayesdfa.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘bayesdfa’ ...
** package ‘bayesdfa’ successfully unpacked and MD5 sums checked
** using staged installation
** libs


g++ -std=gnu++14 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG -I"../inst/include" -I"/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I'/opt/R/4.0.3/lib/R/site-library/BH/include' -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppEigen/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppParallel/include' -I'/opt/R/4.0.3/lib/R/site-library/rstan/include' -I'/opt/R/4.0.3/lib/R/site-library/StanHeaders/include' -I/usr/local/include   -I'/opt/R/4.0.3/lib/R/site-library/RcppParallel/include' -D_REENTRANT -DSTAN_THREADS   -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
In file included from /opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/Core:397,
                 from /opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/Dense:1,
                 from /opt/R/4.0.3/lib/R/site-library/RcppEigen/include/RcppEigenForward.h:30,
...
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/ProductEvaluators.h:35:90:   required from ‘Eigen::internal::evaluator<Eigen::Product<Lhs, Rhs, Option> >::evaluator(const XprType&) [with Lhs = Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>; Rhs = Eigen::Matrix<double, -1, 1>; int Options = 0; Eigen::internal::evaluator<Eigen::Product<Lhs, Rhs, Option> >::XprType = Eigen::Product<Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>, Eigen::Matrix<double, -1, 1>, 0>]’
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/Product.h:132:22:   required from ‘Eigen::internal::dense_product_base<Lhs, Rhs, Option, 6>::operator const Scalar() const [with Lhs = Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>; Rhs = Eigen::Matrix<double, -1, 1>; int Option = 0; Eigen::internal::dense_product_base<Lhs, Rhs, Option, 6>::Scalar = double]’
/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:23:56:   required from ‘double stan::mcmc::dense_e_metric<Model, BaseRNG>::T(stan::mcmc::dense_e_point&) [with Model = model_dfa_namespace::model_dfa; BaseRNG = boost::random::additive_combine_engine<boost::random::linear_congruential_engine<unsigned int, 40014, 0, 2147483563>, boost::random::linear_congruential_engine<unsigned int, 40692, 0, 2147483399> >]’
/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:22:10:   required from here
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:55:30: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__vector(2) double’} [-Wignored-attributes]
g++: fatal error: Killed signal terminated program cc1plus
compilation terminated.
make: *** [/opt/R/4.0.3/lib/R/etc/Makeconf:179: stanExports_dfa.o] Error 1
ERROR: compilation failed for package ‘bayesdfa’
* removing ‘/tmp/workdir/bayesdfa/new/bayesdfa.Rcheck/bayesdfa’


```
### CRAN

```
* installing *source* package ‘bayesdfa’ ...
** package ‘bayesdfa’ successfully unpacked and MD5 sums checked
** using staged installation
** libs


g++ -std=gnu++14 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG -I"../inst/include" -I"/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I'/opt/R/4.0.3/lib/R/site-library/BH/include' -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppEigen/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppParallel/include' -I'/opt/R/4.0.3/lib/R/site-library/rstan/include' -I'/opt/R/4.0.3/lib/R/site-library/StanHeaders/include' -I/usr/local/include   -I'/opt/R/4.0.3/lib/R/site-library/RcppParallel/include' -D_REENTRANT -DSTAN_THREADS   -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
In file included from /opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/Core:397,
                 from /opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/Dense:1,
                 from /opt/R/4.0.3/lib/R/site-library/RcppEigen/include/RcppEigenForward.h:30,
...
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/ProductEvaluators.h:35:90:   required from ‘Eigen::internal::evaluator<Eigen::Product<Lhs, Rhs, Option> >::evaluator(const XprType&) [with Lhs = Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>; Rhs = Eigen::Matrix<double, -1, 1>; int Options = 0; Eigen::internal::evaluator<Eigen::Product<Lhs, Rhs, Option> >::XprType = Eigen::Product<Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>, Eigen::Matrix<double, -1, 1>, 0>]’
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/Product.h:132:22:   required from ‘Eigen::internal::dense_product_base<Lhs, Rhs, Option, 6>::operator const Scalar() const [with Lhs = Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>; Rhs = Eigen::Matrix<double, -1, 1>; int Option = 0; Eigen::internal::dense_product_base<Lhs, Rhs, Option, 6>::Scalar = double]’
/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:23:56:   required from ‘double stan::mcmc::dense_e_metric<Model, BaseRNG>::T(stan::mcmc::dense_e_point&) [with Model = model_dfa_namespace::model_dfa; BaseRNG = boost::random::additive_combine_engine<boost::random::linear_congruential_engine<unsigned int, 40014, 0, 2147483563>, boost::random::linear_congruential_engine<unsigned int, 40692, 0, 2147483399> >]’
/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:22:10:   required from here
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:55:30: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__vector(2) double’} [-Wignored-attributes]
g++: fatal error: Killed signal terminated program cc1plus
compilation terminated.
make: *** [/opt/R/4.0.3/lib/R/etc/Makeconf:179: stanExports_dfa.o] Error 1
ERROR: compilation failed for package ‘bayesdfa’
* removing ‘/tmp/workdir/bayesdfa/old/bayesdfa.Rcheck/bayesdfa’


```
# butcher

<details>

* Version: 0.1.5
* GitHub: https://github.com/tidymodels/butcher
* Source code: https://github.com/cran/butcher
* Date/Publication: 2021-06-28 07:30:02 UTC
* Number of recursive dependencies: 187

Run `cloud_details(, "butcher")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/butcher/new/butcher.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘butcher/DESCRIPTION’ ... OK
* this is package ‘butcher’ version ‘0.1.5’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... NOTE
...
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘adding-models-to-butcher.Rmd’ using ‘UTF-8’... OK
  ‘available-axe-methods.Rmd’ using ‘UTF-8’... OK
  ‘butcher.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 1 NOTE





```
### CRAN

```
* using log directory ‘/tmp/workdir/butcher/old/butcher.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘butcher/DESCRIPTION’ ... OK
* this is package ‘butcher’ version ‘0.1.5’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... NOTE
...
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘adding-models-to-butcher.Rmd’ using ‘UTF-8’... OK
  ‘available-axe-methods.Rmd’ using ‘UTF-8’... OK
  ‘butcher.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 1 NOTE





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
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
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
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
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
# CB2

<details>

* Version: 1.3.4
* GitHub: NA
* Source code: https://github.com/cran/CB2
* Date/Publication: 2020-07-24 09:42:24 UTC
* Number of recursive dependencies: 133

Run `cloud_details(, "CB2")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/CB2/new/CB2.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘CB2/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘CB2’ version ‘1.3.4’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘metap’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/CB2/old/CB2.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘CB2/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘CB2’ version ‘1.3.4’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘metap’

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
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
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
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
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
# DAISIE

<details>

* Version: 3.0.1
* GitHub: NA
* Source code: https://github.com/cran/DAISIE
* Date/Publication: 2020-08-26 10:00:07 UTC
* Number of recursive dependencies: 111

Run `cloud_details(, "DAISIE")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/DAISIE/new/DAISIE.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘DAISIE/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘DAISIE’ version ‘3.0.1’
* package encoding: UTF-8
* checking package namespace information ... OK
...
Package required but not available: ‘DDD’

Packages suggested but not available for checking: 'ggtree', 'phytools'

Package which this enhances but not available for checking: ‘doMC’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/DAISIE/old/DAISIE.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘DAISIE/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘DAISIE’ version ‘3.0.1’
* package encoding: UTF-8
* checking package namespace information ... OK
...
Package required but not available: ‘DDD’

Packages suggested but not available for checking: 'ggtree', 'phytools'

Package which this enhances but not available for checking: ‘doMC’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# diceR

<details>

* Version: 1.1.0
* GitHub: https://github.com/AlineTalhouk/diceR
* Source code: https://github.com/cran/diceR
* Date/Publication: 2021-07-23 19:30:01 UTC
* Number of recursive dependencies: 152

Run `cloud_details(, "diceR")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/diceR/new/diceR.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘diceR/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘diceR’ version ‘1.1.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘NMF’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/diceR/old/diceR.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘diceR/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘diceR’ version ‘1.1.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘NMF’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# ecocomDP

<details>

* Version: 1.2.0
* GitHub: https://github.com/EDIorg/ecocomDP
* Source code: https://github.com/cran/ecocomDP
* Date/Publication: 2021-10-18 22:50:05 UTC
* Number of recursive dependencies: 152

Run `cloud_details(, "ecocomDP")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/ecocomDP/new/ecocomDP.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘ecocomDP/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘ecocomDP’ version ‘1.2.0’
* package encoding: UTF-8
* checking package namespace information ... OK
...
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘convert.Rmd’ using ‘UTF-8’... OK
  ‘create.Rmd’ using ‘UTF-8’... OK
  ‘model_overview.Rmd’ using ‘UTF-8’... OK
  ‘shared_practices_create.Rmd’ using ‘UTF-8’... OK
  ‘use.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 1 NOTE





```
### CRAN

```
* using log directory ‘/tmp/workdir/ecocomDP/old/ecocomDP.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘ecocomDP/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘ecocomDP’ version ‘1.2.0’
* package encoding: UTF-8
* checking package namespace information ... OK
...
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘convert.Rmd’ using ‘UTF-8’... OK
  ‘create.Rmd’ using ‘UTF-8’... OK
  ‘model_overview.Rmd’ using ‘UTF-8’... OK
  ‘shared_practices_create.Rmd’ using ‘UTF-8’... OK
  ‘use.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 1 NOTE





```
# ezcox

<details>

* Version: 1.0.2
* GitHub: https://github.com/ShixiangWang/ezcox
* Source code: https://github.com/cran/ezcox
* Date/Publication: 2021-10-28 15:20:08 UTC
* Number of recursive dependencies: 88

Run `cloud_details(, "ezcox")` for more info

</details>

## Newly broken

*   checking whether package ‘ezcox’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/ezcox/new/ezcox.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      During startup - Warning message:
      package ‘stats’ in options("defaultPackages") was not found 
      Error: package or namespace load failed for ‘utils’:
       .onLoad failed in loadNamespace() for 'utils', details:
        call: system("uname -a", intern = TRUE)
        error: cannot popen 'uname -a', probable reason 'Cannot allocate memory'
      Error: package or namespace load failed for ‘grDevices’ in get(Info[i, 1], envir = env):
       read failed on /opt/R/4.0.3/lib/R/library/grDevices/R/grDevices.rdb
      Error: package or namespace load failed for ‘graphics’:
       .onLoad failed in loadNamespace() for 'grDevices', details:
        call: .select_device()
        error: lazy-load database '/opt/R/4.0.3/lib/R/library/grDevices/R/grDevices.rdb' is corrupt
      Killed
      Error: package or namespace load failed for ‘graphics’ in get(Info[i, 1], envir = env):
       lazy-load database '/opt/R/4.0.3/lib/R/library/grDevices/R/grDevices.rdb' is corrupt
    ```

## Installation

### Devel

```
* installing *source* package ‘ezcox’ ...
** package ‘ezcox’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Killed
ERROR: lazy loading failed for package ‘ezcox’
* removing ‘/tmp/workdir/ezcox/new/ezcox.Rcheck/ezcox’


```
### CRAN

```
* installing *source* package ‘ezcox’ ...
** package ‘ezcox’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (ezcox)


```
# galah

<details>

* Version: 1.3.1
* GitHub: https://github.com/AtlasOfLivingAustralia/galah
* Source code: https://github.com/cran/galah
* Date/Publication: 2021-08-21 09:20:02 UTC
* Number of recursive dependencies: 162

Run `cloud_details(, "galah")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/galah/new/galah.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘galah/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘galah’ version ‘1.3.1’
* package encoding: UTF-8
* checking package namespace information ... OK
...
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘galah.Rmd’ using ‘UTF-8’... OK
  ‘international_atlases.Rmd’ using ‘UTF-8’... OK
  ‘taxonomic_information.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 1 NOTE





```
### CRAN

```
* using log directory ‘/tmp/workdir/galah/old/galah.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘galah/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘galah’ version ‘1.3.1’
* package encoding: UTF-8
* checking package namespace information ... OK
...
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘galah.Rmd’ using ‘UTF-8’... OK
  ‘international_atlases.Rmd’ using ‘UTF-8’... OK
  ‘taxonomic_information.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 1 NOTE





```
# gitdown

<details>

* Version: 0.1.3
* GitHub: https://github.com/Thinkr-open/gitdown
* Source code: https://github.com/cran/gitdown
* Date/Publication: 2021-07-22 07:50:02 UTC
* Number of recursive dependencies: 52

Run `cloud_details(, "gitdown")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/gitdown/new/gitdown.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘gitdown/DESCRIPTION’ ... OK
* this is package ‘gitdown’ version ‘0.1.3’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'bookdown', 'rmarkdown'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/gitdown/old/gitdown.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘gitdown/DESCRIPTION’ ... OK
* this is package ‘gitdown’ version ‘0.1.3’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'bookdown', 'rmarkdown'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# glmmfields

<details>

* Version: 0.1.4
* GitHub: https://github.com/seananderson/glmmfields
* Source code: https://github.com/cran/glmmfields
* Date/Publication: 2020-07-09 05:50:03 UTC
* Number of recursive dependencies: 93

Run `cloud_details(, "glmmfields")` for more info

</details>

## In both

*   checking whether package ‘glmmfields’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/glmmfields/new/glmmfields.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘glmmfields’ ...
** package ‘glmmfields’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
"/opt/R/4.0.3/lib/R/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/glmmfields.stan
Wrote C++ file "stan_files/glmmfields.cc"


g++ -std=gnu++14 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG -I"../inst/include" -I"/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I'/opt/R/4.0.3/lib/R/site-library/BH/include' -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppEigen/include' -I'/opt/R/4.0.3/lib/R/site-library/rstan/include' -I'/opt/R/4.0.3/lib/R/site-library/StanHeaders/include' -I/usr/local/include   -fpic  -g -O2  -c stan_files/glmmfields.cc -o stan_files/glmmfields.o
In file included from /opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/Core:397,
...
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/Product.h:132:22:   required from ‘Eigen::internal::dense_product_base<Lhs, Rhs, Option, 6>::operator const Scalar() const [with Lhs = Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>; Rhs = Eigen::Matrix<double, -1, 1>; int Option = 0; Eigen::internal::dense_product_base<Lhs, Rhs, Option, 6>::Scalar = double]’
/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:23:56:   required from ‘double stan::mcmc::dense_e_metric<Model, BaseRNG>::T(stan::mcmc::dense_e_point&) [with Model = model_glmmfields_namespace::model_glmmfields; BaseRNG = boost::random::additive_combine_engine<boost::random::linear_congruential_engine<unsigned int, 40014, 0, 2147483563>, boost::random::linear_congruential_engine<unsigned int, 40692, 0, 2147483399> >]’
/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:22:10:   required from here
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:55:30: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__vector(2) double’} [-Wignored-attributes]
g++: fatal error: Killed signal terminated program cc1plus
compilation terminated.
make: *** [/opt/R/4.0.3/lib/R/etc/Makeconf:179: stan_files/glmmfields.o] Error 1
rm stan_files/glmmfields.cc
ERROR: compilation failed for package ‘glmmfields’
* removing ‘/tmp/workdir/glmmfields/new/glmmfields.Rcheck/glmmfields’


```
### CRAN

```
* installing *source* package ‘glmmfields’ ...
** package ‘glmmfields’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
"/opt/R/4.0.3/lib/R/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/glmmfields.stan
Wrote C++ file "stan_files/glmmfields.cc"


g++ -std=gnu++14 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG -I"../inst/include" -I"/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I'/opt/R/4.0.3/lib/R/site-library/BH/include' -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppEigen/include' -I'/opt/R/4.0.3/lib/R/site-library/rstan/include' -I'/opt/R/4.0.3/lib/R/site-library/StanHeaders/include' -I/usr/local/include   -fpic  -g -O2  -c stan_files/glmmfields.cc -o stan_files/glmmfields.o
In file included from /opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/Core:397,
...
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/Product.h:132:22:   required from ‘Eigen::internal::dense_product_base<Lhs, Rhs, Option, 6>::operator const Scalar() const [with Lhs = Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>; Rhs = Eigen::Matrix<double, -1, 1>; int Option = 0; Eigen::internal::dense_product_base<Lhs, Rhs, Option, 6>::Scalar = double]’
/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:23:56:   required from ‘double stan::mcmc::dense_e_metric<Model, BaseRNG>::T(stan::mcmc::dense_e_point&) [with Model = model_glmmfields_namespace::model_glmmfields; BaseRNG = boost::random::additive_combine_engine<boost::random::linear_congruential_engine<unsigned int, 40014, 0, 2147483563>, boost::random::linear_congruential_engine<unsigned int, 40692, 0, 2147483399> >]’
/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:22:10:   required from here
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:55:30: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__vector(2) double’} [-Wignored-attributes]
g++: fatal error: Killed signal terminated program cc1plus
compilation terminated.
make: *** [/opt/R/4.0.3/lib/R/etc/Makeconf:179: stan_files/glmmfields.o] Error 1
rm stan_files/glmmfields.cc
ERROR: compilation failed for package ‘glmmfields’
* removing ‘/tmp/workdir/glmmfields/old/glmmfields.Rcheck/glmmfields’


```
# gtfs2gps

<details>

* Version: 1.5-4
* GitHub: https://github.com/ipeaGIT/gtfs2gps
* Source code: https://github.com/cran/gtfs2gps
* Date/Publication: 2021-09-06 08:00:07 UTC
* Number of recursive dependencies: 91

Run `cloud_details(, "gtfs2gps")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```
* using log directory ‘/tmp/workdir/gtfs2gps/old/gtfs2gps.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘gtfs2gps/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘gtfs2gps’ version ‘1.5-4’
* package encoding: UTF-8
* checking package namespace information ... OK
...
  Error: package or namespace load failed for ‘utils’:
   .onLoad failed in loadNamespace() for 'utils', details:
    call: switch(os, Linux = if (file.exists("/etc/os-release")) {
    error: EXPR must be a length 1 vector
  Killed
  Error: package or namespace load failed for ‘stats’ in get(Info[i, 1], envir = env):
   read failed on /opt/R/4.0.3/lib/R/library/stats/R/stats.rdb
  During startup - Warning message:
  package ‘stats’ in options("defaultPackages") was not found 
* checking for unstated dependencies in vignettes ...





```
# idiogramFISH

<details>

* Version: 2.0.6
* GitHub: NA
* Source code: https://github.com/cran/idiogramFISH
* Date/Publication: 2021-09-30 12:00:02 UTC
* Number of recursive dependencies: 190

Run `cloud_details(, "idiogramFISH")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/idiogramFISH/new/idiogramFISH.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘idiogramFISH/DESCRIPTION’ ... OK
* this is package ‘idiogramFISH’ version ‘2.0.6’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... NOTE
...
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘AVignette.Rmd’ using ‘UTF-8’... OK
  ‘index.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 1 NOTE





```
### CRAN

```
* using log directory ‘/tmp/workdir/idiogramFISH/old/idiogramFISH.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘idiogramFISH/DESCRIPTION’ ... OK
* this is package ‘idiogramFISH’ version ‘2.0.6’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... NOTE
...
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘AVignette.Rmd’ using ‘UTF-8’... OK
  ‘index.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 1 NOTE





```
# isotracer

<details>

* Version: 1.0.4
* GitHub: NA
* Source code: https://github.com/cran/isotracer
* Date/Publication: 2021-09-27 08:50:02 UTC
* Number of recursive dependencies: 145

Run `cloud_details(, "isotracer")` for more info

</details>

## In both

*   checking whether package ‘isotracer’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/isotracer/new/isotracer.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘isotracer’ ...
** package ‘isotracer’ successfully unpacked and MD5 sums checked
** using staged installation
** libs


g++ -std=gnu++14 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG -I"../inst/include" -I"/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I'/opt/R/4.0.3/lib/R/site-library/BH/include' -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppEigen/include' -I'/opt/R/4.0.3/lib/R/site-library/StanHeaders/include' -I'/opt/R/4.0.3/lib/R/site-library/rstan/include' -I/usr/local/include   -I'/opt/R/4.0.3/lib/R/site-library/RcppParallel/include' -D_REENTRANT -DSTAN_THREADS   -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
In file included from /opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/Core:397,
                 from /opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/Dense:1,
                 from /opt/R/4.0.3/lib/R/site-library/RcppEigen/include/RcppEigenForward.h:30,
...
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/ProductEvaluators.h:35:90:   required from ‘Eigen::internal::evaluator<Eigen::Product<Lhs, Rhs, Option> >::evaluator(const XprType&) [with Lhs = Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>; Rhs = Eigen::Matrix<double, -1, 1>; int Options = 0; Eigen::internal::evaluator<Eigen::Product<Lhs, Rhs, Option> >::XprType = Eigen::Product<Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>, Eigen::Matrix<double, -1, 1>, 0>]’
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/Product.h:132:22:   required from ‘Eigen::internal::dense_product_base<Lhs, Rhs, Option, 6>::operator const Scalar() const [with Lhs = Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>; Rhs = Eigen::Matrix<double, -1, 1>; int Option = 0; Eigen::internal::dense_product_base<Lhs, Rhs, Option, 6>::Scalar = double]’
/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:23:56:   required from ‘double stan::mcmc::dense_e_metric<Model, BaseRNG>::T(stan::mcmc::dense_e_point&) [with Model = model_networkModelMugen_namespace::model_networkModelMugen; BaseRNG = boost::random::additive_combine_engine<boost::random::linear_congruential_engine<unsigned int, 40014, 0, 2147483563>, boost::random::linear_congruential_engine<unsigned int, 40692, 0, 2147483399> >]’
/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:22:10:   required from here
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:55:30: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__vector(2) double’} [-Wignored-attributes]
g++: fatal error: Killed signal terminated program cc1plus
compilation terminated.
make: *** [/opt/R/4.0.3/lib/R/etc/Makeconf:179: stanExports_networkModelMugen.o] Error 1
ERROR: compilation failed for package ‘isotracer’
* removing ‘/tmp/workdir/isotracer/new/isotracer.Rcheck/isotracer’


```
### CRAN

```
* installing *source* package ‘isotracer’ ...
** package ‘isotracer’ successfully unpacked and MD5 sums checked
** using staged installation
** libs


g++ -std=gnu++14 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG -I"../inst/include" -I"/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I'/opt/R/4.0.3/lib/R/site-library/BH/include' -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppEigen/include' -I'/opt/R/4.0.3/lib/R/site-library/StanHeaders/include' -I'/opt/R/4.0.3/lib/R/site-library/rstan/include' -I/usr/local/include   -I'/opt/R/4.0.3/lib/R/site-library/RcppParallel/include' -D_REENTRANT -DSTAN_THREADS   -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
In file included from /opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/Core:397,
                 from /opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/Dense:1,
                 from /opt/R/4.0.3/lib/R/site-library/RcppEigen/include/RcppEigenForward.h:30,
...
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/ProductEvaluators.h:35:90:   required from ‘Eigen::internal::evaluator<Eigen::Product<Lhs, Rhs, Option> >::evaluator(const XprType&) [with Lhs = Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>; Rhs = Eigen::Matrix<double, -1, 1>; int Options = 0; Eigen::internal::evaluator<Eigen::Product<Lhs, Rhs, Option> >::XprType = Eigen::Product<Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>, Eigen::Matrix<double, -1, 1>, 0>]’
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/Product.h:132:22:   required from ‘Eigen::internal::dense_product_base<Lhs, Rhs, Option, 6>::operator const Scalar() const [with Lhs = Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>; Rhs = Eigen::Matrix<double, -1, 1>; int Option = 0; Eigen::internal::dense_product_base<Lhs, Rhs, Option, 6>::Scalar = double]’
/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:23:56:   required from ‘double stan::mcmc::dense_e_metric<Model, BaseRNG>::T(stan::mcmc::dense_e_point&) [with Model = model_networkModelMugen_namespace::model_networkModelMugen; BaseRNG = boost::random::additive_combine_engine<boost::random::linear_congruential_engine<unsigned int, 40014, 0, 2147483563>, boost::random::linear_congruential_engine<unsigned int, 40692, 0, 2147483399> >]’
/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:22:10:   required from here
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:55:30: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__vector(2) double’} [-Wignored-attributes]
g++: fatal error: Killed signal terminated program cc1plus
compilation terminated.
make: *** [/opt/R/4.0.3/lib/R/etc/Makeconf:179: stanExports_networkModelMugen.o] Error 1
ERROR: compilation failed for package ‘isotracer’
* removing ‘/tmp/workdir/isotracer/old/isotracer.Rcheck/isotracer’


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
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
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
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
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
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
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
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
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
# matsbyname

<details>

* Version: 0.4.25
* GitHub: https://github.com/MatthewHeun/matsbyname
* Source code: https://github.com/cran/matsbyname
* Date/Publication: 2021-10-12 20:20:02 UTC
* Number of recursive dependencies: 92

Run `cloud_details(, "matsbyname")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/matsbyname/new/matsbyname.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘matsbyname/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘matsbyname’ version ‘0.4.25’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘Hmisc’

Package suggested but not available for checking: ‘matsindf’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/matsbyname/old/matsbyname.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘matsbyname/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘matsbyname’ version ‘0.4.25’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘Hmisc’

Package suggested but not available for checking: ‘matsindf’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# metacoder

<details>

* Version: 0.3.5
* GitHub: https://github.com/grunwaldlab/metacoder
* Source code: https://github.com/cran/metacoder
* Date/Publication: 2021-06-23 22:00:02 UTC
* Number of recursive dependencies: 167

Run `cloud_details(, "metacoder")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/metacoder/new/metacoder.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘metacoder/DESCRIPTION’ ... OK
* this is package ‘metacoder’ version ‘0.3.5’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘taxize’

Package suggested but not available for checking: ‘traits’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/metacoder/old/metacoder.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘metacoder/DESCRIPTION’ ... OK
* this is package ‘metacoder’ version ‘0.3.5’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘taxize’

Package suggested but not available for checking: ‘traits’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# multinma

<details>

* Version: 0.3.0
* GitHub: https://github.com/dmphillippo/multinma
* Source code: https://github.com/cran/multinma
* Date/Publication: 2021-03-18 14:00:03 UTC
* Number of recursive dependencies: 134

Run `cloud_details(, "multinma")` for more info

</details>

## In both

*   checking whether package ‘multinma’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/multinma/new/multinma.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘multinma’ ...
** package ‘multinma’ successfully unpacked and MD5 sums checked
** using staged installation
DIAGNOSTIC(S) FROM PARSER:
Info: integer division implicitly rounds to integer. Found int division: nint / int_thin
 Positive values rounded down, negative values rounded up or down in platform-dependent way.

DIAGNOSTIC(S) FROM PARSER:
Info: integer division implicitly rounds to integer. Found int division: nint / int_thin
 Positive values rounded down, negative values rounded up or down in platform-dependent way.
...
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/ProductEvaluators.h:35:90:   required from ‘Eigen::internal::evaluator<Eigen::Product<Lhs, Rhs, Option> >::evaluator(const XprType&) [with Lhs = Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>; Rhs = Eigen::Matrix<double, -1, 1>; int Options = 0; Eigen::internal::evaluator<Eigen::Product<Lhs, Rhs, Option> >::XprType = Eigen::Product<Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>, Eigen::Matrix<double, -1, 1>, 0>]’
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/Product.h:132:22:   required from ‘Eigen::internal::dense_product_base<Lhs, Rhs, Option, 6>::operator const Scalar() const [with Lhs = Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>; Rhs = Eigen::Matrix<double, -1, 1>; int Option = 0; Eigen::internal::dense_product_base<Lhs, Rhs, Option, 6>::Scalar = double]’
/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:23:56:   required from ‘double stan::mcmc::dense_e_metric<Model, BaseRNG>::T(stan::mcmc::dense_e_point&) [with Model = model_binomial_1par_namespace::model_binomial_1par; BaseRNG = boost::random::additive_combine_engine<boost::random::linear_congruential_engine<unsigned int, 40014, 0, 2147483563>, boost::random::linear_congruential_engine<unsigned int, 40692, 0, 2147483399> >]’
/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:22:10:   required from here
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:55:30: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__vector(2) double’} [-Wignored-attributes]
g++: fatal error: Killed signal terminated program cc1plus
compilation terminated.
make: *** [/opt/R/4.0.3/lib/R/etc/Makeconf:179: stanExports_binomial_1par.o] Error 1
ERROR: compilation failed for package ‘multinma’
* removing ‘/tmp/workdir/multinma/new/multinma.Rcheck/multinma’


```
### CRAN

```
* installing *source* package ‘multinma’ ...
** package ‘multinma’ successfully unpacked and MD5 sums checked
** using staged installation
DIAGNOSTIC(S) FROM PARSER:
Info: integer division implicitly rounds to integer. Found int division: nint / int_thin
 Positive values rounded down, negative values rounded up or down in platform-dependent way.

DIAGNOSTIC(S) FROM PARSER:
Info: integer division implicitly rounds to integer. Found int division: nint / int_thin
 Positive values rounded down, negative values rounded up or down in platform-dependent way.
...
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/ProductEvaluators.h:35:90:   required from ‘Eigen::internal::evaluator<Eigen::Product<Lhs, Rhs, Option> >::evaluator(const XprType&) [with Lhs = Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>; Rhs = Eigen::Matrix<double, -1, 1>; int Options = 0; Eigen::internal::evaluator<Eigen::Product<Lhs, Rhs, Option> >::XprType = Eigen::Product<Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>, Eigen::Matrix<double, -1, 1>, 0>]’
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/Product.h:132:22:   required from ‘Eigen::internal::dense_product_base<Lhs, Rhs, Option, 6>::operator const Scalar() const [with Lhs = Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>; Rhs = Eigen::Matrix<double, -1, 1>; int Option = 0; Eigen::internal::dense_product_base<Lhs, Rhs, Option, 6>::Scalar = double]’
/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:23:56:   required from ‘double stan::mcmc::dense_e_metric<Model, BaseRNG>::T(stan::mcmc::dense_e_point&) [with Model = model_binomial_1par_namespace::model_binomial_1par; BaseRNG = boost::random::additive_combine_engine<boost::random::linear_congruential_engine<unsigned int, 40014, 0, 2147483563>, boost::random::linear_congruential_engine<unsigned int, 40692, 0, 2147483399> >]’
/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:22:10:   required from here
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:55:30: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__vector(2) double’} [-Wignored-attributes]
g++: fatal error: Killed signal terminated program cc1plus
compilation terminated.
make: *** [/opt/R/4.0.3/lib/R/etc/Makeconf:179: stanExports_binomial_1par.o] Error 1
ERROR: compilation failed for package ‘multinma’
* removing ‘/tmp/workdir/multinma/old/multinma.Rcheck/multinma’


```
# myTAI

<details>

* Version: 0.9.3
* GitHub: https://github.com/drostlab/myTAI
* Source code: https://github.com/cran/myTAI
* Date/Publication: 2021-02-24 05:40:02 UTC
* Number of recursive dependencies: 161

Run `cloud_details(, "myTAI")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/myTAI/new/myTAI.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘myTAI/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘myTAI’ version ‘0.9.3’
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'taxize', 'edgeR'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/myTAI/old/myTAI.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘myTAI/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘myTAI’ version ‘0.9.3’
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'taxize', 'edgeR'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# nodeSub

<details>

* Version: 1.2
* GitHub: https://github.com/thijsjanzen/nodeSub
* Source code: https://github.com/cran/nodeSub
* Date/Publication: 2021-09-30 08:30:02 UTC
* Number of recursive dependencies: 236

Run `cloud_details(, "nodeSub")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/nodeSub/new/nodeSub.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘nodeSub/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘nodeSub’ version ‘1.2’
* package encoding: UTF-8
* checking package namespace information ... OK
...
Packages required but not available:
  'RPANDA', 'phangorn', 'DDD', 'geiger', 'babette', 'beastier',
  'mauricer'

Packages suggested but not available for checking: 'phytools', 'TreeSim'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/nodeSub/old/nodeSub.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘nodeSub/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘nodeSub’ version ‘1.2’
* package encoding: UTF-8
* checking package namespace information ... OK
...
Packages required but not available:
  'RPANDA', 'phangorn', 'DDD', 'geiger', 'babette', 'beastier',
  'mauricer'

Packages suggested but not available for checking: 'phytools', 'TreeSim'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# occCite

<details>

* Version: 0.5.1
* GitHub: https://github.com/hannahlowens/occCite
* Source code: https://github.com/cran/occCite
* Date/Publication: 2021-11-01 16:00:05 UTC
* Number of recursive dependencies: 171

Run `cloud_details(, "occCite")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/occCite/new/occCite.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘occCite/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘occCite’ version ‘0.5.1’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘taxize’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/occCite/old/occCite.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘occCite/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘occCite’ version ‘0.5.1’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘taxize’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# pencal

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/pencal
* Date/Publication: 2021-09-03 11:10:02 UTC
* Number of recursive dependencies: 153

Run `cloud_details(, "pencal")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/pencal/new/pencal.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘pencal/DESCRIPTION’ ... OK
* this is package ‘pencal’ version ‘1.0.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘survcomp’

Package suggested but not available for checking: ‘ptmixed’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/pencal/old/pencal.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘pencal/DESCRIPTION’ ... OK
* this is package ‘pencal’ version ‘1.0.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘survcomp’

Package suggested but not available for checking: ‘ptmixed’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# phyr

<details>

* Version: 1.1.0
* GitHub: https://github.com/daijiang/phyr
* Source code: https://github.com/cran/phyr
* Date/Publication: 2020-12-18 07:30:05 UTC
* Number of recursive dependencies: 185

Run `cloud_details(, "phyr")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/phyr/new/phyr.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘phyr/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘phyr’ version ‘1.1.0’
* package encoding: UTF-8
* checking package namespace information ... OK
...
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘benchmarks.Rmd’ using ‘UTF-8’... OK
  ‘pglmm.Rmd’ using ‘UTF-8’... OK
  ‘plot-re.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 2 NOTEs





```
### CRAN

```
* using log directory ‘/tmp/workdir/phyr/old/phyr.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘phyr/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘phyr’ version ‘1.1.0’
* package encoding: UTF-8
* checking package namespace information ... OK
...
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘benchmarks.Rmd’ using ‘UTF-8’... OK
  ‘pglmm.Rmd’ using ‘UTF-8’... OK
  ‘plot-re.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 2 NOTEs





```
# Platypus

<details>

* Version: 3.2.3
* GitHub: NA
* Source code: https://github.com/cran/Platypus
* Date/Publication: 2021-10-19 07:00:02 UTC
* Number of recursive dependencies: 241

Run `cloud_details(, "Platypus")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/Platypus/new/Platypus.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘Platypus/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘Platypus’ version ‘3.2.3’
* package encoding: UTF-8
* checking package namespace information ... OK
...
Packages required but not available: 'BiocGenerics', 'Biostrings'

Packages suggested but not available for checking:
  'AnnotationDbi', 'fgsea', 'IRanges', 'limma', 'org.Mm.eg.db',
  'phytools', 'SummarizedExperiment'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/Platypus/old/Platypus.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘Platypus/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘Platypus’ version ‘3.2.3’
* package encoding: UTF-8
* checking package namespace information ... OK
...
Packages required but not available: 'BiocGenerics', 'Biostrings'

Packages suggested but not available for checking:
  'AnnotationDbi', 'fgsea', 'IRanges', 'limma', 'org.Mm.eg.db',
  'phytools', 'SummarizedExperiment'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# pmc

<details>

* Version: 1.0.4
* GitHub: https://github.com/cboettig/pmc
* Source code: https://github.com/cran/pmc
* Date/Publication: 2021-01-07 18:10:12 UTC
* Number of recursive dependencies: 94

Run `cloud_details(, "pmc")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/pmc/new/pmc.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘pmc/DESCRIPTION’ ... OK
* this is package ‘pmc’ version ‘1.0.4’
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘geiger’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/pmc/old/pmc.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘pmc/DESCRIPTION’ ... OK
* this is package ‘pmc’ version ‘1.0.4’
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘geiger’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# Rarefy

<details>

* Version: 1.1
* GitHub: NA
* Source code: https://github.com/cran/Rarefy
* Date/Publication: 2021-03-11 23:10:09 UTC
* Number of recursive dependencies: 203

Run `cloud_details(, "Rarefy")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/Rarefy/new/Rarefy.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘Rarefy/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘Rarefy’ version ‘1.1’
* package encoding: UTF-8
* checking package namespace information ... OK
...
* checking package dependencies ... ERROR
Packages required but not available: 'adiv', 'geiger'

Packages suggested but not available for checking:
  'phyloregion', 'phytools'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/Rarefy/old/Rarefy.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘Rarefy/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘Rarefy’ version ‘1.1’
* package encoding: UTF-8
* checking package namespace information ... OK
...
* checking package dependencies ... ERROR
Packages required but not available: 'adiv', 'geiger'

Packages suggested but not available for checking:
  'phyloregion', 'phytools'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# raw

<details>

* Version: 0.1.8
* GitHub: NA
* Source code: https://github.com/cran/raw
* Date/Publication: 2021-02-05 15:40:03 UTC
* Number of recursive dependencies: 183

Run `cloud_details(, "raw")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/raw/new/raw.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘raw/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘raw’ version ‘0.1.8’
* package encoding: UTF-8
* checking package namespace information ... OK
...
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘raw.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 1 NOTE





```
### CRAN

```
* using log directory ‘/tmp/workdir/raw/old/raw.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘raw/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘raw’ version ‘0.1.8’
* package encoding: UTF-8
* checking package namespace information ... OK
...
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘raw.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 1 NOTE





```
# rbison

<details>

* Version: 1.0.0
* GitHub: https://github.com/ropensci/rbison
* Source code: https://github.com/cran/rbison
* Date/Publication: 2020-06-08 18:30:03 UTC
* Number of recursive dependencies: 138

Run `cloud_details(, "rbison")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/rbison/new/rbison.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘rbison/DESCRIPTION’ ... OK
* this is package ‘rbison’ version ‘1.0.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... NOTE
...
* checking tests ... OK
  Running ‘test-all.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘other_functions.Rmd’ using ‘UTF-8’... OK
  ‘rbison.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 1 NOTE





```
### CRAN

```
* using log directory ‘/tmp/workdir/rbison/old/rbison.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘rbison/DESCRIPTION’ ... OK
* this is package ‘rbison’ version ‘1.0.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... NOTE
...
* checking tests ... OK
  Running ‘test-all.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘other_functions.Rmd’ using ‘UTF-8’... OK
  ‘rbison.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 1 NOTE





```
# RevGadgets

<details>

* Version: 1.0.0
* GitHub: https://github.com/cmt2/RevGadgets
* Source code: https://github.com/cran/RevGadgets
* Date/Publication: 2021-09-13 11:40:04 UTC
* Number of recursive dependencies: 112

Run `cloud_details(, "RevGadgets")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/RevGadgets/new/RevGadgets.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘RevGadgets/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘RevGadgets’ version ‘1.0.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'phytools', 'ggtree'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/RevGadgets/old/RevGadgets.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘RevGadgets/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘RevGadgets’ version ‘1.0.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'phytools', 'ggtree'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# rmdcev

<details>

* Version: 1.2.4
* GitHub: https://github.com/plloydsmith/rmdcev
* Source code: https://github.com/cran/rmdcev
* Date/Publication: 2020-09-30 18:40:02 UTC
* Number of recursive dependencies: 82

Run `cloud_details(, "rmdcev")` for more info

</details>

## In both

*   checking whether package ‘rmdcev’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/rmdcev/new/rmdcev.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘rmdcev’ ...
** package ‘rmdcev’ successfully unpacked and MD5 sums checked
** using staged installation
** libs


g++ -std=gnu++14 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG -I"../inst/include" -I"/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I'/opt/R/4.0.3/lib/R/site-library/BH/include' -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppEigen/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppParallel/include' -I'/opt/R/4.0.3/lib/R/site-library/rstan/include' -I'/opt/R/4.0.3/lib/R/site-library/StanHeaders/include' -I/usr/local/include   -I'/opt/R/4.0.3/lib/R/site-library/RcppParallel/include' -D_REENTRANT -DSTAN_THREADS   -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
In file included from /opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/Core:397,
                 from /opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/Dense:1,
                 from /opt/R/4.0.3/lib/R/site-library/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13,
...
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/ProductEvaluators.h:35:90:   required from ‘Eigen::internal::evaluator<Eigen::Product<Lhs, Rhs, Option> >::evaluator(const XprType&) [with Lhs = Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>; Rhs = Eigen::Matrix<double, -1, 1>; int Options = 0; Eigen::internal::evaluator<Eigen::Product<Lhs, Rhs, Option> >::XprType = Eigen::Product<Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>, Eigen::Matrix<double, -1, 1>, 0>]’
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/Product.h:132:22:   required from ‘Eigen::internal::dense_product_base<Lhs, Rhs, Option, 6>::operator const Scalar() const [with Lhs = Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>; Rhs = Eigen::Matrix<double, -1, 1>; int Option = 0; Eigen::internal::dense_product_base<Lhs, Rhs, Option, 6>::Scalar = double]’
/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:23:56:   required from ‘double stan::mcmc::dense_e_metric<Model, BaseRNG>::T(stan::mcmc::dense_e_point&) [with Model = model_mdcev_namespace::model_mdcev; BaseRNG = boost::random::additive_combine_engine<boost::random::linear_congruential_engine<unsigned int, 40014, 0, 2147483563>, boost::random::linear_congruential_engine<unsigned int, 40692, 0, 2147483399> >]’
/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:22:10:   required from here
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:55:30: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__vector(2) double’} [-Wignored-attributes]
g++: fatal error: Killed signal terminated program cc1plus
compilation terminated.
make: *** [/opt/R/4.0.3/lib/R/etc/Makeconf:179: stanExports_mdcev.o] Error 1
ERROR: compilation failed for package ‘rmdcev’
* removing ‘/tmp/workdir/rmdcev/new/rmdcev.Rcheck/rmdcev’


```
### CRAN

```
* installing *source* package ‘rmdcev’ ...
** package ‘rmdcev’ successfully unpacked and MD5 sums checked
** using staged installation
** libs


g++ -std=gnu++14 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG -I"../inst/include" -I"/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I'/opt/R/4.0.3/lib/R/site-library/BH/include' -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppEigen/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppParallel/include' -I'/opt/R/4.0.3/lib/R/site-library/rstan/include' -I'/opt/R/4.0.3/lib/R/site-library/StanHeaders/include' -I/usr/local/include   -I'/opt/R/4.0.3/lib/R/site-library/RcppParallel/include' -D_REENTRANT -DSTAN_THREADS   -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
In file included from /opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/Core:397,
                 from /opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/Dense:1,
                 from /opt/R/4.0.3/lib/R/site-library/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13,
...
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/ProductEvaluators.h:35:90:   required from ‘Eigen::internal::evaluator<Eigen::Product<Lhs, Rhs, Option> >::evaluator(const XprType&) [with Lhs = Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>; Rhs = Eigen::Matrix<double, -1, 1>; int Options = 0; Eigen::internal::evaluator<Eigen::Product<Lhs, Rhs, Option> >::XprType = Eigen::Product<Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>, Eigen::Matrix<double, -1, 1>, 0>]’
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/Product.h:132:22:   required from ‘Eigen::internal::dense_product_base<Lhs, Rhs, Option, 6>::operator const Scalar() const [with Lhs = Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>; Rhs = Eigen::Matrix<double, -1, 1>; int Option = 0; Eigen::internal::dense_product_base<Lhs, Rhs, Option, 6>::Scalar = double]’
/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:23:56:   required from ‘double stan::mcmc::dense_e_metric<Model, BaseRNG>::T(stan::mcmc::dense_e_point&) [with Model = model_mdcev_namespace::model_mdcev; BaseRNG = boost::random::additive_combine_engine<boost::random::linear_congruential_engine<unsigned int, 40014, 0, 2147483563>, boost::random::linear_congruential_engine<unsigned int, 40692, 0, 2147483399> >]’
/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:22:10:   required from here
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:55:30: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__vector(2) double’} [-Wignored-attributes]
g++: fatal error: Killed signal terminated program cc1plus
compilation terminated.
make: *** [/opt/R/4.0.3/lib/R/etc/Makeconf:179: stanExports_mdcev.o] Error 1
ERROR: compilation failed for package ‘rmdcev’
* removing ‘/tmp/workdir/rmdcev/old/rmdcev.Rcheck/rmdcev’


```
# RNeXML

<details>

* Version: 2.4.5
* GitHub: https://github.com/ropensci/RNeXML
* Source code: https://github.com/cran/RNeXML
* Date/Publication: 2020-06-18 18:40:02 UTC
* Number of recursive dependencies: 138

Run `cloud_details(, "RNeXML")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/RNeXML/new/RNeXML.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘RNeXML/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘RNeXML’ version ‘2.4.5’
* package encoding: UTF-8
* checking package namespace information ... OK
...
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘S4.Rmd’ using ‘UTF-8’... OK
  ‘intro.Rmd’ using ‘UTF-8’... OK
  ‘metadata.Rmd’ using ‘UTF-8’... OK
  ‘simmap.Rmd’ using ‘UTF-8’... OK
  ‘sparql.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 1 ERROR, 1 WARNING, 1 NOTE





```
### CRAN

```
* using log directory ‘/tmp/workdir/RNeXML/old/RNeXML.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘RNeXML/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘RNeXML’ version ‘2.4.5’
* package encoding: UTF-8
* checking package namespace information ... OK
...
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘S4.Rmd’ using ‘UTF-8’... OK
  ‘intro.Rmd’ using ‘UTF-8’... OK
  ‘metadata.Rmd’ using ‘UTF-8’... OK
  ‘simmap.Rmd’ using ‘UTF-8’... OK
  ‘sparql.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 1 ERROR, 1 NOTE





```
# rnoaa

<details>

* Version: 1.3.4
* GitHub: https://github.com/ropensci/rnoaa
* Source code: https://github.com/cran/rnoaa
* Date/Publication: 2021-05-19 07:10:03 UTC
* Number of recursive dependencies: 154

Run `cloud_details(, "rnoaa")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/rnoaa/new/rnoaa.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘rnoaa/DESCRIPTION’ ... OK
* this is package ‘rnoaa’ version ‘1.3.4’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... NOTE
...
* checking for unstated dependencies in examples ... OK
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* DONE
Status: 1 NOTE





```
### CRAN

```
* using log directory ‘/tmp/workdir/rnoaa/old/rnoaa.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘rnoaa/DESCRIPTION’ ... OK
* this is package ‘rnoaa’ version ‘1.3.4’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... NOTE
...
* checking for unstated dependencies in examples ... OK
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* DONE
Status: 1 NOTE





```
# scoper

<details>

* Version: 1.2.0
* GitHub: NA
* Source code: https://github.com/cran/scoper
* Date/Publication: 2021-11-02 21:30:02 UTC
* Number of recursive dependencies: 119

Run `cloud_details(, "scoper")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/scoper/new/scoper.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘scoper/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘scoper’ version ‘1.2.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'alakazam', 'shazam'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/scoper/old/scoper.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘scoper/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘scoper’ version ‘1.2.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'alakazam', 'shazam'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# SPLICE

<details>

* Version: 1.0.0
* GitHub: https://github.com/agi-lab/SPLICE
* Source code: https://github.com/cran/SPLICE
* Date/Publication: 2021-09-13 08:40:06 UTC
* Number of recursive dependencies: 117

Run `cloud_details(, "SPLICE")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/SPLICE/new/SPLICE.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘SPLICE/DESCRIPTION’ ... OK
* this is package ‘SPLICE’ version ‘1.0.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... NOTE
...
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘SPLICE-demo.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 1 NOTE





```
### CRAN

```
* using log directory ‘/tmp/workdir/SPLICE/old/SPLICE.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘SPLICE/DESCRIPTION’ ... OK
* this is package ‘SPLICE’ version ‘1.0.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... NOTE
...
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘SPLICE-demo.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 1 NOTE





```
# SynthETIC

<details>

* Version: 1.0.2
* GitHub: https://github.com/agi-lab/SynthETIC
* Source code: https://github.com/cran/SynthETIC
* Date/Publication: 2021-09-02 23:00:13 UTC
* Number of recursive dependencies: 116

Run `cloud_details(, "SynthETIC")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/SynthETIC/new/SynthETIC.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘SynthETIC/DESCRIPTION’ ... OK
* this is package ‘SynthETIC’ version ‘1.0.2’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... NOTE
...
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘SynthETIC-demo.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 1 NOTE





```
### CRAN

```
* using log directory ‘/tmp/workdir/SynthETIC/old/SynthETIC.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘SynthETIC/DESCRIPTION’ ... OK
* this is package ‘SynthETIC’ version ‘1.0.2’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... NOTE
...
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘SynthETIC-demo.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 1 NOTE





```
# taxizedb

<details>

* Version: 0.3.0
* GitHub: https://github.com/ropensci/taxizedb
* Source code: https://github.com/cran/taxizedb
* Date/Publication: 2021-01-15 06:00:06 UTC
* Number of recursive dependencies: 137

Run `cloud_details(, "taxizedb")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/taxizedb/new/taxizedb.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘taxizedb/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘taxizedb’ version ‘0.3.0’
* package encoding: UTF-8
* checking package namespace information ... OK
...
Running the tests in ‘tests/test-all.R’ failed.
Complete output:
  > library(testthat)
  > test_check("taxizedb")
  Loading required package: taxizedb
  Error in loadNamespace(name) : there is no package called 'taxize'
  Calls: test_check ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
  Execution halted
* DONE
Status: 1 ERROR, 1 NOTE





```
### CRAN

```
* using log directory ‘/tmp/workdir/taxizedb/old/taxizedb.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘taxizedb/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘taxizedb’ version ‘0.3.0’
* package encoding: UTF-8
* checking package namespace information ... OK
...
Running the tests in ‘tests/test-all.R’ failed.
Complete output:
  > library(testthat)
  > test_check("taxizedb")
  Loading required package: taxizedb
  Error in loadNamespace(name) : there is no package called 'taxize'
  Calls: test_check ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
  Execution halted
* DONE
Status: 1 ERROR, 1 NOTE





```
# tigger

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/tigger
* Date/Publication: 2020-05-13 05:10:03 UTC
* Number of recursive dependencies: 120

Run `cloud_details(, "tigger")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/tigger/new/tigger.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘tigger/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘tigger’ version ‘1.0.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'alakazam', 'shazam'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/tigger/old/tigger.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘tigger/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘tigger’ version ‘1.0.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'alakazam', 'shazam'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# tinyarray

<details>

* Version: 2.2.7
* GitHub: https://github.com/xjsun1221/tinyarray
* Source code: https://github.com/cran/tinyarray
* Date/Publication: 2021-11-08 10:00:02 UTC
* Number of recursive dependencies: 231

Run `cloud_details(, "tinyarray")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/tinyarray/new/tinyarray.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘tinyarray/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘tinyarray’ version ‘2.2.7’
* package encoding: UTF-8
* checking package namespace information ... OK
...
Packages required but not available:
  'clusterProfiler', 'limma', 'org.Hs.eg.db'

Packages suggested but not available for checking:
  'AnnoProbe', 'GEOquery', 'Biobase', 'ComplexHeatmap'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/tinyarray/old/tinyarray.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘tinyarray/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘tinyarray’ version ‘2.2.7’
* package encoding: UTF-8
* checking package namespace information ... OK
...
Packages required but not available:
  'clusterProfiler', 'limma', 'org.Hs.eg.db'

Packages suggested but not available for checking:
  'AnnoProbe', 'GEOquery', 'Biobase', 'ComplexHeatmap'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# TR8

<details>

* Version: 0.9.22
* GitHub: https://github.com/GioBo/TR8
* Source code: https://github.com/cran/TR8
* Date/Publication: 2020-12-01 19:10:02 UTC
* Number of recursive dependencies: 135

Run `cloud_details(, "TR8")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/TR8/new/TR8.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘TR8/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘TR8’ version ‘0.9.22’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘taxize’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/TR8/old/TR8.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘TR8/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘TR8’ version ‘0.9.22’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘taxize’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# trackr

<details>

* Version: 0.10.7
* GitHub: NA
* Source code: https://github.com/cran/trackr
* Date/Publication: 2021-05-24 14:50:02 UTC
* Number of recursive dependencies: 99

Run `cloud_details(, "trackr")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/trackr/new/trackr.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘trackr/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘trackr’ version ‘0.10.7’
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available:
  'histry', 'CodeDepends', 'rsolr', 'roprov'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/trackr/old/trackr.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘trackr/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘trackr’ version ‘0.10.7’
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available:
  'histry', 'CodeDepends', 'rsolr', 'roprov'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# traits

<details>

* Version: 0.5.0
* GitHub: https://github.com/ropensci/traits
* Source code: https://github.com/cran/traits
* Date/Publication: 2020-08-27 05:10:06 UTC
* Number of recursive dependencies: 135

Run `cloud_details(, "traits")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/traits/new/traits.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘traits/DESCRIPTION’ ... OK
* this is package ‘traits’ version ‘0.5.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘taxize’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/traits/old/traits.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘traits/DESCRIPTION’ ... OK
* this is package ‘traits’ version ‘0.5.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘taxize’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# treeplyr

<details>

* Version: 0.1.10
* GitHub: https://github.com/uyedaj/treeplyr
* Source code: https://github.com/cran/treeplyr
* Date/Publication: 2020-09-17 10:10:02 UTC
* Number of recursive dependencies: 44

Run `cloud_details(, "treeplyr")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/treeplyr/new/treeplyr.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘treeplyr/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘treeplyr’ version ‘0.1.10’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'phytools', 'geiger'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/treeplyr/old/treeplyr.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘treeplyr/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘treeplyr’ version ‘0.1.10’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'phytools', 'geiger'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# vinereg

<details>

* Version: 0.7.4
* GitHub: https://github.com/tnagler/vinereg
* Source code: https://github.com/cran/vinereg
* Date/Publication: 2021-05-14 18:40:03 UTC
* Number of recursive dependencies: 126

Run `cloud_details(, "vinereg")` for more info

</details>

## In both

*   checking whether package ‘vinereg’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/vinereg/new/vinereg.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘vinereg’ ...
** package ‘vinereg’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
g++ -std=gnu++11 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG -I../inst/include -pthread -I'/opt/R/4.0.3/lib/R/site-library/rvinecopulib/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppEigen/include' -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I'/opt/R/4.0.3/lib/R/site-library/BH/include' -I'/opt/R/4.0.3/lib/R/site-library/wdm/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppThread/include' -I'/opt/R/4.0.3/lib/R/site-library/kde1d/include' -I/usr/local/include   -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
In file included from /opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/Core:397,
                 from /opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/Dense:1,
                 from /opt/R/4.0.3/lib/R/site-library/RcppEigen/include/RcppEigenForward.h:30,
                 from /opt/R/4.0.3/lib/R/site-library/RcppEigen/include/RcppEigen.h:25,
                 from RcppExports.cpp:4:
...
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/Matrix.h:225:24:   required from ‘Eigen::Matrix<_Scalar, _Rows, _Cols, _Options, _MaxRows, _MaxCols>& Eigen::Matrix<_Scalar, _Rows, _Cols, _Options, _MaxRows, _MaxCols>::operator=(const Eigen::DenseBase<OtherDerived>&) [with OtherDerived = Eigen::Inverse<Eigen::CwiseBinaryOp<Eigen::internal::scalar_difference_op<double, double>, const Eigen::Product<Eigen::Product<Eigen::Matrix<double, 2, 2>, Eigen::Product<Eigen::Transpose<Eigen::Matrix<double, -1, -1> >, Eigen::Matrix<double, -1, -1>, 0>, 0>, Eigen::Matrix<double, 2, 2>, 0>, const Eigen::Product<Eigen::Matrix<double, 2, 1>, Eigen::Transpose<Eigen::Matrix<double, 2, 1> >, 0> > >; _Scalar = double; int _Rows = 2; int _Cols = 2; int _Options = 0; int _MaxRows = 2; int _MaxCols = 2]’
/opt/R/4.0.3/lib/R/site-library/rvinecopulib/include/vinecopulib/bicop/implementation/tll.ipp:121:74:   required from here
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:650:34: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__vector(2) double’} [-Wignored-attributes]
  650 |   return internal::first_aligned<int(unpacket_traits<DefaultPacketType>::alignment),Derived>(m);
      |                                  ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
g++: fatal error: Killed signal terminated program cc1plus
compilation terminated.
make: *** [/opt/R/4.0.3/lib/R/etc/Makeconf:181: vinereg.o] Error 1
ERROR: compilation failed for package ‘vinereg’
* removing ‘/tmp/workdir/vinereg/new/vinereg.Rcheck/vinereg’


```
### CRAN

```
* installing *source* package ‘vinereg’ ...
** package ‘vinereg’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
g++ -std=gnu++11 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG -I../inst/include -pthread -I'/opt/R/4.0.3/lib/R/site-library/rvinecopulib/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppEigen/include' -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I'/opt/R/4.0.3/lib/R/site-library/BH/include' -I'/opt/R/4.0.3/lib/R/site-library/wdm/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppThread/include' -I'/opt/R/4.0.3/lib/R/site-library/kde1d/include' -I/usr/local/include   -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
In file included from /opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/Core:397,
                 from /opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/Dense:1,
                 from /opt/R/4.0.3/lib/R/site-library/RcppEigen/include/RcppEigenForward.h:30,
                 from /opt/R/4.0.3/lib/R/site-library/RcppEigen/include/RcppEigen.h:25,
                 from RcppExports.cpp:4:
...
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:650:34: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__vector(2) double’} [-Wignored-attributes]
  650 |   return internal::first_aligned<int(unpacket_traits<DefaultPacketType>::alignment),Derived>(m);
      |                                  ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
vinereg.cpp:309:1: fatal error: error writing to /tmp/ccaoG8xu.s: Cannot allocate memory
  309 | }
      | ^
compilation terminated.
make: *** [/opt/R/4.0.3/lib/R/etc/Makeconf:181: vinereg.o] Error 1
ERROR: compilation failed for package ‘vinereg’
* removing ‘/tmp/workdir/vinereg/old/vinereg.Rcheck/vinereg’


```
# vivid

<details>

* Version: 0.2.2
* GitHub: NA
* Source code: https://github.com/cran/vivid
* Date/Publication: 2021-10-12 13:50:02 UTC
* Number of recursive dependencies: 202

Run `cloud_details(, "vivid")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/vivid/new/vivid.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘vivid/DESCRIPTION’ ... OK
* this is package ‘vivid’ version ‘0.2.2’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... NOTE
...
  Error: Test failures
  Execution halted
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘vivid.Rmd’ using ‘UTF-8’... OK
  ‘vividQStart.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 1 ERROR, 2 NOTEs





```
### CRAN

```
* using log directory ‘/tmp/workdir/vivid/old/vivid.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘vivid/DESCRIPTION’ ... OK
* this is package ‘vivid’ version ‘0.2.2’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... NOTE
...
  Error: Test failures
  Execution halted
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘vivid.Rmd’ using ‘UTF-8’... OK
  ‘vividQStart.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 1 ERROR, 2 NOTEs





```
# wrswoR

<details>

* Version: 1.1.1
* GitHub: https://github.com/krlmlr/wrswoR
* Source code: https://github.com/cran/wrswoR
* Date/Publication: 2020-07-26 18:20:02 UTC
* Number of recursive dependencies: 159

Run `cloud_details(, "wrswoR")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/wrswoR/new/wrswoR.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘wrswoR/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘wrswoR’ version ‘1.1.1’
* package encoding: UTF-8
* checking package namespace information ... OK
...
* checking for GNU extensions in Makefiles ... OK
* checking for portable use of $(BLAS_LIBS) and $(LAPACK_LIBS) ... OK
* checking use of PKG_*FLAGS in Makefiles ... OK
* checking compiled code ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* DONE
Status: 1 NOTE





```
### CRAN

```
* using log directory ‘/tmp/workdir/wrswoR/old/wrswoR.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘wrswoR/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘wrswoR’ version ‘1.1.1’
* package encoding: UTF-8
* checking package namespace information ... OK
...
* checking for GNU extensions in Makefiles ... OK
* checking for portable use of $(BLAS_LIBS) and $(LAPACK_LIBS) ... OK
* checking use of PKG_*FLAGS in Makefiles ... OK
* checking compiled code ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* DONE
Status: 1 NOTE





```
