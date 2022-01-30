# aba

<details>

* Version: 0.0.9
* GitHub: https://github.com/ncullen93/abaR
* Source code: https://github.com/cran/aba
* Date/Publication: 2021-12-16 20:50:05 UTC
* Number of recursive dependencies: 157

Run `cloud_details(, "aba")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘aba-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: aba_longpower
    > ### Title: Run power analysis on a longitudinal-based aba model.
    > ### Aliases: aba_longpower
    > 
    > ### ** Examples
    > 
    > 
    ...
    +     power = c(0.8, 0.85, 0.9),
    +     t_length = 2,
    +     t_freq = 0.25,
    +     dropout = 0.2
    +   )
    > 
    > # generate a standard results figure from the power results
    > fig <- pwr %>% aba_plot()
    Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        4. │     ├─testthat .capture(...)
        5. │     │ └─base::withCallingHandlers(...)
        6. │     └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
        7. ├─model_summary %>% ...
        8. ├─aba::aba_plot_coef(...)
        9. └─base::loadNamespace(x)
       10.   ├─base::namespaceImportFrom(...)
       11.   │ └─base::asNamespace(ns)
       12.   └─base::loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]])
       13.     └─base::namespaceImportFrom(...)
       14.       └─base::importIntoEnv(impenv, impnames, ns, impvars)
      
      [ FAIL 8 | WARN 0 | SKIP 2 | PASS 131 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘aba.Rmd’ using rmarkdown
    --- finished re-building ‘aba.Rmd’
    
    --- re-building ‘intro_to_aba_models.Rmd’ using rmarkdown
    Quitting from lines 90-91 (intro_to_aba_models.Rmd) 
    Error: processing vignette 'intro_to_aba_models.Rmd' failed with diagnostics:
    object 'as_data_frame' is not exported by 'namespace:dplyr'
    --- failed re-building ‘intro_to_aba_models.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘intro_to_aba_models.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# abjutils

<details>

* Version: 0.3.1
* GitHub: https://github.com/abjur/abjutils
* Source code: https://github.com/cran/abjutils
* Date/Publication: 2020-08-12 22:50:05 UTC
* Number of recursive dependencies: 39

Run `cloud_details(, "abjutils")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘abjutils-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: sample_cnj
    > ### Title: Generate sample Brazilian lawsuit identification numbers
    > ### Aliases: sample_cnj
    > 
    > ### ** Examples
    > 
    > {
    ...
    +   # not sampling the parameters
    + 
    +   sample_cnj(3,
    +     foros = c("0000", "0001", "0002"),
    +     anos = c("2014", "2015", "2016"), orgao = rep(8, 3), tr = rep(26, 3),
    +     first_dig = "0", sample_pars = FALSE, return_df = FALSE
    +   )
    + }
    Error: 'data_frame' is not an exported object from 'namespace:dplyr'
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Backtrace:
          ▆
       1. ├─testthat::expect_equal(...) at test-sample_cnj.R:4:2
       2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
       3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
       4. ├─base::nrow(...)
       5. ├─abjutils::sample_cnj(...)
       6. │ └─... %>% dplyr::select(n_processo)
       7. ├─dplyr::select(., n_processo)
       8. ├─dplyr::mutate(...)
       9. └─stats::setNames(., "serial")
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 11 ]
      Error: Test failures
      Execution halted
    ```

*   checking dependencies in R code ... WARNING
    ```
    Missing or unexported objects:
      ‘dplyr::as_data_frame’ ‘dplyr::data_frame’
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘httr’
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# ActFrag

<details>

* Version: 0.1.1
* GitHub: https://github.com/junruidi/ActFrag
* Source code: https://github.com/cran/ActFrag
* Date/Publication: 2020-02-11 19:00:10 UTC
* Number of recursive dependencies: 93

Run `cloud_details(, "ActFrag")` for more info

</details>

## Newly broken

*   checking whether package ‘ActFrag’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/ActFrag/new/ActFrag.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tidyr’
      All declared Imports should be used.
    ```

## Installation

### Devel

```
* installing *source* package ‘ActFrag’ ...
** package ‘ActFrag’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘ActFrag’
* removing ‘/tmp/workdir/ActFrag/new/ActFrag.Rcheck/ActFrag’


```
### CRAN

```
* installing *source* package ‘ActFrag’ ...
** package ‘ActFrag’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (ActFrag)


```
# actogrammr

<details>

* Version: 0.2.3
* GitHub: NA
* Source code: https://github.com/cran/actogrammr
* Date/Publication: 2017-10-25 17:24:10 UTC
* Number of recursive dependencies: 72

Run `cloud_details(, "actogrammr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘actogrammr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: bin_data
    > ### Title: bin_data
    > ### Aliases: bin_data
    > 
    > ### ** Examples
    > 
    > f <- file.path(system.file(package = 'actogrammr'), 'testdata')
    > d <- read_clock_lab_files(file_names = list.files(path = f, full.names = TRUE))
    Error: 'as_data_frame' is not an exported object from 'namespace:dplyr'
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error (test_read_clock_lab.R:7:13): read_clock_lab ──────────────────────────
      Error: 'as_data_frame' is not an exported object from 'namespace:dplyr'
      Backtrace:
          ▆
       1. ├─actogrammr::read_clock_lab_files(...) at test_read_clock_lab.R:7:12
       2. │ └─actogrammr:::read_clock_lab_file(file_name = file_name)
       3. │   └─... %>% return()
       4. ├─dplyr::mutate(., min = as.integer(min), light = long_light$light)
       5. ├─dplyr::select(., -trash)
       6. └─tidyr::separate(., col = min, into = c("trash", "min"))
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

*   checking dependencies in R code ... WARNING
    ```
    Missing or unexported object: ‘dplyr::as_data_frame’
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# afex

<details>

* Version: 1.0-1
* GitHub: https://github.com/singmann/afex
* Source code: https://github.com/cran/afex
* Date/Publication: 2021-07-22 04:40:18 UTC
* Number of recursive dependencies: 213

Run `cloud_details(, "afex")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘afex_analysing_accuracy_data.Rmd’ using rmarkdown
    Loading required package: lme4
    Loading required package: Matrix
    ************
    Welcome to afex. For support visit: http://afex.singmann.science/
    - Functions for ANOVAs: aov_car(), aov_ez(), and aov_4()
    - Methods for calculating p-values with mixed(): 'S', 'KR', 'LRT', and 'PB'
    - 'afex_aov' and 'mixed' objects can be passed to emmeans() for follow-up tests
    - NEWS: emmeans() for ANOVA models now uses model = 'multivariate' as default.
    ...
    --- finished re-building ‘assumptions_of_ANOVAs.Rmd’
    
    --- re-building ‘introduction-mixed-models.pdf.asis’ using asis
    --- finished re-building ‘introduction-mixed-models.pdf.asis’
    
    SUMMARY: processing the following file failed:
      ‘afex_plot_introduction.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# AnnoProbe

<details>

* Version: 0.1.6
* GitHub: https://github.com/jmzeng1314/AnnoProbe
* Source code: https://github.com/cran/AnnoProbe
* Date/Publication: 2021-07-12 07:40:08 UTC
* Number of recursive dependencies: 124

Run `cloud_details(, "AnnoProbe")` for more info

</details>

## Newly broken

*   checking whether package ‘AnnoProbe’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/AnnoProbe/new/AnnoProbe.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘AnnoProbe’ ...
** package ‘AnnoProbe’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘AnnoProbe’
* removing ‘/tmp/workdir/AnnoProbe/new/AnnoProbe.Rcheck/AnnoProbe’


```
### CRAN

```
* installing *source* package ‘AnnoProbe’ ...
** package ‘AnnoProbe’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (AnnoProbe)


```
# apache.sedona

<details>

* Version: 1.1.1
* GitHub: NA
* Source code: https://github.com/cran/apache.sedona
* Date/Publication: 2021-11-23 19:40:02 UTC
* Number of recursive dependencies: 58

Run `cloud_details(, "apache.sedona")` for more info

</details>

## Newly broken

*   checking whether package ‘apache.sedona’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/apache.sedona/new/apache.sedona.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘DBI’ ‘dplyr’
      All declared Imports should be used.
    ```

## Installation

### Devel

```
* installing *source* package ‘apache.sedona’ ...
** package ‘apache.sedona’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error: object ‘type_sum’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘apache.sedona’
* removing ‘/tmp/workdir/apache.sedona/new/apache.sedona.Rcheck/apache.sedona’


```
### CRAN

```
* installing *source* package ‘apache.sedona’ ...
** package ‘apache.sedona’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (apache.sedona)


```
# APCI

<details>

* Version: 1.0.5
* GitHub: NA
* Source code: https://github.com/cran/APCI
* Date/Publication: 2021-12-05 15:40:06 UTC
* Number of recursive dependencies: 93

Run `cloud_details(, "APCI")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘APCI-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: apci.plot.raw
    > ### Title: plot the raw scores
    > ### Aliases: apci.plot.raw
    > 
    > ### ** Examples
    > 
    > # plot the raw scores
    > apci.plot.raw(data = simulation, outcome_var = "y",
    +               age = "age", period = "period")
    Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
    Execution halted
    ```

# APCtools

<details>

* Version: 1.0.1
* GitHub: https://github.com/bauer-alex/APCtools
* Source code: https://github.com/cran/APCtools
* Date/Publication: 2022-01-11 10:22:51 UTC
* Number of recursive dependencies: 103

Run `cloud_details(, "APCtools")` for more info

</details>

## Newly broken

*   checking whether package ‘APCtools’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/APCtools/new/APCtools.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘APCtools’ ...
** package ‘APCtools’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘APCtools’
* removing ‘/tmp/workdir/APCtools/new/APCtools.Rcheck/APCtools’


```
### CRAN

```
* installing *source* package ‘APCtools’ ...
** package ‘APCtools’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
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
* DONE (APCtools)


```
# autoReg

<details>

* Version: 0.1.0
* GitHub: https://github.com/cardiomoon/autoReg
* Source code: https://github.com/cran/autoReg
* Date/Publication: 2022-01-10 18:12:47 UTC
* Number of recursive dependencies: 139

Run `cloud_details(, "autoReg")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Automatic_Regression_Modeling.Rmd’ using rmarkdown
    --- finished re-building ‘Automatic_Regression_Modeling.Rmd’
    
    --- re-building ‘Bootstrap_Prediction.Rmd’ using rmarkdown
    --- finished re-building ‘Bootstrap_Prediction.Rmd’
    
    --- re-building ‘Getting_started.Rmd’ using rmarkdown
    --- finished re-building ‘Getting_started.Rmd’
    
    ...
    Quitting from lines 103-105 (Survival.Rmd) 
    Error: processing vignette 'Survival.Rmd' failed with diagnostics:
    package 'ggpubr' could not be loaded
    --- failed re-building ‘Survival.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Survival.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# autostats

<details>

* Version: 0.2.0
* GitHub: https://github.com/Harrison4192/autostats
* Source code: https://github.com/cran/autostats
* Date/Publication: 2021-12-10 09:30:02 UTC
* Number of recursive dependencies: 214

Run `cloud_details(, "autostats")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘autostats.Rmd’ using rmarkdown
    Quitting from lines 41-43 (autostats.Rmd) 
    Error: processing vignette 'autostats.Rmd' failed with diagnostics:
    package or namespace load failed for 'tune':
     object 'type_sum' is not exported by 'namespace:dplyr'
    --- failed re-building ‘autostats.Rmd’
    
    --- re-building ‘tidyModels.Rmd’ using rmarkdown
    
    ...
    Error: processing vignette 'tidyModels.Rmd' failed with diagnostics:
    package or namespace load failed for 'tune':
     object 'type_sum' is not exported by 'namespace:dplyr'
    --- failed re-building ‘tidyModels.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘autostats.Rmd’ ‘tidyModels.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘autostats-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: get_params
    > ### Title: get params
    > ### Aliases: get_params get_params.xgb.Booster
    > 
    > ### ** Examples
    > 
    > 
    ...
    > 
    > iris %>%
    + tidy_xgboost(p_form, mtry = .5, trees = 5L, loss_reduction = 2, sample_size = .7) -> xgb
    Warning: `early_stop` was reduced to 4.
    > 
    > ## reuse these parameters to find the cross validated error
    > 
    > rlang::exec(auto_model_accuracy, data = iris, formula = p_form, !!!get_params(xgb))
    Error: object ‘type_sum’ is not exported by 'namespace:dplyr'
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘BBmisc’ ‘Ckmeans.1d.dp’ ‘Matrix’ ‘broom.mixed’ ‘ggstance’ ‘glmnet’
      ‘hardhat’
      All declared Imports should be used.
    ```

# baguette

<details>

* Version: 0.1.1
* GitHub: https://github.com/tidymodels/baguette
* Source code: https://github.com/cran/baguette
* Date/Publication: 2021-07-14 17:00:05 UTC
* Number of recursive dependencies: 133

Run `cloud_details(, "baguette")` for more info

</details>

## Newly broken

*   checking whether package ‘baguette’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/baguette/new/baguette.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘baguette’ ...
** package ‘baguette’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘type_sum’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘baguette’
* removing ‘/tmp/workdir/baguette/new/baguette.Rcheck/baguette’


```
### CRAN

```
* installing *source* package ‘baguette’ ...
** package ‘baguette’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (baguette)


```
# bayMDS

<details>

* Version: 1.6
* GitHub: NA
* Source code: https://github.com/cran/bayMDS
* Date/Publication: 2021-12-16 10:00:06 UTC
* Number of recursive dependencies: 112

Run `cloud_details(, "bayMDS")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘bayMDS-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plotObj
    > ### Title: plot object configuration
    > ### Aliases: plotObj
    > 
    > ### ** Examples
    > 
    > data(cityDIST)
    > result <- bmdsMCMC(cityDIST,p=3,nwarm=500,niter=3000)
    > plotObj(result)
    Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘gridExtra’ ‘rgl’
      All declared Imports should be used.
    ```

# bdl

<details>

* Version: 1.0.3
* GitHub: https://github.com/statisticspoland/R_Package_to_API_BDL
* Source code: https://github.com/cran/bdl
* Date/Publication: 2021-03-02 15:10:10 UTC
* Number of recursive dependencies: 137

Run `cloud_details(, "bdl")` for more info

</details>

## Newly broken

*   checking whether package ‘bdl’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/bdl/new/bdl.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

## Installation

### Devel

```
* installing *source* package ‘bdl’ ...
** package ‘bdl’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘bdl’
* removing ‘/tmp/workdir/bdl/new/bdl.Rcheck/bdl’


```
### CRAN

```
* installing *source* package ‘bdl’ ...
** package ‘bdl’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (bdl)


```
# benthos

<details>

* Version: 1.3-6
* GitHub: NA
* Source code: https://github.com/cran/benthos
* Date/Publication: 2019-03-17 22:43:20 UTC
* Number of recursive dependencies: 74

Run `cloud_details(, "benthos")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘benthos-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ambi
    > ### Title: AZTI Marine Biotic Index (AMBI)
    > ### Aliases: ambi ambi_ has_ambi has_ambi_
    > 
    > ### ** Examples
    > 
    >  ambi(
    +      taxon = c("Euspira pulchella", "Nephtys cirrosa"), 
    +      count = c(4, 6)
    +  )
    Error in data_frame(TAXON = taxon, COUNT = count) : 
      could not find function "data_frame"
    Calls: ambi -> ambi_
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘test-all.R’
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
       1. ├─testthat::expect_that(...) at test-poole.R:92:12
       2. │ └─testthat condition(object)
       3. │   └─testthat::expect_false(x)
       4. │     └─testthat::quasi_label(enquo(object), label, arg = "object")
       5. │       └─rlang::eval_bare(expr, quo_get_env(quo))
       6. └─benthos::pool(sample_id = sample_id, area = area, target_area = target_area)
      ── Error (test-poole.R:107:13): areas of pools are in target interval. ─────────
      Error in `data_frame(sample_id, area)`: could not find function "data_frame"
      Backtrace:
          ▆
       1. └─benthos::pool(sample_id = sample_id, area = area, target_area = target_area) at test-poole.R:107:12
      
      [ FAIL 9 | WARN 11 | SKIP 1 | PASS 110 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘benthos.Rmd’ using rmarkdown
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    ...
    ℹ The error occurred in group 1: HABITAT = "Polyhaline-Intertidal", YEAR = "2010".
    Caused by error in `data_frame()`:
    ! could not find function "data_frame"
    --- failed re-building ‘benthos.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘benthos.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

*   checking R code for possible problems ... NOTE
    ```
    .validate_groups: no visible binding for global variable
      ‘as_data_frame’
    ambi_: no visible global function definition for ‘data_frame’
    iti_: no visible global function definition for ‘data_frame’
    pool: no visible global function definition for ‘data_frame’
    validate_beqi2: no visible binding for global variable ‘as_data_frame’
    validate_ref: no visible binding for global variable ‘as_data_frame’
    validate_taxa: no visible binding for global variable ‘as_data_frame’
    validate_twn: no visible binding for global variable ‘as_data_frame’
    Undefined global functions or variables:
      as_data_frame data_frame
    ```

# bib2df

<details>

* Version: 1.1.1
* GitHub: https://github.com/ropensci/bib2df
* Source code: https://github.com/cran/bib2df
* Date/Publication: 2019-05-22 22:15:15 UTC
* Number of recursive dependencies: 72

Run `cloud_details(, "bib2df")` for more info

</details>

## Newly broken

*   checking whether package ‘bib2df’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/bib2df/new/bib2df.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

## Installation

### Devel

```
* installing *source* package ‘bib2df’ ...
** package ‘bib2df’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘bib2df’
* removing ‘/tmp/workdir/bib2df/new/bib2df.Rcheck/bib2df’


```
### CRAN

```
* installing *source* package ‘bib2df’ ...
** package ‘bib2df’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (bib2df)


```
# bibliometrix

<details>

* Version: 3.1.4
* GitHub: https://github.com/massimoaria/bibliometrix
* Source code: https://github.com/cran/bibliometrix
* Date/Publication: 2021-07-05 14:40:05 UTC
* Number of recursive dependencies: 161

Run `cloud_details(, "bibliometrix")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘bibliometrix-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: conceptualStructure
    > ### Title: Creating and plotting conceptual structure map of a scientific
    > ###   field
    > ### Aliases: conceptualStructure
    > 
    > ### ** Examples
    > 
    > # EXAMPLE Conceptual Structure using Keywords Plus
    > 
    > data(scientometrics, package = "bibliometrixData")
    > 
    > CS <- conceptualStructure(scientometrics, field="ID", method="CA", 
    +              stemming=FALSE, minDegree=3, k.max = 5)
    Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
    Execution halted
    ```

# bkmr

<details>

* Version: 0.2.0
* GitHub: https://github.com/jenfb/bkmr
* Source code: https://github.com/cran/bkmr
* Date/Publication: 2017-03-24 19:03:01 UTC
* Number of recursive dependencies: 49

Run `cloud_details(, "bkmr")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... WARNING
    ```
    Missing or unexported object: ‘dplyr::data_frame’
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# bkmrhat

<details>

* Version: 1.1.1
* GitHub: NA
* Source code: https://github.com/cran/bkmrhat
* Date/Publication: 2021-09-07 19:10:05 UTC
* Number of recursive dependencies: 84

Run `cloud_details(, "bkmrhat")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘test_coda.R’
      Running ‘test_continue.R’
      Running ‘test_diag.R’
      Running ‘test_invisibles.R’
      Running ‘test_parallel.R’
    Running the tests in ‘tests/test_parallel.R’ failed.
    Last 13 lines of output:
      Iteration: 10 (100% completed; 0.03622 secs elapsed)
      Chain 2 
      Iteration: 2 (20% completed; 0.0011 secs elapsed)
    ...
      Iteration: 5 (50% completed; 0.02453 secs elapsed)
      Iteration: 6 (60% completed; 0.02601 secs elapsed)
      Iteration: 7 (70% completed; 0.02743 secs elapsed)
      Iteration: 8 (80% completed; 0.02884 secs elapsed)
      Iteration: 9 (90% completed; 0.03026 secs elapsed)
      Iteration: 10 (100% completed; 0.03167 secs elapsed)
      > PredictorResponseBivar_parallel(fitkm.list)
      Chain 1 
      Error: 'data_frame' is not an exported object from 'namespace:dplyr'
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘bkmrhat-vignette.Rmd’ using knitr
    For guided examples, go to 'https://jenfb.github.io/bkmr/overview.html'
    Loading required package: coda
    Diagnostics and parallel chain functioning for Bayesian kernel machine regression
    Quitting from lines 214-266 (bkmrhat-vignette.Rmd) 
    Error: processing vignette 'bkmrhat-vignette.Rmd' failed with diagnostics:
    'data_frame' is not an exported object from 'namespace:dplyr'
    --- failed re-building ‘bkmrhat-vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘bkmrhat-vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# BNPdensity

<details>

* Version: 2021.5.4
* GitHub: NA
* Source code: https://github.com/cran/BNPdensity
* Date/Publication: 2021-05-13 10:52:11 UTC
* Number of recursive dependencies: 102

Run `cloud_details(, "BNPdensity")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘BNPdensity-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: MixPY1
    > ### Title: Pitman-Yor process mixture of Type I
    > ### Aliases: MixPY1
    > 
    > ### ** Examples
    > 
    > # Data
    > data(acidity)
    > x <- acidity
    > # Fitting the model under default specifications
    > out <- MixPY1(x)
    Error: Package "BNPmix" is needed for this function to work. Please install it.
    Execution halted
    ```

# BNPmix

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/BNPmix
* Date/Publication: 2021-11-23 15:40:02 UTC
* Number of recursive dependencies: 95

Run `cloud_details(, "BNPmix")` for more info

</details>

## Newly broken

*   checking whether package ‘BNPmix’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/BNPmix/new/BNPmix.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘BNPmix.ltx’ using tex
    Error: processing vignette 'BNPmix.ltx' failed with diagnostics:
    Running 'texi2dvi' on 'BNPmix.ltx' failed.
    LaTeX errors:
    ! LaTeX Error: File `ae.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    ...
    l.55 \RequirePackage
                        [T1]{fontenc}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    --- failed re-building ‘BNPmix.ltx’
    
    SUMMARY: processing the following file failed:
      ‘BNPmix.ltx’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 30.0Mb
      sub-directories of 1Mb or more:
        libs  28.8Mb
    ```

## Installation

### Devel

```
* installing *source* package ‘BNPmix’ ...
** package ‘BNPmix’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
gcc -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppDist/include' -I/usr/local/include   -fpic  -g -O2  -c BNPmix_init.c -o BNPmix_init.o
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppDist/include' -I/usr/local/include  -fopenmp -fpic  -g -O2  -c CommonUtilities.cpp -o CommonUtilities.o
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppDist/include' -I/usr/local/include  -fopenmp -fpic  -g -O2  -c DdpFunctions.cpp -o DdpFunctions.o
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppDist/include' -I/usr/local/include  -fopenmp -fpic  -g -O2  -c Distributions.cpp -o Distributions.o
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppDist/include' -I/usr/local/include  -fopenmp -fpic  -g -O2  -c IcsFunctions.cpp -o IcsFunctions.o
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppDist/include' -I/usr/local/include  -fopenmp -fpic  -g -O2  -c MAR.cpp -o MAR.o
...
g++ -std=gnu++11 -shared -L/opt/R/4.1.1/lib/R/lib -L/usr/local/lib -o BNPmix.so BNPmix_init.o CommonUtilities.o DdpFunctions.o Distributions.o IcsFunctions.o MAR.o MarFunctions.o RcppExports.o SliFunctions.o cDDP.o cICS.o cSLI.o est_partition.o -fopenmp -llapack -lblas -lgfortran -lm -lquadmath -L/opt/R/4.1.1/lib/R/lib -lR
installing to /tmp/workdir/BNPmix/new/BNPmix.Rcheck/00LOCK-BNPmix/00new/BNPmix/libs
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘BNPmix’
* removing ‘/tmp/workdir/BNPmix/new/BNPmix.Rcheck/BNPmix’


```
### CRAN

```
* installing *source* package ‘BNPmix’ ...
** package ‘BNPmix’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
gcc -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppDist/include' -I/usr/local/include   -fpic  -g -O2  -c BNPmix_init.c -o BNPmix_init.o
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppDist/include' -I/usr/local/include  -fopenmp -fpic  -g -O2  -c CommonUtilities.cpp -o CommonUtilities.o
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppDist/include' -I/usr/local/include  -fopenmp -fpic  -g -O2  -c DdpFunctions.cpp -o DdpFunctions.o
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppDist/include' -I/usr/local/include  -fopenmp -fpic  -g -O2  -c Distributions.cpp -o Distributions.o
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppDist/include' -I/usr/local/include  -fopenmp -fpic  -g -O2  -c IcsFunctions.cpp -o IcsFunctions.o
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppDist/include' -I/usr/local/include  -fopenmp -fpic  -g -O2  -c MAR.cpp -o MAR.o
...
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** checking absolute paths in shared objects and dynamic libraries
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (BNPmix)


```
# broom

<details>

* Version: 0.7.12
* GitHub: https://github.com/tidymodels/broom
* Source code: https://github.com/cran/broom
* Date/Publication: 2022-01-28 23:30:05 UTC
* Number of recursive dependencies: 282

Run `cloud_details(, "broom")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... WARNING
    ```
    Missing or unexported object: ‘dplyr::data_frame’
    ```

# carpenter

<details>

* Version: 0.2.2
* GitHub: https://github.com/lwjohnst86/carpenter
* Source code: https://github.com/cran/carpenter
* Date/Publication: 2019-02-05 08:43:30 UTC
* Number of recursive dependencies: 60

Run `cloud_details(, "carpenter")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        8. ├─dplyr::mutate(., ValOrder = 1:dplyr::n())
        9. └─dplyr::group_by_at(., "Variables")
       10.   └─dplyr:::manip_at(...)
       11.     └─dplyr:::tbl_at_syms(.tbl, .vars, .include_group_vars = .include_group_vars)
       12.       └─dplyr:::tbl_at_vars(tbl, vars, .include_group_vars = .include_group_vars)
       13.         └─dplyr::tbl_vars(tbl)
       14.           ├─dplyr:::new_sel_vars(tbl_vars_dispatch(x), group_vars(x))
       15.           │ └─base::structure(...)
       16.           └─dplyr:::tbl_vars_dispatch(x)
      ── Error (test-build_tables.R:36:5): build_table outputs in order as determined in data ──
      Error: 'data_frame' is not an exported object from 'namespace:dplyr'
      
      [ FAIL 2 | WARN 2 | SKIP 0 | PASS 19 ]
      Error: Test failures
      Execution halted
    ```

*   checking dependencies in R code ... WARNING
    ```
    Missing or unexported object: ‘dplyr::data_frame’
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘carpenter.Rmd’ using rmarkdown
    Quitting from lines 121-137 (carpenter.Rmd) 
    Error: processing vignette 'carpenter.Rmd' failed with diagnostics:
    'data_frame' is not an exported object from 'namespace:dplyr'
    --- failed re-building ‘carpenter.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘carpenter.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# casen

<details>

* Version: 0.1.4
* GitHub: https://github.com/pachamaltese/casen
* Source code: https://github.com/cran/casen
* Date/Publication: 2020-04-08 06:00:02 UTC
* Number of recursive dependencies: 98

Run `cloud_details(, "casen")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘casen-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: configuracion_disenio
    > ### Title: Dise<c3><b1>o complejo para estadistica descriptiva e inferencia
    > ### Aliases: configuracion_disenio
    > 
    > ### ** Examples
    > 
    > cd <- configuracion_disenio(casen_2017_los_rios, "ytotcorh", c("comuna", "sexo"), "expc")
    ...
    > cd$disenio
    Stratified 1 - level Cluster Sampling design (with replacement)
    With (67) clusters.
    Called via srvyr
    Sampling variables:
     - ids: varunit
     - strata: varstrat
     - weights: expc
    Error: 'type_sum' is not an exported object from 'namespace:dplyr'
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘basic-usage.Rmd’ using rmarkdown
    Quitting from lines 72-76 (basic-usage.Rmd) 
    Error: processing vignette 'basic-usage.Rmd' failed with diagnostics:
    'type_sum' is not an exported object from 'namespace:dplyr'
    --- failed re-building ‘basic-usage.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘basic-usage.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘janitor’ ‘tibble’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 75 marked UTF-8 strings
    ```

# catalog

<details>

* Version: 0.1.0
* GitHub: https://github.com/nathaneastwood/catalog
* Source code: https://github.com/cran/catalog
* Date/Publication: 2021-03-20 11:00:05 UTC
* Number of recursive dependencies: 79

Run `cloud_details(, "catalog")` for more info

</details>

## Newly broken

*   checking whether package ‘catalog’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/catalog/new/catalog.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

## Installation

### Devel

```
* installing *source* package ‘catalog’ ...
** package ‘catalog’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘type_sum’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘catalog’
* removing ‘/tmp/workdir/catalog/new/catalog.Rcheck/catalog’


```
### CRAN

```
* installing *source* package ‘catalog’ ...
** package ‘catalog’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (catalog)


```
# causact

<details>

* Version: 0.4.1
* GitHub: https://github.com/flyaflya/causact
* Source code: https://github.com/cran/causact
* Date/Publication: 2022-01-19 17:22:43 UTC
* Number of recursive dependencies: 86

Run `cloud_details(, "causact")` for more info

</details>

## Newly broken

*   checking whether package ‘causact’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/causact/new/causact.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘causact’ ...
** package ‘causact’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘causact’
* removing ‘/tmp/workdir/causact/new/causact.Rcheck/causact’


```
### CRAN

```
* installing *source* package ‘causact’ ...
** package ‘causact’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (causact)


```
# ccrs

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/ccrs
* Date/Publication: 2019-03-04 17:10:02 UTC
* Number of recursive dependencies: 63

Run `cloud_details(, "ccrs")` for more info

</details>

## Newly broken

*   checking whether package ‘ccrs’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/ccrs/new/ccrs.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘methods’
      All declared Imports should be used.
    ```

## Installation

### Devel

```
* installing *source* package ‘ccrs’ ...
** package ‘ccrs’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘ccrs’
* removing ‘/tmp/workdir/ccrs/new/ccrs.Rcheck/ccrs’


```
### CRAN

```
* installing *source* package ‘ccrs’ ...
** package ‘ccrs’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (ccrs)


```
# cdcfluview

<details>

* Version: 0.9.4
* GitHub: https://github.com/hrbrmstr/cdcfluview
* Source code: https://github.com/cran/cdcfluview
* Date/Publication: 2021-05-22 12:20:02 UTC
* Number of recursive dependencies: 69

Run `cloud_details(, "cdcfluview")` for more info

</details>

## Newly broken

*   checking whether package ‘cdcfluview’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/cdcfluview/new/cdcfluview.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘cdcfluview’ ...
** package ‘cdcfluview’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error: object ‘data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘cdcfluview’
* removing ‘/tmp/workdir/cdcfluview/new/cdcfluview.Rcheck/cdcfluview’


```
### CRAN

```
* installing *source* package ‘cdcfluview’ ...
** package ‘cdcfluview’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (cdcfluview)


```
# CDSeq

<details>

* Version: 1.0.8
* GitHub: https://github.com/kkang7/CDSeq_R_Package
* Source code: https://github.com/cran/CDSeq
* Date/Publication: 2021-02-10 16:10:02 UTC
* Number of recursive dependencies: 186

Run `cloud_details(, "CDSeq")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘CDSeq-vignette.Rmd’ using rmarkdown
    Attaching SeuratObject
    Quitting from lines 78-83 (CDSeq-vignette.Rmd) 
    Error: processing vignette 'CDSeq-vignette.Rmd' failed with diagnostics:
    package or namespace load failed for 'ggpubr':
     object 'as_data_frame' is not exported by 'namespace:dplyr'
    --- failed re-building ‘CDSeq-vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘CDSeq-vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking whether package ‘CDSeq’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘RcppThread::detectCores’ by ‘parallel::detectCores’ when loading ‘CDSeq’
    See ‘/tmp/workdir/CDSeq/new/CDSeq.Rcheck/00install.out’ for details.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.6Mb
      sub-directories of 1Mb or more:
        doc    2.4Mb
        libs   2.8Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘ggpubr’ ‘matrixStats’ ‘qlcMatrix’
      All declared Imports should be used.
    ```

# censusr

<details>

* Version: 0.0.4
* GitHub: https://github.com/transportfoundry/censusr
* Source code: https://github.com/cran/censusr
* Date/Publication: 2018-01-25 16:40:14 UTC
* Number of recursive dependencies: 40

Run `cloud_details(, "censusr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘censusr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: aggregate_moe
    > ### Title: Aggregated margin of error across multiple geographies
    > ### Aliases: aggregate_moe
    > 
    > ### ** Examples
    > 
    > x <- c(3, 5, 12, 4)
    > aggregate_moe(x)
    [1] 13.92839
    > data_frame(x = x, group = c(1, 1, 2, 2)) %>%
    +   group_by(group) %>%
    +   summarise(moe = aggregate_moe(x))
    Error in data_frame(x = x, group = c(1, 1, 2, 2)) : 
      could not find function "data_frame"
    Calls: %>% -> summarise -> group_by
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# chunked

<details>

* Version: 0.5.1
* GitHub: https://github.com/edwindj/chunked
* Source code: https://github.com/cran/chunked
* Date/Publication: 2020-11-03 06:40:19 UTC
* Number of recursive dependencies: 51

Run `cloud_details(, "chunked")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        4. │ └─testthat:::quasi_capture(...)
        5. │   ├─testthat .capture(...)
        6. │   │ └─testthat::capture_output_lines(code, print, width = width)
        7. │   │   └─testthat:::eval_with_output(code, print = print, width = width)
        8. │   │     ├─withr::with_output_sink(path, withVisible(code))
        9. │   │     │ └─base::force(code)
       10. │   │     └─base::withVisible(code)
       11. │   └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
       12. ├─base::print(tbl_iris)
       13. └─chunked:::print.chunkwise(tbl_iris)
       14.   └─base::print(trunc_mat(h, n = n, width = width))
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 43 ]
      Error: Test failures
      Execution halted
    ```

*   checking R code for possible problems ... NOTE
    ```
    print.chunkwise: no visible global function definition for ‘trunc_mat’
    Undefined global functions or variables:
      trunc_mat
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# CINNA

<details>

* Version: 1.1.55
* GitHub: NA
* Source code: https://github.com/cran/CINNA
* Date/Publication: 2021-11-15 12:10:40 UTC
* Number of recursive dependencies: 134

Run `cloud_details(, "CINNA")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘CINNA.Rmd’ using rmarkdown
    
    Attaching package: 'igraph'
    
    The following objects are masked from 'package:stats':
    
        decompose, spectrum
    
    ...
    Quitting from lines 231-234 (CINNA.Rmd) 
    Error: processing vignette 'CINNA.Rmd' failed with diagnostics:
    object 'as_data_frame' is not exported by 'namespace:dplyr'
    --- failed re-building ‘CINNA.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘CINNA.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘circlize’
      All declared Imports should be used.
    ```

# clustEff

<details>

* Version: 0.2.0
* GitHub: NA
* Source code: https://github.com/cran/clustEff
* Date/Publication: 2020-06-08 15:00:15 UTC
* Number of recursive dependencies: 125

Run `cloud_details(, "clustEff")` for more info

</details>

## Newly broken

*   checking whether package ‘clustEff’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/clustEff/new/clustEff.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

## Installation

### Devel

```
* installing *source* package ‘clustEff’ ...
** package ‘clustEff’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘ggpubr’:
 object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘clustEff’
* removing ‘/tmp/workdir/clustEff/new/clustEff.Rcheck/clustEff’


```
### CRAN

```
* installing *source* package ‘clustEff’ ...
** package ‘clustEff’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (clustEff)


```
# cmpsR

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/cmpsR
* Date/Publication: 2021-10-29 08:50:09 UTC
* Number of recursive dependencies: 133

Run `cloud_details(, "cmpsR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘cmpsR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: cmps_segment_plot
    > ### Title: Plot the selected basis segment and its cross-correlation curve
    > ###   at all scales based on the results of CMPS algorithm
    > ### Aliases: cmps_segment_plot
    > 
    > ### ** Examples
    > 
    ...
    ✔ tidyr   1.1.4     ✔ stringr 1.4.0
    ✔ readr   2.1.1     ✔ forcats 0.5.1
    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    > library(cmpsR)
    > library(ggpubr)
    Error: package or namespace load failed for ‘ggpubr’:
     object ‘as_data_frame’ is not exported by 'namespace:dplyr'
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘cmpsR-vignette.Rmd’ using rmarkdown
    Quitting from lines 309-313 (cmpsR-vignette.Rmd) 
    Error: processing vignette 'cmpsR-vignette.Rmd' failed with diagnostics:
    object 'as_data_frame' is not exported by 'namespace:dplyr'
    --- failed re-building ‘cmpsR-vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘cmpsR-vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# coefplot

<details>

* Version: 1.2.8
* GitHub: NA
* Source code: https://github.com/cran/coefplot
* Date/Publication: 2022-01-14 09:42:47 UTC
* Number of recursive dependencies: 103

Run `cloud_details(, "coefplot")` for more info

</details>

## Newly broken

*   checking whether package ‘coefplot’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/coefplot/new/coefplot.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘coefplot’ ...
** package ‘coefplot’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error: object ‘data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘coefplot’
* removing ‘/tmp/workdir/coefplot/new/coefplot.Rcheck/coefplot’


```
### CRAN

```
* installing *source* package ‘coefplot’ ...
** package ‘coefplot’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (coefplot)


```
# concurve

<details>

* Version: 2.7.7
* GitHub: https://github.com/zadrafi/concurve
* Source code: https://github.com/cran/concurve
* Date/Publication: 2020-10-12 17:10:06 UTC
* Number of recursive dependencies: 229

Run `cloud_details(, "concurve")` for more info

</details>

## Newly broken

*   checking whether package ‘concurve’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/concurve/new/concurve.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

## Installation

### Devel

```
* installing *source* package ‘concurve’ ...
** package ‘concurve’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘concurve’
* removing ‘/tmp/workdir/concurve/new/concurve.Rcheck/concurve’


```
### CRAN

```
* installing *source* package ‘concurve’ ...
** package ‘concurve’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (concurve)


```
# CondiS

<details>

* Version: 0.1.1
* GitHub: NA
* Source code: https://github.com/cran/CondiS
* Date/Publication: 2022-01-13 19:32:44 UTC
* Number of recursive dependencies: 177

Run `cloud_details(, "CondiS")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘introduction.Rmd’ using rmarkdown
    
    Attaching package: 'purrr'
    
    The following object is masked from 'package:kernlab':
    
        cross
    
    ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──
    ...
    Quitting from lines 37-74 (introduction.Rmd) 
    Error: processing vignette 'introduction.Rmd' failed with diagnostics:
    package 'ggpubr' could not be loaded
    --- failed re-building ‘introduction.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘introduction.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘kernlab’ ‘purrr’ ‘survminer’ ‘tidyverse’
      All declared Imports should be used.
    ```

# convergEU

<details>

* Version: 0.5.1
* GitHub: NA
* Source code: https://github.com/cran/convergEU
* Date/Publication: 2021-01-09 13:50:06 UTC
* Number of recursive dependencies: 174

Run `cloud_details(, "convergEU")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘convergEU-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: sigma_conv_graph
    > ### Title: Graphical representation based on sigma convergence
    > ### Aliases: sigma_conv_graph
    > 
    > ### ** Examples
    > 
    > 
    > # Example 1
    > # Sigma convergence for the emp_20_64_MS Eurofound dataset in the period 2002-2006:
    > data(emp_20_64_MS)
    > reSigConv <- sigma_conv(emp_20_64_MS, timeName = "time", time_0 = 2002,time_t = 2006)
    > 
    > # Graphical plot based on the results for sigma-convergence
    > reSiggraph<-sigma_conv_graph(reSigConv,2002,2006,aggregation = 'EU27')
    Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘PATTERNS-CONV.Rmd’ using rmarkdown
    --- finished re-building ‘PATTERNS-CONV.Rmd’
    
    --- re-building ‘measuring-convergence.Rmd’ using rmarkdown
    Quitting from lines 823-828 (measuring-convergence.Rmd) 
    Error: processing vignette 'measuring-convergence.Rmd' failed with diagnostics:
    object 'as_data_frame' is not exported by 'namespace:dplyr'
    --- failed re-building ‘measuring-convergence.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘measuring-convergence.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked UTF-8 string
    ```

# corx

<details>

* Version: 1.0.6.1
* GitHub: https://github.com/conig/corx
* Source code: https://github.com/cran/corx
* Date/Publication: 2020-06-30 09:20:17 UTC
* Number of recursive dependencies: 96

Run `cloud_details(, "corx")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── Error (test_matrix.R:383:3):  k = "auto" works ──────────────────────────────
      Error: object 'as_data_frame' is not exported by 'namespace:dplyr'
      Backtrace:
          ▆
       1. ├─corx::plot_mds(corx(mtcars), k = "auto") at test_matrix.R:383:2
       2. └─base::loadNamespace(x)
       3.   ├─base::namespaceImportFrom(...)
       4.   │ └─base::asNamespace(ns)
       5.   └─base::loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]])
       6.     └─base::namespaceImportFrom(...)
       7.       └─base::importIntoEnv(impenv, impnames, ns, impvars)
      
      [ FAIL 2 | WARN 1 | SKIP 0 | PASS 59 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# CovidMutations

<details>

* Version: 0.1.3
* GitHub: https://github.com/MSQ-123/CovidMutations
* Source code: https://github.com/cran/CovidMutations
* Date/Publication: 2020-09-18 12:00:39 UTC
* Number of recursive dependencies: 96

Run `cloud_details(, "CovidMutations")` for more info

</details>

## Newly broken

*   checking whether package ‘CovidMutations’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/CovidMutations/new/CovidMutations.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘CovidMutations’ ...
** package ‘CovidMutations’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘CovidMutations’
* removing ‘/tmp/workdir/CovidMutations/new/CovidMutations.Rcheck/CovidMutations’


```
### CRAN

```
* installing *source* package ‘CovidMutations’ ...
** package ‘CovidMutations’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (CovidMutations)


```
# cpr

<details>

* Version: 0.2.3
* GitHub: https://github.com/dewittpe/cpr
* Source code: https://github.com/cran/cpr
* Date/Publication: 2017-03-07 13:41:34
* Number of recursive dependencies: 87

Run `cloud_details(, "cpr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘cpr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: bsplineD
    > ### Title: B-spline Derivatives
    > ### Aliases: bsplineD
    > 
    > ### ** Examples
    > 
    > 
    ...
    > theta <- runif(length(iknots) + 4L, -5, 5)
    > 
    > # plot data
    > plot_data <-
    +   dplyr::data_frame(x = xvec,
    +                     Spline = as.numeric(bmat %*% theta),
    +                     "First Derivative" = as.numeric(bmat1 %*% theta),
    +                     "Second Derivative" = as.numeric(bmat2 %*% theta))
    Error: 'data_frame' is not an exported object from 'namespace:dplyr'
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error (test-recover-know-spline.R:57:13): A known spline can be recovered. ──
      Error: 'data_frame' is not an exported object from 'namespace:dplyr'
      Backtrace:
          ▆
       1. ├─testthat::expect_true(recover_spline(start_with = 40L, progress = FALSE)$recovered) at test-recover-know-spline.R:57:12
       2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
       3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
       4. └─cpr recover_spline(start_with = 40L, progress = FALSE)
       5.   ├─cpr::cp(true_bmat, true_theta, ...) at test-recover-know-spline.R:17:2
       6.   └─cpr:::cp.cpr_bs(true_bmat, true_theta, ...)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 18 ]
      Error: Test failures
      Execution halted
    ```

*   checking dependencies in R code ... WARNING
    ```
    Missing or unexported objects:
      ‘dplyr::as_data_frame’ ‘dplyr::data_frame’
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘cpr-pkg.Rmd’ using rmarkdown
    Quitting from lines 79-84 (cpr-pkg.Rmd) 
    Error: processing vignette 'cpr-pkg.Rmd' failed with diagnostics:
    'data_frame' is not an exported object from 'namespace:dplyr'
    --- failed re-building ‘cpr-pkg.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘cpr-pkg.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.5Mb
      sub-directories of 1Mb or more:
        libs   5.9Mb
    ```

# crsra

<details>

* Version: 0.2.3
* GitHub: https://github.com/jhudsl/crsra
* Source code: https://github.com/cran/crsra
* Date/Publication: 2018-05-05 06:25:58 UTC
* Number of recursive dependencies: 66

Run `cloud_details(, "crsra")` for more info

</details>

## Newly broken

*   checking whether package ‘crsra’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/crsra/new/crsra.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 500 marked UTF-8 strings
    ```

## Installation

### Devel

```
* installing *source* package ‘crsra’ ...
** package ‘crsra’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘crsra’
* removing ‘/tmp/workdir/crsra/new/crsra.Rcheck/crsra’


```
### CRAN

```
* installing *source* package ‘crsra’ ...
** package ‘crsra’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (crsra)


```
# csa

<details>

* Version: 0.7.0
* GitHub: https://github.com/imarkonis/csa
* Source code: https://github.com/cran/csa
* Date/Publication: 2020-05-16 09:50:09 UTC
* Number of recursive dependencies: 99

Run `cloud_details(, "csa")` for more info

</details>

## Newly broken

*   checking whether package ‘csa’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/csa/new/csa.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘csa’ ...
** package ‘csa’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘csa’
* removing ‘/tmp/workdir/csa/new/csa.Rcheck/csa’


```
### CRAN

```
* installing *source* package ‘csa’ ...
** package ‘csa’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (csa)


```
# cvCovEst

<details>

* Version: 1.0.2
* GitHub: https://github.com/PhilBoileau/cvCovEst
* Source code: https://github.com/cran/cvCovEst
* Date/Publication: 2022-01-19 22:12:45 UTC
* Number of recursive dependencies: 120

Run `cloud_details(, "cvCovEst")` for more info

</details>

## Newly broken

*   checking whether package ‘cvCovEst’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/cvCovEst/new/cvCovEst.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘cvCovEst’ ...
** package ‘cvCovEst’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘cvCovEst’
* removing ‘/tmp/workdir/cvCovEst/new/cvCovEst.Rcheck/cvCovEst’


```
### CRAN

```
* installing *source* package ‘cvCovEst’ ...
** package ‘cvCovEst’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (cvCovEst)


```
# dataquieR

<details>

* Version: 1.0.9
* GitHub: NA
* Source code: https://github.com/cran/dataquieR
* Date/Publication: 2021-09-03 12:10:09 UTC
* Number of recursive dependencies: 134

Run `cloud_details(, "dataquieR")` for more info

</details>

## Newly broken

*   checking whether package ‘dataquieR’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/dataquieR/new/dataquieR.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘dataquieR’ ...
** package ‘dataquieR’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘dataquieR’
* removing ‘/tmp/workdir/dataquieR/new/dataquieR.Rcheck/dataquieR’


```
### CRAN

```
* installing *source* package ‘dataquieR’ ...
** package ‘dataquieR’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (dataquieR)


```
# dfoliatR

<details>

* Version: 0.2.0
* GitHub: https://github.com/chguiterman/dfoliatR
* Source code: https://github.com/cran/dfoliatR
* Date/Publication: 2020-09-02 12:30:06 UTC
* Number of recursive dependencies: 105

Run `cloud_details(, "dfoliatR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘dfoliatR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_outbreak
    > ### Title: Produce a stacked plot to present composited, site-level insect
    > ###   outbreak chronologies
    > ### Aliases: plot_outbreak
    > 
    > ### ** Examples
    > 
    > data(dmj_obr)
    > plot_outbreak(dmj_obr)
    Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
           ▆
        1. ├─vdiffr::expect_doppelganger("Outbreak-Tile-Plot", plot_outbreak(dfoliatR::dmj_obr)) at test-plots.R:15:2
        2. │ └─vdiffr writer(fig, testcase, title)
        3. │   └─vdiffr:::print_plot(plot, title)
        4. ├─dfoliatR::plot_outbreak(dfoliatR::dmj_obr)
        5. └─base::loadNamespace(x)
        6.   ├─base::namespaceImportFrom(...)
        7.   │ └─base::asNamespace(ns)
        8.   └─base::loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]])
        9.     └─base::namespaceImportFrom(...)
       10.       └─base::importIntoEnv(impenv, impnames, ns, impvars)
      
      [ FAIL 1 | WARN 0 | SKIP 2 | PASS 11 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘intro-to-dfoliatR.Rmd’ using rmarkdown
    Quitting from lines 136-137 (intro-to-dfoliatR.Rmd) 
    Error: processing vignette 'intro-to-dfoliatR.Rmd' failed with diagnostics:
    object 'as_data_frame' is not exported by 'namespace:dplyr'
    --- failed re-building ‘intro-to-dfoliatR.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘intro-to-dfoliatR.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘MASS’ ‘plyr’
      All declared Imports should be used.
    ```

# dials

<details>

* Version: 0.0.10
* GitHub: https://github.com/tidymodels/dials
* Source code: https://github.com/cran/dials
* Date/Publication: 2021-09-10 11:50:02 UTC
* Number of recursive dependencies: 67

Run `cloud_details(, "dials")` for more info

</details>

## Newly broken

*   checking whether package ‘dials’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/dials/new/dials.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘discrim’, ‘sda’
    ```

## Installation

### Devel

```
* installing *source* package ‘dials’ ...
** package ‘dials’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘type_sum’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘dials’
* removing ‘/tmp/workdir/dials/new/dials.Rcheck/dials’


```
### CRAN

```
* installing *source* package ‘dials’ ...
** package ‘dials’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
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
* DONE (dials)


```
# did

<details>

* Version: 2.1.1
* GitHub: https://github.com/bcallaway11/did
* Source code: https://github.com/cran/did
* Date/Publication: 2022-01-27 22:00:02 UTC
* Number of recursive dependencies: 119

Run `cloud_details(, "did")` for more info

</details>

## Newly broken

*   checking whether package ‘did’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/did/new/did.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘did’ ...
** package ‘did’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘did’
* removing ‘/tmp/workdir/did/new/did.Rcheck/did’


```
### CRAN

```
* installing *source* package ‘did’ ...
** package ‘did’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
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
* DONE (did)


```
# digitalDLSorteR

<details>

* Version: 0.1.1
* GitHub: https://github.com/diegommcc/digitalDLSorteR
* Source code: https://github.com/cran/digitalDLSorteR
* Date/Publication: 2021-10-19 23:00:02 UTC
* Number of recursive dependencies: 184

Run `cloud_details(, "digitalDLSorteR")` for more info

</details>

## Newly broken

*   checking whether package ‘digitalDLSorteR’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/digitalDLSorteR/new/digitalDLSorteR.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘digitalDLSorteR’ ...
** package ‘digitalDLSorteR’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘digitalDLSorteR’
* removing ‘/tmp/workdir/digitalDLSorteR/new/digitalDLSorteR.Rcheck/digitalDLSorteR’


```
### CRAN

```
* installing *source* package ‘digitalDLSorteR’ ...
** package ‘digitalDLSorteR’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Creating a new generic function for ‘saveRDS’ in package ‘digitalDLSorteR’
** help
*** installing help indices
*** copying figures
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (digitalDLSorteR)


```
# discrim

<details>

* Version: 0.1.3
* GitHub: https://github.com/tidymodels/discrim
* Source code: https://github.com/cran/discrim
* Date/Publication: 2021-07-21 21:10:02 UTC
* Number of recursive dependencies: 137

Run `cloud_details(, "discrim")` for more info

</details>

## Newly broken

*   checking whether package ‘discrim’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/discrim/new/discrim.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘utils’
      All declared Imports should be used.
    ```

## Installation

### Devel

```
* installing *source* package ‘discrim’ ...
** package ‘discrim’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘type_sum’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘discrim’
* removing ‘/tmp/workdir/discrim/new/discrim.Rcheck/discrim’


```
### CRAN

```
* installing *source* package ‘discrim’ ...
** package ‘discrim’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (discrim)


```
# disto

<details>

* Version: 0.2.0
* GitHub: https://github.com/talegari/disto
* Source code: https://github.com/cran/disto
* Date/Publication: 2018-08-02 12:50:02 UTC
* Number of recursive dependencies: 115

Run `cloud_details(, "disto")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘disto-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.disto
    > ### Title: Plot a disto object
    > ### Aliases: plot.disto
    > 
    > ### ** Examples
    > 
    > temp <- stats::dist(iris[,1:4])
    > dio  <- disto(objectname = "temp")
    > plot(dio, type = "heatmap")
    > plot(dio, type = "dendrogram")
    Warning: `guides(<scale> = FALSE)` is deprecated. Please use `guides(<scale> = "none")` instead.
    Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘vignette_disto.Rmd’ using rmarkdown
    Quitting from lines 42-72 (vignette_disto.Rmd) 
    Error: processing vignette 'vignette_disto.Rmd' failed with diagnostics:
    object 'as_data_frame' is not exported by 'namespace:dplyr'
    --- failed re-building ‘vignette_disto.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘vignette_disto.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘proxy’
      All declared Imports should be used.
    ```

# diversityForest

<details>

* Version: 0.3.3
* GitHub: NA
* Source code: https://github.com/cran/diversityForest
* Date/Publication: 2022-01-05 12:00:05 UTC
* Number of recursive dependencies: 117

Run `cloud_details(, "diversityForest")` for more info

</details>

## Newly broken

*   checking whether package ‘diversityForest’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/diversityForest/new/diversityForest.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking installed package size ... NOTE
    ```
      installed size is 20.9Mb
      sub-directories of 1Mb or more:
        libs  20.5Mb
    ```

## Installation

### Devel

```
* installing *source* package ‘diversityForest’ ...
** package ‘diversityForest’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG -DR_BUILD -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppEigen/include' -I/usr/local/include   -fpic  -g -O2  -c AAA_check_cpp11.cpp -o AAA_check_cpp11.o
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG -DR_BUILD -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppEigen/include' -I/usr/local/include   -fpic  -g -O2  -c Data.cpp -o Data.o
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG -DR_BUILD -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppEigen/include' -I/usr/local/include   -fpic  -g -O2  -c DataChar.cpp -o DataChar.o
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG -DR_BUILD -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppEigen/include' -I/usr/local/include   -fpic  -g -O2  -c DataFloat.cpp -o DataFloat.o
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG -DR_BUILD -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppEigen/include' -I/usr/local/include   -fpic  -g -O2  -c DataSparse.cpp -o DataSparse.o
In file included from /opt/R/4.1.1/lib/R/site-library/RcppEigen/include/Eigen/Core:397,
...
g++ -std=gnu++11 -shared -L/opt/R/4.1.1/lib/R/lib -L/usr/local/lib -o diversityForest.so AAA_check_cpp11.o Data.o DataChar.o DataFloat.o DataSparse.o Forest.o ForestClassification.o ForestProbability.o ForestRegression.o ForestSurvival.o RcppExports.o Tree.o TreeClassification.o TreeProbability.o TreeRegression.o TreeSurvival.o divforCpp.o utility.o utilityRcpp.o -L/opt/R/4.1.1/lib/R/lib -lR
installing to /tmp/workdir/diversityForest/new/diversityForest.Rcheck/00LOCK-diversityForest/00new/diversityForest/libs
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘diversityForest’
* removing ‘/tmp/workdir/diversityForest/new/diversityForest.Rcheck/diversityForest’


```
### CRAN

```
* installing *source* package ‘diversityForest’ ...
** package ‘diversityForest’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG -DR_BUILD -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppEigen/include' -I/usr/local/include   -fpic  -g -O2  -c AAA_check_cpp11.cpp -o AAA_check_cpp11.o
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG -DR_BUILD -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppEigen/include' -I/usr/local/include   -fpic  -g -O2  -c Data.cpp -o Data.o
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG -DR_BUILD -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppEigen/include' -I/usr/local/include   -fpic  -g -O2  -c DataChar.cpp -o DataChar.o
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG -DR_BUILD -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppEigen/include' -I/usr/local/include   -fpic  -g -O2  -c DataFloat.cpp -o DataFloat.o
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG -DR_BUILD -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppEigen/include' -I/usr/local/include   -fpic  -g -O2  -c DataSparse.cpp -o DataSparse.o
In file included from /opt/R/4.1.1/lib/R/site-library/RcppEigen/include/Eigen/Core:397,
...
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** checking absolute paths in shared objects and dynamic libraries
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (diversityForest)


```
# dmutate

<details>

* Version: 0.1.3
* GitHub: https://github.com/kylebaron/dmutate
* Source code: https://github.com/cran/dmutate
* Date/Publication: 2021-04-22 22:20:03 UTC
* Number of recursive dependencies: 36

Run `cloud_details(, "dmutate")` for more info

</details>

## Newly broken

*   checking whether package ‘dmutate’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/dmutate/new/dmutate.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘dmutate’ ...
** package ‘dmutate’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error: object ‘data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘dmutate’
* removing ‘/tmp/workdir/dmutate/new/dmutate.Rcheck/dmutate’


```
### CRAN

```
* installing *source* package ‘dmutate’ ...
** package ‘dmutate’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (dmutate)


```
# DMwR2

<details>

* Version: 0.0.2
* GitHub: https://github.com/ltorgo/DMwR2
* Source code: https://github.com/cran/DMwR2
* Date/Publication: 2016-10-13 00:23:37
* Number of recursive dependencies: 39

Run `cloud_details(, "DMwR2")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘DMwR2-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: knnImputation
    > ### Title: Fill in NA values with the values of the nearest neighbours
    > ### Aliases: knnImputation
    > ### Keywords: models
    > 
    > ### ** Examples
    > 
    > data(algae)
    > cleanAlgae <- knnImputation(algae)
    Error: 'type_sum' is not an exported object from 'namespace:dplyr'
    Execution halted
    ```

*   checking dependencies in R code ... WARNING
    ```
    Missing or unexported object: ‘dplyr::type_sum’
    ```

# doMIsaul

<details>

* Version: 1.0.1
* GitHub: https://github.com/LilithF/doMIsaul
* Source code: https://github.com/cran/doMIsaul
* Date/Publication: 2021-10-18 18:20:10 UTC
* Number of recursive dependencies: 164

Run `cloud_details(, "doMIsaul")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘doMIsaul-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_MIpca
    > ### Title: Plot a PCA from a multiply imputed dataset.
    > ### Aliases: plot_MIpca plot_MIpca_all
    > 
    > ### ** Examples
    > 
    > data(cancer, package = "survival")
    > cancer.imp <- MImpute(cancer[, -c(1:3)], 4)
    > plot_MIpca(cancer.imp, 1:10,
    +     pca.varsel = c("age", "sex", "ph.ecog", "meal.cal",  "wt.loss"))
    Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
           ▆
        1. ├─testthat::expect_s3_class(...) at test-unsupMI.R:137:2
        2. │ └─testthat::quasi_label(enquo(object), arg = "object")
        3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
        4. ├─doMIsaul::plot_MIpca(...)
        5. └─base::loadNamespace(x)
        6.   ├─base::namespaceImportFrom(...)
        7.   │ └─base::asNamespace(ns)
        8.   └─base::loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]])
        9.     └─base::namespaceImportFrom(...)
       10.       └─base::importIntoEnv(impenv, impnames, ns, impvars)
      
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 57 ]
      Error: Test failures
      Execution halted
    ```

# dumbbell

<details>

* Version: 0.1
* GitHub: https://github.com/foocheung2/dumbbell
* Source code: https://github.com/cran/dumbbell
* Date/Publication: 2021-02-25 09:10:02 UTC
* Number of recursive dependencies: 126

Run `cloud_details(, "dumbbell")` for more info

</details>

## Newly broken

*   checking whether package ‘dumbbell’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/dumbbell/new/dumbbell.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

## Installation

### Devel

```
* installing *source* package ‘dumbbell’ ...
** package ‘dumbbell’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘dumbbell’
* removing ‘/tmp/workdir/dumbbell/new/dumbbell.Rcheck/dumbbell’


```
### CRAN

```
* installing *source* package ‘dumbbell’ ...
** package ‘dumbbell’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (dumbbell)


```
# eesim

<details>

* Version: 0.1.0
* GitHub: https://github.com/sakoehler7/eesim
* Source code: https://github.com/cran/eesim
* Date/Publication: 2017-06-03 17:55:52 UTC
* Number of recursive dependencies: 56

Run `cloud_details(, "eesim")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘eesim.Rmd’ using rmarkdown
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    ...
    Quitting from lines 163-174 (eesim.Rmd) 
    Error: processing vignette 'eesim.Rmd' failed with diagnostics:
    could not find function "data_frame"
    --- failed re-building ‘eesim.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘eesim.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# EGAnet

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/EGAnet
* Date/Publication: 2021-11-10 08:20:02 UTC
* Number of recursive dependencies: 217

Run `cloud_details(, "EGAnet")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘EGAnet-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: dimensionStability
    > ### Title: Dimension Stability Statistics from 'bootEGA'
    > ### Aliases: dimensionStability
    > 
    > ### ** Examples
    > 
    > # Load data
    ...
    R and EGAnet versions:[0m
     • R version: 4.1.1
     • EGAnet version: 1.0.0
    [1;m
    Operating System:[0m
     • OS: Linux
     • Version: 4.14.243-185.433.amzn2.x86_64 #1 SMP Mon Aug 9 05:55:52 UTC 2021
    Error in order(names(stability.dimensions)) : argument 1 is not a vector
    Calls: dimensionStability -> order
    Execution halted
    ```

# egor

<details>

* Version: 1.22.1
* GitHub: https://github.com/tilltnet/egor
* Source code: https://github.com/cran/egor
* Date/Publication: 2022-01-14 20:52:41 UTC
* Number of recursive dependencies: 89

Run `cloud_details(, "egor")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘egor-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: as_tibble.egor
    > ### Title: Extract ego, alter, and alter-alter tables from an 'egor'
    > ###   object.
    > ### Aliases: as_tibble.egor as_survey.egor as_egos_df as_alters_df
    > ###   as_aaties_df as_egos_survey as_alters_survey as_aaties_survey
    > 
    > ### ** Examples
    ...
    
    The following object is masked from ‘package:stats’:
    
        filter
    
    > as_survey(egor32) # Ego table with survey design.
    Independent Sampling design (with replacement)
    Called via srvyr
    Error: 'type_sum' is not an exported object from 'namespace:dplyr'
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        1. ├─testthat::expect_error(summary(egor32), NA) at test-egor.R:92:4
        2. │ └─testthat:::quasi_capture(...)
        3. │   ├─testthat .capture(...)
        4. │   │ └─base::withCallingHandlers(...)
        5. │   └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
        6. ├─base::summary(egor32)
        7. └─egor:::summary.egor(egor32)
        8.   ├─base::print(ego_design(object))
        9.   └─srvyr:::print.tbl_svy(ego_design(object))
       10.     └─base::vapply(x$variables, dplyr::type_sum, character(1))
       11.       └─base::match.fun(FUN)
      
      [ FAIL 3 | WARN 0 | SKIP 0 | PASS 228 ]
      Error: Test failures
      Execution halted
    ```

# ENMTools

<details>

* Version: 1.0.5
* GitHub: NA
* Source code: https://github.com/cran/ENMTools
* Date/Publication: 2021-06-25 10:40:02 UTC
* Number of recursive dependencies: 273

Run `cloud_details(, "ENMTools")` for more info

</details>

## Newly broken

*   checking whether package ‘ENMTools’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/ENMTools/new/ENMTools.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘ENMTools’ ...
** package ‘ENMTools’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘ENMTools’
* removing ‘/tmp/workdir/ENMTools/new/ENMTools.Rcheck/ENMTools’


```
### CRAN

```
* installing *source* package ‘ENMTools’ ...
** package ‘ENMTools’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (ENMTools)


```
# entropart

<details>

* Version: 1.6-8
* GitHub: https://github.com/EricMarcon/entropart
* Source code: https://github.com/cran/entropart
* Date/Publication: 2021-06-04 12:00:02 UTC
* Number of recursive dependencies: 122

Run `cloud_details(, "entropart")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘entropart-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: DivProfile
    > ### Title: Diversity Profile of a metacommunity
    > ### Aliases: DivProfile is.DivProfile plot.DivProfile autoplot.DivProfile
    > ###   summary.DivProfile
    > 
    > ### ** Examples
    > 
    > # Load Paracou data (number of trees per species in two 1-ha plot of a tropical forest)
    > data(Paracou618)
    > # Estimate diversity.
    > Profile <- DivProfile(q.seq = seq(0, 2, 0.1), Paracou618.MC, Biased = FALSE)
    > plot(Profile)
    > autoplot(Profile)
    Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘entropart.Rmd’ using rmarkdown
    Warning in doTryCatch(return(expr), name, parentenv, handler) :
      NAs introduced by coercion
    Warning in doTryCatch(return(expr), name, parentenv, handler) :
      NAs introduced by coercion
    Warning in doTryCatch(return(expr), name, parentenv, handler) :
      NAs introduced by coercion
    Warning in doTryCatch(return(expr), name, parentenv, handler) :
    ...
    Quitting from lines 210-212 (entropart.Rmd) 
    Error: processing vignette 'entropart.Rmd' failed with diagnostics:
    object 'as_data_frame' is not exported by 'namespace:dplyr'
    --- failed re-building ‘entropart.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘entropart.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘SPECIES’ ‘ade4’
      All declared Imports should be used.
    ```

# europepmc

<details>

* Version: 0.4.1
* GitHub: https://github.com/ropensci/europepmc
* Source code: https://github.com/cran/europepmc
* Date/Publication: 2021-09-02 07:20:05 UTC
* Number of recursive dependencies: 78

Run `cloud_details(, "europepmc")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... WARNING
    ```
    Missing or unexported object: ‘dplyr::as_data_frame’
    ```

# eyetrackingR

<details>

* Version: 0.2.0
* GitHub: https://github.com/samhforbes/eyetrackingR
* Source code: https://github.com/cran/eyetrackingR
* Date/Publication: 2021-09-27 10:00:14 UTC
* Number of recursive dependencies: 84

Run `cloud_details(, "eyetrackingR")` for more info

</details>

## Newly broken

*   checking R code for possible problems ... NOTE
    ```
    simulate_eyetrackingr_data: no visible global function definition for
      ‘data_frame’
    Undefined global functions or variables:
      data_frame
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘rlang’
      All declared Imports should be used.
    ```

# ezec

<details>

* Version: 1.0.1
* GitHub: https://github.com/grunwaldlab/ezec
* Source code: https://github.com/cran/ezec
* Date/Publication: 2016-12-05 08:27:32
* Number of recursive dependencies: 100

Run `cloud_details(, "ezec")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ezec-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: EC_table
    > ### Title: Function to generate a table of EC values from a data frame of
    > ###   multiple isolates.
    > ### Aliases: EC_table
    > 
    > ### ** Examples
    > 
    ...
    See vignette('programming') for more help
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.
    Warning: `group_by_()` was deprecated in dplyr 0.7.0.
    Please use `group_by()` instead.
    See vignette('programming') for more help
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.
    Error: 'data_frame' is not an exported object from 'namespace:dplyr'
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        3. │   ├─testthat .capture(...)
        4. │   │ └─testthat::capture_output_lines(code, print, width = width)
        5. │   │   └─testthat:::eval_with_output(code, print = print, width = width)
        6. │   │     ├─withr::with_output_sink(path, withVisible(code))
        7. │   │     │ └─base::force(code)
        8. │   │     └─base::withVisible(code)
        9. │   └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
       10. ├─ezec::EC_table(...)
       11. │ └─dplyr::data_frame(sample = models[[idcol]]) %>% ...
       12. └─dplyr::bind_cols(., EC)
       13.   └─rlang::list2(...)
      
      [ FAIL 2 | WARN 2 | SKIP 1 | PASS 2 ]
      Error: Test failures
      Execution halted
    ```

*   checking dependencies in R code ... WARNING
    ```
    Missing or unexported object: ‘dplyr::data_frame’
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘getting_started.Rmd’ using rmarkdown
    Warning: `do_()` was deprecated in dplyr 0.7.0.
    Please use `do()` instead.
    See vignette('programming') for more help
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.
    Warning: `group_by_()` was deprecated in dplyr 0.7.0.
    Please use `group_by()` instead.
    ...
    Quitting from lines 77-81 (getting_started.Rmd) 
    Error: processing vignette 'getting_started.Rmd' failed with diagnostics:
    'data_frame' is not an exported object from 'namespace:dplyr'
    --- failed re-building ‘getting_started.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘getting_started.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# factoextra

<details>

* Version: 1.0.7
* GitHub: https://github.com/kassambara/factoextra
* Source code: https://github.com/cran/factoextra
* Date/Publication: 2020-04-01 21:20:02 UTC
* Number of recursive dependencies: 112

Run `cloud_details(, "factoextra")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘factoextra-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: eclust
    > ### Title: Visual enhancement of clustering analysis
    > ### Aliases: eclust
    > 
    > ### ** Examples
    > 
    > # Load and scale data
    > data("USArrests")
    > df <- scale(USArrests)
    > 
    > # Enhanced k-means clustering
    > # nboot >= 500 is recommended
    > res.km <- eclust(df, "kmeans", nboot = 2)
    Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unavailable namespace imported from by a ':::' call: ‘ggpubr’
      See the note in ?`:::` about the use of this operator.
    ```

# factorMerger

<details>

* Version: 0.4.0
* GitHub: https://github.com/MI2DataLab/factorMerger
* Source code: https://github.com/cran/factorMerger
* Date/Publication: 2019-07-03 22:50:26 UTC
* Number of recursive dependencies: 159

Run `cloud_details(, "factorMerger")` for more info

</details>

## Newly broken

*   checking whether package ‘factorMerger’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/factorMerger/new/factorMerger.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘forcats’ ‘formula.tools’
      All declared Imports should be used.
    ```

## Installation

### Devel

```
* installing *source* package ‘factorMerger’ ...
** package ‘factorMerger’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘factorMerger’
* removing ‘/tmp/workdir/factorMerger/new/factorMerger.Rcheck/factorMerger’


```
### CRAN

```
* installing *source* package ‘factorMerger’ ...
** package ‘factorMerger’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (factorMerger)


```
# fastai

<details>

* Version: 2.1.0
* GitHub: https://github.com/EagerAI/fastai
* Source code: https://github.com/cran/fastai
* Date/Publication: 2021-10-25 09:50:05 UTC
* Number of recursive dependencies: 105

Run `cloud_details(, "fastai")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(reticulate)
      > library(magrittr)
      
      Attaching package: 'magrittr'
      
      The following objects are masked from 'package:testthat':
      
          equals, is_less_than, not
      
      > library(data.table)
      > library(ggplot2)
      > library(ggpubr)
      Error: package or namespace load failed for 'ggpubr':
       object 'as_data_frame' is not exported by 'namespace:dplyr'
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        doc    3.0Mb
        help   1.2Mb
    ```

# fastR2

<details>

* Version: 1.2.1
* GitHub: https://github.com/rpruim/fastR2
* Source code: https://github.com/cran/fastR2
* Date/Publication: 2018-08-31 20:50:08 UTC
* Number of recursive dependencies: 151

Run `cloud_details(, "fastR2")` for more info

</details>

## Newly broken

*   checking whether package ‘fastR2’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/fastR2/new/fastR2.Rcheck/00install.out’ for details.
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘alr3’
    ```

## Installation

### Devel

```
* installing *source* package ‘fastR2’ ...
** package ‘fastR2’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘fastR2’
* removing ‘/tmp/workdir/fastR2/new/fastR2.Rcheck/fastR2’


```
### CRAN

```
* installing *source* package ‘fastR2’ ...
** package ‘fastR2’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (fastR2)


```
# finalfit

<details>

* Version: 1.0.4
* GitHub: https://github.com/ewenharrison/finalfit
* Source code: https://github.com/cran/finalfit
* Date/Publication: 2021-12-05 16:50:02 UTC
* Number of recursive dependencies: 145

Run `cloud_details(, "finalfit")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘finalfit-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: surv_plot
    > ### Title: Plot survival curves with number-at-risk table
    > ### Aliases: surv_plot
    > 
    > ### ** Examples
    > 
    > library(finalfit)
    ...
    
    > 
    > # Survival plot
    > data(colon_s)
    > explanatory = c("perfor.factor")
    > dependent = "Surv(time, status)"
    > colon_s %>%
    +   surv_plot(dependent, explanatory, xlab="Time (days)", pval=TRUE, legend="none")
    Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
        4. ├─finalfit::surv_plot(colon_s, "Surv(time, status)", "age.factor")
        5. │ └─base::do.call(survminer::ggsurvplot, args)
        6. └─base::loadNamespace(x)
        7.   ├─base::namespaceImport(...)
        8.   └─base::loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]])
        9.     ├─base::namespaceImportFrom(...)
       10.     │ └─base::asNamespace(ns)
       11.     └─base::loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]])
       12.       └─base::namespaceImportFrom(...)
       13.         └─base::importIntoEnv(impenv, impnames, ns, impvars)
      
      [ FAIL 1 | WARN 10 | SKIP 0 | PASS 130 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘all_tables_examples.Rmd’ using rmarkdown
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    ...
    Quitting from lines 153-158 (survival.Rmd) 
    Error: processing vignette 'survival.Rmd' failed with diagnostics:
    object 'as_data_frame' is not exported by 'namespace:dplyr'
    --- failed re-building ‘survival.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘survival.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# finetune

<details>

* Version: 0.1.0
* GitHub: https://github.com/tidymodels/finetune
* Source code: https://github.com/cran/finetune
* Date/Publication: 2021-07-21 19:40:02 UTC
* Number of recursive dependencies: 164

Run `cloud_details(, "finetune")` for more info

</details>

## Newly broken

*   checking whether package ‘finetune’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/finetune/new/finetune.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘ranger’ ‘yardstick’
      All declared Imports should be used.
    ```

## Installation

### Devel

```
* installing *source* package ‘finetune’ ...
** package ‘finetune’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘tune’:
 object ‘type_sum’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘finetune’
* removing ‘/tmp/workdir/finetune/new/finetune.Rcheck/finetune’


```
### CRAN

```
* installing *source* package ‘finetune’ ...
** package ‘finetune’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (finetune)


```
# flipr

<details>

* Version: 0.3.1
* GitHub: https://github.com/astamm/flipr
* Source code: https://github.com/cran/flipr
* Date/Publication: 2021-09-16 12:20:02 UTC
* Number of recursive dependencies: 130

Run `cloud_details(, "flipr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘flipr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: PlausibilityFunction
    > ### Title: R6 Class representing a plausibility function
    > ### Aliases: PlausibilityFunction
    > 
    > ### ** Examples
    > 
    > 
    ...
    > stat_functions <- list(stat_t)
    > stat_assignments <- list(mean = 1)
    > pf <- PlausibilityFunction$new(
    +   null_spec = null_spec,
    +   stat_functions = stat_functions,
    +   stat_assignments = stat_assignments,
    +   x, y
    + )
    Error: object ‘type_sum’ is not exported by 'namespace:dplyr'
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Error: object 'type_sum' is not exported by 'namespace:dplyr'
      Backtrace:
          ▆
       1. ├─PlausibilityFunction$new(...) at test-plausibility-class.R:157:2
       2. │ └─flipr initialize(...)
       3. │   ├─rlang::list2(...)
       4. │   └─rlang::eval_tidy(...)
       5. ├─flipr:::new_inferred_param(...)
       6. └─base::loadNamespace(x)
       7.   └─base::namespaceImportFrom(...)
       8.     └─base::importIntoEnv(impenv, impnames, ns, impvars)
      
      [ FAIL 5 | WARN 0 | SKIP 1 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘alternative.Rmd’ using rmarkdown
    --- finished re-building ‘alternative.Rmd’
    
    --- re-building ‘exactness.Rmd’ using rmarkdown
    --- finished re-building ‘exactness.Rmd’
    
    --- re-building ‘flipr.Rmd’ using rmarkdown
    ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──
    ✔ ggplot2 3.3.5     ✔ purrr   0.3.4
    ...
    --- failed re-building ‘flipr.Rmd’
    
    --- re-building ‘plausibility.Rmd’ using rmarkdown
    --- finished re-building ‘plausibility.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘flipr.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.9Mb
      sub-directories of 1Mb or more:
        doc    7.6Mb
        libs   1.5Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘R6’ ‘pbapply’
      All declared Imports should be used.
    ```

# fmf

<details>

* Version: 1.1.1
* GitHub: NA
* Source code: https://github.com/cran/fmf
* Date/Publication: 2020-09-03 07:32:12 UTC
* Number of recursive dependencies: 158

Run `cloud_details(, "fmf")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘fmf-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot
    > ### Title: PCA Plot of the Noise Score of Each Individual
    > ### Aliases: plot
    > 
    > ### ** Examples
    > 
    > 
    ...
    INFO  [07:39:54.219] Completed growing isolation forest 
    INFO  [07:39:54.244] dataset has duplicated rows 
    INFO  [07:39:54.245] Building Isolation Forest ...  
    INFO  [07:39:54.249] done 
    INFO  [07:39:54.250] Computing depth of terminal nodes ...  
    INFO  [07:39:54.393] done 
    INFO  [07:39:54.398] Completed growing isolation forest 
    > plot(out$noise_score, iris[,-1], iris[,1])
    Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
    Execution halted
    ```

# FRK

<details>

* Version: 2.0.3
* GitHub: https://github.com/andrewzm/FRK
* Source code: https://github.com/cran/FRK
* Date/Publication: 2022-01-10 08:22:45 UTC
* Number of recursive dependencies: 151

Run `cloud_details(, "FRK")` for more info

</details>

## Newly broken

*   checking whether package ‘FRK’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/FRK/new/FRK.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘FRK_intro.Rnw’ using knitr
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    ...
    l.81 ^^M
            
    !  ==> Fatal error occurred, no output PDF file produced!
    --- failed re-building ‘FRK_non-Gaussian.Rnw’
    
    SUMMARY: processing the following files failed:
      ‘FRK_intro.Rnw’ ‘FRK_non-Gaussian.Rnw’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 85.5Mb
      sub-directories of 1Mb or more:
        data   5.0Mb
        doc    2.4Mb
        libs  77.1Mb
    ```

## Installation

### Devel

```
* installing *source* package ‘FRK’ ...
** package ‘FRK’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
g++ -std=gnu++14 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/TMB/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppEigen/include' -I/usr/local/include   -fpic  -g -O2  -c FRK.cpp -o FRK.o
In file included from /opt/R/4.1.1/lib/R/site-library/RcppEigen/include/Eigen/Core:397,
                 from /opt/R/4.1.1/lib/R/site-library/RcppEigen/include/Eigen/Dense:1,
                 from /opt/R/4.1.1/lib/R/site-library/TMB/include/TMB.hpp:58,
                 from FRK.cpp:1:
/opt/R/4.1.1/lib/R/site-library/RcppEigen/include/Eigen/src/Core/arch/SSE/PacketMath.h:60:39: warning: ignoring attributes on template argument ‘__m128’ {aka ‘__vector(4) float’} [-Wignored-attributes]
...
** byte-compile and prepare package for lazy loading
Warning in checkMatrixPackageVersion() :
  Package version inconsistency detected.
TMB was built with Matrix version 1.4.0
Current Matrix version is 1.3.4
Please re-install 'TMB' from source using install.packages('TMB', type = 'source') or ask CRAN for a binary version of 'TMB' matching CRAN's 'Matrix' package
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘FRK’
* removing ‘/tmp/workdir/FRK/new/FRK.Rcheck/FRK’


```
### CRAN

```
* installing *source* package ‘FRK’ ...
** package ‘FRK’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
g++ -std=gnu++14 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/TMB/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppEigen/include' -I/usr/local/include   -fpic  -g -O2  -c FRK.cpp -o FRK.o
In file included from /opt/R/4.1.1/lib/R/site-library/RcppEigen/include/Eigen/Core:397,
                 from /opt/R/4.1.1/lib/R/site-library/RcppEigen/include/Eigen/Dense:1,
                 from /opt/R/4.1.1/lib/R/site-library/TMB/include/TMB.hpp:58,
                 from FRK.cpp:1:
/opt/R/4.1.1/lib/R/site-library/RcppEigen/include/Eigen/src/Core/arch/SSE/PacketMath.h:60:39: warning: ignoring attributes on template argument ‘__m128’ {aka ‘__vector(4) float’} [-Wignored-attributes]
...
Please re-install 'TMB' from source using install.packages('TMB', type = 'source') or ask CRAN for a binary version of 'TMB' matching CRAN's 'Matrix' package
** checking absolute paths in shared objects and dynamic libraries
** testing if installed package can be loaded from final location
Warning in checkMatrixPackageVersion() :
  Package version inconsistency detected.
TMB was built with Matrix version 1.4.0
Current Matrix version is 1.3.4
Please re-install 'TMB' from source using install.packages('TMB', type = 'source') or ask CRAN for a binary version of 'TMB' matching CRAN's 'Matrix' package
** testing if installed package keeps a record of temporary installation path
* DONE (FRK)


```
# FuncNN

<details>

* Version: 1.0
* GitHub: https://github.com/b-thi/FuncNN
* Source code: https://github.com/cran/FuncNN
* Date/Publication: 2020-09-15 09:40:15 UTC
* Number of recursive dependencies: 159

Run `cloud_details(, "FuncNN")` for more info

</details>

## Newly broken

*   checking whether package ‘FuncNN’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/FuncNN/new/FuncNN.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘foreach’
      All declared Imports should be used.
    ```

## Installation

### Devel

```
* installing *source* package ‘FuncNN’ ...
** package ‘FuncNN’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘FuncNN’
* removing ‘/tmp/workdir/FuncNN/new/FuncNN.Rcheck/FuncNN’


```
### CRAN

```
* installing *source* package ‘FuncNN’ ...
** package ‘FuncNN’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (FuncNN)


```
# fuzzyjoin

<details>

* Version: 0.1.6
* GitHub: https://github.com/dgrtwo/fuzzyjoin
* Source code: https://github.com/cran/fuzzyjoin
* Date/Publication: 2020-05-15 05:50:21 UTC
* Number of recursive dependencies: 93

Run `cloud_details(, "fuzzyjoin")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘fuzzyjoin-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: difference_join
    > ### Title: Join two tables based on absolute difference between their
    > ###   columns
    > ### Aliases: difference_join difference_inner_join difference_left_join
    > ###   difference_right_join difference_full_join difference_semi_join
    > ###   difference_anti_join
    > 
    ...
    1          5.1         3.5          1.4         0.2  setosa
    2          4.9         3.0          1.4         0.2  setosa
    3          4.7         3.2          1.3         0.2  setosa
    4          4.6         3.1          1.5         0.2  setosa
    5          5.0         3.6          1.4         0.2  setosa
    6          5.4         3.9          1.7         0.4  setosa
    > sepal_lengths <- data_frame(Sepal.Length = c(5, 6, 7), Type = 1:3)
    Error in data_frame(Sepal.Length = c(5, 6, 7), Type = 1:3) : 
      could not find function "data_frame"
    Execution halted
    ```

# gaiah

<details>

* Version: 0.0.4
* GitHub: NA
* Source code: https://github.com/cran/gaiah
* Date/Publication: 2020-05-18 10:00:03 UTC
* Number of recursive dependencies: 56

Run `cloud_details(, "gaiah")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... WARNING
    ```
    Namespace in Imports field not imported from: ‘maptools’
      All declared Imports should be used.
    Missing or unexported object: ‘dplyr::data_frame’
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘maptools’
      All declared Imports should be used.
    ```

# galvanizer

<details>

* Version: 0.5.3
* GitHub: https://github.com/jonlinca/galvanizer
* Source code: https://github.com/cran/galvanizer
* Date/Publication: 2021-05-28 15:10:07 UTC
* Number of recursive dependencies: 46

Run `cloud_details(, "galvanizer")` for more info

</details>

## Newly broken

*   checking whether package ‘galvanizer’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/galvanizer/new/galvanizer.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘galvanizer’ ...
** package ‘galvanizer’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error in namespaceExport(ns, exports) : undefined exports: as_data_frame
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace -> namespaceExport
Execution halted
ERROR: lazy loading failed for package ‘galvanizer’
* removing ‘/tmp/workdir/galvanizer/new/galvanizer.Rcheck/galvanizer’


```
### CRAN

```
* installing *source* package ‘galvanizer’ ...
** package ‘galvanizer’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (galvanizer)


```
# garchmodels

<details>

* Version: 0.1.1
* GitHub: NA
* Source code: https://github.com/cran/garchmodels
* Date/Publication: 2021-04-12 17:20:02 UTC
* Number of recursive dependencies: 229

Run `cloud_details(, "garchmodels")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘garchmodels-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: garch_params
    > ### Title: Tuning Parameters for Univariate Garch Models
    > ### Aliases: garch_params arch_order garch_order ar_order ma_order
    > 
    > ### ** Examples
    > 
    > arch_order()
    Error: object ‘type_sum’ is not exported by 'namespace:dplyr'
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ✔ readr   2.1.1     ✔ forcats 0.5.1
      ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
      ✖ readr::edition_get()   masks testthat::edition_get()
      ✖ dplyr::filter()        masks stats::filter()
      ✖ dplyr::first()         masks rmgarch::first()
      ✖ purrr::is_null()       masks testthat::is_null()
      ✖ dplyr::lag()           masks stats::lag()
      ✖ dplyr::last()          masks rmgarch::last()
      ✖ readr::local_edition() masks testthat::local_edition()
      ✖ dplyr::matches()       masks tidyr::matches(), testthat::matches()
      ✖ purrr::reduce()        masks rugarch::reduce()
      > library(tidymodels)
      Error: package or namespace load failed for 'tidymodels':
       object 'type_sum' is not exported by 'namespace:dplyr'
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘getting-started.Rmd’ using rmarkdown
    Loading required package: parsnip
    Loading required package: rugarch
    Loading required package: parallel
    
    Attaching package: 'rugarch'
    
    The following object is masked from 'package:stats':
    
    ...
    Error: processing vignette 'tuning_univariate_algorithms.Rmd' failed with diagnostics:
    package or namespace load failed for 'tidymodels':
     object 'type_sum' is not exported by 'namespace:dplyr'
    --- failed re-building ‘tuning_univariate_algorithms.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘getting-started.Rmd’ ‘tuning_univariate_algorithms.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.9Mb
      sub-directories of 1Mb or more:
        doc   6.5Mb
    ```

# genogeographer

<details>

* Version: 0.1.19
* GitHub: NA
* Source code: https://github.com/cran/genogeographer
* Date/Publication: 2019-09-27 10:20:08 UTC
* Number of recursive dependencies: 131

Run `cloud_details(, "genogeographer")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘genogeographer-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: LR_table
    > ### Title: Compute pairwise likelihood ratios
    > ### Aliases: LR_table
    > 
    > ### ** Examples
    > 
    > df_ <- simulate_pops(pop_n = 4, aims_n = 50)
    ...
      9. ├─dplyr:::filter.data.frame(., x0 == 0, X0 == 0)
     10. │ └─dplyr:::filter_rows(.data, ..., caller_env = caller_env())
     11. │   ├─base::withCallingHandlers(...)
     12. │   └─mask$eval_all_filter(dots, env_filter)
     13. ├─rlang::abort(message = message)
     14. │ └─rlang:::signal_abort(cnd, .file)
     15. │   └─base::signalCondition(cnd)
     16. └─dplyr `<fn>`(`<rlng_rrr>`)
     17.   └─rlang::abort(...)
    Execution halted
    ```

# geodiv

<details>

* Version: 1.0.4
* GitHub: https://github.com/bioXgeo/geodiv
* Source code: https://github.com/cran/geodiv
* Date/Publication: 2021-09-03 12:20:13 UTC
* Number of recursive dependencies: 185

Run `cloud_details(, "geodiv")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘geodiv_vignette.Rmd’ using rmarkdown
    trying URL 'https://ndownloader.figshare.com/files/24366086'
    Content type 'image/tiff' length 11197175 bytes (10.7 MB)
    ==================================================
    downloaded 10.7 MB
    
    trying URL 'https://ndownloader.figshare.com/files/28259166'
    Content type 'text/csv' length 219378055 bytes (209.2 MB)
    ...
    Quitting from lines 452-487 (geodiv_vignette.Rmd) 
    Error: processing vignette 'geodiv_vignette.Rmd' failed with diagnostics:
    object 'as_data_frame' is not exported by 'namespace:dplyr'
    --- failed re-building ‘geodiv_vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘geodiv_vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘phonTools’ ‘rgdal’ ‘rgeos’
      All declared Imports should be used.
    ```

# geospark

<details>

* Version: 0.3.1
* GitHub: https://github.com/harryprince/geospark
* Source code: https://github.com/cran/geospark
* Date/Publication: 2020-03-02 05:40:02 UTC
* Number of recursive dependencies: 66

Run `cloud_details(, "geospark")` for more info

</details>

## Newly broken

*   checking whether package ‘geospark’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/geospark/new/geospark.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dbplyr’
      All declared Imports should be used.
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

## Installation

### Devel

```
* installing *source* package ‘geospark’ ...
** package ‘geospark’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘type_sum’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘geospark’
* removing ‘/tmp/workdir/geospark/new/geospark.Rcheck/geospark’


```
### CRAN

```
* installing *source* package ‘geospark’ ...
** package ‘geospark’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (geospark)


```
# GFDsurv

<details>

* Version: 0.1.0
* GitHub: https://github.com/PhilippSteinhauer/GFDsurv
* Source code: https://github.com/cran/GFDsurv
* Date/Publication: 2021-07-14 10:00:02 UTC
* Number of recursive dependencies: 140

Run `cloud_details(, "GFDsurv")` for more info

</details>

## Newly broken

*   checking whether package ‘GFDsurv’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/GFDsurv/new/GFDsurv.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘GFDsurv’ ...
** package ‘GFDsurv’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘GFDsurv’
* removing ‘/tmp/workdir/GFDsurv/new/GFDsurv.Rcheck/GFDsurv’


```
### CRAN

```
* installing *source* package ‘GFDsurv’ ...
** package ‘GFDsurv’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (GFDsurv)


```
# ggedit

<details>

* Version: 0.3.1
* GitHub: https://github.com/yonicd/ggedit
* Source code: https://github.com/cran/ggedit
* Date/Publication: 2020-06-02 11:50:06 UTC
* Number of recursive dependencies: 87

Run `cloud_details(, "ggedit")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggedit-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: compare
    > ### Title: compare
    > ### Aliases: compare
    > 
    > ### ** Examples
    > 
    > compare(ggplot2::theme_bw(),ggplot2::theme_get())
    Error: 'as_data_frame' is not an exported object from 'namespace:dplyr'
    Execution halted
    ```

*   checking dependencies in R code ... WARNING
    ```
    Namespace in Imports field not imported from: ‘magrittr’
      All declared Imports should be used.
    Missing or unexported object: ‘dplyr::as_data_frame’
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘magrittr’
      All declared Imports should be used.
    ```

# ggESDA

<details>

* Version: 0.1.0
* GitHub: https://github.com/kiangkiangkiang/ggESDA
* Source code: https://github.com/cran/ggESDA
* Date/Publication: 2022-01-10 10:02:42 UTC
* Number of recursive dependencies: 195

Run `cloud_details(, "ggESDA")` for more info

</details>

## Newly broken

*   checking whether package ‘ggESDA’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/ggESDA/new/ggESDA.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘ggESDA’ ...
** package ‘ggESDA’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘ggESDA’
* removing ‘/tmp/workdir/ggESDA/new/ggESDA.Rcheck/ggESDA’


```
### CRAN

```
* installing *source* package ‘ggESDA’ ...
** package ‘ggESDA’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (ggESDA)


```
# ggheatmap

<details>

* Version: 2.1
* GitHub: NA
* Source code: https://github.com/cran/ggheatmap
* Date/Publication: 2021-07-25 13:20:02 UTC
* Number of recursive dependencies: 114

Run `cloud_details(, "ggheatmap")` for more info

</details>

## Newly broken

*   checking whether package ‘ggheatmap’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/ggheatmap/new/ggheatmap.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘ggheatmap’ ...
** package ‘ggheatmap’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘ggheatmap’
* removing ‘/tmp/workdir/ggheatmap/new/ggheatmap.Rcheck/ggheatmap’


```
### CRAN

```
* installing *source* package ‘ggheatmap’ ...
** package ‘ggheatmap’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (ggheatmap)


```
# ggpubr

<details>

* Version: 0.4.0
* GitHub: https://github.com/kassambara/ggpubr
* Source code: https://github.com/cran/ggpubr
* Date/Publication: 2020-06-27 06:20:02 UTC
* Number of recursive dependencies: 86

Run `cloud_details(, "ggpubr")` for more info

</details>

## Newly broken

*   checking whether package ‘ggpubr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/ggpubr/new/ggpubr.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘ggpubr’ ...
** package ‘ggpubr’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘ggpubr’
* removing ‘/tmp/workdir/ggpubr/new/ggpubr.Rcheck/ggpubr’


```
### CRAN

```
* installing *source* package ‘ggpubr’ ...
** package ‘ggpubr’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (ggpubr)


```
# gistr

<details>

* Version: 0.9.0
* GitHub: https://github.com/ropensci/gistr
* Source code: https://github.com/cran/gistr
* Date/Publication: 2020-07-29 05:10:15 UTC
* Number of recursive dependencies: 60

Run `cloud_details(, "gistr")` for more info

</details>

## Newly broken

*   checking whether package ‘gistr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/gistr/new/gistr.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘gistr’ ...
** package ‘gistr’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘gistr’
* removing ‘/tmp/workdir/gistr/new/gistr.Rcheck/gistr’


```
### CRAN

```
* installing *source* package ‘gistr’ ...
** package ‘gistr’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (gistr)


```
# glmmSeq

<details>

* Version: 0.1.0
* GitHub: https://github.com/KatrionaGoldmann/glmmSeq
* Source code: https://github.com/cran/glmmSeq
* Date/Publication: 2021-03-30 11:40:02 UTC
* Number of recursive dependencies: 122

Run `cloud_details(, "glmmSeq")` for more info

</details>

## Newly broken

*   checking whether package ‘glmmSeq’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/glmmSeq/new/glmmSeq.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘glmmSeq’ ...
** package ‘glmmSeq’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘glmmSeq’
* removing ‘/tmp/workdir/glmmSeq/new/glmmSeq.Rcheck/glmmSeq’


```
### CRAN

```
* installing *source* package ‘glmmSeq’ ...
** package ‘glmmSeq’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (glmmSeq)


```
# googlesheets

<details>

* Version: 0.3.0
* GitHub: https://github.com/jennybc/googlesheets
* Source code: https://github.com/cran/googlesheets
* Date/Publication: 2018-06-29 04:38:09 UTC
* Number of recursive dependencies: 85

Run `cloud_details(, "googlesheets")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... WARNING
    ```
    Namespace in Imports field not imported from: ‘tibble’
      All declared Imports should be used.
    Missing or unexported objects:
      ‘dplyr::as_data_frame’ ‘dplyr::data_frame’ ‘dplyr::data_frame_’
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tibble’
      All declared Imports should be used.
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# graphframes

<details>

* Version: 0.1.2
* GitHub: https://github.com/rstudio/graphframes
* Source code: https://github.com/cran/graphframes
* Date/Publication: 2018-10-30 19:20:03 UTC
* Number of recursive dependencies: 66

Run `cloud_details(, "graphframes")` for more info

</details>

## Newly broken

*   checking whether package ‘graphframes’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/graphframes/new/graphframes.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

## Installation

### Devel

```
* installing *source* package ‘graphframes’ ...
** package ‘graphframes’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error: object ‘type_sum’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘graphframes’
* removing ‘/tmp/workdir/graphframes/new/graphframes.Rcheck/graphframes’


```
### CRAN

```
* installing *source* package ‘graphframes’ ...
** package ‘graphframes’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (graphframes)


```
# greed

<details>

* Version: 0.5.1
* GitHub: https://github.com/comeetie/greed
* Source code: https://github.com/cran/greed
* Date/Publication: 2021-05-10 06:50:03 UTC
* Number of recursive dependencies: 115

Run `cloud_details(, "greed")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        4. ├─ggplot2::is.ggplot(plot(sol, type = "tree"))
        5. ├─base::plot(sol, type = "tree")
        6. ├─greed::plot(sol, type = "tree")
        7. │ └─greed .local(x, ...)
        8. │   └─greed:::co_dendo(x)
        9. └─base::loadNamespace(x)
       10.   ├─base::namespaceImportFrom(...)
       11.   │ └─base::asNamespace(ns)
       12.   └─base::loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]])
       13.     └─base::namespaceImportFrom(...)
       14.       └─base::importIntoEnv(impenv, impnames, ns, impvars)
      
      [ FAIL 3 | WARN 862 | SKIP 0 | PASS 187 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 47.9Mb
      sub-directories of 1Mb or more:
        libs  45.6Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 6693 marked UTF-8 strings
    ```

# groupr

<details>

* Version: 0.1.0
* GitHub: https://github.com/ngriffiths21/groupr
* Source code: https://github.com/cran/groupr
* Date/Publication: 2020-10-14 12:30:06 UTC
* Number of recursive dependencies: 58

Run `cloud_details(, "groupr")` for more info

</details>

## Newly broken

*   checking whether package ‘groupr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/groupr/new/groupr.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

## Installation

### Devel

```
* installing *source* package ‘groupr’ ...
** package ‘groupr’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘tbl_sum’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘groupr’
* removing ‘/tmp/workdir/groupr/new/groupr.Rcheck/groupr’


```
### CRAN

```
* installing *source* package ‘groupr’ ...
** package ‘groupr’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (groupr)


```
# healthyR.ts

<details>

* Version: 0.1.7
* GitHub: https://github.com/spsanderson/healthyR.ts
* Source code: https://github.com/cran/healthyR.ts
* Date/Publication: 2021-12-11 05:50:02 UTC
* Number of recursive dependencies: 193

Run `cloud_details(, "healthyR.ts")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘healthyR.ts-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ts_wfs_arima_boost
    > ### Title: Auto Arima XGBoost Workflowset Function
    > ### Aliases: ts_wfs_arima_boost
    > 
    > ### ** Examples
    > 
    > suppressPackageStartupMessages(library(modeltime))
    > suppressPackageStartupMessages(library(timetk))
    > suppressPackageStartupMessages(library(dplyr))
    > suppressPackageStartupMessages(library(tidymodels))
    Error: package or namespace load failed for ‘tidymodels’:
     object ‘type_sum’ is not exported by 'namespace:dplyr'
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘auto-model-tune.Rmd’ using rmarkdown
    Quitting from lines 23-30 (auto-model-tune.Rmd) 
    Error: processing vignette 'auto-model-tune.Rmd' failed with diagnostics:
    package or namespace load failed for 'tidymodels':
     object 'type_sum' is not exported by 'namespace:dplyr'
    --- failed re-building ‘auto-model-tune.Rmd’
    
    --- re-building ‘getting-started.Rmd’ using rmarkdown
    ══ Welcome to healthyR.ts ══════════════════════════════════════════════════════
    ...
    Warning: Removed 1920 rows containing missing values (geom_point).
    Warning: Removed 240 row(s) containing missing values (geom_path).
    Warning: Removed 240 rows containing missing values (geom_point).
    --- finished re-building ‘using-tidy-fft.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘auto-model-tune.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 11.3Mb
      sub-directories of 1Mb or more:
        doc  10.7Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘earth’
      All declared Imports should be used.
    ```

# heatwaveR

<details>

* Version: 0.4.6
* GitHub: https://github.com/robwschlegel/heatwaveR
* Source code: https://github.com/cran/heatwaveR
* Date/Publication: 2021-10-27 14:50:02 UTC
* Number of recursive dependencies: 124

Run `cloud_details(, "heatwaveR")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘MHW_to_nc.Rmd’ using rmarkdown
    --- finished re-building ‘MHW_to_nc.Rmd’
    
    --- re-building ‘OISST_preparation.Rmd’ using rmarkdown
    --- finished re-building ‘OISST_preparation.Rmd’
    
    --- re-building ‘complex_clims.Rmd’ using rmarkdown
    
    Attaching package: 'dplyr'
    ...
    --- finished re-building ‘exceedance.Rmd’
    
    --- re-building ‘gridded_event_detection.Rmd’ using rmarkdown
    --- finished re-building ‘gridded_event_detection.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘complex_clims.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# heemod

<details>

* Version: 0.14.4
* GitHub: https://github.com/pierucci/heemod
* Source code: https://github.com/cran/heemod
* Date/Publication: 2021-10-06 11:30:12 UTC
* Number of recursive dependencies: 106

Run `cloud_details(, "heemod")` for more info

</details>

## Newly broken

*   checking whether package ‘heemod’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/heemod/new/heemod.Rcheck/00install.out’ for details.
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘rgho’
    ```

## Installation

### Devel

```
* installing *source* package ‘heemod’ ...
** package ‘heemod’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘heemod’
* removing ‘/tmp/workdir/heemod/new/heemod.Rcheck/heemod’


```
### CRAN

```
* installing *source* package ‘heemod’ ...
** package ‘heemod’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (heemod)


```
# hilldiv

<details>

* Version: 1.5.1
* GitHub: https://github.com/anttonalberdi/hilldiv
* Source code: https://github.com/cran/hilldiv
* Date/Publication: 2019-10-01 14:40:02 UTC
* Number of recursive dependencies: 141

Run `cloud_details(, "hilldiv")` for more info

</details>

## Newly broken

*   checking whether package ‘hilldiv’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/hilldiv/new/hilldiv.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘hilldiv’ ...
** package ‘hilldiv’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘hilldiv’
* removing ‘/tmp/workdir/hilldiv/new/hilldiv.Rcheck/hilldiv’


```
### CRAN

```
* installing *source* package ‘hilldiv’ ...
** package ‘hilldiv’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (hilldiv)


```
# hJAM

<details>

* Version: 1.0.0
* GitHub: https://github.com/lailylajiang/hJAM
* Source code: https://github.com/cran/hJAM
* Date/Publication: 2020-02-20 14:50:05 UTC
* Number of recursive dependencies: 95

Run `cloud_details(, "hJAM")` for more info

</details>

## Newly broken

*   checking whether package ‘hJAM’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/hJAM/new/hJAM.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘hJAM’ ...
** package ‘hJAM’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘hJAM’
* removing ‘/tmp/workdir/hJAM/new/hJAM.Rcheck/hJAM’


```
### CRAN

```
* installing *source* package ‘hJAM’ ...
** package ‘hJAM’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
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
* DONE (hJAM)


```
# HS

<details>

* Version: 1.1
* GitHub: NA
* Source code: https://github.com/cran/HS
* Date/Publication: 2019-09-10 04:40:03 UTC
* Number of recursive dependencies: 95

Run `cloud_details(, "HS")` for more info

</details>

## Newly broken

*   checking whether package ‘HS’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/HS/new/HS.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘HS’ ...
** package ‘HS’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘HS’
* removing ‘/tmp/workdir/HS/new/HS.Rcheck/HS’


```
### CRAN

```
* installing *source* package ‘HS’ ...
** package ‘HS’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (HS)


```
# hypothesisr

<details>

* Version: 0.1.1
* GitHub: https://github.com/mdlincoln/hypothesisr
* Source code: https://github.com/cran/hypothesisr
* Date/Publication: 2016-07-01 16:33:31
* Number of recursive dependencies: 41

Run `cloud_details(, "hypothesisr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘hypothesisr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: hs_search
    > ### Title: Search hypothes.is annotations
    > ### Aliases: hs_search
    > 
    > ### ** Examples
    > 
    > # Search for no more than 5 annotations containing the text "ulysses"
    > hs_search(text = "ulysses", limit = 5)
    No encoding supplied: defaulting to UTF-8.
    Error: 'as_data_frame' is not an exported object from 'namespace:dplyr'
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── Error (test_search.R:36:3): Truncated results generate a message and table attribute. ──
      Error: 'as_data_frame' is not an exported object from 'namespace:dplyr'
      Backtrace:
          ▆
       1. ├─testthat::expect_message(hs_todo <- hs_search(text = "todo")) at test_search.R:36:2
       2. │ └─testthat:::quasi_capture(enquo(object), label, capture_messages)
       3. │   ├─testthat .capture(...)
       4. │   │ └─base::withCallingHandlers(...)
       5. │   └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
       6. └─hypothesisr::hs_search(text = "todo")
       7.   └─hypothesisr:::hs_search_results(query_response)
      
      [ FAIL 3 | WARN 0 | SKIP 1 | PASS 7 ]
      Error: Test failures
      Execution halted
    ```

*   checking dependencies in R code ... WARNING
    ```
    Missing or unexported object: ‘dplyr::as_data_frame’
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# iCellR

<details>

* Version: 1.6.5
* GitHub: https://github.com/rezakj/iCellR
* Source code: https://github.com/cran/iCellR
* Date/Publication: 2021-10-09 15:00:15 UTC
* Number of recursive dependencies: 151

Run `cloud_details(, "iCellR")` for more info

</details>

## Newly broken

*   checking whether package ‘iCellR’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/iCellR/new/iCellR.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘iCellR’ ...
** package ‘iCellR’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
g++ -std=gnu++14 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I/usr/local/include   -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++14 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I/usr/local/include   -fpic  -g -O2  -c jaccard_coeff.cpp -o jaccard_coeff.o
g++ -std=gnu++14 -shared -L/opt/R/4.1.1/lib/R/lib -L/usr/local/lib -o iCellR.so RcppExports.o jaccard_coeff.o -L/opt/R/4.1.1/lib/R/lib -lR
installing to /tmp/workdir/iCellR/new/iCellR.Rcheck/00LOCK-iCellR/00new/iCellR/libs
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘iCellR’
* removing ‘/tmp/workdir/iCellR/new/iCellR.Rcheck/iCellR’


```
### CRAN

```
* installing *source* package ‘iCellR’ ...
** package ‘iCellR’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
g++ -std=gnu++14 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I/usr/local/include   -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++14 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I/usr/local/include   -fpic  -g -O2  -c jaccard_coeff.cpp -o jaccard_coeff.o
g++ -std=gnu++14 -shared -L/opt/R/4.1.1/lib/R/lib -L/usr/local/lib -o iCellR.so RcppExports.o jaccard_coeff.o -L/opt/R/4.1.1/lib/R/lib -lR
installing to /tmp/workdir/iCellR/old/iCellR.Rcheck/00LOCK-iCellR/00new/iCellR/libs
** R
** data
...
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** checking absolute paths in shared objects and dynamic libraries
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (iCellR)


```
# IDE

<details>

* Version: 0.3.0
* GitHub: https://github.com/andrewzm/IDE
* Source code: https://github.com/cran/IDE
* Date/Publication: 2019-06-24 05:40:03 UTC
* Number of recursive dependencies: 116

Run `cloud_details(, "IDE")` for more info

</details>

## Newly broken

*   checking whether package ‘IDE’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/IDE/new/IDE.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘IDE_intro.Rnw’ using knitr
    Error: processing vignette 'IDE_intro.Rnw' failed with diagnostics:
    Running 'texi2dvi' on 'IDE_intro.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `algorithm.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    ...
    l.69 \usepackage
                    {algpseudocode}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    --- failed re-building ‘IDE_intro.Rnw’
    
    SUMMARY: processing the following file failed:
      ‘IDE_intro.Rnw’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

## Installation

### Devel

```
* installing *source* package ‘IDE’ ...
** package ‘IDE’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Warning in checkMatrixPackageVersion() :
  Package version inconsistency detected.
TMB was built with Matrix version 1.4.0
Current Matrix version is 1.3.4
Please re-install 'TMB' from source using install.packages('TMB', type = 'source') or ask CRAN for a binary version of 'TMB' matching CRAN's 'Matrix' package
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘IDE’
* removing ‘/tmp/workdir/IDE/new/IDE.Rcheck/IDE’


```
### CRAN

```
* installing *source* package ‘IDE’ ...
** package ‘IDE’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Warning in checkMatrixPackageVersion() :
  Package version inconsistency detected.
TMB was built with Matrix version 1.4.0
Current Matrix version is 1.3.4
...
Current Matrix version is 1.3.4
Please re-install 'TMB' from source using install.packages('TMB', type = 'source') or ask CRAN for a binary version of 'TMB' matching CRAN's 'Matrix' package
** testing if installed package can be loaded from final location
Warning in checkMatrixPackageVersion() :
  Package version inconsistency detected.
TMB was built with Matrix version 1.4.0
Current Matrix version is 1.3.4
Please re-install 'TMB' from source using install.packages('TMB', type = 'source') or ask CRAN for a binary version of 'TMB' matching CRAN's 'Matrix' package
** testing if installed package keeps a record of temporary installation path
* DONE (IDE)


```
# igate

<details>

* Version: 0.3.3
* GitHub: https://github.com/stefan-stein/igate
* Source code: https://github.com/cran/igate
* Date/Publication: 2019-09-10 22:50:06 UTC
* Number of recursive dependencies: 68

Run `cloud_details(, "igate")` for more info

</details>

## Newly broken

*   checking whether package ‘igate’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/igate/new/igate.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘kableExtra’ ‘knitr’ ‘xtable’
      All declared Imports should be used.
    ```

## Installation

### Devel

```
* installing *source* package ‘igate’ ...
** package ‘igate’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘igate’
* removing ‘/tmp/workdir/igate/new/igate.Rcheck/igate’


```
### CRAN

```
* installing *source* package ‘igate’ ...
** package ‘igate’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (igate)


```
# iglu

<details>

* Version: 3.3.0
* GitHub: NA
* Source code: https://github.com/cran/iglu
* Date/Publication: 2021-12-21 21:50:02 UTC
* Number of recursive dependencies: 123

Run `cloud_details(, "iglu")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘AGP_and_Episodes.Rmd’ using rmarkdown
    --- finished re-building ‘AGP_and_Episodes.Rmd’
    
    --- re-building ‘MAGE.Rmd’ using rmarkdown
    Gap found in data for subject id: Subject 2, that exceeds 12 hours.
    Gap found in data for subject id: Subject 2, that exceeds 12 hours.
    Quitting from lines 93-97 (MAGE.Rmd) 
    Error: processing vignette 'MAGE.Rmd' failed with diagnostics:
    object 'as_data_frame' is not exported by 'namespace:dplyr'
    ...
    --- finished re-building ‘lasagna_plots.Rmd’
    
    --- re-building ‘metrics_list.Rmd’ using rmarkdown
    --- finished re-building ‘metrics_list.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘MAGE.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘plotly’
      All declared Imports should be used.
    ```

# iheiddown

<details>

* Version: 0.8.6
* GitHub: https://github.com/jhollway/iheiddown
* Source code: https://github.com/cran/iheiddown
* Date/Publication: 2021-11-17 11:40:06 UTC
* Number of recursive dependencies: 126

Run `cloud_details(, "iheiddown")` for more info

</details>

## Newly broken

*   checking whether package ‘iheiddown’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/iheiddown/new/iheiddown.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      [1] "33% female authors"
      [1] "Average date of publication: 1963"
      [1] "Total number of pages: 7"
      [1] "Average number of pages: 7"
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 40 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test-countwords.R:4:3): You can count on count_words() ─────────────
      iheiddown::count_words("test/test.Rmd") not equal to 241.
      1/1 mismatches
      [1] 246 - 241 == 5
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 40 ]
      Error: Test failures
      Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘remotes’
      All declared Imports should be used.
    ```

## Installation

### Devel

```
* installing *source* package ‘iheiddown’ ...
** package ‘iheiddown’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘iheiddown’
* removing ‘/tmp/workdir/iheiddown/new/iheiddown.Rcheck/iheiddown’


```
### CRAN

```
* installing *source* package ‘iheiddown’ ...
** package ‘iheiddown’ successfully unpacked and MD5 sums checked
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
* DONE (iheiddown)


```
# imdbapi

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/imdbapi
* Date/Publication: 2018-04-24 08:11:08 UTC
* Number of recursive dependencies: 27

Run `cloud_details(, "imdbapi")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... WARNING
    ```
    Missing or unexported object: ‘dplyr::as_data_frame’
    ```

# immunarch

<details>

* Version: 0.6.7
* GitHub: https://github.com/immunomind/immunarch
* Source code: https://github.com/cran/immunarch
* Date/Publication: 2021-10-29 12:00:07 UTC
* Number of recursive dependencies: 176

Run `cloud_details(, "immunarch")` for more info

</details>

## Newly broken

*   checking whether package ‘immunarch’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/immunarch/new/immunarch.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking installed package size ... NOTE
    ```
      installed size is  8.5Mb
      sub-directories of 1Mb or more:
        data   4.3Mb
        doc    1.6Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dbplyr’
      All declared Imports should be used.
    ```

## Installation

### Devel

```
* installing *source* package ‘immunarch’ ...
** package ‘immunarch’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
g++ -std=gnu++14 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I/usr/local/include   -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++14 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I/usr/local/include   -fpic  -g -O2  -c fill_functions.cpp -o fill_functions.o
g++ -std=gnu++14 -shared -L/opt/R/4.1.1/lib/R/lib -L/usr/local/lib -o immunarch.so RcppExports.o fill_functions.o -L/opt/R/4.1.1/lib/R/lib -lR
installing to /tmp/workdir/immunarch/new/immunarch.Rcheck/00LOCK-immunarch/00new/immunarch/libs
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘immunarch’
* removing ‘/tmp/workdir/immunarch/new/immunarch.Rcheck/immunarch’


```
### CRAN

```
* installing *source* package ‘immunarch’ ...
** package ‘immunarch’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
g++ -std=gnu++14 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I/usr/local/include   -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++14 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I/usr/local/include   -fpic  -g -O2  -c fill_functions.cpp -o fill_functions.o
g++ -std=gnu++14 -shared -L/opt/R/4.1.1/lib/R/lib -L/usr/local/lib -o immunarch.so RcppExports.o fill_functions.o -L/opt/R/4.1.1/lib/R/lib -lR
installing to /tmp/workdir/immunarch/old/immunarch.Rcheck/00LOCK-immunarch/00new/immunarch/libs
** R
** data
...
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** checking absolute paths in shared objects and dynamic libraries
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (immunarch)


```
# inctools

<details>

* Version: 1.0.15
* GitHub: https://github.com/SACEMA/inctools
* Source code: https://github.com/cran/inctools
* Date/Publication: 2019-11-07 14:20:02 UTC
* Number of recursive dependencies: 70

Run `cloud_details(, "inctools")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... WARNING
    ```
    Missing or unexported objects:
      ‘dplyr::as_data_frame’ ‘dplyr::data_frame’
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘EstimatingIncidence.Rmd’ using rmarkdown
    --- finished re-building ‘EstimatingIncidence.Rmd’
    
    --- re-building ‘Introduction.Rmd’ using rmarkdown
    --- finished re-building ‘Introduction.Rmd’
    
    --- re-building ‘SurveyDesign.Rmd’ using rmarkdown
    --- finished re-building ‘SurveyDesign.Rmd’
    ...
    Quitting from lines 152-166 (TestCalibration.Rmd) 
    Error: processing vignette 'TestCalibration.Rmd' failed with diagnostics:
    'as_data_frame' is not an exported object from 'namespace:dplyr'
    --- failed re-building ‘TestCalibration.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘TestCalibration.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# insane

<details>

* Version: 0.1.0
* GitHub: https://github.com/mcanouil/insane
* Source code: https://github.com/cran/insane
* Date/Publication: 2020-11-04 12:10:03 UTC
* Number of recursive dependencies: 132

Run `cloud_details(, "insane")` for more info

</details>

## Newly broken

*   checking whether package ‘insane’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/insane/new/insane.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

## Installation

### Devel

```
* installing *source* package ‘insane’ ...
** package ‘insane’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘insane’
* removing ‘/tmp/workdir/insane/new/insane.Rcheck/insane’


```
### CRAN

```
* installing *source* package ‘insane’ ...
** package ‘insane’ successfully unpacked and MD5 sums checked
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
* DONE (insane)


```
# iNZightTools

<details>

* Version: 1.12.2
* GitHub: https://github.com/iNZightVIT/iNZightTools
* Source code: https://github.com/cran/iNZightTools
* Date/Publication: 2022-01-18 23:32:42 UTC
* Number of recursive dependencies: 88

Run `cloud_details(, "iNZightTools")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘iNZightTools-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: filterNumeric
    > ### Title: Filter data by levels of a numeric variables
    > ### Aliases: filterNumeric
    > 
    > ### ** Examples
    > 
    > filtered <- filterNumeric(iris, var = "Sepal.Length", op = "<=", num = 5)
    ...
        dotchart
    
    > data(api)
    > svy <- svydesign(~dnum+snum, weights = ~pw, fpc = ~fpc1+fpc2, data = apiclus2)
    > (svy_filtered <- filterNumeric(svy, var = "api00", op = "<", num = 700))
    2 - level Cluster Sampling design
    With (24, 63) clusters.
    Called via srvyr
    Error: 'type_sum' is not an exported object from 'namespace:dplyr'
    Execution halted
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘tidyverse’
    ```

# iotables

<details>

* Version: 0.4.7
* GitHub: https://github.com/rOpenGov/iotables
* Source code: https://github.com/cran/iotables
* Date/Publication: 2021-12-22 17:30:02 UTC
* Number of recursive dependencies: 115

Run `cloud_details(, "iotables")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘environmental_impact.Rmd’ using rmarkdown
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    ...
    Columns and rows of CPA_L68A, CPA_T, CPA_U are all zeros and will be removed.
    Joining, by = c("prod_na", "CPA_A01", "CPA_A02", "CPA_A03", "CPA_B", "CPA_C10-12", "CPA_C13-15", "CPA_C16", "CPA_C17", "CPA_C18", "CPA_C19", "CPA_C20", "CPA_C21", "CPA_C22", "CPA_C23", "CPA_C24", "CPA_C25", "CPA_C26", "CPA_C27", "CPA_C28", "CPA_C29", "CPA_C30", "CPA_C31_32", "CPA_C33", "CPA_D", "CPA_E36", "CPA_E37-39", "CPA_F", "CPA_G45", "CPA_G46", "CPA_H49", "CPA_H50", "CPA_H51", "CPA_H52", "CPA_H53", "CPA_I", "CPA_J58", "CPA_J59_60", "CPA_J61", "CPA_J62_63", "CPA_K64", "CPA_K65", "CPA_K66", "CPA_L68B", "CPA_M69_70", "CPA_M71", "CPA_M72", "CPA_M73", "CPA_M74_75", "CPA_N77", "CPA_N78", "CPA_N79", "CPA_N80-82", "CPA_O", "CPA_P", "CPA_Q86", "CPA_Q87_88", "CPA_R90-92", "CPA_R93", "CPA_S94", "CPA_S95", "CPA_S96")
    Columns and rows of CPA_L68A are all zeros and will be removed.
    --- finished re-building ‘working_with_eurostat.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘environmental_impact.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# IRISMustangMetrics

<details>

* Version: 2.4.4
* GitHub: NA
* Source code: https://github.com/cran/IRISMustangMetrics
* Date/Publication: 2021-03-20 19:20:02 UTC
* Number of recursive dependencies: 29

Run `cloud_details(, "IRISMustangMetrics")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... WARNING
    ```
    Missing or unexported object: ‘dplyr::data_frame’
    ```

# isoreader

<details>

* Version: 1.3.2
* GitHub: https://github.com/isoverse/isoreader
* Source code: https://github.com/cran/isoreader
* Date/Publication: 2021-11-18 22:10:02 UTC
* Number of recursive dependencies: 110

Run `cloud_details(, "isoreader")` for more info

</details>

## Newly broken

*   checking whether package ‘isoreader’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/isoreader/new/isoreader.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘isoreader’ ...
** package ‘isoreader’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘isoreader’
* removing ‘/tmp/workdir/isoreader/new/isoreader.Rcheck/isoreader’


```
### CRAN

```
* installing *source* package ‘isoreader’ ...
** package ‘isoreader’ successfully unpacked and MD5 sums checked
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
* DONE (isoreader)


```
# isotracer

<details>

* Version: 1.1.0
* GitHub: NA
* Source code: https://github.com/cran/isotracer
* Date/Publication: 2021-12-19 16:10:02 UTC
* Number of recursive dependencies: 144

Run `cloud_details(, "isotracer")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      > 
      > test_check("isotracer")
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 282 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test-integration.R:287:5): Models using Euler and matrix exponential solvers give similar posteriors ──
      min(overlaps) > 0.7 is not TRUE
      
      `actual`:   FALSE
      `expected`: TRUE 
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 282 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 79.7Mb
      sub-directories of 1Mb or more:
        doc    2.8Mb
        libs  74.8Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘rstantools’
      All declared Imports should be used.
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# ITNr

<details>

* Version: 0.6.0
* GitHub: NA
* Source code: https://github.com/cran/ITNr
* Date/Publication: 2020-03-11 17:30:13 UTC
* Number of recursive dependencies: 66

Run `cloud_details(, "ITNr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ITNr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: core_periphery_weighted
    > ### Title: Core-Periphery for Weighted Networks
    > ### Aliases: core_periphery_weighted
    > 
    > ### ** Examples
    > 
    > require(igraph)
    ...
    > ##Add edge weights
    > E(ITN)$weight<-runif(ecount(ITN), 0, 1)
    > 
    > ##Add vertex names
    > V(ITN)$name<-1:vcount(ITN)
    > 
    > ##Implement core-periphery algorithm
    > ITNcp<-core_periphery_weighted(ITN,"directed")
    Error: 'as_data_frame' is not an exported object from 'namespace:dplyr'
    Execution halted
    ```

*   checking dependencies in R code ... WARNING
    ```
    Missing or unexported object: ‘dplyr::as_data_frame’
    ```

# JSmediation

<details>

* Version: 0.2.0
* GitHub: https://github.com/cedricbatailler/JSmediation
* Source code: https://github.com/cran/JSmediation
* Date/Publication: 2021-09-02 22:30:08 UTC
* Number of recursive dependencies: 205

Run `cloud_details(, "JSmediation")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        2.   ├─base::namespaceImportFrom(...)
        3.   │ └─base::asNamespace(ns)
        4.   └─base::loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]])
        5.     ├─base::namespaceImportFrom(...)
        6.     │ └─base::asNamespace(ns)
        7.     └─base::loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]])
        8.       ├─base::namespaceImportFrom(...)
        9.       │ └─base::asNamespace(ns)
       10.       └─base::loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]])
       11.         └─base::namespaceImportFrom(...)
       12.           └─base::importIntoEnv(impenv, impnames, ns, impvars)
      
      [ FAIL 1 | WARN 0 | SKIP 1 | PASS 90 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘utils’
      All declared Imports should be used.
    ```

# jsmodule

<details>

* Version: 1.3.0
* GitHub: https://github.com/jinseob2kim/jsmodule
* Source code: https://github.com/cran/jsmodule
* Date/Publication: 2022-01-06 13:10:02 UTC
* Number of recursive dependencies: 225

Run `cloud_details(, "jsmodule")` for more info

</details>

## Newly broken

*   checking whether package ‘jsmodule’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/jsmodule/new/jsmodule.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘devEMF’ ‘survC1’
      All declared Imports should be used.
    ```

## Installation

### Devel

```
* installing *source* package ‘jsmodule’ ...
** package ‘jsmodule’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘jsmodule’
* removing ‘/tmp/workdir/jsmodule/new/jsmodule.Rcheck/jsmodule’


```
### CRAN

```
* installing *source* package ‘jsmodule’ ...
** package ‘jsmodule’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (jsmodule)


```
# jstor

<details>

* Version: 0.3.10
* GitHub: https://github.com/ropensci/jstor
* Source code: https://github.com/cran/jstor
* Date/Publication: 2021-12-08 08:50:07 UTC
* Number of recursive dependencies: 77

Run `cloud_details(, "jstor")` for more info

</details>

## Newly broken

*   checking whether package ‘jstor’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/jstor/new/jstor.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘jstor’ ...
** package ‘jstor’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘jstor’
* removing ‘/tmp/workdir/jstor/new/jstor.Rcheck/jstor’


```
### CRAN

```
* installing *source* package ‘jstor’ ...
** package ‘jstor’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (jstor)


```
# JWileymisc

<details>

* Version: 1.2.0
* GitHub: https://github.com/JWiley/JWileymisc
* Source code: https://github.com/cran/JWileymisc
* Date/Publication: 2020-08-31 06:30:06 UTC
* Number of recursive dependencies: 142

Run `cloud_details(, "JWileymisc")` for more info

</details>

## Newly broken

*   checking whether package ‘JWileymisc’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/JWileymisc/new/JWileymisc.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘JWileymisc’ ...
** package ‘JWileymisc’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘JWileymisc’
* removing ‘/tmp/workdir/JWileymisc/new/JWileymisc.Rcheck/JWileymisc’


```
### CRAN

```
* installing *source* package ‘JWileymisc’ ...
** package ‘JWileymisc’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (JWileymisc)


```
# kerastuneR

<details>

* Version: 0.1.0.4
* GitHub: https://github.com/EagerAI/kerastuneR
* Source code: https://github.com/cran/kerastuneR
* Date/Publication: 2022-01-10 16:02:43 UTC
* Number of recursive dependencies: 111

Run `cloud_details(, "kerastuneR")` for more info

</details>

## Newly broken

*   checking whether package ‘kerastuneR’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/kerastuneR/new/kerastuneR.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘keras’
      All declared Imports should be used.
    ```

## Installation

### Devel

```
* installing *source* package ‘kerastuneR’ ...
** package ‘kerastuneR’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in namespaceExport(ns, exports) : undefined exports: as_data_frame
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace -> namespaceExport
Execution halted
ERROR: lazy loading failed for package ‘kerastuneR’
* removing ‘/tmp/workdir/kerastuneR/new/kerastuneR.Rcheck/kerastuneR’


```
### CRAN

```
* installing *source* package ‘kerastuneR’ ...
** package ‘kerastuneR’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (kerastuneR)


```
# konfound

<details>

* Version: 0.4.0
* GitHub: https://github.com/jrosen48/konfound
* Source code: https://github.com/cran/konfound
* Date/Publication: 2021-06-01 07:40:05 UTC
* Number of recursive dependencies: 116

Run `cloud_details(, "konfound")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘konfound-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: konfound
    > ### Title: Perform sensitivity analysis on fitted models
    > ### Aliases: konfound
    > 
    > ### ** Examples
    > 
    > # using lm() for linear models
    ...
    Using Rubin's causal model to interpret the robustness of causal inferences.
    Education, Evaluation and Policy Analysis, 35 437-460.
    For more detailed output, consider setting `to_return` to table
    To consider other predictors of interest, consider setting `test_all` to TRUE.
    > konfound(m1, wt, test_all = TRUE)
    Note that this output is calculated based on the correlation-based approach used in mkonfound()
    Warning in data.frame(t = est_eff/std_err, df = (n_obs - n_covariates -  :
      row names were found from a short variable and have been discarded
    Error: 'data_frame' is not an exported object from 'namespace:dplyr'
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Error: 'data_frame' is not an exported object from 'namespace:dplyr'
      Backtrace:
          ▆
       1. └─konfound::konfound(testmod1, texp, test_all = TRUE, to_return = "raw_output") at test-pkonfound.R:10:0
       2.   └─konfound:::konfound_lm(...)
       3.     └─konfound::mkonfound(d, .data$t, .data$df)
       4.       ├─base::suppressWarnings(...)
       5.       │ └─base::withCallingHandlers(...)
       6.       └─purrr::map2_dfr(.x = t_vec, .y = df_vec, .f = core_sensitivity_mkonfound)
       7.         └─purrr::map2(.x, .y, .f, ...)
       8.           └─konfound .f(.x[[i]], .y[[i]], ...)
      
      [ FAIL 2 | WARN 3 | SKIP 0 | PASS 3 ]
      Error: Test failures
      Execution halted
    ```

*   checking dependencies in R code ... WARNING
    ```
    Namespaces in Imports field not imported from:
      ‘mice’ ‘tibble’
      All declared Imports should be used.
    Missing or unexported object: ‘dplyr::data_frame’
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘introduction-to-konfound.Rmd’ using rmarkdown
    Quitting from lines 149-150 (introduction-to-konfound.Rmd) 
    Error: processing vignette 'introduction-to-konfound.Rmd' failed with diagnostics:
    'data_frame' is not an exported object from 'namespace:dplyr'
    --- failed re-building ‘introduction-to-konfound.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘introduction-to-konfound.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘mice’ ‘tibble’
      All declared Imports should be used.
    ```

# labelled

<details>

* Version: 2.9.0
* GitHub: https://github.com/larmarange/labelled
* Source code: https://github.com/cran/labelled
* Date/Publication: 2021-10-29 19:40:02 UTC
* Number of recursive dependencies: 101

Run `cloud_details(, "labelled")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘intro_labelled.Rmd’ using rmarkdown
    Quitting from lines 480-486 (intro_labelled.Rmd) 
    Error: processing vignette 'intro_labelled.Rmd' failed with diagnostics:
    could not find function "data_frame"
    --- failed re-building ‘intro_labelled.Rmd’
    
    --- re-building ‘look_for.Rmd’ using rmarkdown
    
    Attaching package: 'dplyr'
    ...
    --- finished re-building ‘look_for.Rmd’
    
    --- re-building ‘missing_values.Rmd’ using rmarkdown
    --- finished re-building ‘missing_values.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘intro_labelled.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘memisc’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘memisc’
    ```

# lilikoi

<details>

* Version: 2.0.4
* GitHub: NA
* Source code: https://github.com/cran/lilikoi
* Date/Publication: 2021-01-15 09:40:09 UTC
* Number of recursive dependencies: 216

Run `cloud_details(, "lilikoi")` for more info

</details>

## Newly broken

*   checking whether package ‘lilikoi’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/lilikoi/new/lilikoi.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘M3C’
      All declared Imports should be used.
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

## Installation

### Devel

```
* installing *source* package ‘lilikoi’ ...
** package ‘lilikoi’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘lilikoi’
* removing ‘/tmp/workdir/lilikoi/new/lilikoi.Rcheck/lilikoi’


```
### CRAN

```
* installing *source* package ‘lilikoi’ ...
** package ‘lilikoi’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (lilikoi)


```
# LongCART

<details>

* Version: 3.1
* GitHub: https://github.com/madanstat/LongCART
* Source code: https://github.com/cran/LongCART
* Date/Publication: 2021-05-31 07:10:36 UTC
* Number of recursive dependencies: 110

Run `cloud_details(, "LongCART")` for more info

</details>

## Newly broken

*   checking whether package ‘LongCART’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/LongCART/new/LongCART.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘LongCART’ ...
** package ‘LongCART’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘ggpubr’:
 object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘LongCART’
* removing ‘/tmp/workdir/LongCART/new/LongCART.Rcheck/LongCART’


```
### CRAN

```
* installing *source* package ‘LongCART’ ...
** package ‘LongCART’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (LongCART)


```
# LongDat

<details>

* Version: 1.0.0
* GitHub: https://github.com/CCY-dev/LongDat
* Source code: https://github.com/cran/LongDat
* Date/Publication: 2022-01-18 20:42:44 UTC
* Number of recursive dependencies: 178

Run `cloud_details(, "LongDat")` for more info

</details>

## Newly broken

*   checking whether package ‘LongDat’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/LongDat/new/LongDat.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘LongDat’ ...
** package ‘LongDat’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Warning in checkMatrixPackageVersion() :
  Package version inconsistency detected.
TMB was built with Matrix version 1.4.0
Current Matrix version is 1.3.4
Please re-install 'TMB' from source using install.packages('TMB', type = 'source') or ask CRAN for a binary version of 'TMB' matching CRAN's 'Matrix' package
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘LongDat’
* removing ‘/tmp/workdir/LongDat/new/LongDat.Rcheck/LongDat’


```
### CRAN

```
* installing *source* package ‘LongDat’ ...
** package ‘LongDat’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Warning in checkMatrixPackageVersion() :
  Package version inconsistency detected.
...
Current Matrix version is 1.3.4
Please re-install 'TMB' from source using install.packages('TMB', type = 'source') or ask CRAN for a binary version of 'TMB' matching CRAN's 'Matrix' package
** testing if installed package can be loaded from final location
Warning in checkMatrixPackageVersion() :
  Package version inconsistency detected.
TMB was built with Matrix version 1.4.0
Current Matrix version is 1.3.4
Please re-install 'TMB' from source using install.packages('TMB', type = 'source') or ask CRAN for a binary version of 'TMB' matching CRAN's 'Matrix' package
** testing if installed package keeps a record of temporary installation path
* DONE (LongDat)


```
# lpirfs

<details>

* Version: 0.2.0
* GitHub: https://github.com/adaemmerp/lpirfs
* Source code: https://github.com/cran/lpirfs
* Date/Publication: 2021-03-23 16:10:02 UTC
* Number of recursive dependencies: 117

Run `cloud_details(, "lpirfs")` for more info

</details>

## Newly broken

*   checking whether package ‘lpirfs’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/lpirfs/new/lpirfs.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking installed package size ... NOTE
    ```
      installed size is 10.6Mb
      sub-directories of 1Mb or more:
        libs   9.1Mb
    ```

## Installation

### Devel

```
* installing *source* package ‘lpirfs’ ...
** package ‘lpirfs’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG -I../inst/include/ -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include  -fopenmp -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG -I../inst/include/ -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include  -fopenmp -fpic  -g -O2  -c get_vals_lagcrit.cpp -o get_vals_lagcrit.o
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG -I../inst/include/ -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include  -fopenmp -fpic  -g -O2  -c hp_filter.cpp -o hp_filter.o
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG -I../inst/include/ -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include  -fopenmp -fpic  -g -O2  -c newey_west.cpp -o newey_west.o
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG -I../inst/include/ -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include  -fopenmp -fpic  -g -O2  -c newey_west_pw.cpp -o newey_west_pw.o
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG -I../inst/include/ -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include  -fopenmp -fpic  -g -O2  -c newey_west_tsls.cpp -o newey_west_tsls.o
...
installing to /tmp/workdir/lpirfs/new/lpirfs.Rcheck/00LOCK-lpirfs/00new/lpirfs/libs
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘lpirfs’
* removing ‘/tmp/workdir/lpirfs/new/lpirfs.Rcheck/lpirfs’


```
### CRAN

```
* installing *source* package ‘lpirfs’ ...
** package ‘lpirfs’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG -I../inst/include/ -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include  -fopenmp -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG -I../inst/include/ -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include  -fopenmp -fpic  -g -O2  -c get_vals_lagcrit.cpp -o get_vals_lagcrit.o
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG -I../inst/include/ -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include  -fopenmp -fpic  -g -O2  -c hp_filter.cpp -o hp_filter.o
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG -I../inst/include/ -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include  -fopenmp -fpic  -g -O2  -c newey_west.cpp -o newey_west.o
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG -I../inst/include/ -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include  -fopenmp -fpic  -g -O2  -c newey_west_pw.cpp -o newey_west_pw.o
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG -I../inst/include/ -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include  -fopenmp -fpic  -g -O2  -c newey_west_tsls.cpp -o newey_west_tsls.o
...
** help
*** installing help indices
*** copying figures
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** checking absolute paths in shared objects and dynamic libraries
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (lpirfs)


```
# MachineShop

<details>

* Version: 3.2.0
* GitHub: https://github.com/brian-j-smith/MachineShop
* Source code: https://github.com/cran/MachineShop
* Date/Publication: 2021-12-06 15:10:02 UTC
* Number of recursive dependencies: 199

Run `cloud_details(, "MachineShop")` for more info

</details>

## Newly broken

*   checking whether package ‘MachineShop’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/MachineShop/new/MachineShop.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘MachineShop’ ...
** package ‘MachineShop’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
gcc -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I/usr/local/include   -fpic  -g -O2  -c hazfit_efron.c -o hazfit_efron.o
gcc -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I/usr/local/include   -fpic  -g -O2  -c init.c -o init.o
gcc -shared -L/opt/R/4.1.1/lib/R/lib -L/usr/local/lib -o MachineShop.so hazfit_efron.o init.o -L/opt/R/4.1.1/lib/R/lib -lR
installing to /tmp/workdir/MachineShop/new/MachineShop.Rcheck/00LOCK-MachineShop/00new/MachineShop/libs
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘type_sum’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘MachineShop’
* removing ‘/tmp/workdir/MachineShop/new/MachineShop.Rcheck/MachineShop’


```
### CRAN

```
* installing *source* package ‘MachineShop’ ...
** package ‘MachineShop’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
gcc -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I/usr/local/include   -fpic  -g -O2  -c hazfit_efron.c -o hazfit_efron.o
gcc -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I/usr/local/include   -fpic  -g -O2  -c init.c -o init.o
gcc -shared -L/opt/R/4.1.1/lib/R/lib -L/usr/local/lib -o MachineShop.so hazfit_efron.o init.o -L/opt/R/4.1.1/lib/R/lib -lR
installing to /tmp/workdir/MachineShop/old/MachineShop.Rcheck/00LOCK-MachineShop/00new/MachineShop/libs
** R
** data
...
** help
*** installing help indices
*** copying figures
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** checking absolute paths in shared objects and dynamic libraries
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (MachineShop)


```
# mappoly

<details>

* Version: 0.3.0
* GitHub: https://github.com/mmollina/MAPpoly
* Source code: https://github.com/cran/mappoly
* Date/Publication: 2022-01-11 09:22:41 UTC
* Number of recursive dependencies: 173

Run `cloud_details(, "mappoly")` for more info

</details>

## Newly broken

*   checking whether package ‘mappoly’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/mappoly/new/mappoly.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking installed package size ... NOTE
    ```
      installed size is 20.4Mb
      sub-directories of 1Mb or more:
        R      2.7Mb
        data   2.4Mb
        libs  13.5Mb
    ```

## Installation

### Devel

```
* installing *source* package ‘mappoly’ ...
** package ‘mappoly’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppParallel/include' -I/usr/local/include   -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppParallel/include' -I/usr/local/include   -fpic  -g -O2  -c calc_genoprob.cpp -o calc_genoprob.o
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppParallel/include' -I/usr/local/include   -fpic  -g -O2  -c calc_genoprob_based_on_phased_marker_blocks.cpp -o calc_genoprob_based_on_phased_marker_blocks.o
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppParallel/include' -I/usr/local/include   -fpic  -g -O2  -c calc_loglike_given_map.cpp -o calc_loglike_given_map.o
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppParallel/include' -I/usr/local/include   -fpic  -g -O2  -c combinatorial.cpp -o combinatorial.o
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppParallel/include' -I/usr/local/include   -fpic  -g -O2  -c est_hmm_map_based_on_phased_mrk_blocks.cpp -o est_hmm_map_based_on_phased_mrk_blocks.o
...
installing to /tmp/workdir/mappoly/new/mappoly.Rcheck/00LOCK-mappoly/00new/mappoly/libs
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘mappoly’
* removing ‘/tmp/workdir/mappoly/new/mappoly.Rcheck/mappoly’


```
### CRAN

```
* installing *source* package ‘mappoly’ ...
** package ‘mappoly’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppParallel/include' -I/usr/local/include   -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppParallel/include' -I/usr/local/include   -fpic  -g -O2  -c calc_genoprob.cpp -o calc_genoprob.o
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppParallel/include' -I/usr/local/include   -fpic  -g -O2  -c calc_genoprob_based_on_phased_marker_blocks.cpp -o calc_genoprob_based_on_phased_marker_blocks.o
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppParallel/include' -I/usr/local/include   -fpic  -g -O2  -c calc_loglike_given_map.cpp -o calc_loglike_given_map.o
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppParallel/include' -I/usr/local/include   -fpic  -g -O2  -c combinatorial.cpp -o combinatorial.o
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.1.1/lib/R/site-library/RcppParallel/include' -I/usr/local/include   -fpic  -g -O2  -c est_hmm_map_based_on_phased_mrk_blocks.cpp -o est_hmm_map_based_on_phased_mrk_blocks.o
...
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** checking absolute paths in shared objects and dynamic libraries
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (mappoly)


```
# MetabolicSurv

<details>

* Version: 1.1.2
* GitHub: https://github.com/OlajumokeEvangelina/MetabolicSurv
* Source code: https://github.com/cran/MetabolicSurv
* Date/Publication: 2021-06-11 08:30:02 UTC
* Number of recursive dependencies: 137

Run `cloud_details(, "MetabolicSurv")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘MetabolicSurv-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: SurvPcaClass
    > ### Title: Survival PCA and Classification for metabolic data
    > ### Aliases: SurvPcaClass
    > 
    > ### ** Examples
    > 
    > ## FIRSTLY SIMULATING A METABOLIC SURVIVAL DATA
    > Data = MSData(nPatients = 100, nMet = 150, Prop = 0.5)
    > 
    > ## USING THE FUNCTION
    > Result = SurvPcaClass(Survival=Data$Survival, Mdata=t(Data$Mdata),
    + Censor=Data$Censor, Reduce = FALSE, Select = 150,
    + Prognostic = Data$Prognostic, Plots = FALSE, Quantile = 0.5)
    Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
    Execution halted
    ```

# metaCluster

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/metaCluster
* Date/Publication: 2021-09-30 08:10:02 UTC
* Number of recursive dependencies: 123

Run `cloud_details(, "metaCluster")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘metaCluster-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: clust.suite
    > ### Title: Determination of Suitable Clustering Algorithm for Metagenomics
    > ###   Data
    > ### Aliases: clust.suite
    > ### Keywords: kmeans kmedoids Fuzzy-kmeans DBSCAN Hierarchical Metagenomics
    > ###   Binning
    > 
    > ### ** Examples
    > 
    > 
    > library(metaCluster)
    > data(metafeatures)
    > result <- clust.suite(metafeatures[1:200,],8,0.5,10)
    Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
    Execution halted
    ```

# MetaIntegrator

<details>

* Version: 2.1.3
* GitHub: NA
* Source code: https://github.com/cran/MetaIntegrator
* Date/Publication: 2020-02-26 13:00:11 UTC
* Number of recursive dependencies: 177

Run `cloud_details(, "MetaIntegrator")` for more info

</details>

## Newly broken

*   checking whether package ‘MetaIntegrator’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/MetaIntegrator/new/MetaIntegrator.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘BiocManager’ ‘DT’ ‘GEOmetadb’ ‘RMySQL’ ‘RSQLite’ ‘gplots’ ‘pheatmap’
      ‘readr’
      All declared Imports should be used.
    ```

## Installation

### Devel

```
* installing *source* package ‘MetaIntegrator’ ...
** package ‘MetaIntegrator’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘MetaIntegrator’
* removing ‘/tmp/workdir/MetaIntegrator/new/MetaIntegrator.Rcheck/MetaIntegrator’


```
### CRAN

```
* installing *source* package ‘MetaIntegrator’ ...
** package ‘MetaIntegrator’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (MetaIntegrator)


```
# metaviz

<details>

* Version: 0.3.1
* GitHub: https://github.com/Mkossmeier/metaviz
* Source code: https://github.com/cran/metaviz
* Date/Publication: 2020-04-09 09:10:08 UTC
* Number of recursive dependencies: 128

Run `cloud_details(, "metaviz")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘metaviz-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: viz_forest
    > ### Title: Forest plot variants for meta-analyses
    > ### Aliases: viz_forest
    > 
    > ### ** Examples
    > 
    > library(metaviz)
    ...
    + Name = exrehab[, "study_name"],
    + eventsT = paste(exrehab$ai, "/", exrehab$ai + exrehab$bi, sep = ""),
    + eventsC = paste(exrehab$ci, "/", exrehab$ci + exrehab$di, sep = "")),
    + summary_table = data.frame(
    + Name = "Summary",
    + eventsT = paste(sum(exrehab$ai), "/", sum(exrehab$ai + exrehab$bi), sep = ""),
    + eventsC = paste(sum(exrehab$ci), "/", sum(exrehab$ci + exrehab$di), sep = "")),
    + table_layout = matrix(c(1, 1, 2, 2, 3), nrow = 1))
    Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘funnelinf.Rmd’ using rmarkdown
    Warning in nullabor::decrypt("EwkU v7d7 9S YTV9d9TS Wo") :
      NAs introduced by coercion
    To see the solution run nullabor::decrypt("shmT D0a0 Ku 6EdKaKEu c5")
    To see the solution run nullabor::decrypt("shmT D0a0 Ku 6EdKaKEu 11")
    Warning in nullabor::decrypt("EwkU v7d7 9S YTV9d9TS Wu") :
      NAs introduced by coercion
    To see the solution run nullabor::decrypt("shmT D0a0 Ku 6EdKaKEu 11")
    Warning in nullabor::decrypt("EwkU v7d7 9S YTV9d9TS WW") :
    ...
    Quitting from lines 106-113 (metaviz.Rmd) 
    Error: processing vignette 'metaviz.Rmd' failed with diagnostics:
    object 'as_data_frame' is not exported by 'namespace:dplyr'
    --- failed re-building ‘metaviz.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘metaviz.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# mFD

<details>

* Version: 1.0.1
* GitHub: https://github.com/CmlMagneville/mFD
* Source code: https://github.com/cran/mFD
* Date/Publication: 2021-12-16 10:10:06 UTC
* Number of recursive dependencies: 139

Run `cloud_details(, "mFD")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘mFD-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: traits.faxes.cor
    > ### Title: Correlation between Traits and Axes
    > ### Aliases: traits.faxes.cor
    > 
    > ### ** Examples
    > 
    > # Load Species x Traits Data
    ...
    > mFD::traits.faxes.cor(
    +   sp_tr          = fruits_traits, 
    +   sp_faxes_coord = sp_faxes_coord_fruits, 
    +   tr_nm          = NULL, 
    +   faxes_nm       = NULL,
    +   name_file      = NULL, 
    +   color_signif   = "darkblue",
    +   color_non_signif = "gray80")
    Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Compute_and_interpret_quality_of_functional_spaces.Rmd’ using rmarkdown
    --- finished re-building ‘Compute_and_interpret_quality_of_functional_spaces.Rmd’
    
    --- re-building ‘Compute_functional_hill_indices.Rmd’ using rmarkdown
    --- finished re-building ‘Compute_functional_hill_indices.Rmd’
    
    --- re-building ‘Continuous_traits_framework.Rmd’ using rmarkdown
    --- finished re-building ‘Continuous_traits_framework.Rmd’
    ...
    Quitting from lines 654-658 (mFD_general_workflow.Rmd) 
    Error: processing vignette 'mFD_general_workflow.Rmd' failed with diagnostics:
    object 'as_data_frame' is not exported by 'namespace:dplyr'
    --- failed re-building ‘mFD_general_workflow.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘mFD_general_workflow.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# miceRanger

<details>

* Version: 1.5.0
* GitHub: https://github.com/FarrellDay/miceRanger
* Source code: https://github.com/cran/miceRanger
* Date/Publication: 2021-09-06 15:20:02 UTC
* Number of recursive dependencies: 111

Run `cloud_details(, "miceRanger")` for more info

</details>

## Newly broken

*   checking whether package ‘miceRanger’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/miceRanger/new/miceRanger.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘miceRanger’ ...
** package ‘miceRanger’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘miceRanger’
* removing ‘/tmp/workdir/miceRanger/new/miceRanger.Rcheck/miceRanger’


```
### CRAN

```
* installing *source* package ‘miceRanger’ ...
** package ‘miceRanger’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (miceRanger)


```
# microbial

<details>

* Version: 0.0.20
* GitHub: NA
* Source code: https://github.com/cran/microbial
* Date/Publication: 2021-11-01 15:40:02 UTC
* Number of recursive dependencies: 178

Run `cloud_details(, "microbial")` for more info

</details>

## Newly broken

*   checking whether package ‘microbial’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/microbial/new/microbial.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘testthat’
      All declared Imports should be used.
    ```

## Installation

### Devel

```
* installing *source* package ‘microbial’ ...
** package ‘microbial’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘microbial’
* removing ‘/tmp/workdir/microbial/new/microbial.Rcheck/microbial’


```
### CRAN

```
* installing *source* package ‘microbial’ ...
** package ‘microbial’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (microbial)


```
# mind

<details>

* Version: 0.1.4
* GitHub: NA
* Source code: https://github.com/cran/mind
* Date/Publication: 2021-09-09 14:00:02 UTC
* Number of recursive dependencies: 160

Run `cloud_details(, "mind")` for more info

</details>

## Newly broken

*   checking whether package ‘mind’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/mind/new/mind.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

## Installation

### Devel

```
* installing *source* package ‘mind’ ...
** package ‘mind’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘mind’
* removing ‘/tmp/workdir/mind/new/mind.Rcheck/mind’


```
### CRAN

```
* installing *source* package ‘mind’ ...
** package ‘mind’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (mind)


```
# missingHE

<details>

* Version: 1.4.1
* GitHub: NA
* Source code: https://github.com/cran/missingHE
* Date/Publication: 2020-06-25 21:40:02 UTC
* Number of recursive dependencies: 123

Run `cloud_details(, "missingHE")` for more info

</details>

## Newly broken

*   checking whether package ‘missingHE’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/missingHE/new/missingHE.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘mcmcr’
      All declared Imports should be used.
    ```

## Installation

### Devel

```
* installing *source* package ‘missingHE’ ...
** package ‘missingHE’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘missingHE’
* removing ‘/tmp/workdir/missingHE/new/missingHE.Rcheck/missingHE’


```
### CRAN

```
* installing *source* package ‘missingHE’ ...
** package ‘missingHE’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (missingHE)


```
# modelDown

<details>

* Version: 1.1
* GitHub: https://github.com/ModelOriented/modelDown
* Source code: https://github.com/cran/modelDown
* Date/Publication: 2020-04-15 00:30:03 UTC
* Number of recursive dependencies: 136

Run `cloud_details(, "modelDown")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error (test_parseExplainers.R:2:1): (code run outside of `test_that()`) ─────
      Error: package or namespace load failed for 'useful':
       object 'data_frame' is not exported by 'namespace:dplyr'
      Backtrace:
          ▆
       1. └─base::library(useful) at test_parseExplainers.R:2:0
       2.   └─base::tryCatch(...)
       3.     └─base tryCatchList(expr, classes, parentenv, handlers)
       4.       └─base tryCatchOne(expr, names, parentenv, handlers[[1L]])
       5.         └─value[[3L]](cond)
      
      [ FAIL 1 | WARN 5 | SKIP 0 | PASS 2 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘DALEX’ ‘DT’ ‘auditor’ ‘breakDown’ ‘drifter’ ‘svglite’
      All declared Imports should be used.
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# modeltime

<details>

* Version: 1.1.1
* GitHub: https://github.com/business-science/modeltime
* Source code: https://github.com/cran/modeltime
* Date/Publication: 2022-01-12 16:22:42 UTC
* Number of recursive dependencies: 238

Run `cloud_details(, "modeltime")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘modeltime-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: adam_params
    > ### Title: Tuning Parameters for ADAM Models
    > ### Aliases: adam_params use_constant regressors_treatment
    > ###   outliers_treatment probability_model distribution
    > ###   information_criteria select_order
    > 
    > ### ** Examples
    > 
    > use_constant()
    Error: object ‘type_sum’ is not exported by 'namespace:dplyr'
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      The following object is masked from 'package:forecast':
      
          forecast
      
      This is package "smooth", v3.1.5
      
      > library(greybox)
      > 
      > library(stats)
      > 
      > library(tidymodels)
      Error: package or namespace load failed for 'tidymodels':
       object 'type_sum' is not exported by 'namespace:dplyr'
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘getting-started-with-modeltime.Rmd’ using rmarkdown
    Quitting from lines 82-91 (getting-started-with-modeltime.Rmd) 
    Error: processing vignette 'getting-started-with-modeltime.Rmd' failed with diagnostics:
    package or namespace load failed for 'tidymodels':
     object 'type_sum' is not exported by 'namespace:dplyr'
    --- failed re-building ‘getting-started-with-modeltime.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘getting-started-with-modeltime.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# modeltime.ensemble

<details>

* Version: 1.0.0
* GitHub: https://github.com/business-science/modeltime.ensemble
* Source code: https://github.com/cran/modeltime.ensemble
* Date/Publication: 2021-10-19 17:50:02 UTC
* Number of recursive dependencies: 215

Run `cloud_details(, "modeltime.ensemble")` for more info

</details>

## Newly broken

*   checking whether package ‘modeltime.ensemble’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/modeltime.ensemble/new/modeltime.ensemble.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘parsnip’
      All declared Imports should be used.
    ```

## Installation

### Devel

```
* installing *source* package ‘modeltime.ensemble’ ...
** package ‘modeltime.ensemble’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘modeltime.resample’:
 object ‘type_sum’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘modeltime.ensemble’
* removing ‘/tmp/workdir/modeltime.ensemble/new/modeltime.ensemble.Rcheck/modeltime.ensemble’


```
### CRAN

```
* installing *source* package ‘modeltime.ensemble’ ...
** package ‘modeltime.ensemble’ successfully unpacked and MD5 sums checked
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
* DONE (modeltime.ensemble)


```
# modeltime.gluonts

<details>

* Version: 0.1.0
* GitHub: https://github.com/business-science/modeltime.gluonts
* Source code: https://github.com/cran/modeltime.gluonts
* Date/Publication: 2020-11-30 09:40:02 UTC
* Number of recursive dependencies: 206

Run `cloud_details(, "modeltime.gluonts")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > 
      > library(tidymodels)
      Error: package or namespace load failed for 'tidymodels':
       object 'type_sum' is not exported by 'namespace:dplyr'
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘getting-started.Rmd’ using rmarkdown
    ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──
    ✔ ggplot2 3.3.5     ✔ purrr   0.3.4
    ✔ tibble  3.1.6     ✔ dplyr   1.0.7
    ✔ tidyr   1.1.4     ✔ stringr 1.4.0
    ✔ readr   2.1.1     ✔ forcats 0.5.1
    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ...
    Error: processing vignette 'getting-started.Rmd' failed with diagnostics:
    package or namespace load failed for 'tidymodels':
     object 'type_sum' is not exported by 'namespace:dplyr'
    --- failed re-building ‘getting-started.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘getting-started.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# modeltime.h2o

<details>

* Version: 0.1.1
* GitHub: https://github.com/business-science/modeltime.h2o
* Source code: https://github.com/cran/modeltime.h2o
* Date/Publication: 2021-04-05 14:40:03 UTC
* Number of recursive dependencies: 206

Run `cloud_details(, "modeltime.h2o")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(tidymodels)
      Error: package or namespace load failed for 'tidymodels':
       object 'type_sum' is not exported by 'namespace:dplyr'
      Execution halted
    ```

# modeltime.resample

<details>

* Version: 0.2.0
* GitHub: https://github.com/business-science/modeltime.resample
* Source code: https://github.com/cran/modeltime.resample
* Date/Publication: 2021-03-14 20:40:07 UTC
* Number of recursive dependencies: 213

Run `cloud_details(, "modeltime.resample")` for more info

</details>

## Newly broken

*   checking whether package ‘modeltime.resample’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/modeltime.resample/new/modeltime.resample.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘crayon’ ‘dials’ ‘glue’ ‘parsnip’
      All declared Imports should be used.
    ```

## Installation

### Devel

```
* installing *source* package ‘modeltime.resample’ ...
** package ‘modeltime.resample’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘type_sum’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘modeltime.resample’
* removing ‘/tmp/workdir/modeltime.resample/new/modeltime.resample.Rcheck/modeltime.resample’


```
### CRAN

```
* installing *source* package ‘modeltime.resample’ ...
** package ‘modeltime.resample’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
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
* DONE (modeltime.resample)


```
# Momocs

<details>

* Version: 1.3.2
* GitHub: https://github.com/MomX/Momocs
* Source code: https://github.com/cran/Momocs
* Date/Publication: 2020-10-06 15:20:11 UTC
* Number of recursive dependencies: 120

Run `cloud_details(, "Momocs")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘Momocs-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: CLUST
    > ### Title: Hierarchical clustering
    > ### Aliases: CLUST CLUST.default CLUST.Coe
    > 
    > ### ** Examples
    > 
    > # On Coe
    ...
    > CLUST(bf, ~type, "v")
    > # with some cutting and different dist/hclust methods
    > CLUST(bf,
    +       dist_method="maximum", hclust_method="average",
    +       labels=~type, k=3, lwd=1, cex=1, palette=pal_manual(c("green", "yellow", "red")))
    > 
    > # On PCA
    > bf %>% PCA %>% CLUST
    Error: 'data_frame' is not an exported object from 'namespace:dplyr'
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       6. │ └─base::ifelse(any(class(x) == "Out"), TRUE, FALSE)
       7. ├─Momocs::Out(.)
       8. └─Momocs:::Out.list(.)
       9.   └─base::structure(...)
      ── Error (test-nse.R:3:1): (code run outside of `test_that()`) ─────────────────
      Error: 'data_frame' is not an exported object from 'namespace:dplyr'
      Backtrace:
          ▆
       1. ├─Momocs::Out(...) at test-nse.R:3:0
       2. └─Momocs:::Out.list(...)
       3.   └─base::structure(...)
      
      [ FAIL 6 | WARN 0 | SKIP 0 | PASS 123 ]
      Error: Test failures
      Execution halted
    ```

*   checking dependencies in R code ... WARNING
    ```
    Missing or unexported object: ‘dplyr::data_frame’
    ```

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in documentation object 'as_df.Rd':
      ‘[dplyr:data_frame]{dplyr::data_frame}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

# mpcmp

<details>

* Version: 0.3.6
* GitHub: https://github.com/thomas-fung/mpcmp
* Source code: https://github.com/cran/mpcmp
* Date/Publication: 2020-10-26 06:30:03 UTC
* Number of recursive dependencies: 152

Run `cloud_details(, "mpcmp")` for more info

</details>

## Newly broken

*   checking whether package ‘mpcmp’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/mpcmp/new/mpcmp.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘mpcmp’ ...
** package ‘mpcmp’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
g++ -std=gnu++14 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I/usr/local/include   -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++14 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I/usr/local/include   -fpic  -g -O2  -c utility.cpp -o utility.o
g++ -std=gnu++14 -shared -L/opt/R/4.1.1/lib/R/lib -L/usr/local/lib -o mpcmp.so RcppExports.o utility.o -L/opt/R/4.1.1/lib/R/lib -lR
installing to /tmp/workdir/mpcmp/new/mpcmp.Rcheck/00LOCK-mpcmp/00new/mpcmp/libs
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘mpcmp’
* removing ‘/tmp/workdir/mpcmp/new/mpcmp.Rcheck/mpcmp’


```
### CRAN

```
* installing *source* package ‘mpcmp’ ...
** package ‘mpcmp’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
g++ -std=gnu++14 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I/usr/local/include   -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++14 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I/usr/local/include   -fpic  -g -O2  -c utility.cpp -o utility.o
g++ -std=gnu++14 -shared -L/opt/R/4.1.1/lib/R/lib -L/usr/local/lib -o mpcmp.so RcppExports.o utility.o -L/opt/R/4.1.1/lib/R/lib -lR
installing to /tmp/workdir/mpcmp/old/mpcmp.Rcheck/00LOCK-mpcmp/00new/mpcmp/libs
** R
** data
...
** help
*** installing help indices
*** copying figures
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** checking absolute paths in shared objects and dynamic libraries
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (mpcmp)


```
# MSPRT

<details>

* Version: 3.0
* GitHub: NA
* Source code: https://github.com/cran/MSPRT
* Date/Publication: 2020-11-13 10:20:05 UTC
* Number of recursive dependencies: 92

Run `cloud_details(, "MSPRT")` for more info

</details>

## Newly broken

*   checking whether package ‘MSPRT’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/MSPRT/new/MSPRT.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘datasets’ ‘grDevices’ ‘graphics’ ‘iterators’ ‘methods’
      All declared Imports should be used.
    ```

## Installation

### Devel

```
* installing *source* package ‘MSPRT’ ...
** package ‘MSPRT’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘MSPRT’
* removing ‘/tmp/workdir/MSPRT/new/MSPRT.Rcheck/MSPRT’


```
### CRAN

```
* installing *source* package ‘MSPRT’ ...
** package ‘MSPRT’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (MSPRT)


```
# multilevelTools

<details>

* Version: 0.1.1
* GitHub: https://github.com/JWiley/multilevelTools
* Source code: https://github.com/cran/multilevelTools
* Date/Publication: 2020-03-04 09:50:02 UTC
* Number of recursive dependencies: 140

Run `cloud_details(, "multilevelTools")` for more info

</details>

## Newly broken

*   checking whether package ‘multilevelTools’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/multilevelTools/new/multilevelTools.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

## Installation

### Devel

```
* installing *source* package ‘multilevelTools’ ...
** package ‘multilevelTools’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘multilevelTools’
* removing ‘/tmp/workdir/multilevelTools/new/multilevelTools.Rcheck/multilevelTools’


```
### CRAN

```
* installing *source* package ‘multilevelTools’ ...
** package ‘multilevelTools’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (multilevelTools)


```
# NADA2

<details>

* Version: 1.0.1
* GitHub: https://github.com/SwampThingPaul/NADA2
* Source code: https://github.com/cran/NADA2
* Date/Publication: 2021-04-30 10:10:10 UTC
* Number of recursive dependencies: 131

Run `cloud_details(, "NADA2")` for more info

</details>

## Newly broken

*   checking whether package ‘NADA2’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/NADA2/new/NADA2.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘Kendall’ ‘perm’
      All declared Imports should be used.
    ```

## Installation

### Devel

```
* installing *source* package ‘NADA2’ ...
** package ‘NADA2’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘NADA2’
* removing ‘/tmp/workdir/NADA2/new/NADA2.Rcheck/NADA2’


```
### CRAN

```
* installing *source* package ‘NADA2’ ...
** package ‘NADA2’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (NADA2)


```
# namedropR

<details>

* Version: 2.2.1
* GitHub: https://github.com/nucleic-acid/namedropR
* Source code: https://github.com/cran/namedropR
* Date/Publication: 2022-01-27 00:40:02 UTC
* Number of recursive dependencies: 74

Run `cloud_details(, "namedropR")` for more info

</details>

## Newly broken

*   checking whether package ‘namedropR’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/namedropR/new/namedropR.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘namedropR’ ...
** package ‘namedropR’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘namedropR’
* removing ‘/tmp/workdir/namedropR/new/namedropR.Rcheck/namedropR’


```
### CRAN

```
* installing *source* package ‘namedropR’ ...
** package ‘namedropR’ successfully unpacked and MD5 sums checked
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
* DONE (namedropR)


```
# ncappc

<details>

* Version: 0.3.0
* GitHub: NA
* Source code: https://github.com/cran/ncappc
* Date/Publication: 2018-08-24 20:30:03 UTC
* Number of recursive dependencies: 86

Run `cloud_details(, "ncappc")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... WARNING
    ```
    Namespace in Imports field not imported from: ‘bookdown’
      All declared Imports should be used.
    Missing or unexported object: ‘dplyr::as_data_frame’
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘bookdown’
      All declared Imports should be used.
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# ncdump

<details>

* Version: 0.0.3
* GitHub: https://github.com/r-gris/ncdump
* Source code: https://github.com/cran/ncdump
* Date/Publication: 2017-05-02 12:35:30 UTC
* Number of recursive dependencies: 57

Run `cloud_details(, "ncdump")` for more info

</details>

## Newly broken

*   checking whether package ‘ncdump’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/ncdump/new/ncdump.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

## Installation

### Devel

```
* installing *source* package ‘ncdump’ ...
** package ‘ncdump’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘ncdump’
* removing ‘/tmp/workdir/ncdump/new/ncdump.Rcheck/ncdump’


```
### CRAN

```
* installing *source* package ‘ncdump’ ...
** package ‘ncdump’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (ncdump)


```
# neatStats

<details>

* Version: 1.8.1
* GitHub: https://github.com/gasparl/neatstats
* Source code: https://github.com/cran/neatStats
* Date/Publication: 2021-11-07 15:50:02 UTC
* Number of recursive dependencies: 123

Run `cloud_details(, "neatStats")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘neatStats-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: peek_neat
    > ### Title: Cursory Summaries and Plots per Group
    > ### Aliases: peek_neat
    > 
    > ### ** Examples
    > 
    > 
    ...
       11.0000     2.2860     1.9490     2.6220     0.5696     2.2000     1.8850 
    quantl_3rd  fence_low  fence_upp        min        max         na 
        2.6220    -0.3275     4.8350     1.5130     3.1900     0.0000 
    8:
             n       mean     ci_low     ci_upp         sd     median quantl_1st 
       14.0000     3.9990     3.6010     4.3970     0.7594     3.7550     3.5330 
    quantl_3rd  fence_low  fence_upp        min        max         na 
        4.0140     2.0890     5.4570     3.1700     5.4240     0.0000 
    Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
    Execution halted
    ```

# networkreporting

<details>

* Version: 0.1.1
* GitHub: NA
* Source code: https://github.com/cran/networkreporting
* Date/Publication: 2016-12-05 18:28:47
* Number of recursive dependencies: 63

Run `cloud_details(, "networkreporting")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘test_all.R’
    Running the tests in ‘tests/test_all.R’ failed.
    Last 13 lines of output:
       2.   ├─base::structure(lapply(pieces, .fun, ...), dim = dim(pieces))
       3.   └─base::lapply(pieces, .fun, ...)
       4.     └─networkreporting FUN(X[[i]], ...)
       5.       └─networkreporting::network.survival.estimator_(...) at test_estimators.R:163:16
       6.         └─networkreporting::kp.estimator_(...)
      ── Error (test_variance.R:40:1): (code run outside of `test_that()`) ───────────
      Error in `data_frame(kptot = rowSums(kpdat))`: could not find function "data_frame"
      Backtrace:
          ▆
       1. └─networkreporting::kp.individual.estimator_(...) at test_variance.R:40:0
       2.   └─networkreporting::kp.estimator_(...)
      
      [ FAIL 2 | WARN 1 | SKIP 0 | PASS 12 ]
      Error: Test failures
      Execution halted
    ```

*   checking R code for possible problems ... NOTE
    ```
    kp.estimator_: no visible global function definition for ‘data_frame’
    Undefined global functions or variables:
      data_frame
    ```

# nevada

<details>

* Version: 0.1.0
* GitHub: https://github.com/astamm/nevada
* Source code: https://github.com/cran/nevada
* Date/Publication: 2021-09-25 06:40:02 UTC
* Number of recursive dependencies: 84

Run `cloud_details(, "nevada")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘nevada-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: power2
    > ### Title: Power Simulations for Permutation Tests
    > ### Aliases: power2
    > 
    > ### ** Examples
    > 
    > power2(R = 10, B = 100, seed = 1234)
    Warning in igraph::sample_gnp(n = 25L, p = 1/3) :
      '.Random.seed' is not an integer vector but of type 'NULL', so ignored
    Error: object ‘type_sum’ is not exported by 'namespace:dplyr'
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘nevada.Rmd’ using rmarkdown
    Quitting from lines 262-272 (nevada.Rmd) 
    Error: processing vignette 'nevada.Rmd' failed with diagnostics:
    object 'type_sum' is not exported by 'namespace:dplyr'
    --- failed re-building ‘nevada.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘nevada.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        libs   4.7Mb
    ```

# noaastormevents

<details>

* Version: 0.2.0
* GitHub: https://github.com/geanders/noaastormevents
* Source code: https://github.com/cran/noaastormevents
* Date/Publication: 2021-01-21 10:20:06 UTC
* Number of recursive dependencies: 137

Run `cloud_details(, "noaastormevents")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘noaastormevents-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: match_forecast_county
    > ### Title: Match events by forecast zone to county
    > ### Aliases: match_forecast_county
    > 
    > ### ** Examples
    > 
    > counties_to_parse <- dplyr::data_frame(
    ...
    +                     "Pennsylvania",
    +                     "Minnesota",
    +                     "Oregon",
    +                     "Nevada",
    +                     "California",
    +                     "Florida",
    +                     "Colorado",
    +                     "Wyoming"))
    Error: 'data_frame' is not an exported object from 'namespace:dplyr'
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(noaastormevents)
      > 
      > test_check("noaastormevents")
      [ FAIL 1 | WARN 0 | SKIP 2 | PASS 1 ]
      
      ══ Skipped tests ═══════════════════════════════════════════════════════════════
      • On CRAN (2)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error (test-parsing-and-matching.R:35:13): matching cz_names to county names to get FIPS for events listed by forecast zone ──
      Error: 'data_frame' is not an exported object from 'namespace:dplyr'
      
      [ FAIL 1 | WARN 0 | SKIP 2 | PASS 1 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘RColorBrewer’ ‘choroplethr’ ‘choroplethrMaps’ ‘data.table’ ‘forcats’
      ‘hurricaneexposure’ ‘plyr’
      All declared Imports should be used.
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# OddsPlotty

<details>

* Version: 1.0.2
* GitHub: https://github.com/StatsGary/OddsPlotty
* Source code: https://github.com/cran/OddsPlotty
* Date/Publication: 2021-11-13 14:40:02 UTC
* Number of recursive dependencies: 132

Run `cloud_details(, "OddsPlotty")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘introduction.Rmd’ using rmarkdown
    Quitting from lines 168-177 (introduction.Rmd) 
    Error: processing vignette 'introduction.Rmd' failed with diagnostics:
    package or namespace load failed for 'tidymodels':
     object 'type_sum' is not exported by 'namespace:dplyr'
    --- failed re-building ‘introduction.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘introduction.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘caret’ ‘e1071’ ‘ggthemes’ ‘mlbench’ ‘rmarkdown’ ‘tidymodels’
      All declared Imports should be used.
    ```

# ohenery

<details>

* Version: 0.1.1
* GitHub: https://github.com/shabbychef/ohenery
* Source code: https://github.com/cran/ohenery
* Date/Publication: 2019-10-15 06:30:02 UTC
* Number of recursive dependencies: 66

Run `cloud_details(, "ohenery")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error (test-basic.r:447:2): harsm vs logistic ───────────────────────────────
      Error in `data_frame(eventnum = floor(seq(1, nop + 0.7, by = 0.5)))`: could not find function "data_frame"
      Backtrace:
          ▆
       1. ├─... %>% mutate(place = ohenery::rsm(eta, g = eventnum)) at test-basic.r:447:8
       2. ├─dplyr::mutate(., place = ohenery::rsm(eta, g = eventnum))
       3. ├─dplyr::mutate(., eta = 1.5 * x + 0.3 * intercept)
       4. ├─dplyr::mutate(., intercept = as.numeric(program_num == 1))
       5. ├─dplyr::mutate(., program_num = rep(c(1, 2), nop))
       6. └─dplyr::mutate(., x = rnorm(n()))
      
      [ FAIL 1 | WARN 0 | SKIP 1 | PASS 150 ]
      Error: Test failures
      Execution halted
    ```

# onemap

<details>

* Version: 2.8.1
* GitHub: https://github.com/augusto-garcia/onemap
* Source code: https://github.com/cran/onemap
* Date/Publication: 2021-12-10 13:00:02 UTC
* Number of recursive dependencies: 150

Run `cloud_details(, "onemap")` for more info

</details>

## Newly broken

*   checking whether package ‘onemap’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/onemap/new/onemap.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking installed package size ... NOTE
    ```
      installed size is  8.5Mb
      sub-directories of 1Mb or more:
        doc    1.4Mb
        libs   5.9Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘stringi’
      All declared Imports should be used.
    ```

## Installation

### Devel

```
* installing *source* package ‘onemap’ ...
** package ‘onemap’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
g++ -std=gnu++14 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I/usr/local/include   -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++14 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I/usr/local/include   -fpic  -g -O2  -c find_bins.cpp -o find_bins.o
g++ -std=gnu++14 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I/usr/local/include   -fpic  -g -O2  -c hmm_bc.cpp -o hmm_bc.o
g++ -std=gnu++14 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I/usr/local/include   -fpic  -g -O2  -c hmm_f2.cpp -o hmm_f2.o
g++ -std=gnu++14 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I/usr/local/include   -fpic  -g -O2  -c hmm_out.cpp -o hmm_out.o
gcc -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I/usr/local/include   -fpic  -g -O2  -c onemap_init.c -o onemap_init.o
...
g++ -std=gnu++14 -shared -L/opt/R/4.1.1/lib/R/lib -L/usr/local/lib -o onemap.so RcppExports.o find_bins.o hmm_bc.o hmm_f2.o hmm_out.o onemap_init.o out_est.o twopts_bc.o twopts_out.o utils.o -L/opt/R/4.1.1/lib/R/lib -lR
installing to /tmp/workdir/onemap/new/onemap.Rcheck/00LOCK-onemap/00new/onemap/libs
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘onemap’
* removing ‘/tmp/workdir/onemap/new/onemap.Rcheck/onemap’


```
### CRAN

```
* installing *source* package ‘onemap’ ...
** package ‘onemap’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
g++ -std=gnu++14 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I/usr/local/include   -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++14 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I/usr/local/include   -fpic  -g -O2  -c find_bins.cpp -o find_bins.o
g++ -std=gnu++14 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I/usr/local/include   -fpic  -g -O2  -c hmm_bc.cpp -o hmm_bc.o
g++ -std=gnu++14 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I/usr/local/include   -fpic  -g -O2  -c hmm_f2.cpp -o hmm_f2.o
g++ -std=gnu++14 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I/usr/local/include   -fpic  -g -O2  -c hmm_out.cpp -o hmm_out.o
gcc -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I/usr/local/include   -fpic  -g -O2  -c onemap_init.c -o onemap_init.o
...
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** checking absolute paths in shared objects and dynamic libraries
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (onemap)


```
# packDAMipd

<details>

* Version: 0.2.2
* GitHub: https://github.com/sheejamk/packDAMipd
* Source code: https://github.com/cran/packDAMipd
* Date/Publication: 2021-03-03 09:20:14 UTC
* Number of recursive dependencies: 204

Run `cloud_details(, "packDAMipd")` for more info

</details>

## Newly broken

*   checking whether package ‘packDAMipd’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/packDAMipd/new/packDAMipd.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘flexsurv’ ‘nlme’ ‘tibble’ ‘tidyverse’ ‘tm’
      All declared Imports should be used.
    ```

## Installation

### Devel

```
* installing *source* package ‘packDAMipd’ ...
** package ‘packDAMipd’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘packDAMipd’
* removing ‘/tmp/workdir/packDAMipd/new/packDAMipd.Rcheck/packDAMipd’


```
### CRAN

```
* installing *source* package ‘packDAMipd’ ...
** package ‘packDAMipd’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
[1] "The transition matrix as explained"
   transition number        probability name from from state to to state
...
3:                 3    prob_Dead_to_Healthy    2       Dead  1  Healthy
4:                 4       prob_Dead_to_Dead    2       Dead  2     Dead
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (packDAMipd)


```
# padr

<details>

* Version: 0.6.0
* GitHub: https://github.com/EdwinTh/padr
* Source code: https://github.com/cran/padr
* Date/Publication: 2021-10-01 10:00:02 UTC
* Number of recursive dependencies: 68

Run `cloud_details(, "padr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘padr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: fill_by_function
    > ### Title: Fill missing values by a function of the nonmissings
    > ### Aliases: fill_by_function
    > 
    > ### ** Examples
    > 
    > library(dplyr) # for the pipe operator
    ...
        intersect, setdiff, setequal, union
    
    > x <- seq(as.Date('2016-01-01'), by = 'day', length.out = 366)
    > x <- x[sample(1:366, 200)] %>% sort
    > x_df <- data_frame(x  = x,
    +                    y1 = runif(200, 10, 20) %>% round,
    +                    y2 = runif(200, 1, 50) %>% round)
    Error in data_frame(x = x, y1 = runif(200, 10, 20) %>% round, y2 = runif(200,  : 
      could not find function "data_frame"
    Execution halted
    ```

# pafr

<details>

* Version: 0.0.2
* GitHub: https://github.com/dwinter/pafr
* Source code: https://github.com/cran/pafr
* Date/Publication: 2020-12-08 10:20:12 UTC
* Number of recursive dependencies: 103

Run `cloud_details(, "pafr")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Introduction_to_pafr.Rmd’ using rmarkdown
    Quitting from lines 83-91 (Introduction_to_pafr.Rmd) 
    Error: processing vignette 'Introduction_to_pafr.Rmd' failed with diagnostics:
    package or namespace load failed for 'ggpubr':
     object 'as_data_frame' is not exported by 'namespace:dplyr'
    --- failed re-building ‘Introduction_to_pafr.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Introduction_to_pafr.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# PAMmisc

<details>

* Version: 1.8.0
* GitHub: NA
* Source code: https://github.com/cran/PAMmisc
* Date/Publication: 2022-01-10 22:52:42 UTC
* Number of recursive dependencies: 89

Run `cloud_details(, "PAMmisc")` for more info

</details>

## Newly broken

*   checking whether package ‘PAMmisc’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/PAMmisc/new/PAMmisc.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘PAMmisc’ ...
** package ‘PAMmisc’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘PAMmisc’
* removing ‘/tmp/workdir/PAMmisc/new/PAMmisc.Rcheck/PAMmisc’


```
### CRAN

```
* installing *source* package ‘PAMmisc’ ...
** package ‘PAMmisc’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (PAMmisc)


```
# PAMpal

<details>

* Version: 0.15.0
* GitHub: NA
* Source code: https://github.com/cran/PAMpal
* Date/Publication: 2022-01-10 22:42:42 UTC
* Number of recursive dependencies: 104

Run `cloud_details(, "PAMpal")` for more info

</details>

## Newly broken

*   checking whether package ‘PAMpal’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/PAMpal/new/PAMpal.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘rstudioapi’
      All declared Imports should be used.
    ```

## Installation

### Devel

```
* installing *source* package ‘PAMpal’ ...
** package ‘PAMpal’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘PAMpal’
* removing ‘/tmp/workdir/PAMpal/new/PAMpal.Rcheck/PAMpal’


```
### CRAN

```
* installing *source* package ‘PAMpal’ ...
** package ‘PAMpal’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Creating a new generic function for ‘id’ in package ‘PAMpal’
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (PAMpal)


```
# paramhetero

<details>

* Version: 0.2.0
* GitHub: NA
* Source code: https://github.com/cran/paramhetero
* Date/Publication: 2020-01-08 18:30:02 UTC
* Number of recursive dependencies: 91

Run `cloud_details(, "paramhetero")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘paramhetero-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: residual_plots
    > ### Title: Plot residuals.
    > ### Aliases: residual_plots residual_density residual_boxplot
    > 
    > ### ** Examples
    > 
    >  states = as.data.frame(state.x77)
    ...
    >  m4 = lm(`Life Exp` ~ Income + Illiteracy, data=states,
    +          subset=state.region=='West')
    > 
    >  mList = list(m1, m2, m3, m4)
    > 
    >  residual_plots(model_list = mList, thm=ggplot2::theme_minimal())
    Warning: `guides(<scale> = FALSE)` is deprecated. Please use `guides(<scale> = "none")` instead.
    Warning: `guides(<scale> = FALSE)` is deprecated. Please use `guides(<scale> = "none")` instead.
    Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘survey’
      All declared Imports should be used.
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# ParBayesianOptimization

<details>

* Version: 1.2.4
* GitHub: https://github.com/AnotherSamWilson/ParBayesianOptimization
* Source code: https://github.com/cran/ParBayesianOptimization
* Date/Publication: 2021-02-11 15:00:07 UTC
* Number of recursive dependencies: 102

Run `cloud_details(, "ParBayesianOptimization")` for more info

</details>

## Newly broken

*   checking whether package ‘ParBayesianOptimization’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/ParBayesianOptimization/new/ParBayesianOptimization.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

## Installation

### Devel

```
* installing *source* package ‘ParBayesianOptimization’ ...
** package ‘ParBayesianOptimization’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘ParBayesianOptimization’
* removing ‘/tmp/workdir/ParBayesianOptimization/new/ParBayesianOptimization.Rcheck/ParBayesianOptimization’


```
### CRAN

```
* installing *source* package ‘ParBayesianOptimization’ ...
** package ‘ParBayesianOptimization’ successfully unpacked and MD5 sums checked
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
* DONE (ParBayesianOptimization)


```
# pathwayTMB

<details>

* Version: 0.1.1
* GitHub: NA
* Source code: https://github.com/cran/pathwayTMB
* Date/Publication: 2021-10-10 11:10:02 UTC
* Number of recursive dependencies: 235

Run `cloud_details(, "pathwayTMB")` for more info

</details>

## Newly broken

*   checking whether package ‘pathwayTMB’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/pathwayTMB/new/pathwayTMB.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘pathwayTMB’ ...
** package ‘pathwayTMB’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘pathwayTMB’
* removing ‘/tmp/workdir/pathwayTMB/new/pathwayTMB.Rcheck/pathwayTMB’


```
### CRAN

```
* installing *source* package ‘pathwayTMB’ ...
** package ‘pathwayTMB’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (pathwayTMB)


```
# peacesciencer

<details>

* Version: 0.7.0
* GitHub: https://github.com/svmiller/peacesciencer
* Source code: https://github.com/cran/peacesciencer
* Date/Publication: 2021-11-11 17:00:02 UTC
* Number of recursive dependencies: 100

Run `cloud_details(, "peacesciencer")` for more info

</details>

## Newly broken

*   checking whether package ‘peacesciencer’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/peacesciencer/new/peacesciencer.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘lifecycle’
      All declared Imports should be used.
    ```

## Installation

### Devel

```
* installing *source* package ‘peacesciencer’ ...
** package ‘peacesciencer’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘peacesciencer’
* removing ‘/tmp/workdir/peacesciencer/new/peacesciencer.Rcheck/peacesciencer’


```
### CRAN

```
* installing *source* package ‘peacesciencer’ ...
** package ‘peacesciencer’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (peacesciencer)


```
# pencal

<details>

* Version: 1.0.1
* GitHub: NA
* Source code: https://github.com/cran/pencal
* Date/Publication: 2021-12-01 15:20:01 UTC
* Number of recursive dependencies: 155

Run `cloud_details(, "pencal")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘pencal-vignette.Rmd’ using rmarkdown
    Quitting from lines 149-161 (pencal-vignette.Rmd) 
    Error: processing vignette 'pencal-vignette.Rmd' failed with diagnostics:
    package 'ggpubr' could not be loaded
    --- failed re-building ‘pencal-vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘pencal-vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# petrinetR

<details>

* Version: 0.2.1
* GitHub: https://github.com/gertjanssenswillen/petrinetR
* Source code: https://github.com/cran/petrinetR
* Date/Publication: 2019-03-08 11:30:03 UTC
* Number of recursive dependencies: 63

Run `cloud_details(, "petrinetR")` for more info

</details>

## Newly broken

*   checking R code for possible problems ... NOTE
    ```
    add_places: no visible global function definition for ‘data_frame’
    add_transitions: no visible global function definition for ‘data_frame’
    read_PN: no visible global function definition for ‘data_frame’
    Undefined global functions or variables:
      data_frame
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# pguIMP

<details>

* Version: 0.0.0.3
* GitHub: https://github.com/SMLMS/pguIMP
* Source code: https://github.com/cran/pguIMP
* Date/Publication: 2021-09-30 11:50:02 UTC
* Number of recursive dependencies: 212

Run `cloud_details(, "pguIMP")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... WARNING
    ```
    Namespaces in Imports field not imported from:
      ‘rJava’ ‘shinyWidgets’ ‘shinydashboard’ ‘shinyjs’
      All declared Imports should be used.
    Missing or unexported object: ‘dplyr::type_sum’
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘rJava’ ‘shinyWidgets’ ‘shinydashboard’ ‘shinyjs’
      All declared Imports should be used.
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.5Mb
      sub-directories of 1Mb or more:
        application   5.0Mb
    ```

# photobiology

<details>

* Version: 0.10.8
* GitHub: https://github.com/aphalo/photobiology
* Source code: https://github.com/cran/photobiology
* Date/Publication: 2021-12-08 11:50:02 UTC
* Number of recursive dependencies: 60

Run `cloud_details(, "photobiology")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘photobiology-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: A.illuminant.spct
    > ### Title: CIE A illuminant data
    > ### Aliases: A.illuminant.spct
    > ### Keywords: datasets
    > 
    > ### ** Examples
    > 
    > A.illuminant.spct
    Object: source_spct [97 x 2]
    Wavelength range 300 to 780 nm, step 5 nm 
    Label: CIE A standard illuminant, normalized to one at 560 nm 
    Time unit 1s
    Spectral data normalized to 1 at 560 nm 
    
    Error: 'trunc_mat' is not an exported object from 'namespace:dplyr'
    Execution halted
    ```

*   checking dependencies in R code ... WARNING
    ```
    Missing or unexported object: ‘dplyr::trunc_mat’
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘userguide-1-radiation.Rmd’ using rmarkdown
    News at https://www.r4photobiology.info/
    
    Attaching package: 'lubridate'
    
    The following objects are masked from 'package:base':
    
        date, intersect, setdiff, union
    
    ...
    
        date, intersect, setdiff, union
    
    --- finished re-building ‘userguide-2-astronomy.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘userguide-1-radiation.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# photobiologyFilters

<details>

* Version: 0.5.2
* GitHub: NA
* Source code: https://github.com/cran/photobiologyFilters
* Date/Publication: 2020-10-05 07:10:06 UTC
* Number of recursive dependencies: 61

Run `cloud_details(, "photobiologyFilters")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘photobiologyFilters-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: acetate_filters
    > ### Title: Spectral data for filters made from different materials
    > ### Aliases: acetate_filters materials acrylic_filters
    > ###   polycarbonate_filters plexiglas_filters polystyrene_filters
    > ###   polyester_filters polyvynil_chloride_filters optical_glass_filters
    > ###   plastic_film_filters plastic_films plastic_sheet_filters
    > ###   plastic_sheets
    ...
    Object: filter_mspct [9 x 1]
    --- Member: Evonik_Cherry_3C01_GT ---
    Object: filter_spct [911 x 2]
    Wavelength range 190 to 1100 nm, step 1 nm 
    Label: Poly(methyl methacrylate) (PMMA) 'acrylic' sheet; Plexiglas 'Cherry 3C01 GT'; 0.002 m thick; new; from Evonik Industries, Germany 
    Transmittance of type 'total'
    Rfr (/1): 0.065, thickness (mm): 3, attenuation mode: absorption.
    
    Error: 'trunc_mat' is not an exported object from 'namespace:dplyr'
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘user-guide.Rmd’ using rmarkdown
    Quitting from lines 259-260 (user-guide.Rmd) 
    Error: processing vignette 'user-guide.Rmd' failed with diagnostics:
    'trunc_mat' is not an exported object from 'namespace:dplyr'
    --- failed re-building ‘user-guide.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘user-guide.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# photobiologyInOut

<details>

* Version: 0.4.23
* GitHub: https://github.com/aphalo/photobiologyinout
* Source code: https://github.com/cran/photobiologyInOut
* Date/Publication: 2021-10-11 04:10:01 UTC
* Number of recursive dependencies: 117

Run `cloud_details(, "photobiologyInOut")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘photobiologyInOut-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: colorSpec2mspct
    > ### Title: Convert 'colorSpec::colorSpec' objects
    > ### Aliases: colorSpec2mspct as.source_spct.colorSpec
    > ###   as.source_mspct.colorSpec as.response_spct.colorSpec
    > ###   as.response_mspct.colorSpec as.filter_spct.colorSpec
    > ###   as.filter_mspct.colorSpec as.reflector_spct.colorSpec
    > ###   as.reflector_mspct.colorSpec as.chroma_mspct.colorSpec colorSpec2spct
    ...
    The following object is masked from ‘package:photobiology’:
    
        normalize
    
    Object: source_spct [93 x 2]
    Wavelength range 320 to 780 nm, step 5 nm 
    Time unit 1s
    
    Error: 'trunc_mat' is not an exported object from 'namespace:dplyr'
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘user-guide.Rmd’ using rmarkdown
    News at https://www.r4photobiology.info/
    
    Attaching package: 'lubridate'
    
    The following objects are masked from 'package:base':
    
        date, intersect, setdiff, union
    
    ...
    Quitting from lines 525-530 (user-guide.Rmd) 
    Error: processing vignette 'user-guide.Rmd' failed with diagnostics:
    'trunc_mat' is not an exported object from 'namespace:dplyr'
    --- failed re-building ‘user-guide.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘user-guide.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘lazyeval’
      All declared Imports should be used.
    ```

# photobiologyLamps

<details>

* Version: 0.4.3
* GitHub: NA
* Source code: https://github.com/cran/photobiologyLamps
* Date/Publication: 2019-06-14 22:14:35 UTC
* Number of recursive dependencies: 61

Run `cloud_details(, "photobiologyLamps")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘photobiologyLamps-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: incandescent_lamps
    > ### Title: Spectral data for Lamps of different types
    > ### Aliases: incandescent_lamps types led_lamps mercury_lamps
    > ###   multimetal_lamps sodium_lamps xenon_lamps
    > ### Keywords: datasets
    > 
    > ### ** Examples
    ...
    --- Member: osram.super.vialox ---
    Object: source_spct [301 x 2]
    Wavelength range 300 to 900 nm, step 2 nm 
    Label: File: Osram.Super.Vialox.PRN 
    Measured on 0-10-17 12:14:11 UTC 
    Time unit 1s
    Spectral data normalized to 1 at 820 nm 
    
    Error: 'trunc_mat' is not an exported object from 'namespace:dplyr'
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘user-guide.Rmd’ using rmarkdown
    News at https://www.r4photobiology.info/
    ggspectra: default axis labels updated
    Quitting from lines 67-68 (user-guide.Rmd) 
    Error: processing vignette 'user-guide.Rmd' failed with diagnostics:
    'trunc_mat' is not an exported object from 'namespace:dplyr'
    --- failed re-building ‘user-guide.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘user-guide.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘photobiologyLEDs’
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked Latin-1 string
    ```

# photobiologyLEDs

<details>

* Version: 0.4.3-1
* GitHub: NA
* Source code: https://github.com/cran/photobiologyLEDs
* Date/Publication: 2018-01-14 15:47:06 UTC
* Number of recursive dependencies: 54

Run `cloud_details(, "photobiologyLEDs")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘photobiologyLEDs-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: leds.mspct
    > ### Title: Spectral irradiance for diverse LEDs
    > ### Aliases: leds.mspct
    > ### Keywords: datasets
    > 
    > ### ** Examples
    > 
    ...
    > leds.mspct$white
    Object: source_spct [1,425 x 2]
    Wavelength range 250.05 to 899.78 nm, step 0.43 to 0.48 nm 
    Label: White LED from hardware store
    supplier Clas Ohlsson, Finland 
    Time unit 1s
    Spectral data normalized to 1 at 453.47 nm 
    
    Error: 'trunc_mat' is not an exported object from 'namespace:dplyr'
    Execution halted
    ```

# photobiologySensors

<details>

* Version: 0.5.0
* GitHub: NA
* Source code: https://github.com/cran/photobiologySensors
* Date/Publication: 2020-10-05 07:10:03 UTC
* Number of recursive dependencies: 61

Run `cloud_details(, "photobiologySensors")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘photobiologySensors-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: uv_sensors
    > ### Title: Sensors responsive to different wavebands
    > ### Aliases: uv_sensors 'sensors by waveband' uvc_sensors uvb_sensors
    > ###   erythemal_sensors uva_sensors par_sensors vis_sensors
    > ###   photometric_sensors shortwave_sensors pyranometer_sensors red_sensors
    > ###   far_red_sensors blue_sensors multichannel_sensors
    > ### Keywords: datasets
    ...
    > 
    > # select PAR sensors
    > sensors.mspct[par_sensors]
    $Skye_SKP215
    Object: response_spct [736 x 2]
    Wavelength range 382.04143 to 750.07094 nm, step 0.5007204 nm 
    Time unit 1s
    
    Error: 'trunc_mat' is not an exported object from 'namespace:dplyr'
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘user-guide.Rmd’ using rmarkdown
    News at https://www.r4photobiology.info/
    ggspectra: default axis labels updated
    Quitting from lines 63-64 (user-guide.Rmd) 
    Error: processing vignette 'user-guide.Rmd' failed with diagnostics:
    'trunc_mat' is not an exported object from 'namespace:dplyr'
    --- failed re-building ‘user-guide.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘user-guide.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# photobiologySun

<details>

* Version: 0.4.1
* GitHub: NA
* Source code: https://github.com/cran/photobiologySun
* Date/Publication: 2019-03-27 22:20:03 UTC
* Number of recursive dependencies: 61

Run `cloud_details(, "photobiologySun")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘user-guide.Rmd’ using rmarkdown
    News at https://www.r4photobiology.info/
    
    Attaching package: 'lubridate'
    
    The following objects are masked from 'package:base':
    
        date, intersect, setdiff, union
    ...
    Quitting from lines 57-58 (user-guide.Rmd) 
    Error: processing vignette 'user-guide.Rmd' failed with diagnostics:
    'trunc_mat' is not an exported object from 'namespace:dplyr'
    --- failed re-building ‘user-guide.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘user-guide.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# photobiologyWavebands

<details>

* Version: 0.4.5
* GitHub: NA
* Source code: https://github.com/cran/photobiologyWavebands
* Date/Publication: 2022-01-07 19:52:40 UTC
* Number of recursive dependencies: 43

Run `cloud_details(, "photobiologyWavebands")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘r4p-introduction.Rmd’ using rmarkdown
    --- finished re-building ‘r4p-introduction.Rmd’
    
    --- re-building ‘user-guide.Rmd’ using rmarkdown
    Quitting from lines 222-223 (user-guide.Rmd) 
    Error: processing vignette 'user-guide.Rmd' failed with diagnostics:
    'trunc_mat' is not an exported object from 'namespace:dplyr'
    --- failed re-building ‘user-guide.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘user-guide.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# Plasmidprofiler

<details>

* Version: 0.1.6
* GitHub: NA
* Source code: https://github.com/cran/Plasmidprofiler
* Date/Publication: 2017-01-06 01:10:47
* Number of recursive dependencies: 89

Run `cloud_details(, "Plasmidprofiler")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘Plasmidprofiler-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: main
    > ### Title: Main: Run everything
    > ### Aliases: main
    > 
    > ### ** Examples
    > 
    > main(blastdata,
    + srst2data,
    + coverage.filter=NA,
    + sureness.filter=0.75,
    + length.filter=10000,
    + main.title="Example Results")
    Selecting by pident
    Error in data_frame() : could not find function "data_frame"
    Calls: main -> amr_positives
    Execution halted
    ```

*   checking R code for possible problems ... NOTE
    ```
    amr_positives: no visible global function definition for ‘data_frame’
    Undefined global functions or variables:
      data_frame
    ```

# Platypus

<details>

* Version: 3.3.2
* GitHub: NA
* Source code: https://github.com/cran/Platypus
* Date/Publication: 2022-01-27 23:20:16 UTC
* Number of recursive dependencies: 257

Run `cloud_details(, "Platypus")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘Platypus-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: GEX_phenotype
    > ### Title: Integrates VDJ and gene expression libraries by providing
    > ###   cluster membership seq_per_vdj object and the index of the cell in
    > ###   the Seurat RNA-seq object.
    > ### Aliases: GEX_phenotype
    > 
    > ### ** Examples
    > 
    > GEX_phenotype.test <- GEX_phenotype(seurat.object = Platypus::small_vgm[[2]])
    Error: object ‘data_frame’ is not exported by 'namespace:dplyr'
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘knitr’
      All declared Imports should be used.
    ```

# plotdap

<details>

* Version: 0.0.9
* GitHub: https://github.com/ropensci/plotdap
* Source code: https://github.com/cran/plotdap
* Date/Publication: 2020-10-28 21:20:02 UTC
* Number of recursive dependencies: 109

Run `cloud_details(, "plotdap")` for more info

</details>

## Newly broken

*   checking whether package ‘plotdap’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/plotdap/new/plotdap.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘plotdap’ ...
** package ‘plotdap’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘plotdap’
* removing ‘/tmp/workdir/plotdap/new/plotdap.Rcheck/plotdap’


```
### CRAN

```
* installing *source* package ‘plotdap’ ...
** package ‘plotdap’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (plotdap)


```
# postGGIR

<details>

* Version: 2.4.0.2
* GitHub: https://github.com/dora201888/postGGIR
* Source code: https://github.com/cran/postGGIR
* Date/Publication: 2022-01-06 14:30:02 UTC
* Number of recursive dependencies: 182

Run `cloud_details(, "postGGIR")` for more info

</details>

## Newly broken

*   checking whether package ‘postGGIR’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/postGGIR/new/postGGIR.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘ActCR’ ‘ActFrag’ ‘tidyr’
      All declared Imports should be used.
    ```

## Installation

### Devel

```
* installing *source* package ‘postGGIR’ ...
** package ‘postGGIR’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘postGGIR’
* removing ‘/tmp/workdir/postGGIR/new/postGGIR.Rcheck/postGGIR’


```
### CRAN

```
* installing *source* package ‘postGGIR’ ...
** package ‘postGGIR’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (postGGIR)


```
# powdR

<details>

* Version: 1.3.0
* GitHub: https://github.com/benmbutler/powdR
* Source code: https://github.com/cran/powdR
* Date/Publication: 2021-08-13 15:20:02 UTC
* Number of recursive dependencies: 139

Run `cloud_details(, "powdR")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Loading_and_manipulating.Rmd’ using rmarkdown
    powdR: Full Pattern Summation of X-Ray Powder Diffraction Data
    --- finished re-building ‘Loading_and_manipulating.Rmd’
    
    --- re-building ‘full_pattern_summation.Rmd’ using rmarkdown
    powdR: Full Pattern Summation of X-Ray Powder Diffraction Data
    Quitting from lines 385-388 (full_pattern_summation.Rmd) 
    Error: processing vignette 'full_pattern_summation.Rmd' failed with diagnostics:
    object 'as_data_frame' is not exported by 'namespace:dplyr'
    --- failed re-building ‘full_pattern_summation.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘full_pattern_summation.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.0Mb
      sub-directories of 1Mb or more:
        data   1.1Mb
        doc    3.0Mb
    ```

# PPforest

<details>

* Version: 0.1.2
* GitHub: https://github.com/natydasilva/PPforest
* Source code: https://github.com/cran/PPforest
* Date/Publication: 2021-10-14 14:40:05 UTC
* Number of recursive dependencies: 81

Run `cloud_details(, "PPforest")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘PPforest-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: PPforest
    > ### Title: Projection Pursuit Random Forest
    > ### Aliases: PPforest
    > 
    > ### ** Examples
    > 
    > #crab example with all the observations used as training
    > 
    > pprf.crab <- PPforest(data = crab, class = 'Type',
    +  std = FALSE, size.tr = 1, m = 100, size.p = .5, 
    +  PPmethod = 'LDA' , parallel = TRUE, cores = 2, rule=1)
    Error: 'data_frame' is not an exported object from 'namespace:dplyr'
    Execution halted
    ```

*   checking dependencies in R code ... WARNING
    ```
    Missing or unexported objects:
      ‘dplyr::as_data_frame’ ‘dplyr::data_frame’
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘PPforest-vignette.Rmd’ using rmarkdown
    Loading required package: PPforest
    Loading required package: dplyr
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    ...
    Quitting from lines 155-160 (PPforest-vignette.Rmd) 
    Error: processing vignette 'PPforest-vignette.Rmd' failed with diagnostics:
    'as_data_frame' is not an exported object from 'namespace:dplyr'
    --- failed re-building ‘PPforest-vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘PPforest-vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.4Mb
      sub-directories of 1Mb or more:
        libs   7.2Mb
    ```

# processR

<details>

* Version: 0.2.6
* GitHub: https://github.com/cardiomoon/processR
* Source code: https://github.com/cran/processR
* Date/Publication: 2021-01-07 06:50:02 UTC
* Number of recursive dependencies: 171

Run `cloud_details(, "processR")` for more info

</details>

## Newly broken

*   checking whether package ‘processR’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/processR/new/processR.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘processR’ ...
** package ‘processR’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘processR’
* removing ‘/tmp/workdir/processR/new/processR.Rcheck/processR’


```
### CRAN

```
* installing *source* package ‘processR’ ...
** package ‘processR’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (processR)


```
# promotionImpact

<details>

* Version: 0.1.5
* GitHub: https://github.com/ncsoft/promotionImpact
* Source code: https://github.com/cran/promotionImpact
* Date/Publication: 2021-04-13 15:00:05 UTC
* Number of recursive dependencies: 114

Run `cloud_details(, "promotionImpact")` for more info

</details>

## Newly broken

*   checking whether package ‘promotionImpact’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/promotionImpact/new/promotionImpact.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘promotionImpact’ ...
** package ‘promotionImpact’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘promotionImpact’
* removing ‘/tmp/workdir/promotionImpact/new/promotionImpact.Rcheck/promotionImpact’


```
### CRAN

```
* installing *source* package ‘promotionImpact’ ...
** package ‘promotionImpact’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (promotionImpact)


```
# PropensitySub

<details>

* Version: 0.2.0
* GitHub: NA
* Source code: https://github.com/cran/PropensitySub
* Date/Publication: 2021-07-29 08:50:11 UTC
* Number of recursive dependencies: 116

Run `cloud_details(, "PropensitySub")` for more info

</details>

## Newly broken

*   checking whether package ‘PropensitySub’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/PropensitySub/new/PropensitySub.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘PropensitySub’ ...
** package ‘PropensitySub’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘PropensitySub’
* removing ‘/tmp/workdir/PropensitySub/new/PropensitySub.Rcheck/PropensitySub’


```
### CRAN

```
* installing *source* package ‘PropensitySub’ ...
** package ‘PropensitySub’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (PropensitySub)


```
# pubh

<details>

* Version: 1.2.5
* GitHub: https://github.com/josie-athens/pubh
* Source code: https://github.com/cran/pubh
* Date/Publication: 2021-10-11 07:20:02 UTC
* Number of recursive dependencies: 206

Run `cloud_details(, "pubh")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘introduction.Rmd’ using rmarkdown
    ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──
    ✔ ggplot2 3.3.5     ✔ purrr   0.3.4
    ✔ tibble  3.1.6     ✔ dplyr   1.0.7
    ✔ tidyr   1.1.4     ✔ stringr 1.4.0
    ✔ readr   2.1.1     ✔ forcats 0.5.1
    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    ...
    Error: processing vignette 'regression.Rmd' failed with diagnostics:
    package or namespace load failed for 'rstatix':
     object 'as_data_frame' is not exported by 'namespace:dplyr'
    --- failed re-building ‘regression.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘introduction.Rmd’ ‘regression.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘Hmisc’ ‘sjPlot’
      All declared Imports should be used.
    ```

# qsub

<details>

* Version: 1.1.3
* GitHub: https://github.com/rcannood/qsub
* Source code: https://github.com/cran/qsub
* Date/Publication: 2021-09-23 10:10:02 UTC
* Number of recursive dependencies: 56

Run `cloud_details(, "qsub")` for more info

</details>

## Newly broken

*   checking R code for possible problems ... NOTE
    ```
    qacct_remote : <anonymous>: no visible global function definition for
      ‘data_frame’
    qstat_j_remote : <anonymous>: no visible global function definition for
      ‘data_frame’
    Undefined global functions or variables:
      data_frame
    ```

# quincunx

<details>

* Version: 0.1.4
* GitHub: https://github.com/maialab/quincunx
* Source code: https://github.com/cran/quincunx
* Date/Publication: 2021-10-30 13:50:02 UTC
* Number of recursive dependencies: 87

Run `cloud_details(, "quincunx")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘quincunx-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: get_cohorts
    > ### Title: Get PGS Catalog Cohorts
    > ### Aliases: get_cohorts
    > 
    > ### ** Examples
    > 
    > # Get information about specific cohorts by their symbols (acronyms)
    > get_cohorts(cohort_symbol = c('23andMe', 'IPOBCS'))
    Error in namespaceExport(ns, exports) : undefined exports: as_data_frame
    Calls: get_cohorts ... is_paginated -> count -> loadNamespace -> namespaceExport
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── Error (test-is_paginated.R:13:3): is paginated ──────────────────────────────
      Error in `namespaceExport(ns, exports)`: undefined exports: as_data_frame
      Backtrace:
          ▆
       1. ├─testthat::expect_true(is_paginated(txt)) at test-is_paginated.R:13:2
       2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
       3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
       4. ├─quincunx:::is_paginated(txt)
       5. │ └─quincunx:::count(json_string)
       6. └─base::loadNamespace(x)
       7.   └─base::namespaceExport(ns, exports)
      
      [ FAIL 5 | WARN 0 | SKIP 0 | PASS 12 ]
      Error: Test failures
      Execution halted
    ```

# r4lineups

<details>

* Version: 0.1.1
* GitHub: NA
* Source code: https://github.com/cran/r4lineups
* Date/Publication: 2018-07-18 13:20:02 UTC
* Number of recursive dependencies: 62

Run `cloud_details(, "r4lineups")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘r4lineups-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: make_roc
    > ### Title: Compute and plot ROC curve for lineup accuracy ~ confidence
    > ### Aliases: make_roc
    > 
    > ### ** Examples
    > 
    > #Data:
    > data(mickwick)
    > 
    > #Call:
    > make_roc(mickwick)
    Error: 'data_frame' is not an exported object from 'namespace:dplyr'
    Execution halted
    ```

*   checking dependencies in R code ... WARNING
    ```
    Missing or unexported object: ‘dplyr::data_frame’
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Vignette.Rmd’ using rmarkdown
    Quitting from lines 398-400 (Vignette.Rmd) 
    Error: processing vignette 'Vignette.Rmd' failed with diagnostics:
    'data_frame' is not an exported object from 'namespace:dplyr'
    --- failed re-building ‘Vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# radiant

<details>

* Version: 1.4.1
* GitHub: https://github.com/radiant-rstats/radiant
* Source code: https://github.com/cran/radiant
* Date/Publication: 2021-11-22 08:20:02 UTC
* Number of recursive dependencies: 166

Run `cloud_details(, "radiant")` for more info

</details>

## Newly broken

*   checking whether package ‘radiant’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/radiant/new/radiant.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        doc   5.1Mb
    ```

## Installation

### Devel

```
* installing *source* package ‘radiant’ ...
** package ‘radiant’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘radiant.model’:
 object ‘data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘radiant’
* removing ‘/tmp/workdir/radiant/new/radiant.Rcheck/radiant’


```
### CRAN

```
* installing *source* package ‘radiant’ ...
** package ‘radiant’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (radiant)


```
# radiant.model

<details>

* Version: 1.4.3
* GitHub: https://github.com/radiant-rstats/radiant.model
* Source code: https://github.com/cran/radiant.model
* Date/Publication: 2021-12-21 06:20:02 UTC
* Number of recursive dependencies: 166

Run `cloud_details(, "radiant.model")` for more info

</details>

## Newly broken

*   checking whether package ‘radiant.model’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/radiant.model/new/radiant.model.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘radiant.model’ ...
** package ‘radiant.model’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘radiant.model’
* removing ‘/tmp/workdir/radiant.model/new/radiant.model.Rcheck/radiant.model’


```
### CRAN

```
* installing *source* package ‘radiant.model’ ...
** package ‘radiant.model’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (radiant.model)


```
# radiant.multivariate

<details>

* Version: 1.4.1
* GitHub: https://github.com/radiant-rstats/radiant.multivariate
* Source code: https://github.com/cran/radiant.multivariate
* Date/Publication: 2021-11-22 05:50:02 UTC
* Number of recursive dependencies: 170

Run `cloud_details(, "radiant.multivariate")` for more info

</details>

## Newly broken

*   checking whether package ‘radiant.multivariate’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/radiant.multivariate/new/radiant.multivariate.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘radiant.multivariate’ ...
** package ‘radiant.multivariate’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘radiant.multivariate’
* removing ‘/tmp/workdir/radiant.multivariate/new/radiant.multivariate.Rcheck/radiant.multivariate’


```
### CRAN

```
* installing *source* package ‘radiant.multivariate’ ...
** package ‘radiant.multivariate’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (radiant.multivariate)


```
# rATTAINS

<details>

* Version: 0.1.3
* GitHub: https://github.com/mps9506/rATTAINS
* Source code: https://github.com/cran/rATTAINS
* Date/Publication: 2021-11-03 14:10:02 UTC
* Number of recursive dependencies: 77

Run `cloud_details(, "rATTAINS")` for more info

</details>

## Newly broken

*   checking whether package ‘rATTAINS’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/rATTAINS/new/rATTAINS.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘rATTAINS’ ...
** package ‘rATTAINS’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in namespaceExport(ns, exports) : undefined exports: as_data_frame
Calls: <Anonymous> ... namespaceImport -> loadNamespace -> namespaceExport
Execution halted
ERROR: lazy loading failed for package ‘rATTAINS’
* removing ‘/tmp/workdir/rATTAINS/new/rATTAINS.Rcheck/rATTAINS’


```
### CRAN

```
* installing *source* package ‘rATTAINS’ ...
** package ‘rATTAINS’ successfully unpacked and MD5 sums checked
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
* DONE (rATTAINS)


```
# rbokeh

<details>

* Version: 0.5.2
* GitHub: https://github.com/bokeh/rbokeh
* Source code: https://github.com/cran/rbokeh
* Date/Publication: 2021-08-04 00:40:02 UTC
* Number of recursive dependencies: 90

Run `cloud_details(, "rbokeh")` for more info

</details>

## Newly broken

*   checking whether package ‘rbokeh’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/rbokeh/new/rbokeh.Rcheck/00install.out’ for details.
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘shiny’
    ```

## Installation

### Devel

```
* installing *source* package ‘rbokeh’ ...
** package ‘rbokeh’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘rbokeh’
* removing ‘/tmp/workdir/rbokeh/new/rbokeh.Rcheck/rbokeh’


```
### CRAN

```
* installing *source* package ‘rbokeh’ ...
** package ‘rbokeh’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
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
* DONE (rbokeh)


```
# rchess

<details>

* Version: 0.1
* GitHub: https://github.com/jbkunst/rchess
* Source code: https://github.com/cran/rchess
* Date/Publication: 2015-11-05 17:18:05
* Number of recursive dependencies: 46

Run `cloud_details(, "rchess")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘rchess-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggchessboard
    > ### Title: Plot a fen representation chessboard via ggplot2
    > ### Aliases: ggchessboard
    > 
    > ### ** Examples
    > 
    > ggchessboard()
    Error: 'data_frame' is not an exported object from 'namespace:dplyr'
    Execution halted
    ```

*   checking dependencies in R code ... WARNING
    ```
    Missing or unexported object: ‘dplyr::data_frame’
    ```

*   checking R code for possible problems ... NOTE
    ```
    .add_castlings_rows_to_history: no visible global function definition
      for ‘data_frame’
    .chesspiecedata: no visible global function definition for ‘data_frame’
    .history_detail: no visible global function definition for ‘data_frame’
    .history_detail : <anonymous>: no visible global function definition
      for ‘data_frame’
    Undefined global functions or variables:
      data_frame
    ```

# RClickhouse

<details>

* Version: 0.6.0
* GitHub: https://github.com/IMSMWU/RClickhouse
* Source code: https://github.com/cran/RClickhouse
* Date/Publication: 2022-01-13 12:12:43 UTC
* Number of recursive dependencies: 42

Run `cloud_details(, "RClickhouse")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      ══ Skipped tests ═══════════════════════════════════════════════════════════════
      • On CRAN (20)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error (test-regr-25.R:9:3): correct conversion of Date values ───────────────
      Error in `data_frame(x = c(as.Date("1994-03-15"), as.Date("2000-01-01"), 
          as.Date("2012-04-22"), as.Date("1977-12-23")))`: could not find function "data_frame"
      Backtrace:
          ▆
       1. └─base::as.data.frame(...) at test-regr-25.R:9:2
      
      [ FAIL 1 | WARN 0 | SKIP 20 | PASS 8 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 18.5Mb
      sub-directories of 1Mb or more:
        libs  18.2Mb
    ```

# Rdca

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/Rdca
* Date/Publication: 2020-06-02 08:50:29 UTC
* Number of recursive dependencies: 97

Run `cloud_details(, "Rdca")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Rdca.Rmd’ using rmarkdown
    Quitting from lines 141-171 (Rdca.Rmd) 
    Error: processing vignette 'Rdca.Rmd' failed with diagnostics:
    package or namespace load failed for 'ggpubr':
     object 'as_data_frame' is not exported by 'namespace:dplyr'
    --- failed re-building ‘Rdca.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Rdca.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# recipes

<details>

* Version: 0.1.17
* GitHub: https://github.com/tidymodels/recipes
* Source code: https://github.com/cran/recipes
* Date/Publication: 2021-09-27 10:00:06 UTC
* Number of recursive dependencies: 125

Run `cloud_details(, "recipes")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘recipes-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: check_range
    > ### Title: Check Range Consistency
    > ### Aliases: check_range
    > 
    > ### ** Examples
    > 
    >   slack_df <- data_frame(x = 0:100)
    Error in data_frame(x = 0:100) : could not find function "data_frame"
    Execution halted
    ```

# reconstructKM

<details>

* Version: 0.3.0
* GitHub: NA
* Source code: https://github.com/cran/reconstructKM
* Date/Publication: 2020-11-25 13:40:02 UTC
* Number of recursive dependencies: 113

Run `cloud_details(, "reconstructKM")` for more info

</details>

## Newly broken

*   checking whether package ‘reconstructKM’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/reconstructKM/new/reconstructKM.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘reconstructKM’ ...
** package ‘reconstructKM’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘reconstructKM’
* removing ‘/tmp/workdir/reconstructKM/new/reconstructKM.Rcheck/reconstructKM’


```
### CRAN

```
* installing *source* package ‘reconstructKM’ ...
** package ‘reconstructKM’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (reconstructKM)


```
# rerddap

<details>

* Version: 0.8.0
* GitHub: https://github.com/ropensci/rerddap
* Source code: https://github.com/cran/rerddap
* Date/Publication: 2021-11-19 17:40:02 UTC
* Number of recursive dependencies: 58

Run `cloud_details(, "rerddap")` for more info

</details>

## Newly broken

*   checking whether package ‘rerddap’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/rerddap/new/rerddap.Rcheck/00install.out’ for details.
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘taxize’
    ```

## Installation

### Devel

```
* installing *source* package ‘rerddap’ ...
** package ‘rerddap’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘rerddap’
* removing ‘/tmp/workdir/rerddap/new/rerddap.Rcheck/rerddap’


```
### CRAN

```
* installing *source* package ‘rerddap’ ...
** package ‘rerddap’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (rerddap)


```
# rerddapXtracto

<details>

* Version: 1.1.2
* GitHub: https://github.com/rmendels/rerddapXtracto
* Source code: https://github.com/cran/rerddapXtracto
* Date/Publication: 2021-09-26 15:10:02 UTC
* Number of recursive dependencies: 109

Run `cloud_details(, "rerddapXtracto")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘rerddapXtracto-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plotBBox
    > ### Title: plot result of 'rxtracto_3D'
    > ### Aliases: plotBBox
    > 
    > ### ** Examples
    > 
    > # low resolution selected to keep time to render down
    > suppressWarnings(p <- plotBBox(MBsst, plotColor = 'thermal', maxpixels = 300))
    Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘UsingrerddapXtracto.Rmd’ using rmarkdown
    Quitting from lines 19-26 (UsingrerddapXtracto.Rmd) 
    Error: processing vignette 'UsingrerddapXtracto.Rmd' failed with diagnostics:
    package or namespace load failed for 'plotdap':
     object 'as_data_frame' is not exported by 'namespace:dplyr'
    --- failed re-building ‘UsingrerddapXtracto.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘UsingrerddapXtracto.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# resumer

<details>

* Version: 0.0.5
* GitHub: https://github.com/jaredlander/resumer
* Source code: https://github.com/cran/resumer
* Date/Publication: 2021-02-12 15:00:02 UTC
* Number of recursive dependencies: 66

Run `cloud_details(, "resumer")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘resumer-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: generateListing
    > ### Title: generateListing
    > ### Aliases: generateListing
    > 
    > ### ** Examples
    > 
    > library(dplyr)
    ...
    The following objects are masked from ‘package:base’:
    
        intersect, setdiff, setequal, union
    
    > 
    > jobs <- read.csv(system.file('examples/Jobs.csv', package='resumer'))
    > oneJob <- jobs %>% filter(Company=='Pied Piper', JobName=='Tech Startup')
    > generateListing(oneJob)
    Error: object ‘data_frame’ is not exported by 'namespace:dplyr'
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        1. ├─resumer::generateMultipleListings(jobs, jobList) at test-GenerateSection.R:10:0
        2. │ ├─... %>% unlist
        3. │ └─base::lapply(...)
        4. │   └─resumer FUN(X[[i]], ...)
        5. │     └─resumer::generateListing(...)
        6. │       └─... %>% unlist
        7. ├─base::unlist(.)
        8. ├─base::unlist(.)
        9. └─base::loadNamespace(x)
       10.   └─base::namespaceImportFrom(...)
       11.     └─base::importIntoEnv(impenv, impnames, ns, impvars)
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 23 ]
      Error: Test failures
      Execution halted
    ```

# rfVarImpOOB

<details>

* Version: 1.0.1
* GitHub: NA
* Source code: https://github.com/cran/rfVarImpOOB
* Date/Publication: 2020-10-18 09:00:02 UTC
* Number of recursive dependencies: 106

Run `cloud_details(, "rfVarImpOOB")` for more info

</details>

## Newly broken

*   checking whether package ‘rfVarImpOOB’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/rfVarImpOOB/new/rfVarImpOOB.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘prob’ ‘titanic’
      All declared Imports should be used.
    ```

## Installation

### Devel

```
* installing *source* package ‘rfVarImpOOB’ ...
** package ‘rfVarImpOOB’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘rfVarImpOOB’
* removing ‘/tmp/workdir/rfVarImpOOB/new/rfVarImpOOB.Rcheck/rfVarImpOOB’


```
### CRAN

```
* installing *source* package ‘rfVarImpOOB’ ...
** package ‘rfVarImpOOB’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (rfVarImpOOB)


```
# riskclustr

<details>

* Version: 0.3
* GitHub: https://github.com/zabore/riskclustr
* Source code: https://github.com/cran/riskclustr
* Date/Publication: 2020-05-26 15:20:03 UTC
* Number of recursive dependencies: 79

Run `cloud_details(, "riskclustr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── Error (test-optimal_kmeans_d.R:65:3): optimal_kmeans_d returns expected optimal_d value ──
      Error: 'data_frame' is not an exported object from 'namespace:dplyr'
      Backtrace:
          ▆
       1. ├─testthat::expect_equal(...) at test-optimal_kmeans_d.R:65:2
       2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
       3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
       4. └─riskclustr::optimal_kmeans_d(...)
       5.   └─base::lapply(...)
       6.     └─riskclustr FUN(X[[i]], ...)
       7.       └─dplyr::right_join(...)
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 26 ]
      Error: Test failures
      Execution halted
    ```

*   checking dependencies in R code ... WARNING
    ```
    Missing or unexported object: ‘dplyr::data_frame’
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘calculate_d.Rmd’ using rmarkdown
    --- finished re-building ‘calculate_d.Rmd’
    
    --- re-building ‘contribute.Rmd’ using rmarkdown
    --- finished re-building ‘contribute.Rmd’
    
    --- re-building ‘eh_test.Rmd’ using rmarkdown
    --- finished re-building ‘eh_test.Rmd’
    
    ...
    Quitting from lines 36-46 (optimal_d.Rmd) 
    Error: processing vignette 'optimal_d.Rmd' failed with diagnostics:
    'data_frame' is not an exported object from 'namespace:dplyr'
    --- failed re-building ‘optimal_d.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘optimal_d.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# rKOMICS

<details>

* Version: 1.1
* GitHub: NA
* Source code: https://github.com/cran/rKOMICS
* Date/Publication: 2021-07-21 11:40:02 UTC
* Number of recursive dependencies: 125

Run `cloud_details(, "rKOMICS")` for more info

</details>

## Newly broken

*   checking whether package ‘rKOMICS’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/rKOMICS/new/rKOMICS.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking installed package size ... NOTE
    ```
      installed size is 24.8Mb
      sub-directories of 1Mb or more:
        extdata  24.0Mb
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘example.Rnw’ using Sweave
    Loading required package: viridisLite
    Warning: Removed 95 rows containing non-finite values (stat_boxplot).
    Warning: Removed 89 rows containing non-finite values (stat_boxplot).
    Warning: Removed 149 rows containing non-finite values (stat_boxplot).
    Warning: Removed 286 rows containing non-finite values (stat_boxplot).
    Warning: Use of `depths$CN` is discouraged. Use `CN` instead.
    Warning: Use of `depths$group` is discouraged. Use `group` instead.
    Warning: Use of `depths$group` is discouraged. Use `group` instead.
    ...
    l.5 \usepackage
                   {xcolor}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    --- failed re-building ‘example.Rnw’
    
    SUMMARY: processing the following file failed:
      ‘example.Rnw’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## Installation

### Devel

```
* installing *source* package ‘rKOMICS’ ...
** package ‘rKOMICS’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘rKOMICS’
* removing ‘/tmp/workdir/rKOMICS/new/rKOMICS.Rcheck/rKOMICS’


```
### CRAN

```
* installing *source* package ‘rKOMICS’ ...
** package ‘rKOMICS’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (rKOMICS)


```
# rnmamod

<details>

* Version: 0.1.0
* GitHub: https://github.com/LoukiaSpin/rnmamod
* Source code: https://github.com/cran/rnmamod
* Date/Publication: 2021-11-29 20:00:08 UTC
* Number of recursive dependencies: 121

Run `cloud_details(, "rnmamod")` for more info

</details>

## Newly broken

*   checking whether package ‘rnmamod’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/rnmamod/new/rnmamod.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘rnmamod’ ...
** package ‘rnmamod’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘rnmamod’
* removing ‘/tmp/workdir/rnmamod/new/rnmamod.Rcheck/rnmamod’


```
### CRAN

```
* installing *source* package ‘rnmamod’ ...
** package ‘rnmamod’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
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
* DONE (rnmamod)


```
# RobMixReg

<details>

* Version: 1.1.0
* GitHub: https://github.com/changwn/RobMixReg
* Source code: https://github.com/cran/RobMixReg
* Date/Publication: 2020-08-05 12:00:07 UTC
* Number of recursive dependencies: 52

Run `cloud_details(, "RobMixReg")` for more info

</details>

## Newly broken

*   checking whether package ‘RobMixReg’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/RobMixReg/new/RobMixReg.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘glmnet’
      All declared Imports should be used.
    ```

## Installation

### Devel

```
* installing *source* package ‘RobMixReg’ ...
** package ‘RobMixReg’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error: object ‘data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘RobMixReg’
* removing ‘/tmp/workdir/RobMixReg/new/RobMixReg.Rcheck/RobMixReg’


```
### CRAN

```
* installing *source* package ‘RobMixReg’ ...
** package ‘RobMixReg’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (RobMixReg)


```
# rPACI

<details>

* Version: 0.2.2
* GitHub: https://github.com/dariorlual/rPACI
* Source code: https://github.com/cran/rPACI
* Date/Publication: 2021-11-04 00:10:07 UTC
* Number of recursive dependencies: 94

Run `cloud_details(, "rPACI")` for more info

</details>

## Newly broken

*   checking whether package ‘rPACI’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/rPACI/new/rPACI.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘rPACI’ ...
** package ‘rPACI’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** demo
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘rPACI’
* removing ‘/tmp/workdir/rPACI/new/rPACI.Rcheck/rPACI’


```
### CRAN

```
* installing *source* package ‘rPACI’ ...
** package ‘rPACI’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** demo
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (rPACI)


```
# rrr

<details>

* Version: 1.0.0
* GitHub: https://github.com/chrisaddy/rrr
* Source code: https://github.com/cran/rrr
* Date/Publication: 2016-12-09 15:15:55
* Number of recursive dependencies: 91

Run `cloud_details(, "rrr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘rrr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: pairwise_plot
    > ### Title: Pairwise Plots
    > ### Aliases: pairwise_plot
    > 
    > ### ** Examples
    > 
    > data(pendigits)
    > digits_features <- pendigits[,1:34]
    > digits_class <- pendigits[,35]
    > pairwise_plot(digits_features, digits_class, type = "pca", pair_x = 1, pair_y = 3)
    Error in as_data_frame(A %*% t(A)) : 
      could not find function "as_data_frame"
    Calls: pairwise_plot ... pca_pairwise_plot -> pca_pairwise -> pca_scores -> pca
    Execution halted
    ```

*   checking dependencies in R code ... WARNING
    ```
    Namespace in Imports field not imported from: ‘Rcpp’
      All declared Imports should be used.
    Missing or unexported objects:
      ‘dplyr::as_data_frame’ ‘dplyr::data_frame’
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘rrr.Rmd’ using rmarkdown
    Quitting from lines 66-72 (rrr.Rmd) 
    Error: processing vignette 'rrr.Rmd' failed with diagnostics:
    could not find function "as_data_frame"
    --- failed re-building ‘rrr.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘rrr.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

*   checking R code for possible problems ... NOTE
    ```
    cva_error: no visible global function definition for ‘data_frame’
    cva_error: no visible global function definition for ‘as_data_frame’
    cva_scores: no visible global function definition for ‘as_data_frame’
    delta_coeff: no visible global function definition for ‘data_frame’
    delta_error: no visible global function definition for ‘data_frame’
    lda: no visible global function definition for ‘as_data_frame’
    lda_3D_plot: no visible global function definition for ‘as_data_frame’
    lda_organize: no visible global function definition for ‘as_data_frame’
    lda_scores: no visible global function definition for ‘as_data_frame’
    pca: no visible global function definition for ‘as_data_frame’
    pca_allpairs_plot: no visible global function definition for
      ‘as_data_frame’
    pca_scores: no visible global function definition for ‘as_data_frame’
    rrr: no visible global function definition for ‘as_data_frame’
    rrr_error: no visible global function definition for ‘as_data_frame’
    rrr_predict: no visible global function definition for ‘as_data_frame’
    Undefined global functions or variables:
      as_data_frame data_frame
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘Rcpp’
      All declared Imports should be used.
    ```

# rrtable

<details>

* Version: 0.2.1
* GitHub: NA
* Source code: https://github.com/cran/rrtable
* Date/Publication: 2020-04-02 12:40:02 UTC
* Number of recursive dependencies: 148

Run `cloud_details(, "rrtable")` for more info

</details>

## Newly broken

*   checking whether package ‘rrtable’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/rrtable/new/rrtable.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘rrtable’ ...
** package ‘rrtable’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘rrtable’
* removing ‘/tmp/workdir/rrtable/new/rrtable.Rcheck/rrtable’


```
### CRAN

```
* installing *source* package ‘rrtable’ ...
** package ‘rrtable’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
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
* DONE (rrtable)


```
# rSAFE

<details>

* Version: 0.1.2
* GitHub: https://github.com/ModelOriented/rSAFE
* Source code: https://github.com/cran/rSAFE
* Date/Publication: 2021-03-31 08:20:02 UTC
* Number of recursive dependencies: 108

Run `cloud_details(, "rSAFE")` for more info

</details>

## Newly broken

*   checking whether package ‘rSAFE’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/rSAFE/new/rSAFE.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘rSAFE’ ...
** package ‘rSAFE’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘rSAFE’
* removing ‘/tmp/workdir/rSAFE/new/rSAFE.Rcheck/rSAFE’


```
### CRAN

```
* installing *source* package ‘rSAFE’ ...
** package ‘rSAFE’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
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
* DONE (rSAFE)


```
# rsparkling

<details>

* Version: 0.2.19
* GitHub: https://github.com/h2oai/sparkling-water
* Source code: https://github.com/cran/rsparkling
* Date/Publication: 2020-01-28 23:10:02 UTC
* Number of recursive dependencies: 66

Run `cloud_details(, "rsparkling")` for more info

</details>

## Newly broken

*   checking whether package ‘rsparkling’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/rsparkling/new/rsparkling.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘h2o’
      All declared Imports should be used.
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

## Installation

### Devel

```
* installing *source* package ‘rsparkling’ ...
** package ‘rsparkling’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error: object ‘type_sum’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘rsparkling’
* removing ‘/tmp/workdir/rsparkling/new/rsparkling.Rcheck/rsparkling’


```
### CRAN

```
* installing *source* package ‘rsparkling’ ...
** package ‘rsparkling’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
** help
No man pages found in package  ‘rsparkling’ 
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (rsparkling)


```
# rstatix

<details>

* Version: 0.7.0
* GitHub: https://github.com/kassambara/rstatix
* Source code: https://github.com/cran/rstatix
* Date/Publication: 2021-02-13 11:30:02 UTC
* Number of recursive dependencies: 110

Run `cloud_details(, "rstatix")` for more info

</details>

## Newly broken

*   checking whether package ‘rstatix’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/rstatix/new/rstatix.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

## Installation

### Devel

```
* installing *source* package ‘rstatix’ ...
** package ‘rstatix’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘rstatix’
* removing ‘/tmp/workdir/rstatix/new/rstatix.Rcheck/rstatix’


```
### CRAN

```
* installing *source* package ‘rstatix’ ...
** package ‘rstatix’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (rstatix)


```
# rules

<details>

* Version: 0.1.2
* GitHub: https://github.com/tidymodels/rules
* Source code: https://github.com/cran/rules
* Date/Publication: 2021-08-07 23:10:06 UTC
* Number of recursive dependencies: 121

Run `cloud_details(, "rules")` for more info

</details>

## Newly broken

*   checking whether package ‘rules’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/rules/new/rules.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘rules’ ...
** package ‘rules’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘type_sum’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘rules’
* removing ‘/tmp/workdir/rules/new/rules.Rcheck/rules’


```
### CRAN

```
* installing *source* package ‘rules’ ...
** package ‘rules’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (rules)


```
# RVA

<details>

* Version: 0.0.5
* GitHub: https://github.com/THERMOSTATS/RVA
* Source code: https://github.com/cran/RVA
* Date/Publication: 2021-11-01 21:40:02 UTC
* Number of recursive dependencies: 204

Run `cloud_details(, "RVA")` for more info

</details>

## Newly broken

*   checking whether package ‘RVA’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/RVA/new/RVA.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘XML’
      All declared Imports should be used.
    ```

## Installation

### Devel

```
* installing *source* package ‘RVA’ ...
** package ‘RVA’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘RVA’
* removing ‘/tmp/workdir/RVA/new/RVA.Rcheck/RVA’


```
### CRAN

```
* installing *source* package ‘RVA’ ...
** package ‘RVA’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (RVA)


```
# saeSim

<details>

* Version: 0.10.0
* GitHub: https://github.com/wahani/saeSim
* Source code: https://github.com/cran/saeSim
* Date/Publication: 2019-03-28 12:50:03 UTC
* Number of recursive dependencies: 94

Run `cloud_details(, "saeSim")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘saeSim-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: agg_all
    > ### Title: Aggregation function
    > ### Aliases: agg_all
    > 
    > ### ** Examples
    > 
    > sim_base() %>% sim_gen_x() %>% sim_gen_e() %>% sim_agg(agg_all())
    ...
    Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.
    Warning: `group_by_()` was deprecated in dplyr 0.7.0.
    Please use `group_by()` instead.
    See vignette('programming') for more help
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.
    Error in trunc_mat(dat, n = 6, width = NULL) : 
      could not find function "trunc_mat"
    Calls: <Anonymous> -> <Anonymous> -> print
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘test-all.R’
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      
      [ FAIL 1 | WARN 6 | SKIP 0 | PASS 133 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error (test-sim_setup.R:14:3): methods equal ────────────────────────────────
      Error in `trunc_mat(dat, n = 6, width = NULL)`: could not find function "trunc_mat"
      Backtrace:
          ▆
       1. ├─methods::show(setup) at test-sim_setup.R:14:2
       2. └─saeSim::show(setup)
       3.   └─base::print(trunc_mat(dat, n = 6, width = NULL))
      
      [ FAIL 1 | WARN 6 | SKIP 0 | PASS 133 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Introduction.Rmd’ using rmarkdown
    Quitting from lines 31-39 (Introduction.Rmd) 
    Error: processing vignette 'Introduction.Rmd' failed with diagnostics:
    could not find function "trunc_mat"
    --- failed re-building ‘Introduction.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Introduction.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

*   checking R code for possible problems ... NOTE
    ```
    show,sim_setup: no visible global function definition for ‘trunc_mat’
    Undefined global functions or variables:
      trunc_mat
    ```

# safetyCharts

<details>

* Version: 0.2.0
* GitHub: https://github.com/SafetyGraphics/safetyCharts
* Source code: https://github.com/cran/safetyCharts
* Date/Publication: 2021-09-14 07:10:06 UTC
* Number of recursive dependencies: 143

Run `cloud_details(, "safetyCharts")` for more info

</details>

## Newly broken

*   checking R code for possible problems ... NOTE
    ```
    init_paneledOutlierExplorer: no visible global function definition for
      ‘data_frame’
    init_safetyOutlierExplorer: no visible global function definition for
      ‘data_frame’
    Undefined global functions or variables:
      data_frame
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘kableExtra’ ‘knitr’
      All declared Imports should be used.
    ```

# sandwichr

<details>

* Version: 1.0.0
* GitHub: https://github.com/linyuehzzz/sandwich_spatial_interpolator
* Source code: https://github.com/cran/sandwichr
* Date/Publication: 2022-01-28 21:00:05 UTC
* Number of recursive dependencies: 132

Run `cloud_details(, "sandwichr")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘sandwichr.R.tutorial.Rmd’ using rmarkdown
    Quitting from lines 37-42 (sandwichr.R.tutorial.Rmd) 
    Error: processing vignette 'sandwichr.R.tutorial.Rmd' failed with diagnostics:
    package or namespace load failed for 'ggpubr':
     object 'as_data_frame' is not exported by 'namespace:dplyr'
    --- failed re-building ‘sandwichr.R.tutorial.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘sandwichr.R.tutorial.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# scITD

<details>

* Version: 1.0.1
* GitHub: NA
* Source code: https://github.com/cran/scITD
* Date/Publication: 2022-01-29 00:00:01 UTC
* Number of recursive dependencies: 231

Run `cloud_details(, "scITD")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘scITD-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: determine_ranks_tucker
    > ### Title: Run rank determination by svd on the tensor unfolded along each
    > ###   mode
    > ### Aliases: determine_ranks_tucker
    > 
    > ### ** Examples
    > 
    > test_container <- determine_ranks_tucker(test_container, max_ranks_test=c(3,5),
    + shuffle_level='tensor', num_iter=4, norm_method='trim', scale_factor=10000,
    + scale_var=TRUE, var_scale_power=.5)
    Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
    Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘conos’
    ```

# SCOUTer

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/SCOUTer
* Date/Publication: 2020-06-30 09:30:03 UTC
* Number of recursive dependencies: 93

Run `cloud_details(, "SCOUTer")` for more info

</details>

## Newly broken

*   checking whether package ‘SCOUTer’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/SCOUTer/new/SCOUTer.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘SCOUTer’ ...
** package ‘SCOUTer’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘ggpubr’:
 object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘SCOUTer’
* removing ‘/tmp/workdir/SCOUTer/new/SCOUTer.Rcheck/SCOUTer’


```
### CRAN

```
* installing *source* package ‘SCOUTer’ ...
** package ‘SCOUTer’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (SCOUTer)


```
# SEIRfansy

<details>

* Version: 1.1.1
* GitHub: https://github.com/umich-biostatistics/SEIRfansy
* Source code: https://github.com/cran/SEIRfansy
* Date/Publication: 2021-09-27 16:30:10 UTC
* Number of recursive dependencies: 103

Run `cloud_details(, "SEIRfansy")` for more info

</details>

## Newly broken

*   checking whether package ‘SEIRfansy’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/SEIRfansy/new/SEIRfansy.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘knitr’
      All declared Imports should be used.
    ```

## Installation

### Devel

```
* installing *source* package ‘SEIRfansy’ ...
** package ‘SEIRfansy’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘SEIRfansy’
* removing ‘/tmp/workdir/SEIRfansy/new/SEIRfansy.Rcheck/SEIRfansy’


```
### CRAN

```
* installing *source* package ‘SEIRfansy’ ...
** package ‘SEIRfansy’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (SEIRfansy)


```
# sergeant

<details>

* Version: 0.9.1
* GitHub: NA
* Source code: https://github.com/cran/sergeant
* Date/Publication: 2021-11-29 18:40:02 UTC
* Number of recursive dependencies: 82

Run `cloud_details(, "sergeant")` for more info

</details>

## Newly broken

*   checking whether package ‘sergeant’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/sergeant/new/sergeant.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘sergeant’ ...
** package ‘sergeant’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘sergeant’
* removing ‘/tmp/workdir/sergeant/new/sergeant.Rcheck/sergeant’


```
### CRAN

```
* installing *source* package ‘sergeant’ ...
** package ‘sergeant’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (sergeant)


```
# SHAPforxgboost

<details>

* Version: 0.1.1
* GitHub: https://github.com/liuyanguu/SHAPforxgboost
* Source code: https://github.com/cran/SHAPforxgboost
* Date/Publication: 2021-03-28 03:10:02 UTC
* Number of recursive dependencies: 121

Run `cloud_details(, "SHAPforxgboost")` for more info

</details>

## Newly broken

*   checking whether package ‘SHAPforxgboost’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/SHAPforxgboost/new/SHAPforxgboost.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘SHAPforxgboost’ ...
** package ‘SHAPforxgboost’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘SHAPforxgboost’
* removing ‘/tmp/workdir/SHAPforxgboost/new/SHAPforxgboost.Rcheck/SHAPforxgboost’


```
### CRAN

```
* installing *source* package ‘SHAPforxgboost’ ...
** package ‘SHAPforxgboost’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (SHAPforxgboost)


```
# ShellChron

<details>

* Version: 0.4.0
* GitHub: https://github.com/nielsjdewinter/ShellChron
* Source code: https://github.com/cran/ShellChron
* Date/Publication: 2021-07-05 12:40:02 UTC
* Number of recursive dependencies: 100

Run `cloud_details(, "ShellChron")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ShellChron-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: export_results
    > ### Title: Function to merge and export the results of the ShellChron model
    > ### Aliases: export_results
    > 
    > ### ** Examples
    > 
    > # Create dummy input data column by column
    ...
    +     dat,
    +     testarray,
    +     parmat,
    +     MC = 1000,
    +     dynwindow,
    +     plot = FALSE,
    +     plot_export = FALSE,
    +     export_raw = FALSE)
    Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
    Execution halted
    ```

# shinyloadtest

<details>

* Version: 1.1.0
* GitHub: https://github.com/rstudio/shinyloadtest
* Source code: https://github.com/cran/shinyloadtest
* Date/Publication: 2021-02-11 14:50:02 UTC
* Number of recursive dependencies: 85

Run `cloud_details(, "shinyloadtest")` for more info

</details>

## Newly broken

*   checking R code for possible problems ... NOTE
    ```
    gantt_latency : info: no visible global function definition for
      ‘data_frame’
    Undefined global functions or variables:
      data_frame
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘R6’ ‘scales’ ‘websocket’
      All declared Imports should be used.
    ```

# shinyML

<details>

* Version: 1.0.1
* GitHub: https://github.com/JeanBertinR/shinyML
* Source code: https://github.com/cran/shinyML
* Date/Publication: 2021-02-24 17:00:02 UTC
* Number of recursive dependencies: 116

Run `cloud_details(, "shinyML")` for more info

</details>

## Newly broken

*   checking whether package ‘shinyML’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/shinyML/new/shinyML.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

## Installation

### Devel

```
* installing *source* package ‘shinyML’ ...
** package ‘shinyML’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error: object ‘type_sum’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘shinyML’
* removing ‘/tmp/workdir/shinyML/new/shinyML.Rcheck/shinyML’


```
### CRAN

```
* installing *source* package ‘shinyML’ ...
** package ‘shinyML’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (shinyML)


```
# shinymodels

<details>

* Version: 0.1.0
* GitHub: https://github.com/tidymodels/shinymodels
* Source code: https://github.com/cran/shinymodels
* Date/Publication: 2021-11-17 21:00:02 UTC
* Number of recursive dependencies: 146

Run `cloud_details(, "shinymodels")` for more info

</details>

## Newly broken

*   checking whether package ‘shinymodels’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/shinymodels/new/shinymodels.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘finetune’
    ```

## Installation

### Devel

```
* installing *source* package ‘shinymodels’ ...
** package ‘shinymodels’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘type_sum’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘shinymodels’
* removing ‘/tmp/workdir/shinymodels/new/shinymodels.Rcheck/shinymodels’


```
### CRAN

```
* installing *source* package ‘shinymodels’ ...
** package ‘shinymodels’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (shinymodels)


```
# sigminer

<details>

* Version: 2.1.2
* GitHub: https://github.com/ShixiangWang/sigminer
* Source code: https://github.com/cran/sigminer
* Date/Publication: 2021-12-15 07:50:02 UTC
* Number of recursive dependencies: 201

Run `cloud_details(, "sigminer")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘sigminer-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: get_adj_p
    > ### Title: Get Adjust P Values from Group Comparison
    > ### Aliases: get_adj_p
    > 
    > ### ** Examples
    > 
    > library(ggpubr)
    Loading required package: ggplot2
    Error: package or namespace load failed for ‘ggpubr’:
     object ‘as_data_frame’ is not exported by 'namespace:dplyr'
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.8Mb
      sub-directories of 1Mb or more:
        extdata   2.7Mb
        libs      1.4Mb
    ```

# simPH

<details>

* Version: 1.3.13
* GitHub: https://github.com/christophergandrud/simPH
* Source code: https://github.com/cran/simPH
* Date/Publication: 2021-01-10 14:50:05 UTC
* Number of recursive dependencies: 69

Run `cloud_details(, "simPH")` for more info

</details>

## Newly broken

*   checking whether package ‘simPH’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/simPH/new/simPH.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘simPH-overview.Rnw’ using knitr
    All Xl set to 0.
    `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'
    Warning: It is deprecated to specify `guide = FALSE` to remove a guide. Please use `guide = "none"` instead.
    All Xl set to 0.
    `geom_smooth()` using formula 'y ~ x'
    Warning: It is deprecated to specify `guide = FALSE` to remove a guide. Please use `guide = "none"` instead.
    
    Reminder (1): Currently SurvExpand does not support repeated events data.
    ...
    l.55 \RequirePackage
                        [T1]{fontenc}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    --- failed re-building ‘simPH-overview.Rnw’
    
    SUMMARY: processing the following file failed:
      ‘simPH-overview.Rnw’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## Installation

### Devel

```
* installing *source* package ‘simPH’ ...
** package ‘simPH’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** demo
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘simPH’
* removing ‘/tmp/workdir/simPH/new/simPH.Rcheck/simPH’


```
### CRAN

```
* installing *source* package ‘simPH’ ...
** package ‘simPH’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** demo
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
* DONE (simPH)


```
# SimplyAgree

<details>

* Version: 0.0.2
* GitHub: NA
* Source code: https://github.com/cran/SimplyAgree
* Date/Publication: 2021-05-18 14:10:16 UTC
* Number of recursive dependencies: 152

Run `cloud_details(, "SimplyAgree")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘intro_vignette.Rmd’ using rmarkdown
    --- finished re-building ‘intro_vignette.Rmd’
    
    --- re-building ‘reanalysis.Rmd’ using rmarkdown
    ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──
    ✔ ggplot2 3.3.5     ✔ purrr   0.3.4
    ✔ tibble  3.1.6     ✔ dplyr   1.0.7
    ✔ tidyr   1.1.4     ✔ stringr 1.4.0
    ...
    Error: processing vignette 'reanalysis.Rmd' failed with diagnostics:
    package or namespace load failed for 'ggpubr':
     object 'as_data_frame' is not exported by 'namespace:dplyr'
    --- failed re-building ‘reanalysis.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘reanalysis.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# SiteAdapt

<details>

* Version: 1.1.0
* GitHub: NA
* Source code: https://github.com/cran/SiteAdapt
* Date/Publication: 2020-12-03 15:00:02 UTC
* Number of recursive dependencies: 117

Run `cloud_details(, "SiteAdapt")` for more info

</details>

## Newly broken

*   checking whether package ‘SiteAdapt’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/SiteAdapt/new/SiteAdapt.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘SiteAdapt’ ...
** package ‘SiteAdapt’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘SiteAdapt’
* removing ‘/tmp/workdir/SiteAdapt/new/SiteAdapt.Rcheck/SiteAdapt’


```
### CRAN

```
* installing *source* package ‘SiteAdapt’ ...
** package ‘SiteAdapt’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (SiteAdapt)


```
# sknifedatar

<details>

* Version: 0.1.2
* GitHub: https://github.com/rafzamb/sknifedatar
* Source code: https://github.com/cran/sknifedatar
* Date/Publication: 2021-06-01 08:00:02 UTC
* Number of recursive dependencies: 173

Run `cloud_details(, "sknifedatar")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘sknifedatar-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: modeltime_multifit
    > ### Title: Fit Multiple Models to Multiple Time Series
    > ### Aliases: modeltime_multifit
    > 
    > ### ** Examples
    > 
    > library(modeltime)
    ...
     15. │ ├─base::namespaceImportFrom(...)
     16. │ │ └─base::asNamespace(ns)
     17. │ └─base::loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]])
     18. │   └─base::namespaceImportFrom(...)
     19. │     └─base::importIntoEnv(impenv, impnames, ns, impvars)
     20. │       └─base::stop(...)
     21. └─base::.handleSimpleError(...)
     22.   └─dplyr h(simpleError(msg, call))
     23.     └─rlang::abort(...)
    Execution halted
    ```

# SMDIC

<details>

* Version: 0.1.3
* GitHub: NA
* Source code: https://github.com/cran/SMDIC
* Date/Publication: 2020-11-13 15:00:06 UTC
* Number of recursive dependencies: 192

Run `cloud_details(, "SMDIC")` for more info

</details>

## Newly broken

*   checking whether package ‘SMDIC’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/SMDIC/new/SMDIC.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘quadprog’
      All declared Imports should be used.
    ```

## Installation

### Devel

```
* installing *source* package ‘SMDIC’ ...
** package ‘SMDIC’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘SMDIC’
* removing ‘/tmp/workdir/SMDIC/new/SMDIC.Rcheck/SMDIC’


```
### CRAN

```
* installing *source* package ‘SMDIC’ ...
** package ‘SMDIC’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
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
* DONE (SMDIC)


```
# spark.sas7bdat

<details>

* Version: 1.4
* GitHub: https://github.com/bnosac/spark.sas7bdat
* Source code: https://github.com/cran/spark.sas7bdat
* Date/Publication: 2021-04-19 07:30:02 UTC
* Number of recursive dependencies: 58

Run `cloud_details(, "spark.sas7bdat")` for more info

</details>

## Newly broken

*   checking whether package ‘spark.sas7bdat’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/spark.sas7bdat/new/spark.sas7bdat.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘spark.sas7bdat’ ...
** package ‘spark.sas7bdat’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘type_sum’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘spark.sas7bdat’
* removing ‘/tmp/workdir/spark.sas7bdat/new/spark.sas7bdat.Rcheck/spark.sas7bdat’


```
### CRAN

```
* installing *source* package ‘spark.sas7bdat’ ...
** package ‘spark.sas7bdat’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (spark.sas7bdat)


```
# sparkavro

<details>

* Version: 0.3.0
* GitHub: https://github.com/chezou/sparkavro
* Source code: https://github.com/cran/sparkavro
* Date/Publication: 2020-01-10 04:40:02 UTC
* Number of recursive dependencies: 63

Run `cloud_details(, "sparkavro")` for more info

</details>

## Newly broken

*   checking whether package ‘sparkavro’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/sparkavro/new/sparkavro.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

## Installation

### Devel

```
* installing *source* package ‘sparkavro’ ...
** package ‘sparkavro’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘type_sum’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘sparkavro’
* removing ‘/tmp/workdir/sparkavro/new/sparkavro.Rcheck/sparkavro’


```
### CRAN

```
* installing *source* package ‘sparkavro’ ...
** package ‘sparkavro’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (sparkavro)


```
# sparkbq

<details>

* Version: 0.1.1
* GitHub: https://github.com/miraisolutions/sparkbq
* Source code: https://github.com/cran/sparkbq
* Date/Publication: 2019-12-18 18:00:02 UTC
* Number of recursive dependencies: 51

Run `cloud_details(, "sparkbq")` for more info

</details>

## Newly broken

*   checking whether package ‘sparkbq’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/sparkbq/new/sparkbq.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

## Installation

### Devel

```
* installing *source* package ‘sparkbq’ ...
** package ‘sparkbq’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error: object ‘type_sum’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘sparkbq’
* removing ‘/tmp/workdir/sparkbq/new/sparkbq.Rcheck/sparkbq’


```
### CRAN

```
* installing *source* package ‘sparkbq’ ...
** package ‘sparkbq’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (sparkbq)


```
# sparkhail

<details>

* Version: 0.1.1
* GitHub: NA
* Source code: https://github.com/cran/sparkhail
* Date/Publication: 2019-12-23 17:50:02 UTC
* Number of recursive dependencies: 79

Run `cloud_details(, "sparkhail")` for more info

</details>

## Newly broken

*   checking whether package ‘sparkhail’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/sparkhail/new/sparkhail.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

## Installation

### Devel

```
* installing *source* package ‘sparkhail’ ...
** package ‘sparkhail’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘type_sum’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘sparkhail’
* removing ‘/tmp/workdir/sparkhail/new/sparkhail.Rcheck/sparkhail’


```
### CRAN

```
* installing *source* package ‘sparkhail’ ...
** package ‘sparkhail’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (sparkhail)


```
# sparklyr

<details>

* Version: 1.7.4
* GitHub: https://github.com/sparklyr/sparklyr
* Source code: https://github.com/cran/sparklyr
* Date/Publication: 2022-01-08 11:20:02 UTC
* Number of recursive dependencies: 107

Run `cloud_details(, "sparklyr")` for more info

</details>

## Newly broken

*   checking whether package ‘sparklyr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/sparklyr/new/sparklyr.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking dependencies in R code ... WARNING
    ```
    Namespace in Imports field not imported from: ‘lifecycle’
      All declared Imports should be used.
    Missing or unexported object: ‘rlang::is_env’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.3Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        java   3.4Mb
    ```

## Installation

### Devel

```
* installing *source* package ‘sparklyr’ ...
** package ‘sparklyr’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘type_sum’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘sparklyr’
* removing ‘/tmp/workdir/sparklyr/new/sparklyr.Rcheck/sparklyr’


```
### CRAN

```
* installing *source* package ‘sparklyr’ ...
** package ‘sparklyr’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (sparklyr)


```
# sparklyr.flint

<details>

* Version: 0.2.2
* GitHub: https://github.com/r-spark/sparklyr.flint
* Source code: https://github.com/cran/sparklyr.flint
* Date/Publication: 2022-01-11 08:50:13 UTC
* Number of recursive dependencies: 58

Run `cloud_details(, "sparklyr.flint")` for more info

</details>

## Newly broken

*   checking whether package ‘sparklyr.flint’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/sparklyr.flint/new/sparklyr.flint.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘sparklyr.flint’ ...
** package ‘sparklyr.flint’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘type_sum’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘sparklyr.flint’
* removing ‘/tmp/workdir/sparklyr.flint/new/sparklyr.flint.Rcheck/sparklyr.flint’


```
### CRAN

```
* installing *source* package ‘sparklyr.flint’ ...
** package ‘sparklyr.flint’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (sparklyr.flint)


```
# sparklyr.nested

<details>

* Version: 0.0.3
* GitHub: https://github.com/mitre/sparklyr.nested
* Source code: https://github.com/cran/sparklyr.nested
* Date/Publication: 2018-11-14 14:40:03 UTC
* Number of recursive dependencies: 78

Run `cloud_details(, "sparklyr.nested")` for more info

</details>

## Newly broken

*   checking whether package ‘sparklyr.nested’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/sparklyr.nested/new/sparklyr.nested.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

## Installation

### Devel

```
* installing *source* package ‘sparklyr.nested’ ...
** package ‘sparklyr.nested’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error: object ‘type_sum’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘sparklyr.nested’
* removing ‘/tmp/workdir/sparklyr.nested/new/sparklyr.nested.Rcheck/sparklyr.nested’


```
### CRAN

```
* installing *source* package ‘sparklyr.nested’ ...
** package ‘sparklyr.nested’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (sparklyr.nested)


```
# sparktf

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/sparktf
* Date/Publication: 2019-03-05 14:30:03 UTC
* Number of recursive dependencies: 63

Run `cloud_details(, "sparktf")` for more info

</details>

## Newly broken

*   checking whether package ‘sparktf’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/sparktf/new/sparktf.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

## Installation

### Devel

```
* installing *source* package ‘sparktf’ ...
** package ‘sparktf’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error: object ‘type_sum’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘sparktf’
* removing ‘/tmp/workdir/sparktf/new/sparktf.Rcheck/sparktf’


```
### CRAN

```
* installing *source* package ‘sparktf’ ...
** package ‘sparktf’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (sparktf)


```
# sparkwarc

<details>

* Version: 0.1.6
* GitHub: https://github.com/r-spark/sparkwarc
* Source code: https://github.com/cran/sparkwarc
* Date/Publication: 2022-01-11 08:50:02 UTC
* Number of recursive dependencies: 52

Run `cloud_details(, "sparkwarc")` for more info

</details>

## Newly broken

*   checking whether package ‘sparkwarc’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/sparkwarc/new/sparkwarc.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘sparkwarc’ ...
** package ‘sparkwarc’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I/usr/local/include   -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I/usr/local/include   -fpic  -g -O2  -c warc.cpp -o warc.o
g++ -std=gnu++11 -shared -L/opt/R/4.1.1/lib/R/lib -L/usr/local/lib -o sparkwarc.so RcppExports.o warc.o -lz -L/opt/R/4.1.1/lib/R/lib -lR
installing to /tmp/workdir/sparkwarc/new/sparkwarc.Rcheck/00LOCK-sparkwarc/00new/sparkwarc/libs
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘type_sum’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘sparkwarc’
* removing ‘/tmp/workdir/sparkwarc/new/sparkwarc.Rcheck/sparkwarc’


```
### CRAN

```
* installing *source* package ‘sparkwarc’ ...
** package ‘sparkwarc’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I/usr/local/include   -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++11 -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.1.1/lib/R/site-library/Rcpp/include' -I/usr/local/include   -fpic  -g -O2  -c warc.cpp -o warc.o
g++ -std=gnu++11 -shared -L/opt/R/4.1.1/lib/R/lib -L/usr/local/lib -o sparkwarc.so RcppExports.o warc.o -lz -L/opt/R/4.1.1/lib/R/lib -lR
installing to /tmp/workdir/sparkwarc/old/sparkwarc.Rcheck/00LOCK-sparkwarc/00new/sparkwarc/libs
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** checking absolute paths in shared objects and dynamic libraries
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (sparkwarc)


```
# sparkxgb

<details>

* Version: 0.1.1
* GitHub: NA
* Source code: https://github.com/cran/sparkxgb
* Date/Publication: 2021-02-23 10:20:02 UTC
* Number of recursive dependencies: 63

Run `cloud_details(, "sparkxgb")` for more info

</details>

## Newly broken

*   checking whether package ‘sparkxgb’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/sparkxgb/new/sparkxgb.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

## Installation

### Devel

```
* installing *source* package ‘sparkxgb’ ...
** package ‘sparkxgb’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘type_sum’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘sparkxgb’
* removing ‘/tmp/workdir/sparkxgb/new/sparkxgb.Rcheck/sparkxgb’


```
### CRAN

```
* installing *source* package ‘sparkxgb’ ...
** package ‘sparkxgb’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (sparkxgb)


```
# spatialTIME

<details>

* Version: 1.2.0
* GitHub: https://github.com/FridleyLab/spatialTIME
* Source code: https://github.com/cran/spatialTIME
* Date/Publication: 2021-09-11 04:10:02 UTC
* Number of recursive dependencies: 115

Run `cloud_details(, "spatialTIME")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘intro.Rmd’ using rmarkdown
    Quitting from lines 88-135 (intro.Rmd) 
    Error: processing vignette 'intro.Rmd' failed with diagnostics:
    object 'as_data_frame' is not exported by 'namespace:dplyr'
    --- failed re-building ‘intro.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘intro.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘ggpubr’ ‘pheatmap’ ‘rlist’ ‘stats’ ‘viridis’
      All declared Imports should be used.
    ```

# splashr

<details>

* Version: 0.6.0
* GitHub: NA
* Source code: https://github.com/cran/splashr
* Date/Publication: 2019-02-26 18:50:03 UTC
* Number of recursive dependencies: 75

Run `cloud_details(, "splashr")` for more info

</details>

## Newly broken

*   checking whether package ‘splashr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/splashr/new/splashr.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘splashr’ ...
** package ‘splashr’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘splashr’
* removing ‘/tmp/workdir/splashr/new/splashr.Rcheck/splashr’


```
### CRAN

```
* installing *source* package ‘splashr’ ...
** package ‘splashr’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (splashr)


```
# srvyr

<details>

* Version: 1.1.0
* GitHub: https://github.com/gergness/srvyr
* Source code: https://github.com/cran/srvyr
* Date/Publication: 2021-09-29 04:40:02 UTC
* Number of recursive dependencies: 84

Run `cloud_details(, "srvyr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘srvyr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: as_survey_design
    > ### Title: Create a tbl_svy survey object using sampling design
    > ### Aliases: as_survey_design as_survey_design.data.frame
    > ###   as_survey_design.survey.design2 as_survey_design.tbl_lazy
    > 
    > ### ** Examples
    > 
    ...
    +                            nest = TRUE)
    Stratified 1 - level Cluster Sampling design (with replacement)
    With (162) clusters.
    Called via srvyr
    Sampling variables:
     - ids: dnum
     - strata: stype
     - weights: pw
    Error: 'type_sum' is not an exported object from 'namespace:dplyr'
    Execution halted
    ```

*   checking dependencies in R code ... WARNING
    ```
    Missing or unexported object: ‘dplyr::type_sum’
    ```

# SSLR

<details>

* Version: 0.9.3.3
* GitHub: NA
* Source code: https://github.com/cran/SSLR
* Date/Publication: 2021-07-22 08:10:07 UTC
* Number of recursive dependencies: 216

Run `cloud_details(, "SSLR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘SSLR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: EMLeastSquaresClassifierSSLR
    > ### Title: General Interface for EMLeastSquaresClassifier model
    > ### Aliases: EMLeastSquaresClassifierSSLR
    > 
    > ### ** Examples
    > 
    > library(tidyverse)
    ...
    ✔ tidyr   1.1.4     ✔ stringr 1.4.0
    ✔ readr   2.1.1     ✔ forcats 0.5.1
    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    > #' \donttest{
    > library(tidymodels)
    Error: package or namespace load failed for ‘tidymodels’:
     object ‘type_sum’ is not exported by 'namespace:dplyr'
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       5.         └─value[[3L]](cond)
      ── Error (test-tritraining.R:2:1): (code run outside of `test_that()`) ─────────
      Error: package or namespace load failed for 'tidymodels':
       object 'type_sum' is not exported by 'namespace:dplyr'
      Backtrace:
          ▆
       1. └─base::library(tidymodels) at test-tritraining.R:2:0
       2.   └─base::tryCatch(...)
       3.     └─base tryCatchList(expr, classes, parentenv, handlers)
       4.       └─base tryCatchOne(expr, names, parentenv, handlers[[1L]])
       5.         └─value[[3L]](cond)
      
      [ FAIL 15 | WARN 0 | SKIP 0 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘classification.Rmd’ using rmarkdown
    Quitting from lines 23-33 (classification.Rmd) 
    Error: processing vignette 'classification.Rmd' failed with diagnostics:
    package or namespace load failed for 'tidymodels':
     object 'type_sum' is not exported by 'namespace:dplyr'
    --- failed re-building ‘classification.Rmd’
    
    --- re-building ‘clustering.Rmd’ using rmarkdown
    ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──
    ...
    Error: processing vignette 'regression.Rmd' failed with diagnostics:
    package or namespace load failed for 'tidymodels':
     object 'type_sum' is not exported by 'namespace:dplyr'
    --- failed re-building ‘regression.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘classification.Rmd’ ‘clustering.Rmd’ ‘fit.Rmd’ ‘regression.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# stacks

<details>

* Version: 0.2.2
* GitHub: https://github.com/tidymodels/stacks
* Source code: https://github.com/cran/stacks
* Date/Publication: 2022-01-06 01:00:02 UTC
* Number of recursive dependencies: 135

Run `cloud_details(, "stacks")` for more info

</details>

## Newly broken

*   checking whether package ‘stacks’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: namespace ‘dials’ is not available and has been replaced
    See ‘/tmp/workdir/stacks/new/stacks.Rcheck/00install.out’ for details.
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘basics.Rmd’ using rmarkdown
    Quitting from lines 36-45 (basics.Rmd) 
    Error: processing vignette 'basics.Rmd' failed with diagnostics:
    package or namespace load failed for 'tune':
     object 'type_sum' is not exported by 'namespace:dplyr'
    --- failed re-building ‘basics.Rmd’
    
    --- re-building ‘classification.Rmd’ using rmarkdown
    ...
    Error: processing vignette 'classification.Rmd' failed with diagnostics:
    package or namespace load failed for 'tune':
     object 'type_sum' is not exported by 'namespace:dplyr'
    --- failed re-building ‘classification.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘basics.Rmd’ ‘classification.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘workflowsets’ ‘yardstick’
      All declared Imports should be used.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘h2o’, ‘SuperLearner’
    ```

# statgenHTP

<details>

* Version: 1.0.5
* GitHub: https://github.com/Biometris/statgenHTP
* Source code: https://github.com/cran/statgenHTP
* Date/Publication: 2021-09-15 14:30:03 UTC
* Number of recursive dependencies: 131

Run `cloud_details(, "statgenHTP")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘statgenHTP-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: removeSerieOut
    > ### Title: Replace outliers for series of observations by NA
    > ### Aliases: removeSerieOut
    > 
    > ### ** Examples
    > 
    > ## Run the function to fit P-splines on a subset of genotypes.
    ...
    > outVator <- detectSerieOut(corrDat = spatCorrectedVator,
    +                            predDat = predDat,
    +                            coefDat = coefDat,
    +                            trait = "EffpsII_corr",
    +                            genotypes = subGenoVator,
    +                            thrCor = 0.9,
    +                            thrPca = 30,
    +                            thrSlope = 0.7)
    Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘tinytest.R’
    Running the tests in ‘tests/tinytest.R’ failed.
    Last 13 lines of output:
      test_detectSerieOut.R.........    8 tests [0;32mOK[0m 
      test_detectSerieOut.R.........    9 tests [0;32mOK[0m 
      test_detectSerieOut.R.........   10 tests [0;32mOK[0m 
      test_detectSerieOut.R.........   11 tests [0;32mOK[0m 
      test_detectSerieOut.R.........   12 tests [0;32mOK[0m 
      test_detectSerieOut.R.........   13 tests [0;32mOK[0m 
      test_detectSerieOut.R.........   14 tests [0;32mOK[0m 
      test_detectSerieOut.R.........   15 tests [0;32mOK[0m 
      test_detectSerieOut.R.........   16 tests [0;31m1 fails[0m Error in fun(...) : object 'serieOut1' not found
      Calls: <Anonymous> ... lapply -> FUN -> eval -> eval -> expect_inherits -> fun
      In addition: Warning message:
      The following genotypes have less than 3 plotIds and are skipped in the outlier detection:
      G12
       
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Overview_HTP.Rmd’ using rmarkdown
    2018-05-31 16:37:00
    2018-06-08 16:37:00
    2018-06-09 14:37:00
    2018-06-14 09:07:00
    2018-06-18 16:37:00
    Quitting from lines 339-346 (Overview_HTP.Rmd) 
    Error: processing vignette 'Overview_HTP.Rmd' failed with diagnostics:
    object 'as_data_frame' is not exported by 'namespace:dplyr'
    --- failed re-building ‘Overview_HTP.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Overview_HTP.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘asreml’
    ```

# statVisual

<details>

* Version: 1.2.1
* GitHub: NA
* Source code: https://github.com/cran/statVisual
* Date/Publication: 2020-02-20 19:30:02 UTC
* Number of recursive dependencies: 178

Run `cloud_details(, "statVisual")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘statVisual-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: PCA_score
    > ### Title: Scatter Plot of 2 Specified Principal Components
    > ### Aliases: PCA_score
    > ### Keywords: method
    > 
    > ### ** Examples
    > 
    ...
    > pDat$grp = factor(pDat$grp)
    > 
    > ###
    > 
    > pca.obj = iprcomp(pDat[, c(3:8)], scale. = TRUE)
    > 
    > # scree plot
    > factoextra::fviz_eig(pca.obj, addlabels = TRUE)
    Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘statVisual.Rmd’ using rmarkdown
    Loading required package: BiocGenerics
    
    Attaching package: 'BiocGenerics'
    
    The following objects are masked from 'package:stats':
    
        IQR, mad, sd, var, xtabs
    
    ...
    Quitting from lines 760-791 (statVisual.Rmd) 
    Error: processing vignette 'statVisual.Rmd' failed with diagnostics:
    object 'as_data_frame' is not exported by 'namespace:dplyr'
    --- failed re-building ‘statVisual.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘statVisual.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘gbm’ ‘ggfortify’ ‘tibble’ ‘tidyverse’
      All declared Imports should be used.
    ```

# stoRy

<details>

* Version: 0.2.0
* GitHub: https://github.com/theme-ontology/stoRy
* Source code: https://github.com/cran/stoRy
* Date/Publication: 2021-11-08 05:10:02 UTC
* Number of recursive dependencies: 72

Run `cloud_details(, "stoRy")` for more info

</details>

## Newly broken

*   checking whether package ‘stoRy’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/stoRy/new/stoRy.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘stoRy’ ...
** package ‘stoRy’ successfully unpacked and MD5 sums checked
** using staged installation
** R
Warning: namespace ‘stoRy’ is not available and has been replaced
by .GlobalEnv when processing object ‘background_collection’
** inst
** byte-compile and prepare package for lazy loading
Error in namespaceExport(ns, exports) : undefined exports: as_data_frame
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace -> namespaceExport
Execution halted
ERROR: lazy loading failed for package ‘stoRy’
* removing ‘/tmp/workdir/stoRy/new/stoRy.Rcheck/stoRy’


```
### CRAN

```
* installing *source* package ‘stoRy’ ...
** package ‘stoRy’ successfully unpacked and MD5 sums checked
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
* DONE (stoRy)


```
# SurvHiDim

<details>

* Version: 0.1.1
* GitHub: NA
* Source code: https://github.com/cran/SurvHiDim
* Date/Publication: 2021-06-26 15:40:02 UTC
* Number of recursive dependencies: 160

Run `cloud_details(, "SurvHiDim")` for more info

</details>

## Newly broken

*   checking whether package ‘SurvHiDim’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/SurvHiDim/new/SurvHiDim.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘readr’
      All declared Imports should be used.
    ```

## Installation

### Devel

```
* installing *source* package ‘SurvHiDim’ ...
** package ‘SurvHiDim’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error: object ‘data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘SurvHiDim’
* removing ‘/tmp/workdir/SurvHiDim/new/SurvHiDim.Rcheck/SurvHiDim’


```
### CRAN

```
* installing *source* package ‘SurvHiDim’ ...
** package ‘SurvHiDim’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (SurvHiDim)


```
# survival666

<details>

* Version: 0.5
* GitHub: NA
* Source code: https://github.com/cran/survival666
* Date/Publication: 2021-11-29 08:50:05 UTC
* Number of recursive dependencies: 150

Run `cloud_details(, "survival666")` for more info

</details>

## Newly broken

*   checking whether package ‘survival666’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/survival666/new/survival666.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘survival666’ ...
** package ‘survival666’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘survival666’
* removing ‘/tmp/workdir/survival666/new/survival666.Rcheck/survival666’


```
### CRAN

```
* installing *source* package ‘survival666’ ...
** package ‘survival666’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (survival666)


```
# survivalAnalysis

<details>

* Version: 0.2.0
* GitHub: NA
* Source code: https://github.com/cran/survivalAnalysis
* Date/Publication: 2021-04-24 12:20:05 UTC
* Number of recursive dependencies: 154

Run `cloud_details(, "survivalAnalysis")` for more info

</details>

## Newly broken

*   checking whether package ‘survivalAnalysis’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/survivalAnalysis/new/survivalAnalysis.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking examples ... ERROR
    ```
    Running examples in ‘survivalAnalysis-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: forest_plot
    > ### Title: Forest plots for survival analysis.
    > ### Aliases: forest_plot forest_plot.df
    > 
    > ### ** Examples
    > 
    > library(magrittr)
    ...
     15. │   └─mask$eval_all_mutate(quo)
     16. ├─survivalAnalysis remove_sequential_duplicates_unless_breakafter(...)
     17. │ ├─base::ifelse(...)
     18. │ └─tidytidbits::sequential_duplicates(x, ordering = rev(order(ordered_index)))
     19. │   └─rlang::lgl_along(strings)
     20. │     └─rlang:::stop_defunct("`lgl_along()` is deprecated as of rlang 0.2.0.")
     21. │       └─base::stop(err)
     22. └─dplyr `<fn>`(`<dfnctErr>`)
     23.   └─rlang::abort(...)
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘multivariate.Rmd’ using rmarkdown
    ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──
    ✔ ggplot2 3.3.5     ✔ purrr   0.3.4
    ✔ tibble  3.1.6     ✔ dplyr   1.0.7
    ✔ tidyr   1.1.4     ✔ stringr 1.4.0
    ✔ readr   2.1.1     ✔ forcats 0.5.1
    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    ...
    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    --- finished re-building ‘univariate.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘multivariate.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## Installation

### Devel

```
* installing *source* package ‘survivalAnalysis’ ...
** package ‘survivalAnalysis’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘survivalAnalysis’
* removing ‘/tmp/workdir/survivalAnalysis/new/survivalAnalysis.Rcheck/survivalAnalysis’


```
### CRAN

```
* installing *source* package ‘survivalAnalysis’ ...
** package ‘survivalAnalysis’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (survivalAnalysis)


```
# SurvMetrics

<details>

* Version: 0.4.0
* GitHub: https://github.com/skyee1/SurvMetrics
* Source code: https://github.com/cran/SurvMetrics
* Date/Publication: 2022-01-07 12:12:48 UTC
* Number of recursive dependencies: 179

Run `cloud_details(, "SurvMetrics")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘SurvMetrics-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: Brier
    > ### Title: Brier
    > ### Aliases: Brier
    > 
    > ### ** Examples
    > 
    > library(survival)
    > time = rexp(50)
    > status = sample(c(0,1),50,replace = TRUE)
    > pre_sp = runif(50)
    > t_star = runif(1)
    > Brier(Surv(time,status),pre_sp,t_star)
    Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘SurvMetrics-vignette.Rmd’ using rmarkdown
    Loading required package: ggplot2
    Loading required package: lattice
    
    Attaching package: 'caret'
    
    The following object is masked from 'package:SurvMetrics':
    
        MAE
    ...
    Quitting from lines 152-189 (SurvMetrics-vignette.Rmd) 
    Error: processing vignette 'SurvMetrics-vignette.Rmd' failed with diagnostics:
    object 'as_data_frame' is not exported by 'namespace:dplyr'
    --- failed re-building ‘SurvMetrics-vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘SurvMetrics-vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# survminer

<details>

* Version: 0.4.9
* GitHub: https://github.com/kassambara/survminer
* Source code: https://github.com/cran/survminer
* Date/Publication: 2021-03-09 09:50:03 UTC
* Number of recursive dependencies: 125

Run `cloud_details(, "survminer")` for more info

</details>

## Newly broken

*   checking whether package ‘survminer’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/survminer/new/survminer.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking installed package size ... NOTE
    ```
      installed size is  6.0Mb
      sub-directories of 1Mb or more:
        doc   5.5Mb
    ```

## Installation

### Devel

```
* installing *source* package ‘survminer’ ...
** package ‘survminer’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘ggpubr’:
 object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘survminer’
* removing ‘/tmp/workdir/survminer/new/survminer.Rcheck/survminer’


```
### CRAN

```
* installing *source* package ‘survminer’ ...
** package ‘survminer’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (survminer)


```
# survParamSim

<details>

* Version: 0.1.5
* GitHub: https://github.com/yoshidk6/survParamSim
* Source code: https://github.com/cran/survParamSim
* Date/Publication: 2021-04-26 22:10:02 UTC
* Number of recursive dependencies: 115

Run `cloud_details(, "survParamSim")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘survParamSim.Rmd’ using rmarkdown
    Quitting from lines 53-58 (survParamSim.Rmd) 
    Error: processing vignette 'survParamSim.Rmd' failed with diagnostics:
    object 'as_data_frame' is not exported by 'namespace:dplyr'
    --- failed re-building ‘survParamSim.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘survParamSim.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# survxai

<details>

* Version: 0.2.2
* GitHub: https://github.com/MI2DataLab/survxai
* Source code: https://github.com/cran/survxai
* Date/Publication: 2020-08-28 11:30:03 UTC
* Number of recursive dependencies: 180

Run `cloud_details(, "survxai")` for more info

</details>

## Newly broken

*   checking whether package ‘survxai’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/survxai/new/survxai.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘survxai’ ...
** package ‘survxai’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘survxai’
* removing ‘/tmp/workdir/survxai/new/survxai.Rcheck/survxai’


```
### CRAN

```
* installing *source* package ‘survxai’ ...
** package ‘survxai’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (survxai)


```
# tabxplor

<details>

* Version: 1.0.2
* GitHub: https://github.com/BriceNocenti/tabxplor
* Source code: https://github.com/cran/tabxplor
* Date/Publication: 2021-10-21 07:10:02 UTC
* Number of recursive dependencies: 89

Run `cloud_details(, "tabxplor")` for more info

</details>

## Newly broken

*   checking whether package ‘tabxplor’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/tabxplor/new/tabxplor.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘tabxplor’ ...
** package ‘tabxplor’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘tbl_sum’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘tabxplor’
* removing ‘/tmp/workdir/tabxplor/new/tabxplor.Rcheck/tabxplor’


```
### CRAN

```
* installing *source* package ‘tabxplor’ ...
** package ‘tabxplor’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (tabxplor)


```
# tbrf

<details>

* Version: 0.1.5
* GitHub: https://github.com/mps9506/tbrf
* Source code: https://github.com/cran/tbrf
* Date/Publication: 2020-04-09 04:40:02 UTC
* Number of recursive dependencies: 94

Run `cloud_details(, "tbrf")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘intro_to_tbrf.Rmd’ using rmarkdown
    Quitting from lines 70-77 (intro_to_tbrf.Rmd) 
    Error: processing vignette 'intro_to_tbrf.Rmd' failed with diagnostics:
    could not find function "data_frame"
    --- failed re-building ‘intro_to_tbrf.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘intro_to_tbrf.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# TCIU

<details>

* Version: 1.2.0
* GitHub: https://github.com/SOCR/TCIU
* Source code: https://github.com/cran/TCIU
* Date/Publication: 2021-04-30 15:10:03 UTC
* Number of recursive dependencies: 173

Run `cloud_details(, "TCIU")` for more info

</details>

## Newly broken

*   checking whether package ‘TCIU’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/TCIU/new/TCIU.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking installed package size ... NOTE
    ```
      installed size is 12.4Mb
      sub-directories of 1Mb or more:
        doc  11.2Mb
    ```

## Installation

### Devel

```
* installing *source* package ‘TCIU’ ...
** package ‘TCIU’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
gcc -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG   -I/usr/local/include   -fpic  -g -O2  -c GLS.c -o GLS.o
gcc -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG   -I/usr/local/include   -fpic  -g -O2  -c RC_interface.c -o RC_interface.o
gcc -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG   -I/usr/local/include   -fpic  -g -O2  -c complex_Sig_gen.c -o complex_Sig_gen.o
gcc -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG   -I/usr/local/include   -fpic  -g -O2  -c complex_Sig_sig2I.c -o complex_Sig_sig2I.o
gcc -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG   -I/usr/local/include   -fpic  -g -O2  -c complex_smoother.c -o complex_smoother.o
gcc -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG   -I/usr/local/include   -fpic  -g -O2  -c mat_vec.c -o mat_vec.o
...
** R
** data
*** moving datasets to lazyload DB
** demo
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘TCIU’
* removing ‘/tmp/workdir/TCIU/new/TCIU.Rcheck/TCIU’


```
### CRAN

```
* installing *source* package ‘TCIU’ ...
** package ‘TCIU’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
gcc -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG   -I/usr/local/include   -fpic  -g -O2  -c GLS.c -o GLS.o
gcc -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG   -I/usr/local/include   -fpic  -g -O2  -c RC_interface.c -o RC_interface.o
gcc -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG   -I/usr/local/include   -fpic  -g -O2  -c complex_Sig_gen.c -o complex_Sig_gen.o
gcc -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG   -I/usr/local/include   -fpic  -g -O2  -c complex_Sig_sig2I.c -o complex_Sig_sig2I.o
gcc -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG   -I/usr/local/include   -fpic  -g -O2  -c complex_smoother.c -o complex_smoother.o
gcc -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG   -I/usr/local/include   -fpic  -g -O2  -c mat_vec.c -o mat_vec.o
...
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** checking absolute paths in shared objects and dynamic libraries
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (TCIU)


```
# TestGardener

<details>

* Version: 2.0.0
* GitHub: NA
* Source code: https://github.com/cran/TestGardener
* Date/Publication: 2021-11-10 20:10:02 UTC
* Number of recursive dependencies: 116

Run `cloud_details(, "TestGardener")` for more info

</details>

## Newly broken

*   checking whether package ‘TestGardener’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/TestGardener/new/TestGardener.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘TestGardener’ ...
** package ‘TestGardener’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** demo
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘TestGardener’
* removing ‘/tmp/workdir/TestGardener/new/TestGardener.Rcheck/TestGardener’


```
### CRAN

```
* installing *source* package ‘TestGardener’ ...
** package ‘TestGardener’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** demo
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (TestGardener)


```
# text

<details>

* Version: 0.9.10
* GitHub: https://github.com/OscarKjell/text
* Source code: https://github.com/cran/text
* Date/Publication: 2020-12-14 09:50:02 UTC
* Number of recursive dependencies: 146

Run `cloud_details(, "text")` for more info

</details>

## Newly broken

*   checking whether package ‘text’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/text/new/text.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 3 marked UTF-8 strings
    ```

## Installation

### Devel

```
* installing *source* package ‘text’ ...
** package ‘text’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘type_sum’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘text’
* removing ‘/tmp/workdir/text/new/text.Rcheck/text’


```
### CRAN

```
* installing *source* package ‘text’ ...
** package ‘text’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
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
* DONE (text)


```
# tidyboot

<details>

* Version: 0.1.1
* GitHub: https://github.com/langcog/tidyboot
* Source code: https://github.com/cran/tidyboot
* Date/Publication: 2018-03-14 04:13:49 UTC
* Number of recursive dependencies: 42

Run `cloud_details(, "tidyboot")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘tidyboot-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: tidyboot.data.frame
    > ### Title: Non-parametric bootstrap for data frames
    > ### Aliases: tidyboot.data.frame
    > 
    > ### ** Examples
    > 
    > ## Mean and 95% confidence interval for 500 samples from two different normal distributions
    ...
        filter, lag
    
    The following objects are masked from ‘package:base’:
    
        intersect, setdiff, setequal, union
    
    > gauss1 <- data_frame(value = rnorm(500, mean = 0, sd = 1), condition = 1)
    Error in data_frame(value = rnorm(500, mean = 0, sd = 1), condition = 1) : 
      could not find function "data_frame"
    Execution halted
    ```

*   checking dependencies in R code ... WARNING
    ```
    Missing or unexported objects:
      ‘dplyr::as_data_frame’ ‘dplyr::data_frame’
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# tidyjson

<details>

* Version: 0.3.1
* GitHub: https://github.com/colearendt/tidyjson
* Source code: https://github.com/cran/tidyjson
* Date/Publication: 2020-05-31 21:30:03 UTC
* Number of recursive dependencies: 96

Run `cloud_details(, "tidyjson")` for more info

</details>

## Newly broken

*   checking whether package ‘tidyjson’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/tidyjson/new/tidyjson.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘tidyjson’ ...
** package ‘tidyjson’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error : 'as_data_frame' is not an exported object from 'namespace:dplyr'
Error: unable to load R code in package ‘tidyjson’
Execution halted
ERROR: lazy loading failed for package ‘tidyjson’
* removing ‘/tmp/workdir/tidyjson/new/tidyjson.Rcheck/tidyjson’


```
### CRAN

```
* installing *source* package ‘tidyjson’ ...
** package ‘tidyjson’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (tidyjson)


```
# tidymodels

<details>

* Version: 0.1.4
* GitHub: https://github.com/tidymodels/tidymodels
* Source code: https://github.com/cran/tidymodels
* Date/Publication: 2021-10-01 18:50:02 UTC
* Number of recursive dependencies: 123

Run `cloud_details(, "tidymodels")` for more info

</details>

## Newly broken

*   checking whether package ‘tidymodels’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/tidymodels/new/tidymodels.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘hardhat’ ‘modeldata’
      All declared Imports should be used.
    ```

## Installation

### Devel

```
* installing *source* package ‘tidymodels’ ...
** package ‘tidymodels’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘type_sum’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘tidymodels’
* removing ‘/tmp/workdir/tidymodels/new/tidymodels.Rcheck/tidymodels’


```
### CRAN

```
* installing *source* package ‘tidymodels’ ...
** package ‘tidymodels’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (tidymodels)


```
# tidyposterior

<details>

* Version: 0.1.0
* GitHub: https://github.com/tidymodels/tidyposterior
* Source code: https://github.com/cran/tidyposterior
* Date/Publication: 2021-03-25 13:20:02 UTC
* Number of recursive dependencies: 165

Run `cloud_details(, "tidyposterior")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error (test_perf_mod.R:4:1): (code run outside of `test_that()`) ────────────
      Error: package or namespace load failed for 'workflowsets':
       object 'type_sum' is not exported by 'namespace:dplyr'
      Backtrace:
          ▆
       1. └─base::library(workflowsets) at test_perf_mod.R:4:0
       2.   └─base::tryCatch(...)
       3.     └─base tryCatchList(expr, classes, parentenv, handlers)
       4.       └─base tryCatchOne(expr, names, parentenv, handlers[[1L]])
       5.         └─value[[3L]](cond)
      
      [ FAIL 1 | WARN 8 | SKIP 0 | PASS 47 ]
      Error: Test failures
      Execution halted
    ```

# tidyqwi

<details>

* Version: 0.1.2
* GitHub: https://github.com/medewitt/tidyqwi
* Source code: https://github.com/cran/tidyqwi
* Date/Publication: 2020-05-04 10:30:02 UTC
* Number of recursive dependencies: 77

Run `cloud_details(, "tidyqwi")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... WARNING
    ```
    Missing or unexported object: ‘dplyr::as_data_frame’
    ```

# tidytransit

<details>

* Version: 1.2.0
* GitHub: https://github.com/r-transit/tidytransit
* Source code: https://github.com/cran/tidytransit
* Date/Publication: 2021-11-23 12:10:02 UTC
* Number of recursive dependencies: 89

Run `cloud_details(, "tidytransit")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... WARNING
    ```
    Missing or unexported object: ‘dplyr::data_frame’
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.2Mb
      sub-directories of 1Mb or more:
        doc       1.3Mb
        extdata   4.4Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 61 marked UTF-8 strings
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

## Newly broken

*   checking whether package ‘tigger’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/tigger/new/tigger.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘tigger’ ...
** package ‘tigger’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘tigger’
* removing ‘/tmp/workdir/tigger/new/tigger.Rcheck/tigger’


```
### CRAN

```
* installing *source* package ‘tigger’ ...
** package ‘tigger’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (tigger)


```
# timetk

<details>

* Version: 2.7.0
* GitHub: https://github.com/business-science/timetk
* Source code: https://github.com/cran/timetk
* Date/Publication: 2022-01-19 15:20:02 UTC
* Number of recursive dependencies: 210

Run `cloud_details(, "timetk")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘timetk-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: step_slidify_augment
    > ### Title: Slidify Rolling Window Transformation (Augmented Version)
    > ### Aliases: step_slidify_augment tidy.step_slidify_augment
    > ### Keywords: datagen
    > 
    > ### ** Examples
    > 
    > library(tidymodels)
    Error: package or namespace load failed for ‘tidymodels’:
     object ‘type_sum’ is not exported by 'namespace:dplyr'
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2750 marked UTF-8 strings
    ```

# tinyarray

<details>

* Version: 2.2.7
* GitHub: https://github.com/xjsun1221/tinyarray
* Source code: https://github.com/cran/tinyarray
* Date/Publication: 2021-11-08 10:00:02 UTC
* Number of recursive dependencies: 221

Run `cloud_details(, "tinyarray")` for more info

</details>

## Newly broken

*   checking whether package ‘tinyarray’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/tinyarray/new/tinyarray.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2 marked UTF-8 strings
    ```

## Installation

### Devel

```
* installing *source* package ‘tinyarray’ ...
** package ‘tinyarray’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘tinyarray’
* removing ‘/tmp/workdir/tinyarray/new/tinyarray.Rcheck/tinyarray’


```
### CRAN

```
* installing *source* package ‘tinyarray’ ...
** package ‘tinyarray’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (tinyarray)


```
# TITAN2

<details>

* Version: 2.4.1
* GitHub: NA
* Source code: https://github.com/cran/TITAN2
* Date/Publication: 2020-12-07 16:30:03 UTC
* Number of recursive dependencies: 54

Run `cloud_details(, "TITAN2")` for more info

</details>

## Newly broken

*   checking whether package ‘TITAN2’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/TITAN2/new/TITAN2.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘snow’
      All declared Imports should be used.
    ```

## Installation

### Devel

```
* installing *source* package ‘TITAN2’ ...
** package ‘TITAN2’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘TITAN2’
* removing ‘/tmp/workdir/TITAN2/new/TITAN2.Rcheck/TITAN2’


```
### CRAN

```
* installing *source* package ‘TITAN2’ ...
** package ‘TITAN2’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (TITAN2)


```
# trackter

<details>

* Version: 0.1.1
* GitHub: NA
* Source code: https://github.com/cran/trackter
* Date/Publication: 2021-04-18 22:10:02 UTC
* Number of recursive dependencies: 116

Run `cloud_details(, "trackter")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘trackter-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: kin.search
    > ### Title: Midline tracking over image sequences
    > ### Aliases: kin.search
    > 
    > ### ** Examples
    > 
    > 
    ...
    > 
    > dir.create(paste0(t,"/images"))
    > EBImage::writeImage(i,paste0(t,"/images/sunfish001.jpg"),type = "jpeg")
    > 
    > list.files(paste0(t,"/images"))
    [1] "sunfish001.jpg"
    > #run kin.search and save output image to directory
    > kin.i<- kin.search(image.dir = paste0(t,"/images"),smooth=0.7,save = TRUE,out.dir = t)
    Error: 'data_frame' is not an exported object from 'namespace:dplyr'
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       5.   └─Momocs:::Out.list(EBImage::ocontour(z.m))
       6.     └─base::structure(...)
      ── Error (test-kin.R:101:3): fin.kin works fine ────────────────────────────────
      Error: 'data_frame' is not an exported object from 'namespace:dplyr'
      Backtrace:
          ▆
       1. └─trackter::fin.kin(cont, fin.pos = c(0.25, 0.5)) at test-kin.R:101:2
       2.   ├─Momocs::coo_flipx(Out(list(x.m)))
       3.   ├─Momocs::Out(list(x.m))
       4.   └─Momocs:::Out.list(list(x.m))
       5.     └─base::structure(...)
      
      [ FAIL 3 | WARN 2 | SKIP 0 | PASS 84 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘ggplot2’ ‘jpeg’ ‘pastecs’ ‘plyr’ ‘raster’
      All declared Imports should be used.
    ```

# trawl

<details>

* Version: 0.2.2
* GitHub: NA
* Source code: https://github.com/cran/trawl
* Date/Publication: 2021-02-22 17:30:02 UTC
* Number of recursive dependencies: 107

Run `cloud_details(, "trawl")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘my-vignette.Rmd’ using rmarkdown
    --- finished re-building ‘my-vignette.Rmd’
    
    --- re-building ‘my-vignette2.Rmd’ using rmarkdown
    Quitting from lines 212-291 (my-vignette2.Rmd) 
    Error: processing vignette 'my-vignette2.Rmd' failed with diagnostics:
    object 'as_data_frame' is not exported by 'namespace:dplyr'
    --- failed re-building ‘my-vignette2.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘my-vignette2.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# tune

<details>

* Version: 0.1.6
* GitHub: https://github.com/tidymodels/tune
* Source code: https://github.com/cran/tune
* Date/Publication: 2021-07-21 14:40:06 UTC
* Number of recursive dependencies: 113

Run `cloud_details(, "tune")` for more info

</details>

## Newly broken

*   checking whether package ‘tune’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/tune/new/tune.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘tune’ ...
** package ‘tune’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘type_sum’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘tune’
* removing ‘/tmp/workdir/tune/new/tune.Rcheck/tune’


```
### CRAN

```
* installing *source* package ‘tune’ ...
** package ‘tune’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (tune)


```
# TwitterAutomatedTrading

<details>

* Version: 0.1.0
* GitHub: https://github.com/lucasgodeiro/TwitterAutomatedTrading
* Source code: https://github.com/cran/TwitterAutomatedTrading
* Date/Publication: 2020-05-31 09:50:13 UTC
* Number of recursive dependencies: 60

Run `cloud_details(, "TwitterAutomatedTrading")` for more info

</details>

## Newly broken

*   checking whether package ‘TwitterAutomatedTrading’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/TwitterAutomatedTrading/new/TwitterAutomatedTrading.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 34 marked Latin-1 strings
    ```

## Installation

### Devel

```
* installing *source* package ‘TwitterAutomatedTrading’ ...
** package ‘TwitterAutomatedTrading’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘TwitterAutomatedTrading’
* removing ‘/tmp/workdir/TwitterAutomatedTrading/new/TwitterAutomatedTrading.Rcheck/TwitterAutomatedTrading’


```
### CRAN

```
* installing *source* package ‘TwitterAutomatedTrading’ ...
** package ‘TwitterAutomatedTrading’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (TwitterAutomatedTrading)


```
# UCSCXenaShiny

<details>

* Version: 1.1.5
* GitHub: https://github.com/openbiox/UCSCXenaShiny
* Source code: https://github.com/cran/UCSCXenaShiny
* Date/Publication: 2022-01-15 11:32:42 UTC
* Number of recursive dependencies: 187

Run `cloud_details(, "UCSCXenaShiny")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘api.Rmd’ using rmarkdown
    Quitting from lines 89-99 (api.Rmd) 
    Error: processing vignette 'api.Rmd' failed with diagnostics:
    object 'as_data_frame' is not exported by 'namespace:dplyr'
    --- failed re-building ‘api.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘api.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.1Mb
      sub-directories of 1Mb or more:
        doc        1.5Mb
        shinyapp   3.4Mb
    ```

# ukbtools

<details>

* Version: 0.11.3
* GitHub: NA
* Source code: https://github.com/cran/ukbtools
* Date/Publication: 2019-05-15 11:40:03 UTC
* Number of recursive dependencies: 66

Run `cloud_details(, "ukbtools")` for more info

</details>

## Newly broken

*   checking R code for possible problems ... NOTE
    ```
    ukb_df_field: no visible global function definition for ‘data_frame’
    Undefined global functions or variables:
      data_frame
    ```

# UniprotR

<details>

* Version: 2.1.0
* GitHub: https://github.com/Proteomicslab57357/UniprotR
* Source code: https://github.com/cran/UniprotR
* Date/Publication: 2021-09-23 07:10:01 UTC
* Number of recursive dependencies: 179

Run `cloud_details(, "UniprotR")` for more info

</details>

## Newly broken

*   checking whether package ‘UniprotR’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/UniprotR/new/UniprotR.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘UniprotR’ ...
** package ‘UniprotR’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘UniprotR’
* removing ‘/tmp/workdir/UniprotR/new/UniprotR.Rcheck/UniprotR’


```
### CRAN

```
* installing *source* package ‘UniprotR’ ...
** package ‘UniprotR’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (UniprotR)


```
# useful

<details>

* Version: 1.2.6
* GitHub: https://github.com/jaredlander/useful
* Source code: https://github.com/cran/useful
* Date/Publication: 2018-10-08 16:00:03 UTC
* Number of recursive dependencies: 63

Run `cloud_details(, "useful")` for more info

</details>

## Newly broken

*   checking whether package ‘useful’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/useful/new/useful.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘useful’ ...
** package ‘useful’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error: object ‘data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘useful’
* removing ‘/tmp/workdir/useful/new/useful.Rcheck/useful’


```
### CRAN

```
* installing *source* package ‘useful’ ...
** package ‘useful’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (useful)


```
# usemodels

<details>

* Version: 0.1.0
* GitHub: https://github.com/tidymodels/usemodels
* Source code: https://github.com/cran/usemodels
* Date/Publication: 2020-11-17 13:20:02 UTC
* Number of recursive dependencies: 112

Run `cloud_details(, "usemodels")` for more info

</details>

## Newly broken

*   checking whether package ‘usemodels’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/usemodels/new/usemodels.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘cli’
      All declared Imports should be used.
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

## Installation

### Devel

```
* installing *source* package ‘usemodels’ ...
** package ‘usemodels’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘type_sum’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘usemodels’
* removing ‘/tmp/workdir/usemodels/new/usemodels.Rcheck/usemodels’


```
### CRAN

```
* installing *source* package ‘usemodels’ ...
** package ‘usemodels’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (usemodels)


```
# VALERIE

<details>

* Version: 1.1.0
* GitHub: NA
* Source code: https://github.com/cran/VALERIE
* Date/Publication: 2020-07-10 10:20:13 UTC
* Number of recursive dependencies: 124

Run `cloud_details(, "VALERIE")` for more info

</details>

## Newly broken

*   checking whether package ‘VALERIE’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/VALERIE/new/VALERIE.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking installed package size ... NOTE
    ```
      installed size is  9.5Mb
      sub-directories of 1Mb or more:
        extdata   8.7Mb
    ```

## Installation

### Devel

```
* installing *source* package ‘VALERIE’ ...
** package ‘VALERIE’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘VALERIE’
* removing ‘/tmp/workdir/VALERIE/new/VALERIE.Rcheck/VALERIE’


```
### CRAN

```
* installing *source* package ‘VALERIE’ ...
** package ‘VALERIE’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (VALERIE)


```
# vannstats

<details>

* Version: 1.2.1.19
* GitHub: NA
* Source code: https://github.com/cran/vannstats
* Date/Publication: 2022-01-20 09:10:02 UTC
* Number of recursive dependencies: 133

Run `cloud_details(, "vannstats")` for more info

</details>

## Newly broken

*   checking whether package ‘vannstats’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/vannstats/new/vannstats.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘MASS’ ‘car’ ‘tidyverse’
      All declared Imports should be used.
    ```

## Installation

### Devel

```
* installing *source* package ‘vannstats’ ...
** package ‘vannstats’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘vannstats’
* removing ‘/tmp/workdir/vannstats/new/vannstats.Rcheck/vannstats’


```
### CRAN

```
* installing *source* package ‘vannstats’ ...
** package ‘vannstats’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (vannstats)


```
# variantspark

<details>

* Version: 0.1.1
* GitHub: NA
* Source code: https://github.com/cran/variantspark
* Date/Publication: 2019-06-13 16:20:03 UTC
* Number of recursive dependencies: 63

Run `cloud_details(, "variantspark")` for more info

</details>

## Newly broken

*   checking whether package ‘variantspark’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/variantspark/new/variantspark.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

## Installation

### Devel

```
* installing *source* package ‘variantspark’ ...
** package ‘variantspark’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘type_sum’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘variantspark’
* removing ‘/tmp/workdir/variantspark/new/variantspark.Rcheck/variantspark’


```
### CRAN

```
* installing *source* package ‘variantspark’ ...
** package ‘variantspark’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (variantspark)


```
# vici

<details>

* Version: 0.5.2
* GitHub: https://github.com/borishejblum/vici
* Source code: https://github.com/cran/vici
* Date/Publication: 2019-08-21 09:20:02 UTC
* Number of recursive dependencies: 109

Run `cloud_details(, "vici")` for more info

</details>

## Newly broken

*   checking whether package ‘vici’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/vici/new/vici.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘vici’ ...
** package ‘vici’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘vici’
* removing ‘/tmp/workdir/vici/new/vici.Rcheck/vici’


```
### CRAN

```
* installing *source* package ‘vici’ ...
** package ‘vici’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (vici)


```
# viewpoly

<details>

* Version: 0.1.1
* GitHub: https://github.com/mmollina/viewpoly
* Source code: https://github.com/cran/viewpoly
* Date/Publication: 2022-01-03 16:10:02 UTC
* Number of recursive dependencies: 154

Run `cloud_details(, "viewpoly")` for more info

</details>

## Newly broken

*   checking whether package ‘viewpoly’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/viewpoly/new/viewpoly.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘viewpoly’ ...
** package ‘viewpoly’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘viewpoly’
* removing ‘/tmp/workdir/viewpoly/new/viewpoly.Rcheck/viewpoly’


```
### CRAN

```
* installing *source* package ‘viewpoly’ ...
** package ‘viewpoly’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (viewpoly)


```
# vmeasur

<details>

* Version: 0.1.4
* GitHub: NA
* Source code: https://github.com/cran/vmeasur
* Date/Publication: 2021-11-11 19:00:02 UTC
* Number of recursive dependencies: 122

Run `cloud_details(, "vmeasur")` for more info

</details>

## Newly broken

*   checking whether package ‘vmeasur’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/vmeasur/new/vmeasur.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘vmeasur’ ...
** package ‘vmeasur’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘vmeasur’
* removing ‘/tmp/workdir/vmeasur/new/vmeasur.Rcheck/vmeasur’


```
### CRAN

```
* installing *source* package ‘vmeasur’ ...
** package ‘vmeasur’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (vmeasur)


```
# volcano3D

<details>

* Version: 1.2.0
* GitHub: https://github.com/KatrionaGoldmann/volcano3D
* Source code: https://github.com/cran/volcano3D
* Date/Publication: 2021-03-31 14:40:02 UTC
* Number of recursive dependencies: 124

Run `cloud_details(, "volcano3D")` for more info

</details>

## Newly broken

*   checking whether package ‘volcano3D’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/volcano3D/new/volcano3D.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘volcano3D’ ...
** package ‘volcano3D’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘volcano3D’
* removing ‘/tmp/workdir/volcano3D/new/volcano3D.Rcheck/volcano3D’


```
### CRAN

```
* installing *source* package ‘volcano3D’ ...
** package ‘volcano3D’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (volcano3D)


```
# vsd

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/vsd
* Date/Publication: 2021-05-11 09:40:02 UTC
* Number of recursive dependencies: 133

Run `cloud_details(, "vsd")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘vsd-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: vsd
    > ### Title: Visualizing Survival Data
    > ### Aliases: vsd vsd.formula vsd.Surv vsd.coxph vsd.survfit vsd.survfitcox
    > ###   vsd.flexsurvreg
    > 
    > ### ** Examples
    > 
    > # non-models are cohersed into a survfit object with default arguments
    > vsd(coxph(Surv(time, status) ~ sex + strata(rx) + adhere, data = colon), 
    +     .include = c("haz"))
    Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘flexsurv’
      All declared Imports should be used.
    ```

# VWPre

<details>

* Version: 1.2.4
* GitHub: NA
* Source code: https://github.com/cran/VWPre
* Date/Publication: 2020-11-29 17:10:02 UTC
* Number of recursive dependencies: 69

Run `cloud_details(, "VWPre")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘VWPre_Basic_Preprocessing.Rmd’ using rmarkdown
    Loading required package: dplyr
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    ...
    Surface calculated using Event means.
    Surface calculated using Event means.
    Grand average calculated using Event means.
    --- finished re-building ‘VWPre_Plotting.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘VWPre_Interest_Areas.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# waccR

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/waccR
* Date/Publication: 2017-07-08 22:54:44 UTC
* Number of recursive dependencies: 58

Run `cloud_details(, "waccR")` for more info

</details>

## Newly broken

*   checking whether package ‘waccR’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/waccR/new/waccR.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘lubridate’ ‘tibble’
      All declared Imports should be used.
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

## Installation

### Devel

```
* installing *source* package ‘waccR’ ...
** package ‘waccR’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘waccR’
* removing ‘/tmp/workdir/waccR/new/waccR.Rcheck/waccR’


```
### CRAN

```
* installing *source* package ‘waccR’ ...
** package ‘waccR’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (waccR)


```
# wakefield

<details>

* Version: 0.3.6
* GitHub: https://github.com/trinker/wakefield
* Source code: https://github.com/cran/wakefield
* Date/Publication: 2020-09-13 17:30:02 UTC
* Number of recursive dependencies: 52

Run `cloud_details(, "wakefield")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘wakefield-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: r_insert
    > ### Title: Insert Data Frames Into 'r_data_frame'
    > ### Aliases: r_insert
    > ### Keywords: insert
    > 
    > ### ** Examples
    > 
    > dat <- dplyr::data_frame(
    +     Age_1 = age(100), Age_2 = age(100), Age_3 = age(100),
    +     Smokes = smokes(n=100),
    +     Sick = ifelse(Smokes, sample(5:10, 100, TRUE), sample(0:4, 100, TRUE)),
    +     Death = ifelse(Smokes, sample(0:1, 100, TRUE, prob = c(.2, .8)),
    +         sample(0:1, 100, TRUE, prob = c(.7, .3)))
    + )
    Error: 'data_frame' is not an exported object from 'namespace:dplyr'
    Execution halted
    ```

# webr

<details>

* Version: 0.1.5
* GitHub: https://github.com/cardiomoon/webr
* Source code: https://github.com/cran/webr
* Date/Publication: 2020-01-26 14:20:02 UTC
* Number of recursive dependencies: 158

Run `cloud_details(, "webr")` for more info

</details>

## Newly broken

*   checking whether package ‘webr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/webr/new/webr.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘shiny’ ‘tidyselect’
      All declared Imports should be used.
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

## Installation

### Devel

```
* installing *source* package ‘webr’ ...
** package ‘webr’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘webr’
* removing ‘/tmp/workdir/webr/new/webr.Rcheck/webr’


```
### CRAN

```
* installing *source* package ‘webr’ ...
** package ‘webr’ successfully unpacked and MD5 sums checked
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
* DONE (webr)


```
# workflowsets

<details>

* Version: 0.1.0
* GitHub: https://github.com/tidymodels/workflowsets
* Source code: https://github.com/cran/workflowsets
* Date/Publication: 2021-07-22 14:00:02 UTC
* Number of recursive dependencies: 120

Run `cloud_details(, "workflowsets")` for more info

</details>

## Newly broken

*   checking whether package ‘workflowsets’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/workflowsets/new/workflowsets.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘workflowsets’ ...
** package ‘workflowsets’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
Warning: namespace ‘dials’ is not available and has been replaced
by .GlobalEnv when processing object ‘chi_features_res’
Warning: namespace ‘dials’ is not available and has been replaced
by .GlobalEnv when processing object ‘chi_features_res’
...
Warning: namespace ‘dials’ is not available and has been replaced
by .GlobalEnv when processing object ‘two_class_res’
Warning: namespace ‘dials’ is not available and has been replaced
by .GlobalEnv when processing object ‘two_class_res’
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘type_sum’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘workflowsets’
* removing ‘/tmp/workdir/workflowsets/new/workflowsets.Rcheck/workflowsets’


```
### CRAN

```
* installing *source* package ‘workflowsets’ ...
** package ‘workflowsets’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
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
* DONE (workflowsets)


```
# xesreadR

<details>

* Version: 0.2.3
* GitHub: NA
* Source code: https://github.com/cran/xesreadR
* Date/Publication: 2019-03-19 12:50:03 UTC
* Number of recursive dependencies: 59

Run `cloud_details(, "xesreadR")` for more info

</details>

## Newly broken

*   checking R code for possible problems ... NOTE
    ```
    read_xes: no visible global function definition for ‘data_frame’
    read_xes: no visible global function definition for ‘as_data_frame’
    read_xes_cases: no visible global function definition for ‘data_frame’
    read_xes_cases: no visible global function definition for
      ‘as_data_frame’
    Undefined global functions or variables:
      as_data_frame data_frame
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# xspliner

<details>

* Version: 0.0.4
* GitHub: https://github.com/ModelOriented/xspliner
* Source code: https://github.com/cran/xspliner
* Date/Publication: 2019-09-25 20:20:02 UTC
* Number of recursive dependencies: 179

Run `cloud_details(, "xspliner")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘xspliner-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_variable_transition
    > ### Title: Plot variable profile
    > ### Aliases: plot_variable_transition
    > 
    > ### ** Examples
    > 
    > library(randomForest)
    randomForest 4.6-14
    Type rfNews() to see new features/changes/bug fixes.
    > set.seed(1)
    > data <- iris
    > # regression model
    > iris.rf <- randomForest(Petal.Width ~  Sepal.Length + Petal.Length + Species, data = data)
    > iris.xs <- xspline(iris.rf)
    Error: object ‘as_data_frame’ is not exported by 'namespace:dplyr'
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘automation.Rmd’ using rmarkdown
    
    Attaching package: 'xspliner'
    
    The following object is masked from 'package:graphics':
    
        xspline
    
    randomForest 4.6-14
    ...
    
        margin
    
    --- finished re-building ‘xspliner.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘automation.Rmd’ ‘cases.Rmd’ ‘discrete.Rmd’ ‘methods.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# ZipRadius

<details>

* Version: 1.0.1
* GitHub: NA
* Source code: https://github.com/cran/ZipRadius
* Date/Publication: 2018-08-14 12:10:07 UTC
* Number of recursive dependencies: 64

Run `cloud_details(, "ZipRadius")` for more info

</details>

## Newly broken

*   checking whether package ‘ZipRadius’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/ZipRadius/new/ZipRadius.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘ggplot2’ ‘testthat’
      All declared Imports should be used.
    ```

## Installation

### Devel

```
* installing *source* package ‘ZipRadius’ ...
** package ‘ZipRadius’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘data_frame’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘ZipRadius’
* removing ‘/tmp/workdir/ZipRadius/new/ZipRadius.Rcheck/ZipRadius’


```
### CRAN

```
* installing *source* package ‘ZipRadius’ ...
** package ‘ZipRadius’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (ZipRadius)


```
