# brolgar

<details>

* Version: 0.1.2
* GitHub: https://github.com/njtierney/brolgar
* Source code: https://github.com/cran/brolgar
* Date/Publication: 2021-08-25 12:50:18 UTC
* Number of recursive dependencies: 113

Run `cloud_details(, "brolgar")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       20.           └─vctrs:::stop_lossy_cast(...)
       21.             └─vctrs::stop_incompatible_cast(...)
       22.               └─vctrs::stop_incompatible_type(...)
       23.                 └─vctrs:::stop_incompatible(...)
       24.                   └─vctrs:::stop_vctrs(...)
       25.                     └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
      
      [ FAIL 1 | WARN 0 | SKIP 5 | PASS 270 ]
      Deleting unused snapshots:
      • facet-sample/gg-facet-sample-alt.svg
      • facet-sample/gg-facet-sample.svg
      • facet-strata/gg-facet-strata-along.svg
      • facet-strata/gg-facet-strata.svg
      Error: Test failures
      Execution halted
    ```

# cubble

<details>

* Version: 0.1.0
* GitHub: https://github.com/huizezhang-sherry/cubble
* Source code: https://github.com/cran/cubble
* Date/Publication: 2022-04-22 09:20:05 UTC
* Number of recursive dependencies: 144

Run `cloud_details(, "cubble")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘aggregation.Rmd’ using rmarkdown
    `summarise()` has grouped output by 'id'. You can override using the `.groups`
    argument.
    `summarise()` has grouped output by 'cluster', 'id'. You can override using the
    `.groups` argument.
    `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'
    Adding missing grouping variables: `id`
    Warning: The concaveman package is required for geom_mark_hull
    --- finished re-building ‘aggregation.Rmd’
    ...
    ℹ The error occurred in row 1.
    Caused by error in `between()`:
    ! Can't convert `left` <double> to <character>.
    --- failed re-building ‘matching.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘cubble.Rmd’ ‘matching.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# gmgm

<details>

* Version: 1.1.0
* GitHub: NA
* Source code: https://github.com/cran/gmgm
* Date/Publication: 2021-09-02 17:00:02 UTC
* Number of recursive dependencies: 60

Run `cloud_details(, "gmgm")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       21.         └─vctrs:::vec_cast.integer.double(...)
       22.           └─vctrs::maybe_lossy_cast(...)
       23.             ├─base::withRestarts(...)
       24.             │ └─base withOneRestart(expr, restarts[[1L]])
       25.             │   └─base doWithOneRestart(return(expr), restart)
       26.             └─vctrs:::stop_lossy_cast(...)
       27.               └─vctrs::stop_incompatible_cast(...)
       28.                 └─vctrs::stop_incompatible_type(...)
       29.                   └─vctrs:::stop_incompatible(...)
       30.                     └─vctrs:::stop_vctrs(...)
       31.                       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
      
      [ FAIL 57 | WARN 0 | SKIP 0 | PASS 353 ]
      Error: Test failures
      Execution halted
    ```

# helda

<details>

* Version: 1.1.5
* GitHub: https://github.com/Redcart/helda
* Source code: https://github.com/cran/helda
* Date/Publication: 2021-01-06 11:00:16 UTC
* Number of recursive dependencies: 103

Run `cloud_details(, "helda")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘helda-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: kmeans_procedure
    > ### Title: K-means procedure
    > ### Aliases: kmeans_procedure
    > ### Keywords: cluster kmeans sizes
    > 
    > ### ** Examples
    > 
    ...
      2.   └─dplyr::between(...)
      3.     └─vctrs::vec_cast_common(!!!args, .to = x)
      4.       └─vctrs `<fn>`()
      5.         └─vctrs::vec_default_cast(...)
      6.           └─vctrs::stop_incompatible_cast(...)
      7.             └─vctrs::stop_incompatible_type(...)
      8.               └─vctrs:::stop_incompatible(...)
      9.                 └─vctrs:::stop_vctrs(...)
     10.                   └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked UTF-8 string
    ```

# hgnc

<details>

* Version: 0.1.0
* GitHub: https://github.com/maialab/hgnc
* Source code: https://github.com/cran/hgnc
* Date/Publication: 2022-03-21 19:00:02 UTC
* Number of recursive dependencies: 52

Run `cloud_details(, "hgnc")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘hgnc-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: list_archives
    > ### Title: List monthly and quarterly archives
    > ### Aliases: list_archives
    > 
    > ### ** Examples
    > 
    > list_archives()
    Error in open.connection(x, "rb") : HTTP error 502.
    Calls: list_archives ... <Anonymous> -> <Anonymous> -> <Anonymous> -> read_html.default
    Execution halted
    ```

# imfr

<details>

* Version: 0.1.9.1
* GitHub: https://github.com/christophergandrud/imfr
* Source code: https://github.com/cran/imfr
* Date/Publication: 2020-10-03 06:20:02 UTC
* Number of recursive dependencies: 41

Run `cloud_details(, "imfr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘imfr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: imf_codes
    > ### Title: Retrieve individual database codes
    > ### Aliases: imf_codes
    > 
    > ### ** Examples
    > 
    > # Retrieve indicators from BOP database
    ...
    Downloading: 150 kB     
    Downloading: 150 kB     
    Downloading: 150 kB     
    Downloading: 150 kB     
    Downloading: 150 kB     
    Downloading: 150 kB     
    Downloading: 150 kB     
    Downloading: 150 kB     
    Downloading: 150 kB     Error: data.imf.org appears to be down.
    Execution halted
    ```

## In both

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── Error (test-next.R:11:1): (code run outside of `test_that()`) ───────────────
      Error: data.imf.org appears to be down.
      Backtrace:
          ▆
       1. ├─testthat::expect_equal(...) at test-next.R:11:0
       2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
       3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
       4. ├─base::ncol(...)
       5. └─imfr::imf_data(...)
       6.   └─imfr:::imf_data_one(...)
       7.     └─imfr:::download_parse(URL)
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

# inti

<details>

* Version: 0.5.5
* GitHub: https://github.com/flavjack/inti
* Source code: https://github.com/cran/inti
* Date/Publication: 2022-04-01 07:00:05 UTC
* Number of recursive dependencies: 148

Run `cloud_details(, "inti")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘apps.Rmd’ using rmarkdown
    Could not fetch https://flavjack.github.io/inti/img/tarpuy.png
    HttpExceptionRequest Request {
      host                 = "flavjack.github.io"
      port                 = 443
      secure               = True
      requestHeaders       = []
      path                 = "/inti/img/tarpuy.png"
      queryString          = ""
    ...
    --- finished re-building ‘tarpuy.Rmd’
    
    --- re-building ‘yupana.Rmd’ using rmarkdown
    --- finished re-building ‘yupana.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘apps.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# presenter

<details>

* Version: 0.1.1
* GitHub: https://github.com/Harrison4192/presenter
* Source code: https://github.com/cran/presenter
* Date/Publication: 2021-11-18 06:20:05 UTC
* Number of recursive dependencies: 118

Run `cloud_details(, "presenter")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘exportToExcel.Rmd’ using rmarkdown
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    ...
    --- finished re-building ‘formattedFlextable.Rmd’
    
    --- re-building ‘pivotSummary.Rmd’ using rmarkdown
    --- finished re-building ‘pivotSummary.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘flextableAndPowerpoint.Rmd’ ‘flextableThemes.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘badger’
      All declared Imports should be used.
    ```

# respR

<details>

* Version: 2.0.2
* GitHub: https://github.com/januarharianto/respr
* Source code: https://github.com/cran/respR
* Date/Publication: 2022-03-23 15:50:02 UTC
* Number of recursive dependencies: 122

Run `cloud_details(, "respR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘respR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: calc_rate.ft
    > ### Title: Calculate rate of change in oxygen from flowthrough respirometry
    > ###   data
    > ### Aliases: calc_rate.ft
    > 
    > ### ** Examples
    > 
    ...
     15.                           ├─base::withRestarts(...)
     16.                           │ └─base withOneRestart(expr, restarts[[1L]])
     17.                           │   └─base doWithOneRestart(return(expr), restart)
     18.                           └─vctrs:::stop_lossy_cast(...)
     19.                             └─vctrs::stop_incompatible_cast(...)
     20.                               └─vctrs::stop_incompatible_type(...)
     21.                                 └─vctrs:::stop_incompatible(...)
     22.                                   └─vctrs:::stop_vctrs(...)
     23.                                     └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       12.                 └─vctrs:::vec_cast.integer.double(...)
       13.                   └─vctrs::maybe_lossy_cast(...)
       14.                     ├─base::withRestarts(...)
       15.                     │ └─base withOneRestart(expr, restarts[[1L]])
       16.                     │   └─base doWithOneRestart(return(expr), restart)
       17.                     └─vctrs:::stop_lossy_cast(...)
       18.                       └─vctrs::stop_incompatible_cast(...)
       19.                         └─vctrs::stop_incompatible_type(...)
       20.                           └─vctrs:::stop_incompatible(...)
       21.                             └─vctrs:::stop_vctrs(...)
       22.                               └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
      
      [ FAIL 10 | WARN 0 | SKIP 5 | PASS 4508 ]
      Error: Test failures
      Execution halted
    ```

