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

# functiondepends

<details>

* Version: 0.2.1
* GitHub: NA
* Source code: https://github.com/cran/functiondepends
* Date/Publication: 2020-11-03 12:30:02 UTC
* Number of recursive dependencies: 74

Run `cloud_details(, "functiondepends")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘functiondepends-usage.Rmd’ using rmarkdown
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    ...
    Error: processing vignette 'functiondepends-usage.Rmd' failed with diagnostics:
    Problem while computing `..1 = !is.na(.)`.
    ✖ Input `..1` must be a logical vector, not a logical[,2].
    --- failed re-building ‘functiondepends-usage.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘functiondepends-usage.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# mcp

<details>

* Version: 0.3.1
* GitHub: https://github.com/lindeloev/mcp
* Source code: https://github.com/cran/mcp
* Date/Publication: 2021-11-17 16:50:02 UTC
* Number of recursive dependencies: 102

Run `cloud_details(, "mcp")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── Failure (helper-runs.R:325:7): good_poisson:
          y ~ 1 + ar(1), ~1 + x + ar(2, 1 + x + I(x^3)) ──
      stringr::str_starts(error_message, expected_error_poisson) is not TRUE
      
      `actual`:   FALSE
      `expected`: TRUE 
      Backtrace:
          ▆
       1. └─mcp:::test_runs(model, ...) at tests/testthat/helper-runs.R:325:6
       2.   └─mcp:::test_pp_eval(fit) at tests/testthat/helper-runs.R:113:6
       3.     └─testthat::expect_true(stringr::str_starts(error_message, expected_error_poisson)) at tests/testthat/helper-runs.R:293:6
      
      [ FAIL 7 | WARN 1 | SKIP 6 | PASS 3619 ]
      Error: Test failures
      Execution halted
    ```

# nomisr

<details>

* Version: 0.4.4
* GitHub: https://github.com/ropensci/nomisr
* Source code: https://github.com/cran/nomisr
* Date/Publication: 2021-01-23 17:20:02 UTC
* Number of recursive dependencies: 87

Run `cloud_details(, "nomisr")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Introduction-to-work-and-health-nomis-indicators.Rmd’ using rmarkdown
    Quitting from lines 194-365 (Introduction-to-work-and-health-nomis-indicators.Rmd) 
    Error: processing vignette 'Introduction-to-work-and-health-nomis-indicators.Rmd' failed with diagnostics:
    `desc()` must be called with exactly one argument.
    --- failed re-building ‘Introduction-to-work-and-health-nomis-indicators.Rmd’
    
    --- re-building ‘introduction.Rmd’ using rmarkdown
    --- finished re-building ‘introduction.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Introduction-to-work-and-health-nomis-indicators.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
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

# prider

<details>

* Version: 1.0.2
* GitHub: NA
* Source code: https://github.com/cran/prider
* Date/Publication: 2021-09-13 07:30:02 UTC
* Number of recursive dependencies: 29

Run `cloud_details(, "prider")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘prider-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: prider
    > ### Title: Prider
    > ### Aliases: prider print.prider plot.prider
    > 
    > ### ** Examples
    > 
    > test_fasta <- system.file('extdata', 'test.fasta', package = 'prider')
    ...
     18.   └─base::eval.parent(substitute(eval(quote(expr), envir)))
     19.     └─base::eval(expr, p)
     20.       └─base::eval(expr, p)
     21.         └─base::eval(...)
     22.           └─base::eval(...)
     23.             ├─dplyr:::fix_call(...)
     24.             │ └─base::withCallingHandlers(...)
     25.             └─vctrs::vec_assert(order_by, size = n, arg = "order_by")
     26.               └─rlang::abort(...)
    Execution halted
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

# salem

<details>

* Version: 0.2.0
* GitHub: NA
* Source code: https://github.com/cran/salem
* Date/Publication: 2020-11-05 16:40:05 UTC
* Number of recursive dependencies: 62

Run `cloud_details(, "salem")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘introduction.Rmd’ using rmarkdown
    --- finished re-building ‘introduction.Rmd’
    
    --- re-building ‘recreating_analyses.Rmd’ using rmarkdown
    Joining, by = "Month"
    Quitting from lines 348-367 (recreating_analyses.Rmd) 
    Error: processing vignette 'recreating_analyses.Rmd' failed with diagnostics:
    Can't convert NULL to a symbol.
    --- failed re-building ‘recreating_analyses.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘recreating_analyses.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# sfnetworks

<details>

* Version: 0.5.4
* GitHub: https://github.com/luukvdmeer/sfnetworks
* Source code: https://github.com/cran/sfnetworks
* Date/Publication: 2021-12-17 09:00:02 UTC
* Number of recursive dependencies: 139

Run `cloud_details(, "sfnetworks")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘sfn01_structure.Rmd’ using rmarkdown
    Linking to GEOS 3.8.0, GDAL 3.0.4, PROJ 6.3.1; sf_use_s2() is TRUE
    
    Attaching package: 'tidygraph'
    
    The following object is masked from 'package:stats':
    
        filter
    
    ...
    Quitting from lines 190-198 (sfn05_morphers.Rmd) 
    Error: processing vignette 'sfn05_morphers.Rmd' failed with diagnostics:
    [1m[22m`desc()` must be called with exactly one argument.
    --- failed re-building ‘sfn05_morphers.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘sfn05_morphers.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# telemac

<details>

* Version: 0.1.0
* GitHub: https://github.com/tpilz/telemac
* Source code: https://github.com/cran/telemac
* Date/Publication: 2021-02-19 10:10:05 UTC
* Number of recursive dependencies: 123

Run `cloud_details(, "telemac")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘t2d_basics.Rmd’ using rmarkdown
    Loading required package: sp
    Linking to GEOS 3.8.0, GDAL 3.0.4, PROJ 6.3.1; sf_use_s2() is TRUE
    ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──
    ✔ ggplot2 3.3.5          ✔ purrr   0.3.4     
    ✔ tibble  3.1.6          ✔ dplyr   1.0.7.9000
    ✔ tidyr   1.2.0          ✔ stringr 1.4.0     
    ✔ readr   2.1.2          ✔ forcats 0.5.1     
    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ...
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    Linking to GEOS 3.8.0, GDAL 3.0.4, PROJ 6.3.1; sf_use_s2() is TRUE
    --- finished re-building ‘t2d_rainfall_runoff.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘t2d_basics.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.3Mb
      sub-directories of 1Mb or more:
        doc       3.3Mb
        libs      3.4Mb
        telemac   1.0Mb
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

