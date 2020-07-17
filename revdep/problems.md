# anglr

<details>

* Version: 0.6.0
* Source code: https://github.com/cran/anglr
* URL: https://github.com/hypertidy/anglr
* BugReports: https://github.com/hypertidy/anglr/issues
* Date/Publication: 2020-05-13 23:40:12 UTC
* Number of recursive dependencies: 121

Run `cloud_details(, "anglr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(anglr)
      Warning messages:
      1: In rgl.init(initValue, onlyNULL) : RGL: unable to open X11 display
      2: 'rgl.init' failed, running with 'rgl.useNULL = TRUE'. 
      > 
      > test_check("anglr")
      ── 1. Failure: SC0 round trip suite works (@test-silicate-sanity.R#7)  ─────────
      `{ ... }` produced warnings.
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 80 | SKIPPED: 1 | WARNINGS: 1 | FAILED: 1 ]
      1. Failure: SC0 round trip suite works (@test-silicate-sanity.R#7) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking whether package ‘anglr’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: 'rgl.init' failed, running with 'rgl.useNULL = TRUE'.
    See ‘/tmp/workdir/anglr/new/anglr.Rcheck/00install.out’ for details.
    ```

# dodgr

<details>

* Version: 0.2.7
* Source code: https://github.com/cran/dodgr
* URL: https://github.com/ATFutures/dodgr
* BugReports: https://github.com/ATFutures/dodgr/issues
* Date/Publication: 2020-05-05 22:20:02 UTC
* Number of recursive dependencies: 92

Run `cloud_details(, "dodgr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘dodgr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: dodgr_contract_graph
    > ### Title: dodgr_contract_graph
    > ### Aliases: dodgr_contract_graph
    > 
    > ### ** Examples
    > 
    > graph <- weight_streetnet (hampi)
    > nrow (graph) # 5,973
    [1] 5973
    > graph <- dodgr_contract_graph (graph)
    Error in readRDS(fname_c) : error reading from connection
    Calls: dodgr_contract_graph -> readRDS
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 18.1Mb
      sub-directories of 1Mb or more:
        doc    5.2Mb
        libs  12.4Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘RcppThread’
      All declared Imports should be used.
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# florestal

<details>

* Version: 0.1.1
* Source code: https://github.com/cran/florestal
* Date/Publication: 2020-07-07 08:40:02 UTC
* Number of recursive dependencies: 156

Run `cloud_details(, "florestal")` for more info

</details>

## Newly broken

*   checking whether package ‘florestal’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: 'rgl.init' failed, running with 'rgl.useNULL = TRUE'.
    See ‘/tmp/workdir/florestal/new/florestal.Rcheck/00install.out’ for details.
    ```

# gMOIP

<details>

* Version: 1.4.3
* Source code: https://github.com/cran/gMOIP
* URL: https://github.com/relund/gMOIP/
* BugReports: https://github.com/relund/gMOIP/issues
* Date/Publication: 2020-02-20 15:10:02 UTC
* Number of recursive dependencies: 99

Run `cloud_details(, "gMOIP")` for more info

</details>

## Newly broken

*   checking whether package ‘gMOIP’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: 'rgl.init' failed, running with 'rgl.useNULL = TRUE'.
    See ‘/tmp/workdir/gMOIP/new/gMOIP.Rcheck/00install.out’ for details.
    ```

# keyATM

<details>

* Version: 0.3.0
* Source code: https://github.com/cran/keyATM
* URL: https://keyatm.github.io/keyATM/
* BugReports: https://github.com/keyATM/keyATM/issues
* Date/Publication: 2020-07-12 22:50:02 UTC
* Number of recursive dependencies: 108

Run `cloud_details(, "keyATM")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       2. keyATM:::keyATM_output(fitted, keep)
       3. keyATM:::keyATM_output_theta(model, info)
       6. vctrs::vec_rbind(!!!dots, .names_to = .id)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 17 | SKIPPED: 9 | WARNINGS: 0 | FAILED: 6 ]
      1. Error: (unknown) (@test-keyATMBase.R#16) 
      2. Error: (unknown) (@test-keyATMCov.R#17) 
      3. Error: (unknown) (@test-keyATMDynamic.R#17) 
      4. Error: (unknown) (@test-keyATMHeterogeneity.R#51) 
      5. Error: (unknown) (@test-keyATMvb.R#14) 
      6. Error: (unknown) (@test-plot.R#16) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 16.5Mb
      sub-directories of 1Mb or more:
        libs  16.1Mb
    ```

# tibbletime

<details>

* Version: 0.1.5
* Source code: https://github.com/cran/tibbletime
* URL: https://github.com/business-science/tibbletime
* BugReports: https://github.com/business-science/tibbletime/issues
* Date/Publication: 2020-06-18 19:00:02 UTC
* Number of recursive dependencies: 59

Run `cloud_details(, "tibbletime")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > FB_summarised %>%
    +   dplyr::filter(!is.na(summary_roll)) %>%
    +   tidyr::unnest(summary_roll)
    Error: Internal error in `vec_proxy_assign_opts()`: `proxy` of type `integer` incompatible with `value` proxy of type `double`.
    Backtrace:
         █
      1. └─FB_summarised %>% dplyr::filter(!is.na(summary_roll)) %>% tidyr::unnest(summary_roll)
      2.   ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      3.   └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      4.     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5.       └─`_fseq`(`_lhs`)
      6.         └─magrittr::freduce(value, `_function_list`)
      7.           ├─base::withVisible(function_list[[k]](value))
      8.           └─function_list[[k]](value)
      9.             ├─tidyr::unnest(., summary_roll)
     10.             ├─tibbletime:::unnest.tbl_df(., summary_roll)
     11.             ├─base::NextMethod()
     12.             └─tidyr:::unnest.data.frame(., summary_roll)
     13.               └─tidyr::unchop(data, any_of(cols), keep_empty = keep_empty, ptype = ptype)
     14.                 └─ti
    Execution halted
    ```

# timetk

<details>

* Version: 2.1.0
* Source code: https://github.com/cran/timetk
* URL: https://github.com/business-science/timetk
* BugReports: https://github.com/business-science/timetk/issues
* Date/Publication: 2020-07-03 11:40:02 UTC
* Number of recursive dependencies: 183

Run `cloud_details(, "timetk")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
     2 FB     2013-01-03     27.8           NA              NA  
     3 FB     2013-01-04     28.8           NA              NA  
     4 FB     2013-01-07     29.4           NA              NA  
     5 FB     2013-01-08     29.1           28.6            NA  
     6 FB     2013-01-09     30.6           29.1            NA  
     7 FB     2013-01-10     31.3           29.8            NA  
     8 FB     2013-01-11     31.7           30.4            NA  
     9 FB     2013-01-14     31.0           30.7            NA  
    10 FB     2013-01-15     30.1           30.9            29.8
    # … with 998 more rows
    > 
    > # For summary operations like rolling means, we can accomplish large-scale
    > # multi-rolls with tk_augment_slidify()
    > 
    > FB %>%
    +     select(symbol, date, adjusted) %>%
    +     tk_augment_slidify(
    +         adjusted, .period = 5:10, .f = mean, .align = "right",
    +         .names = str_c("MA_", 5:10)
    +     )
    New names:
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.4Mb
      sub-directories of 1Mb or more:
        doc    3.6Mb
        help   1.2Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2750 marked UTF-8 strings
    ```

# unpivotr

<details>

* Version: 0.6.0
* Source code: https://github.com/cran/unpivotr
* URL: https://github.com/nacnudus/unpivotr
* BugReports: https://github.com/nacnudus/unpivotr/issues
* Date/Publication: 2020-05-08 17:50:02 UTC
* Number of recursive dependencies: 77

Run `cloud_details(, "unpivotr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(unpivotr)
      > 
      > test_check("unpivotr")
      ── 1. Failure: unpack() works on common data types (@test-pack.R#53)  ──────────
      Not all data returned
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 256 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Failure: unpack() works on common data types (@test-pack.R#53) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

