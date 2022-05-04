# comperes

<details>

* Version: 0.2.5
* GitHub: https://github.com/echasnovski/comperes
* Source code: https://github.com/cran/comperes
* Date/Publication: 2020-11-23 21:20:02 UTC
* Number of recursive dependencies: 62

Run `cloud_details(, "comperes")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(comperes)
      > 
      > test_check("comperes")
      [ FAIL 1 | WARN 16 | SKIP 5 | PASS 256 ]
      
      ══ Skipped tests ═══════════════════════════════════════════════════════════════
      • On CRAN (5)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test-head-to-head.R:168:3): h2h_mat allows multiple Head-to-Head functions ──
      `h2h_mat(cr_data)` produced warnings.
      
      [ FAIL 1 | WARN 16 | SKIP 5 | PASS 256 ]
      Error: Test failures
      Execution halted
    ```

# dodgr

<details>

* Version: 0.2.13
* GitHub: https://github.com/ATFutures/dodgr
* Source code: https://github.com/cran/dodgr
* Date/Publication: 2022-04-01 23:50:02 UTC
* Number of recursive dependencies: 107

Run `cloud_details(, "dodgr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘sc-conversion-fns.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > 
      > test_check("dodgr")
      [ FAIL 1 | WARN 0 | SKIP 10 | PASS 201 ]
      
      ══ Skipped tests ═══════════════════════════════════════════════════════════════
      • !test_all is TRUE (1)
      • On CRAN (9)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test-iso.R:16:15): isodists ────────────────────────────────────────
      `net <- weight_streetnet(hsc, wt_profile = "bicycle")` produced warnings.
      
      [ FAIL 1 | WARN 0 | SKIP 10 | PASS 201 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 31.1Mb
      sub-directories of 1Mb or more:
        doc    5.2Mb
        libs  25.3Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# exuber

<details>

* Version: 0.4.2
* GitHub: https://github.com/kvasilopoulos/exuber
* Source code: https://github.com/cran/exuber
* Date/Publication: 2020-12-18 07:30:19 UTC
* Number of recursive dependencies: 98

Run `cloud_details(, "exuber")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       32. │           └─base withOneRestart(expr, restarts[[1L]])
       33. │             └─base doWithOneRestart(return(expr), restart)
       34. └─dplyr `<fn>`(`<vctrs___>`)
       35.   └─dplyr:::rethrow_warning_join_matches_multiple(cnd)
       36.     └─dplyr:::warn_join(...)
       37.       └─dplyr:::warn_dplyr(...)
       38.         └─rlang::warn(...)
       39.           └─base::warning(cnd)
       40.             └─base::withRestarts(...)
       41.               └─base withOneRestart(expr, restarts[[1L]])
       42.                 └─base doWithOneRestart(return(expr), restart)
      
      [ FAIL 42 | WARN 50 | SKIP 4 | PASS 155 ]
      Error: Test failures
      Execution halted
    ```

# lans2r

<details>

* Version: 1.1.0
* GitHub: https://github.com/KopfLab/lans2r
* Source code: https://github.com/cran/lans2r
* Date/Publication: 2020-06-24 05:20:03 UTC
* Number of recursive dependencies: 84

Run `cloud_details(, "lans2r")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      [ FAIL 4 | WARN 5 | SKIP 0 | PASS 140 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test-load-data.R:24:3): test that it is possible to load multiple LANS summaries ──
      `... <- NULL` produced warnings.
      ── Failure (test-load-data.R:43:3): test that it is possible to load multiple LANS summaries ──
      `... <- NULL` produced warnings.
      ── Failure (test-load-data.R:58:3): test that it is possible to load multiple LANS summaries ──
      `... <- NULL` produced warnings.
      ── Failure (test-load-data.R:81:3): test that it is possible to load LANS maps ──
      `... <- NULL` produced warnings.
      
      [ FAIL 4 | WARN 5 | SKIP 0 | PASS 140 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# MBNMAtime

<details>

* Version: 0.2.1
* GitHub: NA
* Source code: https://github.com/cran/MBNMAtime
* Date/Publication: 2021-09-13 15:10:02 UTC
* Number of recursive dependencies: 107

Run `cloud_details(, "MBNMAtime")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        1 
      [ FAIL 2 | WARN 0 | SKIP 13 | PASS 186 ]
      
      ══ Skipped tests ═══════════════════════════════════════════════════════════════
      • On CRAN (13)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test_plot.functions.R:350:3): timeplot functions correctly ─────────
      `timeplot(painnet, plotby = "rel")` produced warnings.
      ── Failure (test_plot.functions.R:357:3): timeplot functions correctly ─────────
      `timeplot(classnetwork, plotby = "rel", level = "class")` produced warnings.
      
      [ FAIL 2 | WARN 0 | SKIP 13 | PASS 186 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘gemtc’
    ```

# modeldb

<details>

* Version: 0.2.2
* GitHub: https://github.com/tidymodels/modeldb
* Source code: https://github.com/cran/modeldb
* Date/Publication: 2020-02-10 20:50:07 UTC
* Number of recursive dependencies: 93

Run `cloud_details(, "modeldb")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      > library(modeldb)
      > 
      > test_check("modeldb")
      [ FAIL 2 | WARN 1 | SKIP 0 | PASS 17 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test_kmeans.R:11:3): Not specifying variables works ────────────────
      `select(mtcars, wt, mpg) %>% simple_kmeans_db()` produced warnings.
      ── Failure (test_kmeans.R:23:3): Centroid argument is accepted ─────────────────
      `simple_kmeans_db(mtcars, mpg, wt, initial_kmeans = ik)` produced warnings.
      
      [ FAIL 2 | WARN 1 | SKIP 0 | PASS 17 ]
      Error: Test failures
      Execution halted
    ```

# multicolor

<details>

* Version: 0.1.5
* GitHub: https://github.com/aedobbyn/multicolor
* Source code: https://github.com/cran/multicolor
* Date/Publication: 2021-11-04 16:50:02 UTC
* Number of recursive dependencies: 67

Run `cloud_details(, "multicolor")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Colors cannot be applied in this environment. Please use another application, such as RStudio or a color-enabled terminal.
      > 
      > test_check("multicolor")
      [ FAIL 1 | WARN 1 | SKIP 1 | PASS 29 ]
      
      ══ Skipped tests ═══════════════════════════════════════════════════════════════
      • use_color() is not TRUE (1)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test-multicolor.R:103:3): colors(), including grays, rainbow, and rbg work ──
      `suppressMessages(...)` produced warnings.
      
      [ FAIL 1 | WARN 1 | SKIP 1 | PASS 29 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘cowsay’
      All declared Imports should be used.
    ```

# parsnip

<details>

* Version: 0.2.1
* GitHub: https://github.com/tidymodels/parsnip
* Source code: https://github.com/cran/parsnip
* Date/Publication: 2022-03-17 11:40:02 UTC
* Number of recursive dependencies: 132

Run `cloud_details(, "parsnip")` for more info

</details>

## Newly broken

*   checking Rd files ... WARNING
    ```
    prepare_Rd: Each row in `x` can match at most 1 row in `y`.
    ℹ Row 1 of `x` matches multiple rows.
    prepare_Rd: Each row in `x` can match at most 1 row in `y`.
    ℹ Row 1 of `x` matches multiple rows.
    prepare_Rd: Each row in `x` can match at most 1 row in `y`.
    ℹ Row 1 of `x` matches multiple rows.
    prepare_Rd: Each row in `x` can match at most 1 row in `y`.
    ℹ Row 1 of `x` matches multiple rows.
    prepare_Rd: Each row in `x` can match at most 1 row in `y`.
    ℹ Row 1 of `x` matches multiple rows.
    ...
    prepare_Rd: Each row in `x` can match at most 1 row in `y`.
    ℹ Row 1 of `x` matches multiple rows.
    prepare_Rd: Each row in `x` can match at most 1 row in `y`.
    ℹ Row 1 of `x` matches multiple rows.
    prepare_Rd: Each row in `x` can match at most 1 row in `y`.
    ℹ Row 1 of `x` matches multiple rows.
    prepare_Rd: Each row in `x` can match at most 1 row in `y`.
    ℹ Row 1 of `x` matches multiple rows.
    prepare_Rd: Each row in `x` can match at most 1 row in `y`.
    ℹ Row 1 of `x` matches multiple rows.
    ```

*   checking for unstated dependencies in examples ... WARNING
    ```
    Warning: Each row in `x` can match at most 1 row in `y`.
    ℹ Row 1 of `x` matches multiple rows.
    Warning: Each row in `x` can match at most 1 row in `y`.
    ℹ Row 1 of `x` matches multiple rows.
    Warning: Each row in `x` can match at most 1 row in `y`.
    ℹ Row 1 of `x` matches multiple rows.
    Warning: Each row in `x` can match at most 1 row in `y`.
    ℹ Row 1 of `x` matches multiple rows.
    Warning: Each row in `x` can match at most 1 row in `y`.
    ℹ Row 1 of `x` matches multiple rows.
    ...
    Warning: Each row in `x` can match at most 1 row in `y`.
    ℹ Row 1 of `x` matches multiple rows.
    Warning: Each row in `x` can match at most 1 row in `y`.
    ℹ Row 1 of `x` matches multiple rows.
    Warning: Each row in `x` can match at most 1 row in `y`.
    ℹ Row 1 of `x` matches multiple rows.
    Warning: Each row in `x` can match at most 1 row in `y`.
    ℹ Row 1 of `x` matches multiple rows.
    Warning: Each row in `x` can match at most 1 row in `y`.
    ℹ Row 1 of `x` matches multiple rows.
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘rules’, ‘baguette’, ‘ipred’, ‘dbarts’, ‘mboost’, ‘party’, ‘mda’, ‘sda’, ‘sparsediscrim’, ‘klaR’, ‘brulee’, ‘glmnet’, ‘rstan’, ‘rstanarm’, ‘naivebayes’, ‘plsmod’, ‘mixOmics’, ‘pscl’, ‘workflows’, ‘randomForest’, ‘xrf’, ‘flexsurv’, ‘broom’
    ```

# sfnetworks

<details>

* Version: 0.5.5
* GitHub: https://github.com/luukvdmeer/sfnetworks
* Source code: https://github.com/cran/sfnetworks
* Date/Publication: 2022-02-16 18:50:02 UTC
* Number of recursive dependencies: 142

Run `cloud_details(, "sfnetworks")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘sfnetworks-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: st_network_join
    > ### Title: Join two spatial networks based on equality of node geometries
    > ### Aliases: st_network_join
    > 
    > ### ** Examples
    > 
    > library(sf, quietly = TRUE)
    ...
    > edge1 = st_sfc(st_linestring(c(node1, node2)))
    > edge2 = st_sfc(st_linestring(c(node2, node3)))
    > edge3 = st_sfc(st_linestring(c(node3, node4)))
    > 
    > net1 = as_sfnetwork(c(edge1, edge2))
    > net2 = as_sfnetwork(c(edge2, edge3))
    > 
    > joined = st_network_join(net1, net2)
    Error: This type is not supported by `vec_order()`.
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        4. ├─sfnetworks::st_network_join(net, rdm_net)
        5. └─sfnetworks:::st_network_join.sfnetwork(net, rdm_net)
        6.   └─sfnetworks:::spatial_join_network(x, y, ...)
        7.     └─tidygraph::graph_join(...)
        8.       ├─dplyr::full_join(...)
        9.       └─dplyr:::full_join.data.frame(...)
       10.         └─dplyr:::join_mutate(...)
       11.           └─dplyr:::join_rows(...)
       12.             └─dplyr:::dplyr_locate_matches(...)
       13.               ├─base::withCallingHandlers(...)
       14.               └─vctrs::vec_locate_matches(...)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 241 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘sfn01_structure.Rmd’ using rmarkdown
    Linking to GEOS 3.8.0, GDAL 3.0.4, PROJ 6.3.1; sf_use_s2() is TRUE
    
    Attaching package: 'tidygraph'
    
    The following object is masked from 'package:stats':
    
        filter
    
    ...
    
        union
    
    --- finished re-building ‘sfn05_morphers.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘sfn03_join_filter.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# stars

<details>

* Version: 0.5-5
* GitHub: https://github.com/r-spatial/stars
* Source code: https://github.com/cran/stars
* Date/Publication: 2021-12-19 03:20:02 UTC
* Number of recursive dependencies: 148

Run `cloud_details(, "stars")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘aggregate.R’
      Comparing ‘aggregate.Rout’ to ‘aggregate.Rout.save’ ...4c4
    < Linking to GEOS 3.8.0, GDAL 3.0.4, PROJ 6.3.1; sf_use_s2() is TRUE
    ---
    > Linking to GEOS 3.9.0, GDAL 3.2.1, PROJ 7.2.1
    57c57
    < file1d84474128c6.tif    33  199.25  365.5 365.5  531.75  698
    ---
    > file46ef83309cd.tif    33  199.25  365.5 365.5  531.75  698
      Running ‘area.R’
    ...
      warn\[1\] does not match "Non-canonical axis order found, attempting to correct.".
      Actual value: "Each row in `x` can match at most 1 row in `y`\.\\nℹ Row 2 of `x` matches multiple rows\."
      Backtrace:
          ▆
       1. └─testthat::expect_match(warn[1], "Non-canonical axis order found, attempting to correct.") at test_ncdf.R:123:2
       2.   └─testthat:::expect_match_(...)
      
      [ FAIL 2 | WARN 42 | SKIP 0 | PASS 98 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.8Mb
      sub-directories of 1Mb or more:
        doc   2.3Mb
        nc    4.5Mb
    ```

# Tplyr

<details>

* Version: 0.4.4
* GitHub: https://github.com/atorus-research/Tplyr
* Source code: https://github.com/cran/Tplyr
* Date/Publication: 2022-01-27 16:00:02 UTC
* Number of recursive dependencies: 117

Run `cloud_details(, "Tplyr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      i Use `spec()` to retrieve the full column specification for this data.
      i Specify the column types or set `show_col_types = FALSE` to quiet this message.
      [ FAIL 1 | WARN 0 | SKIP 52 | PASS 554 ]
      
      ══ Skipped tests ═══════════════════════════════════════════════════════════════
      • On CRAN (52)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test-count.R:197:3): Count layers are summarized without errors and warnings ──
      `build(t19)` produced warnings.
      
      [ FAIL 1 | WARN 0 | SKIP 52 | PASS 554 ]
      Error: Test failures
      Execution halted
    ```

