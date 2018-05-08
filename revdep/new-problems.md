# ddpcr

Version: 1.8

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      16: freduce(value, `_function_list`) at /tmp/RtmphXLGih/R.INSTALL1cc01402b8439/magrittr/R/pipe.R:28
      17: function_list[[i]](value) at /tmp/RtmphXLGih/R.INSTALL1cc01402b8439/magrittr/R/freduce.R:17
      18: init_data(.)
      19: dplyr::select_(new_plate_data, "well", x_var, y_var)
      20: select_.data.frame(new_plate_data, "well", x_var, y_var)
      21: select(.data, !(!(!dots)))
      22: select.data.frame(.data, !(!(!dots)))
      23: select_impl(.data, vars)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 267 SKIPPED: 0 FAILED: 1
      1. Error: reset works (@test-plate.R#14) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Warning in engine$weave(file, quiet = quiet, encoding = enc) :
      Pandoc (>= 1.12.3) and/or pandoc-citeproc not available. Falling back to R Markdown v1.
    Warning in engine$weave(file, quiet = quiet, encoding = enc) :
      Pandoc (>= 1.12.3) and/or pandoc-citeproc not available. Falling back to R Markdown v1.
    Reading data files into plate... DONE (0 seconds)
    Initializing plate of type `custom_thresholds`... DONE (0 seconds)
    Identifying outlier droplets... DONE (0 seconds)
    Classifying droplets... DONE (0 seconds)
    Analysis complete
    Reading data files into plate... DONE (0 seconds)
    Initializing plate of type `fam_positive_pnpp`... DONE (0 seconds)
    Identifying failed wells... DONE (0 seconds)
    Identifying outlier droplets... DONE (0 seconds)
    Identifying empty droplets... DONE (1 seconds)
    Classifying droplets... DONE (1 seconds)
    Reclassifying droplets... skipped (not enough wells with significant mutant clusters)
    Analysis complete
    Initializing plate of type `custom_thresholds`... Quitting from lines 348-357 (overview.Rmd) 
    Error: processing vignette 'overview.Rmd' failed with diagnostics:
    Column `4` cannot have NA as name
    Execution halted
    ```

# desctable

Version: 0.1.1

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    +                                 mpg = "Miles per gallon"))
                         N      Mean        sd     Med       IQR
    1  Miles per gallon 32 20.090625 6.0269481      NA        NA
    2         Cylinders 32        NA        NA   6.000   4.00000
    3              disp 32        NA        NA 196.300 205.17500
    4       Horse Power 32        NA        NA 123.000  83.50000
    5              drat 32  3.596563 0.5346787      NA        NA
    6                wt 32        NA        NA   3.325   1.02875
    7              qsec 32 17.848750 1.7869432      NA        NA
    8                vs 32        NA        NA   0.000   1.00000
    9                am 32        NA        NA   0.000   1.00000
    10             gear 32        NA        NA   4.000   1.00000
    11             carb 32        NA        NA   2.000   2.00000
    > 
    > # With grouping on a factor
    > iris %>%
    +   group_by(Species) %>%
    +   desctable(stats = stats_default)
    Error in eval(grps[[1]]) : object 'Species' not found
    Calls: %>% ... vars_select_eval -> map_if -> map -> .Call -> .f -> eval -> eval
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Warning in engine$weave(file, quiet = quiet, encoding = enc) :
      Pandoc (>= 1.12.3) and/or pandoc-citeproc not available. Falling back to R Markdown v1.
    
    Attaching package: 'desctable'
    
    The following object is masked from 'package:DT':
    
        datatable
    
    The following objects are masked from 'package:stats':
    
        IQR, chisq.test, fisher.test
    
    Quitting from lines 217-222 (desctable.Rmd) 
    Error: processing vignette 'desctable.Rmd' failed with diagnostics:
    object 'Species' not found
    Execution halted
    ```

# FedData

Version: 2.5.2

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Got a 530 ftp-server response when 220 was expected
      1: readLines(curl::curl(url, handle = hand)) at testthat/test.ITRDB.R:23
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 26 SKIPPED: 0 FAILED: 1
      1. Error: ITRDB version files are available (@test.ITRDB.R#23) 
      
      Error: testthat unit tests failed
      In addition: Warning messages:
      1: closing unused connection 4 (ftp://ftp.ncdc.noaa.gov/pub/data/paleo/treering/chronologies/) 
      2: In cb$setcurexpr(call) :
        closing unused connection 4 (http://websoilsurvey.sc.egov.usda.gov/DSD/Download/Cache/SSA/blah.zip)
      3: In cb$setcurexpr(call) :
        closing unused connection 3 (https://websoilsurvey.sc.egov.usda.gov/DSD/Download/Cache/SSA/wss_SSA_CO670_[2017-09-06].zip)
      Execution halted
    ```

# GetITRData

Version: 0.7

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Warning in engine$weave(file, quiet = quiet, encoding = enc) :
      Pandoc (>= 1.12.3) and/or pandoc-citeproc not available. Falling back to R Markdown v1.
    Quitting from lines 99-112 (gitrd-vignette-introduction.Rmd) 
    Error: processing vignette 'gitrd-vignette-introduction.Rmd' failed with diagnostics:
    Zipped file contains 0 files. This is likelly a problem with the downloaded file. Try running the code again as the corrupted zip file was deleted and will be downloaded again.
    
    If the problem persists, my suggestions is to remove the time period with problem.
    Execution halted
    ```

# ggmap

Version: 2.6.1

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggmap-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: get_map
    > ### Title: Grab a map.
    > ### Aliases: get_map
    > 
    > ### ** Examples
    > 
    > map <- get_map()
    Warning in download.file(url, destfile = tmp, quiet = !messaging, mode = "wb") :
      cannot open URL 'http://maps.googleapis.com/maps/api/staticmap?center=29.763284,-95.363271&zoom=10&size=640x640&scale=2&maptype=terrain&language=en-EN&sensor=false': HTTP status was '403 Forbidden'
    Error in download.file(url, destfile = tmp, quiet = !messaging, mode = "wb") : 
      cannot open URL 'http://maps.googleapis.com/maps/api/staticmap?center=29.763284,-95.363271&zoom=10&size=640x640&scale=2&maptype=terrain&language=en-EN&sensor=false'
    Calls: get_map -> get_googlemap -> download.file
    Execution halted
    ```

# keyholder

Version: 0.1.1

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      16: distinct_vars(.data, quos(...), .keep_all = .keep_all)
      17: list_cols_warning(.data, out_vars)
      18: df[keep_cols]
      19: `[.keyed_df`(df, keep_cols)
      20: `keys<-`(`*tmp*`, value = structure(list(vs = NA_real_, am = NA_real_), .Names = c("vs", 
         "am"), row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")))
      21: stop("Keys object should have the same number of rows as data.")
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 306 SKIPPED: 0 FAILED: 2
      1. Error: group_by_if works (@test-keyed-df-one-tbl.R#268) 
      2. Error: distinct works (@test-keyed-df-one-tbl.R#298) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# MonetDBLite

Version: 0.5.1

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    The following objects are masked from ‘package:stats’:
    
        filter, lag
    
    The following objects are masked from ‘package:base’:
    
        intersect, setdiff, setequal, union
    
    > # To connect to a database first create a src:
    > dbdir <- file.path(tempdir(), "dplyrdir")
    > my_db <- MonetDBLite::src_monetdblite(dbdir)
    Warning in if (length(classes) != length(handlers)) stop("bad handler specification") :
      Connection is garbage-collected, use dbDisconnect() to avoid this.
    > 
    > # copy some data to DB
    > my_iris  <- copy_to(my_db, iris)
    Error in .local(conn, statement, ...) : 
      Unable to execute statement 'START TRANSACTION'.
    Server says 'Invalid connection'.
    Calls: copy_to ... dbBegin -> dbBegin -> dbSendQuery -> dbSendQuery -> .local
    Execution halted
    ```

# PPforest

Version: 0.1.0

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Warning in PPclassify2(Tree.result = x[[1]], test.data = xnew, Rule = 1) :
      restarting interrupted promise evaluation
    Warning in PPclassify2(Tree.result = x[[1]], test.data = xnew, Rule = 1) :
      restarting interrupted promise evaluation
    Warning in PPclassify2(Tree.result = x[[1]], test.data = xnew, Rule = 1) :
      restarting interrupted promise evaluation
    Warning in PPclassify2(Tree.result = x[[1]], test.data = xnew, Rule = 1) :
      restarting interrupted promise evaluation
    Warning in PPclassify2(Tree.result = x[[1]], test.data = xnew, Rule = 1) :
      restarting interrupted promise evaluation
    Warning in PPclassify2(Tree.result = x[[1]], test.data = xnew, Rule = 1) :
      restarting interrupted promise evaluation
    Warning in PPclassify2(Tree.result = x[[1]], test.data = xnew, Rule = 1) :
      restarting interrupted promise evaluation
    Warning in PPclassify2(Tree.result = x[[1]], test.data = xnew, Rule = 1) :
      restarting interrupted promise evaluation
    Warning in PPclassify2(Tree.result = x[[1]], test.data = xnew, Rule = 1) :
      restarting interrupted promise evaluation
    Error in do.ply(i) : task 1 failed - "object 'Type' not found"
    Calls: PPforest ... trees_pred -> <Anonymous> -> llply -> <Anonymous> -> <Anonymous>
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
        nasa
    
    Loading required package: gridExtra
    
    Attaching package: 'gridExtra'
    
    The following object is masked from 'package:dplyr':
    
        combine
    
    Loading required package: PPtreeViz
    Loading required package: ggplot2
    Loading required package: partykit
    Loading required package: grid
    Loading required package: libcoin
    Loading required package: mvtnorm
    Loading required package: rpart
    Quitting from lines 155-160 (PPforest-vignette.Rmd) 
    Error: processing vignette 'PPforest-vignette.Rmd' failed with diagnostics:
    object 'Type' not found
    Execution halted
    ```

# purrr

Version: 0.2.4

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      The following object is masked from 'package:testthat':
      
          is_null
      
      > 
      > test_check("purrr")
      ── 1. Failure: can flatten to a data frame with named lists (@test-flatten.R#77)
      `flatten_dfc(list(1))` did not throw an error.
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 393 SKIPPED: 0 FAILED: 1
      1. Failure: can flatten to a data frame with named lists (@test-flatten.R#77) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# ruler

Version: 0.1.2

## Newly broken

*   checking examples ... ERROR
    ```
    ...
     1. dplyr::transmute_at(., c("disp", "qsec"), rules(z_score = abs(. -     mean(.))/sd(.) > 1))
    
    Use 'functions' to extract the individual functions. 
    
    > 
    > # Dealing with one column edge case
    > improper_pack <- . %>% dplyr::transmute_at(
    +   dplyr::vars(vs),
    +   rules(improper_is_neg = . < 0)
    + )
    > 
    > proper_pack <- . %>% dplyr::transmute_at(
    +   dplyr::vars(vs = vs),
    +   rules(proper_is_neg = . < 0)
    + )
    > 
    > mtcars[1:2, ] %>%
    +   expose(cell_packs(improper_pack, proper_pack)) %>%
    +   get_report()
    Error: `~"id"` must be a function name (quoted or unquoted) or an unquoted call, not `~`
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 276 SKIPPED: 0 FAILED: 9
      1. Error: expose works (@test-expose.R#136) 
      2. Error: expose preserves pack names (@test-expose.R#218) 
      3. Error: expose guesses (@test-expose.R#270) 
      4. Error: expose_single.default guesses row pack (@test-expose.R#326) 
      5. Error: expose_single.default guesses cell pack (@test-expose.R#335) 
      6. Error: expose_single.row_pack works (@test-expose.R#437) 
      7. Error: expose_single.cell_pack works (@test-expose.R#453) 
      8. Error: interp_row_pack_out works (@test-expose.R#602) 
      9. Error: interp_cell_pack_out works (@test-expose.R#620) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Warning in engine$weave(file, quiet = quiet, encoding = enc) :
      Pandoc (>= 1.12.3) and/or pandoc-citeproc not available. Falling back to R Markdown v1.
    Warning in engine$weave(file, quiet = quiet, encoding = enc) :
      Pandoc (>= 1.12.3) and/or pandoc-citeproc not available. Falling back to R Markdown v1.
    Warning in engine$weave(file, quiet = quiet, encoding = enc) :
      Pandoc (>= 1.12.3) and/or pandoc-citeproc not available. Falling back to R Markdown v1.
    Quitting from lines 188-209 (validation.Rmd) 
    Error: processing vignette 'validation.Rmd' failed with diagnostics:
    `~"id"` must be a function name (quoted or unquoted) or an unquoted call, not `~`
    Execution halted
    ```

