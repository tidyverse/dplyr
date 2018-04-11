# amt

Version: 0.0.3.0

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > 
    > # move
    > m <- move::move(xy$x, xy$y, lubridate::now() + lubridate::hours(1:10),
    +  proj = sp::CRS("+init=epsg:4326"))
    > move::angle(m)
    [1]   90.00000   90.00000    0.00000   44.41780  -89.58401  -89.16705 -180.00000
    [8]  134.74039 -134.79070
    > direction_abs(trk, degrees = TRUE, full_circle = FALSE, zero_dir = "N",
    +   clockwise = TRUE, append_na = FALSE, lonlat = TRUE)
     [1]   90.00000   90.00000    0.00000   44.41780  -89.58401  -89.16705
     [7]  180.00000  134.74039 -134.79070         NA
    > 
    > # trajectories
    > t1 <- trajectories::Track(
    +   spacetime::STIDF(sp::SpatialPoints(cbind(xy$x, xy$y)),
    +   lubridate::now() + lubridate::hours(1:10), data = data.frame(1:10)))
    Warning in with_tz(Sys.time(), tzone) : Unrecognized time zone ''
    Error in (function (dt, year, month, yday, mday, wday, hour, minute, second,  : 
      Invalid timezone of input vector: ""
    Calls: <Anonymous> ... reclass_date.POSIXlt -> as.POSIXlt -> do.call -> <Anonymous> -> .Call
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Warning in engine$weave(file, quiet = quiet, encoding = enc) :
      Pandoc (>= 1.12.3) and/or pandoc-citeproc not available. Falling back to R Markdown v1.
    Loading required package: tidyverse
    ── Attaching packages ────────────────────────────────── tidyverse 1.2.1 ──
    ✔ ggplot2 2.2.1          ✔ purrr   0.2.4     
    ✔ tibble  1.4.2          ✔ dplyr   0.7.4.9004
    ✔ tidyr   0.8.0          ✔ stringr 1.3.0     
    ✔ readr   1.1.1          ✔ forcats 0.3.0     
    ── Conflicts ───────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    Loading required package: survival
    .t missing, creating `track_xy`.
    Quitting from lines 120-121 (p1_getting_started.Rmd) 
    Error: processing vignette 'p1_getting_started.Rmd' failed with diagnostics:
    Column `time` classes Period and Interval from lubridate are currently not supported.
    Execution halted
    ```

# bioset

Version: 0.2.1

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > data
    # A tibble: 6 x 3
      names value  conc
      <chr> <dbl> <dbl>
    1 A       19.  1.90
    2 B       59.  5.90
    3 C       22.  2.20
    4 A       18.  1.80
    5 B       63.  6.30
    6 C       28.  2.80
    > 
    > set_calc_variability(
    +   data = data,
    +   ids = names,
    +   value,
    +   conc
    + )
    Error in mutate_impl(.data, dots) : 
      Column `value_n` is of unsupported type NULL
    Calls: set_calc_variability ... <Anonymous> -> mutate.tbl_df -> mutate_impl -> .Call
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      14: `_fseq`(`_lhs`)
      15: freduce(value, `_function_list`) at /tmp/Rtmp23zSwj/R.INSTALL1367193e58aa/magrittr/R/pipe.R:28
      16: function_list[[i]](value) at /tmp/Rtmp23zSwj/R.INSTALL1367193e58aa/magrittr/R/freduce.R:17
      17: dplyr::mutate(., `:=`(!(!target_n), n()), `:=`(!(!target_mean), mean(!(!target))), 
             `:=`(!(!target_sd), stats::sd(!(!target))), `:=`(!(!target_cv), stats::sd(!(!target))/mean(!(!target))))
      18: mutate.tbl_df(., `:=`(!(!target_n), n()), `:=`(!(!target_mean), mean(!(!target))), 
             `:=`(!(!target_sd), stats::sd(!(!target))), `:=`(!(!target_cv), stats::sd(!(!target))/mean(!(!target))))
      19: mutate_impl(.data, dots)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 75 SKIPPED: 0 FAILED: 1
      1. Error: variability is calculated correctly (@test_set.R#56) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Warning in engine$weave(file, quiet = quiet, encoding = enc) :
      Pandoc (>= 1.12.3) and/or pandoc-citeproc not available. Falling back to R Markdown v1.
    Quitting from lines 243-251 (introduction.Rmd) 
    Error: processing vignette 'introduction.Rmd' failed with diagnostics:
    Column `value_n` is of unsupported type NULL
    Execution halted
    ```

# ddpcr

Version: 1.8

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      16: freduce(value, `_function_list`) at /tmp/Rtmp23zSwj/R.INSTALL1367193e58aa/magrittr/R/pipe.R:28
      17: function_list[[i]](value) at /tmp/Rtmp23zSwj/R.INSTALL1367193e58aa/magrittr/R/freduce.R:17
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
    Identifying empty droplets... DONE (0 seconds)
    Classifying droplets... DONE (0 seconds)
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
    
        chisq.test, fisher.test, IQR
    
    Quitting from lines 217-222 (desctable.Rmd) 
    Error: processing vignette 'desctable.Rmd' failed with diagnostics:
    object 'Species' not found
    Execution halted
    ```

# fold

Version: 0.2.5

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    1   VARIABLE   META   ID   TIME
    > 
    > # another example
    > x <- Theoph
    > x %<>% mutate(
    +   conc_LABEL = 'theophylline concentration',
    +   conc_GUIDE = 'mg/L',
    +   Time_LABEL = 'time since drug administration',
    +   Time_GUIDE = 'hr',
    +   Time_HALF = Time / 2 # to demonstrate variant attribute of key column
    + )
    > x %<>% fold(Subject, Time)
    Warning in as.folded.data.frame(d, sort = sort, ...) :
      removing unique values where keys are duplicated
    > x %>% unfold %>% head
    Warning in is.na(x$META) :
      is.na() applied to non-(list or vector) of type 'NULL'
    Error in y[sapply(y, function(i) nrow(i) > 0)] : 
      invalid subscript type 'list'
    Calls: %>% ... _fseq -> freduce -> <Anonymous> -> unfold -> unfold.folded
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

# replyr

Version: 0.9.3

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘replyr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: buildJoinPlan
    > ### Title: Build a join plan
    > ### Aliases: buildJoinPlan
    > 
    > ### ** Examples
    > 
    > 
    > d <- data.frame(id=1:3, weight= c(200, 140, 98))
    > tDesc <- rbind(tableDescription('d1', d),
    +                tableDescription('d2', d))
    > tDesc$keys[[1]] <- list(PrimaryKey= 'id')
    > tDesc$keys[[2]] <- list(PrimaryKey= 'id')
    > buildJoinPlan(tDesc)
    Error in summarise_impl(.data, dots) : 
      Column `count` is of unsupported type NULL
    Calls: buildJoinPlan ... <Anonymous> -> summarise.tbl_df -> summarise_impl -> .Call
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
    Quitting from lines 57-59 (DependencySorting.Rmd) 
    Error: processing vignette 'DependencySorting.Rmd' failed with diagnostics:
    Column `count` is of unsupported type NULL
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

