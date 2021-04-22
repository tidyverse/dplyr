# codebook

<details>

* Version: 0.9.2
* GitHub: https://github.com/rubenarslan/codebook
* Source code: https://github.com/cran/codebook
* Date/Publication: 2020-06-06 23:40:03 UTC
* Number of recursive dependencies: 151

Run `cloud_details(, "codebook")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
     19. │       └─dplyr:::summarise.grouped_df(...)
     20. │         └─dplyr:::summarise_cols(.data, ..., caller_env = caller_env())
     21. │           ├─base::withCallingHandlers(...)
     22. │           └─mask$eval_all_summarise(quo)
     23. ├─purrr::map2(...)
     24. │ ├─skimr:::.f(.x[[1L]], .y[[1L]], ...)
     25. │ └─skimr:::skim_by_type.data.frame(.x[[1L]], .y[[1L]], ...)
     26. │   ├─dplyr::summarize(data, dplyr::across(variable_names, mangled_skimmers$funs))
     27. │   └─dplyr:::summarise.data.frame(...)
     28. │     └─dplyr:::summarise_cols(.data, ..., caller_env = caller_env())
     29. │       ├─base::withCallingHandlers(...)
     30. │       └─dplyr:::expand_across(dots[[i]])
     31. │         ├─dplyr:::across_setup(...)
     32. │         └─rlang::eval_tidy(expr$.fns, mask)
     33. ├─base::.handleSimpleError(...)
     34. │ └─dplyr:::h(simpleError(msg, call))
     35. │   └─rlang::abort(bullets, class = "dplyr_error")
     36. │     └─rlang:::signal_abort(cnd)
     37. │       └─base::signalCondition(cnd)
     38. └─(function (e) ...
    Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘userfriendlyscience’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘graphics’ ‘jsonlite’ ‘rlang’ ‘tidyselect’ ‘vctrs’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 65 marked UTF-8 strings
    ```

# DAISIE

<details>

* Version: 3.0.1
* GitHub: NA
* Source code: https://github.com/cran/DAISIE
* Date/Publication: 2020-08-26 10:00:07 UTC
* Number of recursive dependencies: 109

Run `cloud_details(, "DAISIE")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    +  metadata$endemic[3] <- FALSE
    +  metadata$uncertain[2] <- TRUE
    + 
    +  # Island age
    +  age <- 5
    + 
    +  # Make a plot
    +  p <- DAISIE_plot_input(
    +    trees,
    +    age,
    +    tcols,
    +    metadata,
    +    mapping = ggplot2::aes(color = endemic, linetype = uncertain),
    +    pargs = list(size = 3)
    +  )
    + p
    + }
    Error in DataMask$new(.data, caller_env) : 
      argument "caller_env" is missing, with no default
    Calls: DAISIE_plot_input ... mutate.data.frame -> mutate_cols -> <Anonymous> -> initialize
    Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘doMC’
    ```

# diseq

<details>

* Version: 0.2.1
* GitHub: https://github.com/pi-kappa-devel/diseq
* Source code: https://github.com/cran/diseq
* Date/Publication: 2021-04-14 11:20:02 UTC
* Number of recursive dependencies: 114

Run `cloud_details(, "diseq")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
      5. │     ├─methods::callNextMethod(...)
      6. │     │ └─base::eval(call, callEnv)
      7. │     │   └─base::eval(call, callEnv)
      8. │     └─diseq:::.nextMethod(...)
      9. │       └─diseq:::.local(.Object, ...)
     10. │         ├─methods::callNextMethod(...)
     11. │         │ └─base::eval(call, callEnv)
     12. │         │   └─base::eval(call, callEnv)
     13. │         └─diseq:::.nextMethod(...)
     14. │           └─diseq:::.local(.Object, ...)
     15. │             └─`%>%`(...)
     16. ├─dplyr::mutate(., dplyr::across(where(is.factor), remove_unused_levels))
     17. ├─dplyr:::mutate.data.frame(...)
     18. │ └─dplyr:::mutate_cols(.data, ..., caller_env = caller_env())
     19. │   ├─base::withCallingHandlers(...)
     20. │   └─dplyr:::expand_across(dots[[i]])
     21. │     ├─dplyr:::across_setup(...)
     22. │     └─rlang::eval_tidy(expr$.fns, mask)
     23. └─base::.handleSimpleError(...)
     24.   └─dplyr:::h(simpleError(msg, call))
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       18. │     └─rlang::eval_tidy(expr$.fns, mask)
       19. └─base::.handleSimpleError(...)
       20.   └─dplyr:::h(simpleError(msg, call))
      ── Error (test-equilibrium.R:24:1): (code run outside of `test_that()`) ────────
      Error: unable to find an inherited method for function 'model_description' for signature '"NULL"'
      Backtrace:
          █
       1. ├─testthat::test_that(...) test-equilibrium.R:24:0
       2. ├─base::paste0(model_description(mdl), " can be estimated")
       3. └─diseq::model_description(mdl)
       4.   └─(function (classes, fdef, mtable) ...
      
      [ FAIL 6 | WARN 0 | SKIP 2 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.5Mb
      sub-directories of 1Mb or more:
        libs   4.0Mb
    ```

# disk.frame

<details>

* Version: 0.4.1
* GitHub: https://github.com/xiaodaigh/disk.frame
* Source code: https://github.com/cran/disk.frame
* Date/Publication: 2021-03-14 15:40:10 UTC
* Number of recursive dependencies: 112

Run `cloud_details(, "disk.frame")` for more info

</details>

## Newly broken

*   checking S3 generic/method consistency ... WARNING
    ```
    add_count:
      function(x, ..., wt, sort, name, .drop)
    add_count.disk.frame:
      function(.data, ...)
    
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    
    Found the following apparent S3 methods exported but not registered:
      add_count.disk.frame
    See section ‘Registering S3 methods’ in the ‘Writing R Extensions’
    manual.
    ```

*   checking Rd \usage sections ... NOTE
    ```
    S3 methods shown with full name in documentation object 'select.disk.frame':
      ‘add_count.disk.frame’
    
    The \usage entries for S3 methods should use the \method markup and not
    their full name.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

# egor

<details>

* Version: 0.21.02
* GitHub: https://github.com/tilltnet/egor
* Source code: https://github.com/cran/egor
* Date/Publication: 2021-02-12 14:20:02 UTC
* Number of recursive dependencies: 82

Run `cloud_details(, "egor")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    +     e.max.alters = 8,
    +     e.first.var = dy.first.var)
    Sorting data by egoID and alterID.
    Transforming wide edge data to edgelist.
    Error: Problem with `mutate()` input `..1`.
    ℹ `..1 = across(all_of(var_names1), as_lcc)`.
    ✖ object 'as_lcc' not found
    Backtrace:
         █
      1. ├─egor::twofiles_to_egor(...)
      2. │ └─egor:::harmonize_id_var_classes(...)
      3. │   ├─dplyr::mutate(df1, across(all_of(var_names1), as_lcc))
      4. │   └─dplyr:::mutate.data.frame(df1, across(all_of(var_names1), as_lcc))
      5. │     └─dplyr:::mutate_cols(.data, ..., caller_env = caller_env())
      6. │       ├─base::withCallingHandlers(...)
      7. │       └─dplyr:::expand_across(dots[[i]])
      8. │         ├─dplyr:::across_setup(...)
      9. │         └─rlang::eval_tidy(expr$.fns, mask)
     10. └─base::.handleSimpleError(...)
     11.   └─dplyr:::h(simpleError(msg, call))
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        1. ├─egor::twofiles_to_egor(...) test-twofiles_to_egor.R:107:12
        2. │ └─egor:::harmonize_id_var_classes(...)
        3. │   ├─dplyr::mutate(df1, across(all_of(var_names1), as_lcc))
        4. │   └─dplyr:::mutate.data.frame(df1, across(all_of(var_names1), as_lcc))
        5. │     └─dplyr:::mutate_cols(.data, ..., caller_env = caller_env())
        6. │       ├─base::withCallingHandlers(...)
        7. │       └─dplyr:::expand_across(dots[[i]])
        8. │         ├─dplyr:::across_setup(...)
        9. │         └─rlang::eval_tidy(expr$.fns, mask)
       10. └─base::.handleSimpleError(...)
       11.   └─dplyr:::h(simpleError(msg, call))
      
      [ FAIL 8 | WARN 1 | SKIP 0 | PASS 188 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking whether package ‘egor’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘tibble’ was built under R version 4.0.4
    See ‘/tmp/workdir/egor/new/egor.Rcheck/00install.out’ for details.
    ```

# EpiNow2

<details>

* Version: 1.3.2
* GitHub: https://github.com/epiforecasts/EpiNow2
* Source code: https://github.com/cran/EpiNow2
* Date/Publication: 2020-12-14 09:00:15 UTC
* Number of recursive dependencies: 152

Run `cloud_details(, "EpiNow2")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is 265.8Mb
      sub-directories of 1Mb or more:
        libs  264.0Mb
    ```

## Newly fixed

*   checking whether package ‘EpiNow2’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/EpiNow2/old/EpiNow2.Rcheck/00install.out’ for details.
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘EpiSoon’
    ```

# ggasym

<details>

* Version: 0.1.5
* GitHub: https://github.com/jhrcook/ggasym
* Source code: https://github.com/cran/ggasym
* Date/Publication: 2020-07-15 17:30:03 UTC
* Number of recursive dependencies: 98

Run `cloud_details(, "ggasym")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      get_other_combs(a, b) not equal to `df`.
      Attributes: < Names: 1 string mismatch >
      Attributes: < Length mismatch: comparison on first 2 components >
      Attributes: < Component 2: Modes: list, numeric >
      Attributes: < Component 2: names for target but not for current >
      Attributes: < Component 2: Component 1: Numeric: lengths (2, 1) differ >
      Attributes: < Component 2: Component 2: Modes: list, numeric >
      Attributes: < Component 2: Component 2: names for target but not for current >
      Attributes: < Component 2: Component 2: Length mismatch: comparison on first 1 components >
      Attributes: < Component 2: Component 2: Component 1: Modes: character, numeric >
      ...
      
      [ FAIL 3 | WARN 2 | SKIP 0 | PASS 264 ]
      Error: Test failures
      Execution halted
    ```

# groupedstats

<details>

* Version: 2.0.1
* GitHub: https://github.com/IndrajeetPatil/groupedstats
* Source code: https://github.com/cran/groupedstats
* Date/Publication: 2021-01-21 11:40:15 UTC
* Number of recursive dependencies: 75

Run `cloud_details(, "groupedstats")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
     20. │             └─dplyr:::summarise.grouped_df(...)
     21. │               └─dplyr:::summarise_cols(.data, ..., caller_env = caller_env())
     22. │                 ├─base::withCallingHandlers(...)
     23. │                 └─mask$eval_all_summarise(quo)
     24. ├─purrr::map2(...)
     25. │ ├─skimr:::.f(.x[[1L]], .y[[1L]], ...)
     26. │ └─skimr:::skim_by_type.data.frame(.x[[1L]], .y[[1L]], ...)
     27. │   ├─dplyr::summarize(data, dplyr::across(variable_names, mangled_skimmers$funs))
     28. │   └─dplyr:::summarise.data.frame(...)
     29. │     └─dplyr:::summarise_cols(.data, ..., caller_env = caller_env())
     30. │       ├─base::withCallingHandlers(...)
     31. │       └─dplyr:::expand_across(dots[[i]])
     32. │         ├─dplyr:::across_setup(...)
     33. │         └─rlang::eval_tidy(expr$.fns, mask)
     34. ├─base::.handleSimpleError(...)
     35. │ └─dplyr:::h(simpleError(msg, call))
     36. │   └─rlang::abort(bullets, class = "dplyr_error")
     37. │     └─rlang:::signal_abort(cnd)
     38. │       └─base::signalCondition(cnd)
     39. └─(function (e) ...
    Execution halted
    ```

# lpirfs

<details>

* Version: 0.2.0
* GitHub: https://github.com/adaemmerp/lpirfs
* Source code: https://github.com/cran/lpirfs
* Date/Publication: 2021-03-23 16:10:02 UTC
* Number of recursive dependencies: 130

Run `cloud_details(, "lpirfs")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        4. ├─dplyr::select(., -specs$l_exog_data)
        5. ├─dplyr::ungroup(.)
        6. ├─dplyr::mutate(., across(specs$l_exog_data, lag_functions))
        7. ├─dplyr:::mutate.data.frame(., across(specs$l_exog_data, lag_functions))
        8. │ └─dplyr:::mutate_cols(.data, ..., caller_env = caller_env())
        9. │   ├─base::withCallingHandlers(...)
       10. │   └─dplyr:::expand_across(dots[[i]])
       11. │     ├─dplyr:::across_setup(...)
       12. │     └─rlang::eval_tidy(expr$.fns, mask)
       13. └─base::.handleSimpleError(...)
       14.   └─dplyr:::h(simpleError(msg, call))
      
      [ FAIL 9 | WARN 0 | SKIP 0 | PASS 145 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 10.5Mb
      sub-directories of 1Mb or more:
        libs   9.0Mb
    ```

# mason

<details>

* Version: 0.3.0
* GitHub: https://github.com/lwjohnst86/mason
* Source code: https://github.com/cran/mason
* Date/Publication: 2020-06-04 16:10:05 UTC
* Number of recursive dependencies: 67

Run `cloud_details(, "mason")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    +  add_variables('xvar', c('Agriculture', 'Catholic')) %>%
    +  add_variables('covariates', 'Examination') %>%
    +  construct() %>%
    +  scrub()
    > polish_renaming(ds, function(x) gsub('Education', 'Schooling', x))
    Error: Problem with `mutate()` input `..1`.
    ℹ `..1 = dplyr::across(columns, renaming.fun)`.
    ✖ object 'renaming.fun' not found
    Backtrace:
         █
      1. ├─mason::polish_renaming(...)
      2. │ ├─dplyr::mutate(data, dplyr::across(columns, renaming.fun))
      3. │ └─dplyr:::mutate.data.frame(data, dplyr::across(columns, renaming.fun))
      4. │   └─dplyr:::mutate_cols(.data, ..., caller_env = caller_env())
      5. │     ├─base::withCallingHandlers(...)
      6. │     └─dplyr:::expand_across(dots[[i]])
      7. │       ├─dplyr:::across_setup(...)
      8. │       └─rlang::eval_tidy(expr$.fns, mask)
      9. └─base::.handleSimpleError(...)
     10.   └─dplyr:::h(simpleError(msg, call))
    Execution halted
    ```

# mcp

<details>

* Version: 0.3.0
* GitHub: https://github.com/lindeloev/mcp
* Source code: https://github.com/cran/mcp
* Date/Publication: 2020-08-03 09:30:21 UTC
* Number of recursive dependencies: 118

Run `cloud_details(, "mcp")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
          y ~ 1 + ar(1), ~1 + x + ar(2, 1 + x + I(x^3)) ──
      |... is not TRUE
      
      `actual`:   FALSE
      `expected`: TRUE 
      Backtrace:
          █
       1. └─mcp:::test_runs(model, ...) helper-runs.R:325:6
       2.   └─mcp:::test_pp_eval(fit) helper-runs.R:113:6
       3.     └─mcp:::test_pp_eval_func(fit, predict) helper-runs.R:241:2
       4.       └─testthat::expect_true(...) helper-runs.R:228:4
      
      [ FAIL 4 | WARN 0 | SKIP 3 | PASS 3626 ]
      Error: Test failures
      Execution halted
    ```

# mdsr

<details>

* Version: 0.2.5
* GitHub: https://github.com/mdsr-book/mdsr
* Source code: https://github.com/cran/mdsr
* Date/Publication: 2021-03-29 19:50:03 UTC
* Number of recursive dependencies: 143

Run `cloud_details(, "mdsr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
     16. │       │ └─rlang::list2(...)
     17. │       └─skimr::is_data_frame(object)
     18. │         └─skimr:::make_issue(inherits(object, "data.frame"), "not a data.frame")
     19. │           └─base::structure(check, message = if (!check) message else character())
     20. ├─purrr::map2(...)
     21. │ ├─skimr:::.f(.x[[1L]], .y[[1L]], ...)
     22. │ └─skimr:::skim_by_type.data.frame(.x[[1L]], .y[[1L]], ...)
     23. │   ├─dplyr::summarize(data, dplyr::across(variable_names, mangled_skimmers$funs))
     24. │   └─dplyr:::summarise.data.frame(...)
     25. │     └─dplyr:::summarise_cols(.data, ..., caller_env = caller_env())
     26. │       ├─base::withCallingHandlers(...)
     27. │       └─dplyr:::expand_across(dots[[i]])
     28. │         ├─dplyr:::across_setup(...)
     29. │         └─rlang::eval_tidy(expr$.fns, mask)
     30. ├─base::.handleSimpleError(...)
     31. │ └─dplyr:::h(simpleError(msg, call))
     32. │   └─rlang::abort(bullets, class = "dplyr_error")
     33. │     └─rlang:::signal_abort(cnd)
     34. │       └─base::signalCondition(cnd)
     35. └─(function (e) ...
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2866 marked UTF-8 strings
    ```

# modeltime

<details>

* Version: 0.5.1
* GitHub: https://github.com/business-science/modeltime
* Source code: https://github.com/cran/modeltime
* Date/Publication: 2021-04-03 14:40:02 UTC
* Number of recursive dependencies: 207

Run `cloud_details(, "modeltime")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
      2. ├─generics::fit(., log(value) ~ date + id, data = training(splits))
      3. ├─parsnip::fit.model_spec(., log(value) ~ date + id, data = training(splits))
      4. │ └─parsnip:::form_xy(...)
      5. │   └─parsnip:::xy_xy(...)
      6. │     ├─base::system.time(...)
      7. │     └─parsnip:::eval_mod(...)
      8. │       └─rlang::eval_tidy(e, ...)
      9. ├─modeltime::window_function_fit_impl(...)
     10. │ └─`%>%`(...)
     11. ├─dplyr::ungroup(.)
     12. ├─dplyr::summarise(...)
     13. ├─dplyr:::summarise.grouped_df(...)
     14. │ └─dplyr:::summarise_cols(.data, ..., caller_env = caller_env())
     15. │   ├─base::withCallingHandlers(...)
     16. │   └─dplyr:::expand_across(dots[[i]])
     17. │     ├─dplyr:::across_setup(...)
     18. │     └─rlang::eval_tidy(expr$.fns, mask)
     19. └─base::.handleSimpleError(...)
     20.   └─dplyr:::h(simpleError(msg, call))
    Timing stopped at: 0.061 0 0.062
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       15. │ └─`%>%`(...)
       16. ├─dplyr::ungroup(.)
       17. ├─dplyr::summarise(...)
       18. ├─dplyr:::summarise.grouped_df(...)
       19. │ └─dplyr:::summarise_cols(.data, ..., caller_env = caller_env())
       20. │   ├─base::withCallingHandlers(...)
       21. │   └─dplyr:::expand_across(dots[[i]])
       22. │     ├─dplyr:::across_setup(...)
       23. │     └─rlang::eval_tidy(expr$.fns, mask)
       24. └─base::.handleSimpleError(...)
       25.   └─dplyr:::h(simpleError(msg, call))
      
      [ FAIL 3 | WARN 2 | SKIP 8 | PASS 529 ]
      Error: Test failures
      Execution halted
    ```

# modeltime.resample

<details>

* Version: 0.2.0
* GitHub: https://github.com/business-science/modeltime.resample
* Source code: https://github.com/cran/modeltime.resample
* Date/Publication: 2021-03-14 20:40:07 UTC
* Number of recursive dependencies: 204

Run `cloud_details(, "modeltime.resample")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Error: Problem with `summarise()` input `..1`.
    ℹ `..1 = dplyr::across(.fns = summary_fns, ...)`.
    ✖ object 'summary_fns' not found
    ℹ The error occurred in group 1: .model_id = 1, .model_desc = "ARIMA(0,1,1)(0,1,1)[12]", .type = "Resamples", n = 6.
    Backtrace:
         █
      1. ├─`%>%`(...)
      2. ├─modeltime::table_modeltime_accuracy(., .interactive = FALSE)
      3. ├─modeltime.resample::modeltime_resample_accuracy(.)
      4. │ └─`%>%`(...)
      5. ├─dplyr::ungroup(.)
      6. ├─dplyr::summarise(., dplyr::across(.fns = summary_fns, ...), .groups = "drop")
      7. ├─dplyr:::summarise.grouped_df(...)
      8. │ └─dplyr:::summarise_cols(.data, ..., caller_env = caller_env())
      9. │   ├─base::withCallingHandlers(...)
     10. │   └─dplyr:::expand_across(dots[[i]])
     11. │     ├─dplyr:::across_setup(...)
     12. │     └─rlang::eval_tidy(expr$.fns, mask)
     13. └─base::.handleSimpleError(...)
     14.   └─dplyr:::h(simpleError(msg, call))
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        3. │ └─`%>%`(...)
        4. ├─dplyr::ungroup(.)
        5. ├─dplyr::summarise(., dplyr::across(.fns = summary_fns, ...), .groups = "drop")
        6. ├─dplyr:::summarise.grouped_df(...)
        7. │ └─dplyr:::summarise_cols(.data, ..., caller_env = caller_env())
        8. │   ├─base::withCallingHandlers(...)
        9. │   └─dplyr:::expand_across(dots[[i]])
       10. │     ├─dplyr:::across_setup(...)
       11. │     └─rlang::eval_tidy(expr$.fns, mask)
       12. └─base::.handleSimpleError(...)
       13.   └─dplyr:::h(simpleError(msg, call))
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 10 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking whether package ‘modeltime.resample’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘modeltime’ was built under R version 4.0.4
    See ‘/tmp/workdir/modeltime.resample/new/modeltime.resample.Rcheck/00install.out’ for details.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘crayon’ ‘dials’ ‘glue’ ‘parsnip’
      All declared Imports should be used.
    ```

# mrgsolve

<details>

* Version: 0.11.0
* GitHub: https://github.com/metrumresearchgroup/mrgsolve
* Source code: https://github.com/cran/mrgsolve
* Date/Publication: 2021-03-28 07:00:02 UTC
* Number of recursive dependencies: 84

Run `cloud_details(, "mrgsolve")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      test-workflow: ...
      
      ══ Failed ══════════════════════════════════════════════════════════════════════
      ── 1. Failure (test-mrgsim.R:118:3): mrgsim with data and idata ────────────────
      round(out_pars, 6) not identical to round(idata_cut, 6).
      Attributes: < Names: 1 string mismatch >
      Attributes: < Length mismatch: comparison on first 2 components >
      Attributes: < Component 2: Modes: numeric, list >
      Attributes: < Component 2: Lengths: 7, 2 >
      Attributes: < Component 2: names for current but not for target >
      Attributes: < Component 2: target is numeric, current is list >
      
      ══ DONE ════════════════════════════════════════════════════════════════════════
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 12.5Mb
      sub-directories of 1Mb or more:
        libs  10.8Mb
    ```

# panelr

<details>

* Version: 0.7.5
* GitHub: https://github.com/jacob-long/panelr
* Source code: https://github.com/cran/panelr
* Date/Publication: 2021-01-18 17:00:02 UTC
* Number of recursive dependencies: 206

Run `cloud_details(, "panelr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    
    > ### Name: summary.panel_data
    > ### Title: Summarize panel data frames
    > ### Aliases: summary.panel_data
    > 
    > ### ** Examples
    > 
    > 
    > data("WageData")
    > wages <- panel_data(WageData, id = id, wave = t)
    > summary(wages, lwage, exp, wks)
    Loading required namespace: skimr
    Error: Problem with `summarise()` column `skimmed`.
    ℹ `skimmed = purrr::map2(...)`.
    ✖ Problem with `summarise()` input `..1`.
    ℹ `..1 = dplyr::across(variable_names, mangled_skimmers$funs)`.
    ✖ object 'mangled_skimmers' not found
    ℹ The error occurred in group 1: t = 1.
    ℹ The error occurred in group 1: t = 1.
    Backtrace:
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       25. │     └─dplyr:::summarise_cols(.data, ..., caller_env = caller_env())
       26. │       ├─base::withCallingHandlers(...)
       27. │       └─dplyr:::expand_across(dots[[i]])
       28. │         ├─dplyr:::across_setup(...)
       29. │         └─rlang::eval_tidy(expr$.fns, mask)
       30. ├─base::.handleSimpleError(...)
       31. │ └─dplyr:::h(simpleError(msg, call))
       32. │   └─rlang::abort(bullets, class = "dplyr_error")
       33. │     └─rlang:::signal_abort(cnd)
       34. │       └─base::signalCondition(cnd)
       35. └─(function (e) ...
      
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 291 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘AER’
    ```

# ratPASTA

<details>

* Version: 0.1.2
* GitHub: https://github.com/ikodvanj/ratPASTA
* Source code: https://github.com/cran/ratPASTA
* Date/Publication: 2020-07-04 22:50:09 UTC
* Number of recursive dependencies: 123

Run `cloud_details(, "ratPASTA")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── Failure (test-loadstartledata.R:39:3): Auto import false ────────────────────
      `df_no_auto_import` not equal to readRDS("results4.rds").
      Attributes: < Names: 1 string mismatch >
      Attributes: < Length mismatch: comparison on first 2 components >
      Attributes: < Component 2: Lengths: 3384, 66912 >
      Attributes: < Component 2: names for target but not for current >
      Attributes: < Component 2: Attributes: < Modes: list, NULL > >
      Attributes: < Component 2: Attributes: < Lengths: 1, 0 > >
      Attributes: < Component 2: Attributes: < names for target but not for current > >
      Attributes: < Component 2: Attributes: < current is not list-like > >
      Attributes: < Component 2: target is omit, current is numeric >
      
      [ FAIL 3 | WARN 0 | SKIP 0 | PASS 20 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘hms’
      All declared Imports should be used.
    ```

# readabs

<details>

* Version: 0.4.8
* GitHub: https://github.com/mattcowgill/readabs
* Source code: https://github.com/cran/readabs
* Date/Publication: 2021-02-09 06:10:03 UTC
* Number of recursive dependencies: 86

Run `cloud_details(, "readabs")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        1. ├─readabs:::tidy_awe(awe) test-read_awe.R:14:2
        2. │ └─`%>%`(...)
        3. ├─dplyr::mutate(...)
        4. ├─dplyr:::mutate.data.frame(...)
        5. │ └─dplyr:::mutate_cols(.data, ..., caller_env = caller_env())
        6. │   ├─base::withCallingHandlers(...)
        7. │   └─dplyr:::expand_across(dots[[i]])
        8. │     ├─dplyr:::across_setup(...)
        9. │     └─rlang::eval_tidy(expr$.fns, mask)
       10. └─base::.handleSimpleError(...)
       11.   └─dplyr:::h(simpleError(msg, call))
      
      [ FAIL 1 | WARN 0 | SKIP 25 | PASS 47 ]
      Error: Test failures
      Execution halted
    ```

# rsetse

<details>

* Version: 0.4.0
* GitHub: https://github.com/JonnoB/rSETSe
* Source code: https://github.com/cran/rsetse
* Date/Publication: 2020-11-12 09:30:02 UTC
* Number of recursive dependencies: 65

Run `cloud_details(, "rsetse")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > 
    > out <- create_node_edge_df(embeddings_data, function_names = c("mean", "mode", "sum"))
    Error: Problem with `summarise()` input `..1`.
    ℹ `..1 = dplyr::across(.cols = c(tension, strain), .fns = function_list)`.
    ✖ object 'function_list' not found
    ℹ The error occurred in group 1: node = "A".
    Backtrace:
         █
      1. ├─rsetse::create_node_edge_df(...)
      2. │ └─`%>%`(...)
      3. ├─dplyr::left_join(...)
      4. ├─dplyr::summarise(...)
      5. ├─dplyr:::summarise.grouped_df(...)
      6. │ └─dplyr:::summarise_cols(.data, ..., caller_env = caller_env())
      7. │   ├─base::withCallingHandlers(...)
      8. │   └─dplyr:::expand_across(dots[[i]])
      9. │     ├─dplyr:::across_setup(...)
     10. │     └─rlang::eval_tidy(expr$.fns, mask)
     11. └─base::.handleSimpleError(...)
     12.   └─dplyr:::h(simpleError(msg, call))
    Execution halted
    ```

# skimr

<details>

* Version: 2.1.3
* GitHub: https://github.com/ropensci/skimr
* Source code: https://github.com/cran/skimr
* Date/Publication: 2021-03-07 05:50:02 UTC
* Number of recursive dependencies: 74

Run `cloud_details(, "skimr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
      5. │ └─dplyr:::summarise.grouped_df(...)
      6. │   └─dplyr:::summarise_cols(.data, ..., caller_env = caller_env())
      7. │     ├─base::withCallingHandlers(...)
      8. │     └─mask$eval_all_summarise(quo)
      9. ├─purrr::map2(...)
     10. │ ├─skimr:::.f(.x[[1L]], .y[[1L]], ...)
     11. │ └─skimr:::skim_by_type.data.frame(.x[[1L]], .y[[1L]], ...)
     12. │   ├─dplyr::summarize(data, dplyr::across(variable_names, mangled_skimmers$funs))
     13. │   └─dplyr:::summarise.data.frame(...)
     14. │     └─dplyr:::summarise_cols(.data, ..., caller_env = caller_env())
     15. │       ├─base::withCallingHandlers(...)
     16. │       └─dplyr:::expand_across(dots[[i]])
     17. │         ├─dplyr:::across_setup(...)
     18. │         └─rlang::eval_tidy(expr$.fns, mask)
     19. ├─base::.handleSimpleError(...)
     20. │ └─dplyr:::h(simpleError(msg, call))
     21. │   └─rlang::abort(bullets, class = "dplyr_error")
     22. │     └─rlang:::signal_abort(cnd)
     23. │       └─base::signalCondition(cnd)
     24. └─(function (e) ...
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       12. │     └─dplyr:::summarise_cols(.data, ..., caller_env = caller_env())
       13. │       ├─base::withCallingHandlers(...)
       14. │       └─dplyr:::expand_across(dots[[i]])
       15. │         ├─dplyr:::across_setup(...)
       16. │         └─rlang::eval_tidy(expr$.fns, mask)
       17. ├─base::.handleSimpleError(...)
       18. │ └─dplyr:::h(simpleError(msg, call))
       19. │   └─rlang::abort(bullets, class = "dplyr_error")
       20. │     └─rlang:::signal_abort(cnd)
       21. │       └─base::signalCondition(cnd)
       22. └─(function (e) ...
      
      [ FAIL 93 | WARN 0 | SKIP 2 | PASS 95 ]
      Error: Test failures
      Execution halted
    ```

