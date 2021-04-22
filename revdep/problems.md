# AMR

<details>

* Version: 1.6.0
* GitHub: https://github.com/msberends/AMR
* Source code: https://github.com/cran/AMR
* Date/Publication: 2021-03-15 00:10:06 UTC
* Number of recursive dependencies: 84

Run `cloud_details(, "AMR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
      1. ├─skimr::skim(example_isolates)
      2. │ ├─dplyr::summarize(...)
      3. │ └─dplyr:::summarise.grouped_df(...)
      4. │   └─dplyr:::summarise_cols(.data, ..., caller_env = caller_env())
      5. │     ├─base::withCallingHandlers(...)
      6. │     └─mask$eval_all_summarise(quo)
      7. ├─purrr::map2(...)
      8. │ ├─skimr:::.f(.x[[1L]], .y[[1L]], ...)
      9. │ └─skimr:::skim_by_type.data.frame(.x[[1L]], .y[[1L]], ...)
     10. │   ├─dplyr::summarize(data, dplyr::across(variable_names, mangled_skimmers$funs))
     11. │   └─dplyr:::summarise.data.frame(...)
     12. │     └─dplyr:::summarise_cols(.data, ..., caller_env = caller_env())
     13. │       ├─base::withCallingHandlers(...)
     14. │       └─mask$eval_all_summarise(quo)
     15. ├─base::.handleSimpleError(...)
     16. │ └─dplyr:::h(simpleError(msg, call))
     17. │   └─rlang::abort(bullets, class = "dplyr_error")
     18. │     └─rlang:::signal_abort(cnd)
     19. │       └─base::signalCondition(cnd)
     20. └─(function (e) ...
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.2Mb
      sub-directories of 1Mb or more:
        R      4.2Mb
        data   3.5Mb
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
        libs  264.1Mb
    ```

## Newly fixed

*   checking whether package ‘EpiNow2’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/EpiNow2/old/EpiNow2.Rcheck/00install.out’ for details.
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

# JFE

<details>

* Version: 2.5.1
* GitHub: NA
* Source code: https://github.com/cran/JFE
* Date/Publication: 2020-10-01 09:50:02 UTC
* Number of recursive dependencies: 224

Run `cloud_details(, "JFE")` for more info

</details>

## Newly broken

*   checking whether package ‘JFE’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/JFE/new/JFE.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘JFE’ ...
** package ‘JFE’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Warning in fun(libname, pkgname) : couldn't connect to display ":109"
Error in structure(.External(.C_dotTcl, ...), class = "tclObj") : 
  [tcl] invalid command name "ttk::style".

Error: unable to load R code in package ‘JFE’
Execution halted
ERROR: lazy loading failed for package ‘JFE’
* removing ‘/tmp/workdir/JFE/new/JFE.Rcheck/JFE’

```
### CRAN

```
* installing *source* package ‘JFE’ ...
** package ‘JFE’ successfully unpacked and MD5 sums checked
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
* DONE (JFE)

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
      ── Failure (helper-runs.R:325:7): good_poisson:
          y ~ 1 + ar(1), ~1 + x + ar(2, 1 + x + I(x^3)) ──
      stringr::str_starts(error_message, expected_error_poisson) is not TRUE
      
      `actual`:   FALSE
      `expected`: TRUE 
      Backtrace:
          █
       1. └─mcp:::test_runs(model, ...) helper-runs.R:325:6
       2.   └─mcp:::test_pp_eval(fit) helper-runs.R:113:6
       3.     └─testthat::expect_true(stringr::str_starts(error_message, expected_error_poisson)) helper-runs.R:293:6
      
      [ FAIL 7 | WARN 0 | SKIP 3 | PASS 3623 ]
      Error: Test failures
      Execution halted
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

# spatPomp

<details>

* Version: 0.21.0.0
* GitHub: https://github.com/kidusasfaw/spatPomp
* Source code: https://github.com/cran/spatPomp
* Date/Publication: 2021-04-12 08:40:02 UTC
* Number of recursive dependencies: 66

Run `cloud_details(, "spatPomp")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Welcome to pomp! Version 3 incorporates some changes to the behavior of
      package algorithms that are not backward compatible. See the package
      NEWS for the details.
      
      Warning message:
      package 'pomp' was built under R version 4.0.4 
      > 
      > test_check("spatPomp")
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test_lorenz_methods.R:108:3): IGIRF, IUBF and IEnKF produce estimates that are not far from IF2 for low dimensions ──
      abs(logLik(iubf_out) - logLik(mif2_out)) is not strictly less than 25. Difference: 3.94
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 12 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking whether package ‘spatPomp’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘pomp’ was built under R version 4.0.4
    See ‘/tmp/workdir/spatPomp/new/spatPomp.Rcheck/00install.out’ for details.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘doParallel’ ‘ggplot2’ ‘parallel’
      All declared Imports should be used.
    ```

