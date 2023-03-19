# exuber

<details>

* Version: 1.0.1
* GitHub: https://github.com/kvasilopoulos/exuber
* Source code: https://github.com/cran/exuber
* Date/Publication: 2023-02-12 21:42:06 UTC
* Number of recursive dependencies: 100

Run `revdepcheck::cloud_details(, "exuber")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       33. │             └─base (local) withOneRestart(expr, restarts[[1L]])
       34. │               └─base (local) doWithOneRestart(return(expr), restart)
       35. └─dplyr (local) `<fn>`(`<vc______>`)
       36.   └─dplyr:::rethrow_warning_join_relationship_many_to_many(cnd, error_call)
       37.     └─dplyr:::warn_join(...)
       38.       └─dplyr:::warn_dplyr(...)
       39.         └─rlang::warn(...)
       40.           └─base::warning(cnd)
       41.             └─base::withRestarts(...)
       42.               └─base (local) withOneRestart(expr, restarts[[1L]])
       43.                 └─base (local) doWithOneRestart(return(expr), restart)
      
      [ FAIL 22 | WARN 29 | SKIP 4 | PASS 214 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        libs   4.3Mb
    ```

# modelplotr

<details>

* Version: 1.1.0
* GitHub: https://github.com/jurrr/modelplotr
* Source code: https://github.com/cran/modelplotr
* Date/Publication: 2020-10-13 04:20:05 UTC
* Number of recursive dependencies: 150

Run `revdepcheck::cloud_details(, "modelplotr")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘modelplotr.Rmd’ using rmarkdown
    Quitting from lines 198-216 (modelplotr.Rmd) 
    Error: processing vignette 'modelplotr.Rmd' failed with diagnostics:
    replacement has length zero
    --- failed re-building ‘modelplotr.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘modelplotr.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

