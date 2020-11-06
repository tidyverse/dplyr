# jstor

<details>

* Version: 0.3.9
* Source code: https://github.com/cran/jstor
* URL: https://github.com/ropensci/jstor, https://docs.ropensci.org/jstor
* BugReports: https://github.com/ropensci/jstor/issues
* Date/Publication: 2020-06-04 04:50:03 UTC
* Number of recursive dependencies: 69

Run `cloud_details(, "jstor")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    ```

# ralger

<details>

* Version: 2.0.1
* Source code: https://github.com/cran/ralger
* URL: https://github.com/feddelegrand7/ralger
* BugReports: https://github.com/feddelegrand7/ralger/issues
* Date/Publication: 2020-07-24 04:10:02 UTC
* Number of recursive dependencies: 65

Run `cloud_details(, "ralger")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      x[214]: "Dead Poets Society"
      y[214]: "The Wages of Fear"
      
      ── 2. Failure: tidy_scrap() function (@test-tidy_scrap.R#3)  ───────────────────
      tidy_scrap(...) not equal to `%>%`(...).
      Component "value": 2 string mismatches
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 2 | SKIPPED: 0 | WARNINGS: 1 | FAILED: 2 ]
      1. Failure: scrap() function (@test-scrap.R#4) 
      2. Failure: tidy_scrap() function (@test-tidy_scrap.R#3) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘testthat’
      All declared Imports should be used.
    ```

