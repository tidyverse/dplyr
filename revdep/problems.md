# tsibble

<details>

* Version: 0.9.2
* Source code: https://github.com/cran/tsibble
* URL: https://tsibble.tidyverts.org
* BugReports: https://github.com/tidyverts/tsibble/issues
* Date/Publication: 2020-07-24 10:40:02 UTC
* Number of recursive dependencies: 92

Run `cloud_details(, "tsibble")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Expected class: dplyr_error
      Actual class:   rlang_error/error/condition
      Message:        Problem with `summarise()` input `.gaps`.
      ✖ has_length(.name, 3) is not TRUE
      ℹ Input `.gaps` is `tbl_gaps(date, idx_full, .name = .name)`.
      ℹ The error occurred in group 1: group = "b".
      Backtrace:
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 736 | SKIPPED: 2 | WARNINGS: 0 | FAILED: 2 ]
      1. Failure: count_gaps(.full = ) (@test-gaps.R#166) 
      2. Failure: count_gaps(.full = ) (@test-gaps.R#167) 
      
      Error: testthat unit tests failed
      Execution halted
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

# yardstick

<details>

* Version: 0.0.7
* Source code: https://github.com/cran/yardstick
* URL: https://github.com/tidymodels/yardstick, https://yardstick.tidymodels.org
* BugReports: https://github.com/tidymodels/yardstick/issues
* Date/Publication: 2020-07-13 16:10:02 UTC
* Number of recursive dependencies: 83

Run `cloud_details(, "yardstick")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 569 | SKIPPED: 1 | WARNINGS: 0 | FAILED: 19 ]
      1. Failure: `truth` should be factor (@test-error-handling.R#11) 
      2. Failure: At least 2 levels in truth (@test-error-handling.R#19) 
      3. Failure: Single character values are caught with correct errors (@test-error-handling.R#27) 
      4. Failure: Bad unquoted input is caught (@test-error-handling.R#36) 
      5. Failure: Non-allowed estimator (@test-error-handling.R#46) 
      6. Failure: Bad estimator + truth combination (@test-error-handling.R#54) 
      7. Failure: Bad estimator type (@test-error-handling.R#62) 
      8. Failure: Bad estimator type (@test-error-handling.R#68) 
      9. Failure: Numeric matrix in numeric metric (@test-error-handling.R#76) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

