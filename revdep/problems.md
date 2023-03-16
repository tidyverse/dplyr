# dm

<details>

* Version: 1.0.4
* GitHub: https://github.com/cynkra/dm
* Source code: https://github.com/cran/dm
* Date/Publication: 2023-02-11 19:30:02 UTC
* Number of recursive dependencies: 157

Run `revdepcheck::cloud_details(, "dm")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      • only works on `postgres`, `mssql`, `sqlite` (1)
      • only works on `postgres`, `sqlite`, `mssql`, `maria` (1)
      • only works on `sqlite` (1)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-flatten.R:315:5'): tests with 'bad_dm' work ──────────────────
      `expect_equivalent_tbl(...)` did not throw the expected warning.
      ── Failure ('test-flatten.R:372:5'): tests with 'bad_dm' work (2) ──────────────
      `expect_equivalent_tbl(...)` did not throw the expected warning.
      ── Failure ('test-flatten.R:419:5'): tests with 'bad_dm' work (3) ──────────────
      `expect_equivalent_tbl(...)` did not throw the expected warning.
      
      [ FAIL 3 | WARN 222 | SKIP 210 | PASS 1336 ]
      Error: Test failures
      Execution halted
    ```

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

# missCompare

<details>

* Version: 1.0.3
* GitHub: https://github.com/Tirgit/missCompare
* Source code: https://github.com/cran/missCompare
* Date/Publication: 2020-12-01 08:50:03 UTC
* Number of recursive dependencies: 193

Run `revdepcheck::cloud_details(, "missCompare")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        8. └─missCompare::impute_simulated(...)
        9.   ├─base::as.data.frame(test_aregImpute(sim$Simulated_matrix, list = res))
       10.   └─missCompare::test_aregImpute(sim$Simulated_matrix, list = res)
       11.     ├─utils::capture.output(results <- lapply(list, aregImpute_imp))
       12.     │ └─base::withVisible(...elt(i))
       13.     └─base::lapply(list, aregImpute_imp)
       14.       └─missCompare (local) FUN(X[[i]], ...)
       15.         └─Hmisc::aregImpute(...)
       16.           └─Hmisc::areg(...)
       17.             └─Hmisc::aregTran(x[, i], xtype[i], nk)
       18.               └─Hmisc::rcspline.eval(z, knots = parms, nk = nk, inclx = TRUE)
      
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 35 ]
      Error: Test failures
      Execution halted
    ```

# rapbase

<details>

* Version: 1.24.0
* GitHub: https://github.com/Rapporteket/rapbase
* Source code: https://github.com/cran/rapbase
* Date/Publication: 2023-02-27 10:22:31 UTC
* Number of recursive dependencies: 110

Run `revdepcheck::cloud_details(, "rapbase")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-github.R:6:3'): contributors are provided ────────────────────
      class(getGithub("contributors", "rapbase")) not equal to "character".
      1/1 mismatches
      x[1]: "NULL"
      y[1]: "character"
      ── Failure ('test-github.R:10:3'): key can be provided ─────────────────────────
      grepl("ssh-rsa", getGithub("keys", "areedv")) is not TRUE
      
      `actual`:       
      `expected`: TRUE
      
      [ FAIL 2 | WARN 0 | SKIP 37 | PASS 246 ]
      Error: Test failures
      Execution halted
    ```

