# banR

<details>

* Version: 0.2.2
* GitHub: https://github.com/joelgombin/banR
* Source code: https://github.com/cran/banR
* Date/Publication: 2020-05-11 09:10:12 UTC
* Number of recursive dependencies: 60

Run `cloud_details(, "banR")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Error: The API sent back an error 502
      Backtrace:
          █
       1. ├─testthat::expect_true(...) test_geocodetbl.R:77:4
       2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
       3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
       4. ├─dplyr::all_equal(...)
       5. │ └─dplyr:::equal_data_frame(...)
       6. │   └─dplyr:::is_compatible_data_frame(...)
       7. │     └─base::ncol(x)
       8. └─banR::geocode_tbl(tbl = table_check, adresse = num_voie, code_postal = cp)
      
      [ FAIL 4 | WARN 1 | SKIP 0 | PASS 3 ]
      Error: Test failures
      Execution halted
    ```

# DeLorean

<details>

* Version: 1.5.0
* GitHub: NA
* Source code: https://github.com/cran/DeLorean
* Date/Publication: 2018-10-17 22:30:16 UTC
* Number of recursive dependencies: 123

Run `cloud_details(, "DeLorean")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is 151.5Mb
      sub-directories of 1Mb or more:
        libs  148.8Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘lattice’
      All declared Imports should be used.
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

## Newly fixed

*   checking whether package ‘DeLorean’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/DeLorean/old/DeLorean.Rcheck/00install.out’ for details.
    ```

# imfr

<details>

* Version: 0.1.9.1
* GitHub: https://github.com/christophergandrud/imfr
* Source code: https://github.com/cran/imfr
* Date/Publication: 2020-10-03 06:20:02 UTC
* Number of recursive dependencies: 44

Run `cloud_details(, "imfr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Downloading: 110 kB     
    Downloading: 110 kB     
    Downloading: 120 kB     
    Downloading: 120 kB     
    Downloading: 120 kB     
    Downloading: 120 kB     
    Downloading: 120 kB     
    Downloading: 120 kB     
    Downloading: 120 kB     
    Downloading: 120 kB     
    Downloading: 140 kB     
    Downloading: 140 kB     
    Downloading: 150 kB     
    Downloading: 150 kB     
    Downloading: 150 kB     
    Downloading: 150 kB     
    Downloading: 150 kB     
    Downloading: 150 kB     
    Downloading: 150 kB     
    Downloading: 150 kB     Error: Unable to download series.
    Execution halted
    ```

## In both

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── Error (test-next.R:11:1): (code run outside of `test_that()`) ───────────────
      Error: Unable to download series.
      Backtrace:
          █
       1. ├─testthat::expect_equal(...) test-next.R:11:0
       2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
       3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
       4. ├─base::ncol(...)
       5. └─imfr::imf_data(...)
       6.   └─imfr:::imf_data_one(...)
       7.     └─imfr:::download_parse(URL)
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

