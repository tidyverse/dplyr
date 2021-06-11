# imfr

<details>

* Version: 0.1.9.1
* GitHub: https://github.com/christophergandrud/imfr
* Source code: https://github.com/cran/imfr
* Date/Publication: 2020-10-03 06:20:02 UTC
* Number of recursive dependencies: 41

Run `cloud_details(, "imfr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        3. │   ├─testthat:::.capture(...)
        4. │   │ └─base::withCallingHandlers(...)
        5. │   └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
        6. ├─imfr::imf_data(...)
        7. │ └─imfr:::imf_data_one(...)
        8. │   └─imfr:::download_parse(URL)
        9. │     ├─`%>%`(...)
       10. │     └─httr::RETRY("GET", URL, user_agent(""), progress(), times = times)
       11. └─httr::content(., type = "text", encoding = "UTF-8")
       12.   ├─base::stopifnot(is.response(x))
       13.   └─httr:::is.response(x)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 1 ]
      Error: Test failures
      Execution halted
    ```

