# finreportr

<details>

* Version: 1.0.2
* GitHub: https://github.com/sewardlee337/finreportr
* Source code: https://github.com/cran/finreportr
* Date/Publication: 2020-06-13 06:10:02 UTC
* Number of recursive dependencies: 57

Run `cloud_details(, "finreportr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
        4. └─finreportr::CompanyInfo("GOOG")
        5.   ├─xml2::read_html(url)
        6.   └─xml2:::read_html.default(url)
        7.     ├─base::suppressWarnings(...)
        8.     │ └─base::withCallingHandlers(...)
        9.     ├─xml2::read_xml(x, encoding = encoding, ..., as_html = TRUE, options = options)
       10.     └─xml2:::read_xml.character(...)
       11.       └─xml2:::read_xml.connection(...)
       12.         ├─base::open(x, "rb")
       13.         └─base::open.connection(x, "rb")
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘finreportr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: CompanyInfo
    > ### Title: Acquire basic company information.
    > ### Aliases: CompanyInfo
    > 
    > ### ** Examples
    > 
    > CompanyInfo("GOOG")
    Error in open.connection(x, "rb") : HTTP error 403.
    Calls: CompanyInfo -> <Anonymous> -> read_html.default
    Execution halted
    ```

# SwimmeR

<details>

* Version: 0.10.0
* GitHub: NA
* Source code: https://github.com/cran/SwimmeR
* Date/Publication: 2021-06-02 15:30:02 UTC
* Number of recursive dependencies: 63

Run `cloud_details(, "SwimmeR")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       20. │   └─dplyr:::check_length_val(...)
       21. │     └─length_x %in% c(1L, n)
       22. ├─dplyr::mutate(...)
       23. ├─dplyr:::mutate.data.frame(...)
       24. │ └─dplyr:::mutate_cols(.data, ..., caller_env = caller_env())
       25. │   ├─base::withCallingHandlers(...)
       26. │   └─mask$eval_all_mutate(quo)
       27. └─dplyr::case_when(...)
       28.   └─dplyr:::replace_with(...)
       29.     └─dplyr:::check_type(val, x, name)
       30.       └─dplyr:::glubort(header, "must be {friendly_type_of(template)}, not {friendly_type_of(x)}.")
      
      [ FAIL 1 | WARN 1 | SKIP 13 | PASS 21 ]
      Error: Test failures
      Execution halted
    ```

# xray

<details>

* Version: 0.2
* GitHub: https://github.com/sicarul/xray
* Source code: https://github.com/cran/xray
* Date/Publication: 2017-12-08 05:15:59 UTC
* Number of recursive dependencies: 40

Run `cloud_details(, "xray")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘xray-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: anomalies
    > ### Title: Analyze a dataset and search for anomalies
    > ### Aliases: anomalies
    > 
    > ### ** Examples
    > 
    > 
    ...
     16. │ ├─dplyr::mutate(.tbl, !!!funs)
     17. │ └─dplyr:::mutate.data.frame(.tbl, !!!funs)
     18. │   └─dplyr:::mutate_cols(.data, ..., caller_env = caller_env())
     19. │     ├─base::withCallingHandlers(...)
     20. │     └─mask$eval_all_mutate(quo)
     21. └─dplyr::case_when(...)
     22.   └─dplyr:::validate_case_when_length(query, value, fs)
     23.     └─dplyr:::bad_calls(...)
     24.       └─dplyr:::glubort(fmt_calls(calls), ..., .envir = .envir)
    Execution halted
    ```

