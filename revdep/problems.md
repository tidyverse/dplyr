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

*   checking examples ... ERROR
    ```
    Running examples in ‘finreportr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: AnnualReports
    > ### Title: Acquire listing of company annual reports.
    > ### Aliases: AnnualReports
    > 
    > ### ** Examples
    > 
    > AnnualReports("TSLA")
    Error in open.connection(x, "rb") : HTTP error 403.
    Calls: AnnualReports -> <Anonymous> -> read_html.default
    Execution halted
    ```

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

# readabs

<details>

* Version: 0.4.9
* GitHub: https://github.com/mattcowgill/readabs
* Source code: https://github.com/cran/readabs
* Date/Publication: 2021-05-24 04:10:03 UTC
* Number of recursive dependencies: 83

Run `cloud_details(, "readabs")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘readabs-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: search_catalogues
    > ### Title: Search for ABS catalogues that match a string
    > ### Aliases: search_catalogues
    > 
    > ### ** Examples
    > 
    > 
    ...
      3. ├─dplyr::filter(...)
      4. ├─dplyr:::filter.data.frame(...)
      5. │ └─dplyr:::filter_rows(.data, ...)
      6. │   ├─base::withCallingHandlers(...)
      7. │   └─mask$eval_all_filter(dots, env_filter)
      8. ├─rlang:::grepl(string, heading, perl = TRUE, ignore.case = TRUE)
      9. ├─base::grepl(string, heading, perl = TRUE, ignore.case = TRUE)
     10. └─base::.handleSimpleError(...)
     11.   └─dplyr:::h(simpleError(msg, call))
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        1. ├─readabs::search_catalogues("labour") test-search_catalogues.R:4:2
        2. │ └─`%>%`(...)
        3. ├─dplyr::filter(...)
        4. ├─dplyr:::filter.data.frame(...)
        5. │ └─dplyr:::filter_rows(.data, ...)
        6. │   ├─base::withCallingHandlers(...)
        7. │   └─mask$eval_all_filter(dots, env_filter)
        8. ├─rlang:::grepl(string, heading, perl = TRUE, ignore.case = TRUE)
        9. ├─base::grepl(string, heading, perl = TRUE, ignore.case = TRUE)
       10. └─base::.handleSimpleError(...)
       11.   └─dplyr:::h(simpleError(msg, call))
      
      [ FAIL 1 | WARN 0 | SKIP 26 | PASS 52 ]
      Error: Test failures
      Execution halted
    ```

