# eiCompare

<details>

* Version: 3.0.4
* GitHub: https://github.com/RPVote/eiCompare
* Source code: https://github.com/cran/eiCompare
* Date/Publication: 2023-08-31 13:30:02 UTC
* Number of recursive dependencies: 146

Run `revdepcheck::cloud_details(, "eiCompare")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘bisg.Rmd’ using rmarkdown
    
    Quitting from lines 164-175 [unnamed-chunk-16] (bisg.Rmd)
    Error: processing vignette 'bisg.Rmd' failed with diagnostics:
    no applicable method for 'tbl_vars' applied to an object of class "NULL"
    --- failed re-building ‘bisg.Rmd’
    
    --- re-building ‘ei.Rmd’ using rmarkdown
    --- finished re-building ‘ei.Rmd’
    ...
    Quitting from lines 235-263 [performance_analysis] (performance_analysis.Rmd)
    Error: processing vignette 'performance_analysis.Rmd' failed with diagnostics:
    No columns selected for aggregation.
    --- failed re-building ‘performance_analysis.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘bisg.Rmd’ ‘performance_analysis.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

