# cubelyr

<details>

* Version: 1.0.0
* GitHub: https://github.com/hadley/cubelyr
* Source code: https://github.com/cran/cubelyr
* Date/Publication: 2020-02-29 13:00:02 UTC
* Number of recursive dependencies: 48

Run `cloud_details(, "cubelyr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    D: long [dbl, 24]
    D: month [int, 12]
    D: year [int, 1]
    M: cloudhigh [dbl[,24,12,1]]
    M: cloudlow [dbl[,24,12,1]]
    M: cloudmid [dbl[,24,12,1]]
    M: ozone [dbl[,24,12,1]]
    M: pressure [dbl[,24,12,1]]
    M: surftemp [dbl[,24,12,1]]
    M: temperature [dbl[,24,12,1]]
    > # Each component can only refer to one dimensions, ensuring that you always
    > # create a rectangular subset
    > ## Not run: filter(nasa, lat > long)
    > 
    > # Arrange is meaningless for tbl_cubes
    > 
    > by_loc <- group_by(nasa, lat, long)
    Error in UseMethod("ungroup") : 
      no applicable method for 'ungroup' applied to an object of class "tbl_cube"
    Calls: group_by ... group_by.tbl_cube -> <Anonymous> -> add_computed_columns -> ungroup
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Backtrace:
          █
       1. ├─dplyr::group_by(nasa, month) test-cube.R:121:2
       2. └─cubelyr:::group_by.tbl_cube(nasa, month)
       3.   └─dplyr::group_by_prepare(.data, ...)
       4.     ├─dplyr:::add_computed_columns(ungroup(.data), new_groups, "group_by")
       5.     └─dplyr::ungroup(.data)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      ERROR (test-cube.R:94:3): summarise works with single group
      ERROR (test-cube.R:121:3): group_vars() returns variables
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 26 ]
      Error: Test failures
      Execution halted
    ```

