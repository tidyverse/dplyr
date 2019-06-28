# cocktailApp

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/cocktailApp
* URL: https://github.com/shabbychef/cocktailApp
* BugReports: https://github.com/shabbychef/cocktailApp/issues
* Date/Publication: 2018-08-19 16:40:02 UTC
* Number of recursive dependencies: 61

Run `revdep_details(,"cocktailApp")` for more info

</details>

## Newly broken

*   R CMD check timed out
    

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 14661 marked UTF-8 strings
    ```

# creditmodel

<details>

* Version: 1.1.0
* Source code: https://github.com/cran/creditmodel
* URL: https://github.com/FanHansen/creditmodel
* BugReports: https://github.com/FanHansen/creditmodel/issues
* Date/Publication: 2019-05-18 23:10:24 UTC
* Number of recursive dependencies: 106

Run `revdep_details(,"creditmodel")` for more info

</details>

## Newly broken

*   R CMD check timed out
    

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.0Mb
      sub-directories of 1Mb or more:
        data   4.3Mb
    ```

# crplyr

<details>

* Version: 0.3.1
* Source code: https://github.com/cran/crplyr
* URL: https://crunch.io/r/crplyr/, https://github.com/Crunch-io/crplyr
* BugReports: https://github.com/Crunch-io/crplyr/issues
* Date/Publication: 2019-06-06 23:30:07 UTC
* Number of recursive dependencies: 106

Run `revdep_details(,"crplyr")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      The following objects are masked from 'package:base':
      
          intersect, setdiff, setequal, union
      
      [31m──[39m [31m1. Failure: Grouping helpers work on CrunchDatasets (@test-group-by.R#78) [39m [31m─────────────────────────────────────[39m
      tbl_vars(ds) not identical to names(ds).
      Classes differ: dplyr_sel_vars/character is not character
      
      ══ testthat results  ══════════════════════════════════════════════════════════════════════════════════════════════
      OK: 158 SKIPPED: 23 WARNINGS: 0 FAILED: 1
      1. Failure: Grouping helpers work on CrunchDatasets (@test-group-by.R#78) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# infer

<details>

* Version: 0.4.0.1
* Source code: https://github.com/cran/infer
* URL: https://github.com/tidymodels/infer
* BugReports: https://github.com/tidymodels/infer/issues
* Date/Publication: 2019-04-22 06:58:08 UTC
* Number of recursive dependencies: 89

Run `revdep_details(,"infer")` for more info

</details>

## Newly broken

*   R CMD check timed out
    

# perturbatr

<details>

* Version: 1.2.1
* Source code: https://github.com/cran/perturbatr
* URL: https://github.com/cbg-ethz/perturbatr
* BugReports: https://github.com/cbg-ethz/perturbatr/issues
* Date/Publication: 2019-01-04
* Number of recursive dependencies: 81

Run `revdep_details(,"perturbatr")` for more info

</details>

## Newly broken

*   R CMD check timed out
    

# portalr

<details>

* Version: 0.2.5
* Source code: https://github.com/cran/portalr
* URL: https://weecology.github.io/portalr/, https://github.com/weecology/portalr
* BugReports: https://github.com/weecology/portalr/issues
* Date/Publication: 2019-06-22 04:40:04 UTC
* Number of recursive dependencies: 127

Run `revdep_details(,"portalr")` for more info

</details>

## Newly broken

*   R CMD check timed out
    

# RCMIP5

<details>

* Version: 1.2.0
* Source code: https://github.com/cran/RCMIP5
* Date/Publication: 2016-07-30 18:53:27
* Number of recursive dependencies: 52

Run `revdep_details(,"RCMIP5")` for more info

</details>

## Newly broken

*   R CMD check timed out
    

# rcv

<details>

* Version: 0.2.1
* Source code: https://github.com/cran/rcv
* URL: https://github.com/ds-elections/rcv
* BugReports: https://github.com/ds-elections/rcv/issues
* Date/Publication: 2017-08-11 08:11:33 UTC
* Number of recursive dependencies: 46

Run `revdep_details(,"rcv")` for more info

</details>

## Newly broken

*   R CMD check timed out
    

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        data   5.0Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 6543 marked UTF-8 strings
    ```

# strapgod

<details>

* Version: 0.0.2
* Source code: https://github.com/cran/strapgod
* URL: https://github.com/DavisVaughan/strapgod
* BugReports: https://github.com/DavisVaughan/strapgod/issues
* Date/Publication: 2019-05-16 15:30:04 UTC
* Number of recursive dependencies: 54

Run `revdep_details(,"strapgod")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      [31m──[39m [31m1. Failure: group_map() (@test-dplyr-group-funs.R#64) [39m [31m─────────────────────────────────────────────────────────[39m
      group_map(x, ~.x) not equal to group_split(x, keep = FALSE).
      Attributes: < target is NULL, current is list >
      
      [31m──[39m [31m2. Failure: group_vars() returns virtual groups (@test-dplyr-group-funs.R#195) [39m [31m────────────────────────────────[39m
      tbl_vars(x) not equal to colnames(iris).
      Classes differ: dplyr_sel_vars/character is not character
      
      ══ testthat results  ══════════════════════════════════════════════════════════════════════════════════════════════
      OK: 151 SKIPPED: 0 WARNINGS: 0 FAILED: 2
      1. Failure: group_map() (@test-dplyr-group-funs.R#64) 
      2. Failure: group_vars() returns virtual groups (@test-dplyr-group-funs.R#195) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

