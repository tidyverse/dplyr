# MXM

<details>

* Version: 1.5.0
* GitHub: NA
* Source code: https://github.com/cran/MXM
* Date/Publication: 2021-01-09 02:10:03 UTC
* Number of recursive dependencies: 124

Run `cloud_details(, "MXM")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    warning: 
    warning: 
    warning: 
    warning: 
    warning: solve(): system seems singular; attempting approx solutionsolve(): system seems singular; attempting approx solution
    warning: solve(): system seems singular; attempting approx solution
    
    warning: solve(): system seems singular; attempting approx solution
    warning: 
    warning: solve(): system seems singular; attempting approx solutionsolve(): system seems singular; attempting approx solutionsolve(): system seems singular; attempting approx solution
    
    solve(): system seems singular; attempting approx solution
    Infinity foundInfinity found
    
    warning: solve(): system seems singular; attempting approx solution
    Infinity foundError: C stack usage  856407293708 is too close to the limit
    Execution halted
    solve(): system seems singular; attempting approx solution
    Error: C stack usage  856344354572 is too close to the limit
    Fatal error: error during cleanup
    
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘knitr’
      All declared Imports should be used.
    ```

# rattle

<details>

* Version: 5.4.0
* GitHub: NA
* Source code: https://github.com/cran/rattle
* Date/Publication: 2020-05-23 11:20:03 UTC
* Number of recursive dependencies: 216

Run `cloud_details(, "rattle")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... NOTE
    ```
    
    (R:24151): Gtk-WARNING **: 10:06:11.564: gtk_disable_setlocale() must be called before gtk_init()
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘rggobi’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  8.5Mb
      sub-directories of 1Mb or more:
        data   3.6Mb
        etc    1.9Mb
        po     1.2Mb
    ```

