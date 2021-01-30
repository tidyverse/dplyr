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
    
    (R:25552): Gtk-WARNING **: 08:59:52.265: gtk_disable_setlocale() must be called before gtk_init()
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

# stacks

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/stacks
* Date/Publication: 2020-11-23 08:40:02 UTC
* Number of recursive dependencies: 134

Run `cloud_details(, "stacks")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        8. ├─stacks:::multi_net_helper(.)
        9. │ └─`%>%`(...)
       10. ├─dplyr::ungroup(.)
       11. ├─dplyr::mutate(...)
       12. ├─dplyr:::mutate.data.frame(...)
       13. │ └─dplyr:::mutate_cols(.data, ...)
       14. │   ├─base::withCallingHandlers(...)
       15. │   └─mask$eval_all_mutate(quo)
       16. ├─(structure(function (..., .x = ..1, .y = ..2, . = ..1) ...
       17. └─base::.handleSimpleError(...)
       18.   └─dplyr:::h(simpleError(msg, call))
      
      [ FAIL 1 | WARN 0 | SKIP 35 | PASS 4 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘yardstick’
      All declared Imports should be used.
    ```

# TextMiningGUI

<details>

* Version: 0.2
* GitHub: NA
* Source code: https://github.com/cran/TextMiningGUI
* Date/Publication: 2020-12-07 17:40:06 UTC
* Number of recursive dependencies: 159

Run `cloud_details(, "TextMiningGUI")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘TextMiningGUI-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: TextMiningGUI
    > ### Title: TextMiningGUI
    > ### Aliases: TextMiningGUI
    > 
    > ### ** Examples
    > 
    > library(TextMiningGUI)
    > if(TextMiningGUI()){}
    Error in structure(.External(.C_dotTclObjv, objv), class = "tclObj") : 
      [tcl] invalid command name "toplevel".
    Calls: TextMiningGUI ... tktoplevel -> tkwidget -> tcl -> .Tcl.objv -> structure
    Execution halted
    ```

*   checking whether package ‘TextMiningGUI’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: loading Rplot failed
    See ‘/tmp/workdir/TextMiningGUI/new/TextMiningGUI.Rcheck/00install.out’ for details.
    ```

