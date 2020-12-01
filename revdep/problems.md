# alfred

<details>

* Version: 0.1.8
* GitHub: https://github.com/onnokleen/alfred
* Source code: https://github.com/cran/alfred
* Date/Publication: 2020-09-11 14:50:03 UTC
* Number of recursive dependencies: 77

Run `cloud_details(, "alfred")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘alfred-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: get_alfred_series
    > ### Title: Accessing ALFRED
    > ### Aliases: get_alfred_series
    > ### Keywords: alfred
    > 
    > ### ** Examples
    > 
    > ## Not run: 
    > ##D     get_alfred_series("INDPRO", "indpro")
    > ##D     
    > ## End(Not run)
    > get_alfred_series("INDPRO", "indpro", realtime_start = "2008-10-31", realtime_end = "2009-10-31")
    Error in get_alfred_series("INDPRO", "indpro", realtime_start = "2008-10-31",  : 
      Download of specified time-series failed - did you misspell the identifier?
    Execution halted
    ```

# SCORPIUS

<details>

* Version: 1.0.7
* GitHub: https://github.com/rcannood/SCORPIUS
* Source code: https://github.com/cran/SCORPIUS
* Date/Publication: 2020-05-11 11:00:06 UTC
* Number of recursive dependencies: 103

Run `cloud_details(, "SCORPIUS")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       8.     │   └─base::eval(mc, parent.frame())
       9.     └─mclust::meEEE(...)
      
      ── Skip (test-sparse.R:3:1): (code run outside of `test_that()`) ───────────────
      Reason: On CRAN
      
      ── Skipped tests  ──────────────────────────────────────────────────────────────
      ● On CRAN (1)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      ERROR (test-extract_modules.R:15:3): With generated data
      
      [ FAIL 1 | WARN 0 | SKIP 1 | PASS 182 ]
      Error: Test failures
      Execution halted
    ```

# TextMiningGUI

<details>

* Version: 0.1
* GitHub: NA
* Source code: https://github.com/cran/TextMiningGUI
* Date/Publication: 2020-08-11 15:20:08 UTC
* Number of recursive dependencies: 158

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

