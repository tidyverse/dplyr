# chunked

<details>

* Version: 0.5.1
* GitHub: https://github.com/edwindj/chunked
* Source code: https://github.com/cran/chunked
* Date/Publication: 2020-11-03 06:40:19 UTC
* Number of recursive dependencies: 51

Run `cloud_details(, "chunked")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        4. │ └─testthat:::quasi_capture(...)
        5. │   ├─testthat .capture(...)
        6. │   │ └─testthat::capture_output_lines(code, print, width = width)
        7. │   │   └─testthat:::eval_with_output(code, print = print, width = width)
        8. │   │     ├─withr::with_output_sink(path, withVisible(code))
        9. │   │     │ └─base::force(code)
       10. │   │     └─base::withVisible(code)
       11. │   └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
       12. ├─base::print(tbl_iris)
       13. └─chunked:::print.chunkwise(tbl_iris)
       14.   └─base::print(trunc_mat(h, n = n, width = width))
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 43 ]
      Error: Test failures
      Execution halted
    ```

*   checking R code for possible problems ... NOTE
    ```
    print.chunkwise: no visible global function definition for ‘trunc_mat’
    Undefined global functions or variables:
      trunc_mat
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# googlesheets

<details>

* Version: 0.3.0
* GitHub: https://github.com/jennybc/googlesheets
* Source code: https://github.com/cran/googlesheets
* Date/Publication: 2018-06-29 04:38:09 UTC
* Number of recursive dependencies: 85

Run `cloud_details(, "googlesheets")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... WARNING
    ```
    Namespace in Imports field not imported from: ‘tibble’
      All declared Imports should be used.
    Missing or unexported object: ‘dplyr::data_frame_’
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tibble’
      All declared Imports should be used.
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# photobiology

<details>

* Version: 0.10.8
* GitHub: https://github.com/aphalo/photobiology
* Source code: https://github.com/cran/photobiology
* Date/Publication: 2021-12-08 11:50:02 UTC
* Number of recursive dependencies: 60

Run `cloud_details(, "photobiology")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘photobiology-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: A.illuminant.spct
    > ### Title: CIE A illuminant data
    > ### Aliases: A.illuminant.spct
    > ### Keywords: datasets
    > 
    > ### ** Examples
    > 
    > A.illuminant.spct
    Object: source_spct [97 x 2]
    Wavelength range 300 to 780 nm, step 5 nm 
    Label: CIE A standard illuminant, normalized to one at 560 nm 
    Time unit 1s
    Spectral data normalized to 1 at 560 nm 
    
    Error: 'trunc_mat' is not an exported object from 'namespace:dplyr'
    Execution halted
    ```

*   checking dependencies in R code ... WARNING
    ```
    Missing or unexported object: ‘dplyr::trunc_mat’
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘userguide-1-radiation.Rmd’ using rmarkdown
    News at https://www.r4photobiology.info/
    
    Attaching package: 'lubridate'
    
    The following objects are masked from 'package:base':
    
        date, intersect, setdiff, union
    
    ...
    
        date, intersect, setdiff, union
    
    --- finished re-building ‘userguide-2-astronomy.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘userguide-1-radiation.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# photobiologyFilters

<details>

* Version: 0.5.2
* GitHub: NA
* Source code: https://github.com/cran/photobiologyFilters
* Date/Publication: 2020-10-05 07:10:06 UTC
* Number of recursive dependencies: 61

Run `cloud_details(, "photobiologyFilters")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘photobiologyFilters-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: acetate_filters
    > ### Title: Spectral data for filters made from different materials
    > ### Aliases: acetate_filters materials acrylic_filters
    > ###   polycarbonate_filters plexiglas_filters polystyrene_filters
    > ###   polyester_filters polyvynil_chloride_filters optical_glass_filters
    > ###   plastic_film_filters plastic_films plastic_sheet_filters
    > ###   plastic_sheets
    ...
    Object: filter_mspct [9 x 1]
    --- Member: Evonik_Cherry_3C01_GT ---
    Object: filter_spct [911 x 2]
    Wavelength range 190 to 1100 nm, step 1 nm 
    Label: Poly(methyl methacrylate) (PMMA) 'acrylic' sheet; Plexiglas 'Cherry 3C01 GT'; 0.002 m thick; new; from Evonik Industries, Germany 
    Transmittance of type 'total'
    Rfr (/1): 0.065, thickness (mm): 3, attenuation mode: absorption.
    
    Error: 'trunc_mat' is not an exported object from 'namespace:dplyr'
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘user-guide.Rmd’ using rmarkdown
    Quitting from lines 259-260 (user-guide.Rmd) 
    Error: processing vignette 'user-guide.Rmd' failed with diagnostics:
    'trunc_mat' is not an exported object from 'namespace:dplyr'
    --- failed re-building ‘user-guide.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘user-guide.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# photobiologyInOut

<details>

* Version: 0.4.23
* GitHub: https://github.com/aphalo/photobiologyinout
* Source code: https://github.com/cran/photobiologyInOut
* Date/Publication: 2021-10-11 04:10:01 UTC
* Number of recursive dependencies: 117

Run `cloud_details(, "photobiologyInOut")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘photobiologyInOut-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: colorSpec2mspct
    > ### Title: Convert 'colorSpec::colorSpec' objects
    > ### Aliases: colorSpec2mspct as.source_spct.colorSpec
    > ###   as.source_mspct.colorSpec as.response_spct.colorSpec
    > ###   as.response_mspct.colorSpec as.filter_spct.colorSpec
    > ###   as.filter_mspct.colorSpec as.reflector_spct.colorSpec
    > ###   as.reflector_mspct.colorSpec as.chroma_mspct.colorSpec colorSpec2spct
    ...
    The following object is masked from ‘package:photobiology’:
    
        normalize
    
    Object: source_spct [93 x 2]
    Wavelength range 320 to 780 nm, step 5 nm 
    Time unit 1s
    
    Error: 'trunc_mat' is not an exported object from 'namespace:dplyr'
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘user-guide.Rmd’ using rmarkdown
    News at https://www.r4photobiology.info/
    
    Attaching package: 'lubridate'
    
    The following objects are masked from 'package:base':
    
        date, intersect, setdiff, union
    
    ...
    Quitting from lines 525-530 (user-guide.Rmd) 
    Error: processing vignette 'user-guide.Rmd' failed with diagnostics:
    'trunc_mat' is not an exported object from 'namespace:dplyr'
    --- failed re-building ‘user-guide.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘user-guide.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘lazyeval’
      All declared Imports should be used.
    ```

# photobiologyLamps

<details>

* Version: 0.4.3
* GitHub: NA
* Source code: https://github.com/cran/photobiologyLamps
* Date/Publication: 2019-06-14 22:14:35 UTC
* Number of recursive dependencies: 61

Run `cloud_details(, "photobiologyLamps")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘photobiologyLamps-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: incandescent_lamps
    > ### Title: Spectral data for Lamps of different types
    > ### Aliases: incandescent_lamps types led_lamps mercury_lamps
    > ###   multimetal_lamps sodium_lamps xenon_lamps
    > ### Keywords: datasets
    > 
    > ### ** Examples
    ...
    --- Member: osram.super.vialox ---
    Object: source_spct [301 x 2]
    Wavelength range 300 to 900 nm, step 2 nm 
    Label: File: Osram.Super.Vialox.PRN 
    Measured on 0-10-17 12:14:11 UTC 
    Time unit 1s
    Spectral data normalized to 1 at 820 nm 
    
    Error: 'trunc_mat' is not an exported object from 'namespace:dplyr'
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘user-guide.Rmd’ using rmarkdown
    News at https://www.r4photobiology.info/
    ggspectra: default axis labels updated
    Quitting from lines 67-68 (user-guide.Rmd) 
    Error: processing vignette 'user-guide.Rmd' failed with diagnostics:
    'trunc_mat' is not an exported object from 'namespace:dplyr'
    --- failed re-building ‘user-guide.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘user-guide.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘photobiologyLEDs’
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked Latin-1 string
    ```

# photobiologyLEDs

<details>

* Version: 0.4.3-1
* GitHub: NA
* Source code: https://github.com/cran/photobiologyLEDs
* Date/Publication: 2018-01-14 15:47:06 UTC
* Number of recursive dependencies: 54

Run `cloud_details(, "photobiologyLEDs")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘photobiologyLEDs-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: leds.mspct
    > ### Title: Spectral irradiance for diverse LEDs
    > ### Aliases: leds.mspct
    > ### Keywords: datasets
    > 
    > ### ** Examples
    > 
    ...
    > leds.mspct$white
    Object: source_spct [1,425 x 2]
    Wavelength range 250.05 to 899.78 nm, step 0.43 to 0.48 nm 
    Label: White LED from hardware store
    supplier Clas Ohlsson, Finland 
    Time unit 1s
    Spectral data normalized to 1 at 453.47 nm 
    
    Error: 'trunc_mat' is not an exported object from 'namespace:dplyr'
    Execution halted
    ```

# photobiologySensors

<details>

* Version: 0.5.0
* GitHub: NA
* Source code: https://github.com/cran/photobiologySensors
* Date/Publication: 2020-10-05 07:10:03 UTC
* Number of recursive dependencies: 61

Run `cloud_details(, "photobiologySensors")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘photobiologySensors-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: uv_sensors
    > ### Title: Sensors responsive to different wavebands
    > ### Aliases: uv_sensors 'sensors by waveband' uvc_sensors uvb_sensors
    > ###   erythemal_sensors uva_sensors par_sensors vis_sensors
    > ###   photometric_sensors shortwave_sensors pyranometer_sensors red_sensors
    > ###   far_red_sensors blue_sensors multichannel_sensors
    > ### Keywords: datasets
    ...
    > 
    > # select PAR sensors
    > sensors.mspct[par_sensors]
    $Skye_SKP215
    Object: response_spct [736 x 2]
    Wavelength range 382.04143 to 750.07094 nm, step 0.5007204 nm 
    Time unit 1s
    
    Error: 'trunc_mat' is not an exported object from 'namespace:dplyr'
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘user-guide.Rmd’ using rmarkdown
    News at https://www.r4photobiology.info/
    ggspectra: default axis labels updated
    Quitting from lines 63-64 (user-guide.Rmd) 
    Error: processing vignette 'user-guide.Rmd' failed with diagnostics:
    'trunc_mat' is not an exported object from 'namespace:dplyr'
    --- failed re-building ‘user-guide.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘user-guide.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# photobiologySun

<details>

* Version: 0.4.1
* GitHub: NA
* Source code: https://github.com/cran/photobiologySun
* Date/Publication: 2019-03-27 22:20:03 UTC
* Number of recursive dependencies: 61

Run `cloud_details(, "photobiologySun")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘user-guide.Rmd’ using rmarkdown
    News at https://www.r4photobiology.info/
    
    Attaching package: 'lubridate'
    
    The following objects are masked from 'package:base':
    
        date, intersect, setdiff, union
    ...
    Quitting from lines 57-58 (user-guide.Rmd) 
    Error: processing vignette 'user-guide.Rmd' failed with diagnostics:
    'trunc_mat' is not an exported object from 'namespace:dplyr'
    --- failed re-building ‘user-guide.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘user-guide.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# photobiologyWavebands

<details>

* Version: 0.4.5
* GitHub: NA
* Source code: https://github.com/cran/photobiologyWavebands
* Date/Publication: 2022-01-07 19:52:40 UTC
* Number of recursive dependencies: 43

Run `cloud_details(, "photobiologyWavebands")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘r4p-introduction.Rmd’ using rmarkdown
    --- finished re-building ‘r4p-introduction.Rmd’
    
    --- re-building ‘user-guide.Rmd’ using rmarkdown
    Quitting from lines 222-223 (user-guide.Rmd) 
    Error: processing vignette 'user-guide.Rmd' failed with diagnostics:
    'trunc_mat' is not an exported object from 'namespace:dplyr'
    --- failed re-building ‘user-guide.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘user-guide.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# saeSim

<details>

* Version: 0.10.0
* GitHub: https://github.com/wahani/saeSim
* Source code: https://github.com/cran/saeSim
* Date/Publication: 2019-03-28 12:50:03 UTC
* Number of recursive dependencies: 94

Run `cloud_details(, "saeSim")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘saeSim-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: agg_all
    > ### Title: Aggregation function
    > ### Aliases: agg_all
    > 
    > ### ** Examples
    > 
    > sim_base() %>% sim_gen_x() %>% sim_gen_e() %>% sim_agg(agg_all())
    ...
    Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.
    Warning: `group_by_()` was deprecated in dplyr 0.7.0.
    Please use `group_by()` instead.
    See vignette('programming') for more help
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.
    Error in trunc_mat(dat, n = 6, width = NULL) : 
      could not find function "trunc_mat"
    Calls: <Anonymous> -> <Anonymous> -> print
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘test-all.R’
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      
      [ FAIL 1 | WARN 6 | SKIP 0 | PASS 133 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error (test-sim_setup.R:14:3): methods equal ────────────────────────────────
      Error in `trunc_mat(dat, n = 6, width = NULL)`: could not find function "trunc_mat"
      Backtrace:
          ▆
       1. ├─methods::show(setup) at test-sim_setup.R:14:2
       2. └─saeSim::show(setup)
       3.   └─base::print(trunc_mat(dat, n = 6, width = NULL))
      
      [ FAIL 1 | WARN 6 | SKIP 0 | PASS 133 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Introduction.Rmd’ using rmarkdown
    Quitting from lines 31-39 (Introduction.Rmd) 
    Error: processing vignette 'Introduction.Rmd' failed with diagnostics:
    could not find function "trunc_mat"
    --- failed re-building ‘Introduction.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Introduction.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

*   checking R code for possible problems ... NOTE
    ```
    show,sim_setup: no visible global function definition for ‘trunc_mat’
    Undefined global functions or variables:
      trunc_mat
    ```

