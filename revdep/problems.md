# activAnalyzer (2.1.2)

* GitHub: https://github.com/pydemull/activAnalyzer
* Github mirror: https://github.com/cran/activAnalyzer
* Maintainer: Pierre-Yves de Müllenheim <pydemull@uco.fr>

Run `revdepcheck::cloud_details(, "activAnalyzer")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     > mydata <- prepare_dataset(data = file)
     > mydata_with_wear_marks <- mydata %>% mark_wear_time() %>% 
     + dplyr::filter(days == 2 & time >= hms::as_hms("14:00:00") & time <= hms::as_hms("15:00:00")) 
     frame is 90
     streamFrame is 30
     allowanceFrame is 2
     > mets <- compute_mets(
     +     data = mydata_with_wear_marks,
     +     equation = "Sasaki et al. (2011) [Adults]", 
     +     weight = 67, 
     +     sex = "male"
     +     )
     Error in `dplyr::case_when()`:
     ! `..1 (right)` must have size 1, not size 61.
     Backtrace:
         ▆
      1. ├─activAnalyzer::compute_mets(...)
      2. │ └─dplyr::case_when(...)
      3. │   └─dplyr:::vec_case_when(...)
      4. │     └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
      5. └─vctrs:::stop_assert_size(...)
      6.   └─vctrs:::stop_assert(...)
      7.     └─vctrs:::stop_vctrs(...)
      8.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
       Caused by error in `dplyr::case_when()`:
       ! `..1 (right)` must have size 1, not size 6900.
       ── Error ('test-recap_by_day.R:139:1'): Kilocalories are correctly summed when changing the period of the day considered for analysis
                 even with customized variable names ──
       <dplyr:::mutate_error/rlang_error/error/condition>
       Error in `dplyr::mutate(., SED = dplyr::if_else(.data[[col_axis]] < sed_cutpoint/cor_factor, 
           1, 0), LPA = dplyr::if_else(.data[[col_axis]] >= sed_cutpoint/cor_factor & 
           .data[[col_axis]] < mpa_cutpoint/cor_factor, 1, 0), MPA = dplyr::if_else(.data[[col_axis]] >= 
           mpa_cutpoint/cor_factor & .data[[col_axis]] < vpa_cutpoint/cor_factor, 
           1, 0), VPA = dplyr::if_else(.data[[col_axis]] >= vpa_cutpoint/cor_factor, 
           1, 0), METS = suppressMessages(compute_mets(data = data %>% 
           dplyr::mutate(axis1 = axis1 * cor_factor, vm = vm * cor_factor), 
           equation = equation, weight = weight, sex = sex)), kcal = dplyr::case_when(SED == 
           1 ~ bmr_kcal_min/cor_factor, equation == "Sasaki et al. (2011) [Adults]" | 
           equation == "Freedson et al. (1998) [Adults]" ~ METS * weight * 
           (1/60)/cor_factor, equation == "Santos-Lozano et al. (2013) [Adults]" | 
           equation == "Santos-Lozano et al. (2013) [Older adults]" ~ 
           METS * bmr_kcal_min/cor_factor), mets_hours_mvpa = dplyr::if_else(METS >= 
           3, METS * (1/60)/cor_factor, 0), )`: i In argument: `METS = suppressMessages(...)`.
       Caused by error in `dplyr::case_when()`:
       ! `..1 (right)` must have size 1, not size 6900.
       
       [ FAIL 14 | WARN 4 | SKIP 1 | PASS 156 ]
       Error: Test failures
       Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     Error(s) in re-building vignettes:
     --- re-building ‘activAnalyzer.Rmd’ using rmarkdown
     ```

## In both

*   checking installed package size ... NOTE
     ```
       installed size is  5.8Mb
       sub-directories of 1Mb or more:
         R         1.5Mb
         doc       1.0Mb
         extdata   2.0Mb
     ```

# admiral (1.3.1)

* GitHub: https://github.com/pharmaverse/admiral
* Github mirror: https://github.com/cran/admiral
* Maintainer: Ben Straub <ben.x.straub@gsk.com>

Run `revdepcheck::cloud_details(, "admiral")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     Backtrace:
          ▆
       1. ├─admiral::call_derivation(...)
       2. │ └─rlang::eval_tidy(call, env = eval_env)
       3. ├─admiral (local) `<fn>`(...)
       4. │ └─dataset %>% ...
       5. ├─dplyr::mutate(...)
       6. ├─dplyr:::mutate.data.frame(...)
       7. │ └─dplyr:::mutate_cols(.data, dplyr_quosures(...), by)
       8. │   ├─base::withCallingHandlers(...)
       9. │   └─dplyr:::mutate_col(dots[[i]], data, mask, new_columns)
      10. │     └─mask$eval_all_mutate(quo)
      11. │       └─dplyr (local) eval()
      12. ├─admiral::convert_dtc_to_dt(...)
      13. │ └─admiral::impute_dtc_dt(...)
      14. │   └─admiral:::get_imputation_targets(partial, date_imputation)
      15. │     └─admiral:::get_imputation_target_date(...)
      16. │       └─dplyr::case_when(...)
      17. │         └─dplyr:::vec_case_when(...)
      18. │           └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
      19. └─vctrs:::stop_assert_size(...)
      20.   └─vctrs:::stop_assert(...)
      21.     └─vctrs:::stop_vctrs(...)
      22.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
           day = "01"), date_imputation == "mid" ~ list(year = "xxxx", 
           month = "06", day = if_else(is.na(month), "30", "15")), date_imputation == 
           "last" ~ list(year = "9999", month = "12", day = "28"), TRUE ~ 
           list(year = "xxxx", month = str_sub(date_imputation, 1, 2), 
               day = str_sub(date_imputation, 4, 5)))`: `..1 (right)` must have size 1, not size 3.
       Backtrace:
            ▆
         1. ├─admiral::slice_derivation(...) at test-slice_derivation.R:120:3
         2. │ └─rlang::eval_tidy(call, env = eval_env)
         3. ├─admiral (local) `<fn>`(data, dtc = VSDTC, new_vars_prefix = "A", time_imputation = "first")
         4. │ └─admiral::convert_dtc_to_dtm(...)
         5. │   └─admiral::impute_dtc_dtm(...)
         6. │     └─admiral:::get_imputation_targets(partial, date_imputation, time_imputation)
         7. │       └─admiral:::get_imputation_target_date(...)
         8. │         └─dplyr::case_when(...)
         9. │           └─dplyr:::vec_case_when(...)
        10. │             └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
        11. └─vctrs:::stop_assert_size(...)
        12.   └─vctrs:::stop_assert(...)
        13.     └─vctrs:::stop_vctrs(...)
        14.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
       
       [ FAIL 77 | WARN 0 | SKIP 125 | PASS 722 ]
       Error: Test failures
       Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     ...
     ℹ In argument: `ADT = convert_dtc_to_dt(...)`.
     Caused by error in `case_when()`:
     ! `..1 (right)` must have size 1, not size 3.
     --- failed re-building ‘questionnaires.Rmd’
     
     --- re-building ‘visits_periods.Rmd’ using rmarkdown
     
     Quitting from visits_periods.Rmd:193-215 [unnamed-chunk-7]
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     NULL
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'visits_periods.Rmd' failed with diagnostics:
     ℹ In argument: `APERSDT = convert_dtc_to_dt(EXSTDTC)`.
     Caused by error in `case_when()`:
     ! `..1 (right)` must have size 1, not size 3.
     --- failed re-building ‘visits_periods.Rmd’
     
     SUMMARY: processing the following files failed:
       ‘adsl.Rmd’ ‘bds_exposure.Rmd’ ‘bds_finding.Rmd’ ‘bds_tte.Rmd’
       ‘higher_order.Rmd’ ‘imputation.Rmd’ ‘occds.Rmd’ ‘pk_adnca.Rmd’
       ‘questionnaires.Rmd’ ‘visits_periods.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

## In both

*   checking installed package size ... NOTE
     ```
       installed size is  5.2Mb
       sub-directories of 1Mb or more:
         doc    2.3Mb
         help   1.8Mb
     ```

*   checking data for non-ASCII characters ... NOTE
     ```
       Note: found 24 marked UTF-8 strings
     ```

# admiralmetabolic (0.2.0)

* GitHub: https://github.com/pharmaverse/admiralmetabolic
* Github mirror: https://github.com/cran/admiralmetabolic
* Maintainer: Anders Askeland <iakd@novonordisk.com>

Run `revdepcheck::cloud_details(, "admiralmetabolic")` for more info

## Newly broken

*   checking re-building of vignette outputs ... ERROR
     ```
     ...
     Caused by error in `case_when()`:
     ! `..1 (right)` must have size 1, not size 3.
     --- failed re-building ‘adlb.Rmd’
     
     --- re-building ‘admiralmetabolic.Rmd’ using rmarkdown
     --- finished re-building ‘admiralmetabolic.Rmd’
     
     --- re-building ‘advs.Rmd’ using rmarkdown
     
     Quitting from advs.Rmd:90-102 [unnamed-chunk-3]
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     NULL
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'advs.Rmd' failed with diagnostics:
     ℹ In argument: `ADT = convert_dtc_to_dt(...)`.
     Caused by error in `case_when()`:
     ! `..1 (right)` must have size 1, not size 3.
     --- failed re-building ‘advs.Rmd’
     
     SUMMARY: processing the following files failed:
       ‘adcoeq.Rmd’ ‘adlb.Rmd’ ‘advs.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

# admiralneuro (0.1.0)

* GitHub: https://github.com/pharmaverse/admiralneuro
* Github mirror: https://github.com/cran/admiralneuro
* Maintainer: Jian Wang <wang_jian_wj@lilly.com>

Run `revdepcheck::cloud_details(, "admiralneuro")` for more info

## Newly broken

*   checking re-building of vignette outputs ... ERROR
     ```
     Error(s) in re-building vignettes:
       ...
     --- re-building ‘admiralneuro.Rmd’ using rmarkdown
     --- finished re-building ‘admiralneuro.Rmd’
     
     --- re-building ‘adpet.Rmd’ using rmarkdown
     
     Quitting from adpet.Rmd:84-103 [unnamed-chunk-4]
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     NULL
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'adpet.Rmd' failed with diagnostics:
     ℹ In argument: `ADT = convert_dtc_to_dt(...)`.
     Caused by error in `case_when()`:
     ! `..1 (right)` must have size 1, not size 3.
     --- failed re-building ‘adpet.Rmd’
     
     SUMMARY: processing the following file failed:
       ‘adpet.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

# admiralonco (1.3.0)

* GitHub: https://github.com/pharmaverse/admiralonco
* Github mirror: https://github.com/cran/admiralonco
* Maintainer: Stefan Bundfuss <stefan.bundfuss@roche.com>

Run `revdepcheck::cloud_details(, "admiralonco")` for more info

## Newly broken

*   checking re-building of vignette outputs ... ERROR
     ```
     ...
     Error: processing vignette 'irecist.Rmd' failed with diagnostics:
     ℹ In argument: `ADT = convert_dtc_to_dt(...)`.
     Caused by error in `case_when()`:
     ! `..1 (right)` must have size 1, not size 3.
     --- failed re-building ‘irecist.Rmd’
     
     --- re-building ‘nactdt.Rmd’ using rmarkdown
     
     Quitting from nactdt.Rmd:75-85 [unnamed-chunk-3]
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     NULL
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'nactdt.Rmd' failed with diagnostics:
     ℹ In argument: `NACTDT = convert_dtc_to_dt(CMSTDTC)`.
     Caused by error in `case_when()`:
     ! `..1 (right)` must have size 1, not size 3.
     --- failed re-building ‘nactdt.Rmd’
     
     SUMMARY: processing the following files failed:
       ‘adrs.Rmd’ ‘adrs_basic.Rmd’ ‘adrs_gcig.Rmd’ ‘adrs_imwg.Rmd’
       ‘adrs_pcwg3.Rmd’ ‘adtr.Rmd’ ‘irecist.Rmd’ ‘nactdt.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

# admiralophtha (1.3.0)

* GitHub: https://github.com/pharmaverse/admiralophtha
* Github mirror: https://github.com/cran/admiralophtha
* Maintainer: Edoardo Mancini <edoardo.mancini@roche.com>

Run `revdepcheck::cloud_details(, "admiralophtha")` for more info

## Newly broken

*   checking re-building of vignette outputs ... ERROR
     ```
     ...
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'adbcva.Rmd' failed with diagnostics:
     ℹ In argument: `ADT = convert_dtc_to_dt(...)`.
     Caused by error in `case_when()`:
     ! `..1 (right)` must have size 1, not size 3.
     --- failed re-building ‘adbcva.Rmd’
     
     --- re-building ‘admiralophtha.Rmd’ using rmarkdown
     --- finished re-building ‘admiralophtha.Rmd’
     
     --- re-building ‘adoe.Rmd’ using rmarkdown
     --- finished re-building ‘adoe.Rmd’
     
     --- re-building ‘advfq.Rmd’ using rmarkdown
     --- finished re-building ‘advfq.Rmd’
     
     --- re-building ‘standards.Rmd’ using rmarkdown
     --- finished re-building ‘standards.Rmd’
     
     SUMMARY: processing the following file failed:
       ‘adbcva.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

# admiralpeds (0.2.1)

* GitHub: https://github.com/pharmaverse/admiralpeds
* Github mirror: https://github.com/cran/admiralpeds
* Maintainer: Fanny Gautier <fanny.gautier@cytel.com>

Run `revdepcheck::cloud_details(, "admiralpeds")` for more info

## Newly broken

*   checking re-building of vignette outputs ... ERROR
     ```
     Error(s) in re-building vignettes:
       ...
     --- re-building ‘admiralpeds.Rmd’ using rmarkdown
     --- finished re-building ‘admiralpeds.Rmd’
     
     --- re-building ‘advs.Rmd’ using rmarkdown
     
     Quitting from advs.Rmd:234-304 [unnamed-chunk-7]
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     NULL
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'advs.Rmd' failed with diagnostics:
     ℹ In argument: `BRTHDT = convert_dtc_to_dt(...)`.
     Caused by error in `case_when()`:
     ! `..1 (right)` must have size 1, not size 3.
     --- failed re-building ‘advs.Rmd’
     
     SUMMARY: processing the following file failed:
       ‘advs.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

# admiralvaccine (0.5.0)

* GitHub: https://github.com/pharmaverse/admiralvaccine
* Github mirror: https://github.com/cran/admiralvaccine
* Maintainer: Arjun Rubalingam <arjun.rubalingam@pfizer.com>

Run `revdepcheck::cloud_details(, "admiralvaccine")` for more info

## Newly broken

*   checking re-building of vignette outputs ... ERROR
     ```
     ...
     Error: processing vignette 'adis.Rmd' failed with diagnostics:
     ℹ In argument: `ADT = convert_dtc_to_dt(...)`.
     Caused by error in `case_when()`:
     ! `..1 (right)` must have size 1, not size 3.
     --- failed re-building ‘adis.Rmd’
     
     --- re-building ‘admiralvaccine.Rmd’ using rmarkdown
     --- finished re-building ‘admiralvaccine.Rmd’
     
     --- re-building ‘adsl.Rmd’ using rmarkdown
     
     Quitting from adsl.Rmd:123-158 [unnamed-chunk-5]
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     NULL
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'adsl.Rmd' failed with diagnostics:
     `..1 (right)` must have size 1, not size 3.
     --- failed re-building ‘adsl.Rmd’
     
     SUMMARY: processing the following files failed:
       ‘adce.Rmd’ ‘adface.Rmd’ ‘adis.Rmd’ ‘adsl.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

# AeroSampleR (0.2.0)

* Github mirror: https://github.com/cran/AeroSampleR
* Maintainer: Mark Hogue <mark.hogue.chp@gmail.com>

Run `revdepcheck::cloud_details(, "AeroSampleR")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     > 
     > ### ** Examples
     > 
     > df <- particle_dist() # set up particle distribution
     > params <- set_params_1("D_tube" = 2.54, "Q_lpm" = 100,
     + "T_C" = 25, "P_kPa" = 101.325) #example system parameters
     > df <- set_params_2(df, params) #particle size-dependent parameters
     > df <- probe_eff(df, params, orient = 'h') #probe orientation - horizontal
     > df <- bend_eff(df, params, method='Zhang', bend_angle=90,
     + bend_radius=0.1, elnum=3)
     > df <- tube_eff(df, params, L = 100,
     + angle_to_horiz = 90, elnum = 3)
     Error in `dplyr::case_when()`:
     ! `..1 (right)` must have size 1, not size 1003.
     Backtrace:
         ▆
      1. ├─AeroSampleR::tube_eff(...)
      2. │ └─dplyr::case_when(params$Re < 2100 ~ lam, params$Re > 4000 ~ turb, TRUE ~ mixed)
      3. │   └─dplyr:::vec_case_when(...)
      4. │     └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
      5. └─vctrs:::stop_assert_size(...)
      6.   └─vctrs:::stop_assert(...)
      7.     └─vctrs:::stop_vctrs(...)
      8.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
     Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     Error(s) in re-building vignettes:
     --- re-building ‘AeroSampleR_vignette.Rmd’ using rmarkdown
     ```

## In both

*   checking dependencies in R code ... NOTE
     ```
     Namespace in Imports field not imported from: ‘flextable’
       All declared Imports should be used.
     ```

# aggreCAT (1.0.0)

* Github mirror: https://github.com/cran/aggreCAT
* Maintainer: David Wilkinson <david.wilkinson.research@gmail.com>

Run `revdepcheck::cloud_details(, "aggreCAT")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
       > library(testthat)
       > library(pointblank)
       > 
       > test_check("aggreCAT")
       Loading required package: aggreCAT
       ══ aggreCAT ════════════════════════════════════════════════════════════════════
       Version: 1.0.0 
       Please do not feed the cat. 
       ════════════════════════════════════════════════════════════════════════════════
       Starting 2 test processes
       [ FAIL 1 | WARN 0 | SKIP 0 | PASS 412 ]
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Error ('test-agg_methods.R:6:3'): (code run outside of `test_that()`) ───────
       <dplyr:::mutate_error/rlang_error/error/condition>
       Error in `dplyr::mutate(., agg_weight = dplyr::case_when(all(agg_weight == 
           0) ~ 1, TRUE ~ agg_weight))`: ℹ In argument: `agg_weight = dplyr::case_when(all(agg_weight == 0) ~ 1,
         TRUE ~ agg_weight)`.
       ℹ In group 1: `paper_id = "100"`.
       Caused by error in `dplyr::case_when()`:
       ! `..2 (right)` must have size 1, not size 25.
       
       [ FAIL 1 | WARN 0 | SKIP 0 | PASS 412 ]
       Error: Test failures
       Execution halted
     ```

## In both

*   checking dependencies in R code ... NOTE
     ```
     Namespace in Imports field not imported from: ‘mathjaxr’
       All declared Imports should be used.
     ```

*   checking data for non-ASCII characters ... NOTE
     ```
       Note: found 13 marked UTF-8 strings
     ```

# ale (0.5.3)

* GitHub: https://github.com/tripartio/ale
* Github mirror: https://github.com/cran/ale
* Maintainer: Chitu Okoli <Chitu.Okoli@skema.edu>

Run `revdepcheck::cloud_details(, "ale")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
        2.   └─cli::cli_abort(...)
        3.     └─rlang::abort(...)
       ── Error ('test-ALEpDist.R:108:5'): ALEpDist works with binary outcome ─────────
       Error in `ALEpDist(test_gam_binary, data = test_cars, y_col = "vs", parallel = 0, 
           silent = TRUE, rand_it = 10, .skip_validation = TRUE)`: No random p-value distributions could be created.
       i See `warnings()()` for error messages.
       Backtrace:
           ▆
        1. └─ale::ALEpDist(...) at test-ALEpDist.R:108:5
        2.   └─cli::cli_abort(...)
        3.     └─rlang::abort(...)
       ── Error ('test-ALEpDist.R:125:5'): ALEpDist works with categorical outcome ────
       Error in `ALEpDist(test_nn_categorical, model_packages = "nnet", data = test_cars, 
           y_col = "continent", pred_type = "probs", parallel = 0, silent = TRUE, 
           rand_it = 10, .skip_validation = TRUE)`: No random p-value distributions could be created.
       i See `warnings()()` for error messages.
       Backtrace:
           ▆
        1. └─ale::ALEpDist(...) at test-ALEpDist.R:125:5
        2.   └─cli::cli_abort(...)
        3.     └─rlang::abort(...)
       
       [ FAIL 14 | WARN 445 | SKIP 2 | PASS 83 ]
       Error: Test failures
       Execution halted
     ```

# arcpullr (0.3.0)

* Github mirror: https://github.com/cran/arcpullr
* Maintainer: Paul Frater <paul.frater@wisconsin.gov>

Run `revdepcheck::cloud_details(, "arcpullr")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
       Loading required package: sf
       Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE
       > 
       > test_check("arcpullr")
       [ FAIL 2 | WARN 0 | SKIP 0 | PASS 46 ]
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Error ('test_format_spatial_coords.R:70:5'): format_line_coords properly formats multiple lines ──
       <dplyr:::mutate_error/rlang_error/error/condition>
       Error in `dplyr::mutate(do.call("rbind", out), coordinates = dplyr::case_when(geom_type == 
           "multipoint" ~ .data$coordinates, TRUE ~ paste0("[", .data$coordinates, 
           "]")))`: ℹ In argument: `coordinates = dplyr::case_when(...)`.
       Caused by error in `dplyr::case_when()`:
       ! `..1 (right)` must have size 1, not size 2.
       ── Error ('test_format_spatial_coords.R:86:5'): format_polygon_coords properly formats multiple polygons ──
       <dplyr:::mutate_error/rlang_error/error/condition>
       Error in `dplyr::mutate(do.call("rbind", out), coordinates = dplyr::case_when(geom_type == 
           "multipoint" ~ .data$coordinates, TRUE ~ paste0("[", .data$coordinates, 
           "]")))`: ℹ In argument: `coordinates = dplyr::case_when(...)`.
       Caused by error in `dplyr::case_when()`:
       ! `..1 (right)` must have size 1, not size 2.
       
       [ FAIL 2 | WARN 0 | SKIP 0 | PASS 46 ]
       Error: Test failures
       Execution halted
     ```

## In both

*   checking dependencies in R code ... NOTE
     ```
     Namespace in Imports field not imported from: ‘methods’
       All declared Imports should be used.
     ```

# BayesGrowth (1.0.0)

* GitHub: https://github.com/jonathansmart/BayesGrowth
* Github mirror: https://github.com/cran/BayesGrowth
* Maintainer: Jonathan Smart <jonsmartphd@gmail.com>

Run `revdepcheck::cloud_details(, "BayesGrowth")` for more info

## Newly broken

*   checking re-building of vignette outputs ... ERROR
     ```
     Error(s) in re-building vignettes:
     --- re-building ‘MCMC-example.Rmd’ using rmarkdown
     ```

## In both

*   checking installed package size ... NOTE
     ```
       installed size is 82.3Mb
       sub-directories of 1Mb or more:
         data   1.5Mb
         libs  80.1Mb
     ```

*   checking for GNU extensions in Makefiles ... NOTE
     ```
     GNU make is a SystemRequirements.
     ```

# BiVariAn (1.0.2)

* GitHub: https://github.com/AndresFloresG/BiVariAn
* Github mirror: https://github.com/cran/BiVariAn
* Maintainer: José Andrés Flores-García <andres.flores@uaslp.mx>

Run `revdepcheck::cloud_details(, "BiVariAn")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     > ### Aliases: auto_pie_categ
     > 
     > ### ** Examples
     > 
     > data <- data.frame(categ = rep(c("Categ1", "Categ2"), 25),
     + var1 = rbinom(50, 2, prob = 0.3),
     + var2 = rbinom(50, 2, prob = 0.8),
     + var3 = rbinom(50, 2, prob = 0.7))
     > data$categ <- as.factor(data$categ)
     > data$var1 <- as.factor(data$var1)
     > data$var2 <- as.factor(data$var2)
     > data$var3 <- as.factor(data$var3)
     > 
     > pieplot_list <- auto_pie_categ(data = data)
     Error in `if_else()`:
     ! `condition` can't be an array.
     Backtrace:
         ▆
      1. └─BiVariAn::auto_pie_categ(data = data)
      2.   └─dplyr::if_else(is.na(pos), freq/2, pos)
      3.     └─dplyr:::vec_case_when(...)
      4.       └─dplyr:::check_no_dim(condition, arg = condition_arg, call = call)
      5.         └─cli::cli_abort("{.arg {arg}} can't be an array.", call = call)
      6.           └─rlang::abort(...)
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
       > 
       > test_check("BiVariAn")
       [ FAIL 1 | WARN 0 | SKIP 7 | PASS 144 ]
       
       ══ Skipped tests (7) ═══════════════════════════════════════════════════════════
       • On CRAN (7): 'test-auto_shapiro_raw.R:64:3', 'test-continuous_2g.R:30:3',
         'test-continuous_2g.R:87:3', 'test-continuous_2g_pair.R:55:3',
         'test-dichotomous_2k_2sid.R:19:3', 'test-ss_multreg.R:2:3',
         'test-step_bw_firth.R:6:3'
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Error ('test-auto_pie_categ.R:11:3'): auto_pie_categ works ──────────────────
       Error in `if_else(is.na(pos), freq/2, pos)`: `condition` can't be an array.
       Backtrace:
           ▆
        1. └─BiVariAn::auto_pie_categ(data = data) at test-auto_pie_categ.R:11:3
        2.   └─dplyr::if_else(is.na(pos), freq/2, pos)
        3.     └─dplyr:::vec_case_when(...)
        4.       └─dplyr:::check_no_dim(condition, arg = condition_arg, call = call)
        5.         └─cli::cli_abort("{.arg {arg}} can't be an array.", call = call)
        6.           └─rlang::abort(...)
       
       [ FAIL 1 | WARN 0 | SKIP 7 | PASS 144 ]
       Error: Test failures
       Execution halted
     ```

# bp (2.1.0)

* GitHub: https://github.com/johnschwenck/bp
* Github mirror: https://github.com/cran/bp
* Maintainer: John Schwenck <jschwenck12@gmail.com>

Run `revdepcheck::cloud_details(, "bp")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     Error in `dplyr::mutate()`:
     ℹ In argument: `BP_CLASS = dplyr::case_when(...)`.
     Caused by error in `dplyr::case_when()`:
     ! `..1 (right)` must have size 1, not size 250.
     Backtrace:
          ▆
       1. ├─bp::process_data(...)
       2. │ └─bp::bp_stages(...)
       3. │   └─... %>% dplyr::relocate(BP_CLASS, .after = DBP)
       4. ├─dplyr::relocate(., BP_CLASS, .after = DBP)
       5. ├─dplyr::mutate(...)
       6. ├─dplyr:::mutate.data.frame(...)
       7. │ └─dplyr:::mutate_cols(.data, dplyr_quosures(...), by)
       8. │   ├─base::withCallingHandlers(...)
       9. │   └─dplyr:::mutate_col(dots[[i]], data, mask, new_columns)
      10. │     └─mask$eval_all_mutate(quo)
      11. │       └─dplyr (local) eval()
      12. ├─dplyr::case_when(...)
      13. │ └─dplyr:::vec_case_when(...)
      14. │   └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
      15. └─vctrs:::stop_assert_size(...)
      16.   └─vctrs:::stop_assert(...)
      17.     └─vctrs:::stop_vctrs(...)
      18.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
     Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     Error(s) in re-building vignettes:
       ...
     --- re-building ‘bp.Rmd’ using rmarkdown
     
     Quitting from bp.Rmd:81-99 [unnamed-chunk-2]
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     NULL
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'bp.Rmd' failed with diagnostics:
     ℹ In argument: `BP_CLASS = dplyr::case_when(...)`.
     Caused by error in `dplyr::case_when()`:
     ! `..1 (right)` must have size 1, not size 250.
     --- failed re-building ‘bp.Rmd’
     
     SUMMARY: processing the following file failed:
       ‘bp.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

## In both

*   checking Rd files ... NOTE
     ```
     checkRd: (-1) bp_rats.Rd:41: Escaped LaTeX specials: \&
     checkRd: (-1) bp_sv.Rd:75: Lost braces; missing escapes or markup?
         75 | $$SV = sqrt(sum(x_{i+1} - x_i)^2/n-1)$$
            |                   ^
     checkRd: (-1) bp_sv.Rd:80: Lost braces; missing escapes or markup?
         80 | $$SD = sqrt(sum(x_{i+1} - xbar)^2/n-1)$$
            |                   ^
     checkRd: (-1) sv.Rd:78: Lost braces; missing escapes or markup?
         78 | $$SV = sqrt(sum(x_{i+1} - x_i)^2/n-1)$$
            |                   ^
     checkRd: (-1) sv.Rd:82: Lost braces; missing escapes or markup?
         82 | $$SD = sqrt(sum(x_{i+1} - xbar)^2/n-1)$$
            |                   ^
     ```

# brandr (0.1.0)

* GitHub: https://github.com/danielvartan/brandr
* Github mirror: https://github.com/cran/brandr
* Maintainer: Daniel Vartanian <danvartan@gmail.com>

Run `revdepcheck::cloud_details(, "brandr")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     
     > ### Name: color_brand_sequential
     > ### Title: Brand color palettes
     > ### Aliases: color_brand_sequential color_brand_diverging
     > ###   color_brand_qualitative
     > 
     > ### ** Examples
     > 
     > color_brand_sequential(5)
     Error in `dplyr::case_when()`:
     ! `..3 (right)` must have size 1, not size 5.
     Backtrace:
          ▆
       1. ├─brandr::color_brand_sequential(5)
       2. │ └─brandr::interpolate_colors(...)
       3. │   └─brandr:::make_color_ramp(...)
       4. │     └─brandr (local) color_fun(n)
       5. │       └─dplyr::case_when(n == 0 ~ color_ramp_fun(1), is.na(n) ~ NA, TRUE ~ color_ramp_fun(n))
       6. │         └─dplyr:::vec_case_when(...)
       7. │           └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
       8. └─vctrs:::stop_assert_size(...)
       9.   └─vctrs:::stop_assert(...)
      10.     └─vctrs:::stop_vctrs(...)
      11.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
        11.   └─vctrs:::stop_assert(...)
        12.     └─vctrs:::stop_vctrs(...)
        13.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
       ── Error ('test-interpolate_colors.R:189:3'): make_color_ramp() | General test ──
       <vctrs_error_assert_size/vctrs_error_assert/vctrs_error/rlang_error/error/condition>
       Error in `dplyr::case_when(n == 0 ~ color_ramp_fun(1), is.na(n) ~ NA, TRUE ~ 
           color_ramp_fun(n))`: `..3 (right)` must have size 1, not size 3.
       Backtrace:
            ▆
         1. ├─testthat::expect_equal(...) at test-interpolate_colors.R:189:3
         2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
         3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
         4. ├─brandr:::make_color_ramp(n = 3, colors = c("red", "blue"), direction = 1)
         5. │ └─brandr (local) color_fun(n)
         6. │   └─dplyr::case_when(n == 0 ~ color_ramp_fun(1), is.na(n) ~ NA, TRUE ~ color_ramp_fun(n))
         7. │     └─dplyr:::vec_case_when(...)
         8. │       └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
         9. └─vctrs:::stop_assert_size(...)
        10.   └─vctrs:::stop_assert(...)
        11.     └─vctrs:::stop_vctrs(...)
        12.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
       
       [ FAIL 4 | WARN 0 | SKIP 0 | PASS 118 ]
       Error: Test failures
       Execution halted
     ```

# bulkreadr (1.2.1)

* GitHub: https://github.com/gbganalyst/bulkreadr
* Github mirror: https://github.com/cran/bulkreadr
* Maintainer: Ezekiel Ogundepo <gbganalyst@gmail.com>

Run `revdepcheck::cloud_details(, "bulkreadr")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     ℹ In argument: `across(...)`.
     Caused by error in `across()`:
     ! Can't compute column `Sepal_Length`.
     Caused by error in `case_when()`:
     ! `..1 (right)` must have size 1, not size 7.
     Backtrace:
          ▆
       1. ├─bulkreadr::fill_missing_values(df, method = "mean")
       2. │ └─df %>% ...
       3. ├─dplyr::mutate(...)
       4. ├─dplyr:::mutate.data.frame(...)
       5. │ └─dplyr:::mutate_cols(.data, dplyr_quosures(...), by)
       6. │   ├─base::withCallingHandlers(...)
       7. │   └─dplyr:::mutate_col(dots[[i]], data, mask, new_columns)
       8. │     ├─base::withCallingHandlers(...)
       9. │     └─mask$eval_all_mutate(quo)
      10. │       └─dplyr (local) eval()
      11. ├─dplyr::case_when(...)
      12. │ └─dplyr:::vec_case_when(...)
      13. │   └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
      14. └─vctrs:::stop_assert_size(...)
      15.   └─vctrs:::stop_assert(...)
      16.     └─vctrs:::stop_vctrs(...)
      17.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
         5. │ └─dplyr:::mutate_cols(.data, dplyr_quosures(...), by)
         6. │   ├─base::withCallingHandlers(...)
         7. │   └─dplyr:::mutate_col(dots[[i]], data, mask, new_columns)
         8. │     ├─base::withCallingHandlers(...)
         9. │     └─mask$eval_all_mutate(quo)
        10. │       └─dplyr (local) eval()
        11. ├─dplyr::case_when(...)
        12. │ └─dplyr:::vec_case_when(...)
        13. │   └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
        14. ├─vctrs:::stop_assert_size(...)
        15. │ └─vctrs:::stop_assert(...)
        16. │   └─vctrs:::stop_vctrs(...)
        17. │     └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
        18. │       └─rlang:::signal_abort(cnd, .file)
        19. │         └─base::signalCondition(cnd)
        20. ├─dplyr (local) `<fn>`(`<vctrs___>`)
        21. │ └─rlang::abort(msg, call = call("across"), parent = cnd)
        22. │   └─rlang:::signal_abort(cnd, .file)
        23. │     └─base::signalCondition(cnd)
        24. └─dplyr (local) `<fn>`(`<rlng_rrr>`)
        25.   └─rlang::abort(message, class = error_class, parent = parent, call = error_call)
       
       [ FAIL 1 | WARN 0 | SKIP 0 | PASS 21 ]
       Error: Test failures
       Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     ...
     --- finished re-building ‘intro-to-bulkreadr.Rmd’
     
     --- re-building ‘labelled-data.Rmd’ using rmarkdown
     --- finished re-building ‘labelled-data.Rmd’
     
     --- re-building ‘other-functions.Rmd’ using rmarkdown
     
     Quitting from other-functions.Rmd:135-139 [unnamed-chunk-5]
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     NULL
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'other-functions.Rmd' failed with diagnostics:
     ℹ In argument: `across(...)`.
     Caused by error in `across()`:
     ! Can't compute column `Sepal_Length`.
     Caused by error in `case_when()`:
     ! `..1 (right)` must have size 1, not size 7.
     --- failed re-building ‘other-functions.Rmd’
     
     SUMMARY: processing the following file failed:
       ‘other-functions.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

# canadianmaps (2.0.0)

* GitHub: https://github.com/joellecayen/canadianmaps
* Github mirror: https://github.com/cran/canadianmaps
* Maintainer: Joelle Cayen <joelle.cayen@phac-aspc.gc.ca>

Run `revdepcheck::cloud_details(, "canadianmaps")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
               "#525183", "#ACBCD3")))(num), palette == "Starbs" ~ (grDevices::colorRampPalette(c("#0C5C41", 
               "#5A9C84", "#D3E4DF", "#3EC8A9")))(num), palette == "Purples" ~ 
               (grDevices::colorRampPalette(c("#E7E7EF", "#646CAC", 
                   "#393F93", "#0E0F40")))(num), palette == "PHAC" ~ 
               (grDevices::colorRampPalette(c("#F4CDD0", "#D33F49", 
                   "#B72A33", "#851E25")))(num), palette == "CNISP" ~ 
               (grDevices::colorRampPalette(c("#4F666C", "#6E8B93", 
                   "#8CACB5", "#B1C7CD", "#D8E5EB")))(num), palette == 
               "Jess" ~ (grDevices::colorRampPalette(c("#FFC55E", "#D49D43", 
               "#556F60", "#3E543F")))(num), palette == "HAI" ~ (grDevices::colorRampPalette(c("#BF2431", 
               "#563c98", "#154360", "#48C9B0")))(num))`: `..1 (right)` must have size 1, not size 5.
       Backtrace:
           ▆
        1. ├─canadianmaps::scale_color_map(palette = "Kelly", num = 5) at test-functions.R:21:3
        2. │ └─dplyr::case_when(...)
        3. │   └─dplyr:::vec_case_when(...)
        4. │     └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
        5. └─vctrs:::stop_assert_size(...)
        6.   └─vctrs:::stop_assert(...)
        7.     └─vctrs:::stop_vctrs(...)
        8.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
       
       [ FAIL 2 | WARN 0 | SKIP 0 | PASS 4 ]
       Error: Test failures
       Execution halted
     ```

## In both

*   checking installed package size ... NOTE
     ```
       installed size is  8.1Mb
       sub-directories of 1Mb or more:
         data   8.0Mb
     ```

*   checking dependencies in R code ... NOTE
     ```
     Namespace in Imports field not imported from: ‘sf’
       All declared Imports should be used.
     ```

*   checking data for non-ASCII characters ... NOTE
     ```
       Note: found 3061 marked UTF-8 strings
     ```

# care4cmodel (1.0.3)

* Github mirror: https://github.com/cran/care4cmodel
* Maintainer: Peter Biber <p.biber@tum.de>

Run `revdepcheck::cloud_details(, "care4cmodel")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     > 
     > 
     >   sim_base_out <- simulate_single_concept(
     +     pine_thinning_from_above_1,
     +     init_areas = c(1000, 0, 0, 0, 0, 0),
     +     time_span  = 200,
     +     risk_level = 3
     +   )
     > 
     >   # Make a plot
     >   plot(sim_base_out, variable = "area")
     Error in `dplyr::case_when()`:
     ! `..1 (right)` must have size 1, not size 5.
     Backtrace:
         ▆
      1. ├─base::plot(sim_base_out, variable = "area")
      2. ├─care4cmodel:::plot.c4c_base_result(sim_base_out, variable = "area")
      3. │ └─dplyr::case_when(...)
      4. │   └─dplyr:::vec_case_when(...)
      5. │     └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
      6. └─vctrs:::stop_assert_size(...)
      7.   └─vctrs:::stop_assert(...)
      8.     └─vctrs:::stop_vctrs(...)
      9.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
     Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     Error(s) in re-building vignettes:
       ...
     --- re-building ‘getting-started-with-care4cmodel.Rmd’ using rmarkdown
     
     Quitting from getting-started-with-care4cmodel.Rmd:82-90 [quickstart_plot_base]
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     NULL
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'getting-started-with-care4cmodel.Rmd' failed with diagnostics:
     `..1 (right)` must have size 1, not size 5.
     --- failed re-building ‘getting-started-with-care4cmodel.Rmd’
     
     SUMMARY: processing the following file failed:
       ‘getting-started-with-care4cmodel.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

# CATAcode (1.0.0)

* GitHub: https://github.com/knickodem/CATAcode
* Github mirror: https://github.com/cran/CATAcode
* Maintainer: Kyle Nickodem <kyle.nickodem@gmail.com>

Run `revdepcheck::cloud_details(, "CATAcode")` for more info

## Newly broken

*   checking re-building of vignette outputs ... ERROR
     ```
     Error(s) in re-building vignettes:
       ...
     --- re-building ‘CATAcode-overview.Rmd’ using rmarkdown
     
     Quitting from CATAcode-overview.Rmd:245-258 [multiple]
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     NULL
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'CATAcode-overview.Rmd' failed with diagnostics:
     ℹ In argument: `new = case_when(n() == 1 ~ Barriers, TRUE ~
       multi.name)`.
     ℹ In group 1: `ID = 1`.
     Caused by error in `case_when()`:
     ! `..1 (right)` must have size 1, not size 4.
     --- failed re-building ‘CATAcode-overview.Rmd’
     
     SUMMARY: processing the following file failed:
       ‘CATAcode-overview.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

# CGPfunctions (0.6.3)

* GitHub: https://github.com/ibecav/CGPfunctions
* Github mirror: https://github.com/cran/CGPfunctions
* Maintainer: Chuck Powell <ibecav@gmail.com>

Run `revdepcheck::cloud_details(, "CGPfunctions")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     6 1     8         2
     Error in `mutate()`:
     ℹ In argument: `LowerBound = case_when(...)`.
     ℹ In group 1: `am = 0`.
     Caused by error in `case_when()`:
     ! `..1 (right)` must have size 1, not size 3.
     Backtrace:
          ▆
       1. ├─CGPfunctions::Plot2WayANOVA(mpg ~ am * cyl, mtcars, plottype = "line")
       2. │ └─... %>% ...
       3. ├─dplyr::mutate(...)
       4. ├─dplyr:::mutate.data.frame(...)
       5. │ └─dplyr:::mutate_cols(.data, dplyr_quosures(...), by)
       6. │   ├─base::withCallingHandlers(...)
       7. │   └─dplyr:::mutate_col(dots[[i]], data, mask, new_columns)
       8. │     └─mask$eval_all_mutate(quo)
       9. │       └─dplyr (local) eval()
      10. ├─dplyr::case_when(...)
      11. │ └─dplyr:::vec_case_when(...)
      12. │   └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
      13. └─vctrs:::stop_assert_size(...)
      14.   └─vctrs:::stop_assert(...)
      15.     └─vctrs:::stop_vctrs(...)
      16.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
     Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     Error(s) in re-building vignettes:
     --- re-building ‘Using-Plot2WayANOVA.Rmd’ using rmarkdown
     
     Quitting from Using-Plot2WayANOVA.Rmd:147-149 [Plot2WayANOVA]
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     NULL
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'Using-Plot2WayANOVA.Rmd' failed with diagnostics:
     ℹ In argument: `LowerBound = case_when(...)`.
     ℹ In group 1: `am = 0`.
     Caused by error in `case_when()`:
     ! `..1 (right)` must have size 1, not size 3.
     --- failed re-building ‘Using-Plot2WayANOVA.Rmd’
     
     --- re-building ‘Using-PlotXTabs.Rmd’ using rmarkdown
     ```

## In both

*   checking package dependencies ... NOTE
     ```
     Package suggested but not available for checking: ‘hrbrthemes’
     ```

# cocoon (0.2.1)

* GitHub: https://github.com/JeffreyRStevens/cocoon
* Github mirror: https://github.com/cran/cocoon
* Maintainer: Jeffrey R. Stevens <jeffrey.r.stevens@protonmail.com>

Run `revdepcheck::cloud_details(, "cocoon")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     > format_p(0.001)
     [1] "_p_ = .001"
     > 
     > # Format p-value vector
     > format_p(c(0.001, 0.01))
     Error in `dplyr::case_when()`:
     ! Failed to evaluate the right-hand side of formula 3.
     Caused by error in `dplyr::case_when()`:
     ! `..1 (right)` must have size 1, not size 2.
     Backtrace:
          ▆
       1. ├─cocoon::format_p(c(0.001, 0.01))
       2. │ └─dplyr::case_when(...)
       3. │   └─dplyr:::case_formula_evaluate(...)
       4. │     ├─base::withCallingHandlers(...)
       5. │     └─rlang::eval_tidy(pair$rhs, env = default_env)
       6. ├─cocoon::format_num(x, digits = digits)
       7. │ └─dplyr::case_when(...)
       8. │   └─dplyr:::vec_case_when(...)
       9. │     └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
      10. └─vctrs:::stop_assert_size(...)
      11.   └─vctrs:::stop_assert(...)
      12.     └─vctrs:::stop_vctrs(...)
      13.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
         8. │     └─dplyr::case_when(...)
         9. │       └─dplyr:::vec_case_when(...)
        10. │         └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
        11. └─vctrs:::stop_assert_size(...)
        12.   └─vctrs:::stop_assert(...)
        13.     └─vctrs:::stop_vctrs(...)
        14.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
       ── Error ('test-format_statvalues.R:55:3'): format_bf() works properly ─────────
       Error in `dplyr::case_when(bf >= 1000 ~ format_scientific(bf, digits = digits1, 
           type = type), bf <= 1/10^digits2 ~ format_scientific(bf, 
           digits = digits1, type = type), bf >= 1 ~ format_num(bf, 
           digits = digits1), bf < 1 ~ format_num(bf, digits = digits2))`: Failed to evaluate the right-hand side of formula 1.
       Caused by error in `dplyr::case_when()`:
       ! `..1 (right)` must have size 1, not size 8.
       ── Error ('test-format_statvalues.R:99:3'): format_p() works properly ──────────
       Error in `dplyr::case_when(x < cutoff & pzero ~ as.character(as.numeric(paste0("1e-", 
           digits))), x < cutoff & !pzero ~ sub("0\\.", "\\.", as.character(as.numeric(paste0("1e-", 
           digits)))), x >= cutoff & pzero ~ format_num(x, digits = digits), 
           x >= cutoff & !pzero ~ sub("0\\.", "\\.", format_num(x, digits = digits)))`: Failed to evaluate the right-hand side of formula 3.
       Caused by error in `dplyr::case_when()`:
       ! `..1 (right)` must have size 1, not size 2.
       
       [ FAIL 8 | WARN 0 | SKIP 0 | PASS 233 ]
       Error: Test failures
       Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     Error(s) in re-building vignettes:
       ...
     --- re-building ‘cocoon.Rmd’ using rmarkdown
     
     Quitting from cocoon.Rmd:52-52
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     NULL
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'cocoon.Rmd' failed with diagnostics:
     `..1 (right)` must have size 1, not size 2.
     --- failed re-building ‘cocoon.Rmd’
     
     SUMMARY: processing the following file failed:
       ‘cocoon.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

# ConSciR (0.3.0)

* GitHub: https://github.com/BhavShah01/ConSciR
* Github mirror: https://github.com/cran/ConSciR
* Maintainer: Bhavesh Shah <bhaveshshah01@gmail.com>

Run `revdepcheck::cloud_details(, "ConSciR")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     +   alpha = 0.7,
     +   limit_caption = "Example limit box"
     + )
     Error in `dplyr::mutate()`:
     ℹ In argument: `ColorValue = dplyr::case_when(...)`.
     Caused by error in `dplyr::case_when()`:
     ! `..2 (right)` must have size 1, not size 100.
     Backtrace:
          ▆
       1. ├─ConSciR::graph_TRHbivariate(...)
       2. │ ├─dplyr::mutate(...)
       3. │ └─dplyr:::mutate.data.frame(...)
       4. │   └─dplyr:::mutate_cols(.data, dplyr_quosures(...), by)
       5. │     ├─base::withCallingHandlers(...)
       6. │     └─dplyr:::mutate_col(dots[[i]], data, mask, new_columns)
       7. │       └─mask$eval_all_mutate(quo)
       8. │         └─dplyr (local) eval()
       9. ├─dplyr::case_when(...)
      10. │ └─dplyr:::vec_case_when(...)
      11. │   └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
      12. └─vctrs:::stop_assert_size(...)
      13.   └─vctrs:::stop_assert(...)
      14.     └─vctrs:::stop_vctrs(...)
      15.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
     Execution halted
     ```

# cpsvote (0.1.0)

* GitHub: https://github.com/Reed-EVIC/cpsvote
* Github mirror: https://github.com/cran/cpsvote
* Maintainer: Jay Lee <jaylee@reed.edu>

Run `revdepcheck::cloud_details(, "cpsvote")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     > 
     > cps_label(cps_2016_10k)
     Error in `dplyr::mutate()`:
     ℹ In argument: `YEAR = dplyr::case_when(...)`.
     Caused by error in `dplyr::case_when()`:
     ! `..1 (right)` must have size 1, not size 10000.
     Backtrace:
          ▆
       1. ├─cpsvote::cps_label(cps_2016_10k)
       2. │ └─... %>% ...
       3. ├─dplyr::mutate(...)
       4. ├─dplyr:::mutate.data.frame(...)
       5. │ └─dplyr:::mutate_cols(.data, dplyr_quosures(...), by)
       6. │   ├─base::withCallingHandlers(...)
       7. │   └─dplyr:::mutate_col(dots[[i]], data, mask, new_columns)
       8. │     └─mask$eval_all_mutate(quo)
       9. │       └─dplyr (local) eval()
      10. ├─dplyr::case_when(expand_year ~ as.integer(YEAR%%1900 + 1900), TRUE ~ YEAR)
      11. │ └─dplyr:::vec_case_when(...)
      12. │   └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
      13. └─vctrs:::stop_assert_size(...)
      14.   └─vctrs:::stop_assert(...)
      15.     └─vctrs:::stop_vctrs(...)
      16.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
     Execution halted
     ```

# debkeepr (0.1.1)

* GitHub: https://github.com/jessesadler/debkeepr
* Github mirror: https://github.com/cran/debkeepr
* Maintainer: Jesse Sadler <jrsadler@icloud.com>

Run `revdepcheck::cloud_details(, "debkeepr")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     > 
     > 
     > x <- deb_decimal(c(8.825, 15.125, 3.65))
     > y <- deb_decimal(c(56.45, 106.525, 200.4), unit = "s")
     > z <- deb_decimal(c(8472, 14520,  3504),
     +                  unit = "f",
     +                  bases = c(20, 12, 4))
     > 
     > deb_convert_unit(x, to = "s")
     Error in `dplyr::case_when()`:
     ! `..1 (right)` must have size 1, not size 3.
     Backtrace:
          ▆
       1. ├─debkeepr::deb_convert_unit(x, to = "s")
       2. │ └─vctrs::vec_cast(x, deb_decimal(unit = to_unit, bases = deb_bases(x)))
       3. │   └─vctrs (local) `<fn>`()
       4. │     └─debkeepr:::vec_cast.deb_decimal.deb_decimal(...)
       5. │       └─dplyr::case_when(...)
       6. │         └─dplyr:::vec_case_when(...)
       7. │           └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
       8. └─vctrs:::stop_assert_size(...)
       9.   └─vctrs:::stop_assert(...)
      10.     └─vctrs:::stop_vctrs(...)
      11.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
           from_unit == "s" & to_unit == "d" ~ x * bases[[2]], from_unit == 
               "s" & to_unit == "l" ~ x/bases[[1]], from_unit == "d" & 
               to_unit == "l" ~ x/prod(bases[1:2]), from_unit == "d" & 
               to_unit == "s" ~ x/bases[[2]])`: `..1 (right)` must have size 1, not size 4.
       Backtrace:
            ▆
         1. ├─testthat::expect_equal(sum(x, deb_decimal(1.8375)), deb_decimal(15.340625)) at test-mathematics.R:54:3
         2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
         3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
         4. ├─vctrs:::Summary.vctrs_vctr(...)
         5. │ ├─vctrs::vec_math(.Generic, vec_c(...), na.rm = na.rm)
         6. │ └─vctrs::vec_c(...)
         7. │   └─vctrs (local) `<fn>`()
         8. │     └─debkeepr:::vec_cast.deb_decimal.deb_decimal(...)
         9. │       └─dplyr::case_when(...)
        10. │         └─dplyr:::vec_case_when(...)
        11. │           └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
        12. └─vctrs:::stop_assert_size(...)
        13.   └─vctrs:::stop_assert(...)
        14.     └─vctrs:::stop_vctrs(...)
        15.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
       
       [ FAIL 2 | WARN 0 | SKIP 74 | PASS 853 ]
       Error: Test failures
       Execution halted
     ```

## In both

*   checking data for non-ASCII characters ... NOTE
     ```
       Note: found 53 marked UTF-8 strings
     ```

# DeSciDe (1.0.2)

* GitHub: https://github.com/camdouglas/DeSciDe
* Github mirror: https://github.com/cran/DeSciDe
* Maintainer: Cameron Douglas <camerondouglas@ufl.edu>

Run `revdepcheck::cloud_details(, "DeSciDe")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     Running examples in ‘DeSciDe-Ex.R’ failed
     The error most likely occurred in:
     
     > ### Name: search_pubmed
     > ### Title: Search PubMed with Multiple Genes and Terms
     > ### Aliases: search_pubmed
     > 
     > ### ** Examples
     > 
     > genes <- c("TP53", "BRCA1")
     > terms <- c("cancer", "tumor")
     > search_results <- search_pubmed(genes, terms, rank_method = "weighted", verbose = FALSE)
     Error in entrez_check(response) : 
       HTTP failure: 429, too many requests. Functions that contact the NCBI should not be called in parallel. If you are using a shared IP, consider registerring for an API key as described in the rate-limiting section of rentrez tutorial. NCBI message:
      {"error":"API rate limit exceeded","api-key":"3.211.47.164","count":"4","limit":"3"}
     Calls: search_pubmed ... single_pubmed_search -> entrez_search -> make_entrez_query -> entrez_check
     Execution halted
     ```

# describedata (0.1.1)

* GitHub: https://github.com/craigjmcgowan/describedata
* Github mirror: https://github.com/cran/describedata
* Maintainer: Craig McGowan <mcgowan.cj@gmail.com>

Run `revdepcheck::cloud_details(, "describedata")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
       7. ├─purrr::when(...)
       8. ├─purrr::when(...)
       9. ├─purrr::when(...)
      10. │ └─base::eval(dots[[i]][[action]], env, env)
      11. │   └─base::eval(dots[[i]][[action]], env, env)
      12. │     ├─dplyr::bind_rows(...)
      13. │     │ └─rlang::list2(...)
      14. │     └─... %>% ...
      15. ├─purrr::when(...)
      16. ├─dplyr::select(., .data$variable, .data$value, .data$display)
      17. ├─dplyr::mutate(...)
      18. ├─dplyr:::mutate.data.frame(...)
      19. │ └─dplyr:::mutate_cols(.data, dplyr_quosures(...), by)
      20. │   ├─base::withCallingHandlers(...)
      21. │   └─dplyr:::mutate_col(dots[[i]], data, mask, new_columns)
      22. │     └─mask$eval_all_mutate(quo)
      23. │       └─dplyr (local) eval()
      24. ├─dplyr::case_when(...)
      25. │ └─dplyr:::vec_case_when(...)
      26. │   └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
      27. └─vctrs:::stop_assert_size(...)
      28.   └─vctrs:::stop_assert(...)
      29.     └─vctrs:::stop_vctrs(...)
      30.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
     Execution halted
     ```

# discord (1.2.4.1)

* GitHub: https://github.com/R-Computing-Lab/discord
* Github mirror: https://github.com/cran/discord
* Maintainer: S. Mason Garrison <garrissm@wfu.edu>

Run `revdepcheck::cloud_details(, "discord")` for more info

## Newly broken

*   checking re-building of vignette outputs ... ERROR
     ```
     Error(s) in re-building vignettes:
     --- re-building ‘Power.Rmd’ using rmarkdown
     ```

# DisImpact (0.0.21)

* GitHub: https://github.com/vinhdizzo/DisImpact
* Github mirror: https://github.com/cran/DisImpact
* Maintainer: Vinh Nguyen <nguyenvq714@gmail.com>

Run `revdepcheck::cloud_details(, "DisImpact")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
      12. ├─dplyr::arrange(...)
      13. ├─dplyr::mutate(...)
      14. ├─dplyr::bind_rows(.)
      15. │ └─rlang::list2(...)
      16. ├─dplyr::select(...)
      17. ├─dplyr::mutate(...)
      18. ├─dplyr::left_join(...)
      19. ├─dplyr::left_join(...)
      20. ├─dplyr::rename(...)
      21. ├─dplyr::ungroup(.)
      22. ├─dplyr::mutate(...)
      23. ├─dplyr:::mutate.data.frame(...)
      24. │ └─dplyr:::mutate_cols(.data, dplyr_quosures(...), by)
      25. │   ├─base::withCallingHandlers(...)
      26. │   └─dplyr:::mutate_col(dots[[i]], data, mask, new_columns)
      27. │     └─mask$eval_all_mutate(quo)
      28. │       └─dplyr (local) eval()
      29. ├─dplyr::case_when(...)
      30. │ └─dplyr:::vec_case_when(...)
      31. │   └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
      32. └─vctrs:::stop_assert_size(...)
      33.   └─vctrs:::stop_assert(...)
      34.     └─vctrs:::stop_vctrs(...)
      35.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
        18. ├─dplyr::arrange(...)
        19. ├─dplyr::mutate(...)
        20. ├─dplyr::bind_rows(.)
        21. │ └─rlang::list2(...)
        22. ├─dplyr::select(...)
        23. ├─dplyr::mutate(...)
        24. ├─dplyr::left_join(...)
        25. ├─dplyr::left_join(...)
        26. ├─dplyr::rename(...)
        27. ├─dplyr::ungroup(.)
        28. ├─dplyr::mutate(...)
        29. ├─dplyr:::mutate.data.frame(...)
        30. │ └─dplyr:::mutate_cols(.data, dplyr_quosures(...), by)
        31. │   ├─base::withCallingHandlers(...)
        32. │   └─dplyr:::mutate_col(dots[[i]], data, mask, new_columns)
        33. │     └─mask$eval_all_mutate(quo)
        34. │       └─dplyr (local) eval()
        35. ├─dplyr::case_when(...)
        36. │ └─dplyr:::vec_case_when(...)
        37. │   └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
        38. └─vctrs:::stop_assert_size(...)
        39.   └─vctrs:::stop_assert(...)
        40.     └─vctrs:::stop_vctrs(...)
        41.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
       Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     ...
     --- failed re-building ‘Multi-Ethnicity.Rmd’
     
     --- re-building ‘Scaling-DI-Calculations.Rmd’ using rmarkdown
     --- finished re-building ‘Scaling-DI-Calculations.Rmd’
     
     --- re-building ‘Tutorial.Rmd’ using rmarkdown
     
     Quitting from Tutorial.Rmd:70-76 [unnamed-chunk-4]
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     NULL
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'Tutorial.Rmd' failed with diagnostics:
     ℹ In argument: `reference = case_when(...)`.
     ℹ In group 1: `cohort = 1`.
     Caused by error in `case_when()`:
     ! `..3 (right)` must have size 1, not size 6.
     --- failed re-building ‘Tutorial.Rmd’
     
     SUMMARY: processing the following files failed:
       ‘DI-On-Long-Summarized-Data.Rmd’ ‘Intersectionality.Rmd’
       ‘Multi-Ethnicity.Rmd’ ‘Tutorial.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

# duckplyr (1.1.2)

* GitHub: https://github.com/tidyverse/duckplyr
* Github mirror: https://github.com/cran/duckplyr
* Maintainer: Kirill Müller <kirill@cynkra.com>

Run `revdepcheck::cloud_details(, "duckplyr")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
            ▆
         1. ├─testthat::expect_identical(...) at test-dplyr-case-when.R:218:3
         2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
         3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
         4. ├─dplyr::case_when(FALSE ~ 1:5, .default = 2)
         5. │ └─dplyr:::vec_case_when(...)
         6. │   └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
         7. └─vctrs:::stop_assert_size(...)
         8.   └─vctrs:::stop_assert(...)
         9.     └─vctrs:::stop_vctrs(...)
        10.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
       
       [ FAIL 3 | WARN 0 | SKIP 600 | PASS 2287 ]
       Deleting unused snapshots:
       • fallback/fallback-2.dcf
       • fallback/fallback.dcf
       Error: Test failures
       
       🛠: 2208
       🔨: 1172
       🦆: 1036
       add_count, anti_join, anti_join.data.frame, arrange, arrange.data.frame, compute, count, count.data.frame, cross_join, distinct, distinct.data.frame, do, eval, filter, filter.data.frame, full_join, group_by, group_indices, group_keys, group_map, group_modify, group_nest, group_size, group_split, group_trim, head, inner_join, inner_join.data.frame, intersect, left_join, left_join.data.frame, mutate, mutate.data.frame, n_groups, nest_by, nest_join, pull, reframe, relocate, rename, rename_with, right_join, rows_append, rows_delete, rows_insert, rows_patch, rows_update, rows_upsert, rowwise, select, select.data.frame, semi_join, semi_join.data.frame, setdiff, setequal, slice, slice_head, slice_head.data.frame, slice_sample, slice_tail, summarise, summarise.data.frame, symdiff, transmute, ungroup, union_all
       
       00:01:22.243784
       Execution halted
     ```

## In both

*   checking package dependencies ... NOTE
     ```
     Package which this enhances but not available for checking: ‘qs’
     ```

# dynwrap (1.2.4)

* GitHub: https://github.com/dynverse/dynwrap
* Github mirror: https://github.com/cran/dynwrap
* Maintainer: Robrecht Cannoodt <rcannood@gmail.com>

Run `revdepcheck::cloud_details(, "dynwrap")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
       4. │ └─dynwrap::simplify_igraph_network(...)
       5. │   └─purrr::map(...)
       6. │     └─purrr:::map_("list", .x, .f, ..., .progress = .progress)
       7. │       ├─purrr:::with_indexed_errors(...)
       8. │       │ └─base::withCallingHandlers(...)
       9. │       ├─purrr:::call_with_cleanup(...)
      10. │       └─dynwrap (local) .f(.x[[i]], ...)
      11. │         └─dynwrap:::simplify_replace_edges(...)
      12. │           └─... %>% select(id, from, to, percentage)
      13. ├─dplyr::select(., id, from, to, percentage)
      14. ├─dplyr::mutate(...)
      15. ├─dplyr:::mutate.data.frame(...)
      16. │ └─dplyr:::mutate_cols(.data, dplyr_quosures(...), by)
      17. │   ├─base::withCallingHandlers(...)
      18. │   └─dplyr:::mutate_col(dots[[i]], data, mask, new_columns)
      19. │     └─mask$eval_all_mutate(quo)
      20. │       └─dplyr (local) eval()
      21. ├─dplyr::case_when(path_len == 0 ~ 0.5, TRUE ~ (cs + percentage * weight)/path_len)
      22. │ └─dplyr:::vec_case_when(...)
      23. │   └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
      24. └─vctrs:::stop_assert_size(...)
      25.   └─vctrs:::stop_assert(...)
      26.     └─vctrs:::stop_vctrs(...)
      27.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
        17. │ └─dplyr:::mutate_cols(.data, dplyr_quosures(...), by)
        18. │   ├─base::withCallingHandlers(...)
        19. │   └─dplyr:::mutate_col(dots[[i]], data, mask, new_columns)
        20. │     └─mask$eval_all_mutate(quo)
        21. │       └─dplyr (local) eval()
        22. ├─dplyr::case_when(...)
        23. │ └─dplyr:::vec_case_when(...)
        24. │   └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
        25. ├─vctrs:::stop_assert_size(...)
        26. │ └─vctrs:::stop_assert(...)
        27. │   └─vctrs:::stop_vctrs(...)
        28. │     └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
        29. │       └─rlang:::signal_abort(cnd, .file)
        30. │         └─base::signalCondition(cnd)
        31. ├─dplyr (local) `<fn>`(`<vctrs___>`)
        32. │ └─rlang::abort(message, class = error_class, parent = parent, call = error_call)
        33. │   └─rlang:::signal_abort(cnd, .file)
        34. │     └─base::signalCondition(cnd)
        35. └─purrr (local) `<fn>`(`<dply:::_>`)
        36.   └─cli::cli_abort(...)
        37.     └─rlang::abort(...)
       
       [ FAIL 6 | WARN 23 | SKIP 3 | PASS 486 ]
       Error: Test failures
       Execution halted
     ```

# ecocomDP (1.3.2)

* GitHub: https://github.com/EDIorg/ecocomDP
* Github mirror: https://github.com/cran/ecocomDP
* Maintainer: Colin Smith <colin.smith@wisc.edu>

Run `revdepcheck::cloud_details(, "ecocomDP")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     Error in `dplyr::mutate()`:
     ℹ In argument: `datetime = dplyr::case_when(...)`.
     Caused by error in `dplyr::case_when()`:
     ! `..1 (right)` must have size 1, not size 689.
     Backtrace:
          ▆
       1. ├─ecocomDP::plot_taxa_diversity(ants_L1)
       2. │ └─... %>% dplyr::summarize(ntaxa = length(unique(taxon_id)))
       3. ├─dplyr::summarize(., ntaxa = length(unique(taxon_id)))
       4. ├─dplyr::group_by(., .data$location_id, .data$datetime)
       5. ├─dplyr::mutate(...)
       6. ├─dplyr:::mutate.data.frame(...)
       7. │ └─dplyr:::mutate_cols(.data, dplyr_quosures(...), by)
       8. │   ├─base::withCallingHandlers(...)
       9. │   └─dplyr:::mutate_col(dots[[i]], data, mask, new_columns)
      10. │     └─mask$eval_all_mutate(quo)
      11. │       └─dplyr (local) eval()
      12. ├─dplyr::case_when(...)
      13. │ └─dplyr:::vec_case_when(...)
      14. │   └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
      15. └─vctrs:::stop_assert_size(...)
      16.   └─vctrs:::stop_assert(...)
      17.     └─vctrs:::stop_vctrs(...)
      18.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
     Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     ...
     
     --- re-building ‘model_overview.Rmd’ using rmarkdown
     --- finished re-building ‘model_overview.Rmd’
     
     --- re-building ‘shared_practices_create.Rmd’ using rmarkdown
     --- finished re-building ‘shared_practices_create.Rmd’
     
     --- re-building ‘use.Rmd’ using rmarkdown
     
     Quitting from use.Rmd:225-250 [unnamed-chunk-14]
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     NULL
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'use.Rmd' failed with diagnostics:
     ℹ In argument: `datetime = dplyr::case_when(...)`.
     Caused by error in `dplyr::case_when()`:
     ! `..1 (right)` must have size 1, not size 689.
     --- failed re-building ‘use.Rmd’
     
     SUMMARY: processing the following file failed:
       ‘use.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

# efdm (0.2.1)

* GitHub: https://github.com/mikkoku/efdm
* Github mirror: https://github.com/cran/efdm
* Maintainer: Mikko Kuronen <mikko.kuronen@luke.fi>

Run `revdepcheck::cloud_details(, "efdm")` for more info

## Newly broken

*   checking re-building of vignette outputs ... ERROR
     ```
     Error(s) in re-building vignettes:
     --- re-building ‘example.Rmd’ using rmarkdown
     ```

# emery (0.6.0)

* GitHub: https://github.com/therealcfdrake/emery
* Github mirror: https://github.com/cran/emery
* Maintainer: Corie Drake <therealcfdrake@gmail.com>

Run `revdepcheck::cloud_details(, "emery")` for more info

## Newly broken

*   checking re-building of vignette outputs ... ERROR
     ```
     Error(s) in re-building vignettes:
       ...
     --- re-building ‘emery.Rmd’ using rmarkdown
     
     Quitting from emery.Rmd:66-68 [unnamed-chunk-3]
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     NULL
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'emery.Rmd' failed with diagnostics:
     ℹ In argument: `color_col = dplyr::case_when(...)`.
     Caused by error in `dplyr::case_when()`:
     ! `..1 (right)` must have size 1, not size 25.
     --- failed re-building ‘emery.Rmd’
     
     SUMMARY: processing the following file failed:
       ‘emery.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

# eph (1.0.2)

* GitHub: https://github.com/ropensci/eph
* Github mirror: https://github.com/cran/eph
* Maintainer: Carolina Pradier <carolinapradier@gmail.com>

Run `revdepcheck::cloud_details(, "eph")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     +   window = "trimestral"
     + )
     Error in `dplyr::mutate()`:
     ℹ In argument: `Periodo = dplyr::case_when(...)`.
     Caused by error in `dplyr::case_when()`:
     ! `..1 (right)` must have size 1, not size 3996.
     Backtrace:
          ▆
       1. ├─eph::organize_panels(...)
       2. │ └─... %>% ...
       3. ├─dplyr::mutate(...)
       4. ├─dplyr:::mutate.data.frame(...)
       5. │ └─dplyr:::mutate_cols(.data, dplyr_quosures(...), by)
       6. │   ├─base::withCallingHandlers(...)
       7. │   └─dplyr:::mutate_col(dots[[i]], data, mask, new_columns)
       8. │     └─mask$eval_all_mutate(quo)
       9. │       └─dplyr (local) eval()
      10. ├─dplyr::case_when(...)
      11. │ └─dplyr:::vec_case_when(...)
      12. │   └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
      13. └─vctrs:::stop_assert_size(...)
      14.   └─vctrs:::stop_assert(...)
      15.     └─vctrs:::stop_vctrs(...)
      16.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
       11  2016        2     2        4       0.0278         0.0194 
       12  2016        2     2        5       0.0349         0.0184 
       13  2016        2     2        6       0              0      
       14  2016        2     2        7       0.0125         0.0109 
       [ FAIL 1 | WARN 0 | SKIP 11 | PASS 38 ]
       
       ══ Skipped tests (11) ══════════════════════════════════════════════════════════
       • On CRAN (11): 'test-get_eahu.R:2:3', 'test-get_eahu.R:10:3',
         'test-get_eahu.R:18:3', 'test-get_microdata.R:4:3',
         'test-get_microdata.R:13:3', 'test-get_microdata.R:22:3',
         'test-get_poverty_lines.R:4:3', 'test-get_poverty_lines.R:12:3',
         'test-get_total_urbano.R:10:3', 'test-is_in_github.R:3:5',
         'test-is_in_github.R:10:5'
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Error ('test-organize_panels.R:5:3'): consistencia constante ────────────────
       <dplyr:::mutate_error/rlang_error/error/condition>
       Error in `dplyr::mutate(., Periodo = dplyr::case_when(window == "anual" ~ 
           Periodo - 1, window == "trimestral" ~ Periodo - 0.25))`: ℹ In argument: `Periodo = dplyr::case_when(...)`.
       Caused by error in `dplyr::case_when()`:
       ! `..1 (right)` must have size 1, not size 3996.
       
       [ FAIL 1 | WARN 0 | SKIP 11 | PASS 38 ]
       Error: Test failures
       Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     Error(s) in re-building vignettes:
     --- re-building ‘eph.Rmd’ using rmarkdown
     
     Quitting from eph.Rmd:123-151 [unnamed-chunk-7]
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     NULL
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'eph.Rmd' failed with diagnostics:
     ℹ In argument: `Periodo = dplyr::case_when(...)`.
     Caused by error in `dplyr::case_when()`:
     ! `..1 (right)` must have size 1, not size 115642.
     --- failed re-building ‘eph.Rmd’
     
     --- re-building ‘estimacion_pobreza.Rmd’ using rmarkdown
     trying URL 'https://github.com/holatam/data/raw/master/eph/canasta/canastas.rds'
     Content type 'application/octet-stream' length 3499 bytes
     ==================================================
     downloaded 3499 bytes
     ```

## In both

*   checking data for non-ASCII characters ... NOTE
     ```
       Note: found 1 marked Latin-1 string
       Note: found 1508 marked UTF-8 strings
     ```

# epikit (0.1.6)

* GitHub: https://github.com/R4EPI/epikit
* Github mirror: https://github.com/cran/epikit
* Maintainer: Zhian N. Kamvar <zkamvar@gmail.com>

Run `revdepcheck::cloud_details(, "epikit")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
       > test_check("epikit")
       [ FAIL 1 | WARN 1 | SKIP 1 | PASS 120 ]
       
       ══ Skipped tests (1) ═══════════════════════════════════════════════════════════
       • On CRAN (1): 'test-relabel_proportions.R:22:3'
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Error ('test-age-categories.R:179:3'): years alone give years ───────────────
       <vctrs_error_assert_size/vctrs_error_assert/vctrs_error/rlang_error/error/condition>
       Error in `dplyr::case_when(!is.na(da) ~ dac, !is.na(we) ~ wec, !is.na(mo) ~ 
           moc, TRUE ~ yec)`: `..4 (right)` must have size 1, not size 30.
       Backtrace:
           ▆
        1. ├─epikit::group_age_categories(df, years = years, one_column = FALSE) at test-age-categories.R:179:3
        2. │ └─dplyr::case_when(...)
        3. │   └─dplyr:::vec_case_when(...)
        4. │     └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
        5. └─vctrs:::stop_assert_size(...)
        6.   └─vctrs:::stop_assert(...)
        7.     └─vctrs:::stop_vctrs(...)
        8.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
       
       [ FAIL 1 | WARN 1 | SKIP 1 | PASS 120 ]
       Error: Test failures
       Execution halted
     ```

# findSVI (0.2.0)

* GitHub: https://github.com/heli-xu/findSVI
* Github mirror: https://github.com/cran/findSVI
* Maintainer: Heli Xu <xuheli91@gmail.com>

Run `revdepcheck::cloud_details(, "findSVI")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
       ── Error ('test-get_svi.R:77:3'): 2018 svi calculation works ───────────────────
       <dplyr:::mutate_error/rlang_error/error/condition>
       Error in `dplyr::mutate(., dplyr::across(tidyselect::all_of(EP_var_name), 
           ~round(.x, 1)), E_AGE65 = dplyr::case_when(year >= 2017 ~ 
           E_AGE65, TRUE ~ round(E_AGE65, 0)))`: i In argument: `E_AGE65 = dplyr::case_when(...)`.
       Caused by error in `dplyr::case_when()`:
       ! `..1 (right)` must have size 1, not size 67.
       ── Error ('test-get_svi.R:113:3'): 2016 svi calculation works ──────────────────
       <dplyr:::mutate_error/rlang_error/error/condition>
       Error in `dplyr::mutate(., dplyr::across(tidyselect::all_of(EP_var_name), 
           ~round(.x, 1)), E_AGE65 = dplyr::case_when(year >= 2017 ~ 
           E_AGE65, TRUE ~ round(E_AGE65, 0)))`: i In argument: `E_AGE65 = dplyr::case_when(...)`.
       Caused by error in `dplyr::case_when()`:
       ! `..1 (right)` must have size 1, not size 67.
       ── Error ('test-get_svi.R:149:3'): 2014 svi calculation works ──────────────────
       <dplyr:::mutate_error/rlang_error/error/condition>
       Error in `dplyr::mutate(., dplyr::across(tidyselect::all_of(EP_var_name), 
           ~round(.x, 1)), E_AGE65 = dplyr::case_when(year >= 2017 ~ 
           E_AGE65, TRUE ~ round(E_AGE65, 0)))`: i In argument: `E_AGE65 = dplyr::case_when(...)`.
       Caused by error in `dplyr::case_when()`:
       ! `..1 (right)` must have size 1, not size 67.
       
       [ FAIL 5 | WARN 0 | SKIP 0 | PASS 0 ]
       Error: Test failures
       Execution halted
     ```

## In both

*   checking data for non-ASCII characters ... NOTE
     ```
       Note: found 298 marked UTF-8 strings
     ```

# flowchart (0.9.0)

* GitHub: https://github.com/bruigtp/flowchart
* Github mirror: https://github.com/cran/flowchart
* Maintainer: Pau Satorra <psatorra@igtp.cat>

Run `revdepcheck::cloud_details(, "flowchart")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
         'test-fc_export.R:4:3', 'test-fc_export.R:9:3', 'test-fc_export.R:14:3',
         'test-fc_export.R:19:3', 'test-fc_export.R:24:3', 'test-fc_export.R:29:3',
         'test-fc_export.R:34:3', 'test-fc_export.R:39:3', 'test-fc_filter.R:4:3',
         'test-fc_filter.R:9:3', 'test-fc_filter.R:14:3', 'test-fc_filter.R:20:3',
         'test-fc_filter.R:25:3', 'test-fc_filter.R:30:3', 'test-fc_filter.R:35:3',
         'test-fc_filter.R:90:3', 'test-fc_split.R:4:3', 'test-fc_split.R:9:3',
         'test-fc_split.R:14:3', 'test-fc_split.R:19:3', 'test-fc_split.R:24:3',
         'test-fc_view.R:4:3', 'test-utils.R:51:3'
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Error ('test-fc_stack.R:35:3'): successfully handles stacking when unite = TRUE ──
       <dplyr:::mutate_error/rlang_error/error/condition>
       Error in `dplyr::mutate(dplyr::group_by(dplyr::mutate(do.call(rbind, purrr::map(seq_along(object$fc), 
           ~dplyr::mutate(object$fc[[.x]], fc = .x))), y = update_y_stack_unite(.data$y, 
           .data$x, .data$type), change = dplyr::case_when(is.na(dplyr::lag(.data$fc)) ~ 
           FALSE, fc != dplyr::lag(.data$fc) ~ TRUE, TRUE ~ FALSE)), 
           .data$y), type = dplyr::case_when(any(.data$change) ~ "stack", 
           TRUE ~ .data$type))`: i In argument: `type = dplyr::case_when(any(.data$change) ~ "stack", TRUE ~ .data$type)`.
       i In group 1: `y = 0.2`.
       Caused by error in `dplyr::case_when()`:
       ! `..2 (right)` must have size 1, not size 3.
       
       [ FAIL 1 | WARN 0 | SKIP 29 | PASS 177 ]
       Error: Test failures
       Execution halted
     ```

# fluxible (1.3.3)

* GitHub: https://github.com/Plant-Functional-Trait-Course/fluxible
* Github mirror: https://github.com/cran/fluxible
* Maintainer: Joseph Gaudard <joseph.gaudard@pm.me>

Run `revdepcheck::cloud_details(, "fluxible")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     > data(co2_conc)
     > slopes <- flux_fitting(co2_conc, conc, datetime, fit_type = "exp_zhao18")
     Error in `mutate()`:
     ℹ In argument: `f_start = case_when(...)`.
     ℹ In group 1: `f_fluxid = 1`.
     Caused by error in `case_when()`:
     ! `..1 (right)` must have size 1, not size 210.
     Backtrace:
          ▆
       1. ├─fluxible::flux_fitting(co2_conc, conc, datetime, fit_type = "exp_zhao18")
       2. │ ├─dplyr::mutate(...)
       3. │ └─dplyr:::mutate.data.frame(...)
       4. │   └─dplyr:::mutate_cols(.data, dplyr_quosures(...), by)
       5. │     ├─base::withCallingHandlers(...)
       6. │     └─dplyr:::mutate_col(dots[[i]], data, mask, new_columns)
       7. │       └─mask$eval_all_mutate(quo)
       8. │         └─dplyr (local) eval()
       9. ├─dplyr::case_when(...)
      10. │ └─dplyr:::vec_case_when(...)
      11. │   └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
      12. └─vctrs:::stop_assert_size(...)
      13.   └─vctrs:::stop_assert(...)
      14.     └─vctrs:::stop_vctrs(...)
      15.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
               f_datetime
           }
       } >= {
           {
               f_end
           }
       } ~ "cut", TRUE ~ "keep"), f_cut = as_factor(.data$f_cut), f_n_conc = sum(!is.na(.data[[name_conc]])), 
           .by = {
               {
                   f_fluxid
               }
           })`: i In argument: `f_start = case_when(...)`.
       i In group 1: `f_fluxid = 1`.
       Caused by error in `case_when()`:
       ! `..1 (right)` must have size 1, not size 210.
       
       [ FAIL 82 | WARN 0 | SKIP 32 | PASS 30 ]
       Deleting unused snapshots:
       • flux_plot/ggssave-and-print.svg
       • flux_plot/longpdf-and-print.svg
       • flux_plot/plot-as-an-object.svg
       • flux_plot/plot-for-linear-fit.svg
       • flux_plot/plot-with-custom-facet-id.svg
       Error: Test failures
       Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     ...
     ℹ In argument: `f_start = case_when(...)`.
     ℹ In group 1: `f_fluxid = "ACJ_C_10apr2019_plot1_p.txt"`.
     Caused by error in `case_when()`:
     ! `..1 (right)` must have size 1, not size 90.
     --- failed re-building ‘li7500.Rmd’
     
     --- re-building ‘two-gases.Rmd’ using rmarkdown
     
     Quitting from two-gases.Rmd:43-59 [fitting-twogases]
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     NULL
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'two-gases.Rmd' failed with diagnostics:
     ℹ In argument: `f_start = case_when(...)`.
     ℹ In group 1: `f_fluxid = 1`.
     Caused by error in `case_when()`:
     ! `..1 (right)` must have size 1, not size 180.
     --- failed re-building ‘two-gases.Rmd’
     
     SUMMARY: processing the following files failed:
       ‘fluxible.Rmd’ ‘li7500.Rmd’ ‘two-gases.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

# ggfacto (0.3.2)

* GitHub: https://github.com/BriceNocenti/ggfacto
* Github mirror: https://github.com/cran/ggfacto
* Maintainer: Brice Nocenti <brice.nocenti@gmail.com>

Run `revdepcheck::cloud_details(, "ggfacto")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
      10. │             └─pillar:::emit_pillars(x, tier_widths, cb_pillars, focus)
      11. │               └─pillar:::do_emit_focus_pillars(x, tier_widths, cb, focus)
      12. │                 └─pillar:::do_emit_pillars(x, tier_widths, cb)
      13. │                   ├─pillar::ctl_new_pillar_list(...)
      14. │                   └─pillar:::ctl_new_pillar_list.tbl(...)
      15. │                     └─pillar:::new_data_frame_pillar_list(...)
      16. │                       ├─pillar::ctl_new_pillar_list(...)
      17. │                       └─pillar:::ctl_new_pillar_list.tbl(...)
      18. │                         ├─pillar::ctl_new_pillar(controller, x, width, ..., title = prepare_title(title))
      19. │                         └─pillar:::ctl_new_pillar.tbl(controller, x, width, ..., title = prepare_title(title))
      20. │                           └─pillar::pillar(x, title, if (!is.null(width)) max0(width))
      21. │                             ├─pillar:::pillar_from_shaft(...)
      22. │                             │ └─pillar:::get_min_width(data)
      23. │                             │   └─attr(x, "min_width", exact = TRUE) %||% get_width(x)
      24. │                             ├─pillar::pillar_shaft(x, ...)
      25. │                             └─tabxplor:::pillar_shaft.tabxplor_fmt(x, ...)
      26. │                               └─tabxplor:::get_reference(x, mode = "all_totals")
      27. │                                 └─dplyr::case_when(...)
      28. │                                   └─dplyr:::vec_case_when(...)
      29. │                                     └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
      30. └─vctrs:::stop_assert_size(...)
      31.   └─vctrs:::stop_assert(...)
      32.     └─vctrs:::stop_vctrs(...)
      33.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
     Execution halted
     ```

# gglyph (0.2.0)

* GitHub: https://github.com/valentinsvelev/gglyph
* Github mirror: https://github.com/cran/gglyph
* Maintainer: Valentin Velev <valentin.velev@uni-konstanz.de>

Run `revdepcheck::cloud_details(, "gglyph")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     
     > ### Name: geom_glyph
     > ### Title: Create a directed network-style graph
     > ### Aliases: geom_glyph
     > 
     > ### ** Examples
     > 
     > # For non-grouped/-facetted plot
     > data <- gglyph::generate_mock_data(n_groups = 1)
     > 
     > ggplot2::ggplot(data = data) +
     +   gglyph::geom_glyph()
     Error in `case_when()`:
     ! `..1 (right)` must have size 1, not size 5.
     Backtrace:
         ▆
      1. ├─gglyph::geom_glyph()
      2. │ └─dplyr::case_when(...)
      3. │   └─dplyr:::vec_case_when(...)
      4. │     └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
      5. └─vctrs:::stop_assert_size(...)
      6.   └─vctrs:::stop_assert(...)
      7.     └─vctrs:::stop_vctrs(...)
      8.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
       ── Error ('test-geom_glyph.R:26:3'): geom_glyph - edge_fill defaults to edge_colour when edge_fill is NULL ──
       <vctrs_error_assert_size/vctrs_error_assert/vctrs_error/rlang_error/error/condition>
       Error in `case_when(node_count == 3 ~ rep(c(0.5, 0.5, 0.5), length.out = node_count), 
           node_count == 4 ~ rep(c(0.5, 0, 0.5, 1), length.out = node_count), 
           node_count == 5 ~ rep(c(0.5, 0, 0.5, 0.5, 1), length.out = node_count), 
           node_count == 6 ~ rep(c(0.5, 0, 0, 0.5, 1, 1), length.out = node_count), 
           node_count == 7 ~ rep(c(0.5, 0, 0, 0, 1, 1, 1), length.out = node_count), 
           node_count == 8 ~ rep(c(0.5, 0, -1.45, 0, 0.5, 1, 2.45, 1), 
               length.out = node_count), node_count == 9 ~ rep(c(0.5, 
               0, -1.45, 0, 0, 1, 1, 2.45, 1), length.out = node_count), 
           TRUE ~ rep(0.5, node_count))`: `..1 (right)` must have size 1, not size 5.
       Backtrace:
           ▆
        1. ├─gglyph::geom_glyph(data = df, edge_colour = "red") at test-geom_glyph.R:26:3
        2. │ └─dplyr::case_when(...)
        3. │   └─dplyr:::vec_case_when(...)
        4. │     └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
        5. └─vctrs:::stop_assert_size(...)
        6.   └─vctrs:::stop_assert(...)
        7.     └─vctrs:::stop_vctrs(...)
        8.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
       
       [ FAIL 2 | WARN 0 | SKIP 0 | PASS 31 ]
       Error: Test failures
       Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     Error(s) in re-building vignettes:
       ...
     --- re-building ‘gglyph.Rmd’ using rmarkdown
     
     Quitting from gglyph.Rmd:134-143 [example_glyphs_base]
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     NULL
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'gglyph.Rmd' failed with diagnostics:
     `..1 (right)` must have size 1, not size 5.
     --- failed re-building ‘gglyph.Rmd’
     
     SUMMARY: processing the following file failed:
       ‘gglyph.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

# ggstatsplot (0.13.2)

* GitHub: https://github.com/IndrajeetPatil/ggstatsplot
* Github mirror: https://github.com/cran/ggstatsplot
* Maintainer: Indrajeet Patil <patilindrajeet.science@gmail.com>

Run `revdepcheck::cloud_details(, "ggstatsplot")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
       • ggscatterstats/robust-correlation-with-nas.svg
       • ggwithinstats/centrality-path-can-be-turned-off.svg
       • ggwithinstats/defaults-plots-more-than-two-groups.svg
       • ggwithinstats/defaults-plots-two-groups.svg
       • ggwithinstats/grouped-plots-default.svg
       • pairwise-ggsignif/between-bayes.svg
       • pairwise-ggsignif/between-non-parametric-all.svg
       • pairwise-ggsignif/between-non-parametric-only-non-significant.svg
       • pairwise-ggsignif/between-non-parametric-only-significant.svg
       • pairwise-ggsignif/between-parametric-all.svg
       • pairwise-ggsignif/between-parametric-only-significant.svg
       • pairwise-ggsignif/between-robust-all.svg
       • pairwise-ggsignif/between-robust-only-non-significant.svg
       • pairwise-ggsignif/between-robust-only-significant.svg
       • pairwise-ggsignif/within-bayes.svg
       • pairwise-ggsignif/within-non-parametric-all.svg
       • pairwise-ggsignif/within-non-parametric-only-non-significant.svg
       • pairwise-ggsignif/within-non-parametric-only-significant.svg
       • pairwise-ggsignif/within-parametric-all.svg
       • pairwise-ggsignif/within-parametric-only-significant.svg
       • pairwise-ggsignif/within-robust-all.svg
       • pairwise-ggsignif/within-robust-only-non-significant.svg
       • pairwise-ggsignif/within-robust-only-significant.svg
       Error: Test failures
       Execution halted
     ```

# ggsurveillance (0.5.1)

* GitHub: https://github.com/biostats-dev/ggsurveillance
* Github mirror: https://github.com/cran/ggsurveillance
* Maintainer: Alexander Bartel <alexander.bartel@fu-berlin.de>

Run `revdepcheck::cloud_details(, "ggsurveillance")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
         intersect, setdiff, setequal, union
     
     > library(tidyr)
     > 
     > set.seed(123)
     > df_6cat <- data.frame(matrix(sample(1:6, 600, replace = TRUE), ncol = 6)) |>
     +   mutate_all(~ ordered(., labels = c("+++", "++", "+", "-", "--", "---"))) |>
     +   pivot_longer(cols = everything())
     > 
     > ggplot(df_6cat, aes(y = name, fill = value)) +
     +   geom_bar_diverging() + # Bars
     +   stat_diverging() + # Labels
     +   scale_x_continuous_diverging() + # Scale
     +   theme_classic()
     Warning: Computation failed in `stat_diverging()`.
     Caused by error in `case_when()`:
     ! `..1 (right)` must have size 1, not size 2.
     Warning: Computation failed in `stat_diverging()`.
     Caused by error in `case_when()`:
     ! `..1 (right)` must have size 1, not size 2.
     Warning in max(abs(x - center)) :
       no non-missing arguments to max; returning -Inf
     Error in sign(x) : non-numeric argument to mathematical function
     Calls: <Anonymous> ... <Anonymous> -> get_limits -> <Anonymous> -> <Anonymous> -> limits
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
       • geom_vline_year/3-geom-vline-year-day-mix.svg
       • geom_vline_year/3-geom-vline-year-week-mix.svg
       • geom_vline_year/3-geom-vline-year-week2.svg
       • geom_vline_year/4-geom-vline-year-epiweek.svg
       • geom_vline_year/4-geom-vline-year-epiweek2.svg
       • geom_vline_year/5-geom-hline-year-basic2.svg
       • geom_vline_year/5-geom-hline-year-epiweek.svg
       • geom_vline_year/5-geom-hline-year-isoweek.svg
       • geom_vline_year/5-geom-hline-year-week.svg
       • scale_discrete_reverse/2-scale-reverse.svg
       • scale_discrete_reverse/3-scale-reverse.svg
       • scale_discrete_reverse/4-scale-reverse.svg
       • stat_last_value/1-stat-last-value-multiple-lines.svg
       • stat_last_value/2-geom-text-last-value-labeller.svg
       • stat_last_value/2-geom-text-last-value-percent.svg
       • stat_last_value/3-geom-label-last-value-nudge.svg
       • stat_last_value/4-geom-text-last-value-repel-min-segment.svg
       • stat_last_value/5-geom-label-last-value-repel-custom.svg
       • stat_last_value/6-stat-last-value-abs-nudge-date.svg
       • stat_last_value/7-stat-last-value-na-at-end.svg
       Error: Test failures
       In addition: Warning message:
       In Sys.setlocale("LC_ALL", "en_GB.UTF-8") :
         OS reports request to set locale to "en_GB.UTF-8" cannot be honored
       Execution halted
     ```

## In both

*   checking data for non-ASCII characters ... NOTE
     ```
       Note: found 364 marked UTF-8 strings
     ```

# goldilocks (0.4.0)

* GitHub: https://github.com/graemeleehickey/goldilocks
* Github mirror: https://github.com/cran/goldilocks
* Maintainer: Graeme L. Hickey <graemeleehickey@gmail.com>

Run `revdepcheck::cloud_details(, "goldilocks")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
       Running ‘testthat.R’
     Running the tests in ‘tests/testthat.R’ failed.
     Complete output:
       > library(testthat)
       > library(goldilocks)
       Loading required package: survival
       > 
       > test_check("goldilocks")
       [ FAIL 1 | WARN 0 | SKIP 1 | PASS 17 ]
       
       ══ Skipped tests (1) ═══════════════════════════════════════════════════════════
       • On CRAN (1): 'test-survival_adapt.R:81:3'
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Error ('test-survival_adapt.R:54:3'): survival_adapt-cox ────────────────────
       Error in `if (prob_now > prob_ha) {
           expected_success_test <- expected_success_test + 1
       }`: missing value where TRUE/FALSE needed
       Backtrace:
           ▆
        1. └─goldilocks::survival_adapt(...) at test-survival_adapt.R:54:3
       
       [ FAIL 1 | WARN 0 | SKIP 1 | PASS 17 ]
       Error: Test failures
       Execution halted
     ```

# Goodreader (0.1.2)

* GitHub: https://github.com/chaoliu-cl/Goodreader
* Github mirror: https://github.com/cran/Goodreader
* Maintainer: Chao Liu <chaoliu@cedarville.edu>

Run `revdepcheck::cloud_details(, "Goodreader")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
       <dplyr:::mutate_error/rlang_error/error/condition>
       Error in `dplyr::mutate(., period = dplyr::case_when(time_period == "day" ~ 
           as.character(.data$review_date), time_period == "week" ~ 
           as.character(lubridate::floor_date(.data$review_date, "week")), 
           time_period == "month" ~ as.character(lubridate::floor_date(.data$review_date, 
               "month")), time_period == "year" ~ as.character(lubridate::floor_date(.data$review_date, 
               "year")), TRUE ~ as.character(lubridate::floor_date(.data$review_date, 
               "month"))))`: i In argument: `period = dplyr::case_when(...)`.
       Caused by error in `dplyr::case_when()`:
       ! `..1 (right)` must have size 1, not size 5.
       ── Error ('test-sentiment_plots.R:49:5'): sentiment_trend function handles different time periods ──
       <dplyr:::mutate_error/rlang_error/error/condition>
       Error in `dplyr::mutate(., period = dplyr::case_when(time_period == "day" ~ 
           as.character(.data$review_date), time_period == "week" ~ 
           as.character(lubridate::floor_date(.data$review_date, "week")), 
           time_period == "month" ~ as.character(lubridate::floor_date(.data$review_date, 
               "month")), time_period == "year" ~ as.character(lubridate::floor_date(.data$review_date, 
               "year")), TRUE ~ as.character(lubridate::floor_date(.data$review_date, 
               "month"))))`: i In argument: `period = dplyr::case_when(...)`.
       Caused by error in `dplyr::case_when()`:
       ! `..1 (right)` must have size 1, not size 5.
       
       [ FAIL 2 | WARN 0 | SKIP 4 | PASS 45 ]
       Error: Test failures
       Execution halted
     ```

# gtreg (0.4.1)

* GitHub: https://github.com/shannonpileggi/gtreg
* Github mirror: https://github.com/cran/gtreg
* Maintainer: Shannon Pileggi <shannon.pileggi@gmail.com>

Run `revdepcheck::cloud_details(, "gtreg")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     Running examples in ‘gtreg-Ex.R’ failed
     The error most likely occurred in:
     
     > ### Name: style_xxx
     > ### Title: Style numbers as x's
     > ### Aliases: style_xxx
     > 
     > ### ** Examples
     > 
     > style_xxx(7:10, digits = 0)
     Error in `dplyr::case_when()`:
     ! `..1 (right)` must have size 1, not size 4.
     Backtrace:
         ▆
      1. ├─gtreg::style_xxx(7:10, digits = 0)
      2. │ └─dplyr::case_when(...)
      3. │   └─dplyr:::vec_case_when(...)
      4. │     └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
      5. └─vctrs:::stop_assert_size(...)
      6.   └─vctrs:::stop_assert(...)
      7.     └─vctrs:::stop_vctrs(...)
      8.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
        12.   └─vctrs:::stop_assert(...)
        13.     └─vctrs:::stop_vctrs(...)
        14.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
       ── Error ('test-style_xxx.R:6:3'): style_xxx works ─────────────────────────────
       <vctrs_error_assert_size/vctrs_error_assert/vctrs_error/rlang_error/error/condition>
       Error in `dplyr::case_when(digits == 0 ~ paste(rep_len("x", width), collapse = "") %>% 
           rep_len(length(x)), TRUE ~ paste(rep_len("x", width - digits - 
           1), collapse = "") %>% rep_len(length(x)))`: `..1 (right)` must have size 1, not size 4.
       Backtrace:
            ▆
         1. ├─testthat::expect_equal(...) at test-style_xxx.R:6:3
         2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
         3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
         4. ├─gtreg::style_xxx(7:10, width = 2, digits = 0)
         5. │ └─dplyr::case_when(...)
         6. │   └─dplyr:::vec_case_when(...)
         7. │     └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
         8. └─vctrs:::stop_assert_size(...)
         9.   └─vctrs:::stop_assert(...)
        10.     └─vctrs:::stop_vctrs(...)
        11.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
       
       [ FAIL 2 | WARN 14 | SKIP 6 | PASS 85 ]
       Error: Test failures
       Execution halted
     ```

# heiscore (0.1.4)

* GitHub: https://github.com/abhrastat/heiscore
* Github mirror: https://github.com/cran/heiscore
* Maintainer: Vijetha Ramdas <vramdas06@gmail.com>

Run `revdepcheck::cloud_details(, "heiscore")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     Backtrace:
          ▆
       1. ├─heiscore::plotScore(...)
       2. │ └─heiscore::score(...)
       3. │   ├─... %>% tidyr::drop_na(score)
       4. │   └─heiscore:::simpleScore(finalScoringData, scoringVariable, age[1])
       5. │     └─... %>% ...
       6. ├─tidyr::drop_na(., score)
       7. ├─dplyr::mutate(...)
       8. ├─dplyr::select(., SEQN, WTDR2D, SEX, AGE, RACE_ETH, FAMINC, score)
       9. ├─dplyr::mutate(...)
      10. ├─dplyr:::mutate.data.frame(...)
      11. │ └─dplyr:::mutate_cols(.data, dplyr_quosures(...), by)
      12. │   ├─base::withCallingHandlers(...)
      13. │   └─dplyr:::mutate_col(dots[[i]], data, mask, new_columns)
      14. │     └─mask$eval_all_mutate(quo)
      15. │       └─dplyr (local) eval()
      16. ├─dplyr::case_when(...)
      17. │ └─dplyr:::vec_case_when(...)
      18. │   └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
      19. └─vctrs:::stop_assert_size(...)
      20.   └─vctrs:::stop_assert(...)
      21.     └─vctrs:::stop_vctrs(...)
      22.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
     Execution halted
     ```

## In both

*   checking installed package size ... NOTE
     ```
       installed size is  7.8Mb
       sub-directories of 1Mb or more:
         R   7.6Mb
     ```

# heuristicsmineR (0.3.0)

* GitHub: https://github.com/bupaverse/heuristicsmineR
* Github mirror: https://github.com/cran/heuristicsmineR
* Maintainer: Felix Mannhardt <f.mannhardt@tue.nl>

Run `revdepcheck::cloud_details(, "heuristicsmineR")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     ℹ In argument: `label = case_when(...)`.
     Caused by error in `case_when()`:
     ! `..1 (right)` must have size 1, not size 7.
     Backtrace:
          ▆
       1. ├─heuristicsmineR::causal_net(L_heur_1, threshold = 0.8)
       2. │ └─attr(type_nodes, "create_nodes")(bindings, type_nodes, extra_data)
       3. │   └─... %>% na.omit()
       4. ├─stats::na.omit(.)
       5. ├─dplyr::mutate(...)
       6. ├─dplyr::mutate(...)
       7. ├─dplyr:::mutate.data.frame(...)
       8. │ └─dplyr:::mutate_cols(.data, dplyr_quosures(...), by)
       9. │   ├─base::withCallingHandlers(...)
      10. │   └─dplyr:::mutate_col(dots[[i]], data, mask, new_columns)
      11. │     └─mask$eval_all_mutate(quo)
      12. │       └─dplyr (local) eval()
      13. ├─dplyr::case_when(...)
      14. │ └─dplyr:::vec_case_when(...)
      15. │   └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
      16. └─vctrs:::stop_assert_size(...)
      17.   └─vctrs:::stop_assert(...)
      18.     └─vctrs:::stop_vctrs(...)
      19.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
     Execution halted
     ```

## In both

*   checking installed package size ... NOTE
     ```
       installed size is  5.6Mb
       sub-directories of 1Mb or more:
         data   2.0Mb
         libs   3.2Mb
     ```

# iglu (4.2.2)

* GitHub: https://github.com/irinagain/iglu
* Github mirror: https://github.com/cran/iglu
* Maintainer: Irina Gaynanova <irinagn@umich.edu>

Run `revdepcheck::cloud_details(, "iglu")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     ℹ In argument: `gl = dplyr::case_when(...)`.
     ℹ In group 1: `level_group = 1`.
     Caused by error in `dplyr::case_when()`:
     ! `..1 (right)` must have size 1, not size 346.
     Backtrace:
          ▆
       1. ├─iglu::plot_daily(example_data_1_subject)
       2. │ └─... %>% ...
       3. ├─dplyr::reframe(...)
       4. ├─dplyr:::reframe.data.frame(...)
       5. │ └─dplyr:::summarise_cols(.data, dplyr_quosures(...), by, "reframe")
       6. │   ├─base::withCallingHandlers(...)
       7. │   └─dplyr:::map(quosures, summarise_eval_one, mask = mask)
       8. │     └─base::lapply(.x, .f, ...)
       9. │       └─dplyr (local) FUN(X[[i]], ...)
      10. │         └─mask$eval_all_summarise(quo)
      11. │           └─dplyr (local) eval()
      12. ├─dplyr::case_when(...)
      13. │ └─dplyr:::vec_case_when(...)
      14. │   └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
      15. └─vctrs:::stop_assert_size(...)
      16.   └─vctrs:::stop_assert(...)
      17.     └─vctrs:::stop_vctrs(...)
      18.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
     Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     Error(s) in re-building vignettes:
     --- re-building ‘AGP_and_Episodes.Rmd’ using rmarkdown
     
     Quitting from AGP_and_Episodes.Rmd:23-25 [unnamed-chunk-1]
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     NULL
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'AGP_and_Episodes.Rmd' failed with diagnostics:
     ℹ In argument: `gl = dplyr::case_when(...)`.
     ℹ In group 1: `level_group = 1`.
     Caused by error in `dplyr::case_when()`:
     ! `..1 (right)` must have size 1, not size 346.
     --- failed re-building ‘AGP_and_Episodes.Rmd’
     
     --- re-building ‘MAGE.Rmd’ using rmarkdown
     ```

# inspectdf (0.0.12.1)

* GitHub: https://github.com/alastairrushworth/inspectdf
* Github mirror: https://github.com/cran/inspectdf
* Maintainer: Alastair Rushworth <alastairmrushworth@gmail.com>

Run `revdepcheck::cloud_details(, "inspectdf")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     > data("starwars", package = "dplyr")
     > 
     > # Horizontal bar plot for categorical column composition
     > x <- inspect_cat(starwars) 
     > show_plot(x)
     Error in `group_by()`:
     ! Must group by variables found in `.data`.
     ✖ Column `col_name` is not found.
     Backtrace:
          ▆
       1. ├─inspectdf::show_plot(x)
       2. │ └─inspectdf:::plot_cat(x, ...)
       3. │   └─inspectdf:::collapse_levels(lvl_df, i)
       4. │     └─... %>% mutate(level_key = paste0(value, "-", col_name))
       5. ├─dplyr::mutate(., level_key = paste0(value, "-", col_name))
       6. ├─dplyr::arrange(., col_name)
       7. ├─dplyr::ungroup(.)
       8. ├─dplyr::mutate(., colvalstretch = colvalstretch * (1 - 0.8 * (1/length(colval))))
       9. ├─dplyr::mutate(...)
      10. ├─dplyr::mutate(., colval = cumsum(prop))
      11. ├─dplyr::group_by(., col_name)
      12. └─dplyr:::group_by.data.frame(., col_name)
      13.   └─dplyr::group_by_prepare(.data, ..., .add = .add, error_call = current_env())
      14.     └─rlang::abort(bullets, call = error_call)
     Execution halted
     ```

# iNZightPlots (2.16.0)

* GitHub: https://github.com/iNZightVIT/iNZightPlots
* Github mirror: https://github.com/cran/iNZightPlots
* Maintainer: Tom Elliott <tom.elliott@auckland.ac.nz>

Run `revdepcheck::cloud_details(, "iNZightPlots")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
       • {waffle} is not installed (2): 'test_dictionary.R:12:1',
         'test_interaction.R:27:5'
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Error ('test_datetimes.R:52:1'): (code run outside of `test_that()`) ────────
       <vctrs_error_assert_size/vctrs_error_assert/vctrs_error/rlang_error/error/condition>
       Error in `dplyr::case_when(pipe %in% c("dplyr", "%>%", "magrittr") ~ rlang::expr_deparse(expr), 
           TRUE ~ stringr::str_replace_all(rlang::expr_deparse(expr), 
               "%>%", "|>"))`: `..1 (right)` must have size 1, not size 2.
       Backtrace:
            ▆
         1. ├─iNZightTools::extract_part(...) at test_datetimes.R:52:1
         2. │ └─iNZightTools::extract_dt_comp(.data, varname, part, name)
         3. │   └─iNZightTools:::eval_code(expr)
         4. │     └─dplyr::case_when(...)
         5. │       └─dplyr:::vec_case_when(...)
         6. │         └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
         7. └─vctrs:::stop_assert_size(...)
         8.   └─vctrs:::stop_assert(...)
         9.     └─vctrs:::stop_vctrs(...)
        10.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
       
       [ FAIL 1 | WARN 0 | SKIP 13 | PASS 281 ]
       Error: Test failures
       Execution halted
     ```

## In both

*   checking package dependencies ... NOTE
     ```
     Package suggested but not available for checking: ‘waffle’
     ```

# iNZightTools (2.0.3)

* GitHub: https://github.com/iNZightVIT/iNZightTools
* Github mirror: https://github.com/cran/iNZightTools
* Maintainer: Tom Elliott <tom.elliott@auckland.ac.nz>

Run `revdepcheck::cloud_details(, "iNZightTools")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     > ### Name: aggregate_data
     > ### Title: Aggregate data by categorical variables
     > ### Aliases: aggregate_data aggregate_dt
     > 
     > ### ** Examples
     > 
     > aggregated <-
     +     aggregate_data(iris,
     +         group_vars = c("Species"),
     +         summaries = c("mean", "sd", "iqr")
     +     )
     Error in `dplyr::case_when()`:
     ! `..1 (right)` must have size 1, not size 9.
     Backtrace:
         ▆
      1. ├─iNZightTools::aggregate_data(...)
      2. │ └─iNZightTools:::eval_code(expr)
      3. │   └─dplyr::case_when(...)
      4. │     └─dplyr:::vec_case_when(...)
      5. │       └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
      6. └─vctrs:::stop_assert_size(...)
      7.   └─vctrs:::stop_assert(...)
      8.     └─vctrs:::stop_vctrs(...)
      9.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
        5. │       └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
        6. └─vctrs:::stop_assert_size(...)
        7.   └─vctrs:::stop_assert(...)
        8.     └─vctrs:::stop_vctrs(...)
        9.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
       ── Error ('test_standardize_vars.R:14:5'): Standardization works for surveys ───
       <vctrs_error_assert_size/vctrs_error_assert/vctrs_error/rlang_error/error/condition>
       Error in `dplyr::case_when(pipe %in% c("dplyr", "%>%", "magrittr") ~ rlang::expr_deparse(expr), 
           TRUE ~ stringr::str_replace_all(rlang::expr_deparse(expr), 
               "%>%", "|>"))`: `..1 (right)` must have size 1, not size 5.
       Backtrace:
           ▆
        1. ├─iNZightTools::standardize_vars(svy, c("api99", "api00")) at test_standardize_vars.R:14:5
        2. │ └─iNZightTools:::eval_code(expr)
        3. │   └─dplyr::case_when(...)
        4. │     └─dplyr:::vec_case_when(...)
        5. │       └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
        6. └─vctrs:::stop_assert_size(...)
        7.   └─vctrs:::stop_assert(...)
        8.     └─vctrs:::stop_vctrs(...)
        9.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
       
       [ FAIL 36 | WARN 0 | SKIP 4 | PASS 287 ]
       Error: Test failures
       Execution halted
     ```

# iNZightTS (2.0.2)

* GitHub: https://github.com/iNZightVIT/iNZightTS
* Github mirror: https://github.com/cran/iNZightTS
* Maintainer: Tom Elliott <tom.elliott@auckland.ac.nz>

Run `revdepcheck::cloud_details(, "iNZightTS")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     ! `..1 (right)` must have size 1, not size 54.
     Backtrace:
          ▆
       1. ├─iNZightTS::decomp(ts)
       2. │ ├─(function(.) structure(., class = c("inz_dcmp", class(.)), mult_fit = mult_fit))(...)
       3. │ │ └─base::structure(., class = c("inz_dcmp", class(.)), mult_fit = mult_fit)
       4. │ ├─rlang::inject(...)
       5. │ ├─iNZightTS:::.decomp(use_decomp_method(sm_model), x, var, mult_fit)
       6. │ └─iNZightTS:::.decomp.use_stl(...)
       7. │   ├─dplyr::mutate(...)
       8. │   └─dplyr:::mutate.data.frame(...)
       9. │     └─dplyr:::mutate_cols(.data, dplyr_quosures(...), by)
      10. │       ├─base::withCallingHandlers(...)
      11. │       └─dplyr:::mutate_col(dots[[i]], data, mask, new_columns)
      12. │         ├─base::withCallingHandlers(...)
      13. │         └─mask$eval_all_mutate(quo)
      14. │           └─dplyr (local) eval()
      15. ├─dplyr::case_when(mult_fit ~ exp(Australia), TRUE ~ as.numeric(Australia))
      16. │ └─dplyr:::vec_case_when(...)
      17. │   └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
      18. └─vctrs:::stop_assert_size(...)
      19.   └─vctrs:::stop_assert(...)
      20.     └─vctrs:::stop_vctrs(...)
      21.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
        17. │         └─dplyr:::mutate_cols(.data, dplyr_quosures(...), by)
        18. │           ├─base::withCallingHandlers(...)
        19. │           └─dplyr:::mutate_col(dots[[i]], data, mask, new_columns)
        20. │             ├─base::withCallingHandlers(...)
        21. │             └─mask$eval_all_mutate(quo)
        22. │               └─dplyr (local) eval()
        23. ├─dplyr::case_when(mult_fit ~ exp(Daily_Deaths), TRUE ~ as.numeric(Daily_Deaths))
        24. │ └─dplyr:::vec_case_when(...)
        25. │   └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
        26. ├─vctrs:::stop_assert_size(...)
        27. │ └─vctrs:::stop_assert(...)
        28. │   └─vctrs:::stop_vctrs(...)
        29. │     └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
        30. │       └─rlang:::signal_abort(cnd, .file)
        31. │         └─base::signalCondition(cnd)
        32. ├─dplyr (local) `<fn>`(`<vctrs___>`)
        33. │ └─rlang::abort(msg, call = call("across"), parent = cnd)
        34. │   └─rlang:::signal_abort(cnd, .file)
        35. │     └─base::signalCondition(cnd)
        36. └─dplyr (local) `<fn>`(`<rlng_rrr>`)
        37.   └─rlang::abort(message, class = error_class, parent = parent, call = error_call)
       
       [ FAIL 16 | WARN 16 | SKIP 0 | PASS 55 ]
       Error: Test failures
       Execution halted
     ```

## In both

*   checking Rd cross-references ... NOTE
     ```
     Package unavailable to check Rd xrefs: ‘plotly’
     ```

# mantis (0.4.3)

* GitHub: https://github.com/phuongquan/mantis
* Github mirror: https://github.com/cran/mantis
* Maintainer: T. Phuong Quan <phuong.quan@ndm.ox.ac.uk>

Run `revdepcheck::cloud_details(, "mantis")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
       ! `..1 (right)` must have size 1, not size 10.
       ── Error ('test-report.R:494:3'): mantis_report() appends timestamp to filename appropriately ──
       <dplyr:::mutate_error/rlang_error/error/condition>
       Error in `dplyr::mutate(dplyr::arrange(dplyr::group_by(dplyr::mutate(prepared_df, 
           item_order_final = dplyr::row_number()), dplyr::across(dplyr::all_of(item_cols_prefix(inputspec$item_cols)))), 
           timepoint), value_for_history = dplyr::case_when(plot_value_type == 
           "value" ~ as.numeric(value), plot_value_type == "delta" ~ 
           as.numeric(value) - dplyr::lag(as.numeric(value))))`: i In argument: `value_for_history = dplyr::case_when(...)`.
       i In group 1: `item.item = "a"`.
       Caused by error in `dplyr::case_when()`:
       ! `..1 (right)` must have size 1, not size 10.
       ── Error ('test-report.R:546:3'): mantis_report() creates report in wd when no path supplied ──
       <dplyr:::mutate_error/rlang_error/error/condition>
       Error in `dplyr::mutate(dplyr::arrange(dplyr::group_by(dplyr::mutate(prepared_df, 
           item_order_final = dplyr::row_number()), dplyr::across(dplyr::all_of(item_cols_prefix(inputspec$item_cols)))), 
           timepoint), value_for_history = dplyr::case_when(plot_value_type == 
           "value" ~ as.numeric(value), plot_value_type == "delta" ~ 
           as.numeric(value) - dplyr::lag(as.numeric(value))))`: i In argument: `value_for_history = dplyr::case_when(...)`.
       i In group 1: `item.item = "a"`.
       Caused by error in `dplyr::case_when()`:
       ! `..1 (right)` must have size 1, not size 10.
       
       [ FAIL 14 | WARN 0 | SKIP 3 | PASS 417 ]
       Error: Test failures
       Execution halted
     ```

# matlib (1.0.0)

* GitHub: https://github.com/friendly/matlib
* Github mirror: https://github.com/cran/matlib
* Maintainer: Michael Friendly <friendly@yorku.ca>

Run `revdepcheck::cloud_details(, "matlib")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     Warning in matrix2latex(cbind(A/2, b), fractions = TRUE) :
       Function is deprecated. See latexMatrix() and Eqn() for more recent approaches
     \left[
      \begin{array}{llll}
       1 & 1/2 & -1/2 & 8 \\ 
       -3/2 & -1/2 & 1 & -11 \\ 
       -1 & 1/2 & 1 & -3 \\ 
       \end{array} \right]
     > 
     > matrix2latex(A, digits=0, brackets="p", show.size = TRUE)
     Warning in matrix2latex(A, digits = 0, brackets = "p", show.size = TRUE) :
       Function is deprecated. See latexMatrix() and Eqn() for more recent approaches
     Error in `dplyr::case_when()`:
     ! `..1 (right)` must have size 1, not size 2.
     Backtrace:
         ▆
      1. ├─matlib::matrix2latex(A, digits = 0, brackets = "p", show.size = TRUE)
      2. │ └─dplyr::case_when(...)
      3. │   └─dplyr:::vec_case_when(...)
      4. │     └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
      5. └─vctrs:::stop_assert_size(...)
      6.   └─vctrs:::stop_assert(...)
      7.     └─vctrs:::stop_vctrs(...)
      8.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
     Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     Error(s) in re-building vignettes:
     --- re-building ‘data-beta.Rmd’ using rmarkdown
     ```

## In both

*   checking Rd cross-references ... NOTE
     ```
     Package unavailable to check Rd xrefs: ‘plotrix’
     ```

# metan (1.19.0)

* GitHub: https://github.com/nepem-ufsc/metan
* Github mirror: https://github.com/cran/metan
* Maintainer: Tiago Olivoto <tiagoolivoto@gmail.com>

Run `revdepcheck::cloud_details(, "metan")` for more info

## Newly broken

*   checking re-building of vignette outputs ... ERROR
     ```
     Error(s) in re-building vignettes:
     --- re-building ‘metan_start.Rmd’ using rmarkdown
     ```

# processmapR (0.5.7)

* GitHub: https://github.com/bupaverse/processmapr
* Github mirror: https://github.com/cran/processmapR
* Maintainer: Gert Janssenswillen <gert.janssenswillen@uhasselt.be>

Run `revdepcheck::cloud_details(, "processmapR")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     > ### ** Examples
     > 
     > library(processmapR)
     > library(eventdataR)
     > 
     > patients %>%
     +  dotted_chart(x = "absolute", sort = "start", color = "employee")
     Error in `case_when()`:
     ! `..1 (right)` must have size 1, not size 2.
     Backtrace:
          ▆
       1. ├─patients %>% ...
       2. ├─processmapR::dotted_chart(...)
       3. ├─processmapR:::dotted_chart.eventlog(...)
       4. │ └─log %>% dotted_chart_data(color, units) %>% ...
       5. ├─processmapR:::dotted_chart_plot(...)
       6. │ └─processmapR:::configure_x_aes(x)
       7. │   └─dplyr::case_when(...)
       8. │     └─dplyr:::vec_case_when(...)
       9. │       └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
      10. └─vctrs:::stop_assert_size(...)
      11.   └─vctrs:::stop_assert(...)
      12.     └─vctrs:::stop_vctrs(...)
      13.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
            ▆
         1. ├─testthat::expect_warning(patients_act %>% lined_chart(), NA) at test_lined_chart.R:203:3
         2. │ └─testthat:::expect_condition_matching(...)
         3. │   └─testthat:::quasi_capture(...)
         4. │     ├─testthat (local) .capture(...)
         5. │     │ └─base::withCallingHandlers(...)
         6. │     └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
         7. ├─patients_act %>% lined_chart()
         8. ├─processmapR::lined_chart(.)
         9. ├─processmapR:::lined_chart.activitylog(.)
        10. │ └─processmapR:::lined_chart.eventlog(...)
        11. │   └─log %>% lined_chart_data(color, units) %>% ...
        12. ├─processmapR:::lined_chart_plot(...)
        13. │ └─processmapR:::configure_x_aes_lined(x)
        14. │   └─dplyr::case_when(...)
        15. │     └─dplyr:::vec_case_when(...)
        16. │       └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
        17. └─vctrs:::stop_assert_size(...)
        18.   └─vctrs:::stop_assert(...)
        19.     └─vctrs:::stop_vctrs(...)
        20.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
       
       [ FAIL 54 | WARN 0 | SKIP 11 | PASS 47 ]
       Error: Test failures
       Execution halted
     ```

# radsafer (2.3.0)

* GitHub: https://github.com/markhogue/radsafer
* Github mirror: https://github.com/cran/radsafer
* Maintainer: Mark Hogue <mark.hogue.chp@gmail.com>

Run `revdepcheck::cloud_details(, "radsafer")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     Warning in dk_correct(RN_select = "Sr-90", date1 = "2009-01-01", date2 = "2019-01-01",  :
       Date mode only provides precision to the day. Use time_lapse mode if more precision is needed.
         RN half_life units decay_mode
      Sr-90     28.79     y         B-
     
     > 
     > #   RN_select and time_lapse (random sample)
     > dk_correct(
     +   RN_select = base::sample(RadData::ICRP_07.NDX$RN, 1),
     +   time_lapse = 1:10,
     +   time_unit = base::sample(c("y", "d", "h", "m", "s"), 1)
     + )
     Error in `dplyr::case_when()`:
     ! `..1 (right)` must have size 1, not size 10.
     Backtrace:
         ▆
      1. ├─radsafer::dk_correct(...)
      2. │ └─dplyr::case_when(...)
      3. │   └─dplyr:::vec_case_when(...)
      4. │     └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
      5. └─vctrs:::stop_assert_size(...)
      6.   └─vctrs:::stop_assert(...)
      7.     └─vctrs:::stop_vctrs(...)
      8.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
     Execution halted
     ```

# REDCapTidieR (1.2.4)

* GitHub: https://github.com/CHOP-CGTInformatics/REDCapTidieR
* Github mirror: https://github.com/cran/REDCapTidieR
* Maintainer: Richard Hanna <hannar1@chop.edu>

Run `revdepcheck::cloud_details(, "REDCapTidieR")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     Backtrace:
          ▆
       1. ├─... %>% dplyr::first()
       2. ├─dplyr::first(.)
       3. │ └─dplyr::nth(x, 1L, order_by = order_by, default = default, na_rm = na_rm)
       4. │   └─vctrs::vec_size(x)
       5. ├─dplyr::pull(., redcap_data)
       6. ├─REDCapTidieR::combine_checkboxes(...)
       7. │ └─REDCapTidieR:::get_metadata_spec(...)
       8. │   └─out %>% ...
       9. ├─dplyr::mutate(...)
      10. ├─dplyr:::mutate.data.frame(...)
      11. │ └─dplyr:::mutate_cols(.data, dplyr_quosures(...), by)
      12. │   ├─base::withCallingHandlers(...)
      13. │   └─dplyr:::mutate_col(dots[[i]], data, mask, new_columns)
      14. │     └─mask$eval_all_mutate(quo)
      15. │       └─dplyr (local) eval()
      16. ├─dplyr::case_when(...)
      17. │ └─dplyr:::vec_case_when(...)
      18. │   └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
      19. └─vctrs:::stop_assert_size(...)
      20.   └─vctrs:::stop_assert(...)
      21.     └─vctrs:::stop_vctrs(...)
      22.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
       ── Error ('test-combine_checkboxes.R:209:3'): combine_checkboxes works for multiple checkbox fields ──
       <dplyr:::mutate_error/rlang_error/error/condition>
       Error in `mutate(., .new_value = case_when(names_prefix != "" ~ paste(names_prefix, 
           .data$.value, sep = names_sep), .default = paste(names_prefix, 
           .data$.value, sep = "")))`: i In argument: `.new_value = case_when(...)`.
       Caused by error in `case_when()`:
       ! `..1 (right)` must have size 1, not size 4.
       ── Error ('test-combine_checkboxes.R:233:3'): combine_checkboxes works for multiple checkbox fields with concatenation ──
       <dplyr:::mutate_error/rlang_error/error/condition>
       Error in `mutate(., .new_value = case_when(names_prefix != "" ~ paste(names_prefix, 
           .data$.value, sep = names_sep), .default = paste(names_prefix, 
           .data$.value, sep = "")))`: i In argument: `.new_value = case_when(...)`.
       Caused by error in `case_when()`:
       ! `..1 (right)` must have size 1, not size 4.
       ── Error ('test-combine_checkboxes.R:254:3'): combine_checkboxes works for multiple checkbox fields with logicals ──
       <dplyr:::mutate_error/rlang_error/error/condition>
       Error in `mutate(., .new_value = case_when(names_prefix != "" ~ paste(names_prefix, 
           .data$.value, sep = names_sep), .default = paste(names_prefix, 
           .data$.value, sep = "")))`: i In argument: `.new_value = case_when(...)`.
       Caused by error in `case_when()`:
       ! `..1 (right)` must have size 1, not size 4.
       
       [ FAIL 8 | WARN 1 | SKIP 5 | PASS 315 ]
       Error: Test failures
       Execution halted
     ```

# retroharmonize (0.2.0)

* GitHub: https://github.com/rOpenGov/retroharmonize
* Github mirror: https://github.com/cran/retroharmonize
* Maintainer: Daniel Antal <daniel.antal@ceemid.eu>

Run `revdepcheck::cloud_details(, "retroharmonize")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     > ### ** Examples
     > 
     > test_survey <- retroharmonize::read_rds (
     +    file = system.file("examples", "ZA7576.rds",
     +                   package = "retroharmonize"), 
     +    id = "test"
     + )
     > example_metadata <- metadata_create (test_survey)
     Error in `dplyr::case_when()`:
     ! `..1 (right)` must have size 1, not size 56.
     Backtrace:
          ▆
       1. ├─retroharmonize::metadata_create(test_survey)
       2. │ └─tibble::tibble(...)
       3. │   └─tibble:::tibble_quos(xs, .rows, .name_repair)
       4. │     └─rlang::eval_tidy(xs[[j]], mask)
       5. ├─retroharmonize (local) to_list_column(.f = "labels")
       6. │ └─dplyr::case_when(...)
       7. │   └─dplyr:::vec_case_when(...)
       8. │     └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
       9. └─vctrs:::stop_assert_size(...)
      10.   └─vctrs:::stop_assert(...)
      11.     └─vctrs:::stop_vctrs(...)
      12.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
       ── Error ('test-suggest_var_names.R:14:1'): (code run outside of `test_that()`) ──
       <vctrs_error_assert_size/vctrs_error_assert/vctrs_error/rlang_error/error/condition>
       Error in `dplyr::case_when(.f == "na_labels" ~ sapply(survey, na_labels), 
           .f == "na_range" ~ sapply(survey, labelled::na_range), .f == 
               "valid_range" ~ sapply(survey, fn_valid_range), .f == 
               "labels" ~ sapply(survey, labelled::val_labels))`: `..1 (right)` must have size 1, not size 38.
       Backtrace:
            ▆
         1. ├─base::lapply(X = example_surveys, FUN = metadata_create) at test-suggest_var_names.R:14:1
         2. │ └─retroharmonize (local) FUN(X[[i]], ...)
         3. │   └─tibble::tibble(...)
         4. │     └─tibble:::tibble_quos(xs, .rows, .name_repair)
         5. │       └─rlang::eval_tidy(xs[[j]], mask)
         6. ├─retroharmonize (local) to_list_column(.f = "labels")
         7. │ └─dplyr::case_when(...)
         8. │   └─dplyr:::vec_case_when(...)
         9. │     └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
        10. └─vctrs:::stop_assert_size(...)
        11.   └─vctrs:::stop_assert(...)
        12.     └─vctrs:::stop_vctrs(...)
        13.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
       
       [ FAIL 8 | WARN 0 | SKIP 2 | PASS 80 ]
       Error: Test failures
       Execution halted
     ```

## In both

*   checking installed package size ... NOTE
     ```
       installed size is  5.7Mb
       sub-directories of 1Mb or more:
         doc        1.1Mb
         examples   1.9Mb
     ```

*   checking Rd files ... NOTE
     ```
     checkRd: (-1) harmonize_values.Rd:44: Lost braces; missing escapes or markup?
         44 | \item{perl}{Use perl-like regex? Defaults to {FALSE}.}
            |                                              ^
     ```

# rfars (2.0.0)

* GitHub: https://github.com/s87jackson/rfars
* Github mirror: https://github.com/cran/rfars
* Maintainer: Steve Jackson <steve.jackson@toxcel.com>

Run `revdepcheck::cloud_details(, "rfars")` for more info

## Newly broken

*   checking re-building of vignette outputs ... ERROR
     ```
     Error(s) in re-building vignettes:
     --- re-building ‘Alcohol_Counts.Rmd’ using rmarkdown
     trying URL 'https://zenodo.org/records/17162673/files/FARS.rds?download=1'
     Content type 'application/octet-stream' length 87386771 bytes (83.3 MB)
     ==================================================
     downloaded 83.3 MB
     
     --- finished re-building ‘Alcohol_Counts.Rmd’
     
     --- re-building ‘Counts.Rmd’ using rmarkdown
     ```

## In both

*   checking installed package size ... NOTE
     ```
       installed size is  5.1Mb
       sub-directories of 1Mb or more:
         data   2.0Mb
         help   1.9Mb
     ```

*   checking data for non-ASCII characters ... NOTE
     ```
       Note: found 53224 marked UTF-8 strings
     ```

# rFIA (1.1.2)

* GitHub: https://github.com/doserjef/rFIA
* Github mirror: https://github.com/cran/rFIA
* Maintainer: Jeffrey Doser <jwdoser@ncsu.edu>

Run `revdepcheck::cloud_details(, "rFIA")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     Caused by error in `dplyr::case_when()`:
     ! `..1 (right)` must have size 1, not size 3422.
     Backtrace:
          ▆
       1. ├─rFIA::vitalRates(db = fiaRI_mr, landType = "timber", treeType = "gs")
       2. │ └─base::lapply(...)
       3. │   └─rFIA (local) FUN(X[[i]], ...)
       4. │     └─... %>% ...
       5. ├─dplyr::select(...)
       6. ├─dplyr::mutate(...)
       7. ├─dplyr:::mutate.data.frame(...)
       8. │ └─dplyr:::mutate_cols(.data, dplyr_quosures(...), by)
       9. │   ├─base::withCallingHandlers(...)
      10. │   └─dplyr:::mutate_col(dots[[i]], data, mask, new_columns)
      11. │     └─mask$eval_all_mutate(quo)
      12. │       └─dplyr (local) eval()
      13. ├─rFIA:::vrAttHelper(...)
      14. │ └─dplyr::case_when(...)
      15. │   └─dplyr:::vec_case_when(...)
      16. │     └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
      17. └─vctrs:::stop_assert_size(...)
      18.   └─vctrs:::stop_assert(...)
      19.     └─vctrs:::stop_vctrs(...)
      20.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
     Execution halted
     ```

## In both

*   checking installed package size ... NOTE
     ```
       installed size is 16.5Mb
       sub-directories of 1Mb or more:
         R      2.3Mb
         data  13.0Mb
         help   1.1Mb
     ```

# risk.assessr (2.0.1)

* GitHub: https://github.com/Sanofi-Public/risk.assessr
* Github mirror: https://github.com/cran/risk.assessr
* Maintainer: Edward Gillian <edward.gillian-ext@sanofi.com>

Run `revdepcheck::cloud_details(, "risk.assessr")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
         9. │       ├─dplyr::mutate(...)
        10. │       └─dplyr:::mutate.data.frame(...)
        11. │         └─dplyr:::mutate_cols(.data, dplyr_quosures(...), by)
        12. │           ├─base::withCallingHandlers(...)
        13. │           └─dplyr:::mutate_col(dots[[i]], data, mask, new_columns)
        14. │             └─mask$eval_all_mutate(quo)
        15. │               └─dplyr (local) eval()
        16. ├─dplyr::case_when(...)
        17. │ └─dplyr:::vec_case_when(...)
        18. │   └─dplyr:::check_no_dim(condition, arg = condition_arg, call = call)
        19. │     └─cli::cli_abort("{.arg {arg}} can't be an array.", call = call)
        20. │       └─rlang::abort(...)
        21. │         └─rlang:::signal_abort(cnd, .file)
        22. │           └─base::signalCondition(cnd)
        23. ├─dplyr (local) `<fn>`(`<rlng_rrr>`)
        24. │ └─rlang::abort(message, class = error_class, parent = parent, call = error_call)
        25. │   └─rlang:::signal_abort(cnd, .file)
        26. │     └─base::signalCondition(cnd)
        27. └─purrr (local) `<fn>`(`<dply:::_>`)
        28.   └─cli::cli_abort(...)
        29.     └─rlang::abort(...)
       
       [ FAIL 1 | WARN 0 | SKIP 26 | PASS 609 ]
       Error: Test failures
       Execution halted
     ```

# scSpatialSIM (0.1.3.4)

* GitHub: https://github.com/FridleyLab/scSpatialSIM
* Github mirror: https://github.com/cran/scSpatialSIM
* Maintainer: Fridley Lab <fridley.lab@moffitt.org>

Run `revdepcheck::cloud_details(, "scSpatialSIM")` for more info

## Newly broken

*   checking re-building of vignette outputs ... ERROR
     ```
     Error(s) in re-building vignettes:
     --- re-building ‘a01_Introduction.Rmd’ using rmarkdown
     ```

# sdtmval (0.4.1)

* GitHub: https://github.com/skgithub14/sdtmval
* Github mirror: https://github.com/cran/sdtmval
* Maintainer: Stephen Knapp <stephen@knappconsultingllc.com>

Run `revdepcheck::cloud_details(, "sdtmval")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     Caused by error in `dplyr::case_when()`:
     ! `..2 (right)` must have size 1, not size 2.
     Backtrace:
          ▆
       1. ├─sdtmval::create_STAT(df = df, domain = "XX", nd_ind = "ND", nd_ind_cd = "Y")
       2. │ └─... %>% dplyr::ungroup()
       3. ├─dplyr::ungroup(.)
       4. ├─dplyr::select(., -tidyselect::all_of(tmp_var))
       5. ├─dplyr::filter(...)
       6. ├─dplyr::mutate(...)
       7. ├─dplyr::mutate(...)
       8. ├─dplyr:::mutate.data.frame(...)
       9. │ └─dplyr:::mutate_cols(.data, dplyr_quosures(...), by)
      10. │   ├─base::withCallingHandlers(...)
      11. │   └─dplyr:::mutate_col(dots[[i]], data, mask, new_columns)
      12. │     └─mask$eval_all_mutate(quo)
      13. │       └─dplyr (local) eval()
      14. ├─dplyr::case_when(all(!is.na(XXSTAT)) ~ paste0(domain, "ALL"), TRUE ~ XXTESTCD)
      15. │ └─dplyr:::vec_case_when(...)
      16. │   └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
      17. └─vctrs:::stop_assert_size(...)
      18.   └─vctrs:::stop_assert(...)
      19.     └─vctrs:::stop_vctrs(...)
      20.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
       > 
       > library(testthat)
       > library(sdtmval)
       > 
       > test_check("sdtmval")
       
       
       processing file: /tmp/RtmpdpEp5i/test_notebook.Rmd
       output file: /tmp/RtmpdpEp5i/test_notebook.R
       
       [ FAIL 1 | WARN 0 | SKIP 0 | PASS 25 ]
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Error ('test-methods.R:93:3'): STAT ─────────────────────────────────────────
       <dplyr:::mutate_error/rlang_error/error/condition>
       Error in `dplyr::mutate(., `:=`("{domain}TESTCD", dplyr::case_when(all(!is.na(!!rlang::sym(paste0(domain, 
           "STAT")))) ~ paste0(domain, "ALL"), TRUE ~ !!rlang::sym(paste0(domain, 
           "TESTCD")))))`: i In argument: `XXTESTCD = dplyr::case_when(...)`.
       i In group 3: `USUBJID = "Subject B"`, `VISIT = "Visit 1"`.
       Caused by error in `dplyr::case_when()`:
       ! `..2 (right)` must have size 1, not size 2.
       
       [ FAIL 1 | WARN 0 | SKIP 0 | PASS 25 ]
       Error: Test failures
       Execution halted
     ```

# shinyHugePlot (0.3.0)

* Github mirror: https://github.com/cran/shinyHugePlot
* Maintainer: Junta Tagusari <j.tagusari@eng.hokudai.ac.jp>

Run `revdepcheck::cloud_details(, "shinyHugePlot")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     +   x = noise_fluct$time, y = noise_fluct$f500, n_out = 1000
     +   )
     Error in `dplyr::case_when()`:
     ! `..1 (right)` must have size 1, not size 32001.
     Backtrace:
          ▆
       1. ├─agg$aggregate(x = noise_fluct$time, y = noise_fluct$f500, n_out = 1000)
       2. │ └─private$aggregate_exec(x, y, n_out)
       3. │   ├─private$LTTB(...)
       4. │   │ └─assertthat::assert_that(length(x) == length(y), msg = "x and y must be the same-length vectors")
       5. │   │   └─assertthat::see_if(..., env = env, msg = msg)
       6. │   │     ├─base::tryCatch(...)
       7. │   │     │ └─base (local) tryCatchList(expr, classes, parentenv, handlers)
       8. │   │     │   └─base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
       9. │   │     │     └─base (local) doTryCatch(return(expr), name, parentenv, handler)
      10. │   │     └─base::eval(assertion, env)
      11. │   │       └─base::eval(assertion, env)
      12. │   └─dplyr::case_when(...)
      13. │     └─dplyr:::vec_case_when(...)
      14. │       └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
      15. └─vctrs:::stop_assert_size(...)
      16.   └─vctrs:::stop_assert(...)
      17.     └─vctrs:::stop_vctrs(...)
      18.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
     Execution halted
     ```

# SingleCaseES (0.7.3)

* GitHub: https://github.com/jepusto/SingleCaseES
* Github mirror: https://github.com/cran/SingleCaseES
* Maintainer: James E. Pustejovsky <jepusto@gmail.com>

Run `revdepcheck::cloud_details(, "SingleCaseES")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
                   "n_A * n_B") ~ as.numeric(A * B), weighting %in% 
                   c("1/nA+1/nB", "1/nA + 1/nB", "1/n_A+1/n_B", "1/n_A + 1/n_B") ~ 
                   as.numeric(1/A + 1/B)))`: ℹ In argument: `weights = dplyr::case_when(...)`.
       Caused by error in `dplyr::case_when()`:
       ! `..1 (right)` must have size 1, not size 12.
       ── Error ('test-batch-calc-ES.R:397:18'): Synonyms for weighting argument in batch_calc_ES() are equivalent. ──
       <dplyr:::mutate_error/rlang_error/error/condition>
       Error in `dplyr::mutate(dplyr::ungroup(tidyr::pivot_wider(dplyr::summarise(dplyr::group_by(dplyr::mutate(dplyr::filter(dat, 
           !!rlang::sym(condition) %in% c(baseline_phase, intervention_phase)), 
           `:=`(!!rlang::sym(condition), ifelse(!!rlang::sym(condition) == 
               baseline_phase, "A", "B"))), !!!rlang::syms(c(grouping, 
           aggregate, condition))), n = dplyr::n(), .groups = "drop"), 
           names_from = !!rlang::sym(condition), values_from = n)), 
           weights = dplyr::case_when(weighting %in% c("nA", "n_A") ~ 
               as.numeric(A), weighting %in% c("nB", "n_B") ~ as.numeric(B), 
               weighting %in% c("nAnB", "nA*nB", "nA * nB", "n_A*n_B", 
                   "n_A * n_B") ~ as.numeric(A * B), weighting %in% 
                   c("1/nA+1/nB", "1/nA + 1/nB", "1/n_A+1/n_B", "1/n_A + 1/n_B") ~ 
                   as.numeric(1/A + 1/B)))`: ℹ In argument: `weights = dplyr::case_when(...)`.
       Caused by error in `dplyr::case_when()`:
       ! `..1 (right)` must have size 1, not size 12.
       
       [ FAIL 3 | WARN 0 | SKIP 2 | PASS 275 ]
       Error: Test failures
       Execution halted
     ```

# srppp (1.1.0)

* GitHub: https://github.com/agroscope-ch/srppp
* Github mirror: https://github.com/cran/srppp
* Maintainer: Johannes Ranke <johannes.ranke@agroscope.admin.ch>

Run `revdepcheck::cloud_details(, "srppp")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
               min_dosage, max_dosage)), rate = case_when(aggregation == 
           "mean" ~ (rate_min + rate_max)/2, aggregation == "max" ~ 
           rate_max, aggregation == "min" ~ rate_min), dosage = case_when(aggregation == 
           "mean" ~ (dosage_min + dosage_max)/2, aggregation == "max" ~ 
           dosage_max, aggregation == "min" ~ dosage_min))`: i In argument: `rate = case_when(...)`.
       Caused by error in `case_when()`:
       ! `..1 (right)` must have size 1, not size 7.
       ── Error ('test-use_rates.R:86:1'): (code run outside of `test_that()`) ────────
       <dplyr:::mutate_error/rlang_error/error/condition>
       Error in `mutate(mutate(mutate(product_uses, min_rate = na_if(min_rate, 
           0), max_rate = na_if(max_rate, 0), min_dosage = na_if(min_dosage, 
           0), max_dosage = na_if(max_dosage, 0)), rate_min = min_rate, 
           rate_max = if_else(is.na(max_rate), min_rate, max_rate), 
           dosage_min = min_dosage, dosage_max = if_else(is.na(max_dosage), 
               min_dosage, max_dosage)), rate = case_when(aggregation == 
           "mean" ~ (rate_min + rate_max)/2, aggregation == "max" ~ 
           rate_max, aggregation == "min" ~ rate_min), dosage = case_when(aggregation == 
           "mean" ~ (dosage_min + dosage_max)/2, aggregation == "max" ~ 
           dosage_max, aggregation == "min" ~ dosage_min))`: ℹ In argument: `rate = case_when(...)`.
       Caused by error in `case_when()`:
       ! `..1 (right)` must have size 1, not size 6.
       
       [ FAIL 2 | WARN 0 | SKIP 0 | PASS 16 ]
       Error: Test failures
       Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     ...
     
     Quitting from srppp.rmd:276-282 [unnamed-chunk-15]
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     NULL
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'srppp.rmd' failed with diagnostics:
     ℹ In argument: `rate = case_when(...)`.
     Caused by error in `case_when()`:
     ! `..1 (right)` must have size 1, not size 11.
     --- failed re-building ‘srppp.rmd’
     
     --- re-building ‘srppp_products_with_MO.rmd’ using rmarkdown
     trying URL 'https://www.blv.admin.ch/dam/blv/de/dokumente/zulassung-pflanzenschutzmittel/pflanzenschutzmittelverzeichnis/daten-pflanzenschutzmittelverzeichnis.zip.download.zip/Daten%20Pflanzenschutzmittelverzeichnis.zip'
     Content type 'application/x-zip-compressed' length 2501553 bytes (2.4 MB)
     ==================================================
     downloaded 2.4 MB
     
     --- finished re-building ‘srppp_products_with_MO.rmd’
     
     SUMMARY: processing the following file failed:
       ‘srppp.rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

# statsExpressions (1.7.1)

* GitHub: https://github.com/IndrajeetPatil/statsExpressions
* Github mirror: https://github.com/cran/statsExpressions
* Maintainer: Indrajeet Patil <patilindrajeet.science@gmail.com>

Run `revdepcheck::cloud_details(, "statsExpressions")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
         'test-two-sample-bayes.R:45:5', 'test-two-sample-nonparametric.R:17:5',
         'test-two-sample-nonparametric.R:39:5', 'test-two-sample-parametric.R:18:5',
         'test-two-sample-parametric.R:40:5', 'test-two-sample-parametric.R:60:5',
         'test-two-sample-parametric.R:80:5', 'test-two-sample-robust.R:20:5',
         'test-two-sample-robust.R:39:5', 'test-two-sample-robust.R:63:5',
         'test-two-sample-robust.R:83:5', 'test-oneway-anova-robust.R:31:5',
         'test-oneway-anova-robust.R:56:5'
       • On Linux (1): 'test-centrality-description.R:27:5'
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Error ('test-oneway-anova-bayes.R:13:5'): bayesian (between-subjects - anova) ──
       <subscriptOutOfBoundsError/error/condition>
       Error in `data$method[[1L]]`: subscript out of bounds
       Backtrace:
           ▆
        1. ├─base::suppressWarnings(...) at test-oneway-anova-bayes.R:13:5
        2. │ └─base::withCallingHandlers(...)
        3. └─statsExpressions::oneway_anova(...)
        4.   └─statsExpressions::add_expression_col(...)
        5.     └─base::grepl("contingency", data$method[[1L]], fixed = TRUE)
        6.       └─base::is.factor(x)
       
       [ FAIL 1 | WARN 0 | SKIP 64 | PASS 24 ]
       Error: Test failures
       Execution halted
     ```

# STICr (1.1.1)

* GitHub: https://github.com/HEAL-KGS/STICr
* Github mirror: https://github.com/cran/STICr
* Maintainer: Sam Zipper <samzipper@ku.edu>

Run `revdepcheck::cloud_details(, "STICr")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     Running examples in ‘STICr-Ex.R’ failed
     The error most likely occurred in:
     
     > ### Name: classify_wetdry
     > ### Title: classify_wetdry
     > ### Aliases: classify_wetdry
     > 
     > ### ** Examples
     > 
     > classified_df <-
     +   classify_wetdry(calibrated_stic_data,
     +     classify_var = "SpC", method = "absolute", threshold = 200
     +   )
     Error in `dplyr::if_else()`:
     ! `condition` can't be an array.
     Backtrace:
         ▆
      1. └─STICr::classify_wetdry(...)
      2.   └─dplyr::if_else(class_var >= threshold, "wet", "dry")
      3.     └─dplyr:::vec_case_when(...)
      4.       └─dplyr:::check_no_dim(condition, arg = condition_arg, call = call)
      5.         └─cli::cli_abort("{.arg {arg}} can't be an array.", call = call)
      6.           └─rlang::abort(...)
     Execution halted
     ```

# surveyexplorer (0.2.0)

* Github mirror: https://github.com/cran/surveyexplorer
* Maintainer: Liam Haller <liamhllr2@gmail.com>

Run `revdepcheck::cloud_details(, "surveyexplorer")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     Running examples in ‘surveyexplorer-Ex.R’ failed
     The error most likely occurred in:
     
     > ### Name: matrix_freq
     > ### Title: Matrix Frequency Plot
     > ### Aliases: matrix_freq
     > 
     > ### ** Examples
     > 
     >  #Array question (1-5)
     >   matrix_freq(berlinbears, dplyr::starts_with('p_'))
     Error in `dplyr::case_when()`:
     ! `..1 (right)` must have size 1, not size 500.
     Backtrace:
         ▆
      1. ├─surveyexplorer::matrix_freq(berlinbears, dplyr::starts_with("p_"))
      2. │ └─surveyexplorer::multi_summary(...)
      3. │   └─dplyr::case_when(...)
      4. │     └─dplyr:::vec_case_when(...)
      5. │       └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
      6. └─vctrs:::stop_assert_size(...)
      7.   └─vctrs:::stop_assert(...)
      8.     └─vctrs:::stop_vctrs(...)
      9.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
        8.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
       ── Failure ('test-multiplechoice.R:90:3'): Graph: subgroup, weights ────────────
       Expected `multi_freq(...)` to run without any errors.
       i Actually got a <vctrs_error_assert_size> with text:
         `..1 (right)` must have size 1, not size 500.
       ── Failure ('test-multiplechoice.R:96:3'): Graph: subgroup, weights, na.rm ─────
       Expected `multi_freq(...)` to run without any errors.
       i Actually got a <vctrs_error_assert_size> with text:
         `..1 (right)` must have size 1, not size 500.
       ── Failure ('test-multiplechoice.R:113:3'): Table: columns only ────────────────
       Expected `multi_table(berlinbears, question = dplyr::starts_with("will_eat"))` to run without any errors.
       i Actually got a <vctrs_error_assert_size> with text:
         `..1 (right)` must have size 1, not size 500.
       ── Failure ('test-multiplechoice.R:127:3'): Table: subgroup ────────────────────
       Expected `multi_table(...)` to run without any errors.
       i Actually got a <vctrs_error_assert_size> with text:
         `..1 (right)` must have size 1, not size 500.
       ── Failure ('test-multiplechoice.R:133:3'): Table: subgroup, weights, exclude NA ──
       Expected `multi_table(...)` to run without any errors.
       i Actually got a <vctrs_error_assert_size> with text:
         `..1 (right)` must have size 1, not size 500.
       
       [ FAIL 37 | WARN 0 | SKIP 0 | PASS 32 ]
       Error: Test failures
       Execution halted
     ```

# tabxplor (1.3.1)

* GitHub: https://github.com/BriceNocenti/tabxplor
* Github mirror: https://github.com/cran/tabxplor
* Maintainer: Brice Nocenti <brice.nocenti@gmail.com>

Run `revdepcheck::cloud_details(, "tabxplor")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     ℹ With name: age.
     Caused by error in `dplyr::case_when()`:
     ! `..1 (right)` must have size 1, not size 25.
     Backtrace:
          ▆
       1. ├─dplyr::mutate(...)
       2. ├─tabxplor::tab_num(...)
       3. │ └─purrr::pmap_dfc(...)
       4. │   └─purrr::pmap(.l, .f, ...)
       5. │     └─purrr:::pmap_("list", .l, .f, ..., .progress = .progress)
       6. │       ├─purrr:::with_indexed_errors(...)
       7. │       │ └─base::withCallingHandlers(...)
       8. │       ├─purrr:::call_with_cleanup(...)
       9. │       └─tabxplor (local) .f(...)
      10. │         ├─tabxplor:::new_fmt(...)
      11. │         │ └─vctrs::new_rcrd(...)
      12. │         │   └─vctrs::obj_is_list(fields)
      13. │         └─dplyr::case_when(...)
      14. │           └─dplyr:::vec_case_when(...)
      15. │             └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
      16. └─vctrs:::stop_assert_size(...)
      17.   └─vctrs:::stop_assert(...)
      18.     └─vctrs:::stop_vctrs(...)
      19.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
        15. │             │ └─base::withCallingHandlers(...)
        16. │             ├─purrr:::call_with_cleanup(...)
        17. │             └─tabxplor (local) .f(.x[[i]], ...)
        18. │               └─tabxplor:::get_reference(., mode = "all_totals")
        19. │                 └─dplyr::case_when(...)
        20. │                   └─dplyr:::vec_case_when(...)
        21. │                     └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
        22. ├─vctrs:::stop_assert_size(...)
        23. │ └─vctrs:::stop_assert(...)
        24. │   └─vctrs:::stop_vctrs(...)
        25. │     └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
        26. │       └─rlang:::signal_abort(cnd, .file)
        27. │         └─base::signalCondition(cnd)
        28. ├─purrr (local) `<fn>`(`<vctrs___>`)
        29. │ └─cli::cli_abort(...)
        30. │   └─rlang::abort(...)
        31. │     └─rlang:::signal_abort(cnd, .file)
        32. │       └─base::signalCondition(cnd)
        33. └─purrr (local) `<fn>`(`<prrr_rr_>`)
        34.   └─cli::cli_abort(...)
        35.     └─rlang::abort(...)
       
       [ FAIL 14 | WARN 0 | SKIP 0 | PASS 144 ]
       Error: Test failures
       Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     Error(s) in re-building vignettes:
       ...
     --- re-building ‘tabxplor.Rmd’ using rmarkdown
     
     Quitting from tabxplor.Rmd:79-85 [unnamed-chunk-6]
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     NULL
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'tabxplor.Rmd' failed with diagnostics:
     [1m[22m[36mℹ[39m In index: 1.
     [36mℹ[39m With name: Never married.
     [1mCaused by error in `dplyr::case_when()`:[22m
     [33m![39m `..1 (right)` must have size 1, not size 13.
     --- failed re-building ‘tabxplor.Rmd’
     
     SUMMARY: processing the following file failed:
       ‘tabxplor.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

# tcpl (3.3.0)

* GitHub: https://github.com/USEPA/CompTox-ToxCast-tcpl
* Github mirror: https://github.com/cran/tcpl
* Maintainer: Madison Feshuk <feshuk.madison@epa.gov>

Run `revdepcheck::cloud_details(, "tcpl")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
           table == "mcagg" ~ list(tbls = "mc3,mc4,mc4_agg", joins = "mc3.m3id = mc4_agg.m3id AND mc4.m4id = mc4_agg.m4id"), 
           table == "mc4" && add.fld == FALSE ~ list(tbls = "mc4", joins = NULL), 
           table == "mc4" && add.fld == TRUE ~ list(tbls = "mc4,mc4_param", 
               joins = "mc4.m4id = mc4_param.m4id"), table == "mc5" && 
               add.fld == FALSE ~ list(tbls = "mc4,mc5", joins = "mc4.m4id = mc5.m4id"), 
           table == "mc5" && add.fld == TRUE ~ list(tbls = "mc4,mc5,mc5_param", 
               joins = "mc4.m4id = mc5.m4id AND mc5.m5id = mc5_param.m5id"), 
           table == "mc6" ~ list(tbls = "mc4,mc6", joins = "mc6.m4id = mc4.m4id"), 
           table == "mc7" ~ list(tbls = "mc4,mc7", joins = "mc7.m4id = mc4.m4id"), 
           TRUE ~ list(tbls = NULL, joins = NULL))`: `..1 (right)` must have size 1, not size 2.
       Backtrace:
           ▆
        1. ├─tcpl::tcplPlotLoadData(type = "sc", fld = "aeid", val = mocked$aeid) at test-tcplggplot2Utils.R:406:3
        2. │ └─tcpl::tcplLoadData(lvl = lvl, fld = fld, val = val, type = type)
        3. │   └─dplyr::case_when(...)
        4. │     └─dplyr:::vec_case_when(...)
        5. │       └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
        6. └─vctrs:::stop_assert_size(...)
        7.   └─vctrs:::stop_assert(...)
        8.     └─vctrs:::stop_vctrs(...)
        9.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
       
       [ FAIL 167 | WARN 4 | SKIP 5 | PASS 355 ]
       Error: Test failures
       Execution halted
     ```

# tidyCDISC (0.2.1)

* GitHub: https://github.com/Biogen-Inc/tidyCDISC
* Github mirror: https://github.com/cran/tidyCDISC
* Maintainer: Aaron Clark <clark.aaronchris@gmail.com>

Run `revdepcheck::cloud_details(, "tidyCDISC")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
        39. │     └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
        40. │       └─rlang:::signal_abort(cnd, .file)
        41. │         └─base::signalCondition(cnd)
        42. ├─purrr (local) `<fn>`(`<vctrs___>`)
        43. │ └─cli::cli_abort(...)
        44. │   └─rlang::abort(...)
        45. │     └─rlang:::signal_abort(cnd, .file)
        46. │       └─base::signalCondition(cnd)
        47. ├─purrr (local) `<fn>`(`<prrr_rr_>`)
        48. │ └─cli::cli_abort(...)
        49. │   └─rlang::abort(...)
        50. │     └─rlang:::signal_abort(cnd, .file)
        51. │       └─base::signalCondition(cnd)
        52. ├─purrr (local) `<fn>`(`<prrr_rr_>`)
        53. │ └─cli::cli_abort(...)
        54. │   └─rlang::abort(...)
        55. │     └─rlang:::signal_abort(cnd, .file)
        56. │       └─base::signalCondition(cnd)
        57. └─purrr (local) `<fn>`(`<prrr_rr_>`)
        58.   └─cli::cli_abort(...)
        59.     └─rlang::abort(...)
       
       [ FAIL 3 | WARN 1 | SKIP 14 | PASS 92 ]
       Error: Test failures
       Execution halted
     ```

## In both

*   checking installed package size ... NOTE
     ```
       installed size is  6.5Mb
       sub-directories of 1Mb or more:
         R      1.5Mb
         data   2.0Mb
         doc    1.8Mb
     ```

# tidydice (1.0.0)

* GitHub: https://github.com/rolkra/tidydice
* Github mirror: https://github.com/cran/tidydice
* Maintainer: Roland Krasser <roland.krasser@gmail.com>

Run `revdepcheck::cloud_details(, "tidydice")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
           ))`: ℹ In argument: `result = case_when(...)`.
       Caused by error in `case_when()`:
       ! `..1 (right)` must have size 1, not size 200.
       ── Error ('test_dice_formula.R:206:3'): arithmetic operations ──────────────────
       <dplyr:::mutate_error/rlang_error/error/condition>
       Error in `mutate(., result = case_when(dice_op_sign == "+" ~ result + dice_op_n, 
           dice_op_sign == "-" ~ result - dice_op_n, dice_op_sign == 
               "*" ~ result * dice_op_n, dice_op_sign == "/" ~ result/dice_op_n, 
           dice_op_sign %in% c("^", "**") ~ result^dice_op_n, T ~ as.numeric(result), 
           ))`: ℹ In argument: `result = case_when(...)`.
       Caused by error in `case_when()`:
       ! `..1 (right)` must have size 1, not size 2000.
       ── Error ('test_dice_formula.R:237:3'): piping ─────────────────────────────────
       <dplyr:::mutate_error/rlang_error/error/condition>
       Error in `mutate(., result = case_when(dice_op_sign == "+" ~ result + dice_op_n, 
           dice_op_sign == "-" ~ result - dice_op_n, dice_op_sign == 
               "*" ~ result * dice_op_n, dice_op_sign == "/" ~ result/dice_op_n, 
           dice_op_sign %in% c("^", "**") ~ result^dice_op_n, T ~ as.numeric(result), 
           ))`: ℹ In argument: `result = case_when(...)`.
       Caused by error in `case_when()`:
       ! `..1 (right)` must have size 1, not size 20.
       
       [ FAIL 5 | WARN 0 | SKIP 0 | PASS 127 ]
       Error: Test failures
       Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     Error(s) in re-building vignettes:
     --- re-building ‘tidydice.Rmd’ using rmarkdown
     ```

# tidyfinance (0.4.4)

* GitHub: https://github.com/tidy-finance/r-tidyfinance
* Github mirror: https://github.com/cran/tidyfinance
* Maintainer: Christoph Scheuch <christoph@tidy-intelligence.com>

Run `revdepcheck::cloud_details(, "tidyfinance")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     > 
     > estimate_fama_macbeth(data, "ret_excess ~ beta + bm + log_mktcap")
     Error in `mutate()`:
     ℹ In argument: `t_statistic = case_when(...)`.
     Caused by error in `case_when()`:
     ! `..1 (right)` must have size 1, not size 4.
     Backtrace:
          ▆
       1. ├─tidyfinance::estimate_fama_macbeth(data, "ret_excess ~ beta + bm + log_mktcap")
       2. │ ├─dplyr::select(...)
       3. │ ├─dplyr::mutate(...)
       4. │ └─dplyr:::mutate.data.frame(...)
       5. │   └─dplyr:::mutate_cols(.data, dplyr_quosures(...), by)
       6. │     ├─base::withCallingHandlers(...)
       7. │     └─dplyr:::mutate_col(dots[[i]], data, mask, new_columns)
       8. │       └─mask$eval_all_mutate(quo)
       9. │         └─dplyr (local) eval()
      10. ├─dplyr::case_when(...)
      11. │ └─dplyr:::vec_case_when(...)
      12. │   └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
      13. └─vctrs:::stop_assert_size(...)
      14.   └─vctrs:::stop_assert(...)
      15.     └─vctrs:::stop_vctrs(...)
      16.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
       <dplyr:::mutate_error/rlang_error/error/condition>
       Error in `mutate(mutate(tidyr::nest(cross_sections, data = c(all_of(data_options$date), 
           value)), model = purrr::map(data, ~lm("value ~ 1", data = .)), 
           risk_premium = purrr::map_dbl(model, ~.$coefficients), n = purrr::map_dbl(data, 
               nrow), standard_error = purrr::map_dbl(model, ~compute_standard_error(., 
               vcov, vcov_options))), t_statistic = case_when(vcov == 
           "iid" ~ risk_premium/standard_error * sqrt(n), vcov == "newey-west" ~ 
           risk_premium/standard_error))`: i In argument: `t_statistic = case_when(...)`.
       Caused by error in `case_when()`:
       ! `..1 (right)` must have size 1, not size 4.
       ── Error ('test-estimate_fama_macbeth.R:119:3'): estimate_fama_macbeth computes correct number of rows ──
       <dplyr:::mutate_error/rlang_error/error/condition>
       Error in `mutate(mutate(tidyr::nest(cross_sections, data = c(all_of(data_options$date), 
           value)), model = purrr::map(data, ~lm("value ~ 1", data = .)), 
           risk_premium = purrr::map_dbl(model, ~.$coefficients), n = purrr::map_dbl(data, 
               nrow), standard_error = purrr::map_dbl(model, ~compute_standard_error(., 
               vcov, vcov_options))), t_statistic = case_when(vcov == 
           "iid" ~ risk_premium/standard_error * sqrt(n), vcov == "newey-west" ~ 
           risk_premium/standard_error))`: i In argument: `t_statistic = case_when(...)`.
       Caused by error in `case_when()`:
       ! `..1 (right)` must have size 1, not size 4.
       
       [ FAIL 3 | WARN 0 | SKIP 8 | PASS 84 ]
       Error: Test failures
       Execution halted
     ```

# TreatmentPatterns (3.1.1)

* GitHub: https://github.com/darwin-eu/TreatmentPatterns
* Github mirror: https://github.com/cran/TreatmentPatterns
* Maintainer: Maarten van Kessel <m.l.vankessel@erasmusmc.nl>

Run `revdepcheck::cloud_details(, "TreatmentPatterns")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
         'test-pathwaysLogical.R:1754:3', 'test-pathwaysLogical.R:1801:3',
         'test-pathwaysLogical.R:1857:3', 'test-pathwaysLogical.R:1912:3',
         'test-pathwaysMultipleTargetsLogical.R:18:3',
         'test-pathwaysMultipleTargetsLogical.R:220:3',
         'test-pathwaysMultipleTargetsLogical.R:311:3',
         'test-plotEventDuration.R:2:3', 'test-plotEventDuration.R:12:3',
         'test-plotEventDuration.R:37:3', 'test-plotEventDuration.R:63:3'
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Error ('test-CRAN.R:55:3'): CRAN Tests ──────────────────────────────────────
       Error in `filter(., .data$event_count >= minCellCount, case_when(treatmentGroups == 
           "both" ~ .data$event_name == .data$event_name, treatmentGroups == 
           "group" ~ .data$event_name %in% c("mono-event", "combination-event"), 
           treatmentGroups == "individual" ~ !.data$event_name %in% 
               c("mono-event", "combination-event")), case_when(is.null(eventLines) ~ 
           .data$event_name == .data$event_name, .default = .data$line %in% 
           c(as.character(eventLines), "overall")), case_when(includeOverall ~ 
           .data$event_name == .data$event_name, .default = !.data$line == 
           "overall"))`: i In argument: `case_when(...)`.
       Caused by error in `case_when()`:
       ! `..1 (right)` must have size 1, not size 21.
       
       [ FAIL 1 | WARN 1 | SKIP 123 | PASS 20 ]
       Error: Test failures
       Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     ...
       adding: counts_sex.csv (deflated 36%)
       adding: counts_year.csv (deflated 83%)
       adding: metadata.csv (deflated 25%)
       adding: summary_event_duration.csv (deflated 75%)
       adding: treatment_pathways.csv (deflated 95%)
     
     Quitting from a030_Evaluating_Output.Rmd:132-134 [summaryStatsTherapyDuration]
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     NULL
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'a030_Evaluating_Output.Rmd' failed with diagnostics:
     ℹ In argument: `case_when(...)`.
     Caused by error in `case_when()`:
     ! `..1 (right)` must have size 1, not size 88.
     --- failed re-building ‘a030_Evaluating_Output.Rmd’
     
     --- re-building ‘a999_Strategus.Rmd’ using rmarkdown
     --- finished re-building ‘a999_Strategus.Rmd’
     
     SUMMARY: processing the following file failed:
       ‘a030_Evaluating_Output.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

## In both

*   checking installed package size ... NOTE
     ```
       installed size is  7.0Mb
       sub-directories of 1Mb or more:
         doc   5.0Mb
     ```

# trendtestR (1.0.1)

* GitHub: https://github.com/GrahnH/trendtestR
* Github mirror: https://github.com/cran/trendtestR
* Maintainer: Gelan Huang <huanggelan97@icloud.com>

Run `revdepcheck::cloud_details(, "trendtestR")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     Input Value Spalte Datentyp: discrete
     Error in `dplyr::mutate()`:
     ℹ In argument: `jahr_group = dplyr::case_when(...)`.
     Caused by error in `dplyr::case_when()`:
     ! `..1 (right)` must have size 1, not size 94.
     Backtrace:
          ▆
       1. ├─trendtestR::compare_monthly_cases(...)
       2. │ └─... %>% filter(monat_num %in% months, jahr_group %in% years)
       3. ├─dplyr::filter(., monat_num %in% months, jahr_group %in% years)
       4. ├─dplyr::mutate(...)
       5. ├─dplyr:::mutate.data.frame(...)
       6. │ └─dplyr:::mutate_cols(.data, dplyr_quosures(...), by)
       7. │   ├─base::withCallingHandlers(...)
       8. │   └─dplyr:::mutate_col(dots[[i]], data, mask, new_columns)
       9. │     └─mask$eval_all_mutate(quo)
      10. │       └─dplyr (local) eval()
      11. ├─dplyr::case_when(...)
      12. │ └─dplyr:::vec_case_when(...)
      13. │   └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
      14. └─vctrs:::stop_assert_size(...)
      15.   └─vctrs:::stop_assert(...)
      16.     └─vctrs:::stop_vctrs(...)
      17.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
           monat = lubridate::month(.data[[datum_col]], label = TRUE, 
               abbr = TRUE), jahr_group = dplyr::case_when(shift_month == 
               "mth_to_next" ~ lubridate::year(.data[[datum_col]]) + 
               (monat_num %in% head(months, 1):12), shift_month == "mth_to_prev" ~ 
               lubridate::year(.data[[datum_col]]) - (monat_num %in% 
                   1:tail(months, 1)), TRUE ~ lubridate::isoyear(.data[[datum_col]])), 
           jahr = as.factor(jahr_group))`: i In argument: `jahr_group = dplyr::case_when(...)`.
       Caused by error in `dplyr::case_when()`:
       ! `..1 (right)` must have size 1, not size 9.
       ── Error ('test-compare_monthly_cases.R:83:3'): handles 3-level group_col, cross-year shift (prev), and negative values ──
       <dplyr:::mutate_error/rlang_error/error/condition>
       Error in `dplyr::mutate(., monat_num = lubridate::month(.data[[datum_col]]), 
           monat = lubridate::month(.data[[datum_col]], label = TRUE, 
               abbr = TRUE), jahr_group = dplyr::case_when(shift_month == 
               "mth_to_next" ~ lubridate::year(.data[[datum_col]]) + 
               (monat_num %in% head(months, 1):12), shift_month == "mth_to_prev" ~ 
               lubridate::year(.data[[datum_col]]) - (monat_num %in% 
                   1:tail(months, 1)), TRUE ~ lubridate::isoyear(.data[[datum_col]])), 
           jahr = as.factor(jahr_group))`: i In argument: `jahr_group = dplyr::case_when(...)`.
       Caused by error in `dplyr::case_when()`:
       ! `..1 (right)` must have size 1, not size 12.
       
       [ FAIL 9 | WARN 24 | SKIP 1 | PASS 471 ]
       Error: Test failures
       Execution halted
     ```

# vcdExtra (0.8-6)

* GitHub: https://github.com/friendly/vcdExtra
* Github mirror: https://github.com/cran/vcdExtra
* Maintainer: Michael Friendly <friendly@yorku.ca>

Run `revdepcheck::cloud_details(, "vcdExtra")` for more info

## Newly broken

*   checking re-building of vignette outputs ... ERROR
     ```
     Error(s) in re-building vignettes:
     --- re-building ‘a1-creating.Rmd’ using rmarkdown
     ```

## In both

*   checking Rd cross-references ... NOTE
     ```
     Package unavailable to check Rd xrefs: ‘factoextra’
     ```

# vigicaen (0.16.1)

* GitHub: https://github.com/pharmacologie-caen/vigicaen
* Github mirror: https://github.com/cran/vigicaen
* Maintainer: Charles Dolladille <cdolladille@hotmail.com>

Run `revdepcheck::cloud_details(, "vigicaen")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
        10. │     └─vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
        11. └─vctrs:::stop_assert_size(...)
        12.   └─vctrs:::stop_assert(...)
        13.     └─vctrs:::stop_vctrs(...)
        14.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
       
       [ FAIL 9 | WARN 0 | SKIP 104 | PASS 312 ]
       Deleting unused snapshots:
       • vigi_routine/arrow-table.svg
       • vigi_routine/base-graphic.svg
       • vigi_routine/case-time-to-onset.svg
       • vigi_routine/case-tto-above-90-days.svg
       • vigi_routine/case-tto-below-90-days.svg
       • vigi_routine/custom-drug-and-adr-labels.svg
       • vigi_routine/dual-drug-analysis.svg
       • vigi_routine/exporting.svg
       • vigi_routine/ic025-below-0.svg
       • vigi_routine/no-cases.svg
       • vigi_routine/no-rechallenge.svg
       • vigi_routine/no-time-to-onset-export.svg
       • vigi_routine/no-time-to-onset-with-2-drugs.svg
       • vigi_routine/no-time-to-onset.svg
       • vigi_routine/suspect-only-true.svg
       Error: Test failures
       Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     Error(s) in re-building vignettes:
     --- re-building ‘basic_workflow.Rmd’ using rmarkdown
     --- finished re-building ‘basic_workflow.Rmd’
     
     --- re-building ‘descriptive.Rmd’ using rmarkdown
     --- finished re-building ‘descriptive.Rmd’
     
     --- re-building ‘getting_started.Rmd’ using rmarkdown
     --- finished re-building ‘getting_started.Rmd’
     
     --- re-building ‘interactions.Rmd’ using rmarkdown
     --- finished re-building ‘interactions.Rmd’
     
     --- re-building ‘routine_pharmacovigilance.Rmd’ using rmarkdown
     ```

## In both

*   checking dependencies in R code ... NOTE
     ```
     Namespace in Imports field not imported from: ‘fst’
       All declared Imports should be used.
     ```

# vital (2.0.0)

* GitHub: https://github.com/robjhyndman/vital
* Github mirror: https://github.com/cran/vital
* Maintainer: Rob Hyndman <Rob.Hyndman@monash.edu>

Run `revdepcheck::cloud_details(, "vital")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     Running examples in ‘vital-Ex.R’ failed
     The error most likely occurred in:
     
     > ### Name: FDM
     > ### Title: Functional data model
     > ### Aliases: FDM report.FDM
     > 
     > ### ** Examples
     > 
     > hu <- norway_mortality |>
     +   dplyr::filter(Sex == "Female", Year > 2010) |>
     +   smooth_mortality(Mortality) |>
     +   model(hyndman_ullah = FDM(log(.smooth)))
     Warning: 1 error encountered for hyndman_ullah
     [1] In argument: `.innov = if_else(.innov < -1e+20, NA, .innov)`.
     
     > report(hu)
     Series: .smooth 
     Model: NULL model 
     Transformation: log(.smooth) 
     NULL model> autoplot(hu)
     Error: C stack usage  9964732 is too close to the limit
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
       The following object is masked from 'package:testthat':
       
           matches
       
       The following objects are masked from 'package:stats':
       
           filter, lag
       
       The following objects are masked from 'package:base':
       
           intersect, setdiff, setequal, union
       
       > 
       > test_check("vital")
       
       CNTRY not found
       [ FAIL 1 | WARN 7 | SKIP 0 | PASS 144 ]
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Failure ('test-coherent-forecast.R:31:3'): Coherent forecasts ───────────────
       mean(fc1$diff) is not strictly less than 0.0021. Difference: NA
       
       [ FAIL 1 | WARN 7 | SKIP 0 | PASS 144 ]
       Error: Test failures
       Execution halted
     ```

# xlr (1.0.3)

* GitHub: https://github.com/NHilder/xlr
* Github mirror: https://github.com/cran/xlr
* Maintainer: Nicholas Hilderson <nhilderson.code@gmail.com>

Run `revdepcheck::cloud_details(, "xlr")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
       ── Failure ('test-build_multiple_response_table.R:551:3'): build_mtable works with weights, one multiple response col,
                 and cut column ──
       `func_output` (`actual`) not equal to `expected_output` (`expected`).
       
         `attr(actual, 'row.names')`:                
       `attr(expected, 'row.names')`: 1 2 3 4 5 6 7 8
       
       `actual$col_1`:                                  
       `expected$col_1`: "a" "a" "a" "b" "b" "c" "c" "c"
       
       `actual$enjoy_fruit` is an S3 object of class <xlr_vector/vctrs_vctr>, a logical vector
       `expected$enjoy_fruit` is an S3 object of class <xlr_vector/vctrs_vctr>, a character vector
       
       `actual$N`:                                                  
       `expected$N`: "0.4" "0.2" "0.4" "0.2" "0.2" "0.3" "0.3" "0.3"
       
       `actual$N_group`:                                                  
       `expected$N_group`: "0.4" "0.4" "0.4" "0.2" "0.2" "0.6" "0.6" "0.6"
       
       `actual$Percent`:                                                      
       `expected$Percent`: "100%" "50%" "100%" "100%" "100%" "50%" "50%" "50%"
       
       [ FAIL 10 | WARN 0 | SKIP 81 | PASS 506 ]
       Error: Test failures
       Execution halted
     ```

