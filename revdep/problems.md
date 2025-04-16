# BiVariAn

<details>

* Version: 1.0.1
* GitHub: https://github.com/AndresFloresG/BiVariAn
* Source code: https://github.com/cran/BiVariAn
* Date/Publication: 2025-03-05 13:10:02 UTC
* Number of recursive dependencies: 201

Run `revdepcheck::cloud_details(, "BiVariAn")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘BiVariAn-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: auto_pie_categ
    > ### Title: Automatic generation of pieplots
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
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This file is part of the standard setup for testthat.
      > # It is recommended that you do not modify it.
      > #
      > # Where should you do additional test configuration?
      > # Learn more about the roles of various files in:
      > # * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
      > # * https://testthat.r-lib.org/articles/special-files.html
      > 
      > library(testthat)
      > library(BiVariAn)
      Warning message:
      In check_dep_version() : ABI version mismatch: 
      lme4 was built with Matrix ABI version 1
      Current Matrix ABI version is 0
      Please re-install lme4 from source or restore original 'Matrix' package
      > 
      > test_check("BiVariAn")
      [ FAIL 1 | WARN 0 | SKIP 7 | PASS 125 ]
      
      ══ Skipped tests (7) ═══════════════════════════════════════════════════════════
      • On CRAN (7): 'test-auto_shapiro_raw.R:64:3', 'test-continuous_2g.R:30:3',
        'test-continuous_2g.R:87:3', 'test-continuous_2g_pair.R:55:3',
        'test-dichotomous_2k_2sid.R:19:3', 'test-ss_multreg.R:2:3',
        'test-step_bw_firth.R:5:3'
      
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
      
      [ FAIL 1 | WARN 0 | SKIP 7 | PASS 125 ]
      Error: Test failures
      Execution halted
    ```

# CodelistGenerator

<details>

* Version: 3.5.0
* GitHub: NA
* Source code: https://github.com/cran/CodelistGenerator
* Date/Publication: 2025-04-10 19:20:39 UTC
* Number of recursive dependencies: 125

Run `revdepcheck::cloud_details(, "CodelistGenerator")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘a01_GettingOmopCdmVocabularies.Rmd’ using rmarkdown
    --- finished re-building ‘a01_GettingOmopCdmVocabularies.Rmd’
    
    --- re-building ‘a02_ExploreCDMvocabulary.Rmd’ using rmarkdown
    --- finished re-building ‘a02_ExploreCDMvocabulary.Rmd’
    
    --- re-building ‘a03_GenerateCandidateCodelist.Rmd’ using rmarkdown
    --- finished re-building ‘a03_GenerateCandidateCodelist.Rmd’
    
    --- re-building ‘a04_GenerateVocabularyBasedCodelist.Rmd’ using rmarkdown
    trying URL 'https://example-data.ohdsi.dev/synpuf-1k_5.3.zip'
    Content type 'application/zip' length 593048982 bytes (565.6 MB)
    =========================
    downloaded 286.1 MB
    
    
    Quitting from a04_GenerateVocabularyBasedCodelist.Rmd:10-20 [unnamed-chunk-1]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    <error/rlang_error>
    Error in `utils::download.file()`:
    ! download from 'https://example-data.ohdsi.dev/synpuf-1k_5.3.zip' failed
    ---
    Backtrace:
        ▆
     1. └─CDMConnector::requireEunomia("synpuf-1k", "5.3")
     2.   └─CDMConnector::downloadEunomiaData(...)
     3.     ├─withr::with_options(...)
     4.     │ └─base::force(code)
     5.     └─utils::download.file(...)
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    Error: processing vignette 'a04_GenerateVocabularyBasedCodelist.Rmd' failed with diagnostics:
    download from 'https://example-data.ohdsi.dev/synpuf-1k_5.3.zip' failed
    --- failed re-building ‘a04_GenerateVocabularyBasedCodelist.Rmd’
    
    --- re-building ‘a04b_icd_codes.Rmd’ using rmarkdown
    trying URL 'https://example-data.ohdsi.dev/synpuf-1k_5.3.zip'
    Content type 'application/zip' length 593048982 bytes (565.6 MB)
    ==================================================
    downloaded 565.6 MB
    
    --- finished re-building ‘a04b_icd_codes.Rmd’
    
    --- re-building ‘a05_ExtractCodelistFromJSONfile.Rmd’ using rmarkdown
    --- finished re-building ‘a05_ExtractCodelistFromJSONfile.Rmd’
    
    --- re-building ‘a06_CreateSubsetsFromCodelist.Rmd’ using rmarkdown
    --- finished re-building ‘a06_CreateSubsetsFromCodelist.Rmd’
    
    --- re-building ‘a07_RunCodelistDiagnostics.Rmd’ using rmarkdown
    --- finished re-building ‘a07_RunCodelistDiagnostics.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘a04_GenerateVocabularyBasedCodelist.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# inspectdf

<details>

* Version: 0.0.12.1
* GitHub: https://github.com/alastairrushworth/inspectdf
* Source code: https://github.com/cran/inspectdf
* Date/Publication: 2024-12-27 10:25:42 UTC
* Number of recursive dependencies: 68

Run `revdepcheck::cloud_details(, "inspectdf")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘inspectdf-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: show_plot
    > ### Title: Simple graphical inspection of dataframe summaries
    > ### Aliases: show_plot
    > 
    > ### ** Examples
    > 
    > # Load 'starwars' data
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

# REDCapTidieR

<details>

* Version: 1.2.2
* GitHub: https://github.com/CHOP-CGTInformatics/REDCapTidieR
* Source code: https://github.com/cran/REDCapTidieR
* Date/Publication: 2025-03-21 09:20:02 UTC
* Number of recursive dependencies: 92

Run `revdepcheck::cloud_details(, "REDCapTidieR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘REDCapTidieR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: combine_checkboxes
    > ### Title: Combine Checkbox Fields into a Single Column
    > ### Aliases: combine_checkboxes
    > 
    > ### ** Examples
    > 
    > library(dplyr)
    
    Attaching package: ‘dplyr’
    
    The following objects are masked from ‘package:stats’:
    
        filter, lag
    
    The following objects are masked from ‘package:base’:
    
        intersect, setdiff, setequal, union
    
    > # Set up sample data tibble
    > data_tbl <- tibble::tribble(
    +   ~"study_id", ~"multi___1", ~"multi___2", ~"multi___3",
    +   1, TRUE, FALSE, FALSE,
    +   2, TRUE, TRUE, FALSE,
    +   3, FALSE, FALSE, FALSE
    + )
    > 
    > # Set up sample metadata tibble
    > metadata_tbl <- tibble::tribble(
    +   ~"field_name", ~"field_type", ~"select_choices_or_calculations",
    +   "study_id", "text", NA,
    +   "multi___1", "checkbox", "1, Red | 2, Yellow | 3, Blue",
    +   "multi___2", "checkbox", "1, Red | 2, Yellow | 3, Blue",
    +   "multi___3", "checkbox", "1, Red | 2, Yellow | 3, Blue"
    + )
    > 
    > # Create sample supertibble
    > supertbl <- tibble::tribble(
    +   ~"redcap_form_name", ~"redcap_data", ~"redcap_metadata",
    +   "tbl", data_tbl, metadata_tbl
    + )
    > 
    > class(supertbl) <- c("redcap_supertbl", class(supertbl))
    > 
    > # Combine checkboxes under column "multi"
    > combine_checkboxes(
    +   supertbl = supertbl,
    +   tbl = "tbl",
    +   cols = starts_with("multi")
    + ) %>%
    +   dplyr::pull(redcap_data) %>%
    +   dplyr::first()
    Error in `pmap()`:
    ℹ In index: 1.
    Caused by error in `mutate()`:
    ℹ In argument: `multi = case_when(...)`.
    Caused by error in `case_when()`:
    ! `..1 (left)` can't be an array.
    Backtrace:
         ▆
      1. ├─... %>% dplyr::first()
      2. ├─dplyr::first(.)
      3. │ └─dplyr::nth(x, 1L, order_by = order_by, default = default, na_rm = na_rm)
      4. │   └─vctrs::vec_size(x)
      5. ├─dplyr::pull(., redcap_data)
      6. ├─REDCapTidieR::combine_checkboxes(...)
      7. │ └─... %>% ...
      8. ├─purrr::pmap(...)
      9. │ └─purrr:::pmap_("list", .l, .f, ..., .progress = .progress)
     10. │   ├─purrr:::with_indexed_errors(...)
     11. │   │ └─base::withCallingHandlers(...)
     12. │   ├─purrr:::call_with_cleanup(...)
     13. │   └─REDCapTidieR (local) .f(.new_value = .l[[1L]][[i]], metadata = .l[[2L]][[i]], ...)
     14. │     └─... %>% ...
     15. ├─dplyr::mutate(...)
     16. ├─dplyr:::mutate.data.frame(...)
     17. │ └─dplyr:::mutate_cols(.data, dplyr_quosures(...), by)
     18. │   ├─base::withCallingHandlers(...)
     19. │   └─dplyr:::mutate_col(dots[[i]], data, mask, new_columns)
     20. │     └─mask$eval_all_mutate(quo)
     21. │       └─dplyr (local) eval()
     22. └─dplyr::case_when(...)
     23.   └─dplyr:::vec_case_when(...)
     24.     └─dplyr:::check_no_dim(condition, arg = condition_arg, call = call)
     25.       └─cli::cli_abort("{.arg {arg}} can't be an array.", call = call)
     26.         └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This file is part of the standard setup for testthat.
      > # It is recommended that you do not modify it.
      > #
      > # Where should you do additional test configuration?
      > # Learn more about the roles of various files in:
      > # * https://r-pkgs.org/tests.html
      > # * https://testthat.r-lib.org/reference/test_package.html#special-files
      > 
      > library(testthat)
      > library(REDCapTidieR)
      > 
      > test_check("REDCapTidieR")
      [ FAIL 8 | WARN 1 | SKIP 5 | PASS 312 ]
      
      ══ Skipped tests (5) ═══════════════════════════════════════════════════════════
      • On CRAN (5): 'test-lint-free.R:2:3', 'test-read_redcap.R:1:1',
        'test-supertibble.R:2:3', 'test-utils.R:214:3', 'test-write.R:341:3'
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error ('test-combine_checkboxes.R:42:3'): combine_checkboxes returns an expected supertbl ──
      <purrr_error_indexed/rlang_error/error/condition>
      Error in `pmap(., convert_checkbox_vals, data_tbl = data_tbl_mod, raw_or_label = raw_or_label, 
          multi_value_label = multi_value_label, values_fill = values_fill)`: i In index: 1.
      Caused by error in `mutate()`:
      i In argument: `multi = case_when(...)`.
      Caused by error in `case_when()`:
      ! `..1 (left)` can't be an array.
      Backtrace:
           ▆
        1. ├─REDCapTidieR::combine_checkboxes(...) at test-combine_checkboxes.R:42:3
        2. │ └─... %>% ...
        3. ├─purrr::pmap(...)
        4. │ └─purrr:::pmap_("list", .l, .f, ..., .progress = .progress)
        5. │   ├─purrr:::with_indexed_errors(...)
        6. │   │ └─base::withCallingHandlers(...)
        7. │   ├─purrr:::call_with_cleanup(...)
        8. │   └─REDCapTidieR (local) .f(.new_value = .l[[1L]][[i]], metadata = .l[[2L]][[i]], ...)
        9. │     └─... %>% ...
       10. ├─dplyr::mutate(...)
       11. ├─dplyr:::mutate.data.frame(...)
       12. │ └─dplyr:::mutate_cols(.data, dplyr_quosures(...), by)
       13. │   ├─base::withCallingHandlers(...)
       14. │   └─dplyr:::mutate_col(dots[[i]], data, mask, new_columns)
       15. │     └─mask$eval_all_mutate(quo)
       16. │       └─dplyr (local) eval()
       17. ├─dplyr::case_when(...)
       18. │ └─dplyr:::vec_case_when(...)
       19. │   └─dplyr:::check_no_dim(condition, arg = condition_arg, call = call)
       20. │     └─cli::cli_abort("{.arg {arg}} can't be an array.", call = call)
       21. │       └─rlang::abort(...)
       22. │         └─rlang:::signal_abort(cnd, .file)
       23. │           └─base::signalCondition(cnd)
       24. ├─dplyr (local) `<fn>`(`<rlng_rrr>`)
       25. │ └─rlang::abort(message, class = error_class, parent = parent, call = error_call)
       26. │   └─rlang:::signal_abort(cnd, .file)
       27. │     └─base::signalCondition(cnd)
       28. └─purrr (local) `<fn>`(`<dply:::_>`)
       29.   └─cli::cli_abort(...)
       30.     └─rlang::abort(...)
      ── Error ('test-combine_checkboxes.R:53:3'): combine_checkboxes works for nonrepeat instrument ──
      <purrr_error_indexed/rlang_error/error/condition>
      Error in `pmap(., convert_checkbox_vals, data_tbl = data_tbl_mod, raw_or_label = raw_or_label, 
          multi_value_label = multi_value_label, values_fill = values_fill)`: i In index: 1.
      Caused by error in `mutate()`:
      i In argument: `multi = case_when(...)`.
      Caused by error in `case_when()`:
      ! `..1 (left)` can't be an array.
      Backtrace:
           ▆
        1. ├─... %>% dplyr::first() at test-combine_checkboxes.R:53:3
        2. ├─dplyr::first(.)
        3. │ └─dplyr::nth(x, 1L, order_by = order_by, default = default, na_rm = na_rm)
        4. │   └─vctrs::vec_size(x)
        5. ├─dplyr::pull(., redcap_data)
        6. ├─REDCapTidieR::combine_checkboxes(...)
        7. │ └─... %>% ...
        8. ├─purrr::pmap(...)
        9. │ └─purrr:::pmap_("list", .l, .f, ..., .progress = .progress)
       10. │   ├─purrr:::with_indexed_errors(...)
       11. │   │ └─base::withCallingHandlers(...)
       12. │   ├─purrr:::call_with_cleanup(...)
       13. │   └─REDCapTidieR (local) .f(.new_value = .l[[1L]][[i]], metadata = .l[[2L]][[i]], ...)
       14. │     └─... %>% ...
       15. ├─dplyr::mutate(...)
       16. ├─dplyr:::mutate.data.frame(...)
       17. │ └─dplyr:::mutate_cols(.data, dplyr_quosures(...), by)
       18. │   ├─base::withCallingHandlers(...)
       19. │   └─dplyr:::mutate_col(dots[[i]], data, mask, new_columns)
       20. │     └─mask$eval_all_mutate(quo)
       21. │       └─dplyr (local) eval()
       22. ├─dplyr::case_when(...)
       23. │ └─dplyr:::vec_case_when(...)
       24. │   └─dplyr:::check_no_dim(condition, arg = condition_arg, call = call)
       25. │     └─cli::cli_abort("{.arg {arg}} can't be an array.", call = call)
       26. │       └─rlang::abort(...)
       27. │         └─rlang:::signal_abort(cnd, .file)
       28. │           └─base::signalCondition(cnd)
       29. ├─dplyr (local) `<fn>`(`<rlng_rrr>`)
       30. │ └─rlang::abort(message, class = error_class, parent = parent, call = error_call)
       31. │   └─rlang:::signal_abort(cnd, .file)
       32. │     └─base::signalCondition(cnd)
       33. └─purrr (local) `<fn>`(`<dply:::_>`)
       34.   └─cli::cli_abort(...)
       35.     └─rlang::abort(...)
      ── Error ('test-combine_checkboxes.R:77:3'): combine_checkboxes glue spec works ──
      <purrr_error_indexed/rlang_error/error/condition>
      Error in `pmap(., convert_checkbox_vals, data_tbl = data_tbl_mod, raw_or_label = raw_or_label, 
          multi_value_label = multi_value_label, values_fill = values_fill)`: i In index: 1.
      Caused by error in `mutate()`:
      i In argument: `multi_suffix = case_when(...)`.
      Caused by error in `case_when()`:
      ! `..1 (left)` can't be an array.
      Backtrace:
           ▆
        1. ├─... %>% dplyr::first() at test-combine_checkboxes.R:77:3
        2. ├─dplyr::first(.)
        3. │ └─dplyr::nth(x, 1L, order_by = order_by, default = default, na_rm = na_rm)
        4. │   └─vctrs::vec_size(x)
        5. ├─dplyr::pull(., redcap_data)
        6. ├─REDCapTidieR::combine_checkboxes(...)
        7. │ └─... %>% ...
        8. ├─purrr::pmap(...)
        9. │ └─purrr:::pmap_("list", .l, .f, ..., .progress = .progress)
       10. │   ├─purrr:::with_indexed_errors(...)
       11. │   │ └─base::withCallingHandlers(...)
       12. │   ├─purrr:::call_with_cleanup(...)
       13. │   └─REDCapTidieR (local) .f(.new_value = .l[[1L]][[i]], metadata = .l[[2L]][[i]], ...)
       14. │     └─... %>% ...
       15. ├─dplyr::mutate(...)
       16. ├─dplyr:::mutate.data.frame(...)
       17. │ └─dplyr:::mutate_cols(.data, dplyr_quosures(...), by)
       18. │   ├─base::withCallingHandlers(...)
       19. │   └─dplyr:::mutate_col(dots[[i]], data, mask, new_columns)
       20. │     └─mask$eval_all_mutate(quo)
       21. │       └─dplyr (local) eval()
       22. ├─dplyr::case_when(...)
       23. │ └─dplyr:::vec_case_when(...)
       24. │   └─dplyr:::check_no_dim(condition, arg = condition_arg, call = call)
       25. │     └─cli::cli_abort("{.arg {arg}} can't be an array.", call = call)
       26. │       └─rlang::abort(...)
       27. │         └─rlang:::signal_abort(cnd, .file)
       28. │           └─base::signalCondition(cnd)
       29. ├─dplyr (local) `<fn>`(`<rlng_rrr>`)
       30. │ └─rlang::abort(message, class = error_class, parent = parent, call = error_call)
       31. │   └─rlang:::signal_abort(cnd, .file)
       32. │     └─base::signalCondition(cnd)
       33. └─purrr (local) `<fn>`(`<dply:::_>`)
       34.   └─cli::cli_abort(...)
       35.     └─rlang::abort(...)
      ── Error ('test-combine_checkboxes.R:128:3'): combine_checkboxes works for nonrepeat instrument and drop old values ──
      <purrr_error_indexed/rlang_error/error/condition>
      Error in `pmap(., convert_checkbox_vals, data_tbl = data_tbl_mod, raw_or_label = raw_or_label, 
          multi_value_label = multi_value_label, values_fill = values_fill)`: i In index: 1.
      Caused by error in `mutate()`:
      i In argument: `multi = case_when(...)`.
      Caused by error in `case_when()`:
      ! `..1 (left)` can't be an array.
      Backtrace:
           ▆
        1. ├─... %>% dplyr::first() at test-combine_checkboxes.R:128:3
        2. ├─dplyr::first(.)
        3. │ └─dplyr::nth(x, 1L, order_by = order_by, default = default, na_rm = na_rm)
        4. │   └─vctrs::vec_size(x)
        5. ├─dplyr::pull(., redcap_data)
        6. ├─REDCapTidieR::combine_checkboxes(...)
        7. │ └─... %>% ...
        8. ├─purrr::pmap(...)
        9. │ └─purrr:::pmap_("list", .l, .f, ..., .progress = .progress)
       10. │   ├─purrr:::with_indexed_errors(...)
       11. │   │ └─base::withCallingHandlers(...)
       12. │   ├─purrr:::call_with_cleanup(...)
       13. │   └─REDCapTidieR (local) .f(.new_value = .l[[1L]][[i]], metadata = .l[[2L]][[i]], ...)
       14. │     └─... %>% ...
       15. ├─dplyr::mutate(...)
       16. ├─dplyr:::mutate.data.frame(...)
       17. │ └─dplyr:::mutate_cols(.data, dplyr_quosures(...), by)
       18. │   ├─base::withCallingHandlers(...)
       19. │   └─dplyr:::mutate_col(dots[[i]], data, mask, new_columns)
       20. │     └─mask$eval_all_mutate(quo)
       21. │       └─dplyr (local) eval()
       22. ├─dplyr::case_when(...)
       23. │ └─dplyr:::vec_case_when(...)
       24. │   └─dplyr:::check_no_dim(condition, arg = condition_arg, call = call)
       25. │     └─cli::cli_abort("{.arg {arg}} can't be an array.", call = call)
       26. │       └─rlang::abort(...)
       27. │         └─rlang:::signal_abort(cnd, .file)
       28. │           └─base::signalCondition(cnd)
       29. ├─dplyr (local) `<fn>`(`<rlng_rrr>`)
       30. │ └─rlang::abort(message, class = error_class, parent = parent, call = error_call)
       31. │   └─rlang:::signal_abort(cnd, .file)
       32. │     └─base::signalCondition(cnd)
       33. └─purrr (local) `<fn>`(`<dply:::_>`)
       34.   └─cli::cli_abort(...)
       35.     └─rlang::abort(...)
      ── Error ('test-combine_checkboxes.R:151:3'): combine_checkboxes works for repeat instrument ──
      <purrr_error_indexed/rlang_error/error/condition>
      Error in `pmap(., convert_checkbox_vals, data_tbl = data_tbl_mod, raw_or_label = raw_or_label, 
          multi_value_label = multi_value_label, values_fill = values_fill)`: i In index: 1.
      Caused by error in `mutate()`:
      i In argument: `repeat = case_when(...)`.
      Caused by error in `case_when()`:
      ! `..1 (left)` can't be an array.
      Backtrace:
           ▆
        1. ├─... %>% dplyr::nth(2) at test-combine_checkboxes.R:151:3
        2. ├─dplyr::nth(., 2)
        3. │ └─vctrs::vec_size(x)
        4. ├─dplyr::pull(., redcap_data)
        5. ├─REDCapTidieR::combine_checkboxes(...)
        6. │ └─... %>% ...
        7. ├─purrr::pmap(...)
        8. │ └─purrr:::pmap_("list", .l, .f, ..., .progress = .progress)
        9. │   ├─purrr:::with_indexed_errors(...)
       10. │   │ └─base::withCallingHandlers(...)
       11. │   ├─purrr:::call_with_cleanup(...)
       12. │   └─REDCapTidieR (local) .f(.new_value = .l[[1L]][[i]], metadata = .l[[2L]][[i]], ...)
       13. │     └─... %>% ...
       14. ├─dplyr::mutate(...)
       15. ├─dplyr:::mutate.data.frame(...)
       16. │ └─dplyr:::mutate_cols(.data, dplyr_quosures(...), by)
       17. │   ├─base::withCallingHandlers(...)
       18. │   └─dplyr:::mutate_col(dots[[i]], data, mask, new_columns)
       19. │     └─mask$eval_all_mutate(quo)
       20. │       └─dplyr (local) eval()
       21. ├─dplyr::case_when(...)
       22. │ └─dplyr:::vec_case_when(...)
       23. │   └─dplyr:::check_no_dim(condition, arg = condition_arg, call = call)
       24. │     └─cli::cli_abort("{.arg {arg}} can't be an array.", call = call)
       25. │       └─rlang::abort(...)
       26. │         └─rlang:::signal_abort(cnd, .file)
       27. │           └─base::signalCondition(cnd)
       28. ├─dplyr (local) `<fn>`(`<rlng_rrr>`)
       29. │ └─rlang::abort(message, class = error_class, parent = parent, call = error_call)
       30. │   └─rlang:::signal_abort(cnd, .file)
       31. │     └─base::signalCondition(cnd)
       32. └─purrr (local) `<fn>`(`<dply:::_>`)
       33.   └─cli::cli_abort(...)
       34.     └─rlang::abort(...)
      ── Error ('test-combine_checkboxes.R:209:3'): combine_checkboxes works for multiple checkbox fields ──
      <purrr_error_indexed/rlang_error/error/condition>
      Error in `pmap(., convert_checkbox_vals, data_tbl = data_tbl_mod, raw_or_label = raw_or_label, 
          multi_value_label = multi_value_label, values_fill = values_fill)`: i In index: 1.
      Caused by error in `mutate()`:
      i In argument: `multi = case_when(...)`.
      Caused by error in `case_when()`:
      ! `..1 (left)` can't be an array.
      Backtrace:
           ▆
        1. ├─... %>% dplyr::first() at test-combine_checkboxes.R:209:3
        2. ├─dplyr::first(.)
        3. │ └─dplyr::nth(x, 1L, order_by = order_by, default = default, na_rm = na_rm)
        4. │   └─vctrs::vec_size(x)
        5. ├─dplyr::pull(., redcap_data)
        6. ├─REDCapTidieR::combine_checkboxes(...)
        7. │ └─... %>% ...
        8. ├─purrr::pmap(...)
        9. │ └─purrr:::pmap_("list", .l, .f, ..., .progress = .progress)
       10. │   ├─purrr:::with_indexed_errors(...)
       11. │   │ └─base::withCallingHandlers(...)
       12. │   ├─purrr:::call_with_cleanup(...)
       13. │   └─REDCapTidieR (local) .f(.new_value = .l[[1L]][[i]], metadata = .l[[2L]][[i]], ...)
       14. │     └─... %>% ...
       15. ├─dplyr::mutate(...)
       16. ├─dplyr:::mutate.data.frame(...)
       17. │ └─dplyr:::mutate_cols(.data, dplyr_quosures(...), by)
       18. │   ├─base::withCallingHandlers(...)
       19. │   └─dplyr:::mutate_col(dots[[i]], data, mask, new_columns)
       20. │     └─mask$eval_all_mutate(quo)
       21. │       └─dplyr (local) eval()
       22. ├─dplyr::case_when(...)
       23. │ └─dplyr:::vec_case_when(...)
       24. │   └─dplyr:::check_no_dim(condition, arg = condition_arg, call = call)
       25. │     └─cli::cli_abort("{.arg {arg}} can't be an array.", call = call)
       26. │       └─rlang::abort(...)
       27. │         └─rlang:::signal_abort(cnd, .file)
       28. │           └─base::signalCondition(cnd)
       29. ├─dplyr (local) `<fn>`(`<rlng_rrr>`)
       30. │ └─rlang::abort(message, class = error_class, parent = parent, call = error_call)
       31. │   └─rlang:::signal_abort(cnd, .file)
       32. │     └─base::signalCondition(cnd)
       33. └─purrr (local) `<fn>`(`<dply:::_>`)
       34.   └─cli::cli_abort(...)
       35.     └─rlang::abort(...)
      ── Error ('test-combine_checkboxes.R:233:3'): combine_checkboxes works for multiple checkbox fields with logicals ──
      <purrr_error_indexed/rlang_error/error/condition>
      Error in `pmap(., convert_checkbox_vals, data_tbl = data_tbl_mod, raw_or_label = raw_or_label, 
          multi_value_label = multi_value_label, values_fill = values_fill)`: i In index: 1.
      Caused by error in `mutate()`:
      i In argument: `multi = case_when(...)`.
      Caused by error in `case_when()`:
      ! `..1 (left)` can't be an array.
      Backtrace:
           ▆
        1. ├─... %>% dplyr::first() at test-combine_checkboxes.R:233:3
        2. ├─dplyr::first(.)
        3. │ └─dplyr::nth(x, 1L, order_by = order_by, default = default, na_rm = na_rm)
        4. │   └─vctrs::vec_size(x)
        5. ├─dplyr::pull(., redcap_data)
        6. ├─REDCapTidieR::combine_checkboxes(...)
        7. │ └─... %>% ...
        8. ├─purrr::pmap(...)
        9. │ └─purrr:::pmap_("list", .l, .f, ..., .progress = .progress)
       10. │   ├─purrr:::with_indexed_errors(...)
       11. │   │ └─base::withCallingHandlers(...)
       12. │   ├─purrr:::call_with_cleanup(...)
       13. │   └─REDCapTidieR (local) .f(.new_value = .l[[1L]][[i]], metadata = .l[[2L]][[i]], ...)
       14. │     └─... %>% ...
       15. ├─dplyr::mutate(...)
       16. ├─dplyr:::mutate.data.frame(...)
       17. │ └─dplyr:::mutate_cols(.data, dplyr_quosures(...), by)
       18. │   ├─base::withCallingHandlers(...)
       19. │   └─dplyr:::mutate_col(dots[[i]], data, mask, new_columns)
       20. │     └─mask$eval_all_mutate(quo)
       21. │       └─dplyr (local) eval()
       22. ├─dplyr::case_when(...)
       23. │ └─dplyr:::vec_case_when(...)
       24. │   └─dplyr:::check_no_dim(condition, arg = condition_arg, call = call)
       25. │     └─cli::cli_abort("{.arg {arg}} can't be an array.", call = call)
       26. │       └─rlang::abort(...)
       27. │         └─rlang:::signal_abort(cnd, .file)
       28. │           └─base::signalCondition(cnd)
       29. ├─dplyr (local) `<fn>`(`<rlng_rrr>`)
       30. │ └─rlang::abort(message, class = error_class, parent = parent, call = error_call)
       31. │   └─rlang:::signal_abort(cnd, .file)
       32. │     └─base::signalCondition(cnd)
       33. └─purrr (local) `<fn>`(`<dply:::_>`)
       34.   └─cli::cli_abort(...)
       35.     └─rlang::abort(...)
      ── Error ('test-combine_checkboxes.R:272:3'): convert_checkbox_vals works() ────
      <dplyr:::mutate_error/rlang_error/error/condition>
      Error in `mutate(., `:=`(!!.new_value, case_when(. > 1 ~ multi_value_label, 
          . == 1 ~ coalesce(!!!data_tbl[, names(data_tbl) %in% metadata$field_name]), 
          .default = values_fill)), `:=`(!!.new_value, factor(!!sym(.new_value), 
          levels = c(metadata[[raw_or_label]], multi_value_label, values_fill))))`: i In argument: `_multi = case_when(...)`.
      Caused by error in `case_when()`:
      ! `..1 (left)` can't be an array.
      
      [ FAIL 8 | WARN 1 | SKIP 5 | PASS 312 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘REDCapTidieR.Rmd’
      ...
    > token <- Sys.getenv("SUPERHEROES_REDCAP_API")
    
    > library(REDCapTidieR)
    
    > superheroes <- read_redcap(redcap_uri, token)
    
      When sourcing ‘REDCapTidieR.R’:
    Error: ✖ The token is an empty string, which is not allowed.
    ℹ API token: ``
    Execution halted
    
      ‘REDCapTidieR.Rmd’ using ‘UTF-8’... failed
      ‘glossary.Rmd’ using ‘UTF-8’... OK
    ```

# scSpatialSIM

<details>

* Version: 0.1.3.4
* GitHub: https://github.com/FridleyLab/scSpatialSIM
* Source code: https://github.com/cran/scSpatialSIM
* Date/Publication: 2024-10-01 15:20:03 UTC
* Number of recursive dependencies: 132

Run `revdepcheck::cloud_details(, "scSpatialSIM")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘a01_Introduction.Rmd’
      ...
    Computing density heatmap for Cell 1
    Computing probability for Cell 1
    Copying density heatmap for Cell 2
    Computing probability for Cell 2
    
    > PlotSimulation(bivariate_sim_tmp, which = 1, what = "whole core")
    
      When sourcing ‘a01_Introduction.R’:
    Error: Faceting variables must have at least one value.
    Execution halted
    
      ‘a01_Introduction.Rmd’ using ‘UTF-8’... failed
      ‘a02_Using_with_spatialTIME.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘a01_Introduction.Rmd’ using rmarkdown
    ```

# STICr

<details>

* Version: 1.1
* GitHub: https://github.com/HEAL-KGS/STICr
* Source code: https://github.com/cran/STICr
* Date/Publication: 2024-12-02 15:40:24 UTC
* Number of recursive dependencies: 44

Run `revdepcheck::cloud_details(, "STICr")` for more info

</details>

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

# xlr

<details>

* Version: 1.0.3
* GitHub: https://github.com/NHilder/xlr
* Source code: https://github.com/cran/xlr
* Date/Publication: 2025-03-07 11:40:02 UTC
* Number of recursive dependencies: 72

Run `revdepcheck::cloud_details(, "xlr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This file is part of the standard setup for testthat.
      > # It is recommended that you do not modify it.
      > #
      > # Where should you do additional test configuration?
      > # Learn more about the roles of various files in:
      > # * https://r-pkgs.org/tests.html
      > # * https://testthat.r-lib.org/reference/test_package.html#special-files
      > 
      > library(testthat)
      > library(xlr)
      > 
      > test_check("xlr")
      [ FAIL 10 | WARN 0 | SKIP 81 | PASS 506 ]
      
      ══ Skipped tests (81) ══════════════════════════════════════════════════════════
      • On CRAN (81): 'test-build_multiple_response_table.R:184:3',
        'test-build_multiple_response_table.R:189:3',
        'test-build_multiple_response_table.R:195:3',
        'test-build_multiple_response_table.R:201:3',
        'test-build_multiple_response_table.R:206:3',
        'test-build_multiple_response_table.R:211:3',
        'test-build_multiple_response_table.R:239:3',
        'test-build_multiple_response_table.R:273:3',
        'test-build_multiple_response_table.R:284:3',
        'test-build_multiple_response_table.R:294:3',
        'test-build_question_block_table.R:2:3',
        'test-build_question_block_table.R:7:3',
        'test-build_question_block_table.R:13:3',
        'test-build_question_block_table.R:20:3',
        'test-build_question_block_table.R:34:3',
        'test-build_question_block_table.R:42:3',
        'test-build_question_block_table.R:243:3', 'test-build_table.R:2:3',
        'test-build_table.R:10:3', 'test-build_table.R:87:3',
        'test-create_table_of_contents.R:24:3',
        'test-create_table_of_contents.R:35:3',
        'test-create_table_of_contents.R:46:3',
        'test-create_table_of_contents.R:57:3',
        'test-create_table_of_contents.R:69:3',
        'test-create_table_of_contents.R:81:3',
        'test-create_table_of_contents.R:155:3', 'test-error_utils.R:10:3',
        'test-table_utils.R:50:3', 'test-table_utils.R:63:3',
        'test-table_utils.R:103:3', 'test-table_utils.R:125:3',
        'test-table_utils.R:147:3', 'test-table_utils.R:169:3',
        'test-table_utils.R:192:3', 'test-table_utils.R:215:3',
        'test-write_xlsx.R:22:3', 'test-write_xlsx.R:44:3', 'test-write_xlsx.R:65:3',
        'test-write_xlsx.R:88:3', 'test-xlr_format.R:12:3',
        'test-xlr_format.R:219:3', 'test-xlr_integer.R:62:3',
        'test-xlr_numeric.R:76:3', 'test-xlr_numeric.R:84:3',
        'test-xlr_percent.R:97:3', 'test-xlr_table.R:53:3',
        'test-xlr_to_workbook.R:36:3', 'test-xlr_to_workbook.R:44:3',
        'test-xlr_to_workbook.R:70:3', 'test-xlr_to_workbook.R:103:3',
        'test-xlr_to_workbook.R:433:3', 'test-xlr_to_workbook.R:487:3',
        'test-xlr_to_workbook.R:522:3', 'test-xlr_to_workbook.R:536:3',
        'test-xlr_to_workbook.R:572:3', 'test-xlr_to_workbook.R:608:3',
        'test-xlr_to_workbook.R:676:3', 'test-xlr_to_workbook.R:689:3',
        'test-xlr_to_workbook.R:699:3', 'test-xlr_to_workbook.R:744:3',
        'test-xlr_to_workbook.R:755:3', 'test-xlr_to_workbook.R:765:3',
        'test-xlr_to_workbook.R:775:3', 'test-xlr_to_workbook.R:783:3',
        'test-xlr_to_workbook.R:789:3', 'test-xlr_to_workbook.R:794:3',
        'test-xlr_to_workbook.R:799:3', 'test-xlr_to_workbook.R:804:3',
        'test-xlr_to_workbook.R:812:3', 'test-xlr_to_workbook.R:819:3',
        'test-xlr_to_workbook.R:824:3', 'test-xlr_to_workbook.R:830:3',
        'test-xlr_to_workbook.R:836:3', 'test-xlr_to_workbook.R:860:3',
        'test-xlr_to_workbook.R:905:3', 'test-xlr_to_workbook.R:940:3',
        'test-xlr_to_workbook.R:975:3', 'test-xlr_vector.R:76:3',
        'test-xlr_vector.R:89:3', 'test-xlr_vector.R:95:3'
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-build_multiple_response_table.R:22:3'): build_multiple_response_table() works for the simplest case ──
      `func_output` (`actual`) not equal to `expected_output` (`expected`).
      
        `attr(actual, 'row.names')`:      
      `attr(expected, 'row.names')`: 1 2 3
      
      `actual$enjoy_fruit` is an S3 object of class <xlr_vector/vctrs_vctr>, a logical vector
      `expected$enjoy_fruit` is an S3 object of class <xlr_vector/vctrs_vctr>, a character vector
      
      `actual$N`:              
      `expected$N`: "3" "4" "5"
      
      `actual$N_group`:              
      `expected$N_group`: "6" "6" "6"
      
      `actual$Percent`:                    
      `expected$Percent`: "50%" "67%" "83%"
      ── Failure ('test-build_multiple_response_table.R:43:3'): build_multiple_response_table() works with question labels ──
      `func_output` (`actual`) not equal to `expected_output` (`expected`).
      
        `attr(actual, 'row.names')`:      
      `attr(expected, 'row.names')`: 1 2 3
      
      `actual$enjoy_fruit` is an S3 object of class <xlr_vector/vctrs_vctr>, a logical vector
      `expected$enjoy_fruit` is an S3 object of class <xlr_vector/vctrs_vctr>, a character vector
      
      `actual$N`:              
      `expected$N`: "3" "4" "5"
      
      `actual$N_group`:              
      `expected$N_group`: "6" "6" "6"
      
      `actual$Percent`:                    
      `expected$Percent`: "50%" "67%" "83%"
      ── Failure ('test-build_multiple_response_table.R:66:3'): build_multiple_response_table() works when we add one grouping variable ──
      `func_output` (`actual`) not equal to `expected_output` (`expected`).
      
        `attr(actual, 'row.names')`:                
      `attr(expected, 'row.names')`: 1 2 3 4 5 6 7 8
      
      `actual$col_1`:                                  
      `expected$col_1`: "a" "a" "a" "b" "b" "c" "c" "c"
      
      `actual$enjoy_fruit` is an S3 object of class <xlr_vector/vctrs_vctr>, a logical vector
      `expected$enjoy_fruit` is an S3 object of class <xlr_vector/vctrs_vctr>, a character vector
      
      `actual$N`:                                  
      `expected$N`: "2" "1" "2" "2" "2" "1" "1" "1"
      
      `actual$N_group`:                                  
      `expected$N_group`: "2" "2" "2" "2" "2" "2" "2" "2"
      
      `actual$Percent`:                                                      
      `expected$Percent`: "100%" "50%" "100%" "100%" "100%" "50%" "50%" "50%"
      ── Failure ('test-build_multiple_response_table.R:90:3'): build_multiple_response_table() works when we add two grouping variables ──
      `func_output` (`actual`) not equal to `expected_output` (`expected`).
      
        `attr(actual, 'row.names')`:                     
      `attr(expected, 'row.names')`: 1 2 3 4 5 6 7 8 9 10
      
      `actual$col_1`:                                          
      `expected$col_1`: "a" "a" "a" "b" "b" "b" "b" "c" "c" "c"
      
      `actual$col_2`:                                          
      `expected$col_2`: "d" "d" "d" "d" "d" "e" "e" "e" "e" "e"
      
      `actual$enjoy_fruit` is an S3 object of class <xlr_vector/vctrs_vctr>, a logical vector
      `expected$enjoy_fruit` is an S3 object of class <xlr_vector/vctrs_vctr>, a character vector
      
      `actual$N`:                                          
      `expected$N`: "2" "1" "2" "1" "1" "1" "1" "1" "1" "1"
      
      `actual$N_group`:                                          
      `expected$N_group`: "2" "2" "2" "1" "1" "1" "1" "2" "2" "2"
      
       actual$Percent | expected$Percent     
                      - "100%"           [1] 
                      - "50%"            [2] 
                      - "100%"           [3] 
                      - "100%"           [4] 
                      - "100%"           [5] 
                      - "100%"           [6] 
                      - "100%"           [7] 
                      - "50%"            [8] 
                      - "50%"            [9] 
                      - "50%"            [10]
      ── Failure ('test-build_multiple_response_table.R:319:3'): build_multiple_response_table() works for a simple NA case ──
      `func_output` (`actual`) not equal to `expected_output` (`expected`).
      
        `attr(actual, 'row.names')`:        
      `attr(expected, 'row.names')`: 1 2 3 4
      
      `actual$enjoy_fruit` is an S3 object of class <xlr_vector/vctrs_vctr>, a logical vector
      `expected$enjoy_fruit` is an S3 object of class <xlr_vector/vctrs_vctr>, a character vector
      
      `actual$N`:                  
      `expected$N`: "3" "4" "5" "1"
      
      `actual$N_group`:                  
      `expected$N_group`: "7" "7" "7" "7"
      
      `actual$Percent`:                          
      `expected$Percent`: "43%" "57%" "71%" "14%"
      ── Failure ('test-build_multiple_response_table.R:343:3'): build_multiple_response_table() works for NA case and a column group ──
      `func_output` (`actual`) not equal to `expected_output` (`expected`).
      
        `attr(actual, 'row.names')`:                  
      `attr(expected, 'row.names')`: 1 2 3 4 5 6 7 8 9
      
      `actual$col_1`:                                      
      `expected$col_1`: "a" "a" "a" "b" "b" "c" "c" "c" "c"
      
      `actual$enjoy_fruit` is an S3 object of class <xlr_vector/vctrs_vctr>, a logical vector
      `expected$enjoy_fruit` is an S3 object of class <xlr_vector/vctrs_vctr>, a character vector
      
      `actual$N`:                                      
      `expected$N`: "2" "1" "2" "2" "2" "1" "1" "1" "1"
      
      `actual$N_group`:                                      
      `expected$N_group`: "2" "2" "2" "2" "2" "3" "3" "3" "3"
      
      `actual$Percent`:                                                            
      `expected$Percent`: "100%" "50%" "100%" "100%" "100%" "33%" "33%" "33%" "33%"
      ── Failure ('test-build_multiple_response_table.R:374:3'): build_multiple_response_table() works for NA for a group and multiple response column ──
      `func_output` (`actual`) not equal to `expected_output` (`expected`).
      
        `attr(actual, 'row.names')`:                      and 1 more...
      `attr(expected, 'row.names')`: 1 2 3 4 5 6 7 8 9 10           ...
      
          actual$col_1 | expected$col_1               
                       - "a "           [1]           
                       - "a "           [2]           
                       - "b "           [3]           
                       - "b "           [4]           
                       - "c "           [5]           
                       - "c "           [6]           
                       - "c "           [7]           
                       - "c "           [8]           
                       - "NA"           [9]           
                       - "NA"           [10]          
      ... ...            ...            and 1 more ...
      
      `actual$enjoy_fruit` is an S3 object of class <xlr_vector/vctrs_vctr>, a logical vector
      `expected$enjoy_fruit` is an S3 object of class <xlr_vector/vctrs_vctr>, a character vector
      
      `actual$N`:                                           and 1 more...
      `expected$N`: "1" "1" "2" "2" "1" "1" "1" "1" "1" "1" ...          
      
      `actual$N_group`:                                           and 1 more...
      `expected$N_group`: "1" "1" "2" "2" "3" "3" "3" "3" "1" "1" ...          
      
          actual$Percent | expected$Percent               
                         - "100%"           [1]           
                         - "100%"           [2]           
                         - "100%"           [3]           
                         - "100%"           [4]           
                         - "33%"            [5]           
                         - "33%"            [6]           
                         - "33%"            [7]           
                         - "33%"            [8]           
                         - "100%"           [9]           
                         - "100%"           [10]          
      ... ...              ...              and 1 more ...
      ── Failure ('test-build_multiple_response_table.R:498:3'): build_mtable works with weights in the simplest case ──
      `func_output` (`actual`) not equal to `expected_output` (`expected`).
      
        `attr(actual, 'row.names')`:      
      `attr(expected, 'row.names')`: 1 2 3
      
      `actual$enjoy_fruit` is an S3 object of class <xlr_vector/vctrs_vctr>, a logical vector
      `expected$enjoy_fruit` is an S3 object of class <xlr_vector/vctrs_vctr>, a character vector
      
      `actual$N`:                    
      `expected$N`: "0.7" "0.7" "0.9"
      
      `actual$N_group`:                    
      `expected$N_group`: "1.2" "1.2" "1.2"
      
      `actual$Percent`:                    
      `expected$Percent`: "58%" "58%" "75%"
      ── Failure ('test-build_multiple_response_table.R:521:3'): build_mtable works with weights in the simplest case and NA ──
      `func_output` (`actual`) not equal to `expected_output` (`expected`).
      
        `attr(actual, 'row.names')`:        
      `attr(expected, 'row.names')`: 1 2 3 4
      
      `actual$enjoy_fruit` is an S3 object of class <xlr_vector/vctrs_vctr>, a logical vector
      `expected$enjoy_fruit` is an S3 object of class <xlr_vector/vctrs_vctr>, a character vector
      
      `actual$N`:                          
      `expected$N`: "0.7" "0.7" "0.9" "0.4"
      
      `actual$N_group`:                          
      `expected$N_group`: "1.6" "1.6" "1.6" "1.6"
      
      `actual$Percent`:                          
      `expected$Percent`: "44%" "44%" "56%" "25%"
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

