# admiral (1.3.1)

* GitHub: <https://github.com/pharmaverse/admiral>
* Email: <mailto:ben.x.straub@gsk.com>
* GitHub mirror: <https://github.com/cran/admiral>

Run `revdepcheck::cloud_details(, "admiral")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
         'test-derive_vars_query.R:247:3', 'test-derive_vars_query.R:263:3',
         'test-derive_vars_transposed.R:66:3', 'test-derive_vars_transposed.R:111:3',
         'test-dt_level.R:42:3', 'test-dt_level.R:49:3', 'test-duplicates.R:63:3',
         'test-get_summary_records.R:25:3', 'test-period_dataset.R:128:3',
         'test-period_dataset.R:151:3', 'test-period_dataset.R:174:3',
         'test-period_dataset.R:321:3', 'test-period_dataset.R:348:3',
         'test-roxygen2.R:20:3', 'test-slice_derivation.R:242:3',
         'test-transform_range.R:28:3', 'test-transform_range.R:40:3',
         'test-user_helpers.R:16:3', 'test-user_helpers.R:41:3',
         'test-user_utils.R:250:3'
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Failure ('test-derive_vars_dt_dtm_utils.R:154:3'): get_imputation_target_time Test 13: get_dt_dtm_range correctly imputes date ranges ──
       `warnings` has length 4, not length 2.
       ── Failure ('test-derive_vars_dt_dtm_utils.R:156:3'): get_imputation_target_time Test 13: get_dt_dtm_range correctly imputes date ranges ──
       warnings\[2\] does not match "failed to parse".
       Actual value: "Calling `case_when\(\)` with size 1 LHS inputs and size >1 RHS inputs was deprecated in dplyr 1\.2\.0\.\\ni This `case_when\(\)` statement can result in subtle silent bugs and is very inefficient\.\\n\\n  Please use a series of if statements instead:\\n\\n  ```\\n  # Previously\\n  case_when\(scalar_lhs1 ~ rhs1, scalar_lhs2 ~ rhs2, \.default = default\)\\n\\n  # Now\\n  if \(scalar_lhs1\) \{\\n    rhs1\\n  \} else if \(scalar_lhs2\) \{\\n    rhs2\\n  \} else \{\\n    default\\n  \}\\n  ```"
       Backtrace:
           ▆
        1. └─testthat::expect_match(warnings[2], "failed to parse") at test-derive_vars_dt_dtm_utils.R:156:3
        2.   └─testthat:::expect_match_(...)
       
       [ FAIL 2 | WARN 167 | SKIP 129 | PASS 815 ]
       Error: Test failures
       Execution halted
     ```

# adobeanalyticsr (0.5.0)

* GitHub: <https://github.com/benrwoodard/adobeanalyticsr>
* Email: <mailto:benrwoodard@gmail.com>
* GitHub mirror: <https://github.com/cran/adobeanalyticsr>

Run `revdepcheck::cloud_details(, "adobeanalyticsr")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     aw_workspace_report: no visible binding for global variable ‘id’
     make_pretty_segments: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

## In both

*   checking DESCRIPTION meta-information ... NOTE
     ```
       Missing dependency on R >= 4.1.0 because package code uses the pipe
       |> or function shorthand \(...) syntax added in R 4.1.0.
       File(s) using such syntax:
         ‘cm_formula.R’ ‘tag_add.R’
     ```

# agriutilities (1.2.2)

* GitHub: <https://github.com/AparicioJohan/agriutilities>
* Email: <mailto:johanstevenapa@gmail.com>
* GitHub mirror: <https://github.com/cran/agriutilities>

Run `revdepcheck::cloud_details(, "agriutilities")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     check_design_met: no visible binding for global variable ‘id’
     fit_STA: no visible binding for global variable ‘id’
     fit_crd: no visible binding for global variable ‘id’
     single_trial_analysis: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# AHPWR (0.1.0)

* Email: <mailto:lucianea@id.uff.br>
* GitHub mirror: <https://github.com/cran/AHPWR>

Run `revdepcheck::cloud_details(, "AHPWR")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     flow_chart: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# amanida (0.3.0)

* GitHub: <https://github.com/mariallr/amanida>
* Email: <mailto:maria.llambrich@urv.cat>
* GitHub mirror: <https://github.com/cran/amanida>

Run `revdepcheck::cloud_details(, "amanida")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     amanida_vote: no visible binding for global variable ‘id’
     check_names: no visible binding for global variable ‘id’
     compute_amanida: no visible binding for global variable ‘id’
     explore_plot: no visible binding for global variable ‘id’
     volcano_plot: no visible binding for global variable ‘id’
     vote_plot: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

## In both

*   checking dependencies in R code ... NOTE
     ```
     Namespaces in Imports field not imported from:
       ‘magrittr’ ‘tidyverse’
       All declared Imports should be used.
     ```

# amt (0.3.0.0)

* GitHub: <https://github.com/jmsigner/amt>
* Email: <mailto:jsigner@gwdg.de>
* GitHub mirror: <https://github.com/cran/amt>

Run `revdepcheck::cloud_details(, "amt")` for more info

## Newly broken

*   checking whether package ‘amt’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/tmp/workdir/amt/new/amt.Rcheck/00install.out’ for details.
     ```

## Installation

### Devel

```
* installing *source* package ‘amt’ ...
** this is package ‘amt’ version ‘0.3.0.0’
** package ‘amt’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘select_vars’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘amt’
* removing ‘/tmp/workdir/amt/new/amt.Rcheck/amt’


```
### CRAN

```
* installing *source* package ‘amt’ ...
** this is package ‘amt’ version ‘0.3.0.0’
** package ‘amt’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (amt)


```
# AquaticLifeHistory (1.0.5)

* GitHub: <https://github.com/jonathansmart/AquaticLifeHistory>
* Email: <mailto:jonsmartphd@gmail.com>
* GitHub mirror: <https://github.com/cran/AquaticLifeHistory>

Run `revdepcheck::cloud_details(, "AquaticLifeHistory")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     boot_data: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# arrow (22.0.0)

* GitHub: <https://github.com/apache/arrow>
* Email: <mailto:jkeane@gmail.com>
* GitHub mirror: <https://github.com/cran/arrow>

Run `revdepcheck::cloud_details(, "arrow")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
       • tolower(Sys.info()[["sysname"]]) != "windows" is TRUE (1):
         'test-compressed.R:27:3'
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Error ('test-dplyr-group-by.R:274:3'): group_by() with .add ─────────────────
       <lifecycle_error_deprecated/defunctError/rlang_error/error/condition>
       Error: The `add` argument of `group_by()` was deprecated in dplyr 1.0.0 and is now defunct.
       i Please use the `.add` argument instead.
       Backtrace:
            ▆
         1. ├─base::suppressWarnings(...) at test-dplyr-group-by.R:274:3
         2. │ └─base::withCallingHandlers(...)
         3. ├─arrow:::compare_dplyr_binding(...)
         4. │ └─rlang::eval_tidy(expr, rlang::new_data_mask(rlang::env(.input = tbl))) at tests/testthat/helper-expectation.R:87:3
         5. ├─dplyr::collect(group_by(group_by(.input, dbl2), add = FALSE))
         6. ├─dplyr::group_by(group_by(.input, dbl2), add = FALSE)
         7. └─dplyr:::group_by.data.frame(group_by(.input, dbl2), add = FALSE)
         8.   └─dplyr::group_by_prepare(.data, ..., .add = .add, error_call = current_env())
         9.     └─lifecycle::deprecate_stop("1.0.0", "group_by(add = )", "group_by(.add = )")
        10.       └─lifecycle:::deprecate_stop0(msg)
        11.         └─rlang::cnd_signal(...)
       
       [ FAIL 1 | WARN 0 | SKIP 84 | PASS 6709 ]
       Error: Test failures
       Execution halted
     ```

# assertHE (1.0.0)

* GitHub: <https://github.com/dark-peak-analytics/assertHE>
* Email: <mailto:rsmith@darkpeakanalytics.com>
* GitHub mirror: <https://github.com/cran/assertHE>

Run `revdepcheck::cloud_details(, "assertHE")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     find_function_calls_in_folder: no visible binding for global variable
       ‘location’
     processNodes: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id location
     ```

# BaHZING (1.0.0)

* Email: <mailto:jagoodri@usc.edu>
* GitHub mirror: <https://github.com/cran/BaHZING>

Run `revdepcheck::cloud_details(, "BaHZING")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     Format_BaHZING: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# bayesmove (0.2.3)

* GitHub: <https://github.com/joshcullen/bayesmove>
* Email: <mailto:joshcullen10@gmail.com>
* GitHub mirror: <https://github.com/cran/bayesmove>

Run `revdepcheck::cloud_details(, "bayesmove")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     behav_gibbs_sampler: no visible binding for global variable ‘id’
     expand_behavior: no visible binding for global variable ‘id’
     shiny_tracks : server : <anonymous>: no visible binding for global
       variable ‘id’
     Undefined global functions or variables:
       id
     ```

# benchmarkmeData (1.0.4)

* GitHub: <https://github.com/csgillespie/benchmarkme-data>
* Email: <mailto:csgillespie@gmail.com>
* GitHub mirror: <https://github.com/cran/benchmarkmeData>

Run `revdepcheck::cloud_details(, "benchmarkmeData")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     select_results: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# BiVariAn (1.0.2)

* GitHub: <https://github.com/AndresFloresG/BiVariAn>
* Email: <mailto:andres.flores@uaslp.mx>
* GitHub mirror: <https://github.com/cran/BiVariAn>

Run `revdepcheck::cloud_details(, "BiVariAn")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
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
     ! `condition` must be a logical vector, not a logical 1D array.
     Backtrace:
         ▆
      1. ├─BiVariAn::auto_pie_categ(data = data)
      2. │ └─dplyr::if_else(is.na(pos), freq/2, pos)
      3. │   └─vctrs::vec_if_else(...)
      4. └─rlang::abort(message = message, call = call)
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
       > library(testthat)
       > library(BiVariAn)
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
       Error in `if_else(is.na(pos), freq/2, pos)`: `condition` must be a logical vector, not a logical 1D array.
       Backtrace:
           ▆
        1. ├─BiVariAn::auto_pie_categ(data = data) at test-auto_pie_categ.R:11:3
        2. │ └─dplyr::if_else(is.na(pos), freq/2, pos)
        3. │   └─vctrs::vec_if_else(...)
        4. └─rlang::abort(message = message, call = call)
       
       [ FAIL 1 | WARN 0 | SKIP 7 | PASS 144 ]
       Error: Test failures
       Execution halted
     ```

# bootnet (1.6)

* GitHub: <https://github.com/SachaEpskamp/bootnet>
* Email: <mailto:mail@sachaepskamp.com>
* GitHub mirror: <https://github.com/cran/bootnet>

Run `revdepcheck::cloud_details(, "bootnet")` for more info

## Newly broken

*   checking whether package ‘bootnet’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/tmp/workdir/bootnet/new/bootnet.Rcheck/00install.out’ for details.
     ```

## Installation

### Devel

```
* installing *source* package ‘bootnet’ ...
** this is package ‘bootnet’ version ‘1.6’
** package ‘bootnet’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘id’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘bootnet’
* removing ‘/tmp/workdir/bootnet/new/bootnet.Rcheck/bootnet’


```
### CRAN

```
* installing *source* package ‘bootnet’ ...
** this is package ‘bootnet’ version ‘1.6’
** package ‘bootnet’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (bootnet)


```
# bpmnR (0.1.1)

* Email: <mailto:gert.janssenswillen@uhasselt.be>
* GitHub mirror: <https://github.com/cran/bpmnR>

Run `revdepcheck::cloud_details(, "bpmnR")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     calculate_CFC: no visible binding for global variable ‘id’
     create_xml.bpmn: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# broom (1.0.10)

* GitHub: <https://github.com/tidymodels/broom>
* Email: <mailto:simon.couch@posit.co>
* GitHub mirror: <https://github.com/cran/broom>

Run `revdepcheck::cloud_details(, "broom")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     augment.mlogit: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# btw (1.0.0)

* GitHub: <https://github.com/posit-dev/btw>
* Email: <mailto:garrick@adenbuie.com>
* GitHub mirror: <https://github.com/cran/btw>

Run `revdepcheck::cloud_details(, "btw")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     ! No NEWS entries found for package 'dplyr' v1.1.4.9000.
     Backtrace:
          ▆
       1. ├─base::withAutoprint(...)
       2. │ └─base::source(...)
       3. │   ├─base::withVisible(eval(ei, envir))
       4. │   └─base::eval(ei, envir)
       5. │     └─base::eval(ei, envir)
       6. └─btw::btw("@news dplyr", clipboard = FALSE)
       7.   ├─btw::btw_this(...)
       8.   └─btw:::btw_this.environment(...)
       9.     └─btw:::btw_tool_env_describe_environment_impl(...)
      10.       └─btw:::map(...)
      11.         └─base::lapply(.x, .f, ...)
      12.           └─btw (local) FUN(X[[i]], ...)
      13.             ├─btw::btw_this(item, caller_env = environment)
      14.             └─btw:::btw_this.character(item, caller_env = environment)
      15.               └─btw:::dispatch_at_command(cmd, caller_env)
      16.                 └─btw (local) btw_this_cmd(cmd$args)
      17.                   ├─base::I(btw_tool_docs_package_news_impl(package_name, search_term)@value)
      18.                   │ └─base::unique.default(c("AsIs", oldClass(x)))
      19.                   └─btw:::btw_tool_docs_package_news_impl(package_name, search_term)
      20.                     └─cli::cli_abort("No NEWS entries found for package '{package_name}' v{package_version(package_name)}.")
      21.                       └─rlang::abort(...)
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
         2. | \-testthat::quasi_label(enquo(expected), expected.label, arg = "expected")
         3. |   \-rlang::eval_bare(expr, quo_get_env(quo))
         4. +-ellmer::contents_text(btw("@news dplyr"))
         5. | \-S7::S7_dispatch()
         6. \-btw::btw("@news dplyr")
         7.   +-btw::btw_this(...)
         8.   \-btw:::btw_this.environment(...)
         9.     \-btw:::btw_tool_env_describe_environment_impl(...)
        10.       \-btw:::map(...)
        11.         \-base::lapply(.x, .f, ...)
        12.           \-btw (local) FUN(X[[i]], ...)
        13.             +-btw::btw_this(item, caller_env = environment)
        14.             \-btw:::btw_this.character(item, caller_env = environment)
        15.               \-btw:::dispatch_at_command(cmd, caller_env)
        16.                 \-btw (local) btw_this_cmd(cmd$args)
        17.                   +-base::I(btw_tool_docs_package_news_impl(package_name, search_term)@value)
        18.                   | \-base::unique.default(c("AsIs", oldClass(x)))
        19.                   \-btw:::btw_tool_docs_package_news_impl(package_name, search_term)
        20.                     \-cli::cli_abort("No NEWS entries found for package '{package_name}' v{package_version(package_name)}.")
        21.                       \-rlang::abort(...)
       
       [ FAIL 1 | WARN 0 | SKIP 57 | PASS 598 ]
       Error: Test failures
       Execution halted
       Ran 8/8 deferred expressions
     ```

# ccostr (0.1.0)

* Email: <mailto:lars.borty@gmail.com>
* GitHub mirror: <https://github.com/cran/ccostr>

Run `revdepcheck::cloud_details(, "ccostr")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     ccmean: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# chunked (0.6.1)

* GitHub: <https://github.com/edwindj/chunked>
* Email: <mailto:edwindjonge@gmail.com>
* GitHub mirror: <https://github.com/cran/chunked>

Run `revdepcheck::cloud_details(, "chunked")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
         7.       │   │ └─base::withCallingHandlers(...)
         8.       │   └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
         9.       ├─dplyr::groups(ir)
        10.       └─chunked:::groups.chunkwise(ir)
        11.         ├─dplyr::groups(collect(x, first_chunk_only = TRUE))
        12.         ├─dplyr::collect(x, first_chunk_only = TRUE)
        13.         └─chunked:::collect.chunkwise(x, first_chunk_only = TRUE)
        14.           └─x$first_chunk(cmds, x$.warn)
        15.             └─chunked (local) next_chunk(cmds)
        16.               └─chunked:::play(ch, cmds)
        17.                 └─base::eval(expr, list(.data = .data), enclos = env)
        18.                   └─base::eval(expr, list(.data = .data), enclos = env)
        19.                     ├─dplyr::group_by(.data, Species, add = FALSE)
        20.                     └─dplyr:::group_by.data.frame(.data, Species, add = FALSE)
        21.                       └─dplyr::group_by_prepare(.data, ..., .add = .add, error_call = current_env())
        22.                         └─lifecycle::deprecate_stop("1.0.0", "group_by(add = )", "group_by(.add = )")
        23.                           └─lifecycle:::deprecate_stop0(msg)
        24.                             └─rlang::cnd_signal(...)
       
       [ FAIL 2 | WARN 0 | SKIP 0 | PASS 29 ]
       Error: Test failures
       In addition: Warning message:
       In for (k in seq_along(lens)) { :
         closing unused connection 4 (/tmp/RtmpyJTyEH/file11bd2d398f88)
       Execution halted
     ```

# CIMPLE (0.1.0)

* Email: <mailto:howard.baik@yale.edu>
* GitHub mirror: <https://github.com/cran/CIMPLE>

Run `revdepcheck::cloud_details(, "CIMPLE")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     surv_est: no visible binding for global variable ‘id’
     surv_est : coxph_imp: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

## In both

*   checking DESCRIPTION meta-information ... NOTE
     ```
       Missing dependency on R >= 4.1.0 because package code uses the pipe
       |> or function shorthand \(...) syntax added in R 4.1.0.
       File(s) using such syntax:
         ‘long_est.R’
     ```

# cjar (0.2.0)

* Email: <mailto:benrwoodard@gmail.com>
* GitHub mirror: <https://github.com/cran/cjar>

Run `revdepcheck::cloud_details(, "cjar")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     cja_get_dimensions: no visible binding for global variable ‘id’
     cja_get_metrics: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# clockify (0.1.6)

* Email: <mailto:andrew.b.collier@gmail.com>
* GitHub mirror: <https://github.com/cran/clockify>

Run `revdepcheck::cloud_details(, "clockify")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     custom_fields: no visible binding for global variable ‘id’
     parse_client: no visible binding for global variable ‘id’
     parse_projects: no visible binding for global variable ‘id’
     parse_shared_report: no visible binding for global variable ‘id’
     parse_shared_report_list: no visible binding for global variable ‘id’
     parse_tags: no visible binding for global variable ‘id’
     parse_tasks: no visible binding for global variable ‘id’
     parse_time_entries: no visible binding for global variable ‘id’
     reports_summary : <anonymous> : <anonymous> : <anonymous>: no visible
       binding for global variable ‘id’
     simplify_group: no visible binding for global variable ‘id’
     simplify_role : <anonymous>: no visible binding for global variable
       ‘id’
     simplify_user: no visible binding for global variable ‘id’
     unpack_workspace: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# cloudos (0.4.0)

* GitHub: <https://github.com/lifebit-ai/cloudos>
* Email: <mailto:sangram@lifebit.ai>
* GitHub mirror: <https://github.com/cran/cloudos>

Run `revdepcheck::cloud_details(, "cloudos")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     .cb_search_phenotypes_v1: no visible binding for global variable ‘id’
     .cb_search_phenotypes_v2: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

## In both

*   checking Rd files ... NOTE
     ```
     checkRd: (-1) cb_participant_count.Rd:13: Lost braces
         13 | \item{query}{A phenotype query defined using the code{\link{phenotype}} function and logic operators (see example below)}
            |                                                      ^
     checkRd: (-1) phenotype.Rd:10: Lost braces
         10 | \item{id}{A single phenotype id. Possible phenotyoes can be explored using the code{\link{cb_search_phenotypes}} function}
            |                                                                                    ^
     ```

# corella (0.1.4)

* GitHub: <https://github.com/AtlasOfLivingAustralia/corella>
* Email: <mailto:dax.kellie@csiro.au>
* GitHub mirror: <https://github.com/cran/corella>

Run `revdepcheck::cloud_details(, "corella")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
       2. │ └─corella:::check_dataframe(.df)
       3. │   └─corella:::check_contains_terms(...)
       4. │     └─corella:::full_workflow_message(...)
       5. │       └─corella:::minreq_terms_message(req_terms_results)
       6. │         └─corella:::build_req_terms_table(req_terms_results)
       7. │           ├─tidyr::replace_na(...)
       8. │           ├─dplyr::mutate(...)
       9. │           └─dplyr:::mutate.data.frame(...)
      10. │             └─dplyr:::mutate_cols(.data, dplyr_quosures(...), by)
      11. │               ├─base::withCallingHandlers(...)
      12. │               └─dplyr:::mutate_col(dots[[i]], data, mask, new_columns)
      13. │                 └─mask$eval_all_mutate(quo)
      14. │                   └─dplyr (local) eval()
      15. ├─dplyr::case_when(...)
      16. │ └─vctrs::vec_case_when(...)
      17. └─vctrs (local) `<fn>`()
      18.   └─vctrs::vec_default_ptype2(...)
      19.     ├─base::withRestarts(...)
      20.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
      21.     │   └─base (local) doWithOneRestart(return(expr), restart)
      22.     └─vctrs::stop_incompatible_type(...)
      23.       └─vctrs:::stop_incompatible(...)
      24.         └─vctrs:::stop_vctrs(...)
      25.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
       Expected `quiet_sw(df)` to run without any errors.
       i Actually got a <dplyr:::mutate_error> with text:
         i In argument: `missing = case_when(...)`.
         i In group 1: `term_group = "Date/Time"`.
         Caused by error in `case_when()`:
         ! Can't combine <logical> and `.default` <character>.
       Backtrace:
           ▆
        1. └─corella (local) no_error_check(tibble::tibble(basisOfRecord = "humanObservation")) at test-suggest_workflow.R:10:3
        2.   └─testthat::expect_no_error(quiet_sw(df)) at test-suggest_workflow.R:3:3
       ── Failure ('test-suggest_workflow.R:14:3'): suggest_workflow() doesn't error for common use cases ──
       Expected `quiet_sw(df)` to run without any errors.
       i Actually got a <dplyr:::mutate_error> with text:
         i In argument: `missing = case_when(...)`.
         i In group 1: `term_group = "Date/Time"`.
         Caused by error in `case_when()`:
         ! Can't combine <logical> and `.default` <character>.
       Backtrace:
           ▆
        1. └─corella (local) no_error_check(...) at test-suggest_workflow.R:14:3
        2.   └─testthat::expect_no_error(quiet_sw(df)) at test-suggest_workflow.R:3:3
       
       [ FAIL 2 | WARN 0 | SKIP 6 | PASS 341 ]
       Error: Test failures
       Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     ...
     
     .............
     --- finished re-building ‘checking-your-dataset.Rmd’
     
     --- re-building ‘quick_start_guide.Rmd’ using rmarkdown
     
     .............
     
     Quitting from quick_start_guide.Rmd:153-156 [unnamed-chunk-10]
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     NULL
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'quick_start_guide.Rmd' failed with diagnostics:
     ℹ In argument: `missing = case_when(...)`.
     ℹ In group 1: `term_group = "Date/Time"`.
     Caused by error in `case_when()`:
     ! Can't combine <logical> and `.default` <character>.
     --- failed re-building ‘quick_start_guide.Rmd’
     
     SUMMARY: processing the following file failed:
       ‘quick_start_guide.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

# crawl (2.3.0)

* Email: <mailto:devin.johnson@noaa.gov>
* GitHub mirror: <https://github.com/cran/crawl>

Run `revdepcheck::cloud_details(, "crawl")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     crw_as_sf.list : make_mls: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

## In both

*   checking Rd files ... NOTE
     ```
     checkRd: (-1) crwPredict.Rd:54-68: Lost braces
         54 | \item("predTime"){
            |                  ^
     checkRd: (-1) crwPredict.Rd:59: Lost braces
         59 |  no longer the default (see \item{return.type}). If the original data were 
            |                                  ^
     ```

# crosswalkr (0.3.0)

* GitHub: <https://github.com/btskinner/crosswalkr>
* Email: <mailto:ben@btskinner.io>
* GitHub mirror: <https://github.com/cran/crosswalkr>

Run `revdepcheck::cloud_details(, "crosswalkr")` for more info

## Newly broken

*   checking re-building of vignette outputs ... ERROR
     ```
     Error(s) in re-building vignettes:
       ...
     --- re-building ‘collapse.Rmd’ using rmarkdown
     --- finished re-building ‘collapse.Rmd’
     
     --- re-building ‘crosswalkr.Rmd’ using rmarkdown
     
     Quitting from crosswalkr.Rmd:198-204 [unnamed-chunk-9]
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     NULL
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'crosswalkr.Rmd' failed with diagnostics:
     `tbl_df()` was deprecated in dplyr 1.0.0 and is now defunct.
     ℹ Please use `tibble::as_tibble()` instead.
     --- failed re-building ‘crosswalkr.Rmd’
     
     SUMMARY: processing the following file failed:
       ‘crosswalkr.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

# dbplot (0.3.3)

* GitHub: <https://github.com/edgararuiz/dbplot>
* Email: <mailto:edgararuiz@gmail.com>
* GitHub mirror: <https://github.com/cran/dbplot>

Run `revdepcheck::cloud_details(, "dbplot")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     
     > ### Name: db_compute_boxplot
     > ### Title: Returns a dataframe with boxplot calculations
     > ### Aliases: db_compute_boxplot
     > 
     > ### ** Examples
     > 
     > 
     > mtcars %>%
     +   db_compute_boxplot(am, mpg)
     Error:
     ! The `add` argument of `group_by()` was deprecated in dplyr 1.0.0 and
       is now defunct.
     ℹ Please use the `.add` argument instead.
     Backtrace:
         ▆
      1. ├─mtcars %>% db_compute_boxplot(am, mpg)
      2. └─dbplot::db_compute_boxplot(., am, mpg)
      3.   ├─dplyr::group_by(data, !!!x, add = TRUE)
      4.   └─dplyr:::group_by.data.frame(data, !!!x, add = TRUE)
      5.     └─dplyr::group_by_prepare(.data, ..., .add = .add, error_call = current_env())
      6.       └─lifecycle::deprecate_stop("1.0.0", "group_by(add = )", "group_by(.add = )")
      7.         └─lifecycle:::deprecate_stop0(msg)
      8.           └─rlang::cnd_signal(...)
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
         9.         └─lifecycle::deprecate_stop("1.0.0", "group_by(add = )", "group_by(.add = )")
        10.           └─lifecycle:::deprecate_stop0(msg)
        11.             └─rlang::cnd_signal(...)
       ── Error ('test-boxplots.R:8:3'): db_compute_boxplot() returns the right number of rows ──
       <lifecycle_error_deprecated/defunctError/rlang_error/error/condition>
       Error: The `add` argument of `group_by()` was deprecated in dplyr 1.0.0 and is
       now defunct.
       ℹ Please use the `.add` argument instead.
       Backtrace:
            ▆
         1. ├─testthat::expect_equal(...) at test-boxplots.R:8:3
         2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
         3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
         4. ├─base::nrow(db_compute_boxplot(mtcars, am, mpg))
         5. └─dbplot::db_compute_boxplot(mtcars, am, mpg)
         6.   ├─dplyr::group_by(data, !!!x, add = TRUE)
         7.   └─dplyr:::group_by.data.frame(data, !!!x, add = TRUE)
         8.     └─dplyr::group_by_prepare(.data, ..., .add = .add, error_call = current_env())
         9.       └─lifecycle::deprecate_stop("1.0.0", "group_by(add = )", "group_by(.add = )")
        10.         └─lifecycle:::deprecate_stop0(msg)
        11.           └─rlang::cnd_signal(...)
       
       [ FAIL 2 | WARN 0 | SKIP 0 | PASS 26 ]
       Error: Test failures
       Execution halted
     ```

# dbplyr (2.5.1)

* GitHub: <https://github.com/tidyverse/dbplyr>
* Email: <mailto:hadley@posit.co>
* GitHub mirror: <https://github.com/cran/dbplyr>

Run `revdepcheck::cloud_details(, "dbplyr")` for more info

## Newly broken

*   checking Rd cross-references ... WARNING
     ```
     Missing link(s) in Rd file 'nycflights13.Rd':
       ‘[dplyr:src_dbi]{dplyr::src_postgres()}’
     
     See section 'Cross-references' in the 'Writing R Extensions' manual.
     ```

# demodelr (2.0.0)

* Email: <mailto:zobitz@augsburg.edu>
* GitHub mirror: <https://github.com/cran/demodelr>

Run `revdepcheck::cloud_details(, "demodelr")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     compute_likelihood: no visible binding for global variable ‘id’
     mcmc_analyze: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# dexter (1.7.0)

* GitHub: <https://github.com/dexter-psychometrics/dexter>
* Email: <mailto:jesse.koops@cito.nl>
* GitHub mirror: <https://github.com/cran/dexter>

Run `revdepcheck::cloud_details(, "dexter")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     > ## Don't show: 
     >  RcppArmadillo::armadillo_throttle_cores(1)
     > ## End(Don't show)
     > 
     > db = start_new_project(verbAggrRules, ":memory:", person_properties=list(gender='unknown'))
     > add_booklet(db, verbAggrData, "agg")
     no column `person_id` provided, automatically generating unique person id's
     $items
      [1] "S1DoCurse"   "S1DoScold"   "S1DoShout"   "S1WantCurse" "S1WantScold"
      [6] "S1WantShout" "S2DoCurse"   "S2DoScold"   "S2DoShout"   "S2WantCurse"
     [11] "S2WantScold" "S2WantShout" "S3DoCurse"   "S3DoScold"   "S3DoShout"  
     [16] "S3WantCurse" "S3WantScold" "S3WantShout" "S4DoCurse"   "S4DoScold"  
     [21] "S4DoShout"   "S4WantCurse" "S4WantScold" "S4WantShout"
     
     $person_properties
     [1] "gender"
     
     $columns_ignored
     [1] "anger"
     
     > dd = DIF(db,person_property="gender")
     Error in .Object$initialize(...) : 
       'list' object cannot be coerced to type 'logical'
     Calls: DIF ... progress_bar -> new -> initialize -> initialize -> <Anonymous>
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
       [ FAIL 1 | WARN 0 | SKIP 3 | PASS 263 ]
       
       ══ Skipped tests (3) ═══════════════════════════════════════════════════════════
       • On CRAN (3): 'test_latent_cor.R:25:3', 'test_plausible_scores.R:48:3',
         'test_theta.R:61:3'
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Error ('test_ability.R:10:3'): inconsistencies between data and parms are handled correctly ──
       Error in `.Object$initialize(...)`: 'list' object cannot be coerced to type 'logical'
       Backtrace:
           ▆
        1. └─dexter::fit_enorm(db) at test_ability.R:10:3
        2.   └─dexter:::fit_enorm_(...)
        3.     └─dexter:::get_prog_bar(...)
        4.       └─dexter:::progress_bar(...)
        5.         └─methods::new(`<chr>`, ...)
        6.           ├─methods::initialize(value, ...)
        7.           └─methods::initialize(value, ...)
        8.             └─.Object$initialize(...)
       
       [ FAIL 1 | WARN 0 | SKIP 3 | PASS 263 ]
       Error: Test failures
       In addition: Warning message:
       call dbDisconnect() when finished working with a connection 
       Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     Error(s) in re-building vignettes:
     --- re-building ‘DIF_vignette.Rmd’ using rmarkdown
     
     Quitting from DIF_vignette.Rmd:43-46 [dif]
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     <error/rlang_error>
     Error in `.Object$initialize()`:
     ! 'list' object cannot be coerced to type 'logical'
     ---
     Backtrace:
         ▆
      1. └─dexter::DIF(db, "gender")
      2.   └─dexter:::get_prog_bar(retrieve_data = is_db(dataSrc))
      3.     └─dexter:::progress_bar(...)
      4.       └─methods::new(`<chr>`, ...)
      5.         ├─methods::initialize(value, ...)
      6.         └─methods::initialize(value, ...)
      7.           └─.Object$initialize(...)
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'DIF_vignette.Rmd' failed with diagnostics:
     'list' object cannot be coerced to type 'logical'
     --- failed re-building ‘DIF_vignette.Rmd’
     
     --- re-building ‘Equating.Rmd’ using rmarkdown
     ```

# discord (1.2.4.1)

* GitHub: <https://github.com/R-Computing-Lab/discord>
* Email: <mailto:garrissm@wfu.edu>
* GitHub mirror: <https://github.com/cran/discord>

Run `revdepcheck::cloud_details(, "discord")` for more info

## Newly broken

*   checking re-building of vignette outputs ... ERROR
     ```
     Error(s) in re-building vignettes:
     --- re-building ‘Power.Rmd’ using rmarkdown
     ```

# dm (1.0.12)

* GitHub: <https://github.com/cynkra/dm>
* Email: <mailto:kirill@cynkra.com>
* GitHub mirror: <https://github.com/cran/dm>

Run `revdepcheck::cloud_details(, "dm")` for more info

## Newly broken

*   checking Rd cross-references ... WARNING
     ```
     Missing link(s) in Rd file 'copy_dm_to.Rd':
       ‘[dplyr:src_dbi]{dplyr::src_dbi}’
     
     See section 'Cross-references' in the 'Writing R Extensions' manual.
     ```

# doublIn (0.2.0)

* Email: <mailto:v.h.arntzen@math.leidenuniv.nl>
* GitHub mirror: <https://github.com/cran/doublIn>

Run `revdepcheck::cloud_details(, "doublIn")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     Visualize_contact_tracing_data : prepare_network: no visible binding
       for global variable ‘id’
     Visualize_contact_tracing_data : <anonymous>: no visible binding for
       global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# drawsample (1.0.1)

* GitHub: <https://github.com/atalay-k/drawsample>
* Email: <mailto:katalay@hacettepe.edu.tr>
* GitHub mirror: <https://github.com/cran/drawsample>

Run `revdepcheck::cloud_details(, "drawsample")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     draw_sample: no visible binding for global variable ‘id’
     draw_sample_ir: no visible binding for global variable ‘id’
     draw_sample_n: no visible binding for global variable ‘id’
     draw_sample_n_ir: no visible binding for global variable ‘id’
     draw_sample_shiny : server : draw_sample_ir: no visible binding for
       global variable ‘id’
     draw_sample_shiny : server : draw_sample_n_ir: no visible binding for
       global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# DrugExposureDiagnostics (1.1.5)

* GitHub: <https://github.com/darwin-eu/DrugExposureDiagnostics>
* Email: <mailto:g.inberg@erasmusmc.nl>
* GitHub mirror: <https://github.com/cran/DrugExposureDiagnostics>

Run `revdepcheck::cloud_details(, "DrugExposureDiagnostics")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
         3. │   ├─base::do.call(Map, c(f = rbind, Filter(Negate(is.null), resultList)))
         4. │   └─base (local) `<fn>`(f = `<fn>`, `<named list>`, `<named list>`, `<named list>`)
         5. │     └─base::mapply(FUN = f, ..., SIMPLIFY = FALSE)
         6. │       └─base (local) `<fn>`(dots[[1L]][[1L]], dots[[2L]][[1L]], dots[[3L]][[1L]])
         7. │         └─dplyr (local) rbind(deparse.level, ...)
         8. │           └─dplyr::bind_rows(...)
         9. │             └─vctrs::vec_rbind(!!!dots, .names_to = .id, .error_call = current_env())
        10. │               └─vctrs (local) `<fn>`()
        11. │                 └─vctrs (local) vec_ptype2.rowwise_df.rowwise_df(...)
        12. │                   └─vctrs:::rww_ptype2(x, y, ...)
        13. │                     ├─dplyr::rowwise(df_ptype2(x, y, ...))
        14. │                     └─vctrs::df_ptype2(x, y, ...)
        15. └─vctrs (local) `<fn>`()
        16.   └─vctrs::vec_default_ptype2(...)
        17.     ├─base::withRestarts(...)
        18.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
        19.     │   └─base (local) doWithOneRestart(return(expr), restart)
        20.     └─vctrs::stop_incompatible_type(...)
        21.       └─vctrs:::stop_incompatible(...)
        22.         └─vctrs:::stop_vctrs(...)
        23.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
       
       [ FAIL 2 | WARN 122 | SKIP 5 | PASS 483 ]
       Error: Test failures
       Execution halted
     ```

# dtplyr (1.3.2)

* GitHub: <https://github.com/tidyverse/dtplyr>
* Email: <mailto:hadley@posit.co>
* GitHub mirror: <https://github.com/cran/dtplyr>

Run `revdepcheck::cloud_details(, "dtplyr")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
         'test-tidyeval-across.R:289:3', 'test-tidyeval-across.R:299:3',
         'test-tidyeval.R:194:3', 'test-tidyeval.R:299:3', 'test-tidyeval.R:405:3',
         'test-unite.R:71:3'
       • tidyselect issue #221 (1): 'test-tidyeval-across.R:305:3'
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Error ('test-step-group.R:29:3'): can add groups if requested ───────────────
       <lifecycle_error_deprecated/defunctError/rlang_error/error/condition>
       Error: The `add` argument of `group_by()` was deprecated in dplyr 1.0.0 and is now defunct.
       i Please use the `.add` argument instead.
       Backtrace:
           ▆
        1. ├─dt %>% group_by(x) %>% group_by(y, add = TRUE)
        2. ├─dplyr::group_by(., y, add = TRUE)
        3. └─dtplyr:::group_by.dtplyr_step(., y, add = TRUE)
        4.   ├─base::eval(expr(dplyr::group_by_prepare(.data, !!!dots, .add = .add)))
        5.   │ └─base::eval(expr(dplyr::group_by_prepare(.data, !!!dots, .add = .add)))
        6.   └─dplyr::group_by_prepare(.data, y = y, add = TRUE, .add = .add)
        7.     └─lifecycle::deprecate_stop("1.0.0", "group_by(add = )", "group_by(.add = )")
        8.       └─lifecycle:::deprecate_stop0(msg)
        9.         └─rlang::cnd_signal(...)
       
       [ FAIL 1 | WARN 0 | SKIP 35 | PASS 738 ]
       Error: Test failures
       Execution halted
     ```

# duckplyr (1.1.3)

* GitHub: <https://github.com/tidyverse/duckplyr>
* Email: <mailto:kirill@cynkra.com>
* GitHub mirror: <https://github.com/cran/duckplyr>

Run `revdepcheck::cloud_details(, "duckplyr")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
       Error in `count(., x)`: This operation cannot be carried out by DuckDB, and the input is a stingy duckplyr frame.
       i Use `compute(prudence = "lavish")` to materialize to temporary storage and continue with duckplyr.
       i See `vignette("prudence")` for other options.
       Caused by error in `quo_is_null()`:
       ! `quo` must be a quosure
       ── Error ('test-prudence.R:52:3'): prudence after operation with success ───────
       Error in `count(.)`: This operation cannot be carried out by DuckDB, and the input is a stingy duckplyr frame.
       i Use `compute(prudence = "lavish")` to materialize to temporary storage and continue with duckplyr.
       i See `vignette("prudence")` for other options.
       Caused by error in `quo_is_null()`:
       ! `quo` must be a quosure
       
       [ FAIL 17 | WARN 6 | SKIP 597 | PASS 2276 ]
       Deleting unused snapshots:
       • fallback/fallback-2.dcf
       • fallback/fallback.dcf
       Error: Test failures
       
       🛠: 2170
       🔨: 1187
       🦆:  983
       add_count, anti_join, arrange, arrange.data.frame, compute, count, count.data.frame, cross_join, distinct, distinct.data.frame, do, eval, filter, filter.data.frame, full_join, group_by, group_indices, group_keys, group_map, group_modify, group_nest, group_size, group_split, group_trim, head, inner_join, inner_join.data.frame, intersect, left_join, left_join.data.frame, mutate, mutate.data.frame, n_groups, nest_by, nest_join, pull, reframe, relocate, rename, rename_with, right_join, rows_append, rows_delete, rows_insert, rows_patch, rows_update, rows_upsert, rowwise, select, select.data.frame, semi_join, semi_join.data.frame, setdiff, setequal, slice, slice_head, slice_head.data.frame, slice_sample, slice_tail, summarise, summarise.data.frame, symdiff, transmute, ungroup, union_all
       
       00:02:13.59392
       Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     ...
     --- re-building ‘prudence.Rmd’ using rmarkdown
     
     Quitting from prudence.Rmd:288-292 [unnamed-chunk-16]
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     NULL
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'prudence.Rmd' failed with diagnostics:
     This operation cannot be carried out by DuckDB, and the input is a
     stingy duckplyr frame.
     ℹ Use `compute(prudence = "lavish")` to materialize to temporary storage and
       continue with duckplyr.
     ℹ See `vignette("prudence")` for other options.
     Caused by error in `quo_is_null()`:
     ! `quo` must be a quosure
     --- failed re-building ‘prudence.Rmd’
     
     --- re-building ‘telemetry.Rmd’ using rmarkdown
     --- finished re-building ‘telemetry.Rmd’
     
     SUMMARY: processing the following files failed:
       ‘large.Rmd’ ‘prudence.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

# dynwrap (1.2.4)

* GitHub: <https://github.com/dynverse/dynwrap>
* Email: <mailto:rcannood@gmail.com>
* GitHub mirror: <https://github.com/cran/dynwrap>

Run `revdepcheck::cloud_details(, "dynwrap")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     get_ti_methods: no visible binding for global variable ‘id’
     simplify_replace_edges: no visible binding for global variable ‘id’
     simplify_trajectory: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# edeaR (0.9.5)

* GitHub: <https://github.com/bupaverse/edeaR>
* Email: <mailto:gert.janssenswillen@uhasselt.be>
* GitHub mirror: <https://github.com/cran/edeaR>

Run `revdepcheck::cloud_details(, "edeaR")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     plot_trace_coverage: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# ediblecity (0.2.1)

* GitHub: <https://github.com/icra/ediblecity>
* Email: <mailto:josep.pueyo@udg.edu>
* GitHub mirror: <https://github.com/cran/ediblecity>

Run `revdepcheck::cloud_details(, "ediblecity")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     UHI: no visible binding for global variable ‘location’
     green_capita: no visible binding for global variable ‘location’
     no2_seq: no visible binding for global variable ‘location’
     runoff_prev: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id location
     ```

## In both

*   checking DESCRIPTION meta-information ... NOTE
     ```
       Missing dependency on R >= 4.1.0 because package code uses the pipe
       |> or function shorthand \(...) syntax added in R 4.1.0.
       File(s) using such syntax:
         ‘indicator_volunteers.R’
     ```

# eesim (0.1.0)

* GitHub: <https://github.com/sakoehler7/eesim>
* Email: <mailto:brooke.anderson@colostate.edu>
* GitHub mirror: <https://github.com/cran/eesim>

Run `revdepcheck::cloud_details(, "eesim")` for more info

## Newly broken

*   checking re-building of vignette outputs ... ERROR
     ```
     Error(s) in re-building vignettes:
       ...
     --- re-building ‘eesim.Rmd’ using rmarkdown
     
     Quitting from eesim.Rmd:34-50 [unnamed-chunk-2]
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     NULL
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'eesim.Rmd' failed with diagnostics:
     `tbl_df()` was deprecated in dplyr 1.0.0 and is now defunct.
     ℹ Please use `tibble::as_tibble()` instead.
     --- failed re-building ‘eesim.Rmd’
     
     SUMMARY: processing the following file failed:
       ‘eesim.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

## In both

*   checking Rd files ... NOTE
     ```
     checkRd: (-1) eesim.Rd:148-151: Lost braces in \itemize; \value handles \item{}{} directly
     checkRd: (-1) eesim.Rd:152-155: Lost braces in \itemize; \value handles \item{}{} directly
     checkRd: (-1) eesim.Rd:156-158: Lost braces in \itemize; \value handles \item{}{} directly
     ```

# eider (1.0.0)

* GitHub: <https://github.com/alan-turing-institute/eider>
* Email: <mailto:crangelsmith@turing.ac.uk>
* GitHub mirror: <https://github.com/cran/eider>

Run `revdepcheck::cloud_details(, "eider")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     featurise_count: no visible binding for global variable ‘id’
     featurise_present: no visible binding for global variable ‘id’
     featurise_summary: no visible binding for global variable ‘id’
     featurise_time_since: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# eph (1.0.2)

* GitHub: <https://github.com/ropensci/eph>
* Email: <mailto:carolinapradier@gmail.com>
* GitHub mirror: <https://github.com/cran/eph>

Run `revdepcheck::cloud_details(, "eph")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     > pool_trimestral <- organize_panels(
     +   bases = lista_bases,
     +   variables = c("P21", "ESTADO"),
     +   window = "trimestral"
     + )
     Error in `dplyr::mutate()`:
     ℹ In argument: `Periodo = dplyr::case_when(...)`.
     Caused by error in `vec_case_when()`:
     ! Can't merge the outer name `..2 (left)` with a vector of length > 1.
     Please supply a `.name_spec` specification.
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
      11. │ └─vctrs::vec_case_when(...)
      12. └─rlang::abort(message = message)
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
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
       Caused by error in `vec_case_when()`:
       ! Can't merge the outer name `..2 (left)` with a vector of length > 1.
       Please supply a `.name_spec` specification.
       
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
     Caused by error in `vec_case_when()`:
     ! Can't merge the outer name `..2 (left)` with a vector of length > 1.
     Please supply a `.name_spec` specification.
     --- failed re-building ‘eph.Rmd’
     
     --- re-building ‘estimacion_pobreza.Rmd’ using rmarkdown
     trying URL 'https://github.com/holatam/data/raw/master/eph/canasta/canastas.rds'
     Content type 'application/octet-stream' length 3711 bytes
     ==================================================
     downloaded 3711 bytes
     ```

# eq5dsuite (1.0.1)

* Email: <mailto:krand@mathsinhealth.com>
* GitHub mirror: <https://github.com/cran/eq5dsuite>

Run `revdepcheck::cloud_details(, "eq5dsuite")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     .pchctab: no visible binding for global variable ‘id’
     table_1_2_4: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# estadistica (1.0)

* Email: <mailto:estadistic@uv.es>
* GitHub mirror: <https://github.com/cran/estadistica>

Run `revdepcheck::cloud_details(, "estadistica")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     regresion.simple: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# ESTER (0.2.0)

* GitHub: <https://github.com/lnalborczyk/ESTER>
* Email: <mailto:ladislas.nalborczyk@gmail.com>
* GitHub mirror: <https://github.com/cran/ESTER>

Run `revdepcheck::cloud_details(, "ESTER")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     distER: no visible binding for global variable ‘id’
     plot.simER: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

## In both

*   checking Rd files ... NOTE
     ```
     checkRd: (-1) aic.Rd:25: Escaped LaTeX specials: \&
     checkRd: (-1) aic.Rd:29: Escaped LaTeX specials: \&
     checkRd: (-1) ictab.Rd:56: Escaped LaTeX specials: \&
     checkRd: (-1) ictab.Rd:60: Escaped LaTeX specials: \&
     checkRd: (-1) ictab.Rd:64: Escaped LaTeX specials: \&
     ```

# etl (0.4.2)

* GitHub: <https://github.com/beanumber/etl>
* Email: <mailto:ben.baumer@gmail.com>
* GitHub mirror: <https://github.com/cran/etl>

Run `revdepcheck::cloud_details(, "etl")` for more info

## Newly broken

*   checking Rd cross-references ... WARNING
     ```
     Missing link(s) in Rd file 'etl.Rd':
       ‘[dplyr:src_dbi]{dplyr::src_dbi()}’
     
     Missing link(s) in Rd file 'src_mysql_cnf.Rd':
       ‘[dplyr:src_dbi]{dplyr::src_mysql()}’
     
     See section 'Cross-references' in the 'Writing R Extensions' manual.
     ```

*   checking R code for possible problems ... NOTE
     ```
     etl_transform.etl_cities: no visible binding for global variable
       ‘location’
     Undefined global functions or variables:
       location
     ```

# evanverse (0.3.7)

* GitHub: <https://github.com/evanbio/evanverse>
* Email: <mailto:evanzhou.bio@gmail.com>
* GitHub mirror: <https://github.com/cran/evanverse>

Run `revdepcheck::cloud_details(, "evanverse")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
         'test-plot_forest.R:32:3', 'test-plot_forest.R:53:3',
         'test-plot_forest.R:71:3', 'test-plot_forest.R:93:3',
         'test-plot_forest.R:119:3', 'test-plot_forest.R:140:3'
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Failure ('test-plot_pie.R:69:3'): plot_pie() works with character vector input ──
       `p <- plot_pie(vec, preview = FALSE)` produced warnings.
       ── Failure ('test-plot_pie.R:75:3'): plot_pie() works with data.frame input ────
       `p <- plot_pie(df, group_col = "group", count_col = "count", preview = FALSE)` produced warnings.
       ── Failure ('test-plot_pie.R:97:5'): plot_pie() handles different label types ──
       `p <- plot_pie(df, label = lbl, preview = FALSE)` produced warnings.
       ── Failure ('test-plot_pie.R:97:5'): plot_pie() handles different label types ──
       `p <- plot_pie(df, label = lbl, preview = FALSE)` produced warnings.
       ── Failure ('test-plot_pie.R:97:5'): plot_pie() handles different label types ──
       `p <- plot_pie(df, label = lbl, preview = FALSE)` produced warnings.
       ── Failure ('test-plot_pie.R:97:5'): plot_pie() handles different label types ──
       `p <- plot_pie(df, label = lbl, preview = FALSE)` produced warnings.
       ── Failure ('test-plot_pie.R:135:3'): plot_pie() handles custom colors ─────────
       `p <- plot_pie(df, fill = custom_colors, preview = FALSE)` produced warnings.
       ── Failure ('test-plot_pie.R:141:3'): plot_pie() handles custom title and colors ──
       `... <- NULL` produced warnings.
       
       [ FAIL 8 | WARN 5 | SKIP 44 | PASS 1310 ]
       Error: Test failures
       Execution halted
     ```

# expertsurv (1.4.0)

* Email: <mailto:phcooney@tcd.ie>
* GitHub mirror: <https://github.com/cran/expertsurv>

Run `revdepcheck::cloud_details(, "expertsurv")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     make_profile_surv: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# FairMclus (2.2.1)

* Email: <mailto:carlossantos.csm@gmail.com>
* GitHub mirror: <https://github.com/cran/FairMclus>

Run `revdepcheck::cloud_details(, "FairMclus")` for more info

## Newly broken

*   checking whether package ‘FairMclus’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/tmp/workdir/FairMclus/new/FairMclus.Rcheck/00install.out’ for details.
     ```

## Newly fixed

*   checking dependencies in R code ... NOTE
     ```
     Namespaces in Imports field not imported from:
       ‘base’ ‘data.table’
       All declared Imports should be used.
     ```

## Installation

### Devel

```
* installing *source* package ‘FairMclus’ ...
** this is package ‘FairMclus’ version ‘2.2.1’
** package ‘FairMclus’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error: object ‘id’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘FairMclus’
* removing ‘/tmp/workdir/FairMclus/new/FairMclus.Rcheck/FairMclus’


```
### CRAN

```
* installing *source* package ‘FairMclus’ ...
** this is package ‘FairMclus’ version ‘2.2.1’
** package ‘FairMclus’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (FairMclus)


```
# fbar (0.6.0)

* GitHub: <https://github.com/maxconway/fbar>
* Email: <mailto:conway.max1@gmail.com>
* GitHub mirror: <https://github.com/cran/fbar>

Run `revdepcheck::cloud_details(, "fbar")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     get_BiGG: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# feisr (1.3.0)

* GitHub: <https://github.com/ruettenauer/feisr>
* Email: <mailto:ruettenauer@sowi.uni-kl.de>
* GitHub mirror: <https://github.com/cran/feisr>

Run `revdepcheck::cloud_details(, "feisr")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     Running examples in ‘feisr-Ex.R’ failed
     The error most likely occurred in:
     
     > ### Name: feis
     > ### Title: Fixed Effects Individual Slope Estimator
     > ### Aliases: feis formula.feis terms.feis residuals.feis df.residual.feis
     > ###   coef.feis sigma.feis deviance.feis nobs.feis fitted.feis
     > ###   hatvalues.feis
     > 
     > ### ** Examples
     > 
     > data("mwp", package = "feisr")
     > feis.mod <- feis(lnw ~ marry + enrol + as.factor(yeargr) | exp + I(exp^2),
     +                  data = mwp, id = "id", robust = TRUE)
     Error in stats::lm.fit(X, Y, ...) : NA/NaN/Inf in 'x'
     Calls: feis -> <Anonymous>
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
           ▆
        1. └─feisr::feis(...) at test_methods.R:10:1
        2.   └─stats::lm.fit(X, Y, ...)
       ── Error ('test_rownames.R:15:1'): (code run outside of `test_that()`) ─────────
       Error in `stats::lm.fit(X, Y, ...)`: NA/NaN/Inf in 'x'
       Backtrace:
           ▆
        1. └─feisr::feis(...) at test_rownames.R:15:1
        2.   └─stats::lm.fit(X, Y, ...)
       ── Error ('test_slopes.R:14:1'): (code run outside of `test_that()`) ───────────
       Error in `stats::lm.fit(X, Y, ...)`: NA/NaN/Inf in 'x'
       Backtrace:
           ▆
        1. └─feisr::feis(...) at test_slopes.R:14:1
        2.   └─stats::lm.fit(X, Y, ...)
       ── Error ('test_weights.R:19:1'): (code run outside of `test_that()`) ──────────
       Error in `stats::lm.fit(X, Y, ...)`: NA/NaN/Inf in 'x'
       Backtrace:
           ▆
        1. └─feisr::feis(...) at test_weights.R:19:1
        2.   └─stats::lm.fit(X, Y, ...)
       
       [ FAIL 9 | WARN 0 | SKIP 0 | PASS 0 ]
       Error: Test failures
       Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     Error(s) in re-building vignettes:
       ...
     --- re-building ‘feisr-vignette.Rmd’ using rmarkdown
     
     Quitting from feisr-vignette.Rmd:81-84 [feis0]
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     <error/rlang_error>
     Error in `stats::lm.fit()`:
     ! NA/NaN/Inf in 'x'
     ---
     Backtrace:
         ▆
      1. └─feisr::feis(lnw ~ marry | year, data = mwp, id = "id")
      2.   └─stats::lm.fit(X, Y, ...)
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'feisr-vignette.Rmd' failed with diagnostics:
     NA/NaN/Inf in 'x'
     --- failed re-building ‘feisr-vignette.Rmd’
     
     SUMMARY: processing the following file failed:
       ‘feisr-vignette.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

# FLORAL (0.5.0)

* GitHub: <https://github.com/vdblab/FLORAL>
* Email: <mailto:feit1@mskcc.org>
* GitHub mirror: <https://github.com/cran/FLORAL>

Run `revdepcheck::cloud_details(, "FLORAL")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     simu: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# FRK (2.3.1)

* GitHub: <https://github.com/andrewzm/FRK>
* Email: <mailto:andrewzm@gmail.com>
* GitHub mirror: <https://github.com/cran/FRK>

Run `revdepcheck::cloud_details(, "FRK")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     process_isea3h: no visible binding for global variable ‘id’
     show_basis,Basis: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

## In both

*   checking re-building of vignette outputs ... WARNING
     ```
     Error(s) in re-building vignettes:
     --- re-building ‘FRK_intro.Rnw’ using knitr
     ```

# funspotr (0.0.4)

* GitHub: <https://github.com/brshallo/funspotr>
* Email: <mailto:brshallodev@gmail.com>
* GitHub mirror: <https://github.com/cran/funspotr>

Run `revdepcheck::cloud_details(, "funspotr")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     network_plot: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# furniture (1.9.14)

* Email: <mailto:t.barrett88@gmail.com>
* GitHub mirror: <https://github.com/cran/furniture>

Run `revdepcheck::cloud_details(, "furniture")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
       
       
       
       
       [ FAIL 1 | WARN 0 | SKIP 0 | PASS 62 ]
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Error ('test_widelong.R:30:3'): long and wide ───────────────────────────────
       <lifecycle_error_deprecated/defunctError/rlang_error/error/condition>
       Error: `as.tbl()` was deprecated in dplyr 1.0.0 and is now defunct.
       ℹ Please use `tibble::as_tibble()` instead.
       Backtrace:
           ▆
        1. ├─testthat::expect_s3_class(...) at test_widelong.R:30:3
        2. │ └─testthat::quasi_label(enquo(object), arg = "object")
        3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
        4. ├─furniture::long(...)
        5. └─dplyr::as.tbl(df)
        6.   └─lifecycle::deprecate_stop("1.0.0", "as.tbl()", "tibble::as_tibble()")
        7.     └─lifecycle:::deprecate_stop0(msg)
        8.       └─rlang::cnd_signal(...)
       
       [ FAIL 1 | WARN 0 | SKIP 0 | PASS 62 ]
       Error: Test failures
       Execution halted
     ```

# fuzzyjoin (0.1.6.1)

* GitHub: <https://github.com/dgrtwo/fuzzyjoin>
* Email: <mailto:admiral.david@gmail.com>
* GitHub mirror: <https://github.com/cran/fuzzyjoin>

Run `revdepcheck::cloud_details(, "fuzzyjoin")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     
     Attaching package: ‘dplyr’
     
     The following objects are masked from ‘package:stats’:
     
         filter, lag
     
     The following objects are masked from ‘package:base’:
     
         intersect, setdiff, setequal, union
     
     > library(ggplot2)
     > data(diamonds)
     > 
     > diamonds <- tbl_df(diamonds)
     Error:
     ! `tbl_df()` was deprecated in dplyr 1.0.0 and is now defunct.
     ℹ Please use `tibble::as_tibble()` instead.
     Backtrace:
         ▆
      1. └─dplyr::tbl_df(diamonds)
      2.   └─lifecycle::deprecate_stop("1.0.0", "tbl_df()", "tibble::as_tibble()")
      3.     └─lifecycle:::deprecate_stop0(msg)
      4.       └─rlang::cnd_signal(...)
     Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     Error(s) in re-building vignettes:
       ...
     --- re-building ‘stringdist_join.Rmd’ using rmarkdown
     
     Quitting from stringdist_join.Rmd:29-36 [words]
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     NULL
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'stringdist_join.Rmd' failed with diagnostics:
     `tbl_df()` was deprecated in dplyr 1.0.0 and is now defunct.
     ℹ Please use `tibble::as_tibble()` instead.
     --- failed re-building ‘stringdist_join.Rmd’
     
     SUMMARY: processing the following file failed:
       ‘stringdist_join.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

# gaiah (0.0.5)

* Email: <mailto:eric.anderson@noaa.gov>
* GitHub mirror: <https://github.com/cran/gaiah>

Run `revdepcheck::cloud_details(, "gaiah")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     ℹ Please use `tibble::as_tibble()` instead.
     Backtrace:
          ▆
       1. ├─gaiah::comboize_and_fortify(mgen[[1]], miso[[1]], mhab)
       2. │ ├─... %>% dplyr::bind_rows(.id = "genetics_beta")
       3. │ └─base::lapply(...)
       4. │   └─gaiah (local) FUN(X[[i]], ...)
       5. │     ├─... %>% dplyr::bind_rows(.id = "isotope_beta")
       6. │     └─base::lapply(...)
       7. │       └─gaiah (local) FUN(X[[i]], ...)
       8. │         ├─... %>% dplyr::bind_rows(.id = "habitat_beta")
       9. │         └─base::lapply(...)
      10. │           └─gaiah (local) FUN(X[[i]], ...)
      11. │             └─... %>% dplyr::tbl_df()
      12. ├─dplyr::bind_rows(., .id = "genetics_beta")
      13. │ └─rlang::list2(...)
      14. ├─dplyr::bind_rows(., .id = "isotope_beta")
      15. │ └─rlang::list2(...)
      16. ├─dplyr::bind_rows(., .id = "habitat_beta")
      17. │ └─rlang::list2(...)
      18. └─dplyr::tbl_df(.)
      19.   └─lifecycle::deprecate_stop("1.0.0", "tbl_df()", "tibble::as_tibble()")
      20.     └─lifecycle:::deprecate_stop0(msg)
      21.       └─rlang::cnd_signal(...)
     Execution halted
     ```

## In both

*   checking Rd files ... NOTE
     ```
     checkRd: (-1) min_hpd_inc_area_df.Rd:20: Lost braces in \itemize; \value handles \item{}{} directly
     checkRd: (-1) min_hpd_inc_area_df.Rd:21-22: Lost braces in \itemize; \value handles \item{}{} directly
     checkRd: (-1) min_hpd_inc_area_df.Rd:23: Lost braces in \itemize; \value handles \item{}{} directly
     checkRd: (-1) vza_mean_and_var_rasters.Rd:20: Lost braces
         20 | of the paper on Wilson's warbler, this function computes $tilde{T}^{(mu)}$ (returned as
            |                                                                ^
     checkRd: (-1) vza_mean_and_var_rasters.Rd:20: Lost braces; missing escapes or markup?
         20 | of the paper on Wilson's warbler, this function computes $tilde{T}^{(mu)}$ (returned as
            |                                                                    ^
     checkRd: (-1) vza_mean_and_var_rasters.Rd:21: Lost braces; missing escapes or markup?
         21 | list component \code{mean.raster}) and $R^{(sigma^2)}$ (returned as list component
            |                                           ^
     ```

# galaxias (0.1.1)

* GitHub: <https://github.com/AtlasOfLivingAustralia/galaxias>
* Email: <mailto:martin.westgate@csiro.au>
* GitHub mirror: <https://github.com/cran/galaxias>

Run `revdepcheck::cloud_details(, "galaxias")` for more info

## Newly broken

*   checking re-building of vignette outputs ... ERROR
     ```
     ...
     Caused by error in `case_when()`:
     ! Can't combine <logical> and `.default` <character>.
     --- failed re-building ‘occurrences-example.Rmd’
     
     --- re-building ‘quick_start_guide.Rmd’ using rmarkdown
     
     .............
     
     Quitting from quick_start_guide.Rmd:79-84 [unnamed-chunk-5]
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     NULL
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'quick_start_guide.Rmd' failed with diagnostics:
     ℹ In argument: `missing = case_when(...)`.
     ℹ In group 1: `term_group = "Date/Time"`.
     Caused by error in `case_when()`:
     ! Can't combine <logical> and `.default` <character>.
     --- failed re-building ‘quick_start_guide.Rmd’
     
     SUMMARY: processing the following files failed:
       ‘occurrences-example.Rmd’ ‘quick_start_guide.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

# gap (1.6)

* GitHub: <https://github.com/jinghuazhao/R>
* Email: <mailto:jinghuazhao@hotmail.com>
* GitHub mirror: <https://github.com/cran/gap>

Run `revdepcheck::cloud_details(, "gap")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     qtl2dplotly: no visible binding for global variable ‘id’
     qtl3dplotly: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

## In both

*   checking re-building of vignette outputs ... WARNING
     ```
     Error(s) in re-building vignettes:
     --- re-building ‘gap.Rmd’ using rmarkdown
     Read 16 records
     ```

# gatoRs (1.0.2)

* GitHub: <https://github.com/nataliepatten/gatoRs>
* Email: <mailto:natalienpatten@gmail.com>
* GitHub mirror: <https://github.com/cran/gatoRs>

Run `revdepcheck::cloud_details(, "gatoRs")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     > 
     > df <- gators_download(synonyms.list = c("Galax urceolata", "Galax aphylla"), limit = 10)
     Error in `dplyr::case_when()`:
     ! Can't combine <logical> and `.default` <character>.
     Backtrace:
          ▆
       1. ├─gatoRs::gators_download(...)
       2. │ ├─gatoRs::fix_names(get_idigbio(synonyms.list, limit = limit))
       3. │ │ └─base::NROW(df)
       4. │ └─gatoRs::get_idigbio(synonyms.list, limit = limit)
       5. │   ├─base::suppressWarnings(correct_class(query_idigbio))
       6. │   │ └─base::withCallingHandlers(...)
       7. │   └─gatoRs:::correct_class(query_idigbio)
       8. │     └─dplyr::case_when(df[[scientific.name]] == "" ~ NA, .default = as.character(df[[scientific.name]]))
       9. │       └─vctrs::vec_case_when(...)
      10. └─vctrs (local) `<fn>`()
      11.   └─vctrs::vec_default_ptype2(...)
      12.     ├─base::withRestarts(...)
      13.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
      14.     │   └─base (local) doWithOneRestart(return(expr), restart)
      15.     └─vctrs::stop_incompatible_type(...)
      16.       └─vctrs:::stop_incompatible(...)
      17.         └─vctrs:::stop_vctrs(...)
      18.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
     Execution halted
     ```

# GencoDymo2 (1.0.3)

* GitHub: <https://github.com/monahton/GencoDymo2>
* Email: <mailto:aboualezz.monah@hsr.it>
* GitHub mirror: <https://github.com/cran/GencoDymo2>

Run `revdepcheck::cloud_details(, "GencoDymo2")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     eliminate_redundant_elements: no visible binding for global variable
       ‘id’
     Undefined global functions or variables:
       id
     ```

# ggalluvial (0.12.5)

* GitHub: <https://github.com/corybrunson/ggalluvial>
* Email: <mailto:cornelioid@gmail.com>
* GitHub mirror: <https://github.com/cran/ggalluvial>

Run `revdepcheck::cloud_details(, "ggalluvial")` for more info

## Newly broken

*   checking dependencies in R code ... WARNING
     ```
     Missing or unexported object: ‘dplyr::select_vars’
     ```

# ggbrick (0.3.1)

* Email: <mailto:danieloehm@gmail.com>
* GitHub mirror: <https://github.com/cran/ggbrick>

Run `revdepcheck::cloud_details(, "ggbrick")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     robust_random: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# ggFishPlots (0.3.0)

* GitHub: <https://github.com/DeepWaterIMR/ggFishPlots>
* Email: <mailto:mikko.vihtakari@hi.no>
* GitHub mirror: <https://github.com/cran/ggFishPlots>

Run `revdepcheck::cloud_details(, "ggFishPlots")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     plot_growth: no visible binding for global variable ‘id’
     plot_lw: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# gglm (1.0.4)

* GitHub: <https://github.com/graysonwhite/gglm>
* Email: <mailto:graysonwhite13@gmail.com>
* GitHub mirror: <https://github.com/cran/gglm>

Run `revdepcheck::cloud_details(, "gglm")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     fortify.mlogit: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# ggmulti (1.0.8)

* Email: <mailto:z267xu@gmail.com>
* GitHub mirror: <https://github.com/cran/ggmulti>

Run `revdepcheck::cloud_details(, "ggmulti")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     collide_ : <anonymous>: no visible binding for global variable
       ‘location’
     compute_scales.histogram: no visible binding for global variable
       ‘location’
     Undefined global functions or variables:
       location
     ```

# ghypernet (1.1.0)

* Email: <mailto:giona@ethz.ch>
* GitHub mirror: <https://github.com/cran/ghypernet>

Run `revdepcheck::cloud_details(, "ghypernet")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     .bccm: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

## In both

*   checking dependencies in R code ... NOTE
     ```
     Namespace in Imports field not imported from: ‘methods’
       All declared Imports should be used.
     ```

# gimap (1.1.1)

* GitHub: <https://github.com/FredHutch/gimap>
* Email: <mailto:cansav09@gmail.com>
* GitHub mirror: <https://github.com/cran/gimap>

Run `revdepcheck::cloud_details(, "gimap")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     get_figshare: no visible binding for global variable ‘id’
     gimap_filter: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# gitgadget (0.8.2)

* GitHub: <https://github.com/vnijs/gitgadget>
* Email: <mailto:vnijs@ucsd.edu>
* GitHub mirror: <https://github.com/cran/gitgadget>

Run `revdepcheck::cloud_details(, "gitgadget")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     fetch_work: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# gmgm (1.1.2)

* Email: <mailto:jeremy.roos@gmail.com>
* GitHub mirror: <https://github.com/cran/gmgm>

Run `revdepcheck::cloud_details(, "gmgm")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     network: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# gMOIP (1.5.5)

* GitHub: <https://github.com/relund/gMOIP>
* Email: <mailto:lars@relund.dk>
* GitHub mirror: <https://github.com/cran/gMOIP>

Run `revdepcheck::cloud_details(, "gMOIP")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     addNDSet: no visible binding for global variable ‘id’
     classifyNDSet: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# goat (1.1.3)

* GitHub: <https://github.com/ftwkoopmans/goat>
* Email: <mailto:ftwkoopmans@gmail.com>
* GitHub mirror: <https://github.com/cran/goat>

Run `revdepcheck::cloud_details(, "goat")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     build_treemap: no visible binding for global variable ‘id’
     go_gene2go: no visible binding for global variable ‘id’
     load_genesets_gmtfile: no visible binding for global variable ‘id’
     load_genesets_go_bioconductor: no visible binding for global variable
       ‘id’
     load_genesets_go_fromfile: no visible binding for global variable ‘id’
     load_genesets_syngo: no visible binding for global variable ‘id’
     ontology_data_structures: no visible binding for global variable ‘id’
     plot_lollipop: no visible binding for global variable ‘id’
     reduce_genesets: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# googletraffic (0.1.7)

* GitHub: <https://github.com/dime-worldbank/googletraffic>
* Email: <mailto:rmarty@worldbank.org>
* GitHub mirror: <https://github.com/cran/googletraffic>

Run `revdepcheck::cloud_details(, "googletraffic")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     gt_make_grid: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

## In both

*   checking DESCRIPTION meta-information ... NOTE
     ```
       Missing dependency on R >= 4.1.0 because package code uses the pipe
       |> or function shorthand \(...) syntax added in R 4.1.0.
       File(s) using such syntax:
         ‘gt_make_grid.Rd’
     ```

# GOxploreR (1.2.8)

* Email: <mailto:kalifamanjang1@gmail.com>
* GitHub mirror: <https://github.com/cran/GOxploreR>

Run `revdepcheck::cloud_details(, "GOxploreR")` for more info

## Newly broken

*   checking whether package ‘GOxploreR’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/tmp/workdir/GOxploreR/new/GOxploreR.Rcheck/00install.out’ for details.
     ```

## Installation

### Devel

```
* installing *source* package ‘GOxploreR’ ...
** this is package ‘GOxploreR’ version ‘1.2.8’
** package ‘GOxploreR’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘id’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘GOxploreR’
* removing ‘/tmp/workdir/GOxploreR/new/GOxploreR.Rcheck/GOxploreR’


```
### CRAN

```
* installing *source* package ‘GOxploreR’ ...
** this is package ‘GOxploreR’ version ‘1.2.8’
** package ‘GOxploreR’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (GOxploreR)


```
# growthcleanr (2.2.0)

* GitHub: <https://github.com/carriedaymont/growthcleanr>
* Email: <mailto:cdaymont@pennstatehealth.psu.edu>
* GitHub mirror: <https://github.com/cran/growthcleanr>

Run `revdepcheck::cloud_details(, "growthcleanr")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     cleanadult: no visible binding for global variable ‘id’
     cleangrowth: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# grPipe (0.1.0)

* Email: <mailto:daniel.gaspar.goncalves@gmail.com>
* GitHub mirror: <https://github.com/cran/grPipe>

Run `revdepcheck::cloud_details(, "grPipe")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     grPipe.plot: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# heuristicsmineR (0.3.0)

* GitHub: <https://github.com/bupaverse/heuristicsmineR>
* Email: <mailto:f.mannhardt@tue.nl>
* GitHub mirror: <https://github.com/cran/heuristicsmineR>

Run `revdepcheck::cloud_details(, "heuristicsmineR")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     as.petrinet.causal_net: no visible binding for global variable ‘id’
     causal_custom : <anonymous>: no visible binding for global variable
       ‘id’
     causal_frequency : <anonymous>: no visible binding for global variable
       ‘id’
     causal_performance : <anonymous>: no visible binding for global
       variable ‘id’
     render_dependency_matrix: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# himach (1.0.0)

* GitHub: <https://github.com/david6marsh/himach>
* Email: <mailto:david6marsh@gmail.com>
* GitHub mirror: <https://github.com/cran/himach>

Run `revdepcheck::cloud_details(, "himach")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     pathToGC: no visible binding for global variable ‘id’
     smoothSpeed: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# hurricaneexposure (0.1.1)

* GitHub: <https://github.com/geanders/hurricaneexposure>
* Email: <mailto:brooke.anderson@colostate.edu>
* GitHub mirror: <https://github.com/cran/hurricaneexposure>

Run `revdepcheck::cloud_details(, "hurricaneexposure")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     > if (requireNamespace("hurricaneexposuredata", quietly = TRUE)) {
     + 
     + map_counties("Floyd-1999", metric = "rainfall", days_included = c(-2, -1, 0, 1))
     + 
     + map_counties("Katrina-2005", metric = "wind")
     + map_counties("Katrina-2005", metric = "wind", wind_var = "vmax_gust")
     + map_counties("Katrina-2005", metric = "wind", wind_var = "sust_dur")
     + map_counties("Katrina-2005", metric = "wind", wind_source = "ext_tracks")
     + 
     + #' map_counties("Michael-2018", metric = "wind")
     + map_counties("Michael-2018", metric = "wind", wind_var = "vmax_gust")
     + map_counties("Michael-2018", metric = "wind", wind_source = "ext_tracks")
     + }
     Error:
     ! `tbl_df()` was deprecated in dplyr 1.0.0 and is now defunct.
     ℹ Please use `tibble::as_tibble()` instead.
     Backtrace:
         ▆
      1. ├─hurricaneexposure::map_counties(...)
      2. │ └─map_data %>% dplyr::tbl_df()
      3. └─dplyr::tbl_df(.)
      4.   └─lifecycle::deprecate_stop("1.0.0", "tbl_df()", "tibble::as_tibble()")
      5.     └─lifecycle:::deprecate_stop0(msg)
      6.       └─rlang::cnd_signal(...)
     Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     Error(s) in re-building vignettes:
       ...
     --- re-building ‘hurricaneexposure.Rmd’ using rmarkdown
     
     Quitting from hurricaneexposure.Rmd:116-118 [unnamed-chunk-6]
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     NULL
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'hurricaneexposure.Rmd' failed with diagnostics:
     `tbl_df()` was deprecated in dplyr 1.0.0 and is now defunct.
     ℹ Please use `tibble::as_tibble()` instead.
     --- failed re-building ‘hurricaneexposure.Rmd’
     
     SUMMARY: processing the following file failed:
       ‘hurricaneexposure.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

## In both

*   checking dependencies in R code ... NOTE
     ```
     Namespace in Imports field not imported from: ‘mapproj’
       All declared Imports should be used.
     ```

*   checking Rd files ... NOTE
     ```
     ...
     checkRd: (-1) county_rain.Rd:52-54: Lost braces in \itemize; \value handles \item{}{} directly
     checkRd: (-1) county_rain.Rd:55-56: Lost braces in \itemize; \value handles \item{}{} directly
     checkRd: (-1) county_rain.Rd:57-59: Lost braces in \itemize; \value handles \item{}{} directly
     checkRd: (-1) county_rain.Rd:60-62: Lost braces in \itemize; \value handles \item{}{} directly
     checkRd: (-1) county_rain.Rd:63-66: Lost braces in \itemize; \value handles \item{}{} directly
     checkRd: (-1) county_wind.Rd:55-57: Lost braces in \itemize; \value handles \item{}{} directly
     checkRd: (-1) county_wind.Rd:58-59: Lost braces in \itemize; \value handles \item{}{} directly
     checkRd: (-1) county_wind.Rd:60: Lost braces in \itemize; \value handles \item{}{} directly
     checkRd: (-1) county_wind.Rd:61: Lost braces in \itemize; \value handles \item{}{} directly
     checkRd: (-1) county_wind.Rd:62: Lost braces in \itemize; \value handles \item{}{} directly
     checkRd: (-1) county_wind.Rd:63-64: Lost braces in \itemize; \value handles \item{}{} directly
     checkRd: (-1) county_wind.Rd:65-66: Lost braces in \itemize; \value handles \item{}{} directly
     checkRd: (-1) county_wind.Rd:67-70: Lost braces in \itemize; \value handles \item{}{} directly
     checkRd: (-1) county_wind.Rd:71-72: Lost braces in \itemize; \value handles \item{}{} directly
     checkRd: (-1) county_wind.Rd:73-75: Lost braces in \itemize; \value handles \item{}{} directly
     checkRd: (-1) county_wind.Rd:76-78: Lost braces in \itemize; \value handles \item{}{} directly
     checkRd: (-1) multi_county_rain.Rd:48: Lost braces in \itemize; \value handles \item{}{} directly
     checkRd: (-1) multi_county_rain.Rd:49-51: Lost braces in \itemize; \value handles \item{}{} directly
     checkRd: (-1) multi_county_rain.Rd:52-54: Lost braces in \itemize; \value handles \item{}{} directly
     checkRd: (-1) multi_county_rain.Rd:55-57: Lost braces in \itemize; \value handles \item{}{} directly
     checkRd: (-1) multi_county_rain.Rd:58-61: Lost braces in \itemize; \value handles \item{}{} directly
     checkRd: (-1) multi_county_rain.Rd:62-65: Lost braces in \itemize; \value handles \item{}{} directly
     checkRd: (-1) multi_county_rain.Rd:66-68: Lost braces in \itemize; \value handles \item{}{} directly
     checkRd: (-1) multi_county_rain.Rd:69-72: Lost braces in \itemize; \value handles \item{}{} directly
     checkRd: (-1) multi_county_rain.Rd:73-76: Lost braces in \itemize; \value handles \item{}{} directly
     ```

# hutils (1.8.1)

* GitHub: <https://github.com/hughparsonage/hutils>
* Email: <mailto:hugh.parsonage@gmail.com>
* GitHub mirror: <https://github.com/cran/hutils>

Run `revdepcheck::cloud_details(, "hutils")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
       [ FAIL 1 | WARN 0 | SKIP 22 | PASS 678 ]
       
       ══ Skipped tests (22) ══════════════════════════════════════════════════════════
       • Not yet considered (1): 'test-ahull.R:69:3'
       • On CRAN (17): 'test-dev_copy2a4.R:11:3',
         'test_1-0-0_generate_LaTeX_manual.R:4:3', 'test_1-0-0_if_else.R:151:3',
         'test_1-2-0_RQ.R:15:3', 'test_benchmarks.R:4:3', 'test_benchmarks.R:23:3',
         'test_benchmarks.R:60:3', 'test_check_pkg_dependencies.R:4:3',
         'test_find_pattern_in.R:10:3', 'test_find_pattern_in.R:30:3',
         'test_find_pattern_in.R:47:3', 'test_find_pattern_in.R:89:3',
         'test_find_pattern_in.R:108:3', 'test_find_pattern_in.R:139:3',
         'test_mutate_ntile.R:90:3', 'test_provide-file.R:2:3',
         'test_provide-file.R:13:3'
       • identical(.Platform$OS.type, "windows") is not TRUE (3): 'test-dir2.R:4:3',
         'test-dir2.R:33:3', 'test-dir2.R:59:3'
       • packageVersion("survey") >= "4.1" is TRUE (1):
         'test_1-3-0_weighted_ntile.R:30:3'
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Failure ('test_1-0-0_if_else.R:145:5'): Must be faster than dplyr::if_else ──
       out[expr == "dplyr"][["time"]] is not strictly more than out[expr == "hutils"][["time"]]. Difference: -2.11e+05
       
       [ FAIL 1 | WARN 0 | SKIP 22 | PASS 678 ]
       Error: Test failures
       Execution halted
     ```

# HVT (25.2.7)

* GitHub: <https://github.com/Mu-Sigma/HVT>
* Email: <mailto:zubin.dowlaty@mu-sigma.com>
* GitHub mirror: <https://github.com/cran/HVT>

Run `revdepcheck::cloud_details(, "HVT")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     getCellId: no visible binding for global variable ‘id’
     plotHVT: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# IAT (0.3)

* Email: <mailto:dpmartin42@gmail.com>
* GitHub mirror: <https://github.com/cran/IAT>

Run `revdepcheck::cloud_details(, "IAT")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     Warning: `filter_()` was deprecated in dplyr 0.7.0.
     ℹ Please use `filter()` instead.
     ℹ See vignette('programming') for more help
     ℹ The deprecated feature was likely used in the IAT package.
       Please report the issue to the authors.
     Warning: `group_by_()` was deprecated in dplyr 0.7.0.
     ℹ Please use `group_by()` instead.
     ℹ See vignette('programming') for more help
     ℹ The deprecated feature was likely used in the IAT package.
       Please report the issue to the authors.
     Error:
     ! `tbl_df()` was deprecated in dplyr 1.0.0 and is now defunct.
     ℹ Please use `tibble::as_tibble()` instead.
     Backtrace:
         ▆
      1. ├─IAT::cleanIAT(...)
      2. │ └─... %>% ...
      3. ├─dplyr::filter_(., interp(~x < 10000 & x >= 0, x = as.name(trial_latency)))
      4. ├─dplyr::mutate(., SUBEXCL = 0)
      5. ├─dplyr::group_by_(., session_id)
      6. └─dplyr::tbl_df(.)
      7.   └─lifecycle::deprecate_stop("1.0.0", "tbl_df()", "tibble::as_tibble()")
      8.     └─lifecycle:::deprecate_stop0(msg)
      9.       └─rlang::cnd_signal(...)
     Execution halted
     ```

## In both

*   checking Rd files ... NOTE
     ```
     checkRd: (-1) IATData.Rd:24: Lost braces; missing escapes or markup?
         24 | Dan Martin {dpmartin42@gmail.com}
            |            ^
     ```

# inspectdf (0.0.12.1)

* GitHub: <https://github.com/alastairrushworth/inspectdf>
* Email: <mailto:alastairmrushworth@gmail.com>
* GitHub mirror: <https://github.com/cran/inspectdf>

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

# inti (0.6.9)

* GitHub: <https://github.com/flavjack/inti>
* Email: <mailto:flozanoisla@gmail.com>
* GitHub mirror: <https://github.com/cran/inti>

Run `revdepcheck::cloud_details(, "inti")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     figure2rmd: no visible binding for global variable ‘id’
     table2rmd: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# iNZightTS (2.0.2)

* GitHub: <https://github.com/iNZightVIT/iNZightTS>
* Email: <mailto:tom.elliott@auckland.ac.nz>
* GitHub mirror: <https://github.com/cran/iNZightTS>

Run `revdepcheck::cloud_details(, "iNZightTS")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
       Predict variable not specified, automatically selected `var = "Australia"`
       Plot variable not specified, automatically selected `var = "Daily_Deaths"`
       [ FAIL 8 | WARN 123 | SKIP 0 | PASS 117 ]
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Failure ('test_plots.R:22:5'): Decomposition and recomposition plots work ───
       `d <- decomp(t, var = "Australia")` produced warnings.
       ── Failure ('test_submodel.R:8:5'): Decomposition uses sub-portion of the plot ──
       `d <- decomp(x, "Japan", model_range = c(2005, 2009))` produced warnings.
       ── Failure ('test_submodel.R:13:5'): Season plots use sub-set of data ──────────
       `seasonplot(x, "Japan")` produced warnings.
       ── Failure ('test_submodel.R:14:5'): Season plots use sub-set of data ──────────
       `seasonplot(x, "Japan", model_range = c(2000, 2010))` produced warnings.
       ── Failure ('test_submodel.R:15:5'): Season plots use sub-set of data ──────────
       `seasonplot(x, "Japan", model_range = c(2005, 2011))` produced warnings.
       ── Failure ('test_subset.R:8:5'): Subset of time series can be viewed ──────────
       `p <- plot(t, "Australia", xlim = c(2000, 2011))` produced warnings.
       ── Failure ('test_subset.R:9:5'): Subset of time series can be viewed ──────────
       `p1 <- plot(t, "Australia", xlim = c(2000, NA))` produced warnings.
       ── Failure ('test_subset.R:10:5'): Subset of time series can be viewed ─────────
       `p2 <- plot(t, "Australia", xlim = c(NA, 2005))` produced warnings.
       
       [ FAIL 8 | WARN 123 | SKIP 0 | PASS 117 ]
       Error: Test failures
       Execution halted
     ```

# ipft (0.7.2)

* Email: <mailto:esansano@uji.es>
* GitHub mirror: <https://github.com/cran/ipft>

Run `revdepcheck::cloud_details(, "ipft")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     Running examples in ‘ipft-Ex.R’ failed
     The error most likely occurred in:
     
     > ### Name: ipfGroup
     > ### Title: Creates groups based on the specified parameters
     > ### Aliases: ipfGroup
     > 
     > ### ** Examples
     > 
     > 
     >     group <- ipfGroup(mtcars, cyl)
     Error:
     ! The `...` argument of `group_indices()` was deprecated in dplyr 1.0.0
       and is now defunct.
     ℹ Please `group_by()` first
     Backtrace:
         ▆
      1. └─ipft::ipfGroup(mtcars, cyl)
      2.   ├─dplyr::group_indices(data, ...)
      3.   └─dplyr:::group_indices.data.frame(data, ...)
      4.     └─lifecycle::deprecate_stop("1.0.0", "group_indices(... = )", details = "Please `group_by()` first")
      5.       └─lifecycle:::deprecate_stop0(msg)
      6.         └─rlang::cnd_signal(...)
     Execution halted
     ```

# istat (1.0)

* Email: <mailto:elenaagradi@gmail.com>
* GitHub mirror: <https://github.com/cran/istat>

Run `revdepcheck::cloud_details(, "istat")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     list_istatdata: no visible binding for global variable ‘id’
     shinyIstat : server : list_istatdata: no visible binding for global
       variable ‘id’
     Undefined global functions or variables:
       id
     ```

# IVPP (1.1.1)

* GitHub: <https://github.com/xinkaidupsy/IVPP>
* Email: <mailto:xinkai.du.xd@gmail.com>
* GitHub mirror: <https://github.com/cran/IVPP>

Run `revdepcheck::cloud_details(, "IVPP")` for more info

## Newly broken

*   checking whether package ‘IVPP’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/tmp/workdir/IVPP/new/IVPP.Rcheck/00install.out’ for details.
     ```

## Installation

### Devel

```
* installing *source* package ‘IVPP’ ...
** this is package ‘IVPP’ version ‘1.1.1’
** package ‘IVPP’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error: object ‘id’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘IVPP’
* removing ‘/tmp/workdir/IVPP/new/IVPP.Rcheck/IVPP’


```
### CRAN

```
* installing *source* package ‘IVPP’ ...
** this is package ‘IVPP’ version ‘1.1.1’
** package ‘IVPP’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (IVPP)


```
# kerntools (1.2.0)

* GitHub: <https://github.com/elies-ramon/kerntools>
* Email: <mailto:eramon@everlyrusher.com>
* GitHub mirror: <https://github.com/cran/kerntools>

Run `revdepcheck::cloud_details(, "kerntools")` for more info

## Newly broken

*   checking whether package ‘kerntools’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/tmp/workdir/kerntools/new/kerntools.Rcheck/00install.out’ for details.
     ```

## Installation

### Devel

```
* installing *source* package ‘kerntools’ ...
** this is package ‘kerntools’ version ‘1.2.0’
** package ‘kerntools’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘id’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘kerntools’
* removing ‘/tmp/workdir/kerntools/new/kerntools.Rcheck/kerntools’


```
### CRAN

```
* installing *source* package ‘kerntools’ ...
** this is package ‘kerntools’ version ‘1.2.0’
** package ‘kerntools’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (kerntools)


```
# khisr (1.0.6)

* GitHub: <https://github.com/damurka/khisr>
* Email: <mailto:hello@damurka.com>
* GitHub mirror: <https://github.com/cran/khisr>

Run `revdepcheck::cloud_details(, "khisr")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     get_data_elements_with_category_options: no visible binding for global
       variable ‘id’
     get_data_sets_by_level: no visible binding for global variable ‘id’
     get_organisations_by_level: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# kollaR (1.1.2)

* Email: <mailto:johan.lundin.kleberg@su.se>
* GitHub mirror: <https://github.com/cran/kollaR>

Run `revdepcheck::cloud_details(, "kollaR")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     static_plot: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# LightLogR (0.9.2)

* GitHub: <https://github.com/tscnlab/LightLogR>
* Email: <mailto:johannes.zauner@tum.de>
* GitHub mirror: <https://github.com/cran/LightLogR>

Run `revdepcheck::cloud_details(, "LightLogR")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     ℹ In group 1: `Id = Environment`.
     Caused by error in `dplyr::case_when()`:
     ! Can't combine <logical> and `.default` <datetime<Europe/Berlin>>.
     Backtrace:
          ▆
       1. ├─LightLogR::gg_photoperiod(gg_days(sample.data.environment), coordinates)
       2. │ ├─dplyr::mutate(...)
       3. │ └─dplyr:::mutate.data.frame(...)
       4. │   └─dplyr:::mutate_cols(.data, dplyr_quosures(...), by)
       5. │     ├─base::withCallingHandlers(...)
       6. │     └─dplyr:::mutate_col(dots[[i]], data, mask, new_columns)
       7. │       └─mask$eval_all_mutate(quo)
       8. │         └─dplyr (local) eval()
       9. ├─dplyr::case_when(min(date.grouper) > lubridate::date(dawn) ~ NA, .default = dawn)
      10. │ └─vctrs::vec_case_when(...)
      11. └─vctrs (local) `<fn>`()
      12.   └─vctrs::vec_default_ptype2(...)
      13.     ├─base::withRestarts(...)
      14.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
      15.     │   └─base (local) doWithOneRestart(return(expr), restart)
      16.     └─vctrs::stop_incompatible_type(...)
      17.       └─vctrs:::stop_incompatible(...)
      18.         └─vctrs:::stop_vctrs(...)
      19.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
       > test_check("LightLogR")
       [ FAIL 1 | WARN 0 | SKIP 3 | PASS 876 ]
       
       ══ Skipped tests (3) ═══════════════════════════════════════════════════════════
       • On CRAN (3): 'test-gg_overview.R:3:3', 'test-import_States.R:11:3',
         'test-sc2interval.R:6:3'
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Error ('test-photoperiod.R:87:3'): gg_photoperiod works ─────────────────────
       <dplyr:::mutate_error/rlang_error/error/condition>
       Error in `dplyr::mutate(dplyr::mutate(photoperiod_data, midnight.before = dplyr::case_when(!is.na(dplyr::lag(dawn)) ~ 
           dawn, .default = lubridate::floor_date(dawn, "day")), midnight.after = dplyr::case_when(!is.na(dplyr::lead(dawn)) ~ 
           dplyr::lead(dawn), .default = lubridate::ceiling_date(date.grouper, 
           "day"))), dawn = dplyr::case_when(min(date.grouper) > lubridate::date(dawn) ~ 
           NA, .default = dawn), dusk = dplyr::case_when(max(date.grouper) < 
           lubridate::date(dusk) ~ NA, .default = dusk), midnight.before = dplyr::case_when(min(date.grouper) > 
           lubridate::date(midnight.before) ~ NA, .default = midnight.before), 
           )`: i In argument: `dawn = dplyr::case_when(...)`.
       i In group 1: `Id = Environment`.
       Caused by error in `dplyr::case_when()`:
       ! Can't combine <logical> and `.default` <datetime<Europe/Berlin>>.
       
       [ FAIL 1 | WARN 0 | SKIP 3 | PASS 876 ]
       Error: Test failures
       Execution halted
     ```

# manifestoR (1.6.1)

* Email: <mailto:pola.lehmann@wzb.eu>
* GitHub mirror: <https://github.com/cran/manifestoR>

Run `revdepcheck::cloud_details(, "manifestoR")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     mp_rmps: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# mantis (1.0.0)

* GitHub: <https://github.com/phuongquan/mantis>
* Email: <mailto:phuongquan567@outlook.com>
* GitHub mirror: <https://github.com/cran/mantis>

Run `revdepcheck::cloud_details(, "mantis")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
         i In group 1: `item.item = "na"`.
         Caused by warning:
         ! Calling `case_when()` with size 1 LHS inputs and size >1 RHS inputs was deprecated in dplyr 1.2.0.
         i This `case_when()` statement can result in subtle silent bugs and is very inefficient.
         
           Please use a series of if statements instead:
         
           ```
           # Previously
           case_when(scalar_lhs1 ~ rhs1, scalar_lhs2 ~ rhs2, .default = default)
         
           # Now
           if (scalar_lhs1) {
             rhs1
           } else if (scalar_lhs2) {
             rhs2
           } else {
             default
           }
           ```
         i Run `dplyr::last_dplyr_warnings()` to see the 1 remaining warning.
       
       [ FAIL 3 | WARN 3 | SKIP 3 | PASS 483 ]
       Error: Test failures
       Execution halted
     ```

# manydata (1.1.3)

* GitHub: <https://github.com/globalgov/manydata>
* Email: <mailto:james.hollway@graduateinstitute.ch>
* GitHub mirror: <https://github.com/cran/manydata>

Run `revdepcheck::cloud_details(, "manydata")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
        14. ├─get(funny)(out, all_of(vars))
        15. │ └─... %>% dplyr::pull(var = -1)
        16. ├─dplyr::pull(., var = -1)
        17. ├─dplyr::mutate(., dplyr::coalesce(!!!as.data.frame(toCoal)))
        18. ├─dplyr:::mutate.data.frame(., dplyr::coalesce(!!!as.data.frame(toCoal)))
        19. │ └─dplyr:::mutate_cols(.data, dplyr_quosures(...), by)
        20. │   ├─base::withCallingHandlers(...)
        21. │   └─dplyr:::mutate_col(dots[[i]], data, mask, new_columns)
        22. │     └─mask$eval_all_mutate(quo)
        23. │       └─dplyr (local) eval()
        24. ├─dplyr::coalesce(date.x = `<mdate>`, date.y = `<mdate>`, date = `<mdate>`)
        25. │ └─vctrs::vec_case_when(...)
        26. ├─rlang::abort(message = message)
        27. │ └─rlang:::signal_abort(cnd, .file)
        28. │   └─base::signalCondition(cnd)
        29. ├─dplyr (local) `<fn>`(`<rlng_rrr>`)
        30. │ └─rlang::abort(message, class = error_class, parent = parent, call = error_call)
        31. │   └─rlang:::signal_abort(cnd, .file)
        32. │     └─base::signalCondition(cnd)
        33. └─dplyr (local) `<fn>`(`<dply:::_>`)
        34.   └─rlang::abort(message, class = error_class, parent = parent, call = error_call)
       
       [ FAIL 1 | WARN 0 | SKIP 3 | PASS 112 ]
       Error: Test failures
       Execution halted
     ```

# mapboxapi (0.6.2)

* GitHub: <https://github.com/walkerke/mapboxapi>
* Email: <mailto:kyle@walker-data.com>
* GitHub mirror: <https://github.com/cran/mapboxapi>

Run `revdepcheck::cloud_details(, "mapboxapi")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     mb_optimized_route: no visible binding for global variable ‘location’
     Undefined global functions or variables:
       location
     ```

# markets (1.1.6)

* GitHub: <https://github.com/pi-kappa-devel/markets>
* Email: <mailto:pikappa.devel@gmail.com>
* GitHub mirror: <https://github.com/cran/markets>

Run `revdepcheck::cloud_details(, "markets")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     simulate_quantities_and_prices,simulated_model: no visible binding for
       global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# matman (1.1.3)

* Email: <mailto:leon.binder@th-deg.de>
* GitHub mirror: <https://github.com/cran/matman>

Run `revdepcheck::cloud_details(, "matman")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     > # ABC Analysis
     > data("Amount")
     > abcResult = computeABCXYZAnalysis(data = Amount,
     +     value = "value",
     +     item = "item",
     +     timestamp = "date")
     > summary(abcResult)
     Error:
     ! The `.dots` argument of `group_by()` was deprecated in dplyr 1.0.0 and
       is now defunct.
     Backtrace:
          ▆
       1. ├─base::summary(abcResult)
       2. ├─matman::summary(abcResult)
       3. │ └─matman (local) .local(object, ...)
       4. │   └─... %>% as.data.table
       5. ├─data.table::as.data.table(.)
       6. ├─dplyr::summarize(...)
       7. ├─dplyr::group_by(., .dots = "class")
       8. └─dplyr:::group_by.data.frame(., .dots = "class")
       9.   └─dplyr::group_by_prepare(.data, ..., .add = .add, error_call = current_env())
      10.     └─lifecycle::deprecate_stop("1.0.0", "group_by(.dots = )")
      11.       └─lifecycle:::deprecate_stop0(msg)
      12.         └─rlang::cnd_signal(...)
     Execution halted
     ```

# mdsr (0.2.8)

* GitHub: <https://github.com/mdsr-book/mdsr>
* Email: <mailto:ben.baumer@gmail.com>
* GitHub mirror: <https://github.com/cran/mdsr>

Run `revdepcheck::cloud_details(, "mdsr")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     mdsr_sql_explain_table: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# merTools (0.6.3)

* GitHub: <https://github.com/jknowles/merTools>
* Email: <mailto:jared@civilytics.com>
* GitHub mirror: <https://github.com/cran/merTools>

Run `revdepcheck::cloud_details(, "merTools")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     Running examples in ‘merTools-Ex.R’ failed
     The error most likely occurred in:
     
     > ### Name: REsim
     > ### Title: Simulate random effects from merMod 'REsim' simulates random
     > ###   effects from merMod object posterior distributions
     > ### Aliases: REsim
     > 
     > ### ** Examples
     > 
     > require(lme4)
     > m2 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
     > re2 <- REsim(m2, 25)
     Error in `$<-.data.frame`(`*tmp*`, "X1", value = c("308", "309", "310",  : 
       replacement has 36 rows, data has 18
     Calls: REsim -> $<- -> $<-.data.frame
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
       • On CRAN (12): 'test-helpers.R:6:3', 'test-helpers.R:23:3',
         'test-helpers.R:37:3', 'test-helpers.R:72:3', 'test-helpers.R:90:3',
         'test-helpers.R:124:3', 'test-expectedRank.R:12:3',
         'test-expectedRank.R:83:3', 'test-merModList.R:10:3',
         'test-merModList.R:46:3', 'test-merModList.R:72:3', 'test-merModList.R:128:3'
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Error ('test-merExtract.R:88:3'): REsim produces data.frames ────────────────
       Error in ``$<-.data.frame`(`*tmp*`, "X1", value = c("308", "309", "310", 
       "330", "331", "332", "333", "334", "335", "337", "349", "350", 
       "351", "352", "369", "370", "371", "372", "308", "309", "310", 
       "330", "331", "332", "333", "334", "335", "337", "349", "350", 
       "351", "352", "369", "370", "371", "372"))`: replacement has 36 rows, data has 18
       Backtrace:
           ▆
        1. ├─testthat::expect_s3_class(REsim(lmerSlope1, n.sims = 100), "data.frame") at test-merExtract.R:88:3
        2. │ └─testthat::quasi_label(enquo(object), arg = "object")
        3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
        4. └─merTools::REsim(lmerSlope1, n.sims = 100)
        5.   ├─base::`$<-`(`*tmp*`, "X1", value = `<chr>`)
        6.   └─base::`$<-.data.frame`(`*tmp*`, "X1", value = `<chr>`)
       
       [ FAIL 1 | WARN 0 | SKIP 12 | PASS 234 ]
       Error: Test failures
       Execution halted
     ```

# metamicrobiomeR (1.2)

* GitHub: <https://github.com/nhanhocu/metamicrobiomeR>
* Email: <mailto:nhanhocumc@gmail.com>
* GitHub mirror: <https://github.com/cran/metamicrobiomeR>

Run `revdepcheck::cloud_details(, "metamicrobiomeR")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     metatab.show: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

## In both

*   checking dependencies in R code ... NOTE
     ```
     Namespace in Imports field not imported from: ‘lmerTest’
       All declared Imports should be used.
     ```

# microhaplot (1.0.1)

* GitHub: <https://github.com/ngthomas/microhaplot>
* Email: <mailto:tngthomasng@gmail.com>
* GitHub mirror: <https://github.com/cran/microhaplot>

Run `revdepcheck::cloud_details(, "microhaplot")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     > perl.version <- as.numeric(system('perl -e "print $];"', intern=TRUE))
     > 
     > if (perl.version >= 5.014) {
     + haplo.read.tbl <- prepHaplotFiles(run.label = run.label,
     +                             sam.path = sam.path,
     +                             out.path = tempdir(),
     +                             label.path = label.path,
     +                             vcf.path = vcf.path,
     +                             app.path = app.path)
     + }else {
     + message("Perl version is outdated. Must >= 5.014.")}
     Perl is found in system
     ...running Hapture.pl to extract haplotype information (takes a while)...
     Error:
     ! `tbl_df()` was deprecated in dplyr 1.0.0 and is now defunct.
     ℹ Please use `tibble::as_tibble()` instead.
     Backtrace:
         ▆
      1. ├─microhaplot::prepHaplotFiles(...)
      2. │ └─... %>% dplyr::tbl_df()
      3. └─dplyr::tbl_df(.)
      4.   └─lifecycle::deprecate_stop("1.0.0", "tbl_df()", "tibble::as_tibble()")
      5.     └─lifecycle:::deprecate_stop0(msg)
      6.       └─rlang::cnd_signal(...)
     Execution halted
     ```

## In both

*   checking dependencies in R code ... NOTE
     ```
     Namespaces in Imports field not imported from:
       ‘DT’ ‘ggiraph’ ‘ggplot2’ ‘grid’ ‘gtools’ ‘scales’ ‘shinyBS’
       ‘shinyWidgets’ ‘tidyr’
       All declared Imports should be used.
     ```

# mlVAR (0.5.2)

* Email: <mailto:mail@sachaepskamp.com>
* GitHub mirror: <https://github.com/cran/mlVAR>

Run `revdepcheck::cloud_details(, "mlVAR")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     movingWindow: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# mobr (3.0.0)

* GitHub: <https://github.com/MoBiodiv/mobr>
* Email: <mailto:danmcglinn@gmail.com>
* GitHub mirror: <https://github.com/cran/mobr>

Run `revdepcheck::cloud_details(, "mobr")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     plot_rarefaction: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# MonteCarlo (1.0.6)

* GitHub: <https://github.com/FunWithR/MonteCarlo>
* Email: <mailto:christian_leschinski@gmx.de>
* GitHub mirror: <https://github.com/cran/MonteCarlo>

Run `revdepcheck::cloud_details(, "MonteCarlo")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     > 
     > library(dplyr)
     
     Attaching package: ‘dplyr’
     
     The following objects are masked from ‘package:stats’:
     
         filter, lag
     
     The following objects are masked from ‘package:base’:
     
         intersect, setdiff, setequal, union
     
     > library(ggplot2)
     > tbl <- tbl_df(df)
     Error:
     ! `tbl_df()` was deprecated in dplyr 1.0.0 and is now defunct.
     ℹ Please use `tibble::as_tibble()` instead.
     Backtrace:
         ▆
      1. └─dplyr::tbl_df(df)
      2.   └─lifecycle::deprecate_stop("1.0.0", "tbl_df()", "tibble::as_tibble()")
      3.     └─lifecycle:::deprecate_stop0(msg)
      4.       └─rlang::cnd_signal(...)
     Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     Error(s) in re-building vignettes:
       ...
     --- re-building ‘MonteCarlo-Vignette.Rmd’ using rmarkdown
     
     Quitting from MonteCarlo-Vignette.Rmd:315-320 [unnamed-chunk-15]
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     NULL
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'MonteCarlo-Vignette.Rmd' failed with diagnostics:
     `tbl_df()` was deprecated in dplyr 1.0.0 and is now defunct.
     ℹ Please use `tibble::as_tibble()` instead.
     --- failed re-building ‘MonteCarlo-Vignette.Rmd’
     
     SUMMARY: processing the following file failed:
       ‘MonteCarlo-Vignette.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

# moodleR (1.0.1)

* GitHub: <https://github.com/chi2labs/moodleR>
* Email: <mailto:dietrichson@gmail.com>
* GitHub mirror: <https://github.com/cran/moodleR>

Run `revdepcheck::cloud_details(, "moodleR")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     mdl_courses: no visible binding for global variable ‘id’
     mdl_create_cache: no visible binding for global variable ‘id’
     mdl_users: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# morse (3.3.4)

* Email: <mailto:virgile.baudrot@qonfluens.com>
* GitHub mirror: <https://github.com/cran/morse>

Run `revdepcheck::cloud_details(, "morse")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     plot.MFx: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# mvMAPIT (2.0.3)

* GitHub: <https://github.com/lcrawlab/mvMAPIT>
* Email: <mailto:julian_stamp@brown.edu>
* GitHub mirror: <https://github.com/cran/mvMAPIT>

Run `revdepcheck::cloud_details(, "mvMAPIT")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     mvmapit: no visible binding for global variable ‘id’
     simulate_traits: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# NBDCtools (1.0.2)

* Email: <mailto:dairc.service@gmail.com>
* GitHub mirror: <https://github.com/cran/NBDCtools>

Run `revdepcheck::cloud_details(, "NBDCtools")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     shadow_replace_binding_missing: no visible binding for global variable
       ‘id’
     Undefined global functions or variables:
       id
     ```

# nfl4th (1.0.4)

* GitHub: <https://github.com/nflverse/nfl4th>
* Email: <mailto:bbaldwin206@gmail.com>
* GitHub mirror: <https://github.com/cran/nfl4th>

Run `revdepcheck::cloud_details(, "nfl4th")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     get_4th_plays: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# nichetools (0.3.2)

* GitHub: <https://github.com/benjaminhlina/nichetools>
* Email: <mailto:benjamin.hlina@gmail.com>
* GitHub mirror: <https://github.com/cran/nichetools>

Run `revdepcheck::cloud_details(, "nichetools")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     extract_niche_size: no visible binding for global variable ‘id’
     extract_overlap: no visible binding for global variable ‘id’
     extract_sigma: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

## In both

*   checking DESCRIPTION meta-information ... NOTE
     ```
       Missing dependency on R >= 4.1.0 because package code uses the pipe
       |> or function shorthand \(...) syntax added in R 4.1.0.
       File(s) using such syntax:
         ‘create_comparison.R’ ‘create_comparisons.Rd’
         ‘extract_group_metrics.R’ ‘extract_group_metrics.Rd’
         ‘extract_layman.R’ ‘extract_layman.Rd’ ‘extract_mu.R’ ‘extract_mu.Rd’
         ‘extract_niche_size.R’ ‘extract_overlap.R’ ‘extract_sigma.R’
         ‘extract_similarities.R’ ‘extract_similarities.Rd’ ‘niche_ellipse.R’
     ```

# numbat (1.5.1)

* GitHub: <https://github.com/kharchenkolab/numbat>
* Email: <mailto:tgaoteng@gmail.com>
* GitHub mirror: <https://github.com/cran/numbat>

Run `revdepcheck::cloud_details(, "numbat")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     label_edges: no visible binding for global variable ‘id’
     mark_tumor_lineage: no visible binding for global variable ‘id’
     run_numbat : <anonymous>: no visible binding for global variable ‘id’
     transfer_links: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

## In both

*   checking dependencies in R code ... NOTE
     ```
     Namespaces in Imports field not imported from:
       ‘R.utils’ ‘optparse’ ‘vcfR’
       All declared Imports should be used.
     ```

# OnboardClient (1.0.0)

* Email: <mailto:christopher@onboarddata.io>
* GitHub mirror: <https://github.com/cran/OnboardClient>

Run `revdepcheck::cloud_details(, "OnboardClient")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     get_metadata: no visible binding for global variable ‘id’
     get_point_types: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# OpenLand (1.0.3)

* GitHub: <https://github.com/reginalexavier/OpenLand>
* Email: <mailto:reginalexavier@rocketmail.com>
* GitHub mirror: <https://github.com/cran/OpenLand>

Run `revdepcheck::cloud_details(, "OpenLand")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     netgrossplot: no visible binding for global variable ‘changes’
     Undefined global functions or variables:
       changes
     ```

# orderanalyzer (1.0.0)

* Email: <mailto:michael.scholz@th-deg.de>
* GitHub mirror: <https://github.com/cran/orderanalyzer>

Run `revdepcheck::cloud_details(, "orderanalyzer")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     .standardizeTables: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# origin (1.2.0)

* GitHub: <https://github.com/mnist91/origin>
* Email: <mailto:m_nistler@web.de>
* GitHub mirror: <https://github.com/cran/origin>

Run `revdepcheck::cloud_details(, "origin")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
       actual[75:81] vs expected[75:81]
         "data.table::last(id)"
         "szen <- data.table::last(id)"
         "data.table::last(szen)"
       - "id(id)"
       + "dplyr::id(id)"
         ""
         ""
         "# functional programming. Only checked if explicitly called."
       ── Failure ('test-originize_text.R:32:3'): Highlighting of plain text ──────────
       `result` (`actual`) not equal to paste(script_out, collapse = "\n") (`expected`).
       
       lines(actual)[51:57] vs lines(expected)[51:57]
         "data.table::last(id)"
         "szen <- data.table::last(id)"
         "data.table::last(szen)"
       - "id(id)"
       + "dplyr::id(id)"
         ""
         ""
         "# functional programming. Only checked if explicitly called."
       
       [ FAIL 8 | WARN 0 | SKIP 18 | PASS 118 ]
       Error: Test failures
       Execution halted
     ```

# oRus (1.0.0)

* GitHub: <https://github.com/melvidoni/oRus>
* Email: <mailto:melina.vidoni@rmit.edu.au>
* GitHub mirror: <https://github.com/cran/oRus>

Run `revdepcheck::cloud_details(, "oRus")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     parseRelatedStories: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

## In both

*   checking dependencies in R code ... NOTE
     ```
     Namespaces in Imports field not imported from:
       ‘knitr’ ‘tibble’
       All declared Imports should be used.
     ```

# pammtools (0.7.3)

* GitHub: <https://github.com/adibender/pammtools>
* Email: <mailto:andreas.bender@stat.uni-muenchen.de>
* GitHub mirror: <https://github.com/cran/pammtools>

Run `revdepcheck::cloud_details(, "pammtools")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     sim_pexp: no visible binding for global variable ‘id’
     sim_pexp_cr: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# parSim (0.1.5)

* Email: <mailto:mail@sachaepskamp.com>
* GitHub mirror: <https://github.com/cran/parSim>

Run `revdepcheck::cloud_details(, "parSim")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     parSim_dt: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# pccc (1.0.6)

* GitHub: <https://github.com/CUD2V/pccc>
* Email: <mailto:seth.russell@cuanschutz.edu>
* GitHub mirror: <https://github.com/cran/pccc>

Run `revdepcheck::cloud_details(, "pccc")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
       > stopifnot(x$message == "Only ICD version 9 and 10 are supported.")
       > 
       > # "Checking for error if string passed in"
       > x <- tryCatch(get_codes("ABC"), error = function(e) e)
       > stopifnot(inherits(x, "error"))
       > stopifnot(inherits(x, "Rcpp::not_compatible"))
       > stopifnot(x$message == "Not compatible with requested type: [type=character; target=integer].")
       > 
       > # testing the S3 methods for as.tbl and as_tibble
       > x <- get_codes(9)
       > y <- tryCatch(dplyr::as.tbl(x), warning = function(w) w)
       Error:
       ! `as.tbl()` was deprecated in dplyr 1.0.0 and is now defunct.
       ℹ Please use `tibble::as_tibble()` instead.
       Backtrace:
           ▆
        1. ├─base::tryCatch(dplyr::as.tbl(x), warning = function(w) w)
        2. │ └─base (local) tryCatchList(expr, classes, parentenv, handlers)
        3. │   └─base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
        4. │     └─base (local) doTryCatch(return(expr), name, parentenv, handler)
        5. └─dplyr::as.tbl(x)
        6.   └─lifecycle::deprecate_stop("1.0.0", "as.tbl()", "tibble::as_tibble()")
        7.     └─lifecycle:::deprecate_stop0(msg)
        8.       └─rlang::cnd_signal(...)
       Execution halted
     ```

# pctax (0.1.3)

* GitHub: <https://github.com/Asa12138/pctax>
* Email: <mailto:pengchen2001@zju.edu.cn>
* GitHub mirror: <https://github.com/cran/pctax>

Run `revdepcheck::cloud_details(, "pctax")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     plot.time_cm: no visible binding for global variable ‘id’
     plot_two_tree: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# PEIMAN2 (1.0.1)

* Email: <mailto:payman.nickchi@gmail.com>
* GitHub mirror: <https://github.com/cran/PEIMAN2>

Run `revdepcheck::cloud_details(, "PEIMAN2")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     psea2mass: no visible binding for global variable ‘id’
     sea2mass: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# pems.utils (0.3.0.8)

* Email: <mailto:karl.ropkins@gmail.com>
* GitHub mirror: <https://github.com/cran/pems.utils>

Run `revdepcheck::cloud_details(, "pems.utils")` for more info

## Newly broken

*   checking dependencies in R code ... WARNING
     ```
     Missing or unexported objects:
       ‘dplyr::rename_vars’ ‘dplyr::select_vars’
     ```

# petrinetR (0.3.0)

* GitHub: <https://github.com/bupaverse/petrinetR>
* Email: <mailto:gert.janssenswillen@uhasselt.be>
* GitHub mirror: <https://github.com/cran/petrinetR>

Run `revdepcheck::cloud_details(, "petrinetR")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     enabled: no visible binding for global variable ‘id’
     label: no visible binding for global variable ‘id’
     read_PN: no visible binding for global variable ‘id’
     rename_places: no visible binding for global variable ‘id’
     rename_transitions: no visible binding for global variable ‘id’
     render_PN: no visible binding for global variable ‘id’
     visNetwork_from_PN: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

## In both

*   checking Rd files ... NOTE
     ```
     checkRd: (-1) read_PN.Rd:15: Lost braces
         15 | A code{\link{marked_petrinet}}
            |       ^
     ```

# placer (0.1.3)

* Email: <mailto:davi.tavares@leibniz-zmt.de>
* GitHub mirror: <https://github.com/cran/placer>

Run `revdepcheck::cloud_details(, "placer")` for more info

## Newly broken

*   checking re-building of vignette outputs ... ERROR
     ```
     Error(s) in re-building vignettes:
       ...
     --- re-building ‘PlAcER.Rmd’ using rmarkdown
     
     Quitting from PlAcER.Rmd:51-57 [unnamed-chunk-1]
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     NULL
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'PlAcER.Rmd' failed with diagnostics:
     `tbl_df()` was deprecated in dplyr 1.0.0 and is now defunct.
     ℹ Please use `tibble::as_tibble()` instead.
     --- failed re-building ‘PlAcER.Rmd’
     
     SUMMARY: processing the following file failed:
       ‘PlAcER.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

# processmapR (0.5.7)

* GitHub: <https://github.com/bupaverse/processmapr>
* Email: <mailto:gert.janssenswillen@uhasselt.be>
* GitHub mirror: <https://github.com/cran/processmapR>

Run `revdepcheck::cloud_details(, "processmapR")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     render_map: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# protti (0.9.1)

* GitHub: <https://github.com/jpquast/protti>
* Email: <mailto:quast@imsb.biol.ethz.ch>
* GitHub mirror: <https://github.com/cran/protti>

Run `revdepcheck::cloud_details(, "protti")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     find_all_subs: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# RcmdrPlugin.RiskDemo (3.2)

* Email: <mailto:arto.luoma@wippies.com>
* GitHub mirror: <https://github.com/cran/RcmdrPlugin.RiskDemo>

Run `revdepcheck::cloud_details(, "RcmdrPlugin.RiskDemo")` for more info

## Newly broken

*   checking whether package ‘RcmdrPlugin.RiskDemo’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/tmp/workdir/RcmdrPlugin.RiskDemo/new/RcmdrPlugin.RiskDemo.Rcheck/00install.out’ for details.
     ```

## Installation

### Devel

```
* installing *source* package ‘RcmdrPlugin.RiskDemo’ ...
** this is package ‘RcmdrPlugin.RiskDemo’ version ‘3.2’
** package ‘RcmdrPlugin.RiskDemo’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘location’ is not exported by 'namespace:dplyr'
Execution halted
ERROR: lazy loading failed for package ‘RcmdrPlugin.RiskDemo’
* removing ‘/tmp/workdir/RcmdrPlugin.RiskDemo/new/RcmdrPlugin.RiskDemo.Rcheck/RcmdrPlugin.RiskDemo’


```
### CRAN

```
* installing *source* package ‘RcmdrPlugin.RiskDemo’ ...
** this is package ‘RcmdrPlugin.RiskDemo’ version ‘3.2’
** package ‘RcmdrPlugin.RiskDemo’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (RcmdrPlugin.RiskDemo)


```
# recforest (1.0.0)

* Email: <mailto:murris.juliette@gmail.com>
* GitHub mirror: <https://github.com/cran/recforest>

Run `revdepcheck::cloud_details(, "recforest")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     c_index_2: no visible binding for global variable ‘id’
     mse_2: no visible binding for global variable ‘id’
     train_tree_and_calculate_metrics: no visible binding for global
       variable ‘id’
     Undefined global functions or variables:
       id
     ```

# REDCapTidieR (1.2.4)

* GitHub: <https://github.com/CHOP-CGTInformatics/REDCapTidieR>
* Email: <mailto:hannar1@chop.edu>
* GitHub mirror: <https://github.com/cran/REDCapTidieR>

Run `revdepcheck::cloud_details(, "REDCapTidieR")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
       > library(REDCapTidieR)
       > 
       > test_check("REDCapTidieR")
       [ FAIL 1 | WARN 9 | SKIP 5 | PASS 323 ]
       
       ══ Skipped tests (5) ═══════════════════════════════════════════════════════════
       • On CRAN (5): 'test-lint-free.R:2:3', 'test-read_redcap.R:1:1',
         'test-supertibble.R:2:3', 'test-utils.R:214:3', 'test-write.R:341:3'
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Failure ('test-write.R:152:5'): write_redcap_xlsx has expected supertibble and metadata outputs ──
       names(sheet_4) (`actual`) and `expected_meta_labels` (`expected`) don't have the same values.
       * Only in `actual`: NA
       * Only in `expected`: "REDCap Instrument Name", "REDCap Instrument Description"
       
       Backtrace:
           ▆
        1. ├─withr::with_tempdir(...) at test-write.R:134:3
        2. │ └─withr::with_dir(tmp, code)
        3. │   └─base::force(code)
        4. └─testthat::expect_setequal(names(sheet_4), expected_meta_labels) at test-write.R:152:5
       
       [ FAIL 1 | WARN 9 | SKIP 5 | PASS 323 ]
       Error: Test failures
       Execution halted
     ```

# refund.shiny (1.1)

* GitHub: <https://github.com/refunders/refund.shiny>
* Email: <mailto:julia.wrobel@emory.edu>
* GitHub mirror: <https://github.com/cran/refund.shiny>

Run `revdepcheck::cloud_details(, "refund.shiny")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     plot_shiny.flcm : <anonymous>: no visible binding for global variable
       ‘id’
     plot_shiny.fpca : <anonymous>: no visible binding for global variable
       ‘id’
     plot_shiny.registration : <anonymous>: no visible binding for global
       variable ‘id’
     registerLasagna: no visible binding for global variable ‘id’
     thin_functional_data: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# registr (2.1.0)

* Email: <mailto:julia.wrobel@cuanschutz.edu>
* GitHub mirror: <https://github.com/cran/registr>

Run `revdepcheck::cloud_details(, "registr")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     data_clean: no visible binding for global variable ‘id’
     register_fpca: no visible binding for global variable ‘id’
     simulate_functional_data: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

## In both

*   checking C++ specification ... NOTE
     ```
       Specified C++11: please drop specification unless essential
     ```

# revulyticsR (0.0.3)

* GitHub: <https://github.com/chrisumphlett/revulyticsR>
* Email: <mailto:christopher.umphlett@gmail.com>
* GitHub mirror: <https://github.com/cran/revulyticsR>

Run `revdepcheck::cloud_details(, "revulyticsR")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     get_daily_client_properties : get_one_product_metadata: no visible
       binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# rifttable (0.7.1)

* GitHub: <https://github.com/stopsack/rifttable>
* Email: <mailto:stopsack@post.harvard.edu>
* GitHub mirror: <https://github.com/cran/rifttable>

Run `revdepcheck::cloud_details(, "rifttable")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
       
       actual vs expected
                                 Summary
         actual[1, ]    N               
       - actual[2, ]    death           
       + expected[2, ]  Death           
         actual[3, ]      Unknown       
       - actual[4, ]    receptor        
       + expected[4, ]  Hormone receptor
         actual[5, ]      High          
         actual[6, ]      Low           
         actual[7, ]      Unknown       
       
           actual$Summary | expected$Summary      
       [1] "N"            | "N"                [1]
       [2] "death"        - "Death"            [2]
       [3] "  Unknown"    | "  Unknown"        [3]
       [4] "receptor"     - "Hormone receptor" [4]
       [5] "  High"       | "  High"           [5]
       [6] "  Low"        | "  Low"            [6]
       [7] "  Unknown"    | "  Unknown"        [7]
       
       [ FAIL 14 | WARN 0 | SKIP 0 | PASS 149 ]
       Error: Test failures
       Execution halted
     ```

# risk.assessr (2.0.1)

* GitHub: <https://github.com/Sanofi-Public/risk.assessr>
* Email: <mailto:edward.gillian-ext@sanofi.com>
* GitHub mirror: <https://github.com/cran/risk.assessr>

Run `revdepcheck::cloud_details(, "risk.assessr")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
         7. │     ├─purrr:::call_with_cleanup(...)
         8. │     └─risk.assessr (local) .f(.x[[i]], ...)
         9. │       ├─dplyr::mutate(...)
        10. │       └─dplyr:::mutate.data.frame(...)
        11. │         └─dplyr:::mutate_cols(.data, dplyr_quosures(...), by)
        12. │           ├─base::withCallingHandlers(...)
        13. │           └─dplyr:::mutate_col(dots[[i]], data, mask, new_columns)
        14. │             └─mask$eval_all_mutate(quo)
        15. │               └─dplyr (local) eval()
        16. ├─dplyr::case_when(...)
        17. │ └─vctrs::vec_case_when(...)
        18. ├─rlang::abort(message = message, call = call)
        19. │ └─rlang:::signal_abort(cnd, .file)
        20. │   └─base::signalCondition(cnd)
        21. ├─dplyr (local) `<fn>`(`<rlng_rrr>`)
        22. │ └─rlang::abort(message, class = error_class, parent = parent, call = error_call)
        23. │   └─rlang:::signal_abort(cnd, .file)
        24. │     └─base::signalCondition(cnd)
        25. └─purrr (local) `<fn>`(`<dply:::_>`)
        26.   └─cli::cli_abort(...)
        27.     └─rlang::abort(...)
       
       [ FAIL 1 | WARN 0 | SKIP 26 | PASS 609 ]
       Error: Test failures
       Execution halted
     ```

# roads (1.2.0)

* GitHub: <https://github.com/LandSciTech/roads>
* Email: <mailto:sarah.endicott@ec.gc.ca>
* GitHub mirror: <https://github.com/cran/roads>

Run `revdepcheck::cloud_details(, "roads")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     getGraph: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

## In both

*   checking DESCRIPTION meta-information ... NOTE
     ```
       Missing dependency on R >= 4.1.0 because package code uses the pipe
       |> or function shorthand \(...) syntax added in R 4.1.0.
       File(s) using such syntax:
         ‘projectRoads.Rd’
     ```

# rpivotTable (0.3.0)

* GitHub: <https://github.com/NA/NA>
* Email: <mailto:enzo@smartinsightsfromdata.com>
* GitHub mirror: <https://github.com/cran/rpivotTable>

Run `revdepcheck::cloud_details(, "rpivotTable")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
       > library(rpivotTable)
       > 
       > test_check("rpivotTable")
       [ FAIL 1 | WARN 0 | SKIP 0 | PASS 21 ]
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Error ('test_data.R:22:3'): data is not changed ─────────────────────────────
       <lifecycle_error_deprecated/defunctError/rlang_error/error/condition>
       Error: `tbl_df()` was deprecated in dplyr 1.0.0 and is now defunct.
       ℹ Please use `tibble::as_tibble()` instead.
       Backtrace:
           ▆
        1. ├─testthat::expect_is(rpivotTable(tbl_df(iris))$x$data, "tbl_df") at test_data.R:22:3
        2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
        3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
        4. ├─rpivotTable::rpivotTable(tbl_df(iris))
        5. │ └─base::intersect(...)
        6. └─dplyr::tbl_df(iris)
        7.   └─lifecycle::deprecate_stop("1.0.0", "tbl_df()", "tibble::as_tibble()")
        8.     └─lifecycle:::deprecate_stop0(msg)
        9.       └─rlang::cnd_signal(...)
       
       [ FAIL 1 | WARN 0 | SKIP 0 | PASS 21 ]
       Error: Test failures
       Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     Error(s) in re-building vignettes:
       ...
     --- re-building ‘NEWS.Rmd’ using rmarkdown
     --- finished re-building ‘NEWS.Rmd’
     
     --- re-building ‘rpivotTableIntroduction.Rmd’ using rmarkdown
     
     Quitting from rpivotTableIntroduction.Rmd:102-110 [unnamed-chunk-8]
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     NULL
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'rpivotTableIntroduction.Rmd' failed with diagnostics:
     `tbl_df()` was deprecated in dplyr 1.0.0 and is now defunct.
     ℹ Please use `tibble::as_tibble()` instead.
     --- failed re-building ‘rpivotTableIntroduction.Rmd’
     
     SUMMARY: processing the following file failed:
       ‘rpivotTableIntroduction.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

## In both

*   checking Rd files ... NOTE
     ```
     checkRd: (-1) rpivotTable.Rd:60: Lost braces; missing escapes or markup?
         60 | An example is: onRefresh=htmlwidgets::JS("function(config) { Shiny.onInputChange('myPivotData', config); }")
            |                                                            ^
     ```

# scistreer (1.2.0)

* GitHub: <https://github.com/kharchenkolab/scistreer>
* Email: <mailto:tgaoteng@gmail.com>
* GitHub mirror: <https://github.com/cran/scistreer>

Run `revdepcheck::cloud_details(, "scistreer")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     annotate_tree: no visible binding for global variable ‘id’
     label_edges: no visible binding for global variable ‘id’
     mut_to_tree: no visible binding for global variable ‘id’
     transfer_links: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

## In both

*   checking dependencies in R code ... NOTE
     ```
     Namespace in Imports field not imported from: ‘Rcpp’
       All declared Imports should be used.
     ```

# scSpatialSIM (0.1.3.4)

* GitHub: <https://github.com/FridleyLab/scSpatialSIM>
* Email: <mailto:fridley.lab@moffitt.org>
* GitHub mirror: <https://github.com/cran/scSpatialSIM>

Run `revdepcheck::cloud_details(, "scSpatialSIM")` for more info

## Newly broken

*   checking re-building of vignette outputs ... ERROR
     ```
     Error(s) in re-building vignettes:
     --- re-building ‘a01_Introduction.Rmd’ using rmarkdown
     ```

# servosphereR (0.1.1)

* GitHub: <https://github.com/wittja01/servosphereR>
* Email: <mailto:wittja01@gmail.com>
* GitHub mirror: <https://github.com/cran/servosphereR>

Run `revdepcheck::cloud_details(, "servosphereR")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     aggregateData : <anonymous>: no visible binding for global variable
       ‘id’
     summaryAvgVelocity : <anonymous>: no visible binding for global
       variable ‘id’
     summaryNetDisplacement : <anonymous>: no visible binding for global
       variable ‘id’
     summaryStops : <anonymous>: no visible binding for global variable ‘id’
     summaryTotalDistance : <anonymous>: no visible binding for global
       variable ‘id’
     Undefined global functions or variables:
       id
     ```

# shinyloadtest (1.2.0)

* GitHub: <https://github.com/rstudio/shinyloadtest>
* Email: <mailto:barret@posit.co>
* GitHub mirror: <https://github.com/cran/shinyloadtest>

Run `revdepcheck::cloud_details(, "shinyloadtest")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     load_runs : <anonymous>: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# SimplyAgree (0.2.1)

* GitHub: <https://github.com/arcaldwell49/SimplyAgree>
* Email: <mailto:arcaldwell49@gmail.com>
* GitHub mirror: <https://github.com/cran/SimplyAgree>

Run `revdepcheck::cloud_details(, "SimplyAgree")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     calc_loa_nest: no visible binding for global variable ‘id’
     calc_loa_reps: no visible binding for global variable ‘id’
     plot.simple_reli: no visible binding for global variable ‘id’
     simple_ident_plot: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# simTool (1.1.8)

* GitHub: <https://github.com/MarselScheer/simTool>
* Email: <mailto:scheer@freescience.de>
* GitHub mirror: <https://github.com/cran/simTool>

Run `revdepcheck::cloud_details(, "simTool")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
       9.             └─simTool (local) FUN(X[[i]], ...)
      10.               └─purrr::map(...)
      11.                 └─purrr:::map_("list", .x, .f, ..., .progress = .progress)
      12.                   ├─purrr:::with_indexed_errors(...)
      13.                   │ └─base::withCallingHandlers(...)
      14.                   ├─purrr:::call_with_cleanup(...)
      15.                   └─simTool (local) .f(.x[[i]], ...)
      16.                     ├─dplyr::summarize_all(...)
      17.                     │ └─dplyr:::manip_all(...)
      18.                     │   ├─rlang::syms(tbl_nongroup_vars(.tbl))
      19.                     │   │ └─rlang:::map(x, sym)
      20.                     │   │   └─base::lapply(.x, .f, ...)
      21.                     │   └─dplyr::tbl_nongroup_vars(.tbl)
      22.                     │     ├─generics::setdiff(tbl_vars(x), group_vars(x))
      23.                     │     └─dplyr::tbl_vars(x)
      24.                     │       ├─dplyr:::new_sel_vars(tbl_vars_dispatch(x), group_vars(x))
      25.                     │       │ └─base::structure(...)
      26.                     │       └─dplyr:::tbl_vars_dispatch(x)
      27.                     ├─dplyr::group_by(., .dots = group_for_summary)
      28.                     └─dplyr:::group_by.data.frame(., .dots = group_for_summary)
      29.                       └─dplyr::group_by_prepare(.data, ..., .add = .add, error_call = current_env())
      30.                         └─lifecycle::deprecate_stop("1.0.0", "group_by(.dots = )")
      31.                           └─lifecycle:::deprecate_stop0(msg)
      32.                             └─rlang::cnd_signal(...)
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
        15.                         └─simTool (local) FUN(X[[i]], ...)
        16.                           └─purrr::map(...)
        17.                             └─purrr:::map_("list", .x, .f, ..., .progress = .progress)
        18.                               ├─purrr:::with_indexed_errors(...)
        19.                               │ └─base::withCallingHandlers(...)
        20.                               ├─purrr:::call_with_cleanup(...)
        21.                               └─simTool (local) .f(.x[[i]], ...)
        22.                                 ├─dplyr::summarize_all(...)
        23.                                 │ └─dplyr:::manip_all(...)
        24.                                 │   ├─rlang::syms(tbl_nongroup_vars(.tbl))
        25.                                 │   │ └─rlang:::map(x, sym)
        26.                                 │   │   └─base::lapply(.x, .f, ...)
        27.                                 │   └─dplyr::tbl_nongroup_vars(.tbl)
        28.                                 │     ├─generics::setdiff(tbl_vars(x), group_vars(x))
        29.                                 │     └─dplyr::tbl_vars(x)
        30.                                 │       ├─dplyr:::new_sel_vars(tbl_vars_dispatch(x), group_vars(x))
        31.                                 │       │ └─base::structure(...)
        32.                                 │       └─dplyr:::tbl_vars_dispatch(x)
        33.                                 ├─dplyr::group_by(., .dots = group_for_summary)
        34.                                 └─dplyr:::group_by.data.frame(., .dots = group_for_summary)
        35.                                   └─dplyr::group_by_prepare(.data, ..., .add = .add, error_call = current_env())
        36.                                     └─lifecycle::deprecate_stop("1.0.0", "group_by(.dots = )")
        37.                                       └─lifecycle:::deprecate_stop0(msg)
        38.                                         └─rlang::cnd_signal(...)
       Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     Error(s) in re-building vignettes:
       ...
     --- re-building ‘simTool.Rmd’ using rmarkdown
     
     Quitting from simTool.Rmd:44-59 [unnamed-chunk-2]
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     NULL
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'simTool.Rmd' failed with diagnostics:
     ℹ In index: 1.
     Caused by error:
     ! The `.dots` argument of `group_by()` was deprecated in dplyr 1.0.0 and is now defunct.
     --- failed re-building ‘simTool.Rmd’
     
     SUMMARY: processing the following file failed:
       ‘simTool.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

# soc.ca (0.8.1)

* GitHub: <https://github.com/Rsoc/soc.ca>
* Email: <mailto:agraul@ruc.dk>
* GitHub mirror: <https://github.com/cran/soc.ca>

Run `revdepcheck::cloud_details(, "soc.ca")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     mca.triads : get.sup: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# srcr (1.1.1)

* GitHub: <https://github.com/baileych/srcr>
* Email: <mailto:baileyc@chop.edu>
* GitHub mirror: <https://github.com/cran/srcr>

Run `revdepcheck::cloud_details(, "srcr")` for more info

## Newly broken

*   checking Rd cross-references ... WARNING
     ```
     Missing link(s) in Rd file 'srcr.Rd':
       ‘[dplyr:src_dbi]{dplyr::src_postgres()}’
     
     See section 'Cross-references' in the 'Writing R Extensions' manual.
     ```

# stabiliser (1.0.6)

* Email: <mailto:robert.hyde4@nottingham.ac.uk>
* GitHub mirror: <https://github.com/cran/stabiliser>

Run `revdepcheck::cloud_details(, "stabiliser")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     perm_model: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

## In both

*   checking dependencies in R code ... NOTE
     ```
     Namespace in Imports field not imported from: ‘knitr’
       All declared Imports should be used.
     ```

# statsExpressions (1.7.1)

* GitHub: <https://github.com/IndrajeetPatil/statsExpressions>
* Email: <mailto:patilindrajeet.science@gmail.com>
* GitHub mirror: <https://github.com/cran/statsExpressions>

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

* GitHub: <https://github.com/HEAL-KGS/STICr>
* Email: <mailto:samzipper@ku.edu>
* GitHub mirror: <https://github.com/cran/STICr>

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
     ! `condition` must be a logical vector, not a logical matrix.
     Backtrace:
         ▆
      1. ├─STICr::classify_wetdry(...)
      2. │ └─dplyr::if_else(class_var >= threshold, "wet", "dry")
      3. │   └─vctrs::vec_if_else(...)
      4. └─rlang::abort(message = message, call = call)
     Execution halted
     ```

# strand (0.2.2)

* GitHub: <https://github.com/strand-tech/strand>
* Email: <mailto:jeffrey.enos@gmail.com>
* GitHub mirror: <https://github.com/cran/strand>

Run `revdepcheck::cloud_details(, "strand")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     > sim <- Simulation$new(config,
     +                       raw_input_data = sample_inputs,
     +                       raw_pricing_data = sample_pricing,
     +                       security_reference_data = sample_secref)
     > sim$run()
     Error:
     ! The `.dots` argument of `group_by()` was deprecated in dplyr 1.0.0 and
       is now defunct.
     Backtrace:
          ▆
       1. ├─sim$run()
       2. │ └─strand:::calculate_exposures(...)
       3. │   └─... %>% select(-"weight_divisor")
       4. ├─dplyr::select(., -"weight_divisor")
       5. ├─tidyr::pivot_wider(...)
       6. ├─dplyr::mutate(., exposure = .data$exposure/.data$weight_divisor)
       7. ├─dplyr::left_join(., weight_divisor_df, by = "strategy")
       8. ├─dplyr::summarise(., exposure = sum(.data[[in_var]]))
       9. ├─dplyr::group_by(., .dots = c("strategy", cat_var))
      10. └─dplyr:::group_by.data.frame(., .dots = c("strategy", cat_var))
      11.   └─dplyr::group_by_prepare(.data, ..., .add = .add, error_call = current_env())
      12.     └─lifecycle::deprecate_stop("1.0.0", "group_by(.dots = )")
      13.       └─lifecycle:::deprecate_stop0(msg)
      14.         └─rlang::cnd_signal(...)
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Error ('test_Simulation.R:8:1'): (code run outside of `test_that()`) ────────
       <lifecycle_error_deprecated/defunctError/rlang_error/error/condition>
       Error: The `.dots` argument of `group_by()` was deprecated in dplyr 1.0.0 and
       is now defunct.
       Backtrace:
            ▆
         1. ├─sim$run() at test_Simulation.R:8:1
         2. │ └─strand:::calculate_exposures(...)
         3. │   └─... %>% select(-"weight_divisor")
         4. ├─dplyr::select(., -"weight_divisor")
         5. ├─tidyr::pivot_wider(...)
         6. ├─dplyr::mutate(., exposure = .data$exposure/.data$weight_divisor)
         7. ├─dplyr::left_join(., weight_divisor_df, by = "strategy")
         8. ├─dplyr::summarise(., exposure = sum(.data[[in_var]]))
         9. ├─dplyr::group_by(., .dots = c("strategy", cat_var))
        10. └─dplyr:::group_by.data.frame(., .dots = c("strategy", cat_var))
        11.   └─dplyr::group_by_prepare(.data, ..., .add = .add, error_call = current_env())
        12.     └─lifecycle::deprecate_stop("1.0.0", "group_by(.dots = )")
        13.       └─lifecycle:::deprecate_stop0(msg)
        14.         └─rlang::cnd_signal(...)
       
       [ FAIL 1 | WARN 0 | SKIP 0 | PASS 61 ]
       Error: Test failures
       Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     Error(s) in re-building vignettes:
       ...
     --- re-building ‘strand.Rmd’ using rmarkdown
     
     Quitting from strand.Rmd:438-450 [unnamed-chunk-7]
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     NULL
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'strand.Rmd' failed with diagnostics:
     The `.dots` argument of `group_by()` was deprecated in dplyr 1.0.0 and
     is now defunct.
     --- failed re-building ‘strand.Rmd’
     
     SUMMARY: processing the following file failed:
       ‘strand.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

# StratifiedMedicine (1.0.5)

* GitHub: <https://github.com/thomasjemielita/StratifiedMedicine>
* Email: <mailto:thomasjemielita@gmail.com>
* GitHub mirror: <https://github.com/cran/StratifiedMedicine>

Run `revdepcheck::cloud_details(, "StratifiedMedicine")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     plot_ple: no visible binding for global variable ‘id’
     plot_tree: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# sugarbag (0.1.6)

* GitHub: <https://github.com/srkobakian/sugarbag>
* Email: <mailto:dicook@monash.edu>
* GitHub mirror: <https://github.com/cran/sugarbag>

Run `revdepcheck::cloud_details(, "sugarbag")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     allocate: no visible binding for global variable ‘id’
     filter_grid_points: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

## In both

*   checking Rd files ... NOTE
     ```
     checkRd: (-1) fp19.Rd:11: Lost braces in \itemize; meant \describe ?
     checkRd: (-1) fp19.Rd:12-13: Lost braces in \itemize; meant \describe ?
     checkRd: (-1) fp19.Rd:14: Lost braces in \itemize; meant \describe ?
     checkRd: (-1) fp19.Rd:15: Lost braces in \itemize; meant \describe ?
     checkRd: (-1) fp19.Rd:16: Lost braces in \itemize; meant \describe ?
     checkRd: (-1) fp19.Rd:17: Lost braces in \itemize; meant \describe ?
     checkRd: (-1) fp19.Rd:18: Lost braces in \itemize; meant \describe ?
     checkRd: (-1) fp19.Rd:19: Lost braces in \itemize; meant \describe ?
     checkRd: (-1) fp19.Rd:20: Lost braces in \itemize; meant \describe ?
     checkRd: (-1) fp19.Rd:21: Lost braces in \itemize; meant \describe ?
     checkRd: (-1) fp19.Rd:22: Lost braces in \itemize; meant \describe ?
     checkRd: (-1) fp19.Rd:23-24: Lost braces in \itemize; meant \describe ?
     checkRd: (-1) fp19.Rd:25: Lost braces in \itemize; meant \describe ?
     ```

# surveydata (0.2.7)

* GitHub: <https://github.com/andrie/surveydata>
* Email: <mailto:apdevries@gmail.com>
* GitHub mirror: <https://github.com/cran/surveydata>

Run `revdepcheck::cloud_details(, "surveydata")` for more info

## Newly broken

*   checking re-building of vignette outputs ... ERROR
     ```
     Error(s) in re-building vignettes:
     --- re-building ‘surveydata.Rmd’ using rmarkdown
     The `surveydata` package makes it easy to work with typical survey data that originated in SPSS or other formats.
     
     ## Motivation
     
     Specifically, the package makes it easy to include the question text as metadata with the data itself.
     
     To track the questions of a survey, you have two options:
     ```

# survHE (2.0.5)

* GitHub: <https://github.com/giabaio/survHE>
* Email: <mailto:g.baio@ucl.ac.uk>
* GitHub mirror: <https://github.com/cran/survHE>

Run `revdepcheck::cloud_details(, "survHE")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     make_profile_surv: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# survivoR (2.3.8)

* GitHub: <https://github.com/doehm/survivoR>
* Email: <mailto:danieloehm@gmail.com>
* GitHub mirror: <https://github.com/cran/survivoR>

Run `revdepcheck::cloud_details(, "survivoR")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     apply_edits: no visible binding for global variable ‘id’
     conf_app_server: no visible binding for global variable ‘id’
     get_confessional_timing: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# survminer (0.5.1)

* GitHub: <https://github.com/kassambara/survminer>
* Email: <mailto:alboukadel.kassambara@gmail.com>
* GitHub mirror: <https://github.com/cran/survminer>

Run `revdepcheck::cloud_details(, "survminer")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     > set.seed(1234)
     > colon2 <- dplyr::sample_frac(colon, 1/2)
     > 
     > # Create a named list of formulas
     > formula.list <- list(
     +  sex = Surv(time, status) ~ sex,
     +  adhere = Surv(time, status) ~ adhere,
     +  rx = Surv(time, status) ~ rx
     + )
     > 
     > # Fit survival curves
     > fit <- surv_fit(formula.list, data = list(colon1, colon2),
     +                match.fd = FALSE)
     Error:
     ! `combine()` was deprecated in dplyr 1.0.0 and is now defunct.
     ℹ Please use `vctrs::vec_c()` instead.
     Backtrace:
         ▆
      1. ├─survminer::surv_fit(...)
      2. │ └─purrr::map(formula, .map_each, data) %>% dplyr::combine()
      3. └─dplyr::combine(.)
      4.   └─lifecycle::deprecate_stop("1.0.0", "combine()", "vctrs::vec_c()")
      5.     └─lifecycle:::deprecate_stop0(msg)
      6.       └─rlang::cnd_signal(...)
     Execution halted
     ```

# tabr (0.5.4)

* GitHub: <https://github.com/leonawicz/tabr>
* Email: <mailto:rpkgs@pm.me>
* GitHub mirror: <https://github.com/cran/tabr>

Run `revdepcheck::cloud_details(, "tabr")` for more info

## Newly broken

*   checking re-building of vignette outputs ... ERROR
     ```
     ...
     --- finished re-building ‘tabr-prog-noteinfo.Rmd’
     
     --- re-building ‘tabr-prog-notes.Rmd’ using rmarkdown
     --- finished re-building ‘tabr-prog-notes.Rmd’
     
     --- re-building ‘tabr-prog-nw.Rmd’ using rmarkdown
     --- finished re-building ‘tabr-prog-nw.Rmd’
     
     --- re-building ‘tabr-prog-scales.Rmd’ using rmarkdown
     
     Quitting from tabr-prog-scales.Rmd:10-22 [setup]
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     NULL
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'tabr-prog-scales.Rmd' failed with diagnostics:
     [1m[22m`tbl_df()` was deprecated in dplyr 1.0.0 and is now defunct.
     [36mℹ[39m Please use `tibble::as_tibble()` instead.
     --- failed re-building ‘tabr-prog-scales.Rmd’
     
     SUMMARY: processing the following file failed:
       ‘tabr-prog-scales.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

# TextForecast (0.1.3)

* GitHub: <https://github.com/lucasgodeiro/TextForecast>
* Email: <mailto:lucas.godeiro@hotmail.com>
* GitHub mirror: <https://github.com/cran/TextForecast>

Run `revdepcheck::cloud_details(, "TextForecast")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     + ntrms=500,ngrams_number=3,min_freq=1)
     Downloading udpipe model from https://raw.githubusercontent.com/jwijffels/udpipe.models.ud.2.5/master/inst/udpipe-ud-2.5-191206/english-ewt-ud-2.5-191206.udpipe to /tmp/Rtmp7tEFqZ/english-ewt-ud-2.5-191206.udpipe
      - This model has been trained on version 2.5 of data from https://universaldependencies.org
      - The model is distributed under the CC-BY-SA-NC license: https://creativecommons.org/licenses/by-nc-sa/4.0
      - Visit https://github.com/jwijffels/udpipe.models.ud.2.5 for model license details.
      - For a list of all models and their licenses (most models you can download with this package have either a CC-BY-SA or a CC-BY-SA-NC license) read the documentation at ?udpipe_download_model. For building your own models: visit the documentation by typing vignette('udpipe-train', package = 'udpipe')
     trying URL 'https://raw.githubusercontent.com/jwijffels/udpipe.models.ud.2.5/master/inst/udpipe-ud-2.5-191206/english-ewt-ud-2.5-191206.udpipe'
     Content type 'application/octet-stream' length 16309608 bytes (15.6 MB)
     ==================================================
     downloaded 15.6 MB
     
     Downloading finished, model stored at '/tmp/Rtmp7tEFqZ/english-ewt-ud-2.5-191206.udpipe'
     [1] "2019-30-01"
     [1] "2025-11-09 14:06:10 UTC"
     Error:
     ! `tbl_df()` was deprecated in dplyr 1.0.0 and is now defunct.
     ℹ Please use `tibble::as_tibble()` instead.
     Backtrace:
         ▆
      1. └─TextForecast::get_collocations(...)
      2.   └─dplyr::tbl_df(Tdm.stack)
      3.     └─lifecycle::deprecate_stop("1.0.0", "tbl_df()", "tibble::as_tibble()")
      4.       └─lifecycle:::deprecate_stop0(msg)
      5.         └─rlang::cnd_signal(...)
     Execution halted
     ```

## In both

*   checking dependencies in R code ... NOTE
     ```
     Namespaces in Imports field not imported from:
       ‘doParallel’ ‘forecast’ ‘parallel’ ‘tidyr’
       All declared Imports should be used.
     ```

# tglkmeans (0.5.5)

* GitHub: <https://github.com/tanaylab/tglkmeans>
* Email: <mailto:aviezer.lifshitz@weizmann.ac.il>
* GitHub mirror: <https://github.com/cran/tglkmeans>

Run `revdepcheck::cloud_details(, "tglkmeans")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     TGL_kmeans_tidy: no visible binding for global variable ‘id’
     hclust_every_cluster: no visible binding for global variable ‘id’
     match_clusters: no visible binding for global variable ‘id’
     reorder_clusters: no visible binding for global variable ‘id’
     simulate_data: no visible binding for global variable ‘id’
     test_clustering: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

## In both

*   checking dependencies in R code ... NOTE
     ```
     Namespace in Imports field not imported from: ‘doRNG’
       All declared Imports should be used.
     ```

# tidyCDISC (0.2.1)

* GitHub: <https://github.com/Biogen-Inc/tidyCDISC>
* Email: <mailto:clark.aaronchris@gmail.com>
* GitHub mirror: <https://github.com/cran/tidyCDISC>

Run `revdepcheck::cloud_details(, "tidyCDISC")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     app_lineplot: no visible binding for global variable ‘id’
     mod_indvExpPatEvents_server: no visible binding for global variable
       ‘id’
     Undefined global functions or variables:
       id
     ```

# tidycensus (1.7.3)

* GitHub: <https://github.com/walkerke/tidycensus>
* Email: <mailto:kyle@walker-data.com>
* GitHub mirror: <https://github.com/cran/tidycensus>

Run `revdepcheck::cloud_details(, "tidycensus")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     get_flows: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# tidypopgen (0.4.0)

* GitHub: <https://github.com/EvolEcolGroup/tidypopgen>
* Email: <mailto:am315@cam.ac.uk>
* GitHub mirror: <https://github.com/cran/tidypopgen>

Run `revdepcheck::cloud_details(, "tidypopgen")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     annotate_group_info: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# tidyseurat (0.8.4)

* GitHub: <https://github.com/stemangiola/tidyseurat>
* Email: <mailto:mangiolastefano@gmail.com>
* GitHub mirror: <https://github.com/cran/tidyseurat>

Run `revdepcheck::cloud_details(, "tidyseurat")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
         3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
         4. ├─base::nrow(add_count(pbmc_small, groups))
         5. ├─dplyr::add_count(pbmc_small, groups)
         6. ├─tidyseurat:::add_count.Seurat(pbmc_small, groups)
         7. │ └─... %>% as_meta_data(x)
         8. ├─tidyseurat:::as_meta_data(., x)
         9. │ └─... %>% column_to_rownames(c_(seurat_object)$name)
        10. ├─tibble::column_to_rownames(., c_(seurat_object)$name)
        11. │ ├─base::stopifnot(is.data.frame(.data))
        12. │ └─base::is.data.frame(.data)
        13. ├─dplyr::select_if(., !colnames(.) %in% col_to_exclude)
        14. │ └─rlang::is_logical(.predicate)
        15. ├─colnames(.) %in% col_to_exclude
        16. ├─base::colnames(.)
        17. │ └─base::is.data.frame(x)
        18. ├─dplyr::add_count(...)
        19. └─dplyr:::add_count.data.frame(...)
        20.   └─dplyr:::add_count_impl(...)
        21.     └─lifecycle::deprecate_stop("1.0.0", "add_count(.drop = )", env = error_call)
        22.       └─lifecycle:::deprecate_stop0(msg)
        23.         └─rlang::cnd_signal(...)
       
       [ FAIL 1 | WARN 10 | SKIP 0 | PASS 74 ]
       Error: Test failures
       Execution halted
     ```

# treeheatr (0.2.1)

* GitHub: <https://github.com/trang1618/treeheatr>
* Email: <mailto:grixor@gmail.com>
* GitHub mirror: <https://github.com/cran/treeheatr>

Run `revdepcheck::cloud_details(, "treeheatr")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     position_nodes: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# twfeivdecomp (0.1.0)

* GitHub: <https://github.com/shomiyaji/twfeiv-decomp>
* Email: <mailto:sho.miyaji@yale.edu>
* GitHub mirror: <https://github.com/cran/twfeivdecomp>

Run `revdepcheck::cloud_details(, "twfeivdecomp")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     calculate_mean_linear_projection: no visible binding for global
       variable ‘id’
     calculate_within_between_terms: no visible binding for global variable
       ‘id’
     create_cohort: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# TwoArmSurvSim (0.2)

* Email: <mailto:bzhang@pumabiotechnology.com>
* GitHub mirror: <https://github.com/cran/TwoArmSurvSim>

Run `revdepcheck::cloud_details(, "TwoArmSurvSim")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     randomize_trt: no visible binding for global variable ‘id’
     randomize_trt2: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# ulex (0.1.0)

* Email: <mailto:rmarty@worldbank.org>
* GitHub mirror: <https://github.com/cran/ulex>

Run `revdepcheck::cloud_details(, "ulex")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     locate_event_i: no visible binding for global variable ‘location’
     Undefined global functions or variables:
       location
     ```

## In both

*   checking DESCRIPTION meta-information ... NOTE
     ```
       Missing dependency on R >= 4.1.0 because package code uses the pipe
       |> or function shorthand \(...) syntax added in R 4.1.0.
       File(s) using such syntax:
         ‘augment_gazetteer.Rd’ ‘locate_event.Rd’
     ```

# useful (1.2.6.1)

* GitHub: <https://github.com/jaredlander/useful>
* Email: <mailto:packages@jaredlander.com>
* GitHub mirror: <https://github.com/cran/useful>

Run `revdepcheck::cloud_details(, "useful")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
       Loading required package: ggplot2
       > 
       > test_check("useful")
       Error in detach("package:coefplot", unload = TRUE) : 
         invalid 'name' argument
       [ FAIL 1 | WARN 25 | SKIP 2 | PASS 739 ]
       
       ══ Skipped tests (2) ═══════════════════════════════════════════════════════════
       • On CRAN (2): 'test-load-packages.R:11:5', 'test-load-packages.R:18:5'
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Error ('test-unique.R:7:1'): (code run outside of `test_that()`) ────────────
       <lifecycle_error_deprecated/defunctError/rlang_error/error/condition>
       Error: `as.tbl()` was deprecated in dplyr 1.0.0 and is now defunct.
       ℹ Please use `tibble::as_tibble()` instead.
       Backtrace:
           ▆
        1. └─dplyr::as.tbl(...) at test-unique.R:7:1
        2.   └─lifecycle::deprecate_stop("1.0.0", "as.tbl()", "tibble::as_tibble()")
        3.     └─lifecycle:::deprecate_stop0(msg)
        4.       └─rlang::cnd_signal(...)
       
       [ FAIL 1 | WARN 25 | SKIP 2 | PASS 739 ]
       Error: Test failures
       Execution halted
     ```

# vctrs (0.6.5)

* GitHub: <https://github.com/r-lib/vctrs>
* Email: <mailto:davis@posit.co>
* GitHub mirror: <https://github.com/cran/vctrs>

Run `revdepcheck::cloud_details(, "vctrs")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
       Backtrace:
           ▆
        1. ├─vctrs:::unrownames(dplyr::rowwise(bare_mtcars)) at test-type-dplyr.R:124:3
        2. └─base::loadNamespace(x) at tests/testthat/helper-s3.R:30:3
        3.   ├─base::namespaceImport(...)
        4.   └─base::loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]])
       ── Error ('test-type-dplyr.R:155:3'): can cbind rowwise data frames ────────────
       Error in `loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]])`: namespace 'vctrs' 0.6.5 is already loaded, but >= 0.6.5.9000 is required
       Backtrace:
           ▆
        1. └─base::loadNamespace(x) at test-type-dplyr.R:155:3
        2.   ├─base::namespaceImport(...)
        3.   └─base::loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]])
       ── Error ('test-type-dplyr.R:166:3'): common type between rowwise and grouped data frames is a bare df ──
       Error in `loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]])`: namespace 'vctrs' 0.6.5 is already loaded, but >= 0.6.5.9000 is required
       Backtrace:
           ▆
        1. ├─vctrs::vec_ptype_common(...) at test-type-dplyr.R:166:3
        2. └─base::loadNamespace(x)
        3.   ├─base::namespaceImport(...)
        4.   └─base::loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]])
       
       [ FAIL 13 | WARN 0 | SKIP 308 | PASS 5255 ]
       Error: Test failures
       Execution halted
     ```

## In both

*   checking compiled code ... NOTE
     ```
     File ‘vctrs/libs/vctrs.so’:
       Found non-API calls to R: ‘BODY’, ‘CLOENV’, ‘ENCLOS’, ‘EXTPTR_PROT’,
         ‘EXTPTR_TAG’, ‘FORMALS’, ‘FRAME’, ‘HASHTAB’, ‘IS_S4_OBJECT’,
         ‘LEVELS’, ‘OBJECT’, ‘PRENV’, ‘PRVALUE’, ‘R_PromiseExpr’,
         ‘Rf_allocSExp’, ‘Rf_findVarInFrame3’, ‘SETLENGTH’, ‘SET_BODY’,
         ‘SET_CLOENV’, ‘SET_ENCLOS’, ‘SET_FORMALS’, ‘SET_GROWABLE_BIT’,
         ‘SET_S4_OBJECT’, ‘SET_TRUELENGTH’, ‘STDVEC_DATAPTR’, ‘STRING_PTR’,
         ‘TRUELENGTH’, ‘UNSET_S4_OBJECT’
     
     Compiled code should not call non-API entry points in R.
     
     See ‘Writing portable packages’ in the ‘Writing R Extensions’ manual,
     and section ‘Moving into C API compliance’ for issues with the use of
     non-API entry points.
     ```

# viafr (0.3.2)

* GitHub: <https://github.com/stefanieschneider/viafr>
* Email: <mailto:stefanie.schneider@itg.uni-muenchen.de>
* GitHub mirror: <https://github.com/cran/viafr>

Run `revdepcheck::cloud_details(, "viafr")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     get_source_ids: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# vital (2.0.1)

* GitHub: <https://github.com/robjhyndman/vital>
* Email: <mailto:Rob.Hyndman@monash.edu>
* GitHub mirror: <https://github.com/cran/vital>

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
     Error: C stack usage  9962140 is too close to the limit
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
       Attaching package: 'dplyr'
       
       The following object is masked from 'package:testthat':
       
           matches
       
       The following objects are masked from 'package:stats':
       
           filter, lag
       
       The following objects are masked from 'package:base':
       
           intersect, setdiff, setequal, union
       
       > 
       > test_check("vital")
       [ FAIL 1 | WARN 9 | SKIP 0 | PASS 144 ]
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Failure ('test-coherent-forecast.R:31:3'): Coherent forecasts ───────────────
       mean(fc1$diff) is not strictly less than 0.0021. Difference: NA
       
       [ FAIL 1 | WARN 9 | SKIP 0 | PASS 144 ]
       Error: Test failures
       Execution halted
     ```

# vivainsights (0.7.0)

* GitHub: <https://github.com/microsoft/vivainsights>
* Email: <mailto:martin.chan@microsoft.com>
* GitHub mirror: <https://github.com/cran/vivainsights>

Run `revdepcheck::cloud_details(, "vivainsights")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     network_g2g: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# wakefield (0.3.6)

* GitHub: <https://github.com/trinker/wakefield>
* Email: <mailto:tyler.rinker@gmail.com>
* GitHub mirror: <https://github.com/cran/wakefield>

Run `revdepcheck::cloud_details(, "wakefield")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     Running examples in ‘wakefield-Ex.R’ failed
     The error most likely occurred in:
     
     > ### Name: as_integer
     > ### Title: Convert a Factor Data Frame to Integer
     > ### Aliases: as_integer
     > ### Keywords: integer numeric
     > 
     > ### ** Examples
     > 
     > as_integer(r_series(likert_7, 5, 10))
     Error:
     ! `tbl_df()` was deprecated in dplyr 1.0.0 and is now defunct.
     ℹ Please use `tibble::as_tibble()` instead.
     Backtrace:
         ▆
      1. ├─wakefield::as_integer(r_series(likert_7, 5, 10))
      2. │ └─base::lapply(x, fun)
      3. └─wakefield::r_series(likert_7, 5, 10)
      4.   ├─wakefield::seriesname(dplyr::tbl_df(as.data.frame(out)), attributes(out[[1]])[["varname"]])
      5.   └─dplyr::tbl_df(as.data.frame(out))
      6.     └─lifecycle::deprecate_stop("1.0.0", "tbl_df()", "tibble::as_tibble()")
      7.       └─lifecycle:::deprecate_stop0(msg)
      8.         └─rlang::cnd_signal(...)
     Execution halted
     ```

# WhatsR (1.0.6)

* GitHub: <https://github.com/gesiscss/WhatsR>
* Email: <mailto:julian.kohne@gesis.org>
* GitHub mirror: <https://github.com/cran/WhatsR>

Run `revdepcheck::cloud_details(, "WhatsR")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     Running examples in ‘WhatsR-Ex.R’ failed
     The error most likely occurred in:
     
     > ### Name: download_emoji
     > ### Title: Scraping a dictionary of emoji from https://www.unicode.org/
     > ### Aliases: download_emoji
     > 
     > ### ** Examples
     > 
     > Emoji_dictionary <- download_emoji(nlines = 50)
     Warning in file(con, "r") :
       URL 'https://www.unicode.org/Public/emoji/15.1/emoji-test.txt': Timeout of 60 seconds was reached
     Error in file(con, "r") : 
       cannot open the connection to 'https://www.unicode.org/Public/emoji/15.1/emoji-test.txt'
     Calls: download_emoji -> readLines -> file
     Execution halted
     ```

## Newly fixed

*   checking tests ... ERROR
     ```
       Running ‘testthat.R’
     Running the tests in ‘tests/testthat.R’ failed.
     Complete output:
       > library(testthat)
       > library(WhatsR)
       > 
       > test_check("WhatsR")
       'download_emoji()' caused a warning:
       URL 'https://www.unicode.org/Public/emoji/15.1/emoji-test.txt': Timeout of 60 seconds was reachedLoading required package: ragg
       [ FAIL 2 | WARN 5 | SKIP 0 | PASS 291 ]
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Failure ('test-WhatsR-tests.R:33:3'): Updating emoji dictionary ─────────────
       class(emoji_dictionary) (`actual`) not equal to "data.frame" (`expected`).
       
       `actual`:   "NULL"      
       `expected`: "data.frame"
       ── Failure ('test-WhatsR-tests.R:36:3'): Updating emoji dictionary ─────────────
       Names of `emoji_dictionary` ('') don't match 'R.native', 'Desc', 'OriginalOrder'
       
       [ FAIL 2 | WARN 5 | SKIP 0 | PASS 291 ]
       Error: Test failures
       Execution halted
     ```

# wpa (1.10.0)

* GitHub: <https://github.com/microsoft/wpa>
* Email: <mailto:martin.chan@microsoft.com>
* GitHub mirror: <https://github.com/cran/wpa>

Run `revdepcheck::cloud_details(, "wpa")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     g2g_network: no visible binding for global variable ‘id’
     meetingtype_dist_ca: no visible binding for global variable ‘id’
     meetingtype_dist_mt: no visible binding for global variable ‘id’
     network_g2g: no visible binding for global variable ‘id’
     plot_workpatterns_classify_bw: no visible binding for global variable
       ‘id’
     subject_scan: no visible binding for global variable ‘id’
     tm_scan: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# WRTDStidal (1.1.4)

* GitHub: <https://github.com/fawda123/WRTDStidal>
* Email: <mailto:mbeck@tbep.org>
* GitHub mirror: <https://github.com/cran/WRTDStidal>

Run `revdepcheck::cloud_details(, "WRTDStidal")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     wrtdsperf.tidal: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

# xlr (1.0.3)

* GitHub: <https://github.com/NHilder/xlr>
* Email: <mailto:nhilderson.code@gmail.com>
* GitHub mirror: <https://github.com/cran/xlr>

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

# ypssc (1.1.0)

* Email: <mailto:shashank.kumbhare@mavs.uta.edu>
* GitHub mirror: <https://github.com/cran/ypssc>

Run `revdepcheck::cloud_details(, "ypssc")` for more info

## Newly broken

*   checking R code for possible problems ... NOTE
     ```
     calculationAlphaHelix: no visible binding for global variable ‘id’
     calculationBetaSheet: no visible binding for global variable ‘id’
     calculationChain: no visible binding for global variable ‘id’
     Undefined global functions or variables:
       id
     ```

## In both

*   checking dependencies in R code ... NOTE
     ```
     Namespace in Imports field not imported from: ‘spelling’
       All declared Imports should be used.
     ```

# ZIBR (1.0.2)

* GitHub: <https://github.com/PennChopMicrobiomeProgram/ZIBR>
* Email: <mailto:ctbushman@gmail.com>
* GitHub mirror: <https://github.com/cran/ZIBR>

Run `revdepcheck::cloud_details(, "ZIBR")` for more info

## Newly broken

*   checking re-building of vignette outputs ... ERROR
     ```
     Error(s) in re-building vignettes:
       ...
     --- re-building ‘zibr.Rmd’ using rmarkdown
     
     Quitting from zibr.Rmd:169-286 [unnamed-chunk-11]
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     NULL
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'zibr.Rmd' failed with diagnostics:
     `add_rownames()` was deprecated in dplyr 1.0.0 and is now defunct.
     ℹ Please use `tibble::rownames_to_column()` instead.
     --- failed re-building ‘zibr.Rmd’
     
     SUMMARY: processing the following file failed:
       ‘zibr.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

