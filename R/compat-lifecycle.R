# nocov start - compat-lifecycle (last updated: rlang 0.3.0.9000)

# This file serves as a reference for currently unexported rlang
# lifecycle functions. Please find the most recent version in rlang's
# repository. These functions require rlang in your `Imports`
# DESCRIPTION field but you don't need to import rlang in your
# namespace.


#' Signal deprecation
#'
#' @description
#'
#' These functions provide two levels of verbosity for deprecation
#' warnings.
#'
#' * `signal_soft_deprecated()` warns only if called from the global
#'   environment (so the user can change their script) or from the
#'   package currently being tested (so the package developer can fix
#'   the package).
#'
#' * `warn_deprecated()` warns unconditionally.
#'
#' * `stop_defunct()` fails unconditionally.
#'
#' Both functions warn only once per session by default to avoid
#' overwhelming the user with repeated warnings.
#'
#' @param msg The deprecation message.
#' @param id The id of the deprecation. A warning is issued only once
#'   for each `id`. Defaults to `msg`, but you should give a unique ID
#'   when the message is built programmatically and depends on inputs.
#' @param env The environment in which the soft-deprecated function
#'   was called. A warning is issued if called from the global
#'   environment. If testthat is running, a warning is also called if
#'   the retired function was called from the package being tested.
#'
#' @section Controlling verbosity:
#'
#' The verbosity of retirement warnings can be controlled with global
#' options. You'll generally want to set these options locally with
#' one of these helpers:
#'
#' * `with_lifecycle_silence()` disables all soft-deprecation and
#'   deprecation warnings.
#'
#' * `with_lifecycle_warnings()` enforces warnings for both
#'   soft-deprecated and deprecated functions. The warnings are
#'   repeated rather than signalled once per session.
#'
#' * `with_lifecycle_errors()` enforces errors for both
#'   soft-deprecated and deprecated functions.
#'
#' All the `with_` helpers have `scoped_` variants that are
#' particularly useful in testthat blocks.
#'
#' @noRd
#' @seealso [lifecycle()]
NULL

signal_soft_deprecated <- function(msg, id = msg, env = caller_env(2)) {
  if (rlang::is_true(rlang::peek_option("lifecycle_disable_warnings"))) {
    return(invisible(NULL))
  }

  if (rlang::is_true(rlang::peek_option("lifecycle_verbose_soft_deprecation")) ||
      rlang::is_reference(topenv(env), rlang::global_env())) {
    warn_deprecated(msg, id)
    return(invisible(NULL))
  }

  # Test for environment names rather than reference/contents because
  # testthat clones the namespace
  tested_package <- Sys.getenv("TESTTHAT_PKG")
  if (nzchar(tested_package) &&
        identical(Sys.getenv("NOT_CRAN"), "true") &&
        rlang::env_name(topenv(env)) == rlang::env_name(ns_env(tested_package))) {
    warn_deprecated(msg, id)
    return(invisible(NULL))
  }

  rlang::signal(msg, "lifecycle_soft_deprecated")
}

warn_deprecated <- function(msg, id = msg) {
  if (rlang::is_true(rlang::peek_option("lifecycle_disable_warnings"))) {
    return(invisible(NULL))
  }

  if (!rlang::is_true(rlang::peek_option("lifecycle_repeat_warnings")) &&
        rlang::env_has(deprecation_env, id)) {
    return(invisible(NULL))
  }

  rlang::env_poke(deprecation_env, id, TRUE);

  has_colour <- function() rlang::is_installed("crayon") && crayon::has_color()
  silver <- function(x) if (has_colour()) crayon::silver(x) else x

  if (rlang::is_true(rlang::peek_option("lifecycle_warnings_as_errors"))) {
    signal <- .Defunct
  } else {
    signal <- .Deprecated
  }

  signal(msg = paste0(
    msg,
    "\n",
    silver("This warning is displayed once per session.")
  ))
}
deprecation_env <- new.env(parent = emptyenv())

stop_defunct <- function(msg) {
  .Defunct(msg = msg)
}

scoped_lifecycle_silence <- function(frame = rlang::caller_env()) {
  rlang::scoped_options(.frame = frame,
    lifecycle_disable_warnings = TRUE
  )
}
with_lifecycle_silence <- function(expr) {
  scoped_lifecycle_silence()
  expr
}

scoped_lifecycle_warnings <- function(frame = rlang::caller_env()) {
  rlang::scoped_options(.frame = frame,
    lifecycle_disable_warnings = FALSE,
    lifecycle_verbose_soft_deprecation = TRUE,
    lifecycle_repeat_warnings = TRUE
  )
}
with_lifecycle_warnings <- function(expr) {
  scoped_lifecycle_warnings()
  expr
}

scoped_lifecycle_errors <- function(frame = rlang::caller_env()) {
  scoped_lifecycle_warnings(frame = frame)
  rlang::scoped_options(.frame = frame,
    lifecycle_warnings_as_errors = TRUE
  )
}
with_lifecycle_errors <- function(expr) {
  scoped_lifecycle_errors()
  expr
}


#' Embed a lifecycle badge in documentation
#'
#' @description
#'
#' Use `lifecycle()` within a `Sexpr` macro to embed a
#' [lifecycle](https://www.tidyverse.org/lifecycle/) badge in your
#' documentation. The badge should appear first in the description:
#'
#' ```
#' \Sexpr[results=rd, stage=render]{mypkg:::lifecycle("questioning")}
#' ```
#'
#' The badge appears as an image in the HTML version of the
#' documentation. To make them available in your package, visit
#' <https://github.com/r-lib/rlang/tree/master/man/figures> and copy
#' all the files starting with `lifecycle-` in your `man/figures/`
#' folder.
#'
#' @param stage A lifecycle stage as a string, one of:
#'   `"experimental"`, `"maturing"`, `"stable"`, `"questioning"`,
#'   `"archived"`, `"soft-deprecated"`, `"deprecated"`, `"defunct"`.
#'
#' @noRd
NULL

lifecycle <- function(stage) {
  url <- paste0("https://www.tidyverse.org/lifecycle/#", stage)
  img <- lifecycle_img(stage, url)

  sprintf(
    "\\ifelse{html}{%s}{\\strong{%s}}",
    img,
    upcase1(stage)
  )
}

lifecycle_img <- function(stage, url) {
  file <- sprintf("lifecycle-%s.svg", stage)
  stage_alt <- upcase1(stage)

  switch(stage,

    experimental = ,
    maturing = ,
    stable = ,
    questioning = ,
    retired =
      sprintf(
        "\\out{<a href='%s'><img src='%s' alt='%s lifecycle'></a>}",
        url,
        file.path("figures", file),
        stage_alt
      )
   ,

    `soft-deprecated` = ,
    deprecated = ,
    defunct =
      sprintf(
        "\\figure{%s}{options: alt='%s lifecycle'}",
        file,
        stage_alt
      ),

    rlang::abort(sprintf("Unknown lifecycle stage `%s`", stage))

  )
}
upcase1 <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}


# nocov end
