# nocov start --- r-lib/rlang compat-types-check
#
# Dependencies
# ============
#
# - compat-obj-type.R
#
# Changelog
# =========
#
# 2022-08-11:
# - Added changelog.

# Scalars -----------------------------------------------------------------

check_bool <- function(x,
                       ...,
                       what = "`TRUE` or `FALSE`",
                       arg = rlang::caller_arg(x),
                       call = rlang::caller_env()) {
  if (!rlang::is_bool(x)) {
    stop_input_type(x, what, ..., arg = arg, call = call)
  }
}

check_string <- function(x,
                         ...,
                         what = "a single string",
                         arg = rlang::caller_arg(x),
                         call = rlang::caller_env()) {
  if (!rlang::is_string(x)) {
    stop_input_type(x, what, ..., arg = arg, call = call)
  }
}

check_number <- function(x,
                         ...,
                         what = "a round number",
                         arg = rlang::caller_arg(x),
                         call = rlang::caller_env()) {
  if (!is_number(x)) {
    stop_input_type(x, what, ..., arg = arg, call = call)
  }
}

is_number <- function(x) {
  rlang::is_integerish(x, n = 1, finite = TRUE)
}

check_symbol <- function(x,
                         ...,
                         what = "a symbol",
                         arg = rlang::caller_arg(x),
                         call = rlang::caller_env()) {
  if (!rlang::is_symbol(x)) {
    stop_input_type(x, what, ..., arg = arg, call = call)
  }
}

check_arg <- function(x,
                      ...,
                      what = "an argument name",
                      arg = rlang::caller_arg(x),
                      call = rlang::caller_env()) {
  check_symbol(x, ..., what = what, arg = arg, call = call)
}

check_call <- function(x,
                       ...,
                       what = "a defused call",
                       arg = rlang::caller_arg(x),
                       call = rlang::caller_env()) {
  if (!rlang::is_call(x)) {
    stop_input_type(x, what, ..., arg = arg, call = call)
  }
}

check_environment <- function(x,
                              ...,
                              what = "an environment",
                              arg = rlang::caller_arg(x),
                              call = rlang::caller_env()) {
  if (!rlang::is_environment(x)) {
    stop_input_type(x, what, ..., arg = arg, call = call)
  }
}

check_function <- function(x,
                           ...,
                           what = "a function",
                           arg = rlang::caller_arg(x),
                           call = rlang::caller_env()) {
  if (!rlang::is_function(x)) {
    stop_input_type(x, what, ..., arg = arg, call = call)
  }
}

check_closure <- function(x,
                           ...,
                           what = "an R function",
                           arg = rlang::caller_arg(x),
                           call = rlang::caller_env()) {
  if (!rlang::is_closure(x)) {
    stop_input_type(x, what, ..., arg = arg, call = call)
  }
}

check_formula <- function(x,
                          ...,
                          what = "a formula",
                          arg = rlang::caller_arg(x),
                          call = rlang::caller_env()) {
  if (!rlang::is_formula(x)) {
    stop_input_type(x, what, ..., arg = arg, call = call)
  }
}


# Vectors -----------------------------------------------------------------

# TODO: Restrict missing and special values

check_character <- function(x,
                            ...,
                            what = "a character vector",
                            arg = rlang::caller_arg(x),
                            call = rlang::caller_env()) {
  if (!rlang::is_character(x)) {
    stop_input_type(x, what, ..., arg = arg, call = call)
  }
}

# nocov end
