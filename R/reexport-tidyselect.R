
# Flag to disable hotpatching from old tidyselect versions
peek_vars <- tidyselect::peek_vars


# Alias required for help links in downstream packages
#' @aliases select_helpers
#' @importFrom tidyselect contains
#' @export
tidyselect::contains
#' @importFrom tidyselect ends_with
#' @export
tidyselect::ends_with
#' @importFrom tidyselect everything
#' @export
tidyselect::everything
#' @importFrom tidyselect matches
#' @export
tidyselect::matches
#' @importFrom tidyselect num_range
#' @export
tidyselect::num_range
#' @importFrom tidyselect one_of
#' @export
tidyselect::one_of
#' @importFrom tidyselect starts_with
#' @export
tidyselect::starts_with
#' @importFrom tidyselect last_col
#' @export
tidyselect::last_col


#' Select variables
#'
#' \Sexpr[results=rd, stage=render]{dplyr:::lifecycle("deprecated")}
#'
#' @description
#'
#' **Retired**: These functions now live in the tidyselect package as
#' [tidyselect::vars_select()], [tidyselect::vars_rename()] and
#' [tidyselect::vars_pull()]. These dplyr aliases are soft-deprecated
#' and will be deprecated sometimes in the future.
#'
#' @param vars A character vector of existing column names.
#' @param ... Expressions to compute.
#' @param include,exclude Character vector of column names to always
#'   include/exclude.
#' @param strict If `TRUE`, will throw an error if you attempt to
#'   rename a variable that doesn't exist.
#' @param var A variable specified as in the same argument of
#'   [tidyselect::vars_pull()].
#' @export
select_vars <- function(vars = chr(), ..., include = chr(), exclude = chr()) {
  warn_deprecated(paste_line(
    "select_vars() is deprecated. ",
    "Please use tidyselect::vars_select() instead"
  ))

  tidyselect::vars_select(.vars = vars, ..., .include = include, .exclude = exclude)
}
#' @rdname select_vars
#' @inheritParams tidyselect::vars_rename
#' @export
rename_vars <- function(vars = chr(), ..., strict = TRUE) {
  warn_deprecated(paste_line(
    "rename_vars() is deprecated. ",
    "Please use tidyselect::vars_rename() instead"
  ))
  tidyselect::vars_rename(.vars = vars, ..., .strict = strict)
}
#' @rdname select_vars
#' @inheritParams tidyselect::vars_pull
#' @export
select_var <- function(vars, var = -1) {
  warn_deprecated(paste_line(
    "select_var() is deprecated. ",
    "Please use tidyselect::vars_pull() instead"
  ))
  tidyselect::vars_pull(vars, !!enquo(var))
}
#' @rdname select_vars
#' @export
current_vars <- function(...) {
  warn_deprecated(paste_line(
    "current_vars() is deprecated. ",
    "Please use tidyselect::peek_vars() instead"
  ))
  tidyselect::peek_vars(...)
}


#' @rdname se-deprecated
#' @param include,exclude Character vector of column names to always
#'   include/exclude.
#' @export
select_vars_ <- function(vars, args, include = chr(), exclude = chr()) {
  warn_deprecated(paste_line(
    "select_vars_() is deprecated. ",
    "Please use tidyselect::vars_select() instead"
  ))

  args <- compat_lazy_dots(args, caller_env())
  select_vars(vars, !!!args, include = include, exclude = exclude)
}
#' @export
#' @rdname se-deprecated
rename_vars_ <- function(vars, args) {
  warn_deprecated(paste_line(
    "rename_vars_() is deprecated. ",
    "Please use tidyselect::vars_rename() instead"
  ))
  args <- compat_lazy_dots(args, caller_env())
  rename_vars(vars, !!!args)
}
