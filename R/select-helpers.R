#' Select grouping variables
#'
#' This selection helpers matches grouping variables. It can be used
#' in [select()] or [vars()] selections.
#'
#' @param data For advanced use only. The default `NULL` automatically
#'   finds the "current" data frames.
#' @param vars Deprecated; please use data instead.
#' @seealso [groups()] and [group_vars()] for retrieving the grouping
#'   variables outside selection contexts.
#'
#' @examples
#' gdf <- iris %>% group_by(Species)
#' gdf %>% select(group_cols())
#'
#' # Remove the grouping variables from mutate selections:
#' gdf %>% mutate_at(vars(-group_cols()), `/`, 100)
#' # -> No longer necessary with across()
#' gdf %>% mutate(across(everything(), ~ . / 100))
#' @export
group_cols <- function(vars = NULL, data = NULL) {
  # So group_cols() continues to work in _at() helpers.
  data <- data %||% tryCatch(tidyselect::peek_data(), error = function(e) NULL)

  if (!is.null(data)) {
    match(group_vars(data), tbl_vars(data))
  } else {
    group_cols_legacy(vars)
  }
}

group_cols_legacy <- function(vars = NULL) {
  if (!is.null(vars)) {
    lifecycle::deprecate_warn(
      "1.0.0", "group_cols(vars = )",
      details = "Use `data` with entire dataframe instead"
    )
  }

  vars <- vars %||% tidyselect::peek_vars()
  if (is_sel_vars(vars)) {
    matches <- match(vars %@% groups, vars)
    if (anyNA(matches)) {
      abort("Can't find the grouping variables.")
    }
    matches
  } else {
    int()
  }
}

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
#' @importFrom tidyselect any_of
#' @export
tidyselect::any_of
#' @importFrom tidyselect all_of
#' @export
tidyselect::all_of
