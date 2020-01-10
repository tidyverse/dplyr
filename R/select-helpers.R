#' Select grouping variables
#'
#' This selection helpers matches grouping variables. It can be used
#' in [select()] or [vars()][scoped] selections.
#'
#' @inheritParams tidyselect::select_helpers
#' @seealso [groups()] and [group_vars()] for retrieving the grouping
#'   variables outside selection contexts.
#'
#' @examples
#' gdf <- iris %>% group_by(Species)
#'
#' # Select the grouping variables:
#' gdf %>% select(group_cols())
#'
#' # Remove the grouping variables from mutate selections:
#' gdf %>% mutate_at(vars(-group_cols()), `/`, 100)
#' @export
group_cols <- function(vars = peek_vars()) {
  if (is_sel_vars(vars)) {
    matches <- match(vars %@% groups, vars)
    if (anyNA(matches)) {
      abort("Can't find the grouping variables")
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
