
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
