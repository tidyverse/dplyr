#' Extract out common by variables
#'
#' @export
#' @keywords internal
common_by <- function(by = NULL, x, y) UseMethod("common_by", by)

#' @export
common_by.character <- function(by, x, y) {
  by <- common_by_from_vector(by)
  common_by.list(by, x, y)
}

common_by_from_vector <- function(by) {
  by <- by[!duplicated(by)]
  by_x <- names(by) %||% by
  by_y <- unname(by)

  # If x partially named, assume unnamed are the same in both tables
  by_x[by_x == ""] <- by_y[by_x == ""]

  list(x = by_x, y = by_y)
}

#' @export
common_by.list <- function(by, x, y) {
  x_vars <- tbl_vars(x)
  if (!all(by$x %in% x_vars)) {
    bad_args("by", "can't contain join column {missing} which is missing from LHS.",
      missing = fmt_obj(setdiff(by$x, x_vars))
    )
  }

  y_vars <- tbl_vars(y)
  if (!all(by$y %in% y_vars)) {
    bad_args("by", "can't contain join column {missing} which is missing from RHS.",
      missing = fmt_obj(setdiff(by$y, y_vars))
    )
  }

  by
}

#' @export
common_by.NULL <- function(by, x, y) {
  by <- intersect(tbl_vars(x), tbl_vars(y))
  by <- by[!is.na(by)]
  if (length(by) == 0) {
    bad_args("by", "required, because the data sources have no common variables.")
  }
  inform(auto_by_msg(by))

  list(
    x = by,
    y = by
  )
}

auto_by_msg <- function(by) {
  by_quoted <- encodeString(by, quote = '"')
  if (length(by_quoted) == 1L) {
    by_code <- by_quoted
  } else {
    by_code <- paste0("c(", paste(by_quoted, collapse = ", "), ")")
  }
  paste0("Joining, by = ", by_code)
}

#' @export
common_by.default <- function(by, x, y) {
  bad_args("by", "must be a (named) character vector, list, or NULL for ",
    "natural joins (not recommended in production code), not {friendly_type_of(by)}."
  )
}
