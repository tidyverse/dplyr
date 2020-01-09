#' Grouping metadata
#'
#' @description
#' * `group_data()`: data frame that defines the grouping structure.
#'   The last column, always called `.rows`, is a list of integer vectors that
#'   gives the location of the rows in each group.
#'
#' * `group_data()`: just the grouping structure from `group_data()`.
#'
#' * `group_rows()`: just row locations from `group_data()`
#'
#' * `group_vars()`: names of grouping variables as character vector
#'
#' * `groups()`: names of grouping as list of symbols
#'
#' * `group_size()`: vector giving size of each group
#'
#' * `n_groups()`: total number of groups
#'
#' @param .data,.tbl,x A data frame or extension (like a tibble or grouped tibble).
#' @examples
#' df <- tibble(x = c(1,1,2,2))
#' group_vars(df)
#' group_rows(df)
#' group_data(df)
#'
#' gf <- group_by(df, x)
#' group_vars(gf)
#' group_rows(gf)
#' group_data(gf)
#' @export
group_data <- function(.data) {
  UseMethod("group_data")
}

#' @export
group_data.data.frame <- function(.data) {
  out <- vec_init(.data[0], 1)
  rownames(out) <- NULL
  out$.rows <- list_of(seq_len(nrow(.data)), .ptype = integer())
  out
}

#' @export
group_data.rowwise_df <- function(.data) {
  rows <- new_list_of(as.list(seq_len(nrow(.data))), ptype = integer())
  tibble(".rows" := rows)
}

#' @export
group_data.grouped_df <- function(.data) {
  attr(validate_grouped_df(.data), "groups")
}

#' @rdname group_data
#' @export
group_keys <- function(.tbl, ...) {
  if (!missing(...)) {
    abort("Use of `...` is deprecated; please `group_by()` first.")
  }
  UseMethod("group_keys")
}

#' @export
group_keys.data.frame <- function(.tbl, ...){
  out <- group_data(.tbl)
  attr(out, ".drop") <- NULL
  out[-length(out)]
}

#' @rdname group_data
#' @export
group_rows <- function(.data) {
  group_data(.data)[[".rows"]]
}

#' @export
#' @rdname group_data
group_vars <- function(x) {
  UseMethod("group_vars")
}

#' @export
group_vars.data.frame <- function(x) {
  setdiff(names(group_data(x)), ".rows")
}

#' @export
#' @rdname group_data
groups <- function(x) {
  UseMethod("groups")
}

#' @export
groups.data.frame <- function(x) {
  syms(group_vars(x))
}

#' @export
#' @rdname group_data
group_size <- function(x) UseMethod("group_size")

#' @export
group_size.data.frame <- function(x) {
  lengths(group_rows(x))
}

#' @export
#' @rdname group_data
n_groups <- function(x) UseMethod("n_groups")

#' @export
n_groups.data.frame <- function(x) {
  nrow(group_data(x))
}
