# Grouping methods ------------------------------------------------------------

#' @export
regroup.data.frame <- function(x, value) {
  grouped_df(x, value)
}

#' @export
groups.data.frame <- function(x) NULL

#' @export
ungroup.data.frame <- function(x) x

# Manipulation functions ------------------------------------------------------

# These could potentially be rewritten to avoid any copies, but since this
# is just a convenience layer, I didn't bother. They should still be fast.

#' @export
filter.data.frame <- function(.data, ...) {
  as.data.frame(filter(tbl_df(.data), ...))
}
#' @export
summarise.data.frame <- function(.data, ...) {
  as.data.frame(summarise(tbl_df(.data), ...))
}
#' @export
mutate.data.frame <-  function(.data, ...) {
  tbl <- tbl_df(.data)
  res <- mutate.tbl_df(tbl, ...)
  as.data.frame(res)
}
#' @export
arrange.data.frame <- function(.data, ...) {
  as.data.frame(arrange(tbl_df(.data), ...))
}
#' @export
select.data.frame <- function(.data, ...) {
  vars <- select_vars(names(.data), ..., env = parent.frame())
  select_impl(.data, vars)
}
#' @export
do.data.frame <- function(.data, ...) {
  args <- dots(...)
  named <- named_args(args)

  env <- new.env(parent = parent.frame())
  env$. <- .data

  if (!named) {
    out <- eval(args[[1]], envir = env)
    if (!is.data.frame(out)) {
      stop("Result must be a data frame", call. = FALSE)
    }
  } else {
    out <- lapply(args, function(arg) list(eval(arg, envir = env)))
    names(out) <- names(args)
    attr(out, "row.names") <- .set_row_names(1L)
    # Use tbl_df to ensure safe printing of list columns
    class(out) <- c("tbl_df", "data.frame")
  }

  out
}

# Joins ------------------------------------------------------------------------

#' @export
inner_join.data.frame <- function(x, y, by = NULL, copy = FALSE, ...) {
  as.data.frame(inner_join(tbl_df(x), y, by = by, copy = copy, ...))
}

#' @export
left_join.data.frame <- function(x, y, by = NULL, copy = FALSE, ...) {
  as.data.frame(left_join(tbl_df(x), y, by = by, copy = copy, ...))
}

#' @export
semi_join.data.frame <- function(x, y, by = NULL, copy = FALSE, ...) {
  as.data.frame(semi_join(tbl_df(x), y, by = by, copy = copy, ...))
}

#' @export
anti_join.data.frame <- function(x, y, by = NULL, copy = FALSE, ...) {
  as.data.frame(anti_join(tbl_df(x), y, by = by, copy = copy, ...))
}

# Misc -------------------------------------------------------------------------

#' @export
collect.data.frame <- function(x, ...) x
#' @export
compute.data.frame <- function(x, ...) x
#' @export
collapse.data.frame <- function(x, ...) x
