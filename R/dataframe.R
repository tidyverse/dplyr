# Grouping methods ------------------------------------------------------------

#' Convert to a data frame
#'
#' Functions that convert the input to a \code{data_frame}.
#'
#' @details For a grouped data frame, the \code{\link[tibble]{as_data_frame}}
#' S3 generic simply removes the grouping.
#'
#' @inheritParams tibble::as_data_frame
#' @seealso \code{\link[tibble]{as_data_frame}}
#' @name grouped_df
#' @export
as_data_frame.grouped_df <- function(x, ...) {
  x <- ungroup(x)
  class(x) <- c("tbl_df", "tbl", "data.frame")
  x
}

#' Convert row names to an explicit variable.
#'
#' Deprecated, use \code{\link[tibble]{rownames_to_column}} instead.
#'
#' @param df Input data frame with rownames.
#' @param var Name of variable to use
#' @export
#' @examples
#' mtcars %>% tbl_df()
#'
#' mtcars %>% add_rownames()
add_rownames <- function(df, var = "rowname") {
  warning(
    "Deprecated, use tibble::rownames_to_column() instead.",
    call. = FALSE)

  stopifnot(is.data.frame(df))

  rn <- as_data_frame(setNames(list(rownames(df)), var))
  rownames(df) <- NULL

  bind_cols(rn, df)
}

# Grouping methods ------------------------------------------------------------

#' @export
group_by_.data.frame <- function(.data, ..., .dots, add = FALSE) {
  groups <- group_by_prepare(.data, ..., .dots = .dots, add = add)
  grouped_df(groups$data, groups$groups)
}

#' @export
groups.data.frame <- function(x) NULL

#' @export
ungroup.data.frame <- function(x, ...) x

#' @export
group_size.data.frame <- function(x) nrow(x)

#' @export
n_groups.data.frame <- function(x) 1L

# Manipulation functions ------------------------------------------------------

# These could potentially be rewritten to avoid any copies, but since this
# is just a convenience layer, I didn't bother. They should still be fast.

#' @export
filter_.data.frame <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  as.data.frame(filter_(tbl_df(.data), .dots = dots))
}
#' @export
slice_.data.frame <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  as.data.frame(slice_(tbl_df(.data), .dots = dots))
}
#' @export
summarise_.data.frame <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  as.data.frame(summarise_(tbl_df(.data), .dots = dots))
}
#' @export
mutate_.data.frame <-  function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  as.data.frame(mutate_(tbl_df(.data), .dots = dots))
}
#' @export
arrange_.data.frame <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  as.data.frame(arrange_(tbl_df(.data), .dots = dots))
}
#' @export
select_.data.frame <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  vars <- select_vars_(names(.data), dots)
  select_impl(.data, vars)
}
#' @export
rename_.data.frame <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  vars <- rename_vars_(names(.data), dots)
  select_impl(.data, vars)
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
right_join.data.frame <- function(x, y, by = NULL, copy = FALSE, ...) {
  as.data.frame(right_join(tbl_df(x), y, by = by, copy = copy, ...))
}

#' @export
full_join.data.frame <- function(x, y, by = NULL, copy = FALSE, ...) {
  as.data.frame(full_join(tbl_df(x), y, by = by, copy = copy, ...))
}

#' @export
semi_join.data.frame <- function(x, y, by = NULL, copy = FALSE, ...) {
  as.data.frame(semi_join(tbl_df(x), y, by = by, copy = copy, ...))
}

#' @export
anti_join.data.frame <- function(x, y, by = NULL, copy = FALSE, ...) {
  as.data.frame(anti_join(tbl_df(x), y, by = by, copy = copy, ...))
}

# Set operations ---------------------------------------------------------------

#' @export
intersect.data.frame <- function(x, y, ...) intersect_data_frame(x, y)

#' @export
union.data.frame <-     function(x, y, ...) union_data_frame(x, y)

#' @export
union_all.data.frame <- function(x, y, ...) bind_rows(x, y)

#' @export
setdiff.data.frame <-   function(x, y, ...) setdiff_data_frame(x, y)

#' @export
setequal.data.frame <-  function(x, y, ...) equal_data_frame(x, y)

#' @export
distinct_.data.frame <- function(.data, ..., .dots, .keep_all = FALSE) {
  dist <- distinct_vars(.data, ..., .dots = .dots, .keep_all = .keep_all)
  distinct_impl(dist$data, dist$vars, dist$keep)
}


# Do ---------------------------------------------------------------------------

#' @export
do_.data.frame <- function(.data, ..., .dots) {
  args <- lazyeval::all_dots(.dots, ...)
  named <- named_args(args)

  data <- list(. = .data)

  if (!named) {
    env <- new.env(parent = args[[1]]$env)
    env$. <- .data

    out <- lazyeval::lazy_eval(args[[1]], data)
    if (!is.data.frame(out)) {
      stop("Result must be a data frame", call. = FALSE)
    }
  } else {
    out <- lapply(args, function(arg) {
      list(lazyeval::lazy_eval(arg, data))
    })
    names(out) <- names(args)
    attr(out, "row.names") <- .set_row_names(1L)
    # Use tbl_df to ensure safe printing of list columns
    class(out) <- c("tbl_df", "data.frame")
  }

  out
}

# Random samples ---------------------------------------------------------------


#' @export
sample_n.data.frame <- function(tbl, size, replace = FALSE, weight = NULL,
  .env = parent.frame()) {
  if (!missing(weight)) {
    weight <- eval(substitute(weight), tbl, .env)
  }

  sample_n_basic(tbl, size, replace = replace, weight = weight)
}


#' @export
sample_frac.data.frame <- function(tbl, size = 1, replace = FALSE, weight = NULL,
  .env = parent.frame()) {

  if (!missing(weight)) {
    weight <- eval(substitute(weight), tbl, .env)
  }

  sample_n_basic(tbl, round(size * nrow(tbl)), replace = replace, weight = weight)
}

sample_n_basic <- function(tbl, size, replace = FALSE, weight = NULL) {
  n <- nrow(tbl)

  weight <- check_weight(weight, n)
  assert_that(is.numeric(size), length(size) == 1, size >= 0)
  check_size(size, n, replace)

  idx <- sample.int(n, size, replace = replace, prob = weight)
  tbl[idx, , drop = FALSE]
}



# Misc -------------------------------------------------------------------------

#' @export
collect.data.frame <- function(x, ...) x
#' @export
compute.data.frame <- function(x, ...) x
#' @export
collapse.data.frame <- function(x, ...) x
