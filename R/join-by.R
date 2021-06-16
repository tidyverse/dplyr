#' Join specifications
#'
#' @description
#' `join_by()` constructs a specification to join by using a small domain
#' specific language. The result can be supplied as the `by` argument to any
#' of the join functions (such as [left_join()]). `join_by()` supports
#' specifications for equi joins, non-equi joins, and rolling joins.
#'
#' ## Equi Joins:
#'
#' Equi joins match on equality, and are the most common type of join. To
#' construct an equi join, supply two column names to join with separated by
#' `==`. Alternatively, supplying a single name will be interpreted as an equi
#' join between two columns of the same name.
#'
#' ## Non-equi Joins:
#'
#' Non-equi joins match on an inequality, and are common in time series analysis
#' and genomics. To construct a non-equi join, supply two column names separated
#' by `>`, `>=`, `<`, or `<=`.
#'
#' ## Rolling Joins:
#'
#' Rolling joins are a variant of a non-equi join that limit the results
#' returned from each condition to either the maximum or the minimum of
#' the matches. To construct a rolling join, wrap a non-equi join condition
#' in `max()` or `min()`, such as `max(x > y)`.
#'
#' @param ... Expressions specifying the join. Each expression should consist
#'   of:
#'   - A join condition, one of: `==`, `>`, `>=`, `<`, or `<=`.
#'   - A quoted or unquoted column name on the left-hand side of the join
#'     condition.
#'   - A quoted or unquoted column name on the right-hand side of the join
#'     condition.
#'   - Optionally, the entire join condition can be wrapped in `max()` or
#'     `min()` to specify a rolling join.
#'   - If a single column name is provided without any join conditions, it
#'     is interpreted as if that column name was provided on each side of `==`,
#'     i.e. `x` is interpreted as `x == x`.
#'
#' @export
#' @examples
#' sales <- tibble(
#'  id = c(1L, 1L, 1L, 2L, 2L),
#'  sale_date = as.Date(c("2018-12-31", "2019-01-02", "2019-01-05", "2019-01-04", "2019-01-01"))
#' )
#'
#' promos <- tibble(
#'  id = c(1L, 1L, 2L),
#'  promo_date = as.Date(c("2019-01-01", "2019-01-05", "2019-01-03"))
#' )
#'
#' # "Match id to id, and sales_date to promo_date"
#' join_by(id, sales_date == promo_date)
#'
#' # "For each sales_date within a particular id, find all promo_dates that
#' # occurred before this particular sale"
#' join_by(id, sales_date >= promo_date)
#'
#' # "For each sales_date within a particular id, find only the most recent
#' # promo_date that occurred before this particular sale"
#' join_by(id, max(sales_date >= promo_date))
join_by <- function(...) {
  # Should use `enexprs(.named = NULL)`, but https://github.com/r-lib/rlang/issues/1223
  exprs <- enexprs(...)

  if (!is_empty(exprs) && is_named(exprs)) {
    abort(c(
      "`join_by()` expressions can't be named.",
      i = "Did you use `=` rather than `==`?"
    ))
  }

  n <- length(exprs)

  condition <- vector("character", length = n)
  filter <- vector("character", length = n)
  x <- vector("character", length = n)
  y <- vector("character", length = n)

  for (i in seq_len(n)) {
    expr <- exprs[[i]]
    info <- parse_join_by_expr(expr, i)
    condition[[i]] <- info$condition
    filter[[i]] <- info$filter
    x[[i]] <- info$x
    y[[i]] <- info$y
  }

  out <- list(
    exprs = exprs,
    condition = condition,
    filter = filter,
    x = x,
    y = y
  )

  structure(out, class = "dplyr_join_by")
}

#' @export
print.dplyr_join_by <- function(x, ...) {
  out <- map_chr(x$exprs, expr_deparse)
  out <- glue_collapse(glue("- {out}"), sep = "\n")
  cat("Join By:\n")
  cat(out)
  invisible(x)
}

is_join_by <- function(x) {
  inherits(x, "dplyr_join_by")
}

parse_join_by_expr <- function(expr, i) {
  len <- length(expr)

  if (len == 0L) {
    abort(c(
      "Join by expressions can't be empty.",
      x = glue("Expression {i} is empty.")
    ))
  }

  if (len == 2L) {
    info <- parse_join_by_filter_expr(expr, i)
    filter <- info$filter
    expr <- info$expr
  } else {
    filter <- "none"
  }

  out <- parse_join_by_condition_expr(expr, i)
  out$filter <- filter

  out
}

parse_join_by_filter_expr <- function(expr, i) {
  filter <- expr[[1]]
  filters <- c("max", "min")

  if (is_symbol(filter)) {
    filter <- as_string(filter)
  }
  if (!is_true(filter %in% filters)) {
    expr <- expr_deparse(expr)
    expr <- glue::backtick(expr)

    filters <- glue::glue("{filters}()")
    filters <- glue::backtick(filters)
    filters <- glue::glue_collapse(filters, last = " or ")

    abort(c(
      glue("`join_by()` expressions of length 2 must contain either {filters}."),
      x = glue("Expression {i} is {expr}.")
    ))
  }

  expr <- expr[[2]]

  list(
    filter = filter,
    expr = expr
  )
}

parse_join_by_condition_expr <- function(expr, i) {
  len <- length(expr)

  if (len == 1L && (is_symbol(expr) || is_string(expr))) {
    expr <- expr(!!expr == !!expr)
    len <- 3L
  }

  if (len != 3L) {
    expr <- expr_deparse(expr)
    expr <- glue::backtick(expr)

    header <- paste0(
      "Each `join_by()` condition must be length 1 or length 3, ",
      "and be composed of a left-hand side column name, a condition, ",
      "and a right-hand side column name."
    )
    abort(c(
      header,
      x = glue("Expression {i} is length {len} and is {expr}.")
    ))
  }

  condition <- expr[[1]]
  conditions <- c("==", ">", ">=", "<", "<=")

  if (is_symbol(condition)) {
    condition <- as_string(condition)
  }
  if (!is_true(condition %in% conditions)) {
    expr <- expr_deparse(expr)
    expr <- glue::backtick(expr)

    conditions <- glue::glue("{conditions}()")
    conditions <- glue::backtick(conditions)
    conditions <- glue::glue_collapse(conditions, sep = ", ", last = ", or ")

    abort(c(
      glue("Each `join_by()` condition must be separated by one of: {conditions}."),
      x = glue("Expression {i} is {expr}.")
    ))
  }

  x <- expr[[2]]
  y <- expr[[3]]

  if (is_symbol(x)) {
    x <- as_string(x)
  } else if (!is_string(x)) {
    x <- expr_deparse(x)
    x <- glue::backtick(x)

    header <- paste0(
      "The left-hand side of each `join_by()` condition must be ",
      "a string or an unquoted column name."
    )
    abort(c(
      header,
      i = glue("The left-hand side of condition {i} is {x}.")
    ))
  }

  if (is_symbol(y)) {
    y <- as_string(y)
  } else if (!is_string(y)) {
    y <- expr_deparse(y)
    y <- glue::backtick(y)

    header <- paste0(
      "The right-hand side of each `join_by()` condition must be ",
      "a string or an unquoted column name."
    )
    abort(c(
      header,
      i = glue("The right-hand side of condition {i} is {y}.")
    ))
  }

  list(
    condition = condition,
    x = x,
    y = y
  )
}
