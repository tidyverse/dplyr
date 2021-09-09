#' Join specifications
#'
#' @description
#' `join_by()` constructs a specification that describes how to join two tables
#' using a small domain specific language. The result can be supplied as the
#' `by` argument to any of the join functions (such as [left_join()]).
#'
#' `join_by()` is constructed from a comma-separated set of expressions that
#' are generally of the form `x_col OP y_col`, where `OP` is one of `==`,
#' `>=`, `>`, `<=`, or `<`. These are described in detail below, along with
#' additional modifiers that are used to perform rolling and overlap joins.
#'
#' Multiple expressions are combined using an "and" operation in the order
#' they are specified.
#'
#' ## Equi joins:
#'
#' Equi joins match on equality, and are the most common type of join. To
#' construct an equi join, supply two column names to join with separated by
#' `==`. Alternatively, supplying a single name will be interpreted as an equi
#' join between two columns of the same name.
#'
#' ## Non-equi joins:
#'
#' Non-equi joins match on an inequality, and are common in time series analysis
#' and genomics. To construct a non-equi join, supply two column names separated
#' by `>`, `>=`, `<`, or `<=`.
#'
#' Note that non-equi joins will match a single row in `x` to a potentially
#' large number of rows in `y`. Be extra careful when constructing non-equi
#' join specifications!
#'
#' ## Rolling joins:
#'
#' Rolling joins are a variant of a non-equi join that limit the results
#' returned from each condition to either the maximum or minimum value of
#' the matches in `y`. To construct a rolling join, wrap a non-equi join
#' condition in `max()` or `min()`, such as `max(x > y)`, which specifies
#' that for each value of `x`, all matches in `y` should be found where `x > y`,
#' and then the maximum `y` value of those matches should be the only one
#' that is kept.
#'
#' ## Overlap joins:
#'
#' Overlap joins are a special case of a non-equi join generally involving one
#' or two columns from the left-hand side _overlapping_ a range computed from
#' two columns from the right-hand side. There are three helpers that
#' `join_by()` recognizes to assist with constructing overlap joins, all
#' of which can be constructed from simpler binary expressions.
#'
#' - `between(x, y_lower, y_upper)`
#'
#'   Matches when `x` falls between `[y_lower, y_upper]`. Equivalent to
#'   `x >= y_lower, x <= y_upper`.
#'
#' - `within(x_lower, x_upper, y_lower, y_upper)`
#'
#'   Matches when `[x_lower, x_upper]` falls completely within
#'   `[y_lower, y_upper]`. Equivalent to
#'   `x_lower >= y_lower, x_upper <= y_upper`.
#'
#' - `overlaps(x_lower, x_upper, y_lower, y_upper)`
#'
#'   Matches when `[x_lower, x_upper]` overlaps `[y_lower, y_upper]` in any
#'   capacity. Equivalent to `x_lower <= y_upper, x_upper >= y_lower`.
#'
#' Internally, arguments are matched by position and should not be named.
#'
#' These conditions assume that the ranges are well-formed, i.e.
#' `x_lower <= x_upper`.
#'
#' ## Column referencing:
#'
#' When specifying join conditions, `join_by()` assumes that column names on the
#' left-hand side of the condition refer to the left-hand table (`x`), and names
#' on the right-hand side of the condition refer to the right-hand table (`y`).
#' Occasionally, it is clearer to be able to specify a right-hand table name on
#' the left-hand side of the condition, and vice versa. To support this, column
#' names can be prefixed by `x$` or `y$` to explicitly specify which table they
#' come from.
#'
#' @details
#' Note that `join_by()` does not support arbitrary expressions on each side of
#' the join condition. For example, `join_by(sales_date - 40 >=
#' promo_date)` is not allowed. To perform a join like this, pre-compute
#' `sales_date - 40` and store it in a separate column, like `sales_date_lower`,
#' and refer to that column by name in `join_by()`.
#'
#' @param ... Expressions specifying the join.
#'
#'   Each expression should consist of either a join condition or a join helper:
#'
#'   - Join conditions: `==`, `>=`, `>`, `<=`, or `<`.
#'   - Join helpers: `between()`, `within()`, or `overlaps()`.
#'
#'   Column names should be specified as quoted or unquoted names. By default,
#'   the name on the left-hand side of a join condition refers to the left-hand
#'   table, unless overriden by explicitly prefixing the column name with either
#'   `x$` or `y$`.
#'
#'   Optionally, a non-equi join condition can be wrapped in `max()` or `min()`
#'   to specify a rolling join.
#'
#'   If a single column name is provided without any join conditions, it is
#'   interpreted as if that column name was duplicated on each side of `==`,
#'   i.e. `x` is interpreted as `x == x`.
#'
#' @export
#' @examples
#' sales <- tibble(
#'   id = c(1L, 1L, 1L, 2L, 2L),
#'   sale_date = as.Date(c("2018-12-31", "2019-01-02", "2019-01-05", "2019-01-04", "2019-01-01"))
#' )
#'
#' promos <- tibble(
#'   id = c(1L, 1L, 2L),
#'   promo_date = as.Date(c("2019-01-01", "2019-01-05", "2019-01-02"))
#' )
#'
#' # Match `id` to `id`, and `sales_date` to `promo_date`
#' by <- join_by(id, sale_date == promo_date)
#' left_join(sales, promos, by)
#'
#' # For each `sales_date` within a particular `id`, find all `promo_date`s that
#' # occurred before that particular sale
#' by <- join_by(id, sale_date >= promo_date)
#' left_join(sales, promos, by)
#'
#' # For each `sales_date` within a particular `id`, find the most recent
#' # `promo_date` that occurred before that particular sale
#' by <- join_by(id, max(sale_date >= promo_date))
#' left_join(sales, promos, by)
#'
#' # Same as before, but also require that the promo had to occur at most 1
#' # day before the sale was made. We'll use a full join to see that id 2's
#' # promo on `2019-01-02` is no longer matched to the sale on `2019-01-04`.
#' sales <- mutate(sales, sale_date_lower = sale_date - 1)
#' by <- join_by(id, max(sale_date >= promo_date), sale_date_lower <= promo_date)
#' full_join(sales, promos, by)
#'
#' # ---------------------------------------------------------------------------
#'
#' segments <- tibble(
#'   segment_id = 1:4,
#'   chromosome = c("chr1", "chr2", "chr2", "chr1"),
#'   start = c(140, 210, 380, 230),
#'   end = c(150, 240, 415, 280)
#' )
#'
#' reference <- tibble(
#'   reference_id = 1:4,
#'   chromosome = c("chr1", "chr1", "chr2", "chr2"),
#'   start = c(100, 200, 300, 400),
#'   end = c(150, 250, 399, 450)
#' )
#'
#' # Find every time a segment `start` falls between the reference
#' # `[start, end]` range.
#' by <- join_by(chromosome, between(start, start, end))
#' full_join(segments, reference, by)
#'
#' # If you wanted the reference columns first, supply `reference` as `x`
#' # and `segments` as `y`, then explicitly refer to their columns using `x$`
#' # and `y$`.
#' by <- join_by(chromosome, between(y$start, x$start, x$end))
#' full_join(reference, segments, by)
#'
#' # Find every time a segment falls completely within a reference.
#' # Sometimes using `x$` and `y$` makes your intentions clearer, even if they
#' # match the default behavior.
#' by <- join_by(chromosome, within(x$start, x$end, y$start, y$end))
#' inner_join(segments, reference, by)
#'
#' # Find every time a segment overlaps a reference in any way.
#' by <- join_by(chromosome, overlaps(x$start, x$end, y$start, y$end))
#' full_join(segments, reference, by)
join_by <- function(...) {
  exprs <- enexprs(..., .named = NULL)

  if (!is_null(names(exprs))) {
    abort(c(
      "`join_by()` expressions can't be named.",
      i = "Did you use `=` instead of `==`?"
    ))
  }

  n <- length(exprs)
  bys <- vector("list", length = n)

  for (i in seq_len(n)) {
    bys[[i]] <- parse_join_by_expr(exprs[[i]], i)
  }

  # `between()`, `overlaps()`, and `within()` parse into >1 binary conditions
  x <- flat_map_chr(bys, function(by) by$x)
  y <- flat_map_chr(bys, function(by) by$y)
  filter <- flat_map_chr(bys, function(by) by$filter)
  condition <- flat_map_chr(bys, function(by) by$condition)

  new_join_by(
    exprs = exprs,
    condition = condition,
    filter = filter,
    x = x,
    y = y
  )
}

#' @export
print.dplyr_join_by <- function(x, ...) {
  out <- map_chr(x$exprs, expr_deparse)
  out <- glue_collapse(glue("- {out}"), sep = "\n")
  cat("Join By:\n")
  cat(out)
  invisible(x)
}

new_join_by <- function(exprs, condition, filter, x, y) {
  out <- list(
    exprs = exprs,
    condition = condition,
    filter = filter,
    x = x,
    y = y
  )
  structure(out, class = "dplyr_join_by")
}

flat_map_chr <- function(x, fn) {
  vec_unchop(map(x, fn), ptype = character())
}

# ------------------------------------------------------------------------------

# Internal generic
as_join_by <- function(x) {
  UseMethod("as_join_by")
}

#' @export
as_join_by.default <- function(x) {
  bad_args("by", "must be a (named) character vector, list, `join_by()` result, or NULL, not {friendly_type_of(x)}.")
}

#' @export
as_join_by.dplyr_join_by <- function(x) {
  x
}

#' @export
as_join_by.character <- function(x) {
  x_names <- names(x) %||% x
  y_names <- unname(x)

  # If x partially named, assume unnamed are the same in both tables
  x_names[x_names == ""] <- y_names[x_names == ""]

  finalise_equi_join_by(x_names, y_names)
}

#' @export
as_join_by.list <- function(x) {
  # TODO: check lengths
  x_names <- x[["x"]]
  y_names <- x[["y"]]
  finalise_equi_join_by(x_names, y_names)
}

finalise_equi_join_by <- function(x_names, y_names) {
  exprs <- map2(x_names, y_names, function(x, y) expr(!!x == !!y))
  condition <- vec_rep("==", times = length(x_names))
  filter <- vec_rep("none", times = length(x_names))

  new_join_by(
    exprs = exprs,
    condition = condition,
    filter = filter,
    x = x_names,
    y = y_names
  )
}

# ------------------------------------------------------------------------------

join_by_common <- function(x_names, y_names) {
  by <- intersect(x_names, y_names)

  if (length(by) == 0) {
    abort(c(
      "`by` must be supplied when `x` and `y` have no common variables.",
      i = "use `by = character()` to perform a cross-join."
    ))
  }

  by_quoted <- encodeString(by, quote = '"')
  if (length(by_quoted) == 1L) {
    by_code <- by_quoted
  } else {
    by_code <- paste0("c(", paste(by_quoted, collapse = ", "), ")")
  }
  inform(paste0("Joining, by = ", by_code))

  finalise_equi_join_by(by, by)
}

# ------------------------------------------------------------------------------

parse_join_by_expr <- function(expr, i) {
  if (length(expr) == 0L) {
    abort(c(
      "Join by expressions can't be empty.",
      x = glue("Expression {i} is empty.")
    ))
  }

  if (is_symbol_or_string(expr)) {
    expr <- expr(!!expr == !!expr)
  }

  if (!is_call(expr)) {
    abort(c(
      "Each element of `...` must be a single column name or a join by expression.",
      x = glue("Element {i} is not a name and not an expression.")
    ))
  }

  op <- as_string(expr[[1]])

  switch(
    op,

    "==" =,
    ">=" =,
    ">" =,
    "<=" =,
    "<" = parse_join_by_binary(expr, i),

    "between" = parse_join_by_between(expr, i),

    "overlaps" =,
    "within" = parse_join_by_containment(expr, i),

    "max" =,
    "min" = parse_join_by_filter(expr, i),

    "$" = stop_invalid_dollar_sign(expr, i),

    stop_invalid_top_expression(expr, i)
  )
}

stop_invalid_dollar_sign <- function(expr, i) {
  abort(c(
    "When specifying a single column name, `$` cannot be used.",
    x = glue("Expression {i} is {err_expr(expr)}.")
  ))
}

stop_invalid_top_expression <- function(expr, i) {
  options <- c("==", ">=", ">", "<=", "<", "between()", "overlaps()", "within()", "max()", "min()")
  options <- glue::backtick(options)
  options <- glue_collapse(options, sep = ", ", last = ", or ")

  abort(c(
    glue("Join by expressions must use one of: {options}."),
    x = glue("Expression {i} is {err_expr(expr)}.")
  ))
}

parse_join_by_name <- function(expr, i, default_side) {
  if (is_symbol_or_string(expr)) {
    name <- as_string(expr)
    return(list(name = name, side = default_side))
  }

  if (is_call(expr, name = "$")) {
    return(parse_join_by_dollar(expr, i))
  }

  abort(c(
    paste0(
      "`join_by()` expressions cannot contain computed columns, ",
      "and can only reference columns by name or by explicitly specifying ",
      "a side, like `x$col` or `y$col`."
    ),
    x = glue("Expression {i} contains {err_expr(expr)}.")
  ))
}

parse_join_by_dollar <- function(expr, i) {
  n <- length(expr)

  if (n != 3L) {
    abort(c(
      "Expressions containing `$` must have length 3.",
      x = glue("Expression {i} contains {err_expr(expr)}, which is length {n}.")
    ))
  }

  side <- expr[[2]]

  if (!is_symbol_or_string(side)) {
    abort(c(
      "The left-hand side of a `$` expression must be a symbol or string.",
      x = glue("Expression {i} contains {err_expr(expr)}.")
    ))
  }

  side <- as_string(side)
  sides <- c("x", "y")

  if (!side %in% sides) {
    abort(c(
      "The left-hand side of a `$` expression must be either `x$` or `y$`.",
      x = glue("Expression {i} contains {err_expr(expr)}.")
    ))
  }

  name <- expr[[3]]

  if (!is_symbol_or_string(name)) {
    abort(c(
      "The right-hand side of a `$` expression must be a symbol or string.",
      x = glue("Expression {i} contains {err_expr(expr)}.")
    ))
  }

  name <- as_string(name)

  list(name = name, side = side)
}

parse_join_by_binary <- function(expr, i) {
  n <- length(expr)

  if (n != 3L) {
    abort(c(
      "Expressions containing a binary operation must have length 3.",
      x = glue("Expression {i} contains {err_expr(expr)}, which is length {n}.")
    ))
  }

  condition <- as_string(expr[[1]])

  lhs <- expr[[2]]
  rhs <- expr[[3]]

  lhs <- parse_join_by_name(lhs, i, default_side = "x")
  rhs <- parse_join_by_name(rhs, i, default_side = "y")

  if (lhs$side == rhs$side) {
    abort(c(
      "The left and right-hand sides of a binary expression must reference different tables.",
      x = glue("Expression {i} contains {err_expr(expr)}.")
    ))
  }

  if (lhs$side == "x") {
    x <- lhs$name
    y <- rhs$name
  } else {
    # Must reverse the op
    lookup <- c("==" = "==", ">=" = "<=", ">" = "<", "<=" = ">=", "<" = ">")
    condition <- lookup[[condition]]
    x <- rhs$name
    y <- lhs$name
  }

  list(
    x = x,
    y = y,
    condition = condition,
    filter = "none"
  )
}

parse_join_by_filter <- function(expr, i) {
  n <- length(expr)

  if (n != 2L) {
    abort(c(
      "Expressions containing a `max()` or `min()` filter must have length 2.",
      x = glue("Expression {i} is {err_expr(expr)}, which is length {n}.")
    ))
  }

  filter <- as_string(expr[[1]])
  inner <- expr[[2]]

  if (!is_call(inner, c("==", ">=", ">", "<=", "<"))) {
    abort(c(
      "`max()` or `min()` must wrap a binary condition.",
      x = glue("Expression {i} is {err_expr(expr)}.")
    ))
  }

  out <- parse_join_by_binary(inner, i)
  out$filter <- filter

  out
}

parse_join_by_between <- function(expr, i) {
  n <- length(expr)

  if (n != 4L) {
    abort(c(
      "Expressions containing `between()` must have 3 arguments.",
      x = glue("Expression {i} is {err_expr(expr)}, which has {n - 1L} argument(s).")
    ))
  }

  if (!is_null(names(expr))) {
    abort(c(
      "The arguments of `between()` must not be named.",
      x = glue("Expression {i} is {err_expr(expr)}.")
    ))
  }

  lhs <- parse_join_by_name(expr[[2]], i, "x")
  rhs_lower <- parse_join_by_name(expr[[3]], i, "y")
  rhs_upper <- parse_join_by_name(expr[[4]], i, "y")

  if (rhs_lower$side != rhs_upper$side) {
    abort(c(
      "Expressions containing `between()` must reference the same table for the lower and upper bounds.",
      x = glue("Expression {i} is {err_expr(expr)}.")
    ))
  }

  if (lhs$side == rhs_lower$side) {
    abort(c(
      "Expressions containing `between()` can't all reference the same table.",
      x = glue("Expression {i} is {err_expr(expr)}.")
    ))
  }

  if (lhs$side == "x") {
    x <- c(lhs$name, lhs$name)
    y <- c(rhs_lower$name, rhs_upper$name)
    condition <- c(">=", "<=")
  } else {
    x <- c(rhs_lower$name, rhs_upper$name)
    y <- c(lhs$name, lhs$name)
    condition <- c("<=", ">=")
  }

  filter <- c("none", "none")

  list(
    x = x,
    y = y,
    condition = condition,
    filter = filter
  )
}

parse_join_by_containment <- function(expr, i) {
  n <- length(expr)

  if (n != 5L) {
    abort(c(
      "Expressions containing `overlaps()` or `within()` must have 4 arguments.",
      x = glue("Expression {i} is {err_expr(expr)}, which has {n - 1L} argument(s).")
    ))
  }

  if (!is_null(names(expr))) {
    abort(c(
      "The arguments of `overlaps()` and `within()` must not be named.",
      x = glue("Expression {i} is {err_expr(expr)}.")
    ))
  }

  type <- expr[[1]]

  lhs_lower <- parse_join_by_name(expr[[2]], i, "x")
  lhs_upper <- parse_join_by_name(expr[[3]], i, "x")
  rhs_lower <- parse_join_by_name(expr[[4]], i, "y")
  rhs_upper <- parse_join_by_name(expr[[5]], i, "y")

  if (lhs_lower$side != lhs_upper$side) {
    abort(c(
      paste0(
        "Expressions containing `overlaps()` or `within()` must reference ",
        "the same table for the left-hand side lower and upper bounds."
      ),
      x = glue("Expression {i} is {err_expr(expr)}.")
    ))
  }

  if (rhs_lower$side != rhs_upper$side) {
    abort(c(
      paste0(
        "Expressions containing `overlaps()` or `within()` must reference ",
        "the same table for the right-hand side lower and upper bounds."
      ),
      x = glue("Expression {i} is {err_expr(expr)}.")
    ))
  }

  if (lhs_lower$side == rhs_lower$side) {
    abort(c(
      "Expressions containing `overlaps()` or `within()` can't all reference the same table.",
      x = glue("Expression {i} is {err_expr(expr)}.")
    ))
  }

  if (type == "overlaps") {
    if (lhs_lower$side == "x") {
      x <- c(lhs_lower$name, lhs_upper$name)
      y <- c(rhs_upper$name, rhs_lower$name)
      condition <- c("<=", ">=")
    } else {
      x <- c(rhs_upper$name, rhs_lower$name)
      y <- c(lhs_lower$name, lhs_upper$name)
      condition <- c(">=", "<=")
    }
  } else if (type == "within") {
    if (lhs_lower$side == "x") {
      x <- c(lhs_lower$name, lhs_upper$name)
      y <- c(rhs_lower$name, rhs_upper$name)
      condition <- c(">=", "<=")
    } else {
      x <- c(rhs_lower$name, rhs_upper$name)
      y <- c(lhs_lower$name, lhs_upper$name)
      condition <- c("<=", ">=")
    }
  } else {
    abort("Internal error: Unknown containment `type`.")
  }

  filter <- c("none", "none")

  list(
    x = x,
    y = y,
    condition = condition,
    filter = filter
  )
}

is_symbol_or_string <- function(x) {
  is_symbol(x) || is_string(x)
}

err_expr <- function(expr) {
  expr <- expr_deparse(expr)
  expr <- glue::backtick(expr)
  expr
}
