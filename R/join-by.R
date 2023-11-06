#' Join specifications
#'
#' `join_by()` constructs a specification that describes how to join two tables
#' using a small domain specific language. The result can be supplied as the
#' `by` argument to any of the join functions (such as [left_join()]).
#'
#' # Join types
#'
#' The following types of joins are supported by dplyr:
#' - Equality joins
#' - Inequality joins
#' - Rolling joins
#' - Overlap joins
#' - Cross joins
#'
#' Equality, inequality, rolling, and overlap joins are discussed in more detail
#' below. Cross joins are implemented through [cross_join()].
#'
#' ## Equality joins
#'
#' Equality joins require keys to be equal between one or more pairs of columns,
#' and are the most common type of join. To construct an equality join using
#' `join_by()`, supply two column names to join with separated by `==`.
#' Alternatively, supplying a single name will be interpreted as an equality
#' join between two columns of the same name. For example, `join_by(x)` is
#' equivalent to `join_by(x == x)`.
#'
#' ## Inequality joins
#'
#' Inequality joins match on an inequality, such as `>`, `>=`, `<`, or `<=`, and
#' are common in time series analysis and genomics. To construct an inequality
#' join using `join_by()`, supply two column names separated by one of the above
#' mentioned inequalities.
#'
#' Note that inequality joins will match a single row in `x` to a potentially
#' large number of rows in `y`. Be extra careful when constructing inequality
#' join specifications!
#'
#' ## Rolling joins
#'
#' Rolling joins are a variant of inequality joins that limit the results
#' returned from an inequality join condition. They are useful for "rolling" the
#' closest match forward/backwards when there isn't an exact match. To construct
#' a rolling join, wrap an inequality with `closest()`.
#'
#' - `closest(expr)`
#'
#'   `expr` must be an inequality involving one of: `>`, `>=`, `<`, or `<=`.
#'
#'   For example, `closest(x >= y)` is interpreted as: For each value in `x`,
#'   find the closest value in `y` that is less than or equal to that `x` value.
#'
#' `closest()` will always use the left-hand table (`x`) as the primary table,
#' and the right-hand table (`y`) as the one to find the closest match in,
#' regardless of how the inequality is specified. For example,
#' `closest(y$a >= x$b)` will always be interpreted as `closest(x$b <= y$a)`.
#'
#' ## Overlap joins
#'
#' Overlap joins are a special case of inequality joins involving one or two
#' columns from the left-hand table _overlapping_ a range defined by two columns
#' from the right-hand table. There are three helpers that `join_by()`
#' recognizes to assist with constructing overlap joins, all of which can be
#' constructed from simpler inequalities.
#'
#' - `between(x, y_lower, y_upper, ..., bounds = "[]")`
#'
#'   For each value in `x`, this finds everywhere that value falls between
#'   `[y_lower, y_upper]`. Equivalent to `x >= y_lower, x <= y_upper` by
#'   default.
#'
#'   `bounds` can be one of \code{"[]"}, \code{"[)"}, \code{"(]"}, or
#'   \code{"()"} to alter the inclusiveness of the lower and upper bounds. This
#'   changes whether `>=` or `>` and `<=` or `<` are used to build the
#'   inequalities shown above.
#'
#'   Dots are for future extensions and must be empty.
#'
#' - `within(x_lower, x_upper, y_lower, y_upper)`
#'
#'   For each range in `[x_lower, x_upper]`, this finds everywhere that range
#'   falls completely within `[y_lower, y_upper]`. Equivalent to `x_lower >=
#'   y_lower, x_upper <= y_upper`.
#'
#'   The inequalities used to build `within()` are the same regardless of the
#'   inclusiveness of the supplied ranges.
#'
#' - `overlaps(x_lower, x_upper, y_lower, y_upper, ..., bounds = "[]")`
#'
#'   For each range in `[x_lower, x_upper]`, this finds everywhere that range
#'   overlaps `[y_lower, y_upper]` in any capacity. Equivalent to `x_lower <=
#'   y_upper, x_upper >= y_lower` by default.
#'
#'   `bounds` can be one of \code{"[]"}, \code{"[)"}, \code{"(]"}, or
#'   \code{"()"} to alter the inclusiveness of the lower and upper bounds.
#'   \code{"[]"} uses `<=` and `>=`, but the 3 other options use `<` and `>`
#'   and generate the exact same inequalities.
#'
#'   Dots are for future extensions and must be empty.
#'
#' These conditions assume that the ranges are well-formed and non-empty, i.e.
#' `x_lower <= x_upper` when bounds are treated as \code{"[]"}, and
#' `x_lower < x_upper` otherwise.
#'
#' # Column referencing
#'
#' When specifying join conditions, `join_by()` assumes that column names on the
#' left-hand side of the condition refer to the left-hand table (`x`), and names
#' on the right-hand side of the condition refer to the right-hand table (`y`).
#' Occasionally, it is clearer to be able to specify a right-hand table name on
#' the left-hand side of the condition, and vice versa. To support this, column
#' names can be prefixed by `x$` or `y$` to explicitly specify which table they
#' come from.
#'
#' @param ... Expressions specifying the join.
#'
#'   Each expression should consist of one of the following:
#'
#'   - Equality condition: `==`
#'   - Inequality conditions: `>=`, `>`, `<=`, or `<`
#'   - Rolling helper: `closest()`
#'   - Overlap helpers: `between()`, `within()`, or `overlaps()`
#'
#'   Other expressions are not supported. If you need to perform a join on
#'   a computed variable, e.g. `join_by(sales_date - 40 >= promo_date)`,
#'   you'll need to precompute and store it in a separate column.
#'
#'   Column names should be specified as quoted or unquoted names. By default,
#'   the name on the left-hand side of a join condition refers to the left-hand
#'   table, unless overridden by explicitly prefixing the column name with
#'   either `x$` or `y$`.
#'
#'   If a single column name is provided without any join conditions, it is
#'   interpreted as if that column name was duplicated on each side of `==`,
#'   i.e. `x` is interpreted as `x == x`.
#'
#' @aliases closest overlaps within
#'
#' @export
#' @examples
#' sales <- tibble(
#'   id = c(1L, 1L, 1L, 2L, 2L),
#'   sale_date = as.Date(c("2018-12-31", "2019-01-02", "2019-01-05", "2019-01-04", "2019-01-01"))
#' )
#' sales
#'
#' promos <- tibble(
#'   id = c(1L, 1L, 2L),
#'   promo_date = as.Date(c("2019-01-01", "2019-01-05", "2019-01-02"))
#' )
#' promos
#'
#' # Match `id` to `id`, and `sale_date` to `promo_date`
#' by <- join_by(id, sale_date == promo_date)
#' left_join(sales, promos, by)
#'
#' # For each `sale_date` within a particular `id`,
#' # find all `promo_date`s that occurred before that particular sale
#' by <- join_by(id, sale_date >= promo_date)
#' left_join(sales, promos, by)
#'
#' # For each `sale_date` within a particular `id`,
#' # find only the closest `promo_date` that occurred before that sale
#' by <- join_by(id, closest(sale_date >= promo_date))
#' left_join(sales, promos, by)
#'
#' # If you want to disallow exact matching in rolling joins, use `>` rather
#' # than `>=`. Note that the promo on `2019-01-05` is no longer considered the
#' # closest match for the sale on the same date.
#' by <- join_by(id, closest(sale_date > promo_date))
#' left_join(sales, promos, by)
#'
#' # Same as before, but also require that the promo had to occur at most 1
#' # day before the sale was made. We'll use a full join to see that id 2's
#' # promo on `2019-01-02` is no longer matched to the sale on `2019-01-04`.
#' sales <- mutate(sales, sale_date_lower = sale_date - 1)
#' by <- join_by(id, closest(sale_date >= promo_date), sale_date_lower <= promo_date)
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
#' segments
#'
#' reference <- tibble(
#'   reference_id = 1:4,
#'   chromosome = c("chr1", "chr1", "chr2", "chr2"),
#'   start = c(100, 200, 300, 415),
#'   end = c(150, 250, 399, 450)
#' )
#' reference
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
#'
#' # It is common to have right-open ranges with bounds like `[)`, which would
#' # mean an end value of `415` would no longer overlap a start value of `415`.
#' # Setting `bounds` allows you to compute overlaps with those kinds of ranges.
#' by <- join_by(chromosome, overlaps(x$start, x$end, y$start, y$end, bounds = "[)"))
#' full_join(segments, reference, by)
join_by <- function(...) {
  # `join_by()` works off pure expressions with no evaluation in the user's
  # environment, but we want to allow `{{ }}` to make it easier to program with.
  # The best way to do this is to capture quosures with `enquos()`, and then
  # immediately squash them recursively into expressions with `quo_squash()`.
  exprs <- enquos(..., .named = NULL)
  exprs <- map(exprs, quo_squash)

  n <- length(exprs)

  if (n == 0L) {
    abort(c(
      "Must supply at least one expression.",
      i = "If you want a cross join, use `cross_join()`."
    ))
  }

  if (!is_null(names(exprs))) {
    abort(c(
      "Can't name join expressions.",
      i = "Did you use `=` instead of `==`?"
    ))
  }

  error_call <- environment()

  bys <- vector("list", length = n)

  for (i in seq_len(n)) {
    bys[[i]] <- parse_join_by_expr(exprs[[i]], i, error_call = error_call)
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

new_join_by <- function(exprs = list(),
                        condition = character(),
                        filter = character(),
                        x = character(),
                        y = character()) {
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
  list_unchop(map(x, fn), ptype = character())
}

# ------------------------------------------------------------------------------

# Internal generic
as_join_by <- function(x, error_call = caller_env()) {
  UseMethod("as_join_by")
}

#' @export
as_join_by.default <- function(x, error_call = caller_env()) {
  message <- glue(paste0(
    "`by` must be a (named) character vector, list, `join_by()` result, ",
    "or NULL, not {obj_type_friendly(x)}."
  ))
  abort(message, call = error_call)
}

#' @export
as_join_by.dplyr_join_by <- function(x, error_call = caller_env()) {
  x
}

#' @export
as_join_by.character <- function(x, error_call = caller_env()) {
  x_names <- names(x) %||% x
  y_names <- unname(x)

  # If x partially named, assume unnamed are the same in both tables
  x_names[x_names == ""] <- y_names[x_names == ""]

  finalise_equi_join_by(x_names, y_names)
}

#' @export
as_join_by.list <- function(x, error_call = caller_env()) {
  # TODO: check lengths
  x_names <- x[["x"]]
  y_names <- x[["y"]]

  if (!is_character(x_names)) {
    abort("`by$x` must evaluate to a character vector.")
  }
  if (!is_character(y_names)) {
    abort("`by$y` must evaluate to a character vector.")
  }

  finalise_equi_join_by(x_names, y_names)
}

finalise_equi_join_by <- function(x_names, y_names) {
  n <- length(x_names)

  if (n == 0L) {
    abort(
      "Backwards compatible support for cross joins should have been caught earlier.",
      .internal = TRUE
    )
  }

  exprs <- map2(x_names, y_names, function(x, y) expr(!!x == !!y))
  condition <- vec_rep("==", times = n)
  filter <- vec_rep("none", times = n)

  new_join_by(
    exprs = exprs,
    condition = condition,
    filter = filter,
    x = x_names,
    y = y_names
  )
}

# ------------------------------------------------------------------------------

join_by_common <- function(x_names,
                           y_names,
                           ...,
                           error_call = caller_env()) {
  check_dots_empty0(...)

  by <- intersect(x_names, y_names)

  if (length(by) == 0) {
    message <- c(
      "`by` must be supplied when `x` and `y` have no common variables.",
      i = "Use `cross_join()` to perform a cross-join."
    )
    abort(message, call = error_call)
  }

  by_names <- tick_if_needed(by)
  by_names <- glue_collapse(by_names, sep = ", ")
  inform(glue("Joining with `by = join_by({by_names})`"))

  finalise_equi_join_by(by, by)
}

# ------------------------------------------------------------------------------

# In the parsing implementation below, note that all `binding_*()` functions
# should maintain a function signature that exactly matches what is documented
# in `?join_by`, as these get bound directly to their corresponding function
# name, i.e. `binding_join_by_between()` is bound to `between()`. This is why
# these functions don't have an `error_call` argument.

parse_join_by_expr <- function(expr, i, error_call) {
  if (is_missing(expr)) {
    message <- c(
      "Expressions can't be missing.",
      x = glue("Expression {i} is missing.")
    )
    abort(message, call = error_call)
  }

  if (length(expr) == 0L) {
    message <- c(
      "Expressions can't be empty.",
      x = glue("Expression {i} is empty.")
    )
    abort(message, call = error_call)
  }

  if (is_symbol_or_string(expr)) {
    expr <- expr(!!expr == !!expr)
  }

  if (!is_call(expr)) {
    message <- c(
      "Each element of `...` must be a single column name or a join by expression.",
      x = glue("Element {i} is not a name and not an expression.")
    )
    abort(message, call = error_call)
  }

  if (is_call(expr, ns = "dplyr")) {
    # Normalize by removing the `dplyr::`
    expr[[1]] <- sym(call_name(expr))
  }

  op <- expr[[1]]

  if (!is_symbol(op)) {
    if (is_call(op, name = "::")) {
      stop_invalid_namespaced_expression(expr, i, error_call)
    } else {
      stop_invalid_top_expression(expr, i, error_call)
    }
  }

  op <- as_string(op)

  switch(
    op,

    "==" =,
    ">=" =,
    ">" =,
    "<=" =,
    "<" = parse_join_by_binary(expr, i, error_call),

    "between" = parse_join_by_between(expr, i, error_call),
    "within" = parse_join_by_within(expr, i, error_call),
    "overlaps" = parse_join_by_overlaps(expr, i, error_call),

    "closest" = parse_join_by_closest(expr, i, error_call),

    "$" = stop_invalid_dollar_sign(expr, i, error_call),

    stop_invalid_top_expression(expr, i, error_call)
  )
}

stop_invalid_dollar_sign <- function(expr, i, call) {
  message <- c(
    "Can't use `$` when specifying a single column name.",
    i = glue("Expression {i} is {err_expr(expr)}.")
  )

  abort(message, call = call)
}

stop_invalid_top_expression <- function(expr, i, call) {
  options <- c("==", ">=", ">", "<=", "<", "closest()", "between()", "overlaps()", "within()")
  options <- glue::backtick(options)
  options <- glue_collapse(options, sep = ", ", last = ", or ")

  message <- c(
    glue("Expressions must use one of: {options}."),
    i = glue("Expression {i} is {err_expr(expr)}.")
  )

  abort(message, call = call)
}

stop_invalid_namespaced_expression <- function(expr, i, call) {
  message <- c(
    glue("Expressions can only be namespace prefixed with `dplyr::`."),
    i = glue("Expression {i} is {err_expr(expr)}.")
  )

  abort(message, call = call)
}

parse_join_by_name <- function(expr,
                               i,
                               default_side,
                               error_call) {
  if (is_symbol_or_string(expr)) {
    name <- as_string(expr)
    return(list(name = name, side = default_side))
  }

  if (is_call(expr, name = "$")) {
    return(parse_join_by_dollar(expr, i, error_call))
  }

  message <- c(
    paste0(
      "Expressions can't contain computed columns, ",
      "and can only reference columns by name or by explicitly specifying ",
      "a side, like `x$col` or `y$col`."
    ),
    i = glue("Expression {i} contains {err_expr(expr)}.")
  )
  abort(message, call = error_call)
}

parse_join_by_dollar <- function(expr,
                                 i,
                                 error_call) {
  args <- eval_join_by_dollar(expr, error_call)

  side <- args$side

  if (!is_symbol_or_string(side)) {
    message <- c(
      "The left-hand side of a `$` expression must be a symbol or string.",
      i = glue("Expression {i} contains {err_expr(expr)}.")
    )
    abort(message, call = error_call)
  }

  side <- as_string(side)
  sides <- c("x", "y")

  if (!side %in% sides) {
    message <- c(
      "The left-hand side of a `$` expression must be either `x$` or `y$`.",
      i = glue("Expression {i} contains {err_expr(expr)}.")
    )
    abort(message, call = error_call)
  }

  name <- args$name

  if (!is_symbol_or_string(name)) {
    message <- c(
      "The right-hand side of a `$` expression must be a symbol or string.",
      i = glue("Expression {i} contains {err_expr(expr)}.")
    )
    abort(message, call = error_call)
  }

  name <- as_string(name)

  list(name = name, side = side)
}
eval_join_by_dollar <- function(expr, error_call) {
  env <- new_environment()
  local_error_call(error_call, frame = env)

  env_poke(env, "$", binding_join_by_dollar)

  eval_tidy(expr, env = env)
}
binding_join_by_dollar <- function(x, name) {
  error_call <- caller_env()

  x <- enexpr(x)
  name <- enexpr(name)

  check_missing_arg(x, "x", "$", error_call, binary_op = TRUE)
  check_missing_arg(name, "name", "$", error_call, binary_op = TRUE)

  list(side = x, name = name)
}

parse_join_by_binary <- function(expr, i, error_call) {
  args <- eval_join_by_binary(expr, error_call)

  condition <- args$condition

  lhs <- args$lhs
  rhs <- args$rhs

  lhs <- parse_join_by_name(lhs, i, default_side = "x", error_call = error_call)
  rhs <- parse_join_by_name(rhs, i, default_side = "y", error_call = error_call)

  if (lhs$side == rhs$side) {
    message <- c(
      "The left and right-hand sides of a binary expression must reference different tables.",
      i = glue("Expression {i} contains {err_expr(expr)}.")
    )
    abort(message, call = error_call)
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
eval_join_by_binary <- function(expr, error_call) {
  env <- new_environment()
  local_error_call(error_call, frame = env)

  env_bind(
    env,
    `==` = binding_join_by_equality,
    `>` = binding_join_by_greater_than,
    `>=` = binding_join_by_greater_than_or_equal,
    `<` = binding_join_by_less_than,
    `<=` = binding_join_by_less_than_or_equal
  )

  eval_tidy(expr, env = env)
}
binding_join_by_binary <- function(condition, error_call, x, y) {
  x <- enexpr(x)
  y <- enexpr(y)

  check_missing_arg(x, "x", condition, error_call, binary_op = TRUE)
  check_missing_arg(y, "y", condition, error_call, binary_op = TRUE)

  list(condition = condition, lhs = x, rhs = y)
}
binding_join_by_equality <- function(x, y) {
  binding_join_by_binary("==", caller_env(), !!enexpr(x), !!enexpr(y))
}
binding_join_by_greater_than <- function(x, y) {
  binding_join_by_binary(">", caller_env(), !!enexpr(x), !!enexpr(y))
}
binding_join_by_greater_than_or_equal <- function(x, y) {
  binding_join_by_binary(">=", caller_env(), !!enexpr(x), !!enexpr(y))
}
binding_join_by_less_than <- function(x, y) {
  binding_join_by_binary("<", caller_env(), !!enexpr(x), !!enexpr(y))
}
binding_join_by_less_than_or_equal <- function(x, y) {
  binding_join_by_binary("<=", caller_env(), !!enexpr(x), !!enexpr(y))
}

parse_join_by_closest <- function(expr, i, error_call) {
  args <- eval_join_by_closest(expr, error_call)

  expr_binary <- args$expr

  if (!is_call(expr_binary)) {
    message <- c(
      "The first argument of `closest()` must be an expression.",
      i = glue("Expression {i} is {err_expr(expr)}.")
    )
    abort(message, call = error_call)
  }

  op <- as_string(expr_binary[[1]])

  out <- switch(
    op,

    ">=" =,
    ">" =,
    "<=" =,
    "<" = parse_join_by_binary(expr_binary, i, error_call),

    "==" = stop_join_by_closest_equal_expression(expr, i, error_call),

    stop_join_by_closest_invalid_expression(expr, i, error_call)
  )

  filter <- switch(
    out$condition,
    ">=" = "max",
    ">" = "max",
    "<=" = "min",
    "<" = "min",
    abort("Unexpected `closest()` `condition`.", .internal = TRUE)
  )

  out$filter <- filter

  out
}
eval_join_by_closest <- function(expr, error_call) {
  env <- new_environment()
  local_error_call(error_call, frame = env)

  env_poke(env, "closest", binding_join_by_closest)

  eval_tidy(expr, env = env)
}
binding_join_by_closest <- function(expr) {
  error_call <- caller_env()

  expr <- enexpr(expr)

  check_missing_arg(expr, "expr", "closest", error_call)

  list(expr = expr)
}
stop_join_by_closest_equal_expression <- function(expr, i, error_call) {
  # `closest(x == y)` doesn't make any sense,
  # even if vctrs can technically handle it.
  message <- c(
    "The expression used in `closest()` can't use `==`.",
    i = glue("Expression {i} is {err_expr(expr)}.")
  )
  abort(message, call = error_call)
}
stop_join_by_closest_invalid_expression <- function(expr, i, error_call) {
  options <- c(">=", ">", "<=", "<")
  options <- glue::backtick(options)
  options <- glue_collapse(options, sep = ", ", last = ", or ")

  message <- c(
    glue("The expression used in `closest()` must use one of: {options}."),
    i = glue("Expression {i} is {err_expr(expr)}.")
  )

  abort(message, call = error_call)
}

parse_join_by_between <- function(expr, i, error_call) {
  args <- eval_join_by_between(expr, error_call)

  lhs <- parse_join_by_name(args$lhs, i, "x", error_call)
  rhs_lower <- parse_join_by_name(args$rhs_lower, i, "y", error_call)
  rhs_upper <- parse_join_by_name(args$rhs_upper, i, "y", error_call)

  bounds <- args$bounds

  if (rhs_lower$side != rhs_upper$side) {
    message <- c(
      "Expressions containing `between()` must reference the same table for the lower and upper bounds.",
      i = glue("Expression {i} is {err_expr(expr)}.")
    )
    abort(message, call = error_call)
  }

  if (lhs$side == rhs_lower$side) {
    message <- c(
      "Expressions containing `between()` can't all reference the same table.",
      i = glue("Expression {i} is {err_expr(expr)}.")
    )
    abort(message, call = error_call)
  }

  if (lhs$side == "x") {
    x <- c(lhs$name, lhs$name)
    y <- c(rhs_lower$name, rhs_upper$name)
    condition <- switch(
      bounds,
      "[]" = c(">=", "<="),
      "[)" = c(">=", "<"),
      "(]" = c(">", "<="),
      "()" = c(">", "<")
    )
  } else {
    x <- c(rhs_lower$name, rhs_upper$name)
    y <- c(lhs$name, lhs$name)
    condition <- switch(
      bounds,
      "[]" = c("<=", ">="),
      "[)" = c("<=", ">"),
      "(]" = c("<", ">="),
      "()" = c("<", ">")
    )
  }

  filter <- c("none", "none")

  list(
    x = x,
    y = y,
    condition = condition,
    filter = filter
  )
}
eval_join_by_between <- function(expr, error_call) {
  env <- new_environment()
  local_error_call(error_call, frame = env)

  env_poke(env, "between", binding_join_by_between)

  eval_tidy(expr, env = env)
}
binding_join_by_between <- function(x, y_lower, y_upper, ..., bounds = "[]") {
  error_call <- caller_env()

  check_join_by_dots_empty(..., fn = "between", call = error_call)

  x <- enexpr(x)
  y_lower <- enexpr(y_lower)
  y_upper <- enexpr(y_upper)

  check_missing_arg(x, "x", "between", error_call)
  check_missing_arg(y_lower, "y_lower", "between", error_call)
  check_missing_arg(y_upper, "y_upper", "between", error_call)

  bounds <- check_bounds(bounds, call = error_call)

  list(lhs = x, rhs_lower = y_lower, rhs_upper = y_upper, bounds = bounds)
}

parse_join_by_within <- function(expr, i, error_call) {
  args <- eval_join_by_within(expr, error_call)

  lhs_lower <- parse_join_by_name(args$lhs_lower, i, "x", error_call)
  lhs_upper <- parse_join_by_name(args$lhs_upper, i, "x", error_call)
  rhs_lower <- parse_join_by_name(args$rhs_lower, i, "y", error_call)
  rhs_upper <- parse_join_by_name(args$rhs_upper, i, "y", error_call)

  if (lhs_lower$side != lhs_upper$side) {
    message <- c(
      paste0(
        "Expressions containing `within()` must reference ",
        "the same table for the left-hand side lower and upper bounds."
      ),
      i = glue("Expression {i} is {err_expr(expr)}.")
    )
    abort(message, call = error_call)
  }

  if (rhs_lower$side != rhs_upper$side) {
    message <- c(
      paste0(
        "Expressions containing `within()` must reference ",
        "the same table for the right-hand side lower and upper bounds."
      ),
      i = glue("Expression {i} is {err_expr(expr)}.")
    )
    abort(message, call = error_call)
  }

  if (lhs_lower$side == rhs_lower$side) {
    message <- c(
      "Expressions containing `within()` can't all reference the same table.",
      i = glue("Expression {i} is {err_expr(expr)}.")
    )
    abort(message, call = error_call)
  }

  if (lhs_lower$side == "x") {
    x <- c(lhs_lower$name, lhs_upper$name)
    y <- c(rhs_lower$name, rhs_upper$name)
    condition <- c(">=", "<=")
  } else {
    x <- c(rhs_lower$name, rhs_upper$name)
    y <- c(lhs_lower$name, lhs_upper$name)
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
eval_join_by_within <- function(expr, error_call) {
  env <- new_environment()
  local_error_call(error_call, frame = env)

  env_poke(env, "within", binding_join_by_within)

  eval_tidy(expr, env = env)
}
binding_join_by_within <- function(x_lower,
                                   x_upper,
                                   y_lower,
                                   y_upper) {
  error_call <- caller_env()

  x_lower <- enexpr(x_lower)
  x_upper <- enexpr(x_upper)
  y_lower <- enexpr(y_lower)
  y_upper <- enexpr(y_upper)

  check_missing_arg(x_lower, "x_lower", "within", error_call)
  check_missing_arg(x_upper, "x_upper", "within", error_call)
  check_missing_arg(y_lower, "y_lower", "within", error_call)
  check_missing_arg(y_upper, "y_upper", "within", error_call)

  list(
    lhs_lower = x_lower,
    lhs_upper = x_upper,
    rhs_lower = y_lower,
    rhs_upper = y_upper
  )
}

parse_join_by_overlaps <- function(expr, i, error_call) {
  args <- eval_join_by_overlaps(expr, error_call)

  lhs_lower <- parse_join_by_name(args$lhs_lower, i, "x", error_call)
  lhs_upper <- parse_join_by_name(args$lhs_upper, i, "x", error_call)
  rhs_lower <- parse_join_by_name(args$rhs_lower, i, "y", error_call)
  rhs_upper <- parse_join_by_name(args$rhs_upper, i, "y", error_call)

  bounds <- args$bounds

  if (lhs_lower$side != lhs_upper$side) {
    message <- c(
      paste0(
        "Expressions containing `overlaps()` must reference ",
        "the same table for the left-hand side lower and upper bounds."
      ),
      i = glue("Expression {i} is {err_expr(expr)}.")
    )
    abort(message, call = error_call)
  }

  if (rhs_lower$side != rhs_upper$side) {
    message <- c(
      paste0(
        "Expressions containing `overlaps()` must reference ",
        "the same table for the right-hand side lower and upper bounds."
      ),
      i = glue("Expression {i} is {err_expr(expr)}.")
    )
    abort(message, call = error_call)
  }

  if (lhs_lower$side == rhs_lower$side) {
    message <- c(
      "Expressions containing `overlaps()` can't all reference the same table.",
      i = glue("Expression {i} is {err_expr(expr)}.")
    )
    abort(message, call = error_call)
  }

  # 3 of the `bounds` have the exact same behavior, but the argument name is
  # consistent with `between(bounds =)` and easier to remember and interpret
  # than exposing `closed` directly (#6504).
  # - `[]` uses `<=` and `>=`
  # - All other conditions use `<` and `>` due to the presence of a `(` or `)`
  closed <- switch(
    bounds,
    "[]" = TRUE,
    "[)" = FALSE,
    "(]" = FALSE,
    "()" = FALSE,
    abort("Unknown `bounds`.", .internal = TRUE)
  )

  if (lhs_lower$side == "x") {
    x <- c(lhs_lower$name, lhs_upper$name)
    y <- c(rhs_upper$name, rhs_lower$name)

    if (closed) {
      condition <- c("<=", ">=")
    } else {
      condition <- c("<", ">")
    }
  } else {
    x <- c(rhs_upper$name, rhs_lower$name)
    y <- c(lhs_lower$name, lhs_upper$name)

    if (closed) {
      condition <- c(">=", "<=")
    } else {
      condition <- c(">", "<")
    }
  }

  filter <- c("none", "none")

  list(
    x = x,
    y = y,
    condition = condition,
    filter = filter
  )
}
eval_join_by_overlaps <- function(expr, error_call) {
  env <- new_environment()
  local_error_call(error_call, frame = env)

  env_poke(env, "overlaps", binding_join_by_overlaps)

  eval_tidy(expr, env = env)
}
binding_join_by_overlaps <- function(x_lower,
                                     x_upper,
                                     y_lower,
                                     y_upper,
                                     ...,
                                     bounds = "[]") {
  error_call <- caller_env()

  check_join_by_dots_empty(..., fn = "overlaps", call = error_call)

  x_lower <- enexpr(x_lower)
  x_upper <- enexpr(x_upper)
  y_lower <- enexpr(y_lower)
  y_upper <- enexpr(y_upper)

  check_missing_arg(x_lower, "x_lower", "overlaps", error_call)
  check_missing_arg(x_upper, "x_upper", "overlaps", error_call)
  check_missing_arg(y_lower, "y_lower", "overlaps", error_call)
  check_missing_arg(y_upper, "y_upper", "overlaps", error_call)

  bounds <- check_bounds(bounds, call = error_call)

  list(
    lhs_lower = x_lower,
    lhs_upper = x_upper,
    rhs_lower = y_lower,
    rhs_upper = y_upper,
    bounds = bounds
  )
}

check_bounds <- function(bounds, call) {
  arg_match0(
    bounds,
    values = c("[]", "[)", "(]", "()"),
    error_call = call
  )
}

check_join_by_dots_empty <- function(..., fn, call) {
  if (dots_n(...) == 0L) {
    return()
  }

  fn <- glue::backtick(glue("{fn}()"))

  message <- c(
    "`...` must be empty.",
    i = glue("Non-empty dots were detected inside {fn}.")
  )

  abort(message, call = call)
}

check_missing_arg <- function(arg,
                              arg_name,
                              fn_name,
                              error_call,
                              ...,
                              binary_op = FALSE) {
  check_dots_empty0(...)

  if (!is_missing(arg)) {
    return(invisible())
  }

  if (!binary_op) {
    fn_name <- glue("{fn_name}()")
  }

  arg_name <- glue::backtick(arg_name)
  fn_name <- glue::backtick(fn_name)

  message <- c(
    glue("Expressions using {fn_name} can't contain missing arguments."),
    x = glue("Argument {arg_name} is missing.")
  )
  abort(message, call = error_call)
}

is_symbol_or_string <- function(x) {
  is_symbol(x) || is_string(x)
}

err_expr <- function(expr) {
  expr <- expr_deparse(expr)
  expr <- glue::backtick(expr)
  expr
}
