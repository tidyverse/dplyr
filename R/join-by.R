#' Join specifications
#'
#' `join_by()` constructs a specification that describes how to join two tables
#' using a small domain specific language. The result can be supplied as the
#' `by` argument to any of the join functions (such as [left_join()]).
#'
#' # Join types
#' `join_by()` supports four types of join, as described below:
#'
#' * Equi joins
#' * Non-equi joins
#' * Rolling joins
#' * Overlap joins
#'
#' ## Equi joins
#'
#' Equi joins match on equality, and are the most common type of join. To
#' construct an equi join, supply two column names to join with separated by
#' `==`. Alternatively, supplying a single name will be interpreted as an equi
#' join between two columns of the same name. For example, `join_by(x)` is
#' equivalent to `join_by(x == x)`.
#'
#' ## Non-equi joins
#'
#' Non-equi joins match on an inequality, and are common in time series analysis
#' and genomics. To construct a non-equi join, supply two column names separated
#' by `>`, `>=`, `<`, or `<=`.
#'
#' Note that non-equi joins will match a single row in `x` to a potentially
#' large number of rows in `y`. Be extra careful when constructing non-equi
#' join specifications!
#'
#' ## Rolling joins
#'
#' Rolling joins are a variant of a non-equi join that limit the results
#' returned from the non-equi join condition. They are useful for "rolling"
#' the preceding value forward or the following value backwards when there
#' isn't an exact match. There are two helpers that `join_by()` recognizes
#' to assist with constructing rolling joins.
#'
#' - `preceding(x, y, ..., inclusive = TRUE)`
#'
#'   For each value in `x`, this finds the value in `y` that is directly
#'   preceding it. If `inclusive = TRUE`, an exact match is allowed to count as
#'   the preceding value.
#'
#'   Technically, this finds all matches using the binary condition `x >= y`,
#'   then filters those matches to only include the one corresponding to the
#'   maximum value of `y` (i.e. the preceding match). If `inclusive = FALSE`,
#'   `>=` is replaced with `>`.
#'
#'   Dots are for future extensions and must be empty.
#'
#' - `following(x, y, ..., inclusive = TRUE)`
#'
#'   For each value in `x`, this finds the value in `y` that is directly
#'   following it. If `inclusive = TRUE`, an exact match is allowed to count as
#'   the following value.
#'
#'   Technically, this finds all matches using the binary condition `x <= y`,
#'   then filters those matches to only include the one corresponding to the
#'   minimum value of `y` (i.e. the following match). If `inclusive = FALSE`,
#'   `<=` is replaced with `<`.
#'
#'   Dots are for future extensions and must be empty.
#'
#' Unlike other join helpers, the `x` argument must reference the left-hand
#' table (`x`) and the `y` argument must reference the right-hand table (`y`).
#' Attempting something like `preceding(y$a, x$b)` is not defined and will
#' result in an error.
#'
#' Rolling joins can't be constructed directly from binary conditions, but are
#' approximately equivalent to applying the binary condition mentioned above
#' followed by a `filter()` for only the maximum or minimum `y` value.
#'
#' ## Overlap joins
#'
#' Overlap joins are a special case of a non-equi join generally involving one
#' or two columns from the left-hand table _overlapping_ a range computed from
#' two columns from the right-hand table. There are three helpers that
#' `join_by()` recognizes to assist with constructing overlap joins, all of
#' which can be constructed from simpler binary expressions.
#'
#' - `between(x, y_lower, y_upper)`
#'
#'   For each value in `x`, this finds everywhere that value falls between
#'   `[y_lower, y_upper]`. Equivalent to `x >= y_lower, x <= y_upper`.
#'
#' - `within(x_lower, x_upper, y_lower, y_upper)`
#'
#'   For each range in `[x_lower, x_upper]`, this finds everywhere that range
#'   falls completely within `[y_lower, y_upper]`. Equivalent to `x_lower >=
#'   y_lower, x_upper <= y_upper`.
#'
#' - `overlaps(x_lower, x_upper, y_lower, y_upper)`
#'
#'   For each range in `[x_lower, x_upper]`, this finds everywhere that range
#'   overlaps `[y_lower, y_upper]` in any capacity. Equivalent to `x_lower <=
#'   y_upper, x_upper >= y_lower`.
#'
#' These conditions assume that the ranges are well-formed, i.e.
#' `x_lower <= x_upper`.
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
#'   Each expression should consist of either a join condition or a join helper:
#'
#'   - Join conditions: `==`, `>=`, `>`, `<=`, or `<`.
#'   - Rolling helpers: `preceding()` or `following()`.
#'   - Overlap helpers: `between()`, `within()`, or `overlaps()`.
#'
#'   Other expressions are not supported. If you need to perform a join on
#'   a computed variable, e.g. `join_by(sales_date - 40 >= promo_date)`,
#'   you'll need to precompute and store it in a separate column.
#'
#'   Column names should be specified as quoted or unquoted names. By default,
#'   the name on the left-hand side of a join condition refers to the left-hand
#'   table, unless overridden by explicitly prefixing the column name with either
#'   `x$` or `y$`.
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
#' # find only the preceding `promo_date` that occurred directly
#' # before that sale
#' by <- join_by(id, preceding(sale_date, promo_date))
#' left_join(sales, promos, by)
#'
#' # If you want to disallow exact matching in rolling joins,
#' # set `inclusive = FALSE`. Note that the promo on `2019-01-05` is no longer
#' # considered the preceding match for the sale on the same date.
#' by <- join_by(id, preceding(sale_date, promo_date, inclusive = FALSE))
#' left_join(sales, promos, by)
#'
#' # Same as before, but also require that the promo had to occur at most 1
#' # day before the sale was made. We'll use a full join to see that id 2's
#' # promo on `2019-01-02` is no longer matched to the sale on `2019-01-04`.
#' sales <- mutate(sales, sale_date_lower = sale_date - 1)
#' by <- join_by(id, preceding(sale_date, promo_date), sale_date_lower <= promo_date)
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
#'   start = c(100, 200, 300, 400),
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
join_by <- function(...) {
  exprs <- enexprs(..., .named = NULL)

  if (!is_null(names(exprs))) {
    abort(c(
      "`join_by()` expressions can't be named.",
      i = "Did you use `=` instead of `==`?"
    ))
  }

  error_call <- environment()

  n <- length(exprs)
  bys <- vector("list", length = n)

  for (i in seq_len(n)) {
    bys[[i]] <- parse_join_by_expr(exprs[[i]], i, error_call = error_call)
  }

  # `between()`, `overlaps()`, and `within()` parse into >1 binary conditions
  x <- flat_map_chr(bys, function(by) by$x)
  y <- flat_map_chr(bys, function(by) by$y)
  filter <- flat_map_chr(bys, function(by) by$filter)
  condition <- flat_map_chr(bys, function(by) by$condition)

  # Cross join for empty `join_by()` calls
  cross <- n == 0L

  new_join_by(
    exprs = exprs,
    condition = condition,
    filter = filter,
    cross = cross,
    x = x,
    y = y
  )
}

#' @export
print.dplyr_join_by <- function(x, ...) {
  if (x$cross) {
    out <- "- Cross"
  } else {
    out <- map_chr(x$exprs, expr_deparse)
    out <- glue_collapse(glue("- {out}"), sep = "\n")
  }

  cat("Join By:\n")
  cat(out)

  invisible(x)
}

new_join_by <- function(exprs, condition, filter, cross, x, y) {
  out <- list(
    exprs = exprs,
    condition = condition,
    filter = filter,
    cross = cross,
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
as_join_by <- function(x, error_call = caller_env()) {
  UseMethod("as_join_by")
}

#' @export
as_join_by.default <- function(x, error_call = caller_env()) {
  message <- glue(paste0(
    "`by` must be a (named) character vector, list, `join_by()` result, ",
    "or NULL, not {friendly_type_of(x)}."
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
  finalise_equi_join_by(x_names, y_names)
}

finalise_equi_join_by <- function(x_names, y_names) {
  n <- length(x_names)

  exprs <- map2(x_names, y_names, function(x, y) expr(!!x == !!y))
  condition <- vec_rep("==", times = n)
  filter <- vec_rep("none", times = n)
  cross <- n == 0L

  new_join_by(
    exprs = exprs,
    condition = condition,
    filter = filter,
    cross = cross,
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
      i = "Use `by = character()` to perform a cross-join."
    )
    abort(message, call = error_call)
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

# In the parsing implementation below, note that all `binding_*()` functions
# should maintain a function signature that exactly matches what is documented
# in `?join_by`, as these get bound directly to their corresponding function
# name, i.e. `binding_join_by_between()` is bound to `between()`. This is why
# these functions don't have an `error_call` argument.

parse_join_by_expr <- function(expr, i, error_call) {
  if (length(expr) == 0L) {
    message <- c(
      "Join by expressions can't be empty.",
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

  op <- as_string(expr[[1]])

  switch(
    op,

    "==" =,
    ">=" =,
    ">" =,
    "<=" =,
    "<" = parse_join_by_binary(expr, i, error_call),

    "between" = parse_join_by_between(expr, i, error_call),

    "overlaps" =,
    "within" = parse_join_by_containment(expr, i, error_call),

    "preceding" =,
    "following" = parse_join_by_rolling(expr, i, error_call),

    "$" = stop_invalid_dollar_sign(expr, i, error_call),

    stop_invalid_top_expression(expr, i, error_call)
  )
}

stop_invalid_dollar_sign <- function(expr, i, call) {
  message <- c(
    "When specifying a single column name, `$` cannot be used.",
    i = glue("Expression {i} is {err_expr(expr)}.")
  )

  abort(message, call = call)
}

stop_invalid_top_expression <- function(expr, i, call) {
  options <- c("==", ">=", ">", "<=", "<", "preceding()", "following()", "between()", "overlaps()", "within()")
  options <- glue::backtick(options)
  options <- glue_collapse(options, sep = ", ", last = ", or ")

  message <- c(
    glue("Join by expressions must use one of: {options}."),
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
      "`join_by()` expressions cannot contain computed columns, ",
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

parse_join_by_rolling <- function(expr, i, error_call) {
  args <- eval_join_by_rolling(expr, error_call)

  rolling <- args$rolling
  inclusive <- args$inclusive

  filter <- switch(
    rolling,
    preceding = "max",
    following = "min",
    abort("Unknown `rolling` value.", .internal = TRUE)
  )

  condition <- switch(
    rolling,
    preceding = if (inclusive) ">=" else ">",
    following = if (inclusive) "<=" else "<",
    abort("Unknown `rolling` value.", .internal = TRUE)
  )

  lhs <- args$lhs
  rhs <- args$rhs

  lhs <- parse_join_by_name(lhs, i, default_side = "x", error_call = error_call)
  rhs <- parse_join_by_name(rhs, i, default_side = "y", error_call = error_call)

  # It doesn't make sense to allow `preceding(y$a, x$b)`, as that can't
  # translate to anything meaningful / intuitive.
  if (lhs$side == "y") {
    rolling <- glue::backtick(glue("{rolling}()"))

    message <- c(
      glue("The first argument to {rolling} must reference the `x` table."),
      i = glue("Expression {i} contains {err_expr(expr)}.")
    )
    abort(message, call = error_call)
  }

  if (rhs$side == "x") {
    rolling <- glue::backtick(glue("{rolling}()"))

    message <- c(
      glue("The second argument to {rolling} must reference the `y` table."),
      i = glue("Expression {i} contains {err_expr(expr)}.")
    )
    abort(message, call = error_call)
  }

  x <- lhs$name
  y <- rhs$name

  list(
    x = x,
    y = y,
    condition = condition,
    filter = filter
  )
}
eval_join_by_rolling <- function(expr, error_call) {
  env <- new_environment()
  local_error_call(error_call, frame = env)

  env_bind(
    env,
    preceding = binding_join_by_preceding,
    following = binding_join_by_following
  )

  eval_tidy(expr, env = env)
}
binding_join_by_rolling <- function(rolling, error_call, x, y, ..., inclusive) {
  if (dots_n(...) > 0L) {
    rolling <- glue::backtick(glue("{rolling}()"))
    message <- c(
      "`...` must be empty.",
      i = glue("Non-empty dots were detected inside {rolling}.")
    )
    abort(message, call = error_call)
  }

  x <- enexpr(x)
  y <- enexpr(y)

  check_missing_arg(x, "x", rolling, error_call)
  check_missing_arg(y, "y", rolling, error_call)

  if (!is_bool(inclusive)) {
    rolling <- glue::backtick(glue("{rolling}()"))
    message <- c(
      "`inclusive` must be a single `TRUE` or `FALSE`.",
      i = glue("An invalid `inclusive` value was detected inside {rolling}.")
    )
    abort(message, call = error_call)
  }

  list(rolling = rolling, lhs = x, rhs = y, inclusive = inclusive)
}
binding_join_by_preceding <- function(x, y, ..., inclusive = TRUE) {
  binding_join_by_rolling("preceding", caller_env(), !!enexpr(x), !!enexpr(y), ..., inclusive = inclusive)
}
binding_join_by_following <- function(x, y, ..., inclusive = TRUE) {
  binding_join_by_rolling("following", caller_env(), !!enexpr(x), !!enexpr(y), ..., inclusive = inclusive)
}

parse_join_by_between <- function(expr, i, error_call) {
  args <- eval_join_by_between(expr, error_call)

  lhs <- parse_join_by_name(args$lhs, i, "x", error_call)
  rhs_lower <- parse_join_by_name(args$rhs_lower, i, "y", error_call)
  rhs_upper <- parse_join_by_name(args$rhs_upper, i, "y", error_call)

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
eval_join_by_between <- function(expr, error_call) {
  env <- new_environment()
  local_error_call(error_call, frame = env)

  env_poke(env, "between", binding_join_by_between)

  eval_tidy(expr, env = env)
}
binding_join_by_between <- function(x, y_lower, y_upper) {
  error_call <- caller_env()

  x <- enexpr(x)
  y_lower <- enexpr(y_lower)
  y_upper <- enexpr(y_upper)

  check_missing_arg(x, "x", "between", error_call)
  check_missing_arg(y_lower, "y_lower", "between", error_call)
  check_missing_arg(y_upper, "y_upper", "between", error_call)

  list(lhs = x, rhs_lower = y_lower, rhs_upper = y_upper)
}

parse_join_by_containment <- function(expr, i, error_call) {
  args <- eval_join_by_containment(expr, error_call)

  type <- args$type

  lhs_lower <- parse_join_by_name(args$lhs_lower, i, "x", error_call)
  lhs_upper <- parse_join_by_name(args$lhs_upper, i, "x", error_call)
  rhs_lower <- parse_join_by_name(args$rhs_lower, i, "y", error_call)
  rhs_upper <- parse_join_by_name(args$rhs_upper, i, "y", error_call)

  if (lhs_lower$side != lhs_upper$side) {
    message <- c(
      paste0(
        "Expressions containing `overlaps()` or `within()` must reference ",
        "the same table for the left-hand side lower and upper bounds."
      ),
      i = glue("Expression {i} is {err_expr(expr)}.")
    )
    abort(message, call = error_call)
  }

  if (rhs_lower$side != rhs_upper$side) {
    message <- c(
      paste0(
        "Expressions containing `overlaps()` or `within()` must reference ",
        "the same table for the right-hand side lower and upper bounds."
      ),
      i = glue("Expression {i} is {err_expr(expr)}.")
    )
    abort(message, call = error_call)
  }

  if (lhs_lower$side == rhs_lower$side) {
    message <- c(
      "Expressions containing `overlaps()` or `within()` can't all reference the same table.",
      i = glue("Expression {i} is {err_expr(expr)}.")
    )
    abort(message, call = error_call)
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
    abort("Unknown containment `type`.", .internal = TRUE)
  }

  filter <- c("none", "none")

  list(
    x = x,
    y = y,
    condition = condition,
    filter = filter
  )
}
eval_join_by_containment <- function(expr, error_call) {
  env <- new_environment()
  local_error_call(error_call, frame = env)

  env_bind(
    env,
    within = binding_join_by_within,
    overlaps = binding_join_by_overlaps
  )

  eval_tidy(expr, env = env)
}
binding_join_by_containment <- function(type,
                                        error_call,
                                        x_lower,
                                        x_upper,
                                        y_lower,
                                        y_upper) {
  x_lower <- enexpr(x_lower)
  x_upper <- enexpr(x_upper)
  y_lower <- enexpr(y_lower)
  y_upper <- enexpr(y_upper)

  check_missing_arg(x_lower, "x_lower", type, error_call)
  check_missing_arg(x_upper, "x_upper", type, error_call)
  check_missing_arg(y_lower, "y_lower", type, error_call)
  check_missing_arg(y_upper, "y_upper", type, error_call)

  list(
    type = type,
    lhs_lower = x_lower,
    lhs_upper = x_upper,
    rhs_lower = y_lower,
    rhs_upper = y_upper
  )
}
binding_join_by_within <- function(x_lower, x_upper, y_lower, y_upper) {
  binding_join_by_containment(
    type = "within",
    error_call = caller_env(),
    x_lower = !!enexpr(x_lower),
    x_upper = !!enexpr(x_upper),
    y_lower = !!enexpr(y_lower),
    y_upper = !!enexpr(y_upper)
  )
}
binding_join_by_overlaps <- function(x_lower, x_upper, y_lower, y_upper) {
  binding_join_by_containment(
    type = "overlaps",
    error_call = caller_env(),
    x_lower = !!enexpr(x_lower),
    x_upper = !!enexpr(x_upper),
    y_lower = !!enexpr(y_lower),
    y_upper = !!enexpr(y_upper)
  )
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
