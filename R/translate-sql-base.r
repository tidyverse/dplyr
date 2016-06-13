#' @include translate-sql-helpers.r
#' @include sql-escape.r
NULL


sql_if <- function(cond, if_true, if_false = NULL) {
  build_sql(
    "CASE WHEN (", cond, ")",
    " THEN (", if_true, ")",
    if (!is.null(if_false)) build_sql(" ELSE (", if_false, ")"),
    " END"
  )
}

#' @export
#' @rdname sql_variant
#' @format NULL
base_scalar <- sql_translator(
  `+`    = sql_infix("+"),
  `*`    = sql_infix("*"),
  `/`    = sql_infix("/"),
  `%%`   = sql_infix("%"),
  `^`    = sql_prefix("power", 2),
  `-`    = function(x, y = NULL) {
    if (is.null(y)) {
      if (is.numeric(x)) {
        -x
      } else {
        build_sql(sql("-"), x)
      }
    } else {
      build_sql(x, sql(" - "), y)
    }
  },

  `!=`    = sql_infix("!="),
  `==`    = sql_infix("="),
  `<`     = sql_infix("<"),
  `<=`    = sql_infix("<="),
  `>`     = sql_infix(">"),
  `>=`    = sql_infix(">="),

  `!`     = sql_prefix("not"),
  `&`     = sql_infix("and"),
  `&&`    = sql_infix("and"),
  `|`     = sql_infix("or"),
  `||`    = sql_infix("or"),
  xor     = function(x, y) {
    sql(sprintf("%1$s OR %2$s AND NOT (%1$s AND %2$s)", escape(x), escape(y)))
  },

  abs     = sql_prefix("abs", 1),
  acos    = sql_prefix("acos", 1),
  acosh   = sql_prefix("acosh", 1),
  asin    = sql_prefix("asin", 1),
  asinh   = sql_prefix("asinh", 1),
  atan    = sql_prefix("atan", 1),
  atan2   = sql_prefix("atan2", 2),
  atanh   = sql_prefix("atanh", 1),
  ceil    = sql_prefix("ceil", 1),
  ceiling = sql_prefix("ceil", 1),
  cos     = sql_prefix("cos", 1),
  cosh    = sql_prefix("cosh", 1),
  cot     = sql_prefix("cot", 1),
  coth    = sql_prefix("coth", 1),
  exp     = sql_prefix("exp", 1),
  floor   = sql_prefix("floor", 1),
  log     = function(x, base = exp(1)) {
    build_sql(sql("log"), list(x, base))
  },
  log10   = sql_prefix("log10", 1),
  round   = sql_prefix("round", 2),
  sign    = sql_prefix("sign", 1),
  sin     = sql_prefix("sin", 1),
  sinh    = sql_prefix("sinh", 1),
  sqrt    = sql_prefix("sqrt", 1),
  tan     = sql_prefix("tan", 1),

  tolower = sql_prefix("lower", 1),
  toupper = sql_prefix("upper", 1),
  nchar   = sql_prefix("length", 1),

  `if` = sql_if,
  if_else = sql_if,
  ifelse = sql_if,

  sql = function(...) sql(...),
  `(` = function(x) {
    build_sql("(", x, ")")
  },
  `{` = function(x) {
    build_sql("(", x, ")")
  },
  desc = function(x) {
    build_sql(x, sql(" DESC"))
  },

  is.null = function(x) {
    build_sql("(", x, ") IS NULL")
  },
  is.na = function(x) {
    build_sql("(", x, ") IS NULL")
  },
  na_if = sql_prefix("NULL_IF", 2),

  as.numeric = function(x) build_sql("CAST(", x, " AS NUMERIC)"),
  as.integer = function(x) build_sql("CAST(", x, " AS INTEGER)"),
  as.character = function(x) build_sql("CAST(", x, " AS TEXT)"),

  c = function(...) escape(c(...)),
  `:` = function(from, to) escape(from:to),

  between = function(x, left, right) {
    build_sql(x, " BETWEEN ", left, " AND ", right)
  },

  pmin = sql_prefix("min"),
  pmax = sql_prefix("max"),

  `__dplyr_colwise_fun` = function(...) {
    stop("colwise verbs only accept bare functions with local sources",
      call. = FALSE)
  }
)

base_symbols <- sql_translator(
  pi = sql("PI()"),
  `*` = sql("*"),
  `NULL` = sql("NULL")
)

#' @export
#' @rdname sql_variant
#' @format NULL
base_agg <- sql_translator(
  # SQL-92 aggregates
  # http://db.apache.org/derby/docs/10.7/ref/rrefsqlj33923.html
  n          = sql_prefix("count"),
  mean       = sql_prefix("avg", 1),
  var        = sql_prefix("variance", 1),
  sum        = sql_prefix("sum", 1),
  min        = sql_prefix("min", 1),
  max        = sql_prefix("max", 1),
  n_distinct = function(x) {
    build_sql("COUNT(DISTINCT ", x, ")")
  }
)

#' @export
#' @rdname sql_variant
#' @format NULL
base_win <- sql_translator(
  # rank functions have a single order argument that overrides the default
  row_number   = win_rank("row_number"),
  min_rank     = win_rank("rank"),
  rank         = win_rank("rank"),
  dense_rank   = win_rank("dense_rank"),
  percent_rank = win_rank("percent_rank"),
  cume_dist    = win_rank("cume_dist"),
  ntile        = function(order_by, n) {
    over(
      build_sql("NTILE", list(as.integer(n))),
      partition_group(),
      order_by %||% partition_order()
    )
  },

  # Recycled aggregate fuctions take single argument, don't need order and
  # include entire partition in frame.
  mean  = win_recycled("avg"),
  sum   = win_recycled("sum"),
  min   = win_recycled("min"),
  max   = win_recycled("max"),
  n     = function() {
    over(sql("COUNT(*)"), partition_group())
  },

  # Cumulative function are like recycled aggregates except that R names
  # have cum prefix, order_by is inherited and frame goes from -Inf to 0.
  cummean = win_cumulative("mean"),
  cumsum  = win_cumulative("sum"),
  cummin  = win_cumulative("min"),
  cummax  = win_cumulative("max"),

  # Finally there are a few miscellaenous functions that don't follow any
  # particular pattern
  nth = function(x, order = NULL) {
    over(build_sql("NTH_VALUE", list(x)), partition_group(), order %||% partition$order())
  },
  first = function(x, order = NULL) {
    over(build_sql("FIRST_VALUE", list(x)), partition_group(), order %||% partition_order())
  },
  last = function(x, order = NULL) {
    over(build_sql("LAST_VALUE", list(x)), partition_group(), order %||% partition_order())
  },

  lead = function(x, n = 1L, default = NA, order = NULL) {
    over(
      build_sql("LEAD", list(x, n, default)),
      partition_group(),
      order %||% partition_order()
    )
  },
  lag = function(x, n = 1L, default = NA, order = NULL) {
    over(
      build_sql("LAG", list(x, n, default)),
      partition_group(),
      order %||% partition_order()
    )
  },

  order_by = function(order_by, expr) {
    old <- set_partition(partition_group(), order_by)
    on.exit(set_partition(old))

    expr
  }
)

#' @export
#' @rdname sql_variant
#' @format NULL
base_no_win <- sql_translator(
  row_number   = win_absent("row_number"),
  min_rank     = win_absent("rank"),
  rank         = win_absent("rank"),
  dense_rank   = win_absent("dense_rank"),
  percent_rank = win_absent("percent_rank"),
  cume_dist    = win_absent("cume_dist"),
  ntile        = win_absent("ntile"),
  mean         = win_absent("avg"),
  sum          = win_absent("sum"),
  min          = win_absent("min"),
  max          = win_absent("max"),
  n            = win_absent("n"),
  cummean      = win_absent("mean"),
  cumsum       = win_absent("sum"),
  cummin       = win_absent("min"),
  cummax       = win_absent("max"),
  nth          = win_absent("nth_value"),
  first        = win_absent("first_value"),
  last         = win_absent("last_value"),
  lead         = win_absent("lead"),
  lag          = win_absent("lag"),
  order_by     = win_absent("order_by")
)
