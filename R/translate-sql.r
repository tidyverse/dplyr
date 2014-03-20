#' Translate an expression to sql.
#'
#' @section Base translation:
#' The base translator, \code{base_sql},
#' provides custom mappings for \code{!} (to NOT), \code{&&} and \code{&} to
#' \code{AND}, \code{||} and \code{|} to \code{OR}, \code{^} to \code{POWER},
#' \code{\%\%} to \code{\%}, \code{ceiling} to \code{CEIL}, \code{mean} to
#' \code{AVG}, \code{var} to \code{VARIANCE}, \code{tolower} to \code{LOWER},
#' \code{toupper} to \code{UPPER} and \code{nchar} to \code{length}.
#'
#' \code{c} and \code{:} keep their usual R behaviour so you can easily create
#' vectors that are passed to sql.
#'
#' All other functions will be preserved as is. R's infix functions
#' (e.g. \code{\%like\%}) will be converted to their sql equivalents
#' (e.g. \code{LIKE}). You can use this to access SQL string concatenation:
#' \code{||} is mapped to \code{OR}, but \code{\%||\%} is mapped to \code{||}.
#' To suppress this behaviour, and force errors immediately when dplyr doesn't
#' know how to translate a function it encounters, using set the
#' \code{dplyr.strict_sql} option to \code{TRUE}.
#'
#' You can also use \code{sql} to insert a raw sql string.
#'
#' @section SQLite translation:
#' The SQLite variant currently only adds one additional function: a mapping
#' from \code{sd} to the SQL aggregation function \code{stdev}.
#'
#' @param ... unevaluated expression to translate
#' @param expr list of quoted objects to translate
#' @param tbl An optional \code{\link{tbl}}. If supplied, will be used to
#'   automatically figure out the SQL variant to use.
#' @param env environment in which to evaluate expression.
#' @param variant used to override default variant provided by source
#'   useful for testing/examples
#' @param window If \code{variant} not supplied, used to determine whether
#'   the variant is window based or not.
#' @export
#' @examples
#' # Regular maths is translated in a very straightforward way
#' translate_sql(x + 1)
#' translate_sql(sin(x) + tan(y))
#'
#' # Logical operators are converted to their sql equivalents
#' translate_sql(x < 5 & !(y >= 5))
#'
#' # If is translated into select case
#' translate_sql(if (x > 5) "big" else "small")
#'
#' # Infix functions are passed onto SQL with % removed
#' translate_sql(first %like% "Had*")
#' translate_sql(first %is% NULL)
#' translate_sql(first %in% c("John", "Roger", "Robert"))
#'
#' # Note that variable names will be escaped if needed
#' translate_sql(like == 7)
#'
#' # And be careful if you really want integers
#' translate_sql(x == 1)
#' translate_sql(x == 1L)
#'
#' # If you have an already quoted object, use translate_sql_q:
#' x <- quote(y + 1 / sin(t))
#' translate_sql(x)
#' translate_sql_q(list(x))
#'
#' # Translation with data source --------------------------------------------
#' \donttest{
#' hflights <- tbl(hflights_postgres(), "hflights")
#' # Note distinction between integers and reals
#' translate_sql(Month == 1, tbl = hflights)
#' translate_sql(Month == 1L, tbl = hflights)
#'
#' # Know how to translate most simple mathematical expressions
#' translate_sql(Month %in% 1:3, tbl = hflights)
#' translate_sql(Month >= 1L & Month <= 3L, tbl = hflights)
#' translate_sql((Month >= 1L & Month <= 3L) | Carrier == "AA", tbl = hflights)
#'
#' # Some R functions don't have equivalents in SQL: where possible they
#' # will be translated to the equivalent
#' translate_sql(xor(Month <= 3L, Carrier == "AA"), tbl = hflights)
#'
#' # Local variables will be automatically inserted into the SQL
#' x <- 5L
#' translate_sql(Month == x, tbl = hflights)
#'
#' # By default all computation will happen in sql
#' translate_sql(Month < 1 + 1, source = hflights)
#' # Use local to force local evaluation
#' translate_sql(Month < local(1 + 1), source = hflights)
#'
#' # This is also needed if you call a local function:
#' inc <- function(x) x + 1
#' translate_sql(Month == inc(x), source = hflights)
#' translate_sql(Month == local(inc(x)), source = hflights)
#'
#' # Windowed translation --------------------------------------------
#' planes <- arrange(group_by(hflights, TailNum), desc(DepTime))
#'
#' translate_sql(DepTime > mean(DepTime), tbl = planes, window = TRUE)
#' translate_sql(DepTime == min(DepTime), tbl = planes, window = TRUE)
#'
#' translate_sql(rank(), tbl = planes, window = TRUE)
#' translate_sql(rank(DepTime), tbl = planes, window = TRUE)
#' translate_sql(ntile(DepTime, 2L), tbl = planes, window = TRUE)
#' translate_sql(lead(DepTime, 2L), tbl = planes, window = TRUE)
#' translate_sql(cumsum(DepDelay), tbl = planes, window = TRUE)
#' translate_sql(order_by(DepDelay, cumsum(DepDelay)), tbl = planes, window = TRUE)
#' }
translate_sql <- function(..., tbl = NULL, env = parent.frame(), variant = NULL,
                          window = FALSE) {
  translate_sql_q(dots(...), tbl = tbl, env = env, variant = variant,
    window = window)
}

#' @export
#' @rdname translate_sql
translate_sql_q <- function(expr, tbl = NULL, env = parent.frame(),
                            variant = NULL, window = FALSE) {
  stopifnot(is.null(tbl) || inherits(tbl, "tbl_sql"))
  if (is.null(expr)) return(NULL)

  if (!is.null(tbl)) {
    con <- tbl$src$con
  } else {
    con <- NULL
  }

  # If environment not null, and tbl supplied, partially evaluate input
  if (!is.null(env) && !is.null(tbl)) {
    expr <- partial_eval(expr, tbl, env)
  }
  variant <- variant %||% translate_env(tbl)

  # Translate partition ordering and grouping, and make available
  if (window && !is.null(tbl)) {
    group_by <- translate_sql_q(tbl$group_by, variant = variant, env = NULL)
    order_by <- translate_sql_q(tbl$order_by, variant = variant, env = NULL)
    old <- set_partition(group_by, order_by)
    on.exit(set_partition(old))
  }

  pieces <- lapply(expr, function(x) {
    if (is.atomic(x)) return(escape(x, con = con))

    env <- sql_env(x, variant, con, window = window)
    eval(x, envir = env)
  })

  sql(unlist(pieces))
}

translate_env <- function(x) UseMethod("translate_env")
#' @export
translate_env.tbl_sql <- function(x) translate_env(x$src)
#' @export
translate_env.NULL <- function(x) {
  sql_variant(
    base_scalar,
    base_agg,
  )
}

sql_env <- function(expr, variant, con, window = FALSE,
                    strict = getOption("dplyr.strict_sql")) {
  stopifnot(is.sql_variant(variant))

  # Default for unknown functions
  if (!strict) {
    unknown <- setdiff(all_calls(expr), names(variant))
    default_env <- ceply(unknown, default_op, parent = emptyenv())
  } else {
    default_env <- new.env(parent = emptyenv())
  }


  # Known R -> SQL functions
  special_calls <- copy_env(variant$scalar, parent = default_env)
  if (!window) {
    special_calls2 <- copy_env(variant$aggregate, parent = special_calls)
  } else {
    special_calls2 <- copy_env(variant$window, parent = special_calls)
  }

  # Existing symbols in expression
  names <- all_names(expr)
  name_env <- ceply(names, function(x) escape(ident(x), con = con),
    parent = special_calls2)

  # Known sql expressions
  symbol_env <- copy_env(base_symbols, parent = name_env)
  symbol_env
}

default_op <- function(x) {
  assert_that(is.string(x))
  infix <- c("::", "$", "@", "^", "*", "/", "+", "-", ">", ">=", "<", "<=",
    "==", "!=", "!", "&", "&&", "|", "||", "~", "<-", "<<-")

  if (x %in% infix) {
    sql_infix(x)
  } else if (grepl("^%.*%$", x)) {
    x <- substr(x, 2, nchar(x) - 1)
    sql_infix(x)
  } else {
    sql_prefix(x)
  }
}


all_calls <- function(x) {
  if (!is.call(x)) return(NULL)

  fname <- as.character(x[[1]])
  unique(c(fname, unlist(lapply(x[-1], all_calls), use.names = FALSE)))
}

all_names <- function(x) {
  if (is.name(x)) return(as.character(x))
  if (!is.call(x)) return(NULL)

  unique(unlist(lapply(x[-1], all_names), use.names = FALSE))
}

# character vector -> environment
ceply <- function(x, f, ..., parent = parent.frame()) {
  if (length(x) == 0) return(new.env(parent = parent))
  l <- lapply(x, f, ...)
  names(l) <- x
  list2env(l, parent = parent)
}
