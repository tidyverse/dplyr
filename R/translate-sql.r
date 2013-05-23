#' Translate an expression to sql.
#'
#' This is a helper function for convenient exploration. Otherwise conversion
#' normally happens in two distinct phases: first \code{\link{partial_eval}}
#' then \code{\link{to_sql}}.
#'
#' @param expr unevaluated expression to translate
#' @param source data source
#' @param variant SQL variant to use
#' @param env environment in which to evaluate expression
#' @export
#' @examples
#' data(baseball, package = "plyr")
#' translate_sql(year < 1890, baseball)
#' translate_sql(year < 1890L, baseball)
#' translate_sql(year %in% c(1890L, 1891L), baseball)
#' translate_sql(year %in% 1890:1900, baseball)
#' translate_sql(year >= 1890L & year <= 1900L, baseball)
#'
#' x <- 1890L
#' translate_sql(year == x, baseball)
#'
#' # By default all computation will happen in sql
#' translate_sql(year < 1889 + 1, baseball)
#' # Use local to force local evaluation
#' translate_sql(year < local(1889 + 1), baseball)
#'
#' # This is also needed if you call a local function:
#' inc <- function(x) x + 1
#' \dontrun{translate_sql(year == inc(x), baseball)}
#' translate_sql(year == local(inc(x)), baseball)
#'
#' # For testing, translate_sql can be run with source ommitted
#' x <- 1
#' y <- 2L
#' translate_sql(x ^ y)
translate_sql <- function(expr, source = NULL, env = parent.frame(), variant = base_sql) {
  expr <- partial_eval(substitute(expr), source, env = env)
  env <- sql_env(x, variant)
  eval(expr, env)
}

#' Translate an R expression to sql.
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
#'
#' You can also use \code{sql} to insert a raw sql string.
#'
#' @section SQLite translation:
#' The SQLite variant currently only adds one additional function: a mapping
#' from \code{sd} to the SQL aggregation function \code{stdev}.
#'
#' @param x for \code{to_sql}, an unquoted expression; for \code{to_sql_q}
#'   a quoted expression.
#' @param variant the sql variant to use for translation
#' @export
#' @examples
#' # Regular maths is translated in a very straightforward way
#' to_sql(x + 1)
#' to_sql(sin(x) + tan(y))
#'
#' # Logical operators are converted to their sql equivalents
#' to_sql(x < 5 & !(y >= 5))
#'
#' # Infix functions are passed onto SQL with % removed
#' to_sql(first %like% "Had*")
#' to_sql(first %is% NULL)
#' to_sql(first %in% c("John", "Roger", "Robert"))
#'
#' # Note that variable names will be escaped if needed
#' to_sql(like == 7)
#'
#' # And be careful if you really want integers
#' to_sql(x == 1)
#'
#' # If you have an already quoted object, use to_sql_q:
#' x <- quote(y + 1 / sin(t))
#' to_sql(x)
#' to_sql_q(x)
to_sql <- function(x, variant = base_sql) {
  to_sql_q(substitute(x), variant)
}

#' @export
#' @rdname to_sql
to_sql_q  <- function(x, variant = base_sql) {
  env <- sql_env(x, variant)
  eval(x, env)
}

sql_env <- function(expr, variant_env) {
  # Default for unknown functions
  unknown <- setdiff(all_calls(expr), ls(variant_env))
  default_env <- ceply(unknown, default_op, parent = emptyenv())

  # Known R -> SQL functions
  special_calls <- copy_env(variant_env, parent = default_env)

  # Existing symbols in expression
  names <- all_names(expr)
  name_env <- ceply(names, escape_sql, parent = special_calls)

  # Known latex expressions
  symbol_env <- copy_env(senv, parent = name_env)
  symbol_env
}

escape_sql <- function(x) {
  if (x == "") return("")
  ok <- "^[a-zA-Z_][a-zA-Z0-9_]*$"

  escape <- !grepl(ok, x) || toupper(x) %in% sql_keywords
  if (escape) {
    sql(paste0('"', x, '"'))
  } else {
    sql(x)
  }
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

copy_env <- function(from, to = NULL, parent = parent.env(from)) {
  list2env(as.list(from), envir = to, parent = parent)
}

# character vector -> environment
ceply <- function(x, f, ..., parent = parent.frame()) {
  if (length(x) == 0) return(new.env(parent = parent))
  l <- lapply(x, f, ...)
  names(l) <- x
  list2env(l, parent = parent)
}
