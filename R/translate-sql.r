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
#'
#' You can also use \code{sql} to insert a raw sql string.
#'
#' @section SQLite translation:
#' The SQLite variant currently only adds one additional function: a mapping
#' from \code{sd} to the SQL aggregation function \code{stdev}.
#'
#' @param ... unevaluated expression to translate
#' @param expr list of quoted objects to translate
#' @param source If supplied, will be used to automatically figure out the
#'   SQL variant to use. This can either be a \code{src} or a \code{tbl}.
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
#' 
#' # Note distinction between integers and reals
#' translate_sql(Month == 1, source = hflights)
#' translate_sql(Month == 1L, source = hflights)
#' 
#' # Know how to translate most simple mathematical expressions
#' translate_sql(Month %in% 1:3, source = hflights)
#' translate_sql(Month >= 1L & Month <= 3L, source = hflights)
#' translate_sql((Month >= 1L & Month <= 3L) | Carrier == "AA", source = hflights)
#' 
#' # Some R functions don't have equivalents in SQL: where possible they
#' # will be translated to the equivalent
#' translate_sql(xor(Month <= 3L, Carrier == "AA"), source = hflights)
#'
#' # Local variables will be automatically inserted into the SQL
#' x <- 5L
#' translate_sql(Month == x, source = hflights)
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
#' \donttest{
#' hflights_db <- tbl(hflights_postgres(), "hflights")
#' planes <- arrange(group_by(hflights_db, TailNum), desc(DepTime))
#' 
#' translate_sql(DepTime > mean(DepTime), source = planes, window = TRUE)
#' translate_sql(DepTime == min(DepTime), source = planes, window = TRUE)
#' 
#' translate_sql(rank(), source = planes, window = TRUE)
#' translate_sql(rank(DepTime), source = planes, window = TRUE)
#' translate_sql(ntile(DepTime, 2L), source = planes, window = TRUE)
#' translate_sql(lead(DepTime, 2L), source = planes, window = TRUE)
#' translate_sql(cumsum(DepDelay), source = planes, window = TRUE)
#' translate_sql(order_by(DepDelay, cumsum(DepDelay)), source = planes, window = TRUE)
#' }
translate_sql <- function(..., source = NULL, env = parent.frame(), 
                          variant = NULL, window = FALSE) {
  translate_sql_q(dots(...), source = source, env = env, variant = variant,
    window = window)
}

#' @export
translate_env.default <- function(x) base_sql

#' @export
#' @rdname translate_sql
translate_sql_q <- function(expr, source = NULL, env = parent.frame(),
                            variant = NULL, window = FALSE) {
  if (is.null(expr)) return(NULL)
  
  if (!is.null(env) && !is.null(source)) {
    expr <- partial_eval(expr, source, env)
  }
  
  variant <- variant %||%
    if (window) translate_window_env(source) else translate_env(source)
  
  pieces <- lapply(expr, function(x) {
    if (is.atomic(x)) return(escape(x))
    
    env <- sql_env(x, variant, source$con)
    eval(x, envir = env)
  })
  
  sql(unlist(pieces))
}

translate_env <- function(x) UseMethod("translate_env")

#' @export
translate_env.tbl_sql <- function(x) {
  translate_env(x$src)
}

sql_env <- function(expr, variant_env, con) {
  # Default for unknown functions
  unknown <- setdiff(all_calls(expr), ls(envir = variant_env))
  default_env <- ceply(unknown, default_op, parent = emptyenv())

  # Known R -> SQL functions
  special_calls <- copy_env(variant_env, parent = default_env)

  # Existing symbols in expression
  names <- all_names(expr)
  name_env <- ceply(names, function(x) escape(ident(x), con = con), 
    parent = special_calls)
  name_env$`*` <- sql("*")
  
  # Known latex expressions
  symbol_env <- copy_env(senv, parent = name_env)
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
