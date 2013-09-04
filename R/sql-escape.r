#' SQL escaping.
#'
#' These functions are critical when writing functions that translate R
#' functions to sql functions. Typically a conversion function should escape
#' all it's inputs and return an sql object.
#'
#' @param ... Character vectors that will be combined into a single SQL
#'   expression. \code{ident} flags its input as a identifier, to ensure that
#'   it gets the correct quoting.
#' @param x An object to escape. Existing sql vectors will be left as is,
#'   character vectors are escaped with single quotes, numeric vectors have
#'   trailing \code{.0} added if they're whole numbers. 
#' @keywords internal
#' @export
#' @examples
#' # Doubles vs. integers
#' escape(1:5)
#' escape(c(1, 5.4))
#' 
#' # String vs known sql vs. sql identifier 
#' escape("X")
#' escape(sql("X"))
#' escape(ident("X"))
#' 
#' # Escaping is idempotent
#' escape("X")
#' escape(escape("X"))
#' escape(escape(escape("X")))
#'
#' # You can use these functions to make your own R wrappers for SQL functions.
#' # The following is a more sophisticated version of round that have more
#' # informative variable names and if present, checks that the second argument
#' # is a number.
#' sql_round <- function(x, dp = NULL) {
#'   x <- escape(x)
#'   if (is.null(dp)) return(sql("ROUND(", x, ")"))
#'
#'   stopifnot(is.numeric(dp), length(dp) == 1)
#'   sql("ROUND(", x, ", ", dp, ")")
#' }
#' sql_round(sql("X"), 5)
#'
#' rounder <- sql_variant(round = sql_round)
#' to_sql(round(X), rounder)
#' to_sql(round(X, 5), rounder)
#' \dontrun{to_sql(round(X, "a"), rounder)}
sql <- function(...) {
  structure(paste0(...), class = "sql")
}

#' @export 
#' @rdname sql
ident <- function(...) {
  structure(c(...), class = c("ident", "sql"))
}

#' @rdname sql
#' @export
is.sql <- function(x) inherits(x, "sql")

#' @S3method print sql
print.sql <- function(x, ...) cat("<SQL> ", x, "\n", sep = "")

#' @rdname sql
#' @export
escape <- function(x) UseMethod("escape")

#' @S3method escape ident
escape.ident <- function(x) {
  sql_vector(paste0('"', x, '"'))
}

#' @S3method escape character
escape.character <- function(x) {
  sql_vector(paste0("'", x, "'"))
}

#' @S3method escape double
escape.double <- function(x) {
  x <- ifelse(is.wholenumber(x), sprintf("%.1f", x), as.character(x))
  sql_vector(x)
}

#' @S3method escape integer
escape.integer <- function(x) {
  sql_vector(x)
}

#' @S3method escape NULL
escape.NULL <- function(x) sql("NULL")

#' @S3method escape sql
escape.sql <- function(x) x

#' @S3method escape list
escape.list <- function(x) sql(vapply(x, escape, character(1)))

sql_vector <- function(x) {
  if (length(x) == 1) return(sql(x))
  sql("(", paste(x, collapse = ", "), ")")
}
