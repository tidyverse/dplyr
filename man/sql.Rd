\name{sql}
\alias{escape}
\alias{ident}
\alias{is.sql}
\alias{sql}
\title{SQL escaping.}
\usage{
  sql(...)

  ident(...)

  is.sql(x)

  escape(x, parens = NA, collapse = " ")
}
\arguments{
  \item{...}{Character vectors that will be combined into a
  single SQL expression. \code{ident} flags its input as a
  identifier, to ensure that it gets the correct quoting.}

  \item{x}{An object to escape. Existing sql vectors will
  be left as is, character vectors are escaped with single
  quotes, numeric vectors have trailing \code{.0} added if
  they're whole numbers, identifiers are escaped with
  double quotes.}

  \item{parens,collapse}{Controls behaviour when multiple
  values are supplied.  \code{parens} should be a logical
  flag, or if \code{NA}, will wrap in parens if length > 1.

  Default behaviour: lists are always wrapped in parens and
  separated by commas, identifiers are separated by commas
  and never wrapped, atomic vectors are separated by spaces
  and wrapped in parens if needed.}
}
\description{
  These functions are critical when writing functions that
  translate R functions to sql functions. Typically a
  conversion function should escape all it's inputs and
  return an sql object.
}
\examples{
# Doubles vs. integers
escape(1:5)
escape(c(1, 5.4))

# String vs known sql vs. sql identifier
escape("X")
escape(sql("X"))
escape(ident("X"))

# Escaping is idempotent
escape("X")
escape(escape("X"))
escape(escape(escape("X")))

# You can use these functions to make your own R wrappers for SQL functions.
# The following is a more sophisticated version of round that have more
# informative variable names and if present, checks that the second argument
# is a number.
sql_round <- function(x, dp = NULL) {
  x <- escape(x)
  if (is.null(dp)) return(sql("ROUND(", x, ")"))

  stopifnot(is.numeric(dp), length(dp) == 1)
  sql("ROUND(", x, ", ", dp, ")")
}
sql_round(sql("X"), 5)

rounder <- sql_variant(round = sql_round)
to_sql(round(X), rounder)
to_sql(round(X, 5), rounder)
\dontrun{to_sql(round(X, "a"), rounder)}
}
\keyword{internal}

