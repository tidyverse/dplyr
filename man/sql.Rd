\name{sql}
\alias{escape}
\alias{sql}
\title{SQL escaping.}
\usage{
  sql(...)

  escape(x)
}
\arguments{
  \item{...}{Character vectors that will be combined into a
  single SQL expression}

  \item{x}{An object to escape. Existing sql vectors will
  be left as is, character vectors are escaped with
  \code{'}, numeric vectors have trailing .0 added if
  they're whole numbers.}
}
\description{
  These two functions are critical when writing functions
  that translate R functions to sql functions. Typically a
  conversion function should escape all it's inputs and
  return an sql object.
}
\examples{
escape(1:5)
escape(c(1, 5.4))
escape(sql("X"))

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

