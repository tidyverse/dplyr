\name{translate_sql}
\alias{translate_sql}
\title{Translate an expression to sql.}
\usage{
  translate_sql(expr, source = NULL, env = parent.frame(),
    variant = base_sql)
}
\arguments{
  \item{expr}{unevaluated expression to translate}

  \item{source}{tbl}

  \item{variant}{SQL variant to use}

  \item{env}{environment in which to evaluate expression}
}
\description{
  This is a helper function for convenient exploration.
  Otherwise conversion normally happens in two distinct
  phases: first \code{\link{partial_eval}} then
  \code{\link{to_sql}}.
}
\examples{
# Note distinction between integers and reals
translate_sql(Month == 1, hflights)
translate_sql(Month == 1L, hflights)

# Know how to translate most simple mathematical expressions
translate_sql(Month \%in\% 1:3, hflights)
translate_sql(Month >= 1L & Month <= 3L, hflights)
translate_sql((Month >= 1L & Month <= 3L) | Carrier == "AA", hflights)

# Some R functions don't have equivalents in SQL: where possible they
# will be translated to the equivalent
translate_sql(xor(Month <= 3L, Carrier == "AA"), baseball)

# Local variables will be automatically inserted into the SQL
x <- 5L
translate_sql(Month == x, hflights)

# By default all computation will happen in sql
translate_sql(Month < 1 + 1, hflights)
# Use local to force local evaluation
translate_sql(Month < local(1 + 1), hflights)

# This is also needed if you call a local function:
inc <- function(x) x + 1
translate_sql(Month == inc(x), hflights)
translate_sql(Month == local(inc(x)), hflights)

# For testing, translate_sql can be run with source ommitted. In this
# case all variables will be treated as local.
x <- 1
y <- 2L
translate_sql(x ^ y)
translate_sql(inc(x))
}

