\name{translate_sql}
\alias{translate_sql}
\title{Translate an expression to sql.}
\usage{
  translate_sql(expr, source)
}
\arguments{
  \item{expr}{unevaluated expression to translate}

  \item{source}{data source}
}
\description{
  This is a helper function for convenient exploration.
  Otherwise conversion normally happens in two distinct
  phases: first \code{\link{partial_eval}} is called during
  the building phase, and then \code{\link{to_sql}} is
  called during the rendering phase.
}
\examples{
data(baseball, package = "plyr")
translate_sql(year < 1890, baseball)
translate_sql(year \%in\% c(1890, 1891), baseball)
translate_sql(year \%in\% 1890:1900, baseball)
translate_sql(year >= 1890 & year <= 1900, baseball)

x <- 1890
translate_sql(year == x, baseball)

# By default all computation will happen in sql
translate_sql(year < 1889 + 1, baseball)
# Use local to force local evaluation
translate_sql(year < local(1889 + 1), baseball)

# This is also needed if you call a local function:
inc <- function(x) x + 1
\dontrun{translate_sql(year == inc(x), baseball)}
translate_sql(year == local(inc(x)), baseball)
}

