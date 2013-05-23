\docType{data}
\name{base_sql}
\alias{base_sql}
\alias{sqlite_sql}
\alias{to_sql}
\alias{to_sql_q}
\title{Translate an R expression to sql.}
\format{<environment: 0x10680d468>}
\usage{
  base_sql

  sqlite_sql

  to_sql(x, variant = base_sql)

  to_sql_q(x, variant = base_sql)
}
\arguments{
  \item{x}{for \code{to_sql}, an unquoted expression; for
  \code{to_sql_q} a quoted expression.}

  \item{variant}{the sql variant to use for translation}
}
\description{
  Translate an R expression to sql.
}
\section{Base translation}{
  The base translator, \code{base_sql}, provides custom
  mappings for \code{!} (to NOT), \code{&&} and \code{&} to
  \code{AND}, \code{||} and \code{|} to \code{OR}, \code{^}
  to \code{POWER}, \code{\%\%} to \code{\%}, \code{ceiling}
  to \code{CEIL}, \code{mean} to \code{AVG}, \code{var} to
  \code{VARIANCE}, \code{tolower} to \code{LOWER},
  \code{toupper} to \code{UPPER} and \code{nchar} to
  \code{length}.

  \code{c} and \code{:} keep their usual R behaviour so you
  can easily create vectors that are passed to sql.

  All other functions will be preserved as is. R's infix
  functions (e.g. \code{\%like\%}) will be converted to
  their sql equivalents (e.g. \code{LIKE}). You can use
  this to access SQL string concatenation: \code{||} is
  mapped to \code{OR}, but \code{\%||\%} is mapped to
  \code{||}.

  You can also use \code{sql} to insert a raw sql string.
}

\section{SQLite translation}{
  The SQLite variant currently only adds one additional
  function: a mapping from \code{sd} to the SQL aggregation
  function \code{stdev}.
}
\examples{
# Regular maths is translated in a very straightforward way
to_sql(x + 1)
to_sql(sin(x) + tan(y))

# Logical operators are converted to their sql equivalents
to_sql(x < 5 & !(y >= 5))

# Infix functions are passed onto SQL with \% removed
to_sql(first \%like\% "Had*")
to_sql(first \%is\% NULL)
to_sql(first \%in\% c("John", "Roger", "Robert"))

# Note that variable names will be escaped if needed
to_sql(like == 7)

# And be careful if you really want integers
to_sql(x == 1)

# If you have an already quoted object, use to_sql_q:
x <- quote(y + 1 / sin(t))
to_sql(x)
to_sql_q(x)
}
\keyword{datasets}

