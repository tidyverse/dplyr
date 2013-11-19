\name{sql_variant}
\alias{sql_infix}
\alias{sql_not_supported}
\alias{sql_prefix}
\alias{sql_variant}
\title{Create an sql variant.}
\usage{
sql_variant(..., .parent = base_sql)

sql_infix(f)

sql_prefix(f)

sql_not_supported(f)
}
\arguments{
  \item{...}{named functions, used to add custom converters
  from standard R functions to sql functions.}

  \item{.parent}{the sql variant that this variant should
  inherit from.  Defaults to \code{base_sql} which provides
  a standard set of mappings for the most common operators
  and functions.}

  \item{f}{the name of the sql function as a string}
}
\description{
  When creating a package that maps to a new SQL based src,
  you'll often want to provide some additional mappings
  from common R commands to the commands that your tbl
  provides. These three functions make that easy.
}
\section{Helper functions}{
  \code{sql_infix} and \code{sql_prefix} create default SQL
  infix and prefix functions given the name of the SQL
  function. They don't perform any input checking, but do
  correctly escape their input, and are useful for quickly
  providing default wrappers for a new SQL variant.
}
\examples{
# An example of adding some mappings for the statistical functions that
# postgresql provides:
# \\url{http://www.postgresql.org/docs/9.2/static/functions-aggregate.html#FUNCTIONS-AGGREGATE-STATISTICS-TABLE}

postgres_stat <- sql_variant(
  cor = sql_prefix("corr"),
  cov = sql_prefix("covar_samp"),
  sd =  sql_prefix("stddev_samp"),
  var = sql_prefix("var_samp")
)

translate_sql(cor(x, y), variant = postgres_stat)
translate_sql(sd(income / years), variant = postgres_stat)

# Any functions not explicitly listed in the converter will be translated
# to sql as is, so you don't need to convert all functions.
translate_sql(regr_intercept(y, x), variant = postgres_stat)
}
\seealso{
  \code{\link{sql}} for an example of a more customised sql
  conversion function.
}
\keyword{internal}

