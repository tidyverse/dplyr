\docType{data}
\name{base_scalar}
\alias{base_agg}
\alias{base_scalar}
\alias{base_win}
\alias{sql_infix}
\alias{sql_not_supported}
\alias{sql_prefix}
\alias{sql_translator}
\alias{sql_variant}
\title{Create an sql translator}
\usage{
base_scalar

base_agg

base_win

sql_variant(scalar = sql_translator(), aggregate = sql_translator(),
  window = sql_translator())

sql_translator(..., .funs = list(), .parent = new.env(parent = emptyenv()))

sql_infix(f)

sql_prefix(f, n = NULL)

sql_not_supported(f)
}
\arguments{
  \item{scalar,aggregate,window}{The three families of
  functions than an SQL variant can supply.}

  \item{...,.funs}{named functions, used to add custom
  converters from standard R functions to sql functions.
  Specify individually in \code{...}, or provide a list of
  \code{.funs}}

  \item{.parent}{the sql variant that this variant should
  inherit from.  Defaults to \code{base_sql} which provides
  a standard set of mappings for the most common operators
  and functions.}

  \item{f}{the name of the sql function as a string}

  \item{n}{for \code{sql_infix}, an optional number of
  arguments to expect.  Will signal error if not correct.}
}
\description{
When creating a package that maps to a new SQL based src,
you'll often want to provide some additional mappings from
common R commands to the commands that your tbl provides.
These three functions make that easy.
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
# postgresql provides: http://bit.ly/K5EdTn

postgres_agg <- sql_translator(.parent = base_agg,
  cor = sql_prefix("corr"),
  cov = sql_prefix("covar_samp"),
  sd =  sql_prefix("stddev_samp"),
  var = sql_prefix("var_samp")
)
postgres_var <- sql_variant(
  base_scalar,
  postgres_agg
)

translate_sql(cor(x, y), variant = postgres_var)
translate_sql(sd(income / years), variant = postgres_var)

# Any functions not explicitly listed in the converter will be translated
# to sql as is, so you don't need to convert all functions.
translate_sql(regr_intercept(y, x), variant = postgres_var)
}
\seealso{
\code{\link{sql}} for an example of a more customised sql
conversion function.
}
\keyword{datasets}
\keyword{internal}

