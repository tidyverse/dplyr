\name{raw}
\alias{raw}
\title{Embed raw sql.}
\usage{
  raw(x)
}
\arguments{
  \item{x}{a string containing raw SQL. No escaping is done
  to this string.}
}
\description{
  This function allows you to embed arbitrary sql into an
  expression passed on to an SQL data source.  Ideally, you
  should only have to do this rarely since the built in
  shims should automatically translate your R expression to
  sql. This function for you when the existing translation
  doesn't work.
}
\examples{
translate_sql(5 * raw("RAW SQL"))
}

