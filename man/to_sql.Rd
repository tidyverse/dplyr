\name{to_sql}
\alias{to_sql}
\title{Translate an expression to sql.}
\usage{
  to_sql(x)
}
\arguments{
  \item{x}{an expression or list of an expressions. These
  should have been preprocessed by
  \code{\link{partial_eval}} so the are no longer dependent
  on the environment where they were defined.}
}
\description{
  Translate an expression to sql.
}
\keyword{internal}

