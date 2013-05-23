\name{var_eval}
\alias{var_eval}
\title{Evaluate variable names in the context of a data source.}
\usage{
  var_eval(exprs, source, parent = parent.frame())
}
\arguments{
  \item{exprs}{a list of unevaluated expressions}

  \item{source}{a data source}

  \item{parent}{the parent frame in which to evaluate
  variables/functions not found in \code{source}}
}
\description{
  Evaluate variable names in the context of a data source.
}
\examples{
var_eval(list(quote(mpg:wt)), mtcars)
}

