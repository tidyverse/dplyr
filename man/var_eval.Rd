\name{var_eval}
\alias{var_eval}
\title{Evaluate variable names in the context of a tbl.}
\usage{
  var_eval(exprs, tbl, parent = parent.frame())
}
\arguments{
  \item{exprs}{a list of unevaluated expressions}

  \item{tbl}{a tbl}

  \item{parent}{the parent frame in which to evaluate
  variables/functions not found in \code{tbl}}
}
\description{
  Evaluate variable names in the context of a tbl.
}
\examples{
var_eval(list(quote(mpg:wt)), mtcars)
}

