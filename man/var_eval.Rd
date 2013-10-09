\name{var_eval}
\alias{select_eval}
\alias{var_eval}
\title{Evaluate variable names in the context of a tbl.}
\usage{
  var_eval(exprs, tbl, parent = parent.frame())

  select_eval(exprs, select, parent = parent.frame())
}
\arguments{
  \item{exprs}{a list of unevaluated expressions}

  \item{tbl,select}{a tbl or a select language list}

  \item{parent}{the parent frame in which to evaluate
  variables/functions not found in \code{tbl}}
}
\description{
  Evaluate variable names in the context of a tbl.
}
\examples{
var_eval(list(quote(mpg:wt)), mtcars)

select <- lapply(names(mtcars), as.name)
select_eval(list(quote(mpg:wt)), select)

mutate <- c(select, cyl2 = quote(cyl * 2))
select_eval(list(quote(gear:cyl2)), mutate)
}

