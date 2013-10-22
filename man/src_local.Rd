\name{src_local}
\alias{src_cpp}
\alias{src_df}
\alias{src_dt}
\alias{src_local}
\title{A local source.}
\usage{
  src_local(tbl, pkg = NULL, env = NULL)

  src_df(pkg = NULL, env = NULL)

  src_dt(pkg = NULL, env = NULL)

  src_cpp(pkg = NULL, env = NULL)
}
\arguments{
  \item{tbl}{name of the function used to generate
  \code{tbl} objects}

  \item{pkg,env}{Either the name of a package or an
  environment object in which to look for objects.}
}
\description{
  This is mainly useful for testing, since makes it
  possible to refer to local and remote tables using
  exactly the same syntax.
}
\details{
  Generally, \code{src_local} should not be called
  directly, but instead one of the (currently three)
  constructors should be used.
}
\examples{
if (require("Lahman")) {
src_dt("Lahman")
src_df("Lahman")

batting_df <- tbl(src_df("Lahman"), "Batting")
batting_dt <- tbl(src_dt("Lahman"), "Batting")
}
}
\keyword{internal}

