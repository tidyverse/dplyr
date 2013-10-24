\name{bench_compare}
\alias{bench_compare}
\alias{bench_srcs}
\alias{compare_srcs}
\alias{compare_tbls}
\title{Compare and benchmark srcs}
\usage{
  bench_srcs(srcs, setup, op, times = 10)

  compare_srcs(srcs, setup, op, ref = NULL,
    compare = equal_data_frame)

  compare_tbls(tbls, op, ref = NULL,
    compare = equal_data_frame)
}
\arguments{
  \item{srcs}{A list of \code{\link{srcs}}.}

  \item{setup}{A function with a single argument that is
  called with each src. It should either return a
  \code{\link{tbl}} or a list of \code{tbl}s.}

  \item{op}{A function with a single argument, the output
  of \code{setup}}

  \item{comp}{For checking, an data frame to test results
  against. If not supplied, defaults to the results from
  the first \code{src}.}

  \item{times}{For benchmarking, the number of times each
  operation is repeated.}
}
\value{
  \code{compare_srcs}: an invisible \code{TRUE} on success,
  otherwise throws an error.

  \code{bench_srcs}: an object of class
  \code{\link[microbenchmark]{microbenchmark}}
}
\description{
  These functions support the comparison of results and
  timings across multiple sources.
}
\details{
  Comparisons are performed using \code{equal_data_frame}
  so the order of rows and columns are ignored.
}
\examples{
if (require("Lahman") && require("microbenchmark")) {
lahman_local <- lahman_srcs("df", "dt", "cpp")

# A simple example single tbl
teams <- function(src) src \%.\% tbl("Teams")
y2010 <- function(tbl) tbl \%.\% filter(yearID == 2010)

compare_srcs(lahman_local, teams, y2010)
bench_srcs(lahman_local, teams, y2010)

# A more complicated example using multiple tables
setup <- function(src) {
  list(
    src \%.\% tbl("Batting") \%.\% filter(stint == 1) \%.\% select(playerID:H),
    src \%.\% tbl("Teams") \%.\% select(yearID, lgID, teamID, G, R:H)
  )
}
op <- function(tbls) {
  left_join(tbls[[1]], tbls[[2]], by = c("yearID", "teamID", "lgID"))
}

compare_srcs(lahman_local, setup, op)
bench_srcs(lahman_local, setup, op)

}
}
\seealso{
  \code{\link{src_local}} for working with local data
}

