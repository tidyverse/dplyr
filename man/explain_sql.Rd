\name{explain_sql}
\alias{explain_sql}
\alias{explain_tbl}
\alias{show_sql}
\title{Show sql and query plans.}
\usage{
  explain_sql(code)

  show_sql(code)

  explain_tbl(tbl)
}
\arguments{
  \item{code}{code to run. All sql queries executed during
  the running of the code will be shown and explained.}

  \item{tbl}{an sql based table to explain.}
}
\description{
  Any queries run inside this function will automatically
  be explained: displaying information about which indexes
  are used to optimise the query. This requires a little
  bit of knowledge about how \code{EXPLAIN} works for your
  database, but is very useful for diagnosing performance
  problems.
}
\examples{
batting <- tbl(lahman(), "Batting")

# Note that you have to do something that actually triggers a query
# inside the explain function
explain_sql(batting)
show_sql(head(batting))
explain_sql(head(batting))
# This has to run two queries because nrow is the same as dim(x)[1]
explain_sql(nrow(batting))

# If you just want to understand the sql for a tbl, use explain_tbl
explain_tbl(batting)

# The batting database has indices on all ID variables:
# SQLite automatically picks the most restrictive index
explain_tbl(filter(batting, lgID == "NL" && yearID == 2000))

# OR's will use multiple indexes
explain_tbl(filter(batting, lgID == "NL" || yearID == 2000))
}

