\name{tbl_postgres}
\alias{tbl.src_postgres}
\alias{tbl_postgres}
\title{Create an postgres tbl.}
\usage{
  tbl_postgres(..., from)

  \method{tbl}{src_postgres} (src, from, ...)
}
\arguments{
  \item{...,src}{either \code{src_postgres} options or a
  \code{src_postgres} object}

  \item{from}{Either a string giving the name of table in
  database, or \code{\link{sql}} described a derived table
  or compound join.}

  \item{...}{other arguments ignored, but needed for
  compatibility with generic.}
}
\description{
  You can create a postgres tbl with all the connection
  information table or with an existing
  \code{\link{src_postgres}} object. You need to use
  \code{src_postgres} if you're working with multiple
  tables from the same database so that they use the same
  connection, and so can perform joins etc.
}
\details{
  To see exactly what SQL is being sent to the database,
  you can set option \code{dplyr.show_sql} to true:
  \code{options(dplyr.show_sql = TRUE).} If you're
  wondering why a particularly query is slow, it can be
  helpful to see the query plan. You can do this by setting
  \code{options(dplyr.explain_sql = TRUE)}. The output of
  postgres's query plans is relatively easy to make sense
  of and is explained at
  \url{http://www.postgresql.org/docs/9.3/static/using-explain.html}.
}
\section{Grouping}{
  Typically you will create a grouped data table is to call
  the \code{group_by} method on a postgres tbl: this will
  take care of capturing the unevalated expressions for
  you.

  For best performance, the database should have an index
  on the variables that you are grouping by. Use
  \code{\link{explain_sql}} to check that postgres is using
  the indexes that you expect.
}
\examples{
\dontrun{
# You can create from a path and a table name
my_tbl <- tbl_postgres(host = "blah.com", user = "hadley",
  password = "pass", from = "my_table")

# Or (better) from a postgres src and a table name
my_db <- src_postgres(host = "blah.com", user = "hadley",
  password = "pass")
my_tbl <- tbl(my_db, "my_table")
}

# Here we'll use the Lahman database: you can create a local copy yourself
# using cache_lahman()
\dontrun{
batting <- tbl(src_postgres("lahman"), "Batting")
dim(batting)
colnames(batting)
head(batting)

# Perform operations "by group":
players <- group_by(batting, id)
group_size(players)

# See examples of data manipulation operations in ?manip_postgres

# You can also provide sql as is, using the sql function:
batting2008 <- tbl(lahman_sqlite(),
  sql("SELECT * FROM Batting WHERE YearID = 2008"))
batting2008
}
}

