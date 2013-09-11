\name{tbl_sqlite}
\alias{tbl.src_sqlite}
\alias{tbl_sqlite}
\title{Create an sqlite tbl.}
\usage{
  tbl_sqlite(path, from)

  \method{tbl}{src_sqlite} (src, from, ...)
}
\arguments{
  \item{path,src}{either path to sqlite database, or
  \code{src_sqlite} object}

  \item{from}{Either a string giving the name of table in
  database, or \code{\link{sql}} described a derived table
  or compound join.}

  \item{...}{other arguments ignored, but needed for
  compatibility with generic.}
}
\description{
  You can create a sqlite tbl with a table name and a path
  or \code{\link{src_sqlite}} object. You need to use
  \code{src_sqlite} if you're working with multiple tables
  from the same database so that they use the same
  connection, and so can perform joins etc.
}
\details{
  To see exactly what SQL is being sent to the database,
  you can set option \code{dplyr.show_sql} to true:
  \code{options(dplyr.show_sql = TRUE).} If you're
  wondering why a particularly query is slow, it can be
  helpful to see the query plan. You can do this by setting
  \code{options(dplyr.explain_sql = TRUE)}. The output of
  SQLite's query plans is relatively easy to make sense of
  and is explained at \url{http://www.sqlite.org/eqp.html}.
  You may also find the explanation of how SQL indices
  works to be helpful:
  \url{http://www.sqlite.org/queryplanner.html}.
}
\section{Grouping}{
  Typically you will create a grouped data table is to call
  the \code{group_by} method on a sqlite tbl: this will
  take care of capturing the unevalated expressions for
  you.

  For best performance, the database should have an index
  on the variables that you are grouping by. A good
  introduction to how indices affect database performance
  can be found at
  \url{http://www.sqlite.org/queryplanner.html}. See
  \code{\link{explain_sql}} to check that sqlite is using
  the indexes that you expect.
}
\examples{
\dontrun{
# You can create from a path and a table name
my_tbl <- tbl_sqlite(db_path, "my_table")

# Or (better) from a sqlite src and a table name
my_db <- src_sqlite(db_path)
my_tbl <- tbl(my_db, "my_table")
}

# Here we'll use the built-in Lahman database
batting <- tbl(src_lahman(), "Batting")
dim(batting)
colnames(batting)
head(batting)

# Perform operations "by group":
players <- group_by(batting, id)
group_size(players)

# See examples of data manipulation operations in ?manip_sqlite
}

