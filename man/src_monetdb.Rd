\name{src_monetdb}
\alias{src_monetdb}
\title{Connect to MonetDB (http://www.monetdb.org), an Open Source analytics-focused database}
\usage{
src_monetdb(dbname, host = "localhost", port = 50000L, user = "monetdb",
  password = "monetdb", ...)
}
\arguments{
  \item{dbname}{Database name}

  \item{host,port}{Host name and port number of database
  (defaults to localhost:50000)}

  \item{user,password}{User name and password (if needed)}

  \item{...}{for the src, other arguments passed on to the
  underlying database connector, \code{dbConnect}.}

  \item{src}{a MonetDB src created with
  \code{src_monetdb}.}

  \item{from}{Either a string giving the name of table in
  database, or \code{\link{sql}} described a derived table
  or compound join.}
}
\description{
Use \code{src_monetdb} to connect to an existing MonetDB database,
and \code{tbl} to connect to tables within that database.
If you are running a local database with, you only need to define the name of the database you want to connect to.
}
\section{Debugging}{
  To see exactly what SQL is being sent to the database,
  you can set option \code{dplyr.show_sql} to true:
  \code{options(dplyr.show_sql = TRUE).} If you're
  wondering why a particularly query is slow, it can be
  helpful to see the query plan. You can do this by setting
  \code{options(dplyr.explain_sql = TRUE)}.
}

\section{Grouping}{
  Typically you will create a grouped data table is to call
  the \code{group_by} method on a mysql tbl: this will take
  care of capturing the unevalated expressions for you.

  For best performance, the database should have an index
  on the variables that you are grouping by. Use
  \code{\link{explain_sql}} to check that mysql is using
  the indexes that you expect.
}

\section{Output}{
  All data manipulation on SQL tbls are lazy: they will not
  actually run the query or retrieve the data unless you
  ask for it: they all return a new \code{\link{tbl_sql}}
  object. Use \code{\link{compute}} to run the query and
  save the results in a temporary in the database, or use
  \code{\link{collect}} to retrieve the results to R.

  Note that \code{do} is not lazy since it must pull the
  data into R. It returns a \code{\link{tbl_df}} or
  \code{\link{grouped_df}}, with one column for each
  grouping variable, and one list column that contains the
  results of the operation. \code{do} never simplifies its
  output.
}

\section{Query principles}{
  This section attempts to lay out the principles governing
  the generation of SQL queries from the manipulation
  verbs.  The basic principle is that a sequence of
  operations should return the same value (modulo class)
  regardless of where the data is stored.

  \itemize{ \item \code{arrange(arrange(df, x), y)} should
  be equivalent to \code{arrange(df, y, x)}

  \item \code{select(select(df, a:x), n:o)} should be
  equivalent to \code{select(df, n:o)}

  \item \code{mutate(mutate(df, x2 = x * 2), y2 = y * 2)}
  should be equivalent to \code{mutate(df, x2 = x * 2, y2 =
  y * 2)}

  \item \code{filter(filter(df, x == 1), y == 2)} should be
  equivalent to \code{filter(df, x == 1, y == 2)}

  \item \code{summarise} should return the summarised
  output with one level of grouping peeled off. }
}
\examples{
\dontrun{
# Connection basics ---------------------------------------------------------
# To connect to a database first create a src:
my_db <- src_monetdb(dbname="demo",host = "localhost", port=50000, user = "monetdb",
  password = "monetdb")
# Then reference a tbl within that src
my_tbl <- tbl(my_db, "my_table")
}

# Here we'll use the Lahman database: to create your own local copy,
# create a local database called "lahman" first.

if (has_lahman("monetdb")) {
# Methods -------------------------------------------------------------------
batting <- tbl(lahman_monetdb(), "Batting")
dim(batting)
colnames(batting)
head(batting)

# Data manipulation verbs ---------------------------------------------------
filter(batting, yearID > 2005, G > 130)
select(batting, playerID:lgID)
arrange(batting, playerID, desc(yearID))
summarise(batting, G = mean(G), n = n())
mutate(batting, rbi2 = if(is.null(AB)) 1.0 * R / AB else 0)

# note that all operations are lazy: they don't do anything until you
# request the data, either by `print()`ing it (which shows the first ten
# rows), by looking at the `head()`, or `collect()` the results locally.

system.time(recent <- filter(batting, yearID > 2010))
system.time(collect(recent))

# Group by operations -------------------------------------------------------
# To perform operations by group, create a grouped object with group_by
players <- group_by(batting, playerID)
group_size(players)
summarise(players, mean_g = mean(G), best_ab = max(AB))

# When you group by multiple level, each summarise peels off one level
per_year <- group_by(batting, playerID, yearID)
stints <- summarise(per_year, stints = max(stint))
filter(stints, stints > 3)
summarise(stints, max(stints))

# Joins ---------------------------------------------------------------------
player_info <- select(tbl(lahman_monetdb(), "Master"), playerID, hofID,
  birthYear)
hof <- select(filter(tbl(lahman_monetdb(), "HallOfFame"), inducted == "Y"),
 hofID, votedBy, category)

# Match players and their hall of fame data
inner_join(player_info, hof)
# Keep all players, match hof data where available
left_join(player_info, hof)
# Find only players in hof
semi_join(player_info, hof)
# Find players not in hof
anti_join(player_info, hof)

# Arbitrary SQL -------------------------------------------------------------
# You can also provide sql as is, using the sql function:
batting2008 <- tbl(lahman_monetdb(),
  sql('SELECT * FROM "Batting" WHERE "yearID" = 2008'))
batting2008
}
}

