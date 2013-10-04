\name{src_sqlite}
\alias{src_sqlite}
\alias{tbl.src_sqlite}
\title{Connect to a sqlite database.}
\usage{
  src_sqlite(path, create = FALSE)

  \method{tbl}{src_sqlite} (src, from, ...)
}
\arguments{
  \item{path}{Path to SQLite database}

  \item{create}{if \code{FALSE}, \code{path} must already
  exist. If \code{TRUE}, will create a new SQlite3 database
  at \code{path}.}

  \item{src}{a sqlite src created with \code{src_sqlite}.}
}
\description{
  Use \code{src_sqlite} to connect to an existing sqlite
  database, and \code{tbl} to connect to tables within that
  database. If you are running a local sqliteql database,
  leave all parameters set as their defaults to connect. If
  you're connecting to a remote database, ask your database
  administrator for the values of these variables.
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
\examples{
\dontrun{
# Connection basics ---------------------------------------------------------
# To connect to a database first create a src:
my_db <- src_sqlite(path = tempfile(), create = TRUE)
# Then reference a tbl within that src
my_tbl <- tbl(my_db, "my_table")
}

# Here we'll use the Lahman database: to create your own local copy,
# run lahman_sqlite()

if (has_lahman("sqlite")) {
# Methods -------------------------------------------------------------------
batting <- tbl(lahman_sqlite(), "Batting")
dim(batting)
colnames(batting)
head(batting)

# Data manipulation verbs ---------------------------------------------------
filter(batting, yearID > 2005, G > 130)
select(batting, playerID:lgID)
arrange(batting, playerID, desc(yearID))
summarise(batting, G = mean(G), n = n())
mutate(batting, rbi2 = 1.0 * R / AB)

# note that all operations are lazy: they don't do anything until you
# request the data, either by `print()`ing it (which shows the first ten
# rows), by looking at the `head()`, or `collect()` the results locally.

system.time(recent <- filter(batting, yearID > 2010))
system.time(collect(recent))

# Group by operations -------------------------------------------------------
# To perform operations by group, create a grouped object with group_by
players <- group_by(batting, playerID)
group_size(players)

# sqlite doesn't support windowed functions, which means that only
# grouped summaries are really useful:
summarise(players, mean_g = mean(G), best_ab = max(AB))

# When you group by multiple level, each summarise peels off one level
per_year <- group_by(batting, playerID, yearID)
stints <- summarise(per_year, stints = max(stint))
filter(stints, stints > 3)
summarise(stints, max(stints))

# Joins ---------------------------------------------------------------------
player_info <- select(tbl(lahman_sqlite(), "Master"), playerID, hofID,
  birthYear)
hof <- select(filter(tbl(lahman_sqlite(), "HallOfFame"), inducted == "Y"),
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
batting2008 <- tbl(lahman_sqlite(),
  sql("SELECT * FROM Batting WHERE YearID = 2008"))
batting2008
}
}

