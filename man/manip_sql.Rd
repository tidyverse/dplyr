\name{manip_sql}
\alias{arrange.tbl_sql}
\alias{do.tbl_sql}
\alias{filter.tbl_sql}
\alias{manip_sql}
\alias{mutate.tbl_sql}
\alias{select.tbl_sql}
\alias{summarise.tbl_sql}
\title{Data manipulation for SQL tbls.}
\usage{
  \method{filter}{tbl_sql} (.data, ...)

  \method{arrange}{tbl_sql} (.data, ...)

  \method{select}{tbl_sql} (.data, ...)

  \method{summarise}{tbl_sql} (.data, ...)

  \method{mutate}{tbl_sql} (.data, ...)

  \method{do}{tbl_sql} (.data, .f, ...,
    .chunk_size = 10000L)
}
\arguments{
  \item{.data}{an sql data tbl}

  \item{...}{variables interpreted in the context of
  \code{.data}}

  \item{.f,...}{A function to apply to each group, and any
  additional arguments to pass to \code{f}.}

  \item{.chunk_size}{The size of each chunk to pull into R.
  If this number is too big, the process will be slow
  because R has to allocate and free a lot of memory. If
  it's too small, it will be slow, because of the overhead
  of talking to the database.}
}
\description{
  This page documents the specific of data manipulation for
  \code{\link{tbl_sql}} objects. See \code{manip} for the
  documentation of the generics, and how they work in
  general.
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
batting <- tbl(lahman_sqlite(), "Batting")

# filter, select and arrange lazily modify the specification of the table
# they don't execute queries unless you print them
filter(batting, yearID > 2005, G > 130)
select(batting, playerID:lgID)
arrange(batting, playerID, desc(YearID))
summarise(batting, G = mean(G), n = n())
mutate(batting, rbi = 1.0 * R / AB)

# Grouped summaries -----------------------------------
players <- group_by(batting, playerID)

# Due to the lack of windowing functions in SQLite, only summarising
# is really useful with grouped values
summarise(players, mean_g = mean(G), best_ab = max(AB))

# Summarise peels over a single layer of grouping
per_year <- group_by(batting, playerID, yearID)
stints <- summarise(per_year, stints = max(stint))
filter(stints, stints > 3)
summarise(stints, max(stints))

# All other operations will ignore grouping, although they will preserve it
# in the object returned to R.
select(players, playerID:lgID)
arrange(players, playerID, desc(yearID))
mutate(players, rbi = 1.0 * R / (AB + 1))

# NB: If you use an aggregation function with mutate
mutate(players, cyear = YearID - min(YearID) + 1)

# Do arbitrary processing with do ---------------------------------

# First find teams with a decent number of records
teams <- group_by(batting, teamID)
sizes <- summarise(teams, freq = count())
not_small <- filter(sizes, freq > 10)
big_teams <- semi_join(teams, not_small)

# Explore how they have changed over time
mods <- do(ok, failwith(NULL, lm), formula = r ~ poly(year, 2),
  .chunk_size = 1000)

# Note that it's more efficient to select only the variables needed
ok_min <- select(ok, year, r)
mods <- do(ok_min, failwith(NULL, lm), formula = r ~ poly(year, 2),
  .chunk_size = 1000)
}

