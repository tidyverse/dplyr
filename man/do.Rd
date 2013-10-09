\name{do}
\alias{do}
\alias{do.tbl_sql}
\title{Apply a function to a tbl}
\usage{
  do(.data, .f, ...)

  \method{do}{tbl_sql} (.data, .f, ...,
    .chunk_size = 10000L)
}
\arguments{
  \item{.data}{a tbl}

  \item{.f}{a function to apply to each piece. The first
  unnamed argument supplied to \code{.f} will be a data
  frame.}

  \item{...}{other arguments passed on to the function ()}

  \item{.chunk_size}{The size of each chunk to pull into R.
  If this number is too big, the process will be slow
  because R has to allocate and free a lot of memory. If
  it's too small, it will be slow, because of the overhead
  of talking to the database.}
}
\description{
  This is a general purpose complement to the specialised
  manipulation functions \code{\link{filter}},
  \code{\link{select}}, \code{\link{mutate}},
  \code{\link{summarise}} and \code{\link{arrange}}.
}
\examples{
by_dest <- group_by(hflights, Dest)
do(by_dest, nrow)
# Inefficient version of
group_size(by_dest)

# You can use it to do any arbitrary computation, like fitting a linear
# model. Let's explore how carrier departure delays vary over the course
# of a year
hflights <- mutate(hflights, date = ISOdate(Year, Month, DayofMonth))
carriers <- group_by(hflights, UniqueCarrier)
group_size(carriers)

mods <- do(carriers, failwith(NULL, lm), formula = ArrDelay ~ date)
sapply(mods, coef)
}

