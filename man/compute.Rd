\name{compute}
\alias{collapse}
\alias{collect}
\alias{compute}
\title{Compute a lazy tbl.}
\usage{
compute(x, name = random_table_name(), ...)

collect(x, ...)

collapse(x, ...)
}
\arguments{
  \item{x}{a data tbl}

  \item{name}{name of temporary table on database.}

  \item{...}{other arguments passed on to methods}
}
\description{
\code{compute} forces computation of lazy tbls, leaving
data in the remote source. \code{collect} also forces
computation, but will bring data back into an R data.frame
(stored in a \code{\link{tbl_df}}). \code{collapse} doesn't
force computation, but collapses a complex tbl into a form
that additional restrictions can be placed on.
}
\section{Grouping}{
  \code{compute} and \code{collect} preserve grouping,
  \code{collapse} drops it.
}
\examples{
\donttest{
if (require("RSQLite") && has_lahman("sqlite")) {
batting <- tbl(lahman_sqlite(), "Batting")
remote <- select(filter(batting, yearID > 2010 && stint == 1), playerID:H)
remote2 <- collapse(remote)
cached <- compute(remote)
local  <- collect(remote)
}
}
}
\seealso{
\code{\link{copy_to}} which is the conceptual opposite: it
takes a local data frame and makes it available to the
remote source.
}

