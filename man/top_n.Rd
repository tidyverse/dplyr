% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/top-n.R
\name{top_n}
\alias{top_n}
\alias{top_frac}
\title{Select top (or bottom) n rows (by value)}
\usage{
top_n(x, n, wt)

top_frac(x, n, wt)
}
\arguments{
\item{x}{A data frame.}

\item{n}{Number of rows to return for \code{top_n()}, fraction of rows to
return for \code{top_frac()}. If \code{n} is positive, selects the top rows.
If negative, selects the bottom rows.
If \code{x} is grouped, this is the number (or fraction) of rows per group.
Will include more rows if there are ties.}

\item{wt}{(Optional). The variable to use for ordering. If not
specified, defaults to the last variable in the tbl.}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#superseded}{\figure{lifecycle-superseded.svg}{options: alt='[Superseded]'}}}{\strong{[Superseded]}}
\code{top_n()} has been superseded in favour of \code{\link[=slice_min]{slice_min()}}/\code{\link[=slice_max]{slice_max()}}.
While it will not be deprecated in the near future, retirement means
that we will only perform critical bug fixes, so we recommend moving to the
newer alternatives.

\code{top_n()} was superseded because the name was fundamentally confusing as
it returned what you might reasonably consider to be the \emph{bottom}
rows. Additionally, the \code{wt} variable had a confusing name, and strange
default (the last column in the data frame). Unfortunately we could not
see an easy way to fix the existing \code{top_n()} function without breaking
existing code, so we created a new alternative.
}
\examples{
df <- data.frame(x = c(6, 4, 1, 10, 3, 1, 1))

df \%>\% top_n(2)  # highest values
df \%>\% top_n(-2) # lowest values
# now use
df \%>\% slice_max(x, n = 2)
df \%>\% slice_min(x, n = 2)

# top_frac() -> prop argument of slice_min()/slice_max()
df \%>\% top_frac(.5)
# ->
df \%>\% slice_max(x, prop = 0.5)
}
\keyword{internal}
