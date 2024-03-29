% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deprec-combine.R
\name{combine}
\alias{combine}
\title{Combine vectors}
\usage{
combine(...)
}
\arguments{
\item{...}{Vectors to combine.}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}

\code{combine()} is deprecated in favour of \code{\link[vctrs:vec_c]{vctrs::vec_c()}}. \code{combine()}
attempted to automatically guess whether you wanted \code{\link[=c]{c()}} or \code{\link[=unlist]{unlist()}},
but could fail in surprising ways. We now believe it's better to be explicit.
}
\examples{
f1 <- factor("a")
f2 <- factor("b")

combine(f1, f2)
# ->
vctrs::vec_c(f1, f1)

combine(list(f1, f2))
# ->
vctrs::vec_c(!!!list(f1, f2))
}
\keyword{internal}
