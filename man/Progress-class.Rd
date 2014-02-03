\docType{class}
\name{Progress-class}
\alias{Progress-class}
\title{Progress bar with estimated time.}
\description{
This reference class represents a text progress bar
displayed estimated time remaining. When finished, it
displays the total duration.
}
\section{Methods}{
\itemize{
\item \code{init()}: Initialise timer. Call this before beginning timing.

\item \code{pause(x)}: Sleep for x seconds. Useful for testing.

\item \code{tick()}: Process one element
}}
\examples{
p <- Progress(3)$init()
p$tick()
p$tick()
p$tick()

p <- Progress(3)$init()
for (i in 1:3) p$pause(0.1)$tick()$show()

p <- Progress(3)$init()
p$tick()$stop()
}
\keyword{internal}

