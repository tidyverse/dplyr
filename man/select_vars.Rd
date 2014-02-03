\name{select_vars}
\alias{select_vars}
\title{Select variables.}
\usage{
select_vars(vars, ..., env = parent.frame())
}
\arguments{
  \item{vars}{A character vector of existing column names.}

  \item{...}{Expressions to compute}
}
\description{
Select variables.
}
\section{Selection functions}{
  As well as using existing functions like \code{:} and
  \code{c}, there are a number of special functions that
  only work inside \code{select}

  \itemize{ \item \code{starts_with(x, ignore.case =
  FALSE)}: names starts with \code{x} \item
  \code{ends_with(x, ignore.case = FALSE)}: names ends in
  \code{x} \item \code{contains(x, ignore.case = FALSE)}:
  selects all variables whose name contains \code{x} \item
  \code{matches(x, ignore.case = FALSE)}: selects all
  variables whose name matches the regular expression
  \code{x} \item \code{num_range(x = 1:5)}: selects all
  variables (numerically) from x1 to x5. }

  To exclude instead of including variables, use \code{-}.
}
\examples{
# Keep variables
select_vars(names(iris), starts_with("Petal"))
select_vars(names(iris), ends_with("Width"))
select_vars(names(iris), contains("etal"))
select_vars(names(iris), matches(".t."))
select_vars(names(iris), Petal.Length, Petal.Width)

df <- as.data.frame(matrix(runif(100), nrow = 10))
df <- df[c(3, 4, 7, 1, 9, 8, 5, 2, 6, 10)]
select_vars(names(df), num_range(V = 4:6))

# Drop variables
select_vars(names(iris), -starts_with("Petal"))
select_vars(names(iris), -ends_with("Width"))
select_vars(names(iris), -contains("etal"))
select_vars(names(iris), -matches(".t."))
select_vars(names(iris), -Petal.Length, -Petal.Width)

# Rename variables
select_vars(names(iris), petal_length = Petal.Length)
select_vars(names(iris), petal = starts_with("Petal"))
}
\keyword{internal}

