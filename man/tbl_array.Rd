\name{tbl_array}
\alias{tbl_array}
\title{A tbl based on an array.}
\usage{
  tbl_array(dimensions, measures)
}
\arguments{
  \item{dimensions}{A named list of vectors. A dimension is
  a variable whose values are known before the experiement
  is conducted; they are fixed by design (in \pkg{reshape2}
  they are known as id variables).  \code{tbl_arrays} are
  dense which means that almost every combination of the
  dimensions should have associated measurements: missing
  values require an explicit NA, so if the variables are
  nested, not crossed, the majority of the data structure
  will be empty. Dimensions are typically, but not always,
  categorical variables.}

  \item{measures}{A named list of arrays. A measure is
  something that is actually measured, and is not known in
  advance. The dimension of each array should be the same
  as the length of the dimensions. Measures are typically,
  but not always, continuous values.}
}
\description{
  An array table stores data in a compact array format
  where dimension names are not needlessly repeated. They
  are particularly appropriate for experimental data where
  all combinations of factors are tried (e.g. complete
  factorial designs), or for storing the result of
  aggregations. Compared to data frames, they will occupy
  much less memory when variables are crossed, not nested.
}
\details{
  \code{tbl_array} support is currently experimental and
  little performance optimisation has been done, but you
  may find them useful if your data already comes in this
  form, or you struggle with the memory overhead of the
  sparse/crossed of data frames.
}
\section{Implementation}{
  Manipulation functions: \code{select}, \code{summarise},
  \code{filter}. \code{mutate} should be relatively
  straightforward given the implementation of
  \code{summarise}. It's not obvious how much sense
  \code{arrange} makes for arrays.

  Joins: not implemented. See
  \code{vignettes/joins.graffle} for ideas. Probably
  straightforward if you get the indexes right, and that's
  probably some straightforward array/tensor operation.
}
\examples{
# The built in nasa dataset records meterological data (temperature,
# cloud cover, ozone etc) for a 4d spatio-temporal dataset (lat, long,
# month and year)
nasa
head(as.data.frame(nasa))

titanic <- as.tbl_array(Titanic)
head(as.data.frame(titanic))

admit <- as.tbl_array(UCBAdmissions)
head(as.data.frame(admit))

as.tbl_array(esoph, dim_names = 1:3)

# Some manipulation examples with the NASA dataset --------------------------

# select() operates only on measures: it doesn't affect dimensions in any way
select(nasa, cloudhigh:cloudmid)

# filter() operates only on dimensions
filter(nasa, lat > 0, year == 2000)
# Each component can only refer to one dimensions, ensuring that you always
# create a rectangular subset
\dontrun{filter(nasa, lat > long)}

# Arrange is meaningless for tbl_arrays

by_loc <- group_by(nasa, lat, long)
summarise(by_loc, pressure = max(pressure), temp = mean(temperature))
}
\seealso{
  \code{\link{as.tbl_array}} for ways of coercing existing
  data structures into a \code{tbl_array}.
}

