\name{src_bigquery}
\alias{src_bigquery}
\alias{tbl.src_sqlite}
\title{A bigquery data source.}
\usage{
  src_bigquery(project, dataset, billing = project)

  \method{tbl}{src_bigquery} (src, from, ...)
}
\description{
  A bigquery data source.
}
\section{Caching}{
  The variable names and number of rows are cached on
  source creation, on the assumption that you're probably
  doing analysis on a table that's not changing as you run
  queries. If this is not correct, then the values of
  \code{dim} and \code{dimnames} may be out of date, but it
  shouldn't otherwise affect operation.
}
\examples{
billing <- "341409650721" # put your project number here
samples <- src_bigquery("publicdata", "samples", billing)
births <- tbl(samples, "natality")
dim(births)
colnames(births)

head(births)

summarise(births, first_year = min(year), last_year = max(year))
date_info <- select(births, year:wday)
head(date_info)

head(filter(select(births, year:wday), year > 2000))
by_year_sex <- group_by(births, year, is_male)
wt <- summarise(by_year_sex, n = count(), wt = mean(weight_pounds))
wtdf <- as.data.frame(wt)
}

