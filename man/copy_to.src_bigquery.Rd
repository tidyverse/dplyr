\name{copy_to.src_bigquery}
\alias{copy_to.src_bigquery}
\title{lahman <- src_bigquery("hadbilling", "lahman")
copy_to(lahman, mtcars)}
\usage{
  copy_to.src_bigquery(dest, df,
    name = deparse(substitute(df)), ...)
}
\description{
  lahman <- src_bigquery("hadbilling", "lahman")
  copy_to(lahman, mtcars)
}

