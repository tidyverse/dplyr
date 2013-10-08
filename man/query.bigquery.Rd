\name{query.bigquery}
\alias{query.bigquery}
\usage{
  query.bigquery(con, sql)
}
\examples{
billing <- "341409650721" # put your project number here
samples <- src_bigquery("publicdata", "samples", billing)
bq <- big_query(samples$con, ident("natality"))
bq$submit()
bq$wait()
bq$fetch(1000)
bq <- big_query(samples$con, "SELECT * FROM natality WHERE Year == 1979 LIMIT 5")
bq$submit()
bq$wait()
bq$fetch(100)
}

