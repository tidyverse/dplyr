library("testthat")
library("dplyr")

library("Lahman")
lahman_sqlite()
if (identical(Sys.getenv("NOT_CRAN"), "true")) {
  lahman_postgres()  
}

test_package("dplyr")
