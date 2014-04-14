library("testthat")
library("dplyr")

# Ensure database creation done before tests
library("Lahman")
lahman_sqlite()
if (identical(Sys.getenv("NOT_CRAN"), "true")) {
  lahman_postgres()  
}

test_check("dplyr")
