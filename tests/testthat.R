library("testthat")
library("dplyr")

# Ensure database creation done before tests
library("Lahman")
has_lahman("sqlite")
if (identical(Sys.getenv("NOT_CRAN"), "true")) {
  has_lahman("postgresql")
}

test_check("dplyr")
