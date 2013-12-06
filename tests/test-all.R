library("testthat")
library("dplyr")

library("Lahman")
lahman_sqlite()
lahman_postgres()

test_package("dplyr")
