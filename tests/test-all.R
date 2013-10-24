library("testthat")
library("dplyr")

library("Lahman")
lahman_sqlite()

test_check("dplyr")
