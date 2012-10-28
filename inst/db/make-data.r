library(RSQLite)
data("baseball", package = "plyr")
drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname = "baseball.sqlite3")

dbWriteTable(con, "baseball", baseball, row.names = FALSE)
