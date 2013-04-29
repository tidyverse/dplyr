library(RSQLite)

path <- "inst/db/baseball.sqlite3"
db <- dbConnect(dbDriver("SQLite"), dbname = path)

sql1 <- "SELECT count(*) FROM baseball;"
sql2 <- "SELECT count(*) FROM (?);"

res <- dbGetPreparedQuery(db, sql2, bind.data = data.frame("baseball"))

sqliteQuickSQL(db, sql1)
sqliteQuickSQL(db, sql2, list("baseball"))

