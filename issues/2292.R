library(DBI)
library(dplyr)
"%||%" <- function(x, y) if(is.null(x)) y else x

db_disconnector <- function(con, name, quiet = FALSE) {
  reg.finalizer(environment(), function(...) {
    if (!quiet) {
      message("Auto-disconnecting ", name, " connection ",
              "(", paste(con@Id, collapse = ", "), ")")
    }
    dbDisconnect(con)
  })
  environment()
}

src_postgres2 <- function(dbname = NULL, host = NULL, port = NULL, user = NULL,
                         password = NULL, ...) {
  if (!requireNamespace("RPostgres", quietly = TRUE)) {
    stop("RPostgres package required to connect to postgres db", call. = FALSE)
  }

  user <- user %||% ""

  con <- dbConnect(RPostgres::Postgres(), host = host %||% "", dbname = dbname %||% "",
                   user = user, password = password %||% "", port = port %||% "", ...)
  info <- dbGetInfo(con)

  src_sql("postgres", con,
          info = info, disco = db_disconnector(con, "postgres"))
}

src_postgres2()
