#' @examples
#' path <- system.file("db/baseball.sqlite3", package = "dply")
#' path <- "inst/db/baseball.sqlite3"
#' baseball_s <- sqlite_source(path, "baseball")
sqlite_source <- function(path, table) {
  if (!require("RSQLite")) {
    stop("RSQLite package required to connect to sqlite db", call. = FALSE)
  }

  con <- dbConnect(dbDriver("SQLite"), dbname = path)

  structure(list(con = con, table = table),
    class = c("source_sqlite", "source_sql", "source", "op"))
}

source_vars.source_sqlite <- function(x) {
  dbListFields(x$con, x$table)
}

source_name.source_sqlite <- function(x) {
  x$table
}

#' @importFrom plyr is.quoted
gen_sql <- function(source, select = NULL, filter = NULL, mutate = NULL,
                    summarise = NULL, arrange = NULL, by = NULL) {
  vars <- source_vars(source)

  stopifnot(is.null(select) || is.quoted(select))
  stopifnot(is.null(filter) || is.quoted(filter))
  stopifnot(is.null(mutate) || is.quoted(mutate))
  stopifnot(is.null(summarise) || is.quoted(summarise))

  if (!is.null(summarise) && !is.null(mutate)) {
    stop("May only specify one of summarise and mutate", call. = FALSE)
  }

  stopifnot(is.null(arrange) || is.quoted(arrange))

  if (is.null(select)) {
    select_sql <- "*"
  } else {
    select <- translate_quoted(select, source)
    select_text <- vapply(select, eval, character(1))

    nms <- names2(select)
    nms <- ifelse(nms == "", "", str_c(" AS ", nms))
    select_sql <- str_c(select_text, nms, collapse = ", ")
  }

  # If group by, need to automatically add fields to select

  if (!is.null(filter)) {
    filter <- translate_quoted(filter, source)
    filter_text <- vapply(filter, eval, character(1))
    filter_sql <- str_c("(", filter_text, ")", collapse = " AND ")
  }

  str_c(
    "SELECT ", select_sql, "\n",
    "FROM ", source_name(source), "\n",
    if (!is.null(filter)) str_c("WHERE ", filter_sql),
    ";\n"
  )
}

#' @examples
#' baseball_s <- sqlite_source(path, "baseball")
#' run_sql(baseball_s, filter = .(id == "ansonca01"))
#'
#' ids <- c("ansonca01", "mathebo01")
#' run_sql(baseball_s, filter = .(id %in% ids))
run_sql <- function(source, select = NULL, filter = NULL, mutate = NULL,
                    summarise = NULL, arrange = NULL) {

  sql <- gen_sql(source, select = select, filter = filter, mutate = mutate,
    summarise = summarise, arrange = arrange)

  qry <- dbSendQuery(source$con, sql)
  on.exit(dbClearResult(qry))

  # Probably should constrain to (e.g.) 50,000 warning user and telling them
  # what parameter to see.
  fetch(qry, -1)
}

translate_quoted <- function(quoted, source) {
  stopifnot(is.quoted(quoted))

  vars <- source_vars(source)
  env <- attr(quoted, "env")

  lapply(quoted, function(call) translate_sql(call, vars, env))
}
