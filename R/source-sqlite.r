#' @examples
#' path <- system.file("db/baseball.sqlite3", package = "dply")
#' path <- "inst/db/baseball.sqlite3"
#' baseball_s <- sqlite_source(path, "baseball")
#' dim(baseball_s)
#' names(baseball_s)
#' head(baseball_s)
sqlite_source <- function(path, table) {
  if (!require("RSQLite")) {
    stop("RSQLite package required to connect to sqlite db", call. = FALSE)
  }

  con <- dbConnect(dbDriver("SQLite"), dbname = path)

  structure(list(con = con, table = table),
    class = c("source_sqlite", "source_sql", "source", "op"))
}

#' @S3method source_vars source_sqlite
source_vars.source_sqlite <- function(x) {
  dbListFields(x$con, x$table)
}
#' @S3method names source_sqlite
names.source_sqlite <- source_vars.source_sqlite
#' @S3method dimnames source_sqlite
dimnames.source_sqlite <- function(x) {
  list(NULL, source_vars.source_sqlite(x))
}
#' @S3method dim source_sqlite
dim.source_sqlite <- function(x) {
  n <- sql_select(x, "count()", n = 1L)[[1]]

  c(n, length(source_vars(x)))
}

#' @S3method source_name source_sqlite
source_name.source_sqlite <- function(x) {
  x$table
}

#' @S3method head source_sqlite
head.source_sqlite <- function(x, n = 6L, ...) {
  assert_that(length(n) == 1, n > 0L)

  sql_select(x, "*", limit = n, n = n)
}

#' @S3method tail source_sqlite
tail.source_sqlite <- function(x, n = 6L, ...) {
  assert_that(length(n) == 1, n > 0L)

  sql_select(x, "*", limit = n, offset = nrow(x) - n, n = n)
}

#' @importFrom plyr .
#' @S3method subset source_sqlite
subset.source_sqlite <- function(x, subset, select, ..., n = 50000L) {

  sql <- list()
  if (!missing(subset)) {
    sql$where <- translate(x, substitute(subset))
  }

  if (!missing(select)) {
    select <- substitute(select)

    nm <- names(x)
    env <- as.list(setNames(seq_along(nm), nm))
    cols <- nm[eval(select, env, parent.frame())]

    sql$select <- cols
  } else {
    sql$select <- "*"
  }

  sql_select2(x, sql, n = n)
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
    "SELECT ", select_sql,
    "FROM ", source_name(source),
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
