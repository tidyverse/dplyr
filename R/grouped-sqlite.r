#' A grouped sqlite database.
#'
#' Typically you will create a grouped data table is to call the \code{group_by}
#' method on a sqlite tbl: this will take care of capturing
#' the unevalated expressions for you.
#'
#' @section Performance:
#'
#' For best performance, the database should have an index on the variables
#' that you are grouping by. A good introduction to how indices affect database
#' performance can be found at \url{http://www.sqlite.org/queryplanner.html}.
#'
#' @param source a tbl or data frame.
#' @param vars a list of quoted variables.
#' @param group_by \code{vars} partially evaluated in the correct environment
#' @export
#' @examples
#' db_path <- system.file("db", "baseball.sqlite3", package = "dplyr")
#' baseball_s <- tbl_sqlite(db_path, "baseball")
#'
#' by_year_lg <- group_by(baseball_s, year, lg)
#' group_size(by_year_lg)
#' summarise(by_year_lg, players = count(), avg_g = mean(g))
#'
#' by_team <- group_by(baseball_s, team)
#' summarise(by_team, players = count())
#'
#' # do by
#' mods <- do(by_team, failwith(NULL, lm), formula = r ~ poly(year, 2), .chunk_size = 1000)
#'
#' sizes <- summarise(by_team, freq = count())
#' not_small <- as.data.frame(filter(sizes, freq > 10))
#' teams <- not_small$team
#' ok <- filter(by_team, team %in% teams)
#' mods <- do(ok, failwith(NULL, lm), formula = r ~ poly(year, 2),
#'   .chunk_size = 1000)
#'
#' # Since we it's not easy to figure out what variables you are using
#' # in general, it will often be faster to let dplyr know what you need
#' mods <- do(filter(ok, year, r), failwith(NULL, lm), formula = r ~ poly(year, 2),
#'   .chunk_size = 1000)
grouped_sqlite <- function(source, vars, group_by) {
  source$vars <- vars
  source$group_by <- group_by

  structure(source, class = c("grouped_sqlite", "tbl_sqlite", "tbl"))
}

#' @export
#' @rdname grouped_sqlite
#' @method group_by tbl_sqlite
#' @param x an existing sqlite tbl
#' @param ... expressions describing how to group data
group_by.tbl_sqlite <- function(x, ...) {
  vars <- named_dots(...)
  group_by <- partial_eval(vars, x, parent.frame())

  grouped_sqlite(x, vars, group_by)
}

#' @export
#' @rdname grouped_sqlite
#' @method group_by grouped_sqlite
group_by.grouped_sqlite <- function(x, ...) {
  vars <- named_dots(...)
  group_by <- partial_eval(vars, x, parent.frame())

  grouped_sqlite(x, c(x$vars, vars), c(x$group_by, group_by))
}

#' @S3method group_size grouped_sqlite
group_size.grouped_sqlite <- function(x) {
  summarise(x, n = count())$n
}

#' @S3method print grouped_sqlite
print.grouped_sqlite <- function(x, ...) {
  cat("Source: SQLite [", x$path, "]\n", sep = "")
  cat("Table:  ", x$table, " ", dim_desc(x), "\n", sep = "")
  cat("Groups: ", paste0(deparse_all(x$vars), collapse = ", "), "\n", sep = "")

  cat("\n")
  trunc_mat(x)
}

#' @S3method do grouped_sqlite
do.grouped_sqlite <- function(.data, .f, ..., .chunk_size = 1e5L) {
  group_names <- var_names(.data$group_by)
  group_vars <- lapply(group_names, as.name)
  nvars <- length(.data$group_by)

  vars <- .data$select %||% setdiff(tbl_vars(.data), group_names)

  select <- select_query(
    from = .data$table,
    select = c(trans_sqlite(.data$group_by), vars),
    where = trans_sqlite(.data$filter),
    order_by = c(var_names(.data$group_by), trans_sqlite(.data$arrange)))

  qry <- dbSendQuery(.data$src$con, select)
  on.exit(dbClearResult(qry))

  last_group <- NULL
  chunk <- fetch(qry, .chunk_size)
  out <- list()

  while (!dbHasCompleted(qry)) {
    # Last group might be incomplete, so always set it aside and join it
    # with the next chunk. If group size is large than chunk size, it may
    # take a couple of iterations to get the entire group, but that should
    # be an unusual situation.
    if (!is.null(last_group)) chunk <- rbind(last_group, chunk)
    group_id <- id(chunk[seq_len(nvars)])

    last <- group_id == group_id[length(group_id)]
    last_group <- chunk[last, , drop = FALSE]
    chunk <- chunk[!last, , drop = FALSE]

    groups <- grouped_df(chunk, group_vars, lazy = FALSE)
    res <- do(groups, .f, ...)
    out <- c(out, res)

    chunk <- fetch(qry, .chunk_size)
  }

  if (!is.null(last_group)) chunk <- rbind(last_group, chunk)
  groups <- grouped_df(chunk, group_vars)
  res <- do(groups, .f, ...)
  out <- c(out, res)

  out
}
