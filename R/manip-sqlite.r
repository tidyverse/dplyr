#' Data manipulation for SQL tbls.
#'
#' Arrange, filter and select are lazy: they modify the object representing
#' the table, and do not recompute unless needed.  Summarise and mutate
#' are eager: they will always return a tbl_df.
#'
#' @param .data an SQLite data base
#' @param ... variables interpreted in the context of \code{.data}
#' @param .n maximum number of columns to return. Set to \code{-1} to return
#'  all.
#' @examples
#' db_path <- system.file("db", "baseball.sqlite3", package = "dplyr")
#' baseball_s <- tbl_sqlite(db_path, "baseball")
#'
#' # filter, select and arrange lazily modify the specification of the table
#' # they don't execute queries unless you print them
#' filter(baseball_s, year > 2005, g > 130)
#' select(baseball_s, id:team)
#' arrange(baseball_s, id, desc(year))
#'
#' # summarise and mutate always return data frame tbls
#' summarise(baseball_s, g = mean(g), n = count())
#' mutate(baseball_s, rbi = 1.0 * r / ab)
#'
#' # Grouped summaries -----------------------------------
#' players <- group_by(baseball_s, id)
#' 
#' # Due to the lack of windowing functions in SQLite, only summarising
#' # is really useful with grouped values
#' summarise(players, g = mean(g))
#' summarise(players, g = mean(g), best_ab = max(ab))
#'
#' per_year <- group_by(baseball_s, id, year)
#' stints <- summarise(per_year, stints = max(stint))
#' collect(filter(stints, stints > 3))
#'
#' # All other operations will ignore grouping, although they will preserve it
#' # in the object returned to R.
#' filter(players, g > 100)
#' mutate(players, rbi = 1 * r / ab)
#' arrange(players, id, desc(year))
#' select(players, id:team)
#'
#' # NB: If you use an aggregation function, you will get one row:
#' mutate(players, cyear = year - min(year) + 1)
#' summarise(players, g = mean(g), n = count())
#'
#' # do by
#' mods <- do(by_team, failwith(NULL, lm), formula = r ~ poly(year, 2), 
#'   .chunk_size = 1000)
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
#' @name manip_sqlite
NULL

#' @rdname manip_sqlite
#' @export
#' @method filter tbl_sqlite
filter.tbl_sqlite <- function(.data, ...) {
  if (!is.null(.data$group_by)) {
    stop("Grouped filter not supported by SQLite", call. = FALSE)
  }
  
  input <- partial_eval(dots(...), .data, parent.frame())
  .data$filter <- c(.data$filter, input)
  .data
}

#' @rdname manip_sqlite
#' @export
#' @method arrange tbl_sqlite
arrange.tbl_sqlite <- function(.data, ...) {
  input <- partial_eval(dots(...), .data, parent.frame())
  .data$arrange <- c(.data$arrange, input)
  .data
}

#' @rdname manip_sqlite
#' @export
#' @method select tbl_sqlite
select.tbl_sqlite <- function(.data, ...) {
  input <- var_eval(dots(...), .data, parent.frame())
  .data$select <- ident(c(.data$select, input))
  .data
}

#' @rdname manip_sqlite
#' @export
#' @method summarise tbl_sqlite
summarise.tbl_sqlite <- function(.data, ..., .n = 1e5) {
  assert_that(length(.n) == 1, .n > 0L)

  .data$select <- trans_sqlite(dots(...), .data, parent.frame())
    
  collapse(.data)
}

#' @rdname manip_sqlite
#' @export
#' @method mutate tbl_sqlite
mutate.tbl_sqlite <- function(.data, ..., .n = 1e5) {
  assert_that(length(.n) == 1, .n > 0L)

  old_vars <- .data$select %||% sql("*")
  new_vars <- trans_sqlite(dots(...), .data, parent.frame())
  .data$select <- c(old_vars, new_vars)
  
  .data
}

#' @S3method do grouped_sqlite
do.grouped_sqlite <- function(.data, .f, ..., .chunk_size = 1e5L) {
  group_names <- var_names(.data$group_by)
  group_vars <- lapply(group_names, as.name)
  nvars <- length(.data$group_by)
  
  vars <- .data$select %||% setdiff(tbl_vars(.data), group_names)
  
  select <- qry_select(.data, 
    from = .data$table,
    select = c(trans_sqlite(.data$group_by), vars),
    where = trans_sqlite(.data$filter),
    order_by = c(var_names(.data$group_by), trans_sqlite(.data$arrange)))$sql
  
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
