#' Goal is to make valid sql given inputs.
select_sql <- function(select, from, where = NULL, group_by = NULL,
                     having = NULL, order_by = NULL, limit = NULL,
                     offset = NULL) {

  out <- vector("list", 8)
  names(out) <- c("select", "from", "group_by", "having", "order_by", "limit",
    "offset")

  assert_that(is.character(select), length(select) > 0L)
  out$select <- paste0("SELECT ", paste0(select, collapse = ", "))

  assert_that(is.character(from), length(from) == 1L)
  out$from <- paste0("FROM ", escape_sql(from))

  if (!is.null(where)) {
    assert_that(is.character(where), length(where) == 1L)
    out$where <- paste0("WHERE ", where)
  }

  if (!is.null(group_by)) {
    assert_that(is.character(group_by), length(group_by) == 1L)
    out$group_by <- paste0("GROUP BY ", group_by)
  }

  if (!is.null(having)) {
    assert_that(is.character(having), length(having) == 1L)
    out$having <- paste0("HAVING ", having)
  }

  if (!is.null(order_by)) {
    assert_that(is.character(order_by), length(order_by) == 1L)
    out$order_by <- paste0("ORDER BY ", order_by)
  }

  if (!is.null(limit)) {
    assert_that(is.integer(limit), length(limit) == 1L)
    out$limit <- paste0("LIMIT ", limit)
  }

  if (!is.null(offset)) {
    assert_that(is.integer(offset), length(offset) == 1L)
    out$offset <- paste0("OFFSET ", offset)
  }

  sql <- paste0(unlist(out, use.names = FALSE), collapse = "\n")
  paste0(sql, ";")
}

sql_select <- function(x, ..., n = -1L) {
  assert_that(is.source(x))
  assert_that(is.numeric(n), length(n) == 1)

  sql <- select_sql(from = source_name(x), ...)
  exec_sql(x, sql, n = n)
}

sql_select2 <- function(x, args, n = -1L) {
  assert_that(is.source(x))
  assert_that(is.numeric(n), length(n) == 1)

  sql <- select_sql(from = source_name(x),
    select = args$select,
    where = args$where,
    group_by = args$group_by,
    having = args$having,
    order_by = args$order_by,
    limit = args$limit,
    offset = args$offset)

  exec_sql(x, sql, n = n)
}

exec_sql <- function(x, sql, n = -1L) {
  assert_that(is.source(x))
  assert_that(is.string(sql))

  if (isTRUE(getOption("dplyr.show_sql"))) {
    message(sql)
  }

  qry <- dbSendQuery(x$con, sql)
  on.exit(dbClearResult(qry))

  res <- fetch(qry, n)
  if (!dbHasCompleted(qry)) {
    rows <- formatC(nrow(res), big.mark = ",")
    warning("Only first ", rows, " results retrieved. Use n = -1 to retrieve all.",
      call. = FALSE)
  }
  res

}
