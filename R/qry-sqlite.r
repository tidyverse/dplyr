#' @S3method qry_select tbl_sqlite
qry_select.tbl_sqlite <- function(x, select = x$select, from = x$table, 
                                  where = x$where, group_by = x$group_by, 
                                  having = NULL, order_by = x$order_by, 
                                  limit = NULL, offset = NULL) {  
  assert_that(
    is.lang.list(select), 
    is.sql(from),
    is.lang.list(where), 
    is.lang.list(group_by),
    is.lang.list(having), 
    is.lang.list(order_by), 
    is.null(limit) || (is.numeric(limit) && length(limit) == 1), 
    is.null(offset) || (is.lang(offset) && length(offset) == 1))

  if (!has_star(select)) {
    # Can't use unique because it strips names
    select <- c(group_by, select)
    select <- select[!duplicated(select)]
  }
  
  select <- trans_sqlite(select)
  where <- trans_sqlite(where)
  group_by <- trans_sqlite(group_by)
  having <- trans_sqlite(having)
  order_by <- trans_sqlite(order_by)
  
  qry_select(x$src, from = from, select = select, where = where, 
    order_by = order_by, group_by = group_by, limit = limit, offset = offset)
}

#' @S3method qry_select src_sqlite
qry_select.src_sqlite <- function(x, select, from, where = NULL, group_by = NULL,
  having = NULL, order_by = NULL, limit = NULL, offset = NULL) {
  
  out <- vector("list", 8)
  names(out) <- c("select", "from", "where", "group_by", "having", "order_by",
    "limit", "offset")
  
  assert_that(is.character(select), length(select) > 0L)
  out$select <- build_sql("SELECT ", escape(select, collapse = ", "))
  
  assert_that(is.character(from), length(from) == 1L)
  out$from <- build_sql("FROM ", from)
  
  if (length(where) > 0L) {
    assert_that(is.character(where))
    out$where <- build_sql("WHERE ", escape(where, collapse = " AND "))
  }
  
  if (!is.null(group_by)) {
    assert_that(is.character(group_by), length(group_by) > 0L)
    out$group_by <- build_sql("GROUP BY ", escape(group_by, collapse = ", "))
  }
  
  if (!is.null(having)) {
    assert_that(is.character(having), length(having) == 1L)
    out$having <- build_sql("HAVING ", escape(having, collapse = ", "))
  }
  
  if (!is.null(order_by)) {
    assert_that(is.character(order_by), length(order_by) > 0L)
    out$order_by <- build_sql("ORDER BY ", escape(order_by, collapse = ", "))
  }
  
  if (!is.null(limit)) {
    assert_that(is.integer(limit), length(limit) == 1L)
    out$limit <- build_sql("LIMIT ", limit)
  }
  
  if (!is.null(offset)) {
    assert_that(is.integer(offset), length(offset) == 1L)
    out$offset <- build_sql("OFFSET ", offset)
  }
  
  sql <- escape(unname(compact(out)), collapse = "\n", parens = FALSE)
  query(x$con, sql)
}
