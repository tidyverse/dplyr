src_monetdb <- function(dbname, host = "localhost", port = 50000L, user = "monetdb", 
  password = "monetdb", ...) {
  if (!require("MonetDB.R")) {
    stop("MonetDB.R package required to connect to MonetDB", call. = FALSE)
  }
  
  con <- dbi_connect(MonetDB.R(), dbname = dbname , host = host, port = port, 
    username = user, password = password, ...)
  info <- db_info(con)
  
  src_sql("monetdb", con, 
    info = info, disco = db_disconnector(con, "monetdb"))
}

tbl.src_monetdb <- function(src, from, ...) {
  tbl_sql("monetdb", src = src, from = from, ...)
}

brief_desc.src_monetdb <- function(x) {
  info <- x$info
  paste0("MonetDB ",info$monet_version, " (",info$monet_release, ") [", info$merovingian_uri,"]")
}

#' @export
translate_env.src_monetdb <- function(x) {
  sql_variant(
    base_scalar,
    sql_translator(.parent = base_agg,
      n = function() sql("count(*)"),
      # check & extend!
      sd =  sql_prefix("stddev_samp"),
      var = sql_prefix("var_samp"),
      paste = function(x, collapse) build_sql("group_concat(", x, collapse, ")")
    )
  )
}

#' @export
sql_begin_trans.MonetDBConnection <- function(con) {
  qry_run(con, "start transaction")
}

# lifted from postgres equivalent
sql_insert_into.MonetDBConnection <- function(con, table, values) {
  # Convert factors to strings
  is_factor <- vapply(values, is.factor, logical(1))
  values[is_factor] <- lapply(values[is_factor], as.character)

  # Encode special characters in strings
  is_char <- vapply(values, is.character, logical(1))
  values[is_char] <- lapply(values[is_char], encodeString)

  tmp <- tempfile(fileext = ".csv")
  write.table(values, tmp, sep = ",", quote = T,
    row.names = FALSE, col.names = FALSE,na="")

  sql <- build_sql("COPY ",sql(nrow(values))," RECORDS INTO ", ident(table)," FROM ",tmp," USING DELIMITERS ',','\\n','\"' NULL AS ''",
    con = con)
  qry_run(con, sql)

  invisible()
}

# Chuck Norris (and MonetDB) do not need ANALYZE
sql_analyze.MonetDBConnection <- function(con, table) {
  invisible(TRUE) 
}

# MonetDB does not use indices
sql_create_indexes.MonetDBConnection <- function(con, table, indexes = NULL, ...) {
  invisible(TRUE) 
}

