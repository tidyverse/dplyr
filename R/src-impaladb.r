#' Connect to ImpalaDB (http://www.impaladb.org), an Open Source analytics-focused database
#'
#' Use \code{src_impaladb} to connect to an existing ImpalaDB database,
#' and \code{tbl} to connect to tables within that database. Please note that the ORDER BY, LIMIT and OFFSET keywords
#' are not supported in the query when using \code{tbl} on a connection to a ImpalaDB database.
#' If you are running a local database, you only need to define the name of the database you want to connect to.
#' ImpalaDB does nto support anti-joins.
#' Table loading will require rhdfs - https://github.com/RevolutionAnalytics/RHadoop/wiki/rhdfs
#'
#' @template db-info
#' @param dbname Database name
#' @param host,port Host name and port number of database (defaults to localhost:21050)
#' @param user,password User name and password (if needed)
#' @param opts=list() a list of options passed to the ImpalaDB driver - defaults to opts=list(auth="noSasl")
#' @param ... for the src, other arguments passed on to the underlying
#'   database connector, \code{dbConnect}.
#' @param src a ImpalaDB src created with \code{src_impaladb}.
#' @param from Either a string giving the name of table in database, or
#'   \code{\link{sql}} described a derived table or compound join.
#' @export
#' @examples
#' \dontrun{
#' # Connection basics ---------------------------------------------------------
#' # optionally set the class path for the ImpalaDB JDBC connector - alternatively use the CLASSPATH
#' # environment variable
#' options(dplyr.jdbc.classpath = '/path/to/impala-jdbc-0.5-2')
#' # To connect to a database first create a src:
#' my_db <- src_impaladb(dbname="demo")
#' # Then reference a tbl within that src
#' my_tbl <- tbl(my_db, "my_table")
#' }
#'
#' # Here we'll use the Lahman database: to create your own local copy,
#' # create a local database called "lahman" first.
#'
#' if (has_lahman("impaladb")) {
#' # Methods -------------------------------------------------------------------
#' batting <- tbl(lahman_impaladb(), "Batting")
#' dim(batting)
#' colnames(batting)
#' head(batting)
#'
#' # Data manipulation verbs ---------------------------------------------------
#' filter(batting, yearID > 2005, G > 130)
#' select(batting, playerID:lgID)
#' arrange(batting, playerID, desc(yearID))
#' summarise(batting, G = mean(G), n = n())
#' mutate(batting, rbi2 = if(is.null(AB)) 1.0 * R / AB else 0)
#'
#' # note that all operations are lazy: they don't do anything until you
#' # request the data, either by `print()`ing it (which shows the first ten
#' # rows), by looking at the `head()`, or `collect()` the results locally.
#'
#' system.time(recent <- filter(batting, yearID > 2010))
#' system.time(collect(recent))
#'
#' # Group by operations -------------------------------------------------------
#' # To perform operations by group, create a grouped object with group_by
#' players <- group_by(batting, playerID)
#' group_size(players)
#' summarise(players, mean_g = mean(G), best_ab = max(AB))
#'
#' # When you group by multiple level, each summarise peels off one level
#' per_year <- group_by(batting, playerID, yearID)
#' stints <- summarise(per_year, stints = max(stint))
#' filter(stints, stints > 3)
#' summarise(stints, max(stints))
#'
#' # Joins ---------------------------------------------------------------------
#' player_info <- select(tbl(lahman_impaladb(), "Master"), playerID, hofID,
#'   birthYear)
#' hof <- select(filter(tbl(lahman_impaladb(), "HallOfFame"), inducted == "Y"),
#'  hofID, votedBy, category)
#'
#' # Match players and their hall of fame data
#' inner_join(player_info, hof)
#' # Keep all players, match hof data where available
#' left_join(player_info, hof)
#' # Find only players in hof
#' semi_join(player_info, hof)
#' # ImpalaDB cannot do anti-joins
#'
#' # Arbitrary SQL -------------------------------------------------------------
#' # You can also provide sql as is, using the sql function:
#' batting2008 <- tbl(lahman_impaladb(),
#'   sql('SELECT * FROM "Batting" WHERE "yearID" = 2008'))
#' batting2008
#' }

expandAndCheckClassPath <- function(classpath=NULL,
         driverclass="org.apache.hive.jdbc.HiveDriver", jarfiles=c("commons-logging.*.jar", "hive-jdbc.*.jar",
                                                                   "hive-metastore.*.jar", "hive-service.*.jar",
                                                                   "libfb303.*.jar", "libthrift.*.jar",
                                                                   "log4j.*.jar", "slf4j-api.*.jar",
                                                                   "slf4j-log4j.*.jar")) {

  if (is.null(classpath)) classpath <- getOption('dplyr.jdbc.classpath', NULL)
  if (is.null(classpath)) classpath <- unname(Sys.getenv("CLASSPATH"))
  classpath <- unlist(strsplit(classpath, ":"))

  jar.search.path <- c(classpath,
                       ".",
                       Sys.getenv("CLASSPATH"),
                       Sys.getenv("PATH"),
                       if (.Platform$OS == "windows") {
                         file.path(Sys.getenv("PROGRAMFILES"), "Hive")
                       } else c("/usr/local/hive", "/usr/lib/hive"))
  classpath <- lapply(jarfiles,
                      function (x) { head(list.files(path=list.files(path=jar.search.path, full.names=TRUE, all.files=TRUE),
                                                     pattern=paste0("^",x,"$"), full.names=TRUE), 1)})
  do.call(paste, c(as.list(classpath), sep=":"))
}


src_impaladb <- function(dbname, host = "localhost", port = 21050L, user = "", password = "", opts=list(auth="noSasl"), ...) {
  if (!require("RJDBC")) {
    stop("RJDBC package required to connect to ImpalaDB", call. = FALSE)
  }

  driverclass <- "org.apache.hive.jdbc.HiveDriver"
  if (length(names(opts)) > 0) {
    opts <- paste0(";", paste(lapply(names(opts), function(x){paste(x,opts[x], sep="=")}), collapse=";"))
  }
  else {
    opts <- ""
  }

  con <- dbi_connect(JDBC(driverclass,
            expandAndCheckClassPath(driverclass=driverclass),
            identifier.quote='`'), paste0("jdbc:hive2://",
                                           host, ":", as.character(port),
                                           "/", dbname,
                                           opts), ...)
  info <- db_info(con)

  # stash the dbname on the connection for things like db_list_tables
  attr(con, 'dbname') <- dbname

  # I don't trust the connector - so switch the database
  qry_run(con, paste("USE ", dbname))

  env <- environment()
  # temporarily suppress the warning messages from getPackageName()
  wmsg <- getOption('warn')
  options(warn=-1)
  # bless JDBCConnection into ImaplaDB Connection class - this way we can give the JDBC connection
  # driver specific powers
  ImpalaDBConnection <- methods::setRefClass("ImpalaDBConnection", contains = c("JDBCConnection"), where = env)
  options(warn=wmsg)
  con <- structure(con, class = c("ImpalaDBConnection", "JDBCConnection"))

  src_sql("impaladb", con,
    info = info, disco = db_disconnector(con, "impaladb"))
}

double_escape <- function(x) {
  print("SETPUP DOUBLE SQL ESCAPE")
  structure(x, class = c("sql", "sql", "character"))
}

#' @export
impaladb_semi_join <- function(x, y, anti = FALSE, by = NULL, copy = FALSE,
  auto_index = FALSE, ...) {
  # ImpalaDB can't do ANTI Join

  by <- by %||% common_by(x, y)
  y <- auto_copy(x, y, copy, indexes = if (auto_index) list(by))

  con <- x$src$con
  by_escaped <- escape(ident(by), collapse = NULL, con = con)
  left <- escape(ident("_LEFT"), con = con)
  right <- escape(ident("_RIGHT"), con = con)

  join <- sql(paste0(left, ".", by_escaped, " = ", right, ".", by_escaped,
    collapse = " AND "))

  # set the alias attribute for result columns
  pieces <- mapply(function(x, alias) {
      return(paste(sql_quote(alias, "`"), sql_quote(x, "`"), sep="."))
    }, as.character(x$select), rep("_LEFT", length(x$select)))
  fields <- do.call(function(...) {paste(..., sep=", ")}, as.list(pieces))

  # double escape so that list of fields do not get quoted
  fields <- double_escape(fields)

  from <- build_sql(
    'SELECT ', fields, ' FROM ', from(x, "_LEFT"), '\n\n',
    # ' FROM ', from(x, "_LEFT"), '\n\n',
    'LEFT SEMI JOIN \n',
    from(y, "_RIGHT"), '\n',
    ' ON ', join
  )

  update(tbl(x$src, from, vars = x$select), group_by = groups(x))
}

sql_create_table <- function(con, table, types, temporary = FALSE) {
  assert_that(is.string(table), is.character(types))

  # Run a query, abandoning results
  qry_run <- function(con, sql, data = NULL, in_transaction = FALSE,
                      show = getOption("dplyr.show_sql"),
                      explain = getOption("dplyr.explain_sql")) {
    if (show) message(sql)
    if (explain) message(qry_explain(con, sql))

    if (in_transaction) {
      dbBeginTransaction(con)
      on.exit(dbCommit(con))
    }

    if (is.null(data)) {
      res <- dbSendUpdate(con, as.character(sql))
      if (!isTRUE(db_has_table(con, table))) {
        stop("Table ", table, " creation failed.", call. = FALSE)
      }
    } else {
      res <- dbSendPreparedQuery(con, sql, bind.data = data)
    }
    if (!is.null(res)) {
      dbClearResult(res)
    }

    invisible(NULL)
  }

  field_names <- escape(ident(names(types)), collapse = NULL, con = con)
  fields <- sql_vector(paste0(field_names, " ", types), parens = TRUE,
    collapse = ", ", con = con)
  sql <- build_sql("CREATE ", if (temporary) sql("TEMPORARY "),
    "TABLE ", ident(table), " ", fields, " ROW FORMAT DELIMITED FIELDS TERMINATED BY '\\t' ESCAPED BY '\\\\' LINES TERMINATED BY '\\n'", con = con)
  qry_run(con, sql)
}


copy_to.src_impaladb <- function(dest, df, name = deparse(substitute(df)),
                            types = NULL, temporary = FALSE, indexes = NULL,
                            analyze = TRUE, ...) {
  # ImpalaDB can't create temporary tables

  assert_that(is.data.frame(df), is.string(name), is.flag(temporary))
  if (isTRUE(db_has_table(dest$con, name))) {
    stop("Table ", name, " already exists.", call. = FALSE)
  }

  types <- types %||% db_data_type(dest$con, df)
  names(types) <- names(df)

  con <- dest$con

  sql_begin_trans(con)
  sql_create_table(con, name, types, temporary = temporary)
  sql_insert_into(con, name, df)
  sql_create_indexes(con, name, indexes)
  if (analyze) sql_analyze(con, name)
  sql_commit(con)
  tbl(dest, name)
}

#' @export
#' @rdname src_impaladb
tbl.src_impaladb <- function(src, from, ...) {
  # impaladb_check_subquery(from)
  tbl_sql("impaladb", src = src, from = from, ...)
}

#' @export
brief_desc.src_impaladb <- function(x) {
  paste0("ImpalaDB ",x$info$monet_version, " (",x$info$monet_release, ") [", x$info$merovingian_uri,"]")
}

#' @export
translate_env.src_impaladb <- function(x) {
  sql_variant(
    base_scalar,
    sql_translator(.parent = base_agg,
      n = function() sql("COUNT(*)"),
      sd =  sql_prefix("STDDEV_SAMP"),
      var = sql_prefix("VAR_SAMP"),
      median = sql_prefix("MEDIAN")
    )
  )
}

#' @export
db_has_table.ImpalaDBConnection <- function(con, table) {
  # ImpalaDB lower cases the table names silently on creation
  tolower(table) %in% db_list_tables(con)
}

#' @export
db_list_tables.ImpalaDBConnection <- function(con) {
   tables <- dbGetTables(con)
   # use dbGetTables instead of dbListTables as this is not scoped by dbname
   # and we do cannot tell what they belong to
   tables <- tables[tables$TABLE_SCHEM == attr(con, 'dbname'),'TABLE_NAME']
   tables
}

#' @export
sql_begin_trans.ImpalaDBConnection <- function(con) {
  return(TRUE)
}

#' @export
sql_commit.ImpalaDBConnection <- function(con) {
  invisible(TRUE)
}

#' @export
sql_insert_into.ImpalaDBConnection <- function(con, table, values) {
  # Convert factors to strings
  is_factor <- vapply(values, is.factor, logical(1))
  values[is_factor] <- lapply(values[is_factor], as.character)

  # Encode special characters in strings
  is_char <- vapply(values, is.character, logical(1))
  values[is_char] <- lapply(values[is_char], encodeString)

  tmp <- tempfile(fileext = ".csv")
  write.table(values, tmp, sep = "\t", quote = FALSE,
    row.names = FALSE, col.names = FALSE,na="")

  if (!require("rhdfs")) {
    stop("rhdfs package required to connect to store data in ImpalaDB", call. = FALSE)
  }
  if (is.null(Sys.getenv("HADOOP_CMD"))) {
    stop("rhdfs package requires HADOOP_CMD environment variable to be set eg: HADOOP_CMD=/usr/bin/hadoop", call. = FALSE)
  }
  hdfs.init()
  hdfs.put(tmp, '/tmp')
  tmp <- paste0('/tmp/', tail(unlist(strsplit(tmp, '/')), n=1))

  sql <- build_sql("LOAD DATA INPATH ", tmp," INTO TABLE ", ident(table), con = con)
  qry_run(con, sql)

  invisible()
}


#' @export
db_data_type.ImpalaDBConnection <- function(con, fields) {

  data_type <- function(x) {
    switch(class(x)[1],
      logical = "boolean",
      integer = "integer",
      numeric = "double",
      factor =  "STRING",
      character = "STRING",
      Date =    "TIMESTAMP",
      POSIXct = "TIMESTAMP",
      stop("Unknown class ", paste(class(x), collapse = "/"), call. = FALSE)
    )
  }
  vapply(fields, data_type, character(1))
}

#' @export
sql_analyze.ImpalaDBConnection <- function(con, table) {
  # Chuck Norris (and ImpalaDB) do not need ANALYZE
  sql <- build_sql("COMPUTE STATS ", ident(table), con = con)
  qry_run(con, sql)
}

#' @export
sql_create_indexes.ImpalaDBConnection <- function(con, table, indexes = NULL, ...) {
  # ImpalaDB does not benefit from indices
  invisible(TRUE)
}

#' @export
qry_fields.ImpalaDBConnection <- function(con, from) {
  # doesn't like the ; on the end
  qry <- dbGetQuery(con, build_sql("SELECT * FROM ", from, " WHERE 0=1", con = con))
  names(qry)
}

#' @export
table_fields.ImpalaDBConnection <- function(con, table) qry_fields(con, table)

#' @export
query.ImpalaDBConnection <- function(con, sql, .vars) {
  assert_that(is.string(sql))
  ImpalaDBQuery$new(con = con, sql = sql(sql), .vars = .vars, .res = NULL, .nrow = NULL)
}

#' @export
escape_ident.ImpalaDBConnection <- function(con, x, alias=NULL) {
  if (is.null(alias)) {
    return(sql_quote(x, "`"))
  }
  else {
    return(paste(sql_quote(alias, "`"), sql_quote(x, "`"), sep="."))
  }
}

#' @export
qry_fetch.ImpalaDBConnection <- function(con, sql, n = -1L,
                                    show = getOption("dplyr.show_sql"),
                                    explain = getOption("dplyr.explain_sql")) {

  if (show) message(sql)
  if (explain) message(qry_explain(con, sql))

  res <- dbSendQuery(con, sql)
  on.exit(dbClearResult(res))

  out <- fetch(res, n)

  # are there anymore ?
  nrec <- fetch(res, 1)
  if (length(nrec[[1]]) > 0) {
    warning("Only first ", rows, " results retrieved. Use n = -1 to retrieve all.",
      call. = FALSE)
  }
  out
}

#' @export
ImpalaDBQuery <- methods::setRefClass("ImpalaDBQuery", contains = "Query",  methods = list(
  # ImpalaDB needs the WITH DATA in the end
  save_into = function(name = random_table_name()) {
    tt_sql <- build_sql("CREATE TEMPORARY TABLE ", ident(name), " AS ", sql," WITH DATA",
                        con = con)
    qry_run(con, tt_sql)

    name
  },

  from = function() {
    if (is.ident(sql)) {
      sql
    } else {
      # impaladb_check_subquery(sql)
      build_sql("(", sql, ") AS master", con = con)
    }
  },

  nrow = function() {
    if (!is.null(.nrow)) return(.nrow)

    rows <- build_sql("SELECT count(*) FROM ", from(), con = con)
    .nrow <<- as.integer(qry_fetch(con, rows)[[1]])
    .nrow
  }

))

# impaladb_check_subquery <- function(sql) {
#   if (grepl("ORDER BY|LIMIT|OFFSET", as.character(sql), ignore.case=TRUE)) {
#     stop(from," contains ORDER BY, LIMIT or OFFSET keywords, which are not supported.")
#   }
# }
