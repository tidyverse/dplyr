#' Create an SQL tbl (abstract)
#'
#' This method shouldn't be called by users - it should only be used by
#' backend implementors who are creating backends that extend the basic
#' sql behaviour.
#'
#' @keywords internal
#' @export
#' @param subclass name of subclass
#' @param ... needed for agreement with generic. Not otherwise used.
#' @param vars If known, the names of the variables in the tbl. This is
#'   relatively expensive to determine automatically, so is cached throughout
#'   dplyr. However, you should usually be able to leave this blank and it
#'   will be determined from the context.
tbl_sql <- function(subclass, src, from, ..., vars = attr(from, "vars")) {
  make_tbl(c("sql", "lazy"), src = src, ops = op_base_remote(src, from))
}

#' @export
same_src.tbl_sql <- function(x, y) {
  if (!inherits(y, "tbl_sql")) return(FALSE)
  same_src(x$src, y$src)
}

#' @export
tbl_vars.tbl_sql <- function(x) {
  op_vars(x$ops)
}

#' @export
groups.tbl_sql <- function(x) {
  op_grps(x$ops)
}

# Grouping methods -------------------------------------------------------------

#' @export
group_size.tbl_sql <- function(x) {
  df <- collect(summarise(x, n = n()))
  df$n
}

#' @export
n_groups.tbl_sql <- function(x) {
  if (is.null(groups(x))) return(1L)

  x %>%
    select_(.dots = groups(x)) %>%
    distinct_(.dots = groups(x), .keep_all = TRUE) %>%
    compute() %>%
    nrow()
}

# Standard data frame methods --------------------------------------------------

#' @export
as.data.frame.tbl_sql <- function(x, row.names = NULL, optional = NULL,
                                  ..., n = 1e5L) {
  as.data.frame(collect(x, n = n))
}

#' @export
#' @rdname dplyr-formatting
print.tbl_sql <- function(x, ..., n = NULL, width = NULL) {
  cat("Source:   query ", dim_desc(x), "\n", sep = "")
  cat("Database: ", src_desc(x$src), "\n", sep = "")

  grps <- op_grps(x$ops)
  if (length(grps) > 0) {
    cat("Groups: ", commas(op_grps(x$ops)), "\n", sep = "")
  }

  cat("\n")

  print(trunc_mat(x, n = n, width = width))
  invisible(x)
}

#' @export
dimnames.tbl_sql <- function(x) {
  list(NULL, op_vars(x$ops))
}

#' @export
dim.tbl_sql <- function(x) {
  c(NA, length(op_vars(x$ops)))
}

#' @export
head.tbl_sql <- function(x, n = 6L, ...) {
  collect(x, n = n, warn_incomplete = FALSE)
}

#' @export
tail.tbl_sql <- function(x, n = 6L, ...) {
  stop("tail() is not supported by sql sources", call. = FALSE)
}

# Joins ------------------------------------------------------------------------

#' Join sql tbls.
#'
#' See \code{\link{join}} for a description of the general purpose of the
#' functions.
#'
#' @section Implementation notes:
#'
#' Semi-joins are implemented using \code{WHERE EXISTS}, and anti-joins with
#' \code{WHERE NOT EXISTS}. Support for semi-joins is somewhat partial: you
#' can only create semi joins where the \code{x} and \code{y} columns are
#' compared with \code{=} not with more general operators.
#'
#' @inheritParams join
#' @param copy If \code{x} and \code{y} are not from the same data source,
#'   and \code{copy} is \code{TRUE}, then \code{y} will be copied into a
#'   temporary table in same database as \code{x}. \code{join} will automatically
#'   run \code{ANALYZE} on the created table in the hope that this will make
#'   you queries as efficient as possible by giving more data to the query
#'   planner.
#'
#'   This allows you to join tables across srcs, but it's potentially expensive
#'   operation so you must opt into it.
#' @param auto_index if \code{copy} is \code{TRUE}, automatically create
#'   indices for the variables in \code{by}. This may speed up the join if
#'   there are matching indexes in \code{x}.
#' @examples
#' \dontrun{
#' if (require("RSQLite") && has_lahman("sqlite")) {
#'
#' # Left joins ----------------------------------------------------------------
#' lahman_s <- lahman_sqlite()
#' batting <- tbl(lahman_s, "Batting")
#' team_info <- select(tbl(lahman_s, "Teams"), yearID, lgID, teamID, G, R:H)
#'
#' # Combine player and whole team statistics
#' first_stint <- select(filter(batting, stint == 1), playerID:H)
#' both <- left_join(first_stint, team_info, type = "inner", by = c("yearID", "teamID", "lgID"))
#' head(both)
#' explain(both)
#'
#' # Join with a local data frame
#' grid <- expand.grid(
#'   teamID = c("WAS", "ATL", "PHI", "NYA"),
#'   yearID = 2010:2012)
#' top4a <- left_join(batting, grid, copy = TRUE)
#' explain(top4a)
#'
#' # Indices don't really help here because there's no matching index on
#' # batting
#' top4b <- left_join(batting, grid, copy = TRUE, auto_index = TRUE)
#' explain(top4b)
#'
#' # Semi-joins ----------------------------------------------------------------
#'
#' people <- tbl(lahman_s, "Master")
#'
#' # All people in half of fame
#' hof <- tbl(lahman_s, "HallOfFame")
#' semi_join(people, hof)
#'
#' # All people not in the hall of fame
#' anti_join(people, hof)
#'
#' # Find all managers
#' manager <- tbl(lahman_s, "Managers")
#' semi_join(people, manager)
#'
#' # Find all managers in hall of fame
#' famous_manager <- semi_join(semi_join(people, manager), hof)
#' famous_manager
#' explain(famous_manager)
#'
#' # Anti-joins ----------------------------------------------------------------
#'
#' # batters without person covariates
#' anti_join(batting, people)
#' }
#' }
#' @name join.tbl_sql
NULL

#' @rdname join.tbl_sql
#' @export
inner_join.tbl_sql <- function(x, y, by = NULL, copy = FALSE,
  suffix = c(".x", ".y"),
  auto_index = FALSE, ...) {
  sql_mutating_join("inner",
    x, y, by = by, copy = copy, suffix = suffix,auto_index = auto_index, ...
  )
}

#' @rdname join.tbl_sql
#' @export
left_join.tbl_sql <- function(x, y, by = NULL, copy = FALSE,
  suffix = c(".x", ".y"),
  auto_index = FALSE, ...) {
  sql_mutating_join("left",
    x, y, by = by, copy = copy, suffix = suffix,auto_index = auto_index, ...
  )
}

#' @rdname join.tbl_sql
#' @export
right_join.tbl_sql <- function(x, y, by = NULL, copy = FALSE,
  suffix = c(".x", ".y"),
  auto_index = FALSE, ...) {
  sql_mutating_join("right",
    x, y, by = by, copy = copy, suffix = suffix,auto_index = auto_index, ...
  )
}

#' @rdname join.tbl_sql
#' @export
full_join.tbl_sql <- function(x, y, by = NULL, copy = FALSE,
  suffix = c(".x", ".y"),
  auto_index = FALSE, ...) {

  sql_mutating_join("full",
    x, y, by = by, copy = copy, suffix = suffix,auto_index = auto_index, ...
  )
}

sql_mutating_join <- function(type, x, y, by = NULL, copy = FALSE,
  suffix = c(".x", ".y"),
  auto_index = FALSE, ...) {
  by <- common_by(by, x, y)
  y <- auto_copy(x, y, copy, indexes = if (auto_index) list(by$y))
  sql <- sql_join(x$src$con, x, y, type = type, by = by, suffix = suffix)
  update(tbl(x$src, sql), group_by = groups(x))
}

#' @rdname join.tbl_sql
#' @export
semi_join.tbl_sql <- function(x, y, by = NULL, copy = FALSE,
  auto_index = FALSE, ...) {
  by <- common_by(by, x, y)
  y <- auto_copy(x, y, copy, indexes = if (auto_index) list(by$y))
  sql <- sql_semi_join(x$src$con, x, y, anti = FALSE, by = by)
  update(tbl(x$src, sql), group_by = groups(x))
}

#' @rdname join.tbl_sql
#' @export
anti_join.tbl_sql <- function(x, y, by = NULL, copy = FALSE,
  auto_index = FALSE, ...) {
  by <- common_by(by, x, y)
  y <- auto_copy(x, y, copy, indexes = if (auto_index) list(by$y))
  sql <- sql_semi_join(x$src$con, x, y, anti = TRUE, by = by)
  update(tbl(x$src, sql), group_by = groups(x))
}

is.join <- function(x) {
  inherits(x, "join")
}

# Set operations ---------------------------------------------------------------

#' @export
intersect.tbl_sql <- function(x, y, copy = FALSE, ...) {
  y <- auto_copy(x, y, copy)
  sql <- sql_set_op(x$src$con, x, y, "INTERSECT")
  update(tbl(x$src, sql), group_by = groups(x))
}
#' @export
union.tbl_sql <- function(x, y, copy = FALSE, ...) {
  y <- auto_copy(x, y, copy)
  sql <- sql_set_op(x$src$con, x, y, "UNION")
  update(tbl(x$src, sql), group_by = groups(x))
}
#' @export
union_all.tbl_sql <- function(x, y, copy = FALSE, ...) {
  y <- auto_copy(x, y, copy)
  sql <- sql_set_op(x$src$con, x, y, "UNION ALL")
  update(tbl(x$src, sql), group_by = groups(x))
}
#' @export
setdiff.tbl_sql <- function(x, y, copy = FALSE, ...) {
  y <- auto_copy(x, y, copy)
  sql <- sql_set_op(x$src$con, x, y, "EXCEPT")
  update(tbl(x$src, sql), group_by = groups(x))
}


# Copying ----------------------------------------------------------------------

#' @export
auto_copy.tbl_sql <- function(x, y, copy = FALSE, ...) {
  copy_to(x$src, as.data.frame(y), random_table_name(), ...)
}

#' Copy a local data frame to a sqlite src.
#'
#' This standard method works for all sql sources.
#'
#' @export
#' @param types a character vector giving variable types to use for the columns.
#'    See \url{http://www.sqlite.org/datatype3.html} for available types.
#' @param temporary if \code{TRUE}, will create a temporary table that is
#'   local to this connection and will be automatically deleted when the
#'   connection expires
#' @param unique_indexes a list of character vectors. Each element of the list
#'   will create a new unique index over the specified column(s). Duplicate rows
#'   will result in failure.
#' @param indexes a list of character vectors. Each element of the list
#'   will create a new index.
#' @param analyze if \code{TRUE} (the default), will automatically ANALYZE the
#'   new table so that the query optimiser has useful information.
#' @inheritParams copy_to
#' @return a sqlite \code{\link{tbl}} object
#' @examples
#' if (requireNamespace("RSQLite")) {
#' db <- src_sqlite(tempfile(), create = TRUE)
#'
#' iris2 <- copy_to(db, iris)
#' mtcars$model <- rownames(mtcars)
#' mtcars2 <- copy_to(db, mtcars, indexes = list("model"))
#'
#' explain(filter(mtcars2, model == "Hornet 4 Drive"))
#'
#' # Note that tables are temporary by default, so they're not
#' # visible from other connections to the same database.
#' src_tbls(db)
#' db2 <- src_sqlite(db$path)
#' src_tbls(db2)
#' }
copy_to.src_sql <- function(dest, df, name = deparse(substitute(df)),
                            types = NULL, temporary = TRUE,
                            unique_indexes = NULL, indexes = NULL,
                            analyze = TRUE, ...) {
  assert_that(is.data.frame(df), is.string(name), is.flag(temporary))
  class(df) <- "data.frame" # avoid S4 dispatch problem in dbSendPreparedQuery

  if (isTRUE(db_has_table(dest$con, name))) {
    stop("Table ", name, " already exists.", call. = FALSE)
  }

  types <- types %||% db_data_type(dest$con, df)
  names(types) <- names(df)

  con <- dest$con
  db_begin(con)
  on.exit(db_rollback(con))

  db_create_table(con, name, types, temporary = temporary)
  db_insert_into(con, name, df)
  db_create_indexes(con, name, unique_indexes, unique = TRUE)
  db_create_indexes(con, name, indexes, unique = FALSE)
  if (analyze) db_analyze(con, name)

  db_commit(con)
  on.exit(NULL)

  tbl(dest, name)
}

#' @export
collapse.tbl_sql <- function(x, vars = NULL, ...) {
  sql <- sql_render(x)
  tbl(x$src, sql_render(x)) %>% group_by_(groups(x))
}

#' @export
#' @rdname compute
compute.tbl_sql <- function(x, name = random_table_name(), temporary = TRUE,
                            unique_indexes = list(), indexes = list(),
                            ...) {
  if (!is.list(indexes)) {
    indexes <- as.list(indexes)
  }
  if (!is.list(unique_indexes)) {
    unique_indexes <- as.list(unique_indexes)
  }
  assert_that(all(unlist(indexes) %in% x$select))
  assert_that(all(unlist(unique_indexes) %in% x$select))
  db_save_query(x$src$con, sql_render(x), name = name, temporary = temporary)
  db_create_indexes(x$src$con, name, unique_indexes, unique = TRUE)
  db_create_indexes(x$src$con, name, indexes, unique = FALSE)
  update(tbl(x$src, name), group_by = groups(x))
}

#' @export
collect.tbl_sql <- function(x, ..., n = 1e5, warn_incomplete = TRUE) {
  assert_that(length(n) == 1, n > 0L)
  if (n == Inf) {
    n <- -1
  }

  sql <- sql_render(x)
  res <- dbSendQuery(x$src$con, sql)
  on.exit(dbClearResult(res))

  out <- dbFetch(res, n)
  if (warn_incomplete) {
    res_warn_incomplete(res)
  }

  grouped_df(out, op_grps(x$ops))
}

# Do ---------------------------------------------------------------------------

#' @export
#' @rdname do
#' @param .chunk_size The size of each chunk to pull into R. If this number is
#'   too big, the process will be slow because R has to allocate and free a lot
#'   of memory. If it's too small, it will be slow, because of the overhead of
#'   talking to the database.
do_.tbl_sql <- function(.data, ..., .dots, .chunk_size = 1e4L) {
  group_by <- .data$group_by
  if (is.null(group_by)) stop("No grouping", call. = FALSE)

  args <- lazyeval::all_dots(.dots, ...)
  named <- named_args(args)

  gvars <- seq_along(group_by)
  # Create data frame of labels
  labels_tbl <- update(.data,
    select = group_by,
    order_by = NULL,
    summarise = TRUE)
  labels <- as.data.frame(labels_tbl)

  n <- nrow(labels)
  m <- length(args)

  out <- replicate(m, vector("list", n), simplify = FALSE)
  names(out) <- names(args)
  p <- progress_estimated(n * m, min_time = 2)
  env <- new.env(parent = lazyeval::common_env(args))

  # Create ungrouped data frame suitable for chunked retrieval
  chunky <- update(.data,
    select = unique(c(group_by, .data$select)),
    order_by = c(unname(group_by), .data$order_by),
    group_by = NULL
  )

  # When retrieving in pages, there's no guarantee we'll get a complete group.
  # So we always assume the last group in the chunk is incomplete, and leave
  # it for the next. If the group size is large than chunk size, it may
  # take a couple of iterations to get the entire group, but that should
  # be an unusual situation.
  last_group <- NULL
  i <- 0

  chunky$query$fetch_paged(.chunk_size, function(chunk) {
    if (!is.null(last_group)) {
      chunk <- rbind(last_group, chunk)
    }

    # Create an id for each group
    grouped <- chunk %>% group_by_(.dots = names(chunk)[gvars])
    index <- attr(grouped, "indices") # zero indexed

    last_group <<- chunk[index[[length(index)]] + 1L, , drop = FALSE]

    for (j in seq_len(n - 1)) {
      env$. <- chunk[index[[j]] + 1L, , drop = FALSE]
      for (k in seq_len(m)) {
        out[[k]][i + j] <<- list(eval(args[[k]]$expr, envir = env))
        p$tick()$print()
      }
    }
    i <<- i + (n - 1)
  })

  # Process last group
  if (!is.null(last_group)) {
    env$. <- last_group
    for (k in seq_len(m)) {
      out[[k]][i + 1] <- list(eval(args[[k]]$expr, envir = env))
      p$tick()$print()
    }
  }

  if (!named) {
    label_output_dataframe(labels, out, groups(.data))
  } else {
    label_output_list(labels, out, groups(.data))
  }
}

