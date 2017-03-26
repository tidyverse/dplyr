#' Create an SQL tbl (abstract)
#'
#' Deprecated: you should no longer need to provide a custom `tbl()`
#' method. Instead, you can rely on the default `tbl_dbi` method.
#'
#' @keywords internal
#' @export
#' @param subclass name of subclass
#' @param ... needed for agreement with generic. Not otherwise used.
#' @param vars If known, the names of the variables in the tbl. This is
#'   relatively expensive to determine automatically, so is cached throughout
#'   dplyr. However, you should usually be able to leave this blank and it
#'   will be determined from the context.
tbl_sql <- function(subclass, src, from, ..., vars = NULL) {
  # If not literal sql, must be a table identifier
  if (!is.sql(from)) {
    from <- ident(from)
  }

  vars <- db_vars(src, from)

  make_tbl(
    c(subclass, "sql", "lazy"),
    src = src,
    ops = op_base_remote(from, vars)
  )
}

#' @export
same_src.tbl_sql <- function(x, y) {
  if (!inherits(y, "tbl_sql")) return(FALSE)
  same_src(x$src, y$src)
}

# Grouping methods -------------------------------------------------------------

#' @export
group_size.tbl_sql <- function(x) {
  df <- x %>%
    summarise(n = n()) %>%
    collect()
  df$n
}

#' @export
n_groups.tbl_sql <- function(x) {
  if (length(groups(x)) == 0) return(1L)

  df <- x %>%
    summarise() %>%
    ungroup() %>%
    summarise(n = n()) %>%
    collect()
  df$n
}

# Standard data frame methods --------------------------------------------------

#' @export
as.data.frame.tbl_sql <- function(x, row.names = NULL, optional = NULL,
                                  ..., n = Inf) {
  as.data.frame(collect(x, n = n))
}

#' @export
print.tbl_sql <- function(x, ..., n = NULL, width = NULL) {
  cat("Source:     ", tbl_desc(x), "\n", sep = "")
  cat("Database:   ", src_desc(x$src), "\n", sep = "")

  grps <- op_grps(x$ops)
  if (length(grps) > 0) {
    cat("Grouped by: ", commas(grps), "\n", sep = "")
  }
  sort <- op_sort(x$ops)
  if (length(sort) > 0) {
    cat("Ordered by: ", commas(deparse_all(sort)), "\n", sep = "")
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
tail.tbl_sql <- function(x, n = 6L, ...) {
  stop("tail() is not supported by sql sources", call. = FALSE)
}

# Joins ------------------------------------------------------------------------

#' Join sql tbls.
#'
#' See [join] for a description of the general purpose of the
#' functions.
#'
#' @section Implementation notes:
#'
#' Semi-joins are implemented using `WHERE EXISTS`, and anti-joins with
#' `WHERE NOT EXISTS`. Support for semi-joins is somewhat partial: you
#' can only create semi joins where the `x` and `y` columns are
#' compared with `=` not with more general operators.
#'
#' @inheritParams join
#' @param copy If `x` and `y` are not from the same data source,
#'   and `copy` is `TRUE`, then `y` will be copied into a
#'   temporary table in same database as `x`. `*_join()` will automatically
#'   run `ANALYZE` on the created table in the hope that this will make
#'   you queries as efficient as possible by giving more data to the query
#'   planner.
#'
#'   This allows you to join tables across srcs, but it's potentially expensive
#'   operation so you must opt into it.
#' @param auto_index if `copy` is `TRUE`, automatically create
#'   indices for the variables in `by`. This may speed up the join if
#'   there are matching indexes in `x`.
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
inner_join.tbl_lazy <- function(x, y, by = NULL, copy = FALSE,
                                suffix = c(".x", ".y"),
                                auto_index = FALSE, ...) {
  add_op_join(
    x, y,
    "inner",
    by = by,
    copy = copy,
    suffix = suffix,
    auto_index = auto_index,
    ...
  )
}

#' @rdname join.tbl_sql
#' @export
left_join.tbl_lazy <- function(x, y, by = NULL, copy = FALSE,
                               suffix = c(".x", ".y"),
                               auto_index = FALSE, ...) {
  add_op_join(
    x, y,
    "left",
    by = by,
    copy = copy,
    suffix = suffix,
    auto_index = auto_index,
    ...
  )
}

#' @rdname join.tbl_sql
#' @export
right_join.tbl_lazy <- function(x, y, by = NULL, copy = FALSE,
                                suffix = c(".x", ".y"),
                                auto_index = FALSE, ...) {
  add_op_join(
    x, y,
    "right",
    by = by,
    copy = copy,
    suffix = suffix,
    auto_index = auto_index,
    ...
  )
}

#' @rdname join.tbl_sql
#' @export
full_join.tbl_lazy <- function(x, y, by = NULL, copy = FALSE,
                               suffix = c(".x", ".y"),
                               auto_index = FALSE, ...) {
  add_op_join(
    x, y,
    "full",
    by = by,
    copy = copy,
    suffix = suffix,
    auto_index = auto_index,
    ...
  )
}

#' @rdname join.tbl_sql
#' @export
semi_join.tbl_lazy <- function(x, y, by = NULL, copy = FALSE,
                               auto_index = FALSE, ...) {
  add_op_semi_join(
    x, y,
    anti = FALSE,
    by = by,
    copy = copy,
    auto_index = auto_index,
    ...
  )
}

#' @rdname join.tbl_sql
#' @export
anti_join.tbl_lazy <- function(x, y, by = NULL, copy = FALSE,
                               auto_index = FALSE, ...) {
  add_op_semi_join(
    x, y,
    anti = TRUE,
    by = by,
    copy = copy,
    auto_index = auto_index,
    ...
  )
}

# Set operations ---------------------------------------------------------------

#' @export
intersect.tbl_lazy <- function(x, y, copy = FALSE, ...) {
  add_op_set_op(x, y, "INTERSECT", copy = copy, ...)
}
#' @export
union.tbl_lazy <- function(x, y, copy = FALSE, ...) {
  add_op_set_op(x, y, "UNION", copy = copy, ...)
}
#' @export
union_all.tbl_lazy <- function(x, y, copy = FALSE, ...) {
  add_op_set_op(x, y, "UNION ALL", copy = copy, ...)
}
#' @export
setdiff.tbl_lazy <- function(x, y, copy = FALSE, ...) {
  add_op_set_op(x, y, "EXCEPT", copy = copy, ...)
}

# Copying ----------------------------------------------------------------------

#' @export
auto_copy.tbl_sql <- function(x, y, copy = FALSE, ...) {
  copy_to(x$src, as.data.frame(y), random_table_name(), ...)
}

#' Copy a local data frame to a DBI backend.
#'
#' This [copy_to()] method works for all DBI sources. It is useful for
#' copying small amounts of data to a database for examples, experiments,
#' and joins. By default, it creates temporary tables which are typically
#' only visible to the current connection to the database.
#'
#' @export
#' @param types a character vector giving variable types to use for the columns.
#'    See \url{http://www.sqlite.org/datatype3.html} for available types.
#' @param temporary if `TRUE`, will create a temporary table that is
#'   local to this connection and will be automatically deleted when the
#'   connection expires
#' @param unique_indexes a list of character vectors. Each element of the list
#'   will create a new unique index over the specified column(s). Duplicate rows
#'   will result in failure.
#' @param indexes a list of character vectors. Each element of the list
#'   will create a new index.
#' @param analyze if `TRUE` (the default), will automatically ANALYZE the
#'   new table so that the query optimiser has useful information.
#' @inheritParams copy_to
#' @return A [tbl()] object (invisibly).
#' @examples
#' if (requireNamespace("RSQLite")) {
#' set.seed(1014)
#'
#' mtcars$model <- rownames(mtcars)
#' mtcars2 <- src_memdb() %>%
#'   copy_to(mtcars, indexes = list("model"), overwrite = TRUE)
#' mtcars2 %>% filter(model == "Hornet 4 Drive")
#'
#' # copy_to is called automatically if you set copy = TRUE
#' # in the join functions
#' df <- tibble(cyl = c(6, 8))
#' mtcars2 %>% semi_join(df, copy = TRUE)
#' }
copy_to.src_sql <- function(dest, df, name = deparse(substitute(df)),
                            overwrite = FALSE, types = NULL, temporary = TRUE,
                            unique_indexes = NULL, indexes = NULL,
                            analyze = TRUE, ...) {
  assert_that(is.data.frame(df), is_string(name), is.flag(temporary))
  class(df) <- "data.frame" # avoid S4 dispatch problem in dbSendPreparedQuery

  con <- con_acquire(dest)
  tryCatch({
    types <- types %||% db_data_type(con, df)
    names(types) <- names(df)

    db_begin(con)
    tryCatch({
      if (overwrite) {
        db_drop_table(con, name, force = TRUE)
      }

      db_create_table(con, name, types, temporary = temporary)
      db_insert_into(con, name, df)
      db_create_indexes(con, name, unique_indexes, unique = TRUE)
      db_create_indexes(con, name, indexes, unique = FALSE)
      if (analyze) db_analyze(con, name)

      db_commit(con)
    }, error = function(err) {
      db_rollback(con)
      stop(err)
    })
  }, finally = {
    con_release(dest, con)
  })

  invisible(tbl(dest, name))
}

#' @export
collapse.tbl_sql <- function(x, vars = NULL, ...) {
  con <- con_acquire(x$src)
  tryCatch({
    sql <- sql_render(x, con)
  }, finally = {
    con_release(x$src, con)
  })

  tbl(x$src, sql) %>%
    group_by(!!! syms(op_grps(x))) %>%
    add_op_order(op_sort(x))
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

  con <- con_acquire(x$src)
  tryCatch({
    vars <- op_vars(x)
    assert_that(all(unlist(indexes) %in% vars))
    assert_that(all(unlist(unique_indexes) %in% vars))
    x_aliased <- select(x, !!! syms(vars)) # avoids problems with SQLite quoting (#1754)
    db_save_query(con, sql_render(x_aliased, con), name = name, temporary = temporary)
    db_create_indexes(con, name, unique_indexes, unique = TRUE)
    db_create_indexes(con, name, indexes, unique = FALSE)
  }, finally = {
    con_release(x$src, con)
  })

  tbl(x$src, name) %>%
    group_by(!!! syms(op_grps(x))) %>%
    add_op_order(op_sort(x))
}

#' @export
collect.tbl_sql <- function(x, ..., n = Inf, warn_incomplete = TRUE) {
  assert_that(length(n) == 1, n > 0L)
  if (n == Inf) {
    n <- -1
  } else {
    # Gives the query planner information that it might be able to take
    # advantage of
    x <- head(x, n)
  }

  con <- con_acquire(x$src)
  on.exit(con_release(x$src, con), add = TRUE)

  sql <- sql_render(x, con)
  res <- dbSendQuery(con, sql)
  tryCatch({
    out <- dbFetch(res, n)
    if (warn_incomplete) {
      res_warn_incomplete(res, "n = Inf")
    }
  }, finally = {
    dbClearResult(res)
  })


  grouped_df(out, intersect(op_grps(x), names(out)))
}

# Do ---------------------------------------------------------------------------

#' @rdname do
#' @param .chunk_size The size of each chunk to pull into R. If this number is
#'   too big, the process will be slow because R has to allocate and free a lot
#'   of memory. If it's too small, it will be slow, because of the overhead of
#'   talking to the database.
#' @export
do.tbl_sql <- function(.data, ..., .chunk_size = 1e4L) {
  groups_sym <- groups(.data)

  if (length(groups_sym) == 0) {
    .data <- collect(.data)
    return(do(.data, ...))
  }

  args <- quos(...)
  named <- named_args(args)

  # Create data frame of labels
  labels <- .data %>%
    select(!!! groups_sym) %>%
    summarise() %>%
    collect()

  con <- con_acquire(.data$src)
  on.exit(con_release(.data$src, con), add = TRUE)

  n <- nrow(labels)
  m <- length(args)

  out <- replicate(m, vector("list", n), simplify = FALSE)
  names(out) <- names(args)
  p <- progress_estimated(n * m, min_time = 2)

  # Create ungrouped data frame suitable for chunked retrieval
  query <- query(con, sql_render(ungroup(.data), con), op_vars(.data))

  # When retrieving in pages, there's no guarantee we'll get a complete group.
  # So we always assume the last group in the chunk is incomplete, and leave
  # it for the next. If the group size is large than chunk size, it may
  # take a couple of iterations to get the entire group, but that should
  # be an unusual situation.
  last_group <- NULL
  i <- 0

  # Assumes `chunk` to be ordered with group columns first
  gvars <- seq_along(groups_sym)

  # Create the dynamic scope for tidy evaluation
  env <- child_env(NULL)
  overscope <- new_overscope(env)
  on.exit(overscope_clean(overscope))

  query$fetch_paged(.chunk_size, function(chunk) {
    if (!is_null(last_group)) {
      chunk <- rbind(last_group, chunk)
    }

    # Create an id for each group
    grouped <- chunk %>% group_by(!!! syms(names(chunk)[gvars]))
    index <- attr(grouped, "indices") # zero indexed
    n <- length(index)

    last_group <<- chunk[index[[length(index)]] + 1L, , drop = FALSE]

    for (j in seq_len(n - 1)) {
      cur_chunk <- chunk[index[[j]] + 1L, , drop = FALSE]
      # Update pronouns within the overscope
      env$. <- env$.data <- cur_chunk
      for (k in seq_len(m)) {
        out[[k]][i + j] <<- list(overscope_eval_next(overscope, args[[k]]))
        p$tick()$print()
      }
    }
    i <<- i + (n - 1)
  })

  # Process last group
  if (!is_null(last_group)) {
    env$. <- env$.data <- last_group
    for (k in seq_len(m)) {
      out[[k]][i + 1] <- list(overscope_eval_next(overscope, args[[k]]))
      p$tick()$print()
    }
  }

  if (!named) {
    label_output_dataframe(labels, out, groups(.data))
  } else {
    label_output_list(labels, out, groups(.data))
  }
}
#' @rdname se-deprecated
#' @inheritParams do
#' @export
do_.tbl_sql <- function(.data, ..., .dots = list(), .chunk_size = 1e4L) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  do(.data, !!! dots, .chunk_size = .chunk_size)
}
