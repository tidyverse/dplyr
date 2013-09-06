#' Data manipulation for SQL tbls.
#'
#' All operations on SQL tables are lazy: they will not actually
#' run the query or retrieve the data unless you ask for it. Use 
#' \code{\link{compute}} to run the query and save the results in a temporary
#' in the database, or use \code{\link{collect}} to retrieve the results to R.
#'
#' @section Query principles:
#' 
#' This section attempts to lay out the principles governing the generation
#' of SQL queries from the manipulation verbs.  The basic principle is that
#' a sequence of operations should return the same value (modulo class) 
#' regardless of where the data is stored. 
#' 
#' \itemize{
#'  \item \code{arrange(arrange(df, x), y)} should be equivalent to
#'    \code{arrange(df, y, x)}
#'    
#'  \item \code{select(select(df, a:x), n:o)} should be equivalent to
#'    \code{select(df, n:o)}
#' 
#'  \item \code{mutate(mutate(df, x2 = x * 2), y2 = y * 2)} should be 
#'     equivalent to \code{mutate(df, x2 = x * 2, y2 = y * 2)}
#' 
#'  \item \code{filter(filter(df, x == 1), y == 2)} should be 
#'     equivalent to \code{filter(df, x == 1, y == 2)}
#'     
#'  \item \code{summarise} should return the summarised output with
#'    one level of grouping peeled off.
#' }
#' 
#' 
#' @param .data an SQLite data tbl
#' @param ... variables interpreted in the context of \code{.data}
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
#' # Summarise peels over a single layer of grouping
#' groups(stints)
#' collect(summarise(stints, max(stints)))
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
#' # Find decent sized teams
#' sizes <- summarise(by_team, freq = count())
#' not_small <- collect(filter(sizes, freq > 10))
#' teams <- not_small$team
#' ok <- filter(by_team, team %in% teams)
#' 
#' # Do arbitrary processing with do ---------------------------------
#' 
#' # Explore how they have changed over time
#' mods <- do(ok, failwith(NULL, lm), formula = r ~ poly(year, 2),
#'   .chunk_size = 1000)
#'
#' # Note that it's more efficient to select only the variables needed
#' ok_min <- select(ok, year, r)
#' mods <- do(ok_min, failwith(NULL, lm), formula = r ~ poly(year, 2),
#'   .chunk_size = 1000)
#' @name manip_sqlite
NULL

#' @rdname manip_sqlite
#' @export
#' @method filter tbl_sqlite
filter.tbl_sqlite <- function(.data, ...) {  
  input <- partial_eval(dots(...), .data, parent.frame())
  
  update(.data, where = c(.data$where, input))
}

#' @rdname manip_sqlite
#' @export
#' @method arrange tbl_sqlite
arrange.tbl_sqlite <- function(.data, ...) {
  input <- partial_eval(dots(...), .data, parent.frame())
  
  update(.data, order_by = c(input, .data$order_by))
}

#' @rdname manip_sqlite
#' @export
#' @method select tbl_sqlite
select.tbl_sqlite <- function(.data, ...) {
  input <- var_eval(dots(...), .data, parent.frame())
  
  update(.data, select = ident(input))
}

#' @rdname manip_sqlite
#' @export
#' @method summarise tbl_sqlite
summarise.tbl_sqlite <- function(.data, ...) {
  new_vars <- trans_sqlite(dots(...), .data, parent.frame())
  .data <- update(.data, 
    select = c.sql(.data$select, new_vars, drop_null = TRUE))
  
  update(
    collapse(.data),
    group_by = drop_last(.data$group_by)
  )
}

#' @rdname manip_sqlite
#' @export
#' @method mutate tbl_sqlite
mutate.tbl_sqlite <- function(.data, ...) {
  old_vars <- .data$select %||% sql("*")
  new_vars <- trans_sqlite(dots(...), .data, parent.frame())
  
  update(.data, select = c(old_vars, new_vars))
}

#' @method do tbl_sqlite
#' @export 
#' @rdname manip_sqlite
#' @param .f,... A function to apply to each group, and any additional arguments
#'   to pass to \code{f}.
#' @param .chunk_size The size of each chunk to pull into R. If this number is 
#'   too big, the process will be slow because R has to allocate and free a lot
#'   of memory. If it's too small, it will be slow, because of the overhead of
#'   talking to the database.
do.tbl_sqlite <- function(.data, .f, ..., .chunk_size = 1e4L) {
  group_by <- trans_sqlite(.data$group_by)
  gvars <- seq_along(group_by)
  
  # Create data frame of labels.
  labels_tbl <- update(.data, 
    select = NULL, 
    order_by = NULL)
  labels <- collect(labels_tbl)
  
  # Create ungrouped data frame suitable for chunked retrieval
  names(group_by) <- paste0("GRP_", seq_along(group_by))  
  chunky <- update(.data,
    select = c(group_by, .data$select %||% sql("*")),
    order_by = c.sql(ident(names(group_by)), .data$order_by, drop_null = TRUE),
    group_by = NULL
  )
    
  # When retrieving in pages, there's no guarantee we'll get a complete group.
  # So we always assume the last group in the chunk is incomplete, and leave
  # it for the next. If the group size is large than chunk size, it may
  # take a couple of iterations to get the entire group, but that should
  # be an unusual situation.
  last_group <- NULL
  out <- vector("list", nrow(labels))
  i <- 0
  
  chunky$query$fetch_paged(.chunk_size, function(chunk) {
    if (!is.null(last_group)) chunk <- rbind(last_group, chunk)
    
    # Create an id for each group
    group_id <- id(chunk[gvars], drop = TRUE)
    n <- attr(group_id, "n")
    
    index <- split_indices(group_id, n)
    last_group <<- chunk[last(index), , drop = FALSE]
    
    for (j in seq_len(n - 1)) {
      subs <- chunk[index[[j]], , drop = FALSE]
      out[[i + j]] <<- .f(subs, ...)
    }
    i <<- i + (n - 1)
  })

  # Process last group
  if (!is.null(last_group)) {
    out[[i + 1]] <- .f(last_group, ...)
  }
  
  out
}
