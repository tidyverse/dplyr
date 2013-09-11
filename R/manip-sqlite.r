#' Data manipulation for SQL tbls.
#'
#' This page documents the specific of data manipulation for 
#' \code{\link{tbl_sqlite}} objects. See \code{manip} for the documentation of
#' the generics, and how they work in general.
#'
#' @section Output:
#'
#' All data manipulation on SQL tbls are lazy: they will not actually
#' run the query or retrieve the data unless you ask for it: they all return
#' a new \code{\link{tbl_sqlite}} object. Use \code{\link{compute}} to run the 
#' query and save the results in a temporary in the database, or use 
#' \code{\link{collect}} to retrieve the results to R.
#'
#' Note that \code{do} is not lazy since it must pull the data into R.
#' It returns a \code{\link{tbl_df}} or \code{\link{grouped_df}}, with one
#' column for each grouping variable, and one list column that contains the
#' results of the operation. \code{do} never simplifies its output.
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
#' @param .data an SQLite data tbl
#' @param ... variables interpreted in the context of \code{.data}
#' @examples
#' batting <- tbl(lahman(), "Batting")
#'
#' # filter, select and arrange lazily modify the specification of the table
#' # they don't execute queries unless you print them
#' filter(batting, YearID > 2005, G > 130)
#' select(batting, playerID:lgID)
#' arrange(batting, PlayerID, desc(YearID))
#' summarise(batting, g = mean(g), n = count())
#' mutate(batting, rbi = 1.0 * R / AB)
#'
#' # Grouped summaries -----------------------------------
#' players <- group_by(batting, PlayerID)
#' 
#' # Due to the lack of windowing functions in SQLite, only summarising
#' # is really useful with grouped values
#' summarise(players, g = mean(g), best_ab = max(ab))
#'
#' # Summarise peels over a single layer of grouping
#' per_year <- group_by(batting, PlayerID, YearID)
#' stints <- summarise(per_year, stints = max(stint))
#' filter(stints, stints > 3)
#' summarise(stints, max(stints))
#'
#' # All other operations will ignore grouping, although they will preserve it
#' # in the object returned to R.
#' select(players, playerID:lgID)
#' arrange(players, PlayerID, desc(YearID))
#' mutate(players, rbi = 1.0 * R / AB)
#'
#' # NB: If you use an aggregation function with mutate
#' mutate(players, cyear = YearID - min(YearID) + 1)
#'
#' # Do arbitrary processing with do ---------------------------------
#'
#' # First find teams with a decent number of records
#' by_team <- group_by(batting, teamID)
#' sizes <- summarise(by_team, freq = count())
#' not_small <- filter(sizes, freq > 10)
#' ok <- group_by(semi_join(batting, not_small), teamID)
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
  input <- partial_eval(dots(...), .data, parent.frame())
  .data <- update(.data, select = remove_star(c(input, .data$select)))
  
  update(
    collapse(.data),
    group_by = drop_last(.data$group_by)
  )
}

#' @rdname manip_sqlite
#' @export
#' @method mutate tbl_sqlite
mutate.tbl_sqlite <- function(.data, ...) {
  input <- partial_eval(dots(...), .data, parent.frame())  
  update(.data, select = c(.data$select, input))
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
  group_by <- .data$group_by
  if (is.null(group_by)) stop("No grouping", call. = FALSE)

  gvars <- seq_along(group_by)
  # Create data frame of labels.
  labels_tbl <- update(.data, 
    select = group_by, 
    order_by = NULL)
  labels <- as.data.frame(labels_tbl)
  
  # Create ungrouped data frame suitable for chunked retrieval
  names(group_by) <- paste0("GRP_", seq_along(group_by))  
  chunky <- update(.data,
    select = c(group_by, .data$select),
    order_by = c(unname(group_by), .data$order_by),
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
  
  labels$DO <- out
  grouped_df(labels, drop_last(groups(.data)))
}
