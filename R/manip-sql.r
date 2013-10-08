#' Data manipulation for SQL tbls.
#'
#' This page documents the specific of data manipulation for
#' \code{\link{tbl_sql}} objects. See \code{manip} for the documentation of
#' the generics, and how they work in general.
#'
#' @section Output:
#'
#' All data manipulation on SQL tbls are lazy: they will not actually
#' run the query or retrieve the data unless you ask for it: they all return
#' a new \code{\link{tbl_sql}} object. Use \code{\link{compute}} to run the
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
#' @param .data an sql data tbl
#' @param ... variables interpreted in the context of \code{.data}. 
#'   For \code{do}, any additional arguments to pass to \code{f}.
#' @examples
#' batting <- tbl(lahman_sqlite(), "Batting")
#'
#' # filter, select and arrange lazily modify the specification of the table
#' # they don't execute queries unless you print them
#' filter(batting, yearID > 2005, G > 130)
#' select(batting, playerID:lgID)
#' arrange(batting, playerID, desc(YearID))
#' summarise(batting, G = mean(G), n = n())
#' mutate(batting, rbi2 = 1.0 * R / AB)
#'
#' # Grouped summaries -----------------------------------
#' players <- group_by(batting, playerID)
#'
#' # Due to the lack of windowing functions in SQLite, only summarising
#' # is really useful with grouped values
#' summarise(players, mean_g = mean(G), best_ab = max(AB))
#'
#' # Summarise peels over a single layer of grouping
#' per_year <- group_by(batting, playerID, yearID)
#' stints <- summarise(per_year, stints = max(stint))
#' filter(ungroup(stints), stints > 3)
#' summarise(stints, max(stints))
#'
#' # select and arrange aren't affected by grouping
#' select(players, playerID:lgID)
#' arrange(players, playerID, desc(yearID))
#' # grouped mutate doesn't work for sqlite because it doesn't 
#' # support windowed functions
#' \dontrun{
#' mutate(players, cyear = yearID - min(yearID) + 1)
#' }
#'
#' # Do arbitrary processing with do ---------------------------------
#'
#' # First find teams with a decent number of records
#' teams <- group_by(batting, teamID)
#' sizes <- summarise(teams, freq = n())
#' not_small <- filter(sizes, freq > 10)
#' big_teams <- semi_join(teams, not_small)
#'
#' # Explore how they have changed over time
#' mods <- do(big_teams, failwith(NULL, lm), formula = r ~ poly(year, 2),
#'   .chunk_size = 10000)
#'
#' # Note that it's more efficient to select only the variables needed
#' big_teams_min <- select(big_teams, year, r)
#' mods <- do(big_teams_min, failwith(NULL, lm), formula = r ~ poly(year, 2),
#'   .chunk_size = 1000)
#' @name manip_sql
NULL

#' @rdname manip_sql
#' @export
#' @method filter tbl_sql
filter.tbl_sql <- function(.data, ...) {
  input <- partial_eval(dots(...), .data, parent.frame())
  update(.data, where = c(.data$where, input))
}

#' @rdname manip_sql
#' @export
#' @method arrange tbl_sql
arrange.tbl_sql <- function(.data, ...) {
  input <- partial_eval(dots(...), .data, parent.frame())
  update(.data, order_by = c(input, .data$order_by))
}

#' @rdname manip_sql
#' @export
#' @method select tbl_sql
select.tbl_sql <- function(.data, ...) {
  input <- var_eval(dots(...), .data, parent.frame())
  update(.data, select = input)
}

#' @rdname manip_sql
#' @export
#' @method summarise tbl_sql
summarise.tbl_sql <- function(.data, ...) {
  input <- partial_eval(dots(...), .data, parent.frame())
  input <- auto_name(input)
  
  # Automatically collapse data to ensure that arranging and selecting
  # defined before summarise happen first in sql.
#   if (!identical(.data$select, list(star())) || !is.null(.data$arrange)) {
#     .data <- collapse(.data)
#   }
  
  .data <- update(.data, select = input, summarise = TRUE)
  new_vars <- lapply(names(input), as.name)
  
  update(
    collapse(.data),
    group_by = drop_last(.data$group_by)
  )
}

#' @rdname manip_sql
#' @export
#' @method mutate tbl_sql
mutate.tbl_sql <- function(.data, ...) {
  input <- partial_eval(dots(...), .data, parent.frame())
  update(.data, select = c(.data$select, input))
}

#' @method do tbl_sql
#' @export
#' @rdname manip_sql
#' @param .f A function to apply to each group.
#' @param .chunk_size The size of each chunk to pull into R. If this number is
#'   too big, the process will be slow because R has to allocate and free a lot
#'   of memory. If it's too small, it will be slow, because of the overhead of
#'   talking to the database.
do.tbl_sql <- function(.data, .f, ..., .chunk_size = 1e4L) {
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
