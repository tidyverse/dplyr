#' Group a tbl by one or more variables.
#'
#' Most data operations are useful done on groups defined by variables in the
#' the dataset. The \code{group_by} function takes an existing tbl
#' and converts it into a grouped tbl where operations are performed
#' "by group".
#'
#' @section Tbl types:
#'
#' \code{group_by} is an S3 generic with methods for the three built-in
#' tbls. See the help for the corresponding classes and their manip
#' methods for more details:
#'
#' \itemize{
#'   \item data.frame: \link{grouped_df}
#'   \item data.table: \link{grouped_dt}
#'   \item SQLite: \code{\link{src_sqlite}}
#'   \item PostgreSQL: \code{\link{src_postgres}}
#'   \item MySQL: \code{\link{src_mysql}}
#' }
#'
#' @seealso \code{\link{ungroup}} for the inverse operation,
#'   \code{\link{groups}} for accessors that don't do special evaluation.
#' @param x a tbl
#' @param ... variables to group by. All tbls accept variable names,
#'   some will also accept functions of variables. Duplicated groups
#'   will be silently dropped.
#' @param add By default, when \code{add = FALSE}, \code{group_by} will
#'   override existing groups. To instead add to the existing groups,
#'   use \code{add = TRUE}
#' @export
#' @examples
#' by_cyl <- group_by(mtcars, cyl)
#' summarise(by_cyl, mean(disp), mean(hp))
#' filter(by_cyl, disp == max(disp))
#'
#' # summarise peels off a single layer of grouping
#' by_vs_am <- group_by(mtcars, vs, am)
#' by_vs <- summarise(by_vs_am, n = n())
#' by_vs
#' summarise(by_vs, n = sum(n))
#' # use ungroup() to remove if not wanted
#' summarise(ungroup(by_vs), n = sum(n))
#'
#' # You can group by expressions: this is just short-hand for
#' # a mutate followed by a simple group_by
#' group_by(mtcars, vsam = vs + am)
#'
#' # By default, group_by sets groups. Use add = TRUE to add groups
#' groups(group_by(by_cyl, vs, am))
#' groups(group_by(by_cyl, vs, am, add = TRUE))
#'
#' # Duplicate groups are silently dropped
#' groups(group_by(by_cyl, cyl, cyl))
group_by <- function(.data, ..., add = FALSE) {
  new_groups <- named_dots(...)

  # If any calls, use mutate to add new columns, then group by those
  calls <- vapply(new_groups, function(x) !is.name(x), logical(1))
  if (any(calls)) {
    env <- new.env(parent = parent.frame())
    env$.data <- .data

    call <- as.call(c(quote(mutate), quote(.data), new_groups[calls]))
    .data <- eval(call, env)

    new_groups[calls] <- lapply(names(new_groups)[calls], as.name)
  }
  names(new_groups) <- NULL

  if (add) {
    new_groups <- c(groups(.data), new_groups)
  }
  new_groups <- new_groups[!duplicated(new_groups)]

  regroup(.data, new_groups)
}

#' Get/set the grouping variables for tbl.
#'
#' These functions do not perform non-standard evaluation, and so are useful
#' when programming against \code{tbl} objects. \code{ungroup} is a convenient
#' inline way of removing existing grouping.
#'
#' @param x data \code{\link{tbl}}
#' @param value a list of symbols
#' @export
#' @seealso \code{\link{group_by}} for a version that does non-standard
#'   evaluation to save typing
#' @examples
#' grouped <- group_by(mtcars, cyl)
#' groups(grouped)
#' grouped <- regroup(grouped, list(quote(vs)))
#' groups(grouped)
#' groups(ungroup(grouped))
groups <- function(x) {
  UseMethod("groups")
}

#' @export
#' @rdname groups
regroup <- function(x, value) {
  stopifnot(is.list(value))

  UseMethod("regroup")
}

#' @export
#' @rdname groups
ungroup <- function(x) {
  UseMethod("ungroup")
}
