#' Create a "sql src" object
#'
#' \code{src_sql} is the standard constructor for all SQL based srcs.
#'
#' @keywords internal
#' @export
#' @param subclass name of subclass. "src_sql" is an abstract base class, so you
#'   must supply this value. \code{src_} is automatically prepended to the
#'   class name
#' @param con the connection object
#' @param ... fields used by object
src_sql <- function(subclass, con, ...) {
  subclass <- paste0("src_", subclass)
  structure(list(obj = con, ...), class = c(subclass, "src_sql", "src"))
}

#' Acquire/release connections from a src object
#'
#' \code{con_acquire} gets a connection from a src object; \code{con_release}
#' returns a previously acquired connection back to its src object. Intended for
#' internal use.
#'
#' These methods have default implementations for \code{src_sql} and can be
#' overridden for src objects that are not themselves DB connections, but know
#' how to get them (e.g. a connection pool).
#'
#' @keywords internal
#' @export
#' @param src A src object (most commonly, from \code{src_sql})
#' @param con A connection
#' @return For \code{con_acquire}, a connection object; for \code{con_release},
#'   nothing.
con_acquire <- function(src) {
  UseMethod("con_acquire", src)
}

#' @rdname con_acquire
#' @export
con_release <- function(src, con) {
  UseMethod("con_release", src)
}

con_acquire.src_sql <- function(src) {
  src$obj
}

con_release.src_sql <- function(src, con) {
}


#' @export
same_src.src_sql <- function(x, y) {
  if (!inherits(y, "src_sql")) return(FALSE)
  identical(x$obj, y$obj)
}

#' @export
src_tbls.src_sql <- function(x, ...) {
  con <- con_acquire(x)
  on.exit(con_release(x, con), add = TRUE)

  db_list_tables(con)
}

#' @export
format.src_sql <- function(x, ...) {
  paste0("src:  ", src_desc(x), "\n",
    wrap("tbls: ", paste0(sort(src_tbls(x)), collapse = ", ")))
}
