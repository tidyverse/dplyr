#' dbplyr compatiblity functions
#'
#' @description
#' In dplyr 0.6.0, a number of databse and SQL functions moved from dplyr to
#' dbplyr. The generic functions stayed in dplyr (since there is no easy way
#' to conditionally import a generic from different packages), but many other
#' SQL and database helper functions moved. If you have written a backend,
#' these functions generate the code you need to work with both dplyr 0.5.0
#' dplyr 0.6.0.
#'
#' @keywords internal
#' @export
#' @examples
#' if (requireNamespace("dbplyr", quietly = TRUE)) {
#' wrap_dbplyr_obj("build_sql")
#' wrap_dbplyr_obj("base_agg")
#' }
check_dbplyr <- function() {
  check_pkg("dbplyr", "communicate with database backends", install = FALSE)
}

#' @export
#' @rdname check_dbplyr
wrap_dbplyr_obj <- function(obj_name) {
  # Silence R CMD check NOTE
  `UQ<-` <- NULL

  obj <- getExportedValue("dbplyr", obj_name)
  obj_sym <- sym(obj_name)

  dbplyr_sym <- lang("::", quote(dbplyr), obj_sym)
  dplyr_sym <- lang("::", quote(dplyr), obj_sym)

  if (is.function(obj)) {
    args <- formals()
    pass_on <- map(set_names(names(args)), sym)

    dbplyr_call <- expr(UQ(dbplyr_sym)(!!!pass_on))
    dplyr_call <- expr(UQ(dplyr_sym)(!!!pass_on))
  } else {
    args <- list()

    dbplyr_call <- dbplyr_sym
    dplyr_call <- dplyr_sym
  }

  body <- expr({
    if (utils::packageVersion("dplyr") > "0.5.0") {
      dplyr::check_dbplyr()
      UQ(dbplyr_call)
    } else {
      UQ(dplyr_call)
    }
  })
  wrapper <- new_function(args, body, caller_env())

  expr(UQ(obj_sym) <- UQE(wrapper))
}

#' @inherit dbplyr::sql
#' @export
sql <- function(...) {
  check_dbplyr()
  dbplyr::sql(...)
}

#' @inherit dbplyr::ident
#' @export
ident <- function(...) {
  check_dbplyr()
  dbplyr::ident(...)
}
