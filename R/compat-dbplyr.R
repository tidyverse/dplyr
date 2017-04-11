#' dbplyr compatiblity functions
#'
#' @description
#' In dplyr 0.6.0, a number of databse and SQL functions moved from dplyr to
#' dbplyr. The generic functions stayed in dplyr (since there is no easy way
#' to conditionally import a generic from different packages), but many other
#' SQL and database helper functions moved. If you have written a backend,
#' these functions make it easier for your backend to work with both dplyr 0.5.0
#' dplyr 0.6.0.
#'
#' Use `check_dbplyr()` if you need to give an informative message if dbplyr
#' is not available. Use `dbplyr_fun()` if you need a function from dbplyr;
#' use `dbplyr_obj()` if you need an object.
#'
#' @keywords internal
#' @export
#' @examples
#' if (requireNamespace("dbplyr", quietly = TRUE)) {
#'
#' # Functions -------------------------------------------------------
#' # If you need to access a function, you can call it directly
#' dbplyr_fun("build_sql")("min(", 1, ")")
#'
#' # If you use it many times, save it to a local variable
#' build_sql <- dbplyr_fun("build_sql")
#' src_sql <- dbplyr_fun("src_sql")
#'
#' # Objects ---------------------------------------------------------
#' # If you need to access an object like base_agg, you'll need
#' # dbplyr_obj()
#' dbplyr_obj("base_agg")
#'
#' # Do NOT save this to a variable inside your package as it will
#' # cache the value at build time, not run time, causing bugs that
#' # extremely hard to track down.
#' }
check_dbplyr <- function() {
  check_pkg("dbplyr", "communicate with database backends")
}

#' @export
#' @rdname check_dbplyr
dbplyr_fun <- function(fun_name) {
  args <- formals(getExportedValue("dbplyr", fun_name))
  pass_on <- map(set_names(names(args)), as_symbol)
  fun <- as_symbol(fun_name)

  body <- expr({
    if (utils::packageVersion("dplyr") > "0.5.0") {
      dplyr::check_dbplyr()
      dbplyr::UQE(fun)(!!!pass_on)
    } else {
      dplyr::UQE(fun)(!!!pass_on)
    }
  })

  new_function(args, body, caller_env())
}

#' @export
#' @rdname check_dbplyr
dbplyr_obj <- function(obj_name) {
  if (utils::packageVersion("dplyr") > "0.5.0") {
    dplyr::check_dbplyr()
    getExportedValue("dbplyr", obj_name)
  } else {
    getExportedValue("dplyr", obj_name)
  }
}
