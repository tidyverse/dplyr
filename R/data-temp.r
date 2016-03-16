#' Infrastructure for testing dplyr
#'
#' Register testing sources, then use \code{test_load} to load an existing
#' data frame into each source. To create a new table in each source,
#' use \code{test_frame}.
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' test_register_src("df", src_df(env = new.env()))
#' test_register_src("sqlite", src_sqlite(":memory:", create = TRUE))
#'
#' test_frame(x = 1:3, y = 3:1)
#' test_load(mtcars)
#' }
#' @name testing
NULL


#' @export
#' @rdname testing
test_register_src <- function(name, src) {
  message("Registering testing src: ", name)
  test_srcs$add(name, src)
}

#' @export
#' @rdname testing
test_load <- function(df, name = random_table_name(), srcs = test_srcs$get(),
                      ignore = character()) {
  stopifnot(is.data.frame(df))
  stopifnot(is.character(ignore))

  srcs <- srcs[setdiff(names(srcs), ignore)]
  lapply(srcs, copy_to, df, name = name)
}

#' @export
#' @rdname testing
test_frame <- function(..., srcs = test_srcs$get()) {
  df <- data_frame(...)
  test_load(df, srcs = srcs)
}

# Manage cache of testing srcs
test_srcs <- local({
  e <- new.env(parent = emptyenv())
  e$srcs <- list()

  list(
    get = function() e$srcs,

    has = function(x) exists(x, envir = e, inherits = FALSE),

    add = function(name, src) {
      stopifnot(is.src(src))
      e$srcs[[name]] <- src
    },

    set = function(...) {
      old <- e$srcs
      e$srcs <- list(...)
      invisible(old)
    }
  )
})
