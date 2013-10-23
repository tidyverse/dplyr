#' A local source.
#' 
#' This is mainly useful for testing, since makes it possible to refer to 
#' local and remote tables using exactly the same syntax. 
#' 
#' Generally, \code{src_local} should not be called directly, but instead
#' one of the (currently three) constructors should be used.
#' 
#' @param tbl name of the function used to generate \code{tbl} objects
#' @param pkg,env Either the name of a package or an environment object in 
#'   which to look for objects.
#' @keywords internal
#' @export
#' @examples
#' if (require("Lahman")) {
#' src_dt("Lahman")
#' src_df("Lahman")
#' 
#' batting_df <- tbl(src_df("Lahman"), "Batting")
#' batting_dt <- tbl(src_dt("Lahman"), "Batting")
#' }
src_local <- function(tbl, pkg = NULL, env = NULL) {
  if (!xor(is.null(pkg), is.null(env))) {
    stop("Must supply exactly one of pkg and env", call. = FALSE)
  }
  if (is.null(env)) {
    env <- as.environment(paste0("package:", pkg))
    name <- paste0("<package: ", pkg, ">")
  } else {
    name <- capture.output(print(env))
  }
  
  structure(
    list(tbl_f = match.fun(tbl), name = name, env = env),
    class = c("src_local", "src")
  )
}

#' @rdname src_local
#' @export
src_df <- function(pkg = NULL, env = NULL) {
  src_local("tbl_df", pkg, env)
}
#' @rdname src_local
#' @export
src_dt <- function(pkg = NULL, env = NULL) {
  src_local("tbl_dt", pkg, env)
}
#' @rdname src_local
#' @export
src_cpp <- function(pkg = NULL, env = NULL) {
  src_local("tbl_cpp", pkg, env)
}

#' @S3method src_tbls src_local
src_tbls.src_local <- function(x, ...) {
  objs <- ls(envir = x$env)
  Filter(function(obj) is.data.frame(get(obj, envir = x$env)), objs) 
}

#' @S3method tbl src_local
tbl.src_local <- function(src, from, ...) {
  src$tbl_f(get(from, src$env))
}

#' @S3method copy_to src_local
copy_to.src_local <- function(dest, df, name = deparse(substitute(df)), ...) {
  assign(name, envir = dest$env, df)
  tbl(dest, name)
}


#' @S3method format src_local
format.src_local <- function(x, ...) {
  paste0("src:  ", x$name, "\n",
    wrap("tbls: ", paste0(sort(src_tbls(x)), collapse = ", ")))
}
