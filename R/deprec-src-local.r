#' A local source
#'
#' `r lifecycle::badge("deprecated")`
#' This function was deprecated since it existed to support a style of testing
#' dplyr backends that turned out not to be useful.
#'
#' @param tbl name of the function used to generate `tbl` objects
#' @param pkg,env Either the name of a package or an environment object in
#'   which to look for objects.
#' @keywords internal
#' @export
src_local <- function(tbl, pkg = NULL, env = NULL) {
  lifecycle::deprecate_warn("1.0.0", "src_local()", always = TRUE)

  if (!xor(is.null(pkg), is.null(env))) {
    msg <- glue("Exactly one of `pkg` and `env` must be non-NULL, not {(!is.null(pkg)) + (!is.null(env))}.")
    abort(msg)
  }
  if (!is.null(pkg)) {
    env <- getNamespaceInfo(pkg, "lazydata")
    name <- paste0("<package: ", pkg, ">")
  } else {
    stopifnot(is.environment(env))
    name <- utils::capture.output(print(env))
  }

  structure(
    list(tbl_f = match.fun(tbl), name = name, env = env),
    class = c("src_local", "src")
  )
}

#' @rdname src_local
#' @export
src_df <- function(pkg = NULL, env = NULL) {
  src_local("as_tibble", pkg, env)
}

#' @export
src_tbls.src_local <- function(x, ...) {
  objs <- ls(envir = x$env, all.names = TRUE)
  Filter(function(obj) is.data.frame(get(obj, envir = x$env)), objs)
}

#' @export
tbl.src_local <- function(src, from, ...) {
  src$tbl_f(get(from, src$env))
}

#' @export
copy_to.src_local <- function(dest, df, name = deparse(substitute(df)),
                              overwrite = FALSE, ...) {

  if (!overwrite && exists(name, envir = dest$env, inherits = FALSE)) {
    msg <- glue("Object with `name` = {fmt_obj(name)} must not already exist, unless `overwrite` = TRUE.")
    abort(msg)
  }

  assign(name, envir = dest$env, df)
  tbl(dest, name)
}

#' @export
format.src_local <- function(x, ...) {
  paste0(
    "src:  ", x$name, "\n",
    wrap("tbls: ", paste0(sort(src_tbls(x)), collapse = ", "))
  )
}

wrap <- function(..., indent = 0) {
  x <- paste0(..., collapse = "")
  wrapped <- strwrap(
    x,
    indent = indent,
    exdent = indent + 2,
    width = getOption("width")
  )

  paste0(wrapped, collapse = "\n")
}
