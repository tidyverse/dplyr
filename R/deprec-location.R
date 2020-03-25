#' Print the location in memory of a data frame
#'
#' @description
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("deprecated")}
#'
#' This is useful for understand how and when dplyr makes copies of data
#' frames
#'
#' @param df a data frame
#' @param x,y two data frames to compare
#' @keywords internal
#' @export
#'
#' @examples
#' location(mtcars)
#' # ->
#' lobstr::ref(mtcars)
#'
#' mtcars2 <- mutate(mtcars, cyl2 = cyl * 2)
#' # ->
#' lobstr::ref(mtcars2)
#'
#' changes(mtcars, mtcars2)
#' # ->
#' lobstr::ref(mtcars, mtcars2)
location <- function(df) {
  lifecycle::deprecate_warn("1.0.0", "location()", "lobst::ref()")

  check_pkg("lobstr", "compute package locations")

  if (!is.data.frame(df)) {
    abort("location() is meant for data frames")
  }

  attrs <- attributes(df)
  structure(list(
    df = lobstr::obj_addr(df),
    vars = set_names(lobstr::obj_addrs(df), names(df)),
    attr = set_names(lobstr::obj_addrs(attrs), names(attrs))
  ), class = "location")
}

#' @export
print.location <- function(x, ...) {
  cat("<", x$df, ">\n", sep = "")

  width <- max(nchar(c(names(x$vars), names(x$attr)))) + 1
  def_list <- function(x) {
    term <- format(paste0(names(x), ":"), width = width)
    paste0(" * ", term, " <", format(x), ">")
  }

  vars <- paste0(def_list(x$vars), collapse = "\n")
  cat("Variables:\n", vars, "\n", sep = "")

  attr <- paste0(def_list(x$attr), collapse = "\n")
  cat("Attributes:\n", attr, "\n", sep = "")
  invisible(x)
}

#' @rdname location
#' @export
changes <- function(x, y) {
  lifecycle::deprecate_warn("1.0.0", "changes()", "lobstr::ref()")

  x <- location(x)
  y <- location(y)

  if (x$df == y$df) {
    cat("<identical>\n")
    return(invisible())
  }

  # match up x vars to y vars
  vars <- match_up(x$vars, y$vars)
  attr <- match_up(x$attr, y$attr)

  width <- max(nchar(rownames(vars)), nchar(rownames(attr)))
  if (nrow(vars) > 0) rownames(vars) <- format(rownames(vars), width = width)
  if (nrow(attr) > 0) rownames(attr) <- format(rownames(attr), width = width)

  if (nrow(vars) > 0) {
    cat("Changed variables:\n")
    print(vars, quote = FALSE)
  }

  if (nrow(vars) > 0 && nrow(attr)) cat("\n")

  if (nrow(attr) > 0) {
    cat("Changed attributes:\n")
    print(attr, quote = FALSE)
  }
}

match_up <- function(x, y) {
  both <- intersect(names(x), names(y))
  added <- setdiff(names(x), names(y))
  deleted <- setdiff(names(y), names(x))

  out <- cbind(
    old = c(x[both], x[added], rep("<added>", length(deleted))),
    new = c(y[both], rep("<deleted>", length(added)), y[deleted])
  )
  rownames(out) <- c(both, added, deleted)
  out[out[, "old"] != out[, "new"], , drop = FALSE]
}
