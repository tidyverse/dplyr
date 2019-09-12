#' Print the location in memory of a data frame
#'
#' This is useful for understand how and when dplyr makes copies of data
#' frames
#'
#' @param df a data frame
#' @param x,y two data frames to compare
#' @keywords internal
#' @importFrom lobstr obj_addr obj_addrs
#' @export
#'
#' @examples
#' location(mtcars)
#'
#' mtcars2 <- mutate(mtcars, cyl2 = cyl * 2)
#' location(mtcars2)
#'
#' changes(mtcars, mtcars)
#' changes(mtcars, mtcars2)
location <- function(df) {
  assert_that(is.data.frame(df))

  attrs <- attributes(df)
  structure(list(
    df = obj_addr(df),
    vars = set_names(obj_addrs(df), names(df)),
    attr = set_names(obj_addrs(attrs), names(attrs))
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
