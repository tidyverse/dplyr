#' Tools for describing matrices
#'
#' @param x Object to show.
#' @param n Number of rows to show. If \code{NULL}, the default, will print
#'   all rows if less than option \code{dplyr.print_max}. Otherwise, will
#'   print \code{dplyr.print_min}
#' @keywords internal
#' @examples
#' dim_desc(mtcars)
#' trunc_mat(mtcars)
#'
#' print(tbl_df(mtcars))
#' print(tbl_df(mtcars), n = 1)
#' print(tbl_df(mtcars), n = 3)
#' print(tbl_df(mtcars), n = 100)
#'
#' @name dplyr-formatting
NULL

#' @export
#' @rdname dplyr-formatting
dim_desc <- function(x) {
  d <- dim(x)
  d2 <- format(d, big.mark = ",", justify = "none", trim = TRUE)
  d2[is.na(d)] <- "??"

  paste0("[", paste0(d2, collapse = " x "), "]")
}

#' @export
#' @rdname dplyr-formatting
trunc_mat <- function(x, n = NULL) {
  rows <- nrow(x)
  if (!is.na(rows) && rows == 0) return()

  if (is.null(n)) {
    if (is.na(rows) || rows > getOption("dplyr.print_max")) {
      n <- getOption("dplyr.print_min")
    } else {
      n <- rows
    }
  }

  peek <- function(x,n) x[sample.int(size=n,nrow(x),replace=TRUE),]


  df <- as.data.frame(peek(x, n))
  if (nrow(df) == 0) return()

  # List columns need special treatment because format can't be trusted
  is_list <- vapply(df, is.list, logical(1))
  df[is_list] <- lapply(df[is_list], function(x) vapply(x, obj_type, character(1)))

  mat <- format(df, justify = "left")

  width <- getOption("width")

  values <- c(format(rownames(mat))[[1]], unlist(mat[1, ]))
  names <- c("", colnames(mat))
  w <- pmax(nchar(values), nchar(names))
  cumw <- cumsum(w + 1)

  too_wide <- cumw[-1] > width
  # Always display at least one column
  if (all(too_wide)) {
    too_wide[1] <- FALSE
    df[[1]] <- substr(df[[1]], 1, width)
  }
  shrunk <- format(df[, !too_wide, drop = FALSE])

  needs_dots <- is.na(rows) || rows > n
  if (needs_dots) {
    dot_width <- pmin(w[-1][!too_wide], 3)
    dots <- vapply(dot_width, function(i) paste(rep(".", i), collapse = ""),
      FUN.VALUE = character(1))
    shrunk <- rbind(shrunk, ".." = dots)
  }
  print(shrunk)

  if (any(too_wide)) {
    vars <- colnames(mat)[too_wide]
    types <- vapply(df[too_wide], type_sum, character(1))
    var_types <- paste0(vars, " (", types, ")", collapse = ", ")

    cat(wrap("Variables not shown: ", var_types), "\n", sep = "")
  }
}

wrap <- function(..., indent = 0) {
  x <- paste0(..., collapse = "")
  wrapped <- strwrap(x, indent = indent, exdent = indent + 2,
    width = getOption("width"))
  paste0(wrapped, collapse = "\n")
}

ruler <- function(width = getOption("width")) {
  x <- seq_len(width)
  y <- ifelse(x %% 10 == 0, x %/% 10, ifelse(x %% 5 == 0, "+", "-"))
  cat(y, "\n", sep = "")
  cat(x %% 10, "\n", sep = "")
}

rule <- function(char = "-") {
  paste0(rep(char, getOption("width") - 2), collapse = "")
}

#' @export
print.BoolResult <- function(x, ...) {
  cat(x)
  if (!x) cat(": ", attr(x, "comment"), sep = "")
  cat("\n")
}

obj_type <- function(x) {
  if (!is.object(x)) {
    paste0("<", type_sum(x), if (!is.array(x)) paste0("[", length(x), "]"), ">")
  } else if (!isS4(x)) {
    paste0("<S3:", paste0(class(x), collapse = ", "), ">")
  } else {
    paste0("<S4:", paste0(is(x), collapse = ", "), ">")
  }
}
