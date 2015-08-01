#' Tools for describing matrices
#'
#' @param x Object to show.
#' @param n Number of rows to show. If \code{NULL}, the default, will print
#'   all rows if less than option \code{dplyr.print_max}. Otherwise, will
#'   print \code{dplyr.print_min}
#' @param width Width of text output to generate. This defaults to NULL, which
#'   means use \code{getOption("width")} and only display the columns that
#'   fit on one screen. You can also set \code{option(dplyr.width = Inf)} to
#'   override this default and always print all columns.
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
trunc_mat <- function(x, n = NULL, width = NULL) {
  rows <- nrow(x)

  if (is.null(n)) {
    if (is.na(rows) || rows > getOption("dplyr.print_max")) {
      n <- getOption("dplyr.print_min")
    } else {
      n <- rows
    }
  }

  df <- as.data.frame(head(x, n))
  if (ncol(df) == 0 || nrow(df) == 0) {
    types <- vapply(df, type_sum, character(1))
    extra <- setNames(types, names(df))

    return(structure(list(table = NULL, extra = extra), class = "trunc_mat"))
  }

  rownames(df) <- NULL

  # List columns need special treatment because format can't be trusted
  is_list <- vapply(df, is.list, logical(1))
  df[is_list] <- lapply(df[is_list], function(x) vapply(x, obj_type, character(1)))

  mat <- format(df, justify = "left")

  width <- width %||% getOption("dplyr.width", NULL) %||% getOption("width")

  values <- c(format(rownames(mat))[[1]], unlist(mat[1, ]))
  names <- c("", colnames(mat))
  w <- pmax(nchar(encodeString(values)), nchar(encodeString(names)))
  cumw <- cumsum(w + 1)

  too_wide <- cumw[-1] > width
  # Always display at least one column
  if (all(too_wide)) {
    too_wide[1] <- FALSE
    df[[1]] <- substr(df[[1]], 1, width)
  }
  shrunk <- format(df[, !too_wide, drop = FALSE])
  colnames(shrunk) <- colnames(df)

  needs_dots <- is.na(rows) || rows > n
  if (needs_dots) {
    dot_width <- pmin(w[-1][!too_wide], 3)
    dots <- vapply(dot_width, function(i) paste(rep(".", i), collapse = ""),
      FUN.VALUE = character(1))
    shrunk <- rbind(shrunk, ".." = dots)
  }

  if (any(too_wide)) {
    vars <- colnames(mat)[too_wide]
    types <- vapply(df[too_wide], type_sum, character(1))
    extra <- setNames(types, vars)
  } else {
    extra <- character()
  }

  structure(list(table = shrunk, extra = extra), class = "trunc_mat")
}

#' @export
print.trunc_mat <- function(x, ...) {
  if (!is.null(x$table)) {
    print(x$table)
  }

  if (length(x$extra) > 0) {
    var_types <- paste0(names(x$extra), " (", x$extra, ")", collapse = ", ")
    cat(wrap("Variables not shown: ", var_types), "\n", sep = "")
  }
  invisible()
}

#' knit_print method for trunc mat
#' @keywords internal
#' @export
knit_print.trunc_mat <- function(x, options) {
  kable <- knitr::kable(x$table, row.names = FALSE)

  if (length(x$extra) > 0) {
    var_types <- paste0(names(x$extra), " (", x$extra, ")", collapse = ", ")
    extra <- wrap("\n(_Variables not shown_: ", var_types, ")")
  } else {
    extra <- "\n"
  }

  res <- paste(c('', '', kable, '', extra), collapse = '\n')
  knitr::asis_output(res)
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

obj_type <- function(x) UseMethod("obj_type")
#' @export
obj_type.NULL <- function(x) "<NULL>"
#' @export
obj_type.default <- function(x) {
  if (!is.object(x)) {
    paste0("<", type_sum(x), if (!is.array(x)) paste0("[", length(x), "]"), ">")
  } else if (!isS4(x)) {
    paste0("<S3:", paste0(class(x), collapse = ", "), ">")
  } else {
    paste0("<S4:", paste0(is(x), collapse = ", "), ">")
  }
}

#' @export
obj_type.data.frame <- function(x) {
  paste0("<", class(x)[1], " [", paste0(dim(x), collapse = ","), "]", ">")
}
#' @export
obj_type.data_frame <- function(x) {
  paste0("<data_frame [", paste0(dim(x), collapse = ","), "]", ">")
}
