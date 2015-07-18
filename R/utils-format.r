#' Tools for describing matrices
#'
#' @param x Object to show.
#' @param n Number of top rows to show. If \code{NULL}, the default, will print
#'   all rows if less than option \code{dplyr.print_max}. Otherwise, will print
#'   \code{dplyr.print_min}. Setting \code{n} without setting \code{m} implies
#'   \code{m = 0}.
#' @param m Number of bottom rows to show. If \code{NULL}, the default, will
#'   print all rows if less than option \code{dplyr.print_max}. Otherwise, will
#'   print \code{dplyr.print_min}. Setting \code{m} without setting \code{n}
#'   implies \code{n = 0}.
#' @param width Width of text output to generate. This defaults to NULL, which
#'   means use \code{getOption("width")} and only display the columns that
#'   fit on one screen. You can also set \code{option(dplyr.width = Inf)} to
#'   override this default and always print all columns.
#' @param show_classes If set to \code{TRUE}, the class short names will be
#'   printed under the column names. The default value can be overwritten by
#'   setting \code{option(dplyr.print_show_classes = TRUE)}.
#' @keywords internal
#' @examples
#' dim_desc(mtcars)
#' trunc_mat(mtcars)
#'
#' print(tbl_df(mtcars))
#'
#' print(tbl_df(mtcars), n = 1)
#' print(tbl_df(mtcars), n = 3)
#' print(tbl_df(mtcars), n = 100)
#'
#' print(tbl_df(mtcars), m = 1)
#' print(tbl_df(mtcars), m = 3)
#' print(tbl_df(mtcars), m = 100)
#'
#' print(tbl_df(mtcars), n = 1, m = 3)
#' print(tbl_df(mtcars), n = 3, m = 1)
#' print(tbl_df(mtcars), n = 100, m = 100)
#'
#' print(tbl_df(mtcars), show_classes = TRUE)
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
trunc_mat <- function(x, n = NULL, m = NULL, width = NULL, show_classes = NULL) {
  rows <- nrow(x)

  show_classes <- show_classes %||% getOption("dplyr.print_show_classes") %||% FALSE

  # setting only one implies that the other is zero
  if (is.null(n) && !is.null(m)) n <- 0
  if (!is.null(n) && is.null(m)) m <- 0

  # for head
  if (is.null(n)) {
    if (is.na(rows) || rows > getOption("dplyr.print_max")) {
      n <- getOption("dplyr.print_min")
    } else {
      n <- rows
    }
  }

  # for tail
  if (is.null(m)) {
    if (is.na(rows) || rows > getOption("dplyr.print_max")) {
      m <- getOption("dplyr.print_min")
    } else {
      m <- 0
    }
  }

  # make sure the head and tail parts don't overlap
  if (n+m < rows) {
    df <- as.data.frame(head(x, n))
    df_t <- as.data.frame(tail(x, m))
  } else {
    df <- as.data.frame(x)
    n <- rows
    df_t <- as.data.frame(tail(x, 0))
    m <- 0
  }


  if (ncol(df) == 0 || (nrow(df) == 0 && nrow(df_t) == 0)) {
    types <- vapply(df, type_sum, character(1))
    extra <- setNames(types, names(df))

    return(structure(list(table = NULL, extra = extra), class = "trunc_mat"))
  }

  # this function never prints row names
  if (n>0) rownames(df) <- 1:n
  if (m>0) rownames(df_t) <- (rows-m+1):rows

  # List columns need special treatment because format can't be trusted
  is_list <- vapply(df, is.list, logical(1))
  df[is_list] <- lapply(df[is_list], function(x) vapply(x, obj_type, character(1)))

  is_list <- vapply(df_t, is.list, logical(1))
  df_t[is_list] <- lapply(df_t[is_list], function(x) vapply(x, obj_type, character(1)))

  mat <- format(rbind(df, df_t), justify = "left")

  width <- width %||% getOption("dplyr.width", NULL) %||% getOption("width")

  values <- c(format(rownames(mat))[[1]], unlist(mat[1, ]))
  names <- c("", colnames(mat))
  w <- pmax(nchar(encodeString(values)), nchar(encodeString(names)))

  # expand columns to accommodate class names
  if (show_classes) {
    classes <- paste0("<", vapply(df, type_sum, character(1)), ">")
    w <- pmax(w, nchar(encodeString(c("", classes))))
  }

  cumw <- cumsum(w + 1)

  too_wide <- cumw[-1] > width
  # Always display at least one column
  if (all(too_wide)) {
    too_wide[1] <- FALSE
    df[[1]] <- substr(df[[1]], 1, width)
    df_t[[1]] <- substr(df_t[[1]], 1, width)
  }
  shrunk <- format(df[, !too_wide, drop = FALSE])
  shrunk_t <- format(df_t[, !too_wide, drop = FALSE])

  needs_dots <- is.na(rows) || rows > n + m
  if (needs_dots) {
    dot_width <- pmin(w[-1][!too_wide], 3)
    dots <- vapply(dot_width, function(i) paste(rep(".", i), collapse = ""),
                   FUN.VALUE = character(1))
    shrunk <- rbind(shrunk, ".." = dots, shrunk_t)
    if (show_classes) shrunk <- rbind(" " = classes[!too_wide], shrunk)
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
