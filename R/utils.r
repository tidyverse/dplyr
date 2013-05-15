dots <- function(...) {
  eval(substitute(alist(...)))
}

named_dots <- function(...) {
  args <- dots(...)

  nms <- names2(args)
  missing <- nms == ""
  if (all(!missing)) return(args)

  deparse2 <- function(x) paste(deparse(x, 500L), collapse = "")
  defaults <- vapply(args[missing], deparse2, character(1), USE.NAMES = FALSE)

  names(args)[missing] <- defaults
  args
}


names2 <- function(x) {
  names(x) %||% rep("", length(x))
}

"%||%" <- function(x, y) if(is.null(x)) y else x

is.wholenumber <- function(x, tol = .Machine$double.eps ^ 0.5) {
  abs(x - round(x)) < tol
}

as_df <- function(x) {
  class(x) <- "data.frame"
  attr(x, "row.names") <- c(NA_integer_, -length(x[[1]]))

  x
}

trunc_mat <- function(x, n = 10L) {
  mat <- format(head(x, n))

  width <- getOption("width")

  values <- c(format(rownames(mat))[[1]], unlist(mat[1, ]))
  names <- c("", colnames(mat))
  w <- pmax(nchar(values), nchar(names))
  cumw <- cumsum(w + 1)

  too_wide <- cumw[-1] > width
  shrunk <- mat[, !too_wide, drop = FALSE]

  needs_dots <- nrow(x) > n
  if (needs_dots) {
    dot_width <- pmin(w[-1][!too_wide] - 1, 3)
    dots <- vapply(dot_width, function(i) paste(rep(".", i), collapse = ""),
      FUN.VALUE = character(1))
    shrunk <- rbind(shrunk, "." = dots)
  }
  print(shrunk)

  if (any(too_wide)) {
    vars <- paste0(colnames(mat)[too_wide], collapse = ", ")
    msg <- paste0("Variables not shown: ", vars)
    wrapped <- strwrap(msg, width = width, exdent = 2)
    cat(paste(wrapped, collapse = "\n"), "\n", sep = "")
  }
}

dim_desc <- function(x) {
  d <- format(dim(x), big.mark = ",", trim = TRUE)
  paste0(" [", paste0(d, collapse = " x "), "]")
}
