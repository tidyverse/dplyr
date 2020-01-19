format_v <- function(x) UseMethod("format_v")

#' @export
format_v.default <- function(x) {
  dims <- dim(x)

  if (!is.null(dims)){
    dims_out <- paste0(dims, collapse = " x ")
    out <- paste0("<", class(x)[1], "[", dims_out, "]>")
    out
  } else {
    format(x, trim = TRUE, justify = "none")
  }
}

#' @export
format_v.list <- function(x) {
  out <- map(x, format_v)
  atomic <- (map_int(out, length) == 1L)
  out <- map_chr(out, collapse)
  out[!atomic] <- paste0("<", out[!atomic], ">")
  paste0("[", collapse(out), "]")
}

#' @export
format_v.character <- function(x) encodeString(x, quote = '"')

#' @export
format_v.factor <- function(x) {
  if (any(grepl(",", x, fixed = TRUE))) {
    encodeString(as.character(x), quote = '"')
  } else {
    format(x, trim = TRUE, justify = "none")
  }
}
