#' @title Add, remove, or get attributes
#'
#' @description Add, remove, or get attributes to/from the object \code{.data}.
#'
#' @param .data
#' A tbl or any object
#' @param ...
#' Comma separated list of unquoted expressions.
#' @param .dots
#' Used to work around non-standard evaluation. See vignette("nse") for details.
#' @param at
#' Attribute to be obtained.
#' @seealso \code{\link{structure}}, \code{\link{attributes}}
#' @export
#' @examples
#' df <- data.frame(x = sample(10, 5, rep = TRUE),
#'                  y = sample(10, 5, rep = TRUE)) %>%
#'   setattr(example="yes",
#'           package="dplyr")
#'
#' attributes(df)
#'
#' getattr(df, names)
#' getattr_(df, "class")
#' getattr_(df, ~ package)
#'
#' df <- df %>%
#'   setattr_(package = ~ NULL,
#'            example = ~ "no")
#'
#' attributes(df)
setattr <-
function (.data,
          ...)
{
  setattr_(.data, .dots = lazyeval::lazy_dots(...))
}

#' @export
#' @rdname setattr
setattr_ <-
function(.data,
         ...,
         .dots)
{
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  setattr_impl(.data, dots)
}

setattr_impl <-
function(.data,
         dots)
{
  if (length(dots)) {
    specials <- c(".Dim", ".Dimnames", ".Names", ".Tsp", ".Label")
    replace <- c("dim", "dimnames", "names", "tsp", "levels")
    m <- match(names(dots), specials)
    ok <- (!is.na(m) & m)
    names(dots)[ok] <- replace[m[ok]]
    n <- names(dots)
    attributes(.data) <- c(attributes(.data), lazyeval::lazy_eval(dots))
  }
  return(.data)
}

#' @export
#' @rdname setattr
getattr <-
function (.data,
          at)
{
  getattr_(.data, at = deparse(substitute(at)))
}

#' @export
#' @rdname setattr
getattr_ <-
function(.data,
         at)
{
  if (class(at)=="formula") {
    at <- at[[2]]
    at <- deparse(substitute(at))
  }
  attr(.data, at, exact = TRUE)
}
