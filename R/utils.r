#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

dots <- function(...) {
  eval(substitute(alist(...)))
}

deparse_trunc <- function(x, width = getOption("width")) {
  text <- deparse(x, width.cutoff = width)
  if (length(text) == 1 && nchar(text) < width) return(text)

  paste0(substr(text[1], 1, width - 3), "...")
}

is.lang <- function(x) {
  is.name(x) || is.atomic(x) || is.call(x)
}

is.lang.list <- function(x) {
  if (is.null(x)) return(TRUE)

  is.list(x) && all_apply(x, is.lang)
}
on_failure(is.lang.list) <- function(call, env) {
  paste0(call$x, " is not a list containing only names, calls and atomic vectors")
}

all_apply <- function(xs, f) {
  for (x in xs) {
    if (!f(x)) return(FALSE)
  }
  TRUE
}

any_apply <- function(xs, f) {
  for (x in xs) {
    if (f(x)) return(TRUE)
  }
  FALSE
}

drop_last <- function(x) {
  if (length(x) <= 1L) return(NULL)
  x[-length(x)]
}

compact <- function(x) Filter(Negate(is.null), x)

names2 <- function(x) {
  names(x) %||% rep("", length(x))
}

has_names <- function(x) {
  nms <- names(x)
  if (is.null(nms)) {
    rep(FALSE, length(x))
  } else {
    !is.na(nms) & nms != ""
  }
}

"%||%" <- function(x, y) if (is.null(x)) y else x

is.wholenumber <- function(x) {
  trunc(x) == x
}

deparse_all <- function(x) {
  deparse2 <- function(x) paste(deparse(x, width.cutoff = 500L), collapse = "")
  vapply(x, deparse2, FUN.VALUE = character(1L))
}

deparse_names <- function(x) {
  vapply(x, deparse, FUN.VALUE = character(1L))
}

#' Provides comma-separated string out ot the parameters
#' @export
#' @keywords internal
#' @param ... Arguments to be constructed into the string
named_commas <- function(...) {
  x <- c(...)
  if (is.null(names(x))) {
    paste0(x, collapse = ", ")
  } else {
    paste0(names(x), " = ", x, collapse = ", ")
  }
}

commas <- function(...) paste0(..., collapse = ", ")

in_travis <- function() identical(Sys.getenv("TRAVIS"), "true")

named <- function(...) {
  x <- c(...)

  missing_names <- names2(x) == ""
  names(x)[missing_names] <- x[missing_names]

  x
}

unique_name <- local({
  i <- 0

  function() {
    i <<- i + 1
    paste0("zzz", i)
  }
})

isFALSE <- function(x) identical(x, FALSE)

f_lhs <- function(x) if (length(x) >= 3) x[[2]] else NULL
f_rhs <- function(x) x[[length(x)]]

succeeds <- function(x, quiet = FALSE) {
  tryCatch(
    {
      x
      TRUE
    },
    error = function(e) {
      if (!quiet)
        message("Error: ", e$message)
      FALSE
    }
  )
}

# is.atomic() is TRUE for atomic vectors AND NULL!
is_atomic <- function(x) is.atomic(x) && !is.null(x)

is_1d <- function(x) {
  # dimension check is for matrices and data.frames
  (is_atomic(x) || is.list(x)) && length(dim(x)) <= 1
}

is_bare_list <- function(x) is.list(x) && !is.object(x)

is_negated <- function(x) is.call(x) && (length(x) == 2) && identical(x[[1]], quote(`-`))
