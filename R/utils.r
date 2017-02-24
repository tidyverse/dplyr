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

is.wholenumber <- function(x) {
  trunc(x) == x
}

deparse_all <- function(x) {
  deparse2 <- function(x) paste(deparse(x, width.cutoff = 500L), collapse = "")
  map_chr(x, deparse2)
}

deparse_names <- function(x) {
  map_chr(x, deparse)
}

#' Provides comma-separated string out ot the parameters
#' @export
#' @keywords internal
#' @param ... Arguments to be constructed into the string
named_commas <- function(...) {
  x <- c(...)
  if (is_null(names(x))) {
    paste0(x, collapse = ", ")
  } else {
    paste0(names(x), " = ", x, collapse = ", ")
  }
}

commas <- function(...) paste0(..., collapse = ", ")

in_travis <- function() identical(Sys.getenv("TRAVIS"), "true")

has_names <- rlang::have_names

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

is_1d <- function(x) {
  # dimension check is for matrices and data.frames
  (is_atomic(x) || is.list(x)) && length(dim(x)) <= 1
}

is_negated <- function(x) {
  is_lang(x, "-", n = 1)
}
