#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

dots <- function(...) {
  eval_bare(substitute(alist(...)))
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
  x <- map_if(x, is_formula, f_rhs)
  map_chr(x, expr_text, width = 500L)
}

deparse_names <- function(x) {
  x <- map_if(x, is_formula, f_rhs)
  map_chr(x, deparse)
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

succeeds <- function(x, quiet = FALSE) {
  tryCatch(
    {
      x
      TRUE
    },
    error = function(e) {
      if (!quiet)
        inform(paste0("Error: ", e$message))
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

inc_seq <- function(from, to) {
  if (from > to) {
    integer()
  } else {
    seq.int(from, to)
  }
}

random_table_name <- function(n = 10) {
  paste0(sample(letters, n, replace = TRUE), collapse = "")
}

attr_equal <- function(x, y) {
  attr_x <- attributes(x)
  if (!is.null(attr_x)) {
    attr_x <- attr_x[sort(names(attr_x))]
  }

  attr_y <- attributes(y)
  if (!is.null(attr_y)) {
    attr_y <- attr_y[sort(names(attr_y))]
  }

  isTRUE(all.equal(attr_x, attr_y))
}

