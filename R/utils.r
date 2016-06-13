dots <- function(...) {
  eval(substitute(alist(...)))
}

named_dots <- function(...) {
  auto_name(dots(...))
}

auto_names <- function(x) {
  nms <- names2(x)
  missing <- nms == ""
  if (all(!missing)) return(nms)

  deparse2 <- function(x) paste(deparse(x, 500L), collapse = "")
  defaults <- vapply(x[missing], deparse2, character(1), USE.NAMES = FALSE)

  nms[missing] <- defaults
  nms
}

deparse_trunc <- function(x, width = getOption("width")) {
  text <- deparse(x, width.cutoff = width)
  if (length(text) == 1 && nchar(text) < width) return(text)

  paste0(substr(text[1], 1, width - 3), "...")
}

auto_name <- function(x) {
  names(x) <- auto_names(x)
  x
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

only_has_names <- function(x, nms) {
  all(names(x) %in% nms)
}
on_failure(all_names) <- function(call, env) {
  x_nms <- names(eval(call$x, env))
  nms <- eval(call$nms, env)
  extra <- setdiff(x_nms, nms)

  paste0(call$x, " has named components: ", paste0(extra, collapse = ", "), ".",
    "Should only have names: ", paste0(nms, collapse = ","))
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

"%||%" <- function(x, y) if(is.null(x)) y else x

is.wholenumber <- function(x, tol = .Machine$double.eps ^ 0.5) {
  abs(x - round(x)) < tol
}

as_df <- function(x) {
  class(x) <- "data.frame"
  attr(x, "row.names") <- c(NA_integer_, -length(x[[1]]))

  x
}

deparse_all <- function(x) {
  deparse2 <- function(x) paste(deparse(x, width.cutoff = 500L), collapse = "")
  vapply(x, deparse2, FUN.VALUE = character(1))
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
    paste0("_W", i)
  }
})

isFALSE <- function(x) identical(x, FALSE)

substitute_q <- function(x, env) {
  call <- substitute(substitute(x, env), list(x = x))
  eval(call)
}


succeeds <- function(x, quiet = FALSE) {
  tryCatch({x; TRUE}, error = function(e) {
    if (!quiet) message("Error: ", e$message)
    FALSE
  })
}

is_1d <- function(x) {
  # is.atomic() is TRUE for atomic vectors AND NULL!
  # dimension check is for matrices and data.frames
  ((is.atomic(x) && !is.null(x)) || is.list(x)) && length(dim(x)) <= 1
}
