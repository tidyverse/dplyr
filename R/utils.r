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

validate_rdf_entry <- function(call) {

  msg <- "expected cell delimited by '|'"

  if (!is.call(call))
    stop(msg)

  if (length(call) != 3)
    stop(msg)

  if (!is.symbol(call[[1]]))
    stop(msg)

  if (!identical(as.character(call[[1]]), "|"))
    stop(msg)

  rhs <- call[[3]]
  if (length(rhs) != 1 ||
      is.call(rhs))
  {
    stop("expected cell to be length-one symbol, string or literal")
  }

}

is_vbar_call <- function(call) {
  if (!is.call(call))
    return(FALSE)

  if (!(is.symbol(call[[1]]) || is.character(call[[1]])))
    return(FALSE)

  identical(as.character(call[[1]]), "|")
}

extract_rdf_header <- function(call) {
  n <- length(call)
  header <- character(n)
  i <- n
  while (is_vbar_call(call)) {
    header[[i]] <- as.character(call[[3]])
    i <- i - 1
    call <- call[[2]]
  }
  header[[1]] <- as.character(call)
  header
}

#' Row-wise Tbl Creation
#'
#' Create a \code{tbl_df} using a markdown-like language.
#'
#' @export
#' @examples
#' rdf(
#'    1 | 2 | 3,
#'    4 | 5 | 6
#' )
rdf <- function(...) {

  matched <- match.call(expand.dots = FALSE)$`...`

  # drop any missing arguments
  not_missing <- which(unlist(lapply(matched, function(x) {
    !identical(x, quote(expr =)) # `quote(expr =)` == missing arg
  })))
  matched <- matched[not_missing]

  nm <- unique(names(matched))
  if (any(nm != ""))
    stop("all arguments to 'rdf' should be unnamed", call. = FALSE)

  # Figure out if there is a header associated with the call
  dot_indices <- which(unlist(lapply(matched, function(x) {
    is.symbol(x) && grep("\\.+", as.character(x), perl = TRUE)
  })))

  header <- NULL
  if (length(dot_indices) > 0) {

    if (!identical(as.numeric(dot_indices), 2))
      stop("the header delimiter should exist at index 2 of call")

    if (length(matched) < 3)
      stop("expected content following header delimiter")

    header <- extract_rdf_header(matched[[1]])
    matched <- matched[3:length(matched)]

  }


  n_row <- length(matched)

  # Validate that the 'row' calls are valid (should be a cascading
  # set of calls to `|`)
  lengths <- vapply(matched, FUN.VALUE = numeric(1), USE.NAMES = FALSE, function(row) {

    len <- 1
    current_row <- row
    while (is_vbar_call(current_row)) {
      validate_rdf_entry(current_row)
      len <- len + 1
      current_row <- current_row[[2]]
    }
    len
  })

  if (length(unique(lengths)) != 1)
    stop("not all lengths are of same size; row lengths are:\n- ",
         paste(shQuote(lengths), collapse = ", "))

  n_col <- lengths[[1]]

  # create a shell data.frame to house the data -- allow implicit
  # conversions here as speed not a concern
  output <- lapply(vector("list", n_col), function(x) {
    logical(n_row)
  })
  class(output) <- "data.frame"
  attr(output, "row.names") <- .set_row_names(n_row)

  for (i in seq_along(matched)) {
    col_num <- n_col
    row_num <- i
    current_row <- matched[[i]]
    while (is_vbar_call(current_row)) {

      if (is.symbol(current_row[[3]]))
        current_row[[3]] <- as.character(current_row[[3]])

      output[[col_num]][[row_num]] <- current_row[[3]]
      col_num <- col_num - 1
      current_row <- current_row[[2]]
    }

    if (is.symbol(current_row))
      current_row <- as.character(current_row)

    output[[1]][[row_num]] <- current_row
  }

  if (is.null(header))
    names(output) <- paste("V", 1:n_col, sep = "")
  else
    names(output) <- header

  tbl_df(output)
}
