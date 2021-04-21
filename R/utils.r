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

any_apply <- function(xs, f) {
  for (x in xs) {
    if (f(x)) return(TRUE)
  }
  FALSE
}

deparse_names <- function(x) {
  x <- map_if(x, is_quosure, quo_squash)
  x <- map_if(x, is_bare_formula, f_rhs)
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
  tryCatch( #
    {
      x
      TRUE
    },
    error = function(e) {
      if (!quiet) {
        inform(paste0("Error: ", e$message))
      }
      FALSE
    }
  )
}

is_1d <- function(x) {
  # dimension check is for matrices and data.frames
  (is_atomic(x) || is.list(x)) && length(dim(x)) <= 1
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

unstructure <- function(x) {
  attributes(x) <- NULL
  x
}

compact_null <- function(x) {
  Filter(function(elt) !is.null(elt), x)
}

paste_line <- function(...) {
  paste(chr(...), collapse = "\n")
}

# Until fixed upstream. `vec_data()` should not return lists from data
# frames.
dplyr_vec_data <- function(x) {
  out <- vec_data(x)

  if (is.data.frame(x)) {
    new_data_frame(out, n = nrow(x))
  } else {
    out
  }
}

# Until vctrs::new_data_frame() forwards row names automatically
dplyr_new_data_frame <- function(x = data.frame(),
                                 n = NULL,
                                 ...,
                                 row.names = NULL,
                                 class = NULL) {
  row.names <- row.names %||% .row_names_info(x, type = 0L)

  new_data_frame(
    x,
    n = n,
    ...,
    row.names = row.names,
    class = class
  )
}

maybe_restart <- function(restart) {
  if (!is_null(findRestart(restart))) {
    invokeRestart(restart)
  }
}

expr_substitute <- function(expr, old, new) {
  expr <- duplicate(expr)
  switch(typeof(expr),
    language = node_walk_replace(node_cdr(expr), old, new),
    symbol = if (identical(expr, old)) return(new)
  )
  expr
}
node_walk_replace <- function(node, old, new) {
  while (!is_null(node)) {
    switch(
      typeof(node_car(node)),
      language = node_walk_replace(node_cdar(node), old, new),
      symbol = if (identical(node_car(node), old)) node_poke_car(node, new)
    )
    node <- node_cdr(node)
  }
}
