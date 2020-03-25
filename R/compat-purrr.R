# nocov start - compat-purrr (last updated: rlang 0.3.0.9000)

# This file serves as a reference for compatibility functions for
# purrr. They are not drop-in replacements but allow a similar style
# of programming. This is useful in cases where purrr is too heavy a
# package to depend on. Please find the most recent version in rlang's
# repository.

map <- function(.x, .f, ...) {
  lapply(.x, .f, ...)
}
map_mold <- function(.x, .f, .mold, ...) {
  out <- vapply(.x, .f, .mold, ..., USE.NAMES = FALSE)
  names(out) <- names(.x)
  out
}
map_lgl <- function(.x, .f, ...) {
  map_mold(.x, .f, logical(1), ...)
}
map_int <- function(.x, .f, ...) {
  map_mold(.x, .f, integer(1), ...)
}
map_dbl <- function(.x, .f, ...) {
  map_mold(.x, .f, double(1), ...)
}
map_chr <- function(.x, .f, ...) {
  map_mold(.x, .f, character(1), ...)
}
map_cpl <- function(.x, .f, ...) {
  map_mold(.x, .f, complex(1), ...)
}

pluck <- function(.x, .f) {
  map(.x, `[[`, .f)
}
pluck_lgl <- function(.x, .f) {
  map_lgl(.x, `[[`, .f)
}
pluck_int <- function(.x, .f) {
  map_int(.x, `[[`, .f)
}
pluck_dbl <- function(.x, .f) {
  map_dbl(.x, `[[`, .f)
}
pluck_chr <- function(.x, .f) {
  map_chr(.x, `[[`, .f)
}
pluck_cpl <- function(.x, .f) {
  map_cpl(.x, `[[`, .f)
}

map2 <- function(.x, .y, .f, ...) {
  out <- mapply(.f, .x, .y, MoreArgs = list(...), SIMPLIFY = FALSE)
  if (length(out) == length(.x)) {
    set_names(out, names(.x))
  } else {
    set_names(out, NULL)
  }
}
map2_lgl <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "logical")
}
map2_int <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "integer")
}
map2_dbl <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "double")
}
map2_chr <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "character")
}
map2_cpl <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "complex")
}
walk2 <- function(.x, .y, .f, ...) {
  map2(.x, .y, .f, ...)
  invisible(.x)
}
args_recycle <- function(args) {
  lengths <- lengths(args)
  n <- max(lengths)

  abort_if_not(all(lengths == 1L | lengths == n))
  to_recycle <- lengths == 1L
  args[to_recycle] <- map(args[to_recycle], function(x) rep.int(x, n))

  args
}
pmap <- function(.l, .f, ...) {
  args <- args_recycle(.l)
  do.call("mapply", c(
    FUN = list(quote(.f)),
    args, MoreArgs = quote(list(...)),
    SIMPLIFY = FALSE, USE.NAMES = FALSE
  ))
}

probe <- function(.x, .p, ...) {
  if (is_logical(.p)) {
    abort_if_not(length(.p) == length(.x))
    .p
  } else {
    map_lgl(.x, .p, ...)
  }
}

keep <- function(.x, .f, ...) {
  .x[probe(.x, .f, ...)]
}
discard <- function(.x, .p, ...) {
  sel <- probe(.x, .p, ...)
  .x[is.na(sel) | !sel]
}
map_if <- function(.x, .p, .f, ...) {
  matches <- probe(.x, .p)
  .x[matches] <- map(.x[matches], .f, ...)
  .x
}

compact <- function(.x) {
  Filter(length, .x)
}

transpose <- function(.l) {
  inner_names <- names(.l[[1]])
  if (is.null(inner_names)) {
    fields <- seq_along(.l[[1]])
  } else {
    fields <- set_names(inner_names)
  }

  map(fields, function(i) {
    map(.l, .subset2, i)
  })
}

every <- function(.x, .p, ...) {
  for (i in seq_along(.x)) {
    if (!rlang::is_true(.p(.x[[i]], ...))) return(FALSE)
  }
  TRUE
}
some <- function(.x, .p, ...) {
  for (i in seq_along(.x)) {
    if (rlang::is_true(.p(.x[[i]], ...))) return(TRUE)
  }
  FALSE
}
negate <- function(.p) {
  function(...) !.p(...)
}

reduce <- function(.x, .f, ..., .init) {
  f <- function(x, y) .f(x, y, ...)
  Reduce(f, .x, init = .init)
}
reduce_right <- function(.x, .f, ..., .init) {
  f <- function(x, y) .f(y, x, ...)
  Reduce(f, .x, init = .init, right = TRUE)
}
accumulate <- function(.x, .f, ..., .init) {
  f <- function(x, y) .f(x, y, ...)
  Reduce(f, .x, init = .init, accumulate = TRUE)
}
accumulate_right <- function(.x, .f, ..., .init) {
  f <- function(x, y) .f(y, x, ...)
  Reduce(f, .x, init = .init, right = TRUE, accumulate = TRUE)
}

detect <- function(.x, .f, ..., .right = FALSE, .p = is_true) {
  for (i in index(.x, .right)) {
    if (.p(.f(.x[[i]], ...))) {
      return(.x[[i]])
    }
  }
  NULL
}
detect_index <- function(.x, .f, ..., .right = FALSE, .p = is_true) {
  for (i in index(.x, .right)) {
    if (.p(.f(.x[[i]], ...))) {
      return(i)
    }
  }
  0L
}
index <- function(x, right = FALSE) {
  idx <- seq_along(x)
  if (right) {
    idx <- rev(idx)
  }
  idx
}

imap <- function(.x, .f, ...) {
  map2(.x, vec_index(.x), .f, ...)
}
vec_index <- function(x) {
  names(x) %||% seq_along(x)
}


# nocov end
