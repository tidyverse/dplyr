#' @export
select_.tbl_cube <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., env = parent.frame())
  vars <- select_vars_(names(.data$mets), dots)

  .data$mets <- .data$mets[vars]
  .data
}

#' @export
rename_.tbl_cube <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., env = parent.frame())
  vars <- rename_vars_(names(.data$mets), dots)

  .data$mets <- .data$mets[vars]
  .data
}


#' @export
filter_.tbl_cube <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., env = parent.frame())

  idx <- vapply(dots, function(d) find_index_check(d$expr, names(.data$dims)),
    integer(1))
  for(i in seq_along(dots)) {
    sel <- eval(dots[[i]]$expr, .data$dims, dots[[i]]$env)
    sel <- sel & !is.na(sel)

    .data$dims[[idx[i]]] <- .data$dims[[idx[i]]][sel]
    .data$mets <- lapply(.data$mets, subs_index, idx[i], sel)
  }

  .data
}

find_index_check <- function(x, names) {
  idx <- find_index(x, names)
  if (length(idx) != 1) {
    stop(deparse(x), " does not refer to exactly one dimension.", call. = FALSE)
  }
  idx
}

find_index <- function(x, names) {
  # Base cases
  if (is.atomic(x)) return(integer())
  if (is.name(x)) {
    var <- as.character(x)
    return(which(var == names))
  }

  # Recursive case: function call
  stopifnot(is.call(x))
  unlist(lapply(x[-1], find_index, names = names))
}

#' @export
group_by_.tbl_cube <- function(.data, ..., .dots, add = FALSE) {
  groups <- group_by_prepare(.data, ..., .dots = .dots, add = add)

  # Convert symbols to indices
  nms <- names(groups$data$dims)
  nms_list <- as.list(setNames(seq_along(nms), nms))

  groups$data$groups <- unlist(lapply(groups$group, eval, nms_list))
  groups$data
}

#' @export
groups.tbl_cube <- function(x) {
  lapply(x$dims, as.name)[x$group]
}

# mutate and summarise operate similarly need to evaluate variables in special
# context - need to use the same active environment tricks as in dplyr
# for better performance

#' @export
summarise_.tbl_cube <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., env = parent.frame(), all_named = TRUE)

  out_dims <- .data$dims[.data$group]
  n <- vapply(out_dims, length, integer(1))

  out_mets <- list()
  for (nm in names(dots)) {
    out_mets[[nm]] <- array(logical(), n)
  }

  slices <- expand.grid(lapply(out_dims, seq_along), KEEP.OUT.ATTRS = FALSE)

  # Loop over each group
  for (i in seq_len(nrow(slices))) {
    index <- as.list(slices[i, , drop = FALSE])
    mets <- lapply(.data$mets, subs_index, i = .data$group, val = index,
      drop = TRUE)

    # Loop over each expression
    for (j in seq_along(dots)) {
      res <- eval(dots[[j]]$expr, mets, dots[[j]]$env)
      out_mets[[j]][i] <- res
    }
  }

  structure(list(dims = out_dims, mets = out_mets), class = "tbl_cube")
}

subs_index <- function(x, i, val, drop = FALSE) {
  dims <- length(dim(x) %||% 1)

  args <- rep(list(quote(expr = )), dims)

  if (length(i) == 1 && is.atomic(val)) {
    args[[i]] <- quote(val)
  } else if (length(i) >= 1 && is.list(val)) {
    exprs <- lapply(seq_along(i), function(i) as.call(c(quote(`[[`), quote(val), i)))
    args[i] <- exprs
  } else {
    stop("Invalid input", call. = FALSE)
  }

  args$drop <- drop

  call <- as.call(c(quote(`[`), quote(x), args))
  eval(call)
}

