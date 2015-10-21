#' A data cube tbl.
#'
#' A cube tbl stores data in a compact array format where dimension
#' names are not needlessly repeated. They are particularly appropriate for
#' experimental data where all combinations of factors are tried (e.g.
#' complete factorial designs), or for storing the result of aggregations.
#' Compared to data frames, they will occupy much less memory when variables
#' are crossed, not nested.
#'
#' \code{tbl_cube} support is currently experimental and little performance
#' optimisation has been done, but you may find them useful if your data
#' already comes in this form, or you struggle with the memory overhead of the
#' sparse/crossed of data frames.  There is no support for hierarchical
#' indices (although I think that would be a relatively straightforward
#' extension to storing data frames for indices rather than vectors).
#'
#' @section Implementation:
#'
#' Manipulation functions:
#'
#' \itemize{
#'   \item \code{select} (M)
#'
#'   \item \code{summarise} (M), corresponds to roll-up, but rather more
#'     limited since there are no hierarchies.
#'
#'   \item \code{filter} (D), corresponds to slice/dice.
#'
#'   \item \code{mutate} (M) is not implemented, but should be relatively
#'   straightforward given the implementation of \code{summarise}.
#'
#'   \item \code{arrange} (D?) Not implemented: not obvious how much sense
#'     it would make
#' }
#'
#' Joins: not implemented. See \code{vignettes/joins.graffle} for ideas.
#' Probably straightforward if you get the indexes right, and that's probably
#' some straightforward array/tensor operation.
#'
#' @export
#' @param dimensions A named list of vectors. A dimension is a variable
#'   whose values are known before the experiement is conducted; they are
#'   fixed by design (in \pkg{reshape2} they are known as id variables).
#'   \code{tbl_cubes} are dense which means that almost every combination of
#'   the dimensions should have associated measurements: missing values require
#'   an explicit NA, so if the variables are nested, not crossed, the
#'   majority of the data structure will be empty. Dimensions are typically,
#'   but not always, categorical variables.
#' @param measures A named list of arrays. A measure is something that is
#'   actually measured, and is not known in advance. The dimension of each
#'   array should be the same as the length of the dimensions. Measures are
#'   typically, but not always, continuous values.
#' @seealso \code{\link{as.tbl_cube}} for ways of coercing existing data
#'   structures into a \code{tbl_cube}.
#' @examples
#' # The built in nasa dataset records meterological data (temperature,
#' # cloud cover, ozone etc) for a 4d spatio-temporal dataset (lat, long,
#' # month and year)
#' nasa
#' head(as.data.frame(nasa))
#'
#' titanic <- as.tbl_cube(Titanic)
#' head(as.data.frame(titanic))
#'
#' admit <- as.tbl_cube(UCBAdmissions)
#' head(as.data.frame(admit))
#'
#' as.tbl_cube(esoph, dim_names = 1:3)
#'
#' # Some manipulation examples with the NASA dataset --------------------------
#'
#' # select() operates only on measures: it doesn't affect dimensions in any way
#' select(nasa, cloudhigh:cloudmid)
#' select(nasa, matches("temp"))
#'
#' # filter() operates only on dimensions
#' filter(nasa, lat > 0, year == 2000)
#' # Each component can only refer to one dimensions, ensuring that you always
#' # create a rectangular subset
#' \dontrun{filter(nasa, lat > long)}
#'
#' # Arrange is meaningless for tbl_cubes
#'
#' by_loc <- group_by(nasa, lat, long)
#' summarise(by_loc, pressure = max(pressure), temp = mean(temperature))
tbl_cube <- function(dimensions, measures) {
  if (!is.list(dimensions) || any_apply(dimensions, Negate(is.atomic)) ||
      is.null(names(dimensions))) {
    stop("Dimensions must be a named list of vectors", call. = FALSE)
  }

  if (!is.list(measures) || any_apply(measures, Negate(is.array)) ||
    is.null(names(measures))) {
    stop("Measures must be a named list of arrays", call. = FALSE)
  }

  # Check measures have correct dimensions
  dims <- vapply(dimensions, length, integer(1), USE.NAMES = FALSE)
  dims_ok <- vapply(measures, function(x) identical(unname(dim(x)), dims),
    logical(1))
  if (any(!dims_ok)) {
    bad <- names(measures)[!dims_ok]
    stop("Measures ", paste0(bad, collapse = ", "), " don't have correct ",
      "dimensions (", paste0(dims, collapse = " x "), ")", call. = FALSE)
  }

  structure(list(dims = dimensions, mets = measures), class = "tbl_cube")
}

#' @export
tbl_vars.tbl_cube <- function(x) names(x$dims)

#' @export
dim.tbl_cube <- function(x) {
  c(length(x$mets[[1]]), length(x$dim))
}

#' @export
same_src.tbl_cube <- function(x, y) {
  inherits(y, "tbl_cube")
}

#' @export
print.tbl_cube <- function(x, ...) {
  cat("Source: local array ", dim_desc(x), "\n",
    sep = "")
  if (!is.null(x$group)) {
    cat("Grouped by: ", paste(names(x$dims)[x$group], collapse = ", "),
      "\n", sep = "")
  }

  # Dimensions
  types <- vapply(x$dims, type_sum, character(1))
  lengths <- vapply(x$dims, length, integer(1))
  vars <- paste0("D: ", names(x$dims), " [", types, ", ", lengths, "]")
  cat(vars, sep = "\n")

  # Measures
  types <- vapply(x$mets, type_sum, character(1))
  vars <- paste0("M: ", names(x$mets), " [", types, "]")
  cat(vars, sep = "\n")
  invisible(x)
}

#' @export
as.data.frame.tbl_cube <- function(x, ...) {
  dims <- expand.grid(x$dims, KEEP.OUT.ATTRS = FALSE)
  mets <- lapply(x$mets, as.vector)

  all <- c(dims, mets)
  class(all) <- "data.frame"
  attr(all, "row.names") <- .set_row_names(nrow(dims))

  all
}

# Coercion methods -------------------------------------------------------------

#' Coerce an existing data structure into a \code{tbl_cube}
#'
#' @param x an object to convert. Built in methods will convert arrays,
#'   tables and data frames.
#' @param ... Passed on to individual methods; otherwise ignored.
#' @export
as.tbl_cube <- function(x, ...) UseMethod("as.tbl_cube")

#' @export
#' @rdname as.tbl_cube
#' @param met_name a string to use as the name for the metric
#' @param dim_names names of the dimesions. Defaults to the names of
#'   the \code{\link{dimnames}}.
as.tbl_cube.array <- function(x, met_name = deparse(substitute(x)),
                               dim_names = names(dimnames(x)), ...) {
  force(met_name)

  dims <- dimnames(x)
  dims <- lapply(dims, utils::type.convert, as.is = TRUE)

  if (is.table(x)) {
    class(x) <- setdiff(class(x), "table")
  }
  mets <- setNames(list(undimname(x)), met_name)

  tbl_cube(dims, mets)
}

undimname <- function(x) {
  dimnames(x) <- NULL
  x
}

#' @export
#' @rdname as.tbl_cube
as.tbl_cube.table <- as.tbl_cube.array

#' @export
#' @rdname as.tbl_cube
as.tbl_cube.matrix <- as.tbl_cube.array

#' @export
#' @rdname as.tbl_cube
as.tbl_cube.data.frame <- function(x, dim_names, ...) {
  if (!is.character(dim_names)) {
    dim_names <- names(x)[dim_names]
  }
  met_names <- setdiff(names(x), dim_names)

  dims <- lapply(x[dim_names], unique)
  n <- vapply(dims, length, integer(1))
  # need to check for uniqueness of combinations

  grid <- expand.grid(dims, KEEP.OUT.ATTRS = FALSE)
  all <- merge(grid, x, all.x = TRUE, by = dim_names)

  mets <- lapply(met_names, function(i) array(x[[i]], n))
  names(mets) <- met_names

  tbl_cube(dims, mets)
}


# Verbs -------------------------------------------------------------------

#' @export
select_.tbl_cube <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  vars <- select_vars_(names(.data$mets), dots)

  .data$mets <- .data$mets[vars]
  .data
}

#' @export
rename_.tbl_cube <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  vars <- rename_vars_(names(.data$mets), dots)

  .data$mets <- .data$mets[vars]
  .data
}


#' @export
filter_.tbl_cube <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)

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
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)

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


#' @export
auto_copy.tbl_cube <- function(x, y, copy = FALSE, ...) {
  stop("Copying not supported by tbl_cube", call. = FALSE)
}
