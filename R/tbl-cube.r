#' A data cube tbl
#'
#' A cube tbl stores data in a compact array format where dimension
#' names are not needlessly repeated. They are particularly appropriate for
#' experimental data where all combinations of factors are tried (e.g.
#' complete factorial designs), or for storing the result of aggregations.
#' Compared to data frames, they will occupy much less memory when variables
#' are crossed, not nested.
#'
#' `tbl_cube` support is currently experimental and little performance
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
#'   \item `select()` (M)
#'
#'   \item `summarise()` (M), corresponds to roll-up, but rather more
#'     limited since there are no hierarchies.
#'
#'   \item `filter()` (D), corresponds to slice/dice.
#'
#'   \item `mutate()` (M) is not implemented, but should be relatively
#'   straightforward given the implementation of `summarise`.
#'
#'   \item `arrange()` (D?) Not implemented: not obvious how much sense
#'     it would make
#' }
#'
#' Joins: not implemented. See `vignettes/joins.graffle` for ideas.
#' Probably straightforward if you get the indexes right, and that's probably
#' some straightforward array/tensor operation.
#'
#' @export
#' @param dimensions A named list of vectors. A dimension is a variable
#'   whose values are known before the experiment is conducted; they are
#'   fixed by design (in \pkg{reshape2} they are known as id variables).
#'   `tbl_cubes` are dense which means that almost every combination of
#'   the dimensions should have associated measurements: missing values require
#'   an explicit NA, so if the variables are nested, not crossed, the
#'   majority of the data structure will be empty. Dimensions are typically,
#'   but not always, categorical variables.
#' @param measures A named list of arrays. A measure is something that is
#'   actually measured, and is not known in advance. The dimension of each
#'   array should be the same as the length of the dimensions. Measures are
#'   typically, but not always, continuous values.
#' @seealso [as.tbl_cube()] for ways of coercing existing data
#'   structures into a `tbl_cube`.
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
    bad_args("dimensions", "must be a named list of vectors, ",
      "not {friendly_type_of(dimensions)}"
    )
  }

  if (!is.list(measures) || any_apply(measures, Negate(is.array)) ||
    is.null(names(measures))) {
    bad_args("measures", "must be a named list of arrays, ",
      "not {friendly_type_of(measures)}"
    )
  }

  # Check measures have correct dimensions
  dims <- vapply(dimensions, length, integer(1), USE.NAMES = FALSE)
  dims_ok <- vapply(
    measures, function(x) identical(unname(dim(x)), dims),
    logical(1)
  )
  if (any(!dims_ok)) {
    bad <- names(measures)[!dims_ok]
    bad_measures(bad, "needs dimensions {fmt_dims(dims)}, not {bad_dim}",
      bad_dim = fmt_dims(dim(measures[!dims_ok][[1L]]))
    )
  }

  structure(list(dims = dimensions, mets = measures), class = "tbl_cube")
}

#' @export
tbl_vars.tbl_cube <- function(x) names(x$dims)

#' @export
dim.tbl_cube <- function(x) {
  c(length(x$mets[[1]]), length(x$dims))
}

#' @export
same_src.tbl_cube <- function(x, y) {
  inherits(y, "tbl_cube")
}

#' @export
print.tbl_cube <- function(x, ...) {
  cat("Source: local array ", dim_desc(x), "\n", sep = "")
  if (!is.null(x$groups)) {
    cat(
      "Grouped by: ", paste(names(x$dims)[x$groups], collapse = ", "), "\n",
      sep = ""
    )
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

# Coercion methods (from tbl_cube) ---------------------------------------------

#' Coerce a `tbl_cube` to other data structures
#'
#' Supports conversion to tables, data frames, tibbles.
#'
#' @param x a `tbl_cube`
#' @param ... Passed on to individual methods; otherwise ignored.
#' @param measure A measure name or index, default: the first measure
#' @name as.table.tbl_cube
#' @export
as.table.tbl_cube <- function(x, ..., measure = 1L) {
  ret <- x$mets[[measure]]
  dimnames(ret) <- x$dims
  class(ret) <- "table"
  ret
}

#' @rdname as.table.tbl_cube
#' @export
as.data.frame.tbl_cube <- function(x, ...) {
  dims <- expand.grid(x$dims, KEEP.OUT.ATTRS = FALSE, ...)
  mets <- lapply(x$mets, as.vector)

  all <- c(dims, mets)
  class(all) <- "data.frame"
  attr(all, "row.names") <- .set_row_names(nrow(dims))

  all
}


#' @rdname as.table.tbl_cube
#' @description For a cube, the data frame returned by
#'   [tibble::as_tibble()] resulting data frame contains the
#'   dimensions as character values (and not as factors).
#' @export
as_tibble.tbl_cube <- function(x, ...) {
  as_tibble(as.data.frame(x, ..., stringsAsFactors = FALSE))
}

# Coercion methods -------------------------------------------------------------

#' Coerce an existing data structure into a `tbl_cube`
#'
#' @param x an object to convert. Built in methods will convert arrays,
#'   tables and data frames.
#' @param ... Passed on to individual methods; otherwise ignored.
#' @export
as.tbl_cube <- function(x, ...) UseMethod("as.tbl_cube")

#' @export
#' @rdname as.tbl_cube
#' @param dim_names names of the dimensions. Defaults to the names of
#'   the [dimnames()].
#' @param met_name a string to use as the name for the measure.
as.tbl_cube.array <- function(x, dim_names = names(dimnames(x)), met_name = deparse(substitute(x)),
                              ...) {
  force(met_name)

  dims <- dimnames(x)
  dims <- lapply(dims, utils::type.convert, as.is = TRUE)

  mets <- setNames(list(undimname(x)), met_name)

  tbl_cube(dims, mets)
}

undimname <- function(x) {
  dimnames(x) <- NULL
  x
}

#' @export
#' @rdname as.tbl_cube
as.tbl_cube.table <- function(x, dim_names = names(dimnames(x)), met_name = "Freq",
                              ...) {
  as.tbl_cube.array(unclass(x), dim_names = dim_names, met_name = met_name)
}

#' @export
#' @rdname as.tbl_cube
as.tbl_cube.matrix <- as.tbl_cube.array

guess_met <- function(df) {
  if ("Freq" %in% names(df)) {
    met <- "Freq"
  } else {
    is_num <- vapply(df, is.numeric, logical(1L))
    met <- names(df)[is_num]
  }

  inform(paste0("Using ", paste(met, collapse = ", "), " as measure column(s): use `met_name` to override."))
  met
}

#' @export
#' @rdname as.tbl_cube
as.tbl_cube.data.frame <- function(x, dim_names = NULL, met_name = guess_met(x),
                                   ...) {
  if (is.null(dim_names)) {
    dim_names <- setdiff(names(x), met_name)
  } else {
    met_name <- NULL
    if (!is.character(dim_names)) {
      dim_names <- names(x)[dim_names]
    }
  }

  if (is.null(met_name)) {
    met_name <- setdiff(names(x), dim_names)
  } else if (!is.character(met_name)) {
    met_name <- names(x)[met_name]
  }

  dims <- lapply(x[dim_names], unique)
  n <- vapply(dims, length, integer(1))

  grid <- expand.grid(dims, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  all <- left_join(grid, x, by = dim_names)
  if (nrow(all) > nrow(grid)) {
    dupe_row <- anyDuplicated(all[dim_names])
    dupe <- unlist(all[dupe_row, dim_names])

    bad_args("x", "must be unique in all combinations of dimension variables, ",
      "duplicates: {fmt_named(dupe)}"
    )
  }

  mets <- lapply(met_name, function(i) array(all[[i]], unname(n)))
  names(mets) <- met_name

  tbl_cube(dims, mets)
}


# Verbs -------------------------------------------------------------------

#' @export
select.tbl_cube <- function(.data, ...) {
  vars <- tidyselect::vars_select(names(.data$mets), ...)
  .data$mets <- .data$mets[vars]
  .data
}
#' @export
select_.tbl_cube <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  select(.data, !!!dots)
}

#' @export
rename.tbl_cube <- function(.data, ...) {
  vars <- tidyselect::vars_rename(names(.data$mets), !!!enquos(...))
  .data$mets <- .data$mets[vars]
  .data
}
#' @export
rename_.tbl_cube <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  rename(.data, !!!dots)
}


#' @export
filter.tbl_cube <- function(.data, ...) {
  dots <- enquos(...)

  idx <- map2_int(
    seq_along(dots), dots,
    function(i, d) find_index_check(i, d, names(.data$dims))
  )
  for (i in seq_along(dots)) {
    sel <- eval_tidy(dots[[i]], .data$dims)
    sel <- sel & !is.na(sel)

    .data$dims[[idx[i]]] <- .data$dims[[idx[i]]][sel]
    .data$mets <- lapply(.data$mets, subs_index, idx[i], sel)
  }

  .data
}
#' @export
filter_.tbl_cube <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  filter(.data, !!!dots)
}

find_index_check <- function(i, x, names) {
  idx <- find_index(quo_get_expr(x), names)
  if (length(idx) != 1) {
    bad_calls(x, "must refer to exactly one dimension, ",
      "not {fmt_obj(names[idx])}"
    )
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
group_by.tbl_cube <- function(.data, ..., add = FALSE, .drop = FALSE) {
  groups <- group_by_prepare(.data, ..., add = add)

  # Convert symbols to indices
  groups$data$groups <- match(groups$group_names, names(groups$data$dims))
  groups$data
}
#' @export
group_by_.tbl_cube <- function(.data, ..., .dots = list(), add = FALSE) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  group_by(.data, !!!dots, add = add)
}

#' @export
groups.tbl_cube <- function(x) {
  lapply(group_vars(x), as.name)
}

#' @export
group_vars.tbl_cube <- function(x) {
  names(x$dims[x$groups])
}

# mutate and summarise operate similarly need to evaluate variables in special
# context - need to use the same active environment tricks as in dplyr
# for better performance

#' @export
summarise.tbl_cube <- function(.data, ...) {
  dots <- enquos(..., .named = TRUE)

  out_dims <- .data$dims[.data$groups]
  n <- lengths(out_dims)

  out_mets <- list()
  for (nm in names(dots)) {
    out_mets[[nm]] <- array(logical(), n)
  }

  slices <- expand.grid(map(out_dims, seq_along), KEEP.OUT.ATTRS = FALSE)

  # Loop over each group
  for (i in seq_len(nrow(slices))) {
    index <- as.list(slices[i, , drop = FALSE])
    mets <- map(
      .data$mets, subs_index,
      i = .data$groups, val = index,
      drop = TRUE
    )

    # Loop over each expression
    for (j in seq_along(dots)) {
      res <- eval_tidy(dots[[j]], mets)
      out_mets[[j]][i] <- res
    }
  }

  structure(list(dims = out_dims, mets = out_mets), class = "tbl_cube")
}
#' @export
summarise_.tbl_cube <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  summarise(.data, !!!dots)
}

subs_index <- function(x, i, val, drop = FALSE) {
  dims <- length(dim(x) %||% 1)

  args <- rep(list(quote(expr = )), dims)

  if (length(i) == 1 && is.atomic(val)) {
    args[[i]] <- quote(val)
  } else if (length(i) >= 1 && is.list(val)) {
    exprs <- lapply(
      seq_along(i),
      function(i) as.call(c(quote(`[[`), quote(val), i))
    )
    args[i] <- exprs
  } else {
    abort("Invalid input")
  }

  args$drop <- drop

  call <- as.call(c(quote(`[`), quote(x), args))
  eval_bare(call)
}


#' @export
auto_copy.tbl_cube <- function(x, y, copy = FALSE, ...) {
  abort("Copying not supported by tbl_cube")
}
