#' Build a data frame.
#'
#' A trimmed down version of \code{\link{data.frame}} that:
#' \enumerate{
#' \item Never coerces inputs (i.e. strings stay as strings!).
#' \item Never adds \code{row.names}.
#' \item Never munges column names.
#' \item Only recycles length 1 inputs.
#' \item Evaluates its arguments lazily and in order.
#' \item Adds \code{tbl_df} class to output.
#' }
#'
#' @param ... A set of named arguments
#' @param columns A \code{\link[lazyeval]{lazy_dots}}.
#' @seealso \code{\link{as_data_frame}} to turn an existing list into
#'   a data frame.
#' @export
#' @examples
#' a <- 1:5
#' data_frame(a, b = a * 2)
#' data_frame(a, b = a * 2, c = 1)
#' data_frame(x = runif(10), y = x * 2)
#'
#' # data_frame never coerces its inputs
#' str(data_frame(letters))
#' str(data_frame(x = list(diag(1), diag(2))))
#'
#' # or munges column names
#' data_frame(`a + b` = 1:5)
#'
#' # With the SE version, you give it a list of formulas/expressions
#' data_frame_(list(x = ~1:10, y = quote(x * 2)))
data_frame <- function(...) {
  data_frame_(lazyeval::lazy_dots(...))
}

#' @export
#' @rdname data_frame
data_frame_ <- function(columns) {

  n <- length(columns)
  if (n == 0) return(as_data_frame(list()))

  # If named not supplied, used deparsed expression
  col_names <- names2(columns)
  missing_names <- col_names == ""
  if (any(missing_names)) {
    deparse2 <- function(x) paste(deparse(x$expr, 500L), collapse = "")
    defaults <- vapply(columns[missing_names], deparse2, character(1),
      USE.NAMES = FALSE)

    col_names[missing_names] <- defaults
  }

  # Construct the list output
  output <- vector("list", n)
  names(output) <- character(n)
  output_nm <- names(output) # Get reference to names

  # Fill the output
  i <- 1L
  while (i <= n) {

    # Fill by reference
    res <- lazyeval::lazy_eval(columns[[i]], output)
    if (!is_1d(res)) {
      stop("data_frames can only contain 1d atomic vectors and lists",
        call. = FALSE)
    }
    output[[i]] <- res
    names(output)[i] <- col_names[[i]]

    # Update
    i <- i + 1L
  }

  # Validate column lengths
  lengths <- vapply(output, NROW, integer(1))
  max <- max(lengths)

  if (!all(lengths %in% c(1L, max))) {
    stop("arguments imply differing number of rows: ",
         paste(lengths, collapse = ", "))
  }
  short <- lengths == 1
  if (max != 1L && any(short)) {
    output[short] <- lapply(output[short], rep, max)
  }

  # Set attributes (make it a tbl_df)
  attr(output, "row.names") <- c(NA_integer_, max)
  attr(output, "class") <- c("tbl_df", "tbl", "data.frame")
  output
}

#' Coerce a list to a data frame.
#'
#' \code{as.data.frame} is effectively a thin wrapper around \code{data.frame},
#' and hence is rather slow (because it calls \code{data.frame} on each element
#' before \code{cbind}ing together). \code{as_data_frame} just verifies that
#' the list is structured correctly (i.e. named, and each element is same
#' length) then sets class and row name attributes.
#'
#' @param x A list. Each element of the list must have the same length.
#' @export
#' @examples
#' l <- list(x = 1:500, y = runif(500), z = 500:1)
#' df <- as_data_frame(l)
#'
#' # Coercing to a data frame does not copy columns
#' changes(as_data_frame(l), as_data_frame(l))
#'
#' # as_data_frame is considerably simpler/faster than as.data.frame
#' # making it more suitable for use when you have things that are
#' # lists
#' \dontrun{
#' l2 <- replicate(26, sample(letters), simplify = FALSE)
#' names(l2) <- letters
#' microbenchmark::microbenchmark(
#'   as_data_frame(l2),
#'   as.data.frame(l2)
#' )
#' }
as_data_frame <- function(x) {
  stopifnot(is.list(x))
  if (length(x) == 0) {
    x <- list()
    class(x) <- c("tbl_df", "tbl", "data.frame")
    attr(x, "row.names") <- .set_row_names(0)
    return(x)
  }

  names_x <- names2(x)
  if (any(is.na(names_x) | names_x == "")){
    stop("All columns must be named", call. = FALSE)
  }

  ok <- vapply(x, is_1d, logical(1))
  if (any(!ok)) {
    stop("data_frames can only contain 1d atomic vectors and lists",
      call. = FALSE)
  }

  n <- unique(vapply(x, NROW, integer(1)))
  if (length(n) != 1) {
    stop("Columns are not all same length", call. = FALSE)
  }

  class(x) <- c("tbl_df", "tbl", "data.frame")
  attr(x, "row.names") <- .set_row_names(n)

  x
}

#' Convert row names to an explicit variable.
#'
#' @param df Input data frame with rownames.
#' @param var Name of variable to use
#' @export
#' @examples
#' mtcars %>% tbl_df()
#'
#' mtcars %>% add_rownames()
add_rownames <- function(df, var = "rowname") {
  stopifnot(is.data.frame(df))

  rn <- as_data_frame(setNames(list(rownames(df)), var))
  rownames(df) <- NULL

  bind_cols(rn, df)
}

# Grouping methods ------------------------------------------------------------

#' @export
group_by_.data.frame <- function(.data, ..., .dots, add = FALSE) {
  groups <- group_by_prepare(.data, ..., .dots = .dots, add = add)
  grouped_df(groups$data, groups$groups)
}

#' @export
groups.data.frame <- function(x) NULL

#' @export
ungroup.data.frame <- function(x) x

#' @export
group_size.data.frame <- function(x) nrow(x)

#' @export
n_groups.data.frame <- function(x) 1L

# Manipulation functions ------------------------------------------------------

# These could potentially be rewritten to avoid any copies, but since this
# is just a convenience layer, I didn't bother. They should still be fast.

#' @export
filter_.data.frame <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  as.data.frame(filter_(tbl_df(.data), .dots = dots))
}
#' @export
slice_.data.frame <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  as.data.frame(slice_(tbl_df(.data), .dots = dots))
}
#' @export
summarise_.data.frame <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  as.data.frame(summarise_(tbl_df(.data), .dots = dots))
}
#' @export
mutate_.data.frame <-  function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  as.data.frame(mutate_(tbl_df(.data), .dots = dots))
}
#' @export
arrange_.data.frame <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  as.data.frame(arrange_(tbl_df(.data), .dots = dots))
}
#' @export
select_.data.frame <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  vars <- select_vars_(names(.data), dots)
  select_impl(.data, vars)
}
#' @export
rename_.data.frame <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  vars <- rename_vars_(names(.data), dots)
  select_impl(.data, vars)
}


# Joins ------------------------------------------------------------------------

#' @export
inner_join.data.frame <- function(x, y, by = NULL, copy = FALSE, ...) {
  as.data.frame(inner_join(tbl_df(x), y, by = by, copy = copy, ...))
}

#' @export
left_join.data.frame <- function(x, y, by = NULL, copy = FALSE, ...) {
  as.data.frame(left_join(tbl_df(x), y, by = by, copy = copy, ...))
}

#' @export
right_join.data.frame <- function(x, y, by = NULL, copy = FALSE, ...) {
  as.data.frame(right_join(tbl_df(x), y, by = by, copy = copy, ...))
}

#' @export
full_join.data.frame <- function(x, y, by = NULL, copy = FALSE, ...) {
  as.data.frame(full_join(tbl_df(x), y, by = by, copy = copy, ...))
}

#' @export
semi_join.data.frame <- function(x, y, by = NULL, copy = FALSE, ...) {
  as.data.frame(semi_join(tbl_df(x), y, by = by, copy = copy, ...))
}

#' @export
anti_join.data.frame <- function(x, y, by = NULL, copy = FALSE, ...) {
  as.data.frame(anti_join(tbl_df(x), y, by = by, copy = copy, ...))
}

# Set operations ---------------------------------------------------------------

#' @export
intersect.data.frame <- function(x, y, ...) intersect_data_frame(x, y)

#' @export
union.data.frame <-     function(x, y, ...) union_data_frame(x, y)

#' @export
setdiff.data.frame <-   function(x, y, ...) setdiff_data_frame(x, y)

#' @export
setequal.data.frame <-  function(x, y, ...) equal_data_frame(x, y)

#' @export
distinct_.data.frame <- function(.data, ..., .dots) {
  dist <- distinct_vars(.data, ..., .dots = .dots)
  distinct_impl(dist$data, dist$vars)
}


# Do ---------------------------------------------------------------------------

#' @export
do_.data.frame <- function(.data, ..., .dots) {
  args <- lazyeval::all_dots(.dots, ...)
  named <- named_args(args)

  data <- list(. = .data)

  if (!named) {
    env <- new.env(parent = args[[1]]$env)
    env$. <- .data

    out <- lazyeval::lazy_eval(args[[1]], data)
    if (!is.data.frame(out)) {
      stop("Result must be a data frame", call. = FALSE)
    }
  } else {
    out <- lapply(args, function(arg) {
      list(lazyeval::lazy_eval(arg, data))
    })
    names(out) <- names(args)
    attr(out, "row.names") <- .set_row_names(1L)
    # Use tbl_df to ensure safe printing of list columns
    class(out) <- c("tbl_df", "data.frame")
  }

  out
}

# Random samples ---------------------------------------------------------------


#' @export
sample_n.data.frame <- function(tbl, size, replace = FALSE, weight = NULL,
  .env = parent.frame()) {
  if (!missing(weight)) {
    weight <- eval(substitute(weight), tbl, .env)
  }

  sample_n_basic(tbl, size, replace = replace, weight = weight)
}


#' @export
sample_frac.data.frame <- function(tbl, size = 1, replace = FALSE, weight = NULL,
  .env = parent.frame()) {

  if (!missing(weight)) {
    weight <- eval(substitute(weight), tbl, .env)
  }

  sample_n_basic(tbl, round(size * nrow(tbl)), replace = replace, weight = weight)
}

sample_n_basic <- function(tbl, size, replace = FALSE, weight = NULL) {
  n <- nrow(tbl)

  weight <- check_weight(weight, n)
  assert_that(is.numeric(size), length(size) == 1, size >= 0)
  check_size(size, n, replace)

  idx <- sample.int(n, size, replace = replace, prob = weight)
  tbl[idx, , drop = FALSE]
}



# Misc -------------------------------------------------------------------------

#' @export
collect.data.frame <- function(x, ...) x
#' @export
compute.data.frame <- function(x, ...) x
#' @export
collapse.data.frame <- function(x, ...) x
