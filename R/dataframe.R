setOldClass(c("tbl_df", "tbl", "data.frame"))

#' Build a data frame or list.
#'
#' \code{data_frame} is trimmed down version of \code{\link{data.frame}} that:
#' \enumerate{
#' \item Never coerces inputs (i.e. strings stay as strings!).
#' \item Never adds \code{row.names}.
#' \item Never munges column names.
#' \item Only recycles length 1 inputs.
#' \item Evaluates its arguments lazily and in order.
#' \item Adds \code{tbl_df} class to output.
#' \item Automatically adds column names.
#' }
#'
#' \code{lst} is similar to \code{\link{list}}, but like \code{data_frame}, it
#' evaluates its arguments lazily and in order, and automatically adds names.
#'
#' @param ... A set of name-value pairs. Arguments are evaluated sequentially,
#'   so you can refer to previously created variables.
#' @param xs  A list of unevaluated expressions created with \code{~},
#'   \code{quote()}, or \code{\link[lazyeval]{lazy}}.
#' @seealso \code{\link{as_data_frame}} to turn an existing list into
#'   a data frame.
#' @export
#' @examples
#' a <- 1:5
#' data_frame(a, b = a * 2)
#' data_frame(a, b = a * 2, c = 1)
#' data_frame(x = runif(10), y = x * 2)
#'
#' lst(n = 5, x = runif(n))
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
#'
#' # data frames can only contain 1d atomic vectors and lists
#' # and can not contain POSIXlt
#' \dontrun{
#' data_frame(x = data_frame(1, 2, 3))
#' data_frame(y = strptime("2000/01/01", "%x"))
#' }
data_frame <- function(...) {
  as_data_frame(lst(...))
}

#' @export
#' @rdname data_frame
data_frame_ <- function(xs) {
  as_data_frame(lst_(xs))
}

#' @export
#' @rdname data_frame
lst <- function(...) {
  lst_(lazyeval::lazy_dots(...))
}

#' @export
#' @rdname data_frame
lst_ <- function(xs) {
  n <- length(xs)
  if (n == 0) return(list())

  # If named not supplied, used deparsed expression
  col_names <- names2(xs)
  missing_names <- col_names == ""
  if (any(missing_names)) {
    deparse2 <- function(x) paste(deparse(x$expr, 500L), collapse = "")
    defaults <- vapply(xs[missing_names], deparse2, character(1),
      USE.NAMES = FALSE)

    col_names[missing_names] <- defaults
  }

  # Evaluate each column in turn
  output <- vector("list", n)
  names(output) <- character(n)

  for (i in seq_len(n)) {
    output[[i]] <- lazyeval::lazy_eval(xs[[i]], output)
    names(output)[i] <- col_names[[i]]
  }

  output
}


#' Coerce lists and matrices to data frames.
#'
#' \code{as.data.frame} is effectively a thin wrapper around \code{data.frame},
#' and hence is rather slow (because it calls \code{data.frame} on each element
#' before \code{cbind}ing together). \code{as_data_frame} is a new S3 generic
#' with more efficient methods for matrices and data frames.
#'
#' This is an S3 generic. dplyr includes methods for data frames (adds tbl_df
#' classes), tbl_dfs (trivial!), grouped_dfs (ungroups), lists, and matrices.
#'
#' @param x A list. Each element of the list must have the same length.
#' @param ... Other arguments passed on to individual methods.
#' @param validate When \code{TRUE}, verifies that the input is a valid data
#'   frame (i.e. all columns are named, and are 1d vectors or lists). You may
#'   want to suppress this when you know that you already have a valid data
#'   frame and you want to save some time.
#' @export
#' @examples
#' l <- list(x = 1:500, y = runif(500), z = 500:1)
#' df <- as_data_frame(l)
#'
#' m <- matrix(rnorm(50), ncol = 5)
#' colnames(m) <- c("a", "b", "c", "d", "e")
#' df <- as_data_frame(m)
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
#'   as_data_frame(l2, validate = FALSE),
#'   as_data_frame(l2),
#'   as.data.frame(l2)
#' )
#'
#' m <- matrix(runif(26 * 100), ncol = 26)
#' colnames(m) <- letters
#' microbenchmark::microbenchmark(
#'   as_data_frame(m),
#'   as.data.frame(m)
#' )
#' }
as_data_frame <- function(x, ...) {
  UseMethod("as_data_frame")
}

#' @export
#' @rdname as_data_frame
as_data_frame.grouped_df <- function(x, ...) {
  x <- ungroup(x)
  class(x) <- c("tbl_df", "tbl", "data.frame")
  x
}


#' @export
#' @rdname as_data_frame
as_data_frame.tbl_df <- function(x, ...) {
  x
}

#' @export
#' @rdname as_data_frame
as_data_frame.data.frame <- function(x, ...) {
  class(x) <- c("tbl_df", "tbl", "data.frame")
  x
}

#' @export
#' @rdname as_data_frame
as_data_frame.list <- function(x, validate = TRUE, ...) {
  if (length(x) == 0) {
    x <- list()
    class(x) <- c("tbl_df", "tbl", "data.frame")
    attr(x, "row.names") <- .set_row_names(0)
    return(x)
  }

  if (validate) {
    check_data_frame(x)
  }
  x <- recycle_columns(x)

  class(x) <- c("tbl_df", "tbl", "data.frame")
  attr(x, "row.names") <- .set_row_names(length(x[[1]]))

  x
}

#' @export
#' @rdname as_data_frame
as_data_frame.matrix <- function(x, ...) {
  matrixToDataFrame(x)
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

#' Add a row to a data frame
#'
#' This is a convenient way to add a single row of data to an existing data
#' frame. See \code{\link{frame_data}} for an easy way to create an complete
#' data frame row-by-row.
#'
#' @param .data Data frame to append to.
#' @param ... Name-value pairs. If you don't supply the name of a variable,
#'   it'll be given the value \code{NA}.
#' @examples
#' # add_row ---------------------------------
#' df <- data_frame(x = 1:3, y = 3:1)
#'
#' df %>%
#'   add_row(x = 4, y = 0)
#'
#' # You can supply vectors, to add multiple rows (this isn't
#' # recommended because it's a bit hard to read)
#' df %>%
#'   add_row(x = 4:5, y = 0:-1)
#'
#' # Absent variables get missing values
#' df %>%
#'   add_row(x = 4)
#'
#' # You can't create new variables
#' \dontrun{
#' df %>%
#'   add_row(z = 10)
#' }
#' @export
add_row <- function(.data, ...) {
  df <- data_frame(...)

  extra_vars <- setdiff(names(df), names(.data))
  if (length(extra_vars) > 0) {
    stop(
      "This row would add new variables: ", paste0(extra_vars, collapse = ", "),
      call. = FALSE
    )
  }

  bind_rows(.data, data_frame(...))
}

# Validity checks --------------------------------------------------------------

check_data_frame <- function(x) {
  # Names
  names_x <- names2(x)
  bad_name <- is.na(names_x) | names_x == ""
  if (any(bad_name)) {
    invalid_df("Each variable must be named", x, which(bad_name))
  }

  dups <- duplicated(names_x)
  if (any(dups)) {
    invalid_df("Each variable must have a unique name", x, dups)
  }

  # Types
  is_1d <- vapply(x, is_1d, logical(1))
  if (any(!is_1d)) {
    invalid_df("Each variable must be a 1d atomic vector or list", x, !is_1d)
  }

  posixlt <- vapply(x, inherits, "POSIXlt", FUN.VALUE = logical(1))
  if (any(posixlt)) {
    invalid_df("Date/times must be stored as POSIXct, not POSIXlt", x, posixlt)
  }

  x
}

recycle_columns <- function(x) {
  # Validate column lengths
  lengths <- vapply(x, NROW, integer(1))
  max <- max(lengths)

  bad_len <- lengths != 1L & lengths != max
  if (any(bad_len)) {
    invalid_df(paste0("Variables must be length 1 or ", max), x, bad_len)
  }

  short <- lengths == 1
  if (max != 1L && any(short)) {
    x[short] <- lapply(x[short], rep, max)
  }

  x
}

invalid_df <- function(problem, df, vars) {
  if (is.logical(vars)) {
    vars <- names(df)[vars]
  }
  stop(
    problem, ".\n",
    "Problem variables: ", paste0(vars, collapse = ", "), ".\n",
    call. = FALSE
  )
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
ungroup.data.frame <- function(x, ...) x

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
#' @rdname select
select.data.frame <- function(x, ..., .dots) {
  select_.data.frame(x, .dots = lazyeval::lazy_dots(...))
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
